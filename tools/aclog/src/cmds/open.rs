use std::{env, fs, path::Path, process::Command};

use anyhow::Context as _;
use bpaf::{Bpaf, Parser};
use tempfile::Builder as TempBuilder;

use super::Run;
use crate::{langs, utils, workspace_info::WorkspaceInfo};

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(parser))]
pub struct Open {
    /// Task URL to open
    #[bpaf(long, short, argument("URL"))]
    pub(crate) url: Option<String>,

    /// Filename if no 'URL' is specified OR if the filename cannot be guessed
    /// from the 'URL'
    #[bpaf(long, short, argument("FILE"))]
    pub(crate) file: Option<String>,

    /// Contest name if no 'URL' is specified AND cannot be specified if
    /// 'URL' is available
    #[bpaf(long, short, argument("CONTEST"))]
    pub(crate) contest: Option<String>,

    /// Quiet mode: suppress all output except for the directory path
    #[bpaf(long, short)]
    pub(crate) quiet: bool,

    #[bpaf(external(langs::lang))]
    pub(crate) lang: langs::Lang,
}

// Custom parser for `Open`
pub fn open() -> impl Parser<Open> {
    parser()
        .guard(
            |opts| opts.url.is_some() || opts.file.is_some() && opts.contest.is_some(),
            "Either 'URL' or 'FILE and CONTEST' must be specified",
        )
        .guard(
            |opts| opts.url.is_some() != opts.contest.is_some(),
            "'CONTEST' and 'URL' cannot be specified at the same time",
        )
}

impl Open {
    // Run a command and handle output based on quiet mode
    fn run_command(&self, cmd: &mut Command, description: &str) -> anyhow::Result<()> {
        let output = cmd.output()?;
        if !output.status.success() {
            if !self.quiet {
                eprintln!("[[{description}]]");
                eprintln!("Error:\n{}", String::from_utf8_lossy(&output.stderr));
            }
            // We don't want to fail the whole operation if a command fails
            // Just log the error and continue
        } else if !self.quiet && !output.stdout.is_empty() {
            // Only show stdout in verbose mode if there's something to show
            eprintln!("[[{description}]]");
            eprintln!("{}", String::from_utf8_lossy(&output.stdout));
        }
        Ok(())
    }
}

impl Run for Open {
    fn run(&self) -> anyhow::Result<()> {
        // Create WorkspaceInfo from URL or arguments
        let workspace_info = if let Some(url) = self.url.as_deref() {
            WorkspaceInfo::try_from_url(url, self.file.as_deref(), self.lang.clone())?
        } else {
            WorkspaceInfo::try_from_args(
                self.file.as_deref(),
                self.contest.as_deref(),
                self.lang.clone(),
            )?
        };

        // Set up the temporary directory
        let proj_root = utils::get_proj_root()?;
        let lang_root = proj_root
            .join("runtimes")
            .join(workspace_info.lang().to_string());
        let solution = workspace_info.solution_path(&proj_root);

        // Create a temporary directory using tempfile
        // Format: aclog-atcoder-{contest}-{problem}-{lang}-
        // This naming convention allows us to parse the metadata back from the
        // directory name
        let temp_dir_prefix = workspace_info.temp_dir_prefix();
        // Use keep() to prevent automatic cleanup of the temporary directory
        // This ensures the directory remains after the function exits
        let temp_dir = TempBuilder::new().prefix(&temp_dir_prefix).tempdir()?;
        let temp_dir_path = temp_dir.keep();

        // Print temporary directory location to stderr for reference
        if !self.quiet {
            eprintln!("Working directory: {}", temp_dir_path.display());
        }

        // Copy runtime directory contents to the temporary directory
        copy_dir_contents(&lang_root, &temp_dir_path)?;

        // Write URL to .url.txt file
        let url_file_path = temp_dir_path.join(".url.txt");
        let url = workspace_info.get_url();
        fs::write(&url_file_path, &url).context("Failed to write URL to .url.txt")?;

        // Change to the temporary directory
        env::set_current_dir(&temp_dir_path)?;

        let temp_entrypoint = temp_dir_path.join(workspace_info.get_entrypoint());

        // Check if solution file already exists and copy it
        if solution.exists() {
            fs::copy(&solution, &temp_entrypoint)
                .context("Failed to copy an existing solution file")?;
        }

        if command_exists_in_path("oj") {
            if !self.quiet {
                eprintln!("Downloading test cases from {url}...");
            }

            self.run_command(
                Command::new("oj").args(["download", &url]),
                "downloading test cases",
            )?;
        } else if !self.quiet {
            eprintln!("Command not found: oj - skipping test case download");
        }

        if command_exists_in_path("git") {
            self.run_command(
                Command::new("git").args(["init"]),
                "initializing git repository",
            )?;

            self.run_command(
                Command::new("git").args(["config", "user.name", "AtCoder Workspace"]),
                "configuring git user name",
            )?;

            self.run_command(
                Command::new("git").args(["config", "user.email", "workspace@example.com"]),
                "configuring git email",
            )?;

            self.run_command(
                Command::new("git").args(["add", "."]),
                "adding files to git",
            )?;

            self.run_command(
                Command::new("git").args(["commit", "-m", "initial state"]),
                "committing files",
            )?;
        } else if !self.quiet {
            eprintln!("Command not found: git - skipping git operations");
        }

        // Check if direnv exists and should be used
        let envrc_path = temp_dir_path.join(".envrc");
        if envrc_path.exists() && command_exists_in_path("direnv") {
            if !self.quiet {
                eprintln!("Found .envrc file, allowing with direnv...");
            }

            self.run_command(
                Command::new("direnv").args(["allow", temp_dir_path.to_str().unwrap()]),
                "allowing direnv for workspace",
            )?;
        }

        // Output the temp directory path to stdout
        println!("{}", temp_dir_path.display());

        Ok(())
    }
}

/// Checks if a command exists in the system PATH
///
/// This function iterates through all directories in the PATH environment
/// variable and checks if the specified command exists in any of them.
///
/// # Arguments
///
/// * `command` - The name of the command to check for
///
/// # Returns
///
/// `true` if the command exists in the PATH, `false` otherwise
fn command_exists_in_path(command: &str) -> bool {
    if let Ok(paths) = env::var("PATH") {
        for path in env::split_paths(&paths) {
            let full_path = path.join(command);
            if Path::new(&full_path).exists() {
                return true;
            }
        }
    }
    false
}

fn copy_dir_contents(src: &Path, dst: &Path) -> anyhow::Result<()> {
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let src_path = entry.path();

        // Skip .git directory
        if src_path.file_name().unwrap_or_default() == ".git" {
            continue;
        }

        let dst_path = dst.join(src_path.file_name().unwrap());

        if entry.file_type()?.is_dir() {
            fs::create_dir_all(&dst_path)?;
            copy_dir_contents(&src_path, &dst_path)?;
        } else if src_path.is_file() {
            fs::copy(&src_path, &dst_path)?;
        }
    }

    Ok(())
}
