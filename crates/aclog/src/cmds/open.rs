use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{Context as _, bail, ensure};
use bpaf::{Bpaf, Parser};
use regex::Regex;
use tempfile::Builder as TempBuilder;
use url::Url;

use super::Run;
use crate::langs;

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

const ATCODER_CONTEST_URL: &str = "https://atcoder.jp/contests";

impl Run for Open {
    fn run(&self) -> anyhow::Result<()> {
        let file = match &self.file {
            Some(file) => file.to_string(),
            None => match self.url.as_ref() {
                Some(url) if url.starts_with(ATCODER_CONTEST_URL) => {
                    Regex::new(r"/tasks/([a-z0-9_]+)/?$")?
                        .captures(url)
                        .and_then(|captures| {
                            captures
                                .get(1)
                                .map(|m| format!("{}.{}", m.as_str(), self.lang.extension()))
                        })
                        .context("No task ID found")?
                },
                Some(_) => bail!("Cannot guess filename from URL"),
                None => unreachable!(), // guarded by the parser
            },
        };

        let (url, contest) = if let Some(url) = &self.url {
            let parsed_url = Url::parse(url).context("Invalid URL")?;
            let contest = parsed_url
                .path_segments()
                .and_then(|mut segments| segments.nth(1))
                .context("No contest found")?
                .to_string();
            (url.to_string(), contest)
        } else {
            let contest = self.contest.as_ref().unwrap(); // guarded by the parser
            let url = Url::parse(&format!("{ATCODER_CONTEST_URL}/tasks/{contest}"))?;
            (url.to_string(), contest.to_string())
        };

        // Set up the temporary directory
        let proj_root = get_proj_root()?;
        let lang_root = proj_root.join("runtimes").join(self.lang.to_string());
        let solution = proj_root.join("contests").join(&contest).join(&file);

        // Create a temporary directory using tempfile
        // Format: aclog-atcoder-{contest}-{filename}_{extension}-{lang}-
        // This naming convention allows us to parse the metadata back from the
        // directory name
        let temp_dir_prefix = format!(
            "aclog-atcoder-{}-{}-{}-",
            contest,
            file.replace('.', "_"),
            self.lang
        );
        let temp_dir = TempBuilder::new().prefix(&temp_dir_prefix).tempdir()?;
        let temp_dir_path = temp_dir.path().to_path_buf();

        // Print temporary directory location to stderr for reference
        eprintln!("Working directory: {}", temp_dir_path.display());

        // Copy runtime directory contents to the temporary directory
        copy_dir_contents(&lang_root, &temp_dir_path)?;

        // Write URL to .url.txt file
        let url_file_path = temp_dir_path.join(".url.txt");
        fs::write(&url_file_path, &url).context("Failed to write URL to .url.txt")?;

        // Change to the temporary directory
        env::set_current_dir(&temp_dir_path)?;

        // Initialize git repository
        Command::new("git").args(["init"]).status()?;
        Command::new("git")
            .args(["config", "user.name", "AtCoder Solver"])
            .status()?;
        Command::new("git")
            .args(["config", "user.email", "solver@example.com"])
            .status()?;

        // Add all files to git and commit
        Command::new("git").args(["add", "."]).status()?;
        Command::new("git")
            .args(["commit", "-m", "Initial state"])
            .status()?;

        // Download the test cases
        Command::new("oj").arg("download").arg(&url).status()?;

        let temp_entrypoint = temp_dir_path.join(self.lang.entrypoint());

        // Check if solution file already exists
        let solution_existed = solution.exists();
        if solution_existed {
            fs::copy(&solution, &temp_entrypoint)
                .context("Failed to copy an existing solution file")?;
        }

        // Output the temp directory path to stdout
        println!("{}", temp_dir_path.display());

        Ok(())
    }
}

fn get_proj_root() -> anyhow::Result<PathBuf> {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()?;

    ensure!(output.status.success(), "Failed to get project root");

    Ok(PathBuf::from(String::from_utf8(output.stdout)?.trim()))
}

fn copy_dir_contents(src: &Path, dst: &Path) -> anyhow::Result<()> {
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let src_path = entry.path();

        // Skip .git directory
        if src_path.file_name().unwrap_or_default() == ".git" {
            continue;
        }

        let dst_path = dst.join(src_path.file_name().unwrap());

        if file_type.is_dir() {
            fs::create_dir_all(&dst_path)?;
            copy_dir_contents(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{
        fs::{self, File},
        io::Write,
    };

    use tempfile::tempdir;

    use super::*;

    #[test]
    fn test_copy_dir_contents() -> anyhow::Result<()> {
        // Create source directory structure
        let src_dir = tempdir()?;
        let src_path = src_dir.path().to_path_buf();

        // Create some files in the source directory
        let file1_path = src_path.join("file1.txt");
        let mut file1 = File::create(&file1_path)?;
        writeln!(file1, "Test content 1")?;

        // Create a subdirectory with a file
        let subdir_path = src_path.join("subdir");
        fs::create_dir(&subdir_path)?;

        let file2_path = subdir_path.join("file2.txt");
        let mut file2 = File::create(&file2_path)?;
        writeln!(file2, "Test content 2")?;

        // Create a .git directory that should be skipped
        let git_dir_path = src_path.join(".git");
        fs::create_dir(&git_dir_path)?;

        let git_file_path = git_dir_path.join("config");
        let mut git_file = File::create(&git_file_path)?;
        writeln!(git_file, "git config file")?;

        // Create destination directory
        let dst_dir = tempdir()?;
        let dst_path = dst_dir.path().to_path_buf();

        // Execute the function under test
        copy_dir_contents(&src_path, &dst_path)?;

        // Verify the results
        assert!(dst_path.join("file1.txt").exists());
        assert!(dst_path.join("subdir").exists());
        assert!(dst_path.join("subdir/file2.txt").exists());

        // The .git directory should be skipped
        assert!(!dst_path.join(".git").exists());

        // Check file contents
        let copied_content1 = fs::read_to_string(dst_path.join("file1.txt"))?;
        assert_eq!(copied_content1.trim(), "Test content 1");

        let copied_content2 = fs::read_to_string(dst_path.join("subdir/file2.txt"))?;
        assert_eq!(copied_content2.trim(), "Test content 2");

        Ok(())
    }
}
