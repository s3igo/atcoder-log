use std::{fs, path::PathBuf, process::Command};

use anyhow::{Context as _, bail, ensure};
use bpaf::{Bpaf, Parser};

use super::Run;
use crate::{langs::Lang, workspace_info::WorkspaceInfo};

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(parser))]
pub struct Close {
    /// Path to the temporary directory to close
    #[bpaf(positional("DIRECTORY"))]
    pub(crate) dir: PathBuf,
}

// Custom parser for `Close`
pub fn close() -> impl Parser<Close> {
    parser()
}

impl Run for Close {
    fn run(&self) -> anyhow::Result<()> {
        // Check if directory exists
        if !self.dir.exists() || !self.dir.is_dir() {
            bail!("Directory does not exist: {}", self.dir.display());
        }

        // Extract workspace info from directory name
        let dir_name = self.dir.file_name().unwrap_or_default();
        let workspace_info = WorkspaceInfo::try_from_dir_name(dir_name).with_context(|| {
            format!(
                "Failed to parse workspace info from directory name: {}",
                dir_name.to_string_lossy()
            )
        })?;

        // Get project root
        let proj_root = get_proj_root()?;

        // Convert lang string to Lang enum
        let lang = Lang::from(&workspace_info.lang)
            .with_context(|| format!("Invalid language: {}", workspace_info.lang))?;

        // Use Lang::entrypoint() to get the source file path
        let source_file = self.dir.join(lang.entrypoint());

        if !source_file.exists() {
            bail!("Source file does not exist: {}", source_file.display());
        }

        let dest_dir = proj_root.join("contests").join(&workspace_info.contest);
        let dest_file = dest_dir.join(&workspace_info.file);

        // Create destination directory if it doesn't exist
        fs::create_dir_all(&dest_dir).context("Failed to create destination directory")?;

        // Copy the solution file to the destination
        fs::copy(&source_file, &dest_file)
            .with_context(|| format!("Failed to copy solution file to {}", dest_file.display()))?;

        eprintln!("Saved solution to: {}", dest_file.display());

        // Remove the temporary directory
        fs::remove_dir_all(&self.dir).context(format!(
            "Failed to remove temporary directory: {}",
            self.dir.display()
        ))?;

        eprintln!("Removed workspace: {}", self.dir.display());

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
