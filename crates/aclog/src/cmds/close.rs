use std::{fs, path::PathBuf};

use anyhow::{Context as _, bail};
use bpaf::{Bpaf, Parser};

use super::Run;
use crate::{utils, workspace_info::WorkspaceInfo};

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(parser))]
pub struct Close {
    /// Delete the workspace without saving the solution
    #[bpaf(long, short)]
    pub(crate) delete: bool,

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
        let proj_root = utils::get_proj_root()?;

        // Use Lang::entrypoint() to get the source file path
        let source_file = self.dir.join(workspace_info.lang().entrypoint());

        if !source_file.exists() {
            bail!("Source file does not exist: {}", source_file.display());
        }

        // Only save the solution if --delete is not specified
        if self.delete {
            eprintln!("Deleting workspace without saving solution (--delete specified)");
        } else {
            // Get the destination path using WorkspaceInfo.solution_path
            let dest_file = workspace_info.solution_path(&proj_root);
            let dest_dir = dest_file.parent().unwrap_or(&proj_root);

            // Create destination directory if it doesn't exist
            fs::create_dir_all(dest_dir).context("Failed to create destination directory")?;

            // Copy the solution file to the destination
            fs::copy(&source_file, &dest_file).with_context(|| {
                format!("Failed to copy solution file to {}", dest_file.display())
            })?;

            eprintln!("Saved solution to: {}", dest_file.display());
        }

        // Remove the temporary directory
        fs::remove_dir_all(&self.dir).context(format!(
            "Failed to remove temporary directory: {}",
            self.dir.display()
        ))?;

        eprintln!("Removed workspace: {}", self.dir.display());

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn test_close_run_invalid_directory() {
        let nonexistent_dir = PathBuf::from("/path/to/nonexistent");
        let close = Close {
            dir: nonexistent_dir,
            delete: false,
        };

        let result = close.run();
        assert!(result.is_err());
    }

    #[test]
    fn test_close_run_with_delete_option() {
        let nonexistent_dir = PathBuf::from("/path/to/nonexistent");
        let close = Close {
            dir: nonexistent_dir,
            delete: true,
        };

        let result = close.run();
        assert!(result.is_err());
    }

    #[test]
    fn test_try_from_dir_name_invalid_format() {
        use std::ffi::OsString;

        let invalid_dir_name = OsString::from("invalid-dir-name");
        let result = WorkspaceInfo::try_from_dir_name(&invalid_dir_name);

        assert!(result.is_err());
    }
}
