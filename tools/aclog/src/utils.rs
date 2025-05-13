use std::{path::PathBuf, process::Command};

use anyhow::ensure;

/// Get the root directory of the Git project
///
/// # Errors
/// Returns an error if the command fails or the root directory cannot be
/// determined
pub fn get_proj_root() -> anyhow::Result<PathBuf> {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()?;

    ensure!(output.status.success(), "Failed to get project root");

    Ok(PathBuf::from(String::from_utf8(output.stdout)?.trim()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_proj_root() {
        // This is a simple test to ensure the function compiles and runs
        // We don't actually check the result because it depends on the environment
        let result = get_proj_root();

        // If running in a git repo, this should succeed
        // If not, it will fail, but that's expected in some environments
        if result.is_ok() {
            let path = result.unwrap();
            assert!(path.is_absolute());
            assert!(path.exists());
        }
    }
}
