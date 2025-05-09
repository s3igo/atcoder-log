use std::ffi::OsStr;

use regex::Regex;

/// Information about an AtCoder workspace
#[derive(Debug, Clone)]
pub struct WorkspaceInfo {
    pub contest: String,
    pub file: String,
    pub lang: String,
}

impl WorkspaceInfo {
    /// Parse workspace info from directory name
    ///
    /// Returns `None` if the directory name doesn't match the expected format
    pub fn parse_from_dir_name(dir_name: &OsStr) -> Option<Self> {
        let dir_name = dir_name.to_string_lossy();

        // Format: aclog-atcoder-{contest}-{filename}_{extension}-{lang}-
        let re = Regex::new(r"^aclog-atcoder-([^-]+)-([^_]+)_([^-]+)-([^-]+)-").ok()?;

        if let Some(captures) = re.captures(&dir_name) {
            if captures.len() >= 5 {
                return Some(Self {
                    contest: captures[1].to_string(),
                    file: format!("{}.{}", captures[2].to_string(), captures[3].to_string()),
                    lang: captures[4].to_string(),
                });
            }
        }

        None
    }

    /// Parse workspace info from directory name
    ///
    /// # Errors
    /// Returns an error if the directory name doesn't match the expected format
    pub fn try_from_dir_name(dir_name: &OsStr) -> anyhow::Result<Self> {
        Self::parse_from_dir_name(dir_name).ok_or_else(|| {
            let dir_name = dir_name.to_string_lossy();
            anyhow::anyhow!("Invalid workspace directory name format: {}", dir_name)
        })
    }
}
