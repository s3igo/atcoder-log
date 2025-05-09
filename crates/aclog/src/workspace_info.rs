use std::ffi::OsStr;

use regex::Regex;

/// Information about an `AtCoder` workspace
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
                    file: format!("{}.{}", &captures[2], &captures[3]),
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

#[cfg(test)]
mod tests {
    use std::ffi::OsString;

    use super::*;

    #[test]
    fn test_parse_from_dir_name_valid() {
        let dir_name = OsString::from("aclog-atcoder-abc123-problem_rs-rust-abcdef");
        let info = WorkspaceInfo::parse_from_dir_name(&dir_name);

        assert!(info.is_some());
        let info = info.unwrap();
        assert_eq!(info.contest, "abc123");
        assert_eq!(info.file, "problem.rs");
        assert_eq!(info.lang, "rust");
    }

    #[test]
    fn test_parse_from_dir_name_invalid() {
        let dir_name = OsString::from("invalid-directory-name");
        let info = WorkspaceInfo::parse_from_dir_name(&dir_name);
        assert!(info.is_none());
    }

    #[test]
    fn test_try_from_dir_name_valid() {
        let dir_name = OsString::from("aclog-atcoder-abc123-problem_rs-rust-abcdef");
        let result = WorkspaceInfo::try_from_dir_name(&dir_name);

        assert!(result.is_ok());
        let info = result.unwrap();
        assert_eq!(info.contest, "abc123");
        assert_eq!(info.file, "problem.rs");
        assert_eq!(info.lang, "rust");
    }

    #[test]
    fn test_try_from_dir_name_invalid() {
        let dir_name = OsString::from("invalid-directory-name");
        let result = WorkspaceInfo::try_from_dir_name(&dir_name);

        assert!(result.is_err());
    }
}
