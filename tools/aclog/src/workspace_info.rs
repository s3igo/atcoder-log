use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context as _, bail};
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use url::Url;

use crate::langs::Lang;

/// Information about an `AtCoder` workspace
#[derive(Debug, Clone)]
pub struct WorkspaceInfo {
    contest: String,
    problem: String,
    lang: Lang,
    url: Option<String>,
    file: Option<String>,
}

const ATCODER_CONTEST_URL: &str = "https://atcoder.jp/contests";

impl WorkspaceInfo {
    /// Metadata file name
    pub const METADATA_FILE: &str = ".aclog.toml";

    /// Creates a new `WorkspaceInfo` from a URL
    ///
    /// # Errors
    /// Returns an error if the URL cannot be parsed
    pub fn try_from_url(url: &str, file: Option<&str>, lang: Lang) -> anyhow::Result<Self> {
        // Parse the URL to extract contest information
        let parsed_url = Url::parse(url).context("Invalid URL")?;
        let contest = parsed_url
            .path_segments()
            .and_then(|mut segments| segments.nth(1))
            .context("No contest found")?
            .to_string();

        // Determine file name based on provided args or extract from URL
        let file_str = match file {
            Some(file) if file.contains('.') => file.to_string(),
            Some(file) => format!("{}.{}", file, lang.extension()),
            None => {
                if url.starts_with(ATCODER_CONTEST_URL) {
                    Regex::new(r"/tasks/([a-z0-9_]+)/?$")?
                        .captures(url)
                        .and_then(|captures| {
                            captures
                                .get(1)
                                .map(|m| format!("{}.{}", m.as_str(), lang.extension()))
                        })
                        .context("No task ID found")?
                } else {
                    bail!("Cannot guess filename from URL")
                }
            },
        };

        // Get the clean problem ID (without extension)
        let problem = file_str.split('.').next().unwrap_or(&file_str).to_string();

        Ok(Self {
            contest,
            problem,
            lang,
            url: Some(url.to_string()),
            file: Some(file_str),
        })
    }

    /// Creates a new `WorkspaceInfo` from file and contest information
    ///
    /// # Errors
    /// Returns an error if required information is missing
    pub fn try_from_args(
        file: Option<&str>,
        contest: Option<&str>,
        lang: Lang,
    ) -> anyhow::Result<Self> {
        let contest = contest.context("Contest must be specified when URL is not provided")?;

        // Determine file name
        let file_str = match file {
            Some(file) if file.contains('.') => file.to_string(),
            Some(file) => format!("{}.{}", file, lang.extension()),
            None => bail!("File must be specified when URL is not provided"),
        };

        // Get the clean problem ID (without extension)
        let problem = file_str.split('.').next().unwrap_or(&file_str).to_string();

        // Construct URL
        let url = format!("{ATCODER_CONTEST_URL}/{contest}/tasks/{problem}");

        Ok(Self {
            contest: contest.to_string(),
            problem,
            lang,
            url: Some(url),
            file: Some(file_str),
        })
    }

    /// Save metadata to the specified directory
    ///
    /// # Errors
    /// Returns an error if writing the metadata file fails
    pub fn save_metadata(&self, dir: &Path) -> anyhow::Result<()> {
        let metadata_path = dir.join(Self::METADATA_FILE);

        let toml_content = toml::to_string(self)?;
        fs::write(metadata_path, toml_content)?;

        Ok(())
    }

    /// Load metadata from the specified directory
    ///
    /// # Errors
    /// Returns an error if reading or parsing the metadata file fails
    pub fn load_metadata(dir: &Path) -> anyhow::Result<Self> {
        let metadata_path = dir.join(Self::METADATA_FILE);

        let toml_content = fs::read_to_string(&metadata_path).with_context(|| {
            format!("Failed to read metadata file: {}", metadata_path.display())
        })?;

        let workspace_info: Self = toml::from_str(&toml_content)?;
        Ok(workspace_info)
    }

    /// Parse workspace info from directory name
    ///
    /// Returns `None` if the directory name doesn't match the expected format
    ///
    /// This is kept for backward compatibility with existing workspaces
    #[must_use]
    #[allow(clippy::similar_names)]
    pub fn parse_from_dir_name(dir_name: &OsStr) -> Option<Self> {
        let dir_name = dir_name.to_string_lossy();

        // Format: aclog-atcoder-{contest}-{filename}-{lang}-
        let re = Regex::new(r"^aclog-atcoder-([^-]+)-([^-]+)-([^-]+)-").ok()?;

        if let Some(captures) = re.captures(&dir_name) {
            if captures.len() >= 4 {
                let lang_str = captures[3].to_string();
                let lang = Lang::from(&lang_str).unwrap_or({
                    // Fallback to a default language if parsing fails
                    Lang::Rust(())
                });

                return Some(Self {
                    contest: captures[1].to_string(),
                    problem: captures[2].to_string(),
                    lang,
                    url: None,
                    file: None,
                });
            }
        }

        None
    }

    /// Parse workspace info from directory
    ///
    /// # Errors
    /// Returns an error if no valid metadata can be found
    pub fn try_from_dir(dir: &Path) -> anyhow::Result<Self> {
        // First try to load metadata file
        let metadata_path = dir.join(Self::METADATA_FILE);
        if metadata_path.exists() {
            return Self::load_metadata(dir);
        }

        // Fallback to old method: parse from directory name
        let dir_name = dir.file_name().unwrap_or_default();
        Self::parse_from_dir_name(dir_name).ok_or_else(|| {
            let dir_name = dir_name.to_string_lossy();
            anyhow::anyhow!(
                "No metadata file found and invalid workspace directory name: {}",
                dir_name
            )
        })
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

    /// Get contest name
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn contest(&self) -> &str {
        &self.contest
    }

    /// Get problem ID
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn problem(&self) -> &str {
        &self.problem
    }

    /// Get language
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn lang(&self) -> &Lang {
        &self.lang
    }

    /// Generate temporary directory prefix
    #[must_use]
    pub fn temp_dir_prefix(&self) -> String {
        format!(
            "aclog-atcoder-{}-{}-{}-",
            self.contest, self.problem, self.lang
        )
    }

    /// Get file path in solution directory
    #[must_use]
    pub fn solution_path(&self, proj_root: &Path) -> PathBuf {
        let file_name = self.file.as_ref().map_or_else(
            || format!("{}.{}", self.problem, self.get_extension()),
            std::clone::Clone::clone,
        );

        proj_root
            .join("contests")
            .join(&self.contest)
            .join(file_name)
    }

    /// Get full URL for the problem
    #[must_use]
    pub fn get_url(&self) -> String {
        self.url.clone().unwrap_or_else(|| {
            format!(
                "{}/{}/tasks/{}",
                ATCODER_CONTEST_URL, self.contest, self.problem
            )
        })
    }

    /// Get file extension for the language
    #[must_use]
    pub const fn get_extension(&self) -> &str {
        self.lang.extension()
    }

    /// Get entrypoint file path for the language
    #[must_use]
    pub fn get_entrypoint(&self) -> String {
        self.lang.entrypoint()
    }
}

// Helper struct for serialization/deserialization
#[derive(Debug, Serialize, Deserialize)]
struct SerializableWorkspaceInfo {
    contest: String,
    problem: String,
    lang: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    file: Option<String>,
}

// Manual implementation of Serialize for WorkspaceInfo
impl Serialize for WorkspaceInfo {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let serializable = SerializableWorkspaceInfo {
            contest: self.contest.clone(),
            problem: self.problem.clone(),
            lang: self.lang.to_string(),
            url: self.url.clone(),
            file: self.file.clone(),
        };
        serializable.serialize(serializer)
    }
}

// Manual implementation of Deserialize for WorkspaceInfo
impl<'de> Deserialize<'de> for WorkspaceInfo {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let serializable = SerializableWorkspaceInfo::deserialize(deserializer)?;
        let lang = Lang::from(&serializable.lang).map_err(serde::de::Error::custom)?;

        Ok(Self {
            contest: serializable.contest,
            problem: serializable.problem,
            lang,
            url: serializable.url,
            file: serializable.file,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::{ffi::OsString, path::PathBuf};

    use tempfile::tempdir;

    use super::*;
    use crate::langs::Lang;

    #[test]
    fn test_try_from_url() -> anyhow::Result<()> {
        let url = "https://atcoder.jp/contests/abc123/tasks/abc123_a";
        let lang = Lang::Rust(());

        let info = WorkspaceInfo::try_from_url(url, None, lang)?;

        assert_eq!(info.contest(), "abc123");
        assert_eq!(info.problem(), "abc123_a");
        assert_eq!(info.lang().to_string(), "rust");
        assert_eq!(info.url, Some(url.to_string()));
        assert_eq!(info.file, Some("abc123_a.rs".to_string()));

        Ok(())
    }

    #[test]
    fn test_try_from_args() -> anyhow::Result<()> {
        let file = "problem_b";
        let contest = "abc123";
        let lang = Lang::Cpp(());

        let info = WorkspaceInfo::try_from_args(Some(file), Some(contest), lang)?;

        assert_eq!(info.contest(), "abc123");
        assert_eq!(info.problem(), "problem_b");
        assert_eq!(info.lang().to_string(), "cpp");
        assert_eq!(info.file, Some("problem_b.cpp".to_string()));
        assert!(info.url.is_some());
        assert!(info.url.unwrap().contains("abc123/tasks/problem_b"));

        Ok(())
    }

    #[test]
    fn test_parse_from_dir_name_valid() {
        let dir_name = OsString::from("aclog-atcoder-abc123-problem-rust-abcdef");
        let info = WorkspaceInfo::parse_from_dir_name(&dir_name);

        assert!(info.is_some());
        let info = info.unwrap();
        assert_eq!(info.contest(), "abc123");
        assert_eq!(info.problem(), "problem");
        assert_eq!(info.lang().to_string(), "rust");
    }

    #[test]
    fn test_parse_from_dir_name_valid_with_underscore() {
        let dir_name = OsString::from("aclog-atcoder-abc123-practice_1-zig-abcdef");
        let info = WorkspaceInfo::parse_from_dir_name(&dir_name);

        assert!(info.is_some());
        let info = info.unwrap();
        assert_eq!(info.contest(), "abc123");
        assert_eq!(info.problem(), "practice_1");
        assert_eq!(info.lang().to_string(), "zig");
    }

    #[test]
    fn test_parse_from_dir_name_invalid() {
        let dir_name = OsString::from("invalid-directory-name");
        let info = WorkspaceInfo::parse_from_dir_name(&dir_name);
        assert!(info.is_none());
    }

    #[test]
    fn test_try_from_dir_name_valid() {
        let dir_name = OsString::from("aclog-atcoder-abc123-problem-rust-abcdef");
        let result = WorkspaceInfo::try_from_dir_name(&dir_name);

        assert!(result.is_ok());
        let info = result.unwrap();
        assert_eq!(info.contest(), "abc123");
        assert_eq!(info.problem(), "problem");
        assert_eq!(info.lang().to_string(), "rust");
    }

    #[test]
    fn test_try_from_dir_name_valid_with_underscore() {
        let dir_name = OsString::from("aclog-atcoder-abc123-practice_1-zig-abcdef");
        let result = WorkspaceInfo::try_from_dir_name(&dir_name);

        assert!(result.is_ok());
        let info = result.unwrap();
        assert_eq!(info.contest(), "abc123");
        assert_eq!(info.problem(), "practice_1");
        assert_eq!(info.lang().to_string(), "zig");
    }

    #[test]
    fn test_try_from_dir_name_invalid() {
        let dir_name = OsString::from("invalid-directory-name");
        let result = WorkspaceInfo::try_from_dir_name(&dir_name);

        assert!(result.is_err());
    }

    #[test]
    fn test_save_and_load_metadata() -> anyhow::Result<()> {
        let temp_dir = tempdir()?;
        let temp_path = temp_dir.path();

        let info = WorkspaceInfo::try_from_args(Some("problem_d"), Some("abc123"), Lang::Rust(()))?;

        // Save metadata
        info.save_metadata(temp_path)?;

        // Load metadata
        let loaded_info = WorkspaceInfo::load_metadata(temp_path)?;

        // Verify
        assert_eq!(loaded_info.contest(), "abc123");
        assert_eq!(loaded_info.problem(), "problem_d");
        assert_eq!(loaded_info.lang().to_string(), "rust");

        Ok(())
    }

    #[test]
    fn test_temp_dir_prefix() {
        // We need to create WorkspaceInfo through a constructor to maintain
        // encapsulation
        let info = WorkspaceInfo::try_from_args(Some("problem_d"), Some("abc123"), Lang::Rust(()))
            .unwrap();

        assert_eq!(
            info.temp_dir_prefix(),
            "aclog-atcoder-abc123-problem_d-rust-"
        );
    }

    #[test]
    fn test_solution_path() {
        let proj_root = PathBuf::from("/tmp/project");
        // We need to create WorkspaceInfo through a constructor to maintain
        // encapsulation
        let info = WorkspaceInfo::try_from_args(Some("problem_d"), Some("abc123"), Lang::Rust(()))
            .unwrap();

        let expected_path = PathBuf::from("/tmp/project/contests/abc123/problem_d.rs");
        assert_eq!(info.solution_path(&proj_root), expected_path);
    }
}
