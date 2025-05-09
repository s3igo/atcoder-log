use std::{env, ffi::OsStr, fs};

use anyhow::Context as _;
use bpaf::{Bpaf, Parser};
use regex::Regex;

use super::Run;

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(parser))]
pub struct List {}

#[derive(Debug)]
struct WorkspaceInfo {
    contest: String,
    file: String,
    lang: String,
}

// Custom parser for `List`
pub fn list() -> impl Parser<List> {
    parser()
}

impl Run for List {
    fn run(&self) -> anyhow::Result<()> {
        // Find temporary directories with our specific prefix
        let temp_dir = env::temp_dir();

        // Iterate through entries in the temp directory
        let entries = fs::read_dir(&temp_dir).with_context(|| {
            format!("Failed to read temporary directory: {}", temp_dir.display())
        })?;

        // Compile the regex once
        // Format: atcoder-{contest}-{filename}_{extension}-{lang}-
        let dir_pattern = Regex::new(r"^atcoder-([^-]+)-([^_]+)_([^-]+)-([^-]+)-").ok();

        // Filter entries that match our format (atcoder-*)
        let mut found = false;
        for entry in entries {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                let file_name = entry.file_name();
                let file_name_str = file_name.to_string_lossy();

                if file_name_str.starts_with("atcoder-") {
                    if let Some(info) = parse_workspace_info(file_name.as_ref(), &dir_pattern) {
                        // Output the path to stdout with metadata info
                        println!("{}", path.display());
                        eprintln!(
                            "  Contest: {}, Problem: {}, Lang: {}",
                            info.contest, info.file, info.lang
                        );
                        found = true;
                    }
                }
            }
        }

        if !found {
            eprintln!("No active AtCoder workspaces found.");
        }

        Ok(())
    }
}

fn parse_workspace_info(dir_name: &OsStr, regex: &Option<Regex>) -> Option<WorkspaceInfo> {
    let dir_name = dir_name.to_string_lossy();

    if let Some(re) = regex {
        if let Some(captures) = re.captures(&dir_name) {
            if captures.len() >= 5 {
                // Format: atcoder-{contest}-{filename}_{extension}-{lang}-
                return Some(WorkspaceInfo {
                    contest: captures[1].to_string(),
                    file: format!("{}.{}", captures[2].to_string(), captures[3].to_string()),
                    lang: captures[4].to_string(),
                });
            }
        }
    }

    None
}
