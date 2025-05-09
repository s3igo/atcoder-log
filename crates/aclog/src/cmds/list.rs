use std::{env, fs};

use anyhow::Context as _;
use bpaf::{Bpaf, Parser};

use super::Run;
use crate::workspace_info::WorkspaceInfo;

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(parser))]
pub struct List {}

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

        // No need for regex compilation here as it's now encapsulated in
        // WorkspaceInfo::parse_from_dir_name

        // Filter entries that match our format (aclog-atcoder-*)
        let mut found = false;
        for entry in entries {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                let file_name = entry.file_name();
                let file_name_str = file_name.to_string_lossy();

                if file_name_str.starts_with("aclog-atcoder-") {
                    if let Some(info) = WorkspaceInfo::parse_from_dir_name(file_name.as_ref()) {
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

// Function removed as it's now implemented as a method on WorkspaceInfo
