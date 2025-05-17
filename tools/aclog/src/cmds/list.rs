use std::{cmp, env, fs, time::SystemTime};

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

        // Collect workspaces first to calculate max field lengths
        let mut workspaces = Vec::new();
        let mut max_path_len = "PATH".len();
        let mut max_contest_len = "CONTEST".len();
        let mut max_problem_len = "PROBLEM".len();
        let mut max_lang_len = "LANGUAGE".len();
        let mut max_created_len = "CREATED".len();

        // First pass: collect data and calculate max field lengths
        for entry in entries {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                let file_name = entry.file_name();
                let file_name_str = file_name.to_string_lossy();

                // Check if this is an aclog workspace by looking for the .aclog.toml file
                // or falling back to the directory name check
                let metadata_file = path.join(WorkspaceInfo::METADATA_FILE);
                let is_aclog_dir =
                    metadata_file.exists() || file_name_str.starts_with("aclog-atcoder-");

                if is_aclog_dir {
                    if let Ok(info) = WorkspaceInfo::try_from_dir(&path) {
                        // Get creation time (display "unknown" if not available)
                        let created_time =
                            path.metadata().and_then(|meta| meta.created()).map_or_else(
                                |_| "unknown".to_string(),
                                |time| {
                                    let duration =
                                        SystemTime::now().duration_since(time).unwrap_or_default();
                                    if duration.as_secs() < 60 {
                                        format!("{}s ago", duration.as_secs())
                                    } else if duration.as_secs() < 3600 {
                                        format!("{}m ago", duration.as_secs() / 60)
                                    } else if duration.as_secs() < 86400 {
                                        format!("{}h ago", duration.as_secs() / 3600)
                                    } else {
                                        format!("{}d ago", duration.as_secs() / 86400)
                                    }
                                },
                            );

                        // Update max field lengths
                        let path_str = path.display().to_string();
                        max_path_len = cmp::max(max_path_len, path_str.len());
                        max_contest_len = cmp::max(max_contest_len, info.contest().len());
                        max_problem_len = cmp::max(max_problem_len, info.problem().len());
                        max_lang_len = cmp::max(max_lang_len, info.lang().to_string().len());
                        max_created_len = cmp::max(max_created_len, created_time.len());

                        // Store workspace data with constructed filename
                        workspaces.push((path_str, info, created_time));
                    }
                }
            }
        }

        // Check if any workspaces were found
        let found = !workspaces.is_empty();

        if found {
            // Add padding to max field lengths (3 characters)
            max_path_len += 3;
            max_contest_len += 3;
            max_problem_len += 3;
            max_lang_len += 3;
            max_created_len += 3;

            // Output table headers with calculated widths
            println!(
                "{:<width_path$} {:<width_contest$} {:<width_problem$} {:<width_lang$} \
                 {:<width_created$}",
                "PATH",
                "CONTEST",
                "PROBLEM",
                "LANGUAGE",
                "CREATED",
                width_path = max_path_len,
                width_contest = max_contest_len,
                width_problem = max_problem_len,
                width_lang = max_lang_len,
                width_created = max_created_len
            );
        }

        // Second pass: output data with calculated widths if found
        if found {
            for (path, info, created_time) in workspaces {
                println!(
                    "{:<width_path$} {:<width_contest$} {:<width_problem$} {:<width_lang$} \
                     {:<width_created$}",
                    path,
                    info.contest(),
                    info.problem(),
                    info.lang().to_string(),
                    created_time,
                    width_path = max_path_len,
                    width_contest = max_contest_len,
                    width_problem = max_problem_len,
                    width_lang = max_lang_len,
                    width_created = max_created_len
                );
            }
        }

        if !found {
            eprintln!("No active AtCoder workspaces found.");
        }

        Ok(())
    }
}
