use std::{env, fs, path::PathBuf, process::Command};

use anyhow::{bail, ensure, Context as _};
use bpaf::{Bpaf, Parser};
use regex::Regex;
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

        // (1) Set up the runtime directory
        let proj_root = get_proj_root()?;

        // cd to the runtime directory
        let lang_root = proj_root.join("runtimes").join(self.lang.to_string());
        env::set_current_dir(&lang_root)?;

        // Download the test cases
        Command::new("oj").arg("download").arg(&url).status()?;

        let solution = proj_root.join("contests").join(contest).join(file);
        let runtime_path = lang_root.join(self.lang.entrypoint());

        // Copy the solution file if it exists
        if solution.exists() {
            fs::copy(&solution, &runtime_path)
                .context("Failed to copy an existing solution file")?;
        }

        // (2) Solve the task
        // Open the editor
        Command::new("nix")
            .arg("develop")
            .arg(format!("{}#{}", proj_root.display(), self.lang))
            .arg("--command")
            .arg("nvim")
            .arg(runtime_path.display().to_string())
            .env("URL", url)
            .status()?;

        // Save the solution file
        Command::new("mkdir")
            .arg("-p")
            .arg(solution.parent().unwrap())
            .status()?;
        fs::copy(&runtime_path, &solution).context("Failed to save the solution file")?;

        // (3) Clean up the runtime directory
        // Remove the test directory
        let test_dir = lang_root.join("test");
        if test_dir.exists() {
            fs::remove_dir_all(test_dir).context("Failed to clean up test directory")?;
        }

        // Restore the runtime file
        Command::new("git")
            .arg("restore")
            .arg(runtime_path)
            .status()?;

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
