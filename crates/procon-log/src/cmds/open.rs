use std::process::Command;

use anyhow::{bail, ensure, Context as _, Result};
use bpaf::{Bpaf, Parser};
use regex::Regex;

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
    fn run(&self) -> Result<()> {
        // Filename to open
        let target = match &self.file {
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

        // TODO: Use url crate

        let (url, contest) = match &self.url {
            Some(url) => (
                url.to_string(),
                url.split('/')
                    .nth(4)
                    .context("No contest found")?
                    .to_string(),
            ),
            None => {
                let contest = self.contest.as_ref().unwrap(); // guarded by the parser
                (
                    format!("{ATCODER_CONTEST_URL}/tasks/{contest}"),
                    contest.to_string(),
                )
            },
        };

        // TODO: cd to contest dir before opening the file

        Command::new("oj").arg("download").arg(url).status()?;

        let proj_root = get_proj_root()?;

        Command::new("nvim")
            .arg(format!("{proj_root}/contests/{contest}/{target}"))
            .status()?;

        // TODO: Cleanup test dir

        Ok(())
    }
}

fn get_proj_root() -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()?;

    ensure!(output.status.success(), "Failed to get project root");

    Ok(String::from_utf8(output.stdout)?.trim().to_string())
}
