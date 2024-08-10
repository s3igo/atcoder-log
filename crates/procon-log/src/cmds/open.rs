use anyhow::Result;
use bpaf::{Bpaf, Parser};

use super::Run;
use crate::langs;

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(parser))]
pub struct Open {
    /// Task URL to open
    #[bpaf(long, short, argument("URL"))]
    pub(crate) url: Option<String>,

    /// Filename if no URL is specified or if the filename cannot be guessed
    /// from the URL
    #[bpaf(long, short, argument("FILE"))]
    pub(crate) file: Option<String>,

    #[bpaf(external(langs::lang))]
    pub(crate) lang: langs::Lang,
}

// Custom parser for Open
pub fn open() -> impl Parser<Open> {
    parser().guard(
        |opts| opts.url.is_some() || opts.file.is_some(),
        "Either URL or FILE must be provided",
    )
}

impl Run for Open {
    fn run(&self) -> Result<()> {
        println!("{:?} {:?} {:?}", self.lang, self.url, self.file);
        Ok(())
    }
}
