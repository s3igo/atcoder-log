use std::path::PathBuf;

use anyhow::Result;
use bpaf::Bpaf;

use super::Run;
use crate::langs;

#[derive(Debug, Clone, Bpaf)]
pub struct Open {
    #[bpaf(external(langs::lang))]
    pub(crate) lang: langs::Lang,

    #[bpaf(positional("FILE"))]
    pub(crate) file: PathBuf,
}

impl Run for Open {
    fn run(&self) -> Result<()> {
        println!("Opening file {:?} for language {:?}", self.file, self.lang);
        Ok(())
    }
}
