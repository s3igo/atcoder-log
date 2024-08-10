use std::path::PathBuf;

use anyhow::Result;
use bpaf::Bpaf;

use super::Run;
use crate::langs;

#[derive(Debug, Clone, Bpaf)]
pub struct Save {
    #[bpaf(external(langs::lang))]
    pub(crate) lang: langs::Lang,

    #[bpaf(positional("FILE"))]
    pub(crate) file: PathBuf,
}

impl Run for Save {
    fn run(&self) -> Result<()> {
        println!("Saving file {:?} for language {:?}", self.file, self.lang);
        Ok(())
    }
}
