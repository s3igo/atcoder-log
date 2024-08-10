use std::path::PathBuf;

use anyhow::Result;
use bpaf::Bpaf;

use super::Run;
use crate::langs;

#[derive(Debug, Clone, Bpaf)]
pub struct Submit {
    #[bpaf(external(langs::lang))]
    pub(crate) lang: langs::Lang,

    #[bpaf(positional("FILE"))]
    pub(crate) file: PathBuf,
}

impl Run for Submit {
    fn run(&self) -> Result<()> {
        println!(
            "Submitting file {:?} for language {:?}",
            self.file, self.lang
        );
        Ok(())
    }
}
