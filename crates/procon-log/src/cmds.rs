pub(crate) mod open;

use anyhow::Result;

pub trait Run {
    fn run(&self) -> Result<()>;
}
