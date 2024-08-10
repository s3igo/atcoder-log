pub(crate) mod open;
pub(crate) mod test;
pub(crate) mod submit;
pub(crate) mod save;
pub(crate) mod clear;

use anyhow::Result;

pub trait Run {
    fn run(&self) -> Result<()>;
}
