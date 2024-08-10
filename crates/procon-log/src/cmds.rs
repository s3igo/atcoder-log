pub(crate) mod clear;
pub(crate) mod open;
pub(crate) mod save;
pub(crate) mod submit;
pub(crate) mod test;

use anyhow::Result;

pub trait Run {
    fn run(&self) -> Result<()>;
}
