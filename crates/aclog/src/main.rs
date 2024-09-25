use aclog::cmds::Cmds;
use anyhow::Result;

fn main() -> Result<()> {
    Cmds::dispatch()?;

    Ok(())
}
