use anyhow::Result;
use procon_log::cmds::Cmds;

fn main() -> Result<()> {
    Cmds::dispatch()?;

    Ok(())
}
