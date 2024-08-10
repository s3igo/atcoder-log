use procon_log::args::Args;

fn main() {
    match Args::parse() {
        Args::Open { lang, file } => {
            println!("Opening file {:?} for language {:?}", file, lang);
        },
        Args::Test { lang, file } => {
            println!("Testing file {:?} for language {:?}", file, lang);
        },
        Args::Submit { lang, file } => {
            println!("Submitting file {:?} for language {:?}", file, lang);
        },
        Args::Save { lang, file } => {
            println!("Saving file {:?} for language {:?}", file, lang);
        },
        Args::Clear { lang, file } => {
            println!("Clearing file {:?} for language {:?}", file, lang);
        },
        Args::Version { .. } => {
            println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        },
    }
}
