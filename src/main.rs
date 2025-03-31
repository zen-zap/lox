use clap::{Parser, Subcommand}; // command line argument parser
use codecrafters_interpreter::*;
use miette::{IntoDiagnostic, WrapErr};
use std::fs;
use std::path::PathBuf;

/// type to help us parse the command line arguments
#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

/// holds the Command types argument type
#[derive(Debug, Subcommand)]
enum Commands {
    /// takes a file path for tokenization
    Tokenize { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading {} file failed!", filename.display()))?;

            // file_contents.push('\0'); // they don't have it in general files

            let lexer = Lexer::new(&file_contents);

            for token in lexer {
                let token = token?;

                println!("{token}");
            }

            println!("EOF  null");
        }
    }

    Ok(())
}
