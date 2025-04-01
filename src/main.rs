#![allow(unreachable_code)]
use crate::token_type::{SingleTokenError, StringTerminationError};
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

    let mut erry = false;

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
                let token = match token {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{e:?}");

                        if let Some(unk) = e.downcast_ref::<SingleTokenError>() {
                            erry = true;

                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                unk.line(),
                                unk.token
                            );                            
                        }

                        else if let Some(ust) = e.downcast_ref::<StringTerminationError>() {
                            erry = true;

                            eprintln!(
                                "[line {}] Error: Unterminated string.",
                                ust.line()
                            );
                        }

                        continue;

                    }
                };

                println!("{token}");
            }

            println!("EOF  null");
        }
    }

    if erry {
        std::process::exit(65);
    } else {
        std::process::exit(0);
    }

    // dead_code
    Ok(())
}
