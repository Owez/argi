// TODO: finish --empty --arg bug and finish
use argi::cli;

fn main() {
    cli!(
        help: "Quickstart app tutorial for argi",
        simple: {
            help: "Simple command with arguments",
            -a --append [path]: {
                help: "Appends a path",
            },
        }
        do_something [text?]: {
            help: "Does something with optional text",
            run: (|_, data| println!("{:?}", data)),
        }
        --empty-arg: {}
    )
    .launch();
}
