use argi::cli;

fn main() {
    cli!(
        help: "Demo command-line utility",
        run: (|_, _| println!("Hello, world!"))
    )
    .launch();
}
