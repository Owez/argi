use argi::cli;

fn main() {
    cli! {
        help: "Demo command-line utility"
        launch: {
            help: "Launches application",
            run: (|_, _| println!("Hello, world!"))
        }
    }
    .launch()
}
