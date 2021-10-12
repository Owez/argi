use argi::{cli, data};

fn main() {
    cli! {
        help: "Demo application which launches something",
        run: (|ctx, _| {
            println!("Address found: {}", data!(ctx => --address));
            println!("Port found: {}", data!(u16, ctx => --port));
        }),
        --address -a [text]: {
            help: "Address to bind to"
        },
        --port -p [port]: {
            help: "Port number from 0 to 65535"
        }
    }
    .launch();
}
