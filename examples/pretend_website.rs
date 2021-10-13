use argi::{cli, data};

fn main() {
    cli! {
        help: "Demo application which launches something",
        run: (|ctx, _| {
            let addr = data!(ctx => --address).unwrap();
            let port = data!(u16, ctx => --port).unwrap();
            println!("Address found: {}\nPort found: {}", addr, port);
        }),
        --address -a [text]: {
            help: "Address to bind to",
        },
        --port -p [port]: {
            help: "Port number from 0 to 65535",
        }
    }
    .launch();
}
