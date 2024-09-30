use std::{
    io::{stdout, Write},
    sync::{Arc, Mutex},
};

use magus::world::value::{OutputPort, PortType};

fn main() {
    let write_port = Arc::new(Mutex::new(stdout()));
    let mode = PortType::Textual;
    let mut output_port = OutputPort::from((write_port.clone(), mode));

    println!("{:?}", write!(output_port, "coolio\n"));
    println!("{:?}", output_port.write(&[0xce]));
    if let Ok(mut port) = write_port.lock() {
        port.write_all(&[0xbb, 0x33, 0x33]).unwrap();
        // port.flush().unwrap();
    }
    // println!("{:?}", output_port.write(&[0xce, 0xbb]));
    output_port.close();
    println!("{:?}", write!(output_port, "coolio"));
}
