use std::{
    cell::RefCell,
    io::{Write, stdout},
    rc::Rc,
};

use magus::runtime::port::{OutputPort, PortType};

fn main() {
    let write_port = Rc::new(RefCell::new(stdout()));
    let mode = PortType::Textual;
    let mut output_port = OutputPort::from((write_port.clone(), mode));

    println!("{:?}", write!(output_port, "coolio\n"));
    println!("{:?}", output_port.write(&[0xce]));
    output_port.write_all(&[0xbb, 0x33, 0x33]).unwrap();
    // port.flush().unwrap();
    // println!("{:?}", output_port.write(&[0xce, 0xbb]));
    output_port.close();
    println!("{:?}", write!(output_port, "coolio"));
}
