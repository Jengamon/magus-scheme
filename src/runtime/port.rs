use core::fmt;
use std::{
    any::Any,
    cell::RefCell,
    io::{Read, Write},
    rc::Rc,
};

use gc_arena::Collect;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PortType {
    Textual,
    Binary,
}

impl fmt::Display for PortType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PortType::Textual => write!(f, "textual"),
            PortType::Binary => write!(f, "binary"),
        }
    }
}

// I/O ports that provide a threadsafe way to input/output bytes
pub trait Writeable: Write + Any {
    // upcasting ftw
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Write + Any> Writeable for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

/// Output port
///
/// Defaults to blocking write, but the inner mutex is
/// accessible, so non-blocking write can be implemented.
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct OutputPort {
    pub port: Option<Rc<RefCell<dyn Writeable>>>,
    port_type: PortType,
}

impl OutputPort {
    pub fn close(&mut self) -> Option<Rc<RefCell<dyn Writeable>>> {
        self.port.take()
    }

    pub fn is_closed(&self) -> bool {
        self.port.is_none()
    }

    pub fn port_type(&self) -> PortType {
        self.port_type
    }
}

impl<T: Writeable> From<(Rc<RefCell<T>>, PortType)> for OutputPort {
    fn from((port, port_type): (Rc<RefCell<T>>, PortType)) -> Self {
        Self {
            port: Some(port),
            port_type,
        }
    }
}

impl fmt::Debug for OutputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<output {} port {:p}>", self.port_type, &self.port)
    }
}

impl Write for OutputPort {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(port) = self.port.as_ref() {
            let mut writer = port.borrow_mut();
            // Port type affects *what* we write
            //
            // If we are in textual mode, convert the input bytes into UTF-8, then write
            match self.port_type {
                PortType::Binary => writer.write(buf),
                PortType::Textual => {
                    let text = String::from_utf8_lossy(buf);
                    writer.write(text.as_bytes())
                }
            }
        } else {
            Err(std::io::Error::other("output port was closed"))?
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(port) = self.port.as_ref() {
            let mut writer = port.borrow_mut();
            writer.flush()
        } else {
            Err(std::io::Error::other("output port was closed"))?
        }
    }
}

pub trait Readable: Read + Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Read + Any> Readable for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

/// Input port
///
/// Defaults to
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct InputPort {
    pub port: Option<Rc<RefCell<dyn Readable>>>,
    port_type: PortType,
}

impl InputPort {
    pub fn close(&mut self) -> Option<Rc<RefCell<dyn Readable>>> {
        self.port.take()
    }

    pub fn is_closed(&self) -> bool {
        self.port.is_none()
    }

    pub fn port_type(&self) -> PortType {
        self.port_type
    }
}

impl<T: Readable> From<(Rc<RefCell<T>>, PortType)> for InputPort {
    fn from((port, port_type): (Rc<RefCell<T>>, PortType)) -> Self {
        Self {
            port: Some(port),
            port_type,
        }
    }
}

impl fmt::Debug for InputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<input {} port {:p}>", self.port_type, &self.port)
    }
}

impl Read for InputPort {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if let Some(port) = self.port.as_ref() {
            let mut reader = port.borrow_mut();
            // Port type affects *what* we read
            //
            // If we are in textual mode, convert the input bytes into UTF-8, then read
            match self.port_type {
                PortType::Binary => reader.read(buf),
                PortType::Textual => {
                    let mut text = String::with_capacity(buf.len());
                    let res = reader.read_to_string(&mut text);
                    buf.copy_from_slice(text.as_bytes());
                    res
                }
            }
        } else {
            Err(std::io::Error::other("input port was closed"))?
        }
    }
}
