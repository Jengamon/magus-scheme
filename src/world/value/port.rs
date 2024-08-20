use core::fmt;
use std::{
    any::Any,
    io::{Read, Write},
    sync::{Arc, Mutex},
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
pub trait Writeable: Write + Any {}
impl<T: Write + Any> Writeable for T {}
/// Output port
///
/// Defaults to blocking write, but the inner mutex is
/// accessible, so non-blocking write can be implemented.
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct OutputPort {
    pub port: Option<Arc<Mutex<dyn Writeable>>>,
    port_type: PortType,
}

impl OutputPort {
    pub fn close(&mut self) -> Option<Arc<Mutex<dyn Writeable>>> {
        self.port.take()
    }
}

impl<T: Writeable> From<(Arc<Mutex<T>>, PortType)> for OutputPort {
    fn from((port, port_type): (Arc<Mutex<T>>, PortType)) -> Self {
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
            if let Ok(mut writer) = port.lock() {
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
                Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "output port was poisoned",
                ))?
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "output port was closed",
            ))?
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(port) = self.port.as_ref() {
            if let Ok(mut writer) = port.lock() {
                writer.flush()
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "output port was poisoned",
                ))?
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "output port was closed",
            ))?
        }
    }
}

pub trait Readable: Read + Any {}
impl<T: Read + Any> Readable for T {}
/// Input port
///
/// Defaults to
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct InputPort {
    pub port: Option<Arc<Mutex<dyn Readable>>>,
    port_type: PortType,
}

impl InputPort {
    pub fn close(&mut self) -> Option<Arc<Mutex<dyn Readable>>> {
        self.port.take()
    }
}

impl<T: Readable> From<(Arc<Mutex<T>>, PortType)> for InputPort {
    fn from((port, port_type): (Arc<Mutex<T>>, PortType)) -> Self {
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
            if let Ok(mut reader) = port.lock() {
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
                Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "input port was poisoned",
                ))?
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "input port was closed",
            ))?
        }
    }
}
