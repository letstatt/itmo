use super::Frame;
use std::fs::OpenOptions;
use std::io::Write;

fn color_f32_to_u8(color: f32) -> u8 {
    (color.min(1.0).max(0.0) * 255.0).round() as u8
}

pub fn export(filename: &str, frame: Frame) -> std::io::Result<()> {
    let Frame {
        frame,
        height,
        width,
    } = frame;

    let mut f = OpenOptions::new()
        .write(true)
        .create(true)
        .open(filename)
        .expect("[!] Unable to create the file");

    write!(f, "P6\n{} {}\n255\n", width, height)?;

    let mut image: Vec<u8> = Vec::with_capacity(frame.len() * 3);

    for i in frame {
        image.push(color_f32_to_u8(i.x));
        image.push(color_f32_to_u8(i.y));
        image.push(color_f32_to_u8(i.z));
    }

    f.write_all(image.as_slice())?;
    f.flush()?;
    Ok(())
}
