use super::geometry::vec3::Vec3;
use super::scene::{light, object, Scene};
pub mod export;
pub mod rays;
mod ssaa;

pub struct Frame {
    pub frame: Vec<Vec3>,
    pub height: usize,
    pub width: usize,
}

impl Frame {
    pub fn len(&self) -> usize {
        self.frame.len()
    }

    pub fn is_empty(&self) -> bool {
        self.frame.is_empty()
    }
}

pub fn render(width: usize, height: usize, fov: f32, ssaa_num: usize, scene: &Scene) -> Frame {
    let mut frame = vec![Vec3::default(); width * height];
    let zero = Vec3::default();

    let fh = height as f32;
    let fw = width as f32;

    // prepare SSAA (supersample anti-aliasing)
    let offsets: Vec<(f32, f32)> = ssaa::create_offsets(ssaa_num);

    for j in 0..height {
        for i in 0..width {
            // base color
            let ray = rays::pixel_to_ray(i as f32, j as f32, fh, fw, fov);
            let color = rays::cast_ray(zero, ray, scene, 0);

            // apply SSAA
            frame[j * width + i] = ssaa::antialiasing(
                color,
                (i as f32, j as f32),
                offsets.as_slice(),
                fh,
                fw,
                fov,
                scene,
            );
        }
    }

    Frame {
        frame,
        height,
        width,
    }
}
