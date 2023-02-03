use super::rays;
use super::Scene;
use super::Vec3;

const R: f32 = 0.7;

pub fn create_offsets(count: usize) -> Vec<(f32, f32)> {
    let arc = 2.0 * std::f32::consts::PI / count as f32;

    (0..count)
        .into_iter()
        .map(|i| ((arc * i as f32).cos() * R, (arc * i as f32).sin() * R))
        .collect()
}

pub fn antialiasing(
    base_color: Vec3,
    px: (f32, f32),
    offsets: &[(f32, f32)],
    height: f32,
    width: f32,
    fov: f32,
    scene: &Scene,
) -> Vec3 {
    // convert (x, y) and deltas to rays
    let rays = offsets
        .iter()
        .map(|(dx, dy)| rays::pixel_to_ray(px.0 + dx, px.1 + dy, height, width, fov));

    // cast rays to get colors
    rays.into_iter()
        .map(|i| rays::cast_ray(Vec3::default(), i, scene, 0))
        // sum
        .fold(base_color, |i, j| i + j)
        // and calculate average
        / (1 + offsets.len()) as f32
}
