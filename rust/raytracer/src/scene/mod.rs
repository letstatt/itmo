use super::geometry;
use super::render::rays;
use geometry::vec3::Vec3;

pub mod light;
pub mod material;
pub mod object;

pub const DEFAULT_BACKGROUND_COLOR: Vec3 = Vec3 {
    x: 0.2,
    y: 0.7,
    z: 0.8,
};

pub struct Scene {
    pub objects: object::Objects,
    pub lights: Vec<light::Light>,
    pub background_color: Vec3,
}

impl Scene {
    pub fn default() -> Self {
        Self {
            objects: Vec::default(),
            lights: Vec::default(),
            background_color: DEFAULT_BACKGROUND_COLOR,
        }
    }
}
