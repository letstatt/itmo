use super::material::Material;
use super::object::Object;
use super::vec3::Vec3;

pub struct Rectangle {
    pub pos: Vec3,
    pub first: Vec3,
    pub second: Vec3,
    pub material: Material,
    pub(super) norm: Vec3,
    pub(super) first_length: f32,
    pub(super) second_length: f32,
}

impl Rectangle {
    pub fn new(pos: Vec3, first: Vec3, second: Vec3, material: Material) -> Self {
        Rectangle {
            pos,
            first,
            second,
            material,
            norm: first.cross(second).normalize(),
            first_length: first.length(),
            second_length: second.length(),
        }
    }
}

impl Object for Rectangle {
    fn ray_intersect(&self, source: Vec3, direction: Vec3) -> Option<f32> {
        let denominator = direction * self.norm;

        if denominator == 0.0 || direction * self.norm > f32::EPSILON {
            None
        } else {
            let d = -self.norm * self.pos;
            let t = -(source * self.norm + d) / denominator;
            let hit_point = source + direction * t;
            let e = hit_point - self.pos;
            let x = e * self.first / self.first_length;
            let y = e * self.second / self.second_length;
            if 0.0 <= x && x <= self.first_length && 0.0 <= y && y <= self.second_length {
                Some(t)
            } else {
                None
            }
        }
    }

    fn material(&self, _: Vec3) -> Material {
        self.material
    }

    fn norm(&self, _: Vec3) -> Vec3 {
        self.norm
    }
}
