use super::material::Material;
use super::object::Object;
use super::rectangle::Rectangle;
use super::vec3::Vec3;

pub struct Chessboard {
    pub rect: Rectangle,
    pub material1: Material,
}

impl Chessboard {
    pub fn new(rect: Rectangle, material1: Material) -> Self {
        Chessboard { rect, material1 }
    }
}

impl Object for Chessboard {
    fn ray_intersect(&self, source: Vec3, direction: Vec3) -> Option<f32> {
        self.rect.ray_intersect(source, direction)
    }

    fn material(&self, point: Vec3) -> Material {
        let e = point - self.rect.pos;
        let x = e * self.rect.first / self.rect.first_length;
        let y = e * self.rect.second / self.rect.second_length;
        let i = (x / 2.0).floor() as i32;
        let j = (y / 2.0).floor() as i32;
        if i % 2 == j % 2 {
            self.rect.material
        } else {
            self.material1
        }
    }

    fn norm(&self, _: Vec3) -> Vec3 {
        self.rect.norm
    }
}
