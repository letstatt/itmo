use super::material::Material;
use super::object::Object;
use super::vec3::Vec3;

// Plane is not used in the default scene,
// but it helped me further to implement Rectangle
pub struct Plane {
    pub pos: Vec3,
    pub norm: Vec3,
    pub material: Material,
}

impl Plane {
    pub fn new(pos: Vec3, norm: Vec3, material: Material) -> Plane {
        Plane {
            pos,
            norm,
            material,
        }
    }
}

impl Object for Plane {
    fn ray_intersect(&self, source: Vec3, direction: Vec3) -> Option<f32> {
        let denominator = direction * self.norm;

        if denominator == 0.0 || direction * self.norm > f32::EPSILON {
            None
        } else {
            let d = -self.norm * self.pos;
            Some(-(source * self.norm + d) / denominator)
        }
    }

    fn material(&self, _: Vec3) -> Material {
        self.material
    }

    fn norm(&self, _: Vec3) -> Vec3 {
        self.norm
    }
}
