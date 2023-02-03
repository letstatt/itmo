use super::material::Material;
use super::object::Object;
use super::vec3::Vec3;

#[derive(Copy, Clone)]
pub struct Sphere {
    pub center: Vec3,
    pub radius: f32,
    pub material: Material,
}

impl Sphere {
    pub fn new(center: Vec3, radius: f32, material: Material) -> Self {
        Sphere {
            center,
            radius,
            material,
        }
    }
}

impl Object for Sphere {
    fn ray_intersect(&self, source: Vec3, direction: Vec3) -> Option<f32> {
        let hypot: Vec3 = self.center - source;
        let projection: f32 = hypot * direction; // dot product
        if projection < 0.0 {
            return None; // direction doesn't look this way
        }
        let perpendicular = hypot * hypot - projection * projection;
        if perpendicular > self.radius * self.radius {
            return None; // angle between direction and sphere is big
        }
        let delta = (self.radius * self.radius - perpendicular).sqrt();

        // solutions
        if projection - delta >= 0.0 {
            Some(projection - delta)
        } else if projection + delta >= 0.0 {
            Some(projection + delta)
        } else {
            None
        }
    }

    fn material(&self, _: Vec3) -> Material {
        self.material
    }

    fn norm(&self, point: Vec3) -> Vec3 {
        (point - self.center).normalize()
    }
}
