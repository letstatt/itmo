use super::geometry::vec3::Vec3;
use super::material::Material;

pub type Objects = Vec<Box<dyn Object>>;

pub trait Object {
    fn ray_intersect(&self, source: Vec3, direction: Vec3) -> Option<f32>;

    fn material(&self, point: Vec3) -> Material;

    fn norm(&self, point: Vec3) -> Vec3;
}

pub struct ObjectIntersection {
    pub point: Vec3,
    pub material: Material,
    pub norm: Vec3,
}

impl ObjectIntersection {
    fn new(point: Vec3, material: Material, norm: Vec3) -> ObjectIntersection {
        ObjectIntersection {
            point,
            material,
            norm,
        }
    }
}

pub fn intersect_objects(
    source: Vec3,
    direction: Vec3,
    objects: &Objects,
) -> Option<ObjectIntersection> {
    match objects
        .iter()
        .enumerate()
        .map(|(i, obj)| (i, obj.ray_intersect(source, direction)))
        .filter(|(_, opt)| opt.is_some())
        .fold((f32::MAX, None), |result, (i, intersection)| {
            let dist = intersection.unwrap();
            if dist < result.0 {
                (dist, Some(i))
            } else {
                result
            }
        }) {
        (dist, Some(i)) => {
            let hit_point = source + direction * dist;
            Some(ObjectIntersection::new(
                hit_point,
                objects[i].material(hit_point),
                objects[i].norm(hit_point),
            ))
        }
        _ => None,
    }
}
