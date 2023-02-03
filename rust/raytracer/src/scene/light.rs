use super::geometry::vec3::Vec3;
use super::material::Material;
use super::object::intersect_objects;
use super::rays;
use super::Scene;

pub struct Light {
    pub pos: Vec3,
    pub intensity: f32,
}

pub fn calculate_light(
    view_direction: Vec3,
    point: Vec3,
    material: Material,
    norm: Vec3,
    scene: &Scene,
) -> Vec3 {
    let mut diffuse_light_intensity = 0_f32;
    let mut specular_light_intensity = 0_f32;

    let light_sources = &scene.lights;
    let objects = &scene.objects;

    for i in light_sources {
        // a ray from light source to obj
        let light_direction = (point - i.pos).normalize();
        let distance = (point - i.pos).length();

        // does point is hidden from the light source?
        if light_direction * norm > 0.0 {
            continue; // skip
        }

        // reflected ray
        let reflected_direction = rays::reflect_ray(light_direction, norm);

        // offset the original point to avoid occlusion by the object itself
        let shifted = point + norm * 1e-5;

        // try inverted ray - from obj to light source
        if let Some(intersection) = intersect_objects(shifted, -light_direction, objects) {
            if (intersection.point - shifted).length() < distance {
                // found obj between point and the light source.
                // skip the light source (transparent objects will have shadows)
                continue;
            }
        };

        diffuse_light_intensity += (-light_direction * norm) * i.intensity;
        specular_light_intensity += (-reflected_direction * view_direction)
            .max(0_f32)
            .powf(material.shininess)
            * i.intensity;
    }

    material.diffuse_color * diffuse_light_intensity * material.diffuse_reflection
        + Vec3::extend(1_f32) * specular_light_intensity * material.specular_reflection
}
