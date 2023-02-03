use super::light;
use super::object::{intersect_objects, ObjectIntersection};
use super::Scene;
use super::Vec3;

pub const MAX_REFLECTION_DEPTH: usize = 4;

pub fn pixel_to_ray(ix: f32, iy: f32, height: f32, width: f32, fov: f32) -> Vec3 {
    let x = (ix + 0.5) - width / 2.0;
    let y = -(iy + 0.5) + height / 2.0;
    Vec3::new(x, y, -height / (2.0 * (fov / 2.0).tan())).normalize()
}

pub fn reflect_ray(direction: Vec3, norm: Vec3) -> Vec3 {
    // original formula gives normalized vectors if normalized given before, but
    // to avoid computational losses, normalize again
    (direction - norm * 2_f32 * (direction * norm)).normalize()
}

pub fn refract_ray(direction: Vec3, mut norm: Vec3, refractive_index: f32) -> Vec3 {
    let mut cos1: f32 = -((direction * norm).min(1.0).max(-1.0));
    let mut eta = 1.0 / refractive_index;

    // if a ray is inside the object, swap coefficients
    if cos1 < 0.0 {
        cos1 = -cos1;
        norm = -norm;
        eta = 1.0 / eta;
    };

    let cos2_sqr = 1.0 - eta * eta * (1.0 - cos1 * cos1);

    if cos2_sqr < 0.0 {
        // refraction doesn't exist
        Vec3::new(0.0, 0.0, 0.0)
    } else {
        // original formula gives normalized vectors if normalized given before, but
        // to avoid computational losses, normalize again
        (direction * eta + (eta * cos1 - cos2_sqr.sqrt()) * norm).normalize()
    }
}

pub fn cast_ray(source: Vec3, direction: Vec3, scene: &Scene, reflection_depth: usize) -> Vec3 {
    if reflection_depth > MAX_REFLECTION_DEPTH {
        return scene.background_color;
    }

    match intersect_objects(source, direction, &scene.objects) {
        Some(intersection) => {
            let ObjectIntersection {
                point,
                material,
                norm,
            } = intersection;

            // reflect and cast new ray to visualise reflections
            let reflection_color = if material.reflection_visibility != 0.0 {
                let reflection_direction = reflect_ray(direction, norm);

                // offset the original point to avoid occlusion by the object itself
                let shifted = point
                    + if reflection_direction * norm < 0.0 {
                        -1.0
                    } else {
                        1.0
                    } * norm
                        * 1e-5;

                material.reflection_visibility
                    * cast_ray(shifted, reflection_direction, scene, reflection_depth + 1)
            } else {
                Vec3::default()
            };

            // refract and cast new ray to visualise refractions
            let refraction_color = if material.refraction_visibility != 0.0 {
                let refraction_direction = refract_ray(direction, norm, material.refractive_index);

                // check if refraction exists
                if refraction_direction.is_zero() {
                    return refraction_direction; // reuse zero
                };

                // offset the original point to avoid occlusion by the object itself
                let shifted = point
                    + if refraction_direction * norm < 0.0 {
                        -1.0
                    } else {
                        1.0
                    } * norm
                        * 1e-5;

                material.refraction_visibility
                    * cast_ray(shifted, refraction_direction, scene, reflection_depth + 1)
            } else {
                Vec3::default()
            };

            light::calculate_light(direction, point, material, norm, scene)
                + reflection_color
                + refraction_color
        }
        _ => scene.background_color,
    }
}
