use super::geometry::vec3::Vec3;

#[derive(Copy, Clone)]
pub struct Material {
    pub diffuse_color: Vec3,
    pub diffuse_reflection: f32,
    pub specular_reflection: f32,
    pub reflection_visibility: f32,
    pub refraction_visibility: f32,
    pub refractive_index: f32,
    pub shininess: f32, // is larger for surfaces that are smoother and more mirror-like
}

pub const RED_RUBBER: Material = Material {
    diffuse_color: Vec3::new(0.3, 0.1, 0.1),
    diffuse_reflection: 0.9,
    specular_reflection: 0.1,
    reflection_visibility: 0.0,
    refraction_visibility: 0.0,
    refractive_index: 1.0,
    shininess: 10.0,
};
pub const IVORY: Material = Material {
    diffuse_color: Vec3::new(0.4, 0.4, 0.3),
    diffuse_reflection: 0.6,
    specular_reflection: 0.3,
    reflection_visibility: 0.07,
    refraction_visibility: 0.0,
    refractive_index: 1.0,
    shininess: 50.0,
};
pub const MIRROR: Material = Material {
    diffuse_color: Vec3::new(1.0, 1.0, 1.0),
    diffuse_reflection: 0.0,
    specular_reflection: 10.0,
    reflection_visibility: 0.85,
    refraction_visibility: 0.0,
    refractive_index: 1.0,
    shininess: 1425.0,
};
pub const GREEN: Material = Material {
    diffuse_color: Vec3::new(0.2, 0.5, 0.2),
    diffuse_reflection: 0.5,
    specular_reflection: 0.1,
    reflection_visibility: 0.0,
    refraction_visibility: 0.0,
    refractive_index: 1.0,
    shininess: 3.0,
};
pub const WHITE: Material = Material {
    diffuse_color: Vec3::new(1.0, 1.0, 1.0),
    diffuse_reflection: 0.5,
    specular_reflection: 0.1,
    reflection_visibility: 0.0,
    refraction_visibility: 0.0,
    refractive_index: 1.0,
    shininess: 3.0,
};
pub const GLASS: Material = Material {
    diffuse_color: Vec3::new(0.6, 0.7, 0.8),
    diffuse_reflection: 0.0,
    specular_reflection: 0.5,
    reflection_visibility: 0.1,
    refraction_visibility: 0.8,
    refractive_index: 1.5,
    shininess: 125.0,
};

impl Material {
    pub fn default() -> Self {
        RED_RUBBER
    }

    pub fn new(
        diffuse_color: Vec3,
        diffuse_reflection: f32,
        specular_reflection: f32,
        reflection_visibility: f32,
        refraction_visibility: f32,
        refractive_index: f32,
        shininess: f32,
    ) -> Self {
        Self {
            diffuse_color,
            diffuse_reflection,
            specular_reflection,
            reflection_visibility,
            refraction_visibility,
            refractive_index,
            shininess,
        }
    }
}
