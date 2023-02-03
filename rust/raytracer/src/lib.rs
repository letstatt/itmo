pub mod geometry;
pub mod render;
pub mod scene;

pub use render::{export, render};

pub fn demo_scene() -> scene::Scene {
    use geometry::{chessboard::Chessboard, rectangle::Rectangle, sphere::Sphere, vec3::Vec3};
    use scene::{light::Light, material, Scene};
    let mut scene = Scene::default();

    // main red sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 0.0,
            y: 0.0,
            z: -10.0,
        },
        radius: 2.0,
        material: material::RED_RUBBER,
    }));
    // distant right side sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 5.0,
            y: 0.0,
            z: -20.0,
        },
        radius: 1.0,
        material: material::IVORY,
    }));
    // closer right side sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 2.0,
            y: 3.0,
            z: -16.0,
        },
        radius: 1.0,
        material: material::IVORY,
    }));
    // main left ivory sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: -5.0,
            y: -0.5,
            z: -11.0,
        },
        radius: 2.0,
        material: material::IVORY,
    }));
    // secondary right side red sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 10.0,
            y: 4.0,
            z: -17.0,
        },
        radius: 1.5,
        material: material::RED_RUBBER,
    }));
    // bottom ivory sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 1.0,
            y: -3.0,
            z: -10.0,
        },
        radius: 0.8,
        material: material::IVORY,
    }));
    // near mirror sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: -5.0,
            y: -3.3,
            z: -6.9,
        },
        radius: 1.8,
        material: material::MIRROR,
    }));
    // distant mirror sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: -5.0,
            y: 3.3,
            z: -12.5,
        },
        radius: 1.5,
        material: material::MIRROR,
    }));
    // near glass sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 2.3,
            y: -2.0,
            z: -5.8,
        },
        radius: 1.0,
        material: material::GLASS,
    }));
    // distant green sphere
    scene.objects.push(Box::new(Sphere {
        center: Vec3 {
            x: 6.5,
            y: 8.0,
            z: -26.0,
        },
        radius: 1.5,
        material: material::GREEN,
    }));
    // bottom rectangle
    scene.objects.push(Box::new(Chessboard::new(
        Rectangle::new(
            Vec3 {
                x: 4.0,
                y: -5.0,
                z: -9.0,
            },
            Vec3 {
                x: 0.0,
                y: 0.0,
                z: -8.0,
            },
            Vec3 {
                x: -8.0,
                y: 0.0,
                z: 0.0,
            },
            material::GREEN,
        ),
        material::WHITE,
    )));
    // side mirror rectangle
    scene.objects.push(Box::new(Rectangle::new(
        Vec3 {
            x: 15.0,
            y: -6.0,
            z: -12.0,
        },
        Vec3 {
            x: 2.0,
            y: 10.0,
            z: 0.0,
        },
        Vec3 {
            x: -5.0,
            y: 0.0,
            z: -10.0,
        },
        material::MIRROR,
    )));
    // light sources
    scene.lights.push(Light {
        pos: Vec3 {
            x: 7.0,
            y: 15.0,
            z: -3.0,
        },
        intensity: 1.5,
    });
    scene.lights.push(Light {
        pos: Vec3 {
            x: -10.0,
            y: 10.0,
            z: -7.0,
        },
        intensity: 1.8,
    });
    scene.lights.push(Light {
        pos: Vec3 {
            x: 4.0,
            y: 8.0,
            z: 5.0,
        },
        intensity: 1.0,
    });
    scene
}
