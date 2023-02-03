use raytracer::{export, render};

const WIDTH: usize = 1920;
const HEIGHT: usize = 1080;
const SSAA: usize = 3;
const FOV: f32 = std::f32::consts::FRAC_PI_2; // 90 degrees in radians

fn main() {
    let scene = raytracer::demo_scene();

    println!("Rendering started...");
    let frame = render(WIDTH, HEIGHT, FOV, SSAA, &scene);

    println!("Rendered {} pixels", frame.len());
    println!("Exporting...");

    export::export(".\\image.ppm", frame).unwrap();
    println!("Good bye.");
}
