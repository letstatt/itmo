use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::{Add, Div, Index, Mul, Neg, Sub};

#[derive(Copy, Clone)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3 {
    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Vec3 { x, y, z }
    }

    pub const fn extend(val: f32) -> Self {
        Self::new(val, val, val)
    }

    pub fn is_zero(&self) -> bool {
        self.x == 0.0 && self.y == 0.0 && self.z == 0.0
    }

    pub fn length(&self) -> f32 {
        (*self * *self).sqrt()
    }

    pub fn normalize(&self) -> Self {
        if self.is_zero() {
            *self // reuse zero
        } else {
            let length: f32 = self.length();
            Self {
                x: self.x / length,
                y: self.y / length,
                z: self.z / length,
            }
        }
    }

    pub fn cross(&self, other: Vec3) -> Vec3 {
        Vec3::new(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x * other.z,
            self.x * other.y - self.y * other.x,
        )
    }
}

impl Default for Vec3 {
    fn default() -> Self {
        Vec3::new(0.0, 0.0, 0.0)
    }
}

impl Add for Vec3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Sub for Vec3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Neg for Vec3 {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

impl Mul<f32> for Vec3 {
    type Output = Self;

    fn mul(self, scalar: f32) -> Self {
        Self {
            x: self.x * scalar,
            y: self.y * scalar,
            z: self.z * scalar,
        }
    }
}

impl Mul<Vec3> for f32 {
    type Output = Vec3;

    fn mul(self, other: Vec3) -> Self::Output {
        Self::Output {
            x: other.x * self,
            y: other.y * self,
            z: other.z * self,
        }
    }
}

impl Mul<Vec3> for Vec3 {
    type Output = f32;

    fn mul(self, other: Vec3) -> f32 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
}

impl Div<f32> for Vec3 {
    type Output = Self;

    fn div(self, scalar: f32) -> Self::Output {
        Self {
            x: self.x / scalar,
            y: self.y / scalar,
            z: self.z / scalar,
        }
    }
}

impl PartialEq for Vec3 {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}

impl Index<usize> for Vec3 {
    type Output = f32;

    fn index(&self, i: usize) -> &Self::Output {
        match i {
            0 => &self.x,
            1 => &self.y,
            2 => &self.z,
            _ => panic!("Vec3: invalid index {}", i),
        }
    }
}

impl Debug for Vec3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}, {}]", self.x, self.y, self.z)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec3_new() {
        let vec = Vec3::new(1.0, 5.0, 7.0);
        assert_eq!(vec.x, 1.0);
        assert_eq!(vec.y, 5.0);
        assert_eq!(vec.z, 7.0);
    }
    #[test]
    fn test_vec3_extend() {
        let vec = Vec3::extend(7.7);
        assert_eq!(vec.x, 7.7);
        assert_eq!(vec.y, 7.7);
        assert_eq!(vec.z, 7.7);
    }

    #[test]
    fn test_vec3_is_zero() {
        let vec1 = Vec3::extend(0.0);
        let vec2 = Vec3::extend(1.0);
        assert!(vec1.is_zero());
        assert!(!vec2.is_zero());
    }

    #[test]
    fn test_vec3_length() {
        let vec1 = Vec3::new(1.0, 0.0, 0.0);
        let vec2 = Vec3::extend(1.0);
        assert_eq!(vec1.length(), 1.0);
        assert_ne!(vec2.length(), 1.0);
    }

    #[test]
    fn test_vec3_normalize() {
        let vec = Vec3::extend(1.0);
        assert!((vec.normalize().length() - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_vec3_cross() {
        let vec1 = Vec3::new(1.0, 0.0, 0.0);
        let vec2 = Vec3::new(0.0, 1.0, 0.0);
        let vec3 = Vec3::new(0.0, 0.0, 1.0);
        let vec4 = Vec3::new(0.0, 0.0, -1.0);
        assert_eq!(vec1.cross(vec2), vec3);
        assert_eq!(vec2.cross(vec1), vec4);
    }

    #[test]
    fn test_vec3_default() {
        assert!(Vec3::default().is_zero());
    }
    #[test]
    fn test_vec3_add() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 0.0);
        assert_eq!(Vec3::new(-4.0, 15.0, 7.0), vec1 + vec2);
    }
    #[test]
    fn test_vec3_sub() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 0.0);
        assert_eq!(Vec3::new(6.0, -5.0, 7.0), vec1 - vec2);
    }
    #[test]
    fn test_vec3_neg() {
        let vec = Vec3::new(1.0, -5.0, 7.0);
        assert_eq!(Vec3::new(-1.0, 5.0, -7.0), -vec);
    }
    #[test]
    fn test_vec3_mul_scalar() {
        let vec = Vec3::new(1.0, -5.0, 7.0);
        assert_eq!(Vec3::new(-2.0, 10.0, -14.0), vec * -2.0);
        assert_eq!(Vec3::new(-2.0, 10.0, -14.0), -2.0 * vec);
    }
    #[test]
    fn test_vec3_dot_product() {
        let vec1 = Vec3::new(1.0, -5.0, 7.0);
        let vec2 = Vec3::new(2.0, 5.0, 1.0);
        assert_eq!(-16.0, vec1 * vec2);
    }
    #[test]
    fn test_vec3_div_scalar() {
        let vec = Vec3::new(-2.0, 10.0, -14.0);
        assert_eq!(Vec3::new(1.0, -5.0, 7.0), vec / -2.0);
    }
}
