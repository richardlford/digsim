// struct to hold simulation data
#[derive(Debug, Clone)]
pub struct SimData {
    pub damping_coefficient: f64,
    pub dt: f64,
    pub gravity: f64,
    pub mass: f64,
    pub spring_coefficient: f64,
    pub t_stop: f64,
    pub x_ic: f64,
    pub xd_ic: f64,
}

// default conditions for simulation data
impl Default for SimData {
    fn default() -> SimData {
        SimData {
            damping_coefficient: 8.88,
            dt: 0.01,
            gravity: 9.88,
            mass: 1.0,
            spring_coefficient: 39.47,
            t_stop: 2.5,
            x_ic: 0.0,
            xd_ic: 0.0,
        }
    }
}

// https://stackoverflow.com/questions/48071513/how-to-use-one-module-from-another-module-in-a-rust-cargo-project
