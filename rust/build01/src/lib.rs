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
} // Sim

// default conditions for simulation
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
} // Default for Sim

pub trait Sim {
    fn run(&mut self);
}

impl Sim for SimData {
    fn run(&mut self) {
        // set initial conditions
        let mut x = self.x_ic;
        let mut xd = self.xd_ic;
        let mut time = 0.0;

        // main simulation loop
        while time <= self.t_stop {
            // print state for this time
            println!("{:>15.6e} {:>15.6e} {:>15.6e}", time, x, xd);
            // calculate derivative at current time
            let xdd = -(self.spring_coefficient*x + self.damping_coefficient*xd)/self.mass - self.gravity;
            // advance states one time step
            x += xd*self.dt;
            xd += xdd*self.dt;
            // advance time and continue simulation loop
            time += self.dt;
        }
    }
}
