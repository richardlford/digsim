// struct to hold simulation constants
#[derive(Debug, Clone)]
pub struct SimConst {
    pub damping_coefficient: f64,
    pub dt: f64,
    pub gravity: f64,
    pub mass: f64,
    pub spring_coefficient: f64,
    pub t_stop: f64,
    pub x_ic: f64,
    pub xd_ic: f64,
}

// default conditions for simulation constants
impl Default for SimConst {
    fn default() -> SimConst {
        SimConst {
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

// struct to hold simulation state
#[derive(Debug, Clone)]
pub struct SimState {
    pub x: f64,
    pub xd: f64,
    pub time: f64,
    pub sim_const: SimConst,
}

// default conditions for simulation state
impl Default for SimState {
    fn default() -> SimState {
        let c: SimConst = Default::default();
        SimState {
            x: c.x_ic,
            xd: c.xd_ic,
            time: 0.0,
            sim_const: c.clone(),
        }
    }
}

// simulation trait
pub trait Simulation {
    fn is_done(&self) -> bool;
    fn show_time_x_xd(&self) -> String;
}

impl Simulation for SimState {
    // is simulation complete
    fn is_done(&self) -> bool {
        return self.time > self.sim_const.t_stop;
    }

    // display current state
    fn show_time_x_xd(&self) -> String {
        format!("{:>15.6e} {:>15.6e} {:>15.6e}", self.time, self.x, self.xd)
    }
}

// state iterator
impl Iterator for SimState {
    type Item = SimState;
    fn next(&mut self) -> Option<SimState> {
        // assign short name to constants
        let c = self.sim_const.clone();

        // calculate derivative at current time
        let xdd =
            -(c.spring_coefficient * self.x + c.damping_coefficient * self.xd) / c.mass - c.gravity;

        // advance the state one time step
        self.x += self.xd * c.dt;
        self.xd += xdd * c.dt;

        // advance time and continue simulation loop
        self.time += c.dt;

        // return next state
        Some(self.clone())
    }
}
