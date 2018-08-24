mod data;

// struct to hold simulation state
#[derive(Debug, Clone)]
pub struct SimState {
    pub x: f64,
    pub xd: f64,
    pub time: f64,
    pub sim_const: data::SimData,
}

// default conditions for simulation state
impl Default for SimState {
    fn default() -> SimState {
        let c: data::SimData = Default::default();
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

// implementation of Simulation for SimState
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

// implementation of Iterator for SimState
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
