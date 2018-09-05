use state::simdata::TimeData;
use state::DiffEq;
use state::SimState;

// objects for SPRINGS

// struct to hold SPRING simulation data
#[derive(Debug, Clone, Copy)]
pub struct SpringData {
  pub damping_coefficient: f64,
  pub gravity: f64,
  pub mass: f64,
  pub spring_coefficient: f64,
  pub x_ic: f64,
  pub xd_ic: f64,
}

// default conditions for SPRING simulation data
impl Default for SpringData {
  fn default() -> SpringData {
    SpringData {
      damping_coefficient: 8.88,
      gravity: 9.88,
      mass: 1.0,
      spring_coefficient: 39.47,
      x_ic: 0.0,
      xd_ic: 0.0,
    }
  }
}

// implementation of DiffEq for SpringData
impl DiffEq for SpringData {
  fn calc_xdd(&self, x: f64, xd: f64) -> f64 {
    -(self.spring_coefficient * x + self.damping_coefficient * xd) / self.mass - self.gravity
  }
}

// initialize the data for SPRING simulation
pub fn initialize_sim(time_data: TimeData, target: SpringData) -> SimState<SpringData> {
  SimState {
    dt: time_data.dt,
    target: target,
    time: time_data.t_start,
    t_stop: time_data.t_stop,
    x: target.x_ic,
    xd: target.xd_ic,
    xdd: 0.0,
  }
}
