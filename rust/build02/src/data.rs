// struct to hold time data
#[derive(Debug, Clone)]
pub struct TimeData {
  pub dt: f64,
  pub t_start: f64,
  pub t_stop: f64,
}

// default conditions for simulation data
impl Default for TimeData {
  fn default() -> TimeData {
    TimeData {
      dt: 0.01,
      t_start: 0.0,
      t_stop: 2.5,
    }
  }
}

// struct to hold simulation data
#[derive(Debug, Clone)]
pub struct SpringData {
  pub damping_coefficient: f64,
  pub gravity: f64,
  pub mass: f64,
  pub spring_coefficient: f64,
  pub x_ic: f64,
  pub xd_ic: f64,
  pub t_data: TimeData,
}

// default conditions for simulation data
impl Default for SpringData {
  fn default() -> SpringData {
    SpringData {
      damping_coefficient: 8.88,
      gravity: 9.88,
      mass: 1.0,
      spring_coefficient: 39.47,
      x_ic: 0.0,
      xd_ic: 0.0,
      t_data: Default::default(),
    }
  }
}

pub trait DiffEq {
  fn calc_xdd(&self, f64, f64) -> f64;
}

impl DiffEq for SpringData {
  fn calc_xdd(&self, x: f64, xd: f64) -> f64 {
    -(self.spring_coefficient * x + self.damping_coefficient * xd) / self.mass - self.gravity
  }
}
