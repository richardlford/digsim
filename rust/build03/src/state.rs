use data;
use data::{DiffEq, SpringData, TimeData};

// struct to hold simulation state
#[derive(Debug, Clone)]
pub struct SimState<T: DiffEq> {
  pub x: f64,
  pub xd: f64,
  pub time: f64,
  pub time_data: TimeData,
  pub target: T,
}

// simulation trait
pub trait Simulation {
  fn is_done(&self) -> bool;
  fn show_time_x_xd(&self) -> String;
  fn show_var(&self, f64) -> String;
}

// implementation of Simulation for SimState
impl<T> Simulation for SimState<T>
where
  T: DiffEq + Clone,
{
  // is simulation complete
  fn is_done(&self) -> bool {
    self.time > self.time_data.t_stop
  }

  // display current state
  fn show_time_x_xd(&self) -> String {
    format!("{:>15.6e}{:>15.6e}{:>15.6e}", self.time, self.x, self.xd)
  }

  fn show_var(&self, val: f64) -> String {
    format!("{:>15.6e}", val)
  }
}

// implementation of Iterator for SimState
impl<T> Iterator for SimState<T>
where
  T: DiffEq + Clone,
{
  type Item = SimState<T>;
  fn next(&mut self) -> Option<SimState<T>> {
    // assign short name to constants
    let target = &self.target;

    // calculate diff eq at current time
    let xdd = target.calc_xdd(self.x, self.xd);

    // advance the state one time step
    self.x += self.xd * self.time_data.dt;
    self.xd += xdd * self.time_data.dt;

    // advance time and continue simulation loop
    self.time += self.time_data.dt;

    // return next state
    Some(self.clone())
  }
}

pub fn initialize_sim(time: data::TimeData, spring: data::SpringData) -> SimState<SpringData> {
  SimState {
    x: spring.x_ic,
    xd: spring.xd_ic,
    time: spring.t_data.t_start,
    time_data: time.clone(),
    target: spring,
  }
}
