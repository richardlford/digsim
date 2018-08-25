pub mod parse;
pub mod simdata;

use self::simdata::*;

// struct to hold simulation state
#[derive(Debug, Clone)]
pub struct SimState<T: DiffEq> {
  pub x: f64,
  pub xd: f64,
  pub xdd: f64,
  pub time: f64,
  pub time_data: TimeData,
  pub target: T,
}

// simulation trait
pub trait Simulation {
  fn is_done(&self) -> bool;
  fn show_vars(&self, &[f64]) -> String;
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

  fn show_vars(&self, vs: &[f64]) -> String {
    vs.iter()
      .fold("".to_string(), |acc, s| acc + &format!("{:>15.6e}", s))
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
    self.xdd = target.calc_xdd(self.x, self.xd);

    // advance the state one time step
    self.x += self.xd * self.time_data.dt;
    self.xd += self.xdd * self.time_data.dt;

    // advance time and continue simulation loop
    self.time += self.time_data.dt;

    // return next state
    Some(self.clone())
  }
}

pub fn initialize_sim(time: &TimeData, spring: &SpringData) -> SimState<SpringData> {
  SimState {
    target: spring.clone(),
    time_data: time.clone(),
    time: spring.t_data.t_start,
    x: spring.x_ic,
    xd: spring.xd_ic,
    xdd: 0.0,
  }
}

pub fn lookup_spring_vars(st: &SimState<SpringData>, spring_property: SpringProperty) -> f64 {
  match spring_property {
    SpringProperty::DampingCoefficient => st.target.spring_coefficient,
    SpringProperty::Dt => st.time_data.dt,
    SpringProperty::Gravity => st.target.gravity,
    SpringProperty::Mass => st.target.mass,
    SpringProperty::SpringCoefficient => st.target.spring_coefficient,
    SpringProperty::Time => st.time,
    SpringProperty::TStart => st.time_data.t_start,
    SpringProperty::TStop => st.time_data.t_stop,
    SpringProperty::X => st.x,
    SpringProperty::Xd => st.xd,
    SpringProperty::Xdd => st.xdd,
    SpringProperty::XdIc => st.target.xd_ic,
    SpringProperty::Xic => st.target.x_ic,
    SpringProperty::Invalid => 0.0,
  }
}
