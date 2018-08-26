pub mod parse;
pub mod simdata;
pub mod spring;

use self::simdata::*;

// struct to hold simulation state
#[derive(Debug, Clone, Copy)]
pub struct SimState<T>
where
  T: DiffEq + Clone + Default,
{
  pub dt: f64,
  pub target: T,
  pub t_stop: f64,
  pub time: f64,
  pub x: f64,
  pub xd: f64,
  pub xdd: f64,
}

// simulation trait
pub trait Simulation {
  fn is_done(&self) -> bool;
  fn show_states(&self, &Vec<PrintProp>) -> String;
  fn lookup_state_print_props(&self, PrintProp) -> f64;
}

// trait for calculating xdd
pub trait DiffEq {
  fn calc_xdd(&self, f64, f64) -> f64;
}

// implementation of Simulation for SimState
impl<T> Simulation for SimState<T>
where
  T: DiffEq + Clone + Default,
{
  // is simulation complete
  fn is_done(&self) -> bool {
    self.time > self.t_stop
  }

  // create a formated output string of state properties to view
  fn show_states(&self, props: &Vec<PrintProp>) -> String {
    let vs: Vec<f64> = props
      .into_iter()
      .map(|prop| self.lookup_state_print_props(*prop))
      .collect();

    vs.iter()
      .fold("".to_string(), |acc, s| acc + &format!("{:>15.6e}", s))
  }

  // works a getter for state properties
  fn lookup_state_print_props(&self, print_prop: PrintProp) -> f64 {
    use self::PrintProp::*;

    match print_prop {
      Time => self.time,
      X => self.x,
      Xd => self.xd,
      Xdd => self.xdd,
      Invalid => 0.0,
    }
  }
} // impl<T> Simulation for SimState<T>

// implementation of Iterator for SimState
impl<T> Iterator for SimState<T>
where
  T: DiffEq + Clone + Default,
{
  type Item = SimState<T>;
  fn next(&mut self) -> Option<SimState<T>> {
    // calculate diff eq at current time
    self.xdd = self.target.calc_xdd(self.x, self.xd);

    // advance the state one time step
    self.x += self.xd * self.dt;
    self.xd += self.xdd * self.dt;
    self.time += self.dt;

    // return next state
    Some(self.clone())
  }
}
