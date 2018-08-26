pub mod parse;
pub mod simdata;

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
pub trait Simulation<T>
where
  T: DiffEq + Clone + Default,
{
  fn is_done(&self) -> bool;
  fn show_states(&self, &Vec<PrintProp>) -> String;
  fn lookup_state_print_props(&self, PrintProp) -> f64;
  // fn display_items(&self, &Vec<PrintProp>) -> Vec<f64>;
}

// implementation of Simulation for SimState for SPRINGS
impl Simulation<SpringData> for SimState<SpringData> {
  // is simulation complete
  fn is_done(&self) -> bool {
    self.time > self.t_stop
  }

  // create a formated output string of states properties to view
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
} // impl Simulation<SpringData> for SimState<SpringData>

// implementation of Iterator for SimState
impl<T> Iterator for SimState<T>
where
  T: DiffEq + Clone + Default,
{
  type Item = SimState<T>;
  fn next(&mut self) -> Option<SimState<T>> {
    // assign short name to constants
    let target = &self.target;

    // calculate diff eq at current time
    self.xdd = target.calc_xdd(self.x, self.xd);

    // advance the state one time step
    self.x += self.xd * self.dt;
    self.xd += self.xdd * self.dt;

    // advance time and continue simulation loop
    self.time += self.dt;

    // return next state
    Some(self.clone())
  }
}

// initialize the data for the sim
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
