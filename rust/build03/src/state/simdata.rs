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

// objects for springs

// spring simulation variable for dynamic printing
#[derive(Debug, Clone, PartialEq)]
pub enum SpringProperty {
  DampingCoefficient,
  Dt,
  Gravity,
  Mass,
  SpringCoefficient,
  Time,
  TStart,
  TStop,
  X,
  Xd,
  Xdd,
  XdIc,
  Xic,
  Invalid,
}

// command
#[derive(Debug, Clone, PartialEq)]
pub enum Command {
  Print(SpringProperty),
  Run,
  Stop,
  Invalid,
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

pub fn lookup_command(s: &str) -> Command {
  let s_slice: &str = &s.to_lowercase();
  match s_slice.trim() {
    "print" => Command::Print(SpringProperty::Invalid),
    "stop" => Command::Stop,
    "run" => Command::Run,
    _ => Command::Invalid,
  }
}

pub fn lookup_spring_property(s: &str) -> SpringProperty {
  let s_slice: &str = &s.to_lowercase();
  match s_slice.trim() {
    "dampingcoefficient" => SpringProperty::DampingCoefficient,
    "dt" => SpringProperty::Dt,
    "gravity" => SpringProperty::Gravity,
    "mass" => SpringProperty::Mass,
    "springcoefficient" => SpringProperty::SpringCoefficient,
    "time" => SpringProperty::Time,
    "tstart" => SpringProperty::TStart,
    "tstop" => SpringProperty::TStop,
    "x" => SpringProperty::X,
    "xd" => SpringProperty::Xd,
    "xdd" => SpringProperty::Xdd,
    "xdic" => SpringProperty::XdIc,
    "xic" => SpringProperty::Xic,
    _ => SpringProperty::Invalid,
  }
}
