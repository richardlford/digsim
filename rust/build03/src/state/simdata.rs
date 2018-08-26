// struct to hold time data
#[derive(Debug, Clone, Copy)]
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

// simulation variables for dynamic printing
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum PrintProp {
  Time,
  X,
  Xd,
  Xdd,
  Invalid,
}

// dynamic simulation execution commands
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Command {
  Print(PrintProp),
  Run,
  Stop,
  Invalid,
}

// trait for calculating xdd
pub trait DiffEq {
  fn calc_xdd(&self, f64, f64) -> f64;
}

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

// translate a string to a Command enum value
pub fn lookup_command(s: &str) -> Command {
  let s_slice: &str = &s.to_lowercase();
  match s_slice.trim() {
    "print" => Command::Print(PrintProp::Invalid),
    "stop" => Command::Stop,
    "run" => Command::Run,
    _ => Command::Invalid,
  }
}

// translate a string to a PrintProp enum value
pub fn lookup_print_prop(s: &str) -> PrintProp {
  let s_slice: &str = &s.to_lowercase();
  match s_slice.trim() {
    "time" => PrintProp::Time,
    "x" => PrintProp::X,
    "xd" => PrintProp::Xd,
    "xdd" => PrintProp::Xdd,
    _ => PrintProp::Invalid,
  }
}

// tests
#[test]
fn test_lookup_print_prop() {
  use self::PrintProp::*;

  assert!(lookup_print_prop("tIMe") == Time);
  assert!(lookup_print_prop("aaa") == Invalid);
  assert!(lookup_print_prop("   xd  ") == Xd);
  assert!(lookup_print_prop("xDD ") == Xdd);
  assert!(lookup_print_prop("  X  ") == X);
}

#[test]
fn test_lookup_command() {
  assert!(lookup_command("print ") == Command::Print(PrintProp::Invalid));
  assert!(lookup_command("Print") == Command::Print(PrintProp::Invalid));
  assert!(lookup_command("Runn") == Command::Invalid);
  assert!(lookup_command("  RUN   ") == Command::Run);
  assert!(lookup_command("  STOp  ") == Command::Stop);
}
