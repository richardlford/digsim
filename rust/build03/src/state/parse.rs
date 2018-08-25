use state::simdata::{lookup_command, lookup_property, Command, Property};

// parse the input lines
// error checks as it goes
pub fn parse_input(in_str: &str) -> Vec<Command> {
  in_str
    .lines()
    .filter(|&s| s.trim() != "")
    .map(|s| {
      let words: Vec<&str> = s.split(' ').filter(|&s| s.trim() != "").collect();
      // println!("{:?}", words);
      match &words[..] {
        [c, v] | [c, v, _] => {
          let command = lookup_command(c);
          match command {
            Command::Print(Property::Invalid) => Command::Print(lookup_property(v)),
            _ => Command::Invalid,
          }
        }
        [c] => lookup_command(c),
        _ => Command::Invalid,
      }
    })
    .collect()
}

// read a compiled stored input file
pub fn parse_input_file() -> Vec<Command> {
  parse_input(include_str!("../../data/input.dat"))
}

// return true if list does not contain Command::Invalid
pub fn commands_valid(commands: &Vec<Command>) -> bool {
  !&commands[..].contains(&Command::Invalid)
}

// return true if list does not contain Command::Print(Property::Invalid)
pub fn properties_valid(commands: &Vec<Command>) -> bool {
  let xs: Vec<Command> = commands
    .clone()
    .into_iter()
    .filter(|c| match c {
      Command::Print(prop) => *prop == Property::Invalid,
      _ => false,
    })
    .collect();
  xs.len() == 0
}

// translate list of Commands to a run list
// combines all Print statements for a Run
pub fn build_run_list(commands: &Vec<Command>) -> Vec<(Command, Vec<Property>)> {
  // use self::Command::*;
  use self::Property::*;

  let mut runs: Vec<(Command, Vec<Property>)> = vec![];
  let mut props: Vec<Property> = vec![Time];
  for command in commands {
    match command {
      Command::Print(prop) => props.push(*prop),
      Command::Run => {
        runs.push((*command, props.clone()));
        props = vec![Time]
      }
      _ => break,
    }
  }
  runs
}

// tests
#[test]
fn test_parse_input_case() {
  use self::Command::*;
  use self::Property::*;

  let s = "Print X\n\
           print xD\n\
           \n\
           \n\
           pRINT XDD\n\
           Run\n\
           stop\n";

  let parsed = parse_input(s);
  println!("{:?}", parsed);
  let expected: Vec<Command> = vec![Print(X), Print(Xd), Print(Xdd), Run, Stop];
  assert!(parsed == expected)
}

#[test]
fn test_parse_input_whitespace() {
  use self::Command::*;
  use self::Property::*;

  let s = "Print     X   \n\
           print    xD       \n\
           \n\
           Run      \n\
           stop   \n";

  let parsed = parse_input(s);
  println!("{:?}", parsed);
  let expected: Vec<Command> = vec![Print(X), Print(Xd), Run, Stop];
  assert!(parsed == expected)
}

#[test]
fn test_build_run_list() {
  use self::Command::*;
  use self::Property::*;

  let s = "Print X 16   \n\
           Print Xd 17  \n\
           Run          \n\
           Print X 16   \n\
           Print Xd 17  \n\
           Print Xdd    \n\
           Run          \n\
           Stop         \n\
           Print X 16   \n\
           Print Xd 17  \n\
           Print Xdd    \n\
           Run          \n\
           ";

  let runs = build_run_list(&parse_input(s));
  println!("{:?}", runs);
  let expected: Vec<(Command, Vec<Property>)> =
    vec![(Run, vec![Time, X, Xd]), (Run, vec![Time, X, Xd, Xdd])];
  assert!(runs == expected)
}

#[test]
fn test_commands_valid() {
  use self::Command::*;
  assert!(commands_valid(&vec![Print(Property::X), Run, Stop]) == true);
  assert!(commands_valid(&vec![Print(Property::X), Invalid, Stop]) == false);
  assert!(commands_valid(&vec![Print(Property::X), Run, Print(Property::Invalid)]) == true)
}

#[test]
fn test_properties_valid() {
  use self::Command::*;
  assert!(properties_valid(&vec![Print(Property::X), Run, Stop]) == true);
  assert!(properties_valid(&vec![Print(Property::X), Invalid, Stop]) == true);
  assert!(properties_valid(&vec![Print(Property::X), Run, Print(Property::Invalid)]) == false)
}
