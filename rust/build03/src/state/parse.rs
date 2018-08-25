use state::simdata::{lookup_command, lookup_property, Command, Property};

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

pub fn parse_input_file() -> Vec<Command> {
  parse_input(include_str!("../../data/input.dat"))
}

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

#[test]
fn test_lookup_property() {
  use self::Property::*;

  assert!(lookup_property("tIMe") == Time);
  assert!(lookup_property("aaa") == Invalid);
  assert!(lookup_property("   xd  ") == Xd);
  assert!(lookup_property("xDD ") == Xdd);
  assert!(lookup_property("  X  ") == X);
}

#[test]
fn test_lookup_command() {
  assert!(lookup_command("print ") == Command::Print(Property::Invalid));
  assert!(lookup_command("Print") == Command::Print(Property::Invalid));
  assert!(lookup_command("Runn") == Command::Invalid);
  assert!(lookup_command("  RUN   ") == Command::Run);
  assert!(lookup_command("  STOp  ") == Command::Stop);
}

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