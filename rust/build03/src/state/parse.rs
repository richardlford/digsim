use simdata::*;
use state::lookup_spring_property;

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
            Command::Print(SpringProperty::Invalid) => Command::Print(lookup_spring_property(v)),
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

#[test]
fn test_lookup_spring_property() {
  use simdata::SpringProperty::*;

  assert!(lookup_spring_property("dt") == Dt);
  assert!(lookup_spring_property("aaa") == Invalid);
  assert!(lookup_spring_property("TSTOP  ") == TStop);
  assert!(lookup_spring_property("springCoefficient") == SpringCoefficient);
  assert!(lookup_spring_property("  gravityAAA  ") == Invalid);
}

#[test]
fn test_lookup_command() {
  use simdata::Command::*;

  assert!(lookup_command("print ") == Print(SpringProperty::Invalid));
  assert!(lookup_command("Print") == Print(SpringProperty::Invalid));
  assert!(lookup_command("Runn") == Invalid);
  assert!(lookup_command("  RUN   ") == Run);
  assert!(lookup_command("  STOp  ") == Stop);
}

#[test]
fn test_parse_input_case() {
  use simdata::Command::*;
  use simdata::SpringProperty::*;

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
  use simdata::Command::*;
  use simdata::SpringProperty::*;

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
