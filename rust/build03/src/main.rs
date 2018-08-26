mod state;

use state::parse::*;
use state::simdata::*;
use state::spring::*;
use state::*;

fn main() {
  // parse input.dat
  let commands = parse_input_file();

  // were the commands and the property in Print(Property) valid
  if commands_valid(&commands) && properties_valid(&commands) {
    // build run list
    let runs = build_run_list(&commands);

    runs.into_iter().for_each(|(command, print_props)| {
      match command {
        // process the run, print defs are in print_props
        Command::Run => {
          // initialize time data object and spring data object
          let time_data: TimeData = Default::default();
          let spring_data: SpringData = Default::default();

          // create initialized simulation state object
          let st_ic = initialize_sim(time_data, spring_data);

          // display initial state
          println!("{}", st_ic.show_states(&print_props));

          // iterate the simulation
          st_ic
            .take_while(|st| !st.is_done())
            .for_each(|st| println!("{}", st.show_states(&print_props)));
        }
        _ => (),
      }
    })
  } else {
    println!("Simulation never started because there was an error in input.dat")
  }
}
