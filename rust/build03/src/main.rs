mod state;

use state::parse::{build_run_list, parse_input_file};
use state::simdata::{Command, SpringData, TimeData};

use state::{display_items, initialize_sim, Simulation};

fn main() {
  // parse input.dat and build run list
  let runs = build_run_list(&parse_input_file());

  runs.into_iter().for_each(|(command, props)| {
    match command {
      Command::Run => {
        // initialize time data object and spring data object
        let time_data: TimeData = Default::default();
        let spring_data: SpringData = Default::default();

        // initialize the simulation state object
        let st_ic = initialize_sim(&time_data, &spring_data);

        // display initial state
        println!("{}", st_ic.show_states(display_items(&st_ic, &props)));

        // iterate the simulation
        st_ic
          .take_while(|st| !st.is_done())
          .for_each(|st| println!("{}", st.show_states(display_items(&st, &props))));
      }
      _ => (),
    }
  })
}
