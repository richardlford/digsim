mod state;

use state::parse;
use state::simdata;

use state::{initialize_sim, Simulation};

fn main() {
  let commands = parse::parse_input_file();
  println!("{:?}", commands);

  // initialize time data object and spring data object
  let time_data: simdata::TimeData = Default::default();
  let spring_data: simdata::SpringData = Default::default();

  // initialize the simulation state object
  let st_initial = initialize_sim(&time_data, &spring_data);

  // display initial state
  println!(
    "{}",
    st_initial.show_vars(&[st_initial.time, st_initial.x, st_initial.xd])
  );

  // // iterate the simulation
  // st_initial
  //   .take_while(|st| !st.is_done())
  //   .for_each(|st| println!("{}", st.show_vars(&[st.time, st.x, st.xd])));
}
