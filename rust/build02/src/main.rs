mod data;
mod state;

use state::{initialize_sim, Simulation};

fn main() {
  // initialize time data object and spring data object
  let time_data: data::TimeData = Default::default();
  let spring_data: data::SpringData = Default::default();

  // initialize the simulation state object
  let sim_state = initialize_sim(time_data, spring_data);

  // display initial state
  println!("{}", sim_state.show_time_x_xd());

  // iterate the simulation
  sim_state
    .take_while(|sim| !sim.is_done())
    .for_each(|sim| println!("{}", sim.show_time_x_xd()));
}
