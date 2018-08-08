mod lib;

use lib::*;

fn main() {
    // initialize the simulation state iterator
    let state: SimState = Default::default();

    // display initial state
    println!("{}", state.show_time_x_xd());

    // iterate the simulation
    state
        .take_while(|sim| !sim.is_done())
        .for_each(|sim| println!("{}", sim.show_time_x_xd()));
}
