fn main() {
    // system parameters
    let damping_coefficient: f64 = 8.88;
    let dt: f64 = 0.01;
    let gravity: f64 = 9.88;
    let mass: f64 = 1.0;
    let spring_coefficient: f64 = 39.47;
    let t_stop: f64 = 2.50;
    let x_ic: f64 = 0.0;
    let xd_ic: f64 = 0.0;

    // set initial conditions
    let mut x = x_ic;
    let mut xd = xd_ic;
    let mut time = 0.0;

    // main simulation loop
    while time <= t_stop {
        // print state for this time
        println!("{:>15.6e} {:>15.6e} {:>15.6e}", time, x, xd);
        // calculate derivative at current time
        let xdd = -(spring_coefficient * x + damping_coefficient * xd) / mass - gravity;
        // advance states one time step
        x += xd * dt;
        xd += xdd * dt;
        // advance time and continue simulation loop
        time += dt;
    }
}
