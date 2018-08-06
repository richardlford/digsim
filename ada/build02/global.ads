with text_io; use text_io;
package global is
   output              : file_type;        -- Simulation output file handle
   damping_coefficient : aliased float;    -- Damping force per velocity [N/m/s]
   dt                  : aliased float;    -- Simulation time step [sec]
   gravity             : aliased float;    -- Acceleration due to gravity [m/sec**2]
   mass                : aliased float;    -- Mass suspended from spring [Kg]
   spring_coefficient  : aliased float;    -- Restoring force per position [N/m]
   time                : aliased float;    -- Acceleration of suspended mass [m/sec**2]
   time0               : aliased float;    -- Simulation time [sec]
   tstop               : aliased float;    -- Simulation stop time [sec]
   x                   : aliased float;    -- Position of suspended mass [m]
   x_ic                : aliased float;    -- Initial velocity of suspended mass [m/sec]
   xd                  : aliased float;    -- Velocity of suspended mass [m/sec]
   xd_ic               : aliased float;    -- Initial position of suspended mass [m]
   xdd                 : aliased float;    -- Acceleration of suspended mass [m/sec**2]
end global;

