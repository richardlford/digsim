with text_io,ada.numerics;
package global is
   batch  : natural;    -- Simulation Batch Number incremented by STOP command
   runno  : natural;    -- Simulation Run Number incremented by RUN command

   big    : constant := 10_000_000_000.0;
   small  : constant := 0.000_001;
   rdtodg : constant := 180.0/ada.numerics.pi;

   time                : aliased float;    -- Acceleration of suspended mass [m/sec**2]
   time0               : aliased float;    -- Simulation time [sec]
   tstop               : aliased float;    -- Simulation stop time [sec]
   dt                  : aliased float;    -- Simulation time step [sec]
   dtmax               : aliased float;    -- Maximum allowable simulation time step [sec]
   dtmin               : aliased float;    -- Minimum allowable simulation time step [sec]
   dtprint             : aliased float;    -- Time step between printing data [sec]
   recalc              : aliased boolean;  -- Indicates states changed value at a discrete event [Logical]
   quit                : aliased boolean;  -- Flag set simulation events to done [Logical]
   over                : aliased boolean;  -- Indicates no more input to processt [Logical]

   damping_coefficient : aliased float;    -- Damping force per velocity [N/m/s]
   gravity             : aliased float;    -- Acceleration due to gravity [m/sec**2]
   mass                : aliased float;    -- Mass suspended from spring [Kg]
   spring_coefficient  : aliased float;    -- Restoring force per position [N/m]
   x                   : aliased float;    -- Position of suspended mass [m]
   x_ic                : aliased float;    -- Initial velocity of suspended mass [m/sec]
   xd                  : aliased float;    -- Velocity of suspended mass [m/sec]
   xd_ic               : aliased float;    -- Initial position of suspended mass [m]
   xdd                 : aliased float;    -- Acceleration of suspended mass [m/sec**2]
end global;

