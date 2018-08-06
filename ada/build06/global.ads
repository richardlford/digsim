with ada.numerics;
package global is
   batch      : natural;    -- Simulation Batch Number incremented by STOP command
   runno      : natural;    -- Simulation Run Number incremented by RUN command

   big        : constant := 10_000_000_000.0;
   small      : constant := 0.000_001;
   rdtodg     : constant := 180.0/ada.numerics.pi;

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

   -------------------------------------------------------------------------------------------------------
   theta_ic_dg         : aliased float;    -- Initial attitude of missile [deg]
   velocity            : aliased float;    -- Velocity of missile [m/sec]
   x_ic                : aliased float;    -- Initial X position of missile [m]
   z_ic                : aliased float;    -- Initial Z position of missile [m]
   x                   : aliased float;    -- X position of missile [m]
   z                   : aliased float;    -- Z position of missile [m]
   theta               : aliased float;    -- Attitude of missile [rad]
   xd                  : aliased float;    -- X velocity of missile [m/sec]
   zd                  : aliased float;    -- Z velocity of missile [m/sec]
   thetadot            : aliased float;    -- Attitude rate missile [rad/sec]
   thetadot_cmd        : aliased float;    -- Commanded attitude rate of missile [rad/sec]
   q_s                 : aliased float;    -- LOS rate [rad/sec]
   q_s_meas            : aliased float;    -- Measured LOS rate {rad/sec]
   guidance_gain       : aliased float;    -- Commanded angular rate per measured LOS rate [Real]
end global;

