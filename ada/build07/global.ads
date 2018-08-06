with ada.numerics;
package global is
   batch      : natural;    -- Simulation Batch Number incremented by STOP command
   runno      : natural;    -- Simulation Run Number incremented by RUN command

   big        : constant := 10_000_000_000.0;
   small      : constant := 0.000_001;
   rdtodg     : constant := 180.0/ada.numerics.pi;

   time          : aliased float;    -- Acceleration of suspended mass [m/sec**2]
   time0         : aliased float;    -- Simulation time [sec]
   tstop         : aliased float;    -- Simulation stop time [sec]
   dt            : aliased float;    -- Simulation time step [sec]
   dtmax         : aliased float;    -- Maximum allowable simulation time step [sec]
   dtmin         : aliased float;    -- Minimum allowable simulation time step [sec]
   dtprint       : aliased float;    -- Time step between printing data [sec]
   recalc        : aliased boolean;  -- Indicates states changed value at a discrete event [Logical]
   quit          : aliased boolean;  -- Flag set simulation events to done [Logical]
   over          : aliased boolean;  -- Indicates no more input to processt [Logical]
   -------------------------------------------------------------------------------------------------
   thetadot_b_cmd: aliased float;  -- Commanded attitude rate of missile [rad/sec]
   guidance_gain : aliased float;  -- Commanded angular rate per measured LOS rate [Real]
   -------------------------------------------------------------------------------------------------
   x_bi_i        : aliased float;  -- X position of BCS w.r.t. ICS, expressed in the ICS [m]
   z_bi_i        : aliased float;  -- Z position of BCS w.r.t. ICS, expressed in the ICS [m]
   xd_bi_i       : aliased float;  -- X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
   zd_bi_i       : aliased float;  -- Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
   theta_b       : aliased float;  -- Attitude of missile [rad]
   thetadot_b    : aliased float;  -- Attitude rate of missile [rad/sec]
   x_bi_i_ic     : aliased float;  -- Initial X position of BCS w.r t ICS, expressed in the ICS [m]
   z_bi_i_ic     : aliased float;  -- Initial Z position of BCS w.r t ICS, expressed in the ICS [m]
   theta_b_ic_dg : aliased float;  -- Initial attitude of missile [deg]
   velocity      : aliased float;  -- Velocity of missile [m/sec]
   -------------------------------------------------------------------------------------------------
   q_s           : aliased float;  -- LOS rate [rad/sec]
   q_s_meas      : aliased float;  -- Measured LOS rate [rad/sec]
end global;

