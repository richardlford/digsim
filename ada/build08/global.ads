with ada.numerics;
package global is
   batch      : natural;    -- Simulation Batch Number incremented by STOP command
   runno      : natural;    -- Simulation Run Number incremented by RUN command

   big        : constant := 10_000_000_000.0;
   small      : constant := 0.000_001;
   rdtodg     : constant := 180.0/ada.numerics.pi;

   time                           : aliased float;    -- Acceleration of suspended mass [m/sec**2]
   time0                          : aliased float;    -- Simulation time [sec]
   tstop                          : aliased float;    -- Simulation stop time [sec]
   dt                             : aliased float;    -- Simulation time step [sec]
   dtmax                          : aliased float;    -- Maximum allowable simulation time step [sec]
   dtmin                          : aliased float;    -- Minimum allowable simulation time step [sec]
   dtprint                        : aliased float;    -- Time step between printing data [sec]
   recalc                         : aliased boolean;  -- Indicates states changed value at a discrete event [Logical]
   quit                           : aliased boolean;  -- Flag set simulation events to done [Logical]
   over                           : aliased boolean;  -- Indicates no more input to processt [Logical]

   aaerox_bi_b                    : aliased float;
   aaeroz_bi_b                    : aliased float;
   alpha_ref                      : aliased float;
   drag_per_velsq                 : aliased float;
   rmin_xz                        : aliased float;
   omega0_q                       : aliased float;
   zeta_q                         : aliased float;
   acc_alpha                      : aliased float;
   acc_per_alpha                  : aliased float;
   adrag                          : aliased float;
   alpha                          : aliased float;
   alpha_cmd                      : aliased float;
   aq11                           : aliased float;
   aq21                           : aliased float;
   aq22                           : aliased float;
   bq2                            : aliased float;
   velsq                          : aliased float;
   vmag                           : aliased float;
   xd_bi_b                        : aliased float;
   z_acc_max                      : aliased float;
   zd_bi_b                        : aliased float;
   q_b_cmd                        : aliased float;
   guidance_gain                  : aliased float;
   x_bi_i                         : aliased float;
   xd_bi_i                        : aliased float;
   xdd_bi_i                       : aliased float;
   z_bi_i                         : aliased float;
   zd_bi_i                        : aliased float;
   zdd_bi_i                       : aliased float;
   theta_b                        : aliased float;
   q_b                            : aliased float;
   qd_b                           : aliased float;
   x_bi_i_ic                      : aliased float;
   xd_bi_i_ic                     : aliased float;
   z_bi_i_ic                      : aliased float;
   zd_bi_i_ic                     : aliased float;
   theta_b_ic_dg                  : aliased float;
   q_b_ic_dg                      : aliased float;
   gravity                        : aliased float;
   q_s                            : aliased float;
   q_s_meas                       : aliased float;
   x_bt_i                         : aliased float;
   z_bt_i                         : aliased float;
   xd_bt_i                        : aliased float;
   zd_bt_i                        : aliased float;
   x_ti_i                         : aliased float;
   xd_ti_i                        : aliased float;
   z_ti_i                         : aliased float;
   zd_ti_i                        : aliased float;
   x_ti_i_ic                      : aliased float;
   xd_ti_i_ic                     : aliased float;
   z_ti_i_ic                      : aliased float;
   zd_ti_i_ic                     : aliased float;
end global;
