with text_io,global;
use  text_io;
package digsimio is
   -- Global simulation variable names for user access in input.dat file. See what they mean below.
   type names is (time,time0,tstop,dt,dtmax,dtmin,dtprint,recalc,quit,over,
                  aaerox_bi_b, aaeroy_bi_b, aaeroz_bi_b, alpha, alpha_ref, beta, beta_ref,
                  drag_per_velsq, rmin_xy, rmin_xz, tau_p, omega0_q, omega0_r, zeta_q, zeta_r,
                  alpha_ref_dg, beta_ref_dg, freq0_q, freq0_r, p_g_meas, q_g_meas, r_g_meas,
                  p_b_cmd, q_b_cmd, r_b_cmd, q0_b_est, q1_b_est, q2_b_est, q3_b_est,
                  q0d_b_est, q1d_b_est, q2d_b_est, q3d_b_est, pitch_guidance_gain,
                  roll_guidance_gain, yaw_guidance_gain, psi_b_est_dg, theta_b_est_dg,
                  phi_b_est_dg, psi_b_est, theta_b_est, phi_b_est, q_b_cmd_bias, x_bi_i,
                  xd_bi_i, xdd_bi_i, y_bi_i, yd_bi_i, ydd_bi_i, z_bi_i, zd_bi_i, zdd_bi_i,
                  p_b, pd_b, q_b, qd_b, r_b, rd_b, psi_b, theta_b, phi_b, q0_b, q0d_b, q1_b,
                  q1d_b, q2_b, q2d_b, q3_b, q3d_b, tib11, tib12, tib13, tib21, tib22, tib23,
                  tib31, tib32, tib33, psi_b_ic_dg, theta_b_ic_dg, phi_b_ic_dg, p_b_ic_dg,
                  q_b_ic_dg, r_b_ic_dg, xd_bi_i_ic, yd_bi_i_ic, zd_bi_i_ic, x_bi_i_ic,
                  y_bi_i_ic, z_bi_i_ic, acc_gravity, q_si_b_meas, r_si_b_meas, p_s, q_s, r_s,
                  range_tb_sq, x_tb_i, xd_tb_i, y_tb_i, yd_tb_i, z_tb_i, zd_tb_i, x_ti_i,
                  xd_ti_i, y_ti_i, yd_ti_i, z_ti_i, zd_ti_i, x_ti_i_ic, y_ti_i_ic, z_ti_i_ic,
                  xd_ti_i_ic, yd_ti_i_ic, zd_ti_i_ic);

   -- DigSim types
   type string_ptr is access string;
   type string_ptr_list is array(positive range <>) of string_ptr;
   type commands is (print,plot,set,run,stop);
   type kinds is (real,int8,bool);
   type config_type (kind : kinds := real) is
      record
         name : names;
         case kind is
            when real => r : float;
            when int8 => i : integer;
            when bool => b : boolean;
         end case;
      end record;
   type cell_type (kind : kinds := real) is
      record
         numb : integer;
         desc : access string;
         case kind is
            when real => r : access float;
            when int8 => i : access integer;
            when bool => b : access boolean;
         end case;
      end record;
   type cell_list_type is array(names) of cell_type;

   -- Packages needed for IO
   package names_io    is new enumeration_io(names);
   package commands_io is new enumeration_io(commands);
   package boolean_io  is new enumeration_io(boolean);
   package integer_io  is new enumeration_io(integer);
   use names_io, commands_io, boolean_io, integer_io;

   -- Master mapping of global data to user data
   cell : cell_list_type :=
     (time                	=> (real,  1, new string'("Simulation time [sec]"                                            ),global.time                 'access),
      time0               	=> (real,  2, new string'("Initial time [sec]"                                               ),global.time0                'access),
      tstop               	=> (real,  3, new string'("Simulation stop time [sec]"                                       ),global.tstop                'access),
      dt                  	=> (real,  4, new string'("Integration step size [sec]"                                      ),global.dt                   'access),
      dtmax               	=> (real,  5, new string'("Maximum allowable simulation time step [sec]"                     ),global.dtmax                'access),
      dtmin               	=> (real,  6, new string'("Minimum allowable simulation time step [sec]"                     ),global.dtmin                'access),
      dtprint             	=> (real,  7, new string'("Time step between printing data [sec]"                            ),global.dtprint              'access),
      recalc              	=> (bool,  8, new string'("Indicates states changed value at a discrete event [Logical]"     ),global.recalc               'access),
      quit                	=> (bool,  9, new string'("Flag set simulation events to done [Logical]"                     ),global.quit                 'access),
      over                	=> (bool, 10, new string'("Flag set simulation events to done [Logical]"                     ),global.over                 'access),
      aaerox_bi_b               => (real,100, new string'("Accelerations due to aero in BCS [m/sec**2]"			     ), global.aaerox_bi_b         'access),
      aaeroy_bi_b               => (real,101, new string'("Accelerations due to aero in BCS [m/sec**2]"			     ), global.aaeroy_bi_b         'access),
      aaeroz_bi_b               => (real,102, new string'("Accelerations due to aero in BCS [m/sec**2]"			     ), global.aaeroz_bi_b         'access),
      alpha                     => (real,103, new string'("Angle of attack [rad]"					     ), global.alpha               'access),
      alpha_ref                 => (real,104, new string'("Reference alpha [rad]"					     ), global.alpha_ref           'access),
      beta                      => (real,105, new string'("Sideslip angle [rad]" 					     ), global.beta                'access),
      beta_ref                  => (real,106, new string'("Reference beta [rad]" 					     ), global.beta_ref            'access),
      drag_per_velsq            => (real,107, new string'("Base axial drag coefficient [Real]"                               ), global.drag_per_velsq      'access),
      rmin_xy                   => (real,108, new string'("Minimum turning radius in X-Y plane (BCS) [m]"		     ), global.rmin_xy             'access),
      rmin_xz                   => (real,109, new string'("Minimum turning radius in X-Z plane (BCS) [m]"		     ), global.rmin_xz             'access),
      tau_p                     => (real,110, new string'("Time constant for roll channel [sec]"                             ), global.tau_p               'access),
      omega0_q                  => (real,111, new string'("Natural frequency for pitch channel [rad/sec]"		     ), global.omega0_q            'access),
      omega0_r                  => (real,112, new string'("Natural frequency for yaw channel [rad/sec]"  		     ), global.omega0_r            'access),
      zeta_q                    => (real,113, new string'("Damping ratio for pitch channel [Real]"			     ), global.zeta_q              'access),
      zeta_r                    => (real,114, new string'("Damping ratio for yaw channel [Real]"  			     ), global.zeta_r              'access),
      alpha_ref_dg              => (real,115, new string'("Reference alpha [deg]"					     ), global.alpha_ref_dg        'access),
      beta_ref_dg               => (real,116, new string'("Reference beta [deg]" 					     ), global.beta_ref_dg         'access),
      freq0_q                   => (real,117, new string'("Natural frequency for pitch channel [Hz]"			     ), global.freq0_q             'access),
      freq0_r                   => (real,118, new string'("Natural frequency for yaw channel [Hz]"  			     ), global.freq0_r             'access),
      p_g_meas                  => (real,200, new string'("Measured angular velocity [rad/sec]"				     ), global.p_g_meas            'access),
      q_g_meas                  => (real,201, new string'("Measured angular velocity [rad/sec]"				     ), global.q_g_meas            'access),
      r_g_meas                  => (real,202, new string'("Measured angular velocity [rad/sec]"				     ), global.r_g_meas            'access),
      p_b_cmd                   => (real,300, new string'("Commanded inertial angular velocity [rad/sec]"		     ), global.p_b_cmd             'access),
      q_b_cmd                   => (real,301, new string'("Commanded inertial angular velocity [rad/sec]"		     ), global.q_b_cmd             'access),
      r_b_cmd                   => (real,302, new string'("Commanded inertial angular velocity [rad/sec]"		     ), global.r_b_cmd             'access),
      q0_b_est                  => (real,303, new string'("Estimated ICS to BCS quaternion [Real]"			     ), global.q0_b_est            'access),
      q1_b_est                  => (real,304, new string'("Estimated ICS to BCS quaternion [Real]"			     ), global.q1_b_est            'access),
      q2_b_est                  => (real,305, new string'("Estimated ICS to BCS quaternion [Real]"			     ), global.q2_b_est            'access),
      q3_b_est                  => (real,306, new string'("Estimated ICS to BCS quaternion [Real]"			     ), global.q3_b_est            'access),
      q0d_b_est                 => (real,307, new string'("Estimated ICS to BCS quaternion derivatives [Real]"		     ), global.q0d_b_est           'access),
      q1d_b_est                 => (real,308, new string'("Estimated ICS to BCS quaternion derivatives [Real]"		     ), global.q1d_b_est           'access),
      q2d_b_est                 => (real,309, new string'("Estimated ICS to BCS quaternion derivatives [Real]"		     ), global.q2d_b_est           'access),
      q3d_b_est                 => (real,310, new string'("Estimated ICS to BCS quaternion derivatives [Real]"		     ), global.q3d_b_est           'access),
      pitch_guidance_gain       => (real,311, new string'("Pitch guidance gain for terminal homing [Real]"		     ), global.pitch_guidance_gain 'access),
      roll_guidance_gain        => (real,312, new string'("Roll guidance gain for terminal homing [Real]" 		     ), global.roll_guidance_gain  'access),
      yaw_guidance_gain         => (real,313, new string'("Yaw guidance gain for terminal homing [Real]"  		     ), global.yaw_guidance_gain   'access),
      psi_b_est_dg              => (real,314, new string'("Estimated missile Euler angles [deg]"			     ), global.psi_b_est_dg        'access),
      theta_b_est_dg            => (real,315, new string'("Sine of estimated pitch Euler angle" 			     ), global.theta_b_est_dg      'access),
      phi_b_est_dg              => (real,316, new string'("Estimated missile Euler angles [deg]"                             ), global.phi_b_est_dg        'access),
      psi_b_est                 => (real,317, new string'("Estimated missile Euler angles [rad]"                             ), global.psi_b_est           'access),
      theta_b_est               => (real,318, new string'("Estimated missile Euler angles [deg]"		   	     ), global.theta_b_est         'access),
      phi_b_est                 => (real,319, new string'("Estimated missile Euler angles [deg]"		       	     ), global.phi_b_est           'access),
      q_b_cmd_bias              => (real,320, new string'("Bias on pitch rate command [rad/sec]"			     ), global.q_b_cmd_bias        'access),
      x_bi_i                    => (real,500, new string'("Missile terminal position WRT ICS [m]"            		     ), global.x_bi_i              'access),
      xd_bi_i                   => (real,501, new string'("Missile velocity in ICS [m/sec]"                  		     ), global.xd_bi_i             'access),
      xdd_bi_i                  => (real,502, new string'("Missile acceleration WRT ICS in ICS [m/sec**2]"   		     ), global.xdd_bi_i            'access),
      y_bi_i                    => (real,503, new string'("Missile terminal position WRT ICS [m]"            		     ), global.y_bi_i              'access),
      yd_bi_i                   => (real,504, new string'("Missile velocity in ICS [m/sec]"                  		     ), global.yd_bi_i             'access),
      ydd_bi_i                  => (real,505, new string'("Missile acceleration WRT ICS in ICS [m/sec**2]"   		     ), global.ydd_bi_i            'access),
      z_bi_i                    => (real,506, new string'("Missile terminal position WRT ICS [m]"            		     ), global.z_bi_i              'access),
      zd_bi_i                   => (real,507, new string'("Missile velocity in ICS [m/sec]"                  		     ), global.zd_bi_i             'access),
      zdd_bi_i                  => (real,508, new string'("Missile acceleration WRT ICS in ICS [m/sec**2]"   		     ), global.zdd_bi_i            'access),
      p_b                       => (real,509, new string'("Missile inertial angular velocity [rad/sec]"      		     ), global.p_b                 'access),
      pd_b                      => (real,510, new string'("Angular accelerations WRT ICS in BCS [rad/sec**2]"		     ), global.pd_b                'access),
      q_b                       => (real,511, new string'("Missile inertial angular velocity [rad/sec]"      		     ), global.q_b                 'access),
      qd_b                      => (real,512, new string'("Angular accelerations WRT ICS in BCS [rad/sec**2]"		     ), global.qd_b                'access),
      r_b                       => (real,513, new string'("Missile inertial angular velocity [rad/sec]"      		     ), global.r_b                 'access),
      rd_b                      => (real,514, new string'("Angular accelerations WRT ICS in BCS [rad/sec**2]"		     ), global.rd_b                'access),
      psi_b                     => (real,515, new string'("Missile Euler angles [rad]"					     ), global.psi_b               'access),
      theta_b                   => (real,516, new string'("Missile Euler angles [rad]"					     ), global.theta_b             'access),
      phi_b                     => (real,517, new string'("Missile Euler angles [rad]"					     ), global.phi_b               'access),
      q0_b                      => (real,518, new string'("ICS to BCS quaternion [Real]"				     ), global.q0_b                'access),
      q0d_b                     => (real,519, new string'("ICS to BCS quaternion derivative [Real]"                          ), global.q0d_b               'access),
      q1_b                      => (real,520, new string'("ICS to BCS quaternion [Real]"                                     ), global.q1_b                'access),
      q1d_b                     => (real,521, new string'("ICS to BCS quaternion derivative [Real]"                          ), global.q1d_b               'access),
      q2_b                      => (real,522, new string'("ICS to BCS quaternion [Real]"                                     ), global.q2_b                'access),
      q2d_b                     => (real,523, new string'("ICS to BCS quaternion derivative [Real]"                          ), global.q2d_b               'access),
      q3_b                      => (real,524, new string'("ICS to BCS quaternion [Real]"                                     ), global.q3_b                'access),
      q3d_b                     => (real,525, new string'("ICS to BCS quaternion derivative [Real]"			     ), global.q3d_b               'access),
      tib11                     => (real,526, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib11               'access),
      tib12                     => (real,527, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib12               'access),
      tib13                     => (real,528, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib13               'access),
      tib21                     => (real,529, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib21               'access),
      tib22                     => (real,530, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib22               'access),
      tib23                     => (real,531, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib23               'access),
      tib31                     => (real,532, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib31               'access),
      tib32                     => (real,533, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib32               'access),
      tib33                     => (real,534, new string'("ICS to BCS transformation matrix [Real]"			     ), global.tib33               'access),
      psi_b_ic_dg               => (real,536, new string'("Initial Euler angles [deg]"					     ), global.psi_b_ic_dg         'access),
      theta_b_ic_dg             => (real,537, new string'("Initial Euler angles [deg]"					     ), global.theta_b_ic_dg       'access),
      phi_b_ic_dg               => (real,538, new string'("Initial Euler angles [deg]"					     ), global.phi_b_ic_dg         'access),
      p_b_ic_dg                 => (real,539, new string'("Initial missile rotational velocity [deg/sec]"		     ), global.p_b_ic_dg           'access),
      q_b_ic_dg                 => (real,540, new string'("Initial missile rotational velocity [deg/sec]"		     ), global.q_b_ic_dg           'access),
      r_b_ic_dg                 => (real,541, new string'("Initial missile rotational velocity [deg/sec]"		     ), global.r_b_ic_dg           'access),
      xd_bi_i_ic                => (real,542, new string'("Initial missile velocity WRT ICS [m/sec]"			     ), global.xd_bi_i_ic          'access),
      yd_bi_i_ic                => (real,543, new string'("Initial missile velocity WRT ICS [m/sec]"			     ), global.yd_bi_i_ic          'access),
      zd_bi_i_ic                => (real,544, new string'("Initial missile velocity WRT ICS [m/sec]"			     ), global.zd_bi_i_ic          'access),
      x_bi_i_ic                 => (real,545, new string'("Initial missile position [m]"				     ), global.x_bi_i_ic           'access),
      y_bi_i_ic                 => (real,546, new string'("Initial missile position [m]"				     ), global.y_bi_i_ic           'access),
      z_bi_i_ic                 => (real,547, new string'("Initial missile position [m]"				     ), global.z_bi_i_ic           'access),
      acc_gravity               => (real,548, new string'("Acceleration due to gravity [m/sec**2]"                           ), global.acc_gravity         'access),
      q_si_b_meas               => (real,600, new string'("LOS rates expressed in the BCS [rad/sec]"			     ), global.q_si_b_meas         'access),
      r_si_b_meas               => (real,601, new string'("LOS rates expressed in the BCS [rad/sec]"			     ), global.r_si_b_meas         'access),
      p_s                       => (real,602, new string'("Inertially referenced LOS rates, expressed in the ICS"	     ), global.p_s                 'access),
      q_s                       => (real,603, new string'("Inertially referenced LOS rates, expressed in the ICS"	     ), global.q_s                 'access),
      r_s                       => (real,604, new string'("Inertially referenced LOS rates, expressed in the ICS"	     ), global.r_s                 'access),
      range_tb_sq               => (real,605, new string'("Target to body range, squared & limited [m**2]"       	     ), global.range_tb_sq         'access),
      x_tb_i                    => (real,606, new string'("Position of the target w.r.t. the BCS, expressed in the ICS [m]"  ), global.x_tb_i              'access),
      xd_tb_i                   => (real,607, new string'("Velocity of the target w.r.t. the BCS, expressed in the ICS"      ), global.xd_tb_i             'access),
      y_tb_i                    => (real,608, new string'("Position of the target w.r.t. the BCS, expressed in the ICS [m]"  ), global.y_tb_i              'access),
      yd_tb_i                   => (real,609, new string'("Velocity of the target w.r.t. the BCS, expressed in the ICS"      ), global.yd_tb_i             'access),
      z_tb_i                    => (real,610, new string'("Position of the target w.r.t. the BCS, expressed in the ICS [m]"  ), global.z_tb_i              'access),
      zd_tb_i                   => (real,611, new string'("Velocity of the target w.r.t. the BCS, expressed in the ICS"      ), global.zd_tb_i             'access),
      x_ti_i                    => (real,700, new string'("Position of target WRT ICS in ICS [m]"     			     ), global.x_ti_i              'access),
      xd_ti_i                   => (real,701, new string'("Velocity of target WRT ICS in ICS [m/sec]" 			     ), global.xd_ti_i             'access),
      y_ti_i                    => (real,702, new string'("Position of target WRT ICS in ICS [m]"     			     ), global.y_ti_i              'access),
      yd_ti_i                   => (real,703, new string'("Velocity of target WRT ICS in ICS [m/sec]" 			     ), global.yd_ti_i             'access),
      z_ti_i                    => (real,704, new string'("Position of target WRT ICS in ICS [m]"     			     ), global.z_ti_i              'access),
      zd_ti_i                   => (real,705, new string'("Velocity of target WRT ICS in ICS [m/sec]" 			     ), global.zd_ti_i             'access),
      x_ti_i_ic                 => (real,706, new string'("Initial X position of target w.r.t. ICS, expressed in ICS [m]"    ), global.x_ti_i_ic           'access),
      y_ti_i_ic                 => (real,707, new string'("Initial Y position of target w.r.t. ICS, expressed in ICS [m]"    ), global.y_ti_i_ic           'access),
      z_ti_i_ic                 => (real,708, new string'("Initial Z position of target w.r.t. ICS, expressed in ICS [m]"    ), global.z_ti_i_ic           'access),
      xd_ti_i_ic                => (real,709, new string'("Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]"), global.xd_ti_i_ic          'access),
      yd_ti_i_ic                => (real,710, new string'("Initial Y velocity of target w.r.t. ICS, expressed in ICS [m/sec]"), global.yd_ti_i_ic          'access),
      zd_ti_i_ic                => (real,711, new string'("Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]"), global.zd_ti_i_ic          'access));
end digsimio;

