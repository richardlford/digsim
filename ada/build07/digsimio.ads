with text_io,global;
use  text_io;
package digsimio is
   -- Global simulation variable names for user access in input.dat file. See what they mean below.
   type names is (time, time0, tstop, dt, dtmax, dtmin, dtprint, recalc,quit,over,
                  thetadot_b_cmd,guidance_gain,
                  x_bi_i,z_bi_i,xd_bi_i,zd_bi_i,theta_b,thetadot_b,x_bi_i_ic,z_bi_i_ic,theta_b_ic_dg,velocity,
                  q_s,q_s_meas);
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
     (time                      => (real,  1, new string'(""), global.time                'access),
      time0                     => (real,  2, new string'(""), global.time0               'access),
      tstop                     => (real,  3, new string'(""), global.tstop               'access),
      dt                        => (real,  4, new string'(""), global.dt                  'access),
      dtmax                     => (real,  5, new string'(""), global.dtmax               'access),
      dtmin                     => (real,  6, new string'(""), global.dtmin               'access),
      dtprint                   => (real,  7, new string'(""), global.dtprint             'access),
      recalc                    => (bool,  8, new string'(""), global.recalc              'access),
      quit                      => (bool,  9, new string'(""), global.quit                'access),
      over                      => (bool, 10, new string'(""), global.over                'access),
      --------------------------------------------------------------------------------------------------------------------------------------------------
      thetadot_b_cmd => (real,300, new string'("Commanded attitude rate of missile [rad/sec]"                    ), global.thetadot_b_cmd 'access),
      guidance_gain  => (real,301, new string'("Commanded angular rate per measured LOS rate [Real]"             ), global.guidance_gain  'access),
      --------------------------------------------------------------------------------------------------------------------------------------------------
      x_bi_i         => (real,500, new string'("X position of BCS w.r.t. ICS, expressed in the ICS [m]"          ), global.x_bi_i         'access),
      z_bi_i         => (real,501, new string'("Z position of BCS w.r.t. ICS, expressed in the ICS [m]"          ), global.z_bi_i         'access),
      xd_bi_i        => (real,502, new string'("X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]"      ), global.xd_bi_i        'access),
      zd_bi_i        => (real,503, new string'("Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]"      ), global.zd_bi_i        'access),
      theta_b        => (real,504, new string'("Attitude of missile [rad]"                                       ), global.theta_b        'access),
      thetadot_b     => (real,505, new string'("Attitude rate of missile [rad/sec]"                              ), global.thetadot_b     'access),
      x_bi_i_ic      => (real,506, new string'("Initial X position of BCS w.r t ICS, expressed in the ICS [m]"   ), global.x_bi_i_ic      'access),
      z_bi_i_ic      => (real,507, new string'("Initial Z position of BCS w.r t ICS, expressed in the ICS [m]"   ), global.z_bi_i_ic      'access),
      theta_b_ic_dg  => (real,508, new string'("Initial attitude of missile [deg]"                               ), global.theta_b_ic_dg  'access),
      velocity       => (real,510, new string'("Velocity of missile [m/sec]"                                     ), global.velocity       'access),
      --------------------------------------------------------------------------------------------------------------------------------------------------
      q_s            => (real,600, new string'("LOS rate [rad/sec]"                                              ), global.q_s            'access),
      q_s_meas       => (real,601, new string'("Measured LOS rate [rad/sec]"                                     ), global.q_s_meas       'access));

end digsimio;
