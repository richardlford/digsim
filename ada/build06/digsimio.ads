with text_io,global;
use  text_io;
package digsimio is
   -- Global simulation variable names for user access in input.dat file. See what they mean below.
   type names is (time, time0, tstop, dt, dtmax, dtmin, dtprint, recalc,quit,over,
                  theta_ic_dg,velocity,x_ic,z_ic,x,z,theta,xd,zd,thetadot,thetadot_cmd,q_s,q_s_meas,guidance_gain);

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
     (time               => (real, 1, new string'(""), global.time                'access),
      time0              => (real, 2, new string'(""), global.time0               'access),
      tstop              => (real, 3, new string'(""), global.tstop               'access),
      dt                 => (real, 4, new string'(""), global.dt                  'access),
      dtmax              => (real, 5, new string'(""), global.dtmax               'access),
      dtmin              => (real, 6, new string'(""), global.dtmin               'access),
      dtprint            => (real, 7, new string'(""), global.dtprint             'access),
      recalc             => (bool, 8, new string'(""), global.recalc              'access),
      quit               => (bool, 9, new string'(""), global.quit                'access),
      over               => (bool,10, new string'(""), global.over                'access),
      --------------------------------------------------------------------------------------------------------------------------------------------------
      theta_ic_dg        => (real,12, new string'("Initial attitude of missile [deg]"                            ), global.theta_ic_dg         'access),
      velocity           => (real,13, new string'("Velocity of missile [m/sec]"                                  ), global.velocity            'access),
      x_ic               => (real,14, new string'("Initial X position of missile [m]"                            ), global.x_ic                'access),
      z_ic               => (real,15, new string'("Initial Z position of missile [m]"                            ), global.z_ic                'access),
      x                  => (real,16, new string'("X position of missile [m]"                                    ), global.x                   'access),
      z                  => (real,17, new string'("Z position of missile [m]"                                    ), global.z                   'access),
      theta              => (real,18, new string'("Attitude of missile [rad]"                                    ), global.theta               'access),
      xd                 => (real,19, new string'("X velocity of missile [m/sec]"                                ), global.xd                  'access),
      zd                 => (real,20, new string'("Z velocity of missile [m/sec]"                                ), global.zd                  'access),
      thetadot           => (real,21, new string'("Attitude rate missile [rad/sec]"                              ), global.thetadot            'access),
      thetadot_cmd       => (real,22, new string'("Commanded attitude rate of missile [rad/sec]"                 ), global.thetadot_cmd        'access),
      q_s                => (real,23, new string'("LOS rate [rad/sec]"                                           ), global.q_s                 'access),
      q_s_meas           => (real,24, new string'("Measured LOS rate {rad/sec]"                                  ), global.q_s_meas            'access),
      guidance_gain      => (real,25, new string'("Commanded angular rate per measured LOS rate [Real]"          ), global.guidance_gain       'access));
end digsimio;
