with text_io,global;
use  text_io;
package digsimio is
   -- Global simulation variable names for user access in input.dat file. See what they mean below.
   type names is (time, time0, tstop, dt, dtmax, dtmin, dtprint, recalc,quit,over,
                  coef_of_restitution,gravity,x_ic,xd_ic,x,xd,xdd);

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
      coef_of_restitution => (real,10, new string'("Coefficent of restitution [Real]"                            ), global.coef_of_restitution 'access),
      gravity             => (real,11, new string'("Acceleration due to gravity [m/sec**2]"                      ), global.gravity             'access),
      x_ic                => (real,12, new string'("Initial position of ball [m]"                                ), global.x_ic                'access),
      xd_ic               => (real,13, new string'("Initial velocity of ball [m/sec]"                            ), global.xd_ic               'access),
      x                   => (real,14, new string'("Position of ball [m]"                                        ), global.x                   'access),
      xd                  => (real,15, new string'("Velocity of ball [m/sec]"                                    ), global.xd                  'access),
      xdd                 => (real,16, new string'("Acceleration of ball [m/sec**2]"                             ), global.xdd                 'access));

end digsimio;
