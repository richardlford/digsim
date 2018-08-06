with global;
package iomap is
   type names is (time,time0,tstop,dt,damping_coefficient,gravity,mass,spring_coefficient,x_ic,xd_ic,x,xd,xdd);
   type kinds is (real,int8,bool);
   type cells (kind : kinds := real) is
      record
         numb : integer;
         desc : access string;
         case kind is
            when real => r : access float;
            when int8 => i : access integer;
            when bool => b : access boolean;
         end case;
      end record;
   cell : array(names) of cells :=
     (time                => (real, 1, new string'("Simulation time [sec]")                     ,global.time               'access),
      time0               => (real, 2, new string'("Initial time [sec]")                        ,global.time0              'access),
      tstop               => (real, 3, new string'("Simulation stop time [sec]")                ,global.tstop              'access),
      dt                  => (real, 4, new string'("Integration step size [sec]")               ,global.dt                 'access),
      damping_coefficient => (real,10, new string'("Damping force per velocity [N/m/s]")        ,global.damping_coefficient'access),
      gravity             => (real,11, new string'("Acceleration due to gravity [m/sec**2]")    ,global.gravity            'access),
      mass                => (real,12, new string'("Mass suspended from spring [Kg]")           ,global.mass               'access),
      spring_coefficient  => (real,13, new string'("Restoring force per position [N/m]")        ,global.spring_coefficient 'access),
      x_ic                => (real,14, new string'("Initial velocity of suspended mass [m/sec]"),global.x_ic               'access),
      xd_ic               => (real,15, new string'("Initial position of suspended mass [m]")    ,global.xd_ic              'access),
      x                   => (real,16, new string'("Position of suspended mass [m]")            ,global.x                  'access),
      xd                  => (real,17, new string'("Velocity of suspended mass [m/sec]")        ,global.xd                 'access),
      xdd                 => (real,18, new string'("Acceleration of suspended mass [m/sec**2]") ,global.xdd                'access));
end iomap;
