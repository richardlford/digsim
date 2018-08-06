with digsimio;
use  digsimio;
package framework is
   size : constant := 50;
   -- State and state derivative lists
   nstates    : integer;
   xstate     : array(1..size) of names;
   xdotstate  : array(1..size) of names;

   -- Print lists for output.dat files
   print_list : array(1..size) of names;
   nprints    : natural := 0;

   -- Print lists for output.dat files
   plotx_list : array(1..size) of positive;
   ploty_list : array(1..size) of positive;
   nplots     : natural := 0;

   -- Set variable lists for input.dat configuration files
   set_list : array(1..size) of config_type;
   nsets    : natural := 0;

   -- Event lists for scheduling discretes
   type event_name is (noop,log_print_data,terminate_simulation,bounce);
   nevents : natural := 0;
   type event_type is record
      name : event_name;
      time : float;
   end record;
   event_list : array(1..size) of event_type;
end framework;
