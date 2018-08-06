with iomap;
package deriv is
   max_number_of_real_states : constant := 5;
   -- State and state derivative pointers
   ndes       : integer;
   x_real     : array(1..max_number_of_real_states) of iomap.names;
   xdot_real  : array(1..max_number_of_real_states) of iomap.names;
end deriv;
