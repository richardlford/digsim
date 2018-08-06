with framework,digsimio;
use  framework,digsimio;
procedure define_state(x,xdot : names) is
begin
   nstates := nstates + 1;
   xstate(nstates)    := x;
   xdotstate(nstates) := xdot;
end define_state;
