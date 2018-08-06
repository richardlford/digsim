with text_io,global;
use  text_io;
package digsimio is
   -- Global simulation variable names for user access in input.dat file. See what they mean below.
   type names is (time, time0, tstop, dt, dtmax, dtmin, dtprint, recalc,quit,over,
                  aaerox_bi_b, aaeroz_bi_b, alpha_ref, drag_per_velsq, rmin_xz, omega0_q,
                  zeta_q, acc_alpha, acc_per_alpha, adrag, alpha, alpha_cmd, aq11, aq21, aq22,
                  bq2, velsq, vmag, xd_bi_b, z_acc_max, zd_bi_b, q_b_cmd, guidance_gain,
                  x_bi_i, xd_bi_i, xdd_bi_i, z_bi_i, zd_bi_i, zdd_bi_i, theta_b, q_b, qd_b,
                  x_bi_i_ic, xd_bi_i_ic, z_bi_i_ic, zd_bi_i_ic, theta_b_ic_dg, q_b_ic_dg,
                  gravity, q_s, q_s_meas, x_bt_i, z_bt_i, xd_bt_i, zd_bt_i, x_ti_i, xd_ti_i,
                  z_ti_i, zd_ti_i, x_ti_i_ic, xd_ti_i_ic, z_ti_i_ic, zd_ti_i_ic);

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

      aaerox_bi_b               => (real,100, new string'(""), global.aaerox_bi_b          'access),
      aaeroz_bi_b               => (real,101, new string'(""), global.aaeroz_bi_b          'access),
      alpha_ref                 => (real,102, new string'(""), global.alpha_ref            'access),
      drag_per_velsq            => (real,103, new string'(""), global.drag_per_velsq       'access),
      rmin_xz                   => (real,104, new string'(""), global.rmin_xz              'access),
      omega0_q                  => (real,105, new string'(""), global.omega0_q             'access),
      zeta_q                    => (real,106, new string'(""), global.zeta_q               'access),
      acc_alpha                 => (real,107, new string'(""), global.acc_alpha            'access),
      acc_per_alpha             => (real,108, new string'(""), global.acc_per_alpha        'access),
      adrag                     => (real,109, new string'(""), global.adrag                'access),
      alpha                     => (real,110, new string'(""), global.alpha                'access),
      alpha_cmd                 => (real,111, new string'(""), global.alpha_cmd            'access),
      aq11                      => (real,112, new string'(""), global.aq11                 'access),
      aq21                      => (real,113, new string'(""), global.aq21                 'access),
      aq22                      => (real,114, new string'(""), global.aq22                 'access),
      bq2                       => (real,115, new string'(""), global.bq2                  'access),
      velsq                     => (real,116, new string'(""), global.velsq                'access),
      vmag                      => (real,117, new string'(""), global.vmag                 'access),
      xd_bi_b                   => (real,118, new string'(""), global.xd_bi_b              'access),
      z_acc_max                 => (real,119, new string'(""), global.z_acc_max            'access),
      zd_bi_b                   => (real,120, new string'(""), global.zd_bi_b              'access),
      q_b_cmd                   => (real,300, new string'(""), global.q_b_cmd              'access),
      guidance_gain             => (real,301, new string'(""), global.guidance_gain        'access),
      x_bi_i                    => (real,500, new string'(""), global.x_bi_i               'access),
      xd_bi_i                   => (real,501, new string'(""), global.xd_bi_i              'access),
      xdd_bi_i                  => (real,502, new string'(""), global.xdd_bi_i             'access),
      z_bi_i                    => (real,503, new string'(""), global.z_bi_i               'access),
      zd_bi_i                   => (real,504, new string'(""), global.zd_bi_i              'access),
      zdd_bi_i                  => (real,505, new string'(""), global.zdd_bi_i             'access),
      theta_b                   => (real,506, new string'(""), global.theta_b              'access),
      q_b                       => (real,507, new string'(""), global.q_b                  'access),
      qd_b                      => (real,508, new string'(""), global.qd_b                 'access),
      x_bi_i_ic                 => (real,509, new string'(""), global.x_bi_i_ic            'access),
      xd_bi_i_ic                => (real,510, new string'(""), global.xd_bi_i_ic           'access),
      z_bi_i_ic                 => (real,511, new string'(""), global.z_bi_i_ic            'access),
      zd_bi_i_ic                => (real,512, new string'(""), global.zd_bi_i_ic           'access),
      theta_b_ic_dg             => (real,513, new string'(""), global.theta_b_ic_dg        'access),
      q_b_ic_dg                 => (real,514, new string'(""), global.q_b_ic_dg            'access),
      gravity                   => (real,515, new string'(""), global.gravity              'access),
      q_s                       => (real,600, new string'(""), global.q_s                  'access),
      q_s_meas                  => (real,601, new string'(""), global.q_s_meas             'access),
      x_bt_i                    => (real,602, new string'(""), global.x_bt_i               'access),
      z_bt_i                    => (real,603, new string'(""), global.z_bt_i               'access),
      xd_bt_i                   => (real,604, new string'(""), global.xd_bt_i              'access),
      zd_bt_i                   => (real,605, new string'(""), global.zd_bt_i              'access),
      x_ti_i                    => (real,700, new string'(""), global.x_ti_i               'access),
      xd_ti_i                   => (real,701, new string'(""), global.xd_ti_i              'access),
      z_ti_i                    => (real,702, new string'(""), global.z_ti_i               'access),
      zd_ti_i                   => (real,703, new string'(""), global.zd_ti_i              'access),
      x_ti_i_ic                 => (real,704, new string'(""), global.x_ti_i_ic            'access),
      xd_ti_i_ic                => (real,705, new string'(""), global.xd_ti_i_ic           'access),
      z_ti_i_ic                 => (real,706, new string'(""), global.z_ti_i_ic            'access),
      zd_ti_i_ic                => (real,707, new string'(""), global.zd_ti_i_ic           'access));
end digsimio;

