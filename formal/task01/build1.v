From Coq Require Import String List ZArith.
From compcert Require Import Coqlib Integers Floats AST Ctypes Cop Clight Clightdefs.
Local Open Scope Z_scope.

Definition __IO_FILE : ident := 3%positive.
Definition __IO_backup_base : ident := 16%positive.
Definition __IO_buf_base : ident := 13%positive.
Definition __IO_buf_end : ident := 14%positive.
Definition __IO_marker : ident := 1%positive.
Definition __IO_read_base : ident := 9%positive.
Definition __IO_read_end : ident := 8%positive.
Definition __IO_read_ptr : ident := 7%positive.
Definition __IO_save_base : ident := 15%positive.
Definition __IO_save_end : ident := 17%positive.
Definition __IO_write_base : ident := 10%positive.
Definition __IO_write_end : ident := 12%positive.
Definition __IO_write_ptr : ident := 11%positive.
Definition ___builtin_ais_annot : ident := 43%positive.
Definition ___builtin_annot : ident := 50%positive.
Definition ___builtin_annot_intval : ident := 51%positive.
Definition ___builtin_bswap : ident := 44%positive.
Definition ___builtin_bswap16 : ident := 46%positive.
Definition ___builtin_bswap32 : ident := 45%positive.
Definition ___builtin_bswap64 : ident := 76%positive.
Definition ___builtin_clz : ident := 77%positive.
Definition ___builtin_clzl : ident := 78%positive.
Definition ___builtin_clzll : ident := 79%positive.
Definition ___builtin_ctz : ident := 80%positive.
Definition ___builtin_ctzl : ident := 81%positive.
Definition ___builtin_ctzll : ident := 82%positive.
Definition ___builtin_debug : ident := 94%positive.
Definition ___builtin_fabs : ident := 47%positive.
Definition ___builtin_fmadd : ident := 85%positive.
Definition ___builtin_fmax : ident := 83%positive.
Definition ___builtin_fmin : ident := 84%positive.
Definition ___builtin_fmsub : ident := 86%positive.
Definition ___builtin_fnmadd : ident := 87%positive.
Definition ___builtin_fnmsub : ident := 88%positive.
Definition ___builtin_fsqrt : ident := 48%positive.
Definition ___builtin_membar : ident := 52%positive.
Definition ___builtin_memcpy_aligned : ident := 49%positive.
Definition ___builtin_nop : ident := 93%positive.
Definition ___builtin_read16_reversed : ident := 89%positive.
Definition ___builtin_read32_reversed : ident := 90%positive.
Definition ___builtin_va_arg : ident := 54%positive.
Definition ___builtin_va_copy : ident := 55%positive.
Definition ___builtin_va_end : ident := 56%positive.
Definition ___builtin_va_start : ident := 53%positive.
Definition ___builtin_write16_reversed : ident := 91%positive.
Definition ___builtin_write32_reversed : ident := 92%positive.
Definition ___compcert_i64_dtos : ident := 61%positive.
Definition ___compcert_i64_dtou : ident := 62%positive.
Definition ___compcert_i64_sar : ident := 73%positive.
Definition ___compcert_i64_sdiv : ident := 67%positive.
Definition ___compcert_i64_shl : ident := 71%positive.
Definition ___compcert_i64_shr : ident := 72%positive.
Definition ___compcert_i64_smod : ident := 69%positive.
Definition ___compcert_i64_smulh : ident := 74%positive.
Definition ___compcert_i64_stod : ident := 63%positive.
Definition ___compcert_i64_stof : ident := 65%positive.
Definition ___compcert_i64_udiv : ident := 68%positive.
Definition ___compcert_i64_umod : ident := 70%positive.
Definition ___compcert_i64_umulh : ident := 75%positive.
Definition ___compcert_i64_utod : ident := 64%positive.
Definition ___compcert_i64_utof : ident := 66%positive.
Definition ___compcert_va_composite : ident := 60%positive.
Definition ___compcert_va_float64 : ident := 59%positive.
Definition ___compcert_va_int32 : ident := 57%positive.
Definition ___compcert_va_int64 : ident := 58%positive.
Definition ___pad1 : ident := 28%positive.
Definition ___pad2 : ident := 29%positive.
Definition ___pad3 : ident := 30%positive.
Definition ___pad4 : ident := 31%positive.
Definition ___pad5 : ident := 32%positive.
Definition ___stringlit_1 : ident := 111%positive.
Definition ___stringlit_2 : ident := 112%positive.
Definition __chain : ident := 19%positive.
Definition __cur_column : ident := 23%positive.
Definition __fileno : ident := 20%positive.
Definition __flags : ident := 6%positive.
Definition __flags2 : ident := 21%positive.
Definition __lock : ident := 26%positive.
Definition __markers : ident := 18%positive.
Definition __mode : ident := 33%positive.
Definition __next : ident := 2%positive.
Definition __offset : ident := 27%positive.
Definition __old_offset : ident := 22%positive.
Definition __pos : ident := 5%positive.
Definition __sbuf : ident := 4%positive.
Definition __shortbuf : ident := 25%positive.
Definition __unused2 : ident := 34%positive.
Definition __vtable_offset : ident := 24%positive.
Definition _cell : ident := 107%positive.
Definition _data : ident := 37%positive.
Definition _dt : ident := 101%positive.
Definition _exit : ident := 100%positive.
Definition _f : ident := 106%positive.
Definition _fclose : ident := 95%positive.
Definition _filename : ident := 105%positive.
Definition _first : ident := 40%positive.
Definition _fopen : ident := 96%positive.
Definition _fprintf : ident := 97%positive.
Definition _item : ident := 35%positive.
Definition _last : ident := 41%positive.
Definition _main : ident := 113%positive.
Definition _next : ident := 39%positive.
Definition _printf : ident := 98%positive.
Definition _run_sim : ident := 99%positive.
Definition _s_state : ident := 36%positive.
Definition _s_state_list : ident := 42%positive.
Definition _s_state_list_cell : ident := 38%positive.
Definition _state : ident := 103%positive.
Definition _states : ident := 104%positive.
Definition _time : ident := 108%positive.
Definition _tstop : ident := 102%positive.
Definition _x : ident := 109%positive.
Definition _xd : ident := 110%positive.
Definition _t'1 : ident := 114%positive.
Definition _t'2 : ident := 115%positive.

Definition v___stringlit_2 := {|
  gvar_info := (tarray tschar 23);
  gvar_init := (Init_int8 (Int.repr 37) :: Init_int8 (Int.repr 51) ::
                Init_int8 (Int.repr 46) :: Init_int8 (Int.repr 49) ::
                Init_int8 (Int.repr 53) :: Init_int8 (Int.repr 101) ::
                Init_int8 (Int.repr 9) :: Init_int8 (Int.repr 37) ::
                Init_int8 (Int.repr 51) :: Init_int8 (Int.repr 46) ::
                Init_int8 (Int.repr 49) :: Init_int8 (Int.repr 53) ::
                Init_int8 (Int.repr 101) :: Init_int8 (Int.repr 9) ::
                Init_int8 (Int.repr 37) :: Init_int8 (Int.repr 51) ::
                Init_int8 (Int.repr 46) :: Init_int8 (Int.repr 49) ::
                Init_int8 (Int.repr 53) :: Init_int8 (Int.repr 101) ::
                Init_int8 (Int.repr 9) :: Init_int8 (Int.repr 10) ::
                Init_int8 (Int.repr 0) :: nil);
  gvar_readonly := true;
  gvar_volatile := false
|}.

Definition v___stringlit_1 := {|
  gvar_info := (tarray tschar 2);
  gvar_init := (Init_int8 (Int.repr 119) :: Init_int8 (Int.repr 0) :: nil);
  gvar_readonly := true;
  gvar_volatile := false
|}.

Definition f_main := {|
  fn_return := tint;
  fn_callconv := cc_default;
  fn_params := nil;
  fn_vars := ((_state, (Tstruct _s_state noattr)) ::
              (_filename, (tarray tschar 50)) :: nil);
  fn_temps := ((_dt, tdouble) :: (_tstop, tdouble) ::
               (_states, (tptr (Tstruct _s_state_list noattr))) ::
               (_f, (tptr (Tstruct __IO_FILE noattr))) ::
               (_cell, (tptr (Tstruct _s_state_list_cell noattr))) ::
               (_time, tdouble) :: (_x, tdouble) :: (_xd, tdouble) ::
               (_t'2, (tptr (Tstruct __IO_FILE noattr))) ::
               (_t'1, (tptr (Tstruct _s_state_list noattr))) :: nil);
  fn_body :=
(Ssequence
  (Ssequence
    (Sset _dt
      (Econst_float (Float.of_bits (Int64.repr 4576918229304087675)) tdouble))
    (Ssequence
      (Sset _tstop
        (Econst_float (Float.of_bits (Int64.repr 4612811918334230528)) tdouble))
      (Ssequence
        (Sassign
          (Ederef
            (Ebinop Oadd
              (Efield (Evar _state (Tstruct _s_state noattr)) _item
                (tarray tdouble 3)) (Econst_int (Int.repr 0) tint)
              (tptr tdouble)) tdouble)
          (Econst_float (Float.of_bits (Int64.repr 0)) tdouble))
        (Ssequence
          (Sassign
            (Ederef
              (Ebinop Oadd
                (Efield (Evar _state (Tstruct _s_state noattr)) _item
                  (tarray tdouble 3)) (Econst_int (Int.repr 1) tint)
                (tptr tdouble)) tdouble)
            (Econst_float (Float.of_bits (Int64.repr 0)) tdouble))
          (Ssequence
            (Sassign
              (Ederef
                (Ebinop Oadd
                  (Efield (Evar _state (Tstruct _s_state noattr)) _item
                    (tarray tdouble 3)) (Econst_int (Int.repr 2) tint)
                  (tptr tdouble)) tdouble)
              (Econst_float (Float.of_bits (Int64.repr 0)) tdouble))
            (Ssequence
              (Ssequence
                (Scall (Some _t'1)
                  (Evar _run_sim (Tfunction
                                   (Tcons (Tstruct _s_state noattr)
                                     (Tcons tdouble (Tcons tdouble Tnil)))
                                   (tptr (Tstruct _s_state_list noattr))
                                   cc_default))
                  ((Evar _state (Tstruct _s_state noattr)) ::
                   (Etempvar _tstop tdouble) :: (Etempvar _dt tdouble) ::
                   nil))
                (Sset _states
                  (Etempvar _t'1 (tptr (Tstruct _s_state_list noattr)))))
              (Ssequence
                (Sassign
                  (Ederef
                    (Ebinop Oadd (Evar _filename (tarray tschar 50))
                      (Econst_int (Int.repr 0) tint) (tptr tschar)) tschar)
                  (Econst_int (Int.repr 111) tint))
                (Ssequence
                  (Sassign
                    (Ederef
                      (Ebinop Oadd (Evar _filename (tarray tschar 50))
                        (Econst_int (Int.repr 1) tint) (tptr tschar)) tschar)
                    (Econst_int (Int.repr 117) tint))
                  (Ssequence
                    (Sassign
                      (Ederef
                        (Ebinop Oadd (Evar _filename (tarray tschar 50))
                          (Econst_int (Int.repr 2) tint) (tptr tschar))
                        tschar) (Econst_int (Int.repr 116) tint))
                    (Ssequence
                      (Sassign
                        (Ederef
                          (Ebinop Oadd (Evar _filename (tarray tschar 50))
                            (Econst_int (Int.repr 3) tint) (tptr tschar))
                          tschar) (Econst_int (Int.repr 112) tint))
                      (Ssequence
                        (Sassign
                          (Ederef
                            (Ebinop Oadd (Evar _filename (tarray tschar 50))
                              (Econst_int (Int.repr 4) tint) (tptr tschar))
                            tschar) (Econst_int (Int.repr 117) tint))
                        (Ssequence
                          (Sassign
                            (Ederef
                              (Ebinop Oadd
                                (Evar _filename (tarray tschar 50))
                                (Econst_int (Int.repr 5) tint) (tptr tschar))
                              tschar) (Econst_int (Int.repr 116) tint))
                          (Ssequence
                            (Sassign
                              (Ederef
                                (Ebinop Oadd
                                  (Evar _filename (tarray tschar 50))
                                  (Econst_int (Int.repr 6) tint)
                                  (tptr tschar)) tschar)
                              (Econst_int (Int.repr 46) tint))
                            (Ssequence
                              (Sassign
                                (Ederef
                                  (Ebinop Oadd
                                    (Evar _filename (tarray tschar 50))
                                    (Econst_int (Int.repr 7) tint)
                                    (tptr tschar)) tschar)
                                (Econst_int (Int.repr 100) tint))
                              (Ssequence
                                (Sassign
                                  (Ederef
                                    (Ebinop Oadd
                                      (Evar _filename (tarray tschar 50))
                                      (Econst_int (Int.repr 8) tint)
                                      (tptr tschar)) tschar)
                                  (Econst_int (Int.repr 97) tint))
                                (Ssequence
                                  (Sassign
                                    (Ederef
                                      (Ebinop Oadd
                                        (Evar _filename (tarray tschar 50))
                                        (Econst_int (Int.repr 9) tint)
                                        (tptr tschar)) tschar)
                                    (Econst_int (Int.repr 116) tint))
                                  (Ssequence
                                    (Sassign
                                      (Ederef
                                        (Ebinop Oadd
                                          (Evar _filename (tarray tschar 50))
                                          (Econst_int (Int.repr 10) tint)
                                          (tptr tschar)) tschar)
                                      (Econst_int (Int.repr 0) tint))
                                    (Ssequence
                                      (Sassign
                                        (Ederef
                                          (Ebinop Oadd
                                            (Evar _filename (tarray tschar 50))
                                            (Econst_int (Int.repr 11) tint)
                                            (tptr tschar)) tschar)
                                        (Econst_int (Int.repr 0) tint))
                                      (Ssequence
                                        (Sassign
                                          (Ederef
                                            (Ebinop Oadd
                                              (Evar _filename (tarray tschar 50))
                                              (Econst_int (Int.repr 12) tint)
                                              (tptr tschar)) tschar)
                                          (Econst_int (Int.repr 0) tint))
                                        (Ssequence
                                          (Sassign
                                            (Ederef
                                              (Ebinop Oadd
                                                (Evar _filename (tarray tschar 50))
                                                (Econst_int (Int.repr 13) tint)
                                                (tptr tschar)) tschar)
                                            (Econst_int (Int.repr 0) tint))
                                          (Ssequence
                                            (Sassign
                                              (Ederef
                                                (Ebinop Oadd
                                                  (Evar _filename (tarray tschar 50))
                                                  (Econst_int (Int.repr 14) tint)
                                                  (tptr tschar)) tschar)
                                              (Econst_int (Int.repr 0) tint))
                                            (Ssequence
                                              (Sassign
                                                (Ederef
                                                  (Ebinop Oadd
                                                    (Evar _filename (tarray tschar 50))
                                                    (Econst_int (Int.repr 15) tint)
                                                    (tptr tschar)) tschar)
                                                (Econst_int (Int.repr 0) tint))
                                              (Ssequence
                                                (Sassign
                                                  (Ederef
                                                    (Ebinop Oadd
                                                      (Evar _filename (tarray tschar 50))
                                                      (Econst_int (Int.repr 16) tint)
                                                      (tptr tschar)) tschar)
                                                  (Econst_int (Int.repr 0) tint))
                                                (Ssequence
                                                  (Sassign
                                                    (Ederef
                                                      (Ebinop Oadd
                                                        (Evar _filename (tarray tschar 50))
                                                        (Econst_int (Int.repr 17) tint)
                                                        (tptr tschar))
                                                      tschar)
                                                    (Econst_int (Int.repr 0) tint))
                                                  (Ssequence
                                                    (Sassign
                                                      (Ederef
                                                        (Ebinop Oadd
                                                          (Evar _filename (tarray tschar 50))
                                                          (Econst_int (Int.repr 18) tint)
                                                          (tptr tschar))
                                                        tschar)
                                                      (Econst_int (Int.repr 0) tint))
                                                    (Ssequence
                                                      (Sassign
                                                        (Ederef
                                                          (Ebinop Oadd
                                                            (Evar _filename (tarray tschar 50))
                                                            (Econst_int (Int.repr 19) tint)
                                                            (tptr tschar))
                                                          tschar)
                                                        (Econst_int (Int.repr 0) tint))
                                                      (Ssequence
                                                        (Sassign
                                                          (Ederef
                                                            (Ebinop Oadd
                                                              (Evar _filename (tarray tschar 50))
                                                              (Econst_int (Int.repr 20) tint)
                                                              (tptr tschar))
                                                            tschar)
                                                          (Econst_int (Int.repr 0) tint))
                                                        (Ssequence
                                                          (Sassign
                                                            (Ederef
                                                              (Ebinop Oadd
                                                                (Evar _filename (tarray tschar 50))
                                                                (Econst_int (Int.repr 21) tint)
                                                                (tptr tschar))
                                                              tschar)
                                                            (Econst_int (Int.repr 0) tint))
                                                          (Ssequence
                                                            (Sassign
                                                              (Ederef
                                                                (Ebinop Oadd
                                                                  (Evar _filename (tarray tschar 50))
                                                                  (Econst_int (Int.repr 22) tint)
                                                                  (tptr tschar))
                                                                tschar)
                                                              (Econst_int (Int.repr 0) tint))
                                                            (Ssequence
                                                              (Sassign
                                                                (Ederef
                                                                  (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 23) tint)
                                                                    (tptr tschar))
                                                                  tschar)
                                                                (Econst_int (Int.repr 0) tint))
                                                              (Ssequence
                                                                (Sassign
                                                                  (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 24) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                  (Econst_int (Int.repr 0) tint))
                                                                (Ssequence
                                                                  (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 25) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                  (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 26) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 27) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 28) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 29) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 30) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 31) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 32) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 33) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 34) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 35) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 36) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 37) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 38) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 39) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 40) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 41) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 42) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 43) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 44) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 45) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 46) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 47) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 48) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Sassign
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Evar _filename (tarray tschar 50))
                                                                    (Econst_int (Int.repr 49) tint)
                                                                    (tptr tschar))
                                                                    tschar)
                                                                    (Econst_int (Int.repr 0) tint))
                                                                    (Ssequence
                                                                    (Ssequence
                                                                    (Scall (Some _t'2)
                                                                    (Evar _fopen 
                                                                    (Tfunction
                                                                    (Tcons
                                                                    (tptr tschar)
                                                                    (Tcons
                                                                    (tptr tschar)
                                                                    Tnil))
                                                                    (tptr (Tstruct __IO_FILE noattr))
                                                                    cc_default))
                                                                    ((Evar _filename (tarray tschar 50)) ::
                                                                    (Evar ___stringlit_1 (tarray tschar 2)) ::
                                                                    nil))
                                                                    (Sset _f
                                                                    (Etempvar _t'2 (tptr (Tstruct __IO_FILE noattr)))))
                                                                    (Ssequence
                                                                    (Sifthenelse 
                                                                    (Eunop Onotbool
                                                                    (Etempvar _f (tptr (Tstruct __IO_FILE noattr)))
                                                                    tint)
                                                                    (Scall None
                                                                    (Evar _exit 
                                                                    (Tfunction
                                                                    (Tcons
                                                                    tint
                                                                    Tnil)
                                                                    tvoid
                                                                    cc_default))
                                                                    ((Econst_int (Int.repr 2) tint) ::
                                                                    nil))
                                                                    Sskip)
                                                                    (Ssequence
                                                                    (Ssequence
                                                                    (Sset _cell
                                                                    (Efield
                                                                    (Ederef
                                                                    (Etempvar _states (tptr (Tstruct _s_state_list noattr)))
                                                                    (Tstruct _s_state_list noattr))
                                                                    _first
                                                                    (tptr (Tstruct _s_state_list_cell noattr))))
                                                                    (Sloop
                                                                    (Ssequence
                                                                    (Sifthenelse (Etempvar _cell (tptr (Tstruct _s_state_list_cell noattr)))
                                                                    Sskip
                                                                    Sbreak)
                                                                    (Ssequence
                                                                    (Sset _time
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Efield
                                                                    (Efield
                                                                    (Ederef
                                                                    (Etempvar _cell (tptr (Tstruct _s_state_list_cell noattr)))
                                                                    (Tstruct _s_state_list_cell noattr))
                                                                    _data
                                                                    (Tstruct _s_state noattr))
                                                                    _item
                                                                    (tarray tdouble 3))
                                                                    (Econst_int (Int.repr 0) tint)
                                                                    (tptr tdouble))
                                                                    tdouble))
                                                                    (Ssequence
                                                                    (Sset _x
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Efield
                                                                    (Efield
                                                                    (Ederef
                                                                    (Etempvar _cell (tptr (Tstruct _s_state_list_cell noattr)))
                                                                    (Tstruct _s_state_list_cell noattr))
                                                                    _data
                                                                    (Tstruct _s_state noattr))
                                                                    _item
                                                                    (tarray tdouble 3))
                                                                    (Econst_int (Int.repr 1) tint)
                                                                    (tptr tdouble))
                                                                    tdouble))
                                                                    (Ssequence
                                                                    (Sset _xd
                                                                    (Ederef
                                                                    (Ebinop Oadd
                                                                    (Efield
                                                                    (Efield
                                                                    (Ederef
                                                                    (Etempvar _cell (tptr (Tstruct _s_state_list_cell noattr)))
                                                                    (Tstruct _s_state_list_cell noattr))
                                                                    _data
                                                                    (Tstruct _s_state noattr))
                                                                    _item
                                                                    (tarray tdouble 3))
                                                                    (Econst_int (Int.repr 2) tint)
                                                                    (tptr tdouble))
                                                                    tdouble))
                                                                    (Ssequence
                                                                    (Scall None
                                                                    (Evar _printf 
                                                                    (Tfunction
                                                                    (Tcons
                                                                    (tptr tschar)
                                                                    Tnil)
                                                                    tint
                                                                    {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
                                                                    ((Evar ___stringlit_2 (tarray tschar 23)) ::
                                                                    (Etempvar _time tdouble) ::
                                                                    (Etempvar _x tdouble) ::
                                                                    (Etempvar _xd tdouble) ::
                                                                    nil))
                                                                    (Scall None
                                                                    (Evar _fprintf 
                                                                    (Tfunction
                                                                    (Tcons
                                                                    (tptr (Tstruct __IO_FILE noattr))
                                                                    (Tcons
                                                                    (tptr tschar)
                                                                    Tnil))
                                                                    tint
                                                                    {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
                                                                    ((Etempvar _f (tptr (Tstruct __IO_FILE noattr))) ::
                                                                    (Evar ___stringlit_2 (tarray tschar 23)) ::
                                                                    (Etempvar _time tdouble) ::
                                                                    (Etempvar _x tdouble) ::
                                                                    (Etempvar _xd tdouble) ::
                                                                    nil)))))))
                                                                    (Sset _cell
                                                                    (Efield
                                                                    (Ederef
                                                                    (Etempvar _cell (tptr (Tstruct _s_state_list_cell noattr)))
                                                                    (Tstruct _s_state_list_cell noattr))
                                                                    _next
                                                                    (tptr (Tstruct _s_state_list_cell noattr))))))
                                                                    (Ssequence
                                                                    (Scall None
                                                                    (Evar _fclose 
                                                                    (Tfunction
                                                                    (Tcons
                                                                    (tptr (Tstruct __IO_FILE noattr))
                                                                    Tnil)
                                                                    tint
                                                                    cc_default))
                                                                    ((Etempvar _f (tptr (Tstruct __IO_FILE noattr))) ::
                                                                    nil))
                                                                    (Sreturn (Some (Econst_int (Int.repr 0) tint)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  (Sreturn (Some (Econst_int (Int.repr 0) tint))))
|}.

Definition composites : list composite_definition :=
(Composite __IO_marker Struct
   ((__next, (tptr (Tstruct __IO_marker noattr))) ::
    (__sbuf, (tptr (Tstruct __IO_FILE noattr))) :: (__pos, tint) :: nil)
   noattr ::
 Composite __IO_FILE Struct
   ((__flags, tint) :: (__IO_read_ptr, (tptr tschar)) ::
    (__IO_read_end, (tptr tschar)) :: (__IO_read_base, (tptr tschar)) ::
    (__IO_write_base, (tptr tschar)) :: (__IO_write_ptr, (tptr tschar)) ::
    (__IO_write_end, (tptr tschar)) :: (__IO_buf_base, (tptr tschar)) ::
    (__IO_buf_end, (tptr tschar)) :: (__IO_save_base, (tptr tschar)) ::
    (__IO_backup_base, (tptr tschar)) :: (__IO_save_end, (tptr tschar)) ::
    (__markers, (tptr (Tstruct __IO_marker noattr))) ::
    (__chain, (tptr (Tstruct __IO_FILE noattr))) :: (__fileno, tint) ::
    (__flags2, tint) :: (__old_offset, tlong) :: (__cur_column, tushort) ::
    (__vtable_offset, tschar) :: (__shortbuf, (tarray tschar 1)) ::
    (__lock, (tptr tvoid)) :: (__offset, tlong) :: (___pad1, (tptr tvoid)) ::
    (___pad2, (tptr tvoid)) :: (___pad3, (tptr tvoid)) ::
    (___pad4, (tptr tvoid)) :: (___pad5, tulong) :: (__mode, tint) ::
    (__unused2, (tarray tschar 20)) :: nil)
   noattr ::
 Composite _s_state Struct ((_item, (tarray tdouble 3)) :: nil) noattr ::
 Composite _s_state_list_cell Struct
   ((_data, (Tstruct _s_state noattr)) ::
    (_next, (tptr (Tstruct _s_state_list_cell noattr))) :: nil)
   noattr ::
 Composite _s_state_list Struct
   ((_first, (tptr (Tstruct _s_state_list_cell noattr))) ::
    (_last, (tptr (Tstruct _s_state_list_cell noattr))) :: nil)
   noattr :: nil).

Definition global_definitions : list (ident * globdef fundef type) :=
((___stringlit_2, Gvar v___stringlit_2) ::
 (___stringlit_1, Gvar v___stringlit_1) ::
 (___builtin_ais_annot,
   Gfun(External (EF_builtin "__builtin_ais_annot"
                   (mksignature (AST.Tint :: nil) None
                     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
     (Tcons (tptr tschar) Tnil) tvoid
     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|})) ::
 (___builtin_bswap,
   Gfun(External (EF_builtin "__builtin_bswap"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons tuint Tnil) tuint cc_default)) ::
 (___builtin_bswap32,
   Gfun(External (EF_builtin "__builtin_bswap32"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons tuint Tnil) tuint cc_default)) ::
 (___builtin_bswap16,
   Gfun(External (EF_builtin "__builtin_bswap16"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons tushort Tnil) tushort cc_default)) ::
 (___builtin_fabs,
   Gfun(External (EF_builtin "__builtin_fabs"
                   (mksignature (AST.Tfloat :: nil) (Some AST.Tfloat)
                     cc_default)) (Tcons tdouble Tnil) tdouble cc_default)) ::
 (___builtin_fsqrt,
   Gfun(External (EF_builtin "__builtin_fsqrt"
                   (mksignature (AST.Tfloat :: nil) (Some AST.Tfloat)
                     cc_default)) (Tcons tdouble Tnil) tdouble cc_default)) ::
 (___builtin_memcpy_aligned,
   Gfun(External (EF_builtin "__builtin_memcpy_aligned"
                   (mksignature
                     (AST.Tint :: AST.Tint :: AST.Tlong :: AST.Tlong :: nil)
                     None cc_default))
     (Tcons (tptr tvoid)
       (Tcons (tptr tvoid) (Tcons tulong (Tcons tulong Tnil)))) tvoid
     cc_default)) ::
 (___builtin_annot,
   Gfun(External (EF_builtin "__builtin_annot"
                   (mksignature (AST.Tint :: nil) None
                     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
     (Tcons (tptr tschar) Tnil) tvoid
     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|})) ::
 (___builtin_annot_intval,
   Gfun(External (EF_builtin "__builtin_annot_intval"
                   (mksignature (AST.Tint :: AST.Tint :: nil) (Some AST.Tint)
                     cc_default)) (Tcons (tptr tschar) (Tcons tint Tnil))
     tint cc_default)) ::
 (___builtin_membar,
   Gfun(External (EF_builtin "__builtin_membar"
                   (mksignature nil None cc_default)) Tnil tvoid cc_default)) ::
 (___builtin_va_start,
   Gfun(External (EF_builtin "__builtin_va_start"
                   (mksignature (AST.Tint :: nil) None cc_default))
     (Tcons (tptr tvoid) Tnil) tvoid cc_default)) ::
 (___builtin_va_arg,
   Gfun(External (EF_builtin "__builtin_va_arg"
                   (mksignature (AST.Tint :: AST.Tint :: nil) None
                     cc_default)) (Tcons (tptr tvoid) (Tcons tuint Tnil))
     tvoid cc_default)) ::
 (___builtin_va_copy,
   Gfun(External (EF_builtin "__builtin_va_copy"
                   (mksignature (AST.Tint :: AST.Tint :: nil) None
                     cc_default))
     (Tcons (tptr tvoid) (Tcons (tptr tvoid) Tnil)) tvoid cc_default)) ::
 (___builtin_va_end,
   Gfun(External (EF_builtin "__builtin_va_end"
                   (mksignature (AST.Tint :: nil) None cc_default))
     (Tcons (tptr tvoid) Tnil) tvoid cc_default)) ::
 (___compcert_va_int32,
   Gfun(External (EF_external "__compcert_va_int32"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons (tptr tvoid) Tnil) tuint cc_default)) ::
 (___compcert_va_int64,
   Gfun(External (EF_external "__compcert_va_int64"
                   (mksignature (AST.Tint :: nil) (Some AST.Tlong)
                     cc_default)) (Tcons (tptr tvoid) Tnil) tulong
     cc_default)) ::
 (___compcert_va_float64,
   Gfun(External (EF_external "__compcert_va_float64"
                   (mksignature (AST.Tint :: nil) (Some AST.Tfloat)
                     cc_default)) (Tcons (tptr tvoid) Tnil) tdouble
     cc_default)) ::
 (___compcert_va_composite,
   Gfun(External (EF_external "__compcert_va_composite"
                   (mksignature (AST.Tint :: AST.Tlong :: nil)
                     (Some AST.Tint) cc_default))
     (Tcons (tptr tvoid) (Tcons tulong Tnil)) (tptr tvoid) cc_default)) ::
 (___compcert_i64_dtos,
   Gfun(External (EF_runtime "__compcert_i64_dtos"
                   (mksignature (AST.Tfloat :: nil) (Some AST.Tlong)
                     cc_default)) (Tcons tdouble Tnil) tlong cc_default)) ::
 (___compcert_i64_dtou,
   Gfun(External (EF_runtime "__compcert_i64_dtou"
                   (mksignature (AST.Tfloat :: nil) (Some AST.Tlong)
                     cc_default)) (Tcons tdouble Tnil) tulong cc_default)) ::
 (___compcert_i64_stod,
   Gfun(External (EF_runtime "__compcert_i64_stod"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tfloat)
                     cc_default)) (Tcons tlong Tnil) tdouble cc_default)) ::
 (___compcert_i64_utod,
   Gfun(External (EF_runtime "__compcert_i64_utod"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tfloat)
                     cc_default)) (Tcons tulong Tnil) tdouble cc_default)) ::
 (___compcert_i64_stof,
   Gfun(External (EF_runtime "__compcert_i64_stof"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tsingle)
                     cc_default)) (Tcons tlong Tnil) tfloat cc_default)) ::
 (___compcert_i64_utof,
   Gfun(External (EF_runtime "__compcert_i64_utof"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tsingle)
                     cc_default)) (Tcons tulong Tnil) tfloat cc_default)) ::
 (___compcert_i64_sdiv,
   Gfun(External (EF_runtime "__compcert_i64_sdiv"
                   (mksignature (AST.Tlong :: AST.Tlong :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tlong (Tcons tlong Tnil)) tlong cc_default)) ::
 (___compcert_i64_udiv,
   Gfun(External (EF_runtime "__compcert_i64_udiv"
                   (mksignature (AST.Tlong :: AST.Tlong :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tulong (Tcons tulong Tnil)) tulong cc_default)) ::
 (___compcert_i64_smod,
   Gfun(External (EF_runtime "__compcert_i64_smod"
                   (mksignature (AST.Tlong :: AST.Tlong :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tlong (Tcons tlong Tnil)) tlong cc_default)) ::
 (___compcert_i64_umod,
   Gfun(External (EF_runtime "__compcert_i64_umod"
                   (mksignature (AST.Tlong :: AST.Tlong :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tulong (Tcons tulong Tnil)) tulong cc_default)) ::
 (___compcert_i64_shl,
   Gfun(External (EF_runtime "__compcert_i64_shl"
                   (mksignature (AST.Tlong :: AST.Tint :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tlong (Tcons tint Tnil)) tlong cc_default)) ::
 (___compcert_i64_shr,
   Gfun(External (EF_runtime "__compcert_i64_shr"
                   (mksignature (AST.Tlong :: AST.Tint :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tulong (Tcons tint Tnil)) tulong cc_default)) ::
 (___compcert_i64_sar,
   Gfun(External (EF_runtime "__compcert_i64_sar"
                   (mksignature (AST.Tlong :: AST.Tint :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tlong (Tcons tint Tnil)) tlong cc_default)) ::
 (___compcert_i64_smulh,
   Gfun(External (EF_runtime "__compcert_i64_smulh"
                   (mksignature (AST.Tlong :: AST.Tlong :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tlong (Tcons tlong Tnil)) tlong cc_default)) ::
 (___compcert_i64_umulh,
   Gfun(External (EF_runtime "__compcert_i64_umulh"
                   (mksignature (AST.Tlong :: AST.Tlong :: nil)
                     (Some AST.Tlong) cc_default))
     (Tcons tulong (Tcons tulong Tnil)) tulong cc_default)) ::
 (___builtin_bswap64,
   Gfun(External (EF_builtin "__builtin_bswap64"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tlong)
                     cc_default)) (Tcons tulong Tnil) tulong cc_default)) ::
 (___builtin_clz,
   Gfun(External (EF_builtin "__builtin_clz"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons tuint Tnil) tint cc_default)) ::
 (___builtin_clzl,
   Gfun(External (EF_builtin "__builtin_clzl"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tint)
                     cc_default)) (Tcons tulong Tnil) tint cc_default)) ::
 (___builtin_clzll,
   Gfun(External (EF_builtin "__builtin_clzll"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tint)
                     cc_default)) (Tcons tulong Tnil) tint cc_default)) ::
 (___builtin_ctz,
   Gfun(External (EF_builtin "__builtin_ctz"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons tuint Tnil) tint cc_default)) ::
 (___builtin_ctzl,
   Gfun(External (EF_builtin "__builtin_ctzl"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tint)
                     cc_default)) (Tcons tulong Tnil) tint cc_default)) ::
 (___builtin_ctzll,
   Gfun(External (EF_builtin "__builtin_ctzll"
                   (mksignature (AST.Tlong :: nil) (Some AST.Tint)
                     cc_default)) (Tcons tulong Tnil) tint cc_default)) ::
 (___builtin_fmax,
   Gfun(External (EF_builtin "__builtin_fmax"
                   (mksignature (AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tfloat) cc_default))
     (Tcons tdouble (Tcons tdouble Tnil)) tdouble cc_default)) ::
 (___builtin_fmin,
   Gfun(External (EF_builtin "__builtin_fmin"
                   (mksignature (AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tfloat) cc_default))
     (Tcons tdouble (Tcons tdouble Tnil)) tdouble cc_default)) ::
 (___builtin_fmadd,
   Gfun(External (EF_builtin "__builtin_fmadd"
                   (mksignature
                     (AST.Tfloat :: AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tfloat) cc_default))
     (Tcons tdouble (Tcons tdouble (Tcons tdouble Tnil))) tdouble
     cc_default)) ::
 (___builtin_fmsub,
   Gfun(External (EF_builtin "__builtin_fmsub"
                   (mksignature
                     (AST.Tfloat :: AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tfloat) cc_default))
     (Tcons tdouble (Tcons tdouble (Tcons tdouble Tnil))) tdouble
     cc_default)) ::
 (___builtin_fnmadd,
   Gfun(External (EF_builtin "__builtin_fnmadd"
                   (mksignature
                     (AST.Tfloat :: AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tfloat) cc_default))
     (Tcons tdouble (Tcons tdouble (Tcons tdouble Tnil))) tdouble
     cc_default)) ::
 (___builtin_fnmsub,
   Gfun(External (EF_builtin "__builtin_fnmsub"
                   (mksignature
                     (AST.Tfloat :: AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tfloat) cc_default))
     (Tcons tdouble (Tcons tdouble (Tcons tdouble Tnil))) tdouble
     cc_default)) ::
 (___builtin_read16_reversed,
   Gfun(External (EF_builtin "__builtin_read16_reversed"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons (tptr tushort) Tnil) tushort cc_default)) ::
 (___builtin_read32_reversed,
   Gfun(External (EF_builtin "__builtin_read32_reversed"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons (tptr tuint) Tnil) tuint cc_default)) ::
 (___builtin_write16_reversed,
   Gfun(External (EF_builtin "__builtin_write16_reversed"
                   (mksignature (AST.Tint :: AST.Tint :: nil) None
                     cc_default)) (Tcons (tptr tushort) (Tcons tushort Tnil))
     tvoid cc_default)) ::
 (___builtin_write32_reversed,
   Gfun(External (EF_builtin "__builtin_write32_reversed"
                   (mksignature (AST.Tint :: AST.Tint :: nil) None
                     cc_default)) (Tcons (tptr tuint) (Tcons tuint Tnil))
     tvoid cc_default)) ::
 (___builtin_nop,
   Gfun(External (EF_builtin "__builtin_nop"
                   (mksignature nil None cc_default)) Tnil tvoid cc_default)) ::
 (___builtin_debug,
   Gfun(External (EF_external "__builtin_debug"
                   (mksignature (AST.Tint :: nil) None
                     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
     (Tcons tint Tnil) tvoid
     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|})) ::
 (_fclose,
   Gfun(External (EF_external "fclose"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint) cc_default))
     (Tcons (tptr (Tstruct __IO_FILE noattr)) Tnil) tint cc_default)) ::
 (_fopen,
   Gfun(External (EF_external "fopen"
                   (mksignature (AST.Tint :: AST.Tint :: nil) (Some AST.Tint)
                     cc_default))
     (Tcons (tptr tschar) (Tcons (tptr tschar) Tnil))
     (tptr (Tstruct __IO_FILE noattr)) cc_default)) ::
 (_fprintf,
   Gfun(External (EF_external "fprintf"
                   (mksignature (AST.Tint :: AST.Tint :: nil) (Some AST.Tint)
                     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
     (Tcons (tptr (Tstruct __IO_FILE noattr)) (Tcons (tptr tschar) Tnil))
     tint {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|})) ::
 (_printf,
   Gfun(External (EF_external "printf"
                   (mksignature (AST.Tint :: nil) (Some AST.Tint)
                     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|}))
     (Tcons (tptr tschar) Tnil) tint
     {|cc_vararg:=true; cc_unproto:=false; cc_structret:=false|})) ::
 (_run_sim,
   Gfun(External (EF_external "run_sim"
                   (mksignature (AST.Tint :: AST.Tfloat :: AST.Tfloat :: nil)
                     (Some AST.Tint) cc_default))
     (Tcons (Tstruct _s_state noattr) (Tcons tdouble (Tcons tdouble Tnil)))
     (tptr (Tstruct _s_state_list noattr)) cc_default)) ::
 (_exit,
   Gfun(External (EF_external "exit"
                   (mksignature (AST.Tint :: nil) None cc_default))
     (Tcons tint Tnil) tvoid cc_default)) ::
 (_main, Gfun(Internal f_main)) :: nil).

Definition public_idents : list ident :=
(_main :: _exit :: _run_sim :: _printf :: _fprintf :: _fopen :: _fclose ::
 ___builtin_debug :: ___builtin_nop :: ___builtin_write32_reversed ::
 ___builtin_write16_reversed :: ___builtin_read32_reversed ::
 ___builtin_read16_reversed :: ___builtin_fnmsub :: ___builtin_fnmadd ::
 ___builtin_fmsub :: ___builtin_fmadd :: ___builtin_fmin ::
 ___builtin_fmax :: ___builtin_ctzll :: ___builtin_ctzl :: ___builtin_ctz ::
 ___builtin_clzll :: ___builtin_clzl :: ___builtin_clz ::
 ___builtin_bswap64 :: ___compcert_i64_umulh :: ___compcert_i64_smulh ::
 ___compcert_i64_sar :: ___compcert_i64_shr :: ___compcert_i64_shl ::
 ___compcert_i64_umod :: ___compcert_i64_smod :: ___compcert_i64_udiv ::
 ___compcert_i64_sdiv :: ___compcert_i64_utof :: ___compcert_i64_stof ::
 ___compcert_i64_utod :: ___compcert_i64_stod :: ___compcert_i64_dtou ::
 ___compcert_i64_dtos :: ___compcert_va_composite ::
 ___compcert_va_float64 :: ___compcert_va_int64 :: ___compcert_va_int32 ::
 ___builtin_va_end :: ___builtin_va_copy :: ___builtin_va_arg ::
 ___builtin_va_start :: ___builtin_membar :: ___builtin_annot_intval ::
 ___builtin_annot :: ___builtin_memcpy_aligned :: ___builtin_fsqrt ::
 ___builtin_fabs :: ___builtin_bswap16 :: ___builtin_bswap32 ::
 ___builtin_bswap :: ___builtin_ais_annot :: nil).

Definition prog : Clight.program := 
  mkprogram composites global_definitions public_idents _main Logic.I.


