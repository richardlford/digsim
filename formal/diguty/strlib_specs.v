Require Import VST.floyd.proofauto.
(*
  We define versions of the specs that take the identifiers arguments so that
  We can be imported into different separate compilation units. To use, add
  definitions like these:

Definition strchr_spec := strchr_spec' _strchr _str _c.
Definition strcat_spec := strcat_spec' _strcat _dest _src.
Definition strcmp_spec := strcmp_spec' _strcmp _str1 _str2.
Definition strcpy_spec := strcpy_spec' _strcpy _dest _src.
Definition strlen_spec := strlen_spec' _strlen _str.

 *)

Definition strchr_spec' {CSX: compspecs} (_strchr _str _c: positive) :=
 DECLARE _strchr
  WITH sh: share, str : val, s : list byte, c : byte
  PRE  [ _str OF tptr tschar, _c OF tint ]
    PROP (readable_share sh; c <> Byte.zero)
    LOCAL (temp _str str; temp _c (Vbyte c))
    SEP (cstring sh s str)
  POST [ tptr tschar ]
   EX r : val,
    PROP ((exists i, Znth i s = c /\ Forall (fun d => d<>c) (sublist 0 i s)
                     /\ r = offset_val i str)
       \/ (Forall (fun d => d<>c) s /\ r = nullval))
    LOCAL (temp ret_temp r)
    SEP (cstring sh s str).


Definition strcat_spec' {CSX: compspecs} (_strcat _dest _src: positive) :=
 DECLARE _strcat
  WITH sh: share, sh': share, dest : val, sd : list byte, n : Z, src : val, ss : list byte
  PRE  [ _dest OF tptr tschar, _src OF tptr tschar ]
    PROP (writable_share sh; readable_share sh'; Zlength sd + Zlength ss < n)
    LOCAL (temp _dest dest; temp _src src)
    SEP (cstringn sh sd n dest; cstring sh' ss src)
  POST [ tptr tschar ]
    PROP ()
    LOCAL (temp ret_temp dest)
    SEP (cstringn sh (sd ++ ss) n dest; cstring sh' ss src).

Definition strcmp_spec' {CSX: compspecs} (_strcmp _str1 _str2: positive) :=
 DECLARE _strcmp
  WITH sh1: share, sh2: share, str1 : val, s1 : list byte, str2 : val, s2 : list byte
  PRE [ _str1 OF tptr tschar, _str2 OF tptr tschar ]
    PROP (readable_share sh1; readable_share sh2)
    LOCAL (temp _str1 str1; temp _str2 str2)
    SEP (cstring sh1 s1 str1; cstring sh2 s2 str2)
  POST [ tint ]
   EX i : int,
    PROP (if Int.eq_dec i Int.zero then s1 = s2 else s1 <> s2)
    LOCAL (temp ret_temp (Vint i))
    SEP (cstring sh1 s1 str1; cstring sh2 s2 str2).

Definition strcpy_spec' {CSX: compspecs} (_strcpy _dest _src: positive) :=
 DECLARE _strcpy
  WITH sh: share, sh': share, dest : val, n : Z, src : val, s : list byte
  PRE [ _dest OF tptr tschar, _src OF tptr tschar ]
    PROP (writable_share sh; readable_share sh'; Zlength s < n)
    LOCAL (temp _dest dest; temp _src src)
    SEP (data_at_ sh (tarray tschar n) dest; cstring sh' s src)
  POST [ tptr tschar ]
    PROP ()
    LOCAL (temp ret_temp dest)
    SEP (cstringn sh s n dest; cstring sh' s src).

Definition strlen_spec' {CSX: compspecs} (_strlen _str: positive) :=
 DECLARE _strlen
  WITH sh: share, s : list byte, str: val
  PRE [ _str OF tptr tschar ]
    PROP (readable_share sh)
    LOCAL (temp _str str)
    SEP (cstring sh s str)
  POST [ tulong ]
    PROP ()
    LOCAL (temp ret_temp (Vptrofs (Ptrofs.repr (Zlength s))))
    SEP (cstring sh s str).
