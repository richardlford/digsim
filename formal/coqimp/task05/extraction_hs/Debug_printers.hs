module Debug_printers where

import qualified Prelude
import qualified Floats
import qualified Float_text_io

_DebugIO__width :: Prelude.Integer
_DebugIO__width =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    0)))))))))))))

_DebugIO__fdigs :: Prelude.Integer
_DebugIO__fdigs =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ 0))))))))

_DebugIO__print_float :: Floats.Coq_float -> Prelude.String
_DebugIO__print_float =
  Float_text_io._FloatIO__float_to_string _DebugIO__width _DebugIO__fdigs

_DebugIO__print_Z :: Prelude.Integer -> Prelude.String
_DebugIO__print_Z =
  Float_text_io._FloatIO__coq_Z_to_string_base10 (Prelude.succ 0)

