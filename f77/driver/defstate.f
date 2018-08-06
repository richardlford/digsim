C
        Subroutine Define_Real_State(State_Index,State_Deriv_Index)
C
C       This routine adds an integration to the REAL integrator list.
C
C       Inputs:
C               State_Index         - Location of state in Real_Array [Integer]
C               State_Deriv_Index   - Location of state derivative in Real_Array [Integer]
C
C       Outputs:
C               IxDot_Real - Real_Array locations of state derivatives [Integer Array]
C               Ix_Real    - Real_Array locations of states [Integer Array]
C               Ndes_Real  - Number of real states [Integer]
C
C       Internal variables and constants:
C               None.
C
        Include 'deriv.inc'
        Integer State_Index, State_Deriv_Index
C
        Ndes_Real             = Ndes_Real + 1
        Ix_Real(Ndes_Real)    = State_Index
        IxDot_Real(Ndes_Real) = State_Deriv_Index
C
        Return
        End
