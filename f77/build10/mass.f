C
      Subroutine Mass_Data
C
C     This subroutine sets the default data for the missile mass characteristics.
C
C     Inputs:
C       None.
C
C     Outputs:
C       Ixx_B  - Diagonal elements of the inertia tensor for the missile [Kg*m**2]
C       Iyy_B
C       Izz_B
C       Mass_B - Missile mass [Kg]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (400), Ixx_B)
      Equivalence (Real_Array (401), Iyy_B)
      Equivalence (Real_Array (402), Izz_B)
      Equivalence (Real_Array (403), Mass_B)
C
C     Begin default data definition:
C
      Ixx_B  =  0.10e0
      Iyy_B  =  0.75e0
      Izz_B  =  0.75e0
      Mass_B = 10.0e0
C
      Return
      End

      Subroutine Mass_Init
C
C     This subroutine initializes the variables the missile mass characteristics.
C
C     Inputs:
C       None.
C
C     Outputs:
C       None.
C
C     Internal variables and constants:
C       None.
C
C     Begin math model initialization:
C
C     No initialization required.
C
      Return
      End

      Subroutine Mass
C
C     This subroutine processes the dynamic missile mass characteristics.
C
C     Inputs:
C       None.
C
C     Outputs:
C       None.
C
C     Internal variables and constants:
C       None.
C
C     Begin math model:
C
C     No dynamic processing required.
C
      Return
      End
