C
      Subroutine Actuator_Data
C
C     Fin actuators model default data
C
C     Inputs:
C       None.
C
C     Outputs:
C       Fin_Limit_dg - Maximum allowable fin deflection [deg]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (804), Fin_Limit_dg)
C
C     Begin default data definition:
C
      Fin_Limit_dg = 20.0e0
C
      Return
      End

      Subroutine Actuator_Init
C
C     Fin actuators model initialization
C
C     Inputs:
C       Fin_Limit_dg - Maximum allowable fin deflection [deg]
C
C     Outputs:
C       Fin_Limit - Maximum allowable fin deflection [rad]
C
C     Internal variables and constants:
C       RDTODG - Radians to degrees conversion factor [deg/rad]
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array ( 51), RDTODG)
      Equivalence (Real_Array (804), Fin_Limit_dg)
      Equivalence (Real_Array (805), Fin_Limit)
C
C     Begin math model initialization:
C
C     Convert from input units to simulation units
C
      Fin_Limit = Fin_Limit_dg/RDTODG
C
      Return
      End

      Subroutine Actuator
C
C     This model simulates four perfect fin actuators, with position limits.
C
C     Inputs:
C       Fin_1_Cmd - Fin position commands [rad]
C       Fin_2_Cmd
C       Fin_3_Cmd
C       Fin_4_Cmd
C       Fin_Limit - Maximum allowable fin deflection [rad]
C
C     Outputs:
C       Fin_1_Position - Fin positions [rad]
C       Fin_2_Position
C       Fin_3_Position
C       Fin_4_Position
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations:
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (800), Fin_1_Position)
      Equivalence (Real_Array (801), Fin_2_Position)
      Equivalence (Real_Array (802), Fin_3_Position)
      Equivalence (Real_Array (803), Fin_4_Position)
      Equivalence (Real_Array (805), Fin_Limit)
      Equivalence (Real_Array (300), Fin_1_Cmd)
      Equivalence (Real_Array (301), Fin_2_Cmd)
      Equivalence (Real_Array (302), Fin_3_Cmd)
      Equivalence (Real_Array (303), Fin_4_Cmd)
C
C     Perfect actuator response to position commands
C
      Fin_1_Position = Limit (Fin_1_Cmd, -Fin_Limit, +Fin_Limit)
      Fin_2_Position = Limit (Fin_2_Cmd, -Fin_Limit, +Fin_Limit)
      Fin_3_Position = Limit (Fin_3_Cmd, -Fin_Limit, +Fin_Limit)
      Fin_4_Position = Limit (Fin_4_Cmd, -Fin_Limit, +Fin_Limit)
C
      Return
      End
