C
      Subroutine Seeker_Data
C
C     Seeker model default data
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
C     Begin default data initialization:
C
C     No default data required.
C
      Return
      End

      Subroutine Seeker_Init
C
C     Seeker model initialization
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

      Subroutine Seeker
C
C     Perfect seeker/tracker model
C
C     Inputs:
C       Tibij   - ICS to BCS transformation matrix [Real]
C       X_bi_i  - Missile terminal position WRT ICS [m]
C       Y_bi_i
C       Z_bi_i
C       X_ti_i  - Position of target WRT ICS in ICS [m]
C       Y_ti_i
C       Z_ti_i
C       Xd_bi_i - Missile velocity in ICS [m/sec]
C       Yd_bi_i
C       Zd_bi_i
C       Xd_ti_i - Velocity of target WRT ICS in ICS [m/sec]
C       Yd_ti_i
C       Zd_ti_i
C
C     Outputs:
C       P_s         - Inertially referenced LOS rates, expressed in the ICS [rad/sec]
C       Q_s
C       R_s
C       Q_si_b_Meas - LOS rates expressed in the BCS [rad/sec]
C       R_si_b_Meas
C       X_tb_i      - Position of the target w.r.t. the BCS, expressed in the ICS [m]
C       Y_tb_i
C       Z_tb_i
C       Xd_tb_i     - Velocity of the target w.r.t. the BCS, expressed in the ICS [m/sec]
C       Yd_tb_i
C       Zd_tb_i
C
C     Internal variables and constants:
C       SMALL       - Arbitrarily small number [Real]
C       Range_tb_Sq - Target to body range, squared & limited [m**2]
C
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array ( 52), SMALL)
      Equivalence (Real_Array (529), Tib21)
      Equivalence (Real_Array (530), Tib22)
      Equivalence (Real_Array (531), Tib23)
      Equivalence (Real_Array (532), Tib31)
      Equivalence (Real_Array (533), Tib32)
      Equivalence (Real_Array (534), Tib33)
      Equivalence (Real_Array (500), X_bi_i)
      Equivalence (Real_Array (503), Y_bi_i)
      Equivalence (Real_Array (506), Z_bi_i)
      Equivalence (Real_Array (501), Xd_bi_i)
      Equivalence (Real_Array (504), Yd_bi_i)
      Equivalence (Real_Array (507), Zd_bi_i)
      Equivalence (Real_Array (700), X_ti_i)
      Equivalence (Real_Array (702), Y_ti_i)
      Equivalence (Real_Array (704), Z_ti_i)
      Equivalence (Real_Array (701), Xd_ti_i)
      Equivalence (Real_Array (703), Yd_ti_i)
      Equivalence (Real_Array (705), Zd_ti_i)
      Equivalence (Real_Array (600), Q_si_b_Meas)
      Equivalence (Real_Array (601), R_si_b_Meas)
      Equivalence (Real_Array (602), P_s)
      Equivalence (Real_Array (603), Q_s)
      Equivalence (Real_Array (604), R_s)
      Equivalence (Real_Array (605), Range_tb_Sq)
      Equivalence (Real_Array (606), X_tb_i)
      Equivalence (Real_Array (607), Xd_tb_i)
      Equivalence (Real_Array (608), Y_tb_i)
      Equivalence (Real_Array (609), Yd_tb_i)
      Equivalence (Real_Array (610), Z_tb_i)
      Equivalence (Real_Array (611), Zd_tb_i)
C
C     Begin math model:
C
C     Target relative position and velocity in ICS
C
      X_tb_i  = X_ti_i - X_bi_i
      Y_tb_i  = Y_ti_i - Y_bi_i
      Z_tb_i  = Z_ti_i - Z_bi_i
      Xd_tb_i = Xd_ti_i - Xd_bi_i
      Yd_tb_i = Yd_ti_i - Yd_bi_i
      Zd_tb_i = Zd_ti_i - Zd_bi_i
C
C     Missile/target range, squared & limited
C
      Range_tb_Sq = Amax1 (SMALL, (X_tb_i*X_tb_i +
     &                             Y_tb_i*Y_tb_i +
     &                             Z_tb_i*Z_tb_i))
C
C     Beam rotation rate WRT inertial space in ICS
C
      P_s = (Y_tb_i*Zd_tb_i - Z_tb_i*Yd_tb_i)/Range_tb_Sq
      Q_s = (Z_tb_i*Xd_tb_i - X_tb_i*Zd_tb_i)/Range_tb_Sq
      R_s = (X_tb_i*Yd_tb_i - Y_tb_i*Xd_tb_i)/Range_tb_Sq
C
C     Beam rotation rate WRT inertial space in BCS
C
      Q_si_b_Meas = Tib21*P_s + Tib22*Q_s + Tib23*R_s
      R_si_b_Meas = Tib31*P_s + Tib32*Q_s + Tib33*R_s
C
      Return
      End
