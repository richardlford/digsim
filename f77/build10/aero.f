C
      Subroutine Aerodynamics_Data
C
C     Aerodynamics model default data
C
C     Inputs:
C       None.
C
C     Outputs:
C       Air_Density        - Local air density [Kg/m**2]
C       Cx_Base            - Base axial drag coefficient [Real]
C       Cx_Per_Alpha_Total - Change in axial drag coefficient per total angle of attack [Real]
C       Cy_Per_Beta        - Side force coefficient per sideslop angle [Real]
C       Cy_Per_Alpha       - Lift force coefficient per angle of attack [Real]
C       Cmy_Per_Alpha      - Pitching moment coefficient per angle of attack [Real]
C       Cmz_Per_Beta       - Yawing moment coefficient per sideslip angle [Real]
C       Cmx_Per_DelP       - Rolling moment coefficient per effective roll fin deflection [Real]
C       Cmy_Per_DelQ       - Yawing moment coefficient per effective yaw fin deflection [Real]
C       Cmz_Per_DelR       - Pitching moment coefficient per effective pitch fin deflection [Real]
C       Cmp                - Roll damping coefficient [Real]
C       Cmq                - Pitch damping coefficient [Real]
C       Cmr                - Yaw damping coefficient [Real]
C       Ref_Area           - Reference area for aerodynamics calculations [m**2]
C       Ref_Length         - Reference length for aerodynamic calculations [m]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations:
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (109), Air_Density)
      Equivalence (Real_Array (110), Ref_Area)
      Equivalence (Real_Array (111), Ref_Length)
      Equivalence (Real_Array (112), Cx_Base)
      Equivalence (Real_Array (113), Cx_Per_Alpha_Total)
      Equivalence (Real_Array (114), Cy_Per_Beta)
      Equivalence (Real_Array (115), Cz_Per_Alpha)
      Equivalence (Real_Array (116), Cmy_Per_Alpha)
      Equivalence (Real_Array (117), Cmz_Per_Beta)
      Equivalence (Real_Array (118), Cmx_Per_DelP)
      Equivalence (Real_Array (119), Cmy_Per_DelQ)
      Equivalence (Real_Array (120), Cmz_Per_DelR)
      Equivalence (Real_Array (121), Cmp)
      Equivalence (Real_Array (122), Cmq)
      Equivalence (Real_Array (123), Cmr)
C
C     Begin default data definition:
C
C     Local atmospheric density
C
      Air_Density = 1.0e0
C
C     Base axial drag coefficient
C
      Cx_Base = 0.3e0
C
C     Force coefficients per aerodynamic angles
C
      Cx_Per_Alpha_Total =  -0.5e0
      Cy_Per_Beta        = -50.0e0
      Cz_Per_alpha       = -50.0e0
C
C     Moment coefficients per aerodynamic angles
C
      Cmy_Per_Alpha = -50.0e0
      Cmz_Per_Beta  =  50.0e0
C
C     Moment coefficients per effective fin deflections
C
      Cmx_Per_DelP = 10.0e0
      Cmy_Per_DelQ = 40.0e0
      Cmz_Per_DelR = 40.0e0
C
C     Damping coefficients
C
      Cmp =  -20.0e0
      Cmq = -200.0e0
      Cmr = -200.0e0
C
C     Aerodynamic reference values
C
      Ref_Area     = 0.01e0
      Ref_Length   = 1.0e0
C
      Return
      End

      Subroutine Aerodynamics_Init
C
C     Aerodynamics model initialization
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

      Subroutine Aerodynamics
C
C     This model determines the aerodynamic forces and moments acting on the
C     missile.
C
C     Inputs:
C       Air_Density        - Local air density [Kg/m**2]
C       Cx_Base            - Base axial drag coefficient [Real]
C       Cx_Per_Alpha_Total - Change in axial drag coefficient per total angle of attack [Real]
C       Cy_Per_Beta        - Side force coefficient per sideslip angle [Real]
C       Cy_Per_Alpha       - Lift force coefficient per angle of attack [Real]
C       Cmy_Per_Alpha      - Pitching moment coefficient per angle of attack [Real]
C       Cmz_Per_Beta       - Yawing moment coefficient per sideslip angle [Real]
C       Cmx_Per_DelP       - Rolling moment coefficient per effective roll fin deflection [Real]
C       Cmy_Per_DelQ       - Yawing moment coefficient per effective yaw fin deflection [Real]
C       Cmz_Per_DelR       - Pitching moment coefficient per effective pitch fin deflection [Real]
C       Cmp                - Roll damping coefficient [Real]
C       Cmq                - Pitch damping coefficient [Real]
C       Cmr                - Yaw damping coefficient [Real]
C       Fin_1_Position     - Fin positions [rad]
C       Fin_2_Position
C       Fin_3_Position
C       Fin_4_Position
C       P_b                - Missile inertial angular velocity [rad/sec]
C       Q_b
C       R_b
C       Q0_b               - ICS to BCS quaternion [Real]
C       Q1_b
C       Q2_b
C       Q3_b
C       Ref_Area           - Reference area for aerodynamic calculations [m**2]
C       Ref_Length         - Reference length for aerodynamic calculations [m]
C       Xd_bi_i            - Missile velocity in ICS [m/sec]
C       Yd_bi_i
C       Zd_bi_i
C
C     Outputs:
C       Alpha              - Pitch angle of attack [rad]
C       Alpha_Total        - Total angle of attack [rad]
C       Beta               - Sideslip angle [rad]
C       FaeroX_bi_b        - Forces due to aero in BCS [N]
C       FaeroY_bi_b
C       FaeroZ_bi_b
C       MaeroX_bi_b        - Moments due to aero in BCS [N*m]
C       MaeroY_bi_b
C       MaeroZ_bi_b
C       Tibij              - ICS to BCS transformation matrix [Real]
C
C     Internal variables and constants:
C       Del_Eff_P          - Effective fin deflections [rad]
C       Del_Eff_Q
C       Del_Eff_R
C       LrefO2Vmag         - Reference length divided by twice the total velocity [sec]
C       Qbar_B             - Dynamic pressure [N/m**2]
C       QbarSref           - Product of dynamic pressure and reference area [N]
C       QbarSrefLref       - Product of dynamic pressure, reference area, and reference length [N*m]
C       VelSq              - Total missile velocity, squared [m**2/s**2]
C       Vmag               - Total missile velocity WRT earth [m/sec]
C       Xd_bi_b            - Missile velocity WRT Earth in BCS [m/sec]
C       Yd_bi_b
C       Zd_bi_b
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (100), FaeroX_bi_b)
      Equivalence (Real_Array (101), FaeroY_bi_b)
      Equivalence (Real_Array (102), FaeroZ_bi_b)
      Equivalence (Real_Array (103), MaeroX_bi_b)
      Equivalence (Real_Array (104), MaeroY_bi_b)
      Equivalence (Real_Array (105), MaeroZ_bi_b)
      Equivalence (Real_Array (106), Alpha)
      Equivalence (Real_Array (107), Beta)
      Equivalence (Real_Array (108), Alpha_Total)
      Equivalence (Real_Array (109), Air_Density)
      Equivalence (Real_Array (110), Ref_Area)
      Equivalence (Real_Array (111), Ref_Length)
      Equivalence (Real_Array (112), Cx_Base)
      Equivalence (Real_Array (113), Cx_Per_Alpha_Total)
      Equivalence (Real_Array (114), Cy_Per_Beta)
      Equivalence (Real_Array (115), Cz_Per_Alpha)
      Equivalence (Real_Array (116), Cmy_Per_Alpha)
      Equivalence (Real_Array (117), Cmz_Per_Beta)
      Equivalence (Real_Array (118), Cmx_Per_DelP)
      Equivalence (Real_Array (119), Cmy_Per_DelQ)
      Equivalence (Real_Array (120), Cmz_Per_DelR)
      Equivalence (Real_Array (121), Cmp)
      Equivalence (Real_Array (122), Cmq)
      Equivalence (Real_Array (123), Cmr)
      Equivalence (Real_Array (501), Xd_bi_i)
      Equivalence (Real_Array (504), Yd_bi_i)
      Equivalence (Real_Array (507), Zd_bi_i)
      Equivalence (Real_Array (509), P_b)
      Equivalence (Real_Array (511), Q_b)
      Equivalence (Real_Array (513), R_b)
      Equivalence (Real_Array (518), Q0_b)
      Equivalence (Real_Array (520), Q1_b)
      Equivalence (Real_Array (522), Q2_b)
      Equivalence (Real_Array (524), Q3_b)
      Equivalence (Real_Array (526), Tib11)
      Equivalence (Real_Array (527), Tib12)
      Equivalence (Real_Array (528), Tib13)
      Equivalence (Real_Array (529), Tib21)
      Equivalence (Real_Array (530), Tib22)
      Equivalence (Real_Array (531), Tib23)
      Equivalence (Real_Array (532), Tib31)
      Equivalence (Real_Array (533), Tib32)
      Equivalence (Real_Array (534), Tib33)
      Equivalence (Real_Array (800), Fin_1_Position)
      Equivalence (Real_Array (801), Fin_2_Position)
      Equivalence (Real_Array (802), Fin_3_Position)
      Equivalence (Real_Array (803), Fin_4_Position)
C
C     Begin math model:
C
C     Evaluate ICS to BCS transformation matrix
C
      Tib11 = Q0_b*Q0_b + Q1_b*Q1_b - Q2_b*Q2_b - Q3_b*Q3_b
      Tib12 = 2.0*(Q1_b*Q2_b + Q0_b*Q3_b)
      Tib13 = 2.0*(Q1_b*Q3_b - Q0_b*Q2_b)
      Tib21 = 2.0*(Q1_b*Q2_b - Q0_b*Q3_b)
      Tib22 = Q0_b*Q0_b + Q2_b*Q2_b - Q1_b*Q1_b - Q3_b*Q3_b
      Tib23 = 2.0*(Q2_b*Q3_b + Q0_b*Q1_b)
      Tib31 = 2.0*(Q1_b*Q3_b + Q0_b*Q2_b)
      Tib32 = 2.0*(Q2_b*Q3_b - Q0_b*Q1_b)
      Tib33 = Q0_b*Q0_b + Q3_b*Q3_b - Q1_b*Q1_b - Q2_b*Q2_b
C
C     Missile velocity WRT ICS origin in BCS
C
      Xd_bi_b = Xd_bi_i*Tib11 + Yd_bi_i*Tib12 + Zd_bi_i*Tib13
      Yd_bi_b = Xd_bi_i*Tib21 + Yd_bi_i*Tib22 + Zd_bi_i*Tib23
      Zd_bi_b = Xd_bi_i*Tib31 + Yd_bi_i*Tib32 + Zd_bi_i*Tib33
      VelSq   = Xd_bi_i*Xd_bi_i +
     &          Yd_bi_i*Yd_bi_i +
     &          Zd_bi_i*Zd_bi_i
      Vmag    = Sqrt (VelSq)
C
C     Angle of attack, sideslip angle, and total angle-of-attack
C
      Alpha       = ArcTan (Zd_bi_b, Xd_bi_b)
      Beta        = ArcTan (Yd_bi_b, Xd_bi_b)
      Alpha_Total = ArcTan (Sqrt (Yd_bi_b*Yd_bi_b + Zd_bi_b*
     &                            Zd_bi_b), Xd_bi_b)
C
C     Dynamic pressure and multipliers for forces and moments
C
      Qbar_B       = 0.5e0*Air_Density*VelSq
      QbarSref     = Qbar_B*Ref_Area
      QbarSrefLref = QbarSref*Ref_Length
      LrefO2Vmag   = Ref_Length/ (2.0e0*Vmag)
C
C     Equivalent fin deflections
C
      Del_Eff_P = -0.25e0* (Fin_1_Position +
     &                      Fin_2_Position +
     &                      Fin_3_Position +
     &                      Fin_4_Position)
      Del_Eff_q = 0.5e0* (Fin_3_Position -
     &                    Fin_1_Position)
      Del_Eff_R = 0.5e0* (Fin_4_Position -
     &                    Fin_2_Position)
C
C     Aerodynamics forces
C
      FaeroX_bi_b = (Cx_Base + Cx_Per_Alpha_Total*Alpha_Total)*QbarSref
      FaeroY_bi_b = Cy_Per_Beta*Beta*QbarSref
      FaeroZ_Bi_b = Cz_Per_Alpha*Alpha*QbarSref
C
C     Aerodynamics moments
C
      MaeroX_bi_b = QbarSrefLref*
     &              (Cmx_Per_DelP*Del_Eff_P + Cmp*P_b*LrefO2Vmag)
      MaeroY_bi_b = QbarSrefLref*
     &              (Cmy_Per_Alpha*Alpha +
     &               Cmy_Per_DelQ*Del_Eff_Q + Cmq*Q_b*LrefO2Vmag)
      MaeroZ_bi_b = QbarSrefLref*
     &              (Cmz_Per_Beta*Beta +
     &               Cmz_Per_DelR*Del_Eff_R + Cmr*R_b*LrefO2Vmag)
C
      Return
      End
