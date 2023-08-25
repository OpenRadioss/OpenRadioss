Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  PBLAST_3                      source/loads/pblast/pblast_3.F
Chd|-- called by -----------
Chd|        PBLAST                        source/loads/pblast/pblast.F  
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PBLAST_MOD                    ../common_source/modules/loads/pblast_mod.F
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE PBLAST_3(ILOADP  ,FAC      ,A       ,V         ,X       ,
     1                    IADC    ,FSKY     ,LLOADP  ,FEXT      ,
     2                    ITAB    ,H3D_DATA ,NL      ,DTMIN_LOC ,TFEXT_LOC,
     4                    TH_SURF ,FSAVSURF ,NSEG_LOADP,NSEGPL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE PBLAST_MOD
      USE GROUPDEF_MOD
      USE H3D_INC_MOD
      USE TH_SURF_MOD , ONLY : TH_SURF_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "mvsiz_p.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: LLOADP(SLLOADP)
      INTEGER,INTENT(INOUT) :: ILOADP(SIZLOADP,NLOADP)
      INTEGER,INTENT(IN) :: IADC(*)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),NL
      my_real,INTENT(INOUT) :: DTMIN_LOC,TFEXT_LOC
      my_real,INTENT(IN) :: V(3,NUMNOD),X(3,NUMNOD)
      my_real,INTENT(INOUT) :: FAC(LFACLOAD,NLOADP)
      my_real,INTENT(INOUT) :: A(3,NUMNOD),FSKY(8,SFSKY/8), FEXT(3,NUMNOD)
      TYPE(H3D_DATABASE),INTENT(IN) :: H3D_DATA
      TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
      my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
      INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF), NSEGPL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1, N2, N3, N4,IL,IS,IAD,I,IANIM_OR_H3D,IZ_UPDATE,ABAC_ID,ISIZ_SEG,IERR1
      INTEGER Phi_I, ID, ITA_SHIFT, NITER,ITER,IMODEL,ITMP
      INTEGER :: idx_zg1, idx_zg2, idx1_angle, idx2_angle, idx1, idx2,NS,KSURF, I_search
      INTEGER :: curve_id1, curve_id2, NN(4), NDT
      LOGICAL :: calculate, BOOL_UNDERGROUND_CURRENT_SEG, IS_UPDATED

      my_real :: Zx,Zy,Zz,NORM,Nx,Ny,Nz ! target face centroid and normal vector
      my_real NNx,NNy,NNz,NORM_NN, NORM2_NN, tmp_var ! ground vector
      my_real HZ ! height of centroid
      my_real LG,ZG ! ground distance and scaled ground distance(SHDC : scaled horizontal distance from charge)
      my_real ZC ! scaled height of charge, and its value in ft/(lb**1/3)
      my_real SHTP,HTP ! scaled height of triple point
      my_real ANGLE_G ! angle of incidence at ground
      my_real Pra, Pra_refl !mach front incident pressure and refelcted pressure
      my_real Ira, Ira_refl !mach impulse
      my_real ProjZ(3),ProjDet(3), tmp(3)
      my_real Z_struct ! scaled height for target face
      my_real alpha_zc ! interpolation between curves Scaled Height of Charge
      my_real alpha_zg ! interpolation between abscissa on figure 2-13 (scaled distance on ground)
      my_real :: alpha_angle !inteprolation factor

      my_real LAMBDA, cos_theta, alpha_inci, alpha_refl,P_inci,P_refl,Z,Phi_DB,bound1,bound2,
     .         I_inci,I_refl,dt_0,t_a,WAVE_refl,WAVE_inci, W13
      my_real DNORM,Xdet,Ydet,Zdet,Tdet,Wtnt,PMIN,T_STOP,Dx,Dy,Dz,P,
     .        FAC_M_bb, FAC_L_bb, FAC_T_bb, FAC_P_bb, FAC_I_bb, TA_FIRST, TT_STAR
      my_real HC ! height of charge
      my_real logRes
      my_real Z1_
      my_real DECAY_inci,DECAY_refl,ZETA,TMP2,TMP3,FUNCT,DIFF,RES,TOL,AREA
      my_real kk ,NPT
      my_real :: TA_INF_LOC

      my_real PI_
      DATA PI_/3.141592653589793238462643D0/

      my_real dzc
      DATA dzc /0.07058823500000000/

      my_real :: cst_255_div_ln_Z1_on_ZN
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/

      my_real ::log10_
      DATA log10_ /2.30258509299405000000/

      my_real :: cst_180
      DATA cst_180 /180.000000000000000000/

      INTEGER,EXTERNAL :: DICHOTOMIC_SEARCH_R_DESC,DICHOTOMIC_SEARCH_R_ASC

      my_real FAC_UNIT ! convert scaled distance from cm/g^1/3 to ft/lb^1/3
      DATA FAC_unit/3.966977216838196139019/

C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines is applying pressure load to a segment submitted to a blast wave (AIR BURST model).
C Preussre time histories are built from "UFC 3-340-02, Dec. 5th 2008" tables which are hardcoded in unit system {g, cm, mus}
C (table sampling : 256 points ; see pblast_mod.F)
C
C  T* = T + TA_INF (shift with first arrival time for all pblast option,  TA_INF=0. if ita_shift/=2)
C  If request made to update blast profile (iz_update==2) then it is made once on T*=TDET
C
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      !--- subroutine not relevant if no /LOAD/PBLAST option
      IF(NLOADP_B==0)RETURN

      !--- time step
      TA_FIRST = FAC(07,NL) ! arrival time for first segment to be loaded
      IL= NL-NLOADP_F
      TT_STAR = TT + PBLAST_DT%TA_INF
      IZ_UPDATE = ILOADP(06,NL)
      TDET = FAC(01,NL)
      TA_FIRST = FAC(07,NL) ! arrival time for first segment to be loaded
      IF(IZ_UPDATE ==1)THEN
        !if no update during engine
        DTMIN_LOC = (ONE+EM06)*(TA_FIRST - TT)
        DTMIN_LOC=MAX(PBLAST_TAB(IL)%DTMIN, DTMIN_LOC) !go directly to arrival time but avoid too
        IF(TT_STAR<TA_FIRST)RETURN ! (nothing to load for now)
      ELSE
        IF(TDET > TT)THEN
          DTMIN_LOC = (ONE+EM06)*(TDET - TT)
          DTMIN_LOC=MAX(PBLAST_TAB(IL)%DTMIN, DTMIN_LOC)!go directly to arrival time but avoid too
        ELSE
          DTMIN_LOC = PBLAST_TAB(IL)%DTMIN
        ENDIF
        IF(TT_STAR<TDET)RETURN ! (nothing to update for now)
      ENDIF
C-----------------------------------------------,
C   S o u r c e   C o d e
C-----------------------------------------------
      TFEXT_LOC = ZERO
      IANIM_OR_H3D = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT   !output

      !Z Range (tables)
      Z1_ = 0.500000000000000
      !ZN = 400.000

      !translation from Working unit System to {big bang} unit system
      FAC_M_bb = FAC_MASS*EP03
      FAC_L_bb = FAC_LENGTH*EP02
      FAC_T_bb = FAC_TIME*EP06
      FAC_P_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb/FAC_T_bb
      FAC_I_bb = FAC_P_bb*FAC_T_bb !/FAC_M_bb**THIRD

      IS_UPDATED=.FALSE.

      !-----------------------------------------------,
      !    AIR BURST
      !-----------------------------------------------
      IL             = NL-NLOADP_F
      TDET           = FAC(01,NL)
      XDET           = FAC(02,NL)
      YDET           = FAC(03,NL)
      ZDET           = FAC(04,NL)
      WTNT           = FAC(05,NL)
      PMIN           = FAC(06,NL)
      TA_FIRST       = FAC(07,NL)
      NNX            = FAC(08,NL)
      NNY            = FAC(09,NL)
      NNZ            = FAC(10,NL)
      HC             = FAC(11,NL)
      alpha_zc       = FAC(12,NL)  !curve_id1+alpha_zc
      T_STOP         = FAC(13,NL)
      ISIZ_SEG       = ILOADP(01,NL)/4
      IS             = ILOADP(02,NL)
      IZ_UPDATE      = ILOADP(06,NL)
      ABAC_ID        = ILOADP(07,NL)
      ID             = ILOADP(08,NL) !user_id
      ITA_SHIFT      = ILOADP(09,NL)
      NDT            = ILOADP(10,NL)
      IMODEL         = ILOADP(11,NL)
      TA_INF_LOC     = EP20

      !curve_id1+alpha_zc
      curve_id1=INT(alpha_zc)
      curve_id2=min(10,curve_id1+1)
      alpha_zc = alpha_zc - curve_id1

      IERR1 = 0
      W13 = (WTNT*FAC_M_bb)**THIRD   ! '*FAC_M'  g->work unit    '/FAC_M' : WORK_UNIT -> g
      Z = ZERO
      NORM2_NN=NNX*NNx+NNy*NNy+NNz*NNz
      NORM_NN =SQRT(NORM2_NN)

      !---------------------------------------------
      !   LOOP ON SEGMENTS (4N or 3N)
      !---------------------------------------------
!$OMP DO SCHEDULE(GUIDED,MVSIZ)
      DO I = 1,ISIZ_SEG

        BOOL_UNDERGROUND_CURRENT_SEG = .FALSE.

        N1=LLOADP(ILOADP(4,NL)+4*(I-1))
        N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
        N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
        N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)

        IF(N4==0 .OR. N3==N4)THEN
          !3-NODE-SEGMENT
          PBLAST_TAB(IL)%NPt(I) = THREE
          NPT = THREE
          !Segment centroid
          Zx = X(1,N1)+X(1,N2)+X(1,N3)
          Zy = X(2,N1)+X(2,N2)+X(2,N3)
          Zz = X(3,N1)+X(3,N2)+X(3,N3)
          Zx = Zx*THIRD
          Zy = Zy*THIRD
          Zz = Zz*THIRD
          !Normal vector (NX,NY,NZ) = 2*S*n where |n|=1.0
          NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
          NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
          NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
          !NORM = 2*S
          NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
        ELSE
          !4-NODE-SEGMENT
          PBLAST_TAB(IL)%NPt(I) = FOUR
          NPT = FOUR
          !Segment centroid
          Zx = X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4)
          Zy = X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4)
          Zz = X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4)
          Zx = Zx*FOURTH
          Zy = Zy*FOURTH
          Zz = Zz*FOURTH
          !Normal vector (NX,NY,NZ) = 2*S*n where |n|=1.0
          NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
          NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
          NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
          !NORM = 2*S
          NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
        ENDIF
        NN(1)=N1
        NN(2)=N2
        NN(3)=N3
        NN(4)=N4
        PBLAST_TAB(IL)%N(1,I) = N1
        PBLAST_TAB(IL)%N(2,I) = N2
        PBLAST_TAB(IL)%N(3,I) = N3
        PBLAST_TAB(IL)%N(4,I) = N4

!-------------------------------                                                                                                                                                                  ---

        !--------------------------------------------!
        !          Update Wave parameters            !
        ! (experimental)                             !
        ! If requested. Otherwise use Starter param. !
        ! Default : do not update:use Starter param. !
        !--------------------------------------------!
        IF(IZ_UPDATE == 2)THEN

          DTMIN_LOC = EP20

          !Dist
          Dx    = (Xdet - Zx)*FAC_L_bb  ! => working unit to cm
          Dy    = (Ydet - Zy)*FAC_L_bb  ! => ... to cm
          Dz    = (Zdet - Zz)*FAC_L_bb  ! => ... to cm
          DNORM = SQRT(Dx*Dx+Dy*Dy+Dz*Dz)

          !scaled distance
          Z     = DNORM / W13    !in abac unit ID  g,cm,mus

          !Determine Height of centroid (structure face)
          !  Basis = DETPOINT - NN
          ProjDet(1)=Xdet + NNX
          ProjDet(2)=Ydet + NNY
          ProjDet(3)=Zdet + NNZ
          !Height is length Proj->Det. Storing Det->Proj into NN array
          HZ=-(NNX*Zx + NNX*Zy + NNZ*Zz  - NNX*ProjDet(1)-NNX*ProjDet(2)-NNZ*ProjDet(3))/HC

          cos_theta = ZERO

          IF(HZ < ZERO)THEN

            !underground segment (nothing to compute)
            P_inci = zero
            I_inci = zero
            P_refl = zero
            I_refl = zero
            DT_0 = EP20
            T_A = EP20
            DECAY_refl = ONE
            DECAY_inci = ONE
            BOOL_UNDERGROUND_CURRENT_SEG = .TRUE.

          ELSE

            Z_struct = HZ*FAC_L_bb/W13 ! scaled value in same unit system as harcoded tables {g,cm,mus,Mbar}

            !Scaled Height of Charge
            ZC = HC * FAC_L_bb/W13! scaled value in same unit system as harcoded tables {g,cm,mus,Mbar}

            !Horizontal Distance between Charge and Centroid : LG
            ! ZG = scaled distance |ProjC->ProjZ|
            ProjZ(1) = Zx + HZ*NNX/HC
            ProjZ(2) = Zy + HZ*NNY/HC
            ProjZ(3) = Zz + HZ*NNZ/HC
            tmp(1) = (ProjZ(1)-ProjDet(1))
            tmp(2) = (ProjZ(2)-ProjDet(2))
            tmp(3) = (ProjZ(3)-ProjDet(3))
            LG = SQRT(TMP(1)*TMP(1)+TMP(2)*TMP(2)+TMP(3)*TMP(3))
            ZG = LG*FAC_L_bb/W13     !scaled horizontal distance (ground) ; same unit system as harcoded tables {g,cm,mus,Mbar}

            !Angle of structural face (mach wave is planar wave)
            cos_theta = (ProjDet(1)-ProjZ(1))*Nx +  (ProjDet(2)-ProjZ(2))*Ny + (ProjDet(3)-ProjZ(3))*Nz
            cos_theta = cos_theta/MAX(EM20,LG*NORM)

            !determine angle of incidence at ground (AANGLE_g) and interpolation factor (alpha_angle)
            tmp(1)=Xdet-ProjZ(1)
            tmp(2)=Ydet-ProjZ(2)
            tmp(3)=Zdet-ProjZ(3)
            tmp_var=SQRT( tmp(1)*tmp(1) + tmp(2)*tmp(2) + tmp(3)*tmp(3) )
            ANGLE_g = -( NNX*tmp(1) + NNY*tmp(2) + NNZ*tmp(3) ) /Hc/tmp_var  !must be between [-PI_,PI_]
            ANGLE_g = min(ONE,max(-ONE,ANGLE_g)) ! bound it to expected range (epsilon)
            ANGLE_g = acos(ANGLE_g)
            ANGLE_g = cst_180/PI_*ANGLE_g !debug purpose
            IF(ANGLE_g < ZERO)THEN
              WRITE(IOUT,*) ' ** WARNING : /LOAD/PBLAST id=',ID,' NEGATIVE ANGLE,',ANGLE_g,
     .         ' FACE:',ITAB((/N1,N2,N3,N4/)),' SEEMS TO BE BELOW THE GROUND'
              WRITE(ISTDO,*)' ** WARNING : /LOAD/PBLAST id=',ID,' NEGATIVE ANGLE,',ANGLE_g,
     .          ' FACE:',ITAB((/N1,N2,N3,N4/)),' SEEMS TO BE BELOW THE GROUND'
              ANGLE_g = ZERO
            ELSEIF(ANGLE_g > 85.00000)THEN
              WRITE(IOUT,FMT='(A,I0,A,E10.4,A,4I11)')
     .        ' ** WARNING : /LOAD/PBLAST id=',ID,' ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,
     .          '. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))
              WRITE(ISTDO,FMT='(A,I0,A,E10.4,A,4I11)')
     .        ' ** WARNING : /LOAD/PBLAST id=',ID,' ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,
     .          '. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))
              ANGLE_g = 85.00000
            ENDIF
            tmp(1)=ANGLE_g/PBLAST_DATA%delta_angle
            itmp=INT(tmp(1))
            idx1_angle = 1+itmp   ! surjection ANGLE -> idx1
            idx2_angle = MIN(idx1_angle+1,256)
            alpha_angle = (ANGLE_g-itmp*PBLAST_DATA%delta_angle)/PBLAST_DATA%delta_angle  !interpolation factor between angle(idx1_angle) & angle(idx2_angle)


            ! -----------(ZC,ZC) => SHTP (Fig 2-13) ------------ !
            !  -- SCALED HEIGHT OF TRIPLE POINT --
            ! curve_ids          1      2    3    4   5    6    7    8    9    10
            ! 10 curves for ZC : {1.0; 1.5; 2.0; 2.5; 3.0; 3.5; 4.0; 5.0; 6.0; 7.0}    ft/lb**1/3
            !                    <--------curve_id=[2x-1]---------><----[x+3]----->
            !
            !--------------------------------------------------------------------------------------------
            ! --- 1. two retained curves & interpolation factor alpha_zc
            ! --------------------------
            curve_id1 = 0
            alpha_zc = zero
            ZC = ZC/FAC_UNIT   ! ft/lb**0.3333  (fig 2-13 : 10 plots, one for a given ZC value)
            IF(ZC < 4)THEN
              itmp = INT( TWO*(ZC)-ONE )
              curve_id1 =  max(1,itmp)
              curve_id2 = curve_id1+1
              IF(itmp < 1)THEN
                !message out of bounds. curve 1 will be used. no extrapolation
                curve_id2=curve_id1 !=1
                alpha_zc = ZERO
                WRITE(IOUT,FMT='(A,I0,A,E10.4,A,I0)')
     .            ' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HEIGHT OF CHARGE',ZC,
     .            ' IS BELOW THE LOWER BOUND AND SET TO:',PBLAST_DATA%Curve_val_2_13(1)
                WRITE(ISTDO,FMT='(A,I0,A,E10.4,A,I0)')
     .            ' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HEIGHT OF CHARGE',ZC,
     .            ' IS BELOW THE LOWER BOUND AND SET TO:',PBLAST_DATA%Curve_val_2_13(1)
              ELSE
                alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1))
                alpha_zc = alpha_zc/  ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )
              ENDIF
            ELSE
              itmp = INT( ZC+THREE )
              curve_id1 = INT( min(10,itmp) )
              curve_id2 = curve_id1+1
              IF(curve_id1 == 10)THEN
                !message out of bounds. curve 10 will be used. no extrapolation
                curve_id2=curve_id1 !=10
                alpha_zc = ZERO
                WRITE(IOUT,FMT='(A,I0,A,E10.4,A,E10.4)')
     .            ' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HEIGHT OF CHARGE ',ZC,
     .            ' IS OVER THE UPPER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(10)
                WRITE(ISTDO,FMT='(A,I0,A,E10.4,A,E10.4)')
     .            ' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HEIGHT OF CHARGE ',ZC,
     .            ' IS OVER THE UPPER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(10)
              ELSE
                alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1))
                alpha_zc = alpha_zc / ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )
              ENDIF
            ENDIF

            ! --------------------------
            ! --- 2. abscissa (ZG) index to read the plot and interpolate (alpha_zg)  :  idx_zg1, idx_zg2
            ! --------------------------
            !idx_zg1 is index in [1,256] for abscissa %SHTP(curve_id,1:256)
            ! PBLAST_DATA%SHDC(1:2,curve_id) are the SHDC bounds for a given curve_id
            idx_zg1 = MIN(MAX(1,INT((Zg-7.9)/PBLAST_DATA%dSHDC)+1),256)  !curve on pliot 2-13 are starting from 2 ft/lb**0.333, hardcoded table start from  7.9 cm/g**0.333
            idx_zg2 = MAX(1,MIN(256,idx_zg1+1))
            alpha_zg = zero
            IF(ZG < PBLAST_DATA%SHDC(1,curve_id2))THEN
              ZG =  PBLAST_DATA%SHDC(1,curve_id2)
              WRITE(IOUT, FMT='(A,I0,A,E10.4,A,I0,A,E10.4,A)')
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HORIZONTAL DISTANCE ',ZG,
     .          ' IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',
     .          curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
              WRITE(ISTDO,FMT='(A,I0,A,E10.4,A,I0,A,E10.4,A)')
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HORIZONTAL DISTANCE ',ZG,
     .          ' IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',
     .          curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
            ENDIF
            IF(ZG > PBLAST_DATA%SHDC(2,curve_id1))THEN
              ZG =  PBLAST_DATA%SHDC(2,curve_id1)
              WRITE(IOUT,FMT='(A,I0,A,E10.4,A,I0,A,E10.4,A)')
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HORIZONTAL DISTANCE ', ZG ,
     .          ' IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',
     .          curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
              WRITE(ISTDO,FMT='(A,I0,A,E10.4,A,I0,A,E10.4,A)')
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HORIZONTAL DISTANCE ', ZG ,
     .          ' IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',
     .          curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
            ENDIF
            alpha_zg = (ZG-PBLAST_DATA%SHTP_abscissa(idx_zg1))/PBLAST_DATA%dSHDC !abscissa interpolation

            ! --------------------------
            ! --- 3. Read and interpolate ordinate : SHTP
            ! --------------------------
            !Scaled Height of triple point SHTP
            !tmp(1) : angle interpolation on curve_id1 Figure 2_13
            tmp(1)=PBLAST_DATA%SHTP(curve_id1,idx_zg1)
            tmp(2)=PBLAST_DATA%SHTP(curve_id1,idx_zg2)
            tmp(1) = (ONE-alpha_zg)*tmp(1) + alpha_zg*tmp(2)
            !tmp(2) : interpolation on curve_id2 Figure 2_13
            tmp(2)=PBLAST_DATA%SHTP(curve_id2,idx_zg1)
            tmp(3)=PBLAST_DATA%SHTP(curve_id2,idx_zg2)
            tmp(2) = (ONE-alpha_zg)*tmp(2) + alpha_zg*tmp(3)
            !interpolate now with scaled height of charge
            SHTP = (ONE-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)
            HTP = SHTP*W13


            ! --------------------------
            ! --- 4. Compare SHTP with target height
            ! --------------------------
            !Check if triple point is above the target centroid
            ! print warning otherwise and load with the same wave as below (may be updated later but in practise triple point is above the structure)
            !
            IF(Z_struct > SHTP)THEN
              WRITE(IOUT, FMT='(A,I0,A,4(I0,A1),A,E10.4,A,E10.4,A)')
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,' TARGET FACE IS ABOVE THE TRIPLE POINT:',N1,",",N2,",",N3,",",N4,
     .          " ; ","Zface = ",Z_struct," > ",SHTP," = SHTP (ft/fb**1/3)"
              WRITE(ISTDO, FMT='(A,I0,A,4(I0,A1),A,E10.4,A,E10.4,A)')
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,' TARGET FACE IS ABOVE THE TRIPLE POINT:',N1,",",N2,",",N3,",",N4,
     .          " ; ","Zface = ",Z_struct," > ",SHTP," = SHTP (ft/fb**1/3)"
            ENDIF


            ! -----------(alpha,ZC) => Pra (Fig 2-9) ------------ !
            ! -- REFLECTED PRESSURE Pra --
            !
            !--------------------------------------------------------------------------------------------
            ! --- 1. two retained curves & interpolation factor alpha_zc
            ! --------------------------

            ! Pra =
            ! here curves are plot from zc=0.3 to zc=14.3 : need to calculate alpha_zc again since 10 curves are for different ZC values : 0.3 "0.5" 0.8 etc....
            ! ZC IN FT/LB**0.33  (10 plots on fig 2-9 depending on ZC)
            idx1 = MAX(1,DICHOTOMIC_SEARCH_R_ASC(ZC, PBLAST_DATA%Curve_val_2_9, 10))
            calculate=.FALSE.
            IF(idx1 == 1)THEN
              IF(ZC <= PBLAST_DATA%Curve_val_2_9(1))THEN
                alpha_zc = zero
                idx1 = 1
                idx2 = 1
                WRITE(IOUT,*)
     .          '** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,
     .          ' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
                WRITE(ISTDO,*)
     .          '** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,
     .          ' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
              ELSE
                idx1 = 1
                idx2 = 2
                calculate=.TRUE.
              ENDIF
            ELSEIF(idx1 >= 10)THEN
              alpha_zc = zero
              idx1 = 10
              idx2 = 10
              WRITE(IOUT,*)
     .         ' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-9, CURVE=',
     .            10,' HC=',ZC/FAC_UNIT
              WRITE(ISTDO,*)
     .         ' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-9, CURVE=',
     .            10,' HC=',ZC/FAC_UNIT
            ELSE
              idx2=idx1+1
              calculate=.TRUE.
            ENDIF
            IF(calculate)THEN
              alpha_zc=(ZC-PBLAST_DATA%Curve_val_2_9(idx1))
     .                / (PBLAST_DATA%Curve_val_2_9(idx2)-PBLAST_DATA%Curve_val_2_9(idx1)) !interpolation between 2 retained plots (out of 10 from Fig 2-9, each one for a given ZC value)
            ENDIF

            curve_id1=idx1
            curve_id2=idx2
            !tmp(1) : angle interpolation on curve_id1 Figure 2_9
            tmp(1)=PBLAST_DATA%Pra(curve_id1,idx1_angle)
            tmp(2)=PBLAST_DATA%Pra(curve_id1,idx2_angle)
            tmp(1)=(ONE-alpha_angle)*LOG10(tmp(1))+alpha_angle*LOG10(tmp(2))
            !tmp(2) : interpolation on curve_id2 Figure 2_9
            tmp(2)=PBLAST_DATA%Pra(curve_id2,idx1_angle)
            tmp(3)=PBLAST_DATA%Pra(curve_id2,idx2_angle)
            tmp(2)=(ONE-alpha_angle)*LOG10(tmp(2))+alpha_angle*LOG10(tmp(3))
            !interpolate now with scaled height of charge
            Pra = (ONE-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)
            Pra = exp(Pra*log10_)




            ! -----------(alpha,ZC) => Pra (Fig 2-10) ------------ !
            ! -- SCALED REFLECTED IMPULSE --
            !
            !--------------------------------------------------------------------------------------------
            ! --- 1. two retained curves & interpolation factor alpha_zc
            ! --------------------------

            idx1 = MAX(1,DICHOTOMIC_SEARCH_R_ASC(ZC, PBLAST_DATA%Curve_val_2_10, 10))
            calculate=.FALSE.
            IF(idx1 == 1)THEN
              IF(ZC <= PBLAST_DATA%Curve_val_2_10(1))THEN
                alpha_zc = zero
                idx1 = 1
                idx2 = 1
                WRITE(IOUT,*)
     .      '** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-10, CURVE=',1,
     .          ' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
                WRITE(ISTDO,*)
     .      '** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-10, CURVE=',1,
     .          ' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
              ELSE
                idx1 = 1
                idx2 = 2
                calculate=.TRUE.
              ENDIF
            ELSEIF(idx1 >= 10)THEN
                alpha_zc = zero
                idx1 = 10
                idx2 = 10
                WRITE(IOUT,*)
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-10, CURVE=',
     .              10,' HC=',ZC/FAC_UNIT
                WRITE(ISTDO,*)
     .          ' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-10, CURVE=',
     .              10,' HC=',ZC/FAC_UNIT
            ELSE
              idx2=idx1+1
              calculate=.TRUE.
            ENDIF
            IF(calculate)THEN
             alpha_zc=(ZC-PBLAST_DATA%Curve_val_2_10(idx1))
     .                 / (PBLAST_DATA%Curve_val_2_10(idx2)-PBLAST_DATA%Curve_val_2_10(idx1)) !interpolation between 2 retained plots (out of 10 from Fig 2-9, each one for a given ZC value)
            ENDIF

            curve_id1=idx1
            curve_id2=idx2

            ! --------------------------
            ! --- 2. interpolate SRI on each curve
            ! --------------------------
            !tmp(1) : angle interpolation on curve_id1 Figure 2_10
            tmp(1)=PBLAST_DATA%SRI(curve_id1,idx1_angle)
            tmp(2)=PBLAST_DATA%SRI(curve_id1,idx2_angle)
            tmp(1)=(ONE-alpha_angle)*LOG10(tmp(1))+alpha_angle*LOG10(tmp(2))
            !tmp(2) : interpolation on curve_id2 Figure 2_10
            tmp(2)=PBLAST_DATA%SRI(curve_id2,idx1_angle)
            tmp(3)=PBLAST_DATA%SRI(curve_id2,idx2_angle)
            tmp(2)=(ONE-alpha_angle)*LOG10(tmp(2))+alpha_angle*LOG10(tmp(3))
            ! --------------------------
            ! --- 3. interpolate SRI between 2 curves
            ! --------------------------
            !interpolate now with scaled height of charge
            Ira = (ONE-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)
            Ira = exp(Ira*log10_)

            ! Use Pra as Pso on figure 2-7 ; determine corresponding Scaled distance ; read corresponding values Pr, Pso-, ta/W**1/3
            !
            !get Pra
            ! searching in monotonic function : idx1 such as PBLAST_DATA%Pra(idx1) <= Pra <  PBLAST_DATA%Pra(idx1+1)
            idx1 = MAX(1,DICHOTOMIC_SEARCH_R_DESC(Pra, PBLAST_DATA%Pso, 256))
            idx2 = MIN(idx1+1,256)
            bound1=LOG10(PBLAST_DATA%Pso(idx1))
            bound2=LOG10(PBLAST_DATA%Pso(idx2))
            LAMBDA = (LOG10(Pra)-bound1) / (bound2-bound1)
            ! deduce Z
            bound1 = LOG10(PBLAST_DATA%RW3(idx1))
            bound2 = LOG10(PBLAST_DATA%RW3(idx2))
            LogRes =  (ONE-lambda)*bound1+lambda*bound2
            Z = exp(LogRes*log10_)
            !deduce Pra_refl (=Pr(Z) where Z=Z(Pra) )
            bound1 = PBLAST_DATA%Pr(idx1)
            bound2 = PBLAST_DATA%Pr(min(256,idx2))
            LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)
            Pra_refl = exp(LogRes*log10_)
            !deduce ta
            bound1 = PBLAST_DATA%ta(idx1)
            bound2 = PBLAST_DATA%ta(min(256,idx2))
            LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)
            T_A = exp(LogRes*log10_)

                   ! Use Ira as Is on figure 2-7  ; determine corresponding Scaled distance ; read corresponding values Ir,Ir-, t0/W**1/3, t0-/W**1/3
            !
            ! searching in monotonic function : idx1 such as PBLAST_DATA%Pra(idx1) <= Pra <  PBLAST_DATA%Pra(idx1+1)
            idx1 = MAX(1,DICHOTOMIC_SEARCH_R_DESC(Ira, PBLAST_DATA%Iso, 256))
            idx2 = MIN(idx1+1,256)
            bound1=LOG10(PBLAST_DATA%Iso(idx1))
            bound2=LOG10(PBLAST_DATA%Iso(idx2))
            LAMBDA = (LOG10(Ira)-bound1) / (bound2-bound1)
            !deduce Z
            bound1 = LOG10(PBLAST_DATA%RW3(idx1))
            bound2 = LOG10(PBLAST_DATA%RW3(idx2))
            LogRes =  (ONE-lambda)*bound1+lambda*bound2
            Z = exp(LogRes*log10_)
            !deduce Ira_refl (=Pr(Z) where Z=Z(Pra) )
            bound1 = PBLAST_DATA%Irefl(idx1)
            bound2 = PBLAST_DATA%Irefl(min(256,idx2))
            LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)
            Ira_refl = exp(LogRes*log10_)
            !deduce t0
            bound1 = PBLAST_DATA%t0(idx1)
            bound2 = PBLAST_DATA%t0(min(256,idx2))
            LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)
            DT_0 = exp(LogRes*log10_)

            !switch from normalized values.       Pressure are not scaled by W13 in abacuses
            P_inci = Pra
            P_refl = Pra_refl
            I_inci = Ira
            I_refl = Ira_refl
            I_inci  = I_inci * W13
            I_refl  = I_refl * W13
            DT_0    = DT_0   * W13
            T_A     = T_A    * W13

            !---DECAY  ('b' parameter in modified Friedlander model)
            !    iterative solver : it can be solved in same unit system as hardcoded tables {g,cm,µs} since b has no dimension
            IF(IMODEL == 1)THEN
              !-Friedlander
              DECAY_inci = ONE
              DECAY_refl = ONE

            ELSEIF(IMODEL == 2 .AND. .NOT.BOOL_UNDERGROUND_CURRENT_SEG) THEN
              !SOLVE DECAY (b):    I_inci = P_inci*DT_0/b*(ONE-(1-exp(-b))/b)
              !     g: b-> I_inci - P_inci*DT_0/b*(ONE-(1-exp(-b))/b)
              ! find b such as g(b)=0
              ! NEWTON ITERATIONS
              NITER=20
              TOL=EM06
              ITER=0
              ZETA=ONE
              RES=EP20
              TMP2= P_inci*DT_0*EXP(-ZETA)/ZETA/ZETA
              !--initialize first iteration
              kk=P_inci*DT_0
              FUNCT = HALF*kk -I_inci !-ONE_OVER_6*kk*ZETA
              !--iterative solving
              DO WHILE (ITER<=NITER .AND. RES>TOL)
                ITER=ITER+1
                IF(ABS(ZETA) < EM06)THEN
                  !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
                  DIFF = kk*(-ONE_OVER_6 + ONE_OVER_12*ZETA)
                  ZETA = ZETA - FUNCT/DIFF
                  FUNCT = HALF*kk-ONE_OVER_6*kk*ZETA - I_inci
                ELSE
                  DIFF = ZETA*TMP2*EXP(ZETA) - (FUNCT+I_inci)*(ONE + TWO/ZETA)
                  ZETA = ZETA - FUNCT/DIFF
                  TMP2= P_inci*DT_0*EXP(-ZETA)/ZETA/ZETA
                  TMP3 = EXP(ZETA)*(ZETA-ONE)+ONE
                  FUNCT = TMP2 * TMP3 -I_inci
                ENDIF
                RES = ABS(FUNCT)   !g(x_new)
              ENDDO
              DECAY_inci=MAX(ONE,ZETA)   ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

              ITER=0
              ZETA=ONE
              RES=EP20
              TMP2= P_refl*DT_0*EXP(-ZETA)/ZETA/ZETA
              !--initialize first iteration
              kk=P_refl*DT_0
              FUNCT = HALF*kk -I_refl !-ONE_OVER_6*kk*ZETA
              !--iterative solving
              DO WHILE (ITER<=NITER .AND. RES>TOL)
                ITER=ITER+1
                IF(ABS(ZETA) < EM06)THEN
                  !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
                  DIFF = kk*(-ONE_OVER_6 + ONE_OVER_12*ZETA)
                  ZETA = ZETA - FUNCT/DIFF
                  FUNCT = HALF*kk-ONE_OVER_6*kk*ZETA - I_refl
                ELSE
                  DIFF = ZETA*TMP2*EXP(ZETA) - (FUNCT+I_refl)*(ONE + TWO/ZETA)
                  ZETA = ZETA - FUNCT/DIFF
                  TMP2= P_refl*DT_0*EXP(-ZETA)/ZETA/ZETA
                  TMP3 = EXP(ZETA)*(ZETA-ONE)+ONE
                  FUNCT = TMP2 * TMP3 -I_refl
                ENDIF
                RES = ABS(FUNCT)   !g(x_new)
              ENDDO
              DECAY_refl=MAX(ONE,ZETA)   ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

            ENDIF!IMODEL


            !CONVERSION UNITS !
            !g,cm,mus,Bar -> Working unit system
            P_inci  =  P_inci / FAC_P_bb
            I_inci  =  I_inci / FAC_I_bb
            P_refl  =  P_refl / FAC_P_bb
            I_refl  =  I_refl / FAC_I_bb
            DT_0    =  DT_0    / FAC_T_bb
            T_A     =  T_A     / FAC_T_bb
            T_A    = T_A + TDET
            TA_INF_LOC = MIN(TA_INF_LOC, T_A)

            !update wave parameters
            PBLAST_TAB(IL)%cos_theta(I) = cos_theta
            PBLAST_TAB(IL)%P_inci(I) = P_inci
            PBLAST_TAB(IL)%P_refl(I) = P_refl
            PBLAST_TAB(IL)%ta(I) = T_A
            PBLAST_TAB(IL)%t0(I) = DT_0
            PBLAST_TAB(IL)%decay_inci(I) = decay_inci
            PBLAST_TAB(IL)%decay_refl(I) = decay_refl

          ENDIF !HZ

          DTMIN_LOC = MIN(DTMIN_LOC,DT_0/NDT)
          IZ_UPDATE = 1 !update done
          ILOADP(06,NL) = IZ_UPDATE
          IS_UPDATED=.TRUE.

        ELSE

          !Use wave parameters from Starter
          cos_theta = PBLAST_TAB(IL)%cos_theta(I)
          P_inci = PBLAST_TAB(IL)%P_inci(I)
          P_refl = PBLAST_TAB(IL)%P_refl(I)
          T_A  = PBLAST_TAB(IL)%ta(I)
          DT_0 = PBLAST_TAB(IL)%t0(I)
          decay_inci = PBLAST_TAB(IL)%decay_inci(I)
          decay_refl = PBLAST_TAB(IL)%decay_refl(I)
          DTMIN_LOC = PBLAST_TAB(IL)%DTMIN

        ENDIF ! IZ_UPDATE

!-------------------------------                                                                                                                                                                  ---

        !Coefficients for wave superimposition
        !PressureLoad = Reflected_Pressure * cos2X + IncidentPressure * (1 + cos2X -2 cosX)
        IF(cos_theta<=ZERO)THEN
          !Surface not facing the point of explosion
          alpha_refl = ZERO
          alpha_inci = ONE
        ELSE
          alpha_refl = cos_theta**2                              ! cos**2 a
          alpha_inci = ONE + cos_theta - TWO * alpha_refl        ! 1 + cos a -2 cos**2 a
        ENDIF

        !Building pressure waves from Friedlander model. (Modified model can bu introduced later if needed)
        WAVE_INCI = ZERO
        WAVE_REFL = ZERO
        IF(TT_STAR >= T_A)THEN
          WAVE_INCI =  P_inci*(ONE-(TT_STAR-T_A)/DT_0)*exp(-DECAY_inci*(TT_STAR-T_A)/DT_0)
          WAVE_REFL =  P_refl*(ONE-(TT_STAR-T_A)/DT_0)*exp(-DECAY_refl*(TT_STAR-T_A)/DT_0)
        ELSE
          WAVE_INCI = ZERO
          WAVE_REFL = ZERO
        ENDIF
        P = alpha_refl * WAVE_REFL + alpha_inci * WAVE_INCI
        P = MAX(P,PMIN)
        IF (NUMSKINP > 0) PBLAST_TAB(IL)%PRES(I) = P

        !Expand Pressure load to nodes
        PBLAST_TAB(IL)%FX(I)= -P * HALF*NX / NPT
        PBLAST_TAB(IL)%FY(I)= -P * HALF*NY / NPT
        PBLAST_TAB(IL)%FZ(I)= -P * HALF*NZ / NPT

        !External Force work
        ! on a given node : DW = <F,V>*dt
        ! for this current 4-node or 3-node face :   DW = sum(   <F_k,V_k>*dt       k=1,NPT)   where F_k=Fel/NPT
        TFEXT_LOC=TFEXT_LOC+DT1*(   PBLAST_TAB(IL)%FX(I) * SUM(  V( 1, NN(1:NINT(NPt))  )  )
     +                            + PBLAST_TAB(IL)%FY(I) * SUM(  V( 2, NN(1:NINT(NPt))  )  )
     +                            + PBLAST_TAB(IL)%FZ(I) * SUM(  V( 3, NN(1:NINT(NPt))  )  )
     +                          )

C----- /TH/SURF -------
        IF(TH_SURF%LOADP_FLAG > 0 ) THEN
          NSEGPL = NSEGPL + 1
          AREA = HALF*SQRT(NX*NX+NY*NY+NZ*NZ)
          DO NS=TH_SURF%LOADP_KSEGS(NSEGPL) +1,TH_SURF%LOADP_KSEGS(NSEGPL+1)
             KSURF = TH_SURF%LOADP_SEGS(NS)
             NSEG_LOADP(KSURF) = NSEG_LOADP(KSURF) + 1
             FSAVSURF(4,KSURF)= FSAVSURF(4,KSURF) + P
             FSAVSURF(5,KSURF)= FSAVSURF(5,KSURF) + AREA
          ENDDO
        ENDIF


      ENDDO!next I
!$OMP END DO

      IF(IS_UPDATED)THEN
#include "lockon.inc"
        !---arrival time
        ZETA = FAC(07,NL)
        FAC(07,NL) = MIN(TA_INF_LOC, FAC(07,NL)) !smp min value
        !---time step
        DTMIN_LOC = (ONE+EM06)*(FAC(07,NL) - TT) ! go directly to trigger time
        DTMIN_LOC=MAX(PBLAST_TAB(IL)%DTMIN, DTMIN_LOC)
        !---no update on next cycle
        IZ_UPDATE = 1 !update done
        ILOADP(06,NL) = IZ_UPDATE
#include "lockoff.inc"
        CALL MY_BARRIER()
!$OMP SINGLE
        write(*,FMT='(A,I10,A,E16.8,A,E16.8)') "Updated parameters for /LOAD/PBLAST id=",
     .                                         ID,' previous first arrival time :',ZETA,
     .                                         ' is now updated to :',FAC(07,NL)
!$OMP END SINGLE
      ENDIF

      !-------------------------------------------------------------------!
      !   FORCE ASSEMBLY                                                  !
      !     /PARITH/OFF : F directly added in A(1:3,1:NUMNOD).            !
      !     /PARITH/ON  : F added FSKY & and automatically treated later  !
      !-------------------------------------------------------------------!
      ! SPMD/SMP Parith/OFF
      IF(IPARIT==0) THEN
!$OMP SINGLE
        DO I = 1,ISIZ_SEG
          N1=LLOADP(ILOADP(4,NL)+4*(I-1))
          N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
          N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
          N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
          A(1,N1)=A(1,N1)+PBLAST_TAB(IL)%FX(I)
          A(2,N1)=A(2,N1)+PBLAST_TAB(IL)%FY(I)
          A(3,N1)=A(3,N1)+PBLAST_TAB(IL)%FZ(I)
          A(1,N2)=A(1,N2)+PBLAST_TAB(IL)%FX(I)
          A(2,N2)=A(2,N2)+PBLAST_TAB(IL)%FY(I)
          A(3,N2)=A(3,N2)+PBLAST_TAB(IL)%FZ(I)
          A(1,N3)=A(1,N3)+PBLAST_TAB(IL)%FX(I)
          A(2,N3)=A(2,N3)+PBLAST_TAB(IL)%FY(I)
          A(3,N3)=A(3,N3)+PBLAST_TAB(IL)%FZ(I)
          IF(PBLAST_TAB(IL)%NPt(I) == FOUR)THEN
            A(1,N4)=A(1,N4)+PBLAST_TAB(IL)%FX(I)
            A(2,N4)=A(2,N4)+PBLAST_TAB(IL)%FY(I)
            A(3,N4)=A(3,N4)+PBLAST_TAB(IL)%FZ(I)
          ENDIF
        ENDDO
!$OMP END SINGLE
      ELSE
!$OMP DO SCHEDULE(GUIDED,MVSIZ)
        DO I = 1,ISIZ_SEG
          IAD         =IADC(ILOADP(4,NL)+4*(I-1))
          FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)
          FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)
          FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)
          IAD         =IADC(ILOADP(4,NL)+4*(I-1)+1)
          FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)
          FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)
          FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)
          IAD         =IADC(ILOADP(4,NL)+4*(I-1)+2)
          FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)
          FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)
          FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)
          IF(PBLAST_TAB(IL)%NPt(I) == FOUR)THEN
            IAD         =IADC(ILOADP(4,NL)+4*(I-1)+3)
            FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)
            FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)
            FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)
          ENDIF
        ENDDO
!$OMP END DO
      ENDIF !IPARIT


      !-------------------------------------------!
      !   ANIMATION FILE   /ANIM/VECT/FEXT        !
      !   H3D FILE         /H3D/NODA/FEXT         !
      !-------------------------------------------!

!$OMP SINGLE
      IF(IANIM_OR_H3D>0) THEN
        DO I = 1,ISIZ_SEG
          N1=PBLAST_TAB(IL)%N(1,I)
          N2=PBLAST_TAB(IL)%N(2,I)
          N3=PBLAST_TAB(IL)%N(3,I)
          N4=PBLAST_TAB(IL)%N(4,I)
          FEXT(1,N1) = FEXT(1,N1)+PBLAST_TAB(IL)%FX(I)
          FEXT(2,N1) = FEXT(2,N1)+PBLAST_TAB(IL)%FY(I)
          FEXT(3,N1) = FEXT(3,N1)+PBLAST_TAB(IL)%FZ(I)
          FEXT(1,N2) = FEXT(1,N2)+PBLAST_TAB(IL)%FX(I)
          FEXT(2,N2) = FEXT(2,N2)+PBLAST_TAB(IL)%FY(I)
          FEXT(3,N2) = FEXT(3,N2)+PBLAST_TAB(IL)%FZ(I)
          FEXT(1,N3) = FEXT(1,N3)+PBLAST_TAB(IL)%FX(I)
          FEXT(2,N3) = FEXT(2,N3)+PBLAST_TAB(IL)%FY(I)
          FEXT(3,N3) = FEXT(3,N3)+PBLAST_TAB(IL)%FZ(I)
          IF(PBLAST_TAB(IL)%NPt(I)==FOUR)THEN
            FEXT(1,N4) = FEXT(1,N4)+PBLAST_TAB(IL)%FX(I)
            FEXT(2,N4) = FEXT(2,N4)+PBLAST_TAB(IL)%FY(I)
            FEXT(3,N4) = FEXT(3,N4)+PBLAST_TAB(IL)%FZ(I)
          ENDIF
        ENDDO
       ENDIF
!$OMP END SINGLE

      RETURN

C-----------------------------------------------
       IF (IERR1/=0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION - PBLAST LOADING'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION - PBLAST LOADING'
         CALL ARRET(2)
       END IF
C-----------------------------------------------

      END SUBROUTINE
