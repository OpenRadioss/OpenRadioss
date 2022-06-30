Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MATERIAL_FLOW                 source/tools/seatbelts/material_flow.F
Chd|-- called by -----------
Chd|        R23L114DEF3                   source/elements/spring/r23l114def3.F
Chd|-- calls ---------------
Chd|        SOLVE_DELTA_L0                source/tools/seatbelts/material_flow.F
Chd|        SOLVE_DELTA_L02               source/tools/seatbelts/material_flow.F
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        GET_U_FUNC_INV                source/user_interface/ufunc.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE MATERIAL_FLOW(DFS,DFS_OLD,ALDP,SLIPRING_STRAND,XK,
     1                         OFF,AL0,AL02,LMIN,UPDATE_FLAG,
     2                         RING_SLIP,SLIPRING_ID,XL0,DL,DLOLD,
     3                         EXDP,EYDP,EZDP,X1DP,X2DP,
     4                         X3DP,ADHER,NC1,NC2,NC3,
     5                         FLAG_SLIPRING_UPDATE,ADD_NODE1,ADD_NODE2,VX1,VY1,
     6                         VZ1,VX2,VY2,VZ2,VX3,
     7                         VY3,VZ3,XC,RETRACTOR_ID,FLAG_RETRACTOR_UPDATE,
     8                         SENSOR,AL0DP,FR_ID,DDF,FX,FX2,
     9                         ALDP2,AL0DP2,EX2DP,EY2DP,EZ2DP,
     A                         XL02,XK2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE SEATBELT_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER UPDATE_FLAG(*),SLIPRING_ID(*),NC1(*),NC2(*),NC3(*),SLIPRING_STRAND(*),
     .        ADHER(*),FLAG_SLIPRING_UPDATE,ADD_NODE1(*),ADD_NODE2(*),RETRACTOR_ID(*),
     .        FLAG_RETRACTOR_UPDATE,FR_ID(*)
      my_real DFS(*),DFS_OLD(*),AL0(*),AL02(*),LMIN(*),RING_SLIP(*),
     .        XL0(*),DL(*),DLOLD(*),EXDP(*),EYDP(*),EZDP(*),
     .        XK(*),VX1(*),VY1(*),VZ1(*),VX2(*),VY2(*),VZ2(*),VX3(*),VY3(*),VZ3(*),XC(*),
     .        OFF(*),SENSOR(NSENSR,*)
      DOUBLE PRECISION ALDP(*),X1DP(3,*),X2DP(3,*),X3DP(3,*),AL0DP(*)
      my_real, INTENT(IN) :: FX(MVSIZ),FX2(MVSIZ),XK2(MVSIZ)
      my_real, INTENT(OUT) :: DDF(MVSIZ),XL02(MVSIZ)
      DOUBLE PRECISION, INTENT(IN) :: ALDP2(MVSIZ),EX2DP(MVSIZ),EY2DP(MVSIZ),EZ2DP(MVSIZ)
      DOUBLE PRECISION, INTENT(INOUT) :: AL0DP2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NODE1,NODE2,NODE3,ANCHOR_NODE,NEW_NODE2,FUNC,ISENS,FLAG_SENSOR_LOCK,FLAG_NO_COUNT_PULL,FL_IMPOSED_FORCE
      INTEGER FLAG
      my_real DDFS,BETA,HH,HH2,FRIC,DELTAF,DDV,DT11,FB,F1,F2,DF
      my_real TENS,XSCAL,NORM,PRES,VREL,ALPHA,ALPHA2,VEC1(3),VEC2(3)
      my_real FRICD,FRICS,XX,DXDY,YY,GET_U_FUNC,GET_U_FUNC_INV,FB2,ANGLE,DDPULL,DD_PRETENS
      my_real DDX,DDX2,F_LOCK,F_UNLOCK,PRETENS,XX1,XX2,COS_BETA
      my_real OMEGA,EPS1,EPS2,EPSN,FACT
      DOUBLE PRECISION DELTA_LO
C
      EXTERNAL GET_U_FUNC,GET_U_FUNC_INV
C---------------------------------------------------------
C
      DT11 = DT1
      IF(DT11.EQ.ZERO) DT11 = EP30
C
C----------------------------------------------------------
C-    MATERIAL FLOW COMPUTATION FOR SLIPRING AND RETRACTOR
C----------------------------------------------------------
C
      DO I=LFT,LLT
C
        IF (OFF(I) == ONE) THEN
C
        IF (SLIPRING_STRAND(I) == 1) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         SLIPRING - BRIN 1 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C-------- computation of delta_lo
C
          DDX =    EXDP(I) * (VX2(I) - VX1(I))
     .           + EYDP(I) * (VY2(I) - VY1(I))
     .           + EZDP(I) * (VZ2(I) - VZ1(I))

          IF (SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(1) == 1) THEN 
            DDX2 =   EX2DP(I)* (VX3(I) - VX2(I))
     .             + EY2DP(I)* (VY3(I) - VY2(I))
     .             + EZ2DP(I)* (VZ3(I) - VZ2(I))
          ELSE
            DDX2 =   EX2DP(I)* (VX1(I) - VX3(I))
     .             + EY2DP(I)* (VY1(I) - VY3(I))
     .             + EZ2DP(I)* (VZ1(I) - VZ3(I))
          ENDIF
C
C----------------------------------------------------------------------------------
C         Computation of friction coefficient
C
C----     Effect of material flow --
C
          VREL = ABS(SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%MATERIAL_FLOW)
C--       dynamic friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(1) == 0) THEN
            FRICD = SLIPRING(SLIPRING_ID(I))%FRIC
          ELSE
            FRICD = SLIPRING(SLIPRING_ID(I))%FRIC
            XX = TT / SLIPRING(SLIPRING_ID(I))%FAC_D(1)
            FRICD = FRICD*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(1),XX,DXDY)
          ENDIF
C--       static friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(3) == 0) THEN
            FRICS = SLIPRING(SLIPRING_ID(I))%FRICS
          ELSE
            FRICS = SLIPRING(SLIPRING_ID(I))%FRICS
            XX = TT / SLIPRING(SLIPRING_ID(I))%FAC_S(1)
            FRICS = FRICS*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(3),XX,DXDY)
          ENDIF
C          
C----     Effect of normal force--
C
C--       bisectrice computation 
          VEC1(1) = EXDP(I) + EX2DP(I)
          VEC1(2) = EYDP(I) + EY2DP(I)
          VEC1(3) = EZDP(I) + EZ2DP(I)
          NORM = SQRT((VEC1(1)*VEC1(1))+(VEC1(2)*VEC1(2))+(VEC1(3)*VEC1(3)))
          VEC1(1) = VEC1(1)/NORM
          VEC1(2) = VEC1(2)/NORM
          VEC1(3) = VEC1(3)/NORM
C
          EPS1 = (ALDP(I)-AL0DP(I))/MAX(LMIN(I),AL0(I))
          EPS2 = (ALDP2(I)-AL0DP2(I))/MAX(LMIN(I),AL02(I))
C
          FB = FX(I)
          FB2 = FX2(I)
          PRES = FB*(EXDP(I)*VEC1(1)+EYDP(I)*VEC1(2)+EZDP(I)*VEC1(3))
          PRES = PRES + FB2*(EX2DP(I)*VEC1(1)+EY2DP(I)*VEC1(2)+EZ2DP(I)*VEC1(3))
C
C--       dynamic friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(2) > 0) THEN
            XX = PRES / SLIPRING(SLIPRING_ID(I))%FAC_D(2)
            FRICD = FRICD + SLIPRING(SLIPRING_ID(I))%FAC_D(3)*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(2),XX,DXDY)
          ENDIF
C--       static friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(4) > 0) THEN
            XX = PRES / SLIPRING(SLIPRING_ID(I))%FAC_S(2)
            FRICS = FRICS + SLIPRING(SLIPRING_ID(I))%FAC_S(3)*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(4),XX,DXDY)
          ENDIF
C
          IF (FRICS == ZERO) THEN
            FRIC = FRICD
          ELSE
            FRIC = FRICD + (FRICS - FRICD)*EXP(-SLIPRING(SLIPRING_ID(I))%DC*VREL)
          ENDIF
C          
C----     Effect of orientation angle --
C
          IF (SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%ORIENTATION_NODE > 0) THEN
            ANGLE = SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%ORIENTATION_ANGLE
            FRIC = FRIC*(ONE + SLIPRING(SLIPRING_ID(I))%A*ANGLE*ANGLE)
          ENDIF
C
C----------------------------------------------------------------------------------
C
          DFS_OLD(I) =  DFS(I)
          DDFS = DDX*DT1*XK(I)/AL0(I) + DDX2*DT1*XK2(I)/AL02(I)
C
          COS_BETA = EXDP(I)*EX2DP(I)+EYDP(I)*EY2DP(I)+ EZDP(I)*EZ2DP(I)
          COS_BETA = SIGN(MIN(ONE,ABS(COS_BETA)),COS_BETA)
          BETA  = PI - ACOS(COS_BETA)
C
          IF (DFS(I) == ZERO) THEN
            HH = EXP(BETA*SIGN(FRIC,DDFS))
          ELSE
            HH = EXP(BETA*SIGN(FRIC,DFS(I)))
          ENDIF 
C
          FLAG = 0
          OMEGA = (HH*FB2-FB+XK(I)*EPS1-HH*XK2(I)*EPS2)/XK(I)
          HH2 = HH*(XK2(I)/XK(I))
          CALL SOLVE_DELTA_L0(FLAG,DELTA_LO,HH2,ALDP(I),ALDP2(I),AL0DP(I),AL0DP2(I),LMIN(I),OMEGA)
C
          FB = FX(I) + XK(I)*(ALDP(I)-AL0DP(I)-DELTA_LO)/(AL0DP(I)+DELTA_LO) - XK(I)*EPS1
C
          IF (AL0DP2(I) - DELTA_LO < LMIN(I)) THEN
            LMIN(I) = MAX(LMIN(I),AL02(I))
            FLAG = 1
            CALL SOLVE_DELTA_L0(FLAG,DELTA_LO,HH2,ALDP(I),ALDP2(I),AL0DP(I),AL0DP2(I),LMIN(I),OMEGA)
            DDFS = DDX*DT1*XK(I)/AL0DP(I) + DDX2*DT1*XK(I)/LMIN(I)
            FB = FX(I) + XK(I)*(ALDP(I)-AL0DP(I)-DELTA_LO)/(AL0DP(I)+DELTA_LO) - XK(I)*EPS1
          ELSEIF (AL0DP(I) + DELTA_LO < LMIN(I)) THEN
            LMIN(I) = MAX(LMIN(I),AL0(I))
            FLAG = 2
            CALL SOLVE_DELTA_L0(FLAG,DELTA_LO,HH2,ALDP(I),ALDP2(I),AL0DP(I),AL0DP2(I),LMIN(I),OMEGA)
            DDFS = DDX*DT1*XK(I)/LMIN(I) + DDX2*DT1*XK(I)/AL0DP2(I)
            FB = FX(I) + XK(I)*(ALDP(I)-AL0DP(I)-DELTA_LO)/LMIN(I) - XK(I)*EPS1
          ENDIF
C
          DFS(I) = DFS(I) + DDFS
          DELTAF = FB*(ONE-ONE/MAX(EM15,HH))
C
          IF ((ABS(DELTAF) >= ABS(DFS(I))).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FL_FLAG==1).AND.(DELTA_LO > 0)).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FL_FLAG==2).AND.(DELTA_LO < 0)).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED==1).AND.(DELTA_LO > 0)).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED==2).AND.(DELTA_LO < 0))) THEN
C--         adherence or 1 direction flow imposed by fl_flag or blocked because no more seatbelt
            ADHER(I) = 1
            DELTA_LO = 0
          ELSE
            DFS(I) = DELTAF
          ENDIF
C
C--       update of variables
          AL0DP(I) = AL0DP(I) + DELTA_LO
          AL0DP2(I) = AL0DP2(I) - DELTA_LO
          AL0(I) = AL0DP(I)
          AL02(I) = AL0DP2(I)
C
          EPSN = (ALDP(I)-AL0DP(I))/MAX(AL0(I),LMIN(I))
          DDF(I) = XK(I)*(EPSN-EPS1)
          IF (ADHER(I) == 1) DDF(I) = DDF(I) + DFS(I) - DFS_OLD(I) 
          XL0(I) = MAX(AL0(I),LMIN(I))
          XL02(I)= MAX(AL02(I),LMIN(I))
          UPDATE_FLAG(I) = 0
C
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%VECTOR(1) = EXDP(I)*SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(1)
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%VECTOR(2) = EYDP(I)*SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(1)
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%VECTOR(3) = EZDP(I)*SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(1)
C
C--       TH variables stored only for strand1
          RING_SLIP(I) = RING_SLIP(I) + DELTA_LO
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%RINGSLIP = SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%RINGSLIP - DELTA_LO
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%MATERIAL_FLOW = DELTA_LO/DT11
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%BETA = BETA
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%SLIP_FORCE(3) = PRES
C
C-------- Detection of slipring update
C
           IF (SLIPRING(SLIPRING_ID(I))%NFRAM > 1) THEN
             FACT = 6.0
           ELSE
             FACT = 1.3
           ENDIF
C
           IF (ALDP(I) + FACT*DDX*DT11 < ZERO) THEN
             IF (NC1(I)==SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE(1)) THEN
               IF (ADD_NODE1(I) > 0) THEN
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(1) = ADD_NODE1(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(2) = NC1(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(3) = NC2(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%DFS = DFS(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%UPDATE = 2
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 0
               ELSE
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 2
               ENDIF
             ELSE
               IF (ADD_NODE2(I) > 0) THEN
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(1) = ADD_NODE2(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(2) = NC2(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(3) = NC1(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%DFS = DFS(I)
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%UPDATE = 2
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 0
               ELSE
                 SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 1
               ENDIF
             ENDIF
           ENDIF
C 
        ELSEIF (SLIPRING_STRAND(I) == 2) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         SLIPRING - BRIN 2 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C-------- computation of delta_lo
C
          FRIC = SLIPRING(SLIPRING_ID(I))%FRIC
C     
          DDX =    EXDP(I) * (VX1(I) - VX2(I))
     .           + EYDP(I) * (VY1(I) - VY2(I))
     .           + EZDP(I) * (VZ1(I) - VZ2(I))

          IF (SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(2) == 1) THEN
            DDX2 =   EX2DP(I)* (VX3(I) - VX1(I))
     .             + EY2DP(I)* (VY3(I) - VY1(I))
     .             + EZ2DP(I)* (VZ3(I) - VZ1(I))
          ELSE
            DDX2 =   EX2DP(I)* (VX2(I) - VX3(I))
     .             + EY2DP(I)* (VY2(I) - VY3(I))
     .             + EZ2DP(I)* (VZ2(I) - VZ3(I))
          ENDIF

C         Computation of friction coefficient
C
C----     Effect of material flow --
          VREL = ABS(SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%MATERIAL_FLOW)
C--       dynamic friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(1) == 0) THEN
            FRICD = SLIPRING(SLIPRING_ID(I))%FRIC
          ELSE
            FRICD = SLIPRING(SLIPRING_ID(I))%FRIC
            XX = TT / SLIPRING(SLIPRING_ID(I))%FAC_D(1)
            FRICD = FRICD*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(1),XX,DXDY)
          ENDIF
C--       static friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(3) == 0) THEN
            FRICS = SLIPRING(SLIPRING_ID(I))%FRICS
          ELSE
            FRICS = SLIPRING(SLIPRING_ID(I))%FRICS
            XX = TT / SLIPRING(SLIPRING_ID(I))%FAC_S(1)
            FRICS = FRICS*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(3),XX,DXDY)
          ENDIF
C
C----     Effect of normal force--
C
C--       bisectrice computation 
          VEC1(1) = EXDP(I) + EX2DP(I)
          VEC1(2) = EYDP(I) + EY2DP(I)
          VEC1(3) = EZDP(I) + EZ2DP(I)
          NORM = SQRT((VEC1(1)*VEC1(1))+(VEC1(2)*VEC1(2))+(VEC1(3)*VEC1(3)))
          VEC1(1) = VEC1(1)/NORM
          VEC1(2) = VEC1(2)/NORM
          VEC1(3) = VEC1(3)/NORM
C
          EPS1 = (ALDP(I)-AL0DP(I))/MAX(LMIN(I),AL0(I))
          EPS2 = (ALDP2(I)-AL0DP2(I))/MAX(LMIN(I),AL02(I))
C
          FB = FX(I)
          FB2 = FX2(I)
          PRES = FB*(EXDP(I)*VEC1(1)+EYDP(I)*VEC1(2)+EZDP(I)*VEC1(3))
          PRES = PRES + FB2*(EX2DP(I)*VEC1(1)+EY2DP(I)*VEC1(2)+EZ2DP(I)*VEC1(3))
C
C--       dynamic friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(2) > 0) THEN
            XX = PRES / SLIPRING(SLIPRING_ID(I))%FAC_D(2)
            FRICD = FRICD+SLIPRING(SLIPRING_ID(I))%FAC_D(3)*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(2),XX,DXDY)
          ENDIF
C--       static friction
          IF (SLIPRING(SLIPRING_ID(I))%IFUNC(4) > 0) THEN
            XX = PRES / SLIPRING(SLIPRING_ID(I))%FAC_S(2)
            FRICS = FRICS+SLIPRING(SLIPRING_ID(I))%FAC_S(3)*GET_U_FUNC(SLIPRING(SLIPRING_ID(I))%IFUNC(4),XX,DXDY)
          ENDIF
C
          IF (FRICS == ZERO) THEN
            FRIC = FRICD
          ELSE
            FRIC = FRICD + (FRICS - FRICD)*EXP(-SLIPRING(SLIPRING_ID(I))%DC*VREL)
          ENDIF
C          
C----     Effect of orientation angle --
C
          IF (SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%ORIENTATION_NODE > 0) THEN
            ANGLE = SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%ORIENTATION_ANGLE
            FRIC = FRIC*(ONE + SLIPRING(SLIPRING_ID(I))%A*ANGLE*ANGLE)
          ENDIF
C
C----------------------------------------------------------------------------------
C
          DFS_OLD(I) =  DFS(I)
          DDFS = DDX*DT1*XK(I)/AL0(I) + DDX2*DT1*XK2(I)/AL02(I)
C
          COS_BETA = EXDP(I)*EX2DP(I)+EYDP(I)*EY2DP(I)+ EZDP(I)*EZ2DP(I)
          COS_BETA = SIGN(MIN(ONE,ABS(COS_BETA)),COS_BETA)
          BETA  = PI - ACOS(COS_BETA)
C
          IF (DFS(I) == ZERO) THEN
            HH = EXP(BETA*SIGN(FRIC,DDFS))
          ELSE
            HH = EXP(BETA*SIGN(FRIC,DFS(I)))
          ENDIF
C
          FLAG = 0
          OMEGA = (HH*FB-FB2+XK2(I)*EPS2-HH*XK(I)*EPS1)/XK2(I)
          HH2 = HH*(XK(I)/XK2(I))
          CALL SOLVE_DELTA_L0(FLAG,DELTA_LO,HH2,ALDP2(I),ALDP(I),AL0DP2(I),AL0DP(I),LMIN(I),OMEGA)
C
          FB = FX(I) + XK(I)*(ALDP(I)-AL0DP(I)+DELTA_LO)/(AL0DP(I)-DELTA_LO) - XK(I)*EPS1
C
          IF (AL0DP(I) - DELTA_LO < LMIN(I)) THEN
            LMIN(I) = MAX(LMIN(I),AL0(I))
            FLAG = 1
            CALL SOLVE_DELTA_L0(FLAG,DELTA_LO,HH2,ALDP2(I),ALDP(I),AL0DP2(I),AL0DP(I),LMIN(I),OMEGA)
            DDFS = DDX*DT1*XK(I)/LMIN(I) + DDX2*DT1*XK(I)/AL0DP2(I)
            FB = FX(I) + XK(I)*(ALDP(I)-AL0DP(I)+DELTA_LO)/LMIN(I) - XK(I)*EPS1
          ELSEIF (AL0DP2(I) + DELTA_LO < LMIN(I)) THEN
            LMIN(I) = MAX(LMIN(I),AL02(I))
            FLAG = 2
            CALL SOLVE_DELTA_L0(FLAG,DELTA_LO,HH2,ALDP2(I),ALDP(I),AL0DP2(I),AL0DP(I),LMIN(I),OMEGA)
            DDFS = DDX*DT1*XK(I)/AL0(I) + DDX2*DT1*XK(I)/LMIN(I)
            FB = FX(I) + XK(I)*(ALDP(I)-AL0DP(I)+DELTA_LO)/(AL0DP(I)-DELTA_LO) - XK(I)*EPS1
          ENDIF
C
          DFS(I) = DFS(I) + DDFS
          DELTAF = FB*(HH-ONE)
C
          IF ((ABS(DELTAF) >= ABS(DFS(I))).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FL_FLAG==1).AND.(DELTA_LO > 0)).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FL_FLAG==2).AND.(DELTA_LO < 0)).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED==1).AND.(DELTA_LO > 0)).OR.
     .        ((SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED==2).AND.(DELTA_LO < 0))) THEN
C--         adherence or 1 direction flow imposed by fl_flag or blocked because no more seatbelt
            ADHER(I) = 1
            DELTA_LO = 0
          ELSE
            DFS(I) = DELTAF
          ENDIF
C
          AL0DP2(I) = AL0DP2(I) + DELTA_LO
          AL0DP(I) = AL0DP(I) - DELTA_LO
          AL02(I) = AL0DP2(I)
          AL0(I) = AL0DP(I)
C
          EPSN = (ALDP(I)-AL0DP(I))/MAX(AL0(I),LMIN(I))
          DDF(I) = XK(I)*(EPSN-EPS1)
          IF (ADHER(I) == 1) DDF(I) = DDF(I) - DFS(I) + DFS_OLD(I)
          XL0(I) = MAX(AL0(I),LMIN(I))
          XL02(I)= MAX(AL02(I),LMIN(I))
          UPDATE_FLAG(I) = 0
C
          RING_SLIP(I) = 0
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%VECTOR(4) = EXDP(I)*SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(2)
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%VECTOR(5) = EYDP(I)*SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(2)
          SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%VECTOR(6) = EZDP(I)*SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(2)
C
C-------- Detection of slipring update
C
           IF (SLIPRING(SLIPRING_ID(I))%NFRAM > 1) THEN
             FACT = 6.0
           ELSE
             FACT = 1.3
           ENDIF
C
          IF (ALDP(I) - FACT*DDX*DT1 < ZERO) THEN
            IF (NC1(I)==SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE(2)) THEN
              IF (ADD_NODE2(I) > 0) THEN
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(1) = NC1(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(2) = NC2(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(3) = ADD_NODE2(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%DFS = DFS(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%UPDATE = -2
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 0
              ELSE
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 1
              ENDIF
            ELSE
              IF (ADD_NODE1(I) > 0) THEN
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(1) = NC2(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(2) = NC1(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%NODE_NEXT(3) = ADD_NODE1(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%DFS = DFS(I)
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%UPDATE = -2
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 0
              ELSE
                SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%LOCKED = 2
              ENDIF
            ENDIF
          ENDIF
C
        ELSEIF (SLIPRING_STRAND(I) == -1) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                  RETRACTOR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
            EPS1 = (ALDP(I)-AL0DP(I))/MAX(EM20,MAX(AL0(I),LMIN(I)))
            FB = FX(I)
            F_LOCK = FB
            F_UNLOCK = RETRACTOR(RETRACTOR_ID(I))%UNLOCK_FORCE
C
C--------------------Pretensionning -------------------------------------
            IF ((RETRACTOR(RETRACTOR_ID(I))%ISENS(2) > 0).AND.
     .          (RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV == 0)) THEN
              IF (TT > SENSOR(2,RETRACTOR(RETRACTOR_ID(I))%ISENS(2))) THEN
                 RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV = 1
                 RETRACTOR(RETRACTOR_ID(I))%PRETENS_TIME = TT
              ENDIF
            ENDIF
C
            FL_IMPOSED_FORCE = 0
            DD_PRETENS = ZERO
            PRETENS = ZERO
            IF (RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV == 1) THEN
              XX = (TT - RETRACTOR(RETRACTOR_ID(I))%PRETENS_TIME) / RETRACTOR(RETRACTOR_ID(I))%FAC(4)
              PRETENS = RETRACTOR(RETRACTOR_ID(I))%FAC(3)*GET_U_FUNC(RETRACTOR(RETRACTOR_ID(I))%IFUNC(3),XX,DXDY)
              IF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 1) THEN
C--             Pretensioning with pullin
                IF (RETRACTOR(RETRACTOR_ID(I))%FORCE > ZERO) THEN
C--               Pretensioning deactivated when force is reached
                  IF (F_LOCK > RETRACTOR(RETRACTOR_ID(I))%FORCE) THEN
                    RETRACTOR(RETRACTOR_ID(I))%LOCKED = 1
                    YY = F_LOCK/RETRACTOR(RETRACTOR_ID(I))%FAC(1)        
                    XX = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),YY)*RETRACTOR(RETRACTOR_ID(I))%FAC(2)
                    RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL = RETRACTOR(RETRACTOR_ID(I))%FAC(2)*XX
                    RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV = -1
                    RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL = ZERO
                  ENDIF
                ENDIF
              ELSEIF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 2) THEN
C--             Pretensioning with force -> F = MAX(F_retractor,F_pretens)
                IF (RETRACTOR(RETRACTOR_ID(I))%LOCKED == 1) THEN
                  YY = F_LOCK/RETRACTOR(RETRACTOR_ID(I))%FAC(1)        
                  XX = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),YY)*RETRACTOR(RETRACTOR_ID(I))%FAC(2)
                  DELTA_LO = XX - RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL
                  EPSN = (ALDP(I)-AL0DP(I)-DELTA_LO)/MAX(LMIN(I),AL0DP(I)+DELTA_LO)
                  F_UNLOCK = MAX(FX(I) + XK(I)*(EPSN-EPS1),PRETENS)
                  FL_IMPOSED_FORCE = 1
                ELSE
                  F_UNLOCK = MAX(RETRACTOR(RETRACTOR_ID(I))%UNLOCK_FORCE,PRETENS)
                ENDIF
              ELSEIF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 3) THEN
C--             Pretensioning with force -> F = MAX(F_retractor,F_pretens) - same as type2 with locking
                IF (RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL > RETRACTOR(RETRACTOR_ID(I))%PULLOUT) THEN
C--               Pretensioning deactivated when pullout is reached
                  RETRACTOR(RETRACTOR_ID(I))%LOCKED = 1
                  YY = F_LOCK/RETRACTOR(RETRACTOR_ID(I))%FAC(1)        
                  XX = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),YY)*RETRACTOR(RETRACTOR_ID(I))%FAC(2)
                  RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL = RETRACTOR(RETRACTOR_ID(I))%FAC(2)*XX
                  RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV = -1
                  RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL = ZERO
                ELSEIF (RETRACTOR(RETRACTOR_ID(I))%LOCKED == 1) THEN
                  YY = F_LOCK/RETRACTOR(RETRACTOR_ID(I))%FAC(1)        
                  XX = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),YY)*RETRACTOR(RETRACTOR_ID(I))%FAC(2)
                  DELTA_LO = XX - RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL
                  EPSN = (ALDP(I)-AL0DP(I)-DELTA_LO)/MAX(LMIN(I),AL0DP(I)+DELTA_LO)
                  F_UNLOCK = MAX(FX(I) + XK(I)*(EPSN-EPS1),PRETENS)
                  FL_IMPOSED_FORCE = 1
                ELSE
                  F_UNLOCK = MAX(RETRACTOR(RETRACTOR_ID(I))%UNLOCK_FORCE,PRETENS)
                ENDIF
              ELSEIF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 4) THEN
C--             Pretensioning with force -> F = F_retractor + F_pretens
                IF (RETRACTOR(RETRACTOR_ID(I))%LOCKED == 1) THEN
                  F_LOCK = F_LOCK - PRETENS
                  DD_PRETENS = PRETENS
                ENDIF
                F_UNLOCK = RETRACTOR(RETRACTOR_ID(I))%UNLOCK_FORCE + PRETENS
              ELSEIF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 5) THEN
C--             Pretensioning with pullin energy
                IF (RETRACTOR(RETRACTOR_ID(I))%FORCE > ZERO) THEN
C--               Pretensioning deactivated when force is reached
                  IF (F_LOCK > RETRACTOR(RETRACTOR_ID(I))%FORCE) THEN
                    RETRACTOR(RETRACTOR_ID(I))%LOCKED = 1
                    YY = F_LOCK/RETRACTOR(RETRACTOR_ID(I))%FAC(1)        
                    XX = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),YY)
                    RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL = RETRACTOR(RETRACTOR_ID(I))%FAC(2)*XX
                    RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV = -1
                    RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL = ZERO
                  ENDIF
                ENDIF
              ENDIF
            ENDIF          
C
C--------------------Retractor is Locked ---------------------------------
            FLAG_NO_COUNT_PULL = 0
            IF ((RETRACTOR(RETRACTOR_ID(I))%LOCKED == 1).AND.(FL_IMPOSED_FORCE /= 1)) THEN
              IF (F_LOCK <= RETRACTOR(RETRACTOR_ID(I))%UNLOCK_FORCE) THEN
                RETRACTOR(RETRACTOR_ID(I))%LOCK_YIELD_FORCE = ZERO
                FLAG_NO_COUNT_PULL = 1
              ELSEIF (F_LOCK > RETRACTOR(RETRACTOR_ID(I))%LOCK_YIELD_FORCE) THEN
C--             Loading --  
                XX = RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL/RETRACTOR(RETRACTOR_ID(I))%FAC(2)
                YY = GET_U_FUNC(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),XX,DXDY)*RETRACTOR(RETRACTOR_ID(I))%FAC(1)
                DXDY = DXDY * (RETRACTOR(RETRACTOR_ID(I))%FAC(2) / RETRACTOR(RETRACTOR_ID(I))%FAC(1))
                YY = YY + PRETENS
                CALL SOLVE_DELTA_L02(DELTA_LO,DXDY,YY,XK(I),ALDP(I),AL0DP(I),LMIN(I))
                EPSN = (ALDP(I)-AL0DP(I)-DELTA_LO)/MAX(LMIN(I),AL0DP(I)+DELTA_LO)
                FB = FX(I) + XK(I)*(EPSN-EPS1)
                RETRACTOR(RETRACTOR_ID(I))%LOCK_YIELD_FORCE = FB
                RETRACTOR(RETRACTOR_ID(I))%LOCK_OFFSET = ZERO
              ELSE
C--             Unloading --
                IF (RETRACTOR(RETRACTOR_ID(I))%LOCK_OFFSET == ZERO) THEN
                  YY = RETRACTOR(RETRACTOR_ID(I))%LOCK_YIELD_FORCE/RETRACTOR(RETRACTOR_ID(I))%FAC(1)
                  XX1 = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(1),YY)       
                  XX2 = GET_U_FUNC_INV(RETRACTOR(RETRACTOR_ID(I))%IFUNC(2),YY)
                  RETRACTOR(RETRACTOR_ID(I))%LOCK_OFFSET = RETRACTOR(RETRACTOR_ID(I))%FAC(2)*(XX1-XX2)
                ENDIF
                XX = (RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL-RETRACTOR(RETRACTOR_ID(I))%LOCK_OFFSET)/RETRACTOR(RETRACTOR_ID(I))%FAC(2)
                YY = GET_U_FUNC(RETRACTOR(RETRACTOR_ID(I))%IFUNC(2),XX,DXDY)*RETRACTOR(RETRACTOR_ID(I))%FAC(1)
                DXDY = DXDY * (RETRACTOR(RETRACTOR_ID(I))%FAC(2) / RETRACTOR(RETRACTOR_ID(I))%FAC(1))
                YY = YY + PRETENS
                CALL SOLVE_DELTA_L02(DELTA_LO,DXDY,YY,XK(I),ALDP(I),AL0DP(I),LMIN(I))
                EPSN = (ALDP(I)-AL0DP(I)-DELTA_LO)/MAX(LMIN(I),AL0DP(I)+DELTA_LO)
                FB = FX(I) + XK(I)*(EPSN-EPS1)
              ENDIF
            ENDIF
C
C--------------------Retractor is Unlocked ---------------------------------
            IF ((RETRACTOR(RETRACTOR_ID(I))%LOCKED == 0).OR.
     .          (F_LOCK <= RETRACTOR(RETRACTOR_ID(I))%UNLOCK_FORCE).OR.
     .          (FL_IMPOSED_FORCE == 1)) THEN
                 DELTA_LO = (ALDP(I)-AL0DP(I)*(TENS + ONE))/(TENS + ONE)
                 DELTA_LO = (ALDP(I)-AL0DP(I)*(TENS + ONE))/(TENS + ONE)
C
              TENS = (F_UNLOCK - FB) / XK(I) + EPS1
              DELTA_LO = (ALDP(I)-AL0DP(I)*(TENS + ONE))/(TENS + ONE)
C
              IF (AL0DP(I) +  DELTA_LO < LMIN(I)) THEN
                LMIN(I) = MAX(LMIN(I),AL0(I))
                DELTA_LO = ALDP(I)-AL0DP(I)-XL0(I)*TENS
              ENDIF
C
            ENDIF
C
C--------------------Pretensionning - imposed pullin with load limiter-----
            IF (RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV == 1) THEN
              IF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 1) THEN
                IF (RETRACTOR(RETRACTOR_ID(I))%FORCE > ZERO) THEN
                  DELTA_LO = -PRETENS + RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL
                  RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL = PRETENS
                ENDIF                
              ELSEIF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 5) THEN
                IF (RETRACTOR(RETRACTOR_ID(I))%FORCE > ZERO) THEN
                  DELTA_LO = (-PRETENS + RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL)/FB
                  RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL = PRETENS
                ENDIF
              ENDIF
            ENDIF
C
C--------------------Limitation of flow rate ---------------------------------          
C-
            IF (DELTA_LO > ZERO) THEN
              DELTA_LO = MIN(DELTA_LO,0.01*RETRACTOR(RETRACTOR_ID(I))%ELEMENT_SIZE)
            ELSEIF (DELTA_LO < ZERO) THEN
              DELTA_LO = MAX(DELTA_LO,-0.01*RETRACTOR(RETRACTOR_ID(I))%ELEMENT_SIZE)
            ENDIF
C
C--------------------Detection of locking -----------------------------------          
C--
            FLAG_SENSOR_LOCK = 0
            IF (RETRACTOR(RETRACTOR_ID(I))%ISENS(1) > 0) THEN
              IF (TT > SENSOR(2,RETRACTOR(RETRACTOR_ID(I))%ISENS(1))) THEN
C--             lock sensor activated
                FLAG_SENSOR_LOCK = 1
                IF (RETRACTOR(RETRACTOR_ID(I))%LOCKED == 0) THEN 
                  IF (RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL >= RETRACTOR(RETRACTOR_ID(I))%PULLOUT) THEN
                    RETRACTOR(RETRACTOR_ID(I))%LOCKED = 1
C--                 rest of lockpull - counted from time of locking
                    RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL = ZERO
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
C
            IF (FLAG_NO_COUNT_PULL == 0) THEN
              IF ((RETRACTOR(RETRACTOR_ID(I))%LOCKED == 1).OR.(FLAG_SENSOR_LOCK == 1)) THEN
                RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL = RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL + DELTA_LO
              ELSE
                RETRACTOR(RETRACTOR_ID(I))%LOCK_PULL = ZERO
              ENDIF
            ENDIF
C
C--------------------Pretensionning - count of pretens pull-----
            IF (RETRACTOR(RETRACTOR_ID(I))%PRETENS_ACTIV == 1) THEN
              IF (RETRACTOR(RETRACTOR_ID(I))%TENS_TYP == 3) THEN
                RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL = RETRACTOR(RETRACTOR_ID(I))%PRETENS_PULL + DELTA_LO
              ENDIF
            ENDIF
C
C--------------------------------------------------------------
C
            IF (ABS(DELTA_LO) > ZERO) THEN
C
              AL0DP(I) = AL0DP(I) +  DELTA_LO
              AL0(I) = AL0DP(I)
              XL0(I)=MAX(LMIN(I),AL0(I))
C
              DDX =  EXDP(I) * (VX2(I) - VX1(I))
     .             + EYDP(I) * (VY2(I) - VY1(I))
     .             + EZDP(I) * (VZ2(I) - VZ1(I))
C
              EPSN = (ALDP(I)-AL0DP(I))/MAX(AL0(I),LMIN(I))
              DDF(I) = XK(I)*(EPSN-EPS1)

C---          if XSCAL < 0.9 N2 has crossed AC -> update of retractor
              XSCAL = -EXDP(I)*RETRACTOR(RETRACTOR_ID(I))%VECTOR(1)*RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION
     .                -EYDP(I)*RETRACTOR(RETRACTOR_ID(I))%VECTOR(2)*RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION
     .                -EZDP(I)*RETRACTOR(RETRACTOR_ID(I))%VECTOR(3)*RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION
C
              RETRACTOR(RETRACTOR_ID(I))%VECTOR(1) = -EXDP(I)*RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION
              RETRACTOR(RETRACTOR_ID(I))%VECTOR(2) = -EYDP(I)*RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION
              RETRACTOR(RETRACTOR_ID(I))%VECTOR(3) = -EZDP(I)*RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION
C     
C-------- Detection of retractor update---------------------------------
C
              IF (RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION == 1) THEN    
                NEW_NODE2 = ADD_NODE2(I)
              ELSE
                NEW_NODE2 = ADD_NODE1(I)
              ENDIF
C
              RING_SLIP(I) =RING_SLIP(I) + DELTA_LO
              RETRACTOR(RETRACTOR_ID(I))%RINGSLIP = RETRACTOR(RETRACTOR_ID(I))%RINGSLIP  + DELTA_LO
C
              IF (UPDATE_FLAG(I) == 0) THEN
                IF ((RING_SLIP(I) > RETRACTOR(RETRACTOR_ID(I))%ELEMENT_SIZE).AND.(NEW_NODE2 > 0)) THEN
C--             new element to be released - (if no next node it's the end of the belt - no switch)
                  RETRACTOR(RETRACTOR_ID(I))%UPDATE = 2
                  RETRACTOR(RETRACTOR_ID(I))%MATERIAL_FLOW = DELTA_LO/DT11
                  IF (RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION  == 1) THEN 
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(1) = NC2(I)
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(2) = ADD_NODE2(I)
                  ELSE
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(1) = NC1(I)
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(2) = ADD_NODE1(I)
                  ENDIF
                ELSEIF ((ALDP(I) + 1.3*DDX*DT1 < ZERO).OR.(XSCAL < 0.5)) THEN
C--             element to enter the retractor
                  RETRACTOR(RETRACTOR_ID(I))%UPDATE = -2
                  RETRACTOR(RETRACTOR_ID(I))%MATERIAL_FLOW = DELTA_LO/DT11
                  IF (RETRACTOR(RETRACTOR_ID(I))%STRAND_DIRECTION  == 1) THEN 
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(1) = ADD_NODE1(I)
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(2) = NC1(I)
                  ELSE
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(1) = ADD_NODE2(I)
                    RETRACTOR(RETRACTOR_ID(I))%NODE_NEXT(2) = NC2(I)
                  ENDIF
                ENDIF
              ENDIF
C
            ENDIF
C
        ENDIF
C
        ENDIF
C
      ENDDO
 
C----------------------------------------------------------
C

C----------------------------------------------------------      
C
      RETURN
                
      END

Chd|====================================================================
Chd|  SOLVE_DELTA_L0                src/seatbelts/material_flow.F 
Chd|-- called by -----------
Chd|        MATERIAL_FLOW                 src/seatbelts/material_flow.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SOLVE_DELTA_L0(FLAG,DELTA_LO,HH,L1,L2,L01,L02,LMIN,OMEGA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FLAG
      my_real
     .        HH,LMIN,OMEGA
      DOUBLE PRECISION DELTA_LO,L1,L2,L01,L02
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        AA,BB,CC,DD,SOL1,SOL2
C-----------------------------------------------
C
C      Computation of Dl0 for the slipring
C
C      DELTA_LO = -(HH*(ALDP2(I)-AL0DP2(I))*AL0DP(I)-(ALDP(I)-AL0DP(I))*AL0DP2(I))
C     .              /(HH*(ALDP2(I)-AL0DP2(I)+AL0DP(I))+ALDP(I)-AL0DP(I)+AL0DP2(I))
C
      IF ((HH == ONE).AND.(FLAG == 0)) THEN
C--   equation 1 -> no friction and l2 > lmin - order 1
        BB = -L1+L01-L02-HH*(L2-L02+L01) + OMEGA*(L01-L02)
        CC = (L1-L01)*L02-HH*(L2-L02)*L01 - OMEGA*(L02*L01) 
        DELTA_LO = -CC/BB
      ELSEIF (FLAG == 0) THEN
C--   equation 2 -> friction and l2 and l1 > lmin - order 2
        AA = ONE -HH + OMEGA
        BB = -L1+L01-L02-HH*(L2-L02+L01) + OMEGA*(L01-L02)
        CC = (L1-L01)*L02-HH*(L2-L02)*L01 - OMEGA*(L02*L01)
        DD = BB*BB-4*AA*CC
        SOL1 = (-BB+SQRT(DD))/(2*AA)
        SOL2 = (-BB-SQRT(DD))/(2*AA)
        DELTA_LO = SOL2
      ELSEIF (FLAG == 1) THEN
C--   equation 3 -> friction and l2 = lmin - order 2
        AA = -HH
        BB = -LMIN -HH*(L2-L02+L01) - OMEGA*LMIN
        CC = (L1-L01)*LMIN-HH*(L2-L02)*L01 - OMEGA*LMIN*L01
        DD = BB*BB-4*AA*CC
        SOL1 = (-BB+SQRT(DD))/(2*AA)
        SOL2 = (-BB-SQRT(DD))/(2*AA)
        DELTA_LO = SOL2
      ELSEIF (FLAG == 2) THEN
C--   equation 4 -> friction and l1 = lmin - order 2
        AA = ONE
        BB = L01-L02-L1-HH*LMIN + OMEGA*LMIN
        CC = L1*L02-L01*L02-HH*LMIN*(L2-L02) - OMEGA*LMIN*L02
        DD = BB*BB-4*AA*CC
        SOL1 = (-BB+SQRT(DD))/(2*AA)
        SOL2 = (-BB-SQRT(DD))/(2*AA)
        DELTA_LO = SOL2
      ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  SOLVE_DELTA_L02               source/tools/seatbelts/material_flow.F
Chd|-- called by -----------
Chd|        MATERIAL_FLOW                 source/tools/seatbelts/material_flow.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SOLVE_DELTA_L02(DELTA_LO,ALPHA,BETA,XK,L,L0,LMIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        ALPHA,BETA,XK,LMIN
      DOUBLE PRECISION DELTA_LO,L,L0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        DD,SOL1,SOL2,AA,BB,CC
C-----------------------------------------------
C
C      Computation of Dl0 for locked retractor
C
C--    equation 1 -> l0 > lmin
       AA = ALPHA
       BB = ALPHA*L0+BETA+XK
       CC = BETA*L0 - XK*(L-L0)
       IF (ABS(AA) > EM20) THEN
         DD = BB*BB-4*AA*CC
         SOL1 = (-BB+SQRT(DD))/(2*AA)
         SOL2 = (-BB-SQRT(DD))/(2*AA)
         DELTA_LO = SOL1
       ELSE
         DELTA_LO = -CC / MAX(EM20,BB)
       ENDIF
C
       IF (L0 +  DELTA_LO < LMIN) THEN
C--      equation 2 -> l0 < lmin
         LMIN = MAX(LMIN,L0)
         BB = ALPHA*LMIN + XK
         DELTA_LO = (XK*(L-L0) - BETA*LMIN)/MAX(EM20,BB)
       ENDIF

      RETURN
      END
