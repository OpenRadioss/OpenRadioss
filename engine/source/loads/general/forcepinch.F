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
Chd|  FORCEPINCH                    source/loads/general/forcepinch.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE FORCEPINCH (IB     ,FAC     ,NPC    ,TF      ,A     ,
     2                  V      ,X       ,SKEW   ,AR      ,VR    ,
     3                  SENSOR ,LSENSOR ,WEIGHT ,TFEXC   ,IADC  ,
     4                  FSKY   ,FSKYV   ,FEXT   ,H3D_DATA,
     5                  APINCH ,VPINCH)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
      USE PINCHTYPE_MOD  
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "alefvm.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER  GET_U_NUMSENS,GET_U_SENS_FPAR,GET_U_SENS_IPAR,
     .         GET_U_SENS_VALUE,SET_U_SENS_VALUE
      EXTERNAL GET_U_NUMSENS,GET_U_SENS_FPAR,GET_U_SENS_IPAR,
     .         GET_U_SENS_VALUE,SET_U_SENS_VALUE
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*)
      INTEGER IB(NIBCLD,*),LSENSOR(NSENSI,*)
      INTEGER WEIGHT(*), IADC(4,*)
      my_real
     .   FAC(LFACCLD,*), TF(*), A(3,*), V(3,*), AR(3,*), VR(3,*),
     .   X(3,*), SKEW(LSKEW,*), SENSOR(NSENSR,*),TFEXC,
     .   FSKY(8,LSKY), FSKYV(LSKY,8),FEXT(3,*),
     .   APINCH(3,*),VPINCH(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, K1, K2, K3, ISENS,K,LL,IERR,
     .        ICODE,IAD,N_OLD, Bric(2), Face(2), NumBric, IBRIC, IANIM,I,
     .        ISMOOTH
      my_real
     .   NX, NY, NZ, AXI, AA, A0, VV, FX, FY, FZ, AX, DYDX, TS,
     .   SIXTH,TFEXTT,X_OLD, F1, F2,XSENS,FCX,FCY
      my_real FINTER, ZFx,ZFy,ZFz, ZZFx,ZZFy,ZZFz,PS, Zx,Zy,Zz,FINTER_SMOOTH
      my_real 
     .    FCYPINCH, FXPINCH, FYPINCH, FZPINCH, AAPINCH
      EXTERNAL FINTER,FINTER_SMOOTH

C=======================================================================
      SIXTH  = ONE_OVER_6
      TFEXC  = ZERO
      TFEXTT = ZERO
      N_OLD  = 0
      X_OLD  = ZERO
      IANIM  = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .         ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT
C
       DO 10 NL=NCONLD-NPLOADPINCH+1,NCONLD
       N1      = IB(1,NL)
       N2      = IB(2,NL)
       N3      = IB(3,NL)
       N4      = IB(4,NL)
       N5      = IB(5,NL)
       FCY     = FAC(1,NL)
       FCX     = FAC(2,NL)
       FCYPINCH= FAC(1,NL)
C
       ISENS   = 0
       XSENS   = ONE
       DO K=1,NSENSOR
         IF(IB(6,NL)==LSENSOR(1,K))ISENS=K
       ENDDO
       IF(ISENS==0)THEN
          TS=TT
       ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN
         IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)
         IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)
       ELSE
          TS = TT-SENSOR(2,ISENS)
          IF(TS < ZERO) GOTO 10
       ENDIF
C----------------
C       PINCHING PRESSURE
C----------------
        IF(N_OLD/=N5.OR.X_OLD/=TS) THEN

          ISMOOTH = 0
          IF (N5 > 0) ISMOOTH = NPC(2*NFUNCT+N5+1)
!!          F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
          IF (ISMOOTH == 0) THEN
            F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
          ELSE
            F1 = FINTER_SMOOTH(N5,TS*FCX,NPC,TF,DYDX)
          ENDIF ! IF (ISMOOTH == 0)
          N_OLD = N5
          X_OLD = TS
        ENDIF
        AAPINCH = FCYPINCH*F1*XSENS
C
        IF(N4/=0)THEN
          NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
          NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
          NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
C
          FXPINCH = AAPINCH*NX*ONE_OVER_8
          FYPINCH = AAPINCH*NY*ONE_OVER_8
          FZPINCH = AAPINCH*NZ*ONE_OVER_8
C
          APINCH(1,N1) = APINCH(1,N1) + FXPINCH
          APINCH(2,N1) = APINCH(2,N1) + FYPINCH
          APINCH(3,N1) = APINCH(3,N1) + FZPINCH
C
          APINCH(1,N2) = APINCH(1,N2) + FXPINCH
          APINCH(2,N2) = APINCH(2,N2) + FYPINCH
          APINCH(3,N2) = APINCH(3,N2) + FZPINCH
C
          APINCH(1,N3) = APINCH(1,N3) + FXPINCH
          APINCH(2,N3) = APINCH(2,N3) + FYPINCH
          APINCH(3,N3) = APINCH(3,N3) + FZPINCH
C
          APINCH(1,N4) = APINCH(1,N4) + FXPINCH
          APINCH(2,N4) = APINCH(2,N4) + FYPINCH
          APINCH(3,N4) = APINCH(3,N4) + FZPINCH
C
C
          TFEXTT=TFEXTT+DT1*(FXPINCH*(VPINCH(1,N1)+VPINCH(1,N2)+VPINCH(1,N3)+VPINCH(1,N4))
     1                       +FYPINCH*(VPINCH(2,N1)+VPINCH(2,N2)+VPINCH(2,N3)+VPINCH(2,N4))
     2                       +FZPINCH*(VPINCH(3,N1)+VPINCH(3,N2)+VPINCH(3,N3)+VPINCH(3,N4)))

        ELSE ![PM] 3 nodes : triangles (not implemented at the moment)
         ! true triangles.
          NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
          NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
          NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
C
          FXPINCH = AAPINCH*NX*SIXTH
          FYPINCH = AAPINCH*NY*SIXTH
          FZPINCH = AAPINCH*NZ*SIXTH
C
          APINCH(1,N1)=APINCH(1,N1)+FXPINCH
          APINCH(2,N1)=APINCH(2,N1)+FYPINCH
          APINCH(3,N1)=APINCH(3,N1)+FZPINCH
C
          APINCH(1,N2)=APINCH(1,N2)+FXPINCH
          APINCH(2,N2)=APINCH(2,N2)+FYPINCH
          APINCH(3,N2)=APINCH(3,N2)+FZPINCH
C
          APINCH(1,N3)=APINCH(1,N3)+FXPINCH
          APINCH(2,N3)=APINCH(2,N3)+FYPINCH
          APINCH(3,N3)=APINCH(3,N3)+FZPINCH
C         
         ENDIF        
 10    CONTINUE
C
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
      RETURN
      END        
