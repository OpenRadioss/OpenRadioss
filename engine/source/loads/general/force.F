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
Chd|  FORCE                         source/loads/general/force.F  
Chd|-- called by -----------
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE FORCE (IB     ,FAC     ,NPC    ,TF      ,A     ,
     2                  V      ,X       ,SKEW   ,AR      ,VR    ,
     3                  SENSOR ,LSENSOR ,WEIGHT ,TFEXC   ,IADC  ,
     4                  FSKY   ,FSKYV   ,FEXT   ,H3D_DATA,
     5                  CPTREAC,FTHREAC,NODREAC)
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
      INTEGER NPC(*),CPTREAC,NODREAC(*)
      INTEGER IB(NIBCLD,*),LSENSOR(NSENSI,*)
      INTEGER WEIGHT(*), IADC(4,*)
      my_real
     .   FAC(LFACCLD,*), TF(*), A(3,*), V(3,*), AR(3,*), VR(3,*),
     .   X(3,*), SKEW(LSKEW,*), SENSOR(NSENSR,*),TFEXC,
     .   FSKY(8,LSKY), FSKYV(LSKY,8),FEXT(3,*),FTHREAC(6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, K1, K2, K3, ISENS,K,LL,IERR,
     .        ICODE,IAD,N_OLD, Bric(2), Face(2), NumBric, IBRIC, IANIM,I,
     .        ISMOOTH,IDEL
      INTEGER :: UP_BOUND
      my_real
     .   NX, NY, NZ, AXI, AA, A0, VV, FX, FY, FZ, AX, DYDX, TS,
     .   SIXTH,TFEXTT,X_OLD, F1, F2,XSENS,FCX,FCY,FCYPINCH
      my_real FINTER, ZFx,ZFy,ZFz, ZZFx,ZZFy,ZZFz,PS, Zx,Zy,Zz,FINTER_SMOOTH
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
      IF(IMACH/=3.OR.IPARIT==0) THEN
C code SPMD Parith/OFF ou SMP NE PAS OUBLIER LE CODE P/ON !
       ! en parith/on pour le spmd il faut calculer sur chaque proc
       ! en parith/off pour le spmd il ne faut calculer qu'une fois
       ! noeud servant sur ce proc
       DO 10 NL=1,NCONLD-NPLOADPINCH
       N1      = IB(1,NL)
       N2      = IB(2,NL)
       N3      = IB(3,NL)
       N4      = IB(4,NL)
       N5      = IB(5,NL)
       IDEL    = IB(8,NL)
       FCY     = FAC(1,NL)
       FCX     = FAC(2,NL)
C
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
C
       IF(N4==-1)THEN
C----------------
C       FORCE CONCENTREE
C----------------
        IF(N_OLD/=N3.OR.X_OLD/=TS) THEN
          ISMOOTH = 0
          IF (N3 > 0) ISMOOTH = NPC(2*NFUNCT+N3+1)
!!          F1    = FINTER(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
!!          F2    = FINTER(N3,TS*FCX,NPC,TF,DYDX)
          IF (ISMOOTH == 0) THEN
            F1 = FINTER(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
            F2 = FINTER(N3,TS*FCX,NPC,TF,DYDX)
          ELSE
            F1 = FINTER_SMOOTH(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
            F2 = FINTER_SMOOTH(N3,TS*FCX,NPC,TF,DYDX)
          ENDIF ! IF (ISMOOTH == 0)
          N_OLD = N3
          X_OLD = TS
        ENDIF
        A0  = FCY*F1
        AA  = FCY*F2
        ISK = IB(2,NL)/10
        N2  = IB(2,NL)-10*ISK
        IF(N2<=3)THEN
         IF(N2D/=1)THEN
          AXI=ONE
         ELSE
          AXI=X(2,N1)
         ENDIF
         IF(ISK<=1)THEN
          A(N2,N1)=A(N2,N1)+AA
!
          IF (CPTREAC > 0)  THEN
            IF(NODREAC(N1)/=0) THEN
              FTHREAC(N2,NODREAC(N1)) = FTHREAC(N2,NODREAC(N1)) + AA*DT1
            ENDIF
          ENDIF
!
          IF(IANIM >0 .AND.IMPL_S==0) THEN
            FEXT(N2,N1)=FEXT(N2,N1)+AA
          ENDIF
           TFEXC = TFEXC+HALF*(A0+AA)*V(N2,N1)*AXI
         ELSE
          K1=3*N2-2
          K2=3*N2-1
          K3=3*N2
          VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
          A(1,N1)=A(1,N1)+SKEW(K1,ISK)*AA
          A(2,N1)=A(2,N1)+SKEW(K2,ISK)*AA
          A(3,N1)=A(3,N1)+SKEW(K3,ISK)*AA
          IF(IANIM >0 .AND.IMPL_S==0) THEN
            FEXT(1,N1) = FEXT(1,N1)+SKEW(K1,ISK)*AA
            FEXT(2,N1) = FEXT(2,N1)+SKEW(K2,ISK)*AA
            FEXT(3,N1) = FEXT(3,N1)+SKEW(K3,ISK)*AA
          ENDIF
!
!
          IF (CPTREAC > 0 ) THEN
            IF(NODREAC(N1)/=0) THEN
              FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + SKEW(K1,ISK)*AA*DT1
              FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + SKEW(K2,ISK)*AA*DT1
              FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + SKEW(K3,ISK)*AA*DT1
            ENDIF
          ENDIF!
!
         ENDIF
        ELSEIF(N2<=6)THEN
         N2 = N2 - 3
         IF(ISK<=1)THEN
          AR(N2,N1)=AR(N2,N1)+AA
          TFEXC=TFEXC+HALF*(A0+AA)*VR(N2,N1)
!
          IF (CPTREAC > 0) THEN
            IF(NODREAC(N1)/=0) THEN
              FTHREAC(N2+3,NODREAC(N1)) = FTHREAC(N2+3,NODREAC(N1)) + AA*DT1
            ENDIF
          ENDIF!
!
         ELSE
          K1       = 3*N2-2
          K2       = 3*N2-1
          K3       = 3*N2
          VV       = SKEW(K1,ISK)*VR(1,N1)+SKEW(K2,ISK)*VR(2,N1)+SKEW(K3,ISK)*VR(3,N1)
          AR(1,N1) = AR(1,N1)+SKEW(K1,ISK)*AA
          AR(2,N1) = AR(2,N1)+SKEW(K2,ISK)*AA
          AR(3,N1) = AR(3,N1)+SKEW(K3,ISK)*AA
!
          IF (CPTREAC > 0) THEN 
            IF(NODREAC(N1)/=0) THEN
              FTHREAC(4,NODREAC(N1)) = FTHREAC(4,NODREAC(N1)) + SKEW(K1,ISK)*AA*DT1
              FTHREAC(5,NODREAC(N1)) = FTHREAC(5,NODREAC(N1)) + SKEW(K2,ISK)*AA*DT1
              FTHREAC(6,NODREAC(N1)) = FTHREAC(6,NODREAC(N1)) + SKEW(K3,ISK)*AA*DT1
            ENDIF
          ENDIF
!
          TFEXC    = TFEXC+HALF*(A0+AA)*VV
         ENDIF
        ENDIF
C
       ELSE
C----------------
C       PRESSION
C----------------

        IF(IDEL > 0 ) CYCLE  ! SEGMENT DELETED

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
        AA = FCY*F1*XSENS
C
        IF(N2D==0)THEN
C        ANALYSE 3D
         IF(N4/=0)THEN
           NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
           NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
           NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
           FX = AA*NX*ONE_OVER_8
           FY = AA*NY*ONE_OVER_8
           FZ = AA*NZ*ONE_OVER_8
C
           A(1,N1)=A(1,N1)+FX
           A(2,N1)=A(2,N1)+FY
           A(3,N1)=A(3,N1)+FZ
           IF(IANIM >0 .AND.IMPL_S==0) THEN
             FEXT(1,N1) = FEXT(1,N1)+FX
             FEXT(2,N1) = FEXT(2,N1)+FY
             FEXT(3,N1) = FEXT(3,N1)+FZ
           ENDIF
!
          IF (CPTREAC > 0) THEN
            IF(NODREAC(N1)/=0) THEN
              FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
              FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
              FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
            ENDIF
          ENDIF
!
C
           A(1,N2)=A(1,N2)+FX
           A(2,N2)=A(2,N2)+FY
           A(3,N2)=A(3,N2)+FZ
           IF(IANIM >0 .AND.IMPL_S==0) THEN
             FEXT(1,N2) = FEXT(1,N2)+FX
             FEXT(2,N2) = FEXT(2,N2)+FY
             FEXT(3,N2) = FEXT(3,N2)+FZ
           ENDIF
!
          IF (CPTREAC > 0) THEN
            IF(NODREAC(N2)/=0) THEN
              FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
              FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
              FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
            ENDIF
          ENDIF
!
C
           A(1,N3)=A(1,N3)+FX
           A(2,N3)=A(2,N3)+FY
           A(3,N3)=A(3,N3)+FZ
           IF(IANIM >0 .AND.IMPL_S==0) THEN
             FEXT(1,N3) = FEXT(1,N3)+FX
             FEXT(2,N3) = FEXT(2,N3)+FY
             FEXT(3,N3) = FEXT(3,N3)+FZ
           ENDIF
!
          IF (CPTREAC > 0) THEN
            IF(NODREAC(N3)/=0) THEN
              FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
              FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
              FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
            ENDIF
          ENDIF!
!
C
           A(1,N4)=A(1,N4)+FX
           A(2,N4)=A(2,N4)+FY
           A(3,N4)=A(3,N4)+FZ
           IF(IANIM >0 .AND.IMPL_S==0) THEN
             FEXT(1,N4) = FEXT(1,N4)+FX
             FEXT(2,N4) = FEXT(2,N4)+FY
             FEXT(3,N4) = FEXT(3,N4)+FZ
           ENDIF
!
          IF (CPTREAC > 0) THEN
            IF (NODREAC(N4)/=0) THEN
              FTHREAC(1,NODREAC(N4)) = FTHREAC(1,NODREAC(N4)) + FX*DT1
              FTHREAC(2,NODREAC(N4)) = FTHREAC(2,NODREAC(N4)) + FY*DT1
              FTHREAC(3,NODREAC(N4)) = FTHREAC(3,NODREAC(N4)) + FZ*DT1
            ENDIF
          ENDIF
         
!
C
           TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                       +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                       +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
C
         ELSE
         ! true triangles.
          NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
          NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
          NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
          FX = AA*NX*SIXTH
          FY = AA*NY*SIXTH
          FZ = AA*NZ*SIXTH
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
          IF(IANIM >0  .AND.IMPL_S==0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
          ENDIF
!
          IF (CPTREAC > 0) THEN
            IF (NODREAC(N1)/=0) THEN
              FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
              FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
              FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
            ENDIF
          ENDIF
!
C
          A(1,N2) = A(1,N2)+FX
          A(2,N2) = A(2,N2)+FY
          A(3,N2) = A(3,N2)+FZ
          IF(IANIM >0 .AND.IMPL_S==0) THEN
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
          ENDIF
!
          IF (CPTREAC > 0) THEN 
             IF(NODREAC(N2)/=0) THEN
               FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
               FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
               FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
            ENDIF
          ENDIF
!
C
          A(1,N3) = A(1,N3)+FX
          A(2,N3) = A(2,N3)+FY
          A(3,N3) = A(3,N3)+FZ
          IF(IANIM >0  .AND.IMPL_S==0) THEN
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
          ENDIF
!
          IF (CPTREAC > 0 ) THEN
            IF (NODREAC(N3)/=0) THEN
              FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
              FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
              FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
            ENDIF
          ENDIF
!
C
          TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)) + FY*(V(2,N1)+V(2,N2)+V(2,N3)) + FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
          
         ENDIF
        ELSE
C        ANALYSE 2D
         NY      = -X(3,N2)+X(3,N1)
         NZ      =  X(2,N2)-X(2,N1)
         FY      = AA*NY*HALF
         FZ      = AA*NZ*HALF
C
         A(2,N1) = A(2,N1)+FY
         A(3,N1) = A(3,N1)+FZ
C
         A(2,N2) = A(2,N2)+FY
         A(3,N2) = A(3,N2)+FZ
C
         IF(IANIM >0 .AND.IMPL_S==0) THEN
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
c
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
         ENDIF
!
          IF (CPTREAC > 0) THEN
            IF(NODREAC(N1)/=0) THEN
              FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
              FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
            ENDIF
            IF (NODREAC(N2)/=0) THEN
              FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
              FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
            ENDIF
          ENDIF
!
C
         IF(N2D==1)THEN
          AX     = HALF*(X(2,N1)+X(2,N2))
         ELSE
          AX     = ONE
         ENDIF
         TFEXTT  = TFEXTT+DT1*(FY*(V(2,N1)+V(2,N2))+FZ*(V(3,N1)+V(3,N2)))*AX
         
        ENDIF
       ENDIF
 10    CONTINUE
C
#include "atomic.inc"
              TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
      ELSE
C-------------------------
C code SPMD Parith/ON
Code non vectoriel
C-------------------------
       IF(IVECTOR==0) THEN
        DO 100 NL=1,NCONLD
         N1  = IB(1,NL)
         N2  = IB(2,NL)
         N3  = IB(3,NL)
         N4  = IB(4,NL)
         N5  = IB(5,NL)
         IDEL = IB(8,NL)
         FCY = FAC(1,NL)
         FCX = FAC(2,NL)
         UP_BOUND = 4
         ! -------------
         ! flush sky array to 0.
         IF(N4==-1) THEN
            UP_BOUND = 1
         ELSE
            IF(N2D==0)THEN
                !  3D
                IF(N4/=0)THEN
                    UP_BOUND = 4
                ELSE
                    UP_BOUND = 3
                ENDIF
            ELSE
                ! 2D
                UP_BOUND = 2
            ENDIF
         ENDIF
         FSKY(1:8,IADC(1:UP_BOUND,NL))=ZERO
         ! -------------

         ISENS=0
         DO K=1,NSENSOR
           IF(IB(6,NL)==LSENSOR(1,K))ISENS=K
         ENDDO
         IF(ISENS==0)THEN
           TS=TT
         ELSE
           TS = TT-SENSOR(2,ISENS)
           IF(TS<ZERO)GOTO 100
         ENDIF
C
         IF(N4==-1)THEN
C-------------------------
C        FORCE CONCENTREE
C-------------------------
           IF(N_OLD/=N3.OR.X_OLD/=TS) THEN
             ISMOOTH = 0
             IF (N3 > 0) ISMOOTH = NPC(2*NFUNCT+N3+1)
!!             F1    = FINTER(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
!!             F2    = FINTER(N3,TS*FCX,NPC,TF,DYDX)
             IF (ISMOOTH == 0) THEN
               F1 = FINTER(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
               F2 = FINTER(N3,TS*FCX,NPC,TF,DYDX)
             ELSE
               F1 = FINTER_SMOOTH(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
               F2 = FINTER_SMOOTH(N3,TS*FCX,NPC,TF,DYDX)
             ENDIF ! IF (ISMOOTH == 0)
             N_OLD = N3
             X_OLD = TS
           ENDIF
           A0  = FCY*F1
           AA  = FCY*F2
           ISK = IB(2,NL)/10
           N2  = IB(2,NL)-10*ISK
           IF(N2<=3)THEN
             IF(N2D/=1)THEN
               AXI=ONE
             ELSE
               AXI=X(2,N1)
             ENDIF
             IF(ISK<=1)THEN
               FSKY(N2,IADC(1,NL)) = AA
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(N2,N1)=FEXT(N2,N1)+AA
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(N2,NODREAC(N1)) = FTHREAC(N2,NODREAC(N1)) + AA*DT1
                ENDIF
               ENDIF
!
               TFEXC = TFEXC+HALF*(A0+AA)*V(N2,N1)*AXI
             ELSE
               K1  = 3*N2-2
               K2  = 3*N2-1
               K3  = 3*N2
               VV  = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+ SKEW(K3,ISK)*V(3,N1)
               IAD = IADC(1,NL)
               FSKY(1,IAD)  = SKEW(K1,ISK)*AA
               FSKY(2,IAD)  = SKEW(K2,ISK)*AA
               FSKY(3,IAD)  = SKEW(K3,ISK)*AA
               IF(IANIM >0  .AND.IMPL_S==0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+SKEW(K1,ISK)*AA
                 FEXT(2,N1) = FEXT(2,N1)+SKEW(K2,ISK)*AA
                 FEXT(3,N1) = FEXT(3,N1)+SKEW(K3,ISK)*AA
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + SKEW(K1,ISK)*AA*DT1
                   FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + SKEW(K2,ISK)*AA*DT1
                   FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + SKEW(K3,ISK)*AA*DT1
                 ENDIF
               ENDIF
!
               TFEXC = TFEXC+HALF*(A0+AA)*VV*AXI
             ENDIF
           ELSEIF(N2<=6)THEN
             N2 = N2 - 3
             IF(ISK<=1)THEN
               FSKY(3+N2,IADC(1,NL)) = AA
!
               IF (CPTREAC > 0) THEN 
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(N2+3,NODREAC(N1)) = FTHREAC(N2+3,NODREAC(N1)) + AA*DT1
                 ENDIF
               ENDIF
!
               TFEXC=TFEXC+HALF*(A0+AA)*VR(N2,N1)
             ELSE
               K1  = 3*N2-2
               K2  = 3*N2-1
               K3  = 3*N2
               VV  = SKEW(K1,ISK)*VR(1,N1)+SKEW(K2,ISK)*VR(2,N1)+ SKEW(K3,ISK)*VR(3,N1)
               IAD = IADC(1,NL)
               FSKY(4,IAD) = SKEW(K1,ISK)*AA
               FSKY(5,IAD) = SKEW(K2,ISK)*AA
               FSKY(6,IAD) = SKEW(K3,ISK)*AA
!
               IF (CPTREAC > 0) THEN 
                IF(NODREAC(N1)/=0) THEN
                 FTHREAC(4,NODREAC(N1)) = FTHREAC(4,NODREAC(N1)) + SKEW(K1,ISK)*AA*DT1
                 FTHREAC(5,NODREAC(N1)) = FTHREAC(5,NODREAC(N1)) + SKEW(K2,ISK)*AA*DT1
                 FTHREAC(6,NODREAC(N1)) = FTHREAC(6,NODREAC(N1)) + SKEW(K3,ISK)*AA*DT1
                ENDIF
               ENDIF
!
               TFEXC=TFEXC+HALF*(A0+AA)*VV
             ENDIF
           ENDIF
C
         ELSE
C--------------------
C         PRESSION
C--------------------

           IF(IDEL > 0 ) CYCLE  ! SEGMENT DELETED

           IF(N_OLD/=N5.OR.X_OLD/=TS) THEN
             ISMOOTH = 0
             IF (N5 > 0) ISMOOTH = NPC(2*NFUNCT+N5+1)
!!             F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
             IF (ISMOOTH == 0) THEN
               F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
             ELSE
               F1 = FINTER_SMOOTH(N5,TS*FCX,NPC,TF,DYDX)
             ENDIF ! IF (ISMOOTH == 0)
             N_OLD = N5
             X_OLD = TS
           ENDIF
           AA = FCY*F1
           IF(N2D==0)THEN
             !  3D
             IF(N4/=0)THEN
               NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
               NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
               NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
C
               FX = AA*NX*ONE_OVER_8
               FY = AA*NY*ONE_OVER_8
               FZ = AA*NZ*ONE_OVER_8
C
               IAD = IADC(1,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN 
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
                   FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
                   FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(2,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0  .AND.IMPL_S==0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN 
                 IF(NODREAC(N2)/=0) THEN
                   FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
                   FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
                   FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(3,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N3)/=0) THEN
                   FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
                   FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
                   FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(4,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0  .AND.IMPL_S==0) THEN
                 FEXT(1,N4) = FEXT(1,N4)+FX
                 FEXT(2,N4) = FEXT(2,N4)+FY
                 FEXT(3,N4) = FEXT(3,N4)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N4)/=0) THEN
                   FTHREAC(1,NODREAC(N4)) = FTHREAC(1,NODREAC(N4)) + FX*DT1
                   FTHREAC(2,NODREAC(N4)) = FTHREAC(2,NODREAC(N4)) + FY*DT1
                   FTHREAC(3,NODREAC(N4)) = FTHREAC(3,NODREAC(N4)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                           +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                           +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))

             ELSE
               ! true triangles.
               NX  = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
               NY  = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
               NZ  = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
               FX  = AA*NX*SIXTH
               FY  = AA*NY*SIXTH
               FZ  = AA*NZ*SIXTH
C
               IAD = IADC(1,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
                   FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
                   FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(2,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N2)/=0) THEN
                   FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
                   FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
                   FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(3,NL)
               FSKY(1,IAD) = FX
               FSKY(2,IAD) = FY
               FSKY(3,IAD) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN 
                 IF(NODREAC(N3)/=0) THEN
                   FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
                   FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
                   FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)) + FY*(V(2,N1)+V(2,N2)+V(2,N3)) + FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
             ENDIF  
           ELSE
C        ANALYSE 2D
             NY  = -X(3,N2)+X(3,N1)
             NZ  =  X(2,N2)-X(2,N1)
C
             FY  = AA*NY*HALF
             FZ  = AA*NZ*HALF
C
             IAD = IADC(1,NL)
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
C
             IAD = IADC(2,NL)
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
C
             IF(IANIM >0 .AND.IMPL_S==0) THEN
               FEXT(2,N1) = FEXT(2,N1)+FY
               FEXT(3,N1) = FEXT(3,N1)+FZ
c
               FEXT(2,N2) = FEXT(2,N2)+FY
               FEXT(3,N2) = FEXT(3,N2)+FZ
             ENDIF
!
             IF (CPTREAC > 0) THEN
               IF(NODREAC(N1)/=0) THEN
                 FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
                 FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
               ENDIF
               IF (NODREAC(N2)/=0) THEN
                 FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
                 FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
               ENDIF
             ENDIF
!
C
             IF(N2D==1)THEN
               AX=HALF*(X(2,N1)+X(2,N2))
             ELSE
               AX=ONE
             ENDIF
             TFEXTT=TFEXTT+DT1*(FY*(V(2,N1)+V(2,N2))+FZ*(V(3,N1)+V(3,N2)))*AX
           ENDIF
         ENDIF
 100    CONTINUE
       ELSE
C---------------------
C code vectoriel
C---------------------
        DO 1000 NL=1,NCONLD
         N1  = IB(1,NL)
         N2  = IB(2,NL)
         N3  = IB(3,NL)
         N4  = IB(4,NL)
         N5  = IB(5,NL)
         IDEL = IB(8,NL)
         FCY = FAC(1,NL)
         FCX = FAC(2,NL)
         ISENS=0
         DO K=1,NSENSOR
           IF(IB(6,NL)==LSENSOR(1,K))ISENS=K
         ENDDO
         IF(ISENS==0)THEN
           TS=TT
         ELSE
           TS = TT-SENSOR(2,ISENS)
           IF(TS<ZERO)GOTO 1000
         ENDIF
C
         IF(N4==-1)THEN
C---------------------
C          FORCE CONCENTREE
C---------------------
           IF(N_OLD/=N3.OR.X_OLD/=TS) THEN
             ISMOOTH = 0
             IF (N3 > 0) ISMOOTH = NPC(2*NFUNCT+N3+1)
!!             F1    = FINTER(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
!!             F2    = FINTER(N3,TS*FCX,NPC,TF,DYDX)
             IF (ISMOOTH == 0) THEN
               F1 = FINTER(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
               F2 = FINTER(N3,TS*FCX,NPC,TF,DYDX)
             ELSE
               F1 = FINTER_SMOOTH(N3,(TS-DT1)*FCX,NPC,TF,DYDX)
               F2 = FINTER_SMOOTH(N3,TS*FCX,NPC,TF,DYDX)
             ENDIF ! IF (ISMOOTH == 0)
             N_OLD = N3
             X_OLD = TS
           ENDIF
           A0  = FCY*F1
           AA  = FCY*F2
           ISK = IB(2,NL)/10
           N2=IB(2,NL)-10*ISK
           IF(N2<=3)THEN
             IF(N2D/=1)THEN
               AXI=ONE
             ELSE
               AXI=X(2,N1)
             ENDIF
             IF(ISK<=1)THEN
               FSKYV(IADC(1,NL),N2) = AA
               IF(IANIM >0  .AND.IMPL_S==0) THEN
                 FEXT(N2,N1)=FEXT(N2,N1)+AA
               ENDIF
!
               IF (CPTREAC > 0) THEN 
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(N2,NODREAC(N1)) = FTHREAC(N2,NODREAC(N1)) + AA*DT1
                 ENDIF
               ENDIF
!
               TFEXC = TFEXC+HALF*(A0+AA)*V(N2,N1)*AXI
             ELSE
               K1  = 3*N2-2
               K2  = 3*N2-1
               K3  = 3*N2
               VV  = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
               IAD = IADC(1,NL)
               FSKYV(IAD,1) = SKEW(K1,ISK)*AA
               FSKYV(IAD,2) = SKEW(K2,ISK)*AA
               FSKYV(IAD,3) = SKEW(K3,ISK)*AA
               IF(IANIM >0  .AND.IMPL_S==0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+SKEW(K1,ISK)*AA
                 FEXT(2,N1) = FEXT(2,N1)+SKEW(K2,ISK)*AA
                 FEXT(3,N1) = FEXT(3,N1)+SKEW(K3,ISK)*AA
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + SKEW(K1,ISK)*AA*DT1
                   FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + SKEW(K2,ISK)*AA*DT1
                   FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + SKEW(K3,ISK)*AA*DT1
                 ENDIF
               ENDIF
!
               TFEXC = TFEXC+HALF*(A0+AA)*VV*AXI
             ENDIF
           ELSEIF(N2<=6)THEN
             N2 = N2 - 3
             IF(ISK<=1)THEN
               FSKYV(IADC(1,NL),3+N2) = AA
!
               IF (CPTREAC > 0 ) THEN
                 IF( NODREAC(N1)/=0) THEN
                   FTHREAC(N2+3,NODREAC(N1)) = FTHREAC(N2+3,NODREAC(N1)) + AA*DT1
                 ENDIF
               ENDIF
!
               TFEXC=TFEXC+HALF*(A0+AA)*VR(N2,N1)
             ELSE
               K1  = 3*N2-2
               K2  = 3*N2-1
               K3  = 3*N2
               VV  = SKEW(K1,ISK)*VR(1,N1) + SKEW(K2,ISK)*VR(2,N1)+ SKEW(K3,ISK)*VR(3,N1)
               IAD = IADC(1,NL)
               FSKYV(IAD,4) = SKEW(K1,ISK)*AA
               FSKYV(IAD,5) = SKEW(K2,ISK)*AA
               FSKYV(IAD,6) = SKEW(K3,ISK)*AA
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(4,NODREAC(N1)) = FTHREAC(4,NODREAC(N1)) + SKEW(K1,ISK)*AA*DT1
                   FTHREAC(5,NODREAC(N1)) = FTHREAC(5,NODREAC(N1)) + SKEW(K2,ISK)*AA*DT1
                   FTHREAC(6,NODREAC(N1)) = FTHREAC(6,NODREAC(N1)) + SKEW(K3,ISK)*AA*DT1
                 ENDIF
               ENDIF
!
               TFEXC=TFEXC+HALF*(A0+AA)*VV
             ENDIF
           ENDIF
C
         ELSE
C---------------------
C          PRESSION
C---------------------

           IF(IDEL > 0 ) CYCLE  ! SEGMENT DELETED

           IF(N_OLD/=N5.OR.X_OLD/=TS) THEN
             ISMOOTH = 0
             IF (N5 > 0) ISMOOTH = NPC(2*NFUNCT+N5+1)
!!             F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
             IF (ISMOOTH == 0) THEN
               F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
             ELSE
               F1 = FINTER_SMOOTH(N5,TS*FCX,NPC,TF,DYDX)
             ENDIF ! IF (ISMOOTH == 0)
             N_OLD = N5
             X_OLD = TS
           ENDIF
           AA = FCY*F1
           IF(N2D==0)THEN
C            ANALYSE 3D
             IF(N4/=0)THEN
               NX  = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
               NY  = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
               NZ  = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
               FX  = AA*NX*ONE_OVER_8
               FY  = AA*NY*ONE_OVER_8
               FZ  = AA*NZ*ONE_OVER_8
C
               IAD = IADC(1,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
                   FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
                   FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(2,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N2)/=0) THEN
                   FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
                   FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
                   FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(3,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N3)/=0) THEN
                   FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
                   FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
                   FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(4,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N4) = FEXT(1,N4)+FX
                 FEXT(2,N4) = FEXT(2,N4)+FY
                 FEXT(3,N4) = FEXT(3,N4)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N4)/=0) THEN
                   FTHREAC(1,NODREAC(N4)) = FTHREAC(1,NODREAC(N4)) + FX*DT1
                   FTHREAC(2,NODREAC(N4)) = FTHREAC(2,NODREAC(N4)) + FY*DT1
                   FTHREAC(3,NODREAC(N4)) = FTHREAC(3,NODREAC(N4)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                      +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                      +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
     
             ELSE
               ! true triangles.
               NX  = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))-(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
               NY  = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))-(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
               NZ  = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))-(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
               FX  = AA*NX*SIXTH
               FY  = AA*NY*SIXTH
               FZ  = AA*NZ*SIXTH
C
               IAD = IADC(1,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0  .AND.IMPL_S==0) THEN
                 FEXT(1,N1) = FEXT(1,N1)+FX
                 FEXT(2,N1) = FEXT(2,N1)+FY
                 FEXT(3,N1) = FEXT(3,N1)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N1)/=0) THEN
                   FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
                   FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
                   FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(2,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N2) = FEXT(1,N2)+FX
                 FEXT(2,N2) = FEXT(2,N2)+FY
                 FEXT(3,N2) = FEXT(3,N2)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N2)/=0) THEN
                   FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
                   FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
                   FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               IAD = IADC(3,NL)
               FSKYV(IAD,1) = FX
               FSKYV(IAD,2) = FY
               FSKYV(IAD,3) = FZ
               IF(IANIM >0 .AND.IMPL_S==0) THEN
                 FEXT(1,N3) = FEXT(1,N3)+FX
                 FEXT(2,N3) = FEXT(2,N3)+FY
                 FEXT(3,N3) = FEXT(3,N3)+FZ
               ENDIF
!
               IF (CPTREAC > 0) THEN
                 IF(NODREAC(N3)/=0) THEN
                   FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
                   FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
                   FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
                 ENDIF
               ENDIF
!
C
               TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)) + FY*(V(2,N1)+V(2,N2)+V(2,N3)) + FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
               
             ENDIF
           ELSE
C        ANALYSE 2D
             NY  =  -X(3,N2)+X(3,N1)
             NZ  =   X(2,N2)-X(2,N1)
             FY  = AA*NY*HALF
             FZ  = AA*NZ*HALF
C
             IAD = IADC(1,NL)
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
C
             IAD = IADC(2,NL)
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
C
             IF(IANIM >0 .AND.IMPL_S==0) THEN
               FEXT(2,N1) = FEXT(2,N1)+FY
               FEXT(3,N1) = FEXT(3,N1)+FZ
c
               FEXT(2,N2) = FEXT(2,N2)+FY
               FEXT(3,N2) = FEXT(3,N2)+FZ
             ENDIF
!
             IF (CPTREAC > 0) THEN 
               IF(NODREAC(N1)/=0) THEN
                 FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
                 FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
               ENDIF
               IF (NODREAC(N2)/=0) THEN
                 FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
                 FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
               ENDIF
             ENDIF
!
C
             IF(N2D==1)THEN
               AX=HALF*(X(2,N1)+X(2,N2))
             ELSE
               AX=ONE
             ENDIF
             TFEXTT=TFEXTT+DT1*(FY*(V(2,N1)+V(2,N2))+FZ*(V(3,N1)+V(3,N2)))*AX
           ENDIF
         ENDIF
 1000   CONTINUE
       ENDIF
C
       TFEXT = TFEXT + TFEXTT
C
      ENDIF
      RETURN
      END
