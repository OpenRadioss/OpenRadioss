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
Chd|  PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE PFLUID(ILOADP  ,FAC     ,NPC    ,TF    ,A       ,
     2                  V       ,X       ,XFRAME ,AR    ,VR      ,
     3                  SENSOR  ,LSENSOR ,WEIGHT ,TFEXC ,IADC    ,
     4                  FSKY    ,FSKYV   ,LLOADP ,FEXT  ,H3D_DATA
     5                  )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
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
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
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
      INTEGER NPC(*),LLOADP(*)
      INTEGER ILOADP(SIZLOADP,*),LSENSOR(NSENSI,*)
      INTEGER WEIGHT(*), IADC(*)
      my_real
     .   FAC(LFACLOAD,*), TF(*), A(3,*), V(3,*), AR(3,*), VR(3,*),
     .   X(3,*), XFRAME(NXFRAME,*), SENSOR(NSENSR,*),TFEXC,
     .   FSKY(8,SFSKY/8), FSKYV(SFSKY/8,8),FEXT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, FUN_HSP, K1, K2, K3, ISENS,K,LL,
     .        IERR,ICODE,IAD,N_OLD,IFRA1,N1FRAM,DIR_HSP,I,
     .        FUN_CX,FUN_VEL,DIR_VEL,IFRA2, IANIM,IJK,UP_BOUND
      my_real
     .   NX, NY, NZ, AXI, AA, A0, VV, FX, FY, FZ, AX, DYDX, TS,
     .   SIXTH,TFEXTT,X_OLD, F1, F2,XSENS,FCX,FCY, VSEGX, VSEGY, VSEGZ,
     .   FCX1,FCY1,FCX2,FCY2,VX,VY,VZ,VEL,VSEG
      my_real FINTER,COORMEAN,YMEAN,ZMEAN,PVEL,NORM,NSIGN,AREA
      EXTERNAL FINTER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      ! init.
      SIXTH  = UNSIX  
      TFEXC  = ZERO
      TFEXTT = ZERO
      N_OLD  = 0
      X_OLD  = ZERO
      IANIM  = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .         ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT


      !-----------------------------------!
      !            PARITH/OFF             !
      !-----------------------------------!
      IF(IPARIT.EQ.0) THEN
C code SPMD Parith/OFF ou SMP NE PAS OUBLIER LE CODE P/ON !
       DO 10 NL=1,NLOADP_F
       FUN_HSP=ILOADP(7,NL)
       DIR_HSP=ILOADP(8,NL)
       IFRA1=ILOADP(9,NL)
       FCY = FAC(1,NL)
       FCX = FAC(2,NL)
       FUN_CX=ILOADP(10,NL)
       FCY1 = FAC(3,NL)
       FCX1 = FAC(4,NL)
       FUN_VEL=ILOADP(11,NL)
       FCY2 = FAC(5,NL)
       FCX2 = FAC(6,NL)
       ! To avoid a check bound issue when the velocity options are not set in the input, 
       ! the DIR_VEL variable is bounded to a minimal value of 1
       DIR_VEL=MAX(ILOADP(12,NL),1)
       IFRA2=ILOADP(13,NL)
       ISENS=0
       XSENS = UN
       DO K=1,NSENSOR
         IF(ILOADP(6,NL).EQ.LSENSOR(1,K))ISENS=K
       ENDDO
       IF(ISENS.EQ.0)THEN
          TS=TT
       ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN
         IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)         
         IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)  
       ELSE                        
          TS = TT-SENSOR(2,ISENS)  
          IF(TS < ZERO) GOTO 10     
       ENDIF
       DO I = 1,ILOADP(1,NL)/4
         N1=LLOADP(ILOADP(4,NL)+4*(I-1))
         N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
         N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
         N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
C
         AA = ZERO
         VEL = ZERO
         PVEL=ZERO
C----------------  
C
         IF(N4.NE.0 .AND. N1.NE.N2 .AND. N1.NE.N3 .AND. N1.NE.N4 .AND.
     .                    N2.NE.N3 .AND. N2.NE.N4 .AND. N3.NE.N4 )THEN
C
           K1=3*DIR_HSP-2
           K2=3*DIR_HSP-1
           K3=3*DIR_HSP
           ! hydrostatic pressure
           IF(FUN_HSP .NE.0)THEN
              COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/QUATRE)+
     .                   (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/QUATRE)+
     .                   (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/QUATRE)
              AA       = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
           ENDIF
           NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
           NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
           NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
           NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
           AREA = UNDEMI * NORM
           AA = AA * AREA
C vel pressure
           K1=3*DIR_VEL-2
           K2=3*DIR_VEL-1
           K3=3*DIR_VEL
c
           NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .             NY * XFRAME(K2,IFRA2) + 
     .             NZ * XFRAME(K3,IFRA2)) 
           IF(NSIGN.NE.ZERO) NSIGN = SIGN(UN,NSIGN)
C
           VSEG= (XFRAME(K1,IFRA2)*
     .             (V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /QUATRE)+
     .            (XFRAME(K2,IFRA2)*
     .             (V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /QUATRE)+
     .            (XFRAME(K3,IFRA2)*
     .             (V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /QUATRE)
           
           IF(FUN_VEL .NE.0)THEN
              VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
           ELSE
              VEL =  - VSEG
           ENDIF
           IF(FUN_CX .NE.0)   
     .       PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .               (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .               (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .            FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/DEUX
C
c           FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NORM*(NX/NORM))*QUART
           FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NX*NSIGN)*QUART
           FY=(-AA*(NY/NORM)+PVEL*UNDEMI*NY*NSIGN)*QUART
           FZ=(-AA*(NZ/NORM)+PVEL*UNDEMI*NZ*NSIGN)*QUART
C
           A(1,N1)=A(1,N1)+FX
           A(2,N1)=A(2,N1)+FY
           A(3,N1)=A(3,N1)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N1) = FEXT(1,N1)+FX
             FEXT(2,N1) = FEXT(2,N1)+FY
             FEXT(3,N1) = FEXT(3,N1)+FZ
           ENDIF
C
           A(1,N2)=A(1,N2)+FX
           A(2,N2)=A(2,N2)+FY
           A(3,N2)=A(3,N2)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N2) = FEXT(1,N2)+FX
             FEXT(2,N2) = FEXT(2,N2)+FY
             FEXT(3,N2) = FEXT(3,N2)+FZ
           ENDIF
C
           A(1,N3)=A(1,N3)+FX
           A(2,N3)=A(2,N3)+FY
           A(3,N3)=A(3,N3)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N3) = FEXT(1,N3)+FX
             FEXT(2,N3) = FEXT(2,N3)+FY
             FEXT(3,N3) = FEXT(3,N3)+FZ
           ENDIF
C
           A(1,N4)=A(1,N4)+FX
           A(2,N4)=A(2,N4)+FY
           A(3,N4)=A(3,N4)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N4) = FEXT(1,N4)+FX
             FEXT(2,N4) = FEXT(2,N4)+FY
             FEXT(3,N4) = FEXT(3,N4)+FZ
           ENDIF
C
           TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                       +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                       +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
         ELSE
          IF(N1 == N2)THEN
            N2 = N3
            N3 = N4
            N4 = 0
          ELSEIF(N1 == N3)THEN
            N3 = N4
            N4 = 0
          ELSEIF(N1 == N4)THEN
            N4 = 0
          ELSEIF(N2 == N3)THEN
            N3 = N4
            N4 = 0
          ELSEIF(N2 == N4)THEN
            N2 = N3
            N3 = N4
            N4 = 0
          ELSEIF(N3 == N4)THEN
            N4 = 0
          ENDIF
Csv       true triangles.
          IF(FUN_HSP .NE.0)THEN
             K1=3*DIR_HSP-2
             K2=3*DIR_HSP-1
             K3=3*DIR_HSP
             ! hydrostatic pressure
             COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/TROIS)+
     .                  (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/TROIS)+
     .                  (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/TROIS)
             AA       =  FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
           ENDIF
           NX   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
           NY   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
           NZ   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
           NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
           AREA = UNDEMI * NORM
           AA = AA * AREA
C vel pressure
           K1=3*DIR_VEL-2
           K2=3*DIR_VEL-1
           K3=3*DIR_VEL
c
           NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .             NY * XFRAME(K2,IFRA2) + 
     .             NZ * XFRAME(K3,IFRA2)) 
           IF(NSIGN.NE.ZERO) NSIGN = SIGN(UN,NSIGN)
C
           VSEG= (XFRAME(K1,IFRA2)*
     .             (V(1,N1) + V(1,N2) + V(1,N3)) /TROIS)+
     .            (XFRAME(K2,IFRA2)*
     .             (V(2,N1) + V(2,N2) + V(2,N3)) /TROIS)+
     .            (XFRAME(K3,IFRA2)*
     .             (V(3,N1) + V(3,N2) + V(3,N3)) /TROIS)
           
           IF(FUN_VEL .NE.0)THEN
              VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
           ELSE
              VEL =  - VSEG
           ENDIF
           IF(FUN_CX .NE.0)   
     .       PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .               (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .               (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .            FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/DEUX
C
c           FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NORM*(NX/NORM))*TIERS
           FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NX*NSIGN)*TIERS
           FY=(-AA*(NY/NORM)+PVEL*UNDEMI*NY*NSIGN)*TIERS
           FZ=(-AA*(NZ/NORM)+PVEL*UNDEMI*NZ*NSIGN)*TIERS
C
           A(1,N1)=A(1,N1)+FX
           A(2,N1)=A(2,N1)+FY
           A(3,N1)=A(3,N1)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N1) = FEXT(1,N1)+FX
             FEXT(2,N1) = FEXT(2,N1)+FY
             FEXT(3,N1) = FEXT(3,N1)+FZ
           ENDIF
C
           A(1,N2)=A(1,N2)+FX
           A(2,N2)=A(2,N2)+FY
           A(3,N2)=A(3,N2)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N2) = FEXT(1,N2)+FX
             FEXT(2,N2) = FEXT(2,N2)+FY
             FEXT(3,N2) = FEXT(3,N2)+FZ
           ENDIF
C
           A(1,N3)=A(1,N3)+FX
           A(2,N3)=A(2,N3)+FY
           A(3,N3)=A(3,N3)+FZ
           IF(IANIM .GT.0) THEN
             FEXT(1,N3) = FEXT(1,N3)+FX
             FEXT(2,N3) = FEXT(2,N3)+FY
             FEXT(3,N3) = FEXT(3,N3)+FZ
           ENDIF
C
           TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                     +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                     +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
         ENDIF
        ENDDO
 10    CONTINUE
C
#include "atomic.inc"
              TFEXT = TFEXT + TFEXTT
#include "atomend.inc"





      ELSE      
      !-----------------------------------!
      !            PARITH/ON              !
      !-----------------------------------!
       IF(IVECTOR.EQ.0) THEN

         DO 100 NL=1,NLOADP_F
         FUN_HSP=ILOADP(7,NL)
         DIR_HSP=ILOADP(8,NL)
         IFRA1=ILOADP(9,NL)
         FCY = FAC(1,NL)
         FCX = FAC(2,NL)
         FUN_CX=ILOADP(10,NL)
         FCY1 = FAC(3,NL)
         FCX1 = FAC(4,NL)
         FUN_VEL=ILOADP(11,NL)
         FCY2 = FAC(5,NL)
         FCX2 = FAC(6,NL)
         ! To avoid a check bound issue when the velocity options are not set in the input, 
         ! the DIR_VEL variable is bounded to a minimal value of 1
         DIR_VEL=MAX(ILOADP(12,NL),1)
         IFRA2=ILOADP(13,NL)
         ISENS=0
         XSENS = UN
         ! -------------
         ! flush fsky array to 0.
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(ILOADP(4,NL)+4*(I-1))
           N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
           N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
           N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)

           IF(N4.NE.0 .AND. N1.NE.N2 .AND. N1.NE.N3 .AND. N1.NE.N4 .AND.
     .                      N2.NE.N3 .AND. N2.NE.N4 .AND. N3.NE.N4 )THEN
            UP_BOUND=4
           ELSE
            UP_BOUND=3
           ENDIF
           DO IJK=1,UP_BOUND
                IAD = IADC(ILOADP(4,NL)+4*(I-1)+(IJK-1))
                FSKY(1:3,IAD) = ZERO
           ENDDO
         ENDDO
         ! -------------
         DO K=1,NSENSOR
           IF(ILOADP(6,NL).EQ.LSENSOR(1,K))ISENS=K
         ENDDO
         IF(ISENS.EQ.0)THEN
            TS=TT
         ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN
           IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)         
           IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)  
         ELSE                        
            TS = TT-SENSOR(2,ISENS)  
            IF(TS < ZERO) GOTO 100     
         ENDIF
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(ILOADP(4,NL)+4*(I-1))
           N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
           N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
           N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
C
           AA = ZERO
           VEL=ZERO
           PVEL=ZERO
C------  ----------  
C
           IF(N4.NE.0 .AND. N1.NE.N2 .AND. N1.NE.N3 .AND. N1.NE.N4 .AND.
     .                      N2.NE.N3 .AND. N2.NE.N4 .AND. N3.NE.N4 )THEN
C
             K1=3*DIR_HSP-2
             K2=3*DIR_HSP-1
             K3=3*DIR_HSP
             IF(FUN_HSP .NE.0) THEN
               ! hydrostatic pressure
               COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/QUATRE)+
     .                    (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/QUATRE)+
     .                    (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/QUATRE)
               AA       = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
             ENDIF
             NX   = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2))-(X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
             NY   = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2))-(X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
             NZ   = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2))-(X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
             AREA = UNDEMI * NORM
             AA = AA * AREA
C vel p  ressure
             K1=3*DIR_VEL-2
             K2=3*DIR_VEL-1
             K3=3*DIR_VEL
c
             IF (IFRA2 > 0) THEN
               NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .                  NY * XFRAME(K2,IFRA2) + 
     .                  NZ * XFRAME(K3,IFRA2)) 
             ELSE
               NSIGN = (NX * XFRAME(K1,1) + 
     .                  NY * XFRAME(K2,1) + 
     .                  NZ * XFRAME(K3,1)) 
             ENDIF
             IF(NSIGN.NE.ZERO) NSIGN = SIGN(UN,NSIGN)
C
             VSEG= (XFRAME(K1,IFRA2)*
     .               (V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /QUATRE)+
     .              (XFRAME(K2,IFRA2)*
     .               (V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /QUATRE)+
     .              (XFRAME(K3,IFRA2)*
     .               (V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /QUATRE)
             IF(FUN_VEL .NE.0)THEN
                VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
             ELSE
                VEL =  - VSEG
             ENDIF
             IF(FUN_CX .NE.0)   
     .          PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)
     .                     -(NY/NORM)*VEL*XFRAME(K2,IFRA2)
     .                     -(NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .              FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/DEUX
C
             FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NX*NSIGN)*QUART
             FY=(-AA*(NY/NORM)+PVEL*UNDEMI*NY*NSIGN)*QUART
             FZ=(-AA*(NZ/NORM)+PVEL*UNDEMI*NZ*NSIGN)*QUART
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1))
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N1) = FEXT(1,N1)+FX
               FEXT(2,N1) = FEXT(2,N1)+FY
               FEXT(3,N1) = FEXT(3,N1)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N2) = FEXT(1,N2)+FX
               FEXT(2,N2) = FEXT(2,N2)+FY
               FEXT(3,N2) = FEXT(3,N2)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N3) = FEXT(1,N3)+FX
               FEXT(2,N3) = FEXT(2,N3)+FY
               FEXT(3,N3) = FEXT(3,N3)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+3)
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N4) = FEXT(1,N4)+FX
               FEXT(2,N4) = FEXT(2,N4)+FY
               FEXT(3,N4) = FEXT(3,N4)+FZ
             ENDIF
C
             TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                       +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                       +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
           ELSE
            IF(N1 == N2)THEN
              N2 = N3
              N3 = N4
              N4 = 0
            ELSEIF(N1 == N3)THEN
              N3 = N4
              N4 = 0
            ELSEIF(N1 == N4)THEN
              N4 = 0
            ELSEIF(N2 == N3)THEN
              N3 = N4
              N4 = 0
            ELSEIF(N2 == N4)THEN
              N2 = N3
              N3 = N4
              N4 = 0
            ELSEIF(N3 == N4)THEN
              N4 = 0
            ENDIF
            !true triangles.
             IF(FUN_HSP .NE.0)THEN
               K1=3*DIR_HSP-2
               K2=3*DIR_HSP-1
               K3=3*DIR_HSP
               ! hydrostatic pressure
               COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/TROIS)+
     .                    (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/TROIS)+
     .                    (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/TROIS)
               AA       = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
             ENDIF
C
             NX   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))-(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
             NY   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))-(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
             NZ   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))-(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
             AREA = UNDEMI * NORM
             AA = AA * AREA
C vel p  ressure
             K1=3*DIR_VEL-2
             K2=3*DIR_VEL-1
             K3=3*DIR_VEL
c
             NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .               NY * XFRAME(K2,IFRA2) + 
     .               NZ * XFRAME(K3,IFRA2))
             IF(NSIGN.NE.ZERO) NSIGN = SIGN(UN,NSIGN)
C
             VSEG= (XFRAME(K1,IFRA2)*
     .               (V(1,N1) + V(1,N2) + V(1,N3)) /TROIS)+
     .              (XFRAME(K2,IFRA2)*
     .               (V(2,N1) + V(2,N2) + V(2,N3)) /TROIS)+
     .              (XFRAME(K3,IFRA2)*
     .               (V(3,N1) + V(3,N2) + V(3,N3)) /TROIS)
             IF(FUN_VEL .NE.0)THEN
                VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
             ELSE
                VEL =  - VSEG
             ENDIF
             IF(FUN_CX .NE.0)   
     .         PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .                 (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .                 (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .              FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/DEUX
C
             FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NX*NSIGN)*TIERS
             FY=(-AA*(NY/NORM)+PVEL*UNDEMI*NY*NSIGN)*TIERS
             FZ=(-AA*(NZ/NORM)+PVEL*UNDEMI*NZ*NSIGN)*TIERS
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1))
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N1) = FEXT(1,N1)+FX
               FEXT(2,N1) = FEXT(2,N1)+FY
               FEXT(3,N1) = FEXT(3,N1)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N2) = FEXT(1,N2)+FX
               FEXT(2,N2) = FEXT(2,N2)+FY
               FEXT(3,N2) = FEXT(3,N2)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
             FSKY(1,IAD) = FX
             FSKY(2,IAD) = FY
             FSKY(3,IAD) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N3) = FEXT(1,N3)+FX
               FEXT(2,N3) = FEXT(2,N3)+FY
               FEXT(3,N3) = FEXT(3,N3)+FZ
             ENDIF
C
             TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                       +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                       +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
           ENDIF
          ENDDO
 100      CONTINUE
C
#include "atomic.inc"
                TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
         ELSE
C------  ---------------
C code   vectoriel
C------  ---------------

         DO 200 NL=1,NLOADP_F
         FUN_HSP=ILOADP(7,NL)
         DIR_HSP=ILOADP(8,NL)
         IFRA1=ILOADP(9,NL)
         FCY = FAC(1,NL)
         FCX = FAC(2,NL)
         FUN_CX=ILOADP(10,NL)
         FCY1 = FAC(3,NL)
         FCX1 = FAC(4,NL)
         FUN_VEL=ILOADP(11,NL)
         FCY2 = FAC(5,NL)
         FCX2 = FAC(6,NL)
         ! To avoid a check bound issue when the velocity options are not set in the input, 
         ! the DIR_VEL variable is bounded to a minimal value of 1
         DIR_VEL=MAX(ILOADP(12,NL),1)
         IFRA2=ILOADP(13,NL)
         ISENS=0
         XSENS = UN

         DO K=1,NSENSOR
           IF(ILOADP(6,NL).EQ.LSENSOR(1,K))ISENS=K
         ENDDO
         IF(ISENS.EQ.0)THEN
            TS=TT
         ELSEIF (LSENSOR(4,ISENS)==753096514 .AND. TT /= ZERO) THEN
           IERR = SET_U_SENS_VALUE(ISENS,1,ZERO)         
           IERR = GET_U_SENS_VALUE(ISENS,2,XSENS)  
         ELSE                        
            TS = TT-SENSOR(2,ISENS)  
            IF(TS < ZERO) GOTO 200     
         ENDIF
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(ILOADP(4,NL)+4*(I-1))
           N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
           N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
           N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
C
           AA = ZERO
           VEL = ZERO
           PVEL=ZERO
C------  ----------  
C
           IF(N4.NE.0 .AND. N1.NE.N2 .AND. N1.NE.N3 .AND. N1.NE.N4 .AND.
     .                      N2.NE.N3 .AND. N2.NE.N4 .AND. N3.NE.N4 )THEN
C
             K1=3*DIR_HSP-2
             K2=3*DIR_HSP-1
             K3=3*DIR_HSP
             IF(FUN_HSP .NE.0) THEN
               ! hydrostatic pressure
               COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/QUATRE)+
     .                    (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/QUATRE)+
     .                    (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/QUATRE)
               AA       = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
             ENDIF
             NX   = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
             NY   = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
             NZ   = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
             AREA = UNDEMI * NORM
             AA = AA * AREA
C vel p  ressure
             K1=3*DIR_VEL-2
             K2=3*DIR_VEL-1
             K3=3*DIR_VEL
c
             NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .               NY * XFRAME(K2,IFRA2) + 
     .               NZ * XFRAME(K3,IFRA2))
             IF(NSIGN.NE.ZERO) NSIGN = SIGN(UN,NSIGN)
C
             VSEG= (XFRAME(K1,IFRA2)*
     .               (V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /QUATRE)+
     .              (XFRAME(K2,IFRA2)*
     .               (V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /QUATRE)+
     .              (XFRAME(K3,IFRA2)*
     .               (V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /QUATRE)
                
             IF(FUN_VEL .NE.0)THEN
                VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
             ELSE
                VEL =  - VSEG
             ENDIF
             IF(FUN_CX .NE.0)   
     .         PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .                 (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .                 (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .              FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/DEUX
C
c             FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NORM*(NX/NORM))*QUART
             FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NX*NSIGN)*QUART
             FY=(-AA*(NY/NORM)+PVEL*UNDEMI*NY*NSIGN)*QUART
             FZ=(-AA*(NZ/NORM)+PVEL*UNDEMI*NZ*NSIGN)*QUART
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1))
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N1) = FEXT(1,N1)+FX
               FEXT(2,N1) = FEXT(2,N1)+FY
               FEXT(3,N1) = FEXT(3,N1)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N2) = FEXT(1,N2)+FX
               FEXT(2,N2) = FEXT(2,N2)+FY
               FEXT(3,N2) = FEXT(3,N2)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N3) = FEXT(1,N3)+FX
               FEXT(2,N3) = FEXT(2,N3)+FY
               FEXT(3,N3) = FEXT(3,N3)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+3)
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N4) = FEXT(1,N4)+FX
               FEXT(2,N4) = FEXT(2,N4)+FY
               FEXT(3,N4) = FEXT(3,N4)+FZ
             ENDIF
C
             TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                       +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                       +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
           ELSE
            IF(N1 == N2)THEN
              N2 = N3
              N3 = N4
              N4 = 0
            ELSEIF(N1 == N3)THEN
              N3 = N4
              N4 = 0
            ELSEIF(N1 == N4)THEN
              N4 = 0
            ELSEIF(N2 == N3)THEN
              N3 = N4
              N4 = 0
            ELSEIF(N2 == N4)THEN
              N2 = N3
              N3 = N4
              N4 = 0
            ELSEIF(N3 == N4)THEN
              N4 = 0
            ENDIF
Csv         true triangles.
             K1=3*DIR_HSP-2
             K2=3*DIR_HSP-1
             K3=3*DIR_HSP
             IF(FUN_HSP .NE.0)THEN
               ! hydrostatic pressure
               COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/TROIS)+
     .                    (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/TROIS)+
     .                    (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/TROIS)
               AA       =  FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
             ENDIF
             NX   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
             NY   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
             NZ   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
             AREA = UNDEMI * NORM
             AA = AA * AREA
C vel p  ressure
             K1=3*DIR_VEL-2
             K2=3*DIR_VEL-1
             K3=3*DIR_VEL
c
             NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .               NY * XFRAME(K2,IFRA2) + 
     .               NZ * XFRAME(K3,IFRA2))
             IF(NSIGN.NE.ZERO) NSIGN = SIGN(UN,NSIGN)
C
             VSEG= (XFRAME(K1,IFRA2)*
     .               (V(1,N1) + V(1,N2) + V(1,N3)) /TROIS)+
     .              (XFRAME(K2,IFRA2)*
     .               (V(2,N1) + V(2,N2) + V(2,N3)) /TROIS)+
     .              (XFRAME(K3,IFRA2)*
     .               (V(3,N1) + V(3,N2) + V(3,N3)) /TROIS)
             
             IF(FUN_VEL .NE.0)THEN
                VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
             ELSE
                VEL =  - VSEG
             ENDIF
             IF(FUN_CX .NE.0)   
     .         PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .                 (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .                 (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .              FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/DEUX
C
c             FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NORM*(NX/NORM))*TIERS
             FX=(-AA*(NX/NORM)+PVEL*UNDEMI*NX*NSIGN)*TIERS
             FY=(-AA*(NY/NORM)+PVEL*UNDEMI*NY*NSIGN)*TIERS
             FZ=(-AA*(NZ/NORM)+PVEL*UNDEMI*NZ*NSIGN)*TIERS
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1))
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N1) = FEXT(1,N1)+FX
               FEXT(2,N1) = FEXT(2,N1)+FY
               FEXT(3,N1) = FEXT(3,N1)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+1)
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N2) = FEXT(1,N2)+FX
               FEXT(2,N2) = FEXT(2,N2)+FY
               FEXT(3,N2) = FEXT(3,N2)+FZ
             ENDIF
C
             IAD = IADC(ILOADP(4,NL)+4*(I-1)+2)
             FSKYV(IAD,1) = FX
             FSKYV(IAD,2) = FY
             FSKYV(IAD,3) = FZ
             IF(IANIM .GT.0) THEN
               FEXT(1,N3) = FEXT(1,N3)+FX
               FEXT(2,N3) = FEXT(2,N3)+FY
               FEXT(3,N3) = FEXT(3,N3)+FZ
             ENDIF
C
             TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                       +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                       +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
           ENDIF
          ENDDO
 200      CONTINUE
C
#include "atomic.inc"
                TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
         ENDIF ! IF(IVECTOR.EQ.0)


       ENDIF
C
      RETURN
      END
