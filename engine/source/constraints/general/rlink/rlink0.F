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
Chd|  RLINK0                        source/constraints/general/rlink/rlink0.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RLINK0(V    ,VR    ,MS    ,IN  ,NLIK ,
     2                  IDIR ,IDROT ,NOD   ,NSN ,IC   ,
     3                  ICR  ,ISKW  ,ISK   ,SKEW,ISKWN,
     4                  FR_RL,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIK, IDIR, IDROT, NSN, IC, ICR, ISKW, ISK
      INTEGER NOD(*), ISKWN(LISKN,*), FR_RL(*), WEIGHT(*)
C     REAL
      my_real
     .   V(3,*), VR(3,*), MS(*), IN(*), SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IC1, ICC, IC2, IC3, N, K
C     REAL
      my_real
     .   MASS, INER, VX, VY, VZ, DVX, DVY, DVZ, VVX, VVY, VVZ,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN)
      DOUBLE PRECISION FRL6(4,6)
C-----------------------------------------------
C
      IDIR=IC
      IDROT=ICR
      NLIK =NSN
      DO 10 I=0,NUMSKW
      IF(ISK.NE.ISKWN(4,I+1))GOTO 10
       ISKW=I+1
       GOTO 20
  10  CONTINUE
C
  20  CONTINUE
C
      IF(IC.EQ.0)GOTO 150
      IC1=IC/4
      ICC=IC-4*IC1
      IC2=ICC/2
      IC3=ICC-2*IC2
C
      VX  =ZERO
      VY  =ZERO
      VZ  =ZERO
      MASS=ZERO
C
      DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N).EQ.1) THEN
          F1(I)=MS(N)
          F2(I)=MS(N)*V(1,N)
          F3(I)=MS(N)*V(2,N)
          F4(I)=MS(N)*V(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
      ENDDO
C
C Traitement Parith/ON avant echange
C
      DO K = 1, 6
       FRL6(1,K) = ZERO
       FRL6(2,K) = ZERO
       FRL6(3,K) = ZERO
       FRL6(4,K) = ZERO
      END DO
      CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 4)
      CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 4)
      CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 4)
      CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 4)
      IF(NSPMD.GT.1) THEN
        IF(FR_RL(ISPMD+1).NE.0) THEN
          CALL SPMD_EXCH_FR6(FR_RL,FRL6,4*6)
        END IF
      END IF
      MASS = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +       FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
      VX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +     FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
      VY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +     FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
      VZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +     FRL6(4,4)+FRL6(4,5)+FRL6(4,6)

      IF(MASS /= ZERO) THEN
        VX=VX/MASS
        VY=VY/MASS
        VZ=VZ/MASS
      ENDIF
C
      DO I=1,NSN
        N = NOD(I)
        DVX  =V(1,N)-VX
        DVY  =V(2,N)-VY
        DVZ  =V(3,N)-VZ
        VVX  =IC1*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =IC2*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =IC3*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        V(1,N) =V(1,N)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        V(2,N) =V(2,N)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        V(3,N) =V(3,N)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
      ENDDO
C
  150 IF(ICR.EQ.0)RETURN
      IC1=ICR/4
      ICC=ICR-4*IC1
      IC2=ICC/2
      IC3=ICC-2*IC2
C
      VX  =ZERO
      VY  =ZERO
      VZ  =ZERO
      INER=ZERO
C
      DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N).EQ.1) THEN
          F1(I)=IN(N)
          F2(I)=IN(N)*VR(1,N)
          F3(I)=IN(N)*VR(2,N)
          F4(I)=IN(N)*VR(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
      ENDDO
C
C Traitement Parith/ON avant echange
C
      DO K = 1, 6
        FRL6(1,K) = ZERO
        FRL6(2,K) = ZERO
        FRL6(3,K) = ZERO
        FRL6(4,K) = ZERO
      END DO
      CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 4)
      CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 4)
      CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 4)
      CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 4)
      IF(NSPMD.GT.1) THEN
        IF(FR_RL(ISPMD+1).NE.0) THEN
          CALL SPMD_EXCH_FR6(FR_RL,FRL6,4*6)
        END IF
      END IF
      INER = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +       FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
      VX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +     FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
      VY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +     FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
      VZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +     FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
C
      IF(INER.EQ.ZERO)RETURN
C
      VX=VX/INER
      VY=VY/INER
      VZ=VZ/INER
C
      DO I=1,NSN
        N = NOD(I)
        DVX  =VR(1,N)-VX
        DVY  =VR(2,N)-VY
        DVZ  =VR(3,N)-VZ
        VVX  =IC1*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =IC2*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =IC3*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        VR(1,N) =VR(1,N)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        VR(2,N) =VR(2,N)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        VR(3,N) =VR(3,N)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
      ENDDO
C
      RETURN
      END
