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
Chd|  R4EVEC3                       source/elements/spring/r4evec3.F
Chd|-- called by -----------
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R4EVEC3(RLOC ,V       ,EXX2 ,EYX2 ,EZX2,
     2                   EXY2 ,EYY2    ,EZY2 ,EXZ2 ,EYZ2,
     3                   EZZ2 ,AL2DP   ,X1DP ,X2DP ,AL2 ,
     4                   ALDP ,RLOC_ERR,NGL  ,AL   ,EXX ,
     5                   EYX  ,EZX     ,EXY  ,EYY  ,EZY ,
     6                   EXZ  ,EYZ     ,EZZ  ,RX1  ,RY1 ,
     7                   RZ1  ,RX2     ,RY2  ,RZ2  ,VX1 ,
     8                   VX2  ,VY1     ,VY2  ,VZ1  ,VZ2 ,
     9                   NC1  ,NC2     )
C-----------------------------------------------
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*),NC1(*),NC2(*)
C     REAL ou REAL*8
      my_real
     .   RLOC(3,*),V(3,*),
     .   EXX2(MVSIZ), EYX2(MVSIZ), EZX2(MVSIZ),
     .   EXY2(MVSIZ), EYY2(MVSIZ), EZY2(MVSIZ),
     .   EXZ2(MVSIZ), EYZ2(MVSIZ), EZZ2(MVSIZ),
     .   RLOC_ERR(3,*),AL2(MVSIZ),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ),
     .   EXY(MVSIZ), EYY(MVSIZ), EZY(MVSIZ),
     .   EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ),
     .   RX1(MVSIZ),RX2(MVSIZ),RY1(MVSIZ),
     .   RY2(MVSIZ),RZ1(MVSIZ),RZ2(MVSIZ),AL(MVSIZ),
     .   VX1(MVSIZ),VX2(MVSIZ),VY1(MVSIZ), VY2(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ)
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*),
     .                 ALDP(MVSIZ),AL2DP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL ou REAL*8
      my_real
     .     THETAPHI, R1PHI, R2PHI, SINT(MVSIZ),
     .     SUM(MVSIZ) ,SUM2(MVSIZ), SUM3(MVSIZ) ,THETA(MVSIZ),
     .     COST(MVSIZ), SUM1PHI, SUM2PHI, SUM3PHI, COPHI, SIPHI, COSPHI,
     .     SINPHI,SUMPHI,
     .     DT05,COST2(MVSIZ),SINT2(MVSIZ),CC,SS
      DOUBLE PRECISION EXXDP(MVSIZ),EYXDP(MVSIZ),EZXDP(MVSIZ),
     .     EXX2DP(MVSIZ),EYX2DP(MVSIZ),EZX2DP(MVSIZ),
     .     EXYDP(MVSIZ) ,EYYDP(MVSIZ) ,EZYDP(MVSIZ),
     .     EXY2DP(MVSIZ),EYY2DP(MVSIZ),EZY2DP(MVSIZ),
     .     EXZDP(MVSIZ) ,EYZDP(MVSIZ) ,EZZDP(MVSIZ),
     .     EXZ2DP(MVSIZ),EYZ2DP(MVSIZ),EZZ2DP(MVSIZ),
     .     RX1DP, RX2DP
C-----------------------------------------------
      DO I=LFT,LLT
        EXYDP(I)=RLOC(1,I)
        EYYDP(I)=RLOC(2,I)
        EZYDP(I)=RLOC(3,I)
        EXY2DP(I)=RLOC(1,I)
        EYY2DP(I)=RLOC(2,I)
        EZY2DP(I)=RLOC(3,I)
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=LFT,LLT
        EXYDP(I)=EXYDP(I)+RLOC_ERR(1,I)
        EYYDP(I)=EYYDP(I)+RLOC_ERR(2,I)
        EZYDP(I)=EZYDP(I)+RLOC_ERR(3,I)
        EXY2DP(I)=EXY2DP(I)+RLOC_ERR(1,I)
        EYY2DP(I)=EYY2DP(I)+RLOC_ERR(2,I)
        EZY2DP(I)=EZY2DP(I)+RLOC_ERR(3,I)
        ENDDO
      ENDIF
C
      DT05=UNDEMI*DT1
      IF (ISMDISP > 0) DT05=ZERO
C
      DO I=LFT,LLT
        EXXDP(I)=X2DP(1,I)-X1DP(1,I)
        EYXDP(I)=X2DP(2,I)-X1DP(2,I)
        EZXDP(I)=X2DP(3,I)-X1DP(3,I)
        ALDP(I) =SQRT(EXXDP(I)*EXXDP(I)+EYXDP(I)*EYXDP(I)+
     .                                  EZXDP(I)*EZXDP(I))
        AL(I)=ALDP(I)
        EXX2DP(I)=X2DP(1,I)-X1DP(1,I)-DT05*(V(1,NC2(I))-V(1,NC1(I)))
        EYX2DP(I)=X2DP(2,I)-X1DP(2,I)-DT05*(V(2,NC2(I))-V(2,NC1(I)))
        EZX2DP(I)=X2DP(3,I)-X1DP(3,I)-DT05*(V(3,NC2(I))-V(3,NC1(I)))
        AL2DP(I)=SQRT(EXX2DP(I)*EXX2DP(I)+EYX2DP(I)*EYX2DP(I)+
     .                EZX2DP(I)*EZX2DP(I))
        AL2(I)=AL2DP(I)
      ENDDO
C
      DO I=LFT,LLT
        IF (ALDP(I) <= EM15) THEN
          EXXDP(I)= UN
          EYXDP(I)= ZERO
          EZXDP(I)= ZERO
          EXYDP(I)= ZERO
          EYYDP(I)= UN
          EZYDP(I)= ZERO	
        ELSE
          EXXDP(I)=EXXDP(I)/ALDP(I) 
          EYXDP(I)=EYXDP(I)/ALDP(I) 
          EZXDP(I)=EZXDP(I)/ALDP(I) 
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        IF (AL2DP(I) <= EM15) THEN
          EXX2DP(I)= UN
          EYX2DP(I)= ZERO
          EZX2DP(I)= ZERO
          EXY2DP(I)= ZERO
          EYY2DP(I)= UN
          EZY2DP(I)= ZERO
        ELSE
          EXX2DP(I)=EXX2DP(I)/AL2DP(I)
          EYX2DP(I)=EYX2DP(I)/AL2DP(I)
          EZX2DP(I)=EZX2DP(I)/AL2DP(I)
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        EXZDP(I)=EYXDP(I)*EZYDP(I)-EZXDP(I)*EYYDP(I)
        EYZDP(I)=EZXDP(I)*EXYDP(I)-EXXDP(I)*EZYDP(I)
        EZZDP(I)=EXXDP(I)*EYYDP(I)-EYXDP(I)*EXYDP(I)
C
        EXZ2DP(I)=EYX2DP(I)*EZY2DP(I)-EZX2DP(I)*EYY2DP(I)
        EYZ2DP(I)=EZX2DP(I)*EXY2DP(I)-EXX2DP(I)*EZY2DP(I)
        EZZ2DP(I)=EXX2DP(I)*EYY2DP(I)-EYX2DP(I)*EXY2DP(I)
      ENDDO
C
      DO I=LFT,LLT
        EXYDP(I)=EYZDP(I)*EZXDP(I)-EZZDP(I)*EYXDP(I)
        EYYDP(I)=EZZDP(I)*EXXDP(I)-EXZDP(I)*EZXDP(I)
        EZYDP(I)=EXZDP(I)*EYXDP(I)-EYZDP(I)*EXXDP(I)
C
        EXY2DP(I)=EYZ2DP(I)*EZX2DP(I)-EZZ2DP(I)*EYX2DP(I)
        EYY2DP(I)=EZZ2DP(I)*EXX2DP(I)-EXZ2DP(I)*EZX2DP(I)
        EZY2DP(I)=EXZ2DP(I)*EYX2DP(I)-EYZ2DP(I)*EXX2DP(I)
      ENDDO
C--------------------------------------------
C     TORSION MOYENNE EN COORDONNEES GLOBALES
C--------------------------------------------
      DO I=LFT,LLT
        RX1DP   = EXX2DP(I)*RX1(I)+EYX2DP(I)*RY1(I)+EZX2DP(I)*RZ1(I)
        RX2DP   = EXX2DP(I)*RX2(I)+EYX2DP(I)*RY2(I)+EZX2DP(I)*RZ2(I)
        THETA(I) = (RX1DP+RX2DP)/DEUX*DT05
        SUM2(I)  = MAX(EM15,SQRT(EXY2DP(I)**2+EYY2DP(I)**2+EZY2DP(I)**2))
        SUM3(I)  = MAX(EM15,SQRT(EXZ2DP(I)**2+EYZ2DP(I)**2+EZZ2DP(I)**2))
        CC = COS(THETA(I))  
        SS = SIN(THETA(I))
        COST2(I) = CC/SUM2(I)
        SINT2(I) = SS/SUM3(I)
        SUM2(I)  = MAX(EM15,SQRT(EXYDP(I)**2+EYYDP(I)**2+EZYDP(I)**2))
        SUM3(I)  = MAX(EM15,SQRT(EXZDP(I)**2+EYZDP(I)**2+EZZDP(I)**2))
        COST(I)  = (DEUX*CC*CC-UN)/SUM2(I)
        SINT(I)  = DEUX*CC*SS/SUM3(I)
      ENDDO
C
C ... it is modified.
C
      DO I=LFT,LLT
        EXYDP(I)= EXYDP(I)*COST(I)+EXZDP(I)*SINT(I)
        EYYDP(I)= EYYDP(I)*COST(I)+EYZDP(I)*SINT(I)
        EZYDP(I)= EZYDP(I)*COST(I)+EZZDP(I)*SINT(I)
C
        EXY2DP(I)= EXY2DP(I)*COST2(I)+EXZ2DP(I)*SINT2(I)
        EYY2DP(I)= EYY2DP(I)*COST2(I)+EYZ2DP(I)*SINT2(I)
        EZY2DP(I)= EZY2DP(I)*COST2(I)+EZZ2DP(I)*SINT2(I)
      ENDDO
C
      DO I=LFT,LLT
        SUM(I)   =MAX(EM15,SQRT(EXYDP(I)*EXYDP(I)+
     .                          EYYDP(I)*EYYDP(I)+
     .                          EZYDP(I)*EZYDP(I)))
        EXYDP(I)=EXYDP(I)/SUM(I) 
        EYYDP(I)=EYYDP(I)/SUM(I) 
        EZYDP(I)=EZYDP(I)/SUM(I)
C
        SUM(I)   =MAX(EM15,SQRT(EXY2DP(I)*EXY2DP(I)+
     .                          EYY2DP(I)*EYY2DP(I)+
     .                          EZY2DP(I)*EZY2DP(I)))
        EXY2DP(I)=EXY2DP(I)/SUM(I) 
        EYY2DP(I)=EYY2DP(I)/SUM(I) 
        EZY2DP(I)=EZY2DP(I)/SUM(I) 
      ENDDO
C
      DO I=LFT,LLT
        EXZDP(I)=EYXDP(I)*EZYDP(I)-EZXDP(I)*EYYDP(I)
        EYZDP(I)=EZXDP(I)*EXYDP(I)-EXXDP(I)*EZYDP(I)
        EZZDP(I)=EXXDP(I)*EYYDP(I)-EYXDP(I)*EXYDP(I)
C
        EXZ2DP(I)=EYX2DP(I)*EZY2DP(I)-EZX2DP(I)*EYY2DP(I)
        EYZ2DP(I)=EZX2DP(I)*EXY2DP(I)-EXX2DP(I)*EZY2DP(I)
        EZZ2DP(I)=EXX2DP(I)*EYY2DP(I)-EYX2DP(I)*EXY2DP(I)
      ENDDO
C
      DO I=LFT,LLT
        SUM(I)   =MAX(EM15,SQRT(EXZDP(I)*EXZDP(I)+
     .                          EYZDP(I)*EYZDP(I)+
     .                          EZZDP(I)*EZZDP(I)))
        EXZDP(I)=EXZDP(I)/SUM(I) 
        EYZDP(I)=EYZDP(I)/SUM(I) 
        EZZDP(I)=EZZDP(I)/SUM(I) 
C
        SUM(I)   =MAX(EM15,SQRT(EXZ2DP(I)*EXZ2DP(I)+
     .                          EYZ2DP(I)*EYZ2DP(I)+
     .                          EZZ2DP(I)*EZZ2DP(I)))
        EXZ2DP(I)=EXZ2DP(I)/SUM(I) 
        EYZ2DP(I)=EYZ2DP(I)/SUM(I) 
        EZZ2DP(I)=EZZ2DP(I)/SUM(I)
      ENDDO
C
      DO I=LFT,LLT
        RLOC(1,I) = EXYDP(I)
        RLOC(2,I) = EYYDP(I)
        RLOC(3,I) = EZYDP(I)
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=LFT,LLT
          RLOC_ERR(1,I) = EXYDP(I) - RLOC(1,I)
          RLOC_ERR(2,I) = EYYDP(I) - RLOC(2,I)
          RLOC_ERR(3,I) = EZYDP(I) - RLOC(3,I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        VX1(I)=V(1,NC1(I))
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VX2(I)=V(1,NC2(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
      ENDDO
C
      DO I=LFT,LLT
       EXX(I)=EXXDP(I)
       EYX(I)=EYXDP(I)
       EZX(I)=EZXDP(I)
C
       EXY(I)=EXYDP(I)
       EYY(I)=EYYDP(I)
       EZY(I)=EZYDP(I)
C
       EXZ(I)=EXZDP(I)
       EYZ(I)=EYZDP(I)
       EZZ(I)=EZZDP(I)
C
       EXX2(I)=EXX2DP(I)
       EYX2(I)=EYX2DP(I)
       EZX2(I)=EZX2DP(I)
C
       EXY2(I)=EXY2DP(I)
       EYY2(I)=EYY2DP(I)
       EZY2(I)=EZY2DP(I)
C
       EXZ2(I)=EXZ2DP(I)
       EYZ2(I)=EYZ2DP(I)
       EZZ2(I)=EZZ2DP(I)
      ENDDO
C---
      RETURN
      END
