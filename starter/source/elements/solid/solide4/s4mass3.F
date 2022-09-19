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
C
Chd|====================================================================
Chd|  S4MASS3                       source/elements/solid/solide4/s4mass3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|-- calls ---------------
Chd|        S4FRACA                       source/elements/solid/solide4/s4mass3.F
Chd|====================================================================
      SUBROUTINE S4MASS3(
     1   RHO  ,MS ,PARTSAV,X   ,V,
     2   IPART,MSS,MSNF   ,MSSF,WMA,
     3   RHOCP,MCP,MCPS   ,TEMP0 ,TEMP,
     4   MSSA ,IX1   ,IX2   ,IX3   ,IX4,
     5   FILL, VOLU)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
#include      "ale_param.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(*), IX1(*), IX2(*), IX3(*), IX4(*)
      my_real
     .   RHO(*), MS(*),X(3,*),V(3,*),PARTSAV(20,*),MSNF(*), MSS(8,*),
     .   MSSF(8,*),WMA(*), RHOCP(*),MCPS(8,*),TEMP(*),TEMP0(*),MCP(*),
     .   MSSA(*), FILL(*), VOLU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,I1,I2,I3,I4,j
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX, MASS(MVSIZ),RCP,PTG(4,MVSIZ)
C
C-----------------------------------------------------------------------
      IF(ISMS==0)
     .  CALL S4FRACA(X,IX1 ,IX2,IX3 ,IX4 ,PTG   )
      DO I=LFT,LLT
       MASS(I)=FILL(I)*RHO(I)*VOLU(I)*FOURTH
C
       I1 = IX1(I)
       I2 = IX2(I)
       I3 = IX3(I)
       I4 = IX4(I)
C       
       IF(ISMS==0)THEN
         MSS(1,I)=MASS(I)*PTG(1,I)
         MSS(3,I)=MASS(I)*PTG(2,I)
         MSS(6,I)=MASS(I)*PTG(3,I)
         MSS(5,I)=MASS(I)*PTG(4,I)
       ELSE
         MSS(1,I)=MASS(I)
         MSS(3,I)=MASS(I)
         MSS(6,I)=MASS(I)
         MSS(5,I)=MASS(I)
       END IF
C
       MSS(2,I)=ZERO
       MSS(4,I)=ZERO
       MSS(7,I)=ZERO
       MSS(8,I)=ZERO
C
       IP=IPART(I)
       PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*MASS(I)
       PARTSAV(2,IP)=PARTSAV(2,IP)
     .              + MASS(I)*(X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4))
       PARTSAV(3,IP)=PARTSAV(3,IP)
     .              + MASS(I)*(X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4))
       PARTSAV(4,IP)=PARTSAV(4,IP)
     .              + MASS(I)*(X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4))
       XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .      +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4))
       XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .      +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4))
       YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .      +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4))
       YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .      +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4))
       ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .      +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4))
       ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .      +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4))
       PARTSAV(5,IP) =PARTSAV(5,IP)  + MASS(I) * (YY+ZZ)
       PARTSAV(6,IP) =PARTSAV(6,IP)  + MASS(I) * (ZZ+XX)
       PARTSAV(7,IP) =PARTSAV(7,IP)  + MASS(I) * (XX+YY)
       PARTSAV(8,IP) =PARTSAV(8,IP)  - MASS(I) * XY
       PARTSAV(9,IP) =PARTSAV(9,IP)  - MASS(I) * YZ
       PARTSAV(10,IP)=PARTSAV(10,IP) - MASS(I) * ZX
C
       PARTSAV(11,IP)=PARTSAV(11,IP)
     .              + MASS(I)*(V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
       PARTSAV(12,IP)=PARTSAV(12,IP)
     .              + MASS(I)*(V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
       PARTSAV(13,IP)=PARTSAV(13,IP)
     .              + MASS(I)*(V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
       PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .    (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .    +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .    +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .    +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
      ENDDO
C
      IF(IREST_MSELT /= 0)THEN
       DO I=LFT,LLT
        MSSA(NFT+I)=MASS(I)
       ENDDO
      ENDIF
C
      IF(JALE+JEUL > 0)THEN
       DO I=LFT,LLT
         I1 = IX1(I)
         I2 = IX2(I)
         I3 = IX3(I)
         I4 = IX4(I)
         MSSF(1,I)=MASS(I)
         MSSF(3,I)=MASS(I)
         MSSF(6,I)=MASS(I)
         MSSF(5,I)=MASS(I)
         MSSF(2,I)=ZERO
         MSSF(4,I)=ZERO
         MSSF(7,I)=ZERO
         MSSF(8,I)=ZERO
       ENDDO
      ENDIF
C
      IF(JTHE < 0 ) THEN
       DO I=LFT,LLT
        RCP=FILL(I)*RHOCP(I)*VOLU(I)*FOURTH
        MCPS(1,I) = RCP
        MCPS(3,I) = RCP
        MCPS(5,I) = RCP
        MCPS(6,I) = RCP
        MCPS(2,I) = ZERO
        MCPS(4,I) = ZERO
        MCPS(7,I) = ZERO
        MCPS(8,I) = ZERO
       ENDDO
      ENDIF
C
      IF(JALE > 0 .AND. NWALE == 4)THEN
        DO I=LFT,LLT
          I1 = IX1(I)
          I2 = IX2(I)
          I3 = IX3(I)
          I4 = IX4(I)
          WMA(I1)=WMA(I1)+THREE_HALF
          WMA(I2)=WMA(I2)+THREE_HALF
          WMA(I3)=WMA(I3)+THREE_HALF
          WMA(I4)=WMA(I4)+THREE_HALF
        ENDDO
      ENDIF
C
      IF(JTHE < 0 ) THEN
       IF(NINTEMP > 0 ) THEN
          DO I=LFT,LLT
            I1 = IX1(I)
            I2 = IX2(I)
            I3 = IX3(I)
            I4 = IX4(I)
            IF(TEMP(I1)== ZERO) TEMP(I1) = TEMP0(I)
            IF(TEMP(I2)== ZERO) TEMP(I2) = TEMP0(I)
            IF(TEMP(I3)== ZERO) TEMP(I3) = TEMP0(I)
            IF(TEMP(I4)== ZERO) TEMP(I4) = TEMP0(I)
          ENDDO
       ELSE
          DO I=LFT,LLT
            I1 = IX1(I)
            I2 = IX2(I)
            I3 = IX3(I)
            I4 = IX4(I)
            TEMP(I1) = TEMP0(I)
            TEMP(I2) = TEMP0(I)
            TEMP(I3) = TEMP0(I)
            TEMP(I4) = TEMP0(I)
        ENDDO
       ENDIF
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S4FRAC                        source/elements/solid/solide4/s4mass3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        DIS_N1N2                      source/elements/solid/solide4/s4mass3.F
Chd|====================================================================
      SUBROUTINE S4FRAC(X,IX1 ,IX2,IX3 ,IX4 ,PTG   )
C----------------------------------------------
C     MASS PARTITION IN FUNCTION OF NODAL ANGLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX1(*), IX2(*), IX3(*),IX4(*)
      my_real
     .   X(3,*),PTG(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IP,I1,I2,I3,I4
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,P1,P2,P3,P4,S
      my_real
     .   A2(MVSIZ), B2(MVSIZ), C2(MVSIZ),D2(MVSIZ),E2(MVSIZ),F2(MVSIZ),
     .   AA(MVSIZ), BB(MVSIZ), CC(MVSIZ),DD(MVSIZ),EE(MVSIZ),FF(MVSIZ)
C=======================================================================
C----------------------------------------------
C     MASSES ELEMENTAIRES
C----------------------------------------------
C
       CALL DIS_N1N2(X,IX1,IX2,AA,A2 )
       CALL DIS_N1N2(X,IX2,IX3,BB,B2 )
       CALL DIS_N1N2(X,IX3,IX1,CC,C2 )
       CALL DIS_N1N2(X,IX2,IX4,DD,D2 )
       CALL DIS_N1N2(X,IX3,IX4,EE,E2 )
       CALL DIS_N1N2(X,IX1,IX4,FF,F2 )
      DO I=LFT,LLT
        P1 = ACOS((A2(I) + C2(I) - B2(I))/(TWO * AA(I) * CC(I)))
        P2 = ACOS((A2(I) + F2(I) - D2(I))/(TWO * AA(I) * FF(I)))
        P3 = ACOS((C2(I) + F2(I) - E2(I))/(TWO * CC(I) * FF(I)))
        P1 = ACOS((A2(I) + C2(I) - B2(I))/(TWO * AA(I) * CC(I)))+
     +       ACOS((A2(I) + F2(I) - D2(I))/(TWO * AA(I) * FF(I)))+
     +       ACOS((C2(I) + F2(I) - E2(I))/(TWO * CC(I) * FF(I)))
C    
        P2 = ACOS((A2(I) + B2(I) - C2(I))/(TWO * AA(I) * BB(I)))+
     +       ACOS((A2(I) + D2(I) - F2(I))/(TWO * AA(I) * DD(I)))+
     +       ACOS((D2(I) + B2(I) - E2(I))/(TWO * DD(I) * BB(I)))
C    
        P3 = ACOS((B2(I) + C2(I) - A2(I))/(TWO * BB(I) * CC(I)))+
     +       ACOS((B2(I) + E2(I) - D2(I))/(TWO * BB(I) * EE(I)))+
     +       ACOS((E2(I) + C2(I) - F2(I))/(TWO * EE(I) * CC(I)))
C    
        P4 = ACOS((F2(I) + D2(I) - A2(I))/(TWO * FF(I) * DD(I)))+
     +       ACOS((D2(I) + E2(I) - B2(I))/(TWO * DD(I) * EE(I)))+
     +       ACOS((E2(I) + F2(I) - C2(I))/(TWO * EE(I) * FF(I)))
        PTG(1,I)=P1/PI
        PTG(2,I)=P2/PI
        PTG(3,I)=P3/PI
        PTG(4,I)=P4/PI
        S=PTG(1,I)+PTG(2,I)+PTG(3,I)+PTG(4,I)
c        PTG(1,I)=ONE
c        PTG(2,I)=ONE
c        PTG(3,I)=ONE
c        PTG(4,I)=ONE
      END DO
C
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  DIS_N1N2                      source/elements/solid/solide4/s4mass3.F
Chd|-- called by -----------
Chd|        S4FRAC                        source/elements/solid/solide4/s4mass3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIS_N1N2(X,N1 ,N2 ,S ,S2   )
C----------------------------------------------
C     S=L(N1,N2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1(*),N2(*)
      my_real
     .   X(3,*),S(*),S2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
C=======================================================================
      DO I=LFT,LLT
        XX = X(1,N2(I))-X(1,N1(I))
        YY = X(2,N2(I))-X(2,N1(I))
        ZZ = X(3,N2(I))-X(3,N1(I))
        S2(I) = XX*XX+YY*YY+ZZ*ZZ
        S(I) = SQRT(S2(I))
      END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S4FRACA                       source/elements/solid/solide4/s4mass3.F
Chd|-- called by -----------
Chd|        S4MASS3                       source/elements/solid/solide4/s4mass3.F
Chd|-- calls ---------------
Chd|        AREA_TRIA                     source/elements/solid/solide4/s4mass3.F
Chd|====================================================================
      SUBROUTINE S4FRACA(X,IX1 ,IX2,IX3 ,IX4 ,PTG   )
C----------------------------------------------
C     MASS PARTITION IN FUNCTION OF NODAL ANGLES by AREA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr21_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX1(*), IX2(*), IX3(*),IX4(*)
      my_real
     .   X(3,*),PTG(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IP,I1,I2,I3,I4
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,P1,P2,P3,P4,S
      my_real
     .   A1(MVSIZ), A2(MVSIZ), A3(MVSIZ),A4(MVSIZ)
C=======================================================================
      IF (IMAS_DS==0) THEN
       DO I=LFT,LLT
       DO J=1,4
        PTG(J,I)=ONE
       END DO
       END DO
       RETURN
      END IF
C ------------Ai -> A   (i)-----------
       CALL AREA_TRIA(X,IX2,IX3,IX4, A1  )
       CALL AREA_TRIA(X,IX1,IX3,IX4, A2  )
       CALL AREA_TRIA(X,IX1,IX2,IX4, A3  )
       CALL AREA_TRIA(X,IX1,IX2,IX3, A4  )
      DO I=LFT,LLT
        S = FOUR/(A1(I) +A2(I) +A3(I) +A4(I))
        PTG(1,I)=A1(I)*S
        PTG(2,I)=A2(I)*S
        PTG(3,I)=A3(I)*S
        PTG(4,I)=A4(I)*S
      END DO
C
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  AREA_TRIA                     source/elements/solid/solide4/s4mass3.F
Chd|-- called by -----------
Chd|        S4FRACA                       source/elements/solid/solide4/s4mass3.F
Chd|        S4FRACA10                     source/elements/solid/solide10/s10mass3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE AREA_TRIA(X,N1 ,N2 ,N3, A2 )
C----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1(*),N2(*),N3(*)
      my_real
     .   X(3,*),A2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .    X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),
     .    Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),
     .    Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),
     .    X13, Y13, Z13, X12, Y12, Z12,
     .    E3X, E3Y, E3Z, SURF
C=======================================================================
        DO I=LFT,LLT
          X1(I)=X(1,N1(I))
          Y1(I)=X(2,N1(I))
          Z1(I)=X(3,N1(I))
          X2(I)=X(1,N2(I))
          Y2(I)=X(2,N2(I))
          Z2(I)=X(3,N2(I))
          X3(I)=X(1,N3(I))
          Y3(I)=X(2,N3(I))
          Z3(I)=X(3,N3(I))
        ENDDO
        DO I=LFT,LLT
         X13=X3(I)-X1(I)
         Y13=Y3(I)-Y1(I)
         Z13=Z3(I)-Z1(I)
         X12=X2(I)-X1(I)
         Y12=Y2(I)-Y1(I)
         Z12=Z2(I)-Z1(I)
         E3X=Y12*Z13-Z12*Y13
         E3Y=Z12*X13-X12*Z13
         E3Z=X12*Y13-Y12*X13
         A2(I)=SQRT(FOURTH*(E3X*E3X+E3Y*E3Y+E3Z*E3Z))
        ENDDO
C-----------
      RETURN
      END
