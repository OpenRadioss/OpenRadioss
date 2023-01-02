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
Chd|  S4VOLNOD3                     source/elements/solid/solide4_sfem/s4volnod3.F
Chd|-- called by -----------
Chd|        S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|====================================================================
      SUBROUTINE S4VOLNOD3(VOLNOD6,X,NC1,NC2,NC3,NC4,OFFG,XDP,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),NEL
      DOUBLE PRECISION
     .   VOLNOD6(6,2*NUMNOD)
      DOUBLE PRECISION , DIMENSION(3,SXDP/3), INTENT(IN) :: XDP     
C     REAL
      my_real
     .   X(3,*),OFFG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K
C     REAL

      my_real
     .   DET(MVSIZ)
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,
     .   B1, C1, D1
      DOUBLE PRECISION
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)

      DOUBLE PRECISION
     .   DET6(6,MVSIZ),DETDP
C----------------------------
C     NODAL COORDINATES     
C----------------------------
      IF (ISMSTR==1.OR.ISMSTR==11) RETURN
      IF(IRESP == 1) THEN
        IF((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3)THEN
          DO I=LFT,LLT
            IF(ABS(OFFG(I))>ONE) CYCLE
            X1(I)=XDP(1,NC1(I))
            Y1(I)=XDP(2,NC1(I))
            Z1(I)=XDP(3,NC1(I))
            X2(I)=XDP(1,NC2(I))
            Y2(I)=XDP(2,NC2(I))
            Z2(I)=XDP(3,NC2(I))
            X3(I)=XDP(1,NC3(I))
            Y3(I)=XDP(2,NC3(I))
            Z3(I)=XDP(3,NC3(I))
            X4(I)=XDP(1,NC4(I))
            Y4(I)=XDP(2,NC4(I))
            Z4(I)=XDP(3,NC4(I))
          ENDDO
        ELSE
          DO I=LFT,LLT
            X1(I)=XDP(1,NC1(I))
            Y1(I)=XDP(2,NC1(I))
            Z1(I)=XDP(3,NC1(I))
            X2(I)=XDP(1,NC2(I))
            Y2(I)=XDP(2,NC2(I))
            Z2(I)=XDP(3,NC2(I))
            X3(I)=XDP(1,NC3(I))
            Y3(I)=XDP(2,NC3(I))
            Z3(I)=XDP(3,NC3(I))
            X4(I)=XDP(1,NC4(I))
            Y4(I)=XDP(2,NC4(I))
            Z4(I)=XDP(3,NC4(I))
          ENDDO
        ENDIF
      ELSE
        IF((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3)THEN
          DO I=LFT,LLT
            IF(ABS(OFFG(I))>ONE) CYCLE
            X1(I)=X(1,NC1(I))
            Y1(I)=X(2,NC1(I))
            Z1(I)=X(3,NC1(I))
            X2(I)=X(1,NC2(I))
            Y2(I)=X(2,NC2(I))
            Z2(I)=X(3,NC2(I))
            X3(I)=X(1,NC3(I))
            Y3(I)=X(2,NC3(I))
            Z3(I)=X(3,NC3(I))
            X4(I)=X(1,NC4(I))
            Y4(I)=X(2,NC4(I))
            Z4(I)=X(3,NC4(I))
          ENDDO
        ELSE
          DO I=LFT,LLT
            X1(I)=X(1,NC1(I))
            Y1(I)=X(2,NC1(I))
            Z1(I)=X(3,NC1(I))
            X2(I)=X(1,NC2(I))
            Y2(I)=X(2,NC2(I))
            Z2(I)=X(3,NC2(I))
            X3(I)=X(1,NC3(I))
            Y3(I)=X(2,NC3(I))
            Z3(I)=X(3,NC3(I))
            X4(I)=X(1,NC4(I))
            Y4(I)=X(2,NC4(I))
            Z4(I)=X(3,NC4(I))
          ENDDO
        ENDIF
      END IF !(IRESP == 1) THEN
C----------------------------
C     TETRAHEDRON VOLUME
C----------------------------
      DO I=LFT,LLT
       IF (OFFG(I) == ZERO .OR. ABS(OFFG(I))>ONE) THEN
         DET(I)= ZERO
       ELSE
         X43 = X4(I) - X3(I)
         Y43 = Y4(I) - Y3(I)
         Z43 = Z4(I) - Z3(I)
         X41 = X4(I) - X1(I)
         Y41 = Y4(I) - Y1(I)
         Z41 = Z4(I) - Z1(I)
         X42 = X4(I) - X2(I)
         Y42 = Y4(I) - Y2(I)
         Z42 = Z4(I) - Z2(I)
C
         B1 =  Y43*Z42 - Y42*Z43
         C1 =  Z43*X42 - Z42*X43
         D1 =  X43*Y42 - X42*Y43
C
         DETDP= (X41*B1 + Y41*C1 + Z41*D1)*ONE_OVER_6
         DET(I)= DETDP
       ENDIF
      ENDDO

      !Parith-On treatment
      CALL FOAT_TO_6_FLOAT(LFT  ,LLT  ,DET ,DET6 )

      DO I=LFT,LLT
C----------------------------------
C VOLUME <0 SERA TESTE DANS S4DERI3
C----------------------------------
        !Parith-On treatment
        DO K=1,6
          VOLNOD6(K,NC1(I)) = VOLNOD6(K,NC1(I)) + DET6(K,I)
          VOLNOD6(K,NC2(I)) = VOLNOD6(K,NC2(I)) + DET6(K,I)
          VOLNOD6(K,NC3(I)) = VOLNOD6(K,NC3(I)) + DET6(K,I)
          VOLNOD6(K,NC4(I)) = VOLNOD6(K,NC4(I)) + DET6(K,I)
        ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S4VOLNOD_SM                   source/elements/solid/solide4_sfem/s4volnod3.F
Chd|-- called by -----------
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|====================================================================
      SUBROUTINE S4VOLNOD_SM(VOLNOD6,V,NC1,NC2,NC3,NC4,VOL0,AMU,OFFG,SAV,NEL) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),NEL
      DOUBLE PRECISION
     .   VOLNOD6(6,2*NUMNOD),SAV(NEL,9)
C     REAL
      my_real
     .   V(3,*),OFFG(*),VOL0(*),AMU(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K,INDEX(MVSIZ),NE_SM,J
C     REAL

      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,
     .   DVOL,DETDP,B1DP,C1DP,D1DP
      DOUBLE PRECISION
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
      my_real
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ), 
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),
     .   DET(MVSIZ),VOL(MVSIZ),VOLREF(MVSIZ),D,DXX,DYY,DZZ,
     .   PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),  
     .   PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),  
     .   PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), 
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), 
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ) 

      DOUBLE PRECISION
     .   DET6(6,MVSIZ)
C----------------------------
C     NODAL COORDINATES     
C----------------------------
       NE_SM = 0
C-----case ISMSTR=1,11,2,12
      IF(ISMSTR==1.OR.ISMSTR==11)THEN
       NE_SM = LLT
       DO I=LFT,LLT
        VX1(I)=V(1,NC1(I))
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VX2(I)=V(1,NC2(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
        VX3(I)=V(1,NC3(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        VX4(I)=V(1,NC4(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
        VOLREF(I) = VOL0(I)
        INDEX(I)=I
       ENDDO
       IF(ISMSTR==11)THEN
         DO I=LFT,LLT
           X1(I)=SAV(I,1)
           Y1(I)=SAV(I,4)
           Z1(I)=SAV(I,7)
           X2(I)=SAV(I,2)
           Y2(I)=SAV(I,5)
           Z2(I)=SAV(I,8)
           X3(I)=SAV(I,3)
           Y3(I)=SAV(I,6)
           Z3(I)=SAV(I,9)
           X4(I)=ZERO
           Y4(I)=ZERO
           Z4(I)=ZERO
         ENDDO
       ELSE
         DO I=LFT,LLT
           X1(I)=SAV(I,1)
           Y1(I)=SAV(I,2)
           Z1(I)=SAV(I,3)
           X2(I)=SAV(I,4)
           Y2(I)=SAV(I,5)
           Z2(I)=SAV(I,6)
           X3(I)=SAV(I,7)
           Y3(I)=SAV(I,8)
           Z3(I)=SAV(I,9)
           X4(I)=ZERO
           Y4(I)=ZERO
           Z4(I)=ZERO
         ENDDO
       END IF
      ELSEIF((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3) THEN
       DO I=LFT,LLT
         IF(ABS(OFFG(I))>ONE)THEN
          NE_SM = NE_SM+1
          INDEX(NE_SM)=I
          X1(NE_SM)=SAV(I,1)
          Y1(NE_SM)=SAV(I,2)
          Z1(NE_SM)=SAV(I,3)
          X2(NE_SM)=SAV(I,4)
          Y2(NE_SM)=SAV(I,5)
          Z2(NE_SM)=SAV(I,6)
          X3(NE_SM)=SAV(I,7)
          Y3(NE_SM)=SAV(I,8)
          Z3(NE_SM)=SAV(I,9)
          X4(NE_SM)=ZERO
          Y4(NE_SM)=ZERO
          Z4(NE_SM)=ZERO
          VOLREF(NE_SM) = VOL0(I)
          VX1(NE_SM)=V(1,NC1(I))
          VY1(NE_SM)=V(2,NC1(I))
          VZ1(NE_SM)=V(3,NC1(I))
          VX2(NE_SM)=V(1,NC2(I))
          VY2(NE_SM)=V(2,NC2(I))
          VZ2(NE_SM)=V(3,NC2(I))
          VX3(NE_SM)=V(1,NC3(I))
          VY3(NE_SM)=V(2,NC3(I))
          VZ3(NE_SM)=V(3,NC3(I))
          VX4(NE_SM)=V(1,NC4(I))
          VY4(NE_SM)=V(2,NC4(I))
          VZ4(NE_SM)=V(3,NC4(I))
         ENDIF
       ENDDO
      ENDIF
      IF (NE_SM==0) RETURN
C----------------------------
C     TETRAHEDRON equivalent volume in smalll strain options 
C----------------------------
C-----dt1=0 at T=0, no use to do     
C mean value on rate of vol/vol_ref     
      IF (TT==ZERO) THEN
        VOL(1:NE_SM) = VOLREF(1:NE_SM)
      ELSE
       DO I=1,NE_SM
         X43 = X4(I) - X3(I)
         Y43 = Y4(I) - Y3(I)
         Z43 = Z4(I) - Z3(I)
         X41 = X4(I) - X1(I)
         Y41 = Y4(I) - Y1(I)
         Z41 = Z4(I) - Z1(I)
         X42 = X4(I) - X2(I)
         Y42 = Y4(I) - Y2(I)
         Z42 = Z4(I) - Z2(I)
C
         B1DP =  Y43*Z42 - Y42*Z43
         C1DP =  Z43*X42 - Z42*X43
         D1DP =  X43*Y42 - X42*Y43
         B1(I) =  B1DP
         C1(I) =  C1DP
         D1(I) =  D1DP
         B2(I) =  Y41*Z43 - Y43*Z41
         B3(I) =  Y42*Z41 - Y41*Z42
         B4(I) =  -(B1(I) + B2(I) + B3(I))
C
         C2(I) =  Z41*X43 - Z43*X41
         C3(I) =  Z42*X41 - Z41*X42
         C4(I) =  -(C1(I) + C2(I) + C3(I))
C
         D2(I) =  X41*Y43 - X43*Y41
         D3(I) =  X42*Y41 - X41*Y42
         D4(I) =  -(D1(I) + D2(I) + D3(I))
C
         DETDP= (X41*B1DP + Y41*C1DP + Z41*D1DP)*ONE_OVER_6
         DET(I)= DETDP
       ENDDO
       DO I=1,NE_SM
        D = ONE/DET(I)/SIX
        PX1(I)=-B1(I)*D
        PY1(I)=-C1(I)*D
        PZ1(I)=-D1(I)*D
        PX2(I)=-B2(I)*D
        PY2(I)=-C2(I)*D
        PZ2(I)=-D2(I)*D
        PX3(I)=-B3(I)*D
        PY3(I)=-C3(I)*D
        PZ3(I)=-D3(I)*D
        PX4(I)=-B4(I)*D
        PY4(I)=-C4(I)*D
        PZ4(I)=-D4(I)*D
       END DO
C-----compute equivalent (V/V0)*V0=(rho0/rho)*V0
C-----rho/rho0=1+AMU(t)-dvol       
       DO I=1,NE_SM
        DXX=PX1(I)*VX1(I)+PX2(I)*VX2(I)+
     .      PX3(I)*VX3(I)+PX4(I)*VX4(I)
        DYY=PY1(I)*VY1(I)+PY2(I)*VY2(I)+
     .      PY3(I)*VY3(I)+PY4(I)*VY4(I)
        DZZ=PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+
     .         PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
        DVOL = (DXX+DYY+DZZ)*DT1
        DETDP = VOLREF(I)/MAX(EM20,(ONE+AMU(I)-DVOL))
        VOL(I) = DETDP
       ENDDO
      END IF ! (TT==ZERO) THEN

      !Parith-On treatment
      CALL FOAT_TO_6_FLOAT(1  ,NE_SM  ,VOL ,DET6 )

      IF(ISMSTR==1.OR.ISMSTR==11)THEN
        DO I=LFT,LLT
          !Parith-On treatment
          DO K=1,6
            VOLNOD6(K,NC1(I)) = VOLNOD6(K,NC1(I)) + DET6(K,I)
            VOLNOD6(K,NC2(I)) = VOLNOD6(K,NC2(I)) + DET6(K,I)
            VOLNOD6(K,NC3(I)) = VOLNOD6(K,NC3(I)) + DET6(K,I)
            VOLNOD6(K,NC4(I)) = VOLNOD6(K,NC4(I)) + DET6(K,I)
          ENDDO
        ENDDO
      ELSE
#include "vectorize.inc"
        DO J=1,NE_SM
          I = INDEX(J)
          !Parith-On treatment
          DO K=1,6
            VOLNOD6(K,NC1(I)) = VOLNOD6(K,NC1(I)) + DET6(K,J)
            VOLNOD6(K,NC2(I)) = VOLNOD6(K,NC2(I)) + DET6(K,J)
            VOLNOD6(K,NC3(I)) = VOLNOD6(K,NC3(I)) + DET6(K,J)
            VOLNOD6(K,NC4(I)) = VOLNOD6(K,NC4(I)) + DET6(K,J)
          ENDDO
        ENDDO
      ENDIF
C
      RETURN
      END
