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
Chd|  SGCOOR3                       source/elements/solid/solide/sgcoor3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGCOOR3(TIME,NPE,X,IXS,
     1   X0, Y0, Z0, VX0,VY0, VZ0, 
     2   SAV,D ,OFF,OFF0,NEL,XDP )
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
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,NEL
      INTEGER IXS(NIXS,*)
C     REAL
      my_real
     .  X(3,*),TIME,D(3,*),OFF(*),OFF0(*),
     .  VX0(MVSIZ,NPE),VY0(MVSIZ,NPE),VZ0(MVSIZ,NPE)
      DOUBLE PRECISION 
     .   X0(MVSIZ,NPE), Y0(MVSIZ,NPE), Z0(MVSIZ,NPE),
     .  SAV(NEL,3*(NPE-1)),XDP(3,*),XD,YD,ZD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N,N2,N3
      INTEGER NC(MVSIZ,NPE)
C     REAL
C-----------------------------------------------
C
      NPE1=NPE-1
      IF (NPE==4) THEN
       DO I=LFT,LLT
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(4,I)
        NC(I,3)=IXS(7,I)
        NC(I,4)=IXS(6,I)
       ENDDO
      ELSE
       DO N=1,NPE
        DO I=LFT,LLT
         NC(I,N)=IXS(N+1,I)
        ENDDO
       ENDDO
      ENDIF 
C----------------------------
C     NODAL COORDINATES INITIALES    |
C----------------------------
      DO N=1,NPE1
       N2 = N + NPE1
       N3 = N2 + NPE1
       DO I=LFT,LLT
        X0(I,N)=SAV(I,N)
        Y0(I,N)=SAV(I,N2)
        Z0(I,N)=SAV(I,N3)
       ENDDO
      ENDDO 
C--------ISMSTR == 12 diff format of SAV 
      IF (ISMSTR == 12.AND.IDTMIN(1)==3 .AND.MTN==1) THEN
         DO N=1,NPE1
           N2 = 3*(N-1)+1
           DO I=LFT,LLT
            IF (OFF(I) <= ONE ) CYCLE 
            X0(I,N)=SAV(I,N2)
            Y0(I,N)=SAV(I,N2+1)
            Z0(I,N)=SAV(I,N2+2)
           ENDDO
         ENDDO
      END IF !(ISMSTR == 12.AND.IDTMIN(1)==3) THEN
C----------------------------
C     DISPLACEMENT     |
C----------------------------
      IF(IRESP==1)THEN
       DO N=1,NPE1
        DO I=LFT,LLT
         N2 = NC(I,N)
         N3 = NC(I,NPE)
         XD =XDP(1,N2)-XDP(1,N3)-X0(I,N)
         YD =XDP(2,N2)-XDP(2,N3)-Y0(I,N)
         ZD =XDP(3,N2)-XDP(3,N3)-Z0(I,N)
         VX0(I,N) = XD
         VY0(I,N) = YD
         VZ0(I,N) = ZD
        ENDDO
       ENDDO 
      ELSE      
      DO N=1,NPE1
       DO I=LFT,LLT
        N2 = NC(I,N)
        N3 = NC(I,NPE)
        VX0(I,N)=X(1,N2)-X(1,N3)-X0(I,N)
        VY0(I,N)=X(2,N2)-X(2,N3)-Y0(I,N)
        VZ0(I,N)=X(3,N2)-X(3,N3)-Z0(I,N)
       ENDDO
      ENDDO 
      END IF !(IRESP==1)THEN
C      
      DO I=LFT,LLT
       X0(I,NPE)=ZERO
       Y0(I,NPE)=ZERO
       Z0(I,NPE)=ZERO
       VX0(I,NPE)=ZERO
       VY0(I,NPE)=ZERO
       VZ0(I,NPE)=ZERO
      ENDDO
C      
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        DO I=LFT,LLT
         OFF0(I)=OFF(I)
        ENDDO
C-------law1 special two increment        
       IF (MTN/=1) THEN
         DO N=1,NPE1
          DO I=LFT,LLT
           IF (OFF(I) <= ONE ) CYCLE 
            VX0(I,N)=D(1,NC(I,N))-D(1,NC(I,NPE)) 
            VY0(I,N)=D(2,NC(I,N))-D(2,NC(I,NPE))  
            VZ0(I,N)=D(3,NC(I,N))-D(3,NC(I,NPE)) 
          ENDDO
         ENDDO
         DO N=1,NPE1
           N2 = 3*(N-1)+1
           DO I=LFT,LLT
            IF (OFF(I) <= ONE ) CYCLE 
            X0(I,N)=SAV(I,N2)
            Y0(I,N)=SAV(I,N2+1)
            Z0(I,N)=SAV(I,N2+2)
           ENDDO
         ENDDO
       END IF
      END IF !(ISMSTR == 10.AND.IDTMIN(1)==3) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  SGPARAV3                      source/elements/solid/solide/sgcoor3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGPARAV3(NPE,X,IXS,
     1                   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE
      INTEGER IXS(NIXS,*)
C     REAL
      my_real
     .  X(3,*),RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),
     .  TX(*), TY(*), TZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N
      INTEGER NC(MVSIZ,NPE)
C     REAL
      my_real
     .  X0(MVSIZ,NPE), Y0(MVSIZ,NPE), Z0(MVSIZ,NPE)
C-----------------------------------------------
C
      NPE1=NPE-1
      IF (NPE==4.OR.NPE==10) THEN
       DO I=LFT,LLT
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(4,I)
        NC(I,3)=IXS(7,I)
        NC(I,4)=IXS(6,I)
       ENDDO
      ELSEIF (NPE==6) THEN
       DO I=LFT,LLT
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(3,I)
        NC(I,3)=IXS(4,I)
        NC(I,4)=IXS(6,I)
        NC(I,5)=IXS(7,I)
        NC(I,6)=IXS(8,I)
       ENDDO
      ELSE
       DO N=1,NPE
        DO I=LFT,LLT
         NC(I,N)=IXS(N+1,I)
        ENDDO
       ENDDO
      ENDIF 
C----------------------------
C     NODAL COORDINATES     /
C----------------------------
      DO N=1,NPE
       DO I=LFT,LLT
        X0(I,N)=X(1,NC(I,N))
        Y0(I,N)=X(2,NC(I,N))
        Z0(I,N)=X(3,NC(I,N))
       ENDDO
      ENDDO 
C----------------------------
C     R,S,T     |
C----------------------------
      SELECT CASE (NPE)
      CASE(4,10)                                             
       DO I=LFT,LLT
        RX(I) =  X0(I,1) - X0(I,4)
        RY(I) =  Y0(I,1) - Y0(I,4)
        RZ(I) =  Z0(I,1) - Z0(I,4)
C
        SX(I) =  X0(I,2) - X0(I,4)
        SY(I) =  Y0(I,2) - Y0(I,4)
        SZ(I) =  Z0(I,2) - Z0(I,4)
C
        TX(I) =  X0(I,3) - X0(I,4)
        TY(I) =  Y0(I,3) - Y0(I,4)
        TZ(I) =  Z0(I,3) - Z0(I,4)
       ENDDO
      CASE(6)                                          
       DO I=LFT,LLT
        RX(I) =  X0(I,3) + X0(I,6) - X0(I,1)- X0(I,4)
        RY(I) =  Y0(I,3) + Y0(I,6) - Y0(I,1)- Y0(I,4)
        RZ(I) =  Z0(I,3) + Z0(I,6) - Z0(I,1) - Z0(I,4)
C
        SX(I) = (X0(I,4)+X0(I,5)+X0(I,6)-X0(I,1)+X0(I,2)+X0(I,3))*THIRD
        SY(I) = (Y0(I,4)+Y0(I,5)+Y0(I,6)-Y0(I,1)+Y0(I,2)+Y0(I,3))*THIRD
        SZ(I) = (Z0(I,4)+Z0(I,5)+Z0(I,6)-Z0(I,1)+Z0(I,2)+Z0(I,3))*THIRD
C
        TX(I) = X0(I,2) + X0(I,5)- X0(I,1) - X0(I,4)
        TY(I) = Y0(I,2) + Y0(I,5)- Y0(I,1) - Y0(I,4)
        TZ(I) = Z0(I,2) + Z0(I,5)- Z0(I,1) - Z0(I,4)
       ENDDO
      CASE(8)                                          
       DO I=LFT,LLT
        RX(I) = X0(I,3)+X0(I,4)+X0(I,7)+X0(I,8)-
     .         (X0(I,1)+X0(I,2)+X0(I,5)+X0(I,6)) 
        RY(I) = Y0(I,3)+Y0(I,4)+Y0(I,7)+Y0(I,8)-
     .         (Y0(I,1)+Y0(I,2)+Y0(I,5)+Y0(I,6))
        RZ(I) = Z0(I,3)+Z0(I,4)+Z0(I,7)+Z0(I,8)-
     .         (Z0(I,1)+Z0(I,2)+Z0(I,5)+Z0(I,6))
C
        SX(I) = X0(I,5)+X0(I,6)+X0(I,7)+X0(I,8)-
     .         (X0(I,1)+X0(I,2)+X0(I,3)+X0(I,4)) 
        SY(I) = Y0(I,5)+Y0(I,6)+Y0(I,7)+Y0(I,8)-
     .         (Y0(I,1)+Y0(I,2)+Y0(I,3)+Y0(I,4))
        SZ(I) = Z0(I,5)+Z0(I,6)+Z0(I,7)+Z0(I,8)-
     .         (Z0(I,1)+Z0(I,2)+Z0(I,3)+Z0(I,4))
C
        TX(I) = X0(I,3)+X0(I,2)+X0(I,7)+X0(I,6)-
     .         (X0(I,1)+X0(I,4)+X0(I,5)+X0(I,8)) 
        TY(I) = Y0(I,3)+Y0(I,2)+Y0(I,7)+Y0(I,6)-
     .         (Y0(I,1)+Y0(I,4)+Y0(I,5)+Y0(I,8))
        TZ(I) = Z0(I,3)+Z0(I,2)+Z0(I,7)+Z0(I,6)-
     .         (Z0(I,1)+Z0(I,4)+Z0(I,5)+Z0(I,8))
C
       ENDDO
      END SELECT 
C
      RETURN
      END
