Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  S10VOLNOD3                    source/elements/solid/solide4_sfem/s10volnod3.F
Chd|-- called by -----------
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        S10VOLJ                       source/elements/solid/solide4_sfem/s10volj.F
Chd|====================================================================
      SUBROUTINE S10VOLNOD3(
     1   VOLNOD6, X,       NC,      OFFG,
     2   VOLG,    XDP,     NEL,     NPT,
     3   ISMSTR)
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
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NEL,NC(MVSIZ,10)
      DOUBLE PRECISION
     .   VOLNOD6(6,2*NUMNOD)
C     REAL
      my_real
     .   X(3,NUMNOD),OFFG(NEL)
      DOUBLE PRECISION , DIMENSION(3,SXDP/3), INTENT(IN) :: XDP     
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K,N1,N2,NN,N
      INTEGER IP,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10
      INTEGER IPERM1(10),IPERM2(10),IPERM(10,4)
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
      DATA IPERM/
     .            2, 4, 3, 1, 9,10, 6, 5, 8, 7,
     .            4, 1, 3, 2, 8, 7,10, 9, 5, 6,
     .            1, 4, 2, 3, 8, 9, 5, 7,10, 6,
     .            1, 2, 3, 4, 5, 6, 7, 8, 9,10/

      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10), 
     .   XA(MVSIZ,10),YA(MVSIZ,10),ZA(MVSIZ,10),
     .   XB(MVSIZ,10),YB(MVSIZ,10),ZB(MVSIZ,10),
     .   A4,B4,A4M1,B4M1,AA,DET6(6,MVSIZ),VOLDP(MVSIZ)
      my_real
     .   VOLG(MVSIZ),ALPH,BETA,W
C----------------------------
C     NODAL COORDINATES     
C----------------------------
      IF (ISMSTR==1.OR.ISMSTR==11) RETURN
C----------------------------
      IF(IRESP == 1) THEN
        DO N=1,10
          DO I=1,NEL               
            NN = MAX(1,NC(I,N))    
            XX(I,N)=XDP(1,NN)          
            YY(I,N)=XDP(2,NN)          
            ZZ(I,N)=XDP(3,NN)          
          ENDDO                      
        ENDDO
      ELSE
        DO N=1,10
          DO I=1,NEL               
            NN = MAX(1,NC(I,N))    
            XX(I,N)=X(1,NN)          
            YY(I,N)=X(2,NN)          
            ZZ(I,N)=X(3,NN)          
          ENDDO                      
        ENDDO
      END IF !(IRESP == 1) THEN
C
      DO N=5,10
        N1=IPERM1(N)
        N2=IPERM2(N)
        DO I=1,NEL
          IF(NC(I,N)==0)THEN
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
          ENDIF
        ENDDO
      ENDDO
C      NPT =4      
      ALPH = ZEP5854102
      BETA  = ZEP1381966
      W = FOURTH
      A4 = FOUR * ALPH
      B4 = FOUR * BETA
      A4M1  = A4- ONE
      B4M1  = B4- ONE
C
      DO N=1,4
        DO I=1,NEL
          XA(I,N) = A4M1*XX(I,N)
          YA(I,N) = A4M1*YY(I,N)
          ZA(I,N) = A4M1*ZZ(I,N)
C
          XB(I,N) = B4M1*XX(I,N)
          YB(I,N) = B4M1*YY(I,N)
          ZB(I,N) = B4M1*ZZ(I,N)
        ENDDO
      ENDDO
C
      DO N=5,10
        DO I=1,NEL
          XA(I,N) = A4*XX(I,N)
          YA(I,N) = A4*YY(I,N)
          ZA(I,N) = A4*ZZ(I,N)
C
          XB(I,N) = B4*XX(I,N)
          YB(I,N) = B4*YY(I,N)
          ZB(I,N) = B4*ZZ(I,N)
        ENDDO
      ENDDO
C
      VOLG(1:NEL) =ZERO
      DO IP=1,4
        K1 = IPERM(1,IP)
        K2 = IPERM(2,IP)
        K3 = IPERM(3,IP)
        K4 = IPERM(4,IP)
        K5 = IPERM(5,IP)
        K6 = IPERM(6,IP)
        K7 = IPERM(7,IP)
        K8 = IPERM(8,IP)
        K9 = IPERM(9,IP)
        K10= IPERM(10,IP)
        CALL S10VOLJ(W,
     .   XB(1,K1),XB(1,K2),XB(1,K3),XA(1,K4),XB(1,K5),
     .   XB(1,K6),XB(1,K7),XB(1,K8),XB(1,K9),XB(1,K10),
     .   XA(1,K8),XA(1,K9),XA(1,K10),
     .   YB(1,K1),YB(1,K2),YB(1,K3),YA(1,K4),YB(1,K5),
     .   YB(1,K6),YB(1,K7),YB(1,K8),YB(1,K9),YB(1,K10),
     .   YA(1,K8),YA(1,K9),YA(1,K10),
     .   ZB(1,K1),ZB(1,K2),ZB(1,K3),ZA(1,K4),ZB(1,K5),
     .   ZB(1,K6),ZB(1,K7),ZB(1,K8),ZB(1,K9),ZB(1,K10),
     .   ZA(1,K8),ZA(1,K9),ZA(1,K10),
     .   VOLDP,NEL)
c     
         VOLG(1:NEL) =VOLG(1:NEL) + VOLDP(1:NEL)
      ENDDO
      DO I=1,NEL
       IF (OFFG(I) == ZERO .OR. ABS(OFFG(I))>ONE) THEN
         VOLG(I)= ZERO
       ENDIF
      ENDDO

      !Parith-On treatment
      CALL FOAT_TO_6_FLOAT(1  ,NEL  ,VOLG ,DET6 )

      DO I=1,NEL
C----------------------------------
C VOLUME <0 SERA TESTE DANS S4DERI3
C----------------------------------
        !Parith-On treatment
        DO K=1,6
          VOLNOD6(K,NC(I,1)) = VOLNOD6(K,NC(I,1)) + DET6(K,I)
          VOLNOD6(K,NC(I,2)) = VOLNOD6(K,NC(I,2)) + DET6(K,I)
          VOLNOD6(K,NC(I,3)) = VOLNOD6(K,NC(I,3)) + DET6(K,I)
          VOLNOD6(K,NC(I,4)) = VOLNOD6(K,NC(I,4)) + DET6(K,I)
        ENDDO
      ENDDO
C
      RETURN
      END
