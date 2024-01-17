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
Chd|  S10GET_X3                     source/elements/solid/solide10/s10get_x3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10GET_X3(
     1       X,     XDP,      DR,    NUMNOD,
     2      XX,      YY,      ZZ,        NC,
     3   ISROT,   IRESP,    NEL )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISROT,NUMNOD,IRESP,NEL
      INTEGER, DIMENSION(MVSIZ,10), INTENT(IN) :: NC
     
      my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: X,DR
      DOUBLE PRECISION, DIMENSION(3,NUMNOD), INTENT(IN) ::XDP
      DOUBLE PRECISION, DIMENSION(MVSIZ,10), INTENT(INOUT) ::
     .                                      XX, YY, ZZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(10),IPERM2(10),N,N1,N2,NN,IUN,MXT_1
C     REAL
      my_real
     .   DVX,DVY,DVZ,DX,DY,DZ
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      IF (IRESP==1)THEN
        DO N=1,4
           DO I=1,NEL
             NN = NC(I,N)
             XX(I,N)=XDP(1,NN)
             YY(I,N)=XDP(2,NN)
             ZZ(I,N)=XDP(3,NN)
           END DO
        END DO
C        
        IF(ISROT==0.OR.ISROT==2)THEN
          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
              IF(NN/=0)THEN
                 XX(I,N)=XDP(1,NN)
                 YY(I,N)=XDP(2,NN)
                 ZZ(I,N)=XDP(3,NN)      
              ELSE
                N1=IPERM1(N)
                N2=IPERM2(N)
                XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
                YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
                ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
              END IF
            END DO
          END DO
        ELSEIF(ISROT==1)THEN
          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
              N1=IPERM1(N)
              N2=IPERM2(N)
              DX = (YY(I,N2)-YY(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
     .           - (ZZ(I,N2)-ZZ(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
              DY = (ZZ(I,N2)-ZZ(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
     .           - (XX(I,N2)-XX(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
              DZ = (XX(I,N2)-XX(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
     .           - (YY(I,N2)-YY(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))

              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2)) + ONE_OVER_8 * DX
              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2)) + ONE_OVER_8 * DY
              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2)) + ONE_OVER_8 * DZ
            END DO
          END DO
        END IF
      ELSE !DP
        DO N=1,4
           DO I=1,NEL
             NN = NC(I,N)
             XX(I,N)=X(1,NN)
             YY(I,N)=X(2,NN)
             ZZ(I,N)=X(3,NN)
           END DO
        END DO
        IF(ISROT==0.OR.ISROT==2)THEN
          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
              IF(NN/=0)THEN
                 XX(I,N)=X(1,NN)
                 YY(I,N)=X(2,NN)
                 ZZ(I,N)=X(3,NN)      
              ELSE
                N1=IPERM1(N)
                N2=IPERM2(N)
                XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
                YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
                ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
              END IF
            END DO
          END DO
        ELSEIF(ISROT==1)THEN
          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
              N1=IPERM1(N)
              N2=IPERM2(N)
              DX = (YY(I,N2)-YY(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
     .           - (ZZ(I,N2)-ZZ(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
              DY = (ZZ(I,N2)-ZZ(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
     .           - (XX(I,N2)-XX(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
              DZ = (XX(I,N2)-XX(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
     .           - (YY(I,N2)-YY(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))

              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2)) + ONE_OVER_8 * DX
              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2)) + ONE_OVER_8 * DY
              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2)) + ONE_OVER_8 * DZ
            END DO
          END DO
        END IF
      END IF ! (IRESP==1)THEN
C-----------
      RETURN
      END
