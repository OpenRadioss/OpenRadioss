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
Chd|  RMATACCE                      source/materials/mat/mat013/rmatacce.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RMATACCE(RBYM ,ARBYM ,ARRBYM ,VRBYM,VRRBYM,
     2                    IRBYM,LNRBYM,X      ,A    ,AR    ,
     3                    V    ,VR    ,KIND   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IRBYM(NIRBYM,*),LNRBYM(*), KIND(*)
C     REAL
        my_real
     .     X(3,*), A(3,*), AR(3,*), VR(3,*),V(3,*),
     .     VRRBYM(3,*), ARBYM(3,*),ARRBYM(3,*),VRBYM(3,*),
     .     RBYM(NFRBYM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, M, NSL, NSLT
C     REAL
        my_real RX, RY, RZ, MSX, MSY, MSZ, FSX, FSY, FSZ,
     .          XG,YG,ZG,USDT,V1X2,V2X1,V2X3,V3X2,V3X1,V1X3,
     .          VX2,VX1,VX3,VG(3)

C======================================================================|
        USDT = ONE /DT12

!$OMP DO

        DO M=1,NRBYM
C
          NSL = IRBYM(2,M)
          XG = RBYM(2,M)
          YG = RBYM(3,M)
          ZG = RBYM(4,M)
C
          NSLT= KIND(M)-1
C
          VG(1) = VRRBYM(1,M) + ARRBYM(1,M)*DT12
          VG(2) = VRRBYM(2,M) + ARRBYM(2,M)*DT12
          VG(3) = VRRBYM(3,M) + ARRBYM(3,M)*DT12
          DO I=1,NSL
            N = LNRBYM(NSLT + I)
C
            RX = X(1,N) - XG
            RY = X(2,N) - YG
            RZ = X(3,N) - ZG
C
            AR(1,N) = (VG(1) - VR(1,N)) * USDT
            AR(2,N) = (VG(2) - VR(2,N)) * USDT
            AR(3,N) = (VG(3) - VR(3,N)) * USDT
C
            V1X2 = VG(1)*RY
            V2X1 = VG(2)*RX
            V2X3 = VG(2)*RZ
            V3X2 = VG(3)*RY
            V3X1 = VG(3)*RX
            V1X3 = VG(1)*RZ
C
            VX1 = V2X3 - V3X2
            VX2 = V3X1 - V1X3
            VX3 = V1X2 - V2X1
C
            A(1,N)= ARBYM(1,M) + USDT*(
     .       VRBYM(1,M) + VX1+HALF*DT2*(VG(2)*VX3-VG(3)*VX2)-V(1,N) )
            A(2,N)= ARBYM(2,M) + USDT*(
     .        VRBYM(2,M) + VX2+HALF*DT2*(VG(3)*VX1-VG(1)*VX3)-V(2,N))
            A(3,N)= ARBYM(3,M) + USDT*(
     .        VRBYM(3,M)+VX3+HALF*DT2*(VG(1)*VX2-VG(2)*VX1)-V(3,N))
C compute rotational velocity
C
          ENDDO

          VRBYM(1,M) = VRBYM(1,M) + ARBYM(1,M)*DT12
          VRBYM(2,M) = VRBYM(2,M) + ARBYM(2,M)*DT12
          VRBYM(3,M) = VRBYM(3,M) + ARBYM(3,M)*DT12
C
          VRRBYM(1,M) = VG(1)
          VRRBYM(2,M) = VG(2)
          VRRBYM(3,M) = VG(3)
        ENDDO

!$OMP END DO

C---
        RETURN
      END
