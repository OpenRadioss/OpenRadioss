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
Chd|  NSVISUL                       source/materials/mat_share/nsvisul.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NSVISUL(NEL,PM  ,OFF  ,RHO ,GEO ,
     2                  PID ,SSP ,AIRE ,VOL ,D1  ,
     3                  D2  ,D3  ,D4   ,D5  ,D6  ,
     4                  SV1 ,SV2 ,SV3  ,SV4 ,SV5 ,
     5                  SV6 ,S3 , E3   ,RHO0,RHOREF)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER PID(*)
      my_real
     .   PM(NPROPM,*), OFF(*), RHO(*),GEO(NPROPG,*), SSP(*),
     .   AIRE(*), VOL(*), D1(*), D2(*), D3(*),
     .   D4(*), D5(*), D6(*),SV1(*), SV2(*), SV3(*),
     .   SV4(*), SV5(*), SV6(*),S3(*),E3(*),RHO0(*),RHOREF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DD(MVSIZ), AL(MVSIZ), NRHO(MVSIZ), CNS1, CNS2, CNS3, DAV, PVIS
c=======================================================================     
      DD(1:NEL)=-D1(1:NEL)-D2(1:NEL)-D3(1:NEL)
C      
      IF(N2D > 0) THEN
        DO I=1,NEL
          AL(I)=ZERO
          IF(OFF(I) >= ONE)AL(I)=SQRT(AIRE(I))
        ENDDO
      ELSE 
        DO  I=1,NEL
         AL(I)=ZERO
         IF(OFF(I) >= ONE) AL(I)=EXP(THIRD*LOG(VOL(I)))
        ENDDO
      ENDIF
C-----------------------------------------------
C     Large strain :: Critical damping D = L * rho * c, c = sqrt(A11/rho)
C       BUT c is computed as sqrt(A11/rho0) for most of the materials
C       <=> D = L * sqrt(rho) * sqrt(rho0) * sqrt(A11/rho0)
C     Note : if for a given material, c is computed as sqrt(A11/rho), 
C     ----     then damping will result in L * sqrt(rho) * sqrt(rho0) * c
C              and will be in the ratio sqrt(rho0) / sqrt(rho) wrt critical damping
C                                       < 1 in compression
C                                       > 1 in tension 
C                                       this ratio will be more likely limited.
C
C     Small strain :: Critical damping D = L * rhoref * c, c = sqrt(A11/rhoref)
C       BUT c is computed as sqrt(A11/rho0) for most of the materials
C       <=> D = L * sqrt(rhoref) * sqrt(rho0) * sqrt(A11/rho0)
C
C-----------------------------------------------
      DO I=1,NEL
        NRHO(I) = SQRT(RHOREF(I)*RHO0(I))
      ENDDO
C
      IF(GEO(16,PID(1)) >= ZERO)THEN
        DO I=1,NEL
          CNS1=GEO(16,PID(1))*AL(I)*NRHO(I)*SSP(I)*OFF(I)
          CNS2=GEO(17,PID(1))*AL(I)*NRHO(I)*SSP(I)*OFF(I)
          CNS3=HALF*CNS2
          DAV=DD(I) * THIRD
          PVIS=-CNS1*DD(I)
          SV1(I)= SV1(I) + CNS2 *(D1(I)+DAV)+PVIS
          SV2(I)= SV2(I) + CNS2 *(D2(I)+DAV)+PVIS
          SV3(I)= SV3(I) + CNS2 *(D3(I)+DAV)+PVIS
          SV4(I)= SV4(I) + CNS3 * D4(I)
          SV5(I)= SV5(I) + CNS3 * D5(I)
          SV6(I)= SV6(I) + CNS3 * D6(I)
C          IF(GEO(16,PID(I)) /= ZERO .OR. GEO(17,PID(I))/=ZERO) ISVIS = 1
        ENDDO 
      ELSE
        DO I=1,NEL
          CNS1=GEO(16,PID(1))*NRHO(I)*SSP(I)**2*OFF(I)
          CNS2=GEO(17,PID(1))*NRHO(I)*SSP(I)**2*OFF(I)
          CNS3=HALF*CNS2
          DAV=DD(I) * THIRD
          PVIS=-CNS1*DD(I)
          SV1(I)= SV1(I) + CNS2 *(D1(I)+DAV)+PVIS
          SV2(I)= SV2(I) + CNS2 *(D2(I)+DAV)+PVIS
          SV3(I)= SV3(I) + CNS2 *(D3(I)+DAV)+PVIS
          SV4(I)= SV4(I) + CNS3 * D4(I)
          SV5(I)= SV5(I) + CNS3 * D5(I)
          SV6(I)= SV6(I) + CNS3 * D6(I)
C          IF(GEO(16,PID(I)) /= ZERO .OR. GEO(17,PID(I))/=ZERO) ISVIS = 1
        ENDDO 
      END IF
C
      RETURN
      END
