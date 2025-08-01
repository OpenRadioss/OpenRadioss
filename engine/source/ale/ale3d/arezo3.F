Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    arezo3_mod   ../engine/source/ale/ale3d/arezo3.F
!||--- called by ------------------------------------------------------
!||    arezon       ../engine/source/ale/arezon.F
!||====================================================================
      MODULE AREZO3_MOD
      CONTAINS
!||====================================================================
!||    arezo3                 ../engine/source/ale/ale3d/arezo3.F
!||--- called by ------------------------------------------------------
!||    arezon                 ../engine/source/ale/arezon.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||====================================================================
      SUBROUTINE AREZO3(ALE_CONNECT,VAR,PHI,FLUX,VOL,IAD22)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real :: VAR(*), PHI(*), FLUX(6,MVSIZ), VOL(MVSIZ), IAD22(MVSIZ)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IV5, IV6, IAD2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

C NOT YET TREATED WITH INTER22(OBSOLETE), MUST STACK REZONED VALUE WITH SUPERCELLS

      IF(INT22 == 0)THEN
        DO I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IV5=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
          IV6=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
          IF(IV1 <= 0)IV1=IE
          IF(IV2 <= 0)IV2=IE
          IF(IV3 <= 0)IV3=IE
          IF(IV4 <= 0)IV4=IE
          IF(IV5 <= 0)IV5=IE
          IF(IV6 <= 0)IV6=IE
          IF (VOL(I) > ZERO) VAR(I) = VAR(I) + HALF*DT1*(PHI(IE)*
     2     (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))
     3     -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4     -PHI(IV4)*FLUX(4,I)-PHI(IV5)*FLUX(5,I)-PHI(IV6)*FLUX(6,I))/
     5      VOL(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IF(IAD22(I) == ZERO)CYCLE
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IV5=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
          IV6=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1) 
          IF(IV1 <= 0)IV1=IE
          IF(IV2 <= 0)IV2=IE
          IF(IV3 <= 0)IV3=IE
          IF(IV4 <= 0)IV4=IE
          IF(IV5 <= 0)IV5=IE
          IF(IV6 <= 0)IV6=IE
          IF (VOL(I) > ZERO) VAR(I) = VAR(I) + HALF*DT1*(PHI(IE)*
     2     (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))
     3     -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4     -PHI(IV4)*FLUX(4,I)-PHI(IV5)*FLUX(5,I)-PHI(IV6)*FLUX(6,I))/
     5      VOL(I)
        ENDDO       
      ENDIF
C-----------
      RETURN
      END SUBROUTINE AREZO3
      END MODULE AREZO3_MOD