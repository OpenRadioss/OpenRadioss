Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    brezo2                 ../engine/source/ale/ale2d/brezo2.F
!||--- called by ------------------------------------------------------
!||    arezon                 ../engine/source/ale/arezon.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||====================================================================
      SUBROUTINE BREZO2(ALE_CONNECT,VAR,PHI,FLUX,VOL,BHOLE,NM,s_flux,offset)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of FLUX array
      integer, intent(in) :: offset !< offset for FLUX array
      INTEGER BHOLE(NMULT,*),NM
      my_real VAR(*), PHI(*), FLUX(s_flux,4), VOL(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IAD2
      integer :: ie_2
      my_real VOLN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(JMULT == 0)THEN
        DO I=LFT,LLT
          IE =NFT+I
          IE_2 = offset + nft + i
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IF(IV1 <= ZERO)THEN
            IV1=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV1)) /= ABS(BHOLE(NM,IE)))THEN
            IV1=IE
          ENDIF
          IF(IV2 <= 0)THEN
            IV2=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV2)) /= ABS(BHOLE(NM,IE)))THEN
            IV2=IE
          ENDIF
          IF(IV3 <= 0)THEN
            IV3=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV3)) /= ABS(BHOLE(NM,IE)))THEN
            IV3=IE
          ENDIF
          IF(IV4 <= 0)THEN
            IV4=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV4)) /= ABS(BHOLE(NM,IE)))THEN
            IV4=IE
          ENDIF
          VAR(I) = VAR(I) + HALF * DT1 * (PHI(IE)*
     2    (FLUX(IE_2,1)+FLUX(IE_2,2)+FLUX(IE_2,3)+FLUX(IE_2,4))
     3    -PHI(IV1)*FLUX(IE_2,1)-PHI(IV2)*FLUX(IE_2,2)-PHI(IV3)*FLUX(IE_2,3)
     4    -PHI(IV4)*FLUX(IE_2,4)) / VOL(I)
        ENDDO !nexi i
      ELSE
        DO I=LFT,LLT
          IE =NFT+I
          IE_2 = offset + nft + i          
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IF(IV1 <= ZERO)THEN
            IV1=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV1)) /= ABS(BHOLE(NM,IE)))THEN
            IV1=IE
          ENDIF
          IF(IV2 <= ZERO)THEN
            IV2=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV2)) /= ABS(BHOLE(NM,IE)))THEN
            IV2=IE
          ENDIF
          IF(IV3 <= ZERO)THEN
            IV3=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV3)) /= ABS(BHOLE(NM,IE)))THEN
            IV3=IE
          ENDIF
          IF(IV4 <= ZERO)THEN
            IV4=IE
          ELSEIF(BHOLE(NM,IE) < ZERO .AND.ABS(BHOLE(NM,IV4)) /= ABS(BHOLE(NM,IE)))THEN
            IV4=IE
          ENDIF
           VOLN=VOL(I)-DT1 * (FLUX(IE_2,1)+FLUX(IE_2,2)+FLUX(IE_2,3)+FLUX(IE_2,4))
          IF (VOLN > EM15) THEN
            VAR(I) = VAR(I) + HALF * DT1 * (PHI(IE)*
     2                  (FLUX(IE_2,1)+FLUX(IE_2,2)+FLUX(IE_2,3)+FLUX(IE_2,4))
     3                  -PHI(IV1)*FLUX(IE_2,1)-PHI(IV2)*FLUX(IE_2,2)-PHI(IV3)*FLUX(IE_2,3)
     4                  -PHI(IV4)*FLUX(IE_2,4)) / MAX(EM15,VOLN)
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END
