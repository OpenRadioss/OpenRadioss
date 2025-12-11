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
!||    a4conv3                ../engine/source/ale/ale3d/a4conv3.F
!||--- called by ------------------------------------------------------
!||    aconve                 ../engine/source/ale/aconve.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||====================================================================
      SUBROUTINE A4CONV3(VTOT ,PHI,FLUX,FLU1,
     .                   ALE_CONNECT,IOFF,s_flux,offset,nv46,nel,nft)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of FLUX array
      integer, intent(in) :: offset !< offset for FLUX array
      integer, intent(in) :: nv46  !< second dimension of FLUX array
      integer, intent(in) :: nel   !< number of elements of the group
      integer, intent(in) :: nft   !< first element index of the group      
      INTEGER IOFF
      my_real VTOT(*), PHI(*), FLUX(s_flux,nv46), FLU1(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV,J,IAD2
      my_real VALVOIS(MVSIZ,nv46),VALEL(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,nel
        IE =NFT+I
        VALEL(I)=PHI(IE)
      ENDDO
      
      DO I=1,nel
         IE =NFT+I
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
!
         J = 2
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
         IF(IV > 0)THEN
            VALVOIS(I,J)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(I,J)=PHI(IE)
         ELSE
            VALVOIS(I,J)=PHI(-IV+IOFF)
         ENDIF
!
         J = 4
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
         IF(IV > 0)THEN
            VALVOIS(I,J)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(I,J)=PHI(IE)
         ELSE
            VALVOIS(I,J)=PHI(-IV+IOFF)
         ENDIF
!
         J = 5
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
         IF(IV > 0)THEN
            VALVOIS(I,J)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(I,J)=PHI(IE)
         ELSE
            VALVOIS(I,J)=PHI(-IV+IOFF)
         ENDIF
!
         J = 6
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
         IF(IV > 0)THEN
            VALVOIS(I,J)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(I,J)=PHI(IE)
         ELSE
            VALVOIS(I,J)=PHI(-IV+IOFF)
         ENDIF
      ENDDO
C
      DO I=1,nel
        ie = nft+i+offset
        VTOT(I) = VTOT(I) + HALF * DT1 *(
     1                                   -VALEL(I)*FLU1(I)
     2                                   -VALVOIS(I,2)*FLUX(ie,2)
     3                                   -VALVOIS(I,4)*FLUX(ie,4)
     4                                   -VALVOIS(I,5)*FLUX(ie,5)
     5                                   -VALVOIS(I,6)*FLUX(ie,6) 
     6                                   )
      ENDDO
C-----------------------------------------------
      RETURN
      END
C
