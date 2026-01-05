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
!||    aconv2                 ../engine/source/ale/ale2d/aconv2.F
!||--- called by ------------------------------------------------------
!||    aconve                 ../engine/source/ale/aconve.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||====================================================================
      SUBROUTINE ACONV2(VTOT,PHI,FLUX,FLU1,ALE_CONNECT,QMV,IFLG, IXQ, X, IOFF,
     .                  s_flux,s_qmv,offset,nv46)
      USE ALE_CONNECTIVITY_MOD
      use element_mod , only : nixq
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of FLUX array
      integer, intent(in) :: s_qmv !< first dimension of QMV array
      integer, intent(in) :: offset !< offset for FLUX & QMV arrays
      integer, intent(in) :: nv46 !< second dimension of FLUX & QMV arrays
      INTEGER IFLG, IXQ(NIXQ, NUMELQ)
      INTEGER,INTENT(IN) :: IOFF
      my_real VTOT(*), PHI(*), FLUX(s_flux,nv46), FLU1(*), QMV(s_qmv,2*nv46), X(3,NUMNOD)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      my_real :: delta
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IE, IV, IAD2, LGTH,IE_2
      my_real VL(MVSIZ,6), R1, R2, R3, R4
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
         IE =NFT+I
         IE_2 = IE + offset
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
         LGTH = ALE_CONNECT%ee_connect%iad_connect(IE+1) - IAD2
         DO J=1,LGTH
            IV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
            IF(IV > 0)THEN
               VL(I,J)=PHI(IV)*FLUX(IE_2,J)
            ELSEIF(IV == 0)THEN
               VL(I,J)=PHI(IE)*FLUX(IE_2,J)
            ELSE
            ! -IV is segment ID
            !  ebcs : PHI(NUMEL + 1:NSEGFLU) is filled in aconve.F using 
            ! SEGVAR (filled in ebcs[0-9][0-9].F)
               VL(I,J) = PHI(-IV+IOFF) *FLUX(IE_2,J)           
            ENDIF
         ENDDO
      ENDDO
      
      DO I=LFT,LLT
        IE =NFT+I
        delta = HALF * DT1 * (- PHI(IE)*FLU1(I) - VL(I,1)-VL(I,2)-VL(I,3)-VL(I,4) )
        VTOT(I) = VTOT(I) + delta
        VTOT(I)=MAX(EM20,VTOT(I))
      ENDDO
C
      IF(TRIMAT > 0 .AND. IFLG == 1)THEN
         IF (N2D  ==  1) THEN
            DO I=LFT,LLT
               IE =NFT+I
               IE_2 = IE + offset
               R1 = HALF * (X(2, IXQ(2, IE)) + X(2, IXQ(3, IE)))
               R2 = HALF * (X(2, IXQ(3, IE)) + X(2, IXQ(4, IE)))
               R3 = HALF * (X(2, IXQ(4, IE)) + X(2, IXQ(5, IE)))
               R4 = HALF * (X(2, IXQ(5, IE)) + X(2, IXQ(2, IE)))
               !!! Beware of r=0 axis
               IF (R1  ==  ZERO) R1 = ONE
               IF (R2  ==  ZERO) R2 = ONE
               IF (R3  ==  ZERO) R3 = ONE
               IF (R4  ==  ZERO) R4 = ONE
               !!! Divide by mean radius
               QMV(IE_2,1) = QMV(IE_2,1) - VL(I,1) / R1 - PHI(IE)*QMV(IE_2,5) 
               QMV(IE_2,2) = QMV(IE_2,2) - VL(I,2) / R2 - PHI(IE)*QMV(IE_2,6) 
               QMV(IE_2,3) = QMV(IE_2,3) - VL(I,3) / R3 - PHI(IE)*QMV(IE_2,7) 
               QMV(IE_2,4) = QMV(IE_2,4) - VL(I,4) / R4 - PHI(IE)*QMV(IE_2,8) 
            ENDDO
         ELSE
            DO I=LFT,LLT
               IE =NFT+I
               IE_2 = IE + offset
               QMV(IE_2,1) = QMV(IE_2,1) - VL(I,1) - PHI(IE)*QMV(IE_2,5) 
               QMV(IE_2,2) = QMV(IE_2,2) - VL(I,2) - PHI(IE)*QMV(IE_2,6) 
               QMV(IE_2,3) = QMV(IE_2,3) - VL(I,3) - PHI(IE)*QMV(IE_2,7) 
               QMV(IE_2,4) = QMV(IE_2,4) - VL(I,4) - PHI(IE)*QMV(IE_2,8) 
            ENDDO
         ENDIF
      ENDIF
C-----------------------------------------------
      RETURN
      END
