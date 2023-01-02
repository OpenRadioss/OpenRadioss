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
Chd|  ACONV2                        source/ale/ale2d/aconv2.F     
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ACONV2(VTOT,PHI,FLUX,FLU1,ALE_CONNECT,QMV,IFLG, IXQ, X,NVAR, IOFF, ITRIMAT)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLG, IXQ(NIXQ, NUMELQ),NVAR
      INTEGER,INTENT(IN) :: IOFF, ITRIMAT
      my_real VTOT(*), PHI(*), FLUX(4,*), FLU1(*), QMV(8,*), X(3,NUMNOD)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IE, IV, IAD2, LGTH
      my_real VL(6,MVSIZ), R1, R2, R3, R4
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
         IE =NFT+I
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
         LGTH = ALE_CONNECT%ee_connect%iad_connect(IE+1) - IAD2
         DO J=1,LGTH
            IV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
            IF(IV > 0)THEN
               VL(J,I)=PHI(IV)*FLUX(J,I)
            ELSEIF(IV == 0)THEN
               VL(J,I)=PHI(IE)*FLUX(J,I)
            ELSE
            ! -IV is segment ID
            !  ebcs : PHI(NUMEL + 1:NSEGFLU) is filled in aconve.F using 
            ! SEGVAR (filled in ebcs[0-9][0-9].F)
               VL(J,I) = PHI(-IV+IOFF) *FLUX(J,I)           
            ENDIF
         ENDDO
      ENDDO
      
      DO I=LFT,LLT
        IE =NFT+I
        VTOT(I) = VTOT(I) + HALF * DT1 * (- PHI(IE)*FLU1(I) - VL(1,I)-VL(2,I)-VL(3,I)-VL(4,I) )
        VTOT(I)=MAX(EM20,VTOT(I))
      ENDDO
C
      IF(TRIMAT > 0 .AND. IFLG == 1)THEN
         IF (N2D  ==  1) THEN
            DO I=LFT,LLT
               IE =NFT+I
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
               QMV(1,I) = QMV(1,I) - VL(1,I) / R1 - PHI(IE)*QMV(5,I) 
               QMV(2,I) = QMV(2,I) - VL(2,I) / R2 - PHI(IE)*QMV(6,I) 
               QMV(3,I) = QMV(3,I) - VL(3,I) / R3 - PHI(IE)*QMV(7,I) 
               QMV(4,I) = QMV(4,I) - VL(4,I) / R4 - PHI(IE)*QMV(8,I) 
            ENDDO
         ELSE
            DO I=LFT,LLT
               IE =NFT+I
               QMV(1,I) = QMV(1,I) - VL(1,I) - PHI(IE)*QMV(5,I) 
               QMV(2,I) = QMV(2,I) - VL(2,I) - PHI(IE)*QMV(6,I) 
               QMV(3,I) = QMV(3,I) - VL(3,I) - PHI(IE)*QMV(7,I) 
               QMV(4,I) = QMV(4,I) - VL(4,I) - PHI(IE)*QMV(8,I) 
            ENDDO
         ENDIF
      ENDIF
C-----------------------------------------------
      RETURN
      END
