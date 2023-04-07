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
Chd|  IG3DCUMU3                     source/elements/ige3d/ig3dcumu3.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IG3DCUMU3(
     1   IXIG3D,  KXIG3D,  NCTRL,   OFFG,
     2   E,       FX,      FY,      FZ,
     3   BtDBAloc,STIG,    STIFN,   NEL,
     4   NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER NCTRL,KXIG3D(NIXIG3D,*),IXIG3D(*)
      my_real
     .   OFFG(*),E(3,*),BtDBAloc(3*NCTRL,*),
     .   FX(MVSIZ,NCTRL),FY(MVSIZ,NCTRL),FZ(MVSIZ,NCTRL),
     .   STIFN(*),STIG(MVSIZ,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, JJ, INCTRL
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = ZERO
      DO I=1,NEL
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO I=1,NEL
         IF(OFFG(I)<ZERO)THEN
           DO J=1,NCTRL    
            FX(I,J)=ZERO
            FY(I,J)=ZERO
            FZ(I,J)=ZERO
           ENDDO
         ENDIF
        ENDDO
      ENDIF

      DO I=1,NEL
       DO J=1,NCTRL
        IF( J <= KXIG3D(3,I+NFT) ) THEN
         INCTRL = IXIG3D(KXIG3D(4,I+NFT)+J-1)                   
         E(1,INCTRL)=E(1,INCTRL)+FX(I,J)
         E(2,INCTRL)=E(2,INCTRL)+FY(I,J)
         E(3,INCTRL)=E(3,INCTRL)+FZ(I,J)
         STIFN(INCTRL)=STIFN(INCTRL)+STIG(I,J)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

