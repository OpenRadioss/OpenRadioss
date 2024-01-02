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
Chd|  RADIATOFF                     source/constraints/thermic/radiatoff.F
Chd|-- called by -----------
Chd|        DESACTI                       source/elements/desacti.F     
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RADIATOFF(IBCR, FRADIA, IPARG, IGROUPS, IXS, ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCR(NIRADIA,*), IPARG(NPARG,*), IGROUPS(NUMELS), IXS(NIXS,*)
C
      my_real FRADIA(LFACTHER,*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, II, JJ, NG, NEL, MLW, NFT
      my_real, DIMENSION(:), POINTER :: OFFG
C======================================================================|
C------------------------------------------------
C   ACTIVATION/DESACTIVATION DE LA RADIATION
C------------------------------------------------
C      IF(NSPMD > 1) THEN
C        DO N=1,NUMRADIA
C           IF(IBCR(7,N) == 1) THEN
CC          ELEMENTS SOLIDES
C             JJ = IBCR(9,N)
C             DO NG=1,NGROUP
C               MLW= IPARG(1,NG)
C               NEL= IPARG(2,NG)
C               NFT= IPARG(3,NG)
C               IF (MLW == 0 .OR. MLW == 13) CYCLE         
C               OFFG => ELBUF_TAB(NG)%GBUF%OFF
C               DO I=1,NEL
C                 II=I+NFT
C                 IF (IXS(11,II) /= JJ) CYCLE
C                 FRADIA(6,N)=OFFG(I)
C                 GO TO 100
C               ENDDO
C  100          CONTINUE
C             ENDDO
C           ENDIF
C        ENDDO
C      ELSEIF(NSPMD == 1) THEN
        DO N=1,NUMRADIA
           IF(IBCR(7,N) == 1) THEN
C          ELEMENTS SOLIDES
             JJ = IBCR(8,N)
             NG = IGROUPS(JJ)
             MLW= IPARG(1,NG)
             IF (MLW == 0 .OR. MLW == 13) CYCLE         
             OFFG => ELBUF_TAB(NG)%GBUF%OFF
             FRADIA(6,N) = OFFG(IBCR(NIRADIA,N))
           ENDIF
        ENDDO
C     ENDIF
C
      RETURN
      END
