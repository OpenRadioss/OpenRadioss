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
!||    i7stsecnd       ../starter/source/interfaces/inter3d1/i7stslav.F
!||--- called by ------------------------------------------------------
!||    inintr1         ../starter/source/interfaces/interf1/inintr1.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE I7STSECND(IPARI,STIFINT,INTBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      my_real
     .   STIFINT(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN,
     .   I, J,
     .   INACTI,IGAP,MULTIMP,NSN0,ITY,NSNE,S
C---------------------------------------------
C     MISE A JOUR DES RIGIDITES NODALES EQUIVALENTES (COTE SECOND)
C---------------------------------------------
      NSN   =IPARI(5)
      DO I=1,NSN
        J=INTBUF_TAB%NSV(I)
        IF(INTBUF_TAB%STFNS(I)/=ZERO)
     .     INTBUF_TAB%STFNS(I) = STIFINT(J)
      ENDDO
C
      RETURN
      END
