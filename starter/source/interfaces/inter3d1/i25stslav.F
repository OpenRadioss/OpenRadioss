Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I25STSECND                     source/interfaces/inter3d1/i25stslav.F
Chd|-- called by -----------
Chd|        ININTR1                       source/interfaces/interf1/inintr1.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I25STSECND(IPARI,STIFINT,INTBUF_TAB,SFAC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
C     REAL
      my_real
     .   STIFINT(*),SFAC

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, I, J
      my_real
     .   S
C---------------------------------------------
C     MISE A JOUR DES RIGIDITES NODALES EQUIVALENTES (COTE SECOND)
C---------------------------------------------
      NSN   =IPARI(5)
      IF (SFAC >ZERO) THEN
       S = SFAC
      ELSE
       S = ONE
      END IF
      DO I=1,NSN
        J=INTBUF_TAB%NSV(I)
        IF(INTBUF_TAB%STFNS(I).NE.ZERO)
     .     INTBUF_TAB%STFNS(I) = S*STIFINT(J)
      ENDDO
C
      RETURN
      END
