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
C
Chd|====================================================================
Chd|  I2MASTER                      source/interfaces/interf1/i2master.F
Chd|-- called by -----------
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2MASTER(NSV,MSR,IRECTM,IPARI,
     .                   TAG,MSRU,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
C-----------------------------------------------------------------------
C recontruit MSR et IRECTM en fonction des master nodes utiles
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSV(*), MSR(*), IRECTM(4,*), IPARI(*)
      INTEGER TAG(*), MSRU(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, I, J, NIR,
     .        K, M, NM, NMNU
C-----------------------------------------------
      NRTS=IPARI(3)
      NRTM=IPARI(4)
      NSN =IPARI(5)
      NMN =IPARI(6)
      NTY =IPARI(7)
      NMNU = 0
C
      DO I = 1, NUMNOD
        TAG(I) = 0
      ENDDO
C
      IF (N2D.EQ.0) THEN
        NIR = 4
      ELSE
        NIR = 2
      ENDIF
C determination des master nodes reellement utilises
      DO I = 1, NSN
        J = INTBUF_TAB%IRTLM(I)
        DO K = 1, NIR
          M = IRECTM(K,J)
          IF (TAG(M).EQ.0) THEN
            NMNU = NMNU + 1
            TAG(M) = NMNU
            MSRU(NMNU) = MSR(M)
          ENDIF
        ENDDO
      ENDDO
C mise a jour de MSR
      DO I = 1, NMNU
        MSR(I) = MSRU(I)
      ENDDO
C mise a jour de irectm
C---------------------------------------------------------------
C     LE NOEUD LOCAL EST REMPLACE PAR LE NOEUD GLOBAL DANS IRECT
C---------------------------------------------------------------
      DO I = 1, NRTM
        DO K = 1, NIR
          M = IRECTM(K,I)
          NM = TAG(M)
          IF (NM.EQ.0) THEN
            IRECTM(K,I) = 1
          ELSE
CGW            IRECTM(K,I) = NM
            IRECTM(K,I) = MSR(NM)
          ENDIF
        ENDDO
      ENDDO 
C mise a jour de IPARI et offset de IRTLM
      IPARI(6) = NMNU
C
      RETURN
      END
