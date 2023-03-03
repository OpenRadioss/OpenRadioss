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
Chd|  LAW120_UPD                    source/materials/mat/mat120/law120_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW120_UPD(NUPARAM ,NUMTABL ,ITABLE ,TABLE ,UPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUPARAM,NUMTABL
      INTEGER ,DIMENSION(NUMTABL) :: ITABLE
      my_real, DIMENSION(NUPARAM), INTENT(INOUT) :: UPARAM
      TYPE(TTABLE) ,DIMENSION(*) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: TABN,NDIM_YLD
      my_real :: YLD0,EPSPMIN,EPSPMAX
C=======================================================================
c     find initial yield value in static yld function
c          and min/max strain rate in table abscissa if available
c
      TABN = ITABLE(1)
      IF (TABN > 0) THEN
        NDIM_YLD = TABLE(TABN)%NDIM
        YLD0 = TABLE(TABN)%Y%VALUES(1)
        UPARAM(5)  = YLD0
        IF (NDIM_YLD > 1) THEN
          EPSPMIN = MINVAL(TABLE(TABN)%X(2)%VALUES)
          EPSPMAX = MAXVAL(TABLE(TABN)%X(2)%VALUES)
          UPARAM(22) = EPSPMIN
          UPARAM(23) = EPSPMAX
        END IF        
      END IF
c-----------
      RETURN
      END
