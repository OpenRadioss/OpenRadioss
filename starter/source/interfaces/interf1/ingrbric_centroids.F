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
C  
C   Centroids definition for secnd side of Interface Type18 and colocated scheme.
C   Input for secnd side is a GRBRIC. 
!||====================================================================
!||    ingrbric_centroids   ../starter/source/interfaces/interf1/ingrbric_centroids.F
!||--- called by ------------------------------------------------------
!||    lecins               ../starter/source/interfaces/interf1/lecins.F
!||--- uses       -----------------------------------------------------
!||    format_mod           ../starter/share/modules1/format_mod.F90
!||====================================================================
      SUBROUTINE INGRBRIC_CENTROIDS(IBUFSSG,MSV, IXS, NBRIC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE FORMAT_MOD , ONLY : FMW_10I
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NBRIC
      INTEGER,INTENT(IN)    :: IBUFSSG(NBRIC), IXS(NIXS,NUMELS)
      INTEGER,INTENT(INOUT), TARGET :: MSV(1:NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r a i b l e s
C-----------------------------------------------
      INTEGER :: I
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       
      DO I=1,NBRIC
        MSV(I)=IBUFSSG(I)
      ENDDO   

      !PRINTOUT
      IF(IPRI >= 1) THEN
        WRITE(IOUT,'(/,A,/)')' CENTROID EXTRACTED FROM SOLID ELEMENTS : '
        WRITE(IOUT,FMT=FMW_10I)(IXS(11,MSV(I)),I=1,NBRIC)
      ENDIF
C------------------------------------------------------------
      RETURN
      END

