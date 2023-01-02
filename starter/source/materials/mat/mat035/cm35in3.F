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
Chd|  CM35IN3                       source/materials/mat/mat035/cm35in3.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CM35IN3(ELBUF_STR,THK ,AREA,NEL,NLAY,
     .                   NPTR,NPTS,NPTT,IGTYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NLAY,NPTR,NPTS,NPTT,IGTYP
      my_real
     .   THK(*),AREA(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IL,IR,IS,IT
      my_real,
     .  DIMENSION(:) ,POINTER :: UVAR
C======================================================================|
C---  Initialise Volume dans UVAR 
      II = 2*NEL
      DO I=1,NEL
        DO IL=1,NLAY
          IF (IGTYP == 51 .OR. IGTYP==52)NPTT = ELBUF_STR%BUFLY(IL)%NPTT
          DO IR=1,NPTR
            DO IS=1,NPTS
              DO IT=1,NPTT
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR
                UVAR(II+I) = AREA(I)*THK(I)
              ENDDO                                                         
            ENDDO                                                         
          ENDDO                                                            
        ENDDO                                                              
      ENDDO                                                                
C-----------
      RETURN
      END
