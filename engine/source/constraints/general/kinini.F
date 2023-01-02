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
Chd|  KININI                        source/constraints/general/kinini.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KININI()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
C
      DO I=0,8191
        IBC(I) = 1
        IWL(I) = 1
        IRB(I) = 1
        IRB2(I)= 1
        IVF(I) = 1
        IRV(I) = 1
        IJO(I) = 1
        IRBM(I)= 1
        ILMULT(I)= 1
        IRLK(I)= 1
        ITF(I) = 1
        IKRBE2(I) = 1
        IKRBE3(I) = 1
        IF (MOD(I,2)    <   1) IBC(I)   = 0
        IF (MOD(I,4)    <   2) ITF(I)   = 0
        IF (MOD(I,8)    <   4) IWL(I)   = 0
        IF (MOD(I,16)   <   8) IRB(I)   = 0
        IF (MOD(I,32)   <  16) IVF(I)   = 0
        IF (MOD(I,64)   <  32) IRV(I)   = 0
        IF (MOD(I,128)  <  64) IJO(I)   = 0
        IF (MOD(I,256)  < 128) IRB2(I)  = 0
        IF (MOD(I,512)  < 256) IRBM(I)  = 0
        IF (MOD(I,1024) < 512) ILMULT(I)= 0
        IF (MOD(I,2048) < 1024)IRLK(I)  = 0
        IF (MOD(I,4096) < 2048)IKRBE2(I)= 0
        IF (MOD(I,8192) < 4096)IKRBE3(I)= 0
      ENDDO
c-----------
      RETURN
      END
