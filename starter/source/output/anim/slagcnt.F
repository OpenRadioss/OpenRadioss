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
!||    slagcnt   ../starter/source/output/anim/slagcnt.F
!||--- called by ------------------------------------------------------
!||    dd_ani    ../starter/source/output/anim/dd_ani.F
!||====================================================================
      SUBROUTINE SLAGCNT(IPARG,NSLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NSLAG
C-----------------------------------------------
      INTEGER NG,NEL,ITY,IALEL
C
      NSLAG=0
      DO 10 NG=1,NGROUP
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        IALEL =IPARG(7,NG)+IPARG(11,NG)
        IF(ITY==1 .AND. IALEL==0)NSLAG=NSLAG+NEL
 10   CONTINUE
C
      RETURN
      END
