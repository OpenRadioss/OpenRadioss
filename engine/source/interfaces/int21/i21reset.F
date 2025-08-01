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
!||    i21reset        ../engine/source/interfaces/int21/i21reset.F
!||--- called by ------------------------------------------------------
!||    inttri          ../engine/source/interfaces/intsort/inttri.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE I21RESET(
     1  IPARI  ,INTBUF_TAB  ,NIN    ,ITASK  )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, ITASK, IPARI(NPARI,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, FIRST, LAST,
     .        NSN
C     REAL
      my_real
     .   STARTT, STOPT, TOL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C---------------------------------------------------- 
C  Calcul des adresses des buffers d'interfaces. 
C----------------------------------------------------
C
      NSN   =IPARI(5,NIN)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      FIRST = 1 + NSN*ITASK / NTHREAD
      LAST = NSN*(ITASK+1) / NTHREAD
      DO N=FIRST,LAST
          INTBUF_TAB%IRTLM(2*(N-1)+1)=0
          INTBUF_TAB%CSTS(2*(N-1)+1)  =-ONE
          INTBUF_TAB%CSTS(2*(N-1)+2)=-ONE
      END DO
C
      RETURN
      END
