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
!||    i21main_crit_tri   ../engine/source/interfaces/intsort/i21main_crit_tri.F
!||--- called by ------------------------------------------------------
!||    inttri             ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    i21buce_crit       ../engine/source/interfaces/intsort/i21buce_crit.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod      ../common_source/modules/interfaces/intbufdef_mod.F90
!||    intstamp_mod       ../engine/share/modules/intstamp_mod.F
!||====================================================================
      SUBROUTINE I21MAIN_CRIT_TRI(
     1  IPARI  ,INTBUF_TAB     ,X      ,NIN    ,
     2  ITASK  ,V      ,XSLV_L ,XMSR_L ,VSLV_L ,
     3  VMSR_L ,INTSTAMP,X21MSR,V21MSR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK,
     .        IPARI(NPARI,NINTER)
      TYPE(INTSTAMP_DATA) INTSTAMP
C     REAL
      my_real 
     .   X(3,*), V(3,*),
     .   XSLV_L(*), XMSR_L(*), VSLV_L(*), VMSR_L(*),
     .   X21MSR(3), V21MSR(3)
      my_real, DIMENSION(:),ALLOCATABLE :: XSAV0

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, K , ADXLOC
C     REAL
      my_real
     .   STARTT, STOPT
      INTEGER :: NSN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
      NSN   =IPARI(5,NIN)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      IF(NSN > 0) THEN
         ADXLOC = 1+3*(NSN+2)
      ELSE
         ADXLOC = 1
      ENDIF
      CALL I21BUCE_CRIT(
     1  X            ,INTBUF_TAB%NSV,NSN          ,INTSTAMP  ,ITASK    ,
     2  INTBUF_TAB%XSAV,NIN          ,INTBUF_TAB%STFNS,V       ,XSLV_L   ,
     3  XMSR_L       ,VSLV_L       ,VMSR_L       ,X21MSR    ,V21MSR   ,
     4  INTBUF_TAB%XSAV(ADXLOC) )
      
C
      RETURN
      END
