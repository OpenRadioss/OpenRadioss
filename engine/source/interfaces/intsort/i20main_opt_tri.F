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
!||    i20main_opt_tri   ../engine/source/interfaces/intsort/i20main_opt_tri.F
!||--- called by ------------------------------------------------------
!||    imp_tripi         ../engine/source/implicit/imp_int_k.F
!||    inttri            ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    i20optcd          ../engine/source/interfaces/intsort/i20optcd.F
!||    i20optcde         ../engine/source/interfaces/intsort/i20optcd.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod     ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE I20MAIN_OPT_TRI(
     1                  IPARI   ,X    ,V      ,
     2                  NIN     ,ITASK ,COUNT_REMSLV,COUNT_REMSLVE,
     3                  INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), ITASK, NIN,
     * COUNT_REMSLV(*), COUNT_REMSLVE(*)
C     REAL
      my_real 
     .   X(*), V(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, I, IFQ, I_STOK,NSNF,NSNL
C     REAL
      my_real
     .   STARTT,GAP,STOPT
C=======================================================================
C     ALLOCATABLE
C=======================================================================
C-----------------------------------------------
      IFQ =IPARI(31,NIN) 
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN

C
      GAP =INTBUF_TAB%VARIABLES(2)
C
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
      I_STOK = INTBUF_TAB%I_STOK(1)
      CALL I20OPTCD(
     1INTBUF_TAB%NSV,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,INTBUF_TAB%XA,I_STOK   ,
     2INTBUF_TAB%IRECTM,GAP      ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IPARI(21,NIN),
     3INTBUF_TAB%STFA,ITASK   ,INTBUF_TAB%STFM,IFQ   ,INTBUF_TAB%IFPEN,
     4INTBUF_TAB%CAND_FX,INTBUF_TAB%CAND_FY,INTBUF_TAB%CAND_FZ,NIN   ,IPARI(5,NIN) ,
     5INTBUF_TAB%VARIABLES(16),IPARI(39,NIN),COUNT_REMSLV )

C-----------------------------------------------------------
C      EDGES
C-----------------------------------------------------------
      I_STOK = INTBUF_TAB%I_STOK_E(1)
      CALL I20OPTCDE(
     1  INTBUF_TAB%LCAND_N ,INTBUF_TAB%LCAND_S ,INTBUF_TAB%XA ,I_STOK        ,
     2  INTBUF_TAB%IXLINS ,INTBUF_TAB%IXLINM ,GAP      ,NIN     ,
     3  V        ,INTBUF_TAB%GAP_SE ,INTBUF_TAB%GAP_ME ,IPARI(21,NIN) ,
     4  INTBUF_TAB%STFS ,ITASK       ,IPARI(53,NIN) ,INTBUF_TAB%STF ,
     5  COUNT_REMSLVE )
C-----------------------------------------------------------
C
      RETURN
      END
