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
Chd|  I10MAIN_OPT_TRI               source/interfaces/intsort/i10opt_opt_tri.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I10OPTCD                      source/interfaces/intsort/i10optcd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I10MAIN_OPT_TRI(
     1                  IPARI  ,X  ,V  ,
     2                  NIN    ,ITASK ,COUNT_REMSLV ,INTBUF_TAB )
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
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ITASK, NIN,COUNT_REMSLV(*)
C     REAL
      my_real
     .   X(*), V(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NOINT, I_STOK,CT
C     REAL
      my_real
     .   STARTT, GAP, STOPT
C-----------------------------------------------
C     REAL
C-----------------------------------------------
C	Calcul des adresses des buffers d'interfaces. 
C	Les adresses des buffers J10-JFI et K10-KFI 
C	sont remplaces systematiquement par des tableaux
C	JD(i) et KD(i), en gardant les memes numeros d'indexes.
C	Les anciens adresses directs Jn, Kn sont modifies
C	dans la routine sans commentaires additionnels habituels
C----------------------------------------------------
      NOINT  =IPARI(15)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      GAP   =INTBUF_TAB%VARIABLES(2)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
       I_STOK = INTBUF_TAB%I_STOK(1)
       CALL I10OPTCD(
     1INTBUF_TAB%NSV,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,X	  ,I_STOK	,
     2INTBUF_TAB%IRECTM,GAP      ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IPARI(21)    ,
     3ITASK        ,NIN      ,IPARI(5)     ,INTBUF_TAB%STFNS,INTBUF_TAB%STFM,
     4COUNT_REMSLV )
C-----------------------------------------------------------
C
      RETURN
      END
