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
Chd|  I11MAIN_OPT_TRI               source/interfaces/intsort/i11main_opt_tri.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I11OPTCD                      source/interfaces/intsort/i11optcd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I11MAIN_OPT_TRI(
     1                  IPARI   ,INTBUF_TAB  ,X     ,V      ,
     2                  NIN     ,ITASK,COUNT_REMSLV)
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
      INTEGER NIN ,ITASK
      INTEGER IPARI(NPARI,NINTER), COUNT_REMSLV(NINTER)
C     REAL
      my_real 
     .    X(*), V(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   STARTT, GAP, STOPT, DRAD, DGAPLOADP
      INTEGER :: NRTS
C-----------------------------------------------
C  Calcul des adresses des buffers d'interfaces. 
C  Les adresses des buffers J10-JFI et K10-KFI 
C  sont remplaces systematiquement par des tableaux
C  JD(i) et KD(i), en gardant les memes numeros d'indexes.
C  Les anciens adresses directs Jn, Kn sont modifies
C  dans la routine sans commentaires additionnels habituels
C----------------------------------------------------
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      GAP  = INTBUF_TAB%VARIABLES(2)
      DRAD = INTBUF_TAB%VARIABLES(24)
      DGAPLOADP = INTBUF_TAB%VARIABLES(46)
      NRTS   =IPARI(3,NIN)
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
      CALL I11OPTCD(
     1  INTBUF_TAB%CAND_E, INTBUF_TAB%CAND_N, X  , INTBUF_TAB%I_STOK(1),
     2  INTBUF_TAB%IRECTS, INTBUF_TAB%IRECTM, GAP, NIN,V,INTBUF_TAB%GAP_S,
     3  INTBUF_TAB%GAP_M , IPARI(21,NIN)    , INTBUF_TAB%STFS  , ITASK,NRTS,
     4  INTBUF_TAB%STFM  , INTBUF_TAB%GAP_SL, INTBUF_TAB%GAP_ML, COUNT_REMSLV,  
     5  INTBUF_TAB%IFPEN , INTBUF_TAB%FTSAVX, INTBUF_TAB%FTSAVY, INTBUF_TAB%FTSAVZ,
     6  IPARI(30,NIN)    , DRAD             , DGAPLOADP)
C-----------------------------------------------------------
ccc      ENDIF
C
      RETURN
      END
