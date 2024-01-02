Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I24MAIN_OPT_TRI               source/interfaces/intsort/i24main_opt_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I24OPTCD                      source/interfaces/intsort/i24optcd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I24MAIN_OPT_TRI(
     1                  IPARI   ,INTBUF_TAB,X    ,V      ,
     2                  NIN     ,ITASK ,COUNT_REMSLV,T2MAIN_SMS,LSKYI_SMS_NEW)
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
      INTEGER IPARI(NPARI,NINTER),ITASK, NIN,
     .        COUNT_REMSLV(*),NSNR,T2MAIN_SMS(6,*),LSKYI_SMS_NEW
C     REAL
      my_real 
     .   X(*), V(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, I, IFQ, I_STOK,CT,
     .        IEDGE,IGSTI,NSNEL,NRTSE,IEDG4,IFIT,NFIT
C     REAL
      my_real
     .   STARTT,GAP,STOPT
C=======================================================================
C     ALLOCATABLE
C=======================================================================
C-----------------------------------------------

      IFQ =IPARI(31,NIN) 
      NSNR=IPARI(24,NIN)
      IEDGE=IPARI(58,NIN)
      IEDG4=IPARI(59,NIN)
      IGSTI=IPARI(34,NIN)
      NSNEL = IPARI(55,NIN)
      NRTSE = IPARI(52,NIN)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT.OR.TT>STOPT) RETURN
C
      GAP =INTBUF_TAB%VARIABLES(2)
C
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
       I_STOK = INTBUF_TAB%I_STOK(1)
       CALL I24OPTCD(
     1INTBUF_TAB%NSV,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,X,I_STOK      ,
     2INTBUF_TAB%IRECTM,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,V,IPARI(39,NIN),
     3INTBUF_TAB%STFNS,ITASK     ,INTBUF_TAB%STFM,NIN      ,IPARI(5,NIN) ,
     4INTBUF_TAB%IRTLM,INTBUF_TAB%TIME_S,INTBUF_TAB%MSEGLO,COUNT_REMSLV,
     5INTBUF_TAB%SECND_FR,NSNR       ,INTBUF_TAB%PENE_OLD,INTBUF_TAB%STIF_OLD,
     6INTBUF_TAB%VARIABLES(23), INTBUF_TAB%EDGE8L2,IEDGE    ,IGSTI, INTBUF_TAB%MVOISIN ,
     7INTBUF_TAB%ICONT_I , INTBUF_TAB%IS2SE  , INTBUF_TAB%IRTSE,
     8NSNEL,NRTSE , INTBUF_TAB%IS2PT,INTBUF_TAB%ISPT2,INTBUF_TAB%ISEGPT,IEDG4,T2MAIN_SMS,
     9LSKYI_SMS_NEW,INTBUF_TAB%VARIABLES(46))
C
      RETURN
      END
