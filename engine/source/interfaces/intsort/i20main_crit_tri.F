Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I20MAIN_CRIT_TRI              source/interfaces/intsort/i20main_crit_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I20BUCE_CRIT                  source/interfaces/intsort/i20buce_crit.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I20MAIN_CRIT_TRI(
     1  IPARI  ,X      ,NIN    ,
     2  ITASK  ,V      ,XSLV_L ,XMSR_L ,VSLV_L ,
     3  VMSR_L ,MS     ,DXANCG ,IKINE  ,DIAG_SMS,
     4  INTBUF_TAB,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK,I,
     .        IPARI(NPARI,NINTER), IKINE(NUMNOD)
C     REAL
      my_real 
     .   X(3,*), V(3,*),MS(*),DXANCG(3,*),
     .   XSLV_L(*), XMSR_L(*), VSLV_L(*), VMSR_L(*),
     .   DIAG_SMS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NLN
C     REAL
      my_real
     .   STARTT, STOPT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C---------------------------------------------------- 
C	Calcul des adresses des buffers d'interfaces. 
C----------------------------------------------------
C
      NRTM  =IPARI(4,NIN)
      NSN   =IPARI(5,NIN)
      NMN   =IPARI(6,NIN)

      NLN   =IPARI(35,NIN)
      NLINS  =IPARI(51,NIN)
      NLINM  =IPARI(52,NIN)
      NLINSA =IPARI(53,NIN)
      NLINMA =IPARI(54,NIN)
      NSNE   =IPARI(55,NIN)
      NMNE   =IPARI(56,NIN)

      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
C
C
       IF(INTBUF_TAB%VARIABLES(16).EQ.ZERO)INTBUF_TAB%VARIABLES(16)=EP30
       CALL I20BUCE_CRIT(
     1  ITASK	     ,X 	   ,V		 ,MS	       ,
     2  NTY	     ,NIN	   ,IPARI(22,NIN),NSN	       ,
     3  NMN	     ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%XSAV,
     4  INTBUF_TAB%STFA,XSLV_L	   ,XMSR_L	 ,VSLV_L       ,
     5  VMSR_L       ,INTBUF_TAB%DAANC6,INTBUF_TAB%AVX_ANCR,INTBUF_TAB%AVX_ANCR(1+3*NLN),
     6  NSNE	     ,NMNE	   ,NLINSA	 ,NLINMA       ,
     7  INTBUF_TAB%NSVL,INTBUF_TAB%MSRL,INTBUF_TAB%CRITX,INTBUF_TAB%PENISE,
     8  INTBUF_TAB%PENIME,INTBUF_TAB%STFS,INTBUF_TAB%XA,INTBUF_TAB%VA,
     9  IPARI(35,NIN),INTBUF_TAB%NLG,INTBUF_TAB%PENIS,INTBUF_TAB%PENIM,
     A  INTBUF_TAB%PENIA,NRTM	   ,INTBUF_TAB%IXLINS,DXANCG       ,
     B  IKINE	     ,DIAG_SMS     ,INTBUF_TAB%ALPHAK,INTBUF_TAB%AVX_ANCR(1+6*NLN) ,
     C  INTBUF_TAB%STFAC(1),H3D_DATA)

      RETURN
      END
