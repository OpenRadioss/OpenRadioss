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
Chd|  I9MAIN2                       source/interfaces/int09/i9main2.F
Chd|-- called by -----------
Chd|        INTAL1                        source/ale/intal1.F           
Chd|-- calls ---------------
Chd|        I9AVEL                        source/interfaces/int09/i9avel.F
Chd|        I9FRMS                        source/interfaces/int09/i9frms.F
Chd|        I9IMP2                        source/interfaces/int09/i9imp2.F
Chd|        INVOI2                        source/interfaces/inter2d/invoi2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I9MAIN2(IPARI,INTBUF_TAB        ,X         ,V         ,
     1           A           ,ISKEW   ,SKEW      ,LCOD      ,FN        ,
     2           FT          ,MS      ,MSMN      ,MSMT      ,ITAB      ,
     3           FSAV        )
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
#include      "com01_c.inc"
#include      "vect07_c.inc"
#include      "scr04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ISKEW(*), LCOD(*), ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(*), V(*), A(*), SKEW(*), FN(*), MSMN(*),
     .   FT(*), MSMT(*), MS(*), FSAV(NTHVKI,*)

       TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NG, IGIMP, NGROUS
      my_real
     .   STARTT, FRIC, GAP, STOPT
C-----------------------------------------------
C
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      MST   =IPARI(9)
C
      IF(NMN.EQ.0)RETURN
C
      FRIC =INTBUF_TAB%VARIABLES(1)
      GAP  =INTBUF_TAB%VARIABLES(2)
C-----------------------------------------------------
C     RECHERCHE DES POINTS IMPACTES
C-----------------------------------------------------
      CALL INVOI2(X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .          INTBUF_TAB%NSV,
     1          INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN       )
      CALL I9IMP2(X       ,V         ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,
     1          INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,NSN       ,
     .          INTBUF_TAB%CSTS,
     2          INTBUF_TAB%VARIABLES(1),INTBUF_TAB%N)
C-----------------------------------------------------
C     SOMME DES FORCES ET MASSES
C-----------------------------------------------------
      CALL I9FRMS
     1   (X         ,SKEW      ,A         ,FN        ,FT        ,
     2    INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,
     .    INTBUF_TAB%NSV,
     3    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,MS        ,INTBUF_TAB%N,LCOD      ,
     4    ISKEW     ,FRIC      ,MSMN      ,MSMT      )
C-----------------------------------------------------
C     CALCUL DES ACCEL. (MAITRE) ET VITESSES (ESCLAVE)
C-----------------------------------------------------
      CALL I9AVEL
     1   (X         ,SKEW      ,A         ,FN        ,FT        ,
     2    INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,
     .    INTBUF_TAB%NSV,
     3    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,LCOD      ,V         ,ISKEW     ,
     4    INTBUF_TAB%N,MS        ,INTBUF_TAB%VARIABLES(1),MSMN      ,MSMT      ,
     5    INTBUF_TAB%STFNS,ITAB      ,FSAV      )
C
      RETURN
      END
