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
Chd|  INTAL2                        source/ale/inter/intal2.F     
Chd|-- called by -----------
Chd|        INTAL1                        source/ale/inter/intal1.F     
Chd|-- calls ---------------
Chd|        IQEL02                        source/ale/ale2d/iqel02.F     
Chd|        IQEL03                        source/ale/ale3d/iqel03.F     
Chd|        IQELA1                        source/ale/inter/iqela1.F     
Chd|        IQELA2                        source/ale/inter/iqela2.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTAL2(IPARI,INTBUF_TAB         , X        ,V         ,
     1           A           ,ISKEW   ,SKEW      ,LCOD      ,E         ,
     2           MSM         ,MS      ,ITAB      ,FSAV      ,FCONT     ,
     3           FNCONT      ,FTCONT  ,H3D_DATA  )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: IPARI(SIPARI), ISKEW(*), LCOD(*), ITAB(NUMNOD)
      my_real,INTENT(INOUT) :: X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD), SKEW(*), E(*), MSM(*), MS(*),FSAV(*)
      my_real,INTENT(INOUT) :: FCONT(3,*),FNCONT(3,*),FTCONT(3,*)       
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      MST   =IPARI(9)

      IF(N2D == 0)THEN
       CALL IQEL03
     1  (X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%NRT ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV ,
     2   INTBUF_TAB%ILOCS ,INTBUF_TAB%IRTLM ,NSN       ,INTBUF_TAB%NSEGM,INTBUF_TAB%CSTS ,
     3   INTBUF_TAB%N )
      ELSE
       CALL IQEL02
     1  (X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%NRT ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV ,
     2   INTBUF_TAB%ILOCS ,INTBUF_TAB%IRTLM ,NSN       ,INTBUF_TAB%NSEGM,INTBUF_TAB%CSTS ,
     3   INTBUF_TAB%N )
      ENDIF

      CALL IQELA1
     1   (X         ,SKEW      ,A         ,E         ,MSM       ,
     2    INTBUF_TAB%IRECTM,INTBUF_TAB%NRT,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     3    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,MS        ,INTBUF_TAB%N,LCOD      ,
     4    ISKEW           ,FSAV,ITAB ,FCONT,FNCONT,FTCONT,H3D_DATA)

      CALL IQELA2
     1   (X         ,SKEW      ,A         ,E         ,MSM       ,
     2    INTBUF_TAB%IRECTM,INTBUF_TAB%NRT,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     3    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,LCOD      ,V         ,ISKEW     ,
     4    INTBUF_TAB%N,ITAB      )

      RETURN
      END
