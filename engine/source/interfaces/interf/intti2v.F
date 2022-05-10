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
Chd|  INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- called by -----------
Chd|        INTTI2                        source/interfaces/interf/intti2.F
Chd|-- calls ---------------
Chd|        I2CURVV                       source/interfaces/interf/i2curvv.F
Chd|        I2ROT10                       source/interfaces/interf/i2vit10.F
Chd|        I2ROT3                        source/interfaces/interf/i2vit3.F
Chd|        I2VIROT3                      source/interfaces/interf/i2vit3.F
Chd|        I2VIT10                       source/interfaces/interf/i2vit10.F
Chd|        I2VIT27                       source/interfaces/interf/i2vit27.F
Chd|        I2VIT28                       source/interfaces/interf/i2vit28.F
Chd|        I2VIT3                        source/interfaces/interf/i2vit3.F
Chd|        I2VIT3N                       source/interfaces/interf/i2vit3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTTI2V(IPARI,X    ,V   ,
     .                   A    ,VR   ,AR   ,MS  ,IN   ,
     .                   WEIGHT,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   X(*), V(*), A(*),
     .   MS(*),IN(*),AR(*),VR(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN,NMN,NRTS,NRTM,ILEV,IDEL2
C=======================================================================
      NRTS  = IPARI(3)
      NRTM  = IPARI(4)
      NSN   = IPARI(5)
      NMN   = IPARI(6)
      ILEV  = IPARI(20)
      IDEL2 = IPARI(17)
C
      IF (ILEV == 25 .or. ILEV == 26) THEN
        CONTINUE
      ELSEIF (ILEV == 27) THEN
        CALL I2VIT27(INTBUF_TAB,IPARI,WEIGHT,NSN     ,
     .                   X    ,V   ,A    ,VR   ,AR   ,
     .                   MS  ,IN   )
c
      ELSEIF (ILEV == 28) THEN
        CALL I2VIT28(INTBUF_TAB,IPARI,WEIGHT,NSN     ,
     .                   X    ,V   ,A    ,VR   ,AR   ,
     .                   MS  ,IN   )
c            
      ELSEIF (ILEV == 1.AND.IRODDL /= 0) THEN
C
C       FORMULATION SANS MOMENT SUR MASTER
C
        CALL I2VIROT3(NSN       ,NMN       ,
     2 A         ,INTBUF_TAB%IRECTM,
     3 INTBUF_TAB%DPARA,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4 V         ,MS        ,AR        ,VR        ,
     5 X         ,WEIGHT)
      ELSEIF(ILEV == 2)THEN
C
C       FORMULATION AVEC MOMENT SUR MASTER
C
        CALL I2VIT3(NSN       ,NMN       ,
     2 A         ,INTBUF_TAB%IRECTM,
     3 INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4 V         ,MS        ,WEIGHT    ,INTBUF_TAB%NMAS)
C
        IF(IRODDL /= 0)THEN
          CALL I2ROT3(NSN       ,NMN       ,
     2   AR        ,INTBUF_TAB%IRECTM,
     3   INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4   VR        ,IN        ,A         ,V         ,
     5   X         )
        ENDIF
C
      ELSEIF(ILEV == 4)THEN
C
C       FORMULATION decouplant les ddl rot
C
        CALL I2VIT3(NSN       ,NMN       ,
     2 A         ,INTBUF_TAB%IRECTM,
     3 INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4 V         ,MS        ,WEIGHT    ,INTBUF_TAB%NMAS)
C
      ELSEIF (ILEV >= 10 .AND. ILEV < 23) THEN       
C       FORMULATION AVEC RUPTURE
C
         CALL I2VIT10(                                                  
     1       NSN       ,NMN       ,MS        ,V         ,A         ,     
     2       AR        ,VR        ,X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,     
     3       INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRUPT,WEIGHT	 )
         IF(IRODDL /= 0) THEN                                          
           CALL I2ROT10(                                                
     1        NSN       ,NMN       ,MS        ,V         ,A         ,   
     2        AR        ,VR        ,X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,   
     3        INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRUPT,WEIGHT ) 
         ENDIF                                                         
      ELSEIF (ILEV == 30 .AND. IRODDL /= 0) THEN       
C       Formulation avec courbure du master 
         CALL I2CURVV(                                                  
     1       NSN       ,NMN       ,MS        ,V         ,A         ,     
     2       AR        ,VR        ,X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,     
     3       INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,WEIGHT    )   
C
      ELSEIF(ILEV == 0.OR.IRODDL == 0)THEN
C       FORMULATION AVEC MOMENT SUR MASTER
C
        CALL I2VIT3N(NSN       ,NMN       ,
     2 A         ,INTBUF_TAB%IRECTM,
     3 INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4 V         ,MS        ,WEIGHT    ,INTBUF_TAB%NMAS)
C
        IF(IRODDL /= 0)THEN
          CALL I2ROT3(NSN       ,NMN       ,
     2   AR        ,INTBUF_TAB%IRECTM,
     3   INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4   VR        ,IN        ,A         ,V         ,
     5   X         )
        ENDIF
      ENDIF
C-----------
      RETURN
      END
