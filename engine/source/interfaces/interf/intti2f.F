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
!||    intti2f         ../engine/source/interfaces/interf/intti2f.F
!||--- called by ------------------------------------------------------
!||    intti1          ../engine/source/interfaces/interf/intti1.F
!||--- calls      -----------------------------------------------------
!||    i2curvf         ../engine/source/interfaces/interf/i2curvf.F
!||    i2curvfp        ../engine/source/interfaces/interf/i2curvfp.F
!||    i2fomo3         ../engine/source/interfaces/interf/i2for3.F
!||    i2fomo3p        ../engine/source/interfaces/interf/i2for3p.F
!||    i2for10         ../engine/source/interfaces/interf/i2for10.F
!||    i2for10p        ../engine/source/interfaces/interf/i2for10p.F
!||    i2for25         ../engine/source/interfaces/interf/i2for25.F
!||    i2for25p        ../engine/source/interfaces/interf/i2for25p.F
!||    i2for26         ../engine/source/interfaces/interf/i2for26.F
!||    i2for26p        ../engine/source/interfaces/interf/i2for26p.F
!||    i2for27         ../engine/source/interfaces/interf/i2for27.F
!||    i2for27p        ../engine/source/interfaces/interf/i2for27p.F
!||    i2for28         ../engine/source/interfaces/interf/i2for28.F
!||    i2for28p        ../engine/source/interfaces/interf/i2for28p.F
!||    i2for3          ../engine/source/interfaces/interf/i2for3.F
!||    i2for3n         ../engine/source/interfaces/interf/i2for3.F
!||    i2for3o         ../engine/source/interfaces/interf/i2for3.F
!||    i2for3p         ../engine/source/interfaces/interf/i2for3p.F
!||    i2for3pn        ../engine/source/interfaces/interf/i2for3p.F
!||    i2for3po        ../engine/source/interfaces/interf/i2for3p.F
!||    i2mom10         ../engine/source/interfaces/interf/i2for10.F
!||    i2mom10p        ../engine/source/interfaces/interf/i2for10p.F
!||    i2mom3          ../engine/source/interfaces/interf/i2for3.F
!||    i2mom3n         ../engine/source/interfaces/interf/i2for3.F
!||    i2mom3p         ../engine/source/interfaces/interf/i2for3p.F
!||    i2mom3pn        ../engine/source/interfaces/interf/i2for3p.F
!||    i2mzerop        ../engine/source/interfaces/interf/i2for3p.F
!||    i2therm         ../engine/source/interfaces/interf/i2therm.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod         ../engine/share/modules/h3d_mod.F
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE INTTI2F(
     1   IPARI  ,X      ,V      ,A      ,
     2   VR     ,AR     ,MS     ,IN     ,WEIGHT ,STIFN  ,
     3   STIFR  ,FSKYI2 ,IADI2  ,I2MSCH ,DMAST  ,ADM    ,
     4   I0     ,NIR    ,I2SIZE ,ADI    ,IGEO   ,BUFGEO ,
     5   FSAV   ,FNCONT ,NODNX_SMS,DMINT2 ,SAV_FOR_PENA ,
     6   MS_PENA,DT2T   ,NELTST ,ITYPTST,INTBUF_TAB,TEMP,
     7   FTHE   ,FTHESKYI2,CONDN,CONDNSKYI2,ITAB,
     8   SAV_INER_POFF  ,H3D_DATA,T2FAC_SMS,FNCONTP ,
     A   FTCONTP,IDT_THERM    ,THEACCFACT)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), IADI2(*),WEIGHT(*),IGEO(NPROPGI,*),
     .        NODNX_SMS(*),ITAB(*)
      INTEGER I2MSCH, I0, NIR, I2SIZE,NELTST,ITYPTST
      INTEGER ,intent(in) :: IDT_THERM
      my_real ,intent(in) :: theaccfact
      my_real
     .   X(*), V(*), A(*),DMAST,ADM(*), 
     .   MS(*),IN(*),AR(*),VR(*),STIFN(*),STIFR(*),FSKYI2(*),
     .   ADI(*), BUFGEO(*), FSAV(*),FNCONT(3,*),
     .   DMINT2(*),SAV_FOR_PENA(*),MS_PENA(*),DT2T,TEMP(*),
     .   FTHE(*),FTHESKYI2(*),CONDN(*),CONDNSKYI2(*),
     .   SAV_INER_POFF(*),T2FAC_SMS(*),
     .   FNCONTP(3,*),FTCONTP(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   NSN,NMN,NRTS,NRTM,ILEV,I0BASE,IDEL2,PENFLAG,IROT,NOINT,INTTH,IVISC
      my_real
     .   KTHE
C=======================================================================
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NOINT =IPARI(15)
      IDEL2  = IPARI(17)
      ILEV  =IPARI(20)
      IVISC  = IPARI(72)
C
          I0BASE = I0
C
      IF (IPARIT == 0) THEN
       IF((ILEV == 1.OR.ILEV == 3).AND.IRODDL /= 0)THEN
C       FORMULATION SANS MOMENT SUR MAIN
        CALL I2FOMO3(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%DPARA,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,MS        ,WEIGHT    ,
     3   AR        ,IN        ,X         ,STIFN     ,STIFR     ,
     4   ILEV      ,DMAST     ,ADM       ,INTBUF_TAB%NMAS,IDEL2     ,
     5   INTBUF_TAB%SMAS,INTBUF_TAB%SINER,V         ,INTBUF_TAB%CSTS,FSAV      ,
     6   FNCONT    ,H3D_DATA  ,FNCONTP   ,FTCONTP   )

       ELSEIF(ILEV == 2)THEN
C       FORMULATION AVEC MOMENT SUR MAIN
        CALL I2FOR3(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,MS        ,WEIGHT    ,
     3   STIFN     ,INTBUF_TAB%NMAS,X         ,V         ,FSAV      ,
     4   FNCONT    ,H3D_DATA  ,INTBUF_TAB%CSTS_BIS ,FNCONTP ,FTCONTP   )
C
        IF(IRODDL /= 0)THEN
          CALL I2MOM3(
     1   NSN       ,NMN       ,AR        ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,IN        ,MS        ,
     3   A         ,X         ,WEIGHT    ,STIFR     ,STIFN        ,
     4   SAV_INER_POFF,INTBUF_TAB%CSTS_BIS)
        ENDIF
       ELSEIF (ILEV == 4)THEN
C       FORMULATION decouplant les ddl rot
        CALL I2FOR3O(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,MS      ,WEIGHT    ,
     3   STIFN     ,INTBUF_TAB%NMAS,X         ,V         ,FSAV      ,
     5   FNCONT    ,INTBUF_TAB%CSTS_BIS,H3D_DATA ,FNCONTP,FTCONTP)
       ELSEIF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12 .OR. 
     .         ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN       
C        FORMULATION AVEC RUPTURE
         CALL I2FOR10(                                               
     1       A         ,MS        ,STIFN     ,WEIGHT    ,INTBUF_TAB%IRECTM,
     2       INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%IRUPT,INTBUF_TAB%CSTS,
     3       INTBUF_TAB%FSM,NSN,INTBUF_TAB%CSTS_BIS) 
C
         IF(IRODDL /= 0) THEN                           
           CALL I2MOM10(                                                
     1         NSN       ,NMN       ,AR      ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,  
     2         INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,IN     ,MS        ,  
     3         A         ,X         ,WEIGHT    ,STIFR     ,STIFN     ,  
     4         INTBUF_TAB%IRUPT,ILEV,INTBUF_TAB%CSTS_BIS)
         ENDIF                                                          
C
       ELSEIF (ILEV == 25) THEN       
c        formulation en penalite avec rotations
         PENFLAG = IPARI(43)
         IROT    = IPARI(48)         
C
         CALL I2FOR25(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,STIFN     ,STIFR     ,WEIGHT    ,INTBUF_TAB%IRECTM,
     .       INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%SKEW,INTBUF_TAB%DSM,
     .       INTBUF_TAB%FSM,INTBUF_TAB%FINI,FSAV      ,FNCONT    ,NSN       ,
     .       INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,INTBUF_TAB%VARIABLES(14),PENFLAG,IROT ,
     .       NOINT     ,NODNX_SMS ,DMINT2 ,  SAV_FOR_PENA ,
     .       MS_PENA   ,DT2T      ,NELTST ,ITYPTST ,
     .       IVISC     ,H3D_DATA  ,FNCONTP   ,FTCONTP   )
       ELSEIF (ILEV == 26) THEN       
c        formulation en penalite + rot

         PENFLAG = IPARI(43)
         IROT    = IPARI(48)           
C
         CALL I2FOR26(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,STIFN     ,STIFR     ,WEIGHT    ,INTBUF_TAB%IRECTM,
     .       INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%DSM,INTBUF_TAB%FSM,INTBUF_TAB%FINI,
     .       FSAV      ,FNCONT    ,NSN       ,INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,
     .       INTBUF_TAB%VARIABLES(14),NOINT  ,NODNX_SMS  ,DMINT2  ,SAV_FOR_PENA,  
     .       MS_PENA   ,IN        ,DT2T      ,NELTST     ,ITYPTST ,
     .       SAV_INER_POFF,H3D_DATA ,FNCONTP   ,FTCONTP   )
       ELSEIF (ILEV == 27) THEN       
c        formulation en penalite avec rotations
         PENFLAG = IPARI(43)
         IROT    = IPARI(48)         
c
         CALL I2FOR27(                                               
     .   X         ,V         ,VR        ,A         ,AR        ,                               
     .   MS        ,IN        ,STIFN     ,STIFR     ,WEIGHT    ,                               
     .   INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%DSM,INTBUF_TAB%FSM,      
     .   INTBUF_TAB%FINI,INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,FSAV      ,FNCONT    ,    
     .   INTBUF_TAB%IRUPT,INTBUF_TAB%VARIABLES(14),INTBUF_TAB%CSTS,INTBUF_TAB%MSR,ADI    ,     
     .   INTBUF_TAB%SMAS,INTBUF_TAB%SINER,INTBUF_TAB%NMAS(1+NMN),SAV_FOR_PENA,MS_PENA   ,                                                              
     .   NOINT  ,NODNX_SMS  ,DMINT2  ,DT2T      ,NELTST      ,                                                      
     .   ITYPTST  ,NSN       ,NMN       ,IDEL2  , PENFLAG   ,IROT   ,INTBUF_TAB%SKEW,
     .   SAV_INER_POFF, H3D_DATA ,INTBUF_TAB%DPARA,
     .   INTBUF_TAB%MSEGTYP2,INTBUF_TAB%CSTS_BIS,T2FAC_SMS ,FNCONTP   ,FTCONTP   )                                                                                     
C
       ELSEIF (ILEV == 28) THEN       
c        formulation en penalite avec rotations
         PENFLAG = IPARI(43)
         IROT    = IPARI(48)         
c
         CALL I2FOR28(                                               
     .   X         ,V         ,VR        ,A         ,AR        ,                               
     .   MS        ,IN        ,STIFN     ,STIFR     ,WEIGHT    ,                               
     .   INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%DSM,INTBUF_TAB%FSM,      
     .   INTBUF_TAB%FINI,INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,FSAV      ,FNCONT    ,    
     .   INTBUF_TAB%IRUPT,INTBUF_TAB%VARIABLES(14),INTBUF_TAB%CSTS,INTBUF_TAB%MSR,ADI    ,     
     .   INTBUF_TAB%SMAS,INTBUF_TAB%SINER,INTBUF_TAB%NMAS(1+NMN),SAV_FOR_PENA,MS_PENA   ,                                                              
     .   NOINT     ,NODNX_SMS  ,DMINT2   ,DT2T      ,NELTST    ,                                                      
     .   ITYPTST   ,NSN        ,NMN      ,IDEL2     ,PENFLAG   ,
     .   IROT,INTBUF_TAB%SKEW,
     .   INTBUF_TAB%DPARA,INTBUF_TAB%NMAS,DMAST,ADM,SAV_INER_POFF,
     .   H3D_DATA ,FNCONTP   ,FTCONTP   )                                                                                     
C
       ELSEIF (ILEV == 30 .AND. IRODDL /= 0) THEN  
c        formulation avec la courbure du main                   
         CALL I2CURVF(
     .        MS        ,IN        ,X         ,V         ,VR        ,       
     .        A         ,AR        ,STIFN     ,STIFR     ,WEIGHT    ,       
     .        INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,   
     .        INTBUF_TAB%NMAS,INTBUF_TAB%SMAS,INTBUF_TAB%SINER,FSAV      ,NSN ,
     .        NMN       ,IDEL2     ,FNCONT    ,H3D_DATA  ,FNCONTP   ,
     .        FTCONTP)
C  
       ELSEIF(ILEV == 0 .OR. IRODDL == 0)THEN
C       FORMULATION AVEC MOMENT SUR MAIN
        CALL I2FOR3N(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,MS   ,WEIGHT    ,
     3   STIFN     ,INTBUF_TAB%NMAS,IDEL2     ,INTBUF_TAB%SMAS,X         ,
     4   V         ,FSAV      ,FNCONT    ,H3D_DATA ,INTBUF_TAB%CSTS_BIS ,
     5   FNCONTP   ,FTCONTP   )
         IF(IRODDL /= 0)THEN
           CALL I2MOM3N(
     1     NSN       ,NMN       ,AR        ,INTBUF_TAB%IRECTM, INTBUF_TAB%CSTS,
     2     INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,IN ,MS   ,
     3     A         ,X         ,WEIGHT    ,STIFR     ,STIFN     ,
     4     IDEL2     ,INTBUF_TAB%SMAS,INTBUF_TAB%SINER,INTBUF_TAB%NMAS(1+NMN),ADI,
     5     SAV_INER_POFF,H3D_DATA,INTBUF_TAB%CSTS_BIS)
         ENDIF
C
       ENDIF 
C-----------  Fin Parith OFF
      ELSE
C
C parith/on spmd
C attention aux routines duales du parith/on spmd
C
       IF((ILEV == 1.OR.ILEV == 3).AND.IRODDL /= 0)THEN
C       FORMULATION SANS MOMENT SUR MAIN
        CALL I2FOMO3P(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%DPARA,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,MS        ,WEIGHT    ,
     3   AR        ,IN        ,X         ,STIFN     ,STIFR     ,
     4   FSKYI2    ,IADI2     ,ILEV      ,DMAST     ,ADM       ,
     5   INTBUF_TAB%NMAS,I0   ,NIR       ,I2SIZE    ,IDEL2     ,
     6   INTBUF_TAB%SMAS,INTBUF_TAB%SINER,V         ,INTBUF_TAB%CSTS,FSAV   ,
     7   FNCONT    ,H3D_DATA   ,FNCONTP   ,FTCONTP  )
       ELSEIF(ILEV == 2)THEN
C       FORMULATION AVEC MOMENT SUR MAIN
        I0BASE = I0
        CALL I2FOR3P(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,
     2   INTBUF_TAB%NSV,MS    ,WEIGHT    ,STIFN     ,INTBUF_TAB%NMAS,
     3   FSKYI2    ,IADI2     ,I0        ,NIR       ,I2SIZE    ,
     4   INTBUF_TAB%IRECTM,X   ,V        ,FSAV      ,FNCONT    ,
     5   INTBUF_TAB%IRTLM ,H3D_DATA,INTBUF_TAB%CSTS_BIS ,FNCONTP   ,FTCONTP   )
C
        IF(IRODDL /= 0)THEN
          I0 = I0BASE
          CALL I2MOM3P(
     1   NSN       ,NMN       ,AR        ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2   INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,IN        ,MS        ,
     3   A         ,X         ,WEIGHT    ,STIFR     ,FSKYI2    ,
     4   IADI2     ,I0        ,NIR       ,I2SIZE    ,STIFN     ,
     5   INTBUF_TAB%CSTS_BIS)
        ENDIF
       ELSEIF(ILEV == 4)THEN
C       FORMULATION decouplant les ddl rot
        I0BASE = I0
        CALL I2FOR3PO(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,
     2   INTBUF_TAB%NSV,MS    ,WEIGHT    ,STIFN     ,INTBUF_TAB%NMAS,
     3   FSKYI2    ,IADI2     ,I0        ,NIR       ,I2SIZE    ,
     4   INTBUF_TAB%IRECTM,X  ,V         ,FSAV      ,FNCONT    ,
     5   INTBUF_TAB%IRTLM     ,H3D_DATA  ,INTBUF_TAB%CSTS_BIS  ,
     6   FNCONTP   ,FTCONTP   )
        IF(IRODDL /= 0) THEN
          I0 = I0BASE
          CALL I2MZEROP(FSKYI2,I0        ,NIR   ,I2SIZE,IADI2,
     2                  NSN   ,INTBUF_TAB%NSV,WEIGHT)
        ENDIF
       ELSEIF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12 .OR. 
     .         ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN       
C        FORMULATION AVEC RUPTURE
         I0BASE = I0
         CALL I2FOR10P(
     1       A         ,MS        ,STIFN     ,WEIGHT    ,IADI2     ,
     2       FSKYI2    ,INTBUF_TAB%NSV,INTBUF_TAB%IRUPT,INTBUF_TAB%CSTS,INTBUF_TAB%FSM,
     3       NIR       ,NSN       ,I0        ,I2SIZE,INTBUF_TAB%CSTS_BIS)
C
        IF(IRODDL /= 0)THEN
          I0 = I0BASE
          CALL I2MOM10P(
     1         X         ,A         ,AR        ,MS        ,IN        ,
     2         STIFN     ,STIFR     ,WEIGHT    ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     3         INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%IRUPT,INTBUF_TAB%CSTS,IADI2  ,
     4         FSKYI2    ,NSN       ,NMN       ,NIR       ,I0        ,
     5         I2SIZE    ,IDEL2     ,INTBUF_TAB%CSTS_BIS)
        ENDIF
C
       ELSEIF (ILEV == 25) THEN       
         PENFLAG = IPARI(43)
         IROT    = IPARI(48)
c        formulation en penalite         
C
         CALL I2FOR25P(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,STIFN     ,STIFR     ,WEIGHT    ,INTBUF_TAB%IRECTM,
     .       INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%SKEW,INTBUF_TAB%DSM,
     .       INTBUF_TAB%FSM,INTBUF_TAB%FINI,FSAV      ,FNCONT    ,NSN       ,
     .       I0        ,I2SIZE    ,IADI2     ,FSKYI2    ,INTBUF_TAB%SPENALTY, 
     .       INTBUF_TAB%STFR_PENALTY,INTBUF_TAB%VARIABLES(14),PENFLAG,IROT ,NOINT ,
     .       NODNX_SMS ,DMINT2    ,DT2T      ,NELTST    ,ITYPTST ,
     .       IVISC     ,H3D_DATA  ,FNCONTP   ,FTCONTP   ) 
C
       ELSEIF (ILEV == 26) THEN       
c        formulation en penalite avec rotations         
         CALL I2FOR26P(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,STIFN     ,STIFR     ,WEIGHT    ,INTBUF_TAB%IRECTM,
     .       INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%DSM,INTBUF_TAB%FSM,INTBUF_TAB%FINI,
     .       FSAV      ,FNCONT    ,NSN       ,I0        ,I2SIZE    ,
     .       IADI2     ,FSKYI2    ,INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,INTBUF_TAB%VARIABLES(14),
     .       NOINT     ,NODNX_SMS ,DMINT2    ,IN        ,DT2T      ,NELTST    ,ITYPTST , H3D_DATA,
     .       FNCONTP   ,FTCONTP   )
C
       ELSEIF (ILEV == 27) THEN       
c        formulation en cinematique - penalite     
         PENFLAG = IPARI(43)
         IROT    = IPARI(48)

         CALL I2FOR27P(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,IN        ,STIFN     ,STIFR     ,WEIGHT    ,
     .       INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%SKEW,
     .       INTBUF_TAB%DSM,INTBUF_TAB%FSM,INTBUF_TAB%FINI,FSAV      ,FNCONT    ,
     .       NSN       ,I0        ,I2SIZE    ,IADI2     ,FSKYI2    , 
     .       INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,INTBUF_TAB%VARIABLES(14),PENFLAG,IROT ,
     .       NOINT ,NODNX_SMS ,DMINT2    ,DT2T      ,NELTST    ,
     .       ITYPTST   ,INTBUF_TAB%NMAS,INTBUF_TAB%SMAS,INTBUF_TAB%SINER,INTBUF_TAB%NMAS(1+NMN),
     .       INTBUF_TAB%MSR,INTBUF_TAB%IRUPT,ADI    ,IDEL2,NIR ,
     .       NMN, H3D_DATA ,INTBUF_TAB%DPARA,
     .       INTBUF_TAB%MSEGTYP2  ,INTBUF_TAB%CSTS_BIS,T2FAC_SMS,FNCONTP   ,FTCONTP   ) 
c
       ELSEIF (ILEV == 28) THEN       
c        formulation en cinematique - penalite     
         PENFLAG = IPARI(43)
         IROT    = IPARI(48)
         CALL I2FOR28P(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,IN        ,STIFN     ,STIFR     ,WEIGHT    ,
     .       INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%SKEW,
     .       INTBUF_TAB%DSM,INTBUF_TAB%FSM,INTBUF_TAB%FINI,FSAV      ,FNCONT    ,
     .       NSN       ,I0        ,I2SIZE    ,IADI2     ,FSKYI2    , 
     .       INTBUF_TAB%SPENALTY,INTBUF_TAB%STFR_PENALTY,INTBUF_TAB%VARIABLES(14),PENFLAG,IROT ,
     .       NOINT ,NODNX_SMS ,DMINT2    ,DT2T      ,NELTST    ,
     .       ITYPTST   ,INTBUF_TAB%NMAS,INTBUF_TAB%SMAS,INTBUF_TAB%SINER,INTBUF_TAB%NMAS(1+NMN),
     .       INTBUF_TAB%MSR,INTBUF_TAB%IRUPT,ADI    ,IDEL2,NIR       ,
     .       NMN   ,INTBUF_TAB%DPARA,
     .       INTBUF_TAB%NMAS,DMAST,ADM,H3D_DATA,FNCONTP   ,FTCONTP   ) 
c
       ELSEIF (ILEV == 30 .AND. IRODDL /= 0) THEN  
c        formulation avec la courbure du main  
         CALL I2CURVFP(
     .        MS        ,IN        ,X         ,V         ,VR        ,   
     .        A         ,AR        ,STIFN     ,STIFR     ,WEIGHT    ,   
     .        INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,   
     .        NSN       ,NMN       ,IDEL2     ,I0        ,I2SIZE    ,   
     .        IADI2     ,INTBUF_TAB%NMAS,INTBUF_TAB%SMAS,INTBUF_TAB%SINER,FSKYI2,
     .        FSAV      ,FNCONT    ,H3D_DATA  ,FNCONTP   ,FTCONTP)
C
       ELSEIF(ILEV == 0.OR.IRODDL == 0)THEN
C       FORMULATION AVEC MOMENT SUR MAIN
          I0BASE = I0
         CALL I2FOR3PN(
     1   NSN       ,NMN       ,A         ,INTBUF_TAB%CSTS,INTBUF_TAB%NSV,
     2   MS        ,WEIGHT    ,STIFN     ,INTBUF_TAB%NMAS,FSKYI2    ,
     3   IADI2     ,I0        ,NIR       ,I2SIZE    ,IDEL2     ,   
     4   INTBUF_TAB%SMAS,INTBUF_TAB%IRECTM,X        ,V         ,FSAV      ,
     5   FNCONT    ,INTBUF_TAB%IRTLM     ,H3D_DATA  ,INTBUF_TAB%CSTS_BIS  ,
     6   FNCONTP   ,FTCONTP   )
C
         IF(IRODDL /= 0)THEN
           I0 = I0BASE
           CALL I2MOM3PN(
     1     NSN       ,NMN       ,AR        ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2     INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,IN  ,MS       ,
     3     A         ,X         ,WEIGHT    ,STIFR     ,FSKYI2    ,
     4     STIFN     ,IADI2     ,I0        ,NIR       ,I2SIZE    ,
     5     IDEL2     ,INTBUF_TAB%SMAS,INTBUF_TAB%SINER,INTBUF_TAB%NMAS(1+NMN),ADI  ,
     6     H3D_DATA  ,INTBUF_TAB%CSTS_BIS )
         ENDIF
C
C---                      
       ENDIF
      ENDIF

C---------Thermal modelling for  interface Type 2--------
C
       INTTH = IPARI(47)
       KTHE  = INTBUF_TAB%VARIABLES(15)
       IF( INTTH > 0 ) THEN 
         I0 = I0BASE
         CALL I2THERM ( 
     1       X       ,NSN     ,INTBUF_TAB%NSV     ,INTBUF_TAB%IRTLM    ,MS      ,
     2       WEIGHT  ,INTBUF_TAB%IRECTM   ,INTBUF_TAB%CSTS    ,IADI2   ,KTHE    ,
     3       TEMP    ,INTBUF_TAB%AREAS2   ,FTHE              ,FTHESKYI2,CONDN   ,
     4       CONDNSKYI2,I0                ,ITAB     ,IDT_THERM    ,THEACCFACT)
       ENDIF
C
      RETURN
      END
