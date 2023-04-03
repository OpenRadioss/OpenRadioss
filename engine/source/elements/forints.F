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
Chd|  FORINTS                       source/elements/forints.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE FORINTS(
     1    PM        ,GEO       ,X         ,A         ,AR ,
     2    V         ,VR        ,MS        ,IN        ,W         ,
     3    ELBUF                ,VAL2      ,VEUL      ,FV        ,
     4    STIFN     ,STIFR     ,FSKY      ,TF        ,BUFMAT    ,
     5    PARTSAV              ,FANI      ,FSAV      ,
     6    SKEW      ,DT2T      ,
     7    IADS      ,IPARG     ,NPC       ,IXS       ,
     8    NELTST    ,ITYPTST   ,IPART     ,IPARTS    ,ITAB      ,
     9    FSKYI     ,BUFGEO    ,KXX       ,IXX       ,ISKY      ,
     A    IPARTX    ,GRESAV    ,GRTH      ,
     B    IGRTH     ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPART(LIPART1,*) ,IPARTS(*) ,
     .   NPC(*), IPARG(NPARG,*),IADS(8,*),NELTST,ITYPTST,
     .   KXX(NIXX,*),I XX(*), ITAB(*),
     .   IPARTX(*), ISKY(*),GRTH(*),IGRTH(*)
C     REAL
      my_real
     .   X(3,*)    ,V(3,*)   ,VR(3,*),
     .   MS(*)   ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),GEO(NPROPG,*),
     .   BUFMAT(*) ,W(3,*)    ,VEUL(*),TF(*)  ,
     .   FSAV(NTHVKI,*)       ,ELBUF(*) ,
     .   FV(*)     ,A(3,*)    ,AR(3,*)     ,VAL2(*)  ,
     .   FANI(3,*) ,PARTSAV(*),STIFN(*) ,STIFR(*),
     .   FSKY(*) ,
     .   DT2T,FSKYI(LSKYI,NFSKYI) ,BUFGEO(*),GRESAV(*)
C
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N, NG, NVC, MLW, JFT, JLT ,K ,
     .   K1, K2, KAD,IAD2,NF1,IPRI,NGLOC, NELEM, OFFSET,
     .   NSG, NEL,ISOLNOD
      INTEGER IADUIX,IADUX ,IADUV ,IADUVR,IADUMS,
     .        IADUIN,IADUSM,IADUSR,IADUMV,IADURV,
     .        IADUF ,IADUTQ
C-----------------------------------------------
C
      DO 240 NG=1,NGROUP
          IF(IPARG(8,NG)==1.OR.IPARG(5,NG)/=100)GOTO 240
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
C---
C
            OFFSET = NELEM - 1
C
           CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
C
            NSG   =IPARG(10,NG)
            NVC   =IPARG(19,NG)
            JHBE  =IPARG(23,NG)
            IGRE  =IPARG(51,NG)
C
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
C           Multi-purpose element Groups are constituted,
C           but Multi-purpose element is not supposed to be vectorized ..
             CALL XFORC3(
     1   NFT,          NEL,          GEO,          PM,
     2   ITAB,         KXX,          IXX,          X,
     3   V,            VR,           A,            AR,
     4   ELBUF(IAD),   NPC,          TF,           SKEW,
     5   DT2T,         NELTST,       ITYPTST,      STIFN,
     6   STIFR,        MS,           IN,           FSKYI,
     7   ISKY,         PARTSAV,      IPARTX,       BUFMAT,
     8   BUFGEO,       GRESAV,       GRTH,         IGRTH,
     9   ELBUF_TAB(NG),IGRE)
            IF (IDDW>0) CALL STOPTIMEG(NG)
         END DO
 240  CONTINUE
C----------------------------------
      RETURN
      END
