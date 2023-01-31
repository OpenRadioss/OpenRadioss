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
Chd|  FXBYVIT                       source/constraints/fxbody/fxbyvit.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FXBODVP1                      source/constraints/fxbody/fxbodvp.F
Chd|        FXBODVP2                      source/constraints/fxbody/fxbodvp.F
Chd|        SPMD_FXB_CIN                  source/mpi/kinematic_conditions/spmd_fxb_cin.F
Chd|====================================================================
      SUBROUTINE FXBYVIT(FXBIPM, FXBNOD, FXBMOD  , FXBGLM , FXBLM ,
     .                   FXBMVN, FXBMCD, FXBSE   , FXBSV  , FXBVIT,
     .                   FXBACC, FXBRPM, V       , VR     , A     ,
     .                   AR    , MS    , IN      , WEIGHT , FSAV  ,
     .                   FXBFC , FXBEDP, IAD_ELEM, FR_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "param_c.inc"
#include      "fxbcom.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBNOD(*), WEIGHT(*), IAD_ELEM(2,*),
     .        FR_ELEM(*)
      my_real
     .   FXBMOD(*), FXBGLM(*), FXBLM(*) , FXBMVN(*), FXBMCD(*),  
     .   FXBSE(*) , FXBSV(*) , FXBVIT(*), FXBACC(*), FXBRPM(*), 
     .   A(3,*)   , AR(3,*)  , V(3,*)   , VR(3,*)  , MS(*)    ,
     .   IN(*)    , FSAV(NTHVKI,*)      , FXBFC(*) , FXBEDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX,ANOD,AMOD,ACP,AGLM,ALM,AVAR,ARPM,NME,NMOD,AMCD,
     .        ASE,NSN,IDMAST,M,ISH,DMT,LMOD,ITN,NCYCLP1,N2,NN2,
     .        NSNT,IBLO,IFILE,NSNI,IRCM,PMAIN
      my_real
     .        TT2
C
      N2=NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU
      ITN=0
      IF(TT<TANIM) ITN=1
      NCYCLP1=NCYCLE+1
      IF((NCYCLP1/NCRST)*NCRST==NCYCLP1.OR.MREST/=0) ITN=0
      DO NFX=1,NFXBODY
         NN2=N2+NFX
         NMOD=FXBIPM(4,NFX)
         AGLM=FXBIPM(8,NFX)
         ACP =FXBIPM(9,NFX)
         ALM =FXBIPM(10,NFX)
         AVAR=FXBIPM(13,NFX)
         ARPM=FXBIPM(14,NFX)
         AMCD=FXBIPM(15,NFX)
         ASE =(NFX-1)*15+1
         ISH =FXBIPM(16,NFX)
         NME =FXBIPM(17,NFX)
         IF (ISH==0) THEN
            DMT=18
         ELSE
            DMT=24
         ENDIF
         IBLO=FXBIPM(28,NFX)
         PMAIN=FXBIPM(39,NFX)
         IF (ISPMD==PMAIN)
     .     CALL FXBODVP1(
     .FXBRPM(ARPM),FXBGLM(AGLM),FXBLM(ALM)  ,FXBMVN(ACP) ,FXBMCD(AMCD),
     .FXBSE(ASE)  ,FXBSV(ALM)  ,FXBVIT(AVAR),FXBACC(AVAR),NME         ,
     .NMOD        ,ISH         ,DMT         ,FSAV(1,NN2) ,FXBFC(ALM)  ,
     .FXBEDP(NFX) ,IBLO        )
      ENDDO
C
      IF(NSPMD > 1)  CALL SPMD_FXB_CIN(FXBIPM, FXBVIT, FXBACC)
C
      DO NFX=1,NFXBODY
         IDMAST=FXBIPM(2,NFX)
         IF (ITN>0) THEN
            NSN=FXBIPM(18,NFX)
         ELSE
            NSN=FXBIPM(3,NFX)
         ENDIF
         NSNT=FXBIPM(3,NFX)
         NMOD=FXBIPM(4,NFX)
         ANOD=FXBIPM(6,NFX)
         AMOD=FXBIPM(7,NFX)
         AVAR=FXBIPM(13,NFX)
         ARPM=FXBIPM(14,NFX)
         ISH =FXBIPM(16,NFX)
         NME =FXBIPM(17,NFX)
         IFILE=FXBIPM(29,NFX)
         NSNI=FXBIPM(18,NFX)
         IF (IFILE==0) THEN
            LMOD=NSNT*6
         ELSEIF (IFILE==1) THEN
            LMOD=NSNI*6
         ENDIF
         IRCM=FXBIPM(30,NFX)
         PMAIN=FXBIPM(39,NFX)
         CALL FXBODVP2(
     .FXBRPM(ARPM),FXBNOD(ANOD),FXBMOD(AMOD),FXBVIT(AVAR),FXBACC(AVAR),
     .NME         ,NMOD        ,V           ,VR          ,A           ,
     .AR          ,MS          ,IN          ,NSN         ,IDMAST      ,
     .ISH         ,LMOD        ,NSNT        ,IFILE       ,NSNI        ,
     .IRCM        ,PMAIN     ,IAD_ELEM    ,FR_ELEM     )
      ENDDO
C      
      RETURN
      END
