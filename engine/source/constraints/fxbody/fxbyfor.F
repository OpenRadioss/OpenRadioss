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
Chd|  FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FXBODFP1                      source/constraints/fxbody/fxbodfp.F
Chd|        FXBODFP2                      source/constraints/fxbody/fxbodfp.F
Chd|        SPMD_FXB_FOR                  source/mpi/kinematic_conditions/spmd_fxb_for.F
Chd|        SPMD_FXB_FOR_PON              source/mpi/kinematic_conditions/spmd_fxb_for_pon.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FXBYFOR(FXBIPM, FXBRPM,  FXBNOD , FXBMOD  , FXBGLM ,
     .                   FXBCPM, FXBCPS,  FXBLM  , FXBFLS  , FXBDLS ,
     .                   FXBDEP, FXBVIT,  FXBACC , A       , AR     ,
     .                   X     , FXBMVN,  FXBMCD , FXBSE   , FXBSV  ,
     .                   FXBELM, FXBSIG,  ELBUF  , PARTSAV , ELBUF_TAB,       
     .                   FSAV  , FXBFP ,  FXBEFW , FXBFC   , D      ,
     .                   DT2T  , ITYPTST, NELTST , FXBGRVI , FXBGRVR,
     .                   IGRV  , NPC    , TF     , FXBGRP  , FXBGRW ,
     .                   IPARG , SENSOR_TAB, IAD_ELEM, FR_ELEM,
     .                   AGRV  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "fxbcom.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBNOD(*), FXBELM(*), ITYPTST, NELTST,
     .        FXBGRVI(*), IGRV(NIGRV,*), NPC(*), IPARG(NPARG,*),
     .        IAD_ELEM(2,*), FR_ELEM(*)
      my_real
     .   FXBRPM(*), FXBMOD(*), FXBGLM(*), FXBCPM(*), FXBCPS(*), 
     .   FXBLM(*),  FXBFLS(*), FXBDLS(*), FXBDEP(*), FXBVIT(*),
     .   FXBACC(*), A(3,*),    AR(3,*),   X(3,*),    FXBMVN(*),
     .   FXBMCD(*), FXBSE(*),  FXBSV(*),  FXBSIG(*), ELBUF(*) , 
     .   PARTSAV(*),FSAV(NTHVKI,*), FXBFP(*),FXBEFW(*),
     .   FXBFC(*),  D(3,*), DT2T, FXBGRVR(*), TF(*), FXBGRP(*),
     .   FXBGRW(*), AGRV(LFACGRV,*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP) :: ELBUF_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX,ANOD,AMOD,AGLM,ACP,ALM,AFLS,ADLS,AVAR,ARPM,
     .        NME,NMOD,AMCD,ASE,ITN,NCYCLP1,AELM,ASIG,N2,NN2,
     .        AGRVI,AGRVR,NLGRAV,NSN,I,PMAIN
      my_real
     .        MFEXTP(LENVAR)
      INTEGER NSNT, NMMAX, IAD(NFXBODY), ANOD2, J
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: FSKYFXB
C=======================================================================
      N2=NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU
      ITN=0
      IF(TT<TANIM) ITN=1
      NCYCLP1=NCYCLE+1
      IF((NCYCLP1/NCRST)*NCRST==NCYCLP1.OR.MREST/=0) ITN=0
      DO I=1,LENVAR
         MFEXTP(I)=ZERO
      ENDDO
      IF (IPARIT/=0) THEN
         NSNT=0
         NMMAX=0
         DO NFX=1,NFXBODY
            NMOD=FXBIPM(4,NFX)
            NME=FXBIPM(17,NFX)
            NSN=FXBIPM(18,NFX)
            NMMAX=MAX(NMMAX,NME+NMOD)
            IAD(NFX)=NSNT
            NSNT=NSNT+NSN
         ENDDO
         ALLOCATE(FSKYFXB(NSNT,1+NMMAX))
         DO I=1,NSNT
            DO J=1,NMMAX
               FSKYFXB(I,J)=ZERO
            ENDDO
         ENDDO
      ELSE
         ALLOCATE(FSKYFXB(0,0))
      ENDIF
      DO NFX=1,NFXBODY
         NMOD=FXBIPM(4,NFX)
         ANOD=FXBIPM(6,NFX)
         AMOD=FXBIPM(7,NFX)
         AVAR=FXBIPM(13,NFX)
         ARPM=FXBIPM(14,NFX)
         NME=FXBIPM(17,NFX)
         AELM=FXBIPM(19,NFX)
         ASIG=FXBIPM(20,NFX)
         NSN=FXBIPM(18,NFX)
         ANOD2=ANOD+FXBIPM(3,NFX)
         IF (NSN+FXBIPM(3,NFX)>0) 
     .CALL FXBODFP1(
     .FXBIPM(1,NFX),FXBRPM(ARPM),FXBNOD(ANOD),FXBMOD(AMOD),FXBDEP(AVAR),
     .FXBVIT(AVAR) ,FXBACC(AVAR),A           ,AR          ,NME         ,
     .NMOD         ,ITN         ,FXBELM      ,FXBSIG      ,ELBUF       ,
     .PARTSAV      ,X           ,D           ,IPARG       ,NFX         ,
     .NSN          ,MFEXTP(AVAR),IAD_ELEM    ,FR_ELEM     ,NSNT        ,
     .FSKYFXB      ,IAD(NFX)    ,FXBNOD(ANOD2),ELBUF_TAB  )
      ENDDO
C
      IF (IPARIT==0) THEN
         IF(NSPMD > 1) CALL SPMD_FXB_FOR(FXBIPM, MFEXTP)
      ELSE
         CALL SPMD_FXB_FOR_PON(FXBIPM, MFEXTP, FSKYFXB, NSNT, IAD)
      ENDIF
      DEALLOCATE(FSKYFXB)
C
      DO NFX=1,NFXBODY
         NN2=N2+NFX
         NMOD=FXBIPM(4,NFX)
         AGLM=FXBIPM(8,NFX)
         ACP =FXBIPM(9,NFX)
         ALM =FXBIPM(10,NFX)
         AFLS=FXBIPM(11,NFX)
         ADLS=FXBIPM(12,NFX)
         AVAR=FXBIPM(13,NFX)
         ARPM=FXBIPM(14,NFX)
         AMCD=FXBIPM(15,NFX)
         ASE =(NFX-1)*15+1
         NME=FXBIPM(17,NFX)
         NLGRAV=FXBIPM(25,NFX)
         AGRVI=FXBIPM(26,NFX)
         AGRVR=FXBIPM(27,NFX)
         PMAIN=FXBIPM(39,NFX)
         IF (ISPMD==PMAIN) THEN
            IF (DT2T>DTFAC1(11)*FXBRPM(ARPM)) THEN
                DT2T=DTFAC1(11)*FXBRPM(ARPM)
                ITYPTST=11
                NELTST=FXBIPM(2,NFX)
            ENDIF
            CALL FXBODFP2(
     .FXBIPM(1,NFX),FXBRPM(ARPM),FXBGLM(AGLM),FXBCPM(ACP) ,FXBCPS(ACP) ,
     .FXBLM(ALM)   ,FXBFLS(AFLS),FXBDLS(ADLS),FXBDEP(AVAR),FXBVIT(AVAR),
     .NME          ,NMOD        ,FXBMVN(ACP) ,FXBMCD(AMCD),FXBSE(ASE)  ,
     .FXBSV(ALM)   ,FSAV(1,NN2) ,FXBFP(AVAR) ,FXBEFW(NFX) ,FXBFC(ALM)  ,
     .FXBGRVI(AGRVI),FXBGRVR(AGRVR),NLGRAV   ,IGRV        ,NPC         ,
     .TF           ,FXBGRP(AVAR),FXBGRW(NFX) ,SENSOR_TAB  ,
     .MFEXTP(AVAR) ,AGRV        )
         ENDIF
      ENDDO
C
      RETURN
      END
          
