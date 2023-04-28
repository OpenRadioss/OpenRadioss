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
Chd|  H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ROTO_SIG2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        TSH_DIR2                      source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_TENSOR(
     .                   ELBUF_TAB,SKIN_TENSOR, IPARG   ,IXS     ,X     ,PM  ,
     4                   IPARTS  ,IPM     ,IGEO    ,IXS10 ,IXS16 , IXS20  ,
     5                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD,
     6                   IAD_ELEM        ,FR_ELEM     , WEIGHT   ,TAG_SKINS6)
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
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "nchar_c.inc"  
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SKIN_TENSOR(3,*),PM(NPROPM,*), X(3,*)
      INTEGER IPARG(NPARG,*), 
     .   IXS(NIXS,*),IPM(NPROPMI,*),IPARTS(*),
     .   IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,
     .   IGEO(NPROPGI,*),IS_WRITTEN_SKIN(*),
     .   H3D_PART(*),INFO1,TAG_SKINS6(*),IAD_ELEM(2,*),FR_ELEM(*),WEIGHT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      my_real
     .   EVAR(3,MVSIZ),DIR(MVSIZ,2),DIRB(MVSIZ,2)
      my_real
     .   F_EXP,F_STR,F_GAUSS(9)
      INTEGER I,I1,II,J,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,
     .        ID_PLY,NB_PLYOFF,NG,NSKIN,ICSTR
      INTEGER NPT_ALL,IPLY,ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,
     .        ISTRAIN,KCVT,IOR_TSH,MT1,ICSIG,PTI,IOK,IPRT,IOK_PART(MVSIZ),
     .        JJ(6),IS_WRITTEN_TENSOR(MVSIZ),MLWI,MID,PID

      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
C----- facter of extrapolation
      DATA F_GAUSS / 
     9 1.000000000000000,1.732050807568877,1.290994448735806,
     9 1.161256338324528,1.103533701926633,1.072421119155361,
     9 1.053620970803647,1.041352247171806,1.032886870574820/
       INTEGER :: NFT,IAD,ITY,NPT,JALE,ISMSTR,JEUL,JTUR,JTHE,JLAG,JMULT
       INTEGER :: JHBE,JIVF,NVAUX,JPOR,JCLOSE,JPLASOL,IREP,IINT,IGTYP,ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE,JSMS

C-----------------------------------------------
       NSKIN = 0
       IOK_PART(1:MVSIZ)=0
       DO NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        ICSTR = IPARG(17,NG)
        ISTRAIN = IPARG(44,NG)
        ISOLNOD = IPARG(28,NG)
        IVISC = IPARG(61,NG)
        CALL INITBUF(IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
       DO I=1,6
         JJ(I) = NEL*(I-1)
       ENDDO
!
       IF(MLW == 13 .OR. MLW == 0) CYCLE          
C-----------------------------------------------
C       THICK-SHELL 
C-----------------------------------------------
!                8--------------7
!               / |            /|
!              5--------------|6
!              |  |           | |
!              |  4-----------|-3
!              | /            |/     
!              1--------------2
        IF (ITY == 1.AND.(IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22)) THEN
          IOR_TSH = 0
          IF (IGTYP == 21) THEN
           IOR_TSH = 1
          ELSEIF (IGTYP == 22) THEN
           IOR_TSH = 2
          END IF
          IF (KCVT==1.AND.IOR_TSH/=0) KCVT=2
          IOK_PART(1:NEL) = 0
          DO  I=1,NEL 
            N = I + NFT
            IF( H3D_PART(IPARTS(N)) == 1) IOK_PART(I) = 1
            IS_WRITTEN_TENSOR(I) = 0
            EVAR(1:3,I) = ZERO
          ENDDO
          NLAY = ELBUF_TAB(NG)%NLAY                
          NPTR = ELBUF_TAB(NG)%NPTR                 
          NPTS = ELBUF_TAB(NG)%NPTS                 
          NPTT = ELBUF_TAB(NG)%NPTT
            IF (IGTYP == 22 .AND. NLAY>9) THEN
             F_EXP = ONE
            ELSE
             F_EXP = F_GAUSS(NLAY)
            END IF
            IF (JHBE==14.OR.JHBE==16)   F_EXP = F_EXP/(NPTR*NPTS)
C-----------------------------------------------
          IF (KEYWORD == 'TENS/STRESS/OUTER') THEN
            IS_WRITTEN_TENSOR(1:NEL) = 1
C-----------------------------------------------
              ILAY=1
              IT = 1
C-------- grp skin_inf first
           IF (JHBE==15) THEN
              IR = 1
              IS = 1
              LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
             DO I=1,NEL
               EVAR(1:2,I) = LBUF%SIG(JJ(1:2) + I)
               EVAR(3,I) = LBUF%SIG(JJ(4) + I)
             ENDDO
             IF(IVISC > 0) THEN
           	DO I=1,NEL
                  EVAR(1:2,I) = EVAR(1:2,I) + LBUF%VISC(JJ(1:2) + I)
                  EVAR(3,I) = EVAR(3,I) + LBUF%VISC(JJ(4) + I)
           	ENDDO
             ENDIF
           ELSE ! 14,16
             DO IR=1,NPTR
               DO IS=1,NPTS
                  LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                  DO I=1,NEL
                    EVAR(1:2,I) = EVAR(1:2,I) + LBUF%SIG(JJ(1:2) + I)
                    EVAR(3,I)   = EVAR(3,I) + LBUF%SIG(JJ(4) + I)
                  ENDDO
                  IF(IVISC > 0) THEN
                      DO I=1,NEL
                       EVAR(1:2,I) = EVAR(1:2,I) + LBUF%VISC(JJ(1:2) + I)
                       EVAR(3,I)   = EVAR(3,I) + LBUF%VISC(JJ(4) + I)
                      ENDDO
                  ENDIF
               ENDDO 
             ENDDO
C----------            
           END IF !IF (JHBE==15)             
           EVAR(1:3,1:NEL) = F_EXP*EVAR(1:3,1:NEL)
C------to see if need rotate EVAR, Isolid=16 incompatible to orthotropic              
           IF (KCVT==2) THEN
              IF(IOR_TSH==1)THEN
                DO I=1,NEL
                 DIR(I,1:2)= GBUF%GAMA(JJ(1:2) + I)
                ENDDO
              ELSEIF(IOR_TSH==2)THEN
                IF(JHBE==14)THEN
                 IR = 1
                 IS = 1
                END IF
                LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                DO I=1,NEL
                 DIR(I,1:2)= LBUF%GAMA(JJ(1:2) + I)
                ENDDO
              END IF
              CALL TSH_DIR2(
     1   X,       IXS,     DIR,     DIRB,
     2   ICSTR,   NEL,     IREP,    JHBE)
              CALL ROTO_SIG2D(1,NEL,EVAR,DIRB)
           END IF !(KCVT==2) THEN
           DO I=1,NEL
             N = I + NFT
             SKIN_TENSOR(1:3,NSKIN+I) = EVAR(1:3,I)
             IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_TENSOR(I)
           END DO
           NSKIN = NSKIN + NEL
           EVAR(1:3,1:NEL) = ZERO
C-------- grp skin_up
              ILAY=NLAY
              IT = 1
           IF (JHBE==15) THEN
              IR = 1
              IS = 1
              LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
             DO I=1,NEL
               EVAR(1:2,I) = LBUF%SIG(JJ(1:2) + I)
               EVAR(3,I) = LBUF%SIG(JJ(4) + I)
             ENDDO
             IF(IVISC > 0) THEN
           	DO I=1,NEL
                  EVAR(1:2,I) = EVAR(1:2,I) + LBUF%VISC(JJ(1:2) + I)
                  EVAR(3,I)   = EVAR(3,I) + LBUF%VISC(JJ(4) + I)
           	ENDDO
             ENDIF
           ELSE ! 14,16
             DO IR=1,NPTR
               DO IS=1,NPTS
                  LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                  DO I=1,NEL
                    EVAR(1:2,I) = EVAR(1:2,I) + LBUF%SIG(JJ(1:2) + I)
                    EVAR(3,I)   = EVAR(3,I) + LBUF%SIG(JJ(4) + I)
                  ENDDO
                  IF(IVISC > 0) THEN
                      DO I=1,NEL
                       EVAR(1:2,I) = EVAR(1:2,I) + LBUF%VISC(JJ(1:2) + I)
                       EVAR(3,I)   = EVAR(3,I) + LBUF%VISC(JJ(4) + I)
                      ENDDO
                  ENDIF
               ENDDO 
             ENDDO
           END IF   !IF (JHBE==15)            
           EVAR(1:3,1:NEL) = F_EXP*EVAR(1:3,1:NEL)
C--- orthotropic              
           IF (KCVT==2) THEN
              IF(IOR_TSH==1)THEN
                DO I=1,NEL
                 DIR(I,1:2)= GBUF%GAMA(JJ(1:2) + I)
                ENDDO
              ELSEIF(IOR_TSH==2)THEN
                IF(JHBE==14)THEN
                 IR = 1
                 IS = 1
                END IF
                LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                DO I=1,NEL
                 DIR(I,1:2)= LBUF%GAMA(JJ(1:2) + I)
                ENDDO
              END IF
              CALL TSH_DIR2(
     1   X,       IXS,     DIR,     DIRB,
     2   ICSTR,   NEL,     IREP,    JHBE)
              CALL ROTO_SIG2D(1,NEL,EVAR,DIRB)
           END IF !(KCVT==2) THEN
           DO I=1,NEL
             N = I + NFT
             SKIN_TENSOR(1:3,NSKIN+I) = EVAR(1:3,I)
             IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_TENSOR(I)
           END DO
           NSKIN = NSKIN + NEL
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN/OUTER') THEN
C-----------------------------------------------
            IS_WRITTEN_TENSOR(1:NEL) = 1
C-------- grp skin_inf first
              ILAY=1
              IT = 1
              MLWI = MLW
              IF (IGTYP == 22) THEN
               PID = IXS(NIXS-1,1 + NFT)
               MID = IGEO(100+ILAY,PID)
               MLWI=NINT(PM(19,MID))
              END IF
           IF (JHBE==15) THEN
              IR = 1
              IS = 1
              LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
               IF (MLWI == 12 .OR. MLWI == 14) THEN
                 DO I=1,NEL						   
                  EVAR(1:2,I) = LBUF%EPE(JJ(1:2) + I)       
                  EVAR(3,I)   = HALF*LBUF%EPE(JJ(4) + I)
                 ENDDO						       
               ELSEIF (MLWI /= 49 ) THEN                 
                 DO I=1,NEL						   
                  EVAR(1:2,I) = LBUF%STRA(JJ(1:2) + I)       
                  EVAR(3,I)   = HALF*LBUF%STRA(JJ(4) + I)
                 ENDDO						       
               ELSE
                IS_WRITTEN_TENSOR(1:NEL) = 0
               END IF               
C------to see if need rotate EVARL              
           ELSE ! 14,16
             DO IR=1,NPTR
               DO IS=1,NPTS
                  LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                  IF (MLWI == 12 .OR. MLWI == 14) THEN
                    DO I=1,NEL						   
                     EVAR(1:2,I) = EVAR(1:2,I)+LBUF%EPE(JJ(1:2) + I)       
                     EVAR(3,I)   = EVAR(3,I)+HALF*LBUF%EPE(JJ(4) + I)
                    ENDDO						       
                  ELSEIF (MLWI /= 49 ) THEN                 
                    DO I=1,NEL						   
                     EVAR(1:2,I) = EVAR(1:2,I)+LBUF%STRA(JJ(1:2) + I)       
                     EVAR(3,I)   = EVAR(3,I)+ HALF*LBUF%STRA(JJ(4) + I)
                    ENDDO						       
                  ELSE
                   IS_WRITTEN_TENSOR(1:NEL) = 0
                  END IF               
               ENDDO 
             ENDDO
           END IF               
           EVAR(1:3,1:NEL) = F_EXP*EVAR(1:3,1:NEL)
C------to see if need rotate EVAR, Isolid=16 incompatible to orthotropic              
           IF (KCVT==2) THEN
              IF(IOR_TSH==1)THEN
                DO I=1,NEL
                 DIR(I,1:2)= GBUF%GAMA(JJ(1:2) + I)
                ENDDO
              ELSEIF(IOR_TSH==2)THEN
                IF(JHBE==14)THEN
                 IR = 1
                 IS = 1
                END IF
                LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                DO I=1,NEL
                 DIR(I,1:2)= LBUF%GAMA(JJ(1:2) + I)
                ENDDO
              END IF
              CALL TSH_DIR2(
     1   X,       IXS,     DIR,     DIRB,
     2   ICSTR,   NEL,     IREP,    JHBE)
              CALL ROTO_SIG2D(1,NEL,EVAR,DIRB)
           END IF !(KCVT==2) THEN
           DO I=1,NEL
             N = I + NFT
             SKIN_TENSOR(1:3,NSKIN+I) = EVAR(1:3,I)
             IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_TENSOR(I)
           END DO
           NSKIN = NSKIN + NEL
           EVAR(1:3,1:NEL) = ZERO
C-------- grp skin_sup 
              ILAY=NLAY
              IT = 1
              MLWI = MLW
              IF (IGTYP == 22) THEN
               PID = IXS(NIXS-1,1 + NFT)
               MID = IGEO(100+ILAY,PID)
               MLWI=NINT(PM(19,MID))
              END IF
           IF (JHBE==15) THEN
              IR = 1
              IS = 1
              LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
               IF (MLWI == 12 .OR. MLWI == 14) THEN
                 DO I=1,NEL						   
                  EVAR(1:2,I) = LBUF%EPE(JJ(1:2) + I)       
                  EVAR(3,I)   = HALF*LBUF%EPE(JJ(4) + I)
                 ENDDO						       
               ELSEIF (MLWI /= 49 ) THEN                 
                 DO I=1,NEL						   
                  EVAR(1:2,I) = LBUF%STRA(JJ(1:2) + I)       
                  EVAR(3,I)   = HALF*LBUF%STRA(JJ(4) + I)
                 ENDDO						       
               ELSE
                IS_WRITTEN_TENSOR(1:NEL) = 0
               END IF               
C------to see if need rotate EVARL              
           ELSE ! 14,16
             DO IR=1,NPTR
               DO IS=1,NPTS
                  LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                  IF (MLWI == 12 .OR. MLWI == 14) THEN
                    DO I=1,NEL						   
                     EVAR(1:2,I) = EVAR(1:2,I)+LBUF%EPE(JJ(1:2) + I)       
                     EVAR(3,I)   = EVAR(3,I)+HALF*LBUF%EPE(JJ(4) + I)
                    ENDDO						       
                  ELSEIF (MLWI /= 49 ) THEN                 
                    DO I=1,NEL						   
                     EVAR(1:2,I) = EVAR(1:2,I)+LBUF%STRA(JJ(1:2) + I)       
                     EVAR(3,I)   = EVAR(3,I)+ HALF*LBUF%STRA(JJ(4) + I)
                    ENDDO						       
                  ELSE
                   IS_WRITTEN_TENSOR(1:NEL) = 0
                  END IF               
               ENDDO 
             ENDDO
           END IF               
           EVAR(1:3,1:NEL) = F_EXP*EVAR(1:3,1:NEL)
C------to see if need rotate EVAR, Isolid=16 incompatible to orthotropic              
           IF (KCVT==2) THEN
              IF(IOR_TSH==1)THEN
                DO I=1,NEL
                 DIR(I,1:2)= GBUF%GAMA(JJ(1:2) + I)
                ENDDO
              ELSEIF(IOR_TSH==2)THEN
                IF(JHBE==14)THEN
                 IR = 1
                 IS = 1
                END IF
                LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)         
                DO I=1,NEL
                 DIR(I,1:2)= LBUF%GAMA(JJ(1:2) + I)
                ENDDO
              END IF
              CALL TSH_DIR2(
     1   X,       IXS,     DIR,     DIRB,
     2   ICSTR,   NEL,     IREP,    JHBE)
              CALL ROTO_SIG2D(1,NEL,EVAR,DIRB)
           END IF !(KCVT==2) THEN
           DO I=1,NEL
             N = I + NFT
             SKIN_TENSOR(1:3,NSKIN+I) = EVAR(1:3,I)
             IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_TENSOR(I)
           END DO
           NSKIN = NSKIN + NEL
          END IF  !(KEYWORD
        ENDIF !IF (ITY == 1.AND.(IGTYP==20 
C           
       END DO ! NG=1,NGROUP
C------for solid elements
       IF (NUMSKIN> NSKIN)       
     .  CALL H3D_SOL_SKIN_TENSOR(
     .                   ELBUF_TAB,SKIN_TENSOR, IPARG   ,IXS     ,X     ,PM  ,
     4                   IPARTS  ,IPM     ,IGEO    ,IXS10 ,IXS16 , IXS20  ,
     5                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD ,NSKIN ,
     6                   IAD_ELEM        ,FR_ELEM     , WEIGHT   ,TAG_SKINS6)
C-----------
      RETURN
      END
