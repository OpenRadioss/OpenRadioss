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
#include      "vect01_c.inc"
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
              CALL TSH_DIR2(X,IXS,DIR,DIRB,ICSTR,NEL)
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
              CALL TSH_DIR2(X,IXS,DIR,DIRB,ICSTR,NEL)
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
              CALL TSH_DIR2(X,IXS,DIR,DIRB,ICSTR,NEL)
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
              CALL TSH_DIR2(X,IXS,DIR,DIRB,ICSTR,NEL)
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
Chd|====================================================================
Chd|  TSH_DIR2                      source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        H3D_FLD_STRAIN                source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|        SCORTHO3                      source/elements/thickshell/solidec/scortho3.F
Chd|        SREPISOT3                     source/elements/solid/solide/srepisot3.F
Chd|====================================================================
      SUBROUTINE TSH_DIR2(X,IXS,DIR,DIRB,ICSTR,NEL)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICSTR,NEL,IXS(NIXS,*)
      my_real
     .   X(3,*),DIR(MVSIZ,2),DIRB(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      INTEGER 
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),IX5(MVSIZ),
     .   IX6(MVSIZ),IX7(MVSIZ),IX8(MVSIZ)        
C     REAL
      my_real 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),
     .   E1Y(MVSIZ),E2Y(MVSIZ),E3Y(MVSIZ),
     .   E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),
     .   V1,V2,V3,VR,VS,AA,BB,SUMA
C-----------------------------------------------
      IF (IREP == 0) THEN                   
        DO I=1,NEL                        
          DIRB(I,1) = DIR(I,1)              
          DIRB(I,2) = DIR(I,2)              
        ENDDO                               
      ELSE                                  
        DO I=1,NEL
          IX1(I)=IXS(2,I)
          IX2(I)=IXS(3,I)
          IX3(I)=IXS(4,I)
          IX4(I)=IXS(5,I)
          IX5(I)=IXS(6,I)
          IX6(I)=IXS(7,I)
          IX7(I)=IXS(8,I)
          IX8(I)=IXS(9,I)
          X1(I)=X(1,IX1(I))
          Y1(I)=X(2,IX1(I))
          Z1(I)=X(3,IX1(I))
          X2(I)=X(1,IX2(I))
          Y2(I)=X(2,IX2(I))
          Z2(I)=X(3,IX2(I))
          X3(I)=X(1,IX3(I))
          Y3(I)=X(2,IX3(I))
          Z3(I)=X(3,IX3(I))
          X4(I)=X(1,IX4(I))
          Y4(I)=X(2,IX4(I))
          Z4(I)=X(3,IX4(I))
          X5(I)=X(1,IX5(I))
          Y5(I)=X(2,IX5(I))
          Z5(I)=X(3,IX5(I))
          X6(I)=X(1,IX6(I))
          Y6(I)=X(2,IX6(I))
          Z6(I)=X(3,IX6(I))
          X7(I)=X(1,IX7(I))
          Y7(I)=X(2,IX7(I))
          Z7(I)=X(3,IX7(I))
          X8(I)=X(1,IX8(I))
          Y8(I)=X(2,IX8(I))
          Z8(I)=X(3,IX8(I))
        ENDDO
       CALL SREPISOT3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      X7,      X8,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Y7,      Y8,
     5   Z1,      Z2,      Z3,      Z4,
     6   Z5,      Z6,      Z7,      Z8,
     7   RX,      RY,      RZ,      SX,
     8   SY,      SZ,      TX,      TY,
     9   TZ,      NEL)
        CALL SCORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     NEL)
       IF (JHBE == 15) THEN                   
        DO I=1,NEL                        
          AA = DIR(I,1)                     
          BB = DIR(I,2)                     
          V1 = AA*TX(I) + BB*RX(I)          
          V2 = AA*TY(I) + BB*RY(I)          
          V3 = AA*TZ(I) + BB*RZ(I)          
          VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)  
          VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)  
          SUMA=SQRT(VR*VR + VS*VS)          
          SUMA=ONE/MAX(EM20,SUMA)            
          DIRB(I,1) = VR*SUMA               
          DIRB(I,2) = VS*SUMA               
        ENDDO                               
       ELSEIF (JHBE == 14) THEN                   
         SELECT CASE (ICSTR)                                             
          CASE (1)                                                        
          DO I=1,NEL
            AA = DIR(I,1)                     
            BB = DIR(I,2)                     
            V1 = AA*RX(I) + BB*SX(I)
            V2 = AA*RY(I) + BB*SY(I)
            V3 = AA*RZ(I) + BB*SZ(I)
            VR=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            VS=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            DIRB(I,1) = VR*SUMA               
            DIRB(I,2) = VS*SUMA               
          ENDDO
         CASE (100)                                                        
          DO I=1,NEL
            AA = DIR(I,1)                     
            BB = DIR(I,2)                     
            V1 = AA*SX(I) + BB*TX(I)
            V2 = AA*SY(I) + BB*TY(I)
            V3 = AA*SZ(I) + BB*TZ(I)
            VR=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            VS=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            DIRB(I,1) = VR*SUMA               
            DIRB(I,2) = VS*SUMA               
          ENDDO
        CASE (10)                                                        
          DO I=1,NEL
            AA = DIR(I,1)                     
            BB = DIR(I,2)                     
            V1 = AA*TX(I) + BB*RX(I)
            V2 = AA*TY(I) + BB*RY(I)
            V3 = AA*TZ(I) + BB*RZ(I)
            VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            DIRB(I,1) = VR*SUMA               
            DIRB(I,2) = VS*SUMA               
          ENDDO
         END SELECT                                                      
       END IF !(JHBE == 15) THEN                   
      ENDIF                                 
C-----------
      RETURN
      END SUBROUTINE TSH_DIR2
Chd|====================================================================
Chd|  ROTO_SIG2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        H3D_FLD_STRAIN                source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTO_SIG2D(JFT,JLT,SIG,DIR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real
     .   SIG(3,MVSIZ), DIR(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   X2,Y2,XY,XYS2,SIGN(3)
C-----------------------------------------------
      DO I=JFT,JLT
         X2=DIR(I,1)*DIR(I,1)
         Y2=DIR(I,2)*DIR(I,2)
         XY=DIR(I,1)*DIR(I,2)
         XYS2=TWO*XY*SIG(3,I)
         SIGN(1) = X2*SIG(1,I)+Y2*SIG(2,I)-XYS2
         SIGN(2) = Y2*SIG(1,I)+X2*SIG(2,I)+XYS2
         SIGN(3) =(SIG(1,I)-SIG(2,I))*XY+(X2-Y2)*SIG(3,I)
         SIG(1,I) = SIGN(1)
         SIG(2,I) = SIGN(2)
         SIG(3,I) = SIGN(3)
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  TENS3DTO2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        H3D_SOL_SKIN_SCALAR1          source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        ROT3DTO2D                     source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|====================================================================
      SUBROUTINE TENS3DTO2D(NEL,IXC,X,TEN3,TEN2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,IXC(5,*)
      my_real
     .   X(3,*),TEN3(6,*),TEN2(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N , K
      INTEGER 
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ)
C     REAL
      my_real 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,
     .   SX(MVSIZ) ,SY(MVSIZ) ,SZ(MVSIZ) ,
     .   TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),
     .   E1Y(MVSIZ),E2Y(MVSIZ),E3Y(MVSIZ),
     .   E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),
     .   DETA1(MVSIZ),OFFG(MVSIZ),TENS(6,MVSIZ)  
C-----------------------------------------------
        DO I=1,NEL
          IX1(I)=IXC(2,I)
          IX2(I)=IXC(3,I)
          IX3(I)=IXC(4,I)
          IX4(I)=IXC(5,I)
          X1(I)=X(1,IX1(I))
          Y1(I)=X(2,IX1(I))
          Z1(I)=X(3,IX1(I))
          X2(I)=X(1,IX2(I))
          Y2(I)=X(2,IX2(I))
          Z2(I)=X(3,IX2(I))
          X3(I)=X(1,IX3(I))
          Y3(I)=X(2,IX3(I))
          Z3(I)=X(3,IX3(I))
          X4(I)=X(1,IX4(I))
          Y4(I)=X(2,IX4(I))
          Z4(I)=X(3,IX4(I))
        ENDDO
        DO I=1,NEL
          RX(I)=X2(I)+X3(I)-X1(I)-X4(I)
          SX(I)=X3(I)+X4(I)-X1(I)-X2(I)
          RY(I)=Y2(I)+Y3(I)-Y1(I)-Y4(I)
          SY(I)=Y3(I)+Y4(I)-Y1(I)-Y2(I)
          RZ(I)=Z2(I)+Z3(I)-Z1(I)-Z4(I)
          SZ(I)=Z3(I)+Z4(I)-Z1(I)-Z2(I)
        ENDDO
       K = 0
       OFFG(1:NEL) = ONE
       CALL CLSKEW3(1,NEL,K,
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z,
     .   DETA1,OFFG )
        DO I=1,NEL
          IF (IX3(I)==IX4(I)) THEN
           TENS(1:6,I) = THIRD*(TEN3(1:6,IX1(I))+TEN3(1:6,IX2(I))+TEN3(1:6,IX3(I)))
          ELSE
           TENS(1:6,I) = FOURTH*(TEN3(1:6,IX1(I))+TEN3(1:6,IX2(I))
     .                         +TEN3(1:6,IX3(I))+TEN3(1:6,IX4(I)))
          END IF
        ENDDO
        CALL ROT3DTO2D(NEL,TENS,TEN2,E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z)
C-----------
      RETURN
      END SUBROUTINE TENS3DTO2D
Chd|====================================================================
Chd|  ROT3DTO2D                     source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        TENS3DTO2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROT3DTO2D(NEL,SIG3D,SIG2D,
     .                     G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   SIG3D(6,*),SIG2D(3,*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SX,SY,SZ
C-----------------------------------------------
      DO I=1,NEL
         SX = SIG3D(1,I)*G1X(I)+SIG3D(4,I)*G1Y(I)+SIG3D(6,I)*G1Z(I)
         SY = SIG3D(4,I)*G1X(I)+SIG3D(2,I)*G1Y(I)+SIG3D(5,I)*G1Z(I)
         SZ = SIG3D(6,I)*G1X(I)+SIG3D(5,I)*G1Y(I)+SIG3D(3,I)*G1Z(I)
         SIG2D(1,I) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIG2D(3,I) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SX = SIG3D(1,I)*G2X(I)+SIG3D(4,I)*G2Y(I)+SIG3D(6,I)*G2Z(I)
         SY = SIG3D(4,I)*G2X(I)+SIG3D(2,I)*G2Y(I)+SIG3D(5,I)*G2Z(I)
         SZ = SIG3D(6,I)*G2X(I)+SIG3D(5,I)*G2Y(I)+SIG3D(3,I)*G2Z(I)
         SIG2D(2,I) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE ROT3DTO2D
Chd|====================================================================
Chd|  REORDER_N                     source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_N(N,IC)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IMIN,IT,II
C
      IF (N<=0) RETURN
      DO I =1,N
       IMIN=IC(I)
       II=I
       DO J =I+1,N
        IF (IC(J)<IMIN) THEN
         IMIN=IC(J)
         II=J
        ENDIF
       ENDDO
       IT=IC(I)
       IC(I)=IMIN
       IC(II)=IT
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C                                                                     12
