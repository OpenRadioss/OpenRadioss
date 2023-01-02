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
Chd|  H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        H3D_WRITE_SCALAR              source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SCHLIEREN                     source/output/anim/generate/schlieren.F
Chd|        SROTORTH                      source/elements/solid/srotorth.F
Chd|        THS_MAREA                     source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        THS_VOL                       source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SOLID_SCALAR_1(
     .                  ELBUF_TAB       ,SOLID_SCALAR ,IFUNC     ,IPARG       ,GEO         ,
     .                  IXQ             ,IXS          ,IXTG      ,PM          ,BUFMAT      ,
     .                  EL2FA           ,NBF          ,IXUR      ,IADP        ,
     .                  NBF_L           ,EHOUR        ,ANIM      ,NBPART      ,IADG        ,
     .                  IPM             ,IGEO         ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3 ,
     .                  INVERT          ,X            ,V         ,W           ,ALE_CONNECT ,
     .                  NV46            ,NERCVOIS     ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK           ,ID_ELEM      ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                  IR_INPUT        ,IS_INPUT     ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SOLID,INFO1        ,KEYWORD   ,FANI_CELL   ,SFANI_CELL  ,
     .                  MULTI_FVM       , H3D_DATA    ,NG        ,IDMDS       ,IMDSVAR     ,
     .                  MDS_MATID       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD       
      USE H3D_MOD        
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr25_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "mmale51_c.inc"
#include      "alefvm.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SOLID_SCALAR(*),X(3,*),V(3,*),W(3,*),THKE(*),EHOUR(*),GEO(NPROPG,*),
     .   ANIM(*),PM(NPROPM,*),ERR_THK_SH4(*), ERR_THK_SH3(*)
      my_real, INTENT(IN),TARGET :: BUFMAT(*)
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IXTG(NIXTG,*),EL2FA(*),
     .   IXQ(NIXQ,*),IXUR(NIXUR,*),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,*),
     .   IGEO(NPROPGI,*),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),IPARTS(*),
     .   H3D_PART(*),IS_WRITTEN_SOLID(*),INFO1,LAYER_INPUT,IR_INPUT,IS_INPUT,IT_INPUT,
     .   IUVAR_INPUT,SFANI_CELL,NG,IDMDS,IMDSVAR,MDS_MATID(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      my_real, INTENT(IN) :: FANI_CELL(SFANI_CELL)
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   EVAR(MVSIZ),DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   USER(NUMELS),VALUE(MVSIZ),MASS(MVSIZ),PRES(MVSIZ),MASS0,VOL
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM_,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),S11,S22,S33,S4,S5,S6,VONM,GAMA(6),
     .   T11,T21,T31,T12,T22,T32,T13,T23,T33,
     .   PHI,THETA,PSI,DAMMAX,EVAR_TMP,VEL(0:3),VFRAC(MVSIZ,1:21),TMP(3,8),
     .   CUMUL(3),VX,VY,VZ,NX,NY,NZ,SURF,TMP_2(MVSIZ,3)
      my_real
     .   G1(MVSIZ,3),G2(MVSIZ,3),G3(MVSIZ,3),VOLN(MVSIZ),AREAM(MVSIZ),
     .   RHO0,DET(MVSIZ),EZZ(MVSIZ),MAXDAMINI
      INTEGER I,I1,II,J,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),NB_PLYOFF,IUVAR,IDX,IPOS,ITRIMAT,
     .        IALEFVM_FLG, IMAT,IADBUF,NUPARAM,ICSIG,NC(8)
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,
     .        IOK_PART(MVSIZ),JJ(6),IRUPT,IOK,NPG_PLANE,NUMLAY,IJK,IIR,
     .        IS_WRITTEN_VALUE(MVSIZ),NFRAC,IU(4),IV,NB_FACE,KFACE,IS_EULER,IS_ALE,IAD2,KK
      CHARACTER*5 BUFF
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_MAT_)  ,POINTER :: MBUF      
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      my_real,
     .  DIMENSION(:), POINTER  :: UVARF,DAMF,DFMAX,TDELE
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      INTEGER :: ISUBMAT

C-----------------------------------------------

        CALL INITBUF (   IPARG   ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF (MLW /= 13) THEN
          NFT = IPARG(3,NG)
          ISOLNOD = IPARG(28,NG)
          IVISC = IPARG(61,NG)
          IOK_PART(1:NEL) = 0 
          LFT=1
          LLT=NEL
          IS_EULER=IPARG(11,NG)
          IS_ALE=IPARG(7,NG)
c
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          DO I=1,NEL
            VALUE(I) = ZERO
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	     
C-----------------------------------------------
          IF (ITY == 1) THEN
c           SOLID ELEMENTS
            IF (JCVT==1.AND.ISORTH/=0) JCVT=2
C-----------------------------------------------
            GBUF => ELBUF_TAB(NG)%GBUF
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)       
            NLAY = ELBUF_TAB(NG)%NLAY                    
            NPTR = ELBUF_TAB(NG)%NPTR                     
            NPTS = ELBUF_TAB(NG)%NPTS                     
            NPTT = ELBUF_TAB(NG)%NPTT                     
            NPTG = NPTT*NPTS*NPTR*NLAY
            TSHELL  = 0      
            TSH_ORT = 0  
            IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1 
            IF (IGTYP==21 .OR. IGTYP==22) TSH_ORT = 1               
c	           PID=IXS(10,1 + NFT)
c

            IF (ITY == 1) OFFSET = 0
c
            DO  I=1,NEL 
              IF (ITY == 1) THEN
                ID_ELEM(OFFSET+NFT+I) = IXS(NIXS,NFT+I)
                ITY_ELEM(OFFSET+NFT+I) = 1
                IF( H3D_PART(IPARTS(NFT+I)) == 1) IOK_PART(I) = 1
              ENDIF
            ENDDO  
c
            ILAY = LAYER_INPUT
            IUVAR = IUVAR_INPUT
            IF (KEYWORD == 'MDS') IUVAR = IMDSVAR
            IR = IR_INPUT
            IS = IS_INPUT
            IT = IT_INPUT
            IF (ILAY == -2) ILAY = 1
            IF (ILAY == -3) ILAY = NLAY
            IF (TSHELL == 1.AND.(IR_INPUT/=-1.AND.IS_INPUT/=-1.AND.IT_INPUT/=-1)) THEN
             IF (JHBE==15 ) THEN
              ILAY = IS_INPUT
              IR = 1
              IS = 1
              IT = 1
             ELSEIF (JHBE==14 ) THEN
               ICSIG = IPARG(17,NG)
               IF (ICSIG==100) THEN
                 IR = IS_INPUT
                 IS = IT_INPUT
                 ILAY = IR_INPUT
               ELSEIF (ICSIG==10) THEN
                 ILAY = IS_INPUT
                 IR = IT_INPUT
                 IS = IR_INPUT
               ELSEIF (ICSIG==1) THEN
                 ILAY = IT_INPUT
               END IF
              IT = 1
             ELSE
               ILAY = IS_INPUT
               IS = 1
             END IF
            END IF
C-----------------------------------------------
C Mass computation
C-----------------------------------------------
            IF (KEYWORD == 'MASS') THEN
              GBUF => ELBUF_TAB(NG)%GBUF
              IALEL=IPARG(7,NG)+IPARG(11,NG)
              DO I=1,NEL
                N = I + NFT
                IF (MLW == 0 .or. MLW == 13 .or. IGTYP == 0) THEN
                  MASS(I) = ZERO
                ELSEIF(IALEL == 0)THEN
                  MT=IXS(1,N)
                  MASS(I)=PM(89,MT)*GBUF%VOL(I)
                ELSE
                  OFF = MIN(GBUF%OFF(I),ONE)
                  MASS(I)=GBUF%RHO(I)*GBUF%VOL(I)*OFF
                ENDIF
                IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .            MASS(I) = MASS(I) * GBUF%FILL(I)
              ENDDO
            ENDIF
C-----------
            IF (MLW /= 0 .and. MLW /= 13 .and. IGTYP /= 0) THEN
              JTURB=IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG)) 
C--------------------------------------------------
              IF (KEYWORD == 'MASS') THEN   ! MASS
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = MASS(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'DT')THEN
C--------------------------------------------------  
                IF(GBUF%G_DT>0)THEN
                  DO I=1,NEL
                    VALUE(I) = GBUF%DT(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO 
                ENDIF 
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'EPSP' .AND. (MLW /= 12 .AND. MLW /=14 .AND. MLW /= 25))THEN
C--------------------------------------------------  
                DO I=1,NEL     
                  IF (MLW == 10 .OR. MLW == 21) THEN
                    VALUE(I) = LBUF%EPSQ(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ELSEIF (GBUF%G_PLA > 0) THEN  
                    VALUE(I) = GBUF%PLA(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDIF
                ENDDO 
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'WPLA' .AND. (MLW == 12 .OR. MLW == 14 .OR. MLW == 25))THEN
C--------------------------------------------------  
                 DO I=LFT,LLT   
                    VALUE(I) = ZERO                                      
                ENDDO  
                IF (ISOLNOD == 16 .OR. ISOLNOD == 20.OR.
     .             (ISOLNOD == 8  .AND.JHBE == 14).OR.
     .             ((ISOLNOD == 6 .OR.I SOLNOD == 8).AND.JHBE == 15))THEN
                    DO IL=1,NLAY                          
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                        DO IS=1,NPTS 
                         DO IT=1,NPTT                                      
                          DO IR=1,NPTR                                    
                            LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            DO I=LFT,LLT                                 
                               VALUE(I) = VALUE(I) + LBUF%PLA(I)/NPTG 
                               IS_WRITTEN_VALUE(I) = 1         
                            ENDDO
                           ENDDO    
                          ENDDO      
                        ENDDO        
                      ENDIF 
                    ENDDO            
                ELSE
                  DO I=LFT,LLT
                   IF (GBUF%G_PLA > 0)THEN
                      EVAR(I) = GBUF%PLA(I)
                      IS_WRITTEN_VALUE(I) = 1
                    ENDIF  
                  ENDDO
                ENDIF ! Isolid ...         
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'TSAIWU' .AND. GBUF%G_TSAIWU > 0) THEN
C--------------------------------------------------  
                DO I=LFT,LLT   
                  VALUE(I) = ZERO                                      
                ENDDO  
                DO IL=1,NLAY                          
                  IF (ELBUF_TAB(NG)%BUFLY(IL)%L_TSAIWU > 0) THEN
                    DO IS=1,NPTS 
                      DO IT=1,NPTT                                      
                        DO IR=1,NPTR                                    
                          LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                          DO I=LFT,LLT                                 
                            VALUE(I) = VALUE(I) + LBUF%TSAIWU(I)/NPTG 
                            IS_WRITTEN_VALUE(I) = 1         
                          ENDDO
                        ENDDO    
                      ENDDO      
                    ENDDO        
                  ENDIF 
                ENDDO
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'DENS')THEN
C--------------------------------------------------  
                 IF (MLW == 151) THEN
                    DO I = 1, NEL     
                       VALUE(I) = MULTI_FVM%RHO(I + NFT)
                       IS_WRITTEN_VALUE(I) = 1                       
                    ENDDO  
                 ELSE
                    DO I=1,NEL     
                       VALUE(I) = GBUF%RHO(I)
                       IS_WRITTEN_VALUE(I) = 1                       
                    ENDDO    
                 ENDIF
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'TEMP')THEN
C--------------------------------------------------  
                DO I=1,NEL     
                  IF (IPARG(1,NG)  ==  2 .AND. JTHE  ==  0) THEN       
                    VALUE(I) = PM(54,IXS(1,NFT+I)) + GBUF%EINT(I)*     
     .                        GBUF%VOL(I) * PM(53,IXS(1,NFT+I)) 
                    IS_WRITTEN_VALUE(I) = 1  
                  ELSEIF (GBUF%G_TEMP > 0) THEN           
                    VALUE(I) = GBUF%TEMP(I)
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDIF          
                ENDDO 
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'P')THEN
C--------------------------------------------------  
                 IF (MLW == 151) THEN
                    DO I = 1, NEL
                       VALUE(I) = MULTI_FVM%PRES(I + NFT) 
                       IS_WRITTEN_VALUE(I) = 1
                    ENDDO
                 ELSE
                  DO I=1,NEL
                      II = (I-1)*6                                  
                      N = I + NFT
                      S11 = GBUF%SIG(JJ(1) + I)
                      S22 = GBUF%SIG(JJ(2) + I)
                      S33 = GBUF%SIG(JJ(3) + I)
                      S4  = GBUF%SIG(JJ(4) + I) 
                      S5  = GBUF%SIG(JJ(5) + I) 
                      S6  = GBUF%SIG(JJ(6) + I) 
                      IF(IVISC > 0 ) THEN
                        S11 = S11 + LBUF%VISC(JJ(1) + I)
                        S22 = S22 + LBUF%VISC(JJ(2) + I)
                        S33 = S33 + LBUF%VISC(JJ(3) + I)
                        S4  = S4  + LBUF%VISC(JJ(4) + I)
                        S5  = S5  + LBUF%VISC(JJ(5) + I)
                        S6  = S6  + LBUF%VISC(JJ(6) + I)
                      ENDIF
                      P = - (S11  + S22 + S33 ) * THIRD
                      VALUE(I) = P 
                      IS_WRITTEN_VALUE(I) = 1
                  ENDDO 
               ENDIF
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'VONM')THEN
C--------------------------------------------------
                  DO I=1,NEL				  
                    N = I + NFT
                    S11 = GBUF%SIG(JJ(1) + I)
                    S22 = GBUF%SIG(JJ(2) + I)
                    S33 = GBUF%SIG(JJ(3) + I)
                    S4  = GBUF%SIG(JJ(4) + I) 
                    S5  = GBUF%SIG(JJ(5) + I) 
                    S6  = GBUF%SIG(JJ(6) + I) 
                    IF(IVISC > 0 ) THEN
                      S11 = S11 + LBUF%VISC(JJ(1) + I)
                      S22 = S22 + LBUF%VISC(JJ(2) + I)
                      S33 = S33 + LBUF%VISC(JJ(3) + I)
                      S4  = S4  + LBUF%VISC(JJ(4) + I)
                      S5  = S5  + LBUF%VISC(JJ(5) + I)
                      S6  = S6  + LBUF%VISC(JJ(6) + I)
                    ENDIF
                    P = - (S11  + S22 + S33 ) * THIRD
                    S1= S11 + P
                    S2= S22 + P
                    S3= S33 + P
                    VONM2= THREE*(S4*S4  +  S5*S5 + S6*S6 +
     .              	   HALF*(S1*S1 + S2*S2  + S3*S3) )
                    VONM= SQRT(VONM2)
                    VALUE(I) = VONM
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'K' .and. JTURB /= 0)THEN
C--------------------------------------------------
C                 ENERGIE TURBULENTE     
                  DO I=1,NEL         
                    VALUE(I) = GBUF%RK(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------
              ELSEIF(KEYWORD == 'TVIS')THEN
C--------------------------------------------------
C               VISCOSITE TURBULENTE
                DO I=1,NEL 
                   N = I + NFT                                      
                   IF((MLW == 6 .OR. MLW == 17).AND.JTURB/=0)THEN 
                     MT=IXS(1,N)                                    
                     VALUE(I) = PM(81,MT) * GBUF%RK(I)**2              
     .                                 / MAX(EM15,GBUF%RE(I))  
                     IS_WRITTEN_VALUE(I) = 1
                   ELSEIF(MLW == 46 .OR. MLW == 47)THEN            
                      VALUE(I) = MBUF%VAR(I)    
                      IS_WRITTEN_VALUE(I) = 1                   
                   ENDIF 
                ENDDO  
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VORTX')THEN
C--------------------------------------------------
C               VORTICITE-X
                IF(MLW /= 151)THEN
                  DO I=1,NEL    
                    VALUE(I) = FANI_CELL(I+NFT)  
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDDO
                ELSEIF(MLW == 151)THEN
                  !ITY = IPARG(5, NG)
                  NB_FACE = 6
                  DO I=1,NEL
                    II = I + NFT
                    IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
                    NB_FACE = ALE_CONNECT%ee_connect%iad_connect(II+1)-IAD2
                    CUMUL(1:3)=ZERO
                    DO KFACE = 1, NB_FACE
                      IV = ALE_CONNECT%ee_connect%connected(IAD2 + KFACE - 1)
                      NX = ZERO !MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, II)  
                      NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, II)        
                      NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, II)        
                      SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, II)           
                      VX = ZERO !MULTI_FVM%VEL(1, II)                      
                      VY = MULTI_FVM%VEL(2, II)                            
                      VZ = MULTI_FVM%VEL(3, II)                            
                      IF(IV /=0)THEN
                        VX = ZERO ! HALF(VX + MULTI_FVM%VEL(1, IV))      
                        VY = HALF*(VY + MULTI_FVM%VEL(2, IV))            
                        VZ = HALF*(VZ + MULTI_FVM%VEL(3, IV))            
                      ENDIF         
                      CUMUL(1)=CUMUL(1)+SURF*(NY*VZ-NZ*VY)                 
                      !CUMUL(2)=CUMUL(2)+NZ*VX-NX*VZ                       
                      !CUMUL(3)=CUMUL(3)+NX*VY-NY*VX                       
                    ENDDO
                    CUMUL(1)=CUMUL(1)/GBUF%VOL(I)                   
                    VALUE(I) = CUMUL(1)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO  
                ENDIF                              
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VORTY')THEN
C--------------------------------------------------
C               VORTICITE-Y
                IF(MLW /= 151)THEN
                  IDX = MIN(1,ANIM_SE(10)+H3D_DATA%SOL_SCAL_VORTX)*NUMELS 
                  DO I=1,NEL    
                    VALUE(I) = FANI_CELL(IDX+I+NFT)  
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDDO
                ELSEIF(MLW == 151)THEN
                  !ITY = IPARG(5, NG)
                  NB_FACE = 6
                  
                  DO I=1,NEL
                    II = I + NFT
                    IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
                    NB_FACE = ALE_CONNECT%ee_connect%iad_connect(II+1)-IAD2
                    CUMUL(1:3)=ZERO
                    DO KFACE = 1, NB_FACE
                      IV = ALE_CONNECT%ee_connect%connected(IAD2 + KFACE - 1)
                      NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, II)                           
                      NY = ZERO !MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, II)                     
                      NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, II)                           
                      SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, II)                              
                      VX = MULTI_FVM%VEL(1, II)        
                      VY = ZERO !MULTI_FVM%VEL(2, II)  
                      VZ = MULTI_FVM%VEL(3, II)        
                      IF(IV /=0)THEN                   
                        VX = HALF*(VX + MULTI_FVM%VEL(1, IV))                               
                        VY = ZERO !HALF(VY + MULTI_FVM%VEL(2, IV))                          
                        VZ = HALF*(VZ + MULTI_FVM%VEL(3, IV))                               
                      ENDIF                            
                      !CUMUL(1)=CUMUL(1)+NY*VZ-NZ*VY   
                      CUMUL(2)=CUMUL(2)+SURF*(NZ*VX-NX*VZ)                                    
                      !CUMUL(3)=CUMUL(3)+NX*VY-NY*VX   
                    ENDDO
                    CUMUL(2)=CUMUL(2)/GBUF%VOL(I)                   
                    VALUE(I) = CUMUL(2)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO  
                ENDIF                   
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VORTZ')THEN
C--------------------------------------------------
C               VORTICITE-Z
                IF(MLW /= 151)THEN
                  IDX = (MIN(1,ANIM_SE(10)+H3D_DATA%SOL_SCAL_VORTX)+ MIN(1,ANIM_SE(4960)+H3D_DATA%SOL_SCAL_VORTY) )*NUMELS 
                  DO I=1,NEL    
                    VALUE(I) = FANI_CELL(IDX+I+NFT)  
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDDO
                ELSEIF(MLW == 151)THEN
                  !ITY = IPARG(5, NG)
                  NB_FACE = 6
                  DO I=1,NEL
                    II = I + NFT
                    IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
                    NB_FACE = ALE_CONNECT%ee_connect%iad_connect(II+1)-IAD2
                    CUMUL(1:3)=ZERO
                    DO KFACE = 1, NB_FACE
                      IV = ALE_CONNECT%ee_connect%connected(IAD2 + KFACE - 1)               
                      NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, II)               
                      NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, II)               
                      NZ = ZERO !MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, II)         
                      SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, II)                  
                      VX = MULTI_FVM%VEL(1, II)                                   
                      VY = MULTI_FVM%VEL(2, II)                                   
                      VZ = ZERO !MULTI_FVM%VEL(3, II)                             
                      IF(IV /=0)THEN       
                        VX = HALF*(VX + MULTI_FVM%VEL(1, IV))                   
                        VY = HALF*(VY + MULTI_FVM%VEL(2, IV))                   
                        VZ = ZERO !HALF(VZ + MULTI_FVM%VEL(3, IV))              
                      ENDIF                
                      !CUMUL(1)=CUMUL(1)+NY*VZ-NZ*VY                              
                      !CUMUL(2)=CUMUL(2)+NZ*VX-NX*VZ                              
                      CUMUL(3)=CUMUL(3)+SURF*(NX*VY-NY*VX)                        
                    ENDDO
                    CUMUL(3)=CUMUL(3)/GBUF%VOL(I)                   
                    VALUE(I) = CUMUL(3)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO  
                ENDIF                    
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VORT')THEN
C--------------------------------------------------
C               VORTICITE
                DO I=1,NEL
                  IF(MLW == 6 .OR. MLW == 17)THEN      
                    VALUE(I) = LBUF%VK(I)   
                    IS_WRITTEN_VALUE(I) = 1              
                  ELSEIF(MLW == 46 .OR. MLW == 47)THEN 
                     VALUE(I) = MBUF%VAR(NEL+I)      ! UVAR(I,2)  
                     IS_WRITTEN_VALUE(I) = 1
                  ENDIF                       
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DAM1' .AND.MLW == 24)THEN
C--------------------------------------------------
C                dam 1
                 DO I=1,NEL
                   VALUE(I) = LBUF%DAM(JJ(1) + I)
                   IS_WRITTEN_VALUE(I) = 1 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DAM2' .AND.MLW == 24)THEN
C--------------------------------------------------
C                dam 2
                 DO I=1,NEL
                   VALUE(I) = LBUF%DAM(JJ(2) + I)
                   IS_WRITTEN_VALUE(I) = 1 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DAM3' .AND.MLW == 24)THEN
C--------------------------------------------------
C                dam 3
                 DO I=1,NEL
                   VALUE(I) = LBUF%DAM(JJ(3) + I)
                   IS_WRITTEN_VALUE(I) = 1 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGX')THEN
C--------------------------------------------------
                 DO I=1,NEL
                    VALUE(I) = GBUF%SIG(JJ(1) + I)
                    IS_WRITTEN_VALUE(I) = 1 
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IF(IVISC > 0) THEN
                      VALUE(I) = VALUE(I) + LBUF%VISC(JJ(1)+I)
                    ENDIF 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGY')THEN
C--------------------------------------------------
                 DO I=1,NEL
                    VALUE(I) = GBUF%SIG(JJ(2) + I)
                    IS_WRITTEN_VALUE(I) = 1 
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IF(IVISC > 0) THEN
                      VALUE(I) = VALUE(I) + LBUF%VISC(JJ(2)+I)
                    ENDIF 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGZ')THEN
C--------------------------------------------------
                 DO I=1,NEL
                    VALUE(I) = GBUF%SIG(JJ(3) + I)
                    IS_WRITTEN_VALUE(I) = 1 
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IF(IVISC > 0) THEN
                      VALUE(I) = VALUE(I) + LBUF%VISC(JJ(3)+I)
                    ENDIF 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGXY')THEN
C--------------------------------------------------
                 DO I=1,NEL
                    VALUE(I) = GBUF%SIG(JJ(4) + I)
                    IS_WRITTEN_VALUE(I) = 1 
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IF(IVISC > 0) THEN
                      VALUE(I) = VALUE(I) + LBUF%VISC(JJ(4)+I)
                    ENDIF 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGYZ')THEN
C--------------------------------------------------
                 DO I=1,NEL
                    VALUE(I) = GBUF%SIG(JJ(5) + I)
                    IS_WRITTEN_VALUE(I) = 1 
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IF(IVISC > 0) THEN
                      VALUE(I) = VALUE(I) + LBUF%VISC(JJ(5)+I)
                    ENDIF 
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIZX')THEN
C--------------------------------------------------
                 DO I=1,NEL
                    VALUE(I) = GBUF%SIG(JJ(6) + I)
                    IS_WRITTEN_VALUE(I) = 1 
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE(I) = VALUE(I) * GBUF%FILL(I) 
                    IF(IVISC > 0) THEN
                      VALUE(I) = VALUE(I) + LBUF%VISC(JJ(6)+I)
                    ENDIF 
                 ENDDO
C--------------------------------------------------
              ELSEIF((KEYWORD == 'USER' .AND. MLW>=28 .AND. MLW/=51) .OR. KEYWORD == 'MDS') THEN  
C--------------------------------------------------    
c UVAR=IUVAR
                IMAT = IXS(1,NFT+1)
                IF( (KEYWORD == 'MDS' .AND. IMAT == IDMDS) .OR. KEYWORD == 'USER' )THEN
                  IF ( IUVAR > 0) THEN                                  
                    IF (ISOLNOD == 8 .AND. MLW == 59) THEN                      
c                     output = global damage variables of /fail/connect         
                      MT = IXS(1,NFT+1)                                         
                      IRUPT = IPM(111,MT)                                       
                      NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL                      
                      IF (IRUPT == 20) THEN                                     
                        NPTG = 4                                                
                        DO IR=1,NFAIL                                           
                          DO IPT = 1,NPTG                                       
                           UVARF =>                                             
     .                     ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%VAR    
                           DO I=1,NEL                                         
                             VALUE(I) = MAX(VALUE(I),UVARF((IUVAR-1)*NEL + I))  
                             IS_WRITTEN_VALUE(I) = 1      
                           ENDDO                                                
                          ENDDO                                                 
                        ENDDO                                                   
                      ENDIF                                                     
                    ELSE                                                        
                      DO IL=1,NLAY                                              
                        DO IS=1,NPTS                                            
                          DO IT=1,NPTT                                          
                            DO IR=1,NPTR                                        
                              MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)     
                              DO I=1,NEL                                     
                                 N = I + NFT                                    
                                 MT=IXS(1,N)                                    
                                 NUVAR = IPM(8,MT)                              
                                 IF (IUVAR <= NUVAR) THEN
                                   VALUE(I) = VALUE(I)            
     .                                          + MBUF%VAR(I+(IUVAR-1)*NEL)/NPTG   
                                   IS_WRITTEN_VALUE(I) = 1 
                                 ENDIF   
                              ENDDO                                             
                            ENDDO                                               
                          ENDDO                                                 
                        ENDDO                                                   
                      ENDDO                                                     
                    ENDIF  
                  ENDIF  
                ENDIF
C--------------------------------------------------
              ELSEIF(KEYWORD == 'HOURGLASS')THEN
C--------------------------------------------------
                 DO I=1,NEL
                   VALUE(I) = EHOUR(NFT+I)
                   IS_WRITTEN_VALUE(I) = 1   
                 ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'EPSD') THEN
C--------------------------------------------------
                 IF (GBUF%G_EPSD > 0) THEN
                   DO I=1,NEL
                     VALUE(I) = GBUF%EPSD(I)
                     IS_WRITTEN_VALUE(I) = 1 
                   ENDDO
                 ENDIF
C--------------------------------------------------
              ELSEIF(KEYWORD == 'WPLA' .AND. MLW == 25) THEN
C--------------------------------------------------
C               WPLA par couche pour loi 25
                IOK = 0
                DO I=1,NEL
                  EVAR(I) = ZERO 
                ENDDO
                IUS = INFO1
                IF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .             (ISOLNOD == 8.AND.JHBE == 14).OR.
     .             ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15))THEN

		  IF (IUS <= NPTG) THEN                            
                    DO IL=1,NLAY   
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                        IOK = 1
                        DO IS=1,NPTS 
                         DO IT=1,NPTT                                      
                          DO IR=1,NPTR                                    
                            LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            DO I=1,NEL                                
                              VALUE(I) = VALUE(I) + LBUF%PLA(I) 
                              IS_WRITTEN_VALUE(I) = 1                
                            ENDDO
                           ENDDO    
                          ENDDO      
                        ENDDO        
                      ENDIF 
                    ENDDO          
                 ENDIF       
                ENDIF 
C--------------------------------------------------
              ELSEIF (KEYWORD == 'FLAY' .AND. MLW == 25) THEN
C--------------------------------------------------
C---            FAILED LAYERS par elem pour loi 25
                DO I=1,NEL
                  EVAR(I) = ZERO
                ENDDO
                IF( ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .             (ISOLNOD == 8.AND.JHBE == 14).OR.
     .            ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15))THEN
c
                  NPG_PLANE = NPTR * NPTS * NPTT
                  DO I=1,NEL
                    DO IL=1,NLAY
                      VALUE(I)   = ZERO
                      DO J=1,NPTR
                   	DO K=1,NPTS
                   	 DO L=1,NPTT
                   	   LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(J,K,L)
                           IF (LBUF%OFF(I) == 0) VALUE(I) = VALUE(I) + ONE
                   	   IF(INT(VALUE(I))>=NPG_PLANE) EVAR(I)=EVAR(I)+ONE
                           IS_WRITTEN_VALUE(I) = 1           
                   	 ENDDO
                   	ENDDO
                      ENDDO
                    ENDDO
                  ENDDO  
                ENDIF  
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VFRAC1') THEN
C--------------------------------------------------
               IF(MLW==37)THEN
                 IUS=3 !law37 user4 and user5                  
               ELSEIF(MLW==51)THEN
                 IMAT = IXS(1,NFT+1)                        
                 IADBUF = IPM(7,IMAT)                       
                 NUPARAM= IPM(9,IMAT)                       
                 UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                 ISUBMAT=UPARAM(276+1)
                 IUS=N0PHAS+(ISUBMAT-1)*NVPHAS
               ENDIF       
               IF (MLW==51 .OR. MLW==37)THEN                                
                   DO IL=1,NLAY       
                     DO IS=1,NPTS     
                       DO IT=1,NPTT   
                         DO IR=1,NPTR 
                           MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                 DO I=1,NEL 
                           VALUE(I) = VALUE(I) + MBUF%VAR(I+(IUS)*NEL)/NPTG 
                           IS_WRITTEN_VALUE(I) = 1
                 ENDDO	      
                         ENDDO        
                       ENDDO          
                     ENDDO            
                   ENDDO                
               ENDIF 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VFRAC2') THEN
C--------------------------------------------------
               IF(MLW==37)THEN
                 IUS=4 !law37 user4 and user5                  
               ELSEIF(MLW==51)THEN
                 IMAT = IXS(1,NFT+1)                        
                 IADBUF = IPM(7,IMAT)                       
                 NUPARAM= IPM(9,IMAT)                       
                 UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                 ISUBMAT=UPARAM(276+2)
                 IUS=N0PHAS+(ISUBMAT-1)*NVPHAS
               ENDIF       
               IF (MLW==51 .OR. MLW==37)THEN                                
                   DO IL=1,NLAY       
                     DO IS=1,NPTS     
                       DO IT=1,NPTT   
                         DO IR=1,NPTR 
                           MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                 DO I=1,NEL 
                           VALUE(I) = VALUE(I) + MBUF%VAR(I+(IUS)*NEL)/NPTG 
                           IS_WRITTEN_VALUE(I) = 1	 
                 ENDDO     
                         ENDDO        
                       ENDDO          
                     ENDDO            
                   ENDDO              
               ENDIF 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VFRAC3') THEN
C--------------------------------------------------
               IF(MLW==37)THEN
                 IUS=5 !law37 user4 and user5                  
               ELSEIF(MLW==51)THEN
                 IMAT = IXS(1,NFT+1)                        
                 IADBUF = IPM(7,IMAT)                       
                 NUPARAM= IPM(9,IMAT)                       
                 UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                 ISUBMAT=UPARAM(276+3)
                 IUS=N0PHAS+(ISUBMAT-1)*NVPHAS
               ENDIF       
               IF (MLW==51)THEN                               
                   DO IL=1,NLAY       
                     DO IS=1,NPTS     
                       DO IT=1,NPTT   
                         DO IR=1,NPTR 
                           MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                 DO I=1,NEL  
                           VALUE(I) = VALUE(I) + MBUF%VAR(I+(IUS)*NEL)/NPTG 
                           IS_WRITTEN_VALUE(I) = 1	  
                 ENDDO    
                         ENDDO        
                       ENDDO          
                     ENDDO            
                   ENDDO               
               ENDIF 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VFRAC4') THEN
C--------------------------------------------------
               IF(MLW==37)THEN
                 IUS=6 !law37 user4 and user5                  
               ELSEIF(MLW==51)THEN
                 IMAT = IXS(1,NFT+1)                        
                 IADBUF = IPM(7,IMAT)                       
                 NUPARAM= IPM(9,IMAT)                       
                 UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                 ISUBMAT=UPARAM(276+4)
                 IUS=N0PHAS+(ISUBMAT-1)*NVPHAS 
               ENDIF       
               IF (MLW==51)THEN                                
                   DO IL=1,NLAY       
                     DO IS=1,NPTS     
                       DO IT=1,NPTT   
                         DO IR=1,NPTR 
                           MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                 DO I=1,NEL 
                           VALUE(I) = VALUE(I) + MBUF%VAR(I+(IUS)*NEL)/NPTG 
                           IS_WRITTEN_VALUE(I) = 1	      
                 ENDDO
                         ENDDO        
                       ENDDO          
                     ENDDO            
                   ENDDO               
               ENDIF 
C--------------------------------------------------
              ELSEIF(KEYWORD(1:9) == 'M151VFRAC') THEN
C--------------------------------------------------
                 IF (MLW == 151 .AND. MULTI_FVM%NBMAT > 1) THEN 
                    READ(KEYWORD, '(A9,I10)') BUFF, IMAT
                    IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                       DO I=1,NEL
                          VALUE(I) = MULTI_FVM%PHASE_ALPHA(IMAT, I + NFT)
                          IS_WRITTEN_VALUE(I) = 1           
                       ENDDO
                    ENDIF
                 ENDIF    
C--------------------------------------------------
              ELSEIF(KEYWORD(1:8) == 'M151ENER') THEN
C--------------------------------------------------
                 IF (MLW == 151 .AND. MULTI_FVM%NBMAT > 1) THEN 
                    READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                    IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                       DO I=1,NEL
                          IF (MULTI_FVM%PHASE_RHO(IMAT, I + NFT) > 0) THEN
                             VALUE(I) = MULTI_FVM%PHASE_EINT(IMAT, I + NFT) / 
     .                            MULTI_FVM%PHASE_RHO(IMAT, I + NFT)
                          ELSE
                             VALUE(I) = ZERO
                          ENDIF
                          IS_WRITTEN_VALUE(I) = 1           
                       ENDDO
                    ENDIF
                 ENDIF    
C--------------------------------------------------
              ELSEIF(KEYWORD(1:8) == 'M151PRES') THEN
C--------------------------------------------------
                 IF (MLW == 151 .AND. MULTI_FVM%NBMAT > 1) THEN 
                    READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                    IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                       DO I=1,NEL
                          VALUE(I) = MULTI_FVM%PHASE_PRES(IMAT, I + NFT)
                          IS_WRITTEN_VALUE(I) = 1           
                       ENDDO
                    ENDIF
                 ENDIF  
C--------------------------------------------------
              ELSEIF(KEYWORD(1:8) == 'M151DENS') THEN
C--------------------------------------------------
                 IF (MLW == 151 .AND. MULTI_FVM%NBMAT > 1) THEN 
                    READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                    IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                       DO I=1,NEL
                          VALUE(I) = MULTI_FVM%PHASE_RHO(IMAT, I + NFT)
                          IS_WRITTEN_VALUE(I) = 1           
                       ENDDO
                    ENDIF
                 ENDIF  
C--------------------------------------------------
              ELSEIF (KEYWORD == 'ORTHD/PSI')THEN
C--------------------------------------------------
C           EULER ANGLE : rotation of ORTHOTROPIC SYSTEM wrt GLOBAL SYSTEM
c ILAYER=NULL NPT=NULL
	        IF ( (ILAY <= NLAY .AND. ILAY > 0) .OR. 
     .                IR <= NPTR .AND. IR > 0 .AND.  IS <= NPTS .AND. IS > 0 .AND. IT <= NPTT .AND. IT > 0) THEN 
	          IF ( IGTYP == 6 .OR. IGTYP == 21 .OR. IGTYP == 22 ) THEN 
                    LBUF => ELBUF_TAB(NG)%BUFLY(MAX(1,ILAY))%LBUF(1,1,1)
                    DO I=1,NEL
                      N = I + NFT
                      IF(ISORTH ==1) THEN
C                      pour JHBE=14, valeurs moyennes est dans rep. corota.
                   	IF(IGTYP == 21 .OR. IGTYP == 22) THEN	 
                   	  GAMA(1)= LBUF%GAMA(JJ(1)+I)
                   	  GAMA(2)= LBUF%GAMA(JJ(2)+I)
                   	  GAMA(3)= ZERO
                   	  GAMA(4)= ZERO
                   	  GAMA(5)= ZERO
                   	  GAMA(6)= ZERO
                   	ELSE
                   	  GAMA(1) = GBUF%GAMA(JJ(1)+I)
                   	  GAMA(2) = GBUF%GAMA(JJ(2)+I)
                   	  GAMA(3) = GBUF%GAMA(JJ(3)+I)
                   	  GAMA(4) = GBUF%GAMA(JJ(4)+I)
                   	  GAMA(5) = GBUF%GAMA(JJ(5)+I)
                   	  GAMA(6) = GBUF%GAMA(JJ(6)+I)
                   	ENDIF
                   	CALL SROTORTH(X,IXS(1,N),
     .             		    GAMA,JHBE,IGTYP,IPARG(17,NG) )
C--------
                   	T11=GAMA(1)
                   	T21=GAMA(2)
                   	T31=GAMA(3)
                   	T12=GAMA(4)
                   	T22=GAMA(5)
                   	T32=GAMA(6)
                   	T13=T21*T32-T31*T22
                   	T23=T31*T12-T11*T32
                   	T33=T11*T22-T21*T12
                   	IF (ABS(T31) - ONE  < EM20)THEN
                   	  PSI  = ATAN2(T32/COS(THETA),T33/COS(THETA))
                   	ELSE
                   	  IF(T31 == -ONE)THEN
                   	    PSI = ATAN2(T12,T13)
                   	  ELSE
                   	    PSI = ATAN2(-T12,-T13)
                   	  ENDIF
                   	ENDIF
                   	VALUE(I) = PSI*HUNDRED80/PI
                        IS_WRITTEN_VALUE(I) = 1 
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF     
C--------------------------------------------------
              ELSEIF (KEYWORD == 'ORTHD/THETA')THEN
C--------------------------------------------------
C           EULER ANGLE : rotation of ORTHOTROPIC SYSTEM wrt GLOBAL SYSTEM
c ILAYER=NULL NPT=NULL
	        IF ( (ILAY <= NLAY .AND. ILAY > 0) .OR. 
     .                IR <= NPTR .AND. IR > 0 .AND.  IS <= NPTS .AND. IS > 0 .AND. IT <= NPTT .AND. IT > 0) THEN 
	          IF ( IGTYP == 6 .OR. IGTYP == 21 .OR. IGTYP == 22 ) THEN 
                    LBUF => ELBUF_TAB(NG)%BUFLY(MAX(1,ILAY))%LBUF(1,1,1)
                    DO I=1,NEL
                      N = I + NFT
                      IF(ISORTH ==1) THEN
C                      pour JHBE=14, valeurs moyennes est dans rep. corota.
                   	IF(IGTYP == 21 .OR. IGTYP == 22) THEN	 
                   	  GAMA(1)= LBUF%GAMA(JJ(1)+I)
                   	  GAMA(2)= LBUF%GAMA(JJ(2)+I)
                   	  GAMA(3)= ZERO
                   	  GAMA(4)= ZERO
                   	  GAMA(5)= ZERO
                   	  GAMA(6)= ZERO
                   	ELSE
                   	  GAMA(1) = GBUF%GAMA(JJ(1)+I)
                   	  GAMA(2) = GBUF%GAMA(JJ(2)+I)
                   	  GAMA(3) = GBUF%GAMA(JJ(3)+I)
                   	  GAMA(4) = GBUF%GAMA(JJ(4)+I)
                   	  GAMA(5) = GBUF%GAMA(JJ(5)+I)
                   	  GAMA(6) = GBUF%GAMA(JJ(6)+I)
                   	ENDIF
                   	CALL SROTORTH(X,IXS(1,N),
     .             		    GAMA,JHBE,IGTYP,IPARG(17,NG) )
C--------
                   	T11=GAMA(1)
                   	T21=GAMA(2)
                   	T31=GAMA(3)
                   	T12=GAMA(4)
                   	T22=GAMA(5)
                   	T32=GAMA(6)
                   	T13=T21*T32-T31*T22
                   	T23=T31*T12-T11*T32
                   	T33=T11*T22-T21*T12
                   	IF (ABS(T31) - ONE  < EM20)THEN
                   	  THETA = -ASIN(T31)
                   	ELSE
                   	  IF(T31 == -ONE)THEN
                   	    THETA = PI / TWO
                   	  ELSE
                   	    THETA = - PI / TWO
                   	  ENDIF
                   	ENDIF
                   	VALUE(I) = THETA*HUNDRED80/PI
                        IS_WRITTEN_VALUE(I) = 1 
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF  
C--------------------------------------------------
              ELSEIF (KEYWORD == 'ORTHD/PHI')THEN
C--------------------------------------------------
C           EULER ANGLE : rotation of ORTHOTROPIC SYSTEM wrt GLOBAL SYSTEM
c ILAYER=NULL NPT=NULL
	        IF ( (ILAY <= NLAY .AND. ILAY > 0) .OR. 
     .                IR <= NPTR .AND. IR > 0 .AND.  IS <= NPTS .AND. IS > 0 .AND. IT <= NPTT .AND. IT > 0) THEN 
	          IF ( IGTYP == 6 .OR. IGTYP == 21 .OR. IGTYP == 22 ) THEN 
                    LBUF => ELBUF_TAB(NG)%BUFLY(MAX(1,ILAY))%LBUF(1,1,1)
                    DO I=1,NEL
                      N = I + NFT
                      IF(ISORTH ==1) THEN
C                      pour JHBE=14, valeurs moyennes est dans rep. corota.
                   	IF(IGTYP == 21 .OR. IGTYP == 22) THEN	 
                   	  GAMA(1)= LBUF%GAMA(JJ(1)+I)
                   	  GAMA(2)= LBUF%GAMA(JJ(2)+I)
                   	  GAMA(3)= ZERO
                   	  GAMA(4)= ZERO
                   	  GAMA(5)= ZERO
                   	  GAMA(6)= ZERO
                   	ELSE
                   	  GAMA(1) = GBUF%GAMA(JJ(1)+I)
                   	  GAMA(2) = GBUF%GAMA(JJ(2)+I)
                   	  GAMA(3) = GBUF%GAMA(JJ(3)+I)
                   	  GAMA(4) = GBUF%GAMA(JJ(4)+I)
                   	  GAMA(5) = GBUF%GAMA(JJ(5)+I)
                   	  GAMA(6) = GBUF%GAMA(JJ(6)+I)
                   	ENDIF
                   	CALL SROTORTH(X,IXS(1,N),
     .             		    GAMA,JHBE,IGTYP,IPARG(17,NG) )
C--------
                   	T11=GAMA(1)
                   	T21=GAMA(2)
                   	T31=GAMA(3)
                   	T12=GAMA(4)
                   	T22=GAMA(5)
                   	T32=GAMA(6)
                   	T13=T21*T32-T31*T22
                   	T23=T31*T12-T11*T32
                   	T33=T11*T22-T21*T12
                   	IF (ABS(T31) - ONE  < EM20)THEN
                   	  PHI  = ATAN2(T21/COS(THETA),T11/COS(THETA))
                   	ELSE
                   	  PHI = ZERO
                   	ENDIF
                   	VALUE(I) = PHI*HUNDRED80/PI
                        IS_WRITTEN_VALUE(I) = 1 
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF          
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'BFRAC')THEN
C-------------------------------------------------- 
              !BURN FRACTION explosive EOS 
                IF(GBUF%G_BFRAC > 0) THEN
                  IF (MLW==151)THEN
                    DO I=1,NEL
                      VALUE(I)=-EP30
                    ENDDO
                    DO ILAY=1,NLAY
                      DO I=1,NEL
                        VALUE(I) =  MAX(VALUE(I),MULTI_FVM%BFRAC(ILAY,I+NFT))
                        IS_WRITTEN_VALUE(I) = 1
                      ENDDO
                    ENDDO
                  ELSE
                      VALUE(1:NEL) = GBUF%BFRAC(1:NEL) 
                      IS_WRITTEN_VALUE(1:NEL) = 1                  
                  ENDIF
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'VDAM1')THEN
C-------------------------------------------------- 
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
	        IF ( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1) THEN 
                  IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                   output = damage variables of /fail/snconnect  
                    MT = IXS(1,NFT+1)
                    IRUPT = IPM(111,MT)
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                    IF (IRUPT == 26) THEN 
                      NPTG = 4
                      DO IR=1,NFAIL
                 	DO IPT = 1,NPTG
                 	  DAMF => 
     .           	  ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%DAM
                 	  DO I=1,NEL				     
                 	    EVAR(I) = MAX(EVAR(I) ,DAMF(I))	 
                 	  ENDDO 					  
                 	ENDDO
                      ENDDO			  
                      DO I=1,NEL
                 	VALUE(I) = EVAR(I)
                 	IS_WRITTEN_VALUE(I) = 1 
                      ENDDO
                    ENDIF
                  ENDIF
c ILAYER=NULL IR= IS= IT=
                ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
                  IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                   output = damage variables of /fail/snconnect  
                    MT = IXS(1,NFT+1)
                    IRUPT = IPM(111,MT)
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                    IF (IRUPT == 26) THEN 
                      DO IIR=1,NFAIL
                      DAMF => 
     .                ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,1,1)%FLOC(IIR)%DAM
                        DO I=1,NEL				  
                 	  EVAR(I) = DAMF(I)   
                        ENDDO					       
                      ENDDO			 
                      DO I=1,NEL
                 	VALUE(I) = EVAR(I)
                 	IS_WRITTEN_VALUE(I) = 1 
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'VDAM2')THEN
C-------------------------------------------------- 
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
	        IF ( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1) THEN 
                  IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                   output = damage variables of /fail/snconnect  
                    MT = IXS(1,NFT+1)
                    IRUPT = IPM(111,MT)
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                    IF (IRUPT == 26) THEN 
                      NPTG = 4
                      DO IR=1,NFAIL
                 	DO IPT = 1,NPTG
                 	  DAMF => 
     .           	  ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%DAM
                 	  DO I=1,NEL				     
                 	    EVAR(I) = MAX(EVAR(I) ,DAMF(NEL + I))      
                 	  ENDDO 					  
                 	ENDDO
                      ENDDO			  
                      DO I=1,NEL
                 	VALUE(I) = EVAR(I)
                 	IS_WRITTEN_VALUE(I) = 1 
                      ENDDO
                    ENDIF
                  ENDIF
c ILAYER=NULL IR= IS= IT=
                ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
                  IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                   output = damage variables of /fail/snconnect  
                    MT = IXS(1,NFT+1)
                    IRUPT = IPM(111,MT)
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                    IF (IRUPT == 26) THEN 
                      DO IIR=1,NFAIL
                      DAMF => 
     .                ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,1,1)%FLOC(IIR)%DAM
                        DO I=1,NEL				  
                 	  EVAR(I) = DAMF(NEL+I)   
                        ENDDO					       
                      ENDDO			 
                      DO I=1,NEL
                 	VALUE(I) = EVAR(I)
                 	IS_WRITTEN_VALUE(I) = 1 
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'VDAM3')THEN
C-------------------------------------------------- 
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
	        IF ( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1) THEN 
                  IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                   output = damage variables of /fail/snconnect  
                    MT = IXS(1,NFT+1)
                    IRUPT = IPM(111,MT)
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                    IF (IRUPT == 26) THEN 
                      NPTG = 4
                      DO IR=1,NFAIL
                 	DO IPT = 1,NPTG
                 	  DAMF => 
     .           	  ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%DAM
                 	  DO I=1,NEL				     
                 	    EVAR(I) = MAX(EVAR(I) ,DAMF(2*NEL + I))	 
                 	  ENDDO 					  
                 	ENDDO
                      ENDDO			  
                      DO I=1,NEL
                 	VALUE(I) = EVAR(I)
                 	IS_WRITTEN_VALUE(I) = 1 
                      ENDDO
                    ENDIF
                  ENDIF
c ILAYER=NULL IR= IS= IT=
                ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN
                  IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                   output = damage variables of /fail/snconnect  
                    MT = IXS(1,NFT+1)
                    IRUPT = IPM(111,MT)
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                    IF (IRUPT == 26) THEN 
                      DO IIR=1,NFAIL
                      DAMF => 
     .                ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,1,1)%FLOC(IIR)%DAM
                        DO I=1,NEL				  
                 	  EVAR(I) = DAMF(2*NEL+I)   
                        ENDDO					       
                      ENDDO			 
                      DO I=1,NEL
                 	VALUE(I) = EVAR(I)
                 	IS_WRITTEN_VALUE(I) = 1 
                      ENDDO
                    ENDIF
                  ENDIF 
                ENDIF
C--------------------------------------------------              
              ELSEIF(KEYWORD == 'DAMA') THEN
C--------------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
	        IF ( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1) THEN 
                  DO I=1,NEL					      
                    EVAR(I) = ZERO					
                  ENDDO 

                  DO IL=1,NLAY 
                    NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL								    
                    DO IS=1,NPTS					
                      DO IT=1,NPTT					
                        DO IIR=1,NPTR	
                          DO IR=1,NFAIL 											   
                            DFMAX=>
     .                        ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%DAMMX
                            DO I=1,NEL  			       
                              VALUE(I) = MAX(VALUE(I),DFMAX(I))
                              IS_WRITTEN_VALUE(I) = 1		      
                            ENDDO
                          ENDDO 
                        ENDDO
                      ENDDO
                     ENDDO
                   ENDDO 
c ILAYER=NULL IR= IS= IT=
                ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
                  IIR = IR
                  IUS = NLAY*IIR*IS*IT
                  DAMMAX = ZERO
                  IF (IIR <= NPTR.AND.IS <= NPTS.AND.IT <= NPTT) THEN				 
                    DO IL=1,NLAY   
                       NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL			      
                       DO IR=1,NFAIL												
                  	  DFMAX=>
     .            	    ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%DAMMX
                  	  DO I=1,NEL				       
                  	     VALUE(I) = MAX(VALUE(I),DFMAX(I))
                  	     IS_WRITTEN_VALUE(I) = 1				
                  	  ENDDO
                  	ENDDO						
                      ENDDO						
                  ENDIF 						
                ELSEIF ( ILAY > 0 .AND. ILAY <= NLAY .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS) THEN
                      NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL			      
                      DO IIR=1,NFAIL												
                  	    DFMAX=>
     .            	    ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)%FLOC(IIR)%DAMMX
                  	    DO I=1,NEL				       
                  	      VALUE(I) = MAX(VALUE(I),DFMAX(I))
                  	      IS_WRITTEN_VALUE(I) = 1				
                  	    ENDDO
                  	  ENDDO						
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'DAMG') THEN
C--------------------------------------------------
c
                IF (GBUF%G_DMG > 0) THEN
c
                  ! If nothing is specified by the user, computing a mean value
                  IF (IR == -1 .AND. IS == -1 .AND. IT == -1) THEN
c
                    ! Resetting values
                    DO I=1,NEL                                          
                      VALUE(I) = ZERO                                      
                    ENDDO 
c
                    ! Filling the value table
                    DO I=1,NEL 
                      DO IS=1,NPTS                                        
                        DO IT=1,NPTT                                      
                          DO IIR=1,NPTR 
                            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IIR,IS,IT)                       
                            VALUE(I) = VALUE(I) + LBUF%DMG(I)/(NPTS*NPTT*NPTR) 
                          ENDDO
                        ENDDO
                      ENDDO
                      IS_WRITTEN_VALUE(I) = 1   
                    ENDDO
c
                  ! If something is specified by the user
                  ELSEIF ( IR >= 0 .AND. IR <= NPTR .AND. 
     .                     IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
c
                    ! Corresponding buffer
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT) 
c
                    ! Filling the value table
                    IF (IR <= NPTR.AND.IS <= NPTS.AND.IT <= NPTT) THEN      
                      DO I=1,NEL                       
                        VALUE(I) = LBUF%DMG(I)
                        IS_WRITTEN_VALUE(I) = 1                            
                      ENDDO
                    ENDIF                                                 
                  ENDIF 
                ENDIF  
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'DAMINI') THEN
C--------------------------------------------------
                ! Resetting values
                DO I=1,NEL                                          
                  VALUE(I) = ZERO                                      
                ENDDO 
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
                IF ( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1) THEN 
                  DO IL=1,NLAY 
                    NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
                    DO IS=1,NPTS
                      DO IT=1,NPTT
                        DO IIR=1,NPTR
                          FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)
                          DO I=1,NEL
                            MAXDAMINI = ZERO
                            DO IR=1,NFAIL 
                              IF (FBUF%FLOC(IR)%LF_DAMINI > 0) THEN  
                                MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IR)%DAMINI(I))
                              ENDIF
                            ENDDO
                            VALUE(I) = VALUE(I) + MAXDAMINI/(NPTS*NPTR*NPTT*NLAY)
                            IS_WRITTEN_VALUE(I) = 1
                          ENDDO 
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO 
c ILAYER=NULL IR= IS= IT=
                ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
                  IIR = IR
                  IF (IIR <= NPTR .AND. IS <= NPTS .AND. IT <= NPTT) THEN
                    DO IL=1,NLAY   
                      NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)
                      DO I=1,NEL     
                        MAXDAMINI = ZERO
                        DO IR=1,NFAIL
                          IF (FBUF%FLOC(IR)%LF_DAMINI > 0) THEN 
                            MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IR)%DAMINI(I))
                          ENDIF
                        ENDDO
                        VALUE(I) = VALUE(I) + MAXDAMINI/NLAY
                        IS_WRITTEN_VALUE(I) = 1
                      ENDDO
                    ENDDO
                  ENDIF 
c ILAYER= IR= IS= IT=
                ELSEIF ( ILAY > 0 .AND. ILAY <= NLAY .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS) THEN
                  NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL
                  FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)
                  DO I=1,NEL
                    MAXDAMINI = ZERO
                    DO IIR=1,NFAIL
                      IF (FBUF%FLOC(IIR)%LF_DAMINI > 0) THEN
                        MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IIR)%DAMINI(I))
                      ENDIF
                    ENDDO
                    VALUE(I) = MAXDAMINI
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF   
C--------------------------------------------------                
              ELSEIF(KEYWORD == 'TDEL') THEN
C-------------------------------------------------- 
                DO IL=1,NLAY 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                            
                  DO IS=1,NPTS 
                    DO IT=1,NPTT                                      
                      DO IIR=1,NPTR   
                        DO IR=1,NFAIL                 
                          TDELE=>
     .                      ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%TDEL
                          DO I=1,NEL                                 
                            VALUE(I) = MAX(VALUE(I),TDELE(I))  
                            IS_WRITTEN_VALUE(I) = 1		
                          ENDDO
                        ENDDO 
                      ENDDO
                    ENDDO
                   ENDDO
                 ENDDO 
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'SSP') THEN  
C-------------------------------------------------- 
                 IF (MLW == 151) THEN 
                    DO I=1,NEL
                       VALUE(I) = MULTI_FVM%SOUND_SPEED(I + NFT)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ELSE
                    L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP              
                    IF(L /= 0)THEN
                       DO I=1,NEL
                          VALUE(I) = LBUF%SSP(I) 
                          IS_WRITTEN_VALUE(I) = 1	
                       ENDDO
                    ENDIF 
                 ENDIF
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'VOLU') THEN  
C--------------------------------------------------    
                IF (GBUF%G_VOL > 0) THEN
                  IALEL=IPARG(7,NG)+IPARG(11,NG)
                  IF(IALEL==0)THEN
                       MT = IXS(1,NFT+1)
                       DO I=1,NEL
                	 VALUE(I) = PM(1,MT)*GBUF%VOL(I)
                	 IF(GBUF%RHO(I)>ZERO)VALUE(I) = VALUE(I)/GBUF%RHO(I)
                         IS_WRITTEN_VALUE(I) = 1	
                       ENDDO  
                  ELSE
                    DO I=1,NEL
                      VALUE(I) = GBUF%VOL(I)
                      IS_WRITTEN_VALUE(I) = 1
                    ENDDO
                  ENDIF
                ENDIF		    
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'SCHLIEREN') THEN  
C-------------------------------------------------- 
                 IALEL=IPARG(7,NG)+IPARG(11,NG)
                 IF(IALEL /= 0)THEN
                   CALL SCHLIEREN(
     1                            EVAR  ,IXS  ,X         ,V           ,W       ,
     2                            IPARG ,WA_L ,ELBUF_TAB ,ALE_CONNECT ,GBUF%VOL,
     3                            PM    ,NG   ,NIXS      ,ITY)
                    DO I=1,NEL
                      VALUE(I) = EVAR(I) 
                      IS_WRITTEN_VALUE(I) = 1							  
                    ENDDO
                 ENDIF 
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'DOMAIN') THEN
C--------------------------------------------------   
                  DO I=1,NEL
                    VALUE(I) = ISPMD
                    IS_WRITTEN_VALUE(I) = 1	
                  ENDDO
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'FILL') THEN 
C--------------------------------------------------  
                  DO I=1,NEL
                    VALUE(I) = GBUF%FILL(I)
                    IS_WRITTEN_VALUE(I) = 1	
                  ENDDO
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'SIGEQ') THEN  !  /ANIM/ELEM/SIGEQ
C-------------------------------------------------- 
            !  equivalent stress - other then VON MISES
                IF (GBUF%G_SEQ > 0) THEN  !  non VON MISES

                    NPTG = NLAY*NPTR*NPTS*NPTT
                    DO IL=1,NLAY
                      DO IT=1,NPTT
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                            IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0) THEN
                  DO I=1,NEL
                              VALUE(I) = VALUE(I) + LBUF%SEQ(I)/NPTG
                              IS_WRITTEN_VALUE(I) = 1	
                  ENDDO
                            ELSE
                  DO I=1,NEL
                              S11 = LBUF%SIG(JJ(1) + I)
                              S22 = LBUF%SIG(JJ(2) + I)
                              S33 = LBUF%SIG(JJ(3) + I)
                              S4  = LBUF%SIG(JJ(4) + I) 
                              S5  = LBUF%SIG(JJ(5) + I) 
                              S6  = LBUF%SIG(JJ(6) + I) 
                              IF (IVISC > 0) THEN
                                S11 = S11 + LBUF%VISC(JJ(1) + I)
                                S22 = S22 + LBUF%VISC(JJ(2) + I)
                                S33 = S33 + LBUF%VISC(JJ(3) + I)
                                S4  = S4  + LBUF%VISC(JJ(4) + I)
                                S5  = S5  + LBUF%VISC(JJ(5) + I)
                                S6  = S6  + LBUF%VISC(JJ(6) + I)
                              ENDIF                                 
                              P = - (S11 + S22 + S33) * THIRD
                              S1 = S11 + P
                              S2 = S22 + P
                              S3 = S33 + P
                              VONM2 =  THREE*(S4*S4 + S5*S5 + S6*S6 +
     .                                HALF*(S1*S1 + S2*S2 + S3*S3))
                              VONM = SQRT(VONM2)
                              VALUE(I) = VALUE(I) + VONM/NPTG
                              IS_WRITTEN_VALUE(I) = 1	
                  ENDDO
                            ENDIF ! IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0)
                          ENDDO ! DO IS=1,NPTS
                        ENDDO ! DO IR=1,NPTR
                      ENDDO ! DO IT=1,NPTT
                    ENDDO ! DO IL=1,NLAY
                ELSE                      ! VON MISES
                  DO I=1,NEL
                    S11 = GBUF%SIG(JJ(1) + I)
                    S22 = GBUF%SIG(JJ(2) + I)
                    S33 = GBUF%SIG(JJ(3) + I)
                    S4  = GBUF%SIG(JJ(4) + I) 
                    S5  = GBUF%SIG(JJ(5) + I) 
                    S6  = GBUF%SIG(JJ(6) + I) 
                    IF (IVISC > 0) THEN
                      S11 = S11 + LBUF%VISC(JJ(1) + I)
                      S22 = S22 + LBUF%VISC(JJ(2) + I)
                      S33 = S33 + LBUF%VISC(JJ(3) + I)
                      S4  = S4  + LBUF%VISC(JJ(4) + I)
                      S5  = S5  + LBUF%VISC(JJ(5) + I)
                      S6  = S6  + LBUF%VISC(JJ(6) + I)
                    ENDIF                                 
                    P = - (S11 + S22 + S33) * THIRD
                    S1 = S11 + P
                    S2 = S22 + P
                    S3 = S33 + P
                    VONM2 = THREE*(S4*S4 + S5*S5 + S6*S6 +
     .                     HALF*(S1*S1 + S2*S2 + S3*S3))
                    VONM = SQRT(VONM2)
                    VALUE(I) = VONM
                    IS_WRITTEN_VALUE(I) = 1	
                  ENDDO ! DO I=1,NEL
                ENDIF ! IF (GBUF%G_SEQ > 0)
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'NL_EPSP') THEN  
C--------------------------------------------------  
                IF (GBUF%G_PLANL > 0) THEN
                  NPTG = NLAY*NPTR*NPTS*NPTT
                  VALUE(1:NEL) = ZERO
                  DO IT=1,NPTT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)        
                        DO I=1,NEL
                          VALUE(I) = VALUE(I) + LBUF%PLANL(I)/NPTG
                          IS_WRITTEN_VALUE(I) = 1	
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'NL_EPSD') THEN  
C--------------------------------------------------  
                IF (GBUF%G_PLANL > 0) THEN
                  NPTG = NLAY*NPTR*NPTS*NPTT
                  VALUE(1:NEL) = ZERO
                  DO IT=1,NPTT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)        
                        DO I=1,NEL
                          VALUE(I) = VALUE(I) + LBUF%EPSDNL(I)/NPTG
                          IS_WRITTEN_VALUE(I) = 1	
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'BULK') THEN  !  /ANIM/ELEM/QVIS
C-------------------------------------------------- 
                 IF (GBUF%G_QVIS > 0) THEN
                   DO I=1,NEL
                     VALUE(I) = GBUF%QVIS(I)
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO
                 ENDIF 
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'TDET') THEN  !  /ANIM/ELEM/TDET
C-------------------------------------------------- 
                 IF (MLW  /= 51 .AND. GBUF%G_TB > 0) THEN
                   DO I=1,NEL
                     VALUE(I) = -GBUF%TB(I)
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO
                 ELSEIF (MLW == 51)THEN
                   IPOS      = 15
                   ITRIMAT   = 4    
                   MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1) 
                   K         = IPARG(2,NG) * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                             
                   DO I=1,IPARG(2,NG)
                     VALUE(I) = -MBUF%VAR(K+I)
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO 
                 ENDIF 
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'MOMX') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 								      
                    DO I=1,NEL									   
                      VALUE(I) = GBUF%MOM(JJ(1) + I)
                      IS_WRITTEN_VALUE(I) = 1					    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'MOMY') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 								      
                    DO I=1,NEL									   
                      VALUE(I) = GBUF%MOM(JJ(2) + I)
                      IS_WRITTEN_VALUE(I) = 1					    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'MOMZ') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 								      
                    DO I=1,NEL									   
                      VALUE(I) = GBUF%MOM(JJ(3) + I)
                      IS_WRITTEN_VALUE(I) = 1					    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'MOMXY') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   																					  
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                 GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I) ) 
                      IS_WRITTEN_VALUE(I) = 1		    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'MOMYZ') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   																						    
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                 GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) ) 
                      IS_WRITTEN_VALUE(I) = 1		
                    ENDDO 
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'MOMXZ') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   																						    
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                 GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) ) 
                      IS_WRITTEN_VALUE(I) = 1		
                    ENDDO 
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == '|MOM|') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)
                IALEFVM_FLG = IPM(251,MT)
                IF(IALEFVM_FLG >= 2)THEN
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   														    
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                   GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I)+
     .                                   GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )  
                      IS_WRITTEN_VALUE(I) = 1		
                    ENDDO 
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'VELX') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 								      
                    DO I=1,NEL									   
                      VALUE(I) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1					    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'VELY') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 								      
                    DO I=1,NEL									   
                      VALUE(I) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1					    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'VELZ') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 								      
                    DO I=1,NEL									   
                      VALUE(I) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1					    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'VELXY') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   																					  
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                 GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I) )  / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1		    
                    ENDDO  
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'VELYZ') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   																						    
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                 GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )  / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1		
                    ENDDO 
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == 'VELXZ') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   																						    
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                 GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )  / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1		
                    ENDDO 
                  ENDIF
                ENDIF
C--------------------------------------------------  
              ELSEIF (KEYWORD == '|VEL|') THEN 
C--------------------------------------------------          
                MT          = IXS(1,NFT+1)                            
                IALEFVM_FLG = IPM(251,MT)                             
                IF(IALEFVM_FLG >= 2)THEN                            
                  IF (ISOLNOD == 8)THEN 
                    DO I=1,NEL   														    
                      VALUE(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     .                                   GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I)+
     .                                   GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) ) / GBUF%RHO(I) 
                      IS_WRITTEN_VALUE(I) = 1		
                    ENDDO 
                  ENDIF
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'AMS')THEN
C-------------------------------------------------- 
                IF(GBUF%G_ISMS > 0) THEN 
                  DO I=1,NEL                    
                    VALUE(I) = GBUF%ISMS(I)
                    IS_WRITTEN_VALUE(I) = 1	    
                  ENDDO
                ENDIF 
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'EINTM' .OR. KEYWORD == 'ENER')THEN
C-------------------------------------------------- 
               !LAG: GBUF%VOL = V0,    GBUF%EINT=rho0.e
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I)  !
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     N = I + NFT
                     IALEL=IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL == 0)THEN
                        MT=IXS(1,N)
                        VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT))   !
                     ELSE
                        VALUE(I) = GBUF%EINT(I)/MAX(EM20,GBUF%RHO(I))  !
                     ENDIF
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'EINTV')THEN
C-------------------------------------------------- 
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     N = I + NFT
                     IALEL=IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL == 0)THEN
                        MT=IXS(1,N)
                        VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT))*GBUF%RHO(I)
                     ELSE
                        VALUE(I) = GBUF%EINT(I)
                     ENDIF
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'EINT')THEN
C-------------------------------------------------- 
              IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT) * GBUF%VOL(I)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     N = I + NFT
                     IALEL=IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL == 0)THEN
                        MT=IXS(1,N)
                        VOL=GBUF%VOL(I)*PM(89,MT)/GBUF%RHO(I)
                        VALUE(I) = GBUF%EINT(I)/PM(89,MT)*GBUF%RHO(I)*VOL
                     ELSE
                        VALUE(I) = GBUF%EINT(I)*GBUF%VOL(I)
                     ENDIF
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD(1:4) == 'ENTH')THEN
C-------------------------------------------------- 
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     PRES(I) = MULTI_FVM%PRES(I + NFT)
                  ENDDO
               ELSE
                  DO I=1,NEL
                     PRES(I) = - (GBUF%SIG(JJ(1) + I)+ GBUF%SIG(JJ(2) + I) + GBUF%SIG(JJ(3) + I))*THIRD
                  ENDDO
               ENDIF    
               !GBUF%EINT is rho.e           
               IF(KEYWORD == 'ENTH')THEN
                 IF (MLW == 151) THEN
                     DO I = 1, NEL
                        VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT) + PRES(I)*GBUF%VOL(I) !
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ELSE
                     DO I=1,NEL
                        N = I + NFT
                        IALEL=IPARG(7,NG)+IPARG(11,NG)
                        IF(IALEL == 0)THEN
                           MT=IXS(1,N)
                           MASS0=GBUF%VOL(I)*PM(89,MT)
                           VOL=MASS0/MAX(EM20,GBUF%RHO(I))
                           VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT)) + PRES(I)*VOL   !
                        ELSE
                           VALUE(I) = GBUF%EINT(I)/GBUF%RHO(I) + PRES(I)*GBUF%VOL(I) ! 
                        ENDIF
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ENDIF                
               ELSEIF(KEYWORD == 'ENTHV')THEN
                 IF (MLW == 151) THEN
                     DO I = 1, NEL
                        VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT)/GBUF%VOL(I) + PRES(I) !
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ELSE
                     DO I=1,NEL
                        N = I + NFT
                        IALEL=IPARG(7,NG)+IPARG(11,NG)
                        IF(IALEL == 0)THEN
                           MT=IXS(1,N)
                           MASS0=GBUF%VOL(I)*PM(89,MT)
                           VOL=MASS0/MAX(EM20,GBUF%RHO(I))
                           VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT))/VOL + PRES(I)  !
                        ELSE
                           VALUE(I) = GBUF%EINT(I)/GBUF%VOL(I)/GBUF%RHO(I) + PRES(I)  !
                        ENDIF
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ENDIF                                
               ELSEIF(KEYWORD == 'ENTHM')THEN
                 IF (MLW == 151) THEN
                     DO I = 1, NEL
                        MASS(I) = MULTI_FVM%RHO(I + NFT)*GBUF%VOL(I)
                        VALUE(I) = (MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT) + PRES(I)*GBUF%VOL(I))/MASS(I) !
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ELSE
                     DO I=1,NEL
                        N = I + NFT
                        IALEL=IPARG(7,NG)+IPARG(11,NG)
                        IF(IALEL == 0)THEN
                           MT=IXS(1,N)
                           MASS0=GBUF%VOL(I)*PM(89,MT)
                           VOL=MASS0/MAX(EM20,GBUF%RHO(I))
                           MASS(I)=MASS0
                           VALUE(I) = (GBUF%EINT(I)/MAX(EM20,PM(89,MT)) + PRES(I)*VOL)/MASS(I)  !
                        ELSE
                           MASS(I)=GBUF%RHO(I)*GBUF%VOL(I)
                           VALUE(I) = (GBUF%EINT(I)/GBUF%RHO(I) + PRES(I)*GBUF%VOL(I))/MASS(I)
                        ENDIF
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ENDIF                                 
               ENDIF                   
C--------------------------------------------------
              ELSEIF(KEYWORD == 'OFF')THEN  
C--------------------------------------------------                     
                DO I=1,NEL
                  IF (GBUF%G_OFF > 0) THEN
                    IF(GBUF%OFF(I) > ONE) THEN
                      VALUE(I) = GBUF%OFF(I) - ONE
                    ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                      VALUE(I) = GBUF%OFF(I)
                    ELSE
                      VALUE(I) = -ONE
                    ENDIF
                  ENDIF
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'MACH') THEN  
C-------------------------------------------------- 
                IF (MLW == 151) THEN
                   DO I = 1, NEL  
                      VEL(1) = MULTI_FVM%VEL(1, I + NFT)
                      VEL(2) = MULTI_FVM%VEL(2, I + NFT)
                      VEL(3) = MULTI_FVM%VEL(3, I + NFT)
                      VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))
                      VALUE(I) = VEL(0)/MULTI_FVM%SOUND_SPEED(I + NFT)
                      IS_WRITTEN_VALUE(I) = 1                             
                   ENDDO
                ELSEIF(I_ALE_SOLVER>1)THEN
                     L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
                     IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN              
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                        DO  I=1,NEL     
                           VEL(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                           VEL(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                           VEL(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                           VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                                    
                           VALUE(I) = VEL(0)/LBUF%SSP(I) 
                           IS_WRITTEN_VALUE(I) = 1                          
                        ENDDO          
                     ENDIF      
                ELSE  
                  L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP              
                  IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN   
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)    
                    IF(IS_ALE /= 0)THEN  
                    !ale
                      TMP_2(1:MVSIZ,1:3) = ZERO     
                      DO J=1,8          
                       DO  I=1,NEL    
                        TMP_2(I,1)=TMP_2(I,1) + V(1,IXS(J+1,I+NFT))-W(1,IXS(J+1,I+NFT))
                        TMP_2(I,2)=TMP_2(I,2) + V(2,IXS(J+1,I+NFT))-W(2,IXS(J+1,I+NFT))
                        TMP_2(I,3)=TMP_2(I,3) + V(3,IXS(J+1,I+NFT))-W(3,IXS(J+1,I+NFT))
                       ENDDO
                      ENDDO
                      DO  I=1,NEL
                        VEL(1) = TMP_2(I,1)*ONE_OVER_8!SUM(TMP_2(1,1:8))*ONE_OVER_8
                        VEL(2) = TMP_2(I,2)*ONE_OVER_8!SUM(TMP_2(2,1:8))*ONE_OVER_8
                        VEL(3) = TMP_2(I,3)*ONE_OVER_8!SUM(TMP_2(3,1:8))*ONE_OVER_8 
                        VALUE(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)   
                        IS_WRITTEN_VALUE(I) = 1                                  
                      ENDDO
                    ELSE
                    !euler and lagrange
                      TMP_2(1:MVSIZ,1:3) = ZERO     
                      DO J=1,8          
                       DO  I=1,NEL  
                        TMP_2(I,1)=TMP_2(I,1)+V(1,IXS(J+1,I+NFT))               
                        TMP_2(I,2)=TMP_2(I,2)+V(2,IXS(J+1,I+NFT))               
                        TMP_2(I,3)=TMP_2(I,3)+V(3,IXS(J+1,I+NFT))  
                       ENDDO
                      ENDDO 
                      DO  I=1,NEL             
                        VEL(1) = TMP_2(I,1)*ONE_OVER_8!SUM(TMP(1,1:8))*ONE_OVER_8              
                        VEL(2) = TMP_2(I,2)*ONE_OVER_8!SUM(TMP(2,1:8))*ONE_OVER_8              
                        VEL(3) = TMP_2(I,3)*ONE_OVER_8!SUM(TMP(3,1:8))*ONE_OVER_8              
                        VALUE(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)
                        IS_WRITTEN_VALUE(I) = 1  
                      ENDDO      
                    ENDIF
                  ENDIF
                ENDIF     
C--------------------------------------------------
              ELSEIF(KEYWORD == 'GROUP')THEN 
C-------------------------------------------------- 
                DO I=1,NEL
                  VALUE(I) = NG
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'INTERNAL.ID')THEN 
C-------------------------------------------------- 
                DO I=1,NEL
                  VALUE(I) = I+NFT
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'LOCAL.ID')THEN 
C-------------------------------------------------- 
                DO I=1,NEL
                  VALUE(I) = I
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO                                
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'THICK' .AND.TSHELL == 1)THEN
C--------vol=mass/rho=vol0*rho0/rho  make sure GBUF%RHO is well computed in elem
C--------thick = vol/A_m : new routine to compute A_m
                MT = IXS(1,NFT+1)
                RHO0 = PM(1,MT)
                IF (ISOLNOD == 6 )THEN
C-- 2g1=2-1+5-4; 2g2 = 3-1+6-4                
                  DO I=1,NEL
                    N = I + NFT
                    NC(1:3) = IXS(2:4,N)
                    NC(4:6) = IXS(6:8,N)
                    G1(I,1:3) = X(1:3,NC(2))-X(1:3,NC(1))+X(1:3,NC(5))-X(1:3,NC(4))               
                    G2(I,1:3) = X(1:3,NC(3))-X(1:3,NC(1))+X(1:3,NC(6))-X(1:3,NC(4))               
                  ENDDO
                  CALL THS_MAREA(G1(1,1),G1(1,2),G1(1,3),G2(1,1),G2(1,2),G2(1,3),DET,NEL)                 
                  DO I=1,NEL
                    VOLN(I)=GBUF%VOL(I)*RHO0/GBUF%RHO(I)
                    AREAM(I) =ONE_OVER_8*DET(I) 
                    VALUE(I) = VOLN(I)/AREAM(I)
                    IS_WRITTEN_VALUE(I) = 1                       
                  ENDDO    
                ELSEIF (ISOLNOD == 8 )THEN
C-- 4g1=2+3+6+7-1-4-5-8; 4g2 = 3+4+7+8-1-2-5-6; 4g3 = 5+6+7+8-1-2-3-4;               
                  IF (JHBE==15 ) THEN
                    DO I=1,NEL
                      N = I + NFT
                      NC(1:8) = IXS(2:9,N)
                      G1(I,1:3) = X(1:3,NC(2))+X(1:3,NC(3))+X(1:3,NC(6))+X(1:3,NC(7))               
     .                           -X(1:3,NC(1))-X(1:3,NC(4))-X(1:3,NC(5))-X(1:3,NC(8))
                      G2(I,1:3) = X(1:3,NC(3))+X(1:3,NC(4))+X(1:3,NC(7))+X(1:3,NC(8))               
     .                           -X(1:3,NC(1))-X(1:3,NC(2))-X(1:3,NC(5))-X(1:3,NC(6))               
                    ENDDO
                    CALL THS_MAREA(G1(1,1),G1(1,2),G1(1,3),G2(1,1),G2(1,2),G2(1,3),DET,NEL)                 
                    DO I=1,NEL
                      VOLN(I)=GBUF%VOL(I)*RHO0/GBUF%RHO(I) 
                      AREAM(I) =ONE_OVER_16*DET(I) 
                      VALUE(I) = VOLN(I)/AREAM(I)
                      IS_WRITTEN_VALUE(I) = 1                       
                    ENDDO    
                  ELSEIF (JHBE==14 ) THEN
                    DO I=1,NEL
                      N = I + NFT
                      NC(1:8) = IXS(2:9,N)
                      G1(I,1:3) = X(1:3,NC(2))+X(1:3,NC(3))+X(1:3,NC(6))+X(1:3,NC(7))               
     .                           -X(1:3,NC(1))-X(1:3,NC(4))-X(1:3,NC(5))-X(1:3,NC(8))
                      G2(I,1:3) = X(1:3,NC(3))+X(1:3,NC(4))+X(1:3,NC(7))+X(1:3,NC(8))               
     .                           -X(1:3,NC(1))-X(1:3,NC(2))-X(1:3,NC(5))-X(1:3,NC(6))               
                      G3(I,1:3) = X(1:3,NC(5))+X(1:3,NC(6))+X(1:3,NC(7))+X(1:3,NC(8))               
     .                           -X(1:3,NC(1))-X(1:3,NC(2))-X(1:3,NC(3))-X(1:3,NC(4))               
                    ENDDO
C------RHO0 isn't right w/ composite                    
                    IF (IGTYP==22) THEN
                      CALL THS_VOL(G1(1,1),G1(1,2),G1(1,3),G2(1,1),G2(1,2),G2(1,3),
     .                             G3(1,1),G3(1,2),G3(1,3),DET,NEL)               
                      VOLN(1:NEL)=ZEP015625*DET(1:NEL)
                    ELSE
                      VOLN(1:NEL)=GBUF%VOL(1:NEL)*RHO0/GBUF%RHO(1:NEL)
                    END IF
                    ICSIG = IPARG(17,NG)
            	    SELECT CASE (ICSIG)  					   
            	     CASE (1)	! g2,g3						   
                       CALL THS_MAREA(G2(1,1),G2(1,2),G2(1,3),G3(1,1),G3(1,2),G3(1,3),DET,NEL)                 
            	     CASE (10)  ! g1,g2
                       CALL THS_MAREA(G1(1,1),G1(1,2),G1(1,3),G2(1,1),G2(1,2),G2(1,3),DET,NEL)                 
            	     CASE (100)	! g3,g1						   
                       CALL THS_MAREA(G3(1,1),G3(1,2),G3(1,3),G1(1,1),G1(1,2),G1(1,3),DET,NEL)                 
            	    END SELECT
                    DO I=1,NEL
                      AREAM(I) =ONE_OVER_16*DET(I) 
                      VALUE(I) = VOLN(I)/AREAM(I)
                      IS_WRITTEN_VALUE(I) = 1                       
                    ENDDO
                  END IF !(JHBE==15 ) THEN
                ELSEIF (ISOLNOD == 16) THEN
C---- approximated by S8                
                    DO I=1,NEL
                      N = I + NFT
                      NC(1:8) = IXS(2:9,N)
                      G1(I,1:3) = X(1:3,NC(2))+X(1:3,NC(3))+X(1:3,NC(6))+X(1:3,NC(7))               
     .                           -X(1:3,NC(1))-X(1:3,NC(4))-X(1:3,NC(5))-X(1:3,NC(8))
                      G2(I,1:3) = X(1:3,NC(3))+X(1:3,NC(4))+X(1:3,NC(7))+X(1:3,NC(8))               
     .                           -X(1:3,NC(1))-X(1:3,NC(2))-X(1:3,NC(5))-X(1:3,NC(6))               
                    ENDDO
                    CALL THS_MAREA(G1(1,1),G1(1,2),G1(1,3),G2(1,1),G2(1,2),G2(1,3),DET,NEL)                 
                    DO I=1,NEL
                      VOLN(I)=GBUF%VOL(I)*RHO0/GBUF%RHO(I) 
                      AREAM(I) =ONE_OVER_16*DET(I) 
                      VALUE(I) = VOLN(I)/AREAM(I)
                      IS_WRITTEN_VALUE(I) = 1                       
                    ENDDO    
                END IF 
              ELSEIF(KEYWORD == 'THIN'.AND.TSHELL == 1)THEN
                  FAC = ONE/NLAY
                  EZZ(1:NEL) = ZERO
                  IF (JHBE==15 ) THEN
                    FAC = ONE/NLAY
                    DO ILAY=1,NLAY
                      LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
                      DO I=1,NEL
                        EZZ(I) = EZZ(I)+FAC*LBUF%STRA(JJ(3)+I)  
                      ENDDO 
                    END DO    
                    VALUE(1:NEL) = -HUNDRED*(EXP(EZZ(1:NEL))-ONE)
                    IS_WRITTEN_VALUE(1:NEL) = 1                       
                  ELSEIF (JHBE==14 ) THEN
                    FAC = ONE/(NLAY*NPTR*NPTS)
                    DO IR=1,NPTR
                     DO IS=1,NPTS
                      DO ILAY=1,NLAY
                        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                        DO I=1,NEL
                          EZZ(I) = EZZ(I)+FAC*LBUF%STRA(JJ(3)+I)  
                        ENDDO 
                      ENDDO  ! IL=1,NLAY
                     ENDDO   ! IS=1,NPTS
                    ENDDO    ! IR=1,NPTR
                    VALUE(1:NEL) = -HUNDRED*(EXP(EZZ(1:NEL))-ONE)
                    IS_WRITTEN_VALUE(1:NEL) = 1                       
                  ELSEIF (JHBE==16 ) THEN
                    FAC = ONE/(NLAY*NPTR*NPTT)
                    DO IT=1,NPTT
                      DO IR=1,NPTR
                        DO ILAY=1,NLAY
                          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,1,IT)
C-------strain is calculated in basic system of S16 will be done later                          
                          DO I=1,NEL
c                            EZZ(I) = EZZ(I)+FAC*LBUF%STRA(JJ(2)+I)  
                          ENDDO 
                        ENDDO!IL=1,NLAY
                     ENDDO!IR=1,NPTR
                    ENDDO!IT=1,NPTT
c                    VALUE(1:NEL) = HUNDRED*EZZ(1:NEL)
c                    IS_WRITTEN_VALUE(1:NEL) = 1                       
                  END IF 
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'COLOR') THEN  
C--------------------------------------------------                 
              GBUF => ELBUF_TAB(NG)%GBUF 
              IF (MLW == 151) THEN 
                 NFRAC=MULTI_FVM%NBMAT
                 DO IMAT=1,NFRAC
                    LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)
                    DO I=1,NEL
                       VFRAC(I,IMAT) = LBUF%VOL(I) / GBUF%VOL(I)         
                    ENDDO
                  ENDDO
              ELSEIF(MLW == 20)THEN 
                NFRAC=2
                DO I=1,NEL
                  VFRAC(I,1) = ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%VOL(I) / GBUF%VOL(I)  
                  VFRAC(I,2) = ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)%VOL(I) / GBUF%VOL(I)                           
                ENDDO 
              ELSEIF(MLW == 37)THEN 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1) 
                NFRAC=2                  
                DO  I=1,NEL
                  VFRAC(I,1) = MBUF%VAR(I+3*NEL)
                  VFRAC(I,2) = MBUF%VAR(I+4*NEL)
                ENDDO 
              ELSEIF(MLW == 51)THEN
                !get UPARAM
                IMAT = IXS(1,NFT+1)                        
                IADBUF = IPM(7,IMAT)                       
                NUPARAM= IPM(9,IMAT)                       
                UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                !bijective order           !indexes  
                ISUBMAT=UPARAM(276+1);     IU(1)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                ISUBMAT=UPARAM(276+2);     IU(2)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                ISUBMAT=UPARAM(276+3);     IU(3)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                ISUBMAT=UPARAM(276+4);     IU(4)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)  
                NFRAC=4   
                DO  I=1,NEL
                  VFRAC(I,1) = MBUF%VAR(I+IU(1)*NEL)
                  VFRAC(I,2) = MBUF%VAR(I+IU(2)*NEL)   
                  VFRAC(I,3) = MBUF%VAR(I+IU(3)*NEL)   
                  VFRAC(I,4) = MBUF%VAR(I+IU(4)*NEL)
                ENDDO 
              ELSE
                NFRAC=0
                !VFRAC(1:NEL,1:21)=ZERO     
              ENDIF 
              IF(NFRAC>0)THEN
                DO I=1,NEL
                  VALUE(I)=ZERO
                  DO IMAT=1,NFRAC
                    VALUE(I) = VALUE(I) + VFRAC(I,IMAT)*IMAT
                  ENDDO
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
              ENDIF
C--------------------------------------------------                 

C--------------------------------------------------  
              ELSEIF(KEYWORD == 'VONM/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_YIELD(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'SIGEQ/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_SEQ(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'ENER/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_EINT(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'DAMA/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_DMG(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------                 
              ENDIF
c
              CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_SOLID,SOLID_SCALAR,NEL,OFFSET,NFT,
     .   			  VALUE,IS_WRITTEN_VALUE)
            ENDIF
          ENDIF
       ENDIF

C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  THS_MAREA                     source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THS_MAREA(RX, RY, RZ, SX, SY, SZ, DET,NEL)
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
      INTEGER NEL
C     REAL
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: RX,RY,RZ
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: SX,SY,SZ
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: DET
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   E3X(NEL), E3Y(NEL), E3Z(NEL) 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=1,NEL
C---------E3------------
       E3X(I) = RY(I) * SZ(I) - RZ(I) * SY(I) 
       E3Y(I) = RZ(I) * SX(I) - RX(I) * SZ(I) 
       E3Z(I) = RX(I) * SY(I) - RY(I) * SX(I) 
       DET(I) = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
      ENDDO 
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  THS_VOL                       source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THS_VOL(RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,DET,NEL)
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
      INTEGER NEL
C     REAL
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: RX,RY,RZ
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: SX,SY,SZ
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: TX,TY,TZ
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: DET
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real JAC1, JAC2, JAC3, JAC4, JAC5, JAC6, JAC7, JAC8, JAC9 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=1,NEL
       JAC1 = RX(I) 
       JAC2 = RY(I) 
       JAC3 = RZ(I) 
       JAC4 = SX(I) 
       JAC5 = SY(I) 
       JAC6 = SZ(I) 
       JAC7 = TX(I) 
       JAC8 = TY(I) 
       JAC9 = TZ(I) 
       DET(I) = JAC1 * (JAC5 * JAC9 - JAC8 * JAC6) + 
     .          JAC2 * (JAC6 * JAC7 - JAC4 * JAC9) + 
     .          JAC3 * (JAC4 * JAC8 - JAC7 * JAC5)
      ENDDO 
c-----------
      RETURN
      END
