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
Chd|  DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        CLSCONV3                      source/output/anim/generate/dfuncc.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SCHLIEREN                     source/output/anim/generate/schlieren.F
Chd|        SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE DFUNCC(ELBUF_TAB ,FUNC      ,IFUNC     ,IPARG       ,GEO        ,
     .                  IXQ       ,IXC       ,IXTG      ,MASS        ,PM         ,
     .                  EL2FA     ,NBF       ,IADP        ,
     .                  NBF_L     ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM       ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT    ,X         ,V         ,W           ,ALE_CONNECTIVITY,
     .                  NV46      ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK     ,BUFMAT    ,MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD    
      USE MULTI_FVM_MOD            
      USE ALE_CONNECTIVITY_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param
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
#include      "scr25_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   FUNC(*),MASS(*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(*), ERR_THK_SH3(*),BUFMAT(*)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46
      REAL WAL(NBF_L)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY)         :: STACK
      TYPE(BUF_MAT_)  ,POINTER :: MBUF 
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   EVAR(MVSIZ),DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   SIG1(MVSIZ),SIG2(MVSIZ),SIG3(MVSIZ),
     .   A002(MVSIZ),VALUES(MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,VALUE,VALUE1,VALUE2,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM_,AREA,X2L,VAR,RINDX,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),DMGMX_LY,EVAR_TMP,A01,A02,A03,A12,A,
     .   M,FF,GG,NN,
     .   VEL(0:3),VFRAC(MVSIZ,21),PHI,ERR,NINTY
      INTEGER I,IDX,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,PTF,PTM,PTS,NFAIL,
     .        N,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),ID_PLY,NB_PLYOFF,IFRAM_OLD,
     .        JJ(6),NPGT,IADBUF,NUPARAM,IMAT,NS,NRATE,EXPA,IVISC,IU(4),NFRAC
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),LENCOM,NPT_ALL,IPLY,ITRIMAT,IPOS,
     .        ISUBMAT, ISH_EINT, IS_ALE, IS_EULER,IPG,IPINCH,
     .        IMAT_TILLOTSON, NTILLOTSON,NVAREOS,IEOS,IDRAPE
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      TYPE(BUF_EOS_)  ,POINTER :: EBUF  
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
      
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR,OFFL
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2 
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      TARGET :: BUFMAT
      my_real TMP(3,4)
      DATA NS/10/
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
      NINTY = 90.
      NN1  = 1
      NN3  = 1
      NN4  = NN3 + NUMELQ
      NN5  = NN4 + NUMELC
      NN6  = NN5 + NUMELTG
      NN9  = NN6 
      ISH_EINT = 13242 + 4*MX_PLY_ANIM + 2
      
      !-------------------------------------------------------!
      !     INITIALIZATION IF SCHLIEREN DEFINED               !
      !-------------------------------------------------------!      
      IF(IFUNC==10672)THEN
         CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM)
      ENDIF!(IFUNC==10672)

C-----------------------------------------------

      DO NG=1,NGROUP
       CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF(MLW /= 13) THEN
        DO OFFSET = 0,NEL-1,NVSIZ
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-OFFSET)
          ISUBSTACK = IPARG(71,NG)
          IVISC = IPARG(61,NG)
          IS_ALE=IPARG(7,NG)
          IS_EULER=IPARG(11,NG)
          IDRAPE = ELBUF_TAB(NG)%IDRAPE
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO
!
c
          DO I=LFT,LLT
!            EVAR(I) = ZERO   
            SIG1(I) = ZERO
            SIG2(I) = ZERO
            SIG3(I) = ZERO
            A002(I)  = ZERO
          ENDDO
C-----------------------------------------------
C           QUAD OR TRIA
C-----------------------------------------------
        IF (ITY == 2 .OR.(ITY == 7.AND.N2D/=0) ) THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
            JALE=(IPARG(7,NG)+IPARG(11,NG))
            JTURB=IPARG(12,NG)*JALE
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
            NPTT  = ELBUF_TAB(NG)%NPTT
            NLAY  = ELBUF_TAB(NG)%NLAY
            NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
C
            DO I=LFT,LLT
              FUNC(EL2FA(NN3+NFT+I))= ZERO
            ENDDO
C
            IF (IFUNC == 1)THEN  ! EPSP
              IF (MLW == 10 .OR. MLW == 21) THEN
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = LBUF%EPSQ(I)
                ENDDO
              ELSEIF (MLW == 24) THEN   ! et autres a ajouter
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = LBUF%VK(I)
                ENDDO
              ELSEIF (MLW == 6 .OR. MLW == 17 .OR. MLW == 11) THEN   ! et autres a ajouter
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = LBUF%RK(I)
                ENDDO
              ELSEIF (MLW >=28 .AND. MLW /= 49 .and. NUVAR > 0) THEN
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = UVAR(I)
                ENDDO
              ELSE
                IF (GBUF%G_PLA > 0) THEN
                  DO I=LFT,LLT
                    FUNC(EL2FA(NN3+NFT+I)) = GBUF%PLA(I)
                  ENDDO
                ENDIF
              ENDIF
            ELSEIF(IFUNC == 2)THEN
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = GBUF%RHO(I)
              ENDDO
            ELSEIF(IFUNC == 3)THEN
              DO I=LFT,LLT
                  N = I + NFT
                  IALEL=IPARG(7,NG)+IPARG(11,NG)
                  IF(IALEL == 0)THEN
                    MT=IXQ(1,N)
                    VALUE = GBUF%EINT(I)/MAX(EM30,PM(1,MT))
                  ELSE
                    VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                  ENDIF
                  FUNC(EL2FA(NN3+N)) = VALUE
              ENDDO
            ELSEIF(IFUNC == 4)THEN
               IF(GBUF%G_TEMP > 0)THEN
                 DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = GBUF%TEMP(I)
                 ENDDO
               ELSE
                 DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = ZERO
                 ENDDO               
               ENDIF
            ELSEIF(IFUNC == 6)THEN
               DO I=LFT,LLT
                P = - (GBUF%SIG(JJ(1) + I)
     .               + GBUF%SIG(JJ(2) + I)
     .               + GBUF%SIG(JJ(3) + I))*THIRD
                FUNC(EL2FA(NN3+NFT+I)) = P
               ENDDO
            ELSEIF(IFUNC == 7)THEN
               DO I=LFT,LLT
                P = - (GBUF%SIG(JJ(1) + I)
     .               + GBUF%SIG(JJ(2) + I)
     .               + GBUF%SIG(JJ(3) + I) )*THIRD
                S1 = GBUF%SIG(JJ(1) + I) + P
                S2 = GBUF%SIG(JJ(2) + I) + P
                S3 = GBUF%SIG(JJ(3) + I) + P
                VONM2 = THREE*(GBUF%SIG(JJ(4) + I)**2
     .                + HALF*(S1**2+S2**2+S3**2) )
                FUNC(EL2FA(NN3+NFT+I)) = SQRT(VONM2)
               ENDDO
            ELSEIF(IFUNC == 8 .AND. JTURB/=0)THEN
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = GBUF%RK(I)
              ENDDO
            ELSEIF(IFUNC == 9 )THEN  
              IF (MLW == 6 .OR. MLW == 17.AND.JTURB/=0) THEN
              DO I=LFT,LLT
                  N = I + NFT
                   MT=IXQ(1,N)
                   FUNC(EL2FA(NN3+N))=PM(81,MT)*GBUF%RK(I)**2/
     .                  MAX(EM15,GBUF%RE(I))
              ENDDO
             ELSEIF(MLW == 46 .OR. MLW == 47)THEN
              DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I))= UVAR(I)
              ENDDO
              ENDIF
            ELSEIF(IFUNC == 10 )THEN  ! VORTX
              IF (MLW == 6 .OR. MLW == 17) THEN
              DO I=LFT,LLT
                   FUNC(EL2FA(NN3+NFT+I)) = LBUF%VK(I)
                ENDDO
              ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = UVAR(NEL+I)
              ENDDO
             ENDIF
            ELSEIF((IFUNC == 11.OR.IFUNC == 12.OR.IFUNC == 13)
     .            .AND.MLW == 24)THEN   ! DAM(3)
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = LBUF%DAM(JJ(IFUNC-10) + I)
              ENDDO
            ELSEIF(IFUNC == 14)THEN   ! Sigx
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = GBUF%SIG(JJ(3) + I)
              ENDDO
            ELSEIF(IFUNC == 15)THEN   ! Sigy
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = GBUF%SIG(JJ(1) + I)
              ENDDO
            ELSEIF(IFUNC == 16)THEN   ! Sigz
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = GBUF%SIG(JJ(2) + I)
              ENDDO
            ELSEIF(IFUNC == 17.OR.IFUNC == 18)THEN   ! Sigxy
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = GBUF%SIG(JJ(4) + I)
              ENDDO
c----
            ELSEIF(IFUNC>=20.AND.IFUNC<=24.AND.
     .             (MLW == 28.OR.MLW == 29.OR.MLW == 30.OR.
     .              MLW == 31.OR.MLW == 52.OR.MLW == 79))THEN
c----         USER VARIABLES from 1 to 5

              IUS = IFUNC - 20
              DO I=LFT,LLT
                N = I + NFT
                MT = IXQ(1,N)
                NUVAR = IPM(8,MT)
                IF (NUVAR > IUS) FUNC(EL2FA(NN3+N)) = UVAR(IUS*NEL + I)
              ENDDO
            ELSEIF(IFUNC == 25)THEN
               DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN3+NFT+I)) = EHOUR(N)
               ENDDO
c----
            ELSEIF (IFUNC>=27 .AND. IFUNC<=39 .AND.
     .       (MLW == 28.OR.MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.
     .        MLW == 52))THEN
c----           USER VARIABLES from 6 to 60
              IUS = IFUNC - 22    ! IUS = (n-1)'th user variable in UVAR
              DO I=LFT,LLT
                N = I + NFT
                MT=IXQ(1,N)
                  NUVAR = IPM(8,MT)
                IF (NUVAR>IUS) FUNC(EL2FA(NN3+N)) = UVAR(IUS*NEL + I)
              ENDDO
              
            !/ANIM/ELEM/VFRAC law 20
            ELSEIF(MLW == 20 .AND. (IFUNC == 10248.OR.IFUNC == 10249))THEN 
             DO  I=LFT,LLT
             FUNC(EL2FA(NN3+NFT+I)) = 
     .          ELBUF_TAB(NG)%BUFLY(IFUNC-10248+1)%LBUF(1,1,1)%VOL(I)
     .        / ELBUF_TAB(NG)%GBUF%VOL(I)             
             ENDDO   

            !/ANIM/ELEM/VFRAC law 37
            ELSEIF(MLW == 37 .AND. (IFUNC == 10248.OR.IFUNC == 10249))THEN 
             IUS=3+(IFUNC-10248) !law37 user4 and user5    
             MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
             DO  I=LFT,LLT
               FUNC(EL2FA(NN3+NFT+I)) = MBUF%VAR(I+IUS*NEL)
             ENDDO      

            !/ANIM/ELEM/VFRAC law 51
            ELSEIF(MLW == 51 .AND. (IFUNC >= 10248.AND.IFUNC <= 10251))THEN 
              IMAT = IXQ(1,NFT+1)                        
              IADBUF = IPM(7,IMAT)                       
              NUPARAM= IPM(9,IMAT)                       
              UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM) 
              ISUBMAT = (IFUNC-10247)
              ISUBMAT = UPARAM(276+ISUBMAT) !bijective order
             IUS=N0PHAS+(ISUBMAT-1)*NVPHAS 
             MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
             DO  I=LFT,LLT
               FUNC(EL2FA(NN3+NFT+I)) = MBUF%VAR(I+IUS*NEL)
             ENDDO   

            ELSEIF(MLW == 151 .AND. (IFUNC >= 10248.AND.IFUNC <= 10250))THEN 
             IUS= IFUNC - 10248 + 1
             IF(IUS<=NLAY)THEN
               DO  I=LFT,LLT
                 FUNC(EL2FA(NN3+NFT+I)) = ELBUF_TAB(NG)%BUFLY(IUS)%LBUF(1,1,1)%VOL(I) / GBUF%VOL(I)
               ENDDO      
             ELSE
               DO  I=LFT,LLT
                 FUNC(EL2FA(NN3+NFT+I)) = ZERO
               ENDDO               
             ENDIF  
 
            !Burn Fraction      
            ELSEIF(IFUNC == 10252)THEN 
              IF(ELBUF_TAB(NG)%GBUF%G_BFRAC>0)THEN
                DO  I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) =  ELBUF_TAB(NG)%GBUF%BFRAC(I)             
                ENDDO  
              ELSE
                DO  I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = ZERO
                ENDDO
              ENDIF
              
            ELSEIF(IFUNC == 10671)THEN                                
               IF (MLW == 151) THEN
                  DO I = 1, NEL                                         
                    FUNC(EL2FA(NN3+NFT+I)) = MULTI_FVM%SOUND_SPEED(I + NFT)
                  ENDDO
               ELSE
                  L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
                  IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN              
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                     DO  I=1,NEL                                         
                       FUNC(EL2FA(NN3+NFT+I)) = LBUF%SSP(I) 
                     ENDDO                                                 
                  ENDIF       
               ENDIF
              
            ELSEIF(IFUNC == 10672) THEN                                                        
               IALEL=IPARG(7,NG)+IPARG(11,NG)
               IF(IALEL == 0)THEN                                                             
                  DO  I=LFT,LLT                                                                                                 
                    FUNC(EL2FA(NN3+NFT+I)) = ZERO                       
                  ENDDO 
               ELSE
                 IF(ITY==2)THEN                                                                         
                   CALL SCHLIEREN(
     1                   EVAR    , IXQ    , X           , V       , W       ,
     2                   IPARG   , WA_L   , ELBUF_TAB   , ALE_CONNECTIVITY   , GBUF%VOL,
     3                   PM      , NG     , NIXQ        , ITY)
                 ELSEIF(ITY==7.AND.N2D/=0)THEN
                   CALL SCHLIEREN(
     1                   EVAR    , IXTG   , X           , V                  , W       ,
     2                   IPARG   , WA_L   , ELBUF_TAB   , ALE_CONNECTIVITY   , GBUF%VOL,
     3                   PM      , NG     , NIXTG       , ITY)          
                 ENDIF 
                 DO  I=LFT,LLT 
                   FUNC(EL2FA(NN3+NFT+I)) = EVAR(I) 
                 ENDDO
               ENDIF                                                                          
!---
            ELSEIF (IFUNC == 10677) THEN  !  /ANIM/ELEM/SIGEQ
            !  equivalent stress - other than VON MISES
              IF (GBUF%G_SEQ > 0) THEN
C------------------
                ! Total number of integration points
                NPGT = 0
                DO IL=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPGT = NPGT + BUFLY%NPTT*NPTR*NPTS
                ENDDO
                ! Average equivalent stress on integration points
                DO I=LFT,LLT
                  EVAR_TMP = ZERO
                  DO IL=1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                    DO IT=1,BUFLY%NPTT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                          EVAR_TMP = EVAR_TMP + LBUF%SEQ(I)/NPGT
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                  FUNC(EL2FA(NN3+NFT+I)) = EVAR_TMP
                ENDDO
C------------------
              ELSE  ! VON MISES
                DO I=LFT,LLT
                  P = - (GBUF%SIG(JJ(1) + I)
     .                +  GBUF%SIG(JJ(2) + I)
     .                +  GBUF%SIG(JJ(3) + I))*THIRD
                  S1 = GBUF%SIG(JJ(1) + I) + P
                  S2 = GBUF%SIG(JJ(2) + I) + P
                  S3 = GBUF%SIG(JJ(3) + I) + P
                  VONM2 = THREE*(GBUF%SIG(JJ(4) + I)**2
     .                  + HALF*(S1**2+S2**2+S3**2))
                  FUNC(EL2FA(NN3+NFT+I)) = SQRT(VONM2)
                ENDDO
              ENDIF ! IF (GBUF%G_SEQ > 0)
!---
            !=== ARTIFICIAL VISCOSITY ===!  
            ELSEIF(IFUNC == 11888)THEN                                
              IF (GBUF%G_QVIS > 0) THEN
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = GBUF%QVIS(I)
                ENDDO
              ELSE
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = ZERO
                ENDDO              
              ENDIF
            !=== DETONATION TIME ===!                  
            ELSEIF (IFUNC == 11889) THEN               
              IF (MLW  /= 51 .AND. GBUF%G_TB > 0) THEN
                DO I=LFT,LLT                           
                  FUNC(EL2FA(NN3+NFT+I)) = -GBUF%TB(I)   
                ENDDO 
              ELSEIF (MLW == 51)THEN
                 MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                 IPOS      = 15
                 ITRIMAT   = 4
                 LLT       = IPARG(2,NG)      
                 K         = LLT * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                             
                 DO I=LFT,LLT
                   FUNC(EL2FA(NN3+NFT+I)) = -MBUF%VAR(K+I)
                 ENDDO 
              ELSE
                DO I=LFT,LLT                           
                  FUNC(EL2FA(NN3+NFT+I)) = ZERO   
                ENDDO 
              ENDIF                   
            ! === LAW 20 === ! 
            !DENS BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11890 .AND. IFUNC<=11893)THEN
               IF (MLW == 51) THEN
                  MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                  DO I = LFT, LLT
                     N = I + NFT
                     VALUE = ZERO
                     ITRIMAT = IFUNC - 11890 + 1
                     IPOS = 12
                     K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                     VALUE = MBUF%VAR(K + I)
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO
               ELSE 
                  DO I=LFT,LLT
                     VALUE = ZERO
                     N     = I + NFT
                     IALEL = IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL /= 0 .AND. MLW == 20)THEN
                        LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                        LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                        VALUE1 = LBUF1%RHO(I)
                        VALUE2 = LBUF2%RHO(I) 
                        VALUE  = ZERO
                        IF(IFUNC == 11890)VALUE=VALUE1 
                        IF(IFUNC == 11891)VALUE=VALUE2                   
                     ENDIF
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO          
               ENDIF
            !ENER BY VOLUME BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11894 .AND. IFUNC<=11897)THEN
               IF (MLW == 51) THEN
                  MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                  DO I = LFT, LLT
                     N = I + NFT
                     VALUE = ZERO
                     ITRIMAT = IFUNC - 11894 + 1
                     IPOS = 8
                     K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                     K2 = LLT * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+12-1)
                     VALUE = MBUF%VAR(K + I) / MBUF%VAR(K2+I)
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO
               ELSE
                  DO I=LFT,LLT
                     VALUE = ZERO
                     N     = I + NFT
                     IALEL = IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL /= 0 .AND. MLW == 20)THEN
                        LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                        LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                        VALUE1 = LBUF1%EINT(I)/MAX(EM30,LBUF1%RHO(I))
                        VALUE2 = LBUF2%EINT(I)/MAX(EM30,LBUF2%RHO(I)) 
                        VALUE  = ZERO
                        IF(IFUNC == 11894)VALUE=VALUE1 
                        IF(IFUNC == 11895)VALUE=VALUE2                   
                     ENDIF
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO  
              ENDIF
            !TEMP BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11898 .AND. IFUNC<=11901)THEN
               IF (MLW == 51) THEN
                  MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                  DO I = LFT, LLT
                     N = I + NFT
                     VALUE = ZERO
                     ITRIMAT = IFUNC - 11898 + 1
                     IPOS = 16
                     K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                     VALUE = MBUF%VAR(K + I)
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO
               ELSE
                  DO I=LFT,LLT
                     VALUE = ZERO
                     N     = I + NFT
                     IALEL = IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL /= 0 .AND. MLW == 20)THEN
                        LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                        LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                        IF(ELBUF_TAB(NG)%BUFLY(1)%L_TEMP>0)VALUE1 = LBUF1%TEMP(I)
                        IF(ELBUF_TAB(NG)%BUFLY(2)%L_TEMP>0)VALUE2 = LBUF2%TEMP(I) 
                        VALUE  = ZERO
                        IF(IFUNC == 11898)VALUE=VALUE1 
                        IF(IFUNC == 11899)VALUE=VALUE2                   
                     ENDIF
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO 
               ENDIF
            !PRES BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11902 .AND. IFUNC<=11905)THEN
               IF (MLW == 51) THEN
                  MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                  DO I = LFT, LLT
                     N = I + NFT
                     VALUE = ZERO
                     ITRIMAT = IFUNC - 11902 + 1
                     IPOS = 18
                     K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                     VALUE = MBUF%VAR(K + I)
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO
               ELSE
                  DO I=LFT,LLT
                     VALUE = ZERO
                     N     = I + NFT
                     IALEL = IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL /= 0 .AND. MLW == 20)THEN
                        LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                        LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                        VALUE1 = - (LBUF1%SIG(JJ(1) + I) + 
     .                       LBUF1%SIG(JJ(2) + I) + 
     .                       LBUF1%SIG(JJ(3) + I))*THIRD
                        VALUE2 = - (LBUF2%SIG(JJ(1) + I) + 
     .                       LBUF2%SIG(JJ(2) + I) + 
     .                       LBUF2%SIG(JJ(3) + I))*THIRD
                        VALUE = ZERO
                        IF(IFUNC == 11902)VALUE=VALUE1 
                        IF(IFUNC == 11903)VALUE=VALUE2                   
                     ENDIF
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO    
               ENDIF
            !PLASTIC STRAIN BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11906 .AND. IFUNC<=11909)THEN
              DO I=LFT,LLT
                  VALUE  = ZERO
                  VALUE1 = ZERO
                  VALUE2 = ZERO
                  N      = I + NFT
                  IALEL  = IPARG(7,NG)+IPARG(11,NG)
                  IF(IALEL /= 0 .AND. MLW == 20)THEN
                    LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                    LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                    IF(ELBUF_TAB(NG)%BUFLY(1)%L_PLA>0)VALUE1 = LBUF1%PLA(I)
                    IF(ELBUF_TAB(NG)%BUFLY(2)%L_PLA>0)VALUE2 = LBUF2%PLA(I)
                    VALUE  = ZERO
                    IF(IFUNC == 11906)VALUE=VALUE1 
                    IF(IFUNC == 11907)VALUE=VALUE2                   
                  ENDIF
                  FUNC(EL2FA(NN3+N)) = VALUE
              ENDDO  
            !SOUND SPEED BIMAT LAW20 - or future 2d law51 
           ELSEIF(IFUNC>=11910 .AND. IFUNC<=11913)THEN
              IF (MLW == 51) THEN
                 MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                 DO I = LFT, LLT
                    N = I + NFT
                    VALUE = ZERO
                    ITRIMAT = IFUNC - 11910 + 1
                    IPOS = 14
                    K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                    VALUE = MBUF%VAR(K + I)
                    FUNC(EL2FA(NN3+N)) = VALUE
                 ENDDO
              ELSE
                 DO I=LFT,LLT
                    VALUE = ZERO
                    N     = I + NFT
                    IALEL = IPARG(7,NG)+IPARG(11,NG)
                    IF(IALEL /= 0 .AND. MLW == 20)THEN
                       LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                       LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                       VALUE1 = LBUF1%SSP(I)
                       VALUE2 = LBUF2%SSP(I)
                       VALUE  = ZERO
                       IF(IFUNC == 11910)VALUE=VALUE1 
                       IF(IFUNC == 11911)VALUE=VALUE2                   
                    ENDIF
                    FUNC(EL2FA(NN3+N)) = VALUE
                 ENDDO  
              ENDIF
!VOLUME SPEED BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11914 .AND. IFUNC<=11917)THEN
               IF (MLW == 51) THEN
                  MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                  DO I = LFT, LLT
                     N = I + NFT
                     VALUE = ZERO
                     ITRIMAT = IFUNC - 11914 + 1
                     IPOS = 11
                     K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                     VALUE = MBUF%VAR(K + I)
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO
               ELSE
                  DO I=LFT,LLT
                     VALUE = ZERO
                     N     = I + NFT
                     IALEL = IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL /= 0 .AND. MLW == 20)THEN
                        LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                        LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                        VALUE1 = LBUF1%VOL(I)
                        VALUE2 = LBUF2%VOL(I)
                        VALUE  = ZERO
                        IF(IFUNC == 11914)VALUE=VALUE1 
                        IF(IFUNC == 11915)VALUE=VALUE2                   
                     ENDIF
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO 
               ENDIF
            !MASS BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11918 .AND. IFUNC<=11921)THEN
               IF (MLW == 51) THEN
                  MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                  DO I = LFT, LLT
                     N = I + NFT
                     VALUE = ZERO
                     ITRIMAT = IFUNC - 11918 + 1
                     IPOS = 0
                     K = LLT * (N0PHAS + (ITRIMAT - 1) * NVPHAS + IPOS - 1)
                     VALUE = MBUF%VAR(K + I)
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO
               ELSE
                  DO I=LFT,LLT
                     VALUE = ZERO
                     N     = I + NFT
                     IALEL = IPARG(7,NG)+IPARG(11,NG)
                     IF(IALEL /= 0 .AND. MLW == 20)THEN
                        LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                        LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                        VALUE1 = LBUF1%VOL(I) * LBUF1%RHO(I)
                        VALUE2 = LBUF2%VOL(I) * LBUF2%RHO(I)
                        VALUE  = ZERO
                        IF(IFUNC == 11918)VALUE=VALUE1 
                        IF(IFUNC == 11919)VALUE=VALUE2                   
                     ENDIF
                     FUNC(EL2FA(NN3+N)) = VALUE
                  ENDDO 
               ENDIF
            !ARTIFICIAL VISCOSITY BIMAT LAW20 - or future 2d law51 
            ELSEIF(IFUNC>=11922 .AND. IFUNC<=11925)THEN
              DO I=LFT,LLT
                  VALUE = ZERO
                  N     = I + NFT
                  IALEL = IPARG(7,NG)+IPARG(11,NG)
                  IF(IALEL /= 0 .AND. MLW == 20)THEN
                    LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                    LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                    VALUE1 = LBUF1%QVIS(I)
                    VALUE2 = LBUF2%QVIS(I) 
                    VALUE  = ZERO
                    IF(IFUNC == 11922)VALUE=VALUE1 
                    IF(IFUNC == 11923)VALUE=VALUE2                   
                  ENDIF
                  FUNC(EL2FA(NN3+N)) = VALUE
              ENDDO           
            ELSEIF(IFUNC == 13242 + 4*MX_PLY_ANIM )THEN
              IF(GBUF%G_DT>0)THEN
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = GBUF%DT(I) 
                ENDDO 
              ENDIF   
c------------------------------------ Mach Number
            ELSEIF(IFUNC == 13547 + 4*MX_PLY_ANIM + 1000 + 2)THEN                                
              IF (MLW == 151) THEN                                                                        
                 DO I = 1, NEL                                                                            
                    VEL(1) = MULTI_FVM%VEL(1, I + NFT)                                                    
                    VEL(2) = MULTI_FVM%VEL(2, I + NFT)                                                    
                    VEL(3) = MULTI_FVM%VEL(3, I + NFT)                                                    
                    VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                              
                    FUNC(EL2FA(NN3+NFT+I)) = VEL(0)/MULTI_FVM%SOUND_SPEED(I + NFT)                                      
                 ENDDO                                                                                    
              ELSEIF(ALEFVM_Param%ISOLVER>1)THEN                                                                    
                   L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                                                       
                   IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                            
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                          
                      DO  I=1,NEL                                                                         
                         VEL(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I)                                       
                         VEL(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)                                       
                         VEL(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)                                       
                         VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                                      
                         FUNC(EL2FA(NN3+NFT+I)) = VEL(0)/LBUF%SSP(I)                                                    
                      ENDDO                                                                               
                   ENDIF                                                                                   
              ELSE                                                                               
                L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                                                     
                IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                               
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)  
                  IF(IS_ALE /= 0)THEN
                  !ale                                             
                    DO  I=1,NEL                                                                             
                      TMP(1,1:4)=V(1,IXQ(2:5,I+NFT))-W(1,IXQ(2:5,I+NFT))                                                    
                      TMP(2,1:4)=V(2,IXQ(2:5,I+NFT))-W(2,IXQ(2:5,I+NFT))                                                    
                      TMP(3,1:4)=V(3,IXQ(2:5,I+NFT))-W(3,IXQ(2:5,I+NFT))                                                    
                      VEL(1) = SUM(TMP(1,1:4))*FOURTH                                                       
                      VEL(2) = SUM(TMP(2,1:4))*FOURTH                                                       
                      VEL(3) = SUM(TMP(3,1:4))*FOURTH                                                       
                      FUNC(EL2FA(NN3+NFT+I)) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I) 
                    ENDDO
                   ELSE
                   !euler and lagrange
                     DO  I=1,NEL 
                      TMP(1,1:4)=V(1,IXQ(2:5,I+NFT))                                                   
                      TMP(2,1:4)=V(2,IXQ(2:5,I+NFT))                                                   
                      TMP(3,1:4)=V(3,IXQ(2:5,I+NFT))                                                   
                      VEL(1) = SUM(TMP(1,1:4))*FOURTH                                                       
                      VEL(2) = SUM(TMP(2,1:4))*FOURTH                                                       
                      VEL(3) = SUM(TMP(3,1:4))*FOURTH                                                       
                      FUNC(EL2FA(NN3+NFT+I)) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I) 
                     ENDDO                                    
                   ENDIF
                ENDIF                                                                                     
              ENDIF              
c------------------------------------ Color Function
            ELSEIF(IFUNC == 13547 + 4*MX_PLY_ANIM + 1000 + 3)THEN                                
              GBUF => ELBUF_TAB(NG)%GBUF 
              IF (MLW == 151) THEN 
                 NFRAC=NLAY
                 DO IMAT=1,NLAY
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
                IMAT = IXQ(1,NFT+1)                        
                IADBUF = IPM(7,IMAT)                       
                NUPARAM= IPM(9,IMAT)                       
                UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM) 
                !bijective order           !indexes                  
                ISUBMAT = UPARAM(276+1);   IU(1)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                ISUBMAT = UPARAM(276+2);   IU(2)=N0PHAS+(ISUBMAT-1)*NVPHAS
                ISUBMAT = UPARAM(276+3);   IU(3)=N0PHAS+(ISUBMAT-1)*NVPHAS
                ISUBMAT = UPARAM(276+4);   IU(4)=N0PHAS+(ISUBMAT-1)*NVPHAS 
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
                VFRAC(1:NEL,1:21)=ZERO     
              ENDIF 
              IF(NFRAC>0)THEN
                DO I=1,NEL
                  VALUES(I)=ZERO
                  DO IMAT=1,NFRAC
                    VALUES(I) = VALUES(I) + VFRAC(I,IMAT)*IMAT
                  ENDDO
                  FUNC(EL2FA(NN3+NFT+I))=VALUES(I) 
                ENDDO           
              ELSE
                DO I=1,NEL
                  FUNC(EL2FA(NN3+NFT+I))=ZERO
                ENDDO
              ENDIF 
            ELSEIF(IFUNC == 4*MX_PLY_ANIM + 14566)THEN
                  IF(N2D == 1)THEN
                    FAC = TWO*3.141592653589793238  !axi
                  ELSE
                    FAC = ONE                         !plane stress
                  ENDIF
                  DO I = LFT, LLT
                     N = I + NFT
                     FUNC(EL2FA(NN3+N)) = FAC*GBUF%VOL(I)
                  ENDDO  
            !ELSEIF(IFUNC == 4*MX_PLY_ANIM + 14567)THEN 
            ! ...                       
            !ELSEIF(IFUNC == 4*MX_PLY_ANIM + 14568)THEN 
            ! ...                                                                                       
C-------------------------------------
            ELSE IF (IFUNC == 10676) THEN   
C-------------------------------------
              !SPMD DOMAIN
              DO  I=LFT,LLT                                         
                FUNC(EL2FA(NN3+NFT+I)) = ISPMD                       
              ENDDO  
C-------------------------------------
            ELSEIF (IFUNC == 14595 + 4*MX_PLY_ANIM  .AND. (GBUF%G_TSAIWU > 0)) THEN
C-------------------------------------
              ! /ANIM/ELEM/TSAIWU
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)             
              DO I=LFT,LLT
                DO IR=1,NPTR
                  DO IS=1,NPTS   
                    DO IT=1,NPTT
                      FUNC(EL2FA(NN3+NFT+I)) = 
     .                  FUNC(EL2FA(NN3+NFT+I)) 
     .                  + BUFLY%LBUF(IR,IS,IT)%TSAIWU(I)/(NPTT*NPTR*NPTS)
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
c------------------------------------ Tillotson Region id
              ! /ANIM/ELEM/TILLOTSON
            ELSEIF( IFUNC == 14596 + 4*MX_PLY_ANIM ) THEN
              DO I=LFT,LLT
                FUNC(EL2FA(NN3+NFT+I)) = ZERO
              ENDDO 
              MT = IXQ(1,NFT+1)  
              IF (MLW == 151) THEN 
                NLAY = ELBUF_TAB(NG)%NLAY 
                !count number of submaterial based on /EOS/TILLOTSON (IEOS=3)            
                NTILLOTSON = 0 
                DO IMAT=1,NLAY
                  IEOS =  IPM(4, IPM(20 + IMAT,MT) )
                  IF(IEOS == 3)THEN 
                    NTILLOTSON = NTILLOTSON + 1 
                    IMAT_TILLOTSON = IMAT 
                  ENDIF 
                ENDDO 
                !several Tillotson EoS   Value= sum ( Region_i*10**(i-1),  i=1,imat)       
                IF(NTILLOTSON > 1)THEN 
                  FAC=ONE
                  DO IMAT=1,NLAY
                    IEOS =  IPM(4,    IPM(20 + IMAT,MT) )
                    IF(IEOS == 3)THEN 
                      EBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%EOS(1,1,1) 
                      NVAREOS = ELBUF_TAB(NG)%BUFLY(IMAT)%NVAR_EOS
                      DO I=1,NEL
                        FUNC(EL2FA(NN3+NFT+I)) = FUNC(EL2FA(NN3+NFT+I)) + EBUF%VAR(I) * FAC
                      ENDDO 
                    ENDIF 
                    FAC=FAC*TEN 
                  ENDDO 
                !single Tillotson EoS   Value=  Region_i                                 
                ELSEIF(NTILLOTSON == 1)THEN
                      EBUF => ELBUF_TAB(NG)%BUFLY(IMAT_TILLOTSON)%EOS(1,1,1)
                      NVAREOS = ELBUF_TAB(NG)%BUFLY(IMAT_TILLOTSON)%NVAR_EOS 
                      DO I=1,NEL
                        FUNC(EL2FA(NN3+NFT+I)) = EBUF%VAR(I)
                      ENDDO 
                ENDIF 
              ELSE 
                !monomaterial law                                                        
                IEOS = IPM(4,MT)
                IF(IEOS == 3)THEN
                  EBUF => ELBUF_TAB(NG)%BUFLY(1)%EOS(1,1,1)                              
                  NVAREOS = ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS                              
                  DO  I=1,NEL
                    FUNC(EL2FA(NN3+NFT+I)) = EBUF%VAR(I)
                  ENDDO
                ENDIF
              ENDIF              
c------------------------------------ 
            ELSE                                                      
              DO  I=LFT,LLT                                           
                FUNC(EL2FA(NN3+NFT+I)) = ZERO                         
              ENDDO                                                   
            ENDIF                                                     
C-----------------------------------------------
C           COQUES 3 N 4 N
C-----------------------------------------------
        ELSEIF (ITY == 3.OR.(ITY == 7.AND.N2D==0)) THEN
            IF(ITY == 3)THEN
               NNI = NN4
            ELSE
               NNI = NN5
            ENDIF
            GBUF => ELBUF_TAB(NG)%GBUF
            NPT   = IPARG(6,NG)
            IHBE  = IPARG(23,NG)
            IREP  = IPARG(35,NG)
            IGTYP = IPARG(38,NG)
            ITHK  = IPARG(28,NG)
            MPT   = IABS(NPT)
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
            NPTT  = ELBUF_TAB(NG)%NPTT
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPG   = NPTR*NPTS
            NUVAR = 0
            IPINCH= IPARG(90,NG)
            IF (IHBE==3.AND.ISH3NFRAM==0) THEN
             IFRAM_OLD =0
            ELSE
             IFRAM_OLD =1
            END IF
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IPT=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
              ENDDO
              IF (NLAY == 1) MPT  = MAX(1,NPT_ALL)
            ENDIF
C---------------------    
            DO I=LFT,LLT
              EVAR(I) = ZERO   ! Default = zero in all cases !
            ENDDO
C---------------------
C
            IF (MLW == 0 .OR. MLW == 13) THEN
c---
            ELSEIF (IFUNC == 1 .AND. (MLW /= 15 .AND. MLW /= 25)) THEN   ! plastic strain (mid layer : npt/2 + 1) 
! law15 and law25 it's the plastic work instead plastic strain. it can be outp using /ANIM/ELEM/WPLA ( ifunc=4*MX_PLY_ANIM +13245)           
              IF (GBUF%G_PLA > 0) THEN
                IF (NLAY > 1) THEN
                  IPT = IABS(NLAY)/2 + 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                  IF (BUFLY%L_PLA > 0)THEN
                  IF (NPG > 1) THEN
                    DO  I=LFT,LLT  
                      EVAR(I) = ABS(BUFLY%PLAPT(I))
                    ENDDO
                  ELSE
                    NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                    DO  I=LFT,LLT
                      DO IT=1,NPTT
                        EVAR(I) = EVAR(I) + ABS(BUFLY%LBUF(1,1,IT)%PLA(I))/NPTT
                      ENDDO
                    ENDDO
                  ENDIF
                 ENDIF
                ELSEIF (NPG > 1) THEN   ! NLAY = 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                  IPT = IABS(NPTT)/2 + 1
cc                  IPT = IABS(NPT)/2 + 1
                  II = (IPT-1)*NEL  
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%PLAPT(II+I))
                  ENDDO
                 ENDIF
                ELSE   !  NLAY=1, NPG=1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
cc                  IPT = IABS(NPT)/2 + 1
                  IPT = IABS(NPTT)/2 + 1
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I)) 
                  ENDDO
                 ENDIF
                ENDIF
              ENDIF  
                          
c---
            ELSEIF (IFUNC == 2 .AND. MLW == 151) THEN
              DO I=LFT,LLT
                EVAR(I) = GBUF%RHO(I) 
              ENDDO               
c---
            ELSEIF (IFUNC == 3 .AND. MLW == 151) THEN
              DO I=LFT,LLT
                EVAR(I) = GBUF%EINT(I) / GBUF%RHO(I) 
              ENDDO
c---
            ELSEIF (IFUNC == 3 .OR. IFUNC == ISH_EINT) THEN   ! EINT
              DO I=LFT,LLT
cc                K1 = 2 * I 
cc                K2 = K1 - 1
!!                K1 = 2*(I-1)+1
!!                K2 = 2*(I-1)+2
                EVAR(I) = GBUF%EINT(I) + GBUF%EINT(I+LLT)
              ENDDO
c---
            ELSEIF (IFUNC == 4) THEN   ! TEMP
              DO I = LFT, LLT
                EVAR(I) = ZERO
              ENDDO
              NPGT = NPG*NPTT
              DO IL=1,NLAY                                              
                IF(ELBUF_TAB(NG)%BUFLY(IL)%L_TEMP>0) THEN
                  DO IS=1,NPTS                                            
                    DO IT=1,NPTT                                          
                      DO IR=1,NPTR    
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                        DO I=LFT,LLT
                          EVAR(I) = EVAR(I) + LBUF%TEMP(I)/NPGT
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDIF
              ENDDO            
c---
            ELSEIF (IFUNC == 5) THEN   ! THK
             IF (ITHK >0) THEN
              DO I=LFT,LLT
                EVAR(I) = GBUF%THK(I)
              ENDDO
             ELSE
              IF (ITY == 3) THEN ! SHELL
                DO I=LFT,LLT
                 EVAR(I) = THKE(NFT+I)
                ENDDO
              ELSEIF (ITY == 7) THEN ! SH3N
                DO I=LFT,LLT
                  EVAR(I) = THKE(NFT+I+NUMELC)
                ENDDO
              ENDIF ! IF (ITY == 3)
             END IF
c---
            ELSEIF (IFUNC == 6 .AND. MLW == 151) THEN
              DO I=LFT,LLT
                EVAR(I) = - THIRD * (GBUF%SIG(I) + GBUF%SIG(I + NEL) + GBUF%SIG(I + 2 * NEL))
              ENDDO
c---
            ELSEIF (IFUNC == 7) THEN   ! Von Mises
              DO I=LFT,LLT
                S1 = GBUF%FOR(JJ(1)+I)
                S2 = GBUF%FOR(JJ(2)+I)
                S12= GBUF%FOR(JJ(3)+I)
                VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                EVAR(I) = SQRT(VONM2)
              ENDDO
c---
            ELSEIF (IFUNC == 11) THEN ! DAM1
c              IF (MLW == 25 .OR. MLW == 15 )THEN 
c              NB13 = NB12 + 2*NEL*MAX(1,NPT)
c              NB15 = NB12 + 2*NEL*MAX(1,NPT)
c              NB12 = NB12 + 2*OFFSET*MAX(1,NPT)
c              NB13 = NB13 + 4*OFFSET*MAX(1,NPT)
c              DO J=1,NPT
c                N = (IPT-1)*NEL
c                DO I=LFT,LLT
c                  K1 = NB13 + N+I
c                  K2 = NB15 + 2*N+2*I-1 
c                  EVAR(I)=EVAR(I)+BUFEL(K1)*BUFEL(K2)   
c                ENDDO 
c              ENDDO
c             ENDIF
c---
            ELSEIF(IFUNC == 12)THEN ! DAM2 
c             IF(MLW == 25.OR.MLW == 15)THEN 
c              NB13 = NB12 + 2*NEL*MAX(1,NPT)
c              NB15 = NB12 + 2*NEL*MAX(1,NPT)
c              NB12 = NB12 + 2*OFFSET*MAX(1,NPT)
c              NB13 = NB13 + 4*OFFSET*MAX(1,NPT)
c              DO J=1,NPT
c                N = (IPT-1)*NEL
c                DO I=LFT,LLT
c                  K1 = NB13 + N+I
c                  K2 = NB15 + 2*N+2*I 
c
c                ENDDO 
c              ENDDO
c             ENDIF
c---
            ELSEIF(IFUNC == 13)THEN ! DAM3
c              IF(MLW == 25.OR.MLW == 15)THEN 
c                IADD  = NB12 + 6*NEL*MAX(1,NPT)
c                IADD  = IADD + OFFSET
c                DO I=LFT,LLT
c                  EVAR(I) = BUFEL(IADD+I)   
c                ENDDO
c              ENDIF
c---
            ELSEIF (IFUNC >= 14 .AND. IFUNC <= 15) THEN   ! Sigx, Sigy - global
              IUS = IFUNC-13
              DO I=LFT,LLT                    
                EVAR(I) = GBUF%FOR(JJ(IUS)+I)  
              ENDDO    
c---
            ELSEIF (IFUNC == 16 .AND. IHBE == 11 .AND. IPINCH == 1) THEN   ! Sigz for pinching shell
              DO I=LFT,LLT
                EVAR(I) = ZERO
                DO IPG=1,4 
                  EVAR(I) = EVAR(I) + FOURTH*GBUF%FORPGPINCH(NEL*(IPG-1)+I)
                ENDDO  
              ENDDO                     
c---       
            ELSEIF (IFUNC >= 17 .AND. IFUNC <= 19) THEN   ! Sigyx, Sigyz, Sigzx - global
              IUS = IFUNC-14
              DO I=LFT,LLT                    
                EVAR(I) = GBUF%FOR(JJ(IUS)+I)  
              ENDDO                           
c---
            ELSEIF (IFUNC == 26 .and. GBUF%G_EPSD > 0) THEN
              IF (MLW == 104) THEN 
                DO I = LFT, LLT
                  EVAR(I) = ZERO
                ENDDO
                NPGT = NPG*NPTT
                DO IL=1,NLAY                                              
                  DO IS=1,NPTS                                            
                    DO IT=1,NPTT                                          
                      DO IR=1,NPTR    
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                        DO I=LFT,LLT
                          EVAR(I) = EVAR(I) + LBUF%EPSD(I)/NPGT
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO              
              ELSE 
                DO I=LFT,LLT
                  EVAR(I) = GBUF%EPSD(I)
                ENDDO
              ENDIF
c---
            ELSEIF(IFUNC == 2155)THEN
              DO I=LFT,LLT
                EVAR(I) = HUNDRED *(GBUF%THK_I(I)-GBUF%THK(I))/GBUF%THK_I(I)
              ENDDO
c------------------------------------
            ELSEIF (IFUNC>=20 .AND. IFUNC<=24) THEN
C             USER VARIABLES from 1 to 5
c------------------------------------
              IF (MLW==29 .OR. MLW==30 .OR. MLW==31 .OR. MLW>=33) THEN
c
                IUS = IFUNC - 20
                I1  = IUS*NEL                                                              
                IF (NLAY > 1) THEN                                                         
                  IL  = IABS(NLAY)/2 + 1                                                   
                  IPT = 1                                                                  
                ELSE                                                                       
                  IL  = 1                                                                  
                  IPT =  IABS(NPT)/2 + 1                                                   
                ENDIF                                                                      
                NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT                                   
c
                IF (MLW == 58 .or. MLW == 158) THEN                                                        
                  DO I=LFT,LLT                                                             
                    DO IR = 1, NPTR                                                        
                     DO IS = 1, NPTS                                                       
                      UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR                     
                      IF(IUS==3.OR.IUS==4)THEN                                             
                       EVAR(I) = EVAR(I) + LOG(UVAR(I1 + I)+ONE)/NPG                       
                      ELSE                                                                 
                       EVAR(I) = EVAR(I) + UVAR(I1 + I)/NPG                               
                      ENDIF                                                                
                     ENDDO                                                                 
                    ENDDO                                                                  
                  ENDDO                                                                    
                ELSE                                                                       
                 DO I=LFT,LLT                                                              
                   IF (NUVAR > IUS) THEN                                                  
                     DO IR = 1, NPTR                                                       
                      DO IS = 1, NPTS                                                      
                       UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR                    
                       EVAR(I) = EVAR(I) + UVAR(I1 + I)/NPG                               
                      ENDDO
                     ENDDO                                                                 
                   ENDIF                                                                   
                 ENDDO                                                                     
                ENDIF !MLW=58                                                              
              ENDIF
c------------------------------------
            ELSEIF(IFUNC >= 27 .AND. IFUNC < 40) THEN
c             USER VARIABLES from 6 to 60 (mid-layer)
c------------------------------------
              IF (MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.MLW>=33) THEN 
                IUS = IFUNC - 22                                  
                IF (NLAY > 1) THEN                                      
                  IL  = IABS(NLAY)/2 + 1
                  IPT = 1                                               
                ELSE                                                    
                  IL  = 1                                               
                  IPT = IABS(NPT)/2 + 1
                ENDIF                                                   
                NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT 
                IF (NUVAR > IUS .and. NPT >= IPT*IL) THEN                                
                  I1  = IUS*NEL
                  DO I=LFT,LLT                                            
                    DO IR = 1, NPTR                                     
                      DO IS = 1, NPTS                                    
                        UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR
                        EVAR(I) = EVAR(I) + UVAR(I1 + I)/NPG             
                      ENDDO                                              
                    ENDDO                                               
                  ENDDO                                                   
                ENDIF                                                 
              ENDIF
c------------------------------------
            ELSEIF((IFUNC > 39   .AND. IFUNC < 2040) .OR.
     .             (IFUNC > 2239 .AND. IFUNC < 10140)) THEN  ! /USRi/LAYER
c------------------------------------
              IF (IFUNC > 39 .and. IFUNC < 2040) THEN
                IUS = (IFUNC - 39)/100
                IPT = MOD ((IFUNC - 39), 100)
              ELSEIF (IFUNC > 2239 .AND. IFUNC < 10140) THEN 
                IUS = ((IFUNC - 2239)/100) + 20
                IPT = MOD ((IFUNC - 2239), 100)
              ENDIF  
              IF (IPT == 0) THEN
                IPT = 100
                IUS = IUS -1 
              ENDIF
              IF (NLAY > 1) THEN
                IL  = IPT
                IPT = 1
              ELSE
                IL  = 1
              ENDIF
              NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT                   
              IF (NUVAR > IUS .and. (NPT >= IPT*IL)) THEN
                I1 = IUS*NEL                                              
                DO I=LFT,LLT                                               
                  DO IR = 1, NPTR                                            
                    DO IS = 1, NPTS                                          
                      UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR       
                      EVAR(I) = EVAR(I) + UVAR(I1 + I)/NPG                   
                    ENDDO                                                    
                  ENDDO                                                      
                ENDDO                                                      
              ENDIF   ! USER LAW                                        
c------------------------------------
            ELSEIF( (IFUNC>=10140.AND.IFUNC<=10239) 
     .             .OR. IFUNC == 10673.OR. IFUNC == 10674
     .             .OR. IFUNC == 10675 ) THEN
              IF (IFUNC == 10673) THEN      ! phi Output : Mid Layer
                IL = IABS(NLAY)/2 + 1
              ELSEIF (IFUNC == 10674) THEN  ! phi Output : Upper Layer
                IL = NLAY
              ELSEIF (IFUNC == 10675) THEN  ! phi Output : Lower Layer
                IL = 1
              ELSE
                IL = IFUNC - 10139          ! phi Output : layer 1->99
              ENDIF 
c------------------------------------            
c              
              IF (IL <= NLAY) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                IF (ITY == 3) THEN
                  IF (IGTYP == 9  .OR. IGTYP == 10 .OR.IGTYP == 11 .OR.
     .                IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR.
     .                IGTYP == 52 ) THEN
                    IF (MLW /= 0 .AND. MLW /= 13) THEN 
                        
                     IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                      LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1) 
                      DO I=LFT,LLT                                                                     
                        N = I + NFT                                                                    
                        X21 = X(1,IXC(3,N))-X(1,IXC(2,N))                         
                        X32 = X(1,IXC(4,N))-X(1,IXC(3,N))                         
                        X34 = X(1,IXC(4,N))-X(1,IXC(5,N))                         
                        X41 = X(1,IXC(5,N))-X(1,IXC(2,N))                         

                        Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))                         
                        Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))                         
                        Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))                         
                        Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))                         

                        Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))                         
                        Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))                         
                        Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))                         
                        Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))                         
                                                                                  
                        E1X = (X21+X34)                                           
                        E1Y = (Y21+Y34)                                           
                        E1Z = (Z21+Z34)                                           

                        E2X = (X32+X41)                                           
                        E2Y = (Y32+Y41)                                           
                        E2Z = (Z32+Z41)                                           
                 
                        E3X = E1Y*E2Z-E1Z*E2Y                                     
                        E3Y = E1Z*E2X-E1X*E2Z                                     
                        E3Z = E1X*E2Y-E1Y*E2X                                     
                        IF (IREP > 0) THEN                                        
                          RX = E1X                                                
                          RY = E1Y                                                
                          RZ = E1Z                                                
                          SX = E2X                                                
                          SY = E2Y                                                
                          SZ = E2Z                                                
                        ENDIF                                    
                        IF (ISHFRAM == 0 ) THEN                  
C------                   Repere convecte symetrique - version 5 (default)        
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = SQRT(S1/S2)                                    
                          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                      
                          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                      
                          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                      
C
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E1X = E1X * SUMA                                        
                          E1Y = E1Y * SUMA                                        
                          E1Z = E1Z * SUMA                                        
C
                          E2X = E3Y * E1Z - E3Z * E1Y                             
                          E2Y = E3Z * E1X - E3X * E1Z                             
                          E2Z = E3X * E1Y - E3Y * E1X                             
                        ELSEIF (ISHFRAM == 2) THEN                                
C------                   Repere convecte nonsymetrique - version 4               
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y                        
                          E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z                        
                          E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X                        
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E1X = E1X*SUMA                                          
                          E1Y = E1Y*SUMA                                          
                          E1Z = E1Z*SUMA                                          
C
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          E2X = E3Y*E1Z-E3Z*E1Y                                   
                          E2Y = E3Z*E1X-E3X*E1Z                                   
                          E2Z = E3X*E1Y-E3Y*E1X                                   
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E2X = E2X*SUMA                                          
                          E2Y = E2Y*SUMA                                          
                          E2Z = E2Z*SUMA                                          
                        ENDIF                                                     
                        IF (IREP >= 1) THEN                                       
                          AA = LBUF_DIR%DIRA(I)                              
                          BB = LBUF_DIR%DIRA(I+NEL)                       
                          V1 = AA*RX + BB*SX                                      
                          V2 = AA*RY + BB*SY                                      
                          V3 = AA*RZ + BB*SZ                                      
                          VR = V1*E1X+ V2*E1Y + V3*E1Z                            
                          VS = V1*E2X+ V2*E2Y + V3*E2Z                            
                          SUMA=SQRT(VR*VR + VS*VS)                                
                          DIR1_1 = VR/SUMA                                        
                          DIR1_2 = VS/SUMA                                        
                        ELSE                                                      
                          DIR1_1 = LBUF_DIR%DIRA(I)    			      
                          DIR1_2 = LBUF_DIR%DIRA(I+NEL)			      
                        ENDIF                                       
C
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO
                        !!EVAR(I) = (HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                      ENDDO                                
                     ELSE                  
                      DO I=LFT,LLT                                                
                        N = I + NFT                                               
                        X21 = X(1,IXC(3,N))-X(1,IXC(2,N))                         
                        X32 = X(1,IXC(4,N))-X(1,IXC(3,N))                         
                        X34 = X(1,IXC(4,N))-X(1,IXC(5,N))                         
                        X41 = X(1,IXC(5,N))-X(1,IXC(2,N))                         

                        Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))                         
                        Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))                         
                        Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))                         
                        Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))                         

                        Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))                         
                        Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))                         
                        Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))                         
                        Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))                         
                                                                                  
                        E1X = (X21+X34)                                           
                        E1Y = (Y21+Y34)                                           
                        E1Z = (Z21+Z34)                                           

                        E2X = (X32+X41)                                           
                        E2Y = (Y32+Y41)                                           
                        E2Z = (Z32+Z41)                                           
                 
                        E3X = E1Y*E2Z-E1Z*E2Y                                     
                        E3Y = E1Z*E2X-E1X*E2Z                                     
                        E3Z = E1X*E2Y-E1Y*E2X                                     
                        IF (IREP > 0) THEN                                        
                          RX = E1X                                                
                          RY = E1Y                                                
                          RZ = E1Z                                                
                          SX = E2X                                                
                          SY = E2Y                                                
                          SZ = E2Z                                                
                        ENDIF                                    
                        IF (ISHFRAM == 0 .OR. IGTYP == 16 ) THEN                  
C------                   Repere convecte symetrique - version 5 (default)        
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = SQRT(S1/S2)                                    
                          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                      
                          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                      
                          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                      
C
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E1X = E1X * SUMA                                        
                          E1Y = E1Y * SUMA                                        
                          E1Z = E1Z * SUMA                                        
C
                          E2X = E3Y * E1Z - E3Z * E1Y                             
                          E2Y = E3Z * E1X - E3X * E1Z                             
                          E2Z = E3X * E1Y - E3Y * E1X                             
                        ELSEIF (ISHFRAM == 2) THEN                                
C------                   Repere convecte nonsymetrique - version 4               
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y                        
                          E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z                        
                          E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X                        
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E1X = E1X*SUMA                                          
                          E1Y = E1Y*SUMA                                          
                          E1Z = E1Z*SUMA                                          
C
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          E2X = E3Y*E1Z-E3Z*E1Y                                   
                          E2Y = E3Z*E1X-E3X*E1Z                                   
                          E2Z = E3X*E1Y-E3Y*E1X                                   
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E2X = E2X*SUMA                                          
                          E2Y = E2Y*SUMA                                          
                          E2Z = E2Z*SUMA                                          
                        ENDIF                                                     
                        IF (IREP >= 1) THEN                                       
                          AA = BUFLY%DIRA(I)                              
                          BB = BUFLY%DIRA(I+NEL)                       
                          V1 = AA*RX + BB*SX                                      
                          V2 = AA*RY + BB*SY                                      
                          V3 = AA*RZ + BB*SZ                                      
                          VR = V1*E1X+ V2*E1Y + V3*E1Z                            
                          VS = V1*E2X+ V2*E2Y + V3*E2Z                            
                          SUMA=SQRT(VR*VR + VS*VS)                                
                          DIR1_1 = VR/SUMA                                        
                          DIR1_2 = VS/SUMA                                        
                        ELSE                                                      
                          DIR1_1 = BUFLY%DIRA(I)    			      
                          DIR1_2 = BUFLY%DIRA(I+NEL)			      
                        ENDIF                                       
C
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO
                        !!EVAR(I) = (HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                      ENDDO    
                     ENDIF ! IDRAPE                                                    
                    ENDIF  ! MLW                                                  
                  ENDIF  ! IGTYP

                ELSEIF (ITY == 7) THEN
                  NPG = IPARG(48,NG)
                  IF (IGTYP == 9  .OR. IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .                IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR.
     .                IGTYP == 52 ) THEN
                    IF (MLW /= 0 .AND. MLW /= 13) THEN
                     IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                     LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1)
                      DO I=LFT,LLT                                                             
                        N = I + NFT                                             
                        X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))                     
                        X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                     
                        X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                     

                        Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))                     
                        Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))                     
                        Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))                     

                        Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))                     
                        Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))                     
                        Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))                     
                        IF (IREP > 0) THEN                                      
                          E11 = X21                                             
                          E12 = Y21                                             
                          E13 = Z21                                             
                          E21 = X31                                             
                          E22 = Y31                                             
                          E23 = Z31                                             
                        ENDIF                                                   
                        IF(IFRAM_OLD ==0 ) THEN
	                    CALL CLSCONV3(X21,Y21,Z21,X31,Y31,Z31,
     +	                          E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z)
                       ELSE		
                        E1X= X21                                                
                        E1Y= Y21                                                
                        E1Z= Z21                                                
                        X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)                     
                        E1X=E1X/X2L                                             
                        E1Y=E1Y/X2L                                             
                        E1Z=E1Z/X2L                                             
C
                        E3X=Y31*Z32-Z31*Y32                                     
                        E3Y=Z31*X32-X31*Z32                                     
                        E3Z=X31*Y32-Y31*X32                                     
                        SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)                     
                        E3X=E3X/SUM_                                             
                        E3Y=E3Y/SUM_                                             
                        E3Z=E3Z/SUM _                                            
                        AREA = HALF * SUM_                                    
                        E2X=E3Y*E1Z-E3Z*E1Y                                     
                        E2Y=E3Z*E1X-E3X*E1Z                                     
                        E2Z=E3X*E1Y-E3Y*E1X                                     
                        SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)                     
                        E2X=E2X/SUM_                                             
                        E2Y=E2Y/SUM_                                             
                        E2Z=E2Z/SUM_                                             
                      END IF!(ISH3NFRAM ==0 ) THEN
                        IF (IREP >= 1) THEN                                     
                          AA = LBUF_DIR%DIRA(I)				    
                          BB = LBUF_DIR%DIRA(I+NEL)			    
                          V1 = AA*E11 + BB*E21                                  
                          V2 = AA*E12 + BB*E22                                  
                          V3 = AA*E13 + BB*E23                                  
                          VR = V1*E1X + V2*E1Y + V3*E1Z                         
                          VS = V1*E2X + V2*E2Y + V3*E2Z                         
                          SUMA=SQRT(VR*VR + VS*VS)                              
                          DIR1_1 = VR/SUMA                                      
                          DIR1_2 = VS/SUMA                                      
                        ELSE                                                    
                          DIR1_1 = LBUF_DIR%DIRA(I)    			    
                          DIR1_2 = LBUF_DIR%DIRA(I+NEL)			    
                        ENDIF                                         
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO          
                      ENDDO                                                  
                     ELSE  ! IDRAPE                    
                      DO I=LFT,LLT                                              
                        N = I + NFT                                             
                        X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))                     
                        X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                     
                        X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                     

                        Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))                     
                        Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))                     
                        Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))                     

                        Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))                     
                        Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))                     
                        Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))                     
                        IF (IREP > 0) THEN                                      
                          E11 = X21                                             
                          E12 = Y21                                             
                          E13 = Z21                                             
                          E21 = X31                                             
                          E22 = Y31                                             
                          E23 = Z31                                             
                        ENDIF                                                   
                        IF(IFRAM_OLD ==0 ) THEN
	                    CALL CLSCONV3(X21,Y21,Z21,X31,Y31,Z31,
     +	                          E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z)
                       ELSE		
                        E1X= X21                                                
                        E1Y= Y21                                                
                        E1Z= Z21                                                
                        X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)                     
                        E1X=E1X/X2L                                             
                        E1Y=E1Y/X2L                                             
                        E1Z=E1Z/X2L                                             
C
                        E3X=Y31*Z32-Z31*Y32                                     
                        E3Y=Z31*X32-X31*Z32                                     
                        E3Z=X31*Y32-Y31*X32                                     
                        SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)                     
                        E3X=E3X/SUM_                                             
                        E3Y=E3Y/SUM_                                             
                        E3Z=E3Z/SUM _                                            
                        AREA = HALF * SUM_                                    
                        E2X=E3Y*E1Z-E3Z*E1Y                                     
                        E2Y=E3Z*E1X-E3X*E1Z                                     
                        E2Z=E3X*E1Y-E3Y*E1X                                     
                        SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)                     
                        E2X=E2X/SUM_                                             
                        E2Y=E2Y/SUM_                                             
                        E2Z=E2Z/SUM_                                             
                      END IF!(ISH3NFRAM ==0 ) THEN
                        IF (IREP >= 1) THEN                                     
                          AA = BUFLY%DIRA(I)				    
                          BB = BUFLY%DIRA(I+NEL)			    
                          V1 = AA*E11 + BB*E21                                  
                          V2 = AA*E12 + BB*E22                                  
                          V3 = AA*E13 + BB*E23                                  
                          VR = V1*E1X + V2*E1Y + V3*E1Z                         
                          VS = V1*E2X + V2*E2Y + V3*E2Z                         
                          SUMA=SQRT(VR*VR + VS*VS)                              
                          DIR1_1 = VR/SUMA                                      
                          DIR1_2 = VS/SUMA                                      
                        ELSE                                                    
                          DIR1_1 = BUFLY%DIRA(I)    			    
                          DIR1_2 = BUFLY%DIRA(I+NEL)			    
                        ENDIF                                         
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO          
                      ENDDO  
                     ENDIF ! IDRAPE                                                    
                    ENDIF                                                       
                  ENDIF              
                ENDIF
              ELSE
                DO I=LFT,LLT 
                    EVAR(I) = ZERO
                ENDDO 
              ENDIF  ! IL
c------------------------------------
            ELSEIF (IFUNC == 2040 .AND. MLW /= 15 .AND. MLW /= 25) THEN  ! EPSP/UPPER
c------------------------------------
              IF (NLAY > 1) THEN                   
                IL  = MAX(1,NPT)            
                IPT = 1                            
              ELSE                                 
                IL  = 1                            
                IPT = MAX(1,NPT)
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              IF (BUFLY%L_PLA > 0) THEN
                IF (NPG > 1) THEN
                  IF (IGTYP == 51 .OR. IGTYP == 52) IPT = BUFLY%NPTT
                  DO I=LFT,LLT
                     DO IR=1,NPTR
                       DO IS=1,NPTS
                         LBUF => BUFLY%LBUF(IR,IS,IPT)
                         EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                       ENDDO
                     ENDDO
                  ENDDO
                ELSE
                  IF (IGTYP == 51 .OR. IGTYP == 52) IPT = BUFLY%NPTT
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))
                  ENDDO
                ENDIF
              ENDIF 
c------------------------------------
            ELSEIF (IFUNC == 2041 .AND. MLW /= 15 .AND. MLW /= 25) THEN ! EPSP/LOWER
c------------------------------------
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)
              IF (BUFLY%L_PLA > 0) THEN
                IF (NPG > 1) THEN
                   DO I=LFT,LLT
                     DO IR=1,NPTR
                       DO IS=1,NPTS
                         LBUF => BUFLY%LBUF(IR,IS,1)
                         EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                       ENDDO
                     ENDDO
                   ENDDO
                ELSE
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%LBUF(1,1,1)%PLA(I))
                  ENDDO
                ENDIF
              ENDIF 
c------------------------------------
            ELSEIF (IFUNC > 2041 .AND. IFUNC < 2142 .AND. MLW /= 15 .AND. MLW /= 25) THEN  ! anim/shell/epsp/layer
c------------------------------------
              ILAY = MOD((IFUNC - 2041), 100)
              IF (ILAY == 0) ILAY = 100
              IF ((ILAY <= NLAY .or. ILAY <= MPT) .and. GBUF%G_PLA > 0) THEN
                IF (NPT == 0) THEN
                  IL  = 1 
                  IPT = 1
                ELSEIF (NLAY > 1) THEN                   
                  IL = ILAY
                  IPT = 1                            
                ELSE                                 
                  IL  = 1                            
                  IPT = ILAY
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                IF (BUFLY%L_PLA > 0) THEN
                   IF (NPG > 1) THEN
                     IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
C                       PROP/51 : more than 1 integration point by layer: average value per layer 
                        NPTT = BUFLY%NPTT
                        NPGT = NPG*NPTT
                        DO I=LFT,LLT
                          DO IT=1,NPTT
                            DO IR=1,NPTR
                              DO IS=1,NPTS
                                LBUF => BUFLY%LBUF(IR,IS,IT)
                                EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPGT
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO
                      ELSE
                        DO I=LFT,LLT
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              LBUF => BUFLY%LBUF(IR,IS,IPT)
                              EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDIF 
                   ELSE
                     IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                        NPTT = BUFLY%NPTT
                        DO I=LFT,LLT
                          DO IT=1,NPTT
                           EVAR(I) = EVAR(I) + ABS(BUFLY%LBUF(1,1,IT)%PLA(I))/NPTT
                          ENDDO
                        ENDDO
                      ELSE
                        DO I=LFT,LLT
                           EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))
                        ENDDO
                      ENDIF 
                  ENDIF
                 ENDIF  
               ENDIF 
c------------------------------------
            ELSEIF (IFUNC == 10253.OR.IFUNC == 10254.OR.IFUNC == 10255)THEN
C             NXT failure model output
C             Max values: Damage factor +  normalized stress
c------------------------------------
              IF (IFUNC == 10253) THEN  ! output Damage
                DO IL=1,NLAY                                                             
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                           
                    DO IT=1,NPTT                                                         
                      DO IR=1,NPTR                                                       
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)
                        DO IFAIL=1,NFAIL
                          IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model
                            DO I=LFT,LLT                                                   
                              EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))                          
                            ENDDO
                          ENDIF                                                          
                        ENDDO                                                            
                      ENDDO                                                              
                    ENDDO                                                                
                  ENDDO                                                                  
                ENDDO
              ELSEIF (IFUNC == 10254) THEN  ! output Sig1  
                DO IL=1,NLAY                                                              
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                            
                    DO IT=1,NPTT                                                          
                      DO IR=1,NPTR                                                       
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                
                        DO IFAIL=1,NFAIL                                              
                          IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model    
                            NVARF = FBUF%FLOC(IFAIL)%NVAR                             
                            DO I=LFT,LLT                                              
                              VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+1)  ! Sig1                                              
                              EVAR(I) = MAX(EVAR(I), VAR)                               
                            ENDDO                                                     
                          ENDIF                                                          
                        ENDDO                                                             
                      ENDDO                                                               
                    ENDDO                                                                 
                  ENDDO                                                                  
                ENDDO                                                                 
              ELSEIF (IFUNC == 10255) THEN  ! output Sig2  
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IT=1,NPTT                                                                
                      DO IR=1,NPTR                                                              
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                          
                        DO IFAIL=1,NFAIL                                                        
                          IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model              
                            NVARF = FBUF%FLOC(IFAIL)%NVAR                                       
                            DO I=LFT,LLT                                                        
                              VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+2)  ! Sig2                                             
                              EVAR(I) = MAX(EVAR(I), VAR)                                       
                            ENDDO                                                               
                          ENDIF                                                                 
                        ENDDO                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ENDIF                                                                 
C--------------------------------------------------
            ELSE IF (IFUNC >= 10360 .and. IFUNC <= 10668) THEN  
C--------------------------------------------------
C             NXT failure model output par layer : upper/lower/membrane
C             Damage factor +  normalized stress
c---
              IF (IFUNC == 10360) THEN ! Damage factor - upper
                IF (NLAY > 1) THEN
                  IL  = NLAY
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT
                ENDIF
cc                DO IL=1,NLAY      !  loop unneeded for the upper layer only                                                              
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    DO IT=1,NPTT
                      IPT = IT
                      IF (NLAY == 1) IPT = NPTT
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          DO I=LFT,LLT                                                          
                            EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))                          
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO
                    ENDDO                                                                     
                  ENDDO                                                                       
                ENDDO                                                                         
cc                ENDDO
              ELSEIF (IFUNC == 10361) THEN ! Damage factor - lower
                IPT = 1
                IL  = 1
cc                DO IL=1,NLAY         !  loop unneeded for the lower layer only                                                                 
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    DO IT=1,NPTT
                      IPT = IT
                      IF (NLAY == 1) IPT = 1
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          DO I=LFT,LLT                                                          
                            EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))                          
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO
                  ENDDO                                                                       
                ENDDO                                                                         
cc                ENDDO
              ELSEIF (IFUNC == 10362) THEN ! Damage factor - membrane
                IF (NLAY > 1) THEN
                  IL  = IABS(NLAY) / 2
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = IABS(NPTT) / 2
                ENDIF
cc                DO IL=1,NLAY     !  loop unneeded for the membrane layer only                                               
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    DO IT=1,NPTT
                      IPT = IT
                      IF (NLAY == 1) IPT = IABS(NPTT) / 2
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          DO I=LFT,LLT                                                          
                            EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))                          
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO
                  ENDDO                                                                       
                ENDDO                                                                         
cc                ENDDO
              ELSEIF (IFUNC == 10363) THEN ! Sig1 - upper
                IF (NLAY > 1) THEN
                  IL  = NLAY
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT
                ENDIF
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          NVARF = FBUF%FLOC(IFAIL)%NVAR                                         
                          DO I=LFT,LLT                                                          
                            VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+1)  ! Sig1                                               
                            EVAR(I) = MAX(EVAR(I), VAR)                                         
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ELSEIF (IFUNC == 10364) THEN ! Sig1 - lower
                IPT = 1  
                IL  = 1  
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          NVARF = FBUF%FLOC(IFAIL)%NVAR                                         
                          DO I=LFT,LLT                                                          
                            VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+1)  ! Sig1                                               
                            EVAR(I) = MAX(EVAR(I), VAR)                                         
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ELSEIF (IFUNC == 10365) THEN ! Sig1 - membrane
                IF (NLAY > 1) THEN
                  IL  = NLAY / 2
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT / 2
                ENDIF
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          NVARF = FBUF%FLOC(IFAIL)%NVAR                                         
                          DO I=LFT,LLT                                                          
                            VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+1)  ! Sig1                                               
                            EVAR(I) = MAX(EVAR(I), VAR)                                         
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ELSEIF (IFUNC == 10366) THEN ! Sig2 - upper
                IF (NLAY > 1) THEN
                  IL  = NLAY
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT
                ENDIF
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          NVARF = FBUF%FLOC(IFAIL)%NVAR                                         
                          DO I=LFT,LLT                                                          
                            VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+2)  ! Sig2                                               
                            EVAR(I) = MAX(EVAR(I), VAR)                                         
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ELSEIF (IFUNC == 10367) THEN ! Sig2 - lower
                IPT = 1  
                IL  = 1  
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          NVARF = FBUF%FLOC(IFAIL)%NVAR                                         
                          DO I=LFT,LLT                                                          
                            VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+2)  ! Sig2                                               
                            EVAR(I) = MAX(EVAR(I), VAR)                                         
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ELSEIF (IFUNC == 10368) THEN ! Sig2 - membrane
                IF (NLAY > 1) THEN
                  IL  = NLAY / 2
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT / 2
                ENDIF
                DO IL=1,NLAY                                                                    
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model                
                          NVARF = FBUF%FLOC(IFAIL)%NVAR                                         
                          DO I=LFT,LLT                                                          
                            VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+2)  ! Sig2                                               
                            EVAR(I) = MAX(EVAR(I), VAR)                                         
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO                                                                         
                ENDDO                                                                           
              ENDIF
C--------------------------------------------------
            ELSE IF (IFUNC == 2142) THEN   ! FAIL
c------------------------------------
              IF (IGTYP == 10.OR.IGTYP == 11.OR.IGTYP == 17.OR. IGTYP == 51 
     .                                                     .OR. IGTYP == 52) THEN 
                  FAILG = 0
                  DO I=LFT,LLT
                    DAM1(I)=ZERO
                    DAM2(I)=ZERO 
                    WPLA(I)=ZERO
                    FAIL(I)=ZERO
                  END DO                                
                  IF(ITY == 3)THEN
                    DO I=LFT,LLT
                      MAT(I)=IXC(1,NFT+I)
                      PID(I)=IXC(6,NFT+I)
                    END DO                                
                  ELSE
                    DO I=LFT,LLT
                      MAT(I)=IXTG(1,NFT+I)
                      PID(I)=IXTG(5,NFT+I)
                    END DO                                
                  END IF
                  IF (IGTYP == 11) THEN
                    IPMAT = 100                               
                    DO N=1,NPT
                      IADR = (N-1)*NEL                       
                      DO I=LFT,LLT
                        J = IADR+I                            
                        MATLY(J) = IGEO(IPMAT+N,PID(I))
                      END DO                                
                    END DO                                
                  ELSEIF (IGTYP == 10) THEN 
                    DO N=1,NPT
                      IADR = (N-1)*NEL
                      DO I=LFT,LLT
                        J = IADR+I 
                        MATLY(J)=MAT(I)
                      END DO
                    END DO 
                  ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                      IPMAT = 2 + NLAY 
! old stack organisation           IPMAT = 300                             
                    DO N=1,NLAY
                      IADR = (N-1)*NEL                       
                      DO I=LFT,LLT
                        J = IADR+I                            
                        MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                      END DO                                
                    END DO        
                  END IF
c
                  IF (IHBE == 11) THEN ! Batoz shell
                    DO IL=1,NLAY
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                      IADR = (IL-1)*NEL
                      DO IT=1,NPTT
                        DO I=LFT,LLT
                          WPLA(I) = ZERO
                        ENDDO
                        FAILG = 0
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            OFFL => LBUF%OFF
                            IF (BUFLY%L_DAM > 0 .OR. BUFLY%L_OFF > 0 ) THEN
                              DO I=LFT,LLT                                    
                                 J = IADR + I 
                                 IF(IPM(2,MATLY(J)) == 15 .OR. IPM(2,MATLY(J)) == 25) THEN                                 
                                   DAM1(I)=LBUF%DAM(JJ(1)+I)                          
                                   DAM2(I)=LBUF%DAM(JJ(2)+I)                          
                                   WPLA(I) = WPLA(I) + ABS(LBUF%PLA(I))/NPG        
                                   DMAX(I) = PM(64,MATLY(J))                     
                                   WPMAX(I)= PM(41,MATLY(J))                     
                                   IF (DAM1(I) >= DMAX(I).OR.DAM2(I) >= DMAX(I)  
     .                                .OR.WPLA(I) < ZERO.OR.WPLA(I) >= WPMAX(I) 
     .                                .OR.OFFL(I) < ONE)   FAILG(I) = FAILG(I) + 1              
                                   IF (FAILG(I) == NPG ) THEN                 
                                       FAIL(I) = FAIL(I) + ONE                   
                                   ENDIF  
                                ELSE               
                                   IF (OFFL(I) < ONE)   FAILG(I)= FAILG(I) + 1              
                                   IF (FAILG(I) == NPG ) THEN                 
                                       FAIL(I) = FAIL(I) + ONE                     
                                   ENDIF  
                                ENDIF ! law25   
                              ENDDO
                            ENDIF
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO  !  DO IL=1,NLAY
                    DO  I=LFT,LLT
                      EVAR(I) = FAIL(I)
                    ENDDO
                  ELSE ! all shells
                    DO IL=1,NLAY
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                      
                      IADR = (IL-1)*NEL
                      DO IT=1,NPTT
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IT)
                        OFFL => LBUF%OFF
                        IF (BUFLY%L_DAM > 0 .OR.BUFLY%L_OFF > 0 ) THEN
                          DO I=LFT,LLT                                       
                            J = IADR + I 
                            IF(IPM(2,MATLY(J)) == 15 .OR. IPM(2,MATLY(J)) == 25) THEN                                    
                               DAM1(I) = LBUF%DAM(JJ(1)+I)
                               DAM2(I) = LBUF%DAM(JJ(2)+I)
                               WPLA(I) = ABS(LBUF%PLA(I))                      
                               DMAX(I) = PM(64,MATLY(J))                       
                               WPMAX(I)= PM(41,MATLY(J))                       
                               IF (DAM1(I) >= DMAX(I).OR.DAM2(I) >= DMAX(I).OR.
     .                             WPLA(I) < ZERO.OR.WPLA(I) >= WPMAX(I) .OR.
     .                             OFFL(I) < ONE )  FAIL(I) = FAIL(I) + ONE  
                            ELSE                 
                               IF (OFFL(I) < ONE )  FAIL(I) = FAIL(I) + ONE 
                            ENDIF
                          ENDDO
                        ENDIF
                      ENDDO
                    ENDDO  !  DO IL=1,NLAY
                    DO  I=LFT,LLT
                      EVAR(I) = FAIL(I)
                    ENDDO
!!                  ELSE
!!                    IF (IFAILA == 1) THEN
!!                      DO  I=LFT,LLT
!!                        OFF = GBUF%OFF(I)
!!                        IF (OFF<ZERO) THEN
!!                          FAIL(I)= -ONE
!                        ELSEIF (OFF>ZERO) THEN
!!                          FAIL(I)=  ZERO
!!                        ELSE
!!                          FAIL(I)=  ONE
!!                        END IF
!!                         EVAR(I)=FAIL(I)
!!                      END DO  
!!                    ELSE
!!                      DO  I=LFT,LLT

!!                      ENDDO
!!                    ENDIF 
                  ENDIF  ! IHBE
c                 
              ELSE  !
!! Should be clarified for others Pid.            
!!                IF (ITY == 3) THEN                                         
!!                  DO I=LFT,LLT                                           
!!                    MAT(I)=IXC(1,NFT+I)                                  
!!                    PID(I)=IXC(6,NFT+I)                                  
!!                  END DO                                                 
!!                ELSE                                                     
!!                  DO I=LFT,LLT                                           
!!                    MAT(I)=IXTG(1,NFT+I)                                 
!!                    PID(I)=IXTG(5,NFT+I)                                 
!!                  END DO                                                 
!!                END IF                                                   
!!                IF (IGTYP == 11 .OR. IGTYP == 16) THEN                     
!!                  IPMAT = 100                                            
!!                  DO N=1,NPT                                             
!!                    IADR = (N-1)*NEL                                     
!!                    DO I=LFT,LLT                                         
!!                      J = IADR+I                                         
!!                      MATLY(J)   = IGEO(IPMAT+N,PID(I))                  
!!                    END DO                                               
!!                  END DO                                                 
!!                ENDIF                                                    
!!                IF (IFAILURE==0 .OR.(IFAILURE /=0 .AND.IFAILA==1)) THEN
!!                  DO  I=LFT,LLT                                         
!!                    OFF = GBUF%OFF(I)                                  
!!                    IF (OFF<ZERO)THEN                                  
!!                      FAIL(I)= -ONE                                       
!!                    ELSEIF(OFF>ZERO)THEN                              
!!                      FAIL(I)=  ZERO                                       
!!                    ELSE                                                 
!!                      FAIL(I)=  ONE                                     
!!                    END IF                                               
!!                     EVAR(I)=FAIL(I)                                     
!!                  END DO                                                 
!!                ELSE                                                                                     
!!                  DO  I=LFT,LLT                                          

!!                  ENDDO                                                  
!!                ENDIF                                                    
              ENDIF ! MLW , IGTYP
c---------------------           
            ELSE IF (IFUNC >= 10256 .and. IFUNC <= 10359) THEN   
C---------------------Damage Output : ALL Layers
              IF (IFUNC == 10257) THEN      ! Damage Output : Upper Layer
                  IPT = NPT
              ELSEIF (IFUNC == 10258) THEN  ! Damage Output : Lower Layer
                  IPT = 1
              ELSEIF (IFUNC == 10259) THEN  ! Damage Output : Membrane Layer
                  IPT = IABS(NPT)/2 + 1
              ELSEIF (IFUNC >= 10260 .AND. IFUNC <= 10359) THEN  ! Damage Output : layer IPT
                  IPT = MOD ((IFUNC - 10259), 100)
                  IF (IPT == 0) IPT = 100
              ENDIF
c
              DO I = LFT, LLT       
                EVAR(I) = ZERO     
              ENDDO                 
c              
              IF(IFAILURE > 0) THEN
C-------
                IF (IFUNC == 10256) THEN
!  Element Damage Output --> Average value over all layers : 
!                          for each layer --> get mean value over it's NPTT (for each NPTT --> max over all pts Gauss, for all failure models)
C-------
                  IF (NLAY > 1) THEN                                                          
                    DO I = LFT,LLT                                                              
                      DO N = 1,NLAY
                        NPTT = ELBUF_TAB(NG)%BUFLY(N)%NPTT
                        DMGMX_LY = ZERO
                        DO IT = 1,NPTT
                          DMGMX = ZERO
                          DO IR = 1,NPTR                                                       
                            DO IS = 1,NPTS                                                     
                              FBUF => ELBUF_TAB(NG)%BUFLY(N)%FAIL(IR,IS,IT)                      
                              DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(N)%NFAIL                                               
                                DMGMX = MAX(DMGMX,FBUF%FLOC(IFAIL)%DAMMX(I))
                              ENDDO                                                             
                            ENDDO
                          ENDDO
                          DMGMX_LY = DMGMX_LY + DMGMX / NPTT
                        ENDDO ! DO IT = 1,NPTT
                        EVAR(I) = EVAR(I) + DMGMX_LY
                      ENDDO ! N = 1,NLAY                                                          
                      EVAR(I) = EVAR(I) / NLAY
                    ENDDO ! DO I = LFT,LLT
                  ELSEIF (MPT > 0) THEN  ! NLAY = 1                                                 
                    NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                    DO I = LFT,LLT                                                              
                      DO IT = 1,NPTT
                        DMGMX = ZERO
                        DO IR = 1,NPTR                                                       
                          DO IS = 1,NPTS                                                     
                            FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IT)                      
                            DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                               
                              DMGMX = MAX(DMGMX, FBUF%FLOC(IFAIL)%DAMMX(I))                      
                            ENDDO                                                             
                          ENDDO                                                               
                        ENDDO                                                                 
                        EVAR(I) = EVAR(I) + DMGMX
                      ENDDO !  N = 1,NPTT                                                          
                      EVAR(I) = EVAR(I) / NPTT
                    ENDDO ! I = LFT,LLT
                  ENDIF ! IF (NLAY > 1)
C-------
                ELSEIF (NPT >= IPT) THEN
!  Layer Damage Output (UPPER, LOWER, MEMB, ILAY) : 
!                    for layer (ILAY) --> get mean value over it's NPTT (for each NPTT --> max over all pts Gauss, for all failure models)
C-------
                  IF (NLAY > 1 .AND. IPT <= NLAY) THEN
                    NPTT = ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
                    DO I = LFT,LLT
                      DO IT = 1,NPTT
                        DMGMX = ZERO
                        DO IR = 1,NPTR
                          DO IS = 1,NPTS
                            FBUF => ELBUF_TAB(NG)%BUFLY(IPT)%FAIL(IR,IS,IT)
                            DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(IPT)%NFAIL
                              DMGMX = MAX(DMGMX,FBUF%FLOC(IFAIL)%DAMMX(I))
                            ENDDO
                          ENDDO
                        ENDDO
                        EVAR(I) = EVAR(I) + DMGMX
                      ENDDO ! DO IT = 1,NPTT
                      EVAR(I) = EVAR(I) / NPTT
                    ENDDO ! I = LFT,LLT
                  ELSEIF (MPT > 0) THEN  ! NLAY = 1                                                 
                    DO I = LFT,LLT                                                              
                      DO IR = 1, NPTR                                                         
                        DO IS = 1, NPTS                                                       
                          FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)                        
                          DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                                 
                            EVAR(I) = MAX(EVAR(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO ! I = LFT,LLT
                  ENDIF ! IF (NLAY > 1 .AND. IPT <= NLAY)
                ENDIF  ! Damage IF (IFUNC == 10256)
             ENDIF ! IFAILURE
C
C for outp of dam inside law25
C                             
             IF(MLW == 25 .AND. (IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .          IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 )) THEN   
                  IF(ITY == 3)THEN
                    DO I=LFT,LLT
                      MAT(I)=IXC(1,NFT+I)
                      PID(I)=IXC(6,NFT+I)
                    END DO                                
                  ELSE
                    DO I=LFT,LLT
                      MAT(I)=IXTG(1,NFT+I)
                      PID(I)=IXTG(5,NFT+I)
                    END DO                                
                  END IF
                  IF (IGTYP == 11) THEN
                    IPMAT = 100                               
                    DO N=1,NPT
                      IADR = (N-1)*NEL                       
                      DO I=LFT,LLT
                        J = IADR+I                            
                        MATLY(J) = IGEO(IPMAT+N,PID(I))
                      END DO                                
                    END DO                                
                  ELSEIF (IGTYP == 10) THEN 
                    DO N=1,NPT
                      IADR = (N-1)*NEL
                      DO I=LFT,LLT
                        J = IADR+I 
                        MATLY(J)=MAT(I)
                      END DO
                    END DO 
                  ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                    IPMAT = 2 + NLAY                         
                    DO N=1,NLAY
                      IADR = (N-1)*NEL                       
                      DO I=LFT,LLT
                        J = IADR+I                            
                        MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                      END DO                                
                    END DO        
                  END IF
C
CC                  IF (IHBE == 11) THEN Batoz NPG = 4 
                  IF (IFUNC == 10256) THEN 
                     DO I=LFT,LLT                                            
                       VE(1:5) = ZERO
                       DO IL=1,NLAY
                        NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                        IADR = (IL-1)*NEL                           
                        J = IADR + I 
                        VLY(1:5) = ZERO
                        DO IT=1,NPTT
                          VG(1:5)= ZERO
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                              DMAX(I) = ONE/PM(64,MATLY(J))                               
                              WPMAX(I)= ONE/PM(41,MATLY(J))                               
                              EPST1(I)= PM(60,MATLY(J))                               
                              EPST2(I)= PM(61,MATLY(J))                               
                              EPSF1(I)= ONE/PM(98,MATLY(J))                               
                              EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                              VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                              VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                              VG(3)=MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                              IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .                          (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                              IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .                          (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))               
                           ENDDO
                          ENDDO
                           VLY(1) = VLY(1) + VG(1)
                           VLY(2) = VLY(2) + VG(2)
                           VLY(3) = VLY(3) + VG(3)
                           VLY(4) = VLY(4) + VG(4)
                           VLY(5) = VLY(5) + VG(5)
                        ENDDO ! NPTT
                          VE(1)  = VE(1) + VLY(1)/NPTT
                          VE(2)  = VE(2) + VLY(2)/NPTT
                          VE(3)  = VE(3) + VLY(3)/NPTT
                          VE(4)  = VE(4) + VLY(4)/NPTT
                          VE(5)  = VE(5) + VLY(5)/NPTT
                      ENDDO  !   DO IL=1,NLAY 
                          VE(1) = VE(1)/NLAY 
                          VE(2) = VE(2)/NLAY 
                          VE(3) = VE(3)/NLAY 
                          VE(4) = VE(4)/NLAY 
                          VE(5) = VE(5)/NLAY 
                          EVAR(I) = MAX(EVAR(I),VE(1),VE(2),VE(3),
     .                                                VE(4),VE(5))
                    ENDDO  ! I=LFT,JLT
                  ELSEIF(IPT <= NLAY) THEN                 
!!                       DO IL=1,NLAY  ! IPT = IL (layer)
                     DO I=LFT,LLT                                            
                        NPTT = ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                        IADR = (IPT - 1)*NEL                           
                        J = IADR + I 
                        VLY(1:5) = ZERO
                        DO IT=1,NPTT
                          VG(1:5)= ZERO
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,IT)        
                              DMAX(I) = ONE/PM(64,MATLY(J))                               
                              WPMAX(I)= ONE/PM(41,MATLY(J))                               
                              EPST1(I)= PM(60,MATLY(J))                               
                              EPST2(I)= PM(61,MATLY(J))                               
                              EPSF1(I)= ONE/PM(98,MATLY(J))                               
                              EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                              VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                              VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                              VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                              IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .                          (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                              IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .                          (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                           ENDDO
                          ENDDO
                           VLY(1) =VLY(1) + VG(1) 
                           VLY(2) =VLY(2) + VG(2) 
                           VLY(3) =VLY(3) + VG(3) 
                           VLY(4) =VLY(4) + VG(4) 
                           VLY(5) =VLY(5) + VG(5) 
                        ENDDO ! NPTT
                         VLY(1) =VLY(1)/NPTT
                         VLY(2) =VLY(2)/NPTT
                         VLY(3) =VLY(3)/NPTT
                         VLY(4) =VLY(4)/NPTT
                         VLY(5) =VLY(5)/NPTT
C                         
                         EVAR(I) = MAX(EVAR(I),VLY(1),VLY(2),VLY(3),
     .                                               VLY(4),VLY(5))
                    ENDDO  ! I=LFT,JLT
                  ENDIF
            ENDIF ! law 25  + SHell Composite  PID
            
C-------------------------------------
            ELSE IF (IFUNC == 10670) THEN   
C           FAIL TIME : ELEMENT DELETED
C-------------------------------------
              DO I = LFT, LLT       
                EVAR(I) = ZERO
              ENDDO                 
c
              DO IL=1,NLAY                                                               
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                     
                DO IS=1,NPTS                                                             
                  DO IT=1,NPTT                                                           
                    DO IR=1,NPTR                                                        
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                      
                      DO IFAIL=1,NFAIL                                                                                              
                        DO I=LFT,LLT                                                     
                          EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%TDEL(I))                            
                        ENDDO                                                            
                      ENDDO                                                              
                    ENDDO                                                                
                  ENDDO                                                                  
                ENDDO                                                                   
              ENDDO    
C-------------------------------------
            ELSE IF (IFUNC == 10671) THEN   
C           SOUND SPEED
            ! /ANIM/ELEM/SSP
C-------------------------------------   
              L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
              IF(L==0)THEN                                            
                DO  I=LFT,LLT                                         
                  EVAR(I) = ZERO                       
                ENDDO                                                 
              ELSE                                                    
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                DO  I=LFT,LLT                                         
                  EVAR(I) = LBUF%SSP(I)                
                ENDDO                                                 
              ENDIF   
C-------------------------------------
            ELSE IF (IFUNC == 10672) THEN   
C           ALE SCHLIEREN N/A
            !/ANIM/ELEM/SCHLIEREN
C-------------------------------------                             
              DO  I=LFT,LLT                                         
                EVAR(I) = ZERO                       
              ENDDO                                                                                                                          
C-----------------------------------------------
            ELSE IF (IFUNC == 2156) THEN
C-----------------------------------------------
              IF (ITY == 3) THEN
                           DO  I=LFT,LLT
                  EVAR(I) = ERR_THK_SH4(NFT+I)
                END DO
              ELSE
                DO  I=LFT,LLT
                  EVAR(I) = ERR_THK_SH3(NFT+I)
                END DO
              ENDIF
C-------------------------------------
            ELSE IF (IFUNC == 10676) THEN   
C           SPMD DOMAIN
C-------------------------------------
              DO  I=LFT,LLT                                         
                EVAR(I) = ISPMD                       
              ENDDO  
C-------------------------------------
            ELSEIF (IFUNC == 10677) THEN  !  /ANIM/SHELL/SIGEQ
            !  equivalent stress - other than VON MISES
C-------------------------------------
              IF (GBUF%G_SEQ > 0) THEN
C------------------
                ! Total number of integration points
                NPGT = 0
                DO IL=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPGT = NPGT + BUFLY%NPTT*NPTR*NPTS
                ENDDO
                ! Average equivalent stress on integration points
                DO I=LFT,LLT
                  EVAR_TMP = ZERO
                  DO IL=1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                    DO IT=1,BUFLY%NPTT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                          EVAR_TMP = EVAR_TMP + LBUF%SEQ(I)/NPGT
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                  EVAR(I) = EVAR_TMP
                ENDDO
C------------------
              ELSE  !  VON MISES
                DO I=LFT,LLT
                  S1 = GBUF%FOR(JJ(1)+I)
                  S2 = GBUF%FOR(JJ(2)+I)
                  S12= GBUF%FOR(JJ(3)+I)
                  VONM2 = S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                  EVAR(I) = SQRT(VONM2)
                ENDDO
              ENDIF ! IF (GBUF%G_SEQ > 0)
c------------------------------------
            ELSEIF (IFUNC > 10677 .AND. IFUNC < 10778 .AND. 
     .                               (IGTYP == 51 .OR. IGTYP == 52).AND.
     .                                MLW /= 15 .AND. MLW /= 25 ) THEN  
            ! EPSP/ILAY/UPPER
c------------------------------------
C available for IGTYP = 51 and 52  only (to be generalized)
              ILAY = MOD ((IFUNC - 10677), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IPT  = MAX(1,NPTT)
              IF (BUFLY%L_PLA > 0 .AND.
     .           (IL <= NLAY .AND. IPT <= NPTT))THEN
                IF (NPG > 1) THEN
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE ! (NPG == 1)
                  LBUF => BUFLY%LBUF(1,1,IPT)
                  DO I=LFT,LLT
                    EVAR(I) = ABS(LBUF%PLA(I))
                  ENDDO
                ENDIF ! IF (NPG > 1) THEN
              ENDIF ! IF (BUFLY%L_PLA > 0) THEN
c------------------------------------
            ELSEIF (IFUNC > 10777 .AND. IFUNC < 10878 .AND. 
     .                       (IGTYP == 51 .OR. IGTYP == 52) .AND. 
     .                        MLW /= 15 .AND. MLW /= 25) THEN  
            ! EPSP/ILAY/LOWER
c------------------------------------
C available for IGTYP = 51 and 52 only (to be generalized)
              ILAY = MOD ((IFUNC - 10777), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              IPT  = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IF (BUFLY%L_PLA > 0 .AND.
     .           (IL <= NLAY .AND. IPT <= NPTT))THEN
                IF (NPG > 1) THEN
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE ! (NPG == 1)
                  LBUF => BUFLY%LBUF(1,1,IPT)
                  DO I=LFT,LLT
                    EVAR(I) = ABS(LBUF%PLA(I))
                  ENDDO
                ENDIF ! IF (NPG > 1) THEN
              ENDIF ! IF (BUFLY%L_PLA > 0) THEN
c------------------------------------
            ELSEIF (IFUNC > 10877 .AND. IFUNC < 11888 .AND. 
     .                             (IGTYP == 51 .OR. IGTYP == 52).AND. 
     .                        MLW /= 15 .AND. MLW /= 25) THEN  
            ! EPSP/ILAY/NIP
c------------------------------------
C
C available for IGTYP = 51 and 52 only (to be generalized)
C
              IUS = IFUNC - 10877
              IL  = INT((IUS - 1)/10)
              IPT = IUS - 10*IL
              IF (IL <= NLAY ) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT
                IF (BUFLY%L_PLA > 0 .AND. IPT <= NPTT) THEN
                  IF (NPG > 1) THEN
                    DO I=LFT,LLT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => BUFLY%LBUF(IR,IS,IPT)
                          EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                        ENDDO
                      ENDDO
                    ENDDO
                  ELSE ! (NPG == 1)
                    LBUF => BUFLY%LBUF(1,1,IPT)
                    DO I=LFT,LLT
                      EVAR(I) = ABS(LBUF%PLA(I))
                    ENDDO
                  ENDIF ! IF (NPG > 1) THEN
                ENDIF ! IF (BUFLY%L_PLA > 0) THEN
              ENDIF ! IF (IL <= NLAY )
c------------------------------------
            ELSEIF(IFUNC == 11888)THEN  
            !/ANIM/ELEM/BULK (QVIS)
c------------------------------------                                          
              IF (GBUF%G_QVIS > 0) THEN
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = GBUF%QVIS(I)
                ENDDO
              ELSE
                DO I=LFT,LLT
                  FUNC(EL2FA(NN3+NFT+I)) = ZERO
                ENDDO              
              ENDIF
c------------------------------------                                          
            ELSEIF (IFUNC == 11889) THEN               
              IF (MLW  /= 51 .AND. GBUF%G_TB > 0) THEN
                DO I=LFT,LLT                           
                  FUNC(EL2FA(NN3+NFT+I)) = -GBUF%TB(I)   
                ENDDO 
              ELSEIF (MLW == 51)THEN
                 MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                 IPOS      = 15
                 ITRIMAT   = 4
                 LLT       = IPARG(2,NG)      
                 K         = LLT * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                             
                 DO I=LFT,LLT
                   FUNC(EL2FA(NN3+NFT+I)) = -MBUF%VAR(K+I)
                 ENDDO 
              ELSE
                DO I=LFT,LLT                           
                  FUNC(EL2FA(NN3+NFT+I)) = ZERO   
                ENDDO 
              ENDIF                  
C--------------------------------------------------
            ELSE IF (IFUNC>11925 .AND. IFUNC < 11925+MX_PLY_ANIM+1)THEN 
            !/ANIM/SHELL/IDPLY
c------------------------------------
	      IPLY = IFUNC - 11925
	      IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		IF (PLY_ANIM( 3 * (IPLY - 1) + 2) == 1 )THEN
		  DO J=1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    NPTT = BUFLY%NPTT
		    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
		    IF (ID_PLY  == PLY_ANIM( 3 * (IPLY - 1) + 1) ) THEN
		      DO I=LFT,LLT
                        NB_PLYOFF = 0
                        DO IR=1,NPTR                                          
                          DO IS=1,NPTS                                         
                            DO IT=1,NPTT     
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT)  
                              IF (LBUF%OFF(I) == ZERO) NB_PLYOFF = NB_PLYOFF + 1
		            ENDDO 
		          ENDDO 
		        ENDDO
		        IF ( NB_PLYOFF == NPTR * NPTS * NPTT ) THEN
                          EVAR(I) = -ONE
		        ELSE
                          EVAR(I) = ONE 
                        ENDIF
		      ENDDO 
		    ENDIF
		  ENDDO
		ENDIF
	      ELSEIF (IGTYP == 52) THEN
		IF (PLY_ANIM( 3 * (IPLY - 1) + 2) == 1 )THEN
		  DO J=1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    NPTT = BUFLY%NPTT
		    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK)
		    IF (ID_PLY  == PLY_ANIM( 3 * (IPLY - 1) + 1) ) THEN
		      DO I=LFT,LLT
                        NB_PLYOFF = 0
                        DO IR=1,NPTR                                          
                          DO IS=1,NPTS                                         
                            DO IT=1,NPTT     
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT)  
                              IF (LBUF%OFF(I) == ZERO) NB_PLYOFF = NB_PLYOFF + 1
		            ENDDO 
		          ENDDO 
		        ENDDO 
		        IF ( NB_PLYOFF == NPTR * NPTS * NPTT ) THEN
                          EVAR(I) = -ONE
		        ELSE
                          EVAR(I) = ONE 
                        ENDIF
		      ENDDO 
		    ENDIF
		  ENDDO
		ENDIF
	      ENDIF
C--------------------------------------------------
            ELSE IF (IFUNC> 11925+MX_PLY_ANIM .AND. IFUNC < 11925+(2*MX_PLY_ANIM)+1)THEN 
            !/ANIM/SHELL/IDPLY/PHI
c------------------------------------
	      IPLY = IFUNC - (11925+MX_PLY_ANIM)
c
	      DO J=1,NLAY
               ID_PLY = 0
               IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		 ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
               ELSEIF (IGTYP == 52) THEN
                 ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
               ENDIF
c
               IF (ID_PLY  == PLY_ANIM_PHI( 3 * (IPLY - 1) + 1) ) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                IF (ITY == 3) THEN
                  IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                    LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1) 
                    IF (MLW /= 0 .AND. MLW /= 13) THEN   
                      DO I=LFT,LLT                                                
                        N = I + NFT                                               
                        X21 = X(1,IXC(3,N))-X(1,IXC(2,N))                         
                        X32 = X(1,IXC(4,N))-X(1,IXC(3,N))                         
                        X34 = X(1,IXC(4,N))-X(1,IXC(5,N))                         
                        X41 = X(1,IXC(5,N))-X(1,IXC(2,N))                         

                        Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))                         
                        Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))                         
                        Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))                         
                        Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))                         

                        Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))                         
                        Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))                         
                        Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))                         
                        Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))                         
                                                                                  
                        E1X = (X21+X34)                                           
                        E1Y = (Y21+Y34)                                           
                        E1Z = (Z21+Z34)                                           

                        E2X = (X32+X41)                                           
                        E2Y = (Y32+Y41)                                           
                        E2Z = (Z32+Z41)                                           
                 
                        E3X = E1Y*E2Z-E1Z*E2Y                                     
                        E3Y = E1Z*E2X-E1X*E2Z                                     
                        E3Z = E1X*E2Y-E1Y*E2X                                     
                        IF (IREP > 0) THEN                                        
                          RX = E1X                                                
                          RY = E1Y                                                
                          RZ = E1Z                                                
                          SX = E2X                                                
                          SY = E2Y                                                
                          SZ = E2Z                                                
                        ENDIF                                                     
                        IF (ISHFRAM == 0 .OR. IGTYP == 16 ) THEN                  
C------                   Repere convecte symetrique - version 5 (default)        
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = SQRT(S1/S2)                                    
                          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                      
                          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                      
                          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                      
C
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E1X = E1X * SUMA                                        
                          E1Y = E1Y * SUMA                                        
                          E1Z = E1Z * SUMA                                        
C
                          E2X = E3Y * E1Z - E3Z * E1Y                             
                          E2Y = E3Z * E1X - E3X * E1Z                             
                          E2Z = E3X * E1Y - E3Y * E1X                             
                        ELSEIF (ISHFRAM == 2) THEN                                
C------                   Repere convecte nonsymetrique - version 4               
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y                        
                          E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z                        
                          E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X                        
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E1X = E1X*SUMA                                          
                          E1Y = E1Y*SUMA                                          
                          E1Z = E1Z*SUMA                                          
C
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          E2X = E3Y*E1Z-E3Z*E1Y                                   
                          E2Y = E3Z*E1X-E3X*E1Z                                   
                          E2Z = E3X*E1Y-E3Y*E1X                                   
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E2X = E2X*SUMA                                          
                          E2Y = E2Y*SUMA                                          
                          E2Z = E2Z*SUMA                                          
                        ENDIF                                                     
                        IF (IREP >= 1) THEN                                       
                          AA = LBUF_DIR%DIRA(I)				      
                          BB = LBUF_DIR%DIRA(I+NEL)			      
                          V1 = AA*RX + BB*SX                                      
                          V2 = AA*RY + BB*SY                                      
                          V3 = AA*RZ + BB*SZ                                      
                          VR = V1*E1X+ V2*E1Y + V3*E1Z                            
                          VS = V1*E2X+ V2*E2Y + V3*E2Z                            
                          SUMA=SQRT(VR*VR + VS*VS)                                
                          DIR1_1 = VR/SUMA                                        
                          DIR1_2 = VS/SUMA                                        
                        ELSE                                                      
                          DIR1_1 = LBUF_DIR%DIRA(I)    			      
                          DIR1_2 = LBUF_DIR%DIRA(I+NEL)			      
                        ENDIF                                       
c
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO
                      ENDDO                                                       
                    ENDIF  ! MLW                               
                 ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                     BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (MLW /= 0 .AND. MLW /= 13) THEN   
                      DO I=LFT,LLT                                                
                        N = I + NFT                                               
                        X21 = X(1,IXC(3,N))-X(1,IXC(2,N))                         
                        X32 = X(1,IXC(4,N))-X(1,IXC(3,N))                         
                        X34 = X(1,IXC(4,N))-X(1,IXC(5,N))                         
                        X41 = X(1,IXC(5,N))-X(1,IXC(2,N))                         

                        Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))                         
                        Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))                         
                        Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))                         
                        Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))                         

                        Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))                         
                        Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))                         
                        Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))                         
                        Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))                         
                                                                                  
                        E1X = (X21+X34)                                           
                        E1Y = (Y21+Y34)                                           
                        E1Z = (Z21+Z34)                                           

                        E2X = (X32+X41)                                           
                        E2Y = (Y32+Y41)                                           
                        E2Z = (Z32+Z41)                                           
                 
                        E3X = E1Y*E2Z-E1Z*E2Y                                     
                        E3Y = E1Z*E2X-E1X*E2Z                                     
                        E3Z = E1X*E2Y-E1Y*E2X                                     
                        IF (IREP > 0) THEN                                        
                          RX = E1X                                                
                          RY = E1Y                                                
                          RZ = E1Z                                                
                          SX = E2X                                                
                          SY = E2Y                                                
                          SZ = E2Z                                                
                        ENDIF                                                     
                        IF (ISHFRAM == 0 .OR. IGTYP == 16 ) THEN                  
C------                   Repere convecte symetrique - version 5 (default)        
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = SQRT(S1/S2)                                    
                          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                      
                          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                      
                          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                      
C
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E1X = E1X * SUMA                                        
                          E1Y = E1Y * SUMA                                        
                          E1Z = E1Z * SUMA                                        
C
                          E2X = E3Y * E1Z - E3Z * E1Y                             
                          E2Y = E3Z * E1X - E3X * E1Z                             
                          E2Z = E3X * E1Y - E3Y * E1X                             
                        ELSEIF (ISHFRAM == 2) THEN                                
C------                   Repere convecte nonsymetrique - version 4               
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y                        
                          E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z                        
                          E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X                        
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E1X = E1X*SUMA                                          
                          E1Y = E1Y*SUMA                                          
                          E1Z = E1Z*SUMA                                          
C
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          E2X = E3Y*E1Z-E3Z*E1Y                                   
                          E2Y = E3Z*E1X-E3X*E1Z                                   
                          E2Z = E3X*E1Y-E3Y*E1X                                   
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E2X = E2X*SUMA                                          
                          E2Y = E2Y*SUMA                                          
                          E2Z = E2Z*SUMA                                          
                        ENDIF                                                     
                        IF (IREP >= 1) THEN                                       
                          AA = BUFLY%DIRA(I)				      
                          BB = BUFLY%DIRA(I+NEL)			      
                          V1 = AA*RX + BB*SX                                      
                          V2 = AA*RY + BB*SY                                      
                          V3 = AA*RZ + BB*SZ                                      
                          VR = V1*E1X+ V2*E1Y + V3*E1Z                            
                          VS = V1*E2X+ V2*E2Y + V3*E2Z                            
                          SUMA=SQRT(VR*VR + VS*VS)                                
                          DIR1_1 = VR/SUMA                                        
                          DIR1_2 = VS/SUMA                                        
                        ELSE                                                      
                          DIR1_1 = BUFLY%DIRA(I)    			      
                          DIR1_2 = BUFLY%DIRA(I+NEL)			      
                        ENDIF                                       
c
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO
                      ENDDO                                                       
                    ENDIF  ! MLW                                                  
                   ENDIF  ! IGTYP + IDRAPE
                
                  ELSEIF (ITY == 7) THEN
                    IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN  
                     LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1) 
                     IF (MLW /= 0 .AND. MLW /= 13) THEN                      
                      DO I=LFT,LLT                                              
                        N = I + NFT                                             
                        X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))                     
                        X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                     
                        X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                     

                        Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))                     
                        Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))                     
                        Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))                     

                        Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))                     
                        Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))                     
                        Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))                     
                        IF (IREP > 0) THEN                                      
                          E11 = X21                                             
                          E12 = Y21                                             
                          E13 = Z21                                             
                          E21 = X31                                             
                          E22 = Y31                                             
                          E23 = Z31                                             
                        ENDIF                                                   
             IF(IFRAM_OLD ==0 ) THEN
	       CALL CLSCONV3(X21,Y21,Z21,X31,Y31,Z31,
     +	                     E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z)
             ELSE		
                        E1X= X21                                                
                        E1Y= Y21                                                
                        E1Z= Z21                                                
                        X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)                     
                        E1X=E1X/X2L                                             
                        E1Y=E1Y/X2L                                             
                        E1Z=E1Z/X2L                                             
C
                        E3X=Y31*Z32-Z31*Y32                                     
                        E3Y=Z31*X32-X31*Z32                                     
                        E3Z=X31*Y32-Y31*X32                                     
                        SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)                     
                        E3X=E3X/SUM_                                             
                        E3Y=E3Y/SUM_                                             
                        E3Z=E3Z/SUM_                                             
                        AREA = HALF * SUM_                                     
                        E2X=E3Y*E1Z-E3Z*E1Y                                     
                        E2Y=E3Z*E1X-E3X*E1Z                                     
                        E2Z=E3X*E1Y-E3Y*E1X                                     
                        SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)                     
                        E2X=E2X/SUM_                                             
                        E2Y=E2Y/SUM_                                             
                        E2Z=E2Z/SUM_ 
             END IF			
                        IF (IREP >= 1) THEN                                     
                          AA = LBUF_DIR%DIRA(I)				    
                          BB = LBUF_DIR%DIRA(I+NEL)			    
                          V1 = AA*E11 + BB*E21                                  
                          V2 = AA*E12 + BB*E22                                  
                          V3 = AA*E13 + BB*E23                                  
                          VR = V1*E1X + V2*E1Y + V3*E1Z                         
                          VS = V1*E2X + V2*E2Y + V3*E2Z                         
                          SUMA=SQRT(VR*VR + VS*VS)                              
                          DIR1_1 = VR/SUMA                                      
                          DIR1_2 = VS/SUMA                                      
                        ELSE                                                    
                          DIR1_1 = LBUF_DIR%DIRA(I)                        
                          DIR1_2 = LBUF_DIR%DIRA(I+NEL)                        
                        ENDIF                                         
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO     
                      ENDDO                                                     
                    ENDIF                                                        
                   ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                     BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (MLW /= 0 .AND. MLW /= 13) THEN                      
                      DO I=LFT,LLT                                              
                        N = I + NFT                                             
                        X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))                     
                        X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                     
                        X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                     

                        Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))                     
                        Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))                     
                        Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))                     

                        Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))                     
                        Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))                     
                        Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))                     
                        IF (IREP > 0) THEN                                      
                          E11 = X21                                             
                          E12 = Y21                                             
                          E13 = Z21                                             
                          E21 = X31                                             
                          E22 = Y31                                             
                          E23 = Z31                                             
                        ENDIF                                                   
             IF(IFRAM_OLD ==0 ) THEN
	       CALL CLSCONV3(X21,Y21,Z21,X31,Y31,Z31,
     +	                     E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z)
             ELSE		
                        E1X= X21                                                
                        E1Y= Y21                                                
                        E1Z= Z21                                                
                        X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)                     
                        E1X=E1X/X2L                                             
                        E1Y=E1Y/X2L                                             
                        E1Z=E1Z/X2L                                             
C
                        E3X=Y31*Z32-Z31*Y32                                     
                        E3Y=Z31*X32-X31*Z32                                     
                        E3Z=X31*Y32-Y31*X32                                     
                        SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)                     
                        E3X=E3X/SUM_                                             
                        E3Y=E3Y/SUM_                                             
                        E3Z=E3Z/SUM_                                             
                        AREA = HALF * SUM_                                     
                        E2X=E3Y*E1Z-E3Z*E1Y                                     
                        E2Y=E3Z*E1X-E3X*E1Z                                     
                        E2Z=E3X*E1Y-E3Y*E1X                                     
                        SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)                     
                        E2X=E2X/SUM_                                             
                        E2Y=E2Y/SUM_                                             
                        E2Z=E2Z/SUM_ 
             END IF			
                        IF (IREP >= 1) THEN                                     
                          AA = BUFLY%DIRA(I)				    
                          BB = BUFLY%DIRA(I+NEL)			    
                          V1 = AA*E11 + BB*E21                                  
                          V2 = AA*E12 + BB*E22                                  
                          V3 = AA*E13 + BB*E23                                  
                          VR = V1*E1X + V2*E1Y + V3*E1Z                         
                          VS = V1*E2X + V2*E2Y + V3*E2Z                         
                          SUMA=SQRT(VR*VR + VS*VS)                              
                          DIR1_1 = VR/SUMA                                      
                          DIR1_2 = VS/SUMA                                      
                        ELSE                                                    
                          DIR1_1 = BUFLY%DIRA(I)                        
                          DIR1_2 = BUFLY%DIRA(I+NEL)                        
                        ENDIF                                         
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        EVAR(I) = PHI
                        IF(ABS(ERR) < EM02) EVAR(I) = SIGN(NINTY,PHI)
                        IF(ABS(EVAR(I)) < ONE) EVAR(I) = ZERO     
                      ENDDO                                                     
                    ENDIF                                                       
                   ENDIF              
                  ENDIF
c
               ENDIF
              ENDDO
c------------------------------------
            ELSE IF (IFUNC> 11925+(2*MX_PLY_ANIM) .AND. IFUNC < 11925+(3*MX_PLY_ANIM)+1)THEN 
            !/ANIM/SHELL/IDPLY/EPSP
c------------------------------------
	      IPLY = IFUNC - (11925+ 2*MX_PLY_ANIM)
	      IPT = PLY_ANIM_EPSP( 3 * (IPLY - 1) + 3)
c
	      DO J=1,NLAY
               ID_PLY = 0
               IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		 ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
               ELSEIF (IGTYP == 52) THEN
                 ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
               ENDIF
c
               IF (ID_PLY  == PLY_ANIM_EPSP( 3 * (IPLY - 1) + 1) ) THEN
                 BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                 IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                   NPTT = BUFLY%NPTT 
                   IF( IPT <= NPTT) THEN
                    IF( NPG > 1 ) THEN
                     DO  I=LFT,LLT
                       DO IR=1,NPTR
                         DO IS=1,NPTS				  
                           EVAR(I) = EVAR(I) + ABS(BUFLY%LBUF(IR,IS,IPT)%PLA(I))/NPG  
                         ENDDO    
                       ENDDO
                     ENDDO
                    ELSE
                      DO  I=LFT,LLT				  
                        EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))  
                      ENDDO
                    ENDIF
c
                   ELSE
                     DO  I=LFT,LLT				  
                       EVAR(I) = ZERO
                     ENDDO
                   ENDIF
                 ELSE
                   DO  I=LFT,LLT				
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF
               ENDIF 
              ENDDO
c------------------------------------
            ELSE IF (IFUNC> 11925+(3*MX_PLY_ANIM) .AND. IFUNC < 11925+(4*MX_PLY_ANIM)+1)THEN 
            !/ANIM/SHELL/IDPLY/DAMA
c------------------------------------
	      IPLY = IFUNC - (11925+ 3*MX_PLY_ANIM)
	      IPT = PLY_ANIM_DAMA( 3 * (IPLY - 1) + 3)
c
              IF(IFAILURE > 0) THEN 
	        DO J=1,NLAY
                  NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY=PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF 
                  IF (ID_PLY  == PLY_ANIM_DAMA( 3 *(IPLY - 1) + 1) )THEN
                    IF (IPT <= NPTT) THEN                                             
                      DO I=LFT,LLT								  
                        DO IR = 1, NPTR							  
                          DO IS = 1, NPTS							  
                            FBUF => ELBUF_TAB(NG)%BUFLY(J)%FAIL(IR,IS,IPT)			    
                            DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(J)%NFAIL						 
                	      EVAR(I) = MAX(EVAR(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                            ENDDO								  
                          ENDDO								  
                        ENDDO 								  
                      ENDDO	  !  I=LFT,LLT  
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
c
              IF(MLW == 25 .AND. (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52)) THEN     
                IF(ITY == 3)THEN
                  DO I=LFT,LLT
                    MAT(I)=IXC(1,NFT+I)
                    PID(I)=IXC(6,NFT+I)
                  END DO				
                ELSE
                  DO I=LFT,LLT
                    MAT(I)=IXTG(1,NFT+I)
                    PID(I)=IXTG(5,NFT+I)
                  END DO				
                END IF
c
                IPMAT = 2 + NLAY			 
                DO N=1,NLAY
                  IADR = (N-1)*NEL			 
                  DO I=LFT,LLT
                    J = IADR+I  			  
                    MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                  END DO				
                END DO
c
	        DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                   ID_PLY=PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == PLY_ANIM_DAMA( 3 *(IPLY - 1) + 1) )THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    DO I=LFT,LLT					    
                      NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                      IF (IPT <= NPTT) THEN                                      
                        IADR = (J - 1)*NEL
                        VLY(1:5) = ZERO
                        VG(1:5)= ZERO
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            LBUF=> ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT)  
                            DMAX(I) = ONE/PM(64,MATLY(IADR + I))  			     
                            WPMAX(I)= ONE/PM(41,MATLY(IADR + I))  			     
                            EPST1(I)= PM(60,MATLY(IADR + I))				  
                            EPST2(I)= PM(61,MATLY(IADR + I))				  
                            EPSF1(I)= ONE/PM(98,MATLY(IADR + I))  			     
                            EPSF2(I)= ONE/PM(99,MATLY(IADR + I))
C   
                            VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                            VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                            VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                            IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .                        (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                            IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .                        (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))  
                          ENDDO
                        ENDDO
                      ENDIF
                      VLY(1) =VG(1) 
                      VLY(2) =VG(2) 
                      VLY(3) =VG(3) 
                      VLY(4) =VG(4) 
                      VLY(5) =VG(5) 
C                     
                      EVAR(I) = MAX(EVAR(I),VLY(1),VLY(2),VLY(3),VLY(4),VLY(5))
                    ENDDO  ! I=LFT,JLT

                  ENDIF 
                ENDDO
              ENDIF
C-------------------
            ELSEIF (IFUNC > 11925+4*MX_PLY_ANIM .and. 
     .              IFUNC < 11925+4*MX_PLY_ANIM + 4) THEN ! FLD Damage factor
C-------------------
              IDX = 11925+4*MX_PLY_ANIM
              IF (IFUNC == IDX+1) THEN       !  /ANIM/SHELL/FLDF/UPPER
                IF (NLAY > 1) THEN
                  IL  = NLAY
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT
                ENDIF
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    DO IT=1,NPTT
                      IPT = IT
                      IF (NLAY == 1) IPT = NPTT
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                          DO I=LFT,LLT                                                          
                            EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO
                    ENDDO                                                                     
                  ENDDO                                                                       
                ENDDO                                                                         
c
              ELSEIF (IFUNC == IDX+2) THEN    !  /ANIM/SHELL/FLDF/LOWER
                IL = 1
                IPT  = 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT  = BUFLY%NPTT
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                DO IS=1,NPTS
                  DO IR=1,NPTR
                    FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)
                    DO IFAIL=1,NFAIL
                      IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model
                        DO I=LFT,LLT
                          EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))
                        ENDDO
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
c
              ELSEIF (IFUNC == IDX+3) THEN    !  /ANIM/SHELL/FLDF/MEMB
                IL  = NLAY / 2 + 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT  = BUFLY%NPTT
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                IPT   = NPTT/2 + 1
                DO IS=1,NPTS
                  DO IR=1,NPTR
                    FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                    DO IFAIL=1,NFAIL                                                          
                      IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                        DO I=LFT,LLT                                                          
                          EVAR(I) = MAX(EVAR(I),FBUF%FLOC(IFAIL)%DAM(I))                          
                        ENDDO                                                                 
                      ENDIF                                                                   
                    ENDDO
                  ENDDO                                                                       
                ENDDO                                                                         
              ENDIF
C-------------------
            ELSEIF (IFUNC > 11925+4*MX_PLY_ANIM + 3.and. 
     .              IFUNC < 11925+4*MX_PLY_ANIM + 7) THEN ! FLD zone index
C-------------------
              IDX = 11925+4*MX_PLY_ANIM + 3
              IF (IFUNC == IDX+1) THEN       !  /ANIM/SHELL/FLDZ/UPPER
                IF (NLAY > 1) THEN
                  IL  = NLAY
                  IPT = 1
                ELSE
                  IL  = 1
                  IPT = NPTT
                ENDIF
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    DO IT=1,NPTT
                      IPT = IT
                      IF (NLAY == 1) IPT = NPTT
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                          DO I=LFT,LLT                                                          
                            RINDX = FBUF%FLOC(IFAIL)%INDX(I)
                            EVAR(I) = MAX(EVAR(I),RINDX)                          
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO
                    ENDDO                                                                     
                  ENDDO                                                                       
                ENDDO                                                                         
c
              ELSEIF (IFUNC == IDX+2) THEN    !  /ANIM/SHELL/FLDZ/LOWER
                IL = 1
                IPT  = 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT  = BUFLY%NPTT
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                    DO IFAIL=1,NFAIL                                                          
                      IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                        DO I=LFT,LLT
                          RINDX = FBUF%FLOC(IFAIL)%INDX(I)
                          EVAR(I) = MAX(EVAR(I),RINDX)                          
                        ENDDO                                                                 
                      ENDIF                                                                   
                    ENDDO
                  ENDDO                                                                       
                ENDDO                                                                         
c
              ELSEIF (IFUNC == IDX+3) THEN    !  /ANIM/SHELL/FLDZ/MEMB
                IL  = NLAY / 2 + 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT  = BUFLY%NPTT
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                IPT   = NPTT/2 + 1
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                    DO IFAIL=1,NFAIL                                                          
                      IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                        DO I=LFT,LLT                                                          
                          RINDX = FBUF%FLOC(IFAIL)%INDX(I)
                          EVAR(I) = MAX(EVAR(I),RINDX)                          
                        ENDDO                                                                 
                      ENDIF                                                                   
                    ENDDO
                  ENDDO                                                                       
                ENDDO                                                                         
              ENDIF
C------------------------------------------------------------------------------
C------------------- Damage Output : ALL NPTT through ALL Layers --- PID_51, 52
C------------------------------------------------------------------------------
            ELSEIF (IFUNC > 11925+4*MX_PLY_ANIM+6 .AND. IFUNC < 11925+4*MX_PLY_ANIM+107
     .                           .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN  
C-------------------
            ! DAMA/ILAY/UPPER
C-------------------
              IDX = 11925+4*MX_PLY_ANIM+6
              ILAY = MOD((IFUNC - IDX), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IT  = MAX(1,NPTT)
C
              DO I=LFT,LLT
                EVAR(I) = ZERO
              ENDDO
C
              IF (IFAILURE > 0) THEN
                IF (IL <= NLAY .AND. IT <= NPTT) THEN
                  DO I = LFT,LLT                                                              
                    DO IR = 1, NPTR                                                         
                      DO IS = 1, NPTS                                                       
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                        
                        DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                 
                          EVAR(I) = MAX(EVAR(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO ! I = LFT,LLT
                ENDIF ! IF (IL <= NLAY .AND. IT <= NPTT)
              ENDIF ! IF (IFAILURE > 0)
C---
C for outp of dam inside law25
C---
              IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                IF (ITY == 3) THEN
                  DO I=LFT,LLT
                    MAT(I)=IXC(1,NFT+I)
                    PID(I)=IXC(6,NFT+I)
                  ENDDO                                
                ELSE
                  DO I=LFT,LLT
                    MAT(I)=IXTG(1,NFT+I)
                    PID(I)=IXTG(5,NFT+I)
                  ENDDO                                
                ENDIF
C
                IPMAT = 2 + NLAY                         
                DO N=1,NLAY
                  IADR = (N-1)*NEL                       
                  DO I=LFT,LLT
                    J = IADR+I                            
                    MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                  ENDDO                                
                ENDDO        
C
                IF (IL <= NLAY .AND. IT <= NPTT) THEN
                  DO I=LFT,LLT                                            
                    IADR = (IL - 1)*NEL                           
                    J = IADR + I 
                    VG(1:5)= ZERO
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                        DMAX(I) = ONE/PM(64,MATLY(J))                               
                        WPMAX(I)= ONE/PM(41,MATLY(J))                               
                        EPST1(I)= PM(60,MATLY(J))                               
                        EPST2(I)= PM(61,MATLY(J))                               
                        EPSF1(I)= ONE/PM(98,MATLY(J))                               
                        EPSF2(I)= ONE/PM(99,MATLY(J))
C 
                        VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I))
                        VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I))
                        VG(3) = MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                        IF (LBUF%CRAK(JJ(1)+I) > ZERO) VG(4) = MAX(VG(4),
     .                     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I))
                        IF (LBUF%CRAK(JJ(2)+I) > ZERO) VG(5) = MAX(VG(5), 
     .                     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                      ENDDO
                    ENDDO
                    EVAR(I) = MAX(EVAR(I),VG(1),VG(2),VG(3),VG(4),VG(5))
                  ENDDO ! I=LFT,JLT
                ENDIF ! IF (IL <= NLAY .AND. IT <= NPTT)
              ENDIF ! IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52 ))
C-------------------
            ELSEIF (IFUNC > 11925+4*MX_PLY_ANIM+106 .AND. IFUNC < 11925+4*MX_PLY_ANIM+207
     .                           .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN  
C-------------------
            ! DAMA/ILAY/LOWER
C-------------------
              IDX = 11925+4*MX_PLY_ANIM+106
              ILAY = MOD((IFUNC - IDX), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              IT  = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
C
              DO I=LFT,LLT
                EVAR(I) = ZERO
              ENDDO
C
              IF (IFAILURE > 0) THEN
                IF (IL <= NLAY .AND. IT <= NPTT) THEN
                  DO I = LFT,LLT                                                              
                    DO IR = 1, NPTR                                                         
                      DO IS = 1, NPTS                                                       
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                        
                        DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                 
                          EVAR(I) = MAX(EVAR(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO ! I = LFT,LLT
                ENDIF ! IF (IL <= NLAY .AND. IT <= NPTT)
              ENDIF ! IF (IFAILURE > 0) THEN
C---
C for outp of dam inside law25
C---
              IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                IF (ITY == 3) THEN
                  DO I=LFT,LLT
                    MAT(I)=IXC(1,NFT+I)
                    PID(I)=IXC(6,NFT+I)
                  ENDDO                                
                ELSE
                  DO I=LFT,LLT
                    MAT(I)=IXTG(1,NFT+I)
                    PID(I)=IXTG(5,NFT+I)
                  ENDDO                                
                ENDIF
C
                IPMAT = 2 + NLAY                         
                DO N=1,NLAY
                  IADR = (N-1)*NEL                       
                  DO I=LFT,LLT
                    J = IADR+I                            
                    MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                  ENDDO                                
                ENDDO        
C
                IF (IL <= NLAY .AND. IT <= NPTT) THEN
                  DO I=LFT,LLT                                            
                    IADR = (IL - 1)*NEL                           
                    J = IADR + I 
                    VG(1:5)= ZERO
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                        DMAX(I) = ONE/PM(64,MATLY(J))                               
                        WPMAX(I)= ONE/PM(41,MATLY(J))                               
                        EPST1(I)= PM(60,MATLY(J))                               
                        EPST2(I)= PM(61,MATLY(J))                               
                        EPSF1(I)= ONE/PM(98,MATLY(J))                               
                        EPSF2(I)= ONE/PM(99,MATLY(J))
C 
                        VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I))
                        VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I))
                        VG(3) = MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                        IF (LBUF%CRAK(JJ(1)+I) > ZERO) VG(4) = MAX(VG(4),
     .                     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I))
                        IF (LBUF%CRAK(JJ(2)+I) > ZERO) VG(5) = MAX(VG(5), 
     .                     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                      ENDDO
                    ENDDO
                    EVAR(I) = MAX(EVAR(I),VG(1),VG(2),VG(3),VG(4),VG(5))
                  ENDDO ! I=LFT,JLT
                ENDIF ! IF (IL <= NLAY .AND. IT <= NPTT)
              ENDIF ! IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52 ))
C-------------------
            ELSEIF (IFUNC > 11925+4*MX_PLY_ANIM+206 .AND. IFUNC < 11925+4*MX_PLY_ANIM+307
     .                           .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN  
C-------------------
            ! DAMA/ILAY/MEMB
C-------------------
              IDX = 11925+4*MX_PLY_ANIM+206
              ILAY = MOD((IFUNC - IDX), 100)
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IT = NPTT/2 + 1   !  MEMB of layer ILAY
C
              DO I=LFT,LLT
                EVAR(I) = ZERO
              ENDDO
C
              IF (IFAILURE > 0) THEN
                IF (IL <= NLAY .AND. IT <= NPTT) THEN
                  DO I = LFT,LLT                                                              
                    DO IR = 1, NPTR                                                         
                      DO IS = 1, NPTS                                                       
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                        
                        DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                 
                          EVAR(I) = MAX(EVAR(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO ! I = LFT,LLT
                ENDIF ! IF (IL <= NLAY .AND. IT <= NPTT)
              ENDIF ! IF (IFAILURE > 0) THEN
C---
C for outp of dam inside law25
C---
              IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                IF (ITY == 3) THEN
                  DO I=LFT,LLT
                    MAT(I)=IXC(1,NFT+I)
                    PID(I)=IXC(6,NFT+I)
                  ENDDO                                
                ELSE
                  DO I=LFT,LLT
                    MAT(I)=IXTG(1,NFT+I)
                    PID(I)=IXTG(5,NFT+I)
                  ENDDO                                
                ENDIF
C
                IPMAT = 2 + NLAY                         
                DO N=1,NLAY
                  IADR = (N-1)*NEL                       
                  DO I=LFT,LLT
                    J = IADR+I                            
                    MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                  ENDDO                                
                ENDDO        
C
                IF (IL <= NLAY .AND. IT <= NPTT) THEN
                  DO I=LFT,LLT                                            
                    IADR = (IL - 1)*NEL                           
                    J = IADR + I 
                    VG(1:5)= ZERO
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                        DMAX(I) = ONE/PM(64,MATLY(J))                               
                        WPMAX(I)= ONE/PM(41,MATLY(J))                               
                        EPST1(I)= PM(60,MATLY(J))                               
                        EPST2(I)= PM(61,MATLY(J))                               
                        EPSF1(I)= ONE/PM(98,MATLY(J))                               
                        EPSF2(I)= ONE/PM(99,MATLY(J))
C 
                        VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I))
                        VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I))
                        VG(3) = MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                        IF (LBUF%CRAK(JJ(1)+I) > ZERO) VG(4) = MAX(VG(4),
     .                     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I))
                        IF (LBUF%CRAK(JJ(2)+I) > ZERO) VG(5) = MAX(VG(5), 
     .                     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                      ENDDO
                    ENDDO
                    EVAR(I) = MAX(EVAR(I),VG(1),VG(2),VG(3),VG(4),VG(5))
                  ENDDO ! I=LFT,JLT
                ENDIF ! IF (IL <= NLAY .AND. IT <= NPTT)
              ENDIF ! IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52 ))
C-------------------
            ELSEIF (IFUNC > 11925+4*MX_PLY_ANIM+306 .AND. IFUNC < 11925+4*MX_PLY_ANIM+1317
     .                           .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN  
C-------------------
            ! DAMA/ILAY/IPT
C-------------------
              IDX = 11925+4*MX_PLY_ANIM+306
              IUS = IFUNC - IDX
              IL  = INT((IUS - 1)/10)
              IT = IUS - 10*IL
C
              DO I=LFT,LLT
                EVAR(I) = ZERO
              ENDDO
C
              IF (IFAILURE > 0) THEN
                IF (IL <= NLAY) THEN
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  IF (IT <= NPTT) THEN
                    DO I = LFT,LLT                                                              
                      DO IR = 1, NPTR                                                         
                        DO IS = 1, NPTS                                                       
                          FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                        
                          DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                 
                            EVAR(I) = MAX(EVAR(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO ! I = LFT,LLT
                  ENDIF ! IF IT <= NPTT)
                ENDIF ! IF (IL <= NLAY )
              ENDIF ! IF(IFAILURE > 0) THEN
C---
C for outp of dam inside law25
C---
              IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                IF (ITY == 3) THEN
                  DO I=LFT,LLT
                    MAT(I)=IXC(1,NFT+I)
                    PID(I)=IXC(6,NFT+I)
                  ENDDO                                
                ELSE
                  DO I=LFT,LLT
                    MAT(I)=IXTG(1,NFT+I)
                    PID(I)=IXTG(5,NFT+I)
                  ENDDO                                
                ENDIF
C
                IPMAT = 2 + NLAY                         
                DO N=1,NLAY
                  IADR = (N-1)*NEL                       
                  DO I=LFT,LLT
                    J = IADR+I                            
                    MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                  ENDDO                                
                ENDDO        
C
                IF (IL <= NLAY) THEN
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  IF (IT <= NPTT) THEN
                    DO I=LFT,LLT                                            
                      IADR = (IL - 1)*NEL                           
                      J = IADR + I 
                      VG(1:5)= ZERO
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                          DMAX(I) = ONE/PM(64,MATLY(J))                               
                          WPMAX(I)= ONE/PM(41,MATLY(J))                               
                          EPST1(I)= PM(60,MATLY(J))                               
                          EPST2(I)= PM(61,MATLY(J))                               
                          EPSF1(I)= ONE/PM(98,MATLY(J))                               
                          EPSF2(I)= ONE/PM(99,MATLY(J))
C 
                          VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I))
                          VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I))
                          VG(3) = MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                          IF (LBUF%CRAK(JJ(1)+I) > ZERO) VG(4) = MAX(VG(4),
     .                       (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I))
                          IF (LBUF%CRAK(JJ(2)+I) > ZERO) VG(5) = MAX(VG(5), 
     .                       (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                        ENDDO
                      ENDDO
                      EVAR(I) = MAX(EVAR(I),VG(1),VG(2),VG(3),VG(4),VG(5))
                    ENDDO ! I=LFT,JLT
                  ENDIF ! IF (IT <= NPTT)
                ENDIF ! IF (IL <= NLAY)
              ENDIF ! IF (MLW == 25 .AND. (IGTYP == 51 .OR. IGTYP == 52 ))
              
            ELSEIF(IFUNC == 13242 + 4*MX_PLY_ANIM )THEN
              IF(GBUF%G_DT>0)THEN
                DO I=LFT,LLT
                  EVAR(I) = GBUF%DT(I) 
                ENDDO
               ENDIF
C
            ELSEIF(IFUNC == 13243 + 4*MX_PLY_ANIM )THEN
              IF(GBUF%G_ISMS>0)THEN
                DO I=LFT,LLT
                  EVAR(I) = GBUF%ISMS(I) 
                ENDDO 
              ENDIF   
              
            ELSEIF(IFUNC == 13245 + 4*MX_PLY_ANIM  .AND. (MLW == 15 .OR. MLW == 25))THEN
! it's the plastic work  /ANIM/ELEM/WPLA ( ifunc=4*MX_PLY_ANIM +13245)           
              IF (GBUF%G_PLA > 0) THEN
                ! for law25, plastic work < 0 if the layer has reached failure-p !
                IF (NLAY > 1) THEN
                  IPT = IABS(NLAY)/2 + 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                  IF (BUFLY%L_PLA > 0)THEN
                  IF (NPG > 1) THEN
                    DO  I=LFT,LLT  
                      EVAR(I) = ABS(BUFLY%PLAPT(I))
                    ENDDO
                  ELSE
                    NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                    DO  I=LFT,LLT
                      DO IT=1,NPTT
                        EVAR(I) = EVAR(I) + ABS(BUFLY%LBUF(1,1,IT)%PLA(I))/NPTT
                      ENDDO
                    ENDDO
                  ENDIF
                 ENDIF
                ELSEIF (NPG > 1) THEN   ! NLAY = 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                  IPT = IABS(NPTT)/2 + 1
cc                  IPT = IABS(NPT)/2 + 1
                  II = (IPT-1)*NEL  
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%PLAPT(II+I))
                  ENDDO
                 ENDIF
                ELSE   !  NLAY=1, NPG=1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
cc                  IPT = IABS(NPT)/2 + 1
                  IPT = IABS(NPTT)/2 + 1
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I)) 
                  ENDDO
                 ENDIF
                ENDIF
              ENDIF  
c------------------------------------
            ELSEIF (IFUNC == 13246 + 4*MX_PLY_ANIM .AND. (MLW == 15 .OR. MLW == 25)) THEN  ! WPLA/UPPER
c------------------------------------
              IF (NLAY > 1) THEN                   
                IL  = MAX(1,NPT)            
                IPT = 1                            
              ELSE                                 
                IL  = 1                            
                IPT = MAX(1,NPT)
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              IF (BUFLY%L_PLA > 0) THEN
                IF (NPG > 1) THEN
                  IF (IGTYP == 51 .OR. IGTYP == 52) IPT = BUFLY%NPTT
                  DO I=LFT,LLT
                     DO IR=1,NPTR
                       DO IS=1,NPTS
                         LBUF => BUFLY%LBUF(IR,IS,IPT)
                         EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                       ENDDO
                     ENDDO
                  ENDDO
                ELSE
                  IF (IGTYP == 51 .OR. IGTYP == 52) IPT = BUFLY%NPTT
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))
                  ENDDO
                ENDIF
              ENDIF 
c------------------------------------
            ELSEIF (IFUNC == 13247 + 4*MX_PLY_ANIM .AND. (MLW == 15 .OR. MLW == 25 )) THEN ! WPLA/LOWER
c------------------------------------
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)
              IF (BUFLY%L_PLA > 0) THEN
                IF (NPG > 1) THEN
                   DO I=LFT,LLT
                     DO IR=1,NPTR
                       DO IS=1,NPTS
                         LBUF => BUFLY%LBUF(IR,IS,1)
                         EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                       ENDDO
                     ENDDO
                   ENDDO
                ELSE
                  DO  I=LFT,LLT
                    EVAR(I) = ABS(BUFLY%LBUF(1,1,1)%PLA(I))
                  ENDDO
                ENDIF
              ENDIF 
c------------------------------------
            ELSEIF (IFUNC > 13247 + 4*MX_PLY_ANIM .AND. IFUNC <= 13347 + 4*MX_PLY_ANIM .AND.
     .              (MLW == 15 .OR. MLW == 25)) THEN  ! anim/shell/wpla/layer
c------------------------------------
              ILAY = MOD((IFUNC - 13247 - 4*MX_PLY_ANIM), 100)
              IF (ILAY == 0) ILAY = 100
              IF ((ILAY <= NLAY .or. ILAY <= MPT) .and. GBUF%G_PLA > 0) THEN
                IF (NPT == 0) THEN
                  IL  = 1 
                  IPT = 1
                ELSEIF (NLAY > 1) THEN                   
                  IL = ILAY
                  IPT = 1                            
                ELSE                                 
                  IL  = 1                            
                  IPT = ILAY
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                IF (BUFLY%L_PLA > 0) THEN
                   IF (NPG > 1) THEN
                     IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
C                       PROP/51 : more than 1 integration point by layer: average value per layer 
                        NPTT = BUFLY%NPTT
                        NPGT = NPG*NPTT
                        DO I=LFT,LLT
                          DO IT=1,NPTT
                            DO IR=1,NPTR
                              DO IS=1,NPTS
                                LBUF => BUFLY%LBUF(IR,IS,IT)
                                EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPGT
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO
                      ELSE
                        DO I=LFT,LLT
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              LBUF => BUFLY%LBUF(IR,IS,IPT)
                              EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDIF 
                   ELSE
                     IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                        NPTT = BUFLY%NPTT
                        DO I=LFT,LLT
                          DO IT=1,NPTT
                           EVAR(I) = EVAR(I) + ABS(BUFLY%LBUF(1,1,IT)%PLA(I))/NPTT
                          ENDDO
                        ENDDO
                      ELSE
                        DO I=LFT,LLT
                           EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))
                        ENDDO
                      ENDIF 
                  ENDIF
                 ENDIF  
               ENDIF  
c------------------------------------
            ELSEIF (IFUNC > 13347 + 4*MX_PLY_ANIM .AND. IFUNC <= 13447 + 4*MX_PLY_ANIM .AND. 
     .       (IGTYP == 51 .OR. IGTYP == 52) .AND. (MLW == 15 .OR. MLW == 25) ) THEN  
            ! WPLA/ILAY/UPPER
c------------------------------------
C available for IGTYP = 51 and 52  only (to be generalized)
              ILAY = MOD ((IFUNC - 13347 - 4*MX_PLY_ANIM), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IPT  = MAX(1,NPTT)
              IF (BUFLY%L_PLA > 0 .AND.
     .           (IL <= NLAY .AND. IPT <= NPTT))THEN
                IF (NPG > 1) THEN
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE ! (NPG == 1)
                  LBUF => BUFLY%LBUF(1,1,IPT)
                  DO I=LFT,LLT
                    EVAR(I) = ABS(LBUF%PLA(I))
                  ENDDO
                ENDIF ! IF (NPG > 1) THEN
              ENDIF ! IF (BUFLY%L_PLA > 0) THEN
c------------------------------------
            ELSEIF (IFUNC > 13447 + 4*MX_PLY_ANIM .AND. IFUNC <= 13547 + 4*MX_PLY_ANIM .AND. 
     .       (IGTYP == 51 .OR. IGTYP == 52) .AND. (MLW == 15 .OR. MLW == 25) ) THEN   
            ! WPLA/ILAY/LOWER
c------------------------------------
C available for IGTYP = 51 and 52 only (to be generalized)
              ILAY = MOD ((IFUNC - 13447 - 4*MX_PLY_ANIM), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              IPT  = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IF (BUFLY%L_PLA > 0 .AND.
     .           (IL <= NLAY .AND. IPT <= NPTT))THEN
                IF (NPG > 1) THEN
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE ! (NPG == 1)
                  LBUF => BUFLY%LBUF(1,1,IPT)
                  DO I=LFT,LLT
                    EVAR(I) = ABS(LBUF%PLA(I))
                  ENDDO
                ENDIF ! IF (NPG > 1) THEN
              ENDIF ! IF (BUFLY%L_PLA > 0) THEN
c------------------------------------
            ELSEIF (IFUNC > 13547 + 4*MX_PLY_ANIM .AND. IFUNC <= 14547 + 4*MX_PLY_ANIM .AND. 
     .       (IGTYP == 51 .OR. IGTYP == 52) .AND. (MLW == 15 .OR. MLW == 25) )  THEN 
            ! WPLA/ILAY/NIP
c------------------------------------
C
C available for IGTYP = 51 and 52 only (to be generalized)
C
              IUS = IFUNC - 13547 - 4*MX_PLY_ANIM 
              IL  = INT((IUS - 1)/10)
              IPT = IUS - 10*IL
              IL = IL + 1
              IF (IL <= NLAY ) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT    
                IF (BUFLY%L_PLA > 0 .AND. IPT <= NPTT) THEN
                  IF (NPG > 1) THEN
                    DO I=LFT,LLT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => BUFLY%LBUF(IR,IS,IPT)
                          EVAR(I) = EVAR(I) + ABS(LBUF%PLA(I))/NPG
                        ENDDO
                      ENDDO
                    ENDDO
                  ELSE ! (NPG == 1)
                    LBUF => BUFLY%LBUF(1,1,IPT)
                    DO I=LFT,LLT
                      EVAR(I) = ABS(LBUF%PLA(I))
                    ENDDO
                  ENDIF ! IF (NPG > 1) THEN
                ENDIF ! IF (BUFLY%L_PLA > 0) THEN
              ENDIF ! IF (IL <= NLAY )
             ! next IFUNC = 14548 + 4*MX_PLY_ANIM   
c------------------------------------ OFF
            ELSEIF (IFUNC == 13547 + 4*MX_PLY_ANIM + 1000 + 1)  THEN 
              DO I=LFT,LLT
                IF (GBUF%G_OFF > 0) THEN
                  IF(GBUF%OFF(I) > ONE) THEN
                    EVAR(I) = GBUF%OFF(I) - ONE
                  ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                    EVAR(I) = GBUF%OFF(I)
                  ELSE
                    EVAR(I) = -ONE
                  ENDIF
                ENDIF
              ENDDO   
             ! next IFUNC = 13547 + 4*MX_PLY_ANIM + 1000 + 2
c------------------------------------ Mach Number
            ELSEIF(IFUNC == 13547 + 4*MX_PLY_ANIM + 1000 + 2)THEN                                
              IF (MLW == 151) THEN                                                                        
                 DO I = 1, NEL                                                                            
                    VEL(1) = MULTI_FVM%VEL(1, I + NFT)                                                    
                    VEL(2) = MULTI_FVM%VEL(2, I + NFT)                                                    
                    VEL(3) = MULTI_FVM%VEL(3, I + NFT)                                                    
                    VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                              
                    EVAR(I) = VEL(0)/MULTI_FVM%SOUND_SPEED(I + NFT)                                              
                 ENDDO                                                                                    
              ELSEIF(ALEFVM_Param%ISOLVER>1)THEN                                                                    
                   L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                                                       
                   IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                            
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                          
                      DO  I=1,NEL                                                                         
                         VEL(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I)                                       
                         VEL(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)                                       
                         VEL(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)                                       
                         VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                                      
                         EVAR(I) = VEL(0)/LBUF%SSP(I)                                                                 
                      ENDDO                                                                               
                   ENDIF                                                                                  
              ELSE                                                                                 
                L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                                                     
                IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                               
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
                  IF(JALE/=0)THEN                                             
                    DO  I=1,NEL                                                                             
                      TMP(1,1:3)=V(1,IXTG(2:4,I+NFT))-W(1,IXTG(2:4,I+NFT))                                                     
                      TMP(2,1:3)=V(2,IXTG(2:4,I+NFT))-W(2,IXTG(2:4,I+NFT))                                                     
                      TMP(3,1:3)=V(3,IXTG(2:4,I+NFT))-W(3,IXTG(2:4,I+NFT))                                                     
                      VEL(1) = SUM(TMP(1,1:3))*THIRD                                                       
                      VEL(2) = SUM(TMP(2,1:3))*THIRD                                                       
                      VEL(3) = SUM(TMP(3,1:3))*THIRD                                                       
                      EVAR(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)                                       
                    ENDDO                                                                                   
                  ELSE
                    DO  I=1,NEL                                                                             
                      TMP(1,1:3)=V(1,IXTG(2:4,I+NFT))                                                    
                      TMP(2,1:3)=V(2,IXTG(2:4,I+NFT))                                                    
                      TMP(3,1:3)=V(3,IXTG(2:4,I+NFT))                                                    
                      VEL(1) = SUM(TMP(1,1:3))*THIRD                                                       
                      VEL(2) = SUM(TMP(2,1:3))*THIRD                                                       
                      VEL(3) = SUM(TMP(3,1:3))*THIRD                                                       
                      EVAR(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)                                       
                    ENDDO                                                                                   
                  ENDIF
                ENDIF                                                                                     
              ENDIF 
c------------------------------------ DAMG
           ELSEIF((IFUNC >= 13547 + 4*MX_PLY_ANIM + 1000 + 4).AND.
     .           (IFUNC <= 13547 + 4*MX_PLY_ANIM + 1000 + 18).AND.GBUF%G_DMG > 0)THEN
             IDX = 13547 + 4*MX_PLY_ANIM + 1000 + 4
c------------Mean
             IF (IFUNC == IDX) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               NPGT = NPG*NPTT
               DO IL=1,NLAY                                              
                 DO IS=1,NPTS                                            
                   DO IT=1,NPTT                                          
                     DO IR=1,NPTR    
                       LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                       DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%DMG(I)/NPGT  
                       ENDDO
                     ENDDO
                   ENDDO
                 ENDDO
               ENDDO
c------------Upper
             ELSEIF (IFUNC == IDX + 1) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               DO IL=1,NLAY                                              
                 DO IS=1,NPTS 
                   DO IR=1,NPTR                                     
                     LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,NPTT)
                     DO I=LFT,LLT
                       EVAR(I) = EVAR(I) + LBUF%DMG(I)/(NPG*NLAY)
                     ENDDO
                   ENDDO
                 ENDDO
               ENDDO
c------------Lower
             ELSEIF (IFUNC == IDX + 2) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               DO IL=1,NLAY                                              
                 DO IS=1,NPTS 
                   DO IR=1,NPTR                                     
                     LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,1)
                     DO I=LFT,LLT
                       EVAR(I) = EVAR(I) + LBUF%DMG(I)/(NPG*NLAY)
                     ENDDO
                   ENDDO
                 ENDDO
               ENDDO
c----------Membrane
             ELSEIF (IFUNC == IDX + 3) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               ! Odd number of thickness integration points
               IF ((MOD(NPTT,2)/=0).AND.(NPTT>1)) THEN
                 DO IL=1,NLAY                                              
                   DO IS=1,NPTS 
                     DO IR=1,NPTR                                     
                       LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,CEILING(NPTT/TWO))
                       DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%DMG(I)/(NPG*NLAY)
                       ENDDO
                     ENDDO
                   ENDDO
                 ENDDO
               ! Even number of thickness integration points
               ELSEIF ((MOD(NPTT,2)==0).AND.(NPTT>1)) THEN
                 DO IL=1,NLAY                                              
                   DO IS=1,NPTS 
                     DO IR=1,NPTR 
                       LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,NPTT/2) 
                       DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%DMG(I)/(TWO*NPG*NLAY)
                       ENDDO
                       LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,NPTT/2+1) 
                       DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%DMG(I)/(TWO*NPG*NLAY)
                       ENDDO
                     ENDDO
                   ENDDO
                 ENDDO
               ! NPTT = 1
               ELSE
                 DO IL=1,NLAY                                              
                   DO IS=1,NPTS 
                     DO IR=1,NPTR                                     
                       LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,1)
                       DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%DMG(I)/(NPG*NLAY)
                       ENDDO
                     ENDDO
                   ENDDO
                 ENDDO
               ENDIF
c------------At a given integration point in the thickness
             ELSEIF((IFUNC >= IDX + 3 + 1).AND.(IFUNC <= IDX + 3 + 11))THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               IT = IFUNC - (IDX+3)
               IF (IT<=NPTT) THEN
                 DO IL=1,NLAY                                              
                   DO IS=1,NPTS 
                     DO IR=1,NPTR                                     
                       LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                       DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%DMG(I)/(NPG*NLAY)
                       ENDDO
                     ENDDO
                   ENDDO
                 ENDDO
               ENDIF
             ENDIF
c------------------------------------ NL_EPSP
           ELSEIF((IFUNC >= 14567 + 4*MX_PLY_ANIM).AND.
     .            (IFUNC <= 14580 + 4*MX_PLY_ANIM).AND.
     .             GBUF%G_PLANL > 0)THEN
             IDX = 14567 + 4*MX_PLY_ANIM
c------------Mean
             IF (IFUNC == IDX) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               NPGT = NPG*NPTT
               ! (Only 1 layer is supported with non-local)                                           
               DO IS=1,NPTS                                            
                 DO IT=1,NPTT                                          
                   DO IR=1,NPTR    
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                     DO I=LFT,LLT
                       EVAR(I) = EVAR(I) + LBUF%PLANL(I)/NPGT  
                     ENDDO
                   ENDDO
                 ENDDO
               ENDDO
c------------Upper
             ELSEIF (IFUNC == IDX + 1) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO  
               ! (Only 1 layer is supported with non-local)                                            
               DO IS=1,NPTS 
                 DO IR=1,NPTR                                     
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,NPTT)
                   DO I=LFT,LLT
                     EVAR(I) = EVAR(I) + LBUF%PLANL(I)/NPG
                   ENDDO
                 ENDDO
               ENDDO
c------------Lower
             ELSEIF (IFUNC == IDX + 2) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO   
               ! (Only 1 layer is supported with non-local)                                          
               DO IS=1,NPTS 
                 DO IR=1,NPTR                                     
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                   DO I=LFT,LLT
                     EVAR(I) = EVAR(I) + LBUF%PLANL(I)/NPG
                   ENDDO
                 ENDDO
               ENDDO
c------------At a given integration point in the thickness
             ELSEIF((IFUNC >= IDX + 2 + 1).AND.(IFUNC <= IDX + 2 + 11))THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               IT = IFUNC - (IDX+2)
               ! (Only 1 layer is supported with non-local)
               IF (IT<=NPTT) THEN                                             
                 DO IS=1,NPTS 
                   DO IR=1,NPTR                                     
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                     DO I=LFT,LLT
                       EVAR(I) = EVAR(I) + LBUF%PLANL(I)/NPG
                     ENDDO
                   ENDDO
                 ENDDO
               ENDIF
             ENDIF     
c------------------------------------ NL_EPSD
           ELSEIF((IFUNC >= 14581 + 4*MX_PLY_ANIM).AND.
     .            (IFUNC <= 14594 + 4*MX_PLY_ANIM).AND.
     .             GBUF%G_EPSDNL > 0)THEN
             IDX = 14581 + 4*MX_PLY_ANIM
c------------Mean
             IF (IFUNC == IDX) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               NPGT = NPG*NPTT
               ! (Only 1 layer is supported with non-local)                                           
               DO IS=1,NPTS                                            
                 DO IT=1,NPTT                                          
                   DO IR=1,NPTR    
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                     DO I=LFT,LLT
                       EVAR(I) = EVAR(I) + LBUF%EPSDNL(I)/NPGT  
                     ENDDO
                   ENDDO
                 ENDDO
               ENDDO
c------------Upper
             ELSEIF (IFUNC == IDX + 1) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO  
               ! (Only 1 layer is supported with non-local)                                            
               DO IS=1,NPTS 
                 DO IR=1,NPTR                                     
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,NPTT)
                   DO I=LFT,LLT
                     EVAR(I) = EVAR(I) + LBUF%EPSDNL(I)/NPG
                   ENDDO
                 ENDDO
               ENDDO
c------------Lower
             ELSEIF (IFUNC == IDX + 2) THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO   
               ! (Only 1 layer is supported with non-local)                                          
               DO IS=1,NPTS 
                 DO IR=1,NPTR                                     
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                   DO I=LFT,LLT
                     EVAR(I) = EVAR(I) + LBUF%EPSDNL(I)/NPG
                   ENDDO
                 ENDDO
               ENDDO
c------------At a given integration point in the thickness
             ELSEIF((IFUNC >= IDX + 2 + 1).AND.(IFUNC <= IDX + 2 + 11))THEN
               DO I = LFT, LLT
                 EVAR(I) = ZERO
               ENDDO
               IT = IFUNC - (IDX+2)
               ! (Only 1 layer is supported with non-local)
               IF (IT<=NPTT) THEN                                             
                 DO IS=1,NPTS 
                   DO IR=1,NPTR                                     
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                     DO I=LFT,LLT
                       EVAR(I) = EVAR(I) + LBUF%EPSDNL(I)/NPG
                     ENDDO
                   ENDDO
                 ENDDO
               ENDIF
             ENDIF
c------------------------------------ TSAIWU           
            ELSEIF (IFUNC == 14595 + 4*MX_PLY_ANIM  .AND. (GBUF%G_TSAIWU > 0)) THEN
              ! /ANIM/ELEM/TSAIWU ( IFUNC = 4*MX_PLY_ANIM + 14595)           
              IF (NLAY > 1) THEN
                IPT  = IABS(NLAY)/2 + 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)             
                NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                DO I=LFT,LLT
                  DO IR=1,NPTR
                    DO IS=1,NPTS   
                      DO IT=1,NPTT
                        EVAR(I) = EVAR(I) + BUFLY%LBUF(IR,IS,IT)%TSAIWU(I)/(NPTT*NPTR*NPTS)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ELSE  
                BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                IF (BUFLY%L_TSAIWU > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                  IPT  = IABS(NPTT)/2 + 1
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS   
                        EVAR(I) = EVAR(I) + BUFLY%LBUF(IR,IS,IPT)%TSAIWU(I)/(NPTR*NPTS)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF  
c------------------------------------
            ELSEIF (IFUNC == 14596 + 4*MX_PLY_ANIM .AND. (GBUF%G_TSAIWU > 0)) THEN  ! TSAIWU/UPPER
c------------------------------------
              IF (NLAY > 1) THEN                   
                IL  = MAX(1,NPT)            
                IPT = 1                            
              ELSE                                 
                IL  = 1                            
                IPT = MAX(1,NPT)
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              IF (BUFLY%L_TSAIWU > 0) THEN
                IF (NPG > 1) THEN
                  IF (IGTYP == 51 .OR. IGTYP == 52) IPT = BUFLY%NPTT
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE
                  IF (IGTYP == 51 .OR. IGTYP == 52) IPT = BUFLY%NPTT
                  DO  I=LFT,LLT
                    EVAR(I) = BUFLY%LBUF(1,1,IPT)%TSAIWU(I)
                  ENDDO
                ENDIF
              ENDIF 
c------------------------------------
            ELSEIF (IFUNC == 14597 + 4*MX_PLY_ANIM .AND. (GBUF%G_TSAIWU > 0)) THEN ! TSAIWU/LOWER
c------------------------------------
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)
              IF (BUFLY%L_TSAIWU > 0) THEN
                IF (NPG > 1) THEN
                   DO I=LFT,LLT
                     DO IR=1,NPTR
                       DO IS=1,NPTS
                         LBUF => BUFLY%LBUF(IR,IS,1)
                         EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPG
                       ENDDO
                     ENDDO
                   ENDDO
                ELSE
                  DO  I=LFT,LLT
                    EVAR(I) = BUFLY%LBUF(1,1,1)%TSAIWU(I)
                  ENDDO
                ENDIF
              ENDIF 
c------------------------------------
            ELSEIF (IFUNC > 14597 + 4*MX_PLY_ANIM .AND. IFUNC <= 14697 + 4*MX_PLY_ANIM .AND.
     .              (GBUF%G_TSAIWU > 0)) THEN  ! anim/shell/tsaiwu/layer
c------------------------------------
              ILAY = MOD((IFUNC - 14597 - 4*MX_PLY_ANIM), 100)
              IF (ILAY == 0) ILAY = 100
              IF ((ILAY <= NLAY .OR. ILAY <= MPT) .AND. GBUF%G_TSAIWU > 0) THEN
                IF (NPT == 0) THEN
                  IL  = 1 
                  IPT = 1
                ELSEIF (NLAY > 1) THEN                   
                  IL  = ILAY
                  IPT = 1                            
                ELSE                                 
                  IL  = 1                            
                  IPT = ILAY
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                IF (BUFLY%L_TSAIWU > 0) THEN
                  IF (NPG > 1) THEN
                    IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
C                     PROP/51 : more than 1 integration point by layer: average value per layer 
                      NPTT = BUFLY%NPTT
                      NPGT = NPG*NPTT
                      DO I=LFT,LLT
                        DO IT=1,NPTT
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              LBUF => BUFLY%LBUF(IR,IS,IT)
                              EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPGT
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDDO
                    ELSE
                      DO I=LFT,LLT
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            LBUF => BUFLY%LBUF(IR,IS,IPT)
                            EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPG
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDIF 
                  ELSE
                    IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                      NPTT = BUFLY%NPTT
                      DO I = LFT,LLT
                        DO IT = 1,NPTT
                          EVAR(I) = EVAR(I) + BUFLY%LBUF(1,1,IT)%TSAIWU(I)/NPTT
                        ENDDO
                      ENDDO
                    ELSE
                      DO I=LFT,LLT
                        EVAR(I) = BUFLY%LBUF(1,1,IPT)%TSAIWU(I)
                      ENDDO
                    ENDIF 
                  ENDIF
                 ENDIF  
               ENDIF  
c------------------------------------
            ELSEIF (IFUNC > 14697 + 4*MX_PLY_ANIM .AND. IFUNC <= 14797 + 4*MX_PLY_ANIM .AND. 
     .       (IGTYP == 51 .OR. IGTYP == 52) .AND. (GBUF%G_TSAIWU > 0) ) THEN  
            ! TSAIWU/ILAY/UPPER
c------------------------------------
C available for IGTYP = 51 and 52  only (to be generalized)
              ILAY = MOD ((IFUNC - 14697 - 4*MX_PLY_ANIM), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY  > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IPT  = MAX(1,NPTT)
              IF (BUFLY%L_TSAIWU > 0 .AND.
     .           (IL <= NLAY .AND. IPT <= NPTT))THEN
                IF (NPG > 1) THEN
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE ! (NPG == 1)
                  LBUF => BUFLY%LBUF(1,1,IPT)
                  DO I=LFT,LLT
                    EVAR(I) = LBUF%TSAIWU(I)
                  ENDDO
                ENDIF ! IF (NPG > 1) THEN
              ENDIF ! IF (BUFLY%L_TSAIWU > 0) THEN
c------------------------------------
            ELSEIF (IFUNC > 14797 + 4*MX_PLY_ANIM .AND. IFUNC <= 14897 + 4*MX_PLY_ANIM .AND. 
     .       (IGTYP == 51 .OR. IGTYP == 52) .AND. (GBUF%G_TSAIWU > 0) ) THEN   
            ! WPLA/ILAY/LOWER
c------------------------------------
C available for IGTYP = 51 and 52 only (to be generalized)
              ILAY = MOD ((IFUNC - 14797 - 4*MX_PLY_ANIM), 100)
              IF (ILAY == 0) ILAY = 100
              IF (NLAY > 1) THEN
                IL = MAX(1,ILAY)
              ELSE
                IL = 1
              ENDIF
              IPT  = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT = BUFLY%NPTT
              IF (BUFLY%L_TSAIWU > 0 .AND.
     .           (IL <= NLAY .AND. IPT <= NPTT))THEN
                IF (NPG > 1) THEN
                  DO I=LFT,LLT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                        EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPG
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE ! (NPG == 1)
                  LBUF => BUFLY%LBUF(1,1,IPT)
                  DO I=LFT,LLT
                    EVAR(I) = LBUF%TSAIWU(I)
                  ENDDO
                ENDIF ! IF (NPG > 1) THEN
              ENDIF ! IF (BUFLY%L_TSAIWU > 0) THEN
c------------------------------------
            ELSEIF (IFUNC > 14897 + 4*MX_PLY_ANIM .AND. IFUNC <= 15897 + 4*MX_PLY_ANIM .AND. 
     .       (IGTYP == 51 .OR. IGTYP == 52) .AND. (GBUF%G_TSAIWU > 0) )  THEN 
            ! TSAIWU/ILAY/NIP
c------------------------------------
C
C available for IGTYP = 51 and 52 only (to be generalized)
C
              IUS = IFUNC - 14897 - 4*MX_PLY_ANIM 
              IL  = INT((IUS - 1)/10)
              IPT = IUS - 10*IL
              IL = IL + 1
              IF (IL <= NLAY ) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT    
                IF (BUFLY%L_TSAIWU > 0 .AND. IPT <= NPTT) THEN
                  IF (NPG > 1) THEN
                    DO I=LFT,LLT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => BUFLY%LBUF(IR,IS,IPT)
                          EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPG
                        ENDDO
                      ENDDO
                    ENDDO
                  ELSE ! (NPG == 1)
                    LBUF => BUFLY%LBUF(1,1,IPT)
                    DO I=LFT,LLT
                      EVAR(I) = LBUF%TSAIWU(I)
                    ENDDO
                  ENDIF ! IF (NPG > 1) THEN
                ENDIF ! IF (BUFLY%L_TSAIWU > 0) THEN
              ENDIF ! IF (IL <= NLAY )
C
             ! next IFUNC = 15898 + 4*MX_PLY_ANIM               
C-------------------------------------
            ENDIF ! IFUNC SHELLS
C-------------------------------------
            IF (MLW == 0 .OR. MLW == 13) THEN
              IF (ITY == 3) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN4+N)) = ZERO
                ENDDO
              ELSE  ! ITY = 7
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN5+N)) = ZERO 
                ENDDO
              ENDIF
C-------------------
            ELSEIF (IFUNC == 3 .AND. MLW /= 151) THEN
C             energie specifique
C-------------------
              IF (ITY == 3) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN4+N)) = EVAR(I)/
     .                MAX(EM30,MASS(EL2FA(NN4+N)))
                ENDDO
              ELSE  ! ITY = 7
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN5+N)) = EVAR(I)/
     .                MAX(EM30,MASS(EL2FA(NN5+N)))
                ENDDO
              ENDIF
C-------------------
            ELSEIF (IFUNC == 25.AND.ITY == 3) THEN
C             energie hourglass
C-------------------
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN4+N)) = EHOUR(N+NUMELS)/
     .              MAX(EM30,MASS(EL2FA(NN4+N)))
              ENDDO
C-------------------
            ELSE  ! IFUNC SHELLS
C             cas general
C-------------------
              IF(ITY == 3)THEN
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN4+N)) = EVAR(I)
                ENDDO
              ELSE  ! ITY = 7
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN5+N)) = EVAR(I)
                ENDDO
              ENDIF
            ENDIF  ! IFUNC
C-----------------------------------------------
          ENDIF  ! ITY 
C-----------------------------------------------
        END DO   !  OFFSET
       ENDIF     ! MLW /= 13
      ENDDO!next NG 
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        DO N=1,NBF
          R4 = FUNC(N)
          CALL WRITE_R_C(R4,1)
        ENDDO
      ELSE
        DO N = 1, NBF_L
          WAL(N) = FUNC(N)
        ENDDO
C
        IF (ISPMD == 0) THEN
          BUF = (NUMELQG+NUMELCG+NUMELTGG)*4
        ELSE
          BUF=1
        ENDIF
          CALL SPMD_R4GET_PARTN(1,NBF_L,NBPART,IADG,WAL,BUF)
      ENDIF
c-----------
      IF(ALLOCATED(WA_L))DEALLOCATE(WA_L)
      RETURN
      END
Chd|====================================================================
Chd|  CLSCONV3                      source/output/anim/generate/dfuncc.F
Chd|-- called by -----------
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        STAT_C_ORTH_LOC               source/output/sta/stat_c_orth_loc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CLSCONV3(
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z,E3X, E3Y, E3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IREP
      my_real
     .   RX , RY , RZ, SX , SY , SZ,
     .   E1X, E1Y, E1Z,E2X, E2Y, E2Z,E3X, E3Y, E3Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      MY_REAL
     .   C1,C2,CC,DET
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C---------E3------------
       E3X = RY * SZ - RZ * SY 
       E3Y = RZ * SX - RX * SZ 
       E3Z = RX * SY - RY * SX 
       DET  = SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z)
       DET =MAX(EM20,DET)
       CC = ONE / DET
       E3X = E3X * CC 
       E3Y = E3Y * CC 
       E3Z = E3Z * CC 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        C1 = SQRT(RX*RX + RY*RY + RZ*RZ)
        C2 = SQRT(SX*SX + SY*SY + SZ*SZ)
        E1X = RX*C2+(SY*E3Z-SZ*E3Y)*C1
        E1Y = RY*C2+(SZ*E3X-SX*E3Z)*C1
        E1Z = RZ*C2+(SX*E3Y-SY*E3X)*C1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        C1 = SQRT(E1X*E1X + E1Y*E1Y + E1Z*E1Z)
        IF ( C1/=ZERO) C1 = ONE / C1
        E1X = E1X*C1
        E1Y = E1Y*C1
        E1Z = E1Z*C1
        E2X = E3Y * E1Z - E3Z * E1Y
        E2Y = E3Z * E1X - E3X * E1Z
        E2Z = E3X * E1Y - E3Y * E1X
C	
      RETURN
      END
