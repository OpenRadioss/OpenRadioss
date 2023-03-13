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
Chd|  H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_SCALAR              source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SCHLIEREN                     source/output/anim/generate/schlieren.F
Chd|        SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_QUAD_SCALAR(
     .                  ELBUF_TAB   ,QUAD_SCALAR,IFUNC   ,IPARG        ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM          ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,ALE_CONNECT      ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_QUAD,IPARTQ,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD   ,
     .                  BUFMAT      ,MULTI_FVM ,IR_INPUT  ,IS_INPUT    ,IT_INPUT    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD   
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "mmale51_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "alefvm.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   QUAD_SCALAR(*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46,ID_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_QUAD(*),IPARTQ(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,
     .   IR_INPUT,IS_INPUT,IT_INPUT
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   EVAR(MVSIZ),DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   VALUE(MVSIZ),FF0,GG0,HH0,LL0,MM0,NN0,MASS(MVSIZ),PRES(MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),BUFMAT(*),VFRAC(MVSIZ,1:21),
     .   S11,S22,S33,S4,S5,S6,CRIT,VALUE1,VALUE2,VEL(0:3),TMP(3,4),
     .   NX,NY,NZ,CUMUL(3),SURF,VX,VY,VZ,VOL,MASS0
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),ID_PLY,NB_PLYOFF,IOK,IADBUF,NUPARAM,
     .        IMAT,IVISC,IPOS,ITRIMAT,IU(4),IEOS
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,IOK_PART(MVSIZ),JJ(6),NPGT,IUVAR,
     .        IS_WRITTEN_VALUE(MVSIZ),NFRAC,KFACE,NB_FACE,IV,ISUBMAT,IS_EULER,IS_ALE,IAD2,NVAREOS,
     .        NTILLOTSON,IMAT_TILLOTSON
      CHARACTER*5 BUFF
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF    
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      TYPE(BUF_EOS_)  ,POINTER :: EBUF      
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
      TYPE(BUF_MAT_)  ,POINTER :: MBUF 
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      TARGET :: BUFMAT
      my_real PI_
      DATA PI_/3.141592653589793238462643/
C-----------------------------------------------
      ILAY = LAYER_INPUT
      IUVAR = IUVAR_INPUT
      IR = IR_INPUT
      IS = IS_INPUT
      IT = IT_INPUT

      DO I=1,NUMELQ
         IS_WRITTEN_QUAD(I) = 0
      ENDDO
C
      !-------------------------------------------------------!
      !     INITIALIZATION IF SCHLIEREN DEFINED               !
      !-------------------------------------------------------!      
      IF(KEYWORD == 'SCHLIEREN')THEN
        CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM)  
      ENDIF
C-----------
      DO 900 NG=1,NGROUP
       CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF(MLW /= 13) THEN
          NFT   =IPARG(3,NG)
          IAD   =IPARG(4,NG)
          ISUBSTACK = IPARG(71,NG)
          IVISC = IPARG(61,NG)
          IOK_PART(1:NEL) = 0 
          LFT=1
          LLT=NEL
          IS_EULER=IPARG(11,NG)
          IS_ALE=IPARG(7,NG)
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          DO I=1,NEL
            VALUE(I) = ZERO
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	     
C-----------------------------------------------
C           QUAD
C-----------------------------------------------
         IF (ITY == 2) THEN

            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
            EBUF => ELBUF_TAB(NG)%BUFLY(1)%EOS(1,1,1)
            JALE=(IPARG(7,NG)+IPARG(11,NG))
            JTURB=IPARG(12,NG)*JALE
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
            NPTT  = ELBUF_TAB(NG)%NPTT
            NLAY  = ELBUF_TAB(NG)%NLAY
            NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
            NVAREOS = ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS
c
            DO  I=1,NEL 
              ID_ELEM(NFT+I) = IXQ(NIXQ,NFT+I)
              IF( H3D_PART(IPARTQ(NFT+I)) == 1) IOK_PART(I) = 1
            ENDDO
C---------------------    
            DO I=1,NEL
              VALUE(I) = ZERO
              QUAD_SCALAR(NFT+I) = ZERO   ! Default = zero in all cases !
            ENDDO
c
            IUVAR = IUVAR_INPUT
C-----------------------------------------------
C Mass computation
C-----------------------------------------------
            IF (KEYWORD == 'MASS') THEN
    	      IALEL=(IPARG(7,NG)+IPARG(11,NG))
C
    	      DO I=1,NEL
    	        N = I + NFT
    	        IF(IALEL==0)THEN
    	          MT=IXQ(1,N)
    	          MASS(I)=PM(89,MT)*GBUF%VOL(I)
    	        ELSE
    	          OFF = MIN(GBUF%OFF(I),ONE)
    	          MASS(I)=GBUF%RHO(I)*GBUF%VOL(I)*OFF
    	        ENDIF
    	      ENDDO
            ENDIF
C--------------------------------------------------
            IF (KEYWORD == 'MASS') THEN   ! MASS
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = MASS(I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'EPSP')THEN  ! EPSP
C--------------------------------------------------
              IF (MLW == 10 .OR. MLW == 21) THEN
                DO I=1,NEL
                  VALUE(I) = LBUF%EPSQ(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF (MLW == 24) THEN   ! et autres a ajouter
                DO I=1,NEL
                  VALUE(I) = LBUF%VK(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF (MLW == 6 .OR. MLW == 17 .OR. MLW == 11) THEN   ! et autres a ajouter
                DO I=1,NEL
                  VALUE(I) =  LBUF%RK(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF (MLW >=28 .AND. MLW /= 49 .and. NUVAR > 0) THEN
                DO I=1,NEL
                  VALUE(I) =  UVAR(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSE
                IF (GBUF%G_PLA > 0) THEN
                  DO I=1,NEL
                    VALUE(I) =  GBUF%PLA(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TSAIWU' .AND. GBUF%G_TSAIWU > 0) THEN
C--------------------------------------------------  
              DO I=LFT,LLT   
                VALUE(I) = ZERO                                      
              ENDDO                          
              IF (ELBUF_TAB(NG)%BUFLY(1)%L_TSAIWU > 0) THEN
                DO IS=1,NPTS 
                  DO IT=1,NPTT                                      
                    DO IR=1,NPTR                                    
                      LBUF=>ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                      DO I=LFT,LLT                                 
                        VALUE(I) = VALUE(I) + LBUF%TSAIWU(I)/(NPTS*NPTT*NPTR) 
                        IS_WRITTEN_VALUE(I) = 1         
                      ENDDO
                    ENDDO    
                  ENDDO      
                ENDDO        
              ENDIF 
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
                     VALUE(I) =  GBUF%RHO(I)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'TEMP')THEN
C--------------------------------------------------
              IF(GBUF%G_TEMP > 0)THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%TEMP(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
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
                     P = - (GBUF%SIG(JJ(1) + I)
     .                    + GBUF%SIG(JJ(2) + I)
     .                    + GBUF%SIG(JJ(3) + I))*THIRD
                     VALUE(I) = P
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VONM')THEN
C--------------------------------------------------
               DO I=1,NEL
                 P = - (GBUF%SIG(JJ(1) + I)
     .               + GBUF%SIG(JJ(2) + I)
     .               + GBUF%SIG(JJ(3) + I) )*THIRD
                 S1 = GBUF%SIG(JJ(1) + I) + P
                 S2 = GBUF%SIG(JJ(2) + I) + P
                 S3 = GBUF%SIG(JJ(3) + I) + P
                 VONM2 = THREE*(GBUF%SIG(JJ(4) + I)**2
     .                + HALF*(S1**2+S2**2+S3**2) )
                 VALUE(I) = SQRT(VONM2)
                 IS_WRITTEN_VALUE(I) = 1
               ENDDO
C--------------------------------------------------
            ELSEIF(KEYWORD == 'K')THEN
C--------------------------------------------------
              IF(JTURB/=0)THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%RK(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'TVIS')THEN
C--------------------------------------------------
              IF (MLW == 6 .OR. MLW == 17.AND.JTURB/=0) THEN
                DO I=1,NEL
                  MT=IXQ(1,I+NFT)
                  VALUE(I)=PM(81,MT)*GBUF%RK(I)**2/
     .                 MAX(EM15,GBUF%RE(I))
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                DO I=1,NEL
                  VALUE(I)= UVAR(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VORTX')THEN
C--------------------------------------------------
              IF (MLW == 6 .OR. MLW == 17) THEN
                DO I=1,NEL
                  VALUE(I) = LBUF%VK(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                DO I=1,NEL
                  VALUE(I) = UVAR(NEL+I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF(MLW == 151)THEN
                !ITY = IPARG(5, NG)
                NB_FACE = 4
                IF (ITY == 7)NB_FACE=3
                DO I=1,NEL
                  II = I + NFT
                  IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
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
            ELSEIF(KEYWORD == 'DAM1')THEN
C--------------------------------------------------
              IF(MLW == 24)THEN
                DO I=1,NEL
                  VALUE(I) = LBUF%DAM(JJ(1) + I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'DAM2')THEN
C--------------------------------------------------
              IF(MLW == 24)THEN
                DO I=1,NEL
                  VALUE(I) = LBUF%DAM(JJ(2) + I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'DAM3')THEN
C--------------------------------------------------
              IF(MLW == 24)THEN
                DO I=1,NEL
                  VALUE(I) = LBUF%DAM(JJ(3) + I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'SIGX')THEN
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = GBUF%SIG(JJ(1) + I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
C--------------------------------------------------
            ELSEIF(KEYWORD == 'SIGY')THEN
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = GBUF%SIG(JJ(2) + I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
C--------------------------------------------------
            ELSEIF(KEYWORD == 'SIGZ')THEN
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = GBUF%SIG(JJ(3) + I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
C--------------------------------------------------
            ELSEIF(KEYWORD == 'SIGXY')THEN
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = GBUF%SIG(JJ(4) + I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
c----
C--------------------------------------------------
            ELSEIF(KEYWORD == 'USER')THEN
C--------------------------------------------------
              IF(IUVAR > 0 .AND. (MLW == 28.OR.MLW == 29.OR.MLW == 30.OR.
     .              MLW == 31.OR.MLW == 52.OR.MLW == 79))THEN
                DO I=1,NEL
                  N = I + NFT
                  MT = IXQ(1,N)
                  NUVAR = IPM(8,MT)
                  IF (IUVAR <= NUVAR) THEN
                    VALUE(I) = UVAR(IUVAR*NEL + I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDIF
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'HOURGLASS')THEN
C--------------------------------------------------
               DO I=1,NEL
                 N = I + NFT
                 VALUE(I) = EHOUR(N)
                 IS_WRITTEN_VALUE(I) = 1
               ENDDO
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VFRAC1')THEN
C--------------------------------------------------
              IF(MLW == 20)THEN 
                DO  I=1,NEL
                  VALUE(I) = 
     .               ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%VOL(I)
     .               / ELBUF_TAB(NG)%GBUF%VOL(I)
                  IS_WRITTEN_VALUE(I) = 1             
                ENDDO 
              ELSEIF(MLW == 37)THEN 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                DO  I=1,NEL
                  VALUE(I) = MBUF%VAR(I+3*NEL)
                  IS_WRITTEN_VALUE(I) = 1         
                ENDDO 
              ELSEIF(MLW == 51)THEN 
                IUS=N0PHAS 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                DO  I=1,NEL
                  VALUE(I) = MBUF%VAR(I+IUS*NEL)
                  IS_WRITTEN_VALUE(I) = 1      
                ENDDO 
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VFRAC2')THEN
C--------------------------------------------------
              IF(MLW == 20)THEN 
                DO  I=1,NEL
                  VALUE(I) = 
     .               ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)%VOL(I)
     .               / ELBUF_TAB(NG)%GBUF%VOL(I)
                  IS_WRITTEN_VALUE(I) = 1             
                ENDDO 
              ELSEIF(MLW == 37)THEN 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                DO  I=1,NEL
                  VALUE(I) = MBUF%VAR(I+4*NEL)
                  IS_WRITTEN_VALUE(I) = 1         
                ENDDO 
              ELSEIF(MLW == 51)THEN  
                IUS=N0PHAS+NVPHAS 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                DO  I=1,NEL
                  VALUE(I) = MBUF%VAR(I+IUS*NEL)
                  IS_WRITTEN_VALUE(I) = 1      
                ENDDO      
              ENDIF  
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VFRAC3')THEN
C--------------------------------------------------
              IF(MLW == 51)THEN  
                IUS=N0PHAS+2*NVPHAS 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                DO  I=1,NEL
                  VALUE(I) = MBUF%VAR(I+IUS*NEL)
                  IS_WRITTEN_VALUE(I) = 1      
                ENDDO      
              ENDIF  
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VFRAC4')THEN
C--------------------------------------------------
              IF(MLW == 51)THEN  
                IUS=N0PHAS+3*NVPHAS 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)     
                DO  I=1,NEL
                  VALUE(I) = MBUF%VAR(I+IUS*NEL)
                  IS_WRITTEN_VALUE(I) = 1      
                ENDDO      
              ENDIF  
C--------------------------------------------------
           ELSEIF(KEYWORD(1:9) == 'M151VFRAC') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A9,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    GBUF => ELBUF_TAB(NG)%GBUF
                    LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)
                    DO I=1,NEL
                       VALUE(I) = LBUF%VOL(I) / GBUF%VOL(I)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF    
C--------------------------------------------------
           ELSEIF(KEYWORD(1:8) == 'M151ENER') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    DO I=1,NEL
                       VALUE(I) = MULTI_FVM%PHASE_EINT(IMAT, I + NFT) / 
     .                      MULTI_FVM%PHASE_RHO(IMAT, I + NFT)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF    
C--------------------------------------------------
           ELSEIF(KEYWORD(1:8) == 'M151PRES') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
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
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    DO I=1,NEL
                       VALUE(I) = MULTI_FVM%PHASE_RHO(IMAT, I + NFT)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF  
C--------------------------------------------------      
            ELSEIF(KEYWORD == 'BFRAC')THEN
C--------------------------------------------------
              !BURN FRACTION explosive EOS 
                IF(GBUF%G_BFRAC > 0) THEN
                  IF (MLW==151)THEN
                    DO I=1,NEL
                      VALUE(I)=-EP30
                      DO ILAY=1,NLAY
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
            ELSEIF(KEYWORD == 'SSP')THEN 
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL                                         
                     VALUE(I) = MULTI_FVM%SOUND_SPEED(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1		                 
                  ENDDO
               ELSE
                  L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
                  IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN              
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                     DO  I=1,NEL                                         
                        VALUE(I) = LBUF%SSP(I) 
                        IS_WRITTEN_VALUE(I) = 1		                 
                     ENDDO                                                 
                  ENDIF       
               ENDIF
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'DOMAIN') THEN
C--------------------------------------------------   
                  DO I=1,NEL
                    VALUE(I) = ISPMD
                    IS_WRITTEN_VALUE(I) = 1	
                  ENDDO
C--------------------------------------------------
            ELSEIF(KEYWORD == 'SCHLIEREN') THEN     
C--------------------------------------------------                                                   
               IALEL=IPARG(7,NG)+IPARG(11,NG)
               IF(IALEL /= 0)THEN                                         
                 CALL SCHLIEREN(
     1                 EVAR  , IXQ  , X         , V           , W       ,
     2                 IPARG , WA_L , ELBUF_TAB , ALE_CONNECT , GBUF%VOL,
     3                 PM    , NG   , NIXQ      , ITY) 
                 DO  I=1,NEL 
                   VALUE(I) = EVAR(I) 
		   IS_WRITTEN_VALUE(I) = 1
                 ENDDO
               ENDIF   
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGEQ') THEN
C--------------------------------------------------
            !  equivalent stress - other then VON MISES
              IF (GBUF%G_SEQ > 0) THEN
                  IMAT = IXQ(1,NFT+1)
                  IADBUF = IPM(7,IMAT)
                  NUPARAM= IPM(9,IMAT)       
                  UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
!---
                  IF (MLW == 72) THEN
!                 (ILAW = 74) -- Hill MMC (anisotropic)
                    FF0 = UPARAM(11)
                    GG0 = UPARAM(12)
                    HH0 = UPARAM(13)
                    LL0 = UPARAM(14)
                    MM0 = UPARAM(15)
                    NN0 = UPARAM(16)
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
!
                      CRIT = FF0*(S2 - S3)**2
     .                     + GG0*(S3 - S1)**2
     .                     + HH0*(S1 - S2)**2
     .                     + TWO*LL0*S5**2
     .                     + TWO*MM0*S6**2
     .                     + TWO*NN0*S4**2
!
                      VALUE(I) = SQRT(CRIT)
		      IS_WRITTEN_VALUE(I) = 1
                    ENDDO ! DO I=1,NEL
                  ELSEIF (MLW == 74) THEN
!                 (ILAW = 74) -- Thermal Hill Orthotropic 3D Material
                    FF0 = UPARAM(7)
                    GG0 = UPARAM(8)
                    HH0 = UPARAM(9)
                    LL0 = UPARAM(10)
                    MM0 = UPARAM(11)
                    NN0 = UPARAM(12)
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
!
                      CRIT = FF0*(S2 - S3)**2
     .                     + GG0*(S3 - S1)**2
     .                     + HH0*(S1 - S2)**2
     .                     + TWO*LL0*S5**2
     .                     + TWO*MM0*S6**2
     .                     + TWO*NN0*S4**2
!
                      VALUE(I) = SQRT(CRIT)
		      IS_WRITTEN_VALUE(I) = 1
                    ENDDO ! DO I=1,NEL
                  ENDIF ! IF (MLW == 72)
!---
              ELSE  ! VON MISES
                DO I=1,NEL
                  P = - (GBUF%SIG(JJ(1) + I)
     .                +  GBUF%SIG(JJ(2) + I)
     .                +  GBUF%SIG(JJ(3) + I))*THIRD
                  S1 = GBUF%SIG(JJ(1) + I) + P
                  S2 = GBUF%SIG(JJ(2) + I) + P
                  S3 = GBUF%SIG(JJ(3) + I) + P
                  VONM2 = THREE*(GBUF%SIG(JJ(4) + I)**2
     .                  + HALF*(S1**2+S2**2+S3**2))
                  VALUE(I) = SQRT(VONM2)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF ! IF (GBUF%G_SEQ > 0)
C--------------------------------------------------
            ELSEIF(KEYWORD == 'BULK')THEN 
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
                   K         = IPARG(2,NG) * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                             
                   MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
                   DO I=1,IPARG(2,NG)
                     VALUE(I) = -MBUF%VAR(K+I)
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO 
                 ENDIF   
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/DENS1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = LBUF%RHO(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/DENS2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = LBUF%RHO(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/ENER1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = LBUF%EINT(I)/MAX(EM30,LBUF%RHO(I))
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/ENER2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = LBUF%EINT(I)/MAX(EM30,LBUF%RHO(I))
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/TEMP1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = LBUF%TEMP(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/TEMP2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = LBUF%TEMP(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/P1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = - (LBUF%SIG(JJ(1) + I) + 
     .                          LBUF%SIG(JJ(2) + I) + 
     .                          LBUF%SIG(JJ(3) + I))*THIRD
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/P2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = - (LBUF%SIG(JJ(1) + I) + 
     .                          LBUF%SIG(JJ(2) + I) + 
     .                          LBUF%SIG(JJ(3) + I))*THIRD
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/EPSP1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = LBUF%PLA(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/EPSP1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = LBUF%PLA(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/SSP1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = LBUF%SSP(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/SSP2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = LBUF%SSP(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/VOLUM1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              LBUF1  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)              
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                IF(IALEL ==0)THEN
                  MT = IXQ(1,NFT+1)
                  DO I=1,NEL
                    VALUE(I) = PM(1,MT)*LBUF1%VOL(I)*TWO*PI_
                    IF(LBUF1%RHO(I)>ZERO)VALUE(I) = VALUE(I)/LBUF1%RHO(I)
	            IS_WRITTEN_VALUE(I) = 1
                  ENDDO                
                ELSE
                  DO I=1,NEL
                    VALUE(I) = LBUF1%VOL(I)*TWO*PI_
	            IS_WRITTEN_VALUE(I) = 1
                  ENDDO  
                ENDIF     
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/VOLUM2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(NLAY > 1)THEN !otherwise memory is not allocated
                LBUF2  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)              
                IF(IALEL /= 0 .AND. MLW == 20)THEN
                  IF(IALEL ==0)THEN
                    MT = IXQ(1,NFT+1)
                    DO I=1,NEL
                      VALUE(I) = PM(1,MT)*LBUF2%VOL(I)*TWO*PI_
                      IF(LBUF2%RHO(I)>ZERO)VALUE(I) = VALUE(I)/LBUF2%RHO(I)
	              IS_WRITTEN_VALUE(I) = 1
                    ENDDO                
                  ELSE
                    DO I=1,NEL
                      VALUE(I) = LBUF2%VOL(I)*TWO*PI_
	              IS_WRITTEN_VALUE(I) = 1
                    ENDDO  
                  ENDIF     
                ENDIF 
               ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'VOLU' .OR. KEYWORD == 'VOL')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL ==0)THEN                                           
                MT = IXQ(1,NFT+1)                                         
                DO I=1,NEL                                                
                  VALUE(I) = PM(1,MT)*GBUF%VOL(I)*TWO*PI_                
                  IF(GBUF%RHO(I)>ZERO)VALUE(I) = VALUE(I)/GBUF%RHO(I)  
	          IS_WRITTEN_VALUE(I) = 1                                 
                ENDDO                                                     
              ELSE                                                        
                DO I=1,NEL                                                
                  VALUE(I) = GBUF%VOL(I)*TWO*PI_                         
	          IS_WRITTEN_VALUE(I) = 1                                 
                ENDDO                                                     
              ENDIF                                                       
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/QVIS1')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  VALUE(I) = LBUF%QVIS(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
C--------------------------------------------------
            ELSEIF(KEYWORD == 'LAW20/QVIS2')THEN
C--------------------------------------------------
              IALEL = IPARG(7,NG)+IPARG(11,NG)
              IF(IALEL /= 0 .AND. MLW == 20)THEN
                DO I=1,NEL
                  LBUF  => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)
                  VALUE(I) = LBUF%QVIS(I)
		  IS_WRITTEN_VALUE(I) = 1
                ENDDO  	
              ENDIF 
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
                        MT=IXQ(1,N)
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
                        MT=IXQ(1,N)
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
                        MT=IXQ(1,N)
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
                           MT=IXQ(1,N)
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
                           MT=IXQ(1,N)
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
                           MT=IXQ(1,N)
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
              ELSEIF(KEYWORD == 'MACH')THEN 
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
                      DO  I=1,NEL                                                                             
                        TMP(1,1:4)=V(1,IXQ(2:5,I+NFT))-W(1,IXQ(2:5,I+NFT))                                                     
                        TMP(2,1:4)=V(2,IXQ(2:5,I+NFT))-W(2,IXQ(2:5,I+NFT))                                                     
                        TMP(3,1:4)=V(3,IXQ(2:5,I+NFT))-W(3,IXQ(2:5,I+NFT))                                                     
                        VEL(1) = SUM(TMP(1,1:4))*FOURTH                                                       
                        VEL(2) = SUM(TMP(2,1:4))*FOURTH                                                       
                        VEL(3) = SUM(TMP(3,1:4))*FOURTH                                                       
                        VALUE(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)  
                        IS_WRITTEN_VALUE(I) = 1                                   
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
                        VALUE(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I) 
                        IS_WRITTEN_VALUE(I) = 1                                    
                      ENDDO
                    ENDIF                                                                                     
                  ENDIF  
                ENDIF                       
C--------------------------------------------------
              ELSEIF(KEYWORD == 'COLOR')THEN 
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
                IMAT   = IXQ(1,NFT+1)                        
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
                !VFRAC(1:NEL,1:21)=ZERO     
              ENDIF 
              
                 ! print *, "MWL,NFRAC,NEL=", MLW,NFRAC,NEL
              
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
              ELSEIF(KEYWORD == 'TILLOTSON') THEN  
C-------------------------------------------------- 
                 MT = IXQ(1,NFT+1)                                         
                 IF (MLW == 151) THEN
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
                           VALUE(I) = VALUE(I) + EBUF%VAR(I) * FAC
                           IS_WRITTEN_VALUE(I) = 1  
                         ENDDO
                       ENDIF  
                       FAC=FAC*TEN                 
                     ENDDO
                   !single Tillotson EoS   Value=  Region_i
                   ELSEIF(NTILLOTSON == 1)THEN
                         EBUF => ELBUF_TAB(NG)%BUFLY(IMAT_TILLOTSON)%EOS(1,1,1)
                         NVAREOS = ELBUF_TAB(NG)%BUFLY(IMAT_TILLOTSON)%NVAR_EOS
                         DO I=1,NEL
                           VALUE(I) = EBUF%VAR(I)
                           IS_WRITTEN_VALUE(I) = 1  
                         ENDDO
                   ENDIF                  
                 ELSE
                   !monomaterial law  
                   IEOS = IPM(4,MT)
                   IF(IEOS == 3)THEN
                     EBUF => ELBUF_TAB(NG)%BUFLY(1)%EOS(1,1,1) 
                     NVAREOS = ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS
                     DO  I=1,NEL                         
                       VALUE(I) = EBUF%VAR(I)
                       IS_WRITTEN_VALUE(I) = 1  
                     ENDDO  
                   ENDIF                      
                 ENDIF                               
C---------------------------------------------------------------------------
c            ELSEIF (KEYWORD == 'NEWKEY') THEN ! New Output Example
C---------------------------------------------------------------------------
c ILAYER=NULL NPT=NULL
c              IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
c                  DO I=1,NEL 
c                    VALUE(I) =
c                  ENDDO  
c PLY=IPLY NPT=IPT
c              ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN      
c                IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
c
c                ENDIF
c
c PLY=NULL ILAYER=ILAY NPT=IPT 
c              ELSEIF (IPLY == -1 .AND. ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN       
c                IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
c
c                ENDIF
c PLY=NULL ILAYER=IL NPT=NULL
c              ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
c                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
c
c                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
c
c                ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
c              ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
c                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
c
c                ENDIF
c              ENDIF
C---------------------------------------------------------------------------
c 
C-------------------------------------------------- 
            ENDIF  ! KEYWORD
C-------------------------------------------------- 
            CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_QUAD,QUAD_SCALAR,NEL,0,NFT,
     .   			       VALUE,IS_WRITTEN_VALUE)
          ENDIF  ! ITY 
c
C-----------------------------------------------
       ENDIF     ! MLW /= 13
 900  CONTINUE   ! NG 
C-----------------------------------------------
      RETURN
      END
