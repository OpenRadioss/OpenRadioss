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
Chd|  H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTORS             source/output/h3d/h3d_results/h3d_write_vectors.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        QROTA_VECT                    source/output/anim/generate/qrota_vect.F
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_QUAD_VECTOR(
     .                  ELBUF_TAB   ,QUAD_VECTOR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM         ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_QUAD,IPARTQ,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD   ,
     .                  BUFMAT      ,MULTI_FVM ,IR_INPUT  ,IS_INPUT    ,IT_INPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD 
      USE MULTI_FVM_MOD              
      USE ALEFVM_MOD
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
      my_real
     .   QUAD_VECTOR(3,*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,*),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*),  NV46,ID_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_QUAD(*),IPARTQ(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,
     .   IR_INPUT,IS_INPUT,IT_INPUT,IS_WRITTEN_VECTOR(MVSIZ)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   VALUE(3),FF0,GG0,HH0,LL0,MM0,NN0,MASS(MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),BUFMAT(*),
     .   S11,S22,S33,S4,S5,S6,CRIT,VALUE1,VALUE2
      my_real
     .   EVAR(3,MVSIZ)
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),ID_PLY,NB_PLYOFF,IOK,IADBUF,NUPARAM,
     .        IMAT,IVISC,IPOS,ITRIMAT
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,IOK_PART(MVSIZ),JJ(6),NPGT,IUVAR,
     .        IS_WRITTEN_VALUE(MVSIZ)
      CHARACTER*5 BUFF
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
      TYPE(BUF_MAT_)  ,POINTER :: MBUF 
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      TARGET :: BUFMAT
C-----------------------------------------------
      ILAY = LAYER_INPUT
      IUVAR = IUVAR_INPUT
      IR = IR_INPUT
      IS = IS_INPUT
      IT = IT_INPUT
C
      DO I=1,NUMELQ
         IS_WRITTEN_QUAD(I) = 0
      ENDDO
C-----------
      DO 900 NG=1,NGROUP
       CALL INITBUF(IPARG    ,NG      ,                    
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
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          VALUE(1:3) = ZERO
          DO I=1,NEL
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	     
          EVAR(1:3,1:NEL) = ZERO
          IS_WRITTEN_VECTOR(1:NEL) = 0
C-----------------------------------------------
C           QUAD
C-----------------------------------------------
         IF (ITY == 2) THEN

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
c
            DO  I=1,NEL 
              ID_ELEM(NFT+I) = IXQ(NIXQ,NFT+I)
              IF( H3D_PART(IPARTQ(NFT+I)) == 1) IOK_PART(I) = 1
            ENDDO
C---------------------    
            DO I=1,NEL
              QUAD_VECTOR(1:3,NFT+I) = ZERO   ! Default = zero in all cases !
            ENDDO
c
            IUVAR = IUVAR_INPUT
C--------------------------------------------------
            IF (KEYWORD == 'VECT/VEL') THEN 
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     EVAR(1,I) = MULTI_FVM%VEL(1, I + NFT)
                     EVAR(2,I) = MULTI_FVM%VEL(2, I + NFT)
                     EVAR(3,I) = MULTI_FVM%VEL(3, I + NFT)
                     IS_WRITTEN_VECTOR(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     IF(GBUF%G_MOM>0 .AND. ALEFVM_Param%IEnabled > 0)THEN
                        EVAR(1,I) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                        EVAR(2,I) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                        EVAR(3,I) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                        IS_WRITTEN_VECTOR(I) = 1
                     ENDIF
                  ENDDO
               ENDIF
               IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_VECT(X,IXQ(1,NFT+1),JCVT,EVAR,GBUF%GAMA,NEL)
               ENDIF
c 
            ENDIF  ! KEYWORD
          ENDIF  ! ITY 
C-----------------------------------------------
          CALL H3D_WRITE_VECTORS(IOK_PART,IS_WRITTEN_QUAD,QUAD_VECTOR,NEL,0,NFT,
     .                                    EVAR,IS_WRITTEN_VECTOR)
c
C-----------------------------------------------
       ENDIF     ! MLW /= 13
 900  CONTINUE   ! NG 
C-----------------------------------------------
      RETURN
      END
