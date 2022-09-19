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
Chd|  H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SOLID_VECTOR(
     .                  ELBUF_TAB ,SOLID_VECTOR      ,IFUNC     ,IPARG       ,GEO        ,
     .                  IXQ       ,IXS       ,IXTG      ,PM         ,
     .                  EL2FA     ,NBF       ,IXUR      ,IADP        ,
     .                  NBF_L     ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM       ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT    ,X         ,V         ,W           ,
     .                  NV46      ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK     ,ID_ELEM   ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                  IR_INPUT  ,IS_INPUT  ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SOLID,INFO1,KEYWORD  ,FANI_CELL   ,SFANI_CELL  ,
     .                  H3D_DATA, MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE SCHLIEREN_MOD
      USE STACK_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SOLID_VECTOR(3,*),X(3,*),V(3,*),W(3,*),THKE(*),EHOUR(*),GEO(NPROPG,*),
     .   ANIM(*),PM(NPROPM,*),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IXTG(NIXTG,*),EL2FA(*),
     .   IXQ(NIXQ,*),IXUR(NIXUR,*),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,*),
     .   IGEO(NPROPGI,*),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),IPARTS(*),
     .   H3D_PART(*),IS_WRITTEN_SOLID(*),INFO1,LAYER_INPUT,IR_INPUT,IS_INPUT,IT_INPUT,
     .   IUVAR_INPUT,SFANI_CELL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      my_real, INTENT(IN) :: FANI_CELL(SFANI_CELL)
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   EVAR(MVSIZ),DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   USER(NUMELS),VALUE(3),MASS(MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),S11,S22,S33,S4,S5,S6,VONM,GAMA(6),
     .   T11,T21,T31,T12,T22,T32,T13,T23,T33,
     .   PHI,TETA,PSI,DAMMAX,EVAR_TMP
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,SWA_L,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),NB_PLYOFF,IUVAR,IDX,ILEN,IPOS,ITRIMAT,
     .        IALEFVM_FLG
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),LENCOM,IOFF,NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,
     .        IOK_PART(MVSIZ),JJ(6),IRUPT,IOK,NPG_PLANE,NUMLAY,IJK,IIR,
     .        IS_WRITTEN_VALUE(MVSIZ)
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      TYPE(BUF_MAT_)  ,POINTER :: MBUF
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY
      TYPE(BUF_FAIL_) ,POINTER :: FBUF
      my_real,
     .  DIMENSION(:), POINTER  :: UVARF,DAMF,DFMAX,TDELE
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
C-----------------------------------------------
      DO I=1,NUMELS
         IS_WRITTEN_SOLID(I) = 0
      ENDDO
C
c
      DO 900 NG=1,NGROUP

        CALL INITBUF (IPARG    ,NG      ,
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
c
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO
c
          VALUE(1:3) = ZERO
          DO I=1,NEL
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
            IR = IR_INPUT
            IS = IS_INPUT
            IT = IT_INPUT
            IF (ILAY == -2) ILAY = 1
            IF (ILAY == -3) ILAY = NLAY
C--------------------------------------------------
            IF (KEYWORD == 'VECT/VEL') THEN
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(1) = MULTI_FVM%VEL(1, I + NFT)
                     VALUE(2) = MULTI_FVM%VEL(2, I + NFT)
                     VALUE(3) = MULTI_FVM%VEL(3, I + NFT)
                     CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_VECTOR,I,OFFSET,NFT,VALUE)
                  ENDDO
               ELSE
                  DO I=1,NEL
                     IF(GBUF%G_MOM>0 )THEN
                        VALUE(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I)
                        VALUE(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                        VALUE(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                        CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_VECTOR,I,OFFSET,NFT,VALUE)
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
C--------------------------------------------------
            IF (KEYWORD == 'VECT/CONT') THEN
C--------------------------------------------------
               IF (MLW == 151) THEN
                  IDX=NUMELS*(ANIM_SE(10)+ANIM_SE(4960)+ANIM_SE(4961)) !skip vorticity data
                  ILEN=NUMELS+NUMELQ
                  DO I = 1, NEL
                     IPOS = IDX+3*(NFT+I-1)
                     VALUE(1) = FANI_CELL(IPOS+1)
                     VALUE(2) = FANI_CELL(IPOS+2)
                     VALUE(3) = FANI_CELL(IPOS+3)
                     CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_VECTOR,I,OFFSET,NFT,VALUE)
                  ENDDO
               ENDIF
            ENDIF
C--------------------------------------------------
         ENDIF
       ENDIF

 900  CONTINUE   ! NG
C-----------------------------------------------
      RETURN
      END
