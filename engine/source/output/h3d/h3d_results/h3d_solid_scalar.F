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
Chd|  H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SOLID_SCALAR(
     .                  ELBUF_TAB       ,SOLID_SCALAR ,IFUNC     ,IPARG       ,GEO         ,
     .                  IXQ             ,IXS          ,IXTG      ,PM          ,BUFMAT      ,
     .                  EL2FA           ,NBF          ,IXUR      ,IADP        ,
     .                  NBF_L           ,EHOUR        ,ANIM      ,NBPART      ,IADG        ,
     .                  IPM             ,IGEO         ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3 ,
     .                  INVERT          ,X            ,V         ,W           ,ALE_CONNECT,
     .                  NV46            ,NERCVOIS     ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK           ,ID_ELEM      ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                  IR_INPUT        ,IS_INPUT     ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SOLID,INFO1        ,KEYWORD   ,FANI_CELL   ,SFANI_CELL  ,
     .                  MULTI_FVM       , H3D_DATA    ,IDMDS     ,IMDSVAR     ,MDS_MATID )
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
     .   IUVAR_INPUT,SFANI_CELL,IDMDS,IMDSVAR,MDS_MATID(*)
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
     .   USER(NUMELS),VALUE(MVSIZ),MASS(MVSIZ)
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
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),NB_PLYOFF,IUVAR,IDX,IPOS,ITRIMAT,
     .        IALEFVM_FLG, IMAT,IADBUF,NUPARAM
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,
     .        IOK_PART(MVSIZ),JJ(6),IRUPT,IOK,NPG_PLANE,NUMLAY,IJK,IIR,
     .        IS_WRITTEN_VALUE(MVSIZ),NFRAC,IU(4),IV,NB_FACE,KFACE
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
      DO I=1,NUMELS
         IS_WRITTEN_SOLID(I) = 0
      ENDDO
C
      !-------------------------------------------------------!
      !     INITIALIZATION IF SCHLIEREN DEFINED               !
      !-------------------------------------------------------!      
      IF(KEYWORD == 'SCHLIEREN')THEN
        CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM)
      ENDIF
c
      DO 900 NG=1,NGROUP

        call H3D_SOLID_SCALAR_1(
     .                  ELBUF_TAB       ,SOLID_SCALAR ,IFUNC     ,IPARG       ,GEO         ,
     .                  IXQ             ,IXS          ,IXTG      ,PM          ,BUFMAT      ,
     .                  EL2FA           ,NBF          ,IXUR      ,IADP        ,
     .                  NBF_L           ,EHOUR        ,ANIM      ,NBPART      ,IADG        ,
     .                  IPM             ,IGEO         ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3 ,
     .                  INVERT          ,X            ,V         ,W           ,ALE_CONNECT,
     .                  NV46            ,NERCVOIS     ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK           ,ID_ELEM      ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                  IR_INPUT        ,IS_INPUT     ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SOLID,INFO1        ,KEYWORD   ,FANI_CELL   ,SFANI_CELL  ,
     .                  MULTI_FVM       , H3D_DATA    ,NG        ,IDMDS       ,IMDSVAR     ,
     .                  MDS_MATID       )

 900  CONTINUE   ! NG 
C-----------------------------------------------
      RETURN
      END
