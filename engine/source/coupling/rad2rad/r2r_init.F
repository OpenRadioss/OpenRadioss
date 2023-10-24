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
Chd|  R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        CONNECTION_SOCK_C             source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_FBUF_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_IBUF_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_MASS_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_MASS_RBY_C                source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_MASS_RBY_SPMD             source/coupling/rad2rad/r2r_init.F
Chd|        GET_MASS_SPMD                 source/coupling/rad2rad/r2r_init.F
Chd|        GET_NAME_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        INIT_ACTIV_C                  source/coupling/rad2rad/rad2rad_c.c
Chd|        INIT_LINK_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        INIT_LINK_NL_C                source/coupling/rad2rad/rad2rad_c.c
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|        OPENFIFO_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        OPENSEM_C                     source/coupling/rad2rad/rad2rad_c.c
Chd|        OPENSHM_C                     source/coupling/rad2rad/rad2rad_c.c
Chd|        R2R_RBY                       source/coupling/rad2rad/r2r_init.F
Chd|        SEND_FBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_IBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_MASS_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_MASS_NL_C                source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_MASS_RBY_C               source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_MASS_RBY_SPMD            source/coupling/rad2rad/r2r_init.F
Chd|        SEND_MASS_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|        SEND_SOCK_INIT_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_EXCH_R2R                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_EXCH_R2R_RBY             source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_R2R_SYNC                 source/mpi/r2r/spmd_r2r.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE R2R_INIT(IEXLNK  ,ITAB,IGRNOD,X      ,
     2                    MS      ,IN  ,DD_R2R,WEIGHT ,IAD_ELEM,
     3                    FR_ELEM,ADDCNEL,CNEL,IXC,IPARG,ICODT,ICODR,
     4                    IBFV,DX,RBY,NPBY,XDP,STIFN,STIFR,DD_R2R_ELEM,
     5                    SDD_R2R_ELEM,WEIGHT_MD,ILENXV,NUMSPH_GLO_R2R,
     6                    FLG_SPHINOUT_R2R,IPARI,NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RAD2R_MOD
        USE GROUPDEF_MOD
        USE MESSAGE_MOD
        USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chara_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
#include      "scr17_c.inc"
#include      "rad2r_c.inc"
#include      "scr05_c.inc"
#include      "scr03_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IEXLNK(NR2R,NR2RLNK), ITAB(*),
     .          WEIGHT(*), DD_R2R(NSPMD+1,*), IAD_ELEM(2,*), FR_ELEM(*),
     .          IROOT(100), ADDCNEL(0:*),CNEL(0:*),IXC(NIXC,*),ICODR(*),
     .          IPARG(NPARG,*),ICODT(*),IBFV(*),NPBY(*),DD_R2R_ELEM(*),
     .          SDD_R2R_ELEM,WEIGHT_MD(*),ILENXV,NUMSPH_GLO_R2R,FLG_SPHINOUT_R2R,
     .          IPARI(NPARI,*)
C     REAL
        my_real X(3,*), DX(3,*),MS(*),IN(*),RBY(*),STIFN(*),STIFR(*)
     .
        DOUBLE PRECISION XDP(3,*)
!
        TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD
        TYPE(NLOCAL_STR_), TARGET, INTENT(IN)  :: NLOC_DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, IEX, IDP, IDG, NNG, OFC,NFTC,INFO,TYP,ITSK
        INTEGER OMP_GET_THREAD_NUM,NUM_SOCK,SIZE_TAG_RBY,LENR,SIZE
        INTEGER NN,N,SUM,PPID,IDEL_LOC,NSN_GLOB,COMPT
        INTEGER, DIMENSION(:), ALLOCATABLE :: NDOF_NL
        CHARACTER*35 ADDR
C
        INTEGER, DIMENSION(:), POINTER :: GRNOD
        INTEGER, POINTER, DIMENSION(:) :: IDXI,POSI
        my_real, POINTER, DIMENSION(:) :: MSNL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        INFO=NUMELS+NUMELQ+NUMELC
        NBK = 0
        SIZE_TAG_RBY = 0
        IF((NINTER>0).AND.(IDTMIN(10)/=3).AND.(IDTMIN(11)/=3).AND.(IDTMIN(11)/=8)) THEN
          ILENXV = ILENXV + 2
        ENDIF
C
        IF ((R2R_SIU==1).OR.(NSPMD==1)) THEN
C------SPH+Multidomains--------------------->
          IF (R2R_SIU==1) THEN
            NUMSPH_GLO_R2R = NUMSPH
            IF (NSPMD>1) CALL SPMD_ALLGLOB_ISUM9(NUMSPH_GLO_R2R,1)
            IF ((NSPHIO>0).AND.(NUMSPH_GLO_R2R>0)) FLG_SPHINOUT_R2R = 1
            IF (NSPMD>1) THEN
              CALL SPMD_ALLGLOB_ISUM9(FLG_SPHINOUT_R2R,1)
              FLG_SPHINOUT_R2R = MIN(1,FLG_SPHINOUT_R2R)
            ENDIF
          ENDIF
C------Elimination of TYPE2 interfaces without second. nodes---------------->
          IF (R2R_SIU==1) THEN
            DO I=1,NINTER
              NSN_GLOB = IPARI(5,I)
              IF (NSPMD>1) CALL SPMD_ALLGLOB_ISUM9(NSN_GLOB,1)
              IF ((NSN_GLOB==0).AND.(IPARI(7,I)==2)) IPARI(7,I) = 0
            END DO
          ENDIF
C------Cas SMP initialization---------------c
          ALLOCATE(TYPLNK(NR2RLNK),RBYLNK(NR2RLNK),KINLNK(NR2RLNK))
          ALLOCATE(ADD_RBY(NR2RLNK))
          ALLOCATE(SOCKET(NTHREAD))
          ALLOCATE(NLLNK(NR2RLNK))
          ALLOCATE(NBDOF_NL(NR2RLNK))
          NBDOF_NL(1:NR2RLNK) = 0
C
          DO I = 1, ROOTLEN
            IROOT(I) = ICHAR(ROOTNAM(I:I))
          END DO
C----- Connection of first socket-----------c
          IF (ISPMD==0) THEN
            SOCKET(1)=SOCK0
            CALL SEND_SOCK_INIT_C(IROOT,ROOTLEN,ISPMD,SOCKET(1),NTHREAD,NSPMD)
          ENDIF
C----- Synchronisation of the process - transfer of hostname
          IF (NSPMD>1) CALL SPMD_R2R_SYNC(ADDR)
C----- Connection of socket of threads--------c
          IF(ISPMD==0) THEN
            DO ITSK=2,NTHREAD
              CALL GET_NAME_C(ADDR)
              ADDR=trim(ADDR)
              CALL CONNECTION_SOCK_C(ITSK-1,SOCKET(ITSK),ADDR)
            END DO
          ELSE
            DO ITSK=1,NTHREAD
              NUM_SOCK = NTHREAD*ISPMD+ITSK
              CALL CONNECTION_SOCK_C(NUM_SOCK-1,SOCKET(ITSK),ADDR)
            END DO
          ENDIF
C----- Initialize Fifos
          CALL OPENFIFO_C(IROOT,ROOTLEN,R2R_FDW,R2R_FDR,SOCKET(1),ISPMD,NTHREAD,PPID)
C----- set signal catch
          CALL GET_IBUF_C(R2R_IPID,1)
C----- send link interface data
          CALL SEND_IBUF_C(NR2RLNK,1)
          CALL SEND_IBUF_C(IRODDL,1)
          CALL SEND_FBUF_C(TT,1)
          CALL SEND_FBUF_C(TSTOP,1)
          CALL SEND_IBUF_C(NCRST,1)
          CALL SEND_IBUF_C(IDEL7NG,1)
          CALL SEND_IBUF_C(FLG_SPHINOUT_R2R,1)
C----- get info for th
          IF (R2R_SIU==1) THEN
            IF (ISPMD==0) THEN
              DO J=1,10
                CALL GET_IBUF_C(SEEK0(J),1)
                CALL GET_IBUF_C(SEEKC(J),1)
              ENDDO
            ENDIF
            IF (NSPMD>1) THEN
              CALL SPMD_IBCAST(SEEK0,SEEK0,10,1,0,2)
              CALL SPMD_IBCAST(SEEKC,SEEKC,10,1,0,2)
            ENDIF
          ENDIF
C-----
          CALL SEND_IBUF_C(IRUN,1)
          OFC=NUMELS+NUMELQ
C
          DO IEX = 1, NR2RLNK
            IDG  = IEXLNK(1,IEX)
            IDP  = IEXLNK(2,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            NFTC = 0
!
            GRNOD => IGRNOD(IDG)%ENTITY
!
            IF (IDP>NBK) NBK = IDP
C------	determination of the type of the interface
            CALL SEND_IBUF_C(IDP,1)
            CALL GET_IBUF_C(TYPLNK(IEX),1)
            CALL GET_IBUF_C(MAIN_SIDE,1)
            CALL GET_IBUF_C(RBYLNK(IEX),1)
            CALL GET_IBUF_C(KINLNK(IEX),1)
            CALL GET_IBUF_C(NLLNK(IEX),1)
            IF (RBYLNK(IEX)==1) THEN
              ADD_RBY(IEX) = SIZE_TAG_RBY
              SIZE_TAG_RBY = SIZE_TAG_RBY + NNG
            ENDIF

C--------------Reset of weight2 for duplicated nodes--------
            IF ((TYPLNK(IEX)==5).AND.(MAIN_SIDE==1)) THEN
              DO NN=1,NNG
                N = IGRNOD(IDG)%ENTITY(NN)
                WEIGHT_MD(N) = 0
              END DO
            ENDIF
C--------------Initialisation of arrays for rlinks/cyljoints------
            IF ((TYPLNK(IEX)==5).AND.(KINLNK(IEX)==1)) THEN
              ALLOCATE(R2R_KINE(3,NNG))
              R2R_KINE(:,:)=0
            ENDIF
C----------------------------------------------------------------------
C------
            IF (NLLNK(IEX)==1) THEN
C-------- Coupling of non local dof
              IDXI => NLOC_DMG%IDXI(1:NUMNOD)
              POSI => NLOC_DMG%POSI(1:NLOC_DMG%NNOD+1)
              COMPT = 0
              ALLOCATE(NDOF_NL(NNG))
              DO I=1,NNG
                NN = IDXI(GRNOD(I))
                NDOF_NL(I) = POSI(NN+1)-POSI(NN)
                COMPT = COMPT + NDOF_NL(I)
              ENDDO
              NBDOF_NL(IEX) = COMPT
              ALLOCATE(IADD_NL(COMPT))
              COMPT = 0
              DO I=1,NNG
                NN = IDXI(GRNOD(I))
                DO J=POSI(NN),POSI(NN+1)-1
                  COMPT = COMPT + 1
                  IADD_NL(COMPT) = J
                ENDDO
              ENDDO
              CALL INIT_LINK_NL_C(IDP,NNG,ITAB,GRNOD,X,NCPRI,DX,NDOF_NL,NBDOF_NL(IEX),NBK)
              DEALLOCATE(NDOF_NL)
C
              IF ((NSPMD > 1).AND.(SDD_R2R_ELEM>0)) THEN
                DD_R2R_NL(1:2) = 0
                DO I=1,NSPMD
                  DD_R2R_NL(1) = DD_R2R_NL(1) + DD_R2R(I+1,3)-DD_R2R(I,3)
                ENDDO
                DO I=1,NSPMD
                  DD_R2R_NL(2) = DD_R2R_NL(2) + DD_R2R(I+1,4)-DD_R2R(I,4)
                ENDDO
              ENDIF
C
            ELSE
              CALL INIT_LINK_C(IDP,NNG,ITAB,GRNOD,X,ADDCNEL,CNEL,IXC,
     .            OFC,INFO,TYPLNK(IEX),ICODT,ICODR,NCPRI,IRODDL,NBK,DX)
            ENDIF
C
          END DO
!
          CALL INIT_ACTIV_C(R2R_ACTIV)
C       CALL CHECK_RODDL_C()
C----- Initialize Shared Memory
          CALL OPENSHM_C()
C
          CALL GET_FBUF_C(TSTOP,1)
          CALL GET_IBUF_C(IDEL_LOC,1)
          IDEL7NG = MAX(IDEL7NG,IDEL_LOC)
          IF (IDEL7NG>=1) IDEL7NOK = 1
C----- Update mass and inertia
C
          R2RFX1 = ZERO
          R2RFX2 = ZERO
          ALLOCATE (TAG_RBY(SIZE_TAG_RBY))
          DO IEX = 1, NR2RLNK
            IDG  = IEXLNK(1,IEX)
            IDP  = IEXLNK(2,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            GRNOD => IGRNOD(IDG)%ENTITY
            IF (RBYLNK(IEX)==1) THEN
              CALL SEND_MASS_RBY_C(IDP,NNG,GRNOD,MS,IN,NPBY,
     .          NRBODY,RBY,TAG_RBY,ADD_RBY(IEX),NNPBY,NRBY)
            ELSEIF (NLLNK(IEX)==1) THEN
C---------- Coupling of non local dof
              MSNL  => NLOC_DMG%MASS(1:NLOC_DMG%L_NLOC)
              CALL SEND_MASS_NL_C(IDP,NBDOF_NL(IEX),IADD_NL,MSNL)
            ELSE
              CALL SEND_MASS_C(IDP,NNG,GRNOD,MS,IN)
            ENDIF
          END DO
C
          IF (TT==ZERO) THEN
            DO IEX = 1, NR2RLNK
              IDG  = IEXLNK(1,IEX)
              IDP  = IEXLNK(2,IEX)
              NNG  = IGRNOD(IDG)%NENTITY
              GRNOD => IGRNOD(IDG)%ENTITY
              IF (RBYLNK(IEX)==1) THEN
                CALL GET_MASS_RBY_C(IDP,NNG,GRNOD,MS,IN,X,NPBY,NRBODY,RBY,NNPBY,NRBY)
                CALL R2R_RBY(NNG,ITAB,GRNOD,X,MS,IN,NPBY,RBY,XDP,1,WEIGHT)
              ELSEIF (NLLNK(IEX)==1) THEN
C---------- Coupling of non local dof - mass not modifed -
                CALL SEND_IBUF_C(IDP,1)
              ELSE
                CALL GET_MASS_C(IDP,NNG,GRNOD,MS,IN)
              ENDIF
            END DO
C
C---------------Synchronisation (not needed for NL coupling)------C
            IF (NSPMD>1) THEN
              IF (SDD_R2R_ELEM>0) THEN
                SIZE =  3 + IRODDL*3
                LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
                CALL SPMD_EXCH_R2R(
     1          X       ,X     ,STIFN,STIFR ,MS   ,
     2          IAD_ELEM,FR_ELEM,SIZE ,
     3          LENR    ,DD_R2R,DD_R2R_ELEM,2)
C
                SIZE =  1 + IRODDL*1
                CALL SPMD_EXCH_R2R(
     1          X       ,X     ,MS,IN ,MS   ,
     2          IAD_ELEM,FR_ELEM,SIZE ,
     3          LENR    ,DD_R2R,DD_R2R_ELEM,1)
C
                SIZE =  28
                IF (IRESP==1) THEN
                  CALL SPMD_EXCH_R2R_RBY(
     1            NPBY, RBY,
     2            IAD_ELEM,FR_ELEM,SIZE ,
     3            LENR    ,DD_R2R,DD_R2R_ELEM,XDP)
                ELSE
                  CALL SPMD_EXCH_R2R_RBY(
     1            NPBY, RBY,
     2            IAD_ELEM,FR_ELEM,SIZE ,
     3            LENR    ,DD_R2R,DD_R2R_ELEM,X)
                ENDIF
C
              ENDIF
            ENDIF

          ENDIF

        ELSE

C------Allocation of common arrays
          ALLOCATE(TYPLNK(NR2RLNK),ROTLNK(NR2RLNK))
          ALLOCATE(DBN(NR2RLNK,NSPMD),NBEL(NR2RLNK,NSPMD))
          ALLOCATE(TBCNEL(NR2RLNK,NSPMD),TBCNELDB(NR2RLNK,NSPMD))
          ALLOCATE(NBELN(NR2RLNK,NSPMD))
          ALLOCATE(DBNO(NR2RLNK),TCNELT(NR2RLNK),NBELT_R2R(NR2RLNK))
          ALLOCATE(TCNELTDB(NR2RLNK),OFFSET(NSPMD),NBELTN_R2R(NR2RLNK))
          ALLOCATE(RBYLNK(NR2RLNK),ADD_RBY(NR2RLNK),KINLNK(NR2RLNK))
          ALLOCATE(NLLNK(NR2RLNK))
C---------------------------------------

          DO I = 1, ROOTLEN
            IROOT(I) = ICHAR(ROOTNAM(I:I))
          END DO

          IF(ISPMD==0) THEN
C----- Initialize Sockets
            ALLOCATE(SOCKET(NTHREAD))
            SOCKET(1)=SOCK0
            CALL SEND_SOCK_INIT_C(IROOT,ROOTLEN,ISPMD,SOCKET(1),
     1         NTHREAD,NSPMD)
          ENDIF
C----- Synchronisation of the process - transfer of hostname
          CALL SPMD_R2R_SYNC(ADDR)
C----- ----- ----- ----- ----- -----
          IF(ISPMD==0) THEN
            DO ITSK=2,NTHREAD
              CALL CONNECTION_SOCK_C(ITSK-1,SOCKET(ITSK),ADDR)
            END DO
C----- Initialize Fifos
            CALL OPENFIFO_C(IROOT,ROOTLEN,R2R_FDW,R2R_FDR,SOCKET(1),ISPMD,NTHREAD,PPID)
            CALL OPENSEM_C(IROOT,ROOTLEN,ISPMD,NTHREAD,PPID)
C----- set signal catch
            CALL GET_IBUF_C(R2R_IPID,1)
C----- send link interface data
            CALL SEND_IBUF_C(NR2RLNK,1)
            CALL SEND_IBUF_C(IRODDL,1)
            CALL SEND_FBUF_C(TT,1)
            CALL SEND_FBUF_C(TSTOP,1)
            CALL SEND_IBUF_C(NCRST,1)
            CALL SEND_IBUF_C(IDEL7NG,1)
            CALL SEND_IBUF_C(FLG_SPHINOUT_R2R,1)
            CALL SEND_IBUF_C(IRUN,1)
            CALL SPMD_IBCAST(PPID,PPID,1,1,0,2)
          ELSE
C----- Connect Sockets
            ALLOCATE(SOCKET(NTHREAD))
            DO ITSK=1,NTHREAD
              NUM_SOCK = NTHREAD*ISPMD+ITSK
              CALL CONNECTION_SOCK_C(NUM_SOCK-1,SOCKET(ITSK),ADDR)
            END DO
            CALL SPMD_IBCAST(PPID,PPID,1,1,0,2)
            CALL OPENSEM_C(IROOT,ROOTLEN,ISPMD,NTHREAD,PPID)
          ENDIF

C----- ----- ----- ----- ----- -----
          DO IEX = 1, NR2RLNK
            IDG  = IEXLNK(1,IEX)
            IDP  = IEXLNK(2,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            GRNOD => IGRNOD(IDG)%ENTITY
C------	determination of the type of interface and link
            IF (IDP>NBK) NBK = IDP
            IF(ISPMD==0) THEN
              CALL SEND_IBUF_C(IDP,1)
              CALL GET_IBUF_C(TYPLNK(IEX),1)
              CALL GET_IBUF_C(MAIN_SIDE,1)
              CALL GET_IBUF_C(RBYLNK(IEX),1)
              CALL GET_IBUF_C(KINLNK(IEX),1)
              CALL GET_IBUF_C(NLLNK(IEX),1)
            ENDIF
c------
            IF(NSPMD>1) THEN
              CALL SPMD_IBCAST(TYPLNK(IEX),TYPLNK(IEX),1,1,0,2)
              CALL SPMD_IBCAST(MAIN_SIDE,MAIN_SIDE,1,1,0,2)
              CALL SPMD_IBCAST(RBYLNK(IEX),RBYLNK(IEX),1,1,0,2)
              CALL SPMD_IBCAST(KINLNK(IEX),KINLNK(IEX),1,1,0,2)
            ENDIF
C--------------Reset of weight2 for duplicated nodes--------
            IF ((TYPLNK(IEX)==5).AND.(MAIN_SIDE==1)) THEN
              DO NN=1,NNG
                N = IGRNOD(IDG)%ENTITY(NN)
                WEIGHT_MD(N) = 0
              END DO
            ENDIF
C
            IF (RBYLNK(IEX)==1) THEN
              ADD_RBY(IEX) = SIZE_TAG_RBY
              SIZE_TAG_RBY = SIZE_TAG_RBY + NNG
            ENDIF
c------
            CALL INIT_LINK_SPMD(
     1        IDP          ,NNG                  ,ITAB  ,GRNOD,X,
     2        DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,ADDCNEL,CNEL,IXC,
     3        OFC,IEX,INFO,TYPLNK(IEX),ICODT,ICODR,IBFV,DX)
          END DO

          IF(ISPMD==0) THEN
!
            CALL INIT_ACTIV_C(R2R_ACTIV)
C----- Initialize Shared Memory
            CALL OPENSHM_C()
C----- CALL CHECK_RODDL_C()
            CALL GET_FBUF_C(TSTOP,1)
            CALL GET_IBUF_C(IDEL_LOC,1)
            IDEL7NG = MAX(IDEL7NG,IDEL_LOC)
          END IF
          CALL SPMD_IBCAST(IDEL7NG,IDEL7NG,1,1,0,2)
          IF (IDEL7NG>=1) IDEL7NOK = 1
C----- Actualize  mass and inertia
C       IF (TT==ZERO) THEN
          R2RFX1 = ZERO
          R2RFX2 = ZERO
          DO IEX = 1, NR2RLNK
            IDG  = IEXLNK(1,IEX)
            IDP  = IEXLNK(2,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            GRNOD => IGRNOD(IDG)%ENTITY
            IF (RBYLNK(IEX)==0) THEN
              CALL SEND_MASS_SPMD(
     1           IDP,NNG,GRNOD,MS,IN,
     2           DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,ROTLNK(IEX))
            ELSE
              ALLOCATE (TAG_RBY(SIZE_TAG_RBY))
              CALL SEND_MASS_RBY_SPMD(
     1           IDP,NNG,GRNOD,MS,IN,
     2           DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,ROTLNK(IEX),
     3           NPBY,RBY,ADD_RBY(IEX))
            ENDIF
            CALL SPMD_IBCAST(ROTLNK(IEX),ROTLNK(IEX),1,1,0,2)
          END DO
C
          IF (TT==ZERO) THEN
            DO IEX = 1, NR2RLNK
              IDG  = IEXLNK(1,IEX)
              IDP  = IEXLNK(2,IEX)
              NNG  = IGRNOD(IDG)%NENTITY
              GRNOD => IGRNOD(IDG)%ENTITY
              IF (RBYLNK(IEX)==0) THEN
                CALL GET_MASS_SPMD(
     1           IDP,NNG,GRNOD,MS      ,IN     ,
     2           DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,IAD_ELEM,
     3           FR_ELEM,ROTLNK(IEX))
              ELSE
                CALL GET_MASS_RBY_SPMD(
     1           IDP,NNG,GRNOD,MS      ,IN     ,
     2           DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,IAD_ELEM,
     3           FR_ELEM,ROTLNK(IEX),X,NPBY,RBY,ITAB,IEX,XDP)
              ENDIF
            END DO
          ENDIF

        END IF
C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE R2R_INIT
Chd|====================================================================
Chd|  INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        INIT_BUF_SPMD_C               source/coupling/rad2rad/rad2rad_c.c
Chd|        INIT_LINK_SPMD_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_IDEF                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_IGET                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_IGET2                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_IGET4                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RGET3                source/mpi/r2r/spmd_r2r.F     
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE INIT_LINK_SPMD(
     1   IDP   ,NNG   ,ITAB ,GRNOD,X,
     2   DD_R2R,NGLOB,WEIGHT,ADDCNEL,CNEL,IXC,OFC,IEX,INFO,TYP,
     3   ICODT,ICODR,IBFV,DX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RAD2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "rad2r_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB,ITAB(*), GRNOD(*),
     .          WEIGHT(*), DD_R2R(*),OFC,
     .          ADDCNEL(0:*),CNEL(0:*),IXC(NIXC,*),IEX,
     .          INFO,TYP,ICODT(*),ICODR(*),IBFV(NIFV,*)
        my_real
     .          X(3,*),DX(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IBUF(NGLOB),TLEL,LEL(9*NNG),LELNBNOD(9*NNG),TLELN,
     .          LELNOD(9*NNG),NBELEM(NNG),CNELEM(9*NNG),IBUFNONBEL(NGLOB),
     .          TCNEL,TCNELDB,NNGDB,N,K,J,DBNBUF(NSPMD),DDBUF(NSPMD),
     .          BCS(NGLOB),IBUFBCS(NGLOB),I
        INTEGER, ALLOCATABLE :: IBUFEL(:),IBUFELNBNOD(:),IBUFELNOD(:),
     .          IBUFCNEL(:),CNELEMDB(:),DBIBUF(:),DBIBUFNONBEL(:),
     .          IBUFCNELDB(:)
        my_real BUFR(3,NGLOB),BUFR2(3,NGLOB)
C-----------------------------------------------
C*******************************************************************************************************************************************************C
C-  Array: (for each spmd domain)                                                            concatenation on proc 0 :         length :
C......... BCS : boundary conditions on interface nodes -----------------------------------------------------> IBUFBCS                 NNG
C......... CNELEM : list of elements attached to interface nodes --------------------------------------------> IBUFCNEL                TCNELT(IEX)
C......... NBELEM : nb of elements attached to interface nodes ----------------------------------------------> IBUFNONBEL              NNG
C......... CNELEMDB : list of elements attached to interface nodes that are on severla SPMD domains ---------> IBUFCNELDB              TCNELTDB(IEX)
C......... NBELEMDB : nb of elements attached to interface nodes that are on severla SPMD domains -----------> DBIBUFNONBEL            DBNO(IEX)
C..........LEL : List of id ( local numerotation + offset) of elements connected to the interface------------> IBUFEL                  NBELT_R2R(IEX)
C..........LELNBNOD : nb of interface nodes attached to each elements connected to the interface ------------> IBUFELNBNOD             NBELT_R2R(IEX)
C..........LELNOD : List of interface nodes attached to elements of LEL -------------------------------------> IBUFELNOD               NBELTN_R2R(IEX)
C*******************************************************************************************************************************************************C

        NNGDB = 0

        DO K = 1, NNG
          N=GRNOD(K)
          IF(WEIGHT(N)==1)THEN
            BCS(K) = 10*ICODT(N)
            IF (IRODDL==1) BCS(K) = BCS(K) + ICODR(N)
          ELSE
            NNGDB = NNGDB + 1
          END IF
        END DO

C      DO J = 1, NFXVEL
C       DO K = 1, NNG
C         IF ((IBFV(1,J)==GRNOD(K)).AND.(IBFV(2,J)>0)) THEN
C	  BCS(K) =  BCS(K) +100 ;
C	 ENDIF
C       END DO
C      END DO

C--------------------Search of noeuds/elements connectivities ( coupling type 1,2)-----C
        IF (TYP<4) THEN
          ALLOCATE(CNELEMDB(9*NNGDB))
          CALL INIT_BUF_SPMD_C(IDP,NNG,ITAB,GRNOD,X,ADDCNEL,CNEL,IXC,
     .            OFC,TLEL,LEL,LELNBNOD,TLELN,LELNOD,NBELEM,TCNEL,CNELEM,
     .            WEIGHT,TCNELDB,CNELEMDB,INFO,TYP,NGLOB)
        ENDIF

C-----------------------------------------------------------------------------------------C

        CALL SPMD_R2R_IDEF(NNG,GRNOD,WEIGHT,IEX,TLEL,TLELN,TCNEL,
     .          TCNELDB)
        CALL SPMD_R2R_RGET3(X,NNG,GRNOD,DD_R2R,WEIGHT,BUFR2)
        CALL SPMD_R2R_RGET3(DX,NNG,GRNOD,DD_R2R,WEIGHT,BUFR)
        CALL SPMD_R2R_IGET(ITAB,NNG,GRNOD,DD_R2R,WEIGHT,IBUF,1)

C--------------------Computation of initial coordinates in case of rerun------------------C

        DO I=1,NGLOB
          DO J=1,3
            BUFR(J,I)=  BUFR2(J,I)-BUFR(J,I)
          END DO
        END DO

C--------------------Allocation of buffers------------------------------------------------C
        IF (ISPMD>0) THEN
          ALLOCATE(DBIBUF(NNGDB))
        ELSE
          ALLOCATE(DBIBUF(DBNO(IEX)))
        ENDIF

C--------------------Allocation of element buffers ( coupling type 1,2)-------------------C

        IF (TYP<4) THEN
          IF (ISPMD>0) THEN
            ALLOCATE(IBUFEL(TLEL),IBUFELNBNOD(TLEL),IBUFELNOD(TLELN))
            ALLOCATE(IBUFCNEL(TCNEL),DBIBUFNONBEL(NNGDB))
            ALLOCATE(IBUFCNELDB(TCNELDB))
          ELSE
            ALLOCATE(IBUFEL(NBELT_R2R(IEX)),IBUFELNBNOD(NBELT_R2R(IEX)))
            ALLOCATE(IBUFELNOD(NBELTN_R2R(IEX)),IBUFCNEL(TCNELT(IEX)))
            ALLOCATE(DBIBUFNONBEL(DBNO(IEX)))
            ALLOCATE(IBUFCNELDB(TCNELTDB(IEX)))
          ENDIF
        ENDIF

C--------------------Creation of buffers by concatenation of arrays------------------------C
        CALL SPMD_R2R_IGET4(ITAB,NNG,GRNOD,DD_R2R,WEIGHT,DBIBUF,IEX,
     .   DBNBUF,DDBUF,1)
        CALL SPMD_R2R_IGET(BCS,NNG,GRNOD,DD_R2R,WEIGHT,IBUFBCS,0)

        IF (TYP<4) THEN
          CALL SPMD_R2R_IGET(NBELEM,NNG,GRNOD,DD_R2R,WEIGHT,IBUFNONBEL,0)
          CALL SPMD_R2R_IGET2(LEL,TLEL,IEX,IBUFEL,1)
          CALL SPMD_R2R_IGET2(CNELEM,TCNEL,IEX,IBUFCNEL,3)
          CALL SPMD_R2R_IGET2(CNELEMDB,TCNELDB,IEX,IBUFCNELDB,4)
          CALL SPMD_R2R_IGET2(LELNBNOD,TLEL,IEX,IBUFELNBNOD,0)
          CALL SPMD_R2R_IGET2(LELNOD,TLELN,IEX,IBUFELNOD,2)
          CALL SPMD_R2R_IGET4(NBELEM,NNG,GRNOD,DD_R2R,WEIGHT,
     .     DBIBUFNONBEL,IEX,DBNBUF,DDBUF,0)
        ENDIF

C--------------------Send  Rad2rad-------------------------------------------------------C

        IF(ISPMD==0)
     .    CALL INIT_LINK_SPMD_C(IDP,NGLOB,DBNO(IEX),NSPMD,IBUF,DBIBUF,
     .          DBNBUF,DDBUF,BUFR,TCNELT(IEX),IBUFNONBEL,IBUFCNEL,
     .          NBELT_R2R(IEX),NBELTN_R2R(IEX),IBUFEL,IBUFELNBNOD,
     .          IBUFELNOD,TCNELTDB(IEX),IBUFCNELDB,DBIBUFNONBEL,TYP,
     .          IBUFBCS,NCPRI,IRODDL,NBK,NR2RLNK,IEX)

C------------------------------------------------------------------------C
        DEALLOCATE(DBIBUF)

        IF (TYP<4) THEN
          DEALLOCATE(IBUFEL,IBUFELNBNOD,IBUFELNOD,IBUFCNEL,CNELEMDB)
          DEALLOCATE(DBIBUFNONBEL,IBUFCNELDB)
        ENDIF

        RETURN
      END SUBROUTINE INIT_LINK_SPMD
C
Chd|====================================================================
Chd|  SEND_MASS_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        SEND_MASS_SPMD_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_RGET                 source/mpi/r2r/spmd_r2r.F     
Chd|====================================================================
      SUBROUTINE SEND_MASS_SPMD(
     1   IDP      ,NNG   ,GRNOD, MS, IN,
     2   DD_R2R   ,NGLOB ,WEIGHT ,FLAG_ROT )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB, GRNOD(*),WEIGHT(*), DD_R2R(*),FLAG_ROT
        my_real MS(*), IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real BUFR1(NGLOB), BUFR2(NGLOB)
C
C******************************************************************************C
        CALL SPMD_R2R_RGET(MS,NNG,GRNOD,DD_R2R,WEIGHT,BUFR1)
        IF(IRODDL /= 0)THEN
          CALL SPMD_R2R_RGET(IN,NNG,GRNOD,DD_R2R,WEIGHT,BUFR2)
        ENDIF
        IF(ISPMD==0)
     .    CALL SEND_MASS_SPMD_C(IDP,NGLOB,BUFR1,BUFR2,FLAG_ROT)
C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE SEND_MASS_SPMD
C
Chd|====================================================================
Chd|  GET_MASS_SPMD                 source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        GET_MASS_SPMD_C               source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_RSET4                source/mpi/r2r/spmd_r2r.F     
Chd|====================================================================
      SUBROUTINE GET_MASS_SPMD(
     1   IDP      ,NNG   ,GRNOD,MS      ,IN,DD_R2R   ,
     2   NGLOB ,WEIGHT ,IAD_ELEM,FR_ELEM,FLAG_ROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB,
     .          GRNOD(*),FLAG_ROT,
     .          WEIGHT(*), DD_R2R(*), IAD_ELEM(2,*), FR_ELEM(*)
        my_real MS(*), IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LRBUF
        my_real BUFR1(NGLOB), BUFR2(NGLOB)
C
C******************************************************************************C
        IF(ISPMD==0)
     .    CALL GET_MASS_SPMD_C(IDP,NGLOB,BUFR1,BUFR2)
        LRBUF = 2*2*(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))+2*NSPMD
        CALL SPMD_R2R_RSET4(MS   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                     BUFR1,IAD_ELEM,FR_ELEM,LRBUF )
        IF(FLAG_ROT /= 0)THEN
          CALL SPMD_R2R_RSET4(IN   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                       BUFR2,IAD_ELEM,FR_ELEM,LRBUF )
        ENDIF
C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE GET_MASS_SPMD
C
Chd|====================================================================
Chd|  SEND_MASS_RBY_SPMD            source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        SEND_MASS_RBY_SPMD_C          source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_IGET                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RGET                 source/mpi/r2r/spmd_r2r.F     
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SEND_MASS_RBY_SPMD(
     1   IDP      ,NNG   ,GRNOD, MS, IN,
     2   DD_R2R   ,NGLOB ,WEIGHT ,FLAG_ROT,
     3   NPBY  ,RBY, ADDR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RAD2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB,GRNOD(*),
     .          WEIGHT(*), DD_R2R(*),FLAG_ROT,
     .          NPBY(*), ADDR
        my_real
     .          MS(*), IN(*), RBY(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,IDRBY(NNG),N,IBUF(NGLOB),BUFR3(NGLOB)
        my_real
     .          BUFR1(NGLOB),BUFR2(NGLOB),BUFR4(NGLOB),
     .          BUFR5(9*NGLOB)
C
C******************************************************************************C
        CALL SPMD_R2R_RGET(MS,NNG,GRNOD,DD_R2R,WEIGHT,BUFR1)
        IF(IRODDL /= 0)THEN
          CALL SPMD_R2R_RGET(IN,NNG,GRNOD,DD_R2R,WEIGHT,BUFR2)
        ENDIF

C-----------------------------------------------------------------------
        DO I=1,NNG
          N=GRNOD(I)
          DO J = 1,NRBODY
            IF ((N==NPBY(NNPBY*(J-1)+1)).AND.(N>0)) THEN
              IDRBY(I) = J
              TAG_RBY(ADDR+I) = J
            ENDIF
          END DO
        END DO

        CALL SPMD_R2R_IGET(IDRBY,NNG,GRNOD,DD_R2R,WEIGHT,IBUF,0)
C-----------------------------------------------------------------------

        IF(ISPMD==0) THEN
C-----------------------------------------------------------------------
          DO I = 1, NGLOB
            N = IBUF(I)
            BUFR3(I)= NPBY(NNPBY*(N-1)+3)
            BUFR4(I)= RBY(15,N)
            DO J = 1,9
              BUFR5(9*(I-1)+J)= RBY(16+J,N)
            END DO
          END DO
C-----------------------------------------------------------------------
          CALL SEND_MASS_RBY_SPMD_C(IDP,NGLOB,BUFR1,BUFR2,BUFR3,
     .          BUFR4,BUFR5,FLAG_ROT)
        ENDIF
C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE SEND_MASS_RBY_SPMD
C
Chd|====================================================================
Chd|  GET_MASS_RBY_SPMD             source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        GET_MASS_RBY_SPMD_C           source/coupling/rad2rad/rad2rad_c.c
Chd|        R2R_RBY                       source/coupling/rad2rad/r2r_init.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_R2R_IGET                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      SUBROUTINE GET_MASS_RBY_SPMD(
     1   IDP      ,NNG   ,GRNOD,MS      ,IN,DD_R2R   ,
     2   NGLOB ,WEIGHT ,IAD_ELEM,FR_ELEM,FLAG_ROT,
     3   X,NPBY,RBY,ITAB,IEX,XDP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB,
     .          GRNOD(*),FLAG_ROT,IEX,
     .          WEIGHT(*), DD_R2R(*), IAD_ELEM(2,*), FR_ELEM(*),
     .          NPBY(*),ITAB(*)
        my_real MS(*), IN(*), X(3,*),  RBY(NRBY,*)
        DOUBLE PRECISION XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LRBUF,I,N,J,IDRBY(NNG),IBUF(NGLOB),IBUF2(NGLOB),
     .          ID_RB
        my_real
     .          BUFR1(NGLOB),BUFR2(NGLOB),BUFR3(3*NGLOB),BUFR4(9*NGLOB),
     .          RBY_X(3,NRBODY)
C-----------------------------------------------

C--------------> Computation of RBY on proc 0---------------------------

C-----------------------------------------------------------------------
        DO I=1,NNG
          N=GRNOD(I)
          DO J = 1,NRBODY
            IF ((N==NPBY(NNPBY*(J-1)+1)).AND.(N>0)) IDRBY(I) = J
          END DO
        END DO
        CALL SPMD_R2R_IGET(IDRBY,NNG,GRNOD,DD_R2R,WEIGHT,IBUF,0)
        CALL SPMD_R2R_IGET(ITAB,NNG,GRNOD,DD_R2R,WEIGHT,IBUF2,1)

C-----------------------------------------------------------------------

        IF(ISPMD==0) THEN
          CALL GET_MASS_RBY_SPMD_C(IDP,NGLOB,BUFR1,BUFR2,BUFR3,BUFR4)
          DO I = 1, NGLOB
            IF(ISPMD==0) THEN
              N = IBUF(I)
              DO J = 1,3
                RBY_X(J,N)=BUFR3(3*(I-1)+J)
              END DO
              DO J = 1,9
                RBY(16+J,N)=BUFR4(9*(I-1)+J)
              END DO
            ENDIF
          END DO
          CALL R2R_RBY(NGLOB,IBUF2,IBUF,BUFR3,BUFR1,BUFR2,NPBY,RBY,
     .          XDP,NSPMD,WEIGHT)
        ENDIF

C--------------> Exchange of RBY between SPMD domains---------------------------

        DO I = 1, NGLOB
          IF(ISPMD==0) N = IBUF(I)
          CALL SPMD_IBCAST(N,N,1,1,0,2)
          CALL SPMD_RBCAST(RBY_X(1,N),RBY_X(1,N),3,1,0,2)
          CALL SPMD_RBCAST(RBY(1,N),RBY(1,N),NRBY,1,0,2)
        END DO

C--------> Affectation of X,M and I for each SPMD domain -------

        DO I = 1,NNG
          N = GRNOD(I)
          DO J = 1,NRBODY
            IF ((N==NPBY(NNPBY*(J-1)+1)).AND.(N>0)) ID_RB = J
          END DO
          X(1,N) = RBY_X(1,ID_RB)
          X(2,N) = RBY_X(2,ID_RB)
          X(3,N) = RBY_X(3,ID_RB)
          IF (IRESP==1) THEN
C-------Simple precision -> XDP is updated-----
            XDP(1,N)=X(1,N)
            XDP(2,N)=X(2,N)
            XDP(3,N)=X(3,N)
          ENDIF
          MS(N) = RBY(14,ID_RB)
          IN(N) = MIN(RBY(10,ID_RB),RBY(11,ID_RB),RBY(12,ID_RB))
        END DO

C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE GET_MASS_RBY_SPMD
C
Chd|====================================================================
Chd|  R2R_RBY                       source/coupling/rad2rad/r2r_init.F
Chd|-- called by -----------
Chd|        GET_MASS_RBY_SPMD             source/coupling/rad2rad/r2r_init.F
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        INEPRI                        source/constraints/general/rbody/inepri.F
Chd|====================================================================
      SUBROUTINE R2R_RBY(NNOD,ITAB,IBUF,X,MS,IN,NPBY,RBY,XDP,NPROC,
     .                   WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NNOD,IBUF(*),NPBY(*),ITAB(*),NPROC,WEIGHT(*)
        my_real MS(*), IN(*), X(*), RBY(NRBY,*)
        DOUBLE PRECISION XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N,ID,NOD,W,TAG(NRBODY)
        my_real RBYL(NRBY),XIIN
C
C******************************************************************************C
        TAG(:) = 0

        DO I=1,NNOD
          RBYL = 0
          IF (NPROC<2) THEN
            N = IBUF(I)
            DO J=1,NRBODY
              IF (N==NPBY(NNPBY*(J-1)+1)) ID = J
            END DO
          ELSE
            N = I
            ID = IBUF(I)
          ENDIF
          DO J=1,9
            RBYL(J)=RBY(16+J,ID)
          END DO
C---------Computation of principal inertias-----------------------
          CALL INEPRI(RBYL(10),RBYL)
C---------spherical inertia---------------------------------------
          IF (NPBY(NNPBY*(ID-1)+5)==1) THEN
            XIIN = (RBYL(10)+RBYL(11)+RBYL(12))* THIRD
            RBYL(10)=XIIN
            RBYL(11)=XIIN
            RBYL(12)=XIIN
          ENDIF
C-----------------------------------------------------------------
          DO J=1,12
            RBY(J,ID) = RBYL(J)
          END DO
          RBY(14,ID) = MS(N)
          IF (NPROC<2) IN(N) = MIN(RBYL(10),RBYL(11),RBYL(12))
          TAG(ID)=N
        END DO

C---------Printout for Rigid bodies------------------------------
        DO ID=1,NRBODY
          N = TAG(ID)
          IF (N/=0) THEN
            IF (WEIGHT(N)==1) WRITE(IOUT,1000)
            NOD = ITAB(N)
            IF ((IRESP==1).AND.(NPROC==1)) THEN
C-------Simple precision + nproc=1 -> update of XDP--------------
              XDP(1,N)=X(3*(N-1)+1)
              XDP(2,N)=X(3*(N-1)+2)
              XDP(3,N)=X(3*(N-1)+3)
            ENDIF
            IF (WEIGHT(N)==1) THEN
              IF (NPBY(NNPBY*(ID-1)+5)==1) THEN
C-------Spherical inertia -> global matrix is not printed-----
                WRITE(IOUT,1100) ID,NOD,X(3*(N-1)+1),X(3*(N-1)+2),X(3*(N-1)+3),
     .               2*MS(N)
              ELSE
                WRITE(IOUT,1100) NPBY(NNPBY*(ID-1)+6),NOD,X(3*(N-1)+1),
     .               X(3*(N-1)+2),X(3*(N-1)+3),2*MS(N)
                WRITE(IOUT,1300) 2*RBY(17,ID),2*RBY(21,ID),2*RBY(25,ID),
     .               2*RBY(18,ID),2*RBY(22,ID),2*RBY(19,ID)
              ENDIF
              WRITE(IOUT,1200) 2*RBY(10,ID),2*RBY(11,ID),2*RBY(12,ID)
            ENDIF
          ENDIF
        END DO

C-----------------------------------------------------------------
        RETURN
C
1000    FORMAT(/40H MULTIDOMAINS -> RIGID BODY ASSEMBLAGE  )
1100    FORMAT(5X,'RIGID BODY ID',I10
     .         /10X,'PRIMARY NODE    ',I10
     .         /10X,'NEW X,Y,Z       ',1P3G14.7
     .         /10X,'NEW MASS        ',1G14.7)
1300    FORMAT(10X,'NEW INERTIA xx yy zz ',3G14.7
     .         /10X,'NEW INERTIA xy yz zx ',3G14.7)
1200    FORMAT(10X,'PRINCIPAL INERTIA',1P3G20.13,/)
      END SUBROUTINE R2R_RBY
C
