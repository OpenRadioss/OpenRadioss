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
Chd|  WRRESTP                       source/output/restart/wrrestp.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        COMPRESS_I_NNZ                ../common_source/comm/compress_nnz.F
Chd|        COMPRESS_R_NNZ                ../common_source/comm/compress_nnz.F
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        FXBWREST                      source/output/restart/wrrest.F
Chd|        INTFRIC_WRESTI                share/modules/intbuf_fric_mod.F
Chd|        INTFRIC_WRESTR                share/modules/intbuf_fric_mod.F
Chd|        INTSTAMP_WRESTI               share/modules/intstamp_mod.F  
Chd|        INTSTAMP_WRESTR               share/modules/intstamp_mod.F  
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        PBLAST_WRITE_ENGINE           ../common_source/modules/pblast_mod.F
Chd|        SPMD_GLOB_DMIN9               source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_SAVESPH                  source/mpi/sph/spmd_sptool.F  
Chd|        TABLE_WRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|        WRCOMI                        source/output/restart/wrcomm.F
Chd|        WRCOMR                        source/output/restart/wrcomm.F
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|        WRITE_ELGROUP_PARAM           source/output/restart/write_elgroup_param.F
Chd|        WRITE_INTBUF                  source/output/restart/write_intbuf.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRITE_JOINT                   source/output/restart/write_joint.F
Chd|        WRITE_MATPARAM                source/output/restart/write_matparam.F
Chd|        WRITE_NLOC_STRUCT             source/output/restart/write_nloc_struct.F
Chd|        WRITE_SENSOR_TAB              source/output/restart/write_sensor_tab.F
Chd|        W_BUFBRIC_22                  source/interfaces/int22/w_bufbric_22.F
Chd|        W_CLUSTER                     source/output/cluster/w_cluster.F
Chd|        W_ELBUF_STR                   source/elements/elbuf/w_elbuf_str.F
Chd|        W_FAILWAVE                    source/output/restart/w_failwave.F
Chd|        W_GROUP_STR                   source/output/restart/w_group_str.F
Chd|        W_LINE_STR                    source/output/restart/w_line_str.F
Chd|        W_SUBSET_STR                  source/output/restart/w_subset_str.F
Chd|        W_SURF_STR                    source/output/restart/w_surf_str.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        GROUP_MOD                     share/modules/group_mod.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INT8_MOD                      ../common_source/modules/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        SMS_MOD                       share/modules/sms_mod.F       
Chd|        SPH_MOD                       share/modules/sph_mod.F       
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        TABLE_GLOB_MOD                share/modules/table_glob_mod.F
Chd|====================================================================
      SUBROUTINE WRRESTP(AF       ,IAF     ,XDP   ,ICH     ,ADDCNE,
     .           ELBUF_TAB,XFEM_TAB,INTBUF_TAB, MULTI_FVM,
     .           H3D_DATA ,INTBUF_FRIC_TAB,SUBSET,MATPARAM_TAB,GROUP_PARAM_TAB,
     .           PINCH_DATA, ALE_CONNECTIVITY, T_MONVOL,
     .           SENSOR_TAB,EBCS_TAB,DYNAIN_DATA,USER_WINDOWS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFMOD
      USE INTSTAMP_GLOB_MOD
      USE SMS_MOD
      USE TABLE_GLOB_MOD
      USE ELBUFDEF_MOD
      USE SPH_MOD
      USE INTBUFDEF_MOD
      USE INT8_MOD
      USE CLUSTER_MOD
      USE ECND_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE GROUP_MOD
      USE MATPARAM_DEF_MOD
      USE PINCHTYPE_MOD
      USE INOUTFILE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MONVOL_STRUCT_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE SEATBELT_MOD
      USE PBLAST_MOD
      USE OUTMAX_MOD
      USE OUTPUTS_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
C ecriture restart pour version spmd
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAF(*),ICH,ADDCNE(*)
      my_real
     .   AF(*)
      DOUBLE PRECISION XDP(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT)               :: MULTI_FVM
c     TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTER
      TYPE(H3D_DATABASE), INTENT(IN)                      :: H3D_DATA
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE(SUBSET_), TARGET, DIMENSION(NSUBS)             :: SUBSET
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT)         :: MATPARAM_TAB
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP)         :: GROUP_PARAM_TAB
      TYPE(PINCH)                                         :: PINCH_DATA
      TYPE(t_ale_connectivity), INTENT(IN)                :: ALE_CONNECTIVITY
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN)  :: T_MONVOL
      TYPE (SENSOR_STR_)  , DIMENSION(NSENSOR),INTENT(IN) :: SENSOR_TAB
      TYPE(t_ebcs_tab), INTENT(INOUT)                     :: EBCS_TAB
      TYPE (DYNAIN_DATABASE), INTENT(INOUT)               :: DYNAIN_DATA
      TYPE(USER_WINDOWS_),INTENT(IN)                      :: USER_WINDOWS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chara_c.inc"
#include      "couple_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com06_c.inc"
#include      "com07_c.inc"
#include      "com_xfem1.inc"
#include      "intstamp_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr19_c.inc"
#include      "scrcut_c.inc"
#include      "scrfs_c.inc"
#include      "scrnoi_c.inc"
#include      "scr_fac_c.inc"
#include      "sms_c.inc"
#include      "spmd_c.inc"
#include      "tabsiz_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
#include      "thermal_c.inc"
#include      "ale_param.inc"
#include      "inter18.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,LEN,IFILNAM(2148), IFIL,LEN_G,LEN_M,LENG,LEN_S,IXEL,
     .        FLAG_XFEM
      CHARACTER FILNAM*100, FILNAMR*100, FILNAMG*128,
     .          PROCNAM*4, CHRUNR*4,PLAST*4
      CHARACTER*1 IJK(26)
      INTEGER LHEADER,LVARINT,LVARREA,LMXVINT,LMXVREA,M
      PARAMETER (LHEADER=15)
      PARAMETER (LMXVINT=4000)
      PARAMETER (LMXVREA=1000+30*MAXLAW+30)
      INTEGER   TABHEAD(LHEADER),TABVINT(LMXVINT)
      my_real
     .          TABVREA(LMXVREA), ENTMP(100)
      INTEGER MY_ILEN,MY_RLEN,LTABLE, LENI, LENR
      INTEGER NBT8

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME
      INTEGER, DIMENSION(NVOLU) :: NTRI
      INTEGER :: II, NS_DIFF, ITMP
      INTEGER :: MY_SIZE
C-----------------------------------------------
      DATA IJK/'I','J','K','L','M','N','O','P','Q','R',
     .         'S','T','U','V','W','X','Y','Z','A','B',
     .         'C','D','E','F','G','H'/
C--------------------------------------
C     PREPARATION DES VARIABLES ENTIERES ET REELLES.
C--------------------------------------
      IPARI0 = IPARIT
      CALL WRCOMI(LMXVINT,LVARINT,TABVINT,1,MULTI_FVM,H3D_DATA,DYNAIN_DATA)
C
      ENTMP(1) = TFEXT
      ENTMP(2) = EHOUR
      ENTMP(3) = ECONTV
      ENTMP(4) = EPOR
      ENTMP(5) = HEAT_STORED
      ENTMP(6) = HEAT_MECA
      ENTMP(7) = HEAT_CONV
      ENTMP(8) = HEAT_RADIA
      ENTMP(9) = HEAT_FFLUX
      ENTMP(10)= ECONTD
      ENTMP(11)= ECONT_CUMU
      IF(NSPMD > 1) CALL SPMD_GLOB_DSUM9(ENTMP,11)
      IF(ISPMD/=0) THEN
        TFEXT = ZERO
        EHOUR = ZERO
        ECONTV = ZERO
        EPOR = ZERO
        HEAT_STORED = ZERO
        HEAT_MECA   = ZERO
        HEAT_CONV   = ZERO
        HEAT_RADIA  = ZERO
        HEAT_FFLUX  = ZERO
        ECONTD = ZERO
        ECONT_CUMU = ZERO
      ELSE
        TFEXT = ENTMP(1)
        EHOUR = ENTMP(2)
        ECONTV= ENTMP(3)
        EPOR  = ENTMP(4)
        HEAT_STORED = ENTMP(5)
        HEAT_MECA   = ENTMP(6)
        HEAT_CONV   = ENTMP(7)
        HEAT_RADIA  = ENTMP(8)
        HEAT_FFLUX  = ENTMP(9)
        ECONTD = ENTMP(10)
        ECONT_CUMU = ENTMP(11)
      ENDIF
C
      IF(NWALE == 2) THEN
        ENTMP(1) = VGZ
        IF(NSPMD > 1) CALL SPMD_GLOB_DMIN9(ENTMP,1)
        IF(ISPMD==0) VGZ = ENTMP(1)
      ENDIF
C
      CALL WRCOMR(LMXVREA,LVARREA,TABVREA,DYNAIN_DATA)
C--------------------------------------
C     OPEN
C--------------------------------------
      WRITE(CHRUNR,'(I4.4)')IRUN
      WRITE(PROCNAM,'(I4.4)')ISPMD+1
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUNR//'_'//PROCNAM
      WRITE(PLAST,'(I4.4)')NSPMD
      FILNAMG=ROOTNAM(1:ROOTLEN)//'_'//CHRUNR//'_[0001-'//PLAST//']'
      LENG = ROOTLEN+17
      LEN = ROOTLEN + 10
      FILNAMR = FILNAM
      IFIL = 2
      IF(ICH/=0)IFIL = 41
      IF(MULTIREST/=0.AND.MCHECK==0)THEN
        IRPREV = 1 + MOD(IRPREV,MULTIREST)
        FILNAM=FILNAM(1:LEN)//'_'//IJK(IRPREV)
        LEN = LEN+2
        FILNAMG=FILNAMG(1:LENG)//'_'//IJK(IRPREV)
        LENG = LENG+2
      ENDIF
      FILNAM=FILNAM(1:LEN)//'.rst'
      LEN = LEN +4
      FILNAMG=FILNAMG(1:LENG)//'.rst'
      LENG = LENG+4

      LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN)

      DO I = 1, LEN_TMP_NAME
        IFILNAM(I) = ICHAR(TMP_NAME(I:I))
      END DO
      CALL CUR_FIL_C(IFIL)
      IF (IRFORM/5==2) THEN
        CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
      ELSEIF (IRFORM/5==3) THEN
        CALL OPEN_C(IFILNAM,LEN_TMP_NAME,3)
      ELSEIF (IRFORM/5==4) THEN
        CALL OPEN_C(IFILNAM,LEN_TMP_NAME,6)
      ENDIF
      DO I = 1, LEN
        IFILNAM(I) = ICHAR(FILNAM(I:I))
      END DO
      CALL WRITE_C_C(IFILNAM,LEN)
C--------------------------------------
C     ECRITURE DU TEXTE
C--------------------------------------
      CALL WRITE_I_C(TEXT,LCOM05)
C--------------------------------------
C     ECRITURE DU HEADER.
C--------------------------------------
C     RADIOSS FILE R/W
      TABHEAD(1) =IRADIOS
C     RESTART FILE
      TABHEAD(2) =IRESFIL
C     ITESTV
      TABHEAD(3) =ITESTV
C     THIS RUN NUMBER
      TABHEAD(4) =IRUN
C     CODE FOR THIS RUN (-1:UNKNOWN,0:STARTER,1:ENGINE,2:MODIF)
      ICODRUN =1
      TABHEAD(5) =ICODRUN
C     CODVERS FOR THIS RUN
      TABHEAD(6) =CODVERS
C     MINOR VERSION CODE FOR THIS RUN
      TABHEAD(7) =IMINVER
C     SOURCE VERSION CODE FOR THIS RUN
      TABHEAD(8) =ISRCVER
C     INPUT VERSION FOR THIS RUN
      TABHEAD(9) =INVERS
C     STARTER CODVERS
      TABHEAD(10)=SCODVER
C     STARTER MINOR VERSION CODE.
      TABHEAD(11)=SMINVER
C     STARTER SOURCE VERSION CODE.
      TABHEAD(12)=SSRCVER
C     STARTER INPUT VERSION
      TABHEAD(13)=INVSTR
C
      TABHEAD(14) =LVARINT
      TABHEAD(15)=LVARREA
      CALL WRITE_I_C(LHEADER, 1)
      CALL WRITE_I_C(TABHEAD, LHEADER)
C--------------------------------------
C     ECRITURE DES VARIABLES ENTIERES
C--------------------------------------
      CALL WRITE_I_C(TABVINT,LVARINT)
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      LEN_G = NPROPGI*NUMGEO
      LEN_M = NPROPMI*NUMMAT
      LEN_S = NPART
        CALL WRITE_I_C(NODGLOB,SNODGLOB)

        CALL WRITE_I_C(MAIN_PROC,SWEIGHT)

        CALL WRITE_I_C(WEIGHT,SWEIGHT)

        CALL WRITE_I_C(ICODE,SICODE)

        CALL WRITE_I_C(LSHSUB,SLSHSUB)

        CALL WRITE_I_C(ISKEW,SISKEW)

        CALL WRITE_I_C(ISKWN,SISKWN)

        CALL WRITE_I_C(IFRAME,SIFRAME)

        CALL WRITE_I_C(NETH,SNETH)

        CALL WRITE_I_C(IBCSLAG,SIBCSLAG)

        CALL WRITE_I_C(IPART,SIPART)

        CALL WRITE_I_C(NOM_OPT,SNOM_OPT)

        CALL WRITE_I_C(NPC,SNPC)

        CALL WRITE_I_C(IXTG,SIXTG)

        CALL WRITE_I_C(IXTG1,SIXTG1)

        CALL WRITE_I_C(IXS,SIXS)

        CALL WRITE_I_C(IXQ,SIXQ)

        CALL WRITE_I_C(IXC,SIXC)

        CALL WRITE_I_C(IXT,SIXT)

        CALL WRITE_I_C(IXP,SIXP)

        CALL WRITE_I_C(IXR,SIXR)

        CALL WRITE_I_C(ITAB,SITAB)

        CALL WRITE_I_C(ITABM1,SITABM1)

        CALL WRITE_I_C(GJBUFI,SGJBUFI)

        CALL WRITE_I_C(ALE_CONNECTIVITY%NALE,SNALE)

        IF (IALE + IEULER + IALELAG + ITHERM /= 0) THEN
           ITMP = SIZE(ALE_CONNECTIVITY%ee_connect%iad_connect)
           CALL WRITE_I_C(ITMP, 1)
           CALL WRITE_I_C(ALE_CONNECTIVITY%ee_connect%iad_connect, ITMP)

           ITMP = SIZE(ALE_CONNECTIVITY%ee_connect%connected)
           CALL WRITE_I_C(ITMP, 1)
           CALL WRITE_I_C(ALE_CONNECTIVITY%ee_connect%connected, ITMP)

           ITMP = SIZE(ALE_CONNECTIVITY%ee_connect%type)
           CALL WRITE_I_C(ITMP, 1)
           CALL WRITE_I_C(ALE_CONNECTIVITY%ee_connect%type, ITMP)

           ITMP = SIZE(ALE_CONNECTIVITY%ee_connect%iface2)
           CALL WRITE_I_C(ITMP, 1)
           CALL WRITE_I_C(ALE_CONNECTIVITY%ee_connect%iface2, ITMP)
        ENDIF

        CALL WRITE_I_C(ID_GLOBAL_VOIS,SIELVS)

        CALL WRITE_I_C(FACE_VOIS,SIELVS)

        IF(IALE > 0 .AND. NWALE == 6 .OR. INTER18_AUTOPARAM == 1 .OR. INTER18_IS_VARIABLE_GAP_DEFINED) THEN
           CALL WRITE_I_C(NE_NERCVOIS, NSPMD + 1)
           CALL WRITE_I_C(NE_LERCVOIS, NE_SLERCVOIS)
           CALL WRITE_I_C(NE_NESDVOIS, NSPMD + 1)
           CALL WRITE_I_C(NE_LESDVOIS, NE_SLESDVOIS)
        ENDIF
        IF (IALE > 0 .AND. (NWALE < 2 .OR. NWALE == 5)) THEN
           CALL WRITE_I_C(ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT, NUMNOD + 1)
           CALL WRITE_I_C(ALE_CONNECTIVITY%NN_CONNECT%CONNECTED, ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NUMNOD + 1))
        ENDIF
        IF (IALE > 0 .AND. NWALE == 6 .OR. INTER18_AUTOPARAM == 1 .OR. INTER18_IS_VARIABLE_GAP_DEFINED) THEN
           CALL WRITE_I_C(ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT, NUMNOD + 1)
           CALL WRITE_I_C(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED,
     .          ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NUMNOD + 1))
        ENDIF

        NS_DIFF = 0
        IF (MULTI_FVM%NS_DIFF) THEN
           NS_DIFF = 1
        ENDIF
        CALL WRITE_I_C(NS_DIFF, 1)
        IF (NS_DIFF == 1) THEN
           CALL WRITE_I_C(ALE_CONNECTIVITY%IDGLOB%ID,
     .          NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS)
        ENDIF

        CALL WRITE_I_C(IFILL,SIFILL)

        CALL WRITE_I_C(IMS,SIMS)

        CALL W_SUBSET_STR(SUBSET)

        CALL W_GROUP_STR

        CALL W_SURF_STR(IGRSURF)

        CALL W_LINE_STR(IGRSLIN)

        CALL WRITE_I_C(KXX,SKXX)

        CALL WRITE_I_C(IXX,SIXX)

        CALL WRITE_I_C(KXSP,SKXSP)

        CALL WRITE_I_C(IXSP,SIXSP)

        CALL WRITE_I_C(NOD2SP,SNOD2SP)

        CALL WRITE_I_C(ISPSYM,SISPSYM)

        CALL WRITE_I_C(ISPCOND,SISPCOND)

        CALL WRITE_I_C(ISPHIO,SISPHIO)

        CALL WRITE_I_C(LPRTSPH,SLPRTSPH)

        CALL WRITE_I_C(LONFSPH,SLONFSPH)

        CALL WRITE_I_C(IBUFSSG_IO,SIBUFSSG_IO)

        IF(NSPHSOL/=0)THEN

          CALL WRITE_I_C(SPH2SOL,NUMSPH)

          CALL WRITE_I_C(SOL2SPH,2*NUMELS8)

          CALL WRITE_I_C(IRST,3*NSPHSOL)

          CALL WRITE_I_C(SOL2SPH_TYP,NUMELS8)
        END IF

        CALL WRITE_I_C(IBCL,SIBCL)

        IF (NINTLOADP>0) THEN
            CALL WRITE_I_C(KLOADPINTER, NINTER+1)
            CALL WRITE_I_C(LOADPINTER,NINTER*NLOADP_HYD)
        ENDIF

        CALL WRITE_I_C(IBFV,SIBFV)

        CALL WRITE_I_C(IECRAN,SIECRAN)

        CALL WRITE_I_C(ILAS,SILAS)
       
        ! -----------------------------
        ! EBCS option
        CALL WRITE_I_C(EBCS_TAB%nebcs, 1) ! total number of EBCS
        CALL WRITE_I_C(EBCS_TAB%nebcs_fvm, 1) ! number of multifluid ebcs
        CALL WRITE_I_C(EBCS_TAB%nebcs_nrf, 1) ! number of /NRF ebcs
        CALL WRITE_I_C(EBCS_TAB%nebcs_loc, 1) ! number of other ebcs
        LENI = 0
        LENR = 0
        IF (EBCS_TAB%nebcs > 0) THEN
            ! write the type of /EBCS ( /EBCS/NRF --> typ = 10)
            CALL WRITE_I_C(EBCS_TAB%my_typ,EBCS_TAB%nebcs)
        ENDIF
        ! -------------
        ! write the data of /EBCS options
        IF (EBCS_TAB%nebcs_loc +EBCS_TAB%nebcs_nrf + EBCS_TAB%nebcs_fvm> 0) THEN
           DO II = 1, EBCS_TAB%nebcs_fvm + EBCS_TAB%nebcs_loc + EBCS_TAB%nebcs_nrf
              CALL EBCS_TAB%tab(II)%poly%write_common_data(leni, lenr)
              CALL EBCS_TAB%tab(II)%poly%write_data(leni, lenr)
           ENDDO
        ENDIF
        ! -------------
        ! -----------------------------

        CALL WRITE_I_C(LACCELM,SLACCELM)

        CALL WRITE_I_C(LGAUGE,3*NBGAUGE)

        CALL WRITE_I_C(NNLINK,SNNLINK)

        CALL WRITE_I_C(LNLINK,SLNLINK)

        CALL USERWI_WRITE(USER_WINDOWS,ISPMD,NSPMD,NUMNOD)

        CALL WRITE_I_C(IPARG,SIPARG)

        CALL WRITE_I_C(DD_IAD,SDD_IAD)

        CALL WRITE_I_C(IGRV,SIGRV)

        CALL WRITE_I_C(LGRAV,SLGRAV)

        CALL WRITE_I_C(IEXLNK,SIEXLNK)

        CALL WRITE_I_C(IBVEL,SIBVEL)

        CALL WRITE_I_C(LBVEL,SLBVEL)

        CALL WRITE_I_C(IRBE2,SIRBE2)

        CALL WRITE_I_C(LRBE2,SLRBE2)

        CALL WRITE_I_C(IRBE3,SIRBE3)

        CALL WRITE_I_C(LRBE3,SLRBE3)

        CALL WRITE_I_C(IACTIV,SIACTIV)

        CALL WRITE_I_C(IBMPC,SIBMPC)

        CALL WRITE_I_C(KINET,SKINET)

        CALL WRITE_I_C(FASOLFR,SFASOLFR)

        CALL WRITE_I_C(SEGQUADFR,SSEGQUADFR)

        CALL WRITE_I_C(IPARI,SIPARI)

c        CALL WRITE_I_C(INBUF,SINBUF)

        CALL W_BUFBRIC_22() !inter22

        !write new structure INTBUF_TAB
        CALL WRITE_INTBUF(INTBUF_TAB)

        CALL WRITE_I_C(NPRW,SNPRW)

        CALL WRITE_I_C(LPRW,SLPRW)

        CALL WRITE_I_C(ICONX,SICONX)

        CALL WRITE_I_C(MADPRT,SMADPRT)

        CALL WRITE_I_C(MADSH4,SMADSH4)

        CALL WRITE_I_C(MADSH3,SMADSH3)

        CALL WRITE_I_C(MADSOL,SMADSOL)

        CALL WRITE_I_C(MADNOD,SMADNOD)

        CALL WRITE_I_C(MADCLNOD,MADCLNODS)

        CALL WRITE_I_C(MADFAIL,SMADFAIL)

        CALL WRITE_I_C(MADIDX,SMADIDX)

        CALL WRITE_I_C(NPBY,SNPBY)

        CALL WRITE_I_C(NPBYL,SNPBYL)

        CALL WRITE_I_C(LPBY,SLPBY)

        CALL WRITE_I_C(LPBYL,SLPBYL)

        CALL WRITE_I_C(LRIVET,SLRIVET)

        CALL WRITE_I_C(NSTRF,SNSTRF)

        CALL WRITE_I_C(NOM_SECT,SNOM_SECT)

        CALL WRITE_JOINT()

        CALL WRITE_I_C(NODPOR,SNODPOR)

        CALL WRITE_I_C(MONVOL,SMONVOL)

        DO II = 1, NVOLU
           NTRI(II) = T_MONVOL(II)%NB_FILL_TRI
        ENDDO

        CALL WRITE_I_C(NTRI, NVOLU)
        DO II = 1, NVOLU
           IF (NTRI(II) > 0) THEN
              CALL WRITE_I_C(T_MONVOL(II)%FILL_TRI(1:3* NTRI(II)), 3* NTRI(II))
           ENDIF
        ENDDO

        CALL WRITE_I_C(LAGBUF,SLAGBUF)

        IF(NS10E > 0)THEN
          CALL WRITE_I_C(ICNDS10,3*NS10E)
        ENDIF

        CALL WRITE_I_C(ITHGRP,SITHGRP)

        CALL WRITE_I_C(ITHBUF,SITHBUF)

        CALL WRITE_I_C(ITHGRPA,SITHGRPA)

        CALL WRITE_I_C(ITHBUFA,SITHBUFA)

        CALL WRITE_I_C(ITHGRPB,SITHGRPB)

        CALL WRITE_I_C(ITHBUFB,SITHBUFB)

        CALL WRITE_I_C(ITHGRPC,SITHGRPC)

        CALL WRITE_I_C(ITHBUFC,SITHBUFC)

        CALL WRITE_I_C(ITHGRPD,SITHGRPD)

        CALL WRITE_I_C(ITHBUFD,SITHBUFD)

        CALL WRITE_I_C(ITHGRPE,SITHGRPE)

        CALL WRITE_I_C(ITHBUFE,SITHBUFE)

        CALL WRITE_I_C(ITHGRPF,SITHGRPF)

        CALL WRITE_I_C(ITHBUFF,SITHBUFF)

        CALL WRITE_I_C(ITHGRPG,SITHGRPG)

        CALL WRITE_I_C(ITHBUFG,SITHBUFG)

        CALL WRITE_I_C(ITHGRPH,SITHGRPH)

        CALL WRITE_I_C(ITHBUFH,SITHBUFH)

        CALL WRITE_I_C(ITHGRPI,SITHGRPI)

        CALL WRITE_I_C(ITHBUFI,SITHBUFI)

        CALL WRITE_I_C(FR_MV,SFR_MV)

        CALL WRITE_I_C(IAD_ELEM,SIAD_ELEM)

        CALL WRITE_I_C(IAD_RBY,SIAD_RBY)

        CALL WRITE_I_C(IAD_RBY2,SIAD_RBY2)

        CALL WRITE_I_C(IAD_I2M,SIAD_I2M)

        CALL WRITE_I_C(IADCJ,SIADCJ)

        CALL WRITE_I_C(IAD_RBM,SIAD_RBM)

        CALL WRITE_I_C(IAD_RBM2,SIAD_RBM2)

        CALL WRITE_I_C(IAD_RBE2,SIAD_RBE2)

        CALL WRITE_I_C(IAD_RBE3M,SIAD_RBE3M)

        CALL WRITE_I_C(IAD_SEC,SIAD_SEC)

        CALL WRITE_I_C(IAD_CUT,SIAD_CUT)
C
        CALL WRITE_I_C(IAD_RBYM,SIAD_RBYM)

        CALL WRITE_I_C(IAD_RBYM2,SIAD_RBYM2)
C
        CALL WRITE_I_C(FR_ELEM,SFR_ELEM)

        CALL WRITE_I_C(FR_RBY,SFR_RBY)

        CALL WRITE_I_C(FR_WALL,SFR_WALL)

        CALL WRITE_I_C(FR_RBY2,SFR_RBY2)

        CALL WRITE_I_C(FR_I2M,SFR_I2M)

        CALL WRITE_I_C(FR_LL,SFR_LL)

        CALL WRITE_I_C(FR_CJ,SFR_CJ)

        CALL WRITE_I_C(FR_RBM,SFR_RBM)

        CALL WRITE_I_C(FR_RBM2,SFR_RBM2)

        CALL WRITE_I_C(FR_RBE2,SFR_RBE2)

        CALL WRITE_I_C(FR_RBE3M,SFR_RBE3M)

        CALL WRITE_I_C(FR_RBE3MP,SFR_RBE3M)

        CALL WRITE_I_C(FR_SEC,SFR_SEC)

        CALL WRITE_I_C(FR_CUT,SFR_CUT)

        CALL WRITE_I_C(RG_CUT,SRG_CUT)

        CALL WRITE_I_C(FR_MAD,SFR_MAD)

        CALL WRITE_I_C(FR_I18,SFR_I18)

        CALL WRITE_I_C(DD_R2R,SDD_R2R)

        CALL WRITE_I_C(DD_R2R_ELEM,SDD_R2R_ELEM)

        CALL WRITE_I_C(FR_RBYM,SFR_RBYM)

        CALL WRITE_I_C(FR_RBYM2,SFR_RBYM2)

        CALL WRITE_I_C(ADDCSRECT,NUMNOR+1)

        CALL WRITE_I_C(FR_NOR,NBDDNORT)

        CALL WRITE_I_C(IAD_FRNOR,(NSPMD+1)*NINTER25)

        CALL WRITE_I_C(PROCNOR,NBCCNOR)

        CALL WRITE_I_C(FR_EDG,2*NBDDEDGT)

        CALL WRITE_I_C(IAD_FREDG,(NSPMD+1)*NINTER25)

        CALL WRITE_I_C(ISKWP,SISKWP)

        CALL WRITE_I_C(NSKWP,SNSKWP)

        IF(SNSKWP>0) CALL WRITE_I_C(ISKWP_L,NSKWP(ISPMD+1))

        CALL WRITE_I_C(ISENSP,SISENSP)

        CALL WRITE_I_C(NSENSP,SNSENSP)

        CALL WRITE_I_C(IACCP,SIACCP)

        CALL WRITE_I_C(NACCP,SNACCP)

        CALL WRITE_I_C(IGAUP,SIGAUP)

        CALL WRITE_I_C(NGAUP,SNGAUP)

        CALL WRITE_I_C(FR_LAGF,SFR_LAGF)

        CALL WRITE_I_C(NEWFRONT,SNEWFRONT)

        CALL WRITE_I_C(NBRCVOIS,SNBRCVOIS)

        CALL WRITE_I_C(LNRCVOIS,SLNRCVOIS)

        CALL WRITE_I_C(NBSDVOIS,SNBSDVOIS)

        CALL WRITE_I_C(LNSDVOIS,SLNSDVOIS)

        CALL WRITE_I_C(NERCVOIS,SNERCVOIS)

        CALL WRITE_I_C(LERCVOIS,SLERCVOIS)

        CALL WRITE_I_C(NESDVOIS,SNESDVOIS)

        CALL WRITE_I_C(LESDVOIS,SLESDVOIS)

        CALL WRITE_I_C(NPSEGCOM,SNPSEGCOM)

        CALL WRITE_I_C(LSEGCOM,SLSEGCOM)

        CALL WRITE_I_C(NPORGEO,SNPORGEO)

        CALL WRITE_I_C(LNODPOR,SLNODPOR)

        CALL WRITE_I_C(LLAGF,SLLAGF)

        IF(ICRACK3D > 0)THEN
          CALL WRITE_I_C(IAD_EDGE,SIAD_EDGE)
          CALL WRITE_I_C(FR_EDGE,SFR_EDGE)
          CALL WRITE_I_C(FR_NBEDGE,SFR_NBEDGE)
        ENDIF

c        IF(NS10E > 0)THEN
          CALL WRITE_I_C(IAD_CNDM,SIAD_CNDM)
          CALL WRITE_I_C(FR_CNDM,SFR_CNDM)
c        ENDIF

      IF(IPARIT==1)THEN
        CALL WRITE_I_C(ADDCNE,NUMNOD+1)
        CALL WRITE_I_C(PROCNE,LCNE0)
        IF(I2NSNT>0)THEN
          CALL WRITE_I_C(ADDCNI2,NUMNOD+1)
        ENDIF
        CALL WRITE_I_C(PROCNI2,LCNI2)
        IF(NS10E>0) THEN
         CALL WRITE_I_C(ADDCNCND,SADDCNCND)
        ENDIF
        CALL WRITE_I_C(PROCNCND,SPROCNCND)

        CALL WRITE_I_C(IADS,NISKY0)
        CALL WRITE_I_C(IADWAL,NSKYRW0)
        CALL WRITE_I_C(IADRBK,NSKYRBK0)
        CALL WRITE_I_C(IADI2,NISKYI2)
        CALL WRITE_I_C(IADCND,SIADCND)
        CALL WRITE_I_C(IADMV2,NSKYMV0)
        CALL WRITE_I_C(IADMV3,NSKYMVC0)
        CALL WRITE_I_C(IADLL,NSKYLL0)
        CALL WRITE_I_C(IADRBM,NSKYRBM0)
        CALL WRITE_I_C(IADI18,NSKYI18)
        CALL WRITE_I_C(IADRBMK,NSKYRBMK0)
C
C  ply xfem
C
        IF(IPLYXFEM > 0) THEN
            CALL WRITE_I_C(ADSKY_PXFEM,NPLYXFE+1 )
C lecture directe de PROCNE
            CALL WRITE_I_C(PROCNE_PXFEM,LCNEPXFEM)
            CALL WRITE_I_C(IADC_PXFEM,4*EPLYXFE)
        ENDIF
C
C  xfem for layered shell (cracks)
C
        IF(ICRACK3D > 0) THEN
          CALL WRITE_I_C(ADSKY_CRKXFEM,NCRKXFE+1)
          CALL WRITE_I_C(CNE_CRKXFEM,LCNECRKXFEM)
          CALL WRITE_I_C(PROCNE_CRKXFEM,LCNECRKXFEM)
          CALL WRITE_I_C(IADC_CRKXFEM,4*ECRKXFEC+3*ECRKXFETG)
          CALL WRITE_I_C(CRKNODIAD,LCNECRKXFEM)
        ENDIF
      ENDIF
      ! ---------------
      ! ebcs option : adress of FSKY array
      IF(EBCS_TAB%nebcs_nrf>0) THEN
        DO I=1,EBCS_TAB%nebcs
                IF(EBCS_TAB%my_typ(I)==10) THEN
                    MY_SIZE = EBCS_TAB%tab(I)%poly%nb_elem
                    CALL WRITE_I_C(EBCS_PARITHON(I)%ELEM_ADRESS,4*MY_SIZE)
                ENDIF
            ENDDO
       ENDIF
       ! ---------------

      CALL COMPRESS_I_NNZ(IGEO,LEN_G)
      CALL COMPRESS_I_NNZ(IPM,LEN_M)

      CALL WRITE_I_C(ICONTACT ,SICONTACT)
      CALL WRITE_I_C(IPART_STATE,LEN_S)

      IF(NADMESH /= 0)THEN
        CALL WRITE_I_C(SH4TREE,KSH4TREE*NUMELC)
        CALL WRITE_I_C(SH3TREE,KSH3TREE*NUMELTG)
        CALL WRITE_I_C(IPADMESH,KIPADMESH*NPART)
        LEN=ABS(LSH4TRIM)
        CALL WRITE_I_C(SH4TRIM,LEN)
        LEN=ABS(LSH3TRIM)
        CALL WRITE_I_C(SH3TRIM,LEN)
      END IF
C--------------------------------------
C ecriture tableaux additionnels frontieres interfaces (partie entiere)
C--------------------------------------
      CALL SPMD_SAVEFI(IPARI,1,INTBUF_TAB,SENSOR_TAB)
C--------------------------------------
C ecriture tableaux additionnels frontieres sph (partie entiere)
C--------------------------------------
      CALL SPMD_SAVESPH(1)
C--------------------------------------
C structures int 21
C--------------------------------------
      IF(NINTSTAMP /= 0)THEN
        CALL INTSTAMP_WRESTI(INTSTAMP)
      END IF
C--------------------------------------
C  Interface friction buffer (integer )
C--------------------------------------
      IF(NINTERFRIC /= 0)THEN
        CALL INTFRIC_WRESTI(INTBUF_FRIC_TAB)
      END IF
C--------------------------------------
C structure tables (partie entiere)
C--------------------------------------
      IF(NTABLE /= 0)THEN
        CALL TABLE_WRESTI(TABLE, LTABLE)
      END IF
C--------------------------------------
      CALL WRITE_I_C(ITHVAR,SITHVAR)
C--------------------------------------
      IF (NLOADC>0)THEN
        CALL WRITE_I_C(ICFIELD,SIZFIELD*NLOADC)
        CALL WRITE_I_C(LCFIELD,SLCFIELD)
      ENDIF
      IF (NLOADP>0)THEN
        CALL WRITE_I_C(ILOADP,SIZLOADP*NLOADP)
        CALL WRITE_I_C(LLOADP,SLLOADP)
      ENDIF
      IF (NLOADP_B>0)THEN
        CALL PBLAST_WRITE_ENGINE(PBLAST_TAB)
      ENDIF
C--------------------------------------
      IF(ICRACK3D > 0)THEN
        CALL WRITE_I_C(IBORDNODE,SIBORDNODE)
        CALL WRITE_I_C(IEDGESH,SIEDGESH)
        CALL WRITE_I_C(NODEDGE,2*NUMEDGES)
        CALL WRITE_I_C(IEDGE,NUMEDGES)
        CALL WRITE_I_C(NODGLOBXFE,SNODGLOBXFE)
      END IF
C--------------------------------------
      IF(NUMELIG3D > 0)THEN
        CALL WRITE_I_C(KXIG3D,SKXIG3D)

        CALL WRITE_I_C(IXIG3D,SIXIG3D)
      ENDIF
C----------------------------------
      CALL WRITE_I_C(TAG_SKINS6,NUMELS)
C--------------------------------------
      IF(NBCSCYC > 0)THEN
        CALL WRITE_I_C(IBCSCYC,SIBCSCYC)
        CALL WRITE_I_C(LBCSCYC,SLBCSCYC)
      ENDIF
C--------------------------------------
C     ECRITURE DES VARIABLES REELLES
C--------------------------------------
      CALL WRITE_DB(TABVREA, LVARREA)
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
         CALL WRITE_DB(X,SX)

         CALL WRITE_DB(D,SD)

         CALL WRITE_DB(V,SV)

         CALL WRITE_DB(VR,SVR)

         CALL WRITE_DB(DR,SDR)

         CALL WRITE_DB(THKE,STHKE)

         CALL WRITE_DB(DAMPR,SDAMPR)

         CALL WRITE_DB(DAMP,SDAMP)

         CALL WRITE_DB(MS,SMS)

         IF (N2D >0) THEN
           CALL WRITE_DB(MS_2D,NUMNOD)
         ENDIF

         CALL WRITE_DB(IN,SIN)

         CALL WRITE_DB(TF,STF)

         CALL WRITE_DB(PM,SPM)

         CALL WRITE_MATPARAM(MATPARAM_TAB,NUMMAT)

         CALL WRITE_ELGROUP_PARAM(GROUP_PARAM_TAB,NGROUP)

         CALL WRITE_DB(SKEW,SSKEW)

         CALL WRITE_DB(XFRAME,SXFRAME)

         CALL COMPRESS_R_NNZ(GEO,SGEO)

         CALL WRITE_DB(EANI,SEANI)

         CALL WRITE_DB(BUFMAT,SBUFMAT)

         CALL WRITE_DB(BUFGEO,SBUFGEO)

         CALL WRITE_DB(BUFSF,SBUFSF)

         CALL WRITE_DB(RBMPC,SRBMPC)

         CALL WRITE_DB(GJBUFR,SGJBUFR)

         CALL WRITE_DB(W,SW)

         CALL WRITE_DB(VEUL,SVEUL)

         IF (MULTI_FVM%IS_USED) THEN
            CALL WRITE_DB(MULTI_FVM%PRES_SHIFT, 1)
            IF (N2D == 0) THEN
               CALL WRITE_DB(MULTI_FVM%VEL(1, :), NUMELS)
               CALL WRITE_DB(MULTI_FVM%VEL(2, :), NUMELS)
               CALL WRITE_DB(MULTI_FVM%VEL(3, :), NUMELS)
            ELSE
               CALL WRITE_DB(MULTI_FVM%VEL(1, :), NUMELQ + NUMELTG)
               CALL WRITE_DB(MULTI_FVM%VEL(2, :), NUMELQ + NUMELTG)
               CALL WRITE_DB(MULTI_FVM%VEL(3, :), NUMELQ + NUMELTG)
            ENDIF
         ENDIF

         CALL WRITE_DB(FILL,SFILL)

         CALL WRITE_DB(DFILL,SDFILL)

         CALL WRITE_DB(ALPH,SALPH)

         CALL WRITE_DB(WB,SWB)

         CALL WRITE_DB(DSAVE,SDSAVE)

         CALL WRITE_DB(DRSAVE,SDRSAVE)

         CALL WRITE_DB(ASAVE,SASAVE)

         CALL WRITE_DB(ARSAVE,SARSAVE)

         CALL WRITE_DB(STSAVE,SSTSAVE)

         CALL WRITE_DB(STRSAVE,SSTRSAVE)

         CALL WRITE_DB(FSAVSAV,SFSAVSAV)

         CALL WRITE_DB(SPBUF,SSPBUF)

         CALL WRITE_DB(VSPHIO,SVSPHIO)

         CALL WRITE_DB(SPHVELN,SSPHVELN)

         CALL WRITE_DB(MSNF,SMSNF)

         CALL WRITE_DB(FORC,SFORC)

         CALL WRITE_DB(VEL,SVEL)

         CALL WRITE_DB(FSAV,SFSAV)

         CALL WRITE_DB(FBSAV6,12*6*SFSAV6)

         CALL WRITE_I_C(TABSENSOR,STABSEN)

         CALL WRITE_I_C(TAB_UMP,7*TAILLE)

         CALL WRITE_I_C(POIN_UMP,NUMMAT)

	        CALL WRITE_DB(TAB_MAT,STAB_MAT)

         CALL WRITE_DB(FZERO,SFZERO)

         CALL WRITE_DB(XLAS,SXLAS)

         CALL WRITE_DB(ACCELM,SACCELM)

         CALL WRITE_DB(GAUGE,LLGAUGE*NBGAUGE)

         IF(NSENSOR > 0)THEN
            CALL WRITE_SENSOR_TAB(SENSOR_TAB,NSENSOR)
         ENDIF

         CALL WRITE_DB(FBVEL,SFBVEL)

         CALL WRITE_DB(FRBE3,SFRBE3)

         CALL WRITE_DB(FACTIV,LRACTIV*NACTIV)

         CALL WRITE_DB(GRAV,SGRAV)

         CALL WRITE_DB(FR_WAVE,SFR_WAVE)

         CALL W_FAILWAVE(FAILWAVE)

         CALL WRITE_NLOC_STRUCT(NLOC_DMG)

         CALL WRITE_DB(PARTS0,SPARTS0)

         CALL WRITE_DB(ELBUF,SELBUF)

         FLAG_XFEM = 0
         CALL W_ELBUF_STR(IPARG,ELBUF_TAB,FLAG_XFEM)

C  if xfem
         IF (ICRACK3D > 0) THEN
           FLAG_XFEM = 1
           DO IXEL=1,NXEL
             CALL W_ELBUF_STR(IPARG,XFEM_TAB(1:NGROUP,IXEL),FLAG_XFEM)
           ENDDO
         ENDIF

         CALL W_CLUSTER(CLUSTER)

         CALL WRITE_DB(RWBUF,SRWBUF)

         CALL WRITE_DB(RWSAV,SRWSAV)

         CALL WRITE_DB(RBY,SRBY)

         CALL WRITE_DB(RBYL,SRBYL)

         CALL WRITE_DB(RIVET,SRIVET)

         CALL WRITE_DB(SECBUF,SSECBUF)

         CALL WRITE_DB(VOLMON,SVOLMON)

         CALL WRITE_DB(LAMBDA,SLAMBDA)

         CALL WRITE_DB(RCONX,SRCONX)

         CALL WRITE_DB(RCONTACT,SRCONTACT)
         CALL WRITE_DB(ACONTACT,SRCONTACT)
         CALL WRITE_DB(PCONTACT,SRCONTACT)
C--------------------------------------
C ecriture tableaux additionnels frontieres interfaces (partie reelle)
C--------------------------------------
      CALL SPMD_SAVEFI(IPARI,2,INTBUF_TAB,SENSOR_TAB)
C--------------------------------------
C ecriture tableaux additionnels frontieres sph (partie reelle)
C--------------------------------------
      CALL SPMD_SAVESPH(2)
C--------------------------------------
C     ECRITURES FACULTATIVES
C--------------------------------------
      IF (IRESMD==0.AND.NPSAV>=25) THEN
C   on ramene PARTSAV sur le proc et on met a zero les autres
          IF(NSPMD > 1)
     .      CALL SPMD_GLOB_DSUM9(PARTSAV,NPSAV*NPART)
          IF (ISPMD/=0)THEN
            DO M=1,NPSAV*NPART
                 PARTSAV(M) = ZERO
            ENDDO
          ENDIF
          CALL WRITE_DB(PARTSAV,NPSAV*NPART)
      ENDIF
      IF(NNOISE>0)THEN
        CALL WRITE_I_C (IAF(IF01),2*NNOISE+10)
        CALL WRITE_DB(AF(MF01),6*NCNOIS*NNOISE+1)
      ENDIF
C-----
C  Save A, AR to Restart RAD2MD
      IF (IRESMD==1) THEN
        CALL WRITE_DB(A,3*NUMNOD)
        CALL WRITE_DB(AR,3*NUMNOD)
C    Save PARTSAV.
        CALL WRITE_DB(PARTSAV,NPSAV*NPART)
      ENDIF
C
      IF (IRESP == 1) THEN
        CALL WRITE_DPDB(XDP,3*NUMNOD)
        CALL WRITE_DPDB(DDP,3*NUMNOD)
      ENDIF
C besoin pour les check restart
C on a besoin car les restart sont les memes des check restart
        MY_ILEN = 4*NRLINK+NRLINK*(NSPMD+2)
     .           +LLLINK+LLLINK*MIN(1,IPARIT)
     .           +NSFLSW+8*NTFLSW+44*NCUTS !+LLINAL
        CALL WRITE_I_C (MY_ILEN,1)
        MY_RLEN = 9*NSFLSW+7*NCUTS
        CALL WRITE_I_C (MY_RLEN,1)
        CALL WRITE_I_C (ILINK,4*NRLINK)
        CALL WRITE_I_C (FR_RL,NRLINK*(NSPMD+2))
        CALL WRITE_I_C (LLINK,LLLINK)
        CALL WRITE_I_C (IADRL,LLLINK*MIN(1,IPARIT))
        !CALL WRITE_I_C (LINALE,LLINAL)
C       section
	       CALL WRITE_I_C(NEFLSW,NSFLSW)
	       CALL WRITE_I_C(NNFLSW,8*NTFLSW)
	       CALL WRITE_I_C(ICUT,44*NCUTS)
	       CALL WRITE_DB (CRFLSW,9*NSFLSW)
	       CALL WRITE_DB (XCUT,7*NCUTS)
C--------------------------------------
        IF(NADMESH /= 0 .OR. IREST_MSELT /= 0)THEN
          CALL WRITE_DB(MSC,NUMELC)
          CALL WRITE_DB(INC,NUMELC)
          CALL WRITE_DB(MSTG,NUMELTG)
          CALL WRITE_DB(INTG,NUMELTG)
          CALL WRITE_DB(PTG,3*NUMELTG)
        END IF

        IF(IREST_MSELT /= 0)THEN
          CALL WRITE_DB(MSSA,NUMELS)
          CALL WRITE_DB(MSTR,NUMELT)
          CALL WRITE_DB(MSP,NUMELP)
          CALL WRITE_DB(MSRT,NUMELR)
        END IF

        IF(NADMESH /= 0)THEN
          CALL WRITE_DB(PADMESH,KPADMESH*NPART)
          IF(ITHERM_FE > 0)THEN
            CALL WRITE_DB(MCPC,NUMELC)
            CALL WRITE_DB(MCPTG,NUMELTG)
          END IF
        END IF
C pinching shell
        IF(NPINCH > 0) THEN
          CALL WRITE_DB(PINCH_DATA%VPINCH,3*NPINCH)
          CALL WRITE_DB(PINCH_DATA%DPINCH,3*NPINCH)
          CALL WRITE_DB(PINCH_DATA%XPINCH,3*NPINCH)
          CALL WRITE_DB(PINCH_DATA%MSPINCH,NPINCH)
        ENDIF
C--------------------------------------
        IF(ISTATCND /= 0)THEN
          CALL WRITE_DB(MSCND,NUMNOD)
          CALL WRITE_DB(INCND,NUMNOD)
        ENDIF
C--------------------------------------
C structures int 21
C--------------------------------------
        IF(NINTSTAMP /= 0)THEN
          CALL INTSTAMP_WRESTR(INTSTAMP)
        END IF

        IF(H3D_DATA%N_SCAL_SKID > 0) THEN
         IF(NINTSTAMP/=0) THEN
           CALL WRITE_DB(PSKIDS, H3D_DATA%N_SCAL_SKID*NUMNODG)
         ELSE
           CALL WRITE_DB(PSKIDS, H3D_DATA%N_SCAL_SKID*NUMNOD)
         ENDIF
       ENDIF

        IF(IFCONTMAX >0)THEN
          IF(NINTSTAMP/=0.AND.NSPMD > 1 ) THEN
             IF(ISPMD == 0) CALL WRITE_DB(FCONT_MAX, 3*NUMNODG)
          ELSE
             CALL WRITE_DB(FCONT_MAX, 3*NUMNOD)
         ENDIF
        ENDIF

        IF(IFCONTPMAX >0)THEN
          CALL WRITE_DB(FNCONT_MAX, 3*SPCONT_MAX)
          CALL WRITE_DB(FTCONT_MAX, 3*SPCONT_MAX)
        ENDIF

        IF(IFCONT2MAX >0)THEN
          CALL WRITE_DB(FCONT2_MAX, 3*NUMNOD)
        ENDIF
        IF(IFCONTP2MAX >0)THEN
          CALL WRITE_DB(FNCONT2_MAX, 3*NUMNOD)
          CALL WRITE_DB(FTCONT2_MAX, 3*NUMNOD)
        ENDIF

        IF(IFCONT2MIN >0)THEN
          CALL WRITE_DB(FCONT2_MIN, 3*NUMNOD)
        ENDIF
        IF(IFCONTP2MIN >0)THEN
          CALL WRITE_DB(FNCONT2_MIN, 3*NUMNOD)
          CALL WRITE_DB(FTCONT2_MIN, 3*NUMNOD)
        ENDIF
C--------Frictional energy output-------
      IF(S_EFRICINT >0)THEN
         IF(NINEFRIC > 0) CALL WRITE_DB(EFRIC, NINEFRIC*NUMNOD)
         IF(NINEFRIC_STAMP > 0) CALL WRITE_DB(EFRIC_STAMP, NINEFRIC_STAMP*NUMNODG)
      ENDIF
      IF(S_EFRIC >0)THEN
         CALL WRITE_DB(EFRICG, NUMNOD)
         IF(NINTSTAMP/=0) CALL WRITE_DB(EFRICG_STAMP, NUMNODG)
      ENDIF
C--------------------------------------
C  Interface friction buffer (integer )
C--------------------------------------
      IF(NINTERFRIC /= 0)THEN
        CALL INTFRIC_WRESTR(INTBUF_FRIC_TAB)
      END IF
C--------------------------------------
        CALL WRITE_DB(MS0,NUMNOD)
        CALL WRITE_DB(IN0,SIN)
        IF(IDTMINS==1)THEN
          CALL WRITE_DB(ADMSMS,NUMNOD)
        ELSEIF(IDTMINS==2)THEN
          CALL WRITE_DB(DMELC ,NUMELC)
          CALL WRITE_DB(DMELTG,NUMELTG)
          CALL WRITE_DB(DMELS ,NUMELS)
          CALL WRITE_DB(DMELTR,NUMELT)
          CALL WRITE_DB(DMELP ,NUMELP)
          CALL WRITE_DB(DMELRT,NUMELR)
        END IF
        IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
          CALL WRITE_DB(RES_SMS,3*NUMNOD)
        END IF
c
        IF(IDTMINS==2.OR.IDTMINS_INT/=0) THEN
Cfor TYPE20
          CALL WRITE_DB(DIAG_SMS ,NUMNOD)
Cfor TYPE2
          CALL WRITE_DB(DMINT2 ,4*I2NSN25)
        END IF
C
        IF (ISMS_SELEC /= 0) THEN
          CALL WRITE_I_C(NATIV0_SMS,NUMNOD)
        ENDIF
C--------------------------------------
C structure tables (partie reelle)
C--------------------------------------
      IF(NTABLE /= 0)THEN
        CALL TABLE_WRESTR(TABLE, LTABLE)
      END IF
C--------------------------------------
C    MDS Parameters
C--------------------------------------
#ifdef DNC
      CALL ENG_WRT_MDS()
#endif
C--------------------------------------
C ALE LINKS
!      print *,"wrrestp.F: WRITE_I_C, SLINALE=", SLINALE
      IF(SLINALE > 0)THEN
        CALL WRITE_I_C(LINALE,SLINALE)
      END IF
C
c        CALL WRITE_DB(RTHBUF,SRTHBUF)
C--------------------------------------
C--------------------------------------
C 
      CALL WRITE_I_C(LSTOPSENS,NSTOPSENS)
      CALL WRITE_I_C(LSTATSENS,NSTATSENS)
      CALL WRITE_I_C(LOUTPSENS,NOUTPSENS)
      CALL WRITE_I_C(LANISENS,IANISENS)
C--------------------------------------
C FXBODY (moved from resol.F)
C--------------------------------------
      IF (NFXBODY>0) CALL FXBWREST(ICH)
C--------------------------------------
C SEATBELTS - SLIPRING / RETRACTOR
C--------------------------------------
      DO I = 1, NSLIPRING
        CALL WRITE_I_C(SLIPRING(I)%ID, 1)
        CALL WRITE_I_C(SLIPRING(I)%IDG, 1)
        CALL WRITE_I_C(SLIPRING(I)%NFRAM, 1)
        CALL WRITE_I_C(SLIPRING(I)%IFUNC, 4)
        CALL WRITE_I_C(SLIPRING(I)%SENSID, 1)
        CALL WRITE_I_C(SLIPRING(I)%FL_FLAG, 1)
        CALL WRITE_I_C(SLIPRING(I)%RBODY, 1)
C
        CALL WRITE_DB(SLIPRING(I)%DC, 1)
        CALL WRITE_DB(SLIPRING(I)%A, 1)
        CALL WRITE_DB(SLIPRING(I)%FRIC, 1)
        CALL WRITE_DB(SLIPRING(I)%FAC_D, 3)
        CALL WRITE_DB(SLIPRING(I)%FRICS, 1)
        CALL WRITE_DB(SLIPRING(I)%FAC_S, 3)
C
        DO J=1,SLIPRING(I)%NFRAM
C
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%UPDATE, 1)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%ANCHOR_NODE, 1)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%NODE, 3)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%NODE_NEXT, 3)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%NODE2_PREV, 1)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%N_REMOTE_PROC, 1)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%ORIENTATION_NODE, 1)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%STRAND_DIRECTION, 2)
          CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%LOCKED, 1)
C
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%VECTOR, 6)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE, 1)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%MATERIAL_FLOW, 1)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%DFS, 1)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%RESIDUAL_LENGTH, 2)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%CURRENT_LENGTH, 2)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%RINGSLIP, 1)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%BETA, 1)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%SLIP_FORCE, 3)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%PREV_REF_LENGTH, 1)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%INTVAR_STR1, 8)
          CALL WRITE_DB(SLIPRING(I)%FRAM(J)%INTVAR_STR2, 8)
C
        ENDDO
C
      ENDDO
C
      DO I = 1, NRETRACTOR
        CALL WRITE_I_C(RETRACTOR(I)%ID, 1)
        CALL WRITE_I_C(RETRACTOR(I)%IDG, 1)
        CALL WRITE_I_C(RETRACTOR(I)%UPDATE, 1)
        CALL WRITE_I_C(RETRACTOR(I)%ANCHOR_NODE, 1)
        CALL WRITE_I_C(RETRACTOR(I)%NODE, 2)
        CALL WRITE_I_C(RETRACTOR(I)%NODE_NEXT, 2)
        CALL WRITE_I_C(RETRACTOR(I)%STRAND_DIRECTION, 1)
        CALL WRITE_I_C(RETRACTOR(I)%IFUNC, 3)
        CALL WRITE_I_C(RETRACTOR(I)%ISENS, 2)
        CALL WRITE_I_C(RETRACTOR(I)%TENS_TYP, 1)
        CALL WRITE_I_C(RETRACTOR(I)%LOCKED, 1)
        CALL WRITE_I_C(RETRACTOR(I)%PRETENS_ACTIV, 1)
        CALL WRITE_I_C(RETRACTOR(I)%INACTI_NNOD, 1)
        CALL WRITE_I_C(RETRACTOR(I)%INACTI_NNOD_MAX, 1)
        CALL WRITE_I_C(RETRACTOR(I)%INACTI_NODE, RETRACTOR(I)%INACTI_NNOD)
        CALL WRITE_I_C(RETRACTOR(I)%N_REMOTE_PROC, 1)
        CALL WRITE_DB(RETRACTOR(I)%VECTOR, 3)
        CALL WRITE_DB(RETRACTOR(I)%ELEMENT_SIZE, 1)
        CALL WRITE_DB(RETRACTOR(I)%FORCE, 1)
        CALL WRITE_DB(RETRACTOR(I)%MATERIAL_FLOW, 1)
        CALL WRITE_DB(RETRACTOR(I)%RESIDUAL_LENGTH, 1)
        CALL WRITE_DB(RETRACTOR(I)%FAC, 4)
        CALL WRITE_DB(RETRACTOR(I)%PULLOUT, 1)
        CALL WRITE_DB(RETRACTOR(I)%UNLOCK_FORCE, 1)
        CALL WRITE_DB(RETRACTOR(I)%LOCK_PULL, 1)
        CALL WRITE_DB(RETRACTOR(I)%LOCK_OFFSET, 1)
        CALL WRITE_DB(RETRACTOR(I)%LOCK_YIELD_FORCE, 1)
        CALL WRITE_DB(RETRACTOR(I)%RINGSLIP, 1)
        CALL WRITE_DB(RETRACTOR(I)%PRETENS_TIME, 1)
        CALL WRITE_DB(RETRACTOR(I)%PRETENS_PULL, 1)
        CALL WRITE_DB(RETRACTOR(I)%PRETENS_PULLMAX, 1)
        CALL WRITE_DB(RETRACTOR(I)%RET_FORCE, 1)
      ENDDO
C
      IF (N_ANCHOR_REMOTE > 0) THEN
        CALL WRITE_I_C(ANCHOR_REMOTE%ADD_PROC, NSPMD)
        CALL WRITE_I_C(ANCHOR_REMOTE%NODE, N_ANCHOR_REMOTE)
      ENDIF
C
      IF (N_ANCHOR_REMOTE_SEND > 0) THEN
        CALL WRITE_I_C(ANCHOR_REMOTE_SEND%ADD_PROC, NSPMD)
        CALL WRITE_I_C(ANCHOR_REMOTE_SEND%NODE, N_ANCHOR_REMOTE_SEND)
      ENDIF
C
      IF ((NSLIPRING_G + NRETRACTOR_G >0).AND.(ISPMD == 0)) THEN
        CALL WRITE_I_C(NSEATBELT_TH_PROC, 1)
        IF ((NSPMD > 1).AND.(NSEATBELT_TH_PROC > 0)) THEN
          DO I=1,NSEATBELT_TH_PROC
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%ID_PROC, 1)
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%ADD_PROC, 1)
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%NSLIPRING, 1)
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%NRETRACTOR, 1)
          ENDDO
        ENDIF
      ENDIF
C
C--------------------------------------------
C /H3D/?/TMAX part noda
C--------------------------------------------
      CALL WRITE_DB(TM_DIS,LMAX_DIS*NUMNOD)
      CALL WRITE_DB(TM_VEL,LMAX_VEL*NUMNOD)
      CALL WRITE_DB(TM_NSIG1,LMAX_NSIG*NUMNOD)
      CALL WRITE_DB(TM_NSIG3,LMAX_NSIG*NUMNOD)
      CALL WRITE_DB(TM_NSTRA1,LMAX_NSTRA*NUMNOD)
      CALL WRITE_DB(TM_NSTRA3,LMAX_NSTRA*NUMNOD)
C--------------------------------------
      IF(ISPMD==0)THEN
        WRITE (IOUT,1000)  FILNAMG(1:LENG)
        WRITE (ISTDO,1050) FILNAMG(1:LENG)
      ENDIF
C--------------------------------------
 1000 FORMAT (/4X,' RESTART FILES:',1X,A,' WRITTEN'/
     .         4X,' -------------'/)
 1050 FORMAT (4X,' RESTART FILES:',1X,A,' WRITTEN')
C-----------
      RETURN
      END
