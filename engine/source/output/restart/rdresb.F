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
Chd|  RDRESB                        source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        DECOMPRESS_I_NNZ              ../common_source/comm/decompress_nnz.F
Chd|        DECOMPRESS_R_NNZ              ../common_source/comm/decompress_nnz.F
Chd|        EIGRREST                      source/output/restart/rdresb.F
Chd|        ELBUF_INI                     source/elements/elbuf/elbuf_ini.F
Chd|        FILLXDP                       source/output/restart/fillxdp.F
Chd|        FXBRESB                       source/output/restart/rdresb.F
Chd|        GROUP_INI                     source/groups/group_ini.F     
Chd|        INTBUF_INI                    ../common_source/interf/intbuf_ini.F
Chd|        INTFRIC_RRESTI                share/modules/intbuf_fric_mod.F
Chd|        INTFRIC_RRESTR                share/modules/intbuf_fric_mod.F
Chd|        INTSTAMP_RRESTI               share/modules/intstamp_mod.F  
Chd|        INTSTAMP_RRESTR               share/modules/intstamp_mod.F  
Chd|        LINE_INI                      source/groups/line_ini.F      
Chd|        PBLAST_INIT_TABLES            ../common_source/modules/loads/pblast_mod.F
Chd|        PBLAST_LOAD                   ../common_source/modules/loads/pblast_mod.F
Chd|        RDCOMR                        source/output/restart/rdcomm.F
Chd|        READ_CLUSTER                  source/output/cluster/read_cluster.F
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_DPDB                     source/output/tools/read_db.F 
Chd|        READ_ELGROUP_PARAM            source/output/restart/read_elgroup_param.F
Chd|        READ_FAILWAVE                 source/output/restart/read_failwave.F
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        READ_JOINT                    source/output/restart/read_joint.F
Chd|        READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|        READ_NLOC_STRUCT              source/output/restart/read_nloc_struct.F
Chd|        READ_PCYL                     source/output/restart/read_pcyl.F
Chd|        READ_SENSORS                  source/output/restart/read_sensors.F
Chd|        READ_TH_RESTART               source/output/th/read_th_restart.F
Chd|        SPMD_INISPH                   source/mpi/sph/spmd_sptool.F  
Chd|        SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SUBSET_INI                    source/groups/subset_ini.F    
Chd|        SURF_INI                      source/groups/surf_ini.F      
Chd|        TABLE_RRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_RRESTR                  source/tools/curve/table_tools.F
Chd|        THSURF_READ_RESTART           ../common_source/modules/interfaces/th_surf_mod.F
Chd|        USERWI_READ                   source/user_interface/userwindow_interface_routines.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFFRIC_MOD                share/modules/restart_mod.F   
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTBUF_MOD                    share/modules/restart_mod.F   
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/loads/pblast_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PLOAD_CYL_MOD                 ../common_source/modules/loads/pload_cyl_mod.F
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        SPH_MOD                       share/modules/sph_mod.F       
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        TABLE_GLOB_MOD                share/modules/table_glob_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE RDRESB( AF               ,IAF      ,LVARREA      ,XDP             ,IRUNN      ,
     .                   MULTI_FVM        ,H3D_DATA ,MATPARAM_TAB ,GROUP_PARAM_TAB ,PINCH_DATA , 
     .                   ALE_CONNECTIVITY ,T_MONVOL ,SENSORS      ,EBCS_TAB        ,DYNAIN_DATA,
     .                   USER_WINDOWS     ,OUTPUT   ,INTERFACES   ,LOADS           )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUF_MOD
      USE RESTMOD
      USE INTSTAMP_GLOB_MOD
      USE TABLE_GLOB_MOD
      USE MESSAGE_MOD
      USE SPH_MOD
      USE CLUSTER_MOD
      USE ECND_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUFFRIC_MOD
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD
      USE PINCHTYPE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MONVOL_STRUCT_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE SEATBELT_MOD
      USE PBLAST_MOD
      USE OUTMAX_MOD
      USE OUTPUTS_MOD
      USE PLOAD_CYL_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
      USE ALE_MOD
      USE OUTPUT_MOD
      USE INTERFACES_MOD
      USE SENSOR_MOD
      USE LOADS_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "couple_c.inc"
#include      "intstamp_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "scr06_c.inc"
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
#include      "sphcom.inc"
#include      "spmd_c.inc"
#include      "tabsiz_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAF(*),LVARREA,LEN_G,LEN_M, IRUNN
      my_real AF(*)
      DOUBLE PRECISION XDP(*)
      TYPE(MULTI_FVM_STRUCT)                               :: MULTI_FVM
      TYPE(H3D_DATABASE)                                   :: H3D_DATA
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT)          :: MATPARAM_TAB
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP)          :: GROUP_PARAM_TAB
      TYPE(PINCH)                                          :: PINCH_DATA
      TYPE(t_ale_connectivity), INTENT(INOUT)              :: ALE_CONNECTIVITY
      TYPE(MONVOL_STRUCT_),DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
      TYPE(t_ebcs_tab), INTENT(INOUT)                      :: EBCS_TAB
      TYPE(DYNAIN_DATABASE) ,INTENT(INOUT)                 :: DYNAIN_DATA
      TYPE(USER_WINDOWS_)   ,INTENT(INOUT)                 :: USER_WINDOWS
      TYPE(OUTPUT_)         ,INTENT(INOUT)                 :: OUTPUT !< output structure
      TYPE(INTERFACES_)     ,INTENT(INOUT)                 :: INTERFACES
      TYPE(SENSORS_)        ,INTENT(INOUT)                 :: SENSORS
      TYPE(LOADS_)          ,INTENT(INOUT)                 :: LOADS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IDUMM(MVSIZ)
      INTEGER  I,J,LMXVREA, LEN,MY_ILEN,MY_RLEN,ISTAT,II,NS_DIFF
      PARAMETER (LMXVREA=1000+30*MAXLAW+30)
      my_real TABVREA(LMXVREA)
      INTEGER, DIMENSION(:), ALLOCATABLE::IBID
      my_real, DIMENSION(:), ALLOCATABLE::RBID
      DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE :: XDPDUM
      INTEGER, DIMENSION(NVOLU) :: NTRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: INTEGER_DATA
      INTEGER :: INT1, INT2, INT3, INT0, ITMP
      INTEGER :: MY_SIZE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,LMXVREA
        TABVREA(I)=ZERO
      ENDDO
      IF(LVARREA>LMXVREA)THEN
        CALL ANCMSG(MSGID=33,ANMODE=ANINFO,I1=LVARREA,I2=LMXVREA)
        CALL ARRET(2)
      ENDIF
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
C fichier de restart au format binaire ieee
      LEN_G = NPROPGI*NUMGEO
      LEN_M = NPROPMI*NUMMAT
C
        CALL CUR_FIL_C(0)

        CALL READ_I_C(NODGLOB,SNODGLOB)

        CALL READ_I_C(MAIN_PROC,SWEIGHT)

        CALL READ_I_C(WEIGHT,SWEIGHT)

        CALL READ_I_C(ICODE,SICODE)

        CALL READ_I_C(LSHSUB,SLSHSUB)

        CALL READ_I_C(ISKEW,SISKEW)

        CALL READ_I_C(ISKWN,SISKWN)

        CALL READ_I_C(IFRAME,SIFRAME)

        CALL READ_I_C(NETH,SNETH)

        CALL READ_I_C(IBCSLAG,SIBCSLAG)

        CALL READ_I_C(IPART,SIPART)

        CALL READ_I_C(NOM_OPT,SNOM_OPT)

        CALL READ_I_C(NPC,SNPC)

        CALL READ_I_C(IXTG,SIXTG)

        CALL READ_I_C(IXTG1,SIXTG1)

        CALL READ_I_C(IXS,SIXS)

        CALL READ_I_C(IXQ,SIXQ)

        CALL READ_I_C(IXC,SIXC)

        CALL READ_I_C(IXT,SIXT)

        CALL READ_I_C(IXP,SIXP)

        CALL READ_I_C(IXR,SIXR)

        CALL READ_I_C(ITAB,SITAB)
C       WRITE(6,*) "ITAB = ",ITAB(1:SITAB)
        CALL READ_I_C(ITABM1,SITABM1)

        CALL READ_I_C(GJBUFI,SGJBUFI)

        CALL READ_I_C(ALE_CONNECTIVITY%NALE,ALE%GLOBAL%SNALE)

        IF (IALE + IEULER + IALELAG + ITHERM /= 0) THEN
           CALL READ_I_C(ITMP, 1)
           ALLOCATE(ALE_CONNECTIVITY%ee_connect%iad_connect(ITMP))
           CALL READ_I_C(ALE_CONNECTIVITY%ee_connect%iad_connect, ITMP)

           CALL READ_I_C(ITMP, 1)
           ALLOCATE(ALE_CONNECTIVITY%ee_connect%connected(ITMP))
           CALL READ_I_C(ALE_CONNECTIVITY%ee_connect%connected, ITMP)

           CALL READ_I_C(ITMP, 1)
           ALLOCATE(ALE_CONNECTIVITY%ee_connect%type(ITMP))
           CALL READ_I_C(ALE_CONNECTIVITY%ee_connect%type, ITMP)

           CALL READ_I_C(ITMP, 1)
           ALLOCATE(ALE_CONNECTIVITY%ee_connect%iface2(ITMP))
           CALL READ_I_C(ALE_CONNECTIVITY%ee_connect%iface2, ITMP)
        ENDIF

        CALL READ_I_C(ID_GLOBAL_VOIS,ALE%GLOBAL%SIELVS)

        CALL READ_I_C(FACE_VOIS,ALE%GLOBAL%SIELVS)

        IF(ALE_CONNECTIVITY%has_ne_connect) THEN
           CALL READ_I_C(NE_NERCVOIS, NSPMD + 1)
           ALLOCATE(NE_LERCVOIS(NE_NERCVOIS(NSPMD+1)))
           CALL READ_I_C(NE_LERCVOIS, NE_NERCVOIS(NSPMD+1))
           CALL READ_I_C(NE_NESDVOIS, NSPMD + 1)
           ALLOCATE(NE_LESDVOIS(NE_NESDVOIS(NSPMD+1)))
           CALL READ_I_C(NE_LESDVOIS, NE_NESDVOIS(NSPMD+1))
        ENDIF
        IF (IALE > 0 .AND. ALE%GRID%NWALE_RST >= 0) THEN
         IF(ALE_CONNECTIVITY%has_nn_connect)THEN
           ALLOCATE(ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NUMNOD + 1))
           CALL READ_I_C(ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT, NUMNOD + 1)
           ALLOCATE(ALE_CONNECTIVITY%NN_CONNECT%CONNECTED(ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NUMNOD + 1)))
           CALL READ_I_C(ALE_CONNECTIVITY%NN_CONNECT%CONNECTED, ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NUMNOD + 1))
         ENDIF
        ENDIF
        IF(ALE_CONNECTIVITY%has_ne_connect)THEN
          ALLOCATE(ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NUMNOD + 1))
          CALL READ_I_C(ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT, NUMNOD + 1)
          ALLOCATE(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NUMNOD + 1)))
          CALL READ_I_C(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED, ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NUMNOD + 1))
        ENDIF

        CALL READ_I_C(NS_DIFF, 1)
        MULTI_FVM%NS_DIFF = .FALSE.
        IF (NS_DIFF == 1) THEN
           MULTI_FVM%NS_DIFF = .TRUE.
           ALE_CONNECTIVITY%HAS_IDGLOB = .TRUE.
           IF (.NOT. ALLOCATED(ALE_CONNECTIVITY%IDGLOB%ID)) THEN
              ALLOCATE(ALE_CONNECTIVITY%IDGLOB%ID(NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS))
           ENDIF
           IF (.NOT. ALLOCATED(ALE_CONNECTIVITY%IDGLOB%UID)) THEN
              ALLOCATE(ALE_CONNECTIVITY%IDGLOB%UID(NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS))
           ENDIF
           IF (.NOT. ALLOCATED(MULTI_FVM%ELEM_DATA%CENTROID)) THEN
              ALLOCATE(MULTI_FVM%ELEM_DATA%CENTROID(3, NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS))
           ENDIF
           IF (.NOT. ALLOCATED(MULTI_FVM%FACE_DATA%CENTROID)) THEN
              ALLOCATE(MULTI_FVM%FACE_DATA%CENTROID(3, 6, NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS))
           ENDIF
           IF (.NOT. ALLOCATED(MULTI_FVM%VOL)) THEN
              ALLOCATE(MULTI_FVM%VOL(NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS))
           ENDIF
           CALL READ_I_C(ALE_CONNECTIVITY%IDGLOB%ID, NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS)
           CALL READ_I_C(ALE_CONNECTIVITY%IDGLOB%UID, NUMELS + NSVOIS + NUMELQ + NQVOIS + NUMELTG + NTGVOIS)
        ENDIF

        CALL READ_I_C(IFILL,SIFILL)

        CALL READ_I_C(IMS,SIMS)

        CALL SUBSET_INI

        CALL GROUP_INI

        CALL SURF_INI

        CALL LINE_INI

        CALL READ_I_C(KXX,SKXX)

        CALL READ_I_C(IXX,SIXX)

        CALL READ_I_C(KXSP,SKXSP)

        CALL READ_I_C(IXSP,SIXSP)

        CALL READ_I_C(NOD2SP,SNOD2SP)

        CALL READ_I_C(ISPSYM,SISPSYM)

        CALL READ_I_C(ISPCOND,SISPCOND)

        CALL READ_I_C(ISPHIO,SISPHIO)

        CALL READ_I_C(LPRTSPH,SLPRTSPH)

        CALL READ_I_C(LONFSPH,SLONFSPH)

        CALL READ_I_C(IBUFSSG_IO,SIBUFSSG_IO)

        IF(NSPHSOL/=0)THEN

          CALL READ_I_C(SPH2SOL,NUMSPH)

          CALL READ_I_C(SOL2SPH,2*NUMELS8)

          CALL READ_I_C(IRST,3*NSPHSOL)

          CALL READ_I_C(SOL2SPH_TYP,NUMELS8)
        END IF

        CALL READ_I_C(IBCL,SIBCL)

        IF (NINTLOADP>0) THEN
         CALL READ_I_C(KLOADPINTER, NINTER+1)
         CALL READ_I_C(LOADPINTER,NINTER*NLOADP_HYD)
        ENDIF

        CALL READ_I_C(IBFV,SIBFV)

        CALL READ_I_C(IECRAN,SIECRAN)

        CALL READ_I_C(ILAS,SILAS)

        ! -----------------------------
        ! EBCS option
        CALL READ_I_C(INT0, 1) ! total number of EBCS
        CALL READ_I_C(INT1, 1) ! number of multifluid ebcs
        CALL READ_I_C(INT2, 1) ! number of /NRF ebcs
        CALL READ_I_C(INT3, 1) ! number of other ebcs

        EBCS_TAB%nebcs = INT0
        EBCS_TAB%nebcs_fvm = INT1
        EBCS_TAB%nebcs_nrf = INT2
        EBCS_TAB%nebcs_loc = INT3

        IF (INT0 > 0) THEN
           ALLOCATE(INTEGER_DATA(INT0))
           CALL EBCS_TAB%read_type_data(INT0, INTEGER_DATA)
           ALLOCATE(EBCS_TAB%my_typ(INT0))
           ALLOCATE(EBCS_TAB%need_to_compute(INT0))
           ! save the type of /EBCS ( /EBCS/NRF --> typ = 10)
           EBCS_TAB%my_typ(1:INT0) = INTEGER_DATA(1:INT0)
           ! -------------
           ! check if the current proc needs to compute an /EBCS
           ! multifluid /EBCS and /EBCS/NRF are parallelized, can be computed by several proc
           ! the others are sequential, only the main proc (ispmd=0) computes the other /EBCS
           DO II=1,INT0
                EBCS_TAB%need_to_compute(II) = .FALSE.
                IF(ISPMD/=0) THEN
                    IF(EBCS_TAB%my_typ(II)==10)EBCS_TAB%need_to_compute(II) = .TRUE. ! /EBCS/NRF are //
                ELSE
                    EBCS_TAB%need_to_compute(II) = .TRUE.   ! other /EBCS sequential
                ENDIF
           ENDDO
           ! -------------
           ! create the data of /EBCS options
           CALL EBCS_TAB%create_from_types(INT1 + INT2 + INT3, INTEGER_DATA(1:INT1 + INT2 + INT3))
        ENDIF
        ! read the data of /EBCS options
        IF (INT1 + INT2 + INT3 > 0) THEN
           DO II = 1, INT1 + INT2 + INT3
              CALL EBCS_TAB%tab(II)%poly%read_common_data()
              CALL EBCS_TAB%tab(II)%poly%read_data()
           ENDDO
        ENDIF
        ! array allocation : adress for parith/on option
        ALLOCATE( EBCS_PARITHON(EBCS_TAB%nebcs) )
        ! -----------------------------

        CALL READ_I_C(LACCELM,SLACCELM)

        CALL READ_I_C(LGAUGE,3*NBGAUGE)

        CALL READ_I_C(NNLINK,SNNLINK)

        CALL READ_I_C(LNLINK,SLNLINK)

        CALL USERWI_READ(USER_WINDOWS,ISPMD,NSPMD,NUMNOD)

        CALL READ_I_C(IPARG,SIPARG)

        CALL READ_I_C(DD_IAD,SDD_IAD)

        CALL READ_I_C(IGRV,SIGRV)

        CALL READ_I_C(LGRAV,SLGRAV)

        CALL READ_I_C(IEXLNK,SIEXLNK)

        CALL READ_I_C(IBVEL,SIBVEL)

        CALL READ_I_C(LBVEL,SLBVEL)

        CALL READ_I_C(IRBE2,SIRBE2)

        CALL READ_I_C(LRBE2,SLRBE2)

        CALL READ_I_C(IRBE3,SIRBE3)

        CALL READ_I_C(LRBE3,SLRBE3)

        CALL READ_I_C(IACTIV,SIACTIV)

        CALL READ_I_C(IBMPC,SIBMPC)

        CALL READ_I_C(KINET,SKINET)

        CALL READ_I_C(FASOLFR,SFASOLFR)

        CALL READ_I_C(SEGQUADFR,SSEGQUADFR)

        CALL READ_I_C(IPARI,SIPARI)

c        CALL READ_I_C(INBUF,SINBUF)

        CALL R_BUFBRIC_22  ! I22LEN |-> BRICK_LIST(...);  CALL READ_I_C(); CALL READ_DB(); END SUBROUTINE

        !read new structure INTBUF_TAB
        CALL INTBUF_INI(INTBUF_TAB)

        CALL READ_I_C(NPRW,SNPRW)

        CALL READ_I_C(LPRW,SLPRW)

        CALL READ_I_C(ICONX,SICONX)

        CALL READ_I_C(MADPRT,SMADPRT)

        CALL READ_I_C(MADSH4,SMADSH4)

        CALL READ_I_C(MADSH3  ,SMADSH3)

        CALL READ_I_C(MADSOL,SMADSOL)

        CALL READ_I_C(MADNOD,SMADNOD)

        CALL READ_I_C(MADCLNOD,MADCLNODS)

        CALL READ_I_C(MADFAIL,SMADFAIL)

        CALL READ_I_C(MADIDX,SMADIDX)

        CALL READ_I_C(NPBY,SNPBY)

        CALL READ_I_C(NPBYL,SNPBYL)

        CALL READ_I_C(LPBY,SLPBY)

        CALL READ_I_C(LPBYL,SLPBYL)

        CALL READ_I_C(LRIVET,SLRIVET)

        CALL READ_I_C(NSTRF,SNSTRF)

        CALL READ_I_C(NOM_SECT,SNOM_SECT)

        CALL READ_JOINT()

        CALL READ_I_C(NODPOR,SNODPOR)

        CALL READ_I_C(MONVOL,SMONVOL)

        CALL READ_I_C(NTRI, NVOLU)
        DO II = 1, NVOLU
           T_MONVOL(II)%NB_FILL_TRI = NTRI(II)
           IF (NTRI(II) > 0) THEN
              ALLOCATE(T_MONVOL(II)%FILL_TRI(3 * NTRI(II)))
              CALL READ_I_C(T_MONVOL(II)%FILL_TRI(1:3 * NTRI(II)), 3 * NTRI(II))
           ENDIF
        ENDDO

        CALL READ_I_C(LAGBUF,SLAGBUF)

        CALL READ_I_C(ICNDS10,3*NS10E)

        ! Read Time History
        CALL READ_TH_RESTART(OUTPUT%TH)

        CALL THSURF_READ_RESTART(OUTPUT%TH%TH_SURF)

        CALL READ_I_C(FR_MV,SFR_MV)

        CALL READ_I_C(IAD_ELEM,SIAD_ELEM)

        CALL READ_I_C(IAD_RBY,SIAD_RBY)

        CALL READ_I_C(IAD_RBY2,SIAD_RBY2)

        CALL READ_I_C(IAD_I2M,SIAD_I2M)

        CALL READ_I_C(IADCJ,SIADCJ)

        CALL READ_I_C(IAD_RBM,SIAD_RBM)

        CALL READ_I_C(IAD_RBM2,SIAD_RBM2)

        CALL READ_I_C(IAD_RBE2,SIAD_RBE2)

        CALL READ_I_C(IAD_RBE3M,SIAD_RBE3M)

        CALL READ_I_C(IAD_SEC,SIAD_SEC)

        CALL READ_I_C(IAD_CUT,SIAD_CUT)
C
        CALL READ_I_C(IAD_RBYM,SIAD_RBYM)

        CALL READ_I_C(IAD_RBYM2,SIAD_RBYM2)
C
        CALL READ_I_C(FR_ELEM,SFR_ELEM)

        CALL READ_I_C(FR_RBY,SFR_RBY)

        CALL READ_I_C(FR_WALL,SFR_WALL)

        CALL READ_I_C(FR_RBY2,SFR_RBY2)

        CALL READ_I_C(FR_I2M,SFR_I2M)

        CALL READ_I_C(FR_LL,SFR_LL)

        CALL READ_I_C(FR_CJ,SFR_CJ)

        CALL READ_I_C(FR_RBM,SFR_RBM)

        CALL READ_I_C(FR_RBM2,SFR_RBM2)

        CALL READ_I_C(FR_RBE2,SFR_RBE2)

        CALL READ_I_C(FR_RBE3M,SFR_RBE3M)

        CALL READ_I_C(FR_RBE3MP,SFR_RBE3M)

        CALL READ_I_C(FR_SEC,SFR_SEC)

        CALL READ_I_C(FR_CUT,SFR_CUT)

        CALL READ_I_C(RG_CUT,SRG_CUT)

        CALL READ_I_C(FR_MAD,SFR_MAD)

        CALL READ_I_C(FR_I18,SFR_I18)

        CALL READ_I_C(DD_R2R,SDD_R2R)
C
        CALL READ_I_C(DD_R2R_ELEM,SDD_R2R_ELEM)
C
        CALL READ_I_C(FR_RBYM,SFR_RBYM)

        CALL READ_I_C(FR_RBYM2,SFR_RBYM2)
C
        CALL READ_I_C(ADDCSRECT,NUMNOR+1)

        CALL READ_I_C(FR_NOR,NBDDNORT)

        CALL READ_I_C(IAD_FRNOR,(NSPMD+1)*NINTER25)

        CALL READ_I_C(PROCNOR,NBCCNOR)

        CALL READ_I_C(FR_EDG,2*NBDDEDGT)

        CALL READ_I_C(IAD_FREDG,(NSPMD+1)*NINTER25)
C
        CALL READ_I_C(ISKWP,SISKWP)

        CALL READ_I_C(NSKWP,SNSKWP)

        IF(SNSKWP>0) CALL READ_I_C(ISKWP_L,NSKWP(ISPMD+1))

        CALL READ_I_C(ISENSP,SISENSP)

        CALL READ_I_C(NSENSP,SNSENSP)

        CALL READ_I_C(IACCP,SIACCP)

        CALL READ_I_C(NACCP,SNACCP)

        CALL READ_I_C(IGAUP,SIGAUP)

        CALL READ_I_C(NGAUP,SNGAUP)

        CALL READ_I_C(FR_LAGF,SFR_LAGF)

        CALL READ_I_C(NEWFRONT,SNEWFRONT)

        CALL READ_I_C(NBRCVOIS,SNBRCVOIS)

        CALL READ_I_C(LNRCVOIS,SLNRCVOIS)

        CALL READ_I_C(NBSDVOIS,SNBSDVOIS)

        CALL READ_I_C(LNSDVOIS,SLNSDVOIS)

        CALL READ_I_C(NERCVOIS,SNERCVOIS)

        CALL READ_I_C(LERCVOIS,SLERCVOIS)

        CALL READ_I_C(NESDVOIS,SNESDVOIS)

        CALL READ_I_C(LESDVOIS,SLESDVOIS)

        CALL READ_I_C(NPSEGCOM,SNPSEGCOM)

        CALL READ_I_C(LSEGCOM,SLSEGCOM)

        CALL READ_I_C(NPORGEO,SNPORGEO)

        CALL READ_I_C(LNODPOR,SLNODPOR)

        CALL READ_I_C(LLAGF,SLLAGF)
C
        WEIGHT_MD = WEIGHT

C
        IF(ICRACK3D > 0)THEN
          CALL READ_I_C(IAD_EDGE,SIAD_EDGE)
          CALL READ_I_C(FR_EDGE,SFR_EDGE)
          CALL READ_I_C(FR_NBEDGE,SFR_NBEDGE)
        ENDIF
C
        CALL READ_I_C(IAD_CNDM,SIAD_CNDM)
        CALL READ_I_C(FR_CNDM,SFR_CNDM)
C
c        IF (IMACH==3) THEN
C
C Partie Parith/ON lue en SPM dans tous les cas
C
C lecture directe de ADDCNE
        IF(IPARIT==1) THEN
          CALL READ_I_C(ADSKY,NUMNOD+1)
C lecture directe de PROCNE
          CALL READ_I_C(PROCNE,LCNE0)
C lecture directe structure interface type2
          IF (I2NSNT > 0) THEN
            CALL READ_I_C(ADDCNI2,NUMNOD+1)
          ENDIF
          CALL READ_I_C(PROCNI2,LCNI2)
C---Itet2 of S10
          IF(NS10E>0) THEN
           CALL READ_I_C(ADDCNCND,SADDCNCND)
          ENDIF
          CALL READ_I_C(PROCNCND,SPROCNCND)
C lecture directe de IADS+IADWAL+IADRBK+IADI2+IADMV2+IADMV3+IADLL+IADRBM+IADI18+
C  IADRBMK
          CALL READ_I_C(IADS,SIADS)

          CALL READ_I_C(IADWAL,SIADWAL)

          CALL READ_I_C(IADRBK,SIADRBK)

          CALL READ_I_C(IADI2,SIADI2)

          CALL READ_I_C(IADCND,SIADCND)

          CALL READ_I_C(IADMV2,SIADMV2)

          CALL READ_I_C(IADMV3,SIADMV3)

          CALL READ_I_C(IADLL,SIADLL)

          CALL READ_I_C(IADRBM,SIADRBM)

          CALL READ_I_C(IADI18,SIADI18)
C
          CALL READ_I_C(IADRBMK,SIADRBMK)
C
C  ply xfem
C
          IF(IPLYXFEM > 0) THEN
            CALL READ_I_C(ADSKY_PXFEM,NPLYXFE+1 )
C lecture directe de PROCNE
            CALL READ_I_C(PROCNE_PXFEM,LCNEPXFEM)
            CALL READ_I_C(IADC_PXFEM,4*EPLYXFE)
          ENDIF
C
C  xfem for layered shell (cracks)
C
          IF (ICRACK3D > 0) THEN
            CALL READ_I_C(ADSKY_CRKXFEM,NCRKXFE+1)
            CALL READ_I_C(CNE_CRKXFEM,LCNECRKXFEM)
            CALL READ_I_C(PROCNE_CRKXFEM,LCNECRKXFEM)
            CALL READ_I_C(IADC_CRKXFEM,4*ECRKXFEC+3*ECRKXFETG)
            CALL READ_I_C(CRKNODIAD,LCNECRKXFEM)
          ENDIF
C
        ELSEIF(IPARI0==1)THEN
C saut de lecture
          LEN = NUMNOD+1+LCNE0+(NUMNOD+1)*MIN(1,I2NSNT)+LCNI2
     +        + NISKY0+NSKYRW0+NSKYRBK0+NISKYI2+NSKYMV0+NSKYMVC0
     +        + NSKYLL0+NSKYRBM0+NSKYI18+NSKYRBE30+NSKYRBMK0
C
          IF(NS10E>0) LEN = LEN +SADDCNCND+SPROCNCND+SIADCND
          IF(IPLYXFEM > 0)LEN = LEN + LCNEPXFEM + 4*EPLYXFE+ NPLYXFE+1
          DO I = 1,LEN,MVSIZ
            CALL READ_I_C(IDUMM,MIN(MVSIZ,LEN-I+1))
          ENDDO
        ENDIF
c        ENDIF

        ! ---------------
        ! ebcs option : adress of FSKY array
        IF(EBCS_TAB%nebcs_nrf>0) THEN
            DO I=1,EBCS_TAB%nebcs
                IF(EBCS_TAB%my_typ(I)==10) THEN
                    MY_SIZE = EBCS_TAB%tab(I)%poly%nb_elem
                    ALLOCATE(EBCS_PARITHON(I)%ELEM_ADRESS(4,MY_SIZE))
                    CALL READ_I_C(EBCS_PARITHON(I)%ELEM_ADRESS,4*MY_SIZE)
                ENDIF
            ENDDO
        ENDIF
        ! ---------------

        CALL DECOMPRESS_I_NNZ(IGEO,SIGEO)

        CALL DECOMPRESS_I_NNZ(IPM, SIPM)

        CALL READ_I_C(ICONTACT ,SICONTACT)
c
        CALL READ_I_C(IPART_STATE ,NPART)
C--------------------------------------
      IF(NADMESH /= 0)THEN
        CALL READ_I_C(SH4TREE,KSH4TREE*NUMELC)
        CALL READ_I_C(SH3TREE,KSH3TREE*NUMELTG)
        CALL READ_I_C(IPADMESH,KIPADMESH*NPART)
        LEN=ABS(LSH4TRIM)
        CALL READ_I_C(SH4TRIM,LEN)
        LEN=ABS(LSH3TRIM)
        CALL READ_I_C(SH3TRIM,LEN)
      END IF
C--------------------------------------
C allocation + lecture struture interface partie entiere
C--------------------------------------
        CALL SPMD_INITFI(IPARI,1,H3D_DATA,INTERFACES%PARAMETERS)
C
C--------------------------------------
C     Lecture + Init Frontiere SPMD pour SPH (partie entiere)
C--------------------------------------
      CALL SPMD_INISPH(1)
C--------------------------------------
      IF(NINTSTAMP /= 0)THEN
        CALL INTSTAMP_RRESTI(INTSTAMP)
      END IF
C--------------------------------------
C  Interface friction buffer (reak )
C--------------------------------------
      IF(NINTERFRIC /= 0)THEN
        CALL INTFRIC_RRESTI(INTBUF_FRIC_TAB)
      END IF
C--------------------------------------
C  Structure table partie entiere
C--------------------------------------
      IF(NTABLE /= 0)THEN
        CALL TABLE_RRESTI(TABLE)
      END IF
C--------------------------------------
      CALL READ_I_C(ITHVAR,SITHVAR)
C--------------------------------------
      IF (NLOADC>0)THEN
        CALL READ_I_C(ICFIELD,SIZFIELD*NLOADC)
        CALL READ_I_C(LCFIELD,SLCFIELD)
      ENDIF
      IF (NLOADP>0)THEN
        CALL READ_I_C(ILOADP,SIZLOADP*NLOADP)
        CALL READ_I_C(LLOADP,SLLOADP)
      ENDIF
      IF (NLOADP_B>0)THEN
        CALL PBLAST_LOAD(PBLAST_TAB)
        CALL PBLAST_INIT_TABLES(PBLAST_DATA) !PBLAST_DATA defined in PBLAST_MOD
      ENDIF
c
      IF (LOADS%NLOAD_CYL > 0) THEN
        CALL READ_PCYL(LOADS)
      ENDIF
C----------------------------------
      IF(ICRACK3D > 0)THEN
        CALL READ_I_C(IBORDNODE,SIBORDNODE)
        CALL READ_I_C(IEDGESH,SIEDGESH)
        CALL READ_I_C(NODEDGE,2*NUMEDGES)
        CALL READ_I_C(IEDGE,NUMEDGES)
        CALL READ_I_C(NODGLOBXFE,SNODGLOBXFE)
      END IF
C----------------------------------
      CALL READ_I_C(KXIG3D,SKXIG3D)
      CALL READ_I_C(IXIG3D,SIXIG3D)
C----------------------------------
      CALL READ_I_C(TAG_SKINS6,NUMELS)
C----------------------------------
      IF(NBCSCYC > 0)THEN
        CALL READ_I_C(IBCSCYC,SIBCSCYC)
        CALL READ_I_C(LBCSCYC,SLBCSCYC)
      END IF
C----------------------------------
C     LECTURE DES VARIABLES REELLES
C--------------------------------------
C      warning to radioss developers :
C      IF(LVARREA>LMXVREA)THEN
C        WRITE(*,*) ' ** RADIOSS INTERNAL ERROR'
C        CALL ARRET(7)
C      ENDIF
C-----
      CALL READ_DB(TABVREA,LVARREA)
C
C-----
      CALL RDCOMR(LMXVREA,TABVREA,DYNAIN_DATA,INTERFACES%PARAMETERS,
     .            OUTPUT)
C--------------------------------------
C     READING REALS
C--------------------------------------
        CALL READ_DB(X,SX)

        CALL READ_DB(D,SD)

        CALL READ_DB(V,SV)

        CALL READ_DB(VR,SVR)

        CALL READ_DB(DR,SDR)

        CALL READ_DB(THKE,STHKE)

        CALL READ_DB(DAMPR,SDAMPR)

        CALL READ_DB(DAMP,SDAMP)

        CALL READ_DB(MS,SMS)

        IF (N2D >0) THEN
            CALL READ_DB(MS_2D,NUMNOD)
        ENDIF

        CALL READ_DB(IN,SIN)

        CALL READ_DB(TF,STF)

        CALL READ_DB(PM,SPM)

        CALL READ_MATPARAM(MATPARAM_TAB,NUMMAT)

        CALL READ_ELGROUP_PARAM(GROUP_PARAM_TAB,NGROUP)

        CALL READ_DB(SKEW,SSKEW)

        CALL READ_DB(XFRAME,SXFRAME)

        CALL DECOMPRESS_R_NNZ(GEO,SGEO)

        CALL READ_DB(EANI,SEANI)

        CALL READ_DB(BUFMAT,SBUFMAT)

        CALL READ_DB(BUFGEO,SBUFGEO)

        CALL READ_DB(BUFSF,SBUFSF)

        CALL READ_DB(RBMPC,SRBMPC)

        CALL READ_DB(GJBUFR,SGJBUFR)

        CALL READ_DB(W,SW)

        CALL READ_DB(VEUL,SVEUL)

        IF (MULTI_FVM%IS_USED) THEN
           CALL READ_DB(MULTI_FVM%PRES_SHIFT, 1)
           IF (N2D == 0) THEN
              CALL READ_DB(MULTI_FVM%VEL(1, :), NUMELS)
              CALL READ_DB(MULTI_FVM%VEL(2, :), NUMELS)
              CALL READ_DB(MULTI_FVM%VEL(3, :), NUMELS)
           ELSE
              CALL READ_DB(MULTI_FVM%VEL(1, :), NUMELQ + NUMELTG)
              CALL READ_DB(MULTI_FVM%VEL(2, :), NUMELQ + NUMELTG)
              CALL READ_DB(MULTI_FVM%VEL(3, :), NUMELQ + NUMELTG)
           ENDIF
        ENDIF

        CALL READ_DB(FILL,SFILL)

        CALL READ_DB(DFILL,SDFILL)

        CALL READ_DB(ALPH,SALPH)

        CALL READ_DB(WB,SWB)

        CALL READ_DB(DSAVE,SDSAVE)

        CALL READ_DB(DRSAVE,SDRSAVE)

        CALL READ_DB(ASAVE,SASAVE)

        CALL READ_DB(ARSAVE,SARSAVE)

        CALL READ_DB(STSAVE,SSTSAVE)

        CALL READ_DB(STRSAVE,SSTRSAVE)

        CALL READ_DB(FSAVSAV,SFSAVSAV)

        CALL READ_DB(SPBUF,SSPBUF)

        CALL READ_DB(VSPHIO,SVSPHIO)

        CALL READ_DB(SPHVELN,SSPHVELN)

        CALL READ_DB(MSNF,SMSNF)

        CALL READ_DB(FORC,SFORC)

        CALL READ_DB(VEL,SVEL)

        CALL READ_DB(FSAV,SFSAV)

        CALL READ_I_C(TAB_UMP,7*TAILLE)

        CALL READ_I_C(POIN_UMP,NUMMAT)

        CALL READ_DB(TAB_MAT,STAB_MAT)

        CALL READ_DB(FZERO,SFZERO)

        CALL READ_DB(XLAS,SXLAS)

        CALL READ_DB(ACCELM,SACCELM)

        CALL READ_DB(GAUGE,LLGAUGE*NBGAUGE)

        CALL READ_SENSORS(SENSORS)

        CALL READ_DB(FBVEL,SFBVEL)

        CALL READ_DB(FRBE3,SFRBE3)

        CALL READ_DB(FACTIV,LRACTIV*NACTIV)

        CALL READ_DB(GRAV,SGRAV)

        CALL READ_DB(FR_WAVE,SFR_WAVE)

        CALL READ_FAILWAVE(FAILWAVE)

        CALL READ_NLOC_STRUCT(NLOC_DMG)

        CALL READ_DB(PARTS0,SPARTS0)

        CALL READ_DB(ELBUF,SELBUF)

        CALL ELBUF_INI(NGROUP, IPARG)

        CALL READ_CLUSTER(CLUSTER)

        CALL READ_DB(RWBUF,SRWBUF)

        CALL READ_DB(RWSAV,SRWSAV)

        CALL READ_DB(RBY,SRBY)

        CALL READ_DB(RBYL,SRBYL)

        CALL READ_DB(RIVET,SRIVET)

        CALL READ_DB(SECBUF,SSECBUF)

        CALL READ_DB(VOLMON,SVOLMON)

        CALL READ_DB(LAMBDA,SLAMBDA)

        CALL READ_DB(RCONX,SRCONX)

        CALL READ_DB(RCONTACT ,SRCONTACT)
        CALL READ_DB(ACONTACT ,SRCONTACT)
        CALL READ_DB(PCONTACT ,SRCONTACT)

C lecture struture interface partie reelle si irunn > 1
        IF(IRUNN>1.OR.MCHECK/=0)CALL SPMD_INITFI(IPARI,2,H3D_DATA,INTERFACES%PARAMETERS)
C--------------------------------------
C     Lecture + Init Frontiere SPMD pour SPH (partie reelle)
C--------------------------------------
        CALL SPMD_INISPH(2)
C--------------------------------------
C     STOCKAGE FACULTATIFS
C--------------------------------------
      IF (IRESMD==0.AND.NPSAV>=25) THEN
         CALL READ_DB(PARTSAV,NPSAV*NPART)
      ENDIF

      IF(NNOISE>0.AND. (IRUNN>1 .OR. MCHECK/=0))THEN
c      IF(NNOISE>0.AND.IRUNN>1)THEN
          CALL READ_I_C (IAF(IF01),SINOISE)
          CALL READ_DB(AF(MF01),SFNOISE)
      ENDIF

       IF (IRESP == 1) THEN
         IF (IRXDP == 1) THEN
              CALL READ_DPDB(XDP,3*NUMNOD)
              CALL READ_DPDB(DDP,3*NUMNOD)
         ELSE
           CALL FILLXDP(X,XDP,D,DDP)
C next restart, XDP will be on restart head !
           IRXDP=1
         ENDIF
C Cas double precision
       ELSE
         IF (IRXDP ==1)THEN
C Reading Dummy if XDP is in Restart file. XDP written during next restart.
           ALLOCATE(XDPDUM(3*NUMNOD))
C Read XDP
           CALL READ_DB(XDPDUM,3*NUMNOD)
C Read DDP
           CALL READ_DB(XDPDUM,3*NUMNOD)
           DEALLOCATE(XDPDUM)
           IRXDP=0
         ENDIF
       ENDIF
       NPSAV = 28
C   ----------------------------------------------------------------------
C     Utilise pour les checks point restart.
       IF(MCHECK/=0)THEN
        CALL READ_I_C(MY_ILEN,1)
        CALL READ_I_C(MY_RLEN,1)
        IF(MOD(IRFORM,5)>=2.AND.MOD(IRFORM,5)<=4) THEN
          CALL READ_I_C(ILINK,4*NRLINK)
          CALL READ_I_C(FR_RL,(NSPMD+2)*NRLINK)
          CALL READ_I_C(LLINK,LLLINK)
          CALL READ_I_C(IADRL,MIN(1,IPARIT)*LLLINK)
          !CALL READ_I_C(LINALE,LLINAL)
          CALL READ_I_C(NEFLSW,NSFLSW)
          CALL READ_I_C(NNFLSW,8*NTFLSW)
          CALL READ_I_C(ICUT,44*NCUTS)
          CALL READ_DB(CRFLSW,9*NSFLSW)
          CALL READ_DB(XCUT,7*NCUTS)
        ENDIF
       ELSE
        CALL READ_I_C(MY_ILEN,1)
        CALL READ_I_C(MY_RLEN,1)
        ALLOCATE(IBID(MY_ILEN),STAT=ISTAT)
          IF(ISTAT/=0)THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
        ALLOCATE(RBID(MY_RLEN),STAT=ISTAT)
          IF(ISTAT/=0)THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
          CALL READ_I_C(IBID,MY_ILEN)
          CALL READ_DB (RBID,MY_RLEN)
       ENDIF
C   ----------------------------------------------------------------------
      IF(NADMESH /= 0 .OR. IREST_MSELT /= 0)THEN
        CALL READ_DB(MSC,NUMELC)
        CALL READ_DB(INC,NUMELC)
        CALL READ_DB(MSTG,NUMELTG)
        CALL READ_DB(INTG,NUMELTG)
        CALL READ_DB(PTG,3*NUMELTG)
      END IF

      IF(IREST_MSELT /= 0)THEN
        CALL READ_DB(MSSA,NUMELS)
        CALL READ_DB(MSTR,NUMELT)
        CALL READ_DB(MSP,NUMELP)
        CALL READ_DB(MSRT,NUMELR)
      END IF

      IF(NADMESH /= 0)THEN
        CALL READ_DB(PADMESH,KPADMESH*NPART)
        IF(ITHERM_FE > 0)THEN
          CALL READ_DB(MCPC,NUMELC)
          CALL READ_DB(MCPTG,NUMELTG)
        END IF
      END IF
C pinching
      IF(NPINCH > 0) THEN
        CALL READ_DB(PINCH_DATA%VPINCH,3*NPINCH)
        CALL READ_DB(PINCH_DATA%DPINCH,3*NPINCH)
        CALL READ_DB(PINCH_DATA%XPINCH,3*NPINCH)
        CALL READ_DB(PINCH_DATA%MSPINCH,NPINCH)
      ENDIF
C   ----------------------------------------------------------------------
      IF(ISTATCND /= 0)THEN
        CALL READ_DB(MSCND,NUMNOD)
        CALL READ_DB(INCND,NUMNOD)
      ENDIF
C   ----------------------------------------------------------------------
      IF(NINTSTAMP /= 0)THEN
        CALL INTSTAMP_RRESTR(INTSTAMP)
      END IF
      IF(NINTSKIDOLD > 0) THEN
         IF(NINTSTAMP/=0) THEN
           CALL READ_DB(PSKIDS, NINTSKIDOLD*NUMNODG)
         ELSE
           CALL READ_DB(PSKIDS, NINTSKIDOLD*NUMNOD)
         ENDIF
      ENDIF
      IF(IFCONTMAX >0)THEN
        IF(NINTSTAMP/=0.AND.NSPMD > 1 ) THEN
           IF(ISPMD == 0) CALL READ_DB(FCONT_MAX, 3*NUMNODG)
         ELSE
           CALL READ_DB(FCONT_MAX, 3*NUMNOD)
         ENDIF
      ENDIF
      IF(IFCONTPMAX >0)THEN
         CALL READ_DB(FNCONT_MAX, 3*SPCONT_MAX)
         CALL READ_DB(FTCONT_MAX, 3*SPCONT_MAX)
      ENDIF
      IF(IFCONT2MAX >0)THEN
         CALL READ_DB(FCONT2_MAX, 3*NUMNOD)
      ENDIF
      IF(IFCONTP2MAX >0)THEN
         CALL READ_DB(FNCONT2_MAX, 3*NUMNOD)
         CALL READ_DB(FTCONT2_MAX, 3*NUMNOD)
      ENDIF
      IF(IFCONT2MIN >0)THEN
         CALL READ_DB(FCONT2_MIN, 3*NUMNOD)
      ENDIF
      IF(IFCONTP2MIN >0)THEN
         CALL READ_DB(FNCONT2_MIN, 3*NUMNOD)
         CALL READ_DB(FTCONT2_MIN, 3*NUMNOD)
      ENDIF
C--------Frictional energy output-------
      IF(S_EFRICINT >0)THEN
         IF(NINEFRIC > 0) CALL READ_DB(EFRIC, NINEFRIC*NUMNOD)
         IF(NINEFRIC_STAMP > 0) CALL READ_DB(EFRIC_STAMP, NINEFRIC_STAMP*NUMNODG)
      ENDIF
      IF(S_EFRIC >0)THEN
         CALL READ_DB(EFRICG, NUMNOD)
         IF(NINTSTAMP/=0) CALL READ_DB(EFRICG_STAMP, NUMNODG)
      ENDIF
C--------------------------------------
C  Interface friction buffer (Reals )
C--------------------------------------
      IF(NINTERFRIC /= 0)THEN
        CALL INTFRIC_RRESTR(INTBUF_FRIC_TAB)
      END IF
C-------------
C     always
      CALL READ_DB(MS0,NUMNOD)
      CALL READ_DB(IN0,SIN)
      IF(IDTMINS_OLD==1) THEN
        CALL READ_DB(ADMSMS,NUMNOD)
      ELSEIF(IDTMINS_OLD==2) THEN
        CALL READ_DB(DMELC ,NUMELC)
        CALL READ_DB(DMELTG,NUMELTG)
        CALL READ_DB(DMELS ,NUMELS)
        CALL READ_DB(DMELTR,NUMELT)
        CALL READ_DB(DMELP ,NUMELP)
        CALL READ_DB(DMELRT,NUMELR)
      END IF
      IF(IDTMINS_OLD/=0.OR.IDTMINS_INT_OLD/=0)THEN
        CALL READ_DB(RES_SMS,3*NUMNOD)
      END IF
      IF(IDTMINS_OLD==2.OR.IDTMINS_INT_OLD/=0) THEN
Cfor TYPE20
        CALL READ_DB(DIAG_SMS ,NUMNOD)
Cfor TYPE2
        CALL READ_DB(DMINT2 ,4*I2NSN25)
      END IF
      IF (ISMS_SELEC /= 0) THEN
        CALL READ_I_C(NATIV0_SMS,NUMNOD)
      ENDIF
C--------------------------------------
C structure tables partie reelle
C--------------------------------------
      IF(NTABLE /= 0)THEN
        CALL TABLE_RRESTR(TABLE)
      END IF
C------------------------
C Read MDS Parameters
C------------------------
#ifdef DNC
      CALL ENG_READ_MDS()
#endif
C--------------------------------------
C--------------------------------------
C ALE LINK
C--------------------------------------
!      print *,"rdresb.F: READ_I_C, SLINALE=", SLINALE
      IF (SLINALE>0)CALL READ_I_C(LINALE,SLINALE)
C--------------------------------------
C FXBODY (moved from radioss2.F)
C--------------------------------------
      IF (NFXBODY>0) THEN
         CALL FXBRESB()
      ENDIF
C--------------------------------------
C EIGEN MODES (moved from radioss2.F)
C--------------------------------------
      IF (NEIG>0) THEN
         CALL EIGRREST()
      ENDIF
C--------------------------------------
C   SEATBELTS - SLIPRING / RETRACTOR-
C--------------------------------------
      DO I = 1, NSLIPRING
C
        CALL READ_I_C(SLIPRING(I)%ID, 1)
        CALL READ_I_C(SLIPRING(I)%IDG, 1)
        CALL READ_I_C(SLIPRING(I)%NFRAM, 1)
        CALL READ_I_C(SLIPRING(I)%IFUNC, 4)
        CALL READ_I_C(SLIPRING(I)%SENSID, 1)
        CALL READ_I_C(SLIPRING(I)%FL_FLAG, 1)
        CALL READ_I_C(SLIPRING(I)%RBODY, 1)
C
        CALL READ_DB(SLIPRING(I)%DC, 1)
        CALL READ_DB(SLIPRING(I)%A, 1)
        CALL READ_DB(SLIPRING(I)%FRIC, 1)
        CALL READ_DB(SLIPRING(I)%FAC_D, 3)
        CALL READ_DB(SLIPRING(I)%FRICS, 1)
        CALL READ_DB(SLIPRING(I)%FAC_S, 3)
C
        ALLOCATE(SLIPRING(I)%FRAM(SLIPRING(I)%NFRAM))
C
        DO J=1,SLIPRING(I)%NFRAM
C
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%UPDATE, 1)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%ANCHOR_NODE, 1)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%NODE, 3)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%NODE_NEXT, 3)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%NODE2_PREV, 1)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%N_REMOTE_PROC, 1)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%ORIENTATION_NODE, 1)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%STRAND_DIRECTION, 2)
          CALL READ_I_C(SLIPRING(I)%FRAM(J)%LOCKED, 1)
C
          CALL READ_DB(SLIPRING(I)%FRAM(J)%VECTOR, 6)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE, 1)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%MATERIAL_FLOW, 1)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%DFS, 1)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%RESIDUAL_LENGTH, 2)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%CURRENT_LENGTH, 2)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%RINGSLIP, 1)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%BETA, 1)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%SLIP_FORCE, 3)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%PREV_REF_LENGTH, 1)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%INTVAR_STR1, 8)
          CALL READ_DB(SLIPRING(I)%FRAM(J)%INTVAR_STR2, 8)
C
        ENDDO
C
      ENDDO
C
      DO I = 1, NRETRACTOR
        CALL READ_I_C(RETRACTOR(I)%ID, 1)
        CALL READ_I_C(RETRACTOR(I)%IDG, 1)
        CALL READ_I_C(RETRACTOR(I)%UPDATE, 1)
        CALL READ_I_C(RETRACTOR(I)%ANCHOR_NODE, 1)
        CALL READ_I_C(RETRACTOR(I)%NODE, 2)
        CALL READ_I_C(RETRACTOR(I)%NODE_NEXT, 2)
        CALL READ_I_C(RETRACTOR(I)%STRAND_DIRECTION, 1)
        CALL READ_I_C(RETRACTOR(I)%IFUNC, 3)
        CALL READ_I_C(RETRACTOR(I)%ISENS, 2)
        CALL READ_I_C(RETRACTOR(I)%TENS_TYP, 1)
        CALL READ_I_C(RETRACTOR(I)%LOCKED, 1)
        CALL READ_I_C(RETRACTOR(I)%PRETENS_ACTIV, 1)
        CALL READ_I_C(RETRACTOR(I)%INACTI_NNOD, 1)
        CALL READ_I_C(RETRACTOR(I)%INACTI_NNOD_MAX, 1)
        ALLOCATE(RETRACTOR(I)%INACTI_NODE(RETRACTOR(I)%INACTI_NNOD_MAX))
        CALL READ_I_C(RETRACTOR(I)%INACTI_NODE, RETRACTOR(I)%INACTI_NNOD)
        CALL READ_I_C(RETRACTOR(I)%N_REMOTE_PROC, 1)
        CALL READ_DB(RETRACTOR(I)%VECTOR, 3)
        CALL READ_DB(RETRACTOR(I)%ELEMENT_SIZE, 1)
        CALL READ_DB(RETRACTOR(I)%FORCE, 1)
        CALL READ_DB(RETRACTOR(I)%MATERIAL_FLOW, 1)
        CALL READ_DB(RETRACTOR(I)%RESIDUAL_LENGTH, 1)
        CALL READ_DB(RETRACTOR(I)%FAC, 4)
        CALL READ_DB(RETRACTOR(I)%PULLOUT, 1)
        CALL READ_DB(RETRACTOR(I)%UNLOCK_FORCE, 1)
        CALL READ_DB(RETRACTOR(I)%LOCK_PULL, 1)
        CALL READ_DB(RETRACTOR(I)%LOCK_OFFSET, 1)
        CALL READ_DB(RETRACTOR(I)%LOCK_YIELD_FORCE, 1)
        CALL READ_DB(RETRACTOR(I)%RINGSLIP, 1)
        CALL READ_DB(RETRACTOR(I)%PRETENS_TIME, 1)
        CALL READ_DB(RETRACTOR(I)%PRETENS_PULL, 1)
        CALL READ_DB(RETRACTOR(I)%PRETENS_PULLMAX, 1)
        CALL READ_DB(RETRACTOR(I)%RET_FORCE, 1)
      ENDDO
C
      IF (N_ANCHOR_REMOTE > 0) THEN
        CALL READ_I_C(ANCHOR_REMOTE%ADD_PROC, NSPMD+1)
        CALL READ_I_C(ANCHOR_REMOTE%NODE, N_ANCHOR_REMOTE)
      ENDIF
C
      IF (N_ANCHOR_REMOTE_SEND > 0) THEN
        CALL READ_I_C(ANCHOR_REMOTE_SEND%ADD_PROC, NSPMD+1)
        CALL READ_I_C(ANCHOR_REMOTE_SEND%NODE, N_ANCHOR_REMOTE_SEND)
      ENDIF
C
      IF ((NSLIPRING_G + NRETRACTOR_G >0).AND.(ISPMD == 0)) THEN
        ALLOCATE(TH_SLIPRING(NSLIPRING_G,6))
        ALLOCATE(TH_RETRACTOR(NRETRACTOR_G,3))
        TH_SLIPRING = 0
        TH_RETRACTOR = 0
        CALL READ_I_C(NSEATBELT_TH_PROC, 1)
        IF ((NSPMD >1).AND.(NSEATBELT_TH_PROC > 0)) THEN
          ALLOCATE(SEATBELT_TH_EXCH(NSEATBELT_TH_PROC))
          DO I=1,NSEATBELT_TH_PROC
            CALL READ_I_C(SEATBELT_TH_EXCH(I)%ID_PROC, 1)
            CALL READ_I_C(SEATBELT_TH_EXCH(I)%ADD_PROC, 1)
            CALL READ_I_C(SEATBELT_TH_EXCH(I)%NSLIPRING, 1)
            CALL READ_I_C(SEATBELT_TH_EXCH(I)%NRETRACTOR, 1)
          ENDDO
        ENDIF
      ENDIF
C--------------------------------------
C   /H3D/?/TMAX
C--------------------------------------
      CALL READ_DB(TM_DIS,LMAX_DIS*NUMNOD)
      CALL READ_DB(TM_VEL,LMAX_VEL*NUMNOD)
      CALL READ_DB(TM_NSIG1,LMAX_NSIG*NUMNOD)
      CALL READ_DB(TM_NSIG3,LMAX_NSIG*NUMNOD)
      CALL READ_DB(TM_NSTRA1,LMAX_NSTRA*NUMNOD)
      CALL READ_DB(TM_NSTRA3,LMAX_NSTRA*NUMNOD)
C
      RETURN
      END
Chd|====================================================================
Chd|  FXBRESB                       source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        MY_GETPID                     source/system/pid.c           
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|        TMPENVF                       source/system/tmpenv_c.c      
Chd|        FXB_MOD                       share/modules/fxb_mod.F       
Chd|        SCRATCHFILE_MOD               share/modules/scratchfiles_mod.F
Chd|====================================================================
      SUBROUTINE FXBRESB()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SCRATCHFILE_MOD
      USE FXB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "fxbcom.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN_IPM,LEN_MOD, NRECM, NRECS, IRCM, IRCM0, IRCM1,
     .        IRCS, IRCS0, IRCS1, LREC, I, J, RCLEN,TLEN
      my_real
     .        VV(6)
      INTEGER MY_PID
      CHARACTER(LEN=10) :: CPID
      CHARACTER(LEN=6) :: CISPMD
      CHARACTER(LEN=4096) :: TMPDIR
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
      LEN_IPM=NBIPM*NFXBODY
      IF (IRFORM/5<=1) THEN
        CALL REDSQI (FXBIPM,LEN_IPM,IRFORM)
        IF (LENNOD>0) CALL REDSQI (FXBNOD,LENNOD,IRFORM)
        IF (LENELM>0) CALL REDSQI (FXBELM,LENELM,IRFORM)
        IF (LENGRVI>0) CALL REDSQI (FXBGRVI,LENGRVI,IRFORM)
      ELSE
        CALL READ_I_C(FXBIPM,LEN_IPM)
        IF (LENNOD>0) CALL READ_I_C(FXBNOD,LENNOD)
        IF (LENELM>0) CALL READ_I_C(FXBELM,LENELM)
        IF (LENGRVI>0) CALL READ_I_C(FXBGRVI,LENGRVI)
      ENDIF
C--------------------------------------
C     READING REALS
C--------------------------------------
      LEN_MOD=LENMOD*6
      IF (IRFORM/5<=1) THEN
        IF (LEN_MOD>0) CALL REDSQR (FXBMOD,LEN_MOD,IRFORM)
        IF (LENGLM>0) CALL REDSQR (FXBGLM,LENGLM,IRFORM)
        IF (LENCP>0) CALL REDSQR (FXBCPM,LENCP ,IRFORM)
        IF (LENCP>0) CALL REDSQR (FXBCPS,LENCP ,IRFORM)
        IF (LENLM>0) CALL REDSQR (FXBLM, LENLM ,IRFORM)
        IF (LENFLS>0) CALL REDSQR (FXBFLS,LENFLS,IRFORM)
        IF (LENDLS>0) CALL REDSQR (FXBDLS,LENDLS,IRFORM)
        CALL REDSQR (FXBDEP,LENVAR,IRFORM)
        CALL REDSQR (FXBVIT,LENVAR,IRFORM)
        CALL REDSQR (FXBACC,LENVAR,IRFORM)
        CALL REDSQR (FXBRPM,LENRPM,IRFORM)
        IF (LENSIG>0) CALL REDSQR (FXBSIG,LENSIG,IRFORM)
        IF (LENGRVR>0) CALL REDSQR (FXBGRVR,LENGRVR,IRFORM)
      ELSE
        IF (LEN_MOD>0) CALL READ_DB(FXBMOD,LEN_MOD)
        IF (LENGLM>0) CALL READ_DB(FXBGLM,LENGLM)
        IF (LENCP>0) CALL READ_DB(FXBCPM,LENCP )
        IF (LENCP>0) CALL READ_DB(FXBCPS,LENCP )
        IF (LENLM>0) CALL READ_DB(FXBLM, LENLM )
        IF (LENFLS>0) CALL READ_DB(FXBFLS,LENFLS)
        IF (LENDLS>0) CALL READ_DB(FXBDLS,LENDLS)
        CALL READ_DB(FXBDEP,LENVAR)
        CALL READ_DB(FXBVIT,LENVAR)
        CALL READ_DB(FXBACC,LENVAR)
        CALL READ_DB(FXBRPM,LENRPM)
        IF (LENSIG>0) CALL READ_DB(FXBSIG,LENSIG)
        IF (LENGRVR>0) CALL READ_DB(FXBGRVR,LENGRVR)
      ENDIF
C Lecture des fichiers de modes et de contraintes
      INQUIRE(IOLENGTH=RCLEN) VV

      TLEN=4096
      CALL TMPENVF(TMPDIR,TLEN)
      CALL MY_GETPID(MY_PID)
      WRITE(CPID,'(I10.10)') MY_PID
      WRITE(CISPMD,'(I6.6)') ISPMD
      IFXM_FN=TMPDIR(1:TLEN)//'/'//'25_'//ROOTN(1:LENROOTN)//'_'//CPID//'_'//CISPMD//'.tmp'
      IFXS_FN=TMPDIR(1:TLEN)//'/'//'26_'//ROOTN(1:LENROOTN)//'_'//CPID//'_'//CISPMD//'.tmp'
      LEN_IFXM_FN=LEN_TRIM(IFXM_FN)
      LEN_IFXS_FN=LEN_TRIM(IFXS_FN)

      OPEN(UNIT=IFXM,FILE=TRIM(IFXM_FN),ACCESS='DIRECT',RECL=RCLEN)
      OPEN(UNIT=IFXS,FILE=TRIM(IFXS_FN),ACCESS='DIRECT',RECL=RCLEN)
      

      USE_IFXM = 1
      USE_IFXS = 1
      NRECM=0
      NRECS=0
      DO I=1,NFXBODY
         IRCM0=FXBIPM(30,I)
         IRCS0=FXBIPM(31,I)
         IRCM1=FXBIPM(32,I)
         IRCS1=FXBIPM(33,I)
         NRECM=NRECM+IRCM1-IRCM0
         NRECS=NRECS+IRCS1-IRCS0
      ENDDO
      IRCM=0
      IRCS=0
      LREC=6
      DO I=1,NRECM
         IRCM=IRCM+1
         CALL READ_DB(VV,LREC)
         WRITE(IFXM,REC=IRCM) (VV(J),J=1,LREC)
      ENDDO
      DO I=1,NRECS
         IRCS=IRCS+1
         CALL READ_DB(VV,LREC)
         WRITE(IFXS,REC=IRCS) (VV(J),J=1,LREC)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  EIGRREST                      source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        MY_GETPID                     source/system/pid.c           
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|        TMPENVF                       source/system/tmpenv_c.c      
Chd|        EIG_MOD                       share/modules/eig_mod.F       
Chd|        SCRATCHFILE_MOD               share/modules/scratchfiles_mod.F
Chd|====================================================================
      SUBROUTINE EIGRREST()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EIG_MOD
      USE SCRATCHFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "eigcom.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRECM, IRCM, NBN, NBM, LREC, I, J, RCLEN,TLEN
      my_real
     .        VV(6)
      CHARACTER(LEN=10) :: CPID
      CHARACTER(LEN=6) :: CISPMD
      CHARACTER(LEN=4096) :: TMPDIR
      INTEGER MY_PID
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQI (EIGIPM,NEIG*NEIPM,IRFORM)
        CALL REDSQI (EIGIBUF,LEIBUF,IRFORM)
      ELSE
        CALL READ_I_C(EIGIPM,NEIG*NEIPM)
        CALL READ_I_C(EIGIBUF,LEIBUF)
      ENDIF
C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (EIGRPM,NEIG*NERPM,IRFORM)
      ELSE
        CALL READ_DB(EIGRPM,NEIG*NERPM)
      ENDIF
C Lecture du fichier de modes additionnels
      INQUIRE(IOLENGTH=RCLEN) VV

      CALL MY_GETPID(MY_PID)
      WRITE(CPID,'(I10.10)') MY_PID
      WRITE(CISPMD,'(I6.6)') ISPMD
      TLEN=4096
      CALL TMPENVF(TMPDIR,TLEN)
      IEIGM_FN=TMPDIR(1:TLEN)//'/'//'27_'//ROOTN(1:LENROOTN)//'_'//CPID//'_'//CISPMD//'.tmp'
      LEN_IEIGM_FN=LEN_TRIM(IEIGM_FN)
      OPEN(UNIT=IEIGM,FILE=TRIM(IEIGM_FN),ACCESS='DIRECT',RECL=RCLEN)
      USE_IEIGM = 1
      
      NRECM=0
      DO I=1,NEIG
         NBN=EIGIPM(10,I)
         NBM=EIGIPM(14,I)
         NRECM=NRECM+NBN*NBM
      ENDDO
C
      IRCM=0
      LREC=6
      DO I=1,NRECM
         IRCM=IRCM+1
         CALL READ_DB(VV,LREC)
         WRITE(IEIGM,REC=IRCM) (VV(J),J=1,LREC)
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  DSRREST                       source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRREST(GRAPHE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
*
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, TN(7), N, NDDL, DSNDDL, DSLEN, K, L, NSDEC
      my_real
     .        CUTFREQ
C
      DO I=1,NSPROC
C--------------------------------------
C Lecture des parametres entiers
C--------------------------------------
         N=7
         IF (IRFORM/5<=1) THEN
           CALL REDSQI(TN, N, IRFORM)
         ELSE
           CALL READ_I_C(TN, N)
         ENDIF
         DSNDDL=TN(1)
         DSLEN=TN(2)
         NSDEC=TN(4)
         GRAPHE(I)%NDDL=DSNDDL
         GRAPHE(I)%NSUPEL=DSLEN
         GRAPHE(I)%NSLEVEL=TN(3)
         GRAPHE(I)%NSDEC=NSDEC
         GRAPHE(I)%NSVMAX=TN(5)
         GRAPHE(I)%IPRI=TN(6)
         GRAPHE(I)%NDDL_GLOB=TN(7)
C--------------------------------------
C     READING REALS
C--------------------------------------
         N=1
         IF (IRFORM/5<=1) THEN
           CALL REDSQR(CUTFREQ, N, IRFORM)
         ELSE
           CALL READ_DB(CUTFREQ, N)
         ENDIF
         GRAPHE(I)%CUTFREQ=CUTFREQ
C--------------------------------------
C     READING ARRAY DOF
C--------------------------------------
         ALLOCATE(GRAPHE(I)%LSDDL(2,DSNDDL),
     .            GRAPHE(I)%LSDDL_INI(2,DSNDDL),
     .            GRAPHE(I)%LSDDL_GLOB(DSNDDL),
     .            GRAPHE(I)%LSDDL_GLOB_INI(DSNDDL))
         DO J=1,2
            IF (IRFORM/5<=1) THEN
              CALL REDSQI(GRAPHE(I)%LSDDL(J,1:DSNDDL), DSNDDL, IRFORM)
            ELSE
              CALL READ_I_C(GRAPHE(I)%LSDDL(J,1:DSNDDL), DSNDDL)
            ENDIF
         ENDDO
         IF (IRFORM/5<=1) THEN
           CALL REDSQI(GRAPHE(I)%LSDDL_GLOB, DSNDDL, IRFORM)
         ELSE
           CALL READ_I_C(GRAPHE(I)%LSDDL_GLOB, DSNDDL)
         ENDIF
         GRAPHE(I)%NDDL_INI=GRAPHE(I)%NDDL
         DO J=1,2
            DO K=1,DSNDDL
               GRAPHE(I)%LSDDL_INI(J,K)=GRAPHE(I)%LSDDL(J,K)
            ENDDO
         ENDDO
         DO J=1,DSNDDL
            GRAPHE(I)%LSDDL_GLOB_INI(J)=GRAPHE(I)%LSDDL_GLOB(J)
         ENDDO
C--------------------------------------
C     READING SUPERELEMENTS
C--------------------------------------
         ALLOCATE(GRAPHE(I)%DGRAPH(DSLEN))
         DO J=1,DSLEN
            N=3
            IF (IRFORM/5<=1) THEN
              CALL REDSQI(TN, N, IRFORM)
            ELSE
              CALL READ_I_C(TN, N)
            ENDIF
            GRAPHE(I)%DGRAPH(J)%NDDL_I=TN(1)
            GRAPHE(I)%DGRAPH(J)%NDDL_F=TN(2)
            GRAPHE(I)%DGRAPH(J)%NSDMAX=TN(3)
            NDDL=TN(1)+TN(2)
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%CHILD(NSDEC))
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%DDLS(NDDL),
     .               GRAPHE(I)%DGRAPH(J)%DDLS_INI(NDDL))
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%IFAC(TN(3)+1,TN(2)),
     .               GRAPHE(I)%DGRAPH(J)%IFACM(TN(2)))
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%IFAC_INI(TN(3)+1,TN(2)),
     .               GRAPHE(I)%DGRAPH(J)%IFACM_INI(TN(2)))
            N=TN(3)+1
            IF (IRFORM/5<=1) THEN
              CALL REDSQI(GRAPHE(I)%DGRAPH(J)%CHILD, NSDEC, IRFORM)
              CALL REDSQI(GRAPHE(I)%DGRAPH(J)%DDLS, NDDL, IRFORM)
              DO K=1,TN(2)
                 CALL REDSQI(GRAPHE(I)%DGRAPH(J)
     .                                %IFAC(1:N,K), N, IRFORM)
              ENDDO
              CALL REDSQI(GRAPHE(I)%DGRAPH(J)%IFACM, TN(2), IRFORM)
            ELSE
              CALL READ_I_C(GRAPHE(I)%DGRAPH(J)%CHILD, NSDEC)
              CALL READ_I_C(GRAPHE(I)%DGRAPH(J)%DDLS, NDDL)
              DO K=1,TN(2)
                 CALL READ_I_C(GRAPHE(I)%DGRAPH(J)
     .                                  %IFAC(1:N,K), N)
              ENDDO
              CALL READ_I_C(GRAPHE(I)%DGRAPH(J)%IFACM, TN(2))
            ENDIF
            GRAPHE(I)%DGRAPH(J)%NDDL_I_INI=GRAPHE(I)%DGRAPH(J)%NDDL_I
            GRAPHE(I)%DGRAPH(J)%NDDL_F_INI=GRAPHE(I)%DGRAPH(J)%NDDL_F
            DO K=1,NDDL
               GRAPHE(I)%DGRAPH(J)%DDLS_INI(K)=
     .                 GRAPHE(I)%DGRAPH(J)%DDLS(K)
            ENDDO
            DO K=1,TN(2)
               DO L=1,TN(3)+1
                  GRAPHE(I)%DGRAPH(J)%IFAC_INI(L,K)=
     .                 GRAPHE(I)%DGRAPH(J)%IFAC(L,K)
               ENDDO
               GRAPHE(I)%DGRAPH(J)%IFACM_INI(K)=
     .                 GRAPHE(I)%DGRAPH(J)%IFACM(K)
            ENDDO
         ENDDO
C
      ENDDO
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  NFRREST                       source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE NFRREST(IFLOW, RFLOW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "flowcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*)
      my_real
     .        RFLOW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQI (IFLOW,LIFLOW,IRFORM)
      ELSE
        CALL READ_I_C(IFLOW,LIFLOW)
      ENDIF
C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (RFLOW,LRFLOW,IRFORM)
      ELSE
        CALL READ_DB(RFLOW,LRFLOW)
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  IMPRREST                      source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        IMP_REST                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMPRREST(IRFORM,NIMPR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_REST
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRFORM,NIMPR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND,IDYNA0
      my_real
     . A(3*NUMNOD),AR(3*NUMNOD)
C--------------------------------------
       IF(ALLOCATED(IMP_RR)) DEALLOCATE(IMP_RR)
       ALLOCATE(IMP_RR(NIMPR))
C--------------------------------------
C     READING REALS
C--------------------------------------
      ND = NUMNOD*3
      IDYNA0 = NIMPR - 2
      IF (IRFORM/5<=1) THEN
       CALL REDSQR(IMP_RR,NIMPR,IRFORM)
       IF (IDYNA0>0) THEN
        CALL REDSQR(A,ND,IRFORM)
        IF (IRODDL/=0) CALL REDSQR(AR,ND,IRFORM)
       ENDIF
      ELSE
       CALL READ_DB(IMP_RR,NIMPR)
       IF (IDYNA0>0) THEN
        CALL READ_DB(A,ND)
        IF (IRODDL/=0) CALL READ_DB(AR,ND)
       ENDIF
      ENDIF
C
      IF ((IDYNA+IDYNA0)>0) THEN
       ALLOCATE(DY_A(3,NUMNOD))
       IF (IRODDL/=0) ALLOCATE(DY_AR(3,NUMNOD))
       CALL CP_REAL(ND,A,DY_A)
       IF (IRODDL/=0) CALL CP_REAL(ND,AR,DY_AR)
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  FVRREST                       source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        SPMD_COMM_SPLIT               source/mpi/generic/spmd_comm_split.F
Chd|        SPMD_FVB_COMM_PATTERN         source/mpi/airbags/spmd_fvb_comm_pattern.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVRREST(MONVOL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE MESSAGE_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MONVOL(SMONVOL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, I, II, KK, TABI(11), NNS, NNTR, LENP, NPOLY, LENH, NPOLH,
     .        LENP_ANIM, NPOLH_ANIM, LENH_ANIM, NNS_ANIM, NN_L, NNA_L,
     .        NNSA_L, NSA, NELSA, NNI_L, IFV, ID_DT_OPTION, K1, ITYP, 
     .        ID_BAG, ID_BAG_INPUT, N
      INTEGER COLOR,KEY, IH3D_FLAG
      LOGICAL lFOUND
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C----------------------------------------------------------
C   CONSTANT INIT. : VISUALIZATION OF FVBAG RESULTS
C----------------------------------------------------------       
       !---AIRBAGS_TOTAL_FVM_IN_H3D
       AIRBAGS_TOTAL_FVM_IN_H3D = 0 !number of polyhedra to display for visualization subroutines (Sum FVDATA%NPOLH_ANIM)
       
C--------------------------------------
C     READING SIZES
C--------------------------------------
      LEN=1
      CALL READ_I_C(NFVBAG,LEN)
      ALLOCATE(FVDATA_OLD(NFVBAG))
      DO I=1,NFVBAG
        NULLIFY(FVDATA_OLD(I)%BRIC,FVDATA_OLD(I)%TBRIC,
     *          FVDATA_OLD(I)%XB,FVDATA_OLD(I)%SFAC,
     *          FVDATA_OLD(I)%IFVPOLY_ANIM,FVDATA_OLD(I)%IFVTADR_ANIM,
     *          FVDATA_OLD(I)%IFVPOLH_ANIM,FVDATA_OLD(I)%IFVPADR_ANIM,
     *          FVDATA_OLD(I)%REDIR_ANIM,FVDATA_OLD(I)%IFVTRI_ANIM,
     *          FVDATA_OLD(I)%NOD_ANIM,FVDATA_OLD(I)%IFVPOLY,
     *          FVDATA_OLD(I)%IFVTADR,FVDATA_OLD(I)%IFVPOLH,
     *          FVDATA_OLD(I)%IFVPADR,FVDATA_OLD(I)%IDPOLH,
     *          FVDATA_OLD(I)%IBPOLH,FVDATA_OLD(I)%IFVNOD,
     *          FVDATA_OLD(I)%IFVTRI,FVDATA_OLD(I)%RFVNOD,
     *          FVDATA_OLD(I)%MPOLH,FVDATA_OLD(I)%EPOLH,
     *          FVDATA_OLD(I)%PPOLH,FVDATA_OLD(I)%RPOLH,
     *          FVDATA_OLD(I)%GPOLH,FVDATA_OLD(I)%TPOLH,
     *          FVDATA_OLD(I)%CPAPOLH,
     *          FVDATA_OLD(I)%CPBPOLH,FVDATA_OLD(I)%CPCPOLH,
     *          FVDATA_OLD(I)%CPDPOLH,FVDATA_OLD(I)%CPEPOLH,
     *          FVDATA_OLD(I)%CPFPOLH,FVDATA_OLD(I)%DTPOLH,
     *          FVDATA_OLD(I)%RMWPOLH,FVDATA_OLD(I)%VPOLH_INI,
     *          FVDATA_OLD(I)%QPOLH,
     *          FVDATA_OLD(I)%SSPPOLH,     
     *          FVDATA_OLD(I)%CENTROID_POLH )

        FVDATA_OLD(I)%L_TYPE = 0
        FVDATA_OLD(I)%ID_DT_OPTION = -1
        FVDATA_OLD(I)%LAMBDA = ZERO
        FVDATA_OLD(I)%DTOLD = ZERO
        FVDATA_OLD(I)%CFL_COEF = ZERO
        FVDATA_OLD(I)%DTMIN = ZERO
        FVDATA_OLD(I)%PDISP_OLD = ZERO
        FVDATA_OLD(I)%PDISP = ZERO
      ENDDO

      IF (IMACH==3) THEN
         ALLOCATE(FVSPMD(NFVBAG))
         DO I=1,NFVBAG
           NULLIFY(FVSPMD(I)%IBUF_L)
           NULLIFY(FVSPMD(I)%IBUFA_L)
           NULLIFY(FVSPMD(I)%IBUFSA_L)
           NULLIFY(FVSPMD(I)%IXSA)
           NULLIFY(FVSPMD(I)%ELEMSA)
           NULLIFY(FVSPMD(I)%ITAB)
         ENDDO
      ENDIF
C
      
      ALLOCATE(KMESH(NFVBAG))
      DO I=1,NFVBAG
      
         CALL READ_I_C(FVDATA_OLD(I)%L_TYPE,1)
         CALL READ_I_C(FVDATA_OLD(I)%ID_DT_OPTION,1)
         CALL READ_DB(FVDATA_OLD(I)%LAMBDA,1)
         CALL READ_DB(FVDATA_OLD(I)%DTOLD,1)
         CALL READ_DB(FVDATA_OLD(I)%CFL_COEF,1)
         CALL READ_DB(FVDATA_OLD(I)%DTMIN,1)      
      
         IF (IMACH==3) THEN
            LEN=8
            CALL READ_I_C(TABI,LEN)
            NN_L=TABI(1)
            NNI_L=TABI(8)
            NNA_L=TABI(2)
            NNSA_L=TABI(3)
            NSA=TABI(4)
            NELSA=TABI(6)
            FVSPMD(I)%NN_L=NN_L
            FVSPMD(I)%NNI_L=NNI_L
            FVSPMD(I)%NNA_L=NNA_L
            FVSPMD(I)%NNSA_L=NNSA_L
            FVSPMD(I)%NSA=NSA
            FVSPMD(I)%NNSA=TABI(5)
            FVSPMD(I)%NELSA=NELSA
            FVSPMD(I)%PMAIN=TABI(7)
            FVSPMD(I)%NNA_L_GLOB = NNA_L

            COLOR = MIN(1,NN_L+NNI_L+NNA_L+NNSA_L) 
            IF(ISPMD == FVSPMD(I)%PMAIN - 1) THEN
            ! PMAIN will be zero in the subcommunicator
              KEY = 0 
              COLOR = 1
            ELSE
              KEY = 1
            ENDIF


            ! COLOR == 0 => ISPMD does not belong to the fvm
            ! COLOR == 1 => ISPMD belongs to the fvm
            FVSPMD(I)%MPI_COMM = 0
            FVSPMD(I)%RANK = 0
            FVSPMD(I)%NSPMD = 1
            CALL SPMD_COMM_SPLIT(COLOR,KEY,FVSPMD(I)%MPI_COMM,
     .                          FVSPMD(I)%RANK, FVSPMD(I)%NSPMD)


            ALLOCATE(FVSPMD(I)%ITAB(4,NSPMD-1))

            ! Compute the size of the comm. involved in SPMD_FVB_GATHER
            CALL SPMD_FVB_COMM_PATTERN(I)

            ALLOCATE(FVSPMD(I)%IBUF_L(2,NN_L+NNI_L))
            LEN=2*(NN_L+NNI_L)
            IF (LEN>0) THEN
              CALL READ_I_C(FVSPMD(I)%IBUF_L,LEN)
            ENDIF
            ALLOCATE(FVSPMD(I)%IBUFA_L(2,NNA_L))
            LEN=2*NNA_L
            IF (LEN>0) THEN
              CALL READ_I_C(FVSPMD(I)%IBUFA_L,LEN)
            ENDIF
            ALLOCATE(FVSPMD(I)%IBUFSA_L(2,NNSA_L))
            LEN=2*NNSA_L
            IF (LEN>0) THEN
              CALL READ_I_C(FVSPMD(I)%IBUFSA_L,LEN)
            ENDIF
            LEN = 1
            CALL READ_I_C(KMESH(I), LEN)
            IF (ISPMD/=FVSPMD(I)%PMAIN-1) CYCLE
C
            ALLOCATE(FVSPMD(I)%IXSA(8,NSA))
            LEN=8*NSA
            IF (LEN>0) THEN
              CALL READ_I_C(FVSPMD(I)%IXSA,LEN)
            ENDIF
            ALLOCATE(FVSPMD(I)%ELEMSA(3,NELSA))
            LEN=3*NELSA
            IF (LEN>0) THEN
              CALL READ_I_C(FVSPMD(I)%ELEMSA,LEN)
            ENDIF
         ENDIF
         LEN=11
         CALL READ_I_C(TABI,LEN)
C
         NNS=TABI(1)
         NNTR=TABI(2)
         LENP=TABI(3)
         NPOLY=TABI(4)
         LENH=TABI(5)
         NPOLH=TABI(6)
         LENP_ANIM=TABI(7)
         LENH_ANIM=TABI(8)
         NPOLH_ANIM=TABI(9)
         NNS_ANIM=TABI(10)
C
         FVDATA_OLD(I)%NNS=NNS
         FVDATA_OLD(I)%NNTR=NNTR
         FVDATA_OLD(I)%LENP=LENP
         FVDATA_OLD(I)%NPOLY=NPOLY
         FVDATA_OLD(I)%LENH=LENH
         FVDATA_OLD(I)%NPOLH=NPOLH
         FVDATA_OLD(I)%NPOLH_ANIM=NPOLH_ANIM
         FVDATA_OLD(I)%NNS_ANIM=NNS_ANIM
         FVDATA_OLD(I)%ID=TABI(11)
C
         ALLOCATE(FVDATA_OLD(I)%IFVNOD(3,NNS),
     .            FVDATA_OLD(I)%RFVNOD(2,NNS),
     .            FVDATA_OLD(I)%IFVTRI(6,NNTR),
     .            FVDATA_OLD(I)%IFVPOLY(LENP),
     .            FVDATA_OLD(I)%IFVTADR(NPOLY+1),
     .            FVDATA_OLD(I)%IFVPOLH(LENH),
     .            FVDATA_OLD(I)%IFVPADR(NPOLH+1),
     .            FVDATA_OLD(I)%IDPOLH(NPOLH),
     .            FVDATA_OLD(I)%IBPOLH(NPOLH),
     .            FVDATA_OLD(I)%MPOLH(NPOLH),
     .            FVDATA_OLD(I)%QPOLH(3,NPOLH),
     .            FVDATA_OLD(I)%EPOLH(NPOLH),
     .            FVDATA_OLD(I)%PPOLH(NPOLH),
     .            FVDATA_OLD(I)%RPOLH(NPOLH),
     .            FVDATA_OLD(I)%GPOLH(NPOLH),
     .            FVDATA_OLD(I)%TPOLH(NPOLH),
     .            FVDATA_OLD(I)%CPAPOLH(NPOLH),
     .            FVDATA_OLD(I)%CPBPOLH(NPOLH),
     .            FVDATA_OLD(I)%CPCPOLH(NPOLH),
     .            FVDATA_OLD(I)%CPDPOLH(NPOLH),
     .            FVDATA_OLD(I)%CPEPOLH(NPOLH),
     .            FVDATA_OLD(I)%CPFPOLH(NPOLH),
     .            FVDATA_OLD(I)%RMWPOLH(NPOLH),
     .            FVDATA_OLD(I)%VPOLH_INI(NPOLH),
     .            FVDATA_OLD(I)%DTPOLH(NPOLH),
     .            FVDATA_OLD(I)%SSPPOLH(NPOLH),     
     .            FVDATA_OLD(I)%CENTROID_POLH(3,NPOLH) )
     
         IF (NPOLH_ANIM>0)
     .      ALLOCATE(FVDATA_OLD(I)%IFVPOLY_ANIM(LENP_ANIM),
     .               FVDATA_OLD(I)%IFVTADR_ANIM(NPOLY+1),
     .               FVDATA_OLD(I)%IFVPOLH_ANIM(LENH_ANIM),
     .               FVDATA_OLD(I)%IFVPADR_ANIM(NPOLH_ANIM+1),
     .               FVDATA_OLD(I)%IFVTRI_ANIM(6,NNTR),
     .               FVDATA_OLD(I)%REDIR_ANIM(NNS_ANIM),
     .               FVDATA_OLD(I)%NOD_ANIM(3,NNS_ANIM) )


C--------------------------------------
C     READING INTEGERS
C--------------------------------------
         LEN=3*NNS
         CALL READ_I_C(FVDATA_OLD(I)%IFVNOD,LEN)
         LEN=6*NNTR
         CALL READ_I_C(FVDATA_OLD(I)%IFVTRI,LEN)
         CALL READ_I_C(FVDATA_OLD(I)%IFVPOLY,LENP)
         CALL READ_I_C(FVDATA_OLD(I)%IFVTADR,NPOLY+1)
         CALL READ_I_C(FVDATA_OLD(I)%IFVPOLH,LENH)
         CALL READ_I_C(FVDATA_OLD(I)%IFVPADR,NPOLH+1)
         CALL READ_I_C(FVDATA_OLD(I)%IDPOLH,NPOLH)
         CALL READ_I_C(FVDATA_OLD(I)%IBPOLH,NPOLH)
         IF (NPOLH_ANIM>0) THEN
           CALL READ_I_C(FVDATA_OLD(I)%IFVPOLY_ANIM,LENP_ANIM)
           CALL READ_I_C(FVDATA_OLD(I)%IFVTADR_ANIM,NPOLY+1)
           CALL READ_I_C(FVDATA_OLD(I)%IFVPOLH_ANIM,LENH_ANIM)
           CALL READ_I_C(FVDATA_OLD(I)%IFVPADR_ANIM,NPOLH_ANIM+1)
           LEN=6*NNTR
           CALL READ_I_C(FVDATA_OLD(I)%IFVTRI_ANIM,LEN)
           CALL READ_I_C(FVDATA_OLD(I)%REDIR_ANIM,NNS_ANIM)
         ENDIF
C--------------------------------------
C     READING REALS
C--------------------------------------
         LEN = 1
         CALL READ_DB(FVDATA_OLD(I)%PDISP_OLD,LEN)
         CALL READ_DB(FVDATA_OLD(I)%PDISP,LEN)
         LEN=2*NNS
         CALL READ_DB(FVDATA_OLD(I)%RFVNOD,LEN)
         CALL READ_DB(FVDATA_OLD(I)%MPOLH,NPOLH)
         LEN=3*NPOLH
         CALL READ_DB(FVDATA_OLD(I)%QPOLH,LEN)
         CALL READ_DB(FVDATA_OLD(I)%EPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%PPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%RPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%GPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%TPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%CPAPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%CPBPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%CPCPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%CPDPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%CPEPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%CPFPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%RMWPOLH,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%VPOLH_INI,NPOLH)
         CALL READ_DB(FVDATA_OLD(I)%DLH,1)
         CALL READ_DB(FVDATA_OLD(I)%DTPOLH,NPOLH)
         IF (NPOLH_ANIM>0) THEN
            LEN=3*NNS_ANIM
            CALL READ_DB(FVDATA_OLD(I)%NOD_ANIM,LEN)
         ENDIF
         !FVDATA_OLD(I)%SSPPOLH       not read : H3D only. Generated if needed.
         !FVDATA_OLD(I)%CENTROID_POLH not read : H3D only. Generated if needed.
C
         K1 = 1
         IH3D_FLAG = 0
         !Looping over all existing bags to check which one is matching : collect IHED_FLAG
         DO N=1,NVOLU                                                          
           IFV = MONVOL(K1-1 + 45)         
           IF(IFV==I)THEN                  
             IH3D_FLAG = MONVOL(K1-1 + 75)
             IF(IH3D_FLAG==1)THEN
               AIRBAGS_TOTAL_FVM_IN_H3D = AIRBAGS_TOTAL_FVM_IN_H3D + NPOLH_ANIM
             ENDIF 
             EXIT                          
           ENDIF                                                                                                       
           K1=K1+NIMV                                                           
         ENDDO !next N  
           
C
      ENDDO !next FVBAG
      

C--------------------------------------
C     /MONVOL/FVMBAG1(typ=8)
C       /DT/FVMBAG/1 if relevant for ITYP8 only
C--------------------------------------
C FVMBAG_INPUT_OPTIONS is a data structure from FREFORM (Engine Reader) which contains data from /DT/FVMBAG options
C  these parameters are treated once all options are read and after initialization of parameters from restart file
      !looping over all /DT/FVMBAG options                                        
      DO II=NUM_OPT_DT_FVMBAG,1,-1                                                
        ID_DT_OPTION = FVMBAG_INPUT_OPTIONS(II)%ID_DT_OPTION                      
        ! /DT/FVMBAG/2 otherwise next cycle                                       
        IF(ID_DT_OPTION==1)THEN                            
          K1=1                                                                    
            !Looping over all existing bags to check if one id is matching        
            DO N=1,NVOLU                                                          
              ID_BAG=MONVOL(K1-1+1)                                               
              ITYP=MONVOL(K1-1+2)                                                 
              IF (ITYP == 8) THEN                                                 
                IFV = MONVOL(K1 -1 +45)                                           
                !IF(FVDATA_OLD(IFV)%ID_DT_OPTION == 1)THEN                        
                  FVDATA_OLD(IFV)%L_TYPE = -1                                     
                  FVDATA_OLD(IFV)%ID_DT_OPTION = 1                     
                  FVDATA_OLD(IFV)%LAMBDA = ZERO                                   
                  FVDATA_OLD(IFV)%CFL_COEF = FVMBAG_INPUT_OPTIONS(II)%CFL_COEF    
                  FVDATA_OLD(IFV)%DTMIN = FVMBAG_INPUT_OPTIONS(II)%DTMIN          
                !ENDIF                                                             
              ENDIF                                                               
              K1=K1+NIMV                                                          
            ENDDO !next N                                                         
        ENDIF !ID_DT_OPTION                                                       
      ENDDO!next II  
          
C--------------------------------------
C     /MONVOL/FVMBAG1(typ=8) & /MONVOL/FVMBAG2(typ=11)
C       UPDATING DATA WITH ENGINE FILE PARAMETERS(/DT/FVMBAG/2 )
C--------------------------------------
      ! FILL FVDATA BUFFER (NOW ALLOCATED) WITH PARAMETERS READ IN ENGINE FILE (/DT/FVMBAG OPTION)      
        !looping over all /DT/FVMBAG options
      DO II=1,NUM_OPT_DT_FVMBAG                                                         
        ID_DT_OPTION = FVMBAG_INPUT_OPTIONS(II)%ID_DT_OPTION                            
        ! /DT/FVMBAG/2 otherwise next cycle                                             
        IF(ID_DT_OPTION == 2)THEN                                                       
          K1=1                                                                          
          ID_BAG_INPUT = FVMBAG_INPUT_OPTIONS(II)%ID_BAG                                
          lFOUND = .FALSE.                                                              
          IF(ID_BAG_INPUT > 0)THEN                                                      
            !Looping over all existing bags to check if one id is matching              
            DO N=1,NVOLU                                                                
              ID_BAG=MONVOL(K1-1+1)                                                     
              ITYP=MONVOL(K1-1+2)                                                       
              IF(ID_BAG == ID_BAG_INPUT)THEN                                            
                IFV = MONVOL(K1 -1 +45)                                                 
                IF (ITYP == 8 .OR. ITYP == 11) THEN                                                     
                  lFOUND=.TRUE.                                                         
                  FVDATA_OLD(IFV)%L_TYPE = FVMBAG_INPUT_OPTIONS(II)%L_TYPE              
                  FVDATA_OLD(IFV)%ID_DT_OPTION = FVMBAG_INPUT_OPTIONS(II)%ID_DT_OPTION  
                  FVDATA_OLD(IFV)%LAMBDA = FVMBAG_INPUT_OPTIONS(II)%LAMBDA              
                  FVDATA_OLD(IFV)%CFL_COEF = FVMBAG_INPUT_OPTIONS(II)%CFL_COEF          
                  FVDATA_OLD(IFV)%DTMIN = FVMBAG_INPUT_OPTIONS(II)%DTMIN                
                ENDIF                                                                   
              ENDIF                                                                     
              K1=K1+NIMV                                                                
              IF(lFOUND)EXIT                                                            
            ENDDO !next N                                                               
            IF(.NOT.lFOUND)THEN                                                         
              CALL ANCMSG(MSGID=299,ANMODE=ANINFO,I1=ID_BAG_INPUT)                      
              CALL ARRET(2)                                                             
            ENDIF                                                                       
          ENDIF !(ID_BAG_INPUT > 0)                                                     
        ENDIF !IF(ID_DT_OPTION == 2)                                                    
      ENDDO!next II                                                                     


                                                             
                                                                  
      
C--------------------------------------
C     /MONVOL/FVMBAG1(typ=6) & /MONVOL/FVMBAG1(typ=8) & /MONVOL/FVMBAG2(typ=11)
C       DEFAULT
C--------------------------------------
      K1=1                                                                                   
      DO N=1,NVOLU                                                                
        ID_BAG=MONVOL(K1-1+1)                                                     
        ITYP=MONVOL(K1-1+2)                                                       
        IF (ITYP == 6 .OR. ITYP == 8 .OR. ITYP == 11) THEN                                             
          IFV = MONVOL(K1 -1 +45)                                       
          IF(FVDATA_OLD(IFV)%CFL_COEF == ZERO)THEN                      
            FVDATA_OLD(IFV)%CFL_COEF = ZEP9                             
          ENDIF                                                         
          IF(FVDATA_OLD(IFV)%DTMIN == ZERO)THEN                         
            FVDATA_OLD(IFV)%DTMIN = EM20                                
          ENDIF    
          IF(FVDATA_OLD(IFV)%LAMBDA == ZERO)THEN                         
            FVDATA_OLD(IFV)%LAMBDA = EM02                                
          ENDIF    
          IF(FVDATA_OLD(IFV)%L_TYPE == 0)THEN                         
            FVDATA_OLD(IFV)%L_TYPE = 1                                
          ENDIF                                                                      
        ENDIF                                                                     
        K1=K1+NIMV                                                                
      ENDDO !next N                                                               

C--------------------------------------
C     CHECK INCOMPATIBLE TIME STEP OPTION /DT/FVMBAG
C       before deallocating FVMBAG_INPUT_OPTIONS
C--------------------------------------
      !Warn user if incompatible options are defined 
      IF( NUM_OPT_DT_FVMBAG_0+NUM_OPT_DT_FVMBAG_1 > 0 .AND. NUM_OPT_DT_FVMBAG_2 > 0)THEN 
         ! if /DT/FVMBAG/2 is used on a given airbag, then general option /DT/FVMBAG/1 is ignored for this airbag.
         DO KK=1,NUM_OPT_DT_FVMBAG
          !display WARNING DUPLICATED DEFINITION 
          IF(FVMBAG_INPUT_OPTIONS(KK)%ID_DT_OPTION == 2)THEN
            ID_BAG = FVMBAG_INPUT_OPTIONS(KK)%ID_BAG
            CALL ANCMSG(MSGID=302,ANMODE=ANINFO,I1=ID_BAG)
          ENDIF
         ENDDO                                                                                              
      ENDIF
      
      !Warn user if incompatible options are defined 
      IF( NUM_OPT_DT_FVMBAG_1 > 1 )THEN 
         ! if /DT/FVMBAG/2 is used on a given airbag, then general option /DT/FVMBAG/1 is ignored for this airbag.      
            CALL ANCMSG(MSGID=303,ANMODE=ANINFO,I1=ID_BAG)
            CALL ARRET(2)
      ENDIF      

C--------------------------------------
      IF(ALLOCATED(FVMBAG_INPUT_OPTIONS))DEALLOCATE(FVMBAG_INPUT_OPTIONS)
C--------------------------------------
      

      RETURN
      END

C shell heat transfer
Chd|====================================================================
Chd|  THCRREST                      source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE THCRREST(MCP, TEMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        MCP(*),TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (MCP,NUMNOD,IRFORM)
        CALL REDSQR (TEMP,NUMNOD,IRFORM)
      ELSE
        CALL READ_DB(MCP,NUMNOD)
        CALL READ_DB(TEMP,NUMNOD)
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  NITSCHERREST                  source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE NITSCHERREST(FORNEQS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        FORNEQS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (FORNEQS,3*NUMNOD,IRFORM)
      ELSE
        CALL READ_DB(FORNEQS,3*NUMNOD)
      ENDIF
C
      RETURN
      END
C
C convection flux
Chd|====================================================================
Chd|  CONVRREST                     source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE CONVRREST(IBCV, FCONV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCV(*)
      my_real
     .        FCONV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (FCONV,LFACTHER*NUMCONV,IRFORM)
        CALL REDSQI (IBCV,NICONV*NUMCONV,IRFORM)
      ELSE
        CALL READ_DB(FCONV,LFACTHER*NUMCONV)
        CALL READ_I_C(IBCV,NICONV*NUMCONV)
      ENDIF
C
      RETURN
      END
C radiative flux
Chd|====================================================================
Chd|  RADIARREST                    source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE RADIARREST(IBCR, FRADIA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCR(*)
      my_real
     .        FRADIA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (FRADIA,LFACTHER*NUMRADIA,IRFORM)
        CALL REDSQI (IBCR,NIRADIA*NUMRADIA,IRFORM)
      ELSE
        CALL READ_DB(FRADIA,LFACTHER*NUMRADIA)
        CALL READ_I_C(IBCR,NIRADIA*NUMRADIA)
      ENDIF
C
      RETURN
      END
C imposed temperature
Chd|====================================================================
Chd|  FXTEMPRREST                   source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE FXTEMPRREST(IBFTEMP, FBFTEMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFTEMP(*)
      my_real
     .        FBFTEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (FBFTEMP,LFACTHER*NFXTEMP,IRFORM)
        CALL REDSQI (IBFTEMP,NIFT*NFXTEMP,IRFORM)
      ELSE
        CALL READ_DB(FBFTEMP,LFACTHER*NFXTEMP)
        CALL READ_I_C(IBFTEMP,NIFT*NFXTEMP)
      ENDIF
C
      RETURN
      END
C rigid material
Chd|====================================================================
Chd|  RMATRREST                     source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        REDSQI                        source/output/tools/redsqi.F  
Chd|        REDSQR                        source/output/tools/redsqr.F  
Chd|====================================================================
      SUBROUTINE RMATRREST(RBYM,IRBYM, LNRBYM,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBYM(*), LNRBYM(*),WEIGHT(*)
      my_real
     .        RBYM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (RBYM,NRBYM*NFRBYM,IRFORM)
        CALL REDSQI(IRBYM,NRBYM*NIRBYM ,IRFORM)
        CALL REDSQI(LNRBYM,NGSLNRBYM,IRFORM)
        CALL REDSQI(WEIGHT,NRBYM,IRFORM)
      ELSE
        CALL READ_DB(RBYM,NRBYM*NFRBYM)
        CALL READ_I_C(IRBYM,NRBYM*NIRBYM )
        CALL READ_I_C(LNRBYM,NGSLNRBYM )
        CALL READ_I_C(WEIGHT,NRBYM )
      ENDIF
C
      RETURN
      END
C shell xfem
Chd|====================================================================
Chd|  PLYXFEM_RREST                 source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE PLYXFEM_RREST(MS_LAYER,ZI_LAYER,INOD,IEL,ICODE,ISKEW,
     .                         MSZ2)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        MS_LAYER(*),ZI_LAYER(*),MSZ2(*)
      INTEGER
     .   ICODE(*),ISKEW(*),INOD(*),IEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
         CALL READ_DB(MS_LAYER,NPLYXFE*NPLYMAX)
         CALL READ_DB(ZI_LAYER,NPLYXFE*NPLYMAX)
         CALL READ_DB(MSZ2,NPLYXFE)
C
         CALL READ_I_C(INOD,NUMNOD)
         CALL READ_I_C(IEL,NUMELC)
C BCS ply
         IF(IPLYBCS > 0) THEN
            CALL READ_I_C(ICODE,NUMNOD)
            CALL READ_I_C(ISKEW,NUMNOD)
         ENDIF
      RETURN
      END
C shell xfem
Chd|====================================================================
Chd|  PLYXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLYXFEM_RRESTANIM()
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,ELSZ,NDSZ
C-----------------------------------------------
      IF(IPLYXFEM == 0) THEN
! Those array be used as dummy argument
! they MUST be allocated in all cases
        ALLOCATE(INDX_PLY(0))
        ALLOCATE(PLYSHELL(0))
        ALLOCATE(PLYNOD(0))
      ELSE
        CALL READ_I_C(NPLYPART,1)
        ALLOCATE(INDX_PLY(NPLYPART))
        CALL READ_I_C(INDX_PLY,NPLYPART)
       
        ALLOCATE(PLYSHELL(NPLYMAX))
        ALLOCATE(PLYNOD(NPLYMAX))
       
        DO K=1,NPLYMAX
          CALL READ_I_C(ELSZ,1)
          PLYSHELL(K)%PLYNUMSHELL=ELSZ
          ALLOCATE(PLYSHELL(K)%SHID(ELSZ))
          ALLOCATE(PLYSHELL(K)%SHELLIPT(ELSZ))
          ALLOCATE(PLYSHELL(K)%SHELLID(ELSZ))
       
          CALL READ_I_C(PLYSHELL(K)%SHID,     ELSZ)
          CALL READ_I_C(PLYSHELL(K)%SHELLIPT, ELSZ)
          CALL READ_I_C(PLYSHELL(K)%SHELLID,  ELSZ)
       
       
          CALL READ_I_C(NDSZ,1)
          PLYNOD(K)%PLYNUMNODS=NDSZ
          ALLOCATE(PLYNOD(K)%NODES(NDSZ))
          ALLOCATE(PLYNOD(K)%PLYNODID(NDSZ))
       
          CALL READ_I_C(PLYNOD(K)%NODES,    NDSZ)
          CALL READ_I_C(PLYNOD(K)%PLYNODID, NDSZ)
        ENDDO
       
        ALLOCATE(PLYSIZG(NPLYMAX))
        CALL READ_I_C(PLYSIZG,NPLYMAX)
C      
        ALLOCATE (IDPID_PLY(NPLYPART))
        CALL READ_I_C(IDPID_PLY,NPLYPART)
      ENDIF
      RETURN
      END
C centrifugal force
Chd|====================================================================
Chd|  CFIELDREST                    source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CFIELDREST(CFIELD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        CFIELD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(CFIELD,LFACLOAD*NLOADC)
C
      RETURN
      END
Chd|====================================================================
Chd|  LOADPREST                     source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LOADPREST(LOADP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real LOADP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(LOADP,LFACLOAD*NLOADP)
C
      RETURN
      END

Chd|====================================================================
Chd|  CRKXFEM_RREST                 source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE CRKXFEM_RREST(INOD_CRKXFEM,IEL_CRKXFEM,NODLEVXF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INOD_CRKXFEM(*),IEL_CRKXFEM(*),NODLEVXF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C=======================================================================
      CALL READ_I_C(INOD_CRKXFEM,NUMNOD)
      CALL READ_I_C(IEL_CRKXFEM,NUMELC+NUMELTG)
      CALL READ_I_C(NODLEVXF,NCRKXFE)
C---
      RETURN
      END
Chd|====================================================================
Chd|  CRKXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        XFEM2VARS_MOD                 share/modules/xfem2vars_mod.F 
Chd|====================================================================
      SUBROUTINE CRKXFEM_RRESTANIM()
C-----------------------------------------------
      USE XFEM2VARS_MOD
      USE RESTMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,LEN,ELSZ,NENR,ELSZC,ELSZTG,NCRKXFE_G,
     .        CRKNUMNODS
C=======================================================================
      IF(ICRACK3D == 0) THEN
        ALLOCATE(INDX_CRK(0))
        ALLOCATE(CRKSHELL(0))
        ALLOCATE(CRKNOD(0))
        ALLOCATE(CRKLVSET(0))
        ALLOCATE(CRKAVX(0))
        ALLOCATE(CRKSKY(0))
        ALLOCATE(XFEM_PHANTOM(0))
        ALLOCATE(CRKEDGE(0))
        ALLOCATE(XEDGE4N(4,0))
        ALLOCATE(XEDGE3N(3,0))
      ELSE
        CALL READ_I_C(NCRKPART,1)
        CALL READ_I_C(NCRKXFE_G,1)
        ALLOCATE(INDX_CRK(NCRKPART))
        CALL READ_I_C(INDX_CRK,NCRKPART)
C      
        ALLOCATE(CRKSHELL(NLEVMAX))
        ALLOCATE(CRKNOD(NLEVMAX))
        ALLOCATE(CRKLVSET(NLEVMAX))
        ALLOCATE(CRKAVX(NLEVMAX))
        ALLOCATE(CRKSKY(NLEVMAX))
C      
        DO K=1,NLEVMAX
          CALL READ_I_C(ELSZC  ,1)
          CALL READ_I_C(ELSZTG ,1)
          CALL READ_I_C(ELSZ   ,1)
c      
c-------- CRKSHELL
          CRKSHELL(K)%CRKNUMSHELL = ELSZ
          CRKSHELL(K)%CRKNUMSH4   = ELSZC
          CRKSHELL(K)%CRKNUMSH3   = ELSZTG
C      
          ALLOCATE(CRKSHELL(K)%CRKSHELLID(ELSZ))
          ALLOCATE(CRKSHELL(K)%ELTYPE(ELSZ))
          CALL READ_I_C(CRKSHELL(K)%CRKSHELLID  ,ELSZ)
          CALL READ_I_C(CRKSHELL(K)%ELTYPE      ,ELSZ)
c-------- CRKNOD
          LEN = 4*ELSZ
          ALLOCATE(CRKNOD(K)%XFECRKNODID(LEN))
          ALLOCATE(CRKNOD(K)%XFENODES(LEN))
          CALL READ_I_C(CRKNOD(K)%XFECRKNODID ,LEN)
          CALL READ_I_C(CRKNOD(K)%XFENODES    ,LEN)
c-------- CRKSHELL
          LEN = 4*ELSZ
          ALLOCATE(CRKSHELL(K)%XNODEL(4  ,ELSZ))
          CALL READ_I_C(CRKSHELL(K)%XNODEL,  LEN)
c-------- CRKLVSET
          LEN = LCNECRKXFEM
          ALLOCATE(CRKLVSET(K)%ENR0(2,LEN))
          ALLOCATE(CRKLVSET(K)%AREA(ELSZ))
          CALL READ_I_C(CRKLVSET(K)%ENR0(1,1:LEN) ,LEN)
          CALL READ_I_C(CRKLVSET(K)%ENR0(2,1:LEN) ,LEN)
          CALL READ_DB (CRKLVSET(K)%AREA          ,ELSZ)
c-------- CRKAVX
          LEN = LCNECRKXFEM
          ALLOCATE(CRKAVX(K)%A(3,LEN))
          ALLOCATE(CRKAVX(K)%AR(3,LEN))
          ALLOCATE(CRKAVX(K)%V(3,LEN))
          ALLOCATE(CRKAVX(K)%VR(3,LEN))
          ALLOCATE(CRKAVX(K)%X(3,LEN))
          ALLOCATE(CRKAVX(K)%U(3,LEN))
          LEN = LCNECRKXFEM*3
          CALL READ_DB(CRKAVX(K)%A   ,LEN)
          CALL READ_DB(CRKAVX(K)%AR  ,LEN)
          CALL READ_DB(CRKAVX(K)%V   ,LEN)
          CALL READ_DB(CRKAVX(K)%VR  ,LEN)
          CALL READ_DB(CRKAVX(K)%X   ,LEN)
          CALL READ_DB(CRKAVX(K)%U   ,LEN)
          LEN = 4*ELSZ
          ALLOCATE(CRKAVX(K)%XX(3,LEN))
          CALL READ_DB(CRKAVX(K)%XX,  3*LEN)
c-------- CRKSKY
          ALLOCATE(CRKSKY(K)%FSKY(8,LCNECRKXFEM))
          CALL READ_DB(CRKSKY(K)%FSKY,  8*LCNECRKXFEM)
c-------- CRKNOD
          LEN = 4*ELSZ
          ALLOCATE(CRKNOD(K)%NOD2IAD(LEN))
          CALL READ_I_C(CRKNOD(K)%NOD2IAD,  LEN)
          CALL READ_I_C(CRKNUMNODS, 1)
          CRKNOD(K)%CRKNUMNODS = CRKNUMNODS
c      
        ENDDO ! K=1,NLEVMAX
c      
c---    read XFEM_PHANTOM  ----------------------------
c      
       
        NENR = INT(IENRNOD/NLEVMAX)
        ALLOCATE(XFEM_PHANTOM(NXLAYMAX))
        DO K=1,NXLAYMAX
          ALLOCATE(XFEM_PHANTOM(K)%ELCUT(ELSZ))
          ALLOCATE(XFEM_PHANTOM(K)%IFI(LCNECRKXFEM))
          ALLOCATE(XFEM_PHANTOM(K)%TAGXP(5,NCRKXFE,IENRNOD))  ! IENRNOD -> NENR
          ALLOCATE(XFEM_PHANTOM(K)%ITRI(2,ELSZ))
c      
          CALL READ_I_C(XFEM_PHANTOM(K)%ELCUT    ,ELSZ)
          CALL READ_I_C(XFEM_PHANTOM(K)%IFI      ,LCNECRKXFEM)
          CALL READ_I_C(XFEM_PHANTOM(K)%TAGXP    ,NCRKXFE*IENRNOD*5)
          CALL READ_I_C(XFEM_PHANTOM(K)%ITRI     ,ELSZ*2)
        END DO
c      
c---    read CRKEDGE  ----------------------------
c      
        LEN = NUMEDGES
        ALLOCATE(CRKEDGE(NXLAYMAX))
        DO K=1,NXLAYMAX
          ALLOCATE(CRKEDGE(K)%LAYCUT(ELSZ))
          ALLOCATE(CRKEDGE(K)%IEDGEC(4,ELSZC))
          ALLOCATE(CRKEDGE(K)%IEDGETG(3,ELSZTG))
          ALLOCATE(CRKEDGE(K)%EDGEICRK(NUMEDGES))
          ALLOCATE(CRKEDGE(K)%EDGEIFI(2,NUMEDGES))
          ALLOCATE(CRKEDGE(K)%EDGEENR(2,NUMEDGES))
          ALLOCATE(CRKEDGE(K)%EDGETIP(2,NUMEDGES))
          ALLOCATE(CRKEDGE(K)%IBORDEDGE(NUMEDGES))
          ALLOCATE(CRKEDGE(K)%ICUTEDGE(NUMEDGES))
          ALLOCATE(CRKEDGE(K)%RATIO(NUMEDGES))
c      
          CALL READ_I_C(CRKEDGE(K)%LAYCUT     ,ELSZ)
          CALL READ_I_C(CRKEDGE(K)%IEDGEC     ,ELSZC*4)
          CALL READ_I_C(CRKEDGE(K)%IEDGETG    ,ELSZTG*3)
          CALL READ_I_C(CRKEDGE(K)%EDGEICRK   ,NUMEDGES)
          CALL READ_I_C(CRKEDGE(K)%EDGEIFI    ,NUMEDGES*2)
          CALL READ_I_C(CRKEDGE(K)%EDGEENR    ,NUMEDGES*2)
          CALL READ_I_C(CRKEDGE(K)%EDGETIP    ,NUMEDGES*2)
          CALL READ_I_C(CRKEDGE(K)%IBORDEDGE  ,NUMEDGES)
          CALL READ_I_C(CRKEDGE(K)%ICUTEDGE   ,NUMEDGES)
          CALL READ_DB (CRKEDGE(K)%RATIO      ,NUMEDGES)
        END DO
c-------------------------------
        ALLOCATE(XEDGE4N(4,ELSZC))
        ALLOCATE(XEDGE3N(3,ELSZTG))
        CALL READ_I_C(XEDGE4N, 4*ELSZC)
        CALL READ_I_C(XEDGE3N, 3*ELSZTG)
c-------------------------------
        LEN = 2*(NUMELC+NUMELTG)
        CALL READ_I_C(ELCUTC,LEN)
        LEN = NCRKXFE
        CALL READ_I_C(NODENR,LEN)
        LEN = NCRKXFE
        CALL READ_I_C(KXFENOD2ELC,LEN)
        LEN = NUMNOD*IENRNOD
        CALL READ_I_C(ENRTAG,LEN)
C--- 
      ENDIF
      RETURN
      END

C shell xfem
Chd|====================================================================
Chd|  PLYXFEM_RAVUPLY               source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLYXFEM_RAVUPLY()
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NOSZ
C-----------------------------------------------
      ALLOCATE(PLY(NPLYMAX))
      DO K=1,NPLYMAX
        NULLIFY(PLY(K)%V,PLY(K)%U )
        ALLOCATE(PLY(K)%V(3,NPLYXFE), PLY(K)%U(3,NPLYXFE))
        NOSZ = 3*NPLYXFE
        CALL READ_DB(PLY(K)%U ,NOSZ)
        CALL READ_DB(PLY(K)%V ,NOSZ)
      ENDDO
C
      RETURN
      END
C air + faom
Chd|====================================================================
Chd|  ALELAG_RREST                  source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        ALEFLOW_MOD                   share/modules/aleflow_mod.F   
Chd|====================================================================
      SUBROUTINE ALELAG_RREST()
C-----------------------------------------------
      USE ALEFLOW_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NOSZ
C-----------------------------------------------
        ALLOCATE (VFLOW(3*NUMNOD), DFLOW(3*NUMNOD),WFLOW(3*NUMNOD))
        NOSZ = 3*NUMNOD
        CALL READ_DB(DFLOW, NOSZ)
        CALL READ_DB(VFLOW ,NOSZ)
        CALL READ_DB(WFLOW ,NOSZ)
C
      RETURN
      END
Chd|====================================================================
Chd|  RTHBUFREST                    source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RTHBUFREST(RTHBUF,SRTHBUF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        RTHBUF(*)
      INTEGER
     .        SRTHBUF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------

C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(RTHBUF,SRTHBUF)
C
      RETURN
      END

Chd|====================================================================
Chd|  KNOTREST                      source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE KNOTREST(KNOT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        KNOT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(KNOT,SKNOT)
C
      RETURN
      END
Chd|====================================================================
Chd|  KNOTLOCPCREST                 source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE KNOTLOCPCREST(KNOTLOCPC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        KNOTLOCPC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(KNOTLOCPC,SKNOTLOCPC)
C
      RETURN
      END
Chd|====================================================================
Chd|  KNOTLOCELREST                 source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE KNOTLOCELREST(KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real KNOTLOCEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(KNOTLOCEL,SKNOTLOCEL)
C
      RETURN
      END
Chd|====================================================================
Chd|  WIGEREST                      source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE WIGEREST(WIGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real WIGE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
C--------------------------------------
C     READING REALS
C--------------------------------------
      CALL READ_DB(WIGE,NUMNOD)
C
      RETURN
      END
C STACk PART
Chd|====================================================================
Chd|  STACK_RREST                   source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE STACK_RREST(IGEO,GEO,PM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(*)
      my_real GEO(*),PM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LSIZ

C-----------------------------------------------
        LSIZ =(4*NPT_STACK +2)*NS_STACK
        CALL READ_I_C(IGEO,     LSIZ)
        LSIZ =(6*NPT_STACK + 1)*NS_STACK
        CALL READ_DB(GEO,     LSIZ)
        LSIZ =20*NS_STACK
        CALL READ_DB(PM,     LSIZ)
       RETURN
      END
Chd|====================================================================
Chd|  DRAPE_RREST                   source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|====================================================================
      SUBROUTINE DRAPE_RREST(DRAPE_SH4N,DRAPE_SH3N,DRAPEG)
C
        USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(DRAPE_),   DIMENSION(*) ,TARGET       :: DRAPE_SH4N,DRAPE_SH3N
        TYPE(DRAPEG_)                              :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN,OFF,II,J,IS,NSLICE,NPT,NDIM_INDX_PLY,NDIM,NDIM_SLICE,
     .        NPT_DRAPE
      my_real, DIMENSION(:,:), ALLOCATABLE :: RTMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITMP
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPTDRP_ELT,ISLICE,INDX_PLY,
     .                                        NPT_ELT
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
C     4 node Shell 
      SCDRAPE = 0
      STDRAPE = 0
      IF(NUMELC_DRAPE > 0) THEN
        CALL READ_I_C(NDIM ,1)
        CALL READ_I_C(NDIM_SLICE,1)
        CALL READ_I_C(NDIM_INDX_PLY,1)
        DRAPEG%NDIM_SH4N          = NDIM 
        DRAPEG%NDIM_SLICE_SH4N    = NDIM_SLICE 
        DRAPEG%NDIM_INDX_PLY_SH4N = NDIM_INDX_PLY
        !
        ALLOCATE(NPT_ELT(NUMELC_DRAPE),ITMP(NDIM,2),RTMP(NDIM,2),
     .         NPTDRP_ELT(NUMELC_DRAPE),ISLICE(NDIM_SLICE),INDX_PLY(NDIM_INDX_PLY))
        CALL READ_I_C(DRAPEG%INDX_SH4N,NUMELC)
        CALL READ_I_C(NPTDRP_ELT,NUMELC_DRAPE)
        CALL READ_I_C(NPT_ELT,NUMELC_DRAPE)
        CALL READ_I_C(ISLICE,NDIM_SLICE )
        CALL READ_I_C(INDX_PLY,NDIM_INDX_PLY)  
        CALL READ_I_C(ITMP,2*NDIM)            
        CALL READ_DB(RTMP,2*NDIM)
        NDIM_INDX_PLY = 0
        NDIM_SLICE = 0
        NDIM = 0
        DO II= 1,NUMELC_DRAPE
           DRAPE_SH4N(II)%NPLY_DRAPE = NPTDRP_ELT(II)
           DRAPE_SH4N(II)%NPLY       = NPT_ELT(II)
           NPT = DRAPE_SH4N(II)%NPLY 
           ALLOCATE(DRAPE_SH4N(II)%INDX_PLY(NPT))
           DRAPE_SH4N(II)%INDX_PLY = 0
           DO J = 1,NPT
              DRAPE_SH4N(II)%INDX_PLY(J)=INDX_PLY(NDIM_INDX_PLY + J) 
           ENDDO
           NDIM_INDX_PLY = NDIM_INDX_PLY + NPT
           NPT_DRAPE = DRAPE_SH4N(II)%NPLY_DRAPE
           ALLOCATE(DRAPE_SH4N(II)%DRAPE_PLY(NPT_DRAPE))
           DO J=1 ,NPT_DRAPE
             NSLICE = ISLICE(NDIM_SLICE + J)
             DRAPE_SH4N(II)%DRAPE_PLY(J)%NSLICE = NSLICE 
             ALLOCATE(DRAPE_SH4N(II)%DRAPE_PLY(J)%IDRAPE(NSLICE,2),DRAPE_SH4N(II)%DRAPE_PLY(J)%RDRAPE(NSLICE,2))
             DRAPE_SH4N(II)%DRAPE_PLY(J)%IDRAPE = 0
             DRAPE_SH4N(II)%DRAPE_PLY(J)%RDRAPE = ZERO
             DO  IS= 1,NSLICE  
                 DRAPE_SH4N(II)%DRAPE_PLY(J)%IDRAPE(IS,1) = ITMP(NDIM + IS,1)
                 DRAPE_SH4N(II)%DRAPE_PLY(J)%IDRAPE(IS,2) = ITMP(NDIM + IS,2) 
                 DRAPE_SH4N(II)%DRAPE_PLY(J)%RDRAPE(IS,1) = RTMP(NDIM + IS,1) 
                 DRAPE_SH4N(II)%DRAPE_PLY(J)%RDRAPE(IS,2) = RTMP(NDIM + IS,2)
             ENDDO             
             NDIM = NDIM + NSLICE 
           ENDDO
           NDIM_SLICE = NDIM_SLICE + NPT_DRAPE
        ENDDO ! NUMELC_DRAPE
        DEALLOCATE(NPT_ELT,ITMP,RTMP,NPTDRP_ELT,ISLICE,INDX_PLY)
        SCDRAPE = NUMELC
      ENDIF
C
C     3 Node Shell 
      IF(NUMELTG_DRAPE > 0) THEN
        CALL READ_I_C(NDIM ,1)
        CALL READ_I_C(NDIM_SLICE,1)
        Call READ_I_C(NDIM_INDX_PLY,1)
        DRAPEG%NDIM_SH3N          = NDIM 
        DRAPEG%NDIM_SLICE_SH3N    = NDIM_SLICE 
        DRAPEG%NDIM_INDX_PLY_SH3N = NDIM_INDX_PLY
        ALLOCATE( NPT_ELT(NUMELTG_DRAPE),ITMP(NDIM,2),RTMP(NDIM,2),
     .         NPTDRP_ELT(NUMELTG_DRAPE),ISLICE(NDIM_SLICE),INDX_PLY(NDIM_INDX_PLY))
        CALL READ_I_C(DRAPEG%INDX_SH3N,NUMELTG)
        CALL READ_I_C(NPTDRP_ELT,NUMELTG_DRAPE)
        CALL READ_I_C(NPT_ELT,NUMELTG_DRAPE)
        CALL READ_I_C(ISLICE,NDIM_SLICE)
        CALL READ_I_C(INDX_PLY,NDIM_INDX_PLY)  
        CALL READ_I_C(ITMP,2*NDIM)            
        CALL READ_DB(RTMP,2*NDIM)
        NDIM_INDX_PLY = 0
        NDIM_SLICE = 0
        NDIM = 0
        DO II= 1,NUMELTG_DRAPE
           DRAPE_SH3N(II)%NPLY_DRAPE = NPTDRP_ELT(II)
           DRAPE_SH3N(II)%NPLY       = NPT_ELT(II)
           NPT = DRAPE_SH3N(II)%NPLY 
           ALLOCATE(DRAPE_SH3N(II)%INDX_PLY(NPT))
           DRAPE_SH3N(II)%INDX_PLY = 0
           DO J = 1,NPT
              DRAPE_SH3N(II)%INDX_PLY(J)=INDX_PLY(NDIM_INDX_PLY + J) 
           ENDDO
           NDIM_INDX_PLY = NDIM_INDX_PLY + NPT
           NPT_DRAPE = DRAPE_SH3N(II)%NPLY_DRAPE
           ALLOCATE(DRAPE_SH3N(II)%DRAPE_PLY(NPT_DRAPE))
           DO J = 1,NPT_DRAPE
             NSLICE = ISLICE(NDIM_SLICE + J) 
             DRAPE_SH3N(II)%DRAPE_PLY(J)%NSLICE = NSLICE 
             ALLOCATE(DRAPE_SH3N(II)%DRAPE_PLY(J)%IDRAPE(NSLICE,2),DRAPE_SH3N(II)%DRAPE_PLY(J)%RDRAPE(NSLICE,2))
             DRAPE_SH3N(II)%DRAPE_PLY(J)%IDRAPE = 0
             DRAPE_SH3N(II)%DRAPE_PLY(J)%RDRAPE = ZERO
             DO  IS= 1,NSLICE  
                 DRAPE_SH3N(II)%DRAPE_PLY(J)%IDRAPE(IS,1) = ITMP(NDIM + IS,1)
                 DRAPE_SH3N(II)%DRAPE_PLY(J)%IDRAPE(IS,2) = ITMP(NDIM + IS,2) 
                 DRAPE_SH3N(II)%DRAPE_PLY(J)%RDRAPE(IS,1) = RTMP(NDIM + IS,1) 
                 DRAPE_SH3N(II)%DRAPE_PLY(J)%RDRAPE(IS,2) = RTMP(NDIM + IS,2)             
             ENDDO 
             NDIM = NDIM + NSLICE 
           ENDDO
           NDIM_SLICE = NDIM_SLICE + NPT_DRAPE
        ENDDO ! NUMELTG_DRAPE
        DEALLOCATE( NPT_ELT,ITMP,RTMP,NPTDRP_ELT,ISLICE,INDX_PLY)
        STDRAPE = NUMELTG
      ENDIF  
C
      RETURN
      END
Chd|====================================================================
Chd|  PLY_INFO_REST                 source/output/restart/rdresb.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PLY_INFO_REST(PLY_INFO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PLY_INFO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C--------------------------------------
C     READING INTEGERS
C--------------------------------------
      CALL READ_I_C(PLY_INFO,2*NUMPLY)
C--------------------------------------
C     READING REALS
C--------------------------------------
C
      RETURN
      END
