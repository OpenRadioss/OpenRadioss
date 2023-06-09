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
Chd|  RESTMOD                       share/modules/restart_mod.F   
Chd|-- called by -----------
Chd|        UPGRADE_CAND_OPT              ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_E2S             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        CRKXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|        CRKXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|        GET_USER_WINDOW_NODES         source/user_interface/userwindow_interface_routines.F
Chd|        MAT_SOLID_GET_NOD_V           source/user_interface/uaccess.F
Chd|        MAT_SOLID_GET_NOD_X           source/user_interface/uaccess.F
Chd|        PREPARE_DEBUG                 share/modules/debug_mod.F     
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_JOINT                    source/output/restart/read_joint.F
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        RTHBUFWREST                   source/output/restart/wrrest.F
Chd|        SPMD_CHECK_TAG                source/mpi/ams/spmd_check_tag.F
Chd|        WRITE_JOINT                   source/output/restart/write_joint.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      MODULE RESTMOD
      USE CLUSTER_MOD
      USE SPHBOX
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
#include "my_real.inc"

        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      ICODE,   LSHSUB,  ISKEW,    ISKWN,   IFRAME,  NETH,  
     .      IBCSLAG,
     .      IPART,   NOM_OPT, NPC,      IXTG,    IXTG1,
     .      IXS,     IXQ,     IXC,      IXT,     IXP,     IXR,
     .      ITAB,    ITABM1,  GJBUFI,   
     .      IFILL,   IMS,     ISUBS,
     .      KXX,     IXX,      KXSP,    IXSP,    NOD2SP,
     .      ISPSYM,  ISPCOND, ISPHIO,   LPRTSPH, LONFSPH, IBCL,
     .      IBFV,    IECRAN,  ILAS,     LACCELM, 
     .      NNLINK,  LNLINK,  IPARG,   DD_IAD,  IGRV, 
     .      IEXLNK,  IBVEL,    LBVEL,   IACTIV,  IBMPC, 
     .      KINET,   IPARI,   NPRW,    LPRW,    
     .      ICONX,   LGAUGE,
     .      MADPRT,  MADSH4,  MADSH3,   MADSOL,  MADNOD,  MADFAIL, 
     .      NPBY,    NPBYL,   LPBY,     LPBYL,   LRIVET,  NSTRF,
     .      LJOINT,  NODPOR,  MONVOL,   LAGBUF,  
     .      IPART_STATE,IRBYM ,LNRBYM, WEIGHT_RM,ICODRBYM,
     .      IRBE3 ,LRBE3,INOD_PXFEM,IEL_PXFEM,
     .      IADC_PXFEM, ADSKY_PXFEM, MADIDX,MADCLNOD,NOM_SECT,
     .      IRBE2    ,LRBE2, PROCNE_PXFEM, IADSDP_PXFEM,
     .      IADRCP_PXFEM     ,ITHVAR,ICFIELD,ILOADP,INOD_CRKXFEM,
     .      IEL_CRKXFEM,IADC_CRKXFEM, ADSKY_CRKXFEM,PROCNE_CRKXFEM,
     .      CNE_CRKXFEM,IADSDP_CRKXFEM,IADRCP_CRKXFEM,IBUFSSG_IO,
     .      IBC_PLY,IBORDNODE,IEDGESH,XNODANIMEL,NODEDGE,IEDGE,
     .      CRKNODIAD,DD_R2R_ELEM,KXIG3D,IXIG3D,
     .      MAIN_PROC ,TAG_SKINS6,IBCSCYC,LBCSCYC,
     .      INTERLOADP,KLOADPINTER,LOADPINTER
            !rank (1 to NSPMD) of the processor that is the main of the node 
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_GLOBAL_VOIS,FACE_VOIS

      INTEGER, DIMENSION(:), ALLOCATABLE :: IBFTEMP, IBCV, IBCR, IBFFLUX

        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      IAD_ELEM,  IAD_RBY,  IAD_RBY2, IAD_I2M, 
     .      IADCJ,     IAD_RBM,  IAD_RBM2, IAD_SEC,    IAD_CUT,   FR_ELEM, 
     .      FR_RBY,    FR_WALL,  FR_RBY2,  FR_I2M,     FR_MV,	  FR_LL, 
     .      FR_CJ,     FR_RBM,   FR_RBM2,  FR_SEC,     FR_CUT,    RG_CUT, 
     .      FR_MAD,    FR_I18,   WEIGHT,   NEWFRONT,   NODGLOB,   NBRCVOIS, 
     .      LNRCVOIS,  NBSDVOIS, LNSDVOIS, NERCVOIS,   LERCVOIS,  NESDVOIS, 
     .      LESDVOIS,  NPSEGCOM, LSEGCOM,  NPORGEO, 
     .      LNODPOR,   ICODT,    ICODR,    ADSKY,      PROCNE,
     .      ADDCNI2,   PROCNI2,  IADSDP,   IADRCP,     IADS,      IADWAL, 
     .      IADRBK,    IADI2,    IADMV2,   IADMV3,     IADLL,     IADRBM, 
     .      IADI18,    ILINK,    FR_RL,    LLINK,      IADRL,     LINALE,
     .      NEFLSW,    NNFLSW,   ICUT,     LGRAV,      FASOLFR,   DD_R2R,
     .      FR_LAGF,   LLAGF,    IGEO,     IPM,        ICONTACT,  ISKWP,
     .      NSKWP,     ISENSP,   NSENSP,   IACCP,      NACCP,
     .                                                 IAD_RBE3M, FR_RBE3M,
     .      FR_RBE3MP ,FR_RBYM,  FR_RBYM2, IAD_RBYM   ,IAD_RBYM2, IADRBMK,
     .      ICODE_PLY ,ICODT_PLY,ISKEW_PLY,IAD_RBE2   ,FR_RBE2,
     .      LCFIELD   ,LLOADP,
     .      IAD_EDGE  ,FR_EDGE,  FR_NBEDGE,IGAUP      ,NGAUP,     NODLEVXF,WEIGHT_MD,
     .      NODGLOBXFE,ELCUTC,   NODENR,   KXFENOD2ELC,ENRTAG,
     .      TABSENSOR ,ADDCSRECT,FR_NOR,   IAD_FRNOR  , PROCNOR,    
     .      IDRAPE,   NATIV0_SMS , SEGQUADFR,ISKWP_L, NE_NERCVOIS,  
     .      NE_LERCVOIS,  NE_NESDVOIS, NE_LESDVOIS

        my_real , 
     .    DIMENSION(:), ALLOCATABLE :: 
     .      X,       D,       V,       VR,      DR,      THKE,
     .      DAMPR,   DAMP,    MS,      IN,      TF,      PM, 
     .      SKEW,    XFRAME,  GEO,     EANI,    BUFMAT,  BUFGEO,
     .      BUFSF,   RBMPC,   GJBUFR,  W,       VEUL,    FILL, 
     .      DFILL,   ALPH,    WB,      DSAVE,   DRSAVE,  ASAVE, 
     .      ARSAVE,  STSAVE,  STRSAVE, FSAVSAV, SPBUF,
     .      VSPHIO,  SPHVELN, MSNF,    FORC    ,
     .      VEL,     FSAV,    FZERO,   XLAS,   ACCELM, 
     .      FBVEL,   GRAV,    FR_WAVE, PARTS0,
     .      ELBUF,   
     .      RWBUF,   RWSAV,   RBY,     RBYL,    
     .      RIVET,   SECBUF,  VOLMON,  LAMBDA,  WA,      FV, 
     .      A,       AR,      STIFN,   VISCN,   STIFR,   PARTSAV,
     .      FSKY,    FSKYM,   FSKYD,   UWA,     VAL2,    PHI,
     .      R,       ESTIF,   CRFLSW,  FLSW,    FANI,
     .      XCUT,    ANIN,    TANI,    SECFCUM, WASPH, 
     .      W16,     DRETRIO, LBVRS,   PV,      RCONX  , RCONTACT,
     .      ACONTACT,PCONTACT,
     .      FACTIV,           RBYM,    FRBE3 ,
     .      MS_PLY,  ZI_PLY,  MS0,     ADMSMS,  DMELC,   DMELTG,
     .      MSSA,    DMELS,   MSTR,    DMELTR,  MSP,     DMELP,
     .      MSRT,    DMELRT,  RES_SMS, PHIE,    MSF,
     .      CFIELD,  MSZ2,    DIAG_SMS,LOADP,   DMINT2,
     .      POR,     GAUGE,   WAGAP   ,RTHBUF,  KNOT,
     .      WIGE,    RDRAPE,  TAB_MAT, MS_2D ,KNOTLOCPC,KNOTLOCEL,
     .      FCONT_MAX,IN0  ,  DGAPLOADINT
        
      my_real, 
     .    DIMENSION(:, :), ALLOCATABLE :: XCELL
      my_real, 
     .    DIMENSION(:, :, :), ALLOCATABLE :: XFACE

      my_real , 
     .    DIMENSION(:), ALLOCATABLE :: FCONV, FBFTEMP, FRADIA, FBFFLUX

      TYPE (CLUSTER_) ,DIMENSION(:), ALLOCATABLE :: CLUSTER
C Double Precision array FBSAV6 (sensor 11 or 12 and parith/on)
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: FBSAV6
      TYPE (SPSYM_STRUCT)  :: XSPSYM,VSPSYM,WSMCOMP
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
C Mat+ Prop timers (DDW/MONON)
      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_UMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::TAB_UMP
      my_real, DIMENSION(:,:), ALLOCATABLE :: CPUTIME_MP_GLOB
      my_real, DIMENSION(:), ALLOCATABLE :: CPUTIME_MP
c      my_real, DIMENSION(:,:), ALLOCATABLE :: PSKID
C=======================================================================
C     Specific Adaptive Meshing :
C=======================================================================
C     SH4TREE(KSHTREE,NUMELC), SH3TREE(KSHTREE,NUMELTG)
C     1: father
C     2: 1st son (2nd son = 1st + 1, 3rd son = 1st + 2, 4th son = 1st + 3)
C     3:   = LEVEL SI ACTIVE
C         = -(LEVEL+1) SINON
C     4: group Number
C-----------------------------------------------
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: SH4TREE,SH3TREE
C-----------------------------------------------
C     IPADMESH(1,IP) = initial level of elements
C     PADMESH(1,IP) = cos(angle criteria)
C     PADMESH (2,IP) = criteria for error on thickness
C-----------------------------------------------
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IPADMESH
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: PADMESH
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: SH4TRIM, SH3TRIM
C-----------------------------------------------
C     MSC(N) = mass of shell element N
C     INC(N) = inertia of shell element N
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: MSC,MSTG,INC,INTG
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: PTG
C-----------------------------------------------
C     Condensation
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: MSCND,INCND
C-----------------------------------------------
C     Thermic
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: MCPC, MCPTG
C=======================================================================
      DOUBLE PRECISION,
     .         DIMENSION(:), ALLOCATABLE :: DDP

      END MODULE RESTMOD

Chd|====================================================================
Chd|  INTBUFMOD                     share/modules/restart_mod.F   
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTBUFMOD
#include "my_real.inc"
        INTEGER , DIMENSION(:), ALLOCATABLE :: INBUF
        TARGET :: INBUF
       my_real,
     .         DIMENSION(:),ALLOCATABLE :: BUFIN
        TARGET :: BUFIN
      END MODULE INTBUFMOD
C=======================================================================
Chd|====================================================================
Chd|  INTBUF_MOD                    share/modules/restart_mod.F   
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      MODULE INTBUF_MOD

        USE INTBUFDEF_MOD      
        USE INT8_MOD
        TYPE(INTBUF_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: INTBUF_TAB
        TYPE(INT8_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: T8

      END MODULE INTBUF_MOD
C=======================================================================
Chd|====================================================================
Chd|  FSKYI_MOD                     share/modules/restart_mod.F   
Chd|-- called by -----------
Chd|        REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE FSKYI_MOD
        my_real,
     .  DIMENSION(:,:),ALLOCATABLE :: FSKYI,TEMP_FSKYI
        INTEGER,DIMENSION(:),ALLOCATABLE :: ISKY,TEMP_ISKY
      END MODULE FSKYI_MOD
Chd|====================================================================
Chd|  INTBUFFRIC_MOD                share/modules/restart_mod.F   
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|====================================================================
      MODULE INTBUFFRIC_MOD

        USE INTBUF_FRIC_MOD                     

        TYPE(INTBUF_FRIC_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: INTBUF_FRIC_TAB

      END MODULE INTBUFFRIC_MOD
