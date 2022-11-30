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
Chd|  RESTMOD                       share/modules1/restart_mod.F  
Chd|-- called by -----------
Chd|        UPGRADE_CAND_OPT              ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_E2S             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NEW_LINK                      source/coupling/rad2rad/new_link.F
Chd|        R2R_CHECK                     source/coupling/rad2rad/r2r_check.F
Chd|        R2R_CHECK_SEG                 source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_CLEAN_INTER               source/coupling/rad2rad/r2r_clean_inter.F
Chd|        R2R_COUNT                     source/coupling/rad2rad/r2r_count.F
Chd|        R2R_DOMDEC                    source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|        R2R_INPUT                     source/coupling/rad2rad/r2r_input.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|        R2R_VOID_1D                   source/coupling/rad2rad/r2r_void.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        SPLIT_EBCS                    source/boundary_conditions/ebcs/split_ebcs.F
Chd|        ST_QAPRINT_EBCS               source/output/qaprint/st_qaprint_ebcs.F
Chd|        TAGNOD_R2R_S                  source/coupling/rad2rad/tagnod_r2r.F
Chd|        TAG_ELEM_VOID_R2R             source/coupling/rad2rad/tagelem_r2r.F
Chd|        TAG_ELEM_VOID_R2R_LIN         source/coupling/rad2rad/tagelem_r2r.F
Chd|        UELT_SPMD_ADDITIONAL_NODE     source/user_interface/uaccess.F
Chd|        UPGRADE_REMNODE2              source/interfaces/interf1/upgrade_remnode.F
Chd|        R2R_EXIST                     source/coupling/rad2rad/routines_r2r.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE RESTMOD
#include "my_real.inc"

        USE UNITAB_MOD
        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      ICODE,   LSHSUB,  ISKEW,    ISKWN,            NETH,  
     .      IBCSLAG,
     .      IPART,      NOM_OPT, NPC,      IXUR,    IXTG,    IXTG1,
     .      IXS,        IXQ,     IXC,      IXT,     IXP,     IXR,
     .      ITAB,       ITABM1,  GJBUFI,   
     .      IFILL,      IMS,
     .      KXX,        IXX,     KXSP,     NOD2SP,  IEXMAD, 
     .      ISPSYM,     ISPCOND, ISPHIO,   LPRTSPH, LONFSPH, IBCL,
     .      IBFV,       ILAS,    LACCELM,  
     .      NNLINK,     LNLINK,  IUSER,    IPARG,   DD_IAD,  IGRV, 
     .      IEXLNK,     IBVEL,   LBVEL,    IACTIV,  IBMPC, 
     .      KINET,      IPARI,   NPRW,     LPRW,    
     .      ICONX,      LGAUGE,
     .      NPBY,       LPBY,    LRIVET,   NSTRF,
     .      LJOINT,     NODPOR,  MONVOL,   LAGBUF,  ITHGRP,  ITHBUF,
     .      ITHGRPA,    ITHBUFA, ITHGRPB,  ITHBUFB,  ITHGRPC, ITHBUFC, 
     .      ITHGRPD,    ITHBUFD, ITHGRPE,  ITHBUFE,  ITHGRPF, ITHBUFF,
     .      ITHGRPG,    ITHBUFG, ITHGRPH,  ITHBUFH,  ITHGRPI, ITHBUFI,
     .      IPART_STATE,IRBE3,   LRBE3  ,  ICODEP , ISKEWP ,
     .      NOM_SECT,   IRBE2   ,LRBE2,
     .      ITHVAR,     ICFIELD, ILOADP  , IXIG3D  , KXIG3D , INTERLOADP,
     .      KLOADPINTER,LOADPINTER
C 2D to avoid integer limitation
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IXSP

      INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      IAD_ELEM, IAD_RBY,  IAD_RBY2, IAD_I2M, 
     .      IADCJ,    IAD_RBM,  IAD_RBM2, IAD_SEC,  IAD_CUT,  FR_ELEM, 
     .      FR_RBY,   FR_WALL,  FR_RBY2,  FR_I2M,   FR_MV,    FR_LL, 
     .      FR_CJ,    FR_RBM,   FR_RBM2,  FR_SEC,   FR_CUT,   RG_CUT, 
     .      FR_MAD,   FR_I18,   WEIGHT,   NEWFRONT, NODGLOB,  NBRCVOIS, 
     .      LNRCVOIS, NBSDVOIS, LNSDVOIS, NERCVOIS, LERCVOIS, NESDVOIS, 
     .      LESDVOIS, NPSEGCOM, LSEGCOM,  NPORGEO, 
     .      LNODPOR,  ICODT,    ICODR,    ISKY,     ADSKY,    PROCNE, 
     .      ADDCNI2,  PROCNI2,  IADSDP,   IADRCP,   IADS,     IADWAL, 
     .      IADRBK,   IADI2,    IADMV2,   IADMV3,   IADLL,    IADRBM, 
     .      IADI18,   ILINK,    FR_RL,    LLINK,    IADRL,    LINALE,
     .      NEFLSW,   NNFLSW,   ICUT,     LGRAV,    FASOLFR,  DD_R2R,
     .      FR_LAGF,  LLAGF,    IGEO,     IPM,      ICONTACT, ISKWP,
     .      NSKWP,    ISENSP,   NSENSP,   IACCP,    NACCP,
     .                                    IAD_RBE3M,FR_RBE3M,
     .      FR_RBE3MP, IRBYM   ,LNRBYM,   FRONT_RM,
     .      ICODRBYM,  INOISE  ,INOD_PXFEM,IEL_PXFEM,    IAD_RBE2,
     .      FR_RBE2,  LCFIELD,LLOADP,INOD_CRKXFEM,IEL_CRKXFEM,IBORDNODE,
     .      IEDGESH,  NODEDGE, IEDGE,    IAD_EDGE,  FR_EDGE,  FR_NBEDGE,
     .      IGAUP,    NGAUP,   NODLEVXF, NODGLOBXFE,ELCUTC,   NODENR,
     .      KXFENOD2ELC,ENRTAG,TABSENSOR,ADDCSRECT ,FR_NOR,   IAD_FRNOR, 
     .      PROCNOR,  FR_EDG,  IAD_FREDG,IDRAPE    ,SEGQUADFR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBFTEMP, IBCV, IBCR, IBFFLUX

      INTEGER, DIMENSION(:), POINTER ::
     .    IFRAME, MADPRT,  MADSH4,  MADSH3,   MADSOL,  MADNOD, MADFAIL,
     .    NPBYL, LPBYL, NIGE
      TARGET :: 
     .    ISKWN,IPARI,IPART,NPC,IXTG,IXS,IBCL,IGRV,IBMPC,NPBY,LPBY,
     .    NOM_OPT,ITABM1,ICFIELD, ILOADP,IEDGESH,IEL_CRKXFEM


        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: BUFGEO
        my_real , 
     .    DIMENSION(:), ALLOCATABLE :: 
     .      X,       D,       V,       VR,      DR,      THKE,
     .      DAMPR,            MS,      IN,      TF,      PM, 
     .      SKEW,             GEO,     EANI,    BUFMAT,  
     .      BUFSF,   RBMPC,   GJBUFR,  W,       VEUL,    FILL, 
     .      DFILL,   ALPH,    WB,      DSAVE,   DRSAVE,  ASAVE, 
     .      ARSAVE,  STSAVE,  STRSAVE, FSAVSAV, SPBUF ,
     .      VSPHIO,  SPHVELN, MSNF,    FORC, 
     .      VEL,     FSAV,    FZERO,   XLAS,    ACCELM, 
     .      FBVEL,   GRAV,    FR_WAVE, PARTS0,  USER, 
     .      ELBUF,   FANI_CELL,
     .      RWBUF,   RWSAV,   RBY,      
     .      RIVET,   SECBUF,  VOLMON,  LAMBDA,  WA,      FV, 
     .      A,       AR,      STIFN,   VISCN,   STIFR,   PARTSAV,
     .      FSKY,    FSKYM,   FSKYI,   UWA,     VAL2,    PHI,
     .      R,       ESTIF,   CRFLSW,  FLSW,    FANI,
     .      XCUT,    ANIN,    TANI,    SECFCUM, WASPH, 
     .      W16,     DRETRIO, LBVRS,   PV,      RCONX,   RCONTACT,
     .      ACONTACT,PCONTACT,
     .      FACTIV,  MCP,     TEMP,    FRBE3,
     .      RBYM,    FNOISE,  MS0,     ADMSMS,
     .      DMELC,   DMELTG,  MSSA,    DMELS,   MSTR,    DMELTR,   
     .      MSP,     DMELP,   MSRT,    DMELRT,  RES_SMS,
     .      KVOL,    CFIELD, DIAG_SMS, LOADP,   GAUGE,
     .      DFLOW,   VFLOW,   WFLOW   ,KNOT,    WIGE,
     .      RDRAPE,  KNOTLOCPC,KNOTLOCEL
      my_real , 
     .    DIMENSION(:), ALLOCATABLE :: FCONV, FBFTEMP, FRADIA, FBFFLUX
     
      TARGET :: 
     .    THKE,DAMPR,SKEW,EANI,FORC,WB,RBY,VOLMON,CFIELD,
     .    LOADP

      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_UMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_UMP
      
      my_real, DIMENSION(:), POINTER ::
     .    RBYL,   DAMP,    XFRAME,
     .    RIGE,   XIGE,    VIGE
       my_real , 
     .    DIMENSION(:), ALLOCATABLE :: 
     .      MS_PLY,   ZI_PLY

C Simple Precision array XDP
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XDP
C Double Precision array FBSAV6 (sensor 11 or 12 and parith/on)
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: FBSAV6
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
C     PADMESH (1,IP) = cos(angle criteria)
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
C-----------------------------------------------
C     OUTPUT
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: RTHBUF
C=======================================================================
C UNITS
      TYPE (UNIT_TYPE_) :: UNITAB
C
      END MODULE RESTMOD

Chd|====================================================================
Chd|  INTBUFMOD                     share/modules1/restart_mod.F  
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        UPGRADE_REMNODE               source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE2              source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_E2S           source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG           source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG2          source/interfaces/interf1/upgrade_remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTBUFMOD
#include "my_real.inc"
! module a cleaner dans le code
c        INTEGER , DIMENSION(:), ALLOCATABLE :: INBUF
c        TARGET :: INBUF
c       my_real,
c     .         DIMENSION(:),ALLOCATABLE :: BUFIN
c        TARGET :: BUFIN

      END MODULE INTBUFMOD

Chd|====================================================================
Chd|  IXINTMOD                      share/modules1/restart_mod.F  
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        UPGRADE_IXINT                 source/interfaces/interf1/upgrade_ixint.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IXINTMOD
          INTEGER , DIMENSION(:), ALLOCATABLE, TARGET :: IXINT
       END MODULE IXINTMOD
