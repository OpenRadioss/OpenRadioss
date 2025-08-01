Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    restmod                     ../starter/share/modules1/restart_mod.F
!||--- called by ------------------------------------------------------
!||    contrl                      ../starter/source/starter/contrl.F
!||    leclas                      ../starter/source/loads/laser/leclas.F
!||    lecsec42                    ../starter/source/tools/sect/hm_read_sect.F
!||    lectur                      ../starter/source/starter/lectur.F
!||    new_link                    ../starter/source/coupling/rad2rad/new_link.F
!||    r2r_check                   ../starter/source/coupling/rad2rad/r2r_check.F
!||    r2r_check_seg               ../starter/source/coupling/rad2rad/routines_r2r.F
!||    r2r_clean_inter             ../starter/source/coupling/rad2rad/r2r_clean_inter.F
!||    r2r_count                   ../starter/source/coupling/rad2rad/r2r_count.F
!||    r2r_domdec                  ../starter/source/coupling/rad2rad/r2r_domdec.F
!||    r2r_exist                   ../starter/source/coupling/rad2rad/routines_r2r.F
!||    r2r_group                   ../starter/source/coupling/rad2rad/r2r_group.F
!||    r2r_input                   ../starter/source/coupling/rad2rad/r2r_input.F
!||    r2r_monvol                  ../starter/source/coupling/rad2rad/r2r_prelec.F
!||    r2r_prelec                  ../starter/source/coupling/rad2rad/r2r_prelec.F
!||    r2r_prelec_name             ../starter/source/coupling/rad2rad/r2r_prelec_name.F
!||    r2r_split                   ../starter/source/coupling/rad2rad/r2r_split.F
!||    r2r_void                    ../starter/source/coupling/rad2rad/r2r_void.F
!||    r2r_void_1d                 ../starter/source/coupling/rad2rad/r2r_void.F
!||    read_ebcs                   ../starter/source/boundary_conditions/ebcs/read_ebcs.F
!||    set_user_window_nodes       ../starter/source/user_interface/user_windows_tools.F
!||    split_ebcs                  ../starter/source/boundary_conditions/ebcs/split_ebcs.F
!||    st_qaprint_ebcs             ../starter/source/output/qaprint/st_qaprint_ebcs.F
!||    tag_elem_void_r2r           ../starter/source/coupling/rad2rad/tagelem_r2r.F
!||    tag_elem_void_r2r_lin       ../starter/source/coupling/rad2rad/tagelem_r2r.F
!||    tagnod_r2r_s                ../starter/source/coupling/rad2rad/tagnod_r2r.F
!||    uelt_spmd_additional_node   ../starter/source/user_interface/uaccess.F
!||    upgrade_remnode2            ../starter/source/interfaces/interf1/upgrade_remnode.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      MODULE RESTMOD
#include "my_real.inc"

        USE UNITAB_MOD
        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      ICODE,      ISKEW,    ISKWN,            NETH,  
     .      IBCSLAG,
     .      IPART,      NOM_OPT, NPC,      IXUR,    IXTG,    IXTG1,
     .      IXS,        IXQ,     IXC,      IXT,     IXP,     IXR,
     .      ITAB,       ITABM1,  GJBUFI,   
     .      IFILL,      IMS,
     .      KXX,        IXX,     KXSP,     NOD2SP,  IEXMAD, 
     .      ISPSYM,     ISPCOND, ISPHIO,   LPRTSPH, LONFSPH, IBCL,
     .      IBFV,       ILAS,    LACCELM,  
     .      NNLINK,     LNLINK,  IPARG,   DD_IAD,  IGRV, 
     .      IEXLNK,     IBVEL,   LBVEL,    IACTIV,  IBMPC, 
     .      KINET,      IPARI,   NPRW,     LPRW,    
     .      ICONX,      LGAUGE,
     .      NPBY,       LPBY,    LRIVET,   NSTRF,
     .      LJOINT,     NODPOR,  MONVOL,   LAGBUF, 
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
     .      DFILL,   ALPH,    WB,      DSAVE,   ASAVE, 
     .      SPBUF ,
     .      VSPHIO,  SPHVELN, MSNF,    FORC, 
     .      VEL,     FSAV,    FZERO,   XLAS,    ACCELM, 
     .      FBVEL,   GRAV,    FR_WAVE, PARTS0, 
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
     .      RDRAPE,  KNOTLOCPC,KNOTLOCEL,DPL0CLD,VEL0CLD
      my_real , 
     .    DIMENSION(:), ALLOCATABLE :: FCONV, FBFTEMP, FRADIA, FBFFLUX
     
      TARGET :: 
     .    THKE,DAMPR,SKEW,EANI,FORC,WB,RBY,VOLMON,CFIELD,
     .    LOADP,X

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

!||====================================================================
!||    intbufmod              ../starter/share/modules1/restart_mod.F
!||--- called by ------------------------------------------------------
!||    inintr                 ../starter/source/interfaces/interf1/inintr.F
!||    lectur                 ../starter/source/starter/lectur.F
!||    upgrade_remnode        ../starter/source/interfaces/interf1/upgrade_remnode.F
!||    upgrade_remnode2       ../starter/source/interfaces/interf1/upgrade_remnode.F
!||    upgrade_remnode_e2s    ../starter/source/interfaces/interf1/upgrade_remnode.F
!||    upgrade_remnode_edg    ../starter/source/interfaces/interf1/upgrade_remnode.F
!||    upgrade_remnode_edg2   ../starter/source/interfaces/interf1/upgrade_remnode.F
!||====================================================================
      MODULE INTBUFMOD
#include "my_real.inc"
! module a cleaner dans le code
c        INTEGER , DIMENSION(:), ALLOCATABLE :: INBUF
c        TARGET :: INBUF
c       my_real,
c     .         DIMENSION(:),ALLOCATABLE :: BUFIN
c        TARGET :: BUFIN

      END MODULE INTBUFMOD
