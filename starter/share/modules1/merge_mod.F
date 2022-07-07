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
Chd|  MERGEMOD                      share/modules1/merge_mod.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      MODULE MERGEMOD
      USE INTBUFDEF_MOD
#include "my_real.inc"

      TYPE ptrmergint
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE ptrmergint

      TYPE ptrmergrea
        my_real, DIMENSION(:), POINTER :: PTR
      END TYPE ptrmergrea

C tableaux nbre  elem par proc
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMS_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMS8_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMS10_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMS20_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMS16_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMQ_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMC_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMT_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMP_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMR_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMTG_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMTG6_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMUR_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMX_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMSP_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMFASOL_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NCONV_L,NRADIA_L,NHFLUX_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NFXTMP_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NRBE3_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NLRBE3_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMSEGQD_L

C tableau nbre noeuds par proc
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMNODS_L
C tableaux nbre groupes par proc
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMGRP_L
C tableau taille Bufin et inbuf  par proc
      INTEGER, DIMENSION(:), ALLOCATABLE :: SBUFINS_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: SINBUFS_L
C tableau taille ELBUF par proc
      INTEGER, DIMENSION(:), ALLOCATABLE :: SELBUF_LS
C Structure Parallele Interfaces
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: NSNRI_L
      TYPE(ptrmergint), DIMENSION(:,:), ALLOCATABLE :: NSVFI_L
      TYPE(ptrmergint), DIMENSION(:,:), ALLOCATABLE :: NSVSI_L
C Structure Parallele Interface partie flotante
      TYPE(ptrmergrea), DIMENSION(:,:), ALLOCATABLE :: PENFI_L
C
C Structure Parallele SPH entiere
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PSPHR_L,PSPHS_L
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LSPHS_L
C ISPSYMR
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ISPSYMR_L
C FVMBAG, FVSPMD_GLOBAL STRUCTURE
      INTEGER, DIMENSION(:,:,:),ALLOCATABLE ::  FVSPMD_GLOBAL

C tableaux d equivalence num elem local, num elem Global par proc
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: SOLGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: QUADGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: COQGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: TRUSGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: BEAMGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: SPRGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: TRIGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: URGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: XGLOB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: SPGLOB

C Buffers locaux
C Nodglob
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: WEIGHT_LS
C   Weight
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NODGLOB_LS
C ISKEWSP - SPH Skew
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ISKSPH_LS
C Igroup
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IGROUP_LS
C Isurf
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ISURF_LS
C Islin
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ISLIN_LS
C IBUFSSG
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IBUFSSG_LS
C LLINK
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NNLINK_LS
C NLLINK
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LNLINK_LS
C IPARI
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IPARI_LS
C INBUF
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: INBUF_LS
C NRPW
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NPRW_LS
C LRPW
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LPRW_LS
C NPBY
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NPBY_LS
C LPBY
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LPBY_LS
C SECTIONS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NSTRF_LS
C Joints cylindriques
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LJOINT_LS
C Porosite
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NODPOR_LS
C MONVOL
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: MONVOL_LS
C ITHBUF
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUF_LS
C ITHBUFA
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFA_LS
C ITHBUFB
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFB_LS
C ITHBUFC
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFC_LS
C ITHBUFD
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFD_LS
C ITHBUFE
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFE_LS
C ITHBUFF
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFF_LS
C ITHBUFG
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFG_LS
C ITHBUFH
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFH_LS
C ITHBUFI
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ITHBUFI_LS
C FR_MV
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: FR_MV_LS
C IGRV
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IGRV_LS
C LGRV
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LGRAV_LS
C IBVEL_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IBVEL_LS
C LBVEL_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LBVEL_LS
C FASOLFR
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: FASOLFR_LS
C SEGQUADFR
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: SEGQUADFR_LS
C IPARG
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IPARG_LS
C ISKWN_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ISKWN_LS
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISKSUBMOD_L
C NPORGEO_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: NPORGEO_LS
C LNODPOR_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LNODPOR_LS
C NOISE
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: INOISE_LS
C IXSP
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IXSP_LS
C ISPSYM
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ISPSYM_LS
C ICONV_LS, IBCR_LS, IBFFLUX_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IBCV_LS, IBCR_LS, IBFFLUX_LS
C IBFTEMP
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IBFTEMP_LS
C IRBE3
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IRBE3_LS
C LRBE3
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LRBE3_LS
C Rigid Body Material
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: IRBYM_LS
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LNRBYM_LS
C ICFIELD
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ICFIELD_LS
C LCFIELD
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LCFIELD_LS
C ILOADP
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: ILOADP_LS
C LLOADP
      TYPE(ptrmergint), DIMENSION(:), ALLOCATABLE :: LLOADP_LS
C -----------------------------------------------------------
C

C Buffers Flottants
C THKE
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: THKE_LS
C EANI
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: EANI_LS
C VEUL
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: VEUL_LS
C SPBUF
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: SPBUF_LS
C FZERO
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: FZERO_LS
C ELBUF
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: ELBUF_LS
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: ELBUF_TAB_LS
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: XFEM_TAB_LS

C BUFIN
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: BUFIN_LS
C RWL
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: RWBUF_LS
C RWSAV
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: RWSAV_LS
C RBY
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: RBY_LS
C SECBUF
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: SECBUF_LS
C GEO
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: GEO_LS
C VOLMON
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: VOLMON_LS
C NOISEF
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: NOISEF_LS
C SPH SKEW
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: SPSKEW_LS
      my_real, DIMENSION(:), ALLOCATABLE :: SUBMODSK
C Structure Parallele SPH FLOTTANTE
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: XSPHR_LS
C Thermal bc FCONV_LS, FRADIA_LS, FBFFLUX_LS
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: FCONV_LS,FRADIA_LS,FBFFLUX_LS
C Thermal bc FCONF_LS
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: FBFTEMP_LS
C FRBE3_LS
      TYPE(ptrmergrea), DIMENSION(:), ALLOCATABLE :: FRBE3_LS
C interface buffer
      TYPE (INTBUF_STRUCT_), DIMENSION(:,:),ALLOCATABLE:: INTBUF_TAB_L
C -------------------------------------------------------------
      END MODULE MERGEMOD

       
