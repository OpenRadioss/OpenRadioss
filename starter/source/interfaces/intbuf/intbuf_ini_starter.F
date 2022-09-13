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
Chd|  INTBUF_INI_STARTER            source/interfaces/intbuf/intbuf_ini_starter.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INTBUF_INI_STARTER(INTBUF_TAB, IPARI, NUMN, 
     .                              I11FLAG, FLAG_ALLOCATE, PROC ,INTBUF_FRIC_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD                      
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "ige3d_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                         
C-----------------------------------------------
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c      my_real
c     .   
      INTEGER IPARI(NPARI,*), NUMN, FLAG_ALLOCATE, I11FLAG, PROC
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,ITYP,NSN,NMN,NST,NME,NMT,NRTS,NRTM,MULTIMP,IGAP,
     .        INACTI,IFQ,MFROT,NUVAR,ILAGM,ILEV,IDEL7N,INTTH,ITIED,
     .        NSNE,NMNE,NLINS,NLINM,IFORM,NLN,NMNG,NCONT,NCONTE,MULTIMPE,MULTIMPS,IEDGE,
     .        INTKG,INTPLY,NISUB,NISUBS,NISUBM,NISUBE,L24ADD,NREMNODE,
     .        FLAGREMNODE,NRTSE,NADMSR,IEDGE4,NRTM_FE,NRTM_IGE,NRTS_FE,
     .        NRTS_IGE,NSN_FE,NSN_IGE,NMN_FE,NMN_IGE,INTFRIC,NREMNOR,IVIS2,
     .        NEDGE,INTNITSCHE,ITHK,IFLAGLOADP,NREMNODE_EDG,NREMNODE_E2S
C-----------------------------------------------

      DO NIN = 1,NINTER 
C=======================================================================
c        INIT SIZES TO ZERO
C=======================================================================
C=======================================================================
C INTEGER arrays sizes
C=======================================================================
        INTBUF_TAB(NIN)%S_IRTLM        = 0 
        INTBUF_TAB(NIN)%S_IRUPT        = 0 
        INTBUF_TAB(NIN)%S_INORM        = 0 
        INTBUF_TAB(NIN)%S_I_STOK       = 0 
        INTBUF_TAB(NIN)%S_I_STOK_E     = 0 
        INTBUF_TAB(NIN)%S_IRECTS       = 0 
        INTBUF_TAB(NIN)%S_IRECTM       = 0 
        INTBUF_TAB(NIN)%S_NSV          = 0 
        INTBUF_TAB(NIN)%S_MSR          = 0 
        INTBUF_TAB(NIN)%S_CAND_E       = 0 
        INTBUF_TAB(NIN)%S_CAND_N       = 0 
        INTBUF_TAB(NIN)%S_IELES        = 0 
        INTBUF_TAB(NIN)%S_IELEC        = 0 
        INTBUF_TAB(NIN)%S_IFPEN        = 0 
        INTBUF_TAB(NIN)%S_LISUB        = 0 
        INTBUF_TAB(NIN)%S_TYPSUB       = 0 
        INTBUF_TAB(NIN)%S_ADDSUBS      = 0 
        INTBUF_TAB(NIN)%S_ADDSUBM      = 0 
        INTBUF_TAB(NIN)%S_LISUBS       = 0 
        INTBUF_TAB(NIN)%S_LISUBM       = 0 
        INTBUF_TAB(NIN)%S_INFLG_SUBS   = 0 
        INTBUF_TAB(NIN)%S_INFLG_SUBM   = 0 
        INTBUF_TAB(NIN)%S_MSEGTYP      = 0 
        INTBUF_TAB(NIN)%S_KREMNODE     = 0 
        INTBUF_TAB(NIN)%S_REMNODE      = 0 
        INTBUF_TAB(NIN)%S_ADCCM        = 0 
        INTBUF_TAB(NIN)%S_CHAIN        = 0 
        INTBUF_TAB(NIN)%S_NIGE         = 0 
!type20
        INTBUF_TAB(NIN)%S_DAANC6       = 0 
        INTBUF_TAB(NIN)%S_NBINFLG      = 0 
        INTBUF_TAB(NIN)%S_MBINFLG      = 0 
        INTBUF_TAB(NIN)%S_EBINFLG      = 0 
        INTBUF_TAB(NIN)%S_NLG          = 0 
        INTBUF_TAB(NIN)%S_ISLINS       = 0 
        INTBUF_TAB(NIN)%S_ISLINM       = 0 
        INTBUF_TAB(NIN)%S_IXLINS       = 0 
        INTBUF_TAB(NIN)%S_IXLINM       = 0 
        INTBUF_TAB(NIN)%S_NSVL         = 0 
        INTBUF_TAB(NIN)%S_MSRL         = 0 
        INTBUF_TAB(NIN)%S_LCAND_N      = 0 
        INTBUF_TAB(NIN)%S_LCAND_S      = 0 
        INTBUF_TAB(NIN)%S_ADCCM20      = 0 
        INTBUF_TAB(NIN)%S_CHAIN20      = 0 

!type1
        INTBUF_TAB(NIN)%S_ILOCS        = 0 
        INTBUF_TAB(NIN)%S_NSEGM        = 0 
        INTBUF_TAB(NIN)%S_NRT          = 0 
!type3
        INTBUF_TAB(NIN)%S_IRTLS        = 0 
        INTBUF_TAB(NIN)%S_ILOCM        = 0 
        INTBUF_TAB(NIN)%S_IRTLOM       = 0 
        INTBUF_TAB(NIN)%S_IRTLOS       = 0 
        INTBUF_TAB(NIN)%S_NSEGS        = 0 
        INTBUF_TAB(NIN)%S_LNSV         = 0 
        INTBUF_TAB(NIN)%S_LMSR         = 0 
!type4
        INTBUF_TAB(NIN)%S_IELEM        = 0 
!type12
        INTBUF_TAB(NIN)%S_FCOUNT       = 0 
!type14
        INTBUF_TAB(NIN)%S_KSURF        = 0 
        INTBUF_TAB(NIN)%S_IMPACT       = 0 
!type21
        INTBUF_TAB(NIN)%S_MSR21       = 0 !
        INTBUF_TAB(NIN)%S_MNDD        = 0 !
        INTBUF_TAB(NIN)%S_MSR_L       = 0 !
!type24
        INTBUF_TAB(NIN)%S_MVOISIN      = 0 
        INTBUF_TAB(NIN)%S_NVOISIN      = 0 
!type24 & type25
        INTBUF_TAB(NIN)%S_MSEGLO       = 0 
        INTBUF_TAB(NIN)%S_MSEGTYP24    = 0 
!type24
        INTBUF_TAB(NIN)%S_ISEADD       = 0 
        INTBUF_TAB(NIN)%S_ISEDGE       = 0 
        INTBUF_TAB(NIN)%S_CAND_T       = 0 
        INTBUF_TAB(NIN)%S_ISEG_PXFEM   = 0 
        INTBUF_TAB(NIN)%S_ISEG_PLY     = 0 
        INTBUF_TAB(NIN)%S_ICONT_I      = 0 
        INTBUF_TAB(NIN)%S_IRTSE        = 0
        INTBUF_TAB(NIN)%S_IS2SE        = 0 
        INTBUF_TAB(NIN)%S_IS2PT        = 0 
        INTBUF_TAB(NIN)%S_ISPT2        = 0 
        INTBUF_TAB(NIN)%S_ISEGPT       = 0 
        INTBUF_TAB(NIN)%S_IS2ID        = 0 !IS2ID Global ID for T24 E2E Fictive nodes
!type25
        INTBUF_TAB(NIN)%S_EVOISIN      = 0 
        INTBUF_TAB(NIN)%S_ADMSR        = 0
        INTBUF_TAB(NIN)%S_LEDGE        = 0
        INTBUF_TAB(NIN)%S_LBOUND       = 0
        INTBUF_TAB(NIN)%S_ACTNOR       = 0
        INTBUF_TAB(NIN)%S_FARM         = 0
        INTBUF_TAB(NIN)%S_ADSKYN       = 0
        INTBUF_TAB(NIN)%S_IADNOR       = 0
        INTBUF_TAB(NIN)%S_ISLIDE       = 0
        INTBUF_TAB(NIN)%S_KNOR2MSR     = 0
        INTBUF_TAB(NIN)%S_NOR2MSR      = 0
        INTBUF_TAB(NIN)%S_CAND_OPT_N   = 0
        INTBUF_TAB(NIN)%S_CAND_OPT_E   = 0
        INTBUF_TAB(NIN)%S_IF_ADH       = 0
        INTBUF_TAB(NIN)%S_CANDM_E2E    = 0 
        INTBUF_TAB(NIN)%S_CANDS_E2E    = 0 
        INTBUF_TAB(NIN)%S_CANDM_E2S    = 0 
        INTBUF_TAB(NIN)%S_CANDS_E2S    = 0 
        INTBUF_TAB(NIN)%S_ADDSUBE      = 0 
        INTBUF_TAB(NIN)%S_LISUBE       = 0 
        INTBUF_TAB(NIN)%S_INFLG_SUBE   = 0
        INTBUF_TAB(NIN)%S_KREMNODE_EDG = 0 
        INTBUF_TAB(NIN)%S_REMNODE_EDG  = 0 
        INTBUF_TAB(NIN)%S_KREMNODE_E2S = 0 
        INTBUF_TAB(NIN)%S_REMNODE_E2S  = 0 
!type2
        INTBUF_TAB(NIN)%S_MSEGTYP2     = 0
C=======================================================================
C REAL arrays sizes
C=======================================================================
        INTBUF_TAB(NIN)%S_CSTS         = 0 
        INTBUF_TAB(NIN)%S_DPARA        = 0 
        INTBUF_TAB(NIN)%S_NMAS         = 0 
        INTBUF_TAB(NIN)%S_SMAS         = 0 
        INTBUF_TAB(NIN)%S_SINER        = 0 
        INTBUF_TAB(NIN)%S_SPENALTY     = 0 
        INTBUF_TAB(NIN)%S_STFR_PENALTY = 0 
        INTBUF_TAB(NIN)%S_AREAS2       = 0 
        INTBUF_TAB(NIN)%S_UVAR         = 0 
        INTBUF_TAB(NIN)%S_XM0          = 0 
        INTBUF_TAB(NIN)%S_SKEW         = 0 
        INTBUF_TAB(NIN)%S_DSM          = 0 
        INTBUF_TAB(NIN)%S_FSM          = 0 
        INTBUF_TAB(NIN)%S_RUPT         = 0 
        INTBUF_TAB(NIN)%S_FINI         = 0 
        INTBUF_TAB(NIN)%S_STFAC        = 0 
        INTBUF_TAB(NIN)%S_VARIABLES    = 0 
        INTBUF_TAB(NIN)%S_STFNS        = 0 
        INTBUF_TAB(NIN)%S_GAP_M        = 0 
        INTBUF_TAB(NIN)%S_GAP_S        = 0 
        INTBUF_TAB(NIN)%S_STFM         = 0 
        INTBUF_TAB(NIN)%S_CAND_P       = 0 
        INTBUF_TAB(NIN)%S_XSAV         = 0 
        INTBUF_TAB(NIN)%S_CRIT         = 0 
        INTBUF_TAB(NIN)%S_FRIC_P       = 0 
        INTBUF_TAB(NIN)%S_XFILTR       = 0 
        INTBUF_TAB(NIN)%S_FTSAVX       = 0 
        INTBUF_TAB(NIN)%S_FTSAVY       = 0 
        INTBUF_TAB(NIN)%S_FTSAVZ       = 0 
        INTBUF_TAB(NIN)%S_AREAS        = 0 
        INTBUF_TAB(NIN)%S_AREAM        = 0 
        INTBUF_TAB(NIN)%S_GAP_ML       = 0 
        INTBUF_TAB(NIN)%S_GAP_SL       = 0 
        INTBUF_TAB(NIN)%S_STFS         = 0 
        INTBUF_TAB(NIN)%S_PENIS        = 0 
        INTBUF_TAB(NIN)%S_PENIM        = 0 
        INTBUF_TAB(NIN)%S_RIGE         = 0
        INTBUF_TAB(NIN)%S_XIGE         = 0
        INTBUF_TAB(NIN)%S_VIGE         = 0
        INTBUF_TAB(NIN)%S_MASSIGE      = 0
!type10
        INTBUF_TAB(NIN)%S_CAND_F        = 0
!type20
        INTBUF_TAB(NIN)%S_XA            = 0
        INTBUF_TAB(NIN)%S_VA            = 0
        INTBUF_TAB(NIN)%S_STFA          = 0
        INTBUF_TAB(NIN)%S_AVX_ANCR      = 0
        INTBUF_TAB(NIN)%S_GAP_SH        = 0
        INTBUF_TAB(NIN)%S_CAND_FX       = 0
        INTBUF_TAB(NIN)%S_CAND_FY       = 0
        INTBUF_TAB(NIN)%S_CAND_FZ       = 0
        INTBUF_TAB(NIN)%S_GAP_SE        = 0
        INTBUF_TAB(NIN)%S_GAP_ME        = 0
        INTBUF_TAB(NIN)%S_STF           = 0
        INTBUF_TAB(NIN)%S_STFNE         = 0
        INTBUF_TAB(NIN)%S_CRITX         = 0
        INTBUF_TAB(NIN)%S_PENISE        = 0
        INTBUF_TAB(NIN)%S_PENIME        = 0
        INTBUF_TAB(NIN)%S_PENIA         = 0
        INTBUF_TAB(NIN)%S_ALPHAK        = 0

!type1
        INTBUF_TAB(NIN)%S_CSTS          = 0
        INTBUF_TAB(NIN)%S_N             = 0

!type3,4,5,9
        INTBUF_TAB(NIN)%S_CSTM          = 0
        INTBUF_TAB(NIN)%S_EE            = 0
        INTBUF_TAB(NIN)%S_STFNM         = 0
        INTBUF_TAB(NIN)%S_FRICOS        = 0
        INTBUF_TAB(NIN)%S_FRICOM        = 0
        INTBUF_TAB(NIN)%S_FTSAV         = 0
!type6
        INTBUF_TAB(NIN)%S_FCONT         = 0
        INTBUF_TAB(NIN)%S_FS            = 0
        INTBUF_TAB(NIN)%S_FM            = 0
        INTBUF_TAB(NIN)%S_RMAS          = 0
        INTBUF_TAB(NIN)%S_ANSMX0        = 0
!type 8
        INTBUF_TAB(NIN)%S_T8           = 0 
        INTBUF_TAB(NIN)%S_GAPN          = 0
        INTBUF_TAB(NIN)%S_STF8          = 0
!type 14
        INTBUF_TAB(NIN)%S_CIMP          = 0
        INTBUF_TAB(NIN)%S_NIMP          = 0

!type 15
        INTBUF_TAB(NIN)%S_IOLD          = 0
        INTBUF_TAB(NIN)%S_HOLD          = 0
        INTBUF_TAB(NIN)%S_NOLD          = 0
        INTBUF_TAB(NIN)%S_DOLD          = 0
!type 17
        INTBUF_TAB(NIN)%S_KS            = 0
        INTBUF_TAB(NIN)%S_KM            = 0
        INTBUF_TAB(NIN)%S_FROTS         = 0
        INTBUF_TAB(NIN)%S_FROTM         = 0
!type 21 
        INTBUF_TAB(NIN)%S_NOD_NORMAL    = 0
!type 21
        INTBUF_TAB(NIN)%S_RCURV         = 0
        INTBUF_TAB(NIN)%S_ANGLM         = 0
        INTBUF_TAB(NIN)%S_FROT_P        = 0
        INTBUF_TAB(NIN)%S_ALPHA0        = 0
        INTBUF_TAB(NIN)%S_AS            = 0
        INTBUF_TAB(NIN)%S_BS            = 0
        INTBUF_TAB(NIN)%S_THKNOD0       = 0
!type 24 and type25
        INTBUF_TAB(NIN)%S_GAPN_M        = 0
        INTBUF_TAB(NIN)%S_SECND_FR       = 0
        INTBUF_TAB(NIN)%S_PENE_OLD      = 0
        INTBUF_TAB(NIN)%S_STIF_OLD      = 0
        INTBUF_TAB(NIN)%S_TIME_S        = 0
        INTBUF_TAB(NIN)%S_GAP_NM        = 0
        INTBUF_TAB(NIN)%S_EDGE8L2       = 0
        INTBUF_TAB(NIN)%S_NOD_2RY_LGTH  = 0
        INTBUF_TAB(NIN)%S_NOD_MAS_LGTH  = 0
        INTBUF_TAB(NIN)%S_GAP_N0        = 0
        INTBUF_TAB(NIN)%S_DGAP_NM       = 0
        INTBUF_TAB(NIN)%S_DGAP_M        = 0
        INTBUF_TAB(NIN)%S_DELTA_PMAX_DGAP  = 0
        INTBUF_TAB(NIN)%S_XFIC          = 0 
        INTBUF_TAB(NIN)%S_VFIC          = 0 
        INTBUF_TAB(NIN)%S_MSFIC         = 0 
!type 25
        INTBUF_TAB(NIN)%S_EDGE_BISECTOR = 0
        INTBUF_TAB(NIN)%S_PENM          = 0
        INTBUF_TAB(NIN)%S_DISTM         = 0
        INTBUF_TAB(NIN)%S_LBM           = 0
        INTBUF_TAB(NIN)%S_LCM           = 0
        INTBUF_TAB(NIN)%S_VTX_BISECTOR  = 0
        INTBUF_TAB(NIN)%S_KREMNOR       = 0 
        INTBUF_TAB(NIN)%S_REMNOR        = 0 
        INTBUF_TAB(NIN)%S_CAND_PS       = 0 
        INTBUF_TAB(NIN)%S_GAPE          = 0 
        INTBUF_TAB(NIN)%S_GAP_E_L       = 0 
        INTBUF_TAB(NIN)%S_STFE          = 0
        INTBUF_TAB(NIN)%S_GAPMSAV       = 0
        INTBUF_TAB(NIN)%S_FTSAVX_E      = 0 
        INTBUF_TAB(NIN)%S_FTSAVY_E      = 0
        INTBUF_TAB(NIN)%S_FTSAVZ_E      = 0 
        INTBUF_TAB(NIN)%S_FTSAVX_E2S    = 0 
        INTBUF_TAB(NIN)%S_FTSAVY_E2S    = 0 
        INTBUF_TAB(NIN)%S_FTSAVZ_E2S    = 0
        INTBUF_TAB(NIN)%S_IFPEN_E       = 0  
        INTBUF_TAB(NIN)%S_IFPEN_E2S     = 0 
        INTBUF_TAB(NIN)%S_E2S_NOD_NORMAL = 0
        INTBUF_TAB(NIN)%S_E2S_ACTNOR    = 0 
! FRICTION INTERFACE
        INTBUF_TAB(NIN)%S_IPARTFRICS   = 0
        INTBUF_TAB(NIN)%S_IPARTFRICM   = 0
        INTBUF_TAB(NIN)%S_IREP_FRICM   = 0
        INTBUF_TAB(NIN)%S_DIR_FRICM    = 0
        INTBUF_TAB(NIN)%S_IPARTFRIC_E  = 0

!type2
        INTBUF_TAB(NIN)%S_CSTS_BIS     = 0
!NITSCHE METHOD
        INTBUF_TAB(NIN)%S_IELNRTS = 0 
        INTBUF_TAB(NIN)%S_ADRECTS = 0 
        INTBUF_TAB(NIN)%S_FACNRTS = 0 
C=======================================================================
c        DEFINE SIZES
C=======================================================================
        NRTS   = IPARI(3,NIN)
        NRTM   = IPARI(4,NIN)
        NME    = IPARI(4,NIN)
        NSN    = IPARI(5,NIN)
        NMN    = IPARI(6,NIN)
        ITYP   = IPARI(7,NIN)
        NST    = IPARI(8,NIN)
        NMNG   = IPARI(8,NIN) !type21
        NMT    = IPARI(9,NIN)
        IDEL7N = IPARI(17,NIN)
        NCONT  = IPARI(18,NIN)
        NCONTE = NCONT
        ILEV   = IPARI(20,NIN)
        IGAP   = IPARI(21,NIN)
        INACTI = ABS(IPARI(22,NIN))    ! cas INACTI "passant a 0"
        MULTIMP= IPARI(23,NIN)
        MFROT  = IPARI(30,NIN)
        IFQ    = IPARI(31,NIN)
        ILAGM  = IPARI(33,NIN)
        NUVAR  = IPARI(35,NIN)
        NLN    = IPARI(35,NIN)
        NISUB  = IPARI(36,NIN)
        NISUBS = IPARI(37,NIN)
        NISUBM = IPARI(38,NIN)
        INTTH  = IPARI(47,NIN)
        IFORM  = IPARI(48,NIN)
        NLINS  = IPARI(51,NIN)                             
        NLINM  = IPARI(52,NIN)
        ITIED  = IPARI(85,NIN) ! Type7
C----- used for int24+edge ficfive nodes        
        NSNE   = IPARI(55,NIN)                             
        NMNE   = IPARI(56,NIN)                             
        IEDGE  = IPARI(58,NIN)
        L24ADD = IPARI(59,NIN)
        NREMNODE=IPARI(62,NIN) 
        NREMNODE_EDG=IPARI(94,NIN) 
        NREMNODE_E2S=IPARI(96,NIN) 
        FLAGREMNODE=IPARI(63,NIN)
        INTKG  = IPARI(65,NIN) 
        INTPLY = IPARI(66,NIN)  
        NRTSE  = IPARI(52,NIN) 
C
C       Type25 
        NADMSR = IPARI(67,NIN) 
        NEDGE  = IPARI(68,NIN) 
        NREMNOR= IPARI(81,NIN) 
        IVIS2  = IPARI(14,NIN)
        NISUBE = IPARI(90,NIN)
C
C    TYPE 7, 11, 19: friction interface
        INTFRIC = IPARI(72,NIN) 
C
C       Ige
        NRTM_IGE= IPARI(73,NIN)
        NRTM_FE = IPARI(74,NIN)
        NRTS_IGE= IPARI(75,NIN)
        NRTS_FE = IPARI(76,NIN)
        NSN_IGE = IPARI(77,NIN)
        NSN_FE  = IPARI(78,NIN)
        NMN_IGE = IPARI(79,NIN)
        NMN_FE  = IPARI(80,NIN)
C       NITSCHE METHOD
        INTNITSCHE = IPARI(86,NIN)
C   T25 transient gap for main side 
        ITHK = IPARI(91,NIN)

        IFLAGLOADP = IPARI(95,NIN)
        

cc        print*,'-------INTBUF_INI_STARTER NI ITYP PROC',NIN,ITYP,PROC                                     
      
C=======================================================================
        IF (ITYP == 1) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV    = NSN 
            INTBUF_TAB(NIN)%S_MSR    = NMN 
            INTBUF_TAB(NIN)%S_IRECTS = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM = 4*NRTM 
            INTBUF_TAB(NIN)%S_IRTLM  = NSN    
            INTBUF_TAB(NIN)%S_ILOCS  = NSN    
            INTBUF_TAB(NIN)%S_NSEGM  = 1+NMN  
            INTBUF_TAB(NIN)%S_NRT    = NMT    

C----   size real arrays 
            INTBUF_TAB(NIN)%S_STFAC     = 1 
            INTBUF_TAB(NIN)%S_VARIABLES = NPARIR
            INTBUF_TAB(NIN)%S_CSTS      = 2*NSN 
            INTBUF_TAB(NIN)%S_N         = 3*NSN 
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 2) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV = NSN 
          INTBUF_TAB(NIN)%S_MSR = NMN 
          INTBUF_TAB(NIN)%S_IRECTS = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM = 4*NRTM 
          INTBUF_TAB(NIN)%S_IRTLM = NSN 
          IF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12 .OR.
     .        ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN
            INTBUF_TAB(NIN)%S_IRUPT = NSN
            INTBUF_TAB(NIN)%S_INORM = NSN
          ELSEIF ((ILEV == 27).OR.(ILEV == 28)) THEN
            INTBUF_TAB(NIN)%S_IRUPT = NSN
            INTBUF_TAB(NIN)%S_MSEGTYP2 = NRTM
          ENDIF
          INTBUF_TAB(NIN)%S_MSEGTYP = NRTM 

C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_CSTS  = 2*NSN 
          INTBUF_TAB(NIN)%S_DPARA = 7*NSN 
          INTBUF_TAB(NIN)%S_NMAS  = 2*NMN 
          IF (IDEL7N /= 0)THEN
            INTBUF_TAB(NIN)%S_SMAS  = NSN 
            INTBUF_TAB(NIN)%S_SINER = NSN             
          ENDIF
          IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12) THEN
            INTBUF_TAB(NIN)%S_SMAS  = NSN
            INTBUF_TAB(NIN)%S_SINER = NSN  
            INTBUF_TAB(NIN)%S_AREAS2= NSN  
            INTBUF_TAB(NIN)%S_UVAR  = NSN*NUVAR
            INTBUF_TAB(NIN)%S_XM0   = NSN*3  
            INTBUF_TAB(NIN)%S_DSM   = NSN*3  
            INTBUF_TAB(NIN)%S_FSM   = NSN*3  
          ELSEIF (ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN 
            INTBUF_TAB(NIN)%S_SMAS  = NSN
            INTBUF_TAB(NIN)%S_SINER = NSN
            INTBUF_TAB(NIN)%S_AREAS2= NSN 
            INTBUF_TAB(NIN)%S_UVAR  = NSN*NUVAR 
            INTBUF_TAB(NIN)%S_XM0   = NSN*3  
            INTBUF_TAB(NIN)%S_DSM   = NSN*3 
            INTBUF_TAB(NIN)%S_FSM   = NSN*3 
            INTBUF_TAB(NIN)%S_RUPT  = 6 
          ELSEIF (ILEV==25) THEN
            INTBUF_TAB(NIN)%S_SMAS         = NSN    
            INTBUF_TAB(NIN)%S_SINER        = NSN    
            INTBUF_TAB(NIN)%S_SPENALTY     = NSN    
            INTBUF_TAB(NIN)%S_STFR_PENALTY = NSN    
            INTBUF_TAB(NIN)%S_SKEW         = NSN*9  
            INTBUF_TAB(NIN)%S_DSM          = NSN*3  
            INTBUF_TAB(NIN)%S_FSM          = NSN*3  
            INTBUF_TAB(NIN)%S_FINI         = NSN*3  
          ELSEIF (ILEV==26) THEN
            INTBUF_TAB(NIN)%S_SMAS         = NSN    
            INTBUF_TAB(NIN)%S_SINER        = NSN    
            INTBUF_TAB(NIN)%S_SPENALTY     = NSN    
            INTBUF_TAB(NIN)%S_STFR_PENALTY = NSN    
            INTBUF_TAB(NIN)%S_SKEW         = NSN*9  
            INTBUF_TAB(NIN)%S_DSM          = NSN*12 
            INTBUF_TAB(NIN)%S_FSM          = NSN*12 
            INTBUF_TAB(NIN)%S_FINI         = NSN*24 
          ELSEIF (ILEV==27) THEN
            INTBUF_TAB(NIN)%S_SMAS         = NSN    
            INTBUF_TAB(NIN)%S_SINER        = NSN    
            INTBUF_TAB(NIN)%S_SPENALTY     = NSN    
            INTBUF_TAB(NIN)%S_STFR_PENALTY = NSN    
            INTBUF_TAB(NIN)%S_SKEW         = NSN*9  
            INTBUF_TAB(NIN)%S_DSM          = NSN*3  
            INTBUF_TAB(NIN)%S_FSM          = NSN*3  
            INTBUF_TAB(NIN)%S_FINI         = NSN*6  
          ELSEIF (ILEV==28) THEN
            INTBUF_TAB(NIN)%S_SMAS         = NSN    
            INTBUF_TAB(NIN)%S_SINER        = NSN    
            INTBUF_TAB(NIN)%S_SPENALTY     = NSN    
            INTBUF_TAB(NIN)%S_STFR_PENALTY = NSN    
            INTBUF_TAB(NIN)%S_SKEW         = NSN*9  
            INTBUF_TAB(NIN)%S_DSM          = NSN*3  
            INTBUF_TAB(NIN)%S_FSM          = NSN*3  
            INTBUF_TAB(NIN)%S_FINI         = NSN*6   
          ENDIF
          IF(INTTH > 0 ) THEN
            INTBUF_TAB(NIN)%S_AREAS2 = NSN 
          ENDIF   
          INTBUF_TAB(NIN)%S_CSTS_BIS = 2*NSN
C=======================================================================
        ELSEIF (ITYP == 3) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_MSR = NMN 
            INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM  = 4*NRTM 
            INTBUF_TAB(NIN)%S_IRTLM   = NSN 
            INTBUF_TAB(NIN)%S_IRTLS   = NMN 
            INTBUF_TAB(NIN)%S_ILOCS   = NSN 
            INTBUF_TAB(NIN)%S_ILOCM   = NMN 
            INTBUF_TAB(NIN)%S_IRTLOM  = NSN 
            INTBUF_TAB(NIN)%S_IRTLOS  = NMN 
            INTBUF_TAB(NIN)%S_NSEGS   = 1+NSN 
            INTBUF_TAB(NIN)%S_NSEGM   = 1+NMN 
            INTBUF_TAB(NIN)%S_LNSV    = NST
            INTBUF_TAB(NIN)%S_LMSR    = NMT
            INTBUF_TAB(NIN)%S_MSEGTYP = NRTS+NRTM 

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC     = 1 
            INTBUF_TAB(NIN)%S_VARIABLES = NPARIR 
            INTBUF_TAB(NIN)%S_CSTS  = 2*NSN
            INTBUF_TAB(NIN)%S_CSTM  = 2*NMN
            INTBUF_TAB(NIN)%S_STFNS = NSN 
            INTBUF_TAB(NIN)%S_STFNM = NMN 
            INTBUF_TAB(NIN)%S_STFS  = NRTS
            INTBUF_TAB(NIN)%S_STFM  = NRTM
            INTBUF_TAB(NIN)%S_FRICOS = 3*NSN 
            INTBUF_TAB(NIN)%S_FRICOM = 3*NMN 
            IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P = 10 
            IF (IFQ/=0) THEN 
              INTBUF_TAB(NIN)%S_XFILTR = 1 
              INTBUF_TAB(NIN)%S_FTSAV  = 3*NSN 
            ENDIF
            INTBUF_TAB(NIN)%S_AREAS = NSN 
            INTBUF_TAB(NIN)%S_AREAM = NMN 
          ENDIF
C=======================================================================
        ELSEIF (ITYP == 4) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_MSR = NMN 
            INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS
            INTBUF_TAB(NIN)%S_IRECTM  = 4*NRTM
            INTBUF_TAB(NIN)%S_IRTLM   = NSN
            INTBUF_TAB(NIN)%S_IRTLS   = NMN
            INTBUF_TAB(NIN)%S_ILOCS   = NSN
            INTBUF_TAB(NIN)%S_ILOCM   = NMN
            INTBUF_TAB(NIN)%S_IRTLOM  = NSN
            INTBUF_TAB(NIN)%S_IRTLOS  = NMN
            INTBUF_TAB(NIN)%S_NSEGS   = 1+NSN 
            INTBUF_TAB(NIN)%S_NSEGM   = 1+NMN 
            INTBUF_TAB(NIN)%S_LNSV    = NST 
            INTBUF_TAB(NIN)%S_LMSR    = NMT 
            INTBUF_TAB(NIN)%S_IELES   = NRTS
            INTBUF_TAB(NIN)%S_IELEM   = NRTM

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC     = 1 
            INTBUF_TAB(NIN)%S_VARIABLES = NPARIR 
            INTBUF_TAB(NIN)%S_CSTS  = 2*NSN 
            INTBUF_TAB(NIN)%S_CSTM  = 2*NMN 
            INTBUF_TAB(NIN)%S_STFNS = NSN 
            INTBUF_TAB(NIN)%S_STFNM = NMN 
            INTBUF_TAB(NIN)%S_STFS  = NRTS
            INTBUF_TAB(NIN)%S_STFM  = NRTM
            INTBUF_TAB(NIN)%S_FRICOS = 3*NSN 
            INTBUF_TAB(NIN)%S_FRICOM = 3*NMN 
            IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P = 10 
            IF (IFQ/=0) THEN 
              INTBUF_TAB(NIN)%S_XFILTR = 1
              INTBUF_TAB(NIN)%S_FTSAV  = 3*NSN 
            ENDIF
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 5) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_MSR = NMN 
            INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM  = 4*NRTM 
            INTBUF_TAB(NIN)%S_IRTLM   = NSN 
            INTBUF_TAB(NIN)%S_IRTLS   = NMN 
            INTBUF_TAB(NIN)%S_ILOCS   = NSN 
            INTBUF_TAB(NIN)%S_ILOCM   = NMN 
            INTBUF_TAB(NIN)%S_IRTLOM  = NSN 
            INTBUF_TAB(NIN)%S_IRTLOS  = NMN 
            INTBUF_TAB(NIN)%S_NSEGS   = 1+NSN 
            INTBUF_TAB(NIN)%S_NSEGM   = 1+NMN 
            INTBUF_TAB(NIN)%S_LNSV    = NST 
            INTBUF_TAB(NIN)%S_LMSR    = NMT 
            INTBUF_TAB(NIN)%S_MSEGTYP = NRTS+NRTM 

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1 
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_CSTS  = 2*NSN
            INTBUF_TAB(NIN)%S_CSTM  = 2*NMN
            INTBUF_TAB(NIN)%S_STFNS = NSN  
            INTBUF_TAB(NIN)%S_STFNM = NMN  
            INTBUF_TAB(NIN)%S_STFS  = NRTS 
            INTBUF_TAB(NIN)%S_STFM  = NRTM 
            INTBUF_TAB(NIN)%S_FRICOS  = 3*NSN
            INTBUF_TAB(NIN)%S_FRICOM  = 3*NMN
            IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P = 10
            IF (IFQ/=0) THEN 
              INTBUF_TAB(NIN)%S_XFILTR  = 1 
              INTBUF_TAB(NIN)%S_FTSAV  = 3*NSN 
            ENDIF
            INTBUF_TAB(NIN)%S_AREAS = NSN 
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 6) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_MSR = NMN 
            INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM  = 4*NRTM 
            INTBUF_TAB(NIN)%S_IRTLM   = NSN 
            INTBUF_TAB(NIN)%S_IRTLS   = NMN 
            INTBUF_TAB(NIN)%S_ILOCS   = NSN 
            INTBUF_TAB(NIN)%S_ILOCM   = NMN 
            INTBUF_TAB(NIN)%S_IRTLOM  = NSN 
            INTBUF_TAB(NIN)%S_IRTLOS  = NMN 
            INTBUF_TAB(NIN)%S_NSEGS   = 1+NSN
            INTBUF_TAB(NIN)%S_NSEGM   = 1+NMN
            INTBUF_TAB(NIN)%S_LNSV    = NST 
            INTBUF_TAB(NIN)%S_LMSR    = NMT 

C----   size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1 
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_CSTS  = 2*NSN
            INTBUF_TAB(NIN)%S_CSTM  = 2*NMN
            INTBUF_TAB(NIN)%S_STFNS  = NSN 
            INTBUF_TAB(NIN)%S_STFNM  = NMN 
            INTBUF_TAB(NIN)%S_STFS  = NRTS 
            INTBUF_TAB(NIN)%S_STFM  = NRTM 
            INTBUF_TAB(NIN)%S_FRICOS  = 3*NSN 
            INTBUF_TAB(NIN)%S_FRICOM  = 3*NMN 
            INTBUF_TAB(NIN)%S_FCONT  = 1 
            INTBUF_TAB(NIN)%S_FS  = 3*NSN
            INTBUF_TAB(NIN)%S_FM  = 3*NMN
            INTBUF_TAB(NIN)%S_RMAS  = 2 
            INTBUF_TAB(NIN)%S_ANSMX0  = 2
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 7) THEN !and ITYP == 18
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV = NSN 
          INTBUF_TAB(NIN)%S_MSR = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          !IRECTS is mandatory for auto-impactant interfaces
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          IF(INTTH > 0 ) THEN
            INTBUF_TAB(NIN)%S_IELES = NRTM 
            INTBUF_TAB(NIN)%S_IELEC = NSN 
          ENDIF
          IF (IFQ /= 0)INTBUF_TAB(NIN)%S_IFPEN = MULTIMP*NCONT 
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_TYPSUB  = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NSN+1 
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM
            INTBUF_TAB(NIN)%S_INFLG_SUBS  = NISUBS 
            INTBUF_TAB(NIN)%S_INFLG_SUBM  = NISUBM 
          ENDIF
          INTBUF_TAB(NIN)%S_MSEGTYP = NRTM 
          IF (FLAGREMNODE == 2 ) THEN
            INTBUF_TAB(NIN)%S_KREMNODE = 2*(NRTM + 1)
            INTBUF_TAB(NIN)%S_REMNODE  = NREMNODE 
          ENDIF

C----     size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_STFNS   = NSN
          IF(IGAP > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_M   = NRTM 
            INTBUF_TAB(NIN)%S_GAP_S   = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_STFM   = NRTM 

          IF(ITIED/=0)
     .      INTBUF_TAB(NIN)%S_CAND_F  = 8*MULTIMP*NCONT

          IF(INACTI == 5 .OR. INACTI == 6 .OR. INACTI == 7) THEN
            INTBUF_TAB(NIN)%S_CAND_P   = MULTIMP*NCONT 
          ENDIF
          INTBUF_TAB(NIN)%S_XSAV   = 3*MIN(NUMN+NUMFAKENODIGEO,NSN+NMN) 
          INTBUF_TAB(NIN)%S_CRIT   = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P   = 10 
          IF (IFQ/=0) THEN 
            INTBUF_TAB(NIN)%S_XFILTR   = 1 
            INTBUF_TAB(NIN)%S_FTSAVX   = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_FTSAVY   = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_FTSAVZ   = MULTIMP*NCONT 
          ENDIF
          IF (INTTH > 0 ) THEN 
            INTBUF_TAB(NIN)%S_AREAS   = NSN 
          ENDIF
          IF (IGAP == 3 ) THEN 
            INTBUF_TAB(NIN)%S_GAP_ML  = NRTM
            INTBUF_TAB(NIN)%S_GAP_SL  = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_NIGE = NSN_IGE+NMN_IGE
          INTBUF_TAB(NIN)%S_MASSIGE = NSN_IGE+NMN_IGE
          INTBUF_TAB(NIN)%S_RIGE = 3*(NSN_IGE+NMN_IGE)
          INTBUF_TAB(NIN)%S_XIGE = 3*(NSN_IGE+NMN_IGE)
          INTBUF_TAB(NIN)%S_VIGE = 3*(NSN_IGE+NMN_IGE)
          IF (INTFRIC/=0) THEN
            INTBUF_TAB(NIN)%S_IPARTFRICS  = NSN 
            INTBUF_TAB(NIN)%S_IPARTFRICM  = NRTM
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC >0) THEN
               INTBUF_TAB(NIN)%S_IREP_FRICM  = NRTM 
               INTBUF_TAB(NIN)%S_DIR_FRICM  = 2*NRTM 
            ENDIF
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 8) THEN
C=======================================================================
!         IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_I_STOK   = 1 

            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_MSR = NMN 
!           INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM  = 4*NRTM 
            INTBUF_TAB(NIN)%S_IRTLM   = NSN 
!           INTBUF_TAB(NIN)%S_IRTLS   = NMN 
            INTBUF_TAB(NIN)%S_ILOCS   = NSN 
!           INTBUF_TAB(NIN)%S_SL8     = NSN 
!           INTBUF_TAB(NIN)%S_ILOCM   = NMN 
            INTBUF_TAB(NIN)%S_IRTLOM  = NSN 
!           INTBUF_TAB(NIN)%S_IRTLOS  = NMN 
!           INTBUF_TAB(NIN)%S_NSEGS   = 1+NSN
            INTBUF_TAB(NIN)%S_NSEGM   = 1+NMN
            INTBUF_TAB(NIN)%S_LNSV    = NST 
            INTBUF_TAB(NIN)%S_LMSR    = NMT 

C----   size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_CSTS  = 2*NSN 
c           INTBUF_TAB(NIN)%S_CSTM  = 2*NMN 
C           INTBUF_TAB(NIN)%S_STFNS  = NSN 
            INTBUF_TAB(NIN)%S_STFNM  = NMN 
C           INTBUF_TAB(NIN)%S_STFS  = NRTS 
            INTBUF_TAB(NIN)%S_STFM  = NRTM 
C           INTBUF_TAB(NIN)%S_FRICOS  = 3*NSN 
C           INTBUF_TAB(NIN)%S_FRICOM  = 3*NMN 
            IF(IFORM==2)THEN
              INTBUF_TAB(NIN)%S_FTSAVX   = NSN 
              INTBUF_TAB(NIN)%S_FTSAVY   = NSN 
              INTBUF_TAB(NIN)%S_FTSAVZ   = NSN 
            ENDIF
            INTBUF_TAB(NIN)%S_GAPN  = NRTM 
            INTBUF_TAB(NIN)%S_STF8  = NRTM 
            INTBUF_TAB(NIN)%S_T8    = 1
!         ENDIF

C=======================================================================
        ELSEIF (ITYP == 9) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV     = NSN 
          INTBUF_TAB(NIN)%S_MSR     = NMN 
          INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM  = 4*NRTM 
          INTBUF_TAB(NIN)%S_IRTLM   = NSN 
          INTBUF_TAB(NIN)%S_IRTLS   = NMN 
          INTBUF_TAB(NIN)%S_ILOCS   = NSN 
          INTBUF_TAB(NIN)%S_ILOCM   = NMN 
          INTBUF_TAB(NIN)%S_IRTLOM  = NSN 
          INTBUF_TAB(NIN)%S_IRTLOS  = NMN 
          INTBUF_TAB(NIN)%S_NSEGS   = 1+NSN
          INTBUF_TAB(NIN)%S_NSEGM   = 1+NMN
          INTBUF_TAB(NIN)%S_LNSV    = NST 
          INTBUF_TAB(NIN)%S_LMSR    = NMT 
          INTBUF_TAB(NIN)%S_IELES   = NRTS 
          INTBUF_TAB(NIN)%S_IELEM   = NRTM 

C----     size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR
          INTBUF_TAB(NIN)%S_CSTS        = 2*NSN 
          INTBUF_TAB(NIN)%S_CSTM        = 2*NMN 
          INTBUF_TAB(NIN)%S_STFNS       = NSN 
          INTBUF_TAB(NIN)%S_STFNM       = NMN 
          INTBUF_TAB(NIN)%S_STFS        = NRTS 
          INTBUF_TAB(NIN)%S_STFM        = 2*NRTM
          INTBUF_TAB(NIN)%S_FRICOS      = 3*NSN 
          INTBUF_TAB(NIN)%S_FRICOM      = 3*NMN 
          INTBUF_TAB(NIN)%S_N           = 3*NSN 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P = 10 
          IF (IFQ/=0) THEN 
            INTBUF_TAB(NIN)%S_XFILTR = 1 
            INTBUF_TAB(NIN)%S_FTSAV  = 3*NSN 
          ENDIF  

C=======================================================================
        ELSEIF (ITYP == 10) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV = NSN 
          INTBUF_TAB(NIN)%S_MSR = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NSN+1 
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM
          ENDIF
          INTBUF_TAB(NIN)%S_MSEGTYP = NRTM

C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_STFNS   = NSN 
          INTBUF_TAB(NIN)%S_GAP_M   = NRTM
          INTBUF_TAB(NIN)%S_GAP_S   = NSN 
          INTBUF_TAB(NIN)%S_STFM    = NRTM
          INTBUF_TAB(NIN)%S_CAND_F  = 6*MULTIMP*NCONT
          INTBUF_TAB(NIN)%S_XSAV    = 3*MIN(NUMN,NSN+NMN) 
          INTBUF_TAB(NIN)%S_CRIT    = 12 

C=======================================================================
        ELSEIF (ITYP == 11) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV = NSN 
          INTBUF_TAB(NIN)%S_MSR = NMN 
          IF( I11FLAG == 0 ) THEN
            INTBUF_TAB(NIN)%S_IRECTS = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM = 4*NRTM 
          ELSE
            !on I11EDGE we compact IRTS and IRTM on 2*NRTS and 2*NRTM
            INTBUF_TAB(NIN)%S_IRECTS = 2*NRTS
            INTBUF_TAB(NIN)%S_IRECTM = 2*NRTM
          ENDIF
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_ADCCM    = NRTM 
          INTBUF_TAB(NIN)%S_CHAIN    = 2*MULTIMP*NCONT 
          IF (MFROT == 2) THEN !IPARI(30)
            INTBUF_TAB(NIN)%S_IFPEN = MULTIMP*NCONT
          ENDIF
          INTBUF_TAB(NIN)%S_CAND_MAX = 0        


C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_GAP_S   = NRTS 
          INTBUF_TAB(NIN)%S_GAP_M   = NRTM 
          INTBUF_TAB(NIN)%S_STFS    = NRTS 
          INTBUF_TAB(NIN)%S_STFM    = NRTM 
          INTBUF_TAB(NIN)%S_XSAV    = 3*MIN(NUMN,NSN+NMN) 
          INTBUF_TAB(NIN)%S_PENIS   = 2*NRTS 
          INTBUF_TAB(NIN)%S_PENIM   = 2*NRTM 
          INTBUF_TAB(NIN)%S_CRIT    = 12
          IF (MFROT == 2) THEN 
            INTBUF_TAB(NIN)%S_FTSAVX   = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_FTSAVY   = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_FTSAVZ   = MULTIMP*NCONT 
          ENDIF
          IF (IGAP == 3 ) THEN 
            INTBUF_TAB(NIN)%S_GAP_ML  = NRTM 
            INTBUF_TAB(NIN)%S_GAP_SL  = NRTS 
          ENDIF
          IF(INTTH > 0 ) THEN
            INTBUF_TAB(NIN)%S_IELEC = NRTS 
            INTBUF_TAB(NIN)%S_IELES = NRTM 
            INTBUF_TAB(NIN)%S_AREAS = NRTS 
            INTBUF_TAB(NIN)%S_AREAM = NRTM 
          ENDIF   
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_TYPSUB  = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NRTS+1
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM
            INTBUF_TAB(NIN)%S_INFLG_SUBS  = NISUBS 
            INTBUF_TAB(NIN)%S_INFLG_SUBM  = NISUBM 
          ENDIF

          IF (INTFRIC/=0) THEN
            INTBUF_TAB(NIN)%S_IPARTFRICS  = NRTS 
            INTBUF_TAB(NIN)%S_IPARTFRICM  = NRTM 
          ENDIF
C
          IF (FLAGREMNODE == 2 ) THEN
            INTBUF_TAB(NIN)%S_KREMNODE = 2*(NRTM + 1) 
            INTBUF_TAB(NIN)%S_REMNODE  = NREMNODE 
          ENDIF
C=======================================================================
        ELSEIF (ITYP == 12) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_MSR = NMN 
            INTBUF_TAB(NIN)%S_IRECTS = 4*NRTS 
            INTBUF_TAB(NIN)%S_IRECTM = 4*NRTM 
            INTBUF_TAB(NIN)%S_IRTLM  = NSN 
            INTBUF_TAB(NIN)%S_IRTLS  = NMN 
            INTBUF_TAB(NIN)%S_ILOCS  = NSN 
            INTBUF_TAB(NIN)%S_ILOCM  = NMN 
            INTBUF_TAB(NIN)%S_NSEGS  = 1+NSN 
            INTBUF_TAB(NIN)%S_NSEGM  = 1+NMN
            INTBUF_TAB(NIN)%S_LNSV   = NST 
            INTBUF_TAB(NIN)%S_LMSR   = NMT 
            INTBUF_TAB(NIN)%S_IELES  = NRTS 
            INTBUF_TAB(NIN)%S_IELEM  = NRTM 
            INTBUF_TAB(NIN)%S_FCOUNT = NSN 

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1 
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_CSTS  = 2*NSN 
            INTBUF_TAB(NIN)%S_CSTM  = 2*NMN 
            IF(ILEV>0)THEN
              INTBUF_TAB(NIN)%S_STFNS  = NSN
              INTBUF_TAB(NIN)%S_STFNM  = NMN
            ENDIF
            INTBUF_TAB(NIN)%S_NMAS  = NMN       
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 14) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_NSV = NSN 
            INTBUF_TAB(NIN)%S_IMPACT = NSN 
            INTBUF_TAB(NIN)%S_KSURF  = 1 

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1 
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_CIMP  = 3*NSN 
            INTBUF_TAB(NIN)%S_NIMP  = 3*NSN  
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 15) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
            INTBUF_TAB(NIN)%S_NSV = NSN
            INTBUF_TAB(NIN)%S_IMPACT = 4*NRTS 
            INTBUF_TAB(NIN)%S_KSURF  = 1 

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1 
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_IOLD  = 3*4*NRTS 
            INTBUF_TAB(NIN)%S_HOLD  = 3*4*NRTS  
            INTBUF_TAB(NIN)%S_NOLD  = 3*4*NRTS 
            INTBUF_TAB(NIN)%S_DOLD  = 3*4*NRTS  
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 16) THEN
C=======================================================================
          IF(PROC==0)THEN
C----   size integers arrays
            INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
            NCONT = NSN
            INTBUF_TAB(NIN)%S_CAND_E  = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_CAND_N  = MULTIMP*NCONT 

C----     size real arrays
            INTBUF_TAB(NIN)%S_STFAC       = 1 
            INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
            INTBUF_TAB(NIN)%S_XSAV  = 3*NUMN 
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 17) THEN
C=======================================================================
C----   size integers arrays
          NCONT = NSN
          INTBUF_TAB(NIN)%S_IRECTS  = 4*NRTS 
          INTBUF_TAB(NIN)%S_CAND_E  = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N  = MULTIMP*NCONT 

C----     size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR
          INTBUF_TAB(NIN)%S_XSAV        = 3*NUMN
          INTBUF_TAB(NIN)%S_KS          = 2*NSN 
          INTBUF_TAB(NIN)%S_KM          = 2*NME 
          INTBUF_TAB(NIN)%S_FROTS       = 7*NSN 
          INTBUF_TAB(NIN)%S_FROTM       = 7*NME 

C=======================================================================
        ELSEIF (ITYP == 18) THEN
C=======================================================================

c         TYPE 7 + INACTI = 7

C=======================================================================
        ELSEIF (ITYP == 20) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV = NSN
          INTBUF_TAB(NIN)%S_MSR = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_I_STOK_E = 1 
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_DAANC6   = NLN * 18 * 2  
          IF(INTTH > 0 ) THEN
            INTBUF_TAB(NIN)%S_IELES = NRTM 
            INTBUF_TAB(NIN)%S_IELEC = NSN  
          ENDIF
          IF (IFQ /= 0)INTBUF_TAB(NIN)%S_IFPEN = MULTIMP*NCONT 
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NSN+1 
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM
          ENDIF
          INTBUF_TAB(NIN)%S_NBINFLG   = NLN  
          INTBUF_TAB(NIN)%S_MBINFLG   = NRTM 
          INTBUF_TAB(NIN)%S_NLG       = NLN  
          INTBUF_TAB(NIN)%S_ISLINS    = 2*NLINS 
          INTBUF_TAB(NIN)%S_ISLINM    = 2*NLINM 
          INTBUF_TAB(NIN)%S_IXLINS    = 2*NLINS 
          INTBUF_TAB(NIN)%S_IXLINM    = 2*NLINM 
          INTBUF_TAB(NIN)%S_NSVL      = NSNE
          INTBUF_TAB(NIN)%S_MSRL      = NMNE
          INTBUF_TAB(NIN)%S_LCAND_N   = MULTIMP*NCONTE 
          INTBUF_TAB(NIN)%S_LCAND_S   = MULTIMP*NCONTE 
          INTBUF_TAB(NIN)%S_ADCCM20   = NLINM 
          INTBUF_TAB(NIN)%S_CHAIN20   = 2*MULTIMP*NCONTE 

C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_XA       = 3*NLN
          INTBUF_TAB(NIN)%S_VA       = 3*NLN
          INTBUF_TAB(NIN)%S_STFA     = NLN 
          IF(IGAP > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_M  = NRTM
            INTBUF_TAB(NIN)%S_GAP_S  = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_STFM   = NRTM 
          IF(INACTI == 5 .OR. INACTI == 6 .OR. INACTI == 7) THEN
            INTBUF_TAB(NIN)%S_CAND_P  = MULTIMP*NCONT 
          ENDIF
          INTBUF_TAB(NIN)%S_XSAV      = 3*(NSN+NMN)
          INTBUF_TAB(NIN)%S_PENIS     = 2*NSN 
          INTBUF_TAB(NIN)%S_PENIM     = 2*NRTM
          INTBUF_TAB(NIN)%S_AVX_ANCR  = 9*NLN 
          INTBUF_TAB(NIN)%S_GAP_SH    = NRTM 
          INTBUF_TAB(NIN)%S_CRIT      = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P   = 10
          IF (IFQ/=0) THEN 
            INTBUF_TAB(NIN)%S_XFILTR  = 1
            INTBUF_TAB(NIN)%S_CAND_FX = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_CAND_FY = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_CAND_FZ = MULTIMP*NCONT 
          ENDIF
          IF (INTTH > 0 ) THEN 
            INTBUF_TAB(NIN)%S_AREAS  = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_GAP_SE    = NLINS 
          INTBUF_TAB(NIN)%S_GAP_ME    = NLINM 
          INTBUF_TAB(NIN)%S_STFS      = NLINS 
          INTBUF_TAB(NIN)%S_STF       = NLINM 
          INTBUF_TAB(NIN)%S_STFNE     = NSNE 
          INTBUF_TAB(NIN)%S_CRITX     = 3*(NSNE+NMNE)
          INTBUF_TAB(NIN)%S_PENISE    = 2*NLINS
          INTBUF_TAB(NIN)%S_PENIME    = 2*NLINM
          INTBUF_TAB(NIN)%S_PENIA     = 5*NLN 
          INTBUF_TAB(NIN)%S_ALPHAK    = 3*NLN 

C=======================================================================
        ELSEIF (ITYP == 21) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV      = NSN 
          INTBUF_TAB(NIN)%S_MSR      = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_IRTLM    = 2*NSN
          INTBUF_TAB(NIN)%S_MSR21    = NMNG 
          INTBUF_TAB(NIN)%S_IELES    = NRTS 
          INTBUF_TAB(NIN)%S_IFPEN    = NSN 
          IF(INTTH == 2 .OR. IFLAGLOADP > 0) THEN
            INTBUF_TAB(NIN)%S_MNDD   = NMNG 
            INTBUF_TAB(NIN)%S_MSR_L  = NMNG 
          ENDIF
          INTBUF_TAB(NIN)%S_MSEGTYP  = NRTM 

C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_CSTS        = 2*NSN 
          INTBUF_TAB(NIN)%S_STFNS       = NSN 
          IF(IGAP==2)THEN
            INTBUF_TAB(NIN)%S_GAP_S     = 2*NSN  !GAP_S0
          ELSE
            INTBUF_TAB(NIN)%S_GAP_S     = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_STF         = NRTM 
          INTBUF_TAB(NIN)%S_PENIS       = NSN !
          INTBUF_TAB(NIN)%S_XSAV        = 3*(NSN+2) +3*NSN
          INTBUF_TAB(NIN)%S_NOD_NORMAL  = 3*NMNG 
          INTBUF_TAB(NIN)%S_RCURV       = NRTM 
          INTBUF_TAB(NIN)%S_XM0         = 3*NMNG 
          INTBUF_TAB(NIN)%S_ANGLM       = NRTM 
          INTBUF_TAB(NIN)%S_CRIT        = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FROT_P = 10 
          INTBUF_TAB(NIN)%S_ALPHA0      = 1 
          INTBUF_TAB(NIN)%S_FTSAVX      = NSN 
          INTBUF_TAB(NIN)%S_FTSAVY      = NSN 
          INTBUF_TAB(NIN)%S_FTSAVZ      = NSN 
          INTBUF_TAB(NIN)%S_AREAS       = NSN 
          IF(INTTH/=0)THEN
            INTBUF_TAB(NIN)%S_AS        = NSN 
            INTBUF_TAB(NIN)%S_BS        = NSN 
          ENDIF
          IF(IGAP>=1)THEN
            INTBUF_TAB(NIN)%S_THKNOD0   = NSN 
          ENDIF

C=======================================================================
        ELSEIF (ITYP == 22) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV = NSN 
          INTBUF_TAB(NIN)%S_MSR = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT
          IF(INTTH > 0 ) THEN
            INTBUF_TAB(NIN)%S_IELES = NRTM 
            INTBUF_TAB(NIN)%S_IELEC = NSN
          ENDIF
          IF (IFQ /= 0)INTBUF_TAB(NIN)%S_IFPEN = MULTIMP*NCONT 
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NSN+1 
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM
          ENDIF

C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC       = 1 
          INTBUF_TAB(NIN)%S_VARIABLES   = NPARIR 
          INTBUF_TAB(NIN)%S_STFNS   = NSN 
          IF(IGAP > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_M   = NRTM
            INTBUF_TAB(NIN)%S_GAP_S   = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_STFM   = NRTM 
          IF(INACTI == 5 .OR. INACTI == 6 .OR. INACTI == 7) THEN
            INTBUF_TAB(NIN)%S_CAND_P   = MULTIMP*NCONT 
          ENDIF
          INTBUF_TAB(NIN)%S_XSAV   = 3*MIN(NUMN,NSN+NMN) 
          INTBUF_TAB(NIN)%S_CRIT   = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P   = 10 
          IF (IFQ/=0) THEN 
            INTBUF_TAB(NIN)%S_XFILTR   = 1 
            INTBUF_TAB(NIN)%S_FTSAVX   = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_FTSAVY   = MULTIMP*NCONT 
            INTBUF_TAB(NIN)%S_FTSAVZ   = MULTIMP*NCONT 
          ENDIF
          IF (INTTH > 0 ) THEN 
            INTBUF_TAB(NIN)%S_AREAS   = NSN 
          ENDIF
          IF (IGAP == 3 ) THEN 
            INTBUF_TAB(NIN)%S_GAP_ML  = NRTM 
            INTBUF_TAB(NIN)%S_GAP_SL  = NSN 
          ENDIF  

C=======================================================================
        ELSEIF (ITYP == 23) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV      = NSN 
          INTBUF_TAB(NIN)%S_MSR      = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_IFPEN    = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_MSEGTYP  = NRTM 

C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC     = 1
          INTBUF_TAB(NIN)%S_VARIABLES = NPARIR 
          INTBUF_TAB(NIN)%S_STFNS     = NSN 
          IF(IGAP > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_M   = NRTM
            INTBUF_TAB(NIN)%S_GAP_S   = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_STFM      = NRTM
          INTBUF_TAB(NIN)%S_CAND_P    = MULTIMP*NCONT
          INTBUF_TAB(NIN)%S_XSAV      = 3*MIN(NUMN,NSN+NMN) 
          INTBUF_TAB(NIN)%S_CRIT      = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P   = 10 
          INTBUF_TAB(NIN)%S_XFILTR    = 1 
          INTBUF_TAB(NIN)%S_FTSAVX    = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_FTSAVY    = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_FTSAVZ    = MULTIMP*NCONT 

C=======================================================================
        ELSEIF (ITYP == 24) THEN
C=======================================================================
          IEDGE4 = IPARI(59,NIN)
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV      = NSN 
          INTBUF_TAB(NIN)%S_MSR      = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 1 
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_IRTLM    = 2*NSN 
          INTBUF_TAB(NIN)%S_MVOISIN  = 4*NRTM
          INTBUF_TAB(NIN)%S_NVOISIN  = 8*NRTM
          INTBUF_TAB(NIN)%S_MSEGLO   = NRTM 
          INTBUF_TAB(NIN)%S_MSEGTYP24= NRTM 
          IF(INTTH > 0 ) THEN
            INTBUF_TAB(NIN)%S_IELES = NRTM 
            INTBUF_TAB(NIN)%S_IELEC = NSN 
          ENDIF
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_TYPSUB  = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NSN+1 
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM
            INTBUF_TAB(NIN)%S_INFLG_SUBS  = NISUBS 
            INTBUF_TAB(NIN)%S_INFLG_SUBM  = NISUBM 
          ENDIF
          IF (FLAGREMNODE == 2 ) THEN
            INTBUF_TAB(NIN)%S_KREMNODE = 2*(NRTM + 1)
            INTBUF_TAB(NIN)%S_REMNODE  = NREMNODE 
          ENDIF
          IF (IEDGE4 > 0) THEN
            INTBUF_TAB(NIN)%S_IRTSE    = 5*NRTSE 
            INTBUF_TAB(NIN)%S_IS2SE    = 2*NSNE 
            INTBUF_TAB(NIN)%S_IS2PT    = NSNE 
            INTBUF_TAB(NIN)%S_ISPT2    = NSN
            INTBUF_TAB(NIN)%S_ISEGPT   = NSN 
            INTBUF_TAB(NIN)%S_IS2ID    = NSNE 
          ENDIF
          IF (IEDGE > 0) THEN
            INTBUF_TAB(NIN)%S_NBINFLG = NSN 
            INTBUF_TAB(NIN)%S_MBINFLG = NRTM
            INTBUF_TAB(NIN)%S_ISEADD  = NSN 
            INTBUF_TAB(NIN)%S_ISEDGE  = L24ADD 
            INTBUF_TAB(NIN)%S_CAND_T  = MULTIMP * NCONT  
          ELSEIF (ILEV == 2) THEN
            INTBUF_TAB(NIN)%S_NBINFLG = NSN 
            INTBUF_TAB(NIN)%S_MBINFLG = NRTM 
          ENDIF
          !TYPE24+XFEM
          IF(INTPLY > 0)THEN
            INTBUF_TAB(NIN)%S_ISEG_PXFEM = NRTM    
            INTBUF_TAB(NIN)%S_ISEG_PLY   = 12*NRTM 
          ENDIF
     
C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC      = 1
          INTBUF_TAB(NIN)%S_VARIABLES  = NPARIR
          INTBUF_TAB(NIN)%S_STFNS      = NSN 
          IF(IGAP > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_M  = NRTM
            INTBUF_TAB(NIN)%S_GAP_S  = NSN 
          ENDIF
          INTBUF_TAB(NIN)%S_STFM     = NRTM 
          INTBUF_TAB(NIN)%S_XSAV     = 3*MIN(NUMN,NSN+NMN) 
          INTBUF_TAB(NIN)%S_GAPN_M   = NMN 
          INTBUF_TAB(NIN)%S_CRIT     = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P   = 10
          IF (IFQ/=0) INTBUF_TAB(NIN)%S_XFILTR    = 1 
          INTBUF_TAB(NIN)%S_SECND_FR  = 6*NSN
          IF (INTTH > 0 )INTBUF_TAB(NIN)%S_AREAS  = NSN 
          INTBUF_TAB(NIN)%S_PENE_OLD   = 5*NSN 
          INTBUF_TAB(NIN)%S_STIF_OLD   = 2*NSN 
          INTBUF_TAB(NIN)%S_TIME_S     = NSN   
          INTBUF_TAB(NIN)%S_GAP_NM     = 12*NRTM   
          IF(IEDGE > 0 ) INTBUF_TAB(NIN)%S_EDGE8L2     = NSN  
          IF(NSNE > 0 ) THEN
           INTBUF_TAB(NIN)%S_XFIC     = 3*NSNE 
           INTBUF_TAB(NIN)%S_VFIC     = 3*NSNE 
           INTBUF_TAB(NIN)%S_MSFIC    = NSNE 
          END IF           
          IF(INTKG > 0 ) THEN
            INTBUF_TAB(NIN)%S_NOD_2RY_LGTH  = NSN
            INTBUF_TAB(NIN)%S_NOD_MAS_LGTH  = NMN
          ENDIF
          IF(INTPLY > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_N0   = 12*NRTM     
            INTBUF_TAB(NIN)%S_DGAP_NM  = 4*NRTM        
            INTBUF_TAB(NIN)%S_DGAP_M   = NRTM        
            INTBUF_TAB(NIN)%S_DELTA_PMAX_DGAP  = 1   
          ENDIF
          INTBUF_TAB(NIN)%S_ICONT_I     = NSN   

          IF (INTFRIC/=0) THEN
            INTBUF_TAB(NIN)%S_IPARTFRICS  = NSN 
            INTBUF_TAB(NIN)%S_IPARTFRICM  = NRTM 
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC >0) THEN
               INTBUF_TAB(NIN)%S_IREP_FRICM  = NRTM 
               INTBUF_TAB(NIN)%S_DIR_FRICM  = 2*NRTM 
            ENDIF
          ENDIF

          IF(INTNITSCHE > 0 ) THEN
            INTBUF_TAB(NIN)%S_IELNRTS = NRTS 
            INTBUF_TAB(NIN)%S_ADRECTS = 4*NRTS 
            INTBUF_TAB(NIN)%S_FACNRTS = NRTS 
          ENDIF


C=======================================================================
        ELSEIF (ITYP == 25) THEN
C=======================================================================
C----   size integers arrays
          INTBUF_TAB(NIN)%S_NSV      = NSN 
          INTBUF_TAB(NIN)%S_MSR      = NMN 
          INTBUF_TAB(NIN)%S_I_STOK   = 3 
          IF(IEDGE/=0) INTBUF_TAB(NIN)%S_I_STOK_E = 2
          INTBUF_TAB(NIN)%S_IRECTS   = 4*NRTS 
          INTBUF_TAB(NIN)%S_IRECTM   = 4*NRTM 
          INTBUF_TAB(NIN)%S_CAND_E   = MULTIMP*NCONT
          INTBUF_TAB(NIN)%S_CAND_N   = MULTIMP*NCONT 
          INTBUF_TAB(NIN)%S_IRTLM    = 4*NSN
          INTBUF_TAB(NIN)%S_MVOISIN  = 4*NRTM
          INTBUF_TAB(NIN)%S_NVOISIN  = 0
          INTBUF_TAB(NIN)%S_EVOISIN  = 4*NRTM
          INTBUF_TAB(NIN)%S_MSEGLO   = NRTM 
          INTBUF_TAB(NIN)%S_MSEGTYP24= NRTM 
          IF(INTTH > 0 ) THEN
           INTBUF_TAB(NIN)%S_IELEM = NRTM 
           INTBUF_TAB(NIN)%S_IELES = NSN 
          ENDIF
          IF (NISUB /= 0) THEN
            INTBUF_TAB(NIN)%S_LISUB   = NISUB 
            INTBUF_TAB(NIN)%S_TYPSUB  = NISUB 
            INTBUF_TAB(NIN)%S_ADDSUBS = NSN+1 
            INTBUF_TAB(NIN)%S_ADDSUBM = NRTM+1 
            INTBUF_TAB(NIN)%S_LISUBS  = NISUBS 
            INTBUF_TAB(NIN)%S_LISUBM  = NISUBM 
            INTBUF_TAB(NIN)%S_INFLG_SUBS  = NISUBS 
            INTBUF_TAB(NIN)%S_INFLG_SUBM  = NISUBM 
            IF (IEDGE /= 0) THEN
              INTBUF_TAB(NIN)%S_ADDSUBE    = NEDGE+1 
              INTBUF_TAB(NIN)%S_LISUBE     = NISUBE 
              INTBUF_TAB(NIN)%S_INFLG_SUBE = NISUBE
            ENDIF
          ENDIF

          IF (FLAGREMNODE == 2 ) THEN
            INTBUF_TAB(NIN)%S_KREMNODE = 2*(NRTM + 1)
            INTBUF_TAB(NIN)%S_REMNODE  = NREMNODE 
c
            INTBUF_TAB(NIN)%S_KREMNOR = NSN + 1
            INTBUF_TAB(NIN)%S_REMNOR  = NREMNOR 
          ENDIF
          IF (ILEV == 2) THEN
C           ilev=1: SURFACE1; 2: SURFACE1/SURFACE2; 3: NODE/SURFACE
            INTBUF_TAB(NIN)%S_NBINFLG = NSN
            INTBUF_TAB(NIN)%S_MBINFLG = NRTM
            IF (IEDGE /= 0) THEN
              INTBUF_TAB(NIN)%S_EBINFLG = NEDGE
            ENDIF
          ENDIF
          IF(IEDGE/=0)THEN
            MULTIMPE=IPARI(87,NIN)
            MULTIMPS=IPARI(89,NIN)
            NCONTE  =IPARI(88,NIN)
            INTBUF_TAB(NIN)%S_CANDM_E2E = MULTIMPE*NCONTE
            INTBUF_TAB(NIN)%S_CANDS_E2E = MULTIMPE*NCONTE
            INTBUF_TAB(NIN)%S_CAND_P    = MULTIMPE*NCONTE
C           IF(INACTI==5) INTBUF_TAB(NIN)%S_CAND_P    = MULTIMPE*NCONTE
C           WRITE(6,*) NCONTE
C           WRITE(6,*) __FILE__,NIN," S_CAND_P",INTBUF_TAB(NIN)%S_CAND_P 

            NCONTE  =IPARI(88,NIN)
            INTBUF_TAB(NIN)%S_CANDM_E2S = MULTIMPS*NCONTE
            INTBUF_TAB(NIN)%S_CANDS_E2S = MULTIMPS*NCONTE
            INTBUF_TAB(NIN)%S_CAND_PS   = 4*MULTIMPS*NCONTE
C           WRITE(6,*) __FILE__,NIN," S_CAND_PS",INTBUF_TAB(NIN)%S_CAND_PS 
C           IF(INACTI==5) INTBUF_TAB(NIN)%S_CAND_PS   = 4*MULTIMPS*NCONTE
            INTBUF_TAB(NIN)%S_FTSAVX_E   = MULTIMPE*NCONTE 
            INTBUF_TAB(NIN)%S_FTSAVY_E   = MULTIMPE*NCONTE 
            INTBUF_TAB(NIN)%S_FTSAVZ_E   = MULTIMPE*NCONTE 
            INTBUF_TAB(NIN)%S_FTSAVX_E2S   = 4*MULTIMPS*NCONTE 
            INTBUF_TAB(NIN)%S_FTSAVY_E2S   = 4*MULTIMPS*NCONTE 
            INTBUF_TAB(NIN)%S_FTSAVZ_E2S   = 4*MULTIMPS*NCONTE 
            IF (IFQ /= 0)INTBUF_TAB(NIN)%S_IFPEN_E = MULTIMPE*NCONTE 
            IF (IFQ /= 0)INTBUF_TAB(NIN)%S_IFPEN_E2S = MULTIMPS*NCONTE 
          ELSE 
            INTBUF_TAB(NIN)%S_CANDM_E2E = 0
            INTBUF_TAB(NIN)%S_CANDS_E2E = 0
            INTBUF_TAB(NIN)%S_CAND_P    = 0
            INTBUF_TAB(NIN)%S_CANDM_E2S = 0
            INTBUF_TAB(NIN)%S_CANDS_E2S = 0
            INTBUF_TAB(NIN)%S_CAND_PS   = 0
            INTBUF_TAB(NIN)%S_FTSAVX_E   = 0 
            INTBUF_TAB(NIN)%S_FTSAVY_E   = 0
            INTBUF_TAB(NIN)%S_FTSAVZ_E   = 0 
            INTBUF_TAB(NIN)%S_FTSAVX_E2S   = 0 
            INTBUF_TAB(NIN)%S_FTSAVY_E2S   = 0 
            INTBUF_TAB(NIN)%S_FTSAVZ_E2S   = 0
            INTBUF_TAB(NIN)%S_IFPEN_E  = 0
            INTBUF_TAB(NIN)%S_IFPEN_E2S  = 0
          ENDIF
          INTBUF_TAB(NIN)%S_GAPE      = NEDGE
          IF(IGAP==3) INTBUF_TAB(NIN)%S_GAP_E_L   = NEDGE
          INTBUF_TAB(NIN)%S_STFE      = NEDGE
          INTBUF_TAB(NIN)%S_ADMSR     = 4*NRTM
          INTBUF_TAB(NIN)%S_LEDGE     = NLEDGE*NEDGE
          INTBUF_TAB(NIN)%S_LBOUND    = NADMSR
          INTBUF_TAB(NIN)%S_ACTNOR    = NRTM
          INTBUF_TAB(NIN)%S_FARM      = 4*MULTIMP*NCONT
          INTBUF_TAB(NIN)%S_ADSKYN    = NADMSR+1
          INTBUF_TAB(NIN)%S_IADNOR    = 4*NRTM
          INTBUF_TAB(NIN)%S_ISLIDE    = 4*NSN
          INTBUF_TAB(NIN)%S_KNOR2MSR  = NADMSR+1
          INTBUF_TAB(NIN)%S_NOR2MSR   = 4*NRTM
          INTBUF_TAB(NIN)%S_CAND_OPT_N= MULTIMP*NCONT
          INTBUF_TAB(NIN)%S_CAND_OPT_E= MULTIMP*NCONT
          IF (IVIS2==-1) THEN
             INTBUF_TAB(NIN)%S_IF_ADH   = NSN
          ENDIF
          INTBUF_TAB(NIN)%S_CANDL_MAX = 0        
          INTBUF_TAB(NIN)%S_CANDS_MAX = 0        
C----   size real arrays
          INTBUF_TAB(NIN)%S_STFAC      = 1 
          INTBUF_TAB(NIN)%S_VARIABLES  = NPARIR 
          INTBUF_TAB(NIN)%S_STFNS      = NSN
          IF(IGAP > 0)THEN
            INTBUF_TAB(NIN)%S_GAP_M  = NRTM 
            INTBUF_TAB(NIN)%S_GAP_S  = NSN 
          ENDIF
          IF (IGAP == 3 ) THEN 
            INTBUF_TAB(NIN)%S_GAP_ML  = NRTM
            INTBUF_TAB(NIN)%S_GAP_SL  = NSN
          ENDIF
          INTBUF_TAB(NIN)%S_STFM     = NRTM 
          INTBUF_TAB(NIN)%S_XSAV     = 3*MIN(NUMN,NSN+NMN) 
          INTBUF_TAB(NIN)%S_GAPN_M   = NMN 
          INTBUF_TAB(NIN)%S_CRIT     = 12 
          IF (MFROT/=0)INTBUF_TAB(NIN)%S_FRIC_P   = 10 
          IF (IFQ/=0) INTBUF_TAB(NIN)%S_XFILTR    = 1 
          INTBUF_TAB(NIN)%S_SECND_FR  = 6*NSN 
          IF (INTTH > 0 .OR.IVIS2 == -1)INTBUF_TAB(NIN)%S_AREAS  = NSN
          INTBUF_TAB(NIN)%S_PENE_OLD   = 5*NSN 
          INTBUF_TAB(NIN)%S_STIF_OLD   = 2*NSN 
          INTBUF_TAB(NIN)%S_TIME_S     = 2*NSN   
          INTBUF_TAB(NIN)%S_GAP_NM     = 4*NRTM   
          INTBUF_TAB(NIN)%S_ICONT_I    = NSN   
          INTBUF_TAB(NIN)%S_PENM       = 4*MULTIMP*NCONT  
          INTBUF_TAB(NIN)%S_DISTM      = MULTIMP*NCONT  
          INTBUF_TAB(NIN)%S_LBM        = 4*MULTIMP*NCONT  
          INTBUF_TAB(NIN)%S_LCM        = 4*MULTIMP*NCONT  
          INTBUF_TAB(NIN)%S_VTX_BISECTOR  = 2*3*NADMSR 
          INTBUF_TAB(NIN)%S_EDGE_BISECTOR = 12*NRTM  
          IF(IEDGE/=0)INTBUF_TAB(NIN)%S_E2S_NOD_NORMAL = 3*NADMSR   
          IF(IEDGE/=0)INTBUF_TAB(NIN)%S_E2S_ACTNOR = NADMSR  

          IF (INTFRIC/=0) THEN
            INTBUF_TAB(NIN)%S_IPARTFRICS  = NSN 
            INTBUF_TAB(NIN)%S_IPARTFRICM  = NRTM 
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC >0) THEN
               INTBUF_TAB(NIN)%S_IREP_FRICM  = NRTM 
               INTBUF_TAB(NIN)%S_DIR_FRICM  = 2*NRTM 
            ENDIF
            IF (IEDGE /= 0 )INTBUF_TAB(NIN)%S_IPARTFRIC_E = NEDGE
          ENDIF

          IF(ITHK==1) INTBUF_TAB(NIN)%S_GAPMSAV  = NRTM      

          IF (FLAGREMNODE == 2 .AND.IEDGE/=0 ) THEN
            INTBUF_TAB(NIN)%S_KREMNODE_EDG = 2*(NEDGE + 1)
            INTBUF_TAB(NIN)%S_REMNODE_EDG  = NREMNODE_EDG

            INTBUF_TAB(NIN)%S_KREMNODE_E2S = 2*(NRTM + 1)
            INTBUF_TAB(NIN)%S_REMNODE_E2S  = NREMNODE_E2S
          ENDIF

        ENDIF !END ALL INTERFACE TYPES

C=======================================================================
c       ALLOCATIONS
C=======================================================================
        !need to allocate only once at first passage in lectur for
        !global structure INTBUF_TAB (FLAG==1),
        !no allocation at second passage in ddsplit for local INTBUF_TAB_L (FLAG==2)

        IF(FLAG_ALLOCATE == 1)THEN
C=======================================================================
c ALLOCATE INTEGER ARRAYS
C=======================================================================
C
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRECTS,INTBUF_TAB(NIN)%S_IRECTS) !KD(10)
          INTBUF_TAB(NIN)%IRECTS(1:INTBUF_TAB(NIN)%S_IRECTS) = 0 !int11
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%S_IRECTM) !KD(11)
          INTBUF_TAB(NIN)%IRECTM(1:INTBUF_TAB(NIN)%S_IRECTM) = 0 !int11
          MY_ALLOCATE(INTBUF_TAB(NIN)%NSV,INTBUF_TAB(NIN)%S_NSV) !KD(12)
          INTBUF_TAB(NIN)%NSV(1:INTBUF_TAB(NIN)%S_NSV) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSR,INTBUF_TAB(NIN)%S_MSR) !KD(13)
          INTBUF_TAB(NIN)%MSR(1:INTBUF_TAB(NIN)%S_MSR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRTLM,INTBUF_TAB(NIN)%S_IRTLM) !KD(14)
          INTBUF_TAB(NIN)%IRTLM(1:INTBUF_TAB(NIN)%S_IRTLM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRUPT,INTBUF_TAB(NIN)%S_IRUPT) !KD(16)
          INTBUF_TAB(NIN)%IRUPT(1:INTBUF_TAB(NIN)%S_IRUPT) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%INORM,INTBUF_TAB(NIN)%S_INORM) !KD(17)
          INTBUF_TAB(NIN)%INORM(1:INTBUF_TAB(NIN)%S_INORM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%IELEC,INTBUF_TAB(NIN)%S_IELEC) !KD(25)
          INTBUF_TAB(NIN)%IELEC(1:INTBUF_TAB(NIN)%S_IELEC) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%IELES,INTBUF_TAB(NIN)%S_IELES) !KD(24)
          INTBUF_TAB(NIN)%IELES(1:INTBUF_TAB(NIN)%S_IELES) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%LISUB,INTBUF_TAB(NIN)%S_LISUB) !KD(28)
          MY_ALLOCATE(INTBUF_TAB(NIN)%TYPSUB,INTBUF_TAB(NIN)%S_TYPSUB) 
          INTBUF_TAB(NIN)%TYPSUB(1:INTBUF_TAB(NIN)%S_TYPSUB) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADDSUBS,INTBUF_TAB(NIN)%S_ADDSUBS) !KD(29)
          INTBUF_TAB(NIN)%ADDSUBS(1:INTBUF_TAB(NIN)%S_ADDSUBS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADDSUBM,INTBUF_TAB(NIN)%S_ADDSUBM) !KD(30)
          INTBUF_TAB(NIN)%ADDSUBM(1:INTBUF_TAB(NIN)%S_ADDSUBM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%LISUBS,INTBUF_TAB(NIN)%S_LISUBS) !KD(31)
          INTBUF_TAB(NIN)%LISUBS(1:INTBUF_TAB(NIN)%S_LISUBS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%LISUBM,INTBUF_TAB(NIN)%S_LISUBM) !KD(32)
          INTBUF_TAB(NIN)%LISUBM(1:INTBUF_TAB(NIN)%S_LISUBM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%INFLG_SUBS,INTBUF_TAB(NIN)%S_INFLG_SUBS)
          INTBUF_TAB(NIN)%INFLG_SUBS(1:INTBUF_TAB(NIN)%S_INFLG_SUBS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%INFLG_SUBM,INTBUF_TAB(NIN)%S_INFLG_SUBM)
          INTBUF_TAB(NIN)%INFLG_SUBM(1:INTBUF_TAB(NIN)%S_INFLG_SUBM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADDSUBE,INTBUF_TAB(NIN)%S_ADDSUBE) 
          INTBUF_TAB(NIN)%ADDSUBE(1:INTBUF_TAB(NIN)%S_ADDSUBE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%LISUBE,INTBUF_TAB(NIN)%S_LISUBE) 
          INTBUF_TAB(NIN)%LISUBE(1:INTBUF_TAB(NIN)%S_LISUBE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%INFLG_SUBE,INTBUF_TAB(NIN)%S_INFLG_SUBE)
          INTBUF_TAB(NIN)%INFLG_SUBE(1:INTBUF_TAB(NIN)%S_INFLG_SUBE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSEGTYP,INTBUF_TAB(NIN)%S_MSEGTYP)!KD(48)
          INTBUF_TAB(NIN)%MSEGTYP(1:INTBUF_TAB(NIN)%S_MSEGTYP) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_E,INTBUF_TAB(NIN)%S_CAND_E) !KD(14)
          INTBUF_TAB(NIN)%CAND_E(1:INTBUF_TAB(NIN)%S_CAND_E) = 0 !type16
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_N,INTBUF_TAB(NIN)%S_CAND_N) !KD(15)
          INTBUF_TAB(NIN)%CAND_N(1:INTBUF_TAB(NIN)%S_CAND_N) = 0 !type16
          MY_ALLOCATE(INTBUF_TAB(NIN)%I_STOK,INTBUF_TAB(NIN)%S_I_STOK) !KD(1)
          IF(INTBUF_TAB(NIN)%S_I_STOK>0)INTBUF_TAB(NIN)%I_STOK(1:INTBUF_TAB(NIN)%S_I_STOK) = 0 !inter NE 7
          MY_ALLOCATE(INTBUF_TAB(NIN)%I_STOK_E,INTBUF_TAB(NIN)%S_I_STOK_E) !KD(2)
          IF(INTBUF_TAB(NIN)%S_I_STOK_E>0)INTBUF_TAB(NIN)%I_STOK_E(1) = 0 !inter NE 7
          MY_ALLOCATE(INTBUF_TAB(NIN)%IFPEN,INTBUF_TAB(NIN)%S_IFPEN) !KD(27)
          INTBUF_TAB(NIN)%IFPEN(1:INTBUF_TAB(NIN)%S_IFPEN) = 0 !int7 itrc
          MY_ALLOCATE(INTBUF_TAB(NIN)%IFPEN_E,INTBUF_TAB(NIN)%S_IFPEN_E) 
          INTBUF_TAB(NIN)%IFPEN_E(1:INTBUF_TAB(NIN)%S_IFPEN_E) = 0 !int25 E2E
          MY_ALLOCATE(INTBUF_TAB(NIN)%IFPEN_E2S,INTBUF_TAB(NIN)%S_IFPEN_E2S) 
          INTBUF_TAB(NIN)%IFPEN_E2S(1:INTBUF_TAB(NIN)%S_IFPEN_E2S) = 0 !int25 E2E
          MY_ALLOCATE(INTBUF_TAB(NIN)%KREMNODE,INTBUF_TAB(NIN)%S_KREMNODE) !KD(49)
          INTBUF_TAB(NIN)%KREMNODE(1:INTBUF_TAB(NIN)%S_KREMNODE) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%REMNODE,INTBUF_TAB(NIN)%S_REMNODE) !KD(50)
          INTBUF_TAB(NIN)%REMNODE(1:INTBUF_TAB(NIN)%S_REMNODE) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%KREMNOR,INTBUF_TAB(NIN)%S_KREMNOR)
          INTBUF_TAB(NIN)%KREMNOR(1:INTBUF_TAB(NIN)%S_KREMNOR) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%REMNOR,INTBUF_TAB(NIN)%S_REMNOR) 
          INTBUF_TAB(NIN)%REMNOR(1:INTBUF_TAB(NIN)%S_REMNOR) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%KREMNODE_EDG,INTBUF_TAB(NIN)%S_KREMNODE_EDG) 
          INTBUF_TAB(NIN)%KREMNODE_EDG(1:INTBUF_TAB(NIN)%S_KREMNODE_EDG) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%REMNODE_EDG,INTBUF_TAB(NIN)%S_REMNODE_EDG) 
          INTBUF_TAB(NIN)%REMNODE_EDG(1:INTBUF_TAB(NIN)%S_REMNODE_EDG) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%KREMNODE_E2S,INTBUF_TAB(NIN)%S_KREMNODE_E2S) 
          INTBUF_TAB(NIN)%KREMNODE_E2S(1:INTBUF_TAB(NIN)%S_KREMNODE_E2S) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%REMNODE_E2S,INTBUF_TAB(NIN)%S_REMNODE_E2S) 
          INTBUF_TAB(NIN)%REMNODE_E2S(1:INTBUF_TAB(NIN)%S_REMNODE_E2S) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADCCM,INTBUF_TAB(NIN)%S_ADCCM) !KD(16)
          INTBUF_TAB(NIN)%ADCCM(1:INTBUF_TAB(NIN)%S_ADCCM) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%CHAIN,INTBUF_TAB(NIN)%S_CHAIN) !KD(17)
          INTBUF_TAB(NIN)%CHAIN(1:INTBUF_TAB(NIN)%S_CHAIN) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%NIGE,INTBUF_TAB(NIN)%S_NIGE)
          INTBUF_TAB(NIN)%NIGE(1:INTBUF_TAB(NIN)%S_NIGE) = 0 !
!type20
          MY_ALLOCATE(INTBUF_TAB(NIN)%DAANC6,INTBUF_TAB(NIN)%S_DAANC6) !KD(16)
          INTBUF_TAB(NIN)%DAANC6(1:INTBUF_TAB(NIN)%S_DAANC6) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NBINFLG,INTBUF_TAB(NIN)%S_NBINFLG) !KD(33)
          INTBUF_TAB(NIN)%NBINFLG(1:INTBUF_TAB(NIN)%S_NBINFLG) = 0 !
          MY_ALLOCATE(INTBUF_TAB(NIN)%MBINFLG,INTBUF_TAB(NIN)%S_MBINFLG) !KD(34)
          INTBUF_TAB(NIN)%MBINFLG(1:INTBUF_TAB(NIN)%S_MBINFLG) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%EBINFLG,INTBUF_TAB(NIN)%S_EBINFLG) !KD(34)
          INTBUF_TAB(NIN)%EBINFLG(1:INTBUF_TAB(NIN)%S_EBINFLG) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NLG,INTBUF_TAB(NIN)%S_NLG) !KD(35)
          INTBUF_TAB(NIN)%NLG(1:INTBUF_TAB(NIN)%S_NLG) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISLINS,INTBUF_TAB(NIN)%S_ISLINS) !KD(36)
          INTBUF_TAB(NIN)%ISLINS(1:INTBUF_TAB(NIN)%S_ISLINS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISLINM,INTBUF_TAB(NIN)%S_ISLINM) !KD(37)
          INTBUF_TAB(NIN)%ISLINM(1:INTBUF_TAB(NIN)%S_ISLINM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IXLINS,INTBUF_TAB(NIN)%S_IXLINS) !KD(40)
          INTBUF_TAB(NIN)%IXLINS(1:INTBUF_TAB(NIN)%S_IXLINS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IXLINM,INTBUF_TAB(NIN)%S_IXLINM) !KD(41)
          INTBUF_TAB(NIN)%IXLINM(1:INTBUF_TAB(NIN)%S_IXLINM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NSVL,INTBUF_TAB(NIN)%S_NSVL) !KD(42)
          INTBUF_TAB(NIN)%NSVL(1:INTBUF_TAB(NIN)%S_NSVL) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSRL,INTBUF_TAB(NIN)%S_MSRL) !KD(43)
          INTBUF_TAB(NIN)%MSRL(1:INTBUF_TAB(NIN)%S_MSRL) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%LCAND_N,INTBUF_TAB(NIN)%S_LCAND_N) !KD(44)
          INTBUF_TAB(NIN)%LCAND_N(1:INTBUF_TAB(NIN)%S_LCAND_N) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%LCAND_S,INTBUF_TAB(NIN)%S_LCAND_S) !KD(45)
          INTBUF_TAB(NIN)%LCAND_S(1:INTBUF_TAB(NIN)%S_LCAND_S) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADCCM20,INTBUF_TAB(NIN)%S_ADCCM20) !KD(46)
          INTBUF_TAB(NIN)%ADCCM20(1:INTBUF_TAB(NIN)%S_ADCCM20) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CHAIN20,INTBUF_TAB(NIN)%S_CHAIN20) !KD(47)
          INTBUF_TAB(NIN)%CHAIN20(1:INTBUF_TAB(NIN)%S_CHAIN20) = 0 
!type1
          MY_ALLOCATE(INTBUF_TAB(NIN)%ILOCS,INTBUF_TAB(NIN)%S_ILOCS) !KD(16)
          INTBUF_TAB(NIN)%ILOCS(1:INTBUF_TAB(NIN)%S_ILOCS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NSEGM,INTBUF_TAB(NIN)%S_NSEGM) !KD(21)
          INTBUF_TAB(NIN)%NSEGM(1:INTBUF_TAB(NIN)%S_NSEGM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NRT,INTBUF_TAB(NIN)%S_NRT) !KD(23)
          INTBUF_TAB(NIN)%NRT(1:INTBUF_TAB(NIN)%S_NRT) = 0
!type2
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSEGTYP2,INTBUF_TAB(NIN)%S_MSEGTYP2)
          INTBUF_TAB(NIN)%MSEGTYP2(1:INTBUF_TAB(NIN)%S_MSEGTYP2) = 0
!type3
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRTLS,INTBUF_TAB(NIN)%S_IRTLS) !KD(15)
          INTBUF_TAB(NIN)%IRTLS(1:INTBUF_TAB(NIN)%S_IRTLS) = 0 !invoi3
          MY_ALLOCATE(INTBUF_TAB(NIN)%ILOCM,INTBUF_TAB(NIN)%S_ILOCM) !KD(17)
          INTBUF_TAB(NIN)%ILOCM(1:INTBUF_TAB(NIN)%S_ILOCM) = 0 !invoi3
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRTLOM,INTBUF_TAB(NIN)%S_IRTLOM) !KD(18)
          INTBUF_TAB(NIN)%IRTLOM(1:INTBUF_TAB(NIN)%S_IRTLOM) = 0 !invoi3
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRTLOS,INTBUF_TAB(NIN)%S_IRTLOS) !KD(19)
          INTBUF_TAB(NIN)%IRTLOS(1:INTBUF_TAB(NIN)%S_IRTLOS) = 0 !invoi3
          MY_ALLOCATE(INTBUF_TAB(NIN)%NSEGS,INTBUF_TAB(NIN)%S_NSEGS) !KD(20)
          INTBUF_TAB(NIN)%NSEGS(1:INTBUF_TAB(NIN)%S_NSEGS) = 0 !invoi3
          MY_ALLOCATE(INTBUF_TAB(NIN)%LNSV,INTBUF_TAB(NIN)%S_LNSV) !KD(22)
          INTBUF_TAB(NIN)%LNSV(1:INTBUF_TAB(NIN)%S_LNSV) = 0 !i3sti3
          MY_ALLOCATE(INTBUF_TAB(NIN)%LMSR,INTBUF_TAB(NIN)%S_LMSR) !KD(23)
          INTBUF_TAB(NIN)%LMSR(1:INTBUF_TAB(NIN)%S_LMSR) = 0 !i3sti3
!type4
          MY_ALLOCATE(INTBUF_TAB(NIN)%IELEM,INTBUF_TAB(NIN)%S_IELEM) !KD(25)
          INTBUF_TAB(NIN)%IELEM(1:INTBUF_TAB(NIN)%S_IELEM) = 0 
!type12
          MY_ALLOCATE(INTBUF_TAB(NIN)%FCOUNT,INTBUF_TAB(NIN)%S_FCOUNT) !KD(26)
          INTBUF_TAB(NIN)%FCOUNT(1:INTBUF_TAB(NIN)%S_FCOUNT) = 0 
!type14
          MY_ALLOCATE(INTBUF_TAB(NIN)%KSURF,INTBUF_TAB(NIN)%S_KSURF) !KD(11)
          INTBUF_TAB(NIN)%KSURF(1:INTBUF_TAB(NIN)%S_KSURF) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IMPACT,INTBUF_TAB(NIN)%S_IMPACT) !KD(13)
          INTBUF_TAB(NIN)%IMPACT(1:INTBUF_TAB(NIN)%S_IMPACT) = 0
!type21
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSR21,INTBUF_TAB(NIN)%S_MSR21) !KD(28)
          INTBUF_TAB(NIN)%MSR21(1:INTBUF_TAB(NIN)%S_MSR21) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%MNDD,INTBUF_TAB(NIN)%S_MNDD) !KD(28)
          INTBUF_TAB(NIN)%MNDD(1:INTBUF_TAB(NIN)%S_MNDD) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSR_L,INTBUF_TAB(NIN)%S_MSR_L) !KD(29)
          INTBUF_TAB(NIN)%MSR_L(1:INTBUF_TAB(NIN)%S_MSR_L) = 0 
!type24
          MY_ALLOCATE(INTBUF_TAB(NIN)%MVOISIN,INTBUF_TAB(NIN)%S_MVOISIN) !KD(17)
          INTBUF_TAB(NIN)%MVOISIN(1:INTBUF_TAB(NIN)%S_MVOISIN) = 0 

          MY_ALLOCATE(INTBUF_TAB(NIN)%NVOISIN,INTBUF_TAB(NIN)%S_NVOISIN) !KD(18)
          INTBUF_TAB(NIN)%NVOISIN(1:INTBUF_TAB(NIN)%S_NVOISIN) = 0 

          MY_ALLOCATE(INTBUF_TAB(NIN)%MSEGLO,INTBUF_TAB(NIN)%S_MSEGLO) !KD(19)
          INTBUF_TAB(NIN)%MSEGLO(1:INTBUF_TAB(NIN)%S_MSEGLO) = 0 

          MY_ALLOCATE(INTBUF_TAB(NIN)%MSEGTYP24,INTBUF_TAB(NIN)%S_MSEGTYP24) !KD(20)
          INTBUF_TAB(NIN)%MSEGTYP24(1:INTBUF_TAB(NIN)%S_MSEGTYP24) = 0
 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IRTSE,INTBUF_TAB(NIN)%S_IRTSE) 
          INTBUF_TAB(NIN)%IRTSE(1:INTBUF_TAB(NIN)%S_IRTSE) = 0 

          MY_ALLOCATE(INTBUF_TAB(NIN)%IS2SE,INTBUF_TAB(NIN)%S_IS2SE) 
          INTBUF_TAB(NIN)%IS2SE(1:INTBUF_TAB(NIN)%S_IS2SE) = 0 

          MY_ALLOCATE(INTBUF_TAB(NIN)%IS2PT,INTBUF_TAB(NIN)%S_IS2PT)
          INTBUF_TAB(NIN)%IS2PT(1:INTBUF_TAB(NIN)%S_IS2PT) = 0

          MY_ALLOCATE(INTBUF_TAB(NIN)%ISPT2,INTBUF_TAB(NIN)%S_ISPT2) 
          INTBUF_TAB(NIN)%ISPT2(1:INTBUF_TAB(NIN)%S_ISPT2) = 0

          MY_ALLOCATE(INTBUF_TAB(NIN)%ISEGPT,INTBUF_TAB(NIN)%S_ISEGPT) 
          INTBUF_TAB(NIN)%ISEGPT(1:INTBUF_TAB(NIN)%S_ISEGPT) = 0

          MY_ALLOCATE(INTBUF_TAB(NIN)%IS2ID,INTBUF_TAB(NIN)%S_IS2ID)
          ! IS2ID global ID of Fictive nodes
          INTBUF_TAB(NIN)%IS2ID(1:INTBUF_TAB(NIN)%S_IS2ID) = 0
C---------to be cleaned later          
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISEADD,INTBUF_TAB(NIN)%S_ISEADD) !KD(35)
          INTBUF_TAB(NIN)%ISEADD(1:INTBUF_TAB(NIN)%S_ISEADD) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISEDGE,INTBUF_TAB(NIN)%S_ISEDGE) !KD(36)
          INTBUF_TAB(NIN)%ISEDGE(1:INTBUF_TAB(NIN)%S_ISEDGE) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_T,INTBUF_TAB(NIN)%S_CAND_T) !KD(37)
          INTBUF_TAB(NIN)%CAND_T(1:INTBUF_TAB(NIN)%S_CAND_T) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISEG_PXFEM,INTBUF_TAB(NIN)%S_ISEG_PXFEM) !KD(40)
          INTBUF_TAB(NIN)%ISEG_PXFEM(1:INTBUF_TAB(NIN)%S_ISEG_PXFEM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISEG_PLY,INTBUF_TAB(NIN)%S_ISEG_PLY) !KD(41)
          INTBUF_TAB(NIN)%ISEG_PLY(1:INTBUF_TAB(NIN)%S_ISEG_PLY) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ICONT_I,INTBUF_TAB(NIN)%S_ICONT_I) !KD(41)
          INTBUF_TAB(NIN)%ICONT_I(1:INTBUF_TAB(NIN)%S_ICONT_I) = 0 
!type25
          MY_ALLOCATE(INTBUF_TAB(NIN)%EVOISIN,INTBUF_TAB(NIN)%S_EVOISIN) 
          INTBUF_TAB(NIN)%EVOISIN(1:INTBUF_TAB(NIN)%S_EVOISIN) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADMSR,INTBUF_TAB(NIN)%S_ADMSR)
          INTBUF_TAB(NIN)%ADMSR(1:INTBUF_TAB(NIN)%S_ADMSR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%LEDGE,INTBUF_TAB(NIN)%S_LEDGE)
          INTBUF_TAB(NIN)%LEDGE(1:INTBUF_TAB(NIN)%S_LEDGE) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%LBOUND,INTBUF_TAB(NIN)%S_LBOUND)
          INTBUF_TAB(NIN)%LBOUND(1:INTBUF_TAB(NIN)%S_LBOUND) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ACTNOR,INTBUF_TAB(NIN)%S_ACTNOR)
          INTBUF_TAB(NIN)%ACTNOR(1:INTBUF_TAB(NIN)%S_ACTNOR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%FARM,INTBUF_TAB(NIN)%S_FARM) !KD(18)
          INTBUF_TAB(NIN)%FARM(1:INTBUF_TAB(NIN)%S_FARM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADSKYN,INTBUF_TAB(NIN)%S_ADSKYN)
          INTBUF_TAB(NIN)%ADSKYN(1:INTBUF_TAB(NIN)%S_ADSKYN) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IADNOR,INTBUF_TAB(NIN)%S_IADNOR)
          INTBUF_TAB(NIN)%IADNOR(1:INTBUF_TAB(NIN)%S_IADNOR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ISLIDE,INTBUF_TAB(NIN)%S_ISLIDE)
          INTBUF_TAB(NIN)%ISLIDE(1:INTBUF_TAB(NIN)%S_ISLIDE) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%KNOR2MSR,INTBUF_TAB(NIN)%S_KNOR2MSR)
          INTBUF_TAB(NIN)%KNOR2MSR(1:INTBUF_TAB(NIN)%S_KNOR2MSR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NOR2MSR,INTBUF_TAB(NIN)%S_NOR2MSR)
          INTBUF_TAB(NIN)%NOR2MSR(1:INTBUF_TAB(NIN)%S_NOR2MSR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_OPT_N,INTBUF_TAB(NIN)%S_CAND_OPT_N)
          INTBUF_TAB(NIN)%CAND_OPT_N(1:INTBUF_TAB(NIN)%S_CAND_OPT_N) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_OPT_E,INTBUF_TAB(NIN)%S_CAND_OPT_E)
          INTBUF_TAB(NIN)%CAND_OPT_E(1:INTBUF_TAB(NIN)%S_CAND_OPT_E) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%IF_ADH,INTBUF_TAB(NIN)%S_IF_ADH)
          INTBUF_TAB(NIN)%IF_ADH(1:INTBUF_TAB(NIN)%S_IF_ADH) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CANDM_E2E,INTBUF_TAB(NIN)%S_CANDM_E2E) !KD(44)
          INTBUF_TAB(NIN)%CANDM_E2E(1:INTBUF_TAB(NIN)%S_CANDM_E2E) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CANDS_E2E,INTBUF_TAB(NIN)%S_CANDS_E2E) !KD(44)
          INTBUF_TAB(NIN)%CANDS_E2E(1:INTBUF_TAB(NIN)%S_CANDS_E2E) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CANDM_E2S,INTBUF_TAB(NIN)%S_CANDM_E2S) !KD(44)
          INTBUF_TAB(NIN)%CANDM_E2S(1:INTBUF_TAB(NIN)%S_CANDM_E2S) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%CANDS_E2S,INTBUF_TAB(NIN)%S_CANDS_E2S) !KD(44)
          INTBUF_TAB(NIN)%CANDS_E2S(1:INTBUF_TAB(NIN)%S_CANDS_E2S) = 0 
!type7 11 19 24 25 : friction parts
          MY_ALLOCATE(INTBUF_TAB(NIN)%IPARTFRICS,INTBUF_TAB(NIN)%S_IPARTFRICS) 
          INTBUF_TAB(NIN)%IPARTFRICS(1:INTBUF_TAB(NIN)%S_IPARTFRICS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IPARTFRICM,INTBUF_TAB(NIN)%S_IPARTFRICM) 
          INTBUF_TAB(NIN)%IPARTFRICM(1:INTBUF_TAB(NIN)%S_IPARTFRICM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IREP_FRICM,INTBUF_TAB(NIN)%S_IREP_FRICM) 
          INTBUF_TAB(NIN)%IREP_FRICM(1:INTBUF_TAB(NIN)%S_IREP_FRICM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%IPARTFRIC_E,INTBUF_TAB(NIN)%S_IPARTFRIC_E) 
          INTBUF_TAB(NIN)%IPARTFRIC_E(1:INTBUF_TAB(NIN)%S_IPARTFRIC_E) = 0 
!Nitsche Method
          MY_ALLOCATE(INTBUF_TAB(NIN)%IELNRTS,INTBUF_TAB(NIN)%S_IELNRTS) 
          INTBUF_TAB(NIN)%IELNRTS(1:INTBUF_TAB(NIN)%S_IELNRTS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%ADRECTS,INTBUF_TAB(NIN)%S_ADRECTS) 
          INTBUF_TAB(NIN)%ADRECTS(1:INTBUF_TAB(NIN)%S_ADRECTS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%FACNRTS,INTBUF_TAB(NIN)%S_FACNRTS) 
          INTBUF_TAB(NIN)%FACNRTS(1:INTBUF_TAB(NIN)%S_FACNRTS) = 0
C=======================================================================
c ALLOCATE REAL ARRAYS
C=======================================================================
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFAC,INTBUF_TAB(NIN)%S_STFAC) !JD(10)
          MY_ALLOCATE(INTBUF_TAB(NIN)%VARIABLES,INTBUF_TAB(NIN)%S_VARIABLES ) !JD(11)
          MY_ALLOCATE(INTBUF_TAB(NIN)%CSTS,INTBUF_TAB(NIN)%S_CSTS) !JD(12)
          INTBUF_TAB(NIN)%CSTS(1:INTBUF_TAB(NIN)%S_CSTS) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%DPARA,INTBUF_TAB(NIN)%S_DPARA) !JD(21)
          INTBUF_TAB(NIN)%DPARA(1:INTBUF_TAB(NIN)%S_DPARA) = 0 !inint3 NTY==2.AND.ISEARCH==1
          MY_ALLOCATE(INTBUF_TAB(NIN)%NMAS,INTBUF_TAB(NIN)%S_NMAS) !JD(22)
          INTBUF_TAB(NIN)%NMAS(1:INTBUF_TAB(NIN)%S_NMAS) = 0 !ILEV=0
          MY_ALLOCATE(INTBUF_TAB(NIN)%SMAS,INTBUF_TAB(NIN)%S_SMAS) !JD(23)
          INTBUF_TAB(NIN)%SMAS(1:INTBUF_TAB(NIN)%S_SMAS) = 0 !ILEV=0
          MY_ALLOCATE(INTBUF_TAB(NIN)%SINER,INTBUF_TAB(NIN)%S_SINER) !JD(24)
          INTBUF_TAB(NIN)%SINER(1:INTBUF_TAB(NIN)%S_SINER) = 0 !ILEV=0
          MY_ALLOCATE(INTBUF_TAB(NIN)%AREAS2,INTBUF_TAB(NIN)%S_AREAS2) !JD(31)
          INTBUF_TAB(NIN)%AREAS2(1:INTBUF_TAB(NIN)%S_AREAS2) = 0 !ILEV=0
          MY_ALLOCATE(INTBUF_TAB(NIN)%UVAR,INTBUF_TAB(NIN)%S_UVAR) !JD(32)
          INTBUF_TAB(NIN)%UVAR(1:INTBUF_TAB(NIN)%S_UVAR) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%XM0,INTBUF_TAB(NIN)%S_XM0) !JD(33)
          INTBUF_TAB(NIN)%XM0(1:INTBUF_TAB(NIN)%S_XM0) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%SPENALTY,INTBUF_TAB(NIN)%S_SPENALTY) !JD(25)
          INTBUF_TAB(NIN)%SPENALTY(1:INTBUF_TAB(NIN)%S_SPENALTY) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFR_PENALTY,INTBUF_TAB(NIN)%S_STFR_PENALTY) !JD(26)
          INTBUF_TAB(NIN)%STFR_PENALTY(1:INTBUF_TAB(NIN)%S_STFR_PENALTY) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%SKEW,INTBUF_TAB(NIN)%S_SKEW) !JD(33)+ILEV25
          INTBUF_TAB(NIN)%SKEW(1:INTBUF_TAB(NIN)%S_SKEW) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%DSM,INTBUF_TAB(NIN)%S_DSM) !JD(34)
          INTBUF_TAB(NIN)%DSM(1:INTBUF_TAB(NIN)%S_DSM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%FSM,INTBUF_TAB(NIN)%S_FSM) !JD(35)
          INTBUF_TAB(NIN)%FSM(1:INTBUF_TAB(NIN)%S_FSM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%RUPT,INTBUF_TAB(NIN)%S_RUPT) !JD(36)
          INTBUF_TAB(NIN)%RUPT(1:INTBUF_TAB(NIN)%S_RUPT) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FINI,INTBUF_TAB(NIN)%S_FINI) !JD(36)+ILEV25  
          INTBUF_TAB(NIN)%FINI(1:INTBUF_TAB(NIN)%S_FINI) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFNS,INTBUF_TAB(NIN)%S_STFNS ) !JD(14)
          INTBUF_TAB(NIN)%STFNS(1:INTBUF_TAB(NIN)%S_STFNS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFM,INTBUF_TAB(NIN)%S_STFM ) !JD(17)
          INTBUF_TAB(NIN)%STFM(1:INTBUF_TAB(NIN)%S_STFM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFS,INTBUF_TAB(NIN)%S_STFS ) !JD(16)
          INTBUF_TAB(NIN)%STFS(1:INTBUF_TAB(NIN)%S_STFS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENIM,INTBUF_TAB(NIN)%S_PENIM ) !JD(21)
          INTBUF_TAB(NIN)%PENIM(1:INTBUF_TAB(NIN)%S_PENIM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENIS,INTBUF_TAB(NIN)%S_PENIS ) !JD(20)
          INTBUF_TAB(NIN)%PENIS(1:INTBUF_TAB(NIN)%S_PENIS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_M,INTBUF_TAB(NIN)%S_GAP_M ) !JD(15)
          INTBUF_TAB(NIN)%GAP_M(1:INTBUF_TAB(NIN)%S_GAP_M) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_S,INTBUF_TAB(NIN)%S_GAP_S ) !JD(16)
          INTBUF_TAB(NIN)%GAP_S(1:INTBUF_TAB(NIN)%S_GAP_S) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%XSAV,INTBUF_TAB(NIN)%S_XSAV) !JD(19)
          INTBUF_TAB(NIN)%XSAV(1:INTBUF_TAB(NIN)%S_XSAV) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%CRIT,INTBUF_TAB(NIN)%S_CRIT ) !JD(24)
          INTBUF_TAB(NIN)%CRIT(1:INTBUF_TAB(NIN)%S_CRIT) = 0 !ILEV=0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FRIC_P,INTBUF_TAB(NIN)%S_FRIC_P) !JD(25)
          INTBUF_TAB(NIN)%FRIC_P(1:INTBUF_TAB(NIN)%S_FRIC_P) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%XFILTR,INTBUF_TAB(NIN)%S_XFILTR ) !JD(26)
          INTBUF_TAB(NIN)%XFILTR(1:INTBUF_TAB(NIN)%S_XFILTR) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%AREAS,INTBUF_TAB(NIN)%S_AREAS) !JD(30)
          INTBUF_TAB(NIN)%AREAS(1:INTBUF_TAB(NIN)%S_AREAS) = 0 !ILEV=0
          MY_ALLOCATE(INTBUF_TAB(NIN)%AREAM,INTBUF_TAB(NIN)%S_AREAM) 
          INTBUF_TAB(NIN)%AREAM(1:INTBUF_TAB(NIN)%S_AREAM) = 0 
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_SL,INTBUF_TAB(NIN)%S_GAP_SL ) !JD(31)
          INTBUF_TAB(NIN)%GAP_SL(1:INTBUF_TAB(NIN)%S_GAP_SL) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_ML,INTBUF_TAB(NIN)%S_GAP_ML ) !JD(32)
          INTBUF_TAB(NIN)%GAP_ML(1:INTBUF_TAB(NIN)%S_GAP_ML) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_P,INTBUF_TAB(NIN)%S_CAND_P ) !JD(18)
          INTBUF_TAB(NIN)%CAND_P(1:INTBUF_TAB(NIN)%S_CAND_P) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVX,INTBUF_TAB(NIN)%S_FTSAVX ) !JD(27)
          INTBUF_TAB(NIN)%FTSAVX(1:INTBUF_TAB(NIN)%S_FTSAVX) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVY,INTBUF_TAB(NIN)%S_FTSAVY ) !JD(28)
          INTBUF_TAB(NIN)%FTSAVY(1:INTBUF_TAB(NIN)%S_FTSAVY) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVZ,INTBUF_TAB(NIN)%S_FTSAVZ ) !JD(29)
          INTBUF_TAB(NIN)%FTSAVZ(1:INTBUF_TAB(NIN)%S_FTSAVZ) = 0
C
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVX_E,INTBUF_TAB(NIN)%S_FTSAVX_E ) 
          INTBUF_TAB(NIN)%FTSAVX_E(1:INTBUF_TAB(NIN)%S_FTSAVX_E) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVY_E,INTBUF_TAB(NIN)%S_FTSAVY_E ) 
          INTBUF_TAB(NIN)%FTSAVY_E(1:INTBUF_TAB(NIN)%S_FTSAVY_E) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVZ_E,INTBUF_TAB(NIN)%S_FTSAVZ _E) 
          INTBUF_TAB(NIN)%FTSAVZ_E(1:INTBUF_TAB(NIN)%S_FTSAVZ_E) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVX_E2S,INTBUF_TAB(NIN)%S_FTSAVX_E2S ) 
          INTBUF_TAB(NIN)%FTSAVX_E2S(1:INTBUF_TAB(NIN)%S_FTSAVX_E2S) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVY_E2S,INTBUF_TAB(NIN)%S_FTSAVY_E2S ) 
          INTBUF_TAB(NIN)%FTSAVY_E2S(1:INTBUF_TAB(NIN)%S_FTSAVY_E2S) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAVZ_E2S,INTBUF_TAB(NIN)%S_FTSAVZ _E2S) 
          INTBUF_TAB(NIN)%FTSAVZ_E2S(1:INTBUF_TAB(NIN)%S_FTSAVZ_E2S) = 0
C
          MY_ALLOCATE(INTBUF_TAB(NIN)%RIGE,INTBUF_TAB(NIN)%S_RIGE )
          INTBUF_TAB(NIN)%RIGE(1:INTBUF_TAB(NIN)%S_RIGE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%XIGE,INTBUF_TAB(NIN)%S_XIGE )
          INTBUF_TAB(NIN)%XIGE(1:INTBUF_TAB(NIN)%S_XIGE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%VIGE,INTBUF_TAB(NIN)%S_VIGE )
          INTBUF_TAB(NIN)%VIGE(1:INTBUF_TAB(NIN)%S_VIGE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%MASSIGE,INTBUF_TAB(NIN)%S_MASSIGE )
          INTBUF_TAB(NIN)%MASSIGE(1:INTBUF_TAB(NIN)%S_MASSIGE) = 0
!type10
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_F,INTBUF_TAB(NIN)%S_CAND_F ) !JD(18)
          INTBUF_TAB(NIN)%CAND_F(1:INTBUF_TAB(NIN)%S_CAND_F) = 0
!type20
          MY_ALLOCATE(INTBUF_TAB(NIN)%XA,INTBUF_TAB(NIN)%S_XA ) !JD(12)
          INTBUF_TAB(NIN)%XA(1:INTBUF_TAB(NIN)%S_XA) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%VA,INTBUF_TAB(NIN)%S_VA ) !JD(13)
          INTBUF_TAB(NIN)%VA(1:INTBUF_TAB(NIN)%S_VA) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFA,INTBUF_TAB(NIN)%S_STFA ) !JD(14)
          MY_ALLOCATE(INTBUF_TAB(NIN)%AVX_ANCR,INTBUF_TAB(NIN)%S_AVX_ANCR ) !JD(22)
          INTBUF_TAB(NIN)%AVX_ANCR(1:INTBUF_TAB(NIN)%S_AVX_ANCR) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_SH,INTBUF_TAB(NIN)%S_GAP_SH ) !JD(23)
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_FX,INTBUF_TAB(NIN)%S_CAND_FX ) !JD(27)
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_FY,INTBUF_TAB(NIN)%S_CAND_FY ) !JD(28)
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_FZ,INTBUF_TAB(NIN)%S_CAND_FZ ) !JD(29)
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_SE,INTBUF_TAB(NIN)%S_GAP_SE ) !JD(37)
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_ME,INTBUF_TAB(NIN)%S_GAP_ME ) !JD(37)
          MY_ALLOCATE(INTBUF_TAB(NIN)%STF,INTBUF_TAB(NIN)%S_STF ) !JD(37)
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFNE,INTBUF_TAB(NIN)%S_STFNE ) !JD(38)
          MY_ALLOCATE(INTBUF_TAB(NIN)%CRITX,INTBUF_TAB(NIN)%S_CRITX ) !JD(39)
          INTBUF_TAB(NIN)%CRITX(1:INTBUF_TAB(NIN)%S_CRITX) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENISE,INTBUF_TAB(NIN)%S_PENISE ) !JD(40)
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENIME,INTBUF_TAB(NIN)%S_PENIME ) !JD(41)
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENIA,INTBUF_TAB(NIN)%S_PENIA ) !JD(42)
          MY_ALLOCATE(INTBUF_TAB(NIN)%ALPHAK,INTBUF_TAB(NIN)%S_ALPHAK ) !JD(43)
!type1
          MY_ALLOCATE(INTBUF_TAB(NIN)%N,INTBUF_TAB(NIN)%S_N ) !JD(20)
          INTBUF_TAB(NIN)%N(1:INTBUF_TAB(NIN)%S_N) = 0

!type3,4,5,9
          MY_ALLOCATE(INTBUF_TAB(NIN)%CSTM,INTBUF_TAB(NIN)%S_CSTM ) !JD(13)
          INTBUF_TAB(NIN)%CSTM(1:INTBUF_TAB(NIN)%S_CSTM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%EE,INTBUF_TAB(NIN)%S_EE ) !JD(14)
          INTBUF_TAB(NIN)%EE(1:INTBUF_TAB(NIN)%S_EE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFNM,INTBUF_TAB(NIN)%S_STFNM ) !JD(15)
          INTBUF_TAB(NIN)%STFNM(1:INTBUF_TAB(NIN)%S_STFNM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FRICOS,INTBUF_TAB(NIN)%S_FRICOS ) !JD(18)
          INTBUF_TAB(NIN)%FRICOS(1:INTBUF_TAB(NIN)%S_FRICOS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FRICOM,INTBUF_TAB(NIN)%S_FRICOM ) !JD(19)
          INTBUF_TAB(NIN)%FRICOM(1:INTBUF_TAB(NIN)%S_FRICOM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FTSAV,INTBUF_TAB(NIN)%S_FTSAV ) !JD(27)
          INTBUF_TAB(NIN)%FTSAV(1:INTBUF_TAB(NIN)%S_FTSAV) = 0
!type6
          MY_ALLOCATE(INTBUF_TAB(NIN)%FCONT,INTBUF_TAB(NIN)%S_FCONT ) !JD(20)
          INTBUF_TAB(NIN)%FCONT(1:INTBUF_TAB(NIN)%S_FCONT) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FS,INTBUF_TAB(NIN)%S_FS ) !JD(21)
          INTBUF_TAB(NIN)%FS(1:INTBUF_TAB(NIN)%S_FS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FM,INTBUF_TAB(NIN)%S_FM ) !JD(22)
          INTBUF_TAB(NIN)%FM(1:INTBUF_TAB(NIN)%S_FM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%RMAS,INTBUF_TAB(NIN)%S_RMAS ) !JD(23)
          INTBUF_TAB(NIN)%RMAS(1:INTBUF_TAB(NIN)%S_RMAS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%ANSMX0,INTBUF_TAB(NIN)%S_ANSMX0 ) !JD(24)
          INTBUF_TAB(NIN)%ANSMX0(1:INTBUF_TAB(NIN)%S_ANSMX0) = 0
!type8
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAPN,INTBUF_TAB(NIN)%S_GAPN) !JD(31)
          INTBUF_TAB(NIN)%GAPN(1:INTBUF_TAB(NIN)%S_GAPN) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STF8,INTBUF_TAB(NIN)%S_STF8 ) !JD(32)
          INTBUF_TAB(NIN)%STF8(1:INTBUF_TAB(NIN)%S_STF8) = 0
!type14
          MY_ALLOCATE(INTBUF_TAB(NIN)%CIMP,INTBUF_TAB(NIN)%S_CIMP) !JD(12)
          INTBUF_TAB(NIN)%CIMP(1:INTBUF_TAB(NIN)%S_CIMP) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%NIMP,INTBUF_TAB(NIN)%S_NIMP ) !JD(13)
          INTBUF_TAB(NIN)%NIMP(1:INTBUF_TAB(NIN)%S_NIMP) = 0
!type15
          MY_ALLOCATE(INTBUF_TAB(NIN)%IOLD,INTBUF_TAB(NIN)%S_IOLD) !JD(12)
          INTBUF_TAB(NIN)%IOLD(1:INTBUF_TAB(NIN)%S_IOLD) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%HOLD,INTBUF_TAB(NIN)%S_HOLD ) !JD(13)
          INTBUF_TAB(NIN)%HOLD(1:INTBUF_TAB(NIN)%S_HOLD) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%NOLD,INTBUF_TAB(NIN)%S_NOLD) !JD(14)
          INTBUF_TAB(NIN)%NOLD(1:INTBUF_TAB(NIN)%S_NOLD) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%DOLD,INTBUF_TAB(NIN)%S_DOLD ) !JD(15)
          INTBUF_TAB(NIN)%DOLD(1:INTBUF_TAB(NIN)%S_DOLD) = 0
!type17
          MY_ALLOCATE(INTBUF_TAB(NIN)%KS,INTBUF_TAB(NIN)%S_KS) !JD(16)
          INTBUF_TAB(NIN)%KS(1:INTBUF_TAB(NIN)%S_KS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%KM,INTBUF_TAB(NIN)%S_KM ) !JD(17)
          INTBUF_TAB(NIN)%KM(1:INTBUF_TAB(NIN)%S_KM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FROTS,INTBUF_TAB(NIN)%S_FROTS) !JD(18)
          INTBUF_TAB(NIN)%FROTS(1:INTBUF_TAB(NIN)%S_FROTS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FROTM,INTBUF_TAB(NIN)%S_FROTM ) !JD(19)
          INTBUF_TAB(NIN)%FROTM(1:INTBUF_TAB(NIN)%S_FROTM) = 0
!type21 
          MY_ALLOCATE(INTBUF_TAB(NIN)%NOD_NORMAL,INTBUF_TAB(NIN)%S_NOD_NORMAL) !JD(21)
          INTBUF_TAB(NIN)%NOD_NORMAL(1:INTBUF_TAB(NIN)%S_NOD_NORMAL) = 0
!type21
          MY_ALLOCATE(INTBUF_TAB(NIN)%RCURV,INTBUF_TAB(NIN)%S_RCURV) !JD(21)
          INTBUF_TAB(NIN)%RCURV(1:INTBUF_TAB(NIN)%S_RCURV) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%ANGLM,INTBUF_TAB(NIN)%S_ANGLM ) !JD(23)
          INTBUF_TAB(NIN)%ANGLM(1:INTBUF_TAB(NIN)%S_ANGLM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%FROT_P,INTBUF_TAB(NIN)%S_FROT_P) !JD(25)
          INTBUF_TAB(NIN)%FROT_P(1:INTBUF_TAB(NIN)%S_FROT_P) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%ALPHA0,INTBUF_TAB(NIN)%S_ALPHA0 ) !JD(26)
          INTBUF_TAB(NIN)%ALPHA0(1:INTBUF_TAB(NIN)%S_ALPHA0) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%AS,INTBUF_TAB(NIN)%S_AS ) !JD(31)
          INTBUF_TAB(NIN)%AS(1:INTBUF_TAB(NIN)%S_AS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%BS,INTBUF_TAB(NIN)%S_BS ) !JD(32)
          INTBUF_TAB(NIN)%BS(1:INTBUF_TAB(NIN)%S_BS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%THKNOD0,INTBUF_TAB(NIN)%S_THKNOD0 ) !JD(33)
          INTBUF_TAB(NIN)%THKNOD0(1:INTBUF_TAB(NIN)%S_THKNOD0) = 0
!type24
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAPN_M,INTBUF_TAB(NIN)%S_GAPN_M) !JD(22)
          INTBUF_TAB(NIN)%GAPN_M(1:INTBUF_TAB(NIN)%S_GAPN_M) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%SECND_FR,INTBUF_TAB(NIN)%S_SECND_FR) !JD(27)
          INTBUF_TAB(NIN)%SECND_FR(1:INTBUF_TAB(NIN)%S_SECND_FR) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENE_OLD,INTBUF_TAB(NIN)%S_PENE_OLD ) !JD(31)
          INTBUF_TAB(NIN)%PENE_OLD(1:INTBUF_TAB(NIN)%S_PENE_OLD) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STIF_OLD,INTBUF_TAB(NIN)%S_STIF_OLD) !JD(32)
          INTBUF_TAB(NIN)%STIF_OLD(1:INTBUF_TAB(NIN)%S_STIF_OLD) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%TIME_S,INTBUF_TAB(NIN)%S_TIME_S ) !JD(33)
          INTBUF_TAB(NIN)%TIME_S(1:INTBUF_TAB(NIN)%S_TIME_S) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_NM,INTBUF_TAB(NIN)%S_GAP_NM ) !JD(34)
          INTBUF_TAB(NIN)%GAP_NM(1:INTBUF_TAB(NIN)%S_GAP_NM) = 0
C----------to be cleaned          
          MY_ALLOCATE(INTBUF_TAB(NIN)%EDGE8L2,INTBUF_TAB(NIN)%S_EDGE8L2 ) !JD(35)
          INTBUF_TAB(NIN)%EDGE8L2(1:INTBUF_TAB(NIN)%S_EDGE8L2) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%XFIC,INTBUF_TAB(NIN)%S_XFIC ) 
          INTBUF_TAB(NIN)%XFIC(1:INTBUF_TAB(NIN)%S_XFIC) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%VFIC,INTBUF_TAB(NIN)%S_VFIC ) 
          INTBUF_TAB(NIN)%VFIC(1:INTBUF_TAB(NIN)%S_VFIC) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%MSFIC,INTBUF_TAB(NIN)%S_MSFIC) 
          INTBUF_TAB(NIN)%MSFIC(1:INTBUF_TAB(NIN)%S_MSFIC) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%NOD_2RY_LGTH,INTBUF_TAB(NIN)%S_NOD_2RY_LGTH) !JD(42)
          INTBUF_TAB(NIN)%NOD_2RY_LGTH(1:INTBUF_TAB(NIN)%S_NOD_2RY_LGTH) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%NOD_MAS_LGTH,INTBUF_TAB(NIN)%S_NOD_MAS_LGTH ) !JD(43)
          INTBUF_TAB(NIN)%NOD_MAS_LGTH(1:INTBUF_TAB(NIN)%S_NOD_MAS_LGTH) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_N0,INTBUF_TAB(NIN)%S_GAP_N0 ) !JD(36)
          INTBUF_TAB(NIN)%GAP_N0(1:INTBUF_TAB(NIN)%S_GAP_N0) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%DGAP_NM,INTBUF_TAB(NIN)%S_DGAP_NM ) !JD(37)
          INTBUF_TAB(NIN)%DGAP_NM(1:INTBUF_TAB(NIN)%S_DGAP_NM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%DGAP_M,INTBUF_TAB(NIN)%S_DGAP_M ) !JD(38)
          INTBUF_TAB(NIN)%DGAP_M(1:INTBUF_TAB(NIN)%S_DGAP_M) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%DELTA_PMAX_DGAP,INTBUF_TAB(NIN)%S_DELTA_PMAX_DGAP ) !JD(39)
          INTBUF_TAB(NIN)%DELTA_PMAX_DGAP(1:INTBUF_TAB(NIN)%S_DELTA_PMAX_DGAP) = 0
!type25
          MY_ALLOCATE(INTBUF_TAB(NIN)%EDGE_BISECTOR,INTBUF_TAB(NIN)%S_EDGE_BISECTOR)
          INTBUF_TAB(NIN)%EDGE_BISECTOR(1:INTBUF_TAB(NIN)%S_EDGE_BISECTOR) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%E2S_NOD_NORMAL,INTBUF_TAB(NIN)%S_E2S_NOD_NORMAL)
          INTBUF_TAB(NIN)%E2S_NOD_NORMAL(1:INTBUF_TAB(NIN)%S_E2S_NOD_NORMAL )= 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%E2S_ACTNOR,INTBUF_TAB(NIN)%S_E2S_ACTNOR)
          INTBUF_TAB(NIN)%E2S_ACTNOR(1:INTBUF_TAB(NIN)%S_E2S_ACTNOR )= 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%PENM,INTBUF_TAB(NIN)%S_PENM)
          INTBUF_TAB(NIN)%PENM(1:INTBUF_TAB(NIN)%S_PENM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%DISTM,INTBUF_TAB(NIN)%S_DISTM)
          INTBUF_TAB(NIN)%DISTM(1:INTBUF_TAB(NIN)%S_DISTM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%LBM,INTBUF_TAB(NIN)%S_LBM)
          INTBUF_TAB(NIN)%LBM(1:INTBUF_TAB(NIN)%S_LBM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%LCM,INTBUF_TAB(NIN)%S_LCM)
          INTBUF_TAB(NIN)%LCM(1:INTBUF_TAB(NIN)%S_LCM) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%VTX_BISECTOR,INTBUF_TAB(NIN)%S_VTX_BISECTOR)
          INTBUF_TAB(NIN)%VTX_BISECTOR(1:INTBUF_TAB(NIN)%S_VTX_BISECTOR) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%CAND_PS,INTBUF_TAB(NIN)%S_CAND_PS ) !JD(18)
          INTBUF_TAB(NIN)%CAND_PS(1:INTBUF_TAB(NIN)%S_CAND_PS) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAPE,INTBUF_TAB(NIN)%S_GAPE ) !JD(18)
          INTBUF_TAB(NIN)%GAPE(1:INTBUF_TAB(NIN)%S_GAPE) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAP_E_L,INTBUF_TAB(NIN)%S_GAP_E_L ) !JD(18)
          INTBUF_TAB(NIN)%GAP_E_L(1:INTBUF_TAB(NIN)%S_GAP_E_L) = 0
          MY_ALLOCATE(INTBUF_TAB(NIN)%STFE,INTBUF_TAB(NIN)%S_STFE ) !JD(18)
          INTBUF_TAB(NIN)%STFE(1:INTBUF_TAB(NIN)%S_STFE) = 0
!type2
          MY_ALLOCATE(INTBUF_TAB(NIN)%CSTS_BIS,INTBUF_TAB(NIN)%S_CSTS_BIS)
          INTBUF_TAB(NIN)%CSTS_BIS(1:INTBUF_TAB(NIN)%S_CSTS_BIS) = 0
! Orthotropic Friction
          MY_ALLOCATE(INTBUF_TAB(NIN)%DIR_FRICM,INTBUF_TAB(NIN)%S_DIR_FRICM) 
          INTBUF_TAB(NIN)%DIR_FRICM(1:INTBUF_TAB(NIN)%S_DIR_FRICM) = 0 
! type25
          MY_ALLOCATE(INTBUF_TAB(NIN)%GAPMSAV,INTBUF_TAB(NIN)%S_GAPMSAV )
          INTBUF_TAB(NIN)%GAPMSAV(1:INTBUF_TAB(NIN)%S_GAPMSAV) = 0
        ENDIF
C=======================================================================
c       END ALLOCATIONS
C=======================================================================
      ENDDO     ! NIN  = 1,NINTER
C-----
      RETURN

      END SUBROUTINE INTBUF_INI_STARTER
Chd|====================================================================
Chd|  INT8_INI                      source/interfaces/intbuf/intbuf_ini_starter.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT8_INI(INTBUF_TAB, IPARI, NBT8)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD                      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c      my_real
c     .   
      INTEGER IPARI(NPARI,*),NBT8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)


C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,ITYP,NSN,NMN
      INTEGER P,Q,I
C-----------------------------------------------
      NBT8 = 0
      DO NIN = 1,NINTER 
         IF(IPARI(7,NIN) == 8) THEN
           NBT8 = NBT8+1
         ENDIF
      ENDDO
      END SUBROUTINE INT8_INI
