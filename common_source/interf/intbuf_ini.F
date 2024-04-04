Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INTBUF_INI                    interf/intbuf_ini.F           
Chd|-- called by -----------
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        ARRET                         starter/source/system/arret.F 
Chd|        ARRET                         engine/source/system/arret.F  
Chd|        INTBUF_INI_T8                 interf/intbuf_ini.F           
Chd|        READ_DB                       starter/source/restart/ddsplit/wrrest.F
Chd|        READ_DB                       engine/source/output/tools/read_db.F
Chd|        READ_I_C                      tools/input_output/write_routtines.c
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|        MESSAGE_MOD                   starter/share/message_module/message_mod.F
Chd|        MESSAGE_MOD                   engine/share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INTBUF_INI(INTBUF_TAB)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD      
      USE MESSAGE_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                         
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: INTBUF_SIZE
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NI,SIZ,L_INTBUF_SIZE
      INTEGER VAL_TEST
C=======================================================================
      ALLOCATE(INTBUF_SIZE(L_INTBUF_SIZE_MAX))
      INTBUF_SIZE(1:L_INTBUF_SIZE_MAX) = 0

      DO NI= 1, NINTER

        CALL READ_I_C(L_INTBUF_SIZE,1)

        ! read all arrays sizes of structure INTBUF_TAB
        CALL READ_I_C(INTBUF_SIZE,L_INTBUF_SIZE)

        N = 1
C=======================================================================
c READ INTEGER SIZES 
C=======================================================================
        INTBUF_TAB(NI)%S_IRECTS       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IRECTM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NSV          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MSR          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IRTLM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IRUPT        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_INORM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IELEC        = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_IELES        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LISUB        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_TYPSUB        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADDSUBS      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADDSUBM      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LISUBS       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LISUBM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_INFLG_SUBS   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_INFLG_SUBM   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADDSUBE      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LISUBE       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_INFLG_SUBE   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MSEGTYP      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_E       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_N       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_I_STOK       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_I_STOK_E     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IFPEN        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_KREMNODE     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_REMNODE      = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_KREMNOR      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_REMNOR       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADCCM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CHAIN        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NIGE         = INTBUF_SIZE(N)
!type20
        N = N+1
        INTBUF_TAB(NI)%S_DAANC6       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NBINFLG      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MBINFLG      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_EBINFLG      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NLG          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISLINS       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISLINM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IXLINS       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IXLINM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NSVL         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MSRL         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LCAND_N      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LCAND_S      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADCCM20      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CHAIN20      = INTBUF_SIZE(N)
!type1
        N = N+1
        INTBUF_TAB(NI)%S_ILOCS        = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_NSEGM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NRT          = INTBUF_SIZE(N)
        N = N+1
!type2
        INTBUF_TAB(NI)%S_MSEGTYP2     = INTBUF_SIZE(N)
!type3
        N = N+1
        INTBUF_TAB(NI)%S_IRTLS        = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_ILOCM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IRTLOM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IRTLOS       = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_NSEGS        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LNSV         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LMSR         = INTBUF_SIZE(N)
!type4
        N = N+1
        INTBUF_TAB(NI)%S_IELEM        = INTBUF_SIZE(N)
!type12
        N = N+1
        INTBUF_TAB(NI)%S_FCOUNT       = INTBUF_SIZE(N)
!type14
        N = N+1
        INTBUF_TAB(NI)%S_KSURF        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IMPACT       = INTBUF_SIZE(N)
!type21
        N = N+1
        INTBUF_TAB(NI)%S_MSR21       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MNDD        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MSR_L       = INTBUF_SIZE(N)
!type24 
        N = N+1
        INTBUF_TAB(NI)%S_MVOISIN     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NVOISIN     = INTBUF_SIZE(N)
!type24 & 25
        N = N+1
        INTBUF_TAB(NI)%S_MSEGLO      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MSEGTYP24   = INTBUF_SIZE(N)
!type24 
C---------will be cleaned after        
        N = N+1
        INTBUF_TAB(NI)%S_ISEADD      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISEDGE      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_T      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISEG_PXFEM  = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISEG_PLY    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ICONT_I     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IRTSE       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IS2SE      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IS2PT      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISPT2      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISEGPT      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IS2ID      = INTBUF_SIZE(N)
!type25
        N = N+1
        INTBUF_TAB(NI)%S_EVOISIN     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADMSR      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LEDGE      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LBOUND     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ACTNOR     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FARM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADSKYN     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IADNOR     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ISLIDE     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_KNOR2MSR   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NOR2MSR    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_OPT_N = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_OPT_E = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IF_ADH     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CANDM_E2E    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CANDS_E2E    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CANDM_E2S    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CANDS_E2S    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IFPEN_E      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IFPEN_E2S    = INTBUF_SIZE(N)
!friction
        N = N+1
        INTBUF_TAB(NI)%S_IPARTFRICS = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IPARTFRICM = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IPARTFRIC_E = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IELNRTS = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ADRECTS = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FACNRTS = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_IREP_FRICM = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_E2S_ACTNOR = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_KREMNODE_EDG = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_REMNODE_EDG  = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_KREMNODE_E2S = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_REMNODE_E2S  = INTBUF_SIZE(N)

C=======================================================================
c READ REAL SIZES
C=======================================================================
        N = N+1
        INTBUF_TAB(NI)%S_STFAC        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_VARIABLES    = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CSTS         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DPARA        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NMAS         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_AREAS2       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_SMAS         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_SINER        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_UVAR         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_XM0          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_SPENALTY     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFR_PENALTY = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_SKEW         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DSM          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FSM          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_RUPT         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FINI         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFNS        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFM         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFS         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_PENIM        = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_PENIS        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STIFMSDT_S   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STIFMSDT_M   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_M        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_S        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_XSAV         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CRIT         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FRIC_P       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_XFILTR       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_AREAS        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_AREAM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_ML       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_SL       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_P       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_PS      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAPE        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_E_L     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFE        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STIFMSDT_EDG = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVX       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVY       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVZ       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_RIGE         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_XIGE         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_VIGE         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MASSIGE      = INTBUF_SIZE(N)
!type10
        N = N+1
        INTBUF_TAB(NI)%S_CAND_F       = INTBUF_SIZE(N)
!type20
        N = N+1
        INTBUF_TAB(NI)%S_XA           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_VA           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFA         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_AVX_ANCR     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_SH       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_FX      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_FY      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CAND_FZ      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_SE       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_ME       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STF          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFNE        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_CRITX        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_PENISE       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_PENIME       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_PENIA        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ALPHAK       = INTBUF_SIZE(N)
!type1
        N = N+1
        INTBUF_TAB(NI)%S_N            = INTBUF_SIZE(N)
!type 3,4,5,9
        N = N+1
        INTBUF_TAB(NI)%S_CSTM         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_EE           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STFNM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FRICOS       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FRICOM       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAV        = INTBUF_SIZE(N)
!type6
        N = N+1
        INTBUF_TAB(NI)%S_FCONT        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FS           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FM           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_RMAS         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ANSMX0       = INTBUF_SIZE(N)
!type8
        N = N+1
        INTBUF_TAB(NI)%S_T8          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAPN         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STF8         = INTBUF_SIZE(N)
!type14
        N = N+1
        INTBUF_TAB(NI)%S_CIMP         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NIMP         = INTBUF_SIZE(N)
!type14
        N = N+1
        INTBUF_TAB(NI)%S_IOLD         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_HOLD         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NOLD         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DOLD         = INTBUF_SIZE(N)
!type17
        N = N+1
        INTBUF_TAB(NI)%S_KS           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_KM           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FROTS        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FROTM        = INTBUF_SIZE(N)
!type21
        N = N+1
        INTBUF_TAB(NI)%S_NOD_NORMAL   = INTBUF_SIZE(N)
!type21
        N = N+1
        INTBUF_TAB(NI)%S_RCURV        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ANGLM        = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FROT_P       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_ALPHA0       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_AS           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_BS           = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_THKNOD0      = INTBUF_SIZE(N)
!type24 & 25
        N = N+1
        INTBUF_TAB(NI)%S_GAPN_M       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_SECND_FR      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_PENE_OLD     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_STIF_OLD     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_TIME_S       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_NM       = INTBUF_SIZE(N)
C-----to be cleaned after        
        N = N+1
        INTBUF_TAB(NI)%S_EDGE8L2      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NOD_2RY_LGTH = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_NOD_MAS_LGTH = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_GAP_N0       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DGAP_NM      = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DGAP_M       = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DELTA_PMAX_DGAP = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_XFIC          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_VFIC          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_MSFIC          = INTBUF_SIZE(N)
!type25
        N = N+1
        INTBUF_TAB(NI)%S_EDGE_BISECTOR  = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_PENM          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DISTM         = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LBM          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_LCM          = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_VTX_BISECTOR = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVX_E     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVY_E     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVZ_E     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVX_E2S   = INTBUF_SIZE(N)
        N = N+1 
        INTBUF_TAB(NI)%S_FTSAVY_E2S   = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_FTSAVZ_E2S   = INTBUF_SIZE(N)
!type2
        N = N+1
        INTBUF_TAB(NI)%S_CSTS_BIS     = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_DIR_FRICM = INTBUF_SIZE(N)
!type25
        N = N+1
        INTBUF_TAB(NI)%S_GAPMSAV = INTBUF_SIZE(N)
        N = N+1
        INTBUF_TAB(NI)%S_E2S_NOD_NORMAL = INTBUF_SIZE(N)

C=======================================================================
        IF(N /= L_INTBUF_SIZE)THEN
          WRITE(ISTDO,'(A)') 
     .            ' ** Internal error in routine INTBUF_INI'
          CALL ARRET(2)
        END IF
C=======================================================================
c READ INTEGER ARRAYS
C=======================================================================
        SIZ = INTBUF_TAB(NI)%S_IRECTS
        ALLOCATE(INTBUF_TAB(NI)%IRECTS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRECTS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IRECTM
        ALLOCATE(INTBUF_TAB(NI)%IRECTM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRECTM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NSV
        ALLOCATE(INTBUF_TAB(NI)%NSV(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NSV,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MSR
        ALLOCATE(INTBUF_TAB(NI)%MSR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IRTLM
        ALLOCATE(INTBUF_TAB(NI)%IRTLM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRTLM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IRUPT
        ALLOCATE(INTBUF_TAB(NI)%IRUPT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRUPT,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MSEGTYP2
        ALLOCATE(INTBUF_TAB(NI)%MSEGTYP2(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSEGTYP2,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_INORM
        ALLOCATE(INTBUF_TAB(NI)%INORM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%INORM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IELEC
        ALLOCATE(INTBUF_TAB(NI)%IELEC(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IELEC,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IELES
        ALLOCATE(INTBUF_TAB(NI)%IELES(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IELES,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LISUB
        ALLOCATE(INTBUF_TAB(NI)%LISUB(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LISUB,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_TYPSUB
        ALLOCATE(INTBUF_TAB(NI)%TYPSUB(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%TYPSUB,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADDSUBS
        ALLOCATE(INTBUF_TAB(NI)%ADDSUBS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADDSUBS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADDSUBM
        ALLOCATE(INTBUF_TAB(NI)%ADDSUBM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADDSUBM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LISUBS
        ALLOCATE(INTBUF_TAB(NI)%LISUBS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LISUBS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LISUBM
        ALLOCATE(INTBUF_TAB(NI)%LISUBM(SIZ+1))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LISUBM,SIZ)
          !sentinel value to avoid invalid read in i*for3.F
          INTBUF_TAB(NI)%LISUBM(SIZ+1) = HUGE(SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_INFLG_SUBS
        ALLOCATE(INTBUF_TAB(NI)%INFLG_SUBS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%INFLG_SUBS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_INFLG_SUBM
        ALLOCATE(INTBUF_TAB(NI)%INFLG_SUBM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%INFLG_SUBM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADDSUBE
        ALLOCATE(INTBUF_TAB(NI)%ADDSUBE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADDSUBE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LISUBE
        ALLOCATE(INTBUF_TAB(NI)%LISUBE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LISUBE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_INFLG_SUBE
        ALLOCATE(INTBUF_TAB(NI)%INFLG_SUBE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%INFLG_SUBE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_E
        ALLOCATE(INTBUF_TAB(NI)%CAND_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CAND_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_N
        ALLOCATE(INTBUF_TAB(NI)%CAND_N(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CAND_N,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_I_STOK
        ALLOCATE(INTBUF_TAB(NI)%I_STOK(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%I_STOK,SIZ)
        ENDIF

!type20
        SIZ = INTBUF_TAB(NI)%S_LCAND_N
        ALLOCATE(INTBUF_TAB(NI)%LCAND_N(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LCAND_N,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LCAND_S
        ALLOCATE(INTBUF_TAB(NI)%LCAND_S(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LCAND_S,SIZ)
        ENDIF

!fin type20

        SIZ = INTBUF_TAB(NI)%S_I_STOK_E
        ALLOCATE(INTBUF_TAB(NI)%I_STOK_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%I_STOK_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IFPEN
        ALLOCATE(INTBUF_TAB(NI)%IFPEN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IFPEN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IPARTFRICS
        ALLOCATE(INTBUF_TAB(NI)%IPARTFRICS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IPARTFRICS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IPARTFRICM
        ALLOCATE(INTBUF_TAB(NI)%IPARTFRICM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IPARTFRICM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IPARTFRIC_E
        ALLOCATE(INTBUF_TAB(NI)%IPARTFRIC_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IPARTFRIC_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IREP_FRICM
        ALLOCATE(INTBUF_TAB(NI)%IREP_FRICM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IREP_FRICM,SIZ)
        ENDIF
       
        SIZ = INTBUF_TAB(NI)%S_KREMNODE
        ALLOCATE(INTBUF_TAB(NI)%KREMNODE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%KREMNODE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_REMNODE
        ALLOCATE(INTBUF_TAB(NI)%REMNODE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%REMNODE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_KREMNOR
        ALLOCATE(INTBUF_TAB(NI)%KREMNOR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%KREMNOR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_REMNOR
        ALLOCATE(INTBUF_TAB(NI)%REMNOR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%REMNOR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_KREMNODE_EDG
        ALLOCATE(INTBUF_TAB(NI)%KREMNODE_EDG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%KREMNODE_EDG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_REMNODE_EDG
        ALLOCATE(INTBUF_TAB(NI)%REMNODE_EDG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%REMNODE_EDG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_KREMNODE_E2S
        ALLOCATE(INTBUF_TAB(NI)%KREMNODE_E2S(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%KREMNODE_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_REMNODE_E2S
        ALLOCATE(INTBUF_TAB(NI)%REMNODE_E2S(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%REMNODE_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADCCM
        ALLOCATE(INTBUF_TAB(NI)%ADCCM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADCCM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CHAIN
        ALLOCATE(INTBUF_TAB(NI)%CHAIN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CHAIN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NIGE
        ALLOCATE(INTBUF_TAB(NI)%NIGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NIGE,SIZ)
        ENDIF

!type20
        SIZ = INTBUF_TAB(NI)%S_NBINFLG
        ALLOCATE(INTBUF_TAB(NI)%NBINFLG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NBINFLG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MBINFLG
        ALLOCATE(INTBUF_TAB(NI)%MBINFLG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MBINFLG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_EBINFLG
        ALLOCATE(INTBUF_TAB(NI)%EBINFLG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%EBINFLG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NLG
        ALLOCATE(INTBUF_TAB(NI)%NLG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NLG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DAANC6
        ALLOCATE(INTBUF_TAB(NI)%DAANC6(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%DAANC6,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISLINS
        ALLOCATE(INTBUF_TAB(NI)%ISLINS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISLINS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISLINM
        ALLOCATE(INTBUF_TAB(NI)%ISLINM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISLINM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IXLINS
        ALLOCATE(INTBUF_TAB(NI)%IXLINS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IXLINS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IXLINM
        ALLOCATE(INTBUF_TAB(NI)%IXLINM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IXLINM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MSRL
        ALLOCATE(INTBUF_TAB(NI)%MSRL(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSRL,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NSVL
        ALLOCATE(INTBUF_TAB(NI)%NSVL(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NSVL,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADCCM20
        ALLOCATE(INTBUF_TAB(NI)%ADCCM20(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADCCM20,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CHAIN20
        ALLOCATE(INTBUF_TAB(NI)%CHAIN20(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CHAIN20,SIZ)
        ENDIF
!fin type20

!type1
        SIZ = INTBUF_TAB(NI)%S_ILOCS
        ALLOCATE(INTBUF_TAB(NI)%ILOCS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ILOCS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NSEGM
        ALLOCATE(INTBUF_TAB(NI)%NSEGM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NSEGM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NRT
        ALLOCATE(INTBUF_TAB(NI)%NRT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NRT,SIZ)
        ENDIF
!type3
        SIZ = INTBUF_TAB(NI)%S_IRTLS
        ALLOCATE(INTBUF_TAB(NI)%IRTLS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRTLS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ILOCM
        ALLOCATE(INTBUF_TAB(NI)%ILOCM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ILOCM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IRTLOM
        ALLOCATE(INTBUF_TAB(NI)%IRTLOM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRTLOM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IRTLOS
        ALLOCATE(INTBUF_TAB(NI)%IRTLOS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRTLOS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NSEGS
        ALLOCATE(INTBUF_TAB(NI)%NSEGS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NSEGS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LNSV
        ALLOCATE(INTBUF_TAB(NI)%LNSV(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LNSV,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LMSR
        ALLOCATE(INTBUF_TAB(NI)%LMSR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LMSR,SIZ)
        ENDIF

!type4
        SIZ = INTBUF_TAB(NI)%S_IELEM
        ALLOCATE(INTBUF_TAB(NI)%IELEM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IELEM,SIZ)
        ENDIF

!type12
        SIZ = INTBUF_TAB(NI)%S_FCOUNT
        ALLOCATE(INTBUF_TAB(NI)%FCOUNT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%FCOUNT,SIZ)
        ENDIF

!type14
        SIZ = INTBUF_TAB(NI)%S_KSURF
        ALLOCATE(INTBUF_TAB(NI)%KSURF(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%KSURF,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IMPACT
        ALLOCATE(INTBUF_TAB(NI)%IMPACT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IMPACT,SIZ)
        ENDIF

!type21
        SIZ = INTBUF_TAB(NI)%S_MSR21
        ALLOCATE(INTBUF_TAB(NI)%MSR21(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSR21,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MNDD
        ALLOCATE(INTBUF_TAB(NI)%MNDD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MNDD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MSR_L
        ALLOCATE(INTBUF_TAB(NI)%MSR_L(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSR_L,SIZ)
        ENDIF

!type24----to be cleaned later
        SIZ = INTBUF_TAB(NI)%S_CAND_T
        ALLOCATE(INTBUF_TAB(NI)%CAND_T(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CAND_T,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MVOISIN
        ALLOCATE(INTBUF_TAB(NI)%MVOISIN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MVOISIN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NVOISIN
        ALLOCATE(INTBUF_TAB(NI)%NVOISIN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NVOISIN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MSEGLO
        ALLOCATE(INTBUF_TAB(NI)%MSEGLO(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSEGLO,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MSEGTYP24
        ALLOCATE(INTBUF_TAB(NI)%MSEGTYP24(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%MSEGTYP24,SIZ)
        ENDIF
C----------to be cleaned later        
        SIZ = INTBUF_TAB(NI)%S_ISEADD
        ALLOCATE(INTBUF_TAB(NI)%ISEADD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISEADD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISEDGE
        ALLOCATE(INTBUF_TAB(NI)%ISEDGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISEDGE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISEG_PXFEM
        ALLOCATE(INTBUF_TAB(NI)%ISEG_PXFEM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISEG_PXFEM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISEG_PLY
        ALLOCATE(INTBUF_TAB(NI)%ISEG_PLY(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISEG_PLY,SIZ)
        ENDIF
        
        SIZ = INTBUF_TAB(NI)%S_ICONT_I
        ALLOCATE(INTBUF_TAB(NI)%ICONT_I(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ICONT_I,SIZ)
        ENDIF
        SIZ = INTBUF_TAB(NI)%S_IRTSE
        ALLOCATE(INTBUF_TAB(NI)%IRTSE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IRTSE,SIZ)
        ENDIF
 
        SIZ = INTBUF_TAB(NI)%S_IS2SE
        ALLOCATE(INTBUF_TAB(NI)%IS2SE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IS2SE,SIZ)
        ENDIF
        SIZ = INTBUF_TAB(NI)%S_IS2PT
        ALLOCATE(INTBUF_TAB(NI)%IS2PT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IS2PT,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISPT2
        ALLOCATE(INTBUF_TAB(NI)%ISPT2(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISPT2,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISEGPT
        ALLOCATE(INTBUF_TAB(NI)%ISEGPT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISEGPT,SIZ)
        ENDIF
        SIZ = INTBUF_TAB(NI)%S_IS2ID
        ALLOCATE(INTBUF_TAB(NI)%IS2ID(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IS2ID,SIZ)
        ENDIF
!type25
        SIZ = INTBUF_TAB(NI)%S_EVOISIN
        ALLOCATE(INTBUF_TAB(NI)%EVOISIN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%EVOISIN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADMSR
        ALLOCATE(INTBUF_TAB(NI)%ADMSR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADMSR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LEDGE
        ALLOCATE(INTBUF_TAB(NI)%LEDGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LEDGE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LBOUND
        ALLOCATE(INTBUF_TAB(NI)%LBOUND(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%LBOUND,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ACTNOR
        ALLOCATE(INTBUF_TAB(NI)%ACTNOR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ACTNOR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FARM
        ALLOCATE(INTBUF_TAB(NI)%FARM(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%FARM(1:SIZ) = 0
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ADSKYN
        ALLOCATE(INTBUF_TAB(NI)%ADSKYN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADSKYN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IADNOR
        ALLOCATE(INTBUF_TAB(NI)%IADNOR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IADNOR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ISLIDE
        ALLOCATE(INTBUF_TAB(NI)%ISLIDE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ISLIDE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_KNOR2MSR
        ALLOCATE(INTBUF_TAB(NI)%KNOR2MSR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%KNOR2MSR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NOR2MSR
        ALLOCATE(INTBUF_TAB(NI)%NOR2MSR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%NOR2MSR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_OPT_N
        ALLOCATE(INTBUF_TAB(NI)%CAND_OPT_N(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CAND_OPT_N,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_OPT_E
        ALLOCATE(INTBUF_TAB(NI)%CAND_OPT_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%CAND_OPT_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IF_ADH
        ALLOCATE(INTBUF_TAB(NI)%IF_ADH(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IF_ADH,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CANDM_E2E
        ALLOCATE(INTBUF_TAB(NI)%CANDM_E2E(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%CANDM_E2E(1:SIZ) = 0
          SIZ =INTBUF_TAB(NI)%I_STOK_E(1)
          CALL READ_I_C(INTBUF_TAB(NI)%CANDM_E2E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CANDS_E2E
        ALLOCATE(INTBUF_TAB(NI)%CANDS_E2E(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%CANDS_E2E(1:SIZ) = 0
          SIZ =INTBUF_TAB(NI)%I_STOK_E(1)
          CALL READ_I_C(INTBUF_TAB(NI)%CANDS_E2E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CANDM_E2S
        ALLOCATE(INTBUF_TAB(NI)%CANDM_E2S(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%CANDM_E2S(1:SIZ) = 0
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)
          CALL READ_I_C(INTBUF_TAB(NI)%CANDM_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CANDS_E2S
        ALLOCATE(INTBUF_TAB(NI)%CANDS_E2S(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%CANDS_E2S(1:SIZ) = 0
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)
          CALL READ_I_C(INTBUF_TAB(NI)%CANDS_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IFPEN_E
        ALLOCATE(INTBUF_TAB(NI)%IFPEN_E(SIZ))
        IF(SIZ>0)THEN
c          INTBUF_TAB(NI)%IFPEN_E(1:SIZ) = 0
c          SIZ =INTBUF_TAB(NI)%I_STOK_E(1)
          CALL READ_I_C(INTBUF_TAB(NI)%IFPEN_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IFPEN_E2S
        ALLOCATE(INTBUF_TAB(NI)%IFPEN_E2S(SIZ))
        IF(SIZ>0)THEN
c          INTBUF_TAB(NI)%IFPEN_E2S(1:SIZ) = 0
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)
          CALL READ_I_C(INTBUF_TAB(NI)%IFPEN_E2S,SIZ)
        ENDIF

! NITSCHE METHOD 
        SIZ = INTBUF_TAB(NI)%S_IELNRTS
        ALLOCATE(INTBUF_TAB(NI)%IELNRTS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%IELNRTS,SIZ)
        ENDIF
        SIZ = INTBUF_TAB(NI)%S_ADRECTS
        ALLOCATE(INTBUF_TAB(NI)%ADRECTS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%ADRECTS,SIZ)
        ENDIF
        SIZ = INTBUF_TAB(NI)%S_FACNRTS
        ALLOCATE(INTBUF_TAB(NI)%FACNRTS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_I_C(INTBUF_TAB(NI)%FACNRTS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_E2S_ACTNOR
        ALLOCATE(INTBUF_TAB(NI)%E2S_ACTNOR(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%E2S_ACTNOR(1:SIZ) = 0
        ENDIF
C=======================================================================
c READ REAL ARRAYS
C=======================================================================
        SIZ = INTBUF_TAB(NI)%S_STFAC
        ALLOCATE(INTBUF_TAB(NI)%STFAC(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFAC,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_VARIABLES
        ALLOCATE(INTBUF_TAB(NI)%VARIABLES(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%VARIABLES,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CSTS
        ALLOCATE(INTBUF_TAB(NI)%CSTS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CSTS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DPARA
        ALLOCATE(INTBUF_TAB(NI)%DPARA(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DPARA,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NMAS
        ALLOCATE(INTBUF_TAB(NI)%NMAS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%NMAS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_AREAS2
        ALLOCATE(INTBUF_TAB(NI)%AREAS2(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%AREAS2,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_SMAS
        ALLOCATE(INTBUF_TAB(NI)%SMAS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%SMAS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_SINER
        ALLOCATE(INTBUF_TAB(NI)%SINER(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%SINER,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_UVAR
        ALLOCATE(INTBUF_TAB(NI)%UVAR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%UVAR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_XM0
        ALLOCATE(INTBUF_TAB(NI)%XM0(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%XM0,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_SPENALTY
        ALLOCATE(INTBUF_TAB(NI)%SPENALTY(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%SPENALTY,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STFR_PENALTY
        ALLOCATE(INTBUF_TAB(NI)%STFR_PENALTY(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFR_PENALTY,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_SKEW
        ALLOCATE(INTBUF_TAB(NI)%SKEW(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%SKEW,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DSM
        ALLOCATE(INTBUF_TAB(NI)%DSM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DSM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FSM
        ALLOCATE(INTBUF_TAB(NI)%FSM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FSM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_RUPT
        ALLOCATE(INTBUF_TAB(NI)%RUPT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%RUPT,SIZ)
        ENDIF
 
        SIZ = INTBUF_TAB(NI)%S_FINI
        ALLOCATE(INTBUF_TAB(NI)%FINI(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FINI,SIZ)
        ENDIF        

        SIZ = INTBUF_TAB(NI)%S_STFNS
        ALLOCATE(INTBUF_TAB(NI)%STFNS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFNS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STFM
        ALLOCATE(INTBUF_TAB(NI)%STFM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STFS
        ALLOCATE(INTBUF_TAB(NI)%STFS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENIM
        ALLOCATE(INTBUF_TAB(NI)%PENIM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%PENIM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENIS
        ALLOCATE(INTBUF_TAB(NI)%PENIS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%PENIS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STIFMSDT_S
        ALLOCATE(INTBUF_TAB(NI)%STIFMSDT_S(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STIFMSDT_S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STIFMSDT_M
        ALLOCATE(INTBUF_TAB(NI)%STIFMSDT_M(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STIFMSDT_M,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_M
        ALLOCATE(INTBUF_TAB(NI)%GAP_M(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_M,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_S
        ALLOCATE(INTBUF_TAB(NI)%GAP_S(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_XSAV
        ALLOCATE(INTBUF_TAB(NI)%XSAV(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%XSAV,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CRIT
        ALLOCATE(INTBUF_TAB(NI)%CRIT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CRIT,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FRIC_P
        ALLOCATE(INTBUF_TAB(NI)%FRIC_P(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FRIC_P,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_XFILTR
        ALLOCATE(INTBUF_TAB(NI)%XFILTR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%XFILTR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_AREAS
        ALLOCATE(INTBUF_TAB(NI)%AREAS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%AREAS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_AREAM
        ALLOCATE(INTBUF_TAB(NI)%AREAM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%AREAM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_ML
        ALLOCATE(INTBUF_TAB(NI)%GAP_ML(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_ML,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_SL
        ALLOCATE(INTBUF_TAB(NI)%GAP_SL(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_SL,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_P
        ALLOCATE(INTBUF_TAB(NI)%CAND_P(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CAND_P,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_PS
        ALLOCATE(INTBUF_TAB(NI)%CAND_PS(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%CAND_PS(1:SIZ) = ZERO
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)*4
          CALL READ_DB(INTBUF_TAB(NI)%CAND_PS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAPE
        ALLOCATE(INTBUF_TAB(NI)%GAPE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAPE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_E_L
        ALLOCATE(INTBUF_TAB(NI)%GAP_E_L(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_E_L,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STFE
        ALLOCATE(INTBUF_TAB(NI)%STFE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STIFMSDT_EDG
        ALLOCATE(INTBUF_TAB(NI)%STIFMSDT_EDG(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STIFMSDT_EDG,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_RIGE
        ALLOCATE(INTBUF_TAB(NI)%RIGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%RIGE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_XIGE
        ALLOCATE(INTBUF_TAB(NI)%XIGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%XIGE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_VIGE
        ALLOCATE(INTBUF_TAB(NI)%VIGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%VIGE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_MASSIGE
        ALLOCATE(INTBUF_TAB(NI)%MASSIGE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%MASSIGE,SIZ)
        ENDIF

!type10
        SIZ = INTBUF_TAB(NI)%S_CAND_F
        ALLOCATE(INTBUF_TAB(NI)%CAND_F(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CAND_F,SIZ)
        ENDIF
!type10
        SIZ = INTBUF_TAB(NI)%S_FTSAVX
        ALLOCATE(INTBUF_TAB(NI)%FTSAVX(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVX,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVY
        ALLOCATE(INTBUF_TAB(NI)%FTSAVY(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVY,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ
        ALLOCATE(INTBUF_TAB(NI)%FTSAVZ(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVZ,SIZ)
        ENDIF

!type20
        SIZ = INTBUF_TAB(NI)%S_CAND_FX
        ALLOCATE(INTBUF_TAB(NI)%CAND_FX(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CAND_FX,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_FY
        ALLOCATE(INTBUF_TAB(NI)%CAND_FY(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CAND_FY,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CAND_FZ
        ALLOCATE(INTBUF_TAB(NI)%CAND_FZ(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CAND_FZ,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_XA
        ALLOCATE(INTBUF_TAB(NI)%XA(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%XA,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_VA
        ALLOCATE(INTBUF_TAB(NI)%VA(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%VA,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STFA
        ALLOCATE(INTBUF_TAB(NI)%STFA(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFA,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENIA
        ALLOCATE(INTBUF_TAB(NI)%PENIA(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%PENIA,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ALPHAK
        ALLOCATE(INTBUF_TAB(NI)%ALPHAK(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%ALPHAK,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_SH
        ALLOCATE(INTBUF_TAB(NI)%GAP_SH(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_SH,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_AVX_ANCR
        ALLOCATE(INTBUF_TAB(NI)%AVX_ANCR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%AVX_ANCR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_CRITX
        ALLOCATE(INTBUF_TAB(NI)%CRITX(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CRITX,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_ME
        ALLOCATE(INTBUF_TAB(NI)%GAP_ME(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_ME,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STF
        ALLOCATE(INTBUF_TAB(NI)%STF(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STF,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENIME
        ALLOCATE(INTBUF_TAB(NI)%PENIME(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%PENIME,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_SE
        ALLOCATE(INTBUF_TAB(NI)%GAP_SE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_SE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENISE
        ALLOCATE(INTBUF_TAB(NI)%PENISE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%PENISE,SIZ)
        ENDIF
!type20

!type1
        SIZ = INTBUF_TAB(NI)%S_N
        ALLOCATE(INTBUF_TAB(NI)%N(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%N,SIZ)
        ENDIF

!type 3,4,5,9
        SIZ = INTBUF_TAB(NI)%S_CSTM
        ALLOCATE(INTBUF_TAB(NI)%CSTM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CSTM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_EE
        ALLOCATE(INTBUF_TAB(NI)%EE(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%EE,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STFNM
        ALLOCATE(INTBUF_TAB(NI)%STFNM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STFNM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FRICOS
        ALLOCATE(INTBUF_TAB(NI)%FRICOS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FRICOS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FRICOM
        ALLOCATE(INTBUF_TAB(NI)%FRICOM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FRICOM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAV
        ALLOCATE(INTBUF_TAB(NI)%FTSAV(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAV,SIZ)
        ENDIF

!type 6
        SIZ = INTBUF_TAB(NI)%S_FCONT
        ALLOCATE(INTBUF_TAB(NI)%FCONT(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FCONT,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FS
        ALLOCATE(INTBUF_TAB(NI)%FS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FM
        ALLOCATE(INTBUF_TAB(NI)%FM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_RMAS
        ALLOCATE(INTBUF_TAB(NI)%RMAS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%RMAS,SIZ)
        ENDIF
        SIZ = INTBUF_TAB(NI)%S_ANSMX0
        ALLOCATE(INTBUF_TAB(NI)%ANSMX0(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%ANSMX0,SIZ)
        ENDIF
!type 8
        SIZ = INTBUF_TAB(NI)%S_GAPN
        ALLOCATE(INTBUF_TAB(NI)%GAPN(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAPN,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STF8
        ALLOCATE(INTBUF_TAB(NI)%STF8(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STF8,SIZ)
        ENDIF

!type 14
        SIZ = INTBUF_TAB(NI)%S_CIMP
        ALLOCATE(INTBUF_TAB(NI)%CIMP(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CIMP,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NIMP
        ALLOCATE(INTBUF_TAB(NI)%NIMP(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%NIMP,SIZ)
        ENDIF

!type 15
        SIZ = INTBUF_TAB(NI)%S_IOLD
        ALLOCATE(INTBUF_TAB(NI)%IOLD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%IOLD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_HOLD
        ALLOCATE(INTBUF_TAB(NI)%HOLD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%HOLD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NOLD
        ALLOCATE(INTBUF_TAB(NI)%NOLD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%NOLD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DOLD
        ALLOCATE(INTBUF_TAB(NI)%DOLD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DOLD,SIZ)
        ENDIF

!type 17
        SIZ = INTBUF_TAB(NI)%S_KS
        ALLOCATE(INTBUF_TAB(NI)%KS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%KS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_KM
        ALLOCATE(INTBUF_TAB(NI)%KM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%KM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FROTS
        ALLOCATE(INTBUF_TAB(NI)%FROTS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FROTS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FROTM
        ALLOCATE(INTBUF_TAB(NI)%FROTM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FROTM,SIZ)
        ENDIF

!
        SIZ = INTBUF_TAB(NI)%S_RCURV
        ALLOCATE(INTBUF_TAB(NI)%RCURV(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%RCURV,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ANGLM
        ALLOCATE(INTBUF_TAB(NI)%ANGLM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%ANGLM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FROT_P
        ALLOCATE(INTBUF_TAB(NI)%FROT_P(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FROT_P,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_ALPHA0
        ALLOCATE(INTBUF_TAB(NI)%ALPHA0(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%ALPHA0,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_AS
        ALLOCATE(INTBUF_TAB(NI)%AS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%AS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_BS
        ALLOCATE(INTBUF_TAB(NI)%BS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%BS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_THKNOD0
        ALLOCATE(INTBUF_TAB(NI)%THKNOD0(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%THKNOD0,SIZ)
        ENDIF

!type 24 & 25

        SIZ = INTBUF_TAB(NI)%S_GAPN_M
        ALLOCATE(INTBUF_TAB(NI)%GAPN_M(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAPN_M,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_SECND_FR
        ALLOCATE(INTBUF_TAB(NI)%SECND_FR(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%SECND_FR,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENE_OLD
        ALLOCATE(INTBUF_TAB(NI)%PENE_OLD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%PENE_OLD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_STIF_OLD
        ALLOCATE(INTBUF_TAB(NI)%STIF_OLD(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%STIF_OLD,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_TIME_S
        ALLOCATE(INTBUF_TAB(NI)%TIME_S(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%TIME_S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_NM
        ALLOCATE(INTBUF_TAB(NI)%GAP_NM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_NM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_EDGE8L2
        ALLOCATE(INTBUF_TAB(NI)%EDGE8L2(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%EDGE8L2,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NOD_2RY_LGTH
        ALLOCATE(INTBUF_TAB(NI)%NOD_2RY_LGTH(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%NOD_2RY_LGTH,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_NOD_MAS_LGTH
        ALLOCATE(INTBUF_TAB(NI)%NOD_MAS_LGTH(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%NOD_MAS_LGTH,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAP_N0
        ALLOCATE(INTBUF_TAB(NI)%GAP_N0(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAP_N0,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DGAP_NM
        ALLOCATE(INTBUF_TAB(NI)%DGAP_NM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DGAP_NM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DGAP_M
        ALLOCATE(INTBUF_TAB(NI)%DGAP_M(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DGAP_M,SIZ)
        ENDIF  

        SIZ = INTBUF_TAB(NI)%S_DELTA_PMAX_DGAP
        ALLOCATE(INTBUF_TAB(NI)%DELTA_PMAX_DGAP(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DELTA_PMAX_DGAP,SIZ)
        ENDIF  
        
        SIZ = INTBUF_TAB(NI)%S_XFIC
        ALLOCATE(INTBUF_TAB(NI)%XFIC(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%XFIC,SIZ)
        ENDIF  
        
        SIZ = INTBUF_TAB(NI)%S_VFIC
        ALLOCATE(INTBUF_TAB(NI)%VFIC(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%VFIC,SIZ)
        ENDIF  

        SIZ = INTBUF_TAB(NI)%S_MSFIC

        ALLOCATE(INTBUF_TAB(NI)%MSFIC(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%MSFIC,SIZ)
        ENDIF  

!type 25
        SIZ = INTBUF_TAB(NI)%S_EDGE_BISECTOR
        ALLOCATE(INTBUF_TAB(NI)%EDGE_BISECTOR(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%EDGE_BISECTOR(1:SIZ) = 0
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_PENM
        ALLOCATE(INTBUF_TAB(NI)%PENM(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%PENM(1:SIZ) = 0
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_DISTM
        ALLOCATE(INTBUF_TAB(NI)%DISTM(SIZ))
        IF(SIZ>0)THEN
           INTBUF_TAB(NI)%DISTM(1:SIZ) = 0
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LBM
        ALLOCATE(INTBUF_TAB(NI)%LBM(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%LBM(1:SIZ) = 0
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_LCM
        ALLOCATE(INTBUF_TAB(NI)%LCM(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%LCM(1:SIZ) = 0
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_VTX_BISECTOR
        ALLOCATE(INTBUF_TAB(NI)%VTX_BISECTOR(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%VTX_BISECTOR(1:SIZ) = 0
        ENDIF

!type 21 & 25
        SIZ = INTBUF_TAB(NI)%S_NOD_NORMAL
        ALLOCATE(INTBUF_TAB(NI)%NOD_NORMAL(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%NOD_NORMAL,SIZ)
        ENDIF

!type 8
        IF(INTBUF_TAB(NI)%S_T8 == 1) THEN
          CALL INTBUF_INI_T8(INTBUF_TAB(NI)%T8)  
        ENDIF
!type 2
        SIZ = INTBUF_TAB(NI)%S_CSTS_BIS
        ALLOCATE(INTBUF_TAB(NI)%CSTS_BIS(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%CSTS_BIS,SIZ)
        ENDIF
!Orthotropic Friction
        SIZ = INTBUF_TAB(NI)%S_DIR_FRICM
        ALLOCATE(INTBUF_TAB(NI)%DIR_FRICM(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%DIR_FRICM,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAPMSAV
        ALLOCATE(INTBUF_TAB(NI)%GAPMSAV(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%GAPMSAV,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVX_E
        ALLOCATE(INTBUF_TAB(NI)%FTSAVX_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVX_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVY_E
        ALLOCATE(INTBUF_TAB(NI)%FTSAVY_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVY_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ_E
        ALLOCATE(INTBUF_TAB(NI)%FTSAVZ_E(SIZ))
        IF(SIZ>0)THEN
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVZ_E,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVX_E2S
        ALLOCATE(INTBUF_TAB(NI)%FTSAVX_E2S(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%FTSAVX_E2S(1:SIZ) = ZERO
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)*4
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVX_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVY_E2S
        ALLOCATE(INTBUF_TAB(NI)%FTSAVY_E2S(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%FTSAVY_E2S(1:SIZ) = ZERO
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)*4
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVY_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ_E2S
        ALLOCATE(INTBUF_TAB(NI)%FTSAVZ_E2S(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%FTSAVZ_E2S(1:SIZ) = ZERO
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)*4
          CALL READ_DB(INTBUF_TAB(NI)%FTSAVZ_E2S,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_E2S_NOD_NORMAL
        ALLOCATE(INTBUF_TAB(NI)%E2S_NOD_NORMAL(SIZ))
        IF(SIZ>0)THEN
          INTBUF_TAB(NI)%E2S_NOD_NORMAL(1:SIZ) = 0
        ENDIF

      ENDDO !NI=1,NINTER

      DEALLOCATE(INTBUF_SIZE)
C-----
      RETURN

      END SUBROUTINE INTBUF_INI
Chd|====================================================================
Chd|  INTBUF_INI_T8                 interf/intbuf_ini.F           
Chd|-- called by -----------
Chd|        INTBUF_INI                    common_source/interf/intbuf_ini.F
Chd|-- calls ---------------
Chd|        READ_I_C                      tools/input_output/write_routtines.c
Chd|        INT8_MOD                      modules/interfaces/int8_mod.F 
Chd|====================================================================
      SUBROUTINE INTBUF_INI_T8(T8)  
C Reads the T8 structures

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD 
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
      TYPE(INT8_STRUCT_) :: T8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NBS_INTERF,NBM_INTERF,SIZ,P2
      IF(NSPMD > 1) THEN
      ! Specific T8 structures has to be read only 
      ! In SPMD

       T8%IS_ACTIVATED = 0
       
       CALL READ_I_C(SIZ,1)
       T8%S_COMM = SIZ
C      WRITE(6,*) __FILE__,__LINE__,"S_COMM=",SIZ
       ALLOCATE(T8%SPMD_COMM_PATTERN(SIZ))
       DO K=1,T8%S_COMM 
         CALL READ_I_C(T8%SPMD_COMM_PATTERN(K)%NUMLOC,1)
         CALL READ_I_C(T8%SPMD_COMM_PATTERN(K)%UID,1)
         CALL READ_I_C(T8%SPMD_COMM_PATTERN(K)%NBCOM,1)
         ALLOCATE(T8%SPMD_COMM_PATTERN(K)%PROCLIST(NSPMD))
         ALLOCATE(T8%SPMD_COMM_PATTERN(K)%BUF_INDEX(NSPMD))
         CALL READ_I_C(T8%SPMD_COMM_PATTERN(K)%PROCLIST,NSPMD)
         CALL READ_I_C(T8%SPMD_COMM_PATTERN(K)%BUF_INDEX,NSPMD)
       ENDDO
       ALLOCATE(T8%BUFFER(NSPMD))
       DO P2=1,NSPMD
          CALL READ_I_C(NBM_INTERF,1)
          CALL READ_I_C(NBS_INTERF,1)        
          T8%BUFFER(P2)%NBSECND_TOT = NBS_INTERF 
          T8%BUFFER(P2)%NBMAIN    = NBM_INTERF 
         
          !nb secnds per main

          IF(NBM_INTERF > 0) THEN
            ALLOCATE(T8%BUFFER(P2)%NBSECND(NBM_INTERF))
            ALLOCATE(T8%BUFFER(P2)%MAIN_ID(NBM_INTERF))
            ALLOCATE(T8%BUFFER(P2)%MAIN_UID(NBM_INTERF))
            CALL READ_I_C(T8%BUFFER(P2)%NBSECND,NBM_INTERF)
            CALL READ_I_C(T8%BUFFER(P2)%MAIN_ID,NBM_INTERF)
            CALL READ_I_C(T8%BUFFER(P2)%MAIN_UID,NBM_INTERF)
          ELSE
            ALLOCATE(T8%BUFFER(P2)%NBSECND(1))
            ALLOCATE(T8%BUFFER(P2)%MAIN_ID(1))
            ALLOCATE(T8%BUFFER(P2)%MAIN_UID(1))
          ENDIF
         
          IF(NBS_INTERF > 0) THEN
            ALLOCATE(T8%BUFFER(P2)%SECND_ID(NBS_INTERF))
            ALLOCATE(T8%BUFFER(P2)%SECND_UID(NBS_INTERF))
C           ALLOCATE(T8%BUFFER(P2)%NEW_MAIN_UID(NBS_INTERF))
C           ALLOCATE(T8%BUFFER(P2)%DISTANCE(NBS_INTERF))
C           T8%BUFFER(P2)%NEW_MAIN_UID(1:NBS_INTERF) = 0
C           T8%BUFFER(P2)%DISTANCE(1:NBS_INTERF) = 0
C           CALL READ_I_C(T8%BUFFER(P2)%SECND_ID,NBS_INTERF)
C           CALL READ_I_C(T8%BUFFER(P2)%SECND_UID,NBS_INTERF)
            !WRITE(6,*) "SID ="
            !WRITE(6,*) T8%BUFFER(P2)%SECND_ID(1:NBS_INTERF)
            !WRITE(6,*) "SUID ="
            !WRITE(6,*) T8%BUFFER(P2)%SECND_UID(1:NBS_INTERF)
         
c           ALLOCATE(T8%BUFFER(P2)%BUFR(NBS_INTERF*2))
c           ALLOCATE(T8%BUFFER(P2)%BUFI(NBS_INTERF*3))
          !WRITE(6,*) ISPMD+1,T8%BUFFER(P2)%SECND_UID(1:NBS_INTERF)
         ELSE
           ALLOCATE(T8%BUFFER(P2)%SECND_ID(1))
           ALLOCATE(T8%BUFFER(P2)%SECND_UID(1))
c           ALLOCATE(T8%BUFFER(P2)%NEW_MAIN_UID(1))
c           ALLOCATE(T8%BUFFER(P2)%DISTANCE(1))
c           ALLOCATE(T8%BUFFER(P2)%BUFR(1))
c           ALLOCATE(T8%BUFFER(P2)%BUFI(1))
          ENDIF
       ENDDO
       ENDIF !NSPMD > 1

      RETURN
      END SUBROUTINE INTBUF_INI_T8

C-----
