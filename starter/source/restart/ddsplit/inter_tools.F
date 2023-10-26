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
Chd|  W_INTBUF_SIZE                 source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE W_INTBUF_SIZE(INTBUF_TAB_L)
c
c write on disk all sizes of INTBUF_TAB arrays (INTEGERs then REALs)
c
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) INTBUF_TAB_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: INTBUF_SIZE
      INTEGER N,L_INTBUF_SIZE
C ----------------------------------------
      ALLOCATE(INTBUF_SIZE(L_INTBUF_SIZE_MAX))
      INTBUF_SIZE(1:L_INTBUF_SIZE_MAX) = 0

      N = 1
C=======================================================================
C integer arrays sizes
C=======================================================================
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRECTS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRECTM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NSV
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRTLM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRUPT
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_INORM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IELEC
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IELES
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LISUB
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_TYPSUB
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADDSUBS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADDSUBM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LISUBS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LISUBM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_INFLG_SUBS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_INFLG_SUBM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADDSUBE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LISUBE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_INFLG_SUBE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSEGTYP
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_N
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_I_STOK
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_I_STOK_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IFPEN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KREMNODE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_REMNODE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KREMNOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_REMNOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADCCM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CHAIN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NIGE
!type20
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DAANC6
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NBINFLG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MBINFLG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_EBINFLG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NLG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISLINS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISLINM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IXLINS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IXLINM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NSVL
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSRL
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LCAND_N
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LCAND_S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADCCM20
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CHAIN20
!type1
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ILOCS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NSEGM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NRT
!type2
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSEGTYP2
!type3
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRTLS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ILOCM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRTLOM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRTLOS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NSEGS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LNSV
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LMSR
!type4
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IELEM
!type12
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FCOUNT
!type14
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KSURF
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IMPACT
!type21
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSR21
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MNDD
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSR_L
!type24 & 25
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MVOISIN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NVOISIN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSEGLO
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSEGTYP24
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISEADD    
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISEDGE      
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_T 
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISEG_PXFEM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISEG_PLY
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ICONT_I
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IRTSE   
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IS2SE  
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IS2PT 
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISPT2 
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISEGPT 
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IS2ID
!type25
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_EVOISIN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADMSR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LEDGE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LBOUND
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ACTNOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FARM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADSKYN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IADNOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ISLIDE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KNOR2MSR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NOR2MSR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_OPT_N
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_OPT_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IF_ADH
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CANDM_E2E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CANDS_E2E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CANDM_E2S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CANDS_E2S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IFPEN_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IFPEN_E2S
!Friction
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IPARTFRICS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IPARTFRICM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IPARTFRIC_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IELNRTS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ADRECTS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FACNRTS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IREP_FRICM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_E2S_ACTNOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KREMNODE_EDG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_REMNODE_EDG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KREMNODE_E2S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_REMNODE_E2S

C=======================================================================
C real arrays sizes
C=======================================================================
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFAC
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_VARIABLES
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CSTS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DPARA
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NMAS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_AREAS2
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_SMAS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_SINER
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_UVAR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_XM0
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_SPENALTY
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFR_PENALTY
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_SKEW
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DSM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FSM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_RUPT
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FINI
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFNS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENIM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENIS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STIFMSDT_S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STIFMSDT_M
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_M
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_XSAV
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CRIT
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FRIC_P
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_XFILTR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_AREAS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_AREAM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_ML
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_SL
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_P
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_PS
      N = N+1

      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAPE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_E_L
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STIFMSDT_EDG
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVX
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVY
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVZ
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_RIGE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_XIGE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_VIGE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MASSIGE
!type10
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_F
!type20
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_XA
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_VA
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFA
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_AVX_ANCR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_SH
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_FX
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_FY
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CAND_FZ
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_SE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_ME
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STF
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFNE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CRITX
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENISE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENIME
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENIA
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ALPHAK
      N = N+1
!type1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_N
!type 3,4,5,9
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CSTM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_EE
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STFNM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FRICOS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FRICOM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAV
!type 6
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FCONT
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_RMAS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ANSMX0
!type 8
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_T8
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAPN
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STF8
      N = N+1
!type 14
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CIMP
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NIMP
!type 15
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_IOLD
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_HOLD
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NOLD
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DOLD
!type 17
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_KM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FROTS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FROTM
!type 21
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NOD_NORMAL
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_RCURV
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ANGLM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FROT_P
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_ALPHA0
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_AS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_BS
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_THKNOD0
!type 24 & 25
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAPN_M
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_SECND_FR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENE_OLD
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_STIF_OLD
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_TIME_S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_NM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_EDGE8L2
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NOD_2RY_LGTH
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_NOD_MAS_LGTH
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAP_N0
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DGAP_NM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DGAP_M
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DELTA_PMAX_DGAP
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_XFIC
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_VFIC
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_MSFIC
!type 25
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_EDGE_BISECTOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_PENM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DISTM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LBM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_LCM
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_VTX_BISECTOR
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVX_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVY_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVZ_E
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVX_E2S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVY_E2S
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_FTSAVZ_E2S
!type2
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_CSTS_BIS
!type7 24 25 
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_DIR_FRICM
!type25
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_GAPMSAV
      N = N+1
      INTBUF_SIZE(N) = INTBUF_TAB_L%S_E2S_NOD_NORMAL

C=======================================================================
C write all INTBUF_TAB arrays sizes on disk
C=======================================================================
      L_INTBUF_SIZE = N
      IF(L_INTBUF_SIZE > L_INTBUF_SIZE_MAX)THEN
        WRITE(ISTDO,'(A,/,A)') 
     .          ' ** Internal error in routine W_INTBUF_SIZE:',
     .          '    Hard coded value for L_INTBUF_SIZE_MAX needs to be updated'
        CALL ARRET(2)
      END IF
C=======================================================================
      CALL WRITE_I_C(L_INTBUF_SIZE,1)
      CALL WRITE_I_C(INTBUF_SIZE,L_INTBUF_SIZE)

      DEALLOCATE(INTBUF_SIZE)

      RETURN
      END

C=======================================================================
C GENERIC ROUTINES TO SPLIT & WRITE ON DISK
C=======================================================================

Chd|====================================================================
Chd|  SPLIT_NODE_IVAL               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_IVAL(TAB,DIM1,DIM2,TAG)
c
c split & write node array (type INTEGER) with global value
c (see SPLIT_NODE_NODLOC for local values)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG(*),DIM1,DIM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))

      DO I=1, DIM1
        K=TAG(I)
        DO J=1,DIM2
          IBUF(DIM2*(I-1)+J) = TAB(DIM2*(K-1)+J)
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)

      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  FILTER_NODE_NODLOC            source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FILTER_NODE_NODLOC(TAB,DIM1,TAG,NODLOCAL) 
c
c Filter & write node array (type INTEGER) with local value
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG(*),DIM1,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))
      DO I=1, DIM1
        K = TAG(I)
        IF(K > 0) THEN
          IBUF(I) = NODLOCAL(TAB(I))
        ELSE
          IBUF(I) = -1 
        ENDIF
      ENDDO
c     WRITE(6,*) __FILE__,__LINE__,IBUF(1:DIM1)
      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  COPY_NODE_NODLOC              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_NODE_NODLOC(TAB,DIM1,NODLOCAL) 
c
c Copy & write node array (type INTEGER) with local value
c (see SPLIT_NODE_IVAL for global values)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIM1,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))
      DO I=1, DIM1
        IBUF(I) = NODLOCAL(TAB(I))
      ENDDO

c                                              
      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_NODE_NODLOC             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_NODLOC(TAB,DIM1,TAG,NODLOCAL) 
c
c split & write node array (type INTEGER) with local value
c (see SPLIT_NODE_IVAL for global values)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG(*),DIM1,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))
      DO I=1, DIM1
        K = TAG(I)
        IBUF(I) = NODLOCAL(TAB(K))
      ENDDO
!     WRITE(6,*) __FILE__,__LINE__,IBUF(1:DIM1)
      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_NODE_RVAL               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_RVAL(TAB,DIM1,DIM2,TAG)
c
c split & write node array (type REAL) with global value
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG(*),DIM1,DIM2

      my_real TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

      my_real, DIMENSION(:),ALLOCATABLE :: RBUF
C ----------------------------------------
      ALLOCATE(RBUF(DIM1*DIM2))
      DO I=1, DIM1
        K=TAG(I)
        DO J=1,DIM2
          RBUF(DIM2*(I-1)+J) = TAB(DIM2*(K-1)+J)
        ENDDO
      ENDDO

      CALL WRITE_DB(RBUF,DIM1*DIM2)
      DEALLOCATE(RBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_NODLOC              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_NODLOC(TAB,DIM1,DIM2,TAG_SEG,NODLOCAL)
c
c split & write segments array (type INTEGER) with local value
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_SEG(*),DIM1,DIM2,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NOD
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))
      IBUF = 0
      DO I=1, DIM1
        K=TAG_SEG(I)
        DO J=1,DIM2
          NOD = TAB(DIM2*(K-1)+J)
          IF (NOD > 0) THEN
            IBUF(DIM2*(I-1)+J) = NODLOCAL(NOD)
          ENDIF
        ENDDO
      ENDDO
      CALL WRITE_I_C(IBUF,DIM1*DIM2)
!     WRITE(6,*) __FILE__,__LINE__,IBUF(1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_SEGLOC              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_SEGLOC(TAB,DIM1,DIM2,TAG_SEG,SEGLOCAL)
c
c split & write segments array (type INTEGER) with local value
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_SEG(*),DIM1,DIM2,SEGLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,GLOB
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))
      DO I=1, DIM1
        K=TAG_SEG(I)
        DO J=1,DIM2
          GLOB=TAB(DIM2*(K-1)+J)
          IF(GLOB/=0)THEN
            IF(SEGLOCAL(GLOB)/=0)THEN
              IBUF(DIM2*(I-1)+J) = SEGLOCAL(GLOB) ! local if same proc
            ELSE
              IBUF(DIM2*(I-1)+J) = -GLOB
            END IF
          ELSE
            IBUF(DIM2*(I-1)+J) = 0
          END IF
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
!     WRITE(6,*) __FILE__,__LINE__,IBUF(1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  COPY_IVAL                     source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_IVAL(TAB,DIM1,DIM2)
c 
c copy and write array value (type INTEGER)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIM1,DIM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))

      DO I=1, DIM1
        DO J=1, DIM2
          IBUF(DIM2*(I-1)+J) = TAB(DIM2*(I-1)+J)
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  COPY_IVAL_IGEO                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_IVAL_IGEO(TAB,DIM1,DIM2,OFFSET)
c 
c copy and write array value (type INTEGER)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIM1,DIM2,OFFSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))

      DO I=1, DIM1
        DO J=1, DIM2
          IBUF(DIM2*(I-1)+J) = TAB(DIM2*(I-1)+J+OFFSET)
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  COPY_RVAL                     source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_RVAL(TAB,DIM1,DIM2)
c 
c copy and write array value (type REAL)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIM1,DIM2

      my_real 
     .        TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real, DIMENSION(:),ALLOCATABLE :: RBUF
C ----------------------------------------
      ALLOCATE(RBUF(DIM1*DIM2))

      DO I=1, DIM1
        DO J=1, DIM2
          RBUF(DIM2*(I-1)+J) = TAB(DIM2*(I-1)+J)
        ENDDO
      ENDDO

      CALL WRITE_DB(RBUF,DIM1*DIM2)
      DEALLOCATE(RBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_NODE_NODLOC_P0          source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_NODLOC_P0(TAB,DIM1,DIM2,NODLOCAL)
c
c same routine as SPLIT_NODE_NODLOC for interfaces splited 
c only on proc 1 (no TAG array used)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIM1,DIM2,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))
      DO I=1, DIM1
        DO J=1, DIM2
          IBUF(DIM2*(I-1)+J) = NODLOCAL(TAB(DIM2*(I-1)+J))
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  PREPARE_SPLIT_CAND            source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_CAND(INTBUF_TAB, TAG_SEGM2, II_STOK, TAG_II)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG_SEGM2(*),TAG_II(*),II_STOK

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        K,E,C_II
C ----------------------------------------

! prepare split candidates
      C_II = 0
      DO K = 1, II_STOK
        E = INTBUF_TAB%CAND_E(K)
        IF (TAG_SEGM2(E)/=0) THEN
          C_II = C_II + 1
          TAG_II(C_II) = K
        ENDIF
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_IVAL               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_IVAL(TAB    , II_STOK_L, TAG_II,
     .                           MULTIMP, NCONT )
c
c split & write candidates (INTEGERs arrays sized MULTIMP*NCONT)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_II(*),II_STOK_L,MULTIMP,NCONT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(MULTIMP*NCONT))
      IBUF(1:MULTIMP*NCONT) = 0

      DO I=1, II_STOK_L
        K=TAG_II(I)
        IBUF(I) = TAB(K)
      ENDDO

      CALL WRITE_I_C(IBUF,MULTIMP*NCONT)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_RVAL               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_RVAL(TAB,II_STOK_L,TAG_II,MULTIMP,NCONT)
c
c split & write candidates (REALs arrays sized MULTIMP*NCONT)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG_II(*),II_STOK_L,MULTIMP,NCONT

      my_real TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real, DIMENSION(:),ALLOCATABLE :: RBUF
C ----------------------------------------
      ALLOCATE(RBUF(MULTIMP*NCONT))
      RBUF(1:MULTIMP*NCONT) = 0

      DO I=1, II_STOK_L
        K=TAG_II(I)
        RBUF(I) = TAB(K)
      ENDDO

      CALL WRITE_DB(RBUF,MULTIMP*NCONT)
      DEALLOCATE(RBUF)

      RETURN
      END

C=======================================================================
C END GENERIC ROUTINES TO SPLIT & WRITE ON DISK
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT2
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I2              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I2(PROC, INTBUF_TAB  , NSN     , NMN      ,
     +                            NRTM, TAG_NODE_2RY, TAG_SEGM, TAG_SEGM2,
     +                            TAG_IRTL, TAG     , ITABI2M , NODLOCAL ,
     +                            NBDDI2M , NIR     ,NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN
      INTEGER, INTENT(IN) :: NUMNOD_L

      INTEGER PROC,TAG_NODE_2RY(*),TAG_SEGM(*),TAG_SEGM2(*),TAG(*),
     .        TAG_IRTL(*),ITABI2M(*),NBDDI2M,NIR
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NODLOCAL :  integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,K,L,M,N,JJ,
     .        CNRTM_L,CNSN_L,CNMN_L,MY_NODE
C ----------------------------------------
      DO K=1,NSN
        MY_NODE = INTBUF_TAB%NSV(K)
        IF( NODLOCAL( MY_NODE )/=0.AND.NODLOCAL( MY_NODE )<=NUMNOD_L ) THEN 
          L = INTBUF_TAB%IRTLM(K)
          TAG(L) = 1
        ENDIF
      ENDDO 

      CNRTM_L = 0
      DO K=1,NRTM
        IF(TAG(K)==1) THEN
            CNRTM_L = CNRTM_L + 1
            TAG_SEGM(CNRTM_L) = K
            TAG_SEGM2(K) = CNRTM_L

        END IF
      ENDDO   

      CNSN_L = 0
      DO K=1, NSN
        MY_NODE = INTBUF_TAB%NSV(K)
        IF( NODLOCAL( MY_NODE )/=0.AND.NODLOCAL( MY_NODE )<=NUMNOD_L ) THEN 
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
        ENDIF
      ENDDO

      CNMN_L = 0
      DO K = 1, NMN
        N = INTBUF_TAB%MSR(K)
        IF( NODLOCAL( N )/=0.AND.NODLOCAL( N )<=NUMNOD_L ) THEN
          IF(NBDDI2M>0)ITABI2M(NODLOCAL(N)) = 1
          CNMN_L = CNMN_L + 1
          TAG_IRTL(CNMN_L) = K
        ENDIF
      END DO

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_NODE_IVAL2              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_IVAL2(TAB,DIM1,DIM2,TAG_SEGM2,TAG_NODE_2RY)
c specific type2, need use of 2 TAG arrays (one for main segment, one for secnd node)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_SEGM2(*),TAG_NODE_2RY(*),DIM1,DIM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))

      DO I=1, DIM1
        K=TAG_NODE_2RY(I)
        DO J=1,DIM2
          IBUF(DIM2*(I-1)+J) = TAG_SEGM2(TAB(DIM2*(K-1)+J))
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_IVAL2               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_IVAL2(TAB,DIM1,TAG,TAG2)
c
c split segment array with global value specific type2
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIM1,TAG(*),TAG2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))

      DO I=1, DIM1
        K=TAG(I)
        IBUF(I) = TAB(K)
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT2
C=======================================================================


C=======================================================================
C SPECIFIC ROUTINES INT7
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I7              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        I7I10SPLITMOD                 share/modules1/i710split_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I7(PROC     ,   INTBUF_TAB  , IPARI       ,
     .                            INTERCEP ,   TAG_NODE_2RY, TAG_SEGM    ,
     .                            TAG_SEGM2,   TAG_NM      , TAG_NODE_MSR,
     .                            TAG_SCRATCH, NI, CEP, MULTI_FVM,I710XSAV,
     .                            NINDX_NM   , INDX_NM,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .                            NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
      USE I7I10SPLITMOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM

      INTEGER NI,PROC,IPARI(*),
     .        TAG_NODE_2RY(*),TAG_SEGM(*),TAG_NM(*),TAG_NODE_MSR(*),
     .        TAG_SEGM2(*),TAG_SCRATCH(*),CEP(*)
      INTEGER, INTENT(INOUT) :: NINDX_NM,NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_NM,INDX_SCRT
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL
      INTEGER, INTENT(IN) :: NUMNOD_L
      INTEGER I710XSAV(*)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_XXX : size = NUMNOD
!                 index of non-zero TAG_XXX value 
!                 used for optimize the initialization
!                 of TAG_XXX array (XXX = NM or SCRT for SCRATCH)
!                 allocated array in lectur and threadprivate array
!       NINDX_XXX : number of non-zero TAG_XXX value
!       TAG_XXX : size = NUMNOD
!                array used to tag an element for 
!                a given interface ; allocated in lectur
!                allocated array in lectur and threadprivate array
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
     .  
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN,
     .        I,J,K,N,N1,N2,N3,N4,E,
     .        CNSN_L,CNRTM_L,CNMN_L,NSN0,
     .        NSN_FE, NSN_IGE
C ----------------------------------------
      IF(IPARI(7) == 7) THEN
        NRTM = IPARI(4)
        NSN  = IPARI(5)
        NSN_IGE = IPARI(77)
        NMN  = IPARI(6)
        NSN0 = NSN - NSN_IGE
      ELSE
        NRTM = IPARI(4)
        NSN  = IPARI(5)
        NMN  = IPARI(6)
        NSN0 = NSN
        NSN_IGE = 0
      ENDIF

      CNSN_L = 0
      IF (MULTI_FVM%IS_USED .AND. ABS(IPARI(22)) == 7) THEN
C     Interface type 18 law 151
         DO K=1, NSN0
            N=INTBUF_TAB%NSV(K)
            IF(CEP(N) == PROC .AND.TAG_SCRATCH(N)==0) THEN
               CNSN_L = CNSN_L+1
               TAG_NODE_2RY(CNSN_L) = K
               TAG_SCRATCH(N)=1
               NINDX_SCRT = NINDX_SCRT + 1
               INDX_SCRT(NINDX_SCRT) = N
            ENDIF
         ENDDO
      ELSE
         DO K=1, NSN0
            N=INTBUF_TAB%NSV(K)
            IF(     (NODLOCAL(N)/=0.AND.NODLOCAL( N )<=NUMNOD_L)
     +         .AND.TAG_SCRATCH(N)==0) THEN
               CNSN_L = CNSN_L+1
               TAG_NODE_2RY(CNSN_L) = K
               TAG_SCRATCH(N)=1
               NINDX_SCRT = NINDX_SCRT + 1
               INDX_SCRT(NINDX_SCRT) = N
            ENDIF
         ENDDO
         DO K=NSN0+1, NSN0 + NSN_IGE ! Specifique surface IGEO
            N=INTBUF_TAB%NSV(K)
            IF(TAG_SCRATCH(N)==0) THEN
               CNSN_L = CNSN_L+1
               TAG_NODE_2RY(CNSN_L) = K
               TAG_SCRATCH(N)=1
               NINDX_SCRT = NINDX_SCRT + 1
               INDX_SCRT(NINDX_SCRT) = N
            ENDIF
         ENDDO
      ENDIF
      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0

! prepare SPLIT_NRTM_R
      CNRTM_L = 0
      CNMN_L = 0
      DO K=1,NRTM
        IF(INTERCEP%P(K)==PROC+1)THEN
          N1 = INTBUF_TAB%IRECTM(4*(K-1)+1)
          N2 = INTBUF_TAB%IRECTM(4*(K-1)+2)
          N3 = INTBUF_TAB%IRECTM(4*(K-1)+3)
          N4 = INTBUF_TAB%IRECTM(4*(K-1)+4)

          CNRTM_L = CNRTM_L + 1 
          TAG_SEGM(CNRTM_L) = K
          TAG_SEGM2(K) = CNRTM_L
          IF(TAG_NM(N1)==0)THEN
            TAG_NM(N1)=1
            CNMN_L = CNMN_L +1
            ! modif specific
            I710XSAV(CNMN_L) = N1
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N1
          ENDIF
          IF(TAG_NM(N2)==0)THEN
            TAG_NM(N2)=1
            CNMN_L = CNMN_L +1
            I710XSAV(CNMN_L) = N2
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N2
          ENDIF
          IF(TAG_NM(N3)==0)THEN
            TAG_NM(N3)=1
            CNMN_L = CNMN_L +1
            I710XSAV(CNMN_L) = N3
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N3
          ENDIF
          IF(TAG_NM(N4)==0)THEN
            TAG_NM(N4)=1
            CNMN_L = CNMN_L +1
            I710XSAV(CNMN_L) = N4
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N4
          ENDIF
        ENDIF
      ENDDO 

      CNMN_L = 0 
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        IF(TAG_NM(N)==1)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
        ENDIF
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_I7                 source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_I7(PROC     , INTBUF_TAB, NSN    , NSN_L       ,
     .                         TAG_SEGM2, II_STOK   , MULTIMP, NCONT       ,
     .                         NOINT    , INACTI    , TAG_SCRATCH ,
     .                         II_STOK_L, ITYP ,NINDX_SCRT,INDX_SCRT , NODLOCAL,
     .                         NUMNOD_L,NUMNOD,NUMELS,LEN_CEP,CEP,TYPE18_LAW151)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC,NSN,NSN_L,II_STOK,MULTIMP,NCONT,
     .        TAG_SEGM2(*),NOINT,INACTI,
     .        TAG_SCRATCH(*) , II_STOK_L, ITYP   
      INTEGER, INTENT(IN) :: NUMNOD_L !< number of node of the current proc
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_SCRT
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL
      INTEGER, INTENT(IN) :: NUMNOD !< total number of node
      INTEGER, INTENT(in) :: NUMELS !< number of solid element
      INTEGER, INTENT(in) :: LEN_CEP !< size of CEP array
      INTEGER, DIMENSION(LEN_CEP), INTENT(in) :: CEP !< element -> proc connectivity array
      LOGICAL, INTENT(in) :: TYPE18_LAW151
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_SCRT : size = NUMNOD + NUMELS + I24MAXNSNE2
!                 index of non-zero TAG_XXX value 
!                 used for optimize the initialization
!                 of TAG_XXX array (XXX = NM or SCRT for SCRATCH)
!                 allocated array in lectur and threadprivate array
!       NINDX_SCRT : number of non-zero TAG_XXX value
!       TAG_SCRATCH : size = NUMNOD + NUMELS + I24MAXNSNE2
!                array used to tag an element for 
!                a given interface ; allocated in lectur
!                allocated array in lectur and threadprivate array
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,P,E,MULTOK,MSGID,
     .        SPLIST,C_NSNR,MY_NODE
      INTEGER NUMP(NSPMD),WORK(70000)

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF_E,IBUF_N,NSNLOCAL,CPULOCAL,CANDR,PLIST,
     .  INDEX

      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI
C ----------------------------------------
      ALLOCATE(IBUF_E(MULTIMP*NCONT),IBUF_N(MULTIMP*NCONT))
      IBUF_E(1:MULTIMP*NCONT) = 0
      IBUF_N(1:MULTIMP*NCONT) = 0
      II_STOK_L = 0 !mandatory in case of inacti ne 5,6,7 

      IF(ITYP==23.OR.INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN 
        IF(NSN>0) THEN
          ALLOCATE(NSNLOCAL(NSN))
          ALLOCATE(CPULOCAL(NSN))
          ALLOCATE(CANDR(NSN))
        END IF
        NUMP(1:NSPMD) = 0

        ALLOCATE(PLIST(NSPMD)) 
        PLIST(1:NSPMD) = -1
        DO K=1,NSN
          N = INTBUF_TAB%NSV(K)
c          IF(N>NUMNOD) CYCLE
          NSNLOCAL(K) = 0
          IF(TAG_SCRATCH(N)==0) THEN 
            SPLIST=0
            ! ---------------------
            ! law151 + interface type 18 : N is a brick id N € [1;NUMELS]
            ! 
            IF(TYPE18_LAW151) THEN
              P = CEP(N) + 1
              NUMP(P) = NUMP(P)+1
              IF(P==PROC+1) THEN
                NSNLOCAL(K) = NUMP(P)
                CPULOCAL(K) = P
              ENDIF
            ! ---------------------
            ! other kind of interface : N is a node id N € [1;NUMNOD]
            ELSE
                CALL PLIST_IFRONT(PLIST,N,SPLIST) 
                DO I=1,SPLIST
                    P=PLIST(I)
                    NUMP(P) = NUMP(P)+1
                ENDDO
                IF( NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L ) THEN
                    NSNLOCAL(K) = NUMP(PROC+1)
                    CPULOCAL(K) = PROC+1
                ELSE
                    P=PLIST(1)
                    NSNLOCAL(K) = NUMP(P)
                    CPULOCAL(K) = P
                ENDIF              
            ENDIF
            ! ---------------------
            TAG_SCRATCH(N) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
        ENDDO
        DEALLOCATE(PLIST) 

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Reperage des candidats se trouvant sur des procs distants
C
        C_NSNR = 0

        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            N = INTBUF_TAB%CAND_N(K)
            IF(TAG_SCRATCH(N)==0) THEN
              TAG_SCRATCH(N) = 1
              NINDX_SCRT = NINDX_SCRT + 1
              INDX_SCRT(NINDX_SCRT) = N
              MY_NODE = INTBUF_TAB%NSV(N)
c              IF(INTBUF_TAB%NSV(N)>NUMNOD) CYCLE
              IF(TYPE18_LAW151) THEN
                  IF(CEP(MY_NODE)/=PROC) THEN
                    C_NSNR = C_NSNR + 1
                    CANDR(C_NSNR) = N
                  ENDIF
              ELSE
                  IF( NODLOCAL( MY_NODE )==0.OR.NODLOCAL(MY_NODE)>NUMNOD_L ) THEN
                    C_NSNR = C_NSNR + 1
                    CANDR(C_NSNR) = N
                  END IF
              ENDIF
            END IF
          ENDIF
        ENDDO

        !reflush TAG_SCRATCH to zero only when value has changes
!        DO K=1, II_STOK  
!         E = INTBUF_TAB%CAND_E(K)
!          IF (TAG_SEGM2(E)/=0) THEN
!            N = INTBUF_TAB%CAND_N(K)
!            TAG_SCRATCH(N) = 0
!          ENDIF
!        ENDDO
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Tris des candidats remote par proc et par nsv local croissant
C
        IF(C_NSNR>0) THEN
          ALLOCATE(INDEX(2*C_NSNR))
          ALLOCATE(ITRI(2,C_NSNR))
        END IF
        DO I = 1, C_NSNR
          N = CANDR(I)
          ITRI(1,I) = CPULOCAL(N)
          ITRI(2,I) = NSNLOCAL(N)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,C_NSNR,2)
C
        DO I = 1, C_NSNR
          INDEX(C_NSNR+INDEX(I)) = I
        ENDDO
        DO I = 1, C_NSNR
          INDEX(I)=INDEX(C_NSNR+I)
        ENDDO
C
        II_STOK_L = 0

        C_NSNR = 0
        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            II_STOK_L = II_STOK_L + 1
          END IF
        END DO

        IF(II_STOK_L>MULTIMP*NCONT)THEN
          MULTOK= II_STOK_L/NCONT
          CALL ANCMSG(MSGID=626,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MULTOK,
     .                I2=NOINT)
        ELSE
          II_STOK_L = 0
C
          DO K = 1, II_STOK
            E = INTBUF_TAB%CAND_E(K)
            IF (TAG_SEGM2(E)/=0) THEN
              N = INTBUF_TAB%CAND_N(K)
              II_STOK_L = II_STOK_L + 1
              IBUF_E(II_STOK_L)=TAG_SEGM2(E)
c              IF(INTBUF_TAB%NSV(N)>NUMNOD) CYCLE
              MY_NODE = INTBUF_TAB%NSV(N)
              IF( NODLOCAL( MY_NODE )/=0.AND.NODLOCAL(MY_NODE)<=NUMNOD_L ) THEN
                IBUF_N(II_STOK_L)=NSNLOCAL(N)
              ELSE
C noeud remote : numerotation pre calculee ci-dessus
c                IF(TAG(N)==0) THEN
                IF(TAG_SCRATCH(N)==0) THEN
                  C_NSNR = C_NSNR + 1
                  IBUF_N(II_STOK_L)=INDEX(C_NSNR)+NSN_L
                  TAG_SCRATCH(N) = INDEX(C_NSNR)+NSN_L
                  NINDX_SCRT = NINDX_SCRT + 1
                  INDX_SCRT(NINDX_SCRT) = N
                ELSE
                  IBUF_N(II_STOK_L) = TAG_SCRATCH(N)
                END IF ! TAG(N)==0
              END IF ! NODLOCAL/=0
            ENDIF !TAG_SEGM_2(E)/=0
          ENDDO !K = 1, II_STOK
        END IF !II_STOK_L>MULTIMP*NCONT

        !reflush TAG_SCRATCH to zero only when value has changes
!        DO K=1, II_STOK  
!         E = INTBUF_TAB%CAND_E(K)
!          IF (TAG_SEGM2(E)/=0) THEN
!            N = INTBUF_TAB%CAND_N(K)
!            TAG_SCRATCH(N) = 0
!          ENDIF
!        ENDDO
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
  
        IF(NSN>0) DEALLOCATE(NSNLOCAL,CPULOCAL,CANDR)
        IF(C_NSNR>0) DEALLOCATE(INDEX,ITRI)

c        IF(ITYP==23.OR.INACTI==5.OR.INACTI==6.OR.INACTI==7.
c     .          OR.IFQ>0)IPARI_L(24,NI)= C_NRTSR
         
      ENDIF !END INACTI=5,6,7 

      CALL WRITE_I_C(IBUF_E,MULTIMP*NCONT)
      CALL WRITE_I_C(IBUF_N,MULTIMP*NCONT)

      DEALLOCATE(IBUF_E,IBUF_N)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_REMNODE_I7              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_REMNODE_I7(PROC     , INTBUF_TAB, NRTM    , NRTM_L,
     .                            TAG_SEGM2, NREMNODE  , NODLOCAL, ITAB  ,NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER PROC,NRTM,NRTM_L,
     .        TAG_SEGM2(*),NREMNODE,NODLOCAL(*),
     .        ITAB(*)
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .        SIZ_TMP
      INTEGER, INTENT(IN) :: NUMNOD_L

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SIZ,
     .        L,SIZ1,SIZ2,M,N

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF1,IBUF2,NODDEL,NODDELREMOTE
C ----------------------------------------
      ALLOCATE(SIZ_TMP(NRTM),NODDEL(NUMNOD),
     .         NODDELREMOTE(NUMNOD))

      ALLOCATE(IBUF1(2*(NRTM_L + 1)), IBUF2(NREMNODE))
      IBUF1(1:2*(NRTM_L+1)) = 0
      IBUF2(1:NREMNODE) = 0

      SIZ_TMP(1:NRTM) = 0

      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN
          SIZ_TMP(TAG_SEGM2(K)) = INTBUF_TAB%KREMNODE(K+1)
     .              -INTBUF_TAB%KREMNODE(K)
        ENDIF
      END DO

      IBUF1(1) = 0

      NODDEL(1:NUMNOD) = 0
      NODDELREMOTE(1:NUMNOD) = 0
      SIZ1 = 0
      SIZ2 = 0
      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN

          SIZ = SIZ_TMP(TAG_SEGM2(K))
          IBUF1(1+2*TAG_SEGM2(K)) =IBUF1(1+2*(TAG_SEGM2(K)-1)) + SIZ

          L=INTBUF_TAB%KREMNODE(K)
          SIZ1 = 0
          SIZ2 = 0
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M)
            IF( NODLOCAL(N) /=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
              NODDEL(SIZ1+1) = NODLOCAL(N)
              SIZ1 = SIZ1+1
            ENDIF
          ENDDO
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M)
            IF( NODLOCAL( N) ==0.OR.NODLOCAL(N)>NUMNOD_L) THEN
              NODDELREMOTE(SIZ2+1) = ITAB(N)
              SIZ2 = SIZ2+1
            ENDIF
          ENDDO
          L=IBUF1(1+2*(TAG_SEGM2(K)-1))
          DO M=1,SIZ1
            IBUF2(1+L+M-1)= NODDEL(M)
          ENDDO
          IBUF1(1+2*(TAG_SEGM2(K)-1)+1) = L + SIZ1
          L=IBUF1(1+2*(TAG_SEGM2(K)-1)+1)
          DO M=1,SIZ2
            IBUF2(1+L+M-1) = - NODDELREMOTE(M)
          ENDDO
        ENDIF
        DO M=1,SIZ1
          NODDEL(M) = 0
        ENDDO
        DO M=1,SIZ2
          NODDELREMOTE(M) = 0
        ENDDO
      ENDDO

      DEALLOCATE(SIZ_TMP,NODDEL,NODDELREMOTE)

      CALL WRITE_I_C(IBUF1,2*(NRTM_L + 1))
      CALL WRITE_I_C(IBUF2,NREMNODE)

      DEALLOCATE(IBUF1, IBUF2)

      RETURN
      END
Chd|====================================================================
Chd|  PREPARE_SPLIT_CAND_I25_EDGE   source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
            SUBROUTINE PREPARE_SPLIT_CAND_I25_EDGE(INTBUF_TAB,SEGLOC,
     .                             TAG_EDGE, NEDGE_L, TAG_EDGE2, NEDGE,
     .                             II_STOK_E, II_STOK_E_L, TAG_II_E2E,
     .                             II_STOK_S, II_STOK_S_L, TAG_II_E2S, 
     .                             PROC     , FLAGREMNODE, IREMI2    ,
     .                             NRTM     , TAG_JJ_E2E , TAG_JJ_E2S)

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
#include      "assert.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEDGE,NEDGE_L     ! number of edges
      INTEGER :: TAG_EDGE(NEDGE_L), TAG_EDGE2(NEDGE)
      INTEGER :: SEGLOC(*) ! global to local id of segment
      INTEGER :: TAG_II_E2E(*) !local id of global E2E cand (OUT)
      INTEGER :: TAG_II_E2S(*) !local id of global E2S cand (OUT)
      INTEGER :: II_STOK_E, II_STOK_E_L 
      INTEGER :: II_STOK_S, II_STOK_S_L
      INTEGER :: PROC, IREMI2, FLAGREMNODE, NRTM
      INTEGER :: TAG_JJ_E2E(*) !Global id of global E2E cand (OUT)
      INTEGER :: TAG_JJ_E2S(*) !Global id of global E2S cand (OUT)

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,L,M,N1,N2,JJ
      INTEGER :: ID,E1,E2,K1,K2
      INTEGER :: NB_FREE_EDGES            ! number of free edges
      INTEGER :: NB_INTERNAL_EDGES        ! number of edges internal to the domain
      INTEGER :: NB_BOUNDARY_EDGES_LOCAL  ! boundary edges treated by current domain 
      INTEGER :: NB_BOUNDARY_EDGES_REMOTE ! boundary edges treated by the other domain 
      INTEGER, DIMENSION(:),ALLOCATABLE :: KCANDMS,ICANDMS,CANDMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGREMCAND_E2E, TAGREMCAND_E2S ! tabs to tag edges to be deactivated 
C ----------------------------------------

! prepare split candidates
      TAG_II_E2S(1:II_STOK_S) = 0
      TAG_II_E2E(1:II_STOK_E) = 0
      TAG_JJ_E2S(1:II_STOK_S) = 0
      TAG_JJ_E2E(1:II_STOK_E) = 0

      ID = 1
      NB_FREE_EDGES = 0
C FREE EDGES
      DO I=1, NEDGE
        E1=INTBUF_TAB%LEDGE(1+(I-1)*NLEDGE)
        K1=SEGLOC(E1)
        E2=INTBUF_TAB%LEDGE(3+(I-1)*NLEDGE)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 > 0 .AND. K2 ==  -1) THEN
          NB_FREE_EDGES = NB_FREE_EDGES + 1
! Internal edge 
          TAG_EDGE(ID) = I
          ID = ID + 1
        ENDIF
      ENDDO

C INTERNAL EDGES 
      NB_INTERNAL_EDGES = 0
      DO I=1, NEDGE
        E1=INTBUF_TAB%LEDGE(1+(I-1)*NLEDGE)
        K1=SEGLOC(E1)
        E2=INTBUF_TAB%LEDGE(3+(I-1)*NLEDGE)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 > 0 .AND. K2 > 0) THEN
          NB_INTERNAL_EDGES = NB_INTERNAL_EDGES + 1
! Internal edge 
          TAG_EDGE(ID) = I
          ID = ID + 1
        ENDIF
      ENDDO

      NB_BOUNDARY_EDGES_LOCAL = 0
      DO I=1, NEDGE
        E1=INTBUF_TAB%LEDGE(1+(I-1)*NLEDGE)
        K1=SEGLOC(E1)
        E2=INTBUF_TAB%LEDGE(3+(I-1)*NLEDGE)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 > 0 .AND. K2 == 0) THEN
          NB_BOUNDARY_EDGES_LOCAL = NB_BOUNDARY_EDGES_LOCAL + 1
          TAG_EDGE(ID) = I
          ID = ID + 1
        ENDIF
      ENDDO

      NB_BOUNDARY_EDGES_REMOTE = 0
      DO I=1, NEDGE
        E1=INTBUF_TAB%LEDGE(1+(I-1)*NLEDGE)
        K1=SEGLOC(E1)
        E2=INTBUF_TAB%LEDGE(3+(I-1)*NLEDGE)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 ==  0 .AND. K2 > 0) THEN
          NB_BOUNDARY_EDGES_REMOTE = NB_BOUNDARY_EDGES_REMOTE + 1
          TAG_EDGE(ID) = I
          ID = ID + 1
        ENDIF
      ENDDO


      I = NB_FREE_EDGES + NB_INTERNAL_EDGES + NB_BOUNDARY_EDGES_REMOTE+NB_BOUNDARY_EDGES_LOCAL 
      ASSERT(I == NEDGE_L)
      ID = 1
      TAG_EDGE2(1:NEDGE) = 0
      DO I=1, NEDGE_L
        TAG_EDGE2(TAG_EDGE(I)) = I
C       TAG_EDGE2(global id ) = local id 
      ENDDO

C------IREMI2 OPTION : deleting couples already defined in tied contact-------
      
      ALLOCATE(TAGREMCAND_E2E(II_STOK_E))

      TAGREMCAND_E2E(1:II_STOK_E) = 0
      IF(IREMI2==1.AND.FLAGREMNODE==2) THEN
         !  connectivity edge > contact pairs  

         ALLOCATE(KCANDMS(NEDGE+1))
         ALLOCATE(ICANDMS(NEDGE+1))
         ALLOCATE(CANDMS(II_STOK_E))
         KCANDMS(1:NEDGE+1) = 0
         ICANDMS(1:NEDGE+1) = 0
         CANDMS(1:II_STOK_E) = 0

         DO I = 1, II_STOK_E
            E1 = INTBUF_TAB%CANDM_E2E(I)
            KCANDMS(E1) =KCANDMS(E1)+1               
         ENDDO
         ICANDMS(1) = 1
         DO I=1,NEDGE
            ICANDMS(I+1) =  ICANDMS(I) +KCANDMS(I)
         ENDDO
         KCANDMS(1:NEDGE+1) = ICANDMS(1:NEDGE+1)
 
         DO I=1,II_STOK_E
            E1 = INTBUF_TAB%CANDM_E2E(I)
            CANDMS(KCANDMS(E1)) = I
            KCANDMS(E1) = KCANDMS(E1) + 1
         END DO

         DO I=1,NEDGE
            K = INTBUF_TAB%KREMNODE_EDG(I)
            L = INTBUF_TAB%KREMNODE_EDG(I+1)-1
            DO J=ICANDMS(I),ICANDMS(I+1)-1                     
               DO M=K,L
                  IF(INTBUF_TAB%CANDS_E2E(CANDMS(J))== INTBUF_TAB%REMNODE_EDG(M)) 
     .                TAGREMCAND_E2E(CANDMS(J)) = 1 
               ENDDO
            ENDDO
         ENDDO
         DEALLOCATE(KCANDMS,ICANDMS,CANDMS)
      ENDIF
     
      II_STOK_E_L = 0
      JJ= 0
C local numbering of E2E candidates
      DO I = 1, II_STOK_E
         E1 =INTBUF_TAB%CANDM_E2E(I)
         E2 =INTBUF_TAB%CANDS_E2E(I)
        IF(TAG_EDGE2( INTBUF_TAB%CANDM_E2E(I)) > 0) THEN                 
         ! master edge is on this processor
          ID = INTBUF_TAB%CANDM_E2E(I)
          IF( INTBUF_TAB%LEDGE(9+(ID-1)*NLEDGE) == PROC ) THEN
             JJ= JJ + 1
             TAG_JJ_E2E(I) = JJ 
             IF(TAGREMCAND_E2E(I)==0) THEN  ! cand deleted IremI2
C        master edge is owned by this processor
C         master edge is local
C         Slave is local
                II_STOK_E_L = II_STOK_E_L + 1
                TAG_II_E2E(II_STOK_E_L) = I 
             ENDIF
          ENDIF
        ENDIF
      ENDDO


C------IREMI2 OPTION : deleting couples already defined in tied contact-------
     
      ALLOCATE(TAGREMCAND_E2S(II_STOK_S))

      TAGREMCAND_E2S(1:II_STOK_S) = 0
      IF(IREMI2==1.AND.FLAGREMNODE==2.AND.II_STOK_S > 0) THEN

         !  connectivity NRTM > contact pairs  

         ALLOCATE(KCANDMS(NRTM+1))
         ALLOCATE(ICANDMS(NRTM+1))
         ALLOCATE(CANDMS(II_STOK_S))
         KCANDMS(1:NRTM+1) = 0
         ICANDMS(1:NRTM+1) = 0
         CANDMS(1:II_STOK_S) = 0

         DO I = 1, II_STOK_S
            E1 = INTBUF_TAB%CANDM_E2S(I)
            KCANDMS(E1) =KCANDMS(E1)+1               
         ENDDO
         ICANDMS(1) = 1
         DO I=1,NRTM
            ICANDMS(I+1) =  ICANDMS(I) +KCANDMS(I)
        ENDDO
        KCANDMS(1:NRTM+1) = ICANDMS(1:NRTM+1)
 
        DO I=1,II_STOK_S
           E1 = INTBUF_TAB%CANDM_E2S(I)
           CANDMS(KCANDMS(E1)) = I
           KCANDMS(E1) = KCANDMS(E1) + 1
        END DO

        DO I=1,NRTM
            K = INTBUF_TAB%KREMNODE_E2S(I)
            L = INTBUF_TAB%KREMNODE_E2S(I+1)-1
            DO J=ICANDMS(I),ICANDMS(I+1)-1                     
               DO M=K,L
                  IF(INTBUF_TAB%CANDS_E2S(CANDMS(J))== INTBUF_TAB%REMNODE_E2S(M)) 
     .                TAGREMCAND_E2S(CANDMS(J)) = 1 
               ENDDO
            ENDDO
         ENDDO

         DEALLOCATE(KCANDMS,ICANDMS,CANDMS)

      ENDIF

      II_STOK_S_L = 0
      JJ= 0
      DO I = 1, II_STOK_S
        IF(SEGLOC( INTBUF_TAB%CANDM_E2S(I)) > 0) THEN
          JJ= JJ + 1
          TAG_JJ_E2S(I) = JJ      
          IF(TAGREMCAND_E2S(I)==0) THEN ! cand deleted IremI2
C            master sgmt is local
             II_STOK_S_L = II_STOK_S_L + 1
             TAG_II_E2S(II_STOK_S_L) = I 
          ENDIF
        ENDIF
      ENDDO

      DEALLOCATE(TAGREMCAND_E2E,TAGREMCAND_E2S)
      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_I25_EDGE           source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        I25_FIE_MOD                   share/modules1/i25_fie_mod.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
            SUBROUTINE SPLIT_CAND_I25_EDGE(INTBUF_TAB,SEGLOC,PROC,NIN,
     .                             TAG_EDGE, NEDGE_L, TAG_EDGE2, NEDGE,
     .                             II_STOK_E, II_STOK_E_L, TAG_II_E2E,
     .                             II_STOK_S, II_STOK_S_L, TAG_II_E2S,
     .                             TAG_JJ_E2E,TAG_JJ_E2S )

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE I25_FIE_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: PROC,NIN
      INTEGER :: NEDGE,NEDGE_L     ! number of edges
      INTEGER :: TAG_EDGE(NEDGE_L), TAG_EDGE2(NEDGE)
      INTEGER :: SEGLOC(*) ! global to local id of segment
      INTEGER :: TAG_II_E2E(*) !local id of global E2E cand (OUT)
      INTEGER :: TAG_II_E2S(*) !local id of global E2S cand (OUT)
      INTEGER :: II_STOK_E, II_STOK_E_L 
      INTEGER :: II_STOK_S, II_STOK_S_L
      INTEGER :: TAG_JJ_E2E(II_STOK_E) !Global id of global E2E cand (OUT)
      INTEGER :: TAG_JJ_E2S(II_STOK_S) !Globa id of global E2S cand (OUT)


      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,JJ 
      INTEGER :: ID,E1,E2,K1,K2
      INTEGER, DIMENSION(:), ALLOCATABLE :: CANDM_E2E,CANDS_E2E
      INTEGER, DIMENSION(:), ALLOCATABLE :: CANDM_E2S,CANDS_E2S

C ----------------------------------------

! prepare split candidates
      ALLOCATE(CANDM_E2E(II_STOK_E_L)) 
      ALLOCATE(CANDS_E2E(II_STOK_E_L)) 
      ASSERT(II_STOK_E_L==I25_SPLIT_CAND(NIN,PROC+1)%NB_CAND_E2E)
      ASSERT(II_STOK_S_L==I25_SPLIT_CAND(NIN,PROC+1)%NB_CAND_E2S)

C local numbering of E2E candidates
      ID = 0
      DO J = 1, II_STOK_E_L
        I = TAG_II_E2E(J)
        JJ = TAG_JJ_E2E(I)
        ASSERT(I > 0) 
C       main edge is local
        ID = ID + 1 
        CANDM_E2E(ID) = TAG_EDGE2(INTBUF_TAB%CANDM_E2E(I)) 
        IF(INTBUF_TAB%LEDGE(9+(INTBUF_TAB%CANDS_E2E(I)-1)*NLEDGE) == PROC ) THEN
C       Secnd is also local
          CANDS_E2E(ID) = TAG_EDGE2(INTBUF_TAB%CANDS_E2E(I))
        ELSE
C       Remote secnd
C         ASSERT(.FALSE.)
C         CANDS_E2E(ID) = -INTBUF_TAB%CANDS_E2E(I)
          CANDS_E2E(ID) = ABS(I25_SPLIT_CAND(NIN,PROC+1)%CANDS_E2E(JJ)) + NEDGE_L

        ENDIF
      ENDDO

      ALLOCATE(CANDM_E2S(II_STOK_S_L))
      ALLOCATE(CANDS_E2S(II_STOK_S_L))
C local numbering of E2S candidates
      ID = 0
      ASSERT(II_STOK_S_L == I25_SPLIT_CAND(NIN,PROC+1)%NB_CAND_E2S)
C     WRITE(6,*) PROC,"NBCAND=",II_STOK_S_L,I25_SPLIT_CAND(NIN,PROC+1)%NB_CAND_E2S

      DO J = 1, II_STOK_S_L
        I = TAG_II_E2S(J)
        JJ = TAG_JJ_E2S(I)
C       main edge is local
        ASSERT(I > 0) 
        ID = ID + 1 
        CANDM_E2S(ID) = SEGLOC(INTBUF_TAB%CANDM_E2S(I)) 
C       IF(TAG_EDGE2(INTBUF_TAB%CANDS_E2S(I)) > 0) THEN
        IF(INTBUF_TAB%LEDGE(9+(INTBUF_TAB%CANDS_E2S(I)-1)*NLEDGE) == PROC ) THEN
C       Secnd is also local
          CANDS_E2S(ID) = TAG_EDGE2(INTBUF_TAB%CANDS_E2S(I))
          ASSERT(CANDS_E2S(ID) == I25_SPLIT_CAND(NIN,PROC+1)%CANDS_E2S(JJ))
        ELSE
C       Remote secnd
C         ASSERT(.FALSE.)
          CANDS_E2S(ID) = ABS(I25_SPLIT_CAND(NIN,PROC+1)%CANDS_E2S(JJ)) + NEDGE_L
C         CANDS_E2S(ID) = - INTBUF_TAB%CANDS_E2S(I)
        ENDIF
      ENDDO

      CALL WRITE_I_C(CANDM_E2E,II_STOK_E_L)
      CALL WRITE_I_C(CANDS_E2E,II_STOK_E_L)
      CALL WRITE_I_C(CANDM_E2S,II_STOK_S_L)
      CALL WRITE_I_C(CANDS_E2S,II_STOK_S_L)
      DEALLOCATE(CANDM_E2E)
      DEALLOCATE(CANDS_E2E)
      DEALLOCATE(CANDM_E2S)
      DEALLOCATE(CANDS_E2S)

      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_REMNODE_I25             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_REMNODE_I25(PROC     , INTBUF_TAB, NRTM    , NRTM_L ,
     .                            TAG_SEGM2 , NREMNODE  , NODLOCAL ,NREMNOR,
     .                            NSN       , NSN_L     ,TAG_NODE_2RY2,ITAB,
     .                             NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
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
      INTEGER PROC ,NRTM ,NRTM_L ,NSN ,NSN_L ,NREMNOR ,
     .        TAG_SEGM2(*) ,NREMNODE ,NODLOCAL(*) ,
     .        TAG_NODE_2RY2(*),ITAB(*)
      INTEGER, INTENT(IN) :: NUMNOD_L

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SIZ,LL,
     .        L,SIZ1 ,SIZ2 ,M ,N ,NS

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF1,IBUF2,NODDEL,NODDELREMOTE
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .        SIZ_TMP
C ----------------------------------------
      ALLOCATE(SIZ_TMP(NRTM),NODDEL(NUMNOD),
     .         NODDELREMOTE(NUMNOD))

      ALLOCATE(IBUF1(2*(NRTM_L + 1)), IBUF2(NREMNODE))
      IBUF1(1:2*(NRTM_L+1)) = 0
      IBUF2(1:NREMNODE) = 0

      SIZ_TMP(1:NRTM) = 0

      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN
          SIZ_TMP(TAG_SEGM2(K)) = INTBUF_TAB%KREMNODE(K+1)
     .              -INTBUF_TAB%KREMNODE(K)
        ENDIF
      END DO

      IBUF1(1) = 0

      NODDEL(1:NUMNOD) = 0
      NODDELREMOTE(1:NUMNOD) = 0
      SIZ1 = 0
      SIZ2 = 0
      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN

          SIZ = SIZ_TMP(TAG_SEGM2(K))
          IBUF1(1+2*TAG_SEGM2(K)) =IBUF1(1+2*(TAG_SEGM2(K)-1)) + SIZ

          L=INTBUF_TAB%KREMNODE(K)
          SIZ1 = 0
          SIZ2 = 0
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M)
            IF( NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L ) THEN
              NODDEL(SIZ1+1) = NODLOCAL(N)
              SIZ1 = SIZ1+1
            ENDIF
          ENDDO
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M)
            IF( NODLOCAL (N)==0.OR.NODLOCAL(N)>NUMNOD_L ) THEN
              NODDELREMOTE(SIZ2+1) = ITAB(N)
              SIZ2 = SIZ2+1
            ENDIF
          ENDDO
          L=IBUF1(1+2*(TAG_SEGM2(K)-1))
          DO M=1,SIZ1
            IBUF2(1+L+M-1)= NODDEL(M)
          ENDDO
          IBUF1(1+2*(TAG_SEGM2(K)-1)+1) = L + SIZ1
          L=IBUF1(1+2*(TAG_SEGM2(K)-1)+1)
          DO M=1,SIZ2
            IBUF2(1+L+M-1) = - NODDELREMOTE(M)
          ENDDO
        ENDIF
        DO M=1,SIZ1
          NODDEL(M) = 0
        ENDDO
        DO M=1,SIZ2
          NODDELREMOTE(M) = 0
        ENDDO
      ENDDO

      DEALLOCATE(SIZ_TMP,NODDEL,NODDELREMOTE)

      CALL WRITE_I_C(IBUF1,2*(NRTM_L + 1))
      CALL WRITE_I_C(IBUF2,NREMNODE)

      DEALLOCATE(IBUF1, IBUF2)

C----Tab Main segment removed for each secnd node----
c   1st : reorganizing the tab : keep only local main segments

         ALLOCATE(IBUF1(NSN_L+1),IBUF2(NREMNOR))

         ALLOCATE(NODDEL(NRTM))

         IBUF1(1:NSN_L+1) = 0
         IBUF2(1:NREMNOR) = 0

        DO N=1,NSN

           NS = TAG_NODE_2RY2(N)
           IF(NS /= 0)THEN
              SIZ = INTBUF_TAB%KREMNOR(N+1)-INTBUF_TAB%KREMNOR(N)

              L=INTBUF_TAB%KREMNOR(N)
              SIZ1 = 0
c
             DO M=1,SIZ
                I = INTBUF_TAB%REMNOR(L+M)
                IF(TAG_SEGM2(I)/=0) THEN
                   NODDEL(SIZ1+1) = TAG_SEGM2(I)
                   SIZ1 = SIZ1+1
                ENDIF
             ENDDO
c
             L=IBUF1(NS)
             DO M=1,SIZ1
                IBUF2(L+M)= NODDEL(M)
             ENDDO
             IBUF1(NS+1) = L +SIZ1

             DO M=1,SIZ1
               NODDEL(M) = 0
             ENDDO

           ENDIF
         ENDDO

         DEALLOCATE(NODDEL)

         CALL WRITE_I_C(IBUF1,NSN_L+1)
         CALL WRITE_I_C(IBUF2,NREMNOR)
 
         DEALLOCATE(IBUF1, IBUF2)


      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_NISUB_I7                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NISUB_I7(INTBUF_TAB, NSN_L , TAG_NODE_2RY, NRTM_L,
     .                          TAG_SEGM  , NISUBS, NISUBM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSN_L,NRTM_L,NISUBS,NISUBM,
     .         TAG_NODE_2RY(*),TAG_SEGM(*)

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NISUBS_L,NISUBM_L

      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF1 !KD(29)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF2 !KD(30)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF3 !KD(31)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF4 !KD(32)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF5 
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF6 
C ----------------------------------------
      ALLOCATE(IBUF1(NSN_L+1))
      ALLOCATE(IBUF2(NRTM_L+1))
      ALLOCATE(IBUF3(NISUBS))
      ALLOCATE(IBUF4(NISUBM))
      ALLOCATE(IBUF5(NISUBS))
      ALLOCATE(IBUF6(NISUBM))


      IBUF1(1: NSN_L+1) = 0
      IBUF2(1: NRTM_L+1) = 0
      IBUF3(1: NISUBS) = 0
      IBUF4(1: NISUBM) = 0
      IBUF5(1: NISUBS) = 0
      IBUF6(1: NISUBM) = 0

      NISUBS_L = 0

      DO K=1,NSN_L
        IBUF1(K) = NISUBS_L + 1       
        J=TAG_NODE_2RY(K)
        DO I = INTBUF_TAB%ADDSUBS(J),INTBUF_TAB%ADDSUBS(J+1)-1
          IBUF3(1+NISUBS_L) =  INTBUF_TAB%LISUBS(I)
          IF(INTBUF_TAB%S_INFLG_SUBS > 0) IBUF5(1+NISUBS_L) =  INTBUF_TAB%INFLG_SUBS(I)
          NISUBS_L = NISUBS_L + 1
        END DO
      END DO

      IBUF1(NSN_L+1) = NISUBS_L + 1
C
      NISUBM_L = 0
      DO K=1,NRTM_L
        IBUF2(K) = NISUBM_L + 1
        J=TAG_SEGM(K)
        DO I = INTBUF_TAB%ADDSUBM(J), 
     .                 INTBUF_TAB%ADDSUBM(J+1)-1
          IBUF4(1+NISUBM_L) = INTBUF_TAB%LISUBM(I)
          IF(INTBUF_TAB%S_INFLG_SUBM > 0) IBUF6(1+NISUBM_L) = INTBUF_TAB%INFLG_SUBM(I)
          NISUBM_L = NISUBM_L + 1
        END DO
      END DO

      IBUF2(NRTM_L+1) = NISUBM_L + 1

      CALL WRITE_I_C(IBUF1,NSN_L+1)
      CALL WRITE_I_C(IBUF2,NRTM_L+1)
      CALL WRITE_I_C(IBUF3,NISUBS)
      CALL WRITE_I_C(IBUF4,NISUBM)
      IF(INTBUF_TAB%S_INFLG_SUBS > 0) CALL WRITE_I_C(IBUF5,NISUBS) !INFLG_SUBS
      IF(INTBUF_TAB%S_INFLG_SUBM > 0) CALL WRITE_I_C(IBUF6,NISUBM) !INFLG_SUBM

      DEALLOCATE(IBUF1,IBUF2,IBUF3,IBUF4,IBUF5,IBUF6)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_NISUB_I25               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NISUB_I25(INTBUF_TAB, NSN_L , TAG_NODE_2RY, NRTM_L,
     1                          TAG_SEGM  , NISUBS, NISUBM ,
     2       IEDGE,
     3       NEDGE,
     4       NEDGE_L,
     5       TAG_EDGE,
     6       TAG_EDGE2,
     7       NISUBE,
     9       PROC)

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER  NSN_L,NRTM_L,NISUBS,NISUBM,
     .         TAG_NODE_2RY(*),TAG_SEGM(*)
      INTEGER, INTENT(IN) :: IEDGE
      INTEGER, INTENT(IN) :: NEDGE
      INTEGER, INTENT(IN) :: NEDGE_L
      INTEGER, INTENT(IN) :: TAG_EDGE(NEDGE_L) ! Local To global
      INTEGER, INTENT(IN) :: TAG_EDGE2(NEDGE) ! Global to local
      INTEGER, INTENT(IN) :: NISUBE !local number  
      INTEGER, INTENT(IN) :: PROC !local number  




      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NISUBS_L,NISUBM_L,NISUBE_L

      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF1 
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF2 
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF3 
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF4 
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF5 
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF6 
C ----------------------------------------
      ALLOCATE(IBUF1(NSN_L+1))
      ALLOCATE(IBUF2(NRTM_L+1))
      ALLOCATE(IBUF3(NISUBS))
      ALLOCATE(IBUF4(NISUBM))
      ALLOCATE(IBUF5(NISUBS))
      ALLOCATE(IBUF6(NISUBM))

      IBUF1(1: NSN_L+1) = 0
      IBUF2(1: NRTM_L+1) = 0
      IBUF3(1: NISUBS) = 0
      IBUF4(1: NISUBM) = 0
      IBUF5(1: NISUBS) = 0
      IBUF6(1: NISUBM) = 0

      NISUBS_L = 0

      DO K=1,NSN_L
        IBUF1(K) = NISUBS_L + 1       
        J=TAG_NODE_2RY(K)
        DO I = INTBUF_TAB%ADDSUBS(J),INTBUF_TAB%ADDSUBS(J+1)-1
          IBUF3(1+NISUBS_L) =  INTBUF_TAB%LISUBS(I)
          IBUF5(1+NISUBS_L) =  INTBUF_TAB%INFLG_SUBS(I)
          NISUBS_L = NISUBS_L + 1
        END DO
      END DO

      IBUF1(NSN_L+1) = NISUBS_L + 1
C
      NISUBM_L = 0
      DO K=1,NRTM_L
        IBUF2(K) = NISUBM_L + 1
        J=TAG_SEGM(K)
        DO I = INTBUF_TAB%ADDSUBM(J), 
     .                 INTBUF_TAB%ADDSUBM(J+1)-1
          IBUF4(1+NISUBM_L) = INTBUF_TAB%LISUBM(I)
          IBUF6(1+NISUBM_L) = INTBUF_TAB%INFLG_SUBM(I)
          NISUBM_L = NISUBM_L + 1
        END DO
      END DO

      IBUF2(NRTM_L+1) = NISUBM_L + 1

      CALL WRITE_I_C(IBUF1,NSN_L+1) !ADDSUBS
      CALL WRITE_I_C(IBUF2,NRTM_L+1) ! ADDSUBM
      CALL WRITE_I_C(IBUF3,NISUBS) !LISUBSS
      CALL WRITE_I_C(IBUF4,NISUBM) !LISUBMI
      CALL WRITE_I_C(IBUF5,NISUBS) !INFLG_SUBS
      CALL WRITE_I_C(IBUF6,NISUBM) !INFLG_SUBM

      DEALLOCATE(IBUF1,IBUF2,IBUF3,IBUF4,IBUF5,IBUF6)

      IF(IEDGE/=0)THEN ! FAIRE SPMD
C       CALL COPY_IVAL(INTBUF_TAB(NI)%ADDSUBE,NEDGE+1,1)
C       CALL COPY_IVAL(INTBUF_TAB(NI)%LISUBE,NISUBE_L,1) 
C       CALL COPY_IVAL(INTBUF_TAB(NI)%INFLG_SUBE,NISUBE_L,1) 

        ALLOCATE(IBUF1(NEDGE_L+1))
        ALLOCATE(IBUF3(NISUBE))
        ALLOCATE(IBUF5(NISUBE))
       
        IBUF1(1: NEDGE_L+1) = 0
        IBUF3(1: NISUBE) = 0
        IBUF5(1: NISUBE) = 0
       
        NISUBE_L = 0
       
        DO K=1,NEDGE_L
          IBUF1(K) = NISUBE_L + 1       
          J=TAG_EDGE(K)
          IF(INTBUF_TAB%LEDGE(NLEDGE*(J-1)+9) == PROC ) THEN ! IF current proc own the edge
            DO I = INTBUF_TAB%ADDSUBE(J),INTBUF_TAB%ADDSUBE(J+1)-1
              IBUF3(1+NISUBE_L) =  INTBUF_TAB%LISUBE(I)
              IBUF5(1+NISUBE_L) =  INTBUF_TAB%INFLG_SUBE(I)
              NISUBE_L = NISUBE_L + 1
            END DO
          ENDIF
        END DO
       
        IBUF1(NEDGE_L+1) = NISUBE_L + 1
        CALL WRITE_I_C(IBUF1,NEDGE_L+1) !ADDSUBE
        CALL WRITE_I_C(IBUF3,NISUBE) !LISUBES
        CALL WRITE_I_C(IBUF5,NISUBE) !INFLG_SUBE
        DEALLOCATE(IBUF1)
        DEALLOCATE(IBUF3)
        DEALLOCATE(IBUF5)
        ASSERT(NISUBE == NISUBE_L)


      END IF


      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_XSAV                    source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_XSAV(INTBUF_TAB, NUMNOD_L, NSN, NSN_L,
     .                      NMN       , NMN_L , TAG_SCRATCH, TAG_NODE_MSR,
     .                      TAG_NM    , NODLOCAL, PROC     , NI ,I710XSAV,
     .                      NINDX_SCRT, INDX_SCRT)

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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD_L,NSN,NSN_L,NMN,
     .         NMN_L, NOD, PROC, NI
      INTEGER  TAG_SCRATCH(*), TAG_NODE_MSR(*),
     .         TAG_NM(*), NODLOCAL(*)
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_SCRT

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      INTEGER I710XSAV(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,N2,NSN_L2,NMN_L2,
     .        SIZ_XSAV,TAG

      my_real, DIMENSION(:),ALLOCATABLE :: RBUF
C ----------------------------------------

      SIZ_XSAV = 3*MIN(NUMNOD_L,NSN_L+NMN_L)
      ALLOCATE(RBUF(SIZ_XSAV))
      RBUF(1:SIZ_XSAV) = ZERO

C XSAVE
C NSN+NMN < NUMNOD
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
      IF (NSN+NMN<=NUMNOD)THEN
C NSN_L + NMN_L < NUMNOD_L
       IF (NSN_L+NMN_L<=NUMNOD_L)THEN
C 1ere partie NSN
        NSN_L2 = 0
!        TAG_SCRATCH(1:NUMNOD)=0
        DO K=1,NSN
          N=INTBUF_TAB%NSV(K)
          IF(NLOCAL(N,PROC+1)==1.AND.
     .      TAG_SCRATCH(N)==0) THEN
            RBUF(3*(NSN_L2)+1) =
     *                                   INTBUF_TAB%XSAV(3*(K-1)+1)
            RBUF(3*(NSN_L2)+2) =
     *                                   INTBUF_TAB%XSAV(3*(K-1)+2)
            RBUF(3*(NSN_L2)+3) =
     *                                   INTBUF_TAB%XSAV(3*(K-1)+3)
            NSN_L2 = NSN_L2 + 1
            TAG_SCRATCH(N)=1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
        ENDDO

C 2eme partie NMN
        DO K=1,NMN
          N = INTBUF_TAB%MSR(K)
          IF(NLOCAL(N,PROC+1)==1)THEN
            NOD = NODLOCAL(N)
            DO  L=1,NMN
              IF (I710XSAV(L)==NOD)THEN
                RBUF(3*NSN_L+3*(L-1)+1)=
     *                        INTBUF_TAB%XSAV(3*NSN+3*(K-1)+1)
                RBUF(3*NSN_L+3*(L-1)+2)=
     *                        INTBUF_TAB%XSAV(3*NSN+3*(K-1)+2)
                RBUF(3*NSN_L+3*(L-1)+3)=
     *                        INTBUF_TAB%XSAV(3*NSN+3*(K-1)+3)
                GOTO 600
              ENDIF
            ENDDO
          ENDIF
 600      CONTINUE
        ENDDO

       ELSE
C NSN_L + NMN_L > NUMNOD_L
C 1ere partie NSN
        NSN_L2 = 0
!        TAG_SCRATCH(1:NUMNOD)=0
        DO K=1,NSN
          N = INTBUF_TAB%NSV(K)
          IF(NLOCAL(N,PROC+1)==1.AND.
     .      TAG_SCRATCH(N)==0) THEN
            N2 = NODLOCAL(N)
            RBUF(3*(N2-1)+1) =
     *                                   INTBUF_TAB%XSAV(3*(K-1)+1)
            RBUF(3*(N2-1)+2) =
     *                                   INTBUF_TAB%XSAV(3*(K-1)+2)
            RBUF(3*(N2-1)+3) =
     *                                   INTBUF_TAB%XSAV(3*(K-1)+3)
            NSN_L2 = NSN_L2 + 1
            TAG_SCRATCH(N)=1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
        ENDDO

C 2eme partie NMN
        NMN_L2 = 0
        DO K=1,NMN
          N=INTBUF_TAB%MSR(K)
          IF(NLOCAL(N,PROC+1)==1)THEN
            N2 = NODLOCAL(N)
            IF (TAG_NM(N)==1)THEN
              RBUF(3*(N2-1)+1) =
     *                        INTBUF_TAB%XSAV(3*NSN+3*(K-1)+1)
              RBUF(3*(N2-1)+2) =
     *                        INTBUF_TAB%XSAV(3*NSN+3*(K-1)+2)
              RBUF(3*(N2-1)+3) =
     *                        INTBUF_TAB%XSAV(3*NSN+3*(K-1)+3)
              NMN_L2 = NMN_L2 + 1
            ENDIF
          ENDIF
        ENDDO
       ENDIF

      ELSE
C NSN+NMN > NUMNOD
       IF(NSN_L+ NMN_L < NUMNOD_L)THEN
C NSN_L+NMN_L < NUMNOD_L
        NSN_L2 = 0
        DO K=1,NSN
          N=INTBUF_TAB%NSV(K)
          IF(NLOCAL(N,PROC+1)==1) THEN
            N2=NODLOCAL(N)
            RBUF(3*NSN_L2+1) =
     *                                     INTBUF_TAB%XSAV(3*(N-1)+1)
            RBUF(3*NSN_L2+2) =
     *                                     INTBUF_TAB%XSAV(3*(N-1)+2)
            RBUF(3*NSN_L2+3) =
     *                                     INTBUF_TAB%XSAV(3*(N-1)+3)
            NSN_L2 = NSN_L2 + 1
            TAG_SCRATCH(N)=1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
         ENDDO

         DO K=1,NMN
            N=INTBUF_TAB%MSR(K)
            IF(NLOCAL(N,PROC+1)==1)THEN
              NOD = NODLOCAL(N)
              DO  L=1,NMN
c                IF (I710SAV(NI)%P(L)==NOD)THEN
                TAG = TAG_NODE_MSR(L)
                IF (INTBUF_TAB%MSR(TAG)==NOD)THEN
                  RBUF(3*NSN_L2+3*(L-1)) =
     *                                     INTBUF_TAB%XSAV(3*(N-1)+1)
                  RBUF(3*NSN_L2+3*(L-1)+1) =
     *                                     INTBUF_TAB%XSAV(3*(N-1)+2)
                  RBUF(3*NSN_L2+3*(L-1)+2) =
     *                                     INTBUF_TAB%XSAV(3*(N-1)+3)
                  GOTO 610
                ENDIF
              ENDDO
           ENDIF
 610      CONTINUE
        ENDDO

C NSN_L+NMN_L > NUMNOD_L
       ELSE
        NSN_L2 = 0
        DO K=1,NSN
          N=INTBUF_TAB%NSV(K)
          IF(NLOCAL(N,PROC+1)==1) THEN
            N2=NODLOCAL(N)
            RBUF(3*(N2-1)+1)   = INTBUF_TAB%XSAV(3*(N-1)+1)
            RBUF(3*(N2-1)+2)   = INTBUF_TAB%XSAV(3*(N-1)+2)
            RBUF(3*(N2-1)+3)   = INTBUF_TAB%XSAV(3*(N-1)+3)
            NSN_L2 = NSN_L2 + 1
            TAG_SCRATCH(N)=1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
         ENDDO

         DO K=1,NMN
           N=INTBUF_TAB%MSR(K)
           IF(NLOCAL(N,PROC+1)==1)THEN
             IF (TAG_NM(N)==1)THEN
               N2=NODLOCAL(N)
               RBUF(3*(N2-1)+1)   =
     *                                    INTBUF_TAB%XSAV(3*(N-1)+1)
               RBUF(3*(N2-1)+2)   =
     *                                    INTBUF_TAB%XSAV(3*(N-1)+2)
               RBUF(3*(N2-1)+3)   =
     *                                    INTBUF_TAB%XSAV(3*(N-1)+3)
             ENDIF
           ENDIF
         ENDDO
       ENDIF
      ENDIF

      CALL WRITE_DB(RBUF,SIZ_XSAV)


      DEALLOCATE(RBUF)

      RETURN
      END

C=======================================================================
C END SPECIFIC ROUTINES INT7
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT8
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I8              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I8(PROC     ,   INTBUF_TAB  , IPARI       ,
     .                            INTERCEP ,   TAG_NODE_2RY, TAG_SEGM    ,
     .                            TAG_SEGM2,   TAG_NM      , TAG_NODE_MSR,
     .                            TAG_NODE_MSR2,TAG_LMSR,TAG_LMSR2, 
     .                            TAG_NSEG,TAG_NSEG2,
     .                            NI,T8,ITAB,NINDX_NM,INDX_NM)
C Computes the TAGS used to split the data
C Local renumbering of T8%BUFFER ID    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP
      TYPE(INT8_STRUCT_) :: T8
      INTEGER NI,PROC,IPARI(*),
     .        TAG_NODE_2RY(*),TAG_SEGM(*),TAG_NM(*),TAG_NODE_MSR(*),
     .        TAG_NODE_MSR2(*),TAG_LMSR(*),TAG_LMSR2(*),
     .        TAG_NSEG(*),TAG_NSEG2(*),
     .        TAG_SEGM2(*),ITAB(*)
      INTEGER, INTENT(INOUT) :: NINDX_NM
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_NM
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN,NMN_L,
     .        I,J,K,N,N1,N2,N3,N4,P2,
     .        CNSN_L,CNRTM_L,CNMN_L,CLMSR_L,
     .        IBEGIN,IEND
C ----------------------------------------
      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)

      CNSN_L = 0
      DO K=1, NSN
        N=INTBUF_TAB%ILOCS(K)
        N=INTBUF_TAB%MSR(N)
        IF(NLOCAL(N,PROC+1)==1) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(K) = 1
        ENDIF
      ENDDO

! prepare SPLIT_NRTM_R
      CNRTM_L = 0
      CNMN_L = 0
!      already done in split_interface 
!      TAG_NM(1:NUMNOD) = 0 
      DO K=1,NRTM
        N1 = INTBUF_TAB%IRECTM(4*(K-1)+1)
        N2 = INTBUF_TAB%IRECTM(4*(K-1)+2)
        N3 = INTBUF_TAB%IRECTM(4*(K-1)+3)
        N4 = INTBUF_TAB%IRECTM(4*(K-1)+4) 

        N1 = INTBUF_TAB%MSR(N1)
        N2 = INTBUF_TAB%MSR(N2)
        N3 = INTBUF_TAB%MSR(N3)
        N4 = INTBUF_TAB%MSR(N4) 

        IF(INTERCEP%P(K)==PROC+1)THEN
          CNRTM_L = CNRTM_L + 1 
          TAG_SEGM(CNRTM_L) = K
          TAG_SEGM2(K) = CNRTM_L
          IF(TAG_NM(N1)==0)THEN
            TAG_NM(N1)=1
            CNMN_L = CNMN_L +1
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N1
          ENDIF
          IF(TAG_NM(N2)==0)THEN
            TAG_NM(N2)=1
            CNMN_L = CNMN_L +1
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N2
          ENDIF
          IF(TAG_NM(N3)==0)THEN
            TAG_NM(N3)=1
            CNMN_L = CNMN_L +1
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N3
          ENDIF
          IF(TAG_NM(N4)==0)THEN
            TAG_NM(N4)=1
            CNMN_L = CNMN_L +1
            NINDX_NM = NINDX_NM + 1 
            INDX_NM(NINDX_NM) = N4
          ENDIF
        ENDIF
      ENDDO  

C     WRITE(6,*) __FILE__,__LINE__,CNMN_L,CNRTM_L,NRTM
c     IF(NI == 1) THEN
c     DO I=1,NMN
c       N = INTBUF_TAB%MSR(I)
c       !if the node is local
c       IF(TAG_NM(N)==1)THEN
c        WRITE(700+PROC,*) ITAB(N)
c       ENDIF
c     ENDDO
c     DO N=1,NUMNOD
c       !if the node is local
c       IF(TAG_NM(N)==1)THEN
c        WRITE(800+PROC,*) ITAB(N)
c       ENDIF
c     ENDDO
c     ENDIF

C Compute TAG_LMSR,TAG_LMSR2,TAG_NSEG 
      NMN_L = CNMN_L
      IF(NMN_L > 0) TAG_NSEG(1) = 1
      CNMN_L = 0 
      CLMSR_L = 0
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        !if the node is local
        IF(TAG_NM(N)==1)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
          TAG_NODE_MSR2(I) = CNMN_L
          IBEGIN = INTBUF_TAB%NSEGM(I)
          IEND = INTBUF_TAB%NSEGM(I+1)-1 
          DO J=IBEGIN,IEND
           K = INTBUF_TAB%LMSR(J)
           IF(INTERCEP%P(K)==PROC+1)THEN
              ! TAG filled with local number of seg
              TAG_NSEG(CNMN_L + 1) =  TAG_NSEG(CNMN_L + 1) + 1
              CLMSR_L = CLMSR_L + 1 
              TAG_LMSR(CLMSR_L) = J
              TAG_LMSR2(J) = CLMSR_L
           ENDIF
          ENDDO
        ENDIF
      ENDDO

      !Cumulate sum of TAG_SEGM 
      !It becomes the local version of NSEGM 
      DO J=2,CNMN_L+1
       TAG_NSEG(J) = TAG_NSEG(J) + TAG_NSEG(J-1) 
      ENDDO

      IF(NSPMD > 1) THEN
      ! renumber MAIN_ID locally 
       DO P2=1,NSPMD
          IF(P2/=PROC + 1) THEN
          DO I = 1,T8%BUFFER(P2)%NBMAIN
             T8%BUFFER(P2)%MAIN_ID(I) =
     .       TAG_NODE_MSR2(T8%BUFFER(P2)%MAIN_ID(I)) 
          ENDDO
          ENDIF
       ENDDO

       DO I = 1,T8%S_COMM
            T8%SPMD_COMM_PATTERN(I)%UID = ITAB(
     .      INTBUF_TAB%MSR(T8%SPMD_COMM_PATTERN(I)%NUMLOC)) 
            T8%SPMD_COMM_PATTERN(I)%NUMLOC = 
     .      TAG_NODE_MSR2(T8%SPMD_COMM_PATTERN(I)%NUMLOC) 
       ENDDO
      ENDIF




      RETURN
      END SUBROUTINE PREPARE_SPLIT_I8
C=======================================================================
C END SPECIFIC ROUTINES INT8
C=======================================================================


C=======================================================================
C SPECIFIC ROUTINES INT 9
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I9              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I9(PROC        , INTBUF_TAB  , IPARI       ,
     .                            TAG_NODE_2RY, TAG_NODE_MSR, TAG_SCRATCH ,
     .                            TAG_IELES   , TAG_IELEM    , 
     .                            CEP         , CEL ,NINDX_SCRT,INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB

      INTEGER PROC,IPARI(*),
     .        TAG_NODE_2RY(*),TAG_NODE_MSR(*),
     .        TAG_IELES(*)   ,TAG_IELEM(*),
     .        TAG_SCRATCH(*),CEP(*),CEL(*)
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_SCRT
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NRTS,NMN,
     .        I,J,K,N,IE,IE_LOC,PROC2,
     .        CNSN_L,CNMN_L,CNRTS_L,CNRTM_L
C ----------------------------------------
      NRTS = IPARI(3)
      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)

      CNSN_L = 0
      DO K=1, NSN
        N=INTBUF_TAB%NSV(K)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
          TAG_SCRATCH(N)=1
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
!      DO K=1, NSN
!        N=INTBUF_TAB%NSV(K)
!        TAG_SCRATCH(N) = 0
!      ENDDO
#include "vectorize.inc"
      DO K=1,NINDX_SCRT  
                N = INDX_SCRT(K)
                TAG_SCRATCH(N) = 0
      ENDDO
      NINDX_SCRT = 0

      CNMN_L = 0 
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
      DO K=1, NMN
        N=INTBUF_TAB%MSR(K)
        TAG_SCRATCH(N) = 0
      ENDDO

      !IELES
      CNRTS_L = 0 
      DO K = 1, NRTS
        IE = INTBUF_TAB%IELES(K)
        PROC2  = CEP(IE)
        IF(PROC2==PROC) THEN
          IE_LOC = CEL(IE)
        ELSE
          IE_LOC = -IE
        ENDIF
        CNRTS_L = CNRTS_L + 1
        TAG_IELES(CNRTS_L) = IE_LOC
      ENDDO

      !IELEM
      CNRTM_L = 0 
      DO K = 1, NRTM
        IE = INTBUF_TAB%IELEM(K)
        PROC2  = CEP(IE)
        IF(PROC2==PROC) THEN
          IE_LOC = CEL(IE)
        ELSE
          IE_LOC = -IE
        ENDIF
        CNRTM_L = CNRTM_L + 1
        TAG_IELEM(CNRTM_L) = IE_LOC
      ENDDO

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT9
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT 11
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I11             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I11(PROC        , INTBUF_TAB, IPARI       ,
     .                             TAG_NODE_2RY, TAG_SEGM  , TAG_SEGM2   ,
     .                             TAG_NM      , TAG_SEGS  , TAG_NODE_MSR,
     .                             TAG_SCRATCH , INTERCEP  , NI ,NINDX_NM,INDX_NM,
     .                             NINDX_SCRT  , INDX_SCRT  ,TAG_SEGS2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
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
      INTEGER 
     .        IPARI(*),NI

      INTEGER PROC,TAG_NODE_2RY(*),TAG_SEGM(*),TAG_SEGM2(*),
     .        TAG_NM(*),TAG_SEGS(*),TAG_NODE_MSR(*),TAG_SCRATCH(*),TAG_SEGS2(*)
      INTEGER, INTENT(INOUT) :: NINDX_NM,NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_NM,INDX_SCRT

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP(3,NINTER)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN,NRTS

      INTEGER 
     .        I,J,K,L,M,N,N1,N2,JJ,
     .        CNRTM_L,CNRTS_L,CNSN_L,CNMN_L
C ----------------------------------------
      NRTS = IPARI(3)
      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)

C IRECTS     
      CNRTS_L = 0
      DO K=1,NRTS
        IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN
          CNRTS_L = CNRTS_L + 1
          TAG_SEGS(CNRTS_L) = K
          TAG_SEGS2(K) = CNRTS_L
        ENDIF
      ENDDO

      CNSN_L = 0
      DO K=1, NSN
        N=INTBUF_TAB%NSV(K)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
          TAG_SCRATCH(N)=1
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
      DO K=1, NINDX_SCRT
                N=INDX_SCRT(K)
                TAG_SCRATCH(N) = 0
      ENDDO
      NINDX_SCRT = 0

      CNRTM_L = 0
      DO K=1,NRTM
        N1 = INTBUF_TAB%IRECTM(2*(K-1)+1)
        N2 = INTBUF_TAB%IRECTM(2*(K-1)+2)
        IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
          CNRTM_L = CNRTM_L + 1 
          TAG_SEGM(CNRTM_L) = K
          TAG_SEGM2(K) = CNRTM_L
          IF(TAG_NM(N1)==0)THEN
            TAG_NM(N1)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N1
          ENDIF
          IF(TAG_NM(N2)==0)THEN
            TAG_NM(N2)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N2
          ENDIF
        ENDIF
      ENDDO  

      CNMN_L = 0 
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        IF(TAG_NM(N)==1)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
        ENDIF
      ENDDO


      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_I11                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_I11(PROC       , INTBUF_TAB, NRTS   , NRTS_L ,
     .                          TAG_SEGM2  , TAG_SEGS  , II_STOK, MULTIMP,
     .                          NCONT      , NOINT     , INACTI ,
     .                          TAG_SCRATCH, INTERCEP  , NI     , IPARI_L,
     .                          II_STOK_L  ,NINDX_SCRT ,INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER PROC,NRTS,NRTS_L,II_STOK,MULTIMP,NCONT,
     .        NOINT,INACTI,NI,IPARI_L(NPARI,NINTER),
     .        TAG_SEGM2(*),TAG_SEGS(*),TAG_SCRATCH(*),II_STOK_L 
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_SCRT

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP(3,NINTER)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,N1,N2,P,E,MULTOK,MSGID,
     .        SPLIST,C_NRTSR
      INTEGER NUMP(NSPMD),WORK(70000)

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF_E,IBUF_N,NRTSLOCAL,CPULOCAL,CANDR,PLIST,
     .  INDEX,CANDS

      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI

C ----------------------------------------
      ALLOCATE(IBUF_E(MULTIMP*NCONT),IBUF_N(MULTIMP*NCONT))
      IBUF_E(1:MULTIMP*NCONT) = 0
      IBUF_N(1:MULTIMP*NCONT) = 0

      II_STOK_L = 0

      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN 
        IF(NRTS>0) THEN
          ALLOCATE(NRTSLOCAL(NRTS))
          ALLOCATE(CPULOCAL(NRTS))
          ALLOCATE(CANDR(NRTS))
        END IF

        NUMP(1:NSPMD) = 0

! optimize loop with PLIST tool
        ALLOCATE(PLIST(NSPMD)) 
        PLIST(1:NSPMD) = -1

        DO K=1,NRTS
          N1 = INTBUF_TAB%IRECTS(2*(K-1)+1)
          N2 = INTBUF_TAB%IRECTS(2*(K-1)+2)
          NRTSLOCAL(K) = 0
          IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN      
            NUMP(PROC+1) = NUMP(PROC+1) + 1
            NRTSLOCAL(K) = NUMP(PROC+1)
            CPULOCAL(K) = PROC+1
          ENDIF
        ENDDO
        DEALLOCATE(PLIST) 
C
C Reperage des candidats se trouvant sur des procs distants
C
        C_NRTSR = 0
        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            N = INTBUF_TAB%CAND_N(K)
            IF(TAG_SCRATCH(N)==0) THEN
              TAG_SCRATCH(N) = 1
              NINDX_SCRT = NINDX_SCRT + 1
              INDX_SCRT(NINDX_SCRT) = N
              IF(NLOCAL(INTBUF_TAB%NSV(N),PROC+1)/=1)THEN
                C_NRTSR = C_NRTSR + 1
                CANDR(C_NRTSR) = N
              END IF
            END IF
          ENDIF
        ENDDO

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1, NINDX_SCRT
!         E = INTBUF_TAB%CAND_E(K)
!          IF (TAG_SEGM2(E)/=0) THEN
            N = INDX_SCRT(K)
            TAG_SCRATCH(N) = 0
!          ENDIF
        ENDDO
        NINDX_SCRT = 0

C
C Tris des candidats remote par proc et par nsv local croissant
C
        IF(C_NRTSR>0) THEN
          ALLOCATE(INDEX(2*C_NRTSR))
          ALLOCATE(ITRI(2,C_NRTSR))
        END IF
        DO I = 1, C_NRTSR
          N = CANDR(I)
          ITRI(1,I) = CPULOCAL(N)
          ITRI(2,I) = NRTSLOCAL(N)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,C_NRTSR,2)
C
        DO I = 1, C_NRTSR
          INDEX(C_NRTSR+INDEX(I)) = I
        ENDDO
        DO I = 1, C_NRTSR
          INDEX(I)=INDEX(C_NRTSR+I)
        ENDDO
C
        II_STOK_L = 0

        C_NRTSR = 0
        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            II_STOK_L = II_STOK_L + 1
            IBUF_E(II_STOK_L)=TAG_SEGM2(E)
            L = INTBUF_TAB%CAND_N(K)
            N1 = INTBUF_TAB%IRECTS(2*(L-1)+1)
            N2 = INTBUF_TAB%IRECTS(2*(L-1)+2)
            IF(CPULOCAL(L) == (PROC+1))THEN
              IBUF_N(II_STOK_L) = NRTSLOCAL(L)
            ELSE
C noeud remote : numerotation pre calculee ci-dessus
              IF(TAG_SCRATCH(L)==0) THEN
                C_NRTSR =C_NRTSR + 1
                IBUF_N(II_STOK_L) = INDEX(C_NRTSR)+NRTS_L
                TAG_SCRATCH(L) = INDEX(C_NRTSR)+NRTS_L
                NINDX_SCRT = NINDX_SCRT + 1
                INDX_SCRT(NINDX_SCRT) = L
              ELSE
                IBUF_N(II_STOK_L) = TAG_SCRATCH(L)
              END IF
            END IF
          ENDIF
        ENDDO

     
        IF(NRTS>0) DEALLOCATE(NRTSLOCAL,CPULOCAL,CANDR)
        IF(C_NRTSR>0) DEALLOCATE(INDEX,ITRI)

        IF(INACTI==5.OR.INACTI==6.OR.INACTI==7)IPARI_L(24,NI)= C_NRTSR

      ENDIF !END INACTI=5,6,7

      CALL WRITE_I_C(IBUF_E,MULTIMP*NCONT)
      CALL WRITE_I_C(IBUF_N,MULTIMP*NCONT)

      DEALLOCATE(IBUF_E,IBUF_N)

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT11
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT 17
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I17             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I17(PROC        , INTBUF_TAB   , IPARI  ,
     .                            TAG_NODE_2RY , TAG_NODE_MSR , 
     .                            CEP          , CEL          , IGRBRIC ,
     .                            NSN_L        , NME_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
      USE GROUPDEF_MOD
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
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB

      INTEGER PROC,IPARI(*),
     .        TAG_NODE_2RY(*),TAG_NODE_MSR(*),
     .        CEP(*),CEL(*),
     .        NME_L,NSN_L

C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NRTS,NME,
     .        J,K,IE,
     .        IGE,IGN,NAD,EAD,NAS,
     .        CNME_L,CNSN_L

C ----------------------------------------
      NRTS = IPARI(4)
      NME  = IPARI(4)
      NSN  = IPARI(5)
      IGE  = IPARI(34)
      IGN  = IPARI(36)
!
      CNSN_L = 0
      DO K=1, NSN
        IE = IGRBRIC(IGN)%ENTITY(K)
C       IF(CEP(IE)==PROC) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
C       ENDIF
      ENDDO
      CNME_L = 0 
      DO K=1,NME
        IE = IGRBRIC(IGE)%ENTITY(K)
C       IF(CEP(IE)==PROC)THEN
          CNME_L = CNME_L + 1
          TAG_NODE_MSR(CNME_L) = K
C       ENDIF
      ENDDO

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT17
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT20
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I20             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I20(
     .              PROC        , INTBUF_TAB, IPARI    ,
     .              TAG_NODE_2RY, TAG_SEGM  , TAG_NODE_MSR,
     .              TAG_SEGM2   , TAG_NM    , TAG_NLINS,TAG_NLINM,
     .              TAG_NLINS2  , TAG_NLINM2, TAG_NLG  ,TAG_NLG2,
     .              TAG_SCRATCH , INTERCEP  , IPARI_L  ,NI ,TAG_NSNE,
     .              TAG_NMNE    , TAG_NSVE  , TAG_MSRE ,NINDX_NM,INDX_NM,
     .              NINDX_SCRT  , INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
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
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP(3,NINTER)

      INTEGER PROC,IPARI(*),NI,
     .        TAG_NODE_2RY(*),TAG_SEGM(*),TAG_NM(*),TAG_NODE_MSR(*),
     .        TAG_SEGM2(*),TAG_SCRATCH(*),IPARI_L(NPARI,NINTER),
     .        TAG_NLINS(*), TAG_NLINM(*),TAG_NLINS2(*), TAG_NLINM2(*),
     .        TAG_NLG(*),TAG_NSNE(*),TAG_NMNE(*),TAG_NSVE(*),TAG_MSRE(*),
     .        TAG_NLG2(*)
      INTEGER, INTENT(INOUT) ::NINDX_NM,NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) ::INDX_NM,INDX_SCRT
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN,NLN,
     .        NLINS,NLINM,NLINSA,NLINMA,NSNE,NMNE,
     .        I,J,K,L,N,N1,N2,N3,N4,E,
     .        NL,N1L,N2L,N3L,N4L,
     .        NCONT,NCONT1,NCONT2,
     .        CNSN_L,CNRTM_L,CNMN_L,CNLN_L,
     .        CNLINS_L,CNLINSA_L,CNLINM_L,
     .        CNLINMA_L,CNMNE_L ,CNSNE_L
      my_real
     .        RCONT
C ----------------------------------------

      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)

      NLN    = IPARI(35)
      NLINS  = IPARI(51)
      NLINM  = IPARI(52)
      NLINSA = IPARI(53)
      NLINMA = IPARI(54)
      NSNE   = IPARI(55)
      NMNE   = IPARI(56)

      ! for type20, TAG_NM is used to tag main nodes, secnd nodes,
      ! line secnd, line main

      ! prepare SPLIT_NRTM_R
      CNRTM_L = 0
      DO K=1,NRTM
        IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
          N1L = INTBUF_TAB%IRECTM(4*(K-1)+1)
          N2L = INTBUF_TAB%IRECTM(4*(K-1)+2)
          N3L = INTBUF_TAB%IRECTM(4*(K-1)+3)
          N4L = INTBUF_TAB%IRECTM(4*(K-1)+4) 
          N1 = INTBUF_TAB%NLG(N1L)
          N2 = INTBUF_TAB%NLG(N2L)
          N3 = INTBUF_TAB%NLG(N3L)
          N4 = INTBUF_TAB%NLG(N4L)
          CNRTM_L = CNRTM_L + 1 
          TAG_SEGM(CNRTM_L) = K
          TAG_SEGM2(K) = CNRTM_L
          IF(TAG_NM(N1)==0)THEN
            TAG_NM(N1)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N1
          ENDIF
          IF(TAG_NM(N2)==0)THEN
            TAG_NM(N2)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N2
          ENDIF
          IF(TAG_NM(N3)==0)THEN
            TAG_NM(N3)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N3
          ENDIF
          IF(TAG_NM(N4)==0)THEN
            TAG_NM(N4)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N4
          ENDIF
        ENDIF
      ENDDO  

      CNMN_L = 0
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        N1 = INTBUF_TAB%NLG(N)
        IF(TAG_NM(N1)==1)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
        ENDIF
      ENDDO

      CNLN_L = 0
      CNSN_L = 0
      DO K=1, NSN
        NL=INTBUF_TAB%NSV(K)
        N =INTBUF_TAB%NLG(NL)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
          TAG_SCRATCH(N)=1
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
          IF(TAG_NM(N)==0)THEN
            CNLN_L = CNLN_L + 1
            TAG_NM(N)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N
           ENDIF
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0

C
C Partie ligne
C
      CNLINS_L = 0
      CNLINSA_L= 0
      CNLINM_L = 0
      CNLINMA_L= 0
      CNMNE_L  = 0
      CNSNE_L  = 0

      DO K=1,NLINS
        N1L = INTBUF_TAB%IXLINS(2*(K-1)+1)
        N2L = INTBUF_TAB%IXLINS(2*(K-1)+2)
        N1  = INTBUF_TAB%NLG(N1L)
        N2  = INTBUF_TAB%NLG(N2L)
        IF(INTERCEP(3,NI)%P(K)==PROC+1) THEN
          CNLINS_L = CNLINS_L + 1
          TAG_NLINS(CNLINS_L) = K
          TAG_NLINS2(K) = CNLINS_L
C comptage ligne active
          IF(K<=NLINSA)CNLINSA_L = CNLINSA_L + 1
          IF (TAG_SCRATCH(N1)==0) THEN
            CNSNE_L = CNSNE_L + 1
            TAG_NSNE(CNSNE_L) = N1
            TAG_NSVE(CNSNE_L) = N1L
            TAG_SCRATCH(N1) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N1
            IF(TAG_NM(N1)==0)THEN
              CNLN_L = CNLN_L + 1
              TAG_NM(N1) = 1
              NINDX_NM = NINDX_NM + 1
              INDX_NM(NINDX_NM) = N1
            END IF
          ENDIF
          IF (TAG_SCRATCH(N2)==0) THEN
            CNSNE_L = CNSNE_L + 1
            TAG_NSNE(CNSNE_L) = N2
            TAG_NSVE(CNSNE_L) = N2L
            TAG_SCRATCH(N2) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N2
            IF(TAG_NM(N2)==0)THEN
              CNLN_L = CNLN_L + 1
              TAG_NM(N2) = 1
              NINDX_NM = NINDX_NM + 1
              INDX_NM(NINDX_NM) = N2
            END IF
          ENDIF
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0

      DO K=1,NLINM
        N1L = INTBUF_TAB%IXLINM(2*(K-1)+1)
        N2L = INTBUF_TAB%IXLINM(2*(K-1)+2)
        N1  = INTBUF_TAB%NLG(N1L)
        N2  = INTBUF_TAB%NLG(N2L)
        IF(INTERCEP(2,NI)%P(K)==PROC+1) THEN
          CNLINM_L = CNLINM_L + 1
          TAG_NLINM(CNLINM_L) = K
          TAG_NLINM2(K) = CNLINM_L
C comptage ligne active
          IF(K<=NLINMA)CNLINMA_L = CNLINMA_L + 1
          IF (TAG_SCRATCH(N1)==0) THEN
            CNMNE_L = CNMNE_L + 1
            TAG_NMNE(CNMNE_L) = N1
            TAG_MSRE(CNMNE_L) = N1L
            TAG_SCRATCH(N1) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N1
            IF(TAG_NM(N1)==0)THEN
              CNMN_L = CNMN_L + 1
              TAG_NM(N1) = 1
              NINDX_NM = NINDX_NM + 1
              INDX_NM(NINDX_NM) = N1
          END IF
          ENDIF
          IF (TAG_SCRATCH(N2)==0) THEN
            CNMNE_L = CNMNE_L + 1
            TAG_NMNE(CNMNE_L) = N2
            TAG_MSRE(CNMNE_L) = N2L
            TAG_SCRATCH(N2) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N2
            IF(TAG_NM(N2)==0)THEN
              CNMN_L = CNMN_L + 1
              TAG_NM(N2) = 1
              NINDX_NM = NINDX_NM + 1
              INDX_NM(NINDX_NM) = N2
            END IF
          ENDIF
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Calcul de NCONT en prenant le max des edge et non edge
C
      NCONT1 = 0
      IF(NMN/=0) THEN
        RCONT = CNMN_L
        RCONT = RCONT/NMN
        NCONT = NINT(NSN*RCONT)
        IF(CNMN_L>0.AND.NSN>0) NCONT1 = MAX(NCONT,1)
      ENDIF

      NCONT2 = 0
      IF(NMNE/=0) THEN
        RCONT = CNMNE_L
        RCONT = RCONT/NMNE
        NCONT = NINT(NSNE*RCONT)
        IF(CNMNE_L>0.AND.NSNE>0) NCONT2 = MAX(NCONT,1)
      ENDIF
      NCONT = MAX(NCONT1,NCONT2)

C
C
c fill node global to local
      K = 0
      DO L = 1, NLN
        I = INTBUF_TAB%NLG(L)
        IF(TAG_NM(I) == 1) THEN
          K = K + 1
C TAGG(I) = K => noeud I vers numero local de noeud d interface sur le proc
          TAG_NLG(K) = L
          TAG_NLG2(I) = K
        END IF
      END DO

      IPARI_L(35,NI) = CNLN_L
      IPARI_L(51,NI) = CNLINS_L
      IPARI_L(52,NI) = CNLINM_L
      IPARI_L(53,NI) = CNLINSA_L
      IPARI_L(54,NI) = CNLINMA_L
      IPARI_L(55,NI) = CNSNE_L
      IPARI_L(56,NI) = CNMNE_L

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_RVAL_I20            source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_RVAL_I20(TAB,DIM1,DIM2,TAG_SEG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG_SEG(*),DIM1,DIM2

      my_real TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

      my_real, DIMENSION(:),ALLOCATABLE :: RBUF
C ----------------------------------------
      ALLOCATE(RBUF(DIM1*DIM2))
      DO I=1, DIM1
        K=TAG_SEG(I)
        DO J=1,DIM2
          RBUF(DIM2*(I-1)+J) = TAB(DIM2*(K-1)+J)
        ENDDO
      ENDDO

      CALL WRITE_DB(RBUF,DIM1*DIM2)
      DEALLOCATE(RBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_IVAL_I20            source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_IVAL_I20(TAB,TAB_NLG,DIM1,DIM2,TAG,TAG_NLG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG(*),TAB_NLG(*),TAG_NLG(*),DIM1,DIM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))
      DO I=1, DIM1
        K=TAG(I)
        DO J=1,DIM2   
          N = TAB_NLG(TAB(DIM2*(K-1)+J))
          IBUF(DIM2*(I-1)+J) = TAG_NLG(N)
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_IVAL_I20_2          source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_IVAL_I20_2(TAG_SEG,DIM1,TAG_NLG)
c specific inter 20 + redirection local to global
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG_SEG(*),TAG_NLG(*),DIM1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))
      DO I=1, DIM1
        K=TAG_SEG(I)
        IBUF(I) = TAG_NLG(K)
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  PREPARE_SPLIT_CAND_I20_EDGE   source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_CAND_I20_EDGE(INTBUF_TAB, TAG_NLINS2, II_STOK, TAG_II)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG_NLINS2(*),TAG_II(*),II_STOK

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        K,E,C_II
C ----------------------------------------

! prepare split candidates
      C_II = 0
      DO K = 1, II_STOK
        E = INTBUF_TAB%LCAND_N(K)
        IF (TAG_NLINS2(E)/=0) THEN
          C_II = C_II + 1
          TAG_II(C_II) = K
        ENDIF
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_I20                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_I20(PROC     , INTBUF_TAB, NSN    , NSN_L       ,
     .                         TAG_SEGM2 , II_STOK   , MULTIMP, NCONT       ,
     .                         NOINT     , INACTI    , TAG_SCRATCH ,
     .                         II_STOK_L , IPARI_L,NI,NINDX_SCRT,INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER PROC,NSN,NSN_L,II_STOK,MULTIMP,NCONT,
     .        TAG_SEGM2(*),NOINT,INACTI,NI,
     .        TAG_SCRATCH(*) , II_STOK_L, IPARI_L(NPARI,NINTER) 
      INTEGER, INTENT(INOUT) ::NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) ::INDX_SCRT 

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,P,E,MULTOK,MSGID,
     .        SPLIST,C_NSNR
      INTEGER NUMP(NSPMD),WORK(70000)

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF_E,IBUF_N,NSNLOCAL,CPULOCAL,CANDR,PLIST,
     .  INDEX

      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI
C ----------------------------------------
      ALLOCATE(IBUF_E(MULTIMP*NCONT),IBUF_N(MULTIMP*NCONT))

      IBUF_E(1:MULTIMP*NCONT) = 0
      IBUF_N(1:MULTIMP*NCONT) = 0
      II_STOK_L = 0 !mandatory in case of inacti ne 5,6,7 

      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN 
        IF(NSN>0) THEN
          ALLOCATE(NSNLOCAL(NSN))
          ALLOCATE(CPULOCAL(NSN))
          ALLOCATE(CANDR(NSN))
        END IF

        NUMP(1:NSPMD) = 0

        ALLOCATE(PLIST(NSPMD)) 
        PLIST(1:NSPMD) = -1
        DO K=1,NSN
          N = INTBUF_TAB%NSV(K)
          NSNLOCAL(K) = 0
          IF(TAG_SCRATCH(N)==0) THEN 
            SPLIST=0 
            CALL PLIST_IFRONT(PLIST,N,SPLIST) 
            DO I=1,SPLIST
              P=PLIST(I)
              NUMP(P) = NUMP(P)+1
            ENDDO
            IF(NLOCAL(N,PROC+1)==1) THEN
              NSNLOCAL(K) = NUMP(PROC+1)
              CPULOCAL(K) = PROC+1
            ELSE
              P = PLIST(1)
              NSNLOCAL(K) = NUMP(P)
              CPULOCAL(K) = P
            ENDIF
            TAG_SCRATCH(N) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
        ENDDO
        DEALLOCATE(PLIST) 

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Reperage des candidats se trouvant sur des procs distants
C
        C_NSNR = 0

        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            N = INTBUF_TAB%CAND_N(K)
            IF(TAG_SCRATCH(N)==0) THEN
              TAG_SCRATCH(N) = 1
              NINDX_SCRT = NINDX_SCRT + 1
              INDX_SCRT(NINDX_SCRT) = N
              IF(NLOCAL(INTBUF_TAB%NSV(N),PROC+1)/=1)THEN
                C_NSNR = C_NSNR + 1
                CANDR(C_NSNR) = N
              END IF
            END IF
          ENDIF
        ENDDO

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Tris des candidats remote par proc et par nsv local croissant
C
        IF(C_NSNR>0) THEN
          ALLOCATE(INDEX(2*C_NSNR))
          ALLOCATE(ITRI(2,C_NSNR))
        END IF
        DO I = 1, C_NSNR
          N = CANDR(I)
          ITRI(1,I) = CPULOCAL(N)
          ITRI(2,I) = NSNLOCAL(N)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,C_NSNR,2)
C
        DO I = 1, C_NSNR
          INDEX(C_NSNR+INDEX(I)) = I
        ENDDO
        DO I = 1, C_NSNR
          INDEX(I)=INDEX(C_NSNR+I)
        ENDDO
C
        II_STOK_L = 0

        C_NSNR = 0
        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            II_STOK_L = II_STOK_L + 1
          END IF
        END DO

        IF(II_STOK_L>MULTIMP*NCONT)THEN
          MULTOK= II_STOK_L/NCONT
          CALL ANCMSG(MSGID=626,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MULTOK,
     .                I2=NOINT)
        ELSE
          II_STOK_L = 0
C
          DO K = 1, II_STOK
            E = INTBUF_TAB%CAND_E(K)
            IF (TAG_SEGM2(E)/=0) THEN
              N = INTBUF_TAB%CAND_N(K)
              II_STOK_L = II_STOK_L + 1
              IBUF_E(II_STOK_L)=TAG_SEGM2(E)

              IF(NLOCAL(INTBUF_TAB%NSV(N),PROC+1)==1) THEN
                IBUF_N(II_STOK_L)=NSNLOCAL(N)
              ELSE
C noeud remote : numerotation pre calculee ci-dessus
c                IF(TAG(N)==0) THEN
                IF(TAG_SCRATCH(N)==0) THEN
                  C_NSNR = C_NSNR + 1
                  IBUF_N(II_STOK_L)=INDEX(C_NSNR)+NSN_L
                  TAG_SCRATCH(N) = INDEX(C_NSNR)+NSN_L
                  NINDX_SCRT = NINDX_SCRT + 1
                  INDX_SCRT(NINDX_SCRT) = N
                ELSE
                  IBUF_N(II_STOK_L) = TAG_SCRATCH(N)
                END IF ! TAG(N)==0
              END IF ! NLOCAL(INTBUF_TAB%NSV(N),PROC+1)==1
            ENDIF !TAG_SEGM_2(E)/=0
          ENDDO !K = 1, II_STOK
        END IF !II_STOK_L>MULTIMP*NCONT

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
  
        IF(NSN>0) DEALLOCATE(NSNLOCAL,CPULOCAL,CANDR)
        IF(C_NSNR>0) DEALLOCATE(INDEX,ITRI)

        IF(INACTI==5.OR.INACTI==6.OR.INACTI==7)IPARI_L(24,NI)= C_NSNR
         
      ENDIF !END INACTI=5,6,7

      CALL WRITE_I_C(IBUF_E,MULTIMP*NCONT)
      CALL WRITE_I_C(IBUF_N,MULTIMP*NCONT)

      DEALLOCATE(IBUF_E,IBUF_N)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_I20_EDGE           source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_I20_EDGE(PROC      , INTBUF_TAB, NLINS  , NLINS_L     ,
     .                               TAG_NLINS2, II_STOKE  , MULTIMP, NCONTE      ,
     .                               NOINT     , INACTI    , TAG_SCRATCH ,
     .                               II_STOKE_L, IPARI_L   , NI     ,NINDX_SCRT   ,
     .                               INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER PROC,NLINS,NLINS_L,II_STOKE,MULTIMP,NCONTE,
     .        TAG_NLINS2(*),NOINT,INACTI,NI,
     .        TAG_SCRATCH(*) , II_STOKE_L , IPARI_L(NPARI,NINTER)  
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) ::INDX_SCRT 

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,P,N1L,N2L,N1,N2,E,MULTOK,MSGID,
     .        SPLIST,C_NLINSR
      INTEGER NUMP(NSPMD),WORK(70000)

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF_E,IBUF_N,NRTSLOCAL,CPULOCAL,CANDR,PLIST,
     .  INDEX

      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI
C ----------------------------------------
      ALLOCATE(IBUF_E(MULTIMP*NCONTE),IBUF_N(MULTIMP*NCONTE))
      IBUF_E(1:MULTIMP*NCONTE) = 0
      IBUF_N(1:MULTIMP*NCONTE) = 0
      II_STOKE_L = 0 !mandatory in case of inacti ne 5,6,7 

      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN 
        IF(NLINS>0) THEN
          ALLOCATE(NRTSLOCAL(NLINS))
          ALLOCATE(CPULOCAL(NLINS))
          ALLOCATE(CANDR(NLINS))
        END IF

        NUMP(1:NSPMD) = 0

        DO K=1,NLINS
          N1L = INTBUF_TAB%IXLINS(2*(K-1)+1)
          N2L = INTBUF_TAB%IXLINS(2*(K-1)+2)
          N1  = INTBUF_TAB%NLG(N1L)
          N2  = INTBUF_TAB%NLG(N2L)          
          NRTSLOCAL(K) = 0
          IF(NLOCAL(N1,PROC+1)==1.AND.
     .       NLOCAL(N2,PROC+1)==1) THEN
            NUMP(PROC+1) = NUMP(PROC+1) + 1
            NRTSLOCAL(K) = NUMP(PROC+1)
            CPULOCAL(K) = PROC+1
          END IF
          DO P = 1, NSPMD
            IF(P/=PROC+1.AND.NLOCAL(N1,P)==1.AND.
     .         NLOCAL(N2,P)==1) THEN
              IF(NRTSLOCAL(K)==0) THEN
                NUMP(P) = NUMP(P) + 1
                NRTSLOCAL(K) = NUMP(P)
                CPULOCAL(K) = P
              END IF
            END IF
          END DO
        ENDDO
C
C Reperage des candidats se trouvant sur des procs distants
C
        !TAG_SCRATCH must have been reflush correctly to 0
        C_NLINSR = 0
        DO K = 1, II_STOKE
          E = INTBUF_TAB%LCAND_N(K)
          IF (TAG_NLINS2(E)/=0) THEN
            N = INTBUF_TAB%LCAND_S(K)
            IF(TAG_SCRATCH(N)==0) THEN
              TAG_SCRATCH(N) = 1
              NINDX_SCRT = NINDX_SCRT + 1
              INDX_SCRT(NINDX_SCRT) = N
              N1L = INTBUF_TAB%IXLINS(2*(N-1)+1)
              N2L = INTBUF_TAB%IXLINS(2*(N-1)+2)
              N1 =  INTBUF_TAB%NLG(N1L)
              N2 =  INTBUF_TAB%NLG(N2L)
              IF(CPULOCAL(N)/=PROC+1)THEN
                C_NLINSR = C_NLINSR + 1
                CANDR(C_NLINSR) = N
              END IF
            END IF
          ENDIF
        ENDDO

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Tris des candidats remote par proc et par nsv local croissant
C
        IF(C_NLINSR>0) THEN
          ALLOCATE(INDEX(2*C_NLINSR))
          ALLOCATE(ITRI(2,C_NLINSR))
        END IF
        DO I = 1, C_NLINSR
          N = CANDR(I)
          ITRI(1,I) = CPULOCAL(N)
          ITRI(2,I) = NRTSLOCAL(N)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,C_NLINSR,2)
C
        DO I = 1, C_NLINSR
          INDEX(C_NLINSR+INDEX(I)) = I
        ENDDO
        DO I = 1, C_NLINSR
          INDEX(I)=INDEX(C_NLINSR+I)
        ENDDO
C
        II_STOKE_L = 0
        II_STOKE_L = 0
        C_NLINSR = 0
C
        DO K = 1, II_STOKE
          E = INTBUF_TAB%LCAND_N(K)
          IF (TAG_NLINS2(E)/=0) THEN
            II_STOKE_L = II_STOKE_L + 1
            IBUF_E(II_STOKE_L)=TAG_NLINS2(E)
            L = INTBUF_TAB%LCAND_N(K)
            N1L = INTBUF_TAB%IXLINS(2*(L-1)+1)
            N2L = INTBUF_TAB%IXLINS(2*(L-1)+2)
            N1 =  INTBUF_TAB%IXLINS(N1L)
            N2 =  INTBUF_TAB%IXLINS(N2L)
            IF(CPULOCAL(L) == PROC+1)THEN
              IBUF_N(II_STOKE_L)=NRTSLOCAL(L)
            ELSE
C noeud remote : numerotation pre calculee ci-dessus
              IF(TAG_SCRATCH(L)==0) THEN
                C_NLINSR = C_NLINSR + 1
                IBUF_N(II_STOKE_L) = INDEX(C_NLINSR)+NLINS_L
                TAG_SCRATCH(L) = INDEX(C_NLINSR)+NLINS_L
                NINDX_SCRT = NINDX_SCRT + 1
                INDX_SCRT(NINDX_SCRT) = L
              ELSE
                IBUF_N(II_STOKE_L) = TAG_SCRATCH(L)
              END IF
            END IF
          ENDIF !TAG_NLINS2(E)/=0
        ENDDO !K = 1, II_STOKE

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
  
        IF(NLINS>0) DEALLOCATE(NRTSLOCAL,CPULOCAL,CANDR)
        IF(C_NLINSR>0) DEALLOCATE(INDEX,ITRI)

        IF(INACTI==5.OR.INACTI==6.OR.INACTI==7) IPARI_L(57,NI)= C_NLINSR
         
      ENDIF !END INACTI=5,6,7

      CALL WRITE_I_C(IBUF_E,MULTIMP*NCONTE)
      CALL WRITE_I_C(IBUF_N,MULTIMP*NCONTE)

      DEALLOCATE(IBUF_E,IBUF_N)

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT20
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT 21
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I21             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I21(PROC        , INTBUF_TAB  , IPARI       ,
     .                             TAG_NODE_2RY, TAG_SEGM    , TAG_NODE_2RY2,
     .                             TAG_SEGS    , TAG_NODE_MSR,
     .                             TAG_SCRATCH , INTERCEP    , NI, INTTH   ,
     .                             NODLOCAL    ,MSR_L_I21    ,MNDD_I21     ,
     .                             NINDX_SCRT  ,INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        IPARI(*),NI

      INTEGER PROC,TAG_NODE_2RY(*),TAG_SEGM(*),TAG_NODE_2RY2(*),
     .        TAG_SEGS(*),TAG_NODE_MSR(*),TAG_SCRATCH(*), NODLOCAL(*),
     .        MSR_L_I21(*),MNDD_I21(*)
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) ::INDX_SCRT

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP(3,NINTER)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      INTEGER, DIMENSION(:),ALLOCATABLE :: PLIST
      INTEGER 
     .        NSN,NRTM,NMN,NRTS,INTTH, NMNG,FLAGLOADP

      INTEGER 
     .        I,J,K,L,M,N,N1,N2,N3,N4,JJ,SPLIST,
     .        CNRTM_L,CNRTS_L,CNSN_L,CNMN_L
C ----------------------------------------
      NRTS = IPARI(3)
      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)
      NMNG = IPARI(8)
      FLAGLOADP = IPARI(95)

      CNRTS_L = 0
      DO K=1,NRTS
        IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
          CNRTS_L = CNRTS_L + 1
          TAG_SEGS(CNRTS_L) = K
        ENDIF
      ENDDO

      CNSN_L = 0
      DO K=1, NSN
        N=INTBUF_TAB%NSV(K)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
          TAG_NODE_2RY2(K) = CNSN_L
          TAG_SCRATCH(N)=1
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0

      CNRTM_L = 0
      DO K=1,NRTM
        CNRTM_L = CNRTM_L + 1 
        TAG_SEGM(CNRTM_L) = K
      ENDDO  

      CNMN_L = 0 
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0) THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
          TAG_SCRATCH(N)=1
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0

      !PREPARE MNDD TAB FOR main TEMPERATURE COMMUNICATION
      ALLOCATE(PLIST(NSPMD)) 
      PLIST(1:NSPMD) = -1
      IF (INTTH == 2.OR.FLAGLOADP > 0) THEN
        DO K=1,NMNG
          N = INTBUF_TAB%MSR(K)
          IF(NLOCAL(N,PROC+1)==1) THEN
            ! node on domain
            MNDD_I21(K) = 0
          ELSE
            ! node is not on domain, set value to first domain of node
            CALL PLIST_IFRONT(PLIST,N,SPLIST)
            MNDD_I21(K) = PLIST(1)
          ENDIF                      
        END DO  
      ENDIF 
      DEALLOCATE(PLIST)
      ! PREPARE MSRL TAB FOR LOCAL NODES NUMBERING==> main TEMPERATURE COMMUNICATION     
      IF (INTTH == 2.OR.FLAGLOADP > 0) THEN
        MSR_L_I21(1:NMNG)=0
        DO I=1,NMNG
          N = INTBUF_TAB%MSR(I)
          IF(NLOCAL(N,PROC+1)==1) THEN
            MSR_L_I21(I) = NODLOCAL(N)
          ENDIF
        END DO  
      ENDIF 

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_IVAL_I21           source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_IVAL_I21(TAB    , II_STOK_L, TAG_II,
     .                               DIM1, DIM2 )
C introduce a second dimension
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_II(*),II_STOK_L,MULTIMP,NCONT,
     .        DIM1,DIM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))
      IBUF(1:DIM1*DIM2) = 0

      DO I=1,II_STOK_L
        K=TAG_II(I)
        DO J=1,DIM2
          IBUF(DIM2*(I-1)+J) = TAB(DIM2*(K-1)+J)
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_2RY_CAND_IVAL_I21       source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_2RY_CAND_IVAL_I21(TAB    , II_STOK_L, TAG_II,TAG_NODE_2RY2,
     .                               DIM1 )
C introduce a second dimension
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_II(*),II_STOK_L,
     .        DIM1,TAG_NODE_2RY2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))
      IBUF(1:DIM1) = 0

      DO I=1,II_STOK_L
        K=TAG_II(I)
        N = TAB(K)
        IBUF(I) = TAG_NODE_2RY2(N)
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  PREPARE_SPLIT_CAND_I21        source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_CAND_I21(INTBUF_TAB, TAG_NODE_2RY2, II_STOK, TAG_II,C_II,PROC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG_NODE_2RY2(*),TAG_II(*),II_STOK, PROC

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        K,M,N,C_II
C ----------------------------------------

! prepare split candidates
      C_II = 0
      DO K = 1, II_STOK
        M = INTBUF_TAB%CAND_N(K)
        N = INTBUF_TAB%NSV(M)
        IF (NLOCAL(N,PROC+1)==1) THEN
          IF(ABS(INTBUF_TAB%IRTLM(2*(M-1)+1))==INTBUF_TAB%CAND_E(K)) THEN
            C_II = C_II + 1
            TAG_II(C_II) = K
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT21
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT 24
C=======================================================================
Chd|====================================================================
Chd|  PREPARE_SPLIT_I24             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I24(PROC     , INTBUF_TAB  , IPARI       ,
     .                            INTERCEP , TAG_NODE_2RY, TAG_SEGM    ,
     .                            TAG_SEGM2, TAG_NM      , TAG_NODE_MSR,
     .                            TAG_SCRATCH,NODLOCAL24 ,NODLOCAL,
     .                            INTERCEP2,
     .                            NUMNOD_L,TAG_NSNE,TAG_SEGS,TAG_SEGS2,NI,TAG_2RY_INV,
     .                            IEDGE4,TAG_NODE_2RY2,TAG_IELEM,CEP,CEL,TAG_SEGSS,
     .                            NINDX_NM,INDX_NM,NINDX_SCRT,INDX_SCRT,
     .                            NINDX_NDLOCAL24,INDX_NDLOCAL24,INTERCEP3)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
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
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP,INTERCEP2,INTERCEP3

      INTEGER PROC,INTNITSCHE,IPARI(*),
     .        TAG_NODE_2RY(*),TAG_SEGM(*),TAG_NM(*),TAG_NODE_MSR(*),
     .        TAG_SEGM2(*),TAG_SCRATCH(*),NODLOCAL24(*) ,NODLOCAL(*),
     .        NUMNOD_L,TAG_NSNE(*),TAG_SEGS(*),TAG_SEGS2(*),NI,TAG_2RY_INV(*),IEDGE4,
     .        TAG_NODE_2RY2(*),TAG_IELEM(*),CEP(*),CEL(*),TAG_SEGSS(*)
      INTEGER, INTENT(INOUT) ::NINDX_NM,NINDX_SCRT,NINDX_NDLOCAL24
      INTEGER, DIMENSION(*), INTENT(INOUT) ::INDX_NM,INDX_SCRT,INDX_NDLOCAL24
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN,NRTS,
     .        I,J,K,N,N1,N2,N3,N4,E,IE,IE_LOC,PROC2,
     .        CNSN_L,CNRTM_L,CNMN_L,NRTSE,NSNE,NSN0,NSNE_COUNT,SE1,
     .        CNRTS_L
      INTEGER 
     .   SECND_SURFACE_ON_DOMAIN,NRTSE_COUNT
       EXTERNAL SECND_SURFACE_ON_DOMAIN
C ----------------------------------------
      NRTS = IPARI(3)
      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)
      NRTSE= IPARI(52)
      NSNE = IPARI(55)
      NSN0 = NSN - NSNE
      INTNITSCHE = IPARI(86)

      CNSN_L = 0
      DO K=1, NSN0
        N=INTBUF_TAB%NSV(K)
        IF(    (NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L)
     +     .AND.TAG_SCRATCH(N)==0) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
          IF(IEDGE4 > 0) TAG_2RY_INV(K)=CNSN_L
          TAG_SCRATCH(N)=1
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
        ENDIF
      ENDDO
      
      DO K=1+NSN0, NSNE+NSN0
        N=INTBUF_TAB%NSV(K)-NUMNOD
        SE1 = INTBUF_TAB%IS2SE(2*(N-1)+1)
        IF (INTERCEP2%P(SE1)==PROC+1)THEN
           CNSN_L = CNSN_L+1
           TAG_NODE_2RY(CNSN_L) = K
           TAG_2RY_INV(K)=CNSN_L
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0



! prepare SPLIT_NRTM_R
      CNRTM_L = 0
      DO K=1,NRTM
        N1 = INTBUF_TAB%IRECTM(4*(K-1)+1)
        N2 = INTBUF_TAB%IRECTM(4*(K-1)+2)
        N3 = INTBUF_TAB%IRECTM(4*(K-1)+3)
        N4 = INTBUF_TAB%IRECTM(4*(K-1)+4) 
        IF(INTERCEP%P(K)==PROC+1)THEN
          CNRTM_L = CNRTM_L + 1 
          TAG_SEGM(CNRTM_L) = K
          TAG_SEGM2(K) = CNRTM_L
          IF(TAG_NM(N1)==0)THEN
            TAG_NM(N1)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N1 
          ENDIF
          IF(TAG_NM(N2)==0)THEN
            TAG_NM(N2)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N2 
          ENDIF
          IF(TAG_NM(N3)==0)THEN
            TAG_NM(N3)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N3
          ENDIF
          IF(TAG_NM(N4)==0)THEN
            TAG_NM(N4)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N4
          ENDIF
        ENDIF
      ENDDO  
 
      CNMN_L = 0 
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        IF(TAG_NM(N)==1)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
        ENDIF
      ENDDO

      DO I=1,CNSN_L
        N = TAG_NODE_2RY(I)
        TAG_NODE_2RY2(N) = I
      ENDDO



!         NODLOCAL24(1:NUMNOD)=NODLOCAL(1:NUMNOD)
C Prepare Node ID with virtual Type 24 E2E Nodes
      IF (NSNE > 0 ) THEN

         NSNE_COUNT=0


         DO I = 1,NSNE
           
C Get First IS2SE surface 
C If Surface is on P, than speudo node is on pocessor.
C Give him NODLOCAL ID (+NUMNOD_L).
C Evt set TAG array
           SE1 = INTBUF_TAB%IS2SE(2*(I-1)+1)
           IF (INTERCEP2%P(SE1)==PROC+1)THEN
             NSNE_COUNT=NSNE_COUNT+1
             NODLOCAL24(NUMNOD+I) = NUMNOD_L + NSNE_COUNT
             NINDX_NDLOCAL24 = NINDX_NDLOCAL24 + 1
             INDX_NDLOCAL24(NINDX_NDLOCAL24) = NUMNOD+I
             TAG_NSNE(NSNE_COUNT)=I
           ENDIF
         END DO

         NRTSE_COUNT=0
         DO I = 1,NRTSE
            IF(INTERCEP2%P(I)==PROC+1)THEN
               NRTSE_COUNT=NRTSE_COUNT+1
               TAG_SEGS(NRTSE_COUNT)=I
               TAG_SEGS2(I)=NRTSE_COUNT
            ENDIF
         ENDDO


      END IF
C Nitsche Method

      IF(INTNITSCHE > 0) THEN

! prepare SPLIT_NRTM_R
       CNRTS_L = 0
       DO K=1,NRTS
         IF(INTERCEP3%P(K)==PROC+1)THEN
           CNRTS_L = CNRTS_L + 1 
           TAG_SEGSS(CNRTS_L) = K
         ENDIF
       ENDDO  

      !TAG_IELEM for IELNRTS tab writing

        DO I = 1, CNRTS_L ! NRTS = NRTM only if NRT_SH = 0
          K = TAG_SEGSS(I) 
          IE = INTBUF_TAB%IELNRTS(K)
c          PROC2  = CEP(IE)
c          IF(PROC2==PROC) THEN
            IE_LOC = CEL(IE)
            TAG_IELEM(I) = IE_LOC
c          ENDIF
        ENDDO
       ENDIF  
   
 
      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_NODE_IVAL_I24           source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_IVAL_I24(TAB,DIM1,TAG,TAG2)
c
c split node array with global value specific type24 & type25
c (see SPLIT_NODE_NODLOC for local values)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIM1,TAG(*),TAG2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1))

      DO I=1, DIM1
        K=TAG(I)
        N = TAB(K)
        IF(N>0)THEN
          IBUF(I) = TAG2(N)
        ELSEIF(N<0)THEN
          IBUF(I) = -TAG2(-N)
        ELSE
          IBUF(I) = 0
        ENDIF
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEG_NODLOC_I24          source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_NODLOC_I24(TAB,DIM1,DIM2,TAG_SEG,NODLOCAL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),TAG_SEG(*),DIM1,DIM2,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIM1*DIM2))
      DO I=1, DIM1
        K=TAG_SEG(I)
        DO J=1,DIM2
          N = TAB(DIM2*(K-1)+J)

          IF(N>0)THEN
            IBUF(DIM2*(I-1)+J) = NODLOCAL(N)
          ELSEIF(N<0)THEN
            IBUF(DIM2*(I-1)+J) = -NODLOCAL(-N)
          ELSE
            IBUF(DIM2*(I-1)+J) = 0
          ENDIF
        ENDDO
      ENDDO
      CALL WRITE_I_C(IBUF,DIM1*DIM2)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_SEGEDGE_NODLOC_I24      source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEGEDGE_NODLOC_I24(TAB,DIM1,TAG_SEG,NODLOCAL,NI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(5,*),TAG_SEG(*),DIM1,DIM2,NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2,N3,N4,NI
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(5,DIM1))
      DO I=1, DIM1
        K=TAG_SEG(I)
        N1 = TAB(1,K)
        N2 = TAB(2,K)
        N3 = TAB(3,K)
        N4 = TAB(4,K)

        IBUF(1,I)=NODLOCAL(N1)
        IBUF(2,I)=NODLOCAL(N2)
        IBUF(3,I)=NODLOCAL(N3)
        IBUF(4,I)=NODLOCAL(N4)

        IBUF(5,I)=TAB(5,K)
      ENDDO

      CALL WRITE_I_C(IBUF,DIM1*5)

      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_CAND_I24                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_I24(PROC    , INTBUF_TAB, NSN    , NSN_L        ,
     .                         TAG_SEGM2, II_STOK   , MULTIMP, NCONT        ,
     .                         NOINT    , INACTI    , TAG_SCRATCH ,II_STOK_L,
     .                         INTERCEP2, NINDX_SCRT, INDX_SCRT   ,NODLOCAL ,
     .                         NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC,NSN,NSN_L,II_STOK,MULTIMP,NCONT,
     .        TAG_SEGM2(*),NOINT,INACTI,
     .        TAG_SCRATCH(*) , II_STOK_L, ITYP 
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, INTENT(IN) :: NUMNOD_L
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_SCRT
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP2
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_XXX : size = NUMNOD
!                 index of non-zero TAG_XXX value 
!                 used for optimize the initialization
!                 of TAG_XXX array (XXX = NM or SCRT for SCRATCH)
!                 allocated array in lectur and threadprivate array
!       NINDX_XXX : number of non-zero TAG_XXX value
!       TAG_XXX : size = NUMNOD + NUMELS + I24MAXNSNE2
!                array used to tag an element for 
!                a given interface ; allocated in lectur
!                allocated array in lectur and threadprivate array
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,P,E,MULTOK,MSGID,
     .        SPLIST,C_NSNR,NN,SE1,MY_NODE
      INTEGER NUMP(NSPMD),WORK(70000)

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF_E,IBUF_N,NSNLOCAL,CPULOCAL,CANDR,PLIST,
     .  INDEX

      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI
      LOGICAL CONDITION
C ----------------------------------------
C       w to be done :modif w/ edge
C ----------------------------------------
      ALLOCATE(IBUF_E(MULTIMP*NCONT),IBUF_N(MULTIMP*NCONT))
      IBUF_E(1:MULTIMP*NCONT) = 0
      IBUF_N(1:MULTIMP*NCONT) = 0
      II_STOK_L = 0 !mandatory in case of inacti ne 5,6,7 

        IF(NSN>0) THEN
          ALLOCATE(NSNLOCAL(NSN))
          ALLOCATE(CPULOCAL(NSN))
          ALLOCATE(CANDR(NSN))
        END IF

        NUMP(1:NSPMD) = 0

        ALLOCATE(PLIST(NSPMD)) 
        PLIST(1:NSPMD) = -1
        DO K=1,NSN
          N = INTBUF_TAB%NSV(K)
          NSNLOCAL(K) = 0
          IF(TAG_SCRATCH(N)==0) THEN 
            SPLIST=0 
            IF(N<=NUMNOD)THEN
              CALL PLIST_IFRONT(PLIST,N,SPLIST) 
              DO I=1,SPLIST
                P=PLIST(I)
                NUMP(P) = NUMP(P)+1
              ENDDO
              IF( NODLOCAL( N )/=0.AND.NODLOCAL(N)<=NUMNOD_L ) THEN
                NSNLOCAL(K) = NUMP(PROC+1)
                CPULOCAL(K) = PROC+1
              ELSE
                P = PLIST(1)
                NSNLOCAL(K) = NUMP(P)
                CPULOCAL(K) = P
              ENDIF
            ELSE
C T24 E2E Nodes can be on 1 SPMD domain only
              NN = N - NUMNOD
              SE1 = INTBUF_TAB%IS2SE(2*(NN-1)+1)
              P = INTERCEP2%P(SE1)
              NUMP(P) = NUMP(P)+1
              NSNLOCAL(K) = NUMP(P)
              CPULOCAL(K) = P
            ENDIF

            TAG_SCRATCH(N) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
        ENDDO
        DEALLOCATE(PLIST) 

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Reperage des candidats se trouvant sur des procs distants
C
        C_NSNR = 0

        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            N = INTBUF_TAB%CAND_N(K)
C            IF (INTBUF_TAB%NSV(N)> NUMNOD) CYCLE
            IF(TAG_SCRATCH(N)==0) THEN
              TAG_SCRATCH(N) = 1
              NINDX_SCRT = NINDX_SCRT + 1
              INDX_SCRT(NINDX_SCRT) = N
              IF(INTBUF_TAB%NSV(N) <= NUMNOD)THEN
                 MY_NODE = INTBUF_TAB%NSV(N)
                 IF( NODLOCAL( MY_NODE ) ==0.OR.NODLOCAL( MY_NODE )>NUMNOD_L ) THEN
                   C_NSNR = C_NSNR + 1
                   CANDR(C_NSNR) = N
                 END IF
              ELSE
                 NN = INTBUF_TAB%NSV(N) - NUMNOD
                 SE1 = INTBUF_TAB%IS2SE(2*(NN-1)+1)
                 P = INTERCEP2%P(SE1)
                 IF(P/= (PROC+1) ) THEN
                    C_NSNR = C_NSNR + 1
                    CANDR(C_NSNR) = N
                 ENDIF
              ENDIF
            END IF
          ENDIF
        ENDDO

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Tris des candidats remote par proc et par nsv local croissant
C
        IF(C_NSNR>0) THEN
          ALLOCATE(INDEX(2*C_NSNR))
          ALLOCATE(ITRI(2,C_NSNR))
        END IF
        DO I = 1, C_NSNR
          N = CANDR(I)
          ITRI(1,I) = CPULOCAL(N)
          ITRI(2,I) = NSNLOCAL(N)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,C_NSNR,2)
C
        DO I = 1, C_NSNR
          INDEX(C_NSNR+INDEX(I)) = I
        ENDDO
        DO I = 1, C_NSNR
          INDEX(I)=INDEX(C_NSNR+I)
        ENDDO
C
        II_STOK_L = 0

        C_NSNR = 0
        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            II_STOK_L = II_STOK_L + 1
          END IF
        END DO

        IF(II_STOK_L>MULTIMP*NCONT)THEN
          MULTOK= II_STOK_L/NCONT
          CALL ANCMSG(MSGID=626,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MULTOK,
     .                I2=NOINT)
        ELSE
          II_STOK_L = 0
C
          DO K = 1, II_STOK
            E = INTBUF_TAB%CAND_E(K)
            IF (TAG_SEGM2(E)/=0) THEN
              N = INTBUF_TAB%CAND_N(K)
              II_STOK_L = II_STOK_L + 1
              IBUF_E(II_STOK_L)=TAG_SEGM2(E)
              IF (INTBUF_TAB%NSV(N)>NUMNOD) THEN
                  NN = INTBUF_TAB%NSV(N)-NUMNOD
                  SE1 = INTBUF_TAB%IS2SE(2*(NN-1)+1)
                  P=0
                  IF(INTERCEP2%P(SE1)==(PROC+1)) P=1
              ELSE
                  P = 0
                  MY_NODE = INTBUF_TAB%NSV(N)
                  IF( NODLOCAL( MY_NODE )/=0.AND.NODLOCAL( MY_NODE )<=NUMNOD_L )  P=1
              ENDIF    

              IF(P==1 ) THEN
                IBUF_N(II_STOK_L)=NSNLOCAL(N)
              ELSE
C noeud remote : numerotation pre calculee ci-dessus
c                IF(TAG(N)==0) THEN
                IF(TAG_SCRATCH(N)==0) THEN
                  C_NSNR = C_NSNR + 1
                  IBUF_N(II_STOK_L)=INDEX(C_NSNR)+NSN_L
                  TAG_SCRATCH(N) = INDEX(C_NSNR)+NSN_L
                  NINDX_SCRT = NINDX_SCRT + 1
                  INDX_SCRT(NINDX_SCRT) = N
                ELSE
                  IBUF_N(II_STOK_L) = TAG_SCRATCH(N)
                END IF ! TAG(N)==0
              END IF ! NLOCAL(INTBUF_TAB%NSV(N),PROC+1)==1
            ENDIF !TAG_SEGM_2(E)/=0
          ENDDO !K = 1, II_STOK
        END IF !II_STOK_L>MULTIMP*NCONT

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
  
        IF(NSN>0) DEALLOCATE(NSNLOCAL,CPULOCAL,CANDR)
        IF(C_NSNR>0) DEALLOCATE(INDEX,ITRI)

         
      CALL WRITE_I_C(IBUF_E,MULTIMP*NCONT)
      CALL WRITE_I_C(IBUF_N,MULTIMP*NCONT)

      DEALLOCATE(IBUF_E,IBUF_N)

      RETURN
      END
C
Chd|====================================================================
Chd|  SPLIT_SEG_EDGE                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_EDGE(NSNE_L,IS2SE,TAG_NSNE,TAG_SEG2,NI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IS2SE(2,*),NSNE_L,TAG_NSNE(*),TAG_SEG2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,SEG,SE1,SE2,NI
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IBUF
C-----------------------------------------------
      ALLOCATE(IBUF(2,NSNE_L))
      DO I=1,NSNE_L
         SEG=TAG_NSNE(I)
         SE1 = IS2SE(1,SEG)
         SE2 = IS2SE(2,SEG)
         IBUF(1,I)=TAG_SEG2(SE1)
        
         IF(SE2 /=0)THEN
            IBUF(2,I)=TAG_SEG2(SE2)
         ELSE
            IBUF(2,I)=0
         ENDIF
      ENDDO
      CALL WRITE_I_C(IBUF,2*NSNE_L)

      DEALLOCATE(IBUF)

      END
C=======================================================================
C END SPECIFIC ROUTINES INT24
C=======================================================================

C=======================================================================
C SPECIFIC ROUTINES INT 25
C=======================================================================
Chd|====================================================================
Chd|  SPLIT_NODE_IVAL_I25           source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_NODE_IVAL_I25(TAB,DIMO,DIMN,TAG,TAG2)
c
c split node array with global value specific type24 & type25
c (see SPLIT_NODE_NODLOC for local values)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB(*),DIMO,DIMN,TAG(*),TAG2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(DIMN))

      DO I=1, DIMN
        K=TAG(I)
        N = TAB(K)
        IF(N > 0)THEN
          IF(N > DIMO)THEN
            N = N-DIMO
            IBUF(I) = TAG2(N)+DIMN
          ELSE
            IBUF(I) = TAG2(N)
          END IF
        ELSEIF(N < 0)THEN
          IF(N < -DIMO)THEN
            N = N+DIMO
            IBUF(I) = -TAG2(-N)-DIMN
          ELSE
            IBUF(I) = -TAG2(-N)
          END IF
        ELSE
          IBUF(I) = 0
        ENDIF
      ENDDO

      CALL WRITE_I_C(IBUF,DIMN)
      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  PREPARE_SPLIT_I25             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I25(PROC     , INTBUF_TAB  , IPARI       ,
     .                            INTERCEP , TAG_NODE_2RY, TAG_SEGM    ,
     .                            TAG_SEGM2, TAG_NM      , TAG_NODE_MSR,
     .                            TAG_SCRATCH, TAG_SM ,KNOR2MSR,NOR2MSR,
     .                            TAG_NODE_2RY2,NINDX_NM,INDX_NM,NINDX_SCRT,INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP

      INTEGER PROC,IPARI(*),
     .        TAG_NODE_2RY(*),TAG_SEGM(*),TAG_NM(*),TAG_NODE_MSR(*),
     .        TAG_SEGM2(*),TAG_SCRATCH(*),TAG_SM(*),
     .        KNOR2MSR(*), NOR2MSR(*), TAG_NODE_2RY2(*)
      INTEGER, INTENT(INOUT) :: NINDX_NM,NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_NM,INDX_SCRT
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        NSN,NRTM,NMN,
     .        I,J,K,L,N,N1,N2,N3,N4,E,
     .        CNSN_L,CNRTM_L,CNMN_L,NADMSR_L
C ----------------------------------------
      NRTM = IPARI(4)
      NSN  = IPARI(5)
      NMN  = IPARI(6)

      CNSN_L = 0
      DO K=1, NSN
        N=INTBUF_TAB%NSV(K)
        IF(NLOCAL(N,PROC+1)==1.AND.TAG_SCRATCH(N)==0) THEN
          CNSN_L = CNSN_L+1
          TAG_NODE_2RY(CNSN_L) = K
          TAG_SCRATCH(N)=1
          TAG_NODE_2RY2(K) = CNSN_L
          NINDX_SCRT = NINDX_SCRT + 1
          INDX_SCRT(NINDX_SCRT) = N
        ENDIF
      ENDDO

      !reflush to zero only part of TAG_SCRATCH that has been used
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0

! prepare SPLIT_NRTM_R
      CNRTM_L = 0
      DO K=1,NRTM
        N1 = INTBUF_TAB%IRECTM(4*(K-1)+1)
        N2 = INTBUF_TAB%IRECTM(4*(K-1)+2)
        N3 = INTBUF_TAB%IRECTM(4*(K-1)+3)
        N4 = INTBUF_TAB%IRECTM(4*(K-1)+4) 
        IF(INTERCEP%P(K)==PROC+1)THEN
          CNRTM_L = CNRTM_L + 1 
          TAG_SEGM(CNRTM_L) = K
          TAG_SEGM2(K) = CNRTM_L
          IF(TAG_NM(N1)==0)THEN
            TAG_NM(N1)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N1
          ENDIF
          IF(TAG_NM(N2)==0)THEN
            TAG_NM(N2)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N2
          ENDIF
          IF(TAG_NM(N3)==0)THEN
            TAG_NM(N3)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N3
          ENDIF
          IF(TAG_NM(N4)==0)THEN
            TAG_NM(N4)=1
            NINDX_NM = NINDX_NM + 1
            INDX_NM(NINDX_NM) = N4
          ENDIF
        ENDIF
      ENDDO  

! prepare SPLIT Normals <=> Vertices 
      NADMSR_L=0
      DO K=1,NRTM
        N1 = INTBUF_TAB%ADMSR(4*(K-1)+1)
        N2 = INTBUF_TAB%ADMSR(4*(K-1)+2)
        N3 = INTBUF_TAB%ADMSR(4*(K-1)+3)
        N4 = INTBUF_TAB%ADMSR(4*(K-1)+4) 
        IF(INTERCEP%P(K)==PROC+1)THEN
          IF(TAG_SM(N1)==0)THEN
            NADMSR_L=NADMSR_L+1
            TAG_SM(N1)=NADMSR_L
          END IF
          IF(TAG_SM(N2)==0)THEN
            NADMSR_L=NADMSR_L+1
            TAG_SM(N2)=NADMSR_L
          END IF
          IF(TAG_SM(N3)==0)THEN
            NADMSR_L=NADMSR_L+1
            TAG_SM(N3)=NADMSR_L
          END IF
          IF(TAG_SM(N4)==0)THEN
            NADMSR_L=NADMSR_L+1
            TAG_SM(N4)=NADMSR_L
          END IF
        ENDIF
      ENDDO 

      CNMN_L = 0 
      DO I=1,NMN
        N = INTBUF_TAB%MSR(I)
        IF(TAG_NM(N)==1)THEN
          CNMN_L = CNMN_L + 1
          TAG_NODE_MSR(CNMN_L) = I
        ENDIF
      ENDDO

C-----------------------------------------------
C Pre construction ADRESSES
C    
      DO I=1,NRTM
        IF(INTERCEP%P(I)==PROC+1)THEN
          DO K=1,3
            N = TAG_SM(INTBUF_TAB%ADMSR(4*(I-1)+K))
            KNOR2MSR(N) = KNOR2MSR(N) + 1
          END DO
          IF(INTBUF_TAB%IRECTM(4*(I-1)+3)/=INTBUF_TAB%IRECTM(4*(I-1)+4))THEN
            N = TAG_SM(INTBUF_TAB%ADMSR(4*(I-1)+4))
            KNOR2MSR(N) = KNOR2MSR(N) + 1
          END IF
        END IF
      END DO
C
      DO I=1,NADMSR_L
        KNOR2MSR(I+1) = KNOR2MSR(I+1) + KNOR2MSR(I)
      END DO
C
      DO I=NADMSR_L,1,-1
        KNOR2MSR(I+1)=KNOR2MSR(I)
      END DO
      KNOR2MSR(1)=0
C
C Construction de la matrice Nod -> Shell elt
C
      DO I=1,NRTM
        IF(INTERCEP%P(I)==PROC+1)THEN
          DO K=1,3
            N = TAG_SM(INTBUF_TAB%ADMSR(4*(I-1)+K))
            KNOR2MSR(N) = KNOR2MSR(N) + 1
            NOR2MSR(KNOR2MSR(N)) = TAG_SEGM2(I)
          END DO
          IF(INTBUF_TAB%IRECTM(4*(I-1)+3)/=INTBUF_TAB%IRECTM(4*(I-1)+4))THEN
            N = TAG_SM(INTBUF_TAB%ADMSR(4*(I-1)+4))
            KNOR2MSR(N) = KNOR2MSR(N) + 1
            NOR2MSR(KNOR2MSR(N)) = TAG_SEGM2(I)
          END IF
        END IF
      END DO
C
      DO I=NADMSR_L,1,-1
        KNOR2MSR(I+1)=KNOR2MSR(I)
      END DO
      KNOR2MSR(1)=0
C
      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_LEDGE_I25               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_LEDGE_I25(NEDGE,
     .                          NEDGE_L,
     .                          IRECTM,
     .                          NRTM_L,
     .                          LEDGE,
     .                          MSEGLO,
     .                          ADMSR,
     .                          SEGLOC,
     .                          TAG_SM,
     .                          NODLOCAL,
     .                          TAG_EDGE,
     .                          ITAB,
     .                          PROC)
c
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
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDGE, NEDGE_L, NRTM_L, LEDGE(NLEDGE,*), MSEGLO(*), SEGLOC(*), NODLOCAL(*)
      INTEGER :: PROC
      INTEGER :: TAG_EDGE(NEDGE_L)
      INTEGER, INTENT(IN) :: ITAB(*)
      INTEGER, INTENT(IN) :: IRECTM(4,*) 
      INTEGER, INTENT(IN) :: ADMSR(4,*)
      INTEGER, INTENT(IN) :: TAG_SM(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,E1,K1,E2,K2,CMPT
      INTEGER :: ID
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
      INTEGER :: NB_FREE_EDGES
      INTEGER :: NB_INTERNAL_EDGES
      INTEGER :: NB_BOUNDARY_EDGES_LOCAL ! boundary edges treated by current domain 
      INTEGER :: NB_BOUNDARY_EDGES_REMOTE ! boundary edges treated by the other domain 
      INTEGER :: IAS,JAS,IS,N1,N2,I1,I2
C ----------------------------------------
      ALLOCATE(IBUF(NLEDGE*NEDGE_L))

      CMPT=0
      ID = 1
      NB_FREE_EDGES = 0
C FREE EDGES
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        K1=SEGLOC(E1)
        E2=LEDGE(3,I)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 > 0 .AND. K2 ==  -1) THEN
          NB_FREE_EDGES = NB_FREE_EDGES + 1
! Internal edge 
          TAG_EDGE(ID) = I
C at starter phase 9 and 10 are used to save PROC and local ID
          ASSERT(LEDGE(9,I) == PROC) 
          ASSERT(LEDGE(10,I) == ID)
          LEDGE(9,I) = PROC 
          LEDGE(10,I) = ID

          ID = ID + 1
          CMPT=CMPT+1
          IBUF(CMPT) = K1
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(2,I)
          CMPT=CMPT+1
          IBUF(CMPT) = 0
          CMPT=CMPT+1
          IBUF(CMPT) = 0          
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(5,I))
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(6,I))
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(7,I)
          CMPT=CMPT+1
          IBUF(CMPT) = I !  + 10000* ITAB(LEDGE(6,I))
          CMPT=CMPT+1
          IBUF(CMPT) = 1 !  Weight                         
C orientation  segment 1
              CMPT=CMPT+1
          IAS=LEDGE(1,I)
          JAS=LEDGE(2,I)
          N1=LEDGE(5,I)
          N2=LEDGE(6,I)
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
          END IF
          IBUF(CMPT) = IS 
          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
          END IF
          CMPT=CMPT+1
          IBUF(CMPT) = TAG_SM(I1) 
          CMPT=CMPT+1
          IBUF(CMPT) = TAG_SM(I2) 
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)
C orientation segment 2
          CMPT=CMPT+1
          IBUF(CMPT) = 0
          CMPT=CMPT+1
          IBUF(CMPT) = 0
          CMPT=CMPT+1
          IBUF(CMPT) = 0 
        END IF
      ENDDO

C INTERNAL EDGES 
      NB_INTERNAL_EDGES = 0
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        K1=SEGLOC(E1)
        E2=LEDGE(3,I)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 > 0 .AND. K2 > 0) THEN
          NB_INTERNAL_EDGES = NB_INTERNAL_EDGES + 1

! Internal edge 
          TAG_EDGE(ID) = I
C at starter phase 9 and 10 are used to save PROC and local ID
          ASSERT(LEDGE(9,I) == PROC) 
          ASSERT(LEDGE(10,I) == ID)
          LEDGE(9,I) = PROC 
          LEDGE(10,I) = ID

          ID = ID + 1
          CMPT=CMPT+1
          IBUF(CMPT) = K1
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(2,I)
          CMPT=CMPT+1
          IBUF(CMPT) = K2
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(4,I)
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(5,I))
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(6,I))
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(7,I)
          CMPT=CMPT+1
          IBUF(CMPT) = I !  + 10000* ITAB(LEDGE(6,I))
          CMPT=CMPT+1
          IBUF(CMPT) = 1 !  Weight                         
C orientation  segment 1
          CMPT=CMPT+1
          IAS=LEDGE(1,I)
          JAS=LEDGE(2,I)
          N1=LEDGE(5,I)
          N2=LEDGE(6,I)
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
          END IF
          IBUF(CMPT) = IS 
          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
          END IF
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I1) 
          CMPT=CMPT+1
          IBUF(CMPT) = TAG_SM(I2) 
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)

C orientation segment 2
          CMPT=CMPT+1
          IAS=LEDGE(3,I)
          JAS=LEDGE(4,I)
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
          END IF
          IBUF(CMPT) = IS

          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
          END IF
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I1) 
          CMPT=CMPT+1
          IBUF(CMPT) = TAG_SM(I2) 
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)



          END IF
      ENDDO

      NB_BOUNDARY_EDGES_LOCAL = 0
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        K1=SEGLOC(E1)
        E2=LEDGE(3,I)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 > 0 .AND. K2 == 0) THEN
          NB_BOUNDARY_EDGES_LOCAL = NB_BOUNDARY_EDGES_LOCAL + 1
          TAG_EDGE(ID) = I
C at starter phase 9 and 10 are used to save PROC and local ID
          ASSERT(LEDGE(9,I) == PROC) 
c         ASSERT(LEDGE(10,I) == ID)
          LEDGE(9,I) = PROC 
          LEDGE(10,I) = ID

          ID = ID + 1
          CMPT=CMPT+1
          IBUF(CMPT) = K1
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(2,I)
C boundary edge: remote segment
          CMPT=CMPT+1
C         IBUF(CMPT) = -MSEGLO(E2)
          IBUF(CMPT) = -K1
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(4,I)
C         IBUF(CMPT) = LEDGE(2,I)
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(5,I))
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(6,I))
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(7,I)
          CMPT=CMPT+1
          IBUF(CMPT) = I !  + 10000* ITAB(LEDGE(6,I))
          CMPT=CMPT+1
          IBUF(CMPT) = 1 !  Weight                         
C orientation  segment 1
          CMPT=CMPT+1
          IAS=LEDGE(1,I)
          JAS=LEDGE(2,I)
          N1=LEDGE(5,I)
          N2=LEDGE(6,I)
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
          END IF
          IBUF(CMPT) = IS 
          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
          END IF
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I1) 
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I2) 
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)

C orientation segment 2
          CMPT=CMPT+1
          IAS=LEDGE(3,I)
          JAS=LEDGE(4,I)
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
          END IF
          IBUF(CMPT) = IS
          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
        ENDIF
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I1) 
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I2) 
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)

        ENDIF
      ENDDO

      NB_BOUNDARY_EDGES_REMOTE = 0
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        K1=SEGLOC(E1)
        E2=LEDGE(3,I)
        IF(E2/=0)THEN
! not free edge
          K2=SEGLOC(E2)
        ELSE
! Free edge
          K2=-1
        END IF
        IF( K1 ==  0 .AND. K2 > 0) THEN
          NB_BOUNDARY_EDGES_REMOTE = NB_BOUNDARY_EDGES_REMOTE + 1
          TAG_EDGE(ID) = I
          ID = ID + 1
! if at least one segment belongs to this domain
! Put the local segment first
          CMPT=CMPT+1
          IBUF(CMPT) = K2                 !1
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(4,I)         !2
          CMPT=CMPT+1
C         IBUF(CMPT) = -MSEGLO(E1)
          IBUF(CMPT) = -K2                 !3
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(2,I)         !4
C         IBUF(CMPT) = LEDGE(4,I)
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(5,I)) !5
          CMPT=CMPT+1
          IBUF(CMPT) = NODLOCAL(LEDGE(6,I)) !6
          CMPT=CMPT+1
          IBUF(CMPT) = LEDGE(7,I)          !7
          CMPT=CMPT+1
          IBUF(CMPT) = I !8
          CMPT=CMPT+1
          IBUF(CMPT) = 0 !9 
C orientation  segment 1
          CMPT=CMPT+1
          IAS=LEDGE(3,I) ! segments have been switched
          JAS=LEDGE(4,I) ! The first one is 3-4 
          N1=LEDGE(5,I)
          N2=LEDGE(6,I)
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
        ENDIF
          IBUF(CMPT) = IS !10 
          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
          END IF
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I1)!11 
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I2) !12
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)

C orientation segment 2
          CMPT=CMPT+1
          IAS=LEDGE(1,I) ! segments have been switched
          JAS=LEDGE(2,I) ! the second one is 1-2
          IF(IRECTM(JAS,IAS)==N1.AND.IRECTM(MOD(JAS,4)+1,IAS)==N2)THEN
            IS= 1
          ELSEIF(IRECTM(JAS,IAS)==N2.AND.IRECTM(MOD(JAS,4)+1,IAS)==N1)THEN
            IS=-1
          ELSE
            ASSERT(.FALSE.)
        ENDIF
          IBUF(CMPT) = IS ! 13
          IF(IS==1)THEN
            I1=ADMSR(JAS,IAS)
            I2=ADMSR(MOD(JAS,4)+1,IAS)
          ELSE ! IM(I)==-1
            I2=ADMSR(JAS,IAS)
            I1=ADMSR(MOD(JAS,4)+1,IAS)
          END IF
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I1) !14 
          CMPT = CMPT +1 
          IBUF(CMPT) = TAG_SM(I2) !15
          ASSERT(TAG_SM(I1) > 0)
          ASSERT(TAG_SM(I2) > 0)

        ENDIF
      ENDDO


C      WRITE(6,*) __FILE__,"NEDGE_L",NEDGE_L
C      WRITE(6,*) "NB_FREE_EDGES=",NB_FREE_EDGES
C      WRITE(6,*) "NB_INTERNAL_EDGES=",NB_INTERNAL_EDGES
C      WRITE(6,*) "NB_BOUNDARY_EDGES_LOCAL=",NB_BOUNDARY_EDGES_LOCAL
C      WRITE(6,*) "NB_BOUNDARY_EDGES_REMOTE=",NB_BOUNDARY_EDGES_REMOTE



      I = NB_FREE_EDGES+NB_INTERNAL_EDGES+NB_BOUNDARY_EDGES_LOCAL + NB_BOUNDARY_EDGES_REMOTE
      ASSERT(NEDGE_L == I)

      CALL WRITE_I_C(IBUF,NLEDGE*NEDGE_L)
      DEALLOCATE(IBUF)

      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_CAND_I25                source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_CAND_I25(PROC     , INTBUF_TAB, NSN    , NSN_L       ,
     .                         TAG_SEGM2, II_STOK   , MULTIMP, NCONT        ,
     .                         NOINT    , INACTI    , TAG_SCRATCH ,II_STOK_L,
     .                         NINDX_SCRT,INDX_SCRT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC,NSN,NSN_L,II_STOK,MULTIMP,NCONT,
     .        TAG_SEGM2(*),NOINT,INACTI,
     .        TAG_SCRATCH(*) , II_STOK_L, ITYP   
      INTEGER, INTENT(INOUT) :: NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_SCRT

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,P,E,MULTOK,MSGID,
     .        SPLIST,C_NSNR
      INTEGER NUMP(NSPMD),WORK(70000)

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF_E,IBUF_N,NSNLOCAL,CPULOCAL,CANDR,PLIST,
     .  INDEX

      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI
C ----------------------------------------
C       w to be done :modif w/ edge
C ----------------------------------------
      ALLOCATE(IBUF_E(MULTIMP*NCONT),IBUF_N(MULTIMP*NCONT))
      IBUF_E(1:MULTIMP*NCONT) = 0
      IBUF_N(1:MULTIMP*NCONT) = 0
      II_STOK_L = 0 !mandatory in case of inacti ne 5,6,7 

        IF(NSN>0) THEN
          ALLOCATE(NSNLOCAL(NSN))
          ALLOCATE(CPULOCAL(NSN))
          ALLOCATE(CANDR(NSN))
        END IF

        NUMP(1:NSPMD) = 0

        ALLOCATE(PLIST(NSPMD)) 
        PLIST(1:NSPMD) = -1
        DO K=1,NSN
          N = INTBUF_TAB%NSV(K)
          NSNLOCAL(K) = 0
          IF(TAG_SCRATCH(N)==0) THEN 
            SPLIST=0 
            CALL PLIST_IFRONT(PLIST,N,SPLIST) 
            DO I=1,SPLIST
              P=PLIST(I)
              NUMP(P) = NUMP(P)+1
            ENDDO
            IF(NLOCAL(N,PROC+1)==1) THEN
              NSNLOCAL(K) = NUMP(PROC+1)
              CPULOCAL(K) = PROC+1
            ELSE
              P = PLIST(1)
              NSNLOCAL(K) = NUMP(P)
              CPULOCAL(K) = P
            ENDIF
            TAG_SCRATCH(N) = 1
            NINDX_SCRT = NINDX_SCRT + 1
            INDX_SCRT(NINDX_SCRT) = N
          ENDIF
        ENDDO
        DEALLOCATE(PLIST) 

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Reperage des candidats se trouvant sur des procs distants
C
        C_NSNR = 0

        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            N = INTBUF_TAB%CAND_N(K)
            IF (INTBUF_TAB%NSV(N)> NUMNOD) CYCLE
            IF(TAG_SCRATCH(N)==0) THEN
              TAG_SCRATCH(N) = 1
              NINDX_SCRT = NINDX_SCRT + 1
              INDX_SCRT(NINDX_SCRT) = N
              IF(NLOCAL(INTBUF_TAB%NSV(N),PROC+1)/=1)THEN
                C_NSNR = C_NSNR + 1
                CANDR(C_NSNR) = N
              END IF
            END IF
          ENDIF
        ENDDO

        !reflush TAG_SCRATCH to zero only when value has changes
#include "vectorize.inc"
        DO K=1,NINDX_SCRT  
          N = INDX_SCRT(K)
          TAG_SCRATCH(N) = 0
        ENDDO
        NINDX_SCRT = 0
C
C Tris des candidats remote par proc et par nsv local croissant
C
C       IF(C_NSNR>0) THEN
          ALLOCATE(INDEX(2*C_NSNR))
          ALLOCATE(ITRI(2,C_NSNR))
C       END IF
        DO I = 1, C_NSNR
          N = CANDR(I)
          ITRI(1,I) = CPULOCAL(N)
          ITRI(2,I) = NSNLOCAL(N)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,C_NSNR,2)
C
        DO I = 1, C_NSNR
          INDEX(C_NSNR+INDEX(I)) = I
        ENDDO
        DO I = 1, C_NSNR
          INDEX(I)=INDEX(C_NSNR+I)
        ENDDO
C
        II_STOK_L = 0

        C_NSNR = 0
        DO K = 1, II_STOK
          E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            II_STOK_L = II_STOK_L + 1
          END IF
        END DO

        IF(II_STOK_L>MULTIMP*NCONT)THEN
          MULTOK= II_STOK_L/NCONT
          CALL ANCMSG(MSGID=626,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MULTOK,
     .                I2=NOINT)
        ELSE
          II_STOK_L = 0
C
          DO K = 1, II_STOK
            E = INTBUF_TAB%CAND_E(K)
            IF (TAG_SEGM2(E)/=0) THEN
              N = INTBUF_TAB%CAND_N(K)
              II_STOK_L = II_STOK_L + 1
              IBUF_E(II_STOK_L)=TAG_SEGM2(E)
              IF (INTBUF_TAB%NSV(N)>NUMNOD) THEN
                IBUF_N(II_STOK_L)=N
              ELSEIF(NLOCAL(INTBUF_TAB%NSV(N),PROC+1)==1) THEN
                IBUF_N(II_STOK_L)=NSNLOCAL(N)
              ELSE
C noeud remote : numerotation pre calculee ci-dessus
c                IF(TAG(N)==0) THEN
                IF(TAG_SCRATCH(N)==0) THEN
                  C_NSNR = C_NSNR + 1
                  IBUF_N(II_STOK_L)=INDEX(C_NSNR)+NSN_L
                  TAG_SCRATCH(N) = INDEX(C_NSNR)+NSN_L
                  NINDX_SCRT = NINDX_SCRT + 1
                  INDX_SCRT(NINDX_SCRT) = N
                ELSE
                  IBUF_N(II_STOK_L) = TAG_SCRATCH(N)
                END IF ! TAG(N)==0
              END IF ! NLOCAL(INTBUF_TAB%NSV(N),PROC+1)==1
            ENDIF !TAG_SEGM_2(E)/=0
          ENDDO !K = 1, II_STOK
        END IF !II_STOK_L>MULTIMP*NCONT

        !reflush TAG_SCRATCH to zero only when value has changes
        DO K=1, II_STOK  
              E = INTBUF_TAB%CAND_E(K)
          IF (TAG_SEGM2(E)/=0) THEN
            N = INTBUF_TAB%CAND_N(K)
            IF (INTBUF_TAB%NSV(N)<= NUMNOD) TAG_SCRATCH(N) = 0
          ENDIF
        ENDDO
  
        IF(NSN>0) DEALLOCATE(NSNLOCAL,CPULOCAL,CANDR)
        DEALLOCATE(INDEX,ITRI)

         
      CALL WRITE_I_C(IBUF_E,MULTIMP*NCONT)
      CALL WRITE_I_C(IBUF_N,MULTIMP*NCONT)

      DEALLOCATE(IBUF_E,IBUF_N)

      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_ADSKYN_25               source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_ADSKYN_25(ADSKYN,IADNOR,NADMSR,ADMSR,NADMSR_L,
     .                           NRTM_L,TAG_SEGM,TAG_SM,INTERCEP)
c
c build & write skyline matrix for normals assembling
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADSKYN(NADMSR+1),IADNOR(4,*),NADMSR,NADMSR_L,NRTM_L,
     .        ADMSR(4,*), TAG_SEGM(*),TAG_SM(*)
      TYPE(INTERSURFP)     :: INTERCEP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IS,ISL
      INTEGER, DIMENSION(:),ALLOCATABLE :: ADSKYN_L,IADNOR_L, TAG_MS
C ----------------------------------------
      ALLOCATE(ADSKYN_L(NADMSR_L+1),IADNOR_L(4*NRTM_L),TAG_MS(NADMSR_L))

      TAG_MS(1:NADMSR_L)=0
      DO I=1, NADMSR
        K=TAG_SM(I)
        IF(K /= 0) TAG_MS(K) = I
      END DO 

      ADSKYN_L(1)=1
      DO K=1, NADMSR_L
        I=TAG_MS(K)
        ADSKYN_L(K+1)=ADSKYN_L(K)+ADSKYN(I+1)-ADSKYN(I)
      END DO 

      DO I=1, NRTM_L
        K=TAG_SEGM(I)
        DO J=1,4
          IS =ADMSR(J,K)
          ISL=TAG_SM(IS)
          IADNOR_L(4*(I-1)+J) = IADNOR(J,K) - ADSKYN(IS) + ADSKYN_L(ISL)
        ENDDO
      ENDDO

      CALL WRITE_I_C(ADSKYN_L,NADMSR_L+1)
      CALL WRITE_I_C(IADNOR_L,4*NRTM_L)

      DEALLOCATE(ADSKYN_L,IADNOR_L,TAG_MS)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_LBOUND_I25              source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_LBOUND_I25(NADMSR,NADMSR_L,LBOUND,TAG_SM)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NADMSR,NADMSR_L,LBOUND(*), TAG_SM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF, TAG_MS
C ----------------------------------------
      ALLOCATE(TAG_MS(NADMSR_L))

      TAG_MS(1:NADMSR_L)=0
      DO I=1, NADMSR
        K=TAG_SM(I)
        IF(K /= 0) TAG_MS(K) = I
      END DO 
C ----------------------------------------
      ALLOCATE(IBUF(NADMSR_L))

      DO I=1, NADMSR_L
        K=TAG_MS(I)
        IF(K/=0) THEN
          IBUF(I) = LBOUND(K)
        END IF
      ENDDO

      CALL WRITE_I_C(IBUF,NADMSR_L)
      DEALLOCATE(IBUF)

      RETURN
      END
C=======================================================================
C END SPECIFIC ROUTINES INT25
C=======================================================================
Chd|====================================================================
Chd|  SPLIT_ISEGPT_IVAL             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_ISEGPT_IVAL(ISEGPT,NSN_L,DIM2,TAG_NODE_2RY,NI,TAG_2RY_INV,PROC)
c
c split & write node array (type INTEGER) with global value
c (see SPLIT_NODE_NODLOC for local values)
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISEGPT(*),TAG_NODE_2RY(*),NSN_L,DIM2,NI,PROC,
     *        TAG_2RY_INV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SN,FICT_SN
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF
C ----------------------------------------
      ALLOCATE(IBUF(NSN_L))

      DO I=1, NSN_L
        K=TAG_NODE_2RY(I)
        DO J=1,DIM2
             IBUF(I) = 0
          IF(ISEGPT(K)==K)THEN
             IBUF(I) = I
          ELSEIF(-ISEGPT(K)==K)THEN
             IBUF(I) = -I
          ELSE
             SN = ISEGPT(K)
            IF(SN==0)THEN
             IBUF(I) = SN
            ELSE
             FICT_SN = TAG_2RY_INV(SN)
             IBUF(I) = FICT_SN
            ENDIF
          ENDIF
          
        ENDDO
      ENDDO

      CALL WRITE_I_C(IBUF,NSN_L*DIM2)

      DEALLOCATE(IBUF)

      RETURN
      END
Chd|====================================================================
Chd|  SPLIT_REMNODE_I24             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_REMNODE_I24(PROC     , INTBUF_TAB, NRTM    , NRTM_L,
     .                            TAG_SEGM2, NREMNODE  , NODLOCAL, ITAB   ,
     .                             IS2ID   ,INTERCEP2  ,NSNE ,NODLOCAL24)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
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
      INTEGER PROC,NRTM,NRTM_L,
     .        TAG_SEGM2(*),NREMNODE,NODLOCAL(*),
     .        ITAB(*),IS2ID(*),NSNE,NODLOCAL24(*)
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .        SIZ_TMP

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(INTERSURFP)     :: INTERCEP2
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SIZ,L,SIZ1,SIZ2,M,N,SE1,NS,NUMNODT

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .  IBUF1,IBUF2,NODDEL,NODDELREMOTE
C ----------------------------------------
      ALLOCATE(SIZ_TMP(NRTM),NODDEL(NUMNOD+NSNE),
     .         NODDELREMOTE(NUMNOD+NSNE))

      ALLOCATE(IBUF1(2*(NRTM_L + 1)), IBUF2(NREMNODE))
      IBUF1(1:2*(NRTM_L+1)) = 0
      IBUF2(1:NREMNODE) = 0

      SIZ_TMP(1:NRTM) = 0

      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN
          SIZ_TMP(TAG_SEGM2(K)) = INTBUF_TAB%KREMNODE(K+1)
     .              -INTBUF_TAB%KREMNODE(K)
        ENDIF
      END DO

      IBUF1(1) = 0
      NUMNODT = NUMNOD + NSNE
      NODDEL(1:NUMNODT) = 0
      NODDELREMOTE(1:NUMNODT) = 0
      SIZ1 = 0
      SIZ2 = 0
      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN

          SIZ = SIZ_TMP(TAG_SEGM2(K))
          IBUF1(1+2*TAG_SEGM2(K)) =IBUF1(1+2*(TAG_SEGM2(K)-1)) + SIZ
          L=INTBUF_TAB%KREMNODE(K)
          SIZ1 = 0
          SIZ2 = 0
c--------add if N<=NUMNOD else nodlocal_fictive
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M)
           IF (N>NUMNOD) THEN
            NS = N-NUMNOD
            SE1 = INTBUF_TAB%IS2SE(2*(NS-1)+1)
            IF (INTERCEP2%P(SE1)==PROC+1)THEN
              NODDEL(SIZ1+1) = NODLOCAL24(N)
              SIZ1 = SIZ1+1
            ENDIF
           ELSE
            IF(NLOCAL(N,PROC+1)==1) THEN
              NODDEL(SIZ1+1) = NODLOCAL(N)
              SIZ1 = SIZ1+1
            ENDIF
           END IF
          ENDDO
c--------add if N<=NUMNOD else IS2ID(N-NUMNOD)
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M)
           IF (N>NUMNOD) THEN
            NS = N-NUMNOD
            SE1 = INTBUF_TAB%IS2SE(2*(NS-1)+1)
            IF (INTERCEP2%P(SE1)/=PROC+1)THEN
              NODDELREMOTE(SIZ2+1) = IS2ID(NS)
              SIZ2 = SIZ2+1
            ENDIF
           ELSE
            IF(NLOCAL(N,PROC+1)/=1) THEN
              NODDELREMOTE(SIZ2+1) = ITAB(N)
              SIZ2 = SIZ2+1
            ENDIF
           END IF
          ENDDO
c
          L=IBUF1(1+2*(TAG_SEGM2(K)-1))
          DO M=1,SIZ1
            IBUF2(1+L+M-1)= NODDEL(M)
          ENDDO
c
          IBUF1(1+2*(TAG_SEGM2(K)-1)+1) = L + SIZ1
          L=IBUF1(1+2*(TAG_SEGM2(K)-1)+1)
          DO M=1,SIZ2
            IBUF2(1+L+M-1) = - NODDELREMOTE(M)
          ENDDO
        ENDIF
        DO M=1,SIZ1
          NODDEL(M) = 0
        ENDDO
        DO M=1,SIZ2
          NODDELREMOTE(M) = 0
        ENDDO
      ENDDO
c      print *,'NREMNODE,SIZ1,SIZ2=',NREMNODE,SIZ1,SIZ2

      DEALLOCATE(SIZ_TMP,NODDEL,NODDELREMOTE)

      CALL WRITE_I_C(IBUF1,2*(NRTM_L + 1))
      CALL WRITE_I_C(IBUF2,NREMNODE)

      DEALLOCATE(IBUF1, IBUF2)

      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_REMNODE_I11             source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_REMNODE_I11(PROC     , INTBUF_TAB, NRTM    , NRTM_L,
     .                            TAG_SEGM2, NODLOCAL, ITAB  ,NUMNOD_L, TAG_SEGS2,
     .                            NREMNODE_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC,NRTM,NRTM_L,
     .        TAG_SEGM2(*),NREMNODE,NODLOCAL(*),
     .        ITAB(*),TAG_SEGS2(*)
      INTEGER, INTENT(IN) :: NUMNOD_L,NREMNODE_L

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SIZ,
     .        L,SIZ1,SIZ2,M,N,CPT_L,INDEX1

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .        IBUF1,IBUF2
C ----------------------------------------
C
C
      ALLOCATE(IBUF1(2*(NRTM_L + 1)), IBUF2(NREMNODE_L))
      IBUF1(1:2*(NRTM_L+1)) = 0
      IBUF2(1:NREMNODE_L) = 0
C
C---  Split of KERMNODE -> IBUF1
      INDEX1 = 1
      CPT_L = 0
      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN
          SIZ = INTBUF_TAB%KREMNODE(K+1)-INTBUF_TAB%KREMNODE(K)
          L=INTBUF_TAB%KREMNODE(K)
          SIZ1 = 0
          SIZ2 = 0
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M-1)
            IF (TAG_SEGS2(N)/=0) THEN
C--           Local segment - local id is stored
              SIZ1 = SIZ1 + 1
            ELSE
C--           Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
              SIZ2 = SIZ2 + 2
            ENDIF
          END DO
          CPT_L = CPT_L + 1
          IBUF1(2*(CPT_L-1)+1) = INDEX1
          IBUF1(2*(CPT_L-1)+2) = INDEX1 + SIZ1
          INDEX1 = INDEX1 + SIZ1 + SIZ2
        ENDIF
      END DO
      IBUF1(2*NRTM_L+1) = INDEX1
      IBUF1(2*NRTM_L+2) = INDEX1
C
C---  Split of ERMNODE -> IBUF2
      CPT_L = 0
      DO K=1,NRTM
        IF(TAG_SEGM2(K) /= 0)THEN
C
          CPT_L = CPT_L + 1
          L=INTBUF_TAB%KREMNODE(K)
          SIZ = INTBUF_TAB%KREMNODE(K+1)-INTBUF_TAB%KREMNODE(K)
          SIZ1 = IBUF1(2*(CPT_L-1)+1)
          SIZ2 = IBUF1(2*(CPT_L-1)+2)
C
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE(L+M-1)
            IF (TAG_SEGS2(N)/=0) THEN
C-- Local segment - local id is stored
              IBUF2(SIZ1) = TAG_SEGS2(N)
              SIZ1 = SIZ1+1
            ELSE
C-- Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
              IBUF2(SIZ2) = ITAB(INTBUF_TAB%IRECTS(2*(N-1)+1))
              IBUF2(SIZ2+1) = ITAB(INTBUF_TAB%IRECTS(2*(N-1)+2))
              SIZ2 = SIZ2+2
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IBUF1,2*(NRTM_L + 1))
      CALL WRITE_I_C(IBUF2,NREMNODE_L)
C
      DEALLOCATE(IBUF1, IBUF2)
C
      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_REMNODE_I25_EDGE        source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_REMNODE_I25_EDGE(PROC     , INTBUF_TAB, NEDGE    , NEDGE_L,
     .                                 TAG_EDGE  , TAG_EDGE2 , SEGLOC   ,ITAB    ,
     .                                 NUMNOD_L  ,  NREMNODE_EDG_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER PROC,NEDGE,NEDGE_L,
     .        TAG_EDGE2(*),TAG_EDGE(*),SEGLOC(*),
     .        ITAB(*)
      INTEGER, INTENT(IN) :: NUMNOD_L,NREMNODE_EDG_L

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SIZ,
     .        L,SIZ1,SIZ2,M,N,CPT_L,INDEX1,IK,
     .        KM1,EM1,KM2,EM2,KS1,ES1,KS2,ES2

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .        IBUF1,IBUF2
C ----------------------------------------
C
C
      ALLOCATE(IBUF1(2*(NEDGE_L + 1)), IBUF2(NREMNODE_EDG_L))
      IBUF1(1:2*(NEDGE_L+1)) = 0
      IBUF2(1:NREMNODE_EDG_L) = 0
C
C---  Split of KERMNODE -> IBUF1
 
      INDEX1 = 1
      CPT_L = 0
      DO IK=1,NEDGE_L
        K = TAG_EDGE(IK)
        EM1=INTBUF_TAB%LEDGE(1+(K-1)*NLEDGE)
        KM1=0
        IF(EM1/=.0) KM1=SEGLOC(EM1)
        EM2=INTBUF_TAB%LEDGE(3+(K-1)*NLEDGE)
        KM2=0
        IF(EM2/=0) KM2=SEGLOC(EM2)
        IF(KM1 /= 0.OR.KM2/=0)THEN
          SIZ = INTBUF_TAB%KREMNODE_EDG(K+1)-INTBUF_TAB%KREMNODE_EDG(K)
          L=INTBUF_TAB%KREMNODE_EDG(K)
          SIZ1 = 0
          SIZ2 = 0
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE_EDG(L+M-1)
            ES1=INTBUF_TAB%LEDGE(1+(N-1)*NLEDGE)
            KS1=0
            IF(ES1/=0) KS1=SEGLOC(ES1)
            ES2=INTBUF_TAB%LEDGE(3+(N-1)*NLEDGE)
            KS2 = 0
            IF(ES2/=0) KS2=SEGLOC(ES2)
            IF (KM1 /= 0.AND.KM2/=0.AND.KS1/=0.AND.KS2/=0) THEN
C--           Local segment - local id is stored
              SIZ1 = SIZ1 + 1
            ELSE
C--           Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
              SIZ2 = SIZ2 + 2
            ENDIF
          END DO
          CPT_L = CPT_L + 1
          IBUF1(2*(CPT_L-1)+1) = INDEX1
          IBUF1(2*(CPT_L-1)+2) = INDEX1 + SIZ1
          INDEX1 = INDEX1 + SIZ1 + SIZ2
        ENDIF
      END DO
      IBUF1(2*NEDGE_L+1) = INDEX1
      IBUF1(2*NEDGE_L+2) = INDEX1

C
C---  Split of ERMNODE -> IBUF2
      CPT_L = 0
      DO IK=1,NEDGE_L
        K = TAG_EDGE(IK)
        EM1=INTBUF_TAB%LEDGE(1+(K-1)*NLEDGE)
        KM1=0
        IF(EM1/=0) KM1=SEGLOC(EM1)
        EM2=INTBUF_TAB%LEDGE(3+(K-1)*NLEDGE)
        KM2=0
        IF(EM2/=0) KM2=SEGLOC(EM2)
        IF(KM1 /= 0.OR.KM2/=0)THEN
C
          CPT_L = CPT_L + 1
          L=INTBUF_TAB%KREMNODE_EDG(K)
          SIZ = INTBUF_TAB%KREMNODE_EDG(K+1)-INTBUF_TAB%KREMNODE_EDG(K)
          SIZ1 = IBUF1(2*(CPT_L-1)+1)
          SIZ2 = IBUF1(2*(CPT_L-1)+2)
C
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE_EDG(L+M-1)
            KS1=0
            ES1=INTBUF_TAB%LEDGE(1+(N-1)*NLEDGE)
            IF(ES1/=0) KS1=SEGLOC(ES1)
            ES2=INTBUF_TAB%LEDGE(3+(N-1)*NLEDGE)
            KS2 = 0
            IF(ES2/=0) KS2=SEGLOC(ES2)
            IF (KM1 /= 0.AND.KM2/=0.AND.KS1/=0.AND.KS2/=0) THEN
C-- Local segment - local id is stored
              IBUF2(SIZ1) = TAG_EDGE2(N)
              SIZ1 = SIZ1+1
            ELSE
C-- Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
              IBUF2(SIZ2) = ITAB(INTBUF_TAB%LEDGE(5+(N-1)*NLEDGE))
              IBUF2(SIZ2+1) = ITAB(INTBUF_TAB%LEDGE(6+(N-1)*NLEDGE))
              SIZ2 = SIZ2+2
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IBUF1,2*(NEDGE_L + 1))
      CALL WRITE_I_C(IBUF2,NREMNODE_EDG_L)
C
      DEALLOCATE(IBUF1, IBUF2)
C
      RETURN
      END

Chd|====================================================================
Chd|  SPLIT_REMNODE_I25_E2S         source/restart/ddsplit/inter_tools.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_REMNODE_I25_E2S(PROC     , INTBUF_TAB, NRTM    , NRTM_L,
     .                                 TAG_EDGE  , TAG_EDGE2 , SEGLOC   ,ITAB    ,
     .                                 NUMNOD_L  ,  NREMNODE_E2S_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
      INTEGER PROC,NRTM,NRTM_L,
     .        TAG_EDGE2(*),TAG_EDGE(*),SEGLOC(*),
     .        ITAB(*)
      INTEGER, INTENT(IN) :: NUMNOD_L,NREMNODE_E2S_L

      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,SIZ,
     .        L,SIZ1,SIZ2,M,N,CPT_L,INDEX1,IK,
     .        KM1,EM1,KM2,EM2,KS1,ES1,KS2,ES2

      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .        IBUF1,IBUF2
C ----------------------------------------
C
C
      ALLOCATE(IBUF1(2*(NRTM_L + 1)), IBUF2(NREMNODE_E2S_L))
      IBUF1(1:2*(NRTM_L+1)) = 0
      IBUF2(1:NREMNODE_E2S_L) = 0
C
C---  Split of KERMNODE -> IBUF1
 
      INDEX1 = 1
      CPT_L = 0
      DO K=1,NRTM
        IF(SEGLOC(K) >  0) THEN  
          SIZ = INTBUF_TAB%KREMNODE_E2S(K+1)-INTBUF_TAB%KREMNODE_E2S(K)
          L=INTBUF_TAB%KREMNODE_E2S(K)
          SIZ1 = 0
          SIZ2 = 0
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE_E2S(L+M-1)
            ES1=INTBUF_TAB%LEDGE(1+(N-1)*NLEDGE)
            IF(SEGLOC(ES1) >  0) THEN  
C--           Local segment - local id is stored
              SIZ1 = SIZ1 + 1
            ELSE
C--           Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
              SIZ2 = SIZ2 + 2
            ENDIF
          END DO
          CPT_L = CPT_L + 1
          IBUF1(2*(CPT_L-1)+1) = INDEX1
          IBUF1(2*(CPT_L-1)+2) = INDEX1 + SIZ1
          INDEX1 = INDEX1 + SIZ1 + SIZ2
        ENDIF
      END DO
      IBUF1(2*NRTM_L+1) = INDEX1
      IBUF1(2*NRTM_L+2) = INDEX1

C
C---  Split of REMNODE -> IBUF2
      CPT_L = 0
      DO K=1,NRTM
        IF(SEGLOC(K) >  0) THEN  
C
          CPT_L = CPT_L + 1
          L=INTBUF_TAB%KREMNODE_E2S(K)
          SIZ = INTBUF_TAB%KREMNODE_E2S(K+1)-INTBUF_TAB%KREMNODE_E2S(K)
          SIZ1 = IBUF1(2*(CPT_L-1)+1)
          SIZ2 = IBUF1(2*(CPT_L-1)+2)
C
          DO M=1,SIZ
            N = INTBUF_TAB%REMNODE_E2S(L+M-1)
            ES1=INTBUF_TAB%LEDGE(1+(N-1)*NLEDGE)
            IF(SEGLOC(ES1) >  0) THEN  
C-- Local segment - local id is stored
              IBUF2(SIZ1) = TAG_EDGE2(N)
              SIZ1 = SIZ1+1
            ELSE
C-- Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
              IBUF2(SIZ2) = ITAB(INTBUF_TAB%LEDGE(5+(N-1)*NLEDGE))
              IBUF2(SIZ2+1) = ITAB(INTBUF_TAB%LEDGE(6+(N-1)*NLEDGE))
              SIZ2 = SIZ2+2
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IBUF1,2*(NRTM_L + 1))
      CALL WRITE_I_C(IBUF2,NREMNODE_E2S_L)
C
      DEALLOCATE(IBUF1, IBUF2)
C
      RETURN
      END
