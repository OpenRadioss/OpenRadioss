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
Chd|  WRITE_INTBUF                  source/output/restart/write_intbuf.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_INTBUF_T8               source/output/restart/write_intbuf.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_INTBUF(INTBUF_TAB)  
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c      my_real
c     .   
      INTEGER, DIMENSION(:),ALLOCATABLE :: INTBUF_SIZE

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NI,SIZ,L_INTBUF_SIZE
C=======================================================================
      ALLOCATE(INTBUF_SIZE(L_INTBUF_SIZE_MAX))
      INTBUF_SIZE(1:L_INTBUF_SIZE_MAX) = 0

      DO NI= 1, NINTER

        N = 1
C=======================================================================
C integer arrays sizes
C=======================================================================
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRECTS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRECTM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NSV
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRTLM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRUPT
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_INORM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IELEC
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IELES
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LISUB
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_TYPSUB
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADDSUBS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADDSUBM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LISUBS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LISUBM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_INFLG_SUBS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_INFLG_SUBM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADDSUBE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LISUBE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_INFLG_SUBE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSEGTYP
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_N
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_I_STOK
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_I_STOK_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IFPEN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KREMNODE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_REMNODE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KREMNOR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_REMNOR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADCCM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CHAIN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NIGE
!type20
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DAANC6
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NBINFLG
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MBINFLG
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_EBINFLG
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NLG
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISLINS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISLINM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IXLINS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IXLINM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NSVL
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSRL
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LCAND_N
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LCAND_S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADCCM20
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CHAIN20
!type1
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ILOCS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NSEGM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NRT
!type2
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSEGTYP2
!type3
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRTLS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ILOCM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRTLOM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRTLOS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NSEGS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LNSV
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LMSR
!type4
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IELEM
!type12
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FCOUNT
!type14
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KSURF
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IMPACT
!type21
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSR21
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MNDD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSR_L
!type24
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MVOISIN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NVOISIN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSEGLO
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSEGTYP24
C------- to be cleaned later        
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISEADD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISEDGE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_T
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISEG_PXFEM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISEG_PLY
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ICONT_I
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IRTSE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IS2SE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IS2PT
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISPT2
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISEGPT
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IS2ID 
!type25
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_EVOISIN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADMSR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LEDGE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LBOUND
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ACTNOR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FARM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADSKYN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IADNOR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ISLIDE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KNOR2MSR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NOR2MSR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_OPT_N
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_OPT_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IF_ADH
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CANDM_E2E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CANDS_E2E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CANDM_E2S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CANDS_E2S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IFPEN_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IFPEN_E2S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IPARTFRICS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IPARTFRICM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IPARTFRIC_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IELNRTS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ADRECTS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FACNRTS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IREP_FRICM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_E2S_ACTNOR  
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KREMNODE_EDG  
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_REMNODE_EDG 
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KREMNODE_E2S  
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_REMNODE_E2S   

C=======================================================================
C real arrays sizes
C=======================================================================
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFAC
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_VARIABLES
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CSTS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DPARA
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NMAS 
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_AREAS2
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_SMAS 
        N = N+1
        INTBUF_SIZE(N) =  INTBUF_TAB(NI)%S_SINER
        N = N+1
        INTBUF_SIZE(N) =  INTBUF_TAB(NI)%S_UVAR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_XM0
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_SPENALTY
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFR_PENALTY
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_SKEW
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DSM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FSM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_RUPT
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FINI
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFNS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENIM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENIS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_M
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_XSAV
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CRIT
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FRIC_P
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_XFILTR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_AREAS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_AREAM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_ML
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_SL
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_P
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_PS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAPE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_E_L
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVX
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVY
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVZ
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_RIGE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_XIGE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_VIGE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MASSIGE
!type10
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_F
!type20
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_XA
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_VA
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFA
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_AVX_ANCR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_SH
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_FX
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_FY
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CAND_FZ
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_SE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_ME
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STF
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFNE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CRITX
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENISE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENIME
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENIA
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ALPHAK
        N = N+1
!type1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_N
!type 3,4,5,9
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CSTM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_EE
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STFNM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FRICOS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FRICOM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAV
!type 6
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FCONT
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_RMAS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ANSMX0
!type 8
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_T8
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAPN
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STF8
!type 14
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CIMP
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NIMP
!type 15
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_IOLD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_HOLD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NOLD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DOLD
!type 17
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_KM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FROTS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FROTM
!type 21 
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NOD_NORMAL
!type21
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_RCURV
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ANGLM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FROT_P
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_ALPHA0
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_AS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_BS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_THKNOD0
!type 24
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAPN_M
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_SECND_FR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENE_OLD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_STIF_OLD
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_TIME_S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_NM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_EDGE8L2
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NOD_2RY_LGTH
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_NOD_MAS_LGTH
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAP_N0
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DGAP_NM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DGAP_M
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DELTA_PMAX_DGAP
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_XFIC
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_VFIC
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_MSFIC
!type 25
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_EDGE_BISECTOR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_PENM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DISTM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LBM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_LCM
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_VTX_BISECTOR
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVX_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVY_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVZ_E
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVX_E2S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVY_E2S
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_FTSAVZ_E2S
!type2
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_CSTS_BIS
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_DIR_FRICM
!type25
        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_GAPMSAV

        N = N+1
        INTBUF_SIZE(N) = INTBUF_TAB(NI)%S_E2S_NOD_NORMAL

C=======================================================================
C write all INTBUF_TAB arrays sizes on disk
C=======================================================================

        L_INTBUF_SIZE=N
        IF(L_INTBUF_SIZE > L_INTBUF_SIZE_MAX)THEN
          WRITE(ISTDO,'(A,/,A)') 
     .  ' ** Internal error in routine WRITE_INTBUF:',
     .  '    Hard coded value for L_INTBUF_SIZE_MAX needs to be updated'
          CALL ARRET(2)
        END IF

        CALL WRITE_I_C(L_INTBUF_SIZE,1)
        CALL WRITE_I_C(INTBUF_SIZE,L_INTBUF_SIZE)

C=======================================================================
C write INTEGER arrays on disk
C=======================================================================
        SIZ = INTBUF_TAB(NI)%S_IRECTS
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRECTS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IRECTM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRECTM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NSV
        CALL WRITE_I_C(INTBUF_TAB(NI)%NSV,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSR
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IRTLM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRTLM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IRUPT
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRUPT,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSEGTYP2
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSEGTYP2,SIZ)

        SIZ = INTBUF_TAB(NI)%S_INORM
        CALL WRITE_I_C(INTBUF_TAB(NI)%INORM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IELEC
        CALL WRITE_I_C(INTBUF_TAB(NI)%IELEC,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IELES
        CALL WRITE_I_C(INTBUF_TAB(NI)%IELES,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LISUB
        CALL WRITE_I_C(INTBUF_TAB(NI)%LISUB,SIZ)

        SIZ = INTBUF_TAB(NI)%S_TYPSUB
        CALL WRITE_I_C(INTBUF_TAB(NI)%TYPSUB,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADDSUBS
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADDSUBS,SIZ)
 
        SIZ = INTBUF_TAB(NI)%S_ADDSUBM
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADDSUBM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LISUBS
        CALL WRITE_I_C(INTBUF_TAB(NI)%LISUBS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LISUBM
        CALL WRITE_I_C(INTBUF_TAB(NI)%LISUBM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_INFLG_SUBS
        CALL WRITE_I_C(INTBUF_TAB(NI)%INFLG_SUBS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_INFLG_SUBM
        CALL WRITE_I_C(INTBUF_TAB(NI)%INFLG_SUBM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADDSUBE
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADDSUBE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LISUBE
        CALL WRITE_I_C(INTBUF_TAB(NI)%LISUBE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_INFLG_SUBE
        CALL WRITE_I_C(INTBUF_TAB(NI)%INFLG_SUBE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CAND_E
        CALL WRITE_I_C(INTBUF_TAB(NI)%CAND_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CAND_N
        CALL WRITE_I_C(INTBUF_TAB(NI)%CAND_N,SIZ)

        SIZ = INTBUF_TAB(NI)%S_I_STOK
        CALL WRITE_I_C(INTBUF_TAB(NI)%I_STOK,SIZ)

!type20
        SIZ = INTBUF_TAB(NI)%S_LCAND_N
        CALL WRITE_I_C(INTBUF_TAB(NI)%LCAND_N,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LCAND_S
        CALL WRITE_I_C(INTBUF_TAB(NI)%LCAND_S,SIZ)

!fin type20

        SIZ = INTBUF_TAB(NI)%S_I_STOK_E
        CALL WRITE_I_C(INTBUF_TAB(NI)%I_STOK_E,SIZ)


C       ! IF edges
C        SIZ = INTBUF_TAB(NI)%S_GAPE
C        IF(SIZ > 0) THEN
C          CALL WRITE_I_C(INTBUF_TAB(NI)%NB_INTERNAL_EDGES,1)
C          CALL WRITE_I_C(INTBUF_TAB(NI)%NB_BOUNDARY_EDGES_LOCAL,1)
C          CALL WRITE_I_C(INTBUF_TAB(NI)%NB_BOUNDARY_EDGES_REMOTE,1)
C        ENDIF

        SIZ = INTBUF_TAB(NI)%S_IFPEN
        CALL WRITE_I_C(INTBUF_TAB(NI)%IFPEN,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IPARTFRICS
        CALL WRITE_I_C(INTBUF_TAB(NI)%IPARTFRICS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IPARTFRICM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IPARTFRICM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IPARTFRIC_E
        CALL WRITE_I_C(INTBUF_TAB(NI)%IPARTFRIC_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IREP_FRICM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IREP_FRICM,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_KREMNODE
        CALL WRITE_I_C(INTBUF_TAB(NI)%KREMNODE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_REMNODE
        CALL WRITE_I_C(INTBUF_TAB(NI)%REMNODE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_KREMNOR
        CALL WRITE_I_C(INTBUF_TAB(NI)%KREMNOR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_REMNOR
        CALL WRITE_I_C(INTBUF_TAB(NI)%REMNOR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_KREMNODE_EDG
        CALL WRITE_I_C(INTBUF_TAB(NI)%KREMNODE_EDG,SIZ)

        SIZ = INTBUF_TAB(NI)%S_REMNODE_EDG
        CALL WRITE_I_C(INTBUF_TAB(NI)%REMNODE_EDG,SIZ)

        SIZ = INTBUF_TAB(NI)%S_KREMNODE_E2S
        CALL WRITE_I_C(INTBUF_TAB(NI)%KREMNODE_E2S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_REMNODE_E2S
        CALL WRITE_I_C(INTBUF_TAB(NI)%REMNODE_E2S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADCCM
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADCCM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CHAIN
        CALL WRITE_I_C(INTBUF_TAB(NI)%CHAIN,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NIGE
        CALL WRITE_I_C(INTBUF_TAB(NI)%NIGE,SIZ)

!type20
        SIZ = INTBUF_TAB(NI)%S_NBINFLG
        CALL WRITE_I_C(INTBUF_TAB(NI)%NBINFLG,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MBINFLG
        CALL WRITE_I_C(INTBUF_TAB(NI)%MBINFLG,SIZ)

        SIZ = INTBUF_TAB(NI)%S_EBINFLG
        CALL WRITE_I_C(INTBUF_TAB(NI)%EBINFLG,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NLG
        CALL WRITE_I_C(INTBUF_TAB(NI)%NLG,SIZ)

        SIZ = INTBUF_TAB(NI)%S_DAANC6
        CALL WRITE_I_C(INTBUF_TAB(NI)%DAANC6,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ISLINS
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISLINS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ISLINM
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISLINM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IXLINS
        CALL WRITE_I_C(INTBUF_TAB(NI)%IXLINS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IXLINM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IXLINM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSRL
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSRL,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NSVL
        CALL WRITE_I_C(INTBUF_TAB(NI)%NSVL,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADCCM20
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADCCM20,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CHAIN20
        CALL WRITE_I_C(INTBUF_TAB(NI)%CHAIN20,SIZ)
!fin type20

!type1
        SIZ = INTBUF_TAB(NI)%S_ILOCS
        CALL WRITE_I_C(INTBUF_TAB(NI)%ILOCS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NSEGM
        CALL WRITE_I_C(INTBUF_TAB(NI)%NSEGM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NRT
        CALL WRITE_I_C(INTBUF_TAB(NI)%NRT,SIZ)
!type3
        SIZ = INTBUF_TAB(NI)%S_IRTLS
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRTLS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ILOCM
        CALL WRITE_I_C(INTBUF_TAB(NI)%ILOCM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IRTLOM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRTLOM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IRTLOS
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRTLOS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NSEGS
        CALL WRITE_I_C(INTBUF_TAB(NI)%NSEGS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LNSV
        CALL WRITE_I_C(INTBUF_TAB(NI)%LNSV,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LMSR
        CALL WRITE_I_C(INTBUF_TAB(NI)%LMSR,SIZ)

!type4
        SIZ = INTBUF_TAB(NI)%S_IELEM
        CALL WRITE_I_C(INTBUF_TAB(NI)%IELEM,SIZ)

!type12
        SIZ = INTBUF_TAB(NI)%S_FCOUNT
        CALL WRITE_I_C(INTBUF_TAB(NI)%FCOUNT,SIZ)

!type14
        SIZ = INTBUF_TAB(NI)%S_KSURF
        CALL WRITE_I_C(INTBUF_TAB(NI)%KSURF,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IMPACT
        CALL WRITE_I_C(INTBUF_TAB(NI)%IMPACT,SIZ)

!type21
        SIZ = INTBUF_TAB(NI)%S_MSR21
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSR21,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MNDD
        CALL WRITE_I_C(INTBUF_TAB(NI)%MNDD,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSR_L
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSR_L,SIZ)
!type24
        SIZ = INTBUF_TAB(NI)%S_CAND_T
        CALL WRITE_I_C(INTBUF_TAB(NI)%CAND_T,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_MVOISIN
        CALL WRITE_I_C(INTBUF_TAB(NI)%MVOISIN,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NVOISIN
        CALL WRITE_I_C(INTBUF_TAB(NI)%NVOISIN,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSEGLO
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSEGLO,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSEGTYP24
        CALL WRITE_I_C(INTBUF_TAB(NI)%MSEGTYP24,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ISEADD
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISEADD,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ISEDGE
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISEDGE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ISEG_PXFEM
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISEG_PXFEM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ISEG_PLY
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISEG_PLY,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ICONT_I
        CALL WRITE_I_C(INTBUF_TAB(NI)%ICONT_I,SIZ)


        SIZ = INTBUF_TAB(NI)%S_IRTSE
        CALL WRITE_I_C(INTBUF_TAB(NI)%IRTSE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IS2SE
        CALL WRITE_I_C(INTBUF_TAB(NI)%IS2SE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IS2PT
        CALL WRITE_I_C(INTBUF_TAB(NI)%IS2PT,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ISPT2
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISPT2,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ISEGPT
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISEGPT,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_IS2ID
        CALL WRITE_I_C(INTBUF_TAB(NI)%IS2ID,SIZ)

!type25
        SIZ = INTBUF_TAB(NI)%S_EVOISIN
        CALL WRITE_I_C(INTBUF_TAB(NI)%EVOISIN,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADMSR
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADMSR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LEDGE
        CALL WRITE_I_C(INTBUF_TAB(NI)%LEDGE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_LBOUND
        CALL WRITE_I_C(INTBUF_TAB(NI)%LBOUND,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ACTNOR
        CALL WRITE_I_C(INTBUF_TAB(NI)%ACTNOR,SIZ)
        
c       SIZ = INTBUF_TAB(NI)%S_FARM
c       CALL WRITE_I_C(INTBUF_TAB(NI)%FARM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADSKYN
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADSKYN,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_IADNOR
        CALL WRITE_I_C(INTBUF_TAB(NI)%IADNOR,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ISLIDE
        CALL WRITE_I_C(INTBUF_TAB(NI)%ISLIDE,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_KNOR2MSR
        CALL WRITE_I_C(INTBUF_TAB(NI)%KNOR2MSR,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_NOR2MSR
        CALL WRITE_I_C(INTBUF_TAB(NI)%NOR2MSR,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_CAND_OPT_N
        CALL WRITE_I_C(INTBUF_TAB(NI)%CAND_OPT_N,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_CAND_OPT_E
        CALL WRITE_I_C(INTBUF_TAB(NI)%CAND_OPT_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IF_ADH
        CALL WRITE_I_C(INTBUF_TAB(NI)%IF_ADH,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CANDM_E2E
C we can avoid to write 0
        IF(SIZ > 0) SIZ = INTBUF_TAB(NI)%I_STOK_E(1)
        CALL WRITE_I_C(INTBUF_TAB(NI)%CANDM_E2E,SIZ)
        CALL WRITE_I_C(INTBUF_TAB(NI)%CANDS_E2E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CANDM_E2S
        IF(SIZ > 0) SIZ = INTBUF_TAB(NI)%I_STOK_E(2)

        CALL WRITE_I_C(INTBUF_TAB(NI)%CANDM_E2S,SIZ)
        CALL WRITE_I_C(INTBUF_TAB(NI)%CANDS_E2S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IFPEN_E
        CALL WRITE_I_C(INTBUF_TAB(NI)%IFPEN_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IFPEN_E2S
        IF(SIZ > 0) SIZ = INTBUF_TAB(NI)%I_STOK_E(2)
        CALL WRITE_I_C(INTBUF_TAB(NI)%IFPEN_E2S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_IELNRTS
        CALL WRITE_I_C(INTBUF_TAB(NI)%IELNRTS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ADRECTS
        CALL WRITE_I_C(INTBUF_TAB(NI)%ADRECTS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FACNRTS
        CALL WRITE_I_C(INTBUF_TAB(NI)%FACNRTS,SIZ)

C=======================================================================
C write REAL arrays on disk
C=======================================================================
        SIZ = INTBUF_TAB(NI)%S_STFAC
        CALL WRITE_DB(INTBUF_TAB(NI)%STFAC,SIZ)

        SIZ = INTBUF_TAB(NI)%S_VARIABLES
        CALL WRITE_DB(INTBUF_TAB(NI)%VARIABLES,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CSTS
        CALL WRITE_DB(INTBUF_TAB(NI)%CSTS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_DPARA
        CALL WRITE_DB(INTBUF_TAB(NI)%DPARA,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NMAS
        CALL WRITE_DB(INTBUF_TAB(NI)%NMAS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_AREAS2
        CALL WRITE_DB(INTBUF_TAB(NI)%AREAS2,SIZ)

        SIZ = INTBUF_TAB(NI)%S_SMAS
        CALL WRITE_DB(INTBUF_TAB(NI)%SMAS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_SINER
        CALL WRITE_DB(INTBUF_TAB(NI)%SINER,SIZ)

        SIZ = INTBUF_TAB(NI)%S_UVAR
        CALL WRITE_DB(INTBUF_TAB(NI)%UVAR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_XM0
        CALL WRITE_DB(INTBUF_TAB(NI)%XM0,SIZ)

        SIZ = INTBUF_TAB(NI)%S_SPENALTY
        CALL WRITE_DB(INTBUF_TAB(NI)%SPENALTY,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STFR_PENALTY
        CALL WRITE_DB(INTBUF_TAB(NI)%STFR_PENALTY,SIZ)

        SIZ = INTBUF_TAB(NI)%S_SKEW
        CALL WRITE_DB(INTBUF_TAB(NI)%SKEW,SIZ)

        SIZ = INTBUF_TAB(NI)%S_DSM
        CALL WRITE_DB(INTBUF_TAB(NI)%DSM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FSM
        CALL WRITE_DB(INTBUF_TAB(NI)%FSM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_RUPT
        CALL WRITE_DB(INTBUF_TAB(NI)%RUPT,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FINI
        CALL WRITE_DB(INTBUF_TAB(NI)%FINI,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STFNS
        CALL WRITE_DB(INTBUF_TAB(NI)%STFNS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STFM
        CALL WRITE_DB(INTBUF_TAB(NI)%STFM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STFS
        CALL WRITE_DB(INTBUF_TAB(NI)%STFS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_PENIM
        CALL WRITE_DB(INTBUF_TAB(NI)%PENIM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_PENIS
        CALL WRITE_DB(INTBUF_TAB(NI)%PENIS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_M
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_M,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_S
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_XSAV
        CALL WRITE_DB(INTBUF_TAB(NI)%XSAV,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CRIT
        CALL WRITE_DB(INTBUF_TAB(NI)%CRIT,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FRIC_P
        CALL WRITE_DB(INTBUF_TAB(NI)%FRIC_P,SIZ)

        SIZ = INTBUF_TAB(NI)%S_XFILTR
        CALL WRITE_DB(INTBUF_TAB(NI)%XFILTR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_AREAS
        CALL WRITE_DB(INTBUF_TAB(NI)%AREAS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_AREAM
        CALL WRITE_DB(INTBUF_TAB(NI)%AREAM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_ML
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_ML,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_SL
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_SL,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CAND_P
        CALL WRITE_DB(INTBUF_TAB(NI)%CAND_P,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CAND_PS
        IF(SIZ > 0) THEN
          SIZ =INTBUF_TAB(NI)%I_STOK_E(2)*4
          CALL WRITE_DB(INTBUF_TAB(NI)%CAND_PS,SIZ)
        ENDIF

        SIZ = INTBUF_TAB(NI)%S_GAPE
        CALL WRITE_DB(INTBUF_TAB(NI)%GAPE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_E_L
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_E_L,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STFE
        CALL WRITE_DB(INTBUF_TAB(NI)%STFE,SIZ)

!type10
        SIZ = INTBUF_TAB(NI)%S_CAND_F
        CALL WRITE_DB(INTBUF_TAB(NI)%CAND_F,SIZ)
!fin type10

        SIZ = INTBUF_TAB(NI)%S_FTSAVX
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVX,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVY
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVY,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVZ,SIZ)

        SIZ = INTBUF_TAB(NI)%S_RIGE
        CALL WRITE_DB(INTBUF_TAB(NI)%RIGE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_XIGE
        CALL WRITE_DB(INTBUF_TAB(NI)%XIGE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_VIGE
        CALL WRITE_DB(INTBUF_TAB(NI)%VIGE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MASSIGE
        CALL WRITE_DB(INTBUF_TAB(NI)%MASSIGE,SIZ)

!type20
        SIZ = INTBUF_TAB(NI)%S_CAND_FX
        CALL WRITE_DB(INTBUF_TAB(NI)%CAND_FX,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CAND_FY
        CALL WRITE_DB(INTBUF_TAB(NI)%CAND_FY,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CAND_FZ
        CALL WRITE_DB(INTBUF_TAB(NI)%CAND_FZ,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_XA
        CALL WRITE_DB(INTBUF_TAB(NI)%XA,SIZ)

        SIZ = INTBUF_TAB(NI)%S_VA
        CALL WRITE_DB(INTBUF_TAB(NI)%VA,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STFA
        CALL WRITE_DB(INTBUF_TAB(NI)%STFA,SIZ)

        SIZ = INTBUF_TAB(NI)%S_PENIA
        CALL WRITE_DB(INTBUF_TAB(NI)%PENIA,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ALPHAK
        CALL WRITE_DB(INTBUF_TAB(NI)%ALPHAK,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_SH
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_SH,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_AVX_ANCR
        CALL WRITE_DB(INTBUF_TAB(NI)%AVX_ANCR,SIZ)

        SIZ = INTBUF_TAB(NI)%S_CRITX
        CALL WRITE_DB(INTBUF_TAB(NI)%CRITX,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_GAP_ME
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_ME,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_STF
        CALL WRITE_DB(INTBUF_TAB(NI)%STF,SIZ)

        SIZ = INTBUF_TAB(NI)%S_PENIME
        CALL WRITE_DB(INTBUF_TAB(NI)%PENIME,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_SE
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_SE,SIZ)

        SIZ = INTBUF_TAB(NI)%S_PENISE
        CALL WRITE_DB(INTBUF_TAB(NI)%PENISE,SIZ)
!fin type20

!type1
        SIZ = INTBUF_TAB(NI)%S_N
        CALL WRITE_DB(INTBUF_TAB(NI)%N,SIZ)

!type 3,4,5,9
        SIZ = INTBUF_TAB(NI)%S_CSTM
        CALL WRITE_DB(INTBUF_TAB(NI)%CSTM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_EE
        CALL WRITE_DB(INTBUF_TAB(NI)%EE,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_STFNM
        CALL WRITE_DB(INTBUF_TAB(NI)%STFNM,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_FRICOS
        CALL WRITE_DB(INTBUF_TAB(NI)%FRICOS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FRICOM
        CALL WRITE_DB(INTBUF_TAB(NI)%FRICOM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAV
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAV,SIZ)

!type 6
        SIZ = INTBUF_TAB(NI)%S_FCONT
        CALL WRITE_DB(INTBUF_TAB(NI)%FCONT,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_FS
        CALL WRITE_DB(INTBUF_TAB(NI)%FS,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_FM
        CALL WRITE_DB(INTBUF_TAB(NI)%FM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_RMAS
        CALL WRITE_DB(INTBUF_TAB(NI)%RMAS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_ANSMX0
        CALL WRITE_DB(INTBUF_TAB(NI)%ANSMX0,SIZ)

!type 8

        SIZ = INTBUF_TAB(NI)%S_GAPN
        CALL WRITE_DB(INTBUF_TAB(NI)%GAPN,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_STF8
        CALL WRITE_DB(INTBUF_TAB(NI)%STF8,SIZ)

!type 14
        SIZ = INTBUF_TAB(NI)%S_CIMP
        CALL WRITE_DB(INTBUF_TAB(NI)%CIMP,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_NIMP
        CALL WRITE_DB(INTBUF_TAB(NI)%NIMP,SIZ)

!type 15
        SIZ = INTBUF_TAB(NI)%S_IOLD
        CALL WRITE_DB(INTBUF_TAB(NI)%IOLD,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_HOLD
        CALL WRITE_DB(INTBUF_TAB(NI)%HOLD,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NOLD
        CALL WRITE_DB(INTBUF_TAB(NI)%NOLD,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_DOLD
        CALL WRITE_DB(INTBUF_TAB(NI)%DOLD,SIZ)

!type 17
        SIZ = INTBUF_TAB(NI)%S_KS
        CALL WRITE_DB(INTBUF_TAB(NI)%KS,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_KM
        CALL WRITE_DB(INTBUF_TAB(NI)%KM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FROTS
        CALL WRITE_DB(INTBUF_TAB(NI)%FROTS,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_FROTM
        CALL WRITE_DB(INTBUF_TAB(NI)%FROTM,SIZ)

!
        SIZ = INTBUF_TAB(NI)%S_RCURV
        CALL WRITE_DB(INTBUF_TAB(NI)%RCURV,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ANGLM
        CALL WRITE_DB(INTBUF_TAB(NI)%ANGLM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FROT_P
        CALL WRITE_DB(INTBUF_TAB(NI)%FROT_P,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_ALPHA0
        CALL WRITE_DB(INTBUF_TAB(NI)%ALPHA0,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_AS
        CALL WRITE_DB(INTBUF_TAB(NI)%AS,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_BS
        CALL WRITE_DB(INTBUF_TAB(NI)%BS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_THKNOD0
        CALL WRITE_DB(INTBUF_TAB(NI)%THKNOD0,SIZ)

!type 24
        SIZ = INTBUF_TAB(NI)%S_GAPN_M
        CALL WRITE_DB(INTBUF_TAB(NI)%GAPN_M,SIZ)

        SIZ = INTBUF_TAB(NI)%S_SECND_FR
        CALL WRITE_DB(INTBUF_TAB(NI)%SECND_FR,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_PENE_OLD
        CALL WRITE_DB(INTBUF_TAB(NI)%PENE_OLD,SIZ)

        SIZ = INTBUF_TAB(NI)%S_STIF_OLD
        CALL WRITE_DB(INTBUF_TAB(NI)%STIF_OLD,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_TIME_S
        CALL WRITE_DB(INTBUF_TAB(NI)%TIME_S,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_GAP_NM
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_NM,SIZ)
 
        SIZ = INTBUF_TAB(NI)%S_EDGE8L2
        CALL WRITE_DB(INTBUF_TAB(NI)%EDGE8L2,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NOD_2RY_LGTH
        CALL WRITE_DB(INTBUF_TAB(NI)%NOD_2RY_LGTH,SIZ)

        SIZ = INTBUF_TAB(NI)%S_NOD_MAS_LGTH
        CALL WRITE_DB(INTBUF_TAB(NI)%NOD_MAS_LGTH,SIZ)

        SIZ = INTBUF_TAB(NI)%S_GAP_N0
        CALL WRITE_DB(INTBUF_TAB(NI)%GAP_N0,SIZ)
 
        SIZ = INTBUF_TAB(NI)%S_DGAP_NM
        CALL WRITE_DB(INTBUF_TAB(NI)%DGAP_NM,SIZ)

        SIZ = INTBUF_TAB(NI)%S_DGAP_M
        CALL WRITE_DB(INTBUF_TAB(NI)%DGAP_M,SIZ)

        SIZ = INTBUF_TAB(NI)%S_DELTA_PMAX_DGAP
        CALL WRITE_DB(INTBUF_TAB(NI)%DELTA_PMAX_DGAP,SIZ)
        
        SIZ = INTBUF_TAB(NI)%S_XFIC
        CALL WRITE_DB(INTBUF_TAB(NI)%XFIC,SIZ)

        SIZ = INTBUF_TAB(NI)%S_VFIC
        CALL WRITE_DB(INTBUF_TAB(NI)%VFIC,SIZ)

        SIZ = INTBUF_TAB(NI)%S_MSFIC
        CALL WRITE_DB(INTBUF_TAB(NI)%MSFIC,SIZ)

!type 25
c       SIZ = INTBUF_TAB(NI)%S_PENM
c       CALL WRITE_DB(INTBUF_TAB(NI)%PENM,SIZ)

c       SIZ = INTBUF_TAB(NI)%S_DISTM
c       CALL WRITE_DB(INTBUF_TAB(NI)%DISTM,SIZ)

c       SIZ = INTBUF_TAB(NI)%S_LBM
c       CALL WRITE_DB(INTBUF_TAB(NI)%LBM,SIZ)

c       SIZ = INTBUF_TAB(NI)%S_LCM
c       CALL WRITE_DB(INTBUF_TAB(NI)%LCM,SIZ)

c       SIZ = INTBUF_TAB(NI)%S_VTX_BISECTOR
c       CALL WRITE_DB(INTBUF_TAB(NI)%VTX_BISECTOR,SIZ)

!type 21 & 25
        SIZ = INTBUF_TAB(NI)%S_NOD_NORMAL
        CALL WRITE_DB(INTBUF_TAB(NI)%NOD_NORMAL,SIZ)

        IF( INTBUF_TAB(NI)%S_T8 == 1) THEN
          CALL WRITE_INTBUF_T8(INTBUF_TAB(NI)%T8)
        ENDIF
!type 2
        SIZ = INTBUF_TAB(NI)%S_CSTS_BIS
        CALL WRITE_DB(INTBUF_TAB(NI)%CSTS_BIS,SIZ)

        SIZ = INTBUF_TAB(NI)%S_DIR_FRICM
        CALL WRITE_DB(INTBUF_TAB(NI)%DIR_FRICM,SIZ)
!type25
        SIZ = INTBUF_TAB(NI)%S_GAPMSAV
        CALL WRITE_DB(INTBUF_TAB(NI)%GAPMSAV,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVX_E
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVX_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVY_E
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVY_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ_E
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVZ_E,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVX_E2S
        IF(SIZ > 0) SIZ = 4*INTBUF_TAB(NI)%I_STOK_E(2)
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVX_E2S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVY_E2S
        IF(SIZ > 0) SIZ = 4*INTBUF_TAB(NI)%I_STOK_E(2)
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVY_E2S,SIZ)

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ_E2S
        IF(SIZ > 0) SIZ = 4*INTBUF_TAB(NI)%I_STOK_E(2)
        CALL WRITE_DB(INTBUF_TAB(NI)%FTSAVZ_E2S,SIZ)

      ENDDO !NI=1,NINTER

      DEALLOCATE(INTBUF_SIZE)
C-----
      RETURN

      END SUBROUTINE WRITE_INTBUF
C-----
Chd|====================================================================
Chd|  WRITE_INTBUF_T8               source/output/restart/write_intbuf.F
Chd|-- called by -----------
Chd|        WRITE_INTBUF                  source/output/restart/write_intbuf.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_INTBUF_T8(T8)  
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INT8_STRUCT_) :: T8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NBS_INTERF,NBM_INTERF,SIZ,P2

      IF(NSPMD > 1) THEN
         SIZ = T8%S_COMM 
         CALL WRITE_I_C(SIZ,1)

         DO K=1,T8%S_COMM 
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%NUMLOC,1)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%UID,1)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%NBCOM,1)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%PROCLIST,NSPMD)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%BUF_INDEX,NSPMD)

         ENDDO
         DO P2=1,NSPMD
          NBS_INTERF = T8%BUFFER(P2)%NBSECND_TOT 
          NBM_INTERF = T8%BUFFER(P2)%NBMAIN    

          CALL WRITE_I_C(NBM_INTERF,1)
          CALL WRITE_I_C(NBS_INTERF,1)
          !nb secnds per main
          IF(NBM_INTERF > 0) THEN
            CALL WRITE_I_C(T8%BUFFER(P2)%NBSECND,NBM_INTERF)
            CALL WRITE_I_C(T8%BUFFER(P2)%MAIN_ID,NBM_INTERF)
            CALL WRITE_I_C(T8%BUFFER(P2)%MAIN_UID,NBM_INTERF)
          ELSE

          ENDIF
         ENDDO
       ENDIF

      RETURN
      END SUBROUTINE WRITE_INTBUF_T8          
