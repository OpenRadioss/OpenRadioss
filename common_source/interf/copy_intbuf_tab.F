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
Chd|  COPY_INTBUF_TAB               interf/copy_intbuf_tab.F      
Chd|-- called by -----------
Chd|        CP_IMPBUF                     engine/source/implicit/produt_v.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|====================================================================
      SUBROUTINE COPY_INTBUF_TAB(INTBUF_TAB, INTBUF_TAB_C)  
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) INTBUF_TAB, INTBUF_TAB_C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,SIZ,OLD_SIZ
C=======================================================================

! re-set to old sizes and reallocate only arrays that have potential size
! changes with MULTIMP, other sizes should not have been modified
      
        SIZ = INTBUF_TAB%S_CAND_E
        INTBUF_TAB_C%S_CAND_E = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_E)
        ALLOCATE(INTBUF_TAB_C%CAND_E(SIZ))

        SIZ = INTBUF_TAB%S_CAND_N
        INTBUF_TAB_C%S_CAND_N = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_N)
        ALLOCATE(INTBUF_TAB_C%CAND_N(SIZ))

        SIZ = INTBUF_TAB%S_IFPEN
        INTBUF_TAB_C%S_IFPEN = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%IFPEN)
        ALLOCATE(INTBUF_TAB_C%IFPEN(SIZ))

        SIZ = INTBUF_TAB%S_IPARTFRICS
        INTBUF_TAB_C%S_IPARTFRICS = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%IPARTFRICS)
        ALLOCATE(INTBUF_TAB_C%IPARTFRICS(SIZ))

        SIZ = INTBUF_TAB%S_IPARTFRICM
        INTBUF_TAB_C%S_IPARTFRICM = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%IPARTFRICM)
        ALLOCATE(INTBUF_TAB_C%IPARTFRICM(SIZ))

        SIZ = INTBUF_TAB%S_IREP_FRICM
        INTBUF_TAB_C%S_IREP_FRICM = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%IREP_FRICM)
        ALLOCATE(INTBUF_TAB_C%IREP_FRICM(SIZ))

        SIZ = INTBUF_TAB_C%S_FTSAVX
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%FTSAVX)
        OLD_SIZ = INTBUF_TAB%S_FTSAVX
        INTBUF_TAB_C%S_FTSAVX = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%FTSAVX(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_FTSAVY
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%FTSAVY)
        OLD_SIZ = INTBUF_TAB%S_FTSAVY
        INTBUF_TAB_C%S_FTSAVY = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%FTSAVY(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_FTSAVZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%FTSAVZ)
        OLD_SIZ = INTBUF_TAB%S_FTSAVZ
        INTBUF_TAB_C%S_FTSAVZ = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%FTSAVZ(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CAND_P
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_P)
        OLD_SIZ = INTBUF_TAB%S_CAND_P
        INTBUF_TAB_C%S_CAND_P = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_P(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CAND_PS
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_PS)
        OLD_SIZ = INTBUF_TAB%S_CAND_PS
        INTBUF_TAB_C%S_CAND_PS = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_PS(OLD_SIZ))


        SIZ = INTBUF_TAB_C%S_CAND_F
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_F)
        OLD_SIZ = INTBUF_TAB%S_CAND_F
        INTBUF_TAB_C%S_CAND_F = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_F(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CHAIN
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CHAIN)
        OLD_SIZ = INTBUF_TAB%S_CHAIN
        INTBUF_TAB_C%S_CHAIN = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CHAIN(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_LCAND_N
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%LCAND_N)
        OLD_SIZ = INTBUF_TAB%S_LCAND_N
        INTBUF_TAB_C%S_LCAND_N = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%LCAND_N(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_LCAND_S
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%LCAND_S)
        OLD_SIZ = INTBUF_TAB%S_LCAND_S
        INTBUF_TAB_C%S_LCAND_S = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%LCAND_S(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CHAIN20
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CHAIN20)
        OLD_SIZ = INTBUF_TAB%S_CHAIN20
        INTBUF_TAB_C%S_CHAIN20 = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CHAIN20(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CAND_FX
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_FX)
        OLD_SIZ = INTBUF_TAB%S_CAND_FX
        INTBUF_TAB_C%S_CAND_FX = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_FX(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CAND_FY
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_FY)
        OLD_SIZ = INTBUF_TAB%S_CAND_FY
        INTBUF_TAB_C%S_CAND_FY = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_FY(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CAND_FZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_FZ)
        OLD_SIZ = INTBUF_TAB%S_CAND_FZ
        INTBUF_TAB_C%S_CAND_FZ = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_FZ(OLD_SIZ))

        SIZ = INTBUF_TAB_C%S_CAND_T
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%CAND_T)
        OLD_SIZ = INTBUF_TAB%S_CAND_T
        INTBUF_TAB_C%S_CAND_T = OLD_SIZ
        ALLOCATE(INTBUF_TAB_C%CAND_T(OLD_SIZ))

        SIZ = INTBUF_TAB%S_IELNRTS
        INTBUF_TAB_C%S_IELNRTS = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%IELNRTS)
        ALLOCATE(INTBUF_TAB_C%IELNRTS(SIZ))

        SIZ = INTBUF_TAB%S_ADRECTS
        INTBUF_TAB_C%S_ADRECTS = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%ADRECTS)
        ALLOCATE(INTBUF_TAB_C%ADRECTS(SIZ))

        SIZ = INTBUF_TAB%S_FACNRTS
        INTBUF_TAB_C%S_FACNRTS = SIZ
        IF(SIZ>0)DEALLOCATE(INTBUF_TAB_C%FACNRTS)
        ALLOCATE(INTBUF_TAB_C%FACNRTS(SIZ))


! copy all integers arrays

        SIZ = INTBUF_TAB%S_IRECTS
        DO I=1, SIZ
          INTBUF_TAB_C%IRECTS(I) = INTBUF_TAB%IRECTS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IRECTM
        DO I=1, SIZ
          INTBUF_TAB_C%IRECTM(I) = INTBUF_TAB%IRECTM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NSV
        DO I=1, SIZ
          INTBUF_TAB_C%NSV(I) = INTBUF_TAB%NSV(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSR
        DO I=1, SIZ
          INTBUF_TAB_C%MSR(I) = INTBUF_TAB%MSR(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IRTLM
        DO I=1, SIZ
          INTBUF_TAB_C%IRTLM(I) = INTBUF_TAB%IRTLM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IRUPT
        DO I=1, SIZ
          INTBUF_TAB_C%IRUPT(I) = INTBUF_TAB%IRUPT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_INORM
        DO I=1, SIZ
          INTBUF_TAB_C%INORM(I) = INTBUF_TAB%INORM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IELEC
        DO I=1, SIZ
          INTBUF_TAB_C%IELEC(I) = INTBUF_TAB%IELEC(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IELES
        DO I=1, SIZ
          INTBUF_TAB_C%IELES(I) = INTBUF_TAB%IELES(I)
        ENDDO

        SIZ = INTBUF_TAB%S_LISUB
        DO I=1, SIZ
          INTBUF_TAB_C%LISUB(I) = INTBUF_TAB%LISUB(I)
        ENDDO

        SIZ = INTBUF_TAB%S_TYPSUB
        DO I=1, SIZ
          INTBUF_TAB_C%TYPSUB(I) = INTBUF_TAB%TYPSUB(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ADDSUBS
        DO I=1, SIZ
          INTBUF_TAB_C%ADDSUBS(I) = INTBUF_TAB%ADDSUBS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ADDSUBM
        DO I=1, SIZ
          INTBUF_TAB_C%ADDSUBM(I) = INTBUF_TAB%ADDSUBM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_LISUBS
        DO I=1, SIZ
          INTBUF_TAB_C%LISUBS(I) = INTBUF_TAB%LISUBS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_LISUBM
        DO I=1, SIZ
          INTBUF_TAB_C%LISUBM(I) = INTBUF_TAB%LISUBM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_E
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_E(I) = INTBUF_TAB%CAND_E(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_N
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_N(I) = INTBUF_TAB%CAND_N(I)
        ENDDO

        SIZ = INTBUF_TAB%S_I_STOK
        DO I=1, SIZ
          INTBUF_TAB_C%I_STOK(I) = INTBUF_TAB%I_STOK(I)
        ENDDO

        SIZ = INTBUF_TAB%S_LCAND_N
        DO I=1, SIZ
          INTBUF_TAB_C%LCAND_N(I) = INTBUF_TAB%LCAND_N(I)
        ENDDO

        SIZ = INTBUF_TAB%S_LCAND_S
        DO I=1, SIZ
          INTBUF_TAB_C%LCAND_S(I) = INTBUF_TAB%LCAND_S(I)
        ENDDO

        SIZ = INTBUF_TAB%S_I_STOK_E
        DO I=1, SIZ
          INTBUF_TAB_C%I_STOK_E(I) = INTBUF_TAB%I_STOK_E(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IFPEN
        DO I=1, SIZ
          INTBUF_TAB_C%IFPEN(I) = INTBUF_TAB%IFPEN(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IPARTFRICS
        DO I=1, SIZ
          INTBUF_TAB_C%IPARTFRICS(I) = INTBUF_TAB%IPARTFRICS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IPARTFRICM
        DO I=1, SIZ
          INTBUF_TAB_C%IPARTFRICM(I) = INTBUF_TAB%IPARTFRICM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IREP_FRICM
        DO I=1, SIZ
          INTBUF_TAB_C%IREP_FRICM(I) = INTBUF_TAB%IREP_FRICM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_KREMNODE
        DO I=1, SIZ
          INTBUF_TAB_C%KREMNODE(I) = INTBUF_TAB%KREMNODE(I)
        ENDDO

        SIZ = INTBUF_TAB%S_REMNODE
        DO I=1, SIZ
          INTBUF_TAB_C%REMNODE(I) = INTBUF_TAB%REMNODE(I)
        ENDDO

        SIZ = INTBUF_TAB%S_KREMNOR
        DO I=1, SIZ
          INTBUF_TAB_C%KREMNOR(I) = INTBUF_TAB%KREMNOR(I)
        ENDDO

        SIZ = INTBUF_TAB%S_REMNOR
        DO I=1, SIZ
          INTBUF_TAB_C%REMNOR(I) = INTBUF_TAB%REMNOR(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ADCCM
        DO I=1, SIZ
          INTBUF_TAB_C%ADCCM(I) = INTBUF_TAB%ADCCM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CHAIN
        DO I=1, SIZ
          INTBUF_TAB_C%CHAIN(I) = INTBUF_TAB%CHAIN(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NBINFLG
        DO I=1, SIZ
          INTBUF_TAB_C%NBINFLG(I) = INTBUF_TAB%NBINFLG(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MBINFLG
        DO I=1, SIZ
          INTBUF_TAB_C%MBINFLG(I) = INTBUF_TAB%MBINFLG(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NLG
        DO I=1, SIZ
          INTBUF_TAB_C%NLG(I) = INTBUF_TAB%NLG(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DAANC6
        DO I=1, SIZ
          INTBUF_TAB_C%DAANC6(I) = INTBUF_TAB%DAANC6(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ISLINS
        DO I=1, SIZ
          INTBUF_TAB_C%ISLINS(I) = INTBUF_TAB%ISLINS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IXLINS
        DO I=1, SIZ
          INTBUF_TAB_C%IXLINS(I) = INTBUF_TAB%IXLINS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IXLINM
        DO I=1, SIZ
          INTBUF_TAB_C%IXLINM(I) = INTBUF_TAB%IXLINM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSRL
        DO I=1, SIZ
          INTBUF_TAB_C%MSRL(I) = INTBUF_TAB%MSRL(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NSVL
        DO I=1, SIZ
          INTBUF_TAB_C%NSVL(I) = INTBUF_TAB%NSVL(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ADCCM20
        DO I=1, SIZ
          INTBUF_TAB_C%ADCCM20(I) = INTBUF_TAB%ADCCM20(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CHAIN20
        DO I=1, SIZ
          INTBUF_TAB_C%CHAIN20(I) = INTBUF_TAB%CHAIN20(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ILOCS
        DO I=1, SIZ
          INTBUF_TAB_C%ILOCS(I) = INTBUF_TAB%ILOCS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NSEGM
        DO I=1, SIZ
          INTBUF_TAB_C%NSEGM(I) = INTBUF_TAB%NSEGM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NRT
        DO I=1, SIZ
          INTBUF_TAB_C%NRT(I) = INTBUF_TAB%NRT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IRTLS
        DO I=1, SIZ
          INTBUF_TAB_C%IRTLS(I) = INTBUF_TAB%IRTLS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ILOCM
        DO I=1, SIZ
          INTBUF_TAB_C%ILOCM(I) = INTBUF_TAB%ILOCM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IRTLOM
        DO I=1, SIZ
          INTBUF_TAB_C%IRTLOM(I) = INTBUF_TAB%IRTLOM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IRTLOS
        DO I=1, SIZ
          INTBUF_TAB_C%IRTLOS(I) = INTBUF_TAB%IRTLOS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NSEGS
        DO I=1, SIZ
          INTBUF_TAB_C%NSEGS(I) = INTBUF_TAB%NSEGS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_LNSV
        DO I=1, SIZ
          INTBUF_TAB_C%LNSV(I) = INTBUF_TAB%LNSV(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IELEM
        DO I=1, SIZ
          INTBUF_TAB_C%IELEM(I) = INTBUF_TAB%IELEM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FCOUNT
        DO I=1, SIZ
          INTBUF_TAB_C%FCOUNT(I) = INTBUF_TAB%FCOUNT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_KSURF
        DO I=1, SIZ
          INTBUF_TAB_C%KSURF(I) = INTBUF_TAB%KSURF(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IMPACT
        DO I=1, SIZ
          INTBUF_TAB_C%IMPACT(I) = INTBUF_TAB%IMPACT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSR21
        DO I=1, SIZ
          INTBUF_TAB_C%MSR21(I) = INTBUF_TAB%MSR21(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MNDD
        DO I=1, SIZ
          INTBUF_TAB_C%MNDD(I) = INTBUF_TAB%MNDD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSR_L
        DO I=1, SIZ
          INTBUF_TAB_C%MSR_L(I) = INTBUF_TAB%MSR_L(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_T
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_T(I) = INTBUF_TAB%CAND_T(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MVOISIN
        DO I=1, SIZ
          INTBUF_TAB_C%MVOISIN(I) = INTBUF_TAB%MVOISIN(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NVOISIN
        DO I=1, SIZ
          INTBUF_TAB_C%NVOISIN(I) = INTBUF_TAB%NVOISIN(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSEGLO
        DO I=1, SIZ
          INTBUF_TAB_C%MSEGLO(I) = INTBUF_TAB%MSEGLO(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSEGTYP24
        DO I=1, SIZ
          INTBUF_TAB_C%MSEGTYP24(I) = INTBUF_TAB%MSEGTYP24(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ISEADD
        DO I=1, SIZ
          INTBUF_TAB_C%ISEADD(I) = INTBUF_TAB%ISEADD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ISEDGE
        DO I=1, SIZ
          INTBUF_TAB_C%ISEDGE(I) = INTBUF_TAB%ISEDGE(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ISEG_PXFEM
        DO I=1, SIZ
          INTBUF_TAB_C%ISEG_PXFEM(I) = INTBUF_TAB%ISEG_PXFEM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ISEG_PLY
        DO I=1, SIZ
          INTBUF_TAB_C%ISEG_PLY(I) = INTBUF_TAB%ISEG_PLY(I)
        ENDDO
        
        SIZ = INTBUF_TAB%S_ICONT_I
        DO I=1, SIZ
          INTBUF_TAB_C%ICONT_I(I) = INTBUF_TAB%ICONT_I(I)
        ENDDO

        SIZ = INTBUF_TAB%S_MSEGTYP2
        DO I=1, SIZ
          INTBUF_TAB_C%MSEGTYP2(I) = INTBUF_TAB%MSEGTYP2(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IF_ADH
        DO I=1, SIZ
          INTBUF_TAB_C%IF_ADH(I) = INTBUF_TAB%IF_ADH(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IELNRTS
        DO I=1, SIZ
          INTBUF_TAB_C%IELNRTS(I) = INTBUF_TAB%IELNRTS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ADRECTS
        DO I=1, SIZ
          INTBUF_TAB_C%ADRECTS(I) = INTBUF_TAB%ADRECTS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FACNRTS
        DO I=1, SIZ
          INTBUF_TAB_C%FACNRTS(I) = INTBUF_TAB%FACNRTS(I)
        ENDDO

! copy all real arrays

        SIZ = INTBUF_TAB%S_STFAC
        DO I=1, SIZ
          INTBUF_TAB_C%STFAC(I) = INTBUF_TAB%STFAC(I)
        ENDDO

        SIZ = INTBUF_TAB%S_VARIABLES
        DO I=1, SIZ
          INTBUF_TAB_C%VARIABLES(I) = INTBUF_TAB%VARIABLES(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CSTS
        DO I=1, SIZ
          INTBUF_TAB_C%CSTS(I) = INTBUF_TAB%CSTS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DPARA
        DO I=1, SIZ
          INTBUF_TAB_C%DPARA(I) = INTBUF_TAB%DPARA(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NMAS
        DO I=1, SIZ
          INTBUF_TAB_C%NMAS(I) = INTBUF_TAB%NMAS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_AREAS2
        DO I=1, SIZ
          INTBUF_TAB_C%AREAS2(I) = INTBUF_TAB%AREAS2(I)
        ENDDO

        SIZ = INTBUF_TAB%S_SMAS
        DO I=1, SIZ
          INTBUF_TAB_C%SMAS(I) = INTBUF_TAB%SMAS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_SINER
        DO I=1, SIZ
          INTBUF_TAB_C%SINER(I) = INTBUF_TAB%SINER(I)
        ENDDO   

        SIZ = INTBUF_TAB%S_UVAR
        DO I=1, SIZ
          INTBUF_TAB_C%UVAR(I) = INTBUF_TAB%UVAR(I)
        ENDDO

        SIZ = INTBUF_TAB%S_XM0
        DO I=1, SIZ
          INTBUF_TAB_C%XM0(I) = INTBUF_TAB%XM0(I)
        ENDDO

        SIZ = INTBUF_TAB%S_SPENALTY
        DO I=1, SIZ
          INTBUF_TAB_C%SPENALTY(I) = INTBUF_TAB%SPENALTY(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STFR_PENALTY
        DO I=1, SIZ
          INTBUF_TAB_C%STFR_PENALTY(I) = INTBUF_TAB%STFR_PENALTY(I)
        ENDDO

        SIZ = INTBUF_TAB%S_SKEW
        DO I=1, SIZ
          INTBUF_TAB_C%SKEW(I) = INTBUF_TAB%SKEW(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DSM
        DO I=1, SIZ
          INTBUF_TAB_C%DSM(I) = INTBUF_TAB%DSM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FSM
        DO I=1, SIZ
          INTBUF_TAB_C%FSM(I) = INTBUF_TAB%FSM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_RUPT
        DO I=1, SIZ
          INTBUF_TAB_C%RUPT(I) = INTBUF_TAB%RUPT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FINI
        DO I=1, SIZ
          INTBUF_TAB_C%FINI(I) = INTBUF_TAB%FINI(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STFNS
        DO I=1, SIZ
          INTBUF_TAB_C%STFNS(I) = INTBUF_TAB%STFNS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STFM
        DO I=1, SIZ
          INTBUF_TAB_C%STFM(I) = INTBUF_TAB%STFM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STFS
        DO I=1, SIZ
          INTBUF_TAB_C%STFS(I) = INTBUF_TAB%STFS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_PENIM
        DO I=1, SIZ
          INTBUF_TAB_C%PENIM(I) = INTBUF_TAB%PENIM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_PENIS
        DO I=1, SIZ
          INTBUF_TAB_C%PENIS(I) = INTBUF_TAB%PENIS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_M
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_M(I) = INTBUF_TAB%GAP_M(I)
        ENDDO
        
        SIZ = INTBUF_TAB%S_GAP_S
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_S(I) = INTBUF_TAB%GAP_S(I)
        ENDDO
        
        SIZ = INTBUF_TAB%S_CRIT
        DO I=1, SIZ
          INTBUF_TAB_C%CRIT(I) = INTBUF_TAB%CRIT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FRIC_P
        DO I=1, SIZ
          INTBUF_TAB_C%FRIC_P(I) = INTBUF_TAB%FRIC_P(I)
        ENDDO

        SIZ = INTBUF_TAB%S_XFILTR
        DO I=1, SIZ
          INTBUF_TAB_C%XFILTR(I) = INTBUF_TAB%XFILTR(I)
        ENDDO

        SIZ = INTBUF_TAB%S_AREAS
        DO I=1, SIZ
          INTBUF_TAB_C%AREAS(I) = INTBUF_TAB%AREAS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_AREAM
        DO I=1, SIZ
          INTBUF_TAB_C%AREAM(I) = INTBUF_TAB%AREAM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_SL
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_SL(I) = INTBUF_TAB%GAP_SL(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_ML
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_ML(I) = INTBUF_TAB%GAP_ML(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_P
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_P(I) = INTBUF_TAB%CAND_P(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_PS
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_PS(I) = INTBUF_TAB%CAND_PS(I)
        ENDDO


        SIZ = INTBUF_TAB%S_CAND_F
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_F(I) = INTBUF_TAB%CAND_F(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FTSAVX
        DO I=1, SIZ
          INTBUF_TAB_C%FTSAVX(I) = INTBUF_TAB%FTSAVX(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FTSAVY
        DO I=1, SIZ
          INTBUF_TAB_C%FTSAVY(I) = INTBUF_TAB%FTSAVY(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FTSAVZ
        DO I=1, SIZ
          INTBUF_TAB_C%FTSAVZ(I) = INTBUF_TAB%FTSAVZ(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_FX
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_FX(I) = INTBUF_TAB%CAND_FX(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_FY
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_FY(I) = INTBUF_TAB%CAND_FY(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CAND_FZ
        DO I=1, SIZ
          INTBUF_TAB_C%CAND_FZ(I) = INTBUF_TAB%CAND_FZ(I)
        ENDDO

        SIZ = INTBUF_TAB%S_XA
        DO I=1, SIZ
          INTBUF_TAB_C%XA(I) = INTBUF_TAB%XA(I)
        ENDDO

        SIZ = INTBUF_TAB%S_VA
        DO I=1, SIZ
          INTBUF_TAB_C%VA(I) = INTBUF_TAB%VA(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STFA
        DO I=1, SIZ
          INTBUF_TAB_C%STFA(I) = INTBUF_TAB%STFA(I)
        ENDDO

        SIZ = INTBUF_TAB%S_PENIA
        DO I=1, SIZ
          INTBUF_TAB_C%PENIA(I) = INTBUF_TAB%PENIA(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ALPHAK
        DO I=1, SIZ
          INTBUF_TAB_C%ALPHAK(I) = INTBUF_TAB%ALPHAK(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_SH
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_SH(I) = INTBUF_TAB%GAP_SH(I)
        ENDDO

        SIZ = INTBUF_TAB%S_AVX_ANCR
        DO I=1, SIZ
          INTBUF_TAB_C%AVX_ANCR(I) = INTBUF_TAB%AVX_ANCR(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CRITX
        DO I=1, SIZ
          INTBUF_TAB_C%CRITX(I) = INTBUF_TAB%CRITX(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_ME
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_ME(I) = INTBUF_TAB%GAP_ME(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STF
        DO I=1, SIZ
          INTBUF_TAB_C%STF(I) = INTBUF_TAB%STF(I)
        ENDDO

        SIZ = INTBUF_TAB%S_PENIME
        DO I=1, SIZ
          INTBUF_TAB_C%PENIME(I) = INTBUF_TAB%PENIME(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_SE
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_SE(I) = INTBUF_TAB%GAP_SE(I)
        ENDDO

        SIZ = INTBUF_TAB%S_PENISE
        DO I=1, SIZ
          INTBUF_TAB_C%PENISE(I) = INTBUF_TAB%PENISE(I)
        ENDDO

        SIZ = INTBUF_TAB%S_N
        DO I=1, SIZ
          INTBUF_TAB_C%N(I) = INTBUF_TAB%N(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CSTM
        DO I=1, SIZ
          INTBUF_TAB_C%CSTM(I) = INTBUF_TAB%CSTM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_EE
        DO I=1, SIZ
          INTBUF_TAB_C%EE(I) = INTBUF_TAB%EE(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STFNM
        DO I=1, SIZ
          INTBUF_TAB_C%STFNM(I) = INTBUF_TAB%STFNM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FRICOS
        DO I=1, SIZ
          INTBUF_TAB_C%FRICOS(I) = INTBUF_TAB%FRICOS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FRICOM
        DO I=1, SIZ
          INTBUF_TAB_C%FRICOM(I) = INTBUF_TAB%FRICOM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FTSAV
        DO I=1, SIZ
          INTBUF_TAB_C%FTSAV(I) = INTBUF_TAB%FTSAV(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FCONT
        DO I=1, SIZ
          INTBUF_TAB_C%FCONT(I) = INTBUF_TAB%FCONT(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FS
        DO I=1, SIZ
          INTBUF_TAB_C%FS(I) = INTBUF_TAB%FS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FM
        DO I=1, SIZ
          INTBUF_TAB_C%FM(I) = INTBUF_TAB%FM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_RMAS
        DO I=1, SIZ
          INTBUF_TAB_C%RMAS(I) = INTBUF_TAB%RMAS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ANSMX0
        DO I=1, SIZ
          INTBUF_TAB_C%ANSMX0(I) = INTBUF_TAB%ANSMX0(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAPN
        DO I=1, SIZ
          INTBUF_TAB_C%GAPN(I) = INTBUF_TAB%GAPN(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STF8
        DO I=1, SIZ
          INTBUF_TAB_C%STF8(I) = INTBUF_TAB%STF8(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CIMP
        DO I=1, SIZ
          INTBUF_TAB_C%CIMP(I) = INTBUF_TAB%CIMP(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NIMP
        DO I=1, SIZ
          INTBUF_TAB_C%NIMP(I) = INTBUF_TAB%NIMP(I)
        ENDDO

        SIZ = INTBUF_TAB%S_IOLD
        DO I=1, SIZ
          INTBUF_TAB_C%IOLD(I) = INTBUF_TAB%IOLD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_HOLD
        DO I=1, SIZ
          INTBUF_TAB_C%HOLD(I) = INTBUF_TAB%HOLD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NOLD
        DO I=1, SIZ
          INTBUF_TAB_C%NOLD(I) = INTBUF_TAB%NOLD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DOLD
        DO I=1, SIZ
          INTBUF_TAB_C%DOLD(I) = INTBUF_TAB%DOLD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_KS
        DO I=1, SIZ
          INTBUF_TAB_C%KS(I) = INTBUF_TAB%KS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_KM
        DO I=1, SIZ
          INTBUF_TAB_C%KM(I) = INTBUF_TAB%KM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FROTS
        DO I=1, SIZ
          INTBUF_TAB_C%FROTS(I) = INTBUF_TAB%FROTS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FROTM
        DO I=1, SIZ
          INTBUF_TAB_C%FROTM(I) = INTBUF_TAB%FROTM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NOD_NORMAL
        DO I=1, SIZ
          INTBUF_TAB_C%NOD_NORMAL(I) = INTBUF_TAB%NOD_NORMAL(I)
        ENDDO

        SIZ = INTBUF_TAB%S_RCURV
        DO I=1, SIZ
          INTBUF_TAB_C%RCURV(I) = INTBUF_TAB%RCURV(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ANGLM
        DO I=1, SIZ
          INTBUF_TAB_C%ANGLM(I) = INTBUF_TAB%ANGLM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_FROT_P
        DO I=1, SIZ
          INTBUF_TAB_C%FROT_P(I) = INTBUF_TAB%FROT_P(I)
        ENDDO

        SIZ = INTBUF_TAB%S_ALPHA0
        DO I=1, SIZ
          INTBUF_TAB_C%ALPHA0(I) = INTBUF_TAB%ALPHA0(I)
        ENDDO

        SIZ = INTBUF_TAB%S_AS
        DO I=1, SIZ
          INTBUF_TAB_C%AS(I) = INTBUF_TAB%AS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_BS
        DO I=1, SIZ
          INTBUF_TAB_C%BS(I) = INTBUF_TAB%BS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_THKNOD0
        DO I=1, SIZ
          INTBUF_TAB_C%THKNOD0(I) = INTBUF_TAB%THKNOD0(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAPN_M
        DO I=1, SIZ
          INTBUF_TAB_C%GAPN_M(I) = INTBUF_TAB%GAPN_M(I)
        ENDDO

        SIZ = INTBUF_TAB%S_PENE_OLD
        DO I=1, SIZ
          INTBUF_TAB_C%PENE_OLD(I) = INTBUF_TAB%PENE_OLD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_STIF_OLD
        DO I=1, SIZ
          INTBUF_TAB_C%STIF_OLD(I) = INTBUF_TAB%STIF_OLD(I)
        ENDDO

        SIZ = INTBUF_TAB%S_TIME_S
        DO I=1, SIZ
          INTBUF_TAB_C%TIME_S(I) = INTBUF_TAB%TIME_S(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_NM
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_NM(I) = INTBUF_TAB%GAP_NM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_EDGE8L2
        DO I=1, SIZ
          INTBUF_TAB_C%EDGE8L2(I) = INTBUF_TAB%EDGE8L2(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NOD_2RY_LGTH
        DO I=1, SIZ
          INTBUF_TAB_C%NOD_2RY_LGTH(I) = INTBUF_TAB%NOD_2RY_LGTH(I)
        ENDDO

        SIZ = INTBUF_TAB%S_NOD_MAS_LGTH
        DO I=1, SIZ
          INTBUF_TAB_C%NOD_MAS_LGTH(I) = INTBUF_TAB%NOD_MAS_LGTH(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAP_N0
        DO I=1, SIZ
          INTBUF_TAB_C%GAP_N0(I) = INTBUF_TAB%GAP_N0(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DGAP_NM
        DO I=1, SIZ
          INTBUF_TAB_C%DGAP_NM(I) = INTBUF_TAB%DGAP_NM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DGAP_M
        DO I=1, SIZ
          INTBUF_TAB_C%DGAP_M(I) = INTBUF_TAB%DGAP_M(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DELTA_PMAX_DGAP
        DO I=1, SIZ
          INTBUF_TAB_C%DELTA_PMAX_DGAP(I) = 
     .             INTBUF_TAB%DELTA_PMAX_DGAP(I)
        ENDDO

        SIZ = INTBUF_TAB%S_CSTS_BIS
        DO I=1, SIZ
          INTBUF_TAB_C%CSTS_BIS(I) = INTBUF_TAB%CSTS_BIS(I)
        ENDDO

        SIZ = INTBUF_TAB%S_DIR_FRICM
        DO I=1, SIZ
          INTBUF_TAB_C%DIR_FRICM(I) = INTBUF_TAB%DIR_FRICM(I)
        ENDDO

        SIZ = INTBUF_TAB%S_GAPMSAV
        DO I=1, SIZ
          INTBUF_TAB_C%GAPMSAV(I) = INTBUF_TAB%GAPMSAV(I)
        ENDDO
C-----
      RETURN

      END SUBROUTINE COPY_INTBUF_TAB

Chd|====================================================================
Chd|  INTBUF_TAB_C_INI              interf/copy_intbuf_tab.F      
Chd|-- called by -----------
Chd|        DIM_INT7                      engine/source/implicit/ind_glob_k.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|====================================================================
      SUBROUTINE INTBUF_TAB_C_INI(INTBUF_TAB, INTBUF_TAB_C)  
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*), INTBUF_TAB_C(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NI,SIZ
C=======================================================================

      DO NI= 1, NINTER

! uninitialize INTEGER sizes and arrays for INTBUF_TAB_CP
        SIZ = INTBUF_TAB(NI)%S_IRECTS
        INTBUF_TAB_C(NI)%S_IRECTS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRECTS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IRECTM
        INTBUF_TAB_C(NI)%S_IRECTM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRECTM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NSV
        INTBUF_TAB_C(NI)%S_NSV = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NSV(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSR
        INTBUF_TAB_C(NI)%S_MSR = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSR(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IRTLM
        INTBUF_TAB_C(NI)%S_IRTLM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRTLM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IRUPT
        INTBUF_TAB_C(NI)%S_IRUPT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRUPT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_INORM
        INTBUF_TAB_C(NI)%S_INORM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%INORM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IELEC
        INTBUF_TAB_C(NI)%S_IELEC = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IELEC(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IELES
        INTBUF_TAB_C(NI)%S_IELES = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IELES(SIZ))

        SIZ = INTBUF_TAB(NI)%S_LISUB
        INTBUF_TAB_C(NI)%S_LISUB = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%LISUB(SIZ))

        SIZ = INTBUF_TAB(NI)%S_TYPSUB
        INTBUF_TAB_C(NI)%S_TYPSUB = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%TYPSUB(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ADDSUBS
        INTBUF_TAB_C(NI)%S_ADDSUBS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ADDSUBS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ADDSUBM
        INTBUF_TAB_C(NI)%S_ADDSUBM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ADDSUBM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_LISUBS
        INTBUF_TAB_C(NI)%S_LISUBS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%LISUBS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_LISUBM
        INTBUF_TAB_C(NI)%S_LISUBM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%LISUBM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_E
        INTBUF_TAB_C(NI)%S_CAND_E = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_E(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_N
        INTBUF_TAB_C(NI)%S_CAND_N = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_N(SIZ))

        SIZ = INTBUF_TAB(NI)%S_I_STOK
        INTBUF_TAB_C(NI)%S_I_STOK = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%I_STOK(SIZ))

        SIZ = INTBUF_TAB(NI)%S_LCAND_N
        INTBUF_TAB_C(NI)%S_LCAND_N = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%LCAND_N(SIZ))

        SIZ = INTBUF_TAB(NI)%S_LCAND_S
        INTBUF_TAB_C(NI)%S_LCAND_S = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%LCAND_S(SIZ))

        SIZ = INTBUF_TAB(NI)%S_I_STOK_E
        INTBUF_TAB_C(NI)%S_I_STOK_E = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%I_STOK_E(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IFPEN
        INTBUF_TAB_C(NI)%S_IFPEN = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IFPEN(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IPARTFRICS
        INTBUF_TAB_C(NI)%S_IPARTFRICS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IPARTFRICS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IPARTFRICM
        INTBUF_TAB_C(NI)%S_IPARTFRICM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IPARTFRICM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IREP_FRICM
        INTBUF_TAB_C(NI)%S_IREP_FRICM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IREP_FRICM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_KREMNODE
        INTBUF_TAB_C(NI)%S_KREMNODE = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%KREMNODE(SIZ))

        SIZ = INTBUF_TAB(NI)%S_REMNODE
        INTBUF_TAB_C(NI)%S_REMNODE = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%REMNODE(SIZ))

        SIZ = INTBUF_TAB(NI)%S_KREMNOR
        INTBUF_TAB_C(NI)%S_KREMNOR = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%KREMNOR(SIZ))

        SIZ = INTBUF_TAB(NI)%S_REMNOR
        INTBUF_TAB_C(NI)%S_REMNOR = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%REMNOR(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ADCCM
        INTBUF_TAB_C(NI)%S_ADCCM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ADCCM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CHAIN
        INTBUF_TAB_C(NI)%S_CHAIN = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CHAIN(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NBINFLG
        INTBUF_TAB_C(NI)%S_NBINFLG = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NBINFLG(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MBINFLG
        INTBUF_TAB_C(NI)%S_MBINFLG = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MBINFLG(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NLG
        INTBUF_TAB_C(NI)%S_NLG = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NLG(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DAANC6
        INTBUF_TAB_C(NI)%S_DAANC6 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DAANC6(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ISLINS
        INTBUF_TAB_C(NI)%S_ISLINS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ISLINS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IXLINS
        INTBUF_TAB_C(NI)%S_IXLINS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IXLINS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IXLINM
        INTBUF_TAB_C(NI)%S_IXLINM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IXLINM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSRL
        INTBUF_TAB_C(NI)%S_MSRL = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSRL(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NSVL
        INTBUF_TAB_C(NI)%S_NSVL = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NSVL(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ADCCM20
        INTBUF_TAB_C(NI)%S_ADCCM20 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ADCCM20(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CHAIN20
        INTBUF_TAB_C(NI)%S_CHAIN20 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CHAIN20(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ILOCS
        INTBUF_TAB_C(NI)%S_ILOCS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ILOCS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NSEGM
        INTBUF_TAB_C(NI)%S_NSEGM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NSEGM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NRT
        INTBUF_TAB_C(NI)%S_NRT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NRT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IRTLS
        INTBUF_TAB_C(NI)%S_IRTLS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRTLS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ILOCM
        INTBUF_TAB_C(NI)%S_ILOCM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ILOCM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IRTLOM
        INTBUF_TAB_C(NI)%S_IRTLOM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRTLOM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IRTLOS
        INTBUF_TAB_C(NI)%S_IRTLOS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IRTLOS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NSEGS
        INTBUF_TAB_C(NI)%S_NSEGS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NSEGS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_LNSV
        INTBUF_TAB_C(NI)%S_LNSV = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%LNSV(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IELEM
        INTBUF_TAB_C(NI)%S_IELEM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IELEM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FCOUNT
        INTBUF_TAB_C(NI)%S_FCOUNT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FCOUNT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_KSURF
        INTBUF_TAB_C(NI)%S_KSURF = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%KSURF(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IMPACT
        INTBUF_TAB_C(NI)%S_IMPACT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IMPACT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSR21
        INTBUF_TAB_C(NI)%S_MSR21 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSR21(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MNDD
        INTBUF_TAB_C(NI)%S_MNDD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MNDD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSR_L
        INTBUF_TAB_C(NI)%S_MSR_L = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSR_L(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_T
        INTBUF_TAB_C(NI)%S_CAND_T = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_T(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MVOISIN
        INTBUF_TAB_C(NI)%S_MVOISIN = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MVOISIN(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NVOISIN
        INTBUF_TAB_C(NI)%S_NVOISIN = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NVOISIN(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSEGLO
        INTBUF_TAB_C(NI)%S_MSEGLO = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSEGLO(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSEGTYP24
        INTBUF_TAB_C(NI)%S_MSEGTYP24 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSEGTYP24(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ISEADD
        INTBUF_TAB_C(NI)%S_ISEADD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ISEADD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ISEDGE
        INTBUF_TAB_C(NI)%S_ISEDGE = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ISEDGE(SIZ))
       
        SIZ = INTBUF_TAB(NI)%S_ISEG_PXFEM
        INTBUF_TAB_C(NI)%S_ISEG_PXFEM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ISEG_PXFEM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ISEG_PLY
        INTBUF_TAB_C(NI)%S_ISEG_PLY = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ISEG_PLY(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ICONT_I
        INTBUF_TAB_C(NI)%S_ICONT_I = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ICONT_I(SIZ))

        SIZ = INTBUF_TAB(NI)%S_MSEGTYP2
        INTBUF_TAB_C(NI)%S_MSEGTYP2 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%MSEGTYP2(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IF_ADH
        INTBUF_TAB_C(NI)%S_IF_ADH = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IF_ADH(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IELNRTS
        INTBUF_TAB_C(NI)%S_IELNRTS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IELNRTS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ADRECTS
        INTBUF_TAB_C(NI)%S_ADRECTS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ADRECTS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FACNRTS
        INTBUF_TAB_C(NI)%S_FACNRTS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FACNRTS(SIZ))

! uninitialize REAL sizes and arrays for INTBUF_TAB_CP

        SIZ = INTBUF_TAB(NI)%S_STFAC
        INTBUF_TAB_C(NI)%S_STFAC = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFAC(SIZ))

        SIZ = INTBUF_TAB(NI)%S_VARIABLES
        INTBUF_TAB_C(NI)%S_VARIABLES = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%VARIABLES(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CSTS
        INTBUF_TAB_C(NI)%S_CSTS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CSTS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DPARA
        INTBUF_TAB_C(NI)%S_DPARA = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DPARA(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NMAS
        INTBUF_TAB_C(NI)%S_NMAS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NMAS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_AREAS2
        INTBUF_TAB_C(NI)%S_AREAS2 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%AREAS2(SIZ))

        SIZ = INTBUF_TAB(NI)%S_SMAS
        INTBUF_TAB_C(NI)%S_SMAS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%SMAS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_SINER
        INTBUF_TAB_C(NI)%S_SINER = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%SINER(SIZ))   

        SIZ = INTBUF_TAB(NI)%S_UVAR
        INTBUF_TAB_C(NI)%S_UVAR = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%UVAR(SIZ))

        SIZ = INTBUF_TAB(NI)%S_XM0
        INTBUF_TAB_C(NI)%S_XM0 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%XM0(SIZ))

        SIZ = INTBUF_TAB(NI)%S_SPENALTY
        INTBUF_TAB_C(NI)%S_SPENALTY = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%SPENALTY(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STFR_PENALTY
        INTBUF_TAB_C(NI)%S_STFR_PENALTY = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFR_PENALTY(SIZ))

        SIZ = INTBUF_TAB(NI)%S_SKEW
        INTBUF_TAB_C(NI)%S_SKEW = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%SKEW(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DSM
        INTBUF_TAB_C(NI)%S_DSM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DSM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FSM
        INTBUF_TAB_C(NI)%S_FSM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FSM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_RUPT
        INTBUF_TAB_C(NI)%S_RUPT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%RUPT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FINI
        INTBUF_TAB_C(NI)%S_FINI = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FINI(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STFNS
        INTBUF_TAB_C(NI)%S_STFNS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFNS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STFM
        INTBUF_TAB_C(NI)%S_STFM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STFS
        INTBUF_TAB_C(NI)%S_STFS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_PENIM
        INTBUF_TAB_C(NI)%S_PENIM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%PENIM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_PENIS
        INTBUF_TAB_C(NI)%S_PENIS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%PENIS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_M
        INTBUF_TAB_C(NI)%S_GAP_M = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_M(SIZ))
        
        SIZ = INTBUF_TAB(NI)%S_GAP_S
        INTBUF_TAB_C(NI)%S_GAP_S = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_S(SIZ))
        
        SIZ = INTBUF_TAB(NI)%S_CRIT
        INTBUF_TAB_C(NI)%S_CRIT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CRIT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FRIC_P
        INTBUF_TAB_C(NI)%S_FRIC_P = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FRIC_P(SIZ))

        SIZ = INTBUF_TAB(NI)%S_XFILTR
        INTBUF_TAB_C(NI)%S_XFILTR = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%XFILTR(SIZ))

        SIZ = INTBUF_TAB(NI)%S_AREAS
        INTBUF_TAB_C(NI)%S_AREAS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%AREAS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_AREAM
        INTBUF_TAB_C(NI)%S_AREAM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%AREAM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_SL
        INTBUF_TAB_C(NI)%S_GAP_SL = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_SL(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_ML
        INTBUF_TAB_C(NI)%S_GAP_ML = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_ML(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_P
        INTBUF_TAB_C(NI)%S_CAND_P = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_P(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_PS
        INTBUF_TAB_C(NI)%S_CAND_PS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_PS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_F
        INTBUF_TAB_C(NI)%S_CAND_F = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_F(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FTSAVX
        INTBUF_TAB_C(NI)%S_FTSAVX = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FTSAVX(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FTSAVY
        INTBUF_TAB_C(NI)%S_FTSAVY = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FTSAVY(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FTSAVZ
        INTBUF_TAB_C(NI)%S_FTSAVZ = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FTSAVZ(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_FX
        INTBUF_TAB_C(NI)%S_CAND_FX = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_FX(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_FY
        INTBUF_TAB_C(NI)%S_CAND_FY = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_FY(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CAND_FZ
        INTBUF_TAB_C(NI)%S_CAND_FZ = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CAND_FZ(SIZ))

        SIZ = INTBUF_TAB(NI)%S_XA
        INTBUF_TAB_C(NI)%S_XA = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%XA(SIZ))

        SIZ = INTBUF_TAB(NI)%S_VA
        INTBUF_TAB_C(NI)%S_VA = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%VA(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STFA
        INTBUF_TAB_C(NI)%S_STFA = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFA(SIZ))

        SIZ = INTBUF_TAB(NI)%S_PENIA
        INTBUF_TAB_C(NI)%S_PENIA = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%PENIA(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ALPHAK
        INTBUF_TAB_C(NI)%S_ALPHAK = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ALPHAK(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_SH
        INTBUF_TAB_C(NI)%S_GAP_SH = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_SH(SIZ))

        SIZ = INTBUF_TAB(NI)%S_AVX_ANCR
        INTBUF_TAB_C(NI)%S_AVX_ANCR = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%AVX_ANCR(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CRITX
        INTBUF_TAB_C(NI)%S_CRITX = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CRITX(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_ME
        INTBUF_TAB_C(NI)%S_GAP_ME = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_ME(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STF
        INTBUF_TAB_C(NI)%S_STF = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STF(SIZ))

        SIZ = INTBUF_TAB(NI)%S_PENIME
        INTBUF_TAB_C(NI)%S_PENIME = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%PENIME(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_SE
        INTBUF_TAB_C(NI)%S_GAP_SE = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_SE(SIZ))

        SIZ = INTBUF_TAB(NI)%S_PENISE
        INTBUF_TAB_C(NI)%S_PENISE = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%PENISE(SIZ))

        SIZ = INTBUF_TAB(NI)%S_N
        INTBUF_TAB_C(NI)%S_N = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%N(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CSTM
        INTBUF_TAB_C(NI)%S_CSTM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CSTM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_EE
        INTBUF_TAB_C(NI)%S_EE = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%EE(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STFNM
        INTBUF_TAB_C(NI)%S_STFNM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STFNM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FRICOS
        INTBUF_TAB_C(NI)%S_FRICOS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FRICOS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FRICOM
        INTBUF_TAB_C(NI)%S_FRICOM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FRICOM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FTSAV
        INTBUF_TAB_C(NI)%S_FTSAV = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FTSAV(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FCONT
        INTBUF_TAB_C(NI)%S_FCONT = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FCONT(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FS
        INTBUF_TAB_C(NI)%S_FS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FM
        INTBUF_TAB_C(NI)%S_FM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_RMAS
        INTBUF_TAB_C(NI)%S_RMAS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%RMAS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ANSMX0
        INTBUF_TAB_C(NI)%S_ANSMX0 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ANSMX0(SIZ))

        INTBUF_TAB_C(NI)%S_T8= INTBUF_TAB(NI)%S_T8

        SIZ = INTBUF_TAB(NI)%S_GAPN
        INTBUF_TAB_C(NI)%S_GAPN = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAPN(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STF8
        INTBUF_TAB_C(NI)%S_STF8 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STF8(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CIMP
        INTBUF_TAB_C(NI)%S_CIMP = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CIMP(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NIMP
        INTBUF_TAB_C(NI)%S_NIMP = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NIMP(SIZ))

        SIZ = INTBUF_TAB(NI)%S_IOLD
        INTBUF_TAB_C(NI)%S_IOLD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%IOLD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_HOLD
        INTBUF_TAB_C(NI)%S_HOLD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%HOLD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NOLD
        INTBUF_TAB_C(NI)%S_NOLD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NOLD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DOLD
        INTBUF_TAB_C(NI)%S_DOLD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DOLD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_KS
        INTBUF_TAB_C(NI)%S_KS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%KS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_KM
        INTBUF_TAB_C(NI)%S_KM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%KM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FROTS
        INTBUF_TAB_C(NI)%S_FROTS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FROTS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FROTM
        INTBUF_TAB_C(NI)%S_FROTM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FROTM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NOD_NORMAL
        INTBUF_TAB_C(NI)%S_NOD_NORMAL = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NOD_NORMAL(SIZ))

        SIZ = INTBUF_TAB(NI)%S_RCURV
        INTBUF_TAB_C(NI)%S_RCURV = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%RCURV(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ANGLM
        INTBUF_TAB_C(NI)%S_ANGLM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ANGLM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_FROT_P
        INTBUF_TAB_C(NI)%S_FROT_P = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%FROT_P(SIZ))

        SIZ = INTBUF_TAB(NI)%S_ALPHA0
        INTBUF_TAB_C(NI)%S_ALPHA0 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%ALPHA0(SIZ))

        SIZ = INTBUF_TAB(NI)%S_AS
        INTBUF_TAB_C(NI)%S_AS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%AS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_BS
        INTBUF_TAB_C(NI)%S_BS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%BS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_THKNOD0
        INTBUF_TAB_C(NI)%S_THKNOD0 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%THKNOD0(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAPN_M
        INTBUF_TAB_C(NI)%S_GAPN_M = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAPN_M(SIZ))

        SIZ = INTBUF_TAB(NI)%S_PENE_OLD
        INTBUF_TAB_C(NI)%S_PENE_OLD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%PENE_OLD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_STIF_OLD
        INTBUF_TAB_C(NI)%S_STIF_OLD = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%STIF_OLD(SIZ))

        SIZ = INTBUF_TAB(NI)%S_TIME_S
        INTBUF_TAB_C(NI)%S_TIME_S = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%TIME_S(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_NM
        INTBUF_TAB_C(NI)%S_GAP_NM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_NM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_EDGE8L2
        INTBUF_TAB_C(NI)%S_EDGE8L2 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%EDGE8L2(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NOD_2RY_LGTH
        INTBUF_TAB_C(NI)%S_NOD_2RY_LGTH = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NOD_2RY_LGTH(SIZ))

        SIZ = INTBUF_TAB(NI)%S_NOD_MAS_LGTH
        INTBUF_TAB_C(NI)%S_NOD_MAS_LGTH = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%NOD_MAS_LGTH(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAP_N0
        INTBUF_TAB_C(NI)%S_GAP_N0 = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAP_N0(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DGAP_NM
        INTBUF_TAB_C(NI)%S_DGAP_NM = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DGAP_NM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DGAP_M
        INTBUF_TAB_C(NI)%S_DGAP_M = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DGAP_M(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DELTA_PMAX_DGAP
        INTBUF_TAB_C(NI)%S_DELTA_PMAX_DGAP = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DELTA_PMAX_DGAP(SIZ))

        SIZ = INTBUF_TAB(NI)%S_CSTS_BIS
        INTBUF_TAB_C(NI)%S_CSTS_BIS = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%CSTS_BIS(SIZ))

        SIZ = INTBUF_TAB(NI)%S_DIR_FRICM
        INTBUF_TAB_C(NI)%S_DIR_FRICM= SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%DIR_FRICM(SIZ))

        SIZ = INTBUF_TAB(NI)%S_GAPMSAV
        INTBUF_TAB_C(NI)%S_GAPMSAV = SIZ
        ALLOCATE(INTBUF_TAB_C(NI)%GAPMSAV(SIZ))

      ENDDO !NI=1,NINTER
C-----
      RETURN

      END SUBROUTINE INTBUF_TAB_C_INI
