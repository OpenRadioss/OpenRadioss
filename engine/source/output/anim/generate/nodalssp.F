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
!||    nodalssp                     ../engine/source/output/anim/generate/nodalssp.F
!||--- called by ------------------------------------------------------
!||    genani                       ../engine/source/output/anim/generate/genani.F
!||    h3d_nodal_scalar             ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!||--- calls      -----------------------------------------------------
!||    anim_nodal_contour_fvmbags   ../engine/source/output/anim/generate/anim_nodal_contour_fvmbags.F
!||    anim_nodal_ssp_elems         ../engine/source/output/anim/generate/anim_nodal_ssp_elems.F90
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    fvbag_mod                    ../engine/share/modules/fvbag_mod.F
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||    initbuf_mod                  ../engine/share/resol/initbuf.F
!||    multi_fvm_mod                ../common_source/modules/ale/multi_fvm_mod.F90
!||====================================================================
      SUBROUTINE NODALSSP(IFUNC          , WA4                  , WA4_FVM, IFLOW   , RFLOW,
     .                    IPARG          , ELBUF_TAB            , IX     , NIX     , NUMEL,
     .                    ITAB           , NV46                 , MONVOL , VOLMON  , AIRBAGS_TOTAL_FVM_IN_H3D,
     .                    IS_WRITTEN_NODE,IS_WRITTEN_NODE_FVM   , ISPMD  , FVDATA_P,
     .                    SWA4           , AIRBAGS_NODE_ID_SHIFT, MULTI_FVM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine defines & writes nodal sound speed requested by Engine keyword /H3D/NODA/SSP
C      application :  /MONVOL/FVMBAG* (sound speed from polyhedra, specific buffer).
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     none
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE FVBAG_MOD , only:FVBAG_DATA,NFVBAG
      USE GROUPDEF_MOD , only:GROUP_
      USE MULTI_FVM_MOD , ONLY : MULTI_FVM_STRUCT
      use anim_nodal_ssp_elems_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMEL, IFUNC, NIX, NV46,ITAB(NUMNOD), ISPMD, SWA4, AIRBAGS_NODE_ID_SHIFT
      INTEGER,INTENT(IN) :: IFLOW(*),IPARG(NPARG,NGROUP),IX(NIX,NUMEL)
      my_real,INTENT(IN) :: RFLOW(*)
      INTEGER, INTENT(IN) :: AIRBAGS_TOTAL_FVM_IN_H3D
      REAL,INTENT(INOUT) :: WA4(SWA4), WA4_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      TYPE (ELBUF_STRUCT_), INTENT(IN), DIMENSION(NGROUP) :: ELBUF_TAB    
      INTEGER,INTENT(IN) :: MONVOL(SMONVOL) 
      my_real,INTENT(IN) :: VOLMON(SVOLMON)
      INTEGER,INTENT(INOUT) :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D),IS_WRITTEN_NODE(NUMNOD)   
      TYPE(FVBAG_DATA),INTENT(IN) :: FVDATA_P (NFVBAG)
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, NELv,II1, II2,
     .        IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .        IALEL,NNOD,IPOS,IV,NGv,IDLOCv,J1,J2,IBV
      INTEGER MLW, NG, KCVT, II(6), NBF, NBL, IB, ICELL, NIN, MCELL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF,GBUFv     
      my_real, ALLOCATABLE, DIMENSION(:) :: COUNT_VOL    
      my_real SSP,V
      INTEGER,DIMENSION(:,:), POINTER   :: pAdjBRICK      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

C------------------------------------------------------------------
C        DEFAULT - ALL ELEMS (HEXA,PENTA, SHELL, QUAD, ..)
C           expand elem pressure to elem nodes
C------------------------------------------------------------------
        CALL ANIM_NODAL_SSP_ELEMS(WA4,SWA4,IPARG,ELBUF_TAB,IX,NIX,NUMEL,IS_WRITTEN_NODE,
     .                            MULTI_FVM, NPARG, NGROUP, NUMNOD)
         
C------------------------------------------------------------------
C        /MONVOL/FVMBAG*
C           set pressure from polyhedra centroids (H3D only)
C------------------------------------------------------------------
        IF(NFVBAG > 0 .AND. ISPMD == 0 .AND. AIRBAGS_TOTAL_FVM_IN_H3D > 0)THEN
          CALL ANIM_NODAL_CONTOUR_FVMBAGS('SSP ', WA4_FVM, MONVOL , VOLMON                  , FVDATA_P           , 
     .                                    NFVBAG, SMONVOL, SVOLMON, AIRBAGS_TOTAL_FVM_IN_H3D, IS_WRITTEN_NODE_FVM,
     .                                    AIRBAGS_NODE_ID_SHIFT)
        
        ENDIF
        

               
C-----------------------------------------------
      RETURN
      END
