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
Chd|  EBCS_MAIN                     source/boundary_conditions/ebcs/ebcs_main.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        EBCS0                         source/boundary_conditions/ebcs/ebcs0.F
Chd|        EBCS1                         source/boundary_conditions/ebcs/ebcs1.F
Chd|        EBCS10                        source/boundary_conditions/ebcs/ebcs10.F
Chd|        EBCS4                         source/boundary_conditions/ebcs/ebcs4.F
Chd|        EBCS5                         source/boundary_conditions/ebcs/ebcs5.F
Chd|        EBCS6                         source/boundary_conditions/ebcs/ebcs6.F
Chd|        EBCS7                         source/boundary_conditions/ebcs/ebcs7.F
Chd|        EBCS_VOL2SEG                  source/boundary_conditions/ebcs/ebcs_vol2seg.F
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE EBCS_MAIN(IGRSURF,SEGVAR,VOLMON,A,V,W,X,
     .                     FSAV,FV,WA,MS,STIFN,IPARG,ELBUF_TAB,EBCS_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .                     ITAB,NALE,FSKY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD  
      USE GROUPDEF_MOD
      USE EBCS_MOD
      USE MULTI_FVM_MOD
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP)
      INTEGER,INTENT(IN) :: IXQ(NIXQ,NUMELQ),IXS(NIXS,NUMELS),IXTG(NIXTG,NUMELTG),ITAB(NUMNOD),NALE(NUMNOD)
      my_real
     .       VOLMON(*),SEGVAR(*),V(3,NUMNOD),W(3,NUMNOD),A(3,NUMNOD),X(3,NUMNOD),FSAV(NTHVKI,*),
     .       FV(*),WA(*),MS(NUMNOD),STIFN(NUMNOD)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(t_ebcs_tab), TARGET, INTENT(INOUT) :: EBCS_TAB
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
      my_real, DIMENSION(8,LSKY), INTENT(INOUT) :: FSKY ! acceleration array for parith/on option
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,TYP,ISU,NSEG,VOLU,NOD,J
      CLASS(t_ebcs), POINTER :: EBCS
C-----------------------------------------------
C   S o u r  c e   L i n e s
C-----------------------------------------------
 
      DO I = 1, NEBCS
         IF(.NOT.EBCS_TAB%need_to_compute(I)) CYCLE
         EBCS => EBCS_TAB%tab(i)%poly
         IF(EBCS%is_multifluid)return
         TYP = EBCS%type
         ISU = EBCS%surf_id
         NSEG = EBCS%nb_elem
         NOD = EBCS%nb_node
         IF (TYP == 0) THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is (t_ebcs_gradp0) 
            CALL EBCS0(NSEG, EBCS%iseg, SEGVAR,
     .           A, V, X, 
     .           EBCS%node_list, NOD, EBCS%elem_list,EBCS%ielem,
     .           EBCS%vold, EBCS%pold, EBCS%p0,
     .           EBCS%la, FV, MS, STIFN, IPARG, ELBUF_TAB, twf)
            end select
         ELSE IF (TYP <= 3)THEN
            CALL EBCS1(NSEG,EBCS%iseg,SEGVAR,
     .           A,V,X,
     .           EBCS%node_list,NOD,EBCS%elem_list,
     .           EBCS%vold,EBCS%pold,EBCS%la,
     .           FV,MS,STIFN,TYP,EBCS_TAB, I)
         ELSE IF (TYP == 4) THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is (t_ebcs_vel) 
            CALL EBCS4(NSEG,EBCS%iseg,SEGVAR,
     .           A,V,X,
     .           EBCS%node_list,NOD,EBCS%elem_list,
     .           EBCS%la,FV,MS,STIFN,twf)     
            end select
         ELSE IF (TYP == 5) THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is (t_ebcs_normv) 
            CALL EBCS5(NSEG,EBCS%iseg,SEGVAR,
     .           A,V,X,
     .           EBCS%node_list,NOD,EBCS%elem_list,
     .           EBCS%la,FV,MS,STIFN,twf)     
            end select
         ELSE IF (TYP == 6) THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is (t_ebcs_inip) 
            CALL EBCS6(NSEG,EBCS%iseg,SEGVAR,
     .           A,V,X,
     .           EBCS%node_list,NOD,EBCS%elem_list,
     .           EBCS%ro0,EBCS%en0,EBCS%p0,
     .           EBCS%vold,EBCS%pold,EBCS%la,
     .           MS,STIFN,IPARG,twf)
            end select
         ELSE IF (TYP == 7) THEN
            select type (twf => EBCS_TAB%tab(i)%poly)
            type is (t_ebcs_iniv) 
            CALL EBCS7(NSEG,EBCS%iseg,SEGVAR,
     .           A,V,X,
     .           EBCS%node_list,NOD,EBCS%elem_list,
     .           EBCS%ro0,EBCS%en0,EBCS%v0,
     .           EBCS%la,MS,STIFN,twf)     
             end select
         ELSE IF (TYP == 10) THEN
            select type (twf => EBCS)
             type is (t_ebcs_nrf)
              CALL EBCS10(NSEG,twf%iseg,SEGVAR,
     .             A,V,W,X,
     .             twf%node_list,NOD,twf%elem_list,twf%ielem,twf%iface,
     .             twf%ro0,twf%en0,
     .             twf%la,MS,STIFN,twf,IPARG,ELBUF_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .             ITAB, NALE, EBCS_PARITHON(I)%ELEM_ADRESS,FSKY)  
            end select  
         ELSE IF (TYP == 100) THEN
            SELECT TYPE(EBCS)
            TYPE IS (t_ebcs_monvol)
              VOLU = EBCS%monvol_id
              CALL EBCS_VOL2SEG(NSEG,IGRSURF(ISU)%NODES,EBCS%iseg,SEGVAR,
     .             A,V,X,VOLMON(NRVOLU*(VOLU-1)+1),FSAV(1,VOLU))
              END SELECT
         ENDIF
      ENDDO
c-----------
      RETURN
      END
      
