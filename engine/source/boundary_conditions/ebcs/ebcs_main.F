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
!||    ebcs_main          ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||--- called by ------------------------------------------------------
!||    alemain            ../engine/source/ale/alemain.F
!||--- calls      -----------------------------------------------------
!||    ebcs0              ../engine/source/boundary_conditions/ebcs/ebcs0.F
!||    ebcs1              ../engine/source/boundary_conditions/ebcs/ebcs1.F
!||    ebcs10             ../engine/source/boundary_conditions/ebcs/ebcs10.F
!||    ebcs11             ../engine/source/boundary_conditions/ebcs/ebcs11.F90
!||    ebcs4              ../engine/source/boundary_conditions/ebcs/ebcs4.F
!||    ebcs5              ../engine/source/boundary_conditions/ebcs/ebcs5.F
!||    ebcs6              ../engine/source/boundary_conditions/ebcs/ebcs6.F
!||    ebcs7              ../engine/source/boundary_conditions/ebcs/ebcs7.F
!||    ebcs_vol2seg       ../engine/source/boundary_conditions/ebcs/ebcs_vol2seg.F
!||--- uses       -----------------------------------------------------
!||    ebcs_mod           ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    elbufdef_mod       ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    groupdef_mod       ../common_source/modules/groupdef_mod.F
!||    multi_fvm_mod      ../common_source/modules/ale/multi_fvm_mod.F90
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||    segvar_mod         ../engine/share/modules/segvar_mod.F
!||    sensor_mod         ../common_source/modules/sensor_mod.F90
!||    th_surf_mod        ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
      SUBROUTINE EBCS_MAIN(IGRSURF,SEGVAR,VOLMON,A,V,W,X,
     .                     FSAV,FV,MS,STIFN,IPARG,ELBUF_TAB,EBCS_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .                     FSKY,FSAVSURF,TIME,DT1,
     .                     NSENSOR,SENSOR_TAB,PYTHON,
     .                     NPC, TF ,SNPC, STF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SEGVAR_MOD, ONLY : t_segvar
      USE ELBUFDEF_MOD  
      USE GROUPDEF_MOD
      USE EBCS_MOD
      USE MULTI_FVM_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
      USE SENSOR_MOD, ONLY : SENSOR_STR_
      USE PYTHON_FUNCT_MOD
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
      my_real,INTENT(IN) :: DT1 !time step
      my_real,INTENT(IN) :: TIME !simulation time
      my_real,INTENT(INOUT) :: FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF)
      INTEGER IPARG(NPARG,NGROUP)
      INTEGER,INTENT(IN) :: IXQ(NIXQ,NUMELQ),IXS(NIXS,NUMELS),IXTG(NIXTG,NUMELTG)
      my_real VOLMON(*),V(3,NUMNOD),W(3,NUMNOD),A(3,NUMNOD),X(3,NUMNOD),FSAV(NTHVKI,*),
     .        FV(*),STIFN(NUMNOD)
      my_real,intent(inout) :: MS(NUMNOD)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(t_ebcs_tab), TARGET, INTENT(INOUT) :: EBCS_TAB
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
      my_real, DIMENSION(8,LSKY), INTENT(INOUT) :: FSKY ! acceleration array for parith/on option
      INTEGER,INTENT(IN) :: NSENSOR
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE (PYTHON_), INTENT(IN) :: PYTHON
      INTEGER,INTENT(IN) :: SNPC, STF
      INTEGER,INTENT(IN) :: NPC(SNPC)
      my_real :: tf(STF)
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,TYP,ISU,NSEG,VOLU,NOD
      CLASS(t_ebcs), POINTER :: EBCS
      LOGICAL HAS_TH
C-----------------------------------------------
C   S o u r  c e   L i n e s
C-----------------------------------------------

      !/TH/SURF (RESET)
      DO I = 1, NEBCS
         IF(.NOT.EBCS_TAB%need_to_compute(I)) CYCLE     !EBCS may be on domain 0 only depending on ebcs type
         EBCS => EBCS_TAB%tab(i)%poly
         HAS_TH = EBCS%has_th
         IF(HAS_TH)THEN
           ISU = EBCS%surf_id
           FSAVSURF(1:5,ISU) = ZERO
           ! FSAVSURF(6,ISU) is cumulative value : not reset
         ENDIF
      ENDDO

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
     .           FV,MS,STIFN,EBCS_TAB, I)
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
     .           MS,STIFN,twf)
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
     .             twf%la,twf,IPARG,ELBUF_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .             EBCS_PARITHON(I)%ELEM_ADRESS,FSKY,FSAVSURF)
             end select
         ELSE IF (TYP == 11) THEN
            select type (twf => EBCS)
             type is (t_ebcs_propergol)
              call ebcs11(nseg,twf%iseg,segvar,
     .                    v,w,x,
     .                    twf%node_list,nod,twf%elem_list,twf%ielem,twf%iface,
     .                    twf%la,ms,stifn,twf,iparg,elbuf_tab,ixq,ixs,ixtg,
     .                    fsavsurf,time,iparit,dt1,
     .                    numels, numelq, numeltg,numnod, nparg, ngroup, nixs, nixq, nixtg, nsurf, iale, n2d,
     .                    nfunct, npc, tf ,snpc, stf, python,
     .                    nsensor, sensor_tab)
            end select
         ELSE IF (TYP == 100) THEN
            SELECT TYPE(EBCS)
            TYPE IS (t_ebcs_monvol)
              VOLU = EBCS%monvol_id
              CALL EBCS_VOL2SEG(NSEG,IGRSURF(ISU)%NODES,EBCS%iseg,SEGVAR,A,V,X,VOLMON(NRVOLU*(VOLU-1)+1),FSAV(1,VOLU))
              END SELECT
         ENDIF
      ENDDO

c-----------
      RETURN
      END
      
