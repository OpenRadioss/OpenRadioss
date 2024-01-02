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
Chd|  SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        ALE51_ANTIDIFF2               source/ale/ale51/ale51_antidiff2.F
Chd|        ALE51_ANTIDIFF3               source/ale/ale51/ale51_antidiff3.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALEMUSCL_UPWIND               source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              source/ale/alemuscl/alemuscl_upwind2.F
Chd|        ALETHE                        source/ale/alethe.F           
Chd|        EBCS0                         source/boundary_conditions/ebcs/ebcs0.F
Chd|        EBCS1                         source/boundary_conditions/ebcs/ebcs1.F
Chd|        EBCS10                        source/boundary_conditions/ebcs/ebcs10.F
Chd|        EBCS4                         source/boundary_conditions/ebcs/ebcs4.F
Chd|        EBCS5                         source/boundary_conditions/ebcs/ebcs5.F
Chd|        EBCS6                         source/boundary_conditions/ebcs/ebcs6.F
Chd|        EBCS7                         source/boundary_conditions/ebcs/ebcs7.F
Chd|        EBCS_VOL2SEG                  source/boundary_conditions/ebcs/ebcs_vol2seg.F
Chd|        GRADIENT_RECONSTRUCTION       source/ale/alemuscl/gradient_reconstruction.F
Chd|        GRADIENT_RECONSTRUCTION2      source/ale/alemuscl/gradient_reconstruction2.F
Chd|        I12M2S                        source/interfaces/interf/i12m2s.F
Chd|        I12NOD3                       source/interfaces/interf/i12nod3.F
Chd|        I12S2M                        source/interfaces/interf/i12s2m.F
Chd|        INTTI0                        source/interfaces/interf/intti0.F
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SEGGETV                       source/interfaces/interf/seggetv.F
Chd|        SPMD_SEGCOM                   source/mpi/fluid/spmd_cfd.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SEGVAR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
      TYPE T_SEGVAR
        INTEGER :: NSEG
        INTEGER :: NBMAT !NUMBER OF PHASES
        LOGICAL :: HAS_RHO = .FALSE.
        LOGICAL :: HAS_EINT = .FALSE.        
        LOGICAL :: HAS_RK = .FALSE.
        LOGICAL :: HAS_RE = .FALSE.  
        LOGICAL :: HAS_UVAR = .FALSE.                      
        LOGICAL :: HAS_PHASE_ALPHA = .FALSE.  
        LOGICAL :: HAS_PHASE_RHO = .FALSE.    
        LOGICAL :: HAS_PHASE_EINT = .FALSE.            
        MY_REAL,ALLOCATABLE,DIMENSION(:) :: RHO
        MY_REAL,ALLOCATABLE,DIMENSION(:) :: EINT
        MY_REAL,ALLOCATABLE,DIMENSION(:) :: RK
        MY_REAL,ALLOCATABLE,DIMENSION(:) :: RE  
        MY_REAL,ALLOCATABLE,DIMENSION(:) :: UVAR
        MY_REAL,ALLOCATABLE,DIMENSION(:,:) :: PHASE_ALPHA 
        MY_REAL,ALLOCATABLE,DIMENSION(:,:) :: PHASE_RHO
        MY_REAL,ALLOCATABLE,DIMENSION(:,:) :: PHASE_EINT 
      CONTAINS
        PROCEDURE, PASS :: DESTROY => SEGVAR_DESTROY      
        PROCEDURE, PASS :: CREATE => SEGVAR_CREATE  
      END TYPE T_SEGVAR
      
      CONTAINS
      
Chd|====================================================================
Chd|  SEGVAR_DESTROY                share/modules/segvar_mod.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SEGVAR_DESTROY(this)
      IMPLICIT NONE
      CLASS(T_SEGVAR), INTENT(INOUT) :: this
        IF(ALLOCATED(this%RHO)) DEALLOCATE(this%RHO)
        IF(ALLOCATED(this%EINT)) DEALLOCATE(this%EINT)
        IF(ALLOCATED(this%RK)) DEALLOCATE(this%RK)
        IF(ALLOCATED(this%RE)) DEALLOCATE(this%RE)                
        IF(ALLOCATED(this%UVAR)) DEALLOCATE(this%UVAR)
        IF(ALLOCATED(this%PHASE_ALPHA)) DEALLOCATE(this%PHASE_ALPHA) 
        IF(ALLOCATED(this%PHASE_ALPHA)) DEALLOCATE(this%PHASE_RHO) 
        IF(ALLOCATED(this%PHASE_ALPHA)) DEALLOCATE(this%PHASE_EINT)                 
      END SUBROUTINE SEGVAR_DESTROY
      
Chd|====================================================================
Chd|  SEGVAR_CREATE                 share/modules/segvar_mod.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE SEGVAR_CREATE(this,NBMAT)
      USE ALE_MOD      
      IMPLICIT NONE
#include "constant.inc"       
#include "../includes/com04_c.inc" 
      CLASS(T_SEGVAR), INTENT(INOUT) :: this
       INTEGER,INTENT(IN) :: NBMAT
        this%NSEG=NSEGFLU
        this%NBMAT=NBMAT
        IF(this%NSEG > 0)THEN
          IF(ALE%GLOBAL%NVCONV >= 1)this%HAS_RHO=.TRUE.
          IF(ALE%GLOBAL%NVCONV >= 2)this%HAS_EINT=.TRUE.
          IF(ALE%GLOBAL%NVCONV >= 3)this%HAS_RK=.TRUE.
          IF(ALE%GLOBAL%NVCONV >= 4)this%HAS_RE=.TRUE.
          IF(ALE%GLOBAL%NVCONV >= 5)this%HAS_UVAR=.TRUE.                                
          IF(this%HAS_RHO)THEN
            ALLOCATE(this%RHO(this%NSEG))
            this%RHO(1:this%NSEG)=ZERO
          ENDIF
          IF(this%HAS_EINT)THEN
            ALLOCATE(this%EINT(this%NSEG))
            this%EINT(1:this%NSEG)=ZERO
          ENDIF
          IF(this%HAS_RK)THEN
            ALLOCATE(this%RK(this%NSEG))
            this%RK(1:this%NSEG)=ZERO            
          ENDIF
          IF(this%HAS_RE)THEN
            ALLOCATE(this%RE(this%NSEG))
            this%RE(1:this%NSEG)=ZERO
          ENDIF
          IF(this%HAS_UVAR)THEN
            ALLOCATE(this%UVAR(this%NSEG))
            this%UVAR(1:this%NSEG)=ZERO 
          ENDIF
          IF(NBMAT > 0)THEN
            this%HAS_PHASE_ALPHA = .TRUE.
            ALLOCATE(this%PHASE_ALPHA(this%NBMAT,this%NSEG))
            this%PHASE_ALPHA(1:this%NBMAT,1:this%NSEG) = ZERO
            this%HAS_PHASE_RHO = .TRUE.
            ALLOCATE(this%PHASE_RHO(this%NBMAT,this%NSEG))
            this%PHASE_RHO(1:this%NBMAT,1:this%NSEG) = ZERO
            this%HAS_PHASE_EINT = .TRUE.
            ALLOCATE(this%PHASE_EINT(this%NBMAT,this%NSEG))
            this%PHASE_EINT(1:this%NBMAT,1:this%NSEG) = ZERO                        
          ENDIF
        ENDIF                               
      END SUBROUTINE SEGVAR_CREATE      

      END MODULE SEGVAR_MOD
