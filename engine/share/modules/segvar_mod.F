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
      TYPE t_segvar
        INTEGER :: nseg
        INTEGER :: nbmat !number of phases
        logical :: has_rho = .false.
        logical :: has_eint = .false.        
        logical :: has_rk = .false.
        logical :: has_re = .false.  
        logical :: has_uvar = .false.                      
        logical :: has_phase_alpha = .false.  
        logical :: has_phase_rho = .false.    
        logical :: has_phase_eint = .false.            
        my_real,allocatable,dimension(:) :: rho
        my_real,allocatable,dimension(:) :: eint
        my_real,allocatable,dimension(:) :: rk
        my_real,allocatable,dimension(:) :: re  
        my_real,allocatable,dimension(:) :: uvar
        my_real,allocatable,dimension(:,:) :: phase_alpha 
        my_real,allocatable,dimension(:,:) :: phase_rho
        my_real,allocatable,dimension(:,:) :: phase_eint 
      CONTAINS
        procedure, pass :: destroy => segvar_destroy      
        procedure, pass :: create => segvar_create  
      END TYPE t_segvar
      
      CONTAINS
      
      subroutine segvar_destroy(this)
      implicit none
      class(t_segvar), intent(inout) :: this
        if(allocated(this%rho)) deallocate(this%rho)
        if(allocated(this%eint)) deallocate(this%eint)
        if(allocated(this%rk)) deallocate(this%rk)
        if(allocated(this%re)) deallocate(this%re)                
        if(allocated(this%uvar)) deallocate(this%uvar)
        if(allocated(this%phase_alpha)) deallocate(this%phase_alpha) 
        if(allocated(this%phase_alpha)) deallocate(this%phase_rho) 
        if(allocated(this%phase_alpha)) deallocate(this%phase_eint)                 
      end subroutine segvar_destroy
      
      subroutine segvar_create(this,nbmat)
      use ale_mod      
      implicit none
#include "constant.inc"       
#include "../includes/com04_c.inc" 
      class(t_segvar), intent(inout) :: this
       integer,intent(in) :: nbmat
        this%nseg=NSEGFLU
        this%nbmat=nbmat
        if(this%nseg > 0)then
          if(ALE%GLOBAL%NVCONV >= 1)this%has_rho=.true.
          if(ALE%GLOBAL%NVCONV >= 2)this%has_eint=.true.
          if(ALE%GLOBAL%NVCONV >= 3)this%has_rk=.true.
          if(ALE%GLOBAL%NVCONV >= 4)this%has_re=.true.
          if(ALE%GLOBAL%NVCONV >= 5)this%has_uvar=.true.                                
          if(this%has_rho)then
            allocate(this%rho(this%nseg))
            this%rho(1:this%nseg)=ZERO
          endif
          if(this%has_eint)then
            allocate(this%eint(this%nseg))
            this%eint(1:this%nseg)=ZERO
          endif
          if(this%has_rk)then
            allocate(this%rk(this%nseg))
            this%rk(1:this%nseg)=ZERO            
          endif
          if(this%has_re)then
            allocate(this%re(this%nseg))
            this%re(1:this%nseg)=ZERO
          endif
          if(this%has_uvar)then
            allocate(this%uvar(this%nseg))
            this%uvar(1:this%nseg)=ZERO 
          endif
          if(nbmat > 0)then
            this%has_phase_alpha = .true.
            allocate(this%phase_alpha(this%nbmat,this%nseg))
            this%phase_alpha(1:this%nbmat,1:this%nseg) = ZERO
            this%has_phase_rho = .true.
            allocate(this%phase_rho(this%nbmat,this%nseg))
            this%phase_rho(1:this%nbmat,1:this%nseg) = ZERO
            this%has_phase_eint = .true.
            allocate(this%phase_eint(this%nbmat,this%nseg))
            this%phase_eint(1:this%nbmat,1:this%nseg) = ZERO                        
          endif
        endif                               
      end subroutine segvar_create      

      END MODULE SEGVAR_MOD
