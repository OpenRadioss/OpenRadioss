!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!Chd|====================================================================
!Chd|  elbufdef_mod                  modules/mat_elem/elbufdef_mod.F90
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|===================================================================================

      Module elbufdef_mod

!=======================================================================================      
!! \brief  module to define data structure for internal state variables by element group
!! \details 


!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"

!=======================================================================      
!
      Type g_bufel_          ! material and property variables (mean values for each element)
        integer  nvar_glob   
        integer  g_noff   
        integer  g_ierr
        integer  g_off    
        integer  g_gama   
        integer  g_smstr  
        integer  g_hourg  
        integer  g_bfrac  
        integer  g_eint   
        integer  g_eins   
        integer  g_rho    
        integer  g_qvis   
        integer  g_deltax 
        integer  g_vol    
        integer  g_epsd   
        integer  g_epsq
        integer  g_pla    
        integer  g_temp   
        integer  g_tb     
        integer  g_rk     
        integer  g_re     
        integer  g_sig    
        integer  g_for    
        integer  g_mom    
        integer  g_thk    
        integer  g_tag22
        integer  g_stra  
        integer  g_sigi   
        integer  g_dmg   
        integer  g_forpg    
        integer  g_mompg
        integer  g_gama_r   
!
        integer  g_forpgpinch  
        integer  g_mompgpinch 
        integer  g_epgpinchxz
        integer  g_epgpinchyz 
        integer  g_epgpinchzz
!    
        integer  g_strpg
        integer  g_uelr
        integer  g_uelr1
        integer  g_damdl
        integer  g_forth    
        integer  g_eintth    
        integer  g_fill
        integer  g_seq
        integer  g_strw  
        integer  g_strwpg  
        integer  g_thk_i    
        integer  g_jac_i   
        integer  g_dt
        integer  g_isms 
        integer  g_strhg
        integer  g_bpreld        ! bolt preloading
        integer  g_aburn
        integer  g_mu
        integer  g_planl
        integer  g_epsdnl
        integer  g_tempg
        integer  g_cor_nf        ! nodal forces for corotational formulation
        integer  g_cor_fr        ! local frame for corotational formulation
        integer  g_cor_xr        ! reference local coordinates for corotational formulation
        integer  g_maxfrac 
        integer  g_maxeps 
        integer  g_betaorth
        integer  g_amu
        integer  g_tsaiwu
        integer  g_dmgscl
        integer  g_sh_ioffset
!---
!    - 1d - elem (truss, beam, spring) 
        integer  g_area
        integer  g_skew
        integer  g_length
        integer  g_totdepl
        integer  g_totrot
        integer  g_forep
        integer  g_momep
        integer  g_dep_in_tens
        integer  g_dep_in_comp
        integer  g_rot_in_tens
        integer  g_rot_in_comp
        integer  g_posx
        integer  g_posy
        integer  g_posz
        integer  g_posxx
        integer  g_posyy
        integer  g_poszz
        integer  g_yield
        integer  g_length_err
        integer  g_dv
        integer  g_dfs
        integer  g_skew_err
        integer  g_e6
        integer  g_ruptcrit
        integer  g_mass
        integer  g_v_repcvt
        integer  g_vr_repcvt
        integer  g_nuvar
        integer  g_nuvarn
        integer  g_defini
        integer  g_forini
        integer  g_inifric
        integer  g_etotsh
        integer  g_skew_id
!
!    -  for seatbelt elements
        integer  g_slipring_id
        integer  g_slipring_fram_id
        integer  g_slipring_strand
        integer  g_retractor_id
        integer  g_ringslip
        integer  g_add_node
        integer  g_update
        integer  g_fram_factor
        integer  g_intvar
!---
        integer  g_dt_piter
!---
        integer  g_idt_tsh
!-------  max_historic variables     
        integer  g_tm_yield
        integer  g_tm_seq
        integer  g_tm_eint
        integer  g_tm_dmg
        integer  g_tm_sig  ! max(p1)&min(p3) 3 for 2d, 6 for 3d
        integer  g_tm_stra ! max(p1)&min(p3)
!---
        integer, dimension(:) , pointer ::   noff  
        integer, dimension(:) , pointer ::   ierr  
        my_real, dimension(:) , pointer ::   off   
        my_real, dimension(:) , pointer ::   gama  
        double precision, dimension(:) , pointer ::   smstr 
        my_real, dimension(:) , pointer ::   hourg 
        my_real, dimension(:) , pointer ::   bfrac    
        my_real, dimension(:) , pointer ::   eint  
        my_real, dimension(:) , pointer ::   eins  
        my_real, dimension(:) , pointer ::   rho   
        my_real, dimension(:) , pointer ::   qvis       
        my_real, dimension(:) , pointer ::   deltax                 
        my_real, dimension(:) , pointer ::   vol                            
        my_real, dimension(:) , pointer ::   epsd  
        my_real, dimension(:) , pointer ::   epsq
        my_real, dimension(:) , pointer ::   pla   
        my_real, dimension(:) , pointer ::   temp  
        my_real, dimension(:) , pointer ::   tb    
        my_real, dimension(:) , pointer ::   rk    
        my_real, dimension(:) , pointer ::   re    
        my_real, dimension(:) , pointer ::   sig                          
        my_real, dimension(:) , pointer ::   for                        
        my_real, dimension(:) , pointer ::   mom                        
        my_real, dimension(:) , pointer ::   thk                        
        my_real, dimension(:) , pointer ::   tag22                    
        my_real, dimension(:) , pointer ::   stra                     
        my_real, dimension(:) , pointer ::   sigi                     
        my_real, dimension(:) , pointer ::   dmg                     
        my_real, dimension(:) , pointer ::   forpg   ! mean gauss point value    
        my_real, dimension(:) , pointer ::   mompg
        my_real, dimension(:) , pointer ::   gama_r  ! co-rotational local sys  
!
        my_real, dimension(:) , pointer ::   forpgpinch 
        my_real, dimension(:) , pointer ::   mompgpinch
        my_real, dimension(:) , pointer ::   epgpinchxz
        my_real, dimension(:) , pointer ::   epgpinchyz
        my_real, dimension(:) , pointer ::   epgpinchzz
!                        
        my_real, dimension(:) , pointer ::   strpg                  
        my_real, dimension(:) , pointer ::   tempg   
        my_real, dimension(:) , pointer ::   uelr   !  failure global variable
        my_real, dimension(:) , pointer ::   uelr1  !  failure global variable
        my_real, dimension(:) , pointer ::   damdl  !  delamination failure (law25)
        my_real, dimension(:) , pointer ::   forth
        my_real, dimension(:) , pointer ::   eintth                                                
        my_real, dimension(:) , pointer ::   fill
        my_real, dimension(:) , pointer ::   seq
        my_real, dimension(:) , pointer ::   thk_i   !---- add for ismstr=10 shell (xfem not done) strwpg total anti-symme curvature                         
        my_real, dimension(:) , pointer ::   strw 
        my_real, dimension(:) , pointer ::   strwpg
        my_real, dimension(:) , pointer ::   jac_i   !--------inversed [j]
        my_real, dimension(:) , pointer ::   dt
        my_real, dimension(:) , pointer ::   aburn 
        my_real, dimension(:) , pointer ::   mu 
        integer, dimension(:) , pointer ::   isms 
        integer, dimension(:) , pointer ::   sh_ioffset 
        my_real, dimension(:) , pointer ::   bpreld  ! bolt preloading 
        my_real, dimension(:) , pointer ::   cor_nf  ! corotational nodal forces 
        my_real, dimension(:) , pointer ::   cor_fr  ! corotational frame 
        my_real, dimension(:) , pointer ::   cor_xr  ! corotational reference coordinates 
        my_real, dimension(:) , pointer ::   maxfrac
        my_real, dimension(:) , pointer ::   maxeps 
        my_real, dimension(:) , pointer ::   betaorth 
        my_real, dimension(:) , pointer ::   amu
!---
!    - 1d - elem (truss, beam, spring)
        my_real, dimension(:) , pointer ::   area
        my_real, dimension(:) , pointer ::   skew
        my_real, dimension(:) , pointer ::   length
        my_real, dimension(:) , pointer ::   totdepl
        my_real, dimension(:) , pointer ::   totrot
        my_real, dimension(:) , pointer ::   forep
        my_real, dimension(:) , pointer ::   momep
        my_real, dimension(:) , pointer ::   dep_in_tens
        my_real, dimension(:) , pointer ::   dep_in_comp
        my_real, dimension(:) , pointer ::   rot_in_tens
        my_real, dimension(:) , pointer ::   rot_in_comp
        my_real, dimension(:) , pointer ::   posx
        my_real, dimension(:) , pointer ::   posy
        my_real, dimension(:) , pointer ::   posz
        my_real, dimension(:) , pointer ::   posxx
        my_real, dimension(:) , pointer ::   posyy
        my_real, dimension(:) , pointer ::   poszz
        my_real, dimension(:) , pointer ::   yield
        my_real, dimension(:) , pointer ::   length_err
        my_real, dimension(:) , pointer ::   dv
        my_real, dimension(:) , pointer ::   dfs
        my_real, dimension(:) , pointer ::   skew_err
        my_real, dimension(:) , pointer ::   e6
        my_real, dimension(:) , pointer ::   ruptcrit
        my_real, dimension(:) , pointer ::   mass
        my_real, dimension(:) , pointer ::   v_repcvt
        my_real, dimension(:) , pointer ::   vr_repcvt
        my_real, dimension(:) , pointer ::   var
        my_real, dimension(:) , pointer ::   varn
        my_real, dimension(:) , pointer ::   defini
        my_real, dimension(:) , pointer ::   forini
        my_real, dimension(:) , pointer ::   inifric
        my_real, dimension(:) , pointer ::   strhg
        my_real, dimension(:) , pointer ::   etotsh
        integer, dimension(:) , pointer ::   skew_id
        type (fail_loc_) , dimension(:) , pointer ::   fail
!
!    -  for seatbelt elements
        integer, dimension(:) , pointer ::   slipring_id
        integer, dimension(:) , pointer ::   slipring_fram_id
        integer, dimension(:) , pointer ::   slipring_strand
        integer, dimension(:) , pointer ::   retractor_id
        my_real, dimension(:) , pointer ::   ringslip
        integer, dimension(:) , pointer ::   add_node
        integer, dimension(:) , pointer ::   update
        my_real, dimension(:) , pointer ::   fram_factor
        my_real, dimension(:) , pointer ::   intvar
!---
        my_real, dimension(:) , pointer ::   dt_piter ! tetra10 iterative power for time step computation
        integer, dimension(:) , pointer ::   idt_tsh  
!-------  max_historic variables     
        my_real, dimension(:) , pointer ::   tm_yield   
        my_real, dimension(:) , pointer ::   tm_seq   
        my_real, dimension(:) , pointer ::   tm_eint   
        my_real, dimension(:) , pointer ::   tm_dmg   
        my_real, dimension(:) , pointer ::   tm_sig1
        my_real, dimension(:) , pointer ::   tm_stra1
        my_real, dimension(:) , pointer ::   tm_sig3
        my_real, dimension(:) , pointer ::   tm_stra3
!---  work array
        my_real, dimension(:) , pointer ::   tm_psig
        my_real, dimension(:) , pointer ::   tm_pstra
!---
      end type g_bufel_


      Type l_bufel_      ! element variables per integration point
        integer  mlaw    ! material law type          
        integer  lawid   ! material law id    
        my_real, dimension(:) , pointer ::   off    
        my_real, dimension(:) , pointer ::   gama   
        my_real, dimension(:) , pointer ::   stra   
        my_real, dimension(:) , pointer ::   frac          
        my_real, dimension(:) , pointer ::   bfrac
        my_real, dimension(:) , pointer ::   eint   
        my_real, dimension(:) , pointer ::   eins   
        my_real, dimension(:) , pointer ::   rho    
        my_real, dimension(:) , pointer ::   dp_drho
        my_real, dimension(:) , pointer ::   qvis   
        my_real, dimension(:) , pointer ::   deltax 
        my_real, dimension(:) , pointer ::   vol    
        my_real, dimension(:) , pointer ::   epsa   
        my_real, dimension(:) , pointer ::   epsd   
        my_real, dimension(:) , pointer ::   epsq   
        my_real, dimension(:) , pointer ::   epsf   
        my_real, dimension(:) , pointer ::   pla    
        my_real, dimension(:) , pointer ::   temp   
        my_real, dimension(:) , pointer ::   tb     
        my_real, dimension(:) , pointer ::   rk     
        my_real, dimension(:) , pointer ::   re         
        my_real, dimension(:) , pointer ::   vk     
        my_real, dimension(:) , pointer ::   sf     
        my_real, dimension(:) , pointer ::   rob    
        my_real, dimension(:) , pointer ::   dam    
        my_real, dimension(:) , pointer ::   dsum   
        my_real, dimension(:) , pointer ::   dglo   
        my_real, dimension(:) , pointer ::   crak   
        my_real, dimension(:) , pointer ::   ang    
        my_real, dimension(:) , pointer ::   epe    
        my_real, dimension(:) , pointer ::   epc    
        my_real, dimension(:) , pointer ::   xst    
        my_real, dimension(:) , pointer ::   ssp    
        my_real, dimension(:) , pointer ::   z      
        my_real, dimension(:) , pointer ::   visc   
        my_real, dimension(:) , pointer ::   sigl   
        my_real, dimension(:) , pointer ::   sigv   
        my_real, dimension(:) , pointer ::   siga   
        my_real, dimension(:) , pointer ::   sigb   
        my_real, dimension(:) , pointer ::   sigc   
        my_real, dimension(:) , pointer ::   sigd   
        my_real, dimension(:) , pointer ::   sigf   
        my_real, dimension(:) , pointer ::   sig    
        my_real, dimension(:) , pointer ::   sigply    
        my_real, dimension(:) , pointer ::   for    
        my_real, dimension(:) , pointer ::   mom
        my_real, dimension(:) , pointer ::   thk    
        double precision, dimension(:) , pointer ::   smstr    
        my_real, dimension(:) , pointer ::   dmg 
        my_real, dimension(:) , pointer ::   forth
        my_real, dimension(:) , pointer ::   eintth    
        my_real, dimension(:) , pointer ::   seq
        my_real, dimension(:) , pointer ::   jac_i    
        my_real, dimension(:) , pointer ::   fac_yld  
        my_real, dimension(:) , pointer ::   aburn
        my_real, dimension(:) , pointer ::   mu
        my_real, dimension(:) , pointer ::   pij   !--------[ni,j] for imstr10
        double precision, dimension(:) , pointer ::   vol0dp
        my_real, dimension(:) , pointer ::   planl
        my_real, dimension(:) , pointer ::   epsdnl            
        my_real, dimension(:) , pointer ::   dmgscl
        my_real, dimension(:) , pointer ::   tsaiwu
      end type l_bufel_                             

      Type buf_prop_
        my_real, dimension(:)  , pointer ::  var
        my_real, dimension(:)  , pointer ::  varn
      end type buf_prop_

!--------------------------------------------------------------------------------      
!     Non-local buffer for regularization in the shell thickness
      Type buf_nloc_
        my_real, dimension(:,:), pointer :: massth ! embedded wire nodal masses
        my_real, dimension(:,:), pointer :: unlth  ! non-local cumulated variable at nodes
        my_real, dimension(:,:), pointer :: vnlth  ! non-local velocities
        my_real, dimension(:,:), pointer :: fnlth  ! non-local forces
      end type buf_nloc_
!     Non-local buffer for regularization in the thickshell thickness
      Type buf_nlocts_
        my_real, dimension(:,:), pointer :: massth ! embedded wire nodal masses
        my_real, dimension(:,:), pointer :: unlth  ! non-local cumulated variable at nodes
        my_real, dimension(:,:), pointer :: vnlth  ! non-local velocities
        my_real, dimension(:,:), pointer :: fnlth  ! non-local forces
      end type buf_nlocts_
!     Non-local buffer for brick elements geometry configuration
      Type buf_nlocs_
        integer, dimension(:)  , allocatable :: nl_isolnod ! number of effective nodes (nel)
        integer, dimension(:,:), allocatable :: nl_solnod  ! identifiers of effectives nodes (8,nel)
      end type buf_nlocs_
!--------------------------------------------------------------------------------

      Type buf_eos_
        my_real, dimension(:)  , pointer ::  var 
      end type buf_eos_

      Type buf_poro_
        my_real, dimension(:)  , pointer ::  var 
      end type buf_poro_

      Type buf_visc_
!        integer  ilaw    ! type de loi de viscosite
!        integer  nvar
        my_real, dimension(:)  , pointer ::  var 
      end type buf_visc_

      Type buf_xfem_       ! buffer des elements xfem crees par la fissuration
!-------  layer variables     
        integer  ly_smstr
        integer  ly_hourg
        my_real, dimension(:) , pointer ::   dmg
        my_real, dimension(:) , pointer ::   gama
        my_real, dimension(:) , pointer ::   dira
        my_real, dimension(:) , pointer ::   dirb
        my_real, dimension(:) , pointer ::   plapt
        my_real, dimension(:) , pointer ::   sigpt
        my_real, dimension(:) , pointer ::   smstr
        my_real, dimension(:) , pointer ::   hourg
        type (g_bufel_)                                :: xgbuf   ! global variables
        type (l_bufel_)  , dimension(:,:,:)  , pointer :: xlbuf   ! local variables (nptr,npts,nptt)
        type (buf_mat_)  , dimension(:,:,:)  , pointer :: xmat    ! material buffer
        type (buf_fail_) , dimension(:,:,:)  , pointer :: xfail   ! failure models
      end type buf_xfem_
 
      Type fail_loc_
        integer  ilawf    ! type de loi de rupture
        integer  idfail
        integer  nvar
        integer  lf_dam
        integer  lf_dammx
        integer  lf_damini
        integer  lf_tdel
        integer  lf_indx
        integer  lf_off
        integer, dimension(:)  , pointer ::  indx
        integer, dimension(:)  , pointer ::  off
        my_real, dimension(:)  , pointer ::  dam
        my_real, dimension(:)  , pointer ::  var 
        my_real, dimension(:)  , pointer ::  dammx 
        my_real, dimension(:)  , pointer ::  damini
        my_real, dimension(:)  , pointer ::  tdel
      end type fail_loc_

      Type buf_fail_
        type(fail_loc_), dimension(:)  , pointer ::  floc 
      end type buf_fail_

      Type buf_mat_
        my_real, dimension(:)  , pointer ::  var 
        integer, dimension(:)  , pointer ::  vartmp
      end type buf_mat_
!     
      Type l_bufel_dir_      ! element variables per slice in each layer
        my_real, dimension(:) , pointer ::   dira
        my_real, dimension(:) , pointer ::   dirb
      end type l_bufel_dir_                             

      Type buf_lay_
        integer  ilaw     
        integer  imat 
        integer  ieos
        integer  ivisc
        integer  iporo
        integer  nfail
        integer  nvar_mat
        integer  nvar_eos        
        integer  nvartmp
        integer  nvar_visc
        integer  nvar_lay   ! max nb of layer variables = 9
        integer  nvar_loc   ! max nb of local variables in lbuf = 51 (below)
        integer  nptt       ! nb of integration points through layer (pid_51)
!-------
        integer  ly_dmg  
        integer  ly_gama   
        integer  ly_dira   
        integer  ly_dirb   
        integer  ly_crkdir   
        integer  ly_plapt  ! mean plastic strain value between gauss points 
        integer  ly_sigpt  ! mean stress value between gauss points
        integer  ly_hourg
        integer  ly_uelr
        integer  ly_uelr1
        integer  ly_offpg
        integer  ly_off
!-------
        integer  l_off    
        integer  l_gama   
        integer  l_stra   
        integer  l_frac   
        integer  l_bfrac
        integer  l_eint   
        integer  l_eins   
        integer  l_rho    
        integer  l_dp_drho
        integer  l_qvis   
        integer  l_deltax 
        integer  l_vol                        
        integer  l_epsa   
        integer  l_epsd                    
        integer  l_epsq   
        integer  l_epsf   
        integer  l_pla             
        integer  l_temp   
        integer  l_tb     
        integer  l_rk     
        integer  l_re     
        integer  l_vk     
        integer  l_sf     
        integer  l_rob    
        integer  l_dam    
        integer  l_dsum   
        integer  l_dglo   
        integer  l_crak   
        integer  l_ang    
        integer  l_epe    
        integer  l_epc    
        integer  l_xst         
        integer  l_ssp    
        integer  l_z          
        integer  l_visc   
        integer  l_sigl   
        integer  l_sigv   
        integer  l_siga   
        integer  l_sigb   
        integer  l_sigc   
        integer  l_sigd   
        integer  l_sigf   
        integer  l_sig    
        integer  l_sigply ! plyxfem
        integer  l_for    
        integer  l_mom
        integer  l_thk 
        integer  l_smstr      
        integer  l_dmg  
        integer  l_forth
        integer  l_eintth  
        integer  l_seq
        integer  l_jac_i
        integer  l_fac_yld
        integer  l_aburn
        integer  l_mu
        integer  l_pij
        integer  l_vol0dp
        integer  l_planl
        integer  l_epsdnl
        integer  l_dmgscl
        integer  l_tsaiwu
!-------  layer variables     
        my_real, dimension(:) , pointer ::   dmg
        my_real, dimension(:) , pointer ::   gama
        my_real, dimension(:) , pointer ::   dira
        my_real, dimension(:) , pointer ::   dirb
        my_real, dimension(:) , pointer ::   crkdir
        my_real, dimension(:) , pointer ::   plapt
        my_real, dimension(:) , pointer ::   sigpt
        my_real, dimension(:) , pointer ::   hourg
        my_real, dimension(:) , pointer ::   uelr   !  failure layer variable
        my_real, dimension(:) , pointer ::   uelr1  !  failure layer variable
        integer, dimension(:) , pointer ::   offpg  !  failure of gauss point
        integer, dimension(:) , pointer ::   off    !  layer failure flag
!-------       
        type (l_bufel_)  , dimension(:,:,:)  , pointer :: lbuf   ! local variables - per integration point
        type (buf_mat_)  , dimension(:,:,:)  , pointer :: mat    ! material buffer - per integration point
        type (buf_fail_) , dimension(:,:,:)  , pointer :: fail  
        type (buf_prop_) , dimension(:,:,:)  , pointer :: prop 
        type (buf_eos_)  , dimension(:,:,:)  , pointer :: eos  
        type (buf_visc_) , dimension(:,:,:)  , pointer :: visc  
        type (buf_poro_) , dimension(:,:,:)  , pointer :: poro  
        type (buf_xfem_) , dimension(:)      , pointer :: xfem        ! xfem (nxel)
        type (l_bufel_dir_) , dimension(:)  , pointer :: lbuf_dir   ! local direction by int point in the tickness for slice)
      end type buf_lay_
!
!--------------------       
! 
      Type buf_intloc_      ! element variables per integration point
        my_real, dimension(:) , pointer ::   eps   ! (length=3)
        my_real, dimension(:) , pointer ::   sig   ! (length=3)
      end type buf_intloc_                             

      Type buf_intlay_
        integer  ilaw       ! inter ply material law type     
        integer  imat       ! inter ply material number
        integer  nfail
        integer  nvar_mat   ! number of user variables (uvar) in the material buffer
        integer  nvartmp    ! number of temp storage variables (vartmp) in material laws
!------ interlayer variables par couche   (length=1) 
        my_real, dimension(:) , pointer ::   eint         
        my_real, dimension(:) , pointer ::   count
      
        type (buf_intloc_) , dimension(:,:) , pointer :: ilbuf
        type (buf_mat_)    , dimension(:,:) , pointer :: mat
        type (buf_fail_)   , dimension(:,:) , pointer :: fail  
      end type buf_intlay_
!--------------------       
 
      Type elbuf_struct_
        integer     :: igtyp    
        integer     :: nel      
        integer     :: nlay     
        integer     :: nintlay  
        integer     :: nptr     
        integer     :: npts     
        integer     :: nptt   
        integer     :: ixfem
        integer     :: nxel       ! number of xfem parts created after element crack
        integer     :: idrape
       
        type (g_bufel_)                              :: gbuf   ! global variables - mean element values
        type (buf_lay_)   , dimension(:)   , pointer :: bufly  ! bufly(nlay) layer variables 
        type (buf_intlay_), dimension(:)   , pointer :: intlay ! inter-layer (nlay-1)
        type (buf_xfem_)  , dimension(:)   , pointer :: xfem   ! xfem (nxel)
        type (buf_nloc_)  , dimension(:,:) , pointer :: nloc   ! non-local thickness specific structure for shells
        type (buf_nlocts_), dimension(:,:) , pointer :: nlocts ! non-local thickness specific structure for thickshells
        type (buf_nlocs_)                            :: nlocs  ! non-local structure of brick element geometry configuration
      end type elbuf_struct_
!
!---------------
      end module elbufdef_mod
