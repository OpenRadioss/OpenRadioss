!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    fail_changchang_c_mod   ../engine/source/materials/fail/changchang/fail_changchang_c.F90
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
!||====================================================================
      module fail_changchang_c_mod
      contains
!||====================================================================
!||    fail_changchang_c   ../engine/source/materials/fail/changchang/fail_changchang_c.F90
!||--- called by ------------------------------------------------------
!||    mulawc              ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell       ../engine/source/materials/mat_share/usermat_shell.F
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine fail_changchang_c(                                            &
        nel       ,nuparam   ,nuvar     ,uparam    ,uvar      ,                &
        time      ,ngl       ,ipg       ,ilay      ,ipt       ,                &                      
        signxx    ,signyy    ,signxy    ,foff      ,dmg_flag  ,                &
        dmg_scale ,lf_dammx  ,dfmax     ,tdel      ,timestep  ,                &
        igtyp     ,ply_id    ,nipar     ,iparam    )
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use precision_mod , only : WP
      use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include "units_c.inc"
!-----------------------------------------------
!   A r g u m e n t s
!-----------------------------------------------
      integer,                               intent(in)    :: nel       !< Number of elements in the group
      integer,                               intent(in)    :: nuparam   !< Number of material parameters
      integer,                               intent(in)    :: nuvar     !< Number of user variables
      real(kind=WP), dimension(nuparam)  ,   intent(in)    :: uparam    !< Failure criterion real parameters
      real(kind=WP), dimension(nel,nuvar),   intent(inout) :: uvar      !< User variables array
      real(kind=WP),                         intent(in)    :: time      !< Current time
      integer,       dimension(nel),         intent(in)    :: ngl       !< Global element numbers
      integer,                               intent(in)    :: ipg       !< Gauss point number
      integer,                               intent(in)    :: ilay      !< Layer number
      integer,                               intent(in)    :: ipt       !< Integration point number
      real(kind=WP), dimension(nel),         intent(in)    :: signxx    !< Current stress component xx
      real(kind=WP), dimension(nel),         intent(in)    :: signyy    !< Current stress component yy
      real(kind=WP), dimension(nel),         intent(in)    :: signxy    !< Current stress component xy
      integer,       dimension(nel),         intent(inout) :: foff      !< Integration point failure active/inactive flag  
      integer,                               intent(inout) :: dmg_flag  !< Damage flag
      real(kind=WP), dimension(nel),         intent(inout) :: dmg_scale !< Damage scaling factor
      integer,                               intent(in)    :: lf_dammx  !< Size of damage variable array
      real(kind=WP), dimension(nel,lf_dammx),intent(inout) :: dfmax     !< Damage variables
      real(kind=WP), dimension(nel),         intent(inout) :: tdel      !< Time of damage completion
      real(kind=WP),                         intent(in)    :: timestep  !< Current time step
      integer,                               intent(in)    :: igtyp     !< Property type
      integer,                               intent(in)    :: ply_id    !< Ply ID
      integer,                               intent(in)    :: nipar     !< Number of integer material parameters
      integer,       dimension(nipar),       intent(in)    :: iparam    !< Integer material parameters
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,nindx,ifail_sh
      integer ,dimension(nel) :: indx
      real(kind=WP) :: sigt1,sigt2,sigt12,sigc1,sigc2,beta,tmax,fcut,asrate,   &
        damft,damfc,dammt,dammc
      real(kind=WP), dimension(nel) :: sxx,syy,sxy
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Integer material parameter
      ifail_sh = iparam(2)   !< Shear failure flag
      !< Real material parameter
      sigt1    = uparam(1)      
      sigt2    = uparam(2)      
      sigt12   = uparam(3)      
      sigc1    = uparam(4)      
      sigc2    = uparam(5)      
      beta     = uparam(6)      
      tmax     = uparam(7)      
      fcut     = uparam(8)  !< Frequency cut-off for stress smoothing
      if (fcut > zero) then
        asrate = two*pi*fcut*timestep
        asrate = asrate/(one+asrate)
      else
        asrate = one
      endif 
!
      !< Flag for damage softening
      dmg_flag = 1    
!
      !< Stress filtering if activated
      do i = 1,nel
        sxx(i) = asrate*signxx(i) + (one - asrate)*uvar(i,2)
        syy(i) = asrate*signyy(i) + (one - asrate)*uvar(i,3)
        sxy(i) = asrate*signxy(i) + (one - asrate)*uvar(i,4)
        uvar(i,2) = sxx(i)
        uvar(i,3) = syy(i)
        uvar(i,4) = sxy(i)
      enddo
!
      !=========================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !=========================================================================
      nindx = 0  
      indx(1:nel) = 0
!
      !< Loop over the elements
      do i = 1,nel
!
        !< Reset of local variables        
        damft = zero
        damfc = zero
        dammt = zero
        dammc = zero
!
        !< If damage has not been reached yet
        if (dfmax(i,1) < one) then
!
          !-----------------------------------------------------------------
          !< Fiber criterion
          !-----------------------------------------------------------------    
          !< Tensile fiber mode          
          if (sxx(i) >= zero) then
            damft = (sxx(i)/sigt1)**2 + beta*(sxy(i)/sigt12)**2
            damfc = zero
            dfmax(i,2) = max(damft,dfmax(i,2))
            dfmax(i,2) = min(dfmax(i,2),one)
          !< Compression fiber mode
          else 
            damfc = (sxx(i)/sigc1)**2
            damft = zero
            dfmax(i,3) = max(damfc,dfmax(i,3))
            dfmax(i,3) = min(dfmax(i,3),one)
          endif 
!
          !---------------------------------------------------------------------
          !< Matrix failure criterion
          !---------------------------------------------------------------------
          if (ifail_sh < 3) then 
            if (syy(i) >= zero) then                                   
              dammt = (syy(i)/sigt2)**2 + (sxy(i)/sigt12)**2                    
              dammc = zero   
              dfmax(i,4) = max(dammt,dfmax(i,4))
              dfmax(i,4) = min(dfmax(i,4),one)     
            else                                                        
              dammc = (syy(i)/(two*sigt12))**2 + (sxy(i)/sigt12)**2            &                           
                      + syy(i)*((sigc2/(two*sigt12))**2 - one)/sigc2 
              dammt = zero
              dfmax(i,5) = max(dammc,dfmax(i,5))
              dfmax(i,5) = min(dfmax(i,5),one)  
            endif 
          endif
!
          !-----------------------------------------------------------------
          !< Update global damage variable
          !-----------------------------------------------------------------    
          dfmax(i,1) = max(dfmax(i,1),dfmax(i,2),dfmax(i,3),dfmax(i,4),dfmax(i,5))
          dfmax(i,1) = min(one,dfmax(i,1))
!
          !< Check if criterion has been reached
          if (dfmax(i,1) >= one) then
            nindx = nindx+1                                    
            indx(nindx) = i   
            if (ifail_sh > 0) then 
              uvar(i,1) = time
            endif                                  
          endif 
        endif  
!
        !< Stress relaxation in case of damage reached
        if ((uvar(i,1) > zero).and.(foff(i) /= 0).and.(ifail_sh > 0)) then 
          dmg_scale(i) = exp(-(time - uvar(i,1))/tmax)   
          if (dmg_scale(i) < em02) then
            foff(i) = 0
            tdel(i) = time
            dmg_scale(i) = zero
          endif
        endif 
      enddo                
!      
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED MODES
      !==================================================================== 
      if (nindx > 0) then 
        do j=1,nindx    
          i = indx(j)
          if (igtyp == 17 .or. igtyp == 51 .or. igtyp == 52) then 
            write(iout, 1200) ngl(i),ipg,ply_id,ipt
            write(istdo,1200) ngl(i),ipg,ply_id,ipt
          elseif (igtyp == 1 .or. igtyp == 9) then 
            write(iout, 1000) ngl(i),ipg,ipt
            write(istdo,1000) ngl(i),ipg,ipt
          else 
            write(iout, 1100) ngl(i),ipg,ilay,ipt
            write(istdo,1100) ngl(i),ipg,ilay,ipt
          endif
          if (dfmax(i,2) == one) write(iout, 2000) '1 - TENSILE FIBER'
          if (dfmax(i,3) == one) write(iout, 2000) '2 - COMPRESSION FIBER'
          if (dfmax(i,4) == one) write(iout, 2000) '3 - TENSILE MATRIX'
          if (dfmax(i,5) == one) write(iout, 2000) '4 - COMPRESSION MATRIX'
        enddo
      endif         
!-------------------------------------------------------------------------------
 1000 format(1X,'FAILURE (CHANG) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',        &
             I2,1X,',INTEGRATION PT',I3)
 1100 format(1X,'FAILURE (CHANG) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',        &
             I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 1200 format(1X,'FAILURE (CHANG) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',        &
             I2,1X,',PLY ID',I10,1X,',INTEGRATION PT',I3)
 2000 format(1X,'MODE',1X,A)
!-------------------------------------------------------------------------------     
      end subroutine fail_changchang_c
      end module fail_changchang_c_mod
