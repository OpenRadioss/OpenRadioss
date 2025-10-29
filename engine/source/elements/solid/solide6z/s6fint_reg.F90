!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    s6fint_reg_mod   ../engine/source/elements/solid/solide6z/s6fint_reg.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3         ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6fint_reg_mod
      contains
      ! ======================================================================================================================
      ! \brief Non-local regularization for 6 nodes PENTA6 solid elements
      ! \details Computation of non-local internal forces and equivalent nodal stiffness accounting for 
      !          gradient effects in the material behaviour.
      ! ======================================================================================================================
!||====================================================================
!||    s6fint_reg       ../engine/source/elements/solid/solide6z/s6fint_reg.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3         ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    mvsiz_mod        ../engine/share/spe_inc/mvsiz_mod.F90
!||    nlocal_reg_mod   ../common_source/modules/nlocal_reg_mod.F
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine s6fint_reg(                                                   &
         nloc_dmg ,var_reg ,nel      ,off      ,vol      ,nodadt   ,           &
         nc1      ,nc2     ,nc3      ,nc4      ,nc5      ,nc6      ,           &
         px1      ,px2     ,px3      ,px4      ,px5      ,px6      ,           &
         py1      ,py2     ,py3      ,py4      ,py5      ,py6      ,           &     
         pz1      ,pz2     ,pz3      ,pz4      ,pz5      ,pz6      ,           &
         imat     ,itask   ,dt2t     ,vol0     ,nft      ,iparit   ,           &
         dtfac1   )
!-----------------------------------------------
!   M o d u l e s
!----------------------------------------------- 
      use nlocal_reg_mod
      use elbufdef_mod
      use constant_mod
      use precision_mod, only : wp
      use mvsiz_mod, only : mvsiz
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      type(nlocal_str_), intent(inout), target :: nloc_dmg !< non-local data structure
      real(kind=WP), dimension(nel), intent(in) :: var_reg !< variable to regularize
      integer, intent(in)     :: nel                       !< number of elements in the group
      real(kind=WP), dimension(nel), intent(in) :: off     !< flag for element deletion
      real(kind=WP), dimension(mvsiz), intent(in) :: vol   !< current volume of the element
      integer, intent(in)     :: nodadt                    !< Nodal adaptive time step flag
      integer, dimension(nel) :: nc1                       !< node 1 of the elements
      integer, dimension(nel) :: nc2                       !< node 2 of the elements
      integer, dimension(nel) :: nc3                       !< node 3 of the elements
      integer, dimension(nel) :: nc4                       !< node 4 of the elements
      integer, dimension(nel) :: nc5                       !< node 5 of the elements
      integer, dimension(nel) :: nc6                       !< node 6 of the elements
      real(kind=WP), dimension(nel), intent(in) :: px1     !< derivative of the first node shape function along x
      real(kind=WP), dimension(nel), intent(in) :: px2     !< derivative of the second node shape function along x
      real(kind=WP), dimension(nel), intent(in) :: px3     !< derivative of the third node shape function along x
      real(kind=WP), dimension(nel), intent(in) :: px4     !< derivative of the fourth node shape function along x
      real(kind=WP), dimension(nel), intent(in) :: px5     !< derivative of the fifth node shape function along x
      real(kind=WP), dimension(nel), intent(in) :: px6     !< derivative of the sixth node shape function along x
      real(kind=WP), dimension(nel), intent(in) :: py1     !< derivative of the first node shape function along y
      real(kind=WP), dimension(nel), intent(in) :: py2     !< derivative of the second node shape function along y
      real(kind=WP), dimension(nel), intent(in) :: py3     !< derivative of the third node shape function along y
      real(kind=WP), dimension(nel), intent(in) :: py4     !< derivative of the third node shape function along y
      real(kind=WP), dimension(nel), intent(in) :: py5     !< derivative of the fifth node shape function along y
      real(kind=WP), dimension(nel), intent(in) :: py6     !< derivative of the sixth node shape function along y
      real(kind=WP), dimension(nel), intent(in) :: pz1     !< derivative of the first node shape function along z
      real(kind=WP), dimension(nel), intent(in) :: pz2     !< derivative of the second node shape function along z
      real(kind=WP), dimension(nel), intent(in) :: pz3     !< derivative of the third node shape function along z
      real(kind=WP), dimension(nel), intent(in) :: pz4     !< derivative of the third node shape function along z
      real(kind=WP), dimension(nel), intent(in) :: pz5     !< derivative of the fifth node shape function along z
      real(kind=WP), dimension(nel), intent(in) :: pz6     !< derivative of the sixth node shape function along z
      integer, intent(in)     :: imat                      !< material internal id
      integer, intent(in)     :: itask                     !< number of the thread
      real(kind=WP), intent(inout)  :: dt2t                !< element critical timestep
      real(kind=WP), dimension(nel), intent(in) :: vol0    !< initial volume of the element
      integer, intent(in)     :: nft                       !< address of the first element of the group
      integer, intent(in)     :: iparit                    !< parallel option
      real(kind=WP), dimension(102), intent(in) :: dtfac1  !< factor for nodal adaptive time step
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer i,ii,k,nnod,n1,n2,n3,n4,n5,n6,l_nloc
      integer, dimension(:), allocatable :: pos1,pos2,pos3,pos4,pos5,pos6
      real(kind = WP) :: l2,ntn_unl,ntn_vnl,xi,ntvar,a,b1,b2,b3,b4,b5,b6
      real(kind = WP) :: zeta,sspnl,dtnl,le_max,maxstif,ntn
      real(kind=WP), dimension(:) ,allocatable ::                              &
        btb11,btb12,btb13,btb14,btb15,btb16,                                   &
        btb22,btb23,btb24,btb25,btb26,                                         &
        btb33,btb34,btb35,btb36,                                               &
        btb44,btb45,btb46,                                                     &
        btb55,btb56,                                                           &
        btb66
      real(kind=WP), dimension(:) ,allocatable :: sti1,sti2,sti3,sti4,sti5,sti6
      real(kind=WP), dimension(:) ,allocatable :: f1,f2,f3,f4,f5,f6,lc
      real(kind=WP), pointer, dimension(:) :: vnl,fnl,unl,stifnl,mass,mass0,vnl0
      ! coefficient for non-local stability to take into account damping
      real(kind=WP), target :: nothing(1)
!=======================================================================
!
      !< Initialization of pointers
      nothing = zero
      vnl => nothing
      fnl => nothing
      unl => nothing
      stifnl => nothing
      mass => nothing
      mass0 => nothing
      vnl0 => nothing
!      
      !< Recover non-local parameters
      l2     = nloc_dmg%len(imat)**2 !< Squared internal length
      xi     = nloc_dmg%damp(imat)   !< Damping parameter
      l_nloc = nloc_dmg%l_nloc       !< Size of non-local tables
      zeta   = nloc_dmg%dens(imat)   !< Density parameter
      sspnl  = nloc_dmg%sspnl(imat)  !< Non-local "Sound speed"
      le_max = nloc_dmg%le_max(imat) !< Maximal length of convergence
!
      !< Allocation of variable table
      allocate(                                                                &
            btb11(nel),btb12(nel),btb13(nel),btb14(nel),btb15(nel),btb16(nel), &
            btb22(nel),btb23(nel),btb24(nel),btb25(nel),btb26(nel),            &
            btb33(nel),btb34(nel),btb35(nel),btb36(nel),                       &
            btb44(nel),btb45(nel),btb46(nel),                                  &
            btb55(nel),btb56(nel),                                             &
            btb66(nel))
      allocate(pos1(nel),pos2(nel),pos3(nel),pos4(nel),pos5(nel),pos6(nel))
      allocate(f1(nel),f2(nel),f3(nel),f4(nel),f5(nel),f6(nel),lc(nel))
!
      !< Factor for non-local forces
      ntn = six*six
!
      !< Local variables initialization
      lc(1:nel) = zero
      !< For nodal timestep
      if (nodadt > 0) then
        !< Non-local nodal stifness
        allocate(sti1(nel),sti2(nel),sti3(nel),sti4(nel),sti5(nel),sti6(nel))
        !< Non-local mass
        mass  => nloc_dmg%mass(1:l_nloc)
        !< Initial non-local mass
        mass0 => nloc_dmg%mass0(1:l_nloc)
      endif
      !< Current timestep non-local velocities
      vnl  => nloc_dmg%vnl(1:l_nloc)
      !< Previous timestep non-local velocities
      vnl0 => nloc_dmg%vnl_old(1:l_nloc)
      !< Non-local displacements
      unl  => nloc_dmg%unl(1:l_nloc)
!
      !--------------------------------------------------------------------------------
      ! Computation of the position of the non-local d.o.fs and the BtB matrix product
      !--------------------------------------------------------------------------------
      !< Loop over elements
      do i=1,nel
!
        ! Recovering the nodes of the brick element
        n1 = nloc_dmg%idxi(nc1(i))
        n2 = nloc_dmg%idxi(nc2(i))
        n3 = nloc_dmg%idxi(nc3(i))
        n4 = nloc_dmg%idxi(nc4(i))
        n5 = nloc_dmg%idxi(nc5(i))
        n6 = nloc_dmg%idxi(nc6(i))
!
        !< Recovering the positions of the first d.o.fs of each nodes
        pos1(i) = nloc_dmg%posi(n1)
        pos2(i) = nloc_dmg%posi(n2)
        pos3(i) = nloc_dmg%posi(n3)
        pos4(i) = nloc_dmg%posi(n4) 
        pos5(i) = nloc_dmg%posi(n5)
        pos6(i) = nloc_dmg%posi(n6)
!
        !< Computation of the product BtxB 
        btb11(i) = px1(i)**2 + py1(i)**2 + pz1(i)**2
        btb12(i) = px1(i)*px2(i) + py1(i)*py2(i) + pz1(i)*pz2(i)
        btb13(i) = px1(i)*px3(i) + py1(i)*py3(i) + pz1(i)*pz3(i)
        btb14(i) = px1(i)*px4(i) + py1(i)*py4(i) + pz1(i)*pz4(i)
        btb15(i) = px1(i)*px5(i) + py1(i)*py5(i) + pz1(i)*pz5(i)
        btb16(i) = px1(i)*px6(i) + py1(i)*py6(i) + pz1(i)*pz6(i)
        btb22(i) = px2(i)**2 + py2(i)**2 + pz2(i)**2
        btb23(i) = px2(i)*px3(i) + py2(i)*py3(i) + pz2(i)*pz3(i)
        btb24(i) = px2(i)*px4(i) + py2(i)*py4(i) + pz2(i)*pz4(i)
        btb25(i) = px2(i)*px5(i) + py2(i)*py5(i) + pz2(i)*pz5(i)
        btb26(i) = px2(i)*px6(i) + py2(i)*py6(i) + pz2(i)*pz6(i)
        btb33(i) = px3(i)**2 + py3(i)**2 + pz3(i)**2
        btb34(i) = px3(i)*px4(i) + py3(i)*py4(i) + pz3(i)*pz4(i)
        btb35(i) = px3(i)*px5(i) + py3(i)*py5(i) + pz3(i)*pz5(i)
        btb36(i) = px3(i)*px6(i) + py3(i)*py6(i) + pz3(i)*pz6(i)
        btb44(i) = px4(i)**2 + py4(i)**2 + pz4(i)**2
        btb45(i) = px4(i)*px5(i) + py4(i)*py5(i) + pz4(i)*pz5(i)
        btb46(i) = px4(i)*px6(i) + py4(i)*py6(i) + pz4(i)*pz6(i)
        btb55(i) = px5(i)**2 + py5(i)**2 + pz5(i)**2
        btb56(i) = px5(i)*px6(i) + py5(i)*py6(i) + pz5(i)*pz6(i)
        btb66(i) = px6(i)**2 + py6(i)**2 + pz6(i)**2
!
      enddo
!    
      !-----------------------------------------------------------------------
      ! Computation of non-local forces
      !-----------------------------------------------------------------------
      !< Loop over elements
      do i = 1, nel
!   
        !< If the element is not broken, normal computation
        if (off(i) /= zero) then 
!
          !< Computing the product NtN*UNL
          ntn_unl = (unl(pos1(i)) + unl(pos2(i)) + unl(pos3(i))                &
                   + unl(pos4(i)) + unl(pos5(i)) + unl(pos6(i))) / ntn
!        
          !< Computing the product NtN*VNL
          ntn_vnl = (vnl(pos1(i)) + vnl(pos2(i)) + vnl(pos3(i))                &
                   + vnl(pos4(i)) + vnl(pos5(i)) + vnl(pos6(i))) / ntn
          if (nodadt > 0) then 
            ntn_vnl = min(sqrt(mass(pos1(i))/mass0(pos1(i))),                  & 
                          sqrt(mass(pos2(i))/mass0(pos2(i))),                  & 
                          sqrt(mass(pos3(i))/mass0(pos3(i))),                  & 
                          sqrt(mass(pos4(i))/mass0(pos4(i))),                  &
                          sqrt(mass(pos5(i))/mass0(pos5(i))),                  & 
                          sqrt(mass(pos6(i))/mass0(pos6(i))))*ntn_vnl
          endif
!     
          !< Computation of the product LEN**2 * BtxB
          b1 = l2 * vol(i) * ( btb11(i)*unl(pos1(i)) + btb12(i)*unl(pos2(i))   &
                             + btb13(i)*unl(pos3(i)) + btb14(i)*unl(pos4(i))   &
                             + btb15(i)*unl(pos5(i)) + btb16(i)*unl(pos6(i)) )
!        
          b2 = l2 * vol(i) * ( btb12(i)*unl(pos1(i)) + btb22(i)*unl(pos2(i))   &
                             + btb23(i)*unl(pos3(i)) + btb24(i)*unl(pos4(i))   &
                             + btb25(i)*unl(pos5(i)) + btb26(i)*unl(pos6(i)) )
!        
          b3 = l2 * vol(i) * ( btb13(i)*unl(pos1(i)) + btb23(i)*unl(pos2(i))   &
                             + btb33(i)*unl(pos3(i)) + btb34(i)*unl(pos4(i))   &
                             + btb35(i)*unl(pos5(i)) + btb36(i)*unl(pos6(i)) )
!        
          b4 = l2 * vol(i) * ( btb14(i)*unl(pos1(i)) + btb24(i)*unl(pos2(i))   &
                             + btb34(i)*unl(pos3(i)) + btb44(i)*unl(pos4(i))   &
                             + btb45(i)*unl(pos5(i)) + btb46(i)*unl(pos6(i)) )
!       
          b5 = l2 * vol(i) * ( btb15(i)*unl(pos1(i)) + btb25(i)*unl(pos2(i))   &
                             + btb35(i)*unl(pos3(i)) + btb45(i)*unl(pos4(i))   &
                             + btb55(i)*unl(pos5(i)) + btb56(i)*unl(pos6(i)) )
!       
          b6 = l2 * vol(i) * ( btb16(i)*unl(pos1(i)) + btb26(i)*unl(pos2(i))   &
                             + btb36(i)*unl(pos3(i)) + btb46(i)*unl(pos4(i))   &
                             + btb56(i)*unl(pos5(i)) + btb66(i)*unl(pos6(i)) )         
!
          !< Multiplication by the volume of the element (and damping parameter XI)
          ntn_unl = ntn_unl * vol(i)
          ntn_vnl = ntn_vnl * xi * vol(i)
!
          !< Introducing the internal variable to be regularized
          ntvar   = var_reg(i)*one_over_6* vol(i)
!
          !< Computing the elementary non-local forces
          a = ntn_unl + ntn_vnl - ntvar
          f1(i) = a + b1
          f2(i) = a + b2
          f3(i) = a + b3
          f4(i) = a + b4
          f5(i) = a + b5
          f6(i) = a + b6
!
          ! Computing nodal equivalent stiffness
          if (nodadt > 0) then 
            sti1(i) = (abs(l2*btb11(i) + one/ntn) + abs(l2*btb12(i) + one/ntn) &
                     + abs(l2*btb13(i) + one/ntn) + abs(l2*btb14(i) + one/ntn) &
                     + abs(l2*btb15(i) + one/ntn) + abs(l2*btb16(i) + one/ntn))*vol(i)
            sti2(i) = (abs(l2*btb12(i) + one/ntn) + abs(l2*btb22(i) + one/ntn) &
                     + abs(l2*btb23(i) + one/ntn) + abs(l2*btb24(i) + one/ntn) &
                     + abs(l2*btb25(i) + one/ntn) + abs(l2*btb26(i) + one/ntn))*vol(i)
            sti3(i) = (abs(l2*btb13(i) + one/ntn) + abs(l2*btb23(i) + one/ntn) &
                     + abs(l2*btb33(i) + one/ntn) + abs(l2*btb34(i) + one/ntn) &
                     + abs(l2*btb35(i) + one/ntn) + abs(l2*btb36(i) + one/ntn))*vol(i)
            sti4(i) = (abs(l2*btb14(i) + one/ntn) + abs(l2*btb24(i) + one/ntn) &
                     + abs(l2*btb34(i) + one/ntn) + abs(l2*btb44(i) + one/ntn) &
                     + abs(l2*btb45(i) + one/ntn) + abs(l2*btb46(i) + one/ntn))*vol(i)
            sti5(i) = (abs(l2*btb15(i) + one/ntn) + abs(l2*btb25(i) + one/ntn) &
                     + abs(l2*btb35(i) + one/ntn) + abs(l2*btb45(i) + one/ntn) &
                     + abs(l2*btb55(i) + one/ntn) + abs(l2*btb56(i) + one/ntn))*vol(i)
            sti6(i) = (abs(l2*btb16(i) + one/ntn) + abs(l2*btb26(i) + one/ntn) &
                     + abs(l2*btb36(i) + one/ntn) + abs(l2*btb46(i) + one/ntn) &
                     + abs(l2*btb56(i) + one/ntn) + abs(l2*btb66(i) + one/ntn))*vol(i)
          endif
!            
        !< If the element is broken, the non-local wave is absorbed  
        else
!
          !< Initial element characteristic length
          lc(i) = vol0(i)**third  
!
          if (nodadt > 0) then
            !< Non-local absorbing forces
            f1(i) = sqrt(mass(pos1(i))/mass0(pos1(i)))*zeta*sspnl*half*        &
                        (vnl(pos1(i))+vnl0(pos1(i)))*(two_third)*(lc(i)**2)
            f2(i) = sqrt(mass(pos2(i))/mass0(pos2(i)))*zeta*sspnl*half*        &
                        (vnl(pos2(i))+vnl0(pos2(i)))*(two_third)*(lc(i)**2)
            f3(i) = sqrt(mass(pos3(i))/mass0(pos3(i)))*zeta*sspnl*half*        &
                        (vnl(pos3(i))+vnl0(pos3(i)))*(two_third)*(lc(i)**2)
            f4(i) = sqrt(mass(pos4(i))/mass0(pos4(i)))*zeta*sspnl*half*        &
                        (vnl(pos4(i))+vnl0(pos4(i)))*(two_third)*(lc(i)**2)
            f5(i) = sqrt(mass(pos5(i))/mass0(pos5(i)))*zeta*sspnl*half*        &
                        (vnl(pos5(i))+vnl0(pos5(i)))*(two_third)*(lc(i)**2)
            f6(i) = sqrt(mass(pos6(i))/mass0(pos6(i)))*zeta*sspnl*half*        &
                        (vnl(pos6(i))+vnl0(pos6(i)))*(two_third)*(lc(i)**2)
            !< Computing nodal equivalent stiffness
            sti1(i) = em20
            sti2(i) = em20
            sti3(i) = em20
            sti4(i) = em20
            sti5(i) = em20
            sti6(i) = em20
          else
            !< Non-local absorbing forces
            f1(i) = zeta*sspnl*half*(vnl(pos1(i))+vnl0(pos1(i)))*              &
                                          (two_third)*(lc(i)**2)
            f2(i) = zeta*sspnl*half*(vnl(pos2(i))+vnl0(pos2(i)))*              &
                                          (two_third)*(lc(i)**2)
            f3(i) = zeta*sspnl*half*(vnl(pos3(i))+vnl0(pos3(i)))*              &
                                          (two_third)*(lc(i)**2)
            f4(i) = zeta*sspnl*half*(vnl(pos4(i))+vnl0(pos4(i)))*              &
                                          (two_third)*(lc(i)**2)
            f5(i) = zeta*sspnl*half*(vnl(pos5(i))+vnl0(pos5(i)))*              & 
                                          (two_third)*(lc(i)**2)
            f6(i) = zeta*sspnl*half*(vnl(pos6(i))+vnl0(pos6(i)))*              & 
                                          (two_third)*(lc(i)**2)       
          endif
        endif
      enddo
!
      !-------------------------------------------------------------------------
      !< Assemblage       
      !-------------------------------------------------------------------------
      !< If PARITH/OFF
      if (iparit == 0) then 
        fnl => nloc_dmg%fnl(1:l_nloc,itask+1)
        if (nodadt > 0) stifnl => nloc_dmg%stifnl(1:l_nloc,itask+1) !< Non-local equivalent nodal stiffness
        !< Loop over elements
        do i=1,nel
          !< Assembling the forces in the classic way 
          fnl(pos1(i)) = fnl(pos1(i)) - f1(i)
          fnl(pos2(i)) = fnl(pos2(i)) - f2(i)
          fnl(pos3(i)) = fnl(pos3(i)) - f3(i)
          fnl(pos4(i)) = fnl(pos4(i)) - f4(i)
          fnl(pos5(i)) = fnl(pos5(i)) - f5(i)
          fnl(pos6(i)) = fnl(pos6(i)) - f6(i)    
          if (nodadt > 0) then
            !< Spectral radius of stiffness matrix
            maxstif = max(sti1(i),sti2(i),sti3(i),sti4(i),sti5(i),sti6(i))
            !< Computing nodal stiffness
            stifnl(pos1(i)) = stifnl(pos1(i)) + maxstif
            stifnl(pos2(i)) = stifnl(pos2(i)) + maxstif
            stifnl(pos3(i)) = stifnl(pos3(i)) + maxstif
            stifnl(pos4(i)) = stifnl(pos4(i)) + maxstif
            stifnl(pos5(i)) = stifnl(pos5(i)) + maxstif
            stifnl(pos6(i)) = stifnl(pos6(i)) + maxstif
          endif
        enddo
!
      !< If PARITH/ON
      else
!
        !< Loop over elements
        do i=1,nel
          ii  = i + nft
!
          !< Spectral radius of stiffness matrix
          if (nodadt > 0) then
            maxstif = max(sti1(i),sti2(i),sti3(i),sti4(i),sti5(i),sti6(i))
          endif
!            
          k = nloc_dmg%iads(1,ii)
          nloc_dmg%fsky(k,1) = -f1(i)
          if (nodadt > 0) nloc_dmg%stsky(k,1) = maxstif
!
          k = nloc_dmg%iads(2,ii)
          nloc_dmg%fsky(k,1) = -f2(i)
          if (nodadt > 0) nloc_dmg%stsky(k,1) = maxstif
!
          k = nloc_dmg%iads(3,ii)
          nloc_dmg%fsky(k,1) = -f3(i)
          if (nodadt > 0) nloc_dmg%stsky(k,1) = maxstif
!
          k = nloc_dmg%iads(5,ii)
          nloc_dmg%fsky(k,1) = -f4(i)
          if (nodadt > 0) nloc_dmg%stsky(k,1) = maxstif
!
          k = nloc_dmg%iads(6,ii)
          nloc_dmg%fsky(k,1) = -f5(i)
          if (nodadt > 0) nloc_dmg%stsky(k,1) = maxstif
!
          k = nloc_dmg%iads(7,ii)
          nloc_dmg%fsky(k,1) = -f6(i)
          if (nodadt > 0) nloc_dmg%stsky(k,1) = maxstif
!
        enddo
      endif
!   
      !-------------------------------------------------------------------------
      !< Computing non-local timestep
      !-------------------------------------------------------------------------
      if (nodadt == 0) then
        do i = 1,nel
          !< If the element is not broken, normal computation
          if (off(i)/=zero) then
            !< Non-local critical time-step in the plane
            dtnl = (two*(min(vol(i)**third,le_max))*sqrt(three*zeta))/         &
                         sqrt(twelve*l2 + (min(vol(i)**third,le_max))**2)
            !< Retaining the minimal value
            dt2t = min(dt2t,dtfac1(1)*cdamp*dtnl)
          endif
        enddo
      endif
!
      !< Deallocation of tables
      if (allocated(btb11)) deallocate(btb11)
      if (allocated(btb12)) deallocate(btb12)
      if (allocated(btb13)) deallocate(btb13)
      if (allocated(btb14)) deallocate(btb14)
      if (allocated(btb15)) deallocate(btb15)
      if (allocated(btb16)) deallocate(btb16)
      if (allocated(btb22)) deallocate(btb22)
      if (allocated(btb23)) deallocate(btb23)      
      if (allocated(btb24)) deallocate(btb24)
      if (allocated(btb25)) deallocate(btb25)
      if (allocated(btb26)) deallocate(btb26)
      if (allocated(btb33)) deallocate(btb33)
      if (allocated(btb34)) deallocate(btb34)
      if (allocated(btb35)) deallocate(btb35)
      if (allocated(btb36)) deallocate(btb36)
      if (allocated(btb44)) deallocate(btb44)
      if (allocated(btb45)) deallocate(btb45)
      if (allocated(btb46)) deallocate(btb46)
      if (allocated(btb55)) deallocate(btb55)
      if (allocated(btb56)) deallocate(btb56)
      if (allocated(btb66)) deallocate(btb66)
      if (allocated(pos1))  deallocate(pos1)
      if (allocated(pos2))  deallocate(pos2) 
      if (allocated(pos3))  deallocate(pos3)
      if (allocated(pos4))  deallocate(pos4) 
      if (allocated(pos5))  deallocate(pos5)
      if (allocated(pos6))  deallocate(pos6) 
      if (allocated(f1))    deallocate(f1)
      if (allocated(f2))    deallocate(f2) 
      if (allocated(f3))    deallocate(f3)
      if (allocated(f4))    deallocate(f4) 
      if (allocated(f5))    deallocate(f5)
      if (allocated(f6))    deallocate(f6) 
      if (allocated(sti1))  deallocate(sti1)
      if (allocated(sti2))  deallocate(sti2) 
      if (allocated(sti3))  deallocate(sti3)
      if (allocated(sti4))  deallocate(sti4) 
      if (allocated(sti5))  deallocate(sti5)
      if (allocated(sti6))  deallocate(sti6) 
      if (allocated(lc))    deallocate(lc)
!
      end subroutine s6fint_reg
      end module s6fint_reg_mod
