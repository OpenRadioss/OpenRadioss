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
      module fail_hashin_c_mod
      contains
      subroutine fail_hashin_c(                                                &
        nel       ,nuparam   ,nuvar     ,uparam    ,uvar      ,                &
        time      ,ngl       ,ipg       ,ilay      ,ipt       ,                &                      
        signxx    ,signyy    ,signxy    ,signyz    ,signzx    ,                &
        foff      ,dmg_flag  ,dmg_scale ,lf_dammx  ,dfmax     ,                &
        tdel      ,timestep  ,igtyp     ,ply_id    ,nipar     ,                &
        iparam    )
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
      real(kind=WP), dimension(nel),         intent(in)    :: signyz    !< Current stress component yz
      real(kind=WP), dimension(nel),         intent(in)    :: signzx    !< Current stress component zx
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
      integer :: i,j,jj,nindx,ifail_sh,imodel
      integer ,dimension(nel) :: indx
      real(kind=WP) :: sigt1,sigt2,sigc1,sigc2,csig,fsig12,msig12,msig23,      &
        msig13,angle,sdel,tmax,ratio,f1,f2,f3,f4,f5,sig,p,f6,f7,               &
        tsig12,xsig12,xsig23,dammx, telem,k2m,k0,k1,k2,                        &
        dtinv,km,epspref,filt,alpha,asrate,fcut
      real(kind=WP), dimension(nel) :: sxx,syy,sxy,syz,szx
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Integer material parameter
      ifail_sh = iparam(2)   !< Shear failure flag
      imodel   = iparam(3)   !< Failure model type
      !< Real material parameters
      sigt1    = uparam(1)   !< 
      sigt2    = uparam(2)   !< 
      sigc1    = uparam(4)   !< 
      sigc2    = uparam(5)   !< 
      csig     = uparam(6)   !< Crushing strength
      fsig12   = uparam(7)   !< Shear strength in 1-2 plane
      msig12   = uparam(8)   !< Shear strength in 1-2 plane for matrix failure
      msig13   = uparam(9)   !< Shear strength in 1-3 plane for matrix failure
      msig23   = uparam(10)  !< Shear strength in 2-3 plane for matrix failure
      angle    = uparam(11)  !< Friction angle for matrix failure
      sdel     = uparam(12)  !< Delamination strength ratio
      tmax     = uparam(13)  !< Maximum relaxation time
      fcut     = uparam(14)  !< Frequency cut-off for stress smoothing
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
        syz(i) = asrate*signyz(i) + (one - asrate)*uvar(i,5)
        szx(i) = asrate*signzx(i) + (one - asrate)*uvar(i,6)
        uvar(i,2) = sxx(i)
        uvar(i,3) = syy(i)
        uvar(i,4) = sxy(i)
        uvar(i,5) = syz(i)
        uvar(i,6) = szx(i)
      enddo
!
      !=========================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !=========================================================================
      nindx = 0  
      indx(1:nel) = 0
!
      !< Initialization of element failure index     
      select case (imodel)
!
        !<----------------------------------------------------------------------
        !< Unidirectional lamina model
        !<----------------------------------------------------------------------
        case(1)
!
          !< Loop over the elements
          do i = 1,nel
!
            !< If damage has not been reached yet
            if (dfmax(i,1) < one) then 
!
              !< Reset of local variables        
              f1 = zero
              f2 = zero
              f3 = zero
              f4 = zero
              f5 = zero              
!
              !-----------------------------------------------------------------
              !< Fiber criterion
              !-----------------------------------------------------------------
              !< Tensile/shear fiber mode
              f1  = (max(sxx(i),zero)/sigt1)**2 + (sxy(i)**2+szx(i)**2)/fsig12**2
              dfmax(i,2) = max(dfmax(i,2),f1)
              dfmax(i,2) = min(dfmax(i,2),one)
              !< Compressive fiber mode
              sig = -sxx(i) + max(-half*syy(i),zero)
              if (sig > zero) then
                f2  = (sig/sigc1)**2 
                dfmax(i,3) = max(dfmax(i,3),f2)
                dfmax(i,3) = min(dfmax(i,3),one)
              endif
!
              !-----------------------------------------------------------------
              !< Crush mode criterion
              !-----------------------------------------------------------------
              p = -third*(sxx(i) + syy(i))
              if (p > 0) then
                f3 = (p/csig)**2
                dfmax(i,4) = max(dfmax(i,4),f3)
                dfmax(i,4) = min(dfmax(i,4),one)
              endif
!
              !-----------------------------------------------------------------
              !< Matrix failure criterion
              !-----------------------------------------------------------------
              !< Failure matrix mode
              if (syy(i) < zero) then                           
                xsig12 = msig12 - syy(i)*(tan(angle))           
                xsig23 = msig23 - syy(i)*(tan(angle))           
              else                                                 
                xsig12 = msig12                                    
                xsig23 = msig23                                    
              endif
              f4  = (max(syy(i),zero)/sigt2)**2 + (syz(i)/xsig23)**2 +         &
                    (sxy(i)/xsig12)**2  
              dfmax(i,5) = max(dfmax(i,5),f4)
              dfmax(i,5) = min(dfmax(i,5),one)   
!              
              !< Delamination mode                                     
              f5 = (syz(i)/msig23)**2 + (szx(i)/msig13)**2   
              f5 = sdel*sdel*f5       
              dfmax(i,6) = max(dfmax(i,6),f5)
              dfmax(i,6) = min(dfmax(i,6),one)
!
              !-----------------------------------------------------------------
              !< Update global damage variable
              !-----------------------------------------------------------------    
              dfmax(i,1) = max(dfmax(i,1),f1,f2,f3,f4,f5)
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
        !<----------------------------------------------------------------------
        !< Fabric failure model
        !<---------------------------------------------------------------------- 
        case (2)
          !< Loop over the elements
          do i = 1,nel
!
            !< If damage has not been reached yet
            if (dfmax(i,1) < one) then 
!
              !< Reset of local variables        
              f1 = zero
              f2 = zero
              f3 = zero
              f4 = zero
              f5 = zero    
              f6 = zero
              f7 = zero         
!
              !-----------------------------------------------------------------
              !< Fiber criterion
              !-----------------------------------------------------------------
              !< Tensile/shear fiber modes
              f1  = (max(sxx(i),zero)/sigt1)**2 + (sxy(i)**2+szx(i)**2)/fsig12**2
              dfmax(i,2) = max(dfmax(i,2),f1)
              dfmax(i,2) = min(dfmax(i,2),one)
!
              tsig12 = fsig12*sigt2/sigt1
              f2 = (max(syy(i),zero)/sigt2)**2 + (sxy(i)**2 + syz(i)**2)/tsig12**2      
              dfmax(i,3) = max(dfmax(i,3),f2)
              dfmax(i,3) = min(dfmax(i,3),one)
!
              !< Compressive fiber modes
              if (sxx(i) < zero) then
                f3 = (max(-sxx(i),zero)/sigc1)**2
                dfmax(i,4) = max(dfmax(i,4),f3)
                dfmax(i,4) = min(dfmax(i,4),one)
              endif
!
              if (syy(i) < zero) then
                f4 = (max(-syy(i),zero)/sigc2)**2
                dfmax(i,5) = max(dfmax(i,5),f4)
                dfmax(i,5) = min(dfmax(i,5),one)
              endif
!
              !-----------------------------------------------------------------
              !< Crush mode criterion
              !-----------------------------------------------------------------
              p = -third*(sxx(i) + syy(i))
              if (p > zero) then 
                f5 = (p/csig)**2
                dfmax(i,6) = max(dfmax(i,6),f5)
                dfmax(i,6) = min(dfmax(i,6),one)
              endif
!
              !< Shear Matrix failure
              f6 = (sxy(i)/msig12)**2
              dfmax(i,7) = max(dfmax(i,7),f6)
              dfmax(i,7) = min(dfmax(i,7),one)
!
              !< Matrix failure
              f7 = (syz(i)/msig23)**2 + (szx(i)/msig13)**2
              f7 = sdel*sdel*f7
              dfmax(i,8) = max(dfmax(i,8),f7)
              dfmax(i,8) = min(dfmax(i,8),one)
!
              !-----------------------------------------------------------------
              !< Update global damage variable
              !-----------------------------------------------------------------
              dfmax(i,1) = max(dfmax(i,1),f1,f2,f3,f4,f5,f6,f7)
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
      end select
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
          if (imodel == 1) then 
            if (dfmax(i,2) == one) write(iout, 2000) '1 - TENSILE/SHEAR FIBER'
            if (dfmax(i,3) == one) write(iout, 2000) '2 - COMPRESSION FIBER'
            if (dfmax(i,4) == one) write(iout, 2000) '3 - CRUSH'
            if (dfmax(i,5) == one) write(iout, 2000) '4 - MATRIX FAILURE'
            if (dfmax(i,6) == one) write(iout, 2000) '5 - DELAMINATION'
          elseif (imodel == 2) then
            if (dfmax(i,2) == one) write(iout, 2000) '1 - TENSILE/SHEAR FIBER 1'
            if (dfmax(i,3) == one) write(iout, 2000) '2 - TENSILE/SHEAR FIBER 2'
            if (dfmax(i,4) == one) write(iout, 2000) '3 - COMPRESSION FIBER 1'
            if (dfmax(i,5) == one) write(iout, 2000) '4 - COMPRESSION FIBER 2'
            if (dfmax(i,6) == one) write(iout, 2000) '5 - CRUSH'
            if (dfmax(i,7) == one) write(iout, 2000) '6 - SHEAR FAILURE MATRIX'
            if (dfmax(i,8) == one) write(iout, 2000) '7 - MATRIX FAILURE'
          endif            
        enddo
      endif         
!-------------------------------------------------------------------------------
 1000 format(1X,'FAILURE (HASHIN) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',       &
             I2,1X,',INTEGRATION PT',I3)
 1100 format(1X,'FAILURE (HASHIN) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',        &
             I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 1200 format(1X,'FAILURE (HASHIN) IN SHELL ELEMENT ',I10,1X,',GAUSS PT',        &
             I2,1X,',PLY ID',I10,1X,',INTEGRATION PT',I3)
 2000 format(1X,'MODE',1X,A)
!-------------------------------------------------------------------------------     
      end subroutine fail_hashin_c
      end module fail_hashin_c_mod
