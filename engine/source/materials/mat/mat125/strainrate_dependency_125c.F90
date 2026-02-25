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
!||    strainrate_dependency_125c_mod   ../engine/source/materials/mat/mat125/strainrate_dependency_125c.F90
!||--- called by ------------------------------------------------------
!||    sigeps125c                  ../engine/source/materials/mat/mat125/sigeps125c.F90
!||====================================================================
      module strainrate_dependency_125c_mod
      contains
! ======================================================================================================================
!                                                   SUBROUTINE
! ======================================================================================================================    
!||====================================================================
!||    strainrate_dependency_125c       ../engine/source/materials/mat/mat125/strainrate_dependency_125c.F90
!||--- called by ------------------------------------------------------
!||    sigeps125c                  ../engine/source/materials/mat/mat125/sigeps125c.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp           ../engine/source/materials/tools/table_mat_vinterp.F
!||    table_mat_vinterp_inv       ../engine/source/materials/tools/table_mat_vinterp_inv.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                ../common_source/modules/constant_mod.F
!||    matparam_def_mod            ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod               ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_inv_mod   ../engine/source/materials/tools/table_mat_vinterp_inv.F90
!||    table_mat_vinterp_mod       ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine strainrate_dependency_125c( nel, matparam, strain_rate, vartmp , nvartmp,&
                                              em11t,    xt,     em11c,    xc,      &
                                              em22t,    yt,     em22c,    yc,      &
                                              gamma,    tau,    ems,      sc,      &
                                              al1t,     m1t,    al1c,     m1c,     &
                                              al2t,     m2t,    al2c,     m2c,     &
                                              als,       ms,    ef11t,   ef11c,   &
                                              ef22t,     ef22c,   efs  ,  epsf )
         ! 
        use precision_mod, only : WP 
        use constant_mod 
        use matparam_def_mod 
        use table_mat_vinterp_inv_mod , only : table_mat_vinterp_inv
        use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
         implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ---------------------------------------------------------------------------------------------------------------------- 
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nvartmp !< number of user variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< user variables temporairy 
          real(kind=wp), dimension(nel), intent(in) :: strain_rate !< strain rate
          real(kind=wp), dimension(nel), intent(inout) :: em11t,xt, em11c,xc, em22t,yt, em22c,yc, &
                                                          gamma,tau, ems, sc,  epsf ,  al1t,m1t, &
                                                          al1c,m1c,al2t,m2t,al2c,m2c,   als,ms,ef11t, ef11c,   &
                                                          ef22t, ef22c ,efs
                                                          
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------   
         integer :: ipos(nel,1), ifunc(25),nfunc,i
         real(kind=wp) :: xvec(nel,1),dydx(nel),yy(nel)
         real(kind=wp) :: e1,e2,e3,g12,g13,g23,x
! ----------------------------------------------------------------------------------------------------------------------
!                                                   coding 
! ---------------------------------------------------------------------------------------------------------------------- 
        e1    = matparam%uparam(1)
        e2    = matparam%uparam(2)
        e3    = matparam%uparam(3)
        g12   = matparam%uparam(4)
        g13   = matparam%uparam(5)
        g23   = matparam%uparam(6)    
        !
        nfunc =  matparam%ntable 
        xvec(1:nel,1) =strain_rate(1:nel) 
        ifunc(1:25) = 0
        ifunc(1) =  matparam%table(1)%notable 
      ! xt rate computation
        if(ifunc(1) /= 0) then  ! em11t
               ipos(:,1) = vartmp(:,1)
               call table_mat_vinterp(matparam%table(1),nel,nel,ipos,xvec,yy,dydx)
               em11t(1:nel) = yy(1:nel)
              vartmp(:,1)=ipos(:,1)
        end if
          !
        ifunc(2) =  matparam%table(2)%notable 
        if(ifunc(2)/= 0) then  ! xt
              ipos(:,1) = vartmp(:,2)
              call table_mat_vinterp(matparam%table(2),nel,nel,ipos,xvec,yy,dydx)
              xt(1:nel)= yy(1:nel)
              vartmp(:,2)=ipos(:,1)
        end if
          ! fiber - compression  dir 1 -
        ifunc(3) = matparam%table(3)%notable
        if(ifunc(3) /= 0) then  ! em11c
              ipos(:,1) = vartmp(:,3)
              call table_mat_vinterp(matparam%table(3),nel,nel,ipos,xvec,yy,dydx)
              em11c(1:nel)= yy(1:nel)
              vartmp(:,3)=ipos(:,1)
         end if
         ifunc(4) = matparam%table(4)%notable
         if(ifunc(4) /= 0) then  ! xc
              ipos(:,1) = vartmp(:,4)
              call table_mat_vinterp(matparam%table(4),nel,nel,ipos,xvec,yy,dydx)
              xc(1:nel)= yy(1:nel)
              vartmp(:,4)=ipos(:,1)
        end if
          ! matrix - tension dir 2 -
        ifunc(5) = matparam%table(5)%notable
        if(ifunc(5) /= 0) then  ! em22t
              ipos(:,1) = vartmp(:,5)
              call table_mat_vinterp(matparam%table(5),nel,nel,ipos,xvec,yy,dydx)
              em22t(1:nel)= yy(1:nel)
              vartmp(:,5)=ipos(:,1)
        end if
          !
        ifunc(6) = matparam%table(6)%notable
        if(ifunc(6)  /= 0) then  ! yt
              ipos(:,1) = vartmp(:,6)
              call table_mat_vinterp(matparam%table(6),nel,nel,ipos,xvec,yy,dydx)
              yt(1:nel)= yy(1:nel)
              vartmp(:,6)=ipos(:,1)
        end if
          ! matrix - compression  dir 2 -
        ifunc(7)= matparam%table(7)%notable
        if(ifunc(7) /= 0) then  ! em22c
              ipos(:,1) = vartmp(:,7)
              call table_mat_vinterp(matparam%table(7),nel,nel,ipos,xvec,yy,dydx)
              em22c(1:nel)= yy(1:nel)
              vartmp(:,7)=ipos(:,1)
        end if

        ifunc(8) = matparam%table(8)%notable
        if(ifunc(8) /= 0) then  ! yc
              ipos(:,1) = vartmp(:,8)
              call table_mat_vinterp(matparam%table(8),nel,nel,ipos,xvec,yy,dydx)
              yc(1:nel)= yy(1:nel)
              vartmp(:,8)=ipos(:,1)
        end if
       ! shear  12 - gamma
        ifunc(13)  = matparam%table(13)%notable
        if(ifunc(13)  /= 0) then  !
              ipos(:,1) = vartmp(:,13)
              call table_mat_vinterp(matparam%table(13),nel,nel,ipos,xvec,yy,dydx)
              gamma(1:nel)= yy(1:nel)
              vartmp(:,13)=ipos(:,1)
        end if
          ! shear  tau
        ifunc(14)  = matparam%table(14)%notable
        if(ifunc(14) /= 0) then  ! tau
             ipos(:,1) = vartmp(:,14)
              call table_mat_vinterp(matparam%table(14),nel,nel,ipos,xvec,yy,dydx)
              tau(1:nel)= yy(1:nel)
              vartmp(:,14)=ipos(:,1)
        end if
          ! shear strain 12 - ems
        ifunc(15)  = matparam%table(15)%notable
        if(ifunc(15)  /= 0) then  ! ems
             ipos(:,1) = vartmp(:,15)
              call table_mat_vinterp(matparam%table(15),nel,nel,ipos,xvec,yy,dydx)
              ems(1:nel)= yy(1:nel)
              vartmp(:,15)=ipos(:,1)
        end if
          ! shear strengh sc
        ifunc(16) = matparam%table(16)%notable
        if(ifunc(16) /= 0) then  ! sc
             ipos(:,1) = vartmp(:,16)
              call table_mat_vinterp(matparam%table(16),nel,nel,ipos,xvec,yy,dydx)
              sc(1:nel)= yy(1:nel)
             vartmp(:,16)=ipos(:,1)
        end if
        ifunc(25) = matparam%table(25)%notable
        if(ifunc(25) /= 0) then  ! 
               ipos(:,1) = vartmp(:,25)
                call table_mat_vinterp(matparam%table(25),nel,nel,ipos,xvec,yy,dydx)
                epsf(1:nel)= yy(1:nel)
                vartmp(:,25)=ipos(:,1)
        endif
            ! Computing the damage parameters
        do i=1,nel
              if(xt(i) > zero .and. ifunc(2) > 0  )then
                ef11t(i)  = xt(i)/e1
                em11t(i) = max(em11t(i), onep1*ef11t(i))
                x= em11t(i)/ef11t(i)
                m1t(i)=  one/log(x)
                al1t(i) = m1t(i)*(x)**m1t(i)
              end if
              if(xc(i) > zero  .and. ifunc(4) > 0 )then
                ef11c(i)  = xc(i)/e1
                em11c(i) = max(em11c(i), onep1*ef11c(i))
                x = em11c(i)/ef11c(i)
                m1c(i)=  one/log(x)
                al1c(i) = m1c(i)*(x)**m1c(i)
              end if
              if(yt(i) > zero .and. ifunc(6) > 0 )then
                ef22t(i)  = yt(i)/e2
                em22t(i) = max(em22t(i), onep1*ef22t(i))
                x = em22t(i)/ef22t(i)
                m2t(i) =  one/log(x)
                al2t(i) = m2t(i)*(x)**m2t(i)
              end if
            !
              if(yc(i) > zero  .and. ifunc(8) > 0 )then
                ef22c(i)  = yc(i)/e1
                em22c(i) = max(em22c(i),onep1*ef22c(i))
                x = em22c(i)/ef22c(i)
                m2c(i) =  one/log(x)
                al2c(i) = m2c(i)*(x)**m2c(i)
              end if
              if(tau(i) > zero .and. ifunc(14) > 0  )then
                efs(i)  = tau(i) /g12
                gamma(i) = max(gamma(i), onep1*efs(i))
                x = gamma(i)/efs(i)
                ms(i) = one /log(x)
                als(i) = x**ms(i)/log(x)
              end if
           end do ! nel
          return
       end subroutine strainrate_dependency_125c
     end module strainrate_dependency_125c_mod
