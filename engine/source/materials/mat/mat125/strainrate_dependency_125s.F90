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
!||    strainrate_dependency_125S_mod   ../engine/source/materials/mat/mat125/strainrate_dependency_125S.F90
!||--- called by ------------------------------------------------------
!||    sigeps125                   ../engine/source/materials/mat/mat125/sigeps123.F90
!||====================================================================
      module strainrate_dependency_125s_mod
      contains
! ======================================================================================================================
!                                                   SUBROUTINE
! ======================================================================================================================    
!||====================================================================
!||    strainrate_dependency_125s       ../engine/source/materials/mat/mat123/strainrate_dependency_125s.F90
!||--- called by ------------------------------------------------------
!||    sigeps125                  ../engine/source/materials/mat/mat125/sigeps125.F90
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
        subroutine strainrate_dependency_125s( nel, matparam, strain_rate, vartmp , nvartmp,&
                                              em11t,    xt,     em11c,    xc,      &
                                              em22t,    yt,     em22c,    yc,      &
                                              em33t,    zt,     em33c,    zc,      &
                                              gamma,    tau,    ems,      sc,      &
                                              gamma1,   tau1,   ems13,    sc13,    &
                                              gamma2,   tau2,   ems23,    sc23,    &
                                              al1t,     m1t,    al1c,     m1c,     &
                                              al2t,     m2t,    al2c,     m2c,     &
                                              al3t,     m3t,    al3c,      m3c,      &
                                              als,       ms,    als13,    ms13,     &
                                              als23,    ms23,   ef11t,   ef11c,   &
                                              ef22t,     ef22c,  ef33t,   ef33c,  &
                                              efs  ,     efs13, efs23,  epsf )
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
          real(kind=wp), dimension(nel), intent(inout) :: em11t,xt, em11c,xc, em22t,yt, em22c,yc, em33t,zt, &
                                                          em33c,zc, gamma,tau, ems, sc, gamma1,tau1, ems13, &
                                                          sc13, gamma2,tau2, ems23, sc23, epsf , &
                                                          al1t,m1t,al1c,m1c,al2t,m2t,al2c,m2c,al3t,m3t,al3c, &
                                                          m3c,als,ms,als13,ms13,als23,ms23,ef11t, ef11c,   &
                                                          ef22t, ef22c,ef33t,ef33c,efs,efs13, efs23 
                                                          
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
        ifunc(2)  =  matparam%table(2)%notable 
        if(ifunc(2) /= 0) then  ! xt
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
        if(ifunc(6) /= 0) then  ! yt
              ipos(:,1) = vartmp(:,6)
              call table_mat_vinterp(matparam%table(6),nel,nel,ipos,xvec,yy,dydx)
              yt(1:nel)= yy(1:nel)
              vartmp(:,6)=ipos(:,1)
        end if
          ! matrix - compression  dir 2 -
        ifunc(7) = matparam%table(7)%notable
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
          ! Dir 33
          ifunc(9) = matparam%table(9)%notable
        if(ifunc(9) /= 0) then  ! em33t
              ipos(:,1) = vartmp(:,9)
              call table_mat_vinterp(matparam%table(9),nel,nel,ipos,xvec,yy,dydx)
              em33t(1:nel)= yy(1:nel)
              vartmp(:,9)=ipos(:,1)
        end if
        ifunc(10) = matparam%table(10)%notable
        if(ifunc(10) /= 0) then  ! zt
              ipos(:,1) = vartmp(:,10)
              call table_mat_vinterp(matparam%table(10),nel,nel,ipos,xvec,yy,dydx)
              zt(1:nel)= yy(1:nel)
              vartmp(:,10)=ipos(:,1)
        end if
          ! matrix - compression  dir 3 -
        ifunc(11) = matparam%table(11)%notable
        if(ifunc(11) /= 0) then  ! em33c
              ipos(:,1) = vartmp(:,11)
              call table_mat_vinterp(matparam%table(11),nel,nel,ipos,xvec,yy,dydx)
              em33c(1:nel)= yy(1:nel)
              vartmp(:,11)=ipos(:,1)
        end if
        ifunc(12) = matparam%table(12)%notable
        if(ifunc(12) /= 0) then  ! zc
              ipos(:,1) = vartmp(:,12)
              call table_mat_vinterp(matparam%table(12),nel,nel,ipos,xvec,yy,dydx)
              zc(1:nel)= yy(1:nel)
              vartmp(:,12)=ipos(:,1)
        end if
       ! shear  12 - gamma
        ifunc(13) = matparam%table(13)%notable
        if(ifunc(13)/= 0) then  !
              ipos(:,1) = vartmp(:,13)
              call table_mat_vinterp(matparam%table(13),nel,nel,ipos,xvec,yy,dydx)
              gamma(1:nel)= yy(1:nel)
              vartmp(:,13)=ipos(:,1)
        end if
          ! shear  tau
        ifunc = matparam%table(14)%notable
        if(ifunc(14) /= 0) then  ! tau
             ipos(:,1) = vartmp(:,14)
              call table_mat_vinterp(matparam%table(14),nel,nel,ipos,xvec,yy,dydx)
              tau(1:nel)= yy(1:nel)
              vartmp(:,14)=ipos(:,1)
        end if
          ! shear strain 12 - ems
        ifunc(15)= matparam%table(15)%notable
        if(ifunc(15)/= 0) then  ! ems
             ipos(:,1) = vartmp(:,15)
              call table_mat_vinterp(matparam%table(15),nel,nel,ipos,xvec,yy,dydx)
              ems(1:nel)= yy(1:nel)
              vartmp(:,15)=ipos(:,1)
        end if
          ! shear strengh sc
        ifunc = matparam%table(16)%notable
        if(ifunc(16) /= 0) then  ! sc
             ipos(:,1) = vartmp(:,16)
              call table_mat_vinterp(matparam%table(16),nel,nel,ipos,xvec,yy,dydx)
              sc(1:nel)= yy(1:nel)
             vartmp(:,16)=ipos(:,1)
        end if
          ! shear  13 - gamma1
        ifunc(17)= matparam%table(17)%notable
        if(ifunc(17)/= 0) then  !
              ipos(:,1) = vartmp(:,17)
              call table_mat_vinterp(matparam%table(17),nel,nel,ipos,xvec,yy,dydx)
              gamma1(1:nel)= yy(1:nel)
              vartmp(:,17)=ipos(:,1)
         end if
          ! shear  tau1
          ifunc(18) = matparam%table(18)%notable
         if(ifunc(18) /= 0) then  ! tau1
              ipos(:,1) = vartmp(:,18)
              call table_mat_vinterp(matparam%table(18),nel,nel,ipos,xvec,yy,dydx)
              tau1(1:nel)= yy(1:nel)
              vartmp(:,18)=ipos(:,1)
         end if
            ! shear strain 13 - ems13
         ifunc(19) = matparam%table(19)%notable
         if(ifunc(19) /= 0) then  ! em11c
              ipos(:,1) = vartmp(:,19)
              call table_mat_vinterp(matparam%table(19),nel,nel,ipos,xvec,yy,dydx)
              ems13(1:nel)= yy(1:nel)
              vartmp(:,19)=ipos(:,1)
          end if
          ! shear strengh sc13
          ifunc(20) = matparam%table(20)%notable
          if(ifunc(20) /= 0) then  ! sc
              ipos(:,1) = vartmp(:,20)
              call table_mat_vinterp(matparam%table(20),nel,nel,ipos,xvec,yy,dydx)
              sc13(1:nel)= yy(1:nel)
              vartmp(:,20)=ipos(:,1)
          end if
          ! shear  23 - gamma2
           ifunc(21) = matparam%table(21)%notable
           if(ifunc(21) /= 0) then  !
              ipos(:,1) = vartmp(:,21)
              call table_mat_vinterp(matparam%table(21),nel,nel,ipos,xvec,yy,dydx)
              gamma2(1:nel)= yy(1:nel)
              vartmp(:,21)=ipos(:,1)
           end if
            ! shear  tau 2
           ifunc(22)= matparam%table(22)%notable
           if(ifunc(22) /= 0) then  ! tau
             ipos(:,1) = vartmp(:,22)
              call table_mat_vinterp(matparam%table(22),nel,nel,ipos,xvec,yy,dydx)
              tau2(1:nel)= yy(1:nel)
              vartmp(:,22)=ipos(:,1)
           end if
          ! shear strain 23 - ems23
           ifunc(23)= matparam%table(23)%notable
           if(ifunc(23) /= 0) then  !
           ipos(:,1) = vartmp(:,23)
              call table_mat_vinterp(matparam%table(23),nel,nel,ipos,xvec,yy,dydx)
              ems23(1:nel)= yy(1:nel)
              vartmp(:,23)=ipos(:,1)
           end if
          ! shear strengh sc13
          ifunc(24) = matparam%table(24)%notable
          if(ifunc(24)/= 0) then  ! sc
            ipos(:,1) = vartmp(:,24)
              call table_mat_vinterp(matparam%table(24),nel,nel,ipos,xvec,yy,dydx)
              sc23(1:nel)= yy(1:nel)
              vartmp(:,24)=ipos(:,1)
           end if
           ifunc(25) = matparam%table(25)%notable
           if(ifunc(25) /= 0) then  ! mul
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
              if(xc(i) > zero .and. ifunc(4) > 0)then
                ef11c(i)  = xc(i)/e1
                em11c(i) = max(em11c(i), onep1*ef11c(i))
                x = em11c(i)/ef11c(i)
                m1c(i)=  one/log(x)
                al1c(i) = m1c(i)*(x)**m1c(i)
              end if
              if(yt(i) > zero .and. ifunc(6) > 0)then
                ef22t(i)  = yt(i)/e2
                em22t(i) = max(em22t(i), onep1*ef22t(i))
                x = em22t(i)/ef22t(i)
                m2t(i) =  one/log(x)
                al2t(i) = m2t(i)*(x)**m2t(i)
              end if
            !
              if(yc(i) > zero  .and. ifunc(8) > 0)then
                ef22c(i)  = yc(i)/e1
                em22c(i) = max(em22c(i),onep1*ef22c(i))
                x = em22c(i)/ef22c(i)
                m2c(i) =  one/log(x)
                al2c(i) = m2c(i)*(x)**m2c(i)
              end if
              if(zt(i) > zero .and. ifunc(10) > 0 )then
                ef33t(i)  = zt(i)/e3
                em33t(i) = max(em33t(i), onep1*ef33t(i))
                x  = em33t(i)/ef33t(i)
                m3t(i)=  one/log(x)
                al3t(i) = m3t(i)*(x)**m3t(i)
              end if
              if(zc(i) > zero  .and. ifunc(12) > 0 )then
                ef33c(i)  = zc(i)/e1
                em33c(i) = max(em33c(i), onep1*ef33c(i))
                x = em33c(i)/ef33c(i)
                m3c(i)=  one/log(x)
                al3c(i) = m3c(i)*(x)**m3c(i)
              end if
              if(tau(i) > zero  .and. ifunc(14) > 0)then
                efs(i)  = tau(i) /g12
                gamma(i) = max(gamma(i), onep1*efs(i))
                x = gamma(i)/efs(i)
                ms(i) = one /log(x)
                als(i) = x**ms(i)/log(x)
              end if
              if(tau1(i) > zero  .and. ifunc(18) > 0 )then
                efs13(i)  = tau1(i) /g13
                gamma1(i) = max(gamma1(i), onep1*efs13(i))
                x= gamma1(i)/efs13(i)
                ms13(i)= one /log(x)
                als13(i) = x**ms13(i)/log(x) 
              end if
              if(tau2(i) > zero .and. ifunc(22) > 0  )then
                efs23(i)  = tau2(i) /g23
                gamma2(i) = max(gamma2(i), onep1*efs23(i))
                x = gamma2(i)/efs23(i)
                ms23(i) = one /log(x)
                als23(i) = x**ms23(i)/log(x)
              end if
           end do ! nel
        return

       end subroutine strainrate_dependency_125s
     end module strainrate_dependency_125s_mod
