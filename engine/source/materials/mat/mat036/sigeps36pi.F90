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
!===================================================================================================

!||=============================================================================
!||    sigeps36pi_mod   ../engine/source/materials/mat/mat036/sigeps36pi.F90
!||--- called by ------------------------------------------------------
!||    mulaw_ib     ../engine/source/elements/beam/mulaw_ib.F
!||=============================================================================
      module sigeps36pi_mod
      contains

!||====================================================================
!||    sigeps36pi   ../engine/source/materials/mat/mat036/sigeps36pi.F90
!||--- called by ------------------------------------------------------
!||    mulaw_ib     ../engine/source/elements/beam/mulaw_ib.F
!||--- calls      -----------------------------------------------------
!||    finter       ../engine/source/tools/curve/finter.F
!||    vinter       ../engine/source/tools/curve/vinter.F
!||====================================================================
      subroutine sigeps36pi(mat_param,                               &
                 nel     ,ipt     ,ngl     ,nvartmp ,vartmp  ,       &
                 off     ,epsd    ,etse    ,pla     ,sigy    ,       &
                 depsxx  ,depsxy  ,depsxz  ,                         &   
                 sigoxx  ,sigoxy  ,sigoxz  ,                         &      
                 signxx  ,signxy  ,signxz  )      
! ------------------------------------------------------------------------------
!       modules
! ------------------------------------------------------------------------------
          use matparam_def_mod
          use table_mat_vinterp_mod
          use precision_mod, only : wp
          use constant_mod
          use file_descriptor_mod
!-----------------------------------------------
!       i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!       i n p u t   a r g u m e n t s
!-----------------------------------------------
        integer nel,ipt,nvartmp
        integer ngl(nel)
        real(kind=wp) ,dimension(nel) ,intent(in)    :: epsd
        real(kind=wp) ,dimension(nel) ,intent(in)    :: depsxx,depsxy,depsxz
        real(kind=wp) ,dimension(nel) ,intent(in)    :: sigoxx,sigoxy,sigoxz
        real(kind=wp) ,dimension(nel) ,intent(inout) :: signxx,signxy,signxz
        real(kind=wp) ,dimension(nel) ,intent(inout) :: pla,etse,off,sigy
        integer ,intent(inout) :: vartmp(nel,nvartmp)
        type(matparam_struct_) ,intent(in) :: mat_param
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
        integer :: i,ii,nindx
        integer :: ndim,ismooth,ifail,func_e
        integer ,dimension(nel) :: indx
        real(kind=wp) :: young,shear,nu,r,umr,fac,gs,svm,shfact
        real(kind=wp) :: einf,ce
        real(kind=wp) ,dimension(nel) :: e,g,g3,yld,h,dydx,epsmax,escale
        real(kind=wp) ,dimension(nel,1) :: xvec1
        real(kind=wp) ,dimension(nel,2) :: xvec2
!=========================================================================================
        shfact = five_over_6
!
        young    = mat_param%young
        shear    = mat_param%shear
        nu       = mat_param%nu      
!
        ismooth  = mat_param%iparam(1) ! strain rate interpolation flag (linear/log)
        ifail    = mat_param%iparam(2) ! failure flag 
!
        epsmax   = mat_param%uparam(3)
        ce       = mat_param%uparam(8) 
        einf     = mat_param%uparam(9)  

        ndim     = mat_param%table(1)%ndim
        func_e   = mat_param%table(3)%notable
!        
!------------------------------------------
!       elastic parameters
!------------------------------------------
        nindx=0
        do i=1,nel         
          if (pla(i) > zero .and. off(i) == one) then
            nindx=nindx+1
            indx(nindx)=i
          endif
        enddo
!
        if (func_e > 0) then                                                                 
          xvec1(1:nel,1) = pla(1:nel)                                                        
          call table_mat_vinterp(mat_param%table(3),nel,nel,vartmp(1,4),xvec1,escale,dydx)   
          e(1:nel) = escale(1:nel)* young                                                    
          g(1:nel) = half * e(1:nel)/(one+nu)                                   
        elseif (ce /= zero) then       ! variable young modulus defined analytically                                               
          do ii=1,nindx                                                                     
            i = indx(ii)                                                                     
            e(i) = young - (young-einf)*(one-exp(-ce*pla(i)))                                
            g(i) = half * e(i)/(one+nu)                                   
          end do                                                                             
        else                           ! constant Young modulus
          e(1:nel) = young
          g(1:nel)  = shear
        end if
        g3(1:nel) = g(1:nel) * three
!
!-------------------
!--- transverse shear module
!       do i=1,nel
!         sh  = five_over_6*g(i)                        
!         yma = twelve*e(i)/al(i)**2              
!         sh1 = yma*iyy(i)                       
!         sh2 = yma*izz(i)                       
!         shf = ishear   ( no shear = 1, shear = 0 = default)
!         sh0 = (one - shf(i))*sh              
!         gs1 = sh0*sh1/(sh+sh1) + shf(i)*sh1 
!         gs2 = sh0*sh2/(sh+sh2) + shf(i)*sh2 
!      enddo                                    
!
!-------------------
!---   elastic stress                              
        do i=1,nel                                 
          gs = shfact*g(i)                         
          signxx(i) = sigoxx(i) + e(i)*depsxx(i)   
          signxy(i) = sigoxy(i) + gs*depsxy(i)     
          signxz(i) = sigoxz(i) + gs*depsxz(i)     
          etse(i)   = one                          
        enddo                                      
!-------------------
!       yield stress
!-------------------
        if (ndim == 1) then   ! only static curve => no strain rate interpolation
          xvec1(1:nel,1) = pla(1:nel)
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,yld,h)
        else
          xvec2(1:nel,1) = pla(1:nel)
          if (ismooth == 2) then ! log interpolation
            xvec2(1:nel,2) = log(epsd(1:nel))
          else
            xvec2(1:nel,2) = epsd(1:nel)
          end if
          call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,yld,h)
        end if
        sigy(1:nel) = yld(1:nel)                      
!-------------------
!       projection - radial return 
!-------------------
        do i=1,nel                                                     
          svm = signxx(i)**2 + three*(signxy(i)**2 + signxz(i)**2)     
          svm = sqrt(svm)                                             
          r  = min( one, yld(i)/max(em20,svm) )                         
          signxx(i)=signxx(i)*r                                         
          signxy(i)=signxy(i)*r                                         
          signxz(i)=signxz(i)*r                                         
          umr = one - r                                                  
          pla(i) = pla(i) + off(i)*svm*umr/(g3(i)+h(i))
          if (r < one) etse(i)= h(i)/(h(i)+e(i))                         
        enddo                                                           
!                                                        
!---    plasticite uniaxiale : 3 iterations newton                      
!tmp        do j=1,nel                                                     
!tmp          aa(i) = signxx(i)*signxx(i)                               
!tmp          bb(i) = three*signxy(i)*signxy(i)                         
!tmp          cc(i) = three*signxz(i)*signxz(i)                         
!tmp          svm(i)= aa(i) + bb(i) + cc(i)                             
!tmp        enddo                                                           
!tmpc       plastic flow                                                
!tmp        nindx=0                                                     
!tmp        do i=1,nel                                                 
!tmp          if(svm(i)>yld(i).and.off(i)==1.) then                
!tmp            nindx = nindx+1                                         
!tmp            index(nindx) = i                                        
!tmp          endif                                                     
!tmp        enddo                                                       
!tmpc def plastics in uniaxial constraint                      
!tmp        do j=1,nindx                                                
!tmp          i=index(j)                                                
!tmp          dpla_j(i) = (svm(i)-yld(i))/(g3(i)+h(i))                  
!tmp          etse(i)   = h(i)/(h(i)+e(i))                              
!tmp        enddo                                                       
!tmpc       3 iterations newton                                         
!tmp        do n=1,niter                                                
!tmp          do j=1,nindx                                              
!tmp            i = index(j)                                            
!tmp            dpla_i(i) = dpla_j(i)                                   
!tmp            yld_i = yld(i) + h(i)*dpla_i(i)                         
!tmp            dr(i) = e(i)*dpla_i(i)/yld_i                            
!tmp            pp(i) = one/(one+dr(i))                                   
!tmp            qq(i) = one/(one+nu1(i)*dr(i))                            
!tmp            p2    = pp(i)*pp(i)                                     
!tmp            q2    = qq(i)*qq(i)                                     
!tmp            f     = aa(i)*p2 + (bb(i)+cc(i))*q2 - yld_i*yld_i       
!tmp            df    = aa(i)*p2*pp(i) + nu*(bb(i)+cc(i))*q2*qq(i)      
!tmp            df    = df *(h(i)*dr(i) - e(i)) / yld_i - h(i)*yld_i    
!tmp            df    = df * two                                       
!tmp            df    = sign(max(abs(df),em20),df)                      
!tmp            if(dpla_i(i) > zero) then                               
!tmp              dpla_j(i) = max(zero,dpla_i(i)-f/df)                  
!tmp            else                                                    
!tmp              dpla_j(i) = zero                                      
!tmp            endif                                                   
!tmp          enddo                                                     
!tmp        enddo                                                       
!tmpc plastically eligible stresses                       
!tmp        do j=1,nindx                                                
!tmp          i=index(j)                                                
!tmp          pla(i) = pla(i) + dpla_i(i)                               
!tmp          signxx(i) = signxx(i)*pp(i)                               
!tmp          signxy(i) = signxy(i)*qq(i)                               
!tmp          signxz(i) = signxz(i)*qq(i)                               
!tmp        enddo                                                       
!--------------------------------------------------------------------------
        !  element failure output
!--------------------------------------------------------------------------
        if (ifail > 0) then
          do i=1,nel                                   
            if (pla(i) > epsmax(i) .and. off(i)==one) then
              off(i) = four_over_5
!$omp critical
              write(iout, 1000) ngl(i),ipt,pla(i),epsd(i)
              write(istdo,1000) ngl(i),ipt,pla(i),epsd(i)
!$omp end critical
            end if
          enddo
        end if                                       
!---------------------------------------------------------
 1000 format(5x,'failure beam element number',i3,',  integration point number',i3,  &
                ',  plastic strain = ',1pe16.9,',  strain rate = ',1pe16.9)
!--------------------------------------------------------------------------
        return
        end subroutine sigeps36pi
!--------------------------------------------------------------------------
      end module sigeps36pi_mod
