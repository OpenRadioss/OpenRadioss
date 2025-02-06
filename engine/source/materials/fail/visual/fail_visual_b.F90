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
      !||    fail_visual_b_mod   ../engine/source/materials/fail/visual/fail_visual_b.F90
      !||--- called by ------------------------------------------------------
      !||    fail_beam3          ../engine/source/elements/beam/fail_beam3.F
      !||====================================================================
    module fail_visual_b_mod
    contains
! ======================================================================================================================
! \brief   Visual failure criteria for type3 beam elements
! \details The purpose of this failure criteria is to record the maximum tensile 1st principal stress or maximum tensile
! \        1st principal strain in a simulation. The maximum value of all the cycles in a simulation is used to compute 
! \        the damage output.
! ======================================================================================================================
      !||====================================================================
      !||    fail_visual_b   ../engine/source/materials/fail/visual/fail_visual_b.F90
      !||--- called by ------------------------------------------------------
      !||    fail_beam3      ../engine/source/elements/beam/fail_beam3.F
      !||--- calls      -----------------------------------------------------
      !||    finter          ../engine/source/tools/curve/finter.F
      !||    nvar            ../engine/source/input/nvar.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod    ../common_source/modules/constant_mod.F
      !||====================================================================
      subroutine fail_visual_b(                               &
                 nel      ,ngl      ,nuparam  ,uparam   ,     &
                 time     ,dfmax    ,                         &
                 iout     ,istdo    ,                         &
                 area     ,f1       ,                         &
                 epsxx    ,epsxy    ,epsxz    ,               &
                 nvar     ,                                   &
                 uvar     ,ismstr   ,timestep                 )
!c-----------------------------------------------
!c   i m p l i c i t   t y p e s
!c-----------------------------------------------
          use constant_mod      
          implicit none
#include  "my_real.inc"
#include  "comlock.inc"
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
      integer                     ,intent(in)    :: nel     !< size of element group
      integer                     ,intent(in)    :: nuparam !< size of parameter array
      integer                     ,intent(in)    :: iout    !< output file unit
      integer                     ,intent(in)    :: istdo   !< output file unit   
      integer ,dimension(nel)     ,intent(in)    :: ngl     !< table of element identifiers
      integer                     ,intent(in)    :: nvar    !< number of failure element variables 
      my_real                     ,intent(in)    :: time    !< current time  
      my_real                     ,intent(in)    :: area
      my_real ,dimension(nuparam) ,intent(in)    :: uparam  !< failure model parameter array
      my_real ,dimension(nel)     ,intent(inout) :: dfmax   !< maximum damage
      my_real ,dimension(nel)     ,intent(in)    :: f1      !< cross section area
      my_real ,dimension(nel)     ,intent(in)    :: epsxx   !< strain increment component xx
      my_real ,dimension(nel)     ,intent(in)    :: epsxy   !< strain increment component xy
      my_real ,dimension(nel)     ,intent(in)    :: epsxz   !< strain increment component xz
      my_real ,dimension(nel,nvar),intent(inout) :: uvar    !< user variable / user element variable array
      integer                     ,intent(in)    :: ismstr  !< 
      my_real                     ,intent(in)    :: timestep!< time increment!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      integer :: i,j,nindx,type_max,f_flag,strdef,strflag
      integer ,dimension(nel) :: indx
      my_real :: rfac,r1,r2,ie_sp,dydx,rief1,rief2,xfac,finter,fact, &
                 eps11,eps22,eps33,eps12,eps13,eps23,i1,i2,i3,       &
                 e_eq,e_eq1,e11,e22,e33,e_eq2,q,r,r_inter,phi,       &
                 c_min,c_max,ema,f,ff,                               & 
                 sig11,sig22,sig33,sig12,sig13,sig23 
      external finter
      my_real ,dimension(nel) :: damage
      double precision :: a0(2),a1(2),a2(2),c0,c1,c2,c3,c4,c5,c6,c7, &
                          c8,c9, x1,x2,x3,y1,y2,y3,z1,z2,z3,d,dd,d2, &
                          dp,e,g
      double precision, parameter :: pi8   = 0.3926990817d0 
      double precision, parameter :: pi38  = 1.178097245d0
      double precision, parameter :: spi8  = 0.3826834324d0
      double precision, parameter :: spi38 = 0.9238795325d0
!c=======================================================================
!c=======================================================================
     ! C USER VARIABLES 
     !c! User variable # 1, to store the previous damage value
     !c! User variable # 2, to store the previous stress or strain value (for EMA filtering)
     !c! In the case of filter:
     !c! User variable # 3-11, Storage values for the Butterworth filter
     !c! User variable # 12, to store the previous total value of epsxx      
     !c! User variable # 13, to store the previous total value of epsxy       
     !c! User variable # 14, to store the previous total value of epsxz       
     !c! In the case of no filter:
     !c! User variable # 3, to store the previous total value of epsxx      
     !c! User variable # 4, to store the previous total value of epsxy       
     !c! User variable # 5, to store the previous total value of epsxz     
!c===============================================================================================

      type_max  = int(uparam(1))  !< Strain or stress for the damage calculation
      c_min     = uparam(2)       !< Lower limit for stress or strain
      c_max     = uparam(3)       !< Upper limit for stress or strain
      ema       = uparam(4)       !< Exponential Moving Average : filter coefficient value
      ff        = uparam(5)       !< Filter coefficient value
      f_flag    = int(uparam(6))  !< Filter flag
      strdef    = nint(uparam(7)) !< Strain measure definition used in failure criterion.
      nindx     = 0
      f         = min(ff,zep4/max(em20,timestep))
!c----------------------------------------------
!c     strain transformation flag following input definition
!c-------------------

      do i=1,nel
        !< uvar(i,1) the previous damage value
        if (uvar(i,1) < one) then     
            if (type_max == 1) then
                sig11 = f1(i)/area         
                e11 = abs(sig11)


            else if (type_max == 2) then
                !< calculation of the strain tensor
                if (ema == one .and. ff /= zero .and. f_flag > 1) then            
                    uvar(i,12) = uvar(i,12) + epsxx(i)
                    uvar(i,13) = uvar(i,13) + epsxy(i)
                    uvar(i,14) = uvar(i,14) + epsxz(i)      
                    !< For beam Type 3 : epsyy = 0, epszz = 0, epsyz = 0  
                    eps11 = uvar(i,12) !eps22 = 0. eps33 = 0.
                    eps12 = uvar(i,13)
                    eps13 = uvar(i,14) !eps23 = 0                       
                else               
                    uvar(i,3) = uvar(i,3) + epsxx(i)
                    uvar(i,4) = uvar(i,4) + epsxy(i)
                    uvar(i,5) = uvar(i,5) + epsxz(i)  
                    !< For beam Type 3 : epsyy = 0, epszz = 0, epsyz = 0    
                    eps11 = uvar(i,3) !eps22 = 0. eps33 = 0.
                    eps12 = uvar(i,4)
                    eps13 = uvar(i,5) !eps23 = 0 
                endif 
   

                i1 = eps11         !i1 = eps11+eps22+eps33
                i2 = eps12*eps12-eps13*eps13 !i2 = eps11*eps22+eps22*eps33+eps33*eps11-eps12*eps12-eps13*eps13-eps23*eps23

                !i3 = eps11*eps22*eps33-eps11*eps23*eps23-eps22*eps13*eps13-eps33*eps12*eps12+two*eps12*eps13*eps23 = 0

                q  = (three*i2 - i1*i1)/9.0 
                r  = (two*i1*i1*i1-9.0*i1*i2)/54.0        ! (2*i3^3-9*i1*i2+27*i3)/54

                r_inter = min(r/sqrt(max(em20,(-q**3))),one)
                phi = acos(max(r_inter,-one))
                
                e11 = two*sqrt(-q)*cos(phi/three)+third*i1
                e22 = two*sqrt(-q)*cos((phi+two*pi)/three)+third*i1
                e33 = two*sqrt(-q)*cos((phi+4.0*pi)/three) +third*i1      
                
              if (strdef == 2 .and. (ismstr == 0 .or. ismstr == 4)) then
                ! failure defined as engineering strain
                ! transform true strain to engineering         
                 e11 = exp(e11) - one
                 e22 = exp(e22) - one
                 e33 = exp(e33) - one
               else if (strdef == 3 .and. ismstr == 1) then
                ! failure defined as true strain
                ! transform engineering to true strain 
                 e11 = log(e11 + one)
                 e22 = log(e22 + one)
                 e33 = log(e33 + one)
               else 
                 e11 = e11
                 e22 = e22
                 e33 = e33  
               end if
!c 
               if (e11 < e22) then 
                  r_inter = e11
                  e11     = e22
                  e22     = r_inter
               endif 
               if (e22 < e33)then
                  r_inter = e22
                  e22     = e33
                  e33     = r_inter
               endif
               if (e11 < e22)then
                  r_inter = e11
                  e11     = e22
                  e22     = r_inter
               endif
!c          
           endif !type_max 

          if (ema == one .and. ff /= zero .and. f_flag > 1) then
!C-----------------------------------------------
!C           INITIALIALISATION OF THE FILTER-COEFFICIENTS 
!C-----------------------------------------------
            d  = tan(pi*f*timestep)  
            dd = d*d
            d2 = two*d
            dp = one + dd
            e  = d2*spi8
            g  = e + dp
            g  = one/g
!c         
            c0 = dd * g
            c1 = two* c0
            c2 = c0
            c3 = two * g - c1
            c4 = (e - dp) * g
!c         
            e  = d2*spi38
            g  = e + dp
            g  = one/g
!c         
            c5 = dd * g
            c6 = two * c5
            c7 = c5
            c8 = two * g - c6
            c9 = (e - dp) * g

        
!c-----------------------------------------------
!c butterworth filtering
!c-----------------------------------------------

            a0(1) = uvar(i,3)*uvar(i,9) 
            a0(2) = uvar(i,4)*uvar(i,9) 
            a1(1) = uvar(i,5)*uvar(i,10) 
            a1(2) = uvar(i,6)*uvar(i,10)  
            a2(1) = uvar(i,7)*uvar(i,11)  
            a2(2) = uvar(i,8)*uvar(i,11)  

            x1 = a0(2)
            x2 = a0(1)
          
            x3 = e11
            y1 = a1(2)
            y2 = a1(1)
            y3 = c0 * x3
            y3 = y3 + c1 * x2 
            y3 = y3 + c2 * x1
            y3 = y3 + c3 * y2
            y3 = y3 + c4 * y1
            z1 = a2(2)
            z2 = a2(1)
            z3 = c5 * y3 
            z3 = z3 + c6 * y2 
            z3 = z3 + c7 * y1
            z3 = z3 + c8 * z2 
            z3 = z3 + c9 * z1
!c         
            a0(2) = x2
            a0(1) = x3
            a1(2) = y2
            a1(1) = y3
            a2(2) = z2
            a2(1) = z3
          
            if ((x3 /= zero).and.(x2 /= zero)) then 

              uvar(i,3)  = a0(1)/x2
              uvar(i,4)  = a0(2)/x2
              uvar(i,5)  = a1(1)/(c0*x3)
              uvar(i,6)  = a1(2)/(c0*x3)
              uvar(i,7)  = a2(1)/(c0*y3)
              uvar(i,8)  = a2(2)/(c0*y3)
              uvar(i,9)  = x2
              uvar(i,10) = c0*x3
              uvar(i,11) = c0*y3

            else

              uvar(i,3)  = a0(1)
              uvar(i,4)  = a0(2)
              uvar(i,5)  = a1(1)
              uvar(i,6)  = a1(2)
              uvar(i,7)  = a2(1)
              uvar(i,8)  = a2(2)
              uvar(i,9)  = one
              uvar(i,10) = one
              uvar(i,11) = one

            endif
          
            e11 = a2(1)     

          else
            
            e11        = ema * e11 + ( one - ema ) * uvar(i,2)
            uvar(i,2)  = e11      
    
          endif 
!c!      
          damage(i)    =  max(zero , min(one ,(e11-c_min)/max(em6,(c_max-c_min)) ))
          uvar(i,1)    =  max(uvar(i,1),damage(i))
          dfmax(i)     =  uvar(i,1)
        
          if (uvar(i,1) >= one) then
            nindx       = nindx+1
            indx(nindx) = i              
          endif
        endif  ! uvar(i,1) < one
      end do

!c------------------------
      if (nindx > 0) then        
        do j=1,nindx             
          i = indx(j)            
#include "lockon.inc"
          write(iout, 1000) ngl(i),time
          write(istdo,1000) ngl(i),time
#include "lockoff.inc" 
        end do                   
      end if   ! nindx             
!c------------------
 1000 format(5x,'failure (visual) of beam element ',i10,1x, &
        'at time :', 1pe12.4)
!c------------------
      return

      end subroutine fail_visual_b 
    end module fail_visual_b_mod