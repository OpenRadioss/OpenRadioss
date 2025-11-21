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
!||    hm_read_mat36_mod   ../starter/source/materials/mat/mat036/hm_read_mat36.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat         ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat36_mod
      contains

!! \brief  Read material input parameters of law36 


!||====================================================================
!||    hm_read_mat36                  ../starter/source/materials/mat/mat036/hm_read_mat36.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat                    ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                         ../starter/source/output/message/message.F
!||    func_table_copy                ../starter/source/materials/tools/func_table_copy.F90
!||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
!||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim              ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword               ../starter/source/materials/mat/init_mat_keyword.F
!||    table_mat2d_deintersect        ../starter/source/materials/tools/table_mat2d_deintersect.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                   ../starter/share/modules1/elbuftag_mod.F
!||    func_table_copy_mod            ../starter/source/materials/tools/func_table_copy.F90
!||    hm_option_read_mod             ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                    ../starter/share/message_module/message_mod.F
!||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
!||    table_mat2d_deintersect_mod    ../starter/source/materials/tools/table_mat2d_deintersect.F90
!||    table_mod                      ../starter/share/modules1/table_mod.F
!||====================================================================
      subroutine hm_read_mat36(mat_param,                                    &
                 mtag     ,parmat   ,nuvar    ,nvartmp  ,israte   ,          &
                 ntable   ,table    ,unitab   ,lsubmodel,iout     ,          &
                 soundspeed)                     
! -----------------------------------------------------------------------------------------------
!   modules
! -----------------------------------------------------------------------------------------------
          use matparam_def_mod
          use elbuftag_mod
          use table_mod
          use unitab_mod
          use message_mod
          use submodel_mod
          use constant_mod ! ,only : pi,one,third,two,zero,em20,ep10,ep20
          use func_table_copy_mod
          use precision_mod, only : wp
          use hm_option_read_mod
          use names_and_titles_mod
          use table_mat2d_deintersect_mod
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in)    :: ntable
          integer, intent(in)    :: iout
          integer, intent(inout) :: nuvar,nvartmp,israte
          real(kind=wp) ,intent(inout) :: soundspeed
          real(kind=wp) ,dimension(128) ,intent(inout) :: parmat
          type (mlaw_tag_) ,intent(inout) :: mtag
          type (unit_type_),intent(in)    :: unitab 
          type (submodel_data),intent(in) :: lsubmodel(*)
          type (matparam_struct_) ,intent(inout) :: mat_param
          type (ttable) ,dimension(ntable) ,intent(in) :: table
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: nbmat, mat_id  ! number of declared materials
          integer :: i,j,vp,yldcheck
          integer :: rhoflag,icomp,nrate1,nrate,israt,ismooth
          integer :: nbline,nbread,ifail,ilaw,nfunc
          integer :: ipt,npt
          integer :: id_funcp,id_funce
          real(kind=wp) :: x0,x1,x2,y1,y2
          real(kind=wp) :: rho0,rhor,young,nu,shear,bulk,a11,a12
          real(kind=wp) :: epsmax,epsr1,epsr2,epsf,fisokin,fcut,psCAL_UNIT,EINF,CE
          real(kind=wp) :: strainrate_unit,yfac_unit
          logical :: is_available,is_encrypted
          integer :: func_p(1),func_e(1)
          real(kind=wp) :: x1scale,x2scale
          real(kind=wp) ,dimension(:)   ,allocatable :: x_raw
          real(kind=wp) ,dimension(:,:) ,allocatable :: y_raw
          real(kind=wp) :: pscale(1), escale(1)
          real(kind=wp) ,dimension(:) ,allocatable :: yfac, rate
          integer       ,dimension(:) ,allocatable :: yldfunc,id_funcy
          character(len=nchartitle) :: title
!-----------------------------------------------
!   s o u r c e   l i n e s
!=========================================================================================
          is_encrypted = .false.
          is_available = .false.
      
          ilaw   = 36
          mat_id = mat_param%mat_id
          title  = mat_param%title
!--------------------------------------------------
! extract data (is option crypted)
!--------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
!-----------------------------------------------
          call hm_get_floatv('MAT_RHO'      ,rho0     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('Refer_Rho'    ,rhor     ,is_available, lsubmodel, unitab)
!-----------------------------------------------
!card1
          call hm_get_floatv('MAT_E'        ,young    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_NU'       ,nu       ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPS'      ,epsmax   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPST1'    ,epsr1    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPST2'    ,epsr2    ,is_available, lsubmodel, unitab)
!-----------------------------------------------
!card2
          call hm_get_intv  ('NFUNC'        ,nrate    ,is_available,lsubmodel)
          call hm_get_intv  ('Fsmooth'      ,ismooth  ,is_available,lsubmodel)
          call hm_get_floatv('MAT_HARD'     ,fisokin  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('Fcut'         ,fcut     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_Epsilon_F',epsf     ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('Vflag'        ,vp       ,is_available,lsubmodel)
!card3       
          call hm_get_intv  ('Xr_fun'       ,id_funcp ,is_available,lsubmodel)
          call hm_get_floatv('MAT_FScale'   ,pscale(1),is_available, lsubmodel, unitab)
          call hm_get_intv  ('Yr_fun'       ,id_funce ,is_available,lsubmodel)
          call hm_get_floatv('MAT_EFIB'     ,einf     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_C'        ,ce       ,is_available, lsubmodel, unitab)
!-----------------------------------------------
          call hm_get_floatv_dim('mat_fscale',pscal_unit ,is_available,lsubmodel,unitab)
!-----------------------------------------------
          allocate (yldfunc(nrate+1))
          allocate (id_funcy(nrate+1))
          allocate (yfac(nrate+1))
          allocate (rate(nrate+1))
          rate(1:nrate) = zero
!
          if (nrate > 0) then
            do j=1,nrate
              call hm_get_int_array_index ('FUN_LOAD',yldfunc(j),j,is_available,lsubmodel)
            enddo
            call hm_get_float_array_index_dim('SCALE_LOAD',yfac_unit,1,is_available,lsubmodel,unitab) 
            do j=1,nrate
              call hm_get_float_array_index('SCALE_LOAD',yfac(j)    ,j,is_available,lsubmodel,unitab)
              if (yfac(j) == zero) then
                yfac(j) = one * yfac_unit
              endif
            enddo
            do j=1,nrate
              call hm_get_float_array_index('STRAINRATE_LOAD',rate(j),j,is_available,lsubmodel,unitab)
            enddo
          end if
!---------------------------------------------------------------------------------------------------
          ! check input values and set defaults
!---------------------------------------------------------------------------------------------------
          if (nrate == 0) then
            call ancmsg(MSGID=740, MSGTYPE=MSGERROR, ANMODE=ANINFO,              &
                        I1=MAT_ID,C1=title)
          endif
          do i=1,nrate
            if (yldfunc(i) == 0) then
              call ancmsg(MSGID=126, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  &
                          i1=MAT_ID,C1=title,i2=yldfunc(i))
            endif
          enddo
          do i=1,nrate-1                        
            if (rate(i) >= rate(i+1)) then        
              call ancmsg(msgid=478, msgtype=msgerror, anmode=aninfo_blind_1,   &
                          i1=mat_id,c1=title)
              exit                         
            endif                               
          enddo                                 
!
          if (rhor == zero) rhor = rho0
          if (nu == half)   nu = zep499
          if (nu < zero .or. nu > half) then
            call ancmsg(MSGID=49,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,          &
                        R1=NU,I1=MAT_ID,C1=title)
          endif
!
          if (id_funcp == 0) then
            pscale = zero
          elseif (pscale(1) == zero) then
            pscale(1) = one / pscal_unit
          else
            pscale(1) = one /pscale(1)
          endif
!
          if (fisokin > one .or. fisokin < zero) then
            call ancmsg(MSGID=912, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  &
                        I1=MAT_ID,C1='36',C2=title)
          end if
!
          if (epsr1 /= zero .and. epsr2 /= zero) then 
            if (epsr1 >= epsr2) then
             call ancmsg(MSGID=480, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  &
                         I1=MAT_ID,C1=title)
            endif
          endif
!
          if (young <= zero) then
            call ancmsg(MSGID=276,MSGTYPE=MSGERROR,ANMODE=ANINFO,             &
                        I1=36,I2=MAT_ID,C1=title)
            young = zero
          endif
!
          if (nrate == 1) then
            nfunc  = 1
            ismooth= 0
            fcut   = zero
            vp     = 0      ! no plastic strain rate dependency with single static curve
          else    ! nrate > 1
            israte = 1
            if (rate(1) == zero) then
              nfunc = nrate
            else 
              nfunc = nrate+1
              if (ismooth == 2) then
                do j=nrate,1,-1
                  yldfunc(j+1) = yldfunc(j)
                  rate(j+1)    = log(rate(j))
                  yfac(j+1)    = yfac(j)
                enddo
              else
                do j=nrate,1,-1
                  yldfunc(j+1) = yldfunc(j)
                  rate(j+1)    = rate(j)
                  yfac(j+1)    = yfac(j)
                enddo
              endif
              rate(1) = zero
            endif
!
            if (fcut == zero .or. vp == 1) then
              fcut = 10000.0d0*unitab%fac_t_work
            end if 
          endif
          id_funcy(:) = yldfunc(:)
!-----------------------------------------------
          ! check failure option 
!-----------------------------------------------
          ifail = 0
          if (epsmax== zero) epsmax= infinity
          if (epsr1 == zero) epsr1 = infinity
          if (epsr2 == zero) epsr2 = two*infinity
          if (epsf  == zero) epsf  = three*infinity       
!         limit max failure values
          epsmax = min(epsmax  ,infinity)
          epsr1  = min(epsr1   ,infinity)
          epsr2  = min(epsr2   ,two*infinity)
          epsf   = min(epsf    ,three*infinity)
          epsr2  = min(epsr2   ,epsf)
          epsr1  = min(epsr1   ,epsr2)
          ! ifail = 0 => no failure at all inside material
          ! ifail = 1 => only failure vs max plastic strain
          ! ifail = 2 => failure + damage vs principal tensile strain
          if (epsmax < infinity) then
            ifail = 1
          end if          
          if (epsr1<infinity .and. epsr1>zero .and. epsr2>zero .and. epsf>zero) then
            ifail = 2
          endif
!-----------------------------------------------
          ! transform input functions to tables in mat_param
!-----------------------------------------------
          mat_param%ntable = 3
          allocate (mat_param%table(mat_param%ntable))
          mat_param%table(1)%notable = id_funcy(1)
          mat_param%table(2)%notable = id_funcp
          mat_param%table(3)%notable = id_funce
!---------------------------------------------------
          !  copy yield functions to material tables
!---------------------------------------------------
          x1scale = one
          x2scale = one
          call func_table_copy(mat_param%table(1),title,mat_id,             &
               nfunc   ,yldfunc   ,rate    ,x1scale ,x2scale  ,yfac    ,    &
               ntable  ,table     ,ierr    )    
!---------------------------------------------------
          ! check intersections between strain rate curves if ndim == 2
!---------------------------------------------------
          if (mat_param%table(1)%ndim == 2) then
            call table_mat2d_deintersect(mat_param%table(1),title,mat_id)
          end if
!---------------------------------------------------
          ! check if static yield curve decreases to zero
!---------------------------------------------------
          npt = size(mat_param%table(1)%x(1)%values)
          x1 = mat_param%table(1)%x(1)%values(npt-1)
          x2 = mat_param%table(1)%x(1)%values(npt)
          if (mat_param%table(1)%ndim == 1) then
            y1 = mat_param%table(1)%y1d(npt-1)
            y2 = mat_param%table(1)%y1d(npt)
          else
            y1 = mat_param%table(1)%y2d(npt-1,1)
            y2 = mat_param%table(1)%y2d(npt,1)
          end if
          yldcheck = 0
          if (x2 > zero .and. y2 == zero) then  ! last yield value is zero
            epsmax   = min(x2, epsmax)
            ifail    = 1
            yldcheck = 1
          else if (y1 > y2) then                ! yield function slope is negative 
            x0 = (x2*y1 - x1*y2) / (y1 - y2)    ! x value at y=0
            epsmax = min(x0, epsmax) 
            ifail    = 1
            yldcheck = 1
          end if
!-----------------------------------------
          ! triaxiality scale factor on yield value
!-----------------------------------------
          if (id_funcp > 0) then
            nfunc = 1
            func_p(1) = id_funcp
            call func_table_copy(mat_param%table(2),title,mat_id,        &
                 nfunc   ,func_p   ,rate  ,x1scale ,x2scale  ,pscale  ,  &
                 ntable  ,table   ,ierr  )
          end if
!-----------------------------------------
          ! young modulus scale function   
          if (id_funce > 0) then
            nfunc = 1
            func_e(1) = id_funce
            escale(1) = one
            call func_table_copy(mat_param%table(3),title,mat_id,         &       
                 nfunc   ,func_e  ,rate  ,x1scale ,x2scale  ,escale    ,  &               
                 ntable  ,table   ,ierr  )
            ! check if Young modulus scale factor decreases with plastic strain
            if (func_e(1) > 0) then
              npt = size(mat_param%table(3)%x(1)%values)
              do i = 2,npt
                if (mat_param%table(3)%y1d(i-1) < mat_param%table(3)%y1d(i)) then   
                  call ancmsg(MSGID=975, MSGTYPE=MSGERROR, ANMODE=ANINFO,    &
                              i1 = mat_id, c1 =title )                                             
                  exit
                end if
              end do
            end if
          end if
!------------------------------
          shear = half*young/(one+nu)
          bulk  = young/three/(one - two*nu)
          a11   = young / (one - nu**2)  
          a12   = nu * a11
          soundspeed = sqrt(young/max(rho0,em20))
!------------------------------
          ! storage of material parameters
!------------------------------
          if (vp == 1) then
            nuvar = 3
          else
            nuvar = 0
          endif
          nvartmp = 2 + nfunc
          mat_param%niparam = 4
          mat_param%nuparam = 10
!          
          allocate(mat_param%iparam(mat_param%niparam))
          allocate(mat_param%uparam(mat_param%nuparam))
!
          mat_param%iparam(1) = ismooth
          mat_param%iparam(2) = ifail
          mat_param%iparam(3) = vp
          mat_param%iparam(4) = yldcheck       ! variable young modulus option
!
          mat_param%uparam(1) = a11
          mat_param%uparam(2) = a12
          mat_param%uparam(3) = epsmax
          mat_param%uparam(4) = epsr1
          mat_param%uparam(5) = epsr2
          mat_param%uparam(6) = epsf
          mat_param%uparam(7) = fisokin
          mat_param%uparam(8) = ce
          mat_param%uparam(9) = einf
          mat_param%uparam(10)= fcut*two*pi   ! asrate
!
          mat_param%rho   = rhor
          mat_param%rho0  = rho0
          mat_param%young = young
          mat_param%nu    = nu
          mat_param%shear = shear
          mat_param%bulk  = bulk
!-----------------------------------------------------------------------------      
          parmat(1) = bulk
          parmat(2) = young
          parmat(3) = nu
          parmat(4) = israte
          parmat(5) = fcut
          ! formulation for solid elements time step computation.
          parmat(16) = 2
          parmat(17) = two*shear/(bulk+four_over_3*shear) ! == (1-2*nu)/(1-nu)
!--------------------     
          mtag%g_epsd = 1
          mtag%l_epsd = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          if (fisokin /= zero) then
            mtag%l_sigb = 6
          endif
          if (ifail > 0) then 
            mtag%g_dmg = 1
            mtag%l_dmg = 1     
          endif
!-----------------------------------------------------------------------------      
          call init_mat_keyword(mat_param,"ELASTO_PLASTIC")
          call init_mat_keyword(mat_param,"INCREMENTAL")
          call init_mat_keyword(mat_param,"LARGE_STRAIN") 
          ! properties compatibility
          call init_mat_keyword(mat_param,"SHELL_ISOTROPIC")
          call init_mat_keyword(mat_param,"SOLID_ISOTROPIC")
          call init_mat_keyword(mat_param,"SPH")   
          call init_mat_keyword(mat_param,"BEAM_INTEGRATED")    
!
          ! Material compatibility with /EOS option
          call init_mat_keyword(mat_param,"EOS")
!-----------------------      
          write(iout,1001) trim(title),mat_id,36  
          write(iout,1000)   
          if (is_encrypted)then                                
            write(iout,'(5x,a,//)')'confidential data'
          else     
            write(iout,1002) rho0
            write(iout,1100) young,nu,epsmax,epsr1,epsr2,epsf,fisokin,ismooth,fcut,vp
            write(iout,1200)(id_funcy(j),yfac(j),rate(j),j=1,nfunc)
            write(iout,1300) id_funcp,pscale(1), id_funce,einf,ce
            write(iout,*)' '
            ! output yield table after modifications
            npt   = size(mat_param%table(1)%x(1)%values)
            if (mat_param%table(1)%ndim == 1) then
              write(iout,2000) id_funcy(1)
              do i=1,npt
                write(iout,3000) mat_param%table(1)%x(1)%values(i),mat_param%table(1)%y1d(i)
              end do
            else
              nrate = size(mat_param%table(1)%x(2)%values)
              do j=1,nrate
                write(iout,2500) id_funcy(j), mat_param%table(1)%x(2)%values(j)
                do i=1,npt
                  write(iout,3000) mat_param%table(1)%x(1)%values(i),mat_param%table(1)%y2d(i,j)
                end do
              end do
            end if
            write(iout,*)' '
          endif
!
          deallocate (rate)
          deallocate (yfac)
          deallocate (yldfunc)
!-----------        
      return
!--------------------------------------------------------------------------
 1000 format(                                                             &
      5x,'    TABULATED ELASTIC PLASTIC LAW 36    ',/,                    &
      5x,'    --------------------------------    ' ,//)                   
 1001 format(/                                                            &
      5x,A,/,                                                             &
      5x,'MATERIAL NUMBER . . . . . . . . . . . . . .=',i10/,             &
      5x,'MATERIAL LAW. . . . . . . . . . . . . . . .=',i10/)              
 1002 format(                                                             &
      5x,'INITIAL DENSITY . . . . . . . . . . . . . .=',1pg20.13/)         
 1100 format(                                                             &
      5x,'YOUNG MODULUS . . . . . . . . . . . . . . .=',1pg20.13/         &
      5x,'POISSON RATIO . . . . . . . . . . . . . . .=',1pg20.13/         &
      5x,'MAXIMUM PLASTIC STRAIN  . . . . . . . . . .=',1pg20.13/         &
      5x,'TENSION FAILURE STRAIN 1  . . . . . . . . .=',1pg20.13/         &
      5x,'TENSION FAILURE STRAIN 2  . . . . . . . . .=',1pg20.13/         &
      5x,'MAXIMUM TENSION FAILURE STRAIN  . . . . . .=',1pg20.13/         &
      5x,'ISO-KINEMATIC HARDENING FACTOR. . . . . . .=',1pg20.13/         &
      5x,'SMOOTH STRAIN RATE OPTION . . . . . . . . .=',i10/              &
      5x,'     0 -> NO SMOOTHING                      ',/,                &
      5x,'     1 -> SMOOTH + LINEAR INTERPOLATION     ',/,                &
      5x,'     2 -> SMOOTH + LOG_N  INTERPOLATION     ',/                 &
      5x,'STRAIN RATE CUTTING FREQUENCY . . . . . . .=',1pg20.13/         &
      5x,'PLASTIC STRAIN RATE DEPENDENCY FLAG . . . .=',i10/              &
      5x,'   FLAG_PL = 0 -> TOTAL SR DEPENDENCY       ',/,                &
      5x,'   FLAG_PL = 1 -> PLASTIC SR DEPENDENCY     ',/,                &
      5x,'STRAIN RATE INTERPOLATION FLAG. . . . . . .=',i10/)              
 1200 format(                                                             &
      5x,'YIELD STRESS FUNCTION NUMBER. . . . . . . .=',i10/              &
      5x,'YIELD SCALE FACTOR. . . . . . . . . . . . .=',1pg20.13/         &
      5x,'STRAIN RATE . . . . . . . . . . . . . . . .=',1pg20.13)          
 1300 format(                                                             &
      5x,'PRESSURE DEPENDENT YIELD FUNCTION . . . . .=',i10/              &
      5x,'PRESSURE SCALE FACTOR . . . . . . . . . . .=',1pg20.13/         &
      5x,'YOUNG MODULUS SCALE FACTOR FUNCTION . . . .=',i10/              &
      5x,'YOUNG MODULUS EINF. . . . . . . . . . . . .=',1pg20.13/         &
      5x,'PARAMETER CE. . . . . . . . . . . . . . . .=',1pg20.13)
 2000 format(5x,/,'YIELD STRESS FUNCTION=',i10,                           &
             5x,/'      X,              Y')
 2500 format(5x,/,'YIELD STRESS FUNCTION=',i10,                           &
             5x,'STRAIN RATE = ',1pg20.13,/,                              &
             5x,'              X                   Y')
 3000 format(2g20.13)
!--------------------------------------------------------------------------
        end subroutine hm_read_mat36
      end module hm_read_mat36_mod
