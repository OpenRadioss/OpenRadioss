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
!||    hm_read_mat88_mod   ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat         ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat88_mod
      contains
!||====================================================================
!||    hm_read_mat88                     ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat                       ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                            ../starter/source/output/message/message.F
!||    func_table_copy                   ../starter/source/materials/tools/func_table_copy.F90
!||    hm_get_float_array_index          ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_float_array_index_dim      ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
!||    hm_get_floatv                     ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim                 ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index            ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                       ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted            ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword                  ../starter/source/materials/mat/init_mat_keyword.F
!||    monotone_in_rate_signed_highfix   ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||    table_mat2d_intersect             ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                      ../starter/share/modules1/elbuftag_mod.F
!||    func_table_copy_mod               ../starter/source/materials/tools/func_table_copy.F90
!||    hm_option_read_mod                ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                       ../starter/share/message_module/message_mod.F
!||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
!||    table_mat_spline_fit_mod          ../starter/source/materials/mat/mat088/table_mat_spline_fit_mod.F90
!||    table_mod                         ../starter/share/modules1/table_mod.F
!||====================================================================
      subroutine hm_read_mat88(                                                &
                   matparam ,nvartmp  ,parmat   ,unitab   ,mat_id   ,titr     ,&
                   mtag     ,lsubmodel,iout     ,nuvar    ,ilaw     ,ntable   ,&
                   table    ,imatvis  ,israte   ,maxfunc  ,iunit    )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use unitab_mod
        use submodel_mod
        use matparam_def_mod    
        use elbuftag_mod      
        use constant_mod    
        use hm_option_read_mod 
        use table_mod
        use table4d_mod
        use message_mod
        use func_table_copy_mod
        use table_mat_spline_fit_mod
        use precision_mod, only: WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        type(matparam_struct_),intent(inout), target :: matparam !< Material parameters data structure
        integer, intent(inout) :: nvartmp                !< Number of temporary variables
        real(kind=WP), intent(inout) :: parmat(100)      !< Material parameters local table
        type(unit_type_),intent(in) :: unitab            !< Unit conversion table
        integer, intent(in) :: mat_id                    !< Material ID
        character(len=nchartitle), intent(in) :: titr    !< Material title
        type(mlaw_tag_),intent(inout) :: mtag            !< Material tag data structure
        type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< Submodel data structure
        integer, intent(in) :: iout                      !< Output unit
        integer, intent(inout) :: nuvar                  !< Number of user variables
        integer, intent(in) :: ilaw                      !< Material law number
        integer, intent(in) :: ntable                    !< Number of tables
        type(ttable),dimension(ntable),intent(in) :: table !< Table data structure
        integer, intent(inout) :: imatvis                !< Material viscosity flag
        integer, intent(inout) :: israte                 !< Strain rate filtering flag
        integer, intent(in) :: maxfunc                   !< Maximum number of functions
        integer, intent(in) :: iunit                     !< Input unit
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        real(kind=WP) ::                                                       &
          k,nu,g,rate(maxfunc+1),hys,rho0,rhor,bulk,fcut,yfac(maxfunc+1),      &
          yfac_unl,shape,gs,e,x1scale,x2scale,dx,dy,dydx,sgl,sw,st,gdamp,      &
          sigf,kfail,gam1,gam2,eh,areafac,lengthfac,beta,xscale,yscale,        &
          lam,rv,lambda(maxfunc+1),sigpeak,ssp     
        integer ::                                                             &
          i,ii,nl,ifunc(maxfunc+1),ifunc_unload,itens,iunl_for,                &
          ifunc_out(maxfunc+1),npt,npt2,ndim,rtype,j,failip,nv_base
        logical is_available,is_encrypted,found
        type(table_4d_), dimension(:), pointer :: table_mat   
        real(kind=8) :: info,xint,yint
        real(kind=8), dimension(:)  , allocatable :: x_raw
        real(kind=8), dimension(:,:), allocatable :: y_raw
        real(kind=8), dimension(:,:), allocatable :: x_out,y_out
        real(kind=8), dimension(:)  , allocatable :: x_out2,y_out2
        integer, parameter :: nout = 300
        real(kind=8), parameter :: tol_deriv = 1.0d-8
!-----------------------------------------------
!     S o u r c e 
!-----------------------------------------------
        is_encrypted = .false.
        is_available = .false.
        imatvis = 1 
!-------------------------------------------------------------------------------
        call hm_option_is_encrypted(is_encrypted)
!-------------------------------------------------------------------------------
        !< Density
        call hm_get_floatv('MAT_RHO'      ,rho0  ,is_available, lsubmodel, unitab)
        call hm_get_floatv('Refer_Rho'    ,rhor  ,is_available, lsubmodel, unitab)
!-------------------------------------------------------------------------------
        !< 1st line of material card
        call hm_get_floatv('LAW88_Nu'     ,nu    ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_K'      ,bulk  ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_Fcut'   ,fcut  ,is_available, lsubmodel, unitab)
        call hm_get_intv  ('LAW88_NL'     ,nl    ,is_available, lsubmodel)
        ! -> Check if no loading curve is defined
        if (nl == 0) then
          call ancmsg(msgid = 866,                                             &                                                                                                                                     
                      msgtype = msgerror,                                      &
                      anmode = aninfo_blind,                                   &
                      i1 = mat_id,                                             &
                      c1 = titr)
        endif 
!-------------------------------------------------------------------------------
        !< 2nd line of material card
        call hm_get_intv  ('LAW88_fct_IDunL',ifunc_unload,is_available, lsubmodel)
        call hm_get_floatv('LAW88_FscaleunL',yfac_unl    ,is_available, lsubmodel, unitab)
        if (yfac_unl == zero) then
          call hm_get_floatv_dim('LAW88_FscaleunL',yfac_unl,is_available,lsubmodel,unitab)
        endif
        call hm_get_floatv('LAW88_Hys'      ,hys         ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_Shape'    ,shape       ,is_available, lsubmodel, unitab)
        call hm_get_intv  ('LAW88_Tension'  ,itens       ,is_available, lsubmodel)
        call hm_get_intv  ('LAW88_RTYPE'    ,rtype       ,is_available, lsubmodel)
!-------------------------------------------------------------------------------
        !< 3rd line of material card
        ! -> Read the loading curves 
        do i = 1,nl
          call hm_get_int_array_index  ('LAW88_arr1',ifunc(i),i,is_available, lsubmodel)
          call hm_get_float_array_index('LAW88_arr2',yfac(i) ,i,is_available, lsubmodel, unitab)
          if (yfac(i) == zero) then
            call hm_get_float_array_index_dim('LAW88_arr2',yfac(i),i,is_available,lsubmodel, unitab)
          endif
          call hm_get_float_array_index('LAW88_arr3',rate(i) ,i,is_available, lsubmodel, unitab)
          if ((rate(i) == zero).and.(i > 1)) then
            call hm_get_float_array_index_dim('LAW88_arr3',rate(i),i,is_available,lsubmodel, unitab)
          endif
          call hm_get_float_array_index('LAW88_LAMFIT',lambda(i),i,is_available, lsubmodel, unitab)
          if (lambda(i) == zero) lambda(i) = em03
        enddo
!-------------------------------------------------------------------------------
        !< 4th line of material card
        call hm_get_floatv('LAW88_SGL' ,sgl    ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_SW'  ,sw     ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_ST'  ,st     ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_G'   ,gdamp  ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_SIGF',sigf   ,is_available, lsubmodel, unitab)
!----------------------------------------------------------------------------
        !< 5th line of material card
        call hm_get_floatv('LAW88_KFAIL',kfail ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_GAM1' ,gam1  ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_GAM2' ,gam2  ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LAW88_EH'   ,eh    ,is_available, lsubmodel, unitab)
        call hm_get_intv  ('LAW88_FAILIP',failip,is_available,lsubmodel)
!-------------------------------------------------------------------------------  
        !< Poisson's ratio check
        beta = zero
        if (nu <= zero) then
          beta = abs(nu)
          if (iunit > 0) beta = beta/unitab%fac_t(iunit)
          nu = 0.495d0
        endif
        !< Check values and set default values
        if (nl > 1) then 
          do i = 2,nl
            if (rate(i) < rate(i-1)) then
              call ancmsg(msgid=478,                                           &
                          msgtype=msgerror,                                    &
                          anmode=aninfo_blind_1,                               &
                          i1=mat_id,                                           &
                          c1=titr)
              exit
            endif
          enddo
        endif
        !< Compute the geometric factor
        if (sgl == zero) then
          call hm_get_floatv_dim('LAW88_SGL',sgl,is_available,lsubmodel,unitab)
        endif
        if (sw == zero) then
          call hm_get_floatv_dim('LAW88_SW',sw,is_available,lsubmodel,unitab)
        endif
        if (st == zero) then
          call hm_get_floatv_dim('LAW88_ST',st,is_available,lsubmodel,unitab)
        endif
        areafac = one/(st*sw)
        lengthfac = one/sgl
        !< Damage softening parameter
        eh = max(min(eh,one),zero)
        !< Number of failed integration points prior to element deletion
        failip = max(failip,0)
        !< Check if same id for loading and unloading curve has been defined
        if (ifunc_unload > 0) then
          if (ifunc_unload == ifunc(1)) then
            ifunc_unload = 0
          endif
        endif
!-------------------------------------------------------------------------------
        !< Filling buffer tables
!------------------------------------------------------------------------------- 
        !< Number of integer material parameters
        matparam%niparam = 6
        !< Number of real material parameters
        matparam%nuparam = 9
        !< Number of user variables 
        nuvar = 12
        nv_base = nuvar
        !< Number of tables and temporary variables
        if (ifunc_unload > 0) then
          matparam%ntable = 3
        else
          matparam%ntable = 1
        endif
        !< Number of temporary variables
        nvartmp = 6
!          
        !< Allocation of material parameters tables
        allocate(matparam%iparam(matparam%niparam))
        allocate(matparam%uparam(matparam%nuparam))
        allocate(matparam%table (matparam%ntable ))
!        
        !< Material table pointer
        table_mat => matparam%table(1:matparam%ntable)
!    
        !<======================================================================
        !< Transform series of functions into material table
        !<======================================================================
        ! -> Loading curves
        ! ----------------------------------------------------------------------
!
        ! --> Transform the loading curves into a material table
        x1scale = one*lengthfac
        x2scale = one*lengthfac
        ifunc_out(1:nl) = ifunc(1:nl)
        yfac(1:nl) = yfac(1:nl)*areafac
        call func_table_copy(table_mat(1) ,titr      ,mat_id ,nl        ,      &
                             ifunc(1:nl)  ,rate(1:nl),x1scale,x2scale   ,      &
                             yfac(1:nl)   ,ntable    ,table  ,ierr      )    
!
        ! --> Spline interpolation, smoothing, ensure monotonicity and go 
        !     through the origin (0,0)
        ndim = table_mat(1)%ndim
        npt = size(table_mat(1)%x(1)%values)
        allocate(x_out(nout+1,nl),y_out(nout+1,nl))
!
        ! ---> No strain rate dependency
        !-----------------------------------------------------------------------
        if (ndim == 1) then
!
          !< Normalize abscissa and ordinate prior to spline fitting
          xscale = max(abs(table_mat(1)%x(1)%values(1)),                       &
                           table_mat(1)%x(1)%values(npt))
          table_mat(1)%x(1)%values(1:npt) = table_mat(1)%x(1)%values(1:npt)/   &
                                                                     xscale  
          yscale = max(abs(table_mat(1)%y1d(1)),table_mat(1)%y1d(npt))
          table_mat(1)%y1d(1:npt) = table_mat(1)%y1d(1:npt)/yscale
!
          !< Allocate temporary arrays
          if (allocated(x_raw)) deallocate(x_raw)
          if (allocated(y_raw)) deallocate(y_raw)
          allocate(x_raw(npt),y_raw(npt,1))
!
          !< Copy raw data into temporary arrays
          x_raw(1:npt) = table_mat(1)%x(1)%values(1:npt)
          y_raw(1:npt,1) = table_mat(1)%y1d(1:npt)
!
          !< Smoothing spline fit
          call table_mat_spline_fit(npt     ,x_raw(1:npt),y_raw(1:npt,1),nout, &
                                      x_out(1:nout+1,1)  ,y_out(1:nout+1,1)  , &
                                      real(lambda(1),kind=8),info   )
!
          !< Check if spline fit was successful
          if (abs(info) > tol_deriv) then
            call ancmsg(msgid=3107,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_1,                                 &
                        i1=mat_id,                                             &
                        c1=titr,                                               &
                        i2=ifunc_out(1),                                       &
                        r1=rate(1),                                            &
                        r2=lambda(1))
          endif
!
          !< Rescale back to original values
          x_out(1:nout+1,1) = x_out(1:nout+1,1)*xscale
          y_out(1:nout+1,1) = y_out(1:nout+1,1)*yscale
!
          !< Copy the smoothed curve to the material table
          deallocate(table_mat(1)%x(1)%values,table_mat(1)%y1d)
          allocate(table_mat(1)%x(1)%values(nout+1),table_mat(1)%y1d(nout+1))
          table_mat(1)%x(1)%values(1:nout+1) = x_out(1:nout+1,1)
          table_mat(1)%y1d(1:nout+1) = y_out(1:nout+1,1)  
!
        ! ---> Strain rate dependency  
        !-----------------------------------------------------------------------
        else
!
          !< Allocate temporary arrays
          if (allocated(x_raw)) deallocate(x_raw)
          if (allocated(y_raw)) deallocate(y_raw)
          allocate(x_raw(npt),y_raw(npt,nl))
!
          !< Copy raw data into temporary arrays
          x_raw(1:npt) = table_mat(1)%x(1)%values(1:npt)
          do j = 1,nl
            y_raw(1:npt,j) = table_mat(1)%y2d(1:npt,j)
          enddo
!
          !< Correct curves to avoid intersection between them
          do i = 1,nl 
            do j = i+1,nl
              call table_mat2d_intersect(table_mat(1),i,j,npt,.true.,          &
                .true.  ,.true.  ,1.d-12  ,1.d-8  ,found  ,xint  ,yint   )
              !< If intersection found, correct the curves
              if (found) then 
                call monotone_in_rate_signed_highfix(                          &
                  npt      ,x_raw(1:npt),y_raw(1:npt,i),y_raw(1:npt,j),        &
                  0.02d0   ,50.0d0      ,.true.        , .true.       )                
              endif
            enddo
          enddo
!
          !< Normalize common abscissa prior to spline fitting
          xscale = max(abs(table_mat(1)%x(1)%values(1)),                       &
                           table_mat(1)%x(1)%values(npt))
          table_mat(1)%x(1)%values(1:npt) = table_mat(1)%x(1)%values(1:npt)/   &
                                                                     xscale 
!
          !< Re-allocate temporary arrays
          if (allocated(x_raw)) deallocate(x_raw)
          if (allocated(y_raw)) deallocate(y_raw)
          allocate(x_raw(npt),y_raw(npt,nl))   
!
          !< Copy raw data into temporary arrays
          x_raw(1:npt) = table_mat(1)%x(1)%values(1:npt)  
!
          do j = 1,nl
            !< Normalize current ordinate prior to spline fitting
            yscale = max(abs(table_mat(1)%y2d(1,j)),table_mat(1)%y2d(1,j))
            table_mat(1)%y2d(1:npt,j) = table_mat(1)%y2d(1:npt,j)/yscale
            y_raw(1:npt,j) = table_mat(1)%y2d(1:npt,j)
            call table_mat_spline_fit(                                         &
              npt      ,x_raw(1:npt),y_raw(1:npt,j),nout    ,x_out(1:nout+1,j),&
              y_out(1:nout+1,j),real(lambda(j),kind=8),info     ) 
! 
            !< Check if spline fit was successful
            if (abs(info) > tol_deriv) then
              call ancmsg(msgid=3107,                                          &
                          msgtype=msgerror,                                    &
                          anmode=aninfo_blind_1,                               &
                          i1=mat_id,                                           &
                          c1=titr,                                             &
                          i2=ifunc_out(j),                                     &
                          r1=rate(j),                                          &
                          r2=lambda(j))
            endif
!
            !< Rescale back to original values
            x_out(1:nout+1,j) = x_out(1:nout+1,j)*xscale
            y_out(1:nout+1,j) = y_out(1:nout+1,j)*yscale
          enddo
!
          !< Copy the smoothed curves to the material table
          deallocate(table_mat(1)%x(1)%values,table_mat(1)%y2d)
          allocate(table_mat(1)%x(1)%values(nout+1),table_mat(1)%y2d(nout+1,nl))
          do j = 1,nl
            table_mat(1)%x(1)%values(1:nout+1) = x_out(1:nout+1,j)
            table_mat(1)%y2d(1:nout+1,j) = y_out(1:nout+1,j)    
          enddo
!
        endif
!
        ! -> Unloading curves or parameters
        ! ----------------------------------------------------------------------
        iunl_for = 0
        if (ifunc_unload > 0) then 
          iunl_for = 1
          nuvar = nuvar + 13
!
          ! --> Transform the unloading curve into a material table if defined
          yfac(nl+1) = yfac_unl*areafac
          rate(nl+1) = zero
          ifunc(nl+1) = ifunc_unload
          call func_table_copy(table_mat(2) ,titr      ,mat_id   ,1        ,   &
                               ifunc(nl+1)  ,rate(nl+1),x1scale  ,x2scale  ,   &
                               yfac(nl+1)   ,ntable    ,table    ,ierr     )  
!
          !< Ensure the closed loop between quasi-static loading and unloading curves
          allocate(x_out2(nout+1),y_out2(nout+1))
          npt2 = size(table_mat(2)%x(1)%values)
          table_mat(2)%x(1)%values(1) = table_mat(1)%x(1)%values(1)
          table_mat(2)%x(1)%values(npt2) = table_mat(1)%x(1)%values(nout+1)
          if (ndim == 1) then
            table_mat(2)%y1d(1) = table_mat(1)%y1d(1)
            table_mat(2)%y1d(npt2) = table_mat(1)%y1d(nout+1)
          else
            table_mat(2)%y1d(1) = table_mat(1)%y2d(1,1)
            table_mat(2)%y1d(npt2) = table_mat(1)%y2d(nout+1,1)
          endif
!
          !< Normalize abscissa and ordinate prior to spline fitting
          xscale = max(abs(table_mat(2)%x(1)%values(1)),                       &
                           table_mat(2)%x(1)%values(npt2))
          table_mat(2)%x(1)%values(1:npt2) = table_mat(2)%x(1)%values(1:npt2)/ &
                                                                   xscale
          yscale = max(abs(table_mat(2)%y1d(1)),table_mat(2)%y1d(npt2))
          table_mat(2)%y1d(1:npt2) = table_mat(2)%y1d(1:npt2)/yscale
!
          !< Re-allocate temporary arrays
          if (allocated(x_raw)) deallocate(x_raw)
          if (allocated(y_raw)) deallocate(y_raw)
          allocate(x_raw(npt2),y_raw(npt2,1))
!
          !< Copy raw data into temporary arrays
          x_raw(1:npt2) = table_mat(2)%x(1)%values(1:npt2)
          y_raw(1:npt2,1) = table_mat(2)%y1d(1:npt2)
!
          ! --> Spline interpolation, smoothing, ensure monotonicity and go 
          !     through the origin (0,0)
          call table_mat_spline_fit(npt2    ,x_raw(1:npt2),y_raw(1:npt2,1),    &
                                    nout    ,x_out2       ,y_out2         ,    &
                                    real(lambda(1),kind=8),info           )
!
          !< Check if spline fit was successful
          if (abs(info) > tol_deriv) then
            call ancmsg(msgid=3108,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_1,                                 &
                        i1=mat_id,                                             &
                        c1=titr,                                               &
                        i2=ifunc_unload,                                       &
                        r2=lambda(1))
          endif
!
          !< Rescale back to original values
          x_out2(1:nout+1) = x_out2(1:nout+1)*xscale
          y_out2(1:nout+1) = y_out2(1:nout+1)*yscale
!
          !< Make sure again the unloading curve makes a closed loop with the 
          !  loading curve
          x_out2(1) = table_mat(1)%x(1)%values(1)
          if (ndim == 1) then 
            y_out2(1) = table_mat(1)%y1d(1)
          else
            y_out2(1) = table_mat(1)%y2d(1,1)
          endif
          x_out2(nout+1) = table_mat(1)%x(1)%values(nout+1)
          if (ndim == 1) then 
            y_out2(nout+1) = table_mat(1)%y1d(nout+1)
          else
            y_out2(nout+1) = table_mat(1)%y2d(nout+1,1)
          endif
!
          !< Copy the unloading curve to the material table
          deallocate(table_mat(2)%x(1)%values,table_mat(2)%y1d)
          allocate(table_mat(2)%x(1)%values(nout+1),table_mat(2)%y1d(nout+1))
          table_mat(2)%x(1)%values(1:nout+1) = x_out2(1:nout+1)
          table_mat(2)%y1d(1:nout+1) = y_out2(1:nout+1)
        ! --> Hysteretic unloading parameters
        elseif (hys /= zero) then
          iunl_for = 2
          hys = abs(hys)
          hys = max(hys,zero)
          hys = min(hys,one)
        endif 
!
        !<======================================================================
        !< Specific treatment for compressible materials
        !<======================================================================  
        if ((nu > zero) .and. (nu < 0.49d0)) then
          !< Effective bulk modulus
          do i = 1, nout+1
            if (table_mat(1)%x(1)%values(i) /= zero) then
              lam = table_mat(1)%x(1)%values(i) + one
              rv  = lam**(one - two*nu)
              if (ndim == 1) then            
                bulk = min(bulk,(1.0d0 - 0.02d0)*table_mat(1)%y1d(i)*rv/log(rv))
              elseif (ndim == 2) then
                do j = 1,nl
                  bulk = min(bulk,(1.0d0 - 0.02d0)*table_mat(1)%y2d(i,j)*rv/log(rv))
                enddo
              endif
            endif
          enddo
          !< Remove the volumetric part from the stress-strain curve
          do i = 1, nout+1         
            lam = table_mat(1)%x(1)%values(i) + one
            rv  = lam**(one - two*nu)
            if (ndim == 1) then
              table_mat(1)%y1d(i) = table_mat(1)%y1d(i) - bulk*(log(rv))
            elseif (ndim == 2) then
              do j = 1,nl
                table_mat(1)%y2d(i,j) = table_mat(1)%y2d(i,j) - bulk*(log(rv))
              enddo
            endif 
            if (iunl_for == 1) then
              table_mat(2)%y1d(i) = table_mat(2)%y1d(i) - bulk*(log(rv))
            endif
          enddo
        endif
!
        !<======================================================================
        !< Automatic estimation of the elastic parameters
        !<======================================================================
        !< Number of dimensions (> 1 if strain rate dependent)         
        ndim = table_mat(1)%ndim      
        !< Initialize the slope measure
        dydx = zero
        !< Maximum stress
        sigpeak = zero
        !< Loop over the loading curves points
        if (ndim == 1) then
          do i = 1,nout-1
            dx = table_mat(1)%x(1)%values(i+1) - table_mat(1)%x(1)%values(i)
            dy = table_mat(1)%y1d(i+1) - table_mat(1)%y1d(i)
            !< Compute the true stress vs true strain slope
            dydx = max((dy/dx)*(one + (table_mat(1)%x(1)%values(i+1)))**2,dydx)
            sigpeak = max(sigpeak,abs(table_mat(1)%y1d(i+1)),                  &
                                  abs(table_mat(1)%y1d(i)))
          enddo
        elseif (ndim == 2) then
          do j = 1,nl
            do i = 1,nout-1
              dx = table_mat(1)%x(1)%values(i+1) - table_mat(1)%x(1)%values(i)
              dy = table_mat(1)%y2d(i+1,j) - table_mat(1)%y2d(i,j)
              !< Compute the true stress vs true strain slope
              dydx = max((dy/dx)*(one+(table_mat(1)%x(1)%values(i+1)))**2,dydx)
              sigpeak = max(sigpeak,abs(table_mat(1)%y2d(i,j)),                &
                                    abs(table_mat(1)%y2d(i+1,j)))
            enddo
          enddo
        endif
        !< If there is a second table, compute the slope for the unloading curve
        if (matparam%ntable > 1) then 
          do i = 1,nout-1
            dx = table_mat(2)%x(1)%values(i+1) - table_mat(2)%x(1)%values(i)
            dy = table_mat(2)%y1d(i+1) - table_mat(2)%y1d(i)
            dydx = max((dy/dx)*(one + table_mat(2)%x(1)%values(i+1))**2,dydx)
            sigpeak = max(sigpeak,abs(table_mat(2)%y1d(i)),                    &
                                  abs(table_mat(2)%y1d(i+1)))
          enddo
        endif
!
        !<======================================================================
        !< Normalization of the loading and unloading curves    
        !<======================================================================
        if (matparam%ntable > 1) then
          !< Normalize the unloading curve
          where (table_mat(2)%x(1)%values < zero) & 
             table_mat(2)%x(1)%values = table_mat(2)%x(1)%values/abs(x_out2(1))
          where (table_mat(2)%x(1)%values >= zero) & 
             table_mat(2)%x(1)%values = table_mat(2)%x(1)%values/x_out2(nout+1)
          where (table_mat(2)%y1d < zero) & 
             table_mat(2)%y1d = table_mat(2)%y1d/abs(y_out2(1))
          where (table_mat(2)%y1d >= zero) &
             table_mat(2)%y1d = table_mat(2)%y1d/y_out2(nout+1)
          !< Normalize the loading curve
          table_mat(3)%ndim = ndim
          allocate(table_mat(3)%x(ndim))
          if (ndim == 1) then
            allocate(table_mat(3)%x(1)%values(nout+1),                         &
                             table_mat(3)%y1d(nout+1))
            table_mat(3)%x(1)%values(1:nout+1) =                               &
                             table_mat(1)%x(1)%values(1:nout+1)
            table_mat(3)%y1d(1:nout+1) = table_mat(1)%y1d(1:nout+1)
            where (table_mat(3)%x(1)%values < zero)                            & 
              table_mat(3)%x(1)%values =                                       &
                      table_mat(3)%x(1)%values/abs(x_out(1,1))
            where (table_mat(3)%x(1)%values >= zero)                           & 
              table_mat(3)%x(1)%values =                                       &
                      table_mat(3)%x(1)%values/x_out(nout+1,1)
            where (table_mat(3)%y1d < zero) & 
                   table_mat(3)%y1d = table_mat(3)%y1d/abs(y_out(1,1))
            where (table_mat(3)%y1d >= zero) & 
                   table_mat(3)%y1d = table_mat(3)%y1d/y_out(nout+1,1)
          elseif (ndim == 2) then
            allocate(table_mat(3)%x(1)%values(nout+1),                         &
                     table_mat(3)%x(2)%values(nl),                             &
                     table_mat(3)%y2d(nout+1,nl))
            table_mat(3)%x(1)%values(1:nout+1) =                               &
                     table_mat(1)%x(1)%values(1:nout+1)
            table_mat(3)%y2d(1:nout+1,1:nl) =                                  &
                     table_mat(1)%y2d(1:nout+1,1:nl)
            table_mat(3)%x(2)%values(1:nl) =                                   &
                     table_mat(1)%x(2)%values(1:nl)
            where (table_mat(3)%x(1)%values < zero)                            & 
                   table_mat(3)%x(1)%values =                                  &
                     table_mat(3)%x(1)%values/abs(x_out(1,1))
            where (table_mat(3)%x(1)%values >= zero)                           & 
                   table_mat(3)%x(1)%values =                                  &
                     table_mat(3)%x(1)%values/x_out(nout+1,1)
            do i = 1, nl 
              where (table_mat(3)%y2d(:,i) < zero)                             & 
                     table_mat(3)%y2d(:,i) =                                   &
                       table_mat(3)%y2d(:,i)/abs(y_out(1,i))
              where (table_mat(3)%y2d(:,i) >= zero)                            & 
                     table_mat(3)%y2d(:,i) =                                   &
                       table_mat(3)%y2d(:,i)/y_out(nout+1,i)
            enddo
          endif
        endif
!
        !<======================================================================
        !< Convert then engineering strain abscissa to stretches
        !<======================================================================
        table_mat(1)%x(1)%values(1:nout+1) = one +                             &
                                             table_mat(1)%x(1)%values(1:nout+1)
!
        !<======================================================================
        !< Fill parameters table
        !<======================================================================
        !< Young modulus
        e = dydx
        !< Shear modulus 
        gs = three*bulk*e/(nine*bulk - e)
        if (gs < zero) then 
          call ancmsg(msgid=3109,                                             &
                      msgtype=msgwarning,                                     &
                      anmode=aninfo_blind_1,                                  &
                      i1=mat_id,                                              &
                      c1=titr)
          bulk = four*(e/nine)*(one + em3)
          gs = three*bulk*e/(nine*bulk - e)
        endif
!
        !< Default strain rate filtering cut-off frequency 
        israte = 1 
        if (fcut == zero) then
          ssp = sqrt((four_over_3*gs + bulk)/rho0)
          fcut = ssp/(two*nine*ep05)
        endif
!
        !< Default frictionnal damping parameters
        if (sigf  == zero) sigf  = 2.5d-4*sigpeak
        if (gdamp == zero) gdamp = ep03*sigf
!
        !< Check the default parameters
        if (shape == zero) shape = one
        if (hys   == zero) hys   = one
!     
        !< Integer material parameter
        matparam%iparam(1)  = itens
        matparam%iparam(2)  = iunl_for
        matparam%iparam(3)  = nl
        matparam%iparam(4)  = rtype
        matparam%iparam(5)  = failip
        matparam%iparam(6)  = nv_base
!    
        !< Real material parameters
        matparam%young     = e
        matparam%nu        = nu
        matparam%shear     = gs
        matparam%bulk      = bulk
        matparam%uparam(1) = hys
        matparam%uparam(2) = shape         
        matparam%uparam(3) = gdamp
        matparam%uparam(4) = sigf
        matparam%uparam(5) = kfail
        matparam%uparam(6) = gam1
        matparam%uparam(7) = gam2
        matparam%uparam(8) = eh
        matparam%uparam(9) = beta
! 
        !< Material density
        if (rhor == zero) rhor = rho0
        matparam%rho0 = rho0
        matparam%rho  = rhor
!      
        !< PARMAT table
        parmat(1)  = two*gs 
        parmat(2)  = e
        parmat(3)  = nu
        parmat(4)  = israte
        parmat(5)  = fcut
        parmat(16) = 2
        parmat(17) = two*gs/(bulk + four_over_3*gs)
!
        ! MTAG variable activation
        mtag%l_epsd = 1
        mtag%g_epsd = 1
        mtag%l_dmg  = 1
        mtag%g_dmg  = 1
! 
        !< Material model keywords
        call init_mat_keyword(matparam,"INCOMPRESSIBLE")
        call init_mat_keyword(matparam,"INCREMENTAL")
        call init_mat_keyword(matparam,"LARGE_STRAIN")
!
        !< Properties compatibility
        call init_mat_keyword(matparam,"SOLID_ISOTROPIC")
        call init_mat_keyword(matparam,"SHELL_ISOTROPIC")
!
        !< Table deallocation
        deallocate(x_out,y_out)
!
!-------------------------------------------------------------------------------
        !< Printing out the material data
!-------------------------------------------------------------------------------
        write(iout,1010) trim(titr),mat_id,ilaw
        write(iout,1000)     
        if (is_encrypted) then
          write(iout,'(5X,A,//)')'CONFIDENTIAL DATA'
        else     
          write(iout,1020) rho0 
          write(iout,1100) bulk,nu,e,gs
          write(iout,1600) sgl,sw,st
          write(iout,1150) nl
          write(iout,1200) (i,ifunc_out(i),yfac(i),rate(i),lambda(i),i=1,nl) 
          if(iunl_for == 1) then
            write(iout,1300) ifunc_unload,yfac_unl,lambda(1)
          elseif(iunl_for == 2 .or. iunl_for == 3) then
            write(iout,1400) hys,shape
          endif
          write(iout,1500) gdamp,sigf
          if (nl > 1) write(iout,1700) itens,rtype,fcut
          if (beta > zero) write(iout,1800) beta
          if ((kfail > zero).and.(failip > 0)) then
            write(iout,1900) kfail,gam1,gam2,eh,failip
          elseif ((kfail > zero).and.(failip == 0)) then
            write(iout,1950) kfail,gam1,gam2,eh
          endif
          write(iout,2000) 
        endif     
!-----------------
 1000 format(/                                                                 &            
       5X,'-------------------------------------------------------',/          &     
       5X,'        MATERIAL MODEL: TABULATED HYPERELASTIC         ',/,         & 
       5X,'-------------------------------------------------------',/)    
 1010 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . . . .=',I10/,      &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . . . .=',I10/) 
 1020 format(/                                                                 &
       5X,'INITIAL DENSITY. . . . . . . . . . . . . . . . . . . .=',1PG20.13/) 
 1100 format(/                                                                 &
       5X,'ELASTIC PARAMETERS:                                    ',/,         & 
       5X,'-------------------                                    ',/,         &
       5X,'BULK MODULUS (K) . . . . . . . . . . . . . . . . . . .=',1PG20.13/, & 
       5X,"POISSON''S RATIO (NU). . . . . . . . . . . . . . . . .=",1PG20.13/, &
       5X,'MEASURED YOUNG MODULUS (E) . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'COMPUTED SHEAR MODULUS (G) . . . . . . . . . . . . . .=',1PG20.13/)
 1600 format(/                                                                 &
       5X,'GEOMETRIC PARAMETERS OF TENSILE TEST:                  ',/,         &
       5X,'-------------------------------------                  ',/,         &
       5X,'SPECIMEN GAUGE LENGTH (SGL) . . . . .  . . . . . . . .=',1PG20.13/, &
       5X,'SPECIMEN WIDTH (SW). . . . . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'SPECIMEN THICKNESS (ST). . . . . . . . . . . . . . . .=',1PG20.13/) 
 1150 format(/                                                                 &
       5X,'LOADING FUNCTIONS:                                     ',/,         & 
       5X,'------------------                                     ',/,         &
       5X,'NUMBER OF LOADING FUNCTIONS (NL) . . . . . . . . . . .=',I10/)
 1200 format(/                                                                 &
       5X,'FUNCTION NUMBER #'                                      ,I3/,       &
       5X,'LOADING STRESS-STRAIN FUNCTION ID. . . . . . . . . . .=',I10/,      &
       5X,'STRESS SCALE FACTOR. . . . . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'STRAIN RATE. . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'SPLINE SMOOTHING FACTOR (LAMBDA) . . . . . . . . . . .=',1PG20.13/)
 1300 format(/                                                                 &
       5X,'UNLOADING BEHAVIOR PARAMETERS:                         ',/,         &
       5X,'------------------------------                         ',/,         &
       5X,'UNLOADING STRESS-STRAIN FUNCTION ID. . . . . . . . . .=',I10/,      &
       5X,'STRESS SCALE FACTOR. . . . . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'SPLINE SMOOTHING FACTOR (LAMBDA) . . . . . . . . . . .=',1PG20.13/)    
 1400 format(/                                                                 &
       5X,'UNLOADING BEHAVIOR PARAMETERS:                         ',/,         &
       5X,'------------------------------                         ',/,         &
       5X,'HYSTERETIC UNLOADING FACTOR . . . . .  . . . . . . . .=',1PG20.13/, &
       5X,'SHAPE UNLOADING FACTOR . . . . . . . . . . . . . . . .=',1PG20.13/) 
 1500 format(/                                                                 &
       5X,'FRICTIONNAL DAMPING PARAMETERS:                        ',/,         &
       5X,'-------------------------------                        ',/,         &
       5X,'DAMPING SHEAR MODULUS (GDAMP) . . . .  . . . . . . . .=',1PG20.13/, &
       5X,'DAMPING LIMIT STRESS (SIGF). . . . . . . . . . . . . .=',1PG20.13/) 
 1700 format(/                                                                 &
       5X,'STRAIN RATE DEPENDENCY PARAMETERS:                     ',/,         &
       5X,'----------------------------------                     ',/,         &
       5X,'RATE EFFECT FLAG (TENSION) . . . . . . . . . . . . . .=',I10/,      &
       5X,'   = -1: RATE EFFECT CONSIDERED DURING TENSION/COMPRESSION FOR LOADING ONLY,',/, &
       5X,'   =  0: RATE EFFECT CONSIDERED FOR COMPRESSIVE LOADING ONLY,',/,    &
       5X,'   =  1: RATE EFFECT CONSIDERED IDENTICALLY FOR LOADING AND UNLOADING,',/, &
       5X,'         TENSION AND COMPRESSION.',/,                                & 
       5X,'STRAIN RATE TYPE FLAG (RTYPE). . . . . . . . . . . . .=',I10/,      &
       5X,'   =  0: TRUE STRAIN RATE                              ',/,         &
       5X,'   =  1: ENGINEERING STRAIN RATE                       ',/,         &
       5X,'STRAIN RATE FILTERING FREQUENCY (FCUT) . . . . . . . .=',1PG20.13/)
 1800 format(/                                                                 &
       5X,'PRESSURE DAMPING PARAMETERS (SOLIDS ONLY):             ',/,         &
       5X,'------------------------------------------             ',/,         &
       5X,'EXPONENTIAL FILTERING FREQUENCY (BETA) . . . . . . . .=',1PG20.13/)
 1900 format(/                                                                 &
       5X,'FAILURE AND SOFTENING PARAMETERS:                      ',/          &     
       5X,'---------------------------------                      ',/,         & 
       5X,'FENG AND HALLQUIST CRITERION THRESHOLD (KFAIL) . . . .=',1PG20.13/, &
       5X,'CRITERION SHAPE PARAMETER 1 (GAM1) . . . . . . . . . .=',1PG20.13/, &
       5X,'CRITERION SHAPE PARAMETER 2 (GAM2) . . . . . . . . . .=',1PG20.13/, &
       5X,'SOFTENING PARAMETERS (EH). . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'NUMBER OF FAILED INTG. PTS PRIOR TO ELEMENT DELETION .=',I10/)
 1950 format(/                                                                 &
       5X,'FAILURE AND SOFTENING PARAMETERS:                      ',/          &     
       5X,'---------------------------------                      ',/,         & 
       5X,'FENG AND HALLQUIST CRITERION THRESHOLD (KFAIL) . . . .=',1PG20.13/, &
       5X,'CRITERION SHAPE PARAMETER 1 (GAM1) . . . . . . . . . .=',1PG20.13/, &
       5X,'CRITERION SHAPE PARAMETER 2 (GAM2) . . . . . . . . . .=',1PG20.13/, &
       5X,'SOFTENING PARAMETERS (EH). . . . . . . . . . . . . . .=',1PG20.13/, &
       5X,'NO ELEMENT DELETION AFTER FULL DAMAGE                  ',/)
 2000 format(/                                                                 &
       5X,'-------------------------------------------------------',/)
      end subroutine hm_read_mat88
!
      !<========================================================================
      !< Find intersection between two curves in a 2D material table
      !<========================================================================
!||====================================================================
!||    table_mat2d_intersect   ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat88           ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine table_mat2d_intersect(                                        &
        table_mat,i_low    ,i_high   ,npt      ,use_abs  ,lock_neg ,lock_pos , &
        epsx, epsy, found, xint, yint)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------        
        use table4d_mod
        use constant_mod    
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        type(table_4d_) , intent(in)  :: table_mat
        integer         , intent(in)  :: i_low, i_high, npt
        logical         , intent(in)  :: use_abs
        logical         , intent(in)  :: lock_neg
        logical         , intent(in)  :: lock_pos
        real(kind=8)    , intent(in)  :: epsx
        real(kind=8)    , intent(in)  :: epsy
        logical         , intent(out) :: found
        real(kind=8)    , intent(out) :: xint
        real(kind=8)    , intent(out) :: yint
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer  :: k
        real(kind=8) :: x1,x2,yl1,yl2,yh1,yh2,dl1,dl2,denom,a,ycross
        logical  :: in_window
!      
        found = .false.
        xint  = zero
        yint  = zero
!
        if (npt < 2) return
!  
        do k = 1, npt-1
          x1  = table_mat%x(1)%values(k)
          x2  = table_mat%x(1)%values(k+1)
!      
          !< Keep only the side(s) you want
          in_window = ( (lock_pos .and. x1 >= -epsx) .or. (lock_neg .and. x1 <= epsx) )
          if (.not. in_window) cycle
!      
          yl1 = table_mat%y2d(k  , i_low )
          yl2 = table_mat%y2d(k+1, i_low )
          yh1 = table_mat%y2d(k  , i_high)
          yh2 = table_mat%y2d(k+1, i_high)
!      
          if (use_abs) then
            dl1 = abs(yh1) - abs(yl1)
            dl2 = abs(yh2) - abs(yl2)
          else
            dl1 = (yh1 - yl1)
            dl2 = (yh2 - yl2)
          end if
!      
          !< Crossing if difference changes sign (or is within epsy)
          if ( (dl1*dl2 <= zero) .and. (abs(dl1) > epsy .or. abs(dl2) > epsy) ) then
            !< Solve yl(x) = yh(x) with linear interpolation on [x1,x2]
            !  yl = yl1 + a*(yl2-yl1); yh = yh1 + a*(yh2-yh1)
            denom = (yh2 - yh1) - (yl2 - yl1)
            if (abs(denom) > max(1.d-300, ten*epsilon(denom)) ) then
              a = (yl1 - yh1) / denom
              if (a >= -em12 .and. a <= one + em12) then
                a     = min(one, max(zero, a))
                xint  = x1 + a*(x2 - x1)
                ycross= yl1 + a*(yl2 - yl1)
                yint  = merge(abs(ycross),ycross,use_abs)
                !< Intersection found if within the tolerances
                !  (origin is not considered as intersection)
                if ((abs(xint) > em12).and.(abs(yint) > em12)) then 
                  found = .true.
                  return
                endif
              end if
            end if
          end if
        end do
      end subroutine table_mat2d_intersect
      !<========================================================================
!
      !<========================================================================
      !< Ensure monotonicity in rate between two curves (y_high above y_low)
      !<========================================================================
!||====================================================================
!||    monotone_in_rate_signed_highfix   ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat88                     ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- calls      -----------------------------------------------------
!||    light_edge_smooth                 ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine monotone_in_rate_signed_highfix(                              &
        n        ,x        ,y_low    ,y_high   ,eps_rel  ,eps_abs  ,lock_neg,  &
        lock_pos )
!-----------------------------------------------
!   M o d u l e s
!----------------------------------------------- 
        use constant_mod    
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)    :: n
        real(kind=8), intent(in)    :: x(n)
        real(kind=8), intent(in)    :: y_low(n)
        real(kind=8), intent(inout) :: y_high(n)
        real(kind=8), intent(in)    :: eps_rel
        real(kind=8), intent(in)    :: eps_abs
        logical, intent(in)         :: lock_neg
        logical, intent(in)         :: lock_pos
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: j
        real(kind=8) :: base_abs, targ_abs, need_abs, sgn
!
        !< Loop over all points
        do j = 1, n
          !< Apply only on the requested side (depending on the sign of x or y_low)
          if ( (x(j) <  zero .and. .not. lock_neg) ) cycle
          if ( (x(j) >= zero .and. .not. lock_pos) ) cycle
!
          base_abs = dabs(y_low(j))
          targ_abs = dabs(y_high(j))
!      
          !< Minimum needed level (strictly above)
          need_abs = base_abs*(one + eps_rel) + eps_abs
          if (targ_abs < need_abs) then
            !< Keep the current sign of the fast curve.
            sgn = one
            if (y_high(j) < zero) sgn = -one
            y_high(j) = sgn * need_abs
          end if
        end do
!      
        !< Light smoothing of edges to avoid local "steps"
        call light_edge_smooth(n, y_high)
!      
      contains
!      
        !< Small smoothing of edges to avoid local "steps"
!||====================================================================
!||    light_edge_smooth                 ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||--- called by ------------------------------------------------------
!||    monotone_in_rate_signed_highfix   ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||====================================================================
        subroutine light_edge_smooth(n, y)
          implicit none
          integer, intent(in)    :: n
          real(kind=8), intent(inout) :: y(n)
          integer :: k
          real(kind=8) :: ym, yp, yk, corr
          if (n <= 2) return
          do k = 2, n-1
            yk = y(k)
            ym = y(k-1); yp = y(k+1)
            corr = 0.30d0 * ( (ym + yp)/two - yk )
            y(k) = y(k) + corr
          end do
        end subroutine light_edge_smooth
      
      end subroutine monotone_in_rate_signed_highfix
      !<========================================================================
!
      end module hm_read_mat88_mod 
