!||====================================================================
!||    rwall_pen_mod   ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||--- called by ------------------------------------------------------
!||    resol           ../engine/source/engine/resol.F
!||====================================================================
      module rwall_pen_mod
      implicit none
      contains
! ======================================================================================================================
! \brief penalty formulation of rwall 
! ======================================================================================================================
!||====================================================================
!||    rgwal0_pen      ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||--- called by ------------------------------------------------------
!||    resol           ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    my_barrier      ../engine/source/system/machine.F
!||    rgwalt          ../engine/source/constraints/general/rwall/rgwal0.F
!||    rwall_fpen      ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    spmd_exch_fr6   ../engine/source/mpi/kinematic_conditions/spmd_exch_fr6.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    my_alloc_mod    ../common_source/tools/memory/my_alloc.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||    rwall_mod       ../common_source/modules/constraints/rwall_mod.F90
!||====================================================================
      subroutine rgwal0_pen(x          ,a           ,v        ,ms        ,numnod    ,    &
                            fsav       ,nfsav       ,frwl6    ,weight_md ,ncycle    ,    &
                            fopt       ,dimfb       ,fbsav6   ,stabsens  ,tablesensor,   &
                            nsect_offset,stifn     ,dt1       ,nspmd     ,nrwall    ,    &
                            rwall     )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use rwall_mod
          use my_alloc_mod
          use precision_mod,         only : WP
          use constant_mod,          only : zero,half,em20,one
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: nrwall          !< number of rwall
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: ncycle          !< number of cycle
          integer, intent(in)                                      :: nspmd           !< number of spmd
          integer, intent(in)                                      :: nfsav           !< 1er dimension of fsav NTHVKI
          integer, intent(in)                                      :: dimfb           !< last dimension of fbsav6
          integer, intent(in)                                      :: stabsens        !< dimension of tablesensor
          integer, intent(in)                                      :: nsect_offset    !< pointer offset for sensor table
          integer,       dimension(stabsens), intent(inout)        :: tablesensor     !< output data for table sensor
          integer,       dimension(numnod),  intent(in   )         :: weight_md       !< nodal weight multi_domain
          real(kind=WP), dimension(numnod),  intent(in   )         :: ms              !< nodal mass
          real(kind=WP), dimension(3,numnod),intent(in   )         :: x               !< coordinates
          real(kind=WP), dimension(3,numnod),intent(in   )         :: v               !< velocity
          real(kind=WP), dimension(3,numnod),intent(inout)         :: a               !< force
          real(kind=WP), dimension(6,nrwall),intent(inout)         :: fopt            !< rwall output data
          real(kind=WP), dimension(nfsav,nrwall),intent(inout)     :: fsav            !< rwall output data
          real(kind=8 ), dimension(12,6,dimfb)  ,intent(inout)     :: fbsav6          !< rwall output data
          real(kind=8 ),dimension(7,6,nrwall),intent(inout)        :: frwl6           !< rwall work data P/ON
          real(kind=WP), dimension(numnod),  intent(inout)         :: stifn           !< nodal stiffness
          real(kind=WP),                     intent(in   )         :: dt1             !< timr step
          type(rwall_) ,                     intent(inout)         :: rwall           !< rwall structure
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i,n, ifq, j, k,nsn,ipen,n_p,ns,msr,itype,itied,ncont,nm,isl,pmain,iparsens,isect,npen
      real(kind=WP) :: stif_max,stf
      real(kind=8 ), dimension(12,6) :: dummy         
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!$OMP DO
          do n = 1, nrwall
             frwl6(:,:,n) = zero
          end do
!$OMP END DO
! initialization at T=0
!$OMP SINGLE
      if (ncycle == 0) then ! initialization of stifm
        call my_alloc(rwall%pen%pen_old,rwall%pen%lnspen)
        if (rwall%pen%lnspen>0) rwall%pen%pen_old = zero
        nm = 0
        do n = 1, nrwall
          msr = rwall%nprw(n,3)
          ipen = rwall%nprw(n,9)
          if (ipen > 0 .and. msr > 0) nm = nm + 1
        end do
        rwall%pen%lrwmove = nm
        call my_alloc(rwall%pen%stifm,rwall%pen%lrwmove)
        if (nm>0) rwall%pen%stifm = zero
      end if
!$OMP END SINGLE
! penalty forces& nodal stif update; normal and tangential
      k = 1
      n_p = 1
      nm = 0
      isl = 1
      npen = 0
      do n = 1, nrwall
        nsn   = rwall%nprw(n,1)
        itied = rwall%nprw(n,2)
        msr   = rwall%nprw(n,3)
        itype = rwall%nprw(n,4)
        ipen  = rwall%nprw(n,9)
        ifq = nint(rwall%rwbuf(15,n))
        if (ipen > 0) then 
          stif_max = zero
          npen = npen + 1
          call rwall_fpen(x     ,a     ,v     ,itype,                      &
                          rwall%lprw(k),nsn,itied,msr,                     &
                          ms    ,rwall%rwbuf(1,n),rwall%nrwlp,             &
                          rwall%rwsav(isl),frwl6(:,:,n),weight_md,         &
                          stifn,stif_max,rwall%pen%stif(n_p),              &
                          rwall%pen%pen_old(n_p),rwall%pen%leng_m(npen),   &
                          rwall%pen%ft(1,n_p),dt1,rwall%nprw(n,5),         &
                          numnod)
          n_p = n_p + nsn
          if (msr > 0) then 
            nm = nm + 1
            rwall%pen%stifm(nm) = stif_max
          end if
        end if
        k = k + nsn
        if (ifq > 0) isl = isl + 3*nsn
        if (itype == -1) k = k + nint(rwall%rwbuf(8,n))
      end do
!
      CALL MY_BARRIER
!$OMP SINGLE

      nm = 0
      do n = 1, nrwall
         msr   = rwall%nprw(n,3)
         if (msr/=0) then
           if (nspmd > 1) then
              if (rwall%fr_wall(nspmd+1,n) /= 0) then
                 call SPMD_EXCH_FR6(rwall%fr_wall(1,n),frwl6(1,1,n),7*6)
              end if
              pmain = rwall%fr_wall(nspmd+2,n)
           else
              pmain = 1
           end if
         else
            pmain = 1
         end if
!
        ipen = rwall%nprw(n,9)
        if (ipen == 0) cycle 
        iparsens = 0
        isect = 0
        if (stabsens /= 0) then
          isect = tablesensor(n + nsect_offset + 1) - tablesensor(n + nsect_offset)
        endif
        if (isect /= 0) then
          iparsens = 1
          call rgwalt(rwall%nprw(n,3),rwall%rwbuf(1,n),frwl6(1,1,n),pmain,fsav(1,n),  &
                      fopt(1,n),fbsav6(1,1,isect),iparsens)
        else
          call rgwalt(rwall%nprw(n,3),rwall%rwbuf(1,n),frwl6(1,1,n),pmain,fsav(1,n),  &
                      fopt(1,n),dummy,iparsens)
        endif
        if (msr/=0) then 
          nm = nm + 1
          a(1:3,msr)=a(1:3,msr)+rwall%rwbuf(17:19,n)
          stifn(msr)=stifn(msr)+rwall%pen%stifm(nm)
        end if
    end do
!

!$OMP END SINGLE
!
      end subroutine rgwal0_pen
!
! ======================================================================================================================
! \brief penalty forces of rwall 
! ======================================================================================================================
!||====================================================================
!||    rwall_fpen            ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||--- called by ------------------------------------------------------
!||    rgwal0_pen            ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||--- calls      -----------------------------------------------------
!||    cross_product         ../engine/source/constraints/general/rbody/velrot_explicit.F90
!||    my_barrier            ../engine/source/system/machine.F
!||    sum_6_float           ../engine/source/system/parit.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    my_alloc_mod          ../common_source/tools/memory/my_alloc.F90
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    velrot_explicit_mod   ../engine/source/constraints/general/rbody/velrot_explicit.F90
!||====================================================================
      subroutine rwall_fpen(x       ,a      ,v      ,itype   ,              &
                            nsw     ,nsn    ,itied  ,msr     ,              &
                            ms      ,rwl    ,nrwl   ,rwsav   ,              &
                            frwl6   ,weight_md,stifn,stif_max,              &
                            pen_stif,pen_old,leng_m ,pen_ft  ,              &
                            dt1     ,ncont  ,numnod )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod,         only : WP
          use constant_mod,          only : zero,half,em20,one,em10,ep02,two,three,em03,zep05,one_fifth,em02,hundred
          use my_alloc_mod
          use velrot_explicit_mod,   only : cross_product
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: msr             !< main node id (>=0)
          integer, intent(in)                                      :: nsn             !< number of secondary nodes
          integer, intent(in)                                      :: nrwl            !< 1er dimension of rwl
          integer, intent(in)                                      :: itype           !< rwall type 
          integer, intent(in)                                      :: itied           !< flag tied
          integer, intent(inout)                                   :: ncont           !< num. impacted
          integer,       dimension(nsn),     intent(in   )         :: nsw             !< sencondar node lis
          integer,       dimension(numnod),  intent(in   )         :: weight_md       !< nodal weight multi_domain
          real(kind=WP), dimension(numnod),  intent(in   )         :: ms              !< nodal mass
          real(kind=WP), dimension(3,numnod),intent(in   )         :: x               !< coordinates
          real(kind=WP), dimension(3,numnod),intent(in   )         :: v               !< velocity
          real(kind=WP), dimension(3,numnod),intent(inout)         :: a               !< force
          real(kind=WP), dimension(nrwl)    ,intent(inout)         :: rwl             !< rwall data
          real(kind=WP), dimension(3,nsn)   ,intent(inout)         :: rwsav           !< rwall work data
          real(kind=8 ), dimension(7,6)     ,intent(inout)         :: frwl6           !< rwall work data P/ON
          real(kind=WP), dimension(nsn)     ,intent(in   )         :: pen_stif        !< penalty stiffness
          real(kind=WP), dimension(nsn)     ,intent(inout)         :: pen_old         !< penalty of revious cycle
          real(kind=WP), dimension(3,nsn)   ,intent(inout)         :: pen_ft          !< penaltytangent force
          real(kind=WP), dimension(numnod),  intent(inout)         :: stifn           !< nodal stiffness
          real(kind=WP),                     intent(inout)         :: stif_max        !< max stifness applied
          real(kind=WP),                     intent(in   )         :: dt1             !< timr step
          real(kind=WP),                     intent(in   )         :: leng_m          !< average length of 2nd nodes
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i, n, nindex, ifq, j, k, jj,index(nsn),ipri
      real(kind=WP) :: dp, dv, da, dvt,fndfn, ftdft, fric,                       &
                      fric2,fcoe,msw,fac,alpha,alphi,                            &
                      x_rw(3),v_rw(3),vsm(3),dsm(3),vt(3),fsn(3),fst(3),         &
                      nor(3),x1_rw(3),x2_rw(3),n3_rw(3),p_a2,vpn,dd1,dd,ra2,     &
                      dsm1(3),cm1(3),ps,cm1_a2,cm2(3),cm2_a2,stf_maxi,stf,       &
                      g11,g12,g22,detg,inv11,inv12,inv22,ksi,eta,tol,            &
                      dsm_x1,dsm_x2,stf_vi,damp,fact,stf_av,                     &
                      p_min,penref,pendr,fmax,fnon,dffac,damp2
      real(kind=WP), dimension(nsn) :: pene
      real(kind=WP), dimension(3,nsn) :: norj
      real(kind=8 ), dimension(7,6) :: frwl6_l         
      real(kind=WP), dimension(:), allocatable :: f1, f2, f3, f4, f5, f6, f7,    &
                                                  stif,fn,vn,stif_kt
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! a is the internal force here
      ncont=0
      CALL MY_BARRIER
      stf_maxi=zero
      stf_av=zero
      ipri = 0
!      if (mod(ncycle,10) == 0) ipri = 1
! case itype
        nindex=0
        pene = zero
      select case(itype)
        case (1)  !plane
          if(msr == 0)then  !fixed wall
            x_rw(1:3)=rwl(4:6)
            v_rw(1:3)=zero
          else   
            v_rw(1:3)=v(1:3,msr)
            x_rw(1:3)=x(1:3,msr)
          endif
          nor(1:3) = rwl(1:3)
! impact ?
!$OMP DO
          do i=1,nsn
            if (pen_stif(i)<=em20) cycle !free node
            n=nsw(i)
            dsm(1:3)=x(1:3,n)-x_rw(1:3)
            dp=dsm(1)*nor(1)+dsm(2)*nor(2)+dsm(3)*nor(3)
            if(dp >= zero) cycle              
!---   no need for test for penetrated nodes w/ velocity
            nindex = nindex+1
            index(nindex) = i
            pene(i) = -dp
            ncont=1
            norj(1:3,nindex) = nor(1:3)
          end do
!$OMP END DO
        case (2) !cylinder
          if(msr == 0)then  !fixed wall
            x_rw(1:3)=rwl(4:6)
            v_rw(1:3)=zero
          else   
            v_rw(1:3)=v(1:3,msr)
            x_rw(1:3)=x(1:3,msr)
          endif
          ra2=(half*rwl(7))**2
!$OMP DO
          do i=1,nsn
            if (pen_stif(i)<=em20) cycle !free node
            n=nsw(i)
            dsm(1:3)=x(1:3,n)-x_rw(1:3)
            dd1 =dsm(1)**2+dsm(2)**2+dsm(3)**2
            dd =dsm(1)*rwl(1)+dsm(2)*rwl(2)+dsm(3)*rwl(3)
            dp=dd1-dd**2
            if(dp >= ra2) cycle              
            nindex = nindex+1
            index(nindex) = i
            pene(i) = sqrt(ra2-dp)
            nor(1:3) = dsm(1:3)-dd*rwl(1:3)
            dd = sqrt(nor(1)**2+nor(2)**2+nor(3)**2)
            nor(1:3) = nor(1:3)/max(em20,dd)
            ncont=1
            norj(1:3,nindex) = nor(1:3)
          end do
!$OMP END DO
        case (3) !sphere
          if(msr == 0)then  !fixed wall
            x_rw(1:3)=rwl(4:6)
            v_rw(1:3)=zero
          else   
            v_rw(1:3)=v(1:3,msr)
            x_rw(1:3)=x(1:3,msr)
          endif
          ra2=(half*rwl(7))**2
!$OMP DO
          do i=1,nsn
            if (pen_stif(i)<=em20) cycle !free node
            n=nsw(i)
            dsm(1:3)=x(1:3,n)-x_rw(1:3)
            dp =dsm(1)**2+dsm(2)**2+dsm(3)**2
            if(dp >= ra2) cycle              
            nindex = nindex+1
            index(nindex) = i
            pene(i) = sqrt(ra2-dp)
            nor(1:3) = dsm(1:3)/max(em20,sqrt(dp))
            ncont=1
            norj(1:3,nindex) = nor(1:3)
          end do
!$OMP END DO
        case (4) !parallelogram
          if(msr == 0)then  !fixed wall
            x_rw(1:3)=rwl(4:6)
            v_rw(1:3)=zero
          else   
            v_rw(1:3)=v(1:3,msr)
            x_rw(1:3)=x(1:3,msr)
          endif
          nor(1:3) = rwl(1:3)
          x1_rw(1:3)=rwl(7:9)   !MM1
          x2_rw(1:3)=rwl(10:12) !MM2
          call cross_product(x1_rw,x2_rw,n3_rw)
          p_a2= n3_rw(1)*n3_rw(1)+n3_rw(2)*n3_rw(2)+n3_rw(3)*n3_rw(3)
          g11 = x1_rw(1)*x1_rw(1) + x1_rw(2)*x1_rw(2) + x1_rw(3)*x1_rw(3)
          g12 = x1_rw(1)*x2_rw(1) + x1_rw(2)*x2_rw(2) + x1_rw(3)*x2_rw(3)
          g22 = x2_rw(1)*x2_rw(1) + x2_rw(2)*x2_rw(2) + x2_rw(3)*x2_rw(3)
          detg = g11*g22 - g12*g12
          tol = em20
          if (abs(detg) > em10 ) then
            inv11 =  g22 / detg
            inv12 = -g12 / detg
            inv22 =  g11 / detg
!$OMP DO
            do i=1,nsn
              if (pen_stif(i)<=em20) cycle !free node
              n = nsw(i)
              dsm(1:3) = x(1:3,n) - x_rw(1:3)
              dp = dsm(1)*nor(1) + dsm(2)*nor(2) + dsm(3)*nor(3)
              if (dp >= zero) cycle
              dsm1(1:3) = dsm(1:3) - dp*nor(1:3)
              ! project dsm1 onto x1_rw,x2_rw using precomputed inverse
              dsm_x1 = dsm1(1)*x1_rw(1) + dsm1(2)*x1_rw(2) + dsm1(3)*x1_rw(3)
              dsm_x2 = dsm1(1)*x2_rw(1) + dsm1(2)*x2_rw(2) + dsm1(3)*x2_rw(3)
              ksi = inv11*dsm_x1 + inv12*dsm_x2
              ! check inside parallelogram [0,1]x[0,1] (use small tolerance)
              if (ksi < -tol .or. ksi > (one+tol)) cycle
              eta = inv12*dsm_x1 + inv22*dsm_x2
              if (eta < -tol .or. eta > (one+tol)) cycle
              nindex = nindex + 1
              index(nindex) = i
              pene(i) = -dp
              ncont = 1
              norj(1:3,nindex) = nor(1:3)
            end do
!$OMP END DO
          else
!$OMP DO
            do i=1,nsn
              if (pen_stif(i)<=em20) cycle !free node
              n=nsw(i)
              dsm(1:3)=x(1:3,n)-x_rw(1:3)
              dp=dsm(1)*nor(1)+dsm(2)*nor(2)+dsm(3)*nor(3)
              if(dp >= zero) cycle              
              dsm1(1:3)=dsm(1:3)-dp*nor(1:3)
              call cross_product(x1_rw,dsm1,cm1)
              ps = n3_rw(1)*cm1(1)+n3_rw(2)*cm1(2)+n3_rw(3)*cm1(3)
              if(ps < zero) cycle 
              cm1_a2 = cm1(1)**2+cm1(2)**2+cm1(3)**2             
              if(cm1_a2 > p_a2) cycle 
              call cross_product(dsm1,x2_rw,cm2)
              ps = n3_rw(1)*cm2(1)+n3_rw(2)*cm2(2)+n3_rw(3)*cm2(3)
              if(ps < zero) cycle 
              cm2_a2 = cm2(1)**2+cm2(2)**2+cm2(3)**2
              if(cm2_a2 > p_a2) cycle
              nindex = nindex+1
              index(nindex) = i
              pene(i) = -dp
              ncont=1
              norj(1:3,nindex) = nor(1:3)
            end do
!$OMP END DO
          end if !(abs(detg) > em10 ) then
     end select
! if any impact, compute forces
      if (nindex > 0) then
        call my_alloc(f1, nindex)
        call my_alloc(f2, nindex)
        call my_alloc(f3, nindex)
        call my_alloc(f4, nindex)
        call my_alloc(f5, nindex)
        call my_alloc(f6, nindex)
        call my_alloc(f7, nindex)
        call my_alloc(fn, nindex)
        call my_alloc(vn, nindex)
        call my_alloc(stif, nindex)
        call my_alloc(stif_kt, nindex) 
! nonlinear stiffness update
         penref = one_fifth*leng_m
         p_min = em02*penref
         fmax = hundred/three
         fnon = hundred
         dffac = zep05
        do j = 1,nindex
           i = index(j)
           n=nsw(i)
           stif(j) = pen_stif(i)
           stif_kt(j) = stif(j)
           if (pene(i) <= p_min .or. pen_old(i)==zero) cycle
           pendr = (pene(i)/penref)**2
           fac  = min(fmax,(one+fnon*pendr))
           fact = one+three*fnon*pendr
           if (fac == fmax) fact = fmax
           if (pene(i) > pen_old(i)) fac =fac + dffac
           stif(j) = fac*stif(j)
           stif_kt(j) = fact*stif(j)
         end do
! normal force 
!         if (nindex>0.and.ipri>0) write(iout,*)'itype,nindex,ncycle=',itype,nindex,ncycle
         damp= zep05
         damp2= two*damp
         do j = 1,nindex
           i = index(j)
           n=nsw(i)
!
           msw=ms(n)
           nor(1:3)=norj(1:3,j) 
           vsm(1:3)=v(1:3,n)-v_rw(1:3)
           vn(j) = vsm(1)*nor(1)+vsm(2)*nor(2)+vsm(3)*nor(3) 
           stf_vi = damp2*sqrt(stif(j)*msw)
           fn(j) = -stif(j)*pene(i)+stf_vi*vn(j)
           fsn(1:3)=fn(j)*nor(1:3) 
           dvt = fn(j)*weight_md(n)*dt1 ! force*dt same as kinetic formulation
           f1(j) = dvt*nor(1)
           f2(j) = dvt*nor(2)
           f3(j) = dvt*nor(3)
           f4(j) = msw*weight_md(n) 
           a(1:3,n)=a(1:3,n)-fsn(1:3)
           pen_old(i) = pene(i)
           stif_kt(j) = stif_kt(j)*(one+damp2)
           stifn(n)=stifn(n)+stif_kt(j)
           stf_maxi = max(stf_maxi,stif_kt(j))
           stf_av = stf_av + stif(j)
         end do
!         if (nindex>0.and.ipri>0) write(iout,*)'stf_maxi,stf_av,leng_m,ncycle=',stf_maxi,stf_av/nindex,leng_m,ncycle
! tangential force
!    
        ifq = nint(rwl(15))
        f5 = zero
        f6 = zero
        f7 = zero
      if(itied > 0)then
        do j = 1,nindex
          i = index(j)
          n=nsw(i)
! penalty incremental
          nor(1:3)=norj(1:3,j) 
          vsm(1:3)=v(1:3,n)-v_rw(1:3)
          vt(1:3)=vsm(1:3)-vn(j)*nor(1:3)
          pen_ft(1:3,i)=pen_ft(1:3,i)+stif(j)*vt(1:3)*dt1
        end do
        do i=1,nsn
          if(pene(i) > zero) cycle 
!          pen_ft(1:3,i) = zero !rebound
          if (ifq > 0) rwsav(1:3,i) = zero 
        end do
      end if !(itied >1)
!
      if(itied == 1) then
       do j = 1,nindex
        i = index(j)
        n=nsw(i)
        fst(1:3)= pen_ft(1:3,i)
        a(1:3,n)=a(1:3,n)-fst(1:3)
        dvt = weight_md(n)*dt1 ! force*dt same as kinetic formulation
        f5(j) = fst(1)*dvt
        f6(j) = fst(2)*dvt
        f7(j) = fst(3)*dvt
       enddo
      elseif(itied >1)then  ! w/ friction
       if (ifq > 0) then
!---     friction filtering
         fric = rwl(13)
         alpha= rwl(14)
         if (ifq == 3) alpha = alpha * dt1
         alphi = one - alpha
         fric2 = fric**2
         do j = 1,nindex
           i = index(j)
           n=nsw(i)
           dvt = fn(j)   
           nor(1:3)=norj(1:3,j) 
           fsn(1:3)=dvt*nor(1:3)
           fst(1:3)= pen_ft(1:3,i)
!---       filter
           fst(1:3) = fst(1:3) * alpha + rwsav(1:3,i) * alphi
!---
           fndfn=fsn(1)**2+fsn(2)**2+fsn(3)**2
           ftdft=fst(1)**2+fst(2)**2+fst(3)**2
           if (fndfn == 0) then
             rwsav(1:3,i) = zero
           else
             rwsav(1:3,i) = fst(1:3)
           endif
           fcoe=min(one,fric*sqrt(fndfn/max(em20,ftdft)))
           fst(1:3)=fcoe*fst(1:3)
           a(1:3,n)=a(1:3,n)-fst(1:3)
           vsm(1:3)=v(1:3,n)-v_rw(1:3)
           vt(1:3)=vsm(1:3)-vn(j)*nor(1:3)
!           de = (vt(1)*fst(1)+vt(2)*fst(2)+vt(3)*fst(3))*dt1
           dvt = weight_md(n)*dt1 
           f5(j) = fst(1)*dvt
           f6(j) = fst(2)*dvt
           f7(j) = fst(3)*dvt
         end do
       else
!---     no filtering
         fric=rwl(13)
         fric2=fric**2
         do j = 1,nindex
           i = index(j)
           n=nsw(i)
           nor(1:3)=norj(1:3,j) 
           dvt = fn(j)    
           fsn(1:3)=dvt*nor(1:3)
           fst(1:3)= pen_ft(1:3,i)
           fndfn=fsn(1)**2+fsn(2)**2+fsn(3)**2
           ftdft=fst(1)**2+fst(2)**2+fst(3)**2
!
           if(ftdft > fric2*fndfn) then
!---         sliding secnd point     
             fcoe=fric*sqrt(fndfn/ftdft)
             fst(1:3)=fcoe*fst(1:3)
           end if
           a(1:3,n)=a(1:3,n)-fst(1:3)
           dvt = weight_md(n)*dt1 
           f5(j) = fst(1)*dvt
           f6(j) = fst(2)*dvt
           f7(j) = fst(3)*dvt
         end do
       end if !(ifq > 0)
      end if !if(itied == 1)
!
!$omp atomic
          stif_max=max(stif_max,stf_maxi) 
!
          frwl6_l = zero
          call sum_6_float(1, nindex, f1, frwl6_l(1,1), 7)
          call sum_6_float(1, nindex, f2, frwl6_l(2,1), 7)
          call sum_6_float(1, nindex, f3, frwl6_l(3,1), 7)
          call sum_6_float(1, nindex, f4, frwl6_l(4,1), 7)
          call sum_6_float(1, nindex, f5, frwl6_l(5,1), 7)
          call sum_6_float(1, nindex, f6, frwl6_l(6,1), 7)
          call sum_6_float(1, nindex, f7, frwl6_l(7,1), 7)

!$OMP CRITICAL 
          do k = 1, 6
            frwl6(1:7,k) = frwl6(1:7,k)+frwl6_l(1:7,k)
          end do
!$OMP END CRITICAL 
       deallocate(f1)
       deallocate(f2)
       deallocate(f3)
       deallocate(f4)
       deallocate(f5)
       deallocate(f6)
       deallocate(f7)
       deallocate(stif)
       deallocate(fn)
       deallocate(vn)
       deallocate(stif_kt)
      end if !(nindex > 0)
!
      end subroutine rwall_fpen
     end module rwall_pen_mod
