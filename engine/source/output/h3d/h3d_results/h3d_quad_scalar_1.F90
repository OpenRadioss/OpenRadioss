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
      !||    h3d_quad_scalar_1_mod          ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||--- called by ------------------------------------------------------
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    h3d_quad_scalar                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||====================================================================
      module h3d_quad_scalar_1_mod
      contains
!! \brief fill the scalar values for the quad elements
      !||====================================================================
      !||    h3d_quad_scalar_1              ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||--- called by ------------------------------------------------------
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    h3d_quad_scalar                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_write_scalar               ../engine/source/output/h3d/h3d_results/h3d_write_scalar.F
      !||    initbuf                        ../engine/share/resol/initbuf.F
      !||    output_div_u                   ../engine/source/output/anim/generate/output_div_u.F
      !||    output_schlieren               ../engine/source/output/anim/generate/output_schlieren.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod           ../common_source/modules/ale/ale_connectivity_mod.F
      !||    alefvm_mod                     ../common_source/modules/ale/alefvm_mod.F
      !||    constant_mod                   ../common_source/modules/constant_mod.F
      !||    elbufdef_mod                   ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    initbuf_mod                    ../engine/share/resol/initbuf.F
      !||    matparam_def_mod               ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    multi_fvm_mod                  ../common_source/modules/ale/multi_fvm_mod.F90
      !||    names_and_titles_mod           ../common_source/modules/names_and_titles_mod.F
      !||    schlieren_mod                  ../engine/share/modules/schlieren_mod.F
      !||====================================================================
        subroutine h3d_quad_scalar_1(called_from_python, ng, &
        &  n0phas, nvphas,ngroup, n2d, numelq, nummat, numnod, nparg, npropm, npropmi, ispmd,&
        &  elbuf_tab   ,quad_scalar, quad_scalar_size, iparg        ,&
        &  ixq, nixq,         pm          ,&
        &  ehour     ,&
        &  ipm         ,&
        &  x         ,v         ,w           ,ale_connect      ,&
        &  id_elem   ,&
        &  is_written_quad,ipartq,layer_input , npart,&
        &  iuvar_input,h3d_part  ,keyword   ,&
        &  bufmat      ,multi_fvm ,&
        &  id          ,mat_param)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use initbuf_mod, only : initbuf
          use elbufdef_mod, only : elbuf_struct_, G_BUFEL_, L_BUFEL_, BUF_FAIL_, BUF_EOS_, BUF_MAT_
          use schlieren_mod, only : WA_L ! should be passed as an argument
          use multi_fvm_mod, only: multi_fvm_struct
          use ale_connectivity_mod, only: t_ale_connectivity
          use alefvm_mod , only:alefvm_param
          use names_and_titles_mod, only: ncharline100
          use matparam_def_mod , only : matparam_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                     implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     include
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, intent(in ):: called_from_python  !< flag to indicate if the function is called for /FUNCT_PYTHON option
          integer, intent(in) :: ng     !< group id
          integer, intent(in) :: nixq   !< size of the ixq array
          integer, intent(in) :: n0phas !< number of phases ?
          integer, intent(in) :: nvphas !< number of phases ?
          integer, intent(in) :: ngroup !< number of groups
          integer, intent(in) :: n2d     !< is 2D case?
          integer, intent(in) :: numelq  !< number of quad elements
          integer, intent(in) :: nummat !< number of materials
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: nparg !< size of the iparg array
          integer, intent(in) :: npropm !< size of the pm array
          integer, intent(in) :: npropmi !< size of the ipm array
          integer, intent(in) :: ispmd !< MPI rank of the processor
          integer, intent(in) :: id !< used for failure?
          integer, intent(in) :: quad_scalar_size !< size of the quad_scalar array
          integer, intent(in) :: npart
          my_real, intent(inout) :: quad_scalar(quad_scalar_size) !< results array containing the scalar values
          my_real, intent(inout) :: x(3, numnod) !< coordinates of the nodes
          my_real, intent(inout) :: v(3, numnod) !< velocity of the nodes
          my_real, intent(inout) :: w(3, numnod) !< angular velocity of the nodes
          my_real, intent(inout) :: ehour(numelq) !< ?
          my_real, intent(inout) :: pm(npropm, nummat) !< material properties
          integer, intent(inout) :: iparg(nparg, ngroup) !< integer values for the groups
          integer, intent(inout) :: ixq(nixq, numelq) !< quad connectivity
          integer, intent(inout) :: ipm(npropmi, nummat) !< material properties
          integer, intent(inout) :: id_elem(numelq) !< element ids
          integer, intent(inout) :: is_written_quad(numelq) !< flag to indicate if the value is written
          integer, intent(inout) :: ipartq(numelq) !< part ids
          integer, intent(inout) :: h3d_part(npart) !< h3d part ids ?
          integer, intent(inout) :: layer_input !< layer id
          integer, intent(inout) :: iuvar_input !< ?
          type(elbuf_struct_), dimension(ngroup), target :: elbuf_tab !< buffer for the elements
          character(len=ncharline100) :: keyword !< animation keyword for the requested scalar values
          type(multi_fvm_struct), intent(in) :: multi_fvm !< Finite volume method data
          type(t_ale_connectivity), intent(in) :: ale_connect !< ALE connectivity data
          my_real, target :: bufmat(*) !< additional buffer for material law (old buffer. new one is mat_param)
          type (matparam_struct_) ,dimension(nummat) ,intent(in) :: mat_param !< material buffer data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Local variables
! ----------------------------------------------------------------------------------------------------------------------
          my_real :: evar(mvsiz)
          my_real :: value(mvsiz)
          my_real :: ff0
          my_real :: gg0
          my_real :: hh0
          my_real :: ll0
          my_real :: mm0
          my_real :: nn0
          my_real :: mass(mvsiz)
          my_real :: pres(mvsiz)
          my_real :: off
          my_real :: p
          my_real :: vonm2
          my_real :: s1
          my_real :: s2
          my_real :: s3
          my_real :: fac
          my_real :: vfrac(mvsiz, 1:21)
          my_real :: s11
          my_real :: s22
          my_real :: s33
          my_real :: s4
          my_real :: s5
          my_real :: s6
          my_real :: crit
          my_real :: vel(0:3)
          my_real :: tmp(3, 4)
          my_real :: nx
          my_real :: ny
          my_real :: nz
          my_real :: cumul(3)
          my_real :: surf
          my_real :: vx
          my_real :: vy
          my_real :: vz
          my_real :: vol
          my_real :: mass0

          integer :: i
          integer :: ii
          integer :: nel
          integer :: nptr
          integer :: npts
          integer :: nptt
          integer :: nlay
          integer :: l
          integer :: ifail
          integer :: ilay
          integer :: ir
          integer :: is
          integer :: it
          integer :: mlw
          integer :: nuvar
          integer :: ius
          integer :: nfail
          integer :: n
          integer :: jturb
          integer :: mt
          integer :: ialel
          integer :: isubstack
          integer :: iadbuf
          integer :: nuparam
          integer :: imat
          integer :: ivisc
          integer :: ipos
          integer :: iu(4)
          integer :: ieos

          integer :: iok_part(mvsiz)
          integer :: jj(6)
          integer :: iuvar
          integer :: is_written_value(mvsiz)
          integer :: nfrac
          integer :: kface
          integer :: nb_face
          integer :: iv
          integer :: isubmat
          integer :: is_euler
          integer :: is_ale
          integer :: iad2
          integer :: nvareos
          integer :: ntillotson
          integer :: imat_tillotson

          integer :: nft
          integer :: iad
          integer :: ity
          integer :: lft
          integer :: llt
          integer :: npt
          integer :: jale
          integer :: ismstr
          integer :: jeul
          integer :: jthe
          integer :: jlag
          integer :: jmult
          integer :: jhbe
          integer :: jivf
          integer :: nvaux
          integer :: jpor
          integer :: jcvt
          integer :: jclose
          integer :: jplasol
          integer :: irep
          integer :: iint
          integer :: igtyp
          integer :: israt
          integer :: isrot
          integer :: icsen
          integer :: isorth
          integer :: isorthg
          integer :: ifailure
          integer :: jsms
          integer :: mid !< material identifier
          integer :: ierr
          character*5 :: buff
          type(G_BUFEL_), pointer :: gbuf
          type(L_BUFEL_), pointer :: lbuf
          type(L_BUFEL_), pointer :: lbuf1
          type(L_BUFEL_), pointer :: lbuf2
          type(BUF_FAIL_), pointer :: fbuf
          type(BUF_EOS_), pointer :: ebuf
          my_real, dimension(:), pointer :: uvar
          type(BUF_MAT_), pointer :: mbuf
          my_real, dimension(:), pointer :: uparam
          logical :: detected
          my_real, parameter :: pi_ = 3.141592653589793238462643
          my_real :: vi(21) !< submaterial volumes at reference densities (max submat : 21)
          my_real :: v0i(21) !< submaterial volumes at reference densities (max submat : 21)
          my_real :: v0g !< global volume at reference density (mixture)
          my_real :: RHO0i(21) !< submaterial initial mass densities (max submat : 21)
          my_real :: RHOi(21) !< submaterial  mass densities (max submat : 21)
          my_real :: RHO0g !< global initial mass density (mixture)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     body
! ----------------------------------------------------------------------------------------------------------------------
          ilay = layer_input
          iuvar = iuvar_input
          call initbuf(iparg    ,ng      ,&
          &mlw     ,nel     ,nft     ,iad     ,ity     ,&
          &npt     ,jale    ,ismstr  ,jeul    ,jturb   ,&
          &jthe    ,jlag    ,jmult   ,jhbe    ,jivf    ,&
          &nvaux   ,jpor    ,jcvt    ,jclose  ,jplasol ,&
          &irep    ,iint    ,igtyp   ,israt   ,isrot   ,&
          &icsen   ,isorth  ,isorthg ,ifailure,jsms    )
          if(mlw /= 13) then
            nft   =iparg(3,ng)
            iad   =iparg(4,ng)
            isubstack = iparg(71,ng)
            ivisc = iparg(61,ng)
            iok_part(1:nel) = 0
            lft=1
            llt=nel
            is_euler=iparg(11,ng)
            is_ale=iparg(7,ng)
            do i=1,6
              jj(i) = nel*(i-1)
            enddo
            do i=1,nel
              value(i) = zero
              is_written_value(i) = 0
            enddo
!-----------------------------------------------
!           quad
!-----------------------------------------------
            if (ity == 2) then

              gbuf => elbuf_tab(ng)%gbuf
              lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
              uvar => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var
              ebuf => elbuf_tab(ng)%bufly(1)%eos(1,1,1)
              jale=(iparg(7,ng)+iparg(11,ng))
              jturb=iparg(12,ng)*jale
              nptr  = elbuf_tab(ng)%nptr
              npts  = elbuf_tab(ng)%npts
              nptt  = elbuf_tab(ng)%nptt
              nlay  = elbuf_tab(ng)%nlay
              nuvar = elbuf_tab(ng)%bufly(1)%nvar_mat
              nvareos = elbuf_tab(ng)%bufly(1)%nvar_eos
              if(.not. called_from_python) then
                do  i=1,nel
                  id_elem(nft+i) = ixq(nixq,nft+i)
                  if( h3d_part(ipartq(nft+i)) == 1) iok_part(i) = 1
                enddo
              endif
              do i=1,nel
                value(i) = zero
              enddo
              if(called_from_python) then
                quad_scalar(1:mvsiz) = 0
              else
                do i = 1,nel
                  quad_scalar(nft+i) = zero   ! default = zero in all cases !
                enddo
              endif
              iuvar = iuvar_input
!-----------------------------------------------
! mass computation
!-----------------------------------------------
              if (keyword == 'MASS') then
                ialel=(iparg(7,ng)+iparg(11,ng))
                do i=1,nel
                  n = i + nft
                  if(ialel==0)then
                    mt=ixq(1,n)
                    mass(i)=pm(89,mt)*gbuf%vol(i)
                  else
                    off = min(gbuf%off(i),one)
                    mass(i)=gbuf%rho(i)*gbuf%vol(i)*off
                  endif
                enddo
              endif
!--------------------------------------------------
              if (keyword == 'MASS') then   ! mass
!--------------------------------------------------
                do i=1,nel
                  value(i) = mass(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif (keyword == 'EPSP')then  ! epsp
!--------------------------------------------------
                if (mlw == 10 .or. mlw == 21) then
                  do i=1,nel
                    value(i) = lbuf%epsq(i)
                    is_written_value(i) = 1
                  enddo
                elseif (mlw == 24) then   ! et autres a ajouter
                  do i=1,nel
                    value(i) = lbuf%vk(i)
                    is_written_value(i) = 1
                  enddo
                elseif (mlw == 6 .or. mlw == 17 .or. mlw == 11) then   ! et autres a ajouter
                  do i=1,nel
                    value(i) =  lbuf%rk(i)
                    is_written_value(i) = 1
                  enddo
                elseif (mlw >=28 .and. mlw /= 49 .and. nuvar > 0) then
                  do i=1,nel
                    value(i) =  uvar(i)
                    is_written_value(i) = 1
                  enddo
                else
                  if (gbuf%g_pla > 0) then
                    do i=1,nel
                      value(i) =  gbuf%pla(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif (keyword == 'TSAIWU' .and. gbuf%g_tsaiwu > 0) then
!--------------------------------------------------
                do i=lft,llt
                  value(i) = zero
                enddo
                if (elbuf_tab(ng)%bufly(1)%l_tsaiwu > 0) then
                  do is=1,npts
                    do it=1,nptt
                      do ir=1,nptr
                        lbuf=>elbuf_tab(ng)%bufly(1)%lbuf(ir,is,it)
                        do i=lft,llt
                          value(i) = value(i) + lbuf%tsaiwu(i)/(npts*nptt*nptr)
                          is_written_value(i) = 1
                        enddo
                      enddo
                    enddo
                  enddo
                endif
!--------------------------------------------------
              elseif (keyword == 'FAILURE') then
!--------------------------------------------------
                do i=lft,llt
                  value(i) = zero
                enddo
                nfail = elbuf_tab(ng)%bufly(1)%nfail
                do i = lft,llt
                  do is=1,npts
                    do it=1,nptt
                      do ir=1,nptr
                        fbuf => elbuf_tab(ng)%bufly(1)%fail(ir,is,it)
                        do ifail=1,nfail
                          if (fbuf%floc(ifail)%idfail == id) then
                            value(i) = value(i) + fbuf%floc(ifail)%dammx(i)/(npts*nptt*nptr)
                            is_written_value(i) = 1
                          endif
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
!--------------------------------------------------
              elseif(keyword == 'DENS')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    value(i) = multi_fvm%rho(i + nft)
                    is_written_value(i) = 1
                  enddo
                else
                  do i=1,nel
                    value(i) =  gbuf%rho(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'TEMP')then
!--------------------------------------------------
                if(gbuf%g_temp > 0)then
                  do i=1,nel
                    value(i) = gbuf%temp(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'P')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    value(i) = multi_fvm%pres(i + nft)
                    is_written_value(i) = 1
                  enddo
                else
                  do i=1,nel
                    p = - (gbuf%sig(jj(1) + i)&
                    &+ gbuf%sig(jj(2) + i)&
                    &+ gbuf%sig(jj(3) + i))*third
                    value(i) = p
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'VONM')then
!--------------------------------------------------
                do i=1,nel
                  p = - (gbuf%sig(jj(1) + i)&
                  &+ gbuf%sig(jj(2) + i)&
                  &+ gbuf%sig(jj(3) + i) )*third
                  s1 = gbuf%sig(jj(1) + i) + p
                  s2 = gbuf%sig(jj(2) + i) + p
                  s3 = gbuf%sig(jj(3) + i) + p
                  vonm2 = three*(gbuf%sig(jj(4) + i)**2&
                  &+ half*(s1**2+s2**2+s3**2) )
                  value(i) = sqrt(vonm2)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'K')then
!--------------------------------------------------
                if(jturb/=0)then
                  do i=1,nel
                    value(i) = gbuf%rk(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'TVIS')then
!--------------------------------------------------
                if (mlw == 6 .or. mlw == 17.and.jturb/=0) then
                  do i=1,nel
                    mt=ixq(1,i+nft)
                    value(i)=pm(81,mt)*gbuf%rk(i)**2/&
                    &max(em15,gbuf%re(i))
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 46 .or. mlw == 47)then
                  do i=1,nel
                    value(i)= uvar(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'VORTX')then
!--------------------------------------------------
                if (mlw == 6 .or. mlw == 17) then
                  do i=1,nel
                    value(i) = lbuf%vk(i)
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 46 .or. mlw == 47)then
                  do i=1,nel
                    value(i) = uvar(nel+i)
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 151)then
                  !ity = iparg(5, ng)
                  nb_face = 4
                  if (ity == 7)nb_face=3
                  do i=1,nel
                    ii = i + nft
                    iad2 = ale_connect%ee_connect%iad_connect(ii)
                    cumul(1:3)=zero
                    do kface = 1, nb_face
                      iv = ale_connect%ee_connect%connected(iad2+kface-1)
                      nx = zero !multi_fvm%face_data%normal(1, kface, ii)
                      ny = multi_fvm%face_data%normal(2, kface, ii)
                      nz = multi_fvm%face_data%normal(3, kface, ii)
                      surf = multi_fvm%face_data%surf(kface, ii)
                      vx = zero !multi_fvm%vel(1, ii)
                      vy = multi_fvm%vel(2, ii)
                      vz = multi_fvm%vel(3, ii)
                      if(iv /=0)then
                        vx = zero ! half(vx + multi_fvm%vel(1, iv))
                        vy = half*(vy + multi_fvm%vel(2, iv))
                        vz = half*(vz + multi_fvm%vel(3, iv))
                      endif
                      cumul(1)=cumul(1)+surf*(ny*vz-nz*vy)
                    enddo
                    cumul(1)=cumul(1)/gbuf%vol(i)
                    value(i) = cumul(1)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'DAM1')then
!--------------------------------------------------
                if(mlw == 24)then
                  do i=1,nel
                    value(i) = lbuf%dam(jj(1) + i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'DAM2')then
!--------------------------------------------------
                if(mlw == 24)then
                  do i=1,nel
                    value(i) = lbuf%dam(jj(2) + i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'DAM3')then
!--------------------------------------------------
                if(mlw == 24)then
                  do i=1,nel
                    value(i) = lbuf%dam(jj(3) + i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'SIGX')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = gbuf%sig(jj(1) + i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGY')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = gbuf%sig(jj(2) + i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGZ')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = gbuf%sig(jj(3) + i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGXY')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = gbuf%sig(jj(4) + i)
                  is_written_value(i) = 1
                enddo
!----
!--------------------------------------------------
              elseif(keyword == 'USER')then
!--------------------------------------------------
                if(iuvar > 0 .and. (mlw == 28.or.mlw == 29.or.mlw == 30.or.&
                &mlw == 31.or.mlw == 52.or.mlw == 79))then
                  do i=1,nel
                    n = i + nft
                    mt = ixq(1,n)
                    nuvar = ipm(8,mt)
                    if (iuvar <= nuvar) then
                      value(i) = uvar(iuvar*nel + i)
                      is_written_value(i) = 1
                    endif
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'HOURGLASS')then
!--------------------------------------------------
                do i=1,nel
                  n = i + nft
                  value(i) = ehour(n)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'VFRAC1')then
!--------------------------------------------------
                if(mlw == 20)then
                  do  i=1,nel
                    value(i) =&
                    &elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)%vol(i)&
                    &/ elbuf_tab(ng)%gbuf%vol(i)
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 37)then
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  do  i=1,nel
                    value(i) = mbuf%var(i+3*nel)
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 51)then
                  ius=n0phas
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  do  i=1,nel
                    value(i) = mbuf%var(i+ius*nel)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'VFRAC2')then
!--------------------------------------------------
                if(mlw == 20)then
                  do  i=1,nel
                    value(i) =&
                    &elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)%vol(i)&
                    &/ elbuf_tab(ng)%gbuf%vol(i)
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 37)then
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  do  i=1,nel
                    value(i) = mbuf%var(i+4*nel)
                    is_written_value(i) = 1
                  enddo
                elseif(mlw == 51)then
                  ius=n0phas+nvphas
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  do  i=1,nel
                    value(i) = mbuf%var(i+ius*nel)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'VFRAC3')then
!--------------------------------------------------
                if(mlw == 51)then
                  ius=n0phas+2*nvphas
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  do  i=1,nel
                    value(i) = mbuf%var(i+ius*nel)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'VFRAC4')then
!--------------------------------------------------
                if(mlw == 51)then
                  ius=n0phas+3*nvphas
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  do  i=1,nel
                    value(i) = mbuf%var(i+ius*nel)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword(1:9) == 'M151VFRAC') then
!--------------------------------------------------
                if (mlw == 151) then
                  read(keyword, '(A9,I10)') buff, imat
                  if (imat > 0 .and. imat <= nlay) then
                    gbuf => elbuf_tab(ng)%gbuf
                    lbuf => elbuf_tab(ng)%bufly(imat)%lbuf(1,1,1)
                    do i=1,nel
                      value(i) = lbuf%vol(i) / gbuf%vol(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword(1:8) == 'M151ENER') then
!--------------------------------------------------
                if (mlw == 151) then
                  read(keyword, '(A8,I10)') buff, imat
                  if (imat > 0 .and. imat <= nlay) then
                    do i=1,nel
                      value(i) = multi_fvm%phase_eint(imat, i + nft) /&
                      &multi_fvm%phase_rho(imat, i + nft)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword(1:8) == 'M151PRES') then
!--------------------------------------------------
                if (mlw == 151) then
                  read(keyword, '(A8,I10)') buff, imat
                  if (imat > 0 .and. imat <= nlay) then
                    do i=1,nel
                      value(i) = multi_fvm%phase_pres(imat, i + nft)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword(1:8) == 'M151DENS') then
!--------------------------------------------------
                if (mlw == 151) then
                  read(keyword, '(A8,I10)') buff, imat
                  if (imat > 0 .and. imat <= nlay) then
                    do i=1,nel
                      value(i) = multi_fvm%phase_rho(imat, i + nft)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'BFRAC')then
!--------------------------------------------------
                !burn fraction explosive eos
                if(gbuf%g_bfrac > 0) then
                  if (mlw==151)then
                    do i=1,nel
                      value(i)=-ep30
                      do ilay=1,nlay
                        value(i) =  max(value(i),multi_fvm%bfrac(ilay,i+nft))
                        is_written_value(i) = 1
                      enddo
                    enddo
                  else
                    value(1:nel) = gbuf%bfrac(1:nel)
                    is_written_value(1:nel) = 1
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'SSP')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    value(i) = multi_fvm%sound_speed(i + nft)
                    is_written_value(i) = 1
                  enddo
                else
                  l = elbuf_tab(ng)%bufly(1)%l_ssp
                  if(elbuf_tab(ng)%bufly(1)%l_ssp /= 0)then
                    lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    do  i=1,nel
                      value(i) = lbuf%ssp(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'DOMAIN') then
!--------------------------------------------------
                do i=1,nel
                  value(i) = ispmd
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SCHLIEREN') then
!--------------------------------------------------
                ialel=iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0)then
                  call output_schlieren(&
                  &evar  , ixq  , x         ,&
                  &iparg , wa_l , elbuf_tab , ale_connect , gbuf%vol,&
                  &ng    , nixq      , ity)
                  do  i=1,nel
                    value(i) = evar(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif (keyword == 'SIGEQ') then
!--------------------------------------------------
                !  equivalent stress - other then von mises
                if (gbuf%g_seq > 0) then
                  imat = ixq(1,nft+1)
                  iadbuf = ipm(7,imat)
                  nuparam= ipm(9,imat)
                  uparam => bufmat(iadbuf:iadbuf+nuparam)
                  lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
!---
                  if (mlw == 72) then
!                 (ilaw = 74) -- hill mmc (anisotropic)
                    ff0 = uparam(11)
                    gg0 = uparam(12)
                    hh0 = uparam(13)
                    ll0 = uparam(14)
                    mm0 = uparam(15)
                    nn0 = uparam(16)
                    do i=1,nel
                      s11 = gbuf%sig(jj(1) + i)
                      s22 = gbuf%sig(jj(2) + i)
                      s33 = gbuf%sig(jj(3) + i)
                      s4  = gbuf%sig(jj(4) + i)
                      s5  = gbuf%sig(jj(5) + i)
                      s6  = gbuf%sig(jj(6) + i)
                      if (ivisc > 0) then
                        s11 = s11 + lbuf%visc(jj(1) + i)
                        s22 = s22 + lbuf%visc(jj(2) + i)
                        s33 = s33 + lbuf%visc(jj(3) + i)
                        s4  = s4  + lbuf%visc(jj(4) + i)
                        s5  = s5  + lbuf%visc(jj(5) + i)
                        s6  = s6  + lbuf%visc(jj(6) + i)
                      endif
                      p = - (s11 + s22 + s33) * third
                      s1 = s11 + p
                      s2 = s22 + p
                      s3 = s33 + p
!
                      crit = ff0*(s2 - s3)**2&
                      &+ gg0*(s3 - s1)**2&
                      &+ hh0*(s1 - s2)**2&
                      &+ two*ll0*s5**2&
                      &+ two*mm0*s6**2&
                      &+ two*nn0*s4**2
!
                      value(i) = sqrt(crit)
                      is_written_value(i) = 1
                    enddo ! do i=1,nel
                  elseif (mlw == 74) then
!                 (ilaw = 74) -- thermal hill orthotropic 3d material
                    ff0 = uparam(7)
                    gg0 = uparam(8)
                    hh0 = uparam(9)
                    ll0 = uparam(10)
                    mm0 = uparam(11)
                    nn0 = uparam(12)
                    do i=1,nel
                      s11 = gbuf%sig(jj(1) + i)
                      s22 = gbuf%sig(jj(2) + i)
                      s33 = gbuf%sig(jj(3) + i)
                      s4  = gbuf%sig(jj(4) + i)
                      s5  = gbuf%sig(jj(5) + i)
                      s6  = gbuf%sig(jj(6) + i)
                      if (ivisc > 0) then
                        s11 = s11 + lbuf%visc(jj(1) + i)
                        s22 = s22 + lbuf%visc(jj(2) + i)
                        s33 = s33 + lbuf%visc(jj(3) + i)
                        s4  = s4  + lbuf%visc(jj(4) + i)
                        s5  = s5  + lbuf%visc(jj(5) + i)
                        s6  = s6  + lbuf%visc(jj(6) + i)
                      endif
                      p = - (s11 + s22 + s33) * third
                      s1 = s11 + p
                      s2 = s22 + p
                      s3 = s33 + p
!
                      crit = ff0*(s2 - s3)**2&
                      &+ gg0*(s3 - s1)**2&
                      &+ hh0*(s1 - s2)**2&
                      &+ two*ll0*s5**2&
                      &+ two*mm0*s6**2&
                      &+ two*nn0*s4**2
!
                      value(i) = sqrt(crit)
                      is_written_value(i) = 1
                    enddo ! do i=1,nel
                  endif ! if (mlw == 72)
!---
                else  ! von mises
                  do i=1,nel
                    p = - (gbuf%sig(jj(1) + i)&
                    &+  gbuf%sig(jj(2) + i)&
                    &+  gbuf%sig(jj(3) + i))*third
                    s1 = gbuf%sig(jj(1) + i) + p
                    s2 = gbuf%sig(jj(2) + i) + p
                    s3 = gbuf%sig(jj(3) + i) + p
                    vonm2 = three*(gbuf%sig(jj(4) + i)**2&
                    &+ half*(s1**2+s2**2+s3**2))
                    value(i) = sqrt(vonm2)
                    is_written_value(i) = 1
                  enddo
                endif ! if (gbuf%g_seq > 0)
!--------------------------------------------------
              elseif(keyword == 'BULK')then
!--------------------------------------------------
                if (gbuf%g_qvis > 0) then
                  do i=1,nel
                    value(i) = gbuf%qvis(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif (keyword == 'TDET') then  !  /anim/elem/tdet
!--------------------------------------------------
                if (gbuf%g_tb > 0) then
                  do i=1,nel
                    value(i) = -gbuf%tb(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/DENS1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = lbuf%rho(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/DENS2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = lbuf%rho(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/ENER1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = lbuf%eint(i)/max(em30,lbuf%rho(i))
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/ENER2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = lbuf%eint(i)/max(em30,lbuf%rho(i))
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/TEMP1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = lbuf%temp(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/TEMP2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = lbuf%temp(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/P1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = - (lbuf%sig(jj(1) + i) +&
                    &lbuf%sig(jj(2) + i) +&
                    &lbuf%sig(jj(3) + i))*third
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/P2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = - (lbuf%sig(jj(1) + i) +&
                    &lbuf%sig(jj(2) + i) +&
                    &lbuf%sig(jj(3) + i))*third
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/EPSP1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = lbuf%pla(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/EPSP1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = lbuf%pla(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/SSP1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = lbuf%ssp(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/SSP2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = lbuf%ssp(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/VOLUM1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                lbuf1  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                if(ialel /= 0 .and. mlw == 20)then
                  if(ialel ==0)then
                    mt = ixq(1,nft+1)
                    do i=1,nel
                      value(i) = pm(1,mt)*lbuf1%vol(i)*two*pi_
                      if(lbuf1%rho(i)>zero)value(i) = value(i)/lbuf1%rho(i)
                      is_written_value(i) = 1
                    enddo
                  else
                    do i=1,nel
                      value(i) = lbuf1%vol(i)*two*pi_
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/VOLUM2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(nlay > 1)then !otherwise memory is not allocated
                  lbuf2  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                  if(ialel /= 0 .and. mlw == 20)then
                    if(ialel ==0)then
                      mt = ixq(1,nft+1)
                      do i=1,nel
                        value(i) = pm(1,mt)*lbuf2%vol(i)*two*pi_
                        if(lbuf2%rho(i)>zero)value(i) = value(i)/lbuf2%rho(i)
                        is_written_value(i) = 1
                      enddo
                    else
                      do i=1,nel
                        value(i) = lbuf2%vol(i)*two*pi_
                        is_written_value(i) = 1
                      enddo
                    endif
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'VOLU' .or. keyword == 'VOL')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel ==0)then
                  mt = ixq(1,nft+1)
                  do i=1,nel
                    value(i) = pm(1,mt)*gbuf%vol(i)*two*pi_
                    if(gbuf%rho(i)>zero)value(i) = value(i)/gbuf%rho(i)
                    is_written_value(i) = 1
                  enddo
                else
                  do i=1,nel
                    value(i) = gbuf%vol(i)*two*pi_
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/QVIS1')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    value(i) = lbuf%qvis(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'LAW20/QVIS2')then
!--------------------------------------------------
                ialel = iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0 .and. mlw == 20)then
                  do i=1,nel
                    lbuf  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    value(i) = lbuf%qvis(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'DT')then
!--------------------------------------------------
                if(gbuf%g_dt>0)then
                  do i=1,nel
                    value(i) = gbuf%dt(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif (keyword == 'EINTM' .or. keyword == 'ENER')then
!--------------------------------------------------
                !lag: gbuf%vol = v0,    gbuf%eint=rho0.e
                if (mlw == 151) then
                  do i = 1, nel
                    value(i) = multi_fvm%eint(i + nft) / multi_fvm%rho(i+nft)
                    is_written_value(i) = 1
                  enddo
                else
                  ialel=iparg(7,ng)+iparg(11,ng)
                  if(ialel == 0)then
                    do i=1,nel
                      n = i + nft
                      mt=ixq(1,n)
                      value(i) = gbuf%eint(i)/max(em20,pm(89,mt))
                      is_written_value(i) = 1
                    enddo
                  else
                    do i=1,nel
                      value(i) = gbuf%eint(i)/max(em20,gbuf%rho(i))
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif (keyword == 'EINTV')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    value(i) = multi_fvm%eint(i + nft)
                    is_written_value(i) = 1
                  enddo
                else
                  ialel=iparg(7,ng)+iparg(11,ng)
                  if(ialel == 0)then
                    do i=1,nel
                      n = i + nft
                      mt=ixq(1,n)
                      value(i) = gbuf%eint(i)/max(em20,pm(89,mt))*gbuf%rho(i)
                      is_written_value(i) = 1
                    enddo
                  else
                    do i=1,nel
                      value(i) = gbuf%eint(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif (keyword == 'EINT')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    value(i) = multi_fvm%eint(i + nft) * gbuf%vol(i)
                    is_written_value(i) = 1
                  enddo
                else
                  ialel=iparg(7,ng)+iparg(11,ng)
                  if(ialel == 0)then
                    do i=1,nel
                      n = i + nft
                      mt=ixq(1,n)
                      vol=gbuf%vol(i)*pm(89,mt)/gbuf%rho(i)
                      value(i) = gbuf%eint(i)/pm(89,mt)*gbuf%rho(i)*vol
                      is_written_value(i) = 1
                    enddo
                  else
                    do i=1,nel
                      value(i) = gbuf%eint(i)*gbuf%vol(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif (keyword(1:4) == 'ENTH')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    pres(i) = multi_fvm%pres(i + nft)
                  enddo
                else
                  do i=1,nel
                    pres(i) = - (gbuf%sig(jj(1) + i)+ gbuf%sig(jj(2) + i) + gbuf%sig(jj(3) + i))*third
                  enddo
                endif
                !gbuf%eint is rho.e
!--------------------------------------------------
                if(keyword == 'ENTH')then
                  if (mlw == 151) then
                    do i = 1, nel
                      value(i) = multi_fvm%eint(i + nft) / multi_fvm%rho(i + nft) + pres(i)*gbuf%vol(i)
                      is_written_value(i) = 1
                    enddo
                  else
                    ialel=iparg(7,ng)+iparg(11,ng)
                    if(ialel == 0)then
                      do i=1,nel
                        n=i+nft
                        mt=ixq(1,n)
                        mass0=gbuf%vol(i)*pm(89,mt)
                        vol=mass0/max(em20,gbuf%rho(i))
                        value(i) = gbuf%eint(i)/max(em20,pm(89,mt)) + pres(i)*vol
                        is_written_value(i) = 1
                      enddo
                    else
                      do i=1,nel
                        value(i) = gbuf%eint(i)/gbuf%rho(i) + pres(i)*gbuf%vol(i)
                        is_written_value(i) = 1
                      enddo
                    endif!ialel
                  endif!mlw
!--------------------------------------------------
                elseif(keyword == 'ENTHV')then
                  if (mlw == 151) then
                    do i = 1, nel
                      value(i) = multi_fvm%eint(i + nft) / multi_fvm%rho(i + nft)/gbuf%vol(i) + pres(i) !
                      is_written_value(i) = 1
                    enddo
                  else
                    ialel=iparg(7,ng)+iparg(11,ng)
                    if(ialel == 0)then
                      do i=1,nel
                        n=i+nft
                        mt=ixq(1,n)
                        mass0=gbuf%vol(i)*pm(89,mt)
                        vol=mass0/max(em20,gbuf%rho(i))
                        value(i) = gbuf%eint(i)/max(em20,pm(89,mt))/vol + pres(i)
                        is_written_value(i) = 1
                      enddo
                    else
                      do i=1,nel
                        value(i) = gbuf%eint(i)/gbuf%vol(i)/gbuf%rho(i) + pres(i)
                        is_written_value(i) = 1
                      enddo
                    endif!ialel
                  endif!mlw
!--------------------------------------------------
                elseif(keyword == 'ENTHM')then
                  if (mlw == 151) then
                    do i = 1, nel
                      mass(i) = multi_fvm%rho(i + nft)*gbuf%vol(i)
                      value(i) = (multi_fvm%eint(i + nft) / multi_fvm%rho(i + nft) + pres(i)*gbuf%vol(i))/mass(i) !
                      is_written_value(i) = 1
                    enddo
                  else
                    ialel=iparg(7,ng)+iparg(11,ng)

                    if(ialel == 0)then
                      do i=1,nel
                        n=i+nft
                        mt=ixq(1,n)
                        mass0=gbuf%vol(i)*pm(89,mt)
                        vol=mass0/max(em20,gbuf%rho(i))
                        mass(i)=mass0
                        value(i) = (gbuf%eint(i)/max(em20,pm(89,mt)) + pres(i)*vol)/mass(i)
                        is_written_value(i) = 1
                      enddo
                    else
                      do i=1,nel
                        mass(i)=gbuf%rho(i)*gbuf%vol(i)
                        value(i) = (gbuf%eint(i)/gbuf%rho(i) + pres(i)*gbuf%vol(i))/mass(i)
                        is_written_value(i) = 1
                      enddo
                    endif!ialel
                  endif!mlw
                endif!keyword subcase
!--------------------------------------------------
              elseif(keyword == 'OFF')then
!--------------------------------------------------
                do i=1,nel
                  if (gbuf%g_off > 0) then
                    if(gbuf%off(i) > one) then
                      value(i) = gbuf%off(i) - one
                    elseif((gbuf%off(i) >= zero .and. gbuf%off(i) <= one)) then
                      value(i) = gbuf%off(i)
                    else
                      value(i) = -one
                    endif
                  endif
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'MACH')then
!--------------------------------------------------
                if (mlw == 151) then
                  do i = 1, nel
                    vel(1) = multi_fvm%vel(1, i + nft)
                    vel(2) = multi_fvm%vel(2, i + nft)
                    vel(3) = multi_fvm%vel(3, i + nft)
                    vel(0) = sqrt(vel(1)*vel(1)+vel(2)*vel(2)+vel(3)*vel(3))
                    value(i) = vel(0)/multi_fvm%sound_speed(i + nft)
                    is_written_value(i) = 1
                  enddo
                elseif(alefvm_param%isolver>1)then
                  l = elbuf_tab(ng)%bufly(1)%l_ssp
                  if(elbuf_tab(ng)%bufly(1)%l_ssp /= 0)then
                    lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    do  i=1,nel
                      vel(1) = gbuf%mom(jj(1) + i) / gbuf%rho(i)
                      vel(2) = gbuf%mom(jj(2) + i) / gbuf%rho(i)
                      vel(3) = gbuf%mom(jj(3) + i) / gbuf%rho(i)
                      vel(0) = sqrt(vel(1)*vel(1)+vel(2)*vel(2)+vel(3)*vel(3))
                      value(i) = vel(0)/lbuf%ssp(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                else
                  l = elbuf_tab(ng)%bufly(1)%l_ssp
                  if(elbuf_tab(ng)%bufly(1)%l_ssp /= 0)then
                    lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    if(is_ale /= 0)then
                      !ale
                      do  i=1,nel
                        tmp(1,1:4)=v(1,ixq(2:5,i+nft))-w(1,ixq(2:5,i+nft))
                        tmp(2,1:4)=v(2,ixq(2:5,i+nft))-w(2,ixq(2:5,i+nft))
                        tmp(3,1:4)=v(3,ixq(2:5,i+nft))-w(3,ixq(2:5,i+nft))
                        vel(1) = sum(tmp(1,1:4))*fourth
                        vel(2) = sum(tmp(2,1:4))*fourth
                        vel(3) = sum(tmp(3,1:4))*fourth
                        value(i) = sqrt(vel(1)*vel(1)+vel(2)*vel(2)+vel(3)*vel(3))/lbuf%ssp(i)
                        is_written_value(i) = 1
                      enddo
                    else
                      !euler and lagrange
                      do  i=1,nel
                        tmp(1,1:4)=v(1,ixq(2:5,i+nft))
                        tmp(2,1:4)=v(2,ixq(2:5,i+nft))
                        tmp(3,1:4)=v(3,ixq(2:5,i+nft))
                        vel(1) = sum(tmp(1,1:4))*fourth
                        vel(2) = sum(tmp(2,1:4))*fourth
                        vel(3) = sum(tmp(3,1:4))*fourth
                        value(i) = sqrt(vel(1)*vel(1)+vel(2)*vel(2)+vel(3)*vel(3))/lbuf%ssp(i)
                        is_written_value(i) = 1
                      enddo
                    endif
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'COLOR')then
!--------------------------------------------------
                gbuf => elbuf_tab(ng)%gbuf
                if (mlw == 151) then
                  nfrac=multi_fvm%nbmat
                  do imat=1,nfrac
                    lbuf => elbuf_tab(ng)%bufly(imat)%lbuf(1,1,1)
                    do i=1,nel
                      vfrac(i,imat) = lbuf%vol(i) / gbuf%vol(i)
                    enddo
                  enddo
                elseif(mlw == 20)then
                  nfrac=2
                  do i=1,nel
                    vfrac(i,1) = elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)%vol(i) / gbuf%vol(i)
                    vfrac(i,2) = elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)%vol(i) / gbuf%vol(i)
                  enddo
                elseif(mlw == 37)then
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  nfrac=2
                  do  i=1,nel
                    vfrac(i,1) = mbuf%var(i+3*nel)
                    vfrac(i,2) = mbuf%var(i+4*nel)
                  enddo
                elseif(mlw == 51)then
                  !get uparam
                  imat   = ixq(1,nft+1)
                  iadbuf = ipm(7,imat)
                  nuparam= ipm(9,imat)
                  uparam => bufmat(iadbuf:iadbuf+nuparam)
                  !bijective order           !indexes
                  isubmat = nint(uparam(276+1));   iu(1)=n0phas+(isubmat-1)*nvphas
                  isubmat = nint(uparam(276+2));   iu(2)=n0phas+(isubmat-1)*nvphas
                  isubmat = nint(uparam(276+3));   iu(3)=n0phas+(isubmat-1)*nvphas
                  isubmat = nint(uparam(276+4));   iu(4)=n0phas+(isubmat-1)*nvphas
                  mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                  nfrac=4
                  do  i=1,nel
                    vfrac(i,1) = mbuf%var(i+iu(1)*nel)
                    vfrac(i,2) = mbuf%var(i+iu(2)*nel)
                    vfrac(i,3) = mbuf%var(i+iu(3)*nel)
                    vfrac(i,4) = mbuf%var(i+iu(4)*nel)
                  enddo
                else
                  nfrac=0
                  !vfrac(1:nel,1:21)=zero
                endif

                if(nfrac>0)then
                  do i=1,nel
                    value(i)=zero
                    do imat=1,nfrac
                      value(i) = value(i) + vfrac(i,imat)*imat
                    enddo
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'GROUP')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = ng
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'INTERNAL.ID')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = i+nft
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'LOCAL.ID')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = i
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'TILLOTSON') then
!--------------------------------------------------
                mt = ixq(1,nft+1)
                if (mlw == 151) then
                  !count number of submaterial based on /eos/tillotson (ieos=3)
                  ntillotson = 0
                  do imat=1,nlay
                    ieos =  ipm(4, MAT_PARAM(mt)%multimat%mid(imat) )
                    if(ieos == 3)then
                      ntillotson = ntillotson + 1
                      imat_tillotson = imat
                    endif
                  enddo
                  !several tillotson eos   value= sum ( region_i*10**(i-1),  i=1,imat)
                  if(ntillotson > 1)then
                    fac=one
                    do imat=1,nlay
                      ieos =  ipm(4, MAT_PARAM(mt)%multimat%mid(imat) )
                      if(ieos == 3)then
                        ebuf => elbuf_tab(ng)%bufly(imat)%eos(1,1,1)
                        nvareos = elbuf_tab(ng)%bufly(imat)%nvar_eos
                        do i=1,nel
                          value(i) = value(i) + ebuf%var(i) * fac
                          is_written_value(i) = 1
                        enddo
                      endif
                      fac=fac*ten
                    enddo
                    !single tillotson eos   value=  region_i
                  elseif(ntillotson == 1)then
                    ebuf => elbuf_tab(ng)%bufly(imat_tillotson)%eos(1,1,1)
                    nvareos = elbuf_tab(ng)%bufly(imat_tillotson)%nvar_eos
                    do i=1,nel
                      value(i) = ebuf%var(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                else
                  !monomaterial law
                  ieos = ipm(4,mt)
                  if(ieos == 3)then
                    ebuf => elbuf_tab(ng)%bufly(1)%eos(1,1,1)
                    nvareos = elbuf_tab(ng)%bufly(1)%nvar_eos
                    do  i=1,nel
                      value(i) = ebuf%var(i)
                      is_written_value(i) = 1
                    enddo
                  endif
                endif
!--------------------------------------------------
              elseif(keyword == 'DIV(U)') then
!--------------------------------------------------
                ialel=iparg(7,ng)+iparg(11,ng)
                if(ialel /= 0)then
                  call output_div_u(&
                  &evar   ,ixq  ,x      ,v    , iparg , elbuf_tab , ng    ,nixq , 2,&
                  &numelq ,nel  ,numnod ,nparg, ngroup, n2d , nft)
                  do i=1,nel
                    value(i) = evar(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'VSTRAIN') then
!--------------------------------------------------
                do i=1,nel
                  mt = ixq(1,i+nft)
                  if(mlw == 151)then
                    !multimaterial 151 (collocated scheme)
                      do ilay=1,multi_fvm%nbmat
                        mid = MAT_PARAM(mt)%multimat%mid(ilay)
                        rho0i (ilay) = pm(89,mid)
                        Vi (ilay) = multi_fvm%phase_alpha(ilay,i+nft) * gbuf%vol(i)
                        V0i (ilay) =  multi_fvm%phase_rho(ilay,i+nft) * Vi(ilay) / rho0i(ilay)         !rho0.V0 = rho.V
                      enddo
                      V0g = sum(V0i)
                      RHO0g = zero
                      do ilay=1,multi_fvm%nbmat
                        RHO0g = RHO0g + rho0i(ilay)*V0i(ilay)
                      end do
                      RHO0g = RHO0g / V0g
                      value(i) = multi_fvm%rho(i+nft) / RHO0g - ONE
                      is_written_value(i) = 1

                  elseif(mlw == 51)then
                    !multimaterial 51 (staggered scheme)
                    imat   = ixq(1,nft+1)
                    iadbuf = ipm(7,imat)
                    nuparam= ipm(9,imat)
                    uparam => bufmat(iadbuf:iadbuf+nuparam)
                    mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                    ipos = 1
                    !bijective order           !indexes
                    isubmat = nint(uparam(276+1));   iu(1)=n0phas+(isubmat-1)*nvphas + ipos-1
                    isubmat = nint(uparam(276+2));   iu(2)=n0phas+(isubmat-1)*nvphas + ipos-1
                    isubmat = nint(uparam(276+3));   iu(3)=n0phas+(isubmat-1)*nvphas + ipos-1
                    isubmat = nint(uparam(276+4));   iu(4)=n0phas+(isubmat-1)*nvphas + ipos-1
                    vfrac(i,1) = mbuf%var(i+iu(1)*nel)
                    vfrac(i,2) = mbuf%var(i+iu(2)*nel)
                    vfrac(i,3) = mbuf%var(i+iu(3)*nel)
                    vfrac(i,4) = mbuf%var(i+iu(4)*nel)
                    ipos = 12
                    !bijective order           !indexes
                    isubmat = nint(uparam(276+1));   iu(1)=n0phas+(isubmat-1)*nvphas + ipos-1
                    isubmat = nint(uparam(276+2));   iu(2)=n0phas+(isubmat-1)*nvphas + ipos-1
                    isubmat = nint(uparam(276+3));   iu(3)=n0phas+(isubmat-1)*nvphas + ipos-1
                    isubmat = nint(uparam(276+4));   iu(4)=n0phas+(isubmat-1)*nvphas + ipos-1
                    rhoi(1) = mbuf%var(i+iu(1)*nel)
                    rhoi(2) = mbuf%var(i+iu(2)*nel)
                    rhoi(3) = mbuf%var(i+iu(3)*nel)
                    rhoi(4) = mbuf%var(i+iu(4)*nel)
                    do ilay=1,4
                      mid = MAT_PARAM(mt)%multimat%mid(ilay)
                      rho0i (ilay) = pm(89,mid)
                      Vi (ilay) = vfrac(i,ilay) * gbuf%vol(i)
                      ipos = 12
                      V0i (ilay) =  rhoi(ilay) * Vi(ilay) / rho0i(ilay)         !rho0.V0 = rho.V
                    enddo
                    V0g = sum(V0i)
                    RHO0g = zero
                    do ilay=1,4
                      RHO0g = RHO0g + rho0i(ilay)*V0i(ilay)
                    end do
                    RHO0g = RHO0g / V0g
                    value(i) = gbuf%rho(i) / RHO0g - ONE
                    is_written_value(i) = 1

                  elseif(mlw == 37)then
                    !multimaterial 37 (staggered scheme)
                    imat   = ixq(1,nft+1)
                    iadbuf = ipm(7,imat)
                    nuparam= ipm(9,imat)
                    uparam => bufmat(iadbuf:iadbuf+nuparam)
                    mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                    rho0i(1) = uparam(11)
                    rho0i(2) = uparam(12)
                    Vi(1) = mbuf%var(i+3*nel) * gbuf%vol(i) !UVAR(I,4)  = VFRAC1
                    Vi(2) = mbuf%var(i+4*nel) * gbuf%vol(i) !UVAR(I,5)  = VFRAC2
                    rhoi(1) = mbuf%var(i+2*nel) !UVAR(I,3)  = RHO1
                    rhoi(2) = mbuf%var(i+1*nel) !UVAR(I,2)  = RHO2
                    V0i(1) =  rhoi(1) * Vi(1) / rho0i(1)         !rho0.V0 = rho.V
                    V0i(2) =  rhoi(2) * Vi(2) / rho0i(2)         !rho0.V0 = rho.V
                    V0g = sum(V0i)
                    RHO0g = zero
                    do ilay=1,2
                      RHO0g = RHO0g + rho0i(ilay)*V0i(ilay)
                    end do
                    RHO0g = RHO0g / V0g
                    value(i) = gbuf%rho(i) / RHO0g - ONE
                    is_written_value(i) = 1

                  elseif(mlw == 20)then
                    !multimaterial 20 (staggered scheme)
                    lbuf1  => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
                    lbuf2  => elbuf_tab(ng)%bufly(2)%lbuf(1,1,1)
                    mid = MAT_PARAM(mt)%multimat%mid(1)
                    rho0i(1) = pm(89,mid)
                    mid = MAT_PARAM(mt)%multimat%mid(2)
                    rho0i(2) = pm(89,mid)
                    Vi(1) = lbuf1%vol(i)
                    Vi(2) = lbuf2%vol(i)
                    rhoi(1) = lbuf1%rho(i)
                    rhoi(2) = lbuf2%rho(i)
                    V0i(1) =  rhoi(1) * Vi(1) / rho0i(1)         !rho0.V0 = rho.V
                    V0i(2) =  rhoi(2) * Vi(2) / rho0i(2)         !rho0.V0 = rho.V
                    V0g = sum(V0i)
                    RHO0g = zero
                    do ilay=1,2
                      RHO0g = RHO0g + rho0i(ilay)*V0i(ilay)
                    end do
                    RHO0g = RHO0g / V0g
                    value(i) = gbuf%rho(i) / RHO0g - ONE
                    is_written_value(i) = 1

                  else
                    !general case (monomaterial law)
                    if(pm(89,mt) > zero)then
                      value(i) = gbuf%rho(i) / pm(89,mt) - one
                      is_written_value(i) = 1
                    end if
                  end if

                enddo
!--------------------------------------------------
              elseif(keyword(1:8) == 'VSTRAIN/') then
!--------------------------------------------------
                detected = .false.
                read(keyword(9:), '(I2)', IOSTAT=ierr) ilay
                if(ierr == 0 .and. ilay > 0) then
                  if(mlw == 151 .and. ilay <= min(10,multi_fvm%nbmat))detected = .true.
                  if(mlw ==  51 .and. ilay <= 4                      )detected = .true.
                  if(mlw ==  37 .and. ilay <= 2                      )detected = .true.
                  if(mlw ==  20 .and. ilay <= 2                      )detected = .true.
                end if
                if(detected)then
                  do i=1,nel
                    mt = ixq(1,i+nft)

                    if(mlw == 151)then
                      !multimaterial 151 (collocated scheme)
                        mid = MAT_PARAM(mt)%multimat%mid(ilay)
                        rho0i(ilay) = pm(89,mid)
                        Vi(ilay) = multi_fvm%phase_alpha(ilay,i+nft) * gbuf%vol(i)
                        V0i(ilay) = multi_fvm%phase_rho(ilay,i+nft) * Vi(ilay) / rho0i(ilay)         !rho0.V0 = rho.V
                        value(i) = multi_fvm%phase_rho(ilay,i+nft) / rho0i(ilay) - ONE
                        is_written_value(i) = 1

                    elseif(mlw == 51)then
                      !multimaterial 51 (staggered scheme)
                      imat = ixq(1,nft+1)
                      iadbuf = ipm(7,imat)
                      nuparam= ipm(9,imat)
                      uparam => bufmat(iadbuf:iadbuf+nuparam)
                      mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                      mid = MAT_PARAM(mt)%multimat%mid(ilay)
                      rho0i(ilay) = pm(89,mid)
                      ipos = 1
                      !bijective order           !indexes
                      isubmat = nint(uparam(276+ilay));   iu(1)=n0phas+(isubmat-1)*nvphas + ipos-1
                      vfrac(i,ilay) = mbuf%var(i+iu(ilay)*nel)
                      Vi(ilay) = vfrac(i,ilay) * gbuf%vol(i)
                      ipos = 12
                      !bijective order           !indexes
                      isubmat = nint(uparam(276+ilay));   iu(ilay)=n0phas+(isubmat-1)*nvphas + ipos-1
                      rhoi(ilay) = mbuf%var(i+iu(ilay)*nel)
                      V0i (ilay) =  rhoi(ilay) * Vi(ilay) / rho0i(ilay)         !rho0.V0 = rho.V
                      value(i) = rhoi(ilay) / rho0i(ilay) - ONE
                      is_written_value(i) = 1

                    elseif(mlw == 37)then
                      !multimaterial 37 (staggered scheme)
                      imat = ixq(1,nft+1)
                      iadbuf = ipm(7,imat)
                      nuparam= ipm(9,imat)
                      uparam => bufmat(iadbuf:iadbuf+nuparam)
                      mbuf => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                      rho0i(ilay) = uparam(10+ilay)
                      Vi(ilay) = mbuf%var(i+(ilay+2)*nel) * gbuf%vol(i)
                      rhoi(ilay) = mbuf%var(i+(3-ilay)*nel) !UVAR(I,3)  = RHO1
                      V0i(ilay) =  rhoi(ilay) * Vi(ilay) / rho0i(ilay)
                      value(i) = rhoi(ilay) / rho0i(ilay) - ONE
                      is_written_value(i) = 1

                    elseif(mlw == 20)then
                      !multimaterial 20 (staggered scheme)
                      lbuf  => elbuf_tab(ng)%bufly(ilay)%lbuf(1,1,1)
                      mid = MAT_PARAM(mt)%multimat%mid(ilay)
                      rho0i(ilay) = pm(89,mid)
                      Vi(ilay) = lbuf%vol(i)
                      rhoi(ilay) = lbuf%rho(i)
                      V0i(ilay) =  rhoi(ilay) * Vi(ilay) / rho0i(ilay)         !rho0.V0 = rho.V
                      value(i) = rhoi(ilay) / rho0i(ilay) - ONE
                      is_written_value(i) = 1

                    else
                      !general case (monomaterial law)
                      is_written_value(i) = 0
                    end if
                enddo

              end if
!--------------------------------------------------
              endif  ! keyword
!--------------------------------------------------
              if(called_from_python) then
                quad_scalar(1:nel) = value(1:nel)
              else
                call h3d_write_scalar(iok_part,is_written_quad,quad_scalar,nel,0,nft,value,is_written_value)
              endif
            endif  ! ity
!-----------------------------------------------
          endif     ! mlw /= 13
!-----------------------------------------------
          return
        end

      end module
