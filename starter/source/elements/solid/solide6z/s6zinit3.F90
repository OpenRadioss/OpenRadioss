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
! ======================================================================================================================
! \brief   initiation of penta solid element
! ======================================================================================================================
!||====================================================================
!||    s6zinit3_mod   ../starter/source/elements/solid/solide6z/s6zinit3.F90
!||--- called by ------------------------------------------------------
!||    initia         ../starter/source/elements/initia/initia.F
!||====================================================================
      module s6zinit3_mod
      contains
      ! ======================================================================================================================
      ! \brief Initialization of 6-node solid elements
      ! \details Initializes element variables, material model, mass, etc ... for 6-node solid elements
      ! ======================================================================================================================
!||====================================================================
!||    s6zinit3                ../starter/source/elements/solid/solide6z/s6zinit3.F90
!||--- called by ------------------------------------------------------
!||    initia                  ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    atheri                  ../starter/source/ale/atheri.F
!||    dtmain                  ../starter/source/materials/time_step/dtmain.F
!||    failini                 ../starter/source/elements/solid/solide/failini.F
!||    matini                  ../starter/source/materials/mat_share/matini.F
!||    s6ccoor3                ../starter/source/elements/thickshell/solide6c/s6ccoor3.F
!||    s6cderi3                ../starter/source/elements/thickshell/solide6c/s6cderi3.F
!||    s6mass3                 ../starter/source/elements/thickshell/solide6c/s6mass3.F
!||    sbulk3                  ../starter/source/elements/solid/solide/sbulk3.F
!||    sigin20b                ../starter/source/elements/solid/solide20/s20mass3.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod            ../starter/source/modules/defaults_mod.F90
!||    detonators_mod          ../starter/share/modules1/detonators_mod.F
!||    message_mod             ../starter/share/message_module/message_mod.F
!||    table_mat_vinterp_mod   ../starter/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      subroutine s6zinit3(                                                     & 
        elbuf_str,nixs     ,numels   ,ixs      ,numnod   ,mas       ,          &      
        npropm   ,nummat   ,pm       ,x        ,detonators,npropg   ,          &
        numgeo   ,geo      ,ale_connectivity   ,nparg    ,                     &
        iparg    ,nel      ,dtelem   ,nsigs    ,lsigi    ,sigi      ,          &
        lskew    ,numskw   ,skew     ,npropgi  ,igeo     ,stifn     ,          &
        npsav    ,npart    ,partsav  ,v        ,iparts   ,mss       ,          &
        lipart1  ,ipart    ,glob_therm,nsigi   ,lsigsp   ,sigsp     ,          &
        npropmi  ,ipm      ,iuser    ,volnod   ,bvolnod  ,vns       ,          &
        bns      ,ptsol    ,sbufmat  ,bufmat   ,mcp      ,mcps      ,          &
        temp     ,snpc     ,npf      ,stf      ,tf       ,strsglob  ,          &
        straglob ,mssa     ,fail_ini ,sizloadp ,nloadp   ,iloadp    ,          &
        lfacload ,facload  ,nperturb ,srnoise  ,rnoise   ,perturb   ,          &
        mat_param,defaults_solid     ,numsol   ,i7stifs  ,isorth    ,          &   
        istrain  ,jthe     ,mtn      ,nft      )                       
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use elbufdef_mod            
      use message_mod
      use detonators_mod      
      use ale_connectivity_mod
      use matparam_def_mod
      use defaults_mod
      use names_and_titles_mod, only : NCHARTITLE
      use glob_therm_mod
      use constant_mod
      use precision_mod, only : wp
      use eos_param_mod 
      use table_mat_vinterp_mod 
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
      implicit none
! ------------------------------------------------------------------------------
#include      "units_c.inc"
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
! ------------------------------------------------------------------------------
      type(elbuf_struct_), target,                intent(inout) :: elbuf_str  !< Element buffer structure
      integer,                                    intent(in)    :: nixs       !< Element connectivity array size
      integer,                                    intent(in)    :: numels     !< Number of 3D solid elements
      integer,       dimension(nixs,numels),      intent(inout) :: ixs        !< Element connectivity array
      integer,                                    intent(in)    :: numnod     !< Total number of nodes
      real(kind=wp), dimension(numnod),           intent(inout) :: mas        !< Element mass array
      integer,                                    intent(in)    :: npropm     !< Number of material properties
      integer,                                    intent(in)    :: nummat     !< Number of materials
      real(kind=wp), dimension(npropm,nummat),    intent(inout) :: pm         !< Material property array
      real(kind=wp), dimension(3,numnod),         intent(inout) :: x          !< Global coordinate array
      type(detonators_struct_),                   intent(inout) :: detonators
      integer,                                    intent(in)    :: npropg     !< Number of properties per geometric property
      integer,                                    intent(in)    :: numgeo     !< Number of geometric properties
      real(kind=wp), dimension(npropg,numgeo),    intent(inout) :: geo        !< Geometric properties array
      type(t_ale_connectivity),                   intent(inout) :: ale_connectivity
      integer,                                    intent(in)    :: nparg      !< Number of parameters per group
      integer,       dimension(nparg),            intent(inout) :: iparg      !< Element group parameters
      integer,                                    intent(inout) :: nel        !< Number of elements
      real(kind=wp), dimension(nel),              intent(inout) :: dtelem     !< Element time step array
      integer,                                    intent(in)    :: nsigs    
      integer,                                    intent(in)    :: lsigi     
      real(kind=wp), dimension(nsigs,lsigi),      intent(inout) :: sigi
      integer,                                    intent(in)    :: lskew
      integer,                                    intent(in)    :: numskw
      real(kind=wp), dimension(lskew,numskw+1),   intent(inout) :: skew
      integer,                                    intent(in)    :: npropgi    !< Number of geometric integer parameter
      integer,       dimension(npropgi,numgeo),   intent(inout) :: igeo       !< Geometric property integer parameter
      real(kind=wp), dimension(numnod),           intent(inout) :: stifn      !< Nodal stiffness array
      integer,                                    intent(in)    :: npsav      !< Size of the part save array
      integer,                                    intent(in)    :: npart      !< Number of parts
      real(kind=wp), dimension(npsav,npart),      intent(inout) :: partsav    !< Part save array
      real(kind=wp), dimension(3,numnod),         intent(inout) :: v          !< Nodal velocity array
      integer,       dimension(nel),              intent(inout) :: iparts     !< Part array
      real(kind=wp), dimension(8,numels),         intent(inout) :: mss        !< Element mass array
      integer,                                    intent(in)    :: lipart1    !< Logical flag for part 1
      integer,       dimension(lipart1, npart),   intent(inout) :: ipart
      type(glob_therm_),                          intent(in)    :: glob_therm
      integer,                                    intent(in)    :: nsigi
      integer,                                    intent(in)    :: lsigsp
      real(kind=wp), dimension(nsigi,lsigsp),     intent(inout) :: sigsp
      integer,                                    intent(in)    :: npropmi    !< Number of material integer parameter
      integer,       dimension(npropmi,nummat),   intent(inout) :: ipm        !< Material property indices
      integer,                                    intent(in)    :: iuser
      real(kind=wp), dimension(numnod),           intent(inout) :: volnod
      real(kind=wp), dimension(numnod),           intent(inout) :: bvolnod
      real(kind=wp), dimension(8,numels),         intent(inout) :: vns
      real(kind=wp), dimension(8,numels),         intent(inout) :: bns
      integer,       dimension(numels),           intent(in)    :: ptsol
      integer,                                    intent(in)    :: sbufmat
      real(kind=wp), dimension(sbufmat),          intent(inout) :: bufmat
      real(kind=wp), dimension(numnod),           intent(inout) :: mcp
      real(kind=wp), dimension(8,numels),         intent(inout) :: mcps
      real(kind=wp), dimension(numnod),           intent(inout) :: temp
      integer,                                    intent(in)    :: snpc       !< Size of the function pointer array
      integer,       dimension(snpc),             intent(inout) :: npf        !< Function pointer array
      integer,                                    intent(in)    :: stf        !< Size of the time function array
      real(kind=wp), dimension(stf),              intent(inout) :: tf         !< Time function array
      integer,       dimension(numels),           intent(inout) :: strsglob
      integer,       dimension(numels),           intent(inout) :: straglob
      real(kind=wp), dimension(numels),           intent(inout) :: mssa
      integer,       dimension(5),                intent(inout) :: fail_ini
      integer,                                    intent(in)    :: sizloadp   !< Size of load parameter
      integer,                                    intent(in)    :: nloadp     !< Number of load parameters
      integer,       dimension(sizloadp,nloadp),  intent(in)    :: iloadp
      integer,                                    intent(in)    :: lfacload      
      real(kind=wp), dimension(lfacload, nloadp), intent(in)    :: facload    !< Logical flag for shell elements
      integer,                                    intent(in)    :: nperturb   !< Number of perturbations
      integer,                                    intent(in)    :: srnoise    !< Size of the random noise array
      real(kind=wp), dimension(nperturb,srnoise), intent(inout) :: rnoise
      integer,       dimension(nperturb),         intent(in)    :: perturb
      type(matparam_struct_), dimension(nummat),  intent(inout) :: mat_param
      type(solid_defaults_),                      intent(in)    :: defaults_solid
      integer,                                    intent(in)    :: numsol     !< Number of solutions
      integer,                                    intent(inout) :: i7stifs    !< Stiffness matrix index
      integer,                                    intent(inout) :: isorth     !< Orthogonality index
      integer,                                    intent(inout) :: istrain    !< Strain index
      integer,                                    intent(inout) :: jthe       !< Thermal index
      integer,                                    intent(inout) :: mtn        !< Material type number
      integer,                                    intent(inout) :: nft        !< Number of failure types
!------------------------------------------------
!    L o c a l   V a r i a b l e s
!------------------------------------------------
      integer :: i, nf1, ibid, igtyp, irep, ip, ilay, nlay, nuvar, ncc, jhbe
      integer :: nuvarr, idef, ipang, ipthk, ippos, ipmat, ig, im, mtn0, nlymax
      integer :: ipid1, nptr, npts, nptt, l_pla, l_sigb, imas_ds
      integer, dimension(nel) :: mat, pid, ngl, mat0
      integer, dimension(nel) :: ix1, ix2, ix3, ix4, ix5, ix6
      real(kind=wp) :: bid, fv, sti, zi, wi
      real(kind=wp), dimension(nel) :: volu, dtx, vzl, vzq, rx, ry, rz
      real(kind=wp), dimension(nel) :: sx, sy, sz, tx, ty, tz
      real(kind=wp), dimension(nel) :: e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z
      real(kind=wp), dimension(nel) :: f1x, f1y, f1z, f2x, f2y, f2z
      real(kind=wp), dimension(nel) :: rhocp, temp0, deltax, aire
      real(kind=wp), dimension(51, nel) :: v8loc
      real(kind=wp), dimension(nel) :: tempel
      real(kind=wp), dimension(nel) :: x1, x2, x3, x4, x5, x6
      real(kind=wp), dimension(nel) :: y1, y2, y3, y4, y5, y6
      real(kind=wp), dimension(nel) :: z1, z2, z3, z4, z5, z6
      type(g_bufel_), pointer :: gbuf
      type(buf_lay_), pointer :: bufly
      type(l_bufel_), pointer :: lbuf
      type(buf_mat_), pointer :: mbuf
      real(kind=wp), dimension(nel) :: angle, dtx0
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
      gbuf => elbuf_str%gbuf
      lbuf  => elbuf_str%bufly(1)%lbuf(1,1,1)
      mbuf  => elbuf_str%bufly(1)%mat(1,1,1)
      bufly => elbuf_str%bufly(1)
      nptr  =  elbuf_str%nptr
      npts  =  elbuf_str%npts
      nptt  =  elbuf_str%nptt
      nlay  =  elbuf_str%nlay 
      jhbe  =  iparg(23)
      irep  =  iparg(35)
      igtyp =  iparg(38)
      nf1   = nft+1
      idef  = 0
      ibid  = 0
      bid   = zero
      isorth = 0
      imas_ds = defaults_solid%imas
!=======================================================================
      do i = 1, nel
        rhocp(i) = pm(69, ixs(1, nft+i))
        temp0(i) = pm(79, ixs(1, nft+i))
      end do
!
      call s6ccoor3(                                                           &
        x        ,ixs(1,nf1),geo     ,ngl      ,mat      ,pid      ,           &
        rx       ,ry       ,rz       ,sx       ,sy       ,sz       ,           &
        tx       ,ty       ,tz       ,e1x      ,e1y      ,e1z      ,           & 
        e2x      ,e2y      ,e2z      ,e3x      ,e3y      ,e3z      ,           &
        f1x      ,f1y      ,f1z      ,f2x      ,f2y      ,f2z      ,           &
        temp0    ,temp     ,glob_therm%nintemp ,                               &
        ix1      ,ix2      ,ix3      ,ix4      ,ix5      ,ix6      ,           &
        x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,           &
        y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,           &
        z1       ,z2       ,z3       ,z4       ,z5       ,z6       )
!
      call s6cderi3(                                                           &
        nel      ,gbuf%vol ,geo      ,vzl      ,ngl      ,deltax   ,volu     , &
        x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,           &
        y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,           &
        z1       ,z2       ,z3       ,z4       ,z5       ,z6       )                      
!
      !-------------------------------------------------------------------------
      !< Thermal initialization
      if (jthe /= 0) call atheri(mat,pm,gbuf%temp)
      !-------------------------------------------------------------------------
!
      !-------------------------------------------------------------------------
      !< Material initialization
      !-------------------------------------------------------------------------
      ilay   = 1
      ip     = 0
      lbuf   => elbuf_str%bufly(ilay)%lbuf(1,1,1)
      mbuf   => elbuf_str%bufly(ilay)%mat(1,1,1)
      l_pla  =  elbuf_str%bufly(ilay)%l_pla
      l_sigb =  elbuf_str%bufly(ilay)%l_sigb
!
      zi = zero
      wi = two
      do i=1,nel
        lbuf%vol0dp(i)= half*wi*(gbuf%vol(i)+vzl(i)*zi)
        lbuf%vol(i)= lbuf%vol0dp(i)
      end do
      tempel(1:nel) = temp0(1:nel)
!
      call matini(                                                             &
        pm       ,ixs      ,nixs     ,x        ,geo      ,ale_connectivity  ,  &
        detonators,iparg   ,sigi     ,nel      ,skew     ,igeo     ,           &
        ipart    ,iparts   ,mat      ,ipm      ,nsigs    ,numsol   ,ptsol   ,  &
        ilay     ,ngl      ,npf      ,tf       ,bufmat   ,gbuf     ,lbuf    ,  &
        mbuf     ,elbuf_str,iloadp   ,facload  ,deltax   ,tempel   ,mat_param   )
!
      if (mtn >= 28) then
        nuvar = ipm(8,ixs(1,nft+1))
        idef =1
      else
        nuvar = 0
        if (mtn == 14 .or. mtn == 12) then
          idef = 1
        elseif (mtn == 24) then
          idef = 1
        elseif (istrain == 1) then
          if (mtn == 1) then
            idef =1
          elseif(mtn == 2)then
           idef =1
         elseif(mtn == 4)then
           idef =1
        elseif(mtn == 3.or.mtn == 6.or.mtn == 10 &
               .or.mtn == 21.or.mtn == 22.or.mtn == 23.or.mtn == 49)then
           idef =1
         endif
        endif
      end if
      call sigin20b(                                                           &
        lbuf%sig ,pm      ,lbuf%vol ,sigsp    ,                                &
        sigi     ,lbuf%eint,lbuf%rho,mbuf%var ,lbuf%stra,                      &
        ixs      ,nixs    ,nsigi    ,ilay     ,nuvar    ,                      &
        nel      ,iuser   ,idef     ,nsigs    ,strsglob ,                      &
        straglob ,jhbe    ,igtyp    ,x        ,lbuf%gama,                      &
        mat      ,lbuf%pla,l_pla    ,ptsol    ,lbuf%sigb,                      &
        l_sigb   ,ipm     ,bufmat   ,lbuf%vol0dp)
!
      !-------------------------------------------------------------------------
      !< Mass initialization
      !-------------------------------------------------------------------------              
      call s6mass3(                                                            &
        gbuf%rho ,mas      ,partsav  ,x        ,v        ,iparts(nf1),         &
        mss(1,nf1),rhocp   ,mcp      ,mcps(1,nf1),mssa(nf1),gbuf%fill,         &
        volu     ,ix1      ,ix2      ,ix3      ,ix4      ,ix5        ,         &
        ix6      ,imas_ds  )
!
      !-------------------------------------------------------------------------
      !< Failure model initialization
      !-------------------------------------------------------------------------   
      call failini(                                                            &
        elbuf_str,nptr     ,npts     ,nptt     ,nlay     ,ipm      ,sigsp    , &
        nsigi    ,fail_ini ,sigi     ,nsigs    ,ixs      ,nixs     ,ptsol    , &
        rnoise   ,perturb  ,mat_param)
!
      !-------------------------------------------------------------------------
      !< Assemble nodal volumes and moduli for interface stiffness
      !  Warning : ix1, ix2 ... ix6 <=> nc(mvsiz,6)
      !------------------------------------------------------------------------- 
      if (i7stifs /= 0) then
        ncc = 6
        call sbulk3(                                                           &
          volu     ,ix1      ,ncc      ,mat      ,pm       ,volnod   ,         &
          bvolnod  ,vns(1,nf1),bns(1,nf1),bid    ,bid      ,gbuf%fill)     
      endif
!
      !-------------------------------------------------------------------------
      !< Element time step 
      !------------------------------------------------------------------------- 
      aire(:) = zero
      call dtmain(                                                             &
        geo      ,pm       ,ipm      ,pid      ,mat      ,fv       ,lbuf%eint ,&
        lbuf%temp,lbuf%deltax,lbuf%rk,lbuf%re  ,bufmat   ,deltax   ,aire      ,&
        volu     ,dtx      ,igeo     ,igtyp    )
      !------------------------------------------
!
      !-------------------------------------------------------------------------
      !< Nodal stiffness initialization
      !-------------------------------------------------------------------------      
      do i=1,nel
        dtelem(nft+i)=dtx(i)
        sti = fourth * gbuf%fill(i) * gbuf%rho(i) * volu(i) / &
              max(em20,dtx(i)*dtx(i))
        stifn(ixs(2,i+nft))=stifn(ixs(2,i+nft))+sti
        stifn(ixs(3,i+nft))=stifn(ixs(3,i+nft))+sti
        stifn(ixs(4,i+nft))=stifn(ixs(4,i+nft))+sti
        stifn(ixs(5,i+nft))=stifn(ixs(5,i+nft))+sti
        stifn(ixs(6,i+nft))=stifn(ixs(6,i+nft))+sti
        stifn(ixs(7,i+nft))=stifn(ixs(7,i+nft))+sti
      end do
!
      end subroutine s6zinit3
      end module s6zinit3_mod
