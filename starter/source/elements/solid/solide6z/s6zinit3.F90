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
      ! \brief initialization of 6-node solid elements
      ! \details initializes element variables, material model, mass, etc ... for 6-node solid elements
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
!||    s6mass3                 ../starter/source/elements/thickshell/solide6c/s6mass3.F
!||    s6zcoor3                ../starter/source/elements/solid/solide6z/s6zcoor3.F90
!||    s6zderi3                ../starter/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zjacidp               ../starter/source/elements/solid/solide6z/s6zjacidp.F90
!||    s6zrcoor3               ../starter/source/elements/solid/solide6z/s6zrcoor3.F90
!||    sbulk3                  ../starter/source/elements/solid/solide/sbulk3.F
!||    sigin20b                ../starter/source/elements/solid/solide20/s20mass3.F
!||    smorth3                 ../starter/source/elements/solid/solide/smorth3.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod            ../starter/source/modules/defaults_mod.F90
!||    detonators_mod          ../starter/share/modules1/detonators_mod.F
!||    message_mod             ../starter/share/message_module/message_mod.F
!||    s6zcoor3_mod            ../starter/source/elements/solid/solide6z/s6zcoor3.F90
!||    s6zderi3_mod            ../starter/source/elements/solid/solide6z/s6zderi3.F90
!||    s6zjacidp_mod           ../starter/source/elements/solid/solide6z/s6zjacidp.F90
!||    s6zrcoor3_mod           ../starter/source/elements/solid/solide6z/s6zrcoor3.F90
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
        istrain  ,jthe     ,mtn      ,nft      ,ismstr   )                       
!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
      use elbufdef_mod            
      use message_mod
      use detonators_mod      
      use ale_connectivity_mod
      use matparam_def_mod
      use defaults_mod
      use names_and_titles_mod, only : nchartitle
      use glob_therm_mod
      use constant_mod
      use precision_mod, only : wp
      use eos_param_mod 
      use table_mat_vinterp_mod 
      use s6zjacidp_mod
      use s6zderi3_mod
      use s6zcoor3_mod
      use s6zrcoor3_mod
!-------------------------------------------------------------------------------
!    i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none
! ------------------------------------------------------------------------------
#include      "units_c.inc"
!-------------------------------------------------------------------------------
!    d u m m y   a r g u m e n t s
! ------------------------------------------------------------------------------
      type(elbuf_struct_), target,                intent(inout) :: elbuf_str  !< element buffer structure
      integer,                                    intent(in)    :: nixs       !< element connectivity array size
      integer,                                    intent(in)    :: numels     !< number of 3d solid elements
      integer,       dimension(nixs,numels),      intent(inout) :: ixs        !< element connectivity array
      integer,                                    intent(in)    :: numnod     !< total number of nodes
      real(kind=wp), dimension(numnod),           intent(inout) :: mas        !< element mass array
      integer,                                    intent(in)    :: npropm     !< number of material properties
      integer,                                    intent(in)    :: nummat     !< number of materials
      real(kind=wp), dimension(npropm,nummat),    intent(inout) :: pm         !< material property array
      real(kind=wp), dimension(3,numnod),         intent(inout) :: x          !< global coordinate array
      type(detonators_struct_),                   intent(inout) :: detonators
      integer,                                    intent(in)    :: npropg     !< number of properties per geometric property
      integer,                                    intent(in)    :: numgeo     !< number of geometric properties
      real(kind=wp), dimension(npropg,numgeo),    intent(inout) :: geo        !< geometric properties array
      type(t_ale_connectivity),                   intent(inout) :: ale_connectivity
      integer,                                    intent(in)    :: nparg      !< number of parameters per group
      integer,       dimension(nparg),            intent(inout) :: iparg      !< element group parameters
      integer,                                    intent(inout) :: nel        !< number of elements
      real(kind=wp), dimension(numels),           intent(inout) :: dtelem     !< element time step array
      integer,                                    intent(in)    :: nsigs    
      integer,                                    intent(in)    :: lsigi     
      real(kind=wp), dimension(nsigs,lsigi),      intent(inout) :: sigi
      integer,                                    intent(in)    :: lskew
      integer,                                    intent(in)    :: numskw
      real(kind=wp), dimension(lskew,numskw+1),   intent(inout) :: skew
      integer,                                    intent(in)    :: npropgi    !< number of geometric integer parameter
      integer,       dimension(npropgi,numgeo),   intent(inout) :: igeo       !< geometric property integer parameter
      real(kind=wp), dimension(numnod),           intent(inout) :: stifn      !< nodal stiffness array
      integer,                                    intent(in)    :: npsav      !< size of the part save array
      integer,                                    intent(in)    :: npart      !< number of parts
      real(kind=wp), dimension(npsav,npart),      intent(inout) :: partsav    !< part save array
      real(kind=wp), dimension(3,numnod),         intent(inout) :: v          !< nodal velocity array
      integer,       dimension(numels),           intent(inout) :: iparts  !< part array with the number of solid element
      real(kind=wp), dimension(8,numels),         intent(inout) :: mss        !< element mass array
      integer,                                    intent(in)    :: lipart1    !< logical flag for part 1
      integer,       dimension(lipart1, npart),   intent(inout) :: ipart
      type(glob_therm_),                          intent(in)    :: glob_therm
      integer,                                    intent(in)    :: nsigi
      integer,                                    intent(in)    :: lsigsp
      real(kind=wp), dimension(nsigi,lsigsp),     intent(inout) :: sigsp
      integer,                                    intent(in)    :: npropmi    !< number of material integer parameter
      integer,       dimension(npropmi,nummat),   intent(inout) :: ipm        !< material property indices
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
      integer,                                    intent(in)    :: snpc       !< size of the function pointer array
      integer,       dimension(snpc),             intent(inout) :: npf        !< function pointer array
      integer,                                    intent(in)    :: stf        !< size of the time function array
      real(kind=wp), dimension(stf),              intent(inout) :: tf         !< time function array
      integer,       dimension(numels),           intent(inout) :: strsglob
      integer,       dimension(numels),           intent(inout) :: straglob
      real(kind=wp), dimension(numels),           intent(inout) :: mssa
      integer,       dimension(5),                intent(inout) :: fail_ini
      integer,                                    intent(in)    :: sizloadp   !< size of load parameter
      integer,                                    intent(in)    :: nloadp     !< number of load parameters
      integer,       dimension(sizloadp,nloadp),  intent(in)    :: iloadp
      integer,                                    intent(in)    :: lfacload      
      real(kind=wp), dimension(lfacload, nloadp), intent(in)    :: facload    !< logical flag for shell elements
      integer,                                    intent(in)    :: nperturb   !< number of perturbations
      integer,                                    intent(in)    :: srnoise    !< size of the random noise array
      real(kind=wp), dimension(nperturb,srnoise), intent(inout) :: rnoise
      integer,       dimension(nperturb),         intent(in)    :: perturb
      type(matparam_struct_), dimension(nummat),  intent(inout) :: mat_param
      type(solid_defaults_),                      intent(in)    :: defaults_solid
      integer,                                    intent(in)    :: numsol     !< number of solutions
      integer,                                    intent(inout) :: i7stifs    !< stiffness matrix index
      integer,                                    intent(inout) :: isorth     !< orthogonality index
      integer,                                    intent(inout) :: istrain    !< strain index
      integer,                                    intent(inout) :: jthe       !< thermal index
      integer,                                    intent(inout) :: mtn        !< material type number
      integer,                                    intent(inout) :: nft        !< number of failure types
      integer,                                    intent(in) :: ismstr        !< simulation type indicator
!------------------------------------------------
!    l o c a l   v a r i a b l e s
!------------------------------------------------
      integer ::  i, nf1, ibid, igtyp, irep, ip, ilay, nlay, nuvar, ncc, jhbe
      integer ::  idef, jcvt
      integer ::  nptr, npts, nptt, l_pla, l_sigb, imas_ds
      integer, dimension(nel) :: mat, pid, ngl
      integer, dimension(nel) :: ix1, ix2, ix3, ix4, ix5, ix6
      real(kind=wp) :: bid, fv, sti, zi, wi
      real(kind=wp), dimension(nel) :: volu, dtx, vzl, rx, ry, rz
      real(kind=wp), dimension(nel) :: sx, sy, sz, tx, ty, tz
      real(kind=wp), dimension(nel) :: e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z
      real(kind=wp), dimension(nel) :: f1x, f1y, f1z, f2x, f2y, f2z
      real(kind=wp), dimension(nel) :: rhocp, temp0, deltax, aire
      real(kind=wp), dimension(nel) :: tempel
      real(kind=wp), dimension(nel) :: x1, x2, x3, x4, x5, x6
      real(kind=wp), dimension(nel) :: y1, y2, y3, y4, y5, y6
      real(kind=wp), dimension(nel) :: z1, z2, z3, z4, z5, z6
!c     ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.  
      real(kind=8),  dimension(nel) :: xd1, xd2, xd3, xd4, xd5, xd6
      real(kind=8),  dimension(nel) :: yd1, yd2, yd3, yd4, yd5, yd6
      real(kind=8),  dimension(nel) :: zd1, zd2, zd3, zd4, zd5, zd6    
      type(g_bufel_), pointer :: gbuf
      type(buf_lay_), pointer :: bufly
      type(l_bufel_), pointer :: lbuf
      type(buf_mat_), pointer :: mbuf
!===============================================================================
!     s o u r c e  l i n e s
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
          imas_ds = defaults_solid%imas
!=======================================================================
      do i = 1, nel
        rhocp(i) = pm(69, ixs(1, nft+i))
        temp0(i) = pm(79, ixs(1, nft+i))
      end do

      jcvt  = iparg(37)

      if (jcvt==1.and.isorth/=0) jcvt=2

 
      if (ismstr>=10) then
          call s6zcoor3(                                                         &
            x        ,ixs(1,nf1) ,ngl    ,mat      ,pid      ,                     &
            rx       ,ry       ,rz       ,sx       ,sy       ,sz       ,           &
            tx       ,ty       ,tz       ,e1x      ,e1y      ,e1z      ,           & 
            e2x      ,e2y      ,e2z      ,e3x      ,e3y      ,e3z      ,           &
            f1x      ,f1y      ,f1z      ,f2x      ,f2y      ,f2z      ,           &
            temp0    ,temp     ,glob_therm%nintemp ,                               &
            ix1      ,ix2      ,ix3      ,ix4      ,ix5      ,ix6      ,           &
            x1       ,x2       ,x3       ,x4       ,x5       ,x6       ,           &
            y1       ,y2       ,y3       ,y4       ,y5       ,y6       ,           &
            z1       ,z2       ,z3       ,z4       ,z5       ,z6       ,           &           
            xd1      ,xd2      ,xd3      ,xd4      ,xd5      ,xd6      ,           &
            yd1      ,yd2      ,yd3      ,yd4      ,yd5      ,yd6      ,           &
            zd1      ,zd2      ,zd3      ,zd4      ,zd5      ,zd6      ,           &
            nel      ,jthe     ,numnod) 

          call s6zjacidp(                                  &
                     xd1  ,xd2  ,xd3  ,xd4  ,xd5  ,xd6  ,  &
                     yd1  ,yd2  ,yd3  ,yd4  ,yd5  ,yd6  ,  &
                     zd1  ,zd2  ,zd3  ,zd4  ,zd5  ,zd6  ,  &
                     gbuf%jac_i ,nel)
      endif 


    call s6zrcoor3(                                                        &
      x        ,ixs(1,nf1),ngl     ,mat      ,pid      ,                     &
      rx       ,ry       ,rz       ,sx       ,sy       ,sz       ,           &
      tx       ,ty       ,tz       ,e1x      ,e1y      ,e1z      ,           & 
      e2x      ,e2y      ,e2z      ,e3x      ,e3y      ,e3z      ,           &
      f1x      ,f1y      ,f1z      ,f2x      ,f2y      ,f2z      ,           &
      temp0    ,temp     ,glob_therm%nintemp ,                               &
      ix1      ,ix2      ,ix3      ,ix4      ,ix5      ,ix6      ,           &       
      x1      ,x2      ,x3      ,x4      ,x5      ,x6      ,           &
      y1      ,y2      ,y3      ,y4      ,y5      ,y6      ,           &
      z1      ,z2      ,z3      ,z4      ,z5      ,z6      ,           &     
      xd1      ,xd2      ,xd3      ,xd4      ,xd5      ,xd6      ,           &
      yd1      ,yd2      ,yd3      ,yd4      ,yd5      ,yd6      ,           &
      zd1      ,zd2      ,zd3      ,zd4      ,zd5      ,zd6      ,           &
      nel      ,numnod   ,jthe)

      if (isorth == 1) then 
       call smorth3(pid  ,geo  ,igeo ,skew ,irep ,gbuf%gama  , &
              rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   ,&
              e1x  ,e1y  ,e1z  ,e2x  ,e2y  ,e2z  ,e3x  ,e3y  ,e3z  ,&
              f1x  ,f1y  ,f1z  ,f2x  ,f2y  ,f2z  ,nsigi,sigsp,nsigs,&
              sigi ,ixs  ,x    ,jhbe ,ptsol,nel  ,iparg(28),jcvt)             
     endif 

   
      call s6zderi3(                                               &
        nel     ,gbuf%vol,vzl                                      , &
        ngl     ,deltax  ,volu                                     , &
        xd1      ,xd2      ,xd3       ,xd4     ,xd5      ,xd6      , &
        yd1      ,yd2      ,yd3       ,yd4     ,yd5      ,yd6      , &
        zd1      ,zd2      ,zd3       ,zd4     ,zd5      ,zd6      )    

      !-------------------------------------------------------------------------
      !< thermal initialization
      if (jthe /= 0) call atheri(mat,pm,gbuf%temp)
      !-------------------------------------------------------------------------
!
      !-------------------------------------------------------------------------
      !< material initialization
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
      !< mass initialization
      !-------------------------------------------------------------------------              
      call s6mass3(                                                            &
        gbuf%rho ,mas      ,partsav  ,x        ,v        ,iparts(nf1),         &
        mss(1,nf1),rhocp   ,mcp      ,mcps(1,nf1),mssa(nf1),gbuf%fill,         &
        volu     ,ix1      ,ix2      ,ix3      ,ix4      ,ix5        ,         &
        ix6      ,imas_ds  )
!
      !-------------------------------------------------------------------------
      !< failure model initialization
      !-------------------------------------------------------------------------   
      call failini(                                                            &
        elbuf_str,nptr     ,npts     ,nptt     ,nlay     ,ipm      ,sigsp    , &
        nsigi    ,fail_ini ,sigi     ,nsigs    ,ixs      ,nixs     ,ptsol    , &
        rnoise   ,perturb  ,mat_param)
!
      !-------------------------------------------------------------------------
      !< assemble nodal volumes and moduli for interface stiffness
      !  warning : ix1, ix2 ... ix6 <=> nc(mvsiz,6)
      !------------------------------------------------------------------------- 
      if (i7stifs /= 0) then
        ncc = 6
        call sbulk3(                                                           &
          volu     ,ix1      ,ncc      ,mat      ,pm       ,volnod   ,         &
          bvolnod  ,vns(1,nf1),bns(1,nf1),bid    ,bid      ,gbuf%fill)     
      endif
!
      !-------------------------------------------------------------------------
      !< element time step 
      !------------------------------------------------------------------------- 
      aire(:) = zero
      call dtmain(                                                             &
        geo      ,pm       ,ipm      ,pid      ,mat      ,fv       ,lbuf%eint ,&
        lbuf%temp,lbuf%deltax,lbuf%rk,lbuf%re  ,bufmat   ,deltax   ,aire      ,&
        volu     ,dtx      ,igeo     ,igtyp    )
      !------------------------------------------
!
      !-------------------------------------------------------------------------
      !< nodal stiffness initialization
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
