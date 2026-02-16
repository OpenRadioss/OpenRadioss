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
!||    r23law135_mod      ../engine/source/elements/spring/r23law135.F90
!||--- called by ------------------------------------------------------
!||    r23forc3           ../engine/source/elements/spring/r23forc3.F
!||--- calls      -----------------------------------------------------
!||    r23bilan           ../engine/source/elements/spring/r23bilan.F
!||    r23coor3           ../engine/source/elements/spring/r23coor3.F
!||    r23l135def3        ../engine/source/elements/spring/r23l135def3.F90
!||    r23sens3           ../engine/source/elements/spring/r23sens3.F
!||    r2cum3             ../engine/source/elements/spring/r2cum3.F
!||    r2cum3p            ../engine/source/elements/spring/r2cum3p.F
!||    r2len3             ../engine/source/elements/spring/r2len3.F
!||    r2tors             ../engine/source/elements/spring/r2tors.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod       ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod        ../common_source/modules/elements/element_mod.F90
!||    h3d_mod            ../engine/share/modules/h3d_mod.F
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||    sensor_mod         ../common_source/modules/sensor_mod.F90
!||    table_mod          ../engine/share/modules/table_mod.F
!||====================================================================
      module r23law135_mod
      contains
!||====================================================================
!||
!||    r23law135
!||
!||====================================================================
        subroutine r23law135( &
            elbuf_str , nel       , igeo      , geo       , &
            ipm       , ixr       , x         , xdp       , &
            f         , skew      , vr        , ar        , &
            v         , stifn     , stifr     , fsky      , &
            iadr      , nsensor   , sensor_tab, partsav   , &
            ipartr    , tani      , fx1       , fx2       , &
            fy1       , fy2       , fz1       , fz2       , &
            mx1       , mx2       , my1       , my2       , &
            mz1       , mz2       , gresav    , grth      , &
            igrth     , h3d_data  , igre      , nft       , &
            mat_elem  , npropg    , lskew     , iparit    , &
            inispri   , scodver   , ismdisp   , numelr    , &
            tt        , dt1       , anim_fe   , mtn       , &
            ngrth     , nelem     , numgeo    , nummat    , &
            numnod    , npsav     , ngpe      , nthread   , &
            numskw    , lsky)



!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod,    only : wp
          use constant_mod,     only : zero, ten, one, ep30, em20
          use mvsiz_mod,        only : mvsiz
          use prop_param_mod
          use elbufdef_mod
          use table_mod
          use h3d_mod
          use sensor_mod
          use python_funct_mod
          use element_mod,      only : nixr
          use matparam_def_mod
          use mat_elem_mod
          use r23l135def3_mod
          use r2len3law135_mod



!-------------------------------------------------------------------------------
!   I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none


!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nel       !< Number of elements
          integer,                          intent(in)    :: igre      !< Flag
          integer,                          intent(in)    :: nsensor   !< Number of sensors
          integer,                          intent(in)    :: nft       !< Time step offset or flag
          integer,                          intent(in)    :: npropg    !< Number of real geometry parameters
          integer,                          intent(in)    :: lskew     !< Number of skew definitions
          integer,                          intent(in)    :: iparit    !< Partition flag
          integer,                          intent(in)    :: mtn       !< Partition flag  
          integer,                          intent(in)    :: inispri   !< Initial spring flag
          integer,                          intent(in)    :: scodver   !< Code version
          integer,                          intent(in)    :: ismdisp   !< Mass damping flag
          integer,                          intent(in)    :: numelr    !< Number of elements for result output
          real(kind=wp),                    intent(inout) :: tt     !< Time
          real(kind=wp),                    intent(inout) :: dt1    !< Time step  
          integer, dimension(20),           intent(in)    :: anim_fe   !< Animation flag array
          integer,                            intent(in)    :: ngrth     !< number of growth parameters
          integer,                            intent(in)    :: nelem     !< number of elements per growth parameter
          integer,                            intent(in)    :: numgeo
          integer,                            intent(in)    :: nummat
          integer,                            intent(in)    :: numnod
          integer,                            intent(in)    :: npsav
          integer,                            intent(in)    :: ngpe
          integer,                            intent(in)    :: nthread
          integer,                            intent(in)    :: numskw
          integer,                            intent(in)    :: lsky      !< skyline array size
          integer, dimension(nixr,numelr),    intent(in)  :: ixr                  
          integer, dimension(3,numelr),          intent(in)    :: iadr      !< Address pointers????
          integer, dimension(numelr),            intent(in)    :: ipartr    !< Part pointers
          integer, dimension(n_var_igeo,numgeo), intent(in)    :: igeo      !< Integer geometry parameters
          integer, dimension(n_var_ipm,nummat), intent(in)    ::ipm
          integer,   dimension(ngrth+nelem+1),intent(inout) :: grth  
          integer, dimension(nel),            intent(inout) :: igrth     !< Integer thermal/growth array    
          real(kind=wp), dimension(n_var_geo,numgeo), intent(in)  :: geo       !< Real geometry parameters
          real(kind=wp), dimension(3,numnod),      intent(in)    :: x         !< Coordinates (Working Precision)
          real(kind=wp), dimension(numnod),        intent(inout) :: f         !< Forces
          real(kind=WP), dimension(lskew,numskw), intent(in) :: skew   !< main structure for skews
          real(kind=wp), dimension(8,lsky),   intent(inout) :: fsky      !< skyline force array 
          real(kind=wp), dimension(3,numnod),      intent(inout) :: vr        !< Rotational velocities
          real(kind=wp), dimension(3,numnod),      intent(inout) :: v         !< Translational velocities
          real(kind=wp), dimension(3,numnod),      intent(inout) :: ar        !< Rotational accelerations
          real(kind=wp), dimension(numnod),        intent(inout) :: stifn     !< Translational stiffness
          real(kind=wp), dimension(numnod),        intent(inout) :: stifr     !< Rotational stiffness
          real(kind=wp), dimension(npsav),        intent(inout) :: partsav   !< Part saved state
          real(kind=wp), dimension(15,nel),       intent(inout) :: tani      !< Anisotropic/Tensor data
          real(kind=wp), dimension(npsav,ngpe*nthread),      intent(inout) :: gresav    !< Global saved array
          ! Fixed size arrays for vectorized internal calculations
          real(kind=wp), dimension(nel),  intent(inout) :: fx1, fy1, fz1
          real(kind=wp), dimension(nel),  intent(inout) :: fx2, fy2, fz2
          real(kind=wp), dimension(nel),  intent(inout) :: mx1, my1, mz1
          real(kind=wp), dimension(nel),  intent(inout) :: mx2, my2, mz2
          real(kind=8),  dimension(3,numnod),    intent(in)    :: xdp       !< Double precision coordinates
          type(elbuf_struct_),              intent(inout), target :: elbuf_str !< Element buffer structure
          type(h3d_database),               intent(inout) :: h3d_data          !< H3D Output database
          type(sensor_str_), dimension(nsensor), intent(in) :: sensor_tab      !< Sensor configuration
          type(mat_elem_),                  intent(inout), target :: mat_elem  !< Material element data

!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer, dimension(nel) :: ngl, pid, nc1, nc2, mid, iequil
          integer :: igtyp, i, i0, nuvar, iadbuf,j, n1, n2, m1, m2, irb1, irb2
          real(kind=wp) :: xl, xx1, xx2, xx, mass1, iner1, km1, krm1, kx1, kx2, kr
          real(kind=wp) :: mass2, iner2, km2, krm2, kr1, kxmax, krmax,kx,nv
          integer, dimension(6) :: ii
          real(kind=wp), dimension(3,nel) :: sti, stir
          real(kind=wp), dimension(nel)   :: fr_w_e, off
          real(kind=wp) :: bid, DTC, DTA,ALPHA
          real(kind=wp), dimension(nel) :: x1, y1, z1, x2, y2, z2
          real(kind=wp), dimension(nel) :: exx, eyx, ezx, exy, eyy, ezy, exz, eyz, ezz
          real(kind=wp), dimension(nel) :: xcr, xm, rx1, rx2, ry1, ry2, rz1, rz2, xin
          real(kind=wp), dimension(nel) :: ak, xkm, xcm, xkr
          real(kind=8), dimension(3,nel) :: x1dp, x2dp
          type(g_bufel_), pointer :: gbuf
          type(matparam_struct_)  :: mat_param

!===============================================================================
!   B o d y
!===============================================================================
          gbuf => elbuf_str%gbuf      

          fx1(1:nel) = zero
          fx2(1:nel) = zero
          fy1(1:nel) = zero
          fy2(1:nel) = zero
          fz1(1:nel) = zero
          fz2(1:nel) = zero
          mx1(1:nel) = zero
          mx2(1:nel) = zero
          my1(1:nel) = zero
          my2(1:nel) = zero
          mz1(1:nel) = zero
          mz2(1:nel) = zero

          do i = 1, 6
            ii(i) = (i - 1) * nel + 1
          end do

          i0 = ixr(1,1)
          igtyp = igeo(11,i0)

          bid = zero
          fr_w_e(1:nel) = zero
!---------------------------------------------------------------------
! Coordinate Transformation & Sensor Checking
!---------------------------------------------------------------------
          call r23coor3( &
            x       , vr      , ixr     , xdp     , &
            x1dp    , x2dp    , ngl     , x1      , &
            y1      , z1      , x2      , y2      , &
            z2      , pid     , mid     , rx1     , &
            ry1     , rz1     , rx2     , ry2     , &
            rz2     , nc1     , nc2     , nel)

          call r23sens3( &
            geo                 , gbuf%off          , sensor_tab        , gbuf%totdepl(ii(1)), &
            gbuf%totdepl(ii(2)) , gbuf%totdepl(ii(3)), gbuf%length(ii(1)), gbuf%length(ii(2)), &
            gbuf%length(ii(3))  , gbuf%totrot(ii(1)), gbuf%totrot(ii(2)), gbuf%totrot(ii(3)),  &
            igeo                , pid               , nel               , nsensor)

!---------------------------------------------------------------------
! Element Activation Logic
!---------------------------------------------------------------------
          do i = 1, nel
            if (gbuf%off(i) /= -ten) then
              off(i) = min(one, abs(gbuf%off(i)))
            else
              ! spring may be activated by sensor and is actually inactive
              off(i) = zero
            end if
          end do

          do i = 1, nel
            xkm(i) = 0.
            xkr(i) = 0.
            xcm(i) = 0.
            xcr(i) = 0.
            mid(i) = ixr(5,i)
            iadbuf = ipm(7,mid(i))
            nuvar  = gbuf%g_nuvar
          end do
          
          i = mid(1)
          mat_param = mat_elem%mat_param(i)

!---------------------------------------------------------------------
! Constitutive Law Calculation (r23l135def3)
!---------------------------------------------------------------------
          call r23l135def3( &
            pid                 , skew                , &
            geo                 , gbuf%for(ii(1):ii(1)+nel-1)     , gbuf%for(ii(2):ii(2)+nel-1)     , &
            gbuf%for(ii(3):ii(3)+nel-1)     , &
            gbuf%totdepl(ii(1):ii(1)+nel-1) , gbuf%totdepl(ii(2):ii(2)+nel-1) , gbuf%totdepl(ii(3):ii(3)+nel-1) ,  &
            gbuf%length(ii(1):ii(1)+nel-1)  , gbuf%length(ii(2):ii(2)+nel-1)  , gbuf%length(ii(3):ii(3)+nel-1)  , &
            gbuf%mom(ii(1):ii(1)+nel-1)     , &
            gbuf%mom(ii(2):ii(2)+nel-1)     , gbuf%mom(ii(3):ii(3)+nel-1)     , gbuf%totrot(ii(1):ii(1)+nel-1)  , &
            gbuf%totrot(ii(2):ii(2)+nel-1)  , &
            gbuf%totrot(ii(3):ii(3)+nel-1)  , v                               , nel                 , &
            gbuf%length_err     , x1dp                , x2dp                , &
            xkr                 , exx                 , &
            eyx                 , ezx                 , exy                 , eyy                 , &
            ezy                 , exz                 , eyz                 , ezz                 , &
            xcr                 , rx1                 , ry1                 , rz1                 , &
            rx2                 , ry2                 , rz2                 , xin                 , &
            xm                  , xkm                 , xcm                 , &
            nc1                 , nc2                 , nuvar               , gbuf%var            , &
            gbuf%mass           , gbuf%defini(ii(1):ii(1)+nel-1)  , gbuf%defini(ii(2):ii(2)+nel-1)  ,             &
            gbuf%defini(ii(3):ii(3)+nel-1)  , gbuf%defini(ii(4):ii(4)+nel-1)  , gbuf%defini(ii(5):ii(5)+nel-1)  , &
            gbuf%defini(ii(6):ii(6)+nel-1)  , &
            iequil              , &
            gbuf%skew_id        , &
            mat_param           ,                                             &
            lskew               , npropg              ,                       &
            inispri             , scodver             , ismdisp             , numelr              , &
            tt                  , dt1                 ,                                             &
            anim_fe             , numskw             , numgeo              , numnod)

            

!---------------------------------------------------------------------
! Post-Processing & Accumulation
!---------------------------------------------------------------------
          do i = 1, nel
            if (gbuf%off(i) /= -ten .and. off(i) < one) gbuf%off(i) = off(i)
          end do
          call R2LEN3LAW135(          &
            nel   ,  sti   , stir   , &
            xkm   , xkr     )
            
          call r23bilan( &
            gbuf%eint,partsav , ixr     , geo     , &
            v       , ipartr  , gbuf%mass,gresav  , &
            grth    , igrth   , gbuf%off, nc1     , &
            nc2     , x       , vr      , nel     , &
            igre)
            
          call r2tors( &
            gbuf%for(ii(1))   , gbuf%for(ii(2))   , gbuf%for(ii(3))   , gbuf%mom(ii(1))   , &
            gbuf%mom(ii(2))   , gbuf%mom(ii(3))   , tani              , h3d_data          , &
            nel)

          if (iparit == 0) then
            call r2cum3( &
              f       , gbuf%for(ii(1))   , gbuf%for(ii(2))   , gbuf%for(ii(3))   , &
              ar      , gbuf%mom(ii(1))   , gbuf%mom(ii(2))   , gbuf%mom(ii(3))   , &
              sti     , stir    , stifn   , stifr   , &
              fx1     , fx2     , fy1     , fy2     , &
              fz1     , fz2     , mx1     , mx2     , &
              my1     , my2     , mz1     , mz2     , &
              geo     , x1      , y1      , z1      , &
              x2      , y2      , z2      , iequil  , &
              exx     , eyx     , ezx     , exy     , &
              eyy     , ezy     , exz     , eyz     , &
              ezz     , nc1     , nc2     , nel     , &
              mtn)
          else
            call r2cum3p( &
              gbuf%for(ii(1))   , gbuf%for(ii(2))   , gbuf%for(ii(3))   , gbuf%mom(ii(1))   , &
              gbuf%mom(ii(2))   , gbuf%mom(ii(3))   , sti     , stir    , &
              fsky    , fsky    , iadr    , fx1     , &
              fx2     , fy1     , fy2     , fz1     , &
              fz2     , mx1     , mx2     , my1     , &
              my2     , mz1     , mz2     , geo     , &
              x1      , y1      , z1      , x2      , &
              y2      , z2      , iequil  , exx     , &
              eyx     , ezx     , exy     , eyy     , &
              ezy     , exz     , eyz     , ezz     , &
              nel     , nft     , mtn)
          end if

        end subroutine r23law135

      end module r23law135_mod