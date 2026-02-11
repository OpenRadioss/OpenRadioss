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
!||    s6zdefo3_mod   ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3       ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zdefo3_mod
      contains
!||====================================================================
!||    s6zdefo3        ../engine/source/elements/solid/solide6z/s6zdefo3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- uses       -----------------------------------------------------
!||    aleanim_mod     ../common_source/modules/aleanim_mod.F
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine s6zdefo3(                                                      &
          dxx      , dxy      , dxz      , dyx      ,                             &
          dyy      , dyz      , dzx      , dzy      ,                             &
          dzz      , d4       , d5       , d6       ,                             &
          dcxx     , dcxy     , dcxz     , dcyx     ,                             &
          dcyy     , dcyz     , dczx     , dczy     ,                             &
          dczz     ,                                                              &
          vol      , off      , offg     ,                                        &
          offs     , voldp    , nel      , dt1      ,                             &
          ismdisp  , iscau    ,                                                   &
          jcvt     , ISMSTR   , WXX      , WYY      ,                             &
          WZZ      , IMPL_S   ,IDYNA     , FANI_CELL,                             &
          NFT )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod
          USE ALEANIM_MOD, only : FANI_CELL_
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
          TYPE(FANI_CELL_), intent(inout) :: FANI_CELL       
          integer,                    intent(in)       :: nel       !< Number of elements
          INTEGER,                    INTENT(IN)       :: NFT
          integer,                    intent(in)       :: ismdisp
          integer,                    intent(in)       :: iscau
          integer,                    intent(in)       :: JCVT
          integer,                    intent(in)       :: ISMSTR
          integer,                    intent(in)       :: IMPL_S        
          integer,                    intent(in)       :: IDYNA          
          real(kind=WP),              intent(in)       :: dt1       !< Time step
          real(kind=wp), dimension(nel), intent(inout) :: dxx       !< Strain rate component XX
          real(kind=wp), dimension(nel), intent(inout) :: dxy       !< Strain rate component XY
          real(kind=wp), dimension(nel), intent(inout) :: dxz       !< Strain rate component XZ
          real(kind=wp), dimension(nel), intent(inout) :: dyx       !< Strain rate component YX
          real(kind=wp), dimension(nel), intent(inout) :: dyy       !< Strain rate component YY
          real(kind=wp), dimension(nel), intent(inout) :: dyz       !< Strain rate component YZ
          real(kind=wp), dimension(nel), intent(inout) :: dzx       !< Strain rate component ZX
          real(kind=wp), dimension(nel), intent(inout) :: dzy       !< Strain rate component ZY
          real(kind=wp), dimension(nel), intent(inout) :: dzz       !< Strain rate component ZZ

          real(kind=wp), dimension(nel), intent(inout) :: d4        !< Strain rate component 4
          real(kind=wp), dimension(nel), intent(inout) :: d5        !< Strain rate component 5
          real(kind=wp), dimension(nel), intent(inout) :: d6        !< Strain rate component 6

          real(kind=wp), dimension(nel), intent(in)    :: dcxx      !< Constant strain rate XX
          real(kind=wp), dimension(nel), intent(in)    :: dcxy      !< Constant strain rate XY
          real(kind=wp), dimension(nel), intent(in)    :: dcxz      !< Constant strain rate XZ
          real(kind=wp), dimension(nel), intent(in)    :: dcyx      !< Constant strain rate YX
          real(kind=wp), dimension(nel), intent(in)    :: dcyy      !< Constant strain rate YY
          real(kind=wp), dimension(nel), intent(in)    :: dcyz      !< Constant strain rate YZ
          real(kind=wp), dimension(nel), intent(in)    :: dczx      !< Constant strain rate ZX
          real(kind=wp), dimension(nel), intent(in)    :: dczy      !< Constant strain rate ZY
          real(kind=wp), dimension(nel), intent(in)    :: dczz      !< Constant strain rate ZZ
          real(kind=wp), dimension(nel), intent(inout) :: vol       !< Current volume
          real(kind=wp), dimension(nel), intent(inout) :: off       !< Element flag
          real(kind=wp), dimension(nel), intent(in)    :: offg      !< Global element flag
          real(kind=wp), dimension(nel), intent(in)    :: offs      !< Shell element flag
!C     ENSURE DOUBLE-PRECISION (64-BIT) FLOATING-POINT CALCULATIONS, EVEN WHEN COMPILING IN SINGLE-PRECISION MODE.
          real(kind=8), dimension(nel), intent(inout)  :: voldp     !< Double precision volume
          real(kind=WP), dimension(nel), intent(inout) :: wxx   !< Spin rate XX component
          real(kind=WP), dimension(nel), intent(inout) :: wyy   !< Spin rate YY component
          real(kind=WP), dimension(nel), intent(inout) :: wzz   !< Spin rate ZZ component
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i                                              !< Loop counter
          real(kind=wp) ::   dt1d2, dt1d1,FAC
          real(kind=wp) ::   exx, eyy, ezz, exy, eyx, exz, ezx, eyz, ezy
          real(kind=wp) ::   AAA, DT1D
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================
!          tol = one - em20
          do i = 1, nel
!  vol(i) = half * wi * (volg(i) + vzl(i) * zi)
            vol(i) = voldp(i)
            off(i) = offg(i)
            if (vol(i) <= zero) then
              vol(i) = em20
              off(i) = zero
            elseif(off(i) == zero .or. offs(i) == two .or. ismdisp > 0) then
              voldp(i) = max(em20, voldp(i))
              vol(i) = max(em20, vol(i))
            endif
          enddo



          dt1d1 = dt1
          if (ismdisp > 0 .and. iscau == 0) dt1d1 = zero !!???

          

    
          do i = 1, nel     
            dxx(i) = dcxx(i)
            dyy(i) = dcyy(i)
            dzz(i) = dczz(i)
            dxy(i) = dcxy(i)
            dyx(i) = dcyx(i)
            dzx(i) = dczx(i)
            dzy(i) = dczy(i)
            dxz(i) = dcxz(i)
            dyz(i) = dcyz(i)
          enddo

          DT1D2=HALF*DT1
 !         IF(ISCAU>0 .OR. IMP_LR>0)DT1D2=DT1


      IF(JCVT /= 0)THEN !JCVT = 1 OR 2 IN PENTA SOLID
        IF(ISMSTR == 11)THEN
          DO I=1,NEL
            D4(I)=DXY(I)+DYX(I)
            D5(I)=DYZ(I)+DZY(I)
            D6(I)=DXZ(I)+DZX(I)
            WZZ(I)=DT1D2*(DYX(I)-DXY(I))
            WYY(I)=DT1D2*(DXZ(I)-DZX(I))
            WXX(I)=DT1D2*(DZY(I)-DYZ(I))
          ENDDO
        ELSE
          DO I=1,NEL
            WXX(I)=ZERO
            WYY(I)=ZERO
            WZZ(I)=ZERO
          ENDDO
          IF (IMPL_S == 0 .OR. (IDYNA > 0 .AND. ISMDISP == 0)) THEN
            DO I=1,NEL
             EXX=DXX(I)
             EYY=DYY(I)
             EZZ=DZZ(I)
             EXY=DXY(I)
             EYX=DYX(I)
             EXZ=DXZ(I)
             EZX=DZX(I)
             EYZ=DYZ(I)
             EZY=DZY(I)
             DXX(I)=DXX(I)-DT1D2*(EXX*EXX+EYX*EYX+EZX*EZX)
             DYY(I)=DYY(I)-DT1D2*(EYY*EYY+EZY*EZY+EXY*EXY)
             DZZ(I)=DZZ(I)-DT1D2*(EZZ*EZZ+EXZ*EXZ+EYZ*EYZ)
             AAA=DT1D2*(EXX*EXY+EYX*EYY+EZX*EZY)
             DXY(I)=DXY(I)-AAA
             DYX(I)=DYX(I)-AAA
             D4(I)=DXY(I)+DYX(I)
             AAA=DT1D2*(EYY*EYZ+EZY*EZZ+EXY*EXZ)
             DYZ(I)=DYZ(I)-AAA
             DZY(I)=DZY(I)-AAA
             D5(I)=DYZ(I)+DZY(I)
             AAA=DT1D2*(EZZ*EZX+EXZ*EXX+EYZ*EYX)
             DXZ(I)=DXZ(I)-AAA
             DZX(I)=DZX(I)-AAA
             D6(I)=DXZ(I)+DZX(I)
            ENDDO
          ELSEIF (ISMDISP>0.AND.ISCAU==0) THEN
          !---------implicit static---------            
            DO I=1,NEL
             D4(I) = DXY(I)+DYX(I)
             D5(I) = DYZ(I)+DZY(I)
             D6(I) = DXZ(I)+DZX(I)
            ENDDO
          ELSE
            DT1D=TWO*DT1D2
            DO I=1,NEL
             D4(I)=DXY(I)+DYX(I)-DT1D*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
             D5(I)=DYZ(I)+DZY(I)-DT1D*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
             D6(I)=DXZ(I)+DZX(I)-DT1D*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
             DXX(I)=DXX(I)-DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
             DYY(I)=DYY(I)-DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
             DZZ(I)=DZZ(I)-DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
            ENDDO
          ENDIF ! IF (IMPL_S==0.OR.IDYNA>0)
        END IF !(ISMSTR == 11) THEN

      ENDIF

      
      !VORTICITY OUTPUT /ANIM/ELEM/VORTX,VORTY,VORTZ
      IF(DT1/=ZERO)THEN
        FAC = FOUR/DT1
        IF(FANI_CELL%IS_VORT_X_REQUESTED)THEN
          DO I=1,NEL
            FANI_CELL%VORT_X(I+NFT) = FAC*WXX(I)
          ENDDO
        ENDIF
        IF(FANI_CELL%IS_VORT_Y_REQUESTED)THEN
          DO I=1,NEL
            FANI_CELL%VORT_Y(I+NFT) = FAC*WYY(I)
          ENDDO
        ENDIF
        IF(FANI_CELL%IS_VORT_Z_REQUESTED)THEN
          DO I=1,NEL
            FANI_CELL%VORT_Z(I+NFT) = FAC*WZZ(I)
          ENDDO
        ENDIF
      ENDIF          

        end subroutine s6zdefo3
      end module s6zdefo3_mod
