Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    fail_windshield_init      ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||--- called by ------------------------------------------------------
      !||    c3init3                   ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbainit3                  ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cinit3                    ../starter/source/elements/shell/coque/cinit3.F
      !||--- calls      -----------------------------------------------------
      !||    brokmann_crack_init       ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||    crack_depth_init          ../starter/source/materials/fail/windshield_alter/crack_depth_init.F90
      !||    ngr2usrn                  ../starter/source/system/nintrr.F
      !||--- uses       -----------------------------------------------------
      !||    brokmann_crack_init_mod   ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||    crack_depth_init_mod      ../starter/source/materials/fail/windshield_alter/crack_depth_init.F90
      !||====================================================================
      SUBROUTINE FAIL_WINDSHIELD_INIT(ELBUF_STR,MAT_PARAM,FAIL_BROKMANN,
     .           NEL      ,NFT       ,ITY      ,IGRSH4N  ,IGRSH3N  ,
     .           ALDT     ,THK       ,NGL      )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MATPARAM_DEF_MOD
      USE GROUPDEF_MOD
      USE CRACK_DEPTH_INIT_MOD
      USE BROKMANN_CRACK_INIT_MOD
      use brokmann_random_def_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include       "param_c.inc"
#include       "com04_c.inc" 
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NEL,NFT,ITY
      INTEGER, DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real, DIMENSION(NEL) ,INTENT(IN) :: ALDT
      my_real, DIMENSION(NEL) ,INTENT(IN) :: THK
      TYPE (ELBUF_STRUCT_) :: ELBUF_STR
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
      TYPE (FAIL_BROKMANN_) ,INTENT(IN) :: FAIL_BROKMANN
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER I,II,KK,IEL,ID,IL,IR,IS,IT,NPTR,NPTS,NPTT,IFL,ITGLASS,ELNUM,
     .   IGR,IGRID,NUMEL,IMAT,IRUPT,NFAIL,NUPARAM,NUVAR,NINDX,IVAR,NUM
      INTEGER TAGSH(MAX(NUMELC,NUMELTG)),TAGEL(NEL),INDX(NEL)
      EXTERNAL  NGR2USRN
      INTEGER   NGR2USRN
!-----------------------------------------------------------------------
!     Initialization of failure UVAR table => edge element flag for /FAIL/ALTER
!=======================================================================
      DO IL=1, ELBUF_STR%NLAY
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        NPTR  = ELBUF_STR%NPTR
        NPTS  = ELBUF_STR%NPTS
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
        IMAT  = ELBUF_STR%BUFLY(IL)%IMAT
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
          DO IFL = 1,NFAIL
            IRUPT = MAT_PARAM(IMAT)%FAIL(IFL)%IRUPT
            IF (IRUPT == 28) THEN   ! windshield failure model
              NUPARAM = MAT_PARAM(IMAT)%FAIL(IFL)%NUPARAM
              NUVAR   = MAT_PARAM(IMAT)%FAIL(IFL)%NUVAR
              ITGLASS = NINT(MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM(22))
!
              IVAR = NEL*(10-1)     ! edge element flag = UVAR(10)
              IGR  = 0
              IF (ITY == 3) THEN
                KK = NGRNOD + NGRBRIC + NGRQUAD
                IGRID = MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM(12)
                MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM(11) = ONE / SQRT(PI)  ! GEORED for underintegrated 4N shells
                IF (IGRID > 0) IGR = NGR2USRN(IGRID,IGRSH4N,NGRSHEL,NUM)
                TAGSH(1:NUMELC)  = 0
              ELSEIF (ITY == 7) THEN
                IGRID = MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM(13)
                IF (IGRID > 0) IGR = NGR2USRN(IGRID,IGRSH3N,NGRSH3N,NUM)
                TAGSH(1:NUMELTG) = 0
              ENDIF
c
              IF (IGR > 0) THEN
                TAGEL(1:NEL) = 0
                IF (ITY == 3) THEN
                  NUMEL = IGRSH4N(IGR)%NENTITY
                  DO IEL=1,NUMEL
                    II = IGRSH4N(IGR)%ENTITY(IEL)
                    TAGSH(II) = 1
                  ENDDO
                ELSEIF (ITY == 7) THEN
                  NUMEL = IGRSH3N(IGR)%NENTITY
                  DO IEL=1,NUMEL
                    II = IGRSH3N(IGR)%ENTITY(IEL)
                    TAGSH(II) = 1
                  ENDDO
                ENDIF ! IF (ITY == 3)
                NINDX = 0
                DO I=1,NEL
                  IF (TAGSH(I+NFT) == 1) THEN
                    NINDX = NINDX + 1
                    INDX(NINDX) = I
                  ENDIF
                ENDDO
                DO II = 1,NINDX
                  I = INDX(II)            
                  ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(IFL)%VAR(IVAR + I) = 1
                ENDDO
              ENDIF                
!
            END IF               
          END DO    !  IFL = 1,NFAIL              
        END DO      !  IT=1,NPTT
        END DO      !  IT=1,NPTS
        END DO      !  IT=1,NPTR
      END DO        !  IL=1,NLAY
!-----------------------------------------------------------------------------
!     crack depth initialization depending on element and Gauss point position
!-----------------------------------------------------------------------------
      DO IL=1, ELBUF_STR%NLAY
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        NPTR  = ELBUF_STR%NPTR
        NPTS  = ELBUF_STR%NPTS
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
        IMAT  = ELBUF_STR%BUFLY(IL)%IMAT
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
          DO IFL = 1,NFAIL
            IRUPT = MAT_PARAM(IMAT)%FAIL(IFL)%IRUPT
            IF (IRUPT == 28) THEN   ! windshield failure model
              NUPARAM = MAT_PARAM(IMAT)%FAIL(IFL)%NUPARAM
              NUVAR   = MAT_PARAM(IMAT)%FAIL(IFL)%NUVAR
              ITGLASS = NINT(MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM(22))
!
              !  Ch.Alter criterion - crack depth initialization 
!
              CALL CRACK_DEPTH_INIT(NEL   ,IT  ,NPTT ,NUPARAM ,NUVAR,
     .                              MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM,
     .                              ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(IFL)%VAR,
     .                              ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(IFL)%DAMMX,
     .                              ELBUF_STR%GBUF%DMG  )
!
              !  Ch.Brokmann criterion - random crack initialization
!
              IF (ITGLASS == 1 .and. (IT == 1 .or. IT == NPTT)) THEN
                DO II = 1,FAIL_BROKMANN%NFAIL
                  IF (FAIL_BROKMANN%BROKMANN(II)%IMAT == IMAT) THEN
                    ! tag elements of current group in brokmann element list
                    nindx = 0
                    indx(1:nel) = 0
                    do kk = 1,fail_brokmann%brokmann(ii)%nelem
                      id = fail_brokmann%brokmann(ii)%brokmann_elem(kk)%id
                      do iel=1,nel
                        if (id == ngl(iel)) then
                          nindx = nindx + 1
                          indx(nindx) = kk
                        end if
                      end do
                    end do
!
                    if (nindx > 0) then 
                      CALL BROKMANN_CRACK_INIT(NEL  ,IT   ,NPTT ,NUPARAM ,NUVAR,
     .                     MAT_PARAM(IMAT)%FAIL(IFL)%UPARAM    ,FAIL_BROKMANN%BROKMANN(II),
     .                     ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(IFL)%VAR,
     .                     INDX    ,THK      ,ALDT      ,NGL   )
                    end if 
!
                  END IF
                END DO
              END IF        ! END Brokmann criterion initialization
!
            END IF  !  windshield failure
          END DO    !  IFL = 1,NFAIL              
        END DO      !  IT=1,NPTT
        END DO      !  IT=1,NPTS
        END DO      !  IT=1,NPTR
      END DO        !  IL=1,NLAY
!-----------
      RETURN
      END
