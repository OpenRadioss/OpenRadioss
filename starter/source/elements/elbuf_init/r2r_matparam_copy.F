Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    r2r_matparam_copy    ../starter/source/elements/elbuf_init/r2r_matparam_copy.F
      !||--- called by ------------------------------------------------------
      !||    lectur               ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE R2R_MATPARAM_COPY(MATPARAM_TAB, MATPARAM_INI, NUMMAT0  ,NUMMAT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE MULTIMAT_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMMAT0,NUMMAT
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT ) :: MATPARAM_TAB
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT0) :: MATPARAM_INI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFAIL,NUPARAM,NIPARAM,NFAIL,IMAT,NBMAT
C=======================================================================
c     Copy matparam from standard materials
c
      DO I = 1,NUMMAT0
        NUPARAM = MATPARAM_INI(I)%NUPARAM
        NIPARAM = MATPARAM_INI(I)%NIPARAM
        NFAIL   = MATPARAM_INI(I)%NFAIL
        NBMAT   = MATPARAM_INI(I)%MULTIMAT%NB
        MATPARAM_TAB(I)%ILAW               = MATPARAM_INI(I)%ILAW
        MATPARAM_TAB(I)%MAT_ID             = MATPARAM_INI(I)%MAT_ID
        MATPARAM_TAB(I)%NUPARAM            = NUPARAM
        MATPARAM_TAB(I)%NIPARAM            = NIPARAM
        MATPARAM_TAB(I)%NFUNC              = MATPARAM_INI(I)%NFUNC
        MATPARAM_TAB(I)%NTABLE             = MATPARAM_INI(I)%NTABLE
        MATPARAM_TAB(I)%NSUBMAT            = MATPARAM_INI(I)%NSUBMAT
        MATPARAM_TAB(I)%NFAIL              = NFAIL
        MATPARAM_TAB(I)%IVISC              = MATPARAM_INI(I)%IVISC
        MATPARAM_TAB(I)%IEOS               = MATPARAM_INI(I)%IEOS
        MATPARAM_TAB(I)%ITHERM             = MATPARAM_INI(I)%ITHERM
        MATPARAM_TAB(I)%IEXPAN             = MATPARAM_INI(I)%IEXPAN
        MATPARAM_TAB(I)%IALE               = MATPARAM_INI(I)%IALE
        MATPARAM_TAB(I)%ITURB              = MATPARAM_INI(I)%ITURB
        MATPARAM_TAB(I)%HEAT_FLAG          = MATPARAM_INI(I)%HEAT_FLAG
        MATPARAM_TAB(I)%COMPRESSIBILITY    = MATPARAM_INI(I)%COMPRESSIBILITY
        MATPARAM_TAB(I)%SMSTR              = MATPARAM_INI(I)%SMSTR
        MATPARAM_TAB(I)%STRAIN_FORMULATION = MATPARAM_INI(I)%STRAIN_FORMULATION
        MATPARAM_TAB(I)%IPRES              = MATPARAM_INI(I)%IPRES
        MATPARAM_TAB(I)%ORTHOTROPY         = MATPARAM_INI(I)%ORTHOTROPY
        MATPARAM_TAB(I)%NLOC               = MATPARAM_INI(I)%NLOC
        MATPARAM_TAB(I)%IFAILWAVE          = MATPARAM_INI(I)%IFAILWAVE
        MATPARAM_TAB(I)%IXFEM              = MATPARAM_INI(I)%IXFEM
c
        MATPARAM_TAB(I)%VISC%ILAW          = MATPARAM_INI(I)%VISC%ILAW
        MATPARAM_TAB(I)%VISC%NUPARAM       = MATPARAM_INI(I)%VISC%NUPARAM
        MATPARAM_TAB(I)%VISC%NIPARAM       = MATPARAM_INI(I)%VISC%NIPARAM
        MATPARAM_TAB(I)%VISC%NUVAR         = MATPARAM_INI(I)%VISC%NUVAR
        MATPARAM_TAB(I)%VISC%NFUNC         = MATPARAM_INI(I)%VISC%NFUNC
        MATPARAM_TAB(I)%VISC%NTABLE        = MATPARAM_INI(I)%VISC%NTABLE
c
        MATPARAM_TAB(I)%PROP_SOLID         = MATPARAM_INI(I)%PROP_SOLID
        MATPARAM_TAB(I)%PROP_SHELL         = MATPARAM_INI(I)%PROP_SHELL
        MATPARAM_TAB(I)%PROP_BEAM          = MATPARAM_INI(I)%PROP_BEAM
        MATPARAM_TAB(I)%PROP_SPRING        = MATPARAM_INI(I)%PROP_SPRING
        MATPARAM_TAB(I)%PROP_TRUSS         = MATPARAM_INI(I)%PROP_TRUSS
        MATPARAM_TAB(I)%PROP_SPH           = MATPARAM_INI(I)%PROP_SPH
c
        MATPARAM_TAB(I)%RHO                = MATPARAM_INI(I)%RHO
        MATPARAM_TAB(I)%RHO0               = MATPARAM_INI(I)%RHO0
        MATPARAM_TAB(I)%YOUNG              = MATPARAM_INI(I)%YOUNG
        MATPARAM_TAB(I)%SHEAR              = MATPARAM_INI(I)%SHEAR
        MATPARAM_TAB(I)%BULK               = MATPARAM_INI(I)%BULK
        MATPARAM_TAB(I)%NU                 = MATPARAM_INI(I)%NU
        MATPARAM_TAB(I)%STIFF_CONTACT      = MATPARAM_INI(I)%STIFF_CONTACT
        MATPARAM_TAB(I)%STIFF_HGLASS       = MATPARAM_INI(I)%STIFF_HGLASS
        MATPARAM_TAB(I)%STIFF_TSTEP        = MATPARAM_INI(I)%STIFF_TSTEP
c
        MATPARAM_TAB(I)%COMPATIBILITY_EOS  = MATPARAM_INI(I)%COMPATIBILITY_EOS
c
        ALLOCATE(MATPARAM_TAB(I)%UPARAM(NUPARAM))
        IF (NUPARAM > 0) THEN
          MATPARAM_TAB(I)%UPARAM(1:NUPARAM) = MATPARAM_INI(I)%UPARAM(1:NUPARAM)
        END IF
!
        ALLOCATE(MATPARAM_TAB(I)%IPARAM(NIPARAM))
        IF (NIPARAM > 0) THEN
          MATPARAM_TAB(I)%IPARAM(1:NIPARAM) = MATPARAM_INI(I)%IPARAM(1:NIPARAM)
        END IF

        ALLOCATE(MATPARAM_TAB(I)%FAIL(NFAIL))
        IF (NFAIL > 0) THEN
          DO IFAIL=1,NFAIL
            MATPARAM_TAB(I)%FAIL(IFAIL)%KEYWORD = MATPARAM_INI(I)%FAIL(IFAIL)%KEYWORD
            MATPARAM_TAB(I)%FAIL(IFAIL)%IRUPT   = MATPARAM_INI(I)%FAIL(IFAIL)%IRUPT
            MATPARAM_TAB(I)%FAIL(IFAIL)%FAIL_ID = MATPARAM_INI(I)%FAIL(IFAIL)%FAIL_ID
            MATPARAM_TAB(I)%FAIL(IFAIL)%NUPARAM = MATPARAM_INI(I)%FAIL(IFAIL)%NUPARAM
            MATPARAM_TAB(I)%FAIL(IFAIL)%NIPARAM = MATPARAM_INI(I)%FAIL(IFAIL)%NIPARAM
            MATPARAM_TAB(I)%FAIL(IFAIL)%NUVAR   = MATPARAM_INI(I)%FAIL(IFAIL)%NUVAR
            MATPARAM_TAB(I)%FAIL(IFAIL)%NFUNC   = MATPARAM_INI(I)%FAIL(IFAIL)%NFUNC
            MATPARAM_TAB(I)%FAIL(IFAIL)%NTABLE  = MATPARAM_INI(I)%FAIL(IFAIL)%NTABLE
            MATPARAM_TAB(I)%FAIL(IFAIL)%NMOD    = MATPARAM_INI(I)%FAIL(IFAIL)%NMOD
            MATPARAM_TAB(I)%FAIL(IFAIL)%FAIL_IP = MATPARAM_INI(I)%FAIL(IFAIL)%FAIL_IP
            MATPARAM_TAB(I)%FAIL(IFAIL)%PTHK    = MATPARAM_INI(I)%FAIL(IFAIL)%PTHK
            ALLOCATE(MATPARAM_TAB(I)%FAIL(IFAIL)%UPARAM(MATPARAM_TAB(I)%FAIL(IFAIL)%NUPARAM))
            DO J=1,MATPARAM_TAB(I)%FAIL(IFAIL)%NUPARAM
              MATPARAM_TAB(I)%FAIL(IFAIL)%UPARAM(J) = MATPARAM_INI(I)%FAIL(IFAIL)%UPARAM(J)
            END DO
            ALLOCATE(MATPARAM_TAB(I)%FAIL(IFAIL)%IPARAM(MATPARAM_TAB(I)%FAIL(IFAIL)%NIPARAM))
            DO J=1,MATPARAM_TAB(I)%FAIL(IFAIL)%NIPARAM
              MATPARAM_TAB(I)%FAIL(IFAIL)%IPARAM(J) = MATPARAM_INI(I)%FAIL(IFAIL)%IPARAM(J)
            END DO
            ALLOCATE(MATPARAM_TAB(I)%FAIL(IFAIL)%IFUNC(MATPARAM_TAB(I)%FAIL(IFAIL)%NFUNC))
            DO J=1,MATPARAM_TAB(I)%FAIL(IFAIL)%NFUNC
              MATPARAM_TAB(I)%FAIL(IFAIL)%IFUNC(J) = MATPARAM_INI(I)%FAIL(IFAIL)%IFUNC(J)
            END DO
            ALLOCATE(MATPARAM_TAB(I)%FAIL(IFAIL)%TABLE(MATPARAM_TAB(I)%FAIL(IFAIL)%NTABLE))
            DO J=1,MATPARAM_TAB(I)%FAIL(IFAIL)%NTABLE
              MATPARAM_TAB(I)%FAIL(IFAIL)%TABLE(J) = MATPARAM_INI(I)%FAIL(IFAIL)%TABLE(J)
            END DO
          END DO  ! NFAIL
        ENDIF
c
        ALLOCATE(MATPARAM_TAB(I)%MULTIMAT%MID(NBMAT))
        ALLOCATE(MATPARAM_TAB(I)%MULTIMAT%VFRAC(NBMAT))
        IF (NBMAT > 0) THEN
          DO IMAT=1,NBMAT
            MATPARAM_TAB(I)%MULTIMAT%MID(IMAT)   = MATPARAM_INI(I)%MULTIMAT%MID(IMAT)
            MATPARAM_TAB(I)%MULTIMAT%VFRAC(IMAT) = MATPARAM_INI(I)%MULTIMAT%VFRAC(IMAT)
          END DO  ! IMAT
        ENDIF
!
        IF (MATPARAM_TAB(I)%ITHERM > 0 .or. MATPARAM_TAB(I)%IEXPAN > 0) THEN
          MATPARAM_TAB(I)%THERM%iform       = MATPARAM_INI(I)%THERM%iform      
          MATPARAM_TAB(I)%THERM%tini        = MATPARAM_INI(I)%THERM%tini         
          MATPARAM_TAB(I)%THERM%tref        = MATPARAM_INI(I)%THERM%tref         
          MATPARAM_TAB(I)%THERM%tmelt       = MATPARAM_INI(I)%THERM%tmelt      
          MATPARAM_TAB(I)%THERM%rhocp       = MATPARAM_INI(I)%THERM%rhocp      
          MATPARAM_TAB(I)%THERM%as          = MATPARAM_INI(I)%THERM%as         
          MATPARAM_TAB(I)%THERM%bs          = MATPARAM_INI(I)%THERM%bs         
          MATPARAM_TAB(I)%THERM%al          = MATPARAM_INI(I)%THERM%al         
          MATPARAM_TAB(I)%THERM%bl          = MATPARAM_INI(I)%THERM%bl         
          MATPARAM_TAB(I)%THERM%efrac       = MATPARAM_INI(I)%THERM%efrac      
          MATPARAM_TAB(I)%THERM%func_thexp  = MATPARAM_INI(I)%THERM%func_thexp 
          MATPARAM_TAB(I)%THERM%scale_thexp = MATPARAM_INI(I)%THERM%scale_thexp
        END IF
      ENDDO
!
        ! copy of matparam between subdomains must be completed with further evolution of MATPARAM structure
c
c     Fill matparam for additional rad2rad void materials
c
      DO I = NUMMAT0+1,NUMMAT
        MATPARAM_TAB(I)%ILAW               = 0
        MATPARAM_TAB(I)%MAT_ID             = 0
        MATPARAM_TAB(I)%NUPARAM            = 0
        MATPARAM_TAB(I)%NIPARAM            = 0
        MATPARAM_TAB(I)%NFUNC              = 0
        MATPARAM_TAB(I)%NTABLE             = 0
        MATPARAM_TAB(I)%NSUBMAT            = 0
        MATPARAM_TAB(I)%NFAIL              = 0
        MATPARAM_TAB(I)%IVISC              = 0
        MATPARAM_TAB(I)%IEOS               = 0
        MATPARAM_TAB(I)%ITHERM             = 0
        MATPARAM_TAB(I)%IEXPAN             = 0
        MATPARAM_TAB(I)%IALE               = 0
        MATPARAM_TAB(I)%ITURB              = 0
        MATPARAM_TAB(I)%HEAT_FLAG          = 0
        MATPARAM_TAB(I)%COMPRESSIBILITY    = 0
        MATPARAM_TAB(I)%SMSTR              = 0
        MATPARAM_TAB(I)%STRAIN_FORMULATION = 0
        MATPARAM_TAB(I)%IPRES              = 0
        MATPARAM_TAB(I)%ORTHOTROPY         = 0
        MATPARAM_TAB(I)%NLOC               = 0
        MATPARAM_TAB(I)%IFAILWAVE          = 0
        MATPARAM_TAB(I)%IXFEM              = 0
c
        MATPARAM_TAB(I)%VISC%ILAW          = 0
        MATPARAM_TAB(I)%VISC%NUPARAM       = 0
        MATPARAM_TAB(I)%VISC%NIPARAM       = 0
        MATPARAM_TAB(I)%VISC%NUVAR         = 0
        MATPARAM_TAB(I)%VISC%NFUNC         = 0
        MATPARAM_TAB(I)%VISC%NTABLE        = 0
c
        MATPARAM_TAB(I)%PROP_SOLID         = 1
        MATPARAM_TAB(I)%PROP_SHELL         = 1
        MATPARAM_TAB(I)%PROP_BEAM          = 3
        MATPARAM_TAB(I)%PROP_SPRING        = 2
        MATPARAM_TAB(I)%PROP_TRUSS         = 1
        MATPARAM_TAB(I)%PROP_SPH           = 1
        MATPARAM_TAB(I)%COMPATIBILITY_EOS  = 0
c
        MATPARAM_TAB(I)%MULTIMAT%NB        = 0
c
        MATPARAM_TAB(I)%RHO                = ZERO
        MATPARAM_TAB(I)%RHO0               = ZERO
        MATPARAM_TAB(I)%YOUNG              = ZERO
        MATPARAM_TAB(I)%SHEAR              = ZERO
        MATPARAM_TAB(I)%BULK               = ZERO
        MATPARAM_TAB(I)%NU                 = ZERO
        MATPARAM_TAB(I)%STIFF_CONTACT      = ZERO
        MATPARAM_TAB(I)%STIFF_HGLASS       = ZERO
        MATPARAM_TAB(I)%STIFF_TSTEP        = ZERO
      ENDDO
c-----------------------------------------------
      RETURN
      END
