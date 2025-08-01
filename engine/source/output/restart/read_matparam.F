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
!||    read_matparam                 ../engine/source/output/restart/read_matparam.F
!||--- called by ------------------------------------------------------
!||    rdresb                        ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_ale_rezoning_param       ../engine/source/output/restart/read_ale_rezoning_param.F90
!||    read_c_c                      ../common_source/tools/input_output/write_routtines.c
!||    read_db                       ../common_source/tools/input_output/read_db.F
!||    read_eosparam                 ../engine/source/output/restart/read_eosparam.F90
!||    read_failparam                ../engine/source/output/restart/read_failparam.F
!||    read_i_c                      ../common_source/tools/input_output/write_routtines.c
!||    read_mat_table                ../engine/source/materials/tools/read_mat_table.F
!||    read_thermparam               ../engine/source/output/restart/read_thermparam.F90
!||    read_viscparam                ../engine/source/output/restart/read_viscparam.F
!||--- uses       -----------------------------------------------------
!||    mat_elem_mod                  ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    message_mod                   ../engine/share/message_module/message_mod.F
!||    names_and_titles_mod          ../common_source/modules/names_and_titles_mod.F
!||    read_ale_rezoning_param_mod   ../engine/source/output/restart/read_ale_rezoning_param.F90
!||    read_eosparam_mod             ../engine/source/output/restart/read_eosparam.F90
!||    read_therpmaram_mod           ../engine/source/output/restart/read_thermparam.F90
!||====================================================================
      SUBROUTINE READ_MATPARAM(MAT_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MAT_ELEM_MOD
      USE NAMES_AND_TITLES_MOD
      USE read_eosparam_mod
      use read_therpmaram_mod
      use read_ale_rezoning_param_mod , only : read_ale_rezoning_param
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IMAT,NUMMAT,NUMTABL,NUPARAM,NIPARAM,NFAIL,ITHERM
      INTEGER :: IAD,NFIX,NFIXR,LEN,LENR,NMOD,MOD
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
      INTEGER :: NBSUBMAT
      my_real ,DIMENSION(:) ,ALLOCATABLE :: RBUF
c=======================================================================
c     read MATPARAM data
      NFIX   = 24
      NUMMAT = MAT_ELEM%NUMMAT
      ALLOCATE(MAT_ELEM%MAT_PARAM(NUMMAT))
      DO IMAT=1,NUMMAT
        CALL MAT_ELEM%MAT_PARAM(IMAT)%ZEROING
      ENDDO

      LEN = NFIX*NUMMAT
      ALLOCATE (IBUF(LEN) )
      CALL READ_I_C(IBUF, LEN)

      ! write integer parameters and flags
      IAD = 0
      DO IMAT = 1,NUMMAT
        MAT_ELEM%MAT_PARAM(IMAT)%ILAW               = IBUF(IAD+ 1)
        MAT_ELEM%MAT_PARAM(IMAT)%MAT_ID             = IBUF(IAD+ 2)
        MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM            = IBUF(IAD+ 3)
        MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM            = IBUF(IAD+ 4)
        MAT_ELEM%MAT_PARAM(IMAT)%NFUNC              = IBUF(IAD+ 5)
        MAT_ELEM%MAT_PARAM(IMAT)%NTABLE             = IBUF(IAD+ 6)
        MAT_ELEM%MAT_PARAM(IMAT)%NSUBMAT            = IBUF(IAD+ 7)
        MAT_ELEM%MAT_PARAM(IMAT)%NFAIL              = IBUF(IAD+ 8)
        MAT_ELEM%MAT_PARAM(IMAT)%IVISC              = IBUF(IAD+ 9)
        MAT_ELEM%MAT_PARAM(IMAT)%IEOS               = IBUF(IAD+10)
        MAT_ELEM%MAT_PARAM(IMAT)%ITHERM             = IBUF(IAD+11)
        MAT_ELEM%MAT_PARAM(IMAT)%IEXPAN             = IBUF(IAD+12)
        MAT_ELEM%MAT_PARAM(IMAT)%IALE               = IBUF(IAD+13)
        MAT_ELEM%MAT_PARAM(IMAT)%ITURB              = IBUF(IAD+14)
        MAT_ELEM%MAT_PARAM(IMAT)%HEAT_FLAG          = IBUF(IAD+15)
        MAT_ELEM%MAT_PARAM(IMAT)%COMPRESSIBILITY    = IBUF(IAD+16)
        MAT_ELEM%MAT_PARAM(IMAT)%SMSTR              = IBUF(IAD+17)
        MAT_ELEM%MAT_PARAM(IMAT)%STRAIN_FORMULATION = IBUF(IAD+18)
        MAT_ELEM%MAT_PARAM(IMAT)%IPRES              = IBUF(IAD+19)
        MAT_ELEM%MAT_PARAM(IMAT)%ORTHOTROPY         = IBUF(IAD+20)
        MAT_ELEM%MAT_PARAM(IMAT)%NLOC               = IBUF(IAD+21)
        MAT_ELEM%MAT_PARAM(IMAT)%IFAILWAVE          = IBUF(IAD+22)
        MAT_ELEM%MAT_PARAM(IMAT)%IXFEM              = IBUF(IAD+23)
        MAT_ELEM%MAT_PARAM(IMAT)%NMOD               = IBUF(IAD+24)
        IAD = IAD + NFIX
      END DO
      DEALLOCATE( IBUF )

      ! read real parameters
      NFIXR = 9
      LENR  = NFIXR*NUMMAT
      ALLOCATE (RBUF(LENR) )
      CALL READ_DB (RBUF ,LENR)
      IAD   = 0
      DO IMAT = 1,NUMMAT
        MAT_ELEM%MAT_PARAM(IMAT)%RHO           = RBUF(IAD + 1)
        MAT_ELEM%MAT_PARAM(IMAT)%RHO0          = RBUF(IAD + 2)
        MAT_ELEM%MAT_PARAM(IMAT)%YOUNG         = RBUF(IAD + 3)
        MAT_ELEM%MAT_PARAM(IMAT)%BULK          = RBUF(IAD + 4)
        MAT_ELEM%MAT_PARAM(IMAT)%SHEAR         = RBUF(IAD + 5)
        MAT_ELEM%MAT_PARAM(IMAT)%NU            = RBUF(IAD + 6)
        MAT_ELEM%MAT_PARAM(IMAT)%STIFF_CONTACT = RBUF(IAD + 7)
        MAT_ELEM%MAT_PARAM(IMAT)%STIFF_HGLASS  = RBUF(IAD + 8)
        MAT_ELEM%MAT_PARAM(IMAT)%STIFF_TSTEP   = RBUF(IAD + 9)
        IAD = IAD + NFIXR
      END DO
      DEALLOCATE (RBUF)

      ! read material title
      DO IMAT = 1,NUMMAT      
        CALL READ_C_C(NAME,NCHARTITLE)
        DO I=1,NCHARTITLE
          MAT_ELEM%MAT_PARAM(IMAT)%TITLE(I:I) = CHAR(NAME(I))
        END DO
      END DO

      ! read material parameter array
      DO IMAT = 1,NUMMAT      
        NUPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM
        NIPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM
        ALLOCATE(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM(NUPARAM))
        ALLOCATE(MAT_ELEM%MAT_PARAM(IMAT)%IPARAM(NIPARAM))
        MAT_ELEM%MAT_PARAM(IMAT)%UPARAM = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%IPARAM = 0
        IF (NUPARAM > 0) THEN
          CALL READ_DB(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM ,NUPARAM)
        END IF      
        IF (NIPARAM > 0) THEN
          CALL READ_I_C(MAT_ELEM%MAT_PARAM(IMAT)%IPARAM ,NIPARAM)
        END IF      
      END DO

      ! read material law tables
      DO IMAT = 1,NUMMAT
        NUMTABL  = MAT_ELEM%MAT_PARAM(IMAT)%NTABLE        
        IF (NUMTABL > 0) THEN
          ALLOCATE (MAT_ELEM%MAT_PARAM(IMAT)%TABLE(NUMTABL))
          CALL READ_MAT_TABLE(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL)
        END IF
      END DO

      ! read viscosity model parameters
      DO IMAT = 1,NUMMAT
        IF (MAT_ELEM%MAT_PARAM(IMAT)%IVISC > 0) THEN
          CALL READ_VISCPARAM(MAT_ELEM%MAT_PARAM(IMAT)%VISC)
        END IF
      END DO      
c           
c     read thermal parameters
c
      DO IMAT = 1,NUMMAT
        ITHERM = MAT_ELEM%MAT_PARAM(IMAT)%ITHERM
        IF (ITHERM > 0) THEN
          CALL READ_THERMPARAM(MAT_ELEM%MAT_PARAM(IMAT)%THERM)
        END IF
      END DO      
c      
c     read parameters of failure models per material
c          
      DO IMAT = 1,NUMMAT
        NFAIL = MAT_ELEM%MAT_PARAM(IMAT)%NFAIL        
        IF (NFAIL > 0) THEN
          ALLOCATE (MAT_ELEM%MAT_PARAM(IMAT)%FAIL(NFAIL))
          DO I = 1,NFAIL
            CALL READ_FAILPARAM(MAT_ELEM%MAT_PARAM(IMAT)%FAIL(I))
          END DO      
        END IF
      END DO

      ! read damage modes
      DO IMAT = 1,NUMMAT
        NMOD = MAT_ELEM%MAT_PARAM(IMAT)%NMOD
        IF (NMOD > 0) THEN
          ALLOCATE(MAT_ELEM%MAT_PARAM(IMAT)%MODE(NMOD))
          DO MOD = 1,NMOD
            CALL READ_C_C(NAME,NCHARTITLE)
            DO I=1,NCHARTITLE
              MAT_ELEM%MAT_PARAM(IMAT)%MODE(MOD)(I:I) = CHAR(NAME(I))
            END DO
          ENDDO 
        ELSE 
          ALLOCATE(MAT_ELEM%MAT_PARAM(IMAT)%MODE(0))
        ENDIF
      ENDDO

      ! Read multimaterial buffer
      ALLOCATE (IBUF(21) ) !< mex is 21 submaterials
      ALLOCATE (RBUF(21) )
      DO IMAT = 1,NUMMAT
        CALL READ_I_C(IBUF, 1)
        NBSUBMAT = IBUF(1)
        MAT_ELEM%MAT_PARAM(IMAT)%MULTIMAT%NB = NBSUBMAT
        IF(NBSUBMAT > 0)THEN
          CALL READ_I_C(IBUF, NBSUBMAT)
          CALL READ_DB(RBUF, NBSUBMAT)
          MAT_ELEM%MAT_PARAM(IMAT)%MULTIMAT%mid = IBUF(1:NBSUBMAT)
          MAT_ELEM%MAT_PARAM(IMAT)%MULTIMAT%vfrac = RBUF(1:NBSUBMAT)
        ENDIF
      ENDDO
      DEALLOCATE(IBUF)
      DEALLOCATE(RBUF)

      ! read eos model parameters
      DO IMAT = 1,NUMMAT
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%CV=ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%CP=ZERO
        IF (MAT_ELEM%MAT_PARAM(IMAT)%IEOS > 0) THEN
          CALL READ_EOSPARAM(MAT_ELEM%MAT_PARAM(IMAT)%EOS)
        END IF
      END DO
      
      ! read ALE rezoning parameters
      DO IMAT = 1,NUMMAT
        CALL READ_ALE_REZONING_PARAM(MAT_ELEM%MAT_PARAM(IMAT)%REZON)
      END DO       

c-----------
      RETURN
      END
