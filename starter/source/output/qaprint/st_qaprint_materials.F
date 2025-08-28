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
!||    st_qaprint_materials   ../starter/source/output/qaprint/st_qaprint_materials.F
!||--- called by ------------------------------------------------------
!||    st_qaprint_driver      ../starter/source/output/qaprint/st_qaprint_driver.F
!||--- calls      -----------------------------------------------------
!||    fretitl2               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE ST_QAPRINT_MATERIALS(MAT_ELEM    ,IPM       ,PM        ,BUFMAT    )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MAT_ELEM_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPM(NPROPMI,NUMMAT)
      my_real, INTENT(IN) ::
     .                       PM(NPROPM,NUMMAT), BUFMAT(*)
      TYPE(MAT_ELEM_) ,INTENT(IN) :: MAT_ELEM
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IP, MY_ID,MY_MAT,IADBUF,NUPARAM,NIPARAM,NFAIL,IVISC,IVAR
      INTEGER ::  IRUPT,FAIL_ID,FAIL_IP,NUVAR,NFUNCF,NTABF,NMOD,NBMAT,MID,IEOS
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE,PTHK
      LOGICAL :: OK_QA
C-----------------------------------------------
      OK_QA = MYQAKEY('MATERIALS')
      IF (OK_QA) THEN
        DO MY_MAT=1,NUMMAT-1 ! Do not write global material
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,MY_MAT),LTITR)
          TITR = MAT_ELEM%MAT_PARAM(MY_MAT)%TITLE

C         Le Titr du MAT sert de nom de la variable dans le ref.extract , suivi de l'ID du MAT
C         2 MATs peuvent avoir le meme titre
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),IPM(1,MY_MAT),0.0_8)
          ELSE
            CALL QAPRINT('A_MAT_FAKE_NAME',IPM(1,MY_MAT),0.0_8)
          END IF
          DO I=1,NPROPMI-LTITR ! si on ne peut pas tester une chaine de caracteres, do i=1,npropmi
            IF(IPM(I,MY_MAT) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IPM_',I      ! IPM(11) => 'IPM_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPM(I,MY_MAT),0.0_8)
            END IF
          END DO
          DO I=1,NPROPM
            IF(PM(I,MY_MAT)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'PM_',I
              TEMP_DOUBLE = PM(I,MY_MAT)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
          IADBUF =IPM(7,MY_MAT)
          NUPARAM=IPM(9,MY_MAT)
          DO I=1,NUPARAM
            IF(BUFMAT(IADBUF+I-1)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'BUFMAT_',I
              TEMP_DOUBLE = BUFMAT(IADBUF+I-1)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
c-----------------------------
c         MATERIAL FAILURE MODELS 
c-----------------------------
          NFAIL = MAT_ELEM%MAT_PARAM(MY_MAT)%NFAIL
          IF (NFAIL > 0) THEN
            CALL QAPRINT('NUMBER OF FAILURE MODELS',NFAIL,0.0_8)
c
            DO I=1,NFAIL
              IRUPT   = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%IRUPT
              FAIL_ID = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%FAIL_ID
              NUPARAM = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%NUPARAM
              NIPARAM = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%NIPARAM
              NUVAR   = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%NUVAR
              NFUNCF  = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%NFUNC
              NTABF   = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%NTABLE
              NMOD    = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%NMOD
              FAIL_IP = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%FAIL_IP
              PTHK    = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%PTHK
c
              CALL QAPRINT(' FAIL MODEL TYPE',IRUPT,0.0_8)
              CALL QAPRINT(' FAIL_ID',FAIL_ID,0.0_8)
              CALL QAPRINT(' FAIL_IP',FAIL_IP,0.0_8)
              CALL QAPRINT(' PTHK',0,PTHK)
              CALL QAPRINT(' NUMBER OF STATE VARIABLES',NUVAR,0.0_8)
              CALL QAPRINT(' NUMBER OF FAILURE MODES',NMOD,0.0_8)
c
              CALL QAPRINT(' NUPARAM',NUPARAM,0.0_8)
              DO J=1,NUPARAM
                TEMP_DOUBLE = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%UPARAM(J)
                IF (TEMP_DOUBLE /= ZERO) THEN
                  WRITE(VARNAME,'(A,I0,A,I0)') 'UPARF_',I,'_',J
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                END IF
              END DO
              CALL QAPRINT(' NIPARAM',NIPARAM,0.0_8)
              DO J=1,NIPARAM
                IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%IPARAM(J)   
                IF (IVAR /= 0) THEN
                  WRITE(VARNAME,'(A,I0)') 'IPARF_',J
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
                END IF
              END DO
              CALL QAPRINT(' NFUNC',NFUNCF,0.0_8)
              DO J=1,NFUNCF
                IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%IFUNC(J)
                IF (IVAR /= 0) THEN
                  WRITE(VARNAME,'(A,I0)') 'IFUNC_',J
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
                END IF
              END DO
              CALL QAPRINT(' NTABLE',NTABF,0.0_8)
              DO J=1,NTABF
                IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%FAIL(I)%TABLE(J)   
                IF (IVAR /= 0) THEN
                  WRITE(VARNAME,'(A,I0)') 'TABLE_',J
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
                END IF
              END DO
            END DO    ! NFAIL
          END IF      ! NFAIL > 0
c-----------------------------
c         UPARAM of /VISC
c-----------------------------
          IVISC = MAT_ELEM%MAT_PARAM(MY_MAT)%IVISC
          IF (IVISC > 0) THEN
            CALL QAPRINT('** VISC_MODEL',I,0.0_8)
            NUPARAM = MAT_ELEM%MAT_PARAM(MY_MAT)%VISC%NUPARAM
            NIPARAM = MAT_ELEM%MAT_PARAM(MY_MAT)%VISC%NIPARAM
            DO J=1,NUPARAM
              TEMP_DOUBLE = MAT_ELEM%MAT_PARAM(MY_MAT)%VISC%UPARAM(J)
              IF (TEMP_DOUBLE /= ZERO) THEN
                WRITE(VARNAME,'(A,I0)') 'UPARV_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            END DO
            DO J=1,NIPARAM
              IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%VISC%IPARAM(J)
              IF (IVAR /= 0) THEN
                WRITE(VARNAME,'(A,I0)') 'IPARV_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
              END IF
            END DO
          END IF

c-----------------------------
c         /EOS parameters
c-----------------------------
          IEOS = MAT_ELEM%MAT_PARAM(MY_MAT)%IEOS
          IF (IEOS > 0) THEN

      !   1       !     POLYNOMIAL         !
      !   2       !     GRUNEISEN          !
      !   3       !     TILLOTSON          !
      !   4       !     PUFF               !
      !   5       !     SESAME             !
      !   6       !     NOBLE-ABEL         ! 2017.0
      !   7       !     IDEAL GAS          ! 2018.0
      !   8       !     MUNAGHAN           ! 2018.0
      !   9       !     OSBORNE            ! 2018.0
      !  10       !     STIFFENED GAS      ! 2018.0
      !  11       !     LSZK               ! 2018.0
      !  12       !     POWDER-BURN        ! 2019.1
      !  13       !     COMPACTION         ! 2019.1
      !  14       !     NASG               ! 2020.0
      !  15       !     JWL                ! internal use : /INIMAP
      !  16       !     IDEALGAS_VT        ! 2022.0
      !  17       !     TABULATED          ! 2022.2
      !  18       !     LINEAR             ! 2019.0
      !  19       !     EXPONENTIAL        ! 2024.1
      !  20       !     COMPACTION2        ! 2025.1
      !  21       !     COMPACTION_TAB     ! 2026.0
      !------------------------------------!


            SELECT CASE (IEOS)
              CASE(1)
                CALL QAPRINT('** EOS_MODEL > POLYNOMIAL',I,0.0_8)
              CASE(2)
                CALL QAPRINT('** EOS_MODEL > GRUNEISEN',I,0.0_8)
              CASE(3)
                CALL QAPRINT('** EOS_MODEL > TILLOTSON',I,0.0_8)
              CASE(4)
                CALL QAPRINT('** EOS_MODEL > PUFF',I,0.0_8)
              CASE(5)
                CALL QAPRINT('** EOS_MODEL > SESAME',I,0.0_8)
              CASE(6)
                CALL QAPRINT('** EOS_MODEL > NOBLE-ABEL',I,0.0_8)
              CASE(7)
                CALL QAPRINT('** EOS_MODEL > IDEAL-GAS',I,0.0_8)
              CASE(8)
                CALL QAPRINT('** EOS_MODEL > MUNAGHAN',I,0.0_8)
              CASE(9)
                CALL QAPRINT('** EOS_MODEL > OSBORNE',I,0.0_8)
              CASE(10)
                CALL QAPRINT('** EOS_MODEL > STIFFENED-GAS',I,0.0_8)
              CASE(11)
                CALL QAPRINT('** EOS_MODEL > LSZK',I,0.0_8)
              CASE(12)
                CALL QAPRINT('** EOS_MODEL > POWDER-BURN',I,0.0_8)
              CASE(13)
                CALL QAPRINT('** EOS_MODEL > COMPACTION',I,0.0_8)
              CASE(14)
                CALL QAPRINT('** EOS_MODEL > NASG',I,0.0_8)
              CASE(15)
                CALL QAPRINT('** EOS_MODEL > JWL',I,0.0_8)
              CASE(16)
                CALL QAPRINT('** EOS_MODEL > IDEAL-GAS-VT',I,0.0_8)
              CASE(17)
                CALL QAPRINT('** EOS_MODEL > TABULATED',I,0.0_8)
              CASE(18)
                CALL QAPRINT('** EOS_MODEL > LINEAR',I,0.0_8)
              CASE(19)
                CALL QAPRINT('** EOS_MODEL > EXPONENTIAL',I,0.0_8)
              CASE(20)
                CALL QAPRINT('** EOS_MODEL > COMPACTION2',I,0.0_8)
              CASE(21)
                CALL QAPRINT('** EOS_MODEL > COMPACTION_TAB',I,0.0_8)
              CASE DEFAULT
                CALL QAPRINT('** EOS_MODEL ',I,0.0_8)
            END SELECT
            NUPARAM = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%NUPARAM
            NIPARAM = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%NIPARAM
            NFUNCF = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%NFUNC
            NTABF = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%NTABLE
            DO J=1,NUPARAM
              TEMP_DOUBLE = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%UPARAM(J)
              IF (TEMP_DOUBLE /= ZERO) THEN
                WRITE(VARNAME,'(A,I0)') 'EOS_UPARAM_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            END DO
            DO J=1,NIPARAM
              IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%IPARAM(J)
              IF (IVAR /= 0) THEN
                WRITE(VARNAME,'(A,I0)') 'EOS_IPARAM_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
              END IF
            END DO
            DO J=1,NFUNCF
              IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%FUNC(J)
              IF (IVAR /= 0) THEN
                WRITE(VARNAME,'(A,I0)') 'EOS_FUNC_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
              END IF
            END DO
            DO J=1,NTABF
              IVAR = MAT_ELEM%MAT_PARAM(MY_MAT)%EOS%TABLE(J)%notable
              IF (IVAR /= 0) THEN
                WRITE(VARNAME,'(A,I0)') 'EOS_TABLE_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IVAR,0.0_8)
              END IF
            END DO
          END IF

c-----------------------------
c         MULTIMAT
c-----------------------------
          NBMAT = MAT_ELEM%MAT_PARAM(MY_MAT)%MULTIMAT%NB
          IF (NBMAT > 0) THEN
            DO J=1,NBMAT
              MID   = MAT_ELEM%MAT_PARAM(MY_MAT)%MULTIMAT%MID(J)
              TEMP_DOUBLE = MAT_ELEM%MAT_PARAM(MY_MAT)%MULTIMAT%VFRAC(J)
              IF (TEMP_DOUBLE /= ZERO) THEN
                WRITE(VARNAME,'(A,I0)') 'MID_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),MID,0.0_8)              
                WRITE(VARNAME,'(A,I0)') 'VFRAC_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            END DO
          END IF   
                 
c-----------------------------
        END DO   ! MY_MAT
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
