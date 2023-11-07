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
Chd|====================================================================
Chd|  ST_QAPRINT_MATERIALS          source/output/qaprint/st_qaprint_materials.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_MATERIALS(MAT_ELEM    ,IPM       ,PM        ,BUFMAT    )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MAT_ELEM_MOD
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
      INTEGER I,J,IP, MY_ID,MY_MAT,IADBUF,NUPARAM,NIPARAM,NFAIL,IVISC,IVAR,
     .        IRUPT,FAIL_ID,FAIL_IP,NUVAR,NFUNCF,NTABF,NMOD
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE,PTHK
      LOGICAL :: OK_QA
C-----------------------------------------------
      OK_QA = MYQAKEY('MATERIALS')
      IF (OK_QA) THEN
        DO MY_MAT=1,NUMMAT-1 ! Do not write global material
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,MY_MAT),LTITR)

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
        END DO   ! MY_MAT
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
