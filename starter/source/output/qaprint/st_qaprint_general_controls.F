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
Chd|  ST_QAPRINT_GENERAL_CONTROLS   source/output/qaprint/st_qaprint_general_controls.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_GENERAL_CONTROLS(NOM_OPT ,INOM_OPT  ,DAMPR  , IRAND, ALEA,
     1                                       XSEED   ,UNITAB    ,QP_IPERTURB,QP_RPERTURB,
     2                                       EIGIPM  , EIGRPM)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE UNITAB_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr12_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tabsiz_c.inc"
#include      "random_c.inc"
#include      "sphcom.inc"
#include      "sms_c.inc"
#include      "sysunit.inc"
#include      "eigcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT), 
     .                       IRAND(*),QP_IPERTURB(NPERTURB,6),EIGIPM(NEIPM,*)
      my_real, INTENT(IN) :: DAMPR(NRDAMP,*),ALEA(*),XSEED(*),EIGRPM(NERPM,*)
      my_real, INTENT(IN) :: QP_RPERTURB(NPERTURB,4)
      TYPE (UNIT_TYPE_) ::UNITAB
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,IPERT,MY_ID, MY_DAMP, MY_CONSTRAINT,TEMP_INT,
     .        MY_RAND, MY_UNIT, MY_DEFAULTINTER,LENRNOISE,
     .        IDS(NPERTURB),IDX(NPERTURB),IDSEIG(NEIG),IDXEIG(NEIG),MY_EIG
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      LOGICAL :: OK_QA
C-----------------------------------------------
C     /DAMP
C-----------------------------------------------
      IF (MYQAKEY('/DAMP')) THEN
        DO MY_DAMP=1,NDAMP
C
          TITR(1:nchartitle)=''
          MY_ID = NINT(DAMPR(1,MY_DAMP))
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_DAMP_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          DO I=1,NRDAMP
            IF(DAMPR(I,MY_DAMP)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'DAMPR_',I
              TEMP_DOUBLE = DAMPR(I,MY_DAMP)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_DAMP=1,NDAMP
      END IF
C-----------------------------------------------
C     /DAMP/INTER
C-----------------------------------------------
      IF (MYQAKEY('/DAMP/INTER')) THEN
        DO MY_DAMP=1,NDAMP
C
          TITR(1:nchartitle)=''
          MY_ID = NINT(DAMPR(1,MY_DAMP))
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_DAMP_INTER_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          DO I=1,NRDAMP
            IF(DAMPR(I,MY_DAMP)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'DAMPR_',I
              TEMP_DOUBLE = DAMPR(I,MY_DAMP)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
          IF(IDAMP_RDOF/=ZERO)THEN
            WRITE(VARNAME,'(A)') 'IDAMP_RDOF_'
            TEMP_DOUBLE = IDAMP_RDOF
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDIF
C
        END DO ! MY_DAMP=1,NDAMP
      END IF
C-----------------------------------------------
C     /ANALY
C-----------------------------------------------
      IF (MYQAKEY('/ANALY')) THEN
C
        CALL QAPRINT('ANALY', 0,0.0_8)
C
        WRITE(VARNAME,'(A)') 'NANALY'
        TEMP_INT = NANALY
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'IPARI0'
        TEMP_INT = IPARI0
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
C-----------------------------------------------
C     /DEF_SOLID
C-----------------------------------------------
      IF (MYQAKEY('/DEF_SOLID')) THEN
C
        CALL QAPRINT('DEF_SOLID', 0,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Isolid'
        TEMP_INT = IHBE_DS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Ismstr'
        TEMP_INT = ISST_DS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Icpre'
        TEMP_INT = ICPRE_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Istrain'
        TEMP_INT = ISTRA_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Itetra4'
        TEMP_INT = ITET4_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Itetra10'
        TEMP_INT = ITET10_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Imas'
        TEMP_INT = IMAS_DS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Iframe'
        TEMP_INT = IFRAME_DS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
      END IF
C-----------------------------------------------
C     /DEF_SHELL
C-----------------------------------------------
      IF (MYQAKEY('/DEF_SHELL')) THEN
C
        CALL QAPRINT('DEF_SHELL', 0,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Ishell'
        TEMP_INT = IHBE_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Ismstr'
        TEMP_INT = ISST_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Ithick'
        TEMP_INT = ITHK_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Iplas'
        TEMP_INT = IPLA_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Istrain'
        TEMP_INT = ISTR_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Ish3n'
        TEMP_INT = ISH3N_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A)') 'Idrill'
        TEMP_INT = IDRIL_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
      END IF
C-----------------------------------------------
C     /RANDOM
C-----------------------------------------------
      IF (MYQAKEY('/RANDOM')) THEN
C
        CALL QAPRINT('RANDOM',0,0.0_8)

        DO MY_RAND=1,NRAND
C
          WRITE(VARNAME,'(A,I0)') 'IRAND_',MY_RAND
          TEMP_INT = IRAND(MY_RAND)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          IF(ALEA(MY_RAND)/=ZERO)THEN
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'ALEA_',MY_RAND
            TEMP_DOUBLE = ALEA(MY_RAND)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF
C
          IF(XSEED(MY_RAND)/=ZERO)THEN
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'XSEED_',MY_RAND
            TEMP_DOUBLE = XSEED(MY_RAND)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF
C
        END DO ! MY_RAND=1,NRAND
C
      END IF
C-----------------------------------------------
C     /IMPLICIT
C-----------------------------------------------
      IF (MYQAKEY('/IMPLICIT')) THEN
C
        CALL QAPRINT('IMPLICIT',0,0.0_8)

        WRITE(VARNAME,'(A,I0)') 'IIMPLICIT'
        TEMP_INT = IIMPLICIT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IPLA_D'
        TEMP_INT = IPLA_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IHBE_DS'
        TEMP_INT = IHBE_DS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IHBE_D'
        TEMP_INT = IHBE_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IDRIL_D'
        TEMP_INT = IDRIL_D
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
C-----------------------------------------------
C     /SPMD
C-----------------------------------------------
      IF (MYQAKEY('/SPMD')) THEN
C
        CALL QAPRINT('SPMD',0,0.0_8)

        WRITE(VARNAME,'(A,I0)') 'DECTYP'
        TEMP_INT = DECTYP
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'NSPMD'
        TEMP_INT = NSPMD
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'DECANI'
        TEMP_INT = DECANI
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'DECMOT'
        TEMP_INT = DECMOT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'DECNEQ'
        TEMP_INT = DECNEQ
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'NTHREAD'
        TEMP_INT = NTHREAD
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
C-----------------------------------------------
C     /SPHGLO
C-----------------------------------------------
      IF (MYQAKEY('/SPHGLO')) THEN
C
        CALL QAPRINT('SPHGLO',0,0.0_8)

        IF(SPASORT/=ZERO)THEN
C         VARNAME: variable name in ref.extract (without blanks)
          WRITE(VARNAME,'(A,I0)') 'SPASORT'
          TEMP_DOUBLE = SPASORT
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        END IF
C
        WRITE(VARNAME,'(A,I0)') 'LVOISPH'
        TEMP_INT = LVOISPH
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'KVOISPH'
        TEMP_INT = KVOISPH
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'ITSOL2SPH'
        TEMP_INT = ITSOL2SPH
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
C-----------------------------------------------
C     /CAA
C-----------------------------------------------
      IF (MYQAKEY('/CAA')) THEN
C
        CALL QAPRINT('CAA',0,0.0_8)

        WRITE(VARNAME,'(A,I0)') 'ICAA'
        TEMP_INT = ALE%GLOBAL%ICAA
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
C-----------------------------------------------
C     /IOFLAG
C-----------------------------------------------
      IF (MYQAKEY('/IOFLAG')) THEN
C
        CALL QAPRINT('IOFLAG',0,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IPRI'
        TEMP_INT = IPRI
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IOUTPUT'
        TEMP_INT = IOUTPUT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'OUTYY_FMT'
        TEMP_INT = OUTYY_FMT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IROOTYY'
        TEMP_INT = IROOTYY
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IDROT'
        TEMP_INT = IDROT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IRFORM'
        TEMP_INT = IRFORM
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
C-----------------------------------------------
C     /AMS
C-----------------------------------------------
      IF (MYQAKEY('/AMS')) THEN
C
        CALL QAPRINT('AMS',0,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'ISMS'
        TEMP_INT = ISMS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IDTGRS'
        TEMP_INT = IDTGRS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'ISMS_SELEC'
        TEMP_INT = ISMS_SELEC
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        WRITE(VARNAME,'(A,I0)') 'IREST_MSELT'
        TEMP_INT = IREST_MSELT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
        IF(DT_SMS_SWITCH/=ZERO)THEN
C         VARNAME: variable name in ref.extract (without blanks)
          WRITE(VARNAME,'(A,I0)') 'DT_SMS_SWITCH'
          TEMP_DOUBLE = DT_SMS_SWITCH
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        END IF
C
      END IF
C-----------------------------------------------
C     /UNIT
C-----------------------------------------------
      IF (MYQAKEY('/UNIT')) THEN
C
        DO MY_UNIT=1,NUNITS
C
          TITR(1:nchartitle)=''
          MY_ID = UNITAB%UNIT_ID(MY_UNIT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_UNIT_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          IF(UNITAB%FAC_M(MY_UNIT)/=ZERO)THEN
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'FAC_M_',MY_UNIT
            TEMP_DOUBLE = UNITAB%FAC_M(MY_UNIT)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF
C
          IF(UNITAB%FAC_L(MY_UNIT)/=ZERO)THEN
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'FAC_L_',MY_UNIT
            TEMP_DOUBLE = UNITAB%FAC_L(MY_UNIT)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF

          IF(UNITAB%FAC_T(MY_UNIT)/=ZERO)THEN
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'FAC_T_',MY_UNIT
            TEMP_DOUBLE = UNITAB%FAC_T(MY_UNIT)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF
C
        END DO ! MY_UNIT=1,NUNITS
C
      END IF
C-----------------------------------------------
C     /DEFAULT/INTER
C----------------------------------------------
      IF (MYQAKEY('/DEFAULT/INTER')) THEN
C
        CALL QAPRINT('/DEFAULT/INTER', 0,0.0_8)

        DO MY_DEFAULTINTER=1,100
C
          IF(DEF_INTER(MY_DEFAULTINTER) /= 0) THEN
            WRITE(VARNAME,'(A,I0)') 'DEF_INTER_',MY_DEFAULTINTER
            TEMP_INT = DEF_INTER(MY_DEFAULTINTER)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
          ENDIF
C
        ENDDO ! MY_DEFAULTINTER=1,100

      END IF
C-----------------------------------------------
C     /INTTHICK
C----------------------------------------------
      IF (MYQAKEY('/INTTHICK')) THEN
C
        IF (IINTTHICK > 0) THEN
          CALL QAPRINT('/INTTHICK',0,0.0_8)
          WRITE(VARNAME,'(A)') 'INTTHICK_'
          TEMP_INT = IINTTHICK
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
        ENDIF

      END IF
C-----------------------------------------------
C     /ALE/GRID/ *
C----------------------------------------------
      OK_QA = MYQAKEY('/ALE/GRID')
      IF (OK_QA) THEN
         TEMP_DOUBLE = ALE%GRID%ALPHA
         WRITE(VARNAME,'(A)') 'ALPHA_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
         TEMP_DOUBLE = ALE%GRID%GAMMA
         WRITE(VARNAME,'(A)') 'GAMMA_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
         TEMP_DOUBLE = ALE%GRID%VGX
         WRITE(VARNAME,'(A)') 'VGX_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
         TEMP_DOUBLE = ALE%GRID%VGY
         WRITE(VARNAME,'(A)') 'VGY_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
         TEMP_DOUBLE = ALE%GRID%VGZ
         WRITE(VARNAME,'(A)') 'VGZ_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
      ENDIF
C-----------------------------------------------
C     /SHFRA
C----------------------------------------------
      IF (MYQAKEY('/SHFRA')) THEN
C
        IF (ISHFRAM > 0) THEN
          CALL QAPRINT('/ISHFRA',0,0.0_8)
          WRITE(VARNAME,'(A)') 'ISHFRAM_'
          TEMP_INT = ISHFRAM
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
        ENDIF

      END IF
C-----------------------------------------------
C     /UPWIND
C----------------------------------------------
      OK_QA = MYQAKEY('/UPWIND')
      IF (OK_QA) THEN
         TEMP_DOUBLE = ALE%UPWIND%UPWMG
         WRITE(VARNAME,'(A)') 'UPCOEF1_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
         TEMP_DOUBLE = ALE%UPWIND%UPWOG
         WRITE(VARNAME,'(A)') 'UPCOEF2_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
         TEMP_DOUBLE = ALE%UPWIND%UPWSM
         WRITE(VARNAME,'(A)') 'UPCOEF3_'
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
      ENDIF
C-----------------------------------------------
C     /PERTURB
C----------------------------------------------

      IF (MYQAKEY('/PERTURB')) THEN
C       
        IF (NPERTURB > 0) THEN 
C
!         Sort by ID to ensure internal order independant output
          DO I = 1,NPERTURB 
            IDS(I)    = QP_IPERTURB(I,1)
            IDX(I)    = I
          ENDDO
          CALL QUICKSORT_I2(IDS, IDX, 1, NPERTURB)          
C         
!         Loop over INIGRAVs
          DO II = 1, NPERTURB
C
            MY_ID = IDX(II)
            CALL QAPRINT('/PERTURB_FAKE_NAME',II,0.0_8)     
C
            DO I = 1,6
              WRITE(VARNAME,'(A,I0)') 'QP_IPERTURB_',I
              TEMP_INT = QP_IPERTURB(MY_ID,I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)            
            ENDDO
C
            DO I = 1,4
              WRITE(VARNAME,'(A,I0)') 'QP_RPERTURB_',I
              TEMP_DOUBLE = QP_RPERTURB(MY_ID,I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)           
            ENDDO 
C
          ENDDO
C
        ENDIF
C
      END IF
C-----------------------------------------------
C     /STAMPING
C-----------------------------------------------
      IF (MYQAKEY('/STAMPING')) THEN
C
        CALL QAPRINT('STAMPING',0,0.0_8)

        WRITE(VARNAME,'(A,I0)') 'ISTAMPING'
        TEMP_INT = ISTAMPING
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
      ENDIF
C-----------------------------------------------
C     /EIG
C-----------------------------------------------
      IF (MYQAKEY('/EIG')) THEN
C          
!     Sort by ID to ensure internal order independant output
          DO I = 1, NEIG
            IDSEIG(I)    = EIGIPM(1,I)
            IDXEIG(I)    = I
          ENDDO
          IF (NEIG>0) CALL QUICKSORT_I2(IDSEIG, IDXEIG, 1, NEIG)

        DO II=1,NEIG
C
          MY_EIG = IDXEIG(II)
          WRITE(VARNAME,'(A,I0)') 'EIGID_',MY_EIG
          TEMP_INT = EIGIPM(1,MY_EIG)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C          
          DO I=1,NEIPM
            IF(EIGIPM(I,MY_EIG) /=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'EIGIPM_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),EIGIPM(I,MY_EIG),0.0_8)
            END IF
          END DO
C          
          DO I=1,NERPM
            IF(EIGRPM(I,MY_EIG) /=ZERO)THEN
              WRITE(VARNAME,'(A,I0)') 'EIGRPM_',I      
              TEMP_DOUBLE = EIGRPM(I,MY_EIG)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! II=1,NEIG
C
      END IF
C-----------------------------------------------
C     /ANIM/VERS
C-----------------------------------------------
      IF (MYQAKEY('/ANIM/VERS')) THEN
C       
        WRITE(VARNAME,'(A)') 'ANIM_VERS'
        TEMP_INT = ANIM_VERS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
      END IF
c----------------------------------------------------------------------
      RETURN
      END
