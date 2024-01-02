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
Chd|  ST_QAPRINT_INTERFACES         source/output/qaprint/st_qaprint_interfaces.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_INTERFACES(NOM_OPT   ,INOM_OPT  ,IPARI   ,INTBUF_TAB, I2RUPT,
     2                                 AREASL    )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      INTEGER, INTENT(IN) :: IPARI(NPARI,NINTER)
      my_real, INTENT(IN) :: I2RUPT(6,*),AREASL(*)
C
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, MY_INTER
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C     /INTER
C-----------------------------------------------
      IF (MYQAKEY('INTERFACES')) THEN
        DO MY_INTER=1,NINTER
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(3) + MY_INTER),LTITR)
          MY_ID = IPARI(15,MY_INTER)
C
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_INTER_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          DO I=1,NPARI
            IF(IPARI(I,MY_INTER)/=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IPARI_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPARI(I,MY_INTER),0.0_8)
            END IF
          END DO
C
          IF(INTBUF_TAB(MY_INTER)%STFAC(1)/=ZERO)THEN
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'STFAC_',I
            TEMP_DOUBLE = INTBUF_TAB(MY_INTER)%STFAC(1)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF
C
          DO I=1,NPARIR
            IF(INTBUF_TAB(MY_INTER)%VARIABLES(I)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FRIGAP_',I
              TEMP_DOUBLE = INTBUF_TAB(MY_INTER)%VARIABLES(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO

C          IF(IPARI(30,MY_INTER) > 0) THEN
C          S_FRIC_P is the size of FRIC_P if option is not use size is Zero
            DO I=1,INTBUF_TAB(MY_INTER)%S_FRIC_P
              IF(INTBUF_TAB(MY_INTER)%FRIC_P(I) /= ZERO) THEN
                 WRITE(VARNAME,'(A,I0)') 'FRIC_P_',I
                 TEMP_DOUBLE = INTBUF_TAB(MY_INTER)%FRIC_P(I)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              ENDIF
            ENDDO

C
        END DO ! MY_INTER=1,NINTER
      END IF
C-----------------------------------------------
C     /INTER/TYPE2 - additional output
C-----------------------------------------------
      IF (MYQAKEY('/TYPE2')) THEN
C
        IF (INTHEAT /= 0) THEN
          WRITE(VARNAME,'(A)') 'INTHEAT_'
          TEMP_DOUBLE = INTHEAT
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDIF
C
        IF (I7STIFS /= 0) THEN
          WRITE(VARNAME,'(A)') 'I7STIFS_'
          TEMP_DOUBLE = I7STIFS
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDIF
C
        IF (NHIN2 /= 0) THEN
          WRITE(VARNAME,'(A)') 'NHIN2_'
          TEMP_DOUBLE = NHIN2
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDIF
C
        DO MY_INTER=1,NINTER
C
          TITR(1:nchartitle)=''
          MY_ID = IPARI(15,MY_INTER)

          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT('INTERFACE',MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_INTER_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          IF(IPARI(7,MY_INTER)==2)THEN
C
            IF (AREASL(MY_INTER) /= 0) THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A)') 'AREASL_'
              TEMP_DOUBLE = AREASL(MY_INTER)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
C
            DO I=1,6
              IF(I2RUPT(I,MY_INTER)/=0) THEN
C               VARNAME: variable name in ref.extract (without blanks)
                WRITE(VARNAME,'(A,I0)') 'I2RUPT_',I
                TEMP_DOUBLE = I2RUPT(I,MY_INTER)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              ENDIF
            END DO
C
            DO I=1,IPARI(5,MY_INTER)
              IF(INTBUF_TAB(MY_INTER)%S_IRUPT>0) THEN
                IF(INTBUF_TAB(MY_INTER)%IRUPT(I)/=0) THEN
C                   VARNAME: variable name in ref.extract (without blanks)
                    WRITE(VARNAME,'(A,I0)') 'PENALTY_NODE_',I
                    TEMP_DOUBLE = INTBUF_TAB(MY_INTER)%IRUPT(I)
                    CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDIF
              ENDIF
            END DO
C
          ENDIF
C
         ENDDO
      ENDIF
C-----------------------------------------------
C     /INTER/SUB - additional output
C-----------------------------------------------
      IF (MYQAKEY('/INTER/SUB')) THEN
C
        DO MY_INTER=1,NINTSUB
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(4) + MY_INTER),LTITR)
          MY_ID = NOM_OPT(1,INOM_OPT(4)+MY_INTER)
C
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_SUB_INTER_FAKE_NAME', MY_ID,0.0_8)
          END IF  
C
          DO I=2,6
            IF(NOM_OPT(I,INOM_OPT(4)+MY_INTER)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'NOM_OPT_', I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NOM_OPT(I,INOM_OPT(4)+MY_INTER),0.0_8)
            END IF
          ENDDO
C
         ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
