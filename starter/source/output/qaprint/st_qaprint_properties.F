Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  ST_QAPRINT_PROPERTIES         source/output/qaprint/st_qaprint_properties.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_PROPERTIES(IGEO      ,GEO       ,BUFGEO    ,
     .                                 PM_STACK  ,GEO_STACK ,IGEO_STACK)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
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
      INTEGER, INTENT(IN) :: IGEO(NPROPGI,NUMGEO),IGEO_STACK(4* NPT_STACK+2,NS_STACK)
      my_real, INTENT(IN) :: GEO(NPROPG,NUMGEO),GEO_STACK(6*NPT_STACK+1,NS_STACK),
     .                       PM_STACK(20,NS_STACK),BUFGEO(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, MY_PID, 
     .        IADBUF, NUPARAM, IADMAT, NJMAT, IADPID, NJPID, IADFUN, NJFUN, IADTAB, NJTAB
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C=======================================================================

      IF (MYQAKEY('PROPERTIES')) THEN

        DO MY_PID=1,NUMGEO
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,MY_PID),LTITR) 
C
C         Le Titr de la PID sert de nom de la variable dans le ref.extract , suivi de l'ID de la PID 
C         2 PIDs peuvent avoir le meme titre
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),IGEO(1,MY_PID),0.0_8)
          ELSE
            CALL QAPRINT('A_PID_FAKE_NAME',IGEO(1,MY_PID),0.0_8)
          END IF
          DO I=1,NPROPGI-LTITR ! si on ne peut pas tester une chaine de caracteres, do i=1,npropgi
            IF(IGEO(I,MY_PID)/=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IGEO_',I      ! IGEO(11) => 'IGEO_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGEO(I,MY_PID),0.0_8)
            END IF
          END DO
          DO I=1,NPROPG
            IF(GEO(I,MY_PID)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'GEO_',I
              TEMP_DOUBLE = GEO(I,MY_PID)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C---------------------------------------------------
C Warning: Some properties are storing some other data at the same locations 
C         as the addresses (IADBUF, NUPARAM, etc) in IGEO
C         => to be cleaned before branching THIS !
C---------------------------------------------------
          IADBUF =IGEO(57,MY_PID)
          NUPARAM=IGEO(52,MY_PID)
          DO I=1,NUPARAM
            IF(BUFGEO(IADBUF+I-1)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'BUFGEO_IADBUF_',I
              TEMP_DOUBLE = BUFGEO(IADBUF+I-1)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
          IADFUN =IGEO(58,MY_PID)
          NJFUN  =IGEO(53,MY_PID)
          DO I=1,NJFUN
            IF(BUFGEO(IADFUN+I-1)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'BUFGEO_IADFUN_',I
              TEMP_DOUBLE = BUFGEO(IADFUN+I-1)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
          IADMAT =IGEO(59,MY_PID)
          NJMAT  =IGEO(54,MY_PID)
          DO I=1,NJMAT
            IF(BUFGEO(IADMAT+I-1)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'BUFGEO_IADMAT_',I
              TEMP_DOUBLE = BUFGEO(IADMAT+I-1)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
          IADPID =IGEO(60,MY_PID)
          NJPID  =IGEO(55,MY_PID)
          DO I=1,NJPID
            IF(BUFGEO(IADPID+I-1)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'BUFGEO_IADPID_',I
              TEMP_DOUBLE = BUFGEO(IADPID+I-1)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
          IADTAB =IGEO(61,MY_PID)
          NJTAB  =IGEO(56,MY_PID)
          DO I=1,NJTAB
            IF(BUFGEO(IADTAB+I-1)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'BUFGEO_IADTAB_',I
              TEMP_DOUBLE = BUFGEO(IADTAB+I-1)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
        END DO
c          
        DO MY_PID=1,NS_STACK
          MY_ID = MY_PID
          CALL QAPRINT('STACK_NAME_NO', MY_ID,0.0_8)
c
          DO I=1,20
            IF (PM_STACK(I,MY_PID) /= ZERO) THEN
              WRITE(VARNAME,'(A,I0)') 'STACK_PM_',I
              TEMP_DOUBLE = PM_STACK(I,MY_PID)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
c                  
          DO I=1,6*NPT_STACK+1
            IF (GEO_STACK(I,MY_PID) /= ZERO) THEN
              WRITE(VARNAME,'(A,I0)') 'STACK_GEO_',I
              TEMP_DOUBLE = GEO_STACK(I,MY_PID)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
c        
          DO I=1,3*NPT_STACK+2
            IF (IGEO_STACK(I,MY_PID) /= 0) THEN
              WRITE(VARNAME,'(A,I0)') 'STACK_IGEO_',I     
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGEO_STACK(I,MY_PID),0.0_8)
            END IF
          END DO                
        END DO
c        
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
