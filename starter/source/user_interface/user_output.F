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
      !||    user_output           ../starter/source/user_interface/user_output.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail_user     ../starter/source/materials/fail/failuser/hm_read_fail_user.F
      !||    hm_read_mat29_31      ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat_99        ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_prop_user     ../starter/source/properties/user_spring_solid/hm_read_prop_user.F
      !||    hm_read_window_user   ../starter/source/tools/userwi/hm_read_window_user.F
      !||    read_sensor_user      ../starter/source/tools/sensor/read_sensor_user.F
      !||    rinit3                ../starter/source/elements/spring/rinit3.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE USER_OUTPUT(IOUT,ILAW,ROOTN,ROOTLEN,INPF)
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOUT,ILAW,ROOTLEN,INPF
      CHARACTER*80 ROOTN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INPUTF,LEN
      CHARACTER FILNAM*512,CLAW*4
      INTEGER LEN_FILNAM
      LOGICAL TEST
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------
      WRITE(CLAW,'(I4.4)')ILAW
       FILNAM  ='SO'//ROOTN(1:ROOTLEN)//'_'//CLAW//'.scr'
       LEN_FILNAM=LEN_TRIM(FILNAM)

       INPUTF=31

       TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_FILNAM)    
       LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_FILNAM 

       OPEN(UNIT=INPUTF,
     .     FORM='FORMATTED',
     .     FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACTION='READ' )      
     
       TEST=.TRUE.
       DO WHILE(TEST)
          READ(INPUTF,END=999,FMT='(A)')LINE    
          LEN=LEN_TRIM(LINE)
          WRITE(IOUT,FMT='(A)')LINE(1:LEN)
       ENDDO
 999     CONTINUE

       CLOSE(UNIT=INPUTF)

C Delete  Output file
!       CALL DELETE_USER_FILE(FILNAM,LEN_FILNAM)
       CALL DELETE_USER_FILE(TMP_NAME,LEN_TMP_NAME)

       IF(INPF==1)THEN
C Delete Input file
         WRITE(CLAW,'(I4.4)')ILAW
         FILNAM  ='SI'//ROOTN(1:ROOTLEN)//'_'//CLAW//'.scr'
         LEN_FILNAM=LEN_TRIM(FILNAM)
         TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_FILNAM)    
         LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_FILNAM 
       
!         CALL DELETE_USER_FILE(FILNAM,LEN_FILNAM)
          CALL DELETE_USER_FILE(TMP_NAME,LEN_TMP_NAME)
       ENDIF
      END
