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
Chd|  GET_FILE_NAME_INFO            source/system/get_file_name_info.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        PRHELPINFO                    source/starter/execargcheck.F 
Chd|====================================================================
      SUBROUTINE GET_FILE_NAME_INFO(INPUT,LENI,
     *                              ROOTNAME,LENR,
     *                              RUNN,FVERS,IS_DYNA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER INPUT*256
      CHARACTER ROOTNAME*80
      INTEGER LENI,LENR,RUNN,FVERS,IS_DYNA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ERR,FST,LST,V4RNUM,rd
      CHARACTER ARUN*4,EXT*4,EXT3*3,V4LETTER,V5LETTER
C-----------------------------------------------      
      IF (LENI < 4) GOTO 1000
      
      WRITE(EXT,'(A)') INPUT(LENI-3:LENI)
      IF ( LLE(EXT,'.rad') .AND. LGE(EXT,'.rad')) THEN
         IF (LENI < 10 ) GOTO 1000
C Run number
         READ(INPUT(LENI-7:LENI-4),FMT='(I4)',ERR=1000) RUNN
C Rootname
         WRITE(ROOTNAME,FMT='(A)',ERR=1000) INPUT(1:LENI-9)
         LENR=LEN_TRIM(ROOTNAME)
C Dataset letter
         WRITE(V5LETTER,'(A)') INPUT(LENI-8:LENI-8)
         IF ( V5LETTER=='_') THEN
           FVERS = 5
         ELSE
           GOTO 1000
         ENDIF   
      ELSEIF ( INPUT(LENI-1:LENI) == '.k' ) THEN
C Rootname
         WRITE(ROOTNAME,FMT='(A)',ERR=1000) INPUT(1:LENI-2)
         LENR=LEN_TRIM(ROOTNAME)
         IS_DYNA = 1 
         RUNN = 0  
      ELSEIF ( INPUT(LENI-3:LENI) == '.key' ) THEN
C Rootname
         WRITE(ROOTNAME,FMT='(A)',ERR=1000) INPUT(1:LENI-4)
         LENR=LEN_TRIM(ROOTNAME)
         IS_DYNA = 1 
         RUNN = 0
      ELSE
C Run number
         READ(INPUT(LENI-1:LENI),FMT='(I2)',ERR=1000) RUNN
C Rootname
         WRITE(ROOTNAME,FMT='(A)',ERR=1000) INPUT(1:LENI-3)
         LENR=LEN_TRIM(ROOTNAME)
C Dataset letter         
         WRITE(V4LETTER,'(A)') INPUT(LENI-2:LENI-2)
         IF ( V4LETTER=='D') THEN
            FVERS=4
         ELSE
            GOTO 1000
         ENDIF

      ENDIF


      RETURN

 1000  CONTINUE
       WRITE(6,'(A)') ' ' 
       WRITE(6,'(A,A)') '*** ERROR : WRONG RADIOSS INPUT FILE NAME: ',
     *                   INPUT(1:LENI)
       WRITE(6,'(A)') ' '
       CALL PRHELPINFO()
       CALL MY_EXIT(2)
      END
