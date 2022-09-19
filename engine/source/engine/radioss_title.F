Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  RADIOSS_TITLE                 source/engine/radioss_title.F 
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        PRINTCENTER                   source/engine/radioss_title.F 
Chd|====================================================================
      SUBROUTINE RADIOSS_TITLE(IOUT,CPUNAM,ARCHTITLE,
     *             FILNAM,ROOTLEN,CHRUN,IRESP,IFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "build_info.inc"
#include      "commandline.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOUT,ROOTLEN,IRESP,IFLAG
      CHARACTER CPUNAM*20,FILNAM*100,CHRUN*4,ARCHTITLE*66
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN,LEN_RAD,LENBT,LENCPUNAM,LENARCHTIT,CT,LBT,
     *        LENGPU
      CHARACTER RADVERS*68,COPYRIGHTLINE*68,
     *          ARCH_TIT*66,GPUTITLE*66
C-----------------------------------------------
       LENARCHTIT=LEN_TRIM(ARCHTITLE)
       ARCH_TIT(1:66)=''
       ARCH_TIT(1:LENARCHTIT)=ARCHTITLE(1:LENARCHTIT)

       LEN=LEN_TRIM(VERS)
       LBT=LEN_TRIM(BTAG)

       IF( GOT_INSPIRE_ALM == 1)THEN
          WRITE(RADVERS,'(A,A)') 'Altair Solver ',VERS
          LEN = LEN + 14
       ELSE
          WRITE(RADVERS,'(A)') 'OpenRadioss Engine'
          LEN = LEN_TRIM(RADVERS)
       ENDIF

       LENBT = LEN_TRIM(BTAG)
       LENCPUNAM=LEN_TRIM(CPUNAM)
       LENARCHTIT=LEN_TRIM(ARCH_TIT)

       YEARSTRING(1:4)=BDATE(8:11)
       WRITE(COPYRIGHTLINE,'(A,A,A)') 'COPYRIGHT (C) 1986-'//YEARSTRING(1:4)//' Altair Engineering, Inc.'

       CALL PRINTCENTER(" ",0,IOUT,1)
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER(RADVERS,LEN,IOUT,2)
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER("Non-linear Finite Element Analysis Software",
     .                   43,IOUT,2)
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER(ARCH_TIT,LENARCHTIT,IOUT,2)
       IF (IRESP == 1) THEN
         CALL PRINTCENTER("Extended Single Precision Version",33,IOUT,2)
       ELSE
         CALL PRINTCENTER(" ",0,IOUT,0)
       ENDIF
        IF (PMSG ==1)THEN
         CALL PRINTCENTER(MSGO,LEN_MSG,IOUT,2)
       ELSE
        CALL PRINTCENTER(" ",0,IOUT,0)
       ENDIF
       CALL PRINTCENTER(" ",0,IOUT,0)
       CALL PRINTCENTER(BTAG,LENBT,IOUT,3)
       CALL PRINTCENTER(" ",0,IOUT,1)
       CALL PRINTCENTER("OpenRadioss Software",20,IOUT,3)
       CALL PRINTCENTER(COPYRIGHTLINE,64,IOUT,3)
       CALL PRINTCENTER("Licensed under GNU Affero General Public License.",49,IOUT,3)
       CALL PRINTCENTER("See License file.",17,IOUT,3)
       CALL PRINTCENTER(" ",0,IOUT,1)
       WRITE(IOUT,'(A)') ' '
       IF (IFLAG==1)
     *    WRITE(IOUT,'(A,A,A,A)') ' ROOT: ',FILNAM(1:ROOTLEN),
     *          '  RESTART: ',CHRUN

      END


Chd|====================================================================
Chd|  PRINTCENTER                   source/engine/radioss_title.F 
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RADIOSS_TITLE                 source/engine/radioss_title.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRINTCENTER(ARRAY,ARRLEN,LINOUT,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ARRLEN, LINOUT,FLAG
      CHARACTER ARRAY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       CHARACTER PRINTOUT(72)
       INTEGER POS,I
C-----------------------------------------------
C Initialisation
       PRINTOUT=' '
       PRINTOUT(1)='*'
       PRINTOUT(2)='*'
       PRINTOUT(71)='*'
       PRINTOUT(72)='*'

C   Flag = 0 ligne blanche

       IF (FLAG==1)THEN
C   Flag = 1 que des * dans la ligne
         PRINTOUT(1:72)='*'


       ELSEIF (FLAG==2) THEN
C   du texte center

C   Test si sortie n'excede pas 68 cararct  res
         IF (ARRLEN>68) THEN
           WRITE(LINOUT,'(A)') "ERROR PRINTOUT FORMAT"
           RETURN
         ENDIF

         POS= 3+(68-ARRLEN)/2

         DO I=1,ARRLEN
           PRINTOUT(POS+I-1)=ARRAY(I)
          ENDDO

       ELSEIF (FLAG==3) THEN
C   print left
         POS = 4
         DO I=1,ARRLEN
           PRINTOUT(POS+I-1)=ARRAY(I)
          ENDDO


       ENDIF

       WRITE(LINOUT,'(72A)') PRINTOUT

      END
