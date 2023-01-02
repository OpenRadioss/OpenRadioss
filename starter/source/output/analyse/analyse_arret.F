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
Chd|  ANADERR                       source/output/analyse/analyse_arret.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ANADERR()
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C
      IERR = IERR +1
      ANADERR = 0
      RETURN
C
      END
Chd|====================================================================
Chd|  ANADWAR                       source/output/analyse/analyse_arret.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ANADWAR()
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C
      IWARN = IWARN +1
      ANADWAR = 0
      RETURN
C
      END
C
Chd|====================================================================
Chd|  ANGETNB                       source/output/analyse/analyse_arret.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ANGETNB(NBERR,NBWARN)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBERR,NBWARN
C
      NBERR=IERR
      NBWARN=IWARN
      ANGETNB = 0
      RETURN
C
      END
C
Chd|====================================================================
Chd|  ANPRINT                       source/output/analyse/analyse_arret.F
Chd|-- called by -----------
Chd|        ANEND                         source/output/analyse/analyse_arret.F
Chd|-- calls ---------------
Chd|        SUMMSG                        source/output/message/summsg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ANPRINT()
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "titr_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESS*45
      DATA MESS /'PLEASE CHECK LISTING FILE FOR FURTHER DETAILS'/
C-----------------------------------------------
C Print Title
C-----------------------------------------------
      IF (IERR > 0) THEN
         WRITE(ISTDO,'(///A/A)') TITRE(7)(1:40),TITRE(5)
         WRITE(IOUT,'(///A/A)')  TITRE(7)(1:40),TITRE(5)
      ELSE
         IF ( IWARN > 0) THEN
            WRITE(ISTDO,'(///A/A)') TITRE(8)(1:40),TITRE(5)
            WRITE(IOUT,'(///A/A)')  TITRE(8)(1:40),TITRE(5)
         ELSE
            WRITE(ISTDO,'(///A/A)') TITRE(6),TITRE(5)
            WRITE(IOUT,'(///A/A)')  TITRE(6),TITRE(5)
         ENDIF
      ENDIF
C-----------------------------------------------
C Summary Errors/Warnings
C-----------------------------------------------
      CALL SUMMSG()
C-----------------------------------------------
C Print Nb Errors
C-----------------------------------------------
      WRITE(ISTDO,'(5X,I10,1X,A)') IERR,TITRE(7)(41:60)
      WRITE(IOUT ,'(5X,I10,1X,A)') IERR,TITRE(7)(41:60)
c      CALL ANCHECK(90)
C-----------------------------------------------
C Print Nb Warnings
C-----------------------------------------------
      WRITE(ISTDO,'(5X,I10,1X,A//)') IWARN,TITRE(8)(41:60)
      WRITE(IOUT ,'(5X,I10,1X,A//)') IWARN,TITRE(8)(41:60)
c      CALL ANCHECK(91)
C
C-----------------------------------------------
C Print CHECK L00
C-----------------------------------------------
      IF (IPID/=0) WRITE(ISTDO,'(//A//)') MESS

      RETURN
      END
C
Chd|====================================================================
Chd|  ANFILE                        source/output/analyse/analyse_arret.F
Chd|-- called by -----------
Chd|        ANEND                         source/output/analyse/analyse_arret.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ANFILE ()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C
      CLOSE (UNIT=IOUT)
C
      RETURN
      END
C
Chd|====================================================================
Chd|  ANEND                         source/output/analyse/analyse_arret.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ANFILE                        source/output/analyse/analyse_arret.F
Chd|        ANPRINT                       source/output/analyse/analyse_arret.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ANEND()
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "r2r_c.inc"
#include      "xtimscr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEX
      LOGICAL :: FILE_EXIST
C
      CALL ANPRINT()
C
      CALL ANFILE()
C
      ANEND = 0
      CALL starter_lic_release();
      IF(IERR==0)THEN
        IEX=0
      ELSE
        IEX=2
      END IF
c
      IF (IPID/=0) THEN
        INQUIRE(UNIT=RES_MES, EXIST=FILE_EXIST)
        IF (FILE_EXIST) CLOSE(UNIT=RES_MES,STATUS='DELETE')
        INQUIRE(UNIT=RES_CHECK, EXIST=FILE_EXIST)
        IF (FILE_EXIST) CLOSE(UNIT=RES_CHECK,STATUS='DELETE')
        INQUIRE(UNIT=RES_TMP, EXIST=FILE_EXIST)
        IF (FILE_EXIST) CLOSE(UNIT=RES_TMP,STATUS='DELETE')
      ENDIF
c
      CALL MY_EXIT(IEX)
      END
