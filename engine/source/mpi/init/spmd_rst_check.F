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
Chd|  SPMD_RST_CHECK                source/mpi/init/spmd_rst_check.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_RST_CHECK()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "units_c.inc"
#include      "chara_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------

C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGTYP2,
     .        REQ_R(NSPMD),REQ_S(NSPMD), IRINDEX(NSPMD),
     .        INDEX, NBIRECV,I,MSGTYP,MSGOFF
      DATA MSGOFF/100/


      my_real
     .        TTREC,TTST

      CHARACTER FILNAM*100, FILNAM1*100,RSRUN*4,RSRUN1*4


       IF (ISPMD/=0) THEN

C   ENVOI DE TT AU PROC 0
         MSGTYP = MSGOFF
         CALL MPI_SEND(TT,1,REAL,IT_SPMD(1),MSGTYP,
     .               MPI_COMM_WORLD,ierror)
       ELSE
         DO I=2,NSPMD
C   Check sur TT
           MSGTYP = MSGOFF
           CALL MPI_RECV(TTREC,1,REAL,IT_SPMD(I),
     .                         MSGTYP,MPI_COMM_WORLD,STATUS,ierror)
           IF (TTREC/=TT) THEN

             WRITE(RSRUN1,'(I4.4)')1
             FILNAM1=ROOTNAM(1:ROOTLEN)
     .                  //'_'//CHRUN//'_'//RSRUN1//'.rst'

             WRITE(RSRUN,'(I4.4)')I
             FILNAM=ROOTNAM(1:ROOTLEN)
     .                  //'_'//CHRUN//'_'//RSRUN//'.rst'

             CALL ANCMSG(MSGID=24,ANMODE=ANINFO_BLIND,
     .                   C1=FILNAM1(1:ROOTLEN+15),
     .                   C2=FILNAM(1:ROOTLEN+15))
              CALL ARRET(2)
           ENDIF

         ENDDO

       ENDIF
#endif
      RETURN
      END

