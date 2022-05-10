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
Chd|  SPMD_LAGBOUNDS_EXCH_I22       source/mpi/interfaces/spmd_lagbounds_exch_i22.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_LAGBOUNDS_EXCH_I22(
     1   BMINMAL_SPMD, BMINMAL   ,ISENDTO, IRCVFROM , NIN    )  
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
!#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*), NIN

      my_real
     .        BMINMAL(6), BMINMAL_SPMD(6, NSPMD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP, LOC_PROC,P, MSGOFF,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        ISINDEXI(NSPMD),
     .        NBX,NBY,NBZ
      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
      my_real ::
     .        DX, DY, DZ,
     .        XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
      LOGICAL ::
     .        TEST
      DATA MSGOFF/142/

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C===============================================
C     Exchanging each local lagrangian bounds.
C===============================================
      IF(NSPMD == 1)RETURN      !precondition
      LOC_PROC = ISPMD + 1
      !-------------------------------------------!
      !    Domain Bounds from i22xsave            !
      !-------------------------------------------!
      IF (IMONM > 0) CALL STARTIME(25,1)
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
      !-------------------------------------------!
      !  Local Lagrangian Domain Bounds Sending   !
      !-------------------------------------------!
      DO P = 1, NSPMD
        IF(P.NE.LOC_PROC) THEN
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     .      BMINMA(1,LOC_PROC),6  ,REAL  ,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
        ENDIF
      ENDDO
      !-------------------------------------------!
      ! Remote Lagrangian Domain Bounds Recieving !
      !-------------------------------------------!
      NBIRECV=0
      DO P = 1, NSPMD
        IF(LOC_PROC.NE.P) THEN
          NBIRECV=NBIRECV+1
          IRINDEXI(NBIRECV)=P
          MSGTYP = MSGOFF
          CALL MPI_RECV(
     .      BMINMA(1,P)   ,6      ,REAL  ,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD,STATUS,IERROR)
        ENDIF
      ENDDO
      !-------------------------------------------!
      !     MPI_WAITING                           !
      !-------------------------------------------!
      DO P = 1, NSPMD
        IF(P.NE.LOC_PROC) THEN
          CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
        ENDIF
      ENDDO
      !-------------------------------------------!
      ! Returning complete bounds data
      BMINMAL_SPMD = BMINMA
      IF (IMONM > 0) CALL STOPTIME(25,1)
#endif
      RETURN
      END
