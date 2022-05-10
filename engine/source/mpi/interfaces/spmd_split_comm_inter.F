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
Chd|  SPMD_SPLIT_COMM_INTER         source/mpi/interfaces/spmd_split_comm_inter.F
Chd|-- called by -----------
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|-- calls ---------------
Chd|        COMM_TRI7VOX_MOD              share/modules/comm_tri7vox_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_SPLIT_COMM_INTER( NBINTC,INTLIST,IPARI,ISENDTO,IRCVFROM )
!$COMMENT
!       SPMD_SPLIT_COMM_INTER :
!              SPMD_SPLIT_COMM_INTER creates new communicators
!              for interface TYPE07
!       SPMD_SPLIT_COMM_INTER organization :
!              - loop over the interfaces in order to
!                tag all the processors of a interface
!              - then create a communicator for a given 
!                interface
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE COMM_TRI7VOX_MOD
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
#include      "sms_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: NBINTC
      integer, dimension(NINTER), intent(in) :: INTLIST
      integer, dimension(NPARI,NINTER), intent(in) :: IPARI
      integer,dimension(NINTER+1,NSPMD+1), intent(in) :: ISENDTO,IRCVFROM
      
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI

      integer :: KEY,CODE,I,P
      INTEGER :: NIN,KK,NTY

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        ALLOCATE( COMM_TRI7VOX(NINTER) )
        COMM_TRI7VOX(1:NINTER)%INIT=.TRUE.
!           -----------------------------
!       loop over the interface        
        DO KK=1,NBINTC
            NIN = INTLIST(KK)
            NTY   =IPARI(7,NIN)
!           -----------------------------
!           only tag the TYPE07 interface
            IF(NTY==7) THEN
                IF(COMM_TRI7VOX(NIN)%INIT) THEN
                    i=0
                    allocate(COMM_TRI7VOX(NIN)%PROC_LIST(NSPMD))
                    COMM_TRI7VOX(NIN)%PROC_LIST(1:NSPMD) = 0
                    COMM_TRI7VOX(NIN)%RANK = -1
                    !   list of processor for a given interface
                    DO P=1,NSPMD
                        IF(IRCVFROM(NIN,p)/=0.or.ISENDTO(NIN,P)/=0) THEN
                            i=i+1
                            COMM_TRI7VOX(NIN)%PROC_LIST(i) = P
                        ENDIF
                    ENDDO
                    COMM_TRI7VOX(NIN)%PROC_NUMBER = i
                    COMM_TRI7VOX(NIN)%PROC_MIN = -1
                    COMM_TRI7VOX(NIN)%RANK=-1
                    COMM_TRI7VOX(NIN)%COMM=-1
                    !   compute the master proc for a given interface
                    DO I = 1,COMM_TRI7VOX(NIN)%PROC_NUMBER
                        COMM_TRI7VOX(NIN)%PROC_MIN = min(COMM_TRI7VOX(NIN)%PROC_MIN,COMM_TRI7VOX(NIN)%PROC_LIST(I))
                    ENDDO
                    !   color the processor
                    IF(IRCVFROM(NIN,ispmd+1)==0.and.ISENDTO(NIN,ispmd+1)==0) THEN
                        COMM_TRI7VOX(NIN)%COLOR=0
                        KEY = 0
                    ELSE
                        COMM_TRI7VOX(NIN)%COLOR=1
                        KEY = 1
                    ENDIF       
                    !   create the communicator
                    CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,COMM_TRI7VOX(NIN)%COLOR,KEY,COMM_TRI7VOX(NIN)%COMM,CODE)
                    IF(COMM_TRI7VOX(NIN)%COLOR==1) THEN
                        CALL MPI_COMM_RANK(COMM_TRI7VOX(NIN)%comm,COMM_TRI7VOX(NIN)%RANK,CODE)
                    ENDIF
                    COMM_TRI7VOX(NIN)%INIT = .false.
                ENDIF
            ENDIF
        ENDDO
!           -----------------------------
#endif
      RETURN
      END SUBROUTINE SPMD_SPLIT_COMM_INTER
C

