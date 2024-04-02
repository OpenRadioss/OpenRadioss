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
Chd|  SPMD_SPLIT_COMM_INTER         source/mpi/interfaces/spmd_split_comm_inter.F
Chd|-- called by -----------
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|-- calls ---------------
Chd|        COMM_TRI7VOX_MOD              share/modules/comm_tri7vox_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_SPLIT_COMM_INTER( NBINTC,INTLIST,IPARI,ISENDTO,IRCVFROM,SORT_COMM ,
     .                                  NEED_COMM_INT25_SOLID_EROSION,COMM_INT25_SOLID_EROSION )
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
      USE INTBUFDEF_MOD  
      USE INTER_STRUCT_MOD
      USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "task_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: NBINTC
      integer, dimension(NINTER), intent(in) :: INTLIST
      integer, dimension(NPARI,NINTER), intent(in) :: IPARI
      integer,dimension(NINTER+1,NSPMD+1), intent(in) :: ISENDTO,IRCVFROM
      TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm

      LOGICAL, INTENT(inout) :: NEED_COMM_INT25_SOLID_EROSION !< boolean, true if the proc needs to comm some values related to interface type 25 with solid erosion
      INTEGER, INTENT(inout) :: COMM_INT25_SOLID_EROSION !< integer, sub-communicator related to interface type 25 with solid erosion
      
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI

      INTEGER :: KEY,CODE,I,P
      INTEGER :: NIN,KK,NTY
      INTEGER :: COLOR_INACTI,COLOR_INT25_SOLID_EROSION
      INTEGER :: INACTI,IFQ,ITIED
      INTEGER :: IDEL,IDELKEEP,ITY,SOLID_EROSION
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        ALLOCATE( COMM_TRI7VOX(NINTER) )
        COMM_TRI7VOX(1:NINTER)%INIT=.TRUE.

        NEED_COMM_INACTI = .FALSE.
        NB_INTER_7_INACTI = 0
        ALLOCATE( LIST_INTER_7_INACTI(NINTER) )
        LIST_INTER_7_INACTI(1:NINTER) = 0
        NEED_COMM_INT25_SOLID_EROSION = .FALSE.
!           -----------------------------
!       loop over the interface        
        DO KK=1,NBINTC
            NIN = INTLIST(KK)
            NTY   =IPARI(7,NIN)
!           -----------------------------
!           only tag the TYPE07 interface
            IF(NTY==7) THEN
                IF(COMM_TRI7VOX(NIN)%INIT) THEN
                    I=0
                    ALLOCATE(COMM_TRI7VOX(NIN)%PROC_LIST(NSPMD))
                    ALLOCATE(SORT_COMM(NIN)%PROC_LIST(NSPMD))
                    COMM_TRI7VOX(NIN)%PROC_LIST(1:NSPMD) = 0
                    COMM_TRI7VOX(NIN)%RANK = -1
                    !   list of processor for a given interface
                    DO P=1,NSPMD
                        IF(IRCVFROM(NIN,p)/=0.or.ISENDTO(NIN,P)/=0) THEN
                            I=I+1
                            COMM_TRI7VOX(NIN)%PROC_LIST(I) = P
                        ENDIF
                    ENDDO
                    SORT_COMM(NIN)%PROC_LIST(1:NSPMD) = COMM_TRI7VOX(NIN)%PROC_LIST(1:NSPMD)
                    COMM_TRI7VOX(NIN)%PROC_NUMBER = I
                    SORT_COMM(NIN)%PROC_NUMBER = I
                    COMM_TRI7VOX(NIN)%PROC_MIN = -1
                    COMM_TRI7VOX(NIN)%RANK=-1
                    COMM_TRI7VOX(NIN)%COMM=-1
                    !   compute the main proc for a given interface
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
                    CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,COMM_TRI7VOX(NIN)%COLOR,KEY,SORT_COMM(NIN)%COMM,CODE)
                    IF(COMM_TRI7VOX(NIN)%COLOR==1) THEN
                        CALL MPI_COMM_RANK(COMM_TRI7VOX(NIN)%comm,COMM_TRI7VOX(NIN)%RANK,CODE)
                    ENDIF
                    COMM_TRI7VOX(NIN)%INIT = .false.
                ENDIF
                !   ------------------------------
                INACTI = IPARI(22,NIN)
                IFQ = IPARI(31,NIN)
                ITIED = IPARI(85,NIN)
                IF(IMPL_S==0.OR.NEIG==0) THEN       
                    IF( INACTI==5.OR.INACTI==6.OR.IFQ>0.OR.ITIED/=0)THEN
                        NEED_COMM_INACTI = .TRUE.
                        NB_INTER_7_INACTI = NB_INTER_7_INACTI + 1
                        LIST_INTER_7_INACTI(NIN) = NB_INTER_7_INACTI
                    ENDIF
                ENDIF
                !   ------------------------------
            ENDIF

           !   ------------------------------
           ! communicator for interface type 25 with solid erosion
            SOLID_EROSION = IPARI(100,NIN)
            IDEL = IPARI(17,NIN)! idel option
            IDELKEEP = IPARI(61,NIN)
            IF(NTY==25.AND.IPARI(100,NIN)>0.AND.IDELKEEP/=1) THEN
              ! ---------------
              ! check if the proc has S or M node
              IF(ISENDTO(NIN,ISPMD+1)>0.OR.IRCVFROM(NIN,ISPMD+1)>0) THEN 
                        NEED_COMM_INT25_SOLID_EROSION = .TRUE.
              ENDIF
              ! ---------------
            ENDIF
        ENDDO
!           -----------------------------

!           -----------------------------
        !   color the processor for inacti option
        IF(.NOT.NEED_COMM_INACTI) THEN
            COLOR_INACTI = 0
            KEY = 0
        ELSE
            COLOR_INACTI = 1
            KEY = 1
        ENDIF       
        !   create the communicator for the spmd_get_inacti communication
        CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,COLOR_INACTI,KEY,COMM_INACTI,CODE)  
!           -----------------------------

!           -----------------------------
        !   color the processor for interface type 25 + solid erosion option
        IF(.NOT.NEED_COMM_INT25_SOLID_EROSION) THEN
            COLOR_INT25_SOLID_EROSION = 0
            KEY = 0
        ELSE
            COLOR_INT25_SOLID_EROSION = 1
            KEY = 1
        ENDIF       
        !   create the communicator for the type25+solid erosion communication
        CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,COLOR_INT25_SOLID_EROSION,KEY,COMM_INT25_SOLID_EROSION,CODE)  
c       write(*,*) 'need comm',ispmd,NEED_COMM_INT25_SOLID_EROSION
!           -----------------------------
#endif
      RETURN
      END SUBROUTINE SPMD_SPLIT_COMM_INTER
C

