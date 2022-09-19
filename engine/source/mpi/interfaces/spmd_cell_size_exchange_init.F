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
Chd|  SPMD_CELL_SIZE_EXCHANGE_INIT  source/mpi/interfaces/spmd_cell_size_exchange_init.F
Chd|-- called by -----------
Chd|        SPMD_CELL_LIST_EXCHANGE       source/mpi/interfaces/spmd_cell_list_exchange.F
Chd|-- calls ---------------
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
        SUBROUTINE SPMD_CELL_SIZE_EXCHANGE_INIT(IRCVFROM,ISENDTO,IPARI,NIN,INTER_STRUCT,SORT_COMM)
!$COMMENT
!       SPMD_CELL_SIZE_EXCHANGE_INIT description :
!       initialization of buffer size + allocation
!       SPMD_CELL_SIZE_EXCHANGE_INIT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_SORTING_MOD
        USE INTER_STRUCT_MOD
        USE TRI7BOX
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
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
        INTEGER, INTENT(in) :: NIN
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) ::  IPARI
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRCVFROM
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   struture for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
#ifdef MPI
        INTEGER :: I,J,NOD,L,L2,KK,IJK,KJI
        INTEGER :: P,P_LOC
        INTEGER :: ADRESS,SHIFT_
        INTEGER :: ISIZ,RSIZ,IDEB,JDEB
        INTEGER :: NSN,NMN,IGAP,INTTH,INTFRIC,ITYP,ITIED
        INTEGER :: IFQ,INACTI
        INTEGER :: LOC_PROC
!   --------------------------------------------------------------------

        LOC_PROC = ISPMD + 1
        SORT_COMM(NIN)%RSIZ = 0
        SORT_COMM(NIN)%ISIZ = 0

        IGAP = IPARI(21,NIN)
        INTTH = IPARI(47,NIN)
        INTFRIC = IPARI(72,NIN)
        ITYP = IPARI(7,NIN)
        ITIED = IPARI(85,NIN)
        NMN = IPARI(6,NIN)
        NSN = IPARI(5,NIN)
        INACTI = IPARI(22,NIN)
        IFQ =IPARI(31,NIN)

        IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.ITIED/=0) THEN
            IF(.NOT.ALLOCATED(INTER_STRUCT(NIN)%NSNFIOLD) ) THEN
                ALLOCATE(INTER_STRUCT(NIN)%NSNFIOLD(NSPMD))
                INTER_STRUCT(NIN)%NSNFIOLD(1:NSPMD) = 0
            ENDIF
            DO P = 1, NSPMD
                INTER_STRUCT(NIN)%NSNFIOLD(P) = NSNFI(NIN)%P(P)
            END DO
        END IF
        SORT_COMM(NIN)%NBSEND_NB = 0
        SORT_COMM(NIN)%NBRECV_NB = 0

        IF(IRCVFROM(NIN,LOC_PROC)/=0.OR.ISENDTO(NIN,LOC_PROC)/=0) THEN

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%REQUEST_NB_R)) THEN
                ALLOCATE(SORT_COMM(NIN)%REQUEST_NB_R(NSPMD) )
            ENDIF
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%REQUEST_NB_S)) THEN
                ALLOCATE(SORT_COMM(NIN)%REQUEST_NB_S(NSPMD) )
            ENDIF
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%NB)) THEN
                ALLOCATE(SORT_COMM(NIN)%NB(NSPMD) )
                SORT_COMM(NIN)%NB(1:NSPMD) = 0
            ENDIF
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%INDEX_RCV)) THEN
                ALLOCATE( SORT_COMM(NIN)%INDEX_RCV(SORT_COMM(NIN)%PROC_NUMBER) )
            ENDIF

            RSIZ = 8
            ISIZ = 6

c specific cases
c IGAP=1 or IGAP=2
            IF(IGAP==1 .OR. IGAP==2)THEN
                RSIZ = RSIZ + 1
c IGAP=3	
            ELSEIF(IGAP==3)THEN
                RSIZ = RSIZ + 2
            ENDIF

C thermic      
            IF(INTTH > 0 ) THEN
                RSIZ = RSIZ + 2
	            ISIZ = ISIZ + 1
            ENDIF
C Friction      
            IF(INTFRIC > 0 ) THEN
	            ISIZ = ISIZ + 1
            ENDIF

C -- IDTMINS==2      
            IF(IDTMINS == 2)THEN
                ISIZ = ISIZ + 2
C -- IDTMINS_INT /= 0		
            ELSEIF(IDTMINS_INT/=0)THEN
                ISIZ = ISIZ + 1
            END IF

            SORT_COMM(NIN)%RSIZ = RSIZ
            SORT_COMM(NIN)%ISIZ = ISIZ

            IF(ISENDTO(NIN,LOC_PROC)>0) THEN
                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_NB)) ALLOCATE(SORT_COMM(NIN)%SEND_NB(NSPMD))
            ENDIF
            IF(IRCVFROM(NIN,LOC_PROC)>0) THEN
                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RECV_NB)) ALLOCATE(SORT_COMM(NIN)%RECV_NB(NSPMD))
            ENDIF
        ENDIF
#endif
        RETURN
        END SUBROUTINE SPMD_CELL_SIZE_EXCHANGE_INIT
