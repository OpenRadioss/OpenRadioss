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
Chd|  SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|-- called by -----------
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_CELL_EXCHANGE( NIN,ISENDTO,IRCVFROM,NSNR,IGAP,
     1                               IFQ,INACTI,NSNFIOLD,INTTH,ITYP,
     2                               ITIED,NMN,INTER_STRUCT,SORT_COMM )
!$COMMENT
!       SPMD_CELL_EXCHANGE description :
!       exchange of secondary node data (x, v, temp...)
!       SPMD_CELL_EXCHANGE organization :
!       proc P needs to :
!                  * send data if local NSN > 0 & remote NMN > 0 (--> SORT_COMM(NIN)%NB(P)>0)
!                  * rcv data if local NMN > 0 & remote NSN > 0 (--> given by SORT_COMM(NIN)%NBIRECV)
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE MESSAGE_MOD
        USE MULTI_FVM_MOD
        USE INTER_SORTING_MOD
        USE INTER_STRUCT_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NIN, IFQ, INACTI, IGAP,INTTH,NSNR,
     .        ITIED,
     .        NSNFIOLD(NSPMD),
     .        ISENDTO(NINTER+1,NSPMD+1), IRCVFROM(NINTER+1,NSPMD+1),
     .        ITYP
        INTEGER :: NMN
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   struture for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,
     .        SIZ,J, L, BUFSIZ, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,
     .        RSIZ, ISIZ, L2, REQ_RD2(NSPMD),
     .        LEN2, RSHIFT, ISHIFT, ND, JDEB, Q, NBB

      INTEGER :: P_LOC
      INTEGER :: KEY,CODE
     
      DATA MSGOFF4/6026/ 
      DATA MSGOFF5/6027/ 
        
      my_real
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
      
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGNSNFI  
      my_real, DIMENSION(:,:), ALLOCATABLE :: XTMP
      INTEGER :: ADRESS, LOCAL_RANK
      INTEGER :: SIZE_S
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C================================================================
      LOC_PROC = ISPMD + 1
      ! save the old NSNFI values
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0
     .   .OR.ITIED/=0.OR.ITYP==23.OR.ITYP==24   
     .   .OR.ITYP==25) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = INTER_STRUCT(NIN)%NSNFIOLD(P)
         END DO
      END IF
      NSNR = SORT_COMM(NIN)%NSNR

      IF(IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0) RETURN
      IF (IMONM > 0) CALL STARTIME(25,1)

      ! ---------------------------------------
      ! prepare the rcv --> local number of main node > 0
      ! allocation of buffer + reception
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN  !   local nmn>0

        RSIZ = SORT_COMM(NIN)%RSIZ
        ISIZ = SORT_COMM(NIN)%ISIZ  

       IF(NSNR>0) THEN   !   nsn remote > 0 --> only on proc with nmn>0
        
          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR)          
          
        
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, SORT_COMM(NIN)%NBIRECV
            P = SORT_COMM(NIN)%ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*RSIZ
            MSGTYP = MSGOFF4 
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
     
            LEN2 = NSNFI(NIN)%P(P)*ISIZ
            MSGTYP = MSGOFF5 
            CALL MPI_IRECV(
     1        IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD2(L),IERROR)
            IDEB = IDEB + NSNFI(NIN)%P(P)                  
          ENDDO
        ENDIF
      ENDIF
      ! ---------------------------------------

      ! ---------------------------------------
      ! prepare the send --> local number of secondary node > 0 & remote number of main node > 0
      DO P=1,NSPMD
        IF(P/=LOC_PROC) THEN
            IF(SORT_COMM(NIN)%NB(P)/=0 ) THEN
                MSGTYP = MSGOFF4
                SIZE_S = SORT_COMM(NIN)%NB(P) * SORT_COMM(NIN)%RSIZ
                CALL MPI_ISEND(
     1          SORT_COMM(NIN)%DATA_PROC(P)%RBUF(1),SIZE_S,REAL,IT_SPMD(P),MSGTYP,
     2            MPI_COMM_WORLD,SORT_COMM(NIN)%REQ_SD2(P),ierror)
                MSGTYP = MSGOFF5
                SIZE_S = SORT_COMM(NIN)%NB(P) * SORT_COMM(NIN)%ISIZ
                CALL MPI_ISEND(
     1            SORT_COMM(NIN)%DATA_PROC(P)%IBUF(1),SIZE_S,MPI_INTEGER,
     2            IT_SPMD(P),MSGTYP,
     3            MPI_COMM_WORLD,SORT_COMM(NIN)%REQ_SD3(P),ierror)
            ENDIF
        ENDIF
      ENDDO

      ! ---------------------------------------
      ! wait the rcv comm
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN  !   nmn > 0
       IF(NSNR>0) THEN   !   nsnr>0 only on proc with nmn>0
          DO L = 1, SORT_COMM(NIN)%NBIRECV
            CALL MPI_WAITANY(SORT_COMM(NIN)%NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
            CALL MPI_WAITANY(SORT_COMM(NIN)%NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)
          ENDDO
          !set specifics IREM and XREM indexes for INT24 sorting
          IGAPXREMP = IREM(4,1)
          I24XREMP  = IREM(5,1)
          I24IREMP  = IREM(6,1)
        ENDIF
      ENDIF

      ! ---------------------------------------
      ! wait the send comm : only for proc with nsn>0 & nmn>0
      ! for proc with nsn>0 & nmn=0, wait is done after the sort
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN   !   nsn >0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN     !   nmn >0
            IF(P/=LOC_PROC) THEN
              IF(SORT_COMM(NIN)%NB(P)/=0) THEN
                IF(NMN/=0) THEN
                    CALL MPI_WAIT(SORT_COMM(NIN)%REQ_SD2(P),STATUS,IERROR) 
                    DEALLOCATE(SORT_COMM(NIN)%DATA_PROC(P)%RBUF)
                    !   can be moved if local nsn>0 & local nmn=0  
                    CALL MPI_WAIT(SORT_COMM(NIN)%REQ_SD3(P),STATUS,IERROR) 
                    DEALLOCATE(SORT_COMM(NIN)%DATA_PROC(P)%IBUF)         
                    SORT_COMM(NIN)%NB(P) = 0             
                ENDIF
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      ! ---------------------------------------

      IF (IMONM > 0) CALL STOPTIME(25,1)
C
#endif
      RETURN
      END SUBROUTINE SPMD_CELL_EXCHANGE
