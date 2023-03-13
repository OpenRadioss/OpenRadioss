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
Chd|  SPMD_I7ITIED_CAND             source/mpi/interfaces/spmd_i7itied_cand.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
        SUBROUTINE SPMD_I7ITIED_CAND(FLAG,NBINTC,IPARI,INTLIST,INTBUF_TAB)

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: NBINTC,FLAG
        INTEGER IPARI(NPARI,*),INTLIST(*)
C    
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
!       -------------------------------
!       NBINTC : integer , number of interface (different from TYPE2)
!       FLAG : integer , FLAG=1 --> sent part ; FLAG=2 --> received part
!       IPARI : integer , dimension = (NPARI,*) , property interface array
!       INTLIST : integer, dimension = * , index of interface /= from TYPE2
!       INTBUF_TAB : type(INTBUF_STRUCT_), dimension = number of interface, interface structure
!       -------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER :: STATUS(MPI_STATUS_SIZE),REQ_S(PARASIZ),REQ_R(PARASIZ)
        INTEGER :: P,LENSD,LENRV,IADS(PARASIZ+1),IADR(PARASIZ+1),IERROR,
     *        SIZ,LOC_PROC,MSGTYP,IDEB(NINTER),PROC,MSGOFF
        INTEGER :: ITIED

        INTEGER :: I,J,L,NB,NL,NN,K,N,NOD,LEN,ND,NIN,NTY,
     *        NSN,SN,NBI,NSI,
     *        I_STOK,IT,LEN_NSNSI,MS,NSNR,
     *        NI,NII,LL,ILEN,RLEN,LI,NUMERO,P2
        INTEGER, DIMENSION(NINTER) :: LLL
        INTEGER, DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
        INTEGER:: LEN_CANDF
     
        DATA MSGOFF/9000/

        SAVE IADS,IADR,BBUFS,BBUFR,REQ_S,
     *     REQ_R,ILEN,RLEN,LEN,LENSD,LENRV 
!       ----------------------------------------------     
!       FI proc send to SI proc the ITIED+CAND_F/=0 
!       nodes
!       with ITIED==2, a node is linked to an interface unless
!       its CAND_F force is 0. CAND_F is a local to a processor
!       --> need to exchange CAND_F to the local processor if 
!       a node is remote on a processor
!
!
!       sent buffer : (for the 3th proc)
!       proc :       1       *    2      *      4       *
!              <----------->   <------->   <--------->
!              |   |   |   | * |   |  |  * |  |        |* ...
!       inter :  1   3   9       1  ...
!
!       proc :       1           *    2                 *                   4       *
!              <----------->     *   <------->          *              <--------->
!             1             SUM(NSNFI(1))  SUM(NSNFI(1))+SUM(NSNFI(2))            ...
!
!       proc :                           1
!              <------------------------------------------- ...
!              |           |                          |      
!       inter :       1    |             3            |      ...
!              1     NSNFI(1)%P(1)       NSNFI(1)%P(1)+NSNFI(1)%P(3)
!              
!                          
!       length for the nth proc = SUM( NSNFI%P(n) ) for all NINTER interfaces
!       total length the nth proc = SUM( NSNFI%P( 1-->NSPMD ) ) for all NINTER interfaces
!
!
!       to initialize the buffer, the I_STOCK candidate nodes are scanned
!       if a candidate node is a remote node (--> CAND_N > NSN) AND its CAND_F value
!       is non-zero, then this node must be transmitted to the SI proc
!       for a remote node, CAND_N = NSN + SUM( NSNFI(NIN)%P(p)) + ii
!       ii is the local index of the node
!       --> so ii = CAND_N - NSN - SUM( NSNFI(NIN)%P(p)) is sent to the local proc SI
!           if CAND_F /= 0
!
!
!
!
!       received buffer : (for the 3th proc)
!       proc :       1       *    2      *      4       *
!              <----------->   <------->   <--------->
!              |   |   |   | * |   |  |  * |  |        |* ...
!       inter :  8   10  11       1  ...
!       length for the nth proc = SUM( NSNSI%P(n) ) for all NINTER interfaces
!       total length the nth proc = SUM( NSNSI%P( 1-->NSPMD ) ) for all NINTER interfaces
!
!

        IF(NSPMD==1) RETURN
!       ----------------------------------------------

!       sent part
        IF(FLAG==1) THEN
        !       -----------------------------
        !       get the number of sent/received nodes 
                LOC_PROC = ISPMD+1
                IADS(1:NSPMD+1) = 0
                IADR(1:NSPMD+1) = 0
                LENSD = 0
                LENRV = 0
                DO P=1,NSPMD
                        IADR(P)=LENRV+1
                        DO NI=1,NBINTC
                                NIN = INTLIST(NI)
                                NTY = IPARI(7,NIN)
                                ITIED = IPARI(85,NIN)
                                IF(NTY==10 .OR.(NTY==7.AND.ITIED/=0))THEN
                                        LENSD = LENSD + NSNFI(NIN)%P(P)
                                        LENRV = LENRV + NSNSI(NIN)%P(P)
                                ENDIF
                        ENDDO
                ENDDO

                IADR(NSPMD+1)=LENRV+1
        !       allocate the sent/received buffer
                IF(LENSD>0) THEN
                        ALLOCATE(BBUFS(LENSD))
                        BBUFS(1:LENSD) = 0
                ENDIF
                IF(LENRV>0) THEN
                        ALLOCATE(BBUFR(LENRV))
                        BBUFR(1:LENRV) = 0
                ENDIF

        !       received comm
                DO P=1, NSPMD
                        SIZ=IADR(P+1)-IADR(P)
                        IF (SIZ > 0) THEN
                                MSGTYP = MSGOFF
                                CALL MPI_IRECV( BBUFR(IADR(P)),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                                          MPI_COMM_WORLD,REQ_R(P),IERROR )

                        ENDIF
                ENDDO
                
        !       fill the sent buffer
                L=1
                IDEB(1:NINTER) = 0
                DO P=1, NSPMD
                        IADS(P)=L
                        IF (P/= LOC_PROC) THEN
                                DO NI=1,NBINTC
                                        NIN = INTLIST(NI)
                                        NTY   =IPARI(7,NIN)
                                        ITIED = IPARI(85,NIN)
                                        NSN = IPARI(5,NIN)
                                        LEN_CANDF =8
                                        IF(NTY==10) LEN_CANDF=6
                                        IF(NTY==10 .OR. (NTY==7.AND.ITIED/=0)) THEN
                                        !       compute SUM( NSNFI(NIN)%P(1-->P-1))
                                                NUMERO=0
                                                DO P2=1,P-1
                                                        NUMERO=NUMERO+NSNFI(NIN)%P(P2)
                                                ENDDO
                                                NB = NSNFI(NIN)%P(P)
                                                LL = 0
                                                DO NN=1,INTBUF_TAB(NIN)%I_STOK(1)
                                                        NII = INTBUF_TAB(NIN)%CAND_N(NN)
                                                !       check if the remote node is on P processor 
                                                !       --> SUM( NSNFI(NIN)%P(1-->P-1)) < NII-NSN < SUM( NSNFI(NIN)%P(1-->P))
                                                        IF( NII>NSN
     .                                               .AND. ((NII-NSN)>NUMERO)
     .                                               .AND. ((NII-NSN)<=NUMERO+NSNFI(NIN)%P(P)) ) THEN        
                                                        ! remote node        
                                                                IF(INTBUF_TAB(NIN)%CAND_F(LEN_CANDF*(NN-1)+1)/=ZERO) THEN
                                                                        BBUFS(L-1+NII-NSN-NUMERO)= 1
                                                                        LL = LL + 1
                                                                ENDIF
                                                        ENDIF
                                                ENDDO
                                                L = L + NB
                                        ENDIF
                                ENDDO  ! DO NI=1,NBINTC
                                SIZ = L-IADS(P)
                                IF(SIZ>0)THEN
                                        MSGTYP = MSGOFF

                                        CALL MPI_ISEND(BBUFS(IADS(P)),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                                                 MPI_COMM_WORLD,REQ_S(P),IERROR)
                                ENDIF
                        ENDIF ! ENDIF P/= LOC_PROC
                ENDDO  ! DO P=1, NSPMD
        !       end of sent part
        !       -----------------------------
        ELSEIF(FLAG==2) THEN
        !       -----------------------------
        !       receveid part

                L=0
                IDEB(1:NINTER) = 0

                LLL(1:NINTER) = 0
                DO NI=1,NBINTC
                        NIN = INTLIST(NI)
                        NTY   =IPARI(7,NIN)
                        ITIED = IPARI(85,NIN)
                        NSN = IPARI(5,NIN)
                        IF(NTY==10 .OR. (NTY==7.AND.ITIED/=0)) THEN
                                CANDF_SI(NIN)%P(1:NSN)=0
                        ENDIF
                ENDDO

                DO P=1, NSPMD
                        L=0
                        SIZ=IADR(P+1)-IADR(P)
                        IF (SIZ > 0) THEN
                                MSGTYP = MSGOFF
                                CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
                                DO NI=1,NBINTC
                                        NIN = INTLIST(NI)
                                        NTY   =IPARI(7,NIN)
                                        ITIED = IPARI(85,NIN)
                                        NSN = IPARI(5,NIN)
                                !       compute SUM( NSNFI(NIN)%P(1-->P-1))
                                        NUMERO=0
                                        DO P2=1,P-1
                                                NUMERO=NUMERO+NSNSI(NIN)%P(P2)
                                        ENDDO
                                        IF(NTY==10 .OR. (NTY==7.AND.ITIED/=0)) THEN
                                                NB = NSNSI(NIN)%P(P)    
                                                IF (NB > 0)THEN
                                                        DO K=1,NB
                                                                LL = BBUFR(IADR(P)+L)
                                                                IF(LL/=0) THEN
                                                                        SN=NSVSI(NIN)%P(K+IDEB(NIN))
                                                                        CANDF_SI(NIN)%P(SN) = 1
                                                                ENDIF
                                                                L = L + 1
                                                        ENDDO
                                                ENDIF
                                                IDEB(NIN)=IDEB(NIN)+NB
                                        ENDIF ! NTY==7.AND.ITIED/=0
                                ENDDO ! DO NI=1,NBINTC
                        ENDIF !  size > 0
                ENDDO        ! DO P=1, NSPMD

                DO P = 1, NSPMD
                        IF (P==NSPMD)THEN
                                SIZ=LENSD-IADS(P)
                        ELSE
                                SIZ=IADS(P+1)-IADS(P)
                        ENDIF
                        IF(SIZ>0) THEN
                                CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
                        ENDIF
                ENDDO
                
                IF (ALLOCATED(BBUFS)) DEALLOCATE(BBUFS)
                IF (ALLOCATED(BBUFR)) DEALLOCATE(BBUFR)

        !       end of receveid part
        !       -----------------------------
        ENDIF

!       ----------------------------------------------
        

#endif
        RETURN

        END SUBROUTINE SPMD_I7ITIED_CAND  
