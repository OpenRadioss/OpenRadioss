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
Chd|  SPMD_EXCH_SMST2               source/mpi/ams/spmd_exch_smst2.F
Chd|-- called by -----------
Chd|        SMS_INI_INT                   source/ams/sms_init.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_CHECK_TAG                source/mpi/ams/spmd_check_tag.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SMST2(IPARI,TAG,INTLIST,NBINTC,INTBUF_TAB)
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
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NBINTC
      INTEGER, DIMENSION(NPARI,*), INTENT(IN) :: IPARI
      INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: TAG(NUMNOD)
      INTEGER, DIMENSION(*), INTENT(IN) :: INTLIST(*)
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
! ********************************************************
! *  variable  *  type  *  size   *  intent  *  feature
! *------------*--------*---------*----------*-------------
! *    NBINTC  * integ. *   1     *    in    *  number of interf. (/=2)
! *    INTLIST * integ. * NBINTC  *    in    *  kind of interface (/=2)
! *   IPARI    * integ. * NPARI,: *    in    *  size of TAG
! *    TAG     * integ. * NUMNOD  *    in    *  tag array
! * INTBUF_TAB * struct.*         *    inout *  interface pointer
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),
     *        REQ_SI(PARASIZ),REQ_RI(PARASIZ),REQ_S(PARASIZ),
     *        REQ_S2(PARASIZ),REQ_R(PARASIZ),REQ_R2(PARASIZ)
      INTEGER P,LENSD,LENRV,IADS(PARASIZ+1),IADR(PARASIZ+1),IERROR,
     *        SIZ,LOC_PROC,MSGTYP,IDEBS(NINTER),IDEBR(NINTER),IDB,PROC,
     *        MSGOFF,MSGOFF2,LENSD_0,LENRV_0
      INTEGER IADINT(NINTER,NSPMD)

      INTEGER I,J,L,NB,NL,NN,K,N,LEN,ND,FLG,NIN,NTY,
     *        NSN,SN,SSIZ,
     *        IT,LEN_NSNSI,NSNR,NI,NP,ALEN,NOD,NOD1,NOD2,
     *        SIZE_LOC, I_STOK,INACTI,IFQ,ITIED,NRTS
!     Exchanged arrays for sent buffer 
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAB_SEND,TAB_LOC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFS, IBUFR

      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: TAG_LOC,TAB_NSVSI
C     REAL ou REAL*8
      DATA MSGOFF/240/
      DATA MSGOFF2/241/
!       -----------------------------------------------
!       at tt = 0 and with AMS, one must eliminate cand_a/e for
!       type 2 interface and ilev/+25 or 26
!       ( if NTY==2 .AND. ILEV/=25 .and. ILEV /= 26 )
!
!       NSNSI and NSVSI are unknown at tt = 0
!       --> one must build NSNSI and NSVSI one each process/domain
!           using NSNFI and NSVFI arrays located on the others domain
!
!       1srt comm : building of NSNSI array (for ispmd domain) with NSNFI array (located on p domain, p/=ispmd) 
!       2rd comm : building of NSVSI array (for ispmd domain) with NSVFI array (located on p domain, p/=ispmd) !      
!       3rd comm  : comm of tag array (each ispmd domain sends NSNSI values of tag arrays to p domain) 
!       chek the value of tag : if tag = -1, cand_a/e are deleted   
!       -----------------------------------------------
        IF(NSPMD == 1)RETURN
         
        IERROR = 0       
        IF(NSPMD>1) THEN
        ! -----------------------
            ! FIRST COMM.
            ! building of NSNSI
            ! tab_loc : sent buffer (=NSNFI)
            ! tab_send : recv. buffer (-->NSNSI) 
            IF(NINTER>0) THEN
             ALLOCATE(TAB_SEND(NINTER*NSPMD), STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF
             ALLOCATE(TAB_LOC(NINTER*NSPMD), STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF
             TAB_LOC(1:NINTER*NSPMD) = 0
            ENDIF       ! ninter/=0
!
            LOC_PROC = ISPMD+1
            IADS(1:NSPMD+1) = 0
            IADR(1:NSPMD+1) = 0
            LENSD_0 = 0
            LENRV_0 = 0

            ALEN = 1
            DO P=1,NSPMD
              IADS(P)=LENSD_0+1
              DO NI=1,NBINTC
	         NIN = INTLIST(NI)
                 NTY=IPARI(7,NIN)
                 IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
                    LENSD_0 = LENSD_0 + NSNFI(NIN)%P(P)*ALEN
                    TAB_LOC(NIN+NINTER*(P-1)) = TAB_LOC(NIN+NINTER*(P-1)) + NSNFI(NIN)%P(P)
                 ENDIF
              ENDDO
            ENDDO
            IADS(NSPMD+1)=LENSD_0+1

            ! AlltoAll comm in order to know the sent buffer size
            CALL MPI_ALLTOALL(TAB_LOC(1),NINTER,MPI_INTEGER,
     .                        TAB_SEND(1),NINTER,MPI_INTEGER,
     .                        MPI_COMM_WORLD,IERROR)

        ! -----------------------
            ! SECOND COMM.
            ! building of NSVSI
            ! IBUFS : sent buffer (=NSVFI)
            ! IBUFR : recv. buffer (-->NSVSI)
            ! TAB_NSVSI : NSVSI array 
            DO P=1,NSPMD
              IADR(P) = LENRV_0 + 1
              DO NI=1,NBINTC
	         NIN = INTLIST(NI)
                 NTY=IPARI(7,NIN)
                 IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
                    LENRV_0 = LENRV_0 + TAB_SEND(NIN+NINTER*(P-1))*ALEN
                 ENDIF
              ENDDO
            ENDDO
            IADR(NSPMD+1) = LENRV_0 + 1

            ! Sent buffer allocation
            IF(LENSD_0>0) THEN
             ALLOCATE(IBUFS(LENSD_0),STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF
            ENDIF

            ! Received buffer allocation
            IF(LENRV_0>0) THEN
             ALLOCATE(IBUFR(LENRV_0),STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF
             ALLOCATE(TAB_NSVSI(NINTER),STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF

             DO NI=1,NBINTC
	      NIN = INTLIST(NI)
              NTY = IPARI(7,NIN)
              IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
                NSNR = IPARI(24,NIN)
                LENRV_0 = 0
                DO P=1,NSPMD
                 LENRV_0 = LENRV_0 + TAB_SEND(NIN+NINTER*(P-1))
                ENDDO
                ALLOCATE(TAB_NSVSI(NIN)%P(LENRV_0), STAT=IERROR)
                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                ENDIF
                TAB_NSVSI(NIN)%P(1:LENRV_0) = 0
              ENDIF
             ENDDO
            ENDIF
            ! -----------------
            ! Recieved MPI comm
            DO P=1, NSPMD
              SIZ=IADR(P+1)-IADR(P)
              IF (SIZ > 0) THEN
                 MSGTYP = MSGOFF
                 CALL MPI_IRECV( IBUFR(IADR(P)),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                           MPI_COMM_WORLD,REQ_R(P),IERROR )
              ENDIF
            ENDDO
            ! -----------------
            ! Sent MPI comm
            L=1
            IDEBS=0
            DO P=1, NSPMD
              IADS(P)=L
              IF (P/= LOC_PROC) THEN
                DO NI=1,NBINTC
	          NIN = INTLIST(NI)
                  NTY = IPARI(7,NIN)
                  IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11) THEN
                    NB = NSNFI(NIN)%P(P) ! size = nsnfi
                    IF(NB>0) THEN
                     DO NN=1,NB
                      IBUFS(L) = NSVFI(NIN)%P(IDEBS(NIN)+NN)
                      L=L+ALEN                
                     ENDDO
                     IDEBS(NIN)=IDEBS(NIN)+NB
                    ENDIF
                  ENDIF
                ENDDO  ! DO NIN=1,NINTER
                SIZ = L-IADS(P)
                IF(SIZ.GT.0)THEN
                  MSGTYP = MSGOFF
                  CALL MPI_ISEND( IBUFS(IADS(P)),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                            MPI_COMM_WORLD,REQ_SI(P),IERROR )
                ENDIF
              ENDIF ! ENDIF P/= LOC_PROC
            ENDDO  ! DO P=1, NSPMD
            ! -----------------
            ! Recieve
            L=0
            IDEBR = 0                

            DO P=1, NSPMD
              L=0
              SIZ=IADR(P+1)-IADR(P)
              IF (SIZ > 0) THEN
                MSGTYP = MSGOFF
                ! MPI WAIT
                CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
                DO NI=1,NBINTC
	         NIN = INTLIST(NI)
                 NTY = IPARI(7,NIN)
                 IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
                  NB = TAB_SEND(NIN+NINTER*(P-1)) ! nsnfi becomes nsnsi on local proc
                  IF (NB > 0)THEN
                   DO K=1,NB
                     TAB_NSVSI(NIN)%P(IDEBR(NIN)+K) =  IBUFR(IADR(P)+L) ! nsvfi becomes nsvsi on local proc
                     L=L+ALEN
                    ENDDO
                    IDEBR(NIN)=IDEBR(NIN)+NB
                  ENDIF
                 ENDIF ! ity==7
                ENDDO
              ENDIF !  IF (NB > 0)
!              L=L+SIZ
            ENDDO   ! DO P=1, NSPMD

            ! -----------------
            ! Fin du send
            DO P = 1, NSPMD
              IF (P==NSPMD)THEN
                SIZ=LENSD_0-IADS(P)
              ELSE
                SIZ=IADS(P+1)-IADS(P)
              ENDIF
              IF(SIZ.GT.0) THEN
               CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
              ENDIF
            ENDDO
            ! -----------------
        ! -----------------------
            ! THIRD COMM.
            ! building of tag array
            ! IBUFS : sent buffer
            ! IBUFR : recv. buffer
            ! TAG_LOC is used as tag array 
            !    example for nspmd = 5,       TAG_LOC(nin)%P 
            !         nin     |------*---*-----*-------------*-----|
            !                   P1   * P2*  P3 *    P4       *  P5
            IF(ALLOCATED(IBUFS)) DEALLOCATE(IBUFS)
            IF(ALLOCATED(IBUFR)) DEALLOCATE(IBUFR)
            LENSD = 0
            LENRV = 0
            ALEN = 1
            IADS(:) = 0
            IADR(:) = 0
            DO P=1,NSPMD
              IADS(P)=LENSD+1
              IADR(P)=LENRV+1
              DO NI=1,NBINTC
	         NIN = INTLIST(NI)
                 NTY=IPARI(7,NIN)
                 IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
                    LENSD = LENSD + TAB_SEND(NIN+NINTER*(P-1))*ALEN
                    LENRV = LENRV + NSNFI(NIN)%P(P)*ALEN
                 ENDIF
              ENDDO
            ENDDO
            IADS(NSPMD+1)=LENSD+1
            IADR(NSPMD+1)=LENRV+1
            ! -----------------
            ! Sent buffer allocation
            IF(LENSD>0) THEN
             ALLOCATE(IBUFS(LENSD),STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF
            ENDIF
            ! -----------------
            ! Received buffer allocation
            IF(LENRV>0) THEN
             ALLOCATE(IBUFR(LENRV),STAT=IERROR)
             IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
             ENDIF
            ENDIF 
            ! -----------------
            ! Recieved MPI comm
            DO P=1, NSPMD
              SIZ=IADR(P+1)-IADR(P)
              IF (SIZ > 0) THEN
                 MSGTYP = MSGOFF2
                 CALL MPI_IRECV( IBUFR(IADR(P)),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                           MPI_COMM_WORLD,REQ_R(P),IERROR )
              ENDIF
            ENDDO
            ! -----------------
            ! Sent MPI comm
            L=1
            IDEBS = 0
            DO P=1, NSPMD
              IADS(P)=L
              IF (P/= LOC_PROC) THEN
                DO NI=1,NBINTC
	          NIN = INTLIST(NI)
                  NTY = IPARI(7,NIN)
                  IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25) THEN
                    NB = TAB_SEND(NIN+NINTER*(P-1)) ! size = nsnsi
                    IF(NB>0) THEN
                     DO NN=1,NB
                      ND = TAB_NSVSI(NIN)%P(IDEBS(NIN)+NN)      ! = nsvsi
                      NOD = INTBUF_TAB(NIN)%NSV(ND)
                      IBUFS(L)=  TAG(NOD)
                      L=L+ALEN
                     ENDDO
                     IDEBS(NIN)=IDEBS(NIN)+NB
                    ENDIF
                  ELSEIF (NTY == 11)THEN
                    NB = TAB_SEND(NIN+NINTER*(P-1)) ! size = nsnsi
                    IF(NB>0) THEN
                     DO NN=1,NB
                       ND = TAB_NSVSI(NIN)%P(IDEBS(NIN)+NN)      ! = nsvsi
                       NOD1 = INTBUF_TAB(NIN)%IRECTS(2*(ND-1)+1)
                       NOD2 = INTBUF_TAB(NIN)%IRECTS(2*(ND-1)+2)
                       IF(TAG(NOD1)==1 .OR.TAG(NOD2)==1)THEN
                          IBUFS(L)=  1
                       ELSE
                          IBUFS(L)=  0
                       ENDIF
                       L = L+ALEN
                     ENDDO
                     IDEBS(NIN)=IDEBS(NIN)+NB
                    ENDIF
                  ENDIF
                ENDDO  ! DO NIN=1,NINTER
                SIZ = L-IADS(P)
                IF(SIZ.GT.0)THEN
                  MSGTYP = MSGOFF2
                  CALL MPI_ISEND( IBUFS(IADS(P)),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                            MPI_COMM_WORLD,REQ_SI(P),IERROR )
                ENDIF
              ENDIF ! ENDIF P/= LOC_PROC
            ENDDO  ! DO P=1, NSPMD

            ! -----------------
            ! Recv. MPI comm
            ALLOCATE( TAG_LOC(NINTER) )
            DO NI=1,NBINTC
	     NIN = INTLIST(NI)
             NTY = IPARI(7,NIN)
             IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
               NSNR = IPARI(24,NIN)
               ALLOCATE(TAG_LOC(NIN)%P(NSNR), STAT=IERROR)
               IF(IERROR/=0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
               ENDIF
               TAG_LOC(NIN)%P(1:NSNR) = 0
             ENDIF
            ENDDO
            ! -----------------
            ! building of tag array (TAG_LOC) as a structure 
            L=0
            IDEBR(:) = 0
            DO P=1, NSPMD
              L=0
              SIZ=IADR(P+1)-IADR(P)
              IF (SIZ > 0) THEN
                MSGTYP = MSGOFF2
                ! MPI WAIT
                CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
                
                DO NI=1,NBINTC
	         NIN = INTLIST(NI)
                 NTY = IPARI(7,NIN)
                 IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11)THEN
                  NB = NSNFI(NIN)%P(P) ! size = nsnfi
                  IF (NB > 0)THEN
                   DO K=1,NB
                     ND = IDEBR(NIN)+K
                     TAG_LOC(NIN)%P(ND) = - IBUFR(IADR(P)+L)
                     L=L+ALEN
                   ENDDO
                   IDEBR(NIN) = IDEBR(NIN) + NB
                  ENDIF
                 ENDIF ! ity==7
                ENDDO
              ENDIF !  IF (NB > 0)
            ENDDO ! DO P=1, NSPMD
            ! -----------------
            ! Fin du send
            DO P = 1, NSPMD
              IF (P==NSPMD)THEN
                SIZ=LENSD-IADS(P)
              ELSE
                SIZ=IADS(P+1)-IADS(P)
              ENDIF
              IF(SIZ.GT.0) THEN
               CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
              ENDIF
            ENDDO
        ! -----------------------
            ! Chek the value of tag and elimination of cand_a/e if tag = -1
            DO NI=1,NBINTC
	      NIN = INTLIST(NI)
              NTY = IPARI(7,NIN)
              NSN = IPARI(5,NIN)
              NSNR = IPARI(24,NIN)
              I_STOK = INTBUF_TAB(NIN)%I_STOK(1)
              INACTI = IPARI(22,NIN)
              IFQ    = IPARI(31,NIN)
              ITIED  = IPARI(85,NIN)
              NRTS = IPARI(3,NIN) 
              IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.NTY==24.OR.NTY==25.OR.NTY==11) THEN
                  CALL SPMD_CHECK_TAG(NIN,I_STOK,INTBUF_TAB(NIN),TAG_LOC(NIN)%P,
     .                                 NSNR,NSN,NTY,INACTI,IFQ,ITIED,NRTS)
              ENDIF
              INTBUF_TAB(NIN)%I_STOK(1) = I_STOK  
            ENDDO

            IF(ALLOCATED(IBUFS)) DEALLOCATE(IBUFS)
            IF(ALLOCATED(IBUFR)) DEALLOCATE(IBUFR)
            IF(ALLOCATED(TAB_NSVSI)) DEALLOCATE(TAB_NSVSI)
            IF(ALLOCATED(TAB_LOC)) DEALLOCATE(TAB_LOC)
            IF(ALLOCATED(TAB_SEND)) DEALLOCATE(TAB_SEND)
            DEALLOCATE( TAG_LOC )
        ! -----------------------
        ENDIF ! NSPMD > 1
#endif
        RETURN
        END SUBROUTINE



