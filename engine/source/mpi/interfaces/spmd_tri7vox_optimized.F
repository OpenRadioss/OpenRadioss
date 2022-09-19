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
Chd|  SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|-- called by -----------
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_IALLGATHERV              source/mpi/generic/spmd_iallgatherv.F
Chd|        SPMD_IALLGATHERV_INT          source/mpi/generic/spmd_iallgatherv_int.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        COMM_TRI7VOX_MOD              share/modules/comm_tri7vox_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI7VOX_OPTIMIZED(
     1   NSV     ,NSN      ,X      ,V     ,MS     ,
     2   BMINMAL ,WEIGHT   ,STIFN  ,NIN   ,ISENDTO,
     3   IRCVFROM,IAD_ELEM ,FR_ELEM,NSNR  ,IGAP   ,
     4   GAP_S   ,ITAB     ,KINET  ,IFQ   ,INACTI ,
     5   NSNFIOLD,INTTH    ,IELEC  ,AREAS ,TEMP   ,
     6   NUM_IMP ,NODNX_SMS,GAP_S_L       ,ITYP,
     7   IRTLM   ,I24_TIME_S,I24_FRFI,I24_PENE_OLD,
     8   I24_STIF_OLD ,NBINFLG,ILEV ,I24_ICONT_I  ,
     9   INTFRIC ,IPARTFRICS,ITIED  ,IVIS2, IF_ADH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IFQ, INACTI, IGAP,INTTH,NSNR,INTFRIC,
     .        ITIED, IVIS2,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), KINET(*),
     .        IELEC(*),NUM_IMP, NODNX_SMS(*),IRTLM(*),ITYP,
     .        NBINFLG(*),ILEV,I24_ICONT_I(*),IPARTFRICS(*),IF_ADH(*)

      my_real
     .        X(3,*), V(3,*), MS(*), BMINMAL(*), STIFN(*), GAP_S(*),
     .        AREAS(*),TEMP(*),GAP_S_L(*),I24_TIME_S(*),I24_FRFI(6,*),
     .        I24_PENE_OLD(5,*),I24_STIF_OLD(2,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,
     .        SIZ,J, L, BUFSIZ, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD),NBOX(NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,
     .        RSIZ, ISIZ, L2, REQ_SD3(NSPMD),REQ_RD2(NSPMD),
     .        LEN2, RSHIFT, ISHIFT, ND, JDEB, Q, NBB

      INTEGER :: P_LOC
      INTEGER :: SEND_SIZE_BMINMA
      INTEGER :: REQUEST_BMINMA
      INTEGER, DIMENSION(COMM_TRI7VOX(NIN)%proc_number) :: RCV_SIZE_BMINMA,DISPLS_BMINMA

      INTEGER :: SEND_SIZE_CRVOX
      INTEGER :: REQUEST_CRVOX
      INTEGER, DIMENSION(COMM_TRI7VOX(NIN)%proc_number) :: RCV_SIZE_CRVOX,DISPLS_CRVOX
      my_real, DIMENSION(6) :: BMINMA_LOC
      INTEGER, DIMENSION(0:LRVOXEL,0:LRVOXEL) :: CRVOXEL_LOC


      integer :: key,code

      DATA MSGOFF/6000/
      DATA MSGOFF2/6001/
      DATA MSGOFF3/6002/
      DATA MSGOFF4/6003/
      DATA MSGOFF5/6004/

      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB

      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGNSNFI
      my_real, DIMENSION(:,:), ALLOCATABLE :: XTMP
      INTEGER, DIMENSION(NSPMD) :: TAB_NB

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================
      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL
C
C Sauvegarde valeur ancienne des nsn frontieres
C
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0
     .   .OR.NUM_IMP>0.OR.ITIED/=0.OR.ITYP==23.OR.ITYP==24
     .   .OR.ITYP==25) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = NSNFI(NIN)%P(P)
         END DO
      END IF
C
Cel boite minmax pour le tri provenant de i7buce BMINMA
C
      IF(IRCVFROM(NIN,LOC_PROC)==0.AND.
     .   ISENDTO(NIN,LOC_PROC)==0) RETURN
      IF (IMONM > 0) CALL STARTIME(25,1)
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)

!     --------------------------
!     compute the displacment and size of send/rcv message
!     for allgatherv communications
      SEND_SIZE_BMINMA = 0
      SEND_SIZE_CRVOX = 0
      RCV_SIZE_BMINMA(1:COMM_TRI7VOX(NIN)%proc_number) = 0
      DISPLS_BMINMA(1:COMM_TRI7VOX(NIN)%proc_number) = 0
      RCV_SIZE_CRVOX(1:COMM_TRI7VOX(NIN)%proc_number) = 0
      DISPLS_CRVOX(1:COMM_TRI7VOX(NIN)%proc_number) = 0
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        SEND_SIZE_BMINMA = 6
        SEND_SIZE_CRVOX = (LRVOXEL+1)*(LRVOXEL+1)
      ENDIF

      DO P_LOC = 1, COMM_TRI7VOX(NIN)%PROC_NUMBER
            P =  COMM_TRI7VOX(NIN)%PROC_LIST(P_LOC)
            IF(IRCVFROM(NIN,P)/=0) THEN
                RCV_SIZE_BMINMA(P_LOC) = 6
                RCV_SIZE_CRVOX(P_LOC) = (LRVOXEL+1)*(LRVOXEL+1)
            ENDIF
      ENDDO

      P_LOC=COMM_TRI7VOX(NIN)%RANK+1
      RCV_SIZE_BMINMA(P_LOC) = SEND_SIZE_BMINMA
      RCV_SIZE_CRVOX(P_LOC) = SEND_SIZE_CRVOX

      DO P_LOC = 1, COMM_TRI7VOX(NIN)%PROC_NUMBER
        P =  COMM_TRI7VOX(NIN)%PROC_LIST(P_LOC)
        IF(P>0) THEN
            DISPLS_BMINMA(P_LOC) = (P-1)*6
            DISPLS_CRVOX(P_LOC) = (P-1)*(LRVOXEL+1)*(LRVOXEL+1)
        ELSE
            DISPLS_BMINMA(P_LOC) = 0
            DISPLS_CRVOX(P_LOC) = 0
        ENDIF
      ENDDO
!     --------------------------

!       send/rcv min-max
      BMINMA_LOC(1:6) = BMINMA(1:6,LOC_PROC)
      CALL SPMD_IALLGATHERV(BMINMA_LOC,BMINMA,SEND_SIZE_BMINMA,
     .     6*NSPMD,RCV_SIZE_BMINMA,DISPLS_BMINMA,REQUEST_BMINMA,
     .     COMM_TRI7VOX(NIN)%COMM,COMM_TRI7VOX(NIN)%PROC_NUMBER)

!       send/rcv voxel
      CRVOXEL_LOC(0:LRVOXEL,0:LRVOXEL) = CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)
      CALL SPMD_IALLGATHERV_INT(CRVOXEL_LOC(0,0),CRVOXEL(0,0,1),SEND_SIZE_CRVOX,
     .     (LRVOXEL+1)*(LRVOXEL+1)*NSPMD,RCV_SIZE_CRVOX,DISPLS_CRVOX,REQUEST_CRVOX,
     .     COMM_TRI7VOX(NIN)%COMM,COMM_TRI7VOX(NIN)%PROC_NUMBER)

      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        NBIRECV=0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              NBIRECV=NBIRECV+1
              IRINDEXI(NBIRECV)=P
            ENDIF
          ENDIF
        ENDDO
      ENDIF

C
Cel envoi voxel + boite min/max
C

C
Cel envoi de XREM
C
C computation of real and integer sending buffers sizes
c general case
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

C Interface Adhesion      
      IF(ITYP==25.AND.(IVIS2==-1.AND.INTTH == 0) ) THEN
        RSIZ = RSIZ + 1 ! areas
        ISIZ = ISIZ + 2 ! if_adh+ioldnsnfi
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

c INT24
      IF(ITYP==24)THEN
        RSIZ = RSIZ + 8
        ISIZ = ISIZ + 3
C-----for   NBINFLG      
        IF (ILEV==2) ISIZ = ISIZ + 1

      ENDIF

c INT25
      IF(ITYP==25)THEN
        RSIZ = RSIZ + 3
        ISIZ = ISIZ + 6
C-----for   NBINFLG      
        IF (ILEV==2) ISIZ = ISIZ + 1
      ENDIF
      IDEB = 1

      JDEB = 0
      IF(ITYP==25)THEN
        ALLOCATE(ITAGNSNFI(NUMNOD),STAT=IERROR)
        ITAGNSNFI(1:NUMNOD) = 0
      END IF
      TAB_NB(1:NSPMD) = 0

#if _PLMPI
!   -------------------------
!   PLMPI uses MPI-2.x version without non blocking allgatherv comm
!   -------------------------
#else
!   -------------------------
!   wait the previous comm
      CALL MPI_WAIT(REQUEST_BMINMA,STATUS,IERROR)
      CALL MPI_WAIT(REQUEST_CRVOX,STATUS,IERROR)
!   -------------------------
#endif

      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN

        DO KK = 1, NBIRECV
           P=IRINDEXI(KK)
C Traitement special sur d.d. ne consever que les noeuds internes
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C weight < 0 temporairement pour ne conserver que les noeuds non frontiere
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
          L = IDEB
          NBOX(P) = 0
          NB = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)
          DO I=1,NSN
            NOD = NSV(I)
            IF(WEIGHT(NOD)==1)THEN
             IF(STIFN(I)>ZERO)THEN
               IF(ITYP==25.AND.IRTLM(4*(I-1)+4)==P)THEN
                 NB = NB + 1
                 INDEX(NB) = I
               ELSEIF(ITIED/=0.AND.ITYP==7.AND.CANDF_SI(NIN)%P(I)/=0) THEN
                 NB = NB + 1
                 INDEX(NB) = I
               ELSE
                 IF(X(1,NOD) < XMINB)  CYCLE
                 IF(X(1,NOD) > XMAXB)  CYCLE
                 IF(X(2,NOD) < YMINB)  CYCLE
                 IF(X(2,NOD) > YMAXB)  CYCLE
                 IF(X(3,NOD) < ZMINB)  CYCLE
                 IF(X(3,NOD) > ZMAXB)  CYCLE

                 IX=INT(NBX*(X(1,NOD)-XMINB)/(XMAXB-XMINB))
                 IF(IX >= 0 .AND. IX <= NBX) THEN
                   IY=INT(NBY*(X(2,NOD)-YMINB)/(YMAXB-YMINB))
                   IF(IY >= 0 .AND. IY <= NBY) THEN
                     IZ=INT(NBZ*(X(3,NOD)-ZMINB)/(ZMAXB-ZMINB))
                     IF(IZ >= 0 .AND. IZ <= NBZ) THEN
                       IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
                         NB = NB + 1
                         INDEX(NB) = I
                       ENDIF
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF
            ENDIF
          ENDDO
          NBOX(P) = NB
C
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C remise de weight > 0
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C old tag
          IF(ITYP==25)THEN
            JDEB = 0
            DO Q=1,P-1
              JDEB = JDEB + NSNSI(NIN)%P(Q)
            END DO
            NBB = NSNSI(NIN)%P(P)
            DO J = 1, NBB
              ND = NSVSI(NIN)%P(JDEB+J)
              NOD= NSV(ND)
              ITAGNSNFI(NOD)=J
            END DO
          END IF
C
C Envoi taille msg
C
          MSGTYP = MSGOFF3
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB>0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
            ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)
             IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
            L2= 0

#include      "vectorize.inc"
            DO J = 1, NB
               I = INDEX(J)
               NOD = NSV(I)
               RBUF(P)%p(L+1) = X(1,NOD)
               RBUF(P)%p(L+2) = X(2,NOD)
               RBUF(P)%p(L+3) = X(3,NOD)
               RBUF(P)%p(L+4) = V(1,NOD)
               RBUF(P)%p(L+5) = V(2,NOD)
               RBUF(P)%p(L+6) = V(3,NOD)
               RBUF(P)%p(L+7) = MS(NOD)
               RBUF(P)%p(L+8) = STIFN(I)
               IBUF(P)%p(L2+1) = I
               IBUF(P)%p(L2+2) = ITAB(NOD)
               IBUF(P)%p(L2+3) = KINET(NOD)
!     save specifics IREM and XREM indexes for INT24 sorting
               IBUF(P)%p(L2+4) = 0 !IGAPXREMP
               IBUF(P)%p(L2+5) = 0 !I24XREMP
               IBUF(P)%p(L2+6) = 0 !I24IREMP
               L = L + RSIZ
               L2 = L2 + ISIZ
            END DO

c shift for real variables (prepare for next setting)
            RSHIFT = 9
c shift for integer variables (prepare for next setting)
            ISHIFT = 7

c specific cases
c IGAP=1 or IGAP=2
            IF(IGAP==1 .OR. IGAP==2)THEN
               L = 0
               IGAPXREMP = RSHIFT
#include      "vectorize.inc"
               DO J = 1, NB
                 I = INDEX(J)
                 RBUF(P)%p(L+RSHIFT)= GAP_S(I)
                 L = L + RSIZ
               ENDDO
               RSHIFT = RSHIFT + 1

c IGAP=3
            ELSEIF(IGAP==3)THEN
               L = 0
               IGAPXREMP = RSHIFT
#include      "vectorize.inc"
               DO J = 1, NB
                 I = INDEX(J)
                 RBUF(P)%p(L+RSHIFT)  = GAP_S(I)
                 RBUF(P)%p(L+RSHIFT+1)= GAP_S_L(I)
                 L = L + RSIZ
               END DO
               RSHIFT = RSHIFT + 2
            ENDIF

C thermic
            IF(INTTH>0)THEN
               L = 0
               L2 = 0
#include      "vectorize.inc"
               DO J = 1, NB
                 I = INDEX(J)
                 NOD = NSV(I)
                 RBUF(P)%p(L+RSHIFT)   = TEMP(NOD)
                 RBUF(P)%p(L+RSHIFT+1) = AREAS(I)
                 IBUF(P)%p(L2+ISHIFT) = IELEC(I)
                 L = L + RSIZ
                 L2 = L2 + ISIZ
               END DO
               RSHIFT = RSHIFT + 2
               ISHIFT = ISHIFT + 1
            ENDIF

C Interface Adhesion
            IF(ITYP==25.AND.IVIS2==-1.AND.INTTH==0)THEN
              L = 0
              L2 = 0
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
                RBUF(P)%p(L+RSHIFT) = AREAS(I)
                IBUF(p)%p(L2+ISHIFT) = IF_ADH(I)
                IBUF(P)%p(L2+ISHIFT+1)=ITAGNSNFI(NOD)
                L = L + RSIZ
                L2 = L2 + ISIZ
              END DO
              RSHIFT = RSHIFT + 1
              ISHIFT = ISHIFT + 2
            ENDIF

C Friction
            IF(INTFRIC>0)THEN
               L2 = 0
#include      "vectorize.inc"
               DO J = 1, NB
                 I = INDEX(J)
                 IBUF(P)%p(L2+ISHIFT) = IPARTFRICS(I)
                 L2 = L2 + ISIZ
               END DO
               ISHIFT = ISHIFT + 1
            ENDIF

C -- IDTMINS==2
            IF(IDTMINS==2)THEN
               L2 = 0
#include      "vectorize.inc"
               DO J = 1, NB
                 I = INDEX(J)
                 NOD = NSV(I)
                 IBUF(P)%p(L2+ISHIFT)  = NODNX_SMS(NOD)
                 IBUF(P)%p(L2+ISHIFT+1)= NOD
                 L2 = L2 + ISIZ
               END DO
               ISHIFT = ISHIFT + 2

C -- IDTMINS_INT /= 0               
            ELSEIF(IDTMINS_INT/=0)THEN
              L2 = 0
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
                IBUF(P)%p(L2+ISHIFT)= NOD
                L2 = L2 + ISIZ
              END DO
              ISHIFT = ISHIFT + 1
            ENDIF

c INT24
            IF(ITYP==24)THEN

              L = 0
              I24XREMP = RSHIFT
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                RBUF(P)%p(L+RSHIFT)    =I24_TIME_S(I)
                RBUF(P)%p(L+RSHIFT+1)  =I24_FRFI(1,I)
                RBUF(P)%p(L+RSHIFT+2)  =I24_FRFI(2,I)
                RBUF(P)%p(L+RSHIFT+3)  =I24_FRFI(3,I)
                RBUF(P)%p(L+RSHIFT+4)  =I24_PENE_OLD(1,I)
                RBUF(P)%p(L+RSHIFT+5)  =I24_STIF_OLD(1,I)
                RBUF(P)%p(L+RSHIFT+6)  =I24_PENE_OLD(3,I)
                RBUF(P)%p(L+RSHIFT+7)  =I24_PENE_OLD(5,I)
                L = L + RSIZ
              END DO
              RSHIFT = RSHIFT + 8

              L2 = 0
              I24IREMP = ISHIFT
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
C               IRTLM(2,NSN) in TYPE24
                IBUF(P)%p(L2+ISHIFT)  =IRTLM(2*(I-1)+1)
                IBUF(P)%p(L2+ISHIFT+1)=IRTLM(2*(I-1)+2)
                IBUF(P)%p(L2+ISHIFT+2)=I24_ICONT_I(I)
                L2 = L2 + ISIZ
              END DO
              ISHIFT = ISHIFT + 3
C---pay attention in i24sto.F IREM(I24IREMP+3,N-NSN) is used, 
C----change the shift value when new tabel was added like I24_ICONT_I                 
              IF (ILEV==2) THEN
                    L2 = 0
#include      "vectorize.inc"
                DO J = 1, NB
                  I = INDEX(J)
                  IBUF(P)%p(L2+ISHIFT)=NBINFLG(I)
                  L2 = L2 + ISIZ
                END DO
              END IF
              ISHIFT = ISHIFT + 1

            END IF !(ITYP==24)

c INT25
            IF(ITYP==25)THEN
              L = 0
              I24XREMP = RSHIFT
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                RBUF(P)%p(L+RSHIFT)    =I24_TIME_S(2*(I-1)+1)
                RBUF(P)%p(L+RSHIFT+1)  =I24_TIME_S(2*(I-1)+2)
                RBUF(P)%p(L+RSHIFT+2)  =I24_PENE_OLD(5,I) !  used only at time=0
                L = L + RSIZ
              END DO
              RSHIFT = RSHIFT + 3

              L2 = 0
              I24IREMP = ISHIFT

#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
C               IRTLM(3,NSN) en TYPE25 / IRTLM(3,-) inutile ici 
                IBUF(P)%p(L2+ISHIFT)  =IRTLM(4*(I-1)+1)
                IBUF(P)%p(L2+ISHIFT+1)=IRTLM(4*(I-1)+2)
C
C               IRTLM(3,I) == local n    of the impacted segment is shared but only valid on proc == IRTLM(4,I)
                IBUF(P)%p(L2+ISHIFT+2)=IRTLM(4*(I-1)+3)
                IBUF(P)%p(L2+ISHIFT+3)=IRTLM(4*(I-1)+4)
                IBUF(P)%p(L2+ISHIFT+4)=I24_ICONT_I(I)
                IBUF(P)%p(L2+ISHIFT+5)=ITAGNSNFI(NOD)
                L2 = L2 + ISIZ
              END DO
              ISHIFT = ISHIFT + 6
C---pay attention in i25sto.F IREM(I24IREMP+4,N-NSN) is used, 
C----change the shift value when new tabel was added like IRTLM(3*(I-1)+2)                
              IF (ILEV==2) THEN
                    L2 = 0
#include      "vectorize.inc"
                DO J = 1, NB
                  I = INDEX(J)
                  IBUF(P)%p(L2+ISHIFT)=NBINFLG(I)
                  L2 = L2 + ISIZ
                END DO
              END IF
              ISHIFT = ISHIFT + 1

            END IF !(ITYP==25)
C
            !save specifics IREM and XREM indexes for INT24 sorting
            L2 = 0
#include      "vectorize.inc"
            DO J = 1, NB
              I = INDEX(J)
              NOD = NSV(I)
              !save specifics IREM and XREM indexes for INT24 sorting
              IBUF(P)%p(L2+4) = IGAPXREMP
              IBUF(P)%p(L2+5) = I24XREMP
              IBUF(P)%p(L2+6) = I24IREMP
              L2 = L2 + ISIZ
            END DO
            TAB_NB(P) = NB
          ENDIF
C
C reset old tag for next P
          IF(ITYP==25)THEN
            NBB = NSNSI(NIN)%P(P)
            DO J = 1, NBB
              ND = NSVSI(NIN)%P(JDEB+J)
              NOD= NSV(ND)
              ITAGNSNFI(NOD)=0
            END DO
          END IF
        ENDDO
      ENDIF
C
      IF(ITYP==25) DEALLOCATE(ITAGNSNFI)
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              MSGTYP = MSGOFF3
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P)>0) THEN
                L=L+1
                ISINDEXI(L)=P
                NSNR = NSNR + NSNFI(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
C
C Allocate total size
C

       IF(NSNR>0) THEN

          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR)


          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
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

      DO P=1,NSPMD
        IF(TAB_NB(P) /= 0 ) THEN
            MSGTYP = MSGOFF4
            CALL MPI_ISEND(
     1        RBUF(P)%P(1),TAB_NB(P)*RSIZ,REAL,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
            MSGTYP = MSGOFF5
            CALL MPI_ISEND(
     1        IBUF(P)%P(1),TAB_NB(P)*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD3(P),ierror)
        ENDIF
      ENDDO

      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
       IF(NSNR>0) THEN
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
            CALL MPI_WAITANY(NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)
          ENDDO
          !set specifics IREM and XREM indexes for INT24 sorting
          IGAPXREMP = IREM(4,1)
          I24XREMP  = IREM(5,1)
          I24IREMP  = IREM(6,1)
        ENDIF
      ENDIF
C
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
              IF(NBOX(P)/=0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
                CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C

      IF (IMONM > 0) CALL STOPTIME(25,1)
C
#endif
      RETURN
      END SUBROUTINE SPMD_TRI7VOX_OPTIMIZED
