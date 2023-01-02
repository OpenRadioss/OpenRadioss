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

C
Chd|====================================================================
Chd|  SPMD_EXCHSEG_IDEL             source/mpi/kinematic_conditions/spmd_exchseg_idel.F
Chd|-- called by -----------
Chd|        CHKLOAD                       source/interfaces/chkload.F   
Chd|-- calls ---------------
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|====================================================================
      SUBROUTINE SPMD_EXCHSEG_IDEL(
     1    BUFS  ,LBUFS ,IXS    ,IXC    ,IXTG   ,
     2    IXQ   ,IPARG ,ITAGL  ,ITABM1 ,TAGEL  ,
     3    IRSIZE,IRECV ,CNEL   ,ADDCNEL,OFC    ,
     4    OFT   ,OFTG  ,OFUR   ,OFR    ,OFP    ,
     5    OFQ   ,LINDEX,IXP    ,IXR    ,IXT    ,
     6    GEO   ,IAD_ELEM)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXT(NIXT,*), TAGEL(*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITABM1(*),
     .        BUFS(*),ITAGL(*), IRECV(*), CNEL(0:*), ADDCNEL(0:*),
     .        IRSIZE, LBUFS, OFC, OFT, OFTG, OFUR, OFR, OFP, LINDEX,
     .        OFQ
      INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
      my_real
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF2 ,MSGOFF3, MSGTYP, LOC_PROC,
     .        IERROR,I, IDEB, LEN, N1, N2, N3, N4,
     .        K, IX, LFT, LLT, II, NN, J, IOFF,
     .        JD(50), KD(50), NG, IDEL, ITYP, NBEL,
     .        ITY, MLW, NEL, NFT, KAD, NPT, ISTRA, IHBE,
     .        REQ_S2(NSPMD),REQ_S3(NSPMD),STATUS(MPI_STATUS_SIZE),
     .        REQ_R1(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE ::  BUFR, BUFS2
      INTEGER :: SIZ

      DATA MSGOFF2/188/
      DATA MSGOFF3/189/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
C-----------------------------------------------    
      ALLOCATE(BUFR(IRSIZE))
      LOC_PROC = ISPMD+1

      IDEB = 1
      IOFF = 0
      LEN = 0
      REQ_R1(1:NSPMD) = MPI_REQUEST_NULL
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I/=LOC_PROC.AND.IRECV(I)>0.AND.SIZ>0) THEN
          MSGTYP = MSGOFF2 
          CALL MPI_IRECV(
     .      BUFR(IDEB),IRECV(I),MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,REQ_R1(I),IERROR)
          LEN = LEN+IRECV(I)
          IDEB = LEN + 1 
         ENDIF
       ENDDO


C Proc sends the same BUFS to everybody
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I/=LOC_PROC.AND.LBUFS>0.AND.SIZ>0) THEN
           MSGTYP = MSGOFF2
           CALL MPI_ISEND(
     C      BUFS,LBUFS,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S2(I),IERROR)
        ENDIF
      ENDDO

      IDEB = 0
      IOFF = 0
      LEN = 0
C Proc receive only if IRECV(I) > 0
C
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I/=LOC_PROC.AND.IRECV(I)>0.AND.SIZ>0) THEN
          MSGTYP = MSGOFF2 
          CALL MPI_WAIT(REQ_R1(I),STATUS,IERROR)
          LEN = LEN+IRECV(I)
          NBEL = IRECV(I)/FOUR
          IRECV(I)=0

          DO NN = 1, NBEL
             N1 = SYSFUS2(BUFR(IDEB+4*(NN-1)+1),ITABM1,NUMNOD)
             BUFR(NN+IOFF) = 0
             IF(N1/=0) THEN

              N2 = SYSFUS2(BUFR(IDEB+4*(NN-1)+2),ITABM1,NUMNOD)
              IF(N2/=0) THEN

               N3 = SYSFUS2(BUFR(IDEB+4*(NN-1)+3),ITABM1,NUMNOD)
               IF(N3/=0) THEN

                N4 = SYSFUS2(BUFR(IDEB+4*(NN-1)+4),ITABM1,NUMNOD)
                IF(N4/=0) THEN

                 DO J=ADDCNEL(N1),ADDCNEL(N1+1)-1
                  II = CNEL(J)
                  IF(TAGEL(II)>0) THEN    !    elt actif found
                    ITAGL(N1) = 0
                    ITAGL(N2) = 0
                    ITAGL(N3) = 0
                    ITAGL(N4) = 0
                    IF(II<=OFC) THEN ! solide actif
                      DO K = 2, 9
                       IX = IXS(K,II)
                       ITAGL(IX) = 1
                      END DO
                    ELSEIF(II>OFQ.AND.II<=OFC) THEN ! Quad actif
                      II = II - OFQ
                      DO K=2,5
                        IX = IXQ(K,II)
                        ITAGL(IX)=1
                      END DO
                    ELSEIF(II>OFC.AND.II<=OFT) THEN ! shell actif
                      II = II - OFC
                      DO K=2,5
                        IX = IXC(K,II)
                        ITAGL(IX)=1
                      END DO
                    ELSEIF(II>OFTG.AND.II<=OFUR)THEN  ! triangle actif
                      II = II - OFTG
                      DO K=2,4
                        IX = IXTG(K,II)
                        ITAGL(IX) = 1
                      END DO
                    END IF

                    IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4)==4)THEN
                      BUFR(NN+IOFF) = 1
                      GOTO 410
                    ENDIF

                  END IF

                 ENDDO
 410             CONTINUE

                ENDIF
               ENDIF
              ENDIF
             ENDIF
            END DO
            IDEB = IDEB + 4*NBEL

           IOFF = IOFF + NBEL
           IRECV(I)=IRECV(I)+NBEL

        ENDIF
      ENDDO
C
C Envoi BUFR
C
      IDEB = 1
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I/=LOC_PROC.AND.IRECV(I)>0.AND.SIZ>0) THEN
          LEN = IRECV(I)
          MSGTYP = MSGOFF3
          CALL MPI_ISEND(
     C      BUFR(IDEB),LEN,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S3(I),IERROR)
          IDEB = IDEB + LEN
        ENDIF
      ENDDO
C
C Test reception envoi BUFS
C
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I/=LOC_PROC.AND.LBUFS>0.AND.SIZ>0) THEN
          CALL MPI_WAIT(REQ_S2(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
C Reception BUFR dans BUFS2
C
      ALLOCATE(BUFS2(LINDEX))
      IF(LINDEX>0) THEN
        DO I = 1, LINDEX
          BUFS(I) = 0
        ENDDO
        DO I = 1, NSPMD
          SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
          IF(I/=LOC_PROC.AND.LINDEX>0.AND.SIZ>0) THEN
            MSGTYP = MSGOFF3
            CALL MPI_RECV(
     .        BUFS2,LINDEX,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .        MPI_COMM_WORLD,STATUS,IERROR)
              DO J = 1, LINDEX
                BUFS(J) = MAX(BUFS(J),BUFS2(J))
              ENDDO
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE(BUFS2)

C
C Test reception envoi BUFR
C
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I/=LOC_PROC.AND.SIZ>0.AND.IRECV(I)>0) THEN
          CALL MPI_WAIT(REQ_S3(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      DEALLOCATE(BUFR)
#endif
      RETURN
      END

