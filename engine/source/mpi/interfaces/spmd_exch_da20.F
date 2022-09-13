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
C
Chd|====================================================================
Chd|  SPMD_EXCH_DA20                source/mpi/interfaces/spmd_exch_da20.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ADDDP                         source/mpi/interfaces/spmd_i7tool.F
Chd|        GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        PUTDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_DA20(
     1   INTBUF_TAB,IPARI,IAD_ELEM,FR_ELEM,
     2   LEN20,NBINT20,LENR ,INTLIST ,NBINTC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), IPARI(NPARI,*),
     .        INTLIST(*),
     .        NBINTC, LEN20, NBINT20, LENR

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,INDEX, N, MSGOFF,
     .        SIZ,J,K,L,NB_NOD,NBIRECV, II, JJ, NI,INC,
     .        NIN, NLN, LL,IL,IG,
     .        IAD_RECV(NSPMD+1),DEBUT(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),RSTATUS(MPI_STATUS_SIZE,NSPMD),
     .        REQ_R(NSPMD),REQ_S(NSPMD),IRINDEX(NSPMD),
     .        ITAGX(NUMNOD),ISIGN,P
      DOUBLE PRECISION
     .        RBUF(NBINT20*(LEN20*LENR+NSPMD)),
     .        SBUF(NBINT20*(LEN20*LENR+NSPMD)),DAANC6L(3,6)
      DATA MSGOFF/172/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C LEN20 = 1 + 18 + 2  ! (1 : longueur ; 18 : daanc6 et 2 : alphak)
C SIZE = LEN20*NBINT20
C
      LOC_PROC = ISPMD + 1
C
      NBIRECV = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
          SIZ = NBINT20*(LEN20*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))+1)
          MSGTYP = MSGOFF 
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
        DEBUT(I) = IAD_RECV(I)
      ENDDO
C
C Pre Traitement interfaces
C
      DO NI = 1, NBINTC
        NIN = INTLIST(NI)
        IF(IPARI(7,NIN)==20)THEN
          NLN=IPARI(35,NIN)
C
          DO I = 1, NUMNOD
            ITAGX(I) = 0
          ENDDO
#include    "vectorize.inc"
          DO IL = 1, NLN
            IG = INTBUF_TAB(NIN)%NLG(IL)
            ITAGX(IG) = IL
          END DO
C
          DO I = 1, NSPMD
            IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
              LL = DEBUT(I)
              SBUF(LL)=0
              L = LL+1
#include    "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                IF(ITAGX(NOD)/=0)THEN
                  II = ITAGX(NOD)
                  SBUF(L) = J+1-IAD_ELEM(1,I)
C ALPHAK(2)
                  SBUF(L+1) =INTBUF_TAB(NIN)%ALPHAK((II-1)*3+2)
C ALPHAK(3)
                  SBUF(L+2) =INTBUF_TAB(NIN)%ALPHAK((II-1)*3+3)
                  CALL PUTDPDAANC(
     .            INTBUF_TAB(NIN)%DAANC6(1+(II-1)*3*6*2),SBUF(L+3),IRESP,INC)
C                 L  = L + 18 car variable en double precision
                  SBUF(LL)  = SBUF(LL)+1
                  L = L + LEN20
                END IF
              ENDDO
              DEBUT(I) = L
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C   echange messages
C
      DO II = 1, NBIRECV
        I = IRINDEX(II)
        L = IAD_RECV(I)
        SIZ = DEBUT(I)-L
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(I),IERROR)
      ENDDO
C
C decompactage
C
        CALL MPI_WAITALL(NBIRECV,REQ_R,RSTATUS,IERROR)
        DO NI = 1, NBINTC
          NIN = INTLIST(NI)
          IF(IPARI(7,NIN)==20)THEN
            NLN=IPARI(35,NIN)
C
c            DO II = 1, NUMNOD
              ITAGX(1:NUMNOD) = 0
c            ENDDO
#include    "vectorize.inc"
            DO IL = 1, NLN
              IG = INTBUF_TAB(NIN)%NLG(IL)
              ITAGX(IG) = IL
            END DO
C
            DO I=1,NBIRECV

            P = IRINDEX(I)
            L = IAD_RECV(P)

            NB_NOD = NINT(RBUF(L))
            L = L + 1
#include    "vectorize.inc"
            DO N = 1, NB_NOD
              J = NINT(RBUF(L))
              NOD = FR_ELEM(IAD_ELEM(1,P)+J-1)
              II = ITAGX(NOD)
              if (II>0)THEN
C ALPHAK(2)
              ISIGN = 1
              IF(INTBUF_TAB(NIN)%ALPHAK((II-1)*3+2) < ZERO .OR. RBUF(L+1) <ZERO)
     *             ISIGN = -1
              INTBUF_TAB(NIN)%ALPHAK((II-1)*3+2)=ISIGN*
     .                 MIN(ABS(INTBUF_TAB(NIN)%ALPHAK((II-1)*3+2)),
     .                                       ABS(RBUF(L+1)))
C ALPHAK(3)
              INTBUF_TAB(NIN)%ALPHAK((II-1)*3+3)=MIN(INTBUF_TAB(NIN)%ALPHAK((II-1)*3+3),
     .                                       RBUF(L+2))
              CALL GETDPDAANC(DAANC6L,RBUF(L+3),IRESP,INC)
C             L  = L + 18 car variable en double precision
              CALL ADDDP(INTBUF_TAB(NIN)%DAANC6(1+(II-1)*3*6*2),DAANC6L,18)
              ENDIF
              L = L + LEN20
            END DO
C Repositionne IAD_RECV pour l'interface suivante
            IAD_RECV(P)=L
        END DO
          END IF
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END
