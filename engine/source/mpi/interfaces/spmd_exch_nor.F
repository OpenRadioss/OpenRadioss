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
Chd|  SPMD_EXCH_NOR                 source/mpi/interfaces/spmd_exch_nor.F
Chd|-- called by -----------
Chd|        I25NORMP                      source/interfaces/int25/i25norm.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_NOR(
     1   NI25,IAD_FREDG,FR_EDG,NOD_NORMAL,WNOD_NORMAL,SIZE,NADMSR,
     2   REQ_R  ,REQ_S  ,IRINDEX,ISINDEX,IAD_RECV    ,
     3   NBIRECV,NBISEND,RBUF   ,SBUF   ,VTX_BISECTOR,
     4   LBOUND ,IAD_FRNOR,FR_NOR,IFLAG  ,FSKYN ,ISHIFT,
     5   ADDCSRECT, PROCNOR,SOL_EDGE)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "sphcom.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NADMSR,SOL_EDGE
      INTEGER NI25, IAD_FREDG(NINTER25,*), FR_EDG(2,*),SIZE,ISHIFT,
     .        REQ_R(NSPMD),REQ_S(NSPMD),IRINDEX(NSPMD),ISINDEX(NSPMD),IAD_RECV(NSPMD+1), 
     .        NBIRECV, NBISEND, IAD_FRNOR(NINTER25,*), FR_NOR(*), IFLAG, LBOUND(*),
     .        ADDCSRECT(*), PROCNOR(*)
      REAL*4 NOD_NORMAL(3,4,*), WNOD_NORMAL(3,4,*), VTX_BISECTOR(3,2,NADMSR),FSKYN(3,*),
     .       RBUF(*), SBUF(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,INDEX, N, M, E, IS,
     .        SIZ,J,K,L0,L,CC,II, MSGOFF,
     .        STATUS(MPI_STATUS_SIZE)
      REAL*4 RZERO
      DATA MSGOFF/6014/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      RZERO = 0.
C
      LOC_PROC = ISPMD + 1
C
      IF(IFLAG==1)THEN
C
        NBIRECV = 0
        L = 1
        IAD_RECV(1) = 1
        DO I = 1, NSPMD

          IF(I/=LOC_PROC)THEN

            L0 = L
            L  = L+  SIZE*(IAD_FREDG(NI25,I+1)-IAD_FREDG(NI25,I))
     .            +2*SIZE*(IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I))
     .            +       (IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I))


           IF(SOL_EDGE/=0)THEN
            IF(IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I)>0) THEN
              DO J=IAD_FRNOR(NI25,I),IAD_FRNOR(NI25,I+1)-1
                NOD = ISHIFT + FR_NOR(J)
                DO CC = ADDCSRECT(NOD),ADDCSRECT(NOD+1)-1
                  IF(PROCNOR(CC)==I) THEN
                    L = L + SIZE
                  ENDIF
                END DO
              END DO
            ENDIF
           ENDIF

            SIZ = L-L0
c                  print *,'recoit siz',ispmd+1,i,siz
            IF(SIZ > 0)THEN
              MSGTYP = MSGOFF
              NBIRECV = NBIRECV + 1
              IRINDEX(NBIRECV) = I
              CALL MPI_IRECV(
     S          RBUF(L0),SIZ,MPI_REAL4,IT_SPMD(I),MSGTYP,
     G          MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
            ENDIF
          ENDIF
          IAD_RECV(I+1) = L
        ENDDO
C
        NBISEND = 0
        L = 1
        DO I=1,NSPMD

          IF(I/=LOC_PROC)THEN
            L0 = L
            IF(IAD_FREDG(NI25,I+1)-IAD_FREDG(NI25,I)>0) THEN
              DO J=IAD_FREDG(NI25,I),IAD_FREDG(NI25,I+1)-1
                M = FR_EDG(1,J)
                E=  FR_EDG(2,J)
                SBUF(L)   = NOD_NORMAL(1,E,M)
                SBUF(L+1) = NOD_NORMAL(2,E,M)
                SBUF(L+2) = NOD_NORMAL(3,E,M)
                L = L + SIZE
C#ifdef D_ES
C                IF(ISPMD == 0 .AND. I-1 == 1) THEN
C                WRITE(6,"(2I10,A,I10,3Z20)") E,M," SEND TO",I-1,NOD_NORMAL(1,E,M),NOD_NORMAL(3,E,M),NOD_NORMAL(2,E,M)
C                ENDIF
C#endif 
c                  print *,'envoi',ispmd+1,i,j-IAD_FREDG(NI25,I)+1,mseglo(m),mvoisin(e,m)
              ENDDO
            ENDIF
            IF(IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I)>0) THEN
              DO J=IAD_FRNOR(NI25,I),IAD_FRNOR(NI25,I+1)-1
                IS = FR_NOR(J)
                SBUF(L)   = VTX_BISECTOR(1,1,IS)
                SBUF(L+1) = VTX_BISECTOR(2,1,IS)
                SBUF(L+2) = VTX_BISECTOR(3,1,IS)
                L = L + SIZE
                SBUF(L)   = VTX_BISECTOR(1,2,IS)
                SBUF(L+1) = VTX_BISECTOR(2,2,IS)
                SBUF(L+2) = VTX_BISECTOR(3,2,IS)
                L = L + SIZE
                SBUF(L)   = LBOUND(IS)
                L = L + 1
              ENDDO
            ENDIF

           IF(SOL_EDGE/=0)THEN

             IF(IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I)>0) THEN
              DO J=IAD_FRNOR(NI25,I),IAD_FRNOR(NI25,I+1)-1
                NOD = ISHIFT + FR_NOR(J)
                DO CC = ADDCSRECT(NOD),ADDCSRECT(NOD+1)-1
                  IF(PROCNOR(CC)==LOC_PROC) THEN
                    SBUF(L)   =  FSKYN(1,CC)
                    SBUF(L+1) = FSKYN(2,CC)
                    SBUF(L+2) = FSKYN(3,CC)
                    L = L + SIZE
                  ENDIF
                ENDDO
              ENDDO
             ENDIF
            ENDIF

            SIZ = L-L0
c                  print *,'envoi siz',ispmd+1,i,siz
            IF(SIZ > 0)THEN
              MSGTYP = MSGOFF
              NBISEND = NBISEND + 1
              ISINDEX(NBISEND)=I
              CALL MPI_ISEND(
     S        SBUF(L0),SIZ,MPI_REAL4,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
            ENDIF
          END IF
        ENDDO
C
      ELSE ! IF(IFLAG==1)THEN
C
C decompactage
C
        DO II=1,NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
          I = IRINDEX(INDEX)
          L = IAD_RECV(I)

          DO J=IAD_FREDG(NI25,I),IAD_FREDG(NI25,I+1)-1
            M= FR_EDG(1,J)
            E= FR_EDG(2,J)
            WNOD_NORMAL(1,E,M) = RBUF(L)
            WNOD_NORMAL(2,E,M) = RBUF(L+1)
            WNOD_NORMAL(3,E,M) = RBUF(L+2)
            L = L + SIZE
c                print *,'recoit',ispmd+1,i,j-IAD_FREDG(NI25,I)+1,mseglo(m),mvoisin(e,m)
          ENDDO

          DO J=IAD_FRNOR(NI25,I),IAD_FRNOR(NI25,I+1)-1
            IS= FR_NOR(J)
            IF(RBUF(L)/=RZERO.OR.RBUF(L+1)/=RZERO.OR.RBUF(L+2)/=RZERO)THEN
              IF(VTX_BISECTOR(1,1,IS)==RZERO.AND.
     .           VTX_BISECTOR(2,1,IS)==RZERO.AND.
     .           VTX_BISECTOR(3,1,IS)==RZERO)THEN
                VTX_BISECTOR(1,1,IS)=RBUF(L)
                VTX_BISECTOR(2,1,IS)=RBUF(L+1)
                VTX_BISECTOR(3,1,IS)=RBUF(L+2)
              ELSEIF(VTX_BISECTOR(1,2,IS)==RZERO.AND.
     .               VTX_BISECTOR(2,2,IS)==RZERO.AND.
     .               VTX_BISECTOR(3,2,IS)==RZERO)THEN
                VTX_BISECTOR(1,2,IS)=RBUF(L)
                VTX_BISECTOR(2,2,IS)=RBUF(L+1)
                VTX_BISECTOR(3,2,IS)=RBUF(L+2)
              ELSE
                VTX_BISECTOR(1,1,IS) = RZERO
                VTX_BISECTOR(2,1,IS) = RZERO
                VTX_BISECTOR(3,1,IS) = RZERO
                VTX_BISECTOR(1,2,IS) = RZERO 
                VTX_BISECTOR(2,2,IS) = RZERO
                VTX_BISECTOR(3,2,IS) = RZERO
              END IF    
            END IF      
            L = L + SIZE
            IF(RBUF(L)/=RZERO.OR.RBUF(L+1)/=RZERO.OR.RBUF(L+2)/=RZERO)THEN
              IF(VTX_BISECTOR(1,1,IS)==RZERO.AND.
     .           VTX_BISECTOR(2,1,IS)==RZERO.AND.
     .           VTX_BISECTOR(3,1,IS)==RZERO)THEN
                VTX_BISECTOR(1,1,IS)=RBUF(L)
                VTX_BISECTOR(2,1,IS)=RBUF(L+1)
                VTX_BISECTOR(3,1,IS)=RBUF(L+2)
              ELSEIF(VTX_BISECTOR(1,2,IS)==RZERO.AND.
     .               VTX_BISECTOR(2,2,IS)==RZERO.AND.
     .               VTX_BISECTOR(3,2,IS)==RZERO)THEN
                VTX_BISECTOR(1,2,IS)=RBUF(L)
                VTX_BISECTOR(2,2,IS)=RBUF(L+1)
                VTX_BISECTOR(3,2,IS)=RBUF(L+2)
              END IF    
            END IF      
            L = L + SIZE

            LBOUND(IS) =  LBOUND(IS)+NINT(RBUF(L))
            IF(LBOUND(IS) > 2) THEN
              VTX_BISECTOR(1,1,IS) = RZERO
              VTX_BISECTOR(2,1,IS) = RZERO
              VTX_BISECTOR(3,1,IS) = RZERO
              VTX_BISECTOR(1,2,IS) = RZERO
              VTX_BISECTOR(2,2,IS) = RZERO
              VTX_BISECTOR(3,2,IS) = RZERO
            ENDIF

            L = L + 1
          ENDDO

          IF(SOL_EDGE/=0)THEN
           DO J=IAD_FRNOR(NI25,I),IAD_FRNOR(NI25,I+1)-1
            NOD = ISHIFT + FR_NOR(J)
            DO CC = ADDCSRECT(NOD),ADDCSRECT(NOD+1)-1
              IF(PROCNOR(CC)==I) THEN
                FSKYN(1,CC) = RBUF(L)
                FSKYN(2,CC) = RBUF(L+1)
                FSKYN(3,CC) = RBUF(L+2)
                L = L + SIZE
              END IF
            END DO
           ENDDO
          ENDIF


        ENDDO
C
        DO II=1,NBISEND
          I = ISINDEX(II)
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDDO
C
      END IF
C
#endif
      RETURN
      END

