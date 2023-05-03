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
Chd|  SPMD_EXCH_IDEL_SEGLO          source/mpi/interfaces/spmd_exch_idel_seglo.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        I24_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        I25_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
        SUBROUTINE SPMD_EXCH_IDEL_SEGLO(IPARI,INTBUF_TAB,INDSEGLO,IBUFSEGLO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),INDSEGLO(*),IBUFSEGLO(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF1 ,MSGOFF2, MSGTYP, LOC_PROC,IERROR,REQ_S1(NSPMD),REQ_S2(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      INTEGER RSEGLO(NINTER+1)
C
      INTEGER NRTM,NG,IND_SEG
      INTEGER I,SIZ,IAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF
C-----------------------------------------------
         DATA MSGOFF1/125/
         DATA MSGOFF2/126/


         LOC_PROC = ISPMD+1
         DO I = 1, NSPMD
           IF(I/=LOC_PROC) THEN
             MSGTYP = MSGOFF1
             CALL MPI_ISEND(
     C         INDSEGLO,NINTER+1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G         MPI_COMM_WORLD,REQ_S1(I),IERROR) 

             SIZ = INDSEGLO(NINTER+1)-INDSEGLO(1)
             IF(SIZ>0)THEN
               MSGTYP = MSGOFF2
               CALL MPI_ISEND(
     C           IBUFSEGLO,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G           MPI_COMM_WORLD,REQ_S2(I),IERROR) 
             ENDIF
           ENDIF
         ENDDO
      
         DO I = 1, NSPMD
           IF(I/=LOC_PROC) THEN
             MSGTYP = MSGOFF1
             CALL MPI_RECV(
     .         RSEGLO,NINTER+1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .         MPI_COMM_WORLD,STATUS,IERROR)
             
               SIZ = RSEGLO(NINTER+1)-RSEGLO(1)
               IF(SIZ>0)THEN
                 ALLOCATE(RBUF(SIZ))
                 MSGTYP = MSGOFF2
                 CALL MPI_RECV(
     .             RBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .             MPI_COMM_WORLD,STATUS,IERROR)
                 
                   DO NG=1,NINTER
                      IND_SEG=RSEGLO(NG+1)-RSEGLO(NG)
                      IAD=RSEGLO(NG)
                      IF(IND_SEG >0)THEN
                        IF(IPARI(7,NG)==24)THEN
                          NRTM=IPARI(4,NG)
                          CALL I24_REMOVE_GLOBAL_SEGMENT(RBUF(IAD),IND_SEG,NG,NRTM,
     .                                  INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,2)
                        ELSEIF(IPARI(7,NG)==25)THEN
                          NRTM=IPARI(4,NG)
                          CALL I25_REMOVE_GLOBAL_SEGMENT(RBUF(IAD),IND_SEG,NG,NRTM,
     .                                 INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,2)
                        END IF
                      ENDIF   ! IF(IND_SEG >0)
                   ENDDO     ! DO NG=1,NINTER
                 DEALLOCATE(RBUF)
               ENDIF       ! IF(SIZ>0)THEN
           ENDIF           ! IF(I/=LOC_PROC) THEN
         ENDDO
C
C Test reception envoi BUFR
C
      DO I = 1, NSPMD
        IF(I/=LOC_PROC) THEN
          CALL MPI_WAIT(REQ_S1(I),STATUS,IERROR)
          SIZ = INDSEGLO(NINTER+1)-INDSEGLO(1)
          IF(SIZ>0)THEN
              CALL MPI_WAIT(REQ_S2(I),STATUS,IERROR)
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
#endif
      END
