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
#define TO1D(i,j,k,s1,s2) i+(j-1)*s1+(k-1)*s1*s2
Chd|====================================================================
Chd|  SPMD_I25FRONT_INIT            source/mpi/interfaces/spmd_i25front.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I25FREE_BOUND                 source/interfaces/int25/i25free_bound.F
Chd|        SPMD_COMM_SPLIT               source/mpi/generic/spmd_comm_split.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I25FRONT_INIT(ITAB,MAIN_PROC,INTBUF_TAB,IPARI)
C============================================================================
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
C #include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "impl1_c.inc"
#include      "assert.inc"
#include      "macro.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),MAIN_PROC(NUMNOD)
      INTEGER, INTENT(IN) :: IPARI(NPARI,NINTER)
C     INTEGER, INTENT(IN) :: IRECVFROM(NINTER+1,*),ISENDTO(NINTER+1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(int_pointer), DIMENSION(NINTER,NSPMD) :: SEND_BUF,RECV_BUF
      INTEGER, DIMENSION(NINTER,NSPMD) :: SIZ_SEND_BUF,SIZ_RECV_BUF
      INTEGER :: I,J,K,L,II,NIN,ITY,P,UID
      INTEGER :: IERROR
      INTEGER :: COLOR,KEY,NEDGE
      INTEGER :: IED,IE,JE,WGT,NRTM,NSN
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB
#ifdef MPI
      INTEGER :: STATUS(MPI_STATUS_SIZE), MSGTYP, REQ_S(NSPMD),REQ_R(NSPMD)
#else
      INTEGER,PARAMETER :: MPI_COMM_NULL = 0
#endif

      NINTER25E = 0
      DO NIN = 1,NINTER
        INTBUF_TAB(NIN)%MPI_COMM = MPI_COMM_NULL
        INTBUF_TAB(NIN)%NB_INTERNAL_EDGES = 0
        INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_LOCAL = 0
        INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_REMOTE = 0
        ITY = IPARI(7,NIN)
        IF( ITY == 25 ) THEN
C Compute free bound
            NRTM = IPARI(4,NIN)
            ALLOCATE(INTBUF_TAB(NIN)%FREE_IRECT_ID(NRTM))
            CALL I25FREE_BOUND( 
     .           NRTM,
     .           INTBUF_TAB(NIN)%MVOISIN,
     .           INTBUF_TAB(NIN)%IRECTM,
     .           INTBUF_TAB(NIN)%STFM,
     .           INTBUF_TAB(NIN)%NRTM_FREE,
     .           INTBUF_TAB(NIN)%FREE_IRECT_ID)

C Split communicator

            KEY = ISPMD
            COLOR = 0 
C           IF(IRECVFROM(NIN,ISPMD+1) > 0 .OR. ISENDTO(NIN,ISPMD+1) > 0) THEN
            NSN = IPARI(MACRO_NSN,NIN)
            NEDGE = IPARI(MACRO_NEDGE,NIN)
            IF(IPARI(MACRO_IEDGE,NIN) > 0) NINTER25E = NINTER25E + 1
            IF(NEDGE > 0) THEN 
              COLOR = 1 

              ALLOCATE(TAB(NEDGE,3))

              ! Gather iformations
              DO IE = 1,NEDGE
                TAB(IE,1) =  INTBUF_TAB(NIN)%LEDGE( (IE-1)*NLEDGE + 1 ) !IE
                TAB(IE,2) =  INTBUF_TAB(NIN)%LEDGE( (IE-1)*NLEDGE + 3 ) !JE
                TAB(IE,3) =  INTBUF_TAB(NIN)%LEDGE( (IE-1)*NLEDGE + 9 ) !weight
              ENDDO

              IED = COUNT(TAB(1:NEDGE,1) >= 0 .AND. TAB(1:NEDGE,2) >= 0)              
              INTBUF_TAB(NIN)%NB_INTERNAL_EDGES = IED

C             WRITE(6,*) "Internal edges:", INTBUF_TAB(NIN)%NB_INTERNAL_EDGES

              IED = COUNT(.NOT. (TAB(1:NEDGE,1) >= 0 .AND. TAB(1:NEDGE,2) >= 0)
     .           .AND. TAB(1:NEDGE,3) == 1)    
              INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_LOCAL = IED 

C             WRITE(6,*) "local boundary edges:", INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_LOCAL

              INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_REMOTE = NEDGE 
     .               - INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_LOCAL 
     .               - INTBUF_TAB(NIN)%NB_INTERNAL_EDGES
C             WRITE(6,*) "remote boundary edges:", INTBUF_TAB(NIN)%NB_BOUNDARY_EDGES_REMOTE


              DEALLOCATE(TAB)
            ENDIF
#ifdef MPI
            CALL SPMD_COMM_SPLIT(COLOR,KEY,INTBUF_TAB(NIN)%MPI_COMM,
     .                          INTBUF_TAB(NIN)%RANK, INTBUF_TAB(NIN)%NSPMD)
#endif
        ENDIF
      ENDDO



#ifdef MPI
      SIZ_RECV_BUF(1:NINTER,1:NSPMD) = 0
      SIZ_SEND_BUF(1:NINTER,1:NSPMD) = 0
C     sizes of send buffers
      DO NIN = 1,NINTER
        ITY = IPARI(MACRO_ITY,NIN)
        NSN = IPARI(MACRO_NSN,NIN)
        IF( ITY==25) THEN
          DO I = 1,NSN
            P = MAIN_PROC(INTBUF_TAB(NIN)%NSV(I))
            SIZ_SEND_BUF(NIN,P) = SIZ_SEND_BUF(NIN,P) + 1
          ENDDO
        ENDIF
      ENDDO

      CALL MPI_ALLTOALL(SIZ_SEND_BUF, NINTER, MPI_INTEGER,
     .                  SIZ_RECV_BUF, NINTER, MPI_INTEGER,
     .                  MPI_COMM_WORLD,IERROR)

C     allocation of buffers
      DO NIN = 1,NINTER
        ITY=IPARI(MACRO_ITY,NIN)
        NSN = IPARI(MACRO_NSN,NIN)
        IF( ITY==25 ) THEN
          DO P=1,NSPMD
c           WRITE(6,*) ISPMD+1,P, SIZ_SEND_BUF(NIN,P),SIZ_RECV_BUF(NIN,P)
            ALLOCATE(SEND_BUF(NIN,P)%P(SIZ_SEND_BUF(NIN,P))) 
            ALLOCATE(RECV_BUF(NIN,P)%P(SIZ_RECV_BUF(NIN,P))) 
          ENDDO
        ENDIF
      ENDDO

C     sizes of send buffers
      SIZ_SEND_BUF(1:NINTER,1:NSPMD) = 0
      DO NIN = 1,NINTER
        ITY=IPARI(MACRO_ITY,NIN)
        NSN = IPARI(MACRO_NSN,NIN)
        IF( ITY==25) THEN
          DO I = 1,NSN
            P = MAIN_PROC(INTBUF_TAB(NIN)%NSV(I))
            SIZ_SEND_BUF(NIN,P) = SIZ_SEND_BUF(NIN,P) + 1
            II = SIZ_SEND_BUF(NIN,P) 
            IF(ISPMD+1 == P ) THEN
              SEND_BUF(NIN,P)%P(II) = I 
            ELSE
              SEND_BUF(NIN,P)%P(II) = ITAB(INTBUF_TAB(NIN)%NSV(I)) 
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      MSGTYP = 1000
      DO NIN = 1, NINTER
        DO P = 1, NSPMD
          IF(P .NE. ISPMD + 1 .AND. SIZ_SEND_BUF(NIN,P) > 0) THEN
             K = SIZ_SEND_BUF(NIN,P)
c            WRITE(6,*) "SEND ->",P,SEND_BUF(NIN,P)%P(1:k)
             CALL MPI_ISEND(
     .       SEND_BUF(NIN,P)%P(1),K,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .       MPI_COMM_WORLD,REQ_S(P),IERROR)
          ENDIF
          IF(P .NE. ISPMD + 1 .AND. SIZ_RECV_BUF(NIN,P) > 0) THEN
             K = SIZ_RECV_BUF(NIN,P)
             CALL MPI_IRECV(
     .       RECV_BUF(NIN,P)%P(1),K,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .       MPI_COMM_WORLD,REQ_R(P),IERROR)
          ENDIF
        ENDDO
c       CALL FLUSH(6)
c       CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      
        DO P = 1,NSPMD
          IF(P .NE. ISPMD + 1 .AND. SIZ_SEND_BUF(NIN,P) > 0) THEN
             CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
          ENDIF
          IF(P .NE. ISPMD + 1 .AND. SIZ_RECV_BUF(NIN,P) > 0) THEN
             CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
          ENDIF
        ENDDO
C ----------------------------------------------------------------   
C                     RESOLVE UID to LOCAL NODE
        DO P = 1, NSPMD
c         WRITE(6,*) __FILE__,__LINE__, P,SIZ_RECV_BUF(NIN,P) 
          IF(P .NE. ISPMD +1) THEN
            DO L = 1, SIZ_RECV_BUF(NIN,P)
              K = 1
              UID = RECV_BUF(NIN,P)%P(L)
c             WRITE(6,*) "looking for UID",UID
              NSN = IPARI(MACRO_NSN,NIN)
              DO WHILE (K<NSN .AND. UID .NE. ITAB(INTBUF_TAB(NIN)%NSV(K)))
                K = K + 1 
              ENDDO  
              IF(ITAB(INTBUF_TAB(NIN)%NSV(K))==UID) THEN
c                WRITE(6,*) "found at ",K
                 RECV_BUF(NIN,P)%P(L) = K 
              ELSE
                ASSERT(.FALSE.)
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        DO P = 1, NSPMD
          IF(P .NE. ISPMD + 1 .AND. SIZ_RECV_BUF(NIN,P) > 0) THEN
             K = SIZ_RECV_BUF(NIN,P)
             CALL MPI_ISEND(
     .       RECV_BUF(NIN,P)%P(1),K,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .       MPI_COMM_WORLD,REQ_S(P),IERROR)
          ENDIF
          IF(P .NE. ISPMD + 1 .AND. SIZ_SEND_BUF(NIN,P) > 0) THEN
             K = SIZ_SEND_BUF(NIN,P)
             CALL MPI_IRECV(
     .       SEND_BUF(NIN,P)%P(1),K,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .       MPI_COMM_WORLD,REQ_R(P),IERROR)
          ENDIF
        ENDDO
        DO P = 1,NSPMD
          IF(P .NE. ISPMD + 1 .AND. SIZ_RECV_BUF(NIN,P) > 0) THEN
             CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
          ENDIF
          IF(P .NE. ISPMD + 1 .AND. SIZ_SEND_BUF(NIN,P) > 0) THEN
             CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
          ENDIF
        ENDDO
      ENDDO ! NIN


      SIZ_SEND_BUF(1:NINTER,1:NSPMD) = 0
      DO NIN = 1,NINTER
        ITY=IPARI(MACRO_ITY,NIN)
        NSN = IPARI(MACRO_NSN,NIN)
        IF( ITY==25) THEN
          ALLOCATE(INTBUF_TAB(NIN)%NSV_ON_PMAIN(NSN))
          DO I = 1, NSN
            P = MAIN_PROC(INTBUF_TAB(NIN)%NSV(I))
            SIZ_SEND_BUF(NIN,P) = SIZ_SEND_BUF(NIN,P) + 1
            II = SIZ_SEND_BUF(NIN,P) 
            INTBUF_TAB(NIN)%NSV_ON_PMAIN(I) = SEND_BUF(NIN,P)%P(II) 
           ENDDO
        ENDIF
      ENDDO
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_I25FRONT_NOR             source/mpi/interfaces/spmd_i25front.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I25FRONT_NOR(IPARI,
     . INTBUF_TAB,
     . INTLIST25,
     . X) ! list of type 25 interfaces

C Update normal (EDG_BISSECTOR and VTX_BISSECTOR)
C on secnd node that are candidates on other domain
C out: updated value of EDGE_BISECTOR_FIE and VTX_BISECTOR_FIE
C and LEDGE (important if rupture) is modified here
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
      USE MESSAGE_MOD
      USE MPI_COMMOD
      USE INTBUFDEF_MOD
#ifdef WITH_ASSERT
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: ieee_arithmetic 
#endif
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
#include      "macro.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "i25edge_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IPARI(NPARI,NINTER), INTLIST25(*)
      TYPE(INTBUF_STRUCT_)  :: INTBUF_TAB(*)
      my_real, INTENT(IN) :: X(3,NUMNOD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER :: NI25 
      INTEGER :: N
      INTEGER :: NB
      INTEGER :: IEDGE
      INTEGER :: P
      INTEGER :: SEND_SIZE
      INTEGER :: RECV_SIZE
      INTEGER :: K,I,I1,I2,IE,JE,IED,L,L0
      TYPE(MPI_COMM_NOR_STRUCT) , DIMENSION(NINTER25) :: BUFFERS
      INTEGER :: IERROR
      INTEGER :: MSGTYP,MSGOFF
      INTEGER :: EID
      INTEGER :: IM,M1,M2
      INTEGER :: NEDGE_LOCAL
      INTEGER :: IGLOB,IBEGIN
      INTEGER :: N1,N2,N3,N4,NN1,NN2,PP
      INTEGER :: NRTM
      REAL *4 :: SP
#ifdef WITH_ASSERT
      real (real32) :: nan32
#endif
      DATA MSGOFF/163/
      INTEGER NTRIA(3,4)
      DATA NTRIA/1,2,4,2,4,1,0,0,0,4,1,2/
C-----------------------------------------------

#ifdef MYREAL8
      INTEGER, PARAMETER :: NB_VALUES = 3 + 12 + 4 + 12 * 2  + 3 * 2!3 vtx bissector + 6 x 2 VTX_BISECTOR
                              ! + LEDGE right / LEDGE left
#else
      INTEGER, PARAMETER :: NB_VALUES = 3 + 12 + 4 + 12  + 3 * 2!3 vtx bissector + 6 x 2 VTX_BISECTOR
                              ! + LEDGE right / LEDGE left
#endif


                                   
C     WRITE(6,*) __FILE__,__LINE__
C===== ALlocations && IRECV 
      DO NI25=1,NINTER25
        N = INTLIST25(NI25)
        IEDGE = IPARI(58,N)
        IF( IEDGE > 0 ) THEN
          BUFFERS(NI25)%NBIRECV = 0 

          ALLOCATE(BUFFERS(NI25)%SEND_RQ(NSPMD))
          ALLOCATE(BUFFERS(NI25)%RECV_RQ(NSPMD))
          ALLOCATE(BUFFERS(NI25)%IAD_RECV(NSPMD+1))
          ALLOCATE(BUFFERS(NI25)%IAD_SEND(NSPMD+1))
          
          SEND_SIZE = 0 
          RECV_SIZE = 0
          BUFFERS(NI25)%IAD_SEND(1) = 1
          BUFFERS(NI25)%IAD_RECV(1) = 1
          DO P = 1, NSPMD 
            NB = NSNSIE(N)%P(P) 
            SEND_SIZE = SEND_SIZE + NB*NB_VALUES
            BUFFERS(NI25)%IAD_SEND(P+1) = BUFFERS(NI25)%IAD_SEND(P) + NB
          
            NB = NSNFIE(N)%P(P) 
            RECV_SIZE = RECV_SIZE + NB*NB_VALUES
            BUFFERS(NI25)%IAD_RECV(P+1) = BUFFERS(NI25)%IAD_RECV(P) + NB
          ENDDO
          ALLOCATE(BUFFERS(NI25)%SEND_BUF(SEND_SIZE))
          ALLOCATE(BUFFERS(NI25)%RECV_BUF(RECV_SIZE))
          DO P = 1, NSPMD 
            RECV_SIZE = NB_VALUES * ( BUFFERS(NI25)%IAD_RECV(P+1)-BUFFERS(NI25)%IAD_RECV(P))
            BUFFERS(NI25)%RECV_RQ(P) = MPI_REQUEST_NULL
            IF(RECV_SIZE > 0) THEN
              BUFFERS(NI25)%NBIRECV = BUFFERS(NI25)%NBIRECV + 1
              MSGTYP = MSGOFF
              I = BUFFERS(NI25)%IAD_RECV(P)
              L = (I-1) * NB_VALUES + 1
              ASSERT(L > 0)

C           WRITE(6,*) "RECV FROM",IT_SPMD(P),RECV_SIZE
C           CALL FLUSH(6)

              CALL MPI_IRECV(
     .          BUFFERS(NI25)%RECV_BUF(L),
     .          RECV_SIZE,
     .          MPI_REAL4,
     .          IT_SPMD(P),
     .          MSGTYP,
     .          MPI_COMM_WORLD,
     .          BUFFERS(NI25)%RECV_RQ(P),
     .          IERROR)
            ENDIF
          ENDDO
        ENDIF
      ENDDO


C === Fill buffer and ISEND                            
      DO NI25=1,NINTER25
        N = INTLIST25(NI25)
        IEDGE = IPARI(58,N)
        IF( IEDGE > 0 ) THEN
          NEDGE_LOCAL = INTBUF_TAB(N)%NB_INTERNAL_EDGES + INTBUF_TAB(N)%NB_BOUNDARY_EDGES_LOCAL

          BUFFERS(NI25)%NBISEND = 0 
          DO P = 1, NSPMD 
            BUFFERS(NI25)%SEND_RQ(P) = MPI_REQUEST_NULL
            SEND_SIZE = ( BUFFERS(NI25)%IAD_SEND(P+1)-BUFFERS(NI25)%IAD_SEND(P)) * NB_VALUES
            DO I = BUFFERS(NI25)%IAD_SEND(P), BUFFERS(NI25)%IAD_SEND(P+1)-1
              IED = NSVSIE(N)%P(I)
C             ASSERT(IED <= NEDGE)
              IE = INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+1)
              JE = INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+2)

              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,P-1)
              M1 = INTBUF_TAB(N)%LEDGE(5+(IED-1)*NLEDGE)
              M2 = INTBUF_TAB(N)%LEDGE(6+(IED-1)*NLEDGE)
              IM = INTBUF_TAB(N)%LEDGE(10+(IED-1)*NLEDGE)
C             ASSERT( (IM == 1 .OR. IM == -1) )

              NN1 = INTBUF_TAB(N)%ADMSR(JE+(IE-1)*4)
              NN2 = INTBUF_TAB(N)%ADMSR(MOD(JE,4)+1+(IE-1)*4)

C C====================================================
C               IF( IM .NE. 1 .AND. IM .NE. -1) THEN
C                 WRITE(6,*) "IM=",IM 
C                 CALL FLUSH(6)
C                 ASSERT(.FALSE.)
C               ENDIF
C               ASSERT(IE > 0)
C               ASSERT(JE > 0)
C               IF(INTBUF_TAB(N)%IRECTM(JE+(IE-1)*4)==M1     .AND.
C      .           INTBUF_TAB(N)%IRECTM(MOD(JE,4)+1+(IE-1)*4) == M2)THEN
C                 IM= 1
C               ELSEIF(INTBUF_TAB(N)%IRECTM(JE+(IE-1)*4)==M2  .AND.
C      .               INTBUF_TAB(N)%IRECTM(MOD(JE,4)+1+(IE-1)*4)==M1)THEN
C                 IM=-1
C               ELSE
C                 ASSERT(.FALSE.)
C               ENDIF
C   
C               IF(IM==1)THEN
C                 ASSERT(JE+(IE-1)*4 > 0)
C                 ASSERT(MOD(JE,4)+1+(IE-1)*4 > 0)
C                 ASSERT(JE+(IE-1)*4 <= INTBUF_TAB(N)%S_ADMSR)
C                 ASSERT(MOD(JE,4)+1+(IE-1)*4 <= INTBUF_TAB(N)%S_ADMSR)
C                 I1=INTBUF_TAB(N)%ADMSR(JE+(IE-1)*4)
C                 I2=INTBUF_TAB(N)%ADMSR(MOD(JE,4)+1+(IE-1)*4)
C               ELSE IF(IM==-1) THEN
C                 ASSERT(JE+(IE-1)*4 > 0)
C                 ASSERT(MOD(JE,4)+1+(IE-1)*4 > 0)
C                 ASSERT(JE+(IE-1)*4 <= INTBUF_TAB(N)%S_ADMSR)
C                 ASSERT(MOD(JE,4)+1+(IE-1)*4 <= INTBUF_TAB(N)%S_ADMSR)
C                 I2=INTBUF_TAB(N)%ADMSR(JE+(IE-1)*4)
C                 I1=INTBUF_TAB(N)%ADMSR(MOD(JE,4)+1+(IE-1)*4)
C               ELSE 
C                 WRITE(6,*) "IM=",IM 
C                 CALL FLUSH(6)
C                 ASSERT(.FALSE.)
C C               STOP
C               END IF
C C====================================================

C             ASSERT(I1 == INTBUF_TAB(N)%LEDGE(11+(IED-1)*NLEDGE))
C             ASSERT(I2 == INTBUF_TAB(N)%LEDGE(12+(IED-1)*NLEDGE))

              NRTM = IPARI(4,N)

              PRINTIF(4 +(IE-1)*4 > 4 * NRTM,NRTM)
              PRINTIF(4 +(IE-1)*4 > 4 * NRTM,IE)

              IF(IE > 0) THEN ! Used only for solids edges
                IF(INTBUF_TAB(N)%IRECTM(3 +(IE-1)*4)
     .        /=   INTBUF_TAB(N)%IRECTM(4 +(IE-1)*4)  ) THEN
                  N1 = INTBUF_TAB(N)%IRECTM(    JE       +(IE-1)*4)
                  N2 = INTBUF_TAB(N)%IRECTM(MOD(JE,4)  +1+(IE-1)*4)
                  N3 = INTBUF_TAB(N)%IRECTM(MOD(JE+1,4)+1+(IE-1)*4)
                  N4 = INTBUF_TAB(N)%IRECTM(MOD(JE+2,4)+1+(IE-1)*4)
                ELSE
                  N1 = INTBUF_TAB(N)%IRECTM(NTRIA(1,JE)+(IE-1)*4)
                  N2 = INTBUF_TAB(N)%IRECTM(NTRIA(2,JE)+(IE-1)*4)
                  N3 = INTBUF_TAB(N)%IRECTM(NTRIA(3,JE)+(IE-1)*4)
                  N4 = N3
                END IF
              ENDIF

              I1 =  INTBUF_TAB(N)%LEDGE(11+(IED-1)*NLEDGE)
              I2 =  INTBUF_TAB(N)%LEDGE(12+(IED-1)*NLEDGE)
      
              L = (I-1) * NB_VALUES

              IF(IE > 0) THEN
                BUFFERS(NI25)%SEND_BUF(L+1)  = INTBUF_TAB(N)%EDGE_BISECTOR(TO1D(1,JE,IE,3,4))
                BUFFERS(NI25)%SEND_BUF(L+2)  = INTBUF_TAB(N)%EDGE_BISECTOR(TO1D(2,JE,IE,3,4))
                BUFFERS(NI25)%SEND_BUF(L+3)  = INTBUF_TAB(N)%EDGE_BISECTOR(TO1D(3,JE,IE,3,4))

              ELSEIF (IE < 0) THEN
C Local IRECT of the edge is deleted
C But ISPMD is still in charge to send that secondary edge.
                BUFFERS(NI25)%SEND_BUF(L+1)  = INTBUF_TAB(N)%EDGE_BISECTOR(TO1D(1,JE,-IE,3,4))
                BUFFERS(NI25)%SEND_BUF(L+2)  = INTBUF_TAB(N)%EDGE_BISECTOR(TO1D(2,JE,-IE,3,4))
                BUFFERS(NI25)%SEND_BUF(L+3)  = INTBUF_TAB(N)%EDGE_BISECTOR(TO1D(3,JE,-IE,3,4))
              ELSE
                ASSERT(.FALSE.)
              ENDIF
C             WRITE(6,*) "Out:",INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8),L

              BUFFERS(NI25)%SEND_BUF(L+4)  = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(1,1,I1,3,2))
              BUFFERS(NI25)%SEND_BUF(L+5)  = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(2,1,I1,3,2))
              BUFFERS(NI25)%SEND_BUF(L+6)  = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(3,1,I1,3,2))
              BUFFERS(NI25)%SEND_BUF(L+7)  = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(1,2,I1,3,2))
              BUFFERS(NI25)%SEND_BUF(L+8)  = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(2,2,I1,3,2))
              BUFFERS(NI25)%SEND_BUF(L+9)  = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(3,2,I1,3,2))
              BUFFERS(NI25)%SEND_BUF(L+10) = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(1,1,I2,3,2))
              BUFFERS(NI25)%SEND_BUF(L+11) = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(2,1,I2,3,2))
              BUFFERS(NI25)%SEND_BUF(L+12) = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(3,1,I2,3,2))
              BUFFERS(NI25)%SEND_BUF(L+13) = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(1,2,I2,3,2))
              BUFFERS(NI25)%SEND_BUF(L+14) = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(2,2,I2,3,2))
              BUFFERS(NI25)%SEND_BUF(L+15) = INTBUF_TAB(N)%VTX_BISECTOR(TO1D(3,2,I2,3,2))



              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+5) ))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+6) ))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+7) ))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+8) ))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+9) ))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+10)))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+11)))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+12)))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+13)))
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+14)))              
              ASSERT( .NOT. isnan( BUFFERS(NI25)%SEND_BUF(L+15)))

              BUFFERS(NI25)%SEND_BUF(L+16) =
     .        TRANSFER(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+1),BUFFERS(NI25)%SEND_BUF(L+16))
              BUFFERS(NI25)%SEND_BUF(L+17) = 
     .        TRANSFER(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+2),BUFFERS(NI25)%SEND_BUF(L+17))
              BUFFERS(NI25)%SEND_BUF(L+18) =
     .        TRANSFER(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+3),BUFFERS(NI25)%SEND_BUF(L+18))
              BUFFERS(NI25)%SEND_BUF(L+19) = 
     .        TRANSFER(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+4),BUFFERS(NI25)%SEND_BUF(L+19))

              EID = INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+LEDGE_GLOBAL_ID)
              DEBUG_E2E(EID==D_ES, INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+3))


              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(1,N1))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(2,N1))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(3,N1))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(1,N2))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(2,N2))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(3,N2))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(1,N3))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(2,N3))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(3,N3))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(1,N4))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(2,N4))
              DEBUG_E2E(INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8) == D_ES,X(3,N4))

              IF(IE > 0) THEN
#ifdef MYREAL8
                BUFFERS(NI25)%SEND_BUF(L+20:L+20+1)=TRANSFER(X(1,N1),SP,2) 
                BUFFERS(NI25)%SEND_BUF(L+22:L+22+1)=TRANSFER(X(2,N1),SP,2) 
                BUFFERS(NI25)%SEND_BUF(L+24:L+24+1)=TRANSFER(X(3,N1),SP,2) 
                BUFFERS(NI25)%SEND_BUF(L+26:L+26+1)=TRANSFER(X(1,N2),SP,2) 
                BUFFERS(NI25)%SEND_BUF(L+28:L+28+1)=TRANSFER(X(2,N2),SP,2) 
                BUFFERS(NI25)%SEND_BUF(L+30:L+30+1)=TRANSFER(X(3,N2),SP,2) 
                BUFFERS(NI25)%SEND_BUF(L+32:L+32+1)=TRANSFER(X(1,N3),SP,2)
                BUFFERS(NI25)%SEND_BUF(L+34:L+34+1)=TRANSFER(X(2,N3),SP,2)
                BUFFERS(NI25)%SEND_BUF(L+36:L+36+1)=TRANSFER(X(3,N3),SP,2)
                BUFFERS(NI25)%SEND_BUF(L+38:L+38+1)=TRANSFER(X(1,N4),SP,2)
                BUFFERS(NI25)%SEND_BUF(L+40:L+40+1)=TRANSFER(X(2,N4),SP,2)
                BUFFERS(NI25)%SEND_BUF(L+42:L+42+1)=TRANSFER(X(3,N4),SP,2)
                PP = 43
#else
                BUFFERS(NI25)%SEND_BUF(L+20) = X(1,N1)  
                BUFFERS(NI25)%SEND_BUF(L+21) = X(2,N1)  
                BUFFERS(NI25)%SEND_BUF(L+22) = X(3,N1)  
                BUFFERS(NI25)%SEND_BUF(L+23) = X(1,N2)  
                BUFFERS(NI25)%SEND_BUF(L+24) = X(2,N2)  
                BUFFERS(NI25)%SEND_BUF(L+25) = X(3,N2)  
                BUFFERS(NI25)%SEND_BUF(L+26) = X(1,N3) 
                BUFFERS(NI25)%SEND_BUF(L+27) = X(2,N3) 
                BUFFERS(NI25)%SEND_BUF(L+28) = X(3,N3) 
                BUFFERS(NI25)%SEND_BUF(L+29) = X(1,N4) 
                BUFFERS(NI25)%SEND_BUF(L+30) = X(2,N4) 
                BUFFERS(NI25)%SEND_BUF(L+31) = X(3,N4) 
                PP = 31
#endif
              ELSE
#ifdef MYREAL8
                BUFFERS(NI25)%SEND_BUF(L+20) = 0  
                BUFFERS(NI25)%SEND_BUF(L+21) = 0  
                BUFFERS(NI25)%SEND_BUF(L+22) = 0  
                BUFFERS(NI25)%SEND_BUF(L+23) = 0  
                BUFFERS(NI25)%SEND_BUF(L+24) = 0  
                BUFFERS(NI25)%SEND_BUF(L+25) = 0  
                BUFFERS(NI25)%SEND_BUF(L+26) = 0 
                BUFFERS(NI25)%SEND_BUF(L+27) = 0 
                BUFFERS(NI25)%SEND_BUF(L+28) = 0 
                BUFFERS(NI25)%SEND_BUF(L+29) = 0 
                BUFFERS(NI25)%SEND_BUF(L+30) = 0 
                BUFFERS(NI25)%SEND_BUF(L+31) = 0 
                BUFFERS(NI25)%SEND_BUF(L+32) = 0 
                BUFFERS(NI25)%SEND_BUF(L+33) = 0 
                BUFFERS(NI25)%SEND_BUF(L+34) = 0 
                BUFFERS(NI25)%SEND_BUF(L+35) = 0
                BUFFERS(NI25)%SEND_BUF(L+36) = 0 
                BUFFERS(NI25)%SEND_BUF(L+37) = 0 
                BUFFERS(NI25)%SEND_BUF(L+38) = 0  
                BUFFERS(NI25)%SEND_BUF(L+39) = 0 
                BUFFERS(NI25)%SEND_BUF(L+40) = 0 
                BUFFERS(NI25)%SEND_BUF(L+41) = 0 
                BUFFERS(NI25)%SEND_BUF(L+42) = 0 
                BUFFERS(NI25)%SEND_BUF(L+43) = 0 

                PP = 43
#else
                BUFFERS(NI25)%SEND_BUF(L+20) = 0  
                BUFFERS(NI25)%SEND_BUF(L+21) = 0  
                BUFFERS(NI25)%SEND_BUF(L+22) = 0  
                BUFFERS(NI25)%SEND_BUF(L+23) = 0  
                BUFFERS(NI25)%SEND_BUF(L+24) = 0  
                BUFFERS(NI25)%SEND_BUF(L+25) = 0  
                BUFFERS(NI25)%SEND_BUF(L+26) = 0 
                BUFFERS(NI25)%SEND_BUF(L+27) = 0 
                BUFFERS(NI25)%SEND_BUF(L+28) = 0 
                BUFFERS(NI25)%SEND_BUF(L+29) = 0 
                BUFFERS(NI25)%SEND_BUF(L+30) = 0 
                BUFFERS(NI25)%SEND_BUF(L+31) = 0 
                PP = 31
#endif
              ENDIF

C Send Normal of nodes of second edge : 

                BUFFERS(NI25)%SEND_BUF(L+PP+1)  = INTBUF_TAB(N)%E2S_NOD_NORMAL(3*(NN1-1)+1)
                BUFFERS(NI25)%SEND_BUF(L+PP+2)  = INTBUF_TAB(N)%E2S_NOD_NORMAL(3*(NN1-1)+2)
                BUFFERS(NI25)%SEND_BUF(L+PP+3)  = INTBUF_TAB(N)%E2S_NOD_NORMAL(3*(NN1-1)+3)

                BUFFERS(NI25)%SEND_BUF(L+PP+4)  = INTBUF_TAB(N)%E2S_NOD_NORMAL(3*(NN2-1)+1)
                BUFFERS(NI25)%SEND_BUF(L+PP+5)  = INTBUF_TAB(N)%E2S_NOD_NORMAL(3*(NN2-1)+2)
                BUFFERS(NI25)%SEND_BUF(L+PP+6)  = INTBUF_TAB(N)%E2S_NOD_NORMAL(3*(NN2-1)+3)

C             EID = INTBUF_TAB(N)%LEDGE((IED-1)*NLEDGE+8)

            ENDDO
            IF(SEND_SIZE > 0) THEN
              BUFFERS(NI25)%NBISEND = BUFFERS(NI25)%NBISEND + 1
              MSGTYP = MSGOFF
              I = BUFFERS(NI25)%IAD_SEND(P)
              L = (I-1) * NB_VALUES+1
C           WRITE(6,*) "SEND TO",IT_SPMD(P),SEND_SIZE
C           CALL FLUSH(6)

              CALL MPI_ISEND(
     .          BUFFERS(NI25)%SEND_BUF(L),
     .          SEND_SIZE,
     .          MPI_REAL4,
     .          IT_SPMD(P),
     .          MSGTYP,
     .          MPI_COMM_WORLD,
     .          BUFFERS(NI25)%SEND_RQ(P),
     .          IERROR)
            ENDIF
          ENDDO ! PROC
        ENDIF
      ENDDO ! NI25

#ifdef WITH_ASSERT
      nan32 = ieee_value(nan32,ieee_quiet_nan)
      do ni25=1,ninter25
        n = intlist25(ni25)
        iedge = ipari(58,n)
        if( iedge > 0 ) then
          do p = 1,nspmd
            iglob = 0
            do i =1,nsnfie(n)%p(p) 
              iglob = iglob + 1
              edg_bisector_fie(n)%p(1,1,iglob) = nan32 
              edg_bisector_fie(n)%p(2,1,iglob) = nan32 
              edg_bisector_fie(n)%p(3,1,iglob) = nan32 
              vtx_bisector_fie(n)%p(1,1,iglob) = nan32 
              vtx_bisector_fie(n)%p(2,1,iglob) = nan32 
              vtx_bisector_fie(n)%p(3,1,iglob) = nan32 
              vtx_bisector_fie(n)%p(1,2,iglob) = nan32 
              vtx_bisector_fie(n)%p(2,2,iglob) = nan32 
              vtx_bisector_fie(n)%p(3,2,iglob) = nan32 
              vtx_bisector_fie(n)%p(1,3,iglob) = nan32 
              vtx_bisector_fie(n)%p(2,3,iglob) = nan32 
              vtx_bisector_fie(n)%p(3,3,iglob) = nan32 
              vtx_bisector_fie(n)%p(1,4,iglob) = nan32 
              vtx_bisector_fie(n)%p(2,4,iglob) = nan32 
              vtx_bisector_fie(n)%p(3,4,iglob) = nan32 
            enddo
          enddo
        endif
      enddo
#endif


      DO NI25=1,NINTER25
        N = INTLIST25(NI25)
        IEDGE = IPARI(58,N)
        IF( IEDGE > 0 ) THEN
          DO K = 1,BUFFERS(NI25)%NBIRECV
C           WRITE(6,*) "Go into wait"
            CALL FLUSH(6)
            CALL MPI_WAITANY(NSPMD,BUFFERS(NI25)%RECV_RQ,P,MPI_STATUS_IGNORE,IERROR)
C           WRITE(6,*) "RECIEVE FROM",P-1
            L0 = (BUFFERS(NI25)%IAD_RECV(P) - 1)*NB_VALUES 
            IBEGIN = 0
            IF( P > 1) THEN
              DO L = 1,P-1 
                IF( L - 1 .ne. ISPMD)  IBEGIN = IBEGIN + NSNFIE(N)%P(L) 
              ENDDO
            ENDIF
            DO I =1,NSNFIE(N)%P(P) 
              L = L0 + (I-1) * NB_VALUES  
              IGLOB = I + IBEGIN 
c             WRITE(6,*) iglob,"In:",LEDGE_FIE%P(E_GLOBAL_ID,IGLOB),L
              EDG_BISECTOR_FIE(N)%P(1,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+1 )  
              EDG_BISECTOR_FIE(N)%P(2,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+2 )  
              EDG_BISECTOR_FIE(N)%P(3,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+3 )  
              VTX_BISECTOR_FIE(N)%P(1,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+4 )  
              VTX_BISECTOR_FIE(N)%P(2,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+5 )  
              VTX_BISECTOR_FIE(N)%P(3,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+6 )  
              VTX_BISECTOR_FIE(N)%P(1,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+7 )  
              VTX_BISECTOR_FIE(N)%P(2,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+8 )  
              VTX_BISECTOR_FIE(N)%P(3,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+9 )  
              VTX_BISECTOR_FIE(N)%P(1,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+10) 
              VTX_BISECTOR_FIE(N)%P(2,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+11) 
              VTX_BISECTOR_FIE(N)%P(3,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+12) 
              VTX_BISECTOR_FIE(N)%P(1,4,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+13) 
              VTX_BISECTOR_FIE(N)%P(2,4,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+14) 
              VTX_BISECTOR_FIE(N)%P(3,4,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+15) 


              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+1 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+2 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+3 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+4 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+5 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+6 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+7 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+8 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+9 ))) 
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+10)))
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+11)))
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+12)))
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+13)))
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+14)))
              ASSERT(.NOT. isnan(BUFFERS(NI25)%RECV_BUF(L+15)))

              LEDGE_FIE(N)%P(E_LEFT_SEG ,IGLOB) =
     .    TRANSFER(BUFFERS(NI25)%RECV_BUF(L+16),L0)
              LEDGE_FIE(N)%P(E_LEFT_ID,IGLOB) = 
     .    TRANSFER(BUFFERS(NI25)%RECV_BUF(L+17),L0)
              LEDGE_FIE(N)%P(E_RIGHT_SEG ,IGLOB) =
     .    TRANSFER(BUFFERS(NI25)%RECV_BUF(L+18),L0)
              LEDGE_FIE(N)%P(E_RIGHT_ID,IGLOB) = 
     .    TRANSFER(BUFFERS(NI25)%RECV_BUF(L+19),L0)



              DEBUG_E2E(LEDGE_FIE(N)%P(1,IGLOB)==D_ES, LEDGE_FIE(N)%P(E_RIGHT_SEG ,IGLOB))

#ifdef MYREAL8
              X_SEG_FIE(N)%P(1,1,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+20:L+20+1),ONE) 
              X_SEG_FIE(N)%P(2,1,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+22:L+22+1),ONE) 
              X_SEG_FIE(N)%P(3,1,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+24:L+24+1),ONE) 
              X_SEG_FIE(N)%P(1,2,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+26:L+26+1),ONE) 
              X_SEG_FIE(N)%P(2,2,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+28:L+28+1),ONE) 
              X_SEG_FIE(N)%P(3,2,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+30:L+30+1),ONE) 
              X_SEG_FIE(N)%P(1,3,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+32:L+32+1),ONE)
              X_SEG_FIE(N)%P(2,3,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+34:L+34+1),ONE)
              X_SEG_FIE(N)%P(3,3,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+36:L+36+1),ONE)
              X_SEG_FIE(N)%P(1,4,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+38:L+38+1),ONE)
              X_SEG_FIE(N)%P(2,4,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+40:L+40+1),ONE)
              X_SEG_FIE(N)%P(3,4,IGLOB) =TRANSFER( BUFFERS(NI25)%RECV_BUF(L+42:L+42+1),ONE)
              PP = 43
#else
              X_SEG_FIE(N)%P(1,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+20)  
              X_SEG_FIE(N)%P(2,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+21)  
              X_SEG_FIE(N)%P(3,1,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+22)  
              X_SEG_FIE(N)%P(1,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+23)  
              X_SEG_FIE(N)%P(2,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+24)  
              X_SEG_FIE(N)%P(3,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+25)  
              X_SEG_FIE(N)%P(1,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+26) 
              X_SEG_FIE(N)%P(2,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+27) 
              X_SEG_FIE(N)%P(3,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+28) 
              X_SEG_FIE(N)%P(1,4,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+29) 
              X_SEG_FIE(N)%P(2,4,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+30) 
              X_SEG_FIE(N)%P(3,4,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+31) 
              PP = 31
#endif
              EDG_BISECTOR_FIE(N)%P(1,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+PP+1 )  
              EDG_BISECTOR_FIE(N)%P(2,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+PP+2 )  
              EDG_BISECTOR_FIE(N)%P(3,2,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+PP+3 ) 

              EDG_BISECTOR_FIE(N)%P(1,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+PP+4 )  
              EDG_BISECTOR_FIE(N)%P(2,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+PP+5 )  
              EDG_BISECTOR_FIE(N)%P(3,3,IGLOB) = BUFFERS(NI25)%RECV_BUF(L+PP+6 ) 

            ENDDO
          ENDDO ! RECV
        ENDIF ! IEDGE
      ENDDO ! NINTER25

#ifdef WITH_ASSERT
C debug mode
      nan32 = ieee_value(nan32,ieee_quiet_nan)
      do ni25=1,ninter25
        n = intlist25(ni25)
        iedge = ipari(58,n)
        if( iedge > 0 ) then
          do p = 1,nspmd
            iglob = 0
            do i =1,nsnfie(n)%p(p) 
              iglob = iglob + 1
              ASSERT(.NOT. ieee_is_nan(edg_bisector_fie(n)%p(1,1,iglob))) 
              ASSERT(.NOT. ieee_is_nan(edg_bisector_fie(n)%p(2,1,iglob))) 
              ASSERT(.NOT. ieee_is_nan(edg_bisector_fie(n)%p(3,1,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(1,1,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(2,1,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(3,1,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(1,2,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(2,2,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(3,2,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(1,3,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(2,3,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(3,3,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(1,4,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(2,4,iglob))) 
              ASSERT(.NOT. ieee_is_nan(vtx_bisector_fie(n)%p(3,4,iglob))) 
            enddo
          enddo
        endif
      enddo
#endif


C --------------- Free Send rquest SEND wait 
      DO NI25=1,NINTER25
        N = INTLIST25(NI25)
        IEDGE = IPARI(58,N)
        IF( IEDGE > 0 ) THEN
          CALL MPI_WAITALL(NSPMD,BUFFERS(NI25)%SEND_RQ,MPI_STATUSES_IGNORE,IERROR)
C         CALL MPI_WAITALL(NSPMD,BUFFERS(NI25)%RECV_RQ,MPI_STATUSES_IGNORE,IERROR)
          DEALLOCATE(BUFFERS(NI25)%SEND_BUF)
          DEALLOCATE(BUFFERS(NI25)%RECV_BUF)
          DEALLOCATE(BUFFERS(NI25)%SEND_RQ)
          DEALLOCATE(BUFFERS(NI25)%RECV_RQ)
          DEALLOCATE(BUFFERS(NI25)%IAD_RECV)
          DEALLOCATE(BUFFERS(NI25)%IAD_SEND)
        ENDIF
      ENDDO

#endif
      RETURN
      END

