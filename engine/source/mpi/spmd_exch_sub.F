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
Chd|  SPMD_SUB_BOUNDARIES           source/mpi/spmd_exch_sub.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_SUB_BOUNDARIES(NLOC_DMG,IAD_ELEM,FR_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
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
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "scr02_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*), FR_ELEM(*)
      TYPE (NLOCAL_STR_) ,TARGET :: NLOC_DMG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: COUNT,COUNT2
      INTEGER P,J,TOTAL_NODES,NCOUNT,NOD,NN,NPOS,NDDL

      INTEGER :: LSD,LRD,CC,LOC_PROC,KK
C-----------------------------------------------
C      

C     ---------------------------------
C      SPMD Non Local nodes boundaries
C     ---------------------------------


      ! If already allocated, deallocate non-local IAD_ELEM, IAD_SIZE and FR_ELEM tables      
      IF (ALLOCATED(NLOC_DMG%IAD_ELEM)) DEALLOCATE (NLOC_DMG%IAD_ELEM) 
      IF (ALLOCATED(NLOC_DMG%IAD_SIZE)) DEALLOCATE (NLOC_DMG%IAD_SIZE) 
      IF (ALLOCATED(NLOC_DMG%FR_ELEM )) DEALLOCATE (NLOC_DMG%FR_ELEM) 
C
      ! Allocation of the counter tables
      ALLOCATE(COUNT(NSPMD))  ! For nodes
      ALLOCATE(COUNT2(NSPMD)) ! For d.o.fs
c
      ! Initialization of the counter tables and the total boundaries non-local nodes
      COUNT(1:NSPMD)  = 0
      COUNT2(1:NSPMD) = 0
      TOTAL_NODES     = 0
c
      KK = 1
      IF (NODADT > 0) KK = 2
c
      ! Loop over domains
      DO P=1,NSPMD
        DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1         ! Loop over nodes at the boundaries
          NOD = FR_ELEM(J)                           ! Global number of the nodes
          NN  = NLOC_DMG%IDXI(NOD)                   ! Corresponding non-local node number
          IF (NLOC_DMG%IDXI(NOD) > 0)THEN            ! If the node belongs to the subset
            NPOS = NLOC_DMG%POSI(NN)                 !   Position of its first d.o.f in A
            NDDL = NLOC_DMG%POSI(NN+1) - NPOS        !   Number of additional d.o.fs
            COUNT(P) = COUNT(P) +1                   !   Updating the counter tables
            COUNT2(P)= COUNT2(P)+KK*NDDL
          ENDIF
        ENDDO
        TOTAL_NODES = TOTAL_NODES + COUNT(P)         ! Total number of non-local d.o.fs at the boundaries
      ENDDO
c
      ! Allocation of the non-local IAD_ELEM and FR_ELEM tables
      ALLOCATE(NLOC_DMG%IAD_ELEM(NSPMD+1))
      ALLOCATE(NLOC_DMG%IAD_SIZE(NSPMD+1))
      ALLOCATE(NLOC_DMG%FR_ELEM(TOTAL_NODES))
c
      ! Filling the non-local IAD_ELEM table
      NLOC_DMG%IAD_ELEM(1) = 1
      NLOC_DMG%IAD_SIZE(1) = 1
c
      ! Filling the IAD_ELEM/IAD_SIZE tables of the subset
      DO P=2,NSPMD+1
         NLOC_DMG%IAD_ELEM(P) = NLOC_DMG%IAD_ELEM(P-1) + COUNT(P-1)
         NLOC_DMG%IAD_SIZE(P) = NLOC_DMG%IAD_SIZE(P-1) + COUNT2(P-1)
      ENDDO
c
      ! Filling the non-local FR_ELEM table
      NCOUNT=0
      ! Loop over domains
      DO P=1,NSPMD
         DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1        ! Loop nodes at the boundaries
            NOD = FR_ELEM(J)                         !   Global number of the nodes
            NN  = NLOC_DMG%IDXI(NOD)                 !   Corresponding non-local node number
            IF (NN > 0)THEN                          !   If the nodes belongs to the subset
              NCOUNT = NCOUNT+1                      !     Updating the counter
              NLOC_DMG%FR_ELEM(NCOUNT) = NN          !     Filling the FR_ELEM table of the subset
            ENDIF
         ENDDO
      ENDDO
      
      !WRITE(*,*) 'IAD_ELEM = ', NLOC_DMG%IAD_ELEM
      !WRITE(*,*) 'IAD_SIZE = ', NLOC_DMG%IAD_SIZE
      !WRITE(*,*) 'FR_ELEM  = ', NLOC_DMG%FR_ELEM
      
      IF (ALLOCATED(COUNT))   DEALLOCATE(COUNT)
      IF (ALLOCATED(COUNT2)) DEALLOCATE(COUNT2)

C     -----------------------------
C      SPMD PON SKYLINE boundaries
C     -----------------------------
      IF(IPARIT==1)THEN
!   ------------------------
!   compute the number of send / rcv
        LSD = 0
        LRD = 0
        LOC_PROC = ISPMD + 1
        DO P=1,NSPMD
            DO J=NLOC_DMG%IAD_ELEM(P),NLOC_DMG%IAD_ELEM(P+1)-1
                NN = NLOC_DMG%FR_ELEM(J)
                NDDL = NLOC_DMG%POSI(NN+1) - NLOC_DMG%POSI(NN)
                DO CC = NLOC_DMG%ADDCNE(NN),NLOC_DMG%ADDCNE(NN+1)-1
                  IF( NLOC_DMG%PROCNE(CC).EQ.LOC_PROC)THEN                          
                    LSD=LSD+1
                  ELSEIF(NLOC_DMG%PROCNE(CC).EQ.P) THEN
                    LRD=LRD+1
                  ENDIF
 
               ENDDO
           ENDDO
        ENDDO
!   ------------------------
!   allocation of ISENDSP and IRECSP
        ALLOCATE( NLOC_DMG%ISENDSP(LSD) )
        ALLOCATE( NLOC_DMG%IRECSP(LRD) ) 
        NLOC_DMG%ISENDSP(1:LSD) = 0
        NLOC_DMG%IRECSP(1:LRD) = 0
                                
!   ------------------------
!   fill ISENDSP and IRECSP    
        IF( .NOT.ALLOCATED( NLOC_DMG%IADSDP ) ) ALLOCATE( NLOC_DMG%IADSDP(NSPMD+1) )
        IF( .NOT.ALLOCATED( NLOC_DMG%IADRCP ) ) ALLOCATE( NLOC_DMG%IADRCP(NSPMD+1) ) 
        IF( .NOT.ALLOCATED( NLOC_DMG%FR_NBCC ) ) ALLOCATE( NLOC_DMG%FR_NBCC(2,NSPMD+1) )

        IF( .NOT.ALLOCATED( NLOC_DMG%FR_ELEM_S ) ) ALLOCATE( NLOC_DMG%FR_ELEM_S(LSD) )
        IF( .NOT.ALLOCATED( NLOC_DMG%FR_ELEM_R ) ) ALLOCATE( NLOC_DMG%FR_ELEM_R(LRD) ) 
        NLOC_DMG%FR_ELEM_S(1:LSD) = 0
        NLOC_DMG%FR_ELEM_R(1:LRD) = 0

        NLOC_DMG%IADSDP(1:NSPMD+1) = 0
        NLOC_DMG%IADRCP(1:NSPMD+1) = 0  
        NLOC_DMG%FR_NBCC(1:2,1:NSPMD+1) = 0
 
        LSD = 1
        LRD = 1
        LOC_PROC = ISPMD + 1
        DO P=1,NSPMD
           NLOC_DMG%IADSDP(P)=LSD
           NLOC_DMG%IADRCP(P)=LRD

            DO J=NLOC_DMG%IAD_ELEM(P),NLOC_DMG%IAD_ELEM(P+1)-1
                NN = NLOC_DMG%FR_ELEM(J)
                NDDL = NLOC_DMG%POSI(NN+1) - NLOC_DMG%POSI(NN)

                DO CC = NLOC_DMG%ADDCNE(NN),NLOC_DMG%ADDCNE(NN+1)-1
                  IF( NLOC_DMG%PROCNE(CC).EQ.LOC_PROC)THEN
                    NLOC_DMG%FR_NBCC(1,P) = NLOC_DMG%FR_NBCC(1,P)+KK*NDDL      
                    NLOC_DMG%ISENDSP(LSD)=CC                    
                    NLOC_DMG%FR_ELEM_S(LSD)=NN             
                    LSD=LSD+1
                  ELSEIF(NLOC_DMG%PROCNE(CC).EQ.P) THEN
                    NLOC_DMG%FR_NBCC(2,P) = NLOC_DMG%FR_NBCC(2,P)+KK*NDDL      
                    NLOC_DMG%IRECSP(LRD)=CC 
                    NLOC_DMG%FR_ELEM_R(LRD)=NN                                    
                    LRD=LRD+1
                  ENDIF
 
               ENDDO
           ENDDO
        ENDDO
!   ------------------------
        NLOC_DMG%FR_NBCC(1:2,NSPMD+1) = 0
        DO J=1,NSPMD
            NLOC_DMG%FR_NBCC(1,NSPMD+1) = NLOC_DMG%FR_NBCC(1,NSPMD+1) + NLOC_DMG%FR_NBCC(1,J)
            NLOC_DMG%FR_NBCC(2,NSPMD+1) = NLOC_DMG%FR_NBCC(2,NSPMD+1) + NLOC_DMG%FR_NBCC(2,J)
        ENDDO

        NLOC_DMG%IADSDP(NSPMD+1)=LSD
        NLOC_DMG%IADRCP(NSPMD+1)=LRD
      ENDIF
      END


Chd|====================================================================
Chd|  SPMD_EXCH_SUB_POFF            source/mpi/spmd_exch_sub.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SUB_POFF(NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
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
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr02_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (NLOCAL_STR_) ,TARGET :: NLOC_DMG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,K,L,M,ND
      INTEGER NDDL,NSIZESUB,NN,NPOS,SIZEA

C MPI  VARIABLES

      INTEGER MSGTYP,NOD,LOC_PROC, SIZ,NB_NOD,NB,MAXLEV,VALUE

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      INTEGER MSGOFF1
      INTEGER IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1)
      INTEGER SEND_SIZ(NSPMD),RECV_SIZ(NSPMD)
      INTEGER SIZ_SEND,SIZ_RECV

      INTEGER REQ_S(NSPMD)
      INTEGER REQ_R(NSPMD)
      
      my_real, DIMENSION(:), ALLOCATABLE :: SEND_BUF,RECV_BUF

      DATA MSGOFF1/1282/
c=======================================================================
      NSIZESUB=NLOC_DMG%NNOD
      SIZEA = NLOC_DMG%L_NLOC
C-------------------------------
C 1. SEND & RECIEVE Buffer size
C-------------------------------
      SIZ = NLOC_DMG%IAD_SIZE(NSPMD+1)-NLOC_DMG%IAD_SIZE(1)
      ALLOCATE(RECV_BUF(SIZ))
      ALLOCATE(SEND_BUF(SIZ))
c--------------------------------------------------------------------
c     MPI receive
c--------------------------------------------------------------------
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = NLOC_DMG%IAD_SIZE(I+1)-NLOC_DMG%IAD_SIZE(I)
        IF (SIZ > 0)THEN
          MSGTYP = MSGOFF1
          CALL MPI_IRECV( RECV_BUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G                    MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
C --------------------------------------------------------------------
C Prepare send buffers
C --------------------------------------------------------------------
      K=1
      IAD_SEND(1)=K
      DO I=1,NSPMD
         DO J=NLOC_DMG%IAD_ELEM(I),NLOC_DMG%IAD_ELEM(I+1)-1
           NN = NLOC_DMG%FR_ELEM(J)
           NPOS = NLOC_DMG%POSI(NN)
           NDDL = NLOC_DMG%POSI(NN+1)-NLOC_DMG%POSI(NN)

           DO L=1,NDDL
              SEND_BUF(K) = NLOC_DMG%FNL(NPOS+L-1,1)
              K = K + 1
              IF (NODADT > 0) THEN    
                SEND_BUF(K) = NLOC_DMG%STIFNL(NPOS+L-1,1)
                K = K + 1
              ENDIF 
           ENDDO
         ENDDO
         IAD_SEND(I+1)=K
      ENDDO
c--------------------------------------------------------------------
c    MPI send
c--------------------------------------------------------------------
      DO I=1,NSPMD
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          IF (SIZ > 0)THEN
            L = IAD_SEND(I)
            MSGTYP = MSGOFF1
            CALL MPI_ISEND(
     S        SEND_BUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
      ENDDO
c--------------------------------------------------------------------
c     Receive buffer & compute recieve_size
c-------------------------------------------------------------------
      K=1
      DO I = 1, NSPMD
        SIZ = NLOC_DMG%IAD_SIZE(I+1)-NLOC_DMG%IAD_SIZE(I)
        IF (SIZ > 0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)

          K = IAD_RECV(I)

          DO J=NLOC_DMG%IAD_ELEM(I),NLOC_DMG%IAD_ELEM(I+1)-1
             NN = NLOC_DMG%FR_ELEM(J)
             NPOS = NLOC_DMG%POSI(NN)
             NDDL = NLOC_DMG%POSI(NN+1) -  NLOC_DMG%POSI(NN)

             DO L=1,NDDL
                NLOC_DMG%FNL(NPOS+L-1,1)=NLOC_DMG%FNL(NPOS+L-1,1)+RECV_BUF(K)
                K=K+1
                IF (NODADT > 0) THEN
                  NLOC_DMG%STIFNL(NPOS+L-1,1)=NLOC_DMG%STIFNL(NPOS+L-1,1)+RECV_BUF(K)
                  K = K + 1
                ENDIF
             END DO

          END DO
        ENDIF
      END DO


c--------------------------------------------------------------------
c     Terminate send
c-------------------------------------------------------------------
      DO I = 1, NSPMD
        SIZ = IAD_SEND(I+1)-IAD_SEND(I)
        IF (SIZ > 0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO

      DEALLOCATE(SEND_BUF)
      DEALLOCATE(RECV_BUF)
#endif

      RETURN
      END


Chd|====================================================================
Chd|  SPMD_EXCH_SUB_PON             source/mpi/spmd_exch_sub.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SUB_PON(NLOC_DMG)
!$COMMENT
!       SPMD_EXCH_SUB_PON
!       communication of neighbouring values for 
!       NLOCAL option and parith/ON
!       
!       SPMD_EXCH_SUB_PON organization :
!       - prepare the non-blocking received
!       - fill the sending buffer
!       - send the sending buffer
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
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
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr02_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (NLOCAL_STR_) ,TARGET :: NLOC_DMG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,K,L,M,ND,IJK
      INTEGER NDDL,NSIZESUB,NN,NPOS,SIZEA

C MPI  VARIABLES

      INTEGER MSGTYP,NOD,LOC_PROC, SIZ,NB_NOD,NB,MAXLEV,VALUE

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      INTEGER MSGOFF1
      INTEGER IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1)
      INTEGER SEND_SIZ(NSPMD),RECV_SIZ(NSPMD)
      INTEGER SIZ_SEND,SIZ_RECV

      INTEGER :: SIZ_S,SIZ_R
      INTEGER REQ_S(NSPMD)
      INTEGER REQ_R(NSPMD)
      
      my_real, DIMENSION(:), ALLOCATABLE :: SEND_BUF,RECV_BUF

      DATA MSGOFF1/1283/
c=======================================================================
      NSIZESUB=NLOC_DMG%NNOD
      SIZEA = NLOC_DMG%L_NLOC
C-------------------------------
C 1. SEND & RECIEVE Buffer size
C-------------------------------
      SIZ_S = NLOC_DMG%FR_NBCC(1,NSPMD+1)
      SIZ_R = NLOC_DMG%FR_NBCC(2,NSPMD+1)
      ALLOCATE(RECV_BUF(SIZ_R))
      ALLOCATE(SEND_BUF(SIZ_S))

      RECV_BUF(1:SIZ_R) = -123456.
      SEND_BUF(1:SIZ_S) = 123456.
c--------------------------------------------------------------------
c     MPI receive
c--------------------------------------------------------------------
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = NLOC_DMG%FR_NBCC(2,I)
        IF (SIZ > 0)THEN
          MSGTYP = MSGOFF1
          CALL MPI_IRECV( RECV_BUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G                    MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO

C --------------------------------------------------------------------
C Prepare send buffers
C --------------------------------------------------------------------
      K=1
      IAD_SEND(1)=K
      DO I=1,NSPMD
          DO J = NLOC_DMG%IADSDP(I), NLOC_DMG%IADSDP(I+1)-1
           NN = NLOC_DMG%FR_ELEM_S(J)
           NPOS = NLOC_DMG%POSI(NN)
           NDDL = NLOC_DMG%POSI(NN+1)-NLOC_DMG%POSI(NN)
           IJK = NLOC_DMG%ISENDSP(J)
           DO L=1,NDDL
              SEND_BUF(K) = NLOC_DMG%FSKY(IJK,L)
              K = K + 1
              IF (NODADT > 0) THEN
                SEND_BUF(K) = NLOC_DMG%STSKY(IJK,L)
                K = K + 1              
              ENDIF
           ENDDO

         ENDDO
         IAD_SEND(I+1)=K
      ENDDO
c--------------------------------------------------------------------
c    MPI send
c--------------------------------------------------------------------
      DO I=1,NSPMD
          SIZ = NLOC_DMG%FR_NBCC(1,I)
          IF (SIZ > 0)THEN
            L = IAD_SEND(I)
            MSGTYP = MSGOFF1
            CALL MPI_ISEND(
     S        SEND_BUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
      ENDDO
c--------------------------------------------------------------------
c     Receive buffer & compute recieve_size
c-------------------------------------------------------------------
      K=1
      DO I = 1, NSPMD
        SIZ = NLOC_DMG%FR_NBCC(2,I)
        IF (SIZ > 0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)

          K = IAD_RECV(I)
          DO J=NLOC_DMG%IADRCP(I),NLOC_DMG%IADRCP(I+1)-1
             NN = NLOC_DMG%FR_ELEM_R(J)
             NPOS = NLOC_DMG%POSI(NN)
             NDDL = NLOC_DMG%POSI(NN+1) -  NLOC_DMG%POSI(NN)

             IJK = NLOC_DMG%IRECSP(J)
             DO L=1,NDDL
                NLOC_DMG%FSKY(IJK,L) = RECV_BUF(K)
                K = K + 1
                IF (NODADT > 0) THEN
                  NLOC_DMG%STSKY(IJK,L) = RECV_BUF(K) 
                  K = K + 1              
                ENDIF
             ENDDO
          END DO
        ENDIF
      END DO

c--------------------------------------------------------------------
c     Terminate send
c-------------------------------------------------------------------
      DO I = 1, NSPMD
        SIZ = NLOC_DMG%FR_NBCC(1,I)
        IF (SIZ > 0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO

      DEALLOCATE(SEND_BUF)
      DEALLOCATE(RECV_BUF)
#endif

      RETURN
      END SUBROUTINE SPMD_EXCH_SUB_PON



