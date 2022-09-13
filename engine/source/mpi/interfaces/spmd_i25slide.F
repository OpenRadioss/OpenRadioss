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
Chd|  SPMD_I25_SLIDE_EXCH           source/mpi/interfaces/spmd_i25slide.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|        POINTERDEF                    share/modules/pointerdef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_I25_SLIDE_EXCH(IBUF    ,RBUF      ,ISIZ    ,RSIZ 
     .                              ,NB      ,COMM_INT, COMM_REAL, COMM_SIZ
     .                              ,MODE    ,NIN,      COMM_PATTERN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MPI_COMMOD
      USE POINTERDEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "my_allocate.inc"
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
      INTEGER, INTENT(IN) :: MODE
      ! MODE 0 =>  ISEND sizes
      ! MODE 1 =>  ISEND buffer
      ! MODE 2 =>  Recieve the sizes 
      ! MODE 3 =>  Recieve the messages
      ! MODE 4 =>  Deallocate Recv buffer
      ! MODE 5 =>  Deallocate Send buffer 
      INTEGER, INTENT(IN) :: NIN ! Interface number
      INTEGER, INTENT(IN) :: RSIZ,ISIZ
      ! Real and Integer buffer size
      INTEGER, INTENT(INOUT) :: NB(NSPMD,NINTER25) 
      ! Number of secnds node to exchange (send or recv depending on MODE)
      INTEGER, INTENT(IN) :: COMM_PATTERN(NSPMD,NINTER25) 
      ! COMM_PATTERN(P,NIN) == 1 => Proc ISPMD and P shares nodes in NIN interface


      ! Communication structures
      TYPE(MPI_COMM_STRUCT):: COMM_INT
      TYPE(MPI_COMM_STRUCT):: COMM_REAL
      TYPE(MPI_COMM_STRUCT):: COMM_SIZ

      ! Buffers
      TYPE(real_pointer),    DIMENSION(NSPMD,NINTER25) :: RBUF
      TYPE(int_pointer) ,    DIMENSION(NSPMD,NINTER25) :: IBUF  
      INTEGER MSGTYP
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR
      INTEGER P,PP,LOC_PROC
      INTEGER LENGTH
      INTEGER MSGOFF1,MSGOFF2,MSGOFF3
      INTEGER NBCOM
      DATA MSGOFF1/2400/
      DATA MSGOFF2/2500/
      DATA MSGOFF3/2600/



#ifdef MPI
      INTEGER STATUS (MPI_STATUS_SIZE)

      LOC_PROC = ISPMD + 1 

      IF(MODE == 0) THEN        
      !SEND sizes
        DO P = 1, NSPMD
          IF(P /= LOC_PROC .AND. COMM_PATTERN(P,NIN) == 1) THEN
            MSGTYP = MSGOFF1 
            COMM_SIZ%SIZ(P,NIN) = 1
            COMM_SIZ%TAG(P,NIN) = MSGTYP 
            CALL MPI_ISEND(
     1        NB(P,NIN),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,COMM_SIZ%SEND_RQ(P,NIN),ierror)
          ELSE
             COMM_SIZ%SEND_RQ(P,NIN) = MPI_REQUEST_NULL
          ENDIF
        ENDDO
      ELSE IF(MODE == 1) THEN        
      !SEND
         DO P = 1, NSPMD
           IF(P /= LOC_PROC .AND. COMM_PATTERN(P,NIN) == 1) THEN
             IF(NB(P,NIN) > 0) THEN 
               LENGTH = NB(P,NIN)*RSIZ
               MSGTYP = MSGOFF2 
               COMM_REAL%SIZ(P,NIN) = LENGTH
               COMM_REAL%TAG(P,NIN) = MSGTYP 
               
               CALL MPI_ISEND(
     1           RBUF(P,NIN)%P(1),LENGTH,REAL,IT_SPMD(P),MSGTYP,
     2           MPI_COMM_WORLD,COMM_REAL%SEND_RQ(P,NIN),ierror)
               
               MSGTYP = MSGOFF3 
               LENGTH = NB(P,NIN) * ISIZ
               COMM_INT%SIZ(P,NIN) = LENGTH
               COMM_INT%TAG(P,NIN) = MSGTYP 
               CALL MPI_ISEND(
     1           IBUF(P,NIN)%P(1),LENGTH,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2           MPI_COMM_WORLD,COMM_INT%SEND_RQ(P,NIN),ierror)
             ELSE
                 COMM_INT%SEND_RQ(P,NIN) = MPI_REQUEST_NULL
                 COMM_REAL%SEND_RQ(P,NIN) = MPI_REQUEST_NULL
             ENDIF
           ENDIF
         ENDDO
       ELSEIF(MODE==2) THEN         
       !RECV SIZES
         DO P = 1, NSPMD
            IF( P/=LOC_PROC .AND. COMM_PATTERN(P,NIN) == 1) THEN
             MSGTYP = MSGOFF1 
             COMM_REAL%SIZ(P,NIN) = 1

             CALL MPI_IRECV(
     1         NB(P,NIN),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,COMM_SIZ%RECV_RQ(P,NIN),ierror)
             ELSE
               COMM_SIZ%RECV_RQ(P,NIN) = MPI_REQUEST_NULL
             ENDIF 
         ENDDO

       ELSEIF(MODE == 3) THEN
         !WAIT
         NBCOM = 0
         DO P = 1,NSPMD
           COMM_REAL%SIZ(P,NIN) = 0 
           COMM_INT%SIZ(P,NIN) = 0 
           IF(P/= LOC_PROC .AND. COMM_PATTERN(P,NIN) == 1) NBCOM = NBCOM + 1
         ENDDO
         
         DO PP = 1,NBCOM
C          IF(P/= LOC_PROC .AND. COMM_PATTERN(P,NIN) == 1) THEN
C            CALL MPI_WAIT(COMM_SIZ%RECV_RQ(P,NIN),STATUS,IERROR)
             CALL MPI_WAITANY(NSPMD,COMM_SIZ%RECV_RQ(1:NSPMD,NIN),P,STATUS,IERROR)

             IF(NB(P,NIN) > 0) THEN
               LENGTH = NB(P,NIN)*RSIZ
               MSGTYP = MSGOFF2 
               COMM_REAL%SIZ(P,NIN) = NB(P,NIN) 
               MY_ALLOCATE(RBUF(P,NIN)%P,LENGTH)
               RBUF(P,NIN)%P(1:LENGTH) = 0
               
               CALL MPI_RECV(
     1           RBUF(P,NIN)%P(1),LENGTH,REAL,IT_SPMD(P),MSGTYP,
     2           MPI_COMM_WORLD,STATUS,ierror)
               
               LENGTH = NB(P,NIN)*ISIZ
               MSGTYP = MSGOFF3 
               COMM_INT%SIZ(P,NIN) = NB(P,NIN) 
             
               MY_ALLOCATE(IBUF(P,NIN)%P,LENGTH)
               IBUF(P,NIN)%P(1:LENGTH) = 0
             
               CALL MPI_RECV(
     1           IBUF(P,NIN)%P(1),LENGTH,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2           MPI_COMM_WORLD,STATUS,ierror)
             ENDIF
C          ENDIF
         ENDDO
 
       ELSEIF(MODE == 4) THEN
         !CLEAN
         DO P = 1, NSPMD
           IF(NB(P,NIN) > 0 .AND. COMM_PATTERN(P,NIN) == 1) THEN
             DEALLOCATE(IBUF(P,NIN)%P)
             DEALLOCATE(RBUF(P,NIN)%P)
           ENDIF
         ENDDO
       ELSEIF(MODE == 5) THEN
         !CLEAN
         DO P = 1, NSPMD
            IF(P /= LOC_PROC .AND. COMM_PATTERN(P,NIN) == 1) THEN
             CALL MPI_WAIT(COMM_SIZ%SEND_RQ(P,NIN),STATUS,IERROR)
            ENDIF
            IF(NB(P,NIN) > 0 .AND. COMM_PATTERN(P,NIN) == 1) THEN
              CALL MPI_WAIT(COMM_INT%SEND_RQ(P,NIN),STATUS,IERROR)
              DEALLOCATE(IBUF(P,NIN)%P)
              CALL MPI_WAIT(COMM_REAL%SEND_RQ(P,NIN),STATUS,IERROR)
              DEALLOCATE(RBUF(P,NIN)%P)
            ENDIF
         ENDDO
       ENDIF

#endif
      RETURN
      END


C   
Chd|====================================================================
Chd|  SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        DEALLOCATE_FI1_TMP            source/mpi/interfaces/spmd_i25slide.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        REALLOCATE_FI1                source/mpi/interfaces/spmd_i25slide.F
Chd|        REALLOCATE_FI2                source/mpi/interfaces/spmd_i25slide.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IFRONT_MOD                    share/modules/ifront_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25TMP                      share/modules/tri25tmp_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I25_SLIDE_GAT(NSN  ,NIN, NI25,
     2                             IGAP  ,NSNR   ,INTTH   ,ILEV,INTBUF_TAB,
     3                             FR_NOR, IAD_FRNOR, NB_SLID   ,ITAB, H3D_DATA,
     4                             INTFRIC,FLAGREMN , LREMNORMAX,NRTM,IVIS2 )
c
c -  Nodes in IREM are not necessarly from their PMAIN processor.
c -  IREM is reordered according to ITAB for each PMAIN 
c        INDX_FOR_P(0) = positions in IREM of the sliding secnd nodes that
c                        have 0 as PMAIN
c
c -  IAUX( "position in new FI" ) =  value
c           value = 0 =>  Secnd is sliding, but is a local node
c                         IAUX_K("position in new FI") = position in INTBUF_TAB
c           value < 0 => -value is position in INDX_FOR_P (secnd is sliding)   
c           value > 0 =>  value is position in old *FI structures
c                          (TMP%ITAFI) (secnd is not sliding)
c    Remark "position in new FI" is the position in the new *FI structure considering
c            before the removal of nodes that are actually local (and will be put into 
c            INTBUF_TAB). 
c  
c  
c - INDEX( "numbering on old FI" ) = numbering in new FI
c   - This is be used to renumber the remote part of CAND_N , CAND_OPT_N
c   
c   Remark If a secnd node known as an old remote slides
c    (present in XREM and old TMP%*FI strucutre), then the values in *FI structures
c    are replaced by the values coming from [XI]REM. 
c
c  Example using NSPMD = 4. 
c  proc P3 recieved some incoming sliding nodes from P0 P1 & P2, and 
c  store the secnds nodes that have P0 as PMAIN:
c     - The node with UID 500 is sliding from P1 to P3 and has P0 as PMAIN 
c        This node is a new (was not included in the old *FI structures).  
c        It will be store as the 1st remote nodes (ITAFI(nin)%P(5) = 500)
c
c     - The node 1000 is sliding from P0 to P3, and has P0 as PMAIN.
c       This node was already remote on P3
c       (old position = I, new position = INDEX("old position = 1" ) = 5)
c
c
c  "Pmain"    "ITAB"
c   IREM(3,:)  IREM(2,:) 
c        +----+----+
c  Sent 1|    |....|
c  BY    +----+----+
c  P    2| 0  |1000|<--------+
c  0     +----+----+         |
c       3|    |....|         |
c        +----+----+         |
c       4|    |....|         |
c        +----+----+         |
c       5|    |....|         |
c  =================         |
c       6|    |....|         |
c        +----+----+         |   INDX_FOR_P(0) : array of index into IREM of 
c       7| 0  | 500|<------+ |   +----+          Secnds that have 0 as PMAIN    
c  P     +----+----+       +-----|  7 |1         (sorted by USER ID)
c  1    8| 0  | 600|<----+   |   +----+
c        +----+----+     +-------|  8 |2
c       9| 0  | 700|         |   +----+
c        +----+----+         |   |  9 |3
c      10|    |....|         |   +----+
c  =================         |   | 13 |4
c      11|    |....|         |   +----+                  
c        +----+----+         +---|  2 |5<----------+      (New) ITAFI      
c      12|    |....|             +----+            |         +----+
c  P     +----+----+                               |         |500 | 1           
c  2   13| 0  | 800|                               |         +----+ 
c        +----+----+                               |         |600 | 2
c      14|    |....|                        IAUX(5)=-5       +----+
c        +----+----+                               |         |700 | 3
c                                                  |         +----+      P0  
c                                                  |         |800 | 4                   
c            (old)                                 |         +----+                    
c           TMP%ITAFI                  +-----------+-------- |1000| 5                   
c             +----+    INDEX(1)=5     |                     +----+
c            1|1000| <-----------------+                     |1001| 6
c  P0         +----+                                    =====================
c            2|1001|                                         |    | 7
c  =================                                         +----+
c            3|    | IDEB_OLD                                |    | 8
c  P2         +----+                                         +----+
c            4|    |                                         |    | 9
c             -----+                                         +----+
c            5|    |                                         |    | 10
c             -----+                                         +----+
c                                                             ....    
c                                                            +----+
c                                                            |    | 19
c                                                            +----+

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IFRONT_MOD
      USE TRI7BOX
      USE TRI25TMP
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE H3D_MOD  
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
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      INTEGER, INTENT(IN) :: NIN, NI25, NSN, IGAP, INTTH,ILEV,INTFRIC,FLAGREMN,
     .        LREMNORMAX, NRTM, IVIS2,
     .        NB_SLID(NSPMD), ! NB of new sliding secnd nodes
     .        ITAB(*)
      INTEGER, INTENT(INOUT) :: NSNR
      INTEGER IAD_FRNOR(NINTER25,NSPMD+1),FR_NOR(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NSNR_OLD,NSNR_NEW,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K,L,JJ, I_STOK, IX, II
      INTEGER NSNR_TOT
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX ! renumbering cand_n
      INTEGER  NN2,RSHIFT,ISHIFT, IOLDNSNFI, ND, JDEB, Q
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX,IAUX_LOCAL,IAUX_REV,IAUX_K,IAUX_L
      INTEGER IDEB_NEW  ! shift for resulting *FI 
      INTEGER IDEB_OLD  ! shift for old *FI
      INTEGER IDEB_SLID ! shift for new sliding nodes
      INTEGER PNEW,POLD,ID_OLD,ID_NEW 
      INTEGER SNEW,SOLD,PMAIN,NB_SLID_TOT
      my_real,
     .    DIMENSION(:), ALLOCATABLE :: PENEFI_OLD, STIFFI_OLD
      INTEGER MARGIN,N_NEW_SECND,UID
      TYPE(I25_TMP_STRUCT) :: TMP
      TYPE(int_pointer) , DIMENSION(NSPMD) :: INDX_FOR_P,UID_FOR_P
      INTEGER, ALLOCATABLE, DIMENSION(:) :: PERMUTATIONS, PROC_ORIG
      INTEGER SINDEX(NSPMD), SIZE_PERM_MAX, PROC_FROM
      INTEGER WORK(70000)
      INTEGER PLOCAL, NBLOCAL, PM, KK, KM, KI, LL, SIZREMNORFI, SIZ, NE
      INTEGER, DIMENSION(:), ALLOCATABLE :: REMNOR_FI_TMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      NODFI = 0
      LSKYFI= 0
      NB_SLID_TOT = 0
      NSNR_TOT = 0
C Reperage des candidats

C
C Allocation des tableaux de frontieres interfaces
C
      DO P = 1,NSPMD
         NODFI =  NODFI + NSNFI(NIN)%P(P) + NB_SLID(P)
         NB_SLID_TOT = NB_SLID_TOT + NB_SLID(P) 
         NSNR_TOT = NSNR_TOT + NSNFI(NIN)%P(P) 
      ENDDO
      MY_ALLOCATE(PROC_ORIG,NB_SLID_TOT)
      IDEB = 0
      DO P = 1,NSPMD
         PROC_ORIG(IDEB+1:IDEB+NB_SLID(P)) =  P
         IDEB = IDEB + NB_SLID(P)
      ENDDO


      MY_ALLOCATE(INDEX,NSNR_TOT)
      INDEX(1:NSNR_TOT) = 0
      CALL REALLOCATE_FI1(NODFI,TMP,NIN,INTTH,IGAP,ILEV,INTFRIC,FLAGREMN,IVIS2)
      NSNFI(NIN)%P(1:NSPMD) = TMP%NSNFI%P(1:NSPMD)
C
C Compactage des candidats
C
      IDEB = 0
      IDEB_NEW = 0  
      JDEB = 0
      IDEB_NEW = 0
      IDEB_OLD = 0
      IDEB_SLID = 0

!
!     sort  XREM/IREM wrt to main processor
!     only new remote kept in XREM                                                                      
!
!
      DO P = 1,NSPMD
        ! overestimation
        nullify(INDX_FOR_P(P)%P)
        nullify(UID_FOR_P(P)%P)
        MY_ALLOCATE(INDX_FOR_P(P)%P,MAX(NB_SLID_TOT,1))
        MY_ALLOCATE(UID_FOR_P(P)%P,MAX(1,NB_SLID_TOT))
        INDX_FOR_P(P)%P(1:NB_SLID_TOT) = 0
        UID_FOR_P(P)%P(1:NB_SLID_TOT) = 0
        SINDEX(P) = 0
      ENDDO

! 
      DO I = 1,NB_SLID_TOT 
        UID = IREM(2,I)
        P = IREM(3,I) ! PMAIN
        SINDEX(P) = SINDEX(P) +  1
!       Array of pointer to XREM  (per PMAIN)
        INDX_FOR_P(P)%P(SINDEX(P)) = I
        UID_FOR_P(P)%P(SINDEX(P)) = UID 
      ENDDO 

      SIZE_PERM_MAX = 0
      DO P = 1, NSPMD
         SIZE_PERM_MAX = MAX(SIZE_PERM_MAX,SINDEX(P))
      ENDDO

      MY_ALLOCATE(PERMUTATIONS,2*SIZE_PERM_MAX)
      IF(FLAGREMN == 2 ) THEN
         MY_ALLOCATE(REMNOR_FI_TMP,NODFI*LREMNORMAX)
         KI = 0
      ENDIF
      DO P = 1, NSPMD
      ! Pour l'instant on garde l'iteration P = LOC_PROC
      ! voir avec  ce qu'il faut faire dans ce cas
        NN = 0
        NSNR_OLD = NSNFI(NIN)%P(P)
        NSNR_NEW = SINDEX(P) ! NSNR from XREM, we don't know yet if they are already knwon 
        NSNFI(NIN)%P(P) = NSNR_NEW + NSNR_OLD 

        IF(NSNR_OLD +  NSNR_NEW > 0) THEN

         ! Tri INDX_FOR_P en fonction de UID on each proc
          IF(NSNR_NEW > 0) THEN
            CALL MY_ORDERS(0,WORK,UID_FOR_P(P)%P,PERMUTATIONS,NSNR_NEW,1)
            DO I = 1,NSNR_NEW
              PERMUTATIONS(I) =  INDX_FOR_P(P)%P(PERMUTATIONS(I))  
            ENDDO
            DO I = 1,NSNR_NEW
              INDX_FOR_P(P)%P(I) = PERMUTATIONS(I) 
            ENDDO
          ENDIF

          MY_ALLOCATE(IAUX,NSNR_OLD+NSNR_NEW)
          MY_ALLOCATE(IAUX_LOCAL,NSNR_OLD+NSNR_NEW)
          MY_ALLOCATE(IAUX_REV,NSNR_OLD+NSNR_NEW)
          MY_ALLOCATE(IAUX_K,NSNR_OLD+NSNR_NEW)
          MY_ALLOCATE(IAUX_L,NSNR_OLD+NSNR_NEW)

          POLD=1
          PNEW=1

          ! IAUX : array of index for the merge sort of *FI and *REM 
          I = 1
          IX= 1
          PLOCAL = 1
          NBLOCAL = 0
          DO WHILE(POLD<= NSNR_OLD .OR. PNEW <= NSNR_NEW) 
 
        ! DO I = 1, NSNR_OLD + NSNR_NEW
            IF(POLD > NSNR_OLD) THEN
              ID_OLD = 0
            ELSE
              ID_OLD = TMP%ITAFI%P(IDEB_OLD+POLD)
            ENDIF
            IF(PNEW > NSNR_NEW) THEN
            !it happens if NSNR_NEW = 0 for eg
            !In that case we use a dummy ID_NEW 
            !that will not bo chosen
              ID_NEW = ID_OLD + 1  
            ELSE
              ID_NEW = IREM(2,INDX_FOR_P(P)%P(PNEW))
            ENDIF
            IF((ID_NEW > ID_OLD .OR. PNEW > NSNR_NEW).AND. POLD <= NSNR_OLD) THEN
              INDEX(IDEB_OLD+POLD) = IX + IDEB_NEW
              IX = IX + 1
              IAUX(I) = POLD
              POLD = POLD + 1 
!             IF(ID_OLD==27569)  WRITE(6,*) ispmd+1,"NODE :",ID_OLD,"Is old",
!     .            ideb_old+pold-1,index(ideb_old+pold-1)
            ELSEIF ((ID_NEW < ID_OLD .OR. POLD > NSNR_OLD) .AND. PNEW <= NSNR_NEW) THEN

              ! Search in NSV (NSV must be sorted wrt ITAB)
              IF(NSN > 0)THEN
                DO WHILE( ITAB(INTBUF_TAB%NSV(PLOCAL)) < ID_NEW .AND. PLOCAL < NSN )
                 PLOCAL = PLOCAL + 1  
                ENDDO
                IF(ITAB(INTBUF_TAB%NSV(PLOCAL)) == ID_NEW) THEN
                  ! if the node is local
                  NBLOCAL = NBLOCAL + 1 
                  IAUX_REV(NBLOCAL) = I 
                  IAUX_LOCAL(NBLOCAL) = PLOCAL
!                 IF(ID_NEW==27569) WRITE(6,*) ispmd+1,"NODE :",ID_NEW,"Is Local:", PLOCAL
                ELSE
!                  IF(ID_NEW==27569) WRITE(6,*) ispmd+1,"NODE :",ID_NEW,"is new"
                   IX = IX + 1
                ENDIF
                IAUX(I) = -PNEW
                PNEW = PNEW + 1
              ELSE
                ! some processor may have 0 secnd nodes
                IX = IX + 1
                IAUX(I) = -PNEW
                PNEW = PNEW + 1
              END IF
            ELSEIF (ID_NEW == ID_OLD .AND. PNEW <= NSNR_NEW .AND. POLD <= NSNR_OLD) THEN
              ! In the case the secnd sliding from another processor
              ! is knwon already on this processor, then we keep the new values
              ! in IREM and XREM
              NSNFI(NIN)%P(P) =  NSNFI(NIN)%P(P) - 1
              INDEX(IDEB_OLD+POLD) = IX + IDEB_NEW
              IX = IX + 1
              IAUX(I) = -PNEW
              PNEW = PNEW + 1
              POLD = POLD + 1
!             IF(ID_NEW==27569)  WRITE(6,*) ispmd+1,"NODE :",ID_NEW,"Is already in frontier",
!     .         ideb_old+pold-1,INDEX(IDEB_OLD+POLD-1)
            ENDIF
            I = I + 1
          ENDDO


C ========================================================
C If a secnd is already known, update local strcuture here
C Or in ELSEIF(I == 0) THEN blocks below
C ========================================================
          DO J = 1,NBLOCAL
            I = IAUX(IAUX_REV(J))
            L = INDX_FOR_P(P)%P(-I)
            K = IAUX_LOCAL(J)
            IAUX_K(IAUX_REV(J)) = K
            IAUX_L(IAUX_REV(J)) = L
            IAUX(IAUX_REV(J)) = 0  !flag to identify local nodes
          ENDDO

C ========================================================
C If a secnd is already known, update local strcuture here
C ========================================================



          ! NN = Taille de XREM + Taille ancien FI - noeuds communs
          NN = NSNFI(NIN)%P(P)
          NSNFI(NIN)%P(P) = NN - NBLOCAL

C =============================================================
          NBLOCAL = 0
          DO J=1,NN ! NN is the number of unique nodes that are in X
            I = IAUX(J)
            K = IDEB_NEW+J-NBLOCAL
            IF(I > 0 )THEN
              ! IAUX > 0 => Pointer to  secnds already in FI strcuture
              L = IDEB_OLD+I
              XFI(NIN)%P(1,K)     = TMP%XFI%P(1,L)
              XFI(NIN)%P(2,K)     = TMP%XFI%P(2,L)
              XFI(NIN)%P(3,K)     = TMP%XFI%P(3,L)
              VFI(NIN)%P(1,K)     = TMP%VFI%P(1,L)
              VFI(NIN)%P(2,K)     = TMP%VFI%P(2,L)
              VFI(NIN)%P(3,K)     = TMP%VFI%P(3,L)
               MSFI(NIN)%P(K)     = TMP%MSFI%P(L)
              STIFI(NIN)%P(K)     = TMP%STIFI%P(L)                                       
              NSVFI(NIN)%P(K)     = TMP%NSVFI%P(L)
              ITAFI(NIN)%P(K)     = TMP%ITAFI%P(L) 
              PMAINFI(NIN)%P(K) = TMP%PMAINFI%P(L)
              KINFI(NIN)%P(K)     = TMP%KINFI%P(L)
            ELSEIF( I < 0 ) THEN
              ! IAUX < 0 => Pointer to secnds new secnds in XREM
              L = INDX_FOR_P(P)%P(-I)
              XFI(NIN)%P(1,K)     = XREM(1,L)
              XFI(NIN)%P(2,K)     = XREM(2,L)
              XFI(NIN)%P(3,K)     = XREM(3,L)
              VFI(NIN)%P(1,K)     = XREM(4,L)
              VFI(NIN)%P(2,K)     = XREM(5,L)
              VFI(NIN)%P(3,K)     = XREM(6,L)
               MSFI(NIN)%P(K)     = XREM(7,L)
              STIFI(NIN)%P(K)     = XREM(8,L)
              NSVFI(NIN)%P(K)     = IREM(1,L)
              ITAFI(NIN)%P(K)     = IREM(2,L)
              PMAINFI(NIN)%P(K) = IREM(3,L)
              KINFI(NIN)%P(K)     = IREM(4,L)
            ELSEIF(I == 0) THEN
              ! IAUX=0 => Secnd is known locally (in NSV),
              ! we do not put in the FI structures
              NBLOCAL = NBLOCAL +1
            ENDIF
          END DO

C
C shift for real variables (prepare for next setting)
              RSHIFT = 9

C shift for integer variables (prepare for next setting) 
              ISHIFT = 8 

C  symmetric plane
          IF(.TRUE. )THEN
             NBLOCAL = 0
             DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J -NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 ICODT_FI(NIN)%P(K) = TMP%ICODT_FI%P(L)
                 ISKEW_FI(NIN)%P(K) = TMP%ISKEW_FI%P(L)
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                 ICODT_FI(NIN)%P(K) = IREM(ISHIFT + 0,L)
                 ISKEW_FI(NIN)%P(K) = IREM(ISHIFT + 1,L)
               ELSEIF(I == 0) THEN
                 NBLOCAL = NBLOCAL+1
               END IF
             END DO
             ISHIFT = ISHIFT + 2
          ENDIF


C
C specific cases ../..
          IF(IGAP==1 .OR. IGAP==2)THEN
            NBLOCAL = 0
            DO J=1,NN
              I = IAUX(J)
              K = IDEB_NEW+J-NBLOCAL
              IF (I > 0) THEN
                L = IDEB_OLD+I
                GAPFI(NIN)%P(K) = TMP%GAPFI%P(L)
              ELSEIF( I < 0 ) THEN
                L = INDX_FOR_P(P)%P(-I)
                GAPFI(NIN)%P(K) = XREM(RSHIFT,L)
              ELSEIF(I == 0) THEN
                NBLOCAL = NBLOCAL+1
              END IF
            END DO
            RSHIFT = RSHIFT + 1    
          ELSEIF(IGAP==3)THEN 
            NBLOCAL = 0
            DO J = 1, NN
              I = IAUX(J)
              K = IDEB_NEW+J - NBLOCAL
              IF (I > 0) THEN
                L = IDEB_OLD+I
                  GAPFI(NIN)%P(K) =   TMP%GAPFI%P(L) 
                GAP_LFI(NIN)%P(K) = TMP%GAP_LFI%P(L) 
              ELSEIF( I < 0 ) THEN
                L = INDX_FOR_P(P)%P(-I)
                  GAPFI(NIN)%P(K) = XREM(RSHIFT  ,L) 
                GAP_LFI(NIN)%P(K) = XREM(RSHIFT+1,L) 
              ELSEIF(I == 0) THEN
                NBLOCAL = NBLOCAL+1
              END IF
            END DO
            RSHIFT = RSHIFT + 2
          ENDIF
C

C thermic
          IF(INTTH>0)THEN
             NBLOCAL = 0
             DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J -NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                  TEMPFI(NIN)%P(K) =  TMP%TEMPFI%P(L) 
                  AREASFI(NIN)%P(K) = TMP%AREASFI%P(L) 
                  MATSFI(NIN)%P(K) =  TMP%MATSFI%P(L) 
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                  TEMPFI(NIN)%P(K) =  XREM(RSHIFT  ,L) 
                  AREASFI(NIN)%P(K) =  XREM(RSHIFT+1,L)                      
                  MATSFI(NIN)%P(K) =  IREM(ISHIFT,  L) 
               ELSEIF(I == 0) THEN
                 NBLOCAL = NBLOCAL+1
               END IF
             END DO
             RSHIFT = RSHIFT + 2
             ISHIFT = ISHIFT + 1
          ENDIF
          
C Adhesion
          IF(IVIS2==-1.AND.INTTH==0)THEN
             NBLOCAL = 0
             DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J -NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 AREASFI(NIN)%P(K) = TMP%AREASFI%P(L) 
                 IF_ADHFI(NIN)%P(K) =  TMP%IF_ADHFI%P(L)
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                 AREASFI(NIN)%P(K) =  XREM(RSHIFT,L)
                 IF_ADHFI(NIN)%P(K) =  IREM(ISHIFT,L)                
               ELSEIF(I == 0) THEN                 
                 K = IAUX_K(J) 
                 L = IAUX_L(J) 
                 INTBUF_TAB%AREAS(K)   = XREM(RSHIFT,L)
                 INTBUF_TAB%IF_ADH(K)  = IREM(ISHIFT,L)
                 NBLOCAL = NBLOCAL+1
               END IF
             END DO
             RSHIFT = RSHIFT + 1
             ISHIFT = ISHIFT + 1
          ENDIF
              
C Friction      
          IF(INTFRIC > 0 ) THEN    
             NBLOCAL = 0
             DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J -NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 IPARTFRICSFI(NIN)%P(K) = TMP%IPARTFRICSFI%P(L) 
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                  IPARTFRICSFI(NIN)%P(K) =  IREM(ISHIFT,  L) 
               ELSEIF(I == 0) THEN
                 NBLOCAL = NBLOCAL+1
               END IF
             END DO
             ISHIFT = ISHIFT + 1
          ENDIF
C
          IF(IDTMINS==2)THEN
             NBLOCAL  = 0
             DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J-NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 NODNXFI(NIN)%P(K) =  TMP%NODNXFI%P(L) 
                 NODAMSFI(NIN)%P(K) =  TMP%NODAMSFI%P(L) 
                 PROCAMSFI(NIN)%P(K) = TMP%PROCAMSFI%P(L)  
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                 NODNXFI(NIN)%P(K)  = IREM(ISHIFT  ,  L) 
                 NODAMSFI(NIN)%P(K) = IREM(ISHIFT+1,  L) 
                 PROCAMSFI(NIN)%P(K) =IREM(3,L) 
               ELSEIF(I == 0) THEN
                 NBLOCAL = NBLOCAL+1
               END IF
             END DO
             ISHIFT = ISHIFT + 2
        
          ELSEIF(IDTMINS_INT/=0)THEN
             NBLOCAL = 0
             DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J-NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 NODAMSFI(NIN)%P(K) =  TMP%NODAMSFI%P(L) 
                 PROCAMSFI(NIN)%P(K) = TMP%PROCAMSFI%P(L)  
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                 NODAMSFI(NIN)%P(K) = IREM(ISHIFT+1,  L) 
                 PROCAMSFI(NIN)%P(K) =IREM(3,L) 
               ELSEIF(I == 0) THEN
                 NBLOCAL = NBLOCAL+1
               END IF
             END DO
            ISHIFT = ISHIFT + 1
          ENDIF

C
C         IF(ITYP==25)THEN
            NBLOCAL = 0
            DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J-NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 TIME_SFI(NIN)%P(2*(K-1)+1) = TMP%TIME_SFI%P(2*(L-1)+1)    
                 TIME_SFI(NIN)%P(2*(K-1)+2) = TMP%TIME_SFI%P(2*(L-1)+2)    
                 SECND_FRFI(NIN)%P(4,K) = TMP%SECND_FRFI%P(4,L) 
                 SECND_FRFI(NIN)%P(5,K) = TMP%SECND_FRFI%P(5,L) 
                 SECND_FRFI(NIN)%P(6,K) = TMP%SECND_FRFI%P(6,L) 
                 PENE_OLDFI(NIN)%P(2,K)= TMP%PENE_OLDFI%P(2,L)
                 STIF_OLDFI(NIN)%P(2,K)= TMP%STIF_OLDFI%P(2,L) 
                 PENE_OLDFI(NIN)%P(3,K)= TMP%PENE_OLDFI%P(3,L)
                 PENE_OLDFI(NIN)%P(4,K)= TMP%PENE_OLDFI%P(4,L)
                 PENE_OLDFI(NIN)%P(5,K)= TMP%PENE_OLDFI%P(5,L)
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                 TIME_SFI(NIN)%P(2*(K-1)+1) = XREM(RSHIFT+0,L)
                 TIME_SFI(NIN)%P(2*(K-1)+2) = XREM(RSHIFT+1,L)
                 SECND_FRFI(NIN)%P(4,K)      = XREM(RSHIFT+2,L)
                 SECND_FRFI(NIN)%P(5,K)      = XREM(RSHIFT+3,L)
                 SECND_FRFI(NIN)%P(6,K)      = XREM(RSHIFT+4,L)
                 PENE_OLDFI(NIN)%P(2,K)     = XREM(RSHIFT+5,L)
                 STIF_OLDFI(NIN)%P(2,K)     = XREM(RSHIFT+6,L)
                 PENE_OLDFI(NIN)%P(3,K)     = XREM(RSHIFT+7,L)
                 PENE_OLDFI(NIN)%P(3,K)     = XREM(RSHIFT+8,L)
                 PENE_OLDFI(NIN)%P(5,K)     = XREM(RSHIFT+9,L)
               ELSEIF(I == 0) THEN
                 K = IAUX_K(J) 
                 L = IAUX_L(J) 
                 INTBUF_TAB%TIME_S(2*(K-1)+1)   = XREM(RSHIFT+0,L)
                 INTBUF_TAB%TIME_S(2*(K-1)+2)   = XREM(RSHIFT+1,L)
                 INTBUF_TAB%SECND_FR(6*(K-1)+4)  = XREM(RSHIFT+2,L)
                 INTBUF_TAB%SECND_FR(6*(K-1)+5)  = XREM(RSHIFT+3,L)
                 INTBUF_TAB%SECND_FR(6*(K-1)+6)  = XREM(RSHIFT+4,L)
                 INTBUF_TAB%PENE_OLD(5*(K-1)+2) = XREM(RSHIFT+5,L)
                 INTBUF_TAB%STIF_OLD(2*(K-1)+2) = XREM(RSHIFT+6,L)
                 INTBUF_TAB%PENE_OLD(5*(K-1)+3) = XREM(RSHIFT+7,L)
                 INTBUF_TAB%PENE_OLD(5*(K-1)+4) = XREM(RSHIFT+8,L)
                 INTBUF_TAB%PENE_OLD(5*(K-1)+5) = XREM(RSHIFT+9,L)
                 NBLOCAL = NBLOCAL+1
              END IF
            END DO          
            RSHIFT = RSHIFT + 10 

            NBLOCAL = 0  
            DO J = 1, NN
               I = IAUX(J)
               K = IDEB_NEW+J-NBLOCAL
               IF (I > 0) THEN
                 L = IDEB_OLD+I
                 IRTLM_FI(NIN)%P(1,K) = TMP%IRTLM_FI%P(1,L) 
                 IRTLM_FI(NIN)%P(2,K) = TMP%IRTLM_FI%P(2,L) 
                 IRTLM_FI(NIN)%P(3,K) = TMP%IRTLM_FI%P(3,L) 
                 IRTLM_FI(NIN)%P(4,K) = TMP%IRTLM_FI%P(4,L) 
                 ICONT_I_FI(NIN)%P(K) = TMP%ICONT_I_FI%P(L) 
               ELSEIF( I < 0 ) THEN
                 L = INDX_FOR_P(P)%P(-I)
                 IRTLM_FI(NIN)%P(1,K) = IREM(ISHIFT+0,L)
                 IRTLM_FI(NIN)%P(2,K) = IREM(ISHIFT+1,L)
                 IRTLM_FI(NIN)%P(3,K) = IREM(ISHIFT+2,L)
                 IRTLM_FI(NIN)%P(4,K) = IREM(ISHIFT+3,L)
                 ICONT_I_FI(NIN)%P(K) = IREM(ISHIFT+4,L)
               ELSEIF(I == 0) THEN
                 K = IAUX_K(J) 
                 L = IAUX_L(J) 
                 INTBUF_TAB%IRTLM(4*(K-1)+1) = IREM(ISHIFT+0,L)
                 INTBUF_TAB%IRTLM(4*(K-1)+2) = IREM(ISHIFT+1,L)
                 INTBUF_TAB%IRTLM(4*(K-1)+3) = IREM(ISHIFT+2,L)
                 INTBUF_TAB%IRTLM(4*(K-1)+4) = IREM(ISHIFT+3,L)
                 INTBUF_TAB%ICONT_I(K)       = IREM(ISHIFT+4,L)  
                 NBLOCAL = NBLOCAL+1
               END IF
            END DO          
            ISHIFT = ISHIFT + 5
 
            IF (ILEV==2) THEN  
C          NBINFLFI local ne semble pas toujours alloue 
               NBLOCAL = 0
               DO J = 1, NN
                 I = IAUX(J)
                 K = IDEB_NEW+J-NBLOCAL
                 IF (I > 0) THEN
                   L = IDEB_OLD+I
!                  NBINFLFI(NIN)%P(K) = TMP%NBINFLFI%P(L)
                 ELSEIF( I < 0 ) THEN
                   L = INDX_FOR_P(P)%P(-I)
                   NBINFLFI(NIN)%P(K) = IREM(ISHIFT ,L)
                 ELSEIF(I == 0) THEN
                   NBLOCAL = NBLOCAL+1
                 END IF
               END DO          
             ISHIFT = ISHIFT + 1
            END IF

            NBLOCAL = 0
            DO J = 1, NN
              I = IAUX(J)
              K = IDEB_NEW+J-NBLOCAL
              IF (I > 0) THEN
C old node
                L = IDEB_OLD+I
                ISLIDE_FI(NIN)%P(1,K) = TMP%ISLIDE_FI%P(1,L)       
                ISLIDE_FI(NIN)%P(2,K) = TMP%ISLIDE_FI%P(2,L)                   
                ISLIDE_FI(NIN)%P(3,K) = TMP%ISLIDE_FI%P(3,L)                   
                ISLIDE_FI(NIN)%P(4,K) = TMP%ISLIDE_FI%P(4,L)                   
              ELSEIF( I < 0 ) THEN
C new remote node
                L = INDX_FOR_P(P)%P(-I)
                PROC_FROM = PROC_ORIG(L)
                DO JJ = 1,4
                  IF( IREM(ISHIFT-1+JJ,L) >0 ) THEN 
                    ISLIDE_FI(NIN)%P(JJ,K) = FR_NOR( IREM(ISHIFT-1+JJ,L) + IAD_FRNOR(NI25,PROC_FROM) - 1)                                     
                  ELSE
                    ISLIDE_FI(NIN)%P(JJ,K) = 0 
                  ENDIF
                ENDDO
              ELSEIF(I == 0) THEN
C new node,  knonw locally
                 K = IAUX_K(J) 
                 L = IAUX_L(J) 
                 PROC_FROM = PROC_ORIG(L)
                 DO JJ = 1,4
                   IF( IREM(ISHIFT-1+JJ,L) >0 ) THEN 
                     INTBUF_TAB%ISLIDE(4*(K-1)+JJ) = FR_NOR( IREM(ISHIFT-1+JJ,L) + IAD_FRNOR(NI25,PROC_FROM) - 1)                                     
                   ELSE
                     INTBUF_TAB%ISLIDE(4*(K-1)+JJ) = 0 
                   ENDIF
                 ENDDO
                 NBLOCAL = NBLOCAL+1
              END IF
            END DO          
            ISHIFT = ISHIFT + 4

C         ENDIF ! (ITYP==25)

C REMOVE main SEGMENTS : no reception but reconstruction of the tab REMNOR_FI
          IF(FLAGREMN==2)THEN
             NBLOCAL = 0  
             DO J = 1, NN
                I = IAUX(J)
                K = IDEB_NEW+J-NBLOCAL
                IF (I > 0) THEN
                    L = IDEB_OLD+I
                    SIZ = TMP%KREMNOR_FI%P(L+1)- TMP%KREMNOR_FI%P(L) 
                    KREMNOR_FI(NIN)%P(K)=KREMNOR_FI(NIN)%P(K)+ SIZ
c                    KK = TMP%KREMNOR_FI%P(L)+1
                    DO KM=TMP%KREMNOR_FI%P(L)+1,TMP%KREMNOR_FI%P(L+1)
                       KI = KI +1
                       REMNOR_FI_TMP(KI) = TMP%REMNOR_FI%P(KM)
                    ENDDO
                ELSEIF( I < 0 ) THEN
                    DO NE=1,NRTM
                       KK = INTBUF_TAB%KREMNODE(2*(NE-1)+2) + 1
                       LL = INTBUF_TAB%KREMNODE(2*(NE-1)+3)  
                       DO KM=KK,LL
                         IF(INTBUF_TAB%REMNODE(KM) == -ITAFI(NIN)%P(K) ) THEN
                            KREMNOR_FI(NIN)%P(K)=KREMNOR_FI(NIN)%P(K)+1
                            KI = KI+1
                            REMNOR_FI_TMP(KI) = NE
                         ENDIF
                       ENDDO
                     ENDDO
                ELSEIF(I == 0) THEN
                    NBLOCAL = NBLOCAL+1
                END IF

              END DO       
C   
          ENDIF



!         IDEB_NEW = IDEB_NEW + NN                          
          IDEB_NEW = IDEB_NEW +  NSNFI(NIN)%P(P) 


          IDEB_OLD = IDEB_OLD + NSNR_OLD    
          IDEB_SLID = IDEB_SLID + NB_SLID(P)
          DEALLOCATE(IAUX,IAUX_LOCAL,IAUX_REV,IAUX_K,IAUX_L)


        ENDIF !IF(NSNR_OLD/=0) 
      ENDDO  ! end do NSPMD        	

c ===============================================================
c                           TESTS
c ==============================================================
!      DO II = 1,NSPMD 
!        IF(II == LOC_PROC) THEN
!          IF(NSNR /= IDEB_NEW) WRITE(6,*) __FILE__,"NSNR = ",NSNR," --> ", IDEB_NEW
!          J = 1
!          DO P = 1,NSPMD
!            WRITE(6,*) ispmd+1, "P = ",P
!            WRITE(6,*) "NSNFI = ",NSNFI(NIN)%P(P)
!            DO I = 1,NSNFI(NIN)%P(P) 
!               WRITE(6,*) I,NSVFI(NIN)%P(J),ITAFI(NIN)%P(J), "on ",PMAINFI(NIN)%P(J)
!               J = J + 1
!            ENDDO
!          ENDDO
!        ENDIF
!        CALL FLUSH(6)
!        CALL MPI_BARRIER(MPI_COMM_WORLD,IDEB)
!      ENDDO
c ==============================================================

      IF(FLAGREMN == 2 ) THEN
        DO N=1,NODFI
           KREMNOR_FI(NIN)%P(N+1) = KREMNOR_FI(NIN)%P(N+1) + KREMNOR_FI(NIN)%P(N)
        END DO
C
        DO N=NODFI,1,-1
           KREMNOR_FI(NIN)%P(N+1)=KREMNOR_FI(NIN)%P(N)
         END DO
         KREMNOR_FI(NIN)%P(1)=0  

         SIZREMNORFI = KREMNOR_FI(NIN)%P(NODFI+1)
         MY_ALLOCATE(REMNOR_FI(NIN)%P,SIZREMNORFI)
         IF(SIZREMNORFI > 0) THEN
            DO N=1,SIZREMNORFI
               REMNOR_FI(NIN)%P(N) =REMNOR_FI_TMP(N)
            ENDDO
         ENDIF
         DEALLOCATE(REMNOR_FI_TMP)
      ENDIF



C  ===============================================================
      LSKYFI = IDEB_NEW*MULTIMAX
      NSNR = IDEB_NEW
C ================================================================



C
C Deallocation de XREM IREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)
  
C
C   ALLOCATIONS FOR THE ASSEMBLY  
C
      CALL REALLOCATE_FI2(NIN, INTTH, NODFI, LSKYFI, H3D_DATA, IVIS2) 
C
C      ! DEALLOCATE old structures
C
      CALL DEALLOCATE_FI1_TMP(NODFI,TMP,NIN,INTTH,IGAP,ILEV,INTFRIC,FLAGREMN,IVIS2)
C
      DO I = 1, INTBUF_TAB%I_STOK(1)
        N = INTBUF_TAB%CAND_N(I)
        NN = N-NSN
        IF(NN>0)THEN
          INTBUF_TAB%CAND_N(I) = ABS(INDEX(NN))+NSN
        ENDIF
      ENDDO
C
      DO I = 1, INTBUF_TAB%I_STOK(2)
        N = INTBUF_TAB%CAND_OPT_N(I)
        NN = N-NSN
        IF(NN>0)THEN
          INTBUF_TAB%CAND_OPT_N(I) = ABS(INDEX(NN))+NSN
        ENDIF
      ENDDO

      DO P=1,NSPMD
       DEALLOCATE(INDX_FOR_P(P)%P)
       DEALLOCATE(UID_FOR_P(P)%P)
      ENDDO

      DEALLOCATE(PERMUTATIONS,PROC_ORIG)

C
#endif
      RETURN
      END

Chd|====================================================================
Chd|  REALLOCATE_FI1                source/mpi/interfaces/spmd_i25slide.F
Chd|-- called by -----------
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25TMP                      share/modules/tri25tmp_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE REALLOCATE_FI1(NEW_SIZE,TMP,NIN,INTTH,IGAP,ILEV,INTFRIC,
     .                          FLAGREMN,IVIS2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25TMP
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "assert.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                             
C-----------------------------------------------
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NEW_SIZE,NIN,INTTH,IGAP,ILEV,INTFRIC,FLAGREMN,
     .                        IVIS2
      TYPE(I25_TMP_STRUCT) TMP
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------

      TMP%NSNFI%P=>NSNFI(NIN)%P
      TMP%NSVFI%P=>NSVFI(NIN)%P
      TMP%PMAINFI%P=>PMAINFI(NIN)%P
      TMP%XFI%P=>XFI(NIN)%P
      TMP%VFI%P=>VFI(NIN)%P
      TMP%MSFI%P=>MSFI(NIN)%P
      TMP%STIFI%P=>STIFI(NIN)%P
      TMP%ITAFI%P=>ITAFI(NIN)%P
      TMP%KINFI%P=>KINFI(NIN)%P


      IF(.TRUE.) THEN
       TMP%ICODT_FI%P=>ICODT_FI(NIN)%P
       TMP%ISKEW_FI%P=>ISKEW_FI(NIN)%P
      ENDIF
      IF(INTTH > 0 ) THEN
       TMP%TEMPFI%P=>TEMPFI(NIN)%P
       TMP%MATSFI%P=>MATSFI(NIN)%P
       TMP%AREASFI%P=>AREASFI(NIN)%P
      ENDIF 
      IF(INTTH==0.AND.IVIS2==-1) THEN
        TMP%AREASFI%P=>AREASFI(NIN)%P
        TMP%IF_ADHFI%P=>IF_ADHFI(NIN)%P
      ENDIF
      IF(INTFRIC > 0 ) THEN
       TMP%IPARTFRICSFI%P=>IPARTFRICSFI(NIN)%P
      ENDIF 
      IF(IDTMINS == 2) THEN
       TMP%NODNXFI%P=>NODNXFI(NIN)%P
       TMP%NODAMSFI%P=>NODAMSFI(NIN)%P
       TMP%PROCAMSFI%P=>PROCAMSFI(NIN)%P
      ELSEIF(IDTMINS_INT /= 0) THEN
       TMP%NODAMSFI%P=>NODAMSFI(NIN)%P
       TMP%PROCAMSFI%P=>PROCAMSFI(NIN)%P
      ENDIF 
      IF(IGAP/=0) THEN
        TMP%GAPFI%P=>GAPFI(NIN)%P
        IF(IGAP==3) THEN
          TMP%GAP_LFI%P=>GAP_LFI(NIN)%P
        ENDIF
      ENDIF


      IF(ILEV == 2) THEN
        TMP%NBINFLFI%P=>NBINFLFI(NIN)%P
      ENDIF
      TMP%IRTLM_FI%P=>IRTLM_FI(NIN)%P
      TMP%TIME_SFI%P=>TIME_SFI(NIN)%P
      TMP%SECND_FRFI%P=>SECND_FRFI(NIN)%P
      TMP%PENE_OLDFI%P=>PENE_OLDFI(NIN)%P
      TMP%STIF_OLDFI%P=>STIF_OLDFI(NIN)%P
      TMP%ICONT_I_FI%P=>ICONT_I_FI(NIN)%P
      TMP%ISLIDE_FI%P=>ISLIDE_FI(NIN)%P
      IF(FLAGREMN == 2) THEN
        TMP%REMNOR_FI%P=>REMNOR_FI(NIN)%P
        TMP%KREMNOR_FI%P=>KREMNOR_FI(NIN)%P
      ENDIF
C

      ASSERT(NEW_SIZE >= 0)

      nullify(NSNFI(NIN)%P)
      nullify(NSVFI(NIN)%P)
      nullify(XFI(NIN)%P)
      nullify(VFI(NIN)%P)

C     WRITE(6,*) "NEW SIZE:",NEW_SIZE

      MY_ALLOCATE(NSNFI(NIN)%P,NSPMD)
      MY_ALLOCATE(NSVFI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE(MSFI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE(STIFI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE(ITAFI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE(PMAINFI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE(KINFI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE2D(VFI(NIN)%P,3,NEW_SIZE)
      MY_ALLOCATE2D(XFI(NIN)%P,3,NEW_SIZE)
      IF(.TRUE.) THEN
       MY_ALLOCATE(ICODT_FI(NIN)%P,NEW_SIZE)
       MY_ALLOCATE(ISKEW_FI(NIN)%P,NEW_SIZE)
      ENDIF
      IF(INTTH > 0 ) THEN
       MY_ALLOCATE(TEMPFI(NIN)%P,NEW_SIZE)
       MY_ALLOCATE(MATSFI(NIN)%P,NEW_SIZE)
       MY_ALLOCATE(AREASFI(NIN)%P,NEW_SIZE)
      ENDIF 
      IF(INTTH==0.AND.IVIS2==-1) THEN
        MY_ALLOCATE(AREASFI(NIN)%P,NEW_SIZE)
        MY_ALLOCATE(IF_ADHFI(NIN)%P,NEW_SIZE)
      ENDIF
      IF(INTFRIC > 0 ) THEN
       MY_ALLOCATE(IPARTFRICSFI(NIN)%P,NEW_SIZE)
      ENDIF 
      IF(IDTMINS == 2) THEN
       MY_ALLOCATE(NODNXFI(NIN)%P,NEW_SIZE)
       MY_ALLOCATE(NODAMSFI(NIN)%P,NEW_SIZE)
       MY_ALLOCATE(PROCAMSFI(NIN)%P,NEW_SIZE)
      ELSEIF(IDTMINS_INT /= 0) THEN
       MY_ALLOCATE(NODAMSFI(NIN)%P,NEW_SIZE)
       MY_ALLOCATE(PROCAMSFI(NIN)%P,NEW_SIZE)
      ENDIF 
      IF(IGAP/=0) THEN
        MY_ALLOCATE(GAPFI(NIN)%P,NEW_SIZE)
        IF(IGAP==3) THEN
          MY_ALLOCATE(GAP_LFI(NIN)%P,NEW_SIZE)
        ENDIF
      ENDIF
      IF(ILEV == 2 ) THEN 
        MY_ALLOCATE(NBINFLFI(NIN)%P,NEW_SIZE)
      ENDIF
      MY_ALLOCATE2D(IRTLM_FI(NIN)%P,4,NEW_SIZE)
      MY_ALLOCATE(TIME_SFI(NIN)%P,2*NEW_SIZE)
      MY_ALLOCATE2D(SECND_FRFI(NIN)%P,6,NEW_SIZE)
      SECND_FRFI(NIN)%P (1:6,1:NEW_SIZE)=ZERO
      MY_ALLOCATE2D(PENE_OLDFI(NIN)%P,5,NEW_SIZE)
      PENE_OLDFI(NIN)%P(1:5,1:NEW_SIZE)=ZERO
      MY_ALLOCATE2D(STIF_OLDFI(NIN)%P,2,NEW_SIZE)
      STIF_OLDFI(NIN)%P(1:2,1:NEW_SIZE)=ZERO
      MY_ALLOCATE(ICONT_I_FI(NIN)%P,NEW_SIZE)
      MY_ALLOCATE2D(ISLIDE_FI(NIN)%P,4,NEW_SIZE)
      IF(FLAGREMN == 2) THEN       
       MY_ALLOCATE(KREMNOR_FI(NIN)%P,NEW_SIZE+1)
       KREMNOR_FI(NIN)%P(1:NEW_SIZE+1) = 0
      ENDIF
C
C

      RETURN
      END SUBROUTINE    
Chd|====================================================================
Chd|  DEALLOCATE_FI1_TMP            source/mpi/interfaces/spmd_i25slide.F
Chd|-- called by -----------
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25TMP                      share/modules/tri25tmp_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE DEALLOCATE_FI1_TMP(NEW_SIZE,TMP,NIN,INTTH,IGAP,ILEV,INTFRIC,FLAGREMN,IVIS2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25TMP
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                             
C-----------------------------------------------
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NEW_SIZE,NIN,INTTH,IGAP,ILEV,INTFRIC,FLAGREMN,IVIS2
      TYPE(I25_TMP_STRUCT) TMP
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------

      IF(.NOT.ASSOCIATED(TMP%NSVFI%P))RETURN
      DEALLOCATE(TMP%NSNFI%P)
      DEALLOCATE(TMP%NSVFI%P)
      DEALLOCATE(TMP%XFI%P)
      DEALLOCATE(TMP%VFI%P)
      DEALLOCATE(TMP%MSFI%P)
      DEALLOCATE(TMP%STIFI%P)
      DEALLOCATE(TMP%ITAFI%P)
      DEALLOCATE(TMP%PMAINFI%P)
      DEALLOCATE(TMP%KINFI%P)
      IF(.TRUE.) THEN
        DEALLOCATE(TMP%ICODT_FI%P)
        DEALLOCATE(TMP%ISKEW_FI%P)
      ENDIF
      IF(INTTH > 0 ) THEN
       DEALLOCATE(TMP%TEMPFI%P)
       DEALLOCATE(TMP%MATSFI%P)
       DEALLOCATE(TMP%AREASFI%P)
      ENDIF 
      IF(INTTH==0.AND.IVIS2==-1) THEN
        DEALLOCATE(TMP%AREASFI%P)
        DEALLOCATE(TMP%IF_ADHFI%P)
      ENDIF
      IF(INTFRIC > 0 ) THEN
       DEALLOCATE(TMP%IPARTFRICSFI%P)
      ENDIF 
      IF(IDTMINS == 2) THEN
       DEALLOCATE(TMP%NODNXFI%P)
       DEALLOCATE(TMP%NODAMSFI%P)
       DEALLOCATE(TMP%PROCAMSFI%P)
      ELSEIF(IDTMINS_INT /= 0) THEN
       DEALLOCATE(TMP%NODAMSFI%P)
       DEALLOCATE(TMP%PROCAMSFI%P)
      ENDIF 
      IF(IGAP/=0) THEN
        DEALLOCATE(TMP%GAPFI%P)
        IF(IGAP==3) THEN
          DEALLOCATE(TMP%GAP_LFI%P)
        ENDIF
      ENDIF
      IF(ILEV == 2 .AND. ASSOCIATED(TMP%NBINFLFI%P)) THEN
        DEALLOCATE(TMP%NBINFLFI%P)
      ENDIF
      DEALLOCATE(TMP%IRTLM_FI%P)
      DEALLOCATE(TMP%TIME_SFI%P)
      DEALLOCATE(TMP%SECND_FRFI%P)

      DEALLOCATE(TMP%PENE_OLDFI%P)

      DEALLOCATE(TMP%STIF_OLDFI%P)

      DEALLOCATE(TMP%ICONT_I_FI%P)
      DEALLOCATE(TMP%ISLIDE_FI%P)
      IF(FLAGREMN == 2) THEN
         IF(ASSOCIATED(TMP%REMNOR_FI%P)) DEALLOCATE(TMP%REMNOR_FI%P)
         IF(ASSOCIATED(TMP%KREMNOR_FI%P)) DEALLOCATE(TMP%KREMNOR_FI%P)
      ENDIF
C

      RETURN
      END SUBROUTINE    


Chd|====================================================================
Chd|  REALLOCATE_FI2                source/mpi/interfaces/spmd_i25slide.F
Chd|-- called by -----------
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE REALLOCATE_FI2(NIN, INTTH, NODFI, LSKYFI,H3D_DATA,IVIS2) 
C
C                               
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
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
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NIN, INTTH, NODFI, LSKYFI, IVIS2
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IERROR1,IERROR2,IERROR3,IERROR4

      IERROR1 = 0
      IERROR2 = 0
      IERROR3 = 0
      IERROR4 = 0

      IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
        IF(IPARIT==0) THEN
         
          IF(ASSOCIATED(AFI(NIN)%P)) THEN
              DEALLOCATE(AFI(NIN)%P)
              NULLIFY(AFI(NIN)%P)
          ENDIF
          IF(ASSOCIATED(STNFI(NIN)%P)) THEN
              DEALLOCATE(STNFI(NIN)%P)
              NULLIFY(AFI(NIN)%P) 
          ENDIF

          IF(NODFI>0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI>0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
          ENDDO
C
          IF(KDTINT/=0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI>0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
            DO I = 1, NODFI*NTHREAD
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
          NLSKYFI(NIN) = NODFI
C
        ELSE
C
C Allocation Parith/ON
C
          IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
          IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
          NLSKYFI(NIN) = LSKYFI
          IF(LSKYFI>0) THEN
            ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
            IF(KDTINT==0) THEN
              ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
            ELSE
              ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
            ENDIF
          ENDIF
        ENDIF
      ELSE
C
C Allocation Parith/OFF
C
        IF(IPARIT==0) THEN
          IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
          IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
          IF(ASSOCIATED(FTHEFI(NIN)%P)) DEALLOCATE(FTHEFI(NIN)%P)
          IF(NODFI>0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI>0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
          IF(NODFI>0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
            IF(NODFI>0.AND.NODADT_THERM ==1)ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR4)
          ENDIF
C

        
C Init a 0

          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
            FTHEFI(NIN)%P(I) = ZERO
          ENDDO
          IF(NODADT_THERM ==1) THEN
            DO I = 1, NODFI
               CONDNFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
          IF(KDTINT/=0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI>0)ALLOCATE(VSCFI(NIN)%P(NODFI),STAT=IERROR4)
C Init a 0
            DO I = 1, NODFI
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
        ELSE
C
C Allocation Parith/ON
C
          IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
          IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
          IF(ASSOCIATED(FTHESKYFI(NIN)%P)) DEALLOCATE(FTHESKYFI(NIN)%P)
          NLSKYFI(NIN) = LSKYFI
          IF(LSKYFI>0) THEN
            ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
            IF(KDTINT==0) THEN
              ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
              ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
            ELSE
              ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
              ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
            ENDIF

          ENDIF
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNSKYFI(NIN)%P)) DEALLOCATE(CONDNSKYFI(NIN)%P)
            IF(LSKYFI>0) ALLOCATE(CONDNSKYFI(NIN)%P(LSKYFI),STAT=IERROR4)
          ENDIF
C

        ENDIF        
      ENDIF    
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C allocations conditionnelles output pression
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0)THEN
        IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
        IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
        ALLOCATE(FNCONTI(NIN)%P(3,NODFI),STAT=IERROR1)
        ALLOCATE(FTCONTI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(IERROR1+IERROR2/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ELSE
          DO J = 1, NODFI
            FNCONTI(NIN)%P(1,J)=ZERO
            FNCONTI(NIN)%P(2,J)=ZERO
            FNCONTI(NIN)%P(3,J)=ZERO
            FTCONTI(NIN)%P(1,J)=ZERO
            FTCONTI(NIN)%P(2,J)=ZERO
            FTCONTI(NIN)%P(3,J)=ZERO
          END DO                  
        END IF          
      END IF
C

      RETURN
      END SUBROUTINE

Chd|====================================================================
Chd|  CHECK_FI                      source/mpi/interfaces/spmd_i25slide.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25TMP                      share/modules/tri25tmp_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE CHECK_FI()
C use for debug only
C
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25TMP
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                             
C-----------------------------------------------
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------

       IF(ISPMD == 1) THEN
      !   IF(NSNR > 0) THEN
           IF(ABS(NSVFI(1)%P(2)) ==  5864) THEN 
             WRITE(6,*) __FILE__,__LINE__
           ENDIF
      !   ENDIF
       ENDIF 
       RETURN  
       END SUBROUTINE
