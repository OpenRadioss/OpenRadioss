Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
C Parith/On SPMD finalization (1st part carried out in Starter)
!||====================================================================
!||    assadd2          ../engine/source/assembly/assadd2.F
!||--- called by ------------------------------------------------------
!||    resol_init       ../engine/source/engine/resol_init.F
!||--- calls      -----------------------------------------------------
!||    my_orders        ../common_source/tools/sort/my_orders.c
!||--- uses       -----------------------------------------------------
!||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    my_alloc_mod     ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE ASSADD2(ADDCNE  ,INDSKY  ,FSKY  ,FSKYM    ,IAD_ELEM ,
     2             FR_ELEM ,FR_NBCC ,PROCNE,NISKYFI  ,ADDCNI2  ,        
     3             PROCNI2 ,IAD_I2M ,FR_I2M,FR_NBCCI2,INDSKYI2 ,        
     4             IADSDP  ,IADRCP  ,ISENDP,IRECVP   ,FTHESKY  ,        
     5             NISKYFIE,INOD_PXFEM,ADDCNE_PXFEM,PROCNE_PXFEM,
     6             ISENDP_PXFEM,IRECVP_PXFEM ,IADSDP_PXFEM,IADRCP_PXFEM,
     7             FR_NBCC1 ,INOD_CRKXFEM,ADDCNE_CRKXFEM,PROCNE_CRKXFEM,
     8     ISENDP_CRKXFEM,IRECVP_CRKXFEM,IADSDP_CRKXFEM,IADRCP_CRKXFEM,
     9             CONDNSKY,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      use glob_therm_mod
      use my_alloc_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "scr18_c.inc"
      INTEGER MAXBLOC,NBLOC,NBVAL,NBCOL
      PARAMETER (MAXBLOC=1000)
      COMMON/PTMPARIT/NBLOC,NBVAL(1:MAXBLOC),NBCOL(1:MAXBLOC)
      INTEGER MAXBLOCI2,NBLOCI2,NBVALI2,NBCOLI2
      PARAMETER (MAXBLOCI2=1000)
      COMMON/PTMPARI2/NBLOCI2,NBVALI2(1:MAXBLOCI2),NBCOLI2(1:MAXBLOCI2)
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INDSKY(*),ADDCNE(*),IAD_ELEM(2,*),FR_NBCC(2,*),
     .        PROCNE(*),FR_ELEM(*), NISKYFI(*), ADDCNI2(*),NISKYFIE(*),
     .        PROCNI2(*), IAD_I2M(*), FR_I2M(*), FR_NBCCI2(2,*),
     .        INDSKYI2(*), IADSDP(*), IADRCP(*), ISENDP(*), IRECVP(*),
     .        PROCNE_PXFEM(*),ISENDP_PXFEM(*),IRECVP_PXFEM(*),
     .        IADSDP_PXFEM(*),IADRCP_PXFEM(*),FR_NBCC1(2,*),
     .        ADDCNE_PXFEM(*),INOD_PXFEM(*),ADDCNE_CRKXFEM(*),
     .        INOD_CRKXFEM(*),PROCNE_CRKXFEM(*),ISENDP_CRKXFEM(*),
     .        IRECVP_CRKXFEM(*),IADSDP_CRKXFEM(*),IADRCP_CRKXFEM(*)
      my_real 
     .        FSKY(8,LSKY),FSKYM(LSKY),FTHESKY(LSKY),CONDNSKY(LSKY)
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, K, NC1, NC, LOC_PROC, NOD, CC, lsd, lrc, 
     .        WORK(70000), LSD1,LRC1,NOD1,XFEM_REM_COUNT
      INTEGER,DIMENSION(:),ALLOCATABLE :: INDEX
C-----------------------------------------------
      call my_alloc(INDEX, 2*NUMNOD, "INDEX")
C-----------------------------------------------
C   Init a ZERO FORCES NODALES 
C-----------------------------------------------
      NISKY = 0
      DO N = 1, NINTER
        NISKYFI(N) = 0
        NISKYFIE(N) = 0
      ENDDO
      DO K=1,8
        DO I = 1, LSKY
          FSKY(K,I)=ZERO
        ENDDO
      ENDDO
C
C
      IF (GLOB_THERM%ITHERM_FE > 0 ) THEN
        DO I=1,LSKY 
         FTHESKY(I) = ZERO
       ENDDO
      ENDIF
C
      IF (GLOB_THERM%NODADT_THERM > 0 ) THEN
        DO I=1,LSKY 
         CONDNSKY(I) = ZERO
       ENDDO
      ENDIF
C
      IF (N2D == 0 .AND. IALE+IEULER + GLOB_THERM%ITHERM > 0) THEN
        DO I = 1, LSKY
          FSKYM(I)=ZERO
        ENDDO
      ENDIF
C
C SPMD CNE and ADDCNE already obtained
C iadx already computed in RADIOSS Starter
C
C-----------------------------------------------
C Computing elementary communication sizes
C-----------------------------------------------
      DO I = 1, NSPMD+1
        FR_NBCC(1,I) = 0
        FR_NBCC(2,I) = 0
      ENDDO
      IF(IPLYXFEM > 0 ) THEN
        DO I = 1, NSPMD+1
           FR_NBCC1(1,I) = 0
           FR_NBCC1(2,I) = 0
        ENDDO 
      ENDIF
C
      IF(ICRACK3D > 0 .AND. NSPMD > 1)THEN
        DO I = 1, NSPMD+1
           FR_NBCC1(1,I) = 0
           FR_NBCC1(2,I) = 0
        ENDDO 
      ENDIF
C
      LOC_PROC = ISPMD+1
C
      LSD = 1
      LRC = 1  
C
      LSD1 = 1
      LRC1 = 1
      IF(IPLYXFEM == 0 .AND. ICRACK3D == 0) THEN
         DO I = 1, NSPMD
           IADSDP(I)=LSD
           IADRCP(I)=LRC
           IF(I/=LOC_PROC) THEN
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   FR_NBCC(1,I) = FR_NBCC(1,I)+1
                   ISENDP(LSD) = CC
                   LSD = LSD + 1
                 ELSEIF(PROCNE(CC)==I) THEN
                   FR_NBCC(2,I) = FR_NBCC(2,I)+1
                   IRECVP(LRC) = CC
                   LRC = LRC + 1
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ENDDO
C
         IADSDP(NSPMD+1)=LSD
         IADRCP(NSPMD+1)=LRC
      ELSE IF (IPLYXFEM > 0) THEN
        DO I = 1, NSPMD
           IADSDP(I)=LSD
           IADRCP(I)=LRC
C           
           IADSDP_PXFEM(I)=LSD1
           IADRCP_PXFEM(I)=LRC1
           IF(I/=LOC_PROC) THEN
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   FR_NBCC(1,I) = FR_NBCC(1,I)+1
                   ISENDP(LSD) = CC
                   LSD = LSD + 1
                 ELSEIF(PROCNE(CC)==I) THEN
                   FR_NBCC(2,I) = FR_NBCC(2,I)+1
                   IRECVP(LRC) = CC
                   LRC = LRC + 1
                 ENDIF
               ENDDO
               NOD1 = INOD_PXFEM(NOD)
               IF(NOD1 > 0 ) THEN
                   DO CC = ADDCNE_PXFEM(NOD1),ADDCNE_PXFEM(NOD1+1)-1
                    IF(PROCNE_PXFEM(CC)==LOC_PROC) THEN
                      FR_NBCC1(1,I) = FR_NBCC1(1,I)+1
                      ISENDP_PXFEM(LSD1) = CC
                      LSD1 = LSD1 + 1
                    ELSEIF(PROCNE_PXFEM(CC)==I) THEN
                      FR_NBCC1(2,I) = FR_NBCC1(2,I)+1
                      IRECVP_PXFEM(LRC1) = CC
                      LRC1 = LRC1 + 1
                    ENDIF
                  ENDDO  
               ENDIF
             ENDDO
           ENDIF
         ENDDO
         IADSDP(NSPMD+1)=LSD
         IADRCP(NSPMD+1)=LRC      
C      
         IADSDP_PXFEM(NSPMD+1)=LSD1
         IADRCP_PXFEM(NSPMD+1)=LRC1            
c
      ELSE IF (ICRACK3D > 0 .AND. NSPMD > 1) THEN
        DO I = 1, NSPMD
          IADSDP(I)=LSD                                                       
          IADRCP(I)=LRC                                                       
C                                                                             
          IADSDP_CRKXFEM(I)=LSD1                                              
          IADRCP_CRKXFEM(I)=LRC1                                              
          IF (I /= LOC_PROC) THEN                                             
            DO J = IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1                              
              NOD = FR_ELEM(J)                                                
              DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1                             
                IF (PROCNE(CC) == LOC_PROC) THEN                               
                  FR_NBCC(1,I) = FR_NBCC(1,I)+1                               
                  ISENDP(LSD) = CC                                            
                  LSD = LSD + 1                                               
                ELSEIF (PROCNE(CC) == I) THEN                                  
                  FR_NBCC(2,I) = FR_NBCC(2,I)+1                               
                  IRECVP(LRC) = CC                                            
                  LRC = LRC + 1                                               
                ENDIF                                                         
              ENDDO                                                           
c---
              NOD1 = INOD_CRKXFEM(NOD)                                        
              IF (NOD1 > 0 ) THEN                                             
                         XFEM_REM_COUNT = 0                                            
                DO CC = ADDCNE_CRKXFEM(NOD1),ADDCNE_CRKXFEM(NOD1+1)-1         
                  IF (PROCNE_CRKXFEM(CC) == I) XFEM_REM_COUNT =                 
     .                                         XFEM_REM_COUNT+1                 
                        ENDDO                                                         
c
                        IF (XFEM_REM_COUNT /= 0) THEN                                 
                  DO CC = ADDCNE_CRKXFEM(NOD1),ADDCNE_CRKXFEM(NOD1+1)-1       
                    IF (PROCNE_CRKXFEM(CC)==LOC_PROC) THEN                  
                      FR_NBCC1(1,I) = FR_NBCC1(1,I)+1                         
                      ISENDP_CRKXFEM(LSD1) = CC                               
                      LSD1 = LSD1 + 1                                         
                    ELSEIF (PROCNE_CRKXFEM(CC)==I) THEN                     
                      FR_NBCC1(2,I) = FR_NBCC1(2,I)+1                         
                      IRECVP_CRKXFEM(LRC1) = CC                               
                      LRC1 = LRC1 + 1                                         
                    ENDIF                                                     
                  ENDDO                                                       
                        ENDIF                                                         
C                                                                              
              ENDIF                                                           
c---
            ENDDO !  loop over frontier nodes                                                            
          ENDIF   ! I /= LOC_PROC                                                           
        ENDDO     ! I = 1, NSPMD                   
        IADSDP(NSPMD+1)=LSD          
        IADRCP(NSPMD+1)=LRC          
C      
        IADSDP_CRKXFEM(NSPMD+1)=LSD1 
        IADRCP_CRKXFEM(NSPMD+1)=LRC1 
      ENDIF   
C-------------------------------------
      DO I = 1, NSPMD
        FR_NBCC(1,NSPMD+1) = FR_NBCC(1,NSPMD+1) +  FR_NBCC(1,I)
        FR_NBCC(2,NSPMD+1) = FR_NBCC(2,NSPMD+1) +  FR_NBCC(2,I)
      ENDDO
C      
      IF(IPLYXFEM > 0) THEN
          DO I = 1, NSPMD
            FR_NBCC1(1,NSPMD+1) = FR_NBCC1(1,NSPMD+1) +  FR_NBCC1(1,I)
            FR_NBCC1(2,NSPMD+1) = FR_NBCC1(2,NSPMD+1) +  FR_NBCC1(2,I)
          ENDDO
      ENDIF
C
      IF(ICRACK3D > 0 .AND. NSPMD > 1)THEN
          DO I = 1, NSPMD
            FR_NBCC1(1,NSPMD+1) = FR_NBCC1(1,NSPMD+1) +  FR_NBCC1(1,I)
            FR_NBCC1(2,NSPMD+1) = FR_NBCC1(2,NSPMD+1) +  FR_NBCC1(2,I)
          ENDDO
      ENDIF
C-----------------------------------------------
C Vectorial optimization of asspar4
C-----------------------------------------------
      IF (IVECTOR==1) THEN
C   index : 2*numnod
C   indsky : numnod
        DO N = 1, NUMNOD
          INDSKY(N) = ADDCNE(N+1) - ADDCNE(N)
          INDEX(N) = N
        ENDDO
        CALL MY_ORDERS(0,WORK,INDSKY,INDEX,NUMNOD,1)
        DO N = 1, NUMNOD
          INDSKY(N) = INDEX(N)
        ENDDO
C
C block identification
C    
        NC1 = -1
        I = 1
        NBLOC = 0
        DO WHILE (I<=NUMNOD)
          N = INDSKY(I)
          NC = ADDCNE(N+1)-ADDCNE(N)
          IF(NC==NC1) THEN
            NBVAL(NBLOC) = NBVAL(NBLOC)+1
          ELSE
            NC1 = NC
            NBLOC = NBLOC+1
            IF (NBLOC>MAXBLOC) THEN
              WRITE(IOUT,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
              WRITE(ISTDO,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
              TSTOP=ZERO
              RETURN
            ENDIF
            NBVAL(NBLOC) = 1
            NBCOL(NBLOC) = NC
          ENDIF
          I = I+1
        ENDDO
C fin ivector=1
      ENDIF
C
C Partie Interface type 2
C
      DO I = 1, NSPMD+1
        FR_NBCCI2(1,I) = 0
        FR_NBCCI2(2,I) = 0
      ENDDO

      IF(I2NSNT>0) THEN
C-----------------------------------------------
C Computing elementary communication sizes
C-----------------------------------------------
C
        LOC_PROC = ISPMD+1
        DO I = 1, NSPMD
          IF(I/=LOC_PROC) THEN
            DO J=IAD_I2M(I),IAD_I2M(I+1)-1
              NOD = FR_I2M(J)
              DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
                IF(PROCNI2(CC)==LOC_PROC) THEN
                  FR_NBCCI2(1,I) = FR_NBCCI2(1,I)+1
C                ELSE
                ELSEIF(PROCNI2(CC)==I) THEN
                  FR_NBCCI2(2,I) = FR_NBCCI2(2,I)+1
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        DO I = 1, NSPMD
          FR_NBCCI2(1,NSPMD+1) = FR_NBCCI2(1,NSPMD+1)+FR_NBCCI2(1,I)
          FR_NBCCI2(2,NSPMD+1) = FR_NBCCI2(2,NSPMD+1)+FR_NBCCI2(2,I)
        ENDDO
C-----------------------------------------------
C Vectorial optimization of asspari2
C-----------------------------------------------
        IF (IVECTOR==1) THEN
C index : 2*numnod
C indskyi2 : numnod
          DO N = 1, NUMNOD
            INDSKYI2(N) = ADDCNI2(N+1) - ADDCNI2(N)
            INDEX(N) = N
          ENDDO
          CALL MY_ORDERS(0,WORK,INDSKYI2,INDEX,NUMNOD,1)
          DO N = 1, NUMNOD
            INDSKYI2(N) = INDEX(N)
          ENDDO
C
C block identification
C    
          NC1 = -1
          I = 1
          NBLOCI2 = 0
          DO WHILE (I<=NUMNOD)
            N = INDSKYI2(I)
            NC = ADDCNI2(N+1)-ADDCNI2(N)
            IF(NC==NC1) THEN
              NBVALI2(NBLOCI2) = NBVALI2(NBLOCI2)+1
            ELSE
              NC1 = NC
              NBLOCI2 = NBLOCI2+1
              IF (NBLOCI2>MAXBLOC) THEN
                WRITE(IOUT,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
                WRITE(ISTDO,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
                TSTOP=ZERO
                  RETURN
              ENDIF
              NBVALI2(NBLOCI2) = 1
              NBCOLI2(NBLOCI2) = NC
            ENDIF
            I = I+1
          ENDDO
C fin ivector=1
        ENDIF
C fin interface type I2NSNT > 0
      END IF
C
      call my_dealloc(INDEX)
      RETURN
      END




C 
!||====================================================================
!||    assinit              ../engine/source/assembly/assadd2.F
!||--- called by ------------------------------------------------------
!||    resol_alloc_phase2   ../engine/source/engine/resol_alloc.F90
!||====================================================================
      SUBROUTINE ASSINIT(
     1    ADDCNE, IAD_ELEM, FR_ELEM, PROCNE, LISENDP, LIRECVP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE(*),IAD_ELEM(2,*),
     .        PROCNE(*),FR_ELEM(*), LISENDP, LIRECVP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, LOC_PROC, NOD, CC, LSD, LRC
C-----------------------------------------------
C Computing optimized communication sizes parith/on
C-----------------------------------------------
      IF(NSPMD>0.AND.IPARIT>0)THEN
        LOC_PROC = ISPMD+1
C
        LSD = 0
        LRC = 0
        DO I = 1, NSPMD
         IF(I/=LOC_PROC) THEN
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
              IF(PROCNE(CC)==LOC_PROC) THEN
                LSD = LSD + 1
              ELSEIF(PROCNE(CC)==I) THEN
                LRC = LRC + 1
              ENDIF
            ENDDO
          ENDDO
         ENDIF
        ENDDO
        LISENDP = LSD
        LIRECVP = LRC
      ELSE
        LISENDP = 0
        LIRECVP = 0
      END IF
C
      RETURN
      END
 !||====================================================================
!||    rebuild_pon_tables   ../engine/source/assembly/assadd2.F
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||====================================================================
      SUBROUTINE REBUILD_PON_TABLES(
     .    ADDCNE, IAD_ELEM, FR_ELEM, PROCNE,
     .    FR_NBCC, IADSDP, IADRCP, ISENDP, IRECVP,
     .    NUMNOD_OLD, ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE(*), IAD_ELEM(2,*), FR_ELEM(*), PROCNE(*)
      INTEGER FR_NBCC(2,*), IADSDP(*), IADRCP(*), ISENDP(*), IRECVP(*)
      INTEGER NUMNOD_OLD
      INTEGER ITAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NOD, CC, LOC_PROC, LSD, LRC
      INTEGER NNEW, IA, IB, ITMP, KK
      INTEGER, ALLOCATABLE :: NEWLIST(:)
C      LOGICAL DBG57
C-----------------------------------------------
C  Rebuild Parith/ON send/recv index tables after a topology change
C  (node split). Mirrors the basic branch of ASSADD2 without zeroing
C  FSKY — continuity of FSKY values across the split is intentional.
C
C  Two-pass loop: process pre-existing nodes (ID <= NUMNOD_OLD) first,
C  then new nodes (ID > NUMNOD_OLD). This guarantees that the new FSKY
C  slots are appended at the same relative position in ISENDP/IRECVP on
C  EVERY rank, so that SPMD_EXCH2_A_PON pairs them correctly:
C    - On the owner rank the new node appears in the SEND sub-block
C      (after all old entries).
C    - On the ghost rank the new node appears in the RECV sub-block
C      (before the old SEND entries in the merged boundary).
C  Without the two-pass approach both insertion points differ, causing a
C  position mismatch and wrong values being received into the new slot.
C-----------------------------------------------
      IF (NSPMD > 0 .AND. IPARIT > 0) THEN
        LOC_PROC = ISPMD + 1
        DO I = 1, NSPMD+1
          FR_NBCC(1,I) = 0
          FR_NBCC(2,I) = 0
        ENDDO
        LSD = 1
        LRC = 1
        DO I = 1, NSPMD
          IADSDP(I) = LSD
          IADRCP(I) = LRC
          IF (I /= LOC_PROC) THEN
C            DBG57 = (LOC_PROC == 6 .AND. I == 8) .OR.
C     .              (LOC_PROC == 8 .AND. I == 6)
C            IF (DBG57) THEN
C              WRITE(6,'(A,I0,A,I0,A,I0,A,I0)')
C     .          '[EXCH_DBG] PAIR begin r=',ISPMD,' partner=',I-1,
C     .          ' nbnd=',IAD_ELEM(1,I+1)-IAD_ELEM(1,I),
C     .          ' numnod_old=',NUMNOD_OLD
C              CALL FLUSH(6)
C            ENDIF
C           Pass 1: pre-existing nodes (ID <= NUMNOD_OLD) — same order as ASSADD2
            DO J = IAD_ELEM(1,I), IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              IF (NOD > NUMNOD_OLD) CYCLE
              DO CC = ADDCNE(NOD), ADDCNE(NOD+1)-1
                IF (PROCNE(CC) == LOC_PROC) THEN
                  FR_NBCC(1,I) = FR_NBCC(1,I) + 1
                  ISENDP(LSD)  = CC
C                   IF (DBG57) WRITE(6,900) ISPMD,I-1,1,LSD,NOD,
C     .                       ITAB(NOD),CC,PROCNE(CC)
                  LSD = LSD + 1
                ELSEIF (PROCNE(CC) == I) THEN
                  FR_NBCC(2,I) = FR_NBCC(2,I) + 1
                  IRECVP(LRC)  = CC
C                   IF (DBG57) WRITE(6,901) ISPMD,I-1,1,LRC,NOD,
C     .                       ITAB(NOD),CC,PROCNE(CC)
                  LRC = LRC + 1
                ENDIF
              ENDDO
            ENDDO
C           Pass 2: new split nodes (ID > NUMNOD_OLD), enumerated in ascending
C           global-id (ITAB) order rather than boundary order. A split node is
C           bidirectional (owner AND mirror both assemble it) and can sit in
C           opposite recv/send sub-blocks on the two ranks, so boundary order
C           lists them differently on each rank and SPMD_EXCH2_A_PON then pairs
C           the wrong send/recv slots. ITAB is identical on every rank, so
C           sorting by it makes the owner SEND order match the partner RECV order.
            NNEW = 0
            DO J = IAD_ELEM(1,I), IAD_ELEM(1,I+1)-1
              IF (FR_ELEM(J) > NUMNOD_OLD) NNEW = NNEW + 1
            ENDDO
            IF (NNEW > 0) THEN
              ALLOCATE(NEWLIST(NNEW))
              KK = 0
              DO J = IAD_ELEM(1,I), IAD_ELEM(1,I+1)-1
                IF (FR_ELEM(J) > NUMNOD_OLD) THEN
                  KK = KK + 1
                  NEWLIST(KK) = FR_ELEM(J)
                ENDIF
              ENDDO
C             insertion sort NEWLIST by ascending ITAB (global id)
              DO IA = 2, NNEW
                ITMP = NEWLIST(IA)
                IB = IA - 1
                DO WHILE (IB >= 1)
                  IF (ITAB(NEWLIST(IB)) <= ITAB(ITMP)) EXIT
                  NEWLIST(IB+1) = NEWLIST(IB)
                  IB = IB - 1
                ENDDO
                NEWLIST(IB+1) = ITMP
              ENDDO
              DO KK = 1, NNEW
                NOD = NEWLIST(KK)
                DO CC = ADDCNE(NOD), ADDCNE(NOD+1)-1
                  IF (PROCNE(CC) == LOC_PROC) THEN
                    FR_NBCC(1,I) = FR_NBCC(1,I) + 1
                    ISENDP(LSD)  = CC
C                     IF (DBG57) WRITE(6,900) ISPMD,I-1,2,LSD,NOD,
C     .                         ITAB(NOD),CC,PROCNE(CC)
                    LSD = LSD + 1
                  ELSEIF (PROCNE(CC) == I) THEN
                    FR_NBCC(2,I) = FR_NBCC(2,I) + 1
                    IRECVP(LRC)  = CC
C                     IF (DBG57) WRITE(6,901) ISPMD,I-1,2,LRC,NOD,
C     .                         ITAB(NOD),CC,PROCNE(CC)
                    LRC = LRC + 1
                  ENDIF
                ENDDO
              ENDDO
              DEALLOCATE(NEWLIST)
            ENDIF
C            IF (DBG57) THEN
C              WRITE(6,'(A,I0,A,I0,A,I0,A,I0)')
C     .          '[EXCH_DBG] PAIR end r=',ISPMD,' partner=',I-1,
C     .          ' send_cnt=',FR_NBCC(1,I),' recv_cnt=',FR_NBCC(2,I)
C              CALL FLUSH(6)
C            ENDIF
          ENDIF
        ENDDO
        IADSDP(NSPMD+1) = LSD
        IADRCP(NSPMD+1) = LRC
        DO I = 1, NSPMD
          FR_NBCC(1,NSPMD+1) = FR_NBCC(1,NSPMD+1) + FR_NBCC(1,I)
          FR_NBCC(2,NSPMD+1) = FR_NBCC(2,NSPMD+1) + FR_NBCC(2,I)
        ENDDO
      END IF
C
C  900 FORMAT('[EXCH_DBG] r=',I0,' p',I0,' SEND pass',I0,' pos=',I0,
C     .       ' nod=',I0,' uid=',I0,' cc=',I0,' procne=',I0)
C  901 FORMAT('[EXCH_DBG] r=',I0,' p',I0,' RECV pass',I0,' pos=',I0,
C     .       ' nod=',I0,' uid=',I0,' cc=',I0,' procne=',I0)
      RETURN
      END
!||====================================================================
!||    assinit_pxfem        ../engine/source/assembly/assadd2.F
!||--- called by ------------------------------------------------------
!||    resol_alloc_phase2   ../engine/source/engine/resol_alloc.F90
!||====================================================================
      SUBROUTINE ASSINIT_PXFEM(
     1    ADDCNE,INOD, IAD_ELEM, FR_ELEM, PROCNE, LISENDP, LIRECVP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE(*),IAD_ELEM(2,*),INOD(*),
     .        PROCNE(*),FR_ELEM(*), LISENDP, LIRECVP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, LOC_PROC, NOD, CC, LSD, LRC,N
C-----------------------------------------------
C Computing optimized communication sizes parith/on
C-----------------------------------------------
      IF(NSPMD>0.AND.IPARIT>0)THEN
        LOC_PROC = ISPMD+1
C
        LSD = 0
        LRC = 0
        DO I = 1, NSPMD
         IF(I/=LOC_PROC) THEN
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N = FR_ELEM(J)
            NOD = INOD(N)
            IF(NOD > 0 )THEN
               DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   LSD = LSD + 1
                 ELSEIF(PROCNE(CC)==I) THEN
                   LRC = LRC + 1
                 ENDIF
               ENDDO
            ENDIF 
          ENDDO
         ENDIF
        ENDDO
        LISENDP = LSD
        LIRECVP = LRC
      ELSE
        LISENDP = 0
        LIRECVP = 0
      END IF
C
      RETURN
      END
!||====================================================================
!||    assinit_crkxfem      ../engine/source/assembly/assadd2.F
!||--- called by ------------------------------------------------------
!||    resol_alloc_phase2   ../engine/source/engine/resol_alloc.F90
!||====================================================================
      SUBROUTINE ASSINIT_CRKXFEM(
     1    ADDCNE_CRKXFEM,INOD_CRKXFEM, IAD_ELEM, FR_ELEM, PROCNE, LISENDP, LIRECVP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE_CRKXFEM(*),IAD_ELEM(2,*),INOD_CRKXFEM(*),
     .        PROCNE(*),FR_ELEM(*), LISENDP, LIRECVP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, LOC_PROC, NOD, CC, LSD, LRC,N
C-----------------------------------------------
C Computing optimized communication sizes parith/on
C-----------------------------------------------
      IF(NSPMD>0.AND.IPARIT>0)THEN
        LOC_PROC = ISPMD+1
C
        LSD = 0
        LRC = 0
        DO I = 1, NSPMD
         IF(I/=LOC_PROC) THEN
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N = FR_ELEM(J)
            NOD = INOD_CRKXFEM(N)
            IF(NOD > 0 )THEN    ! check if node is Xfem sur proc i /= proc_loc
               DO CC = ADDCNE_CRKXFEM(NOD),ADDCNE_CRKXFEM(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   LSD = LSD + 1           ! exchange boundary length for send
                 ELSEIF(PROCNE(CC)==I) THEN
                   LRC = LRC + 1           ! exchange boundary length for receive
                 ENDIF
               ENDDO
            ENDIF
          ENDDO
         ENDIF
        ENDDO
        LISENDP = LSD
        LIRECVP = LRC
      ELSE
        LISENDP = 0
        LIRECVP = 0
      END IF
C
      RETURN
      END
C
