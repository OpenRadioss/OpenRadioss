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
Chd|====================================================================
Chd|  C_SPMD_NE_CONNECT             source/ale/spmd_ne_connect.F  
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE C_SPMD_NE_CONNECT(ALE_CONNECTIVITY, PROC    , CEP      , CEL     , 
     .                             NODGLOBAL       , NODLOCAL, 
     .                             NUMEL           , NUMNOD  , NUMEL_L  , NUMNOD_L, 
     .                             NUMELS_L        , NUMELQ_L, NUMELTG_L,
     .                             NERVOIS         , NESVOIS ,
     .                             NSVOIS          , NQVOIS  , NTGVOIS)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is used to split connectivity 
C buffers in case of SPMD domain decomposition
C-----------------------------------------------
C  M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
!     Includes
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: CEP(*), CEL(*)
      INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: NODLOCAL
      INTEGER, DIMENSION(NUMNOD_L), INTENT(IN) :: NODGLOBAL
      INTEGER, INTENT(IN) :: PROC, NUMNOD, NUMNOD_L, NUMEL, NUMEL_L, 
     .     NUMELS_L, NUMELQ_L, NUMELTG_L
      INTEGER, INTENT(INOUT) :: NSVOIS, NQVOIS, NTGVOIS, NERVOIS, NESVOIS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: INODE, IAD, IAD1, IAD2, NODE_ID, P, IELEM, IELEM_L, PROC2, II
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGELE
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: TAGELE_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: LERCVOIS, LESDVOIS, NERCVOIS, NESDVOIS
      INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_LIST
      INTEGER :: ELEM1_L, ELEM2_L
      LOGICAL :: OK_RC
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE(TAGELE(NUMEL))
      TAGELE(1:NUMEL) = 0
      ALLOCATE(TAGELE_L(NSPMD, NUMEL_L))
      TAGELE_L(1:NSPMD, 1:NUMEL_L) = 0
      
      ALLOCATE(NERCVOIS(NSPMD + 1))
      NERCVOIS(1:NSPMD + 1) = 0
      ALLOCATE(NESDVOIS(NSPMD + 1))
      NESDVOIS(1:NSPMD + 1) = 0

      NSVOIS = 0
      NQVOIS = 0
      NTGVOIS = 0
  
      DO INODE = 1, NUMNOD_L
         NODE_ID = NODGLOBAL(INODE)
         IAD1 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODE_ID)
         IAD2 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODE_ID + 1) - 1
         IF (IAD2 >= IAD1) ALLOCATE(PROC_LIST(IAD2 - IAD1 + 1))
         OK_RC = .FALSE.
         DO IAD = IAD1, IAD2
            IELEM = ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD)
            IELEM_L = CEL(IELEM)
            PROC2 = CEP(IELEM) ! CEP numbered from 0 to NSPMD - 1
            PROC_LIST(IAD - IAD1 + 1) = PROC2
            IF (PROC2 == PROC) THEN
               OK_RC = .TRUE.
            ENDIF
         ENDDO
        
         IF (OK_RC) THEN
            DO IAD = IAD1, IAD2
               IELEM = ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD)
               IELEM_L = CEL(IELEM)
               PROC2 = PROC_LIST(IAD - IAD1 + 1)
               IF (PROC2 /= PROC) THEN
                  IF (TAGELE(IELEM) == 0) THEN
                     TAGELE(IELEM) = 1
                     NERCVOIS(PROC2 + 1) = NERCVOIS(PROC2 + 1) + 1
                     IF (ALE_CONNECTIVITY%NE_CONNECT%TYPE(IAD) == 1) THEN
                        NSVOIS = NSVOIS + 1
                     ELSE IF (ALE_CONNECTIVITY%NE_CONNECT%TYPE(IAD) == 2) THEN
                        NQVOIS = NQVOIS + 1
                     ELSE IF (ALE_CONNECTIVITY%NE_CONNECT%TYPE(IAD) == 2) THEN
                        NTGVOIS = NTGVOIS + 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         DO IAD = IAD1, IAD2
            IF (PROC_LIST(IAD - IAD1 + 1) == PROC) THEN
               ELEM1_L = CEL(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD))
               DO II = IAD1, IAD2
                  IF (II /= IAD) THEN
                     ELEM2_L = CEL(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(II))
                     PROC2 = PROC_LIST(II - IAD1 + 1)
                     IF (PROC2 /= PROC) THEN
                        IF (TAGELE_L(PROC2 + 1, ELEM1_L) == 0) THEN
                           TAGELE_L(PROC2 + 1, ELEM1_L) = 1
                           NESDVOIS(PROC2 + 1) = NESDVOIS(PROC2 + 1) + 1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF (ALLOCATED(PROC_LIST)) DEALLOCATE(PROC_LIST)
      ENDDO
      NESVOIS = 0
      NERVOIS = 0
      DO P = 1, NSPMD
         NESVOIS = NESVOIS + NESDVOIS(P)
         NERVOIS = NERVOIS + NERCVOIS(P)
      ENDDO         
      
      DEALLOCATE(TAGELE_L, TAGELE, NERCVOIS, NESDVOIS)
      
      END SUBROUTINE C_SPMD_NE_CONNECT


Chd|====================================================================
Chd|  SPMD_NE_CONNECT               source/ale/spmd_ne_connect.F  
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_NE_CONNECT(ALE_CONNECTIVITY, PROC    , CEP      , CEL     , 
     .                           NODGLOBAL       , NODLOCAL, 
     .                           NUMEL           , NUMNOD  , NUMEL_L  , NUMNOD_L, 
     .                           NUMELS_L        , NUMELQ_L, NUMELTG_L,
     .                           NERVOIS         , NESVOIS ,
     .                           NSVOIS          , NQVOIS  , NTGVOIS  , ELEMID_L, LEN_IA,IXS)     
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is used to split connectivity 
C buffers in case of SPMD domain decomposition
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: CEP(*), CEL(*)
      INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: NODLOCAL
      INTEGER, DIMENSION(NUMNOD_L), INTENT(IN) :: NODGLOBAL
      INTEGER, INTENT(IN) :: PROC, NUMNOD, NUMNOD_L, NUMEL, NUMEL_L, NUMELS_L, NUMELQ_L, NUMELTG_L
      INTEGER, INTENT(IN) :: NERVOIS, NESVOIS
      INTEGER, INTENT(INOUT) :: NSVOIS, NQVOIS, NTGVOIS
      INTEGER, DIMENSION(NUMEL), INTENT(INOUT) :: ELEMID_L
      INTEGER, INTENT(INOUT) :: LEN_IA
      INTEGER, INTENT(IN) :: IXS(11,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: INODE, IAD, IAD1, IAD2, NODE_ID, P, IELEM, IELEM_L, PROC2, IFE, JFE, II
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGELE
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: TAGELE_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: LERCVOIS, LESDVOIS, NERCVOIS, NESDVOIS
      INTEGER, DIMENSION(:), ALLOCATABLE :: CPULOCALER, CPULOCALES, TAGER, TAGES
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDX, TMP, TMP2, PROC_LIST
      INTEGER :: ELEM1_L, ELEM2_L
      LOGICAL :: OK_RC
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE(TAGELE(NUMEL))
      TAGELE(1:NUMEL) = 0
      ALLOCATE(TAGELE_L(NSPMD, NUMEL_L))
      TAGELE_L(1:NSPMD, 1:NUMEL_L) = 0
      ELEMID_L(1:NUMEL) = 0
      ALLOCATE(LERCVOIS(NERVOIS))
      LERCVOIS(1:NERVOIS) = 0
      ALLOCATE(LESDVOIS(NESVOIS))
      LERCVOIS(1:NERVOIS) = 0
      ALLOCATE(NERCVOIS(NSPMD + 1))
      NERCVOIS(1:NSPMD + 1) = 0
      ALLOCATE(NESDVOIS(NSPMD + 1))
      NESDVOIS(1:NSPMD + 1) = 0
      ALLOCATE(TAGER(NERVOIS))
      TAGER(1:NERVOIS) = 0
      ALLOCATE(CPULOCALER(NERVOIS))
      CPULOCALER(1:NERVOIS) = 0
      ALLOCATE(TAGES(NESVOIS))
      TAGES(1:NESVOIS) = 0
      ALLOCATE(CPULOCALES(NESVOIS))
      CPULOCALES(1:NESVOIS) = 0
      ALLOCATE(IDX(MAX(NERVOIS, NESVOIS)))
      IDX(1:MAX(NERVOIS, NESVOIS)) = 0
      ALLOCATE(TMP(MAX(NERVOIS, NESVOIS)))
      TMP(1:MAX(NERVOIS, NESVOIS)) = 0
      ALLOCATE(TMP2(MAX(NERVOIS, NESVOIS)))
      TMP2(1:MAX(NERVOIS, NESVOIS)) = 0

      IFE = 0
      JFE = 0

      DO INODE = 1, NUMNOD_L
         NODE_ID = NODGLOBAL(INODE)
         IAD1 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODE_ID)
         IAD2 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODE_ID + 1) - 1
         IF (IAD2 >= IAD1) ALLOCATE(PROC_LIST(IAD2 - IAD1 + 1))
         OK_RC = .FALSE.
         DO IAD = IAD1, IAD2
            IELEM = ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD)
            IELEM_L = CEL(IELEM)
            PROC2 = CEP(IELEM) ! CEP numbered from 0 to NSPMD - 1
            PROC_LIST(IAD - IAD1 + 1) = PROC2
            IF (PROC2 == PROC) THEN
               OK_RC = .TRUE.
               ELEMID_L(IELEM) = IELEM_L
            ENDIF
         ENDDO
        
         IF (OK_RC) THEN
            DO IAD = IAD1, IAD2
               IELEM = ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD)
               IELEM_L = CEL(IELEM)
               PROC2 = PROC_LIST(IAD - IAD1 + 1)
               IF (PROC2 /= PROC) THEN
                  IF (TAGELE(IELEM) == 0) THEN
                     TAGELE(IELEM) = 1
                     IFE = IFE + 1
                     IF (ALE_CONNECTIVITY%NE_CONNECT%TYPE(IAD) == 1) THEN
                        ELEMID_L(IELEM) = NUMELS_L + IFE
                        LERCVOIS(IFE) = NUMELS_L + IFE
                     ELSE IF (ALE_CONNECTIVITY%NE_CONNECT%TYPE(IAD) == 2) THEN
                        ELEMID_L(IELEM) = NUMELQ_L + IFE
                        LERCVOIS(IFE) = NUMELQ_L + IFE
                     ELSE IF (ALE_CONNECTIVITY%NE_CONNECT%TYPE(IAD) == 3) THEN
                        ELEMID_L(IELEM) = NUMELTG_L + IFE
                        LERCVOIS(IFE) = NUMELTG_L + IFE
                     ENDIF
                     NERCVOIS(PROC2 + 1) = NERCVOIS(PROC2 + 1) + 1
                     TAGER(IFE) = IELEM
                     CPULOCALER(IFE) = PROC2
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         DO IAD = IAD1, IAD2
            IF (PROC_LIST(IAD - IAD1 + 1) == PROC) THEN
               ELEM1_L = CEL(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD))
               DO II = IAD1, IAD2
                  IF (II /= IAD) THEN
                     ELEM2_L = CEL(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(II))
                     PROC2 = PROC_LIST(II - IAD1 + 1)
                     IF (PROC2 /= PROC) THEN
                        IF (TAGELE_L(PROC2 + 1, ELEM1_L) == 0) THEN
                           TAGELE_L(PROC2 + 1, ELEM1_L) = 1
                           NESDVOIS(PROC2 + 1) = NESDVOIS(PROC2 + 1) + 1
                           JFE = JFE + 1
                           LESDVOIS(JFE) = ELEM1_L
                           TAGES(JFE) = ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(IAD)
                           CPULOCALES(JFE) = PROC2
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         
         IF (ALLOCATED(PROC_LIST)) DEALLOCATE(PROC_LIST)
      ENDDO

      DO P = 1, NSPMD
         NERCVOIS(NSPMD + 1) = NERCVOIS(NSPMD + 1) + NERCVOIS(P)
         NESDVOIS(NSPMD + 1) = NESDVOIS(NSPMD + 1) + NESDVOIS(P)
      ENDDO
 
C     Sorting LERCVOIS
      DO II = 1, NERVOIS
         TMP(II) = TAGER(II)
         TMP2(II) = LERCVOIS(II)
         IDX(II) = II
      ENDDO
      CALL QUICKSORT_I2(CPULOCALER, IDX, 1, NERVOIS)

      DO II = 1, NERVOIS
         TAGER(II) = TMP(IDX(II))
         LERCVOIS(II) = TMP2(IDX(II))
      ENDDO
      DO II = 1, NERVOIS
         TMP2(II) = LERCVOIS(II)
      ENDDO
     
      IAD = 1
      DO WHILE (IAD <= NERVOIS)
         DO IAD1 = IAD, NERVOIS
            IF (CPULOCALER(IAD) /= CPULOCALER(IAD1)) THEN
               EXIT
            ENDIF
         ENDDO
         DO II = IAD, IAD1 - 1
            IDX(II) = II
         ENDDO
         CALL QUICKSORT_I2(TAGER(IAD:IAD1 - 1), IDX(IAD:IAD1 - 1), 1, IAD1 - IAD)
         IAD = IAD1
      ENDDO
     
      DO II = 1, NERVOIS
         LERCVOIS(II) = TMP2(IDX(II))
      ENDDO

C     Sorting LESDVOIS
      DO II = 1, NESVOIS
         TMP(II) = TAGES(II)
         TMP2(II) = LESDVOIS(II)
         IDX(II) = II
      ENDDO
      CALL QUICKSORT_I2(CPULOCALES, IDX, 1, NESVOIS)

      DO II = 1, NESVOIS
         TAGES(II) = TMP(IDX(II))
         LESDVOIS(II) = TMP2(IDX(II))
      ENDDO
      DO II = 1, NESVOIS
         TMP2(II) = LESDVOIS(II)
      ENDDO
     
      IAD = 1
      DO WHILE (IAD <= NESVOIS)
         DO IAD1 = IAD, NESVOIS
            IF (CPULOCALES(IAD) /= CPULOCALES(IAD1)) THEN
               EXIT
            ENDIF
         ENDDO
         DO II = IAD, IAD1 - 1
            IDX(II) = II
         ENDDO
         CALL QUICKSORT_I2(TAGES(IAD:IAD1 - 1), IDX(IAD:IAD1 - 1), 1, IAD1 - IAD)
         IAD = IAD1
      ENDDO
     
      DO II = 1, NESVOIS
         LESDVOIS(II) = TMP2(IDX(II))
      ENDDO
      
      CALL WRITE_I_C(NERCVOIS, NSPMD + 1)
      LEN_IA = LEN_IA + NSPMD + 1
      CALL WRITE_I_C(LERCVOIS, NERVOIS)
      LEN_IA = LEN_IA + NERVOIS
      CALL WRITE_I_C(NESDVOIS, NSPMD + 1)
      LEN_IA = LEN_IA + NSPMD + 1
      CALL WRITE_I_C(LESDVOIS, NESVOIS)
      LEN_IA = LEN_IA + NESVOIS

      DEALLOCATE(TAGELE, TAGELE_L, NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .     CPULOCALER, CPULOCALES, TAGER, TAGES, TMP, TMP2, IDX)
      END SUBROUTINE SPMD_NE_CONNECT
