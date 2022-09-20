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
Chd|  PREPARE_SPLIT_I25E2E          source/spmd/prepare_split_i25e2e.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        LOCAL_EDGE_NUMBERING          source/spmd/prepare_split_i25e2e.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        I25_FIE_MOD                   share/modules1/i25_fie_mod.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_SPLIT_I25E2E(NSPMD,
     1   INTBUF_TAB , IPARI, INTERCEP)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE I25_FIE_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C#include      "scr17_c.inc"
C#include      "scr23_c.inc"
C#include      "com01_c.inc"
C#include      "rnur_c.inc"
C#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,             INTENT(IN) ::    NSPMD
      INTEGER,             INTENT(IN) ::    IPARI(NPARI,*)
      TYPE(INTERSURFP),    INTENT(IN) ::    INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_),INTENT(INOUT) :: INTBUF_TAB(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IEDGE
      INTEGER :: ISPMD
      INTEGER :: NEDGE
      INTEGER :: NIN
      INTEGER :: I,J,K, NRTM, NTY, CS,CM
      INTEGER :: SH_EDGE,SOL_EDGE
      INTEGER, DIMENSION(:), ALLOCATABLE :: CEP_EDGE,CEPM
      INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_ID_EDG
      INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_ID_SEG
      INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_ID

C C++ interface
      INTEGER :: nbCand, nbCandE2E, nbCandE2S, sizeM
      INTEGER, DIMENSION(:), ALLOCATABLE :: CANDS,CANDM
      INTEGER, DIMENSION(:), ALLOCATABLE :: localIdx
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: secondaryRemoteCount
      INTEGER :: LOCAL_CAND_COUNT(NSPMD)
C-----------------------------------------------

        ALLOCATE(I25_SPLIT_CAND(NINTER,NSPMD))
        ALLOCATE(I25_FIE(NINTER,NSPMD))


        DO  NIN = 1,NINTER
          NTY=IPARI(7,NIN)
          IEDGE = IPARI(58,NIN)
          SOL_EDGE =IEDGE/10 ! solids
          SH_EDGE  =IEDGE-10*SOL_EDGE ! shells
C         WRITE(6,*) NIN,NTY,IEDGE
          IF(NTY/=25 .OR. IEDGE==0) CYCLE
          NEDGE = IPARI(68,NIN)
          NRTM = IPARI(4,NIN) 
          ALLOCATE(CEP_EDGE(NEDGE)) !saved also temporarily in LEDGE
          ALLOCATE(LOCAL_ID_EDG(NEDGE))
          ALLOCATE(LOCAL_ID_SEG(NRTM))
          CALL LOCAL_EDGE_NUMBERING(NEDGE,NRTM,NSPMD,
     .                              INTBUF_TAB(NIN)%LEDGE,
     .                              INTERCEP(1,NIN)%P, ! = CEP_SEG
     .                              LOCAL_ID_SEG,
     .                              CEP_EDGE,
     .                              LOCAL_ID_EDG)
        
         nbCandE2E = INTBUF_TAB(NIN)%I_STOK_E(1)
         nbCandE2S = INTBUF_TAB(NIN)%I_STOK_E(2)


         nbCand = nbCandE2E + nbCandE2S 
         ALLOCATE(localIdx(nbCand))
         ALLOCATE(secondaryRemoteCount(NSPMD,NSPMD))
         ALLOCATE(CANDM(nbCand))
         ALLOCATE(CANDS(nbCand))
         ALLOCATE(CEPM(NEDGE+NRTM))
         ALLOCATE(LOCAL_ID(NEDGE+NRTM))
         LOCAL_ID(1:NEDGE) = LOCAL_ID_EDG(1:NEDGE)
         LOCAL_ID(NEDGE+1:NEDGE+NRTM) = LOCAL_ID_SEG(1:NRTM)

         sizeM = NEDGE + NRTM
         CANDM(1:nbCandE2E) = INTBUF_TAB(NIN)%CANDM_E2E(1:nbCandE2E) 
         CANDS(1:nbCandE2E) = INTBUF_TAB(NIN)%CANDS_E2E(1:nbCandE2E) 
         CANDM(nbCandE2E+1:nbCand) = NEDGE + INTBUF_TAB(NIN)%CANDM_E2S(1:nbCandE2S) 
         CANDS(nbCandE2E+1:nbCand) = INTBUF_TAB(NIN)%CANDS_E2S(1:nbCandE2S) 
         CEPM(1:NEDGE) = CEP_EDGE(1:NEDGE)
         CEPM(NEDGE+1:NEDGE+NRTM) = INTERCEP(1,NIN)%P(1:NRTM) - 1

         CALL CPP_COUNT_CANDIDATES(nbCand,
     .     sizeM, !sizeM,
     .     CEPM,!CepM
     .     LOCAL_ID, !localIdM,
     .     CANDM, !candM, 
     .     NEDGE,  !sizeS,
     .     CEP_EDGE, !cepS,
     .     LOCAL_ID_EDG, ! localIdS,
     .     CANDS, !candS 
     .     NSPMD, 
     .     secondaryRemoteCount,
     .     localIdx) 

          DO I = 1,NSPMD
            I25_FIE(NIN,I)%NEDGE_TOT = 0 
            ALLOCATE(I25_FIE(NIN,I)%NEDGE(NSPMD))
            I25_FIE(NIN,I)%NEDGE(1:NSPMD) = 0
            DO J = 1,NSPMD
              I25_FIE(NIN,I)%NEDGE(J) = I25_FIE(NIN,I)%NEDGE(J) + secondaryRemoteCount(J,I) 
              I25_FIE(NIN,I)%NEDGE_TOT = I25_FIE(NIN,I)%NEDGE_TOT + secondaryRemoteCount(J,I) 
            ENDDO
            ALLOCATE(I25_FIE(NIN,I)%ID(I25_FIE(NIN,I)%NEDGE_TOT))
            I25_FIE(NIN,I)%ID(1:I25_FIE(NIN,I)%NEDGE_TOT) = -666
C           WRITE(6,*) I,"NEDGE_TOT=",I25_FIE(NIN,I)%NEDGE_TOT
          ENDDO
C         E2E
          LOCAL_CAND_COUNT(1:NSPMD) = 0
          DO I = 1, nbCandE2E
            CM = CEPM(INTBUF_TAB(NIN)%CANDM_E2E(I)) + 1
            LOCAL_CAND_COUNT(CM) = 1 + LOCAL_CAND_COUNT(CM)
          ENDDO
          DO I = 1, NSPMD 
             I25_SPLIT_CAND(NIN,I)%NB_CAND_E2E = LOCAL_CAND_COUNT(I)
C            WRITE(6,*) "SPLIT_CAND size",I-1,LOCAL_CAND_COUNT(I)
             ALLOCATE(I25_SPLIT_CAND(NIN,I)%CANDM_E2E(LOCAL_CAND_COUNT(I)))
             ALLOCATE(I25_SPLIT_CAND(NIN,I)%CANDS_E2E(LOCAL_CAND_COUNT(I)))
             ALLOCATE(I25_SPLIT_CAND(NIN,I)%ID_E2E(LOCAL_CAND_COUNT(I)))
             LOCAL_CAND_COUNT(I) = 0
          ENDDO
          DO I = 1, nbCandE2E
             J = CEPM(INTBUF_TAB(NIN)%CANDM_E2E(I)) + 1 ! domain
             LOCAL_CAND_COUNT(J) = 1 + LOCAL_CAND_COUNT(J)
             K = LOCAL_CAND_COUNT(J)
             I25_SPLIT_CAND(NIN,J)%CANDS_E2E(K) = CANDS(I) 
             ASSERT(CANDM(I) > 0)
             I25_SPLIT_CAND(NIN,J)%CANDM_E2E(K) = CANDM(I) 
             I25_SPLIT_CAND(NIN,J)%ID_E2E(K) = I
             IF(CANDS(I) < 0) THEN !remote
                I25_FIE(NIN,J)%ID(ABS(CANDS(I))) = localIdx(I)
                ASSERT(localIdx(I) > 0)
             ENDIF
C            WRITE(6,"(I10,A,2I10,A,I10)") J," has E candidate ",
C    .       localIdx(I),INTBUF_TAB(NIN)%CANDS_E2E(I)," on ",CEP_EDGE(INTBUF_TAB(NIN)%CANDS_E2E(I))+1 
          ENDDO

C         E2S
          LOCAL_CAND_COUNT(1:NSPMD) = 0
          DO I = 1, nbCandE2S
            CM = CEPM(INTBUF_TAB(NIN)%CANDM_E2S(I) + NEDGE ) + 1
            LOCAL_CAND_COUNT(CM) = 1 + LOCAL_CAND_COUNT(CM)
          ENDDO
          DO I = 1, NSPMD 
             I25_SPLIT_CAND(NIN,I)%NB_CAND_E2S = LOCAL_CAND_COUNT(I)
             ALLOCATE(I25_SPLIT_CAND(NIN,I)%CANDM_E2S(LOCAL_CAND_COUNT(I)))
             ALLOCATE(I25_SPLIT_CAND(NIN,I)%CANDS_E2S(LOCAL_CAND_COUNT(I)))
             ALLOCATE(I25_SPLIT_CAND(NIN,I)%ID_E2S(LOCAL_CAND_COUNT(I)))
             LOCAL_CAND_COUNT(I) = 0
          ENDDO
          DO I = 1, nbCandE2S
             J = CEPM(INTBUF_TAB(NIN)%CANDM_E2S(I) + NEDGE) + 1 ! domain
             LOCAL_CAND_COUNT(J) = 1 + LOCAL_CAND_COUNT(J)
             K = LOCAL_CAND_COUNT(J)
C            ASSERT(CANDM(I+nbCandE2E) < 0) 
             I25_SPLIT_CAND(NIN,J)%CANDS_E2S(K) = CANDS(I+nbCandE2E) 
             I25_SPLIT_CAND(NIN,J)%CANDM_E2S(K) = CANDM(I+nbCandE2E)-NEDGE 
             I25_SPLIT_CAND(NIN,J)%ID_E2S(K) = I
             IF(CANDS(I+nbCandE2E) < 0) THEN !remote
                I25_FIE(NIN,J)%ID(ABS(CANDS(I+nbCandE2E))) = localIdx(I+nbCandE2E)
                ASSERT(localIdx(I+nbCandE2E) > 0 )
             ENDIF
C            WRITE(6,"(I10,A,2I10,A,I10)") J," has S candidate ",
C    . localIdx(I),INTBUF_TAB(NIN)%CANDS_E2S(I)," on ",CEP_EDGE(INTBUF_TAB(NIN)%CANDS_E2S(I))+1 
          ENDDO

CC ==debug print
C         DO J = 1,NSPMD
C            DO I = 1, I25_FIE(NIN,J)%NEDGE_TOT
C              IF(I25_FIE(NIN,J)%ID(I) < 0) WRITE(6,*) J,"FIE(",I,")=",I25_FIE(NIN,J)%ID(I)
C            ENDDO 
C         ENDDO
CC ==========
           
          DEALLOCATE(CANDM)
          DEALLOCATE(CANDS)
          DEALLOCATE(localIdx)
          DEALLOCATE(secondaryRemoteCount)
          DEALLOCATE(CEPM)
          DEALLOCATE(CEP_EDGE)
          DEALLOCATE(LOCAL_ID_SEG)
          DEALLOCATE(LOCAL_ID_EDG)
          DEALLOCATE(LOCAL_ID)

        ENDDO
      END SUBROUTINE

Chd|====================================================================
Chd|  LOCAL_EDGE_NUMBERING          source/spmd/prepare_split_i25e2e.F
Chd|-- called by -----------
Chd|        PREPARE_SPLIT_I25E2E          source/spmd/prepare_split_i25e2e.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE LOCAL_EDGE_NUMBERING(NEDGE,NRTM,NSPMD,
     .                              LEDGE,           
     .                              CEP_SEG,
     .                              LOCAL_ID_SEG,
     .                              CEP_EDGE,
     .                              LOCAL_ID_EDG)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NEDGE,NRTM,NSPMD
      INTEGER, INTENT(INOUT) ::  LEDGE(NLEDGE,NEDGE) 
      INTEGER, INTENT(IN) ::  CEP_SEG(NRTM)          !starts at 1
      INTEGER, INTENT(INOUT) ::  LOCAL_ID_SEG(NRTM)
      INTEGER, INTENT(INOUT) ::  CEP_EDGE(NEDGE)
      INTEGER, INTENT(INOUT) ::  LOCAL_ID_EDG(NEDGE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NRTM_LOCAL(NSPMD)
      INTEGER :: NB_FREE_EDGES(NSPMD)
      INTEGER :: NB_INTERNAL_EDGES(NSPMD)
      INTEGER :: NB_BOUNDARY_EDGES_LOCAL(NSPMD) ! boundary edges treated by current domain 
      INTEGER :: NB_BOUNDARY_EDGES_REMOTE(NSPMD) ! boundary edges treated by the other domain 
      INTEGER :: EDGE_LOCAL(NSPMD)
      INTEGER :: ISPMD
      INTEGER :: I,E1,E2,K1,K2
C-----------------------------------------------
      NRTM_LOCAL(1:NSPMD) = 0
      EDGE_LOCAL(1:NSPMD) = 0
      NB_FREE_EDGES(1:NSPMD) = 0
      NB_INTERNAL_EDGES(1:NSPMD) = 0
      NB_BOUNDARY_EDGES_LOCAL(1:NSPMD) = 0
      NB_BOUNDARY_EDGES_REMOTE(1:NSPMD) = 0

      CEP_EDGE(1:NEDGE) = -1
C FREE EDGES
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        E2=LEDGE(3,I)
        IF(E2 == 0) THEN
          ISPMD = CEP_SEG(E1)
          NB_FREE_EDGES(ISPMD) = NB_FREE_EDGES(ISPMD) + 1
          EDGE_LOCAL(ISPMD) = EDGE_LOCAL(ISPMD) + 1
          CEP_EDGE(I) = ISPMD - 1
          LOCAL_ID_EDG(I) = EDGE_LOCAL(ISPMD)
          ASSERT(LEDGE(9,I) == 0)
          LEDGE(9,I) = ISPMD - 1
          LEDGE(10,I) = LOCAL_ID_EDG(I)
        END IF
      ENDDO

C INTERNAL EDGES 
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        E2=LEDGE(3,I)
        ISPMD = CEP_SEG(E1)
        IF(E2 > 0 ) THEN ! edge not boundary
        IF(ISPMD /= CEP_SEG(E2)) CYCLE ! edge not internal
          NB_INTERNAL_EDGES(ISPMD) = NB_INTERNAL_EDGES(ISPMD) + 1
          EDGE_LOCAL(ISPMD) = EDGE_LOCAL(ISPMD) + 1 
          CEP_EDGE(I) = ISPMD - 1
          LOCAL_ID_EDG(I) = EDGE_LOCAL(ISPMD)
          ASSERT(LEDGE(9,I) == 0)
          LEDGE(9,I) = ISPMD - 1
          LEDGE(10,I) = LOCAL_ID_EDG(I)
        END IF
      ENDDO

C BOUNDARY EDGE LOCAL
      DO I=1, NEDGE
        E1=LEDGE(1,I)
        E2=LEDGE(3,I)
        ISPMD = CEP_SEG(E1)
        IF(E2 > 0 ) THEN ! edge not boundary
        IF(ISPMD == CEP_SEG(E2)) CYCLE ! edge internal
          NB_BOUNDARY_EDGES_LOCAL(ISPMD) = NB_BOUNDARY_EDGES_LOCAL(ISPMD) + 1
          EDGE_LOCAL(ISPMD) = EDGE_LOCAL(ISPMD) +1 
          CEP_EDGE(I) = ISPMD - 1
          LOCAL_ID_EDG(I) = EDGE_LOCAL(ISPMD)
          ASSERT(LEDGE(9,I) == 0)
          LEDGE(9,I) = ISPMD - 1
          LEDGE(10,I) = LOCAL_ID_EDG(I)
        END IF
      ENDDO


      DO I = 1,NEDGE
        ASSERT(CEP_EDGE(I) >= 0)
      ENDDO
C     compute local id of segment
      DO K = 1,NRTM
        ISPMD = CEP_SEG(K) ! starts at 1
        NRTM_LOCAL(ISPMD) = NRTM_LOCAL(ISPMD) + 1
        LOCAL_ID_SEG(K) = -NRTM_LOCAL(ISPMD)
      ENDDO

C Debug print 
C      DO ISPMD = 1,NSPMD
C        write(6,*) ISPMD,"free=",NB_FREE_EDGES(ispmd)
C        write(6,*) ISPMD,"INTERNAL=",NB_INTERNAL_EDGES(ispmd)
C        write(6,*) ISPMD,"BOUNDARY=",NB_BOUNDARY_EDGES_LOCAL(ispmd)
C      ENDDO
C =============

      END SUBROUTINE

