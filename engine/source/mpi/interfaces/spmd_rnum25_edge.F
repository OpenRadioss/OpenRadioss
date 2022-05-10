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
Chd|  SPMD_RNUM25_EDGE              source/mpi/interfaces/spmd_rnum25_edge.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_RNUM25_EDGE(NIN,NEDGE,CAND_E2E,ISTOK_E2E, CAND_E2S,ISTOK_E2S)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "assert.inc"
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NIN, NEDGE,ISTOK_E2E,ISTOK_E2S
      INTEGER, INTENT(INOUT) ::  CAND_E2E(ISTOK_E2E),CAND_E2S(ISTOK_E2S)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, I_STOK, IDEB, JDEB, NI
      INTEGER EID,N_REMOTE_OLD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      N_REMOTE_OLD = 0
      DO P = 1, NSPMD
        N_REMOTE_OLD = N_REMOTE_OLD + NSNFIEOLD(P)
      ENDDO

      ALLOCATE(RENUM_EDGE(N_REMOTE_OLD))
      DO I = 1, N_REMOTE_OLD
        RENUM_EDGE(I) = -1
      END DO


      ALLOCATE(OLDNUM_EDGE(NEDGE_REMOTE))
      DO I = 1, NEDGE_REMOTE
        OLDNUM_EDGE(I) = 0 
!       OLDNUM_EDGE(NEW) = OLD or 0
      END DO

      IDEB = 0
      JDEB = 0


      DO P = 1, NSPMD
        I = 1
        J = 1
        DO WHILE (J.LE.NSNFIE(NIN)%P(P).AND.I.LE.NSNFIEOLD(P))

          IF(IREM_EDGE(E_LOCAL_ID,J+JDEB).EQ.
     +       NSVFIE(NIN)%P(I+IDEB)) THEN
C           RENUM_EDGE(OLD) = NEW
            RENUM_EDGE(I+IDEB) = J+JDEB
C debug
C           WRITE(6,"(I10,A,I10,A,I10,I10)") IREM_EDGE(E_GLOBAL_ID,J+JDEB),
C    .                 " RENUM(",J+JDEB,")= ",I+IDEB,NEDGE_REMOTE
C
            I = I + 1
            J = J + 1
          ELSEIF(IREM_EDGE(E_LOCAL_ID,J+JDEB).LT.
     +           NSVFIE(NIN)%P(I+IDEB)) THEN
           J = J + 1

          ELSEIF(IREM_EDGE(E_LOCAL_ID,J+JDEB).GT.
     +           NSVFIE(NIN)%P(I+IDEB)) THEN
Cas candidat non penetre et non retenu
            I = I + 1
          END IF
        END DO
        JDEB = JDEB + NSNFIE(NIN)%P(P)  
        IDEB = IDEB + NSNFIEOLD(P)
      END DO
C

      DO I = 1, N_REMOTE_OLD
        IF(RENUM_EDGE(I).GT.0)  THEN
!          OLDNUM_EDGE(NEW) = OLD
           OLDNUM_EDGE(RENUM_EDGE(I)) = I
        ENDIF
      END DO

      DO I = 1, ISTOK_E2E
        NI = CAND_E2E(I)
        IF(NI.GT.NEDGE) THEN
          NI = NI - NEDGE 
          CAND_E2E(I) = RENUM_EDGE(NI) + NEDGE
        END IF        
      END DO

      DO I = 1, ISTOK_E2S
        NI = CAND_E2S(I)
        IF(NI.GT.NEDGE) THEN
          NI = NI - NEDGE 
          CAND_E2S(I) = RENUM_EDGE(NI) + NEDGE
        END IF        
      END DO


      RETURN
      END

