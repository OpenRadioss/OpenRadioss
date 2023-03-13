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
Chd|  I25STO_EDG                    source/interfaces/intsort/i25sto_edg.F
Chd|-- called by -----------
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|-- calls ---------------
Chd|        I25PEN3_EDG                   source/interfaces/intsort/i25pen3_edg.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25STO_EDG(
     1      J_STOK,IRECT  ,X     ,II_STOK,INACTI,
     2      CAND_S,CAND_M ,MULNSN,NOINT  ,MARGE,
     3      I_MEM ,PROV_S ,PROV_M,IGAP0  ,CAND_A,
     4      NEDGE ,LEDGE  ,ITAB  ,DRAD   ,IGAP ,
     5      GAPE  ,GAP_E_L,ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     6      CAND_P,IFQ    ,CAND_FX ,CAND_FY,CAND_FZ,IFPEN ,
     7      DGAPLOAD)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, IGAP0, NEDGE, NIN, ITAB(NUMNOD), INACTI,IFQ
      INTEGER J_STOK,MULNSN,NOINT,IFORM,IGAP
      INTEGER IRECT(4,*),LEDGE(NLEDGE,NEDGE),ADMSR(4,*),CAND_S(*),CAND_M(*),II_STOK,CAND_A(*)
      INTEGER PROV_S(MVSIZ),PROV_M(MVSIZ),IFPEN(*)
C     INTEGER NEDGEROLD,RENUM(*)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   X(3,NUMNOD), MARGE, GAPE(*), GAP_E_L(*), CAND_P(*),
     .   CAND_FX(*), CAND_FY(*), CAND_FZ(*)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD0,IAD,IADFIN,N,NE,ie,n1,n2
      INTEGER I_STOK_FIRST,NINDX,INDEX(MVSIZ) 
      INTEGER :: EID
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
      CALL I25PEN3_EDG( J_STOK,PROV_S ,PROV_M ,DRAD    ,IGAP0   ,
     .                  NEDGE ,LEDGE  ,MARGE  ,GAPE    ,GAP_E_L ,
     .                  IGAP   ,X     ,IRECT  ,PENE    ,ADMSR   ,
     .                  EDG_BISECTOR,VTX_BISECTOR,ITAB,
     .                  XREM_EDGE,E_RBUF_SIZE,NEDGE_REMOTE,DGAPLOAD)

C-----------------------------------------------
C SUPPRESSION DES ANCIENS CANDIDATS DEJA STOCKES (PENE INITIALE)
C-----------------------------------------------
C     IF(INACTI==5)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            N  = PROV_S(I)
            NE = PROV_M(I)

            IF(N>NEDGE) THEN
C numerotation tris precedent pour les noeuds non locaux (SPMD)
              N = OLDNUM_EDGE(N-NEDGE)+NEDGE
              IF(N <= NEDGE) N = NEDGE+NEDGE_REMOTE_OLD+1
            END IF

            ASSERT(N > 0)
            J = CAND_A(N)
            DO WHILE(J<=CAND_A(N+1)-1)
              IF(CAND_M(J)==NE)THEN
! This is an old candidate
! pene is set to 0 in order to ignore it
C               WRITE(6,*) "PENE SET TO 0 FOR OLD CAND",J,CAND_M(J),LEDGE(8,NE)
                PENE(I)=ZERO
                J=CAND_A(N+1)
              ELSE
                J=J+1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C     ENDIF
C-----------------------------------------------
      K_STOK = 0
      DO I=1,J_STOK
          IF(PENE(I)/=ZERO) THEN
            K_STOK = K_STOK + 1
          END IF 
      ENDDO
      IF(K_STOK==0)RETURN
C
#include "lockon.inc"
      I_STOK = II_STOK
      IF(I_STOK+K_STOK>MULNSN) THEN
            I_MEM = 2
#include "lockoff.inc"
            RETURN
      ENDIF
      II_STOK   = I_STOK + K_STOK
#include "lockoff.inc"
C     IF(INACTI==5)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN

            I_STOK = I_STOK + 1
            ASSERT(PROV_S(I) > 0)
            ASSERT(PROV_S(I) <= NEDGE + NEDGE_REMOTE)
C#ifdef D_EM
C            IF(LEDGE(8,PROV_M(I)) == D_EM) THEN
C              IF(PROV_S(I) <= NEDGE) THEN
C                EID = LEDGE(8,PROV_S(I))
C              ELSE
C                EID = IREM_EDGE(E_GLOBAL_ID,PROV_S(I) - NEDGE)
C              ENDIF
C              IF(EID == D_ES .AND. LEDGE(8,PROV_M(i)) == D_EM) THEN
C              WRITE(6,*) "Saved",LEDGE(8,PROV_M(I)),EID
C              ENDIF
C            ENDIF
C#endif
            CAND_S(I_STOK) = PROV_S(I)
            CAND_M(I_STOK) = PROV_M(I)
            CAND_P(I_STOK) = ZERO

            IF(IFQ > 0) THEN
               CAND_FX(I_STOK) = ZERO
               CAND_FY(I_STOK) = ZERO
               CAND_FZ(I_STOK) = ZERO
               IFPEN(I_STOK) = 0
            ENDIF
          ENDIF
        ENDDO
C     ELSE
C       DO I=1,J_STOK
C         IF(PENE(I)/=ZERO)THEN
C           I_STOK = I_STOK + 1
C           CAND_S(I_STOK) = PROV_S(I)
C           CAND_M(I_STOK) = PROV_M(I)
C         ENDIF
C       ENDDO
C     END IF
C-----------------------------------------------
      RETURN
      END


