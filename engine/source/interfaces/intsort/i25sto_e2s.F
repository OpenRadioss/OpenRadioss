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
Chd|  I25STO_E2S                    source/interfaces/intsort/i25sto_e2s.F
Chd|-- called by -----------
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|-- calls ---------------
Chd|        I25PEN3_E2S                   source/interfaces/intsort/i25pen3_e2s.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25STO_E2S(
     1      J_STOK,IRECT  ,X     ,II_STOK,INACTI,
     2      CAND_S,CAND_M ,MULNSN,NOINT  ,MARGE,
     3      I_MEM ,PROV_S ,PROV_M,IGAP0  ,CAND_A,
     4      NEDGE ,LEDGE  ,ITAB  ,DRAD   ,IGAP ,
     5      GAP_M ,GAP_M_L,GAPE  ,GAP_E_L,ADMSR,
     6      EDG_BISECTOR,VTX_BISECTOR,CAND_P, PROV_IDS,
     7      IFQ    ,CAND_FX ,CAND_FY,CAND_FZ,IFPEN,
     8      DGAPLOAD)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: PROV_IDS(2,NVSIZ)
      INTEGER I_MEM, IGAP0, NEDGE, NIN, ITAB(*), INACTI,IFQ
      INTEGER J_STOK,MULNSN,NOINT,IFORM,IGAP
      INTEGER IRECT(4,*),LEDGE(NLEDGE,*),ADMSR(4,*),CAND_S(*),CAND_M(*),II_STOK,
     .        CAND_A(*),IFPEN(*)
      INTEGER PROV_S(MVSIZ),PROV_M(MVSIZ)
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   MARGE,X(3,*), GAP_M(*), GAP_M_L(*), GAPE(*), GAP_E_L(*), 
     .   CAND_P(4,*),CAND_FX(4,*) ,CAND_FY(4,*),CAND_FZ(4,*)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD0,IAD,IADFIN,N,NE,EJ
      INTEGER I_STOK_FIRST,NINDX,INDEX(MVSIZ) 
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
      CALL I25PEN3_E2S( J_STOK,PROV_S ,PROV_M ,DRAD     ,IGAP0 ,
     .                  NEDGE ,LEDGE  ,MARGE  ,GAP_M    ,GAP_M_L ,
     .                  GAPE  ,GAP_E_L   ,IGAP ,X      ,IRECT    ,
     .                  PENE  ,ADMSR     ,EDG_BISECTOR ,VTX_BISECTOR,ITAB,
     .                  XREM_EDGE,E_RBUF_SIZE,NEDGE_REMOTE,DGAPLOAD)
C-----------------------------------------------
C SUPPRESSION DES ANCIENS CANDIDATS DEJA STOCKES (PENE INITIALE)
C-----------------------------------------------
C     IF(INACTI.EQ.5)THEN
        DO I=1,J_STOK
C         DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, PENE(I))

          IF(PENE(I).NE.ZERO)THEN
            N  = PROV_S(I)
            NE = PROV_M(I)

C           IF(N.GT.NEDGE) THEN
C numerotation tris precedent pour les noeuds non locaux (SPMD)
C              N = OLDNUM(N-NEDGE)+NEDGE
C              IF(N.EQ.NEDGE) N = NEDGE+NEDGEROLD+1
C           END IF

C           DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, N)
            IF(N.GT.NEDGE) THEN
C numerotation tris precedent pour les noeuds non locaux (SPMD)
C             DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, OLDNUM_EDGE(N-NEDGE))
              N = OLDNUM_EDGE(N-NEDGE)+NEDGE
C             DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, N)
              IF(N <= NEDGE) N = NEDGE+NEDGE_REMOTE_OLD+1
            END IF
C           DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, N)

            ASSERT(N > 0)
            J = CAND_A(N)
            DO WHILE(J.LE.CAND_A(N+1)-1)
              IF(CAND_M(J).EQ.NE)THEN
                PENE(I)=ZERO
                J=CAND_A(N+1)
C               DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, J)
              ELSE
                J=J+1
              ENDIF
            ENDDO
C           DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, PENE(I))

          ENDIF
        ENDDO
C     ENDIF
C-----------------------------------------------
      K_STOK = 0
      DO I=1,J_STOK
          IF(PENE(I).NE.ZERO) THEN
            K_STOK = K_STOK + 1
          END IF 
      ENDDO
      IF(K_STOK.EQ.0)RETURN
C
#include "lockon.inc"
      I_STOK = II_STOK
      IF(I_STOK+K_STOK.GT.MULNSN) THEN
            I_MEM = 2
#include "lockoff.inc"
            RETURN
      ENDIF
      II_STOK   = I_STOK + K_STOK
#include "lockoff.inc"
C     IF(INACTI==5)THEN
        DO I=1,J_STOK
          DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM,PENE(I))
          IF(PENE(I).NE.ZERO)THEN
            ASSERT(PROV_S(I) > 0)
            ASSERT(PROV_S(I) <= NEDGE + NEDGE_REMOTE)
C           DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM, I_STOK)
            I_STOK = I_STOK + 1
            CAND_S(I_STOK) = PROV_S(I)
            CAND_M(I_STOK) = PROV_M(I)
            CAND_P(1:4,I_STOK) = ZERO
            IF(IFQ > 0) THEN
               CAND_FX(1:4,I_STOK) = ZERO
               CAND_FY(1:4,I_STOK) = ZERO
               CAND_FZ(1:4,I_STOK) = ZERO
               IFPEN(I_STOK) = 0
            ENDIF
          ENDIF
          DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM,CAND_P(1,I_STOK))
          DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM,CAND_P(2,I_STOK))
          DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM,CAND_P(3,I_STOK))
          DEBUG_E2E(PROV_IDS(2,I)==D_ES.AND.PROV_IDS(1,I)==D_EM,CAND_P(4,I_STOK))
        ENDDO
C     ELSE
C       DO I=1,J_STOK
C         IF(PENE(I).NE.ZERO)THEN
C           I_STOK = I_STOK + 1
C           CAND_S(I_STOK) = PROV_S(I)
C           CAND_M(I_STOK) = PROV_M(I)
C         ENDIF
C       ENDDO
C     END IF
C-----------------------------------------------
      RETURN
      END


