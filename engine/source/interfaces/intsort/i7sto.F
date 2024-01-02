Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I7STO                         source/interfaces/intsort/i7sto.F
Chd|-- called by -----------
Chd|        I7TRI                         source/interfaces/intsort/i7tri.F
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|-- calls ---------------
Chd|        I7COR3T                       source/interfaces/int07/i7cor3t.F
Chd|        I7PEN3                        source/interfaces/intsort/i7pen3.F
Chd|====================================================================
      SUBROUTINE I7STO(
     1      J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2      CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3      I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4      IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5      OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6      GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7      GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8      CAND_F ,DGAPLOAD)
C============================================================================
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NSN, NSNROLD,IGAP,NIN,INTTH,ITIED
      INTEGER J_STOK,MULNSN,NOINT,INACTI,IFQ,ESHIFT
      INTEGER IRECT(4,*),NSV(*),CAND_N(*),CAND_E(*),CAND_A(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),IFPEN(*), OLDNUM(*),
     *        II_STOK
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      my_real
     .        X(3,*), CAND_P(*), GAP_S(*), GAP_M(*),
     .        MARGE, GAP, GAPMIN, GAPMAX,CURV_MAX(*),
     .        GAP_S_L(*),GAP_M_L(*), CAND_F(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J,ITYPE
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), 
     .   PENE(MVSIZ), GAPV(MVSIZ)
      DATA ITYPE/7/
C-----------------------------------------------
        CALL I7COR3T(J_STOK  ,X    ,IRECT ,NSV   ,PROV_E  ,
     1               PROV_N  ,IGAP ,GAP   ,X1    ,X2      ,
     2               X3      ,X4   ,Y1    ,Y2    ,Y3      ,
     3               Y4      ,Z1   ,Z2    ,Z3    ,Z4      ,
     4               XI      ,YI   ,ZI    ,STIF  ,IX1     ,
     5               IX2     ,IX3  ,IX4   ,NSN   ,GAP_S   ,
     6               GAP_M   ,GAPV ,GAPMAX,GAPMIN,CURV_MAX,
     7               ITYPE   ,NIN  ,GAP_S_L,GAP_M_L,INTTH ,
     8               DRAD    ,DGAPLOAD)
C-----------------------------------------------
        CALL I7PEN3(J_STOK ,MARGE ,X1    ,X2     ,X3   ,
     .               X4    ,Y1    ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4   ,IGAP  ,GAP    ,GAPV )
C-----------------------------------------------
C SUPPRESSION DES ANCIENS CANDIDATS DEJE STOCKES (PENE INITIALE)
C-----------------------------------------------
       IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.
     .    ITIED/=0)THEN
            DO I=1,J_STOK
              IF(PENE(I)/=ZERO)THEN
                N  = PROV_N(I)
                NE = PROV_E(I)+ESHIFT
                IF(N>NSN) THEN
C numerotation tris precedent pour les noeuds non locaux (SPMD)
                  N = OLDNUM(N-NSN)+NSN
                  IF(N==NSN) N = NSN+NSNROLD+1
                END IF
                J = CAND_A(N)
                DO WHILE(J<=CAND_A(N+1)-1)
                  IF(CAND_E(J)==NE)THEN
                    PENE(I)=ZERO
                    J=CAND_A(N+1)
                  ELSE
                    J=J+1
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
        ENDIF
C-----------------------------------------------
        K_STOK = 0
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO) K_STOK = K_STOK + 1
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
C-----------------------------------------------
      IF(IFQ > 0 .AND. 
     .   (INACTI == 5 .OR. INACTI ==6 .OR. INACTI ==7).AND.ITIED /=0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK)  = 0
            CAND_P(I_STOK) = ZERO
            CAND_F(1:8,I_STOK)=ZERO
          ENDIF
        ENDDO
      ELSEIF(IFQ > 0 .AND. 
     .   (INACTI == 5 .OR. INACTI ==6 .OR. INACTI ==7))THEN
        DO I=1,J_STOK
          IF(PENE(I)/=0.0)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK)   = 0
            CAND_P(I_STOK) = ZERO
          ENDIF
        ENDDO
      ELSEIF(IFQ > 0..AND. ITIED /=0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK)  = 0
            CAND_F(1:8,I_STOK)=ZERO
          ENDIF
        ENDDO
      ELSEIF(IFQ > 0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK)  = 0
          ENDIF
        ENDDO
      ELSEIF((INACTI==5.OR.INACTI==6.OR.INACTI==7).AND.ITIED /= 0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            CAND_P(I_STOK) = ZERO
            CAND_F(1:8,I_STOK)=ZERO
          ENDIF
        ENDDO
      ELSEIF(INACTI==5.OR.INACTI==6.OR.INACTI==7)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            CAND_P(I_STOK) = ZERO
          ENDIF
        ENDDO
      ELSEIF(ITIED /= 0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            CAND_F(1:8,I_STOK)=ZERO
          ENDIF
        ENDDO
      ELSE
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
