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
Chd|  I23STO                        source/interfaces/int23/i23sto.F
Chd|-- called by -----------
Chd|        I23TRIVOX                     source/interfaces/intsort/i23trivox.F
Chd|-- calls ---------------
Chd|        I23COR3T                      source/interfaces/int23/i23cor3t.F
Chd|        I7PEN3                        source/interfaces/intsort/i7pen3.F
Chd|====================================================================
      SUBROUTINE I23STO(
     1      J_STOK   ,IRECT  ,X        ,NSV     ,II_STOK,
     2      CAND_N ,CAND_E ,MULNSN     ,NOINT   ,MARGE  ,
     3      I_MEM  ,PROV_N ,PROV_E     ,ESHIFT  ,INACTI ,
     4      IGAP     ,GAP  ,GAP_S      ,GAP_M   ,GAPMIN ,
     5      GAPMAX ,CURV_MAX ,MSR      ,NSN     ,OLDNUM ,
     6      NSNROLD,CAND_A   ,IFPEN    ,CAND_P  )
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
      INTEGER I_MEM, NSN,NSNROLD,IGAP
      INTEGER J_STOK,MULNSN,NOINT,INACTI,ESHIFT
      INTEGER IRECT(4,*),CAND_N(*),CAND_E(*),CAND_A(*),NSV(*),MSR(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),IFPEN(*), OLDNUM(*),II_STOK
C     REAL
      my_real
     .        X(3,*), GAP_S(*), GAP_M(*),
     .        MARGE, GAP, GAPMIN, GAPMAX, CURV_MAX(*),
     .        CAND_P(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J,
     .        ME,N1,N2,N3,N4,M1,M2,M3,M4
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ),
     .   NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .   NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .   NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .   PENE(MVSIZ), GAPV(MVSIZ)
C-----------------------------------------------
        CALL I23COR3T(J_STOK ,X    ,IRECT ,PROV_E ,
     1               PROV_N  ,IGAP ,GAP   ,X1    ,X2     ,
     2               X3      ,X4   ,Y1    ,Y2	 ,Y3	 ,
     3               Y4      ,Z1   ,Z2    ,Z3	 ,Z4	 ,
     4               XI      ,YI   ,ZI    ,IX1   ,IX2    ,  
     5               IX3     ,IX4  ,NSN   ,GAP_S ,GAPV   ,
     6               GAPMAX  ,GAPMIN,CURV_MAX,NSV,MSR    ,
     7               GAP_M   )
C-----------------------------------------------
       CALL I7PEN3(J_STOK ,MARGE  ,X1    ,X2     ,X3   ,
     .               X4    ,Y1     ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2     ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI     ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4    ,IGAP  ,GAP    ,GAPV )
C-----------------------------------------------
C SUPPRESSION DES ANCIENS CANDIDATS DEJE STOCKES (PENE INITIALE)
C-----------------------------------------------
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
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK)  = 0
            CAND_P(I_STOK) = ZERO
          ENDIF
        ENDDO
        RETURN

C a refaire
c        DO I=1,J_STOK
c          IF(PENE(I)/=ZERO)THEN
c            I_STOK = I_STOK + 1
c            CAND_N(I_STOK) = PROV_N(I)
c            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
cC
c            N  = PROV_N(I)
c            NE = PROV_E(I)+ESHIFT
c            N1 = IRECTG(1,NE)
c            N2 = IRECTG(2,NE)
c            N3 = IRECTG(3,NE)
c            N4 = IRECTG(4,NE)
c	    ITAGP(N1)=1
c	    ITAGP(N2)=1
c	    ITAGP(N3)=1
c	    ITAGP(N4)=1
c            IF(N>NSN) THEN
cC numerotation tris precedent pour les noeuds non locaux (SPMD)
c              N = OLDNUM(N-NSN)+NSN
c              IF(N==NSN) N = NSN+NSNROLD+1
c            END IF
c            IFPEN(I_STOK)  = 0
c            CAND_P(I_STOK) = ZERO
cC
cC look for a previous contact w/neighbour
c            J = CAND_A(N)
c            DO WHILE(J<=CAND_A(N+1)-1)
c              ME=CAND_E(J)
c              IF(ME/=NE)THEN
c                M1 = IRECTG(1,ME)
c                M2 = IRECTG(2,ME)
c                M3 = IRECTG(3,ME)
c                M4 = IRECTG(4,ME)
c                IF((ITAGP(M1)/=0.AND.ITAGP(M2)/=0).OR.
c     .             (ITAGP(M2)/=0.AND.ITAGP(M3)/=0).OR.
c     .             (M4/=M3.AND.ITAGP(M3)/=0.AND.ITAGP(M4)/=0).OR.
c     .             (M4/=M3.AND.ITAGP(M4)/=0.AND.ITAGP(M1)/=0).OR.
c     .             (M4==M3.AND.ITAGP(M3)/=0.AND.ITAGP(M1)/=0))THEN
cC
cC retains information from 1 neighbor only (1st penetrated)
cCa doit etre refait / tri
c                  IF(ABS(IFPEN(J)) > ABS(IFPEN(I_STOK)))THEN
c                    IFPEN(I_STOK)  = IFPEN(J)
c                    CAND_P(I_STOK) = CAND_P(J)
c                 END IF
c                ENDIF
c        	J=J+1
c              END IF
c            ENDDO
c            ITAGP(N1)=0
c            ITAGP(N2)=0
c            ITAGP(N3)=0
c            ITAGP(N4)=0
c          ENDIF
c        ENDDO
C-----------------------------------------------
      RETURN
      END
