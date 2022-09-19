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
Chd|  I10STO                        source/interfaces/intsort/i10sto.F
Chd|-- called by -----------
Chd|        I10TRI                        source/interfaces/intsort/i10tri.F
Chd|-- calls ---------------
Chd|        I7COR3T                       source/interfaces/int07/i7cor3t.F
Chd|        I7PEN3                        source/interfaces/intsort/i7pen3.F
Chd|====================================================================
      SUBROUTINE I10STO(
     1      J_STOK,IRECT ,X      ,NSV   ,II_STOK,
     2      CAND_N,CAND_E,NSN4   ,NOINT ,MARGE  ,
     3      I_MEM ,PROV_N,PROV_E ,CAND_A,ESHIFT ,
     4      NSN   ,OLDNUM,NSNROLD,IGAP  ,GAP    ,
     6      GAP_S ,GAP_M ,GAPMIN ,GAPMAX,NIN    )
C============================================================================
C  cette routine est appelee par : I10TRI(/int10/i10tri.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I7COR3T(/int7/i7cor3.F)
C                          I7PEN3(/int7/i7pen3.F)
C                          MY_LOCK(/spe/machine.F)
C                          MY_FREE
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,J_STOK,NSN4,NOINT,ESHIFT,NSN,NSNROLD,IGAP,NIN
      INTEGER IRECT(4,*),NSV(*),CAND_N(*),CAND_E(*),CAND_A(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ), OLDNUM(*),II_STOK
C     REAL
      my_real
     .   X(3,*), GAP_S(*), GAP_M(*),
     .   MARGE, GAP, GAPMIN, GAPMAX, BID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,N,NE,NN,IBID
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   PENE(MVSIZ), GAPV(MVSIZ), RBID
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
       IBID = 0
       RBID = ZERO
        CALL I7COR3T(J_STOK  ,X     ,IRECT ,NSV   ,PROV_E  ,
     1               PROV_N  ,IGAP  ,GAP   ,X1    ,X2     ,
     2               X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     3               Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     4               XI      ,YI    ,ZI    ,STIF  ,IX1    ,
     5               IX2     ,IX3   ,IX4   ,NSN   ,GAP_S  ,
     6               GAP_M   ,GAPV  ,GAPMAX,GAPMIN,BID    ,
     7               10      ,NIN   ,RBID  ,RBID  ,IBID   ,
     8               RBID    ,RBID  )
C-----------------------------------------------
        CALL I7PEN3(J_STOK ,MARGE ,X1    ,X2     ,X3   ,
     .               X4    ,Y1    ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4   ,IGAP  ,GAP    ,GAPV )
C-----------------------------------------------
        K_STOK = 0
        DO I=1,J_STOK
          IF (PENE(I)/=ZERO) THEN
            N  = PROV_N(I)
            IF(N>NSN) THEN
C numerotation tris precedent pour les noeuds non locaux (SPMD)
              N = OLDNUM(N-NSN)+NSN
              IF(N==NSN) N = NSN+NSNROLD+1
            END IF
Cel attention au shift
            NE = PROV_E(I)+ESHIFT
            DO J = CAND_A(N),CAND_A(N+1)-1
              IF (CAND_E(J)==NE) GOTO 100
            ENDDO
            K_STOK = K_STOK + 1
 100        CONTINUE
          ENDIF
        ENDDO
C
        IF(K_STOK==0)RETURN
C
#include "lockon.inc"
          I_STOK = II_STOK
          IF(I_STOK+K_STOK>NSN4) THEN
            I_MEM = 2
#include "lockoff.inc"
            RETURN
          ENDIF
          II_STOK   = I_STOK + K_STOK
#include "lockoff.inc"
C-----------------------------------------------
        DO I=1,J_STOK
          IF (PENE(I)/=ZERO) THEN
            N  = PROV_N(I)
            NN = N
            IF(N>NSN) THEN
C numerotation tris precedent pour les noeuds non locaux (SPMD)
              N = OLDNUM(N-NSN)+NSN
              IF(N==NSN) N = NSN+NSNROLD+1
            END IF
Cel attention au shift
            NE = PROV_E(I)+ESHIFT
            DO J = CAND_A(N),CAND_A(N+1)-1
              IF (CAND_E(J)==NE) GOTO 200
            ENDDO
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = NN
            CAND_E(I_STOK) = NE
 200        CONTINUE
          ENDIF
        ENDDO
C-----------------------------------------------
      RETURN
      END
