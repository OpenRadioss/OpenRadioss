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
Chd|  I24STO                        source/interfaces/intsort/i24sto.F
Chd|-- called by -----------
Chd|        I24TRIVOX                     source/interfaces/intsort/i24trivox.F
Chd|-- calls ---------------
Chd|        I24COR3T                      source/interfaces/intsort/i24cor3t.F
Chd|        I24EDGT                       source/interfaces/intsort/i24sto.F
Chd|        I24PEN3                       source/interfaces/intsort/i24pen3.F
Chd|        I24S1S2                       source/interfaces/intsort/i24sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24STO(
     1      J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2      CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3      I_MEM ,PROV_N ,PROV_E,ESHIFT,V      ,
     4      NSN   ,GAP_S  ,GAP_M ,CURV_MAX,NIN  ,
     5      PENE_OLD,NBINFLG,MBINFLG,ILEV ,MSEGTYP,
     6      EDGE_L2,IEDGE,ISEADD ,ISEDGE ,CAND_T,itab,
     7      CAND_A,OLDNUM,NSNROLD,DGAPLOAD)
C============================================================================
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NSN, NIN,NBINFLG(*),MBINFLG(*),ILEV
      INTEGER J_STOK,MULNSN,NOINT,ESHIFT,IEDGE,NSNROLD
      INTEGER IRECT(4,*),NSV(*),CAND_N(*),CAND_E(*),CAND_T(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),II_STOK,MSEGTYP(*),ISEADD(*),
     .        ISEDGE(*) ,itab(*), CAND_A(*),OLDNUM(*)
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .        X(3,*), V(3,*), GAP_S(*), GAP_M(*),
     .        MARGE, CURV_MAX(*),PENE_OLD(5,NSN),EDGE_L2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J,ITYPE,ISH
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), 
     .   PENE(MVSIZ), GAPV(MVSIZ), GAPVE(MVSIZ), PENE_E(MVSIZ)
      DATA ITYPE/24/
C-----------------------------------------------
      CALL I24COR3T( J_STOK  ,X    ,IRECT   ,NSV   ,PROV_E  ,
     1               PROV_N  ,X1    ,X2      ,
     2               X3      ,X4   ,Y1      ,Y2    ,Y3      ,
     3               Y4      ,Z1   ,Z2      ,Z3    ,Z4      ,
     4               XI      ,YI   ,ZI      ,STIF  ,IX1     ,
     5               IX2     ,IX3  ,IX4     ,NSN   ,GAP_S   ,
     6               GAP_M   ,GAPV ,CURV_MAX,ITYPE ,NIN     ,
     7               V       ,PENE_OLD,GAPVE ,EDGE_L2,IEDGE ,
     8               DGAPLOAD)
C-----------------------------------------------
      CALL I24PEN3( J_STOK ,MARGE ,X1    ,X2     ,X3   ,
     .               X4    ,Y1    ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4   ,GAPV  ,GAPVE  ,PENE_E)
C-----------------------------------------------
      IF (ILEV==2) 
     .  CALL I24S1S2(J_STOK,NSN,ESHIFT,PROV_N,PROV_E,
     .               NBINFLG,MBINFLG,PENE)
      CALL I24EDGT(J_STOK ,NSN    ,ESHIFT ,PROV_N,PROV_E,
     .             MBINFLG,ISEADD ,ISEDGE ,PENE_E,IEDGE )

C-----------------------------------------------
C SUPPRESSION DES ANCIENS CANDIDATS DEJE STOCKES 
C-----------------------------------------------
      DO I=1,J_STOK
       IF(PENE(I)/=ZERO)THEN
         N  = PROV_N(I)
         NE = PROV_E(I)+ESHIFT
         IF(N>NSN)THEN
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

      K_STOK = 0
      DO I=1,J_STOK
          IF(PENE(I)+PENE_E(I)/=ZERO) THEN
           K_STOK = K_STOK + 1
           IF( MSEGTYP(PROV_E(I)+ESHIFT)>0) K_STOK = K_STOK + 1
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
      IF(IEDGE==0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            ISH=MSEGTYP(CAND_E(I_STOK))
            IF( ISH > 0 ) THEN
             I_STOK = I_STOK + 1
             CAND_N(I_STOK) = PROV_N(I)
             CAND_E(I_STOK) = ISH
            END IF
          ENDIF
        ENDDO
      ELSE
        DO I=1,J_STOK
          IF(PENE(I)+PENE_E(I) /= ZERO )THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            ISH=MSEGTYP(CAND_E(I_STOK))
            IF(PENE_E(I) == ZERO)THEN
               CAND_T(I_STOK) = 0 ! only node candidate
            ELSEIF(PENE(I) == ZERO)THEN
               CAND_T(I_STOK) = 2 ! only edge candidate
            ELSE
               CAND_T(I_STOK) = 1 ! edge and node candidate
            ENDIF
            IF( ISH > 0 ) THEN
             I_STOK = I_STOK + 1
             CAND_N(I_STOK) = PROV_N(I)
             CAND_E(I_STOK) = ISH
             IF(PENE_E(I) == ZERO)THEN
               CAND_T(I_STOK) = 0 ! only node candidate
             ELSEIF(PENE(I) == ZERO)THEN
               CAND_T(I_STOK) = 2 ! only edge candidate
             ELSE
               CAND_T(I_STOK) = 1 ! edge and node candidate
             ENDIF
            END IF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I24S1S2                       source/interfaces/intsort/i24sto.F
Chd|-- called by -----------
Chd|        I24STO                        source/interfaces/intsort/i24sto.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24S1S2(LLT  ,NSN  ,ESHIFT,PROV_N,PROV_E,
     .                   NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,NSN,ESHIFT,PROV_N(*),PROV_E(*),NBINFLG(*),MBINFLG(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NE,IMS1,IMS2,ISS1,ISS2
C     REAL
C-----------------------------------------------
      my_real
     .   PENE(*)
      INTEGER BITGET
      EXTERNAL BITGET
C=======================================================================
         DO I=1,LLT
           N  = PROV_N(I)
           NE = PROV_E(I)+ESHIFT
           IMS1 = BITGET(MBINFLG(NE),0)
           IMS2 = BITGET(MBINFLG(NE),1)
           IF(N <= NSN) THEN
             ISS1 = BITGET(NBINFLG(N),0)
             ISS2 = BITGET(NBINFLG(N),1)
           ELSE
             ISS1 = BITGET(IREM(I24IREMP+3,N-NSN),0) 
             ISS2 = BITGET(IREM(I24IREMP+3,N-NSN),1) 
           ENDIF
           IF((IMS1 == 1 .and. ISS1==1).or.
     .        (IMS2 == 1 .and. ISS2==1))THEN
             PENE(I)=ZERO
           ENDIF
         ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I24EDGT                       source/interfaces/intsort/i24sto.F
Chd|-- called by -----------
Chd|        I24STO                        source/interfaces/intsort/i24sto.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24EDGT(LLT  ,NSN  ,ESHIFT,PROV_N,PROV_E,
     .                   MBINFLG,ISEADD ,ISEDGE ,PENE_E,IEDGE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,NSN,ESHIFT,PROV_N(*),PROV_E(*),MBINFLG(*),
     .        ISEADD(*) ,ISEDGE(*),IEDGE
C     REAL
      my_real
     .    PENE_E(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NE,IME,NES,IAD
C     REAL
C-----------------------------------------------
      INTEGER BITGET
      EXTERNAL BITGET
C=======================================================================
      IF(IEDGE==0)THEN
        DO I=1,LLT
          PENE_E(I)=ZERO
        ENDDO
      ELSE
        DO I=1,LLT
           N  = PROV_N(I)
           NE = PROV_E(I)+ESHIFT
           IME = BITGET(MBINFLG(NE),8)
           IF(N <= NSN) THEN
             IAD = ISEADD(N)
             NES = ISEDGE(IAD)
           ELSE
c a faire !!!!!!!!!!!!!!!!!!!!!!!!!
             stop 987
           ENDIF
           IF(IME /= 1 .or. NES == 0) PENE_E(I)=ZERO
        ENDDO
      ENDIF

      RETURN
      END
