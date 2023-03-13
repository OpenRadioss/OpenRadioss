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
Chd|  I20STO                        source/interfaces/intsort/i20sto.F
Chd|-- called by -----------
Chd|        I20TRI                        source/interfaces/intsort/i20tri.F
Chd|-- calls ---------------
Chd|        I20COR3T                      source/interfaces/int20/i20cor3t.F
Chd|        I7PEN3                        source/interfaces/intsort/i7pen3.F
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20STO(
     1      J_STOK,IRECT  ,XA    ,NSV   ,II_STOK,
     2      CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3      I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4      IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5      OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6      GAP_M ,GAPMIN ,GAPMAX,CURV_MAX ,NIN ,
     7      GAP_SH,NBINFLG,MBINFLG,ISYM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
      INTEGER I_MEM, NSN, NSNROLD,IGAP,NIN,ISYM 
      INTEGER J_STOK,MULNSN,NOINT,INACTI,IFQ,ESHIFT
      INTEGER IRECT(4,*),NSV(*),CAND_N(*),CAND_E(*),CAND_A(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),IFPEN(*), OLDNUM(*),
     .        NBINFLG(*),MBINFLG(*),II_STOK
C     REAL
      my_real
     .        XA(3,*), CAND_P(*), GAP_S(*), GAP_M(*), GAP_SH(*),
     .        MARGE, GAP, GAPMIN, GAPMAX,CURV_MAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J,ISS1,ISS2,IMS1,IMS2
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), 
     .   PENE(MVSIZ), GAPV(MVSIZ)
      INTEGER BITGET
      EXTERNAL BITGET
C-----------------------------------------------
        CALL I20COR3T(J_STOK ,XA    ,IRECT ,NSV   ,PROV_E  ,
     1                PROV_N ,IGAP  ,GAP   ,X1    ,X2      ,
     2                X3     ,X4    ,Y1    ,Y2   ,Y3       ,
     3                Y4     ,Z1    ,Z2    ,Z3   ,Z4       ,
     4                XI     ,YI    ,ZI    ,STIF  ,IX1     ,
     5                IX2    ,IX3   ,IX4   ,NSN   ,GAP_S   ,
     6                GAP_M  ,GAPV  ,GAPMAX,GAPMIN,CURV_MAX, 
     7                NIN    ,GAP_SH)
C-----------------------------------------------
        CALL I7PEN3(J_STOK ,MARGE ,X1    ,X2     ,X3   ,
     .               X4    ,Y1    ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4   ,IGAP  ,GAP    ,GAPV )
C-----------------------------------------------
C SUPPRESSION DES CANDIDATS AUTOIMPACTES S1 OU S2
C-----------------------------------------------
       IF(ISYM==1)THEN
         DO I=1,J_STOK
           N  = PROV_N(I)
           NE = PROV_E(I)+ESHIFT
           IMS1 = BITGET(MBINFLG(NE),0)
           IMS2 = BITGET(MBINFLG(NE),1)
           IF(N <= NSN) THEN
             ISS1 = BITGET(NBINFLG(NSV(N)),0)
             ISS2 = BITGET(NBINFLG(NSV(N)),1)
           ELSE
             ISS1 = BITGET(NINT(XREM(12,N-NSN)),0) 
             ISS2 = BITGET(NINT(XREM(12,N-NSN)),1) 
           ENDIF
           IF((IMS1 == 0 .and. ISS1==0).or.
     .        (IMS2 == 0 .and. ISS2==0))THEN
             PENE(I)=ZERO
           ENDIF
         ENDDO
       ENDIF
C-----------------------------------------------
C SUPPRESSION DES ANCIENS CANDIDATS DEJA STOCKES (PENE INITIALE)
C-----------------------------------------------
       IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0)THEN
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
     .   (INACTI == 5 .OR. INACTI ==6 .OR. INACTI ==7))THEN
        DO I=1,J_STOK
          IF(PENE(I)/=0.0)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK)  = 0
            CAND_P(I_STOK) = ZERO
          ENDIF
        ENDDO
      ELSEIF(IFQ > 0)THEN
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)+ESHIFT
            IFPEN(I_STOK) = 0
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
Chd|====================================================================
Chd|  I20STO_EDGE                   source/interfaces/intsort/i20sto.F
Chd|-- called by -----------
Chd|        I20TRI_EDGE                   source/interfaces/intsort/i20tri.F
Chd|-- calls ---------------
Chd|        I20PEN3_EDGE                  source/interfaces/intsort/i20sto.F
Chd|====================================================================
      SUBROUTINE I20STO_EDGE(
     1      J_STOK,IXLINS,IXLINM,XA    ,II_STOKE,
     2      CAND_S,CAND_M,NSN4  ,NOINT ,TZINF ,
     3      I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM,
     4      CHAINE,NLINSA ,NIN   )
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
      INTEGER I_MEM, NLINSA, NIN
      INTEGER J_STOK,NSN4,NOINT
      INTEGER IXLINS(2,*),IXLINM(2,*),CAND_S(*),CAND_M(*),ADDCM(*),
     .        CHAINE(2,*)
      INTEGER PROV_S(MVSIZ),PROV_M(MVSIZ),ESHIFT,II_STOKE
C     REAL
      my_real
     .   XA(3,*),TZINF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD0,IAD,IADFIN
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
        CALL I20PEN3_EDGE(J_STOK ,PROV_S,PROV_M,TZINF ,XA    ,
     .               IXLINS,IXLINM,PENE  ,NLINSA ,NIN   )
C-----------------------------------------------
C il faut un lock sur toute la boucle (modification de chaine)
#include "lockon.inc"
        K_STOK = 0
C-----------------------------------------------
C elimination des couples deja trouves dans 1 boite precedente
C-----------------------------------------------
        I_STOK = II_STOKE
        DO I=1,J_STOK
          IF(PENE(I)>ZERO)THEN
            IAD=ADDCM(PROV_M(I))
            J=0
            DO WHILE(IAD/=0.AND.J<NSN4)
              J=J+1
              IF(CHAINE(1,IAD)==PROV_S(I))THEN
                PENE(I) = ZERO
                IAD=0
              ELSE
                IAD0=IAD
                IAD=CHAINE(2,IAD)
              ENDIF
            ENDDO
            IF(PENE(I)>ZERO)THEN
              K_STOK = K_STOK + 1
                IADFIN=II_STOKE+1
                IF(IADFIN>NSN4) THEN
                  I_MEM = 2
#include "lockoff.inc"
                  RETURN
                ENDIF
                II_STOKE   = IADFIN
              CHAINE(1,IADFIN)=PROV_S(I)
              CHAINE(2,IADFIN)=0
              IF(ADDCM(PROV_M(I))==0)THEN
                ADDCM(PROV_M(I))=IADFIN
              ELSE
                CHAINE(2,IAD0)=IADFIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        IF(K_STOK==0) THEN
#include "lockoff.inc"
          RETURN
        ENDIF
C-----------------------------------------------
        DO 200 I=1,J_STOK
          IF(PENE(I)>ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_S(I_STOK) = PROV_S(I)
            CAND_M(I_STOK) = PROV_M(I)+ESHIFT
          ENDIF
 200    CONTINUE
C
#include "lockoff.inc"
C
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I20PEN3_EDGE                  source/interfaces/intsort/i20sto.F
Chd|-- called by -----------
Chd|        I20STO_EDGE                   source/interfaces/intsort/i20sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20PEN3_EDGE(JLT   ,CAND_N,CAND_E,GAP   ,XA    ,
     .                        IXLINS,IXLINM,PENE  ,NLINSA,NIN   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NLINSA, NIN
      INTEGER IXLINS(2,*), IXLINM(2,*),CAND_N(*),CAND_E(*)
      my_real
     .     GAP
      my_real
     .     XA(3,*), PENE(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG,N1,N2,M1,M2,NI,L
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XXA,XXB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,
     .     GAP2, X11, X12, X13, X21, X22, X23,
     .     XMAX1,YMAX1,ZMAX1,XMAX2,YMAX2,ZMAX2,
     .     XMIN1,YMIN1,ZMIN1,XMIN2,YMIN2,ZMIN2,DD
C-----------------------------------------------
       GAP2=GAP*GAP
C--------------------------------------------------------
C  
C--------------------------------------------------------
C       F = [A*X1+(1-A)*X2-B*X3-(1-B)*X4]^2 + [..Y..]^2 + [..Z..]^2
C       DF/DA = 0 = (X1-X2)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DA = 0 = A(X1-X2)^2 +X2-X4 + B(X1-X2)(X4-X3))+...
C       DF/DA = 0 = A[(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2] 
C                 + B[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4) 
C       DF/DB = 0 = (X4-X3)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DB = 0 = B[(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2] 
C                 + A[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4) 
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XM2 = [(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       XB = (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4)
C       A XS2 + B XSM +   XA = 0
C       A XSM + B XM2 +   XB = 0
C
C       A = -(XA + B XSM)/XS2
C       -(XA + B XSM)*XSM + B XM2*XS2 +   XB*XS2 = 0
C       -B XSM*XSM + B XM2*XS2 +   XB*XS2-XA*XSM  = 0
C       B*(XM2*XS2 - XSM*XSM) = -XB*XS2+XA*XSM  
C       B = (XA*XSM-XB*XS2) / (XM2*XS2 - XSM*XSM)
C       A = (XB*XSM-XA*XM2) / (XM2*XS2 - XSM*XSM)
C--------------------------------------------------------
      IF(IMACH /= 3)THEN
        DO I=1,JLT
          N1=IXLINS(1,CAND_N(I))
          N2=IXLINS(2,CAND_N(I))
          M1=IXLINM(1,CAND_E(I))
          M2=IXLINM(2,CAND_E(I))

c         calcul d'un minorant de la distance

          XMAX1 = MAX(XA(1,N1),XA(1,N2))
          YMAX1 = MAX(XA(2,N1),XA(2,N2))
          ZMAX1 = MAX(XA(3,N1),XA(3,N2))
          XMAX2 = MAX(XA(1,M1),XA(1,M2))
          YMAX2 = MAX(XA(2,M1),XA(2,M2))
          ZMAX2 = MAX(XA(3,M1),XA(3,M2))
          XMIN1 = MIN(XA(1,N1),XA(1,N2))
          YMIN1 = MIN(XA(2,N1),XA(2,N2))
          ZMIN1 = MIN(XA(3,N1),XA(3,N2))
          XMIN2 = MIN(XA(1,M1),XA(1,M2))
          YMIN2 = MIN(XA(2,M1),XA(2,M2))
          ZMIN2 = MIN(XA(3,M1),XA(3,M2))
          DD = MAX(XMIN1-XMAX2,YMIN1-YMAX2,ZMIN1-ZMAX2,
     .             XMIN2-XMAX1,YMIN2-YMAX1,ZMIN2-ZMAX1)
          IF(DD > GAP)THEN
            PENE(I) = ZERO
            CYCLE
          ENDIF

c         calcul de la distance^2

          XS12 = XA(1,N2)-XA(1,N1)
          YS12 = XA(2,N2)-XA(2,N1)
          ZS12 = XA(3,N2)-XA(3,N1)
          XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
          XM12 = XA(1,M2)-XA(1,M1)
          YM12 = XA(2,M2)-XA(2,M1)
          ZM12 = XA(3,M2)-XA(3,M1)
          XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
          XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
          XS2M2 = XA(1,M2)-XA(1,N2)
          YS2M2 = XA(2,M2)-XA(2,N2)
          ZS2M2 = XA(3,M2)-XA(3,N2)
          XXA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
          XXB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
          DET = XM2*XS2 - XSM*XSM
          DET = MAX(EM20,DET)

          ALM = (XXA*XSM-XXB*XS2) / DET
          XS2 = MAX(XS2,EM20)
          XM2 = MAX(XM2,EM20)
          ALM=MIN(ONE,MAX(ZERO,ALM))
          ALS = -(XXA + ALM*XSM) / XS2
          ALS=MIN(ONE,MAX(ZERO,ALS))
          ALM = -(XXB + ALS*XSM) / XM2
          ALM=MIN(ONE,MAX(ZERO,ALM))

C         PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL

          XX =  ALS*XA(1,N1) + (ONE-ALS)*XA(1,N2)
     .        - ALM*XA(1,M1) - (ONE-ALM)*XA(1,M2)
          YY =  ALS*XA(2,N1) + (ONE-ALS)*XA(2,N2)
     .        - ALM*XA(2,M1) - (ONE-ALM)*XA(2,M2)
          ZZ =  ALS*XA(3,N1) + (ONE-ALS)*XA(3,N2)
     .        - ALM*XA(3,M1) - (ONE-ALM)*XA(3,M2)
          PENE(I) = GAP2 - XX*XX - YY*YY - ZZ*ZZ
C
        ENDDO
      ELSE
C
        DO I=1,JLT
          L = CAND_N(I)
          IF(L<=NLINSA) THEN
            N1=IXLINS(1,CAND_N(I))
            N2=IXLINS(2,CAND_N(I))
            X11 = XA(1,N1)
            X12 = XA(2,N1)
            X13 = XA(3,N1)
            X21 = XA(1,N2)
            X22 = XA(2,N2)
            X23 = XA(3,N2)
          ELSE
            NI = L - NLINSA
            X11 = XREM(2,NI)
            X12 = XREM(3,NI)
            X13 = XREM(4,NI)
            X21 = XREM(10,NI)
            X22 = XREM(11,NI)
            X23 = XREM(12,NI)
          END IF
          M1=IXLINM(1,CAND_E(I))
          M2=IXLINM(2,CAND_E(I))

c         calcul d'un minorant de la distance

          XMAX1 = MAX(X11,X21)
          YMAX1 = MAX(X12,X22)
          ZMAX1 = MAX(X13,X23)
          XMAX2 = MAX(XA(1,M1),XA(1,M2))
          YMAX2 = MAX(XA(2,M1),XA(2,M2))
          ZMAX2 = MAX(XA(3,M1),XA(3,M2))
          XMIN1 = MIN(X11,X21)
          YMIN1 = MIN(X12,X22)
          ZMIN1 = MIN(X13,X23)
          XMIN2 = MIN(XA(1,M1),XA(1,M2))
          YMIN2 = MIN(XA(2,M1),XA(2,M2))
          ZMIN2 = MIN(XA(3,M1),XA(3,M2))
          DD = MAX(XMIN1-XMAX2,YMIN1-YMAX2,ZMIN1-ZMAX2,
     .             XMIN2-XMAX1,YMIN2-YMAX1,ZMIN2-ZMAX1)
          IF(DD > GAP)THEN
            PENE(I) = ZERO
            CYCLE
          ENDIF

c         calcul de la distance^2

          XS12 = X21-X11
          YS12 = X22-X12
          ZS12 = X23-X13
          XS2M2 = XA(1,M2)-X21
          YS2M2 = XA(2,M2)-X22
          ZS2M2 = XA(3,M2)-X23
          XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
          XM12 = XA(1,M2)-XA(1,M1)
          YM12 = XA(2,M2)-XA(2,M1)
          ZM12 = XA(3,M2)-XA(3,M1)
          XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
          XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
          XXA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
          XXB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
          DET = XM2*XS2 - XSM*XSM
          DET = MAX(EM20,DET)
C
          ALS = (XXB*XSM-XXA*XM2) / DET
          ALM = (XXA*XSM-XXB*XS2) / DET
          XS2 = MAX(XS2,EM20)
          XM2 = MAX(XM2,EM20)
          IF(ALM<ZERO)THEN
            ALM = ZERO
            ALS = -XXA / XS2
          ELSEIF(ALM>ONE)THEN
            ALM = ONE
            ALS = -(XXA + XSM) / XS2
          ENDIF
C
          IF(ALS<ZERO)THEN
            ALS = ZERO
            ALM = -XXB / XM2
          ELSEIF(ALS>ONE)THEN
            ALS = ONE
            ALM = -(XXB + XSM) / XM2
          ENDIF

          ALM = MIN(ONE,ALM)
          ALM = MAX(ZERO,ALM)

C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL

          XX =  ALS*X11 + (ONE-ALS)*X21
     .        - ALM*XA(1,M1) - (ONE-ALM)*XA(1,M2)
          YY =  ALS*X12 + (ONE-ALS)*X22
     .        - ALM*XA(2,M1) - (ONE-ALM)*XA(2,M2)
          ZZ =  ALS*X13 + (ONE-ALS)*X23
     .        - ALM*XA(3,M1) - (ONE-ALM)*XA(3,M2)
          PENE(I) = GAP2- XX*XX - YY*YY - ZZ*ZZ
C
        END DO
      END IF
C
      RETURN
      END
C===============================================================================

Chd|====================================================================
Chd|  BITGET                        source/interfaces/intsort/i20sto.F
Chd|-- called by -----------
Chd|        I11FOR3                       source/interfaces/int11/i11for3.F
Chd|        I20DST3                       source/interfaces/int20/i20dst3.F
Chd|        I20STO                        source/interfaces/intsort/i20sto.F
Chd|        I24DST3E                      source/interfaces/int24/i24dst3e.F
Chd|        I24EDGT                       source/interfaces/intsort/i24sto.F
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|        I24S1S2                       source/interfaces/intsort/i24sto.F
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|        I25FOR3E                      source/interfaces/int25/i25for3e.F
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|        I25S1S2                       source/interfaces/intsort/i25sto.F
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|        I7FOR3                        source/interfaces/int07/i7for3.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION BITGET(I,N)
      INTEGER I,N
      INTEGER S,I2P(0:12)! limite a 23 (cast reel pour spmd)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      BITGET = S - (S/2)*2
      RETURN 
      END
      
