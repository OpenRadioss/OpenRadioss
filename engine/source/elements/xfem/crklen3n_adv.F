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
Chd|  CRKLEN3N_ADV                  source/elements/xfem/crklen3n_adv.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRKLEN3N_ADV(
     .           NEL       ,NFT      ,ILAY     ,NLAY     ,IXTG      ,
     .           CRKLEN    ,ELCRKINI ,IEL_CRKTG,DIR1     ,DIR2      ,
     .           NODEDGE   ,CRKEDGE  ,XEDGE3N  ,NGL      ,XL2       ,
     .           XL3       ,YL2      ,YL3      ,ALDT     )
c-----------------------------------------------
C crack advancement, shells 3N
c-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,ILAY,NLAY
      INTEGER IXTG(NIXTG,*),NGL(NEL),IEL_CRKTG(*),ELCRKINI(NLAY,*),
     . NODEDGE(2,*),XEDGE3N(3,*)
      my_real DIR1(NLAY,NEL),DIR2(NLAY,NEL),CRKLEN(NEL),ALDT(NEL)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      my_real, DIMENSION(NEL) :: XL2,YL2,XL3,YL3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IR,p1,p2,NEWCRK,IED,IED1,IED2,FAC,OK,ICRK,
     . NOD1,NOD2,ELCRK,ELCRKTG,IEDGE,ICUT
      INTEGER JCT(NEL),EDGEL(3,NEL),ELTIP(NEL),TIP(NEL)
      INTEGER DD(3),D(6),ISIGN(3),N(3),IENR(3),NN(3),INV(2)
C----------
      my_real, DIMENSION(NEL)   ::  XL1,YL1
      my_real, DIMENSION(2,NEL) ::  XIN,YIN
      my_real, DIMENSION(3,NEL) ::  XXL,YYL,LEN
      my_real BETA0(3,NEL),XN(3),YN(3),ZN(3),XMI(2),YMI(2)
      my_real BETA,XINT,YINT,BMIN,BMAX,X10,Y10,Z10,X20,Y20,Z20,
     .   M12,MM,CROSS1,CROSS12,XINT0,YINT0,DIR11,DIR22
c
      DATA d/1,2,2,3,1,3/
      DATA dd/2,3,1/
      PARAMETER (BMIN = 0.01, BMAX = 0.99)
C=======================================================================
      NEWCRK = 0
      DO I=1,NEL
        JCT(I) = 0
        IF (ELCRKINI(ILAY,I) == 5) THEN       ! avancement de fissure
          NEWCRK = NEWCRK + 1
          JCT(NEWCRK) = I
          ELCRKINI(ILAY,I) = 2                ! reset pour l avancement
        ELSEIF (ELCRKINI(ILAY,I) == -5) THEN  ! initialisation de fissure
          CRKLEN(I) = ALDT(I)
          ELCRKINI(ILAY,I) = -1               ! reset pour initialisation
        ENDIF
      ENDDO
      IF (NEWCRK == 0) RETURN
C---
      DO I=1,NEL
        BETA0(1:3,I) = ZERO
        TIP(I) = 0
        EDGEL(1,I)=0
        EDGEL(2,I)=0
        EDGEL(3,I)=0
        XIN(1,I) = ZERO  ! first inters point in local skew
        YIN(1,I) = ZERO
        XIN(2,I) = ZERO  ! second inters point in local skew
        YIN(2,I) = ZERO
      ENDDO
C
      INV(1) = 2
      INV(2) = 1
C
C---
C     advance crack inside uncut elements in the layer
C---
      DO IR=1,NEWCRK ! loop over elements with advanving cracks
        I = JCT(IR)
        ELCRKTG = IEL_CRKTG(I+NFT)
        OK   = 0
        ICUT = 0
        IED  = 0
        DO K=1,3
          IEDGE = XEDGE3N(K,ELCRKTG)
          ICUT  = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
          NOD1  = NODEDGE(1,IEDGE)
          NOD2  = NODEDGE(2,IEDGE)
          IF (NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I)) THEN
            p1 = K
            p2 = dd(K)
          ELSE IF (NOD2 == IXTG(K+1,I) .and. NOD1 == IXTG(dd(K)+1,I)) THEN
            p1 = dd(K)
            p2 = K
          ENDIF
          IF (ICUT == 1) THEN
            OK = OK + 1
            IED = K
c           tag
            ICRK  = CRKEDGE(ILAY)%EDGEICRK(IEDGE)
            EXIT
          ENDIF !  IF(ICUT == 1)THEN
        ENDDO !  DO K=1,4
C---
        IF (OK /= 1) THEN
          WRITE(IOUT,*) 'ERROR IN ADVANCING CRACK --- CHECK CRACK TIP'
          CALL ARRET(2)           
        ENDIF
C---
        EDGEL(IED,I) = 1
        IEDGE = XEDGE3N(IED,ELCRKTG)
        TIP(I) = CRKEDGE(ILAY)%EDGETIP(1,IEDGE)
C---
      END DO !  DO IR=1,NEWCRK
C----------------------------------------------------------------------
c     local coords
C----------------------------------------------------------------------
      DO I=1,NEL
       XL1(I) = ZERO
       YL1(I) = ZERO
       XXL(1,I)=XL1(I)
       YYL(1,I)=YL1(I)
       XXL(2,I)=XL2(I)
       YYL(2,I)=YL2(I)
       XXL(3,I)=XL3(I)
       YYL(3,I)=YL3(I)
      ENDDO
C---
      DO I=1,NEL
        LEN(1,I) = (XL2(I)-XL1(I))*(XL2(I)-XL1(I))
     .           + (YL2(I)-YL1(I))*(YL2(I)-YL1(I))
        LEN(2,I) = (XL3(I)-XL2(I))*(XL3(I)-XL2(I))
     .           + (YL3(I)-YL2(I))*(YL3(I)-YL2(I))
        LEN(3,I) = (XL1(I)-XL3(I))*(XL1(I)-XL3(I))
     .           + (YL1(I)-YL3(I))*(YL1(I)-YL3(I))
      ENDDO
C------------------------------------------------
C - intersections -
C------------------------------------------------
      DO IR=1,NEWCRK
        I=JCT(IR)
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK   = ELCRKTG + ECRKXFEC
        IED1 = 0
        IED2 = 0
        DO K=1,3
          IF(EDGEL(K,I) > 0)THEN
            IED1 = EDGEL(K,I)
            IED2 = INV(IED1)
            EXIT
           END IF
        END DO
        DO K=1,3
          IEDGE = XEDGE3N(K,ELCRKTG)
          IF (IEDGE > 0 .and. EDGEL(K,I) == 1) THEN
            ICUT = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
            IF (ICUT == 1) THEN
              BETA = CRKEDGE(ILAY)%RATIO(IEDGE)
C
              IF (BETA > ONE .or. BETA == ZERO) THEN
                WRITE(*,*) 'ERROR NEGATIV BETA, NO INTERSECTION!'
                CALL ARRET(2)
              ENDIF
C
              NOD1 = NODEDGE(1,IEDGE)
              NOD2 = NODEDGE(2,IEDGE)
              IF (NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I)) THEN
                p1 = K
                p2 = dd(K)
              ELSEIF (NOD2 == IXTG(K+1,I).and.NOD1==IXTG(dd(K)+1,I)) THEN
                p1 = dd(K)
                p2 = K
              ENDIF
              X10 = XXL(p1,I)
              Y10 = YYL(p1,I)
              X20 = XXL(p2,I)
              Y20 = YYL(p2,I)
C
              XINT = X10+BETA*(X20-X10)
              YINT = Y10+BETA*(Y20-Y10)
              XIN(IED1,I) = XINT
              YIN(IED1,I) = YINT
            ENDIF
          ENDIF
        ENDDO
C---
        IF (IED1 == 0 .or. IED2 == 0) GOTO 130
        XINT0 = XIN(IED1,I)
        YINT0 = YIN(IED1,I)
C---
        DIR11 = -DIR2(ILAY,I)
        DIR22 =  DIR1(ILAY,I)
C---
        IF (DIR11 == ZERO) THEN
          DO 140 K=1,3
            XINT = ZERO
            YINT = ZERO
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK   = ELCRKTG + ECRKXFEC
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IF(NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I))THEN
              p1 = K
              p2 = dd(K)
            ELSE IF(NOD2 == IXTG(K+1,I).and.NOD1==IXTG(dd(K)+1,I))THEN
              p1 = dd(K)
              p2 = K
            ENDIF
C
            IF (EDGEL(K,I) == IED1)     GOTO 140
            IF (XXL(p1,I) == XXL(p2,I)) GOTO 140 ! no inters (parallel to dir 2)
            M12 =  XXL(p2,I)-XXL(p1,I)
            M12 = (YYL(p2,I)-YYL(p1,I))/M12
            XINT = XINT0
            YINT = YYL(p1,I)+M12*(XINT-XXL(p1,I))
            CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
            IF (CROSS12 > ZERO) GOTO 140
c
            CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
            BETA   = SQRT(CROSS1 / LEN(K,I))
            BETA   = MAX(BETA, BMIN)
            BETA   = MIN(BETA, BMAX)
            BETA0(K,I) = BETA
C
            XIN(IED2,I) = XINT
            YIN(IED2,I) = YINT
            EDGEL(K,I) = IED2
            EXIT
 140    CONTINUE
        ELSEIF(DIR22 == ZERO)THEN
          DO 150 K=1,3
            XINT = ZERO
            YINT = ZERO
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK   = ELCRKTG + ECRKXFEC
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IF(NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I))THEN
              p1 = K
              p2 = dd(K)
            ELSE IF(NOD2 == IXTG(K+1,I).and.NOD1==IXTG(dd(K)+1,I))THEN
              p1 = dd(K)
              p2 = K
            ENDIF
C
            IF (EDGEL(K,I) == IED1)     GOTO 150
            IF (YYL(p1,I) == YYL(p2,I)) GOTO 150 ! no inters (parallel to dir 2)
            M12 =  YYL(p2,I)-YYL(p1,I)
            M12 = (XXL(p2,I)-XXL(p1,I))/M12
            YINT = YINT0
            XINT = XXL(p1,I)+M12*(YINT-YYL(p1,I))
            CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
            IF (CROSS12 > ZERO) GOTO 150
C
            CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
            BETA   = SQRT(CROSS1 / LEN(K,I))
            BETA   = MAX(BETA, BMIN)
            BETA   = MIN(BETA, BMAX)
            BETA0(K,I) = BETA
C
            XIN(IED2,I) = XINT
            YIN(IED2,I) = YINT
            EDGEL(K,I) = IED2
            EXIT
 150    CONTINUE
        ELSEIF(DIR11 /= ZERO .AND. DIR22 /= ZERO)THEN
          DO 160 K=1,3
            XINT = ZERO
            YINT = ZERO
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK   = ELCRKTG + ECRKXFEC
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IF (NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I))THEN
              p1 = K
              p2 = dd(K)
            ELSE IF (NOD2 == IXTG(K+1,I).and.NOD1==IXTG(dd(K)+1,I))THEN
              p1 = dd(K)
              p2 = K
            ENDIF
C
            IF (EDGEL(K,I) == IED1) GOTO 160
            IF (XXL(p1,I) == XXL(p2,I)) THEN
              MM   =  DIR22/DIR11
              XINT = XXL(p1,I)   !  or = XXL(p2,I)
              YINT = YINT0+MM*(XINT-XINT0)
              CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                  (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
              IF (CROSS12 > ZERO) GOTO 160
C
              CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
              BETA   = SQRT(CROSS1 / LEN(K,I))
              BETA   = MAX(BETA, BMIN)
              BETA   = MIN(BETA, BMAX)
              BETA0(K,I) = BETA
C
              XIN(IED2,I) = XINT
              YIN(IED2,I) = YINT
              EDGEL(K,I) = IED2
              EXIT
           ELSE
              MM  =  DIR22/DIR11
              M12 =  XXL(p2,I)-XXL(p1,I)
              M12 = (YYL(p2,I)-YYL(p1,I))/M12
              IF (MM == M12) GOTO 160  ! no inters (parallel to dir 2)
              XINT = (YINT0-YYL(p1,I)+M12*XXL(p1,I)-MM*XINT0)/(M12-MM)
              YINT = YINT0+MM*(XINT-XINT0)
              CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                  (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
              IF (CROSS12 > ZERO) GOTO 160
C
              CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
              BETA   = SQRT(CROSS1 / LEN(K,I))
              BETA   = MAX(BETA, BMIN)
              BETA   = MIN(BETA, BMAX)
              BETA0(K,I) = BETA
C
              XIN(IED2,I) = XINT
              YIN(IED2,I) = YINT
              EDGEL(K,I) = IED2
              EXIT
           ENDIF
 160    CONTINUE
        ENDIF
 130    CONTINUE
      ENDDO
C----------------------------------------------------------------------
C     check for getting both intersections
C
      DO IR=1,NEWCRK
        I = JCT(IR)
        FAC = 0
        DO K=1,3
          IF (EDGEL(K,I)==1 .or. EDGEL(K,I)==2) FAC=FAC+1
        ENDDO
        IF (FAC /= 2) THEN 
          WRITE(IOUT,*) 'ERROR IN ADVANCING CRACK.NO CUT EDGES'
          CALL ARRET(2)           
        ENDIF
        CRKLEN(I) = SQRT((XIN(2,I) - XIN(1,I))**2 + (YIN(2,I) - YIN(1,I))**2)
      ENDDO
c-----------
      RETURN
      END
