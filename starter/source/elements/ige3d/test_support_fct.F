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
Chd|  TEST_SUPPORT_FCT              source/elements/ige3d/test_support_fct.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE TEST_SUPPORT_FCT(IXIG3D, KXIG3D, KNOTLOCPC, DEGTANG1, DEGTANG2, DIR,
     .                            TAB_ELCUT, L_TAB_ELCUT,
     .                            TAB_COINKNOT,L_TAB_COINKNOT,
     .                            TAB_FCTCUT,L_TAB_FCTCUT,DECALGEO,FLAG)
C----------------------------------------------------------------------
C   ROUTINE QUI PERMET DE METTRE DE COTE TOUTES LES FONCTIONS D'ONE 
C   ENSEMBLE D'ELEMENTS DU PATCH QUI VONT POTENTIELLEMENT ETRE MODIFIEES 
C   EN INSERANT LE KNOT DE LA MESHSURF
C   CETTE ROUTINE UTILISE ONE ALGORITHME DE DETECTION D'INCLUSION
C   DE SEGMENTS DANS UNE SURFACE (LEGEREMENT MODIFIE)
C----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXIG3D(*),KXIG3D(NIXIG3D,*),DEGTANG1,DEGTANG2,DIR,DECALGEO,
     .        TAB_ELCUT(L_TAB_ELCUT),L_TAB_ELCUT,L_TAB_COINKNOT,
     .        TAB_FCTCUT(L_TAB_FCTCUT),L_TAB_FCTCUT,FLAG
      my_real KNOTLOCPC(DEG_MAX,3,*),TAB_COINKNOT(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,DIRTANG1,DIRTANG2,
     .        IEL,INTERSEC,
     .        WORK(70000),SIZ_LIST_FCTTOT,IDFCT,IOUT
      my_real DET, T1, T2, XA(5),YA(5),COIN(2,2),
     .        XB, YB, XC, YC, XD, YD, TOL
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX, LIST_FCTTOT, LIST_FCTTRI
C-----------------------------------------------
c
      L_TAB_FCTCUT = 0
      TOL = EM06
c
      IF(DIR.EQ.1) THEN
        DIRTANG1 = 2
        DIRTANG2 = 3
      ELSEIF(DIR.EQ.2) THEN
        DIRTANG1 = 3
        DIRTANG2 = 1
      ELSEIF(DIR.EQ.3) THEN
        DIRTANG1 = 1
        DIRTANG2 = 2
      ENDIF
c
C----------------------------------------------------------------------
c  on liste toutes les fonctions a traiter et les trier pour
c  ne pas traiter deux fois la meme fonction
C----------------------------------------------------------------------
c
      SIZ_LIST_FCTTOT = L_TAB_ELCUT*KXIG3D(3,TAB_ELCUT(1))
      ALLOCATE(LIST_FCTTOT(SIZ_LIST_FCTTOT))
      ALLOCATE(LIST_FCTTRI(SIZ_LIST_FCTTOT))
      LIST_FCTTOT(:) = EP06
c
      DO I=1,L_TAB_ELCUT
        IEL=TAB_ELCUT(I)
        DO J=1,KXIG3D(3,IEL)
          LIST_FCTTOT((I-1)*KXIG3D(3,IEL)+J) = IXIG3D(KXIG3D(4,IEL)+J-1)
        ENDDO
      ENDDO
c
      ALLOCATE(INDEX(2*SIZ_LIST_FCTTOT))
      CALL MY_ORDERS(0, WORK, LIST_FCTTOT, INDEX, SIZ_LIST_FCTTOT , 1)
c
      DO I=1,SIZ_LIST_FCTTOT
        LIST_FCTTRI(I)=LIST_FCTTOT(INDEX(I))
      ENDDO
c
      DEALLOCATE(LIST_FCTTOT)
      DEALLOCATE(INDEX)
c
      COIN(1,1) = MINVAL(TAB_COINKNOT(1,1:(L_TAB_COINKNOT)))
      COIN(2,1) = MINVAL(TAB_COINKNOT(2,1:(L_TAB_COINKNOT)))
      COIN(1,2) = MAXVAL(TAB_COINKNOT(1,1:(L_TAB_COINKNOT)))
      COIN(2,2) = MAXVAL(TAB_COINKNOT(2,1:(L_TAB_COINKNOT)))
c
      DO I=1,SIZ_LIST_FCTTOT
c
        IF(I.NE.1) THEN
          IF(LIST_FCTTRI(I-1).EQ.LIST_FCTTRI(I)) CYCLE
        ENDIF
c
        IDFCT = LIST_FCTTRI(I)
        IOUT=0
c
C----------------------------------------------------------------------
c  CREATION DES VARIABLES DE TRAVAIL : COIN DES ETENDUES KNOT DE LA FONCTION
c  A TESTER
C----------------------------------------------------------------------
c
        XA(1) = KNOTLOCPC(1,DIRTANG1,DECALGEO+IDFCT) + TOL
        XA(2) = KNOTLOCPC(DEGTANG1+1,DIRTANG1,DECALGEO+IDFCT) - TOL
        XA(3) = KNOTLOCPC(DEGTANG1+1,DIRTANG1,DECALGEO+IDFCT) - TOL
        XA(4) = KNOTLOCPC(1,DIRTANG1,DECALGEO+IDFCT) + TOL
        XA(5) = XA(1)
c
        YA(1) = KNOTLOCPC(1,DIRTANG2,DECALGEO+IDFCT) + TOL
        YA(2) = KNOTLOCPC(1,DIRTANG2,DECALGEO+IDFCT) + TOL
        YA(3) = KNOTLOCPC(DEGTANG2+1,DIRTANG2,DECALGEO+IDFCT) - TOL
        YA(4) = KNOTLOCPC(DEGTANG2+1,DIRTANG2,DECALGEO+IDFCT) - TOL
        YA(5) = YA(1)
c
C----------------------------------------------------------------------
c  1er test : test des 4 points en fonction des 4 coins convexes de la meshsurf
C----------------------------------------------------------------------
c
        IF(XA(1).LT.COIN(1,1).OR.YA(1).LT.COIN(2,1)) CYCLE
        IF(XA(3).GT.COIN(1,2).OR.YA(3).GT.COIN(2,2)) CYCLE
c
C----------------------------------------------------------------------
cc 2eme test : test si le segment forme avec un point eloigne intersecte un cote du polygone
C----------------------------------------------------------------------
c
        XB=COIN(1,1)-1000  !  ON PREND LE POINT SUFFISAMMENT ELOIGNE DE LA MESHSURF
        YB=COIN(2,1)-2000
        DO J=1,4  ! BOUCLE SUR LES 4 COINS DE L'ETENDUE
          INTERSEC=0
          DO K=1,L_TAB_COINKNOT-1
            XC=TAB_COINKNOT(1,K)
            YC=TAB_COINKNOT(2,K)
            XD=TAB_COINKNOT(1,K+1)
            YD=TAB_COINKNOT(2,K+1)
            DET = (XB-XA(J))*(YC-YD) - (XC-XD)*(YB-YA(J))
            IF(DET.EQ.0) THEN
c             segments paralleles ou colineaires
            ELSE
              T1 = ((XC-XA(J))*(YC-YD)-(XC-XD)*(YC-YA(J)))/DET
              T2 = ((XB-XA(J))*(YC-YA(J))-(XC-XA(J))*(YB-YA(J)))/DET
              IF(T1.GT.1.OR.T1.LT.0.OR.T2.GT.1.OR.T2.LE.0) THEN ! pas d'intersection
c                ! pas d'intersection
              ELSE
                INTERSEC = INTERSEC + 1 
c                ! intersection : incrementer le compteur de 1
              ENDIF
            ENDIF
          ENDDO
          IF(MOD(INTERSEC,2).EQ.0) IOUT=1 ! NOMBRE PAIRE D'INTERSECTION, LE POINT EST EN DEHORS DE LA MESHSURF
        ENDDO
c
        IF(IOUT.EQ.1) CYCLE
c
C----------------------------------------------------------------------
c  3eme test : Pour traiter les polygones concaves, il faut tester les segments forme par les etendus 
C----------------------------------------------------------------------
c
        DO J=1,4  ! BOUCLE SUR LES 4 COINS DE L'ETENDUE        
          DO K=1,L_TAB_COINKNOT-1
            XC=TAB_COINKNOT(1,K)
            YC=TAB_COINKNOT(2,K)
            XD=TAB_COINKNOT(1,K+1)
            YD=TAB_COINKNOT(2,K+1)
            DET = (XA(J+1)-XA(J))*(YC-YD) - (XC-XD)*(YA(J+1)-YA(J))
            IF(DET.EQ.0) THEN
c             segments paralleles ou colineaires donc boucler sur un autre segment du polygone
            ELSE
              T1 = ((XC-XA(J))*(YC-YD)-(XC-XD)*(YC-YA(J)))/DET
              T2 = ((XA(J+1)-XA(J))*(YC-YA(J))-(XC-XA(J))*(YA(J+1)-YA(J)))/DET
              IF(T1.GT.1.OR.T1.LT.0.OR.T2.GT.1.OR.T2.LE.0) THEN ! pas d'intersection
c                ! pas d'intersection
              ELSE
                IOUT=1 ! intersection : fonction a exclure
                CYCLE
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C----------------------------------------------------------------------
C  DIMENSIONNEMENT ET STOCKAGE DU TABLEAU DES FONCTIONS A RAFFINER
C----------------------------------------------------------------------
C
        IF(IOUT.EQ.0) THEN
          IF(FLAG.EQ.0) THEN
            L_TAB_FCTCUT = L_TAB_FCTCUT + 1
          ELSE
            L_TAB_FCTCUT = L_TAB_FCTCUT + 1 
            TAB_FCTCUT(L_TAB_FCTCUT) = IDFCT
          ENDIF
        ENDIF
c
      ENDDO
c
      DEALLOCATE(LIST_FCTTRI)
C
      RETURN
      END
c

