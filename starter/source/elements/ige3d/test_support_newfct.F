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
Chd|  TEST_SUPPORT_NEWFCT           source/elements/ige3d/test_support_newfct.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TEST_SUPPORT_NEWFCT(KNOTLOCPC, DIRDEG, DEGTANG1, DEGTANG2, 
     .                               DIR,NEWKNOT,
     .                               TAB_COINKNOT,L_TAB_COINKNOT,TAB_NEWFCT,
     .                               TAB_NEWFCTCUT,L_TAB_NEWFCTCUT,DECALGEO,TAB_REMOVE,FLAG)
C----------------------------------------------------------------------
C   ROUTINE QUI MET DE COTE TOUTES LES FONCTIONS QUI ONT 
C   ETE MODIFIEES OU CREES PAR RAFIG3D.F MAIS QUI PEUVENT ENCORE ETRE
C   MODIFIEE OU SUPPRIMEES PAR LES ANCIENNES MESHSURFS QUE LA MESHSURF
C   ACTUELLE CROISE
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
      INTEGER DIRDEG,DEGTANG1,DEGTANG2,DIR,DECALGEO,
     .        L_TAB_COINKNOT,TAB_NEWFCT(*),
     .        TAB_NEWFCTCUT(L_TAB_NEWFCTCUT),L_TAB_NEWFCTCUT,FLAG,TAB_REMOVE(*)
      my_real KNOTLOCPC(DEG_MAX,3,*),TAB_COINKNOT(2,*),NEWKNOT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,DIRTANG1,DIRTANG2,
     .        IEL,INTERSEC,IDFCT,IOUT,ADD
      my_real DET, T1, T2, XA(5),YA(5),COIN(2,2),
     .        XB, YB, XC, YC, XD, YD, TOL
C-----------------------------------------------
C
      L_TAB_NEWFCTCUT = 0
      TOL = EM06
C
      IF(DIR==1) THEN
        DIRTANG1 = 2
        DIRTANG2 = 3
      ELSEIF(DIR==2) THEN
        DIRTANG1 = 3
        DIRTANG2 = 1
      ELSEIF(DIR==3) THEN
        DIRTANG1 = 1
        DIRTANG2 = 2
      ENDIF
C
      COIN(1,1) = MINVAL(TAB_COINKNOT(1,1:(L_TAB_COINKNOT)))
      COIN(2,1) = MINVAL(TAB_COINKNOT(2,1:(L_TAB_COINKNOT)))
      COIN(1,2) = MAXVAL(TAB_COINKNOT(1,1:(L_TAB_COINKNOT)))
      COIN(2,2) = MAXVAL(TAB_COINKNOT(2,1:(L_TAB_COINKNOT)))
C
      DO I=1,NEWFCT ! les nouvelles fonctions ajoutees par le dernier rafig3d.F

        IDFCT = TAB_NEWFCT(OFFSET_NEWFCT+I) 
C       Fonctions stockees dans le tableau tab_newfct offsete 
c       pour ne pas prendre les newfct des anciennes coupes terminees

        DO J=1,L_TAB_REMOVE
          IF(TAB_REMOVE(J)==IDFCT) EXIT
        ENDDO
        IF(J<=L_TAB_REMOVE) CYCLE

        IF(KNOTLOCPC(1,DIR,DECALGEO+IDFCT)>=NEWKNOT) CYCLE
        IF(KNOTLOCPC(DIRDEG+1,DIR,DECALGEO+IDFCT)<=NEWKNOT) CYCLE
C
C----------------------------------------------------------------------
C  LE KNOT A INSERER NE DOIT PAS DEJA ETRE PRESENT
C----------------------------------------------------------------------
C
        DO J=2,DIRDEG
          IF(KNOTLOCPC(J,DIR,DECALGEO+IDFCT)==NEWKNOT) EXIT
        ENDDO
        IF(J<=DIRDEG) CYCLE
c
        IOUT=0
C
C----------------------------------------------------------------------
CC DOCUMENTATION TEST D'INCLUSION : VOIR TEST_SUPPORT_FCT.F
C----------------------------------------------------------------------
C
        XA(1) = KNOTLOCPC(1,DIRTANG1,DECALGEO+IDFCT) + TOL
        XA(2) = KNOTLOCPC(DEGTANG1+1,DIRTANG1,DECALGEO+IDFCT) - TOL
        XA(3) = KNOTLOCPC(DEGTANG1+1,DIRTANG1,DECALGEO+IDFCT) - TOL
        XA(4) = KNOTLOCPC(1,DIRTANG1,DECALGEO+IDFCT) + TOL
        XA(5) = XA(1)
        YA(1) = KNOTLOCPC(1,DIRTANG2,DECALGEO+IDFCT) + TOL
        YA(2) = KNOTLOCPC(1,DIRTANG2,DECALGEO+IDFCT) + TOL
        YA(3) = KNOTLOCPC(DEGTANG2+1,DIRTANG2,DECALGEO+IDFCT) - TOL
        YA(4) = KNOTLOCPC(DEGTANG2+1,DIRTANG2,DECALGEO+IDFCT) - TOL
        YA(5) = YA(1)
C
        IF(XA(1)<COIN(1,1).OR.YA(1)<COIN(2,1)) CYCLE
        IF(XA(3)>COIN(1,2).OR.YA(3)>COIN(2,2)) CYCLE
C
        XB=COIN(1,1)-1000
        YB=COIN(2,1)-2000
C
        DO J=1,4
          INTERSEC=0
          DO K=1,L_TAB_COINKNOT-1
            XC=TAB_COINKNOT(1,K)
            YC=TAB_COINKNOT(2,K)
            XD=TAB_COINKNOT(1,K+1)
            YD=TAB_COINKNOT(2,K+1)
            DET = (XB-XA(J))*(YC-YD) - (XC-XD)*(YB-YA(J))
            IF(DET==0) THEN
            ELSE
              T1 = ((XC-XA(J))*(YC-YD)-(XC-XD)*(YC-YA(J)))/DET
              T2 = ((XB-XA(J))*(YC-YA(J))-(XC-XA(J))*(YB-YA(J)))/DET
              IF(T1>1.OR.T1<0.OR.T2>1.OR.T2<=0) THEN
              ELSE
                INTERSEC = INTERSEC + 1 
              ENDIF
            ENDIF
          ENDDO
          IF(MOD(INTERSEC,2)==0) IOUT=1
        ENDDO
C
        DO J=1,4  
          DO K=1,L_TAB_COINKNOT-1
            XC=TAB_COINKNOT(1,K)
            YC=TAB_COINKNOT(2,K)
            XD=TAB_COINKNOT(1,K+1)
            YD=TAB_COINKNOT(2,K+1)
            DET = (XA(J+1)-XA(J))*(YC-YD) - (XC-XD)*(YA(J+1)-YA(J))
            IF(DET==0) THEN
            ELSE
              T1 = ((XC-XA(J))*(YC-YD)-(XC-XD)*(YC-YA(J)))/DET
              T2 = ((XA(J+1)-XA(J))*(YC-YA(J))-(XC-XA(J))*(YA(J+1)-YA(J)))/DET
              IF(T1>1.OR.T1<0.OR.T2>1.OR.T2<=0) THEN
              ELSE
                IOUT=1
                CYCLE
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C----------------------------------------------------------------------
C  DIMENSIONNEMENT ET STOCKAGE DU TABLEAU DES FONCTIONS A RETRAITER 
C  PAR LES ANCIENNES COUPES (TAB_MESHSURFCUT)
C----------------------------------------------------------------------
C
        IF(IOUT==0) THEN
          IF(FLAG==0) THEN
            L_TAB_NEWFCTCUT = L_TAB_NEWFCTCUT + 1
          ELSE
            L_TAB_NEWFCTCUT = L_TAB_NEWFCTCUT + 1
            TAB_NEWFCTCUT(L_TAB_NEWFCTCUT)=IDFCT
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END


