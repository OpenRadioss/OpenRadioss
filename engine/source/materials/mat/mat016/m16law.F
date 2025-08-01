Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    m16law   ../engine/source/materials/mat/mat016/m16law.F
!||--- called by ------------------------------------------------------
!||    mmain    ../engine/source/materials/mat_share/mmain.F90
!||====================================================================
      SUBROUTINE M16LAW(PM   ,OFF   ,SIG ,NEL  ,
     2                  EPXE ,THETA ,EPD ,XIST ,
     3                  MAT  ,D1    ,D2  ,D3   ,
     4                  D4   ,D5    ,D6  ,POLD )
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
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NEL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EPXE(*), THETA(*),
     .   EPD(*), XIST(*), D1(*), D2(*),
     .   D3(*), D4(*), D5(*), D6(*), POLD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ), AK(MVSIZ), QH(MVSIZ), TMELT(MVSIZ),
     .   AJ2(MVSIZ), DAV(MVSIZ), EPMX(MVSIZ),
     .   THETL(MVSIZ), CA(MVSIZ), CB(MVSIZ), CC(MVSIZ), CN(MVSIZ), EPDR(MVSIZ), CMX(MVSIZ),
     .   SIGMX(MVSIZ),  TSTAR, CT, CE, CH, SCALE(MVSIZ), DPLA(MVSIZ)
C------------------------------------------------------------------------------------------
      MX = MAT(1)
      DO I=1,NEL
        G(I)    =PM(22,MX)*OFF(I)
        CA(I)   =PM(38,MX)
        CB(I)   =PM(39,MX)
        CN(I)   =PM(40,MX)
        CC(I)   =PM(43,MX)
        CMX(I)  =PM(45,MX)
        TMELT(I)=PM(46,MX)
        EPDR(I) =PM(44,MX)
        THETL(I)=PM(47,MX)
        EPMX(I) =PM(41,MX)
        SIGMX(I)=PM(42,MX)
      ENDDO
C
      DO I=1,NEL
        POLD(I)=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        DAV(I) =-THIRD*(D1(I)+D2(I)+D3(I))
        G1(I)  =DT1*G(I)
        G2(I)  =TWO*G1(I)
      ENDDO
C-------------------------------
C     CONTRAINTES DEVIATORIQUES
C-------------------------------
      DO I=1,NEL
        SIG(I,1)=SIG(I,1)+POLD(I)+G2(I)*(D1(I)+DAV(I))
        SIG(I,2)=SIG(I,2)+POLD(I)+G2(I)*(D2(I)+DAV(I))
        SIG(I,3)=SIG(I,3)+POLD(I)+G2(I)*(D3(I)+DAV(I))
        SIG(I,4)=SIG(I,4)+G1(I)*D4(I)
        SIG(I,5)=SIG(I,5)+G1(I)*D5(I)
        SIG(I,6)=SIG(I,6)+G1(I)*D6(I)
      ENDDO
C
      DO I=1,NEL
        AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     1                +SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(THREE*AJ2(I))
      ENDDO
C
      DO I=1,NEL
        IF(THETA(I)>=TMELT(I)) CYCLE
C
        TSTAR=ZERO
        CT=ONE
        IF(THETA(I)<=THREE100) GOTO 60
        TSTAR=(THETA(I)-THREE100)/(TMELT(I)-THREE100)
        IF(THETA(I)>THETL(I)) CMX(I)=ONE
        CT=ONE-TSTAR**CMX(I)
C
   60   CE=ONE
C
        EPD(I)=OFF(I) * MAX(   ABS(D1(I)),   ABS(D2(I)),   ABS(D3(I)),
     .          HALF*ABS(D4(I)),HALF*ABS(D5(I)),HALF*ABS(D6(I)))
C----------------------------------------------------------
        IF(EPD(I)<=EPDR(I)) GOTO 70
        CE=ONE+CC(I) * LOG(EPD(I)/EPDR(I))
C
   70   CH=CA(I)
        IF(EPXE(I)<=ZERO) GOTO 80
        CH=CA(I)+CB(I)*EPXE(I)**CN(I)
        IF(EPXE(I)>EPMX(I)) CH=CA(I)+CB(I)*EPMX(I)**CN(I)
C
   80   AK(I)= MIN(SIGMX(I),CH)*CE*CT
C-----------------------
C     MODULE ECROUISSAGE
C-----------------------
        IF(CN(I)>=1) THEN
          QH(I)= (CB(I)*CN(I)*EPXE(I)**(CN(I)-ONE))*CE*CT
        ELSEIF(EPXE(I)/=ZERO)THEN
          QH(I)= (CB(I)*CN(I)/EPXE(I)**(ONE-CN(I)))*CE*CT
        ELSE
          QH(I)=ZERO
        ENDIF
C-------------------------------
C     PROJECTION
C-------------------------------
        IF(AJ2(I)<=AK(I)) THEN
          SCALE(I)=ONE
        ELSEIF(AJ2(I)/=ZERO) THEN
          SCALE(I)=AK(I)/AJ2(I)
        ELSE
          SCALE(I)=ZERO
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IF(THETA(I)>=TMELT(I) .OR. XIST(I)/=ZERO) THEN
          EPXE(I)=ZERO
          SIG(I,1)=ZERO
          SIG(I,2)=ZERO
          SIG(I,3)=ZERO
          SIG(I,4)=ZERO
          SIG(I,5)=ZERO
          SIG(I,6)=ZERO    
        ELSEIF(AJ2(I)>AK(I)) THEN
          DPLA(I)=(ONE -SCALE(I))*AJ2(I)/(THREE*G(I)+QH(I))
          AK(I)=AK(I)+DPLA(I)*QH(I)
          SCALE(I)= MIN(ONE,AK(I)/ MAX(AJ2(I),EM15))
          SIG(I,1)=SCALE(I)*SIG(I,1)
          SIG(I,2)=SCALE(I)*SIG(I,2)
          SIG(I,3)=SCALE(I)*SIG(I,3)
          SIG(I,4)=SCALE(I)*SIG(I,4)
          SIG(I,5)=SCALE(I)*SIG(I,5)
          SIG(I,6)=SCALE(I)*SIG(I,6)
          EPXE(I)=EPXE(I)+DPLA(I)
          EPXE(I)=EPXE(I)*OFF(I)
        ENDIF
      ENDDO
C
      RETURN
      END
