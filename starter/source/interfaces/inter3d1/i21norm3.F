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
Chd|  I21NORM3                      source/interfaces/inter3d1/i21norm3.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21NORM3(
     1 X     ,IRECTM ,NRTM  ,NMN   ,MSR   ,
     2 NINT  ,NTY   ,NOINT  ,NOD_NORMAL, XM0   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, NINT, NTY, NOINT,NMN
      INTEGER IRECTM(4,*), MSR(*)
C     REAL
      my_real
     .   X(3,*), NOD_NORMAL(3,*), XM0(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, FIRST, LAST, IRM, I1, I2, I3, I4
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .     X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ),
     .     XN1(MVSIZ),YN1(MVSIZ),ZN1(MVSIZ),
     .     XN2(MVSIZ),YN2(MVSIZ),ZN2(MVSIZ),
     .     XN3(MVSIZ),YN3(MVSIZ),ZN3(MVSIZ),
     .     XN4(MVSIZ),YN4(MVSIZ),ZN4(MVSIZ),
     .     AAA
C--------------------------------------------------------------
      FIRST=1
      LAST =MIN(NRTM,MVSIZ)
C
 100  CONTINUE
C
      DO I=1,LAST-FIRST+1
        IRM=I+FIRST-1
        IX1(I)=MSR(IRECTM(1,IRM))
        IX2(I)=MSR(IRECTM(2,IRM))
        IX3(I)=MSR(IRECTM(3,IRM))
        IX4(I)=MSR(IRECTM(4,IRM))
        X1(I)=X(1,IX1(I))
        Y1(I)=X(2,IX1(I))
        Z1(I)=X(3,IX1(I))
        X2(I)=X(1,IX2(I))
        Y2(I)=X(2,IX2(I))
        Z2(I)=X(3,IX2(I))
        X3(I)=X(1,IX3(I))
        Y3(I)=X(2,IX3(I))
        Z3(I)=X(3,IX3(I))
        X4(I)=X(1,IX4(I))
        Y4(I)=X(2,IX4(I))
        Z4(I)=X(3,IX4(I))
      END DO
C
      DO I=1,LAST-FIRST+1
        IF(IX3(I)/=IX4(I))THEN
         X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
         Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
         Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
        ELSE
         X0(I) = X3(I)
         Y0(I) = Y3(I)
         Z0(I) = Z3(I)
        ENDIF
      END DO
C
      DO I=1,LAST-FIRST+1
C
        X01(I) = X1(I) - X0(I)
        Y01(I) = Y1(I) - Y0(I)
        Z01(I) = Z1(I) - Z0(I)
C
        X02(I) = X2(I) - X0(I)
        Y02(I) = Y2(I) - Y0(I)
        Z02(I) = Z2(I) - Z0(I)
C
        X03(I) = X3(I) - X0(I)
        Y03(I) = Y3(I) - Y0(I)
        Z03(I) = Z3(I) - Z0(I)
C
        X04(I) = X4(I) - X0(I)
        Y04(I) = Y4(I) - Y0(I)
        Z04(I) = Z4(I) - Z0(I)
C
      ENDDO
C
      DO I=1,LAST-FIRST+1
C
        XN1(I) = Y01(I)*Z02(I) - Z01(I)*Y02(I)
        YN1(I) = Z01(I)*X02(I) - X01(I)*Z02(I)
        ZN1(I) = X01(I)*Y02(I) - Y01(I)*X02(I)
C
        XN2(I) = Y02(I)*Z03(I) - Z02(I)*Y03(I)
        YN2(I) = Z02(I)*X03(I) - X02(I)*Z03(I)
        ZN2(I) = X02(I)*Y03(I) - Y02(I)*X03(I)
C
        XN3(I) = Y03(I)*Z04(I) - Z03(I)*Y04(I)
        YN3(I) = Z03(I)*X04(I) - X03(I)*Z04(I)
        ZN3(I) = X03(I)*Y04(I) - Y03(I)*X04(I)
C
        XN4(I) = Y04(I)*Z01(I) - Z04(I)*Y01(I)
        YN4(I) = Z04(I)*X01(I) - X04(I)*Z01(I)
        ZN4(I) = X04(I)*Y01(I) - Y04(I)*X01(I)
C
      ENDDO
C
      DO I=1,LAST-FIRST+1
C
        AAA=ONE/MAX(EM30,SQRT(XN1(I)*XN1(I)+YN1(I)*YN1(I)+ZN1(I)*ZN1(I)))
        XN1(I) = XN1(I)*AAA
        YN1(I) = YN1(I)*AAA
        ZN1(I) = ZN1(I)*AAA
C
        AAA=ONE/MAX(EM30,SQRT(XN2(I)*XN2(I)+YN2(I)*YN2(I)+ZN2(I)*ZN2(I)))
        XN2(I) = XN2(I)*AAA
        YN2(I) = YN2(I)*AAA
        ZN2(I) = ZN2(I)*AAA
C
        AAA=ONE/MAX(EM30,SQRT(XN3(I)*XN3(I)+YN3(I)*YN3(I)+ZN3(I)*ZN3(I)))
        XN3(I) = XN3(I)*AAA
        YN3(I) = YN3(I)*AAA
        ZN3(I) = ZN3(I)*AAA
C
        AAA=ONE/MAX(EM30,SQRT(XN4(I)*XN4(I)+YN4(I)*YN4(I)+ZN4(I)*ZN4(I)))
        XN4(I) = XN4(I)*AAA
        YN4(I) = YN4(I)*AAA
        ZN4(I) = ZN4(I)*AAA
C
      ENDDO
C
      DO I=1,LAST-FIRST+1
C
        IRM=I+FIRST-1
C
        I1=IRECTM(1,IRM)
        I2=IRECTM(2,IRM)
        I3=IRECTM(3,IRM)
        I4=IRECTM(4,IRM)
C
        IF(I4/=I3)THEN
C
          NOD_NORMAL(1,I1)=NOD_NORMAL(1,I1)+XN4(I)+XN1(I)
          NOD_NORMAL(2,I1)=NOD_NORMAL(2,I1)+YN4(I)+YN1(I)
          NOD_NORMAL(3,I1)=NOD_NORMAL(3,I1)+ZN4(I)+ZN1(I)
C
          NOD_NORMAL(1,I2)=NOD_NORMAL(1,I2)+XN1(I)+XN2(I)
          NOD_NORMAL(2,I2)=NOD_NORMAL(2,I2)+YN1(I)+YN2(I)
          NOD_NORMAL(3,I2)=NOD_NORMAL(3,I2)+ZN1(I)+ZN2(I)
C
          NOD_NORMAL(1,I3)=NOD_NORMAL(1,I3)+XN2(I)+XN3(I)
          NOD_NORMAL(2,I3)=NOD_NORMAL(2,I3)+YN2(I)+YN3(I)
          NOD_NORMAL(3,I3)=NOD_NORMAL(3,I3)+ZN2(I)+ZN3(I)
C
          NOD_NORMAL(1,I4)=NOD_NORMAL(1,I4)+XN3(I)+XN4(I)
          NOD_NORMAL(2,I4)=NOD_NORMAL(2,I4)+YN3(I)+YN4(I)
          NOD_NORMAL(3,I4)=NOD_NORMAL(3,I4)+ZN3(I)+ZN4(I)
C
        ELSE
C
          NOD_NORMAL(1,I1)=NOD_NORMAL(1,I1)+XN1(I)
          NOD_NORMAL(2,I1)=NOD_NORMAL(2,I1)+YN1(I)
          NOD_NORMAL(3,I1)=NOD_NORMAL(3,I1)+ZN1(I)
C
          NOD_NORMAL(1,I2)=NOD_NORMAL(1,I2)+XN1(I)
          NOD_NORMAL(2,I2)=NOD_NORMAL(2,I2)+YN1(I)
          NOD_NORMAL(3,I2)=NOD_NORMAL(3,I2)+ZN1(I)
C
          NOD_NORMAL(1,I3)=NOD_NORMAL(1,I3)+XN1(I)
          NOD_NORMAL(2,I3)=NOD_NORMAL(2,I3)+YN1(I)
          NOD_NORMAL(3,I3)=NOD_NORMAL(3,I3)+ZN1(I)
C
        END IF
C
      ENDDO
C
      IF(LAST < NRTM)THEN
        FIRST=LAST+1
        LAST =MIN(LAST+MVSIZ,NRTM)
        GO TO 100
      END IF
C------------------------------------
C     normales aux noeuds.
C------------------------------------
      DO I=1,NMN
C
        AAA=ONE/MAX(EM30,SQRT(NOD_NORMAL(1,I)*NOD_NORMAL(1,I)+
     .                       NOD_NORMAL(2,I)*NOD_NORMAL(2,I)+
     .                       NOD_NORMAL(3,I)*NOD_NORMAL(3,I)))
        NOD_NORMAL(1,I)=NOD_NORMAL(1,I)*AAA
        NOD_NORMAL(2,I)=NOD_NORMAL(2,I)*AAA
        NOD_NORMAL(3,I)=NOD_NORMAL(3,I)*AAA
C
      END DO
C------------------------------------
C     Coordonnees initiales cote main (1: active ; 0: inactive)
C------------------------------------
      DO I=1,NMN
        DO J=1,3
          XM0(J,I)=X(J,MSR(I))
        END DO
      END DO
C------------------------------------
      RETURN
 1000 FORMAT(2X,'GAP MIN = ',1PG20.13)
      END
Chd|====================================================================
Chd|  I21RCURV                      source/interfaces/inter3d1/i21norm3.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21RCURV(NRTM,XM0 ,NOD_NORMAL ,IRECT ,RCURV ,
     .                   ANGLM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, IRECT(4,*)
      my_real
     .    XM0(3,*), NOD_NORMAL(3,*), RCURV(*), ANGLM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N1, N2, N3, N4
      my_real
     .   X1, X2, X3, X4,
     .   Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4,
     .   NNX1, NNX2, NNX3, NNX4,
     .   NNY1, NNY2, NNY3, NNY4,
     .   NNZ1, NNZ2, NNZ3, NNZ4,
     .   SURFX, SURFY, SURFZ, 
     .   ERX, ERY, ERZ, DNX, DNY, DNZ, DNT, LL, AAA, RR,
     .   X13, Y13, Z13, X24, Y24, Z24, NX, NY, NZ, CC
C-----------------------------------------------
      RCURV(1:NRTM) = EP30
      ANGLM(1:NRTM) = EP30

      DO I=1,NRTM
        N1=IRECT(1,I)
        N2=IRECT(2,I)
        N3=IRECT(3,I)
        N4=IRECT(4,I)

        X1=XM0(1,N1)  
        Y1=XM0(2,N1)  
        Z1=XM0(3,N1)  

        X2=XM0(1,N2)  
        Y2=XM0(2,N2)  
        Z2=XM0(3,N2)  

        X3=XM0(1,N3)  
        Y3=XM0(2,N3)  
        Z3=XM0(3,N3)  

        X4=XM0(1,N4)  
        Y4=XM0(2,N4)  
        Z4=XM0(3,N4)  

        NNX1=NOD_NORMAL(1,N1)
        NNY1=NOD_NORMAL(2,N1)
        NNZ1=NOD_NORMAL(3,N1)

        NNX2=NOD_NORMAL(1,N2)
        NNY2=NOD_NORMAL(2,N2)
        NNZ2=NOD_NORMAL(3,N2)

        NNX3=NOD_NORMAL(1,N3)
        NNY3=NOD_NORMAL(2,N3)
        NNZ3=NOD_NORMAL(3,N3)

        NNX4=NOD_NORMAL(1,N4)
        NNY4=NOD_NORMAL(2,N4)
        NNZ4=NOD_NORMAL(3,N4)

C-------
        ERX = (X2+X3)-(X1+X4)
        ERY = (Y2+Y3)-(Y1+Y4)
        ERZ = (Z2+Z3)-(Z1+Z4)

C       Longueur vraie = LL/2
        LL  = SQRT(ERX*ERX+ERY*ERY+ERZ*ERZ)
        AAA = ONE / LL
        ERX = ERX*AAA
        ERY = ERY*AAA
        ERZ = ERZ*AAA

        DNX= (NNX2+NNX3)-(NNX1+NNX4)
        DNY= (NNY2+NNY3)-(NNY1+NNY4)
        DNZ= (NNZ2+NNZ3)-(NNZ1+NNZ4)
C
C       DN vraie = DNT/2
        DNT=(DNX*ERX+DNY*ERY+DNZ*ERZ)

        RR=LL/MAX(EM20,ABS(DNT))
        RCURV(I)=MIN(RCURV(I),RR)
C-------
        ERX = (X4+X3)-(X1+X2)
        ERY = (Y4+Y3)-(Y1+Y2)
        ERZ = (Z4+Z3)-(Z1+Z2)

C       Longueur vraie = LL/2
        LL  = SQRT(ERX*ERX+ERY*ERY+ERZ*ERZ)
        AAA = ONE / LL
        ERX = ERX*AAA
        ERY = ERY*AAA
        ERZ = ERZ*AAA

        DNX= (NNX4+NNX3)-(NNX1+NNX2)
        DNY= (NNY4+NNY3)-(NNY1+NNY2)
        DNZ= (NNZ4+NNZ3)-(NNZ1+NNZ2)
C
C       DN vraie = DNT/2
        DNT=(DNX*ERX+DNY*ERY+DNZ*ERZ)

        RR=LL/MAX(EM20,ABS(DNT))
        RCURV(I)=MIN(RCURV(I),RR)
C-------
C-------
C-------
C       Angles.
C-------
        X13 = X3 - X1
        Y13 = Y3 - Y1
        Z13 = Z3 - Z1

        X24 = X4 - X2
        Y24 = Y4 - Y2
        Z24 = Z4 - Z2

        SURFX = Y13*Z24 - Z13*Y24
        SURFY = Z13*X24 - X13*Z24
        SURFZ = X13*Y24 - Y13*X24

        AAA=ONE/MAX(EM30,SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ))
        SURFX = SURFX * AAA
        SURFY = SURFY * AAA
        SURFZ = SURFZ * AAA

        CC=(SURFX*NNX1+SURFY*NNY1+SURFZ*NNZ1)
        ANGLM(I)=MIN(ANGLM(I),CC)

        CC=(SURFX*NNX2+SURFY*NNY2+SURFZ*NNZ2)
        ANGLM(I)=MIN(ANGLM(I),CC)

        CC=(SURFX*NNX3+SURFY*NNY3+SURFZ*NNZ3)
        ANGLM(I)=MIN(ANGLM(I),CC)

        CC=(SURFX*NNX4+SURFY*NNY4+SURFZ*NNZ4)
        ANGLM(I)=MIN(ANGLM(I),CC)
      ENDDO

      RETURN
      END
