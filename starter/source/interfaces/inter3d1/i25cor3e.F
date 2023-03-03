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
Chd|  I25COR3E                      source/interfaces/inter3d1/i25cor3e.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25COR3E(
     1      JLT    ,LEDGE  ,IRECT  ,X      ,
     2      CAND_S ,CAND_M ,
     4      XXS1   ,XXS2   ,XYS1   ,XYS2   ,XZS1   ,
     5      XZS2   ,XXM1   ,XXM2   ,XYM1   ,XYM2   ,
     6      XZM1   ,XZM2   ,EX     ,EY     ,EZ     ,
     7      FX     ,FY     ,FZ     ,
     8      N1     ,N2     ,M1     ,M2     ,NEDGE  ,
     9      GAPE   ,GAPVE  ,
     A      IEDGE  ,ADMSR  ,LBOUND ,EDG_BISECTOR   ,
     B      VTX_BISECTOR   ,ITAB   ,IGAP0  ,IGAP   ,
     C      GAP_E_L )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEDGE(NLEDGE,*), IRECT(4,*), CAND_M(*), CAND_S(*), ADMSR(4,*),
     .        LBOUND(*), JLT, NEDGE, IEDGE, ITAB(*), IGAP0, IGAP, 
     .        N1(MVSIZ), N2(MVSIZ), 
     .        M1(MVSIZ), M2(MVSIZ)
C     REAL
      my_real
     .        X(3,*), 
     .        XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .        XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .        XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .        GAPE(*),GAPVE(MVSIZ),GAP_E_L(NEDGE),
     .        EX(MVSIZ), EY(MVSIZ), EZ(MVSIZ), FX(MVSIZ), FY(MVSIZ), FZ(MVSIZ)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NN, J, JRM, K, KRM, I1, J1, I2, J2, 
     .         IE, JE, SOL_EDGE, SH_EDGE, IM(MVSIZ), IS(MVSIZ)
      my_real
     .   AAA, DX, DY, DZ, DD, NNI, NI2, INVCOS, GAPE_M(MVSIZ), GAPE_S(MVSIZ)
C-----------------------------------------------
      DO I=1,JLT
        IF(CAND_S(I)<=NEDGE) THEN

          I1=LEDGE(1,CAND_S(I))
          J1=LEDGE(2,CAND_S(I))
          N1(I)=LEDGE(5,CAND_S(I))
          N2(I)=LEDGE(6,CAND_S(I))

          IF(IRECT(J1,I1)==N1(I).AND.IRECT(MOD(J1,4)+1,I1)==N2(I))THEN
            IS(I)= 1
          ELSEIF(IRECT(J1,I1)==N2(I).AND.IRECT(MOD(J1,4)+1,I1)==N1(I))THEN
            IS(I)=-1
          ELSE
            print *,'i25cor3e - internal problem',CAND_S(I),N1(I),N2(I),
     .                          IRECT(J1,I1),IRECT(MOD(J1,4)+1,I1)
          END IF

          I2=LEDGE(1,CAND_M(I))
          J2=LEDGE(2,CAND_M(I))
          M1(I)=LEDGE(5,CAND_M(I))
          M2(I)=LEDGE(6,CAND_M(I))

          IF(IRECT(J2,I2)==M1(I).AND.IRECT(MOD(J2,4)+1,I2)==M2(I))THEN
            IM(I)= 1
          ELSEIF(IRECT(J2,I2)==M2(I).AND.IRECT(MOD(J2,4)+1,I2)==M1(I))THEN
            IM(I)=-1
          ELSE
            print *,'i25cor3e - internal problem',CAND_M(I),M1(I),M2(I),
     .                          IRECT(J2,I2),IRECT(MOD(J2,4)+1,I2)
          END IF

          XXS1(I) = X(1,N1(I))
          XYS1(I) = X(2,N1(I))
          XZS1(I) = X(3,N1(I))
          XXS2(I) = X(1,N2(I))
          XYS2(I) = X(2,N2(I))
          XZS2(I) = X(3,N2(I))
          XXM1(I) = X(1,M1(I))
          XYM1(I) = X(2,M1(I))
          XZM1(I) = X(3,M1(I))
          XXM2(I) = X(1,M2(I))
          XYM2(I) = X(2,M2(I))
          XZM2(I) = X(3,M2(I))
        END IF
      END DO

      DO I=1,JLT
        GAPE_M(I)=GAPE(CAND_M(I))
	IF(CAND_S(I)<=NEDGE) THEN
          GAPE_S(I)=GAPE(CAND_S(I))
        ELSE ! TBD
	END IF
        GAPVE(I)=GAPE_M(I)+GAPE_S(I)
      END DO
      IF(IGAP==3) THEN
        DO I=1,JLT
           GAPE_M(I)=MIN(GAPE_M(I),GAP_E_L(CAND_M(I)))
	   IF(CAND_S(I)<=NEDGE) THEN
              GAPE_S(I)=MIN(GAPE_S(I),GAP_E_L(CAND_S(I)))
              GAPVE(I)=MIN(GAP_E_L(CAND_M(I))+GAP_E_L(CAND_S(I)),GAPVE(I))
           ENDIF
        ENDDO
      ENDIF
C
      SOL_EDGE=IEDGE/10 ! solids
      SH_EDGE =IEDGE-10*SOL_EDGE ! shells

      EX(1:JLT)=ZERO
      EY(1:JLT)=ZERO
      EZ(1:JLT)=ZERO
      FX(1:JLT)=ZERO
      FY(1:JLT)=ZERO
      FZ(1:JLT)=ZERO
      IF(SH_EDGE/=0)THEN
        DO I=1,JLT
C
          IE=LEDGE(1,CAND_M(I))
          JE=LEDGE(2,CAND_M(I))
          EX(I) = EDG_BISECTOR(1,JE,IE)
          EY(I) = EDG_BISECTOR(2,JE,IE)
          EZ(I) = EDG_BISECTOR(3,JE,IE)
C
          IF(IABS(LEDGE(7,CAND_S(I)))/=1)THEN
            IE=LEDGE(1,CAND_S(I))
            JE=LEDGE(2,CAND_S(I))
            FX(I) = EDG_BISECTOR(1,JE,IE)
            FY(I) = EDG_BISECTOR(2,JE,IE)
            FZ(I) = EDG_BISECTOR(3,JE,IE)
          END IF

        END DO
      END IF

      DO I=1,JLT
C
        IF(LEDGE(3,CAND_M(I))/=0)CYCLE
C
        IE=LEDGE(1,CAND_M(I))
        JE=LEDGE(2,CAND_M(I))
        IF(IM(I)==1)THEN
          I1=ADMSR(JE,IE)
          I2=ADMSR(MOD(JE,4)+1,IE)
        ELSE ! IM(I)==-1
          I2=ADMSR(JE,IE)
          I1=ADMSR(MOD(JE,4)+1,IE)
        END IF
C
        EX(I) = EDG_BISECTOR(1,JE,IE)
        EY(I) = EDG_BISECTOR(2,JE,IE)
        EZ(I) = EDG_BISECTOR(3,JE,IE)
C
C       DX, DY, DZ Direction for shifting
        DX = VTX_BISECTOR(1,1,I1)+VTX_BISECTOR(1,2,I1)
        DY = VTX_BISECTOR(2,1,I1)+VTX_BISECTOR(2,2,I1)
        DZ = VTX_BISECTOR(3,1,I1)+VTX_BISECTOR(3,2,I1)
C
        NNI = EX(I)*DX + EY(I)*DY + EZ(I)*DZ
        NI2 = DX*DX + DY*DY + DZ*DZ

        IF(NNI < ZERO)THEN
          DX=DX-TWO*NNI*EX(I)
          DY=DY-TWO*NNI*EY(I)
          DZ=DZ-TWO*NNI*EZ(I)
          NNI=-NNI
        END IF

        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from edge normal
          AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
          DX = DX + AAA*EX(I)
          DY = DY + AAA*EY(I)
          DZ = DZ + AAA*EZ(I)
        ENDIF
        DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
        DX = DX*DD
        DY = DY*DD
        DZ = DZ*DD
        INVCOS  = ONE / MAX(EM20,EX(I)*DX  + EY(I)*DY  + EZ(I)*DZ)
        DX = DX*INVCOS
        DY = DY*INVCOS
        DZ = DZ*INVCOS
C
        XXM1(I) = XXM1(I)-GAPE_M(I)*DX
        XYM1(I) = XYM1(I)-GAPE_M(I)*DY
        XZM1(I) = XZM1(I)-GAPE_M(I)*DZ
C
C       DX, DY, DZ Direction for shifting
        DX = VTX_BISECTOR(1,1,I2)+VTX_BISECTOR(1,2,I2)
        DY = VTX_BISECTOR(2,1,I2)+VTX_BISECTOR(2,2,I2)
        DZ = VTX_BISECTOR(3,1,I2)+VTX_BISECTOR(3,2,I2)
C
        NNI = EX(I)*DX  + EY(I)*DY  + EZ(I)*DZ
        NI2 = DX*DX + DY*DY + DZ*DZ

        IF(NNI < ZERO)THEN
          DX=DX-TWO*NNI*EX(I)
          DY=DY-TWO*NNI*EY(I)
          DZ=DZ-TWO*NNI*EZ(I)
          NNI=-NNI
        END IF

        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from edge normal
          AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
          DX = DX + AAA*EX(I)
          DY = DY + AAA*EY(I)
          DZ = DZ + AAA*EZ(I)
        ENDIF
        DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
        DX = DX*DD
        DY = DY*DD
        DZ = DZ*DD
        INVCOS  = ONE / MAX(EM20,EX(I)*DX  + EY(I)*DY  + EZ(I)*DZ)
        DX = DX*INVCOS
        DY = DY*INVCOS
        DZ = DZ*INVCOS
C
        XXM2(I) = XXM2(I)-GAPE_M(I)*DX
        XYM2(I) = XYM2(I)-GAPE_M(I)*DY
        XZM2(I) = XZM2(I)-GAPE_M(I)*DZ
C
      END DO

      IF(IGAP0/=0) THEN
        DO I=1,JLT
C
          IF(LEDGE(3,CAND_S(I))/=0)CYCLE
C
          IE=LEDGE(1,CAND_S(I))
          JE=LEDGE(2,CAND_S(I))
          IF(IS(I)==1)THEN
            I1=ADMSR(JE,IE)
            I2=ADMSR(MOD(JE,4)+1,IE)
          ELSE ! IS(I)==-1
            I2=ADMSR(JE,IE)
            I1=ADMSR(MOD(JE,4)+1,IE)
          END IF
C
          FX(I) = EDG_BISECTOR(1,JE,IE)
          FY(I) = EDG_BISECTOR(2,JE,IE)
          FZ(I) = EDG_BISECTOR(3,JE,IE)
C
C         DX, DY, DZ Direction for shifting
	  DX = VTX_BISECTOR(1,1,I1)+VTX_BISECTOR(1,2,I1)
	  DY = VTX_BISECTOR(2,1,I1)+VTX_BISECTOR(2,2,I1)
	  DZ = VTX_BISECTOR(3,1,I1)+VTX_BISECTOR(3,2,I1)
C
          NNI = FX(I)*DX + FY(I)*DY + FZ(I)*DZ
          NI2 = DX*DX + DY*DY + DZ*DZ

          IF(NNI < ZERO)THEN
            DX=DX-TWO*NNI*FX(I)
            DY=DY-TWO*NNI*FY(I)
            DZ=DZ-TWO*NNI*FZ(I)
            NNI=-NNI
          END IF

          IF(TWO*NNI*NNI < NI2)THEN
c           scharp angle bound nodal normal to 45 from edge normal
            AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
            DX = DX + AAA*FX(I)
            DY = DY + AAA*FY(I)
            DZ = DZ + AAA*FZ(I)
          ENDIF
          DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
	  DX = DX*DD
	  DY = DY*DD
	  DZ = DZ*DD
          INVCOS  = ONE / MAX(EM20,FX(I)*DX  + FY(I)*DY  + FZ(I)*DZ)
          DX = DX*INVCOS
          DY = DY*INVCOS
          DZ = DZ*INVCOS
C
	  XXS1(I) = XXS1(I)-GAPE_S(I)*DX
	  XYS1(I) = XYS1(I)-GAPE_S(I)*DY
	  XZS1(I) = XZS1(I)-GAPE_S(I)*DZ
C
C         DX, DY, DZ Direction for shifting
	  DX = VTX_BISECTOR(1,1,I2)+VTX_BISECTOR(1,2,I2)
	  DY = VTX_BISECTOR(2,1,I2)+VTX_BISECTOR(2,2,I2)
	  DZ = VTX_BISECTOR(3,1,I2)+VTX_BISECTOR(3,2,I2)
C
          NNI = FX(I)*DX  + FY(I)*DY  + FZ(I)*DZ
          NI2 = DX*DX + DY*DY + DZ*DZ

          IF(NNI < ZERO)THEN
            DX=DX-TWO*NNI*FX(I)
            DY=DY-TWO*NNI*FY(I)
            DZ=DZ-TWO*NNI*FZ(I)
            NNI=-NNI
          END IF

          IF(TWO*NNI*NNI < NI2)THEN
c           scharp angle bound nodal normal to 45 from edge normal
            AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
            DX = DX + AAA*FX(I)
            DY = DY + AAA*FY(I)
            DZ = DZ + AAA*FZ(I)
          ENDIF
          DD=ONE/MAX(EM20,SQRT(DX*DX+DY*DY+DZ*DZ))
	  DX = DX*DD
	  DY = DY*DD
	  DZ = DZ*DD
          INVCOS  = ONE / MAX(EM20,FX(I)*DX  + FY(I)*DY  + FZ(I)*DZ)
          DX = DX*INVCOS
          DY = DY*INVCOS
          DZ = DZ*INVCOS
C
	  XXS2(I) = XXS2(I)-GAPE_S(I)*DX
	  XYS2(I) = XYS2(I)-GAPE_S(I)*DY
	  XZS2(I) = XZS2(I)-GAPE_S(I)*DZ
C
        END DO
      END IF
C
      RETURN
      END
