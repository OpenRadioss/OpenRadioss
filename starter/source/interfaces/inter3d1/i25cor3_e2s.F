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
Chd|  I25COR3_E2S                   source/interfaces/inter3d1/i25cor3_e2s.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25COR3_E2S(
     1      JLT    ,LEDGE  ,IRECT  ,X      ,
     2      CAND_S ,CAND_M ,EX     ,EY     ,EZ     ,
     3      XXS1   ,XXS2   ,XYS1   ,XYS2   ,
     4      XZS1   ,XZS2   ,XXM1   ,XXM2   ,XYM1   ,
     5      XYM2   ,XZM1   ,XZM2   ,
     6      N1     ,N2     ,M1     ,M2     ,NRTS   ,
     7      GAPE  ,GAPVE  ,FX      ,FY     ,FZ     ,
     8      IEDGE  ,ADMSR  ,LBOUND ,EDG_BISECTOR   ,
     9      VTX_BISECTOR,ITAB   )
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
     .        LBOUND(*), JLT, NRTS, IEDGE, ITAB(*),
     .        N1(MVSIZ), N2(MVSIZ), 
     .        M1(4,MVSIZ), M2(4,MVSIZ)
C     REAL
      my_real
     .        X(3,*), 
     .        XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .        XZS1(MVSIZ), XZS2(MVSIZ), XXM1(4,MVSIZ), XXM2(4,MVSIZ),
     .        XYM1(4,MVSIZ), XYM2(4,MVSIZ), XZM1(4,MVSIZ), XZM2(4,MVSIZ),
     .        GAPE(*),GAPVE(MVSIZ),
     .        EX(4,MVSIZ), EY(4,MVSIZ), EZ(4,MVSIZ), FX(MVSIZ), FY(MVSIZ), FZ(MVSIZ)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NN, J, JRM, K, KRM, I1, J1, I2, J2, EJ, ES,
     .         IE, JE, SOL_EDGE, SH_EDGE
      INTEGER IAM(MVSIZ),JAM(4,MVSIZ),IAS(MVSIZ),JAS(MVSIZ)
      my_real
     .   AAA, DX, DY, DZ, DD, NNI, NI2, INVCOS, GAPE_M(MVSIZ), GAPE_S(MVSIZ)
C-----------------------------------------------
      DO I=1,JLT
        IF(CAND_S(I)<=NRTS) THEN

          ES    =CAND_S(I)
          IAS(I)=LEDGE(1,ES)
          JAS(I)=LEDGE(2,ES)
          N1(I)=LEDGE(5,ES)
          N2(I)=LEDGE(6,ES)

          XXS1(I) = X(1,N1(I))
          XYS1(I) = X(2,N1(I))
          XZS1(I) = X(3,N1(I))
          XXS2(I) = X(1,N2(I))
          XYS2(I) = X(2,N2(I))
          XZS2(I) = X(3,N2(I))

          IAM(I)=CAND_M(I)
          DO EJ=1,4
            JAM(EJ,I)=EJ
            M1(EJ,I)=IRECT(EJ,IAM(I))
            M2(EJ,I)=IRECT(MOD(EJ,4)+1,IAM(I))

            XXM1(EJ,I) = X(1,M1(EJ,I))
            XYM1(EJ,I) = X(2,M1(EJ,I))
            XZM1(EJ,I) = X(3,M1(EJ,I))
            XXM2(EJ,I) = X(1,M2(EJ,I))
            XYM2(EJ,I) = X(2,M2(EJ,I))
            XZM2(EJ,I) = X(3,M2(EJ,I))
          END DO
        END IF
      END DO

      DO I=1,JLT
        GAPE_M(I)=ZERO ! Solids 
                       ! If edge is shared by solid and shell : edge is considered as a shell edge
          IF(CAND_S(I)<=NRTS) THEN
          GAPE_S(I)=GAPE(CAND_S(I))
          END IF
        GAPVE(I)=ZERO
      END DO

C
      SOL_EDGE=IEDGE/10 ! solids
      SH_EDGE =IEDGE-10*SOL_EDGE ! shells

      DO I=1,JLT
        DO EJ=1,4
          EX(EJ,I)=EDG_BISECTOR(1,EJ,IAM(I))
          EY(EJ,I)=EDG_BISECTOR(2,EJ,IAM(I))
          EZ(EJ,I)=EDG_BISECTOR(3,EJ,IAM(I))
        END DO
      END DO
C
      DO I=1,JLT
        FX(I) = EDG_BISECTOR(1,JAS(I),IAS(I))
        FY(I) = EDG_BISECTOR(2,JAS(I),IAS(I))
        FZ(I) = EDG_BISECTOR(3,JAS(I),IAS(I))
      END DO
C
      RETURN
      END
