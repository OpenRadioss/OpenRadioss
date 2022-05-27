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
Chd|  I25DST3_3                     source/interfaces/int25/i25dst3_3.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25DST3_3(
     1 JLT         ,CAND_N      ,CAND_E      ,CN_LOC      ,CE_LOC      ,
     2 IRTLM       ,XX          ,YY          ,ZZ          ,GAP_NM      ,
     3 XI          ,YI          ,ZI          ,GAPS        ,GAPMXL      ,
     4 ISHARP      ,NNX         ,NNY         ,NNZ         ,
     5 N1          ,N2          ,N3          ,H1          ,H2          ,
     5 H3          ,H4          ,NIN         ,NSN         ,IX1         ,
     6 IX2         ,IX3         ,IX4         ,NSVG        ,STIF        ,
     7 INACTI      ,KINI        ,ITAB        ,LB          ,LC          ,
     8 PENMIN      ,EPS         ,PENE        ,PENE_OLD    ,SUBTRIA     , 
     9 GAPV        ,IVIS2       ,IF_ADH      ,IFADHI      ,BASE_ADH    ,
     A MVOISN      ,IBOUND      ,VTX_BISECTOR,DIST       )
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
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, INACTI, IVIS2, ISHARP, ITAB(*),
     .        CAND_N(*),CAND_E(*),
     .        MVOISN(MVSIZ,4),IBOUND(4,*)
      INTEGER NSVG(MVSIZ), KINI(MVSIZ), CN_LOC(MVSIZ), CE_LOC(MVSIZ),
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), SUBTRIA(*), IRTLM(4,*),
     .        IF_ADH(*), IFADHI(MVSIZ)
      my_real
     .     PENMIN, EPS, PENE_OLD(5,*)
      my_real
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5), 
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), 
     .     NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .     LB(MVSIZ), LC(MVSIZ), GAP_NM(4,MVSIZ), GAPS(MVSIZ), 
     .     PENE(MVSIZ), GAPMXL(MVSIZ), GAPV(MVSIZ), BASE_ADH(MVSIZ)
      REAL*4 VTX_BISECTOR(3,2,*)
      my_real  , INTENT(INOUT) :: DIST(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, I1, I2, JG, IT, ITRIA(2,4), I3, I4,
     .        IB1, IB2, IB3, IBX, IX, IY, IZ, NBORD, KBORD(MVSIZ)
      my_real
     .        AAA, NNI, NI2, H0, PENE_SHFT,
     .        NN, NNE(MVSIZ), XH(MVSIZ), YH(MVSIZ), ZH(MVSIZ), XC, YC, ZC, DC, P1, P2, GAPM,
     .        BB(MVSIZ), LA(MVSIZ)
      my_real
     .        NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .        NN1(MVSIZ), NN2(MVSIZ), NN3(MVSIZ), DD, S,
     .        EPSEG, AX, BX, CX, GAP_MM(MVSIZ),
     .        LBS(MVSIZ), LCS(MVSIZ), XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ), 
     .        X0N(MVSIZ,4), Y0N(MVSIZ,4), Z0N(MVSIZ,4), 
     .        N1X,N1Y,N1Z,N1N,
     .        N2X,N2Y,N2Z,N2N,
     .        LINTERP, LBM, LCM, LAM, XL, YL, ZL,
     .        PN, VX, VY, VZ,
     .        NNE1,NNE2,NNE4
      DATA ITRIA/1,2,2,3,3,4,4,1/
C--------------------------------------------------------
C     zone limite interpolation des normales
      EPSEG = (TWO+HALF)/HUNDRED
C--------------------------------------------------------
      DO I=1,JLT
C    
        X0N(I,1) = XX(I,1) - XX(I,5)
        Y0N(I,1) = YY(I,1) - YY(I,5)
        Z0N(I,1) = ZZ(I,1) - ZZ(I,5)
C
        X0N(I,2) = XX(I,2) - XX(I,5)
        Y0N(I,2) = YY(I,2) - YY(I,5)
        Z0N(I,2) = ZZ(I,2) - ZZ(I,5)
C
        X0N(I,3) = XX(I,3) - XX(I,5)
        Y0N(I,3) = YY(I,3) - YY(I,5)
        Z0N(I,3) = ZZ(I,3) - ZZ(I,5)
C
        X0N(I,4) = XX(I,4) - XX(I,5)
        Y0N(I,4) = YY(I,4) - YY(I,5)
        Z0N(I,4) = ZZ(I,4) - ZZ(I,5)
C
        IF(IX3(I)/=IX4(I))THEN
          GAP_MM(I)=FOURTH*(GAP_NM(1,I)+GAP_NM(2,I)+GAP_NM(3,I)+GAP_NM(4,I))
        ELSE
          GAP_MM(I)=GAP_NM(3,I)
        END IF
C
      ENDDO
C--------------------------------------------------------
C normales aux triangles (recalculees ici pour pas les stocker).
C--------------------------------------------------------
      DO I=1,JLT
C    
        IT = SUBTRIA(I)
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        NX(I) = Y0N(I,I1)*Z0N(I,I2) - Z0N(I,I1)*Y0N(I,I2)
        NY(I) = Z0N(I,I1)*X0N(I,I2) - X0N(I,I1)*Z0N(I,I2)
        NZ(I) = X0N(I,I1)*Y0N(I,I2) - Y0N(I,I1)*X0N(I,I2)
C
        NN=ONE/MAX(EM30,SQRT(NX(I)*NX(I)+ NY(I)*NY(I)+ NZ(I)*NZ(I)))
        NX(I)=NX(I)*NN
        NY(I)=NY(I)*NN
        NZ(I)=NZ(I)*NN
C
      ENDDO
C--------------------------------------------------------
C cas general
C--------------------------------------------------------
      DO I=1,JLT
C    
        IT = SUBTRIA(I)
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        LA(I)=ONE-LB(I)-LC(I)
C
        GAPV(I)= MIN(GAPS(I)+LA(I)*GAP_MM(I)+LB(I)*GAP_NM(I1,I)+LC(I)*GAP_NM(I2,I),GAPMXL(I))
        BB(I)  = (XX(I,5)-XI(I))*NX(I)+(YY(I,5)-YI(I))*NY(I)+(ZZ(I,5)-ZI(I))*NZ(I)
C
        IF(IX3(I)/=IX4(I))THEN

          IF(BB(I) <= ZERO)THEN

            XP(I) =LA(I)*XX(I,5)+LB(I)*XX(I,I1)+LC(I)*XX(I,I2)
            YP(I) =LA(I)*YY(I,5)+LB(I)*YY(I,I1)+LC(I)*YY(I,I2)
            ZP(I) =LA(I)*ZZ(I,5)+LB(I)*ZZ(I,I1)+LC(I)*ZZ(I,I2)
            NN1(I) =XI(I)-XP(I)
            NN2(I) =YI(I)-YP(I)
            NN3(I) =ZI(I)-ZP(I)
            DD = SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I))
            IF(DD > EM03) THEN
              NN = ONE/DD
              N1(I) = NN1(I)*NN
              N2(I) = NN2(I)*NN
              N3(I) = NN3(I)*NN
            ELSE
              N1(I) = NX(I)
              N2(I) = NY(I)
              N3(I) = NZ(I)
            END IF
            PENE(I)=MAX(ZERO,GAPV(I)-DD)
            DIST(I)=DD
          ELSE

            IF(BB(I) > ZERO .AND. LA(I) < EPSEG .AND. MVOISN(I,IT)/=0)THEN
C
C             zone limite interpolation des normales
C
C             rota solides works well ::
              NN1(I)=NNX(I,IT)
              NN2(I)=NNY(I,IT)
              NN3(I)=NNZ(I,IT)
C
              NNI = NX(I)*NN1(I)  + NY(I)*NN2(I)  + NZ(I)*NN3(I)
              NI2 = NN1(I)*NN1(I) + NN2(I)*NN2(I) + NN3(I)*NN3(I)
              IF(NNI < ZERO .OR. TWO*NNI*NNI < NI2)THEN
c               scharp angle bound nodal normal to 45 from segment normal
                AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
                NN1(I) = NN1(I) + AAA*NX(I)
                NN2(I) = NN2(I) + AAA*NY(I)
                NN3(I) = NN3(I) + AAA*NZ(I)
              ENDIF
              NN = ONE/
     .             MAX(EM30,SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I)))
              NN1(I)=NN1(I)*NN
              NN2(I)=NN2(I)*NN
              NN3(I)=NN3(I)*NN
C
              S   = LA(I)/EPSEG
C
C continuite de la normale a la frontiere de la zone limite d'interpolation
              NN1(I)=(ONE-S)*NN1(I)+S*NX(I)
              NN2(I)=(ONE-S)*NN2(I)+S*NY(I)
              NN3(I)=(ONE-S)*NN3(I)+S*NZ(I)
              NN = ONE/
     .             MAX(EM30,SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I)))
              NN1(I)=NN1(I)*NN
              NN2(I)=NN2(I)*NN
              NN3(I)=NN3(I)*NN
C
              N1(I) = NN1(I)
              N2(I) = NN2(I)
              N3(I) = NN3(I)
C
              XP(I)  =LA(I)*XX(I,5)+LB(I)*XX(I,I1)+LC(I)*XX(I,I2)
              YP(I)  =LA(I)*YY(I,5)+LB(I)*YY(I,I1)+LC(I)*YY(I,I2)
              ZP(I)  =LA(I)*ZZ(I,5)+LB(I)*ZZ(I,I1)+LC(I)*ZZ(I,I2)
C
C             PENE(I)=MAX(ZERO,GAPV(I)+(XP(I)-XI(I))*N1(I)+(YP(I)-YI(I))*N2(I)+(ZP(I)-ZI(I))*N3(I))
              PENE(I)=MAX(ZERO,GAPV(I)+BB(I)) ! The distance against the normal estimates the penetration
C
              DIST(I)=BB(I)
            ELSE
C
C             All other cases : normal == normal to the segment
              N1(I) = NX(I)
              N2(I) = NY(I)
              N3(I) = NZ(I)
              PENE(I)=MAX(ZERO,GAPV(I)+BB(I))
              DIST(I)=BB(I)
            END IF

          END IF 

        ELSEIF(IX3(I)==IX4(I))THEN

          IF(BB(I) <= ZERO)THEN

            XP(I) =LA(I)*XX(I,5)+LB(I)*XX(I,I1)+LC(I)*XX(I,I2)
            YP(I) =LA(I)*YY(I,5)+LB(I)*YY(I,I1)+LC(I)*YY(I,I2)
            ZP(I) =LA(I)*ZZ(I,5)+LB(I)*ZZ(I,I1)+LC(I)*ZZ(I,I2)
            NN1(I) =XI(I)-XP(I)
            NN2(I) =YI(I)-YP(I)
            NN3(I) =ZI(I)-ZP(I)
            DD = SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I))
            IF(DD > EM03) THEN
              NN = ONE/DD
              N1(I) = NN1(I)*NN
              N2(I) = NN2(I)*NN
              N3(I) = NN3(I)*NN
            ELSE
              N1(I) = NX(I)
              N2(I) = NY(I)
              N3(I) = NZ(I)
            END IF
            PENE(I)=MAX(ZERO,GAPV(I)-DD)
            DIST(I)=DD

          ELSEIF(BB(I) > ZERO .AND. ((LA(I) < EPSEG .AND. MVOISN(I,1)/=0).OR.
     .                               (LB(I) < EPSEG .AND. MVOISN(I,2)/=0).OR. 
     .                               (LC(I) < EPSEG .AND. MVOISN(I,4)/=0)))THEN
C
C           zone limite interpolation des normales
            IF(LA(I) < EPSEG .AND. MVOISN(I,1)/=0)THEN
              AAA=LB(I)+LC(I)
              AX=ZERO
              BX=LB(I)/AAA
              CX=LC(I)/AAA
              S = LA(I)/EPSEG
            ELSEIF(LB(I) < EPSEG .AND. MVOISN(I,2)/=0)THEN
              AAA=LA(I)+LC(I)
              AX=LA(I)/AAA
              BX=ZERO
              CX=LC(I)/AAA
              S = LB(I)/EPSEG
            ELSEIF(LC(I) < EPSEG .AND. MVOISN(I,4)/=0)THEN
              AAA=LA(I)+LB(I)
              AX=LA(I)/AAA
              BX=LB(I)/AAA
              CX=ZERO
              S = LC(I)/EPSEG
            END IF
            NN1(I)=(BX+CX-AX)*NNX(I,I1)+(AX+CX-BX)*NNX(I,I2)+(AX+BX-CX)*NNX(I,5)
            NN2(I)=(BX+CX-AX)*NNY(I,I1)+(AX+CX-BX)*NNY(I,I2)+(AX+BX-CX)*NNY(I,5)
            NN3(I)=(BX+CX-AX)*NNZ(I,I1)+(AX+CX-BX)*NNZ(I,I2)+(AX+BX-CX)*NNZ(I,5)
C
            NNI = NX(I)*NN1(I)  + NY(I)*NN2(I)  + NZ(I)*NN3(I)
            NI2 = NN1(I)*NN1(I) + NN2(I)*NN2(I) + NN3(I)*NN3(I)
            IF(NNI < ZERO .OR. TWO*NNI*NNI < NI2)THEN
c             scharp angle bound nodal normal to 45 from segment normal
              AAA = SQRT(MAX(ZERO,NI2-NNI*NNI)) - NNI
              NN1(I) = NN1(I) + AAA*NX(I)
              NN2(I) = NN2(I) + AAA*NY(I)
              NN3(I) = NN3(I) + AAA*NZ(I)
            ENDIF
            NN = ONE/
     .           MAX(EM30,SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I)))
            NN1(I)=NN1(I)*NN
            NN2(I)=NN2(I)*NN
            NN3(I)=NN3(I)*NN
C
C continuite de la normale a la frontiere de la zone limite d'interpolation
            NN1(I)=(ONE-S)*NN1(I)+S*NX(I)
            NN2(I)=(ONE-S)*NN2(I)+S*NY(I)
            NN3(I)=(ONE-S)*NN3(I)+S*NZ(I)
            NN = ONE/
     .           MAX(EM30,SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I)))
            NN1(I)=NN1(I)*NN
            NN2(I)=NN2(I)*NN
            NN3(I)=NN3(I)*NN
C
            N1(I) = NN1(I)
            N2(I) = NN2(I)
            N3(I) = NN3(I)
C
            XP(I)  =LA(I)*XX(I,5)+LB(I)*XX(I,I1)+LC(I)*XX(I,I2)
            YP(I)  =LA(I)*YY(I,5)+LB(I)*YY(I,I1)+LC(I)*YY(I,I2)
            ZP(I)  =LA(I)*ZZ(I,5)+LB(I)*ZZ(I,I1)+LC(I)*ZZ(I,I2)
C
C           PENE(I)=MAX(ZERO,GAPV(I)+(XP(I)-XI(I))*N1(I)+(YP(I)-YI(I))*N2(I)+(ZP(I)-ZI(I))*N3(I))
            PENE(I)=MAX(ZERO,GAPV(I)+BB(I)) ! The distance against the normal estimates the penetration
            DIST(I)=BB(I)
C
          ELSE
C
C           All other cases : normal == normal to the segment
            N1(I) = NX(I)
            N2(I) = NY(I)
            N3(I) = NZ(I)
            PENE(I)=MAX(ZERO,GAPV(I)+BB(I))
            DIST(I)=BB(I)
C
          END IF

C
        END IF

C-------------------------------------------
c        if(itab(nsvg(i))==2810875.or.
c     .     itab(ix1(i))==2810875.or.itab(ix2(i))==2810875.or.itab(ix3(i))==2810875.or.itab(ix4(i))==2810875)
c     . print *,'dst3-avant',itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),mvoisn(i,1:4),
c     .   pene(i),pene_old(5,cand_n(i))
C-------------------------------------------
c         if(itab(nsvg(i))==10105970)
c     . print *,'dst3-avant',itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),mvoisn(i,1:4),
c     .   pene(i),pene_old(5,cand_n(i))
      END DO
C--------------------------------------------------------
C cas particulier au bord
C--------------------------------------------------------
      NBORD = 0
      DO I=1,JLT
C    
        IT = SUBTRIA(I)
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        IF(IX3(I)/=IX4(I))THEN

          IB1=IBOUND(I1,I)
          IB2=IBOUND(I2,I)
C
C         Projection sur le plan et Distance signee au plan
          XH(I)=XI(I)+BB(I)*NX(I)
          YH(I)=YI(I)+BB(I)*NY(I)
          ZH(I)=ZI(I)+BB(I)*NZ(I)

          IF(MVOISN(I,IT)==0)THEN
C
C             Distance signee a l'arete
C
C upper skin      --------------------
C                                     |
C                            I        |
C                       -BB: |        |
C                            |   NNE  |
C neutral fiber - - - C - -  H < - - >
C                     <-------------->|
C                          Gap        |
C                                     |
C                 --------------------
C
            NN1(I)=NNX(I,IT)
            NN2(I)=NNY(I,IT)
            NN3(I)=NNZ(I,IT)
            NNE(I)= (XH(I)-XX(I,I1))*NN1(I)+ (YH(I)-YY(I,I1))*NN2(I)+ (ZH(I)-ZZ(I,I1))*NN3(I)
C
            NBORD=NBORD+1
            KBORD(NBORD)=I
C
          ELSEIF((IB1/=0.AND.IB2==0).OR.
     .           (IB2/=0.AND.IB1==0))THEN
C
            IBX=MAX(IB1,IB2)
            IF(IB1/=0)THEN
              IX =I1
            ELSEIF(IB2/=0)THEN
              IX =I2
            END IF

            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
C
              P1 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
C
              IF(P1 < GAPS(I) .AND. P2 < GAPS(I))THEN
C
                NN1(I)=VTX_BISECTOR(1,1,IBX)+VTX_BISECTOR(1,2,IBX)
                NN2(I)=VTX_BISECTOR(2,1,IBX)+VTX_BISECTOR(2,2,IBX)
                NN3(I)=VTX_BISECTOR(3,1,IBX)+VTX_BISECTOR(3,2,IBX)
C
                NN=SQRT(NN1(I)*NN1(I)+NN2(I)*NN2(I)+NN3(I)*NN3(I))
                NN = ONE/MAX(EM30,NN)
                NN1(I)=NN1(I)*NN
                NN2(I)=NN2(I)*NN
                NN3(I)=NN3(I)*NN
C
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
                NBORD=NBORD+1
                KBORD(NBORD)=I
C
              ELSEIF(P1 < GAPS(I))THEN
                
                NN1(I)= VTX_BISECTOR(1,1,IBX)
                NN2(I)= VTX_BISECTOR(2,1,IBX)
                NN3(I)= VTX_BISECTOR(3,1,IBX)
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)

                NBORD=NBORD+1
                KBORD(NBORD)=I

              ELSEIF(P2 < GAPS(I))THEN
                
                NN1(I)= VTX_BISECTOR(1,2,IBX)
                NN2(I)= VTX_BISECTOR(2,2,IBX)
                NN3(I)= VTX_BISECTOR(3,2,IBX)
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)

                NBORD=NBORD+1
                KBORD(NBORD)=I

              ELSE

c                IF(NSVG(I) > 0)THEN
c                  print *,' ** possible internal error wrt p1,p2 in i25dst3-3',ib1,ib2,
c     .            itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),p1,p2
c                ELSE
c                  print *,' ** possible internal error wrt p1,p2 in i25dst3-3',ib1,ib2,
c     .            itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),p1,p2
c                END IF

              END IF
C
            ELSE ! IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.

              VX = X0N(I,IX) ! fake bisector of angle at vertex IX
              VY = Y0N(I,IX)
              VZ = Z0N(I,IX)
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH(I)-XX(I,IX))*VX+(YH(I)-YY(I,IX))*VY+(ZH(I)-ZZ(I,IX))*VZ)*NN
              IF(PN < GAPS(I))THEN
                
                NN1(I)= VX*NN
                NN2(I)= VY*NN
                NN3(I)= VZ*NN
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)

                NBORD=NBORD+1
                KBORD(NBORD)=I

              ELSE

c                IF(NSVG(I) > 0)THEN
c                  print *,' ** possible internal error wrt pn in i25dst3-3',ib1,ib2,
c     .            itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),pn
c                ELSE
c                  print *,' ** possible internal error wrt pn in i25dst3-3',ib1,ib2,
c     .            itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),pn
c                END IF

              END IF
C
            END IF ! IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.

          END IF
 
        ELSEIF(IX3(I)==IX4(I))THEN

          IB1=IBOUND(1,I)
          IB2=IBOUND(2,I)
          IB3=IBOUND(3,I)
C
C         Projection sur le plan et Distance signee au plan
          XH(I)=XI(I)+BB(I)*NX(I)
          YH(I)=YI(I)+BB(I)*NY(I)
          ZH(I)=ZI(I)+BB(I)*NZ(I)

          IF(MVOISN(I,1)==0.OR.
     .       MVOISN(I,2)==0.OR.
     .       MVOISN(I,4)==0)THEN

            NNE(I)=GAPS(I)
              NNE1 = (XH(I)-XX(I,I1))*NNX(I,I1)+(YH(I)-YY(I,I1))*NNY(I,I1)+(ZH(I)-ZZ(I,I1))*NNZ(I,I1)
              NNE2 = (XH(I)-XX(I,I2))*NNX(I,I2)+(YH(I)-YY(I,I2))*NNY(I,I2)+(ZH(I)-ZZ(I,I2))*NNZ(I,I2)
              NNE4 = (XH(I)-XX(I,5))*NNX(I,5)+(YH(I)-YY(I,5))*NNY(I,5)+(ZH(I)-ZZ(I,5))*NNZ(I,5)


            IF((MVOISN(I,1)==0 .AND. NNE1 < NNE(I)) .OR.
     .        (MVOISN(I,2)==0 .AND. NNE2 < NNE(I)) .OR.
     .        (MVOISN(I,4)==0 .AND. NNE4 < NNE(I))) THEN

              NBORD=NBORD+1
              KBORD(NBORD)=I

              IF(MVOISN(I,1) == 0 .AND. NNE1 < NNE(I)) THEN
                  NNE(I)=NNE1
                  NN1(I)=NNX(I,I1)
                  NN2(I)=NNY(I,I1)
                  NN3(I)=NNZ(I,I1)                
              ENDIF
              IF(MVOISN(I,2) == 0 .AND. NNE2 < NNE(I))THEN
C               Distance signee a l'arete
                  NNE(I)=NNE2
                  NN1(I)=NNX(I,I2)
                  NN2(I)=NNY(I,I2)
                  NN3(I)=NNZ(I,I2)                
              ENDIF
              IF(MVOISN(I,4) == 0 .AND. NNE4 < NNE(I)) THEN
                  NNE(I)=NNE4
                  NN1(I)=NNX(I,5)
                  NN2(I)=NNY(I,5)
                  NN3(I)=NNZ(I,5)                
              END IF
            ENDIF
C
          ELSEIF((IB1/=0 .AND. IB2==0 .AND. IB3==0).OR.
     .           (IB2/=0 .AND. IB3==0 .AND. IB1==0).OR.
     .           (IB3/=0 .AND. IB1==0 .AND. IB2==0))THEN
C
            IBX=MAX(IB1,IB2,IB3)
            IF(IB1/=0)THEN
              IX =1
              IY =2
              IZ =3
            ELSEIF(IB2/=0)THEN
              IX =2
              IY =3
              IZ =1
            ELSEIF(IB3/=0)THEN
              IX =3
              IY =1
              IZ =2
            END IF
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)

              IF(P1 < GAPS(I) .AND. P2 < GAPS(I))THEN
C
                NN1(I)=VTX_BISECTOR(1,1,IBX)+VTX_BISECTOR(1,2,IBX)
                NN2(I)=VTX_BISECTOR(2,1,IBX)+VTX_BISECTOR(2,2,IBX)
                NN3(I)=VTX_BISECTOR(3,1,IBX)+VTX_BISECTOR(3,2,IBX)
C
                NN=SQRT(NN1(I)*NN1(I)+NN2(I)*NN2(I)+NN3(I)*NN3(I))
                NN = ONE/MAX(EM30,NN)
                NN1(I)=NN1(I)*NN
                NN2(I)=NN2(I)*NN
                NN3(I)=NN3(I)*NN
C
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
                NBORD=NBORD+1
                KBORD(NBORD)=I
C
              ELSEIF(P1 < GAPS(I))THEN
                
                NN1(I)= VTX_BISECTOR(1,1,IBX)
                NN2(I)= VTX_BISECTOR(2,1,IBX)
                NN3(I)= VTX_BISECTOR(3,1,IBX)
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)

                NBORD=NBORD+1
                KBORD(NBORD)=I

              ELSEIF(P2 < GAPS(I))THEN
                
                NN1(I)= VTX_BISECTOR(1,2,IBX)
                NN2(I)= VTX_BISECTOR(2,2,IBX)
                NN3(I)= VTX_BISECTOR(3,2,IBX)
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)

                NBORD=NBORD+1
                KBORD(NBORD)=I

              ELSE

c                IF(NSVG(I) > 0)THEN
c                  print *,' ** possible internal error wrt p1,p2 in i25dst3-3',ib1,ib2,
c     .            itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),p1,p2
c                ELSE
c                  print *,' ** possible internal error wrt p1,p2 in i25dst3-3',ib1,ib2,
c     .            itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),p1,p2
c                END IF

              END IF

            ELSE ! IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.

              VX = TWO*XX(I,IX)-(XX(I,IY)+XX(I,IZ)) ! fake bisector of angle 2,1,3
              VY = TWO*YY(I,IX)-(YY(I,IY)+YY(I,IZ))
              VZ = TWO*ZZ(I,IX)-(ZZ(I,IY)+ZZ(I,IZ))
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH(I)-XX(I,IX))*VX+(YH(I)-YY(I,IX))*VY+(ZH(I)-ZZ(I,IX))*VZ)*NN

              IF(PN < GAPS(I))THEN
                
                NN1(I)= VX*NN
                NN2(I)= VY*NN
                NN3(I)= VZ*NN
                NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)

                NBORD=NBORD+1
                KBORD(NBORD)=I

              ELSE

c                IF(NSVG(I) > 0)THEN
c                  print *,' ** possible internal error wrt pn in i25dst3-3',ib1,ib2,
c     .            itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),pn
c                ELSE
c                  print *,' ** possible internal error wrt pn in i25dst3-3',ib1,ib2,
c     .            itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),pn
c                END IF

              END IF
 
            END IF ! IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.

          END IF
C
        END IF

      END DO
C-------------------------------------------
      IF(ISHARP==3)THEN

        DO K=1,NBORD
          I=KBORD(K)

          IF(NNE(I) > ZERO)THEN
            PENE(I)=ZERO
          END IF

        END DO
      ELSEIF(ISHARP==1)THEN

        DO K=1,NBORD
          I=KBORD(K)

          GAPM = GAPV(I)-GAPS(I)
          IF(NNE(I) > ZERO .AND. BB(I)+GAPM < ZERO)THEN
C
C           ___________________________ xxx
C                                      |xxxxxx     
C            GapS                      |xxxxxxxx   <=> Upper Round Corner
C           ___________________________|xxxxxxxx
C                                      |       |
C            GapM                      |       |   <=> Straight shape of the edge
C           ---------------------------M-------
C                                      <------> 
C                                         GapS
C

            XC=XP(I)+GAPM*NX(I)
            YC=YP(I)+GAPM*NY(I)
            ZC=ZP(I)+GAPM*NZ(I)
            N1(I) =XI(I)-XC
            N2(I) =YI(I)-YC
            N3(I) =ZI(I)-ZC
            DC = SQRT(N1(I)*N1(I)+ N2(I)*N2(I)+ N3(I)*N3(I))

            IF(DC > EM04) THEN
              NN = ONE/DC
              N1(I) = N1(I)*NN
              N2(I) = N2(I)*NN
              N3(I) = N3(I)*NN
              PENE(I)=MAX(ZERO,GAPS(I)-DC)
            ELSE
C
              NNE(I)=NNE(I)-GAPS(I)
              IF(-BB(I) < GAPV(I)+NNE(I))THEN ! Test 45 degres !
C
C               Normale horizonale
                N1(I) = NN1(I)
                N2(I) = NN2(I)
                N3(I) = NN3(I)
C
                PENE(I)=MAX(ZERO,-NNE(I))
C                
              ELSE
C
C               Normale verticale !
                N1(I) = NX(I)
                N2(I) = NY(I)
                N3(I) = NZ(I)
C
                PENE(I)=MAX(ZERO,GAPV(I)+BB(I))
              END IF
            END IF

          ELSE !IF(NNE(I) > ZERO .AND. BB(I)+GAPM < ZERO)THEN

            NNE(I)=NNE(I)-GAPS(I)
            IF(NNE(I) >= ZERO)THEN
C
C             Must remain a roundoff error <=> NNE ~ 0
c             IF(NSVG(I) > 0)THEN
c               print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .         itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I),pene(i)
c             ELSE
c               print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .         itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I),pene(i)
c             END IF

              PENE(I)=ZERO
              CYCLE

            END IF
C
            IF(GAPV(I)+NNE(I) > ZERO)THEN

              IF(-BB(I) < GAPV(I)+NNE(I))THEN ! Test 45 degres !
C
C               Normale horizonale
                N1(I) = NN1(I)
                N2(I) = NN2(I)
                N3(I) = NN3(I)
C
                PENE(I)=-NNE(I)
C                
              ELSE
C
C               Normale verticale !
                N1(I) = NX(I)
                N2(I) = NY(I)
                N3(I) = NZ(I)
C
                PENE(I)=MAX(ZERO,GAPV(I)+BB(I))
              END IF

            END IF

          END IF
C-------------------------------------------
c       if(itab(nsvg(i))==10105970)
c     .      print *,'dst3-apres',itab(nsvg(i)),kbord,pene(i),pene_old(5,cand_n(i)),n1(i),n2(i),n3(i),
c     .   nne(I),bb(I),gapv(i)
C-------------------------------------------
        END DO

      ELSEIF(ISHARP==2)THEN

        DO K=1,NBORD
          I=KBORD(K)

          NNE(I)=NNE(I)-GAPS(I)
          IF(NNE(I) >= ZERO)THEN
C
C           Must remain a roundoff error <=> NNE ~ 0
c            IF(NSVG(I) > 0)THEN
c              print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .        itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I)
c            ELSE
c              print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .        itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I)
c            END IF

            CYCLE

          END IF
C
          IF(BB(I) > ZERO)THEN

            IF(-BB(I) < GAPV(I)+NNE(I))THEN ! Test 45 degres !
C
C             Normale horizonale
              N1(I) = NN1(I)
              N2(I) = NN2(I)
              N3(I) = NN3(I)
C
              PENE(I)=-NNE(I)
C              
            ELSE
C
C             Normale verticale !
              N1(I) = NX(I)
              N2(I) = NY(I)
              N3(I) = NZ(I)
C
              PENE(I)=MAX(ZERO,GAPV(I)+BB(I))
            END IF

          ELSEIF(GAPV(I)+NNE(I) > ZERO)THEN
C
C           Normale radiale shiftee
            XC =XH(I)-(GAPV(I)+NNE(I))*NN1(I)
            YC =YH(I)-(GAPV(I)+NNE(I))*NN2(I)
            ZC =ZH(I)-(GAPV(I)+NNE(I))*NN3(I)
            NN1(I) =XI(I)-XC
            NN2(I) =YI(I)-YC
            NN3(I) =ZI(I)-ZC
            DC = SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I))
            IF(DC > EM04) THEN
              NN = ONE/DC
              N1(I) = NN1(I)*NN
              N2(I) = NN2(I)*NN
              N3(I) = NN3(I)*NN
              PENE(I)=MAX(ZERO,GAPV(I)-DC)
            ELSE
C
C             Keep what was done in the general case : Normale ~ verticale.
            END IF

          END IF
C-------------------------------------------
c       if(itab(nsvg(i))==10105970)
c     .      print *,'dst3-apres',itab(nsvg(i)),kbord,pene(i),pene_old(5,cand_n(i)),n1(i),n2(i),n3(i),
c     .   nne(I),bb(i),gapv(i)
C-------------------------------------------
        ENDDO
      END IF
C--------------------------------------------------------
C      Calculer PENE et Hi
C--------------------------------------------------------
      DO I=1,JLT
C
        CE_LOC(I) =  CAND_E(I)
        CN_LOC(I) =  CAND_N(I)
C          
        IT = SUBTRIA(I)
C
        IF(IX3(I).NE.IX4(I))THEN
C
          H0 = FOURTH * LA(I)
          IF(IT==1)THEN
            H1(I) = H0 + LB(I) 
            H2(I) = H0 + LC(I)
            H3(I) = H0 
            H4(I) = H0 
          ELSEIF(IT==2)THEN
            H1(I) = H0 
            H2(I) = H0 + LB(I) 
            H3(I) = H0 + LC(I)
            H4(I) = H0
          ELSEIF(IT==3)THEN
            H1(I) = H0
            H2(I) = H0
            H3(I) = H0 + LB(I) 
            H4(I) = H0 + LC(I)
          ELSEIF(IT==4)THEN
            H1(I) = H0 + LC(I)
            H2(I) = H0 
            H3(I) = H0 
            H4(I) = H0 + LB(I) 
          END IF
C
        ELSE
C
          H1(I) = LB(I)
          H2(I) = LC(I)
          H3(I) = LA(I)
          H4(I) = ZERO
C
        END IF
C
      END DO
C
C-----------------------------------------------------------------------
C      PENE_OLD(5) <=> Initial Penetration
C-----------------------------------------------------------------------
      IF(IVIS2/=-1) THEN
C       IF(INACTI==5)THEN
        DO I=1,JLT
C
         IF(PENE(I) == ZERO)CYCLE 
C
         JG = NSVG(I)
         N  = CAND_N(I)
         IF(JG > 0)THEN
           IF(IRTLM(1,N) < 0)THEN ! new impact
             PENE_OLD(5,N)=PENE(I)
           END IF
           PENE_SHFT = MAX(ZERO,PENE(I)-PENE_OLD(5,N))
C         if(itab(jg)== 29687 .OR. ITAB(IX1(I)) == 29687 .OR. ITAB(IX2(I)) == 29687
C    . .OR. ITAB(IX4(I)) == 29687 .OR. ITAB(IX3(I)) == 29687)
C    .    THEN
C              print *,ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)),
C    . ITAB(JG),PENE_OLD(5,N),pene_shft
C         ENDIF
          ELSE
           JG = -JG
           IF(IRTLM_FI(NIN)%P(1,JG) < 0)THEN ! new impact
             PENE_OLDFI(NIN)%P(5,JG)=PENE(I)
           END IF
           PENE_SHFT= MAX(ZERO,PENE(I)-PENE_OLDFI(NIN)%P(5,JG))
C         if(itafi(nin)%p(jg)==29687 .OR. ITAB(IX1(I)) == 29687 .OR. ITAB(IX2(I)) == 29687
C    ..OR.  ITAB(IX4(I)) == 29687 .OR. ITAB(IX3(I)) == 29687) THEN
C              print *,ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)),
C    . ITAB(JG),PENE_OLDFI(NIN)%P(5,JG),pene_shft
C         endif

         ENDIF
         PENE(I) = PENE_SHFT
        ENDDO
C       END IF !IF (INACTI==5)
C pene_old(5,N) is defined from the real_gap, gap = 2*real_gap, pene is measured from real_gap
      ELSE ! IVIS2==-1 (Adhesion case)
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE 
          JG = NSVG(I)
          N  = CAND_N(I)
          IF(JG > 0)THEN
            IF(IRTLM(1,N) < 0)THEN ! new impact
               PENE_OLD(5,N)= MAX(ZERO,PENE(I)-HALF*GAPV(I))              
            ENDIF
            IF(PENE(I).GE.HALF*GAPV(I)) IF_ADH(N) = 1
            BASE_ADH(I) = PENE_OLD(5,N) + HALF*GAPV(I) 
            IFADHI(I) = IF_ADH(N)        
          ELSE
            JG = -JG
            IF(IRTLM_FI(NIN)%P(1,JG) < 0)THEN 
              PENE_OLDFI(NIN)%P(5,JG)= MAX(ZERO,PENE(I)-HALF*GAPV(I))
            ENDIF
            IF(PENE(I).GE.HALF*GAPV(I)) IF_ADHFI(NIN)%P(JG) = 1 
            BASE_ADH(I) = PENE_OLDFI(NIN)%P(5,JG) + HALF*GAPV(I)
            IFADHI(I) = IF_ADHFI(NIN)%P(JG)  
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
