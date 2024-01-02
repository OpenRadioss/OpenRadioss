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
Chd|  I21FOR3                       source/interfaces/int21/i21for3.F
Chd|-- called by -----------
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|-- calls ---------------
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        STRI                          source/tools/univ/stri.F      
Chd|====================================================================
      SUBROUTINE I21FOR3(JLT    ,NIN    ,NOINT  ,IBCC   ,ICODT  ,
     2                  FSAV    ,GAP    ,STIGLO ,FRIC   ,VISC   ,
     3                  INACTI  ,MFROT  ,IFQ    ,IBAG   ,IADM   ,
     4                  ICURV   ,STIF    ,GAPV  ,ITAB   ,
     5                  PENI    ,FROT_P ,ALPHA0 ,IFPEN  ,
     6                  ICONTACT,RCONTACT,ACONTACT,PCONTACT,
     7                  NSVG    ,X1     ,Y1     ,Z1     ,X2     ,
     8                  Y2      ,Z2     ,X3     ,Y3     ,Z3     ,
     9                  X4      ,Y4     ,Z4     ,XI     ,YI     ,
     A                  ZI     ,VXI    ,VYI    ,VZI     ,MSI    ,
     B                  VXM    ,VYM    ,VZM    ,NX      ,NY     ,
     C                  NZ     ,PENE   ,FXT    ,FYT    ,FZT     ,
     D                  FXN    ,FYN    ,FZN    ,RCURVI ,ANGLMI  ,
     E                  PADM   ,CAND_N_N, WEIGHT,IGAP   ,GAP0   ,
     F                  AREA0   ,PMAX   ,IROT   ,XG     ,MXI    ,
     G                  MYI     ,MZI    ,STRI   ,WXM    ,WYM    ,
     H                  WZM     ,XP     ,YP     ,ZP     ,KT     ,
     I                  C       ,ILEV   ,FNI    ,INTTH  ,FHEAT  ,
     J                  EFRICT  ,QFRIC  ,IFRIC  ,XFRIC  ,TEMPI  , 
     k                  TEMPM   ,NPC    ,TF     ,IX1    ,IX2    ,
     L                  IX3     ,IX4    ,DT2T   ,NELTST ,ITYPTST,
     M                  KINET   ,NISUB  ,ISENSINT,FSAVPARIT,NFT ,
     N                  IFTLIM  ,PSKIDFLAG,PRATIO,PMAXSKID,INTEREFRIC,
     O                  EFRIC_L )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "sms_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IBCC, INACTI, IBAG, NIN, NOINT, IADM,
     .        MFROT, IFQ, ICURV(3), IGAP, IROT, ILEV,INTTH,IFRIC,
     .        NELTST,ITYPTST,IFTLIM,
     .        ICODT(*), ITAB(*),IFPEN(*) ,ICONTACT(*), NPC(*),KINET(*)
      INTEGER NSVG(MVSIZ),CAND_N_N(MVSIZ), WEIGHT(*),
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), NISUB, 
     .        ISENSINT(*),NFT,PSKIDFLAG
      INTEGER  , INTENT(IN) :: INTEREFRIC
      my_real
     .   STIGLO, PENI(*), FROT_P(*), FSAV(*), TF(*),
     .   ALPHA0, GAP, FRIC, VISC,DT2T,PMAXSKID ,
     .   FNI(MVSIZ),
     .   FXN(MVSIZ), FYN(MVSIZ), FZN(MVSIZ), 
     .   FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ)
      my_real
     .   STIF(MVSIZ), GAPV(MVSIZ),
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .   X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .   X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .   X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .   X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .   XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),PENE(MVSIZ),
     .   GAP0(MVSIZ), AREA0(MVSIZ), PMAX, FTMAX,
     .   VXM, VYM, VZM, WXM, WYM, WZM,
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ)
      my_real
     .   RCURVI(*), RCONTACT(*), ACONTACT(*),
     .   PCONTACT(*), PADM, ANGLMI(*),
     .   XG(3), MXI(MVSIZ), MYI(MVSIZ), MZI(MVSIZ), STRI(MVSIZ),
     .   KT(MVSIZ), C(MVSIZ), FHEAT,EFRICT(MVSIZ),QFRIC,
     .   TEMPI(MVSIZ),TEMPM(MVSIZ),XFRIC,DYDX,
     .   FSAVPARIT(NISUB+1,11,*),PRATIO(MVSIZ)
      my_real  , INTENT(INOUT) :: EFRIC_L(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2,IBCM,IBCS
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ),
     .   XMU(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), DTMI(MVSIZ), 
     .   VNX, VNY, VNZ, AA, S2,
     .   V2, FF, ALPHA, BETA,
     .   FX, FY, FZ, FT, FN, FMAX, FTN,
     .   ECONTT, ECONVT, FS2,ECONTDT,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8, 
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15,FSAV25,
     .   VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,AREA,P,VV1,VV2,DMU,
     .   DT1INV, VIS, PA, PLIN, FS, QFRICT, DTI,DTI2,DTMI0, MAS2,BETA1
      my_real
     .   PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   PENX(MVSIZ),STIF0(MVSIZ),FRICT(MVSIZ)
      my_real
     .   IMPX,IMPY,IMPZ,XX,YY,ZZ,TH
      my_real 
     .   FINTER 
      EXTERNAL FINTER
C
C-----------------------------------------------
      IF (IRESP==1) THEN
           PREC = FIVEEM4
      ELSE
           PREC = EM10
      ENDIF
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      IF(PSKIDFLAG >0) THEN
         DO I=1,JLT
            PRATIO(I) =  ZERO
         ENDDO
      ENDIF
      EFRIC_L(1:JLT) =  ZERO
      EFRICT(1:JLT) =  ZERO
C---------------------
C      PENE INITIALE
C---------------------
      IF(ILEV==0)THEN
       IF(IGAP<=1)THEN
        IF(INACTI==5)THEN
         DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
           PENI(CAND_N_N(I))=MIN(PENI(CAND_N_N(I)),
     .            ((ONE-FIVEEM2)*PENI(CAND_N_N(I))+FIVEEM2*PENE(I))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
           PENE(I)=MAX(ZERO,PENE(I)-PENI(CAND_N_N(I)))
           IF( PENE(I)==ZERO )  STIF(I) = ZERO
           GAPV(I)=GAPV(I)-PENI(CAND_N_N(I))
         ENDDO
        ELSEIF(INACTI==6)THEN
         DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
           PENI(CAND_N_N(I))=MIN(PENI(CAND_N_N(I)),
     .        ( (ONE-FIVEEM2)*PENI(CAND_N_N(I))
     .          +FIVEEM2*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I))))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
           PENE(I)=MAX(ZERO,PENE(I)-PENI(CAND_N_N(I)))
           IF( PENE(I)==ZERO )  STIF(I) = ZERO
           GAPV(I)=GAPV(I)-PENI(CAND_N_N(I))
         ENDDO
        ELSE
         DO I=1,JLT
           IF( PENE(I)==ZERO )  STIF(I) = ZERO
         ENDDO
        ENDIF
       ELSE
        IF(INACTI==5)THEN
         DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
           PENX(I)=PENE(I)
           IF(PENX(I) > MAX(GAPV(I)-GAP0(I),ZERO))
     .        PENX(I) = PENX(I)-MAX(GAPV(I)-GAP0(I),ZERO)
           PENI(CAND_N_N(I))=MIN(PENI(CAND_N_N(I)),
     .            ((ONE-FIVEEM2)*PENI(CAND_N_N(I))+FIVEEM2*PENX(I))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
           PENE(I)=MAX(ZERO,PENE(I)-PENI(CAND_N_N(I)))
           IF( PENE(I)==ZERO )  STIF(I) = ZERO
           GAPV(I)=GAPV(I)-PENI(CAND_N_N(I))
           GAP0(I)=GAP0(I)-PENI(CAND_N_N(I))
         ENDDO
        ELSEIF(INACTI==6)THEN
         DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
           PENX(I)=PENE(I)
           IF(PENX(I) > MAX(GAPV(I)-GAP0(I),ZERO))
     .        PENX(I) = PENX(I)-MAX(GAPV(I)-GAP0(I),ZERO)
           PENI(CAND_N_N(I))=MIN(PENI(CAND_N_N(I)),
     .        ( (ONE-FIVEEM2)*PENI(CAND_N_N(I))
     .          +FIVEEM2*(PENX(I)+FIVEEM2*(GAP0(I)-PENX(I))))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
           PENE(I)=MAX(ZERO,PENE(I)-PENI(CAND_N_N(I)))
           IF( PENE(I)==ZERO )  STIF(I) = ZERO
           GAPV(I)=GAPV(I)-PENI(CAND_N_N(I))
           GAP0(I)=GAP0(I)-PENI(CAND_N_N(I))
         ENDDO
        ELSE
         DO I=1,JLT
           IF( PENE(I)==ZERO )  STIF(I) = ZERO
         ENDDO
        ENDIF
       END IF
      ELSE
C
C----  ILEV=1
       IF(INACTI==6)THEN
        DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
          PENI(CAND_N_N(I))=MIN(PENI(CAND_N_N(I)),
     .       ( (ONE-FIVEEM2)*PENI(CAND_N_N(I))
     .         +FIVEEM2*(PENE(I)+FIVEEM2*ABS(GAPV(I)-PENE(I))))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
          PENE(I)=MAX(ZERO,PENE(I)-PENI(CAND_N_N(I)))
          IF( PENE(I)==ZERO .AND. 
     .        ( IFPEN(CAND_N_N(I))/=1 .OR.TT==ZERO ) )  STIF(I) = ZERO
        ENDDO
       ELSE
        DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
          PENI(CAND_N_N(I))=MIN(PENI(CAND_N_N(I)),
     .           ((ONE-FIVEEM2)*PENI(CAND_N_N(I))+FIVEEM2*PENE(I))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
          PENE(I)=MAX(ZERO,PENE(I)-PENI(CAND_N_N(I)))
          IF( PENE(I)==ZERO .AND. 
     .        (IFPEN(CAND_N_N(I))/=1 .OR.
     .          (INACTI==5.AND.TT==ZERO) ) ) STIF(I) = ZERO
        ENDDO

       END IF
      END IF
C
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO
C
C-------------------------------------------
C     FNI + STIF
C---------------------------------
      ECONTT = ZERO
      ECONVT = ZERO
      QFRICT = ZERO
      ECONTDT = ZERO
      IF(IGAP<=1)THEN
        DO I=1,JLT
         IG=NSVG(I)
         IF(STIGLO<=ZERO)THEN
           ECONTT  = ECONTT - HALF*STIGLO*STIF(I)*PENE(I)**2 
     .                              * WEIGHT(IG)
           STIF(I) = -STIGLO*STIF(I)

         ELSEIF(STIF(I)/=ZERO)THEN
           ECONTT = ECONTT + STIGLO**PENE(I)**2 * WEIGHT(IG)
           STIF(I) = STIGLO
         ENDIF
         FNI(I)= - STIF(I) * PENE(I) 
        END DO
       ELSE
        DO I=1,JLT
         IG=NSVG(I)
         IF(STIGLO<=ZERO)THEN
           STIF(I) = -STIGLO*STIF(I)
         ELSEIF(STIF(I)/=ZERO)THEN
           STIF(I) = STIGLO
         ENDIF
         PA = AREA0(I)*PMAX
         IF(STIF(I)*MAX(GAPV(I)-GAP0(I),ZERO) <= PA)THEN
           FNI(I)= - STIF(I)*PENE(I)
           ECONTT = ECONTT - HALF * PENE(I) * FNI(I)* WEIGHT(IG)
         ELSE
           FNI(I)= - STIF(I)*MAX(PENE(I)-MAX(GAPV(I)-GAP0(I),ZERO),ZERO)
     .             - MIN(PA,STIF(I)*PENE(I))
           PLIN  = -FNI(I)/MAX(EM20,STIF(I))
           ECONTT = ECONTT + WEIGHT(IG)*
     .   ( MAX(PENE(I)-PLIN,ZERO)*AREA0(I)*PMAX - HALF *PLIN *FNI(I) )
         END IF
        END DO
       END IF
C---------------------------------
C     DAMPING
C---------------------------------
      DO I=1,JLT
        XX=XP(I)-XG(1)
        YY=YP(I)-XG(2)
        ZZ=ZP(I)-XG(3)
        VX(I)=VXI(I)-(VXM + WYM*ZZ-WZM*YY)
        VY(I)=VYI(I)-(VYM + WZM*XX-WXM*ZZ)
        VZ(I)=VZI(I)-(VZM + WXM*YY-WYM*XX)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
      ENDDO
C-------------------------------------------------
C
C     IF(KDTINT==0.AND.IDTMINS/=2)THEN
      IF(IDTMINS/=2.AND.IDTMINS_INT==0)THEN
        DO I=1,JLT
          VIS = VISC * SQRT(TWO * STIF(I) * MSI(I))
          STIF(I) = STIF(I) + VIS *DT1INV
          FNI(I)  = FNI(I) + VIS * VN(I)
          IG=NSVG(I)
          ECONTDT = ECONTDT + VIS * VN(I) * VN(I) * DT1 * WEIGHT(IG)
        ENDDO
      ELSE
        DO I=1,JLT
          C(I) = VISC * SQRT(TWO * STIF(I) * MSI(I))
          KT(I)= STIF(I)
          STIF(I) = STIF(I) + C(I) *DT1INV
          FNI(I)  = FNI(I) + C(I) * VN(I)
          IG=NSVG(I)
          ECONTDT = ECONTDT + C(I) * VN(I) * VN(I) * DT1 * WEIGHT(IG)
        ENDDO
      END IF
C---------------------------------
C     CALCUL DE LA FORCE NORMALE
C---------------------------------
      DO I=1,JLT
       FXN(I)=FNI(I)*NX(I)
       FYN(I)=FNI(I)*NY(I)
       FZN(I)=FNI(I)*NZ(I)
      END DO
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION NORMALE
C---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV8 = ZERO
      FSAV9 = ZERO
      FSAV10= ZERO
      FSAV11= ZERO
      DO I=1,JLT
       IG=NSVG(I)
       IMPX=FXN(I)*DT12*WEIGHT(IG)
       IMPY=FYN(I)*DT12*WEIGHT(IG)
       IMPZ=FZN(I)*DT12*WEIGHT(IG)
       FSAV1 =FSAV1 +IMPX
       FSAV2 =FSAV2 +IMPY
       FSAV3 =FSAV3 +IMPZ
       FSAV8 =FSAV8 +ABS(IMPX)
       FSAV9 =FSAV9 +ABS(IMPY)
       FSAV10=FSAV10+ABS(IMPZ)
       FSAV11=FSAV11+FNI(I)*DT12
      ENDDO
c
#include "lockon.inc"
       FSAV(1)=FSAV(1)+FSAV1
       FSAV(2)=FSAV(2)+FSAV2
       FSAV(3)=FSAV(3)+FSAV3
       FSAV(8)=FSAV(8)+FSAV8
       FSAV(9)=FSAV(9)+FSAV9
       FSAV(10)=FSAV(10)+FSAV10
       FSAV(11)=FSAV(11)+FSAV11
#include "lockoff.inc"
c
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          IG=NSVG(I)
          FSAVPARIT(1,1,I+NFT) =  FXN(I)*WEIGHT(IG)
          FSAVPARIT(1,2,I+NFT) =  FYN(I)*WEIGHT(IG)
          FSAVPARIT(1,3,I+NFT) =  FZN(I)*WEIGHT(IG)
        ENDDO
      ENDIF
C---------------------------------
C     NEW FRICTION MODELS
C---------------------------------
      IF (IFRIC/=0) THEN ! Friction coef = f(Temp)
        DO I=1,JLT
          TH = XFRIC *(TEMPI(I)+TEMPM(I))/2
          FRICT(I)  = FRIC*FINTER(IFRIC,TH,NPC,TF,DYDX)     
        ENDDO
      ELSE
        DO I=1,JLT
          FRICT(I)  = FRIC 
        ENDDO    

      ENDIF
      IF (MFROT==0) THEN
C---    Coulomb friction
        DO I=1,JLT
          XMU(I) = FRICT(I)
        ENDDO
      ELSEIF (MFROT==1) THEN
C---    Viscous friction
        DO I=1,JLT
C attention : normale <> normale a l'elt
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
          VV  = SQRT(MAX(EM30,V2))
C
C should be the current nodal surface instead of the initial one.
          AREA = AREA0(I)
          P = -FNI(I)/AREA
          XMU(I) = FRICT(I) + (FROT_P(1) + FROT_P(4)*P ) * P 
     .           +(FROT_P(2) + FROT_P(3)*P) * VV + FROT_P(5)*V2
          XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ELSEIF(MFROT==2)THEN
C---    Darmstad LAW
        DO I=1,JLT
C attention : normale <> normale a l'elt
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
          VV  = SQRT(MAX(EM30,V2))
C
C should be the current nodal surface instead of the initial one.
          AREA = AREA0(I)
          P = -FNI(I)/AREA
          XMU(I) = FRICT(I)
     .           + FROT_P(1)*EXP(FROT_P(2)*VV)*P*P
     .           + FROT_P(3)*EXP(FROT_P(4)*VV)*P
     .           + FROT_P(5)*EXP(FROT_P(6)*VV)
          XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ELSEIF (MFROT==3) THEN
C---    Renard LAW
        DO I=1,JLT
C attention : normale <> normale a l'elt
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
          VV = SQRT(MAX(EM30,V2))
          IF(VV>=0.AND.VV<=FROT_P(5)) THEN
            DMU = FROT_P(3)-FROT_P(1)
            VV1 = VV / FROT_P(5)
            XMU(I) = FROT_P(1)+ DMU*VV1*(TWO-VV1)
          ELSEIF(VV>FROT_P(5).AND.VV<FROT_P(6)) THEN
            DMU = FROT_P(4)-FROT_P(3) 
            VV1 = (VV - FROT_P(5))/(FROT_P(6)-FROT_P(5))
            XMU(I) = FROT_P(3)+ DMU * (THREE-TWO*VV1)*VV1**2
          ELSE
            DMU = FROT_P(2)-FROT_P(4)
            VV2 = (VV - FROT_P(6))**2
            XMU(I) = FROT_P(2) - DMU / (ONE + DMU*VV2)
          ENDIF
          XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ELSEIF(MFROT==4)THEN
C---        Exponential decay model
        DO I=1,JLT
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
           VV = SQRT(MAX(EM30,V2))
           XMU(I) = FRIC
     .            + (FROT_P(1)-FRIC)*EXP(-FROT_P(2)*VV)
           XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ENDIF
C------------------
C    TANGENT FORCE CALCULATION
C------------------
      FSAV4 = ZERO
      FSAV5 = ZERO
      FSAV6 = ZERO
      FSAV12= ZERO
      FSAV13= ZERO
      FSAV14= ZERO
      FSAV15= ZERO
      FSAV25= ZERO
C---------------------------------
C     INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
      IF (IFQ==13) THEN
        ALPHA = MAX(ONE,ALPHA0*DT12)
      ELSE
        ALPHA = ALPHA0
      ENDIF
      IF(IFTLIM == 0 ) THEN ! if flag to limit tangential force is ON : FT<= YIELD/(S*sqrt(3))
         FTMAX = PMAX
      ELSE
         FTMAX = EP30         
      ENDIF
      DO I=1,JLT
        FX = STIF0(I)*VX(I)*DT12
        FY = STIF0(I)*VY(I)*DT12
        FZ = STIF0(I)*VZ(I)*DT12

        FX = FXT(I) + ALPHA*FX
        FY = FYT(I) + ALPHA*FY
        FZ = FZT(I) + ALPHA*FZ

        FTN = FX*NX(I) + FY*NY(I) + FZ*NZ(I)

        FX = FX - FTN*NX(I)
        FY = FY - FTN*NY(I)
        FZ = FZ - FTN*NZ(I)

        FT = FX*FX + FY*FY + FZ*FZ
        FT = MAX(FT,EM30)

        FN = FNI(I)*FNI(I)

        FN = SQRT(FN)
        FT = SQRT(FT)

        BETA1 = XMU(I)*FN/FT
        BETA = MIN(ONE,BETA1)

C should be the current nodal surface instead of the initial one.
        AREA = AREA0(I)
        FS   = FTMAX/SQRT(THREE)*AREA
        BETA = MIN(BETA,FS/FT)
        IF(PSKIDFLAG >0) THEN
           BETA1 = MIN(BETA1,FS/FT)
           IF(BETA1 <= ONE) THEN
             FS2   = PMAXSKID/SQRT(THREE)*AREA
             PRATIO(I) =  MIN(ONE,BETA*FT/FS2)
           ENDIF
        ENDIF
C
        FXT(I) = FX * BETA
        FYT(I) = FY * BETA
        FZT(I) = FZ * BETA
C-------    total force
        FXI(I)=FXN(I) + FXT(I)
        FYI(I)=FYN(I) + FYT(I)
        FZI(I)=FZN(I) + FZT(I)
        IG=NSVG(I)
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
        EFRIC_L(I) = DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I)) 
     .              * WEIGHT(IG)
        ECONVT = ECONVT   + EFRIC_L(I)

        IF( INTTH > 0 .AND.BETA/=ZERO) THEN
          EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                (FZ-FZT(I))*FZT(I) ! FRICTIONAL ENERGY
          EFRICT(I) = EFRICT(I)/STIF0(I)
          QFRICT   =  QFRICT + EFRICT(I)
        ENDIF
      ENDDO    
C---------------------------------
      DO I=1,JLT
       IG=NSVG(I)
       IMPX=FXT(I)*DT12*WEIGHT(IG)
       IMPY=FYT(I)*DT12*WEIGHT(IG)
       IMPZ=FZT(I)*DT12*WEIGHT(IG)
       FSAV4 =FSAV4 +IMPX
       FSAV5 =FSAV5 +IMPY
       FSAV6 =FSAV6 +IMPZ
       IMPX=FXI(I)*DT12
       IMPY=FYI(I)*DT12
       IMPZ=FZI(I)*DT12
       FSAV12=FSAV12+ABS(IMPX)
       FSAV13=FSAV13+ABS(IMPY)
       FSAV14=FSAV14+ABS(IMPZ)
       FSAV15=FSAV15+SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
      ENDDO
c
#include "lockon.inc"
      FSAV(4) = FSAV(4) + FSAV4
      FSAV(5) = FSAV(5) + FSAV5
      FSAV(6) = FSAV(6) + FSAV6
      FSAV(12) = FSAV(12) + FSAV12
      FSAV(13) = FSAV(13) + FSAV13
      FSAV(14) = FSAV(14) + FSAV14
      FSAV(15) = FSAV(15) + FSAV15
      FSAV(25) = FSAV(25) + FHEAT*QFRICT
      FSAV(26) = FSAV(26) + ECONTT
      FSAV(27) = FSAV(27) + ECONVT- FHEAT*QFRICT
      FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
c
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          IG=NSVG(I)
          FSAVPARIT(1,4,I+NFT) =  FXT(I)*WEIGHT(IG)
          FSAVPARIT(1,5,I+NFT) =  FYT(I)*WEIGHT(IG)
          FSAVPARIT(1,6,I+NFT) =  FZT(I)*WEIGHT(IG)
        ENDDO
      ENDIF
C---------------------------------
#include "lockon.inc"
      ECONTV = ECONTV + ECONVT  ! Frictional Energy
      ECONT  = ECONT + ECONTT   ! Elatic Energy
      ECONTD = ECONTD + ECONTDT ! Damping Energy
      IF (INTTH/=0) THEN
         QFRIC = QFRIC + FHEAT*QFRICT ! FRICTIONAL HEAT ADDED TO INTERNAL ENERGY
         ECONTV = ECONTV - FHEAT*QFRICT ! FRICTIONAL HEAT REMOVED FROM CONTACT ENERGY
      ENDIF
#include "lockoff.inc"
C---------------------------------
      IF(IROT/=0)THEN
        DO I=1,JLT
          XX=XP(I)-XG(1)
          YY=YP(I)-XG(2)
          ZZ=ZP(I)-XG(3)
          MXI(I) =YY*FZI(I)-ZZ*FYI(I)
          MYI(I) =ZZ*FXI(I)-XX*FZI(I)
          MZI(I) =XX*FYI(I)-YY*FXI(I)
          STRI(I)= STIF(I)*(XX*XX+YY*YY+ZZ*ZZ)
        END DO
      END IF
C---------------------------------
C
c      IF(IDTMIN(10)==1.OR.IDTMIN(10)==2.OR.
c     .   IDTMIN(10)==5.OR.IDTMIN(10)==6)THEN
      IF(IDTMIN(10)==1)THEN
       DTMI0 = EP20
        DO I=1,JLT
         DTMI(I) = EP20
         MAS2  = TWO * MSI(I)
         JG = NSVG(I)
         IF(MAS2>ZERO.AND.STIF(I)>ZERO.AND.
     .     IRB(KINET(JG))==0.AND.IRB2(KINET(JG))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STIF(I)))
         ENDIF
         DTMI0 = MIN(DTMI0,DTMI(I))
        ENDDO

       IF(DTMI0<=DTMIN1(10))THEN
        DO I=1,JLT
         IF(DTMI(I)<=DTMIN1(10))THEN
           JG = NSVG(I)
           NI = ITAB(JG)
           IF(IDTMIN(10)==1)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE ',NOINT,'(DTMIN =',DTMIN1(10),')'
             WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                
c             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
c     .         ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
             TSTOP = TT
             IF ( ISTAMPING == 1) THEN
               WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 21.'
               WRITE(ISTDO,'(A)')'Maximum penetration may be reached'
               WRITE(ISTDO,'(A)')'You may need to check if contact normals 
     .of tools are oriented toward the blank,'
               WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 21.'
               WRITE(IOUT, '(A)')'Maximum penetration may be reached'
               WRITE(IOUT, '(A)')'You may need to check if contact normals 
     .of tools are oriented toward the blank,'
             ENDIF
           ENDIF                
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------------
      IF(IBAG/=0.OR.IADM/=0)THEN
       DO I=1,JLT
        IF(PENE(I)/=ZERO)THEN
          JG = NSVG(I)
          ICONTACT(JG)=1
        ENDIF
       ENDDO
      ENDIF
      IF(IADM/=0)THEN
       DO I=1,JLT
         JG = NSVG(I)
#include "lockon.inc"
         RCONTACT(JG)=MIN(RCONTACT(JG),RCURVI(I))
#include "lockoff.inc"
       END DO
      END IF
      IF(IADM>=2)THEN
       DO I=1,JLT
         JG = NSVG(I)
#include "lockon.inc"
         PCONTACT(JG)=MAX(PCONTACT(JG),PENE(I)/(PADM*GAPV(I)))
         ACONTACT(JG)=MIN(ACONTACT(JG),ANGLMI(I))
#include "lockoff.inc"
       END DO
      END IF
C
      IF(IBCC==0) RETURN
C
      DO 400 I=1,JLT
        IF(PENE(I)==ZERO)GOTO 400
        IBCM = IBCC / 8
        IBCS = IBCC - 8 * IBCM
        IF(IBCS>0) THEN
          IG=NSVG(I)
          CALL IBCOFF(IBCS,ICODT(IG))
        ENDIF
 400  CONTINUE
C
      RETURN
      END
