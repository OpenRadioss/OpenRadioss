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
Chd|  I11COR3                       source/interfaces/int11/i11cor3.F
Chd|-- called by -----------
Chd|        I11FKU3                       source/interfaces/int11/i11ke3.F
Chd|        I11KE3                        source/interfaces/int11/i11ke3.F
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        IMP_I11MAINF                  source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11COR3(
     1      JLT     ,IRECTS  ,IRECTM  ,X        ,V        ,
     2      CAND_S  ,CAND_M  ,STFS    ,STFM     ,GAP      ,
     3      GAP_S   ,GAP_M   ,IGAP    ,GAPV     ,MS       ,
     4      STIF    ,XXS1    ,XXS2    ,XYS1     ,XYS2     ,
     5      XZS1    ,XZS2    ,XXM1    ,XXM2     ,XYM1     ,
     6      XYM2    ,XZM1    ,XZM2    ,VXS1     ,VXS2     ,
     7      VYS1    ,VYS2    ,VZS1    ,VZS2     ,VXM1     ,
     8      VXM2    ,VYM1    ,VYM2    ,VZM1     ,VZM2     ,
     9      MS1     ,MS2     ,MM1     ,MM2      ,N1       ,
     A      N2      ,M1      ,M2      ,NRTS     ,NIN      ,
     B      IGSTI   ,KMIN    ,KMAX    ,NODNX_SMS,NSMS     ,
     C      GAP_S_L ,GAP_M_L ,INTTH   ,TEMP     ,TEMPI1   ,
     D      TEMPI2  ,TEMPM1  ,TEMPM2  ,AREAS    ,AREAM    ,
     E      AREAC   ,IELECI  ,IELESI  ,IELEC    ,IELES    ,
     F      IFORM   ,ITAB    ,INTFRIC ,IPARTFRICS,IPARTFRICSI,
     G      IPARTFRICM,IPARTFRICMI)
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
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s 
C-----------------------------------------------
      INTEGER IRECTS(2,*), IRECTM(2,*), CAND_M(*), CAND_S(*),
     .        JLT, IGAP , NRTS, NIN, IGSTI, NODNX_SMS(*),
     .        N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ), NSMS(MVSIZ),
     .        INTTH,IELEC(*),IELECI(MVSIZ),ITAB(*),IELES(*),IELESI(MVSIZ),IFORM,
     .        INTFRIC,
     .        IPARTFRICS(*),IPARTFRICSI(MVSIZ),IPARTFRICM(*),IPARTFRICMI(MVSIZ)
C     REAL
      my_real
     .        GAP, X(3,*), STFM(*), STFS(*),GAP_S(*),GAP_M(*),
     .        MS(*), V(3,*),
     .        XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .        XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .        XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .        VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .        VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .        VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .        MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ),
     .        GAPV(MVSIZ), STIF(MVSIZ), KMIN, KMAX, DRAD,
     .        GAP_S_L(*),GAP_M_L(*),TEMP(*),AREAS(*),AREAM(*),
     .        TEMPI1(MVSIZ),TEMPI2(MVSIZ),TEMPM1(MVSIZ),TEMPM2(MVSIZ),
     .        AREAC(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NN ,NI ,L
      my_real
     .        TM,DIST,SECS,SECM,XS,YS,ZS,XM,YM,ZM,LS,LM,CT,ST,AREA1,AREA2
C-----------------------------------------------
C
      IF(IGAP==0)THEN
        DO I=1,JLT
          GAPV(I)=GAP
        ENDDO
      ELSE
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            GAPV(I)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
            IF(IGAP == 3)  
     .      GAPV(I)=MIN(GAP_S_L(CAND_S(I))+GAP_M_L(CAND_M(I)),GAPV(I))
          ELSE
            GAPV(I)=GAPFI(NIN)%P(CAND_S(I)-NRTS)+GAP_M(CAND_M(I))
            IF(IGAP == 3) 
     .      GAPV(I)=
     .    MIN(GAP_LFI(NIN)%P(CAND_S(I)-NRTS)+GAP_M_L(CAND_M(I)),GAPV(I))
          ENDIF
          GAPV(I)=MAX(GAP,GAPV(I))
        ENDDO
      ENDIF
C
      IF(IGSTI == 1)THEN
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            STIF(I)=ABS(STFS(CAND_S(I)))*STFM(CAND_M(I))
     .             / MAX(EM20,ABS(STFS(CAND_S(I)))+STFM(CAND_M(I)))
          ELSE
            NN = CAND_S(I) - NRTS            
            STIF(I)=ABS(STIFI(NIN)%P(NN))*STFM(CAND_M(I))
     .             / MAX(EM20,ABS(STIFI(NIN)%P(NN))+STFM(CAND_M(I)))
          END IF
        END DO
      ELSEIF(IGSTI == 5)THEN
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            STIF(I)=ABS(STFS(CAND_S(I)))*STFM(CAND_M(I))
     .             / MAX(EM20,ABS(STFS(CAND_S(I)))+STFM(CAND_M(I)))
          ELSE
            NN = CAND_S(I) - NRTS            
            STIF(I)=ABS(STIFI(NIN)%P(NN))*STFM(CAND_M(I))
     .             / MAX(EM20,ABS(STIFI(NIN)%P(NN))+STFM(CAND_M(I)))
          END IF
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
        END DO
      ELSEIF(IGSTI == 2)THEN
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            STIF(I)=HALF*(ABS(STFS(CAND_S(I)))+STFM(CAND_M(I)))
          ELSE
            NN = CAND_S(I) - NRTS            
            STIF(I)=HALF*(ABS(STIFI(NIN)%P(NN))+STFM(CAND_M(I)))
          END IF
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
        END DO
      ELSEIF(IGSTI == 3)THEN
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            STIF(I)=MAX(ABS(STFS(CAND_S(I))),STFM(CAND_M(I)))
          ELSE
            NN = CAND_S(I) - NRTS            
            STIF(I)=MAX(ABS(STIFI(NIN)%P(NN)),STFM(CAND_M(I)))
          END IF
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
        END DO
      ELSEIF(IGSTI == 4)THEN
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            STIF(I)=MIN(ABS(STFS(CAND_S(I))),STFM(CAND_M(I)))
          ELSE
            NN = CAND_S(I) - NRTS            
            STIF(I)=MIN(ABS(STIFI(NIN)%P(NN)),STFM(CAND_M(I)))
          END IF
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
        END DO
      END IF
C
      DO I=1,JLT
        IF(CAND_S(I)<=NRTS) THEN
          N1(I)=IRECTS(1,CAND_S(I))
          N2(I)=IRECTS(2,CAND_S(I))
          M1(I)=IRECTM(1,CAND_M(I))
          M2(I)=IRECTM(2,CAND_M(I))
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
          VXS1(I) = V(1,N1(I))
          VYS1(I) = V(2,N1(I))
          VZS1(I) = V(3,N1(I))
          VXS2(I) = V(1,N2(I))
          VYS2(I) = V(2,N2(I))
          VZS2(I) = V(3,N2(I))
          VXM1(I) = V(1,M1(I))
          VYM1(I) = V(2,M1(I))
          VZM1(I) = V(3,M1(I))
          VXM2(I) = V(1,M2(I))
          VYM2(I) = V(2,M2(I))
          VZM2(I) = V(3,M2(I))
          MS1(I) = MS(N1(I))
          MS2(I) = MS(N2(I))
          MM1(I) = MS(M1(I))
          MM2(I) = MS(M2(I))
        ELSE
          NN = CAND_S(I) - NRTS            
          N1(I)=2*(NN-1)+1
          N2(I)=2*NN
          M1(I)=IRECTM(1,CAND_M(I))
          M2(I)=IRECTM(2,CAND_M(I))
          XXS1(I) = XFI(NIN)%P(1,N1(I))
          XYS1(I) = XFI(NIN)%P(2,N1(I))
          XZS1(I) = XFI(NIN)%P(3,N1(I))
          XXS2(I) = XFI(NIN)%P(1,N2(I))
          XYS2(I) = XFI(NIN)%P(2,N2(I))
          XZS2(I) = XFI(NIN)%P(3,N2(I))
          XXM1(I) = X(1,M1(I))
          XYM1(I) = X(2,M1(I))
          XZM1(I) = X(3,M1(I))
          XXM2(I) = X(1,M2(I))
          XYM2(I) = X(2,M2(I))
          XZM2(I) = X(3,M2(I))
          VXS1(I) = VFI(NIN)%P(1,N1(I))
          VYS1(I) = VFI(NIN)%P(2,N1(I))
          VZS1(I) = VFI(NIN)%P(3,N1(I))
          VXS2(I) = VFI(NIN)%P(1,N2(I))
          VYS2(I) = VFI(NIN)%P(2,N2(I))
          VZS2(I) = VFI(NIN)%P(3,N2(I))
          VXM1(I) = V(1,M1(I))
          VYM1(I) = V(2,M1(I))
          VZM1(I) = V(3,M1(I))
          VXM2(I) = V(1,M2(I))
          VYM2(I) = V(2,M2(I))
          VZM2(I) = V(3,M2(I))
          MS1(I) = MSFI(NIN)%P(N1(I))
          MS2(I) = MSFI(NIN)%P(N2(I))
          MM1(I) = MS(M1(I))
          MM2(I) = MS(M2(I))
        END IF
      END DO
C
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(CAND_S(I)<=NRTS)THEN
          NSMS(I)=NODNX_SMS(N1(I))+NODNX_SMS(N2(I))+
     .            NODNX_SMS(M1(I))+NODNX_SMS(M2(I))
        ELSE
          NSMS(I)=NODNXFI(NIN)%P(N1(I))+NODNXFI(NIN)%P(N2(I))+
     .            NODNX_SMS(M1(I))+NODNX_SMS(M2(I))
        END IF
       ENDDO
       IF(IDTMINS_INT/=0)THEN
         DO I=1,JLT
          IF(NSMS(I)==0)NSMS(I)=-1
         ENDDO
       END IF
      ELSEIF(IDTMINS_INT/=0)THEN
        DO I=1,JLT
         NSMS(I)=-1
        ENDDO
      ENDIF
C
C    Thermal Modelling
C
      IF(INTTH/=0)THEN
C
        IF(IFORM == 0) THEN
C
          DO I=1,JLT
           IF(CAND_S(I)<=NRTS) THEN
C     SECND EDGE AREA
             SECS=AREAS(CAND_S(I))
C     AREA COMPUTING
             XS = XXS2(I)-XXS1(I)
             YS = XYS2(I)-XYS1(I)
             ZS = XZS2(I)-XZS1(I)
C
             LS = SQRT(XS*XS + YS*YS + ZS*ZS)
C
             AREAC(I) = LS*SECS
C     SECND TEMPERATURE
             TEMPI1(I) = TEMP(N1(I))
             TEMPI2(I) = TEMP(N2(I))
             IELECI(I)= IELEC(CAND_S(I))
           ELSE
             NN = CAND_S(I) - NRTS            
C     SECND EDGE AREA
             SECS =AREASFI(NIN)%P(NN)
C     AREA COMPUTING
             XS = XXS2(I)-XXS1(I)
             YS = XYS2(I)-XYS1(I)
             ZS = XZS2(I)-XZS1(I)
C
             LS = SQRT(XS*XS + YS*YS + ZS*ZS)
C
             AREAC(I) = LS*SECS
C     SECND TEMPERATURE
             TEMPI1(I) = TEMPFI(NIN)%P(N1(I))
             TEMPI2(I) = TEMPFI(NIN)%P(N2(I))

             IELECI(I)= MATSFI(NIN)%P(NN)
C
           END IF
          END DO
C
        ELSE
C
          DO I=1,JLT
           IF(CAND_S(I)<=NRTS) THEN
C     SECND EDGE AREA
             SECS=AREAS(CAND_S(I))
C     main EDGE AREA
             SECM=AREAM(CAND_M(I))
C     AREA COMPUTING
             XS = XXS2(I)-XXS1(I)
             YS = XYS2(I)-XYS1(I)
             ZS = XZS2(I)-XZS1(I)
             XM = XXM2(I)-XXM1(I)
             YM = XYM2(I)-XYM1(I)
             ZM = XZM2(I)-XZM1(I)

             LS = SQRT(XS*XS + YS*YS + ZS*ZS)
             LM = SQRT(XM*XM + YM*YM + ZM*ZM)

             CT = (XS*XM + YS*YM + ZS*ZM)/(LS*LM)
             ST = SQRT(ONE-MIN(CT*CT,ONE))

             AREA1 = MIN(LS,LM)*MIN(SECS,SECM)
             AREA2 = SECS*SECM/MAX(ST,EM30)

             AREAC(I) = MIN(AREA1,AREA2)

C     SECND TEMPERATURE
             TEMPI1(I) = TEMP(N1(I))
             TEMPI2(I) = TEMP(N2(I))
C     main TEMPERATURE
             TEMPM1(I) = TEMP(M1(I))
             TEMPM2(I) = TEMP(M2(I))

             IELECI(I)= IELEC(CAND_S(I))
             IELESI(I)= IELES(CAND_M(I))
           ELSE
             NN = CAND_S(I) - NRTS            
C     SECND NODAL AREA
             SECS =AREASFI(NIN)%P(NN)
C     main EDGE AREA
             SECM =AREAM(CAND_M(I))
C     AREA COMPUTING
             XS = XXS2(I)-XXS1(I)
             YS = XYS2(I)-XYS1(I)
             ZS = XZS2(I)-XZS1(I)
             XM = XXM2(I)-XXM1(I)
             YM = XYM2(I)-XYM1(I)
             ZM = XZM2(I)-XZM1(I)

             LS = SQRT(XS*XS + YS*YS + ZS*ZS)
             LM = SQRT(XM*XM + YM*YM + ZM*ZM)

             CT = (XS*XM + YS*YM + ZS*ZM)/(LS*LM)
             ST = SQRT(ONE-MIN(CT*CT,ONE))

             AREA1 = MIN(LS,LM)*MIN(SECS,SECM)
             AREA2 = SECS*SECM/MAX(ST,EM30)

             AREAC(I) = MIN(AREA1,AREA2)*HALF

C     SECND TEMPERATURE
             TEMPI1(I) = TEMPFI(NIN)%P(N1(I))
             TEMPI2(I) = TEMPFI(NIN)%P(N2(I))
C     main TEMPERATURE
             TEMPM1(I) = TEMP(M1(I))
             TEMPM2(I) = TEMP(M2(I))

             IELECI(I)= MATSFI(NIN)%P(NN)
             IELESI(I)= IELES(CAND_M(I))
         END IF
C
        END DO
C
       ENDIF
C
      ENDIF
C
      IF(INTFRIC > 0) THEN
         DO I=1,JLT
           NI = CAND_S(I)
           L  = CAND_M(I)
           IF(NI<=NRTS)THEN
             IPARTFRICSI(I)= IPARTFRICS(NI)
           ELSE
             NN = NI - NRTS
             IPARTFRICSI(I)= IPARTFRICSFI(NIN)%P(NN)
           END IF
C
           IPARTFRICMI(I) = IPARTFRICM(L)
         ENDDO
       ENDIF
C
      RETURN
      END
