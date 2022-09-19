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
Chd|  I7COR3                        source/interfaces/int07/i7cor3.F
Chd|-- called by -----------
Chd|        I10MAINF                      source/interfaces/int10/i10mainf.F
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7COR3(
     1           JLT        ,X           ,IRECT      ,NSV       ,CAND_E   ,
     2           CAND_N     ,STF         ,STFN       ,X1        ,X2       ,
     3           X3         ,X4          ,Y1         ,Y2        ,Y3       ,
     4           Y4         ,Z1          ,Z2         ,Z3        ,Z4       ,
     5           XI         ,YI          ,ZI         ,STIF      ,IX1      ,
     6           IX2        ,IX3         ,IX4        ,NSVG      ,IGAP     ,
     7           GAP        ,GAP_S       ,GAP_M      ,GAPV      ,
     9           MS         ,VXI         ,VYI        ,
     A           VZI        ,MSI         ,NSN        ,V         ,KINET    ,
     B           KINI       ,ITY         ,NIN        ,IGSTI     ,KMIN     ,
     C           KMAX       ,GAPMAX      ,GAPMIN     ,IADM      ,RCURV    ,
     D           RCURVI     ,ANGLM       ,ANGLMI     ,INTTH     ,TEMP     ,
     E           TEMPI      ,PHI         ,AREAS      ,IELEC     ,AREASI   ,
     F           IELECI     ,NODNX_SMS   ,NSMS       ,GAP_S_L   ,GAP_M_L  ,
     G           INTFRIC    ,IPARTFRICS  ,IPARTFRICSI,IPARTFRICM,IPARTFRICMI,
     H           IORTHFRIC  ,IREP_FRICM  ,DIR_FRICM  ,IREP_FRICMI,DIR_FRICMI)
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
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),KINET(*),KINI(*),
     .        JLT,IDT, NOINT,IGAP , NSN, ITY, NIN, IGSTI,
     .        IADM,INTTH,INTFRIC,IORTHFRIC
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),IELEC(*),IELECI(MVSIZ),  NSMS(MVSIZ),
     .        NODNX_SMS(*),IPARTFRICS(*),IPARTFRICSI(MVSIZ),IPARTFRICM(*),
     .        IPARTFRICMI(MVSIZ),IREP_FRICM(*),IREP_FRICMI(MVSIZ)
C     REAL
      my_real
     .   GAP, X(3,*), STF(*), STFN(*),GAP_S(*),GAP_M(*),
     .   MS(*), V(3,*), RCURV(*),ANGLM(*),TEMP(*),AREAS(*),PHI(*),
     .   TEMPI(*),AREASI(*),GAP_S_L(*),GAP_M_L(*)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   GAPV(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   KMIN, KMAX, GAPMAX, GAPMIN,
     .   RCURVI(MVSIZ), ANGLMI(MVSIZ),
     .   DIR_FRICM(2,*)  ,DIR_FRICMI(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(IGAP==0)THEN
        DO I=1,JLT
          GAPV(I)=GAP
        ENDDO
      ELSE
        DO I=1,JLT
          IF(CAND_N(I)<=NSN) THEN
            GAPV(I)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
          ELSE
            GAPV(I)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
          ENDIF
        ENDDO!next I
        IF(IGAP==3)THEN
          DO I=1,JLT
            IF(CAND_N(I)<=NSN) THEN
              GAPV(I)=MIN(GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)),GAPV(I))
            ELSE
              GAPV(I)= MIN(GAP_LFI(NIN)%P(CAND_N(I)-NSN)+GAP_M_L(CAND_E(I)),GAPV(I))
            ENDIF
          ENDDO!next I
        ENDIF
        DO I=1,JLT
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
        ENDDO
      ENDIF

      IF(ITY==7) THEN
        IF(INTTH == 0 )THEN
          DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I) = MS(IG)
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
             KINI(I) = KINFI(NIN)%P(NN)
             XI(I) = XFI(NIN)%P(1,NN)
             YI(I) = XFI(NIN)%P(2,NN)
             ZI(I) = XFI(NIN)%P(3,NN)
             VXI(I)= VFI(NIN)%P(1,NN)
             VYI(I)= VFI(NIN)%P(2,NN)
             VZI(I)= VFI(NIN)%P(3,NN)
             MSI(I)= MSFI(NIN)%P(NN)
           END IF
C
           IX=IRECT(1,L)
           IX1(I)=IX
           X1(I)=X(1,IX)
           Y1(I)=X(2,IX)
           Z1(I)=X(3,IX)
C
           IX=IRECT(2,L)
           IX2(I)=IX
           X2(I)=X(1,IX)
           Y2(I)=X(2,IX)
           Z2(I)=X(3,IX)
C
           IX=IRECT(3,L)
           IX3(I)=IX
           X3(I)=X(1,IX)
           Y3(I)=X(2,IX)
           Z3(I)=X(3,IX)
C
           IX=IRECT(4,L)
           IX4(I)=IX
           X4(I)=X(1,IX)
           Y4(I)=X(2,IX)
           Z4(I)=X(3,IX)
C
          END DO
        ELSE
          DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
             TEMPI(I) = TEMP(IG)
             AREASI(I)= AREAS(NI)
             IELECI(I)= IELEC(NI)
             PHI(I) = ZERO
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
             KINI(I) = KINFI(NIN)%P(NN)
             XI(I) = XFI(NIN)%P(1,NN)
             YI(I) = XFI(NIN)%P(2,NN)
             ZI(I) = XFI(NIN)%P(3,NN)
             VXI(I)= VFI(NIN)%P(1,NN)
             VYI(I)= VFI(NIN)%P(2,NN)
             VZI(I)= VFI(NIN)%P(3,NN)
             MSI(I)= MSFI(NIN)%P(NN)
             TEMPI(I) = TEMPFI(NIN)%P(NN)
             AREASI(I)= AREASFI(NIN)%P(NN)
             IELECI(I)= MATSFI(NIN)%P(NN)
           END IF
C
           IX=IRECT(1,L)
           IX1(I)=IX
           X1(I)=X(1,IX)
           Y1(I)=X(2,IX)
           Z1(I)=X(3,IX)
C
           IX=IRECT(2,L)
           IX2(I)=IX
           X2(I)=X(1,IX)
           Y2(I)=X(2,IX)
           Z2(I)=X(3,IX)
C
           IX=IRECT(3,L)
           IX3(I)=IX
           X3(I)=X(1,IX)
           Y3(I)=X(2,IX)
           Z3(I)=X(3,IX)
C
           IX=IRECT(4,L)
           IX4(I)=IX
           X4(I)=X(1,IX)
           Y4(I)=X(2,IX)
           Z4(I)=X(3,IX)
C
          END DO
        ENDIF
        IF(IGSTI<=1)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=STF(L)*ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=STF(L)*ABS(STIFI(NIN)%P(NN))
          END IF
         ENDDO
        ELSEIF(IGSTI==2)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=HALF*(STF(L)+STIF(I))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
        ELSEIF(IGSTI==3)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=MAX(STF(L),STIF(I))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
        ELSEIF(IGSTI==4)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=MIN(STF(L),STIF(I))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
        ELSEIF(IGSTI==5)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=STF(L)*STIF(I)/MAX(EM30,(STF(L)+STIF(I)))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
        ENDIF
        IF(INTFRIC > 0) THEN
          DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IPARTFRICSI(I)= IPARTFRICS(NI)
           ELSE
             NN = NI - NSN
             IPARTFRICSI(I)= IPARTFRICSFI(NIN)%P(NN)
           END IF
C
           IPARTFRICMI(I) = IPARTFRICM(L)
           IF(IORTHFRIC > 0) THEN
              IREP_FRICMI(I) =IREP_FRICM(L)
              DIR_FRICMI(I,1:2)=DIR_FRICM(1:2,L)
           ENDIF
          ENDDO
        ENDIF
C
      ELSE
C type7 KINET en -
        IF(INTTH == 0 ) THEN
          DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
C             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
             STIF(I)=STF(L)*ABS(STFN(NI))
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
C             KINI(I) = KINFI(NIN)%P(NN)
             XI(I) = XFI(NIN)%P(1,NN)
             YI(I) = XFI(NIN)%P(2,NN)
             ZI(I) = XFI(NIN)%P(3,NN)
             VXI(I)= VFI(NIN)%P(1,NN)
             VYI(I)= VFI(NIN)%P(2,NN)
             VZI(I)= VFI(NIN)%P(3,NN)
             MSI(I)= MSFI(NIN)%P(NN)
             STIF(I)=STF(L)*ABS(STIFI(NIN)%P(NN))
           END IF
C
           IX=IRECT(1,L)
           IX1(I)=IX
           X1(I)=X(1,IX)
           Y1(I)=X(2,IX)
           Z1(I)=X(3,IX)
C
           IX=IRECT(2,L)
           IX2(I)=IX
           X2(I)=X(1,IX)
           Y2(I)=X(2,IX)
           Z2(I)=X(3,IX)
C
           IX=IRECT(3,L)
           IX3(I)=IX
           X3(I)=X(1,IX)
           Y3(I)=X(2,IX)
           Z3(I)=X(3,IX)
C
           IX=IRECT(4,L)
           IX4(I)=IX
           X4(I)=X(1,IX)
           Y4(I)=X(2,IX)
           Z4(I)=X(3,IX)
C
          END DO
         ELSEIF(INTTH > 0 .AND. ITY == 7) THEN
          DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
             STIF(I)=STF(L)*ABS(STFN(NI))
             TEMPI(I) = TEMP(IG)
             AREASI(I)= AREAS(NI)
             IELECI(I)= IELEC(NI)
             PHI(I) = ZERO
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
             XI(I) = XFI(NIN)%P(1,NN)
             YI(I) = XFI(NIN)%P(2,NN)
             ZI(I) = XFI(NIN)%P(3,NN)
             VXI(I)= VFI(NIN)%P(1,NN)
             VYI(I)= VFI(NIN)%P(2,NN)
             VZI(I)= VFI(NIN)%P(3,NN)
             MSI(I)= MSFI(NIN)%P(NN)
             STIF(I)=STF(L)*ABS(STIFI(NIN)%P(NN))
             TEMPI(I) = TEMPFI(NIN)%P(NN)
             AREASI(I)= AREASFI(NIN)%P(NN)
             IELECI(I)= MATSFI(NIN)%P(NN)
           END IF
C
           IX=IRECT(1,L)
           IX1(I)=IX
           X1(I)=X(1,IX)
           Y1(I)=X(2,IX)
           Z1(I)=X(3,IX)
C
           IX=IRECT(2,L)
           IX2(I)=IX
           X2(I)=X(1,IX)
           Y2(I)=X(2,IX)
           Z2(I)=X(3,IX)
C
           IX=IRECT(3,L)
           IX3(I)=IX
           X3(I)=X(1,IX)
           Y3(I)=X(2,IX)
           Z3(I)=X(3,IX)
C
           IX=IRECT(4,L)
           IX4(I)=IX
           X4(I)=X(1,IX)
           Y4(I)=X(2,IX)
           Z4(I)=X(3,IX)
          END DO
C         
         ENDIF
      END IF
C
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(NSVG(I)>0)THEN
          NSMS(I)=NODNX_SMS(NSVG(I))
     .                       +NODNX_SMS(IX1(I))+NODNX_SMS(IX2(I))
     .                       +NODNX_SMS(IX3(I))+NODNX_SMS(IX4(I))
        ELSE
          NN=-NSVG(I)
          NSMS(I)=NODNXFI(NIN)%P(NN)
     .                       +NODNX_SMS(IX1(I))+NODNX_SMS(IX2(I))
     .                       +NODNX_SMS(IX3(I))+NODNX_SMS(IX4(I))
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
      IF(IADM/=0)THEN
        DO I=1,JLT
         L  = CAND_E(I)
         RCURVI(I)=RCURV(L)
         ANGLMI(I)=ANGLM(L)
        END DO
      END IF
C
      RETURN
      END
