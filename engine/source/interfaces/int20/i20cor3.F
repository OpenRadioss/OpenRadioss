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
Chd|  I20COR3                       source/interfaces/int20/i20cor3.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20COR3(
     1                JLT     ,XA    ,IRECT ,NSV   ,CAND_E ,
     2                CAND_N  ,STF   ,STFA  ,X1    ,X2     ,
     3                X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     4                Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     5                XI      ,YI    ,ZI    ,STIF  ,IX1    ,
     6                IX2     ,IX3   ,IX4   ,NSVG  ,IGAP   ,
     7                GAP     ,GAP_S ,GAP_M ,GAPV  ,GAPR   ,
     8                MS      ,VXI   ,VYI   ,NLN   ,NLG    ,
     9                VZI     ,MSI   ,NSN   ,VA    ,KINET  ,
     A                KINI    ,ITY   ,NIN   ,IGSTI ,KMIN   ,
     B                KMAX    ,GAPMAX,GAPMIN,IADM  ,RCURV  ,
     C                RCURVI  ,ANGLM ,ANGLMI,INTTH ,TEMP   ,
     D                TEMPI   ,PHI   ,AREAS ,IELEC ,AREASI ,
     E                IELECI  ,GAP_SH,STFAC ,NODNX_SMS,NSMS)
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
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),KINET(*),KINI(*),
     .        JLT,IDT, NOINT,IGAP ,NSN, ITY, NIN, IGSTI,
     .        IADM,INTTH, NLN, NLG(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),IELEC(*),IELECI(MVSIZ),  NSMS(MVSIZ), 
     .        NODNX_SMS(*)
C     REAL
      my_real
     .   GAP, XA(3,*), STF(*), STFA(*),GAP_S(*),GAP_M(*),
     .   MS(*), VA(3,*), RCURV(*),TEMP(*),AREAS(*),PHI(*),TEMPI(*),
     .   AREASI(*), ANGLM(*),GAP_SH(*),STFAC
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   GAPV(MVSIZ),GAPR(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   KMIN, KMAX, GAPMAX, GAPMIN,
     .   RCURVI(MVSIZ), ANGLMI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI
C-----------------------------------------------
C
      IF(IGAP==0)THEN
        DO I=1,JLT
          GAPV(I)=GAP
          GAPR(I)=GAPV(I)
        ENDDO
      ELSE
        DO I=1,JLT
          IF(CAND_N(I)<=NSN) THEN
            GAPV(I)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
          ELSE
            GAPV(I)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
          ENDIF
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=GAPV(I)+GAP_SH(CAND_E(I))*(ONE-EM5)
          GAPR(I)=GAPV(I)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
        ENDDO
      ENDIF
C
      IF(INTTH == 0 )THEN
        DO I=1,JLT
         NI = CAND_N(I)
         L  = CAND_E(I)
         IF(NI<=NSN)THEN
           IL = NSV(NI)
           IG = NLG(IL)
           NSVG(I) = IG
           KINI(I) = KINET(IG)

           XI(I)  = XA(1,IL) 
           YI(I)  = XA(2,IL) 
           ZI(I)  = XA(3,IL) 
           VXI(I) = VA(1,IL)
           VYI(I) = VA(2,IL)
           VZI(I) = VA(3,IL)

           MSI(I)= MS(IG)
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
         X1(I)=XA(1,IX)
         Y1(I)=XA(2,IX)
         Z1(I)=XA(3,IX)
C
         IX=IRECT(2,L)
         IX2(I)=IX    
         X2(I)=XA(1,IX)
         Y2(I)=XA(2,IX)
         Z2(I)=XA(3,IX)
C
         IX=IRECT(3,L)
         IX3(I)=IX    
         X3(I)=XA(1,IX)
         Y3(I)=XA(2,IX)
         Z3(I)=XA(3,IX)
C
         IX=IRECT(4,L)
         IX4(I)=IX    
         X4(I)=XA(1,IX)
         Y4(I)=XA(2,IX)
         Z4(I)=XA(3,IX)

        END DO
      ELSE
        DO I=1,JLT
         NI = CAND_N(I)
         L  = CAND_E(I)
         IF(NI<=NSN)THEN
           IL = NSV(NI)
           IG = NLG(IL)
           NSVG(I) = IG
           KINI(I) = KINET(IG)

           XI(I) = XA(1,IL) 
           YI(I) = XA(2,IL) 
           ZI(I) = XA(3,IL) 
           VXI(I) = VA(1,IL)
           VYI(I) = VA(2,IL)
           VZI(I) = VA(3,IL)

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

         IX=IRECT(1,L) 
         IX1(I)=IX     
         X1(I)=XA(1,IX) 
         Y1(I)=XA(2,IX) 
         Z1(I)=XA(3,IX) 
C
         IX=IRECT(2,L) 
         IX2(I)=IX     
         X2(I)=XA(1,IX) 
         Y2(I)=XA(2,IX) 
         Z2(I)=XA(3,IX) 
C
         IX=IRECT(3,L) 
         IX3(I)=IX     
         X3(I)=XA(1,IX) 
         Y3(I)=XA(2,IX) 
         Z3(I)=XA(3,IX) 
C
         IX=IRECT(4,L) 
         IX4(I)=IX     
         X4(I)=XA(1,IX) 
         Y4(I)=XA(2,IX) 
         Z4(I)=XA(3,IX) 

        END DO
      ENDIF 
C
      IF(IGSTI<=1)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=STF(L)*ABS(STFA(NSV(NI)))
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
            STIF(I)=ABS(STFA(NSV(NI)))
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
            STIF(I)=ABS(STFA(NSV(NI)))
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
            STIF(I)=ABS(STFA(NSV(NI)))
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
            STIF(I)=ABS(STFA(NSV(NI)))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=STF(L)*STIF(I)/
     .            MAX(EM30,(STF(L)+STIF(I)))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
      ENDIF

      DO I=1,JLT
        STIF(I)=MAX(STFAC,ONE)*STIF(I)
      ENDDO

      IF(IADM/=0)THEN
        DO I=1,JLT
         L  = CAND_E(I)
         RCURVI(I)=RCURV(L)
         ANGLMI(I)=ANGLM(L)
        END DO
      END IF

C
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(NSVG(I)>0)THEN
          NSMS(I)=NODNX_SMS(NSVG(I))
     .                       +NODNX_SMS(NLG(IX1(I)))
     .                       +NODNX_SMS(NLG(IX2(I)))
     .                       +NODNX_SMS(NLG(IX3(I)))
     .                       +NODNX_SMS(NLG(IX4(I)))
        ELSE
          NN=-NSVG(I)
          NSMS(I)=NODNXFI(NIN)%P(NN)
     .                       +NODNX_SMS(NLG(IX1(I)))
     .                       +NODNX_SMS(NLG(IX2(I)))
     .                       +NODNX_SMS(NLG(IX3(I)))
     .                       +NODNX_SMS(NLG(IX4(I)))
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

      RETURN
      END
Chd|====================================================================
Chd|  I20COR3E                      source/interfaces/int20/i20cor3.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20COR3E(
     1      JLT    ,IXLINS ,IXLINM ,XA     ,VA     ,
     2      CAND_S ,CAND_M ,STFS   ,STFM   ,GAPMIN ,
     3      GAP_S  ,GAP_M  ,IGAP   ,GAPV   ,MS     ,
     4      STIF   ,XXS1   ,XXS2   ,XYS1   ,XYS2   ,
     5      XZS1   ,XZS2   ,XXM1   ,XXM2   ,XYM1   ,
     6      XYM2   ,XZM1   ,XZM2   ,VXS1   ,VXS2   ,
     7      VYS1   ,VYS2   ,VZS1   ,VZS2   ,VXM1   ,
     8      VXM2   ,VYM1   ,VYM2   ,VZM1   ,VZM2   ,
     9      MS1    ,MS2    ,MM1    ,MM2    ,N1     ,
     A      N2     ,M1     ,M2     ,NRTS   ,NIN    ,
     B      NL1    ,NL2    ,ML1    ,ML2    ,NLG    ,
     C      STFAC  ,NODNX_SMS,NSMS )
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
#include      "scr05_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXLINS(2,*), IXLINM(2,*), CAND_M(*), CAND_S(*),
     .        JLT, IGAP , NRTS, NIN,
     .        N1(MVSIZ), N2(MVSIZ), NL1(MVSIZ), NL2(MVSIZ),
     .        M1(MVSIZ), M2(MVSIZ), ML1(MVSIZ), ML2(MVSIZ),NLG(*),
     .        NODNX_SMS(*), NSMS(MVSIZ)
C     REAL
      my_real
     .        GAPMIN, XA(3,*), STFM(*), STFS(*),GAP_S(*),GAP_M(*),
     .        MS(*), VA(3,*),
     .        XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .        XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .        XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .        VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .        VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .        VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .        MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ),
     .        GAPV(MVSIZ), STIF(MVSIZ),STFAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NN
C-----------------------------------------------
      IF(IMACH/=3) THEN
        IF(IGAP==0)THEN
          DO I=1,JLT
            GAPV(I)=GAPMIN
          ENDDO
        ELSE
          DO I=1,JLT
            GAPV(I)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
            GAPV(I)=MAX(GAPMIN,GAPV(I))
          ENDDO
        ENDIF
C
        DO I=1,JLT
          NL1(I)=IXLINS(1,CAND_S(I))
          NL2(I)=IXLINS(2,CAND_S(I))
          ML1(I)=IXLINM(1,CAND_M(I))
          ML2(I)=IXLINM(2,CAND_M(I))
          N1(I)=NLG(NL1(I))
          N2(I)=NLG(NL2(I))
          M1(I)=NLG(ML1(I))
          M2(I)=NLG(ML2(I))
          STIF(I)=ABS(STFS(CAND_S(I)))*STFM(CAND_M(I))
     .          / MAX(EM20,ABS(STFS(CAND_S(I)))+STFM(CAND_M(I)))
          XXS1(I) = XA(1,NL1(I))
          XYS1(I) = XA(2,NL1(I))
          XZS1(I) = XA(3,NL1(I))
          XXS2(I) = XA(1,NL2(I))
          XYS2(I) = XA(2,NL2(I))
          XZS2(I) = XA(3,NL2(I))
          XXM1(I) = XA(1,ML1(I))
          XYM1(I) = XA(2,ML1(I))
          XZM1(I) = XA(3,ML1(I))
          XXM2(I) = XA(1,ML2(I))
          XYM2(I) = XA(2,ML2(I))
          XZM2(I) = XA(3,ML2(I))
          VXS1(I) = VA(1,NL1(I))
          VYS1(I) = VA(2,NL1(I))
          VZS1(I) = VA(3,NL1(I))
          VXS2(I) = VA(1,NL2(I))
          VYS2(I) = VA(2,NL2(I))
          VZS2(I) = VA(3,NL2(I))
          VXM1(I) = VA(1,ML1(I))
          VYM1(I) = VA(2,ML1(I))
          VZM1(I) = VA(3,ML1(I))
          VXM2(I) = VA(1,ML2(I))
          VYM2(I) = VA(2,ML2(I))
          VZM2(I) = VA(3,ML2(I))
          MS1(I) = MS(N1(I))
          MS2(I) = MS(N2(I))
          MM1(I) = MS(M1(I))
          MM2(I) = MS(M2(I))
        ENDDO
C
      ELSE
        IF(IGAP==0)THEN
          DO I=1,JLT
            GAPV(I)=GAPMIN
          ENDDO
        ELSE
          DO I=1,JLT
            IF(CAND_S(I)<=NRTS) THEN
              GAPV(I)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
            ELSE
              GAPV(I)=GAPFIE(NIN)%P(CAND_S(I)-NRTS)+GAP_M(CAND_M(I))
            ENDIF
            GAPV(I)=MAX(GAPMIN,GAPV(I))
          ENDDO
        ENDIF
C
        DO I=1,JLT
          IF(CAND_S(I)<=NRTS) THEN
            NL1(I)=IXLINS(1,CAND_S(I))
            NL2(I)=IXLINS(2,CAND_S(I))
            ML1(I)=IXLINM(1,CAND_M(I))
            ML2(I)=IXLINM(2,CAND_M(I))
            N1(I)=NLG(NL1(I))
            N2(I)=NLG(NL2(I))
            M1(I)=NLG(ML1(I))
            M2(I)=NLG(ML2(I))
            STIF(I)=ABS(STFS(CAND_S(I)))*STFM(CAND_M(I))
     .             / MAX(EM20,ABS(STFS(CAND_S(I)))+STFM(CAND_M(I)))
            XXS1(I) = XA(1,NL1(I))
            XYS1(I) = XA(2,NL1(I))
            XZS1(I) = XA(3,NL1(I))
            XXS2(I) = XA(1,NL2(I))
            XYS2(I) = XA(2,NL2(I))
            XZS2(I) = XA(3,NL2(I))
            XXM1(I) = XA(1,ML1(I))
            XYM1(I) = XA(2,ML1(I))
            XZM1(I) = XA(3,ML1(I))
            XXM2(I) = XA(1,ML2(I))
            XYM2(I) = XA(2,ML2(I))
            XZM2(I) = XA(3,ML2(I))
            VXS1(I) = VA(1,NL1(I))
            VYS1(I) = VA(2,NL1(I))
            VZS1(I) = VA(3,NL1(I))
            VXS2(I) = VA(1,NL2(I))
            VYS2(I) = VA(2,NL2(I))
            VZS2(I) = VA(3,NL2(I))
            VXM1(I) = VA(1,ML1(I))
            VYM1(I) = VA(2,ML1(I))
            VZM1(I) = VA(3,ML1(I))
            VXM2(I) = VA(1,ML2(I))
            VYM2(I) = VA(2,ML2(I))
            VZM2(I) = VA(3,ML2(I))
            MS1(I) = MS(N1(I))
            MS2(I) = MS(N2(I))
            MM1(I) = MS(M1(I))
            MM2(I) = MS(M2(I))
          ELSE
            NN = CAND_S(I) - NRTS            
            N1(I)=2*(NN-1)+1
            N2(I)=2*NN
            ML1(I)=IXLINM(1,CAND_M(I))
            ML2(I)=IXLINM(2,CAND_M(I))
            M1(I) =NLG(ML1(I))
            M2(I) =NLG(ML2(I))
            STIF(I)=ABS(STIFIE(NIN)%P(NN))*STFM(CAND_M(I))
     .            / MAX(EM20,ABS(STIFIE(NIN)%P(NN))+STFM(CAND_M(I)))
            XXS1(I) = XFIE(NIN)%P(1,N1(I))
            XYS1(I) = XFIE(NIN)%P(2,N1(I))
            XZS1(I) = XFIE(NIN)%P(3,N1(I))
            XXS2(I) = XFIE(NIN)%P(1,N2(I))
            XYS2(I) = XFIE(NIN)%P(2,N2(I))
            XZS2(I) = XFIE(NIN)%P(3,N2(I))
            XXM1(I) = XA(1,ML1(I))
            XYM1(I) = XA(2,ML1(I))
            XZM1(I) = XA(3,ML1(I))
            XXM2(I) = XA(1,ML2(I))
            XYM2(I) = XA(2,ML2(I))
            XZM2(I) = XA(3,ML2(I))
            VXS1(I) = VFIE(NIN)%P(1,N1(I))
            VYS1(I) = VFIE(NIN)%P(2,N1(I))
            VZS1(I) = VFIE(NIN)%P(3,N1(I))
            VXS2(I) = VFIE(NIN)%P(1,N2(I))
            VYS2(I) = VFIE(NIN)%P(2,N2(I))
            VZS2(I) = VFIE(NIN)%P(3,N2(I))
            VXM1(I) = VA(1,ML1(I))
            VYM1(I) = VA(2,ML1(I))
            VZM1(I) = VA(3,ML1(I))
            VXM2(I) = VA(1,ML2(I))
            VYM2(I) = VA(2,ML2(I))
            VZM2(I) = VA(3,ML2(I))
            MS1(I) = MSFIE(NIN)%P(N1(I))
            MS2(I) = MSFIE(NIN)%P(N2(I))
            MM1(I) = MS(M1(I))
            MM2(I) = MS(M2(I))
          END IF
        END DO
      END IF

      DO I=1,JLT
        STIF(I)=MAX(STFAC,ONE)*STIF(I)
      ENDDO
C
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(CAND_S(I)<=NRTS)THEN
          NSMS(I)=NODNX_SMS(N1(I))+NODNX_SMS(N2(I))+
     .            NODNX_SMS(M1(I))+NODNX_SMS(M2(I))
        ELSE
          NSMS(I)=NODNXFIE(NIN)%P(N1(I))+NODNXFIE(NIN)%P(N2(I))+
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
      RETURN
      END
Chd|====================================================================
Chd|  I20DST3E                      source/interfaces/int20/i20cor3.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20DST3E(
     1   JLT    ,CAND_S,CAND_M,H1S   ,H2S   ,
     2   H1M    ,H2M   ,NX    ,NY    ,NZ    ,
     3   STIF   ,N1    ,N2    ,M1    ,M2    ,
     4   JLT_NEW,XXS1  ,XXS2  ,XYS1  ,XYS2  ,
     5   XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     6   XYM2   ,XZM1  ,XZM2  ,VXS1  ,VXS2  ,
     7   VYS1   ,VYS2  ,VZS1  ,VZS2  ,VXM1  ,
     8   VXM2   ,VYM1  ,VYM2  ,VZM1  ,VZM2  ,
     9   MS1    ,MS2   ,MM1   ,MM2   ,GAPV  ,
     A   NL1    ,NL2   ,ML1   ,ML2   ,IGAP  ,
     B   SOLIDN_NORMAL,GAP_S,GAP_M   ,NLINSA,
     C   SOLIDN_NORMAL_FE,NSMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,JLT_NEW,IGAP,NLINSA
      INTEGER CAND_S(MVSIZ),CAND_M(MVSIZ),
     .        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ),
     .        NL1(MVSIZ), NL2(MVSIZ),ML1(MVSIZ), ML2(MVSIZ),
     .        NSMS(MVSIZ)
      INTEGER SOLIDN_NORMAL(3,*), SOLIDN_NORMAL_FE(3,*)
      my_real
     .     H1S(*),H2S(*),H1M(*),H2M(*),NX(*),NY(*),NZ(*),STIF(*),
     .     XXS1(*) ,XXS2(*) ,XYS1(*) ,XYS2(*) ,
     .     XZS1(*) ,XZS2(*) ,XXM1(*) ,XXM2(*) ,XYM1(*),
     .     XYM2(*) ,XZM1(*) ,XZM2(*) ,VXS1(*) ,VXS2(*),
     .     VYS1(*) ,VYS2(*) ,VZS1(*) ,VZS2(*) ,VXM1(*),
     .     VXM2(*) ,VYM1(*) ,VYM2(*) ,VZM1(*) ,VZM2(*),
     .     MS1(*) ,MS2(*) ,MM1(*) ,MM2(*), GAPV(*),
     .     GAP_S(*),GAP_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     PENE2(MVSIZ),
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,AAA,GAP2,
     .     SX1,SX2,SX3,SX4,SY1,SY2,SY3,SY4,SZ1,SZ2,SZ3,SZ4
C-----------------------------------------------
       JLT_NEW = 0
C--------------------------------------------------------
C  
C--------------------------------------------------------
C       F = [A*X1+(1-A)*X2-B*X3-(1-B)*X4]^2 + [..Y..]^2 + [..Z..]^2
C       DF/DA = 0 = (X1-X2)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DA = 0 = A(X1-X2)^2 +X2-X4 + B(X1-X2)(X4-X3))+...
C       DF/DA = 0 = A[(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2] 
C                 + B[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4) 
C       DF/DB = 0 = (X4-X3)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DB = 0 = B[(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2] 
C                 + A[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4) 
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XM2 = [(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       XB = (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4)
C       A XS2 + B XSM +   XA = 0
C       A XSM + B XM2 +   XB = 0
C
C       A = -(XA + B XSM)/XS2
C       -(XA + B XSM)*XSM + B XM2*XS2 +   XB*XS2 = 0
C       -B XSM*XSM + B XM2*XS2 +   XB*XS2-XA*XSM  = 0
C       B*(XM2*XS2 - XSM*XSM) = -XB*XS2+XA*XSM  
C       B = (XA*XSM-XB*XS2) / (XM2*XS2 - XSM*XSM)
C       A = (XB*XSM-XA*XM2) / (XM2*XS2 - XSM*XSM)
C
C IF B<0 => B=0
C
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = - XA /XS2
C       B = 0
C
C ELSEIF B>1 => B=1
C
C       B = 1
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = -(XA + XSM)/XS2
C
C IF A<0 => A=0
C
C
C ELSEIF A>1 => A=1
C
C
      DO I=1,JLT
       IF(IGAP/=0)THEN
         AAA = GAP_S(CAND_S(I))
         IF(CAND_S(I)<=NLINSA) THEN
           SX1 = SOLIDN_NORMAL(1,N1(I))*AAA
           SY1 = SOLIDN_NORMAL(2,N1(I))*AAA
           SZ1 = SOLIDN_NORMAL(3,N1(I))*AAA
           SX2 = SOLIDN_NORMAL(1,N2(I))*AAA
           SY2 = SOLIDN_NORMAL(2,N2(I))*AAA
           SZ2 = SOLIDN_NORMAL(3,N2(I))*AAA
         ELSE
C noeuds remote en SPMD
           SX1 = SOLIDN_NORMAL_FE(1,N1(I))*AAA
           SY1 = SOLIDN_NORMAL_FE(2,N1(I))*AAA
           SZ1 = SOLIDN_NORMAL_FE(3,N1(I))*AAA
           SX2 = SOLIDN_NORMAL_FE(1,N2(I))*AAA
           SY2 = SOLIDN_NORMAL_FE(2,N2(I))*AAA
           SZ2 = SOLIDN_NORMAL_FE(3,N2(I))*AAA
         END IF
         AAA = GAP_M(CAND_M(I))
         SX3 = SOLIDN_NORMAL(1,M1(I))*AAA
         SY3 = SOLIDN_NORMAL(2,M1(I))*AAA
         SZ3 = SOLIDN_NORMAL(3,M1(I))*AAA
         SX4 = SOLIDN_NORMAL(1,M2(I))*AAA
         SY4 = SOLIDN_NORMAL(2,M2(I))*AAA
         SZ4 = SOLIDN_NORMAL(3,M2(I))*AAA
         XXS1(I) = XXS1(I) - SX1
         XYS1(I) = XYS1(I) - SY1
         XZS1(I) = XZS1(I) - SZ1
         XXS2(I) = XXS2(I) - SX2 
         XYS2(I) = XYS2(I) - SY2
         XZS2(I) = XZS2(I) - SZ2
         XXM1(I) = XXM1(I) - SX3
         XYM1(I) = XYM1(I) - SY3
         XZM1(I) = XZM1(I) - SZ3
         XXM2(I) = XXM2(I) - SX4
         XYM2(I) = XYM2(I) - SY4
         XZM2(I) = XZM2(I) - SZ4
       ENDIF
       XS12 = XXS2(I)-XXS1(I)
       YS12 = XYS2(I)-XYS1(I)
       ZS12 = XZS2(I)-XZS1(I)
       XS2  = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = XXM2(I)-XXM1(I)
       YM12 = XYM2(I)-XYM1(I)
       ZM12 = XZM2(I)-XZM1(I)
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = XXM2(I)-XXS2(I)
       YS2M2 = XYM2(I)-XYS2(I)
       ZS2M2 = XZM2(I)-XZS2(I)

       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       H1M(I) = (XA*XSM-XB*XS2) / DET
C
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       H1M(I)=MIN(ONE,MAX(ZERO,H1M(I)))
       H1S(I) = -(XA + H1M(I)*XSM) / XS2
       H1S(I)=MIN(ONE,MAX(ZERO,H1S(I)))
       H1M(I) = -(XB + H1S(I)*XSM) / XM2
       H1M(I)=MIN(ONE,MAX(ZERO,H1M(I)))

       H2S(I) = ONE -H1S(I)
       H2M(I) = ONE -H1M(I)
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       NX(I) = H1S(I)*XXS1(I) + H2S(I)*XXS2(I)
     .       - H1M(I)*XXM1(I) - H2M(I)*XXM2(I)
       NY(I) = H1S(I)*XYS1(I) + H2S(I)*XYS2(I)
     .       - H1M(I)*XYM1(I) - H2M(I)*XYM2(I)
       NZ(I) = H1S(I)*XZS1(I) + H2S(I)*XZS2(I)
     .       - H1M(I)*XZM1(I) - H2M(I)*XZM2(I)
       GAP2 = GAPV(I)*GAPV(I)
       PENE2(I) = GAP2 - NX(I)*NX(I) - NY(I)*NY(I) - NZ(I)*NZ(I)
       PENE2(I) = MAX(ZERO,PENE2(I)) 

      ENDDO
      IF(IDTMINS/=2)THEN
#include      "vectorize.inc"
       DO I=1,JLT
        IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
          JLT_NEW = JLT_NEW + 1
          CAND_S(JLT_NEW) = CAND_S(I)
          CAND_M(JLT_NEW) = CAND_M(I)
          NL1(JLT_NEW)    = NL1(I)
          NL2(JLT_NEW)    = NL2(I)
          ML1(JLT_NEW)    = ML1(I)
          ML2(JLT_NEW)    = ML2(I)
          N1(JLT_NEW)     = N1(I)
          N2(JLT_NEW)     = N2(I)
          M1(JLT_NEW)     = M1(I)
          M2(JLT_NEW)     = M2(I)
          H1S(JLT_NEW)    = H1S(I)
          H2S(JLT_NEW)    = H2S(I)
          H1M(JLT_NEW)    = H1M(I)
          H2M(JLT_NEW)    = H2M(I)
          NX(JLT_NEW)     = NX(I)
          NY(JLT_NEW)     = NY(I)
          NZ(JLT_NEW)     = NZ(I)
          STIF(JLT_NEW)   = STIF(I)
          GAPV(JLT_NEW)   = GAPV(I)
          VXS1(JLT_NEW)   = VXS1(I)
          VYS1(JLT_NEW)   = VYS1(I)
          VZS1(JLT_NEW)   = VZS1(I)
          VXS2(JLT_NEW)   = VXS2(I)
          VYS2(JLT_NEW)   = VYS2(I)
          VZS2(JLT_NEW)   = VZS2(I)
          VXM1(JLT_NEW)   = VXM1(I)
          VYM1(JLT_NEW)   = VYM1(I)
          VZM1(JLT_NEW)   = VZM1(I)
          VXM2(JLT_NEW)   = VXM2(I)
          VYM2(JLT_NEW)   = VYM2(I)
          VZM2(JLT_NEW)   = VZM2(I)
          MS1(JLT_NEW)    = MS1(I)
          MS2(JLT_NEW)    = MS2(I)
          MM1(JLT_NEW)    = MM1(I)
          MM2(JLT_NEW)    = MM2(I)
        ENDIF
       ENDDO
      ELSE
#include      "vectorize.inc"
       DO I=1,JLT
        IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
          JLT_NEW = JLT_NEW + 1
          CAND_S(JLT_NEW) = CAND_S(I)
          CAND_M(JLT_NEW) = CAND_M(I)
          NL1(JLT_NEW)    = NL1(I)
          NL2(JLT_NEW)    = NL2(I)
          ML1(JLT_NEW)    = ML1(I)
          ML2(JLT_NEW)    = ML2(I)
          N1(JLT_NEW)     = N1(I)
          N2(JLT_NEW)     = N2(I)
          M1(JLT_NEW)     = M1(I)
          M2(JLT_NEW)     = M2(I)
          H1S(JLT_NEW)    = H1S(I)
          H2S(JLT_NEW)    = H2S(I)
          H1M(JLT_NEW)    = H1M(I)
          H2M(JLT_NEW)    = H2M(I)
          NX(JLT_NEW)     = NX(I)
          NY(JLT_NEW)     = NY(I)
          NZ(JLT_NEW)     = NZ(I)
          STIF(JLT_NEW)   = STIF(I)
          GAPV(JLT_NEW)   = GAPV(I)
          VXS1(JLT_NEW)   = VXS1(I)
          VYS1(JLT_NEW)   = VYS1(I)
          VZS1(JLT_NEW)   = VZS1(I)
          VXS2(JLT_NEW)   = VXS2(I)
          VYS2(JLT_NEW)   = VYS2(I)
          VZS2(JLT_NEW)   = VZS2(I)
          VXM1(JLT_NEW)   = VXM1(I)
          VYM1(JLT_NEW)   = VYM1(I)
          VZM1(JLT_NEW)   = VZM1(I)
          VXM2(JLT_NEW)   = VXM2(I)
          VYM2(JLT_NEW)   = VYM2(I)
          VZM2(JLT_NEW)   = VZM2(I)
          MS1(JLT_NEW)    = MS1(I)
          MS2(JLT_NEW)    = MS2(I)
          MM1(JLT_NEW)    = MM1(I)
          MM2(JLT_NEW)    = MM2(I)
          NSMS(JLT_NEW)= NSMS(I)
        ENDIF
       ENDDO
      END IF
C
      RETURN
      END
C===============================================================================
