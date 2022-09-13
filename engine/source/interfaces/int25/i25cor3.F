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
Chd|  I25COR3_1                     source/interfaces/int25/i25cor3.F
Chd|-- called by -----------
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3_1(
     1               JLT     ,X      ,IRECT  ,NSV  ,CAND_E ,
     2               CAND_N  ,IRTLM  ,STF    ,STFN ,STIF   ,
     3               IGAP    ,XI     ,YI     ,ZI   ,
     4               IX1     ,IX2    ,IX3    ,IX4 ,
     5               NSVG    ,NSN    ,
     6               NIN     ,GAP_S  ,GAPS   ,ADMSR ,NOD_NORMAL  ,
     7               XX      ,YY     ,ZZ     ,
     8               NNX     ,NNY    ,NNZ    ,
     9               GAP_M   ,GAPM   ,GAPN_M ,GAPNM ,SUBTRIA     ,
     A               MVOISIN ,MVOISN ,GAP_S_L,GAP_M_L,GAPMXL     ,
     B               LBOUND  ,IBOUND )
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
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NSN, NIN, IGAP,
     .        IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), IRTLM(4,NSN)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), 
     .        NSVG(MVSIZ), ADMSR(4,*), SUBTRIA(MVSIZ),
     .        MVOISIN(4,*), MVOISN(MVSIZ,4), LBOUND(*), IBOUND(4,MVSIZ)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*), GAP_S(*), 
     .   GAPS(MVSIZ), GAP_M(*), GAPM(*), GAPN_M(4,*), GAPNM(4,*), 
     .   GAP_S_L(*), GAP_M_L(*), GAPMXL(*)
C     REAL
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5),
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5)
      REAL*4 NOD_NORMAL(3,4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , L, NN, IG, JFT, IX,  NI, I1, I2, I3, I4
      my_real
     .        XN
C-----------------------------------------------
      DO I=1,JLT
        NI = CAND_N(I)
        IF(NI<=NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
          XI(I) = X(1,IG)
          YI(I) = X(2,IG)
          ZI(I) = X(3,IG)
          GAPS(I) = GAP_S(NI)
        ELSE
          NN = NI - NSN
          NSVG(I) = -NN
          XI(I) = XFI(NIN)%P(1,NN)
          YI(I) = XFI(NIN)%P(2,NN)
          ZI(I) = XFI(NIN)%P(3,NN)
          GAPS(I) = GAPFI(NIN)%P(NN)
        END IF
      END DO
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        IX=IRECT(1,L)  
        IX1(I)=IX      
        XX(I,1)=X(1,IX)  
        YY(I,1)=X(2,IX)  
        ZZ(I,1)=X(3,IX)  
C
        IX=IRECT(2,L)  
        IX2(I)=IX      
        XX(I,2)=X(1,IX)  
        YY(I,2)=X(2,IX)  
        ZZ(I,2)=X(3,IX)  
C
        IX=IRECT(3,L)  
        IX3(I)=IX      
        XX(I,3)=X(1,IX)  
        YY(I,3)=X(2,IX)  
        ZZ(I,3)=X(3,IX)  
C
        IX=IRECT(4,L)  
        IX4(I)=IX      
        XX(I,4)=X(1,IX)  
        YY(I,4)=X(2,IX)  
        ZZ(I,4)=X(3,IX)  
C
        GAPM(I)     =GAP_M(L)
        GAPNM(1:4,I)=GAPN_M(1:4,L)
C
      END DO
C
      IF(IGAP/=3)THEN
        GAPMXL(1:JLT)=EP30
      ELSE
        DO I=1,JLT
          NI = CAND_N(I)
          L  = CAND_E(I)
          IF(NI<=NSN)THEN
            GAPMXL(I)=GAP_S_L(NI)+GAP_M_L(L)
          ELSE
            GAPMXL(I)=GAP_LFI(NIN)%P(NI-NSN)+GAP_M_L(L)
          END IF
        END DO
      END IF
C
      DO I=1,JLT
C
        IF(IX3(I) /= IX4(I))THEN
         XX(I,5)= FOURTH*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4))
         YY(I,5)= FOURTH*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4))
         ZZ(I,5)= FOURTH*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4))
        ELSE
         XX(I,5)= XX(I,3)
         YY(I,5)= YY(I,3)
         ZZ(I,5)= ZZ(I,3) 
        ENDIF
C
      END DO
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        NNX(I,1)=NOD_NORMAL(1,1,L)
        NNY(I,1)=NOD_NORMAL(2,1,L)
        NNZ(I,1)=NOD_NORMAL(3,1,L)
C
        NNX(I,2)=NOD_NORMAL(1,2,L)
        NNY(I,2)=NOD_NORMAL(2,2,L)
        NNZ(I,2)=NOD_NORMAL(3,2,L)
C
        NNX(I,3)=NOD_NORMAL(1,3,L)
        NNY(I,3)=NOD_NORMAL(2,3,L)
        NNZ(I,3)=NOD_NORMAL(3,3,L)
C
        NNX(I,4)=NOD_NORMAL(1,4,L)
        NNY(I,4)=NOD_NORMAL(2,4,L)
        NNZ(I,4)=NOD_NORMAL(3,4,L)
C
      END DO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
          NNX(I,5)=FOURTH*(NNX(I,1)+NNX(I,2)+NNX(I,3)+NNX(I,4))
          NNY(I,5)=FOURTH*(NNY(I,1)+NNY(I,2)+NNY(I,3)+NNY(I,4))
          NNZ(I,5)=FOURTH*(NNZ(I,1)+NNZ(I,2)+NNZ(I,3)+NNZ(I,4))
        ELSE
          NNX(I,5)=NNX(I,4)
          NNY(I,5)=NNY(I,4)
          NNZ(I,5)=NNZ(I,4)
        ENDIF
        XN=ONE/MAX(EM20,SQRT(NNX(I,5)*NNX(I,5)+NNY(I,5)*NNY(I,5)+NNZ(I,5)*NNZ(I,5)))
        NNX(I,5)=XN*NNX(I,5)
        NNY(I,5)=XN*NNY(I,5)
        NNZ(I,5)=XN*NNZ(I,5)       
      END DO
C
      DO I=1,JLT
        L  = CAND_E(I)
        NI = CAND_N(I)
        IF(NI<=NSN)THEN
          STIF(I)=STF(L)*ABS(STFN(NI))
        ELSE
          NN = NI - NSN
          STIF(I)=STF(L)*ABS(STIFI(NIN)%P(NN))
        END IF
C
        IF(NI <= NSN)THEN
          SUBTRIA(I) = MOD(IRTLM(2,NI),5)
        ELSE
          SUBTRIA(I) = MOD(IRTLM_FI(NIN)%P(2,NI-NSN),5)
        END IF
        IF(SUBTRIA(I) < 0) SUBTRIA(I)=-SUBTRIA(I)
      ENDDO
C
      IBOUND(1:4,1:JLT)=0
      DO I=1,JLT
        L   = CAND_E(I)
        DO J=1,4
          MVOISN(I,J)=MVOISIN(J,L)
          IF(LBOUND(ADMSR(J,L))/=0)IBOUND(J,I)=ADMSR(J,L)
        END DO
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  I25COR3_21                    source/interfaces/int25/i25cor3.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3_21(
     1               JLT     ,X      ,IRECT ,NSV   ,CAND_E    ,
     2               CAND_N  ,STF    ,STFN  ,STIF  ,IGAP      ,
     3               XI     ,YI      ,ZI    ,IX1   ,IX2       ,
     4               IX3    ,IX4     ,NSVG  ,NSN   ,MSEGTYP   ,
     5               ETYP   ,NIN     ,GAP_S ,GAPS  ,ADMSR     ,
     6               NOD_NORMAL  ,XX ,YY    ,ZZ    ,NNX       ,
     7               NNY    ,NNZ     ,GAP_M ,GAPM  ,GAPN_M    ,
     8               GAPNM  ,ISLIDE  ,KSLIDE,MVOISIN ,MVOISN  ,
     9               GAP_S_L,GAP_M_L ,GAPMXL,LBOUND  ,IBOUND  )
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
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), MSEGTYP(*),
     .        JLT, NSN, NIN, IGAP
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), 
     .        NSVG(MVSIZ), ADMSR(4,*), ISLIDE(4,*), KSLIDE(MVSIZ,4),
     .        MVOISIN(4,*), MVOISN(MVSIZ,4), LBOUND(*), IBOUND(4,MVSIZ), ETYP(MVSIZ)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*), GAP_S(*), 
     .   GAPS(MVSIZ), GAP_M(*), GAPM(*), GAPN_M(4,*), GAPNM(4,*), 
     .   GAP_S_L(*), GAP_M_L(*), GAPMXL(*)
C     REAL
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5),
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5)
      REAL*4 NOD_NORMAL(3,4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , K, L, NN, IG, JFT, IX,  NI, I1, I2, I3, I4, NOR, II(4)
      my_real
     .        XN
C-----------------------------------------------
      DO I=1,JLT
        NI = CAND_N(I)
        IF(NI<=NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
          XI(I) = X(1,IG)
          YI(I) = X(2,IG)
          ZI(I) = X(3,IG)
          GAPS(I) = GAP_S(NI)
        ELSE
          NN = NI - NSN
          NSVG(I) = -NN
          XI(I) = XFI(NIN)%P(1,NN)
          YI(I) = XFI(NIN)%P(2,NN)
          ZI(I) = XFI(NIN)%P(3,NN)
          GAPS(I) = GAPFI(NIN)%P(NN)
        END IF
      END DO
C
      DO I=1,JLT
C
        L       = CAND_E(I)
        ETYP(I) =MSEGTYP(L)
C
        IX=IRECT(1,L)  
        IX1(I)=IX      
        XX(I,1)=X(1,IX)  
        YY(I,1)=X(2,IX)  
        ZZ(I,1)=X(3,IX)  
C
        IX=IRECT(2,L)  
        IX2(I)=IX      
        XX(I,2)=X(1,IX)  
        YY(I,2)=X(2,IX)  
        ZZ(I,2)=X(3,IX)  
C
        IX=IRECT(3,L)  
        IX3(I)=IX      
        XX(I,3)=X(1,IX)  
        YY(I,3)=X(2,IX)  
        ZZ(I,3)=X(3,IX)  
C
        IX=IRECT(4,L)  
        IX4(I)=IX      
        XX(I,4)=X(1,IX)  
        YY(I,4)=X(2,IX)  
        ZZ(I,4)=X(3,IX)  
C
        GAPM(I)     =GAP_M(L)
        GAPNM(1:4,I)=GAPN_M(1:4,L)
C
      END DO
C
      IF(IGAP/=3)THEN
        GAPMXL(1:JLT)=EP30
      ELSE
        DO I=1,JLT
          NI = CAND_N(I)
          L  = CAND_E(I)
          IF(NI<=NSN)THEN
            GAPMXL(I)=GAP_S_L(NI)+GAP_M_L(L)
          ELSE
            GAPMXL(I)=GAP_LFI(NIN)%P(NI-NSN)+GAP_M_L(L)
          END IF
        END DO
      END IF
C
      DO I=1,JLT
C
        IF(IX3(I) /= IX4(I))THEN
         XX(I,5)= FOURTH*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4))
         YY(I,5)= FOURTH*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4))
         ZZ(I,5)= FOURTH*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4))
        ELSE
         XX(I,5)= XX(I,3)
         YY(I,5)= YY(I,3)
         ZZ(I,5)= ZZ(I,3) 
        ENDIF
C
      END DO
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        NNX(I,1)=NOD_NORMAL(1,1,L)
        NNY(I,1)=NOD_NORMAL(2,1,L)
        NNZ(I,1)=NOD_NORMAL(3,1,L)
C
        NNX(I,2)=NOD_NORMAL(1,2,L)
        NNY(I,2)=NOD_NORMAL(2,2,L)
        NNZ(I,2)=NOD_NORMAL(3,2,L)
C
        NNX(I,3)=NOD_NORMAL(1,3,L)
        NNY(I,3)=NOD_NORMAL(2,3,L)
        NNZ(I,3)=NOD_NORMAL(3,3,L)
C
        NNX(I,4)=NOD_NORMAL(1,4,L)
        NNY(I,4)=NOD_NORMAL(2,4,L)
        NNZ(I,4)=NOD_NORMAL(3,4,L)
C
      END DO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
          NNX(I,5)=FOURTH*(NNX(I,1)+NNX(I,2)+NNX(I,3)+NNX(I,4))
          NNY(I,5)=FOURTH*(NNY(I,1)+NNY(I,2)+NNY(I,3)+NNY(I,4))
          NNZ(I,5)=FOURTH*(NNZ(I,1)+NNZ(I,2)+NNZ(I,3)+NNZ(I,4))
        ELSE
          NNX(I,5)=NNX(I,4)
          NNY(I,5)=NNY(I,4)
          NNZ(I,5)=NNZ(I,4)
        ENDIF
        XN=ONE/MAX(EM20,SQRT(NNX(I,5)*NNX(I,5)+NNY(I,5)*NNY(I,5)+NNZ(I,5)*NNZ(I,5)))
        NNX(I,5)=XN*NNX(I,5)
        NNY(I,5)=XN*NNY(I,5)
        NNZ(I,5)=XN*NNZ(I,5)       
      END DO
C
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
C
      KSLIDE(1:MVSIZ,1:4)=0
      IBOUND(1:4,1:JLT)=0
      DO I=1,JLT
C
        NI  = CAND_N(I)
        L   = CAND_E(I)
        II(1)=ABS(ADMSR(1,L))
        II(2)=ABS(ADMSR(2,L))
        II(3)=ABS(ADMSR(3,L))
        II(4)=ABS(ADMSR(4,L))
        DO J=1,4
          MVOISN(I,J)=MVOISIN(J,L)

          IF(NI<=NSN)THEN
            NOR=ISLIDE(J,NI)
          ELSE
            NN = NI - NSN
            NOR=ISLIDE_FI(NIN)%P(J,NN)
          END IF

          if(nor < 0) print *,'i25cor3-2 internal error'

          IF(NOR/=0)THEN
            DO K=1,4
              IF(NOR==II(K))THEN
                KSLIDE(I,K)=1
                EXIT
              END IF
            END DO
          END IF

          IF(LBOUND(ADMSR(J,L))/=0)IBOUND(J,I)=ADMSR(J,L)
        END DO
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  I25COR3_22                    source/interfaces/int25/i25cor3.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3_22(
     1               JLT     ,X      ,IRECT ,NSV   ,CAND_E      ,
     2               CAND_N  ,STF    ,STFN ,STIF   ,IGAP        ,
     3               XI     ,YI      ,ZI    ,VXI   ,VYI         ,
     4               VZI    ,IX1     ,IX2   ,IX3   ,IX4         ,
     5               NSVG   ,NSN     ,V     ,
     6               NIN    ,GAP_S   ,GAPS  ,ADMSR ,NOD_NORMAL  ,
     7               XX     ,YY     ,ZZ     ,
     C               VX1     ,VX2    ,VX3   ,VX4   ,
     D               VY1     ,VY2    ,VY3   ,VY4   ,
     E               VZ1     ,VZ2    ,VZ3   ,VZ4   ,
     E               NAX     ,NAY   ,NAZ     ,
     E               NBX     ,NBY   ,NBZ     ,
     J               GAP_M   ,GAPM  ,GAPN_M  ,GAPNM  ,
     L               MVOISIN ,NRTM  ,MSEGTYP ,ISHEL  ,
     P               MVOISA  ,MVOISB,GAP_S_L,GAP_M_L,GAPMXL     ,
     Q               LBOUND  ,IBOUNDA,IBOUNDB)
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
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        JLT, NSN, NIN, NRTM, IGAP, MSEGTYP(*), ISHEL(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), 
     .        NSVG(MVSIZ), ADMSR(4,*), 
     .        MVOISIN(4,*), MVOISA(MVSIZ,4), MVOISB(MVSIZ,4),
     .        LBOUND(*),IBOUNDA(4,MVSIZ),IBOUNDB(4,MVSIZ)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*), V(3,*), GAP_S(*), 
     .   GAPS(MVSIZ), GAP_M(*), GAPM(*), GAPN_M(4,*), GAPNM(4,*), 
     .   GAP_S_L(*), GAP_M_L(*), GAPMXL(*)
C     REAL
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5),
     .   VX1(MVSIZ),VY1(MVSIZ),VZ1(MVSIZ),
     .   VX2(MVSIZ),VY2(MVSIZ),VZ2(MVSIZ),
     .   VX3(MVSIZ),VY3(MVSIZ),VZ3(MVSIZ),
     .   VX4(MVSIZ),VY4(MVSIZ),VZ4(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ),
     .   NAX(MVSIZ,5), NAY(MVSIZ,5), NAZ(MVSIZ,5),
     .   NBX(MVSIZ,5), NBY(MVSIZ,5), NBZ(MVSIZ,5)
      REAL*4 NOD_NORMAL(3,4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , K, L, NN, IG, JFT, IX,  NI, I1, I2, I3, I4, NOR, II(4), ISH
      my_real
     .        XN
C-----------------------------------------------
      DO I=1,JLT
        NI = CAND_N(I)
        IF(NI<=NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
          XI(I) = X(1,IG)
          YI(I) = X(2,IG)
          ZI(I) = X(3,IG)
          VXI(I) = V(1,IG)
          VYI(I) = V(2,IG)
          VZI(I) = V(3,IG)
          GAPS(I) = GAP_S(NI)
        ELSE
          NN = NI - NSN
          NSVG(I) = -NN
          XI(I) = XFI(NIN)%P(1,NN)
          YI(I) = XFI(NIN)%P(2,NN)
          ZI(I) = XFI(NIN)%P(3,NN)
          VXI(I)= VFI(NIN)%P(1,NN)
          VYI(I)= VFI(NIN)%P(2,NN)
          VZI(I)= VFI(NIN)%P(3,NN)
          GAPS(I) = GAPFI(NIN)%P(NN)
        END IF
      END DO
C
      ISHEL(1:MVSIZ)=0
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        IX=IRECT(1,L)  
        IX1(I)=IX      
        XX(I,1)=X(1,IX)  
        YY(I,1)=X(2,IX)  
        ZZ(I,1)=X(3,IX)  
        VX1(I)=V(1,IX)  
        VY1(I)=V(2,IX)  
        VZ1(I)=V(3,IX)  
C
        IX=IRECT(2,L)  
        IX2(I)=IX      
        XX(I,2)=X(1,IX)  
        YY(I,2)=X(2,IX)  
        ZZ(I,2)=X(3,IX)  
        VX2(I)=V(1,IX)  
        VY2(I)=V(2,IX)  
        VZ2(I)=V(3,IX)  
C
        IX=IRECT(3,L)  
        IX3(I)=IX      
        XX(I,3)=X(1,IX)  
        YY(I,3)=X(2,IX)  
        ZZ(I,3)=X(3,IX)  
        VX3(I)=V(1,IX)  
        VY3(I)=V(2,IX)  
        VZ3(I)=V(3,IX)  
C
        IX=IRECT(4,L)  
        IX4(I)=IX      
        XX(I,4)=X(1,IX)  
        YY(I,4)=X(2,IX)  
        ZZ(I,4)=X(3,IX)  
        VX4(I)=V(1,IX)  
        VY4(I)=V(2,IX)  
        VZ4(I)=V(3,IX)  
C
        GAPM(I)     =GAP_M(L)
        GAPNM(1:4,I)=GAPN_M(1:4,L)
C
        ISH=MSEGTYP(L)
        IF(ISH > 0) THEN
          IF(ISH > NRTM)ISH=ISH-NRTM
          ISHEL(I)=ISH
        END IF
C
      END DO
C
      IF(IGAP/=3)THEN
        GAPMXL(1:JLT)=EP30
      ELSE
        DO I=1,JLT
          NI = CAND_N(I)
          L  = CAND_E(I)
          IF(NI<=NSN)THEN
            GAPMXL(I)=GAP_S_L(NI)+GAP_M_L(L)
          ELSE
            GAPMXL(I)=GAP_LFI(NIN)%P(NI-NSN)+GAP_M_L(L)
          END IF
        END DO
      END IF
C
      DO I=1,JLT
C
        IF(IX3(I) /= IX4(I))THEN
         XX(I,5)= FOURTH*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4))
         YY(I,5)= FOURTH*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4))
         ZZ(I,5)= FOURTH*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4))
        ELSE
         XX(I,5)= XX(I,3)
         YY(I,5)= YY(I,3)
         ZZ(I,5)= ZZ(I,3) 
        ENDIF
C
      END DO
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        NAX(I,1)=NOD_NORMAL(1,1,L)
        NAY(I,1)=NOD_NORMAL(2,1,L)
        NAZ(I,1)=NOD_NORMAL(3,1,L)
C
        NAX(I,2)=NOD_NORMAL(1,2,L)
        NAY(I,2)=NOD_NORMAL(2,2,L)
        NAZ(I,2)=NOD_NORMAL(3,2,L)
C
        NAX(I,3)=NOD_NORMAL(1,3,L)
        NAY(I,3)=NOD_NORMAL(2,3,L)
        NAZ(I,3)=NOD_NORMAL(3,3,L)
C
        NAX(I,4)=NOD_NORMAL(1,4,L)
        NAY(I,4)=NOD_NORMAL(2,4,L)
        NAZ(I,4)=NOD_NORMAL(3,4,L)
C
      END DO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
         NAX(I,5)= FOURTH*(NAX(I,1)+NAX(I,2)+NAX(I,3)+NAX(I,4))
         NAY(I,5)= FOURTH*(NAY(I,1)+NAY(I,2)+NAY(I,3)+NAY(I,4))
         NAZ(I,5)= FOURTH*(NAZ(I,1)+NAZ(I,2)+NAZ(I,3)+NAZ(I,4))
        ELSE
         NAX(I,5)= NAX(I,4)
         NAY(I,5)= NAY(I,4)
         NAZ(I,5)= NAZ(I,4)
        ENDIF
        XN=ONE/MAX(EM20,SQRT(NAX(I,5)*NAX(I,5)+NAY(I,5)*NAY(I,5)+NAZ(I,5)*NAZ(I,5)))
        NAX(I,5)=XN*NAX(I,5)
        NAY(I,5)=XN*NAY(I,5)
        NAZ(I,5)=XN*NAZ(I,5)       
      END DO
C
      DO I=1,JLT
C
        L  = ISHEL(I)
        IF(L==0) CYCLE
C
        IF(IX3(I)/=IX4(I))THEN
C
          NBX(I,1)=NOD_NORMAL(1,1,L)
          NBY(I,1)=NOD_NORMAL(2,1,L)
          NBZ(I,1)=NOD_NORMAL(3,1,L)
C
          NBX(I,2)=NOD_NORMAL(1,4,L)
          NBY(I,2)=NOD_NORMAL(2,4,L)
          NBZ(I,2)=NOD_NORMAL(3,4,L)
C
          NBX(I,3)=NOD_NORMAL(1,3,L)
          NBY(I,3)=NOD_NORMAL(2,3,L)
          NBZ(I,3)=NOD_NORMAL(3,3,L)
C
          NBX(I,4)=NOD_NORMAL(1,2,L)
          NBY(I,4)=NOD_NORMAL(2,2,L)
          NBZ(I,4)=NOD_NORMAL(3,2,L)
C
        ELSE
C
          NBX(I,1)=NOD_NORMAL(1,1,L)
          NBY(I,1)=NOD_NORMAL(2,1,L)
          NBZ(I,1)=NOD_NORMAL(3,1,L)
C
          NBX(I,2)=NOD_NORMAL(1,4,L)
          NBY(I,2)=NOD_NORMAL(2,4,L)
          NBZ(I,2)=NOD_NORMAL(3,4,L)
C
          NBX(I,4)=NOD_NORMAL(1,2,L)
          NBY(I,4)=NOD_NORMAL(2,2,L)
          NBZ(I,4)=NOD_NORMAL(3,2,L)
C
        ENDIF
C
      END DO
C
      DO I=1,JLT
C
        L  = ISHEL(I)
        IF(L==0) CYCLE
C
        IF(IX3(I)/=IX4(I))THEN
         NBX(I,5)= FOURTH*(NBX(I,1)+NBX(I,2)+NBX(I,3)+NBX(I,4))
         NBY(I,5)= FOURTH*(NBY(I,1)+NBY(I,2)+NBY(I,3)+NBY(I,4))
         NBZ(I,5)= FOURTH*(NBZ(I,1)+NBZ(I,2)+NBZ(I,3)+NBZ(I,4))
        ELSE
         NBX(I,5)= NBX(I,4)
         NBY(I,5)= NBY(I,4)
         NBZ(I,5)= NBZ(I,4)
        ENDIF
        XN=ONE/MAX(EM20,SQRT(NBX(I,5)*NBX(I,5)+NBY(I,5)*NBY(I,5)+NBZ(I,5)*NBZ(I,5)))
        NBX(I,5)=XN*NBX(I,5)
        NBY(I,5)=XN*NBY(I,5)
        NBZ(I,5)=XN*NBZ(I,5)       
      END DO
C
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
C
      IBOUNDA(1:4,1:JLT)=0
      DO I=1,JLT
        L   = CAND_E(I)
        DO J=1,4
          MVOISA(I,J) =MVOISIN(J,L)
          IF(LBOUND(ADMSR(J,L))/=0)IBOUNDA(J,I)=ADMSR(J,L)
        END DO
      END DO
C
      IBOUNDB(1:4,1:JLT)=0
      DO I=1,JLT
        L  = ISHEL(I)
        IF(L==0) CYCLE
  
        MVOISB(I,1)=MVOISIN(1,L)
        MVOISB(I,2)=MVOISIN(4,L)
        MVOISB(I,3)=MVOISIN(3,L)
        MVOISB(I,4)=MVOISIN(2,L)

        IF(LBOUND(ADMSR(2,L))/=0)IBOUNDB(1,I)=ADMSR(2,L)
        IF(LBOUND(ADMSR(1,L))/=0)IBOUNDB(2,I)=ADMSR(1,L)
        IF(LBOUND(ADMSR(4,L))/=0)IBOUNDB(3,I)=ADMSR(4,L)
        IF(LBOUND(ADMSR(3,L))/=0)IBOUNDB(4,I)=ADMSR(3,L)
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  I25COR3_3                     source/interfaces/int25/i25cor3.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3_3(
     1               JLT     ,X       ,IRECT  ,NSV   ,CAND_E      ,
     2               CAND_N  ,STF     ,STFN   ,STIF   ,NOD_NORMAL ,
     3               IGSTI  ,KMIN     ,KMAX   ,MS     ,MSI        ,
     3               XI     ,YI       ,ZI     ,VXI    ,VYI        ,
     4               VZI    ,IX1      ,IX2    ,IX3    ,IX4        ,
     5               NSVG   ,NSN      ,V      ,KINET  ,KINI       ,
     6               NIN    ,ADMSR    ,IRTLM  ,SUBTRIA,    
     7               XX      ,YY      ,ZZ     ,LBOUND ,IBOUND     ,
     8               NNX     ,NNY     ,NNZ    ,
     9               VX1     ,VX2     ,VX3    ,VX4    ,
     A               VY1     ,VY2     ,VY3    ,VY4    ,
     B               VZ1     ,VZ2     ,VZ3    ,VZ4    ,
     C               NODNX_SMS,NSMS   ,INDEX  ,PENM   ,LBM         ,
     D               LCM      ,PENE   ,LB     ,LC     , 
     E               GAPN_M   ,GAPNM  ,GAP_S  ,GAPS    ,IGAP       ,
     F               GAP_S_L  ,GAP_M_L,GAPMXL ,INTFRIC ,IPARTFRICS ,
     G               IPARTFRICSI,IPARTFRICM,IPARTFRICMI,AREAS,AREASI,
     H               IVIS2    ,MVOISIN,MVOISN ,IORTHFRIC,IREP_FRICM ,
     I               DIR_FRICM ,IREP_FRICMI,DIR_FRICMI,X1 ,Y1       , 
     J               Z1        ,X2    ,Y2      ,Z2     ,X3          , 
     K               Y3        ,Z3    ,X4      ,Y4     ,Z4          ,
     L               INTTH     ,TEMP  ,TEMPI   ,IELES  ,IELESI      ,
     M               IELEM     ,IELEMI) 
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
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INTTH ,JLT, NSN, NIN, IGSTI, IGAP,INTFRIC, IVIS2, IORTHFRIC
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), KINET(*), KINI(*),
     .        NODNX_SMS(*), INDEX(*), ADMSR(4,*),
     .        LBOUND(*), IBOUND(4,*), MVOISIN(4,*), MVOISN(MVSIZ,4),IELES(*),
     .        IELEM(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), 
     .        NSVG(MVSIZ), NSMS(MVSIZ), IRTLM(4,*), SUBTRIA(MVSIZ),
     .        IPARTFRICS(*),IPARTFRICSI(MVSIZ),IPARTFRICM(*),IPARTFRICMI(MVSIZ),
     .        IREP_FRICM(*),IREP_FRICMI(MVSIZ),IELESI(MVSIZ),IELEMI(MVSIZ)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*), MS(*), V(3,*), 
     .   PENM(4,*), LBM(4,*), LCM(4,*), 
     .   GAPN_M(4,*), GAP_S(*), 
     .   GAP_S_L(*), GAP_M_L(*), GAPMXL(*), AREAS(*),TEMP(*)
C     REAL
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), KMIN, KMAX,
     .   XX(MVSIZ,5),YY(MVSIZ,5),ZZ(MVSIZ,5),
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .   VX1(MVSIZ),VY1(MVSIZ),VZ1(MVSIZ),
     .   VX2(MVSIZ),VY2(MVSIZ),VZ2(MVSIZ),
     .   VX3(MVSIZ),VY3(MVSIZ),VZ3(MVSIZ),
     .   VX4(MVSIZ),VY4(MVSIZ),VZ4(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ),
     .   MSI(MVSIZ), LB(MVSIZ), LC(MVSIZ), PENE(MVSIZ),
     .   GAPNM(4,MVSIZ), GAPS(MVSIZ),AREASI(MVSIZ),
     .   DIR_FRICM(2,*) ,DIR_FRICMI(MVSIZ,2),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   TEMPI(MVSIZ)
      REAL*4 NOD_NORMAL(3,4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , L, NN, IG, JFT, IX,  NI, I1, I2, I3, I4, IT
      my_real
     .        XN
C-----------------------------------------------
      IBOUND(1:4,1:JLT)=0
      DO I=1,JLT
           NI = CAND_N(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
C---------------voir quand KINET(IG) est utilise  
             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
             GAPS(I) = GAP_S(NI)

             SUBTRIA(I)=MOD(IRTLM(2,NI),5)
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
             GAPS(I) = GAPFI(NIN)%P(NN)

             SUBTRIA(I)=MOD(IRTLM_FI(NIN)%P(2,NN),5)
           END IF
C
           IF(SUBTRIA(I) < 0)SUBTRIA(I)=-SUBTRIA(I)
C
           L  = CAND_E(I)
C
           IX=IRECT(1,L)  
           IX1(I)=IX	  
           XX(I,1)=X(1,IX)  
           YY(I,1)=X(2,IX)  
           ZZ(I,1)=X(3,IX)  
           VX1(I)=V(1,IX)  
           VY1(I)=V(2,IX)  
           VZ1(I)=V(3,IX)  
C
           IX=IRECT(2,L)  
           IX2(I)=IX	  
           XX(I,2)=X(1,IX)  
           YY(I,2)=X(2,IX)  
           ZZ(I,2)=X(3,IX)  
           VX2(I)=V(1,IX)  
           VY2(I)=V(2,IX)  
           VZ2(I)=V(3,IX)  
C
           IX=IRECT(3,L)  
           IX3(I)=IX	  
           XX(I,3)=X(1,IX)  
           YY(I,3)=X(2,IX)  
           ZZ(I,3)=X(3,IX)  
           VX3(I)=V(1,IX)  
           VY3(I)=V(2,IX)  
           VZ3(I)=V(3,IX)  
C
           IX=IRECT(4,L)  
           IX4(I)=IX	  
           XX(I,4)=X(1,IX)  
           YY(I,4)=X(2,IX)  
           ZZ(I,4)=X(3,IX)  
           VX4(I)=V(1,IX)  
           VY4(I)=V(2,IX)  
           VZ4(I)=V(3,IX)  
C
           IF(IX3(I) /= IX4(I))THEN
            XX(I,5)= FOURTH*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4))
            YY(I,5)= FOURTH*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4))
            ZZ(I,5)= FOURTH*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4))
           ELSE
            XX(I,5)= XX(I,3)
            YY(I,5)= YY(I,3)
            ZZ(I,5)= ZZ(I,3) 
           ENDIF
C
           GAPNM(1:4,I)=GAPN_M(1:4,L)
C
           DO J=1,4
             MVOISN(I,J) =MVOISIN(J,L)
             IF(LBOUND(ADMSR(J,L))/=0)IBOUND(J,I)=ADMSR(J,L)
           END DO
C
      END DO
C
      IF(IGAP/=3)THEN
        GAPMXL(1:JLT)=EP30
      ELSE
        DO I=1,JLT
          NI = CAND_N(I)
          L  = CAND_E(I)
          IF(NI<=NSN)THEN
            GAPMXL(I)=GAP_S_L(NI)+GAP_M_L(L)
          ELSE
            GAPMXL(I)=GAP_LFI(NIN)%P(NI-NSN)+GAP_M_L(L)
          END IF
        END DO
      END IF
C
      DO I=1,JLT
C
        L  = CAND_E(I)
C
        NNX(I,1)=NOD_NORMAL(1,1,L)
        NNY(I,1)=NOD_NORMAL(2,1,L)
        NNZ(I,1)=NOD_NORMAL(3,1,L)
C
        NNX(I,2)=NOD_NORMAL(1,2,L)
        NNY(I,2)=NOD_NORMAL(2,2,L)
        NNZ(I,2)=NOD_NORMAL(3,2,L)
C
        NNX(I,3)=NOD_NORMAL(1,3,L)
        NNY(I,3)=NOD_NORMAL(2,3,L)
        NNZ(I,3)=NOD_NORMAL(3,3,L)
C
        NNX(I,4)=NOD_NORMAL(1,4,L)
        NNY(I,4)=NOD_NORMAL(2,4,L)
        NNZ(I,4)=NOD_NORMAL(3,4,L)
C
      END DO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
          NNX(I,5)=FOURTH*(NNX(I,1)+NNX(I,2)+NNX(I,3)+NNX(I,4))
          NNY(I,5)=FOURTH*(NNY(I,1)+NNY(I,2)+NNY(I,3)+NNY(I,4))
          NNZ(I,5)=FOURTH*(NNZ(I,1)+NNZ(I,2)+NNZ(I,3)+NNZ(I,4))
        ELSE
          NNX(I,5)=NNX(I,4)
          NNY(I,5)=NNY(I,4)
          NNZ(I,5)=NNZ(I,4)
        ENDIF
        XN=ONE/MAX(EM20,SQRT(NNX(I,5)*NNX(I,5)+NNY(I,5)*NNY(I,5)+NNZ(I,5)*NNZ(I,5)))
        NNX(I,5)=XN*NNX(I,5)
        NNY(I,5)=XN*NNY(I,5)
        NNZ(I,5)=XN*NNZ(I,5)       
      END DO
C
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
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
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
      ELSEIF(IGSTI==4.OR.IGSTI==6)THEN
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
          STIF(I)=STF(L)*STIF(I)/
     .            MAX(EM30,(STF(L)+STIF(I)))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
      ENDIF
C----------
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
C----------
      DO I=1,JLT
        IT = SUBTRIA(I)
        PENE(I)=PENM(IT,INDEX(I))
        LB(I)  =LBM(IT,INDEX(I))
        LC(I)  =LCM(IT,INDEX(I))
      ENDDO

C----Friction model : secnd part IDs---------
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
C
           IF(IORTHFRIC > 0) THEN
              IREP_FRICMI(I) =IREP_FRICM(L) 
              DIR_FRICMI(I,1:2)=DIR_FRICM(1:2,L) 
           ENDIF
         ENDDO
       ENDIF

        DO I=1,JLT
           X1(I)= XX(I,1)
           X2(I)= XX(I,2)
           X3(I)= XX(I,3)
           X4(I)= XX(I,4)
           Y1(I)= YY(I,1)
           Y2(I)= YY(I,2)
           Y3(I)= YY(I,3)
           Y4(I)= YY(I,4)
           Z1(I)= ZZ(I,1)
           Z2(I)= ZZ(I,2)
           Z3(I)= ZZ(I,3)
           Z4(I)= ZZ(I,4)
        ENDDO

C----Adhesion case - mvsize division of if_adh done in dst3_3
        
      IF(INTTH>0.OR.IVIS2==-1) THEN
        DO I=1,JLT
           NI = CAND_N(I)           
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             AREASI(I)= AREAS(NI)
           ELSE                
             NN = NI - NSN
             AREASI(I)= AREASFI(NIN)%P(NN)
           END IF
         ENDDO
      ENDIF

C----Thermal case - 
      IF(INTTH>0) THEN
        DO I=1,JLT
           NI = CAND_N(I)           
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             TEMPI(I) = TEMP(IG)
             IELESI(I)= IELES(NI)
           ELSE
             NN = NI - NSN
             TEMPI(I) = TEMPFI(NIN)%P(NN)
             IELESI(I)= MATSFI(NIN)%P(NN)
           END IF
           IELEMI(I) = IELEM(L)
         ENDDO
      ENDIF    
C
      RETURN
      END
