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
Chd|  I25ASS3                       source/interfaces/int25/i25ass3.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        I25ASS0                       source/interfaces/int25/i25ass3.F
Chd|        I25ASS2                       source/interfaces/int25/i25ass3.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASS3(JLT     ,NSVG      ,ITAB      ,CE_LOC    ,
     2    JTASK       ,NIN       ,NOINT     ,INTPLY    ,A         ,
     3    STIF        ,STIFN     ,NISKYFI   ,FSKYI     ,ISKY      ,
     4    N1          ,N2        ,N3        ,H1        ,H2        ,
     5    H3          ,H4        ,IX1       ,IX2       ,IX3       ,
     6    IX4         ,INTTH     ,FTHE      ,FTHESKYI  ,
     7    PHI         ,PHI1      ,PHI2      ,PHI3      ,PHI4      ,
     8    FNI         ,MSEGTYP   ,APINCH    ,STIFPINCH , 
     9    FX1         ,FY1      ,FZ1        ,FX2       ,FY2       ,
     A    FZ2         ,FX3      ,FY3        ,FZ3       ,FX4       ,
     B    FY4         ,FZ4      ,FXI        ,FYI       ,FZI       ,
     F    IFORM    ,CONDINT    ,CONDN     ,CONDNSKYI )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PINCHTYPE_MOD
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NOINT, JTASK, NISKYFI, INTTH, INTPLY,
     .        IFORM,
     .        ITAB(*), ISKY(*), MSEGTYP(*)
      INTEGER 
     .        CE_LOC(MVSIZ),NSVG(MVSIZ),
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
      my_real
     .   A(3,*), STIFN(*), FSKYI(LSKYI,NFSKYI), 
     .   APINCH(3,*),STIFPINCH(*)
      my_real
     .     STIF(MVSIZ), N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     FTHE(*), FTHESKYI(LSKYI),
     .    CONDN(*),CONDNSKYI(LSKYI)
      my_real
     .    FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .    FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .    FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .    FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .    FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .    PHI(MVSIZ),PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),
     .    PHI4(MVSIZ),CONDINT(MVSIZ)
C-----------------------------------------------
C
      IF(IPARIT==3)THEN
         stop 789
      ELSEIF(IPARIT==0)THEN
         CALL I25ASS0(JLT  ,IX1    ,IX2  ,IX3  ,IX4    ,
     2               NSVG  ,H1     ,H2   ,H3   ,H4     ,STIF ,
     3               FX1   ,FY1    ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4               FX3   ,FY3    ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5               FXI   ,FYI    ,FZI  ,A    ,STIFN  ,NIN  ,
     6               INTTH ,PHI    ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7               PHI4  ,JTASK  ,APINCH     ,STIFPINCH, MSEGTYP, CE_LOC,     
     8               FNI   ,N1     ,N2         ,N3     ,IFORM,
     9               CONDINT,CONDN )
      ELSE
         CALL I25ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2               NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3               FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4               FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5               FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6               NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI ,INTPLY,
     7               PHI1  ,PHI2  ,PHI3 , PHI4 ,ITAB ,IFORM,
     C               CONDINT,CONDNSKYI )     
      ENDIF
C
C
      RETURN
      END
Chd|====================================================================
Chd|  I25ASS2                       source/interfaces/int25/i25ass3.F
Chd|-- called by -----------
Chd|        I25ASS3                       source/interfaces/int25/i25ass3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  FX1   ,FY1   ,FZ1  ,FX2  ,FY2   ,FZ2    ,
     4                  FX3   ,FY3   ,FZ3  ,FX4  ,FY4   ,FZ4    ,
     5                  FXI   ,FYI   ,FZI  ,FSKYI,ISKY  ,NISKYFI,
     6                  NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI,INTPLY,
     7                  PHI1  ,PHI2  ,PHI3 , PHI4 ,ITAB,IFORM,
     C                  CONDINT,CONDNSKYI )       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
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
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NISKYFI,NIN,NOINT,INTTH,INTPLY,IFORM,
     .        ISKY(*),ITAB(*),
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    FSKYI(LSKYI,NFSKYI),FTHESKYI(LSKYI),PHI(MVSIZ),
     .    PHI1(MVSIZ),PHI2(MVSIZ)  ,PHI3(MVSIZ) ,PHI4(MVSIZ),
     .    CONDINT(MVSIZ),
     .    CONDNSKYI(LSKYI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    HH(MVSIZ),FICI(5),FICS(4,4),FICSTH(4,5)
      INTEGER I, J1, IG, NISKYL1, NISKYL,IGP,IGM,IDR,NISKYFIL,J
      INTEGER ITG,NN,ILY,JG,IXSS(4)
C
      NISKYL1 = 0
      DO I = 1, JLT
       HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
       IF(HH(I)==ZERO)CYCLE
        IF (H1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
        IF (H2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
        IF (H3(I)/=ZERO) NISKYL1 = NISKYL1 + 1
        IF (H4(I)/=ZERO) NISKYL1 = NISKYL1 + 1
        
      ENDDO
C
C Precalcul impact locaux / remote
C
      IGP = 0
      IGM = 0
      DO I=1,JLT
       IF(HH(I)==ZERO)CYCLE
        IG =NSVG(I)
        IF(IG>0) THEN
          IGP = IGP+1
        ELSE
          IGM = IGM+1
        ENDIF
      ENDDO
C
#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIL = NISKYFI
      NISKYFI = NISKYFI + IGM
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM > NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C  
      IF(INTTH == 0 ) THEN
        DO I=1,JLT
          IF (H1(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
            ISKY(NISKYL) = IX1(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H2(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
            ISKY(NISKYL) = IX2(I)    
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H3(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
            ISKY(NISKYL) = IX3(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H4(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX4(I)
            FSKYI(NISKYL,2)=FY4(I)
            FSKYI(NISKYL,3)=FZ4(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
            ISKY(NISKYL) = IX4(I)
          ENDIF
        ENDDO
C
        DO I=1,JLT
          IF(HH(I)==ZERO)CYCLE
          IG =NSVG(I)
          IF(IG>0) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=-FXI(I)
            FSKYI(NISKYL,2)=-FYI(I)
            FSKYI(NISKYL,3)=-FZI(I)
            FSKYI(NISKYL,4)= STIF(I)
            ISKY(NISKYL) = IG 
          ELSE
            IG = -IG
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
            FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
            ISKYFI(NIN)%P(NISKYFIL) = IG
          ENDIF  
        ENDDO
C Thermique
      ELSE
        DO I=1,JLT
          IF (H1(I)/=0.) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
            ISKY(NISKYL) = IX1(I)
            FTHESKYI(NISKYL) = PHI1(I)
            IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H1(I))
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H2(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
            ISKY(NISKYL) = IX2(I)
            FTHESKYI(NISKYL) = PHI2(I) 
            IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H2(I))         
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H3(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
            ISKY(NISKYL) = IX3(I)
            FTHESKYI(NISKYL) = PHI3(I)
            IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H3(I))
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H4(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX4(I)
            FSKYI(NISKYL,2)=FY4(I)
            FSKYI(NISKYL,3)=FZ4(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
            ISKY(NISKYL) = IX4(I)
            FTHESKYI(NISKYL) = PHI4(I)
            IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H4(I))
          ENDIF
        ENDDO       
C
        DO I=1,JLT
         IF(HH(I)==ZERO)CYCLE
          IG =NSVG(I)
          IF(IG>0) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=-FXI(I)
            FSKYI(NISKYL,2)=-FYI(I)
            FSKYI(NISKYL,3)=-FZI(I)
            FSKYI(NISKYL,4)= STIF(I)
            ISKY(NISKYL) = IG
            FTHESKYI(NISKYL)=PHI(I)
            IF(NODADT_THERM == 1) CONDNSKYI(NISKYL) = CONDINT(I)
          ELSE
            IG = -IG
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
            FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
            ISKYFI(NIN)%P(NISKYFIL) = IG
            FTHESKYFI(NIN)%P(NISKYFIL)=PHI(I)
            IF(NODADT_THERM == 1) CONDNSKYFI(NIN)%P(NISKYFIL) = CONDINT(I)
          ENDIF  
        ENDDO
      ENDIF
C      
      RETURN
      END
Chd|====================================================================
Chd|  I25ASS0                       source/interfaces/int25/i25ass3.F
Chd|-- called by -----------
Chd|        I25ASS3                       source/interfaces/int25/i25ass3.F
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                  NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                  FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                  FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                  FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6                  INTTH ,PHI  ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7                  PHI4  ,JTASK,APINCH     ,STIFPINCH ,MSEGTYP, CE_LOC,
     8                  FNI   ,N1   ,N2         ,N3     ,IFORM,
     9                  CONDINT,CONDN )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PINCHTYPE_MOD
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
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN,INTTH,IFORM,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),JTASK,
     .        MSEGTYP(*),CE_LOC(MVSIZ) 
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    CONDINT(MVSIZ),
     .    A(3,*),  STIFN(*),PHI(*), FTHE(*),
     .    PHI1(*), PHI2(*), PHI3(*), PHI4(*),
     .    APINCH(3,*),STIFPINCH(*),FNI(*),N1(*),N2(*),N3(*),
     .    CONDN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    HH(MVSIZ),FICI(5),FICS(4,4),FICSTH(4,5),FACT
      INTEGER I, J1, IG,ILY,NN,JG,IXSS(4),NFIC,J,ISHIFT,NODFI

C      
      IF(INTTH == 0) THEN
        IF(NPINCH > 0)THEN
          DO I=1,JLT
           FACT = HALF
           IF(MSEGTYP(CE_LOC(I))<0) THEN
             FACT = -HALF
           ENDIF
C
           HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
           IF(HH(I)==ZERO)CYCLE
C
           J1=IX1(I)
           A(1,J1)=A(1,J1)+FX1(I)
           A(2,J1)=A(2,J1)+FY1(I)
           A(3,J1)=A(3,J1)+FZ1(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
           APINCH(1,J1)=APINCH(1,J1)+FACT*FNI(I)*H1(I)*N1(I)
           APINCH(2,J1)=APINCH(2,J1)+FACT*FNI(I)*H1(I)*N2(I)
           APINCH(3,J1)=APINCH(3,J1)+FACT*FNI(I)*H1(I)*N3(I)
           STIFPINCH(J1) = STIFPINCH(J1) + STIF(I)*ABS(H1(I))
C
           J1=IX2(I)
           A(1,J1)=A(1,J1)+FX2(I)
           A(2,J1)=A(2,J1)+FY2(I)
           A(3,J1)=A(3,J1)+FZ2(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
           APINCH(1,J1)=APINCH(1,J1)+FACT*FNI(I)*H2(I)*N1(I)
           APINCH(2,J1)=APINCH(2,J1)+FACT*FNI(I)*H2(I)*N2(I)
           APINCH(3,J1)=APINCH(3,J1)+FACT*FNI(I)*H2(I)*N3(I)
           STIFPINCH(J1) = STIFPINCH(J1) + STIF(I)*ABS(H2(I))
C
           J1=IX3(I)
           A(1,J1)=A(1,J1)+FX3(I)
           A(2,J1)=A(2,J1)+FY3(I)
           A(3,J1)=A(3,J1)+FZ3(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
           APINCH(1,J1)=APINCH(1,J1)+FACT*FNI(I)*H3(I)*N1(I)
           APINCH(2,J1)=APINCH(2,J1)+FACT*FNI(I)*H3(I)*N2(I)
           APINCH(3,J1)=APINCH(3,J1)+FACT*FNI(I)*H3(I)*N3(I)
           STIFPINCH(J1) = STIFPINCH(J1) + STIF(I)*ABS(H3(I))
C
           J1=IX4(I)
           A(1,J1)=A(1,J1)+FX4(I)
           A(2,J1)=A(2,J1)+FY4(I)
           A(3,J1)=A(3,J1)+FZ4(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
           APINCH(1,J1)=APINCH(1,J1)+FACT*FNI(I)*H4(I)*N1(I)
           APINCH(2,J1)=APINCH(2,J1)+FACT*FNI(I)*H4(I)*N2(I)
           APINCH(3,J1)=APINCH(3,J1)+FACT*FNI(I)*H4(I)*N3(I)
           STIFPINCH(J1) = STIFPINCH(J1) + STIF(I)*ABS(H4(I))      
         ENDDO  
        ELSE ! NPINCH > 0
         DO I=1,JLT
           HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
           IF(HH(I)==ZERO)CYCLE
           J1=IX1(I)
           A(1,J1)=A(1,J1)+FX1(I)
           A(2,J1)=A(2,J1)+FY1(I)
           A(3,J1)=A(3,J1)+FZ1(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
C
           J1=IX2(I)
           A(1,J1)=A(1,J1)+FX2(I)
           A(2,J1)=A(2,J1)+FY2(I)
           A(3,J1)=A(3,J1)+FZ2(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
C
           J1=IX3(I)
           A(1,J1)=A(1,J1)+FX3(I)
           A(2,J1)=A(2,J1)+FY3(I)
           A(3,J1)=A(3,J1)+FZ3(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
C
           J1=IX4(I)
           A(1,J1)=A(1,J1)+FX4(I)
           A(2,J1)=A(2,J1)+FY4(I)
           A(3,J1)=A(3,J1)+FZ4(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
         ENDDO
        ENDIF ! NPINCH > 0
      ELSE
          DO I=1,JLT
           HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
           IF(HH(I)==ZERO)CYCLE
           J1=IX1(I)
           A(1,J1)=A(1,J1)+FX1(I)
           A(2,J1)=A(2,J1)+FY1(I)
           A(3,J1)=A(3,J1)+FZ1(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
           FTHE(J1) = FTHE(J1) + PHI1(I)
           IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDN(J1) =  CONDN(J1) + CONDINT(I)*ABS(H1(I))
C
           J1=IX2(I)
           A(1,J1)=A(1,J1)+FX2(I)
           A(2,J1)=A(2,J1)+FY2(I)
           A(3,J1)=A(3,J1)+FZ2(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
           FTHE(J1) = FTHE(J1) + PHI2(I)
           IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDN(J1) =  CONDN(J1) + CONDINT(I)*ABS(H2(I))
C
           J1=IX3(I)
           A(1,J1)=A(1,J1)+FX3(I)
           A(2,J1)=A(2,J1)+FY3(I)
           A(3,J1)=A(3,J1)+FZ3(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
           FTHE(J1) = FTHE(J1) + PHI3(I)
           IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDN(J1) =  CONDN(J1) + CONDINT(I)*ABS(H3(I))
C
           J1=IX4(I)
           A(1,J1)=A(1,J1)+FX4(I)
           A(2,J1)=A(2,J1)+FY4(I)
           A(3,J1)=A(3,J1)+FZ4(I)
           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
           FTHE(J1) = FTHE(J1) + PHI4(I)
           IF(NODADT_THERM == 1.AND.IFORM > 0 ) CONDN(J1) =  CONDN(J1) + CONDINT(I)*ABS(H4(I))
         ENDDO
      ENDIF 
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
      IF(INTTH == 0 ) THEN
         DO I=1,JLT
           IF(HH(I)==ZERO)CYCLE
           IG=NSVG(I)
           IF(IG>0)THEN
             A(1,IG)=A(1,IG)-FXI(I)
             A(2,IG)=A(2,IG)-FYI(I)
             A(3,IG)=A(3,IG)-FZI(I)
             STIFN(IG) = STIFN(IG) + STIF(I)
           ELSE
             IG = -IG
             AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
             AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
             AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
             STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT) + STIF(I)
           ENDIF
         ENDDO
C       
      ELSE 
         DO I=1,JLT
           IF(HH(I)==ZERO)CYCLE
           IG=NSVG(I)
           IF(IG>0)THEN
             A(1,IG)=A(1,IG)-FXI(I)
             A(2,IG)=A(2,IG)-FYI(I)
             A(3,IG)=A(3,IG)-FZI(I)
             STIFN(IG) = STIFN(IG) + STIF(I)
             FTHE(IG)=FTHE(IG) + PHI(I)
             IF(NODADT_THERM == 1) CONDN(IG) =  CONDN(IG) + CONDINT(I)
           ELSE
             IG = -IG
             AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
             AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
             AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
             STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT) + STIF(I)
             FTHEFI(NIN)%P(IG+ISHIFT)= FTHEFI(NIN)%P(IG+ISHIFT) + PHI(I)
             IF(NODADT_THERM == 1) CONDNFI(NIN)%P(IG+ISHIFT)=CONDNFI(NIN)%P(IG+ISHIFT) + CONDINT(I)
           ENDIF
         ENDDO 
      ENDIF
C
      RETURN
      END

