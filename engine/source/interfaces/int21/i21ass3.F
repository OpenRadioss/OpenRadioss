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
Chd|  I21ASS3                       source/interfaces/int21/i21ass3.F
Chd|-- called by -----------
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I21ASS3(JLT   ,A      ,NIN    ,NOINT  ,FXN    ,
     2                  FYN    ,FZN    ,FXT    ,FYT    ,FZT    ,
     3                  IX1    ,IX2    ,IX3    ,IX4    ,NSVG   ,
     4                  FCONT  ,FNCONT ,FTCONT ,LB     ,LC     ,
     5                  ITRIA  ,STIFN  ,STIF   ,FSKYI  ,ISKY   ,
     6                  ISECIN ,NSTRF  ,SECFCUM,FTXSAV ,FTYSAV ,
     7                  FTZSAV ,CAND_N ,INTSTAMP,WEIGHT,MSR    ,
     8                  INTTH  ,PHI    ,FTHE   ,FTHESKYI,MXI   ,
     9                  MYI    ,MZI    ,STRI   ,NODGLOB,NCONT  ,
     A                  INDEXCONT,TAGCONT,CONDN,CONDINT,CONDNSKYI,
     B                  IFORM  ,PHI1   ,PHI2   ,PHI3  ,PHI4    ,
     C                  H1     ,H2     ,H3     ,H4    ,NISKYFI ,
     D                  MSRL   ,ITAB   ,H3D_DATA,NINSKID    ,
     E                  PRATIO ,NINTERSKID,PSKIDS,IFLAGLOADP,
     F                  TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE TRI7BOX
      USE H3D_MOD
      USE TRI7BOX
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NOINT, ISKY(*), ISECIN, NSTRF(*),NCONT,IFORM,NISKYFI,
     .        NINSKID,NINTERSKID,IFLAGLOADP
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), ITRIA(MVSIZ), CAND_N(*), WEIGHT(*),
     .        MSR(*), INTTH, NODGLOB(*),INDEXCONT(*),TAGCONT(*),MSRL(*),ITAB(*),
     .        TAGNCONT(NLOADP_HYD_INTER,NUMNOD),KLOADPINTER(NINTER+1),
     .        LOADPINTER((NINTER+1)*NLOADP_HYD),LOADP_HYD_INTER(NLOADP_HYD)
      my_real
     .   A(3,*), FCONT(3,*),FNCONT(3,*), FTCONT(3,*), STIFN(*),
     .   FSKYI(LSKYI,NFSKYI), SECFCUM(7,NUMNOD,NSECT),
     .   FXN(MVSIZ), FYN(MVSIZ), FZN(MVSIZ),
     .   FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ),
     .   STIF(MVSIZ), LB(MVSIZ), LC(MVSIZ),
     .   FTXSAV(*),  FTYSAV(*),  FTZSAV(*),
     .   PHI(*), FTHE(*), FTHESKYI(*),
     .   MXI(MVSIZ), MYI(MVSIZ), MZI(MVSIZ), STRI(MVSIZ),CONDN(*),
     .   CONDINT(MVSIZ),CONDNSKYI(LSKYI),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),PRATIO(MVSIZ),
     .   PSKIDS(NINTERSKID,*)
      TYPE(INTSTAMP_DATA) INTSTAMP
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J, JG , K0, NBINTER, K1S, K, NISKYL, IROT, I1,
     .        NISKYL1,NISKYL2, NISKYFIL, ND , N,PP ,PPL
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), 
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   H1(MVSIZ) , H2(MVSIZ) , H3(MVSIZ) , H4(MVSIZ), H0 
      DOUBLE PRECISION
     .   FX6(6,MVSIZ), FY6(6,MVSIZ), FZ6(6,MVSIZ), ST6(6,MVSIZ),
     .   FX, FY, FZ, STF, 
     .   MX6(6,MVSIZ), MY6(6,MVSIZ), MZ6(6,MVSIZ), STR6(6,MVSIZ),
     .   XX, YY, ZZ, MX, MY, MZ, STR
C---------------------------------
      DO I=1,JLT
       FXI(I)=FXN(I)+FXT(I)
       FYI(I)=FYN(I)+FYT(I)
       FZI(I)=FZN(I)+FZT(I)
      ENDDO
C---------------------------------
      DO I=1,JLT
        FTXSAV(CAND_N(I))=FXT(I)
        FTYSAV(CAND_N(I))=FYT(I)
        FTZSAV(CAND_N(I))=FZT(I)
      ENDDO
C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(IFLAGLOADP > 0) THEN
         DO I=1,JLT
            JG = NSVG(I)
            IF(WEIGHT(JG)/=1)CYCLE
            DO K = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
               PP = LOADPINTER(K)
               PPL = LOADP_HYD_INTER(PP)

               TAGNCONT(PPL,JG) = 1
 
               I1 =  IX1(I)
               ND = MSRL(I1)
               IF(ND.GT.0) THEN             
                  TAGNCONT(PPL,ND)=1
               ELSE
                 ND = -ND
                 TAGNCONTFI(NIN)%P(ND)= 1
               ENDIF

               I1 =  IX2(I)
               ND = MSRL(I1)
               IF(ND.GT.0) THEN             
                  TAGNCONT(PPL,ND)=1
               ELSE
                 ND = -ND
                 TAGNCONTFI(NIN)%P(ND)= 1
               ENDIF

               I1 =  IX3(I)
               ND = MSRL(I1)
               IF(ND.GT.0) THEN             
                  TAGNCONT(PPL,ND)=1
               ELSE
                 ND = -ND
                 TAGNCONTFI(NIN)%P(ND)= 1
               ENDIF

               I1 =  IX4(I)
               ND = MSRL(I1)
               IF(ND.GT.0) THEN             
                  TAGNCONT(PPL,ND)=1
               ELSE
                 ND = -ND
                 TAGNCONTFI(NIN)%P(ND)= 1
               ENDIF
            ENDDO
         ENDDO
      ENDIF

C---------------------------------
      IF(IPARIT.EQ.0)THEN
        DO I=1,JLT
          IG=NSVG(I)
          A(1,IG)=A(1,IG)-FXI(I)*WEIGHT(IG)
          A(2,IG)=A(2,IG)-FYI(I)*WEIGHT(IG)
          A(3,IG)=A(3,IG)-FZI(I)*WEIGHT(IG)
          STIFN(IG) = STIFN(IG) + STIF(I)*WEIGHT(IG)
        END DO
        IF(INTTH/=0)THEN
         IF(NODADT_THERM == 1 ) THEN  
           DO I=1,JLT
            IG=NSVG(I)
            FTHE(IG)=FTHE(IG)+PHI(I)*WEIGHT(IG)
            CONDN(IG)=CONDN(IG)+CONDINT(I)*WEIGHT(IG)
           END DO
         ELSE
           DO I=1,JLT
            IG=NSVG(I)
            FTHE(IG)=FTHE(IG)+PHI(I)*WEIGHT(IG)
           END DO
         ENDIF
C
         IF(IFORM==1) THEN
          DO I=1,JLT
            I1 =  IX1(I)
            ND = MSRL(I1)
            IG=NSVG(I)
            IF(ND.GT.0) THEN             
              FTHE(ND)=FTHE(ND) + PHI1(I)*WEIGHT(IG)
            ELSE
              ND = -ND
              FTHEFI(NIN)%P(ND)=FTHEFI(NIN)%P(ND) + PHI1(I)*WEIGHT(IG)
            ENDIF
c
            I1 =  IX2(I)
            ND = MSRL(I1)
            IF(ND.GT.0) THEN             
              FTHE(ND)=FTHE(ND) + PHI2(I)*WEIGHT(IG)
            ELSE
              ND = -ND
              FTHEFI(NIN)%P(ND)=FTHEFI(NIN)%P(ND) + PHI2(I)*WEIGHT(IG)
            ENDIF
c
            I1 =  IX3(I)
            ND = MSRL(I1)
            IF(ND.GT.0) THEN             
              FTHE(ND)=FTHE(ND) + PHI3(I)*WEIGHT(IG)
            ELSE
              ND = -ND
              FTHEFI(NIN)%P(ND)=FTHEFI(NIN)%P(ND) + PHI3(I)*WEIGHT(IG)
            ENDIF
c
            I1 =  IX4(I)
            ND = MSRL(I1)
            IF(ND.GT.0) THEN             
              FTHE(ND)=FTHE(ND) + PHI4(I)*WEIGHT(IG)
            ELSE
              ND = -ND
              FTHEFI(NIN)%P(ND)=FTHEFI(NIN)%P(ND) + PHI4(I)*WEIGHT(IG)
            ENDIF
c
          ENDDO
         ENDIF

        END IF

C
        FX =ZERO
        FY =ZERO
        FZ =ZERO
        STF=ZERO
        DO I=1,JLT
          IG=NSVG(I)
          FX=FX+FXI(I)   *WEIGHT(IG)
          FY=FY+FYI(I)   *WEIGHT(IG)
          FZ=FZ+FZI(I)   *WEIGHT(IG)
          STF=STF+STIF(I)*WEIGHT(IG)
        END DO
#include "lockon.inc"
        INTSTAMP%FC(1)=INTSTAMP%FC(1)+FX
        INTSTAMP%FC(2)=INTSTAMP%FC(2)+FY
        INTSTAMP%FC(3)=INTSTAMP%FC(3)+FZ
        INTSTAMP%STF  =INTSTAMP%STF  +STF
#include "lockoff.inc"
        IROT=INTSTAMP%IROT
        IF(IROT/=0)THEN
          MX =ZERO
          MY =ZERO
          MZ =ZERO
          STR=ZERO
          DO I=1,JLT
            IG=NSVG(I)
            MX=MX+MXI(I) *WEIGHT(IG)
            MY=MY+MYI(I) *WEIGHT(IG)
            MZ=MZ+MZI(I) *WEIGHT(IG)
            STR=STR+STRI(I)*WEIGHT(IG)
          END DO
#include "lockon.inc"
          INTSTAMP%MC(1)=INTSTAMP%MC(1)+MX
          INTSTAMP%MC(2)=INTSTAMP%MC(2)+MY
          INTSTAMP%MC(3)=INTSTAMP%MC(3)+MZ
          INTSTAMP%STR  =INTSTAMP%STR  +STR
#include "lockoff.inc"
        END IF
      ELSE
C
C Precalcul impact locaux / remote
C
        NISKYL1 = 0
        NISKYL2 = 0
        IF(IFORM /= 0) THEN
         DO I = 1, JLT
            IF (H1(I).NE.ZERO) THEN
               I1 =  IX1(I)
               ND = MSRL(I1)
               IF(ND.GT.0)  THEN                  
                 NISKYL1 = NISKYL1 + 1
               ELSE
                 NISKYL2 = NISKYL2 + 1
               ENDIF
            ENDIF
            IF (H2(I).NE.ZERO) THEN
               I1 =  IX2(I)
               ND = MSRL(I1)
               IF(ND.GT.0)  THEN                  
                 NISKYL1 = NISKYL1 + 1
               ELSE
                 NISKYL2 = NISKYL2 + 1
               ENDIF
            ENDIF
            IF (H3(I).NE.ZERO) THEN
               I1 =  IX3(I)
               ND = MSRL(I1)
               IF(ND.GT.0)  THEN                  
                 NISKYL1 = NISKYL1 + 1
               ELSE
                 NISKYL2 = NISKYL2 + 1
               ENDIF
            ENDIF
            IF (H4(I).NE.ZERO) THEN
               I1 =  IX4(I)
               ND = MSRL(I1)
               IF(ND.GT.0)  THEN                  
                 NISKYL1 = NISKYL1 + 1
               ELSE
                 NISKYL2 = NISKYL2 + 1
               ENDIF
            ENDIF
         ENDDO
        ENDIF
C

#include "lockon.inc"
        NISKYL = NISKY
        NISKY = NISKY + JLT + NISKYL1
        IF(IFORM /= 0) THEN
          NISKYFIL = NISKYFI
          NISKYFI = NISKYFI + NISKYL2
        ENDIF
#include "lockoff.inc"
        IF(INTTH==0)THEN
          DO I=1,JLT
            NISKYL = NISKYL + 1
            IG=NSVG(I)
            FSKYI(NISKYL,1)=-FXI(I)*WEIGHT(IG)
            FSKYI(NISKYL,2)=-FYI(I)*WEIGHT(IG)
            FSKYI(NISKYL,3)=-FZI(I)*WEIGHT(IG)
            FSKYI(NISKYL,4)=STIF(I)*WEIGHT(IG)
            ISKY(NISKYL) = IG
          END DO
        ELSE
         IF(NODADT_THERM == 1 ) THEN 
           DO I=1,JLT
            NISKYL = NISKYL + 1
            IG=NSVG(I)
            FSKYI(NISKYL,1)=-FXI(I)*WEIGHT(IG)
            FSKYI(NISKYL,2)=-FYI(I)*WEIGHT(IG)
            FSKYI(NISKYL,3)=-FZI(I)*WEIGHT(IG)
            FSKYI(NISKYL,4)=STIF(I)*WEIGHT(IG)
            FTHESKYI(NISKYL)=PHI(I)*WEIGHT(IG)
            CONDNSKYI(NISKYL)=CONDINT(I)*WEIGHT(IG)
            ISKY(NISKYL) = IG
           END DO
         ELSE
           DO I=1,JLT
            NISKYL = NISKYL + 1
            IG=NSVG(I)
            FSKYI(NISKYL,1)=-FXI(I)*WEIGHT(IG)
            FSKYI(NISKYL,2)=-FYI(I)*WEIGHT(IG)
            FSKYI(NISKYL,3)=-FZI(I)*WEIGHT(IG)
            FSKYI(NISKYL,4)=STIF(I)*WEIGHT(IG)
            FTHESKYI(NISKYL)=PHI(I)*WEIGHT(IG)
            ISKY(NISKYL) = IG
           END DO
         ENDIF
         IF(IFORM==1) THEN
           IF(NODADT_THERM == 1 ) THEN 
C
             DO I=1,JLT
                IG=NSVG(I)
                IF (H1(I).NE.ZERO) THEN
                  I1 =  IX1(I)
                  ND = MSRL(I1)
                  IF(ND.GT.0) THEN             
                    NISKYL = NISKYL + 1
                    FSKYI(NISKYL,1)=ZERO
                    FSKYI(NISKYL,2)=ZERO
                    FSKYI(NISKYL,3)=ZERO
                    FSKYI(NISKYL,4)=ZERO
                    CONDNSKYI(NISKYL)=ZERO
                    FTHESKYI(NISKYL)=PHI1(I)*WEIGHT(IG)
                    ISKY(NISKYL) = ND
                 ELSE
                    ND = -ND
                    NISKYFIL = NISKYFIL + 1
                    FTHESKYFI(NIN)%P(NISKYFIL)=PHI1(I)*WEIGHT(IG)
                    ISKYFI(NIN)%P(NISKYFIL) = ND
                 ENDIF
               ENDIF
c
               IF (H2(I).NE.ZERO) THEN
                 I1 =  IX2(I)
                 ND = MSRL(I1)
                IF(ND.GT.0) THEN             
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=ZERO
                  FSKYI(NISKYL,2)=ZERO
                  FSKYI(NISKYL,3)=ZERO
                  FSKYI(NISKYL,4)=ZERO
                  CONDNSKYI(NISKYL)=ZERO
                  FTHESKYI(NISKYL)=PHI2(I)*WEIGHT(IG)
                  ISKY(NISKYL) = ND
                ELSE
                  ND = -ND
                  NISKYFIL = NISKYFIL + 1
                  FTHESKYFI(NIN)%P(NISKYFIL)=PHI2(I)*WEIGHT(IG)
                  ISKYFI(NIN)%P(NISKYFIL) = ND
               ENDIF 
              ENDIF
c
              IF (H3(I).NE.ZERO) THEN
                I1 =  IX3(I)
                ND = MSRL(I1)
                IF(ND.GT.0) THEN             
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=ZERO
                  FSKYI(NISKYL,2)=ZERO
                  FSKYI(NISKYL,3)=ZERO
                  FSKYI(NISKYL,4)=ZERO
                  CONDNSKYI(NISKYL)=ZERO
                  FTHESKYI(NISKYL)=PHI3(I)*WEIGHT(IG)
                  ISKY(NISKYL) = ND
                ELSE
                  ND = -ND
                  NISKYFIL = NISKYFIL + 1
                  FTHESKYFI(NIN)%P(NISKYFIL)=PHI3(I)*WEIGHT(IG)
                  ISKYFI(NIN)%P(NISKYFIL) = ND
                ENDIF
              ENDIF
c
              IF (H4(I).NE.ZERO) THEN
                I1 =  IX4(I)
                ND = MSRL(I1)
                IF(ND.GT.0) THEN             
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=ZERO
                  FSKYI(NISKYL,2)=ZERO
                  FSKYI(NISKYL,3)=ZERO
                  FSKYI(NISKYL,4)=ZERO
                  CONDNSKYI(NISKYL)=ZERO
                  FTHESKYI(NISKYL)=PHI4(I)*WEIGHT(IG)
                  ISKY(NISKYL) = ND
                ELSE
                  ND = -ND
                  NISKYFIL = NISKYFIL + 1
                  FTHESKYFI(NIN)%P(NISKYFIL)=PHI4(I)*WEIGHT(IG)
                  ISKYFI(NIN)%P(NISKYFIL) = ND
               ENDIF
             ENDIF
            ENDDO
C 
           ELSE ! NODADT_THERM
C
             DO I=1,JLT
                IG=NSVG(I)
                IF (H1(I).NE.ZERO) THEN
                  I1 =  IX1(I)
                  ND = MSRL(I1)
                  IF(ND.GT.0) THEN             
                    NISKYL = NISKYL + 1
                    FSKYI(NISKYL,1)=ZERO
                    FSKYI(NISKYL,2)=ZERO
                    FSKYI(NISKYL,3)=ZERO
                    FSKYI(NISKYL,4)=ZERO
                    FTHESKYI(NISKYL)=PHI1(I)*WEIGHT(IG)
                    ISKY(NISKYL) = ND
                 ELSE
                    ND = -ND
                    NISKYFIL = NISKYFIL + 1
                    FTHESKYFI(NIN)%P(NISKYFIL)=PHI1(I)*WEIGHT(IG)
                    ISKYFI(NIN)%P(NISKYFIL) = ND
                 ENDIF
               ENDIF
c
               IF (H2(I).NE.ZERO) THEN
                 I1 =  IX2(I)
                 ND = MSRL(I1)
                IF(ND.GT.0) THEN             
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=ZERO
                  FSKYI(NISKYL,2)=ZERO
                  FSKYI(NISKYL,3)=ZERO
                  FSKYI(NISKYL,4)=ZERO
                  FTHESKYI(NISKYL)=PHI2(I)*WEIGHT(IG)
                  ISKY(NISKYL) = ND
                ELSE
                  ND = -ND
                  NISKYFIL = NISKYFIL + 1
                  FTHESKYFI(NIN)%P(NISKYFIL)=PHI2(I)*WEIGHT(IG)
                  ISKYFI(NIN)%P(NISKYFIL) = ND
               ENDIF 
              ENDIF
c
              IF (H3(I).NE.ZERO) THEN
                I1 =  IX3(I)
                ND = MSRL(I1)
                IF(ND.GT.0) THEN             
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=ZERO
                  FSKYI(NISKYL,2)=ZERO
                  FSKYI(NISKYL,3)=ZERO
                  FSKYI(NISKYL,4)=ZERO
                  FTHESKYI(NISKYL)=PHI3(I)*WEIGHT(IG)
                  ISKY(NISKYL) = ND
                ELSE
                  ND = -ND
                  NISKYFIL = NISKYFIL + 1
                  FTHESKYFI(NIN)%P(NISKYFIL)=PHI3(I)*WEIGHT(IG)
                  ISKYFI(NIN)%P(NISKYFIL) = ND
                ENDIF
              ENDIF
c
              IF (H4(I).NE.ZERO) THEN
                I1 =  IX4(I)
                ND = MSRL(I1)
                IF(ND.GT.0) THEN             
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=ZERO
                  FSKYI(NISKYL,2)=ZERO
                  FSKYI(NISKYL,3)=ZERO
                  FSKYI(NISKYL,4)=ZERO
                  FTHESKYI(NISKYL)=PHI4(I)*WEIGHT(IG)
                  ISKY(NISKYL) = ND
                ELSE
                  ND = -ND
                  NISKYFIL = NISKYFIL + 1
                  FTHESKYFI(NIN)%P(NISKYFIL)=PHI4(I)*WEIGHT(IG)
                  ISKYFI(NIN)%P(NISKYFIL) = ND
               ENDIF
             ENDIF
C 
            ENDDO
          ENDIF

         ENDIF
        END IF

        CALL FOAT_TO_6_FLOAT(1,JLT,FXI,FX6)
        CALL FOAT_TO_6_FLOAT(1,JLT,FYI,FY6)
        CALL FOAT_TO_6_FLOAT(1,JLT,FZI,FZ6)
        CALL FOAT_TO_6_FLOAT(1,JLT,STIF,ST6)
        DO K=1,6
          FX =ZERO
          FY =ZERO
          FZ =ZERO
          STF=ZERO
          DO I=1,JLT
          IG=NSVG(I)
          FX =FX +FX6(K,I)*WEIGHT(IG)
          FY =FY +FY6(K,I)*WEIGHT(IG)
          FZ =FZ +FZ6(K,I)*WEIGHT(IG)
          STF=STF+ST6(K,I)*WEIGHT(IG)
          ENDDO
#include "lockon.inc"
          INTSTAMP%FC6(K,1)=INTSTAMP%FC6(K,1)+FX
          INTSTAMP%FC6(K,2)=INTSTAMP%FC6(K,2)+FY
          INTSTAMP%FC6(K,3)=INTSTAMP%FC6(K,3)+FZ
          INTSTAMP%ST6(K)  =INTSTAMP%ST6(K)  +STF
#include "lockoff.inc"
        ENDDO
        IROT=INTSTAMP%IROT
        IF(IROT/=0)THEN
          CALL FOAT_TO_6_FLOAT(1,JLT,MXI,MX6)
          CALL FOAT_TO_6_FLOAT(1,JLT,MYI,MY6)
          CALL FOAT_TO_6_FLOAT(1,JLT,MZI,MZ6)
          CALL FOAT_TO_6_FLOAT(1,JLT,STRI,STR6)
          DO K=1,6
            MX =ZERO
            MY =ZERO
            MZ =ZERO
            STR=ZERO
            DO I=1,JLT
            IG=NSVG(I)
            MX =MX +MX6(K,I)*WEIGHT(IG)
            MY =MY +MY6(K,I)*WEIGHT(IG)
            MZ =MZ +MZ6(K,I)*WEIGHT(IG)
            STR=STR+STR6(K,I)*WEIGHT(IG)
            ENDDO
#include "lockon.inc"
            INTSTAMP%MC6(K,1)=INTSTAMP%MC6(K,1)+MX
            INTSTAMP%MC6(K,2)=INTSTAMP%MC6(K,2)+MY
            INTSTAMP%MC6(K,3)=INTSTAMP%MC6(K,3)+MZ
            INTSTAMP%STR6(K) =INTSTAMP%STR6(K) +STR
#include "lockoff.inc"
          END DO
        END IF
      ENDIF

      IF(NINSKID > 0)THEN
C-------------SKID LINES OUTPUTTING ----------------
#include "lockon.inc"
         DO I=1,JLT
          JG = NSVG(I)
          IF(WEIGHT(JG)/=1)CYCLE
          N = NODGLOB(JG)
          PSKIDS(NINSKID,N)=MAX(PSKIDS(NINSKID,N),PRATIO(I))

          N= IX1(I)
          PSKIDS(NINSKID,N)=MAX(PSKIDS(NINSKID,N),PRATIO(I))
          N= IX2(I)
          PSKIDS(NINSKID,N)=MAX(PSKIDS(NINSKID,N),PRATIO(I))
          N= IX3(I)
          PSKIDS(NINSKID,N)=MAX(PSKIDS(NINSKID,N),PRATIO(I))
          N= IX4(I)
          PSKIDS(NINSKID,N)=MAX(PSKIDS(NINSKID,N),PRATIO(I))

         ENDDO
#include "lockoff.inc"

C---------------------------------
      IF(.NOT.(    (ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0 .AND.
     .                  (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .                  (MANIM.GE.4.AND.MANIM.LE.15) .OR. H3D_DATA%MH3D .NE. 0))
     .         .OR.(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .                  (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .                  (MANIM.GE.4.AND.MANIM.LE.15) .OR. H3D_DATA%MH3D .NE. 0))
     .         .OR.ISECIN.NE.0)  ) RETURN
C--------------------------------- 
      DO I=1,JLT
        IF(IX3(I).NE.IX4(I))THEN
          H0   =QUART*(UN - LB(I) - LC(I))
          IF(ABS(ITRIA(I))==1)THEN
            H1(I)= LB(I)+H0
            H2(I)= LC(I)+H0
            H3(I)= H0
            H4(I)= H0
          ELSEIF(ABS(ITRIA(I))==2)THEN
            H1(I)= H0
            H2(I)= LB(I)+H0
            H3(I)= LC(I)+H0
            H4(I)= H0
          ELSEIF(ABS(ITRIA(I))==3)THEN
            H1(I)= H0
            H2(I)= H0
            H3(I)= LB(I)+H0
            H4(I)= LC(I)+H0
          ELSEIF(ABS(ITRIA(I))==4)THEN
            H1(I)= LC(I)+H0
            H2(I)= H0
            H3(I)= H0
            H4(I)= LB(I)+H0
          END IF
        ELSE
          H1(I) = LB(I)
          H2(I) = LC(I)
          H3(I) = UN - LB(I) - LC(I)
          H4(I) = ZERO
        END IF
      END DO
C---------------------------------
      DO I=1,JLT
        IX1(I)=MSR(IX1(I))
        IX2(I)=MSR(IX2(I))
        IX3(I)=MSR(IX3(I))
        IX4(I)=MSR(IX4(I))
      END DO
C-------------Tag of nodes really impacting contact forces updating ----------------
       IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT .GT.0.OR.
     .        ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0)THEN
#include "lockon.inc"
         DO I=1,JLT
          JG = NSVG(I)
          IF(WEIGHT(JG)/=1)CYCLE
          IG = NODGLOB(JG)
C
          IF(TAGCONT(IG)==0) THEN 
             NCONT= NCONT+1     
             INDEXCONT(NCONT) = IG
             TAGCONT(IG)= 1 
          ENDIF
          IF(TAGCONT(IX1(I))==0) THEN 
             NCONT= NCONT+1     
             INDEXCONT(NCONT) = IX1(I) 
             TAGCONT(IX1(I))= 1 
          ENDIF
          IF(TAGCONT(IX2(I))==0) THEN 
             NCONT= NCONT+1     
             INDEXCONT(NCONT) = IX2(I) 
             TAGCONT(IX2(I))= 1 
          ENDIF
          IF(TAGCONT(IX3(I))==0) THEN 
             NCONT= NCONT+1     
             INDEXCONT(NCONT) = IX3(I)
             TAGCONT(IX3(I))= 1  
          ENDIF
          IF(TAGCONT(IX4(I))==0) THEN 
             NCONT= NCONT+1     
             INDEXCONT(NCONT) = IX4(I) 
             TAGCONT(IX4(I))= 1 
          ENDIF    
         ENDDO
#include "lockoff.inc"
        ENDIF

C---------------------------------
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .        (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .            (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
         DO I=1,JLT
          JG = NSVG(I)
          IF(WEIGHT(JG)/=1)CYCLE
          FNCONT(1,NODGLOB(JG))=FNCONT(1,NODGLOB(JG))- FXN(I)
          FNCONT(2,NODGLOB(JG))=FNCONT(2,NODGLOB(JG))- FYN(I)
          FNCONT(3,NODGLOB(JG))=FNCONT(3,NODGLOB(JG))- FZN(I)
C
          FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FXN(I)*H1(I)
          FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FYN(I)*H1(I)
          FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZN(I)*H1(I)
          FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FXN(I)*H2(I)
          FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FYN(I)*H2(I)
          FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZN(I)*H2(I)
          FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FXN(I)*H3(I)
          FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FYN(I)*H3(I)
          FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZN(I)*H3(I)
          FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FXN(I)*H4(I)
          FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FYN(I)*H4(I)
          FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZN(I)*H4(I)
         ENDDO
#include "lockoff.inc"
      ENDIF
C---------------------------------
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .        (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .            (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
         DO I=1,JLT
          JG = NSVG(I)
          IF(WEIGHT(JG)/=1)CYCLE
          FTCONT(1,NODGLOB(JG))=FTCONT(1,NODGLOB(JG))- FXT(I)
          FTCONT(2,NODGLOB(JG))=FTCONT(2,NODGLOB(JG))- FYT(I)
          FTCONT(3,NODGLOB(JG))=FTCONT(3,NODGLOB(JG))- FZT(I)
C
          FTCONT(1,IX1(I)) =FTCONT(1,IX1(I)) + FXT(I)*H1(I)
          FTCONT(2,IX1(I)) =FTCONT(2,IX1(I)) + FYT(I)*H1(I)
          FTCONT(3,IX1(I)) =FTCONT(3,IX1(I)) + FZT(I)*H1(I)
          FTCONT(1,IX2(I)) =FTCONT(1,IX2(I)) + FXT(I)*H2(I)
          FTCONT(2,IX2(I)) =FTCONT(2,IX2(I)) + FYT(I)*H2(I)
          FTCONT(3,IX2(I)) =FTCONT(3,IX2(I)) + FZT(I)*H2(I)
          FTCONT(1,IX3(I)) =FTCONT(1,IX3(I)) + FXT(I)*H3(I)
          FTCONT(2,IX3(I)) =FTCONT(2,IX3(I)) + FYT(I)*H3(I)
          FTCONT(3,IX3(I)) =FTCONT(3,IX3(I)) + FZT(I)*H3(I)
          FTCONT(1,IX4(I)) =FTCONT(1,IX4(I)) + FXT(I)*H4(I)
          FTCONT(2,IX4(I)) =FTCONT(2,IX4(I)) + FYT(I)*H4(I)
          FTCONT(3,IX4(I)) =FTCONT(3,IX4(I)) + FZT(I)*H4(I)
         ENDDO
#include "lockoff.inc"
      ENDIF

C-----------------------------------------------
      DO I=1,JLT
C
        FX1(I)=FXI(I)*H1(I)
        FY1(I)=FYI(I)*H1(I)
        FZ1(I)=FZI(I)*H1(I)
C
        FX2(I)=FXI(I)*H2(I)
        FY2(I)=FYI(I)*H2(I)
        FZ2(I)=FZI(I)*H2(I)
C
        FX3(I)=FXI(I)*H3(I)
        FY3(I)=FYI(I)*H3(I)
        FZ3(I)=FZI(I)*H3(I)
C
        FX4(I)=FXI(I)*H4(I)
        FY4(I)=FYI(I)*H4(I)
        FZ4(I)=FZI(I)*H4(I)
C
      ENDDO
C-----------------------------------------------
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0.AND.
     .        (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .            (MANIM.GE.4.AND.MANIM.LE.15) .OR. H3D_DATA%MH3D .NE. 0 ))THEN
#include "lockon.inc"
         DO I=1,JLT
          JG = NSVG(I)
          IF(WEIGHT(JG)/=1)CYCLE
          FCONT(1,NODGLOB(JG))=FCONT(1,NODGLOB(JG))- FXI(I)
          FCONT(2,NODGLOB(JG))=FCONT(2,NODGLOB(JG))- FYI(I)
          FCONT(3,NODGLOB(JG))=FCONT(3,NODGLOB(JG))- FZI(I)
C
          FCONT(1,IX1(I)) =FCONT(1,IX1(I)) + FX1(I)
          FCONT(2,IX1(I)) =FCONT(2,IX1(I)) + FY1(I)
          FCONT(3,IX1(I)) =FCONT(3,IX1(I)) + FZ1(I)
          FCONT(1,IX2(I)) =FCONT(1,IX2(I)) + FX2(I)
          FCONT(2,IX2(I)) =FCONT(2,IX2(I)) + FY2(I)
          FCONT(3,IX2(I)) =FCONT(3,IX2(I)) + FZ2(I)
          FCONT(1,IX3(I)) =FCONT(1,IX3(I)) + FX3(I)
          FCONT(2,IX3(I)) =FCONT(2,IX3(I)) + FY3(I)
          FCONT(3,IX3(I)) =FCONT(3,IX3(I)) + FZ3(I)
          FCONT(1,IX4(I)) =FCONT(1,IX4(I)) + FX4(I)
          FCONT(2,IX4(I)) =FCONT(2,IX4(I)) + FY4(I)
          FCONT(3,IX4(I)) =FCONT(3,IX4(I)) + FZ4(I)
         ENDDO
#include "lockoff.inc"
      ENDIF

C-----------------------------------------------
C code NSPMD > 1 a traiter de maniere specifique (IXi non present localement)
      IF(NSPMD == 1)THEN
        IF(ISECIN.GT.0)THEN
         K0=NSTRF(25)
         IF(NSTRF(1)+NSTRF(2).NE.0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S).EQ.NOINT)THEN
              IF(ISECUT.NE.0)THEN
#include "lockon.inc"
                DO K=1,JLT
C attention aux signes pour le cumul des efforts
C a rendre conforme avec CFORC3
                 IF(SECFCUM(4,IX1(K),I).EQ.1.)THEN
                  SECFCUM(1,IX1(K),I)=SECFCUM(1,IX1(K),I)-FX1(K)
                  SECFCUM(2,IX1(K),I)=SECFCUM(2,IX1(K),I)-FY1(K)
                  SECFCUM(3,IX1(K),I)=SECFCUM(3,IX1(K),I)-FZ1(K)
                 ENDIF
                 IF(SECFCUM(4,IX2(K),I).EQ.1.)THEN
                  SECFCUM(1,IX2(K),I)=SECFCUM(1,IX2(K),I)-FX2(K)
                  SECFCUM(2,IX2(K),I)=SECFCUM(2,IX2(K),I)-FY2(K)
                  SECFCUM(3,IX2(K),I)=SECFCUM(3,IX2(K),I)-FZ2(K)
                 ENDIF
                 IF(SECFCUM(4,IX3(K),I).EQ.1.)THEN
                  SECFCUM(1,IX3(K),I)=SECFCUM(1,IX3(K),I)-FX3(K)
                  SECFCUM(2,IX3(K),I)=SECFCUM(2,IX3(K),I)-FY3(K)
                  SECFCUM(3,IX3(K),I)=SECFCUM(3,IX3(K),I)-FZ3(K)
                 ENDIF
                 IF(SECFCUM(4,IX4(K),I).EQ.1.)THEN
                  SECFCUM(1,IX4(K),I)=SECFCUM(1,IX4(K),I)-FX4(K)
                  SECFCUM(2,IX4(K),I)=SECFCUM(2,IX4(K),I)-FY4(K)
                  SECFCUM(3,IX4(K),I)=SECFCUM(3,IX4(K),I)-FZ4(K)
                 ENDIF
                 JG = NSVG(K)
                 IF(SECFCUM(4,JG,I).EQ.1.)THEN
                  SECFCUM(1,JG,I)=SECFCUM(1,JG,I)+FXI(K)
                  SECFCUM(2,JG,I)=SECFCUM(2,JG,I)+FYI(K)
                  SECFCUM(3,JG,I)=SECFCUM(3,JG,I)+FZI(K)
                 ENDIF
                ENDDO
#include "lockoff.inc"
              ENDIF
C +fsav(section)
            ENDIF
            K1S=K1S+1
           ENDDO
           K0=NSTRF(K0+24)
          ENDDO
         ENDIF
        ENDIF
      ELSE
C       NSPMD > 1
      ENDIF 
C

      ENDIF
      RETURN
      END
