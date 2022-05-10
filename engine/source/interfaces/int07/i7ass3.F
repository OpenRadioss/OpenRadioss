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
Chd|  I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        I7ASS0                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS05                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS25                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS3                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS35                       source/interfaces/int07/i7ass3.F
Chd|        I7ASSIGEO0                    source/interfaces/int07/i7ass3.F
Chd|        I7SMS2                        source/interfaces/int07/i7sms2.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7ASS33(JLT       ,A        ,NOINT     ,ITAB      ,STIFN    ,
     2                  STIF      ,FSKYI    ,ISKY      ,FCONT     ,IX1       ,
     3                  IX2       ,IX3      ,IX4       ,NSVG      ,NELTST    ,
     4                  ITYPTST   ,DT2T     ,NISKYFI   ,ISECIN    ,NSTRF     ,
     5                  SECFCUM   ,VISCN    ,NIN       ,FXI       ,FYI       , 
     6                  FZI       ,FX1      ,FY1       ,FZ1       ,FX2       ,
     7                  FY2       ,FZ2      ,FX3       ,FY3       ,FZ3       ,
     8                  FX4       ,FY4      ,FZ4       ,H1        ,H2        ,
     9                  H3        ,H4       ,KS        ,KT        ,K1        ,
     A                  K2        ,K3       ,K4        ,CS        ,CF        ,
     B                  C1        ,C2       ,C3        ,C4        ,C         ,
     C                  INTTH     ,PHI      ,PHI1      ,PHI2      ,PHI3      ,
     D                  PHI4      ,FTHE     ,FTHESKYI  ,MSKYI_SMS ,ISKYI_SMS ,
     E                  NSMS      ,DTMINI    ,JTASK     ,
     F                  CONDN     ,CONDINT   ,CONDNSKYI,IXIG3D   ,KXIG3D    ,
     J                  WIGE      ,KNOT      ,IGEO      ,NIGE     ,RIGE      ,
     K                  X         ,H3D_DATA  ,KNOTLOCPC ,KNOTLOCEL,IFORM    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE H3D_MOD
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
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,NIN,NOINT,ISECIN,JTASK,NISKYFI,INTTH,IFORM
      
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),ITAB(*), ISKY(*), NSTRF(*),
     .        NISUB,ISKYI_SMS(*), NSMS(*),
     .        KXIG3D(NIXIG3D,*),IXIG3D(*),IGEO(NPROPGI,*),NIGE(*)
      my_real
     .   X(3,*), A(3,*), FCONT(3,*),DT2T,DTMINI,
     .   STIFN(*), FSKYI(LSKYI,NFSKYI), 
     .   MSKYI_SMS(*)
      my_real
     .     STIF(MVSIZ),SECFCUM(7,NUMNOD,NSECT),
     .     VISCN(*),PHI(MVSIZ), FTHE(*),FTHESKYI(LSKYI),
     .     PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ) ,
     .     CONDINT(MVSIZ),CONDN(*),CONDNSKYI(LSKYI) 
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), 
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ)
      my_real
     .   WIGE(*),KNOT(*),RIGE(3,*),KNOTLOCPC(*),KNOTLOCEL(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,JG,K0,NBINTER,K1S,J,IG,IBCM,IBCS,TAGIGEO
      my_real
     .   DTI     
C-------------------------------------------------------------------------------
C
      IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
        DTI=DT2T
        CALL I7SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3              NIN   ,NOINT ,MSKYI_SMS, ISKYI_SMS,NSMS  ,
     4              KT    ,C     ,CF   ,DTMINI,DTI )
        IF(DTI.LT.DT2T)THEN
          DT2T    = DTI
          NELTST  = NOINT
          ITYPTST = 10
        ENDIF
      ENDIF
C
      IF(IDTMINS_INT/=0)THEN
        STIF(1:JLT)=ZERO
      END IF
C
C-----Isogeometric elements     
      TAGIGEO=0
c
      IF(IPARIT.EQ.3)THEN
       IF(KDTINT.EQ.0)THEN
        CALL I7ASS3(JLT  ,IX1  ,IX2  ,IX3  ,IX4  ,
     2             NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3             FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4             FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5             FXI  ,FYI  ,FZI  ,A    ,STIFN)
       ELSE
        CALL I7ASS35(JLT  ,IX1  ,IX2  ,IX3  ,IX4  ,
     2                  NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3                  FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4                  FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5                  FXI  ,FYI  ,FZI  ,A    ,STIFN,VISCN,
     6                  KS   ,K1   ,K2   ,K3   ,K4   ,CS   ,
     7                  C1   ,C2   ,C3   ,C4   )
       ENDIF
      ELSEIF(IPARIT.EQ.0)THEN
       IF(KDTINT.EQ.0)THEN
        DO I=1,JLT
         IF(IX1(I).GE.NUMNOD.AND.IX2(I).GE.NUMNOD.AND. 
     .      IX3(I).GE.NUMNOD.AND.IX4(I).GE.NUMNOD)THEN
            TAGIGEO=TAGIGEO+1 
         ENDIF
        ENDDO
        IF(TAGIGEO.EQ.0) THEN
         CALL I7ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2               NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3               FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4               FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5               FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6               INTTH ,PHI  ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7               PHI4  ,CONDN,CONDINT,JTASK,IFORM)
        ELSE
         CALL I7ASSIGEO0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2               NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3               FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4               FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5               FXI   ,FYI  ,FZI  ,A    ,STIFN  ,IXIG3D,
     6               KXIG3D,X    ,WIGE ,KNOT ,IGEO   ,NIGE, 
     7               RIGE  ,FCONT,H3D_DATA,KNOTLOCPC,KNOTLOCEL)
        ENDIF
       ELSE
C
         CALL I7ASS05(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                FXI   ,FYI  ,FZI  ,A    ,STIFN  ,VISCN ,
     6                KS    ,K1   ,K2   ,K3   ,K4     ,CS    ,
     7                C1    ,C2   ,C3   ,C4   ,NIN    ,INTTH ,
     8                PHI   ,FTHE ,PHI1 , PHI2  ,PHI3 , PHI4 ,
     9                JTASK,CONDN,CONDINT,IFORM)
       ENDIF
C
      ELSE
        IF(KDTINT.EQ.0)THEN
         IF(TAGIGEO.EQ.0) THEN
          CALL I7ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2                NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3                FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4                FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5                FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6                NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI ,PHI1,
     7                PHI2  ,PHI3 , PHI4 ,CONDNSKYI,CONDINT,
     A                IFORM )  
         ELSE
c         CALL I7ASSIGEO2(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
c     2               NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
c     3               FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
c     4               FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
c     5               FXI   ,FYI  ,FZI  ,FSKYI,ISKY ,NISKYFI,
c     6               NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI ,PHI1,
c     7               PHI2  ,PHI3 , PHI4 ,CONDNSKYI,CONDINT,IXIG3D,
c     6               KXIG3D,X    ,WIGE ,KNOT ,IGEO   ,NIGE,
c     7               RIGE  ,FCONT,H3D_DATA,KNOTLOCPC,KNOTLOCEL)
         ENDIF   
        ELSE
          CALL I7ASS25(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                 NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                 FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                 FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                 FXI   ,FYI  ,FZI  ,FSKYI,NISKYFI,NIN  ,
     6                 KS    ,K1   ,K2   ,K3   ,K4     ,CS   ,
     7                 C1    ,C2   ,C3   ,C4   ,ISKY  ,NOINT ,
     8                 INTTH ,PHI  ,FTHESKYI  ,PHI1   ,PHI2  , PHI3, 
     9                  PHI4 ,CONDNSKYI,CONDINT,IFORM)
        ENDIF
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT .GT.0)THEN
         IF (INCONV.EQ.1) THEN
#include "lockon.inc"
           DO I=1,JLT
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
            JG = NSVG(I)
            IF(JG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              FCONT(1,JG)=FCONT(1,JG)- FXI(I)
              FCONT(2,JG)=FCONT(2,JG)- FYI(I)
              FCONT(3,JG)=FCONT(3,JG)- FZI(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV.EQ.1) THEN
      ENDIF
C-----------------------------------------------------
      IF(ISECIN.GT.0.AND.INCONV.EQ.1)THEN
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
                IF(JG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
                  IF(SECFCUM(4,JG,I).EQ.1.)THEN
                    SECFCUM(1,JG,I)=SECFCUM(1,JG,I)+FXI(K)
                    SECFCUM(2,JG,I)=SECFCUM(2,JG,I)+FYI(K)
                    SECFCUM(3,JG,I)=SECFCUM(3,JG,I)+FZI(K)
                  ENDIF
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
C-----------------------------------------------------   
      RETURN
      END
Chd|====================================================================
Chd|  I7ASS3                        source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I22FOR3                       source/interfaces/int22/i22for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        DOUBLE_FLOT_IEEE              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE I7ASS3(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                  NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                  FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                  FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                  FXI   ,FYI  ,FZI  ,I8A  ,I8STIFN      )
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
      INTEGER JLT
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      integer*8 I8A(3,3,*), I8STIFN(3,*)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer*8 
     .  I8STIF(3,MVSIZ),I8FX(3,MVSIZ),I8FY(3,MVSIZ),I8FZ(3,MVSIZ)
      INTEGER I,J1
      my_real
     .    STIF1(MVSIZ),STIF2(MVSIZ),STIF3(MVSIZ),STIF4(MVSIZ)
C
      DO I=1 ,JLT
        STIF1(I) = STIF(I)*ABS(H1(I))
        STIF2(I) = STIF(I)*ABS(H2(I))
        STIF3(I) = STIF(I)*ABS(H3(I))
        STIF4(I) = STIF(I)*ABS(H4(I))
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX1,FX1,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY1,FY1,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ1,FZ1,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF1,STIF1,I8STIF)
C     
      DO I=1 ,JLT
        J1=IX1(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX2,FX2,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY2,FY2,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ2,FZ2,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF2,STIF2,I8STIF)
C     
      DO I=1 ,JLT
        J1=IX2(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX3,FX3,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY3,FY3,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ3,FZ3,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF3,STIF3,I8STIF)
C     
      DO I=1 ,JLT
        J1=IX3(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX4,FX4,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY4,FY4,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ4,FZ4,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF4,STIF4,I8STIF)
C     
      DO I=1 ,JLT
        J1=IX4(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FXI,FXI,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FYI,FYI,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZI,FZI,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF,STIF,I8STIF)
C     
      DO I=1 ,JLT
        J1=NSVG(I)
        I8A(1,1,J1)=I8A(1,1,J1) - I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) - I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) - I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) - I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) - I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) - I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) - I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) - I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) - I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I7ASS35                       source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        DOUBLE_FLOT_IEEE              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE I7ASS35(JLT  ,IX1  ,IX2  ,IX3  ,IX4  ,
     2                  NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3                  FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4                  FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5                  FXI  ,FYI  ,FZI  ,I8A  ,I8STIFN ,I8VISCN,
     6                  KS   ,K1   ,K2   ,K3   ,K4   ,CS   ,
     7                  C1   ,C2   ,C3   ,C4   )
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
      INTEGER JLT
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      integer*8 I8A(3,3,*), I8STIFN(3,*), I8VISCN(3,*)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .    CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer*8 
     .  I8STIF(3,MVSIZ),I8FX(3,MVSIZ),I8FY(3,MVSIZ),I8FZ(3,MVSIZ),
     .  I8VISC(3,MVSIZ)
      INTEGER I,J1
      my_real
     .    STIF1(MVSIZ),STIF2(MVSIZ),STIF3(MVSIZ),STIF4(MVSIZ),
     .    VISC1(MVSIZ),VISC2(MVSIZ),VISC3(MVSIZ),VISC4(MVSIZ)
C
      DO I=1 ,JLT
        STIF1(I) = K1(I)
        STIF2(I) = K2(I)
        STIF3(I) = K3(I)
        STIF4(I) = K4(I)
        VISC1(I) = C1(I)
        VISC2(I) = C2(I)
        VISC3(I) = C3(I)
        VISC4(I) = C4(I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX1,FX1,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY1,FY1,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ1,FZ1,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF1,STIF1,I8STIF)
      CALL DOUBLE_FLOT_IEEE(1,JLT,VISC1,VISC1,I8VISC)
C     
      DO I=1 ,JLT
        J1=IX1(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
C
        I8VISCN(1,J1) = I8VISCN(1,J1) + I8VISC(1,I)
        I8VISCN(2,J1) = I8VISCN(2,J1) + I8VISC(2,I)
        I8VISCN(3,J1) = I8VISCN(3,J1) + I8VISC(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX2,FX2,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY2,FY2,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ2,FZ2,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF2,STIF2,I8STIF)
      CALL DOUBLE_FLOT_IEEE(1,JLT,VISC2,VISC2,I8VISC)
C     
      DO I=1 ,JLT
        J1=IX2(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
C
        I8VISCN(1,J1) = I8VISCN(1,J1) + I8VISC(1,I)
        I8VISCN(2,J1) = I8VISCN(2,J1) + I8VISC(2,I)
        I8VISCN(3,J1) = I8VISCN(3,J1) + I8VISC(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX3,FX3,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY3,FY3,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ3,FZ3,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF3,STIF3,I8STIF)
      CALL DOUBLE_FLOT_IEEE(1,JLT,VISC3,VISC3,I8VISC)
C     
      DO I=1 ,JLT
        J1=IX3(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
C
        I8VISCN(1,J1) = I8VISCN(1,J1) + I8VISC(1,I)
        I8VISCN(2,J1) = I8VISCN(2,J1) + I8VISC(2,I)
        I8VISCN(3,J1) = I8VISCN(3,J1) + I8VISC(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FX4,FX4,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FY4,FY4,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZ4,FZ4,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,STIF4,STIF4,I8STIF)
      CALL DOUBLE_FLOT_IEEE(1,JLT,VISC4,VISC4,I8VISC)
C     
      DO I=1 ,JLT
        J1=IX4(I)
        I8A(1,1,J1)=I8A(1,1,J1) + I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) + I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) + I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) + I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) + I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) + I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) + I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) + I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) + I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
C
        I8VISCN(1,J1) = I8VISCN(1,J1) + I8VISC(1,I)
        I8VISCN(2,J1) = I8VISCN(2,J1) + I8VISC(2,I)
        I8VISCN(3,J1) = I8VISCN(3,J1) + I8VISC(3,I)
      ENDDO
C
      CALL DOUBLE_FLOT_IEEE(1,JLT,FXI,FXI,I8FX)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FYI,FYI,I8FY)
      CALL DOUBLE_FLOT_IEEE(1,JLT,FZI,FZI,I8FZ)
      CALL DOUBLE_FLOT_IEEE(1,JLT,KS,KS,I8STIF)
      CALL DOUBLE_FLOT_IEEE(1,JLT,CS,CS,I8VISC)
C     
      DO I=1 ,JLT
        J1=NSVG(I)
        I8A(1,1,J1)=I8A(1,1,J1) - I8FX(1,I)
        I8A(2,1,J1)=I8A(2,1,J1) - I8FX(2,I)
        I8A(3,1,J1)=I8A(3,1,J1) - I8FX(3,I)
C
        I8A(1,2,J1)=I8A(1,2,J1) - I8FY(1,I)
        I8A(2,2,J1)=I8A(2,2,J1) - I8FY(2,I)
        I8A(3,2,J1)=I8A(3,2,J1) - I8FY(3,I)
C
        I8A(1,3,J1)=I8A(1,3,J1) - I8FZ(1,I)
        I8A(2,3,J1)=I8A(2,3,J1) - I8FZ(2,I)
        I8A(3,3,J1)=I8A(3,3,J1) - I8FZ(3,I)
C
        I8STIFN(1,J1) = I8STIFN(1,J1) + I8STIF(1,I)
        I8STIFN(2,J1) = I8STIFN(2,J1) + I8STIF(2,I)
        I8STIFN(3,J1) = I8STIFN(3,J1) + I8STIF(3,I)
C
        I8VISCN(1,J1) = I8VISCN(1,J1) + I8VISC(1,I)
        I8VISCN(2,J1) = I8VISCN(2,J1) + I8VISC(2,I)
        I8VISCN(3,J1) = I8VISCN(3,J1) + I8VISC(3,I)
      ENDDO
C
      RETURN
      END
C
Cel nouvelle routine
Chd|====================================================================
Chd|  I7ASS0                        source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                  NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                  FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                  FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                  FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6                  INTTH ,PHI  ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7                  PHI4  ,CONDN,CONDINT,JTASK,IFORM)
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
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN,INTTH,JTASK,IFORM,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    A(3,*),  STIFN(*),PHI(*), FTHE(*),
     .    PHI1(*), PHI2(*), PHI3(*), PHI4(*),
     .    CONDN(*),CONDINT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, ISHIFT, NODFI
C
      !---REACTION FORCE AGAINST LAGRANGIAN FACE
      !
      IF(INTTH == 0) THEN
          DO I=1,JLT
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
      ELSE
         IF(NODADT_THERM == 1.AND.IFORM > 0 ) THEN  
            DO I=1,JLT
               J1=IX1(I)
               A(1,J1)=A(1,J1)+FX1(I)
               A(2,J1)=A(2,J1)+FY1(I)
               A(3,J1)=A(3,J1)+FZ1(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
               FTHE(J1) = FTHE(J1) + PHI1(I)
               CONDN(J1) = CONDN(J1) + CONDINT(I)*ABS(H1(I))
C
               J1=IX2(I)
               A(1,J1)=A(1,J1)+FX2(I)
               A(2,J1)=A(2,J1)+FY2(I)
               A(3,J1)=A(3,J1)+FZ2(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
               FTHE(J1) = FTHE(J1) + PHI2(I)
               CONDN(J1) = CONDN(J1) + CONDINT(I)*ABS(H2(I))
C
               J1=IX3(I)
               A(1,J1)=A(1,J1)+FX3(I)
               A(2,J1)=A(2,J1)+FY3(I)
               A(3,J1)=A(3,J1)+FZ3(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
               FTHE(J1) = FTHE(J1) + PHI3(I)
               CONDN(J1) = CONDN(J1) + CONDINT(I)*ABS(H3(I))
C
               J1=IX4(I)
               A(1,J1)=A(1,J1)+FX4(I)
               A(2,J1)=A(2,J1)+FY4(I)
               A(3,J1)=A(3,J1)+FZ4(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
               FTHE(J1) = FTHE(J1) + PHI4(I)
               CONDN(J1) = CONDN(J1) + CONDINT(I)*ABS(H4(I))
            ENDDO
         ELSE
            DO I=1,JLT
               J1=IX1(I)
               A(1,J1)=A(1,J1)+FX1(I)
               A(2,J1)=A(2,J1)+FY1(I)
               A(3,J1)=A(3,J1)+FZ1(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
               FTHE(J1) = FTHE(J1) + PHI1(I)
C
               J1=IX2(I)
               A(1,J1)=A(1,J1)+FX2(I)
               A(2,J1)=A(2,J1)+FY2(I)
               A(3,J1)=A(3,J1)+FZ2(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
               FTHE(J1) = FTHE(J1) + PHI2(I)
C
               J1=IX3(I)
               A(1,J1)=A(1,J1)+FX3(I)
               A(2,J1)=A(2,J1)+FY3(I)
               A(3,J1)=A(3,J1)+FZ3(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
               FTHE(J1) = FTHE(J1) + PHI3(I)
C
               J1=IX4(I)
               A(1,J1)=A(1,J1)+FX4(I)
               A(2,J1)=A(2,J1)+FY4(I)
               A(3,J1)=A(3,J1)+FZ4(I)
               STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
               FTHE(J1) = FTHE(J1) + PHI4(I)
            ENDDO
         ENDIF  
      ENDIF 
C
      !---FLUID REACTION FORCE
      !
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
      IF(INTTH == 0 ) THEN
         DO I=1,JLT
           IG=NSVG(I)
           IF(IG.GT.0)THEN
             A(1,IG)=A(1,IG)-FXI(I)
             A(2,IG)=A(2,IG)-FYI(I)
             A(3,IG)=A(3,IG)-FZI(I)
             STIFN(IG) = STIFN(IG) + STIF(I)
           ELSE
             IG = -IG
             AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
             AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
             AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
             STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT)+STIF(I)
          ENDIF
         ENDDO
C       
      ELSE 
         IF(NODADT_THERM == 1 ) THEN  
          DO I=1,JLT
            IG=NSVG(I)
            IF(IG.GT.0)THEN
              A(1,IG)=A(1,IG)-FXI(I)
              A(2,IG)=A(2,IG)-FYI(I)
              A(3,IG)=A(3,IG)-FZI(I)
              STIFN(IG) = STIFN(IG) + STIF(I)
              FTHE(IG)=FTHE(IG) + PHI(I)
              CONDN(IG) = CONDN(IG) + CONDINT(I)
            ELSE
              IG = -IG
              AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
              AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
              AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
              STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT) + STIF(I)
              FTHEFI(NIN)%P(IG+ISHIFT)= FTHEFI(NIN)%P(IG+ISHIFT) + PHI(I)
              CONDNFI(NIN)%P(IG+ISHIFT)=CONDNFI(NIN)%P(IG+ISHIFT) + CONDINT(I)
            ENDIF
          ENDDO
         ELSE
          DO I=1,JLT
            IG=NSVG(I)
            IF(IG.GT.0)THEN
              A(1,IG)=A(1,IG)-FXI(I)
              A(2,IG)=A(2,IG)-FYI(I)
              A(3,IG)=A(3,IG)-FZI(I)
              STIFN(IG) = STIFN(IG) + STIF(I)
              FTHE(IG)=FTHE(IG) + PHI(I)
            ELSE
              IG = -IG
              AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
              AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
              AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
              STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT) + STIF(I)
              FTHEFI(NIN)%P(IG+ISHIFT)= FTHEFI(NIN)%P(IG+ISHIFT) + PHI(I)
            ENDIF
          ENDDO
         ENDIF 
      ENDIF    
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I7ASS05                       source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7ASS05(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                   NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                   FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                   FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                   FXI   ,FYI  ,FZI  ,A    ,STIFN  ,VISCN,
     6                   KS    ,K1   ,K2   ,K3   ,K4     ,CS   ,
     7                   C1    ,C2   ,C3   ,C4   ,NIN    ,INTTH ,
     8                   PHI   ,FTHE ,PHI1 , PHI2  ,PHI3 , PHI4 ,
     9                   JTASK ,CONDN,CONDINT,IFORM)
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
#include      "scr05_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN,INTTH ,JTASK,IFORM,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .    CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .    A(3,*), STIFN(*), VISCN(*),PHI(*),FTHE(*),
     ,    PHI1(*) , PHI2(*)  ,PHI3(*) , PHI4(*),CONDINT(*),CONDN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG,NODFI,ISHIFT
C
       IF(INTTH == 0) THEN
          DO I=1,JLT
            J1=IX1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1)= STIFN(J1)+K1(I)
            VISCN(J1)=VISCN(J1)+C1(I)
C
            J1=IX2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1)=STIFN(J1)+K2(I)
            VISCN(J1)=VISCN(J1)+C2(I)
C
            J1=IX3(I)
            A(1,J1)=A(1,J1)+FX3(I)
            A(2,J1)=A(2,J1)+FY3(I)
            A(3,J1)=A(3,J1)+FZ3(I)
            STIFN(J1)=STIFN(J1)+K3(I)
            VISCN(J1)=VISCN(J1)+C3(I)
C
            J1=IX4(I)
            A(1,J1)=A(1,J1)+FX4(I)
            A(2,J1)=A(2,J1)+FY4(I)
            A(3,J1)=A(3,J1)+FZ4(I)
            STIFN(J1)=STIFN(J1)+K4(I)
            VISCN(J1)=VISCN(J1)+C4(I)
          ENDDO
        ELSE
          IF(NODADT_THERM == 1 .AND.IFORM > 0) THEN
             DO I=1,JLT
                J1=IX1(I)
                A(1,J1)=A(1,J1)+FX1(I)
                A(2,J1)=A(2,J1)+FY1(I)
                A(3,J1)=A(3,J1)+FZ1(I)
                STIFN(J1)= STIFN(J1)+K1(I)
                VISCN(J1)=VISCN(J1)+C1(I)
                FTHE(J1)= FTHE(J1) +  PHI1(I)
                CONDN(J1)= CONDN(J1) +  CONDINT(I)*ABS(H1(I))
C
                J1=IX2(I)
                A(1,J1)=A(1,J1)+FX2(I)
                A(2,J1)=A(2,J1)+FY2(I)
                A(3,J1)=A(3,J1)+FZ2(I)
                STIFN(J1)=STIFN(J1)+K2(I)
                VISCN(J1)=VISCN(J1)+C2(I)
                FTHE(J1)= FTHE(J1) +  PHI2(I)
                CONDN(J1)= CONDN(J1) +  CONDINT(I)*ABS(H2(I))
C
                J1=IX3(I)
                A(1,J1)=A(1,J1)+FX3(I)
                A(2,J1)=A(2,J1)+FY3(I)
                A(3,J1)=A(3,J1)+FZ3(I)
                STIFN(J1)=STIFN(J1)+K3(I)
                VISCN(J1)=VISCN(J1)+C3(I)
                FTHE(J1)= FTHE(J1) +  PHI3(I)
                CONDN(J1)= CONDN(J1) +  CONDINT(I)*ABS(H3(I))
C
                J1=IX4(I)
                A(1,J1)=A(1,J1)+FX4(I)
                A(2,J1)=A(2,J1)+FY4(I)
                A(3,J1)=A(3,J1)+FZ4(I)
                STIFN(J1)=STIFN(J1)+K4(I)
                VISCN(J1)=VISCN(J1)+C4(I)
                FTHE(J1)= FTHE(J1) +  PHI4(I)
                CONDN(J1)= CONDN(J1) +  CONDINT(I)*ABS(H4(I))
             ENDDO 
           ELSE
             DO I=1,JLT
                J1=IX1(I)
                A(1,J1)=A(1,J1)+FX1(I)
                A(2,J1)=A(2,J1)+FY1(I)
                A(3,J1)=A(3,J1)+FZ1(I)
                STIFN(J1)= STIFN(J1)+K1(I)
                VISCN(J1)=VISCN(J1)+C1(I)
                FTHE(J1)= FTHE(J1) +  PHI1(I)
C
                J1=IX2(I)
                A(1,J1)=A(1,J1)+FX2(I)
                A(2,J1)=A(2,J1)+FY2(I)
                A(3,J1)=A(3,J1)+FZ2(I)
                STIFN(J1)=STIFN(J1)+K2(I)
                VISCN(J1)=VISCN(J1)+C2(I)
                FTHE(J1)= FTHE(J1) +  PHI2(I)
C
                J1=IX3(I)
                A(1,J1)=A(1,J1)+FX3(I)
                A(2,J1)=A(2,J1)+FY3(I)
                A(3,J1)=A(3,J1)+FZ3(I)
                STIFN(J1)=STIFN(J1)+K3(I)
                VISCN(J1)=VISCN(J1)+C3(I)
                FTHE(J1)= FTHE(J1) +  PHI3(I)
C
                J1=IX4(I)
                A(1,J1)=A(1,J1)+FX4(I)
                A(2,J1)=A(2,J1)+FY4(I)
                A(3,J1)=A(3,J1)+FZ4(I)
                STIFN(J1)=STIFN(J1)+K4(I)
                VISCN(J1)=VISCN(J1)+C4(I)
                FTHE(J1)= FTHE(J1) +  PHI4(I)
             ENDDO 

           ENDIF
        ENDIF  
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      IF(INTTH == 0) THEN     
        DO I=1,JLT
          IG=NSVG(I)
          IF(IG.GT.0)THEN
            A(1,IG)=A(1,IG)-FXI(I)
            A(2,IG)=A(2,IG)-FYI(I)
            A(3,IG)=A(3,IG)-FZI(I)
            STIFN(IG)=STIFN(IG)+KS(I)
            VISCN(IG)=VISCN(IG)+CS(I)
          ELSE
            IG = -IG
            AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
            AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
            AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
            STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT)+KS(I)
            VSCFI(NIN)%P(IG+ISHIFT)=VSCFI(NIN)%P(IG+ISHIFT)+CS(I)
          ENDIF
        ENDDO 
      ELSE
        DO I=1,JLT
          IG=NSVG(I)
          IF(IG.GT.0)THEN
            A(1,IG)=A(1,IG)-FXI(I)
            A(2,IG)=A(2,IG)-FYI(I)
            A(3,IG)=A(3,IG)-FZI(I)
            STIFN(IG)=STIFN(IG)+KS(I)
            VISCN(IG)=VISCN(IG)+CS(I)
            FTHE(IG)=FTHE(IG) + PHI(I)
          ELSE
            IG = -IG
            AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
            AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
            AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
            STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT)+KS(I)
            VSCFI(NIN)%P(IG+ISHIFT)=VSCFI(NIN)%P(IG+ISHIFT)+CS(I)
            FTHEFI(NIN)%P(IG+ISHIFT)=FTHEFI(NIN)%P(IG+ISHIFT) + PHI(I)
          ENDIF
        ENDDO 
      ENDIF
C
      RETURN
      END
C
Cel nouvelle routine
Chd|====================================================================
Chd|  I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  FX1   ,FY1   ,FZ1  ,FX2  ,FY2   ,FZ2    ,
     4                  FX3   ,FY3   ,FZ3  ,FX4  ,FY4   ,FZ4    ,
     5                  FXI   ,FYI   ,FZI  ,FSKYI,ISKY  ,NISKYFI,
     6                  NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI,PHI1,
     7                  PHI2  ,PHI3 , PHI4 ,CONDNSKYI,CONDINT,
     A                  IFORM )     
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "warn_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NISKYFI,NIN,NOINT,INTTH,IFORM,
     .        ISKY(*),
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    FSKYI(LSKYI,NFSKYI),FTHESKYI(LSKYI),PHI(MVSIZ),CONDINT(MVSIZ),
     .    PHI1(*),PHI2(*)  ,PHI3(*) ,PHI4(*),CONDNSKYI(LSKYI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, NISKYL1, NISKYL,IGP,IGM,IDR,NISKYFIL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF (H1(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H2(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H3(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H4(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
C
C Precalcul impact locaux / remote
C
      IGP = 0
      IGM = 0
      DO I=1,JLT
        IG =NSVG(I)
        IF(IG.GT.0) THEN
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
      !!debug
      !I0 = NISKYL+1
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP .GT. LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM .GT. NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      IF(INTTH == 0 ) THEN
        DO I=1,JLT
          IF (H1(I).NE.0.) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
            ISKY(NISKYL) = IX1(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H2(I).NE.ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
            ISKY(NISKYL) = IX2(I)  
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H3(I).NE.ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
            ISKY(NISKYL) = IX3(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H4(I).NE.ZERO) THEN
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
          IG =NSVG(I)
          IF(IG.GT.0) THEN
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
        IF(NODADT_THERM == 1 .AND.IFORM > 0) THEN
           DO I=1,JLT
              IF (H1(I).NE.0.) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX1(I)
                FSKYI(NISKYL,2)=FY1(I)
                FSKYI(NISKYL,3)=FZ1(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
                ISKY(NISKYL) = IX1(I)
                FTHESKYI(NISKYL) = PHI1(I)
                CONDNSKYI(NISKYL)=CONDINT(I)*ABS(H1(I))
              ENDIF
           ENDDO
           DO I=1,JLT
              IF (H2(I).NE.ZERO) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX2(I)
                FSKYI(NISKYL,2)=FY2(I)
                FSKYI(NISKYL,3)=FZ2(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
                ISKY(NISKYL) = IX2(I)
                FTHESKYI(NISKYL) = PHI2(I)     
                CONDNSKYI(NISKYL)=CONDINT(I)*ABS(H2(I))     
              ENDIF
           ENDDO
           DO I=1,JLT
              IF (H3(I).NE.ZERO) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX3(I)
                FSKYI(NISKYL,2)=FY3(I)
                FSKYI(NISKYL,3)=FZ3(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
                ISKY(NISKYL) = IX3(I)
                FTHESKYI(NISKYL) = PHI3(I)
                CONDNSKYI(NISKYL)=CONDINT(I)*ABS(H3(I))
              ENDIF
           ENDDO
           DO I=1,JLT
              IF (H4(I).NE.ZERO) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX4(I)
                FSKYI(NISKYL,2)=FY4(I)
                FSKYI(NISKYL,3)=FZ4(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
                ISKY(NISKYL) = IX4(I)
                FTHESKYI(NISKYL) = PHI4(I)
                CONDNSKYI(NISKYL)=CONDINT(I)*ABS(H4(I))
              ENDIF
           ENDDO  
        ELSE
           DO I=1,JLT
              IF (H1(I).NE.0.) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX1(I)
                FSKYI(NISKYL,2)=FY1(I)
                FSKYI(NISKYL,3)=FZ1(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
                ISKY(NISKYL) = IX1(I)
                FTHESKYI(NISKYL) = PHI1(I)
              ENDIF
           ENDDO
           DO I=1,JLT
              IF (H2(I).NE.ZERO) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX2(I)
                FSKYI(NISKYL,2)=FY2(I)
                FSKYI(NISKYL,3)=FZ2(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
                ISKY(NISKYL) = IX2(I)
               FTHESKYI(NISKYL) = PHI2(I)          
              ENDIF
           ENDDO
           DO I=1,JLT
              IF (H3(I).NE.ZERO) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX3(I)
                FSKYI(NISKYL,2)=FY3(I)
                FSKYI(NISKYL,3)=FZ3(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
                ISKY(NISKYL) = IX3(I)
                FTHESKYI(NISKYL) = PHI3(I)
              ENDIF
           ENDDO
           DO I=1,JLT
              IF (H4(I).NE.ZERO) THEN
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=FX4(I)
                FSKYI(NISKYL,2)=FY4(I)
                FSKYI(NISKYL,3)=FZ4(I)
                FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
                ISKY(NISKYL) = IX4(I)
                FTHESKYI(NISKYL) = PHI4(I)
              ENDIF
           ENDDO 
        ENDIF    
C
        IF(NODADT_THERM == 1) THEN
         DO I=1,JLT
           IG =NSVG(I)
           IF(IG.GT.0) THEN
             NISKYL = NISKYL + 1
             FSKYI(NISKYL,1)=-FXI(I)
             FSKYI(NISKYL,2)=-FYI(I)
             FSKYI(NISKYL,3)=-FZI(I)
             FSKYI(NISKYL,4)= STIF(I)
             ISKY(NISKYL) = IG
             FTHESKYI(NISKYL)=PHI(I)
             CONDNSKYI(NISKYL)=CONDINT(I)
           ELSE
             IG = -IG
             NISKYFIL = NISKYFIL + 1
             FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
             FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
             FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
             FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
             ISKYFI(NIN)%P(NISKYFIL) = IG
             FTHESKYFI(NIN)%P(NISKYFIL)=PHI(I)
             CONDNSKYFI(NIN)%P(NISKYFIL)=CONDINT(I)
           ENDIF  
         ENDDO
        ELSE
         DO I=1,JLT
          IG =NSVG(I)
          IF(IG.GT.0) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=-FXI(I)
            FSKYI(NISKYL,2)=-FYI(I)
            FSKYI(NISKYL,3)=-FZI(I)
            FSKYI(NISKYL,4)= STIF(I)
            ISKY(NISKYL) = IG
            FTHESKYI(NISKYL)=PHI(I)
          ELSE
            IG = -IG
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
            FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
            ISKYFI(NIN)%P(NISKYFIL) = IG
            FTHESKYFI(NIN)%P(NISKYFIL)=PHI(I)
          ENDIF  
         ENDDO 
        ENDIF             
      ENDIF
C
!#include "lockon.inc"
!      DO i = 1,jlt
!        WRITE(914,'(1X,I5,I5,3(1X,Z20))')I,NSVG(I),FXI(I),FYI(I),FZI(I)
!        WRITE(914,'(1X,I5,I5,3(1X,Z20))')I,IX1(I),FX1(I),FY1(I),FZ1(I)
!        WRITE(914,'(1X,I5,I5,3(1X,Z20))')I,IX2(I),FX2(I),FY2(I),FZ2(I)
!        WRITE(914,'(1X,I5,I5,3(1X,Z20))')I,IX3(I),FX3(I),FY3(I),FZ3(I)
!        WRITE(914,'(1X,I5,I5,3(1X,Z20))')I,IX4(I),FX4(I),FY4(I),FZ4(I)
!      enddo
!#include "lockoff.inc"
C    
      RETURN
      END
C
Cel nouvelle routine
Chd|====================================================================
Chd|  I7ASS25                       source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7ASS25(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                   NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                   FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2   ,
     4                   FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4   ,
     5                   FXI   ,FYI  ,FZI  ,FSKYI,NISKYFI,NIN   ,
     6                   KS    ,K1   ,K2   ,K3   ,K4     ,CS    ,
     7                   C1    ,C2   ,C3   ,C4   ,ISKY   ,NOINT ,
     8                   INTTH ,PHI  ,FTHESKYI   ,PHI1   ,PHI2  ,  
     9                   PHI3  ,PHI4 ,CONDNSKYI  ,CONDINT,IFORM )
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NISKYFI,NIN,INTTH,IFORM,
     .        ISKY(*),NOINT,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .    CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .    FSKYI(LSKYI,NFSKYI),FTHESKYI(LSKYI),PHI(MVSIZ),
     .    PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),
     .    CONDINT(MVSIZ),CONDNSKYI(LSKYI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, NISKYL1, NISKYL,IGP,IGM,IDR,NISKYFIL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF (H1(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H2(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H3(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H4(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
C
C Precalcul impact locaux / remote
C
      IGP = 0
      IGM = 0
      DO I=1,JLT
        IG =NSVG(I)
        IF(IG.GT.0) THEN
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
      IF (NISKYL+NISKYL1+IGP .GT. LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM .GT. NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
Cs
      IF(INTTH == 0 ) THEN
        DO I=1,JLT
          IF (H1(I).NE.0.) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=K1(I)
            FSKYI(NISKYL,5)=C1(I)
            ISKY(NISKYL) = IX1(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H2(I).NE.ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=K2(I)
            FSKYI(NISKYL,5)=C2(I)
            ISKY(NISKYL) = IX2(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H3(I).NE.ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=K3(I)
            FSKYI(NISKYL,5)=C3(I)
            ISKY(NISKYL) = IX3(I)        
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (H4(I).NE.ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX4(I)
            FSKYI(NISKYL,2)=FY4(I)
            FSKYI(NISKYL,3)=FZ4(I)
            FSKYI(NISKYL,4)=K4(I)
            FSKYI(NISKYL,5)=C4(I)
            ISKY(NISKYL) = IX4(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IG = NSVG(I)
          IF(IG.GT.0) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=-FXI(I)
            FSKYI(NISKYL,2)=-FYI(I)
            FSKYI(NISKYL,3)=-FZI(I)
            FSKYI(NISKYL,4)= KS(I)
            FSKYI(NISKYL,5)= CS(I)
            ISKY(NISKYL) = IG
          ELSE
            IG = -IG
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
            FSKYFI(NIN)%P(4,NISKYFIL)= KS(I)
            FSKYFI(NIN)%P(5,NISKYFIL)= CS(I)
            ISKYFI(NIN)%P(NISKYFIL) = IG           
          ENDIF  
        ENDDO               
C + la thermique 
      ELSE
        IF(NODADT_THERM == 1 .AND.IFORM > 0) THEN
          DO I=1,JLT
            IF (H1(I).NE.0.) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=FX1(I)
               FSKYI(NISKYL,2)=FY1(I)
               FSKYI(NISKYL,3)=FZ1(I)
               FSKYI(NISKYL,4)=K1(I)
               FSKYI(NISKYL,5)=C1(I)
               ISKY(NISKYL) = IX1(I)
               FTHESKYI(NISKYL) = PHI1(I)
               CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H1(I))
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H2(I).NE.ZERO) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=FX2(I)
               FSKYI(NISKYL,2)=FY2(I)
               FSKYI(NISKYL,3)=FZ2(I)
               FSKYI(NISKYL,4)=K2(I)
               FSKYI(NISKYL,5)=C2(I)
               ISKY(NISKYL) = IX2(I)
               FTHESKYI(NISKYL) = PHI2(I)
               CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H2(I))
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H3(I).NE.ZERO) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=FX3(I)
               FSKYI(NISKYL,2)=FY3(I)
               FSKYI(NISKYL,3)=FZ3(I)
               FSKYI(NISKYL,4)=K3(I)
               FSKYI(NISKYL,5)=C3(I)
               ISKY(NISKYL) = IX3(I)
               FTHESKYI(NISKYL) = PHI3(I)   
               CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H3(I))       
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H4(I).NE.ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX4(I)
              FSKYI(NISKYL,2)=FY4(I)
              FSKYI(NISKYL,3)=FZ4(I)
              FSKYI(NISKYL,4)=K4(I)
              FSKYI(NISKYL,5)=C4(I)
              ISKY(NISKYL) = IX4(I)
              FTHESKYI(NISKYL) = PHI4(I)
              CONDNSKYI(NISKYL) = CONDINT(I)*ABS(H4(I))
            ENDIF
          ENDDO
        ELSE
          DO I=1,JLT
            IF (H1(I).NE.0.) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=FX1(I)
               FSKYI(NISKYL,2)=FY1(I)
               FSKYI(NISKYL,3)=FZ1(I)
               FSKYI(NISKYL,4)=K1(I)
               FSKYI(NISKYL,5)=C1(I)
               ISKY(NISKYL) = IX1(I)
               FTHESKYI(NISKYL) = PHI1(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H2(I).NE.ZERO) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=FX2(I)
               FSKYI(NISKYL,2)=FY2(I)
               FSKYI(NISKYL,3)=FZ2(I)
               FSKYI(NISKYL,4)=K2(I)
               FSKYI(NISKYL,5)=C2(I)
               ISKY(NISKYL) = IX2(I)
               FTHESKYI(NISKYL) = PHI2(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H3(I).NE.ZERO) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=FX3(I)
               FSKYI(NISKYL,2)=FY3(I)
               FSKYI(NISKYL,3)=FZ3(I)
               FSKYI(NISKYL,4)=K3(I)
               FSKYI(NISKYL,5)=C3(I)
               ISKY(NISKYL) = IX3(I)
               FTHESKYI(NISKYL) = PHI3(I)          
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H4(I).NE.ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX4(I)
              FSKYI(NISKYL,2)=FY4(I)
              FSKYI(NISKYL,3)=FZ4(I)
              FSKYI(NISKYL,4)=K4(I)
              FSKYI(NISKYL,5)=C4(I)
              ISKY(NISKYL) = IX4(I)
              FTHESKYI(NISKYL) = PHI4(I)
            ENDIF
          ENDDO
        ENDIF
C
        IF(NODADT_THERM == 1 ) THEN
          DO I=1,JLT
             IG = NSVG(I)
             IF(IG.GT.0) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=-FXI(I)
               FSKYI(NISKYL,2)=-FYI(I)
               FSKYI(NISKYL,3)=-FZI(I)
               FSKYI(NISKYL,4)= KS(I)
               FSKYI(NISKYL,5)= CS(I)
               ISKY(NISKYL) = IG
               FTHESKYI(NISKYL)=PHI(I)
               CONDNSKYI(NISKYL)=CONDINT(I)
             ELSE
               IG = -IG
               NISKYFIL = NISKYFIL + 1
               FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
               FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
               FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
               FSKYFI(NIN)%P(4,NISKYFIL)= KS(I)
               FSKYFI(NIN)%P(5,NISKYFIL)= CS(I)
               ISKYFI(NIN)%P(NISKYFIL) = IG
               FTHESKYFI(NIN)%P(NISKYFIL) =PHI(I) 
               CONDNSKYFI(NIN)%P(NISKYFIL) =CONDINT(I)           
             ENDIF  
         ENDDO 
        ELSE
          DO I=1,JLT
             IG = NSVG(I)
             IF(IG.GT.0) THEN
               NISKYL = NISKYL + 1
               FSKYI(NISKYL,1)=-FXI(I)
               FSKYI(NISKYL,2)=-FYI(I)
               FSKYI(NISKYL,3)=-FZI(I)
               FSKYI(NISKYL,4)= KS(I)
               FSKYI(NISKYL,5)= CS(I)
               ISKY(NISKYL) = IG
               FTHESKYI(NISKYL)=PHI(I)
             ELSE
               IG = -IG
               NISKYFIL = NISKYFIL + 1
               FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
               FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
               FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
               FSKYFI(NIN)%P(4,NISKYFIL)= KS(I)
               FSKYFI(NIN)%P(5,NISKYFIL)= CS(I)
               ISKYFI(NIN)%P(NISKYFIL) = IG
               FTHESKYFI(NIN)%P(NISKYFIL) =PHI(I)            
             ENDIF  
         ENDDO 
        ENDIF
      ENDIF
C
      RETURN
      END
Cel nouvelle routine
Chd|====================================================================
Chd|  I7ASSIGEO0                    source/interfaces/int07/i7ass3.F
Chd|-- called by -----------
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|-- calls ---------------
Chd|        IG3DONEBASIS                  source/elements/ige3d/ig3donebasis.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7ASSIGEO0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                  NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                  FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                  FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                  FXI   ,FYI  ,FZI  ,A    ,STIFN  ,IXIG3D,
     6                  KXIG3D,X    ,WIGE ,KNOT ,IGEO   ,NIGE ,
     7                  RIGE  ,FCONT,H3D_DATA,KNOTLOCPC,KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
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
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "scr18_c.inc"
#include      "com04_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .        NSVG(MVSIZ),KXIG3D(NIXIG3D,*),IXIG3D(*),
     .        IGEO(NPROPGI,*),NIGE(*)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    A(3,*),  STIFN(*), FCONT(3,*)
      my_real
     .    X(3,*),WIGE(*),KNOT(*),RIGE(3,*),
     .    KNOTLOCPC(DEG_MAX,3,*),KNOTLOCEL(2,3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JS, IPID, IAD_KNOT, NKNOT1,
     .        NKNOT2, NKNOT3, IDX, IDY, IDZ, N1, N2, N3,
     .        NCTRL, IERROR, IG, NUMCP(64), K, IDX2, IDY2, IDZ2,
     .        IDFRSTLOCKNT,IDPC,PX,PY,PZ, J1
      my_real
     .   ZR,ZS,ZT
      my_real
     .   X_IGEO(64),Y_IGEO(64),Z_IGEO(64),
     .   W_IGEO(64),R(64),
     .   FX(MVSIZ),FY(MVSIZ),FZ(MVSIZ),KNOTLOCELX(2,JLT),
     .   KNOTLOCELY(2,JLT),KNOTLOCELZ(2,JLT)
c     .   KNOTLOCX(PX+1,NCTRL,JLT),
c     .   KNOTLOCY(PY+1,NCTRL,JLT),KNOTLOCZ(PZ+1,NCTRL,JLT)
      my_real, DIMENSION(:,:), ALLOCATABLE :: KNOTLOCX, KNOTLOCY, KNOTLOCZ
C======================================================================|

C-------------------------------------
C     ASSEMBLAGE FORCES - COTE MAITRE
C-------------------------------------

      DO I=1,JLT

      IF(IX1(I).LE.NUMNOD) THEN
        J1=IX1(I)
        A(1,J1)=A(1,J1)+FX1(I)
        A(2,J1)=A(2,J1)+FY1(I)
        A(3,J1)=A(3,J1)+FZ1(I)
        STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
        CYCLE
      ENDIF

      JS=NIGE(IX1(I)-NUMNOD)
c      print*,'JS master ', JS, KXIG3D(2,JS)
      IPID=KXIG3D(2,JS)
      NCTRL = KXIG3D(3,JS)
      IAD_KNOT = IGEO(40,IPID)
      PX = IGEO(41,IPID)
      PY = IGEO(42,IPID)
      PZ = IGEO(43,IPID)
      N1 = IGEO(44,IPID)
      N2 = IGEO(45,IPID)
      N3 = IGEO(46,IPID)
      IDFRSTLOCKNT = IGEO(47,IPID)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
      IDX = KXIG3D(6,JS)
      IDY = KXIG3D(7,JS)
      IDZ = KXIG3D(8,JS)
      IDX2 = KXIG3D(9,JS)
      IDY2 = KXIG3D(10,JS)
      IDZ2 = KXIG3D(11,JS)

C   PAREIL POUR NCTRL

      DO J=1,NCTRL
        K = IXIG3D(KXIG3D(4,JS)+J-1)
        NUMCP(J) =K 
        X_IGEO(J)=X(1,K)
        Y_IGEO(J)=X(2,K)
        Z_IGEO(J)=X(3,K)
        W_IGEO(J)=1!WIGE(K)
      ENDDO

      ALLOCATE(KNOTLOCX(PX+1,NCTRL),KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL))

      DO J=1,NCTRL 
        DO K=1,PX+1
          KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PY+1
          KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PZ+1
          KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
      ENDDO

      KNOTLOCELX(1,I) = KNOTLOCEL(1,1,JS)
      KNOTLOCELY(1,I) = KNOTLOCEL(1,2,JS)
      KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,JS)
      KNOTLOCELX(2,I) = KNOTLOCEL(2,1,JS)
      KNOTLOCELY(2,I) = KNOTLOCEL(2,2,JS)
      KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,JS)

      ZR = RIGE(1,IX1(I)-NUMNOD)
      ZS = RIGE(2,IX1(I)-NUMNOD)
      ZT = RIGE(3,IX1(I)-NUMNOD)

c      CALL IGE3DBASIS(
c     1     JS        ,0   ,X_IGEO(:)    ,Y_IGEO(:)  ,
c     2     Z_IGEO(:)    ,W_IGEO(:)     ,IDX     ,IDY     ,
c     3     IDZ       ,R         ,
c     4     NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5     KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6     KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7     PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,0        ,X_IGEO(:)  ,Y_IGEO(:),
     2      Z_IGEO(:),W_IGEO(:)    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2   ,IDY2       ,IDZ2   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))

      DO J=1,NCTRL
        K = NUMCP(J)
        IF(ANIM_V(4)+H3D_DATA%N_VECT_CONT .GT.0)THEN
        FCONT(1,K) = FCONT(1,K) + R(J)*FX1(I)
        FCONT(2,K) = FCONT(2,K) + R(J)*FY1(I)
        FCONT(3,K) = FCONT(3,K) + R(J)*FZ1(I)
        ENDIF
        A(1,K) = A(1,K) + R(J)*FX1(I)
        A(2,K) = A(2,K) + R(J)*FY1(I)
        A(3,K) = A(3,K) + R(J)*FZ1(I)
        STIFN(K) = STIFN(K) + STIF(I)*ABS(H1(I))*R(J)
      ENDDO

      DEALLOCATE(KNOTLOCX,KNOTLOCY,KNOTLOCZ)

      ENDDO

      DO I=1,JLT

      IF(IX2(I).LE.NUMNOD) THEN
        J1=IX2(I)
        A(1,J1)=A(1,J1)+FX2(I)
        A(2,J1)=A(2,J1)+FY2(I)
        A(3,J1)=A(3,J1)+FZ2(I)
        STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
        CYCLE
      ENDIF

      JS=NIGE(IX2(I)-NUMNOD)
      IPID=KXIG3D(2,JS)
      NCTRL = KXIG3D(3,JS)
      IAD_KNOT = IGEO(40,IPID)
      PX = IGEO(41,IPID)
      PY = IGEO(42,IPID)
      PZ = IGEO(43,IPID)
      N1 = IGEO(44,IPID)
      N2 = IGEO(45,IPID)
      N3 = IGEO(46,IPID)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
      IDX = KXIG3D(6,JS)
      IDY = KXIG3D(7,JS)
      IDZ = KXIG3D(8,JS)

      DO J=1,NCTRL
        K = IXIG3D(KXIG3D(4,JS)+J-1)
        NUMCP(J) =K 
        X_IGEO(J)=X(1,K)
        Y_IGEO(J)=X(2,K)
        Z_IGEO(J)=X(3,K)
        W_IGEO(J)=1!WIGE(K)
      ENDDO

      ALLOCATE(KNOTLOCX(PX+1,NCTRL),KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL))

      DO J=1,NCTRL 
        DO K=1,PX+1
          KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PY+1
          KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PZ+1
          KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
      ENDDO

      KNOTLOCELX(1,I) = KNOTLOCEL(1,1,JS)
      KNOTLOCELY(1,I) = KNOTLOCEL(1,2,JS)
      KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,JS)
      KNOTLOCELX(2,I) = KNOTLOCEL(2,1,JS)
      KNOTLOCELY(2,I) = KNOTLOCEL(2,2,JS)
      KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,JS)

      ZR = RIGE(1,IX2(I)-NUMNOD)
      ZS = RIGE(2,IX2(I)-NUMNOD)
      ZT = RIGE(3,IX2(I)-NUMNOD)

c      CALL IGE3DBASIS(
c     1     JS        ,0   ,X_IGEO(:)    ,Y_IGEO(:)  ,
c     2     Z_IGEO(:)    ,W_IGEO(:)     ,IDX     ,IDY     ,
c     3     IDZ       ,R         ,
c     4     NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5     KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6     KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7     PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,0        ,X_IGEO(:)  ,Y_IGEO(:),
     2      Z_IGEO(:),W_IGEO(:)    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2   ,IDY2       ,IDZ2   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))
 
      DO J=1,NCTRL
        K = NUMCP(J)
        IF(ANIM_V(4)+H3D_DATA%N_VECT_CONT .GT.0)THEN
        FCONT(1,K) = FCONT(1,K) + R(J)*FX2(I)
        FCONT(2,K) = FCONT(2,K) + R(J)*FY2(I)
        FCONT(3,K) = FCONT(3,K) + R(J)*FZ2(I)
        ENDIF
        A(1,K) = A(1,K) + R(J)*FX2(I)
        A(2,K) = A(2,K) + R(J)*FY2(I)
        A(3,K) = A(3,K) + R(J)*FZ2(I)
        STIFN(K) = STIFN(K) + STIF(I)*ABS(H2(I))*R(J)
      ENDDO

      DEALLOCATE(KNOTLOCX,KNOTLOCY,KNOTLOCZ)

      ENDDO

      DO I=1,JLT

      IF(IX3(I).LE.NUMNOD) THEN
        J1=IX3(I)
        A(1,J1)=A(1,J1)+FX3(I)
        A(2,J1)=A(2,J1)+FY3(I)
        A(3,J1)=A(3,J1)+FZ3(I)
        STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
        CYCLE
      ENDIF

      JS=NIGE(IX3(I)-NUMNOD)
      IPID=KXIG3D(2,JS)
      NCTRL = KXIG3D(3,JS)
      IAD_KNOT = IGEO(40,IPID)
      PX = IGEO(41,IPID)
      PY = IGEO(42,IPID)
      PZ = IGEO(43,IPID)
      N1 = IGEO(44,IPID)
      N2 = IGEO(45,IPID)
      N3 = IGEO(46,IPID)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
      IDX = KXIG3D(6,JS)
      IDY = KXIG3D(7,JS)
      IDZ = KXIG3D(8,JS)

      DO J=1,NCTRL
        K = IXIG3D(KXIG3D(4,JS)+J-1)
        NUMCP(J) =K 
        X_IGEO(J)=X(1,K)
        Y_IGEO(J)=X(2,K)
        Z_IGEO(J)=X(3,K)
        W_IGEO(J)=1!WIGE(K)
      ENDDO

      ALLOCATE(KNOTLOCX(PX+1,NCTRL),KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL))

      DO J=1,NCTRL 
        DO K=1,PX+1
          KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PY+1
          KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PZ+1
          KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
      ENDDO

      KNOTLOCELX(1,I) = KNOTLOCEL(1,1,JS)
      KNOTLOCELY(1,I) = KNOTLOCEL(1,2,JS)
      KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,JS)
      KNOTLOCELX(2,I) = KNOTLOCEL(2,1,JS)
      KNOTLOCELY(2,I) = KNOTLOCEL(2,2,JS)
      KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,JS)

      ZR = RIGE(1,IX3(I)-NUMNOD)
      ZS = RIGE(2,IX3(I)-NUMNOD)
      ZT = RIGE(3,IX3(I)-NUMNOD)

c      CALL IGE3DBASIS(
c     1     JS        ,0   ,X_IGEO(:)    ,Y_IGEO(:)  ,
c     2     Z_IGEO(:)    ,W_IGEO(:)     ,IDX     ,IDY     ,
c     3     IDZ       ,R         ,
c     4     NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5     KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6     KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7     PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,0        ,X_IGEO(:)  ,Y_IGEO(:),
     2      Z_IGEO(:),W_IGEO(:)    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2   ,IDY2       ,IDZ2   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))
 
      DO J=1,NCTRL
        K = NUMCP(J)
        IF(ANIM_V(4)+H3D_DATA%N_VECT_CONT .GT.0)THEN
        FCONT(1,K) = FCONT(1,K) + R(J)*FX3(I)
        FCONT(2,K) = FCONT(2,K) + R(J)*FY3(I)
        FCONT(3,K) = FCONT(3,K) + R(J)*FZ3(I)
        ENDIF
        A(1,K) = A(1,K) + R(J)*FX3(I)
        A(2,K) = A(2,K) + R(J)*FY3(I)
        A(3,K) = A(3,K) + R(J)*FZ3(I)
        STIFN(K) = STIFN(K) + STIF(I)*ABS(H3(I))*R(J)
      ENDDO

      DEALLOCATE(KNOTLOCX,KNOTLOCY,KNOTLOCZ)

      ENDDO

      DO I=1,JLT

      IF(IX4(I).LE.NUMNOD) THEN
        J1=IX4(I)
        A(1,J1)=A(1,J1)+FX4(I)
        A(2,J1)=A(2,J1)+FY4(I)
        A(3,J1)=A(3,J1)+FZ4(I)
        STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
        CYCLE
      ENDIF

      JS=NIGE(IX4(I)-NUMNOD)
      IPID=KXIG3D(2,JS)
      NCTRL = KXIG3D(3,JS)
      IAD_KNOT = IGEO(40,IPID)
      PX = IGEO(41,IPID)
      PY = IGEO(42,IPID)
      PZ = IGEO(43,IPID)
      N1 = IGEO(44,IPID)
      N2 = IGEO(45,IPID)
      N3 = IGEO(46,IPID)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
      IDX = KXIG3D(6,JS)
      IDY = KXIG3D(7,JS)
      IDZ = KXIG3D(8,JS)

      DO J=1,NCTRL
        K = IXIG3D(KXIG3D(4,JS)+J-1)
        NUMCP(J) =K 
        X_IGEO(J)=X(1,K)
        Y_IGEO(J)=X(2,K)
        Z_IGEO(J)=X(3,K)
        W_IGEO(J)=1!WIGE(K)
      ENDDO

      ALLOCATE(KNOTLOCX(PX+1,NCTRL),KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL))

      DO J=1,NCTRL 
        DO K=1,PX+1
          KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PY+1
          KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PZ+1
          KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
      ENDDO

      KNOTLOCELX(1,I) = KNOTLOCEL(1,1,JS)
      KNOTLOCELY(1,I) = KNOTLOCEL(1,2,JS)
      KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,JS)
      KNOTLOCELX(2,I) = KNOTLOCEL(2,1,JS)
      KNOTLOCELY(2,I) = KNOTLOCEL(2,2,JS)
      KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,JS)

      ZR = RIGE(1,IX4(I)-NUMNOD)
      ZS = RIGE(2,IX4(I)-NUMNOD)
      ZT = RIGE(3,IX4(I)-NUMNOD)

c      CALL IGE3DBASIS(
c     1     JS        ,0   ,X_IGEO(:)    ,Y_IGEO(:)  ,
c     2     Z_IGEO(:)    ,W_IGEO(:)     ,IDX     ,IDY     ,
c     3     IDZ       ,R         ,
c     4     NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5     KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6     KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7     PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,0        ,X_IGEO(:)  ,Y_IGEO(:),
     2      Z_IGEO(:),W_IGEO(:)    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2   ,IDY2       ,IDZ2   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))
 
      DO J=1,NCTRL
        K = NUMCP(J)
        IF(ANIM_V(4)+H3D_DATA%N_VECT_CONT .GT.0)THEN
         FCONT(1,K) = FCONT(1,K) + R(J)*FX4(I)
         FCONT(2,K) = FCONT(2,K) + R(J)*FY4(I)
         FCONT(3,K) = FCONT(3,K) + R(J)*FZ4(I)
        ENDIF
c        print*,'R', R(J)
c        print*,'A(J)',A(1,K),A(2,K),A(3,K)
        A(1,K) = A(1,K) + R(J)*FX4(I)
        A(2,K) = A(2,K) + R(J)*FY4(I)
        A(3,K) = A(3,K) + R(J)*FZ4(I)
c      print*,'F', FX4(I),FY4(I),FZ4(I)
c        print*,'A(J)',A(1,K),A(2,K),A(3,K)
        STIFN(K) = STIFN(K) + STIF(I)*ABS(H4(I))*R(J)
      ENDDO

      DEALLOCATE(KNOTLOCX,KNOTLOCY,KNOTLOCZ)
C
      ENDDO

C-------------------------------------
C     ASSEMBLAGE FORCES - COTE ESCLAVE
C-------------------------------------

      FX(:) = 0
      FY(:) = 0
      FZ(:) = 0

      DO I=1,JLT
       IG=NSVG(I)
       IF(IG.LE.NUMNOD)THEN 
        A(1,IG)=A(1,IG)-FXI(I)
        A(2,IG)=A(2,IG)-FYI(I)
        A(3,IG)=A(3,IG)-FZI(I)
        STIFN(IG) = STIFN(IG) + STIF(I)
       ENDIF
      ENDDO

      DO I=1,JLT
       IG=NSVG(I)
       IF(NSVG(I).LE.NUMNOD) CYCLE
        JS=NIGE(IG-NUMNOD)
        NCTRL = KXIG3D(3,JS)
        DO J=1,NCTRL
          K = IXIG3D(KXIG3D(4,JS)+J-1)  !! PAS BON SI UN SEMGENT CONTIENT DES POINTS QUI NE SONT PAS SUR LE MEME ELEMENT
          NUMCP(J) =K 
          X_IGEO(J)=X(1,K)
          Y_IGEO(J)=X(2,K)
          Z_IGEO(J)=X(3,K)
          W_IGEO(J)=1!WIGE(K)
        ENDDO
        IPID=KXIG3D(2,JS)
        NCTRL = KXIG3D(3,JS)
        IAD_KNOT = IGEO(40,IPID)
        PX = IGEO(41,IPID)
        PY = IGEO(42,IPID)
        PZ = IGEO(43,IPID)
        N1 = IGEO(44,IPID)
        N2 = IGEO(45,IPID)
        N3 = IGEO(46,IPID)
        IDFRSTLOCKNT = IGEO(47,IPID)
        NKNOT1 = N1+PX
        NKNOT2 = N2+PY
        NKNOT3 = N3+PZ
        IDX = KXIG3D(6,JS)
        IDY = KXIG3D(7,JS)
        IDZ = KXIG3D(8,JS)
        IDX2 = KXIG3D(9,JS)
        IDY2 = KXIG3D(10,JS)
        IDZ2 = KXIG3D(11,JS)

C NE PAS OUBLIER DE FAIRE ARRIVER KNOTLOCPC DANS L'APPEL

      ALLOCATE(KNOTLOCX(PX+1,NCTRL),KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL))

      DO J=1,NCTRL 
        DO K=1,PX+1
          KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PY+1
          KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
        DO K=1,PZ+1
          KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
        ENDDO
      ENDDO

      KNOTLOCELX(1,I) = KNOTLOCEL(1,1,JS)
      KNOTLOCELY(1,I) = KNOTLOCEL(1,2,JS)
      KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,JS)
      KNOTLOCELX(2,I) = KNOTLOCEL(2,1,JS)
      KNOTLOCELY(2,I) = KNOTLOCEL(2,2,JS)
      KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,JS)

        ZR = RIGE(1,IG-NUMNOD)
        ZS = RIGE(2,IG-NUMNOD)
        ZT = RIGE(3,IG-NUMNOD)

c      CALL IGE3DBASIS(
c     1     JS        ,0   ,X_IGEO(:)     ,Y_IGEO(:)  ,
c     2     Z_IGEO(:)    ,W_IGEO(:)     ,IDX     ,IDY     ,
c     3     IDZ       ,R         ,
c     4     NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5     KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6     KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7     PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,0        ,X_IGEO(:)  ,Y_IGEO(:),
     2      Z_IGEO(:),W_IGEO(:)    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX(:,I),KNOTLOCELY(:,I),KNOTLOCELZ(:,I))

 
        DO J=1,NCTRL
          K = NUMCP(J)
          IF(ANIM_V(4)+H3D_DATA%N_VECT_CONT .GT.0)THEN
           FCONT(1,K) = FCONT(1,K) - R(J)*FXI(I)
           FCONT(2,K) = FCONT(2,K) - R(J)*FYI(I)
           FCONT(3,K) = FCONT(3,K) - R(J)*FZI(I)
          ENDIF
          A(1,K) = A(1,K) - R(J)*FXI(I)
          A(2,K) = A(2,K) - R(J)*FYI(I)
          A(3,K) = A(3,K) - R(J)*FZI(I)
          STIFN(K) = STIFN(K) + STIF(I)*R(J)

          FX(J) = FX(J) + R(J)*FXI(I)
          FY(J) = FY(J) + R(J)*FYI(I)
          FZ(J) = FZ(J) + R(J)*FZI(I)
        ENDDO

      DEALLOCATE(KNOTLOCX,KNOTLOCY,KNOTLOCZ)

      ENDDO

      RETURN
      END

