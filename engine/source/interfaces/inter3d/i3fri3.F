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
Chd|  I3FRI3                        source/interfaces/inter3d/i3fri3.F
Chd|-- called by -----------
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I3FRI3(LFT   ,LLT   ,NFT   ,
     2                  X     ,E     ,IRECT ,MSR   ,NSV   ,
     3                  IRTL  ,NTY   ,CST   ,IRTLO ,FRIC0 ,
     4                  FRIC  ,IMAST ,FSAV  ,FSKYI ,ISKY  ,
     5                  FCONT ,H3D_DATA,N1  ,N2    ,N3    ,
     6                  IX1   ,IX2     ,IX3 ,IX4   ,H1    ,
     7                  H2    ,H3      ,H4  ,SSC   ,TTC   ,
     8                  XFACE ,STIF    ,XP  ,YP    ,ZP    ,
     9                  FNI   )!,FXI     ,FYI ,FZI   ,FX1   ,
!     1                  FX2   ,FX3     ,FX4 ,FY1   ,FY2   ,
!     2                  FY3   ,FY4     ,FZ1 ,FZ2   ,FZ3   ,
!     3                  FZ4    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
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
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTY, IMAST,LFT, LLT, NFT
      my_real
     .   FRIC
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*), IRTLO(*), ISKY(*)
      my_real
     .   X(3,*), E(*), CST(2,*), FRIC0(3,*), FSAV(*),
     .   FSKYI(LSKYI,NFSKYI),FCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  N1,N2,N3
      INTEGER, DIMENSION(MVSIZ), INTENT(IN):: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: SSC,TTC,XFACE,STIF
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: XP,YP,ZP,FNI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, LOLD, JJ, NN, J3,
     .   J2, J1, IG, I3, I2, I1
      INTEGER NISKYL
      my_real
     .   H(4), XX1(4), XX2(4), XX3(4),SS0, TT0, XC,
     .   YC, ZC, XC0, YC0, ZC0, SP, SM, TP, TM, ANSX, ANSY, ANSZ, FMAX,
     .   STF, FTI, FN, TN1, TN2, TN3, TN, DTM, ECONVT
      my_real, DIMENSION(MVSIZ) :: FXI,FYI,FZI
      my_real, DIMENSION(MVSIZ) :: FX1,FX2,FX3,FX4
      my_real, DIMENSION(MVSIZ) :: FY1,FY2,FY3,FY4
      my_real, DIMENSION(MVSIZ) :: FZ1,FZ2,FZ3,FZ4
C-----------------------------------------------
C   Source Lines
C-----------------------------------------------
      DO 300 I=LFT,LLT
      FXI(I)=ZERO
      FYI(I)=ZERO
      FZI(I)=ZERO
      IL=I+NFT
      ECONVT = ZERO
      IF(XFACE(I)==ZERO) THEN
C-------------------------------
C      POINT NON IMPACTE
C-------------------------------
       IRTLO(IL)=0
       FRIC0(1,IL)=ZERO
       FRIC0(2,IL)=ZERO
       FRIC0(3,IL)=ZERO
      ELSE
C
       LOLD=IABS(IRTLO(IL))
       IF(LOLD==0)THEN
C-------------------------------
C       POINT NON IMPACTE PRECEDEMENT
C-------------------------------
        IRTLO(IL)=IRTL(IL)*XFACE(I)
        CST(1,IL)=SSC(I)
        CST(2,IL)=TTC(I)
       ELSE
C-------------------------------
C       POINT IMPACTE PRECEDEMENT
C-------------------------------
        SS0=CST(1,IL)
        TT0=CST(2,IL)
        FXI(I)=FRIC0(1,IL)
        FYI(I)=FRIC0(2,IL)
        FZI(I)=FRIC0(3,IL)
C
        XC=XP(I)
        YC=YP(I)
        ZC=ZP(I)
        DO JJ=1,4
          NN=MSR(IRECT(JJ,LOLD))
          XX1(JJ)=X(1,NN)
          XX2(JJ)=X(2,NN)
          XX3(JJ)=X(3,NN)
        ENDDO
        XC0=ZERO
        YC0=ZERO
        ZC0=ZERO
        SP=ONE+SS0
        SM=ONE-SS0
        TP= FOURTH*(ONE+TT0)
        TM= FOURTH*(ONE-TT0)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
        DO JJ=1,4
          XC0=XC0+H(JJ)*XX1(JJ)
          YC0=YC0+H(JJ)*XX2(JJ)
          ZC0=ZC0+H(JJ)*XX3(JJ)
        ENDDO
        ANSX= (XC-XC0)
        ANSY= (YC-YC0)
        ANSZ= (ZC-ZC0)
        ECONVT = ECONVT + HALF*(FXI(I)*ANSX+FYI(I)*ANSY+FZI(I)*ANSZ)
C
        FMAX= -MIN(FRIC*FNI(I),ZERO)
C
        STF=EM01*STIF(I)
        FXI(I)=FXI(I) + ANSX*STF
        FYI(I)=FYI(I) + ANSY*STF
        FZI(I)=FZI(I) + ANSZ*STF
        FTI=SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))
C
        FN=FXI(I)*N1(I)+FYI(I)*N2(I)+FZI(I)*N3(I)
        TN1=FXI(I)-N1(I)*FN
        TN2=FYI(I)-N2(I)*FN
        TN3=FZI(I)-N3(I)*FN
        TN=SQRT(TN1*TN1+TN2*TN2+TN3*TN3)
        IF(TN/=ZERO)THEN
         TN1=TN1/TN
         TN2=TN2/TN
         TN3=TN3/TN
        ELSE
         TN3=ZERO
         TN=SQRT(N1(I)*N1(I)+N2(I)*N2(I))
         IF(TN/=ZERO)THEN
          TN2=-N1(I)/TN
          TN1=N2(I)/TN
         ELSE
          TN2=ZERO
          TN1=ONE
         ENDIF
        ENDIF
C
        IF(FTI>FMAX)THEN
C-------------------------------
C        POINT GLISSANT
C-------------------------------
         FXI(I)=TN1*FMAX
         FYI(I)=TN2*FMAX
         FZI(I)=TN3*FMAX
         FRIC0(1,IL)=FXI(I)
         FRIC0(2,IL)=FYI(I)
         FRIC0(3,IL)=FZI(I)
         IRTLO(IL)=IRTL(IL)*XFACE(I)
         CST(1,IL)=SSC(I)
         CST(2,IL)=TTC(I)
        ELSE
C-------------------------------
C        POINT NON GLISSANT
C-------------------------------
         FXI(I)=TN1*FTI
         FYI(I)=TN2*FTI
         FZI(I)=TN3*FTI
        ENDIF
        ECONVT = ECONVT + HALF*(FXI(I)*ANSX+FYI(I)*ANSY+FZI(I)*ANSZ)
       ENDIF
      ENDIF
C
 300  CONTINUE
C
C
#include "atomic.inc"
        ECONTV = ECONTV + ECONVT ! Frictional contact energy
#include "atomend.inc"
#include "atomic.inc"
        FSAV(27) = FSAV(27) + ECONVT
#include "atomend.inc"
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION TOTALE
C---------------------------------
      DTM=IMAST*DT12
      DO 350 I=LFT,LLT
      FSAV(4)=FSAV(4)+FXI(I)*DTM
      FSAV(5)=FSAV(5)+FYI(I)*DTM
      FSAV(6)=FSAV(6)+FZI(I)*DTM
 350  CONTINUE
C
C
      DO 400 I=LFT,LLT
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
 400  CONTINUE
C
      IF(IPARIT==0)THEN
C---------------------------------
C     FRICTION MAIN
C---------------------------------
        DO I=LFT,LLT
          IL=I+NFT
          J3=3*IX1(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX1(I)
          E(J2)=E(J2)+FY1(I)
          E(J3)=E(J3)+FZ1(I)
C  
          J3=3*IX2(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX2(I)
          E(J2)=E(J2)+FY2(I)
          E(J3)=E(J3)+FZ2(I)
C  
          J3=3*IX3(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX3(I)
          E(J2)=E(J2)+FY3(I)
          E(J3)=E(J3)+FZ3(I)
C  
          J3=3*IX4(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX4(I)
          E(J2)=E(J2)+FY4(I)
          E(J3)=E(J3)+FZ4(I)
C---------------------------------
C     FRICTION SECND
C---------------------------------
          IG=NSV(IL)
          I3=3*IG
          I2=I3-1
          I1=I2-1
          E(I1)=E(I1)-FXI(I)
          E(I2)=E(I2)-FYI(I)
          E(I3)=E(I3)-FZI(I)
C
       ENDDO
C
      ELSE
C
#include "lockon.inc"
         NISKYL = NISKY
         NISKY = NISKY + 5 * LLT
#include "lockoff.inc"
C
        IF(KDTINT==0)THEN
          DO I=LFT,LLT
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=ZERO
            ISKY(NISKYL) = IX1(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=ZERO
            ISKY(NISKYL) = IX2(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=ZERO
            ISKY(NISKYL) = IX3(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX4(I)
            FSKYI(NISKYL,2)=FY4(I)
            FSKYI(NISKYL,3)=FZ4(I)
            FSKYI(NISKYL,4)=ZERO
            ISKY(NISKYL) = IX4(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=-FXI(I)
            FSKYI(NISKYL,2)=-FYI(I)
            FSKYI(NISKYL,3)=-FZI(I)
            FSKYI(NISKYL,4)=ZERO
            IL=I+NFT
            ISKY(NISKYL) = NSV(IL)
          ENDDO
        ELSE
          DO I=LFT,LLT
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=ZERO
            FSKYI(NISKYL,5)=ZERO
            ISKY(NISKYL) = IX1(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=ZERO
            FSKYI(NISKYL,5)=ZERO
            ISKY(NISKYL) = IX2(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=ZERO
            FSKYI(NISKYL,5)=ZERO
            ISKY(NISKYL) = IX3(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX4(I)
            FSKYI(NISKYL,2)=FY4(I)
            FSKYI(NISKYL,3)=FZ4(I)
            FSKYI(NISKYL,4)=ZERO
            FSKYI(NISKYL,5)=ZERO
            ISKY(NISKYL) = IX4(I)
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=-FXI(I)
            FSKYI(NISKYL,2)=-FYI(I)
            FSKYI(NISKYL,3)=-FZI(I)
            FSKYI(NISKYL,4)=ZERO
            FSKYI(NISKYL,5)=ZERO
            IL=I+NFT
            ISKY(NISKYL) = NSV(IL)
          ENDDO
        ENDIF
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
#include "lockon.inc"
           DO I=1,LLT
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
            FCONT(1,NSV(I+NFT))=FCONT(1,NSV(I+NFT))- FXI(I)
            FCONT(2,NSV(I+NFT))=FCONT(2,NSV(I+NFT))- FYI(I)
            FCONT(3,NSV(I+NFT))=FCONT(3,NSV(I+NFT))- FZI(I)
           ENDDO
#include "lockoff.inc"
      ENDIF
C
      RETURN
      END
C  054 +++
Chd|====================================================================
Chd|  I5FRI3                        source/interfaces/inter3d/i3fri3.F
Chd|-- called by -----------
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I5FRI3(LFT   ,LLT   ,NFT   ,IPARI ,
     2                  X     ,E     ,IRECT ,MSR   ,NSV   ,
     3                  IRTL  ,NTY   ,CST   ,IRTLO ,FRIC0 ,
     4                  FRIC  ,IMAST ,FSAV  ,FSKYI ,ISKY  ,
     5                  FCONT ,V     ,CF    ,FROT_P,FREQ  ,
     6                  FTSAV ,FTCONT,H3D_DATA,N1  ,N2    ,
     7                  N3    ,IX1   ,IX2     ,IX3 ,IX4   ,
     8                  XP    ,YP    ,ZP      ,SSC ,TTC   ,
     9                  XFACE ,STIF  ,H1    ,H2    ,H3    ,
     1                  H4    ,AREA  ,FNI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
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
#include      "scr03_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), NTY, IMAST, LFT, LLT, NFT
      my_real FRIC
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*), IRTLO(*), ISKY(*)
      my_real X(3,*), E(*), CST(2,*), FRIC0(3,*), FSAV(*),
     .        FSKYI(LSKYI,NFSKYI),
     .        V(3,*), FCONT(3,*),CF(*), FREQ, FROT_P(*),FTSAV(*),
     .        FTCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: XP,YP,ZP
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: SSC,TTC,XFACE,STIF
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4,AREA,FNI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, LOLD, JJ, NN, J3,J2, J1, IG, I3, I2, I1, K, IFQ, MFROT, NISKYL
      my_real
     .   H(4), XX1(4), XX2(4), XX3(4),
     .   FXI(MVSIZ), FYI(MVSIZ),
     .   FZI(MVSIZ), FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ), FY1(MVSIZ), FY2(MVSIZ),
     .   FY3(MVSIZ), FY4(MVSIZ), FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   SS0, TT0, XC, ECONVT, ALPHA, ALPHI,
     .   YC, ZC, XC0, YC0, ZC0, SP, SM, TP, TM, ANSX, ANSY, ANSZ, FMAX,
     .   STF, FTI, FN, TN1, TN2, TN3, TN, DTM, XMU, VX,VY,VZ,VV,V2,P,
     .   VV1,VV2,V21,DMU,AA
      INTEGER IRTLO_I(MVSIZ)
      my_real FRIC0_I(3,MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
C------for implicit run---save previous values-
      IF (INCONV/=1) THEN
       DO I=LFT,LLT
        IL=I+NFT
        IRTLO_I(I)=IRTLO(IL)
        FRIC0_I(1,I)=FRIC0(1,IL)
        FRIC0_I(2,I)=FRIC0(2,IL)
        FRIC0_I(3,I)=FRIC0(3,IL)
       ENDDO
      ENDIF
      DO 300 I=LFT,LLT
      FXI(I)=ZERO
      FYI(I)=ZERO
      FZI(I)=ZERO
      IL=I+NFT
      ECONVT = ZERO
      IF(XFACE(I)==ZERO) THEN
C-------------------------------
C      Point without Impact
C-------------------------------
       IRTLO(IL)=0
       FRIC0(1,IL)=ZERO
       FRIC0(2,IL)=ZERO
       FRIC0(3,IL)=ZERO      
      ELSE
C
       LOLD=IABS(IRTLO(IL))
       IF(LOLD==0)THEN
C-------------------------------
C       Point not impacted previously
C-------------------------------
        IRTLO(IL)=IRTL(IL)*XFACE(I)
        CST(1,IL)=SSC(I)
        CST(2,IL)=TTC(I)
       ELSE
C-------------------------------
C       Point previously impacted
C-------------------------------
        SS0=CST(1,IL)
        TT0=CST(2,IL)
        FXI(I)=FRIC0(1,IL)
        FYI(I)=FRIC0(2,IL)
        FZI(I)=FRIC0(3,IL)
C
        XC=XP(I)
        YC=YP(I)
        ZC=ZP(I)
        DO  JJ=1,4
          NN=MSR(IRECT(JJ,LOLD))
          XX1(JJ)=X(1,NN)
          XX2(JJ)=X(2,NN)
          XX3(JJ)=X(3,NN)
        ENDDO
        XC0=ZERO
        YC0=ZERO
        ZC0=ZERO
        SP=ONE+SS0
        SM=ONE-SS0
        TP=FOURTH*(ONE + TT0)
        TM=FOURTH*(ONE - TT0)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
        DO JJ=1,4
          XC0=XC0+H(JJ)*XX1(JJ)
          YC0=YC0+H(JJ)*XX2(JJ)
          ZC0=ZC0+H(JJ)*XX3(JJ)
        ENDDO
        ANSX= (XC-XC0)
        ANSY= (YC-YC0)
        ANSZ= (ZC-ZC0)
        ECONVT = ECONVT + HALF*(FXI(I)*ANSX+FYI(I)*ANSY+FZI(I)*ANSZ)
        STF=EM01*STIF(I)
        FXI(I)=FXI(I) + ANSX*STF
        FYI(I)=FYI(I) + ANSY*STF
        FZI(I)=FZI(I) + ANSZ*STF
C----        Filtrage du frottement
        IF (CODVERS>=44) THEN
          IFQ = IPARI(31)
          IF (IFQ>0) THEN
                IF (IFQ==3) FREQ = MAX(ONE,FREQ*DT12)
            ALPHA = FREQ
            ALPHI = ONE - ALPHA
            K = 3*(IL-1)
                IF (FNI(I)==0) THEN
              FTSAV(K+1) = ZERO
              FTSAV(K+2) = ZERO
              FTSAV(K+3) = ZERO
              ELSE
              FXI(I)= ALPHA*FXI(I) + ALPHI*FTSAV(K+1)
              FYI(I)= ALPHA*FYI(I) + ALPHI*FTSAV(K+2)
              FZI(I)= ALPHA*FZI(I) + ALPHI*FTSAV(K+3)
               IF (INCONV==1) THEN
              FTSAV(K+1) = FXI(I) 
              FTSAV(K+2) = FYI(I) 
              FTSAV(K+3) = FZI(I) 
             ENDIF
            ENDIF
          ENDIF
        ENDIF
        FTI=SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))
C
        FN =FXI(I)*N1(I)+FYI(I)*N2(I)+FZI(I)*N3(I)
        TN1=FXI(I)-N1(I)*FN
        TN2=FYI(I)-N2(I)*FN
        TN3=FZI(I)-N3(I)*FN
        TN =SQRT(TN1*TN1+TN2*TN2+TN3*TN3)
        IF(TN/=ZERO)THEN
          TN1=TN1/TN
          TN2=TN2/TN
          TN3=TN3/TN
        ELSE
          TN3=ZERO
          TN=SQRT(N1(I)*N1(I)+N2(I)*N2(I))
          IF(TN/=ZERO)THEN
            TN2=-N1(I)/TN
            TN1= N2(I)/TN
          ELSE
            TN2=ZERO
            TN1=ONE
          ENDIF
        ENDIF
C
        P = -FNI(I)/AREA(I)
        XMU = FRIC
        IF(CODVERS>=44) THEN
C---------------------------------
C       NEW FRICTION MODELS
C---------------------------------
          MFROT = IPARI(30)
          IF(MFROT>=1) THEN
            IG=NSV(IL)
            VX = V(1,IG) - H(1)*V(1,IX1(I)) - H(2)*V(1,IX2(I))
     .                   - H(3)*V(1,IX3(I)) - H(4)*V(1,IX4(I))
            VY = V(2,IG) - H(1)*V(2,IX1(I)) - H(2)*V(2,IX2(I))
     .                   - H(3)*V(2,IX3(I)) - H(4)*V(2,IX4(I))
            VZ = V(3,IG) - H(1)*V(3,IX1(I)) - H(2)*V(3,IX2(I))
     .                   - H(3)*V(3,IX3(I)) - H(4)*V(3,IX4(I))
C---        tangantial velocities
            AA = N1(I)*VX + N2(I)*VY + N3(I)*VZ
            VX = VX - N1(I)*AA
            VY = VY - N2(I)*AA
            VZ = VZ - N3(I)*AA
            V2 = VX*VX + VY*VY + VZ*VZ
            VV = SQRT(MAX(EM30, V2))
          ENDIF
          IF(MFROT==1)THEN
            XMU = FRIC + (FROT_P(1) + FROT_P(4)*P ) * P 
     .           +(FROT_P(2) + FROT_P(3)*P) * VV + FROT_P(5)*V2
          ELSEIF(MFROT==2)THEN
C---        Darmstad's law
            XMU = FRIC
     .           + FROT_P(1)*EXP(FROT_P(2)*VV)*P**2
     .           + FROT_P(3)*EXP(FROT_P(4)*VV)*P
     .           + FROT_P(5)*EXP(FROT_P(6)*VV)
          ELSEIF(MFROT==3)THEN
C---        Renard's law
            IF(VV>=0.AND.VV<=FROT_P(5)) THEN
                    DMU = FROT_P(3)-FROT_P(1)
              VV1 = VV / FROT_P(5)
              XMU = FROT_P(1)+ DMU*VV1*(TWO-VV1)
            ELSEIF(VV>FROT_P(5).AND.VV<FROT_P(6)) THEN
                    DMU  = FROT_P(4)-FROT_P(3) 
              VV1  = (VV - FROT_P(5))/(FROT_P(6)-FROT_P(5))
              XMU = FROT_P(3)+ DMU * (THREE-TWO*VV1)*VV1**2
                  ELSE
              DMU  = FROT_P(2)-FROT_P(4)
              VV2  = (VV - FROT_P(6))**2
              XMU = FROT_P(2) - DMU / (ONE + DMU*VV2)
                  ENDIF
                ENDIF
          FMAX = -MIN(XMU*FNI(I),ZERO)
          IF(FTI>FMAX)THEN
C---        sliding
            FXI(I)=TN1*FMAX
            FYI(I)=TN2*FMAX
            FZI(I)=TN3*FMAX
            FRIC0(1,IL)=FXI(I)
            FRIC0(2,IL)=FYI(I)
            FRIC0(3,IL)=FZI(I)
            IRTLO(IL)=IRTL(IL)*XFACE(I)
            CST(1,IL)=SSC(I)
            CST(2,IL)=TTC(I)
          ELSE
C---        tied
            FXI(I)=TN1*FTI
            FYI(I)=TN2*FTI
            FZI(I)=TN3*FTI
          ENDIF
        ELSE
C-----------------------------------------------------
C         CODVERS < 44
C-----------------------------------------------------
            IG=NSV(IL)
            VX = V(1,IG) - H(1)*V(1,IX1(I)) - H(2)*V(1,IX2(I))
     .                   - H(3)*V(1,IX3(I)) - H(4)*V(1,IX4(I))
            VY = V(2,IG) - H(1)*V(2,IX1(I)) - H(2)*V(2,IX2(I))
     .                   - H(3)*V(2,IX3(I)) - H(4)*V(2,IX4(I))
            VZ = V(3,IG) - H(1)*V(3,IX1(I)) - H(2)*V(3,IX2(I))
     .                   - H(3)*V(3,IX3(I)) - H(4)*V(3,IX4(I))
            VV = VX*VX + VY*VY + VZ*VZ 
            XMU  = FRIC + ( CF(1) + CF(4)*P ) * P 
     .           + ( CF(2) + CF(3)*P ) * SQRT(VV) + CF(5)*VV
            FMAX= -MIN(XMU*FNI(I),ZERO)
          IF(FTI>FMAX)THEN
C------------------------------------------------
C        SLIDING POINT
C-------------------------------
            FXI(I)=TN1*FMAX
            FYI(I)=TN2*FMAX
            FZI(I)=TN3*FMAX
            FRIC0(1,IL)=FXI(I)
            FRIC0(2,IL)=FYI(I)
            FRIC0(3,IL)=FZI(I)
            IRTLO(IL)=IRTL(IL)*XFACE(I)
            CST(1,IL)=SSC(I)
            CST(2,IL)=TTC(I)
          ELSE
C-------------------------------
C        NON SLIDING POINTS
C-------------------------------
            FXI(I)=TN1*FTI
            FYI(I)=TN2*FTI
            FZI(I)=TN3*FTI
          ENDIF
        ENDIF
         ECONVT = ECONVT + HALF*(FXI(I)*ANSX+FYI(I)*ANSY+FZI(I)*ANSZ)
       ENDIF
      ENDIF
C
 300  CONTINUE
C
      IF (INCONV==1) THEN
#include "atomic.inc"
        ECONTV = ECONTV + ECONVT ! Frictional contact energy
#include "atomend.inc"
#include "atomic.inc"
        FSAV(27) = FSAV(27) + ECONVT
#include "atomend.inc"
C---------------------------------
C     TOTAL IMPULSE BACKUP
C---------------------------------
        DTM=IMAST*DT12
        DO I=LFT,LLT
          FSAV(4)=FSAV(4)+FXI(I)*DTM
          FSAV(5)=FSAV(5)+FYI(I)*DTM
          FSAV(6)=FSAV(6)+FZI(I)*DTM
        ENDDO
      END IF !(INCONV==1) THEN
C
      DO I=LFT,LLT
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
      ENDDO
C
      IF(IPARIT==0)THEN
C---------------------------------
C     FRICTION MAIN
C---------------------------------
        DO I=LFT,LLT
          IL=I+NFT
          J3=3*IX1(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX1(I)
          E(J2)=E(J2)+FY1(I)
          E(J3)=E(J3)+FZ1(I)
C  
          J3=3*IX2(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX2(I)
          E(J2)=E(J2)+FY2(I)
          E(J3)=E(J3)+FZ2(I)
C  
          J3=3*IX3(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX3(I)
          E(J2)=E(J2)+FY3(I)
          E(J3)=E(J3)+FZ3(I)
C  
          J3=3*IX4(I)
          J2=J3-1
          J1=J2-1
          E(J1)=E(J1)+FX4(I)
          E(J2)=E(J2)+FY4(I)
          E(J3)=E(J3)+FZ4(I)
C---------------------------------
C     FRICTION SECND
C---------------------------------
          IG=NSV(IL)
          I3=3*IG
          I2=I3-1
          I1=I2-1
          E(I1)=E(I1)-FXI(I)
          E(I2)=E(I2)-FYI(I)
          E(I3)=E(I3)-FZI(I)
C
        ENDDO
C
      ELSE
C
#include "lockon.inc"
         NISKYL = NISKY
         NISKY = NISKY + 5 * LLT
#include "lockoff.inc"
C
        IF(KDTINT==0)THEN
         DO 440 I=LFT,LLT
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=ZERO
          ISKY(NISKYL) = IX1(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=ZERO
          ISKY(NISKYL) = IX2(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=ZERO
          ISKY(NISKYL) = IX3(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=ZERO
          ISKY(NISKYL) = IX4(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=-FXI(I)
          FSKYI(NISKYL,2)=-FYI(I)
          FSKYI(NISKYL,3)=-FZI(I)
          FSKYI(NISKYL,4)=ZERO
          IL=I+NFT
          ISKY(NISKYL) = NSV(IL)
 440     CONTINUE
        ELSE
         DO I=LFT,LLT
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=ZERO
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX1(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=ZERO
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX2(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=ZERO
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX3(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=ZERO
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX4(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=-FXI(I)
          FSKYI(NISKYL,2)=-FYI(I)
          FSKYI(NISKYL,3)=-FZI(I)
          FSKYI(NISKYL,4)=ZERO
          FSKYI(NISKYL,5)=ZERO
          IL=I+NFT
          ISKY(NISKYL) = NSV(IL)
         ENDDO
        ENDIF
      ENDIF
C------for implicit run---restore previous values-
      IF (INCONV/=1) THEN
       DO I=LFT,LLT
        IL=I+NFT
        IRTLO(IL)=IRTLO_I(I)
        FRIC0(1,IL)=FRIC0_I(1,I)
        FRIC0(2,IL)=FRIC0_I(2,I)
        FRIC0(3,IL)=FRIC0_I(3,I)
       ENDDO
       RETURN
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0 .AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
#include "lockon.inc"
           DO I=1,LLT
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
            FCONT(1,NSV(I+NFT))=FCONT(1,NSV(I+NFT))- FXI(I)
            FCONT(2,NSV(I+NFT))=FCONT(2,NSV(I+NFT))- FYI(I)
            FCONT(3,NSV(I+NFT))=FCONT(3,NSV(I+NFT))- FZI(I)
           ENDDO
#include "lockoff.inc"
      ENDIF
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP) .OR.
     .   (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))THEN
#include "lockon.inc"
           DO I=1,LLT
            FTCONT(1,IX1(I)) =FTCONT(1,IX1(I)) + FX1(I)
            FTCONT(2,IX1(I)) =FTCONT(2,IX1(I)) + FY1(I)
            FTCONT(3,IX1(I)) =FTCONT(3,IX1(I)) + FZ1(I)
            FTCONT(1,IX2(I)) =FTCONT(1,IX2(I)) + FX2(I)
            FTCONT(2,IX2(I)) =FTCONT(2,IX2(I)) + FY2(I)
            FTCONT(3,IX2(I)) =FTCONT(3,IX2(I)) + FZ2(I)
            FTCONT(1,IX3(I)) =FTCONT(1,IX3(I)) + FX3(I)
            FTCONT(2,IX3(I)) =FTCONT(2,IX3(I)) + FY3(I)
            FTCONT(3,IX3(I)) =FTCONT(3,IX3(I)) + FZ3(I)
            FTCONT(1,IX4(I)) =FTCONT(1,IX4(I)) + FX4(I)
            FTCONT(2,IX4(I)) =FTCONT(2,IX4(I)) + FY4(I)
            FTCONT(3,IX4(I)) =FTCONT(3,IX4(I)) + FZ4(I)
            FTCONT(1,NSV(I+NFT))=FTCONT(1,NSV(I+NFT))- FXI(I)
            FTCONT(2,NSV(I+NFT))=FTCONT(2,NSV(I+NFT))- FYI(I)
            FTCONT(3,NSV(I+NFT))=FTCONT(3,NSV(I+NFT))- FZI(I)
           ENDDO
#include "lockoff.inc"
      ENDIF
C
      RETURN
      END
