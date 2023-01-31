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
Chd|  I5FOR3                        source/interfaces/inter3d/i5for3.F
Chd|-- called by -----------
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- calls ---------------
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I5FOR3(LFT   ,LLT   ,NFT   ,
     2                  E     ,MSR   ,NSV   ,IRTL  ,STF   ,
     3                  STFN  ,IBC   ,ICODT ,FSAV  ,IGIMP ,
     4                  FSKYI ,ISKY  ,FCONT ,FNCONT,ICONTACT,
     5                  IBAG  ,IADM  ,H3D_DATA,
     6                  IX1   ,IX2   ,IX3     ,IX4    ,N1    ,
     7                  N2    ,N3    ,XFACE   ,H1     ,H2    ,
     8                  H3    ,H4    ,THK     ,ANS    ,STIF  ,
     9                  FNI )

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
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBC, IGIMP,LFT, LLT, NFT, IBAG, IADM
      INTEGER MSR(*), NSV(*), IRTL(*), ICODT(*), ISKY(*),
     .        ICONTACT(*)
      my_real 
     .  E(*), STF(*), STFN(*), FSAV(*),FSKYI(LSKYI,4),FCONT(3,*),
     .  FNCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4,THK
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: ANS,STIF,FNI,XFACE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, IL, L, J3, J2, J1, IG,
     .   I3, I2, I1
      INTEGER NISKYL
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ), FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ)
C-----------------------------------------------
!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4))
C
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (ANS,BV(1,22)),(SSC,BV(1,23)),(TTC,BV(1,24))
!      EQUIVALENCE (H1,BV(1,25)) ,(H2,BV(1,26)) ,(H3,BV(1,27))
!      EQUIVALENCE (H4,BV(1,28)) ,(XFACE,BV(1,29)),(STIF,BV(1,30))
!      EQUIVALENCE (FNI,BV(1,40))
!      EQUIVALENCE (FXI,BV(1,41)),(FYI,BV(1,42)),(FZI,BV(1,43))         --> local
!      EQUIVALENCE (FX1,BV(1,44)),(FY1,BV(1,45)),(FZ1,BV(1,46))         --> local
!      EQUIVALENCE (FX2,BV(1,47)),(FY2,BV(1,48)),(FZ2,BV(1,49))         --> local
!      EQUIVALENCE (FX3,BV(1,50)),(FY3,BV(1,51)),(FZ3,BV(1,52))         --> local
!      EQUIVALENCE (FX4,BV(1,53)),(FY4,BV(1,54)),(FZ4,BV(1,55))         --> local
!      EQUIVALENCE (THK,BV(1,70))
C
      DO 100 I=LFT,LLT
      ANS(I)= MIN(ZERO,(ANS(I)*XFACE(I)-THK(I)))
C
C  A=CVMGN(B,C,D) => IF D=0 THEN A=C ELSE A=B
C
C      XFACE(I)=CVMGN(XFACE(I),ZERO,ANS(I))
      IF(ANS(I)==ZERO)XFACE(I)=ZERO
      ANS(I)=XFACE(I)*ANS(I)
 100  CONTINUE
C
      IGIMP=0
      DO 110 I=LFT,LLT
      IGIMP=IGIMP+ABS(XFACE(I))
 110  CONTINUE
      IF(IGIMP==0)RETURN
C
      DO 140 I=LFT,LLT
      IL=I+NFT
      L=IRTL(IL)
      STIF(I)=HALF*STF(L)
 140  CONTINUE
C
      DO 150 I=LFT,LLT
      FNI(I)=ANS(I)*STIF(I)
      FXI(I)=N1(I)*FNI(I)
      FYI(I)=N2(I)*FNI(I)
      FZI(I)=N3(I)*FNI(I)
 150  CONTINUE
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION TOTALE
C---------------------------------
      DO 155 I=LFT,LLT
      FSAV(1)=FSAV(1)+FXI(I)*DT12
      FSAV(2)=FSAV(2)+FYI(I)*DT12
      FSAV(3)=FSAV(3)+FZI(I)*DT12
 155  CONTINUE
C
      DO 160 I=LFT,LLT
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
 160  CONTINUE
C
      IF(IPARIT==0)THEN
        DO 180 I=LFT,LLT
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
C
        IL=I+NFT
        IG=NSV(IL)
        I3=3*IG
        I2=I3-1
        I1=I2-1
        E(I1)=E(I1)-FXI(I)
        E(I2)=E(I2)-FYI(I)
        E(I3)=E(I3)-FZI(I)
 180    CONTINUE
C
      ELSE
C
#include "lockon.inc"
         NISKYL = NISKY
         NISKY = NISKY + 5 * LLT
#include "lockoff.inc"
C
        DO 190 I=LFT,LLT
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
 190    CONTINUE
      ENDIF
C
      IF(INCONV/=1) RETURN
      IF(IBAG/=0.OR.IADM/=0)THEN
#include "lockon.inc"
       DO I=LFT,LLT
          IL=I+NFT
          ICONTACT(NSV(IL))=1
          ICONTACT(IX1(I))=1
          ICONTACT(IX2(I))=1
          ICONTACT(IX3(I))=1
          ICONTACT(IX4(I))=1
       ENDDO
#include "lockoff.inc"
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.
     .   (MANIM>=4.AND.MANIM<=15)))THEN
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
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .   (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))THEN
#include "lockon.inc"
           DO I=1,LLT
            FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FX1(I)
            FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FY1(I)
            FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZ1(I)
            FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FX2(I)
            FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FY2(I)
            FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZ2(I)
            FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FX3(I)
            FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FY3(I)
            FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZ3(I)
            FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FX4(I)
            FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FY4(I)
            FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZ4(I)
            FNCONT(1,NSV(I+NFT))=FNCONT(1,NSV(I+NFT))- FXI(I)
            FNCONT(2,NSV(I+NFT))=FNCONT(2,NSV(I+NFT))- FYI(I)
            FNCONT(3,NSV(I+NFT))=FNCONT(3,NSV(I+NFT))- FZI(I)
           ENDDO
#include "lockoff.inc"
      ENDIF
C
      IF(IBC==0) RETURN
       DO 200 I=LFT,LLT
       IF(IBC==0.OR.XFACE(I)==ZERO)GOTO 200
       IL=I+NFT
       IG=NSV(IL)
       CALL IBCOFF(IBC,ICODT(IG))
 200   CONTINUE
C
      RETURN
      END
