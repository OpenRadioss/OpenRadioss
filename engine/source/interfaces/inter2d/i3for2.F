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
Chd|  I3FOR2                        source/interfaces/inter2d/i3for2.F
Chd|-- called by -----------
Chd|        INTVO2                        source/interfaces/inter2d/intvo2.F
Chd|-- calls ---------------
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I3FOR2(X     ,E    ,IRECT,LMSR ,MSR  ,
     1                  NSV   ,ILOC ,IRTL ,STF  ,NSN  ,
     2                  NMN   ,NTY  ,CST  ,IRTLO,FRIC0,
     3                  FRIGAP,STFN ,IBC  ,ICODT,IMAST,
     4                  FSAV  ,FSKYI,ISKY ,PTMAX,AREAS,
     5                  FCONT ,FNCONT,FTCONT,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "scr16_c.inc"
#include      "com06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN, NTY, IBC, IMAST
      INTEGER IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), IRTL(*),
     .   IRTLO(*), ICODT(*), ISKY(*)
C     REAL
      my_real
     .   X(3,*), E(*), STF(*), CST(2,*), FRIC0(3,*), FRIGAP(*),
     .   STFN(*), FSAV(*),FSKYI(LSKYI,NFSKYI),PTMAX, AREAS(*),
     .   FCONT(3,*),FNCONT(3,*), FTCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX(2), II, I, J, K, L, M, IMP, I3, I2, JJ, J3, J2, LOLD,
     .   NISKYL, IPAS
C     REAL
      my_real
     .   H(2), N2, N3, FRIC, GAP, YM1, ZM1, YM2, ZM2, YS, ZS, T2, T3,
     .   XL, ANS, SS, STIF, FNI, FYI, FZI, SS0, FTI, DS, ANST, FMAX,
     .   STFRI, AX, FS, FT
C-----------------------------------------------
      FRIC=FRIGAP(1)
      GAP =FRIGAP(2)
C
      DO 500 II=1,NSN
      IPAS = 0
      I=NSV(II)
      J=ILOC(II)
      K=MSR(J)
      L=IRTL(II)
      M=MSR(IRECT(1,L))
      IX(1)=M
      YM1=X(2,M)
      ZM1=X(3,M)
      M=MSR(IRECT(2,L))
      IX(2)=M
      YM2=X(2,M)
      ZM2=X(3,M)
      YS =X(2,I)
      ZS =X(3,I)
      IF(N2D==1)THEN
       AX=YS
      ELSE
       AX=ONE
      ENDIF
      T2=YM2-YM1
      T3=ZM2-ZM1
      XL=SQRT(T2**2+T3**2)
      T2=T2/XL
      T3=T3/XL
      N2= T3
      N3=-T2
C
      IMP=0
      I3=3*I
      I2=I3-1
C
      ANS =N2*(YS-YM1)+N3*(ZS-ZM1)
      ANS =ANS-GAP
      IF(ANS>ZERO)GOTO 120
      H(2)=T2*(YS-YM1)+T3*(ZS-ZM1)
      H(2)=H(2)/XL
      H(1)=ONE-H(2)
      SS=H(2)-H(1)
      IF(SS> ONEP05)GO TO 120
      IF(SS<-ONEP05)GO TO 120
      SS= MAX(-ONE,SS)
      SS= MIN( ONE,SS)
C
      IF(NTY==5)THEN
C rajout test sur stifness cote secnd
       IF (STFN(II)<ZERO) THEN
         STIF = ZERO
       ELSE
         STIF=STF(L)
       ENDIF
      ELSE
       STIF=STF(L)*STFN(II)/ MAX(EM20,(STF(L)+STFN(II)))
      ENDIF
      FNI=ANS*STIF
      FYI=N2*FNI
      FZI=N3*FNI
      IMP=1
C-------------------------------------
C     SAUVEGARDE DE L'IMPULSION TOTALE
C-------------------------------------
      FSAV(2)=FSAV(2)+FYI*IMAST*DT12*AX
      FSAV(3)=FSAV(3)+FZI*IMAST*DT12*AX
C
      IF(IPARIT==0)THEN
       DO 100 JJ=1,2
       J3=3*IX(JJ)
       J2=J3-1
       E(J2)=E(J2)+FYI*H(JJ)
       E(J3)=E(J3)+FZI*H(JJ)
 100   CONTINUE
       E(I2)=E(I2)-FYI
       E(I3)=E(I3)-FZI
      ELSE
#include "lockon.inc"
         NISKYL = NISKY
         NISKY = NISKY + 3
#include "lockoff.inc"
        IPAS = 1
C
        IF(KDTINT==0)THEN
         FSKYI(NISKYL+1,1)= ZERO
         FSKYI(NISKYL+1,2)= FYI*H(1)
         FSKYI(NISKYL+1,3)= FZI*H(1)
         FSKYI(NISKYL+1,4)= ZERO
         ISKY(NISKYL+1) = IX(1)
C
         FSKYI(NISKYL+2,1)= ZERO
         FSKYI(NISKYL+2,2)= FYI*H(2)
         FSKYI(NISKYL+2,3)= FZI*H(2)
         FSKYI(NISKYL+2,4)= ZERO
         ISKY(NISKYL+2) = IX(2)
C
         FSKYI(NISKYL+3,1)= ZERO
         FSKYI(NISKYL+3,2)= -FYI
         FSKYI(NISKYL+3,3)= -FZI
         FSKYI(NISKYL+3,4)= ZERO
         ISKY(NISKYL+3) = I
        ELSE
         FSKYI(NISKYL+1,1)= ZERO
         FSKYI(NISKYL+1,2)= FYI*H(1)
         FSKYI(NISKYL+1,3)= FZI*H(1)
         FSKYI(NISKYL+1,4)= ZERO
         FSKYI(NISKYL+1,5)= ZERO
         ISKY(NISKYL+1) = IX(1)
C
         FSKYI(NISKYL+2,1)= ZERO
         FSKYI(NISKYL+2,2)= FYI*H(2)
         FSKYI(NISKYL+2,3)= FZI*H(2)
         FSKYI(NISKYL+1,4)= ZERO
         FSKYI(NISKYL+1,5)= ZERO
         ISKY(NISKYL+2) = IX(2)
C
         FSKYI(NISKYL+3,1)= ZERO
         FSKYI(NISKYL+3,2)= -FYI
         FSKYI(NISKYL+3,3)= -FZI
         FSKYI(NISKYL+1,4)= ZERO
         FSKYI(NISKYL+1,5)= ZERO
         ISKY(NISKYL+3) = I
        ENDIF
      ENDIF  
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
         FCONT(2,IX(1)) =FCONT(2,IX(1)) + FYI*H(1)
         FCONT(3,IX(1)) =FCONT(3,IX(1)) + FZI*H(1)
         FCONT(2,IX(2)) =FCONT(2,IX(2)) + FYI*H(2)
         FCONT(3,IX(2)) =FCONT(3,IX(2)) + FZI*H(2)
c
         FCONT(2,I)=FCONT(2,I)- FYI
         FCONT(3,I)=FCONT(3,I)- FZI
      ENDIF

      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .        ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D .OR.
     .            (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))THEN

        FNCONT(2,IX(1)) =FNCONT(2,IX(1)) + FYI*H(1)
        FNCONT(3,IX(1)) =FNCONT(3,IX(1)) + FZI*H(1)
        FNCONT(2,IX(2)) =FNCONT(2,IX(2)) + FYI*H(2)
        FNCONT(3,IX(2)) =FNCONT(3,IX(2)) + FZI*H(2)
C
        FNCONT(2,I)=FNCONT(2,I)- FYI
        FNCONT(3,I)=FNCONT(3,I)- FZI
      ENDIF

      IF(IBC/=0)CALL IBCOFF(IBC,ICODT(I))
C
 120  CONTINUE
      IF(FRIC==ZERO)GO TO 500
      IF(IMP==0) THEN
       IRTLO(II)=0
       FRIC0(2,II)=ZERO
       FRIC0(3,II)=ZERO
       GO TO 500
      ENDIF
C
      LOLD=IRTLO(II)
      IF(LOLD==0)THEN
       IRTLO(II)=L
       CST(1,II)=SS
       GO TO 500
      ENDIF
C
      SS0=CST(1,II)
      FTI=FRIC0(1,II)
      DS=SS-SS0
      ANST=HALF*DS*XL
      FMAX=-MIN(FRIC*FNI,ZERO)
      STFRI=EM01*STIF
      FTI=FTI + ANST*STFRI
C
      IF(FTI>FMAX)THEN
       FTI=FMAX
      ELSE
       IF(FTI<-FMAX)THEN
        FTI=-FMAX
       ELSE
        FRIC0(1,II)=FTI
        IRTLO(II)=L
        CST(1,II)=SS
       ENDIF
      ENDIF
C
c-------limit tangential force is ON : FT<= YIELD/(S*sqrt(3))
      FS   = PTMAX*AREAS(II)/SQRT(THREE)
      FT =FTI
      IF(FS>ZERO) THEN
         IF(FTI>FS)THEN
            FT=FS
         ELSEIF(FTI<-FS)THEN
            FT=-FS
         ENDIF
      ENDIF
C---------------------------------------------------------------
      FYI=T2*FT
      FZI=T3*FT
C-------------------------------------
C     SAUVEGARDE DE L'IMPULSION TOTALE
C-------------------------------------
      FSAV(5)=FSAV(5)+FYI*IMAST*DT12*AX
      FSAV(6)=FSAV(6)+FZI*IMAST*DT12*AX
C
      IF(IPARIT==0)THEN
       DO 400 JJ=1,2
       J3=3*IX(JJ)
       J2=J3-1
       E(J2)=E(J2)+FYI*H(JJ)
 400   E(J3)=E(J3)+FZI*H(JJ)
       E(I2)=E(I2)-FYI
       E(I3)=E(I3)-FZI
      ELSE 
C
        IF(IPAS==0) THEN
#include "lockon.inc"
          NISKYL = NISKY
          NISKY = NISKY + 3
#include "lockoff.inc"
         IF(KDTINT==0)THEN
          FSKYI(NISKYL,1)= ZERO
          FSKYI(NISKYL+1,2)= FYI*H(1)
          FSKYI(NISKYL+1,3)= FZI*H(1)
          FSKYI(NISKYL+1,4)= ZERO
          ISKY(NISKYL+1) = IX(1)
C
          FSKYI(NISKYL+2,1)= ZERO
          FSKYI(NISKYL+2,2)= FYI*H(2)
          FSKYI(NISKYL+2,3)= FZI*H(2)
          FSKYI(NISKYL+2,4)= ZERO
          ISKY(NISKYL+2) = IX(2)
C
          FSKYI(NISKYL+3,1)= ZERO
          FSKYI(NISKYL+3,2)= -FYI
          FSKYI(NISKYL+3,3)= -FZI
          FSKYI(NISKYL+3,4)= ZERO
          ISKY(NISKYL+3) = I
         ELSE
          FSKYI(NISKYL,1)= ZERO
          FSKYI(NISKYL+1,2)= FYI*H(1)
          FSKYI(NISKYL+1,3)= FZI*H(1)
          FSKYI(NISKYL+1,4)= ZERO
          FSKYI(NISKYL+1,5)= ZERO
          ISKY(NISKYL+1) = IX(1)
C
          FSKYI(NISKYL+2,1)= ZERO
          FSKYI(NISKYL+2,2)= FYI*H(2)
          FSKYI(NISKYL+2,3)= FZI*H(2)
          FSKYI(NISKYL+2,4)= ZERO
          FSKYI(NISKYL+1,5)= ZERO
          ISKY(NISKYL+2) = IX(2)
C
          FSKYI(NISKYL+3,1)= ZERO
          FSKYI(NISKYL+3,2)= -FYI
          FSKYI(NISKYL+3,3)= -FZI
          FSKYI(NISKYL+3,4)= ZERO
          FSKYI(NISKYL+1,5)= ZERO
          ISKY(NISKYL+3) = I
         ENDIF
        ELSE
C deja traite plus haut
         FSKYI(NISKYL+1,2)= FSKYI(NISKYL+1,2)+FYI*H(1)
         FSKYI(NISKYL+1,3)= FSKYI(NISKYL+1,3)+FZI*H(1)
C
         FSKYI(NISKYL+2,2)= FSKYI(NISKYL+2,2)+FYI*H(2)
         FSKYI(NISKYL+2,3)= FSKYI(NISKYL+2,3)+FZI*H(2)
C
         FSKYI(NISKYL+3,2)= FSKYI(NISKYL+3,2)-FYI
         FSKYI(NISKYL+3,3)= FSKYI(NISKYL+3,3)-FZI
        ENDIF
      ENDIF  
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
          FCONT(2,IX(1)) =FCONT(2,IX(1)) + FYI*H(1)
          FCONT(3,IX(1)) =FCONT(3,IX(1)) + FZI*H(1)
          FCONT(2,IX(2)) =FCONT(2,IX(2)) + FYI*H(2)
          FCONT(3,IX(2)) =FCONT(3,IX(2)) + FZI*H(2)
c
          FCONT(2,I)=FCONT(2,I)- FYI
          FCONT(3,I)=FCONT(3,I)- FZI
      ENDIF
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .        ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .            (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))THEN
        FTCONT(2,IX(1)) =FTCONT(2,IX(1)) + FYI*H(1)
        FTCONT(3,IX(1)) =FTCONT(3,IX(1)) + FZI*H(1)
        FTCONT(2,IX(2)) =FTCONT(2,IX(2)) + FYI*H(2)
        FTCONT(3,IX(2)) =FTCONT(3,IX(2)) + FZI*H(2)
C
        FTCONT(2,I)=FTCONT(2,I)- FYI
        FTCONT(3,I)=FTCONT(3,I)- FZI
      ENDIF

 500  CONTINUE
      RETURN
      END
