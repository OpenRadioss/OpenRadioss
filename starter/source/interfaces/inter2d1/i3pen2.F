Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
C
!||====================================================================
!||    i3pen2                 ../starter/source/interfaces/inter2d1/i3pen2.F
!||--- called by ------------------------------------------------------
!||    inint2                 ../starter/source/interfaces/inter2d1/inint2.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    format_mod             ../starter/share/modules1/format_mod.F90
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE I3PEN2(X,IRECT,MSR,NSV,ILOC,IRTL,NSN,
     1                  CST,IRTLO,FRIC0,FRIC,GAP,IWPENE,
     2                  ITAB,ID,TITR)
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE FORMAT_MOD , ONLY : FMW_5I_F, FMW_5I
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,IWPENE
      my_real FRIC, GAP
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*), IRTLO(*), ITAB(*)
      my_real X(3,*), CST(2,*), FRIC0(3,*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, M, JJ
      my_real N2, N3, YS, ZS, T2, T3, XL, PEN, SS, YM1, YM2, ZM1, ZM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO 150 II=1,NSN
      I=NSV(II)
      J=ILOC(II)
      K=MSR(J)
      L=IRTL(II)
      IRTLO(II)=0
      FRIC0(1,II)=ZERO
      M=MSR(IRECT(1,L))
      YM1=X(2,M)
      ZM1=X(3,M)
      M=MSR(IRECT(2,L))
      YM2=X(2,M)
      ZM2=X(3,M)
      YS =X(2,I)
      ZS =X(3,I)
C-----------------------------
C     PENETRATION
C-----------------------------
      T2=YM2-YM1
      T3=ZM2-ZM1
      XL=SQRT(T2**2+T3**2)
      IF(XL==0.0)THEN
       CALL ANCMSG(MSGID=80,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=L,I3=ITAB(MSR(IRECT(1,L))),I4=ITAB(MSR(IRECT(2,L))))
      ENDIF
      T2=T2/XL
      T3=T3/XL
      N2= T3
      N3=-T2
      PEN=N2*(YS-YM1)+N3*(ZS-ZM1)-GAP
      IF(PEN>0.0)THEN
       GOTO 110
      ENDIF
      SS=T2*(YS-YM1)+T3*(ZS-ZM1)
      SS=SS/XL
      SS=TWO*SS-ONE
      IF(SS> ONEP05)GO TO 110
      IF(SS<-ONEP05)GO TO 110
      IF(PEN<ZERO)THEN
       IWPENE=IWPENE+1
       CALL ANCMSG(MSGID=346,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=ID,I2=ITAB(I),C1=TITR,R1=PEN)
      ENDIF
      IF(IPRI>=1)THEN
       WRITE(IOUT,FMT=FMW_5I_F)ITAB(I),ITAB(K),L,ITAB(MSR(IRECT(1,L))),ITAB(MSR(IRECT(2,L))),SS
      ENDIF
      IF(FRIC==0.0) GO TO 150
      IRTLO(II)=L
      CST(1,II)=SS
      GO TO 150
 110  CONTINUE
      IF(IPRI>=1)THEN
       WRITE(IOUT,FMT=FMW_5I)ITAB(I),ITAB(K),L,ITAB(MSR(IRECT(1,L))),ITAB(MSR(IRECT(2,L)))
      ENDIF
 150  CONTINUE
C
      RETURN
      END
