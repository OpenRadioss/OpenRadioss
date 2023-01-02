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
C
Chd|====================================================================
Chd|  I6PEN3                        source/interfaces/inter3d1/i6pen3.F
Chd|-- called by -----------
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INIST3                        source/interfaces/inter3d1/inist3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I6PEN3(X     ,IRECT  ,MSR    ,NSV   ,ILOC  ,
     1                  IRTL  ,CST    ,IRTL0  ,GAP   ,NSN   ,
     2                  ITAB  ,IWPENE ,PENI   ,ICOR  ,ID    ,
     3                  INACTI,TITR   )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.2.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,IWPENE,ICOR,INACTI
C     REAL
      my_real
     .   GAP,PENI
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*), IRTL0(*),
     .   ITAB(*)
C     REAL
      my_real
     .   X(3,*), CST(2,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, JJ, NN, IER
C     REAL
      my_real
     .   N1, N2, N3, PEN, ALP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      ALP = TWOEM2
      DO 500 II=1,NSN
      I=NSV(II)
      J=ILOC(II)
      K=MSR(J)
      L=IRTL(II)
      DO 10 JJ=1,4
      NN=MSR(IRECT(JJ,L))
      XX1(JJ)=X(1,NN)
      XX2(JJ)=X(2,NN)
   10 XX3(JJ)=X(3,NN)
      XS1=X(1,I)
      YS1=X(2,I)
      ZS1=X(3,I)
      CALL INIST3(N1,N2,N3,CST(1,II),CST(2,II),IER,ALP)
      IF(IER==-1)THEN
         CALL ANCMSG(MSGID=85,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               I2=ITAB(I),
     .               I3=ITAB(K),
     .               I4=L,
     .               I5=ITAB(MSR(IRECT(1,L))),
     .               I6=ITAB(MSR(IRECT(2,L))),
     .               I7=ITAB(MSR(IRECT(3,L))),
     .               I8=ITAB(MSR(IRECT(4,L))))
      ELSE IF(IER==1)THEN
       IF(IPRI>=1)WRITE(IOUT,FMT=FMW_7I)ITAB(I),ITAB(K),L,
     .                    (ITAB(MSR(IRECT(JJ,L))),JJ=1,4)
      ELSE
        PEN = N1*(XS1-XC)+N2*(YS1-YC)+N3*(ZS1-ZC) - GAP
        IF (PEN<=ZERO) IRTL0(II)=L
        IF (PEN < ZERO) THEN
          PENI = MIN(PEN,PENI)
          CALL ANCMSG(MSGID=346,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,I2=ITAB(I),
     .                C1=TITR,
     .                R1=PEN)
          IF (INACTI == 5) THEN
c            PEN = MIN(ZERO, PEN + GAP)
            GAP = MAX(ZERO, GAP + PEN)   ! constant gap
          ELSEIF (INACTI == 6) THEN
c            PEN = MIN(ZERO, PEN + ZEP05 * (GAP - PEN))
            GAP = MAX(ZERO, GAP + PEN + ZEP05 * (GAP + PEN))   ! constant gap
          ENDIF
          IWPENE=IWPENE+1
        ENDIF
       IF(IPRI>=1)WRITE(IOUT,FMT=FMW_7I_2F)
     .  ITAB(I),ITAB(K),L,
     .  (ITAB(MSR(IRECT(JJ,L))),JJ=1,4),CST(1,II),CST(2,II)
      ENDIF
  500 CONTINUE
      IF (ICOR == 0) PENI = ZERO
C-----------
      RETURN
      END
