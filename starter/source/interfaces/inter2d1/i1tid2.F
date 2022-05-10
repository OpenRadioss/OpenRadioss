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
C
Chd|====================================================================
Chd|  I1TID2                        source/interfaces/inter2d1/i1tid2.F
Chd|-- called by -----------
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I1TID2(X,IRECT,CRST,MSR,NSV,ILOC,IRTL,NSN,ITAB,
     .                  ID,TITR)
      USE MESSAGE_MOD
C
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*), ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), CRST(2,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IER1, II, I, J, K, L, M, JJ, IER2
C     REAL ou REAL*8
      my_real
     .   N2, N3, YS, ZS, T2, T3, XL, SS,YM1,YM2,ZM1,ZM2
C
      IER1=0
      DO 150 II=1,NSN
      I=NSV(II)
      J=ILOC(II)
      K=MSR(J)
      L=IRTL(II)
      M=MSR(IRECT(1,L))
      YM1=X(2,M)
      ZM1=X(3,M)
      M=MSR(IRECT(2,L))
      YM2=X(2,M)
      ZM2=X(3,M)
      YS =X(2,I)
      ZS =X(3,I)
      T2=YM2-YM1
      T3=ZM2-ZM1
      XL=SQRT(T2**2+T3**2)
      IF(XL.EQ.0.0)THEN
C       WRITE(ISTDO,1100)L,(ITAB(MSR(IRECT(JJ,L))),JJ=1,2)
C       WRITE(IOUT,1100)L,(ITAB(MSR(IRECT(JJ,L))),JJ=1,2)
C       CALL ARRET(2)
         CALL ANCMSG(MSGID=80,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               I2=L,
     .               I3=ITAB(MSR(IRECT(1,L))),
     .               I4=ITAB(MSR(IRECT(2,L))))
      ENDIF
      T2=T2/XL
      T3=T3/XL
      N2= T3
      N3=-T2
      SS=T2*(YS-YM1)+T3*(ZS-ZM1)
      SS=SS/XL
      SS=DEUX*SS-UN
      CRST(1,II)=SS
      CRST(2,II)=-UN
      IER2=0
       IF(SS.GT. UNP05.OR.SS.LT.-UNP05)THEN  
       IER1=IER1+1
       IER2=1
      ENDIF
      IF(IPRI.GE.1.OR.IER2.GT.0)THEN
Cgw121       WRITE(IOUT,'(I5,2I9,2I5,F10.6)')
       WRITE(IOUT,FMT=FMW_5I_F)
     . ITAB(I),ITAB(K),L,
     . ITAB(MSR(IRECT(1,L))),ITAB(MSR(IRECT(2,L))),SS
      ENDIF
  150 CONTINUE
C
      IF(IER1.GT.0)THEN
C       WRITE(ISTDO,1200)
C       WRITE(IOUT,1200)
C       CALL ARRET(2)
        CALL ANCMSG(MSGID=81,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR)
      ENDIF
C
      RETURN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C 1100 FORMAT(2X,'**  ERROR SEGMENT',I5,' (NODES',2I8,
C     .       ') HAS ZERO LENGTH')
C 1200 FORMAT(/2X,'** ERROR TIED INTERFACE:',
C     .       ' SLAVE NODE OUTSIDE MASTER SEGMENT')
      END
