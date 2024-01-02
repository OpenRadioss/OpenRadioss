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
Chd|  LCBCSF                        source/constraints/general/bcs/lcbcsf.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LCBCSF(ICODE,ISKEW,NUMBCSN,ITAB,ITABM1,
     2                  NPBY ,ISKWN,WEIGHT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMBCSN
      INTEGER ICODE(*), ISKEW(*), ITAB(*), ITABM1(*), NPBY(*),
     .        ISKWN(LISKN,*), WEIGHT(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ(12), JO(12), IC, NC, N, NUSR, IS, NOSYS, ICO, ICO1,
     .   ICO2, ICO3, ICO4, I, IC1, IC2, IC3, IC4, LL, NOSYSV
      CHARACTER MESS*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
C     REAL
      my_real
     .   SYSFUS
C-----------------------------------------------
C
      DATA MESS/'BOUNDARY CONDITIONS                     '/
C
       IC=0
       NC=1
       DO 10 N=1,NUMBCSN
       READ (IIN,'(I10,4(1X,3I1),I10)') NUSR,JJ,IS
       NOSYS=SYSFUS2(NUSR,ITABM1,NUMNOD)
       NOSYSV = NOSYS
       IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(NOSYSV,1)
       IF(ISPMD==0) THEN
         IF(NOSYSV==0) THEN
           CALL ANCMSG(MSGID=202,ANMODE=ANINFO,
     .                 I1=NUSR)
           IERR=IERR+1
         END IF
       END IF
       IF(NOSYS==0)GOTO 10
C
       ICO=ICODE(NOSYS)
       ICO1=ICO/512
       ICO2=(ICO-512*ICO1)/64
       ICO3=(ICO-512*ICO1-64*ICO2)/8
       ICO4=(ICO-512*ICO1-64*ICO2-8*ICO3)
       JO(1)=ICO1/4
       JO(2)=(ICO1-4*JO(1))/2
       JO(3)=(ICO1-4*JO(1)-2*JO(2))
       JO(4)=ICO2/4
       JO(5)=(ICO2-4*JO(4))/2
       JO(6)=(ICO2-4*JO(4)-2*JO(5))
       JO(7)=ICO3/4
       JO(8)=(ICO3-4*JO(7))/2
       JO(9)=(ICO3-4*JO(7)-2*JO(8))
       JO(10)=ICO4/4
       JO(11)=(ICO4-4*JO(10))/2
       JO(12)=(ICO4-4*JO(10)-2*JO(11))
C
       DO 5 I=1,12
       IF(JJ(I)==0)THEN
        JJ(I)=JO(I)
       ELSEIF(JJ(I)==2)THEN
        JJ(I)=0
       ENDIF
 5     CONTINUE
C
       IC1=JJ(1)*4 +JJ(2)*2 +JJ(3)
       IC2=JJ(4)*4 +JJ(5)*2 +JJ(6)
       IC3=JJ(7)*4 +JJ(8)*2 +JJ(9)
       IC4=JJ(10)*4+JJ(11)*2+JJ(12)
       IC=IC1*512+IC2*64+IC3*8+IC4
       ICODE(NOSYS)=IC
       DO 7 LL=0,NUMSKW
 7       IF(IS==ISKWN(4,LL+1)) ISKEW(NOSYS)=LL+1
C       ISKEW(NOSYS)=IS
   10  CONTINUE
C
       IF(ISPMD==0) WRITE(IOUT,1300)
       DO 500 N=1,NUMNOD
       IC=ICODE(N)
       IF (IC==0) GO TO 500
       IC1=IC/512
       IC2=(IC-512*IC1)/64
       IC3=(IC-512*IC1-64*IC2)/8
       IC4=(IC-512*IC1-64*IC2-8*IC3)
       JJ(1)=IC1/4
       JJ(2)=(IC1-4*JJ(1))/2
       JJ(3)=(IC1-4*JJ(1)-2*JJ(2))
       JJ(4)=IC2/4
       JJ(5)=(IC2-4*JJ(4))/2
       JJ(6)=(IC2-4*JJ(4)-2*JJ(5))
       JJ(7)=IC3/4
       JJ(8)=(IC3-4*JJ(7))/2
       JJ(9)=(IC3-4*JJ(7)-2*JJ(8))
       JJ(10)=IC4/4
       JJ(11)=(IC4-4*JJ(10))/2
       JJ(12)=(IC4-4*JJ(10)-2*JJ(11))
       IF(WEIGHT(N)==1)
     .   WRITE(IOUT,'(1X,I10,4(1X,3I2),3X,I10)')ITAB(N),JJ,
     .                                        ISKWN(4,ISKEW(N))
 500   CONTINUE
      RETURN
C-----------------------------------------------------------------
 1300  FORMAT(/,
     . 1X,'      BOUNDARY CONDITIONS',/
     . 1X,'      -------------------',/
     . 1X,'      NODE  TRANS. ROTAT. GRID   LAGRA.    SKEW',/)
C
      END
