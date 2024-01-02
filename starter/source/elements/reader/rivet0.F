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
Chd|  RIVET0                        source/elements/reader/rivet0.F
Chd|-- called by -----------
Chd|        HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RIVET0(V,VR,MS,IN,IXRI,RIVET,GEO,ITAB,IKINE)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXRI(4,*), ITAB(*), IKINE(*)
C     REAL
      my_real
     .   V(3,*), VR(3,*), MS(*), IN(*), RIVET(NRIVF,*),
     .   GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IGL, IG, IROT, K1, K2, IGTYP, IKINE1(3*NUMNOD)
C     REAL
      my_real
     .   XM, XIN
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
      DO 100 I=1,NRIVET
       RIVET(1,I) = ONE
      IG=IXRI(1,I)
C
      IROT=GEO(4,IG)
      K1=IXRI(2,I)
      K2=IXRI(3,I)
      XM=(MS(K1)+MS(K2))
         IGTYP=GEO(12,IG)
         IF (IGTYP/=5) THEN
C           WRITE(ISTDO,*)' ** ERROR/RIVET PROPERTY SET'
C           WRITE(IOUT,1000)IGTYP
C 1000      FORMAT(//,' ** ERROR WRONG RIVET PROPERTY SET IDENTIFIER :',
C     .            I5,//)
C           IERR=IERR+1
            CALL ANCMSG(MSGID=46,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IXRI(4,I),
     .                  I2=IGTYP)
         ENDIF
      IF(MS(K1)<1.E-15.OR.MS(K2)<1.E-15) THEN
C         WRITE(ISTDO,*)' ** ERROR/RIVET OR SPOTWELD DEFINITION'
C         WRITE(IOUT,2000)IXRI(4,I)
C 2000    FORMAT(//,' ** ERROR:ONE OR BOTH OF THE TWO NODES OF RIVET :'
C     .          ,I5,/,'HAVE A NULL MASS',
C     .          ' (MAY BE SECND NODE(S) OF A RIGID BODY)',//)
C         IERR=IERR+1
C        IF(MS(K1)<1.E-15.AND.MS(K2)<1.E-15) CALL ARRET(2)
         IF(MS(K1)<EM15.AND.MS(K2)<EM15) THEN
           CALL ANCMSG(MSGID=47,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=IXRI(4,I))
         END IF
         CALL ANCMSG(MSGID=47,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IXRI(4,I))
      ENDIF
      V(1,K1)=(V(1,K1)*MS(K1)+V(1,K2)*MS(K2))/XM
      V(2,K1)=(V(2,K1)*MS(K1)+V(2,K2)*MS(K2))/XM
      V(3,K1)=(V(3,K1)*MS(K1)+V(3,K2)*MS(K2))/XM
      V(1,K2)=V(1,K1)
      V(2,K2)=V(2,K1)
      V(3,K2)=V(3,K1)
      CALL KINSET(32,ITAB(K1),IKINE(K1),1,0,IKINE1(K1))
      CALL KINSET(32,ITAB(K1),IKINE(K1),2,0,IKINE1(K1))
      CALL KINSET(32,ITAB(K1),IKINE(K1),3,0,IKINE1(K1))
      CALL KINSET(32,ITAB(K2),IKINE(K2),1,0,IKINE1(K2))
      CALL KINSET(32,ITAB(K2),IKINE(K2),2,0,IKINE1(K2))
      CALL KINSET(32,ITAB(K2),IKINE(K2),3,0,IKINE1(K2))
      IF(IROT==1)THEN
       CALL KINSET(32,ITAB(K1),IKINE(K1),4,0,IKINE1(K1))
       CALL KINSET(32,ITAB(K1),IKINE(K1),5,0,IKINE1(K1))
       CALL KINSET(32,ITAB(K1),IKINE(K1),6,0,IKINE1(K1))
       CALL KINSET(32,ITAB(K2),IKINE(K2),4,0,IKINE1(K2))
       CALL KINSET(32,ITAB(K2),IKINE(K2),5,0,IKINE1(K2))
       CALL KINSET(32,ITAB(K2),IKINE(K2),6,0,IKINE1(K2))
       IF(IN(K1)<EM15.AND.IN(K2)<EM15) THEN
C         WRITE(ISTDO,*)' ** ERROR/RIVET OR SPOTWELD DEFINITION'
C         WRITE(IOUT,3000)IXRI(4,I)
C 3000    FORMAT(//,' ** ERROR:ONE BOTH OF THE TWO NODES OF RIVET :'
C     .          ,I5,/,'HAVE A NULL INERTIA',
C     .          ' (MAY BE NODES OF 8 NODES SOLIDS)',//)
C         IERR=IERR+1
C         CALL ARRET(2)
          CALL ANCMSG(MSGID=48,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=IXRI(4,I))
       ENDIF
       XIN=(IN(K1)+IN(K2))
       VR(1,K1)=(VR(1,K1)*IN(K1)+VR(1,K2)*IN(K2))/XIN
       VR(2,K1)=(VR(2,K1)*IN(K1)+VR(2,K2)*IN(K2))/XIN
       VR(3,K1)=(VR(3,K1)*IN(K1)+VR(3,K2)*IN(K2))/XIN
       VR(1,K2)=VR(1,K1)
       VR(2,K2)=VR(2,K1)
       VR(3,K2)=VR(3,K1)
      ENDIF
 100  CONTINUE
C
      RETURN
      END
