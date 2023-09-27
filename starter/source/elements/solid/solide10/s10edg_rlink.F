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
Chd|  S10EDG_RLINK                  source/elements/solid/solide10/s10edg_rlink.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10EDG_RLINK(NLINK, NUMLINK,NNLINK,LNLINK,
     .                       ITAGND,ICNDS10,ITAB,IPRI,NUMNOD,NS10E)
C=======================================================================
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NLINK,NUMLINK,IPRI,NUMNOD,NS10E
      INTEGER, DIMENSION(10,NLINK), INTENT (INOUT) :: NNLINK
      INTEGER, DIMENSION(NUMLINK),  INTENT (INOUT) :: LNLINK
      INTEGER, DIMENSION(NUMNOD),   INTENT (IN   ) :: ITAB
      INTEGER, DIMENSION(NUMNOD),   INTENT (INOUT) :: ITAGND
      INTEGER, DIMENSION(3,NS10E),  INTENT (IN   ) :: ICNDS10
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K, N,ND,N1,N2,NNEW,ID,IER1,IER2
      INTEGER IAD,IU,NSL,NS,NN,NNSL
      LOGICAL IS1,IS2
      INTEGER, DIMENSION(:), ALLOCATABLE :: LL_TMP
C     REAL
       K = 0
       NNEW = 0
       IER1 = 0
       IER2 = 0
       DO N=1,NLINK
         NSL =  NNLINK(1,N)
         IU  =  NNLINK(2,N)
         DO I=1,NSL
           NS = LNLINK(K+I)
           IF (ITAGND(NS) /=0 ) THEN
            ID = IABS(ITAGND(NS))
            ND = ICNDS10(1,ID)
            N1 = ICNDS10(2,ID)
            N2 = ICNDS10(3,ID)
            IS1 = INTAB(NSL,LNLINK(K+1),N1)
            IS2 = INTAB(NSL,LNLINK(K+1),N2)
            IF (IS1.AND.IS2) THEN
              ITAGND(NS) = ITAGND(NS) + NS10E
              NNEW = NNEW + 1
              LNLINK(K+I) = -LNLINK(K+I)
              IER1 =1
              IF (IPRI>=5)
     .         CALL ANCMSG(MSGID=1213,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RIGID LINK ',
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
            ELSEIF (.NOT.(IS1).AND..NOT.(IS2)) THEN
C----error out ND is alone in RLINK
              CALL ANCMSG(MSGID=1216,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                C1='RIGID LINK ',
     .                I2=IU,
     .                C2='RIGID LINK ')
            ELSE
C----removed from INN directly----------
              NNEW = NNEW + 1
              LNLINK(K+I) = -LNLINK(K+I)
              IER2 =1
              IF (IPRI>=5)
     .         CALL ANCMSG(MSGID=1210,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RIGID LINK ',
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
            END IF
           END IF !(ITAGND(NS) /=0 ) THEN
         END DO
         IF (IER1 >0.AND.IPRI>=5) THEN
            CALL ANCMSG(MSGID=1213,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1='RIGID LINK ',
     .                  C2='RIGID LINK ',
     .                  I1=IU,
     .                  PRMOD=MSG_PRINT)
         END IF 
         IF (IER2 >0.AND.IPRI>=5) THEN
            CALL ANCMSG(MSGID=1210,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1='RIGID LINK ',
     .                  C2='RIGID LINK ',
     .                  I1=IU,
     .                  PRMOD=MSG_PRINT)
         END IF 
         K = K + NSL
       END DO
C-------nodes removed from       
       IF (NNEW>0) THEN
         ALLOCATE(LL_TMP(NUMLINK))
         LL_TMP = LNLINK
         K = 0
         NN = 0
         DO N=1,NLINK
           NSL =  NNLINK(1,N)
           NNSL=0
           DO I=1,NSL
             NS = LL_TMP(K+I)
             IF (NS>0) THEN
               NNSL = NNSL+1
               LNLINK(NN+I) = NS
             END IF
           END DO
           NNLINK(1,N) = NNSL
           K = K + NSL
           NN = NN + NNSL
         END DO
         DEALLOCATE(LL_TMP)
       END IF
C       
      RETURN
      END
