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
Chd|  ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ELSTAGBOX2(IXS  ,ELSTAG   ,KEY2  ,IEXT,X ,SKEW,
     .              ISKN   ,FAC_L,IFLAGUNIT,ITABM1,ISK ,BOXTYPE,
     .              DIAM   ,XP1  ,YP1      ,ZP1   ,XP2 ,YP2    ,
     .              ZP2    ,J2   ,ID       ,TITR  ,TITR1)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IEXT,ISKN(LISKN,*),ELSTAG(*),IFLAGUNIT,
     .      ITABM1(*),FLAG,ISK,BOXTYPE,J2(*)
      MY_REAL
     .      X(3,*),SKEW(LSKEW,*),FAC_L,DIAM,XP1,YP1,ZP1,XP2,YP2,ZP2
      CHARACTER KEY2*5
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR,TITR1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,JS,K,J,OK,FAC,OK1,
     .    JREC,FACES(4,6),PWR(7)

      MY_REAL
     .    NODINB(3)
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA PWR/1,2,4,8,16,32,64/
      CHARACTER MESS*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,USRTOS
      DATA MESS/'BOX DEFINITION                          '/
C=======================================================================
      OK  = 0
      OK1 = 0
C
      IF(KEY2 == 'RECTA')THEN
C-------------------------
C  parallelepiped (oriented) box selection
C-------------------------
        FAC = 0
        DO J=1,2
          IF(USRTOS(J2(J),ITABM1) /= 0) FAC = FAC + 1
        ENDDO
        IF(FAC.LT.2)GOTO 100
          XP1 = X(1,USR2SYS(J2(1),ITABM1,MESS,ID))
          YP1 = X(2,USR2SYS(J2(1),ITABM1,MESS,ID))
          ZP1 = X(3,USR2SYS(J2(1),ITABM1,MESS,ID))
          XP2 = X(1,USR2SYS(J2(2),ITABM1,MESS,ID))
          YP2 = X(2,USR2SYS(J2(2),ITABM1,MESS,ID))
          ZP2 = X(3,USR2SYS(J2(2),ITABM1,MESS,ID))
        IF(FAC == 2)GOTO 200
 100  CONTINUE
        FAC = 0
        IF((XP1 == ZERO.and.YP1 == ZERO.and.ZP1 == ZERO).and.
     .     (XP2 == ZERO.and.YP2 == ZERO.and.ZP2 == ZERO))FAC = FAC + 1
         IF(FAC .EQ. 1)THEN
           CALL ANCMSG(MSGID=752,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=TITR1,
     .                 I1=ID,
     .                 C2=TITR1,
     .                 C3=TITR,
     .                 C4=KLINE)
         END IF
 200  CONTINUE
C
         IF(IFLAGUNIT /= 0)THEN
          XP1  = XP1  * FAC_L
          YP1  = YP1  * FAC_L
          ZP1  = ZP1  * FAC_L
          XP2  = XP2  * FAC_L
          YP2  = YP2  * FAC_L
          ZP2  = ZP2  * FAC_L
         END IF
C
         IF (BOXTYPE == 2) THEN
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                         ISK,NODINB,SKEW,OK)
                IF (OK == 1) THEN
C-------------------------
                 IF(IEXT==0)THEN
                 END IF
C-------------------------
               	 ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
            	 EXIT
                END IF
              ENDDO
            ENDDO
          ENDDO
         ELSE IF (BOXTYPE == 1) THEN
C
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              OK1=0
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                         ISK,NODINB,SKEW,OK)
                IF(OK == 1) OK1 = OK1 + 1
              ENDDO
              IF (OK1 == 4) THEN
                 ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
              ENDIF
            ENDDO
          ENDDO
         ENDIF
C---
      ELSE IF(KEY2 == 'CYLIN')THEN
C-------------------------
C  cylindrical box selection
C-------------------------
        FAC = 0
        DO J=1,2
          IF(USRTOS(J2(J),ITABM1) /= 0) FAC = FAC + 1
        ENDDO
        IF(FAC.LT.2)GOTO 300
          XP1 = X(1,USR2SYS(J2(1),ITABM1,MESS,ID))
          YP1 = X(2,USR2SYS(J2(1),ITABM1,MESS,ID))
          ZP1 = X(3,USR2SYS(J2(1),ITABM1,MESS,ID))
          XP2 = X(1,USR2SYS(J2(2),ITABM1,MESS,ID))
          YP2 = X(2,USR2SYS(J2(2),ITABM1,MESS,ID))
          ZP2 = X(3,USR2SYS(J2(2),ITABM1,MESS,ID))
        IF(FAC == 2)GOTO 400
 300  CONTINUE
        FAC = 0
        IF((XP1 == ZERO.and.YP1 == ZERO.and.ZP1 == ZERO).and.
     .     (XP2 == ZERO.and.YP2 == ZERO.and.ZP2 == ZERO))FAC = FAC + 1
         IF(FAC .EQ. 1)THEN
           CALL ANCMSG(MSGID=752,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=TITR1,
     .                 I1=ID,
     .                 C2=TITR1,
     .                 C3=TITR,
     .                 C4=KLINE)
         END IF
 400  CONTINUE
C
         IF(IFLAGUNIT /= 0)THEN
          DIAM = DIAM * FAC_L
          XP1  = XP1  * FAC_L
          YP1  = YP1  * FAC_L
          ZP1  = ZP1  * FAC_L
          XP2  = XP2  * FAC_L
          YP2  = YP2  * FAC_L
          ZP2  = ZP2  * FAC_L
         END IF
C
         IF (BOXTYPE == 2) THEN
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                        NODINB  , DIAM, OK )
                IF (OK == 1) THEN
C-------------------------
                 IF(IEXT==0)THEN
                 END IF
C-------------------------
               	 ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
            	 EXIT
                END IF
              ENDDO
            ENDDO
          ENDDO
         ELSE IF (BOXTYPE == 1) THEN
C
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                        NODINB  , DIAM, OK )
                IF(OK == 0)THEN
            	  ELSTAG(JS)=ELSTAG(JS)-PWR(JJ)
                  EXIT
                END IF
              ENDDO
            ENDDO
          ENDDO
         ENDIF
C---
      ELSE IF(KEY2 == 'SPHER')THEN
C-------------------------
C  spherical box selection
C-------------------------
        FAC = 0
        IF(USRTOS(J2(1),ITABM1) /= 0) FAC = FAC + 1
        IF(FAC.LT.1)GOTO 500
          XP1 = X(1,USR2SYS(J2(1),ITABM1,MESS,ID))
          YP1 = X(2,USR2SYS(J2(1),ITABM1,MESS,ID))
          ZP1 = X(3,USR2SYS(J2(1),ITABM1,MESS,ID))
        IF(FAC == 1)GOTO 600
 500  CONTINUE
        FAC = 0
        IF(XP1 == ZERO.and.YP1 == ZERO.and.ZP1 == ZERO)
     .     FAC = FAC + 1
         IF(FAC .EQ. 1)THEN
           CALL ANCMSG(MSGID=753,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=TITR1,
     .                 I1=ID,
     .                 C2=TITR1,
     .                 C3=TITR,
     .                 C4=KLINE)
         END IF
 600  CONTINUE
C
         IF(IFLAGUNIT /= 0)THEN
          DIAM = DIAM * FAC_L
          XP1  = XP1  * FAC_L
          YP1  = YP1  * FAC_L
          ZP1  = ZP1  * FAC_L
         END IF
C
         IF (BOXTYPE == 2) THEN
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                CALL CHECKSPHERE(XP1 , YP1, ZP1, NODINB,
     .                           DIAM, OK )
                IF (OK == 1) THEN
C-------------------------
                 IF(IEXT==0)THEN
                 END IF
C-------------------------
               	 ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
            	 EXIT
                END IF
              ENDDO
            ENDDO
          ENDDO
         ELSE IF (BOXTYPE == 1) THEN
C
          DO JS=1,NUMELS
            ELSTAG(JS)=0
            DO JJ=1,6
              ELSTAG(JS)=ELSTAG(JS)+PWR(JJ)
              DO K=1,4
                OK=0
                J=IXS(FACES(K,JJ)+1,JS)
                NODINB(1) = X(1,J)
                NODINB(2) = X(2,J)
                NODINB(3) = X(3,J)
                CALL CHECKSPHERE(XP1 , YP1, ZP1, NODINB,
     .                           DIAM, OK )
                IF(OK == 0)THEN
            	  ELSTAG(JS)=ELSTAG(JS)-PWR(JJ)
                  EXIT
                END IF
              ENDDO
            ENDDO
          ENDDO
         ENDIF
      END IF
C-----------
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  CHECKPARA                     source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        BOXTAGE                       source/model/box/bigbox.F     
Chd|        BOXTAGN                       source/model/box/bigbox.F     
Chd|        BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|        ELSTAGBOX                     source/model/box/bigbox.F     
Chd|        ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|        SIMPLE_ELT_BOX                source/model/sets/simpl_elt_box.F
Chd|        SIMPLE_NODE_BOX               source/model/sets/simple_node_box.F
Chd|        SIMPLE_RBODY_BOX              source/model/sets/simple_rbody_box.F
Chd|-- calls ---------------
Chd|        INSIDE_PARALLELEPIPED         source/model/box/rdbox.F      
Chd|        PROJSKEW                      source/model/box/rdbox.F      
Chd|====================================================================
      SUBROUTINE CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                     ISK,NODIN,SKEW,OK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISK,OK
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,SKEW(LSKEW,*),NODIN(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .    P1(3),P2(3),P3(3),P4(3),PP2(3)
C-----------------------------------------------
      P1(1) = XP1
      P1(2) = YP1
      P1(3) = ZP1
      CALL PROJSKEW(P1,SKEW,ISK)
C
      PP2(1) = XP2
      PP2(2) = YP2
      PP2(3) = ZP2
      CALL PROJSKEW(PP2,SKEW,ISK)
C
      P2(1) = PP2(1)
      P2(2) = P1(2)
      P2(3) = P1(3)
C
      P3(1) = P1(1)
      P3(2) = PP2(2)
      P3(3) = P1(3)
C
      P4(1) = P1(1)
      P4(2) = P1(2)
      P4(3) = PP2(3)
C
      CALL PROJSKEW(NODIN,SKEW,ISK)
C
      CALL INSIDE_PARALLELEPIPED(P1, P2, P3, P4, NODIN, OK)
C
      RETURN
      END
Chd|====================================================================
Chd|  PROJSKEW                      source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROJSKEW(PO,SK,ISK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISK
      my_real
     .      PO(3),SK(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JSK
      my_real
     .    SUM,PN(3)
C-----------------------------------------------
      JSK = ISK + 1
C
      PN(1) = PO(1)*SK(1,JSK) + PO(2)*SK(2,JSK) + PO(3)*SK(3,JSK)
      SUM   = SK(1,JSK)**2 + SK(2,JSK)**2 + SK(3,JSK)**2
      SUM   = SQRT(SUM)
      PN(1) = PN(1) / SUM
C
      PN(2) = PO(1)*SK(4,JSK) + PO(2)*SK(5,JSK) + PO(3)*SK(6,JSK)
      SUM   = SK(4,JSK)**2 + SK(5,JSK)**2 + SK(6,JSK)**2
      SUM   = SQRT(SUM)
      PN(2) = PN(2) / SUM
C
      PN(3) = PO(1)*SK(7,JSK) + PO(2)*SK(8,JSK) + PO(3)*SK(9,JSK)
      SUM   = SK(7,JSK)**2 + SK(8,JSK)**2 + SK(9,JSK)**2
      SUM   = SQRT(SUM)
      PN(3) = PN(3) / SUM
C
      PO(1) = PN(1)
      PO(2) = PN(2)
      PO(3) = PN(3)
C
      RETURN
      END
Chd|====================================================================
Chd|  INSIDE_PARALLELEPIPED         source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INSIDE_PARALLELEPIPED(P1, P2, P3, P4, P, OK)
C---nouvelle routine 10.0 SA1
C  nodes inside parallelepiped in 3D.
C
C
C         *------------------*
C        / .                / \
C       /   .              /   \
C      /     .            /     \
C    P4------------------*       \
C      \        .         \       \
C       \        .         \       \
C        \        .         \       \
C         \       P2.........\.......\
C          \     .            \     /
C           \   .              \   /
C            \ .                \ /
C            P1-----------------P3
C
C
C  Parameters:
C
C    Input, reals: P1(3), P2(3), P3(3), P4(3), four corners
C    of the parallelepiped.  It is assumed that P2, P3 and P4 are
C    immediate neighbors of P1.
C
C    Input, real: P(3), the node to be checked.
C
C    IF integer "OK == 1", the node P
C    is inside the parallelepiped, or on its boundary.
C
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
      INTEGER OK
      my_real
     .    P1(3),P2(3),P3(3),P4(3),P(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    DOT,SUMA
C-----------------------------------------------
      DOT = DOT_PRODUCT( P(1:3) - P1(1:3),
     .                  P2(1:3) - P1(1:3))
      IF(DOT < ZERO)RETURN
      SUMA = SUM ((P2(1:3) - P1(1:3) )**2)
      IF((SUMA == ZERO . AND. P(1) /= P1(1)) .OR.
     .    SUMA < DOT)RETURN
C---
      DOT = DOT_PRODUCT( P(1:3) - P1(1:3),
     .                  P3(1:3) - P1(1:3))
      IF(DOT < ZERO)RETURN
      SUMA = SUM ((P3(1:3) - P1(1:3) )**2)
      IF((SUMA == ZERO . AND. P(2) /= P1(2)) .OR.
     .    SUMA < DOT)RETURN
C---
      DOT = DOT_PRODUCT( P(1:3) - P1(1:3),
     .                  P4(1:3) - P1(1:3))
      IF(DOT < ZERO)RETURN
      SUMA = SUM ((P4(1:3) - P1(1:3) )**2)
      IF((SUMA == ZERO . AND. P(3) /= P1(3)) .OR.
     .    SUMA < DOT)RETURN
C---
      OK = 1
C---
      RETURN
      END
Chd|====================================================================
Chd|  CHECKCYL                      source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        BOXTAGE                       source/model/box/bigbox.F     
Chd|        BOXTAGN                       source/model/box/bigbox.F     
Chd|        BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|        ELSTAGBOX                     source/model/box/bigbox.F     
Chd|        ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|        SIMPLE_ELT_BOX                source/model/sets/simpl_elt_box.F
Chd|        SIMPLE_NODE_BOX               source/model/sets/simple_node_box.F
Chd|        SIMPLE_RBODY_BOX              source/model/sets/simple_rbody_box.F
Chd|-- calls ---------------
Chd|        INSIDE_CYLINDER               source/model/box/rdbox.F      
Chd|====================================================================
      SUBROUTINE CHECKCYL(XP1, YP1, ZP1, XP2, YP2, ZP2,
     .                    NODIN, D, OK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OK
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,NODIN(3),D
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    P1(3),P2(3)
C-----------------------------------------------

C-----------------------------------------------
      P1(1) = XP1
      P1(2) = YP1
      P1(3) = ZP1
C
      P2(1) = XP2
      P2(2) = YP2
      P2(3) = ZP2
C
      CALL INSIDE_CYLINDER(P1, P2, NODIN, D, OK)
C
      RETURN
      END
Chd|====================================================================
Chd|  INSIDE_CYLINDER               source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INSIDE_CYLINDER(P1, P2, P, D, OK)
C-----------------------------------------------
C    The surface and interior of a (finite) cylinder in 3D is defined
C    by an axis, which is the line segment from point P1 to P2, and a
C    diameter D.  The points contained in the volume include:
C    * points at a distance less than or equal to D/2 from the line through P1
C      and P2, whose nearest point on the line through P1 and P2 is, in fact,
C      P1, P2, or any point between them.
C---
C    Input, D, the diameter of the cylinder.
C    Input, P(3), the checked point.
C    Input, P1(3), P2(3), the points defining the cylinder axis.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OK
      my_real
     .    P1(3),P2(3),P(3),D
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  AXIS(3),AXIS_LENGTH,VEC_LENGTH,OFF_AXIX_COMPONENT,
     .  P_DOT_AXIS,P_LENGTH
C-----------------------------------------------
      AXIS(1:3) = P2(1:3) - P1(1:3)
      AXIS_LENGTH = VEC_LENGTH(3,AXIS)
      IF(AXIS_LENGTH == ZERO)RETURN
C
      AXIS(1:3) = AXIS(1:3) / AXIS_LENGTH
      P_DOT_AXIS = DOT_PRODUCT(P(1:3) - P1(1:3),AXIS)
C
C  If the point lies below or above the "caps" of the cylinder, we're done.
C
      IF(P_DOT_AXIS < ZERO .or. AXIS_LENGTH < P_DOT_AXIS)RETURN
C
C  Otherwise, determine the distance from P to the axis.
C
      P_LENGTH = VEC_LENGTH(3, P(1:3) - P1(1:3))
      OFF_AXIX_COMPONENT = SQRT(P_LENGTH**2 - P_DOT_AXIS**2)
      IF(OFF_AXIX_COMPONENT <= HALF*D)OK = 1
C
      RETURN
      END
C-----------------
      FUNCTION VEC_LENGTH(DIMENS,X)
C-----------------
C  VEC_LENGTH returns the Euclidean length of a vector.
C
      IMPLICIT NONE
C
      INTEGER DIMENS
      my_real
     .  VEC_LENGTH,X(DIMENS)
C-------------------------------
      VEC_LENGTH = SQRT(SUM((X(1:DIMENS))**2))
C
      RETURN
      END
Chd|====================================================================
Chd|  CHECKSPHERE                   source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        BOXTAGE                       source/model/box/bigbox.F     
Chd|        BOXTAGN                       source/model/box/bigbox.F     
Chd|        BOX_SURF_SH                   source/model/box/bigbox.F     
Chd|        ELSTAGBOX                     source/model/box/bigbox.F     
Chd|        ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|        SIMPLE_ELT_BOX                source/model/sets/simpl_elt_box.F
Chd|        SIMPLE_NODE_BOX               source/model/sets/simple_node_box.F
Chd|        SIMPLE_RBODY_BOX              source/model/sets/simple_rbody_box.F
Chd|-- calls ---------------
Chd|        INSIDE_SPHERE                 source/model/box/rdbox.F      
Chd|====================================================================
      SUBROUTINE CHECKSPHERE(XP, YP, ZP, NODIN, D, OK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OK
      my_real
     .    XP,YP,ZP,NODIN(3),D
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    P(3)
C-----------------------------------------------
      P(1) = XP
      P(2) = YP
      P(3) = ZP
C
      CALL INSIDE_SPHERE(P, NODIN, D, OK)
C
      RETURN
      END
Chd|====================================================================
Chd|  INSIDE_SPHERE                 source/model/box/rdbox.F      
Chd|-- called by -----------
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INSIDE_SPHERE(PC, P, D, OK)
C-----------------------------------------------
C    Implicit sphere equation:
C
C      SUM ( ( P(1:3) - PC(1:3) )**2 ) = D**2/4
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OK
      my_real
     .    PC(3),P(3),D
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    P1(3),P2(3),SUMA
C-----------------------------------------------
      SUMA = SUM((P(1:3) - PC(1:3))**2)
      SUMA = FOUR*SUMA
      IF(SUMA <= D**2) OK = 1
C
      RETURN
      END
