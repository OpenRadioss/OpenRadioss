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
Chd|  SIMPLE_RBODY_BOX              source/model/sets/simple_rbody_box.F
Chd|-- called by -----------
Chd|        FILL_CLAUSE_RBODY_BOX         source/model/sets/fill_clause_rbody_box.F
Chd|-- calls ---------------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE SIMPLE_RBODY_BOX(
     .                    IBOX   ,X    ,SKEW  ,
     .                    IB, ND_ARRAY,ND_SIZE,RBY_MSN)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   create node list from BOX
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     IBOX          BOX structure
C     X             Node position 
C     SKEW          SKEW array
C     NOD_ARRAY     Result list of nodes
C     ND_SIZE       number of stacked nodes
C     IB            Box to treat
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND_ARRAY(*),IB,ND_SIZE
      my_real
     .      X(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
      TYPE (BOX_) , DIMENSION(NBBOX)  :: IBOX
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY) :: RBY_MSN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,INSIDE,ISK,BOX_TYPE,NBOXBOX,IBX
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
C-----------------------------------------------
       ND_SIZE = 0
C------------
!      IBOX(IGS)%ISKBOX = ISK
!      IBOX(IGS)%NOD1 = J2(1)
!      IBOX(IGS)%NOD2 = J2(2)
C      IBOX(IGS)%NBOXBOX  : NUMBER OF SUB BOXES (BOXES OF BOXES)
!      IF(KEY(1:5) == 'RECTA')IBOX(IGS)%TYPE = 1
!      IF(KEY(1:5) == 'CYLIN')IBOX(IGS)%TYPE = 2
!      IF(KEY(1:5) == 'SPHER')IBOX(IGS)%TYPE = 3
!      IBOX(IGS)%DIAM = DIAM
!      IBOX(IGS)%X1 = XP1
!      IBOX(IGS)%Y1 = YP1
!      IBOX(IGS)%Z1 = ZP1
!      IBOX(IGS)%X2 = XP2
!      IBOX(IGS)%Y2 = YP2
!      IBOX(IGS)%Z2 = ZP2
C------------
        IBX = ABS(IB) ! a box can be have negative user_ID if within Box of Box
        ISK = IBOX(IBX)%ISKBOX
        BOX_TYPE = IBOX(IBX)%TYPE
        XP1 = IBOX(IBX)%X1
        YP1 = IBOX(IBX)%Y1
        ZP1 = IBOX(IBX)%Z1
        XP2 = IBOX(IBX)%X2
        YP2 = IBOX(IBX)%Y2
        ZP2 = IBOX(IBX)%Z2
        DIAM = IBOX(IBX)%DIAM
       !
       ! RECTA
        IF (BOX_TYPE == 1) THEN
!!          DO J=1,NUMNOD
          DO I=1,NRBODY
            J =  RBY_MSN(2,I)
            INSIDE = 0
            NODINB(1) = X(1,J)
            NODINB(2) = X(2,J)
            NODINB(3) = X(3,J)
            CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                     ISK,NODINB,SKEW,INSIDE)

            IF (INSIDE == 1) THEN 
              ND_SIZE = ND_SIZE + 1
              ND_ARRAY(ND_SIZE) = J ! add node
            ENDIF

          ENDDO ! DO I=1,NRBODY
       !
       ! CYLIN
        ELSEIF (BOX_TYPE == 2) THEN
!          DO J=1,NUMNOD
          DO I=1,NRBODY
            J =  RBY_MSN(2,I)
            INSIDE = 0
            NODINB(1) = X(1,J)
            NODINB(2) = X(2,J)
            NODINB(3) = X(3,J)
            CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                    NODINB  , DIAM, INSIDE )

            IF (INSIDE == 1) THEN 
              ND_SIZE = ND_SIZE + 1
              ND_ARRAY(ND_SIZE) = J ! add node
            ENDIF

          ENDDO ! DO I=1,NRBODY
       !
       ! SPHER
        ELSEIF (BOX_TYPE == 3) THEN
!          DO J=1,NUMNOD
          DO I=1,NRBODY
            J =  RBY_MSN(2,I)
            INSIDE = 0
            NODINB(1) = X(1,J)
            NODINB(2) = X(2,J)
            NODINB(3) = X(3,J)
            CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, INSIDE)

            IF (INSIDE == 1) THEN 
              ND_SIZE = ND_SIZE + 1
              ND_ARRAY(ND_SIZE) = J ! add node
            ENDIF

          ENDDO ! DO I=1,NRBODY
        ENDIF ! IF (BOX_TYPE == 1)
!
C---------------
      RETURN
      END
