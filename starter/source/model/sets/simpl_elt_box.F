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
Chd|  SIMPLE_ELT_BOX                source/model/sets/simpl_elt_box.F
Chd|-- called by -----------
Chd|        FILL_CLAUSE_ELT_BOX           source/model/sets/fill_clause_elt_box.F
Chd|-- calls ---------------
Chd|        CHECKCYL                      source/model/box/rdbox.F      
Chd|        CHECKPARA                     source/model/box/rdbox.F      
Chd|        CHECKSPHERE                   source/model/box/rdbox.F      
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE SIMPLE_ELT_BOX(
     .                    IBOX    ,X      ,SKEW     ,IB      ,ELT_ARRAY,
     .                    ELT_SIZE,NIX    ,IX       ,NIX1    ,IPARTE   ,
     .                    IPART   ,ELTREE ,KLEVTREE ,KELTREE ,NUMEL    ,
     .                    ITYPE   )
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
C     ELT_ARRAY     Result list of elems
C     ELT_SIZE      number of stacked elems
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
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ELT_ARRAY(*),IPARTE(*),IPART(LIPART1,*),IX(NIX,*),ELTREE(KELTREE,*)
      INTEGER IB,ELT_SIZE,NIX,NIX1,KLEVTREE,KELTREE,NUMEL,ITYPE
      my_real
     .      X(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
      TYPE (BOX_) , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,INSIDE,ISK,BOX_TYPE,NBOXBOX,IBX,COUNT,IP,
     .    NLEV,MY_LEV
      my_real
     .    XP1,YP1,ZP1,XP2,YP2,ZP2,DIAM,NODINB(3)
C-----------------------------------------------
       ELT_SIZE = 0
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
        IF (NADMESH == 0) THEN
          !
          ! RECTA
          IF (BOX_TYPE == 1) THEN
            DO J=1,NUMEL
              COUNT=0
              DO K=2,NIX1+1
                INSIDE = 0
                I=IX(K,J)
                NODINB(1) = X(1,I)
                NODINB(2) = X(2,I)
                NODINB(3) = X(3,I)
                CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                         ISK,NODINB,SKEW,INSIDE)
                IF (INSIDE == 1) COUNT = COUNT + 1
              ENDDO
              IF ( ITYPE == 1 ) THEN
                !
                ! all elt nodes inide box
                IF (COUNT == NIX1) THEN
                  ELT_SIZE = ELT_SIZE + 1
                  ELT_ARRAY(ELT_SIZE) = J ! add elt
                ENDIF
              ELSEIF ( ITYPE == 2 ) THEN
                !
                ! at least one elt node inide box
                IF (INSIDE > 0 ) THEN
                  ELT_SIZE = ELT_SIZE + 1
                  ELT_ARRAY(ELT_SIZE) = J ! add elt
                ENDIF
              ENDIF ! IF ( ITYPE == 1 )
            ENDDO !DO J=1,NUMEL
          !
          ! CYLIN
          ELSEIF (BOX_TYPE == 2) THEN
            DO J=1,NUMEL
              COUNT=0
              DO K=2,NIX1+1
                INSIDE = 0
                I=IX(K,J)
                NODINB(1) = X(1,I)
                NODINB(2) = X(2,I)
                NODINB(3) = X(3,I)
                CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                        NODINB  , DIAM, INSIDE )
                IF (INSIDE == 1) COUNT = COUNT + 1
              ENDDO
              IF ( ITYPE == 1 ) THEN
                !
                ! all elt nodes inide box
                IF (COUNT == NIX1) THEN
                  ELT_SIZE = ELT_SIZE + 1
                  ELT_ARRAY(ELT_SIZE) = J ! add elt
                ENDIF
              ELSEIF ( ITYPE == 2 ) THEN
                !
                ! at least one elt node inide box
                IF (INSIDE > 0 ) THEN
                  ELT_SIZE = ELT_SIZE + 1
                  ELT_ARRAY(ELT_SIZE) = J ! add elt
                ENDIF
              ENDIF ! IF ( ITYPE == 1 )
            ENDDO !DO J=1,NUMEL
           !
           ! SPHER
          ELSEIF (BOX_TYPE == 3) THEN
            DO J=1,NUMEL
              COUNT=0
              DO K=2,NIX1+1
                INSIDE = 0
                I=IX(K,J)
                NODINB(1) = X(1,I)
                NODINB(2) = X(2,I)
                NODINB(3) = X(3,I)
                CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, INSIDE)
                IF (INSIDE == 1) COUNT = COUNT + 1
              ENDDO
              IF ( ITYPE == 1 ) THEN
                !
                ! all elt nodes inide box
                IF (COUNT == NIX1) THEN
                  ELT_SIZE = ELT_SIZE + 1
                  ELT_ARRAY(ELT_SIZE) = J ! add elt
                ENDIF
              ELSEIF ( ITYPE == 2 ) THEN
                !
                ! at least one elt node inide box
                IF (INSIDE > 0 ) THEN
                  ELT_SIZE = ELT_SIZE + 1
                  ELT_ARRAY(ELT_SIZE) = J ! add elt
                ENDIF
              ENDIF ! IF ( ITYPE == 1 )
            ENDDO ! DO J=1,NUMEL
          ENDIF ! IF (BOX_TYPE == 1)
!
        ELSEIF (NADMESH /= 0) THEN
!
          !
          ! RECTA
          IF (BOX_TYPE == 1) THEN
            DO J=1,NUMEL
              COUNT=0
              DO K=2,NIX1+1
                INSIDE = 0
                I=IX(K,J)
                NODINB(1) = X(1,I)
                NODINB(2) = X(2,I)
                NODINB(3) = X(3,I)
                CALL CHECKPARA(XP1,YP1,ZP1,XP2,YP2,ZP2,
     .                         ISK,NODINB,SKEW,INSIDE)
                IF (INSIDE == 1) COUNT = COUNT + 1
              ENDDO
              IF ( ITYPE == 1 ) THEN
                !
                ! all elt nodes inide box
                IF (COUNT == NIX1) THEN
                  IP=IPARTE(J)
                  NLEV  =IPART(10,IP)
                  MY_LEV=ELTREE(KLEVTREE,J)
                  IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                  IF (MY_LEV==NLEV) THEN
                    ELT_SIZE = ELT_SIZE + 1
                    ELT_ARRAY(ELT_SIZE) = J ! add elt
                  ENDIF
                ENDIF
              ELSEIF ( ITYPE == 2 ) THEN
                !
                ! at least one elt node inide box
                IF (INSIDE > 0 ) THEN
                  IP=IPARTE(J)
                  NLEV  =IPART(10,IP)
                  MY_LEV=ELTREE(KLEVTREE,J)
                  IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                  IF (MY_LEV==NLEV) THEN
                    ELT_SIZE = ELT_SIZE + 1
                    ELT_ARRAY(ELT_SIZE) = J ! add elt
                  ENDIF
                ENDIF
              ENDIF ! IF ( ITYPE == 1 )
            ENDDO ! DO J=1,NUMEL
          !
          ! CYLIN
          ELSEIF (BOX_TYPE == 2) THEN
            DO J=1,NUMEL
              COUNT=0
              DO K=2,NIX1+1
                INSIDE = 0
                I=IX(K,J)
                NODINB(1) = X(1,I)
                NODINB(2) = X(2,I)
                NODINB(3) = X(3,I)
                CALL CHECKCYL(XP1, YP1, ZP1 , XP2, YP2, ZP2,
     .                        NODINB  , DIAM, INSIDE )
                IF (INSIDE == 1) COUNT = COUNT + 1
              ENDDO
              IF ( ITYPE == 1 ) THEN
                !
                ! all elt nodes inide box
                IF (COUNT == NIX1) THEN
                  IP=IPARTE(J)
                  NLEV  =IPART(10,IP)
                  MY_LEV=ELTREE(KLEVTREE,J)
                  IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                  IF (MY_LEV==NLEV) THEN
                    ELT_SIZE = ELT_SIZE + 1
                    ELT_ARRAY(ELT_SIZE) = J ! add elt
                  ENDIF
                ENDIF
              ELSEIF ( ITYPE == 2 ) THEN
                !
                ! at least one elt node inide box
                IF (INSIDE > 0 ) THEN
                  IP=IPARTE(J)
                  NLEV  =IPART(10,IP)
                  MY_LEV=ELTREE(KLEVTREE,J)
                  IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                  IF (MY_LEV==NLEV) THEN
                    ELT_SIZE = ELT_SIZE + 1
                    ELT_ARRAY(ELT_SIZE) = J ! add elt
                  ENDIF
                ENDIF
              ENDIF ! IF ( ITYPE == 1 )
            ENDDO ! DO J=1,NUMEL
          !
          ! SPHER
          ELSEIF (BOX_TYPE == 3) THEN
            DO J=1,NUMEL
              COUNT=0
              DO K=2,NIX1+1
                INSIDE = 0
                I=IX(K,J)
                NODINB(1) = X(1,I)
                NODINB(2) = X(2,I)
                NODINB(3) = X(3,I)
                CALL CHECKSPHERE(XP1, YP1, ZP1, NODINB, DIAM, INSIDE)
                IF (INSIDE == 1) COUNT = COUNT + 1
              ENDDO
              IF ( ITYPE == 1 ) THEN
                !
                ! all elt nodes inide box
                IF (COUNT == NIX1) THEN
                  IP=IPARTE(J)
                  NLEV  =IPART(10,IP)
                  MY_LEV=ELTREE(KLEVTREE,J)
                  IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                  IF (MY_LEV==NLEV) THEN
                    ELT_SIZE = ELT_SIZE + 1
                    ELT_ARRAY(ELT_SIZE) = J ! add elt
                  ENDIF
                ENDIF
              ELSEIF ( ITYPE == 2 ) THEN
                !
                ! at least one elt node inide box
                IF (INSIDE > 0 ) THEN
                  IP=IPARTE(J)
                  NLEV  =IPART(10,IP)
                  MY_LEV=ELTREE(KLEVTREE,J)
                  IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
                  IF (MY_LEV==NLEV) THEN
                    ELT_SIZE = ELT_SIZE + 1
                    ELT_ARRAY(ELT_SIZE) = J ! add elt
                  ENDIF
                ENDIF
              ENDIF ! IF ( ITYPE == 1 )
            ENDDO ! DO J=1,NUMEL
          ENDIF ! IF (BOX_TYPE == 1)
        ENDIF ! IF (NADMESH == 0) THEN
C---------------
      RETURN
      END
