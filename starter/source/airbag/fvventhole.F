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
Chd|  FVVENTHOLE                    source/airbag/fvventhole.F    
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVVENTHOLE(IBUF  , ELEM   , IBAGHOL,
     .                      NVENT , IGRSURF,
     .                      ITAGEL, NN     , NEL    , 
     .                      ID    , TAGVENT, TITR   ,
     .                      ELTG, NB_NODE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,NEL), IBAGHOL(NIBHOL,*), 
     .        NVENT,
     .        NN, NEL, ID, TAGVENT(NB_NODE)
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: ITAGEL
      INTEGER NB_NODE
      INTEGER, DIMENSION(NEL), INTENT(IN) :: ELTG
      CHARACTER*nchartitle,
     .        TITR
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITABINV(NB_NODE), ITAGVENT(NN),
     .        ISU, NELSU, IEL, NG1, NG2, NG3, NG4, N1, N2, N3, N4,
     .        IVENT, NALL, NN1, NN2, IERROR
      INTEGER K, ITY, KEL, IVENTYP, IERROR1
      CHARACTER*17, CVENTYP
C----------------------------------------------------
C  TAG Vent Holes and Porous Surfaces nodes : TAGVENT
C  Test if the nodes belong to the airbag
C----------------------------------------------------
      DO I=1,NB_NODE
         ITABINV(I)=0
      ENDDO
C
      DO I=1,NN
         II=IBUF(I)
         ITABINV(II)=I
         ITAGVENT(I)=0
      ENDDO
C     
      DO IVENT=1,NVENT
         ISU=IBAGHOL(2,IVENT)
         IF(ISU == 0) CYCLE
         IVENTYP=IBAGHOL(13,IVENT)
         IF(IVENTYP == 0) CVENTYP='VENT HOLE SURFACE'
         IF(IVENTYP == 1) CVENTYP='POROUS SURFACE'
         NELSU=IGRSURF(ISU)%NSEG
         DO I=1,NELSU
            NG1 = IGRSURF(ISU)%NODES(I,1)
            NG2 = IGRSURF(ISU)%NODES(I,2)
            NG3 = IGRSURF(ISU)%NODES(I,3)
            NG4 = IGRSURF(ISU)%NODES(I,4)
            N1=ITABINV(NG1)
            N2=ITABINV(NG2)
            N3=ITABINV(NG3)
            N4=ITABINV(NG4)
            IERROR = 0
            IF(N1==0.AND.TAGVENT(NG1)==0) THEN
              IERROR = 1
            ELSE
              IF (N1 /= 0) ITAGVENT(N1)=IVENT
            END IF
            IF(N2==0.AND.TAGVENT(NG2)==0) THEN
              IERROR = 1
            ELSE
              IF (N2 /= 0) ITAGVENT(N2)=IVENT
            END IF
            IF(N3==0.AND.TAGVENT(NG3)==0) THEN
              IERROR = 1
            ELSE
              IF (N3 /= 0) ITAGVENT(N3)=IVENT
            END IF
            IF(N4==0.AND.TAGVENT(NG4)==0) THEN
              IERROR = 1
            ELSE
              IF (N4 /= 0) ITAGVENT(N4)=IVENT
            END IF
         ENDDO
         IF(IERROR==1)THEN
            CALL ANCMSG(MSGID=632,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  C2=CVENTYP,
     .                  I2=IGRSURF(ISU)%ID)
         ENDIF
      ENDDO
C-----------------------------------------
C  TAG Vent hole and Porous Surface ITAGEL
C-----------------------------------------
      DO IVENT=1,NVENT
         ISU=IBAGHOL(2,IVENT)
         IF(ISU == 0) CYCLE
         IVENTYP=IBAGHOL(13,IVENT)
         IF(IVENTYP == 0) CVENTYP='VENT HOLE     '
         IF(IVENTYP == 1) CVENTYP='POROUS SURFACE'
         NELSU=IGRSURF(ISU)%NSEG
         IERROR =0
         IERROR1=0
         DO I=1,NELSU
            ITY = IGRSURF(ISU)%ELTYP(I)
            KEL = IGRSURF(ISU)%ELEM(I)
            IF(ITY == 7) KEL=KEL+NUMELC
            IF(ITY == 3 .OR. ITY == 7) THEN
              DO IEL=1,NEL
                 K=ELTG(IEL)
                 IF(K == KEL) THEN
                   IF(ITAGEL(IEL) == 0) THEN
C Element IEL belongs to the current vent hole
                     ITAGEL(IEL)=-IVENT
                   ELSEIF(ITAGEL(IEL) > 0) THEN
C Element IEL is an injector
                     IERROR=IERROR+1
                   ELSEIF(ITAGEL(IEL) < 0) THEN
C Element IEL belongs to a previous vent hole
                     IERROR1=IERROR1+1
                   ENDIF                    
                 ENDIF                 
              ENDDO
            ELSE
              DO IEL=1,NEL
                 N1=ELEM(1,IEL)
                 N2=ELEM(2,IEL)
                 N3=ELEM(3,IEL)
                 NALL=ITAGVENT(N1)*ITAGVENT(N2)*ITAGVENT(N3)
                 IF (NALL/=0) THEN
                    NN1=ITAGVENT(N2)-ITAGVENT(N1)
                    NN2=ITAGVENT(N3)-ITAGVENT(N1)
                    IF (NN1 == 0.AND.NN2 == 0) THEN
                      IF(ITAGEL(IEL) == 0) THEN
C Element IEL belongs to the current vent hole
                        ITAGEL(IEL)=-IVENT
                      ELSEIF(ITAGEL(IEL) > 0) THEN
C Element IEL is an injector
                        IERROR=IERROR+1
                      ELSEIF(ITAGEL(IEL) < 0) THEN
C Element IEL belongs to a previous vent hole
                        IERROR1=IERROR1+1
                      ENDIF
                    ENDIF
                 ENDIF 
              ENDDO
            ENDIF
         ENDDO
         IF(IERROR  > 0)THEN
            CALL ANCMSG(MSGID=1045,MSGTYPE=MSGWARNING,ANMODE=ANINFO,
     .                  I1=ID,I2=IERROR,C1=TITR,C2=CVENTYP,I3=IVENT)
         ENDIF
         IF(IERROR1 > 0)THEN
            CALL ANCMSG(MSGID=1180,MSGTYPE=MSGWARNING,ANMODE=ANINFO,
     .                  I1=ID,I2=IERROR1,C1=TITR,C2=CVENTYP,I3=IVENT)
         ENDIF
      ENDDO
C
      RETURN
      END
