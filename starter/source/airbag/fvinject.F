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
Chd|  FVINJECT                      source/airbag/fvinject.F      
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVINJECT(IBUF  , ELEM   , IBAGJET, 
     .                    NJET  , IGRSURF, 
     .                    ITAGEL, NN     , NEL    , 
     .                    ID    , TAGVENT, TITR   ,
     .                    ITAB  )
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
      INTEGER IBUF(*), ELEM(3,NEL), IBAGJET(NIBJET,*), 
     .        NJET, 
     .        NN, NEL, ID, TAGVENT(NUMNOD),ITAB(NUMNOD)
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: ITAGEL
      CHARACTER*nchartitle,
     .        TITR
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITABINV(NUMNOD), ITAGINJ(NN), IINJ, JINJ,
     .        ISU, NELSU, IAD, IEL, NG1, NG2, NG3, NG4, N1, N2, N3, N4,
     .        NALL, NN1, NN2, IERROR, INOD
C
      DO I=1,NUMNOD
         ITABINV(I)=0
      ENDDO
C
      DO I=1,NN
         II=IBUF(I)
         IF(ITABINV(II)==0)ITABINV(II)=I
C         ITAGINJ(I)=0
      ENDDO
C
      DO IINJ=1,NJET
         ISU=IBAGJET(14,IINJ)
         IF(ISU==0) CYCLE
         NELSU=IGRSURF(ISU)%NSEG
         INOD=0
         DO I = 1, NN
            ITAGINJ(I) = 0
         ENDDO
         DO IEL=1,NELSU
            NG1 = IGRSURF(ISU)%NODES(IEL,1)
            NG2 = IGRSURF(ISU)%NODES(IEL,2)
            NG3 = IGRSURF(ISU)%NODES(IEL,3)
            NG4 = IGRSURF(ISU)%NODES(IEL,4)
            N1=ITABINV(NG1)
            N2=ITABINV(NG2)
            N3=ITABINV(NG3)
            N4=ITABINV(NG4)
            IERROR = 0
            IF(N1==0.AND.TAGVENT(NG1)==0) THEN
              IERROR = 1
            ELSE
              JINJ=ITAGINJ(N1)
              IF(JINJ > 0 .AND. JINJ < IINJ) THEN
                  INOD=INOD+1
                  CALL ANCMSG(MSGID=1046,MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=ID,I2=ITAB(NG1),I3=JINJ)
              ENDIF
              ITAGINJ(N1)=IINJ
            END IF
            IF(N2==0.AND.TAGVENT(NG2)==0) THEN
              IERROR = 1
            ELSE
              JINJ=ITAGINJ(N2)
              IF(JINJ > 0 .AND. JINJ < IINJ) THEN
                  INOD=INOD+1
                  CALL ANCMSG(MSGID=1046,MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=ID,I2=ITAB(NG2),I3=JINJ)
              ENDIF
              ITAGINJ(N2)=IINJ
            END IF
            IF(N3==0.AND.TAGVENT(NG3)==0) THEN
              IERROR = 1
            ELSE
              JINJ=ITAGINJ(N3)
              IF(JINJ > 0 .AND. JINJ < IINJ) THEN
                  INOD=INOD+1
                  CALL ANCMSG(MSGID=1046,MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=ID,I2=ITAB(NG3),I3=JINJ)
              ENDIF
              ITAGINJ(N3)=IINJ
            END IF
            IF(N4==0.AND.TAGVENT(NG4)==0) THEN
              IERROR = 1
            ELSE
              JINJ=ITAGINJ(N4)
              IF(JINJ > 0 .AND. JINJ < IINJ) THEN
                  INOD=INOD+1
                  CALL ANCMSG(MSGID=1046,MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=ID,I2=ITAB(NG4),I3=JINJ)
              ENDIF
              ITAGINJ(N4)=IINJ
            END IF
            IF(IERROR==1)THEN
              CALL ANCMSG(MSGID=632,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                    I1=ID,I2=IINJ,C1=TITR,C2='INFLATOR')
            ENDIF                
         ENDDO
         IF(INOD > 0) THEN
           CALL ANCMSG(MSGID=1047,MSGTYPE=MSGWARNING,ANMODE=ANINFO,
     .                 I1=ID,I2=IINJ,I3=INOD)
         ENDIF
C
         DO IEL=1,NEL
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            NALL=ITAGINJ(N1)*ITAGINJ(N2)*ITAGINJ(N3)
            IF (NALL/=0) THEN
               NN1=ITAGINJ(N2)-ITAGINJ(N1)
               NN2=ITAGINJ(N3)-ITAGINJ(N1)
               IF (NN1==0.AND.NN2==0) THEN
                  IF (ITAGEL(IEL) > 0) THEN
                     CALL ANCMSG(MSGID=1616,MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,I2=ITAGEL(IEL),I3=ITAGINJ(N1))
                  ENDIF
                  ITAGEL(IEL)=ITAGINJ(N1)
               ENDIF
            ENDIF
         ENDDO
      ENDDO

C
      RETURN
      END
