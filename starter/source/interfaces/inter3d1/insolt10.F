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
Chd|  INSOLT10                      source/interfaces/inter3d1/insolt10.F
Chd|-- called by -----------
Chd|        I12SOL3                       source/interfaces/inter3d1/insol3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INSOLT10(IXS,IXS10,IRECT,NOINT,NRTM,ITAB,
     .                    KNOD2ELS,NOD2ELS,NTY,NSV ,MSEGTYP,ID,TITR)
C
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),IRECT(4,*),NOINT,NRTM,
     .        ITAB(*), KNOD2ELS(*), NOD2ELS(*),NTY,NSV(*),MSEGTYP(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IW,I1,I2,I3,IPERM1(6),IPERM2(6),IPERM3(6),
     .        IDBID, IAD, IEDGE, N, N10, NC(4)
      DATA NC/2,4,7,6/
      DATA IPERM1/2,4,7,2,6,7/
      DATA IPERM2/4,7,2,6,4,6/
      DATA IPERM3/1,2,3,4,5,6/
      DATA IDBID/0/
      INTEGER IDEGEN(6)
C-----------------------------------------------
C  Previous version in inint3.F
C-----------------------------------------------
C

      DO I=1,NRTM
        IF (MSEGTYP(I) /= 10) CYCLE
        DO IEDGE=1,4
          IF (NTY == 5)THEN
            I1=NSV(IRECT(IEDGE,I))
            I2=NSV(IRECT(MOD(IEDGE,4)+1,I))
          ELSE
            I1=IRECT(IEDGE,I)
            I2=IRECT(MOD(IEDGE,4)+1,I)
          ENDIF
          IF(I2==I1)CYCLE

          DO IAD=KNOD2ELS(I1)+1,KNOD2ELS(I1+1)
           N = NOD2ELS(IAD)
           IF(NUMELS8 < N .AND. N <= NUMELS8+NUMELS10)THEN
             N10=N-NUMELS8
             DO J=1,4
               IF(IXS(NC(J),N10)==I2)THEN
                 DO K=1,6
                   IF((I1==IXS(IPERM1(K),N10).AND.
     .                 I2==IXS(IPERM2(K),N10)).OR.
     .                (I2==IXS(IPERM1(K),N10).AND.
     .                 I1==IXS(IPERM2(K),N10)))THEN
                       I3=IXS10(IPERM3(K),J)
                     IF(I3/=0)THEN
               IXS10(IPERM3(K),N10)=-ABS(IXS10(IPERM3(K),N10))
                     END IF
             END IF
           END DO
               END IF
             END DO
           END IF
          ENDDO
        ENDDO
      ENDDO
C
      DO J=1,NUMELS10
        IW=0
        DO K=1,6
          I3=IXS10(IPERM3(K),J)
          IF(I3 < 0)THEN
            IW=1
            IXS10(IPERM3(K),J)=0
          END IF
        ENDDO 
        IF(IW==1)THEN
             IDEGEN=0
             IF(IXS10(1,J)/=0)THEN
              IDEGEN(1)=ITAB(IXS10(1,J))
             ENDIF
             IF(IXS10(2,J)/=0)THEN
              IDEGEN(2)=ITAB(IXS10(2,J))
             ENDIF
             IF(IXS10(3,J)/=0)THEN
              IDEGEN(3)=ITAB(IXS10(3,J))
             ENDIF
             IF(IXS10(4,J)/=0)THEN
              IDEGEN(4)=ITAB(IXS10(4,J))
             ENDIF
             IF(IXS10(5,J)/=0)THEN
              IDEGEN(5)=ITAB(IXS10(5,J))
             ENDIF
             IF(IXS10(6,J)/=0)THEN
              IDEGEN(6)=ITAB(IXS10(6,J))
             ENDIF
             CALL ANCMSG(MSGID=344,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,J),
     .                   I3=ITAB(IXS(2,J)),
     .                   I4=ITAB(IXS(4,J)),
     .                   I5=ITAB(IXS(7,J)),
     .                   I6=ITAB(IXS(6,J)),
C
     .                   I7=IDEGEN(1),
     .                   I8=IDEGEN(2),
     .                   I9=IDEGEN(3),
     .                   I10=IDEGEN(4),
     .                   I11=IDEGEN(5),
     .                   I12=IDEGEN(6))
        ENDIF
      ENDDO
C
      RETURN
      END
