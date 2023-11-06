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
Chd|  ELEGROR                       source/groups/elegror.F
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ELEGROR(IGU,IGRELE,NGRELE,MOT,
     .                  IBUF,NIB,MESS,NFRAM,TAGELEMS,ISTYP,
     .                  ID,TITR)
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IGU,NIB,NGRELE,NFRAM,
     .          IBUF(NIB,*),TAGELEMS(*),ISTYP
        INTEGER ID
C-----------------------------------------------
        TYPE (GROUP_)  ,DIMENSION(NGRELE)   :: IGRELE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        CHARACTER MESS*40,MOT*4
        INTEGER I,J,K,IG,L,IE,IADV
        CHARACTER*nchartitle,
     .     TITR
C-----------------------------------------------
        ELEGROR=0
        IF(IGU==0)RETURN
C
        IG=0
        DO I=1,NGRELE
          IF(IGRELE(I)%ID==IGU)IG=I
        ENDDO
C
C     IF(IG==0)THEN
C        CALL ANCMSG(MSGID=171,
C    .               MSGTYPE=MSGERROR,
C    .               ANMODE=ANINFO,
C    .               C1= MESS,
C    .               I1= ID,
C    .               C2= TITR,
C    .               C3= MOT,
C    .               I2=IGU)
C           RETURN
C     ENDIF
C
        IF (NFRAM > 0 .OR. ISTYP >= 1) THEN
          DO L=1,IGRELE(IG)%NENTITY
            IE=IGRELE(IG)%ENTITY(L)
            IF ( TAGELEMS(IE) /= 0 ) ELEGROR = ELEGROR + 1
          ENDDO
        ELSE
          ELEGROR=IGRELE(IG)%NENTITY
        ENDIF
        J = 0
        IF (NFRAM > 0 .OR. ISTYP >= 1) THEN
          DO I=1,IGRELE(IG)%NENTITY
            IF (TAGELEMS(IGRELE(IG)%ENTITY(I)) == 1 ) THEN
              J = J + 1
              IBUF(1,J)=IGRELE(IG)%ENTITY(I)
            ENDIF
          ENDDO
        ELSE
          DO I=1,IGRELE(IG)%NENTITY
            IBUF(1,I)=IGRELE(IG)%ENTITY(I)
          ENDDO
        ENDIF
C
        RETURN
      END
