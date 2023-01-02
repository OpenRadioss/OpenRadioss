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
Chd|  INIGUSER                      source/system/iniguser.F      
Chd|-- called by -----------
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|====================================================================
      SUBROUTINE INIGUSER(BUFGEO,IGEO,IPM,NPC)
      USE MESSAGE_MOD
      USE TABLE_GLOB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IGEO(NPROPGI,*),IPM(NPROPMI,*)
      DOUBLE PRECISION
     .      BUFGEO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGTYP
      INTEGER IADBUF,IADFUN,IADMAT,IADPID,IADTAB
      INTEGER I,J, II, NJFUNC, ILAW,
     .        JFUNC, NUPARAM, NUVAR,IINU,JTAB
      INTEGER NJMAT, JMAT,NJPID,JPID,NJTAB
      CHARACTER*nchartitle,
     .   TITR
C=======================================================================
      DO II=1,NUMGEO
        IGTYP = IGEO(11,II)
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,II),LTITR)
C       Cf generic data storage wrt so-called "user properties"
C       IF(IGTYP > 19 .AND. IGTYP /= 26 .AND. IGTYP /= 50 
C   .                .AND. IGTYP /= 51 .AND. IGTYP /= 52)THEN
        NJFUNC = IGEO(53,II)
        NJMAT  = IGEO(54,II)
        NJPID  = IGEO(55,II)
        NJTAB  = IGEO(56,II)
        IADBUF = IGEO(57,II)
        IADFUN = IGEO(58,II)
        IADMAT = IGEO(59,II)
        IADPID = IGEO(60,II)
        IADTAB = IGEO(61,II)
C
        DO I=IADFUN,IADFUN+NJFUNC-1
          JFUNC = NINT(BUFGEO(I))
          BUFGEO(I) = ZERO
          DO J=1,NFUNCT
            IF(NPC(J) == JFUNC)BUFGEO(I)=J
          ENDDO
          IF(JFUNC /= 0.AND.BUFGEO(I) == ZERO)THEN
             CALL ANCMSG(MSGID=373,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,II),C1=TITR,
     .                   C2='FUNCTION',
     .                   I2=JFUNC)
          ENDIF
        ENDDO
        DO I=IADMAT,IADMAT+NJMAT-1
          JMAT = BUFGEO(I)
          BUFGEO(I) = ZERO
          DO J=1,NUMMAT
            IF(IPM(1,J) == JMAT) BUFGEO(I)=J
          ENDDO
          IF(JMAT /= 0.AND.BUFGEO(I) == ZERO)THEN
             CALL ANCMSG(MSGID=373,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,II),C1=TITR,
     .                   C2='MATERIAL',
     .                   I2=JMAT)
          ENDIF
        ENDDO
        DO I=IADPID,IADPID+NJPID-1
          JPID=NINT(BUFGEO(I))
          BUFGEO(I) = ZERO
          DO J=1,NUMGEO
            IF(IGEO(1,J) == JPID)BUFGEO(I)=J
          ENDDO
          IF(JPID /= 0.AND.BUFGEO(I) == ZERO)THEN
             CALL ANCMSG(MSGID=373,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,II),C1=TITR,
     .                   C2='PROPERTY',
     .                   I2=JPID)
          ENDIF
        ENDDO
        DO I=IADTAB,IADTAB+NJTAB-1
          JTAB=NINT(BUFGEO(I))
          BUFGEO(I) = ZERO
          DO J=1,NTABLE
            IF(TABLE(J)%NOTABLE == JTAB)BUFGEO(I)=J
          ENDDO
          IF(JTAB /= 0.AND.BUFGEO(I) == ZERO)THEN
             CALL ANCMSG(MSGID=373,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,II),C1=TITR,
     .                   C2='TABLE',
     .                   I2=JTAB)
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
