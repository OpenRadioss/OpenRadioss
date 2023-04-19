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
C
Chd|====================================================================
Chd|  SPHDCOD                       source/elements/sph/sphdcod.F 
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPHDCOD(NPC,ISPHIO,NOM_OPT)
      USE MESSAGE_MOD
C     OPTIONS SPH (INLET/OUTLET CONDITIONS,..):
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*), ISPHIO(NISPHIO,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, IS, J, I, K
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C=======================================================================
C     (I) TRAITEMENT DES FCTS
C=======================================================================
C     I.1 INLET/OUTLET CONDITIONS
C=======================================================================
      DO N=1,NSPHIO
        IF (ISPHIO(1,N)==4) CYCLE
        ID=NOM_OPT(1,N)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
            IF(ISPHIO(5,N)/=0)THEN
              DO J=1,NFUNCT
                IF(ISPHIO(5,N)==NPC(J)) THEN
                  ISPHIO(5,N)=J
                  GOTO 110
                ENDIF
              ENDDO
              CALL ANCMSG(MSGID=439,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISPHIO(5,N))
C              WRITE(ISTDO,*)' ** ERROR SPH INLET/OUTLET CONDITIONS'
C              WRITE(IOUT,*) ' ** ERROR SPH INLET/OUTLET CONDITIONS:',
C     .        ' FUNCTION ID FOR DENSITY:',ISPHIO(5,N)
C              IERR=IERR+1
 110          CONTINUE
            ENDIF
            IF(ISPHIO(6,N)/=0)THEN
              DO J=1,NFUNCT
                IF(ISPHIO(6,N)==NPC(J)) THEN
                  ISPHIO(6,N)=J
                  GOTO 120
                ENDIF
              ENDDO
              CALL ANCMSG(MSGID=439,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISPHIO(6,N))
C              WRITE(ISTDO,*)' ** ERROR SPH INLET/OUTLET CONDITIONS'
C              WRITE(IOUT,*) ' ** ERROR SPH INLET/OUTLET CONDITIONS:',
C     .        ' FUNCTION ID FOR PRESSURE:',ISPHIO(6,N)
C              IERR=IERR+1
 120          CONTINUE
            ENDIF
            IF(ISPHIO(7,N)/=0)THEN
              DO J=1,NFUNCT
                IF(ISPHIO(7,N)==NPC(J)) THEN
                  ISPHIO(7,N)=J
                  GOTO 130
                ENDIF
              ENDDO
              CALL ANCMSG(MSGID=439,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISPHIO(7,N))
C              WRITE(ISTDO,*)' ** ERROR SPH INLET/OUTLET CONDITIONS'
C              WRITE(IOUT,*) ' ** ERROR SPH INLET/OUTLET CONDITIONS:',
C     .        ' FUNCTION ID FOR ENERGY:',ISPHIO(7,N)
C              IERR=IERR+1
 130          CONTINUE
            ENDIF
            IF(ISPHIO(8,N)/=0)THEN
              DO J=1,NFUNCT
                IF(ISPHIO(8,N)==NPC(J)) THEN
                  ISPHIO(8,N)=J
                  GOTO 140
                ENDIF
              ENDDO
              CALL ANCMSG(MSGID=439,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISPHIO(8,N))
C              WRITE(ISTDO,*)' ** ERROR SPH INLET/OUTLET CONDITIONS'
C              WRITE(IOUT,*) ' ** ERROR SPH INLET/OUTLET CONDITIONS:',
C     .        ' NON EXISTING FUNCTION ID=',ISPHIO(8,N)
C              IERR=IERR+1
 140          CONTINUE
            ENDIF
      ENDDO
C=======================================================================
 900  CONTINUE
      RETURN
      END
