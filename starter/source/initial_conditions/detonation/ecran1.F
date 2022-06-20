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
Chd|  ECRAN1                        source/initial_conditions/detonation/ecran1.F
Chd|-- called by -----------
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN2T                        source/initial_conditions/detonation/m5in2t.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        IOMBR                         source/initial_conditions/detonation/iombr.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ECRAN1(DETONATORS,N1,NP1,X,VDET)
C-----------------------------------------------
      USE MESSAGE_MOD
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real X(3,NUMNOD)
      my_real,INTENT(IN)::VDET
      TYPE(DETONATOR_STRUCT_),TARGET ::DETONATORS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr11_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,POINTER,DIMENSION(:) :: IECR, IORDR, IFLG
      my_real,POINTER,DIMENSION(:) :: DTIME
      INTEGER,INTENT(IN) :: N1,NP1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, J, JJ, JJJ,NPE
      my_real DTOMIN, DDMX, D2
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL :: IOMBR
C-----------------------------------------------
      
      NPE=DETONATORS%NPE
      IECR => DETONATORS%IECRAN(N1:)
      IORDR => DETONATORS%IECRAN(NP1:) 
      IFLG => DETONATORS%IECRAN(NP1+NPE:) 
      DTIME => DETONATORS%DECRAN(1:)    
C---------------------------------------
C     COMPUTING DETONATION TIMES OF THE SCREEN POINTS
C---------------------------------------
C     1) LIGHTENING FROM DETONATION ORIGIN
C         check if the target can be directly lightened from detonation origin.
C            yes if no crossing with the screen (IOMBR returns 0)
C             no otherwise (IOMBR return 1)
C---------------------------------------
      DTOMIN=EP20
      IORDR(1)=0
      
      DO I=1,NPE
        DDMX  =EP20
        IFLG(I)=0
        II=IECR(I)
        YL=X(2,II)
        ZL=X(3,II)
        IF(IOMBR(DETONATORS,X,IECR,DDMX,VDET) == 0)THEN   !Vdet is optionnal velocity from origin (default is material velocity VDTO)
          DTIME(I)=DTO !DT0 is lightening time from function IOMBR
          IF(DTIME(I) <= DTOMIN)THEN
            IORDR(1)=I
            DTOMIN=DTIME(I)
          ENDIF
        ELSE
          DTIME(I)=EP20
        ENDIF
      END DO !I=1,NPE
      
      IF(IORDR(1) == 0)THEN
        CALL ANCMSG(MSGID=422,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO)
      ENDIF
      
C-----------------------------------------
C     2) LIGHTENING SCREEN POINTS FROM SCREEN POINTS THEMSELVES
C-----------------------------------------
      DO J=1,NPE-1
        JJ=IORDR(J)
        JJJ=IECR(JJ)
        !define J as a new source. (J from N-1, N-2, ..., 1 ; first point is the nearest one from detonation origin)
        YD=X(2,JJJ)
        ZD=X(3,JJJ)
        DTO0=DTIME(JJ)
        IFLG(JJ)=1
        DTOMIN=EP20
        DO I=1,NPE
          ! check if I can be lightened from J (I from 1,2,...,N ; first node is the farthest one from detonation origin)
          IF(IFLG(I) == 1)CYCLE !I
          DDMX  =EP20
          II=IECR(I)
          YL=X(2,II)
          ZL=X(3,II)
          IF(IABS(I-JJ) == 1)THEN
            D2 =(YD-YL)**2+(ZD-ZL)**2
            DTO=DTO0+SQRT(D2)/VDTO
            DTIME(I)= MIN(DTIME(I),DTO)
          ELSEIF(IOMBR(DETONATORS,X,IECR,DDMX,VDTO) == 0)THEN
            DTIME(I)= MIN(DTIME(I),DTO)
          ENDIF
          IF(DTIME(I) <= DTOMIN)THEN
            IORDR(J+1)=I
            DTOMIN=DTIME(I)
          ENDIF
        END DO !I=1,NPE
      END DO !J=1,NPE-1

      RETURN
      END
