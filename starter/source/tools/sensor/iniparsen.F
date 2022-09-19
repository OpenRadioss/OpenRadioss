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
Chd|  INIPARSEN                     source/tools/sensor/iniparsen.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIPARSEN(NBR_SENS,STABSEN,LSENSOR,TABSENSOR,FBSAV6)
      USE MESSAGE_MOD
C     Traitement special de sensors 11/12 avec Parith/on
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NBR_SENS,LSENSOR(NSENSI,*),STABSEN
      INTEGER, INTENT(INOUT) :: TABSENSOR(STABSEN)
      DOUBLE PRECISION, INTENT(INOUT) :: FBSAV6(12,6,NBR_SENS)
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NOSYS,ISECT,IINT,IRWAL,IRBOD,COMPTEUR
      INTEGER, DIMENSION(0:NSECT+NINTER+NINTSUB+NRWALL+NRBODY) :: TABLOCAL
C---------------------------------------------
C     Tabsensor(i) pointe vers les interfaces/sections/rwalls/rbodies concernees 
C     par les sensors type force
C---------------------------------------------
      TABLOCAL(:)=0
      DO I=1,NSENSOR
        IF (LSENSOR(2,I) == 12 ) THEN         ! sensort type section
          ISECT=LSENSOR(4,I)
          TABLOCAL(ISECT)=I
        ELSEIF (LSENSOR(2,I) == 6) THEN       ! sensort type interface
          IINT=LSENSOR(4,I) + NSECT
          TABLOCAL(IINT)=I
        ELSEIF (LSENSOR(2,I) == 7) THEN       ! sensort type rwall
          IRWAL=LSENSOR(4,I) + NSECT + NINTER+ NINTSUB
          TABLOCAL(IRWAL)=I
        ELSEIF (LSENSOR(2,I) == 11) THEN      ! sensort type rbody
          IRBOD=LSENSOR(4,I) + NSECT + NINTER+ NINTSUB + NRWALL
          TABLOCAL(IRBOD)=I
        ELSEIF (LSENSOR(2,I) == 13) THEN      ! sensort type work
          ISECT = LSENSOR(6,I)
          IF (ISECT > 0) TABLOCAL(ISECT)=I
c
          IINT = LSENSOR(7,I) + NSECT
          IF (LSENSOR(7,I) > 0) TABLOCAL(IINT)=I     ! interface
          IINT = LSENSOR(8,I) + NSECT
          IF (LSENSOR(8,I) > 0) TABLOCAL(IINT)=I     ! sub-interface
c
          IRWAL = LSENSOR(9,I) + NSECT + NINTER+ NINTSUB
          IF (LSENSOR(9,I) > 0) TABLOCAL(IRWAL)=I
c
          IRBOD = LSENSOR(10,I) + NSECT + NINTER+ NINTSUB + NRWALL
          IF (LSENSOR(10,I) > 0) TABLOCAL(IRBOD)=I
        ENDIF
      ENDDO
c------------
      NOSYS=1      ! No systeme section,interface,rwall, rbody
      DO WHILE(TABLOCAL(NOSYS)==0)
        NOSYS=NOSYS+1
      ENDDO
C
      DO I=1,NOSYS
        TABSENSOR(I)=TABLOCAL(NOSYS)
      ENDDO
C
      COMPTEUR=1
      DO I=NOSYS,NSECT+NINTER+NINTSUB+NRWALL+NRBODY
        IF(TABLOCAL(I)==0) THEN
          TABSENSOR(I+1)=TABSENSOR(I)
        ELSE
          TABSENSOR(I+1)=TABSENSOR(I)+COMPTEUR
          COMPTEUR=COMPTEUR+1
        ENDIF
      ENDDO
C
      DO I=1,NBR_SENS
        DO J=1,6
          DO K=1,12
            FBSAV6(K,J,I) = ZERO
          ENDDO
        ENDDO
      ENDDO
C------------
      RETURN
      END
