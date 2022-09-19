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
Chd|  ANIM_RESET_INDEX_ALL          source/output/anim/reader/anim_reset_index_all.F
Chd|-- called by -----------
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE ANIM_RESET_INDEX_ALL()
C-----------------------------------------------
C     This routine is resetting counters in cas of /STRESS/ALL, /STRAIN/ALL, etc
C     The correct indexes wrt integration points or layers of all actual elements 
C     will be rebuilt 
C       - just after in the starter
C       - after reading the restart file in the engine.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr25_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IUS, IDX
C-----------------------------------------------
      IF (ISTRESALL == 1) THEN
        DO I=1,100
          ANIM_CT(100+I) = 0
        ENDDO
      ENDIF
!---
      IF (ISTRESFULL == 1) THEN
!   ...STRESS/N1/ALL
        DO I=1,100
          IF (ANIM_STRESS(I) == 1) THEN
            DO J=1,10
              IUS = 10*I+J
              ANIM_CT(600+IUS) = 0
            ENDDO
          ENDIF
        ENDDO
      ELSEIF (ISTRESFULL == 2) THEN
!   ...STRESS/ALL/ALL
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            ANIM_CT(600+IUS) = 0
          ENDDO
        ENDDO
      ENDIF ! IF (ISTRESFULL == 1)
!---
      IF (ISTRAINFULL == 1) THEN
!   ...STRAIN/N1/ALL
        IDX = 1810 + 3*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_STRAIN(I) > 0) THEN
              ANIM_CT(IDX + IUS) = 0
            ENDIF
          ENDDO
        ENDDO
      ELSEIF (ISTRAINFULL == 2) THEN
!   ...STRAIN/ALL/ALL
        IDX = 1810 + 3*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            ANIM_CT(IDX + IUS) = 0
          ENDDO
        ENDDO
      ENDIF ! IF (ISTRAINFULL == 1)
!---
      IF (IEPSDOFULL == 1) THEN
!   ...EPSPDOT/N1/ALL
        IDX = 2820 + 3*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_EPSDOT(I) > 0) THEN
              ANIM_CT(IDX + IUS) = 0
            ENDIF
          ENDDO
        ENDDO
      ELSEIF (IEPSDOFULL == 2) THEN
!   ...EPSPDOT/ALL/ALL
        IDX = 2820 + 3*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            ANIM_CT(IDX + IUS) = 0
          ENDDO
        ENDDO
      ENDIF ! IF (IEPSDOFULL == 1)
!---
      IF (IEPSPALL == 1) THEN
        DO I=1,100
          ANIM_CE(2041+I) = 0
        ENDDO
      ENDIF
!---
      IF (IEPSPNLALL == 1) THEN
        IDX = 4*MX_PLY_ANIM + 14567 + 2
        DO I=1,11
          ANIM_CE(IDX+I) = 0
        ENDDO
      ENDIF
!---
      IF (IEPSDNLALL == 1) THEN
        IDX = 4*MX_PLY_ANIM + 14581 + 2
        DO I=1,11
          ANIM_CE(IDX+I) = 0
        ENDDO
      ENDIF
!---
      IF (IEPSPFULL == 1) THEN
!   ...EPSP/N1/ALL
        DO I=1,100
          IF (ANIM_EPSP(I) == 1) THEN
            DO J=1,10
              IUS = 10*I+J
              ANIM_CE(10877+IUS) = 0
            ENDDO
          ENDIF
        ENDDO
      ELSEIF (IEPSPFULL == 2) THEN
!   ...EPSP/ALL/ALL
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            ANIM_CE(10877+IUS) = 0
          ENDDO
        ENDDO
      ENDIF ! IF (IEPSPFULL == 1)
C       
      IF (IWPLAALL == 1) THEN
        IDX =   13247 + 4*MX_PLY_ANIM
        DO I=1,100
          ANIM_CE(IDX + I) = 0
        ENDDO
      ENDIF
!---
      IF (IWPLAFULL == 1) THEN
!   ...WPLA/N1/ALL
        IDX =   13547 + 4*MX_PLY_ANIM
        DO I=1,100
          IF (ANIM_WPLA(I) == 1) THEN
            DO J=1,10
              IUS = 10*(I-1) + J
              ANIM_CE(IDX + IUS) = 0
            ENDDO
          ENDIF
        ENDDO
      ELSEIF (IWPLAFULL == 2) THEN
!   ...WPLA/ALL/ALL
        IDX =   13547 + 4*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*(I-1)+J
            ANIM_CE(IDX + IUS) = 0
          ENDDO
        ENDDO
      ENDIF ! IF (IWPLAFULL == 1)
!---
      IF (IDAMAFULL == 1) THEN
        IDX = 11931 + 4*MX_PLY_ANIM
        DO I=1,100
          IF (ANIM_DAMA(I) == 1) THEN
            DO J=1,10
              IUS = 10*I+J
              ANIM_CE(IDX + 300 + IUS) = 0
            ENDDO
          ENDIF
        ENDDO
      ELSEIF (IDAMAFULL == 2) THEN
        IDX = 11931 + 4*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            ANIM_CE(IDX + 300 + IUS) = 0
          ENDDO
        ENDDO
      ENDIF ! IF (IDAMAFULL == 1)
C-----------------------------------------------
      RETURN
      END
