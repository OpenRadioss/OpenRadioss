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
Chd|  SCONNECT_OFF                  source/elements/solid/sconnect/sconnect_off.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SCONNECT_OFF(ELBUF_STR,OFFG     ,NEL    ,NPG    ,NGL     ,
     .                        ISOLID   ,TIME     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "units_c.inc"
#include "scr17_c.inc"
#include "comlock.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER NEL,NPG,ISOLID
      INTEGER NGL(NEL)
      my_real TIME
      my_real ,DIMENSION(NEL) :: OFFG
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,IPG,IEL,NINDX,NDEL
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real ,DIMENSION(NEL) :: COUNTPG
      my_real ,DIMENSION(:) ,POINTER :: OFFL     
C=======================================================================
      DO IEL=1,NEL
        IF (OFFG(IEL) < EM01) OFFG(IEL) = ZERO  
        IF (OFFG(IEL) < ONE)   OFFG(IEL) = OFFG(IEL)*FOUR_OVER_5
      ENDDO
c
      COUNTPG(1:NEL) = ZERO
      DO IPG = 1,NPG
        OFFL  => ELBUF_STR%BUFLY(1)%LBUF(IPG,1,1)%OFF(1:NEL)
        DO IEL=1,NEL
          IF (OFFG(IEL)==ZERO .AND. OFFL(IEL)/=OFFG(IEL)) 
     .        OFFL(IEL)=OFFG(IEL)  
          COUNTPG(IEL) = COUNTPG(IEL) + OFFL(IEL)
        ENDDO         ! IEL=1,NEL
      ENDDO
c
      NINDX = 0
      DO IEL=1,NEL
        IF (OFFG(IEL) == ONE) THEN
          NDEL = NPG - COUNTPG(IEL)
          IF (NDEL >= ISOLID) THEN
          NINDX = NINDX+1
          INDX(NINDX) = IEL
          OFFG(IEL) = FOUR_OVER_5
          IDEL7NOK  = 1
          ENDIF
        ENDIF
      ENDDO         ! IEL=1,NEL          
c----------------------------------------
c     Print out
c----------------------------------------
      IF (NINDX > 0) THEN
        DO I=1,NINDX
          IEL = INDX(I)
#include "lockon.inc"
          WRITE(IOUT ,1000) NGL(IEL)
          WRITE(ISTDO,1100) NGL(IEL),TIME
#include "lockoff.inc"
        END DO
      ENDIF         
c-------------------------------
 1000 FORMAT(5X,' DELETE CONNECTION SOLID ELEMENT ',I10)
 1100 FORMAT(5X,' DELETE CONNECTION SOLID ELEMENT ',I10,' AT TIME ',1PE16.9)
c-----------
      RETURN
      END
