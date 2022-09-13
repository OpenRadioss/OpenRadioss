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
Chd|  SPMALLB3                      source/elements/sph/spmall3.F 
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMALLB3(OFFG ,OFF ,KXSP, SPH2SOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER KXSP(NISP,*),SPH2SOL(*)
      my_real
     .   OFFG(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF(ISMSTR==1.OR.ISMSTR==3)THEN
        DO I=LFT,LLT
          IF(OFFG(I)>ZERO)OFFG(I)=TWO 
        ENDDO
      ENDIF
C
      IF (NSPHSOL/=0) THEN
C-- Sol2sph
        DO I=LFT,LLT
          IF (SPH2SOL(I)==0) THEN 
            IF(OFFG(I)/=ZERO.AND.OFF(I)==ZERO)THEN
              KXSP(2,I)=0
#include "lockon.inc"
              ISPHBUC =1
              IDEL7NOK=1
#include "lockoff.inc"
            END IF
            IF(OFF(I)<ONE) OFFG(I) = OFF(I)
          ENDIF
        ENDDO
C
      ELSE
C
        DO I=LFT,LLT
          IF(OFFG(I)/=ZERO.AND.OFF(I)==ZERO)THEN
            KXSP(2,I)=0
#include "lockon.inc"
            ISPHBUC =1
            IDEL7NOK=1
#include "lockoff.inc"
          END IF
          IF(OFF(I)<ONE) OFFG(I) = OFF(I)
        ENDDO
C
      ENDIF
C
      RETURN
      END
