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
Chd|  ASSPAR5                       source/assembly/asspar5.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSPAR5(NTHREAD ,NUMNOD,NODFT ,NODLT,IRODDL,
     .                  NPART,PARTFT ,PARTLT,A    ,AR    ,
     .                  PARTSAV,STIFN ,STIFR,I8A  ,I8AR  ,
     .                  I8STIFN,I8STIFR,VISCN ,I8VISCN,GREFT,
     .                  GRELT  ,GRESAV ,NGPE,NTHPART)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr18_c.inc"
C
      INTEGER NTHREAD,NUMNOD,NODFT,NODLT,IRODDL,
     .              NPART,PARTFT,PARTLT,GREFT,GRELT,NGPE,NTHPART
      INTEGER K,KN,IKN,IKN1,IKN2,I,KM,KM1,KM2,NUM7,NUM8
      integer*8 
     .  I8A(3,3,*),I8AR(3,3,*),I8STIFN(3,*),I8STIFR(3,*),
     .  I8VISCN(3,*)
      my_real 
     .  A(3,*),AR(3,*),PARTSAV(*),STIFN(*),STIFR(*),VISCN(*),GRESAV(*)
c___________________________________________________          
      double precision r8_deuxm43
      integer*8  i8_deuxp43
      data i8_deuxp43 /'80000000000'x/
      r8_deuxm43 = 1.d00 / i8_deuxp43
c___________________________________________________          
C
      NUM7 = NPSAV*NPART
      NUM8 = NGPE*NPART
C
      KN = 0
      KM = 0
      KM1 = 0
      DO K=1,NTHREAD-1
          KN = KN + NUMNOD
#include "vectorize.inc"
          DO I=NODFT,NODLT
            IKN = I+KN
            I8STIFN(1,I) = I8STIFN(1,I) + I8STIFN(1,IKN)
            I8STIFN(1,IKN) = 0
            I8STIFN(2,I) = I8STIFN(2,I) + I8STIFN(2,IKN)
            I8STIFN(2,IKN) = 0
            I8STIFN(3,I) = I8STIFN(3,I) + I8STIFN(3,IKN)
            I8STIFN(3,IKN) = 0
            I8A(1,1,I) = I8A(1,1,I) + I8A(1,1,IKN)
            I8A(1,2,I) = I8A(1,2,I) + I8A(1,2,IKN)
            I8A(1,3,I) = I8A(1,3,I) + I8A(1,3,IKN)
            I8A(1,1,IKN) = 0
            I8A(1,2,IKN) = 0
            I8A(1,3,IKN) = 0
            I8A(2,1,I) = I8A(2,1,I) + I8A(2,1,IKN)
            I8A(2,2,I) = I8A(2,2,I) + I8A(2,2,IKN)
            I8A(2,3,I) = I8A(2,3,I) + I8A(2,3,IKN)
            I8A(2,1,IKN) = 0
            I8A(2,2,IKN) = 0
            I8A(2,3,IKN) = 0
            I8A(3,1,I) = I8A(3,1,I) + I8A(3,1,IKN)
            I8A(3,2,I) = I8A(3,2,I) + I8A(3,2,IKN)
            I8A(3,3,I) = I8A(3,3,I) + I8A(3,3,IKN)
            I8A(3,1,IKN) = 0
            I8A(3,2,IKN) = 0
            I8A(3,3,IKN) = 0
          ENDDO
          IF (IRODDL/=0) THEN
#include "vectorize.inc"
           DO I=NODFT,NODLT
            IKN = I+KN
            I8STIFR(1,I) = I8STIFR(1,I) + I8STIFR(1,IKN)
            I8STIFR(1,IKN) = 0
            I8STIFR(2,I) = I8STIFR(2,I) + I8STIFR(2,IKN)
            I8STIFR(2,IKN) = 0
            I8STIFR(3,I) = I8STIFR(3,I) + I8STIFR(3,IKN)
            I8STIFR(3,IKN) = 0
            I8AR(1,1,I) = I8AR(1,1,I) + I8AR(1,1,IKN)
            I8AR(1,2,I) = I8AR(1,2,I) + I8AR(1,2,IKN)
            I8AR(1,3,I) = I8AR(1,3,I) + I8AR(1,3,IKN)
            I8AR(1,1,IKN) = 0
            I8AR(1,2,IKN) = 0
            I8AR(1,3,IKN) = 0
            I8AR(2,1,I) = I8AR(2,1,I) + I8AR(2,1,IKN)
            I8AR(2,2,I) = I8AR(2,2,I) + I8AR(2,2,IKN)
            I8AR(2,3,I) = I8AR(2,3,I) + I8AR(2,3,IKN)
            I8AR(2,1,IKN) = 0
            I8AR(2,2,IKN) = 0
            I8AR(2,3,IKN) = 0
            I8AR(3,1,I) = I8AR(3,1,I) + I8AR(3,1,IKN)
            I8AR(3,2,I) = I8AR(3,2,I) + I8AR(3,2,IKN)
            I8AR(3,3,I) = I8AR(3,3,I) + I8AR(3,3,IKN)
            I8AR(3,1,IKN) = 0
            I8AR(3,2,IKN) = 0
            I8AR(3,3,IKN) = 0
           ENDDO
          ENDIF
          IF(KDTINT/=0)THEN
#include "vectorize.inc"
           DO I=NODFT,NODLT
            IKN = I+KN
            I8VISCN(1,I) = I8VISCN(1,I) + I8VISCN(1,IKN)
            I8VISCN(1,IKN) = 0
            I8VISCN(2,I) = I8VISCN(2,I) + I8VISCN(2,IKN)
            I8VISCN(2,IKN) = 0
            I8VISCN(3,I) = I8VISCN(3,I) + I8VISCN(3,IKN)
            I8VISCN(3,IKN) = 0
           ENDDO
          ENDIF
          KM = KM + NUM7
#include "vectorize.inc"
          DO I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM)
           PARTSAV(I+KM) = 0.
          ENDDO
          KM1 = KM1 + NUM8
          IF (NTHPART > 0) THEN
#include "vectorize.inc"
            DO I=GREFT,GRELT
             GRESAV(I) = GRESAV(I) + GRESAV(I+KM1)
             GRESAV(I+KM1) = 0.
            ENDDO
          ENDIF
      ENDDO
#include "vectorize.inc"
      DO I=NODFT,NODLT
            STIFN(I) = STIFN(I) + 
     .                 I8STIFN(1,I) + r8_deuxm43 * (
     .                 I8STIFN(2,I) + r8_deuxm43 * I8STIFN(3,I))
            I8STIFN(1,I) = 0
            I8STIFN(2,I) = 0
            I8STIFN(3,I) = 0
            A(1,I) = A(1,I) + 
     .               I8A(1,1,I) + r8_deuxm43 * (
     .               I8A(2,1,I) + r8_deuxm43 * I8A(3,1,I))
            A(2,I) = A(2,I) + 
     .               I8A(1,2,I) + r8_deuxm43 * (
     .               I8A(2,2,I) + r8_deuxm43 * I8A(3,2,I))
            A(3,I) = A(3,I) +
     .               I8A(1,3,I) + r8_deuxm43 * (
     .               I8A(2,3,I) + r8_deuxm43 * I8A(3,3,I))
            I8A(1,1,I) = 0
            I8A(1,2,I) = 0
            I8A(1,3,I) = 0
            I8A(2,1,I) = 0
            I8A(2,2,I) = 0
            I8A(2,3,I) = 0
            I8A(3,1,I) = 0
            I8A(3,2,I) = 0
            I8A(3,3,I) = 0
      ENDDO
      IF (IRODDL/=0) THEN
#include "vectorize.inc"
         DO I=NODFT,NODLT
            STIFR(I) = STIFR(I) + 
     .                 I8STIFR(1,I) + r8_deuxm43 * (
     .                 I8STIFR(2,I) + r8_deuxm43 * I8STIFR(3,I))
            I8STIFR(1,I) = 0
            I8STIFR(2,I) = 0
            I8STIFR(3,I) = 0
            AR(1,I) = AR(1,I) + 
     .                I8AR(1,1,I) + r8_deuxm43 * (
     .                I8AR(2,1,I) + r8_deuxm43 * I8AR(3,1,I))
            AR(2,I) = AR(2,I) + 
     .                I8AR(1,2,I) + r8_deuxm43 * (
     .                I8AR(2,2,I) + r8_deuxm43 * I8AR(3,2,I))
            AR(3,I) = AR(3,I) +
     .                I8AR(1,3,I) + r8_deuxm43 * (
     .                I8AR(2,3,I) + r8_deuxm43 * I8AR(3,3,I))
            I8AR(1,1,I) = 0
            I8AR(1,2,I) = 0
            I8AR(1,3,I) = 0
            I8AR(2,1,I) = 0
            I8AR(2,2,I) = 0
            I8AR(2,3,I) = 0
            I8AR(3,1,I) = 0
            I8AR(3,2,I) = 0
            I8AR(3,3,I) = 0
         ENDDO
      ENDIF
      IF(KDTINT/=0)THEN
#include "vectorize.inc"
        DO I=NODFT,NODLT
            VISCN(I) = VISCN(I) + 
     .                 I8VISCN(1,I) + r8_deuxm43 * (
     .                 I8VISCN(2,I) + r8_deuxm43 * I8VISCN(3,I))
            I8VISCN(1,I) = 0
            I8VISCN(2,I) = 0
            I8VISCN(3,I) = 0
        ENDDO
      ENDIF
C
 1000 CONTINUE
      RETURN
      END
