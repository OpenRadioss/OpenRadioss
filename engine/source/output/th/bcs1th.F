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
Chd|  BCS1TH                        source/output/th/bcs1th.F     
Chd|-- called by -----------
Chd|        THBCS                         source/output/th/thbcs.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS1TH(NINDX,INDX,ISKEW,ICODT  ,
     .                  A    ,SKEW,MS   ,FTHREAC,
     .                  NODREAC,FLAG,IN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, FLAG, INDX(*), ISKEW(*), ICODT(*),NODREAC(*)
      my_real
     .   A(3,*),SKEW(LSKEW,*),MS(*),FTHREAC(6,*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K, L, ISK, LCOD
      my_real
     .   AA,FTHREAC0(6)
C-----------------------------------------------
      IF (FLAG == 0) N = 0
      IF (FLAG == 1) N = 3
C
#include "vectorize.inc"
      DO K = 1, NINDX
        L = INDX(K)
        ISK = ISKEW(L)
        LCOD= ICODT(L)
        FTHREAC0 = ZERO
C
        IF(ISK==1) THEN
C------------------
C     REPERE GLOBAL
C------------------
          IF(LCOD==1)THEN
          FTHREAC0(N+3) = - A(3,L)
          ELSEIF(LCOD==2)THEN
          FTHREAC0(N+2) = - A(2,L)
          ELSEIF(LCOD==3)THEN
          FTHREAC0(N+2) = - A(2,L)
          FTHREAC0(N+3) = - A(3,L)
          ELSEIF(LCOD==4)THEN
          FTHREAC0(N+1) = - A(1,L)
          ELSEIF(LCOD==5)THEN
          FTHREAC0(N+1) = - A(1,L)
          FTHREAC0(N+3) = - A(3,L)
          ELSEIF(LCOD==6)THEN
          FTHREAC0(N+1) = - A(1,L)
          FTHREAC0(N+2) = - A(2,L)
          ELSEIF(LCOD==7)THEN
          FTHREAC0(N+1) = - A(1,L)
          FTHREAC0(N+2) = - A(2,L)
          FTHREAC0(N+3) = - A(3,L)
          ENDIF
C
        ELSE
C-------------------
C     REPERE OBLIQUE
C-------------------
          IF(LCOD==1)THEN
          AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
          FTHREAC0(N+1)=-SKEW(7,ISK)*AA
          FTHREAC0(N+2)=-SKEW(8,ISK)*AA
          FTHREAC0(N+3)=-SKEW(9,ISK)*AA
          ELSEIF(LCOD==2)THEN
          AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
          FTHREAC0(N+1)=-SKEW(4,ISK)*AA
          FTHREAC0(N+2)=-SKEW(5,ISK)*AA
          FTHREAC0(N+3)=-SKEW(6,ISK)*AA
          ELSEIF(LCOD==3)THEN
          AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
          FTHREAC0(N+1)=-SKEW(7,ISK)*AA
          FTHREAC0(N+2)=-SKEW(8,ISK)*AA
          FTHREAC0(N+3)=-SKEW(9,ISK)*AA
          AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
          FTHREAC0(N+1)=FTHREAC0(N+1)-SKEW(4,ISK)*AA
          FTHREAC0(N+2)=FTHREAC0(N+2)-SKEW(5,ISK)*AA
          FTHREAC0(N+3)=FTHREAC0(N+3)-SKEW(6,ISK)*AA
          ELSEIF(LCOD==4)THEN
          AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
          FTHREAC0(N+1)=-SKEW(1,ISK)*AA
          FTHREAC0(N+2)=-SKEW(2,ISK)*AA
          FTHREAC0(N+3)=-SKEW(3,ISK)*AA
          ELSEIF(LCOD==5)THEN
          AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
          FTHREAC0(N+1)=-SKEW(7,ISK)*AA
          FTHREAC0(N+2)=-SKEW(8,ISK)*AA
          FTHREAC0(N+3)=-SKEW(9,ISK)*AA
          AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
          FTHREAC0(N+1)=FTHREAC0(N+1)-SKEW(1,ISK)*AA
          FTHREAC0(N+2)=FTHREAC0(N+2)-SKEW(2,ISK)*AA
          FTHREAC0(N+3)=FTHREAC0(N+3)-SKEW(3,ISK)*AA
          ELSEIF(LCOD==6)THEN
          AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
          FTHREAC0(N+1)=-SKEW(1,ISK)*AA
          FTHREAC0(N+2)=-SKEW(2,ISK)*AA
          FTHREAC0(N+3)=-SKEW(3,ISK)*AA
          AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
          FTHREAC0(N+1)=FTHREAC0(N+1)-SKEW(4,ISK)*AA
          FTHREAC0(N+2)=FTHREAC0(N+2)-SKEW(5,ISK)*AA
          FTHREAC0(N+3)=FTHREAC0(N+3)-SKEW(6,ISK)*AA
          ELSEIF(LCOD==7)THEN
          FTHREAC0(N+1) = - A(1,L)
          FTHREAC0(N+2) = - A(2,L)
          FTHREAC0(N+3) = - A(3,L)
          ENDIF
C
        ENDIF
C
        IF (FLAG == 0)THEN
          FTHREAC(1,NODREAC(L)) = FTHREAC(1,NODREAC(L))
     &                           + FTHREAC0(1) * MS(L) * DT12
          FTHREAC(2,NODREAC(L)) = FTHREAC(2,NODREAC(L))
     &                           + FTHREAC0(2) * MS(L) * DT12
          FTHREAC(3,NODREAC(L)) = FTHREAC(3,NODREAC(L))
     &                           + FTHREAC0(3) * MS(L) * DT12
        ELSE
          FTHREAC(4,NODREAC(L)) = FTHREAC(4,NODREAC(L))
     &                           + FTHREAC0(4) * IN(L) * DT12
          FTHREAC(5,NODREAC(L)) = FTHREAC(5,NODREAC(L))
     &                           + FTHREAC0(5) * IN(L) * DT12
          FTHREAC(6,NODREAC(L)) = FTHREAC(6,NODREAC(L))
     &                           + FTHREAC0(6) * IN(L) * DT12
        ENDIF
C
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  BCS1AN                        source/output/th/bcs1th.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS1AN(NINDX,INDX,ISKEW,ICODT  ,
     .                  A    ,SKEW,MS   ,FANREAC,
     .                  FLAG,IN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, FLAG, INDX(*), ISKEW(*), ICODT(*)
      my_real
     .   A(3,*),SKEW(LSKEW,*),MS(*),FANREAC(6,*),IN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K, L, ISK, LCOD
      my_real
     .   AA,FANREAC0(6)
C-----------------------------------------------
C
      IF (FLAG == 0) N = 0
      IF (FLAG == 1) N = 3
C
#include "vectorize.inc"
      DO K = 1, NINDX
        L = INDX(K)
        ISK = ISKEW(L)
        LCOD = ICODT(L)
        FANREAC0 = ZERO
C
        IF(ISK==1) THEN
C------------------
C     REPERE GLOBAL
C------------------
          IF(LCOD==1)THEN
          FANREAC0(N+3) = - A(3,L)
          ELSEIF(LCOD==2)THEN
          FANREAC0(N+2) = - A(2,L)
          ELSEIF(LCOD==3)THEN
          FANREAC0(N+2) = - A(2,L)
          FANREAC0(N+3) = - A(3,L)
          ELSEIF(LCOD==4)THEN
          FANREAC0(N+1) = - A(1,L)
          ELSEIF(LCOD==5)THEN
          FANREAC0(N+1) = - A(1,L)
          FANREAC0(N+3) = - A(3,L)
          ELSEIF(LCOD==6)THEN
          FANREAC0(N+1) = - A(1,L)
          FANREAC0(N+2) = - A(2,L)
          ELSEIF(LCOD==7)THEN
          FANREAC0(N+1) = - A(1,L)
          FANREAC0(N+2) = - A(2,L)
          FANREAC0(N+3) = - A(3,L)
          ENDIF
C
        ELSE
C-------------------
C     REPERE OBLIQUE
C-------------------
          IF(LCOD==1)THEN
          AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
          FANREAC0(N+1)=-SKEW(7,ISK)*AA
          FANREAC0(N+2)=-SKEW(8,ISK)*AA
          FANREAC0(N+3)=-SKEW(9,ISK)*AA
          ELSEIF(LCOD==2)THEN
          AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
          FANREAC0(N+1)=-SKEW(4,ISK)*AA
          FANREAC0(N+2)=-SKEW(5,ISK)*AA
          FANREAC0(N+3)=-SKEW(6,ISK)*AA
          ELSEIF(LCOD==3)THEN
          AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
          FANREAC0(N+1)=-SKEW(7,ISK)*AA
          FANREAC0(N+2)=-SKEW(8,ISK)*AA
          FANREAC0(N+3)=-SKEW(9,ISK)*AA
          AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
          FANREAC0(N+1)=FANREAC0(N+1)-SKEW(4,ISK)*AA
          FANREAC0(N+2)=FANREAC0(N+2)-SKEW(5,ISK)*AA
          FANREAC0(N+3)=FANREAC0(N+3)-SKEW(6,ISK)*AA
          ELSEIF(LCOD==4)THEN
          AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
          FANREAC0(N+1)=-SKEW(1,ISK)*AA
          FANREAC0(N+2)=-SKEW(2,ISK)*AA
          FANREAC0(N+3)=-SKEW(3,ISK)*AA
          ELSEIF(LCOD==5)THEN
          AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
          FANREAC0(N+1)=-SKEW(7,ISK)*AA
          FANREAC0(N+2)=-SKEW(8,ISK)*AA
          FANREAC0(N+3)=-SKEW(9,ISK)*AA
          AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
          FANREAC0(N+1)=FANREAC0(N+1)-SKEW(1,ISK)*AA
          FANREAC0(N+2)=FANREAC0(N+2)-SKEW(2,ISK)*AA
          FANREAC0(N+3)=FANREAC0(N+3)-SKEW(3,ISK)*AA
          ELSEIF(LCOD==6)THEN
          AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
          FANREAC0(N+1)=-SKEW(1,ISK)*AA
          FANREAC0(N+2)=-SKEW(2,ISK)*AA
          FANREAC0(N+3)=-SKEW(3,ISK)*AA
          AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
          FANREAC0(N+1)=FANREAC0(N+1)-SKEW(4,ISK)*AA
          FANREAC0(N+2)=FANREAC0(N+2)-SKEW(5,ISK)*AA
          FANREAC0(N+3)=FANREAC0(N+3)-SKEW(6,ISK)*AA
          ELSEIF(LCOD==7)THEN
          FANREAC0(N+1) = - A(1,L)
          FANREAC0(N+2) = - A(2,L)
          FANREAC0(N+3) = - A(3,L)
          ENDIF
C
        ENDIF
C
        IF (FLAG == 0)THEN
          FANREAC(1,L) = FANREAC(1,L) + FANREAC0(1) * MS(L)
          FANREAC(2,L) = FANREAC(2,L) + FANREAC0(2) * MS(L)
          FANREAC(3,L) = FANREAC(3,L) + FANREAC0(3) * MS(L)
        ELSE
          FANREAC(4,L) = FANREAC(4,L) + FANREAC0(4) * IN(L)
          FANREAC(5,L) = FANREAC(5,L) + FANREAC0(5) * IN(L)
          FANREAC(6,L) = FANREAC(6,L) + FANREAC0(6) * IN(L)
        ENDIF
C
      ENDDO
C
      RETURN
      END
