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
Chd|  ACCELE                        source/assembly/accele.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ACCELE(
     1                  A       ,AR    ,V    ,MS      ,IN   ,
     2                  NODFT   ,NODLT ,ITAB ,NALE   ,MS_2D  ,
     3                  NPBY   )
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "inter22.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, ITAB(*),NALE(*),NPBY(NNPBY,*)
      my_real
     .   A(3,NUMNOD) ,
     .   V(3,NUMNOD) ,
     .   AR(3,NUMNOD),
     .   MS(NUMNOD)  ,
     .   IN(*),MS_2D(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J,K,N,I,MSR
      my_real
     .         RTMP
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       CALCUL DES  ACCELERATIONS (TRANSLATIONS) !
      !------------------------------------------------!
!#include      "vectorize.inc"

      IF(N2D == 0) THEN
        DO N=NODFT,NODLT
          IF(MS(N)>ZERO) THEN
            RTMP = ONE / MS(N)
            A(1,N) = A(1,N) * RTMP
            A(2,N) = A(2,N) * RTMP
            A(3,N) = A(3,N) * RTMP
          ELSE
            A(1,N) = ZERO
            A(2,N) = ZERO
            A(3,N) = ZERO
            IF(NMULT/=0) THEN
              V(1,N) = ZERO
              V(2,N) = ZERO
              V(3,N) = ZERO
            ENDIF
          ENDIF
        ENDDO
       ELSE
        DO I=1,NRBODY
          MSR = NPBY(1,I)
          MS(MSR) = MS_2D(MSR)
        ENDDO
        DO N=NODFT,NODLT
          IF(MS(N)>ZERO) THEN
            RTMP = ONE / MS(N)
            A(1,N) = A(1,N) * RTMP
            A(2,N) = A(2,N) * RTMP
            A(3,N) = A(3,N) * RTMP
          ELSE
            A(1,N) = ZERO
            A(2,N) = ZERO
            A(3,N) = ZERO
            IF(NMULT/=0) THEN
              V(1,N) = ZERO
              V(2,N) = ZERO
              V(3,N) = ZERO
            ENDIF
          ENDIF
        ENDDO
       ENDIF

      !------------------------------------------------!
      !       CALCUL DES  ACCELERATIONS (ROTATIONS)    !
      !------------------------------------------------!
      IF (IRODDL/=0) THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          IF(IN(N)>ZERO) THEN
            RTMP = ONE / IN(N)
            AR(1,N) = AR(1,N) * RTMP
            AR(2,N) = AR(2,N) * RTMP
            AR(3,N) = AR(3,N) * RTMP
          ELSE
            AR(1,N) = ZERO
            AR(2,N) = ZERO
            AR(3,N) = ZERO      
          ENDIF
        ENDDO
      ENDIF

      !------------------------------------------------!
      !      FVM DOES NOT NEED ACCEL AND VEL           !
      !------------------------------------------------!      
      IF(INT22>0)THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          IF(NALE(N)/=0)THEN
            A( 1:3,N) = ZERO
            AR(1:3,N) = ZERO
            MS(N)     = ZERO
          ENDIF
        ENDDO     
      ENDIF
      
      !------------------------------------------------!

      
      
      RETURN
      END
