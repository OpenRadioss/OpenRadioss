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
Chd|  VITESSE                       source/assembly/vitesse.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VITESSE(
     1                   A    , AR   , V    ,VR    ,FZERO,
     2                   NODFT, NODLT, ITAB ,NALE)
C----6------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, NDDIM,ITAB(*),NALE(*)
      my_real
     .   FZERO(3,NUMNOD) ,A(3,NUMNOD) ,V(3,NUMNOD) ,AR(3,NUMNOD)  ,VR(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      
#include      "vectorize.inc"
      DO N=NODFT,NODLT
        V(1,N)=V(1,N)+DT12*A(1,N)
        V(2,N)=V(2,N)+DT12*A(2,N)
        V(3,N)=V(3,N)+DT12*A(3,N)
        A(1,N)=ZERO
        A(2,N)=ZERO
        A(3,N)=ZERO     
      ENDDO

      IF(ISIGI==2.OR.ISIGI==4) THEN
#include      "vectorize.inc"
        DO I=NODFT,NODLT
          A(1,I)=FZERO(1,I)
          A(2,I)=FZERO(2,I)
          A(3,I)=FZERO(3,I)
        ENDDO
      ENDIF
      IF(IRODDL/=0) THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          VR(1,N)=VR(1,N)+DT12*AR(1,N)
          VR(2,N)=VR(2,N)+DT12*AR(2,N)
          VR(3,N)=VR(3,N)+DT12*AR(3,N)
          AR(1,N)=ZERO
          AR(2,N)=ZERO
          AR(3,N)=ZERO      
        ENDDO
      ENDIF

      !------------------------------------------------!
      !      FVM DOES NOT NEED ACCEL AND VEL           !
      !------------------------------------------------!  
      IF(INT22>0) THEN  !FVM
#include      "vectorize.inc"
        DO N=NODFT,NODLT
         IF(NALE(N)/=0)THEN
          V(1,N) =ZERO
          V(2,N) =ZERO
          V(3,N) =ZERO
          VR(1,N)=ZERO
          VR(2,N)=ZERO
          VR(3,N)=ZERO
         ENDIF
        ENDDO
      ENDIF
      
      RETURN
      END
