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
Chd|  DEPLA                         source/assembly/depla.F       
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DEPLA(V    ,D    ,X     ,VR   ,DR    ,
     2                 NODFT,NODLT,XDP,DDP   )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, NDDIM
C     REAL
      my_real
     .   X(3,*) ,D(3,*) ,V(3,*) ,DR(3,*)  ,VR(3,*)
      DOUBLE PRECISION
     .   XDP(3,*), DDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,N,J
      my_real
     .   VDT
      DOUBLE PRECISION VDT2
C
      IF(N2D==1)THEN
#include      "vectorize.inc"
 	DO I=NODFT,NODLT
 	  IF(X(2,I)+DT2*V(2,I)<ZERO) V(2,I)=ZERO
 	ENDDO
      ENDIF
C-------------------------------------------------------------
C         CALCUL DES  DEPLACEMENTS TRANSLATIONS PUIS ROTATIONS
C-------------------------------------------------------------
      IF (IRESP==1) THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          VDT2 = DT2*V(1,N)
          DDP(1,N) = DDP(1,N)+VDT2
          D(1,N)=DDP(1,N)
          XDP(1,N)=XDP(1,N)+VDT2
          X(1,N)=XDP(1,N)

          VDT2 = DT2*V(2,N)
          DDP(2,N) = DDP(2,N)+VDT2
          D(2,N)=DDP(2,N)
          XDP(2,N)=XDP(2,N)+VDT2
          X(2,N)=XDP(2,N)

          VDT2 = DT2*V(3,N)
          DDP(3,N) = DDP(3,N)+VDT2
          D(3,N)=DDP(3,N)
          XDP(3,N)=XDP(3,N)+VDT2
          X(3,N)=XDP(3,N)
        ENDDO
      ELSE
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          VDT = DT2*V(1,N)
          D(1,N)=D(1,N)+VDT
          X(1,N)=X(1,N)+VDT
          VDT = DT2*V(2,N)
          D(2,N)=D(2,N)+VDT
          X(2,N)=X(2,N)+VDT
          VDT = DT2*V(3,N)
          D(3,N)=D(3,N)+VDT
          X(3,N)=X(3,N)+VDT
        ENDDO
      ENDIF 
      IF((ISECUT>0 .OR. IISROT>0 .OR. IMPOSE_DR/=0 .OR. IDROT == 1) .AND. IRODDL /= 0) THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          DR(1,N)=DR(1,N)+DT2*VR(1,N)
          DR(2,N)=DR(2,N)+DT2*VR(2,N)
          DR(3,N)=DR(3,N)+DT2*VR(3,N)
        ENDDO
      ENDIF
C
      RETURN
      END
