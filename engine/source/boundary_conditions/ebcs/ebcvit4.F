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
Chd|  EBCVIT4                       source/boundary_conditions/ebcs/ebcvit4.F
Chd|-- called by -----------
Chd|        EBCCLAP                       source/boundary_conditions/ebcs/ebcclap.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE EBCVIT4(V, A, LISTE, NOD, FV, RESO, LA, EBCS)
      USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LISTE(*),NOD
      my_real
     .       V(3,*),A(3,*),LA(3,NOD),RESO(3,NOD),FV(*)
      TYPE(t_ebcs_vel), INTENT(IN) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,IVX,IVY,IVZ
      my_real
     .     C,LCAR,ALP,ALPDT,AX,AY,AZ,VIMP,VX,VY,VZ
C
      IVX = EBCS%ivx
      IVY = EBCS%ivy
      IVZ = EBCS%ivz
      IF(IVX>0)THEN
        VX=EBCS%vx*FV(IVX)
      ELSE
        VX=EBCS%vx
      ENDIF
      IF(IVY>0)THEN
        VY=EBCS%vy*FV(IVY)
      ELSE
        VY=EBCS%vy
      ENDIF
      IF(IVZ>0)THEN
        VZ=EBCS%vz*FV(IVZ)
      ELSE
        VZ=EBCS%vz
      ENDIF
      C=EBCS%c
      LCAR=EBCS%lcar
      ALP=ZERO
      IF(LCAR>ZERO)ALP=C/LCAR
      ALPDT=ALP*DT1
C
      IF(TT==ZERO)THEN
     	  DO I=1,NOD
	    N=LISTE(I)
	    RESO(1,I)=A(1,N)
	    RESO(2,I)=A(2,N)
	    RESO(3,I)=A(3,N)
     	  ENDDO
      ENDIF
C
c      write(6,*)'ebcvit4',VX,VY,VZ
      IF(ALP>ZERO)THEN
        DO I=1,NOD
          N=LISTE(I)
c          write(6,*)'ebcvit5 N',N,' RESO',RESO(3,I),'A',A(3,N)
          AX=RESO(1,I)+ALPDT*(A(1,N)-RESO(1,I))
          AY=RESO(2,I)+ALPDT*(A(2,N)-RESO(2,I))
          AZ=RESO(3,I)+ALPDT*(A(3,N)-RESO(3,I))
C memorisation de la reaction
          RESO(1,I)=AX
          RESO(2,I)=AY
          RESO(3,I)=AZ
C
          A(1,N)=A(1,N)-AX+ALP*(VX-V(1,N))
          A(2,N)=A(2,N)-AY+ALP*(VY-V(2,N))
          A(3,N)=A(3,N)-AZ+ALP*(VZ-V(3,N))
        ENDDO
      ELSE
        DO I=1,NOD
          N=LISTE(I)
          A(1,N)=ZERO
          A(2,N)=ZERO
          A(3,N)=ZERO
          V(1,N)=VX
          V(2,N)=VY
          V(3,N)=VZ
        ENDDO
      ENDIF
      RETURN
      END
