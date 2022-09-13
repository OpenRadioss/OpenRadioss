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
Chd|  BCSN                          source/constraints/general/bcs/bcsn.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCSN(ICODE,ICODT,ICODR,PARTS0,PARTSAV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PARTSAV(NPSAV,*),PARTS0(*)
      INTEGER ICODE(*), ICODT(*), ICODR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N, IC, IC1, IC2, IC3, IC4
C-----------------------------------------------
C
      DO 10 N=1,NUMNOD
      ICODE(N)=ICODE(N)+512*(ICODT(N)-ICODE(N)/512)
   10 CONTINUE
C
      IF(IRODDL>0)THEN
       DO 20 N=1,NUMNOD
       IC=ICODE(N)
       IC1=IC/512
       IC2=(IC-512*IC1)/64
       IC3=(IC-512*IC1-64*IC2)/8
       IC4=(IC-512*IC1-64*IC2-8*IC3)
       IC2=ICODR(N)
       ICODE(N)=512*IC1+64*IC2+8*IC3+IC4
   20  CONTINUE
      END IF
C
      IF(IALE>0) THEN
      DO 30 N=1,NUMNOD
       IC=ICODE(N)
       IC1=IC/512
       IC2=(IC-512*IC1)/64
       IC3=(IC-512*IC1-64*IC2)/8
       IC4=(IC-512*IC1-64*IC2-8*IC3)
       IC3=ICODT(N+NUMNOD)
       ICODE(N)=512*IC1+64*IC2+8*IC3+IC4
   30 CONTINUE
      DO 40 N=1,NUMNOD
       IC=ICODE(N)
       IC1=IC/512
       IC2=(IC-512*IC1)/64
       IC3=(IC-512*IC1-64*IC2)/8
       IC4=(IC-512*IC1-64*IC2-8*IC3)
       IC4=ICODT(N+2*NUMNOD)
       ICODE(N)=512*IC1+64*IC2+8*IC3+IC4
   40 CONTINUE
      END IF
C
      DO I=1,NPART
        PARTS0(I)=PARTSAV(8,I)
      ENDDO
C
      RETURN
      END
