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
Chd|  FVVENTHOLEINT                 source/airbag/fvventholeint.F 
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE FVVENTHOLEINT(IBUF  , ELEM   , IBAGHOL,
     .                         NVENT , IGRSURF,
     .                         ITAGEL, NN     , NEL, NB_NODE) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,*), IBAGHOL(NIBHOL,*), 
     .        NVENT,
     .        ITAGEL(*), NN, NEL, NB_NODE
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITABINV(NB_NODE), ITAGVENT(NN),
     .        ISU, NELSU, IEL, NG1, NG2, NG3, NG4, N1, N2, N3, N4,
     .        IVENT, NALL, NN1, NN2, IVENTYP
C----------------------------------------------------
      DO I=1,NB_NODE
         ITABINV(I)=0
      ENDDO
C
      DO I=1,NN
         II=IBUF(I)
         ITABINV(II)=I
         ITAGVENT(I)=0
      ENDDO
C     
      DO IVENT=1,NVENT
         ISU=IBAGHOL(2,IVENT)
         IF(ISU == 0) CYCLE
         IVENTYP=IBAGHOL(13,IVENT)
         IF(IVENTYP == 0) CYCLE
         NELSU=IGRSURF(ISU)%NSEG
         DO I=1,NELSU
            NG1 = IGRSURF(ISU)%NODES(I,1)
            NG2 = IGRSURF(ISU)%NODES(I,2)
            NG3 = IGRSURF(ISU)%NODES(I,3)
            NG4 = IGRSURF(ISU)%NODES(I,4)
            N1=ITABINV(NG1)
            N2=ITABINV(NG2)
            N3=ITABINV(NG3)
            N4=ITABINV(NG4)
            ITAGVENT(N1)=IVENT
            ITAGVENT(N2)=IVENT
            ITAGVENT(N3)=IVENT
            ITAGVENT(N4)=IVENT
         ENDDO
      ENDDO
C			
      DO IEL=1,NEL
         IF(ITAGEL(IEL) > 0) CYCLE   ! injecteur
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         NALL=ITAGVENT(N1)*ITAGVENT(N2)*ITAGVENT(N3)
         IF(NALL.NE.0) THEN
            NN1=ITAGVENT(N2)-ITAGVENT(N1)
            NN2=ITAGVENT(N3)-ITAGVENT(N1)
            IF (NN1 == 0 .AND. NN2 == 0) ITAGEL(IEL)=-ITAGVENT(N1)					
         ENDIF 
      ENDDO
C
      RETURN
      END
