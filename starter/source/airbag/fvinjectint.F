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
Chd|  FVINJECTINT                   source/airbag/fvinjectint.F   
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE FVINJECTINT(IBUF  , ELEM   , IBAGJET, 
     .                       NJET  , IGRSURF,
     .                       ITAGEL, NN     , NEL, NB_NODE, MINUS_SIGN_REVERSE)
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
      INTEGER IBUF(*), ELEM(3,*), IBAGJET(NIBJET,*), 
     .        NJET, 
     .        ITAGEL(*), NN, NEL, NB_NODE, MINUS_SIGN_REVERSE(NEL)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITABINV(NB_NODE), ITAGINJ(NN), IREVERSED(NN), IINJ,
     .        ISU, NELSU, IEL, NG1, NG2, NG3, NG4, N1, N2, N3, N4,
     .        NALL, NN1, NN2, REV

      INTEGER :: INV3(3)
      DATA INV3/3,2,1/
C
      DO I=1,NB_NODE
         ITABINV(I)=0
      ENDDO
C
      DO I=1,NN
         II=IBUF(I)
         ITABINV(II)=I
         ITAGINJ(I)=0
      ENDDO
C
      DO IINJ=1,NJET
         ISU=IBAGJET(14,IINJ)
         IF(ISU==0) CYCLE
         NELSU=IGRSURF(ISU)%NSEG
         DO IEL=1,NELSU
            NG1 = IGRSURF(ISU)%NODES(IEL,1)
            NG2 = IGRSURF(ISU)%NODES(IEL,2)
            NG3 = IGRSURF(ISU)%NODES(IEL,3)
            NG4 = IGRSURF(ISU)%NODES(IEL,4)
            N1=ITABINV(NG1)
            N2=ITABINV(NG2)
            N3=ITABINV(NG3)
            N4=ITABINV(NG4)
            ITAGINJ(N1)=IINJ
            ITAGINJ(N2)=IINJ
            ITAGINJ(N3)=IINJ
            ITAGINJ(N4)=IINJ
            REV = IGRSURF(ISU)%REVERSED(IEL)
            IREVERSED(N1) = REV
            IREVERSED(N2) = REV
            IREVERSED(N3) = REV
            IREVERSED(N4) = REV
         ENDDO
      ENDDO
C
      DO IEL=1,NEL
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         ITAGEL(IEL)=0
         NALL=ITAGINJ(N1)*ITAGINJ(N2)*ITAGINJ(N3)
         IF (NALL/=0) THEN
            NN1=ITAGINJ(N2)-ITAGINJ(N1)
            NN2=ITAGINJ(N3)-ITAGINJ(N1)
            IF (NN1==0.AND.NN2==0) THEN 
               ITAGEL(IEL)=ITAGINJ(N1)
               IF(IREVERSED(N1) + IREVERSED(N2) + IREVERSED(N3) == 3) THEN
                  ELEM(1,IEL) = N3
                  ELEM(3,IEL) = N1
                  MINUS_SIGN_REVERSE(IEL) = 1
               ENDIF
            ENDIF
         ENDIF
      ENDDO
C
      RETURN
      END
