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
Chd|  SDEFO3                        source/elements/solid/solide/sdefo3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|====================================================================
      SUBROUTINE SDEFO3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   WXX, WYY, WZZ, VXR,VYR,VZR,IXS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEANIM_MOD !,ONLY:FANI_CELL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), VXR(*),VYR(*),VZR(*)
      INTEGER, INTENT(IN) :: IXS(NIXS,NUMELS)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, IDX
      my_real
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46, DT1D2, DT1D
      my_real 
     .   PXX2,PYY2,PZZ2,PXX2P,PYY2P,PZZ2P,AAA,BBB,FAC,
     .   EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ
C=======================================================================
      IF(INTEG8*JEUL.EQ.0)THEN
       DO I=LFT,LLT
        VX17=VX1(I)-VX7(I)
        VX28=VX2(I)-VX8(I)
        VX35=VX3(I)-VX5(I)
        VX46=VX4(I)-VX6(I)
        VY17=VY1(I)-VY7(I)
        VY28=VY2(I)-VY8(I)
        VY35=VY3(I)-VY5(I)
        VY46=VY4(I)-VY6(I)
        VZ17=VZ1(I)-VZ7(I)
        VZ28=VZ2(I)-VZ8(I)
        VZ35=VZ3(I)-VZ5(I)
        VZ46=VZ4(I)-VZ6(I)

        DXX(I)=PX1(I)*VX17+PX2(I)*VX28+
     .  	PX3(I)*VX35+PX4(I)*VX46
        DYY(I)=PY1(I)*VY17+PY2(I)*VY28+
     .  	PY3(I)*VY35+PY4(I)*VY46
        DZZ(I)=PZ1(I)*VZ17+PZ2(I)*VZ28+
     .  	PZ3(I)*VZ35+PZ4(I)*VZ46
        DXY(I)=PY1(I)*VX17+PY2(I)*VX28+
     .  	PY3(I)*VX35+PY4(I)*VX46
        DXZ(I)=PZ1(I)*VX17+PZ2(I)*VX28+
     .  	PZ3(I)*VX35+PZ4(I)*VX46
        DYX(I)=PX1(I)*VY17+PX2(I)*VY28+
     .  	PX3(I)*VY35+PX4(I)*VY46
        DYZ(I)=PZ1(I)*VY17+PZ2(I)*VY28+
     .  	PZ3(I)*VY35+PZ4(I)*VY46
        DZX(I)=PX1(I)*VZ17+PX2(I)*VZ28+
     .  	PX3(I)*VZ35+PX4(I)*VZ46
        DZY(I)=PY1(I)*VZ17+PY2(I)*VZ28+
     .  	PY3(I)*VZ35+PY4(I)*VZ46
       ENDDO

      ELSE
       DO I=LFT,LLT
        DXX(I)=PX1(I)*VX1(I)+PX2(I)*VX2(I)+PX3(I)*VX3(I)+PX4(I)*VX4(I)
     +        +PX5(I)*VX5(I)+PX6(I)*VX6(I)+PX7(I)*VX7(I)+PX8(I)*VX8(I)
        DYY(I)=PY1(I)*VY1(I)+PY2(I)*VY2(I)+PY3(I)*VY3(I)+PY4(I)*VY4(I)
     +        +PY5(I)*VY5(I)+PY6(I)*VY6(I)+PY7(I)*VY7(I)+PY8(I)*VY8(I)
        DZZ(I)=PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
     +        +PZ5(I)*VZ5(I)+PZ6(I)*VZ6(I)+PZ7(I)*VZ7(I)+PZ8(I)*VZ8(I)
        DXY(I)=PY1(I)*VX1(I)+PY2(I)*VX2(I)+PY3(I)*VX3(I)+PY4(I)*VX4(I)
     +        +PY5(I)*VX5(I)+PY6(I)*VX6(I)+PY7(I)*VX7(I)+PY8(I)*VX8(I)
        DXZ(I)=PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
     +        +PZ5(I)*VX5(I)+PZ6(I)*VX6(I)+PZ7(I)*VX7(I)+PZ8(I)*VX8(I)
        DYX(I)=PX1(I)*VY1(I)+PX2(I)*VY2(I)+PX3(I)*VY3(I)+PX4(I)*VY4(I)
     +        +PX5(I)*VY5(I)+PX6(I)*VY6(I)+PX7(I)*VY7(I)+PX8(I)*VY8(I)
        DYZ(I)=PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
     +        +PZ5(I)*VY5(I)+PZ6(I)*VY6(I)+PZ7(I)*VY7(I)+PZ8(I)*VY8(I)
        DZX(I)=PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
     +        +PX5(I)*VZ5(I)+PX6(I)*VZ6(I)+PX7(I)*VZ7(I)+PX8(I)*VZ8(I)
        DZY(I)=PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
     +        +PY5(I)*VZ5(I)+PY6(I)*VZ6(I)+PY7(I)*VZ7(I)+PY8(I)*VZ8(I)
       ENDDO
      ENDIF

      DT1D2=HALF*DT1
      IF (ISCAU.GT.0.OR.IMP_LR.GT.0)DT1D2=DT1

      IF (JCVT /= 0) THEN
        IF (ISMSTR == 11) THEN
          DO I=LFT,LLT
            D4(I)   = DXY(I)+DYX(I)
            D5(I)   = DYZ(I)+DZY(I)
            D6(I)   = DXZ(I)+DZX(I)
            WZZ(I)=DT1D2*(DYX(I)-DXY(I))
            WYY(I)=DT1D2*(DXZ(I)-DZX(I))
            WXX(I)=DT1D2*(DZY(I)-DYZ(I))
          ENDDO
        ELSE
          DO I=LFT,LLT
            WXX(I)=ZERO
            WYY(I)=ZERO
            WZZ(I)=ZERO
          ENDDO
          IF (IMPL_S.EQ.0.OR.(IDYNA.GT.0.AND.ISMDISP.EQ.0)) THEN
            DO I=LFT,LLT
C
             EXX=DXX(I)
             EYY=DYY(I)
             EZZ=DZZ(I)
             EXY=DXY(I)
             EYX=DYX(I)
             EXZ=DXZ(I)
             EZX=DZX(I)
             EYZ=DYZ(I)
             EZY=DZY(I)
C
             DXX(I)  = DXX(I)-DT1D2*(EXX*EXX+EYX*EYX+EZX*EZX)
             DYY(I)  = DYY(I)-DT1D2*(EYY*EYY+EZY*EZY+EXY*EXY)
             DZZ(I)  = DZZ(I)-DT1D2*(EZZ*EZZ+EXZ*EXZ+EYZ*EYZ)
             AAA = DT1D2*(EXX*EXY+EYX*EYY+EZX*EZY)
             DXY(I)  = DXY(I) -AAA
             DYX(I)  = DYX(I) -AAA
             D4(I)   = DXY(I)+DYX(I)
             AAA = DT1D2*(EYY*EYZ+EZY*EZZ+EXY*EXZ)
             DYZ(I)  = DYZ(I) -AAA
             DZY(I)  = DZY(I) -AAA
             D5(I)   = DYZ(I)+DZY(I)
             AAA = DT1D2*(EZZ*EZX+EXZ*EXX+EYZ*EYX)
             DXZ(I)  = DXZ(I) -AAA
             DZX(I)  = DZX(I) -AAA
             D6(I)   = DXZ(I)+DZX(I)
            ENDDO
C---------implicit static---------	 
          ELSEIF (ISMDISP.GT.0.AND.ISCAU.EQ.0) THEN
            DO I=LFT,LLT
             D4(I)   = DXY(I)+DYX(I)
             D5(I)   = DYZ(I)+DZY(I)
             D6(I)   = DXZ(I)+DZX(I)
            ENDDO
          ELSE
            DT1D=TWO*DT1D2
            DO I=LFT,LLT
             D4(I)   = DXY(I)+DYX(I)
     .  	      -DT1D*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
             D5(I)   = DYZ(I)+DZY(I)
     .  	      -DT1D*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
             D6(I)   = DXZ(I)+DZX(I)
     .  	      -DT1D*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
             DXX(I)  = DXX(I)
     .  	      -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
             DYY(I)  = DYY(I)
     .  	      -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
             DZZ(I)  = DZZ(I)
     .  	      -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
            ENDDO
          ENDIF ! IF (IMPL_S.EQ.0.OR.IDYNA.GT.0)
        END IF !(ISMSTR == 11) THEN

      ELSEIF (JHBE.GE.2) THEN
       DO I=LFT,LLT
C
        EXX=DXX(I)
        EYY=DYY(I)
        EZZ=DZZ(I)
        EXY=DXY(I)
        EYX=DYX(I)
        EXZ=DXZ(I)
        EZX=DZX(I)
        EYZ=DYZ(I)
        EZY=DZY(I)
C
        DXX(I)  = DXX(I)-DT1D2*(EXX*EXX+EYX*EYX+EZX*EZX)
        DYY(I)  = DYY(I)-DT1D2*(EYY*EYY+EZY*EZY+EXY*EXY)
        DZZ(I)  = DZZ(I)-DT1D2*(EZZ*EZZ+EXZ*EXZ+EYZ*EYZ)
        AAA = DT1D2*(EXX*EXY+EYX*EYY+EZX*EZY)
        DXY(I)  = DXY(I) -AAA
        DYX(I)  = DYX(I) -AAA
        D4(I)   = DXY(I)+DYX(I)
        AAA = DT1D2*(EYY*EYZ+EZY*EZZ+EXY*EXZ)
        DYZ(I)  = DYZ(I) -AAA
        DZY(I)  = DZY(I) -AAA
        D5(I)   = DYZ(I)+DZY(I)
        AAA = DT1D2*(EZZ*EZX+EXZ*EXX+EYZ*EYX)
        DXZ(I)  = DXZ(I) -AAA
        DZX(I)  = DZX(I) -AAA
        D6(I)   = DXZ(I)+DZX(I)

        PXX2 = PX1(I)*PX1(I)+PX2(I)*PX2(I)
     .        +PX3(I)*PX3(I)+PX4(I)*PX4(I)
        PYY2 = PY1(I)*PY1(I)+PY2(I)*PY2(I)
     .        +PY3(I)*PY3(I)+PY4(I)*PY4(I)
        PZZ2 = PZ1(I)*PZ1(I)+PZ2(I)*PZ2(I)
     .        +PZ3(I)*PZ3(I)+PZ4(I)*PZ4(I)
        WZZ(I)=DT1*(PYY2*DYX(I)-PXX2*DXY(I))/(PXX2+PYY2)
        WXX(I)=DT1*(PZZ2*DZY(I)-PYY2*DYZ(I))/(PYY2+PZZ2)
        WYY(I)=DT1*(PXX2*DXZ(I)-PZZ2*DZX(I))/(PZZ2+PXX2)
       ENDDO

      ELSE
        DO I=LFT,LLT
         D4(I)   = DXY(I)+DYX(I)
         D5(I)   = DYZ(I)+DZY(I)
         D6(I)   = DXZ(I)+DZX(I)
         DXX(I)  = DXX(I)
         DYY(I)  = DYY(I)
         DZZ(I)  = DZZ(I)
         WZZ(I)=DT1D2*(DYX(I)-DXY(I))
         WYY(I)=DT1D2*(DXZ(I)-DZX(I))
         WXX(I)=DT1D2*(DZY(I)-DYZ(I))
        ENDDO

      ENDIF

      IF(ISROT.NE.0)THEN
       DO I=LFT,LLT
        DXY(I)=DXY(I) + VZR(I)
        DYX(I)=DYX(I) - VZR(I)
        DYZ(I)=DYZ(I) + VXR(I)
        DZY(I)=DZY(I) - VXR(I)
        DZX(I)=DZX(I) + VYR(I)
        DXZ(I)=DXZ(I) - VYR(I)
       ENDDO
       AAA = ZERO
       BBB = DT1 - AAA*DT1
       DO I=LFT,LLT
        WXX(I)=AAA*WXX(I) + BBB*VXR(I)
        WYY(I)=AAA*WYY(I) + BBB*VYR(I)
        WZZ(I)=AAA*WZZ(I) + BBB*VZR(I)
       ENDDO
      ENDIF
      
      !VORTICITY OUTPUT /ANIM/ELEM/VORTX VORTY VORTZ
      IF(DT1.NE.ZERO)THEN
        FAC = FOUR/DT1
        IF(ANIM_SE(10).EQ.1)THEN
          DO I=LFT,LLT
            FANI_CELL(I+NFT) = FAC*WXX(I)
          ENDDO
        ENDIF
        IF(ANIM_SE(4960).EQ.1)THEN
          IDX=(NUMELS)*ANIM_SE(10)
          DO I=LFT,LLT
            FANI_CELL(IDX+I+NFT) = FAC*WYY(I)
          ENDDO
        ENDIF
        IF(ANIM_SE(4961).EQ.1)THEN
          IDX = (NUMELS)*(ANIM_SE(10)+ANIM_SE(4960))
          DO I=LFT,LLT
            FANI_CELL(IDX+I+NFT) = FAC*WZZ(I)
          ENDDO
        ENDIF
      ENDIF
      
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SDEFOT3                       source/elements/solid/solide/sdefo3.F
Chd|-- called by -----------
Chd|        JACOB_F0                      source/elements/solid/solide8e/s8edefo3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDEFOT3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C     REAL ou REAL*8
      my_real
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46
C-----------------------------------------------
      DO I=LFT,LLT
        VX17=VX1(I)-VX7(I)
        VX28=VX2(I)-VX8(I)
        VX35=VX3(I)-VX5(I)
        VX46=VX4(I)-VX6(I)
        VY17=VY1(I)-VY7(I)
        VY28=VY2(I)-VY8(I)
        VY35=VY3(I)-VY5(I)
        VY46=VY4(I)-VY6(I)
        VZ17=VZ1(I)-VZ7(I)
        VZ28=VZ2(I)-VZ8(I)
        VZ35=VZ3(I)-VZ5(I)
        VZ46=VZ4(I)-VZ6(I)
!
        DXX(I)=PX1(I)*VX17+PX2(I)*VX28+
     .         PX3(I)*VX35+PX4(I)*VX46
        DYY(I)=PY1(I)*VY17+PY2(I)*VY28+
     .         PY3(I)*VY35+PY4(I)*VY46
        DZZ(I)=PZ1(I)*VZ17+PZ2(I)*VZ28+
     .         PZ3(I)*VZ35+PZ4(I)*VZ46
        DXY(I)=PY1(I)*VX17+PY2(I)*VX28+
     .         PY3(I)*VX35+PY4(I)*VX46
        DXZ(I)=PZ1(I)*VX17+PZ2(I)*VX28+
     .         PZ3(I)*VX35+PZ4(I)*VX46
        DYX(I)=PX1(I)*VY17+PX2(I)*VY28+
     .         PX3(I)*VY35+PX4(I)*VY46
        DYZ(I)=PZ1(I)*VY17+PZ2(I)*VY28+
     .         PZ3(I)*VY35+PZ4(I)*VY46
        DZX(I)=PX1(I)*VZ17+PX2(I)*VZ28+
     .         PX3(I)*VZ35+PX4(I)*VZ46
        DZY(I)=PY1(I)*VZ17+PY2(I)*VZ28+
     .         PY3(I)*VZ35+PY4(I)*VZ46
      ENDDO
C-----------
      RETURN
      END
