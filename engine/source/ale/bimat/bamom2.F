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
Chd|  BAMOM2                        source/ale/bimat/bamom2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BAMOM2(
     1   PM,      V,       W,       RHO,
     2   ALPH,    ALPHC,   FILL,    B11,
     3   B12,     B13,     B14,     B21,
     4   B22,     B23,     B24,     PY1,
     5   PY2,     PZ1,     PZ2,     AIRE,
     6   MAT,     NC1,     NC2,     NC3,
     7   NC4,     NEL)
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
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   PM(NPROPM,*), V(3,*), W(3,*), RHO(*), ALPH(*), ALPHC(*),
     .   FILL(*)
      my_real
     .   B11(*), B12(*), B13(*), B14(*),
     .   B21(*), B22(*), B23(*), B24(*),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*), AIRE(*)

      INTEGER MAT(*), NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NV
      my_real
     .   GAMMA(MVSIZ), XMS(MVSIZ), 
     .   VDY1(MVSIZ), VDY2(MVSIZ), VDY3(MVSIZ), VDY4(MVSIZ), VDZ1(MVSIZ), VDZ2(MVSIZ),
     .   VDZ3(MVSIZ), VDZ4(MVSIZ), VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VZ1(MVSIZ),
     .   VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ), VY13(MVSIZ), VY24(MVSIZ), VZ13(MVSIZ), VZ24(MVSIZ),
     .   DYY(MVSIZ), DZZ(MVSIZ), DYZ(MVSIZ), DZY(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ), F1(MVSIZ), F2(MVSIZ),
     .   A1(MVSIZ), A2(MVSIZ), G1(MVSIZ), G2(MVSIZ), FF1, FF2, FF3, FF4, DVY, DVZ
C-------------------------------
      DO I=1,NEL
        XMS(I)  =FOURTH*RHO(I)*ALPH(I)
        GAMMA(I)= PM(15,MAT(I))
      ENDDO
C-------------------------------
      DO I=1,NEL
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
      ENDDO
C
      DO I=1,NEL
        VY13(I)=VY1(I)-VY3(I)
        VY24(I)=VY2(I)-VY4(I)
        VZ13(I)=VZ1(I)-VZ3(I)
        VZ24(I)=VZ2(I)-VZ4(I)
      ENDDO
C
      DO I=1,NEL
        DYY(I)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
        DZZ(I)=PZ1(I)*VZ13(I)+PZ2(I)*VZ24(I)
        DYZ(I)=PZ1(I)*VY13(I)+PZ2(I)*VY24(I)
        DZY(I)=PY1(I)*VZ13(I)+PY2(I)*VZ24(I)
      ENDDO
C-----------------------------------------------
C     CALCUL PAR NOEUD DE V MATIERE - V MAILLAGE
C-----------------------------------------------
      DO I=1,NEL
        VDY1(I)=V(2,NC1(I)) - W(2,NC1(I))
        VDZ1(I)=V(3,NC1(I)) - W(3,NC1(I))

        VDY2(I)=V(2,NC2(I)) - W(2,NC2(I))
        VDZ2(I)=V(3,NC2(I)) - W(3,NC2(I))

        VDY3(I)=V(2,NC3(I)) - W(2,NC3(I))
        VDZ3(I)=V(3,NC3(I)) - W(3,NC3(I))

        VDY4(I)=V(2,NC4(I)) - W(2,NC4(I))
        VDZ4(I)=V(3,NC4(I)) - W(3,NC4(I))
      ENDDO
C-----------------------------------------------
C     CALCUL DE (V MATIERE - V MAILLAGE) MOYEN
C-----------------------------------------------
      DO I=1,NEL
        VDY(I)=FOURTH*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I))
        VDZ(I)=FOURTH*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I))
      ENDDO

      DO I=1,NEL
        F1(I) = (VDY(I)*DYY(I)+VDZ(I)*DYZ(I))*XMS(I)
        F2(I) = (VDY(I)*DZY(I)+VDZ(I)*DZZ(I))*XMS(I)
      ENDDO

      DO I=1,NEL
        A1(I) = PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
        A2(I) = PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
      ENDDO

      DO I=1,NEL
        G1(I) = SIGN(GAMMA(I),A1(I))
        G2(I) = SIGN(GAMMA(I),A2(I))
      ENDDO

      DO I=1,NEL
        B11(I) =  (ONE  + G1(I))*F1(I)
        B12(I) =  (ONE  + G2(I))*F1(I)
        B13(I) =  (ONE  - G1(I))*F1(I)
        B14(I) =  (ONE  - G2(I))*F1(I)

        B21(I) =  (ONE+G1(I))*F2(I)
        B22(I) =  (ONE+G2(I))*F2(I)
        B23(I) =  (ONE-G1(I))*F2(I)
        B24(I) =  (ONE-G2(I))*F2(I)
      ENDDO

      DO I=1,NEL
        XMS(I)  =FOURTH*RHO(I)*AIRE(I)*(ONE-ALPH(I)) / MAX(EM15,DT1)
      ENDDO

       DO I=1,NEL
       IF(ALPH(I)<ONE
     .   .AND.ALPH(I)>ZERO
     .   .AND.ALPHC(I)==ZERO
     .   .AND.DT1>ZERO)THEN

         FF1=FILL(NC1(I))
         FF2=FILL(NC2(I))
         FF3=FILL(NC3(I))
         FF4=FILL(NC4(I))
C  1
        IF(FF1<ZERO)THEN
         NV=0
         DVY=ZERO
         DVZ=ZERO
         IF(FF2>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC2(I))-V(2,NC1(I)))
          DVZ=DVZ+(V(3,NC2(I))-V(3,NC1(I)))
         ENDIF
         IF(FF4>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC4(I))-V(2,NC1(I)))
          DVZ=DVZ+(V(3,NC4(I))-V(3,NC1(I)))
         ENDIF
         IF(NV==0.AND.FF3>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC3(I))-V(2,NC1(I)))
          DVZ=DVZ+(V(3,NC3(I))-V(3,NC1(I)))
         ENDIF
         B11(I)=B11(I)-XMS(I)*DVY/MAX(1,NV)
         B21(I)=B21(I)-XMS(I)*DVZ/MAX(1,NV)
        ENDIF
C  2
        IF(FF2<ZERO)THEN
         NV=0
         DVY=ZERO
         DVZ=ZERO
         IF(FF3>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC3(I))-V(2,NC2(I)))
          DVZ=DVZ+(V(3,NC3(I))-V(3,NC2(I)))
         ENDIF
         IF(FF1>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC1(I))-V(2,NC2(I)))
          DVZ=DVZ+(V(3,NC1(I))-V(3,NC2(I)))
         ENDIF
         IF(NV==0.AND.FF4>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC4(I))-V(2,NC2(I)))
          DVZ=DVZ+(V(3,NC4(I))-V(3,NC2(I)))
         ENDIF
         B12(I)=B12(I)-XMS(I)*DVY/MAX(1,NV)
         B22(I)=B22(I)-XMS(I)*DVZ/MAX(1,NV)
        ENDIF
C  3
        IF(FF3<ZERO)THEN
         NV=0
         DVY=ZERO
         DVZ=ZERO
         IF(FF4>ZERO)THEN   
          NV=NV+1
          DVY=DVY+(V(2,NC4(I))-V(2,NC3(I)))
          DVZ=DVZ+(V(3,NC4(I))-V(3,NC3(I)))
         ENDIF
         IF(FF2>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC2(I))-V(2,NC3(I)))
          DVZ=DVZ+(V(3,NC2(I))-V(3,NC3(I)))
         ENDIF
         IF(NV==0.AND.FF1>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC1(I))-V(2,NC3(I)))
          DVZ=DVZ+(V(3,NC1(I))-V(3,NC3(I)))
         ENDIF
         B13(I)=B13(I)-XMS(I)*DVY/MAX(1,NV)
         B23(I)=B23(I)-XMS(I)*DVZ/MAX(1,NV)
        ENDIF
C  4
        IF(FF4<ZERO)THEN
         NV=0
         DVY=ZERO
         DVZ=ZERO
         IF(FF1>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC1(I))-V(2,NC4(I)))
          DVZ=DVZ+(V(3,NC1(I))-V(3,NC4(I)))
         ENDIF
         IF(FF3>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC3(I))-V(2,NC4(I)))
          DVZ=DVZ+(V(3,NC3(I))-V(3,NC4(I)))
         ENDIF
         IF(NV==0.AND.FF2>ZERO)THEN
          NV=NV+1
          DVY=DVY+(V(2,NC2(I))-V(2,NC4(I)))
          DVZ=DVZ+(V(3,NC2(I))-V(3,NC4(I)))
         ENDIF
         B14(I)=B14(I)-XMS(I)*DVY/MAX(1,NV)
         B24(I)=B24(I)-XMS(I)*DVZ/MAX(1,NV)
        ENDIF
       ENDIF
      ENDDO !next I
      RETURN
      END
