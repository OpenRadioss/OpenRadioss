Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    orthrg       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||--- called by ------------------------------------------------------
      !||    ini_fxbody   ../starter/source/constraints/fxbody/ini_fxbody.F
      !||--- calls      -----------------------------------------------------
      !||    prscal       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    wsum         ../starter/source/constraints/fxbody/ortho_normalization.F
      !||====================================================================
      SUBROUTINE ORTHRG(VECT, MAS, NDDL,NB_MODES ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, NB_MODES
      my_real
     .        VECT(NDDL,*), MAS(NDDL,NDDL)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
      my_real
     .        VT(NDDL), S, MS, UNS
C
      my_real
     .        PRSCAL
      EXTERNAL PRSCAL
C
         DO I=1,NDDL
            VT(I)=ZERO
         ENDDO
         DO I=1,NB_MODES
            CALL WSUM(VT, VECT(1,I), ZERO, ONE, NDDL)
            DO II=1,I-1
               S=PRSCAL(VT, VECT(1,II), NDDL, MAS)
               MS=-S
               CALL WSUM(VECT(1,I), VECT(1,II), ONE, MS, NDDL)
            ENDDO
            S=PRSCAL(VECT(1,I), VECT(1,I), NDDL, MAS)
            UNS=ONE/SQRT(S)
            CALL WSUM(VECT(1,I), VT, UNS, ZERO, NDDL)
         ENDDO
C
      RETURN
      END
C
      !||====================================================================
      !||    orthsr       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||--- called by ------------------------------------------------------
      !||    ini_fxbody   ../starter/source/constraints/fxbody/ini_fxbody.F
      !||--- calls      -----------------------------------------------------
      !||    prscal       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    wsum         ../starter/source/constraints/fxbody/ortho_normalization.F
      !||====================================================================
      SUBROUTINE ORTHSR(VECTS, VECTR, MAS, NDDL, NMS,NMR) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, NMS, NMR
      my_real
     .        VECTS(NDDL,*), VECTR(NDDL,*), MAS(NDDL,NDDL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
      my_real
     .        VT(NDDL), S, MS
C
      my_real 
     .        PRSCAL
      EXTERNAL PRSCAL
C
         DO I=1,NDDL
            VT(I)=ZERO
         ENDDO
         DO I=1,NMS
            CALL WSUM(VT, VECTS(1,I), ZERO, ONE, NDDL)
            DO II=1,NMR
               S=PRSCAL(VT, VECTR(1,II), NDDL, MAS)
               MS=-S
               CALL WSUM(VECTS(1,I), VECTR(1,II), ONE, MS, NDDL)
            ENDDO
         ENDDO
C      
      RETURN
      END
C
      !||====================================================================
      !||    orthst       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||--- called by ------------------------------------------------------
      !||    ini_fxbody   ../starter/source/constraints/fxbody/ini_fxbody.F
      !||--- calls      -----------------------------------------------------
      !||    prscal       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    wsum         ../starter/source/constraints/fxbody/ortho_normalization.F
      !||====================================================================
      SUBROUTINE ORTHST(VECTS, MAS  , NDDL, NMS, NMSF,TOLE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, NMS, NMSF
      my_real
     .        VECTS(NDDL,*), MAS(NDDL,NDDL), TOLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
      my_real
     .        S, MS, NORM, UNSN, REF, VT(NDDL)
C
      my_real 
     .        PRSCAL,MAXI
C-----------------------------------------------
      EXTERNAL PRSCAL
C-----------------------------------------------
C
         DO I=1,NDDL
            VT(I)=ZERO
         ENDDO

         MAXI = ZERO
         DO I=1,NMS
           NORM=PRSCAL(VECTS(1,I), VECTS(1,I), NDDL, MAS)
           IF (SQRT(NORM)>MAXI) MAXI  = MAX(MAXI,SQRT(NORM))
         ENDDO
         REF = MAXI
         NMSF=0
C
         DO I=1,NMS
C
            CALL WSUM(VT, VECTS(1,I), ZERO, ONE, NDDL)
            DO II=1,NMSF
               S=PRSCAL(VT, VECTS(1,II), NDDL, MAS)
               MS=-S
               CALL WSUM(VECTS(1,I), VECTS(1,II), ONE, MS, NDDL)
            ENDDO
            NORM=PRSCAL(VECTS(1,I), VECTS(1,I), NDDL, MAS)
            IF (SQRT(NORM)>TOLE*REF) THEN
               NMSF=NMSF+1
               UNSN=ONE/SQRT(NORM)
               CALL WSUM(VECTS(1,NMSF), VECTS(1,I), ZERO, UNSN, NDDL)
            ENDIF
         ENDDO
C
      RETURN
      END 
      !||====================================================================
      !||    prscal       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||--- called by ------------------------------------------------------
      !||    ini_fxbody   ../starter/source/constraints/fxbody/ini_fxbody.F
      !||    orthrg       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    orthsr       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    orthst       ../starter/source/constraints/fxbody/ortho_normalization.F
      !||====================================================================
      my_real FUNCTION PRSCAL(V1    , V2, NDDL, VALUE) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL
      my_real
     .        V1(*), V2(*), VALUE(NDDL,NDDL)    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,CPT
      my_real
     .        VAL
C
      PRSCAL=ZERO
      DO I=1,NDDL
         VAL=ZERO
         DO CPT=1,NDDL
            VAL=VAL+VALUE(I,CPT)*V2(CPT)
         ENDDO
         PRSCAL=PRSCAL+V1(I)*VAL
      ENDDO
C
      END
C
      !||====================================================================
      !||    wsum     ../starter/source/constraints/fxbody/ortho_normalization.F
      !||--- called by ------------------------------------------------------
      !||    orthrg   ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    orthsr   ../starter/source/constraints/fxbody/ortho_normalization.F
      !||    orthst   ../starter/source/constraints/fxbody/ortho_normalization.F
      !||====================================================================
      SUBROUTINE WSUM(V1, V2, A1, A2, NDDL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL
      my_real
     .        V1(*), V2(*), A1, A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      DO I=1,NDDL
         V1(I)=A1*V1(I)+A2*V2(I)
      ENDDO
C
      RETURN
      END       
