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
Chd|  ORTHRG                        source/constraints/fxbody/ortho_normalization.F
Chd|-- called by -----------
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|-- calls ---------------
Chd|        WSUM                          source/constraints/fxbody/ortho_normalization.F
Chd|        PRSCAL                        source/constraints/fxbody/ortho_normalization.F
Chd|====================================================================
      SUBROUTINE ORTHRG(VECT, MAS, NDDL,NB_MODES ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr05_c.inc"
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
Chd|====================================================================
Chd|  ORTHSR                        source/constraints/fxbody/ortho_normalization.F
Chd|-- called by -----------
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|-- calls ---------------
Chd|        WSUM                          source/constraints/fxbody/ortho_normalization.F
Chd|        PRSCAL                        source/constraints/fxbody/ortho_normalization.F
Chd|====================================================================
      SUBROUTINE ORTHSR(VECTS, VECTR, MAS, NDDL, NMS,NMR) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr05_c.inc"
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
Chd|====================================================================
Chd|  ORTHST                        source/constraints/fxbody/ortho_normalization.F
Chd|-- called by -----------
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|-- calls ---------------
Chd|        WSUM                          source/constraints/fxbody/ortho_normalization.F
Chd|        PRSCAL                        source/constraints/fxbody/ortho_normalization.F
Chd|====================================================================
      SUBROUTINE ORTHST(VECTS, MAS  , NDDL, NMS, NMSF,TOLE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr05_c.inc"
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
     .        PRSCAL
      EXTERNAL PRSCAL
C
         DO I=1,NDDL
            VT(I)=ZERO
         ENDDO
         REF=PRSCAL(VECTS(1,1), VECTS(1,1), NDDL, MAS)
         REF=SQRT(REF)
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
Chd|====================================================================
Chd|  PRSCAL                        source/constraints/fxbody/ortho_normalization.F
Chd|-- called by -----------
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|        ORTHRG                        source/constraints/fxbody/ortho_normalization.F
Chd|        ORTHSR                        source/constraints/fxbody/ortho_normalization.F
Chd|        ORTHST                        source/constraints/fxbody/ortho_normalization.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION PRSCAL(V1    , V2, NDDL, VALUE) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
Chd|====================================================================
Chd|  WSUM                          source/constraints/fxbody/ortho_normalization.F
Chd|-- called by -----------
Chd|        ORTHRG                        source/constraints/fxbody/ortho_normalization.F
Chd|        ORTHSR                        source/constraints/fxbody/ortho_normalization.F
Chd|        ORTHST                        source/constraints/fxbody/ortho_normalization.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WSUM(V1, V2, A1, A2, NDDL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
