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
Chd|  QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        IDP_FREE                      source/system/machine.F       
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE QMASS2(OFF,RHO,MS,
     .                  AREA,NC1,NC2,NC3,NC4)
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
C     REAL
      my_real
     .   OFF(*), RHO(*), MS(*),AREA(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I
C-----------------------------------------------
      DO I=LFT,LLT
       MASS(I)=RHO(I)*AREA(I)*OFF(I)*FOURTH
      ENDDO
C
      CALL IDP_LOCK(1)
      DO I=LFT,LLT
C
       MS(NC1(I))=MS(NC1(I)) + MASS(I)
       MS(NC2(I))=MS(NC2(I)) + MASS(I)
       MS(NC3(I))=MS(NC3(I)) + MASS(I)
       MS(NC4(I))=MS(NC4(I)) + MASS(I)
      ENDDO
      CALL IDP_FREE(1)
C
      RETURN
      END
Chd|====================================================================
Chd|  QMASS2P                       source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QMASS2P(OFF ,RHO,AREA,FSKY,FSKYV,
     .                   IADQ)
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF(*), RHO(*), AREA(*), FSKYV(LSKY,8),FSKY(8,LSKY)
      INTEGER IADQ(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I, II, K
C-----------------------------------------------
      DO I=LFT,LLT
       MASS(I)=RHO(I)*AREA(I)*OFF(I)*FOURTH
      ENDDO
C

      IF(IVECTOR==1) THEN
        DO I=LFT,LLT
C
          II=I+NFT
          K = IADQ(1,II)
          FSKYV(K,1)=MASS(I)
C
          K = IADQ(2,II)
          FSKYV(K,1)=MASS(I)
C
          K = IADQ(3,II)
          FSKYV(K,1)=MASS(I)
C
          K = IADQ(4,II)
          FSKYV(K,1)=MASS(I)
C
        ENDDO
C
      ELSE
        DO I=LFT,LLT
C
          II=I+NFT
          K = IADQ(1,II)
          FSKY(1,K) = MASS(I)
C
          K = IADQ(2,II)
          FSKY(1,K) = MASS(I)
C
          K = IADQ(3,II)
          FSKY(1,K) = MASS(I)
C
          K = IADQ(4,II)
          FSKY(1,K) = MASS(I)
C
        ENDDO
C
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  QMASS2AP                      source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QMASS2AP(OFF ,RHO,AREA,FSKY,FSKYV,
     .                   IADQ)
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF(*), RHO(*), AREA(*), FSKYV(LSKY,8),FSKY(8,LSKY)
      INTEGER IADQ(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I, II, K
C-----------------------------------------------
      DO I=LFT,LLT
       MASS(I)=RHO(I)*AREA(I)*OFF(I)*FOURTH
      ENDDO
C

      IF(IVECTOR==1) THEN
        DO I=LFT,LLT
C
          II=I+NFT
          K = IADQ(1,II)
          FSKYV(K,1)=FSKYV(K,1)+MASS(I)
C
          K = IADQ(2,II)
          FSKYV(K,1)=FSKYV(K,1)+MASS(I)
C
          K = IADQ(3,II)
          FSKYV(K,1)=FSKYV(K,1)+MASS(I)
C
          K = IADQ(4,II)
          FSKYV(K,1)=FSKYV(K,1)+MASS(I)
C
        ENDDO
C
      ELSE
        DO I=LFT,LLT
C
          II=I+NFT
          K = IADQ(1,II)
          FSKY(1,K) = FSKY(1,K)+MASS(I)
C
          K = IADQ(2,II)
          FSKY(1,K) = FSKY(1,K)+MASS(I)
C
          K = IADQ(3,II)
          FSKY(1,K) = FSKY(1,K)+MASS(I)
C
          K = IADQ(4,II)
          FSKY(1,K) = FSKY(1,K)+MASS(I)
C
        ENDDO
C
      ENDIF
C
      RETURN
      END




Chd|====================================================================
Chd|  QMASSREAL2                    source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        IDP_FREE                      source/system/machine.F       
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE QMASSREAL2(OFF,RHO,MS_2D,
     .                  VNEW,NC1,NC2,NC3,NC4)
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
C     REAL
      my_real
     .   OFF(*), RHO(*), MS_2D(*),VNEW(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I
C-----------------------------------------------
      DO I=LFT,LLT
       MASS(I)=RHO(I)*VNEW(I)*OFF(I)*FOURTH
      ENDDO
C
      CALL IDP_LOCK(1)
      DO I=LFT,LLT
C
       MS_2D(NC1(I))=MS_2D(NC1(I)) + MASS(I)
       MS_2D(NC2(I))=MS_2D(NC2(I)) + MASS(I)
       MS_2D(NC3(I))=MS_2D(NC3(I)) + MASS(I)
       MS_2D(NC4(I))=MS_2D(NC4(I)) + MASS(I)
      ENDDO
      CALL IDP_FREE(1)
C
      RETURN
      END
Chd|====================================================================
Chd|  QMASSREAL2P                   source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QMASSREAL2P(OFF ,RHO,VNEW,FSKYM,IADQ)
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
C     REAL
      my_real
     .   OFF(*), RHO(*), VNEW(*), FSKYM(*)
      INTEGER IADQ(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I, II, K
C-----------------------------------------------
      DO I=LFT,LLT
       MASS(I)=RHO(I)*VNEW(I)*OFF(I)*FOURTH
      ENDDO
C
      DO I=LFT,LLT
C
         II=I+NFT
         K = IADQ(1,II)
         FSKYM(K)=MASS(I)
C
         K = IADQ(2,II)
         FSKYM(K)=MASS(I)
C
         K = IADQ(3,II)
         FSKYM(K)=MASS(I)
C
         K = IADQ(4,II)
         FSKYM(K)=MASS(I)
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  QMASSREAL2AP                  source/elements/solid_2d/quad/qmass2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QMASSREAL2AP(OFF ,RHO,VNEW,FSKYM,IADQ)
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
C     REAL
      my_real
     .   OFF(*), RHO(*), VNEW(*), FSKYM(*)
      INTEGER IADQ(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I, II, K
C-----------------------------------------------
      DO I=LFT,LLT
       MASS(I)=RHO(I)*VNEW(I)*OFF(I)*FOURTH
      ENDDO
C
      DO I=LFT,LLT
C
         II=I+NFT
         K = IADQ(1,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
C
         K = IADQ(2,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
C
         K = IADQ(3,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
C
         K = IADQ(4,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
      ENDDO
C
      RETURN
      END
