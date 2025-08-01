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
!||    amass3f   ../engine/source/elements/solid/solide/amass3f.F
!||--- called by ------------------------------------------------------
!||    sforc3    ../engine/source/elements/solid/solide/sforc3.F
!||====================================================================
      SUBROUTINE AMASS3F(
     1   MS,      RHO,     VOLU,    NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     MSNF,
     4   NVC,     OFF,     NEL)
C------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   MS(*), RHO(*),VOLU(*),
     .   MSNF(*),OFF(*)
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      INTEGER NVC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAG
C     REAL
      my_real DMASS(MVSIZ)
      INTEGER NVC1,NVC2,NVC3,NVC4,NVC5,NVC6,NVC7,NVC8
C-----------------------------------------------
      NVC1 =  NVC / 128
      NVC2 = (NVC-NVC1*128) / 64
      NVC3 = (NVC-NVC1*128-NVC2*64) / 32
      NVC4 = (NVC-NVC1*128-NVC2*64-NVC3*32)/16
      NVC5 = (NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16)/8
      NVC6 = (NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8)/4
      NVC7 = (NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4)/2
      NVC8 =  NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4-NVC7*2
C-
       DO  I=1,NEL
          DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
       ENDDO
C
#include "lockon.inc"
      IF(NVC1==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC1(I))=MS(NC1(I)) + DMASS(I)
         MSNF(NC1(I))=MSNF(NC1(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC1(I))=MS(NC1(I)) + DMASS(I)
         MSNF(NC1(I))=MSNF(NC1(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC2==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC2(I))=MS(NC2(I)) + DMASS(I)
         MSNF(NC2(I))=MSNF(NC2(I)) + DMASS(I)
       ENDDO
      ELSE
        DO I=1,NEL
         MS(NC2(I))=MS(NC2(I)) + DMASS(I)
         MSNF(NC2(I))=MSNF(NC2(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC3==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC3(I))=MS(NC3(I)) + DMASS(I)
         MSNF(NC3(I))=MSNF(NC3(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC3(I))=MS(NC3(I)) + DMASS(I)
         MSNF(NC3(I))=MSNF(NC3(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC4==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC4(I))=MS(NC4(I)) + DMASS(I)
         MSNF(NC4(I))=MSNF(NC4(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC4(I))=MS(NC4(I)) + DMASS(I)
         MSNF(NC4(I))=MSNF(NC4(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC5==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC5(I))=MS(NC5(I)) + DMASS(I)
         MSNF(NC5(I))=MSNF(NC5(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC5(I))=MS(NC5(I)) + DMASS(I)
         MSNF(NC5(I))=MSNF(NC5(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC6==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC6(I))=MS(NC6(I)) + DMASS(I)
         MSNF(NC6(I))=MSNF(NC6(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC6(I))=MS(NC6(I)) + DMASS(I)
         MSNF(NC6(I))=MSNF(NC6(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC7==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC7(I))=MS(NC7(I)) + DMASS(I)
         MSNF(NC7(I))=MSNF(NC7(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC7(I))=MS(NC7(I)) + DMASS(I)
         MSNF(NC7(I))=MSNF(NC7(I)) + DMASS(I)
       ENDDO
      ENDIF
C
      IF(NVC8==0)THEN
#include "vectorize.inc"
       DO I=1,NEL
         MS(NC8(I))=MS(NC8(I)) + DMASS(I)
         MSNF(NC8(I))=MSNF(NC8(I)) + DMASS(I)
       ENDDO
      ELSE
       DO I=1,NEL
         MS(NC8(I))=MS(NC8(I)) + DMASS(I)
         MSNF(NC8(I))=MSNF(NC8(I)) + DMASS(I)
       ENDDO
      ENDIF
C
#include "lockoff.inc"
      RETURN
      END
