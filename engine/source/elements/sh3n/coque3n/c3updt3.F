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
Chd|  C3UPDT3                       source/elements/sh3n/coque3n/c3updt3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3UPDT3(JFT  ,JLT ,F   ,M   ,NVC  ,
     2                   OFFG ,OFF ,STI ,STIR,STIFN,
     3                   STIFR,IXTG,F11  ,
     4                   F12  ,F13 ,F21 ,F22 ,F23  ,
     5                   F31  ,F32 ,F33 ,M11 ,M12  ,
     6                   M13  ,M21 ,M22 ,M23 ,M31  ,
     7                   M32  ,M33 ,JTHE,THEM,FTHE,
     8                   EINT ,PM  ,AREA,THK ,PARTSAV,
     9                   MAT,IPARTTG,CONDN,CONDE)
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
#include      "task_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JTHE,JFT, JLT, NVC
      INTEGER IXTG(NIXTG,*),MAT(MVSIZ),IPARTTG(*)
      my_real
     .   F(3,*), M(3,*), OFFG(*), OFF(*), STI(*), STIR(*),
     .   STIFN(*), STIFR(*),CONDN(*),CONDE(*)
      my_real F11(MVSIZ), F12(MVSIZ), F13(MVSIZ),
     .     F21(MVSIZ), F22(MVSIZ), F23(MVSIZ),
     .     F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .     M11(MVSIZ), M12(MVSIZ), M13(MVSIZ),
     .     M21(MVSIZ), M22(MVSIZ), M23(MVSIZ),
     .     M31(MVSIZ), M32(MVSIZ), M33(MVSIZ),
     .   THEM(MVSIZ,3),FTHE(*),EINT(JLT,2),PM(NPROPM,*),AREA(*),THK(*),
     .   PARTSAV(NPSAV,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NVC1,NVC2,NVC3,NC1,NC2,NC3,IOFF,MX,MT
      my_real
     .   OFF_L
C=======================================================================
C cumul de l'energie des elements deletes AU moment du delete
      IOFF=0
      DO I=JFT,JLT
        IF(OFF(I) == ZERO .AND. OFFG(I) > ZERO) IOFF=1
      ENDDO
      IF(IOFF == 1)THEN
       MT = MAT(JFT)
       DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO)THEN
           MX = IPARTTG(I)
           PARTSAV(24,MX)=PARTSAV(24,MX) + EINT(I,1) + EINT(I,2)
           PARTSAV(25,MX)=PARTSAV(25,MX) + PM(1,MT)*AREA(I)*THK(I)
        ENDIF
       ENDDO
      ENDIF
      OFF_L = ZERO
      DO I=JFT,JLT
        IF(OFF(I)<ONE)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF (OFF_L < ZERO)THEN
        DO I=JFT,JLT
         IF (OFFG(I) < ZERO)THEN
           F11(I)=ZERO
           F21(I)=ZERO
           F31(I)=ZERO
           M11(I)=ZERO
           M21(I)=ZERO
           M31(I)=ZERO
           F12(I)=ZERO
           F22(I)=ZERO
           F32(I)=ZERO
           M12(I)=ZERO
           M22(I)=ZERO
           M32(I)=ZERO
           F13(I)=ZERO
           F23(I)=ZERO
           F33(I)=ZERO
           M13(I)=ZERO
           M23(I)=ZERO
           M33(I)=ZERO
           STI(I)=ZERO
           STIR(I)=ZERO
           THEM(I,1) = ZERO
           THEM(I,2) = ZERO
           THEM(I,3) = ZERO
           CONDE(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C
      NVC1= NVC/8
      NVC2=(NVC-NVC1*8)/4
      NVC3=(NVC-NVC1*8-NVC2*4)/2
C
      IF(NVC1 == 0)THEN
       IF(JTHE == 0) THEN
#include "vectorize.inc"
          DO 100 I=JFT,JLT
           NC1 = IXTG(2,I)
           F(1,NC1)=F(1,NC1)-F11(I)
           F(2,NC1)=F(2,NC1)-F21(I)
           F(3,NC1)=F(3,NC1)-F31(I)
           M(1,NC1)=M(1,NC1)-M11(I)
           M(2,NC1)=M(2,NC1)-M21(I)
           M(3,NC1)=M(3,NC1)-M31(I)
           STIFN(NC1)=STIFN(NC1)+STI(I)
           STIFR(NC1)=STIFR(NC1)+STIR(I)
 100      CONTINUE
        ELSE
         IF(NODADT_THERM == 1 ) THEN
#include "vectorize.inc"
           DO  I=JFT,JLT
            NC1 = IXTG(2,I)
            F(1,NC1)=F(1,NC1)-F11(I)
            F(2,NC1)=F(2,NC1)-F21(I)
            F(3,NC1)=F(3,NC1)-F31(I)
            M(1,NC1)=M(1,NC1)-M11(I)
            M(2,NC1)=M(2,NC1)-M21(I)
            M(3,NC1)=M(3,NC1)-M31(I)
            STIFN(NC1)=STIFN(NC1)+STI(I)
            STIFR(NC1)=STIFR(NC1)+STIR(I)
            FTHE(NC1) = FTHE(NC1) + THEM(I,1)
            CONDN(NC1)=CONDN(NC1)+CONDE(I)
           ENDDO
          ELSE
#include "vectorize.inc"
           DO  I=JFT,JLT
            NC1 = IXTG(2,I)
            F(1,NC1)=F(1,NC1)-F11(I)
            F(2,NC1)=F(2,NC1)-F21(I)
            F(3,NC1)=F(3,NC1)-F31(I)
            M(1,NC1)=M(1,NC1)-M11(I)
            M(2,NC1)=M(2,NC1)-M21(I)
            M(3,NC1)=M(3,NC1)-M31(I)
            STIFN(NC1)=STIFN(NC1)+STI(I)
            STIFR(NC1)=STIFR(NC1)+STIR(I)
            FTHE(NC1) = FTHE(NC1) + THEM(I,1)
           ENDDO
          ENDIF
        ENDIF

      ELSE
       IF(JTHE == 0 ) THEN
         DO 110 I=JFT,JLT
          NC1 = IXTG(2,I)
          F(1,NC1)=F(1,NC1)-F11(I)
          F(2,NC1)=F(2,NC1)-F21(I)
          F(3,NC1)=F(3,NC1)-F31(I)
          M(1,NC1)=M(1,NC1)-M11(I)
          M(2,NC1)=M(2,NC1)-M21(I)
          M(3,NC1)=M(3,NC1)-M31(I)
          STIFN(NC1)=STIFN(NC1)+STI(I)
          STIFR(NC1)=STIFR(NC1)+STIR(I)
  110    CONTINUE
       ELSE
         IF(NODADT_THERM == 1 ) THEN
           DO  I=JFT,JLT
             NC1 = IXTG(2,I)
             F(1,NC1)=F(1,NC1)-F11(I)
             F(2,NC1)=F(2,NC1)-F21(I)
             F(3,NC1)=F(3,NC1)-F31(I)
             M(1,NC1)=M(1,NC1)-M11(I)
             M(2,NC1)=M(2,NC1)-M21(I)
             M(3,NC1)=M(3,NC1)-M31(I)
             STIFN(NC1)=STIFN(NC1)+STI(I)
             STIFR(NC1)=STIFR(NC1)+STIR(I)
             FTHE(NC1) = FTHE(NC1) + THEM(I,1)
             CONDN(NC1)=CONDN(NC1)+CONDE(I)
           ENDDO
         ELSE
           DO  I=JFT,JLT
             NC1 = IXTG(2,I)
             F(1,NC1)=F(1,NC1)-F11(I)
             F(2,NC1)=F(2,NC1)-F21(I)
             F(3,NC1)=F(3,NC1)-F31(I)
             M(1,NC1)=M(1,NC1)-M11(I)
             M(2,NC1)=M(2,NC1)-M21(I)
             M(3,NC1)=M(3,NC1)-M31(I)
             STIFN(NC1)=STIFN(NC1)+STI(I)
             STIFR(NC1)=STIFR(NC1)+STIR(I)
             FTHE(NC1) = FTHE(NC1) + THEM(I,1)
           ENDDO
         ENDIF
       ENDIF
      ENDIF
C
      IF(NVC2 == 0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
         DO 200 I=JFT,JLT
          NC2 = IXTG(3,I)
          F(1,NC2)=F(1,NC2)-F12(I)
          F(2,NC2)=F(2,NC2)-F22(I)
          F(3,NC2)=F(3,NC2)-F32(I)
          M(1,NC2)=M(1,NC2)-M12(I)
          M(2,NC2)=M(2,NC2)-M22(I)
          M(3,NC2)=M(3,NC2)-M32(I)
          STIFN(NC2)=STIFN(NC2)+STI(I)
          STIFR(NC2)=STIFR(NC2)+STIR(I)
  200    CONTINUE
        ELSE
         IF(NODADT_THERM == 1 ) THEN
#include "vectorize.inc"
          DO  I=JFT,JLT
            NC2 = IXTG(3,I)
            F(1,NC2)=F(1,NC2)-F12(I)
            F(2,NC2)=F(2,NC2)-F22(I)
            F(3,NC2)=F(3,NC2)-F32(I)
            M(1,NC2)=M(1,NC2)-M12(I)
            M(2,NC2)=M(2,NC2)-M22(I)
            M(3,NC2)=M(3,NC2)-M32(I)
            STIFN(NC2)=STIFN(NC2)+STI(I)
            STIFR(NC2)=STIFR(NC2)+STIR(I)
            FTHE(NC2) = FTHE(NC2) + THEM(I,2)
            CONDN(NC2)=CONDN(NC2)+CONDE(I)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  I=JFT,JLT
            NC2 = IXTG(3,I)
            F(1,NC2)=F(1,NC2)-F12(I)
            F(2,NC2)=F(2,NC2)-F22(I)
            F(3,NC2)=F(3,NC2)-F32(I)
            M(1,NC2)=M(1,NC2)-M12(I)
            M(2,NC2)=M(2,NC2)-M22(I)
            M(3,NC2)=M(3,NC2)-M32(I)
            STIFN(NC2)=STIFN(NC2)+STI(I)
            STIFR(NC2)=STIFR(NC2)+STIR(I)
            FTHE(NC2) = FTHE(NC2) + THEM(I,2)
          ENDDO
         ENDIF
        ENDIF
      ELSE
       IF(JTHE == 0 ) THEN
         DO 210 I=JFT,JLT
          NC2 = IXTG(3,I)
          F(1,NC2)=F(1,NC2)-F12(I)
          F(2,NC2)=F(2,NC2)-F22(I)
          F(3,NC2)=F(3,NC2)-F32(I)
          M(1,NC2)=M(1,NC2)-M12(I)
          M(2,NC2)=M(2,NC2)-M22(I)
          M(3,NC2)=M(3,NC2)-M32(I)
          STIFN(NC2)=STIFN(NC2)+STI(I)
          STIFR(NC2)=STIFR(NC2)+STIR(I)
  210    CONTINUE
       ELSE
         IF(NODADT_THERM == 1 ) THEN
          DO  I=JFT,JLT
            NC2 = IXTG(3,I)
            F(1,NC2)=F(1,NC2)-F12(I)
            F(2,NC2)=F(2,NC2)-F22(I)
            F(3,NC2)=F(3,NC2)-F32(I)
            M(1,NC2)=M(1,NC2)-M12(I)
            M(2,NC2)=M(2,NC2)-M22(I)
            M(3,NC2)=M(3,NC2)-M32(I)
            STIFN(NC2)=STIFN(NC2)+STI(I)
            STIFR(NC2)=STIFR(NC2)+STIR(I)
            FTHE(NC2) = FTHE(NC2) + THEM(I,2)
            CONDN(NC2)=CONDN(NC2)+CONDE(I)
          ENDDO
         ELSE
          DO  I=JFT,JLT
            NC2 = IXTG(3,I)
            F(1,NC2)=F(1,NC2)-F12(I)
            F(2,NC2)=F(2,NC2)-F22(I)
            F(3,NC2)=F(3,NC2)-F32(I)
            M(1,NC2)=M(1,NC2)-M12(I)
            M(2,NC2)=M(2,NC2)-M22(I)
            M(3,NC2)=M(3,NC2)-M32(I)
            STIFN(NC2)=STIFN(NC2)+STI(I)
            STIFR(NC2)=STIFR(NC2)+STIR(I)
            FTHE(NC2) = FTHE(NC2) + THEM(I,2)
          ENDDO
         ENDIF
       ENDIF
      ENDIF
C
      IF(NVC3 == 0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
          DO 300 I=JFT,JLT
           NC3 = IXTG(4,I)
           F(1,NC3)=F(1,NC3)-F13(I)
           F(2,NC3)=F(2,NC3)-F23(I)
           F(3,NC3)=F(3,NC3)-F33(I)
           M(1,NC3)=M(1,NC3)-M13(I)
           M(2,NC3)=M(2,NC3)-M23(I)
           M(3,NC3)=M(3,NC3)-M33(I)
           STIFN(NC3)=STIFN(NC3)+STI(I)
           STIFR(NC3)=STIFR(NC3)+STIR(I)
  300     CONTINUE
         ELSE
          IF(NODADT_THERM == 1 ) THEN
#include "vectorize.inc"
           DO I=JFT,JLT
             NC3 = IXTG(4,I)
             F(1,NC3)=F(1,NC3)-F13(I)
             F(2,NC3)=F(2,NC3)-F23(I)
             F(3,NC3)=F(3,NC3)-F33(I)
             M(1,NC3)=M(1,NC3)-M13(I)
             M(2,NC3)=M(2,NC3)-M23(I)
             M(3,NC3)=M(3,NC3)-M33(I)
             STIFN(NC3)=STIFN(NC3)+STI(I)
             STIFR(NC3)=STIFR(NC3)+STIR(I)
             FTHE(NC3) = FTHE(NC3) + THEM(I,3)
             CONDN(NC3)=CONDN(NC3)+CONDE(I)
           ENDDO
          ELSE
#include "vectorize.inc"
           DO I=JFT,JLT
             NC3 = IXTG(4,I)
             F(1,NC3)=F(1,NC3)-F13(I)
             F(2,NC3)=F(2,NC3)-F23(I)
             F(3,NC3)=F(3,NC3)-F33(I)
             M(1,NC3)=M(1,NC3)-M13(I)
             M(2,NC3)=M(2,NC3)-M23(I)
             M(3,NC3)=M(3,NC3)-M33(I)
             STIFN(NC3)=STIFN(NC3)+STI(I)
             STIFR(NC3)=STIFR(NC3)+STIR(I)
             FTHE(NC3) = FTHE(NC3) + THEM(I,3)
           ENDDO
          ENDIF
         ENDIF

      ELSE
       IF(JTHE == 0 ) THEN
         DO 310 I=JFT,JLT
          NC3 = IXTG(4,I)
          F(1,NC3)=F(1,NC3)-F13(I)
          F(2,NC3)=F(2,NC3)-F23(I)
          F(3,NC3)=F(3,NC3)-F33(I)
          M(1,NC3)=M(1,NC3)-M13(I)
          M(2,NC3)=M(2,NC3)-M23(I)
          M(3,NC3)=M(3,NC3)-M33(I)
          STIFN(NC3)=STIFN(NC3)+STI(I)
          STIFR(NC3)=STIFR(NC3)+STIR(I)
  310    CONTINUE
        ELSE
          IF(NODADT_THERM == 1 ) THEN
           DO I=JFT,JLT
             NC3 = IXTG(4,I)
             F(1,NC3)=F(1,NC3)-F13(I)
             F(2,NC3)=F(2,NC3)-F23(I)
             F(3,NC3)=F(3,NC3)-F33(I)
             M(1,NC3)=M(1,NC3)-M13(I)
             M(2,NC3)=M(2,NC3)-M23(I)
             M(3,NC3)=M(3,NC3)-M33(I)
             STIFN(NC3)=STIFN(NC3)+STI(I)
             STIFR(NC3)=STIFR(NC3)+STIR(I)
             FTHE(NC3) = FTHE(NC3) + THEM(I,3)
             CONDN(NC3)=CONDN(NC3)+CONDE(I)
           ENDDO
          ELSE
           DO I=JFT,JLT
             NC3 = IXTG(4,I)
             F(1,NC3)=F(1,NC3)-F13(I)
             F(2,NC3)=F(2,NC3)-F23(I)
             F(3,NC3)=F(3,NC3)-F33(I)
             M(1,NC3)=M(1,NC3)-M13(I)
             M(2,NC3)=M(2,NC3)-M23(I)
             M(3,NC3)=M(3,NC3)-M33(I)
             STIFN(NC3)=STIFN(NC3)+STI(I)
             STIFR(NC3)=STIFR(NC3)+STIR(I)
             FTHE(NC3) = FTHE(NC3) + THEM(I,3)
           ENDDO
          ENDIF
        ENDIF
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3UPDT3P                      source/elements/sh3n/coque3n/c3updt3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3UPDT3P(JFT,JLT ,OFFG ,OFF,STI,
     2                   STIR,FSKY,FSKYV,IADTG,F11  ,
     4                   F12  ,F13 ,F21 ,F22 ,F23  ,
     5                   F31  ,F32 ,F33 ,M11 ,M12  ,
     6                   M13  ,M21 ,M22 ,M23 ,M31  ,
     7                   M32  ,M33 ,JTHE,THEM,FTHESKY,
     8                   EINT ,PM  ,AREA,THK ,PARTSAV,
     9                   MAT,IPARTTG,CONDNSKY,CONDE  )
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
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IADTG(3,*),JTHE,MAT(MVSIZ),IPARTTG(*)
      my_real
     .   OFFG(*), OFF(*), STI(*), STIR(*), FSKYV(LSKY,8),
     .   FSKY(8,LSKY)
      my_real F11(MVSIZ), F12(MVSIZ), F13(MVSIZ),
     .     F21(MVSIZ), F22(MVSIZ), F23(MVSIZ),
     .     F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .     M11(MVSIZ), M12(MVSIZ), M13(MVSIZ),
     .     M21(MVSIZ), M22(MVSIZ), M23(MVSIZ),
     .     M31(MVSIZ), M32(MVSIZ), M33(MVSIZ),
     .     CONDE(MVSIZ),
     .     THEM(MVSIZ,3),FTHESKY(LSKY),CONDNSKY(*),
     .     EINT(JLT,2),PM(NPROPM,*),AREA(*),THK(*),PARTSAV(NPSAV,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K, IOFF, MX, MT
      my_real
     .   OFF_L
C=======================================================================
C cumul de l'energie des elements deletes AU moment du delete
      IOFF=0
      DO I=JFT,JLT
        IF(OFF(I) == ZERO.AND.OFFG(I) > ZERO)IOFF=1
      ENDDO
      IF(IOFF == 1)THEN
       MT = MAT(JFT)
       DO I=JFT,JLT
        IF(OFF(I) == ZERO.AND.OFFG(I) > ZERO)THEN
           MX = IPARTTG(I)
           PARTSAV(24,MX)=PARTSAV(24,MX) + EINT(I,1) + EINT(I,2)
           PARTSAV(25,MX)=PARTSAV(25,MX) + PM(1,MT)*AREA(I)*THK(I)
        ENDIF
       ENDDO
      ENDIF
      OFF_L = ZERO
      DO I=JFT,JLT
        IF (OFF(I) < ONE) OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L < ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I) < ZERO)THEN
           F11(I)=ZERO
           F21(I)=ZERO
           F31(I)=ZERO
           M11(I)=ZERO
           M21(I)=ZERO
           M31(I)=ZERO
           F12(I)=ZERO
           F22(I)=ZERO
           F32(I)=ZERO
           M12(I)=ZERO
           M22(I)=ZERO
           M32(I)=ZERO
           F13(I)=ZERO
           F23(I)=ZERO
           F33(I)=ZERO
           M13(I)=ZERO
           M23(I)=ZERO
           M33(I)=ZERO
           STI(I)=ZERO
           STIR(I)=ZERO
           CONDE(I)=ZERO
c           THEM(I,1) = ZERO
c           THEM(I,2) = ZERO
c           THEM(I,3) = ZERO
         ENDIF
        ENDDO
      ENDIF
C
      IF(JTHE == 0 ) THEN
       IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          K = IADTG(1,I)
          FSKYV(K,1)=-F11(I)
          FSKYV(K,2)=-F21(I)
          FSKYV(K,3)=-F31(I)
          FSKYV(K,4)=-M11(I)
          FSKYV(K,5)=-M21(I)
          FSKYV(K,6)=-M31(I)
          FSKYV(K,7)=STI(I)
          FSKYV(K,8)=STIR(I)
          K = IADTG(2,I)
          FSKYV(K,1)=-F12(I)
          FSKYV(K,2)=-F22(I)
          FSKYV(K,3)=-F32(I)
          FSKYV(K,4)=-M12(I)
          FSKYV(K,5)=-M22(I)
          FSKYV(K,6)=-M32(I)
          FSKYV(K,7)=STI(I)
          FSKYV(K,8)=STIR(I)
          K = IADTG(3,I)
          FSKYV(K,1)=-F13(I)
          FSKYV(K,2)=-F23(I)
          FSKYV(K,3)=-F33(I)
          FSKYV(K,4)=-M13(I)
          FSKYV(K,5)=-M23(I)
          FSKYV(K,6)=-M33(I)
          FSKYV(K,7)=STI(I)
          FSKYV(K,8)=STIR(I)
        ENDDO
       ELSE
        DO I=JFT,JLT
          K = IADTG(1,I)
          FSKY(1,K)=-F11(I)
          FSKY(2,K)=-F21(I)
          FSKY(3,K)=-F31(I)
          FSKY(4,K)=-M11(I)
          FSKY(5,K)=-M21(I)
          FSKY(6,K)=-M31(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
          K = IADTG(2,I)
          FSKY(1,K)=-F12(I)
          FSKY(2,K)=-F22(I)
          FSKY(3,K)=-F32(I)
          FSKY(4,K)=-M12(I)
          FSKY(5,K)=-M22(I)
          FSKY(6,K)=-M32(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
          K = IADTG(3,I)
          FSKY(1,K)=-F13(I)
          FSKY(2,K)=-F23(I)
          FSKY(3,K)=-F33(I)
          FSKY(4,K)=-M13(I)
          FSKY(5,K)=-M23(I)
          FSKY(6,K)=-M33(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
        ENDDO
       ENDIF
      ELSE
       IF (IVECTOR == 1) THEN
        IF(NODADT_THERM == 1) THEN
#include "vectorize.inc"
         DO I=JFT,JLT
           K = IADTG(1,I)
           FSKYV(K,1)=-F11(I)
           FSKYV(K,2)=-F21(I)
           FSKYV(K,3)=-F31(I)
           FSKYV(K,4)=-M11(I)
           FSKYV(K,5)=-M21(I)
           FSKYV(K,6)=-M31(I)
           FSKYV(K,7)=STI(I)
           FSKYV(K,8)=STIR(I)
           FTHESKY(K) = THEM(I,1)
           CONDNSKY(K) = CONDE(I)
           K = IADTG(2,I)
           FSKYV(K,1)=-F12(I)
           FSKYV(K,2)=-F22(I)
           FSKYV(K,3)=-F32(I)
           FSKYV(K,4)=-M12(I)
           FSKYV(K,5)=-M22(I)
           FSKYV(K,6)=-M32(I)
           FSKYV(K,7)=STI(I)
           FSKYV(K,8)=STIR(I)
           FTHESKY(K) = THEM(I,2)
           CONDNSKY(K) = CONDE(I)
           K = IADTG(3,I)
           FSKYV(K,1)=-F13(I)
           FSKYV(K,2)=-F23(I)
           FSKYV(K,3)=-F33(I)
           FSKYV(K,4)=-M13(I)
           FSKYV(K,5)=-M23(I)
           FSKYV(K,6)=-M33(I)
           FSKYV(K,7)=STI(I)
           FSKYV(K,8)=STIR(I)
           FTHESKY(K) = THEM(I,3)
           CONDNSKY(K) = CONDE(I)
         ENDDO
        ELSE
#include "vectorize.inc"
         DO I=JFT,JLT
           K = IADTG(1,I)
           FSKYV(K,1)=-F11(I)
           FSKYV(K,2)=-F21(I)
           FSKYV(K,3)=-F31(I)
           FSKYV(K,4)=-M11(I)
           FSKYV(K,5)=-M21(I)
           FSKYV(K,6)=-M31(I)
           FSKYV(K,7)=STI(I)
           FSKYV(K,8)=STIR(I)
           FTHESKY(K) = THEM(I,1)
           CONDNSKY(K) = CONDE(I)
           K = IADTG(2,I)
           FSKYV(K,1)=-F12(I)
           FSKYV(K,2)=-F22(I)
           FSKYV(K,3)=-F32(I)
           FSKYV(K,4)=-M12(I)
           FSKYV(K,5)=-M22(I)
           FSKYV(K,6)=-M32(I)
           FSKYV(K,7)=STI(I)
           FSKYV(K,8)=STIR(I)
           FTHESKY(K) = THEM(I,2)
           CONDNSKY(K) = CONDE(I)
           K = IADTG(3,I)
           FSKYV(K,1)=-F13(I)
           FSKYV(K,2)=-F23(I)
           FSKYV(K,3)=-F33(I)
           FSKYV(K,4)=-M13(I)
           FSKYV(K,5)=-M23(I)
           FSKYV(K,6)=-M33(I)
           FSKYV(K,7)=STI(I)
           FSKYV(K,8)=STIR(I)
           FTHESKY(K) = THEM(I,3)
           CONDNSKY(K) = CONDE(I)
         ENDDO
        ENDIF
       ELSE
        IF(NODADT_THERM == 1) THEN
         DO I=JFT,JLT
           K = IADTG(1,I)
           FSKY(1,K)=-F11(I)
           FSKY(2,K)=-F21(I)
           FSKY(3,K)=-F31(I)
           FSKY(4,K)=-M11(I)
           FSKY(5,K)=-M21(I)
           FSKY(6,K)=-M31(I)
           FSKY(7,K)=STI(I)
           FSKY(8,K)=STIR(I)
           FTHESKY(K) = THEM(I,1)
           K = IADTG(2,I)
           FSKY(1,K)=-F12(I)
           FSKY(2,K)=-F22(I)
           FSKY(3,K)=-F32(I)
           FSKY(4,K)=-M12(I)
           FSKY(5,K)=-M22(I)
           FSKY(6,K)=-M32(I)
           FSKY(7,K)=STI(I)
           FSKY(8,K)=STIR(I)
           FTHESKY(K) = THEM(I,2)
           K = IADTG(3,I)
           FSKY(1,K)=-F13(I)
           FSKY(2,K)=-F23(I)
           FSKY(3,K)=-F33(I)
           FSKY(4,K)=-M13(I)
           FSKY(5,K)=-M23(I)
           FSKY(6,K)=-M33(I)
           FSKY(7,K)=STI(I)
           FSKY(8,K)=STIR(I)
           FTHESKY(K) = THEM(I,3)
         ENDDO
        ELSE
         DO I=JFT,JLT
           K = IADTG(1,I)
           FSKY(1,K)=-F11(I)
           FSKY(2,K)=-F21(I)
           FSKY(3,K)=-F31(I)
           FSKY(4,K)=-M11(I)
           FSKY(5,K)=-M21(I)
           FSKY(6,K)=-M31(I)
           FSKY(7,K)=STI(I)
           FSKY(8,K)=STIR(I)
           FTHESKY(K) = THEM(I,1)
           K = IADTG(2,I)
           FSKY(1,K)=-F12(I)
           FSKY(2,K)=-F22(I)
           FSKY(3,K)=-F32(I)
           FSKY(4,K)=-M12(I)
           FSKY(5,K)=-M22(I)
           FSKY(6,K)=-M32(I)
           FSKY(7,K)=STI(I)
           FSKY(8,K)=STIR(I)
           FTHESKY(K) = THEM(I,2)
           K = IADTG(3,I)
           FSKY(1,K)=-F13(I)
           FSKY(2,K)=-F23(I)
           FSKY(3,K)=-F33(I)
           FSKY(4,K)=-M13(I)
           FSKY(5,K)=-M23(I)
           FSKY(6,K)=-M33(I)
           FSKY(7,K)=STI(I)
           FSKY(8,K)=STIR(I)
           FTHESKY(K) = THEM(I,3)
         ENDDO
        ENDIF
       ENDIF

      ENDIF
C-----------
      RETURN
      END
