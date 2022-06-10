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
Chd|  SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCUMU3(OFFG,E,NC1, NC2, NC3, NC4,
     2   NC5, NC6, NC7, NC8,STIFN,STI,
     3   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     4   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     5   NVC,AR,FR_WAVE,FR_WAV,
     6   MX1,MY1,MZ1,MX2,MY2,MZ2,MX3,MY3,MZ3,MX4,MY4,MZ4,
     7   MX5,MY5,MZ5,MX6,MY6,MZ6,MX7,MY7,MZ7,MX8,MY8,MZ8,
     8   THEM, FTHE,CONDN,CONDE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*),
     .   NC8(*), NVC
C     REAL
      my_real
     .   OFFG(*),E(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   AR(3,*),FR_WAVE(*),FR_WAV(*),
     .   MX1(*),MY1(*),MZ1(*),MX2(*),MY2(*),MZ2(*),
     .   MX3(*),MY3(*),MZ3(*),MX4(*),MY4(*),MZ4(*),
     .   MX5(*),MY5(*),MZ5(*),MX6(*),MY6(*),MZ6(*),
     .   MX7(*),MY7(*),MZ7(*),MX8(*),MY8(*),MZ8(*),
     .  THEM(MVSIZ,8), FTHE(*),CONDN(*),CONDE(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
      my_real
     .   OFF_L
      INTEGER NVC1,NVC2,NVC3,NVC4,NVC5,NVC6,NVC7,NVC8
C-----------------------------------------------
      OFF_L = ZERO
      DO I=LFT,LLT
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(IPARTSPH==0)THEN
        IF(OFF_L.LT.ZERO)THEN
          DO I=LFT,LLT
           IF(OFFG(I).LT.ZERO)THEN
             F11(I)=ZERO
             F21(I)=ZERO
             F31(I)=ZERO
             F12(I)=ZERO
             F22(I)=ZERO
             F32(I)=ZERO
             F13(I)=ZERO
             F23(I)=ZERO
             F33(I)=ZERO
             F14(I)=ZERO
             F24(I)=ZERO
             F34(I)=ZERO
             F15(I)=ZERO
             F25(I)=ZERO
             F35(I)=ZERO
             F16(I)=ZERO
             F26(I)=ZERO
             F36(I)=ZERO
             F17(I)=ZERO
             F27(I)=ZERO
             F37(I)=ZERO
             F18(I)=ZERO
             F28(I)=ZERO
             F38(I)=ZERO
             STI(I)=ZERO
           ENDIF
          ENDDO
        ENDIF
      ELSE
        IF(OFF_L.LE.ZERO)THEN
          DO I=LFT,LLT
           IF(OFFG(I).LE.ZERO)THEN
C
C case /DT/BRICK/DEL => OFFG==0 &  F,STI/=0
             F11(I)=ZERO
             F21(I)=ZERO
             F31(I)=ZERO
             F12(I)=ZERO
             F22(I)=ZERO
             F32(I)=ZERO
             F13(I)=ZERO
             F23(I)=ZERO
             F33(I)=ZERO
             F14(I)=ZERO
             F24(I)=ZERO
             F34(I)=ZERO
             F15(I)=ZERO
             F25(I)=ZERO
             F35(I)=ZERO
             F16(I)=ZERO
             F26(I)=ZERO
             F36(I)=ZERO
             F17(I)=ZERO
             F27(I)=ZERO
             F37(I)=ZERO
             F18(I)=ZERO
             F28(I)=ZERO
             F38(I)=ZERO
             STI(I)=ZERO
           ENDIF
          ENDDO
        ENDIF
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L.LE.ZERO)THEN
        DO J=1,8
         DO I=LFT,LLT
          IF(OFFG(I).LE.ZERO)THEN
             THEM(I,J)=ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(NODADT_THERM == 1) THEN
        IF(OFF_L.LT.ZERO)THEN
         DO I=LFT,LLT
          IF(OFFG(I).LT.ZERO)THEN
             CONDE(I)=ZERO
          ENDIF
         ENDDO
        ENDIF
       ENDIF  
      ENDIF
      NVC1 = NVC / 128
      NVC2 =(NVC-NVC1*128) / 64
      NVC3 =(NVC-NVC1*128-NVC2*64) / 32
      NVC4 =(NVC-NVC1*128-NVC2*64-NVC3*32)/16
      NVC5 =(NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16)/8
      NVC6 =(NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8)/4
      NVC7 =(NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4)/2
      NVC8 = NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4-NVC7*2
C
C     because 2*Mnodal = 2*Melement/8
      DO I=LFT,LLT
        STI(I)=FOURTH*STI(I)
      END DO
      IF(NODADT_THERM == 1 ) THEN  
        DO I=LFT,LLT
           CONDE(I)=ONE_OVER_8*CONDE(I)
        END DO
      ENDIF
C
C
      IF(JTHE >= 0) THEN
       IF(NVC1.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC1(I))=E(1,NC1(I))+F11(I)
         E(2,NC1(I))=E(2,NC1(I))+F21(I)
         E(3,NC1(I))=E(3,NC1(I))+F31(I)
         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC1(I))=E(1,NC1(I))+F11(I)
         E(2,NC1(I))=E(2,NC1(I))+F21(I)
         E(3,NC1(I))=E(3,NC1(I))+F31(I)
         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC2.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC2(I))=E(1,NC2(I))+F12(I)
         E(2,NC2(I))=E(2,NC2(I))+F22(I)
         E(3,NC2(I))=E(3,NC2(I))+F32(I)
         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC2(I))=E(1,NC2(I))+F12(I)
         E(2,NC2(I))=E(2,NC2(I))+F22(I)
         E(3,NC2(I))=E(3,NC2(I))+F32(I)
         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC3.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC3(I))=E(1,NC3(I))+F13(I)
         E(2,NC3(I))=E(2,NC3(I))+F23(I)
         E(3,NC3(I))=E(3,NC3(I))+F33(I)
         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC3(I))=E(1,NC3(I))+F13(I)
         E(2,NC3(I))=E(2,NC3(I))+F23(I)
         E(3,NC3(I))=E(3,NC3(I))+F33(I)
         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC4.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC4(I))=E(1,NC4(I))+F14(I)
         E(2,NC4(I))=E(2,NC4(I))+F24(I)
         E(3,NC4(I))=E(3,NC4(I))+F34(I)
         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC4(I))=E(1,NC4(I))+F14(I)
         E(2,NC4(I))=E(2,NC4(I))+F24(I)
         E(3,NC4(I))=E(3,NC4(I))+F34(I)
         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC5.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC5(I))=E(1,NC5(I))+F15(I)
         E(2,NC5(I))=E(2,NC5(I))+F25(I)
         E(3,NC5(I))=E(3,NC5(I))+F35(I)
         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC5(I))=E(1,NC5(I))+F15(I)
         E(2,NC5(I))=E(2,NC5(I))+F25(I)
         E(3,NC5(I))=E(3,NC5(I))+F35(I)
         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
       ENDDO
       ENDIF
       IF(NVC6.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC6(I))=E(1,NC6(I))+F16(I)
         E(2,NC6(I))=E(2,NC6(I))+F26(I)
         E(3,NC6(I))=E(3,NC6(I))+F36(I)
         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC6(I))=E(1,NC6(I))+F16(I)
         E(2,NC6(I))=E(2,NC6(I))+F26(I)
         E(3,NC6(I))=E(3,NC6(I))+F36(I)
         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC7.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC7(I))=E(1,NC7(I))+F17(I)
         E(2,NC7(I))=E(2,NC7(I))+F27(I)
         E(3,NC7(I))=E(3,NC7(I))+F37(I)
         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC7(I))=E(1,NC7(I))+F17(I)
         E(2,NC7(I))=E(2,NC7(I))+F27(I)
         E(3,NC7(I))=E(3,NC7(I))+F37(I)
         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
        ENDDO
       ENDIF
      IF(NVC8.EQ.0)THEN
       DO I=LFT,LLT
         E(1,NC8(I))=E(1,NC8(I))+F18(I)
         E(2,NC8(I))=E(2,NC8(I))+F28(I)
         E(3,NC8(I))=E(3,NC8(I))+F38(I)
         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         E(1,NC8(I))=E(1,NC8(I))+F18(I)
         E(2,NC8(I))=E(2,NC8(I))+F28(I)
         E(3,NC8(I))=E(3,NC8(I))+F38(I)
         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
       ENDDO
      ENDIF
CCC
      ELSE
       IF(NODADT_THERM == 1 ) THEN  
        IF(NVC1.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC1(I))=E(1,NC1(I))+F11(I)
         E(2,NC1(I))=E(2,NC1(I))+F21(I)
         E(3,NC1(I))=E(3,NC1(I))+F31(I)
         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
         FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
         CONDN(NC1(I))= CONDN(NC1(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC1(I))=E(1,NC1(I))+F11(I)
         E(2,NC1(I))=E(2,NC1(I))+F21(I)
         E(3,NC1(I))=E(3,NC1(I))+F31(I)
         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
         FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
         CONDN(NC1(I))= CONDN(NC1(I))+ CONDE(I)
        ENDDO
       ENDIF
       IF(NVC2.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC2(I))=E(1,NC2(I))+F12(I)
         E(2,NC2(I))=E(2,NC2(I))+F22(I)
         E(3,NC2(I))=E(3,NC2(I))+F32(I)
         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
         FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
         CONDN(NC2(I))= CONDN(NC2(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC2(I))=E(1,NC2(I))+F12(I)
         E(2,NC2(I))=E(2,NC2(I))+F22(I)
         E(3,NC2(I))=E(3,NC2(I))+F32(I)
         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
         FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
         CONDN(NC2(I))= CONDN(NC2(I))+ CONDE(I)
        ENDDO
       ENDIF
       IF(NVC3.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC3(I))=E(1,NC3(I))+F13(I)
         E(2,NC3(I))=E(2,NC3(I))+F23(I)
         E(3,NC3(I))=E(3,NC3(I))+F33(I)
         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
         FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3)
         CONDN(NC3(I))= CONDN(NC3(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC3(I))=E(1,NC3(I))+F13(I)
         E(2,NC3(I))=E(2,NC3(I))+F23(I)
         E(3,NC3(I))=E(3,NC3(I))+F33(I)
         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
         FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3) 
         CONDN(NC3(I))= CONDN(NC3(I))+ CONDE(I)
        ENDDO
       ENDIF
       IF(NVC4.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC4(I))=E(1,NC4(I))+F14(I)
         E(2,NC4(I))=E(2,NC4(I))+F24(I)
         E(3,NC4(I))=E(3,NC4(I))+F34(I)
         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
         FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4)
         CONDN(NC4(I))= CONDN(NC4(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC4(I))=E(1,NC4(I))+F14(I)
         E(2,NC4(I))=E(2,NC4(I))+F24(I)
         E(3,NC4(I))=E(3,NC4(I))+F34(I)
         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
         FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4)
         CONDN(NC4(I))= CONDN(NC4(I))+ CONDE(I)
        ENDDO
       ENDIF
       IF(NVC5.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC5(I))=E(1,NC5(I))+F15(I)
         E(2,NC5(I))=E(2,NC5(I))+F25(I)
         E(3,NC5(I))=E(3,NC5(I))+F35(I)
         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
         FTHE(NC5(I)) = FTHE(NC5(I)) + THEM(I,5)
         CONDN(NC5(I))= CONDN(NC5(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC5(I))=E(1,NC5(I))+F15(I)
         E(2,NC5(I))=E(2,NC5(I))+F25(I)
         E(3,NC5(I))=E(3,NC5(I))+F35(I)
         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
         FTHE(NC5(I)) = FTHE(NC5(I)) + THEM(I,5)
         CONDN(NC5(I))= CONDN(NC5(I))+ CONDE(I)
       ENDDO
       ENDIF
       IF(NVC6.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC6(I))=E(1,NC6(I))+F16(I)
         E(2,NC6(I))=E(2,NC6(I))+F26(I)
         E(3,NC6(I))=E(3,NC6(I))+F36(I)
         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
         FTHE(NC6(I)) = FTHE(NC6(I)) + THEM(I,6)
         CONDN(NC6(I))= CONDN(NC6(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC6(I))=E(1,NC6(I))+F16(I)
         E(2,NC6(I))=E(2,NC6(I))+F26(I)
         E(3,NC6(I))=E(3,NC6(I))+F36(I)
         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
         FTHE(NC6(I)) = FTHE(NC6(I)) + THEM(I,6)
         CONDN(NC6(I))= CONDN(NC6(I))+ CONDE(I)
        ENDDO
       ENDIF
       IF(NVC7.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC7(I))=E(1,NC7(I))+F17(I)
         E(2,NC7(I))=E(2,NC7(I))+F27(I)
         E(3,NC7(I))=E(3,NC7(I))+F37(I)
         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
         FTHE(NC7(I)) = FTHE(NC7(I)) + THEM(I,7)
         CONDN(NC7(I))= CONDN(NC7(I))+ CONDE(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC7(I))=E(1,NC7(I))+F17(I)
         E(2,NC7(I))=E(2,NC7(I))+F27(I)
         E(3,NC7(I))=E(3,NC7(I))+F37(I)
         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
         FTHE(NC7(I)) = FTHE(NC7(I)) + THEM(I,7)
         CONDN(NC7(I))= CONDN(NC7(I))+ CONDE(I)
        ENDDO
       ENDIF
      IF(NVC8.EQ.0)THEN
       DO I=LFT,LLT
         E(1,NC8(I))=E(1,NC8(I))+F18(I)
         E(2,NC8(I))=E(2,NC8(I))+F28(I)
         E(3,NC8(I))=E(3,NC8(I))+F38(I)
         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
         FTHE(NC8(I)) = FTHE(NC8(I)) + THEM(I,8)
         CONDN(NC8(I))= CONDN(NC8(I))+ CONDE(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         E(1,NC8(I))=E(1,NC8(I))+F18(I)
         E(2,NC8(I))=E(2,NC8(I))+F28(I)
         E(3,NC8(I))=E(3,NC8(I))+F38(I)
         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
         FTHE(NC8(I)) = FTHE(NC8(I)) + THEM(I,8)
         CONDN(NC8(I))= CONDN(NC8(I))+ CONDE(I)
        ENDDO
       ENDIF
C   
       ELSE ! NODADT_THERM
C
        IF(NVC1.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC1(I))=E(1,NC1(I))+F11(I)
         E(2,NC1(I))=E(2,NC1(I))+F21(I)
         E(3,NC1(I))=E(3,NC1(I))+F31(I)
         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
         FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC1(I))=E(1,NC1(I))+F11(I)
         E(2,NC1(I))=E(2,NC1(I))+F21(I)
         E(3,NC1(I))=E(3,NC1(I))+F31(I)
         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
         FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
        ENDDO
       ENDIF
       IF(NVC2.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC2(I))=E(1,NC2(I))+F12(I)
         E(2,NC2(I))=E(2,NC2(I))+F22(I)
         E(3,NC2(I))=E(3,NC2(I))+F32(I)
         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
         FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC2(I))=E(1,NC2(I))+F12(I)
         E(2,NC2(I))=E(2,NC2(I))+F22(I)
         E(3,NC2(I))=E(3,NC2(I))+F32(I)
         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
         FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
        ENDDO
       ENDIF
       IF(NVC3.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC3(I))=E(1,NC3(I))+F13(I)
         E(2,NC3(I))=E(2,NC3(I))+F23(I)
         E(3,NC3(I))=E(3,NC3(I))+F33(I)
         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
         FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC3(I))=E(1,NC3(I))+F13(I)
         E(2,NC3(I))=E(2,NC3(I))+F23(I)
         E(3,NC3(I))=E(3,NC3(I))+F33(I)
         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
         FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3) 
        ENDDO
       ENDIF
       IF(NVC4.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC4(I))=E(1,NC4(I))+F14(I)
         E(2,NC4(I))=E(2,NC4(I))+F24(I)
         E(3,NC4(I))=E(3,NC4(I))+F34(I)
         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
         FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4) 
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC4(I))=E(1,NC4(I))+F14(I)
         E(2,NC4(I))=E(2,NC4(I))+F24(I)
         E(3,NC4(I))=E(3,NC4(I))+F34(I)
         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
         FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4)
        ENDDO
       ENDIF
       IF(NVC5.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC5(I))=E(1,NC5(I))+F15(I)
         E(2,NC5(I))=E(2,NC5(I))+F25(I)
         E(3,NC5(I))=E(3,NC5(I))+F35(I)
         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
         FTHE(NC5(I)) = FTHE(NC5(I)) + THEM(I,5)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC5(I))=E(1,NC5(I))+F15(I)
         E(2,NC5(I))=E(2,NC5(I))+F25(I)
         E(3,NC5(I))=E(3,NC5(I))+F35(I)
         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
         FTHE(NC5(I)) = FTHE(NC5(I)) + THEM(I,5)
       ENDDO
       ENDIF
       IF(NVC6.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC6(I))=E(1,NC6(I))+F16(I)
         E(2,NC6(I))=E(2,NC6(I))+F26(I)
         E(3,NC6(I))=E(3,NC6(I))+F36(I)
         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
         FTHE(NC6(I)) = FTHE(NC6(I)) + THEM(I,6)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC6(I))=E(1,NC6(I))+F16(I)
         E(2,NC6(I))=E(2,NC6(I))+F26(I)
         E(3,NC6(I))=E(3,NC6(I))+F36(I)
         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
         FTHE(NC6(I)) = FTHE(NC6(I)) + THEM(I,6)
        ENDDO
       ENDIF
       IF(NVC7.EQ.0)THEN
        DO I=LFT,LLT
         E(1,NC7(I))=E(1,NC7(I))+F17(I)
         E(2,NC7(I))=E(2,NC7(I))+F27(I)
         E(3,NC7(I))=E(3,NC7(I))+F37(I)
         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
         FTHE(NC7(I)) = FTHE(NC7(I)) + THEM(I,7)
        ENDDO
       ELSE
        DO I=LFT,LLT
         E(1,NC7(I))=E(1,NC7(I))+F17(I)
         E(2,NC7(I))=E(2,NC7(I))+F27(I)
         E(3,NC7(I))=E(3,NC7(I))+F37(I)
         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
         FTHE(NC7(I)) = FTHE(NC7(I)) + THEM(I,7)
        ENDDO
       ENDIF
      IF(NVC8.EQ.0)THEN
       DO I=LFT,LLT
         E(1,NC8(I))=E(1,NC8(I))+F18(I)
         E(2,NC8(I))=E(2,NC8(I))+F28(I)
         E(3,NC8(I))=E(3,NC8(I))+F38(I)
         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
         FTHE(NC8(I)) = FTHE(NC8(I)) + THEM(I,8)
       ENDDO
      ELSE
       DO I=LFT,LLT
         E(1,NC8(I))=E(1,NC8(I))+F18(I)
         E(2,NC8(I))=E(2,NC8(I))+F28(I)
         E(3,NC8(I))=E(3,NC8(I))+F38(I)
         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
         FTHE(NC8(I)) = FTHE(NC8(I)) + THEM(I,8)
        ENDDO
       ENDIF
C
       ENDIF       
      ENDIF
C
      IF(ISROT.NE.0)THEN
       IF(OFF_L.LT.ZERO)THEN
        DO I=LFT,LLT
         IF(OFFG(I).LT.ZERO)THEN
           MX1(I)=0.
           MY1(I)=0.
           MZ1(I)=0.
           MX2(I)=0.
           MY2(I)=0.
           MZ2(I)=0.
           MX3(I)=0.
           MY3(I)=0.
           MZ3(I)=0.
           MX4(I)=0.
           MY4(I)=0.
           MZ4(I)=0.
           MX5(I)=0.
           MY5(I)=0.
           MZ5(I)=0.
           MX6(I)=0.
           MY6(I)=0.
           MZ6(I)=0.
           MX7(I)=0.
           MY7(I)=0.
           MZ7(I)=0.
           MX8(I)=0.
           MY8(I)=0.
           MZ8(I)=0.
         ENDIF
        ENDDO
       ENDIF
#include "lockon.inc"
       DO I=LFT,LLT
         AR(1,NC1(I))=AR(1,NC1(I))+MX1(I)
         AR(2,NC1(I))=AR(2,NC1(I))+MY1(I)
         AR(3,NC1(I))=AR(3,NC1(I))+MZ1(I)
C
         AR(1,NC2(I))=AR(1,NC2(I))+MX2(I)
         AR(2,NC2(I))=AR(2,NC2(I))+MY2(I)
         AR(3,NC2(I))=AR(3,NC2(I))+MZ2(I)
C
         AR(1,NC3(I))=AR(1,NC3(I))+MX3(I)
         AR(2,NC3(I))=AR(2,NC3(I))+MY3(I)
         AR(3,NC3(I))=AR(3,NC3(I))+MZ3(I)
C
         AR(1,NC4(I))=AR(1,NC4(I))+MX4(I)
         AR(2,NC4(I))=AR(2,NC4(I))+MY4(I)
         AR(3,NC4(I))=AR(3,NC4(I))+MZ4(I)
C
         AR(1,NC5(I))=AR(1,NC5(I))+MX5(I)
         AR(2,NC5(I))=AR(2,NC5(I))+MY5(I)
         AR(3,NC5(I))=AR(3,NC5(I))+MZ5(I)
C
         AR(1,NC6(I))=AR(1,NC6(I))+MX6(I)
         AR(2,NC6(I))=AR(2,NC6(I))+MY6(I)
         AR(3,NC6(I))=AR(3,NC6(I))+MZ6(I)
C
         AR(1,NC7(I))=AR(1,NC7(I))+MX7(I)
         AR(2,NC7(I))=AR(2,NC7(I))+MY7(I)
         AR(3,NC7(I))=AR(3,NC7(I))+MZ7(I)
C
         AR(1,NC8(I))=AR(1,NC8(I))+MX8(I)
         AR(2,NC8(I))=AR(2,NC8(I))+MY8(I)
         AR(3,NC8(I))=AR(3,NC8(I))+MZ8(I)
       ENDDO
#include "lockoff.inc"
C
C--------------------------------------------
C     Front wave
C--------------------------------------------
       IF (IFRWV.NE.0)THEN
#include "lockon.inc"
         DO I=LFT,LLT
           IF(FR_WAVE(NC1(I)).EQ.0.0)FR_WAVE(NC1(I))=-FR_WAV(I)
           IF(FR_WAVE(NC2(I)).EQ.0.0)FR_WAVE(NC2(I))=-FR_WAV(I)
           IF(FR_WAVE(NC3(I)).EQ.0.0)FR_WAVE(NC3(I))=-FR_WAV(I)
           IF(FR_WAVE(NC4(I)).EQ.0.0)FR_WAVE(NC4(I))=-FR_WAV(I)
           IF(FR_WAVE(NC5(I)).EQ.0.0)FR_WAVE(NC5(I))=-FR_WAV(I)
           IF(FR_WAVE(NC6(I)).EQ.0.0)FR_WAVE(NC6(I))=-FR_WAV(I)
           IF(FR_WAVE(NC7(I)).EQ.0.0)FR_WAVE(NC7(I))=-FR_WAV(I)
           IF(FR_WAVE(NC8(I)).EQ.0.0)FR_WAVE(NC8(I))=-FR_WAV(I)
c         FR_WAVE(NC1(I)) = MAX(FR_WAVE(NC1(I)),FR_WAV(I))
c         FR_WAVE(NC2(I)) = MAX(FR_WAVE(NC2(I)),FR_WAV(I))
c         FR_WAVE(NC3(I)) = MAX(FR_WAVE(NC3(I)),FR_WAV(I))
c         FR_WAVE(NC4(I)) = MAX(FR_WAVE(NC4(I)),FR_WAV(I))
c         FR_WAVE(NC5(I)) = MAX(FR_WAVE(NC5(I)),FR_WAV(I))
c         FR_WAVE(NC6(I)) = MAX(FR_WAVE(NC6(I)),FR_WAV(I))
c         FR_WAVE(NC7(I)) = MAX(FR_WAVE(NC7(I)),FR_WAV(I))
c         FR_WAVE(NC8(I)) = MAX(FR_WAVE(NC8(I)),FR_WAV(I))
         ENDDO
#include "lockoff.inc"
        ENDIF
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  SCUMU3P                       source/elements/solid/solide/scumu3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCUMU3P(OFFG,STI,FSKY,FSKYV,IADS,
     2   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     3   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     4   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     5   AR,FR_WAVE,FR_WAV,
     6   MX1,MY1,MZ1,MX2,MY2,MZ2,MX3,MY3,MZ3,MX4,MY4,MZ4,
     7   MX5,MY5,MZ5,MX6,MY6,MZ6,MX7,MY7,MZ7,MX8,MY8,MZ8,
     8   THEM,FTHESKY,CONDNSKY,CONDE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*),
     .   NC8(*)
      my_real
     .   OFFG(*),FSKYV(LSKY,8),FSKY(8,LSKY),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   AR(3,*),FR_WAVE(*),FR_WAV(*),
     .   MX1(*),MY1(*),MZ1(*),MX2(*),MY2(*),MZ2(*),
     .   MX3(*),MY3(*),MZ3(*),MX4(*),MY4(*),MZ4(*),
     .   MX5(*),MY5(*),MZ5(*),MX6(*),MY6(*),MZ6(*),
     .   MX7(*),MY7(*),MZ7(*),MX8(*),MY8(*),MZ8(*),
     .   THEM(MVSIZ,8),FTHESKY(*),CONDNSKY(*),CONDE(*)
      INTEGER IADS(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K, J
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = ZERO
      DO I=LFT,LLT
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(IPARTSPH==0)THEN
        IF(OFF_L.LT.ZERO)THEN
          DO I=LFT,LLT
           IF(OFFG(I).LT.ZERO)THEN
             F11(I)=ZERO
             F21(I)=ZERO
             F31(I)=ZERO
             F12(I)=ZERO
             F22(I)=ZERO
             F32(I)=ZERO
             F13(I)=ZERO
             F23(I)=ZERO
             F33(I)=ZERO
             F14(I)=ZERO
             F24(I)=ZERO
             F34(I)=ZERO
             F15(I)=ZERO
             F25(I)=ZERO
             F35(I)=ZERO
             F16(I)=ZERO
             F26(I)=ZERO
             F36(I)=ZERO
             F17(I)=ZERO
             F27(I)=ZERO
             F37(I)=ZERO
             F18(I)=ZERO
             F28(I)=ZERO
             F38(I)=ZERO
           ENDIF
          ENDDO
        ENDIF
      ELSE
        IF(OFF_L.LE.ZERO)THEN
          DO I=LFT,LLT
           IF(OFFG(I).LE.ZERO)THEN
C
C case /DT/BRICK/DEL => OFFG==0 &  F,STI/=0
             F11(I)=ZERO
             F21(I)=ZERO
             F31(I)=ZERO
             F12(I)=ZERO
             F22(I)=ZERO
             F32(I)=ZERO
             F13(I)=ZERO
             F23(I)=ZERO
             F33(I)=ZERO
             F14(I)=ZERO
             F24(I)=ZERO
             F34(I)=ZERO
             F15(I)=ZERO
             F25(I)=ZERO
             F35(I)=ZERO
             F16(I)=ZERO
             F26(I)=ZERO
             F36(I)=ZERO
             F17(I)=ZERO
             F27(I)=ZERO
             F37(I)=ZERO
             F18(I)=ZERO
             F28(I)=ZERO
             F38(I)=ZERO
             STI(I)=ZERO
           ENDIF
          ENDDO
        ENDIF
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L.LE.ZERO)THEN
        DO J=1,8
         DO I=LFT,LLT
          IF(OFFG(I).LE.ZERO)THEN
             THEM(I,J)=ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(NODADT_THERM == 1) THEN
        IF(OFF_L.LT.ZERO)THEN
         DO I=LFT,LLT
          IF(OFFG(I).LT.ZERO)THEN
             CONDE(I)=ZERO
          ENDIF
         ENDDO
        ENDIF
       ENDIF  
      ENDIF

C
C     because 2*Mnodal = 2*Melement/8
      DO I=LFT,LLT
        STI(I)=FOURTH*STI(I)
      END DO
      IF(NODADT_THERM == 1 ) THEN  
        DO I=LFT,LLT
           CONDE(I)=ONE_OVER_8*CONDE(I)
        END DO
      ENDIF
C
      IF(JTHE >= 0) THEN
       IF(IVECTOR.EQ.1) THEN
#include "vectorize.inc"
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKYV(K,1)=F11(I)
          FSKYV(K,2)=F21(I)
          FSKYV(K,3)=F31(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(7,II)
          FSKYV(K,1)=F17(I)
          FSKYV(K,2)=F27(I)
          FSKYV(K,3)=F37(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(2,II)
          FSKYV(K,1)=F12(I)
          FSKYV(K,2)=F22(I)
          FSKYV(K,3)=F32(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(8,II)
          FSKYV(K,1)=F18(I)
          FSKYV(K,2)=F28(I)
          FSKYV(K,3)=F38(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(3,II)
          FSKYV(K,1)=F13(I)
          FSKYV(K,2)=F23(I)
          FSKYV(K,3)=F33(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(5,II)
          FSKYV(K,1)=F15(I)
          FSKYV(K,2)=F25(I)
          FSKYV(K,3)=F35(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(4,II)
          FSKYV(K,1)=F14(I)
          FSKYV(K,2)=F24(I)
          FSKYV(K,3)=F34(I)
          FSKYV(K,7)=STI(I)
C
          K = IADS(6,II)
          FSKYV(K,1)=F16(I)
          FSKYV(K,2)=F26(I)
          FSKYV(K,3)=F36(I)
          FSKYV(K,7)=STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKY(1,K)=F11(I)
          FSKY(2,K)=F21(I)
          FSKY(3,K)=F31(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(7,II)
          FSKY(1,K)=F17(I)
          FSKY(2,K)=F27(I)
          FSKY(3,K)=F37(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(2,II)
          FSKY(1,K)=F12(I)
          FSKY(2,K)=F22(I)
          FSKY(3,K)=F32(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(8,II)
          FSKY(1,K)=F18(I)
          FSKY(2,K)=F28(I)
          FSKY(3,K)=F38(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(3,II)
          FSKY(1,K)=F13(I)
          FSKY(2,K)=F23(I)
          FSKY(3,K)=F33(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(5,II)
          FSKY(1,K)=F15(I)
          FSKY(2,K)=F25(I)
          FSKY(3,K)=F35(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(4,II)
          FSKY(1,K)=F14(I)
          FSKY(2,K)=F24(I)
          FSKY(3,K)=F34(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(6,II)
          FSKY(1,K)=F16(I)
          FSKY(2,K)=F26(I)
          FSKY(3,K)=F36(I)
          FSKY(7,K)=STI(I)
        ENDDO
       ENDIF
C
C   heat transfert
C         
      ELSE
       IF(IVECTOR.EQ.1) THEN
#include "vectorize.inc"
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKYV(K,1)=F11(I)
          FSKYV(K,2)=F21(I)
          FSKYV(K,3)=F31(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,1)       
C
          K = IADS(7,II)
          FSKYV(K,1)=F17(I)
          FSKYV(K,2)=F27(I)
          FSKYV(K,3)=F37(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,7)
C
          K = IADS(2,II)
          FSKYV(K,1)=F12(I)
          FSKYV(K,2)=F22(I)
          FSKYV(K,3)=F32(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,2)
C
          K = IADS(8,II)
          FSKYV(K,1)=F18(I)
          FSKYV(K,2)=F28(I)
          FSKYV(K,3)=F38(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,8)
C
          K = IADS(3,II)
          FSKYV(K,1)=F13(I)
          FSKYV(K,2)=F23(I)
          FSKYV(K,3)=F33(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,3)
C
          K = IADS(5,II)
          FSKYV(K,1)=F15(I)
          FSKYV(K,2)=F25(I)
          FSKYV(K,3)=F35(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,5)
C
          K = IADS(4,II)
          FSKYV(K,1)=F14(I)
          FSKYV(K,2)=F24(I)
          FSKYV(K,3)=F34(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,4)
C
          K = IADS(6,II)
          FSKYV(K,1)=F16(I)
          FSKYV(K,2)=F26(I)
          FSKYV(K,3)=F36(I)
          FSKYV(K,7)=STI(I)
          FTHESKY(K) = THEM(I,6)
        ENDDO
       ELSE
        IF(NODADT_THERM == 1) THEN
         DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKY(1,K)=F11(I)
          FSKY(2,K)=F21(I)
          FSKY(3,K)=F31(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,1)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(7,II)
          FSKY(1,K)=F17(I)
          FSKY(2,K)=F27(I)
          FSKY(3,K)=F37(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,7)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(2,II)
          FSKY(1,K)=F12(I)
          FSKY(2,K)=F22(I)
          FSKY(3,K)=F32(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,2)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(8,II)
          FSKY(1,K)=F18(I)
          FSKY(2,K)=F28(I)
          FSKY(3,K)=F38(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,8)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(3,II)
          FSKY(1,K)=F13(I)
          FSKY(2,K)=F23(I)
          FSKY(3,K)=F33(I)
          FSKY(7,K)=STI(I)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(5,II)
          FSKY(1,K)=F15(I)
          FSKY(2,K)=F25(I)
          FSKY(3,K)=F35(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,5)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(4,II)
          FSKY(1,K)=F14(I)
          FSKY(2,K)=F24(I)
          FSKY(3,K)=F34(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,4)
          CONDNSKY(K) = CONDE(I)
C
          K = IADS(6,II)
          FSKY(1,K)=F16(I)
          FSKY(2,K)=F26(I)
          FSKY(3,K)=F36(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,6)
          CONDNSKY(K) = CONDE(I)
         ENDDO
        ELSE
         DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKY(1,K)=F11(I)
          FSKY(2,K)=F21(I)
          FSKY(3,K)=F31(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,1)
C
          K = IADS(7,II)
          FSKY(1,K)=F17(I)
          FSKY(2,K)=F27(I)
          FSKY(3,K)=F37(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,7)
C
          K = IADS(2,II)
          FSKY(1,K)=F12(I)
          FSKY(2,K)=F22(I)
          FSKY(3,K)=F32(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,2)
C
          K = IADS(8,II)
          FSKY(1,K)=F18(I)
          FSKY(2,K)=F28(I)
          FSKY(3,K)=F38(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,8)
C
          K = IADS(3,II)
          FSKY(1,K)=F13(I)
          FSKY(2,K)=F23(I)
          FSKY(3,K)=F33(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,3)
C
          K = IADS(5,II)
          FSKY(1,K)=F15(I)
          FSKY(2,K)=F25(I)
          FSKY(3,K)=F35(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,5)
C
          K = IADS(4,II)
          FSKY(1,K)=F14(I)
          FSKY(2,K)=F24(I)
          FSKY(3,K)=F34(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,4)
C
          K = IADS(6,II)
          FSKY(1,K)=F16(I)
          FSKY(2,K)=F26(I)
          FSKY(3,K)=F36(I)
          FSKY(7,K)=STI(I)
          FTHESKY(K) = THEM(I,6)
         ENDDO
        ENDIF
       ENDIF      
      
      ENDIF

      IF(ISROT.NE.0)THEN
       IF(OFF_L.LT.ZERO)THEN
        DO I=LFT,LLT
         IF(OFFG(I).LT.ZERO)THEN
           MX1(I)=0.
           MY1(I)=0.
           MZ1(I)=0.
           MX2(I)=0.
           MY2(I)=0.
           MZ2(I)=0.
           MX3(I)=0.
           MY3(I)=0.
           MZ3(I)=0.
           MX4(I)=0.
           MY4(I)=0.
           MZ4(I)=0.
           MX5(I)=0.
           MY5(I)=0.
           MZ5(I)=0.
           MX6(I)=0.
           MY6(I)=0.
           MZ6(I)=0.
           MX7(I)=0.
           MY7(I)=0.
           MZ7(I)=0.
           MX8(I)=0.
           MY8(I)=0.
           MZ8(I)=0.
         ENDIF
        ENDDO
       ENDIF
       IF(IVECTOR.EQ.1) THEN
#include "vectorize.inc"
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKYV(K,4)=MX1(I)
          FSKYV(K,5)=MY1(I)
          FSKYV(K,6)=MZ1(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(2,II)
          FSKYV(K,4)=MX2(I)
          FSKYV(K,5)=MY2(I)
          FSKYV(K,6)=MZ2(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(3,II)
          FSKYV(K,4)=MX3(I)
          FSKYV(K,5)=MY3(I)
          FSKYV(K,6)=MZ3(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(4,II)
          FSKYV(K,4)=MX4(I)
          FSKYV(K,5)=MY4(I)
          FSKYV(K,6)=MZ4(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(5,II)
          FSKYV(K,4)=MX5(I)
          FSKYV(K,5)=MY5(I)
          FSKYV(K,6)=MZ5(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(6,II)
          FSKYV(K,4)=MX6(I)
          FSKYV(K,5)=MY6(I)
          FSKYV(K,6)=MZ6(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(7,II)
          FSKYV(K,4)=MX7(I)
          FSKYV(K,5)=MY7(I)
          FSKYV(K,6)=MZ7(I)
C         FSKYV(K,8)=STIR(I)
C
          K = IADS(8,II)
          FSKYV(K,4)=MX8(I)
          FSKYV(K,5)=MY8(I)
          FSKYV(K,6)=MZ8(I)
C         FSKYV(K,8)=STIR(I)
C
        ENDDO
       ELSE
#include "vectorize.inc"
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKY(4,K)=MX1(I)
          FSKY(5,K)=MY1(I)
          FSKY(6,K)=MZ1(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(2,II)
          FSKY(4,K)=MX2(I)
          FSKY(5,K)=MY2(I)
          FSKY(6,K)=MZ2(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(3,II)
          FSKY(4,K)=MX3(I)
          FSKY(5,K)=MY3(I)
          FSKY(6,K)=MZ3(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(4,II)
          FSKY(4,K)=MX4(I)
          FSKY(5,K)=MY4(I)
          FSKY(6,K)=MZ4(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(5,II)
          FSKY(4,K)=MX5(I)
          FSKY(5,K)=MY5(I)
          FSKY(6,K)=MZ5(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(6,II)
          FSKY(4,K)=MX6(I)
          FSKY(5,K)=MY6(I)
          FSKY(6,K)=MZ6(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(7,II)
          FSKY(4,K)=MX7(I)
          FSKY(5,K)=MY7(I)
          FSKY(6,K)=MZ7(I)
C         FSKY(8,K)=STIR(I)
C
          K = IADS(8,II)
          FSKY(4,K)=MX8(I)
          FSKY(5,K)=MY8(I)
          FSKY(6,K)=MZ8(I)
C         FSKY(8,K)=STIR(I)
C
        ENDDO
       ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
       IF (IFRWV.NE.0)THEN
#include "lockon.inc"
         DO I=LFT,LLT
           IF(FR_WAVE(NC1(I)).EQ.0.0)FR_WAVE(NC1(I))=-FR_WAV(I)
           IF(FR_WAVE(NC2(I)).EQ.0.0)FR_WAVE(NC2(I))=-FR_WAV(I)
           IF(FR_WAVE(NC3(I)).EQ.0.0)FR_WAVE(NC3(I))=-FR_WAV(I)
           IF(FR_WAVE(NC4(I)).EQ.0.0)FR_WAVE(NC4(I))=-FR_WAV(I)
           IF(FR_WAVE(NC5(I)).EQ.0.0)FR_WAVE(NC5(I))=-FR_WAV(I)
           IF(FR_WAVE(NC6(I)).EQ.0.0)FR_WAVE(NC6(I))=-FR_WAV(I)
           IF(FR_WAVE(NC7(I)).EQ.0.0)FR_WAVE(NC7(I))=-FR_WAV(I)
           IF(FR_WAVE(NC8(I)).EQ.0.0)FR_WAVE(NC8(I))=-FR_WAV(I)
       ENDDO
#include "lockoff.inc"
       ENDIF
      ENDIF
C
      RETURN
      END
