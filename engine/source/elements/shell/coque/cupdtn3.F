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
Chd|  CUPDTN3                       source/elements/shell/coque/cupdtn3.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUPDTN3(JFT  ,JLT  ,F   ,M   ,NVC  ,
     2                  OFFG ,OFF  ,STI ,STIR,STIFN,
     3                  STIFR,IXC  ,PM  ,AREA ,THK  ,
     4                  F11  ,F12  ,F13 ,F14 ,F21  ,
     5                  F22  ,F23  ,F24 ,F31 ,F32  ,
     6                  F33  ,F34  ,M11 ,M12 ,M13  ,
     7                  M14  ,M21  ,M22 ,M23 ,M24  ,
     8                  M31  ,M32  ,M33 ,M34 ,EINT,
     A                  PARTSAV,MAT,IPARTC,FAC,JTHE,
     B                  THEM ,FTHE ,CONDN,CONDE)
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
#include      "com01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NVC,JTHE
      INTEGER IXC(NIXC,MVSIZ),MAT(MVSIZ),IPARTC(*)
C     REAL
      my_real
     .   F(3,*), M(3,*), OFFG(*), OFF(*), STI(*), STIR(*), 
     .   STIFN(*), STIFR(*),PM(NPROPM,*),
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   EINT(JLT,2),PARTSAV(NPSAV,*),AREA(*) ,THK(*),FAC(MVSIZ,2),
     .   THEM(MVSIZ,4)  ,FTHE(*),CONDN(*),CONDE(*)
CMasParINCLUDE 'cupdt3.intmap.inc'
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, NVC4, I, J, MX,IOFF,MT
      my_real
     .   OFF_L,CF(MVSIZ)
C-----------------------------------------------
C
C cumul de l'energie des elements deletes AU moment du delete
            
      IOFF=0
      DO I=JFT,JLT
        IF(OFF(I)==ZERO.AND.OFFG(I)>ZERO)IOFF=1
      ENDDO
      IF(IOFF==1)THEN
       MT = MAT(JFT)
       DO I=JFT,JLT
        IF(OFF(I)==ZERO.AND.OFFG(I)>ZERO)THEN
           MX = IPARTC(I)
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
      IF(OFF_L<ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I)<ZERO)THEN
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
           F14(I)=ZERO
           F24(I)=ZERO
           F34(I)=ZERO
           M14(I)=ZERO
           M24(I)=ZERO
           M34(I)=ZERO
           STI(I)=ZERO
           STIR(I)=ZERO
           CONDE(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C  140---
C
      NVC1= NVC/8
      NVC2=(NVC-NVC1*8)/4
      NVC3=(NVC-NVC1*8-NVC2*4)/2
      NVC4=(NVC-NVC1*8-NVC2*4-NVC3*2)
C
      IF(NVC1==0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
          DO I=JFT,JLT
             F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
             F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
             F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
             M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
             M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
             M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
             STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)*FAC(I,1)
             STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)*FAC(I,1)
          ENDDO
       ELSE
          IF(NODADT_THERM == 1 ) THEN  
#include "vectorize.inc"
           DO I=JFT,JLT
             F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
             F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
             F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
             M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
             M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
             M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
             STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)*FAC(I,1)
             STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)*FAC(I,1)
             FTHE(IXC(2,I))=FTHE(IXC(2,I)) + THEM(I,1)
             CONDN(IXC(2,I))=CONDN(IXC(2,I))+CONDE(I)
           ENDDO  
          ELSE
#include "vectorize.inc"
           DO I=JFT,JLT
             F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
             F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
             F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
             M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
             M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
             M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
             STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)*FAC(I,1)
             STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)*FAC(I,1)
             FTHE(IXC(2,I))=FTHE(IXC(2,I)) + THEM(I,1)
           ENDDO 
          ENDIF       
        ENDIF

      ELSE
C
       IF(JTHE == 0 ) THEN
          DO I=JFT,JLT
            F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
            F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
            F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
            M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
            M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
            M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
            STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)*FAC(I,1)
            STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)*FAC(I,1)
          ENDDO
       ELSE
          IF(NODADT_THERM == 1 ) THEN  
            DO I=JFT,JLT
              F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
              F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
              F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
              M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
              M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
              M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
              STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)*FAC(I,1)
              STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)*FAC(I,1)        
              FTHE(IXC(2,I))=FTHE(IXC(2,I)) + THEM(I,1)
              CONDN(IXC(2,I))=CONDN(IXC(2,I))+CONDE(I)
            ENDDO  
          ELSE
            DO I=JFT,JLT
              F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
              F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
              F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
              M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
              M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
              M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
              STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)*FAC(I,1)
              STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)*FAC(I,1)        
              FTHE(IXC(2,I))=FTHE(IXC(2,I)) + THEM(I,1)
            ENDDO  
          ENDIF       
       ENDIF
      ENDIF
C
      IF(NVC2==0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
         DO I=JFT,JLT
            F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
            F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
            F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
            M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
            M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
            M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
            STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)*FAC(I,2)
            STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)*FAC(I,2)
         ENDDO  
        ELSE
         IF(NODADT_THERM == 1 ) THEN  
#include "vectorize.inc"
          DO I=JFT,JLT
            F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
            F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
            F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
            M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
            M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
            M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
            STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)*FAC(I,2)
            STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(3,I))=FTHE(IXC(3,I)) + THEM(I,2) 
            CONDN(IXC(3,I))=CONDN(IXC(3,I))+CONDE(I)         
          ENDDO  
         ELSE
#include "vectorize.inc"
          DO I=JFT,JLT
            F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
            F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
            F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
            M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
            M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
            M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
            STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)*FAC(I,2)
            STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(3,I))=FTHE(IXC(3,I)) + THEM(I,2) 
          ENDDO  
         ENDIF       
        ENDIF
C   
      ELSE
       IF(JTHE == 0 ) THEN
         DO I=JFT,JLT
            F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
            F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
            F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
            M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
            M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
            M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
            STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)*FAC(I,2)
            STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)*FAC(I,2)
         ENDDO
        ELSE
         IF(NODADT_THERM == 1 ) THEN  
          DO I=JFT,JLT
            F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
            F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
            F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
            M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
            M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
            M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
            STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)*FAC(I,2)
            STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(3,I))=FTHE(IXC(3,I)) + THEM(I,2) 
            CONDN(IXC(3,I))=CONDN(IXC(3,I))+CONDE(I)         
          ENDDO  
         ELSE
          DO I=JFT,JLT
            F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
            F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
            F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
            M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
            M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
            M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
            STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)*FAC(I,2)
            STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(3,I))=FTHE(IXC(3,I)) + THEM(I,2) 
          ENDDO  
         ENDIF        
        ENDIF
C     
      ENDIF
C
      IF(NVC3==0)THEN
        IF(JTHE == 0) THEN
#include "vectorize.inc"
         DO I=JFT,JLT
           F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
           F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
           F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
           M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
           M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
           M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
           STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)*FAC(I,1)
           STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)*FAC(I,1)
         ENDDO
        ELSE
         IF(NODADT_THERM == 1 ) THEN  
#include "vectorize.inc"
          DO I=JFT,JLT
           F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
           F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
           F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
           M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
           M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
           M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
           STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)*FAC(I,1)
           STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)*FAC(I,1)        
           FTHE(IXC(4,I))=FTHE(IXC(4,I)) + THEM(I,3)
           CONDN(IXC(4,I))=CONDN(IXC(4,I))+CONDE(I)
          ENDDO 
         ELSE
#include "vectorize.inc"
          DO I=JFT,JLT
           F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
           F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
           F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
           M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
           M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
           M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
           STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)*FAC(I,1)
           STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)*FAC(I,1)        
           FTHE(IXC(4,I))=FTHE(IXC(4,I)) + THEM(I,3)
          ENDDO
         ENDIF        
        ENDIF
C    
      ELSE
       IF(JTHE == 0 ) THEN
          DO I=JFT,JLT
             F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
             F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
             F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
             M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
             M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
             M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
             STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)*FAC(I,1)
             STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)*FAC(I,1)
          ENDDO
        ELSE
         IF(NODADT_THERM == 1 ) THEN  
           DO I=JFT,JLT
             F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
             F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
             F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
             M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
             M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
             M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
             STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)*FAC(I,1)
             STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)*FAC(I,1)        
             FTHE(IXC(4,I))=FTHE(IXC(4,I)) + THEM(I,3)
             CONDN(IXC(4,I))=CONDN(IXC(4,I))+CONDE(I)   
           ENDDO
         ELSE
           DO I=JFT,JLT
             F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
             F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
             F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
             M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
             M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
             M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
             STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)*FAC(I,1)
             STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)*FAC(I,1)        
             FTHE(IXC(4,I))=FTHE(IXC(4,I)) + THEM(I,3)
           ENDDO
         ENDIF         
        ENDIF
C       
      ENDIF
C
      IF(NVC4==0)THEN
       IF(JTHE == 0) THEN
#include "vectorize.inc"
           DO I=JFT,JLT
              F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
              F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
              F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
              M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
              M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
              M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
              STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)*FAC(I,2)
              STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)*FAC(I,2)
           ENDDO
         ELSE
         IF(NODADT_THERM == 1 ) THEN  
#include "vectorize.inc"
           DO I=JFT,JLT
            F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
            F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
            F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
            M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
            M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
            M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
            STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)*FAC(I,2)
            STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(5,I))=FTHE(IXC(5,I)) + THEM(I,4)
            CONDN(IXC(5,I))=CONDN(IXC(5,I))+CONDE(I) 
           ENDDO
         ELSE
#include "vectorize.inc"
           DO I=JFT,JLT
            F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
            F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
            F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
            M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
            M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
            M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
            STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)*FAC(I,2)
            STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(5,I))=FTHE(IXC(5,I)) + THEM(I,4)
           ENDDO
         ENDIF         
       
        ENDIF
C     
      ELSE
       IF(JTHE == 0) THEN
         DO I=JFT,JLT
            F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
            F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
            F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
            M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
            M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
            M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
            STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)*FAC(I,2)
            STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)*FAC(I,2)
         ENDDO
        ELSE
         IF(NODADT_THERM == 1 ) THEN  
          DO I=JFT,JLT
            F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
            F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
            F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
            M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
            M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
            M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
            STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)*FAC(I,2)
            STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(5,I))=FTHE(IXC(5,I)) + THEM(I,4)
            CONDN(IXC(5,I))=CONDN(IXC(5,I))+CONDE(I)
          ENDDO
         ELSE
          DO I=JFT,JLT
            F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
            F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
            F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
            M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
            M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
            M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
            STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)*FAC(I,2)
            STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)*FAC(I,2)        
            FTHE(IXC(5,I))=FTHE(IXC(5,I)) + THEM(I,4)
          ENDDO
         ENDIF         
        ENDIF
C        
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CUPDTN3P                      source/elements/shell/coque/cupdtn3.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUPDTN3P(JFT  ,JLT  ,OFFG ,OFF  ,STI ,
     2                   STIR ,FSKY ,FSKYV,IADC ,
     4                   F11  ,F12  ,F13 ,F14 ,F21  ,
     5                   F22  ,F23  ,F24 ,F31 ,F32  ,
     6                   F33  ,F34  ,M11 ,M12 ,M13  ,
     7                   M14  ,M21  ,M22 ,M23 ,M24  ,
     8                   M31  ,M32  ,M33 ,M34 ,IXC,
     A                   EINT ,PARTSAV,MAT,IPARTC,PM,
     B                   AREA ,THK  ,FAC , JTHE , THEM,
     C                   FTHESKY,CONDNSKY,CONDE )  
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
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C ds 41i 001 13/16/00 +1
C     INTEGER JFT, JLT, IADC(4,*)
      INTEGER JFT, JLT, IADC(4,*), JTHE
      INTEGER IXC(NIXC,MVSIZ),MAT(MVSIZ),IPARTC(*)
C     REAL
      my_real
     .   OFFG(*), OFF(*), STI(*), STIR(*),PM(NPROPM,*), 
     .   FSKYV(LSKY,8), FSKY(8,LSKY)
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .     F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .     F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .     M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .     M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .     M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ), 
     .     CONDE(MVSIZ),
     .     EINT(JLT,2),PARTSAV(NPSAV,*), AREA(*) ,THK(*),FAC(MVSIZ,2),
     .     THEM(MVSIZ,4), FTHESKY(LSKY),CONDNSKY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C ds 41i 001 13/16/00 +1
C     INTEGER I, II, K
      INTEGER I, II, K, J, MX,IOFF, MT
      my_real
     .   OFF_L
C-----------------------------------------------
C cumul de l'energie des elements deletes AU moment du delete 
      IOFF=0
      DO I=JFT,JLT
        IF(OFF(I)==ZERO.AND.OFFG(I)>ZERO)IOFF=1
      ENDDO
      IF(IOFF==1)THEN
       MT = MAT(JFT)
       DO I=JFT,JLT
        IF(OFF(I)==ZERO.AND.OFFG(I)>ZERO)THEN
           MX = IPARTC(I)
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
      IF(OFF_L<ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I)<ZERO)THEN
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
           F14(I)=ZERO
           F24(I)=ZERO
           F34(I)=ZERO
           M14(I)=ZERO
           M24(I)=ZERO
           M34(I)=ZERO
           STI(I)=ZERO
           STIR(I)=ZERO
           CONDE(I)=ZERO
         ENDIF
        ENDDO
      ENDIF

      IF (IVECTOR==1) THEN
#include "vectorize.inc"
       DO I=JFT,JLT
          FSKYV(IADC(1,I),1)=-F11(I)
          FSKYV(IADC(1,I),2)=-F21(I)
          FSKYV(IADC(1,I),3)=-F31(I)
          FSKYV(IADC(1,I),4)=-M11(I)
          FSKYV(IADC(1,I),5)=-M21(I)
          FSKYV(IADC(1,I),6)=-M31(I)
          FSKYV(IADC(1,I),7)=STI(I)*FAC(I,1)
          FSKYV(IADC(1,I),8)=STIR(I)*FAC(I,1)
C
          FSKYV(IADC(2,I),1)=-F12(I)
          FSKYV(IADC(2,I),2)=-F22(I)
          FSKYV(IADC(2,I),3)=-F32(I)
          FSKYV(IADC(2,I),4)=-M12(I)
          FSKYV(IADC(2,I),5)=-M22(I)
          FSKYV(IADC(2,I),6)=-M32(I)
          FSKYV(IADC(2,I),7)=STI(I)*FAC(I,2)
          FSKYV(IADC(2,I),8)=STIR(I)*FAC(I,2)
C
          FSKYV(IADC(3,I),1)=-F13(I)
          FSKYV(IADC(3,I),2)=-F23(I)
          FSKYV(IADC(3,I),3)=-F33(I)
          FSKYV(IADC(3,I),4)=-M13(I)
          FSKYV(IADC(3,I),5)=-M23(I)
          FSKYV(IADC(3,I),6)=-M33(I)
          FSKYV(IADC(3,I),7)=STI(I)*FAC(I,1)
          FSKYV(IADC(3,I),8)=STIR(I)*FAC(I,1)
C
          FSKYV(IADC(4,I),1)=-F14(I)
          FSKYV(IADC(4,I),2)=-F24(I)
          FSKYV(IADC(4,I),3)=-F34(I)
          FSKYV(IADC(4,I),4)=-M14(I)
          FSKYV(IADC(4,I),5)=-M24(I)
          FSKYV(IADC(4,I),6)=-M34(I)
          FSKYV(IADC(4,I),7)=STI(I)*FAC(I,2)
          FSKYV(IADC(4,I),8)=STIR(I)*FAC(I,2)
        ENDDO
C
         IF(JTHE > 0 ) THEN
#include "vectorize.inc"
           DO I=JFT,JLT
             FTHESKY(IADC(1,I)) = THEM(I,1)
             FTHESKY(IADC(2,I)) = THEM(I,2)
             FTHESKY(IADC(3,I)) = THEM(I,3)
             FTHESKY(IADC(4,I)) = THEM(I,4)
           ENDDO   
           IF(NODADT_THERM ==1) THEN
#include "vectorize.inc"
            DO I=JFT,JLT
             CONDNSKY(IADC(1,I)) = CONDE(I)
             CONDNSKY(IADC(2,I)) = CONDE(I)
             CONDNSKY(IADC(3,I)) = CONDE(I)
             CONDNSKY(IADC(4,I)) = CONDE(I)
            ENDDO
           ENDIF     
         ENDIF
C   
      ELSE
        DO I=JFT,JLT
C
C  Prefetch test for HP
C
C$DIR PREFETCH IADC(1,I+12)
C$DIR PREFETCH FSKY(1,IADC(1,I+4))
C$DIR PREFETCH FSKY(8,IADC(1,I+4))
C$DIR PREFETCH FSKY(1,IADC(2,I+4))
C$DIR PREFETCH FSKY(8,IADC(2,I+4))
C$DIR PREFETCH FSKY(1,IADC(3,I+4))
C$DIR PREFETCH FSKY(8,IADC(3,I+4))
C$DIR PREFETCH FSKY(1,IADC(4,I+4))
C$DIR PREFETCH FSKY(8,IADC(4,I+4))
C
C  End of Prefetch
C
          K = IADC(1,I)
          FSKY(1,K)=-F11(I)
          FSKY(2,K)=-F21(I)
          FSKY(3,K)=-F31(I)
          FSKY(4,K)=-M11(I)
          FSKY(5,K)=-M21(I)
          FSKY(6,K)=-M31(I)
          FSKY(7,K)=STI(I)*FAC(I,1)
          FSKY(8,K)=STIR(I)*FAC(I,1)
C
          K = IADC(2,I)
          FSKY(1,K)=-F12(I)
          FSKY(2,K)=-F22(I)
          FSKY(3,K)=-F32(I)
          FSKY(4,K)=-M12(I)
          FSKY(5,K)=-M22(I)
          FSKY(6,K)=-M32(I)
          FSKY(7,K)=STI(I)*FAC(I,2)
          FSKY(8,K)=STIR(I)*FAC(I,2)
C
          K = IADC(3,I)
          FSKY(1,K)=-F13(I)
          FSKY(2,K)=-F23(I)
          FSKY(3,K)=-F33(I)
          FSKY(4,K)=-M13(I)
          FSKY(5,K)=-M23(I)
          FSKY(6,K)=-M33(I)
          FSKY(7,K)=STI(I)*FAC(I,1)
          FSKY(8,K)=STIR(I)*FAC(I,1)
C
          K = IADC(4,I)
          FSKY(1,K)=-F14(I)
          FSKY(2,K)=-F24(I)
          FSKY(3,K)=-F34(I)
          FSKY(4,K)=-M14(I)
          FSKY(5,K)=-M24(I)
          FSKY(6,K)=-M34(I)
          FSKY(7,K)=STI(I)*FAC(I,2)
          FSKY(8,K)=STIR(I)*FAC(I,2)
        ENDDO
C
         IF(JTHE > 0 ) THEN
          DO I=JFT,JLT
c#include "cupdt3_pf.inc"
            FTHESKY(IADC(1,I)) = THEM(I,1)
            FTHESKY(IADC(2,I)) = THEM(I,2)
            FTHESKY(IADC(3,I)) = THEM(I,3)
            FTHESKY(IADC(4,I)) = THEM(I,4)
          ENDDO  
           IF(NODADT_THERM ==1) THEN
            DO I=JFT,JLT
             CONDNSKY(IADC(1,I)) = CONDE(I)*FAC(I,1)
             CONDNSKY(IADC(2,I)) = CONDE(I)*FAC(I,2)
             CONDNSKY(IADC(3,I)) = CONDE(I)*FAC(I,1)
             CONDNSKY(IADC(4,I)) = CONDE(I)*FAC(I,2)
            ENDDO
           ENDIF         
         ENDIF
C  
      ENDIF
C
      RETURN
      END
