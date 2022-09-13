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
Chd|  CUPDT3F                       source/elements/shell/coque/cupdt3.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        DOUBLE_FLOT_IEEE              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE CUPDT3F(JFT   ,JLT  ,I8F   ,I8M   ,NVC    ,
     2                  OFFG   ,OFF  ,STI   ,STIR  ,I8STIFN,
     3                  I8STIFR,IXC  ,PM    ,AREA  ,THK    ,
     4                  F11    ,F12  ,F13   ,F14   ,F21    ,
     5                  F22    ,F23  ,F24   ,F31   ,F32    ,
     6                  F33    ,F34  ,M11   ,M12   ,M13    ,
     7                  M14    ,M21  ,M22   ,M23   ,M24    ,
     8                  M31    ,M32  ,M33   ,M34   ,EINT   ,
     9                  PARTSAV,MAT  ,IPARTC)
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
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NVC
      INTEGER IXC(NIXC,MVSIZ),MAT(MVSIZ),IPARTC(*)
      integer*8 I8F(3,3,*), I8M(3,3,*), I8STIFN(3,*), I8STIFR(3,*)
C     REAL
      my_real
     .   OFFG(*), OFF(*), STI(*), STIR(*), 
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   EINT(JLT,2),PM(NPROPM,*),PARTSAV(NPSAV,*) ,AREA(*) ,THK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer*8 
     .  I8STI(3,MVSIZ), I8STIR(3,MVSIZ), 
     .  I8F11(3,MVSIZ), I8F12(3,MVSIZ), I8F13(3,MVSIZ), I8F14(3,MVSIZ),
     .  I8F21(3,MVSIZ), I8F22(3,MVSIZ), I8F23(3,MVSIZ), I8F24(3,MVSIZ),
     .  I8F31(3,MVSIZ), I8F32(3,MVSIZ), I8F33(3,MVSIZ), I8F34(3,MVSIZ),
     .  I8M11(3,MVSIZ), I8M12(3,MVSIZ), I8M13(3,MVSIZ), I8M14(3,MVSIZ),
     .  I8M21(3,MVSIZ), I8M22(3,MVSIZ), I8M23(3,MVSIZ), I8M24(3,MVSIZ),
     .  I8M31(3,MVSIZ), I8M32(3,MVSIZ), I8M33(3,MVSIZ), I8M34(3,MVSIZ)
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, NVC4, I, J,N,IOFF,MX,MT
      my_real OFF_L
C=======================================================================
C cumul de l'energie des elements deletes AU moment du delete
      IOFF=0
      DO I=JFT,JLT
        IF(OFF(I) == 0.0.AND.OFFG(I) > 0.)IOFF=1
      ENDDO
      IF(IOFF == 1)THEN
       MT = MAT(JFT)
       DO I=JFT,JLT
        IF(OFF(I) == 0.0.AND.OFFG(I) > 0.)THEN
           MX = IPARTC(I)
           PARTSAV(24,MX)=PARTSAV(24,MX) + EINT(I,1) + EINT(I,2)
           PARTSAV(25,MX)=PARTSAV(25,MX) + PM(1,MT)*AREA(I)*THK(I)
        ENDIF
       ENDDO
      ENDIF
      OFF_L = ZERO
      DO I=JFT,JLT
        IF(OFF(I)<1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<0.)THEN
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
         ENDIF
        ENDDO
      ENDIF
C
      NVC1= NVC/8
      NVC2=(NVC-NVC1*8)/4
      NVC3=(NVC-NVC1*8-NVC2*4)/2
      NVC4=(NVC-NVC1*8-NVC2*4-NVC3*2)
C
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F11,F11,I8F11)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F12,F12,I8F12)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F13,F13,I8F13)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F14,F14,I8F14)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F21,F21,I8F21)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F22,F22,I8F22)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F23,F23,I8F23)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F24,F24,I8F24)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F31,F31,I8F31)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F32,F32,I8F32)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F33,F33,I8F33)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,F34,F34,I8F34)
C
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M11,M11,I8M11)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M12,M12,I8M12)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M13,M13,I8M13)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M14,M14,I8M14)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M21,M21,I8M21)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M22,M22,I8M22)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M23,M23,I8M23)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M24,M24,I8M24)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M31,M31,I8M31)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M32,M32,I8M32)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M33,M33,I8M33)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,M34,M34,I8M34)
C
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,STI,STI,I8STI)
      CALL DOUBLE_FLOT_IEEE(JFT,JLT,STIR,STIR,I8STIR)
C
      IF(NVC1 == 0)THEN
#include "vectorize.inc"
       DO I=JFT,JLT
c        F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
c        F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
c        F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
c        M(1,N)=M(1,N)-M11(I)
c        M(2,N)=M(2,N)-M21(I)
c        M(3,N)=M(3,N)-M31(I)
c        STIFN(N)=STIFN(N)+STI(I)
c        STIFR(N)=STIFR(N)+STIR(I)
c         <ent1.dec1,dec2> = <ent1.dec1,dec2> - F11(I)
          N = IXC(2,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F11(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F11(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F11(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F21(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F21(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F21(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F31(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F31(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F31(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M11(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M11(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M11(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M21(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M21(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M21(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M31(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M31(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M31(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
C
      ELSE
       DO I=JFT,JLT
          N = IXC(2,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F11(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F11(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F11(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F21(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F21(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F21(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F31(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F31(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F31(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M11(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M11(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M11(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M21(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M21(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M21(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M31(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M31(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M31(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ENDIF
C
      IF(NVC2 == 0)THEN
#include "vectorize.inc"
       DO I=JFT,JLT
c        F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
c        F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
c        F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
c        M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
c        M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
c        M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
c        STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
c        STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
          N = IXC(3,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F12(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F12(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F12(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F22(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F22(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F22(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F32(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F32(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F32(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M12(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M12(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M12(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M22(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M22(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M22(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M32(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M32(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M32(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ELSE
       DO I=JFT,JLT
          N = IXC(3,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F12(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F12(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F12(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F22(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F22(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F22(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F32(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F32(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F32(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M12(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M12(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M12(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M22(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M22(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M22(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M32(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M32(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M32(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ENDIF
C
      IF(NVC3 == 0)THEN
#include "vectorize.inc"
       DO I=JFT,JLT
c        F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
c        F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
c        F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
c        M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
c        M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
c        M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
c        STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
c        STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
          N = IXC(4,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F13(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F13(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F13(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F23(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F23(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F23(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F33(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F33(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F33(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M13(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M13(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M13(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M23(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M23(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M23(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M33(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M33(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M33(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ELSE
       DO I=JFT,JLT
          N = IXC(4,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F13(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F13(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F13(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F23(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F23(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F23(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F33(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F33(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F33(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M13(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M13(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M13(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M23(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M23(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M23(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M33(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M33(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M33(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ENDIF
C
      IF(NVC4 == 0)THEN
#include "vectorize.inc"
       DO I=JFT,JLT
c        F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
c        F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
c        F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
c        M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
c        M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
c        M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
c        STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
c        STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)
          N = IXC(5,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F14(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F14(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F14(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F24(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F24(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F24(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F34(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F34(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F34(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M14(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M14(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M14(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M24(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M24(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M24(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M34(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M34(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M34(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) - I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) - I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) - I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) - I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) - I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) - I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ELSE
       DO I=JFT,JLT
          N = IXC(5,I)
c___________________________________________________          
          I8F(1,1,N) = I8F(1,1,N) - I8F14(1,I) 
          I8F(2,1,N) = I8F(2,1,N) - I8F14(2,I)
          I8F(3,1,N) = I8F(3,1,N) - I8F14(3,I)
c___________________________________________________          
          I8F(1,2,N) = I8F(1,2,N) - I8F24(1,I)
          I8F(2,2,N) = I8F(2,2,N) - I8F24(2,I)
          I8F(3,2,N) = I8F(3,2,N) - I8F24(3,I)
c___________________________________________________          
          I8F(1,3,N) = I8F(1,3,N) - I8F34(1,I)
          I8F(2,3,N) = I8F(2,3,N) - I8F34(1,I)
          I8F(3,3,N) = I8F(3,3,N) - I8F34(1,I)
c___________________________________________________          
          I8M(1,1,N) = I8M(1,1,N) - I8M14(1,I) 
          I8M(2,1,N) = I8M(2,1,N) - I8M14(2,I)
          I8M(3,1,N) = I8M(3,1,N) - I8M14(3,I)
c___________________________________________________          
          I8M(1,2,N) = I8M(1,2,N) - I8M24(1,I) 
          I8M(2,2,N) = I8M(2,2,N) - I8M24(2,I)
          I8M(3,2,N) = I8M(3,2,N) - I8M24(3,I)
c___________________________________________________          
          I8M(1,3,N) = I8M(1,3,N) - I8M34(1,I)
          I8M(2,3,N) = I8M(2,3,N) - I8M34(2,I)
          I8M(3,3,N) = I8M(3,3,N) - I8M34(3,I)
c___________________________________________________          
          I8STIFN(1,N) = I8STIFN(1,N) + I8STI(1,I) 
          I8STIFN(2,N) = I8STIFN(2,N) + I8STI(2,I) 
          I8STIFN(3,N) = I8STIFN(3,N) + I8STI(3,I) 
c___________________________________________________          
          I8STIFR(1,N) = I8STIFR(1,N) + I8STIR(1,I)
          I8STIFR(2,N) = I8STIFR(2,N) + I8STIR(2,I)
          I8STIFR(3,N) = I8STIFR(3,N) + I8STIR(3,I)
c___________________________________________________          
       ENDDO
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CUPDT3                        source/elements/shell/coque/cupdt3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUPDT3(JFT  ,JLT  ,F      ,M    ,NVC  ,
     2                  OFFG ,OFF  ,STI    ,STIR ,STIFN,
     3                  STIFR,IXC  ,PM     ,AREA ,THK  ,
     4                  F11  ,F12  ,F13    ,F14  ,F21  ,
     5                  F22  ,F23  ,F24    ,F31  ,F32  ,
     6                  F33  ,F34  ,M11    ,M12  ,M13  ,
     7                  M14  ,M21  ,M22    ,M23  ,M24  ,
     8                  M31  ,M32  ,M33    ,M34  ,EINT ,
     9                  PARTSAV,MAT,IPARTC ,JTHE  ,THEM   ,
     A                  FTHE ,CONDN,CONDE)
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
      INTEGER JFT, JLT, NVC, JTHE
      INTEGER IXC(NIXC,MVSIZ),MAT(MVSIZ),IPARTC(*)
C     REAL
      my_real
     .   F(3,*), M(3,*), OFFG(*), OFF(*), STI(*), STIR(*), 
     .   STIFN(*), STIFR(*),
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   EINT(JLT,2),PM(NPROPM,*),PARTSAV(NPSAV,*) ,AREA(*) ,THK(*),
     .   THEM(MVSIZ,4)  ,FTHE(*),CONDN(*),CONDE(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, NVC4, I, J,IOFF,MX, MT
      my_real
     .   OFF_L,CF(MVSIZ)
C=======================================================================
C
C cumul de l'energie des elements deletes AU moment du delete
      IOFF=0
      DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) IOFF=1
      ENDDO
      MT = MAT(JFT)
      IF (IOFF == 1)THEN
       DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) THEN
           MX = IPARTC(I)
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
C
      NVC1= NVC/8
      NVC2=(NVC-NVC1*8)/4
      NVC3=(NVC-NVC1*8-NVC2*4)/2
      NVC4=(NVC-NVC1*8-NVC2*4-NVC3*2)
C
      IF(NVC1 == 0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
         DO I=JFT,JLT
          F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
          F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
          F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
          M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
          M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
          M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
          STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)
          STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)     
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
               STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)
               STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)
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
               STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)
               STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)
               FTHE(IXC(2,I))=FTHE(IXC(2,I)) + THEM(I,1)
            ENDDO 
         ENDIF
        
        ENDIF
C
      ELSE
        IF(JTHE == 0 ) THEN
         DO I=JFT,JLT
          F(1,IXC(2,I))=F(1,IXC(2,I))-F11(I)
          F(2,IXC(2,I))=F(2,IXC(2,I))-F21(I)
          F(3,IXC(2,I))=F(3,IXC(2,I))-F31(I)
          M(1,IXC(2,I))=M(1,IXC(2,I))-M11(I)
          M(2,IXC(2,I))=M(2,IXC(2,I))-M21(I)
          M(3,IXC(2,I))=M(3,IXC(2,I))-M31(I)
          STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)
          STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)
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
               STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)
               STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)
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
               STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI(I)
               STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR(I)
               FTHE(IXC(2,I))=FTHE(IXC(2,I)) + THEM(I,1)
            ENDDO
         ENDIF
        ENDIF 
C
      ENDIF
C
      IF(NVC2 == 0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
         DO I=JFT,JLT
           F(1,IXC(3,I))=F(1,IXC(3,I))-F12(I)
           F(2,IXC(3,I))=F(2,IXC(3,I))-F22(I)
           F(3,IXC(3,I))=F(3,IXC(3,I))-F32(I)
           M(1,IXC(3,I))=M(1,IXC(3,I))-M12(I)
           M(2,IXC(3,I))=M(2,IXC(3,I))-M22(I)
           M(3,IXC(3,I))=M(3,IXC(3,I))-M32(I)
           STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
           STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
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
              STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
              STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
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
              STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
              STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
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
           STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
           STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
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
               STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
               STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
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
               STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI(I)
               STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR(I)
               FTHE(IXC(3,I))=FTHE(IXC(3,I)) + THEM(I,2)
            ENDDO   
         ENDIF           
        ENDIF
C       
      ENDIF
C
      IF(NVC3 == 0)THEN
        IF(JTHE == 0 )THEN
#include "vectorize.inc"
         DO I=JFT,JLT
            F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
            F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
            F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
            M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
            M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
            M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
            STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
            STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
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
               STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
               STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
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
               STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
               STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
               FTHE(IXC(4,I))=FTHE(IXC(4,I)) + THEM(I,3)
            ENDDO  
          ENDIF
        ENDIF
      ELSE
       IF(JTHE == 0 ) THEN
         DO I=JFT,JLT
          F(1,IXC(4,I))=F(1,IXC(4,I))-F13(I)
          F(2,IXC(4,I))=F(2,IXC(4,I))-F23(I)
          F(3,IXC(4,I))=F(3,IXC(4,I))-F33(I)
          M(1,IXC(4,I))=M(1,IXC(4,I))-M13(I)
          M(2,IXC(4,I))=M(2,IXC(4,I))-M23(I)
          M(3,IXC(4,I))=M(3,IXC(4,I))-M33(I)
          STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
          STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
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
               STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
               STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
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
               STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI(I)
               STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR(I)
               FTHE(IXC(4,I))=FTHE(IXC(4,I)) + THEM(I,3)
            ENDDO  
          ENDIF       
        ENDIF
C       
      ENDIF
C
      IF(NVC4 == 0)THEN
       IF(JTHE == 0 ) THEN
#include "vectorize.inc"
         DO I=JFT,JLT
          F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
          F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
          F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
          M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
          M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
          M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
          STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
          STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)
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
               STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
               STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)          
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
               STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
               STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)          
               FTHE(IXC(5,I))=FTHE(IXC(5,I)) + THEM(I,4)
            ENDDO  
          ENDIF
        ENDIF
C     
      ELSE
       IF(JTHE == 0 ) THEN
         DO I=JFT,JLT
           F(1,IXC(5,I))=F(1,IXC(5,I))-F14(I)
           F(2,IXC(5,I))=F(2,IXC(5,I))-F24(I)
           F(3,IXC(5,I))=F(3,IXC(5,I))-F34(I)
           M(1,IXC(5,I))=M(1,IXC(5,I))-M14(I)
           M(2,IXC(5,I))=M(2,IXC(5,I))-M24(I)
           M(3,IXC(5,I))=M(3,IXC(5,I))-M34(I)
           STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
           STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)
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
               STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
               STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)          
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
               STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI(I)
               STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR(I)          
               FTHE(IXC(5,I))=FTHE(IXC(5,I)) + THEM(I,4)
            ENDDO  
          ENDIF       
        ENDIF
C
      ENDIF
C        
      RETURN
      END
C
Chd|====================================================================
Chd|  CUPDT3P                       source/elements/shell/coque/cupdt3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUPDT3P(JFT  ,JLT    ,OFFG   ,OFF   ,STI  ,
     2                   STIR ,FSKY   ,FSKYV  ,IADC  ,IXC  ,
     3                   F11  ,F12    ,F13    ,F14   ,F21  ,
     4                   F22  ,F23    ,F24    ,F31   ,F32  ,
     5                   F33  ,F34    ,M11    ,M12   ,M13  ,
     6                   M14  ,M21    ,M22    ,M23   ,M24  ,
     7                   M31  ,M32    ,M33    ,M34   ,
     8                   EINT ,PARTSAV,MAT    ,IPARTC,PM   ,
     9                   AREA ,THK    ,JTHE   ,THEM,FTHESKY,
     A                   CONDNSKY,CONDE)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, JTHE
      INTEGER IXC(NIXC,MVSIZ),MAT(MVSIZ),IPARTC(*),IADC(4,*)
      my_real
     .   OFFG(*), OFF(*), STI(*), STIR(*), 
     .   FSKYV(LSKY,8), FSKY(8,LSKY)
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),             
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),            
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),            
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),            
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),            
     .   CONDE(MVSIZ),                                              
     .   EINT(JLT,2),PM(NPROPM,*),PARTSAV(NPSAV,*), AREA(*) ,THK(*),  
     .   FTHESKY(LSKY),THEM(MVSIZ,4),CONDNSKY(*)                        
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K, J,IOFF,MX,MT
      my_real OFF_L
C=======================================================================
C cumul de l'energie des elements deletes AU moment du delete
      IOFF=0
      DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) IOFF=1
      ENDDO
      IF (IOFF == 1) THEN
       MT = MAT(JFT)
       DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) THEN
           MX = IPARTC(I)
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
      IF (OFF_L < ZERO) THEN
        DO I=JFT,JLT
         IF (OFFG(I) < ZERO) THEN
           F11(I)  = ZERO
           F21(I)  = ZERO
           F31(I)  = ZERO
           M11(I)  = ZERO
           M21(I)  = ZERO
           M31(I)  = ZERO
           F12(I)  = ZERO
           F22(I)  = ZERO
           F32(I)  = ZERO
           M12(I)  = ZERO
           M22(I)  = ZERO
           M32(I)  = ZERO
           F13(I)  = ZERO
           F23(I)  = ZERO
           F33(I)  = ZERO
           M13(I)  = ZERO
           M23(I)  = ZERO
           M33(I)  = ZERO
           F14(I)  = ZERO
           F24(I)  = ZERO
           F34(I)  = ZERO
           M14(I)  = ZERO
           M24(I)  = ZERO
           M34(I)  = ZERO
           STI(I)  = ZERO
           STIR(I) = ZERO
           CONDE(I)= ZERO
         ENDIF
        ENDDO
      ENDIF
C
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          FSKYV(IADC(1,I),1)=-F11(I)
          FSKYV(IADC(1,I),2)=-F21(I)
          FSKYV(IADC(1,I),3)=-F31(I)
          FSKYV(IADC(1,I),4)=-M11(I)
          FSKYV(IADC(1,I),5)=-M21(I)
          FSKYV(IADC(1,I),6)=-M31(I)
          FSKYV(IADC(1,I),7)=STI(I)
          FSKYV(IADC(1,I),8)=STIR(I)
C
          FSKYV(IADC(2,I),1)=-F12(I)
          FSKYV(IADC(2,I),2)=-F22(I)
          FSKYV(IADC(2,I),3)=-F32(I)
          FSKYV(IADC(2,I),4)=-M12(I)
          FSKYV(IADC(2,I),5)=-M22(I)
          FSKYV(IADC(2,I),6)=-M32(I)
          FSKYV(IADC(2,I),7)=STI(I)
          FSKYV(IADC(2,I),8)=STIR(I)
C
          FSKYV(IADC(3,I),1)=-F13(I)
          FSKYV(IADC(3,I),2)=-F23(I)
          FSKYV(IADC(3,I),3)=-F33(I)
          FSKYV(IADC(3,I),4)=-M13(I)
          FSKYV(IADC(3,I),5)=-M23(I)
          FSKYV(IADC(3,I),6)=-M33(I)
          FSKYV(IADC(3,I),7)=STI(I)
          FSKYV(IADC(3,I),8)=STIR(I)
C
          FSKYV(IADC(4,I),1)=-F14(I)
          FSKYV(IADC(4,I),2)=-F24(I)
          FSKYV(IADC(4,I),3)=-F34(I)
          FSKYV(IADC(4,I),4)=-M14(I)
          FSKYV(IADC(4,I),5)=-M24(I)
          FSKYV(IADC(4,I),6)=-M34(I)
          FSKYV(IADC(4,I),7)=STI(I)
          FSKYV(IADC(4,I),8)=STIR(I)
        ENDDO
C
        IF (JTHE > 0 ) THEN
#include "vectorize.inc"
          DO I=JFT,JLT
            FTHESKY(IADC(1,I)) = THEM(I,1)
            FTHESKY(IADC(2,I)) = THEM(I,2)
            FTHESKY(IADC(3,I)) = THEM(I,3)
            FTHESKY(IADC(4,I)) = THEM(I,4)
          ENDDO   
          IF (NODADT_THERM == 1) THEN  
#include "vectorize.inc"
            DO I=JFT,JLT
               CONDNSKY(IADC(1,I)) = CONDE(I)
               CONDNSKY(IADC(2,I)) = CONDE(I)
               CONDNSKY(IADC(3,I)) = CONDE(I)
               CONDNSKY(IADC(4,I)) = CONDE(I)
            ENDDO
          ENDIF      
        ENDIF
C----------        
      ELSE   ! Scalar  (IVECTOR=0)
c----------
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
c  End of Prefetch
C
Cel  prefetch FSKY
c#include "cupdt3_pf.inc"
          K = IADC(1,I)
          FSKY(1,K)=-F11(I)
          FSKY(2,K)=-F21(I)
          FSKY(3,K)=-F31(I)
          FSKY(4,K)=-M11(I)
          FSKY(5,K)=-M21(I)
          FSKY(6,K)=-M31(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
C
          K = IADC(2,I)
          FSKY(1,K)=-F12(I)
          FSKY(2,K)=-F22(I)
          FSKY(3,K)=-F32(I)
          FSKY(4,K)=-M12(I)
          FSKY(5,K)=-M22(I)
          FSKY(6,K)=-M32(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
C
          K = IADC(3,I)
          FSKY(1,K)=-F13(I)
          FSKY(2,K)=-F23(I)
          FSKY(3,K)=-F33(I)
          FSKY(4,K)=-M13(I)
          FSKY(5,K)=-M23(I)
          FSKY(6,K)=-M33(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
C
          K = IADC(4,I)
          FSKY(1,K)=-F14(I)
          FSKY(2,K)=-F24(I)
          FSKY(3,K)=-F34(I)
          FSKY(4,K)=-M14(I)
          FSKY(5,K)=-M24(I)
          FSKY(6,K)=-M34(I)
          FSKY(7,K)=STI(I)
          FSKY(8,K)=STIR(I)
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
          IF(NODADT_THERM == 1) THEN  
            DO I=JFT,JLT
               CONDNSKY(IADC(1,I)) = CONDE(I)
               CONDNSKY(IADC(2,I)) = CONDE(I)
               CONDNSKY(IADC(3,I)) = CONDE(I)
               CONDNSKY(IADC(4,I)) = CONDE(I)
            ENDDO
          ENDIF      
        ENDIF
C   
      ENDIF
C-----------
      RETURN
      END
