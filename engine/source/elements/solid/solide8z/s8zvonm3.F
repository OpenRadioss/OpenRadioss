Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  S8ZVONM3                      source/elements/solid/solide8z/s8zvonm3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZVONM3(LFT ,LLT ,DD  ,G   ,SIG  ,
     .                    DD1 ,GT  ,HT  ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NEL
C     REAL
      my_real
     .   DD(3,3,*),G(*),SIG(NEL,6),DD1(3,3,*),GT(3,3,*),HT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   SS(3,MVSIZ),ST(3,MVSIZ),P,SIGY49,DS(3,MVSIZ),
     .   SS2,DS2,ST2,NORM(MVSIZ),NORMI,G2
C----------  verifier-------------------------------------
      DO I=LFT,LLT
       G2 = TWO*G(I)
      DO J=1,3
       GT(J,J,I)=G2
      ENDDO
      ENDDO
C----------HT(i)>0 
      DO I=LFT,LLT
       IF (HT(I)>ZERO) THEN
        P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        SS(1,I)=SIG(I,1)+P
        SS(2,I)=SIG(I,2)+P
        SS(3,I)=SIG(I,3)+P
        ST(1,I)=SIG(I,4)
        ST(2,I)=SIG(I,5)
        ST(3,I)=SIG(I,6)
       ENDIF 
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I)>ZERO) THEN
        DS(1,I)=DD(1,1,I)*SS(1,I)+DD(1,2,I)*SS(2,I)+DD(1,3,I)*SS(3,I)
        DS(2,I)=DD(2,1,I)*SS(1,I)+DD(2,2,I)*SS(2,I)+DD(2,3,I)*SS(3,I)
        DS(3,I)=DD(3,1,I)*SS(1,I)+DD(3,2,I)*SS(2,I)+DD(3,3,I)*SS(3,I)
       ENDIF
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I)>ZERO) THEN
        SS2=SS(1,I)**2+SS(2,I)**2+SS(3,I)**2
        ST2=ST(1,I)**2+ST(2,I)**2+ST(3,I)**2
        SIGY49=TWO_THIRD*SS2+TWO_THIRD*ST2
c        SIGY49=TWO_THIRD*SS2+FOUR_OVER_3*ST2
        DS2=DS(1,I)*SS(1,I)+DS(2,I)*SS(2,I)+DS(3,I)*SS(3,I)
        NORM(I)=SQRT(DS2+ST2*G2+SIGY49*HT(I))
c        NORM(I)=SQRT(DS2+ST2*G(I)+SIGY49*HT(I))
       ENDIF
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I)>ZERO) THEN
        NORMI = SQRT(HALF)/MAX(EM20,NORM(I))
        DO J=1,3
         DS(J,I)=DS(J,I)*NORMI
         ST(J,I)=ST(J,I)*G(I)*NORMI
        ENDDO
       ENDIF 
      ENDDO
C-----------GT->u-------- 
      DO I=LFT,LLT
       IF (HT(I)>ZERO) THEN
        DO J=1,3
        DO K=J,3
         DD(J,K,I)=DD(J,K,I)-DS(J,I)*DS(K,I)
         GT(J,K,I)=(GT(J,K,I)-ST(J,I)*ST(K,I))*HALF
         DD(K,J,I)=DD(J,K,I)
         GT(K,J,I)=GT(J,K,I)
        ENDDO
        ENDDO
        DO J=1,3
        DO K=1,3
         DD1(J,K,I)=-DS(J,I)*ST(K,I)*HALF
        ENDDO
        ENDDO
        ENDIF 
      ENDDO
C
      RETURN
      END
