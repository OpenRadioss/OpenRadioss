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
Chd|  SKTVONM4                      source/elements/solid/solide8z/sktvonm4.F
Chd|-- called by -----------
Chd|        SKTCONS2                      source/elements/solid/solide8z/sktcons2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SKTVONM4(LFT ,LLT ,DD  ,SIG  ,DD1 ,
     .                    GT  ,R   )
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
      INTEGER LFT,LLT
C     REAL
      my_real
     .   DD(3,3,*),SIG(6,*),DD1(3,3,*),GT(3,3,*),R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C---------deviatoric normalized stress: sig
      INTEGER I,J,K
C     REAL
      my_real
     .   SS(3,MVSIZ),ST(3,MVSIZ)
C----------  verifier-------------------------------------
C----------HT(i)>0 
      DO I=LFT,LLT
c .....(R(I) is the alpha parameter of the radial return .. 
       IF (R(I)/=ONE) THEN
c ..... SS(), ST() contain deviatoric stresses ..
        SS(1,I)=SIG(1,I)
        SS(2,I)=SIG(2,I)
        SS(3,I)=SIG(3,I)
        ST(1,I)=SIG(4,I)
        ST(2,I)=SIG(5,I)
        ST(3,I)=SIG(6,I)
c        print *,'DS,ST='
c        print *,SS(1,I),SS(2,I),SS(3,I)
c        print *,ST(1,I),ST(2,I),ST(3,I)
       ENDIF 
      ENDDO
C-----------GT->u-------- 
      DO I=LFT,LLT
c .....(R(I) is the alpha parameter of the radial return ..
c         print *,'[Ce]='
c         print *,DD(1,1,I),DD(1,2,I),GT(1,1,I)
       IF (R(I)/=ONE) THEN
        DO J=1,3
        DO K=J,3
         DD(J,K,I)=DD(J,K,I)-SS(J,I)*SS(K,I)
         GT(J,K,I)=GT(J,K,I)-ST(J,I)*ST(K,I)
         DD(K,J,I)=DD(J,K,I)
         GT(K,J,I)=GT(J,K,I)
        ENDDO
C         IF (DD(J,J,I)<EM10.OR.GT(J,J,I)<EM10) THEN
C          print *,'GT(J,J,I)=',GT(J,J,I)
          DD(J,J,I)=MAX(EM10,DD(J,J,I))
          GT(J,J,I)=MAX(EM10,GT(J,J,I))
C         ENDIF
        ENDDO
        DO J=1,3
        DO K=1,3
         DD1(J,K,I)=-SS(J,I)*ST(K,I)
        ENDDO
        ENDDO
c         print *,'[Ct]='
c         print *,DD(1,1,I),DD(1,2,I),GT(1,1,I)
c         print *,DD1(1,1,I),DD1(2,1,I)
        ENDIF 
      ENDDO
C
      RETURN
      END
