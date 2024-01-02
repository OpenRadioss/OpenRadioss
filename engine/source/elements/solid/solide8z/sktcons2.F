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
Chd|  SKTCONS2                      source/elements/solid/solide8z/sktcons2.F
Chd|-- called by -----------
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmstifs.F
Chd|-- calls ---------------
Chd|        SKTVONM4                      source/elements/solid/solide8z/sktvonm4.F
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SKTCONS2(
     1   JFT,     JLT,     CC,      LAMDA,
     2   G,       CG,      G33,     IR,
     3   IS,      IT,      NEL,     MTN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: MTN
      INTEGER JFT, JLT  ,IR,IS,IT
C     REAL
      my_real
     .   LAMDA(*),G(*),CC(3,3,*),CG(3,3,*),G33(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,K,IPLAST
C     REAL
      my_real
     .   NU,SIG(6,MVSIZ),ALPHA(MVSIZ),G2,TT,TV
      TYPE(L_KTBUFEP_)    , POINTER :: LBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
C-----------------------------------------------
        MTAG => KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)
        LBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IR,IS,IT)
        IF (MTAG%L_A_KT>0) THEN
         DO I=JFT,JLT
c ...... SIG contains normalized deviatoric stresses ...
          J=6*(I-1)
          SIG(1,I)= LBUF%SIGE(J+1)
          SIG(2,I)= LBUF%SIGE(J+2)
          SIG(3,I)= LBUF%SIGE(J+3)
          SIG(4,I)= LBUF%SIGE(J+4)
          SIG(5,I)= LBUF%SIGE(J+5)
          SIG(6,I)= LBUF%SIGE(J+6)
c .....  ALPHA(I) is the alpha constant of the radial return..
          ALPHA(I)= LBUF%A_KT(I)
         ENDDO
        ELSE
         DO I=JFT,JLT
          ALPHA(I)= ONE
         ENDDO
        END IF !(MTAG%L_A_KT>0) THEN
        DO I=JFT,JLT
         TV=LAMDA(I)+TWO_THIRD*G(I)*(ONE-ALPHA(I))
         TT=TV+TWO*G(I)*ALPHA(I)
         CC(1,1,I)=TT
         CC(2,2,I)=TT
         CC(3,3,I)=TT
         CC(1,2,I)=TV
         CC(1,3,I)=TV
         CC(2,3,I)=TV
         CC(2,1,I)=TV
         CC(3,1,I)=TV
         CC(3,2,I)=TV
        ENDDO
         DO J = 1, 3 
         DO K = 1, 3 
          DO I=JFT,JLT
           CG(J,K,I)=ZERO
           G33(J,K,I)=ZERO
          ENDDO
         ENDDO
         ENDDO
        DO I=JFT,JLT
         G2 = G(I)*ALPHA(I)
         G33(1,1,I)=G2
         G33(2,2,I)=G2
         G33(3,3,I)=G2
        ENDDO
        CALL SKTVONM4(1 ,NEL ,CC  ,SIG  ,CG  ,
     .                G33 ,ALPHA  )
C
      RETURN
      END
