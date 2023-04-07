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
Chd|  MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        GETHKT3                       source/elements/solid/solide8z/mmats.F
Chd|        SKTCONS2                      source/elements/solid/solide8z/mmats.F
Chd|        SKTVONM3                      source/elements/solid/solide8z/mmats.F
Chd|====================================================================
      SUBROUTINE MMSTIFS(
     1   PM,      MAT,     HH,      VOL,
     2   ICSIG,   DD,      GG,      DG,
     3   G33,     DM,      GM,      DGM,
     4   IORTH,   SIG,     IR,      IS,
     5   IT,      NEL,     JHBE,    MTN)
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: JHBE
      INTEGER ICSIG,IORTH,IR,IS,IT,NEL
      INTEGER MAT(*)
C     REAL
      my_real
     .   PM(NPROPM,*),HH(2,*),DD(3,3,*),GG(*),VOL(*),
     .   DG(3,3,*),G33(3,3,*),DM(3,3,*),GM(3,3,*),DGM(3,3,*),
     .   SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ICS,ICT,ICR,J,K
C     REAL
      my_real
     .   TT,TV,TT0,G(MVSIZ),QH(MVSIZ),EE,SFAC
C-----------------------------------------------
      SFAC=ONE
      IF (ISMDISP>0) SFAC=EM05
      IF (IORTH>0) THEN
       IF (IKT==3) THEN
          DO I=1,NEL
C           QH(I)=GM(1,1,I)
candr           write(*,*)' QH(I)=', QH(I)!.. contains H modulus ..
candr ... H module is wrong here, it is due to an 'unclean'
candr ... passing it from routine 'mmats'
candr ... setting it 'by hand' proves that everything else
candr ... is correct
candr     QH(I)=1949.29d0 ! .. H for tension-torsion test ..

c           G(I) = HH(2,I)
          ENDDO
         DO J = 1, 3
         DO K = 1, 3
          DO I=1,NEL
           DGM(J,K,I)=ZERO
           GM(J,K,I)=ZERO
          ENDDO
         ENDDO
         ENDDO
         CALL GETHKT3(
     1   1,       NEL,     QH,      IR,
     2   IS,      IT,      MTN)
         CALL SKTVONM3(1 ,NEL ,DM  ,HH   ,SIG  ,
     .                 DGM ,GM  ,QH  ,NEL  )
       ELSEIF (IKT==4) THEN
          DO I=1,NEL
           QH(I)= HH(1,I)
           G(I) = HH(2,I)
          ENDDO
        CALL SKTCONS2(
     1   1,       NEL,     DM,      QH,
     2   G,       DGM,     GM,      IR,
     3   IS,      IT,      NEL,     MTN)
       END IF
        DO J = 1, 3
        DO K = J, 3
         DO I=1,NEL
          DD(J,K,I)=DM(J,K,I)*VOL(I)
          G33(J,K,I)=GM(J,K,I)*VOL(I)
          DD(K,J,I)=DD(J,K,I)
          G33(K,J,I)=G33(J,K,I)
         ENDDO
        ENDDO
        ENDDO
candr (DG is non-symmetric)
        DO J = 1, 3
        DO K = 1, 3
         DO I=1,NEL
          DG(J,K,I)=DGM(J,K,I)*VOL(I)
         ENDDO
        ENDDO
        ENDDO

      ELSE
      IF (JHBE==15) THEN
       DO I=1,NEL
         TV=HH(1,I)*VOL(I)
         GG(I)=HH(2,I)*VOL(I)
         TT0=ONE/(TV+TWO*GG(I))
         TT=FOUR*GG(I)*(TV+GG(I))*TT0
         TV=TWO*GG(I)*TV*TT0
         EE=GG(I)*(THREE*HH(1,I)+TWO*HH(2,I))/(HH(1,I)+HH(2,I))
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=SFAC*EE
         DD(1,2,I)=TV
         DD(1,3,I)=ZERO
         DD(2,1,I)=TV
         DD(2,3,I)=ZERO
         DD(3,1,I)=ZERO
         DD(3,2,I)=ZERO
       ENDDO
      ELSEIF (JHBE==14.AND.ICSIG>0) THEN
       ICS=ICSIG/100
       ICT=MOD(ICSIG/10,10)
       ICR=MOD(ICSIG,10)
       DO I=1,NEL
         TV=HH(1,I)*VOL(I)
         GG(I)=HH(2,I)*VOL(I)
         TT0=ONE/(TV+TWO*GG(I))
         TT=FOUR*GG(I)*(TV+GG(I))*TT0
         TV=TWO*GG(I)*TV*TT0
         EE=GG(I)*(THREE*HH(1,I)+TWO*HH(2,I))/(HH(1,I)+HH(2,I))
         DD(1,1,I)=TT
         DD(2,2,I)=TV
         DD(3,3,I)=SFAC*EE
       ENDDO
       IF (ICS==1) THEN
        DO I=1,NEL
         TT=DD(1,1,I)
         TV=DD(2,2,I)
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=ZERO
         DD(2,1,I)=TV
         DD(2,3,I)=ZERO
         DD(3,1,I)=ZERO
         DD(3,2,I)=ZERO
        ENDDO
       ELSEIF (ICT==1) THEN
        DO I=1,NEL
         TT=DD(1,1,I)
         TV=DD(2,2,I)
         DD(1,1,I)=TT
         DD(2,2,I)=DD(3,3,I)
         DD(3,3,I)=TT
         DD(1,2,I)=ZERO
         DD(1,3,I)=TV
         DD(2,1,I)=ZERO
         DD(2,3,I)=ZERO
         DD(3,1,I)=TV
         DD(3,2,I)=ZERO
        ENDDO
       ELSEIF (ICR==1) THEN
        DO I=1,NEL
         TT=DD(1,1,I)
         TV=DD(2,2,I)
         DD(1,1,I)=DD(3,3,I)
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=ZERO
         DD(1,3,I)=ZERO
         DD(2,1,I)=ZERO
         DD(2,3,I)=TV
         DD(3,1,I)=ZERO
         DD(3,2,I)=TV
        ENDDO
       ENDIF
      ELSE
       DO I=1,NEL
         TV=HH(1,I)*VOL(I)
         GG(I)=HH(2,I)*VOL(I)
         TT=TV+TWO*GG(I)
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,1,I)=TV
         DD(2,3,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
       ENDDO
      ENDIF
      END IF !(IORTH>0) THEN
C
      RETURN
      END
