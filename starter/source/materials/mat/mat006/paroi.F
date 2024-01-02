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
Chd|  PAROI                         source/materials/mat/mat006/paroi.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PAROI(PM,IXS,IXQ,ICODE,NALE)
C
C   TAG LES ELEMENTS DE PAROI POUR LA TURBULENCE
C   SI NOEUD FIXE OU LAGRANGIEN
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),ICODE(*),NALE(*)
      my_real PM(NPROPM,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MT, MLAW, JTUR,IC,IC1,IC2,IC3,IC4
C
      IF(NUMELS>0)THEN
       DO 10 I=1,NUMELS
        MT=IXS(1,I)
        IF(MT>0) THEN
         MLAW=NINT(PM(19,MT))
         JTUR=NINT(PM(70,MT))
C        IF(MLAW/=6.OR.JTUR==0) GOTO10
         IF((MLAW==6.AND.JTUR/=0) .OR. MLAW==46) THEN
          DO 11 J=2,9
           IF(NALE(IXS(J,I))==0)IXS(1,I)=-IABS(IXS(1,I))
  11      CONTINUE
         ENDIF
        ENDIF
  10   CONTINUE
      ENDIF
C
      IF(NUMELQ>0)THEN
       DO 20 I=1,NUMELQ
        MT=IXQ(1,I)
        IF(MT>0) THEN
         MLAW=NINT(PM(19,MT))
         JTUR=NINT(PM(70,MT))
C        IF(MLAW/=6.OR.JTUR==0) GOTO20
         IF((MLAW==6.AND.JTUR/=0) .OR. MLAW==46) THEN
          DO 21 J=2,5
           IF(NALE(IXQ(J,I))==0)IXQ(1,I)=-IABS(IXQ(1,I))
  21      CONTINUE
         ENDIF
        ENDIF
  20   CONTINUE
C
      DO I=1,NUMELQ
          MT=IXQ(1,I)
          IF(MT < 1)CYCLE
          MLAW=NINT(PM(19,MT))
          JTUR=NINT(PM(70,MT))
          DO J=2,5
           IF((MLAW==6.AND.JTUR/=0) .OR. MLAW==46)THEN
            IC=ICODE(IXQ(J,I))
            IC1=IC/512
            IC2=(IC-512*IC1)/64
            IC3=(IC-512*IC1-64*IC2)/8
            IC4=(IC-512*IC1-64*IC2-8*IC3)
            IF(IC1==3.OR.IC1==7.OR.IC4==3.OR.IC4==7)
     .        IXQ(1,I)=-IABS(IXQ(1,I))
           ENDIF
          ENDDO
      ENDDO
C
      ENDIF
C
      RETURN
      END
