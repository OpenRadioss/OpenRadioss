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
C  46d11+++
Chd|====================================================================
Chd|  LASERP1                       source/loads/laser/laserp.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        LASERP2                       source/loads/laser/laserp.F   
Chd|====================================================================
      SUBROUTINE LASERP1(LAS ,CEP, IXQ)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LAS(*), IXQ(7,*), CEP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K1, NL, NS1, NS2
C
      K1 = 1
      DO 200 I=1,NLASER
        NL = LAS(K1)
        CALL LASERP2(NL ,LAS(K1+6) ,CEP, IXQ)
        K1 = K1 + 6 + 2*(NL+1)
 200  CONTINUE
C
      RETURN
      END
C
Chd|====================================================================
Chd|  LASERP2                       source/loads/laser/laserp.F   
Chd|-- called by -----------
Chd|        LASERP1                       source/loads/laser/laserp.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LASERP2(NL ,LAS ,CEP ,IXQ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NL, CEP(*), LAS(2,*),IXQ(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL, NG, II, NEXT
C--------------------------------------------------
C RECHERCHE DE L'ELEMENT ET AFFECTATION A P0
C--------------------------------------------------
      DO IL=1,NL+1
        NEXT = LAS(1,IL)
        DO II = 1, NUMELQ
          IF(IXQ(7,II)==NEXT) THEN
C           LAS(1,IL) = NG
C           LAS(2,IL) = I
C  46d11 on force les elements laser sur p0
            CEP(II) = 0
            GOTO 180
          ENDIF
        ENDDO
  180   CONTINUE
C
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  LASERP3                       source/loads/laser/laserp.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        LASERP4                       source/loads/laser/laserp.F   
Chd|====================================================================
      SUBROUTINE LASERP3(LAS ,IPARG)
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
      INTEGER LAS(*), IPARG(NPARG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K1, NL
C
      K1 = 1
      DO 200 I=1,NLASER
        NL = LAS(K1)
        CALL LASERP4(NL ,LAS(K1+6) ,IPARG)
        K1 = K1 + 6 + 2*(NL+1)
 200  CONTINUE
C
      RETURN
      END
C
Chd|====================================================================
Chd|  LASERP4                       source/loads/laser/laserp.F   
Chd|-- called by -----------
Chd|        LASERP3                       source/loads/laser/laserp.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LASERP4(NL ,LAS ,IPARG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NL, LAS(2,*), IPARG(NPARG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL, NG, NN, NG_L
C--------------------------------------------------
C Renumerotation locale du no de groupe pour proc0
C--------------------------------------------------
      DO IL=1,NL+1
       NG = LAS(1,IL)
       NG_L = 0
       DO NN = 1, NG
         IF(IPARG(32,NN)==0) NG_L = NG_L + 1
       ENDDO
       LAS(1,IL) = NG_L
C
      ENDDO
C
      RETURN
      END
