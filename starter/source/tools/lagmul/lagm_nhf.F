Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    lagm_nhf   ../starter/source/tools/lagmul/lagm_nhf.F
!||--- called by ------------------------------------------------------
!||    lectur     ../starter/source/starter/lectur.F
!||====================================================================
      SUBROUTINE LAGM_NHF(NCF, IADLL  ,JLL    ,LLL    ,JCIHF  )
C----------------------------------------------------------------
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
      INTEGER NCF
      INTEGER JCIHF(*), IADLL(*), JLL(*), LLL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LTSM(6,NUMNOD)
      INTEGER IC, IK, JC, IHF, HIJ
C======================================================================|
      LTSM = 0
      IHF  = 1
      DO IC=1,NCF
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = 1
        ENDDO
        DO JC=IC+1,NCF
          HIJ = 0
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF (HIJ > 0) THEN
            JCIHF(IHF) = JC
            IHF        = IHF + 1
          ENDIF
        ENDDO
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = 0
        ENDDO
      ENDDO
C-----------
      RETURN
      END
      
