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
Chd|  ACTIV_XFEM                    source/elements/xfem/activ_xfem.F
Chd|-- called by -----------
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE ACTIV_XFEM(IPARG ,NFT   ,JFT    ,JLT ,NXLAY  ,
     .                      NG    ,ELCUTC,IEL_CRK,ITY ,CRKEDGE)
C-----------------------------------------------
      USE CRACKXFEM_MOD
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY,IPARG(NPARG,*),NG,
     .        IEL_CRK(*),ELCUTC(2,*),ITY
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ELCRK,IELCUT,ILAYCUT,ILAY,IGON
      INTEGER ISHEON(MVSIZ)
C=======================================================================
      DO I=JFT,JLT
        IELCUT = ELCUTC(1,I+NFT)
        ISHEON(I) = 0
        IF (IELCUT /= 0) THEN
          ELCRK = IEL_CRK(I+NFT)
          IF (ITY == 7) ELCRK = ELCRK + ECRKXFEC
          DO ILAY = 1,NXLAY
            ILAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
            IF (ABS(ILAYCUT) == 1) ISHEON(I) = 1
          ENDDO
        ENDIF
      ENDDO
C
      IGON = 0
      DO I=JFT,JLT
        IF (ISHEON(I) > 0) THEN
          IGON = 1
        ENDIF
      ENDDO
C----------------------------------------
C     TEST FOR NEW GROUP ACTIVATION
C----------------------------------------
      IF (IGON == 1) IPARG(70,NG) = IGON
C-----------
      RETURN
      END
