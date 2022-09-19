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
Chd|  XFECUT                        source/output/anim/generate/xfecut.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        XFECONNEC3N                   source/output/anim/generate/xfeconnec3n.F
Chd|        XFECONNEC4N                   source/output/anim/generate/xfeconnec4n.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE XFECUT(IPARG      ,IXC     ,IXTG     ,ILEV   ,ELCUTC  ,
     .                  IEL_CRK    ,IADC_CRK,NODEDGE  ,CRKEDGE,XEDGE4N ,
     .                  XEDGE3N    )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILEV,IPARG(NPARG,*),IXC(NIXC,*),ELCUTC(2,*),IEL_CRK(*),
     .  IADC_CRK(*),IXTG(NIXTG,*),NODEDGE(2,*),XEDGE4N(4,*),XEDGE3N(3,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,NEL,JFT,JLT,NFT,IXFEM,ITY,ITG1,ITG2,ACTIFXFEM
C=======================================================================
      IF (NLEVSET > 0) THEN
C---
        ITG1 = 1+NUMELC
        ITG2 = 1+4*ECRKXFEC
C
        DO NG=1,NGROUP
          NEL       = IPARG(2,NG)
          NFT       = IPARG(3,NG)
          ITY       = IPARG(5,NG)
          IXFEM     = IPARG(54,NG)
          ACTIFXFEM = IPARG(70,NG)
          IF (ITY/=3 .and. ITY/=7) CYCLE
          IF (IXFEM == 0)          CYCLE
          IF (ACTIFXFEM == 0)      CYCLE
          JFT = 1
          JLT = NEL
C
          IF (ITY == 3) THEN
            CALL XFECONNEC4N(
     .        JFT     ,JLT     ,NFT     ,IXC     ,ELCUTC  ,
     .        IEL_CRK ,IADC_CRK,ILEV    ,NODEDGE ,CRKEDGE ,
     .        XEDGE4N )
          ELSE IF (ITY == 7) THEN
            CALL XFECONNEC3N(
     .        JFT          ,JLT           ,NFT  ,IXTG  ,ELCUTC(1,ITG1),
     .        IEL_CRK(ITG1),IADC_CRK(ITG2),ILEV ,NODEDGE,CRKEDGE,
     .        XEDGE3N      )
          END IF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
