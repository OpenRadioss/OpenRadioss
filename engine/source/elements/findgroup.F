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
Chd|  FINDGROUPC                    source/elements/findgroup.F   
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  FINDGROUPC(
     1     IPARG, IGROUC, NGROUC, IGROUPC, IGROUPTG)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), NGROUC, IGROUC(*)
      INTEGER IGROUPC(NUMELC), IGROUPTG(NUMELTG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, NG, NEL, NFT, ITY
C-----------------------------------------------
C
       DO IG = 1, NGROUC
         NG  =IGROUC(IG)
         NEL =IPARG(2,NG)
         NFT =IPARG(3,NG)
         ITY =IPARG(5,NG)
         IF(ITY==3) THEN
           DO I=1,NEL
             IGROUPC(I+NFT)=NG
           ENDDO
         ELSEIF(ITY==7) THEN
           DO I=1,NEL
             IGROUPTG(I+NFT)=NG
           ENDDO
         ENDIF
       ENDDO
C
       RETURN
       END
Chd|====================================================================
Chd|  FINDGROUPS                    source/elements/findgroup.F   
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  FINDGROUPS(IPARG, IGROUPS)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*)
      INTEGER IGROUPS(NUMELS)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NG, NEL, NFT, ITY
C-----------------------------------------------
C
      DO NG = 1, NGROUP
         NEL =IPARG(2,NG)
         NFT =IPARG(3,NG)
         ITY =IPARG(5,NG)
         IF(ITY == 1) THEN
           DO I=1,NEL
             IGROUPS(I+NFT)=NG
           ENDDO
         ENDIF
      ENDDO
C
      RETURN
      END
