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
Chd|  PRECRKXFEM                    source/elements/xfem/precrkxfem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ADDCNE_XFE                    source/elements/xfem/precrkxfem.F
Chd|====================================================================
      SUBROUTINE PRECRKXFEM(
     .           IPARG      ,IXC         ,IXTG          , NCRKXFE    ,      
     .           IEL_CRKXFEM,INOD_CRKXFEM,ADDCNE_CRKXFEM)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCRKXFE
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,*),
     .  IEL_CRKXFEM(*),INOD_CRKXFEM(*),ADDCNE_CRKXFEM(0:NCRKXFE+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITG,NG,NEL,LFT,LLT,NFT,ITY,IXFEM,NIX,KX
C=======================================================================
      ITG = NUMELC+1
C
      DO NG=1,NGROUP
        IXFEM=IPARG(54,NG)
        IF (IXFEM == 0) CYCLE
        NEL = IPARG(2,NG)
        NFT = IPARG(3,NG)
        ITY = IPARG(5,NG)
        LFT = 1
        LLT = MIN(NVSIZ,NEL)
        IF (ITY==3) THEN
          NIX = NIXC
          KX = 5
          CALL ADDCNE_XFE(IEL_CRKXFEM ,KX   ,NIX   ,IXC   ,ADDCNE_CRKXFEM,
     .                    INOD_CRKXFEM,LFT  ,LLT   ,NFT   ,NCRKXFE       )
        ELSE IF(ITY==7)THEN
          NIX = NIXTG
          KX = 4
          CALL ADDCNE_XFE(IEL_CRKXFEM(ITG),KX   ,NIX  ,IXTG  ,ADDCNE_CRKXFEM,
     .                    INOD_CRKXFEM    ,LFT  ,LLT  ,NFT   ,NCRKXFE       )
        END IF
      END DO        
c-----------          
C     decalage d'adresses
c-----------          
      ADDCNE_CRKXFEM(1) = 1
      DO I=2,NCRKXFE+1
        ADDCNE_CRKXFEM(I) = ADDCNE_CRKXFEM(I) + ADDCNE_CRKXFEM(I-1)
      END DO    
c-----------          
      RETURN
      END
c
Chd|====================================================================
Chd|  ADDCNE_XFE                    source/elements/xfem/precrkxfem.F
Chd|-- called by -----------
Chd|        PRECRKXFEM                    source/elements/xfem/precrkxfem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ADDCNE_XFE(IEL_CRKXFEM ,KX   ,NIX  ,IX    ,ADDCNE_CRKXFEM,
     .                      INOD_CRKXFEM,LFT  ,LLT  ,NFT   ,NCRKXFE       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KX,NIX,LFT,LLT,NFT,NCRKXFE
      INTEGER IEL_CRKXFEM(*),INOD_CRKXFEM(*),IX(NIX,*),
     .        ADDCNE_CRKXFEM(0:NCRKXFE+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N
C=======================================================================
      DO  K=2,KX
        DO I=LFT,LLT
          IF (IEL_CRKXFEM(I+NFT) > 0) THEN
            N = INOD_CRKXFEM(IX(K,I+NFT)) + 1     ! numerotation locale des noeuds phantomes
            ADDCNE_CRKXFEM(N) = ADDCNE_CRKXFEM(N) + 1
          ENDIF
        ENDDO
      ENDDO
C-----------          
      RETURN
      END
