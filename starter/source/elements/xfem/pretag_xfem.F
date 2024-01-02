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
Chd|  PRETAG_XFEM                   source/elements/xfem/pretag_xfem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        TAG_SH                        source/elements/xfem/pretag_xfem.F
Chd|====================================================================
      SUBROUTINE PRETAG_XFEM(IPARG  ,ITAGE   ,IEL_CRKXFEM,ITAGN  ,INOD_CRKXFEM)
C-----------------------------------------------
c     numerotation locale des noeuds et elems fantomes des parts xfem
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP)
      INTEGER , DIMENSION(NUMNOD) :: ITAGN, INOD_CRKXFEM
      INTEGER , DIMENSION(NUMELC+NUMELTG) :: ITAGE, IEL_CRKXFEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NG,NEL,IXFEM,NFT,ITY,LFT,LLT,ITG,IGTYP,ICRK_ALL(2)
C=======================================================================
      ITG = 1 + NUMELC
C
      NCRKXFE = 0   ! local par proc 
      DO I=1,NUMNOD
        IF(ITAGN(I) > 0)THEN
          NCRKXFE = NCRKXFE + 1
          INOD_CRKXFEM(I) = NCRKXFE
        ENDIF
      ENDDO
C
      ICRK_ALL(1:2) = 0
      ECRKXFE = 0             ! local par proc, toutes les coques
c
      DO NG=1,NGROUP
        IXFEM=IPARG(54,NG)
        IF (IXFEM == 0) CYCLE
c
        NEL  =IPARG(2,NG)
        NFT  =IPARG(3,NG)
        ITY  =IPARG(5,NG)
        IGTYP=IPARG(38,NG)
        LFT  =1
        LLT  =MIN(NVSIZ,NEL)
        NXLAYMAX = MAX(NXLAYMAX, IPARG(59,NG)) ! Max Xfem layers, for alloc
c
c---    Fill xfem elements table : IEL_CRKXFEM(NUMELC+NUMELTG) = sys numbers
c
        IF (ITY==3) THEN
          CALL TAG_SH(ITAGE     ,IEL_CRKXFEM     ,ECRKXFE,
     .                LFT       ,LLT             ,NFT     )
        ELSE IF (ITY==7) THEN
          CALL TAG_SH(ITAGE(ITG),IEL_CRKXFEM(ITG),ECRKXFE,
     .                LFT       ,LLT             ,NFT      )
        END IF
C
        IF (IGTYP == 11 .AND. IXFEM == 1) THEN
          ICRK_ALL(1) = ICRK_ALL(1) + 1
        ELSEIF (IGTYP == 1 .AND. IXFEM == 2) THEN
          ICRK_ALL(2) = ICRK_ALL(2) + 1
        END IF
      ENDDO
C
      IF (ICRK_ALL(2) > 0) THEN
        ICRACK3D = ICRACK3D + 1
        IF (ICRK_ALL(1) > 0) ICRACK3D = ICRACK3D + 1
      ENDIF
C---  Count Xfem elements
      ECRKXFEC = 0            ! local par proc, coques 4N  
      ECRKXFETG= 0            ! local par proc, coques 3N  
      DO I=1,NUMELC
        IF (IEL_CRKXFEM(I) > 0)        ECRKXFEC  = ECRKXFEC + 1
      END DO
      DO I=1,NUMELTG
        IF (IEL_CRKXFEM(I+NUMELC) > 0) ECRKXFETG = ECRKXFETG + 1
      END DO
C-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  TAG_SH                        source/elements/xfem/pretag_xfem.F
Chd|-- called by -----------
Chd|        PRETAG_XFEM                   source/elements/xfem/pretag_xfem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TAG_SH(ITAGE,IEL_CRKXFEM,ECRKXFE,LFT,LLT,NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAGE(*),IEL_CRKXFEM(*),ECRKXFE,LFT,LLT,NFT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
      DO I=LFT,LLT
        IF (ITAGE(I+NFT) > 0) THEN
          ECRKXFE = ECRKXFE + 1
          IEL_CRKXFEM(I+NFT) = ECRKXFE
        ENDIF
      ENDDO
C-----------
      RETURN
      END
