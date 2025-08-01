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
!||    deltax22         ../engine/source/interfaces/int22/deltax22.F
!||--- called by ------------------------------------------------------
!||    sforc3           ../engine/source/elements/solid/solide/sforc3.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    i22bufbric_mod   ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22tri_mod       ../common_source/modules/interfaces/cut-cell-search_mod.F
!||====================================================================
      SUBROUTINE DELTAX22(
     1   VOL,     VEUL,    VOLN,    DELTAX,
     2   IAD22,   IXS,     LFT,     LLT,
     3   JALE,    JEUL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C this subroutines update VOLN for cut cells.
C Main cells have extended values of volume due
C to secnd cell merging. This extension data is 
C stored in cut cell buffer and is now used here
C to update VOLN(*)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD 
      USE I22TRI_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: JALE
      INTEGER, INTENT(INOUT) :: JEUL
      my_real :: VOL(*), VEUL(LVEUL,*), VOLN(*),DELTAX(*)
      my_real :: IAD22(*)
      INTEGER :: IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IB, NCELL, IPOS, MCELL, NIN , NSecnds, IBV, ICELLv, NumSecnd,ISECND,NBCUT,K
      my_real :: VAR, AREAM, VOLM, VOLsecnds, FACE0, FACE0M, VOLMAST
      TYPE(POLY_ENTITY),DIMENSION(:),    POINTER    :: pIsMain 
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
      IF(INT22==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NIN = 1
      
      IF(JEUL+JALE/=0)THEN    
        DO I=LFT,LLT
        
           IB        =  NINT(IAD22(I))
           IF(IB==0)CYCLE
           NCELL     =  BRICK_LIST(NIN,IB)%NBCUT      
           pIsMain(1:9) => BRICK_LIST(NIN,IB)%POLY(1:9)!%IsMain
           MCELL     =  BRICK_LIST(NIN,IB)%MainID
           IF(MCELL == 0)THEN
             DELTAX(I) = EP30   !don't take into account this time step, nothing is stored here since MCELL=0.
             CYCLE
           ENDIF
          
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           !           CHARACTERISTIC LENGTH           !
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           NSecnds     = BRICK_LIST(NIN,IB)%SecndList%Num
           VOLsecnds   = SUM(BRICK_LIST(NIN,IB)%SecndList%VOL(1:NSecnds))
           VOLM        = BRICK_LIST(NIN,IB)%Vnew_SCell
           MCELL       = BRICK_LIST(NIN,IB)%mainID
           IF(MCELL==0)MCELL=1
           VOLMAST     = BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew
           NBCUT       = BRICK_LIST(NIN,IB)%NBCUT
           AREAM       = MAXVAL(BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(1:6)%Surf)
           FACE0M      = ZERO
           DO K=1,NBCUT
             FACE0     = BRICK_LIST(NIN,IB)%PCUT(K)%Scut(1)
             FACE0M    = MAX(FACE0M,FACE0)
           ENDDO
           AREAM       = MAX(AREAM,FACE0M)
           
           !---meth1:supercell dx with 50% of brick dx (smooth time step)
           !DELTAX(I)   = HALF * DELTAX(I)
           
           !---meth2:supercell dx estimated with main cell dx (minorant)
           !DELTAX(I)   = VOLM/AREAM  
            
           !---meth3:isomorph supercell to equivalent brick(cube)
           !DELTAX(I)   = (VOLN(I))**THIRD
           
           !---meth4:isomorph main cell only to equivalent brick(cube)
           !DELTAX(I)   = VOLM**THIRD  
                    
           !---meth5: no specific treatment (FAST but only valid if time step scale factor <= 0.5 in engine file)
           !DELTAX(I)    = DELTAX(I)
           
           !---meth6: use scale factor on solid element to compute target 0.5
           !DELTAX(I)    = DTFAC22*DELTAX(I)

           !---meth7: Vol(main)/area_max
           DELTAX(I)   = VOLMAST/AREAM  
           
        ENDDO
      ENDIF

C-----------------------------------------------
      RETURN
      END SUBROUTINE DELTAX22
