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
Chd|  DELTAX22                      source/interfaces/int22/deltax22.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE DELTAX22(VOL,VEUL,VOLN,DELTAX,IAD22,
     .                  IXS)
C-----------------------------------------------
C   D e s c r i p  t i o n
C-----------------------------------------------
C this subroutines update VOLN for cut cells.
C Master cells have extended values of volume due
C to slave cell merging. This extension data is 
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "fluid_scr_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real :: VOL(*), VEUL(LVEUL,*), VOLN(*),DELTAX(*)
      my_real :: IAD22(*)
      INTEGER :: IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IB, NCELL, IPOS, MCELL, NIN , NSlaves, IBV, ICELLv, NumSlave,ISLAVE,NBCUT,K
      my_real :: VAR, AREAM, VOLM, VOLslaves, FACE0, FACE0M, VOLMAST
      TYPE(POLY_ENTITY),DIMENSION(:),    POINTER    :: pIsMaster 
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
           pIsMaster(1:9) => BRICK_LIST(NIN,IB)%POLY(1:9)!%IsMaster
           MCELL     =  BRICK_LIST(NIN,IB)%MasterID
           IF(MCELL == 0)THEN
             DELTAX(I) = EP30   !don't take into account this time step, nothing is stored here since MCELL=0.
             CYCLE
           ENDIF
          
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           !           CHARACTERISTIC LENGTH           !
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           NSlaves     = BRICK_LIST(NIN,IB)%SlaveList%Num
           VOLslaves   = SUM(BRICK_LIST(NIN,IB)%SlaveList%VOL(1:NSlaves))
           VOLM        = BRICK_LIST(NIN,IB)%Vnew_SCell
           MCELL       = BRICK_LIST(NIN,IB)%masterID
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
           !DELTAX(I)   = UNDEMI * DELTAX(I)
           
           !---meth2:supercell dx estimated with master cell dx (minorant)
           !DELTAX(I)   = VOLM/AREAM  
            
           !---meth3:isomorph supercell to equivlent brick(cube) 
           !DELTAX(I)   = (VOLN(I))**TIERS
           
           !---meth4:isomorph master cell only to equivlent brick(cube) 
           !DELTAX(I)   = VOLM**TIERS  
                    
           !---meth5: no specific treatment (FAST but only valid if time step scale factor <= 0.5 in engine file)
           !DELTAX(I)    = DELTAX(I)
           
           !---meth6: use scale factor on solid element to compute target 0.5
           !DELTAX(I)    = DTFAC22*DELTAX(I)

           !---meth7: Vol(master)/area_max
           DELTAX(I)   = VOLMAST/AREAM  
           
        ENDDO
      ENDIF

C-----------------------------------------------
      RETURN
      END SUBROUTINE DELTAX22
