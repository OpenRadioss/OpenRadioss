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
Chd|  VOLN22                        source/interfaces/int22/voln22.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE VOLN22(VOLN,IAD22,IXS)
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "fluid_scr_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(INOUT) :: VOLN(*)
      my_real,INTENT(IN)    :: IAD22(*)
      INTEGER,INTENT(IN)    :: IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IB, NCELL, IPOS, MCELL, NIN , NSecnds, IBV, ICELLv, NumSecnd,ISECND
      my_real :: VAR, AREAM, VOLM, VOLsecnds
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
           MCELL     =  BRICK_LIST(NIN,IB)%MainID
           VOLN(I)   =  EM20*BRICK_LIST(NIN,IB)%UncutVOL  !!!ZERO !in case of no main cell                               
           IF(MCELL == 0)CYCLE          
          
           IF(BRICK_LIST(NIN,IB)%POLY(MCELL)%Vnew>ZERO)THEN
             VOLN(I)    =  BRICK_LIST(NIN,IB)%Vnew_SCell
           ENDIF
                     
        ENDDO
      ENDIF

C-----------------------------------------------
      RETURN
      END SUBROUTINE VOLN22
