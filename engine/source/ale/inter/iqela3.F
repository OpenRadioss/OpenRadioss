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
Chd|  IQELA3                        source/ale/iqela3.F           
Chd|-- called by -----------
Chd|        INT12W                        source/ale/int12w.F           
Chd|        INTAL4                        source/ale/intal4.F           
Chd|-- calls ---------------
Chd|        SHAPEH                        source/ale/shapeh.F           
Chd|====================================================================
      SUBROUTINE IQELA3(
     1                  V    ,W    ,IRECT ,CRST ,NSV ,
     2                  ILOC ,IRTL ,ICODE ,IBCS ,BCS ,
     3                  MSR 
     .                  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr04_c.inc"
#include      "scr08_s_c.inc"
#include      "scr08_a_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: IRECT(4,*), NSV(*), ILOC(*), IRTL(*), ICODE(*), IBCS(*), MSR(*)
      my_real,INTENT(IN)    :: V(3,NUMNOD), CRST(2,*), BCS(9,*)
      my_real,INTENT(INOUT) :: W(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NIR, II, N, L, JJ, NN
      my_real :: H(4), SS, TT, VMX, VMY, VMZ
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NIR=2
      IF(N2D == 0)NIR=4      
      DO  II=1,NSN
        N        = NSV(II)
        L        = IRTL(II)
        DO JJ=1,NIR
          NN     = IRECT(JJ,L)
          IX(JJ) = MSR(NN)
        ENDDO
        SS       = CRST(1,II)
        TT       = CRST(2,II)
        CALL SHAPEH(H,SS,TT)
        VMX      = ZERO
        VMY      = ZERO
        VMZ      = ZERO     
        DO JJ=1,NIR
        VMX      = VMX+V(1,IX(JJ))*H(JJ)
        VMY      = VMY+V(2,IX(JJ))*H(JJ)
        VMZ      = VMZ+V(3,IX(JJ))*H(JJ)
        ENDDO
        !GRID VELOCITY
        W(1,N)   = VMX
        W(2,N)   = VMY
        W(3,N)   = VMZ
      ENDDO ! NEXT II
C-----------------------------------------------
      RETURN
      END
