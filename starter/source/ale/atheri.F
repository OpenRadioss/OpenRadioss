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
Chd|  ATHERI                        source/ale/atheri.F           
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ATHERI(MAT,PM,TEMP)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is initializing elem temperatures
C initial temperature is stored in material buffer PM(79,[mat_internal_id])
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: MAT(MVSIZ)
      my_real, INTENT(IN) :: PM(NPROPM,NUMMAT)
      my_real, INTENT(INOUT) :: TEMP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        TEMP(I) = PM(79,MAT(I))
      ENDDO
C-----------------------------------------------
      RETURN
      END
