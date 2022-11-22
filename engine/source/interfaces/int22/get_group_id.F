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
Chd|  GET_GROUP_ID                  source/interfaces/int22/get_group_id.F
Chd|-- called by -----------
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_GROUP_ID(
     1             II      , NG        , IG        , IPARG     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)    :: II, IPARG(NPARG,*)
      INTEGER, INTENT(INOUT) :: NG, IG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NEL, NFL, I, NG_
      LOGICAL DONE
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C   Defines following surjective application  
C            [1:SPHI]    -->   [1:NGROUP] x [1:MVSIZ] 
C               II      |-->        (NG,IG)
C          (intent IN)           (intent OUT)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        DONE=.FALSE.
        DO NG_=1,NGROUP
          NEL=IPARG(2,NG_)
          NFL=IPARG(3,NG_)
          IF((II>NFL).AND.(II<=NFL+NEL))THEN
            IG    = II - NFL
            NG    = NG_
            DONE  =.TRUE.        
            EXIT  !exit loop
          ELSE
            CYCLE !next NG  
          ENDIF
          if (.NOT.(DONE).OR.II==0)then
            print *, "int 22 : error in group sorting."
            stop
          end if
        ENDDO !next NG
C-----------------------------------------------
      RETURN
      END SUBROUTINE GET_GROUP_ID
