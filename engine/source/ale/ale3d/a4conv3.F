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
Chd|  A4CONV3                       source/ale/ale3d/a4conv3.F    
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE A4CONV3(VTOT ,PHI,FLUX,FLU1,
     .                   ALE_CONNECT,IOFF)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOFF
      my_real VTOT(*), PHI(*), FLUX(6,*), FLU1(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV,J,IAD2
      my_real VALVOIS(6,MVSIZ),VALEL(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        IE =NFT+I
        VALEL(I)=PHI(IE)
      ENDDO

      DO I=LFT,LLT
         IE =NFT+I
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
!
         J = 2
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
         IF(IV > 0)THEN
            VALVOIS(J,I)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(J,I)=PHI(IE)
         ELSE
            VALVOIS(J,I)=PHI(-IV+IOFF)
         ENDIF
!
         J = 4
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
         IF(IV > 0)THEN
            VALVOIS(J,I)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(J,I)=PHI(IE)
         ELSE
            VALVOIS(J,I)=PHI(-IV+IOFF)
         ENDIF
!
         J = 5
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
         IF(IV > 0)THEN
            VALVOIS(J,I)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(J,I)=PHI(IE)
         ELSE
            VALVOIS(J,I)=PHI(-IV+IOFF)
         ENDIF
!
         J = 6
         IV = ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
         IF(IV > 0)THEN
            VALVOIS(J,I)=PHI(IV)
         ELSEIF(IV == 0)THEN
            VALVOIS(J,I)=PHI(IE)
         ELSE
            VALVOIS(J,I)=PHI(-IV+IOFF)
         ENDIF
      ENDDO
C
      DO I=LFT,LLT
        VTOT(I) = VTOT(I) + HALF * DT1 *(
     1                                   -VALEL(I)*FLU1(I)
     2                                   -VALVOIS(2,I)*FLUX(2,I)
     3                                   -VALVOIS(4,I)*FLUX(4,I)
     4                                   -VALVOIS(5,I)*FLUX(5,I)
     5                                   -VALVOIS(6,I)*FLUX(6,I)
     6                                   )
      ENDDO
C-----------------------------------------------
      RETURN
      END
C
