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
Chd|  ALECONVE                      source/ale/porous/aleconv.F   
Chd|-- called by -----------
Chd|        ALEFLOW                       source/ale/porous/aleflow.F   
Chd|-- calls ---------------
Chd|        ALECONV3                      source/ale/porous/aleconv.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALECONVE(VAR     ,FLUX    ,FLU1    ,PHI     ,
     .                    ALE_CONNECT   ,PHIV    ,NGL     ,nvar )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is related to porous material law 77
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NGL(*),nvar
      my_real VAR(*),FLUX(MVSIZ,6),FLU1(*),PHI(*),PHIV(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IOFF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
         IOFF = NUMELS
         CALL ALECONV3(VAR         ,PHI    ,FLUX    ,FLU1    ,PHIV   ,
     .                 ALE_CONNECT ,IOFF   ,NGL     )
C-------------
      RETURN
      END SUBROUTINE ALECONVE
c
Chd|====================================================================
Chd|  ALECONV3                      source/ale/porous/aleconv.F   
Chd|-- called by -----------
Chd|        ALECONVE                      source/ale/porous/aleconv.F   
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALECONV3(VTOT        ,PHI    ,FLUX  ,FLU1  ,PHIV  ,
     .                    ALE_CONNECT ,IOFF   ,NGL   )
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOFF,NGL(*)
      my_real VTOT(*), PHI(*), FLUX(MVSIZ,6), FLU1(*),PHIV(*)
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
      INTEGER I, IE, IV,J, IAD2
      my_real
     .   VALVOIS(MVSIZ,6),VALEL(MVSIZ),VL(6)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        IE =NFT+I
        VALEL(I)=PHI(IE)
      ENDDO
C      
      DO I=LFT,LLT
         DO J=1,6
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          IF(IV > 0)THEN
            VALVOIS(I,J)=PHI(IV)
          ELSEIF(IV == 0)THEN
             VALVOIS(I,J)= PHIV(I)
!C  not incoming gas
cc            VALVOIS(I,J)= ZERO
c
cccc for ale continuit      VALVOIS(I,J)= PHI(IE)
          ELSE
            VALVOIS(I,J)=PHI(-IV+IOFF)
          ENDIF
        ENDDO
      ENDDO
C
      DO I=LFT,LLT
        VL(1) = VALVOIS(I,1)*FLUX(I,1)
        VL(2) = VALVOIS(I,2)*FLUX(I,2)
        VL(3) = VALVOIS(I,3)*FLUX(I,3)
        VL(4) = VALVOIS(I,4)*FLUX(I,4)
        VL(5) = VALVOIS(I,5)*FLUX(I,5)
        VL(6) = VALVOIS(I,6)*FLUX(I,6)
        VTOT(I)= VTOT(I) + HALF * DT1 *(
     .          -VALEL(I)*FLU1(I) - VL(1)-VL(2)-VL(3)-VL(4)-VL(5)-VL(6))
      ENDDO
C-----------
      RETURN
      END SUBROUTINE ALECONV3
