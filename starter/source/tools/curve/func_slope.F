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
Chd|  FUNC_SLOPE                    source/tools/curve/func_slope.F
Chd|-- called by -----------
Chd|        LAW111_UPD                    source/materials/mat/mat111/law111_upd.F
Chd|        LAW158_UPD                    source/materials/mat/mat158/law158_upd.F
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        LAW77_UPD                     source/materials/mat/mat077/law77_upd.F
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|        LAW90_UPD                     source/materials/mat/mat090/law90_upd.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FUNC_SLOPE(IDN,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDN,NPC(*) 
      my_real PLD(*),FAC,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG
C-----------------------------------------------
      INTENT(IN)    :: NPC,PLD,IDN
      INTENT(OUT)   :: STIFFMAX,STIFFINI,STIFFAVG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,NP1,NP2,K1,PN1,PN2,COUNT
      my_real DYDX,DX,DY
C=======================================================================
          ! COMPUTE MAXIMUM SLOPE AND INITIAL SLOPE OF FUNCTION
C=======================================================================
      PN1 = NPC(IDN)
      PN2 = NPC(IDN+1)
      STIFFINI = ZERO
      STIFFAVG = ZERO
      STIFFMAX = ZERO
      STIFFMIN = EP20
      COUNT = 0
      DO J = PN1,PN2-4,2
        COUNT = COUNT + 1
        DX = PLD(J+2) - PLD(J)
        DY = PLD(J+3) - PLD(J+1)               
        DYDX = FAC*DY/DX
        STIFFMAX = MAX(STIFFMAX,DYDX)
        STIFFMIN = MIN(STIFFMIN,DYDX)
        STIFFAVG = STIFFAVG + DYDX
        IF(PLD(J+2)== ZERO )THEN
           DX = PLD(J+2) - PLD(J)
           DY = PLD(J+3) - PLD(J+1)            
           STIFFINI = MAX(STIFFINI, FAC*DY/DX)
        ELSEIF(PLD(J) == ZERO) THEN
           DX = PLD(J+2) - PLD(J)
           DY = PLD(J+3) - PLD(J+1)             
           STIFFINI = MAX(STIFFINI, FAC*DY/DX)
        ELSEIF(PLD(PN1) >= ZERO) THEN
           DX = PLD(PN1+2) - PLD(PN1 )
           DY = PLD(PN1+3) - PLD(PN1 + 1)             
           STIFFINI = MAX(STIFFINI, FAC*DY/DX)
        ENDIF
      ENDDO
      STIFFAVG = STIFFAVG / COUNT
c-----------
      RETURN
      END
C=======================================================================
Chd|====================================================================
Chd|  UNIFY_X                       source/tools/curve/func_slope.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE UNIFY_X(IDN1,IDN2,NPC,PLD,NPOINT,LEN1,LEN2,XUNI,NPTNEW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER     IDN1,IDN2,IDN,NPOINT,LEN1,LEN2,
     .        NPTNEW,NPC(*) 
      my_real 
     .        PLD(*),XUNI(NPOINT) 
C-----------------------------------------------
      INTENT(IN)    :: NPC,PLD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,NP1,NP2,K1,PN1,PN2
      my_real 
     .      EC , ET  
c=======================================================================
       !IDN1 = IFUNC(1) ! uni C
       !IDN2 = IFUNC(2) ! uni T
       I = 0
       J = 0
       EC = PLD(NPC(IDN1) )
       ET = PLD(NPC(IDN2) )
       DO K = 1,NPOINT
        IF(I == 2*LEN1 .AND. J == 2*LEN2 )THEN 
         EXIT
        ELSE
         IF     ((EC < ET.AND.I<2*LEN1) .OR. J >= 2*LEN2)THEN
           XUNI(K) = EC 
           I = I + 2 
           EC = PLD(NPC(IDN1)+ I )
         ELSEIF ((EC > ET.AND.J<2*LEN2)   .OR. I >= 2*LEN1)THEN  
           XUNI(K) = ET 
           J = J + 2
           ET = PLD(NPC(IDN2)+ J )
         ELSEIF (EC == ET)THEN 
           XUNI(K) = ET 
           I = I + 2
           J = J + 2
           EC = PLD(NPC(IDN1)+ I )
           ET = PLD(NPC(IDN2)+ J )
         ENDIF
        ENDIF
       ENDDO
       NPTNEW = K
c-----------
      RETURN
      END

