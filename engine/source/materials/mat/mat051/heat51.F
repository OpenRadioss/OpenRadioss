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
Chd|  M51TH                         source/materials/mat/mat051/heat51.F
Chd|-- called by -----------
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M51TH(T,AV1,AV2,AV3,UPARAM,XK)
C----------------------------------------
C     CALCUL DE LA CONDUCTIVITE THERMIQUE
C----------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   T(*), XK(*), AV1(*), AV2(*), AV3(*), UPARAM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   XK1,XK2,XK3,AL1,AL2,AL3,AL12,AL22,AL32,AA
C-----------------------------------------------
C
C     rigidit    (ou conductibilite thermique) equivalente 
C
C
C   hyp. de distribution de la phase 1 sur le volume:
C
C
C                  0-----+--------0  
C                 /     /|       /|  
C                /     / |      / |  
C               +-----+  |     /  |  
C              /|     |  |    /   |  
C             / |     |  |   /    |  
C            /  |     |  +--/-----+  
C           0--------------0     /|
C           |   |     |/   |    / |
C           |   +-----+----|---+  |
C           |  /     /|    |   |  0
C           | /     / |    |   | /
C           |/     /  |    |   |/
C        ^  +-----+   +----|---+   
C        |  |     |  /     |  /    
C    al1 |  |     | /      | /     
C        |  |     |/       |/      
C        v  0-----+--------0      
C
C           <----->
C             al1
C
C
C-----------------------------------------------
C  pour 2 phases:
C
C                             (1-al1^2-al2^2)
C   K = K1 al1^2 + K2 al2^2 + ---------------
C                             al1/K1 + al2/K2 
C
C si K1=K2=KO K=K0  (al1+al2 = 1)
C-----------------------------------------------
C  generalisation pour 3 phases:
C
C
C                                            (1-al1^2-al2^2-al3^2)
C1)   K = K1 al1^2 + K2 al2^2 + K3 al3^2 + ------------------------
C                                          al1/K1 + al2/K2 + al3/K3
C
C => si K1=K2=K3=K0 => K/=KO   (al1+al2+al3 /= 1)!!! pb
C
C2) => normalisation
C
C                                     (al1+al2+al3)(1-al1^2-al2^2-al3^2)
C K = K1 al1^2 + K2 al2^2 + K3 al3^2 + --------------------------------
C                                         al1/K1 + al2/K2 + al3/K3
C
C => si K1=K2=K3=K0 => K=KO   
C-----------------------------------------------
C   calcul de al a partir de av (pour chaque phase):
C
C   av = 3 al^2 - 2 al^3
C
C-----------------------------------------------
C   solution approchee:
C
C   si av -> 0  al -> sqrt(av/3)
C   si av -> 1  al -> 1 - sqrt((1-av)/3)
C
C   al -> av - av sqrt((1-av)/3) + (1-av) sqrt(av/3)
C
C   av      al(approche)   av(correspondant) erreur
C   0       0              0                 0%
C   0.25    0.34           0.27              2%
C   0.5     0.5            0.5               0%
C   0.75    0.66           0.73              2%
C   1       1              1                 0%
C-----------------------------------------------
C----------------------------
C     CONDUCTION THERMIQUE
C----------------------------

      DO I=LFT,LLT
C            
csm        AV3(I) = ONE - AV1(I) - AV2(I)
        XK1 = UPARAM(114)+UPARAM(115)*T(I)
        XK2 = UPARAM(164)+UPARAM(165)*T(I)
        XK3 = UPARAM(214)+UPARAM(215)*T(I)

        
        AL1 = AV1(I) 
     .      - AV1(I)     *SQRT((ONE-AV1(I))*THIRD) 
     .      + (ONE-AV1(I))*SQRT(AV1(I)*THIRD)

        AL2 = AV2(I) 
     .      - AV2(I)     *SQRT((ONE-AV2(I))*THIRD) 
     .      + (ONE-AV2(I))*SQRT(AV2(I)*THIRD)

        AL3 = AV3(I) 
     .      - AV3(I)     *SQRT((ONE-AV3(I))*THIRD) 
     .      + (ONE-AV3(I))*SQRT(AV3(I)*THIRD)

        AL12 = AL1*AL1
        AL22 = AL2*AL2
        AL32 = AL3*AL3
        AA = (AL1+AL2+AL3)*(ONE-AL12-AL22-AL32)

        XK(I)= AL12 * XK1 + AL22 * XK2 + AL32 * XK3 
     .         + AA / (AL1/XK1+AL2/XK2+AL3/XK3)
          
      ENDDO
       
C
      RETURN
      END
      
      
      
