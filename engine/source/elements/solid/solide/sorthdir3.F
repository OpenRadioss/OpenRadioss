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
Chd|  SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRCOORK                       source/elements/solid/solide8z/srcoork.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTHDIR3(
     .       RX    ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X   ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  , 
     .       GAMA0,GAMA )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(LLT), INTENT(IN)    ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
      my_real, 
     .  DIMENSION(LLT,6), INTENT(IN)  :: GAMA0
      my_real, 
     .  DIMENSION(MVSIZ,6), INTENT(OUT) :: GAMA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .  UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,D1,D2,D3,GX,GY,GZ,SUMA,S2,S3
C=======================================================================
      IF (IREP == 0) THEN 
        DO I=LFT,LLT                                            
          GAMA(I,1) = GAMA0(I,1)				  
          GAMA(I,2) = GAMA0(I,2)				  
          GAMA(I,3) = GAMA0(I,3) 
          GAMA(I,4) = GAMA0(I,4)				  
          GAMA(I,5) = GAMA0(I,5)				  
          GAMA(I,6) = GAMA0(I,6)
        ENDDO                                   
      ELSEIF (IREP > 0) THEN 
ctmp      ELSEIF (IREP == 1) THEN 
C         dir 1 = const
        DO I=LFT,LLT                                            
C         Dir 1
          
          D1 = GAMA0(I,1)*RX(I) + GAMA0(I,2)*SX(I) + GAMA0(I,3)*TX(I)     
          D2 = GAMA0(I,1)*RY(I) + GAMA0(I,2)*SY(I) + GAMA0(I,3)*TY(I)     
          D3 = GAMA0(I,1)*RZ(I) + GAMA0(I,2)*SZ(I) + GAMA0(I,3)*TZ(I) !DIRECTION1 DS GLOBAL 
          ! ISO -> ELEM
          UX = D1*E1X(I)+ D2*E1Y(I) + D3*E1Z(I)                 
          UY = D1*E2X(I)+ D2*E2Y(I) + D3*E2Z(I)                 
          UZ = D1*E3X(I)+ D2*E3Y(I) + D3*E3Z(I) ! COORD DU ORTHO DS ELEME             
          SUMA = ONE/SQRT(UX*UX + UY*UY + UZ*UZ) 
          GAMA(I,1) = UX*SUMA                                   
          GAMA(I,2) = UY*SUMA                                   
          GAMA(I,3) = UZ*SUMA                                   
C         Dir 2
          D1 = GAMA0(I,4)*RX(I) + GAMA0(I,5)*SX(I) + GAMA0(I,6)*TX(I)     
          D2 = GAMA0(I,4)*RY(I) + GAMA0(I,5)*SY(I) + GAMA0(I,6)*TY(I)     
          D3 = GAMA0(I,4)*RZ(I) + GAMA0(I,5)*SZ(I) + GAMA0(I,6)*TZ(I)    
          VX = D1*E1X(I)+ D2*E1Y(I) + D3*E1Z(I)                 
          VY = D1*E2X(I)+ D2*E2Y(I) + D3*E2Z(I)                 
          VZ = D1*E3X(I)+ D2*E3Y(I) + D3*E3Z(I)                 
          SUMA = ONE/SQRT(VX*VX + VY*VY + VZ*VZ)
          VX = VX*SUMA                 
          VY = VY*SUMA                 
          VZ = VZ*SUMA                 
C         Orthogonalisation: 
C         Dir1' = Dir1,  Dir3 = Dir1 x Dir2, Dir2' = Dir3 x Dir1
C ON VEUT LA 3EME DIRECTION DE GAMA (ELEM -> ORTHO)
          D1 = GAMA(I,2) * VZ - GAMA(I,3) * VY
          D2 = GAMA(I,3) * VX - GAMA(I,1) * VZ
          D3 = GAMA(I,1) * VY - GAMA(I,2) * VX
          GAMA(I,4) = D2 * GAMA(I,3) - D3 * GAMA(I,2)
          GAMA(I,5) = D3 * GAMA(I,1) - D1 * GAMA(I,3)
          GAMA(I,6) = D1 * GAMA(I,2) - D2 * GAMA(I,1)
    
        ENDDO                                     
c      ELSEIF (IREP == 2) THEN 
C       Plan (dir1,dir2) = const
c        DO I=LFT,LLT                                            
C         Dir 1  - normale au plan
c          D1 = GAMA0(I,1)*RX(I) + GAMA0(I,2)*SX(I) + GAMA0(I,3)*TX(I)     
c          D2 = GAMA0(I,1)*RY(I) + GAMA0(I,2)*SY(I) + GAMA0(I,3)*TY(I)     
c          D3 = GAMA0(I,1)*RZ(I) + GAMA0(I,2)*SZ(I) + GAMA0(I,3)*TZ(I)    
c          UX = D1*E1X(I)+ D2*E1Y(I) + D3*E1Z(I)                 
c          UY = D1*E2X(I)+ D2*E2Y(I) + D3*E2Z(I)                 
c          UZ = D1*E3X(I)+ D2*E3Y(I) + D3*E3Z(I)                 
c          SUM= ONE/SQRT(UX*UX + UY*UY + UZ*UZ)
c          UX = UX*S2                 
c          UY = UY*S2               
c          UZ = UZ*S2    
C         Dir 2
c          D1 = GAMA0(I,4)*RX(I) + GAMA0(I,5)*SX(I) + GAMA0(I,6)*TX(I)     
c          D2 = GAMA0(I,4)*RY(I) + GAMA0(I,5)*SY(I) + GAMA0(I,6)*TY(I)     
c          D3 = GAMA0(I,4)*RZ(I) + GAMA0(I,5)*SZ(I) + GAMA0(I,6)*TZ(I)    
c          VX = D1*E1X(I)+ D2*E1Y(I) + D3*E1Z(I)                 
c          VY = D1*E2X(I)+ D2*E2Y(I) + D3*E2Z(I)                 
c          VZ = D1*E3X(I)+ D2*E3Y(I) + D3*E3Z(I)                 
c          S2 = ONE/SQRT(VX*VX + VY*VY + VZ*VZ)
c          VX = VX*S2                 
c          VY = VY*S2               
c          VZ = VZ*S2                
C         Dir 3
c          UX = VY*WZ - VZ*WY
c          UY = VZ*WX - VX*WZ
c          UZ = VX*WY - VY*WX
c
c          D1 = GAMA0(I,7)*RX(I) + GAMA0(I,8)*SX(I) + GAMA0(I,9)*TX(I)     
c          D2 = GAMA0(I,7)*RY(I) + GAMA0(I,8)*SY(I) + GAMA0(I,9)*TY(I)     
c          D3 = GAMA0(I,7)*RZ(I) + GAMA0(I,8)*SZ(I) + GAMA0(I,9)*TZ(I)    
c          WX = D1*E1X(I)+ D2*E1Y(I) + D3*E1Z(I)                 
c          WY = D1*E2X(I)+ D2*E2Y(I) + D3*E2Z(I)                 
c          WZ = D1*E3X(I)+ D2*E3Y(I) + D3*E3Z(I)                 
c          S3 = ONE/SQRT(WX*WX + WY*WY + WZ*WZ)
c          WX = WX*S3                 
c          WY = WY*S3                
c          WZ = WZ*S3
C         Dir 1 = Dir2 x Dir3
c          UX = VY*WZ - VZ*WY
c          UY = VZ*WX - VX*WZ
c          UZ = VX*WY - VY*WX
C         Orthogonalisation de la base dir2/dir3 : 
C         Dir2'/Dir3' = dir2/dir3 orthogonalize symmetriquement, Dir1=Dir3xDir2
c          SUMA   = SQRT(S2/S3)                
c          D1 = VX + (WY*UZ-WZ*UY)*SUMA
c          D2 = VY + (WZ*UX-WX*UZ)*SUMA
c          D3 = VZ + (WX*UY-WY*UX)*SUMA
c          SUMA = ONE/SQRT(D1*D1 + D2*D2 + D3*D3)                 
c          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                  
c          GAMA(1,I) = UX                                            
c          GAMA(2,I) = UY                                            
c          GAMA(3,I) = UZ                                            
c          GAMA(4,I) = D1 * SUMA
c          GAMA(5,I) = D2 * SUMA
c          GAMA(6,I) = D3 * SUMA
c        ENDDO                                     
      ENDIF                                      
C-------------
      RETURN
      END SUBROUTINE SORTHDIR3
