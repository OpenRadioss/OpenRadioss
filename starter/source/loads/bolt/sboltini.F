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
Chd|  SBOLTINI                      source/loads/bolt/sboltini.F  
Chd|-- called by -----------
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SBOLTINI(E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .                    BPRELD     ,NEL  ,IX   ,NIX  ,VPRELOAD, IFLAG_BPRELOAD)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "boltpr_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   NIX , NEL
      INTEGER   IX(NIX,*), IFLAG_BPRELOAD(*)
      my_real
     .   VPRELOAD(7,*),BPRELD(NEL,NBPRELD),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAGINI,J,II,JJ
      my_real
     .  NX,NY,NZ,TSTART,TEND,LOAD,SENS,
     .  L11,L22,L33,L12,L23,L13,T11,T22,T33,T12,T23,T13,
     .  S11,S12,S13,S21,S22,S23,S31,S32,S33,
     .  EE1X,EE1Y,EE1Z,EE2X,EE2Y,EE2Z,EE3X,EE3Y,EE3Z
C=======================================================================
      IF (JCVT /= 0)THEN
       DO I=LFT,LLT
         IFLAGINI = 0
         II=NFT+I
         JJ=IFLAG_BPRELOAD(II)
         IF (JJ == 0)GOTO 200
         IFLAGINI = 1
C-----------
         IF (IFLAGINI == 1)THEN
           TSTART = VPRELOAD(1,II)
           TEND   = VPRELOAD(2,II)
           LOAD   = VPRELOAD(3,II)
           NX = VPRELOAD(4,II)
           NY = VPRELOAD(5,II)
           NZ = VPRELOAD(6,II)
           SENS = VPRELOAD(7,II)
           L11 = LOAD*NX*NX
           L22 = LOAD*NY*NY
           L33 = LOAD*NZ*NZ
           L12 = LOAD*NX*NY
           L23 = LOAD*NY*NZ
           L13 = LOAD*NZ*NX
           EE1X = E1X(I)
           EE1Y = E1Y(I)
           EE1Z = E1Z(I)
           EE2X = E2X(I)
           EE2Y = E2Y(I)
           EE2Z = E2Z(I)
           EE3X = E3X(I)
           EE3Y = E3Y(I)
           EE3Z = E3Z(I)
           S11 = L11*EE1X+L12*EE1Y+L13*EE1Z
           S12 = L11*EE2X+L12*EE2Y+L13*EE2Z
           S13 = L11*EE3X+L12*EE3Y+L13*EE3Z
           S21 = L12*EE1X+L22*EE1Y+L23*EE1Z
           S22 = L12*EE2X+L22*EE2Y+L23*EE2Z
           S23 = L12*EE3X+L22*EE3Y+L23*EE3Z
           S31 = L13*EE1X+L23*EE1Y+L33*EE1Z
           S32 = L13*EE2X+L23*EE2Y+L33*EE2Z
           S33 = L13*EE3X+L23*EE3Y+L33*EE3Z
           T11 = EE1X*S11+EE1Y*S21+EE1Z*S31
           T22 = EE2X*S12+EE2Y*S22+EE2Z*S32
           T33 = EE3X*S13+EE3Y*S23+EE3Z*S33
           T12 = EE1X*S12+EE1Y*S22+EE1Z*S32
           T23 = EE2X*S13+EE2Y*S23+EE2Z*S33
           T13 = EE1X*S13+EE1Y*S23+EE1Z*S33
           BPRELD(I,1)=TSTART
           BPRELD(I,2)=TEND
           BPRELD(I,3)=ZERO
           BPRELD(I,4)=T11  
           BPRELD(I,5)=T22  
           BPRELD(I,6)=T33
           BPRELD(I,7)=T12
           BPRELD(I,8)=T23
           BPRELD(I,9)=T13
           BPRELD(I,10)=SENS
          
          !write (957,'(3I5,1X,10E10.3)')II,I,N,BPRELD(I,1:10)
         ENDIF
  200    CONTINUE
       ENDDO
      ELSE
       DO I=LFT,LLT
        IFLAGINI = 0
        II=NFT+I
        JJ=IFLAG_BPRELOAD(II)
        IF (JJ == 0)GOTO 300
        IFLAGINI = 1
C-----------
        IF (IFLAGINI == 1)THEN
          TSTART = VPRELOAD(1,II)
          TEND   = VPRELOAD(2,II)
          LOAD   = VPRELOAD(3,II)
          NX = VPRELOAD(4,II)
          NY = VPRELOAD(5,II)
          NZ = VPRELOAD(6,II)
          SENS = VPRELOAD(7,II)
          L11 = LOAD*NX*NX
          L22 = LOAD*NY*NY
          L33 = LOAD*NZ*NZ
          L12 = LOAD*NX*NY
          L23 = LOAD*NY*NZ
          L13 = LOAD*NZ*NX
          BPRELD(I,1)=TSTART
          BPRELD(I,2)=TEND
          BPRELD(I,3)=ZERO
          BPRELD(I,4)=L11  
          BPRELD(I,5)=L22  
          BPRELD(I,6)=L33
          BPRELD(I,7)=L12
          BPRELD(I,8)=L23
          BPRELD(I,9)=L13
          BPRELD(I,10)=SENS
          
          !write (917,'(3I5,1X,10E10.3)')II,I,N,BPRELD(I,1:10)
        ENDIF
  300   CONTINUE
       ENDDO
      ENDIF
C-----------
      RETURN
      END

