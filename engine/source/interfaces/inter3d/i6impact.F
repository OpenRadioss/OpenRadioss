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
Chd|  I6IPMACT                      source/interfaces/inter3d/i6impact.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I6IPMACT(LOLD   ,IRTL   ,IRTLO   ,CST   ,
     1                    IRECT  ,MSR    ,NSV     ,V     ,
     2                    VNI    ,VNT    ,N1      ,N2    ,
     3                    N3     ,SSC    ,TTC     ,H1    ,
     4                    H2     ,H3     ,H4      ,XFACE)
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
      INTEGER IRECT(4,*),MSR(*),NSV(*), IRTL(*), IRTLO(*), LOLD(*)
C     REAL
      my_real
     .   V(3,*),CST(2,*),VNI(*),VNT(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  N1,N2,N3,SSC,TTC,XFACE
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IL,IG,NN
C     REAL
      my_real
     .   VPX(LLT),VPY(LLT),VPZ(LLT),VIX(LLT),VIY(LLT),VIZ(LLT),
     .   VX1(LLT),VY1(LLT),VZ1(LLT),VX2(LLT),VY2(LLT),VZ2(LLT),
     .   VX3(LLT),VY3(LLT),VZ3(LLT),VX4(LLT),VY4(LLT),VZ4(LLT)
      my_real
     .   DVX,DVY,DVZ,VTX,VTY,VTZ
C-----------------------------------------------
!      EQUIVALENCE (SSC,BV(1,23)),(TTC,BV(1,24)),(XFACE,BV(1,29))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (H1,BV(1,25)) ,(H2,BV(1,26)) ,(H3,BV(1,27))
!      EQUIVALENCE (H4,BV(1,28))
C=======================================================================
      DO I=LFT,LLT
        IL = I+NFT
c
        IF (XFACE(I) == ZERO) THEN            ! no impact
          IRTLO(IL) = 0
          LOLD(I)   = 0
        ELSE
          LOLD(I) = IABS(IRTLO(IL))
          IF (LOLD(I) == 0) THEN              ! first time impact
            IRTLO(IL) = IRTL(IL)*XFACE(I)
            CST(1,IL) = SSC(I)
            CST(2,IL) = TTC(I)
          ENDIF
        ENDIF
      ENDDO
c-----------------------------
      DO I=LFT,LLT
        IL = I+NFT
        IF (LOLD(I) == 0) THEN              ! first time impact
          VNI(I) = ZERO
          VNT(I) = ZERO
        ELSE
          IG = NSV(IL)
          L  = IRTL(IL)
c
          VIX(I) = V(1,IG)
          VIY(I) = V(2,IG)
          VIZ(I) = V(2,IG)
c
          NN = MSR(IRECT(1,LOLD(I)))
          VX1(I) = V(1,NN)
          VY1(I) = V(2,NN)
          VZ1(I) = V(2,NN)
          NN = MSR(IRECT(2,LOLD(I)))
          VX2(I) = V(1,NN)
          VY2(I) = V(2,NN)
          VZ2(I) = V(2,NN)
          NN = MSR(IRECT(3,LOLD(I)))
          VX3(I) = V(1,NN)
          VY3(I) = V(2,NN)
          VZ3(I) = V(2,NN)
          NN = MSR(IRECT(4,LOLD(I)))
          VX4(I) = V(1,NN)
          VY4(I) = V(2,NN)
          VZ4(I) = V(2,NN)
c
          VPX(I) = H1(I)*VX1(I)+H2(I)*VX2(I)+H3(I)*VX3(I)+H4(I)*VX4(I)
          VPY(I) = H1(I)*VY1(I)+H2(I)*VY2(I)+H3(I)*VY3(I)+H4(I)*VY4(I)
          VPZ(I) = H1(I)*VZ1(I)+H2(I)*VZ2(I)+H3(I)*VZ3(I)+H4(I)*VZ4(I)
          DVX = VIX(I)-VPX(I)
          DVY = VIY(I)-VPY(I)
          DVZ = VIZ(I)-VPZ(I)
c
          VNI(I) = N1(I) * DVX + N2(I) * DVY + N3(I) * DVZ

          VTX = DVX - VNI(I)*N1(I)
          VTY = DVY - VNI(I)*N2(I)
          VTZ = DVZ - VNI(I)*N3(I)
          VNT(I) = SQRT(VTX**2 +  VTY**2 +  VTZ**2)
        ENDIF
      ENDDO
C-----------
      RETURN
      END
