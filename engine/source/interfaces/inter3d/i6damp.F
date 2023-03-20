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
Chd|  I6DAMP                        source/interfaces/inter3d/i6damp.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|-- calls ---------------
Chd|        NINTERP                       source/interfaces/int14/ninterp.F
Chd|====================================================================
      SUBROUTINE I6DAMP(V      ,NPC    ,TF     ,IRECT  ,
     1                  MSR    ,NSV    ,IRTL   ,IRTLO  ,
     2                  CST    ,ES     ,EM     ,VISC   ,
     3                  NDAMP1 ,NDAMP2 ,LOLD   ,MASS   ,
     4                  VNI    ,ASCALF ,ASCALV ,FSCALV ,
     5                  H1     ,H2     ,H3     ,H4     ,
     6                  FNI    ,FXI    ,FYI    ,FZI    ,
     7                  FX1    ,FX2    ,FX3    ,FX4    ,
     8                  FY1    ,FY2    ,FY3    ,FY4    ,
     9                  FZ1    ,FZ2    ,FZ3    ,FZ4)
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
      INTEGER NDAMP1 ,NDAMP2
      INTEGER IRECT(4,*),MSR(*),NSV(*),IRTL(*),IRTLO(*),NPC(*),LOLD(*)
C     REAL
      my_real
     .   VISC,ASCALF,ASCALV,FSCALV
      my_real
     .   V(3,*),CST(2,*),ES(*),EM(*),TF(*),MASS(*),VNI(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4,FNI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FXI,FYI,FZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FX1,FX2,FX3,FX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FY1,FY2,FY3,FY4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FZ1,FZ2,FZ3,FZ4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,IL, L, J3, J2, J1, I3, I2, I1
C     REAL
      my_real
     .   FDAMP
      my_real
     .   VIS(LLT),VISCV(LLT),FACT(LLT),XX(LLT)
C-----------------------------------------------
C     Coefficient d'amortissement
C--------------------------------
      IF (NDAMP1 > 0) THEN    ! velocity function
        DO I=LFT,LLT
          XX(I) = VNI(I)*ASCALV
        ENDDO 
        CALL NINTERP(NDAMP1,NPC,TF,LLT,XX,VISCV)
      ELSE
        VISCV(1:LLT) = ZERO
      ENDIF 
c
      IF (NDAMP2 > 0) THEN    ! Force amplification factor
        DO I=LFT,LLT
          XX(I) = FNI(I)*ASCALF
        ENDDO 
        CALL NINTERP(NDAMP2,NPC,TF,LLT,XX,FACT)
      ELSE
        FACT(1:LLT) = ONE
      ENDIF 
C--------------------------------
      DO  I=LFT,LLT
        IL = I+NFT
        FXI(I) = ZERO
        FYI(I) = ZERO
        FZI(I) = ZERO
c
        IF (LOLD(I) /= 0) THEN
          IG = NSV(IL)     
          L  = IRTL(IL)
c
          FDAMP = - (VISC*VNI(I) + FSCALV*VISCV(I))*FACT(I)   
c     
c          FXI(I) = N1(I)*FDAMP
c          FYI(I) = N2(I)*FDAMP
c          FZI(I) = N3(I)*FDAMP
          FXI(I) = FDAMP
          FYI(I) = FDAMP
          FZI(I) = FDAMP
c
          FX1(I)=FXI(I)*H1(I)
          FY1(I)=FYI(I)*H1(I)
          FZ1(I)=FZI(I)*H1(I)
C
          FX2(I)=FXI(I)*H2(I)
          FY2(I)=FYI(I)*H2(I)
          FZ2(I)=FZI(I)*H2(I)
C
          FX3(I)=FXI(I)*H3(I)
          FY3(I)=FYI(I)*H3(I)
          FZ3(I)=FZI(I)*H3(I)
C
          FX4(I)=FXI(I)*H4(I)
          FY4(I)=FYI(I)*H4(I)
          FZ4(I)=FZI(I)*H4(I)
c
c         Force visc main        
c
          J3=3*IRECT(1,L)
          J2=J3-1
          J1=J2-1
          EM(J1)=EM(J1)+FX1(I)
          EM(J2)=EM(J2)+FY1(I)
          EM(J3)=EM(J3)+FZ1(I)
C
          J3=3*IRECT(2,L)
          J2=J3-1
          J1=J2-1
          EM(J1)=EM(J1)+FX2(I)
          EM(J2)=EM(J2)+FY2(I)
          EM(J3)=EM(J3)+FZ2(I)
C
          J3=3*IRECT(3,L)
          J2=J3-1
          J1=J2-1
          EM(J1)=EM(J1)+FX3(I)
          EM(J2)=EM(J2)+FY3(I)
          EM(J3)=EM(J3)+FZ3(I)
C
          J3=3*IRECT(4,L)
          J2=J3-1
          J1=J2-1
          EM(J1)=EM(J1)+FX4(I)
          EM(J2)=EM(J2)+FY4(I)
          EM(J3)=EM(J3)+FZ4(I)
c
c         Force visc secnd        
c
          I3 = 3*IL
          I2 = I3-1
          I1 = I2-1
          ES(I1) = ES(I1)-FXI(I)
          ES(I2) = ES(I2)-FYI(I)
          ES(I3) = ES(I3)-FZI(I)
c
        ENDIF    ! LOLD
C
      ENDDO      ! I=LFT,LLT
C-----------
      RETURN
      END
