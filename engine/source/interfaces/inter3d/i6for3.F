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
Chd|  I6FOR3                        source/interfaces/inter3d/i6for3.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I6FOR3(IRECT ,MSR   ,NSV   ,IRTL  ,STF   ,
     2                  STFN  ,IGIMP ,ES    ,EM    ,ANSMX ,
     3                  FMX   ,FMY   ,FMZ   ,PENI  ,
     4                  N1    ,N2    ,N3    ,XFACE ,ANS   ,
     5                  H1    ,H2    ,H3    ,H4    ,THK   ,
     6                  FNI   ,FXI   ,FYI   ,FZI   ,FX1   ,
     7                  FX2   ,FX3   ,FX4   ,FY1   ,FY2   ,
     8                  FY3   ,FY4   ,FZ1   ,FZ2   ,FZ3   ,
     9                  FZ4)
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
      INTEGER IGIMP
C     REAL
      my_real
     .   ANSMX, FMX, FMY, FMZ, PENI
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*)
C     REAL
      my_real
     .   STF(*), STFN(*), ES(*), EM(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: ANS,XFACE
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4,THK
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FNI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FXI,FYI,FZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FX1,FX2,FX3,FX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FY1,FY2,FY3,FY4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: FZ1,FZ2,FZ3,FZ4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, L, J3, J2, J1, I3,I2, I1
C     REAL
      my_real
     .   PENET
C-----------------------------------------------
      DO I=LFT,LLT
        PENET = (ANS(I)-THK(I))*XFACE(I)
        ANSMX = MIN(ANSMX,PENET)
        ANS(I)= MIN(ZERO,PENET)
        IF (ANS(I) == ZERO) XFACE(I)=ZERO
        ANS(I) = XFACE(I)*ANS(I)
      ENDDO
C
      IGIMP = 0
      DO I=LFT,LLT
        IGIMP = IGIMP+ABS(XFACE(I))
      ENDDO
      IF (IGIMP == 0) RETURN
C
      DO I=LFT,LLT
        FNI(I)=ANS(I)
        FXI(I)=N1(I)*FNI(I)
        FYI(I)=N2(I)*FNI(I)
        FZI(I)=N3(I)*FNI(I)
      ENDDO
C
      DO I=LFT,LLT
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
      ENDDO
C
      DO I=LFT,LLT
        IL=I+NFT
        L =IRTL(IL)
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
      ENDDO
C
      DO I=LFT,LLT
        I3=3*(I+NFT)
        I2=I3-1
        I1=I2-1
        ES(I1)=ES(I1)-FXI(I)
        ES(I2)=ES(I2)-FYI(I)
        ES(I3)=ES(I3)-FZI(I)
        FMX = FMX + FXI(I)
        FMY = FMY + FYI(I)
        FMZ = FMZ + FZI(I)
      ENDDO
C-----------
      RETURN
      END
