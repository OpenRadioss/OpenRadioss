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
Chd|  S6CLKE3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|-- called by -----------
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S6CKEL3                       source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        S6CKEG3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|        S8ZKEB3                       source/elements/solid/solide8z/s8zlke3.F
Chd|====================================================================
      SUBROUTINE S6CLKE3(
     .   PXC1, PXC2, PXC3, PXC4,
     .   PYC1, PYC2, PYC3, PYC4,
     .   PZC1, PZC2, PZC3, PZC4,
     .   PX1H, PX2H, PX3H, 
     .   PY1H, PY2H, PY3H, 
     .   PZ1H, PZ2H, PZ3H, 
     .   JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .   B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .   DD  ,GG  ,VOL ,ZI  ,WI ,NU ,
     .   K11 ,K12  ,K13  ,K14  ,K15 ,
     .   K16 ,K22  ,K23  ,K24  ,K25 ,
     .   K26 ,K33  ,K34  ,K35  ,K36 ,
     .   K44 ,K45  ,K46  ,K55  ,K56 ,
     .   K66 )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),  
     .   PX1H(*), PX2H(*), PX3H(*),  
     .   PY1H(*), PY2H(*), PY3H(*),   
     .   PZ1H(*), PZ2H(*), PZ3H(*),   
     .   JI33(*),B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   B1122(*),B1221(*),B2212(*),B1121(*),
     .   B1122H(*),B1221H(*),B2212H(*),B1121H(*),
     .   ZI,WI,NU(*),DD(3,3,*),GG(*),VOL(*),
     .   K11(9,*) ,K12(9,*)  ,K13(9,*)  ,K14(9,*)  ,K15(9,*) ,
     .   K16(9,*) ,K22(9,*)  ,K23(9,*)  ,K24(9,*)  ,K25(9,*) ,
     .   K26(9,*) ,K33(9,*)  ,K34(9,*)  ,K35(9,*)  ,K36(9,*) ,
     .   K44(9,*) ,K45(9,*)  ,K46(9,*)  ,K55(9,*)  ,K56(9,*) ,
     .   K66(9,*) 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IS
C     REAL ou REAL*8
      my_real
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),
     .  PX4(MVSIZ),PX5(MVSIZ),PX6(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),
     .  PY4(MVSIZ),PY5(MVSIZ),PY6(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),
     .  PZ4(MVSIZ),PZ5(MVSIZ),PZ6(MVSIZ),
     .  PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .  PXY5(MVSIZ),PXY6(MVSIZ),
     .  PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .  PYX5(MVSIZ),PYX6(MVSIZ),
     .  PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .  PXZ5(MVSIZ),PXZ6(MVSIZ),
     .  PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .  PZX5(MVSIZ),PZX6(MVSIZ),
     .  PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .  PYZ5(MVSIZ),PYZ6(MVSIZ),
     .  PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .  PZY5(MVSIZ),PZY6(MVSIZ),
     .  BXY1(MVSIZ),BXY2(MVSIZ),BXY3(MVSIZ),BXY4(MVSIZ),
     .  BXY5(MVSIZ),BXY6(MVSIZ),
     .  BYX1(MVSIZ),BYX2(MVSIZ),BYX3(MVSIZ),BYX4(MVSIZ),
     .  BYX5(MVSIZ),BYX6(MVSIZ),
     .  BXZ1(MVSIZ),BXZ2(MVSIZ),BXZ3(MVSIZ),BXZ4(MVSIZ),
     .  BXZ5(MVSIZ),BXZ6(MVSIZ),
     .  BZX1(MVSIZ),BZX2(MVSIZ),BZX3(MVSIZ),BZX4(MVSIZ),
     .  BZX5(MVSIZ),BZX6(MVSIZ),
     .  BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .  BYZ5(MVSIZ),BYZ6(MVSIZ),
     .  BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     .  BZY5(MVSIZ),BZY6(MVSIZ)
      my_real
     .   ZNU,ZNU1,J2,J3
C ----------PXI,PYI,PZI-------------------------------------
C | PX  BYX BZX|
C | BXY PY  BZY|
C | BXZ BYZ PZ |
      DO I=LFT,LLT
       PX1(I) = PXC1(I)-PXC4(I) + ZI*(PX1H(I)-PXC1(I))
       PX2(I) = PXC2(I)-PXC4(I) + ZI*(PX2H(I)-PXC2(I))
       PX3(I) = PXC3(I)-PXC4(I) + ZI*(PX3H(I)-PXC3(I))
       PX4(I) = PXC1(I)+PXC4(I) + ZI*(PX1H(I)+PXC1(I))
       PX5(I) = PXC2(I)+PXC4(I) + ZI*(PX2H(I)+PXC2(I))
       PX6(I) = PXC3(I)+PXC4(I) + ZI*(PX3H(I)+PXC3(I))
      ENDDO
      DO I=LFT,LLT
       PY1(I) = PYC1(I)-PYC4(I) + ZI*(PY1H(I)-PYC1(I))
       PY2(I) = PYC2(I)-PYC4(I) + ZI*(PY2H(I)-PYC2(I))
       PY3(I) = PYC3(I)-PYC4(I) + ZI*(PY3H(I)-PYC3(I))
       PY4(I) = PYC1(I)+PYC4(I) + ZI*(PY1H(I)+PYC1(I))
       PY5(I) = PYC2(I)+PYC4(I) + ZI*(PY2H(I)+PYC2(I))
       PY6(I) = PYC3(I)+PYC4(I) + ZI*(PY3H(I)+PYC3(I))
      ENDDO
      DO I=LFT,LLT
       PZ1(I) = PZC1(I)-PZC4(I) + ZI*(PZ1H(I)-PZC1(I))
       PZ2(I) = PZC2(I)-PZC4(I) + ZI*(PZ2H(I)-PZC2(I))
       PZ3(I) = PZC3(I)-PZC4(I) + ZI*(PZ3H(I)-PZC3(I))
       PZ4(I) = PZC1(I)+PZC4(I) + ZI*(PZ1H(I)+PZC1(I))
       PZ5(I) = PZC2(I)+PZC4(I) + ZI*(PZ2H(I)+PZC2(I))
       PZ6(I) = PZC3(I)+PZC4(I) + ZI*(PZ3H(I)+PZC3(I))
      ENDDO
C ----------BXYI,BYXI...-------------------------------------
      DO I=LFT,LLT
       ZNU = ZI*NU(I)
       ZNU1 = ZNU/(ONE - NU(I))
       BXY1(I) = -ZNU*PX1H(I)
       BYX1(I) =  ZNU*PYC1(I)
       BXY2(I) = -ZNU*PX2H(I)
       BYX2(I) =  ZNU*PYC2(I)
       BXY3(I) = -ZNU*PX3H(I)
       BYX3(I) =  ZNU*PYC3(I)
       BXY4(I) = -ZNU*PX1H(I)
       BYX4(I) = -ZNU*PYC1(I)
       BXY5(I) = -ZNU*PX2H(I)
       BYX5(I) = -ZNU*PYC2(I)
       BXY6(I) = -ZNU*PX3H(I)
       BYX6(I) = -ZNU*PYC3(I)
C
       BXZ1(I) = -ZNU*PX1H(I)+ZNU1*PXC1(I)
       BZX1(I) =  ZNU1*PZC1(I)
       BXZ2(I) = -ZNU*PX2H(I)+ZNU1*PXC2(I)
       BZX2(I) =  ZNU1*PZC2(I)
       BXZ3(I) = -ZNU*PX3H(I)+ZNU1*PXC3(I)
       BZX3(I) =  ZNU1*PZC3(I)
       BXZ4(I) = -ZNU*PX1H(I)-ZNU1*PXC1(I)
       BZX4(I) = -ZNU1*PZC1(I)
       BXZ5(I) = -ZNU*PX2H(I)-ZNU1*PXC2(I)
       BZX5(I) = -ZNU1*PZC2(I)
       BXZ6(I) = -ZNU*PX3H(I)-ZNU1*PXC3(I)
       BZX6(I) = -ZNU1*PZC3(I)
C
       BYZ1(I) = -ZNU1*PY1H(I)+ZNU*PYC1(I)
       BZY1(I) = -ZNU1*PZ1H(I)
       BYZ2(I) = -ZNU1*PY2H(I)+ZNU*PYC2(I)
       BZY2(I) = -ZNU1*PZ2H(I)
       BYZ3(I) = -ZNU1*PY3H(I)+ZNU*PYC3(I)
       BZY3(I) = -ZNU1*PZ3H(I)
       BYZ4(I) = -ZNU1*PY1H(I)-ZNU*PYC1(I)
       BZY4(I) = -ZNU1*PZ1H(I)
       BYZ5(I) = -ZNU1*PY2H(I)-ZNU*PYC2(I)
       BZY5(I) = -ZNU1*PZ2H(I)
       BYZ6(I) = -ZNU1*PY3H(I)-ZNU*PYC3(I)
       BZY6(I) = -ZNU1*PZ3H(I)
      ENDDO
C ----------shear,PX,PY->DXY;PXY,PXZ...pour DYZ,DXZ-------------
C | PY  PX   0 |
C | PXY PYZ PZY|
C | PXZ PYX PZX|
      DO I=LFT,LLT
       J2 = TWO*JI33(I)
       J3 = THREE*JI33(I)
       PXZ1(I) = -J2-B1X(I,1)+B1X(I,2)+ ZI*(-B1XH(I,1)+B1XH(I,2))  
       PXZ2(I) = -J3-B1221(I)-B1X(I,2)+ ZI*(-B1221H(I)-B1XH(I,2)) 
       PXZ3(I) = -J3+B1122(I)+B1X(I,1)+ ZI*(B1122H(I)+B1XH(I,1))
       PXZ4(I) = J2+B1X(I,1)-B1X(I,2) + ZI*(B1XH(I,1)-B1XH(I,2))  
       PXZ5(I) = J3+B1221(I)+B1X(I,2) + ZI*(B1221H(I)+B1XH(I,2))  
       PXZ6(I) = J3-B1122(I)-B1X(I,1) + ZI*(-B1122H(I)-B1XH(I,1)) 
C 
       PYZ1(I)= -J2+B2Y(I,1)-B2Y(I,2) + ZI*(B2YH(I,1)-B2YH(I,2))
       PYZ2(I)= -J3+B1122(I)+B2Y(I,2) + ZI*(B1122H(I)+B2YH(I,2)) 
       PYZ3(I)= -J3-B1221(I)-B2Y(I,1) + ZI*(-B1221H(I)-B2YH(I,1))  
       PYZ4(I)= J2-B2Y(I,1)+B2Y(I,2)  + ZI*(-B2YH(I,1)+B2YH(I,2))
       PYZ5(I)= J3-B1122(I)-B2Y(I,2)  + ZI*(-B1122H(I)-B2YH(I,2))  
       PYZ6(I)= J3+B1221(I)+B2Y(I,1)  + ZI*( B1221H(I)+B2YH(I,1)) 
      ENDDO
      DO I=LFT,LLT
       PYX1(I) = -B1Y(I,1)+B1Y(I,2)+ZI*(-B1YH(I,1)+B1YH(I,2)) 
       PYX2(I) = -B2212(I)-B1Y(I,2)+ZI*(-B2212H(I)-B1YH(I,2)) 
       PYX3(I) =  B2212(I)+B1Y(I,1)+ZI*(B2212H(I)+B1YH(I,1)) 
       PYX4(I) =  B1Y(I,1)-B1Y(I,2)+ZI*(B1YH(I,1)-B1YH(I,2)) 
       PYX5(I) =  B2212(I)+B1Y(I,2)+ZI*(B2212H(I)+B1YH(I,2)) 
       PYX6(I) = -B2212(I)-B1Y(I,1)+ZI*(-B2212H(I)-B1YH(I,1)) 
      ENDDO
      DO I=LFT,LLT
       PXY1(I)= B2X(I,1)-B2X(I,2) +ZI*(B2XH(I,1)-B2XH(I,2))  
       PXY2(I)= B1121(I)+B2X(I,2) +ZI*(B1121H(I)+B2XH(I,2))  
       PXY3(I)= -B1121(I)-B2X(I,1)+ZI*(-B1121H(I)-B2XH(I,1))  
       PXY4(I)= -B2X(I,1)+B2X(I,2)+ZI*(-B2XH(I,1)+B2XH(I,2))  
       PXY5(I)=-B1121(I)-B2X(I,2) +ZI*(-B1121H(I)-B2XH(I,2))  
       PXY6(I)= B1121(I)+B2X(I,1) +ZI*( B1121H(I)+B2XH(I,1))  
      ENDDO
C
      DO I=LFT,LLT
       PZX1(I)= HALF*(PXC1(I) + ZI*PX1H(I))  
       PZX2(I)= HALF*(PXC2(I) + ZI*PX2H(I))   
       PZX3(I)= HALF*(PXC3(I) + ZI*PX3H(I))   
       PZX4(I)= HALF*(PXC1(I) + ZI*PX1H(I))   
       PZX5(I)= HALF*(PXC2(I) + ZI*PX2H(I))  
       PZX6(I)= HALF*(PXC3(I) + ZI*PX3H(I))  
C
       PZY1(I)= HALF*(PYC1(I) + ZI*PY1H(I))   
       PZY2(I)= HALF*(PYC2(I) + ZI*PY2H(I)) 
       PZY3(I)= HALF*(PYC3(I) + ZI*PY3H(I))  
       PZY4(I)= HALF*(PYC1(I) + ZI*PY1H(I))  
       PZY5(I)= HALF*(PYC2(I) + ZI*PY2H(I)) 
       PZY6(I)= HALF*(PYC3(I) + ZI*PY3H(I)) 
      ENDDO
Cqz-------symm(diag)------
      IS =1
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,DD,K11,IS) 
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,DD,K22,IS) 
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,DD,K33,IS) 
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K44,IS) 
       CALL S8ZKEB3(LFT,LLT,PX5, PY5 ,PZ5 ,PX5, PY5, PZ5,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K55,IS) 
       CALL S8ZKEB3(LFT,LLT,PX6, PY6 ,PZ6 ,PX6, PY6, PZ6,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K66,IS) 
C
       CALL S6CKEG3(LFT,LLT,PY1, PX1 ,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .              PY1,PX1,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,GG,K11,IS) 
       CALL S6CKEG3(LFT,LLT,PY2, PX2 ,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .              PY2,PX2,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,GG,K22,IS) 
       CALL S6CKEG3(LFT,LLT,PY3, PX3 ,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .              PY3,PX3,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,GG,K33,IS) 
       CALL S6CKEG3(LFT,LLT,PY4, PX4 ,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .              PY4,PX4,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K44,IS) 
       CALL S6CKEG3(LFT,LLT,PY5, PX5 ,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .              PY5,PX5,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K55,IS) 
       CALL S6CKEG3(LFT,LLT,PY6, PX6 ,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .              PY6,PX6,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K66,IS) 
Cqz-------non symm------
      IS =0
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,DD,K12,IS) 
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,DD,K13,IS) 
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K14,IS) 
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX5, PY5, PZ5,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K15,IS) 
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX6, PY6, PZ6,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K16,IS) 
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,DD,K23,IS) 
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K24,IS) 
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX5, PY5, PZ5,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K25,IS) 
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX6, PY6, PZ6,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K26,IS) 
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K34,IS) 
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX5, PY5, PZ5,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K35,IS) 
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX6, PY6, PZ6,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K36,IS) 
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX5, PY5, PZ5,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K45,IS) 
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX6, PY6, PZ6,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K46,IS) 
       CALL S8ZKEB3(LFT,LLT,PX5, PY5 ,PZ5 ,PX6, PY6, PZ6,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K56,IS) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       CALL S6CKEG3(LFT,LLT,PY1, PX1 ,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .              PY2,PX2,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,GG,K12,IS) 
       CALL S6CKEG3(LFT,LLT,PY1, PX1 ,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .              PY3,PX3,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,GG,K13,IS) 
       CALL S6CKEG3(LFT,LLT,PY1, PX1 ,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .              PY4,PX4,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K14,IS) 
       CALL S6CKEG3(LFT,LLT,PY1, PX1 ,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .              PY5,PX5,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K15,IS) 
       CALL S6CKEG3(LFT,LLT,PY1, PX1 ,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .              PY6,PX6,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K16,IS) 
       CALL S6CKEG3(LFT,LLT,PY2, PX2 ,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .              PY3,PX3,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,GG,K23,IS) 
       CALL S6CKEG3(LFT,LLT,PY2, PX2 ,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .              PY4,PX4,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K24,IS) 
       CALL S6CKEG3(LFT,LLT,PY2, PX2 ,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .              PY5,PX5,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K25,IS) 
       CALL S6CKEG3(LFT,LLT,PY2, PX2 ,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .              PY6,PX6,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K26,IS) 
       CALL S6CKEG3(LFT,LLT,PY3, PX3 ,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .              PY4,PX4,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K34,IS) 
       CALL S6CKEG3(LFT,LLT,PY3, PX3 ,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .              PY5,PX5,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K35,IS) 
       CALL S6CKEG3(LFT,LLT,PY3, PX3 ,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .              PY6,PX6,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K36,IS) 
       CALL S6CKEG3(LFT,LLT,PY4, PX4 ,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .              PY5,PX5,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K45,IS) 
       CALL S6CKEG3(LFT,LLT,PY4, PX4 ,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .              PY6,PX6,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K46,IS) 
       CALL S6CKEG3(LFT,LLT,PY5, PX5 ,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .              PY6,PX6,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K56,IS) 
C
      RETURN
      END
Chd|====================================================================
Chd|  S6CKEG3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|-- called by -----------
Chd|        S6CLKE3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CKEG3(LFT,LLT,
     .   PXYI, PYXI ,P21I, P32I ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,P21J, P32J ,PXZJ,PZXJ ,PYZJ,PZYJ,
     .   G, KIJ,IS)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,IS
C     REAL ou REAL*8
      my_real
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*), 
     .   P21I(*), P32I(*) ,P21J(*), P32J(*) ,G(*), KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C | PXY PYX  0 |
C | P21 PYZ PZY|
C | PXZ P32 PZX|
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+
     .        G(I)*(PXYI(I)*PXYJ(I)+PXZI(I)*PXZJ(I)+P21I(I)*P21J(I))
        KIJ(2,2,I)=KIJ(2,2,I)+
     .        G(I)*(PYXI(I)*PYXJ(I)+PYZI(I)*PYZJ(I)+P32I(I)*P32J(I))
        KIJ(3,3,I)=KIJ(3,3,I)+G(I)*(PZXI(I)*PZXJ(I)+PZYI(I)*PZYJ(I))
        KIJ(1,2,I)=KIJ(1,2,I)+
     .        G(I)*(PXYI(I)*PYXJ(I)+P21I(I)*PYZJ(I)+PXZI(I)*P32J(I))
        KIJ(1,3,I)=KIJ(1,3,I)+
     .        G(I)*(P21I(I)*PZYJ(I)+PXZI(I)*PZXJ(I))
        KIJ(2,3,I)=KIJ(2,3,I)+
     .        G(I)*(PYZI(I)*PZYJ(I)+P32I(I)*PZXJ(I))
       ENDDO
      IF (IS.EQ.0) THEN
       DO I=LFT,LLT
        KIJ(2,1,I)=KIJ(2,1,I)+
     .        G(I)*(PXYJ(I)*PYXI(I)+P21J(I)*PYZI(I)+PXZJ(I)*P32I(I))
        KIJ(3,1,I)=KIJ(3,1,I)+
     .        G(I)*(P21J(I)*PZYI(I)+PXZJ(I)*PZXI(I))
        KIJ(3,2,I)=KIJ(3,2,I)+
     .        G(I)*(PYZJ(I)*PZYI(I)+P32J(I)*PZXI(I))
       ENDDO
      ENDIF 
C
      RETURN
      END
