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
Chd|  C3FINT3                       source/elements/sh3n/coque3n/c3fint3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3FINT3(JFT  ,JLT  ,FOR  ,MOM  ,THK,
     2                   PX1  ,PY1  ,PY2  ,FX1  ,FX2,
     3                   FX3  ,FY1  ,FY2  ,FY3  ,FZ1,
     4                   FZ2  ,FZ3  ,MX1  ,MX2  ,MX3,
     5                   MY1  ,MY2  ,MY3  ,NEL  )
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
      INTEGER JFT, JLT, NEL
C     REAL
      my_real
     .   FOR(NEL,5), MOM(NEL,3), THK(*),
     .   PX1(*), PY1(*), PY2(*),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ),
     .   MX1(MVSIZ), MX2(MVSIZ), MX3(MVSIZ),
     .   MY1(MVSIZ), MY2(MVSIZ), MY3(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   F1, F2, F3, F4, F5, M1, M2, M3, M4, M5, TH2
C-----------------------------------------------
C
      DO I=JFT,JLT
C
      F1 = FOR(I,1)*THK(I)
      F3 = FOR(I,3)*THK(I)
      FX1(I) =  F1 * PX1(I) + F3 * PY1(I)
      FX2(I) = -F1 * PX1(I) + F3 * PY2(I)
      FX3(I) = -FX1(I) - FX2(I)
C
      F2 = FOR(I,2)*THK(I)
      FY1(I) =  F2 * PY1(I) + F3 * PX1(I)
      FY2(I) =  F2 * PY2(I) - F3 * PX1(I)
      FY3(I) = -FY1(I) - FY2(I)
C
      F4 = FOR(I,4)*THK(I)
      F5 = FOR(I,5)*THK(I)
      FZ1(I) =  F5 * PX1(I) + F4 * PY1(I)
      FZ2(I) = -F5 * PX1(I) + F4 * PY2(I)
      FZ3(I) = -FZ1(I) - FZ2(I)
C
      TH2 = THK(I)*THK(I)
      M2 = MOM(I,2)*TH2
      M3 = MOM(I,3)*TH2
C
      MX1(I) = -M2 * PY1(I) - M3 * PX1(I)
      MX2(I) = -M2 * PY2(I) + M3 * PX1(I)
      MX3(I) = -MX1(I) - MX2(I)
C
      M1 = MOM(I,1)*TH2
      MY1(I) =  M1 * PX1(I) + M3 * PY1(I)
      MY2(I) = -M1 * PX1(I) + M3 * PY2(I)
      MY3(I) = -MY1(I) - MY2(I)
C
      M4 = F4 * THIRD
      M5 = F5 * THIRD
      M5 = M5 * PX1(I)
C
      MY1(I) = MY1(I) + M5 * (TWO*PY1(I)+THREE*PY2(I))
     .                + M4 * PY1(I) * (PY1(I)+TWO*PY2(I))
      MY2(I) = MY2(I) + M5 * (THREE*PY1(I)+TWO*PY2(I))
     .                - M4 * PY2(I) * (TWO*PY1(I)+PY2(I))
      MY3(I) = MY3(I) + M5 * (PY1(I)+PY2(I))
     .                + M4 * (PY2(I)**2 - PY1(I)**2)
C
      M5 = M5 * PX1(I)
      M4 = M4 * PX1(I)
C
      MX1(I) = MX1(I) - M5 - M4 * (TWO*PY1(I)+PY2(I)) 
      MX2(I) = MX2(I) + M5 - M4 * (PY1(I)+TWO*PY2(I)) 
      MX3(I) = MX3(I)      - M4 * THREE*(PY1(I)+PY2(I))       
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C3FINTRZ                      source/elements/sh3n/coque3n/c3fint3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE C3FINTRZ(JFT  ,JLT  ,THK  ,AREA ,PX1  ,
     2                      PY1  ,PY2  ,F11  ,F12  ,F13  ,
     3                      F21  ,F22  ,F23  ,WXY  ,VSTRE,
     4                      VSRZ ,VMZ  ,BM0RZ,B0RZ ,BKRZ ,
     5                      BERZ ,KRZ  ,RLZ  ,DT1C ,EINT ,
     6                      OFF  ,VOL  ,NEL  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT ,JLT,NEL
        my_real 
     .     THK(*)   ,AREA(*)   ,PX1(*)     ,PY1(*)    ,
     .     PY2(*)   ,F11(*)    ,F12(*)     ,F13(*)    ,
     .     F21(*)   ,F22(*)    ,F23(*)     ,WXY(*)   ,
     .     VSTRE(NEL,5),VSRZ(NEL,5) ,VMZ(MVSIZ,3),
     .     BM0RZ(MVSIZ,3,2),B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2),KRZ(*),
     .     RLZ(MVSIZ,3),DT1C,EINT(NEL,2),OFF(*),VOL(*)
C-----------------------------------------------
c FUNCTION: strains relative to the drilling dof for Tria
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   AREA,THK(NEL)         - element area A, thickness
c  IO  FIJ(NEL)     - internal force in local system (J=1-3 node)
c  I   VSTRE(NEL,5) - stress Sxx,Syy,Sxy,Syz,Sxz
C  I   PX1,PY1,PX2=-PX1,PY2(NEL): standard [B] of Tria
c  O   VSRZ(NEL,NG)        asymmetrical stress Srz using 3of 5 places
c  O   VMZ(J,NEL)        -internal moment Mz(nodal) due to drilling dof
c  O   BM0RZ(NEL,I,J)   - constant terms of derivations for membrane
C                       I=1:A*Nx,x;I=2:A*Ny,y;I=3:A*(Nx,y+Ny,x); J=1,2(node)
C                        only store J=1,2 as f(j=3)=-f(j=1)-f(j=2)
C  O   B0RZ(NEL,J)       A*(-Nx,y+Ny,x -2Ni) for asymmetric rotation
c  O   BKRZ(NEL,J)     - Ksi terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  O   BERZ(NEL,J)    - Eta terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  I   WXY(NEL)        - asymmetric strain : 0.5*(-NxJ,y*VxJ+NyJ,x*VyJ)
c  I   KRZ(NEL)         -drilling dof modulus
c  I   RLZ(NEL,J)       - nodal Rz rotation velocity (J=1-3)
c  I   DT1C,EINT(NEL,2) - time step and internal energy (1:membrane 2:moment)
c  I   VOL,OFF(NEL)    - element volume and activation flag value
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J ,NG,NPG
       PARAMETER (NPG = 3)
        my_real
     .     S1, C2, ERZ,OFF05(MVSIZ),BRZ(MVSIZ,3),VRLZ(MVSIZ),
     .     FX1,FX2,FY1,SRZM(MVSIZ),A_HAMMER(NPG,2),A05(MVSIZ),
     .     THK_6(MVSIZ),BN1RZ,BN2RZ,BN3RZ
      DATA A_HAMMER /
     1 0.166666666666667,0.666666666666667,0.166666666666667,
     2 0.166666666666667,0.166666666666667,0.666666666666667/
C--------------------------------c
       DO I=JFT,JLT
        VMZ(I,1)=(BM0RZ(I,1,1)*VSTRE(I,1)+BM0RZ(I,2,1)*VSTRE(I,2)
     .           +BM0RZ(I,3,1)*VSTRE(I,3))*THK(I)
        VMZ(I,2)=(BM0RZ(I,1,2)*VSTRE(I,1)+BM0RZ(I,2,2)*VSTRE(I,2)
     .           +BM0RZ(I,3,2)*VSTRE(I,3))*THK(I)
        VMZ(I,3)=-VMZ(I,1)-VMZ(I,2)
       ENDDO
c
       DO I=JFT,JLT
        SRZM(I)=ZERO
        OFF05(I)=ONE_OVER_6*OFF(I)*VOL(I)
        A05(I)=HALF/AREA(I)
        THK_6(I)=ONE_OVER_6*THK(I)
       ENDDO
C--------NxI,x *A------pay attention Bi*A--- RLZ: VRZ--
      DO NG =1,NPG
       DO I=JFT,JLT
        BN1RZ=BKRZ(I,1)*A_HAMMER(NG,1)+BERZ(I,1)*A_HAMMER(NG,2)
        BN2RZ=BKRZ(I,2)*A_HAMMER(NG,1)+BERZ(I,2)*A_HAMMER(NG,2)
        BN3RZ=-BN1RZ-BN2RZ
        BRZ(I,1)=B0RZ(I,1)+BN1RZ
        BRZ(I,2)=B0RZ(I,2)+BN2RZ
        BRZ(I,3)=B0RZ(I,3)+BN3RZ
        VRLZ(I)= WXY(I)+(BRZ(I,1)*RLZ(I,1)+
     1                   BRZ(I,2)*RLZ(I,2)+BRZ(I,3)*RLZ(I,3))*A05(I)
       ENDDO
       DO I=JFT,JLT
        ERZ= VRLZ(I)*DT1C
        EINT(I,1) = EINT(I,1)+ VSRZ(I,NG)*ERZ*OFF05(I)
        VSRZ(I,NG)= VSRZ(I,NG)+KRZ(I)*ERZ*OFF(I)
        EINT(I,1) = EINT(I,1)+ VSRZ(I,NG)*ERZ*OFF05(I)
        SRZM(I)=SRZM(I)+VSRZ(I,NG)
       ENDDO
C        
        DO J=1,3
         DO I=JFT,JLT
          C2=THK_6(I)*VSRZ(I,NG)
          VMZ(I,J)= VMZ(I,J)+BRZ(I,J)*C2
         ENDDO
        ENDDO
      END DO !NG =1,NPG
C-------------------------------------------------
C        constant part PX2=-PX1
C-------------------------------------------------
        DO I=JFT,JLT
          C2=THK(I)*SRZM(I)*ONE_OVER_6
          FX1=-C2*PY1(I)
          FX2=-C2*PY2(I)
C
          F11(I) = F11(I) + FX1
          F12(I) = F12(I) + FX2
          F13(I) = F13(I) - FX1 - FX2
C
          FY1=C2*PX1(I)
          F21(I) = F21(I) + FY1
          F22(I) = F22(I) - FY1
C          F23(I) = F23(I) - FY1
        ENDDO
C
       RETURN
       END
