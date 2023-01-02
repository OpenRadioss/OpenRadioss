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
Chd|  FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|-- called by -----------
Chd|        FXBSINI                       source/constraints/fxbody/fxbsini.F
Chd|-- calls ---------------
Chd|        SCHREP                        source/constraints/fxbody/fsigsini.F
Chd|        SDEFOI                        source/constraints/fxbody/fsigsini.F
Chd|        SDERII                        source/constraints/fxbody/fsigsini.F
Chd|        SM1INIF                       source/constraints/fxbody/fsigsini.F
Chd|        SRHOI                         source/constraints/fxbody/fsigsini.F
Chd|====================================================================
      SUBROUTINE FSIGSINI(FXBELM, IPARG , X     , PM, IXS, 
     .                    GEO   , FXBMOD, FXBSIG, R , NELS)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBELM(*), IPARG(NPARG,*), IXS(NIXS,*), NELS
      my_real
     .        FXBSIG(*), X(3,*), PM(NPROPM,*), FXBMOD(*),
     .        GEO(NPROPG,*), R(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, OFFSET, LAST, NFT, NFS, NG, IEL,
     .        N1, N2, N3, N4, N5, N6, N7, N8
      INTEGER MAT(MVSIZ)
      my_real
     .   X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ), 
     .   X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ), 
     .   X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ), 
     .   X4(MVSIZ), Y4(MVSIZ), Z4(MVSIZ), 
     .   X5(MVSIZ), Y5(MVSIZ), Z5(MVSIZ), 
     .   X6(MVSIZ), Y6(MVSIZ), Z6(MVSIZ), 
     .   X7(MVSIZ), Y7(MVSIZ), Z7(MVSIZ), 
     .   X8(MVSIZ), Y8(MVSIZ), Z8(MVSIZ),
     .   D11, D12, D13, D21, D22, D23, D31, D32, D33, D41, D42, D43, 
     .   D51, D52, D53, D61, D62, D63, D71, D72, D73, D81, D82, D83,
     .   VX1(MVSIZ), VY1(MVSIZ), VZ1(MVSIZ), 
     .   VX2(MVSIZ), VY2(MVSIZ), VZ2(MVSIZ), 
     .   VX3(MVSIZ), VY3(MVSIZ), VZ3(MVSIZ), 
     .   VX4(MVSIZ), VY4(MVSIZ), VZ4(MVSIZ), 
     .   VX5(MVSIZ), VY5(MVSIZ), VZ5(MVSIZ), 
     .   VX6(MVSIZ), VY6(MVSIZ), VZ6(MVSIZ), 
     .   VX7(MVSIZ), VY7(MVSIZ), VZ7(MVSIZ), 
     .   VX8(MVSIZ), VY8(MVSIZ), VZ8(MVSIZ),
     .   X1P(MVSIZ), Y1P(MVSIZ), Z1P(MVSIZ), 
     .   X2P(MVSIZ), Y2P(MVSIZ), Z2P(MVSIZ), 
     .   X3P(MVSIZ), Y3P(MVSIZ), Z3P(MVSIZ), 
     .   X4P(MVSIZ), Y4P(MVSIZ), Z4P(MVSIZ), 
     .   X5P(MVSIZ), Y5P(MVSIZ), Z5P(MVSIZ), 
     .   X6P(MVSIZ), Y6P(MVSIZ), Z6P(MVSIZ), 
     .   X7P(MVSIZ), Y7P(MVSIZ), Z7P(MVSIZ), 
     .   X8P(MVSIZ), Y8P(MVSIZ), Z8P(MVSIZ),
     .   PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),
     .   PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),
     .   PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ),
     .   VOL(MVSIZ), VOLP(MVSIZ), D1(MVSIZ), D2(MVSIZ), D3(MVSIZ),
     .   D4(MVSIZ), D5(MVSIZ), D6(MVSIZ), RHOP(MVSIZ), SIG(6,MVSIZ),
     .   EINT(MVSIZ), SIGL(6,MVSIZ)
C
      DO IG=1,NELS,MVSIZ
        OFFSET=IG-1
        LAST=MIN(MVSIZ,NELS-OFFSET)
        NFT=OFFSET*13
        NFS=OFFSET*7
        DO I=1,LAST
          NG=FXBELM(NFT+13*(I-1)+1)
          IEL=IPARG(3,NG)+FXBELM(NFT+13*(I-1)+2)
          MAT(I)=IXS(1,IEL)
          X1(I)=X(1,IXS(2,IEL))
          Y1(I)=X(2,IXS(2,IEL))
          Z1(I)=X(3,IXS(2,IEL))
          X2(I)=X(1,IXS(3,IEL))
          Y2(I)=X(2,IXS(3,IEL))
          Z2(I)=X(3,IXS(3,IEL))
          X3(I)=X(1,IXS(4,IEL))
          Y3(I)=X(2,IXS(4,IEL))
          Z3(I)=X(3,IXS(4,IEL))
          X4(I)=X(1,IXS(5,IEL))
          Y4(I)=X(2,IXS(5,IEL))
          Z4(I)=X(3,IXS(5,IEL))
          X5(I)=X(1,IXS(6,IEL))
          Y5(I)=X(2,IXS(6,IEL))
          Z5(I)=X(3,IXS(6,IEL))
          X6(I)=X(1,IXS(7,IEL))
          Y6(I)=X(2,IXS(7,IEL))
          Z6(I)=X(3,IXS(7,IEL))
          X7(I)=X(1,IXS(8,IEL))
          Y7(I)=X(2,IXS(8,IEL))
          Z7(I)=X(3,IXS(8,IEL))
          X8(I)=X(1,IXS(9,IEL))
          Y8(I)=X(2,IXS(9,IEL))
          Z8(I)=X(3,IXS(9,IEL))
          N1=FXBELM(NFT+13*(I-1)+3)
          N2=FXBELM(NFT+13*(I-1)+4)
          N3=FXBELM(NFT+13*(I-1)+5)
          N4=FXBELM(NFT+13*(I-1)+6)
          N5=FXBELM(NFT+13*(I-1)+7)
          N6=FXBELM(NFT+13*(I-1)+8)
          N7=FXBELM(NFT+13*(I-1)+9)
          N8=FXBELM(NFT+13*(I-1)+10)
          D11=FXBMOD(6*(N1-1)+1)
          D12=FXBMOD(6*(N1-1)+2)
          D13=FXBMOD(6*(N1-1)+3)
          D21=FXBMOD(6*(N2-1)+1)
          D22=FXBMOD(6*(N2-1)+2)
          D23=FXBMOD(6*(N2-1)+3)
          D31=FXBMOD(6*(N3-1)+1)
          D32=FXBMOD(6*(N3-1)+2)
          D33=FXBMOD(6*(N3-1)+3)
          D41=FXBMOD(6*(N4-1)+1)
          D42=FXBMOD(6*(N4-1)+2)
          D43=FXBMOD(6*(N4-1)+3)
          D51=FXBMOD(6*(N5-1)+1)
          D52=FXBMOD(6*(N5-1)+2)
          D53=FXBMOD(6*(N5-1)+3)
          D61=FXBMOD(6*(N6-1)+1)
          D62=FXBMOD(6*(N6-1)+2)
          D63=FXBMOD(6*(N6-1)+3)
          D71=FXBMOD(6*(N7-1)+1)
          D72=FXBMOD(6*(N7-1)+2)
          D73=FXBMOD(6*(N7-1)+3)
          D81=FXBMOD(6*(N8-1)+1)
          D82=FXBMOD(6*(N8-1)+2)
          D83=FXBMOD(6*(N8-1)+3)
          VX1(I)=R(1,1)*D11+R(1,2)*D12+R(1,3)*D13
          VY1(I)=R(2,1)*D11+R(2,2)*D12+R(2,3)*D13
          VZ1(I)=R(3,1)*D11+R(3,2)*D12+R(3,3)*D13
          VX2(I)=R(1,1)*D21+R(1,2)*D22+R(1,3)*D23
          VY2(I)=R(2,1)*D21+R(2,2)*D22+R(2,3)*D23
          VZ2(I)=R(3,1)*D21+R(3,2)*D22+R(3,3)*D23
          VX3(I)=R(1,1)*D31+R(1,2)*D32+R(1,3)*D33
          VY3(I)=R(2,1)*D31+R(2,2)*D32+R(2,3)*D33
          VZ3(I)=R(3,1)*D31+R(3,2)*D32+R(3,3)*D33
          VX4(I)=R(1,1)*D41+R(1,2)*D42+R(1,3)*D43
          VY4(I)=R(2,1)*D41+R(2,2)*D42+R(2,3)*D43
          VZ4(I)=R(3,1)*D41+R(3,2)*D42+R(3,3)*D43
          VX5(I)=R(1,1)*D51+R(1,2)*D52+R(1,3)*D53
          VY5(I)=R(2,1)*D51+R(2,2)*D52+R(2,3)*D53
          VZ5(I)=R(3,1)*D51+R(3,2)*D52+R(3,3)*D53
          VX6(I)=R(1,1)*D61+R(1,2)*D62+R(1,3)*D63
          VY6(I)=R(2,1)*D61+R(2,2)*D62+R(2,3)*D63
          VZ6(I)=R(3,1)*D61+R(3,2)*D62+R(3,3)*D63
          VX7(I)=R(1,1)*D71+R(1,2)*D72+R(1,3)*D73
          VY7(I)=R(2,1)*D71+R(2,2)*D72+R(2,3)*D73
          VZ7(I)=R(3,1)*D71+R(3,2)*D72+R(3,3)*D73
          VX8(I)=R(1,1)*D81+R(1,2)*D82+R(1,3)*D83
          VY8(I)=R(2,1)*D81+R(2,2)*D82+R(2,3)*D83
          VZ8(I)=R(3,1)*D81+R(3,2)*D82+R(3,3)*D83
          X1P(I)=X1(I)+VX1(I)
          Y1P(I)=Y1(I)+VY1(I)
          Z1P(I)=Z1(I)+VZ1(I)
          X2P(I)=X2(I)+VX2(I)
          Y2P(I)=Y2(I)+VY2(I)
          Z2P(I)=Z2(I)+VZ2(I)
          X3P(I)=X3(I)+VX3(I)
          Y3P(I)=Y3(I)+VY3(I)
          Z3P(I)=Z3(I)+VZ3(I)
          X4P(I)=X4(I)+VX4(I)
          Y4P(I)=Y4(I)+VY4(I)
          Z4P(I)=Z4(I)+VZ4(I)
          X5P(I)=X5(I)+VX5(I)
          Y5P(I)=Y5(I)+VY5(I)
          Z5P(I)=Z5(I)+VZ5(I)
          X6P(I)=X6(I)+VX6(I)
          Y6P(I)=Y6(I)+VY6(I)
          Z6P(I)=Z6(I)+VZ6(I)
          X7P(I)=X7(I)+VX7(I)
          Y7P(I)=Y7(I)+VY7(I)
          Z7P(I)=Z7(I)+VZ7(I)
          X8P(I)=X8(I)+VX8(I)
          Y8P(I)=Y8(I)+VY8(I)
          Z8P(I)=Z8(I)+VZ8(I)
        ENDDO
C
        CALL SDERII(LAST, VOLP,
     .   X1P, X2P, X3P, X4P, X5P, X6P, X7P, X8P,
     .   Y1P, Y2P, Y3P, Y4P, Y5P, Y6P, Y7P, Y8P,
     .   Z1P, Z2P, Z3P, Z4P, Z5P, Z6P, Z7P, Z8P,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4)
        CALL SDERII(LAST, VOL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4)
        CALL SDEFOI(LAST,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   D1,  D2 , D3,  D4,  D5,  D6 )
        CALL SRHOI(LAST, PM, VOL, RHOP, VOLP, MAT)
        CALL SM1INIF(LAST, PM,  SIG, EINT,
     .               RHOP, D1,  D2,  D3,  D4,
     .               D5,   D6,  MAT)
        CALL SCHREP(LAST, SIG, SIGL, R)
C
        DO I=1,LAST
           IEL=IPARG(3,NG)+FXBELM(NFT+13*(I-1)+2)
           FXBSIG(NFS+7*(I-1)+1)=SIGL(1,I)
           FXBSIG(NFS+7*(I-1)+2)=SIGL(2,I)
           FXBSIG(NFS+7*(I-1)+3)=SIGL(3,I)
           FXBSIG(NFS+7*(I-1)+4)=SIGL(4,I)
           FXBSIG(NFS+7*(I-1)+5)=SIGL(5,I)
           FXBSIG(NFS+7*(I-1)+6)=SIGL(6,I)
           FXBSIG(NFS+7*(I-1)+7)=EINT(I)
        ENDDO
      ENDDO
C 
      RETURN
      END SUBROUTINE FSIGSINI
Chd|====================================================================
Chd|  SDERII                        source/constraints/fxbody/fsigsini.F
Chd|-- called by -----------
Chd|        FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDERII(NEL, DET,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4)
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
      INTEGER :: NEL
      my_real
     .   DET(*), 
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   JAC1(MVSIZ),JAC2(MVSIZ),JAC3(MVSIZ),
     .   JAC4(MVSIZ),JAC5(MVSIZ),JAC6(MVSIZ),
     .   DETT(MVSIZ) ,  JAC7(MVSIZ), JAC8(MVSIZ) , JAC9(MVSIZ),
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ),
     .   JACI4(MVSIZ), JACI5(MVSIZ), JACI6(MVSIZ),
     .   JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ),
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JACI12(MVSIZ), JACI45(MVSIZ), JACI78(MVSIZ),
     .   X_17_46(MVSIZ) , X_28_35(MVSIZ) ,
     .   Y_17_46(MVSIZ) , Y_28_35(MVSIZ) ,
     .   Z_17_46(MVSIZ) , Z_28_35(MVSIZ) 
C
      DO I=1,NEL
         X17(I)=X7(I)-X1(I)
         X28(I)=X8(I)-X2(I)
         X35(I)=X5(I)-X3(I)
         X46(I)=X6(I)-X4(I)
         Y17(I)=Y7(I)-Y1(I)
         Y28(I)=Y8(I)-Y2(I)
         Y35(I)=Y5(I)-Y3(I)
         Y46(I)=Y6(I)-Y4(I)
         Z17(I)=Z7(I)-Z1(I)
         Z28(I)=Z8(I)-Z2(I)
         Z35(I)=Z5(I)-Z3(I)
         Z46(I)=Z6(I)-Z4(I)
      ENDDO
C
      DO I=1,NEL               
         JAC1(I)=X17(I)+X28(I)-X35(I)-X46(I)
         JAC2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
         JAC3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
         X_17_46(I)=X17(I)+X46(I)
         X_28_35(I)=X28(I)+X35(I)
         Y_17_46(I)=Y17(I)+Y46(I)
         Y_28_35(I)=Y28(I)+Y35(I)
         Z_17_46(I)=Z17(I)+Z46(I)
         Z_28_35(I)=Z28(I)+Z35(I)
      ENDDO
C
C Jacobian matrix
      DO I=1,NEL
         JAC4(I)=X_17_46(I)+X_28_35(I)
         JAC5(I)=Y_17_46(I)+Y_28_35(I)
         JAC6(I)=Z_17_46(I)+Z_28_35(I)
         JAC7(I)=X_17_46(I)-X_28_35(I)
         JAC8(I)=Y_17_46(I)-Y_28_35(I)
         JAC9(I)=Z_17_46(I)-Z_28_35(I)
      ENDDO
C                  
      DO I=1,NEL     
         JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
         JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
         JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL   
         DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)
     .         +JAC3(I)*JAC_48_57(I))
      ENDDO
C
      DO I=1,NEL
         DETT(I)=ONE_OVER_64/DET(I)
      ENDDO
C
C Jacobian matrix inverse
      DO I=1,NEL
         JACI1(I)=DETT(I)*JAC_59_68(I)
         JACI4(I)=DETT(I)*JAC_67_49(I)
         JACI7(I)=DETT(I)*JAC_48_57(I)
         JACI2(I)=DETT(I)*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
         JACI5(I)=DETT(I)*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
         JACI8(I)=DETT(I)*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
         JACI3(I)=DETT(I)*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
         JACI6(I)=DETT(I)*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
         JACI9(I)=DETT(I)*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      ENDDO
C
      DO I=1,NEL
         JACI12(I)=JACI1(I)-JACI2(I)
         JACI45(I)=JACI4(I)-JACI5(I)
         JACI78(I)=JACI7(I)-JACI8(I)
      ENDDO
      DO I=1,NEL
         PX3(I)= JACI12(I)+JACI3(I)
         PY3(I)= JACI45(I)+JACI6(I)
         PZ3(I)= JACI78(I)+JACI9(I)
         PX4(I)= JACI12(I)-JACI3(I)
         PY4(I)= JACI45(I)-JACI6(I)
         PZ4(I)= JACI78(I)-JACI9(I)
      ENDDO
      DO I=1,NEL
         JACI12(I)=JACI1(I)+JACI2(I)
         JACI45(I)=JACI4(I)+JACI5(I)
         JACI78(I)=JACI7(I)+JACI8(I)
      ENDDO
      DO I=1,NEL
         PX1(I)=-JACI12(I)-JACI3(I)
         PY1(I)=-JACI45(I)-JACI6(I)
         PZ1(I)=-JACI78(I)-JACI9(I)
         PX2(I)=-JACI12(I)+JACI3(I)
         PY2(I)=-JACI45(I)+JACI6(I)
         PZ2(I)=-JACI78(I)+JACI9(I)
      ENDDO
C
      RETURN
      END SUBROUTINE SDERII
Chd|====================================================================
Chd|  SDEFOI                        source/constraints/fxbody/fsigsini.F
Chd|-- called by -----------
Chd|        FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDEFOI(NEL,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DYY, DZZ, D4 , D5 , D6 )
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
      INTEGER :: NEL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DYY(*), DZZ(*), D4(*) , D5(*), D6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   VX17(MVSIZ), VY17(MVSIZ), VZ17(MVSIZ),
     .   VX28(MVSIZ), VY28(MVSIZ), VZ28(MVSIZ),
     .   VX35(MVSIZ), VY35(MVSIZ), VZ35(MVSIZ),
     .   VX46(MVSIZ), VY46(MVSIZ), VZ46(MVSIZ)
      my_real
     .   DXY(MVSIZ), DXZ(MVSIZ), DYX(MVSIZ), DYZ(MVSIZ), DZX(MVSIZ),
     .   DZY(MVSIZ)
C
      DO I=1,NEL
         VX17(I)=VX1(I)-VX7(I)
         VX28(I)=VX2(I)-VX8(I)
         VX35(I)=VX3(I)-VX5(I)
         VX46(I)=VX4(I)-VX6(I)
         VY17(I)=VY1(I)-VY7(I)
         VY28(I)=VY2(I)-VY8(I)
         VY35(I)=VY3(I)-VY5(I)
         VY46(I)=VY4(I)-VY6(I)
         VZ17(I)=VZ1(I)-VZ7(I)
         VZ28(I)=VZ2(I)-VZ8(I)
         VZ35(I)=VZ3(I)-VZ5(I)
         VZ46(I)=VZ4(I)-VZ6(I)
      ENDDO
C
      DO I=1,NEL
         DXX(I)=PX1(I)*VX17(I)+PX2(I)*VX28(I)+
     .          PX3(I)*VX35(I)+PX4(I)*VX46(I)
         DYY(I)=PY1(I)*VY17(I)+PY2(I)*VY28(I)+
     .          PY3(I)*VY35(I)+PY4(I)*VY46(I)
         DZZ(I)=PZ1(I)*VZ17(I)+PZ2(I)*VZ28(I)+
     .          PZ3(I)*VZ35(I)+PZ4(I)*VZ46(I)
         DXY(I)=PY1(I)*VX17(I)+PY2(I)*VX28(I)+
     .          PY3(I)*VX35(I)+PY4(I)*VX46(I)
         DXZ(I)=PZ1(I)*VX17(I)+PZ2(I)*VX28(I)+
     .          PZ3(I)*VX35(I)+PZ4(I)*VX46(I)
         DYX(I)=PX1(I)*VY17(I)+PX2(I)*VY28(I)+
     .          PX3(I)*VY35(I)+PX4(I)*VY46(I)
         DYZ(I)=PZ1(I)*VY17(I)+PZ2(I)*VY28(I)+
     .          PZ3(I)*VY35(I)+PZ4(I)*VY46(I)
         DZX(I)=PX1(I)*VZ17(I)+PX2(I)*VZ28(I)+
     .          PX3(I)*VZ35(I)+PX4(I)*VZ46(I)
         DZY(I)=PY1(I)*VZ17(I)+PY2(I)*VZ28(I)+
     .          PY3(I)*VZ35(I)+PY4(I)*VZ46(I)
      ENDDO
C
      DO I=1,NEL
         D4(I)   = DXY(I)+DYX(I)
         D5(I)   = DYZ(I)+DZY(I)
         D6(I)   = DXZ(I)+DZX(I)
         DXX(I)  = DXX(I)
         DYY(I)  = DYY(I)
         DZZ(I)  = DZZ(I)
      ENDDO
C
      RETURN
      END SUBROUTINE SDEFOI
Chd|====================================================================
Chd|  SRHOI                         source/constraints/fxbody/fsigsini.F
Chd|-- called by -----------
Chd|        FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SRHOI(NEL, PM, VOLO, RHON, VOLN, MAT)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, MAT(*)
      my_real
     .        PM(NPROPM,*), VOLO(*), RHON(*), VOLN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   RHO0(MVSIZ)
C
      DO I=1,NEL
         RHO0(I)=PM(1,MAT(I))
         RHON(I)=RHO0(I)*(VOLO(I)/VOLN(I))
      ENDDO
C
      RETURN
      END SUBROUTINE SRHOI
Chd|====================================================================
Chd|  SM1INIF                       source/constraints/fxbody/fsigsini.F
Chd|-- called by -----------
Chd|        FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SM1INIF(NEL, PM , SIG , EINT, 
     .                   RHO, D1 , D2 , D3  , D4  , 
     .                   D5  ,D6 , MAT)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL, MAT(*)
      my_real
     .        PM(NPROPM,*), SIG(6,*), EINT(*), RHO(*),D1(*), D2(*),
     .        D3(*), D4(*), D5(*), D6(*)    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
      my_real
     .   RHO0(MVSIZ), G1(MVSIZ), G2(MVSIZ), C1(MVSIZ), AMU(MVSIZ),
     .   DAV, PNEW(MVSIZ), E1, E2, E3, E4, E5, E6, NU1(MVSIZ),
     .   CNU1(MVSIZ)
C
      DO I=1,NEL
         MX     =MAT(I)
         RHO0(I)=PM( 1,MX)
         NU1(I) =PM(21,MX)
         CNU1(I)=NU1(I)/(ONE-TWO*NU1(I))
         G1(I)  =PM(22,MX)
         G2(I)  =TWO*G1(I)
         C1(I)  =PM(32,MX)
         AMU(I) =RHO(I)/RHO0(I)-ONE
      ENDDO       
C
      DO I=1,NEL
         DAV=CNU1(I)*(D1(I)+D2(I)+D3(I))
         SIG(1,I)=G2(I)*(D1(I)+DAV)
         SIG(2,I)=G2(I)*(D2(I)+DAV)
         SIG(3,I)=G2(I)*(D3(I)+DAV)
         SIG(4,I)=G1(I)*D4(I)
         SIG(5,I)=G1(I)*D5(I)
         SIG(6,I)=G1(I)*D6(I)
      ENDDO       
C
      DO I=1,NEL
         E1=D1(I)*SIG(1,I)
         E2=D2(I)*SIG(2,I)
         E3=D3(I)*SIG(3,I)
         E4=D4(I)*SIG(4,I)
         E5=D5(I)*SIG(5,I)
         E6=D6(I)*SIG(6,I)
         EINT(I)=E1+E2+E3+E4+E5+E6
      ENDDO
C
      RETURN
      END SUBROUTINE SM1INIF
Chd|====================================================================
Chd|  SCHREP                        source/constraints/fxbody/fsigsini.F
Chd|-- called by -----------
Chd|        FSIGSINI                      source/constraints/fxbody/fsigsini.F
Chd|        MODBUFEL                      source/constraints/fxbody/modbufel.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCHREP(NEL, SIG, SIGL, R)
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
      INTEGER :: NEL
      my_real
     .        SIG(6,*), SIGL(6,*), R(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      DO I=1,NEL
         SIGL(1,I)=
     .      R(1,1)*(R(1,1)*SIG(1,I)+R(2,1)*SIG(4,I)+R(3,1)*SIG(6,I))
     .     +R(2,1)*(R(1,1)*SIG(4,I)+R(2,1)*SIG(2,I)+R(3,1)*SIG(5,I))
     .     +R(3,1)*(R(1,1)*SIG(6,I)+R(2,1)*SIG(5,I)+R(3,1)*SIG(3,I))
         SIGL(2,I)=
     .      R(1,2)*(R(1,2)*SIG(1,I)+R(2,2)*SIG(4,I)+R(3,2)*SIG(6,I))
     .     +R(2,2)*(R(1,2)*SIG(4,I)+R(2,2)*SIG(2,I)+R(3,2)*SIG(5,I))
     .     +R(3,2)*(R(1,2)*SIG(6,I)+R(2,2)*SIG(5,I)+R(3,2)*SIG(3,I))
         SIGL(3,I)=
     .      R(1,3)*(R(1,3)*SIG(1,I)+R(2,3)*SIG(4,I)+R(3,3)*SIG(6,I))
     .     +R(2,3)*(R(1,3)*SIG(4,I)+R(2,3)*SIG(2,I)+R(3,3)*SIG(5,I))
     .     +R(3,3)*(R(1,3)*SIG(6,I)+R(2,3)*SIG(5,I)+R(3,3)*SIG(3,I))
         SIGL(4,I)=
     .      R(1,1)*(R(1,2)*SIG(1,I)+R(2,2)*SIG(4,I)+R(3,2)*SIG(6,I))
     .     +R(2,1)*(R(1,2)*SIG(4,I)+R(2,2)*SIG(2,I)+R(3,2)*SIG(5,I))
     .     +R(3,1)*(R(1,2)*SIG(6,I)+R(2,2)*SIG(5,I)+R(3,2)*SIG(3,I))
         SIGL(5,I)=
     .      R(1,2)*(R(1,3)*SIG(1,I)+R(2,3)*SIG(4,I)+R(3,3)*SIG(6,I))
     .     +R(2,2)*(R(1,3)*SIG(4,I)+R(2,3)*SIG(2,I)+R(3,3)*SIG(5,I))
     .     +R(3,2)*(R(1,3)*SIG(6,I)+R(2,3)*SIG(5,I)+R(3,3)*SIG(3,I))
         SIGL(6,I)=
     .      R(1,1)*(R(1,3)*SIG(1,I)+R(2,3)*SIG(4,I)+R(3,3)*SIG(6,I))
     .     +R(2,1)*(R(1,3)*SIG(4,I)+R(2,3)*SIG(2,I)+R(3,3)*SIG(5,I))
     .     +R(3,1)*(R(1,3)*SIG(6,I)+R(2,3)*SIG(5,I)+R(3,3)*SIG(3,I))
      ENDDO
C
      RETURN
      END SUBROUTINE SCHREP
         
