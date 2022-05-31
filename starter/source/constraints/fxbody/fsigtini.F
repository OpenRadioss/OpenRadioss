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
Chd|  FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|-- called by -----------
Chd|        FXBSINI                       source/constraints/fxbody/fxbsini.F
Chd|-- calls ---------------
Chd|        C3CURVI                       source/constraints/fxbody/fsigtini.F
Chd|        C3DEFLI                       source/constraints/fxbody/fsigtini.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3PXPYI                       source/elements/sh3n/coque3n/c3epsini.F
Chd|        CCOEFI                        source/constraints/fxbody/fsigcini.F
Chd|        CM1INIF                       source/elements/shell/coque/cm1inif.F
Chd|====================================================================
      SUBROUTINE FSIGTINI(FXBELM, IPARG , X     , PM, IXTG , 
     .                    GEO   , FXBMOD, FXBSIG, R , NELTG)
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
      INTEGER FXBELM(*), IPARG(NPARG,*), IXTG(NIXTG,*), NELTG
      my_real
     .        FXBSIG(*), X(3,*), PM(NPROPM), FXBMOD(*),
     .        GEO(NPROPG,*), R(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG,OFFSET,LFT,LLT,NFT,I,NG,IEL,
     .        N1,N2,N3,ISM,ITHK,NPT,NFS
      INTEGER MAT(MVSIZ), PROP(MVSIZ)
      INTEGER II,J
      my_real
     .   VL(3,3,MVSIZ), VRL(3,3,MVSIZ)
      my_real
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) , 
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) , 
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ) ,
     .   X31(MVSIZ) , Y31(MVSIZ) , Z31(MVSIZ) 
      my_real
     .   D11, D12, D13, D21, D22, D23, D31, D32, D33, 
     .   DR11, DR12, DR13, DR21, DR22, DR23, DR31, DR32, DR33
      my_real
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), 
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),
     .   PX1G(MVSIZ), PY1G(MVSIZ), PY2G(MVSIZ),
     .   PX1(MVSIZ) , PY1(MVSIZ) , PY2(MVSIZ),
     .   X2S(MVSIZ), X3S(MVSIZ), Y3S(MVSIZ)
      my_real
     .   GSTRBID(8,MVSIZ), FOR(5,MVSIZ), MOM(3,MVSIZ),
     .   EINT(2,MVSIZ)   , THK(MVSIZ), AREA(MVSIZ),
     .   NU(MVSIZ), G(MVSIZ), A1(MVSIZ), A2(MVSIZ), GS(MVSIZ)
c=======================================================================
      DO IG=1,NELTG,MVSIZ
        OFFSET=IG-1
        LFT=1
        LLT=MIN(MVSIZ,NELTG-OFFSET)
        NFT=OFFSET*9
        NFS=OFFSET*10
        DO I=LFT,LLT
          NG=FXBELM(NFT+9*(I-1)+1)
          IEL=IPARG(3,NG)+FXBELM(NFT+9*(I-1)+2)
          MAT(I)=IXTG(1,IEL)
          PROP(I)=IXTG(5,IEL)
          THK(I)=GEO(1,PROP(I))
          X1(I)=ZERO
          Y1(I)=ZERO
          Z1(I)=ZERO
          X2(I)=X(1,IXTG(3,IEL))-X(1,IXTG(2,IEL))
          Y2(I)=X(2,IXTG(3,IEL))-X(2,IXTG(2,IEL))
          Z2(I)=X(3,IXTG(3,IEL))-X(3,IXTG(2,IEL))
          X3(I)=X(1,IXTG(4,IEL))-X(1,IXTG(2,IEL))
          Y3(I)=X(2,IXTG(4,IEL))-X(2,IXTG(2,IEL))
          Z3(I)=X(3,IXTG(4,IEL))-X(3,IXTG(2,IEL))
          N1=FXBELM(NFT+9*(I-1)+3)
          N2=FXBELM(NFT+9*(I-1)+4)
          N3=FXBELM(NFT+9*(I-1)+5)
          D11=FXBMOD(6*(N1-1)+1)
          D12=FXBMOD(6*(N1-1)+2)
          D13=FXBMOD(6*(N1-1)+3)
          D21=FXBMOD(6*(N2-1)+1)
          D22=FXBMOD(6*(N2-1)+2)
          D23=FXBMOD(6*(N2-1)+3)
          D31=FXBMOD(6*(N3-1)+1)
          D32=FXBMOD(6*(N3-1)+2)
          D33=FXBMOD(6*(N3-1)+3)
          VL(1,1,I)=R(1,1)*D11+R(1,2)*D12+R(1,3)*D13
          VL(2,1,I)=R(2,1)*D11+R(2,2)*D12+R(2,3)*D13
          VL(3,1,I)=R(3,1)*D11+R(3,2)*D12+R(3,3)*D13
          VL(1,2,I)=R(1,1)*D21+R(1,2)*D22+R(1,3)*D23
          VL(2,2,I)=R(2,1)*D21+R(2,2)*D22+R(2,3)*D23
          VL(3,2,I)=R(3,1)*D21+R(3,2)*D22+R(3,3)*D23
          VL(1,3,I)=R(1,1)*D31+R(1,2)*D32+R(1,3)*D33
          VL(2,3,I)=R(2,1)*D31+R(2,2)*D32+R(2,3)*D33
          VL(3,3,I)=R(3,1)*D31+R(3,2)*D32+R(3,3)*D33
          DR11=FXBMOD(6*(N1-1)+4)
          DR12=FXBMOD(6*(N1-1)+5)
          DR13=FXBMOD(6*(N1-1)+6)
          DR21=FXBMOD(6*(N2-1)+4)
          DR22=FXBMOD(6*(N2-1)+5)
          DR23=FXBMOD(6*(N2-1)+6)
          DR31=FXBMOD(6*(N3-1)+4)
          DR32=FXBMOD(6*(N3-1)+5)
          DR33=FXBMOD(6*(N3-1)+6)
          VRL(1,1,I)=R(1,1)*DR11+R(1,2)*DR12+R(1,3)*DR13
          VRL(2,1,I)=R(2,1)*DR11+R(2,2)*DR12+R(2,3)*DR13
          VRL(3,1,I)=R(3,1)*DR11+R(3,2)*DR12+R(3,3)*DR13
          VRL(1,2,I)=R(1,1)*DR21+R(1,2)*DR22+R(1,3)*DR23
          VRL(2,2,I)=R(2,1)*DR21+R(2,2)*DR22+R(2,3)*DR23
          VRL(3,2,I)=R(3,1)*DR21+R(3,2)*DR22+R(3,3)*DR23
          VRL(1,3,I)=R(1,1)*DR31+R(1,2)*DR32+R(1,3)*DR33
          VRL(2,3,I)=R(2,1)*DR31+R(2,2)*DR32+R(2,3)*DR33
          VRL(3,3,I)=R(3,1)*DR31+R(3,2)*DR32+R(3,3)*DR33
          GSTRBID(1,I)=ZERO
          GSTRBID(2,I)=ZERO
          GSTRBID(3,I)=ZERO
          FOR(1,I)=ZERO
          FOR(2,I)=ZERO
          FOR(3,I)=ZERO
          FOR(4,I)=ZERO
          FOR(5,I)=ZERO
          MOM(1,I)=ZERO
          MOM(2,I)=ZERO
          MOM(3,I)=ZERO
          PX1G(I)=ZERO
          PY1G(I)=ZERO
          PY2G(I)=ZERO
          ISM=1
          ITHK=0
        ENDDO
C
        CALL C3EVEC3(LFT ,LLT ,AREA,
     .               X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .               Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .               E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .               X31 ,Y31 ,Z31 ,X2S ,X3S ,Y3S )
        NPT=1
        CALL CCOEFI(LFT, LLT,  PM,  GEO, NU,
     .              G,   A1,   A2,  GS , THK,
     .              MAT, PROP, NPT, AREA )
        CALL C3PXPYI(LFT  ,LLT  ,ISM ,
     .               PX1G ,PY1G ,PY2G ,
     .               PX1  ,PY1  ,PY2  ,
     .               X2S  ,X3S  ,Y3S  ,
     .               X1   ,X2   ,X3   ,Y1   ,Y2   ,Y3   ,
     .               Z1   ,Z2   ,Z3   ,E1X  ,E2X  ,E3X  ,
     .               E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
        CALL C3DEFLI(LFT  ,LLT  ,VL   ,GSTRBID ,
     .               PX1  ,PY1  ,PY2  ,
     .               E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .               EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,AREA )
        CALL C3CURVI(LFT  ,LLT  ,VRL  ,
     .               PX1  ,PY1  ,PY2  ,
     .               E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .               EYZ  ,EZX  ,KXX  ,KYY  ,KXY  ,AREA )
        ITHK=0
        CALL CM1INIF(LFT  ,LLT  ,FOR  ,MOM  ,ITHK , 
     .               THK  ,EINT ,NU   ,G    ,A1   , 
     .               A2   ,GS   ,KXX  ,KYY  ,KXY  ,
     .               EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,
     .               AREA )
C
        DO I=LFT,LLT
           FXBSIG(NFS+10*(I-1)+1)=FOR(1,I)
           FXBSIG(NFS+10*(I-1)+2)=FOR(2,I)
           FXBSIG(NFS+10*(I-1)+3)=FOR(3,I)
           FXBSIG(NFS+10*(I-1)+4)=FOR(4,I)
           FXBSIG(NFS+10*(I-1)+5)=FOR(5,I)
           FXBSIG(NFS+10*(I-1)+6)=MOM(1,I)
           FXBSIG(NFS+10*(I-1)+7)=MOM(2,I)
           FXBSIG(NFS+10*(I-1)+8)=MOM(3,I)
           FXBSIG(NFS+10*(I-1)+9)=EINT(1,I)
           FXBSIG(NFS+10*(I-1)+10)=EINT(2,I)
        ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C3DEFLI                       source/constraints/fxbody/fsigtini.F
Chd|-- called by -----------
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3DEFLI (JFT  ,JLT  ,VL   ,GSTR ,
     .                    PX1  ,PY1  ,PY2  ,
     .                    E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                    EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,AREA )
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
      INTEGER JFT, JLT
C     REAL ou REAL*8
      my_real VL(3,3,*),GSTR(8,*),PX1(*),PY1(*),PY2(*)
      my_real EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EZX(MVSIZ),EYZ(MVSIZ),
     .    E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .    E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) ,
     .    E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,AREA(MVSIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .    VX1(MVSIZ) , VX2(MVSIZ) , VX3(MVSIZ) ,
     .    VY1(MVSIZ) , VY2(MVSIZ) , VY3(MVSIZ) ,
     .    VZ1(MVSIZ) , VZ2(MVSIZ) , VZ3(MVSIZ) ,
     .    VX12(MVSIZ), VX13(MVSIZ), VX23(MVSIZ),
     .    VY12(MVSIZ), VY13(MVSIZ), VY23(MVSIZ),
     .    VZ12(MVSIZ), VZ13(MVSIZ), VZ23(MVSIZ)
      my_real FAC1
c=======================================================================
      DO I=JFT,JLT
        VX1(I)=E1X(I)*VL(1,1,I)+E1Y(I)*VL(2,1,I)+E1Z(I)*VL(3,1,I)
        VX2(I)=E1X(I)*VL(1,2,I)+E1Y(I)*VL(2,2,I)+E1Z(I)*VL(3,2,I)
        VX3(I)=E1X(I)*VL(1,3,I)+E1Y(I)*VL(2,3,I)+E1Z(I)*VL(3,3,I)
C
        VY3(I)=E2X(I)*VL(1,3,I)+E2Y(I)*VL(2,3,I)+E2Z(I)*VL(3,3,I)
        VY2(I)=E2X(I)*VL(1,2,I)+E2Y(I)*VL(2,2,I)+E2Z(I)*VL(3,2,I)
        VY1(I)=E2X(I)*VL(1,1,I)+E2Y(I)*VL(2,1,I)+E2Z(I)*VL(3,1,I)
C
        VZ1(I)=E3X(I)*VL(1,1,I)+E3Y(I)*VL(2,1,I)+E3Z(I)*VL(3,1,I)
        VZ2(I)=E3X(I)*VL(1,2,I)+E3Y(I)*VL(2,2,I)+E3Z(I)*VL(3,2,I)
        VZ3(I)=E3X(I)*VL(1,3,I)+E3Y(I)*VL(2,3,I)+E3Z(I)*VL(3,3,I)
      ENDDO
C
      DO I=JFT,JLT
        VZ12(I)=VZ1(I) - VZ2(I)
        VZ13(I)=VZ1(I) - VZ3(I)
        VZ23(I)=VZ2(I) - VZ3(I)
C
        VX12(I)=VX1(I) - VX2(I)
        VY12(I)=VY1(I) - VY2(I)
        VX13(I)=VX1(I) - VX3(I)
        VY13(I)=VY1(I) - VY3(I)
        VX23(I)=VX2(I) - VX3(I)
        VY23(I)=VY2(I) - VY3(I)
C
        EXX(I)=PX1(I)*VX12(I)
        EYY(I)=PY1(I)*VY13(I) + PY2(I)*VY23(I)
C
        EXY(I)=PY1(I)*VX13(I) + PY2(I)*VX23(I) + PX1(I)*VY12(I)
        EYZ(I)=PY1(I)*VZ13(I) + PY2(I)*VZ23(I)
        EZX(I)=PX1(I)*VZ12(I)
      ENDDO
C
      DO I=JFT,JLT
        FAC1  = ONE/AREA(I)
        EXX(I)=EXX(I)*FAC1
        EYY(I)=EYY(I)*FAC1
        EXY(I)=EXY(I)*FAC1
      ENDDO
C
      DO I=JFT,JLT
        GSTR(1,I)=GSTR(1,I)+EXX(I)
        GSTR(2,I)=GSTR(2,I)+EYY(I)
        GSTR(3,I)=GSTR(3,I)+EXY(I)
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  C3CURVI                       source/constraints/fxbody/fsigtini.F
Chd|-- called by -----------
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3CURVI(JFT  ,JLT  ,VRL  ,
     .                   PX1  ,PY1  ,PY2  ,
     .                   E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                   EYZ  ,EZX  ,KXX  ,KYY  ,KXY  ,AREA )
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
      INTEGER JFT,JLT
      my_real VRL(3,3,*),PX1(*),PY1(*),PY2(*),
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        KXX(MVSIZ),  KYY(MVSIZ),  KXY(MVSIZ),
     .        EYZ(MVSIZ),EZX(MVSIZ),  AREA(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     RX1(MVSIZ),  RX2(MVSIZ),  RX3(MVSIZ),
     .     RY1(MVSIZ),  RY2(MVSIZ),  RY3(MVSIZ)
      my_real
     .     RX12T,RX13T,RX23T,RY12T,RY13T,RY23T,RYAVT,RXAVT,FAC1
c=======================================================================
      DO I=JFT,JLT
        RX1(I)=E1X(I)*VRL(1,1,I)+E1Y(I)*VRL(2,1,I)+E1Z(I)*VRL(3,1,I)
        RX2(I)=E1X(I)*VRL(1,2,I)+E1Y(I)*VRL(2,2,I)+E1Z(I)*VRL(3,2,I)
        RX3(I)=E1X(I)*VRL(1,3,I)+E1Y(I)*VRL(2,3,I)+E1Z(I)*VRL(3,3,I)
        RY1(I)=E2X(I)*VRL(1,1,I)+E2Y(I)*VRL(2,1,I)+E2Z(I)*VRL(3,1,I)
        RY2(I)=E2X(I)*VRL(1,2,I)+E2Y(I)*VRL(2,2,I)+E2Z(I)*VRL(3,2,I)
        RY3(I)=E2X(I)*VRL(1,3,I)+E2Y(I)*VRL(2,3,I)+E2Z(I)*VRL(3,3,I)
      ENDDO
C
      DO I=JFT,JLT
        RX12T  = RX1(I) - RX2(I)
        RX13T  = RX1(I) - RX3(I)
        RX23T  = RX2(I) - RX3(I)
C
        KYY(I)=-PY1(I) * RX13T - PY2(I) * RX23T
        KXY(I)= PX1(I) * RX12T
C
        RY12T = RY1(I) - RY2(I)
        RY13T = RY1(I) - RY3(I)
        RY23T = RY2(I) - RY3(I)
C
        KXX(I) = PX1(I) * RY12T
        KXY(I) = PY1(I) * RY13T + PY2(I) * RY23T - KXY(I)
C
        RYAVT  = PX1(I) * (
     .              PX1(I) * (-RX1(I)+RX2(I)) 
     .            + (TWO*PY1(I)+THREE*PY2(I)) * RY1(I)
     .            + (THREE*PY1(I)+TWO*PY2(I)) * RY2(I)
     .            + ( PY1(I) + PY2(I) )   * RY3(I) )
        RXAVT  = -PX1(I) * (   
     .            + (TWO*PY1(I)+PY2(I)) * RX1(I)
     .            + (PY1(I)+TWO*PY2(I)) * RX2(I)
     .            + THREE*(PY1(I)+PY2(I)) * RX3(I) )
     .           + PY1(I) * (PY1(I)+THREE*PY2(I)) * RY1(I)
     .           - PY2(I) * (THREE*PY1(I)+PY2(I)) * RY2(I)
     .           + ( PY2(I)**2 - PY1(I)**2 )   * RY3(I)
C
        EZX(I) = EZX(I) + RYAVT / THREE
        EYZ(I) = EYZ(I) + RXAVT / THREE
      ENDDO      
C
      DO I=JFT,JLT
        FAC1  = ONE/AREA(I)
        EZX(I)=EZX(I)*FAC1
        EYZ(I)=EYZ(I)*FAC1
        KXX(I)=KXX(I)*FAC1
        KYY(I)=KYY(I)*FAC1
        KXY(I)=KXY(I)*FAC1
      ENDDO
C
      RETURN
      END
      
