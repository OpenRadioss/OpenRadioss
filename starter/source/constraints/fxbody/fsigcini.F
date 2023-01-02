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
Chd|  FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|-- called by -----------
Chd|        FXBSINI                       source/constraints/fxbody/fxbsini.F
Chd|-- calls ---------------
Chd|        CCOEFI                        source/constraints/fxbody/fsigcini.F
Chd|        CCURVI                        source/constraints/fxbody/fsigcini.F
Chd|        CDEFLI                        source/constraints/fxbody/fsigcini.F
Chd|        CEVECI                        source/elements/shell/coque/ceveci.F
Chd|        CM1INIF                       source/elements/shell/coque/cm1inif.F
Chd|        CPXPYI                        source/elements/shell/coque/cepsini.F
Chd|====================================================================
      SUBROUTINE FSIGCINI(FXBELM, IPARG , X     , PM, IXC , 
     .                    GEO   , FXBMOD, FXBSIG, R , NELC) 
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
      INTEGER FXBELM(*), IPARG(NPARG,*), IXC(NIXC,*), NELC
      my_real
     .        FXBSIG(*), X(3,*), PM(NPROPM), FXBMOD(*),
     .        GEO(NPROPG,*), R(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG,OFFSET,FIRST,LAST,NFT,I,NG,IEL,
     .        N1,N2,N3,N4,ISM,ITHK,NPT,NFS
      INTEGER MAT(MVSIZ), PROP(MVSIZ)
      INTEGER II,J
      my_real
     .   D11, D12, D13, D21, D22, D23, D31, D32, D33, D41, D42, D43,
     .   DR11, DR12, DR13, DR21, DR22, DR23, DR31, DR32, DR33,
     .   DR41, DR42, DR43
      my_real VL(3,4,MVSIZ), VRL(3,4,MVSIZ),
     .   PX1G(MVSIZ), PX2G(MVSIZ), PY1G(MVSIZ), PY2G(MVSIZ),
     .   PX1(MVSIZ) , PX2(MVSIZ) , PY1(MVSIZ) , PY2(MVSIZ),
     .   X2S(MVSIZ), Y2S(MVSIZ), X3S(MVSIZ),
     .   Y3S(MVSIZ), X4S(MVSIZ), Y4S(MVSIZ),
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) , X4(MVSIZ) ,
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) , Y4(MVSIZ) ,
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ) , Z4(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),
     .   GSTRBID(8,MVSIZ), FOR(5,MVSIZ), MOM(3,MVSIZ),
     .   EINT(2,MVSIZ)   , THK(MVSIZ), AREA(MVSIZ),
     .   NU(MVSIZ), G(MVSIZ), A1(MVSIZ), A2(MVSIZ), GS(MVSIZ)
C=======================================================================
       FIRST=1
       DO IG=1,NELC,MVSIZ
        OFFSET=IG-1
        LAST=MIN(MVSIZ,NELC-OFFSET)
        NFT=OFFSET*10
        NFS=OFFSET*10
        DO I=1,LAST
          NG=FXBELM(NFT+10*(I-1)+1)
          IEL=IPARG(3,NG)+FXBELM(NFT+10*(I-1)+2)
          MAT(I)=IXC(1,IEL)
          PROP(I)=IXC(6,IEL)
          THK(I)=GEO(1,PROP(I))
          X1(I)=0.
          Y1(I)=0.
          Z1(I)=0.
          X2(I)=X(1,IXC(3,IEL))-X(1,IXC(2,IEL))
          Y2(I)=X(2,IXC(3,IEL))-X(2,IXC(2,IEL))
          Z2(I)=X(3,IXC(3,IEL))-X(3,IXC(2,IEL))
          X3(I)=X(1,IXC(4,IEL))-X(1,IXC(2,IEL))
          Y3(I)=X(2,IXC(4,IEL))-X(2,IXC(2,IEL))
          Z3(I)=X(3,IXC(4,IEL))-X(3,IXC(2,IEL))
          X4(I)=X(1,IXC(5,IEL))-X(1,IXC(2,IEL))
          Y4(I)=X(2,IXC(5,IEL))-X(2,IXC(2,IEL))
          Z4(I)=X(3,IXC(5,IEL))-X(3,IXC(2,IEL))
          N1=FXBELM(NFT+10*(I-1)+3)
          N2=FXBELM(NFT+10*(I-1)+4)
          N3=FXBELM(NFT+10*(I-1)+5)
          N4=FXBELM(NFT+10*(I-1)+6)
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
          VL(1,1,I)=R(1,1)*D11+R(1,2)*D12+R(1,3)*D13
          VL(2,1,I)=R(2,1)*D11+R(2,2)*D12+R(2,3)*D13
          VL(3,1,I)=R(3,1)*D11+R(3,2)*D12+R(3,3)*D13
          VL(1,2,I)=R(1,1)*D21+R(1,2)*D22+R(1,3)*D23
          VL(2,2,I)=R(2,1)*D21+R(2,2)*D22+R(2,3)*D23
          VL(3,2,I)=R(3,1)*D21+R(3,2)*D22+R(3,3)*D23
          VL(1,3,I)=R(1,1)*D31+R(1,2)*D32+R(1,3)*D33
          VL(2,3,I)=R(2,1)*D31+R(2,2)*D32+R(2,3)*D33
          VL(3,3,I)=R(3,1)*D31+R(3,2)*D32+R(3,3)*D33
          VL(1,4,I)=R(1,1)*D41+R(1,2)*D42+R(1,3)*D43
          VL(2,4,I)=R(2,1)*D41+R(2,2)*D42+R(2,3)*D43
          VL(3,4,I)=R(3,1)*D41+R(3,2)*D42+R(3,3)*D43
          DR11=FXBMOD(6*(N1-1)+4)
          DR12=FXBMOD(6*(N1-1)+5)
          DR13=FXBMOD(6*(N1-1)+6)
          DR21=FXBMOD(6*(N2-1)+4)
          DR22=FXBMOD(6*(N2-1)+5)
          DR23=FXBMOD(6*(N2-1)+6)
          DR31=FXBMOD(6*(N3-1)+4)
          DR32=FXBMOD(6*(N3-1)+5)
          DR33=FXBMOD(6*(N3-1)+6)
          DR41=FXBMOD(6*(N4-1)+4)
          DR42=FXBMOD(6*(N4-1)+5)
          DR43=FXBMOD(6*(N4-1)+6)
          VRL(1,1,I)=R(1,1)*DR11+R(1,2)*DR12+R(1,3)*DR13
          VRL(2,1,I)=R(2,1)*DR11+R(2,2)*DR12+R(2,3)*DR13
          VRL(3,1,I)=R(3,1)*DR11+R(3,2)*DR12+R(3,3)*DR13
          VRL(1,2,I)=R(1,1)*DR21+R(1,2)*DR22+R(1,3)*DR23
          VRL(2,2,I)=R(2,1)*DR21+R(2,2)*DR22+R(2,3)*DR23
          VRL(3,2,I)=R(3,1)*DR21+R(3,2)*DR22+R(3,3)*DR23
          VRL(1,3,I)=R(1,1)*DR31+R(1,2)*DR32+R(1,3)*DR33
          VRL(2,3,I)=R(2,1)*DR31+R(2,2)*DR32+R(2,3)*DR33
          VRL(3,3,I)=R(3,1)*DR31+R(3,2)*DR32+R(3,3)*DR33
          VRL(1,4,I)=R(1,1)*DR41+R(1,2)*DR42+R(1,3)*DR43
          VRL(2,4,I)=R(2,1)*DR41+R(2,2)*DR42+R(2,3)*DR43
          VRL(3,4,I)=R(3,1)*DR41+R(3,2)*DR42+R(3,3)*DR43
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
          PX2G(I)=ZERO
          PY1G(I)=ZERO
          PY2G(I)=ZERO
          ISM=1
          ITHK=0
        ENDDO
C
        CALL CEVECI(FIRST, LAST , AREA,
     .              X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .              Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .              E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
        NPT=1
        CALL CCOEFI(LAST, PM  ,  GEO,  NU  ,
     .              G   , A1  ,  A2 ,  GS  , THK,
     .              MAT , PROP,  NPT,  AREA)
        CALL CPXPYI(FIRST,LAST,  ISM,
     .              PX1G ,PX2G ,PY1G ,PY2G ,AREA  ,
     .              PX1  ,PX2  ,PY1  ,PY2  ,
     .              X1   ,X2   ,X3   ,X4   ,Y1   ,Y2  ,
     .              Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4  ,
     .              E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .              X2S  ,Y2S  ,X3S  ,Y3S  ,X4S  ,Y4S  )
        CALL CDEFLI(LAST ,VL   ,GSTRBID, 
     .              PX1  ,PX2  ,PY1  ,PY2  ,
     .              E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .              EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,AREA )
        CALL CCURVI(LAST ,VRL  ,
     .              PX1  ,PX2  ,PY1  ,PY2  ,
     .              E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .              EYZ  ,EZX  ,KXX  ,KYY  ,KXY  ,AREA )
        ITHK=0
        CALL CM1INIF(FIRST,LAST ,FOR  ,MOM  ,ITHK , 
     .               THK  ,EINT ,NU   ,G    ,A1   , 
     .               A2   ,GS   ,KXX  ,KYY  ,KXY  ,
     .               EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,
     .               AREA )
C
        DO I=1,LAST
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
C-----------
      RETURN
      END SUBROUTINE FSIGCINI
Chd|====================================================================
Chd|  CCOEFI                        source/constraints/fxbody/fsigcini.F
Chd|-- called by -----------
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCOEFI(NEL , PM  , GEO, NU ,
     .                  G   , A1  , A2 , GS , THK,
     .                  MAT , PROP, NPT, AREA )
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
      INTEGER :: NEL, MAT(*), PROP(*), NPT
      my_real AREA(MVSIZ),
     .        PM(NPROPM,*), GEO(NPROPG,*), NU(*), G(*), A1(*),
     .        A2(*), GS(*), THK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IM,IP,ISH
      my_real THK02,FAC1,FSH,SHF  
C=======================================================================
      DO I=1,NEL
         IM=MAT(I)
         IP=PROP(I)
         NU(I)=PM(21,IM)
         G(I)=PM(22,IM)
         A1(I)=PM(24,IM)
         A2(I)=PM(25,IM)
         IF (NPT > 1) THEN
            THK02=THK(I)*THK(I)
            FAC1=TWO*(ONE+NU(I))*THK02
            ISH=NINT(GEO(37,IP))
            FSH=GEO(38,IP)
            SHF=FSH*(ONE-ISH+ISH*FAC1/(FSH*AREA(I)+FAC1))
         ELSE
            SHF=ZERO
         ENDIF
         GS(I)=G(I)*SHF
      ENDDO
C
      RETURN
      END SUBROUTINE CCOEFI
Chd|====================================================================
Chd|  CDEFLI                        source/constraints/fxbody/fsigcini.F
Chd|-- called by -----------
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDEFLI (NEL  ,VL   ,GSTR ,
     .                   PX1  ,PX2  ,PY1  ,PY2  ,
     .                   E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                   EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,AREA )
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
      INTEGER :: NEL
      my_real VL(3,4,*),GSTR(8,*),PX1(*) ,PX2(*) ,PY1(*) ,PY2(*)
      my_real EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EZX(MVSIZ),EYZ(MVSIZ),
     .    E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .    E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) ,
     .    E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,AREA(MVSIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real FAC1
      my_real
     .    VX1(MVSIZ) , VX2(MVSIZ) , VX3(MVSIZ) , VX4(MVSIZ) ,
     .    VY1(MVSIZ) , VY2(MVSIZ) , VY3(MVSIZ) , VY4(MVSIZ) ,
     .    VZ1(MVSIZ) , VZ2(MVSIZ) , VZ3(MVSIZ) , VZ4(MVSIZ) ,
     .    VX13(MVSIZ), VX24(MVSIZ), VY13(MVSIZ), VY24(MVSIZ),
     .    VZ13(MVSIZ), VZ24(MVSIZ)
C=======================================================================
      DO I=1,NEL
        VX1(I)=E1X(I)*VL(1,1,I)+E1Y(I)*VL(2,1,I)+E1Z(I)*VL(3,1,I)
        VX2(I)=E1X(I)*VL(1,2,I)+E1Y(I)*VL(2,2,I)+E1Z(I)*VL(3,2,I)
        VX3(I)=E1X(I)*VL(1,3,I)+E1Y(I)*VL(2,3,I)+E1Z(I)*VL(3,3,I)
        VX4(I)=E1X(I)*VL(1,4,I)+E1Y(I)*VL(2,4,I)+E1Z(I)*VL(3,4,I)
C
        VY4(I)=E2X(I)*VL(1,4,I)+E2Y(I)*VL(2,4,I)+E2Z(I)*VL(3,4,I)
        VY3(I)=E2X(I)*VL(1,3,I)+E2Y(I)*VL(2,3,I)+E2Z(I)*VL(3,3,I)
        VY2(I)=E2X(I)*VL(1,2,I)+E2Y(I)*VL(2,2,I)+E2Z(I)*VL(3,2,I)
        VY1(I)=E2X(I)*VL(1,1,I)+E2Y(I)*VL(2,1,I)+E2Z(I)*VL(3,1,I)
C
        VZ1(I)=E3X(I)*VL(1,1,I)+E3Y(I)*VL(2,1,I)+E3Z(I)*VL(3,1,I)
        VZ2(I)=E3X(I)*VL(1,2,I)+E3Y(I)*VL(2,2,I)+E3Z(I)*VL(3,2,I)
        VZ3(I)=E3X(I)*VL(1,3,I)+E3Y(I)*VL(2,3,I)+E3Z(I)*VL(3,3,I)
        VZ4(I)=E3X(I)*VL(1,4,I)+E3Y(I)*VL(2,4,I)+E3Z(I)*VL(3,4,I)
      ENDDO
C
      DO I=1,NEL
        VZ13(I)=VZ1(I)-VZ3(I)
        VZ24(I)=VZ2(I)-VZ4(I)
C
        VX13(I)=VX1(I)-VX3(I)
        VX24(I)=VX2(I)-VX4(I)
C
        EXX(I)=PX1(I)*VX13(I)+PX2(I)*VX24(I)
        EXY(I)=PY1(I)*VX13(I)+PY2(I)*VX24(I)
C
        VY13(I)=VY1(I)-VY3(I)
        VY24(I)=VY2(I)-VY4(I)
C
        EXY(I)=EXY(I)+PX1(I)*VY13(I)+PX2(I)*VY24(I)
        EYY(I)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
        EYZ(I)=PY1(I)*VZ13(I)+PY2(I)*VZ24(I)
        EZX(I)=PX1(I)*VZ13(I)+PX2(I)*VZ24(I)
      ENDDO
C
      DO I=1,NEL
        FAC1  = ONE/AREA(I)
        EXX(I)=EXX(I)*FAC1
        EYY(I)=EYY(I)*FAC1
        EXY(I)=EXY(I)*FAC1
      ENDDO
C
      DO I=1,NEL
        GSTR(1,I)=GSTR(1,I)+EXX(I)
        GSTR(2,I)=GSTR(2,I)+EYY(I)
        GSTR(3,I)=GSTR(3,I)+EXY(I)
      ENDDO
      RETURN
      END SUBROUTINE CDEFLI
Chd|====================================================================
Chd|  CCURVI                        source/constraints/fxbody/fsigcini.F
Chd|-- called by -----------
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCURVI(NEL  ,VRL  ,
     .                  PX1  ,PX2  ,PY1  ,PY2  ,
     .                  E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                  EYZ  ,EZX  ,KXX  ,KYY  ,KXY  ,AREA )
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
      INTEGER :: NEL
      my_real VRL(3,4,*),PX1(*),PX2(*),PY1(*),PY2(*)
      my_real E1X(MVSIZ),  E1Y(MVSIZ),  E1Z(MVSIZ),  E2X(MVSIZ),
     .        E2Y(MVSIZ),  E2Z(MVSIZ),  E3X(MVSIZ),  E3Y(MVSIZ),
     .        E3Z(MVSIZ),  KXX(MVSIZ),  KYY(MVSIZ),  KXY(MVSIZ),
     .        AREA(MVSIZ), EZX(MVSIZ),  EYZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     RX1(MVSIZ),  RX2(MVSIZ),  RX3(MVSIZ),  RX4(MVSIZ),
     .     RY1(MVSIZ),  RY2(MVSIZ),  RY3(MVSIZ),  RY4(MVSIZ)
      my_real
     .     RXAVTA,RX13TA,RX24TA,RY13TA,RYAVTA,RY24TA,FAC1
C=======================================================================
      DO I=1,NEL
        RX1(I)=E1X(I)*VRL(1,1,I)+E1Y(I)*VRL(2,1,I)+E1Z(I)*VRL(3,1,I)
        RX2(I)=E1X(I)*VRL(1,2,I)+E1Y(I)*VRL(2,2,I)+E1Z(I)*VRL(3,2,I)
        RX3(I)=E1X(I)*VRL(1,3,I)+E1Y(I)*VRL(2,3,I)+E1Z(I)*VRL(3,3,I)
        RX4(I)=E1X(I)*VRL(1,4,I)+E1Y(I)*VRL(2,4,I)+E1Z(I)*VRL(3,4,I)
        RY1(I)=E2X(I)*VRL(1,1,I)+E2Y(I)*VRL(2,1,I)+E2Z(I)*VRL(3,1,I)
        RY2(I)=E2X(I)*VRL(1,2,I)+E2Y(I)*VRL(2,2,I)+E2Z(I)*VRL(3,2,I)
        RY3(I)=E2X(I)*VRL(1,3,I)+E2Y(I)*VRL(2,3,I)+E2Z(I)*VRL(3,3,I)
        RY4(I)=E2X(I)*VRL(1,4,I)+E2Y(I)*VRL(2,4,I)+E2Z(I)*VRL(3,4,I)
      ENDDO
C
      DO I=1,NEL
        RX13TA =RX1(I)-RX3(I)
        RXAVTA =RX1(I)+RX2(I)+RX3(I)+RX4(I)
        RX24TA =RX2(I)-RX4(I)
C
        KYY(I)=-PY1(I)*RX13TA-PY2(I)*RX24TA
        KXY(I)= PX1(I)*RX13TA+PX2(I)*RX24TA
C
        RY13TA = RY1(I)-RY3(I)
        RYAVTA = RY1(I)+RY2(I)+RY3(I)+RY4(I)
        RY24TA = RY2(I)-RY4(I)
C
        KXX(I)= PX1(I)*RY13TA+PX2(I)*RY24TA
        KXY(I)= PY1(I)*RY13TA+PY2(I)*RY24TA
     +       -KXY(I)
C
        EZX(I)=EZX(I)+RYAVTA*(FOURTH*AREA(I))
        EYZ(I)=EYZ(I)-RXAVTA*(FOURTH*AREA(I))
      ENDDO
C
      DO I=1,NEL
        FAC1  = ONE/AREA(I)
        EZX(I)=EZX(I)*FAC1
        EYZ(I)=EYZ(I)*FAC1
        KXX(I)=KXX(I)*FAC1
        KYY(I)=KYY(I)*FAC1
        KXY(I)=KXY(I)*FAC1
      ENDDO
C
      RETURN
      END SUBROUTINE CCURVI
     
