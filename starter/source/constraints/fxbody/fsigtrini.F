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
Chd|  FSIGTRINI                     source/constraints/fxbody/fsigtrini.F
Chd|-- called by -----------
Chd|        FXBSINI                       source/constraints/fxbody/fxbsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FSIGTRINI(FXBELM, IPARG , X     , PM  , IXT ,
     .                     GEO   , FXBMOD, FXBSIG, NELT)
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
      INTEGER FXBELM(*), IPARG(NPARG,*), IXT(NIXT,*), NELT
      my_real
     .        FXBSIG(*), X(3,*), PM(NPROPM,*), FXBMOD(*),
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG, OFFSET, LFT, LLT, NFT, NFS, I, NG, IEL,
     .        MAT, PROP, N1, N2
      my_real
     .        AREA, E, X1, Y1, Z1, X2, Y2, Z2, L0, D11, D12, D13,
     .        D21, D22, D23, XC1, YC1, ZC1, XC2, YC2, ZC2, LC
C-----------------------------------------------
C
      DO IG=1,NELT,MVSIZ
         OFFSET=IG-1
         LFT=1
         LLT=MIN(MVSIZ,NELT-OFFSET)
         NFT=OFFSET*7
         NFS=OFFSET*2
         DO I=LFT,LLT
            NG=FXBELM(NFT+7*(I-1)+1)
            IEL=IPARG(3,NG)+FXBELM(NFT+7*(I-1)+2)
            MAT=IXT(1,IEL)
            PROP=IXT(4,IEL)
            AREA=GEO(1,PROP)
            E=PM(20,MAT)
            X1=X(1,IXT(2,IEL))
            Y1=X(2,IXT(2,IEL))
            Z1=X(3,IXT(2,IEL))
            X2=X(1,IXT(3,IEL))
            Y2=X(2,IXT(3,IEL))
            Z2=X(3,IXT(3,IEL))
            L0=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
            N1=FXBELM(NFT+7*(I-1)+3)
            N2=FXBELM(NFT+7*(I-1)+4)
            D11=FXBMOD(6*(N1-1)+1)
            D12=FXBMOD(6*(N1-1)+2)
            D13=FXBMOD(6*(N1-1)+3)
            D21=FXBMOD(6*(N2-1)+1)
            D22=FXBMOD(6*(N2-1)+2)
            D23=FXBMOD(6*(N2-1)+3)
            XC1=X1+D11
            YC1=Y1+D12
            ZC1=Z1+D13
            XC2=X2+D21
            YC2=Y2+D22
            ZC2=Z2+D23
            LC=SQRT((XC2-XC1)**2+(YC2-YC1)**2+(ZC2-ZC1)**2)
C
            FXBSIG(NFS+2*(I-1)+1)=E*AREA*(LC-L0)
            FXBSIG(NFS+2*(I-1)+2)=HALF*E*AREA*(LC-L0)**2
         ENDDO
      ENDDO
C
      RETURN
      END
      
