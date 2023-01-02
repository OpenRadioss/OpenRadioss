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
Chd|  CBAVISC                       source/elements/shell/coqueba/cbavisc.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBAVISC(JFT    ,JLT   ,VDEF  ,AMU  ,OFF ,
     2                   SHF    ,NU    ,RHO   ,SSP  ,AREA,
     3                   THK    ,FOR   ,MOM   ,NPT  ,MTN ,
     4                   IPARTC ,EVIS  ,DT1   ,NEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT ,NPT,MTN,MX,IPARTC(*),NEL
      my_real
     .   VDEF(MVSIZ,8),OFF(*),SHF(*),
     .   FOR(NEL,5),MOM(NEL,3),AMU(*),RHO(*),SSP(*),
     .   AREA(*),THK(*),NU(*),EVIS(NPSAV,*),DT1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   DTINV,VISC(MVSIZ),GG,GS,FAC,FX,FY,FXY,DV(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
        IF (NPT == 1.AND.MTN /= 1) THEN
          FAC =ZERO
        ELSE
          FAC =ONE
        ENDIF
        DO I=JFT,JLT
          VISC(I) =ONEP414*OFF(I)*AMU(I)*RHO(I)*SSP(I)*SQRT(AREA(I))
          GG =HALF/(ONE+NU(I))
          FX =VISC(I)*(VDEF(I,1)+NU(I)*VDEF(I,2))
          FY =VISC(I)*(VDEF(I,2)+NU(I)*VDEF(I,1))
          FXY =VISC(I)*VDEF(I,3) *GG
          FOR(I,1)=FOR(I,1)+FX
          FOR(I,2)=FOR(I,2)+FY
          FOR(I,3)=FOR(I,3)+FXY
          DV(I) =AREA(I)*THK(I)*DT1
          MX = IPARTC(I)
          EVIS(8,MX)=EVIS(8,MX)+(FX*VDEF(I,1)+FY*VDEF(I,2))*DV(I)
        ENDDO
        IF (NPT /= 1) THEN
         DO I=JFT,JLT
          VISC(I)    =ZEP3*THK(I)*VISC(I)
          GG =HALF/(ONE+NU(I))
          FX =VISC(I)*(VDEF(I,6)+NU(I)*VDEF(I,7))
          FY =VISC(I)*(VDEF(I,7)+NU(I)*VDEF(I,6))
          FXY =VISC(I)* VDEF(I,8) *GG
          MOM(I,1)=MOM(I,1)+FX
          MOM(I,2)=MOM(I,2)+FY
          MOM(I,3)=MOM(I,3)+FXY
          DV(I) =DV(I)*THK(I)
          MX = IPARTC(I)
          EVIS(8,MX)=EVIS(8,MX) 
     .             +(FX*VDEF(I,6)+FY*VDEF(I,7)+FXY*VDEF(I,8))*DV(I)
         ENDDO
        ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CBAVISNP1                     source/elements/shell/coqueba/cbavisc.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBAVISNP1(JFT, JLT,VXYZ,RXYZ,VCORE,
     1                     AMU, OFF,RHO ,SSP ,AREA,THK  ,
     2                     G  ,DT1 ,VF  ,
     3                     IPARTC,EVIS,KFTS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT ,IPARTC(*)
      my_real
     .   OFF(*),VF(MVSIZ,3,4),G(*),
     .   VCORE(MVSIZ,3,4),VXYZ(MVSIZ,3,4),RXYZ(MVSIZ,2,4),
     .   AMU(*),RHO(*),SSP(*),AREA(*),THK(*),EVIS(NPSAV,*),DT1
      INTEGER KFTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX, IC, II, J, JST(MVSIZ+1)
        my_real 
     .  C2,HSURA,MX23,MY23,MX34,MY34,MX13R,MY13R,
     .  SC6_V,SC5_V,CXZ_V,CYZ_V,SS3_V,HVL,RX13,RY13,RX24,RY24,
     .  RXSOM,RYSOM,TESY(MVSIZ),VHXZ(MVSIZ),VHYZ(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       DO I=JFT,JLT
        RX13 =RXYZ(I,1,1)
        RX24 =RXYZ(I,1,2)
        RXSOM=RXYZ(I,1,4)
        RY13 =RXYZ(I,2,1)
        RY24 =RXYZ(I,2,2)
        RYSOM=RXYZ(I,2,4)
        MY13R=VCORE(I,3,4)*RXSOM
        MX13R=VCORE(I,2,4)*RYSOM
        VHXZ(I)=VXYZ(I,3,3)-(MY13R-VCORE(I,2,3)*(RX13+RX24)
     1                     -MX13R+VCORE(I,1,3)*(RY13+RY24))*FOURTH
        VHYZ(I)=VXYZ(I,3,3)-(MY13R-VCORE(I,1,4)*(RX13-RX24)
     1                     -MX13R+VCORE(I,3,3)*(RY13-RY24))*FOURTH
       ENDDO
C
       DO I=JFT,JLT
         MX23=VCORE(I,1,3)
         MX34=VCORE(I,3,3)
         MY23=VCORE(I,2,3)
         MY34=VCORE(I,1,4)
         C2 =ONE_OVER_12*G(I)*RHO(I)*AREA(I)
         HVL = TWO1000*AMU(I)*SQRT(C2)*OFF(I)
         CXZ_V=(MY34*MY34+MX34*MX34)*HVL
         CYZ_V=(MY23*MY23+MX23*MX23)*HVL
         HSURA=THK(I)/AREA(I)
         SC5_V=CXZ_V*VHXZ(I)*HSURA
         SC6_V=CYZ_V*VHYZ(I)*HSURA
         SS3_V = SC5_V+SC6_V
C--------NOEUDS 1,3-------
         VF(I,3,3)=VF(I,3,3)+SS3_V
C--------NOEUDS 2,4-------
         VF(I,3,4)=VF(I,3,4)-SS3_V
         TESY(I)= (SC5_V*VHXZ(I)+SC6_V*VHYZ(I))*DT1
       ENDDO 
C
       MX = IPARTC(JFT)
       DO I=JFT,JLT
         EVIS(8,MX)=EVIS(8,MX) + TESY(I)
       ENDDO
C
      RETURN
      END
