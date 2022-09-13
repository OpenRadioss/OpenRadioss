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
Chd|  FLUXSW                        source/fluid/fluxsw.F         
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FLUXSW (NSFLSW,NEFLSW,  NNFLSW,  CRFLSW,  FLSW  ,
     .                   X     ,V     ,ELBUF                )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSFLSW
      INTEGER NEFLSW(*), NNFLSW(8,*)
C     REAL
      my_real
     .   CRFLSW(6,*), FLSW(9,*), X(3,*), V(3,*), ELBUF(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I2, IS, NEL, I1, I, IN, N1, N2, N3, N4, ND, IB2, IB3, IB4,
     .   IB5, IB6, IB10, IB11, IB12, J
C     REAL
      my_real
     .   CRX, CRY, CRZ, CNX, CNY, CNZ, SRT, SV2, FLVM, PRE, ENE, RHO,
     .   VOL, TKE, TEM, TDE, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4,
     .   Z4, VX1, VY1, VZ1, VX2, VY2, VZ2, VX3, VY3, VZ3, VX4, VY4, VZ4,
     .   VMX, VMY, VMZ, OMX, OMY, OMZ, SFX, SFY, SFZ, VL2, VLM, SFM,
     .   FLV, SW1, SW2
C-----------------------------------------------
      I2=0
      DO 500 IS = 1, NSFLSW
C
        FLSW(1,IS) = ZERO
        FLSW(2,IS) = ZERO
        FLSW(3,IS) = ZERO
        FLSW(4,IS) = ZERO
        FLSW(5,IS) = ZERO
        FLSW(6,IS) = ZERO
        FLSW(7,IS) = ZERO
        FLSW(8,IS) = ZERO
        FLSW(9,IS) = ZERO        
C
        NEL = NEFLSW(IS)
        I1=I2+1
        I2=I2+NEL
        CRX = CRFLSW(1,IS)
        CRY = CRFLSW(2,IS)
        CRZ = CRFLSW(3,IS)
        CNX = CRFLSW(4,IS)
        CNY = CRFLSW(5,IS)
        CNZ = CRFLSW(6,IS)
        SRT = ZERO
        SV2 = ZERO
        FLVM = ZERO        
C
        DO 400 I = I1, I2
          IN = NNFLSW(1,I)
          N1 = NNFLSW(2,I)
          N2 = NNFLSW(3,I)
          N3 = NNFLSW(4,I)
          N4 = NNFLSW(5,I)
          ND = NNFLSW(6,I)
          IB2 = NNFLSW(7,I)
          IB3 = NNFLSW(8,I)
C
          IB4 = IB3 +  IN
          IB5 = IB4 +  IN
          IB6 = IB5 +  IN
          IB10= IB6 +  IN
          IB11= IB10+  IN
          IB12= IB11+  IN
          PRE = -(ELBUF(IB2)+ELBUF(IB2+1)+ELBUF(IB2+2))*THIRD
          ENE = ELBUF(IB3)
          RHO = ELBUF(IB4)
          VOL = ELBUF(IB6)
          TKE = ELBUF(IB10)
          TEM = ELBUF(IB11)
          TDE = ELBUF(IB12)
C
          X1 = X(1,N1)
          Y1 = X(2,N1)
          Z1 = X(3,N1)
C
          X2 = X(1,N2)
          Y2 = X(2,N2)
          Z2 = X(3,N2)
C
          X3 = X(1,N3)
          Y3 = X(2,N3)
          Z3 = X(3,N3)
C
          X4 = X(1,N4)
          Y4 = X(2,N4)
          Z4 = X(3,N4)
C
          VX1 = V(1,N1)
          VY1 = V(2,N1)
          VZ1 = V(3,N1)
C
          VX2 = V(1,N2)
          VY2 = V(2,N2)
          VZ2 = V(3,N2)
C
          VX3 = V(1,N3)
          VY3 = V(2,N3)
          VZ3 = V(3,N3)
C
          VX4 = V(1,N4)
          VY4 = V(2,N4)
          VZ4 = V(3,N4)
C
          VMX = (VX1+VX2+VX3+VX4)/ND
          VMY = (VY1+VY2+VY3+VY4)/ND
          VMZ = (VZ1+VZ2+VZ3+VZ4)/ND
C
          OMX = .25*(X1 +X2 +X3 +X4) - CRX
          OMY = .25*(Y1 +Y2 +Y3 +Y4) - CRY
          OMZ = .25*(Z1 +Z2 +Z3 +Z4) - CRZ
C
          SFX = HALF*((Y3-Y1)*(Z4-Z2)-
     1                 (Z3-Z1)*(Y4-Y2))
          SFY =HALF*((Z3-Z1)*(X4-X2)-
     1                 (X3-X1)*(Z4-Z2))
                    SFZ = HALF*((X3-X1)*(Y4-Y2)-
     1                 (Y3-Y1)*(X4-X2))
C
          VL2 = VMX*VMX+VMY*VMY+VMZ*VMZ
          VLM = SQRT(VL2)
          SFM = SQRT(SFX*SFX+SFY*SFY+SFZ*SFZ)
          FLV = VMX*SFX + VMY*SFY + VMZ*SFZ
          SW1 = SFX * (OMY*VMZ - OMZ*VMY) +
     1             SFY * (OMZ*VMX - OMX*VMZ) +
     2             SFZ * (OMX*VMY - OMY*VMX)
          SW2 = (OMX*OMX + OMY*OMY + OMZ*OMZ)*
     1             (CNX   *SFX + CNY   *SFY + CNZ   *SFZ)-
     2             (OMX*CNX    + OMY*CNY    + OMZ*CNZ   )*
     3             (OMX*SFX + OMY*SFY + OMZ*SFZ)
C
          SRT        = SRT        + SFM
          SV2        = SV2        + SFM*VL2
          FLVM       = FLVM       + FLV
          FLSW(1,IS) = FLSW(1,IS) + FLV*RHO
          FLSW(2,IS) = FLSW(2,IS) + SW1
          FLSW(3,IS) = FLSW(3,IS) + SW2
          FLSW(4,IS) = FLSW(4,IS) + VLM*SFM
          FLSW(5,IS) = FLSW(5,IS) + RHO*SFM
          FLSW(6,IS) = FLSW(6,IS) + PRE*SFM
          FLSW(7,IS) = FLSW(7,IS) + ENE*SFM
          FLSW(8,IS) = FLSW(8,IS) + TKE*SFM
          FLSW(9,IS) = FLSW(9,IS) + TDE*SFM
C
  400   CONTINUE
C
        FLSW(2,IS) = FLSW(2,IS)/FLSW(3,IS)
        FLSW(4,IS) = FLSW(4,IS)/SRT
        FLSW(5,IS) = FLSW(5,IS)/SRT
        FLSW(6,IS) = FLSW(6,IS)/SRT
        FLSW(7,IS) = FLSW(7,IS)/SRT
        FLSW(8,IS) = FLSW(8,IS)/SRT
        FLSW(9,IS) = FLSW(9,IS)/SRT
C
        FLSW(3,IS) = SQRT( SV2/SRT - (FLVM/SRT)**2 )
        IF(FLSW(4,IS)==ZERO)THEN
         FLSW(3,IS)=ZERO         
        ELSE
         FLSW(3,IS)=FLSW(3,IS)/FLSW(4,IS)
        ENDIF
C
  500 CONTINUE
C
      WRITE (IOUT, 1000) TT
      DO 600 IS = 1, NSFLSW
        WRITE (IOUT,1100) IS, (FLSW(J,IS),J=1,9)
  600 CONTINUE
C
 1000 FORMAT (3H T=,1PE9.3,11H  M. FLUX  ,11H  ROTATION ,
     .                     11H  STD ERROR,11H  VELOCITY ,
     .                     11H  DENSITY  ,11H  PRESSURE ,
     .                     11H  ENERGIE  ,11H  ENER. T. ,
     .                     11H  DISS. T. )
 1100 FORMAT (4H SET,I5,3X,1P9E11.3)
      RETURN
      END
