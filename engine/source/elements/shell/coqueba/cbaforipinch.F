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
      SUBROUTINE CBAFORIPINCH(
     1                          JFT   ,JLT   ,NG    ,NEL   ,NPLAT ,IPLAT ,
     2                          CDET  ,THK0  ,TH12  ,VOL,
     3                          FF    ,MM    ,
     4                          BCP   ,BP    , VFPINCH  ,DBETADXY,
     5                          RHO   ,LC    ,SSP       ,BPINCHDAMP,
     6                          VFPINCHDAMPX,VFPINCHDAMPY)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,NG,NPLAT,IPLAT(*),NEL
        my_real
     .     FF(NEL),MM(NEL,2),CDET(*),VOL(*),
     .     BCP(MVSIZ,8),BP(MVSIZ,4),
     .     THK0(*),TH12(*),VFPINCH(MVSIZ,4),DBETADXY(MVSIZ,3),
     .     RHO(MVSIZ),LC(MVSIZ),SSP(MVSIZ),
     .     FXX(MVSIZ),FYY(MVSIZ),FXY(MVSIZ),
     .     BPINCHDAMP(MVSIZ,8),
     .     VFPINCHDAMPX(MVSIZ,4),VFPINCHDAMPY(MVSIZ,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER EP,I,J
        my_real C1,C2
C-----------------------------------------------
#include "vectorize.inc"
        DO I=JFT,JLT
          EP=IPLAT(I)
          C2=VOL(EP)
          C1=TH12(EP)*CDET(EP)
C
          VFPINCH(EP,1) = VFPINCH(EP,1) + C1*BCP(EP,1)*MM(EP,1)
     .                    + C1*BCP(EP,2)*MM(EP,2)+ C2*BP(EP,1)*FF(EP)
          VFPINCH(EP,2) = VFPINCH(EP,2) + C1*BCP(EP,3)*MM(EP,1)
     .                    + C1*BCP(EP,4)*MM(EP,2)+ C2*BP(EP,2)*FF(EP)
          VFPINCH(EP,3) = VFPINCH(EP,3) + C1*BCP(EP,5)*MM(EP,1)
     .                    + C1*BCP(EP,6)*MM(EP,2)+ C2*BP(EP,3)*FF(EP)
          VFPINCH(EP,4) = VFPINCH(EP,4) + C1*BCP(EP,7)*MM(EP,1)
     .                    + C1*BCP(EP,8)*MM(EP,2)+ C2*BP(EP,4)*FF(EP)
C
          FXX(EP) = 0.0D0*RHO(EP)*LC(EP)*DBETADXY(EP,1)
          FYY(EP) = 0.0D0*RHO(EP)*LC(EP)*DBETADXY(EP,2)
          FXY(EP) = 0.0D0*RHO(EP)*LC(EP)*DBETADXY(EP,3)
C
          VFPINCHDAMPX(EP,1) = VFPINCHDAMPX(EP,1) + C2*(BPINCHDAMP(EP,1)*FXX(EP)
     .                       + HALF*BPINCHDAMP(EP,2)*FXY(EP))
          VFPINCHDAMPX(EP,2) = VFPINCHDAMPX(EP,2) + C2*(BPINCHDAMP(EP,3)*FXX(EP)
     .                       + HALF*BPINCHDAMP(EP,4)*FXY(EP))
          VFPINCHDAMPX(EP,3) = VFPINCHDAMPX(EP,3) + C2*(BPINCHDAMP(EP,5)*FXX(EP)
     .                       + HALF*BPINCHDAMP(EP,6)*FXY(EP))
          VFPINCHDAMPX(EP,4) = VFPINCHDAMPX(EP,4) + C2*(BPINCHDAMP(EP,7)*FXX(EP)
     .                       + HALF*BPINCHDAMP(EP,8)*FXY(EP))
C
          VFPINCHDAMPY(EP,1) = VFPINCHDAMPY(EP,1) + C2*(BPINCHDAMP(EP,2)*FYY(EP)
     .                       + HALF*BPINCHDAMP(EP,1)*FXY(EP))
          VFPINCHDAMPY(EP,2) = VFPINCHDAMPY(EP,2) + C2*(BPINCHDAMP(EP,4)*FYY(EP)
     .                       + HALF*BPINCHDAMP(EP,3)*FXY(EP))
          VFPINCHDAMPY(EP,3) = VFPINCHDAMPY(EP,3) + C2*(BPINCHDAMP(EP,6)*FYY(EP)
     .                       + HALF*BPINCHDAMP(EP,5)*FXY(EP))
          VFPINCHDAMPY(EP,4) = VFPINCHDAMPY(EP,4) + C2*(BPINCHDAMP(EP,8)*FYY(EP)
     .                       + HALF*BPINCHDAMP(EP,7)*FXY(EP))
C
        ENDDO
        RETURN
      END
