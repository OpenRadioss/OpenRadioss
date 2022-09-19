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
Chd|  SHELL_LOC_COR                 source/tools/seatbelts/shell_loc_cor.F
Chd|-- called by -----------
Chd|        UPDATE_SLIPRING               source/tools/seatbelts/update_slipring.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|====================================================================
      SUBROUTINE SHELL_LOC_COR(X,IXC,J,XL2,YL2,XL3,YL3,XL4,YL4,IREP,NN1,NN2,N_DIR2)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXC(NIXC,NUMELC),J,IREP,NN1,NN2
      my_real, INTENT(IN) ::  X(3,NUMNOD)
      my_real, INTENT(INOUT) ::  XL2,YL2,XL3,YL3,XL4,YL4,N_DIR2(2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        RXL,RYL,RZL,SXL,SYL,SSZL,R11L,R12L,R13L,
     .        R21L,R22L,R23L,R31L,R32L,R33L,XX,YY,ZZ,OFFBL,DETA1L
C---------------------------------------------------------
C
C---------------------------------------------------------
C     Computation of new reference configuration
C---------------------------------------------------------
C
      RXL=X(1,IXC(3,J))+X(1,IXC(4,J))-X(1,IXC(2,J))-X(1,IXC(5,J))
      SXL=X(1,IXC(4,J))+X(1,IXC(5,J))-X(1,IXC(2,J))-X(1,IXC(3,J))
      RYL=X(2,IXC(3,J))+X(2,IXC(4,J))-X(2,IXC(2,J))-X(2,IXC(5,J))
      SYL=X(2,IXC(4,J))+X(2,IXC(5,J))-X(2,IXC(2,J))-X(2,IXC(3,J))
      RZL=X(3,IXC(3,J))+X(3,IXC(4,J))-X(3,IXC(2,J))-X(3,IXC(5,J))
      SSZL=X(3,IXC(4,J))+X(3,IXC(5,J))-X(3,IXC(2,J))-X(3,IXC(3,J))
C
      OFFBL = ONE
      CALL CLSKEW3(1,1,IREP,
     .             RXL, RYL, RZL,
     .             SXL, SYL, SSZL,
     .             R11L,R12L,R13L,R21L,R22L,R23L,R31L,R32L,R33L,DETA1L,OFFBL)
C
      XX=X(1,IXC(3,J))-X(1,IXC(2,J))
      YY=X(2,IXC(3,J))-X(2,IXC(2,J))
      ZZ=X(3,IXC(3,J))-X(3,IXC(2,J))
      XL2=R11L*XX+R21L*YY+R31L*ZZ
      YL2=R12L*XX+R22L*YY+R32L*ZZ
      XX=X(1,IXC(4,J))-X(1,IXC(2,J))
      YY=X(2,IXC(4,J))-X(2,IXC(2,J))
      ZZ=X(3,IXC(4,J))-X(3,IXC(2,J))
      XL3=R11L*XX+R21L*YY+R31L*ZZ
      YL3=R12L*XX+R22L*YY+R32L*ZZ
      XX=X(1,IXC(5,J))-X(1,IXC(2,J))
      YY=X(2,IXC(5,J))-X(2,IXC(2,J))
      ZZ=X(3,IXC(5,J))-X(3,IXC(2,J))
      XL4=R11L*XX+R21L*YY+R31L*ZZ
      YL4=R12L*XX+R22L*YY+R32L*ZZ
C
      IF ((NN1 > 0).AND.(NN2 > 0)) THEN
        XX=X(1,NN2)-X(1,NN1)
        YY=X(2,NN2)-X(2,NN1)
        ZZ=X(3,NN2)-X(3,NN1)
        N_DIR2(1)=R11L*XX+R21L*YY+R31L*ZZ
        N_DIR2(2)=R12L*XX+R22L*YY+R32L*ZZ
      ENDIF
C
      RETURN
      END
