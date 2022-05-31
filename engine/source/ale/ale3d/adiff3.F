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
Chd|  ADIFF3                        source/ale/ale3d/adiff3.F     
Chd|-- called by -----------
Chd|        AETURB                        source/ale/aeturb.F           
Chd|        AKTURB                        source/ale/akturb.F           
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ADIFF3(PHIN,PHI,GRAD,COEF,ALE_CONNECT,VOL)
      USE ALE_CONNECTIVITY_MOD
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
C     REAL ou REAL*8
      my_real
     .   PHIN(*), PHI(*), GRAD(6,*), COEF(*), VOL(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IV5, IV6, IAD2
C     REAL ou REAL*8
      my_real
     .   DPHI(MVSIZ)      
C-----------------------------------------------
      DO 10 I=LFT,LLT
      IE =NFT+I
      IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
      IV1 = ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
      IV2 = ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
      IV3 = ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
      IV4 = ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
      IV5 = ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
      IV6 = ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
      IF(IV1.LE.0)IV1=IE
      IF(IV2.LE.0)IV2=IE
      IF(IV3.LE.0)IV3=IE
      IF(IV4.LE.0)IV4=IE
      IF(IV5.LE.0)IV5=IE
      IF(IV6.LE.0)IV6=IE
      DPHI(I) =
     1   (COEF(IE)*COEF(IV1))*(PHI(IV1)-PHI(IE))*GRAD(1,I)
     2  /MAX(EM20,(COEF(IE)+COEF(IV1)))
     3 + (COEF(IE)*COEF(IV2))*(PHI(IV2)-PHI(IE))*GRAD(2,I)
     4  /MAX(EM20,(COEF(IE)+COEF(IV2)))
     5 + (COEF(IE)*COEF(IV3))*(PHI(IV3)-PHI(IE))*GRAD(3,I)
     6  /MAX(EM20,(COEF(IE)+COEF(IV3)))
     7 + (COEF(IE)*COEF(IV4))*(PHI(IV4)-PHI(IE))*GRAD(4,I)
     8  /MAX(EM20,(COEF(IE)+COEF(IV4)))
     9 + (COEF(IE)*COEF(IV5))*(PHI(IV5)-PHI(IE))*GRAD(5,I)
     A  /MAX(EM20,(COEF(IE)+COEF(IV5)))
     B + (COEF(IE)*COEF(IV6))*(PHI(IV6)-PHI(IE))*GRAD(6,I)
     C  /MAX(EM20,(COEF(IE)+COEF(IV6)))
  10  CONTINUE
C-----------------------------------------------------------
      DO 30 I=LFT,LLT
  30  DPHI(I)=TWO  *DPHI(I)*DT1/MAX(VOL(I),EM20)
C-----------------------------------------------------------
      DO 40 I=LFT,LLT
  40  PHIN(I)=PHIN(I)+DPHI(I)
C-----------------------------------------------------------
      RETURN
      END
