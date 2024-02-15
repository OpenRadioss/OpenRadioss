Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  M25DELAM                      source/materials/mat/mat025/m25delam.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M25DELAM(MAT_PARAM,JFT,JLT,GSTR,DAMDL,MAT,NGL,NEL)
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use matparam_def_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
      INTEGER MAT(MVSIZ),NGL(MVSIZ)
C     REAL
      my_real :: DAMDL(*), GSTR(NEL,8)
      type (matparam_struct_) ,intent(in) :: mat_param
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX  
C     REAL
      my_real
     .   SHEAR, DAM1, DAM2
C     REAL
      my_real
     .   SHRDAM_1,SHRMAX_1,SHRDMAX_1
C--------------------------------------------------
C   E q u i v a l e n c e
C--------------------------------------------------
      MX = MAT(JFT)
      SHRDAM_1  =  mat_param%uparam(27) !   PM(65,MX)
      SHRMAX_1  =  mat_param%uparam(28) !   PM(66,MX)
      SHRDMAX_1 =  mat_param%uparam(29) !   PM(67,MX)
C      
C      shrdam=0.0655/1.3
C      shrmax=shrdam+0.01
C      igr=1+nft/mvsiz
C
      DO 100 I=JFT,JLT
      IF(DAMDL(I)>ZERO) THEN
        SHEAR=THREE_HALF*SQRT(GSTR(I,4)**2+GSTR(I,5)**2)
        DAM1=(SHEAR-SHRDAM_1)/(SHRMAX_1-SHRDAM_1)
        DAM2=DAM1*SHRMAX_1/SHEAR
        DAMDL(I)=MAX(DAM2,DAMDL(I))
        DAMDL(I)=MIN(SHRDMAX_1,DAMDL(I))       
      ENDIF
  100 CONTINUE
C
      DO 110 I=JFT,JLT
      IF(DAMDL(I)==ZERO) THEN
       SHEAR=THREE_HALF*SQRT(GSTR(I,4)**2+GSTR(I,5)**2)
       IF(SHEAR>SHRDAM_1) THEN
        DAM1=(SHEAR-SHRDAM_1)/(SHRMAX_1-SHRDAM_1)
        DAM2=DAM1*SHRMAX_1/SHEAR
        DAMDL(I)=MIN(SHRDMAX_1,DAM2)
        IF(IMCONV==1)THEN
#include "lockon.inc"
        WRITE(IOUT, 1200) NGL(I)
        WRITE(ISTDO,1300) NGL(I),TT
#include "lockoff.inc"
        ENDIF
       ENDIF
      ENDIF
  110 CONTINUE 
C
 1200 FORMAT('- DELAMINATION OF SHELL ELEMENT NUMBER ',I10)
 1300 FORMAT('- DELAMINATION OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      RETURN
      END     
