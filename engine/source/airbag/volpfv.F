Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    volpfv             ../engine/source/airbag/volpfv.F
!||--- called by ------------------------------------------------------
!||    monvol0            ../engine/source/airbag/monvol0.F
!||--- uses       -----------------------------------------------------
!||    finter_mixed_mod   ../engine/source/tools/finter_mixed.F90
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||====================================================================
      SUBROUTINE VOLPFV(IVOLU,RVOLU  ,VOL,FSAV,NPC,
     2                  TF   ,PMAIN, WFEXT,PYTHON,NFUNCT)
         USE python_funct_mod
         USE finter_mixed_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IVOLU(*), PMAIN
      my_real TF(*),RVOLU(*),FSAV(*)
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
      TYPE(PYTHON_), intent(inout) :: PYTHON
      INTEGER, intent(in) :: NFUNCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITFUN,ITYPFUN
      my_real VOL,V0,AREA,VEPS,VINC,POLD,PEXT,VOLD,PRES,Q,QOLD,DV,SCALE,XFUN
C-----------------------------------------------
C
C CALCUL DE LA PRESSION
C
      ITFUN  =IVOLU(7)
      ITYPFUN=IVOLU(19)
      PEXT   =RVOLU(3)
      V0     =RVOLU(4)
      VINC   =RVOLU(5)
      POLD   =RVOLU(12)
      VOLD   =RVOLU(16)
      VEPS   =RVOLU(17)
      AREA   =RVOLU(18)
      QOLD   =RVOLU(23)
      SCALE  =RVOLU(15)
      VOL = VOL + VEPS
      V0  =  V0
      SELECT CASE (ITYPFUN)
      CASE(0)
         XFUN = (V0-VINC)/(VOL-VINC)
      CASE(1, 3)
         XFUN = TT
      CASE(2)
         XFUN = (VOL-VINC) / (V0-VINC)
      CASE DEFAULT
         
      END SELECT
      PRES = SCALE * FINTER_MIXED(PYTHON,NFUNCT,ITFUN,XFUN,NPC,TF)
      IF (ITYPFUN == 3) THEN
         PRES = PRES * (V0-VINC) / (VOL-VINC)
      ENDIF
      Q = ZERO
      IF (ISPMD+1==PMAIN) THEN
        DV=VOL-VOLD
        WFEXT=WFEXT+(HALF*(Q+QOLD+PRES+POLD))*DV
        FSAV(1)=ZERO
        FSAV(2)=VOL
        FSAV(3)=PRES
        FSAV(4)=AREA
        FSAV(5)=ZERO
        FSAV(6)=ZERO
        FSAV(7)=ZERO
        FSAV(8)=ZERO
        FSAV(9)=ZERO
        FSAV(10)=ZERO
        FSAV(11)=ZERO
        FSAV(12)=ZERO
      ENDIF
C
      RVOLU(16)=VOL
      RVOLU(12)=PRES
      RVOLU(23)=Q
C
      RETURN
      END
