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
!||    volp_lfluid        ../engine/source/airbag/volp_lfluid.F
!||--- called by ------------------------------------------------------
!||    monvol0            ../engine/source/airbag/monvol0.F
!||--- uses       -----------------------------------------------------
!||    finter_mixed_mod   ../engine/source/tools/finter_mixed.F90
!||    output_mod         ../common_source/modules/output/output_mod.F90
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||====================================================================
      SUBROUTINE VOLP_LFLUID(IVOLU, RVOLU, VOL, FSAV, NPC, TF, PMAIN, OUTPUT,PYTHON, NFUNCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE python_funct_mod
      USE finter_mixed_mod
      USE OUTPUT_MOD , ONLY : OUTPUT_
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
      my_real TF(*), RVOLU(*), FSAV(*), VOL
      TYPE(OUTPUT_),INTENT(INOUT) :: OUTPUT
      TYPE(PYTHON_), intent(inout) :: PYTHON
      INTEGER, intent(in) :: NFUNCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNCT

      my_real P0, VOL0,  AREA, VEPS, VINC, POLD, PEXT, VOLD, PRES, Q,  QOLD, DV,   XFUN
      my_real SCALEF, BULK, PMAX, GMASS, DMASS, RHOI
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !Pressure calculation
      PEXT = RVOLU(3)
      VOL0 = RVOLU(4)
      VINC = RVOLU(5)
      POLD = RVOLU(12)
      VOLD = RVOLU(16)
      VEPS = RVOLU(17)
      AREA = RVOLU(18)
      QOLD = RVOLU(23)
      GMASS = RVOLU(20)
      RHOI = RVOLU(34)

C   BULK
      IFUNCT = IVOLU(21)
      SCALEF = RVOLU(35)
      IF(IFUNCT == 0) THEN
         BULK = SCALEF    
      ELSE
         BULK = SCALEF * FINTER_MIXED(PYTHON,NFUNCT,IFUNCT,TT,NPC,TF)
      ENDIF

C   FLUID MASS IN
      IFUNCT = IVOLU(22)
      SCALEF = RVOLU(36)
      IF(IFUNCT == 0) THEN
         DMASS = SCALEF * DT1
      ELSE
         DMASS = SCALEF * FINTER_MIXED(PYTHON,NFUNCT,IFUNCT,TT,NPC,TF) * DT1
      ENDIF 
      GMASS = GMASS + DMASS
      RVOLU(54)=RVOLU(54)+DMASS

C   FLUID MASS OUT (time)
      IFUNCT = IVOLU(23)
      SCALEF = RVOLU(37)
      IF(IFUNCT == 0) THEN
         DMASS = SCALEF * DT1
      ELSE
         DMASS = SCALEF * FINTER_MIXED(PYTHON,NFUNCT,IFUNCT,TT,NPC,TF) * DT1
      ENDIF 
      GMASS = GMASS - DMASS

C   FLUID MASS OUT (pressure)
      IFUNCT = IVOLU(24)
      SCALEF = RVOLU(38)
      IF(IFUNCT == 0) THEN
         DMASS = SCALEF * DT1
      ELSE
         DMASS = SCALEF * FINTER_MIXED(PYTHON,NFUNCT,IFUNCT,POLD,NPC,TF) * DT1
      ENDIF 
      GMASS = GMASS - DMASS

C   P0
      IFUNCT = IVOLU(25)
      SCALEF = RVOLU(39)
      IF(IFUNCT == 0) THEN
         P0 = SCALEF    
      ELSE
         P0 = SCALEF * FINTER_MIXED(PYTHON,NFUNCT,IFUNCT,TT,NPC,TF)
      ENDIF

C   PMAX
      IFUNCT = IVOLU(26)
      SCALEF = RVOLU(40)
      IF(IFUNCT == 0) THEN
         PMAX = SCALEF    
      ELSE
         PMAX = SCALEF * FINTER_MIXED(PYTHON,NFUNCT,IFUNCT,TT,NPC,TF)
      ENDIF

      VOL0 = GMASS / RHOI
      VOL = VOL + VEPS
      XFUN = (VOL0-VINC)/(VOL-VINC)
      PRES = BULK * MAX(ZERO,LOG(XFUN)) + P0
      PRES = MIN(PRES,PMAX)
      Q = ZERO

      IF (ISPMD+1==PMAIN) THEN
        DV=VOL-VOLD
        OUTPUT%WFEXT=OUTPUT%WFEXT+(HALF*(Q+QOLD+PRES+POLD))*DV
        FSAV(1)=GMASS
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
        FSAV(13)=ZERO
        FSAV(14)=ZERO
        FSAV(15)=RVOLU(54)
        FSAV(16)=ZERO
      ENDIF
C
      RVOLU(16)=VOL
      RVOLU(12)=PRES
      RVOLU(20)=GMASS
      RVOLU(23)=Q
C
      RETURN
      END
