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
!||    fixfingeo                     ../engine/source/constraints/general/impvel/fixfingeo.F
!||--- called by ------------------------------------------------------
!||    resol                         ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    finter2                       ../engine/source/tools/curve/vinter.F
!||    finter2_smooth                ../engine/source/tools/curve/finter_smooth.F
!||--- uses       -----------------------------------------------------
!||    nodal_arrays_mod              ../common_source/modules/nodal_arrays.F90
!||    python_call_funct_cload_mod   ../engine/source/loads/general/python_call_funct_cload.F90
!||    python_funct_mod              ../common_source/modules/python_mod.F90
!||    sensor_mod                    ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE FIXFINGEO(PYTHON, NODES, IBFV    ,NPC    ,TF       ,
     2                     VEL    ,SENSOR_TAB,
     3                     CPTREAC,NODREAC,NODNX_SMS,NSENSOR,
     4                     FTHREAC, WFEXT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE python_funct_mod
      use python_call_funct_cload_mod
      use nodal_arrays_mod
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(python_), INTENT(IN) :: PYTHON
      TYPE(nodal_arrays_) :: NODES
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*),CPTREAC,NODREAC(*)
      INTEGER IBFV(NIFV,*), NODNX_SMS(*)
      my_real :: TF(*), VEL(LFXVELR,*), FTHREAC(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, L, I1, N1, N2, ISENS,
     .        ILENC, IPOSC, IADC, ISMOOTH
      my_real AX, AXI, VV, A0, AA, FAC, FACX, STARTT, STOPT, TS,
     .         DW, DD,
     .        YC, TSC, DYDXC, RW_SMS,
     .        SKEW1, SKEW2, SKEW3,
     .           XF, YF, ZF, XA, YA, ZA,
     .        AOLD0(3),
     .        VX1, VY1, VZ1, VX2, VY2, VZ2
      my_real, DIMENSION(:), ALLOCATABLE :: MASS, VX, VY, VZ      
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      DOUBLE PRECISION, INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real FINTER2,FINTER2_SMOOTH
      EXTERNAL FINTER2,FINTER2_SMOOTH
C-----------------------------------------------
      IF(N2D==1)THEN
       AX=ONE
      ELSE
       AX=ZERO
      ENDIF
C-------------------------------------------------
C  /IMPVEL/FGEO RIGID LINKS
C-------------------------------------------------
      ALLOCATE(MASS(NUMNOD),VX(NUMNOD),VY(NUMNOD),VZ(NUMNOD))
      MASS(1:NUMNOD)=ZERO
      VX(1:NUMNOD)=ZERO
      VY(1:NUMNOD)=ZERO
      VZ(1:NUMNOD)=ZERO
      DO N=1,NFXVEL
        IF (IBFV(13,N) /= 2) CYCLE
        STOPT  = VEL(3,N)
        IF(TT>STOPT)  THEN
           N1 = IABS(IBFV(1,N))
           N2 = IBFV(14,N)
C        
           VX1= NODES%V(1,N1)+NODES%A(1,N1)*DT12
           VY1= NODES%V(2,N1)+NODES%A(2,N1)*DT12
           VZ1= NODES%V(3,N1)+NODES%A(3,N1)*DT12
           VX2= NODES%V(1,N2)+NODES%A(1,N2)*DT12
           VY2= NODES%V(2,N2)+NODES%A(2,N2)*DT12
           VZ2= NODES%V(3,N2)+NODES%A(3,N2)*DT12
C
           VX(N2) = VX(N2) + NODES%MS(N1)*VX1+ NODES%MS(N2)*VX2/IBFV(16,N)
           VY(N2) = VY(N2) + NODES%MS(N1)*VY1+ NODES%MS(N2)*VY2/IBFV(16,N)
           VZ(N2) = VZ(N2) + NODES%MS(N1)*VZ1+ NODES%MS(N2)*VZ2/IBFV(16,N)
           MASS(N2) = MASS(N2) + NODES%MS(N1) + NODES%MS(N2)/IBFV(16,N)
        ENDIF
      ENDDO
      DO N=1,NUMNOD
         IF(MASS(N) == ZERO) CYCLE
         VX(N)=VX(N)/MASS(N)
         VY(N)=VY(N)/MASS(N)
         VZ(N)=VZ(N)/MASS(N)
      ENDDO
C    
      DO N=1,NFXVEL
        IF (IBFV(13,N) /= 2) CYCLE
        STOPT  = VEL(3,N)
        IF(TT>STOPT)  THEN
           N1 = IABS(IBFV(1,N))
           N2 = IBFV(14,N)       
           NODES%A(1,N1)=(VX(N2)-NODES%V(1,N1))/DT12
           NODES%A(2,N1)=(VY(N2)-NODES%V(2,N1))/DT12
           NODES%A(3,N1)=(VZ(N2)-NODES%V(3,N1))/DT12
           NODES%A(1,N2)=(VX(N2)-NODES%V(1,N2))/DT12
           NODES%A(2,N2)=(VY(N2)-NODES%V(2,N2))/DT12
           NODES%A(3,N2)=(VZ(N2)-NODES%V(3,N2))/DT12
        ENDIF
      ENDDO
      DEALLOCATE(MASS, VX, VY, VZ)
C-----------------------
C  DISPLACEMENT PHASE
C-----------------------
      DO N=1,NFXVEL
        IF (IBFV(13,N) == 0) CYCLE
        STARTT = VEL(2,N)
        STOPT  = VEL(3,N)
        FACX   = VEL(5,N)
        I=IABS(IBFV(1,N))
        IF(TT<STARTT) CYCLE
        IF(TT>STOPT)  CYCLE
        IF(NSENSOR>0) THEN
           ISENS = IBFV(4,N)
           IF(ISENS==0)THEN
              TS=TT
           ELSE
              TS = TT - SENSOR_TAB(ISENS)%TSTART
              IF(TS<0.0) CYCLE
           ENDIF
        ELSE
           TS=TT
        ENDIF
        TSC = (TS+DT2)*FACX
        IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
           RW_SMS = ONE
        ELSE
           IF(NODNX_SMS(I)==0)THEN
              RW_SMS = ONE
           ELSE
              RW_SMS = ZERO
           ENDIF
        ENDIF
C
        IF(CPTREAC/=0) THEN
           AOLD0(1)=NODES%A(1,I)
           AOLD0(2)=NODES%A(2,I)
           AOLD0(3)=NODES%A(3,I)
        ENDIF
C
        L = IBFV(3,N)
        IF(NCYCLE==0)THEN
           IPOSC = 0
        ELSE
           IPOSC = IBFV(5,N)
        ENDIF
        IADC  = NPC(L) / 2 + 1
        ILENC = NPC(L+1) / 2 - IADC - IPOSC
!!        YC = FINTER2(TF,IADC,IPOSC,ILENC,TSC,DYDXC)
        ISMOOTH = 0
        IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
        IF (ISMOOTH == 0) THEN
          YC = FINTER2(TF,IADC,IPOSC,ILENC,TSC,DYDXC)
        ELSE IF(ISMOOTH > 0) THEN
          YC = FINTER2_SMOOTH(TF,IADC,IPOSC,ILENC,TSC,DYDXC)
        ELSE
          ISMOOTH = -ISMOOTH ! the id the python function is saved in the position of ISMOOTH in the NPC array 
          ! interpolation function with ACTIVE_NODE enabled
          CALL PYTHON_CALL_FUNCT_CLOAD(PYTHON, ISMOOTH,TSC, YC,I,NODES)
        ENDIF ! IF (ISMOOTH == 0)
        IBFV(5,N) = IPOSC
C
        IF(IBFV(13,N) == 1) THEN
             !/IMPDISP/FGEO
           FAC  = VEL(1,N)
           SKEW1= VEL(7,N)
           SKEW2= VEL(8,N)
           SKEW3= VEL(9,N)
        ELSEIF(IBFV(13,N) == 2) THEN
           !/IMPVEL/FGEO
           XA = NODES%X(1,I)
           YA = NODES%X(2,I)
           ZA = NODES%X(3,I)
           I1 = IBFV(14,N)
           XF = NODES%X(1,I1)
           YF = NODES%X(2,I1)
           ZF = NODES%X(3,I1)
           FAC= SQRT((XF-XA)**2+(YF-YA)**2+(ZF-ZA)**2)
           IF(FAC < VEL(7,N)) THEN
              VEL(3,N) = TT
              YC = ZERO
              WRITE(IOUT,'(A,I10,1X,I10,A,1PE12.5)') 
     .       ' RIGID LINK ON NODES',NODES%ITAB(IBFV(1,N)),IBFV(14,N),
     .       ' ACTIVATED AT TIME',TT
           ENDIF
           SKEW1= (XF-XA)/FAC
           SKEW2= (YF-YA)/FAC
           SKEW3= (ZF-ZA)/FAC
           FAC  = VEL(1,N)
        ENDIF
        VEL(6,N) = YC
C  
C IF sms on the degree of freedom,
C dw was already counted in WFEXT, in sms_fixvel...
        DW      = VEL(4,N)
        WFEXT   = WFEXT + RW_SMS*DW
        VEL(4,N)= (ONE-RW_SMS)*VEL(4,N)
C
        YC = YC * FAC
        AXI=ONE-AX+AX*NODES%X(2,I)
C
        DD = SKEW1*NODES%D(1,I) + SKEW2*NODES%D(2,I) + SKEW3*NODES%D(3,I)
        VV = SKEW1*NODES%V(1,I) + SKEW2*NODES%V(2,I) + SKEW3*NODES%V(3,I)
        A0 = SKEW1*NODES%A(1,I) + SKEW2*NODES%A(2,I) + SKEW3*NODES%A(3,I)
C
        IF(IBFV(13,N) == 1) YC =(YC-DD)/DT2
        YC =(YC-VV)/DT12
        AA = YC - A0
C
        NODES%A(1,I)=SKEW1*YC
        NODES%A(2,I)=SKEW2*YC
        NODES%A(3,I)=SKEW3*YC
C
        DW = FOURTH*NODES%MS(I)*(YC*DT12+TWO*VV)*AA*AXI*NODES%WEIGHT(I)
        WFEXT = WFEXT + DT1*DW
C  
C dt2*dw into memory ; if sms on the degree of freedom,
C part of dt2*dw was already computed and stored in sms_fixvel...
        VEL(4,N) = VEL(4,N)+DT2*DW
C
        IF (CPTREAC/=0) THEN
            I=IABS(IBFV(1,N))
            IF (NODREAC(I)/=0) THEN
              FTHREAC(1,NODREAC(I)) = FTHREAC(1,NODREAC(I)) + (NODES%A(1,I) -
     &                                AOLD0(1))*NODES%MS(I)*DT12
              FTHREAC(2,NODREAC(I)) = FTHREAC(2,NODREAC(I)) + (NODES%A(2,I) -
     &                                AOLD0(2))*NODES%MS(I)*DT12
              FTHREAC(3,NODREAC(I)) = FTHREAC(3,NODREAC(I)) + (NODES%A(3,I) -
     &                                AOLD0(3))*NODES%MS(I)*DT12
            ENDIF
        ENDIF
C
      ENDDO ! N=1,NFXVEL
C
      RETURN
      END
