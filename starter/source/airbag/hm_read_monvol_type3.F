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
Chd|  HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|-- called by -----------
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MONVOL_CHECK_SURFCLOSE        share/modules1/monvol_struct_mod.F
Chd|        MONVOL_COMPUTE_VOLUME         share/modules1/monvol_struct_mod.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|        MONVOL_REVERSE_NORMALS        share/modules1/monvol_struct_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MONVOL_TYPE3(T_MONVOLN, IPM, IGEO, ITABM1, 
     .     NPT, PLD,UNITAB, LUID, NPC, IGRSURF, 
     .     ITAB, X, PM, GEO, IXC, IXTG, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NSURF, NFUNCT
#include      "com04_c.inc"
C     KMONVO, IREC
C     NIMV, NRVOLU
#include      "param_c.inc"
C     IIN
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: LUID, IPM(NPROPMI, *), IGEO(NPROPGI, *), ITABM1(*), NPT(*)
      INTEGER, INTENT(IN) :: NPC(*), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *),PLD(2, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, I, J
      INTEGER :: SURFID
      my_real :: FAC_M, FAC_L, FAC_T, FAC_C, FAC_GEN
      my_real :: SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      LOGICAL :: FOUND
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV, RHOI
      my_real :: GAMMA, T_RELAX, PEXT, PINI, PMAX, VINC, MI, EI, AVENT, BVENT, 
     .     TVENT, DPDEF, DTPDEF, FPORT, FPORP, FPORA, DUMMY, TI
      INTEGER :: NVENT, IPVENT, IDEF, IPORT, IPORP, IPORA, IEQUI
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
C     ==================
C     Conversion factors
C     ==================
      FAC_M = UNITAB%FAC_M(LUID)
      FAC_L = UNITAB%FAC_L(LUID)
      FAC_T = UNITAB%FAC_T(LUID)
      FAC_C = FAC_M / (FAC_L * FAC_T * FAC_T)
C     =======
C     Reading
C     =======
!     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('I_equi', IEQUI, IS_AVAILABLE, LSUBMODEL)
     
!     Line 2
      CALL HM_GET_FLOATV('Ascalet', SCAL_T, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleP', SCAL_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleS', SCAL_S, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleA', SCAL_A, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleD', SCAL_D, IS_AVAILABLE, LSUBMODEL, UNITAB)     
!     Line 3
      CALL HM_GET_FLOATV('Gamma', GAMMA, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Mu', AMU, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Trelax', T_RELAX, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Tini', TI, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Rhoi', RHOI, IS_AVAILABLE, LSUBMODEL, UNITAB)         
!     Line 4
      CALL HM_GET_FLOATV('Pext', PEXT, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Pini', PINI, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Pmax', PMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Vinc', VINC, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('Mini', MI, IS_AVAILABLE, LSUBMODEL, UNITAB)          
!     Ventholes
      CALL HM_GET_INTV('Nvent', NVENT, IS_AVAILABLE, LSUBMODEL)
      T_MONVOLN%IVOLU(11) = NVENT
      T_MONVOLN%NVENT = NVENT
      IF (NVENT > 0) THEN
         ALLOCATE(T_MONVOLN%IBAGHOL(NIBHOL, NVENT))
         T_MONVOLN%IBAGHOL(1:NIBHOL, 1:NVENT) = 0
         ALLOCATE(T_MONVOLN%RBAGHOL(NRBHOL, NVENT))
         T_MONVOLN%RBAGHOL(1:NRBHOL, 1:NVENT) = ZERO
      ENDIF
C     **********
C     Vent-holes
C     **********
C     NVENT dependant number of lines
      DO II = 1, NVENT
         CALL HM_GET_INT_ARRAY_INDEX('surf_IDv', IPVENT, II,IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOAT_ARRAY_INDEX('Avent', AVENT, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_INT_ARRAY_INDEX('Ideleted', IDEF, II,IS_AVAILABLE, LSUBMODEL)
         BVENT = ZERO
         CALL HM_GET_FLOAT_ARRAY_INDEX('Tvent', TVENT, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOAT_ARRAY_INDEX('DeltaPdef', DPDEF, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOAT_ARRAY_INDEX('DeltatPdef', DTPDEF, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
        
         CALL HM_GET_INT_ARRAY_INDEX('fct_IDt', IPORT, II,IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('fct_IDP', IPORP, II,IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('fct_IDA', IPORA, II,IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalet', FPORT, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleP', FPORP, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleA', FPORA, II, IS_AVAILABLE, LSUBMODEL, UNITAB)

         IF (FPORT == ZERO) FPORT = ONE
         IF (FPORP == ZERO) FPORP = ONE
         IF (FPORA == ZERO) FPORA = ONE
         IF(IPVENT == 0 .OR. IDEF == 0)THEN
            IDEF = -1
         ELSE
            IDEF = -2
         ENDIF
         T_MONVOLN%IBAGHOL(1, II) = IDEF
         IF(IPVENT == 0) THEN
            T_MONVOLN%IBAGHOL(2, II) = 0
         ELSE
            T_MONVOLN%IBAGHOL(2, II) = 0
            DO J = 1, NSURF
               IF (IPVENT == IGRSURF(J)%ID) T_MONVOLN%IBAGHOL(2, II) = J
            ENDDO
            IF(AVENT == ZERO) AVENT = ONE
         ENDIF
         IF(AVENT == ZERO) DPDEF = INFINITY
         IF(AVENT == ZERO) TVENT = INFINITY
         IF(DPDEF == ZERO .AND. DTPDEF == ZERO .AND. TVENT == ZERO) THEN
            T_MONVOLN%IBAGHOL(1, II) = -T_MONVOLN%IBAGHOL(1, II)
         ENDIF
         T_MONVOLN%RBAGHOL(1, II) = DPDEF
         T_MONVOLN%RBAGHOL(2, II) = AVENT
         T_MONVOLN%RBAGHOL(3, II) = TVENT
         T_MONVOLN%RBAGHOL(4, II) = DTPDEF
         T_MONVOLN%RBAGHOL(6, II) = BVENT
         T_MONVOLN%RBAGHOL(7, II) = FPORT
         T_MONVOLN%RBAGHOL(8, II) = FPORP
         T_MONVOLN%RBAGHOL(9, II) = FPORA
C     
         T_MONVOLN%IBAGHOL(3, II) = -1
         T_MONVOLN%IBAGHOL(4, II) = -1
         T_MONVOLN%IBAGHOL(5, II) = -1
         DO J = 1, NFUNCT
            IF(IPORT == NPC(J)) T_MONVOLN%IBAGHOL(3, II) = J
            IF(IPORP == NPC(J)) T_MONVOLN%IBAGHOL(4, II) = J
            IF(IPORA == NPC(J)) T_MONVOLN%IBAGHOL(5, II) = J
         ENDDO
         IF(IPORT == 0) T_MONVOLN%IBAGHOL(3, II) = 0
         IF(IPORP == 0) T_MONVOLN%IBAGHOL(4, II) = 0
         IF(IPORA == 0) T_MONVOLN%IBAGHOL(5, II) = 0
         T_MONVOLN%IBAGHOL(6, II) = 0
         T_MONVOLN%IBAGHOL(7, II) = 0
         T_MONVOLN%IBAGHOL(8, II) = 0
         IF(T_MONVOLN%IBAGHOL(3, II) == -1)THEN
            T_MONVOLN%IBAGHOL(3, II) = 0
            CALL ANCMSG(MSGID = 331,  ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORT)
         ENDIF
         IF(T_MONVOLN%IBAGHOL(4, II) == -1)THEN
            T_MONVOLN%IBAGHOL(4, II) = 0
            CALL ANCMSG(MSGID = 332, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORP)
         ENDIF
         IF(T_MONVOLN%IBAGHOL(5, II) == -1)THEN
            T_MONVOLN%IBAGHOL(5, II) = 0
            CALL ANCMSG(MSGID = 333, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORA)
         ENDIF  
      ENDDO
C     ================
C     Check operations
C     ================
C     External surface check
      T_MONVOLN%IVOLU(4) = 0
      FOUND = .FALSE. 
      DO II = 1, NSURF
         IF (SURFID == IGRSURF(II)%ID) THEN
            T_MONVOLN%IVOLU(4) = II
            T_MONVOLN%EXT_SURFID = II
            FOUND = .TRUE.
            EXIT
         ENDIF
      ENDDO
      IF (.NOT. FOUND) THEN
         CALL FREERR(3)
      ELSEIF (IGRSURF(T_MONVOLN%EXT_SURFID)%ISH4N3N == 0) THEN
         CALL ANCMSG(MSGID = 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = SURFID)
         CALL FREERR(3)
      ENDIF
      
C     Check surface closure
      CALL MONVOL_CHECK_SURFCLOSE(T_MONVOLN, ITAB, IGRSURF(T_MONVOLN%EXT_SURFID), X)
C     Set all normal on same side
      CALL MONVOL_ORIENT_SURF(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 3)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 3)

C     Parameter checks
      IF (GAMMA == ONE) THEN
         CALL ANCMSG(MSGID = 641, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_1,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF
      IF (AMU == ZERO) THEN
         AMU = EM02
      ENDIF

C     =====
C     Units
C     =====
C     Default value for time scale factor
      IF (SCAL_T == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('Ascalet', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_T = ONE * FAC_GEN
      ENDIF
      IF (SCAL_P == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleP', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_P = ONE * FAC_GEN
      ENDIF
      IF (SCAL_S == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleS', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_S = ONE * FAC_GEN
      ENDIF
      IF (SCAL_A == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleA', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_A = ONE * FAC_GEN
      ENDIF
      IF (SCAL_D == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleD', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_D = ONE * FAC_GEN
      ENDIF
      IF (PMAX == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('Pmax', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         PMAX = INFINITY * FAC_GEN
      ENDIF
!     Initial temperature 
      IF (TI == ZERO) TI = TWOHUNDRED95
      IF (IEQUI > 0) THEN
         IF (RHOI /= ZERO .AND. MI /= ZERO) THEN
            MI = RHOI / VOL
            CALL ANCMSG(MSGID = 1718,  ANMODE = ANINFO, MSGTYPE = MSGWARNING,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
         ENDIF
         IF (RHOI == ZERO .AND. MI == ZERO) THEN
            CALL ANCMSG(MSGID = 1717,  ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
         ENDIF
         IF (RHOI /= ZERO .AND. MI == ZERO) THEN
            MI = RHOI * VOL
         ENDIF
         IF (MI /= ZERO .AND. RHOI == ZERO) THEN
            RHOI = MI / VOL
         ENDIF
      ENDIF
C     =====
C     Store
C     =====
C     
!     Store CV
      T_MONVOLN%RVOLU(19) = ZERO
      IF (RHOI /= ZERO) THEN
         T_MONVOLN%RVOLU(19) = PEXT / ((GAMMA - ONE) * RHOI * TI)
      ENDIF
      T_MONVOLN%IVOLU(15) = IEQUI
      T_MONVOLN%IVOLU(11) = NVENT
      T_MONVOLN%RVOLU(1) = GAMMA
      T_MONVOLN%RVOLU(3) = PEXT
      VEPS = MAX(ZERO, VMIN - VOL + VINC)
      T_MONVOLN%RVOLU(4) = VOL + VEPS
      T_MONVOLN%RVOLU(5) = VINC
      T_MONVOLN%RVOLU(6) = PMAX
      T_MONVOLN%RVOLU(17) = VEPS
      IF (T_RELAX > ZERO) THEN
         EI = PEXT * (VOL + VEPS - VINC) / (GAMMA - ONE)
         T_MONVOLN%RVOLU(12) = PEXT
         T_MONVOLN%RVOLU(52) = (PINI - PEXT) * (VOL + VEPS - VINC) / T_RELAX / (GAMMA-ONE)
      ELSE
          EI = PINI * (VOL + VEPS - VINC) / (GAMMA - ONE)
          T_MONVOLN%RVOLU(12) = PINI
          T_MONVOLN%RVOLU(52) = ZERO
      ENDIF
      T_MONVOLN%RVOLU(56) = PINI
      T_MONVOLN%RVOLU(13) = EI
      
      T_MONVOLN%RVOLU(20) = MI
      T_MONVOLN%RVOLU(57) = MI
      T_MONVOLN%RVOLU(48) = T_RELAX

C     Store in data structure
      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE / SCAL_S
      T_MONVOLN%RVOLU(29) = ONE / SCAL_A
      T_MONVOLN%RVOLU(30) = ONE / SCAL_D   
C
      
C
      T_MONVOLN%RVOLU(2) = AMU
      T_MONVOLN%RVOLU(16) = VOL + VEPS
      T_MONVOLN%RVOLU(18) = SA
      T_MONVOLN%RVOLU(21) = ROT
      T_MONVOLN%RVOLU(22:24) = ZERO

C     =========
C     Print out
C     =========
      WRITE(IOUT, 1005) SURFID
      WRITE(IOUT, 1003) SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      WRITE(IOUT, 1002) SA, SV, VOL
      WRITE(IOUT, 1300) GAMMA, AMU, PEXT, PINI, PMAX, VINC, MI, T_RELAX, RHOI, TI
      IF (IEQUI == 1) THEN
         WRITE(IOUT, 1301)
      ELSE IF (IEQUI == 2) THEN
         WRITE(IOUT, 1302)
      ENDIF
      WRITE(IOUT,1370)
      WRITE(IOUT,1371) NVENT
      DO II = 1, NVENT
         IDEF = T_MONVOLN%IBAGHOL(1, II)
         IF (IPVENT /= 0) IPVENT = IGRSURF(T_MONVOLN%IBAGHOL(2, II))%ID
         IPORT = T_MONVOLN%IBAGHOL(3, II)
         IPORP = T_MONVOLN%IBAGHOL(4, II)
         IPORA = T_MONVOLN%IBAGHOL(5, II)
         IF(IPORT /= 0) IPORT = NPC(IPORT)
         IF(IPORP /= 0) IPORP = NPC(IPORP)
         IF(IPORA /= 0) IPORA = NPC(IPORA)
         AVENT = T_MONVOLN%RBAGHOL(2, II)
         TVENT = T_MONVOLN%RBAGHOL(3, II)
         DPDEF = T_MONVOLN%RBAGHOL(1, II)
         IF(ABS(IDEF) == 1)THEN
            WRITE(IOUT,1380) II, IPVENT, AVENT, IPORT, IPORP, IPORA,
     .           TVENT, DPDEF, DTPDEF, FPORT, FPORP, FPORA
         ELSE
            WRITE(IOUT,1381) II, IPVENT, AVENT, IPORT, IPORP, IPORA,
     .           TVENT, DPDEF, DTPDEF, FPORT, FPORP, FPORA
         ENDIF
      ENDDO
C-----------------------------------------------
C     E n d   o f   s o u r c e
C-----------------------------------------------
      
      RETURN
 1002 FORMAT(
     .     /5X,'INITIAL SURFACE OF MONITORED VOLUME . .=',1PG20.13,
     .     /5X,'SURFACE ERROR(NE.0 FOR NON CLOSED SURF)=',1PG20.13,
     .     /5X,'INITIAL VOLUME OF MONITORED VOLUME. . .=',1PG20.13)    
 1003 FORMAT(
     .     5X,'UNIT SCALE FOR TIME FUNCTIONS          =',1PG20.13,
     .     /5X,'UNIT SCALE FOR PRESSURE FUNCTIONS      =',1PG20.13,
     .     /5X,'UNIT SCALE FOR AREA FUNCTIONS          =',1PG20.13,
     .     /5X,'UNIT SCALE FOR ANGLE FUNCTIONS         =',1PG20.13,
     .     /5X,'UNIT SCALE FOR DISTANCE FUNCTIONS      =',1PG20.13)
 1005 FORMAT( 5X,'EXTERNAL SURFACE ID . . . . . . . . . .=',I10)
 1200 FORMAT( 5X,'LOAD CURVE NUMBER . . . . . . . . . . .=',I10,
     .     /5X,'CURVE TYPE  . . . . . . . . . . . . . .=',I10,
     .     /5X,'  0 : P=F(V)',
     .     /5X,'  1 : P=F(T)'/)
 1300 FORMAT(
     .     5X,'GAMMA . . . . . . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'VOLUMIC VISCOSITY . . . . . . . . . . .=',1PG20.13,
     .     /5X,'EXTERNAL PRESSURE . . . . . . . . . . .=',1PG20.13,
     .     /5X,'INITIAL PRESSURE. . . . . . . . . . . .=',1PG20.13,
     .     /5X,'BURSTING PRESSURE . . . . . . . . . . .=',1PG20.13,
     .     /5X,'INCOMPRESSIBLE VOLUME.. . . . . . . . .=',1PG20.13,
     .     /5X,'INITIAL MASS. . . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'RELAXATION TIME . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'INITIAL MASS DENSITY. . . . . . . . . .=',1PG20.13,
     .     /5X,'INITIAL TEMPERATURE . . . . . . . . . .=',1PG20.13)
 1370 FORMAT(
     .     /5X,'VENT HOLES                              ',
     .     /5X,'----------                              ')
 1301 FORMAT(
     .     /5X,'ISOTHERMAL EQUILIBRIUM . . . . . . . . .')
 1302 FORMAT(
     .     /5X,'ISENTROPIC EQUILIBRIUM . . . . . . . . .')
 1371 FORMAT(
     .     5X,'NUMBER OF VENT HOLES. . . . . . . . . .=',I10/)
 1380 FORMAT(
     .     5X,'VENT HOLE NUMBER . . . . . . . . . . . =',I10,
     .     /15X,'VENT HOLE SURFACE ID. . . . . . . . . .=',I10,
     .     /15X,'  (TOTAL AREA IS CONSIDERED)            ',
     .     /15X,'AVENT (VENT HOLE AREA OR SCALE FACTOR) =',1PG20.13,
     .     /15X,'  IF IPDEF=0    : VENT HOLE AREA '
     .     /15X,'  IF IPDEF/=0 : VENT HOLE AREA SCALE FACTOR'
     .     /15X,'POROSITY FUNCTION / TIME. . . . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / PRESSURE. . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / AREA. . . . . . . .=',I10,
     .     /15X,'START TIME FOR VENTING. . . . . . . . .=',1PG20.13,
     .     /15X,'RELATIVE PRES. FOR MEMBRANE DEFLATION .=',1PG20.13,
     .     /15X,'       (DPDEF = PDEF - PEXT)            ',
     .     /15X,'MIMINUM DURATION PRES. EXCEEDS PDEF     ',
     .     /15X,'         FOR MEMBRANE DEFLATION . . . .=',1PG20.13,
     .     /15X,'POROSITY TIME FUNCTION SCALE FACTOR    =',1PG20.13,
     .     /15X,'POROSITY PRESSURE FUNCTION SCALE FACTOR=',1PG20.13,
     .     /15X,'POROSITY AREA FUNCTION SCALE FACTOR . .=',1PG20.13)
 1381 FORMAT(
     .     5X,'VENT HOLE NUMBER . . . . . . . . . . . =',I10,
     .     /15X,'VENT HOLE SURFACE ID. . . . . . . . . .=',I10,
     .     /15X,'  (DELETED ELEMENTS AREA IS CONSIDERED) ',
     .     /15X,'AVENT (VENT HOLE AREA OR SCALE FACTOR) =',1PG20.13,
     .     /15X,'  IF IPDEF=0    : VENT HOLE AREA '
     .     /15X,'  IF IPDEF/=0 : VENT HOLE AREA SCALE FACTOR'
     .     /15X,'POROSITY FUNCTION / TIME. . . . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / PRESSURE. . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / AREA. . . . . . . .=',I10,
     .     /15X,'START TIME FOR VENTING. . . . . . . . .=',1PG20.13,
     .     /15X,'RELATIVE PRES. FOR MEMBRANE DEFLATION .=',1PG20.13,
     .     /15X,'       (DPDEF = PDEF - PEXT)            ',
     .     /15X,'MIMINUM DURATION PRES. EXCEEDS PDEF     ',
     .     /15X,'         FOR MEMBRANE DEFLATION . . . .=',1PG20.13,
     .     /15X,'POROSITY TIME FUNCTION SCALE FACTOR    =',1PG20.13,
     .     /15X,'POROSITY PRESSURE FUNCTION SCALE FACTOR=',1PG20.13,
     .     /15X,'POROSITY AREA FUNCTION SCALE FACTOR . .=',1PG20.13)
      END SUBROUTINE HM_READ_MONVOL_TYPE3
