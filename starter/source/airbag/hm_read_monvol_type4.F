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
Chd|  HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|-- called by -----------
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MONVOL_CHECK_SURFCLOSE        share/modules1/monvol_struct_mod.F
Chd|        MONVOL_CHECK_VENTHOLE_SURF    share/modules1/monvol_struct_mod.F
Chd|        MONVOL_COMPUTE_VOLUME         share/modules1/monvol_struct_mod.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|        MONVOL_REVERSE_NORMALS        share/modules1/monvol_struct_mod.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MONVOL_TYPE4(T_MONVOLN, IPM, IGEO, ITABM1,
     .     SENSORS, NPT, PLD,
     .     UNITAB, LUID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .     LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NSURF
#include      "com04_c.inc"
C     KMONVO, IREC
C     NIMV, NRVOLU
#include      "param_c.inc"
C     IIN
#include      "units_c.inc"
C     LUNIT
#include      "sysunit.inc"
C     IPRI
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: LUID, IPM(NPROPMI, *), IGEO(NPROPGI, *),ITABM1(*), NPT(*)
      INTEGER, INTENT(IN) :: NPC(*), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *),PLD(2, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ
      INTEGER :: SURFID
      my_real :: FAC_M, FAC_L, FAC_T, FAC_C
      my_real :: SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      LOGICAL :: FOUND, DECREASE
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV, 
     .     PEXT, TI, PINI, FAC_GEN
      INTEGER :: IEQUI, ITTF, NP, IP, IS, NJET, NVENT
      my_real :: GAMAI, CPAI, CPBI, CPCI, CPI, CVI, RMWI, CPG, RMWG, RHOI, ESPECI, TI2, MI,
     .     TTFIRE, SHOL
      my_real, DIMENSION(:), ALLOCATABLE :: GAMA, CPA, CPB, CPC, FMASS, FTEMP, FPT, FPA, FPZ,
     .     TVENT, DPDEF, DTPDEF, FVDP, AVENT, BVENT, TSTOPE, FPORT, FPORP, FPORA, 
     .     FPORT1, FPORP1,FPORA1
      INTEGER, DIMENSION(:), ALLOCATABLE :: IMASS, IFLU, ITEMP, ISENS, IJET, NJ1, NJ2, NJ3, 
     .     IPT, IPA, IPZ, IVDP, IDTPDEF, IPVENT, IFVENT, IPORT, IPORP, IPORA, IPORT1, IPORP1, IPORA1
      CHARACTER(LEN = 40) :: MESS
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      EXTERNAL USR2SYS
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
      MESS = 'MONITORED VOLUME DEFINITION             '
C     =======
C     Reading
C     =======
C     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
C     
      CALL HM_GET_FLOATV('Ascalet', SCAL_T, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleP', SCAL_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleS', SCAL_S, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleA', SCAL_A, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleD', SCAL_D, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     Line 2
      CALL HM_GET_FLOATV('MU', AMU, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Pext', PEXT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T0', TI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Iequi', IEQUI, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ittf', ITTF, IS_AVAILABLE, LSUBMODEL)
C     Line 3
      CALL HM_GET_FLOATV('GAMMAi', GAMAI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('cpai', CPAI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('cpbi', CPBI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('cpci', CPCI, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     Injectors
      CALL HM_GET_INTV('Njet', NJET, IS_AVAILABLE, LSUBMODEL)
      
      T_MONVOLN%NJET = NJET
      T_MONVOLN%IVOLU(8) = NJET
      IF (NJET > 0) THEN
         ALLOCATE(T_MONVOLN%IBAGJET(NIBJET, NJET))
         T_MONVOLN%IBAGJET(1:NIBJET, 1:NJET) = 0
         ALLOCATE(T_MONVOLN%RBAGJET(NRBJET, NJET))
         T_MONVOLN%RBAGJET(1:NRBJET, 1:NJET) = ZERO
      ENDIF
      IF (NJET > 0) THEN
         ALLOCATE(GAMA(NJET), CPA(NJET), CPB(NJET), CPC(NJET))
         ALLOCATE(IMASS(NJET), IFLU(NJET), FMASS(NJET), ITEMP(NJET), FTEMP(NJET), ISENS(NJET))
         ALLOCATE(IJET(NJET), NJ1(NJET), NJ2(NJET), NJ3(NJET))
         ALLOCATE(IPT(NJET), IPA(NJET), IPZ(NJET), FPT(NJET), FPA(NJET), FPZ(NJET))
         DO II = 1, NJET
            CALL HM_GET_FLOAT_ARRAY_INDEX('GAMMAj', GAMA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('cpa', CPA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('cpb', CPB(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('cpc', CPC(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDmas', IMASS(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iflow', IFLU(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalemas', FMASS(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDT', ITEMP(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleT', FTEMP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('sens_ID', ISENS(II), II,IS_AVAILABLE, LSUBMODEL)
           
            CALL HM_GET_INT_ARRAY_INDEX('Ijet', IJET(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('ABG_N1', NJ1(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('ABG_N2', NJ2(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('ABG_N3', NJ3(II), II,IS_AVAILABLE, LSUBMODEL)

            FPT(II) = ZERO
            FPA(II) = ZERO
            FPZ(II) = ZERO
            IF (IJET(II) > 0) THEN
               CALL HM_GET_INT_ARRAY_INDEX('IPt', IPT(II), II,IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('IPtheta', IPA(II), II,IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('IPdelta', IPZ(II), II,IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Fpt', FPT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Fptheta', FPA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Fpdelta', FPZ(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDIF
         ENDDO
      ENDIF

C     Ventholes
      CALL HM_GET_INTV('Nvent', NVENT, IS_AVAILABLE, LSUBMODEL)
      
      T_MONVOLN%IVOLU(11) = NVENT
      T_MONVOLN%NVENT = NVENT
      IF (NVENT > 0) THEN
         ALLOCATE(T_MONVOLN%IBAGHOL(NIBHOL, NVENT))
         T_MONVOLN%IBAGHOL(1:NIBHOL, 1:NVENT) = 0
         ALLOCATE(T_MONVOLN%RBAGHOL(NRBHOL, NVENT))
         T_MONVOLN%RBAGHOL(1:NRBHOL, 1:NVENT) = ZERO
      ENDIF
      IF (NVENT > 0) THEN
         ALLOCATE(TVENT(NVENT), DPDEF(NVENT), DTPDEF(NVENT), FVDP(NVENT), AVENT(NVENT), 
     .        BVENT(NVENT), TSTOPE(NVENT))
         ALLOCATE(IPVENT(NVENT), IVDP(NVENT), IDTPDEF(NVENT), IFVENT(NVENT))
         ALLOCATE(IPORT(NVENT), IPORP(NVENT), IPORA(NVENT), IPORT1(NVENT), 
     .        IPORP1(NVENT), IPORA1(NVENT))
         ALLOCATE(FPORT(NVENT), FPORP(NVENT), FPORA(NVENT), FPORT1(NVENT), 
     .        FPORP1(NVENT), FPORA1(NVENT))
         DO II = 1, NVENT
            IFVENT(II) = 0
            CALL HM_GET_INT_ARRAY_INDEX('surf_IDv', IPVENT(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Avent', AVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Bvent', BVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tstop', TSTOPE(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
 
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tvent', TVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dPdef', DPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dtPdef', DTPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDv', IVDP(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalev', FVDP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('IdtPdef', IDTPDEF(II), II,IS_AVAILABLE, LSUBMODEL)
           
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDt', IPORT(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDP', IPORP(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDA', IPORA(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalet', FPORT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleP', FPORP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleA', FPORA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)

            CALL HM_GET_INT_ARRAY_INDEX("fct_IDt'", IPORT1(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDP'", IPORP1(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDA'", IPORA1(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX("Fscalet'", FPORT1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX("FscaleP'", FPORP1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX("FscaleA'", FPORA1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         ENDDO
      ENDIF
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
      ELSEIF (IGRSURF(T_MONVOLN%IVOLU(4))%ISH4N3N == 0) THEN
         CALL ANCMSG(MSGID = 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = SURFID)
         CALL FREERR(3)
      ENDIF

C     Check surface closure
      CALL MONVOL_CHECK_SURFCLOSE(T_MONVOLN, ITAB, IGRSURF(T_MONVOLN%EXT_SURFID), X)
C     Set all normal on same side
      CALL MONVOL_ORIENT_SURF(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 2)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 2)

      IF (ITTF < 0 .OR. ITTF > 3) THEN
         CALL ANCMSG(MSGID = 773, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF
C     =====
C     Units
C     =====
      FAC_M = UNITAB%FAC_M(LUID)
      FAC_L = UNITAB%FAC_L(LUID)
      FAC_T = UNITAB%FAC_T(LUID)
      FAC_C = FAC_M / (FAC_L * FAC_T * FAC_T)

C     ==============
C     Default values
C     ==============
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
    
      IF (AMU == ZERO) AMU = EM02
      IF(PEXT == ZERO) THEN
         PEXT = 101325.D0 * (FAC_L_WORK * FAC_T_WORK * FAC_T_WORK) / FAC_M_WORK
      ENDIF
      PINI = PEXT
      IF (TI == ZERO) TI = TWOHUNDRED95
      DO II = 1, NJET
         IF (IMASS(II) /= 0 .AND. FMASS(II) == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fscalemas', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FMASS(II) = ONE * FAC_GEN
         ENDIF
         IF (ITEMP(II) /= 0 .AND. FTEMP(II) == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('FscaleT', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FTEMP(II) = ONE * FAC_GEN
         ENDIF
         IF (IFLU(II) /= 0) THEN
            FMASS(II) = FMASS(II) / FAC_T
         ENDIF
         IF (FPT(II) == ZERO) THEN 
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fpt', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FPT(II) = ONE * FAC_GEN
         ENDIF
         IF (FPA(II) == ZERO) THEN 
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fptheta', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FPA(II) = ONE * FAC_GEN
         ENDIF
         IF (FPZ(II) == ZERO) THEN 
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fpdelta', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FPZ(II) = ONE * FAC_GEN
         ENDIF
      ENDDO
      DO II = 1, NVENT
         IF (IVDP(II) > 0) IFVENT(II) = 2
         IF (IPVENT(II) == 0) THEN
            BVENT(II) = ZERO
         ENDIF
         IF (FPORT(II)  == ZERO) FPORT(II) = ONE
         IF (FPORP(II)  == ZERO) FPORP(II) = ONE
         IF (FPORA(II)  == ZERO) FPORA(II) = ONE
         IF (FPORT1(II) == ZERO) FPORT1(II) = ONE
         IF (FPORP1(II) == ZERO) FPORP1(II) = ONE
         IF (FPORA1(II) == ZERO) FPORA1(II) = ONE
      ENDDO
      IF (GAMAI == ZERO) THEN
         IF (NJET > 0) THEN
            GAMAI = GAMA(1)
            CPAI = CPA(1)
            CPBI = CPB(1)
            CPCI = CPC(1)
         ENDIF
      ENDIF
      CPI = CPAI + TI * (CPBI + CPCI * TI)
      CVI = CPI / GAMAI
      RMWI = CVI * (GAMAI - ONE)
      MI = PINI * (VOL + VEPS) / (RMWI * TI)
      TTFIRE = INFINITY
      DO II = 1, NJET
         IF (ISENS(II) > 0) THEN
            FOUND = .FALSE.
            DO IS = 1, SENSORS%NSENSOR
               IF (ISENS(II) == SENSORS%SENSOR_TAB(IS)%SENS_ID) THEN
                  T_MONVOLN%IBAGJET(4, II) = IS
                  IF (SENSORS%SENSOR_TAB(IS)%TCRIT < TTFIRE) TTFIRE = SENSORS%SENSOR_TAB(IS)%TCRIT
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 17, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = ISENS(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
      ENDDO
      IF (TTFIRE == INFINITY) THEN
         TTFIRE = ZERO
      ENDIF

C     =====
C     Store
C     ===== 
      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE / SCAL_S
      T_MONVOLN%RVOLU(29) = ONE / SCAL_A
      T_MONVOLN%RVOLU(30) = ONE / SCAL_D
C     
      IF (IEQUI > 0) IEQUI = 1
      T_MONVOLN%IVOLU(15) = IEQUI
      T_MONVOLN%IVOLU(17) = ITTF
      T_MONVOLN%IVOLU(8) = NJET
C     
      T_MONVOLN%RVOLU(31) = PINI
      T_MONVOLN%RVOLU(7) = CPAI
      T_MONVOLN%RVOLU(8) = CPBI
      T_MONVOLN%RVOLU(9) = CPCI
      T_MONVOLN%RVOLU(10) = RMWI
      T_MONVOLN%RVOLU(49) = TTFIRE
C
      T_MONVOLN%RVOLU(1) = GAMAI
      T_MONVOLN%RVOLU(3) = PEXT
      T_MONVOLN%RVOLU(4) = VOL+VEPS
      T_MONVOLN%RVOLU(11) = MI
      T_MONVOLN%RVOLU(12) = PINI
      T_MONVOLN%RVOLU(13) = TI
      T_MONVOLN%RVOLU(14) = RMWI*MI
      T_MONVOLN%RVOLU(17) = VEPS
      T_MONVOLN%RVOLU(20) = MI
      T_MONVOLN%RVOLU(25) = TI
      T_MONVOLN%RVOLU(61) = GAMAI
      RHOI = PINI / (TI * RMWI)
      T_MONVOLN%RVOLU(62) = RHOI
      TI2 = TI * TI
      ESPECI = TI * (CPAI + HALF * CPBI * TI + THIRD * CPCI * TI2 - RMWI)
     
      T_MONVOLN%RVOLU(63) = ESPECI + RMWI * TI
      T_MONVOLN%RVOLU(64) = ZERO
      T_MONVOLN%RVOLU(65) = ZERO
      T_MONVOLN%RVOLU(66) = ESPECI
      DO II = 1, NJET
         T_MONVOLN%IBAGJET(13, II) = 0            
         T_MONVOLN%RBAGJET(1, II) = GAMA(II)
         T_MONVOLN%RBAGJET(2, II) = CPA(II)
         T_MONVOLN%RBAGJET(3, II) = CPB(II)
         T_MONVOLN%RBAGJET(4, II) = CPC(II)
         T_MONVOLN%RBAGJET(5, II) = FMASS(II)
         T_MONVOLN%RBAGJET(6, II) = FTEMP(II)
         T_MONVOLN%RBAGJET(12, II) = FPT(II)
         T_MONVOLN%RBAGJET(13, II) = FPA(II)
         T_MONVOLN%RBAGJET(14, II) = FPZ(II)
         IF (IMASS(II) == 0)THEN
            T_MONVOLN%IBAGJET(1, II) = 0
         ELSE
            FOUND = .FALSE.
            DO JJ = 1, NFUNCT
               IF (IMASS(II) == NPC(JJ)) THEN
                  T_MONVOLN%IBAGJET(1, II) = JJ
                  DECREASE = .FALSE.
                  NP = (NPT(JJ + 1) - NPT(JJ)) / 2
                  IF (IFLU(II) == 0) THEN
                     DO IP = (NPT(JJ) - 1) / 2 + 1, (NPT(JJ + 1) - 1) / 2 - 1
                        IF (PLD(2, IP + 1) < PLD(2, IP)) DECREASE = .TRUE.
                     ENDDO
                     IF (DECREASE) THEN
                        CALL ANCMSG(MSGID=540, MSGTYPE = MSGWARNING, ANMODE = ANINFO_BLIND_1,
     .                       I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IMASS(II), I3 = II)
                     ENDIF
                  ELSE
                     DO IP = (NPT(JJ) - 1) / 2 + 1, (NPT(JJ + 1) - 1) / 2
                        IF (PLD(2, IP) < ZERO) DECREASE = .TRUE.
                     ENDDO
                     IF (DECREASE) THEN
                        CALL ANCMSG(MSGID = 541, MSGTYPE = MSGWARNING, ANMODE = ANINFO_BLIND_1,
     .                       I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IMASS(II), I3 = II)
                     ENDIF
                  ENDIF
                  FOUND = .TRUE.
                 EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 10, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IMASS(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
         T_MONVOLN%IBAGJET(2, II) = IFLU(II)
         IF (ITEMP(II) == 0) THEN
            T_MONVOLN%IBAGJET(3, II) = 0
         ELSE
            FOUND = .FALSE.
            DO JJ = 1, NFUNCT
               IF (ITEMP(II) == NPC(JJ)) THEN
                  T_MONVOLN%IBAGJET(3, II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 11, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = ITEMP(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
!     Jetting
         IF (IJET(II) > 0) THEN
            T_MONVOLN%IBAGJET(5, II) = USR2SYS(NJ1(II), ITABM1, MESS, T_MONVOLN%ID)
            T_MONVOLN%IBAGJET(6, II) = USR2SYS(NJ2(II), ITABM1, MESS, T_MONVOLN%ID)
            IF(NJ3(II) /= 0) THEN
               T_MONVOLN%IBAGJET(7, II) = USR2SYS(NJ3(II), ITABM1, MESS, T_MONVOLN%ID)
            ENDIF
            FOUND = .FALSE.
            DO JJ= 1, NFUNCT
               IF (IPT(II) == NPC(JJ)) THEN
                  T_MONVOLN%IBAGJET(8, II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 12, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IPT(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
            FOUND = .FALSE.
            DO JJ = 1, NFUNCT
               IF (IPA(II) == NPC(JJ)) THEN
                  T_MONVOLN%IBAGJET(9, II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 13, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IPA(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
            FOUND = .FALSE.
            DO JJ = 1, NFUNCT
               IF (IPZ(II) == NPC(JJ)) THEN
                  T_MONVOLN%IBAGJET(10, II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 14, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IPZ(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
         CPG = CPA(II) + TI * (CPB(II) + CPC(II) * TI)
         RMWG = CPG * (GAMA(II) - ONE) / GAMA(II)
         T_MONVOLN%RBAGJET(1, II) = RMWG
         IF (NJ1(II) == 0) THEN
            IJET(II) = 0
         ELSE
            IJET(II) = 1
         ENDIF
         IF (NJ3(II) == 0) THEN
            T_MONVOLN%IBAGJET(7, II) = T_MONVOLN%IBAGJET(5, II)
         ENDIF
      ENDDO                     ! NJET loop
!     Ventholes
      DO II = 1, NVENT
         T_MONVOLN%IBAGHOL(13, II) = 0
         T_MONVOLN%RBAGHOL(7, II) = FPORT(II)
         T_MONVOLN%RBAGHOL(8, II) = FPORP(II)
         T_MONVOLN%RBAGHOL(9, II) = FPORA(II)
         T_MONVOLN%RBAGHOL(10, II) = FPORT1(II)
         T_MONVOLN%RBAGHOL(11, II) = FPORP1(II)
         T_MONVOLN%RBAGHOL(12, II) = FPORA1(II)
         T_MONVOLN%IBAGHOL(1, II) = 0
         T_MONVOLN%IBAGHOL(10, II) = IFVENT(II)
         T_MONVOLN%IBAGHOL(11, II) = IDTPDEF(II)
         T_MONVOLN%IBAGHOL(12, II) = 0
         IF (IPVENT(II) == 0) THEN
            T_MONVOLN%IBAGHOL(2, II) = 0
         ELSE
            T_MONVOLN%IBAGHOL(2, II) = 0
            FOUND = .FALSE.
            DO JJ = 1, NSURF
               IF (IPVENT(II) == IGRSURF(JJ)%ID) THEN
                  T_MONVOLN%IBAGHOL(2, II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF(.NOT. FOUND)THEN
               CALL ANCMSG(MSGID = 532, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IPVENT(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ELSEIF(IGRSURF(T_MONVOLN%IBAGHOL(2, II))%ISH4N3N == 0) THEN
               CALL ANCMSG(MSGID = 330, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
            IF (AVENT(II) == ZERO) AVENT(II) = ONE
         ENDIF
         IF (AVENT(II) == ZERO) DPDEF(II) = INFINITY
         IF (AVENT(II) == ZERO) TVENT(II) = INFINITY
         IF (DPDEF(II) == ZERO .AND. DTPDEF(II) == ZERO .AND. TVENT(II) == ZERO) THEN
            T_MONVOLN%IBAGHOL(1, II) = 1
         ENDIF
         T_MONVOLN%RBAGHOL(1, II) = DPDEF(II)
         T_MONVOLN%RBAGHOL(2, II) = AVENT(II)
         T_MONVOLN%RBAGHOL(3, II) = TVENT(II)
         T_MONVOLN%RBAGHOL(4, II) = DTPDEF(II)
         T_MONVOLN%RBAGHOL(6, II) = BVENT(II)
         IF (IVDP(II) /= 0 .AND. FVDP(II) == ZERO) FVDP(II) = ONE
         T_MONVOLN%RBAGHOL(13, II) = FVDP(II)
         IF (TSTOPE(II) == ZERO) TSTOPE(II) = INFINITY
         T_MONVOLN%RBAGHOL(14, II) = TSTOPE(II)
C     
         T_MONVOLN%IBAGHOL(3, II) = -1
         T_MONVOLN%IBAGHOL(4, II) = -1
         T_MONVOLN%IBAGHOL(5, II) = -1
         T_MONVOLN%IBAGHOL(6, II) = -1
         T_MONVOLN%IBAGHOL(7, II) = -1
         T_MONVOLN%IBAGHOL(8, II) = -1
         T_MONVOLN%IBAGHOL(9, II) = -1
         DO JJ = 1, NFUNCT
            IF (IPORT(II) == NPC(JJ))  T_MONVOLN%IBAGHOL(3, II) = JJ
            IF (IPORP(II) == NPC(JJ))  T_MONVOLN%IBAGHOL(4, II) = JJ
            IF (IPORA(II) == NPC(JJ))  T_MONVOLN%IBAGHOL(5, II) = JJ
            IF (IPORT1(II) == NPC(JJ)) T_MONVOLN%IBAGHOL(6, II) = JJ
            IF (IPORP1(II) == NPC(JJ)) T_MONVOLN%IBAGHOL(7, II) = JJ
            IF (IPORA1(II) == NPC(JJ)) T_MONVOLN%IBAGHOL(8, II) = JJ
            IF (IVDP(II) == NPC(JJ))   T_MONVOLN%IBAGHOL(9, II) = JJ
         ENDDO
         IF (IPORT(II) == 0)  T_MONVOLN%IBAGHOL(3, II) = 0
         IF (IPORP(II) == 0)  T_MONVOLN%IBAGHOL(4, II) = 0
         IF (IPORA(II) == 0)  T_MONVOLN%IBAGHOL(5, II) = 0
         IF (IPORT1(II) == 0) T_MONVOLN%IBAGHOL(6, II) = 0
         IF (IPORP1(II) == 0) T_MONVOLN%IBAGHOL(7, II) = 0
         IF (IPORA1(II) == 0) T_MONVOLN%IBAGHOL(8, II) = 0
         IF (IVDP(II) == 0) T_MONVOLN%IBAGHOL(9, II) = 0
         IF (T_MONVOLN%IBAGHOL(3, II) == -1) THEN
            T_MONVOLN%IBAGHOL(3, II) = 0
            CALL ANCMSG(MSGID = 331, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORT(II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(4, II) == -1) THEN
            T_MONVOLN%IBAGHOL(4, II) = 0
            CALL ANCMSG(MSGID = 332, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORP(II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(5, II) == -1) THEN
            T_MONVOLN%IBAGHOL(5, II)=0
            CALL ANCMSG(MSGID = 333, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORA(II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(6, II) == -1) THEN
            T_MONVOLN%IBAGHOL(6, II) = 0
            CALL ANCMSG(MSGID=331, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORT1(II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(7, II) == -1) THEN
            T_MONVOLN%IBAGHOL(7, II)=0
            CALL ANCMSG(MSGID=332, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORP1(II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(8, II) == -1) THEN
            T_MONVOLN%IBAGHOL(8, II) = 0
            CALL ANCMSG(MSGID=333, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORA1(II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(9, II) == -1) THEN
            T_MONVOLN%IBAGHOL(9, II) = 0
            CALL ANCMSG(MSGID = 518, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IVDP(II))
         ENDIF
      ENDDO
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
      WRITE(IOUT, 1400) AMU, TI, PEXT, PINI
      IF (IEQUI == 0) THEN
         WRITE(IOUT, 1401)
      ELSE
         WRITE(IOUT, 1402)
      ENDIF
      WRITE(IOUT, 1410) GAMAI, CPAI, CPBI, CPCI
      WRITE(IOUT,1420)
      WRITE(IOUT,1421)NJET
      DO II = 1, NJET
         WRITE(IOUT,1430) II,
     .        IMASS(II), IFLU(II), FMASS(II), ITEMP(II), FTEMP(II), ISENS(II)
         WRITE(IOUT, 1440) GAMA(II), CPA(II), CPB(II), CPC(II)
         WRITE(IOUT, 1450) IJET(II)
         IF (IJET(II) > 0) THEN
            IF (NJ3(II) == 0) THEN
               WRITE(IOUT, 1460) NJ1(II), NJ2(II), IPT(II), IPA(II), IPZ(II), 
     .              FPT(II), FPA(II), FPZ(II)
            ELSE
               WRITE(IOUT, 1461) NJ1(II), NJ2(II), NJ3(II), IPT(II), IPA(II), IPZ(II),
     .              FPT(II), FPA(II), FPZ(II)
            ENDIF
         ENDIF
      ENDDO
      WRITE(IOUT, 1470) NVENT,TTFIRE
      IF(NVENT > 0) THEN
         WRITE(IOUT, 1471) ITTF
      ENDIF
      DO II = 1, NVENT
         WRITE(IOUT,1472) II, IPVENT(II)
         IF (IPVENT(II) == 0 .AND. AVENT(II) == ZERO) THEN
            CALL ANCMSG(MSGID = 1019, MSGTYPE = MSGWARNING, ANMODE = ANINFO,
     .           I1 = T_MONVOLN%ID, I2 = II, C1 = T_MONVOLN%TITLE, C2 = 'VENT HOLE SURFACE')
         ENDIF
         IF (IFVENT(II) <= 1) WRITE(IOUT, 1481)
         IF (IFVENT(II) == 2) THEN
            WRITE(IOUT, 1482) IVDP(II), FVDP(II)
         ENDIF
         IF (IFVENT(II) == 3) WRITE(IOUT, 1484)
         IF (IFVENT(II) == 4) WRITE(IOUT, 1485)
         IF (IPVENT(II) /= 0) THEN
            CALL MONVOL_CHECK_VENTHOLE_SURF(IPRI, T_MONVOLN, IGRSURF, II, SHOL, X, IXC, IXTG)
            T_MONVOLN%RBAGHOL(15, II) = SHOL
            WRITE(IOUT,1479) 
     .           SHOL,AVENT(II),BVENT(II),
     .           IPORT(II),IPORP(II),IPORA(II),FPORT(II),FPORP(II),FPORA(II),
     .           IPORT1(II),IPORP1(II),IPORA1(II),FPORT1(II),FPORP1(II),FPORA1(II)
            WRITE(IOUT,1480) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
         ELSE
            WRITE(IOUT,1489)
     .           AVENT(II),BVENT(II),
     .           IPORT(II),IPORP(II),IPORA(II),FPORT(II),FPORP(II),FPORA(II),
     .           IPORT1(II),IPORP1(II),IPORA1(II),FPORT1(II),FPORP1(II),FPORA1(II)
            WRITE(IOUT,1480) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
         ENDIF
      ENDDO
C     ===========
C     Memory free
C     ===========
      IF (NJET > 0) THEN
         DEALLOCATE(GAMA, CPA, CPB, CPC)
         DEALLOCATE(IMASS, IFLU, FMASS, ITEMP, FTEMP, ISENS)
         DEALLOCATE(IJET, NJ1, NJ2, NJ3)
         DEALLOCATE(IPT, IPA, IPZ, FPT, FPA, FPZ)
      ENDIF
      IF (NVENT > 0) THEN
         DEALLOCATE(TVENT, DPDEF, DTPDEF, FVDP, AVENT, BVENT, TSTOPE)
         DEALLOCATE(IPVENT, IVDP, IDTPDEF, IFVENT)
         DEALLOCATE(IPORT, IPORP, IPORA, IPORT1, IPORP1, IPORA1)
         DEALLOCATE(FPORT, FPORP, FPORA, FPORT1, FPORP1, FPORA1)
      ENDIF
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
 1400 FORMAT(
     .     5X,'VOLUMIC VISCOSITY . . . . . . . . . . .=',1PG20.13,
     .     /5X,'INITIAL TEMPERATURE . . . . . . . . . .=',1PG20.13,
     .     /5X,'EXTERNAL PRESSURE . . . . . . . . . . .=',1PG20.13,
     .     /5X,'INITIAL PRESSURE. . . . . . . . . . . .=',1PG20.13/)
 1401 FORMAT(
     .     5X,'INITIAL THERMODYNAMIC EQUILIBRIUM IS SET AT TIME 0'
     .     /5X,'--------------------------------------------------'/)
 1402 FORMAT(
     .     5X,'INITIAL THERMODYNAMIC EQUILIBRIUM IS SET AT INJECTION TIME'
     .     /5X,'----------------------------------------------------------'/)
 1410 FORMAT(
     .     5X,'CHARACTERISTICS OF INITIAL GAZ          ',
     .     /5X,'------------------------------          ',
     .     /5X,'GAMMA AT INITIAL TEMPERATURE. . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13/)
 1420 FORMAT(
     .     5X,'INFLATORS                               ',
     .     /5X,'---------                               ')
 1421 FORMAT(
     .     5X,'NUMBER OF INFLATORS . . . . . . . . . .=',I10/)
 1430 FORMAT(
     .     5X,'INFLATOR NUMBER . . . . . . . . . . . .=',I10,
     .     /15X,'TIME FUNCTION FOR INCOMING TOTAL MASS .=',I10,
     .     /15X,'   or MASS FLUX if IFLU=1 . . . . IFLU =',I10,
     .     /15X,'SCALE FACTOR FOR INCOMING TOTAL MASS  .=',1PG20.13,
     .     /15X,'TIME FUNCTION FOR INCOMIMG GAS TEMP . .=',I10,
     .     /15X,'SCALE FACTOR FOR INCOMIMG GAS TEMP  . .=',1PG20.13,
     .     /15X,'SENSOR NUMBER . . . . . . . . . . . . .=',I10)
 1440 FORMAT(
     .     /15X,'GAZ CHARACTERISTICS                     ',
     .     /15X,'-------------------                     ',
     .     /15X,'GAMMA AT INITIAL TEMPERATURE. . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13)
 1450 FORMAT(
     .     /15X,'JETTING OPTION. . . . . . . . . . . . .=',I10,
     .     /15X,'----------------------------------------')
 1460 FORMAT(
     .     15X,'CONICAL JET . . . . . . . . . . . . . .',
     .     /15X,'NODE NUMBER DEFINING INJECTION CENTER .=',I10,
     .     /15X,'NODE NUMBER DEFINING INJECTION AXIS . .=',I10,
     .     /15X,'JETTING PRESSURE TIME CURVE NUMBER. . .=',I10,
     .     /15X,'JETTING PRESSURE THETA CURVE NUMBER . .=',I10,
     .     /15X,'JETTING PRESSURE DIST. CURVE NUMBER . .=',I10,
     .     /15X,'TIME FUNCTION SCALE FACTOR          . .=',1PG20.13,
     .     /15X,'THETA FUNCTION SCALE FACTOR         . .=',1PG20.13,
     .     /15X,'DIST FUNCTION SCALE FACTOR          . .=',1PG20.13/)
 1461 FORMAT(
     .     15X,'DIHEDRAL JET. . . . . . . . . . . . . .',
     .     /15X,'NODE NUMBER DEFINING INJECTION CENTER .=',I10,
     .     /15X,'NODE NUMBER DEFINING INJECTION AXIS . .=',I10,
     .     /15X,'NODE NUMBER DEFINING BASE LINE. . . . .=',I10,
     .     /15X,'JETTING PRESSURE TIME CURVE NUMBER. . .=',I10,
     .     /15X,'JETTING PRESSURE THETA CURVE NUMBER . .=',I10,
     .     /15X,'JETTING PRESSURE DIST. CURVE NUMBER . .=',I10,
     .     /15X,'TIME FUNCTION SCALE FACTOR          . .=',1PG20.13,
     .     /15X,'THETA FUNCTION SCALE FACTOR         . .=',1PG20.13,
     .     /15X,'DIST FUNCTION SCALE FACTOR          . .=',1PG20.13)
 1470 FORMAT(
     .     /5X,'VENT HOLES AND POROUS FABRIC SURFACES   ',
     .     /5X,'-------------------------------------   ',
     .     /5X,'NUMBER OF VENT HOLES AND POROUS SURFACES . .=',I10,
     .     /5X,'INJECTION TIME TINJ. . . . . . . . . . . . .=',1PG20.13)
 1471 FORMAT(
     .     5X,'VENTING START TIME SHIFT . . . . . . . . . .=',I10,
     .     /5X,'  0 : NO SHIFT',
     .     /5X,'  1 : JETTING FUNCTIONS ARE SHIFTED BY INJECTION TIME',
     .     /5X,'  2 : JETTING AND VENTING FUNCTIONS ARE SHIFTED BY',
     .     /5X,'      INJECTION TIME TINJ',
     .     /5X,'  3 : JETTING AND VENTING FUNCTIONS ARE SHIFTED',
     .     /5X,'      BY TINJ FOR JETTING FUNCTIONS',
     .     /5X,'      BY TINJ+TSTART FOR VENTING FUNCTIONS')
 1472 FORMAT(
     .     / 5X,'VENT HOLE NUMBER. . . . . . . . . . . .=',I10,
     .     /15X,'VENT HOLE SURFACE ID. . . . . . . . . .=',I10)
 1481 FORMAT(15X,'ISENTHALPIC VENTING MODEL ')
 1482 FORMAT(15X,'CHEMKIN MODEL FOR POROSITY : ',
     .     /15X,'VELOCITY VS RELATIVE PRESSURE FUNCTION =',I10,
     .     /15X,'              SCALE FACTOR. . . . . . .=',1PG20.13)
 1484 FORMAT(15X,'GRAEFE POROSITY FORMULATION')
 1485 FORMAT(15X,'ISENTHALPIC VENTING MODEL WITH POSSIBLE FLOW IN')
 1479 FORMAT(
     .     15X,'INITIAL SURFACE . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'AVENT:VENT HOLE SCALE FACTOR. . . . . .=',1PG20.13,
     .     /15X,'BVENT:VENT HOLE SCALE FACTOR IF CONTACT=',1PG20.13,
     .     /15X,'POROSITY FUNCTION / TIME. . . . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / PRESSURE. . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / AREA. . . . . . . .=',I10,
     .     /15X,'POROSITY TIME FUNCTION SCALE FACTOR    =',1PG20.13,
     .     /15X,'POROSITY PRESSURE FUNCTION SCALE FACTOR=',1PG20.13,
     .     /15X,'POROSITY AREA FUNCTION SCALE FACTOR . .=',1PG20.13,
     .     /15X,'POROSITY FUNCTION / TIME(after contact)=',I10,
     .     /15X,'POROSITY FUNCTION / PRESSURE. . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / AREA. . . . . . . .=',I10,
     .     /15X,'POROSITY TIME FUNCTION SCALE FACTOR    =',1PG20.13,
     .     /15X,'POROSITY PRESSURE FUNCTION SCALE FACTOR=',1PG20.13,
     .     /15X,'POROSITY AREA FUNCTION SCALE FACTOR . .=',1PG20.13)
 1480 FORMAT(
     .     15X,'START TIME FOR VENTING TSTART . . . . .=',1PG20.13,
     .     /15X,'RELATIVE PRES. FOR MEMBRANE DEFLATION .=',1PG20.13,
     .     /15X,'       (DPDEF = PDEF - PEXT)            ',
     .     /15X,'TIME DELAY BEFORE MEMBRANE DEFLATION  .=',1PG20.13,
     .     /15X,'TIME DELAY FLAG . . . . . . . . . . . .=',I10,
     .     /15X,'  IF IDTPDEF : 0',
     .     /15X,'       PRESSURE SHOULD BE OVER PDEF DURING',
     .     /15X,'       A CUMULATED DTPDEF TIME'
     .     /15X,'       BEFORE ACTIVATING DEFLATION'
     .     /15X,'  IF IDTPDEF : 1',
     .     /15X,'       DEFLATION START DTPDEF AFTER',
     .     /15X,'       DPDEF HAS BEEN REACHED',
     .     /15X,'END TIME FOR VENTING TSTOP. . . . . . .=',1PG20.13)
 1489 FORMAT(
     .     15X,'AVENT:VENT HOLE AREA. . . . . . . . . .=',1PG20.13,
     .     /15X,'BVENT:VENT HOLE SCALE FACTOR IF CONTACT=',1PG20.13,
     .     /15X,'POROSITY FUNCTION / TIME. . . . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / PRESSURE. . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / AREA. . . . . . . .=',I10,
     .     /15X,'POROSITY TIME FUNCTION SCALE FACTOR    =',1PG20.13,
     .     /15X,'POROSITY PRESSURE FUNCTION SCALE FACTOR=',1PG20.13,
     .     /15X,'POROSITY AREA FUNCTION SCALE FACTOR . .=',1PG20.13,
     .     /15X,'POROSITY FUNCTION / TIME(after contact)=',I10,
     .     /15X,'POROSITY FUNCTION / PRESSURE. . . . . .=',I10,
     .     /15X,'POROSITY FUNCTION / AREA. . . . . . . .=',I10,
     .     /15X,'POROSITY TIME FUNCTION SCALE FACTOR    =',1PG20.13,
     .     /15X,'POROSITY PRESSURE FUNCTION SCALE FACTOR=',1PG20.13,
     .     /15X,'POROSITY AREA FUNCTION SCALE FACTOR . .=',1PG20.13)
      END SUBROUTINE HM_READ_MONVOL_TYPE4
