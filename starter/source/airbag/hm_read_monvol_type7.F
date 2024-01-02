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
Chd|  HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
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
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
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
      SUBROUTINE HM_READ_MONVOL_TYPE7(T_MONVOLN, IPM, IGEO, ITABM1, 
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
#include      "scr17_c.inc"
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
      INTEGER, INTENT(IN) :: LUID, IPM(NPROPMI, *), IGEO(NPROPGI, *), ITABM1(*), NPT(*)
      INTEGER, INTENT(IN) :: NPC(*), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *), PLD(2, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ
      INTEGER :: SURFID
      my_real :: FAC_M, FAC_L, FAC_T, FAC_C, FAC_GEN
      my_real :: SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      LOGICAL :: FOUND
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV
      INTEGER :: MID_INI, IEQUI, ITTF, MID_INISYS
      my_real :: HCONV, PEXT, TI, PINI
      my_real :: MWI, CPAI, CPBI, CPCI, CPDI, CPEI, CPFI, R_IGC1, CPI, RMWI, CVI, GAMAI
      INTEGER :: NJET, NINJ
      INTEGER, DIMENSION(:), ALLOCATABLE :: I_INJ, ISENS, IJET, NJ1, NJ2, NJ3, 
     .     IPT, IPA, IPZ, I_INJSYS
      my_real, DIMENSION(:), ALLOCATABLE :: FPT, FPA, FPZ
      INTEGER :: NVENTHOLES, NPORSURF, NVENT
      CHARACTER(LEN = 40) :: MESS
      CHARACTER(LEN = 20) :: VENTTITLE
      INTEGER :: TITREVENT(20)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPVENT, IFVENT, IDTPDEF, IPORT, IPORP, IPORA, 
     .     IPORT1, IPORP1, IPORA1, IVDP, IBLOCKAGE
      my_real, DIMENSION(:), ALLOCATABLE :: AVENT, BVENT, TVENT, TSTOPE, DPDEF, DTPDEF,
     .     FPORT, FPORP, FPORA, FPORT1, FPORP1, FPORA1, FVDP, CLEAK
      my_real :: TTFIRE
      CHARACTER(LEN = nchartitle) :: TITR1
      my_real :: MI, RHOI, TI2, ESPECI, CPA, CPB, CPC, CPD, CPE, CPF, MW_TMP, RMWG, 
     .     CPG, CVG, GAMA, SHOL
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
!     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('hconv',HCONV, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 2
      CALL HM_GET_FLOATV('scale_t', SCAL_T, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale_p', SCAL_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale_s', SCAL_S, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale_a', SCAL_A, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale_d', SCAL_D, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 3
      CALL HM_GET_INTV('matid', MID_INI, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('mu', AMU, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('pext', PEXT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('t_initial', TI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('iequil', IEQUI, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('ittf', ITTF, IS_AVAILABLE, LSUBMODEL)
!     Injectors
      CALL HM_GET_INTV('nb_jet', NJET, IS_AVAILABLE, LSUBMODEL)
      T_MONVOLN%NJET = NJET
      T_MONVOLN%IVOLU(8) = NJET
      IF (NJET > 0) THEN
         ALLOCATE(T_MONVOLN%IBAGJET(NIBJET, NJET))
         T_MONVOLN%IBAGJET(1:NIBJET, 1:NJET) = 0
         ALLOCATE(T_MONVOLN%RBAGJET(NRBJET, NJET))
         T_MONVOLN%RBAGJET(1:NRBJET, 1:NJET) = ZERO
      ENDIF
      IF (NJET > 0) THEN
         ALLOCATE(I_INJ(NJET), ISENS(NJET), IJET(NJET), NJ1(NJET), NJ2(NJET), NJ3(NJET),
     .        IPT(NJET), IPA(NJET), IPZ(NJET), FPT(NJET), FPA(NJET), FPZ(NJET))
         DO II = 1, NJET
            CALL HM_GET_INT_ARRAY_INDEX('inject_ID', I_INJ(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('sensor', ISENS(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('ijet', IJET(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('node1', NJ1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('node2', NJ2(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('node3', NJ3(II), II, IS_AVAILABLE, LSUBMODEL)
            IF (IJET(II) > 0) THEN
               CALL HM_GET_INT_ARRAY_INDEX('fct_pt', IPT(II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('fct_theta', IPA(II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('fct_delta', IPZ(II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('fscale_pt', FPT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('fscale_ptheta', FPA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('fscale_pdelta', FPZ(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDIF
         ENDDO
      ENDIF
!     Ventholes and porous surfaces
      CALL HM_GET_INTV('nb_vent', NVENTHOLES, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('nb_porous', NPORSURF, IS_AVAILABLE, LSUBMODEL)
      NVENT = NVENTHOLES + NPORSURF
      T_MONVOLN%IVOLU(11) = NVENT
      T_MONVOLN%NVENT = NVENT
      T_MONVOLN%NPORSURF = NPORSURF
      IF (NVENT > 0) THEN
         ALLOCATE(T_MONVOLN%IBAGHOL(NIBHOL, NVENT))
         T_MONVOLN%IBAGHOL(1:NIBHOL, 1:NVENT) = 0
         ALLOCATE(T_MONVOLN%RBAGHOL(NRBHOL, NVENT))
         T_MONVOLN%RBAGHOL(1:NRBHOL, 1:NVENT) = ZERO
      ENDIF
      IF (NVENT > 0) THEN
         ALLOCATE(IPVENT(NVENT), IFVENT(NVENT), AVENT(NVENT), BVENT(NVENT))
         ALLOCATE(TVENT(NVENT), TSTOPE(NVENT), DPDEF(NVENT), DTPDEF(NVENT),
     .        IDTPDEF(NVENT))
         ALLOCATE(IPORT(NVENT), IPORP(NVENT), IPORA(NVENT), FPORT(NVENT),
     .        FPORP(NVENT), FPORA(NVENT))
         ALLOCATE(IPORT1(NVENT), IPORP1(NVENT), IPORA1(NVENT), FPORT1(NVENT),
     .        FPORP1(NVENT), FPORA1(NVENT))
         ALLOCATE(IBLOCKAGE(NVENT), CLEAK(NVENT))
         ALLOCATE(IVDP(NVENT), FVDP(NVENT))
      ENDIF
      IF (NVENTHOLES > 0) THEN
         DO II = 1, NVENTHOLES
            VENTTITLE = ''
            CALL HM_GET_INT_ARRAY_INDEX('surf_IDv', IPVENT(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iform', IFVENT(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Avent', AVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Bvent', BVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_STRING_INDEX('vent_title', VENTTITLE, II, 20, IS_AVAILABLE)
            DO JJ = 1, 20
               TITREVENT(JJ) = IACHAR(VENTTITLE(JJ:JJ))
               T_MONVOLN%IBAGHOL(JJ + 14, II) = TITREVENT(JJ)
            ENDDO
!
            CALL HM_GET_FLOAT_ARRAY_INDEX('tstart', TVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('tstop', TSTOPE(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dpdef', DPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dtpdef', DTPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('idtpdef', IDTPDEF(II), II, IS_AVAILABLE, LSUBMODEL)
!     
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDt', IPORT(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDP', IPORP(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDA', IPORA(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalet', FPORT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)            
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleP', FPORP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleA', FPORA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
!
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDt'", IPORT1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDP'", IPORP1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDA'", IPORA1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX("Fscalet'", FPORT1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX("FscaleP'", FPORP1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX("FscaleA'", FPORA1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            
            IF (IFVENT(II) == 2) THEN
               CALL HM_GET_INT_ARRAY_INDEX('fct_IDvvh', IVDP(II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalevvh', FVDP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ELSE
               IVDP(II) = 0
               FVDP(II) = ZERO
            ENDIF
         ENDDO
      ENDIF
      IF (NPORSURF > 0) THEN
CALL HM_GET_INT_ARRAY_INDEX('', , II, IS_AVAILABLE, LSUBMODEL)
CALL HM_GET_FLOAT_ARRAY_INDEX('', , II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         DO II = 1, NPORSURF
            VENTTITLE = ''
            CALL HM_GET_INT_ARRAY_INDEX('surf_ps', IPVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('iform_ps', IFVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('iblockage_ps', IBLOCKAGE(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_STRING_INDEX('title_ps', VENTTITLE, II, 20, IS_AVAILABLE)        
            DO JJ = 1, 20
               TITREVENT(JJ) = IACHAR(VENTTITLE(JJ:JJ))
               T_MONVOLN%IBAGHOL(JJ + 14, NVENTHOLES + II) = TITREVENT(JJ)
            ENDDO
!
            CALL HM_GET_FLOAT_ARRAY_INDEX('tstart_ps', TVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('tstop_ps', TSTOPE(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dpdef_ps', DPDEF(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dtpdef_ps', DTPDEF(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('idtpdef_ps', IDTPDEF(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            
            CLEAK(NVENTHOLES + II) = ZERO
            AVENT(NVENTHOLES + II) = ZERO
            IPORT(NVENTHOLES + II) = 0
            IPORA(NVENTHOLES + II) = 0
            FPORT(NVENTHOLES + II) = ZERO
            FPORA(NVENTHOLES + II) = ZERO
            IF (IFVENT(NVENTHOLES + II) == 0) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX('cps', CLEAK(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('area_ps', AVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_INT_ARRAY_INDEX('fct_cps', IPORT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('fct_aps', IPORA(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('fscale_cps', FPORT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('fscale_aps', FPORA(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDIF
            IVDP(NVENTHOLES + II) = 0
            FVDP(NVENTHOLES + II) = ZERO
            IF (IFVENT(NVENTHOLES + II) == 2) THEN
               CALL HM_GET_INT_ARRAY_INDEX('fct_IDvps', IVDP(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalevps', FVDP(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDIF
         ENDDO
      ENDIF
C     =====
C     Units
C     =====
      FAC_M = UNITAB%FAC_M(LUID)
      FAC_L = UNITAB%FAC_L(LUID)
      FAC_T = UNITAB%FAC_T(LUID)
      FAC_C = FAC_M / (FAC_L * FAC_T * FAC_T)

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
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 7)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 7)

      IF (ITTF < 0 .OR. ITTF > 3) THEN
         CALL ANCMSG(MSGID = 773, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      END IF
      IF (TI == ZERO) THEN
         TI = TWOHUNDRED95
      ENDIF
!     Find material 
      T_MONVOLN%IVOLU(66) = 0
      FOUND = .FALSE.
      DO II = 1, NUMMAT
         IF (IPM(1, II) == MID_INI .AND. IPM(2, II) == 999) THEN
            MWI = PM(20, II) * FAC_M
            CPAI = PM(21, II) * FAC_L * FAC_L / (FAC_T * FAC_T)
            CPBI = PM(22, II) * FAC_L * FAC_L / (FAC_T * FAC_T)
            CPCI = PM(23, II) * FAC_L * FAC_L / (FAC_T * FAC_T)
            CPDI = PM(24, II) * FAC_L * FAC_L / (FAC_T * FAC_T)
            CPEI = PM(25, II) * FAC_L * FAC_L / (FAC_T * FAC_T)
            CPFI = PM(26, II) * FAC_L * FAC_L / (FAC_T * FAC_T)
            R_IGC1 = PM(27, II)
            CPI = CPAI + CPBI * TI + CPCI * TI * TI + CPDI * TI * TI * TI +
     .           CPEI / (TI * TI) + CPFI * TI * TI * TI * TI
            RMWI = R_IGC1 / MWI
            CVI = CPI - RMWI
            T_MONVOLN%IVOLU(66) = II
            IF (CVI == ZERO) THEN
               CALL ANCMSG(MSGID = 709, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
               GAMAI = ZERO
            ELSE
               GAMAI = CPI / CVI
            END IF
            FOUND = .TRUE. 
            EXIT
         ENDIF
      ENDDO
      IF (.NOT. FOUND) THEN
         CALL ANCMSG(MSGID = 699, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I2 = MID_INI, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF
!     Injectors
      IF (NJET > 0) THEN
         ALLOCATE(I_INJSYS(NJET))
         DO II = 1, NJET
!     Check injector property
            I_INJSYS(II) = 0
            FOUND = .FALSE.
            DO JJ = 1, NUMGEO
               IF (IGEO(1, JJ) == I_INJ(II)) THEN
                  I_INJSYS(II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 723, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = I_INJ(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDDO
      ENDIF
 

C     ==============
C     Default values
C     ==============
      IF (SCAL_T == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('scale_t', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_T = ONE * FAC_GEN
      ENDIF
      IF (SCAL_P == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('scale_p', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_P = ONE * FAC_GEN
      ENDIF
      IF (SCAL_S == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('scale_s', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_S = ONE * FAC_GEN
      ENDIF
      IF (SCAL_A == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('scale_a', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_A = ONE * FAC_GEN
      ENDIF
      IF (SCAL_D == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('scale_d', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SCAL_D = ONE * FAC_GEN
      ENDIF

      IF (AMU == ZERO) AMU = EM02
      IF (PEXT == ZERO) THEN
         PEXT = 101325.D0 * (FAC_L_WORK * FAC_T_WORK * FAC_T_WORK) / FAC_M_WORK
      ENDIF
      
      PINI = PEXT
!     Injectors
      IF (NJET > 0) THEN
         DO II = 1, NJET
            IF (IJET(II) > 0) THEN
               IF (FPT(II) == ZERO) THEN
                  CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_pt', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
                  FPT(II) = ONE * FAC_GEN
               ENDIF
               IF (FPA(II) == ZERO) THEN
                  CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_ptheta', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
                  FPA(II) = ONE * FAC_GEN
               ENDIF
               IF (FPZ(II) == ZERO) THEN
                  CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_pdelta', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
                  FPZ(II) = ONE
               ENDIF
            ELSE
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_pt', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPT(II) = ONE * FAC_GEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_ptheta', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPA(II) = ONE * FAC_GEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_pdelta', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPZ(II) = ONE * FAC_GEN
            ENDIF
         ENDDO
      ENDIF
!     Ventholes
      IF (NVENTHOLES > 0) THEN
         DO II = 1, NVENTHOLES
            IF (IFVENT(II) == 0) IFVENT(II) = 1
            IF (IPVENT(II) == 0) THEN
               BVENT(II) = ZERO
            ENDIF
            IF (FPORT(II)  == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fscalet', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPORT(II) = ONE * FAC_GEN
            ENDIF
            IF (FPORP(II)  == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('FscaleP', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPORP(II) = ONE * FAC_GEN
            ENDIF
            IF (FPORA(II)  == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_aps', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPORA(II) = ONE * FAC_GEN
            ENDIF
            IF (FPORT1(II) == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM("Fscalet'", FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPORT1(II) = ONE * FAC_GEN
            ENDIF
            IF (FPORP1(II) == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM("FscaleP'", FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPORP1(II) = ONE * FAC_GEN
            ENDIF
            IF (FPORA1(II) == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM("FscaleA'", FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               FPORA1(II) = ONE * FAC_GEN
            ENDIF
         ENDDO
      ENDIF
C     =====
C     Store
C     ===== 
      T_MONVOLN%IVOLU(8) = NJET
      IF (IEQUI > 0 .AND. IEQUI /= 1) IEQUI = 1
      T_MONVOLN%IVOLU(15) = IEQUI
      T_MONVOLN%IVOLU(17) = ITTF

      T_MONVOLN%RVOLU(19) = HCONV
      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE / SCAL_S
      T_MONVOLN%RVOLU(29) = ONE / SCAL_A
      T_MONVOLN%RVOLU(30) = ONE / SCAL_D

      T_MONVOLN%RVOLU(31) = PINI
      
      MID_INISYS = T_MONVOLN%IVOLU(66)
      IF (MID_INISYS /= 0) THEN
         MWI = PM(20, MID_INISYS)
         CPAI = PM(21, MID_INISYS)
         CPBI = PM(22, MID_INISYS)
         CPCI = PM(23, MID_INISYS)
         CPDI = PM(24, MID_INISYS)
         CPEI = PM(25, MID_INISYS)
         CPFI = PM(26, MID_INISYS)
         R_IGC1 = PM(27, MID_INISYS)
      ELSE
         MWI = ZERO
         CPAI = ZERO
         CPBI = ZERO
         CPCI = ZERO
         CPDI = ZERO
         CPEI = ZERO
         CPFI = ZERO
         R_IGC1 = ZERO
         CALL ANCMSG(MSGID= 7 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF
      T_MONVOLN%RVOLU(7)=CPAI
      T_MONVOLN%RVOLU(8)=CPBI
      T_MONVOLN%RVOLU(9)=CPCI
      T_MONVOLN%RVOLU(56)=CPDI
      T_MONVOLN%RVOLU(57)=CPEI
      T_MONVOLN%RVOLU(58)=CPFI
      T_MONVOLN%RVOLU(59)=MWI
      CPI = CPAI + CPBI * TI + CPCI * TI * TI + CPDI * TI * TI * TI +
     .     CPEI / (TI * TI) + CPFI * TI * TI * TI * TI
      IF (MWI == ZERO) THEN
         GAMAI = ZERO
      ELSE
         RMWI = R_IGC1 / MWI
         CVI = CPI - RMWI
         GAMAI = CPI / CVI
      ENDIF
      MI = PINI * (VOL + VEPS) / (RMWI * TI)
      T_MONVOLN%RVOLU(1) = GAMAI
      T_MONVOLN%RVOLU(3) = PEXT
      T_MONVOLN%RVOLU(4) = VOL + VEPS
      T_MONVOLN%RVOLU(11) = MI
      T_MONVOLN%RVOLU(12) = PINI
      T_MONVOLN%RVOLU(13) = TI
      T_MONVOLN%RVOLU(14) = RMWI * MI
      T_MONVOLN%RVOLU(17) = VEPS
      T_MONVOLN%RVOLU(20) = MI
      T_MONVOLN%RVOLU(25) = TI
      T_MONVOLN%RVOLU(61) = GAMAI
      RHOI = PINI / (TI * RMWI)
      T_MONVOLN%RVOLU(62) = RHOI
      T_MONVOLN%RVOLU(10) = RMWI
      TI2=TI*TI
      ESPECI=TI*(CPAI+HALF*CPBI*TI+THIRD*CPCI*TI2-RMWI)
      ESPECI=ESPECI+FOURTH*CPDI*TI2*TI2-CPEI/TI+ONE_FIFTH*CPFI*TI2*TI2*TI
      T_MONVOLN%RVOLU(63)= ESPECI+RMWI*TI
      T_MONVOLN%RVOLU(64)= ZERO
      T_MONVOLN%RVOLU(65)= ZERO
      T_MONVOLN%RVOLU(66)= ESPECI     
!     Injectors
      TTFIRE = INFINITY
      DO II = 1, NJET
         T_MONVOLN%IBAGJET(13, II) = I_INJSYS(II)
         T_MONVOLN%RBAGJET(1, II) = ZERO
         T_MONVOLN%RBAGJET(2, II) = GEO(203, I_INJSYS(II))
         T_MONVOLN%RBAGJET(3, II) = GEO(204, I_INJSYS(II))
         T_MONVOLN%RBAGJET(4, II) = GEO(205, I_INJSYS(II))
         T_MONVOLN%RBAGJET(16, II) = GEO(206, I_INJSYS(II))
         T_MONVOLN%RBAGJET(17, II) = GEO(207, I_INJSYS(II))
         T_MONVOLN%RBAGJET(18, II) = GEO(208, I_INJSYS(II))
         T_MONVOLN%RBAGJET(19, II) = GEO(202, I_INJSYS(II))
         T_MONVOLN%RBAGJET(5, II) = ZERO !FMASS
         T_MONVOLN%RBAGJET(6, II) = ZERO !FTEMP
         T_MONVOLN%RBAGJET(12, II) = FPT(II)
         T_MONVOLN%RBAGJET(13, II) = FPA(II)
         T_MONVOLN%RBAGJET(14, II) = FPZ(II)
         T_MONVOLN%IBAGJET(2, II) = 0 !IFLU
         T_MONVOLN%IBAGJET(3, II) = 0 !ITEMP
         T_MONVOLN%IBAGJET(4, II) = ISENS(II)
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
         IF (ISENS(II) > 0) THEN
            FOUND = .FALSE.
            DO JJ = 1, SENSORS%NSENSOR
               IF (ISENS(II) == SENSORS%SENSOR_TAB(JJ)%SENS_ID) THEN
                  T_MONVOLN%IBAGJET(4, II) = JJ
                  IF (SENSORS%SENSOR_TAB(JJ)%TCRIT < TTFIRE) TTFIRE = SENSORS%SENSOR_TAB(JJ)%TCRIT
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 17, ANMODE =ANINFO, MSGTYPE = MSGERROR,
     .              I2 = ISENS(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
         CPA = T_MONVOLN%RBAGJET(2, II)
         CPB = T_MONVOLN%RBAGJET(3, II)
         CPC = T_MONVOLN%RBAGJET(4, II)
         CPD = T_MONVOLN%RBAGJET(16, II)
         CPE = T_MONVOLN%RBAGJET(17, II)
         CPF = T_MONVOLN%RBAGJET(18, II)
         MW_TMP = T_MONVOLN%RBAGJET(19, II)
         RMWG = R_IGC1 / MW_TMP
         CPG = CPA + CPB*TI+CPC*TI*TI+CPD*TI*TI*TI+
     .        CPE/(TI*TI)+CPF*TI*TI*TI*TI
         CVG = CPG - RMWG
         GAMA = CPG / CVG
         T_MONVOLN%RBAGJET(1, II) = RMWG
      ENDDO
      IF (TTFIRE == INFINITY) TTFIRE = ZERO
      T_MONVOLN%RVOLU(49) = TTFIRE
!     Ventholes
      DO II = 1, NVENTHOLES
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
            IF (AVENT(II) == ZERO) THEN
               CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('avent', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
               AVENT(II) = ONE * FAC_GEN
            ENDIF
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
         IF (IVDP(II) /= 0 .AND. FVDP(II) == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_v', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FVDP(II) = ONE * FAC_GEN
         ENDIF
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
         IF (IVDP(II) == 0 .AND. IFVENT(II) /= 2) T_MONVOLN%IBAGHOL(9, II) = 0
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
      
      DO II = 1, NPORSURF
         T_MONVOLN%IBAGHOL(13, NVENTHOLES + II) = 1
         IF (IFVENT(NVENTHOLES + II) == 0) THEN
            IF (CLEAK(NVENTHOLES + II) > ZERO) IPORT(NVENTHOLES + II) = 0
            IF (AVENT(NVENTHOLES + II) > ZERO) IPORA(NVENTHOLES + II) = 0
            IPVENT(NVENTHOLES + II) = 0
            IBLOCKAGE(NVENTHOLES + II) = 0
         ENDIF
         IF (FPORT(NVENTHOLES + II)  == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_cps', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FPORT(NVENTHOLES + II) = ONE * FAC_GEN
         ENDIF
         IF (FPORA(NVENTHOLES + II)  == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_aps', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FPORA(NVENTHOLES + II) = ONE * FAC_GEN
         ENDIF
         T_MONVOLN%RBAGHOL(7, NVENTHOLES + II) = FPORT(NVENTHOLES + II)
         T_MONVOLN%RBAGHOL(9, NVENTHOLES + II) = FPORA(NVENTHOLES + II)
C     
         T_MONVOLN%IBAGHOL(1, NVENTHOLES + II) = 0
         T_MONVOLN%IBAGHOL(10, NVENTHOLES + II) = IFVENT(NVENTHOLES + II)
         T_MONVOLN%IBAGHOL(11, NVENTHOLES + II) = IDTPDEF(NVENTHOLES + II)
         T_MONVOLN%IBAGHOL(12, NVENTHOLES + II) = 0
         T_MONVOLN%IBAGHOL(14, NVENTHOLES + II) = IBLOCKAGE(NVENTHOLES + II)
C     
         IF (IPVENT(NVENTHOLES + II) == 0) THEN
            T_MONVOLN%IBAGHOL(2, NVENTHOLES + II) = 0
         ELSE
            T_MONVOLN%IBAGHOL(2, NVENTHOLES + II) = 0
            DO JJ = 1, NSURF
               IF (IPVENT(NVENTHOLES + II) == IGRSURF(JJ)%ID) THEN
                  T_MONVOLN%IBAGHOL(2, NVENTHOLES + II) = JJ
               ENDIF
            ENDDO    
            IF (T_MONVOLN%IBAGHOL(2, NVENTHOLES + II) == 0) THEN
               CALL ANCMSG(MSGID = 532, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPVENT(NVENTHOLES + II))
            ELSEIF (IGRSURF(T_MONVOLN%IBAGHOL(2, NVENTHOLES + II))%ISH4N3N == 0) THEN
               CALL ANCMSG(MSGID = 330, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
C     
         IF (DPDEF(NVENTHOLES + II) == ZERO .AND. DTPDEF(NVENTHOLES + II) == ZERO .AND. 
     .        TVENT(NVENTHOLES + II) == ZERO) THEN
            T_MONVOLN%IBAGHOL(1, NVENTHOLES + II) = 1
         ENDIF
         T_MONVOLN%RBAGHOL(1, NVENTHOLES + II) = DPDEF(NVENTHOLES + II)
         T_MONVOLN%RBAGHOL(2, NVENTHOLES + II) = AVENT(NVENTHOLES + II)
         T_MONVOLN%RBAGHOL(3, NVENTHOLES + II) = TVENT(NVENTHOLES + II)
         T_MONVOLN%RBAGHOL(4, NVENTHOLES + II) = DTPDEF(NVENTHOLES + II)
         T_MONVOLN%RBAGHOL(6, NVENTHOLES + II) = CLEAK(NVENTHOLES + II)
         IF (IVDP(NVENTHOLES + II) /= 0 .AND. FVDP(NVENTHOLES + II) == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('fscale_v', FAC_GEN, II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FVDP = ONE * FAC_GEN
         ENDIF
         T_MONVOLN%RBAGHOL(13, NVENTHOLES + II)=FVDP(NVENTHOLES + II)
         IF (TSTOPE(NVENTHOLES + II) == ZERO) TSTOPE(NVENTHOLES + II) = INFINITY
         T_MONVOLN%RBAGHOL(14, NVENTHOLES + II) = TSTOPE(NVENTHOLES + II)
C     
         T_MONVOLN%IBAGHOL(3, NVENTHOLES + II) = -1
         T_MONVOLN%IBAGHOL(5, NVENTHOLES + II) = -1
         T_MONVOLN%IBAGHOL(9, NVENTHOLES + II) = -1
C     
         DO JJ = 1, NFUNCT
            IF (IPORT(NVENTHOLES + II) == NPC(JJ))  THEN
               T_MONVOLN%IBAGHOL(3, NVENTHOLES + II) = JJ
            ENDIF
            IF (IPORA(NVENTHOLES + II) == NPC(JJ)) THEN
               T_MONVOLN%IBAGHOL(5, NVENTHOLES + II) = JJ
            ENDIF
            IF (IVDP(NVENTHOLES + II) == NPC(JJ)) THEN
               T_MONVOLN%IBAGHOL(9, NVENTHOLES + II) = JJ
            ENDIF
         ENDDO
         IF (IPORT(NVENTHOLES + II) == 0)  T_MONVOLN%IBAGHOL(3, NVENTHOLES + II) = 0
         IF (IPORA(NVENTHOLES + II) == 0)  T_MONVOLN%IBAGHOL(5, NVENTHOLES + II) = 0
         IF (IVDP(NVENTHOLES + II) == 0 .AND. IFVENT(NVENTHOLES + II) /= 2) THEN
            T_MONVOLN%IBAGHOL(9, NVENTHOLES + II) = 0
         ENDIF
        
         IF (T_MONVOLN%IBAGHOL(3, NVENTHOLES + II) == -1) THEN
            T_MONVOLN%IBAGHOL(3, NVENTHOLES + II) =0
            CALL ANCMSG(MSGID = 331, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORT(NVENTHOLES + II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(5, NVENTHOLES + II) == -1) THEN
            T_MONVOLN%IBAGHOL(5, NVENTHOLES + II) = 0
            CALL ANCMSG(MSGID = 333, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORA(NVENTHOLES + II))
         ENDIF
         IF (T_MONVOLN%IBAGHOL(9, NVENTHOLES + II) == -1) THEN
            T_MONVOLN%IBAGHOL(9, NVENTHOLES + II) = 0
            CALL ANCMSG(MSGID = 518, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IVDP(NVENTHOLES + II))
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
      WRITE(IOUT, 1006) HCONV
      WRITE(IOUT, 1002) SA, SV, VOL
      WRITE(IOUT,1395) MID_INI
      WRITE(IOUT,1400) AMU, TI, PEXT, PINI
      IF (IEQUI == 0) THEN
         WRITE(IOUT,1401)
      ENDIF
      IF (IEQUI > 0) THEN
         WRITE(IOUT,1402)
      ENDIF
      WRITE(IOUT,1415) MWI, GAMAI, CPAI, CPBI, CPCI, CPDI, CPEI, CPFI
      WRITE(IOUT,1425)
      WRITE(IOUT,1426) NJET
      DO II = 1, NJET
         WRITE(IOUT,1432) II,
     .        IGEO(1, I_INJSYS(II)), ISENS(II)
         CPA = T_MONVOLN%RBAGJET(2, II)
         CPB = T_MONVOLN%RBAGJET(3, II)
         CPC = T_MONVOLN%RBAGJET(4, II)
         CPD = T_MONVOLN%RBAGJET(16, II)
         CPE = T_MONVOLN%RBAGJET(17, II)
         CPF = T_MONVOLN%RBAGJET(18, II)
         MW_TMP = T_MONVOLN%RBAGJET(19, II)
         RMWG = R_IGC1 / MW_TMP
         CPG = CPA + CPB*TI+CPC*TI*TI+CPD*TI*TI*TI+
     .        CPE/(TI*TI)+CPF*TI*TI*TI*TI
         CVG = CPG - RMWG
         GAMA = CPG / CVG
         WRITE(IOUT,1442) GAMA, MW_TMP, CPA, CPB, CPC,
     .        CPD, CPE, CPF
         IF (NJ1(II) == 0) THEN
            WRITE(IOUT, 1455) 0
         ELSE
            WRITE(IOUT, 1455) 1
            IF (NJ3(II) == 0) THEN
               WRITE(IOUT, 1460) NJ1(II), NJ2(II), 
     .              IPT(II), IPA(II), IPZ(II), FPT(II), FPA(II), FPZ(II)
               T_MONVOLN%IBAGJET(7, II) = T_MONVOLN%IBAGJET(5, II)
            ELSE
               WRITE(IOUT, 1461) NJ1(II), NJ2(II), NJ3(II),
     .              IPT(II), IPA(II), IPZ(II),
     .              FPT(II), FPA(II) ,FPZ(II)
            ENDIF
         ENDIF
      ENDDO
      WRITE(IOUT,1470) NVENT, TTFIRE
      IF (NVENT > 0) THEN
         WRITE(IOUT,1471) ITTF
      ENDIF
      DO II = 1, NVENT
         IF (T_MONVOLN%IBAGHOL(13, II) == 0) THEN
            TITR1='VENT HOLE SURFACE'
            WRITE(IOUT,1472) II,IPVENT(II)
         ELSE
            TITR1='POROUS SURFACE'
            WRITE(IOUT,1473) II,IPVENT(II)
         ENDIF
         DO JJ = 1, 20
            TITREVENT(JJ) = T_MONVOLN%IBAGHOL(JJ + 14, II)
         ENDDO
         WRITE(IOUT,1476) VENTTITLE
         IF (IPVENT(II) == 0 .AND. AVENT(II) == ZERO) THEN
            CALL ANCMSG(MSGID=1019, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .           I1=T_MONVOLN%ID,I2=II, C1=T_MONVOLN%TITLE,C2=TITR1)
         ENDIF
         IF(IFVENT(II) <= 1)WRITE(IOUT,1481)
         IF(IFVENT(II)==2) THEN
            WRITE(IOUT,1482)IVDP(II),FVDP(II)
         ENDIF
         IF(IFVENT(II)==3) WRITE(IOUT,1484)
         IF(IFVENT(II)==4) WRITE(IOUT,1485)
         SHOL = ZERO
         IF (IPVENT(II) /= 0) THEN
            CALL MONVOL_CHECK_VENTHOLE_SURF(IPRI, T_MONVOLN, IGRSURF, II, SHOL, X, IXC, IXTG)
            T_MONVOLN%RBAGHOL(15, II) = SHOL
            IF (T_MONVOLN%IBAGHOL(13, II) == 0) THEN
               WRITE(IOUT,1479) 
     .              SHOL,AVENT(II),BVENT(II),
     .              IPORT(II),IPORP(II),IPORA(II),FPORT(II),FPORP(II),FPORA(II),
     .              IPORT1(II),IPORP1(II),IPORA1(II),FPORT1(II),FPORP1(II),FPORA1(II)
               WRITE(IOUT,1480) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
            ELSE
               WRITE(IOUT,1579) SHOL,IBLOCKAGE(II)
               WRITE(IOUT,1580) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
            ENDIF
         ELSE
            IF (T_MONVOLN%IBAGHOL(13, II) == 0) THEN
               WRITE(IOUT,1489)
     .              AVENT(II),BVENT(II),
     .              IPORT(II),IPORP(II),IPORA(II),FPORT(II),FPORP(II),FPORA(II),
     .              IPORT1(II),IPORP1(II),IPORA1(II),FPORT1(II),FPORP1(II),FPORA1(II)
               WRITE(IOUT,1480) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
            ELSE
               CLEAK =T_MONVOLN%RBAGHOL(6, II)
               WRITE(IOUT,1585) CLEAK(II),AVENT(II),IPORT(II),FPORT(II),IPORA(II),FPORA(II)
               WRITE(IOUT,1580) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
            ENDIF
         ENDIF
      ENDDO
C     ===================
C     Memory deallocation
C     ===================
      IF (ALLOCATED(I_INJ)) DEALLOCATE(I_INJ)
      IF (ALLOCATED(ISENS)) DEALLOCATE(ISENS)
      IF (ALLOCATED(IJET)) DEALLOCATE(IJET)
      IF (ALLOCATED(NJ1)) DEALLOCATE(NJ1)
      IF (ALLOCATED(NJ2)) DEALLOCATE(NJ2)
      IF (ALLOCATED(NJ3)) DEALLOCATE(NJ3)
      IF (ALLOCATED(IPT)) DEALLOCATE(IPT)
      IF (ALLOCATED(IPA)) DEALLOCATE(IPA)     
      IF (ALLOCATED(IPZ)) DEALLOCATE(IPZ)     
      IF (ALLOCATED(FPT)) DEALLOCATE(FPT)     
      IF (ALLOCATED(FPA)) DEALLOCATE(FPA)     
      IF (ALLOCATED(FPZ)) DEALLOCATE(FPZ)     
      IF (ALLOCATED(I_INJSYS)) DEALLOCATE(I_INJSYS)  
      IF (NVENT > 0) THEN
         DEALLOCATE(IPVENT, IFVENT, AVENT, BVENT)
         DEALLOCATE(TVENT, TSTOPE, DPDEF, DTPDEF,
     .        IDTPDEF)
         DEALLOCATE(IPORT, IPORP, IPORA, FPORT,
     .        FPORP, FPORA)
         DEALLOCATE(IPORT1, IPORP1, IPORA1, FPORT1,
     .        FPORP1, FPORA1)
         DEALLOCATE(IBLOCKAGE, CLEAK)
         DEALLOCATE(IVDP, FVDP)
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
 1006 FORMAT( 5X,'GLOBAL HEAT TRANSFER COEFFICIENT. . . .=',1PG20.13)
 1395 FORMAT(
     .     /5X,'INITIAL GAS . . . . . . . . . . . . . .=',I10)
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
 1415 FORMAT(
     .     5X,'CHARACTERISTICS OF INITIAL GAZ          ',
     .     /5X,'------------------------------          ',
     .     /5X,'MOLECULAR WEIGHT',
     .     /5X,'       AT INITIAL TEMPERATURE . . . . .=',1PG20.13,
     .     /5X,'GAMMA  AT INITIAL TEMPERATURE . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPD . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPE . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPF . . . . . . . . . . . .=',1PG20.13/)
 1425 FORMAT(
     .     /5X,'INJECTORS                               ',
     .     /5X,'---------                               ')
 1426 FORMAT(
     .     5X,'NUMBER OF INJECTORS . . . . . . . . . .=',I10/)
 1432 FORMAT(
     .     5X,'INJECTOR. . . . . . . . . . . . . . . .=',I10,
     .     /10X,'INJECTOR NUMBER . . . . . . . . . . . .=',I10,
     .     /10X,'SENSOR NUMBER . . . . . . . . . . . . .=',I10)
 1442 FORMAT(
     .     /15X,'MIXTURE CHARACTERISTICS                 ',
     .     /15X,'AT BEGINNING OF INJECTION               ',
     .     /15X,'-------------------------               ',
     .     /15X,'GAMMA . . . . . . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'MOLECULAR WEIGHT. . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPD . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPE . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPF . . . . . . . . . . . .=',1PG20.13)
 1455 FORMAT(
     .     /10X,'JETTING OPTION. . . . . . . . . . . . .=',I10,
     .     /10X,'----------------------------------------')
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
 1473 FORMAT(
     .     / 5X,'POROUS SURFACE NUMBER . . . . . . . . .=',I10,
     .     /15X,'POROUS SURFACE ID . . . . . . . . . . .=',I10)
 1476 FORMAT(
     .     15X,'TITLE . . . . . . . . . . . . . . . . .=',1X,A20)
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
 1579 FORMAT(
     .     15X,'INITIAL SURFACE . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'BLOCKAGE IF CONTACT . . . . . . . . . .=',I10,
     .     /15X,'   =0 NO  =1 YES')
 1580 FORMAT(
     .     15X,'START TIME FOR VENTING TSTART . . . . .=',1PG20.13,
     .     /15X,'RELATIVE PRESSURE TO INITIATE LEAKAGE .=',1PG20.13,
     .     /15X,'       (DPDEF = PDEF - PEXT)            ',
     .     /15X,'TIME DELAY BEFORE LEAKAGE . . . . . . .=',1PG20.13,
     .     /15X,'TIME DELAY FLAG . . . . . . . . . . . .=',I10,
     .     /15X,'  IF IDTPDEF = 0',
     .     /15X,'       PRESSURE SHOULD BE OVER PDEF DURING',
     .     /15X,'       A CUMULATED DTPDEF TIME '
     .     /15X,'       BEFORE INITIATING LEAKAGE'
     .     /15X,'  IF IDTPDEF = 1',
     .     /15X,'       DEFLATION STARTS DTPDEF AFTER',
     .     /15X,'       DPDEF HAS BEEN REACHED',
     .     /15X,'END TIME FOR VENTING TSTOP. . . . . . .=',1PG20.13)
 1585 FORMAT(
     .     15X,'LEAKAGE COEFFICIENT . . . . . . . . . .=',1PG20.13,
     .     /15X,'LEAKAGE AREA. . . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'LEAKAGE COEFFICIENT TIME FUNCTION . . .=',I10,
     .     /15X,'        SCALE FACTOR. . . . . . . . . .=',1PG20.13,
     .     /15X,'LEAKAGE AREA TIME FUNCTION. . . . . . .=',I10,
     .     /15X,'        SCALE FACTOR. . . . . . . . . .=',1PG20.13)
      END SUBROUTINE HM_READ_MONVOL_TYPE7
