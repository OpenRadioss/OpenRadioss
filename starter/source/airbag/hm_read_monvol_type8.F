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
Chd|  HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
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
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MONVOL_TYPE8(T_MONVOLN, IPM, IGEO, ITABM1, 
     .     SENSOR_TAB, NPT, PLD, IFRAME, XFRAME, IGRBRIC,
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
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: LUID, IPM(NPROPMI, *), IGEO(NPROPGI, *), ITABM1(*), IFRAME(LISKN,*), NPT(*)
      INTEGER, INTENT(IN) :: NPC(*), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *),
     .     XFRAME(NXFRAME, *), PLD(2, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE (GROUP_)  , DIMENSION(NGRBRIC), INTENT(IN) :: IGRBRIC
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ
      INTEGER :: SURFID, SURFID_INT,SURFID_INJ
      my_real :: FAC_M, FAC_L, FAC_T, FAC_C, FAC_GEN
      my_real :: SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      LOGICAL :: FOUND
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV
      INTEGER :: MID_INI, IEQUI, ITTF, MID_INISYS
      my_real :: HCONV, PEXT, TI, PINI
      my_real :: MWI, CPAI, CPBI, CPCI, CPDI, CPEI, CPFI, R_IGC1, CPI, RMWI, CVI, GAMAI
      INTEGER :: NJET
      INTEGER, DIMENSION(:), ALLOCATABLE :: I_INJ, ISENS, IJET, NJ1, NJ2, NJ3, 
     .     IPT, IPA, IPZ, I_INJSYS,IVEL
      my_real, DIMENSION(:), ALLOCATABLE :: FPT, FPA, FPZ,FVEL
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
      LOGICAL lFOUND
      INTEGER KFRAME, IMESH_ALL, AUTO_UP_SWITCH, NBX, NBY, NBZ, IBRIC
      INTEGER IREF, IGMERG, ILVOUT, NLAYER, NFACMAX, NPPMAX, IFVANI
      INTEGER I1,I2,I3, I4, IGBRIC, NBRIC, NN, ISH34
      my_real TSWITCH, PARAM, LX, LY, LZ, CNMERG, QA, QB, PTOLE,HMIN, CFL_COEF, DTMIN, SJET, CGMERG
      my_real NX, NY, NZ, XX, YY, ZZ, X13, Y13, Z13, X24, Y24, Z24
      my_real ORIGX, ORIGY, ORIGZ, DIR2X, DIR2Y, DIR2Z, DIR, DS, DIRX, DIRY, DIRZ
      INTEGER :: KMESH, ID_DT_OPTION
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
      lFOUND=.FALSE.
      ID_DT_OPTION=2
C     =======
C     Reading
C     =======
!     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Hconv',HCONV, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 2
      CALL HM_GET_FLOATV('Ascalet', SCAL_T, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleP', SCAL_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleS', SCAL_S, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleA', SCAL_A, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleD', SCAL_D, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 3
      CALL HM_GET_INTV('mat_ID', MID_INI, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Pext', PEXT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T0', TI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Iequil', IEQUI, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ittf', ITTF, IS_AVAILABLE, LSUBMODEL)
!     Injectors
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
         ALLOCATE(I_INJ(NJET), ISENS(NJET), IJET(NJET), NJ1(NJET), NJ2(NJET), NJ3(NJET))
         ALLOCATE(IPT(NJET), IPA(NJET), IPZ(NJET), FPT(NJET), FPA(NJET), FPZ(NJET))
         ALLOCATE(IVEL(NJET),FVEL(NJET))
         IPT(:)=0
         IPA(:)=0
         IPZ(:)=0
         FPT(:)=ZERO
         FPA(:)=ZERO
         FPZ(:)=ZERO 
         IVEL(:)=0
         FVEL(:)=ZERO                
         DO II = 1, NJET
            CALL HM_GET_INT_ARRAY_INDEX('inject_ID', I_INJ(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('sens_ID', ISENS(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('surf_IDinj', IJET(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDvel', IVEL(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalevel', FVEL(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)               
         ENDDO                                                          
      ENDIF
!     Ventholes and porous surfaces
      CALL HM_GET_INTV('Nvent', NVENTHOLES, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nporsurf', NPORSURF, IS_AVAILABLE, LSUBMODEL)

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
            IF (IFVENT(II) == 0) IFVENT(II) = 1
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tstart_arr_X', TVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tstop_arr_X', TSTOPE(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('ABG_dPdef', DPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('ABG_dtPdef', DTPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('I_dtPdef_arr', IDTPDEF(II), II, IS_AVAILABLE, LSUBMODEL)
            
            VENTTITLE = VENTTITLE(1:LEN_TRIM(VENTTITLE))
            DO JJ = 1, 20
               TITREVENT(JJ) = IACHAR(VENTTITLE(JJ:JJ))
               T_MONVOLN%IBAGHOL(JJ + 14, II) = TITREVENT(JJ)
            ENDDO
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDt', IPORT(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDP', IPORP(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDA', IPORA(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalet', FPORT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleP', FPORP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleA', FPORA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)

            CALL HM_GET_INT_ARRAY_INDEX("fct_IDt'", IPORT1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDP'", IPORP1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX("fct_IDA'", IPORA1(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX("Fscalet'", FPORT1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX("FscaleP'", FPORP1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX("FscaleA'", FPORA1(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)

            IF (IFVENT(II) == 2) THEN
               CALL HM_GET_INT_ARRAY_INDEX('fct_IDV', IVDP(II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleV', FVDP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ELSE
               IVDP(II) = 0
               FVDP(II) = ZERO
            ENDIF
         ENDDO
      ENDIF
      IF (NPORSURF > 0) THEN
         DO II = 1, NPORSURF
            VENTTITLE = ''
            CALL HM_GET_INT_ARRAY_INDEX('surf_IDps', IPVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iformps', IFVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iblockage', IBLOCKAGE(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_STRING_INDEX('surface_title', VENTTITLE, II, 20, IS_AVAILABLE)

            VENTTITLE = VENTTITLE(1:LEN_TRIM(VENTTITLE))
            DO JJ = 1, 20
               TITREVENT(JJ) = IACHAR(VENTTITLE(JJ:JJ))
               T_MONVOLN%IBAGHOL(JJ + 14, NVENTHOLES + II) = TITREVENT(JJ)
            ENDDO
            
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tstart_arr_Y', TVENT(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tstop_arr_Y', TSTOPE(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('ABG_dPdef_ps', DPDEF(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('ABG_dtPdef_ps', DTPDEF(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('I_dtPdef_arr_ps', IDTPDEF(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)

           
            CLEAK(NVENTHOLES + II) = ZERO
            AVENT(NVENTHOLES + II) = ZERO
            IPORT(NVENTHOLES + II) = 0
            IPORA(NVENTHOLES + II) = 0
            FPORT(NVENTHOLES + II) = ZERO
            FPORA(NVENTHOLES + II) = ZERO
            IF (IFVENT(NVENTHOLES + II) == 0) IFVENT(NVENTHOLES + II) = 1
            IF (IFVENT(NVENTHOLES + II) == 3) IFVENT(NVENTHOLES + II) = 5
            IVDP(NVENTHOLES + II) = 0
            FVDP(NVENTHOLES + II) = ZERO
            IF (IFVENT(NVENTHOLES + II) == 2) THEN
               CALL HM_GET_INT_ARRAY_INDEX('fct_IDv', IVDP(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalev', FVDP(NVENTHOLES + II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            ENDIF
         ENDDO
      ENDIF
      !Specific lines to Iyp=8 (FVMBAG1)
      IMESH_ALL = 0
      CALL HM_GET_INTV('frame_ID', KFRAME, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('kmesh', KMESH, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Tswitch', TSWITCH, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Iswitch', AUTO_UP_SWITCH, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Pswitch', PARAM, IS_AVAILABLE, LSUBMODEL, UNITAB)
     
      CALL HM_GET_FLOATV('L1', LX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('L2', LY, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('L3', LZ, IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_INTV('Nb1', NBX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nb2', NBY, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nb3', NBZ, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('grbric_ID', IBRIC, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('surf_IDin', SURFID_INT, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Iref', IREF, IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_INTV('Igmerg', IGMERG, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Cgmerg', CGMERG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Cnmerg', CNMERG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Ptole', PTOLE, IS_AVAILABLE, LSUBMODEL, UNITAB)
     
      CALL HM_GET_FLOATV('qa', QA, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('qb', QB, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Hmin', HMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dtsca', CFL_COEF, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dtmin', DTMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
     
      CALL HM_GET_INTV('Ilvout', ILVOUT, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nlayer', NLAYER, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nfacmax', NFACMAX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nppmax', NPPMAX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ifvani', IFVANI, IS_AVAILABLE, LSUBMODEL)

      lFOUND = .FALSE.
      IF (KFRAME == 0)THEN
         JJ = 1
      ELSE
         DO II = 1, NUMFRAM
            IF (IFRAME(4, II + 1) == KFRAME) THEN
               JJ = II + 1
               lFOUND = .TRUE.
            ENDIF
         ENDDO
        IF (.NOT. lFOUND) THEN
           CALL ANCMSG(MSGID = 853, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .          I2 = KFRAME, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
        ENDIF
      ENDIF      
      DIRX = XFRAME(7, JJ)
      DIRY = XFRAME(8, JJ)
      DIRZ = XFRAME(9, JJ)
      DIR2X = XFRAME(1, JJ)
      DIR2Y = XFRAME(2, JJ)
      DIR2Z = XFRAME(3, JJ)
      ORIGX = XFRAME(10, JJ)
      ORIGY = XFRAME(11, JJ)
      ORIGZ = XFRAME(12, JJ)                                                                      
 
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
      T_MONVOLN%EXT_SURFID = 0
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
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 8)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 8)
     
      IF (ITTF < 0 .OR. ITTF > 3) THEN
         CALL ANCMSG(MSGID = 773, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF
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
      
           
      T_MONVOLN%IVOLU(61)=0
      IF(IBRIC>0) THEN
         DO II=1,NGRBRIC
            IF (IBRIC==IGRBRIC(II)%ID) THEN
               T_MONVOLN%IVOLU(61)=II
               EXIT
            ENDIF
         ENDDO
      ENDIF   

C     internal surface check      
      T_MONVOLN%IVOLU(67)=0
      T_MONVOLN%INT_SURFID = 0
      IF(SURFID_INT>0) THEN
         DO II=1,NSURF
            IF (SURFID_INT==IGRSURF(II)%ID) THEN
               T_MONVOLN%IVOLU(67) = II
               T_MONVOLN%INT_SURFID = II
               EXIT
            ENDIF
         ENDDO
      ENDIF                                                              

      NBRIC=0
      IF(T_MONVOLN%IVOLU(61)/=0) NBRIC=IGRBRIC(T_MONVOLN%IVOLU(61))%ID
      SURFID_INT=0
      IF(T_MONVOLN%INT_SURFID/=0) SURFID_INT=IGRSURF(T_MONVOLN%INT_SURFID)%ID
                      
   !-----------------------------------------------   
 

C     ==============
C     Default values
C     ==============
      IF (SCAL_T == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('Ascalet', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)      
         SCAL_T = ONE
      ENDIF
      IF (SCAL_P == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleP', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)      
         SCAL_P = ONE
      ENDIF
      IF (SCAL_S == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleS', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)      
         SCAL_S = ONE
      ENDIF
      IF (SCAL_A == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleA', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)      
         SCAL_A = ONE
      ENDIF
      IF (SCAL_D == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleD', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)      
         SCAL_D = ONE
      ENDIF

      AMU = EM02
      
      IF (PEXT == ZERO) THEN
         PEXT = 101325.D0 * (FAC_L_WORK * FAC_T_WORK * FAC_T_WORK) / FAC_M_WORK
      ELSE
         CALL HM_GET_FLOATV_DIM('Pext', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)    
         PEXT = PEXT * FAC_GEN
      ENDIF

      CALL HM_GET_FLOATV_DIM('', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)      
      PINI = PEXT
!     Injectors
      IF (NJET > 0) THEN
         DO II = 1, NJET
            FPT(II) = ONE * FAC_C
            FPA(II) = ONE * FAC_C
            FPZ(II) = ONE * FAC_C
         ENDDO
      ENDIF
!     Ventholes
      IF (NVENTHOLES > 0) THEN
         DO II = 1, NVENTHOLES
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
      ENDIF
!     
     
      DO II=1,NJET
        T_MONVOLN%IBAGJET(14,II) = 0
        lFOUND=.FALSE.
        DO JJ=1,NSURF
           IF (IJET(II)==IGRSURF(JJ)%ID) THEN
              T_MONVOLN%IBAGJET(14, II) = JJ
              lFOUND=.TRUE.
              EXIT
           ENDIF
        ENDDO
        IJET(II)=0
        IF(.NOT.lFOUND)CALL ANCMSG(MSGID=847,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=IJET(II),I1=T_MONVOLN%ID,C1=T_MONVOLN%TITLE)
        DO JJ=1,NFUNCT
           IF (IVEL(II)==NPC(JJ)) T_MONVOLN%IBAGJET(11, II)=JJ
        ENDDO
        IF (FVEL(II) == ZERO)  FVEL(II) = ONE 
      ENDDO 

      IF(TSWITCH == ZERO) TSWITCH=EP30
      IF (KMESH == 0) KMESH=14
      IF (IBRIC /= 0) THEN      !IF IBRIC is not 0 and KMESH has been set to 2, KMESH is ignored
         KMESH = 1
      ENDIF
      IF (NBX==0) NBX=1
      IF (NBY==0) NBY=1
      IF (NBZ==0) NBZ=1
                   
       
      IF (IGMERG==0) IGMERG=1                            
      IF (CGMERG==ZERO) CGMERG=EM01     
      IF (CNMERG==ZERO) CNMERG=EM04    
      IF (PTOLE==ZERO) PTOLE=EM5 
      IF (NLAYER ==0) NLAYER =10
      IF (NFACMAX==0) NFACMAX=20
      IF (NPPMAX ==0) NPPMAX =20            

      IF(CFL_COEF/=ZERO .AND. DTMIN/=ZERO)ID_DT_OPTION=1
      IF(CFL_COEF==ZERO)CFL_COEF=ZEP9        
      IF(DTMIN==ZERO)DTMIN=EM20      

         !------------------------------------------------------!  
C     =====
C     Store
C     ===== 
      T_MONVOLN%IMESH_ALL = IMESH_ALL
      T_MONVOLN%IVOLU(8) = NJET
      IF (IEQUI > 0 .AND. IEQUI /= 1) IEQUI = 1
      T_MONVOLN%IVOLU(15) = IEQUI
      T_MONVOLN%IVOLU(17) = ITTF
      T_MONVOLN%IVOLU(27) = ID_DT_OPTION      
      T_MONVOLN%IVOLU(30) = KMESH
      T_MONVOLN%IVOLU(35)=NBX*NBY
      T_MONVOLN%IVOLU(37) = 0
      T_MONVOLN%IVOLU(40)=NLAYER
      T_MONVOLN%IVOLU(41)=NFACMAX
      T_MONVOLN%IVOLU(42)=NPPMAX
      T_MONVOLN%IVOLU(44)=ILVOUT
      T_MONVOLN%IVOLU(54)=NBX
      T_MONVOLN%IVOLU(55)=NBY
      T_MONVOLN%IVOLU(59)=IREF
      T_MONVOLN%IVOLU(60)=IGMERG 
      T_MONVOLN%IVOLU(65)=NBZ
      T_MONVOLN%IVOLU(74) = AUTO_UP_SWITCH !Parameter for autoswitching to uniform pressure airbag

      T_MONVOLN%RVOLU(19) = HCONV
      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE / SCAL_S
      T_MONVOLN%RVOLU(29) = ONE / SCAL_A
      T_MONVOLN%RVOLU(30) = ONE / SCAL_D
      T_MONVOLN%RVOLU(31)=CGMERG             
      T_MONVOLN%RVOLU(34)=CNMERG             
      T_MONVOLN%RVOLU(35)=DIRX          
      T_MONVOLN%RVOLU(36)=DIRY          
      T_MONVOLN%RVOLU(37)=DIRZ          
      T_MONVOLN%RVOLU(38)=DIR2X         
      T_MONVOLN%RVOLU(39)=DIR2Y         
      T_MONVOLN%RVOLU(40)=DIR2Z         
      T_MONVOLN%RVOLU(41)=ORIGX         
      T_MONVOLN%RVOLU(42)=ORIGY         
      T_MONVOLN%RVOLU(43)=ORIGZ         
      T_MONVOLN%RVOLU(44)=LX            
      T_MONVOLN%RVOLU(45)=LY            
      T_MONVOLN%RVOLU(46)=QA
      T_MONVOLN%RVOLU(47)=QB
      T_MONVOLN%RVOLU(50)=PTOLE   
      T_MONVOLN%RVOLU(51)=HMIN
      T_MONVOLN%RVOLU(53)=LZ          
      T_MONVOLN%RVOLU(70) = TSWITCH
      T_MONVOLN%RVOLU(71) = CFL_COEF
      T_MONVOLN%RVOLU(72) = DTMIN
      T_MONVOLN%RVOLU(73) = PARAM 

                     
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
         !------------------------------------------------------!
         T_MONVOLN%RBAGJET(15,II)=FVEL(II)
         !------------------------------------------------------!
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
            DO JJ = 1, NSENSOR
               IF (ISENS(II) == SENSOR_TAB(JJ)%SENS_ID) THEN
                  T_MONVOLN%IBAGJET(4, II) = JJ
                  IF (SENSOR_TAB(JJ)%TCRIT < TTFIRE) TTFIRE = SENSOR_TAB(JJ)%TCRIT
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
         IF (IFVENT(II) == 1) T_MONVOLN%IBAGHOL(10, II) = 2
         IF (IFVENT(II) == 2) T_MONVOLN%IBAGHOL(10, II) = 3
         IF (IFVENT(II) == 3) T_MONVOLN%IBAGHOL(10, II) = 1

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
         IF (IVDP(II) == 0 .AND. IFVENT(II) /= 3) T_MONVOLN%IBAGHOL(9, II) = 0
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
         IF (IFVENT(NVENTHOLES + II) == 1) T_MONVOLN%IBAGHOL(10, NVENTHOLES + II) = 2
         IF (IFVENT(NVENTHOLES + II) == 2) T_MONVOLN%IBAGHOL(10, NVENTHOLES + II) = 3
         IF (FPORT(NVENTHOLES + II)  == ZERO) FPORT(NVENTHOLES + II) = ONE
         IF (FPORA(NVENTHOLES + II)  == ZERO) FPORA(NVENTHOLES + II) = ONE
         T_MONVOLN%RBAGHOL(7, NVENTHOLES + II) = FPORT(NVENTHOLES + II)
         T_MONVOLN%RBAGHOL(9, NVENTHOLES + II) = FPORA(NVENTHOLES + II)
C     
         T_MONVOLN%IBAGHOL(1, NVENTHOLES + II) = 0
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
         IF (IVDP(NVENTHOLES + II) /= 0 .AND. FVDP(NVENTHOLES + II) == ZERO) FVDP=ONE
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
         IF (T_MONVOLN%IBAGHOL(4, II) == -1) THEN
            T_MONVOLN%IBAGHOL(4, II) = 0
            CALL ANCMSG(MSGID = 332, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IPORP(II))
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
      ELSEIF(IEQUI == 1) THEN
        WRITE(IOUT,1404)
      ELSE 
        WRITE(IOUT,1405) IEQUI
      ENDIF
      WRITE(IOUT,1415) MWI, GAMAI, CPAI, CPBI, CPCI, CPDI, CPEI, CPFI
      WRITE(IOUT,1700)
      WRITE(IOUT,1702) KMESH
      WRITE(IOUT,2703) TSWITCH
      IF (AUTO_UP_SWITCH > 0) THEN
         WRITE (IOUT, 1789) AUTO_UP_SWITCH, PARAM, TSWITCH, PARAM, TSWITCH
      ENDIF
      IF (T_MONVOLN%IMESH_ALL > 0) THEN
         WRITE(IOUT, 1706) T_MONVOLN%IMESH_ALL
      ENDIF
      WRITE(IOUT,1704)
      WRITE(IOUT,1705) KFRAME
      WRITE(IOUT,1710) T_MONVOLN%RVOLU(41),T_MONVOLN%RVOLU(42),T_MONVOLN%RVOLU(43),
     .      T_MONVOLN%RVOLU(38),T_MONVOLN%RVOLU(39),T_MONVOLN%RVOLU(40),
     .     T_MONVOLN%RVOLU(35),T_MONVOLN%RVOLU(36),T_MONVOLN%RVOLU(37),
     .     T_MONVOLN%RVOLU(44),T_MONVOLN%RVOLU(45),T_MONVOLN%RVOLU(53),
     .     T_MONVOLN%IVOLU(54),T_MONVOLN%IVOLU(55),T_MONVOLN%IVOLU(65),
     .     NBRIC      ,SURFID_INT       ,T_MONVOLN%IVOLU(59),
     .     T_MONVOLN%IVOLU(60),T_MONVOLN%RVOLU(31),T_MONVOLN%RVOLU(34),
     .     T_MONVOLN%RVOLU(50),
     .     T_MONVOLN%RVOLU(46),T_MONVOLN%RVOLU(47),T_MONVOLN%RVOLU(51),
     .     T_MONVOLN%RVOLU(71),T_MONVOLN%RVOLU(72),
     .     T_MONVOLN%IVOLU(44),T_MONVOLN%IVOLU(40),T_MONVOLN%IVOLU(41),
     .     T_MONVOLN%IVOLU(42),IFVANI
      WRITE(IOUT,1425)
      WRITE(IOUT,1426) NJET
      DO II = 1, NJET
         WRITE(IOUT,1432) II, IGEO(1, I_INJSYS(II)), ISENS(II)
         IF (IVEL(II) > 0) THEN
            WRITE(IOUT, 1435) IVEL(II), FVEL(II)
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
         WRITE(IOUT,1442) GAMA, MW_TMP, CPA, CPB, CPC,
     .        CPD, CPE, CPF
         SURFID_INJ = T_MONVOLN%IBAGJET(14, II)
         SJET = ZERO                                                                                                         
         IF (SURFID_INJ/=0) THEN
            IF (IGRSURF(SURFID_INJ)%ISH4N3N == 0) THEN 
               CALL ANCMSG(MSGID = 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IGRSURF(SURFID_INJ)%ID, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ELSE 
               NN = IGRSURF(SURFID_INJ)%NSEG  
               DO JJ = 1, NN    
                  DIR = HALF   
                  I1 = IGRSURF(SURFID_INJ)%NODES(JJ,1)   
                  I2 = IGRSURF(SURFID_INJ)%NODES(JJ,2)   
                  I3 = IGRSURF(SURFID_INJ)%NODES(JJ,3)  
                  I4 = IGRSURF(SURFID_INJ)%NODES(JJ,4)  
                  ISH34 = IGRSURF(SURFID_INJ)%ELTYP(JJ) 
                  IF (ISH34 == 7)I4 = I3    
                  XX = HALF * (X(1, I1) + X(1, I2))   
                  YY = HALF * (X(2, I1) + X(2, I2))   
                  ZZ = HALF * (X(3, I1) + X(3, I2))   
                  X13 = X(1, I3) - X(1, I1)  
                  Y13 = X(2, I3) - X(2, I1)  
                  Z13 = X(3, I3) - X(3, I1)  
                  X24 = X(1, I4) - X(1, I2)  
                  Y24 = X(2, I4) - X(2, I2)  
                  Z24 = X(3, I4) - X(3, I2)  
                  NX = DIR * (Y13 * Z24 - Y24 * Z13)   
                  NY = DIR * (Z13 * X24 - Z24 * X13)  
                  NZ = DIR * (X13 * Y24 - X24 * Y13)  
                  DS = SQRT(NX * NX + NY * NY + NZ * NZ)  
                  SJET = SJET + DS     
               ENDDO     
            ENDIF  
            WRITE(IOUT,1445) IGRSURF(SURFID_INJ)%ID, SJET
         ENDIF    
      ENDDO
      WRITE(IOUT,1470) NVENT, TTFIRE
      WRITE(IOUT,1475)ITTF
      DO II = 1, NVENT
         IF (T_MONVOLN%IBAGHOL(13, II) == 0) THEN
            TITR1='VENT HOLE SURFACE'
            WRITE(IOUT,1472) II, IPVENT(II)
         ELSE
            TITR1='POROUS SURFACE'
            WRITE(IOUT,1473) II, IPVENT(II)
         ENDIF
         DO JJ = 1, 20
            TITREVENT(JJ) = T_MONVOLN%IBAGHOL(JJ + 14, II)
         ENDDO
         WRITE(IOUT,1476) VENTTITLE
         IF (IPVENT(II)==0) THEN                      
            CALL ANCMSG(MSGID = 1050, MSGTYPE = MSGERROR, ANMODE = ANINFO,                       
     .           I1 = T_MONVOLN%ID, I2 = II, C1 = T_MONVOLN%TITLE, C2 = TITR1)         
         ENDIF  
         IFVENT(II) = T_MONVOLN%IBAGHOL(10, II)
         IF (IFVENT(II) == 1) WRITE(IOUT, 1483)            
         IF (IFVENT(II) == 2) WRITE(IOUT, 1481)            
         IF (IFVENT(II) == 3) WRITE(IOUT, 1482) IVDP(II), FVDP(II)                 
         IF (IFVENT(II) == 5) WRITE(IOUT, 1484)                
         IF (IFVENT(II) == 4) WRITE(IOUT, 1485)
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
     .      5X,'UNIT SCALE FOR TIME FUNCTIONS          =',1PG20.13,
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
 1404 FORMAT(
     .     5X,'THERMODYNAMIC EQUILIBRIUM IS SET EVERY CYCLE UP TO INJECTION TIME'/)
 1405 FORMAT(
     .     5X,'THERMODYNAMIC EQUILIBRIUM IS SET EVERY',I10,' CYCLES UP TO INJECTION TIME'/)
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
 1435 FORMAT(15X,'TIME FUNCTION FOR INCOMING GAS VELOCITY=',I10,
     .     /15X,'SCALE FACTOR FOR INCOMING GAS VELOCITY.=',1PG20.13)
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
 1445 FORMAT(
     .     15X,'INFLATOR SURFACE ID . . . . . . . . . .=',I10,
     .     /15X,'INITIAL SURFACE OF INFLATOR . . . . . .=',1PG20.13)
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
     .     /15X,'TIME FUNCTION SCALE FACTOR          . .=',1PG20.13 
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
 1475 FORMAT(
     .     5X,'VENTING START TIME SHIFT . . . . . . . . . .=',I10,
     .     /5X,'  0  : NO SHIFT',
     .     /5X,'  1,2: VENTING FUNCTIONS ARE SHIFTED BY TINJ',
     .     /5X,'  3  : VENTING FUNCTIONS ARE SHIFTED BY TINJ+TSTART')
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
 1483 FORMAT(15X,'LOCAL VELOCITY VENTING FORMULATION')
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
 1700 FORMAT(5X,'FVMBAG PARAMETERS                       ',
     .     /5X,'-----------------                       ')
 1702 FORMAT(15X,'GAS MESH OPTION . . . . . . . . . . . .=',I10,
     .     /15X,'       = 1 POLYHEDRON AUTOMESH (obsolete)          ',
     .     /15X,'       = 2 TETRAHEDRON AUTOMESH                     ',
     .     /15X,'       = 4 TETRAHEDRON AUTOMESH WITH MESH OUTPUT    ',
     .     /15X,'       = 12 TETRAHEDRON HM AUTOMESH                 ',
     .     /15X,'       = 14 TETRAHEDRON HM AUTOMESH WITH MESH OUTPUT')
 1704 FORMAT(15X,'AIRBAG CUTTING FRAME:                   ')
 1705 FORMAT(15X,'FRAME NUMBER. . . . . . . . . . . . . .=',I10)
 1706 FORMAT(15X,'MESH HOLES. . . . . . . . . . . . . . .=',I10)
 1710 FORMAT(15X,'   ORIGIN X COORDINATE. . . . . . . . .=',1PG20.13,
     .     /15X,'   ORIGIN Y COORDINATE. . . . . . . . .=',1PG20.13,
     .     /15X,'   ORIGIN Z COORDINATE. . . . . . . . .=',1PG20.13,
     .     /15X,'   DIRECTION 1 X COORDINATE . . . . . .=',1PG20.13,
     .     /15X,'   DIRECTION 1 Y COORDINATE . . . . . .=',1PG20.13,
     .     /15X,'   DIRECTION 1 Z COORDINATE . . . . . .=',1PG20.13,
     .     /15X,'   DIRECTION 3 X COORDINATE . . . . . .=',1PG20.13,
     .     /15X,'   DIRECTION 3 Y COORDINATE . . . . . .=',1PG20.13,
     .     /15X,'   DIRECTION 3 Z COORDINATE . . . . . .=',1PG20.13,
     .     /15X,'CUTTING LENGTH IN LOCAL DIRECTION 1 . .=',1PG20.13,
     .     /15X,'CUTTING LENGTH IN LOCAL DIRECTION 2 . .=',1PG20.13,
     .     /15X,'CUTTING LENGTH IN LOCAL DIRECTION 3 . .=',1PG20.13,
     .     /15X,'NUMBER OF CELLS IN LOCAL DIRECTION 1. .=',I10,
     .     /15X,'NUMBER OF CELLS IN LOCAL DIRECTION 2. .=',I10,
     .     /15X,'NUMBER OF CELLS IN LOCAL DIRECTION 3. .=',I10,
     .     /15X,'ADDITIONAL BRICK GROUP NUMBER . . . . .=',I10,
     .     /15X,'INTERNAL SURFACE ID. .  . . . . . . . .=',I10,
     .     /15X,'FLAG FOR MESH ON REFERENCE METRICS. . .=',I10,
     .     /15X,'CELL MERGING STRATEGY:                  ',
     .     /15X,'   GLOBAL MERGING TYPE. . . . . . . . .=',I10,
     .     /15X,'   GLOBAL MERGING COEFFICIENT . . . . .=',1PG20.13,
     .     /15X,'   NEIGHBORHOOD MERGING COEFFICIENT . .=',1PG20.13,
     .     /15X,'TOLERANCE FOR SMALL SEGMENT REMOVAL . .=',1PG20.13,
     .     /15X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13,
     .     /15X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13,
     .     /15X,'MINIMUM HEIGHT. . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'DT SCALE FACTOR . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'MINIMUM TIMESTEP. . . . . . . . . . . .=',1PG20.13,
     .     /15X,'OUTPUT LEVEL. . . . . . . . . . . . . .=',I10,
     .     /15X,'MEMORY PARAMETERS:                      ',
     .     /15X,'   ESTIMATED NUMBER OF AIRBAG LAYERS. .=',I10,
     .     /15X,'   ESTIMATED MAXIMUM NUMBER OF AIRBAG   ',
     .     /15X,'       FACETS PER CUTTING CELL. . . . .=',I10,
     .     /15X,'   ESTIMATED MAXIMUM NUMBER OF POINTS   ',
     .     /15X,'       PER POLYGON. . . . . . . . . . .=',I10,
     .     /15X,'FLAG FOR ANIMATION FILE A000. . . . . .=',I10)
 1789 FORMAT(15X,'AUTOSWITCH TO UNIFORM PRESSURE OPTION .=',I10,
     .     /15X,'       = 1 DISPERSION BASED WITH ', 1PG20.13, ' TOLERANCE AROUND AVERAGE OR TIME > ', 1PG20.13,
     .     /15X,'       = 2 DISPERSION BASED WITH ', 1PG20.13, ' TOLERANCE AROUND AVERAGE AND TIME > ', 1PG20.13)
 2703 FORMAT(15X,'TIME TO SWITCH TO UNIFORM PRESSURE. . .=',1PG20.13)
      END SUBROUTINE HM_READ_MONVOL_TYPE8
