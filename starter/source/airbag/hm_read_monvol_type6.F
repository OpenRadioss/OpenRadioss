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
Chd|  HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
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
Chd|        MONVOL_CHECK_VENTHOLE_SURF    share/modules1/monvol_struct_mod.F
Chd|        MONVOL_COMPUTE_VOLUME         share/modules1/monvol_struct_mod.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|        MONVOL_REVERSE_NORMALS        share/modules1/monvol_struct_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MONVOL_TYPE6(T_MONVOLN, IPM, IGEO, ITABM1,
     .     SENSOR_TAB, NPT, PLD, IGRBRIC,
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
      INTEGER, INTENT(IN) :: LUID, IPM(NPROPMI, *), IGEO(NPROPGI, *),ITABM1(*), NPT(*)
      INTEGER, INTENT(IN) :: NPC(*), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *), PLD(2, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE (GROUP_), DIMENSION(NGRBRIC), INTENT(IN) :: IGRBRIC
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ
      INTEGER :: SURFID
      my_real :: FAC_GEN
      my_real :: SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      LOGICAL :: FOUND, DECREASE
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV
      my_real :: PEXT, TI, PINI, GAMAI, CPAI, CPBI, CPCI
      INTEGER :: IEQUI, ITTF, NJET, NVENT
      my_real, DIMENSION(:), ALLOCATABLE :: GAMA, CPA, CPB, CPC, FMASS, FTEMP, FVEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: IMASS, IFLU, ITEMP, ISENS, IJET, IVEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPVENT, ITVENT, IDTPDEF, IVDP
      my_real, DIMENSION(:), ALLOCATABLE :: AVENT, BVENT, TSTOPE, TVENT, DPDEF, DTPDEF, FVDP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPORT, IPORP, IPORA, IPORT1, IPORP1, IPORA1
      my_real, DIMENSION(:), ALLOCATABLE :: FPORT, FPORP, FPORA, FPORT1, FPORP1, FPORA1
      my_real :: DIRX, DIRY, DIRZ, DIR2X, DIR2Y, DIR2Z, ORIGX, ORIGY, ORIGZ, LX, LY, LZ
      INTEGER :: NBX, NBY, NBZ, IBRIC, ISUR, IREF, IGMERG
      my_real :: CGMERG, CNMERG, PTOLE, QA, QB, HMIN
      INTEGER :: ILVOUT, NLAYER, NFACMAX, NPPMAX, IFVANI
      my_real :: CPI, CVI, RMWI, MI, CPG, RMWG
      INTEGER :: NBRIC, NN, I1, I2, I3, I4, ISH34
      my_real :: SJET, DIR, XX, YY, ZZ, X13, Y13, Z13, X24, Y24, Z24, NX, NY, NZ, DS, 
     .     RHOI, TI2, ESPECI, SHOL
      INTEGER :: NP, IP, IFVENT
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
C     =======
C     Reading
C     =======
!     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
!     Line 2
      CALL HM_GET_FLOATV('Ascalet', SCAL_T, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleP', SCAL_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleS', SCAL_S, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleA', SCAL_A, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleD', SCAL_D, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 3
      CALL HM_GET_FLOATV('Pext', PEXT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T0', TI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Iequi', IEQUI, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ittf', ITTF, IS_AVAILABLE, LSUBMODEL)
!     Line 4
      CALL HM_GET_FLOATV('Gammai', GAMAI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('cpai', CPAI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('cpbi', CPBI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('cpci', CPCI, IS_AVAILABLE, LSUBMODEL, UNITAB)
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
         ALLOCATE(GAMA(NJET), CPA(NJET), CPB(NJET), CPC(NJET), FMASS(NJET), FTEMP(NJET))
         ALLOCATE(IMASS(NJET), IFLU(NJET), ITEMP(NJET), ISENS(NJET))
         ALLOCATE(IJET(NJET))
         ALLOCATE(IVEL(NJET), FVEL(NJET))
         DO II = 1, NJET
            CALL HM_GET_FLOAT_ARRAY_INDEX('Gamma', GAMA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('cpa', CPA(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('cpb', CPB(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('cpc', CPC(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
 
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDmas', IMASS(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iflow', IFLU(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalemas', FMASS(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDT', ITEMP(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleT', FTEMP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('sens_ID', ISENS(II), II,IS_AVAILABLE, LSUBMODEL)   

            CALL HM_GET_INT_ARRAY_INDEX('Isjet', IJET(II), II,IS_AVAILABLE, LSUBMODEL)

            CALL HM_GET_INT_ARRAY_INDEX('fct_IDvel', IVEL(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalevel', FVEL(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
         ENDDO
      ENDIF
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
      IF (NVENT > 0) THEN
         ALLOCATE(IPVENT(NVENT), ITVENT(NVENT), IDTPDEF(NVENT), IVDP(NVENT))
         ALLOCATE(AVENT(NVENT), BVENT(NVENT), TSTOPE(NVENT),
     .        TVENT(NVENT), DPDEF(NVENT), DTPDEF(NVENT), FVDP(NVENT))
         ALLOCATE(IPORT(NVENT), IPORP(NVENT), IPORA(NVENT), IPORT1(NVENT), 
     .        IPORP1(NVENT), IPORA1(NVENT))
         ALLOCATE(FPORT(NVENT), FPORP(NVENT), FPORA(NVENT), FPORT1(NVENT), 
     .        FPORP1(NVENT), FPORA1(NVENT))
         DO II = 1, NVENT
            CALL HM_GET_INT_ARRAY_INDEX('surf_IDv', IPVENT(II), II, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Avent', AVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Bvent', BVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('Itvent', ITVENT(II), II, IS_AVAILABLE, LSUBMODEL)
            
            CALL HM_GET_FLOAT_ARRAY_INDEX('Tvent', TVENT(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dPdef', DPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('dtPdef', DTPDEF(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('fct_IDV', IVDP(II), II,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('FscaleV', FVDP(II), II, IS_AVAILABLE, LSUBMODEL, UNITAB)
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
      CALL HM_GET_FLOATV('Vx3', DIRX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Vy3', DIRY, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Vz3', DIRZ, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('Vx1', DIR2X, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Vy1', DIR2Y, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Vz1', DIR2Z, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('X0', ORIGX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Y0', ORIGY, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Z0', ORIGZ, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('L1', LX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('L2', LY, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('L3', LZ, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV('Nb1', NBX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nb2', NBY, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nb3', NBZ, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('grbrc_ID', IBRIC, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('surf_IDin', ISUR, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Iref', IREF, IS_AVAILABLE, LSUBMODEL)
          
      CALL HM_GET_INTV('Igmerg', IGMERG, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Cgmerg', CGMERG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Cnmerg', CNMERG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Ptole', PTOLE, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('qa', QA, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('qb', QB, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Hmin', HMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV('Ilvout', ILVOUT, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nlayer', NLAYER, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nfacmax', NFACMAX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nppmax', NPPMAX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ifvani', IFVANI, IS_AVAILABLE, LSUBMODEL)
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
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 10)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 10)
     
      IF (ITTF < 0 .OR. ITTF > 3) THEN
         CALL ANCMSG(MSGID = 773, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      END IF
      IF (GAMAI == ZERO .OR. CPAI == ZERO) THEN
         CALL ANCMSG(MSGID = 473, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF
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
      
      AMU = EM02
      IF(PEXT == ZERO) THEN
         PEXT = 101325.D0 * (FAC_L_WORK * FAC_T_WORK * FAC_T_WORK) / FAC_M_WORK
      ENDIF
      IF (TI == ZERO) TI = TWOHUNDRED95
      PINI = PEXT
      
      DO II = 1, NJET
         IF (IMASS(II) /=0 .AND. FMASS(II) == ZERO) THEN 
            CALL HM_GET_FLOATV_DIM('Fscalemas', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FMASS(II) = ONE * FAC_GEN
         ENDIF
         IF (ITEMP(II) /=0 .AND. FTEMP(II) == ZERO) THEN 
            CALL HM_GET_FLOATV_DIM('FscaleT', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FTEMP(II) = ONE * FAC_GEN
         ENDIF
         
         IF (FVEL(II) == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('Fscalevel', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)
            FVEL(II) = ONE * FAC_GEN
         ENDIF
      ENDDO
      DO II = 1, NVENT
         IF (ITVENT(II) == 0) ITVENT(II) = 2
         IF (IPVENT(II) == 0) THEN
            AVENT(II) = ZERO
            BVENT(II) = ZERO
         ENDIF
         IF (FPORT(II)  == ZERO) FPORT(II) = ONE
         IF (FPORP(II)  == ZERO) FPORP(II) = ONE
         IF (FPORA(II)  == ZERO) FPORA(II) = ONE
         IF (FPORT1(II) == ZERO) FPORT1(II) = ONE
         IF (FPORP1(II) == ZERO) FPORP1(II) = ONE
         IF (FPORA1(II) == ZERO) FPORA1(II) = ONE
      ENDDO
      IF (NBX == 0) NBX = 1
      IF (NBY == 0) NBY = 1
      IF (NBZ == 0) NBZ = 1
      IF (IGMERG == 0) IGMERG = 1
      IF (CGMERG == ZERO) CGMERG = ZEP02
      IF (PTOLE == ZERO) PTOLE = EM5
      IF (NLAYER == 0) NLAYER = 10
      IF (NFACMAX == 0) NFACMAX = 20
      IF (NPPMAX == 0) NPPMAX = 20
      CPI = CPAI + TI * (CPBI + CPCI * TI)
      CVI = CPI / GAMAI
      RMWI = CVI * (GAMAI - ONE)
      MI = PINI * (VOL + VEPS) / (RMWI * TI)
C     =====
C     Store
C     ===== 
      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE / SCAL_S
      T_MONVOLN%RVOLU(29) = ONE / SCAL_A
      T_MONVOLN%RVOLU(30) = ONE / SCAL_D
      
      T_MONVOLN%IVOLU(17) = ITTF
      T_MONVOLN%IVOLU(8) = NJET
      DO II = 1, NJET
         T_MONVOLN%IBAGJET(14, II) = 0
         FOUND = .FALSE.
         DO JJ = 1, NSURF
            IF (IJET(II) == IGRSURF(JJ)%ID) THEN
               T_MONVOLN%IBAGJET(14, II) = JJ
               FOUND = .TRUE.
               EXIT
            ENDIF
         ENDDO
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 847, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IJET(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
         ENDIF
         IJET(II) = 0
         FOUND = .FALSE.
         DO JJ = 1, NFUNCT
            IF (IVEL(II) == NPC(JJ)) THEN
               T_MONVOLN%IBAGJET(11, II) = JJ
               FOUND = .TRUE.
               EXIT
            ENDIF
         ENDDO
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 6, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = IVEL(II))
         ENDIF
         T_MONVOLN%RBAGJET(15, II) = FVEL(II)
         T_MONVOLN%IBAGJET(13, II) = 0            
         T_MONVOLN%RBAGJET(1, II) = GAMA(II) 
         T_MONVOLN%RBAGJET(2, II) = CPA(II)
         T_MONVOLN%RBAGJET(3, II) = CPB(II)
         T_MONVOLN%RBAGJET(4, II) = CPC(II)
         T_MONVOLN%RBAGJET(5, II) = FMASS(II)
         T_MONVOLN%RBAGJET(6, II) = FTEMP(II)
         T_MONVOLN%RBAGJET(12, II) = ONE
         T_MONVOLN%RBAGJET(13, II) = ONE
         T_MONVOLN%RBAGJET(14, II) = ONE
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
         IF (ISENS(II) > 0) THEN
            FOUND = .FALSE.
            DO JJ = 1, NSENSOR
               IF (ISENS(II) == SENSOR_TAB(JJ)%SENS_ID) THEN
                  T_MONVOLN%IBAGJET(4, II) = JJ
                  FOUND = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF (.NOT. FOUND) THEN
               CALL ANCMSG(MSGID = 17, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = ISENS(II), I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ENDIF
         ENDIF
         CPG = CPA(II) + TI * (CPB(II) + CPC(II) * TI)
         RMWG = CPG * (GAMA(II) - ONE) / GAMA(II)
         T_MONVOLN%RBAGJET(1, II) = RMWG
      ENDDO
      DO II = 1, NVENT
         T_MONVOLN%RBAGHOL(7, II) = FPORT(II)
         T_MONVOLN%RBAGHOL(8, II) = FPORP(II)
         T_MONVOLN%RBAGHOL(9, II) = FPORA(II)
         T_MONVOLN%RBAGHOL(10, II) = FPORT1(II)
         T_MONVOLN%RBAGHOL(11, II) = FPORP1(II)
         T_MONVOLN%RBAGHOL(12, II) = FPORA1(II)
         T_MONVOLN%IBAGHOL(1, II) = 0
         T_MONVOLN%IBAGHOL(10, II) = 0
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
         TSTOPE(II) = INFINITY
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
         T_MONVOLN%IBAGHOL(10, II) = ITVENT(II)
      ENDDO
      T_MONVOLN%RVOLU(35) = DIRX
      T_MONVOLN%RVOLU(36) = DIRY
      T_MONVOLN%RVOLU(37) = DIRZ
      T_MONVOLN%RVOLU(38) = DIR2X
      T_MONVOLN%RVOLU(39) = DIR2Y
      T_MONVOLN%RVOLU(40) = DIR2Z
      T_MONVOLN%RVOLU(41) = ORIGX
      T_MONVOLN%RVOLU(42) = ORIGY
      T_MONVOLN%RVOLU(43) = ORIGZ
      T_MONVOLN%RVOLU(44) = LX
      T_MONVOLN%RVOLU(45) = LY
      T_MONVOLN%RVOLU(53) = LZ
      T_MONVOLN%IVOLU(54) = NBX
      T_MONVOLN%IVOLU(55) = NBY
      T_MONVOLN%IVOLU(35) = NBX * NBY
      T_MONVOLN%IVOLU(65) = NBZ
      T_MONVOLN%IVOLU(59) = IREF
      T_MONVOLN%IVOLU(61) = 0
      IF(IBRIC > 0) THEN
         DO JJ = 1, NGRBRIC
            IF (IBRIC == IGRBRIC(JJ)%ID) T_MONVOLN%IVOLU(61) = JJ
            EXIT
         ENDDO
      ENDIF
      T_MONVOLN%IVOLU(67)=0
      T_MONVOLN%INT_SURFID = 0
      IF (ISUR > 0) THEN
         DO JJ = 1, NSURF
            IF (ISUR == IGRSURF(JJ)%ID) THEN
               T_MONVOLN%IVOLU(67) = JJ
               T_MONVOLN%INT_SURFID = JJ
               EXIT
            ENDIF
         ENDDO
      ENDIF
      T_MONVOLN%IVOLU(60) = IGMERG
      T_MONVOLN%RVOLU(31) = CGMERG
      T_MONVOLN%RVOLU(34) = CNMERG
      T_MONVOLN%RVOLU(50) = PTOLE
      T_MONVOLN%RVOLU(46) = QA
      T_MONVOLN%RVOLU(47) = QB
      T_MONVOLN%RVOLU(51) = HMIN
      T_MONVOLN%IVOLU(44) = ILVOUT
      T_MONVOLN%IVOLU(40) = NLAYER
      T_MONVOLN%IVOLU(41) = NFACMAX
      T_MONVOLN%IVOLU(42) = NPPMAX
      T_MONVOLN%IVOLU(15) = IEQUI
      T_MONVOLN%IVOLU(27) = 1      
      T_MONVOLN%RVOLU(7) = CPAI
      T_MONVOLN%RVOLU(8) = CPBI
      T_MONVOLN%RVOLU(9) = CPCI
      T_MONVOLN%RVOLU(10) = RMWI
      T_MONVOLN%RVOLU(1) = GAMAI
      T_MONVOLN%RVOLU(3) = PEXT
      T_MONVOLN%RVOLU(4) = VOL+VEPS
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
      TI2 = TI * TI
      ESPECI = TI * (CPAI + HALF * CPBI * TI + THIRD * CPCI * TI2 - RMWI)
      T_MONVOLN%RVOLU(63) = ESPECI + RMWI * TI
      T_MONVOLN%RVOLU(64) = ZERO
      T_MONVOLN%RVOLU(65) = ZERO
      T_MONVOLN%RVOLU(66) = ESPECI
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
      ENDIF
      IF (IEQUI > 0) THEN
         IF (IEQUI == 1) THEN
            WRITE(IOUT, 1404)
         ELSE
            WRITE(IOUT,1405) IEQUI
         ENDIF
      ENDIF
      WRITE(IOUT, 1410) GAMAI, CPAI, CPBI, CPCI
      NBRIC = 0
      IF (T_MONVOLN%IVOLU(61) /= 0) NBRIC = IGRBRIC(T_MONVOLN%IVOLU(61))%ID
      WRITE(IOUT, 1700)
      WRITE(IOUT, 1704)
      WRITE(IOUT,1710) T_MONVOLN%RVOLU(41),T_MONVOLN%RVOLU(42),T_MONVOLN%RVOLU(43),
     .     T_MONVOLN%RVOLU(38),T_MONVOLN%RVOLU(39),T_MONVOLN%RVOLU(40),
     .     T_MONVOLN%RVOLU(35),T_MONVOLN%RVOLU(36),T_MONVOLN%RVOLU(37),
     .     T_MONVOLN%RVOLU(44),T_MONVOLN%RVOLU(45),T_MONVOLN%RVOLU(53),
     .     T_MONVOLN%IVOLU(54),T_MONVOLN%IVOLU(55),T_MONVOLN%IVOLU(65),
     .     NBRIC      ,ISUR       ,T_MONVOLN%IVOLU(59),
     .     T_MONVOLN%IVOLU(60),T_MONVOLN%RVOLU(31),T_MONVOLN%RVOLU(34),
     .     T_MONVOLN%RVOLU(50),
     .     T_MONVOLN%RVOLU(46),T_MONVOLN%RVOLU(47),T_MONVOLN%RVOLU(51),
     .     T_MONVOLN%RVOLU(71),T_MONVOLN%RVOLU(72),
     .     T_MONVOLN%IVOLU(44),T_MONVOLN%IVOLU(40),T_MONVOLN%IVOLU(41),
     .     T_MONVOLN%IVOLU(42),IFVANI
      WRITE(IOUT, 1420)
      WRITE(IOUT, 1421) NJET
      DO II = 1, NJET
         WRITE(IOUT, 1430) II, IMASS(II), IFLU(II), FMASS(II), ITEMP(II), FTEMP(II), ISENS(II)
         IF (IVEL(II) > 0) THEN
            WRITE(IOUT,1435) IVEL(II), FVEL(II)
         ENDIF
         WRITE(IOUT, 1440) GAMA(II), CPA(II), CPB(II), CPC(II)
!     Injector surface
         ISUR = T_MONVOLN%IBAGJET(14, II)
         SJET = ZERO
         IF (ISUR /= 0) THEN
            IF (IGRSURF(ISUR)%ISH4N3N == 0) THEN
               CALL ANCMSG(MSGID = 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I2 = IGRSURF(ISUR)%ID, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
            ELSE
               NN =IGRSURF(ISUR)%NSEG
               DO JJ = 1, NN
                  DIR = HALF
                  I1 = IGRSURF(ISUR)%NODES(JJ, 1)
                  I2 = IGRSURF(ISUR)%NODES(JJ, 2)
                  I3 = IGRSURF(ISUR)%NODES(JJ, 3)
                  I4 = IGRSURF(ISUR)%NODES(JJ, 4)
                  ISH34 = IGRSURF(ISUR)%ELTYP(JJ)
                  IF (ISH34 == 7) I4 = I3
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
         ENDIF
         WRITE(IOUT,1445) IGRSURF(ISUR)%ID, SJET
      ENDDO
      WRITE(IOUT, 1470) NVENT, ZERO
      IF (NVENT > 0) THEN
         WRITE(IOUT, 1475) ITTF
         DO II = 1, NVENT
            WRITE(IOUT, 1472) II, IPVENT(II)
            IF (IPVENT(II) == 0) THEN
               CALL ANCMSG(MSGID = 1050, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .              I1 = T_MONVOLN%ID, I2 = II, C1 = T_MONVOLN%TITLE, C2 = 'VENT HOLE SURFACE')
            ENDIF        
            IFVENT = T_MONVOLN%IBAGHOL(10, II)
            IF (IFVENT == 1) WRITE(IOUT, 1483)
            IF (IFVENT == 2) WRITE(IOUT, 1481)
            IF (IFVENT == 3) WRITE(IOUT, 1482) IVDP(II), FVDP(II)
            IF (IFVENT == 5) WRITE(IOUT, 1484)
            IF (IPVENT(II) /= 0) THEN
               CALL MONVOL_CHECK_VENTHOLE_SURF(IPRI, T_MONVOLN, IGRSURF, II, SHOL, X, IXC, IXTG)
               T_MONVOLN%RBAGHOL(15, II) = SHOL
               WRITE(IOUT,1479) 
     .              SHOL,AVENT(II),BVENT(II),
     .              IPORT(II),IPORP(II),IPORA(II),FPORT(II),FPORP(II),FPORA(II),
     .              IPORT1(II),IPORP1(II),IPORA1(II),FPORT1(II),FPORP1(II),FPORA1(II)
               WRITE(IOUT,1480) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
            ELSE
               WRITE(IOUT,1489)
     .              AVENT(II),BVENT(II),
     .              IPORT(II),IPORP(II),IPORA(II),FPORT(II),FPORP(II),FPORA(II),
     .              IPORT1(II),IPORP1(II),IPORA1(II),FPORT1(II),FPORP1(II),FPORA1(II)
               WRITE(IOUT,1480) TVENT(II),DPDEF(II),DTPDEF(II),IDTPDEF(II),TSTOPE(II)
            ENDIF
         ENDDO
      ENDIF
C     ===========
C     Memory free
C     ===========
      IF (NJET > 0) THEN
         DEALLOCATE(GAMA, CPA, CPB, CPC, FMASS, FTEMP)
         DEALLOCATE(IMASS, IFLU, ITEMP, ISENS)
         DEALLOCATE(IJET)
         DEALLOCATE(IVEL, FVEL)
      ENDIF
      IF (NVENT > 0) THEN
         DEALLOCATE(IPVENT, ITVENT, IDTPDEF, IVDP)
         DEALLOCATE(AVENT, BVENT, TSTOPE, TVENT, DPDEF, DTPDEF, FVDP)
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
 1404 FORMAT(
     .     5X,'THERMODYNAMIC EQUILIBRIUM IS SET EVERY CYCLE UP TO INJECTION TIME'/)
 1405 FORMAT(
     .     5X,'THERMODYNAMIC EQUILIBRIUM IS SET EVERY',I10,' CYCLES UP TO INJECTION TIME'/)
 1410 FORMAT(
     .     5X,'CHARACTERISTICS OF INITIAL GAZ          ',
     .     /5X,'------------------------------          ',
     .     /5X,'GAMMA AT INITIAL TEMPERATURE. . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /5X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13/)
 1700 FORMAT(5X,'FVMBAG PARAMETERS                       ',
     .     /5X,'-----------------                       ')
 1704 FORMAT(15X,'AIRBAG CUTTING FRAME:                   ')
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
 1435 FORMAT(15X,'TIME FUNCTION FOR INCOMING GAS VELOCITY=',I10,
     .     /15X,'SCALE FACTOR FOR INCOMING GAS VELOCITY.=',1PG20.13)
 1440 FORMAT(
     .     /15X,'GAZ CHARACTERISTICS                     ',
     .     /15X,'-------------------                     ',
     .     /15X,'GAMMA AT INITIAL TEMPERATURE. . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,
     .     /15X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13)
 1445 FORMAT(
     .     15X,'INFLATOR SURFACE ID . . . . . . . . . .=',I10,
     .     /15X,'INITIAL SURFACE OF INFLATOR . . . . . .=',1PG20.13)
 1470 FORMAT(
     .     /5X,'VENT HOLES AND POROUS FABRIC SURFACES   ',
     .     /5X,'-------------------------------------   ',
     .     /5X,'NUMBER OF VENT HOLES AND POROUS SURFACES . .=',I10,
     .     /5X,'INJECTION TIME TINJ. . . . . . . . . . . . .=',1PG20.13)
 1475 FORMAT(
     .     5X,'VENTING START TIME SHIFT . . . . . . . . . .=',I10,
     .     /5X,'  0  : NO SHIFT',
     .     /5X,'  1,2: VENTING FUNCTIONS ARE SHIFTED BY TINJ',
     .     /5X,'  3  : VENTING FUNCTIONS ARE SHIFTED BY TINJ+TSTART')
 1472 FORMAT(
     .     / 5X,'VENT HOLE NUMBER. . . . . . . . . . . .=',I10,
     .     /15X,'VENT HOLE SURFACE ID. . . . . . . . . .=',I10)
 1481 FORMAT(15X,'ISENTHALPIC VENTING MODEL ')
 1482 FORMAT(15X,'CHEMKIN MODEL FOR POROSITY : ',
     .     /15X,'VELOCITY VS RELATIVE PRESSURE FUNCTION =',I10,
     .     /15X,'              SCALE FACTOR. . . . . . .=',1PG20.13)
 1483 FORMAT(15X,'LOCAL VELOCITY VENTING FORMULATION')
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
      END SUBROUTINE HM_READ_MONVOL_TYPE6
