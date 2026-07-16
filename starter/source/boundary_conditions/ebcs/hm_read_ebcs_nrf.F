Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_ebcs_nrf         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
!||--- called by ------------------------------------------------------
!||    read_ebcs                ../starter/source/boundary_conditions/ebcs/read_ebcs.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    ngr2usr                  ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EBCS_NRF(IGRSURF, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL, EBCS)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C            TCAR_P = infinity (1E20) : pure non-reflecting boundary condition, no wave transmission, only outgoing waves
C DEFAULT -> TCAR_P defined means relaxation to initial pressure
C            TCAR_P << DT1 (1E-20) : constant pressure, all waves transmitted
C
C DEFAULT -> TCAR_VF = infinity (1E20) : no relaxation to initial volume fractions
C            TCAR_VF /=0 : user tuning parameter for relaxation to initial volume fractions, smaller values more relaxation, larger values less relaxation
C            TCAR_VF << DT1 (1E-20) : constant initial fractions
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EBCS_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER ID,UID
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE (SURF_)   ,TARGET,  DIMENSION(NSURF)   :: IGRSURF
      CHARACTER(LEN=NCHARTITLE), INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
      TYPE(t_ebcs_nrf), INTENT(INOUT) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,SURF,J,NSEG
      INTEGER IMAT,IFLAGUNIT
      my_real TCAR_VF,TCAR_P
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      INTEGER, EXTERNAL :: NGR2USR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      EBCS%title = TRIM(TITR)

      IFLAGUNIT=0
      DO J=1,UNITAB%NUNITS                                              
        IF (UNITAB%UNIT_ID(J) == UID) THEN                                               
          IFLAGUNIT = 1                                          
          EXIT                                                   
        ENDIF                                                    
      ENDDO                                                      
      IF (UID /=0 .AND. IFLAGUNIT == 0) THEN
        CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=UID,I1=ID,C1='EBCS',C2='EBCS',C3=TITR)
      ENDIF
    
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      CALL HM_GET_INTV('entityid',  SURF  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('tcar_p',  TCAR_P  ,IS_AVAILABLE,LSUBMODEL,UNITAB)                
      CALL HM_GET_FLOATV('tcar_vf',  TCAR_VF ,IS_AVAILABLE,LSUBMODEL,UNITAB) 

      IF(TCAR_P < ZERO)THEN
        CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1 = EBCS%ebcs_id,C1 = TRIM(ebcs%title),
     .             C2 = "/EBCS/NRF PARAMETER TCAR_P CANNOT BE NEGATIVE ")
        TCAR_P = ZERO
      ELSEIF(TCAR_P <= EM20 .AND. TCAR_P /= ZERO)THEN
        CALL ANCMSG(MSGID=3077,MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1 = EBCS%ebcs_id,C1 = TRIM(ebcs%title),
     .             C2 = "/EBCS/NRF : TCAR_P = 1E-20 IS NOT RECOMMENDED",
     .             C3 = "  FOR PURE NON-REFLECTIVE BOUNDARY TCAR_P IS SET TO 1E20")
        TCAR_P = EP20
      ENDIF

      IF(TCAR_VF < ZERO)THEN
        CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1 = EBCS%ebcs_id,C1 = TRIM(ebcs%title),
     .             C2 = "/EBCS/NRF PARAMETER TCAR_VF CANNOT BE NEGATIVE ")
        TCAR_VF = ZERO
      ELSEIF(TCAR_VF <= EM20 .AND. TCAR_VF /= ZERO)THEN
        CALL ANCMSG(MSGID=3077,MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1 = EBCS%ebcs_id,C1 = TRIM(ebcs%title),
     .             C2 = "/EBCS/NRF : TCAR_VF IS SET TO 1E-20. IN CASE OF INCOMING FLUX VOLUME FRACTIONS",
     .             C3 = " ARE REINJECTED WITH THEIR INITIAL VALUES (WITHOUT RELAXATION)" )
      ENDIF

      IF(TCAR_VF == ZERO)TCAR_VF = INFINITY

      EBCS%TITLE = TITR
      EBCS%tcar_p = TCAR_P  
      EBCS%tcar_vf = TCAR_VF
      EBCS%HAS_IELEM = .TRUE. 
      
      IF(MULTI_FVM%IS_USED)THEN
        EBCS%is_multifluid = .TRUE.
      ENDIF 
      
       EBCS%fvm_inlet_data%FUNC_VEL(1:3) = -1
       EBCS%fvm_inlet_data%VAL_VEL(1:3)  =  ZERO
       EBCS%fvm_inlet_data%FORMULATION = 2
       EBCS%fvm_inlet_data%VECTOR_VELOCITY = 1
       DO IMAT = 1, 21 !  MULTI_FVM%NBMAT       -> init from NBMAT+1,21 to avoid uninit values transmitted to starter
          EBCS%fvm_inlet_data%FUNC_ALPHA(IMAT) = -1
          EBCS%fvm_inlet_data%FUNC_RHO(IMAT)   = -1
          EBCS%fvm_inlet_data%FUNC_PRES(IMAT)  = -1
          EBCS%fvm_inlet_data%VAL_ALPHA(IMAT)  = ZERO
          EBCS%fvm_inlet_data%VAL_RHO(IMAT)    = ZERO
          EBCS%fvm_inlet_data%VAL_PRES(IMAT)   = ZERO
       ENDDO
      
         ISU=0
         INGR2USR => IGRSURF(1:NSURF)%ID
         IF (SURF/=0) ISU=NGR2USR(SURF,INGR2USR,NSURF)
         NSEG=0
         IF (ISU/=0) NSEG=IGRSURF(ISU)%NSEG
         IF(SURF==0)THEN
            IERR=IERR+1
            WRITE(ISTDO,'(6X,A)')' ** A SURFACE SHOULD BE INPUT'  
            WRITE(IOUT, '(6X,A)')' ** A SURFACE SHOULD BE INPUT'       
         ELSEIF(ISU==0)THEN
            IERR=IERR+1
            WRITE(ISTDO,*)' ** ERROR SURFACE NOT FOUND, ID=',SURF
            WRITE(IOUT,*) ' ** ERROR SURFACE NOT FOUND, ID=',SURF        
         ELSEIF(NSEG==0)THEN
            IERR=IERR+1
            WRITE(ISTDO,*)' ** ERROR EMPTY SURFACE',SURF
            WRITE(IOUT,*) ' ** ERROR EMPTY SURFACE',SURF
         ENDIF
         
         EBCS%nb_elem = NSEG

         WRITE(IOUT,1001)ID, TRIM(TITR)
         WRITE(IOUT,1002)
         WRITE(IOUT,1003)
         IF(TCAR_P == ZERO)THEN
           WRITE(IOUT,1119)SURF,NSEG,TCAR_VF
         ELSE
           WRITE(IOUT,1118)SURF,NSEG,TCAR_P,TCAR_VF
         ENDIF
      
C-----------
      RETURN
C-----------
 1001 FORMAT( //'NON-REFLECTING FRONTIER EBCS NUMBER. . . . :',I8,1X,A,/)
 1002 FORMAT(   '    TCAR_P  : CHARACTERISTIC TIME FOR PRESSURE RELAXATION',/,
     .          '              SMALL VALUE: PRESSURE STRONGLY RELAXED TO REFERENCE STATE',/,
     .          '                           MAY INCREASE NUMERICAL REFLECTIONS',/,
     .          '              DEFAULT    : LCAR/2/LOG(2)/MAX(SSP)',/,
     .          '              = 1E20     : PRESSURE RELAXATION DISABLED',/,
     .          '                           CHARACTERISTIC NON-REFLECTING UPDATE ONLY'/)

 1003 FORMAT(   '    TCAR_VF : CHARACTERISTIC TIME FOR VOLUME FRACTION RELAXATION (INFLOW ONLY)',/,
     .          '              CONTROLS RELAXATION OF INCOMING VOLUME FRACTIONS',/,
     .          '              TOWARDS THEIR INITIAL VALUES',/,
     .          '              < DT       : INITIAL VOLUME FRACTIONS FULLY REINJECTED',/,
     .          '              > DT       : PROGRESSIVE RELAXATION TO INITIAL VALUES',/,
     .          '              = 1E20     : VOLUME FRACTION RELAXATION DISABLED'/)

 1118 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    TCAR_P  . . . . . . . . . . . . . . . . . ',E20.12,/,
     .        '    TCAR_VF . . . . . . . . . . . . . . . . . ',E20.12,/)
 1119 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    TCAR_P  . . . . . . . . . . . . . . . . . ','  auto',/,
     .        '    TCAR_VF . . . . . . . . . . . . . . . . . ',E20.12,/)

      END SUBROUTINE HM_READ_EBCS_NRF

