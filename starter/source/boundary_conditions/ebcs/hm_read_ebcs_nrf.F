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
      IF (UID/=0.AND.IFLAGUNIT==0) THEN                      
        CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=UID,I1=ID,C1='EBCS',C2='EBCS',C3=TITR)
      ENDIF
    
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      CALL HM_GET_INTV('entityid',  SURF  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('tcar_p',  TCAR_P  ,IS_AVAILABLE,LSUBMODEL,UNITAB)                
      CALL HM_GET_FLOATV('tcar_vf',  TCAR_VF ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      
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
         WRITE(IOUT,1118)SURF,NSEG,TCAR_P,TCAR_VF
         
      
C-----------
      RETURN
C-----------
 1001 FORMAT( //'NON-REFLECTING FRONTIER EBCS NUMBER. . . . :',I8,1X,A)    

 1118 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    TCAR_P  . . . . . . . . . . . . . . . . . ',E20.12,/,
     .        '    TCAT_ALPHA  . . . . . . . . . . . . . . . ',E20.12,/)

      END SUBROUTINE HM_READ_EBCS_NRF

