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
Chd|  HM_READ_EBCS_FLUXOUT          source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
Chd|-- called by -----------
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EBCS_FLUXOUT( IGRSURF, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL, EBCS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER ID,UID
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE (SURF_)   ,TARGET,  DIMENSION(NSURF)   :: IGRSURF
      CHARACTER*nchartitle, INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)      
      TYPE(t_ebcs_fluxout), INTENT(INOUT) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,SURF,NGR2USR,J,NSEG
      INTEGER IMAT,IFLAGUNIT
      EXTERNAL NGR2USR
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      EBCS%IS_MULTIFLUID = .TRUE.
      EBCS%HAS_IELEM = .TRUE.             
      EBCS%FVM_INLET_DATA%FORMULATION = -1
      EBCS%FVM_INLET_DATA%VECTOR_VELOCITY = 0
      EBCS%FVM_INLET_DATA%FUNC_VEL(1:3) = 0
      EBCS%FVM_INLET_DATA%FUNC_ALPHA(1:21) = 0
      EBCS%FVM_INLET_DATA%FUNC_RHO(1:21) = 0
      EBCS%FVM_INLET_DATA%FUNC_PRES(1:21) = 0
      EBCS%FVM_INLET_DATA%VAL_VEL(1:3) = ZERO
      EBCS%FVM_INLET_DATA%VAL_ALPHA(1:21) = ZERO
      EBCS%FVM_INLET_DATA%VAL_RHO(1:21) = ZERO
      EBCS%FVM_INLET_DATA%VAL_PRES(1:21) = ZERO
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
        

         WRITE(IOUT,1019)ID,TRIM(TITR)
         EBCS%fvm_inlet_data%FUNC_VEL(1) = -1
         EBCS%fvm_inlet_data%VAL_VEL(1)  =  ZERO
         EBCS%fvm_inlet_data%FUNC_VEL(2) = -1
         EBCS%fvm_inlet_data%VAL_VEL(2)  =  ZERO
         EBCS%fvm_inlet_data%FUNC_VEL(3) = -1
         EBCS%fvm_inlet_data%VAL_VEL(3)  =  ZERO
         EBCS%fvm_inlet_data%FORMULATION = 2
         EBCS%fvm_inlet_data%VECTOR_VELOCITY = 1
         DO IMAT = 1, MULTI_FVM%NBMAT
            EBCS%fvm_inlet_data%FUNC_ALPHA(IMAT) = -1
            EBCS%fvm_inlet_data%FUNC_RHO(IMAT)   = -1
            EBCS%fvm_inlet_data%FUNC_PRES(IMAT)  = -1
            EBCS%fvm_inlet_data%VAL_ALPHA(IMAT)  = ZERO
            EBCS%fvm_inlet_data%VAL_RHO(IMAT)    = ZERO
            EBCS%fvm_inlet_data%VAL_PRES(IMAT)   = ZERO
         ENDDO

                 
         WRITE(IOUT,1118)SURF,NSEG
         IF (.NOT. MULTI_FVM%IS_USED) THEN
               CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .              I1 = ID,C1 = TRIM(TITR), C2 = "ONLY COMPATIBLE WITH LAW 151")
            ENDIF
                  
C-----------
      RETURN
C-----------
 1019 FORMAT( //'FLUX OUT  EBCS NUMBER  . . . . . . . . . :',I8,1X,A)
 1118 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/)

      END SUBROUTINE HM_READ_EBCS_FLUXOUT

