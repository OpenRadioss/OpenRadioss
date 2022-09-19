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
Chd|  HM_READ_EBCS_FLUXOUT          source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
Chd|-- called by -----------
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EBCS_FLUXOUT(LOCAL_ID, IGRSURF,
     .     NPC, NOM_OPT, MULTI_FVM, UNITAB, ID, TITR, TYP, UID, LSUBMODEL, EBCS)
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
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER LOCAL_ID, NPC(*)
      INTEGER NOM_OPT(LNOPT1),ID,TYP,UID
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE (SURF_)   ,TARGET,  DIMENSION(NSURF)   :: IGRSURF
      CHARACTER*nchartitle, INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      TYPE(t_ebcs_fluxout), INTENT(INOUT) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISU,SENS,MONVOL,SURF,NGR2USR,
     .        IPRES,IRHO,NOD,IAD,J,K1,K2,NSEG,IENER,IVX,IVY,IVZ,IALPHA
      INTEGER LISTE(NUMNOD), IMAT,U_IALPHA,U_IRHO,U_IPRES,IFLAGUNIT,FLAG_FMT,
     .        CHECK_CUMUL_VF(2)
      my_real
     .   C,PRES,RHO,LCAR,R1,R2,ENER,VX,VY,VZ, ALPHA,
     .   FAC_C,FAC_M,FAC_L,FAC_T
      CHARACTER MESS*40,KEY*ncharkey,OPT*ncharkey,chain*9, chain1*64
      EXTERNAL NGR2USR
      LOGICAL FOUND,IS_AVAILABLE,IS_ENCRYPTED
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
      DO J=1,NUNITS
        IF (UNITAB%UNIT_ID(J) == UID) THEN
          FAC_M = UNITAB%FAC_M(J)
          FAC_L = UNITAB%FAC_L(J)
          FAC_T = UNITAB%FAC_T(J)
          FAC_C = FAC_M/ ( FAC_L * FAC_T * FAC_T )
          IFLAGUNIT = 1
          EXIT
        ENDIF
      ENDDO
      IF (UID/=0.AND.IFLAGUNIT==0) THEN
        CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I2=UID,
     .              I1=ID,
     .              C1='EBCS',
     .              C2='EBCS',
     .              C3=TITR)
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

         IF(TYP==9) THEN
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
         ENDIF

         WRITE(IOUT,1118)SURF,NSEG
         IF (.NOT. MULTI_FVM%IS_USED) THEN
               CALL ANCMSG(MSGID=1602,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1 = ID,
     .              C1 = TRIM(TITR),
     .              C2 = "ONLY COMPATIBLE WITH LAW 151")
            ENDIF

C-----------
      RETURN
C-----------
 1000 FORMAT( //'ZERO PRESSURE GRADIENT EBCS NUMBER . . . :',I8,1X,A)
 1001 FORMAT( //'IMPOSED PRESSURE EBCS NUMBER . . . . . . :',I8,1X,A)
 1002 FORMAT( //'INLET VALVE  EBCS NUMBER   . . . . . . . :',I8,1X,A)
 1003 FORMAT( //'OUTLET VALVE  EBCS NUMBER  . . . . . . . :',I8,1X,A)
 1004 FORMAT( //'IMPOSED VELOCITY . . . . . . . . . . . . :',I8,1X,A)
 1005 FORMAT( //'IMPOSED NORMAL VELOCITY EBCS NUMBER  . . :',I8,1X,A)
 1006 FORMAT( //'INITIAL PRESSURE EBCS NUMBER . . . . . . :',I8,1X,A)
 1007 FORMAT( //'INITIAL VELOCITY  EBCS NUMBER  . . . . . :',I8,1X,A)
 1018 FORMAT( //'FLUID INLET EBCS NUMBER .  . . . . . . . :',I8,1X,A)
 1019 FORMAT( //'FLUX OUT  EBCS NUMBER  . . . . . . . . . :',I8,1X,A)
 1020 FORMAT( //'AXIS (N2D = 1) EBCS NUMBER.  . . . . . . :',I8,1X,A)
 1021 FORMAT( '   VELOCITY & PRESSURE')
 1022 FORMAT( '   VELOCITY & ENERGY')

 1101 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    SPEED OF SOUND  . . . . . . . . . . . . . ',E16.6,/,
     .        '    IMPOSED PRESSURE  . . . . . . . . . . . . ',E16.6,/,
     .        '    PRESSURE SCALING FUNCTION . . . . . . . . ',I8,/,
     .        '    IMPOSED DENSITY . . . . . . . . . . . . . ',E16.6,/,
     .        '    DENSITY SCALING FUNCTION  . . . . . . . . ',I8,/,
     .        '    IMPOSED ENERGY  . . . . . . . . . . . . . ',E16.6,/,
     .        '    ENERGY SCALING FUNCTION . . . . . . . . . ',I8,/,
     .        '    CHARACTERISTIC LENGTH . . . . . . . . . . ',E16.6,/,
     .        '    LINEAR RESISTANCE . . . . . . . . . . . . ',E16.6,/,
     .        '    QUADRATIC RESISTANCE  . . . . . . . . . . ',E16.6,/)
 1102 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    SPEED OF SOUND  . . . . . . . . . . . . . ',E16.6,/,
     .        '    IMPOSED VELOCITY  . . . . . . . . . . . . ',E16.6,/,
     .        '    VELOCITY SCALING FUNCTION . . . . . . . . ',I8,/,
     .        '    IMPOSED DENSITY . . . . . . . . . . . . . ',E16.6,/,
     .        '    DENSITY SCALING FUNCTION  . . . . . . . . ',I8,/,
     .        '    IMPOSED ENERGY  . . . . . . . . . . . . . ',E16.6,/,
     .        '    ENERGY SCALING FUNCTION . . . . . . . . . ',I8,/,
     .        '    CHARACTERISTIC LENGTH . . . . . . . . . . ',E16.6,/)
 1103 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    SPEED OF SOUND  . . . . . . . . . . . . . ',E16.6,/,
     .        '    IMPOSED VELOCITY VX . . . . . . . . . . . ',E16.6,/,
     .        '    VX SCALING FUNCTION . . . . . . . . . . . ',I8,/,
     .        '    IMPOSED VELOCITY VY . . . . . . . . . . . ',E16.6,/,
     .        '    VY SCALING FUNCTION . . . . . . . . . . . ',I8,/,
     .        '    IMPOSED VELOCITY VZ . . . . . . . . . . . ',E16.6,/,
     .        '    VZ SCALING FUNCTION . . . . . . . . . . . ',I8,/,
     .        '    IMPOSED DENSITY . . . . . . . . . . . . . ',E16.6,/,
     .        '    DENSITY SCALING FUNCTION  . . . . . . . . ',I8,/,
     .        '    IMPOSED ENERGY  . . . . . . . . . . . . . ',E16.6,/,
     .        '    ENERGY SCALING FUNCTION . . . . . . . . . ',I8,/,
     .        '    CHARACTERISTIC LENGTH . . . . . . . . . . ',E16.6,/)
 1104 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/,
     .        '    DENSITY . . . . . . . . . . . . . . . . . ',E16.6,/,
     .        '    SOUND SPEED . . . . . . . . . . . . . . . ',E16.6,/,
     .        '    CHARACTERISTIC LENGTH . . . . . . . . . . ',E16.6,/)
 1118 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/)
 1008 FORMAT(//
     .        'EBCS NUMBER :',I8,1X,A,/,
     .        '    CONNECTION TO MONITORED VOLUME. . . . . . ',I8,/,
     .        '    THROUGH SURFACE . . . . . . . . . . . . . ',I8,/,
     .        '    ACTIVATED BY SENSOR . . . . . . . . . . . ',I8,/,
     .        '    NUMBER OF SEGMENTS FOUND. . . . . . . . . ',I8,/)
 1121 FORMAT(
     .        '    IVx FUNCTION ID . . . . . . . . . . . . . ',I8,/,
     .        '    Vx SCALE FACTOR . . . . . . . . . . . . . ',E16.6)
 1122 FORMAT(
     .        '    IVy FUNCTION ID . . . . . . . . . . . . . ',I8,/,
     .        '    Vy SCALE FACTOR . . . . . . . . . . . . . ',E16.6)
 1123 FORMAT(
     .        '    IVz FUNCTION ID . . . . . . . . . . . . . ',I8,/,
     .        '    Vz SCALE FACTOR . . . . . . . . . . . . . ',E16.6)
 1124 FORMAT(
     .        '    IVx FUNCTION ID . . . . . . . . . . . . . ',I2)
 1125 FORMAT(
     .        '    IVy FUNCTION ID . . . . . . . . . . . . . ',I2)
 1126 FORMAT(
     .        '    IVz FUNCTION ID . . . . . . . . . . . . . ',I2)
 1127 FORMAT(
     .        '    IVx FUNCTION ID . . . . . . . . . . . . . ',I2,/,
     .        '      Von Neumann BCS :  d/dn(Vx) = 0')
 1128 FORMAT(
     .        '    IVy FUNCTION ID . . . . . . . . . . . . . ',I2,/,
     .        '      Von Neumann BCS :  d/dn(Vy) = 0')
 1129 FORMAT(
     .        '    IVz FUNCTION ID . . . . . . . . . . . . . ',I2,/,
     .        '      Von Neumann BCS :  d/dn(Vz) = 0')

 1130 FORMAT(
     .      /,'    SUBMAT-',I2)
 1131 FORMAT(
     .        '      IALPHA FUNCTION. . . . . . . . . . . . .',I8,/,
     .        '      IRHO FUNCTION. . . . . . . . . . . . . .',I8,/,
     .        '      IPRES FUNCTION. . . . . . . . . . . . . ',I8)
 1132 FORMAT(
     .        '      ALPHA SCALE FACTOR . . . . . . . . . . .',E16.6,/,
     .        '      RHO SCALE FACTOR . . . . . . . . . . . .',E16.6,/,
     .        '      PRES SCALE FUNCTION . . . . . . . . . . ',E16.6)
 1133 FORMAT(
     .        '    IVn FUNCTION ID . . . . . . . . . . . . . ',I8,/,
     .        '    Vn SCALE FACTOR . . . . . . . . . . . . . ',E16.6)
 1134 FORMAT(
     .        '    Vn CONSTANT VELOCITY. . . . . . . . . . . ',E16.6)
 1135 FORMAT(
     .        '    IVn FUNCTION ID . . . . . . . . . . . . . ',I2,/,
     .        '      Von Neumann BCS :  d/dn Vn = 0')


      END

