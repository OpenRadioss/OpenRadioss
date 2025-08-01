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
!||    hm_read_ebcs_inlet         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
!||--- called by ------------------------------------------------------
!||    read_ebcs                  ../starter/source/boundary_conditions/ebcs/read_ebcs.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    ngr2usr                    ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EBCS_INLET(IGRSURF,NPC, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL, KEY2, SUB_INDEX, EBCS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE EBCS_MOD
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
      INTEGER  NPC(*)
      INTEGER ID,UID
      INTEGER,INTENT(IN) :: SUB_INDEX !< submodel index used to shift function identifiers if defined
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE (SURF_)   ,TARGET,  DIMENSION(NSURF)   :: IGRSURF
      CHARACTER(LEN=NCHARTITLE), INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
      CHARACTER(LEN=NCHARKEY),INTENT(IN) :: KEY2
      TYPE(t_ebcs_inlet), INTENT(INOUT) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,SURF,NGR2USR,IPRES,IRHO,J,NSEG,IENER,IVX,IVY,IVZ,IALPHA
      INTEGER IMAT,IVEL_TYP,U_IALPHA,U_IRHO,U_IPRES,IFLAGUNIT,OFF_DEF
      my_real :: CHECK_CUMUL_VF(2)
      my_real C,PRES,RHO,LCAR,R1,R2,ENER,VX,VY,VZ, ALPHA
      CHARACTER chain*9, chain1*64
      EXTERNAL NGR2USR
      LOGICAL FOUND
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      EBCS%title = TRIM(TITR)

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
      IPRES=0
      IVX=0
      IVY=0
      IVZ=0
      IRHO=0
      IENER=0
      C=ZERO
      PRES=ZERO
      RHO=ZERO
      LCAR=ZERO
      R1=ZERO
      R2=ZERO
      ENER=ZERO
      VX=ZERO
      VY=ZERO
      VZ=ZERO
      
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
      CALL HM_GET_INTV('vel_flag',  EBCS%fvm_inlet_data%VECTOR_VELOCITY  ,IS_AVAILABLE,LSUBMODEL)

      ISU=0
      IVEL_TYP = EBCS%fvm_inlet_data%VECTOR_VELOCITY
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

      WRITE(IOUT,1018)ID,TRIM(TITR)

      IF (KEY2(1:2) == 'VP') THEN
         EBCS%fvm_inlet_data%FORMULATION = 1
         WRITE(IOUT,1021)
      ELSEIF (KEY2(1:2) == 'VE') THEN
         EBCS%fvm_inlet_data%FORMULATION = 2
         WRITE(IOUT,1022)
      ELSE
         CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .        I1 = ID,
     .        C1 = TRIM(TITR),
     .        C2 = "AN INPUT FORMULATION HAS TO BE PROVIDED : VE, OR VP")
      ENDIF
            
      CALL HM_GET_FLOATV('rad_ebcs_fscale_vx',  VX  ,IS_AVAILABLE,LSUBMODEL,UNITAB)                
      CALL HM_GET_FLOATV('rad_ebcs_fscale_vy',  VY  ,IS_AVAILABLE,LSUBMODEL,UNITAB)                
      CALL HM_GET_FLOATV('rad_ebcs_fscale_vz',  VZ  ,IS_AVAILABLE,LSUBMODEL,UNITAB)                
      CALL HM_GET_INTV('fct_IDvx',  IVX  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('fct_IDvy',  IVY  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('fct_IDvz',  IVZ  ,IS_AVAILABLE,LSUBMODEL)
      IF(SUB_INDEX /= 0 ) THEN
        OFF_DEF = LSUBMODEL(SUB_INDEX)%OFF_DEF
        !since IVX, IVY, and IVZ may be -1, these values are shifted only if they are defined Spositive
        IF(IVX > 0) IVX = IVX + OFF_DEF
        IF(IVY > 0) IVY = IVY + OFF_DEF
        IF(IVZ > 0) IVZ = IVZ + OFF_DEF
      ENDIF

      IF(IVEL_TYP==0)THEN
        !NORMAL VELOCITY
        IF(IVX>0)THEN
          FOUND = .FALSE.
          DO J=1,NFUNCT
            IF(IVX==NPC(J)) THEN
              WRITE(IOUT,1133)IVX,VX
              IVX=J
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
        ELSEIF(IVX==0)THEN
          IF(VY/=ZERO.OR.VZ/=ZERO)THEN
          !check that user is defining VX ocrrectly
            CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .           I1 = ID,
     .           C1 = TRIM(TITR),
     .           C2 = "NORMAL VELOCITY MUST BE INPUT WITH COMPONENT-1 WHEN VEL_FLAG SET TO 0")
          ENDIF
          WRITE(IOUT,1134)VX
        ELSEIF(IVX==-1)THEN
          WRITE(IOUT,1135)IVX
        ENDIF

        IF(IVX<-1 .OR. (IVX>0.AND. .NOT.FOUND))THEN
           CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                 I1     = ID,
     .                 C1     = TRIM(TITR),
     .                 C2     = "INVALID FUNCTION ID FOR VELOCITY-X")
        ENDIF

      ELSE
        !VELOCITY COMPONENTS
        IF(IVX>0)THEN
          FOUND = .FALSE.
          DO J=1,NFUNCT
            IF(IVX==NPC(J)) THEN
              WRITE(IOUT,1121)IVX,VX
              IVX=J
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
        ELSEIF(IVX==0)THEN
          WRITE(IOUT,1124)IVX
        ELSEIF(IVX==-1)THEN
          WRITE(IOUT,1127)IVX
        ENDIF
        IF(IVX<-1 .OR. (IVX>0.AND. .NOT.FOUND))THEN
                CALL ANCMSG(MSGID  = 1602, MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                      I1     = ID,
     .                      C1     = TRIM(TITR),
     .                      C2     = "INVALID FUNCTION ID FOR VELOCITY-X")
        ENDIF
        IF(IVY>0)THEN
         FOUND = .FALSE.
         DO J=1,NFUNCT
           IF(IVY==NPC(J)) THEN
             WRITE(IOUT,1122)IVY,VY
             IVY=J
             FOUND = .TRUE.
             EXIT
           ENDIF
         ENDDO
        ELSEIF(IVY==0)THEN
          WRITE(IOUT,1125)IVY
        ELSEIF(IVY==-1)THEN
          WRITE(IOUT,1128)IVY
        ENDIF
        IF(IVY<-1 .OR. (IVY>0.AND. .NOT.FOUND))THEN
           CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                 I1     = ID,
     .                 C1     = TRIM(TITR),
     .                 C2     = "INVALID FUNCTION ID FOR VELOCITY-Y")
        ENDIF

        IF(IVZ>0)THEN
          FOUND = .FALSE.
          DO J=1,NFUNCT
            IF(IVZ==NPC(J)) THEN
              WRITE(IOUT,1123)IVZ,VZ
              IVZ=J
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
        ELSEIF(IVZ==0)THEN
          WRITE(IOUT,1126)IVZ
        ELSEIF(IVZ==-1)THEN
          WRITE(IOUT,1129)IVZ
        ENDIF

        IF(IVZ<-1 .OR. (IVZ>0.AND. .NOT.FOUND))THEN
          CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                I1     = ID,
     .                C1     = TRIM(TITR),
     .                C2     = "INVALID FUNCTION ID FOR VELOCITY-Z")
        ENDIF

      ENDIF

      EBCS%fvm_inlet_data%FUNC_VEL(1) = IVX
      EBCS%fvm_inlet_data%VAL_VEL(1)  =  VX
      EBCS%fvm_inlet_data%FUNC_VEL(2) = IVY
      EBCS%fvm_inlet_data%VAL_VEL(2)  =  VY
      EBCS%fvm_inlet_data%FUNC_VEL(3) = IVZ
      EBCS%fvm_inlet_data%VAL_VEL(3)  =  VZ
      CHECK_CUMUL_VF(1:2) = ZERO

      DO IMAT = 1, MULTI_FVM%NBMAT
         CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalevf_n', ALPHA ,IMAT,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalerho_n',RHO   ,IMAT,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalep_e_n',PRES  ,IMAT,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_INT_ARRAY_INDEX('fct_IDvf_n',   IALPHA,IMAT,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('fct_IDrho_n',  IRHO  ,IMAT,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('fct_IDp_e_n',  IPRES ,IMAT,IS_AVAILABLE,LSUBMODEL)
      IF(SUB_INDEX /= 0 ) THEN
        OFF_DEF = LSUBMODEL(SUB_INDEX)%OFF_DEF
        IF(IALPHA > 0) IALPHA = IALPHA + OFF_DEF
        IF(IRHO > 0) IRHO = IRHO + OFF_DEF
        IF(IPRES > 0) IPRES = IPRES + OFF_DEF
      ENDIF
         CHECK_CUMUL_VF(1)=CHECK_CUMUL_VF(1)+ABS(IALPHA)
         CHECK_CUMUL_VF(2)=CHECK_CUMUL_VF(2)+ABS(ALPHA)
         !user ids backup
         U_IALPHA=IALPHA
         U_IRHO  =IRHO
         U_IPRES =IPRES
         !check and get internal function id
         IF(IALPHA>0)THEN
           FOUND = .FALSE.
           DO J=1,NFUNCT
              IF(IALPHA==NPC(J)) THEN
                 IALPHA=J
                 FOUND=.TRUE.
                 EXIT
              ENDIF
           ENDDO
           IF(.NOT.FOUND)THEN
              chain='SUBMAT-00'
              write(chain(8:9),'(i2)')IMAT
              chain1='INVALID FUNCTION ID FOR IALPHA & '//chain
              CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                    I1     = ID,
     .                    C1     = TRIM(TITR),
     .                    C2     = chain1)
           ENDIF
         ENDIF
         !check and get internal function id
         IF(IRHO>0)THEN
           FOUND = .FALSE.
           DO J=1,NFUNCT
              IF(IRHO==NPC(J)) THEN
                 IRHO=J
                 FOUND=.TRUE.
                 EXIT
              ENDIF
           ENDDO
           IF(.NOT.FOUND)THEN
              chain='SUBMAT-00'
              write(chain(8:9),'(i2)')IMAT
              chain1='INVALID FUNCTION ID FOR IRHO & '//chain
              CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                    I1     = ID,
     .                    C1     = TRIM(TITR),
     .                    C2     = chain1)
           ENDIF
         ENDIF
         !check and get internal function id
         IF(IPRES>0)THEN
           FOUND = .FALSE.
           DO J=1,NFUNCT
              IF(IPRES==NPC(J)) THEN
                 IPRES=J
                 FOUND=.TRUE.
                 EXIT
              ENDIF
           ENDDO
           IF(.NOT.FOUND)THEN
              chain='SUBMAT-00'
              write(chain(8:9),'(i2)')IMAT
              chain1='INVALID FUNCTION ID FOR IPRES & '//chain
              CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                    I1     = ID,
     .                    C1     = TRIM(TITR),
     .                    C2     = chain1)
           ENDIF
         ENDIF
        IF(ALPHA<ZERO)THEN
          CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                I1     = ID,
     .                C1     = TRIM(TITR),
     .                C2     = "VOLUME FRACTION CANNOT BE NEGATIVE")
        ENDIF
        IF(RHO<ZERO)THEN
          CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .                I1     = ID,
     .                C1     = TRIM(TITR),
     .                C2     = "MASS DENSITY CANNOT BE NEGATIVE")
        ENDIF
         EBCS%fvm_inlet_data%FUNC_ALPHA(IMAT) = IALPHA
         EBCS%fvm_inlet_data%FUNC_RHO(IMAT)   = IRHO
         EBCS%fvm_inlet_data%FUNC_PRES(IMAT)  = IPRES
         EBCS%fvm_inlet_data%VAL_ALPHA(IMAT)  = ALPHA
         EBCS%fvm_inlet_data%VAL_RHO(IMAT)    = RHO
         EBCS%fvm_inlet_data%VAL_PRES(IMAT)   = PRES
         WRITE(IOUT,1130)IMAT
         WRITE(IOUT,1131)U_IALPHA,U_IRHO,U_IPRES
         WRITE(IOUT,1132)ALPHA,RHO,PRES
      ENDDO
      WRITE(IOUT, FMT='(/)' )
      IF(CHECK_CUMUL_VF(1)==ZERO .AND. CHECK_CUMUL_VF(2)==ZERO)THEN
        CALL ANCMSG(MSGID  = 1602,MSGTYPE= MSGERROR,ANMODE = ANINFO,
     .              I1     = ID,
     .              C1     = TRIM(TITR),
     .              C2     = "INPUT VOLUME FRACTIONS ARE EMPTY")
      ENDIF

        
      WRITE(IOUT,1118)SURF,NSEG
         
      IF (.NOT. MULTI_FVM%IS_USED) THEN
         CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1 = ID,C1 = TRIM(TITR),
     .               C2 = "ONLY COMPATIBLE WITH LAW 151")
      ENDIF
      
C-----------
      RETURN
C-----------
 1018 FORMAT( //'FLUID INLET EBCS NUMBER .  . . . . . . . :',I8,1X,A)
 1021 FORMAT( '   VELOCITY & PRESSURE')    
 1022 FORMAT( '   VELOCITY & ENERGY')
 1118 FORMAT(
     .        '    ON SURFACE  . . . . . . . . . . . . . . . ',I8,/,
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

