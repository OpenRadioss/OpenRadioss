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
!||    hm_read_ebcs_gradp0      ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
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
      SUBROUTINE HM_READ_EBCS_GRADP0(IGRSURF, NPC, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, EBCS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE EBCS_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NPC(*),ID
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE (SURF_),TARGET, DIMENSION(NSURF)   :: IGRSURF
      CHARACTER(LEN=NCHARTITLE), INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      TYPE(t_ebcs_gradp0), INTENT(INOUT) :: EBCS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,ISU,SURF,NGR2USR,IPRES,IRHO,NSEG,IENER
      my_real C,PRES,RHO,LCAR,R1,R2,ENER
      EXTERNAL NGR2USR
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_ENCRYPTED,IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IPRES=0
      IRHO=0
      IENER=0
      C=ZERO
      PRES=ZERO
      RHO=ZERO
      LCAR=ZERO
      R1=ZERO
      R2=ZERO
      ENER=ZERO

      EBCS%title = TRIM(TITR)
      EBCS%HAS_IELEM = .TRUE. 


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
 
      !--line-2
      CALL HM_GET_FLOATV('rad_ebcs_c',  C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)                
      !--line-3
      CALL HM_GET_INTV('rad_fct_pr',  IPRES  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('rad_ebcs_fscale_pr',  PRES  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !--line-4
      CALL HM_GET_INTV('rad_fct_rho',  IRHO  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('rad_ebcs_fscale_rho',  RHO  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !--line-5
      CALL HM_GET_INTV('rad_fct_en',  IENER  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('rad_ebcs_fscale_en',  ENER  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !--line-6
      CALL HM_GET_FLOATV('rad_ebcs_lc',  LCAR  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('rad_ebcs_r1',  R1  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('rad_ebcs_r2',  R2  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
       
        IF(SURF /= 0 .AND. ISU /= 0 .AND. NSEG /= 0)THEN
           WRITE(IOUT,1000)ID,TRIM(TITR)
           WRITE(IOUT,1101)SURF,NSEG,C,PRES,IPRES,RHO,IRHO,ENER,IENER,LCAR,R1,R2
        ENDIF        
        DO J=1,NFUNCT
          IF(IPRES/=0 .AND. IPRES==NPC(J)) THEN 
             IPRES=J
             EXIT
          ENDIF
        ENDDO
        DO J=1,NFUNCT
           IF(IRHO/=0 .AND. IRHO==NPC(J)) THEN 
              IRHO=J
              EXIT
           ENDIF
        ENDDO
        DO J=1,NFUNCT
           IF(IENER/=0 .AND. IENER==NPC(J)) THEN
              IENER=J
              EXIT
           ENDIF
        ENDDO
        
        EBCS%title = TITR
        EBCS%ipres = IPRES
        EBCS%irho = IRHO
        EBCS%iener = IENER
        EBCS%c = C
        EBCS%pres = PRES
        EBCS%rho = RHO
        EBCS%lcar = LCAR
        EBCS%r1 = R1
        EBCS%r2 = R2
        EBCS%ener = ENER

        IF (MULTI_FVM%IS_USED) THEN
           CALL ANCMSG(MSGID = 1602, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .          I1 = ID, C1 = TRIM(TITR), C2 = "NOT COMPATIBLE WITH LAW 151")
        ENDIF

        IF(NSPMD > 1) THEN
           CALL ANCMSG(MSGID = 755, MSGTYPE = MSGERROR, ANMODE = ANINFO,C1 = 'ELEMENTARY BOUNDARY CONDITIONS')
        ENDIF
      
C-----------
      RETURN
C-----------

 1000 FORMAT( //'ZERO PRESSURE GRADIENT EBCS NUMBER . . . :',I8,1X,A)    
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


      END
