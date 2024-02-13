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
Chd|  HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_BOOLV                  source/devtools/hm_reader/hm_get_boolv.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SPMDSET                       source/constraints/general/rbody/spmdset.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DAMP(DAMPR,IGRNOD, ISKN,LSUBMODEL,UNITAB,NPBY,
     .                        SNPC1,NPC1)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD  
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      INTEGER ISKN(LISKN,*)
      INTEGER, INTENT(IN) :: SNPC1,NPC1(SNPC1),NPBY(NNPBY,NRBYKIN)
      my_real
     .  DAMPR(NRDAMP,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR
      EXTERNAL NGR2USR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,JGRN,ISK,FL_VREL
      INTEGER NB_PAS,RANGE,FLINT,FLG_PRI,SUB_INDEX
      INTEGER FUNC_ID,RBODY_ID,IRBY,IFUN,IGR
      my_real
     .   FACTB,TSTART,TSTOP,
     .   ALPHA,BETA,ALPHA_Y,BETA_Y,ALPHA_Z,BETA_Z,ALPHA_XX,BETA_XX,ALPHA_YY,
     .   BETA_YY,ALPHA_ZZ,BETA_ZZ,CDAMP_MX,CDAMP_MY,CDAMP_MZ,
     .   DV2_MX,DV2_MY,DV2_MZ,FREQ,XSCALE
      CHARACTER  TITR*nchartitle,KEY*nchartitle
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
      LOGICAL FULL_FORMAT
C======================================================================|
      IS_AVAILABLE = .FALSE.
      WRITE(IOUT,1000)
C--------------------------------------------------
C START BROWSING MODEL /DAMP
C--------------------------------------------------
      CALL HM_OPTION_START('/DAMP')
C--------------------------------------------------
C BROWSING MODEL DAMP 1->NDAMP
C--------------------------------------------------
      DO I=1,NDAMP
C--------------------------------------------------
C EXTRACT DATAS OF /DAMP/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                        OPTION_ID = ID,
     .                        OPTION_TITR = TITR,
     .                        SUBMODEL_INDEX = SUB_INDEX,
     .                        KEYWORD2=KEY)
        FULL_FORMAT = .FALSE.
C--------------------------------------------------
C HIDDEN FLAG FACTB
C--------------------------------------------------
C        IF(NBLINES == 2) THEN
C          IREC=IREC+1
C          READ(IIN,REC=IREC,FMT=FMT_F) FACTB
C        ENDIF
C-->     SET TO 1.0
C--------------------------------------------------
        FLINT = 0
        FL_VREL = 0
C        
        IF(KEY(1:5)=='INTER')THEN
          FLINT = 1
          CALL HM_GET_INTV('Nb_time_step',NB_PAS,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Range',RANGE,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('grnod_id',JGRN,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('skew_id',ISK,IS_AVAILABLE,LSUBMODEL)
          IF(ISK == 0 .AND. SUB_INDEX /= 0 ) ISK = LSUBMODEL(SUB_INDEX)%SKEW
          CALL HM_GET_BOOLV('Mass_Damp_Factor_Option',FULL_FORMAT,IS_AVAILABLE)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('Alpha',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta',BETA,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Tstart',TSTART,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Tstop',TSTOP,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_yy',ALPHA_YY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_yy',BETA_YY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_zz',ALPHA_ZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_zz',BETA_ZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
          IF (NB_PAS == 0) NB_PAS = 20
          WRITE(IOUT,1300)
          WRITE(IOUT,1400) NB_PAS
          WRITE(IOUT,1600) RANGE
          IDAMP_RDOF = IDAMP_RDOF+1 
          KCONTACT = 1
          DAMPR(19,I) = NB_PAS
          DAMPR(20,I) = RANGE
          DAMPR(21,I) = 0
        ELSEIF(KEY(1:4).EQ.'VREL')THEN
          FL_VREL = 1
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('grnod_id',JGRN,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('skew_id',ISK,IS_AVAILABLE,LSUBMODEL)
          IF(ISK == 0 .AND. SUB_INDEX .NE. 0 ) ISK = LSUBMODEL(SUB_INDEX)%SKEW
          CALL HM_GET_INTV('RbodyID',RBODY_ID,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('FuncID',FUNC_ID,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('Tstart',TSTART,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Tstop',TSTOP,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Freq',FREQ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale',XSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_x',CDAMP_MX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_y',CDAMP_MY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_z',CDAMP_MZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha2_x',DV2_MX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha2_y',DV2_MY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha2_z',DV2_MZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
          IF (CDAMP_MY == ZERO) CDAMP_MY = CDAMP_MX
          IF (CDAMP_MZ == ZERO) CDAMP_MZ = CDAMP_MX
          IF (DV2_MY == ZERO) DV2_MY = DV2_MX
          IF (DV2_MZ == ZERO) DV2_MZ = DV2_MX
          IF (XSCALE == ZERO) XSCALE = ONE
C---------RBODY ID user----------------------------
          IRBY=0
          IF (RBODY_ID /= 0) THEN  
            DO J=1,NRBYKIN
              IF (RBODY_ID == NPBY(6,J)) THEN
                IRBY=J                     
                EXIT
              ENDIF 
            ENDDO
            IF (IRBY == 0)THEN  !  Rbody not found    
              CALL ANCMSG(MSGID=3048,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=RBODY_ID)                        
            ENDIF
          ENDIF
C---------FUNC ID user-----------------------------
          IFUN=0    
          IF (FUNC_ID /= 0) THEN  
            DO J=1,NFUNCT
              IF (FUNC_ID == NPC1(J)) THEN
                IFUN=J                  
                EXIT
              ENDIF 
            ENDDO
            IF (IFUN == 0)THEN  !  Function not found    
              CALL ANCMSG(MSGID=3049,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=FUNC_ID)                           
            ENDIF
          ENDIF
C--------------------------------------------------     
          WRITE(IOUT,1700)
          DAMPR(19,I) = 0
          DAMPR(20,I) = 0
          DAMPR(21,I) = 1
          DAMPR(22,I) = DV2_MX
          DAMPR(23,I) = DV2_MY
          DAMPR(24,I) = DV2_MZ
          DAMPR(25,I) = IRBY
          DAMPR(26,I) = IFUN
          DAMPR(27,I) = XSCALE
          FULL_FORMAT = .TRUE.               
        ELSE
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('grnod_id',JGRN,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('skew_id',ISK,IS_AVAILABLE,LSUBMODEL)
          IF(ISK == 0 .AND. SUB_INDEX /= 0 ) ISK = LSUBMODEL(SUB_INDEX)%SKEW
          CALL HM_GET_BOOLV('Mass_Damp_Factor_Option',FULL_FORMAT,IS_AVAILABLE)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('Alpha',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta',BETA,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Tstart',TSTART,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Tstop',TSTOP,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_y',ALPHA_Y,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_y',BETA_Y,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_z',ALPHA_Z,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_z',BETA_Z,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_xx',ALPHA_XX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_xx',BETA_XX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_yy',ALPHA_YY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_yy',BETA_YY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Alpha_zz',ALPHA_ZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Beta_zz',BETA_ZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------     
          DAMPR(19,I) = 0
          DAMPR(20,I) = 0
          DAMPR(21,I) = 0       
C--------------------------------------------------
        ENDIF !   IF(KEY(1:5)=='INTER')THEN
C
        DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(ISK == ISKN(4,J+1)) THEN
            ISK=J+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                C1='DAMP',
     .                C2='DAMP',
     .                I1=ID,I2=ISK,C3=TITR)
 100   CONTINUE 

C
        IF (TSTOP == ZERO) TSTOP=EP30
C
        DAMPR(1,I) = ID
        INGR2USR => IGRNOD(1:NGRNOD)%ID
        IGR = NGR2USR(JGRN,INGR2USR,NGRNOD)
        DAMPR(2,I) = IGR
        DAMPR(3,I) = ALPHA
        DAMPR(4,I) = BETA
        IF (DAMPR(2,I) == 0) THEN
          CALL ANCMSG(MSGID=171,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1='RAYLEIGH DAMPING',
     .                I1= ID,
     .                C2= TITR,
     .                C3='NODE',
     .                I2=JGRN)
        ENDIF
        DAMPR(15,I) = ISK
        DAMPR(17,I) = TSTART
        DAMPR(18,I) = TSTOP
C
C
        IF (.NOT. FULL_FORMAT) THEN
C--       reduced format only for /DAMP and /DAMP/INTER
          FACTB = ONE
          DAMPR(5,I)  = ALPHA
          DAMPR(6,I)  = BETA
          DAMPR(7,I)  = ALPHA
          DAMPR(8,I)  = BETA
          DAMPR(9,I)  = ALPHA
          DAMPR(10,I) = BETA
          DAMPR(11,I) = ALPHA
          DAMPR(12,I) = BETA
          DAMPR(13,I) = ALPHA
          DAMPR(14,I) = BETA
          DAMPR(16,I) = FACTB
          IF (FLINT==1) THEN
            DAMPR(3,I)  = ZERO
            DAMPR(4,I)  = ZERO    
            DAMPR(5,I)  = ZERO
            DAMPR(6,I)  = ZERO
            DAMPR(7,I)  = ZERO
            DAMPR(8,I)  = ZERO
          ENDIF
          WRITE (IOUT,1100) JGRN,ALPHA,BETA,FACTB,TSTART,TSTOP
        ELSE
          IF ((FLINT==0).AND.(FL_VREL==0)) THEN
C--         /DAMP
            FACTB = ONE
            FLG_PRI = 1
            DAMPR(3,I) = ALPHA
            DAMPR(4,I) = BETA
            DAMPR(5,I) = ALPHA_Y
            DAMPR(6,I) = BETA_Y
            DAMPR(7,I) = ALPHA_Z
            DAMPR(8,I) = BETA_Z
            DAMPR(9,I)  = ALPHA_XX
            DAMPR(10,I) = BETA_XX
            DAMPR(11,I) = ALPHA_YY
            DAMPR(12,I) = BETA_YY
            DAMPR(13,I) = ALPHA_ZZ
            DAMPR(14,I) = BETA_ZZ
            WRITE (IOUT,1200) JGRN,ISKN(4,ISK),
     .                    ALPHA,BETA,ALPHA_Y,BETA_Y,ALPHA_Z,BETA_Z,
     .              ALPHA_XX,BETA_XX,ALPHA_YY,BETA_YY,ALPHA_ZZ,BETA_ZZ,
     .              TSTART,TSTOP

          ELSEIF (FLINT==1) THEN
C--         /DAMP/INTER         
            FACTB = ONE
            DAMPR(3,I)  = ZERO
            DAMPR(4,I)  = ZERO      
            DAMPR(5,I)  = ZERO
            DAMPR(6,I)  = ZERO
            DAMPR(7,I)  = ZERO
            DAMPR(8,I)  = ZERO
            DAMPR(9,I)  = ALPHA
            DAMPR(10,I) = BETA
            DAMPR(11,I) = ALPHA_YY
            DAMPR(12,I) = BETA_YY
            DAMPR(13,I) = ALPHA_ZZ
            DAMPR(14,I) = BETA_ZZ
            DAMPR(16,I) = FACTB
            WRITE (IOUT,1500) JGRN,ISKN(4,ISK),
     .                      ALPHA,BETA,ALPHA_YY,BETA_YY,
     .                      ALPHA_ZZ,BETA_ZZ,TSTART,TSTOP

          ELSEIF (FL_VREL == 1) THEN
C--       /DAMP/VREL
            FACTB = ONE
            DAMPR(3,I) = CDAMP_MX
            DAMPR(4,I) = ZERO
            DAMPR(5,I) = CDAMP_MY
            DAMPR(6,I) = ZERO
            DAMPR(7,I) = CDAMP_MZ
            DAMPR(8,I) = ZERO
            DAMPR(9,I)  = ZERO
            DAMPR(10,I) = ZERO
            DAMPR(11,I) = ZERO
            DAMPR(12,I) = ZERO
            DAMPR(13,I) = ZERO
            DAMPR(14,I) = ZERO
            WRITE (IOUT,1800) JGRN,ISKN(4,ISK),RBODY_ID,FUNC_ID,
     .                        CDAMP_MX,CDAMP_MY,CDAMP_MZ,
     .                        DV2_MX,DV2_MY,DV2_MZ,
     .                        FREQ,TSTART,TSTOP
            IF (FREQ == ZERO) FREQ = EP20
            DAMPR(28,I) = FREQ
            IF (IRBY > 0) THEN
C--           Main node is sticked on proc with damped nodes
              CALL SPMDSET(IRBY,NPBY,NNPBY,IGRNOD(IGR)%ENTITY,IGRNOD(IGR)%NENTITY,0)   
            ENDIF              
          ENDIF
        ENDIF
        DAMPR(16,I) = FACTB
      END DO ! NDAMP
C---
      RETURN

 1000 FORMAT(//
     .'       RAYLEIGH DAMPING       '/
     . '      ---------------------- ')
 1100 FORMAT(  8X,'NODE GROUP ID . . . . . . . . .',I10
     .       /10X,'ALPHA. . . . . . . . . . . . . .',1PG20.13
     .       /10X,'BETA . . . . . . . . . . . . . .',1PG20.13
     .       /10X,'MAX TIME STEP FACTOR . . . . . .',1PG20.13
     .       /10X,'START TIME . . . . . . . . . . .',1PG20.13
     .       /10X,'STOP TIME  . . . . . . . . . . .',1PG20.13)
 1200 FORMAT( 10X,'NODE GROUP ID . . . . . . . . .',I10
     .       /10X,'SKEW ID . . . . . . . . .   . .',I10
     .       /10X,'ALPHA IN X-DIRECTION. . . . . .',1PG20.13
     .       /10X,'BETA  IN X-DIRECTION. . . . . .',1PG20.13
     .       /10X,'ALPHA IN Y-DIRECTION. . . . . .',1PG20.13
     .       /10X,'BETA  IN Y-DIRECTION. . . . . .',1PG20.13
     .       /10X,'ALPHA IN Z-DIRECTION. . . . . .',1PG20.13
     .       /10X,'BETA  IN Z-DIRECTION. . . . . .',1PG20.13
     .       /10X,'ALPHA IN RX-DIRECTION . . . . .',1PG20.13
     .       /10X,'BETA  IN RX-DIRECTION . . . . .',1PG20.13
     .       /10X,'ALPHA IN RY-DIRECTION . . . . .',1PG20.13
     .       /10X,'BETA  IN RY-DIRECTION . . . . .',1PG20.13
     .       /10X,'ALPHA IN RZ-DIRECTION . . . . .',1PG20.13
     .       /10X,'BETA  IN RZ-DIRECTION . . . . .',1PG20.13
     .       /10X,'START TIME . . . . . . . . . . .',1PG20.13
     .       /10X,'STOP TIME  . . . . . . . . . . .',1PG20.13)
 1300 FORMAT(/,10X,'SELECTIVE RAYLEIGH DAMPING ON CONTACT NODES')
 1400 FORMAT( 10X,'NUMBER OF TIME STEP . . . . . .',I10,/)
 1500 FORMAT( 10X,'NODE GROUP ID . . . . . . . . .',I10
     .       /10X,'SKEW ID . . . . . . . . .   . .',I10
     .       /10X,'ALPHA IN RX-DIRECTION . . . . .',1PG20.13
     .       /10X,'BETA  IN RX-DIRECTION . . . . .',1PG20.13
     .       /10X,'ALPHA IN RY-DIRECTION . . . . .',1PG20.13
     .       /10X,'BETA  IN RY-DIRECTION . . . . .',1PG20.13
     .       /10X,'ALPHA IN RZ-DIRECTION . . . . .',1PG20.13
     .       /10X,'BETA  IN RZ-DIRECTION . . . . .',1PG20.13
     .       /10X,'START TIME . . . . . . . . . . .',1PG20.13
     .       /10X,'STOP TIME  . . . . . . . . . . .',1PG20.13)
 1600 FORMAT( 10X,'EXTENSION OF NODES SELECTION . ',I10,/)
 1700 FORMAT(/,10X,'RAYLEIGH DAMPING WITH RELATIVE VELOCITIES')
 1800 FORMAT( 10X,'NODE GROUP ID . . . . . . . . .',I10
     .       /10X,'SKEW ID . . . . . . . . . . . .',I10
     .       /10X,'RBODY ID . . . . . . . . . . . ',I10
     .       /10X,'DAMPING FUNCTION ID . . . . . .',I10
     .       /10X,'MASS DAMPING COEFFICIENT IN X-DIRECTION. . . . . .',1PG20.13
     .       /10X,'MASS DAMPING COEFFICIENT IN Y-DIRECTION. . . . . .',1PG20.13
     .       /10X,'MASS DAMPING COEFFICIENT IN Z-DIRECTION. . . . . .',1PG20.13
     .       /10X,'QUADRATIC MASS DAMPING COEFFICIENT IN X-DIRECTION.',1PG20.13
     .       /10X,'QUADRATIC MASS DAMPING COEFFICIENT IN Y-DIRECTION.',1PG20.13
     .       /10X,'QUADRATIC MASS DAMPING COEFFICIENT IN Z-DIRECTION.',1PG20.13
     .       /10X,'DAMPING FREQUENCY . . . . . . . . . . . . . . . . ',1PG20.13
     .       /10X,'START TIME . . . . . . . . . . . . . . . . . . . .',1PG20.13
     .       /10X,'STOP TIME  . . . . . . . . . . . . . . . . . . . .',1PG20.13)         
C---
      RETURN
      END
