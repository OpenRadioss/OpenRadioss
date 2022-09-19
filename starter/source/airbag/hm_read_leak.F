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
Chd|  HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_LEAK(IPM,PM,UNITAB,LSUBMODEL)

      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      INTEGER IPM(NPROPMI,*)
C     REAL
      my_real
     .   PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER UID,HM_NLEAK,IFLAGUNIT,IUNIT
      INTEGER I, K, ILAW, IMID, IMAT, ILEAKAGE, IFTLC, IFTAC, IFTACP,
     .        NFUNC
C     REAL
      my_real
     .   ASCALET, ASCALEP, SCALELC, SCALEAC, FLC, FAC,
     .   X0, X1, X2, X3,
     .   LR1,LR2,FTHK,C1,C2,C3,
     .   FACP, SCALEACP
C
      CHARACTER TITR*nchartitle,LINESAVE*ncharline
      LOGICAL :: IS_AVAILABLE
      IS_AVAILABLE = .FALSE.
C=================================================================================
C     START READING
C=================================================================================
c
      ! Count number of /LEAK
      CALL HM_OPTION_COUNT('/LEAK/MAT',HM_NLEAK)
c
      ! Start reading /LEAK
      CALL HM_OPTION_START('/LEAK/MAT')
c
      ! Loop over /LEAK cards
      DO K = 1, HM_NLEAK
c
        ! Read keys
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = IMAT  ,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID   )

c
        ! Checking unit IDs
        IFLAGUNIT = 0
        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=IMAT,
     .                C1='LEAK',C2='LEAK',
     .                C3=TITR)
        ENDIF
c
        ! Initialization of Material ID
        IMID=0
        ! Loop over all materials
        DO I=1,NUMMAT-1
          ! If the material exist, the reading pursue
          IF(IPM(1,I)==IMAT)THEN
            IMID=IMAT
            WRITE(IOUT,1000)TRIM(TITR),IMID
            ILAW = IPM(2,I)
c
            ! Material law must be /MAT/LAW19 or /MAT/LAW58
            IF(ILAW==19.OR.ILAW==58)THEN
c
              ! Read first card
              CALL HM_GET_INTV  ('Ileakage' ,ILEAKAGE  ,IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_FLOATV('scale1'   ,ASCALET   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_FLOATV('scale2'   ,ASCALEP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              ! Checking, writing and saving the values
              IF(ASCALET == ZERO) ASCALET=ONE
              IF(ASCALEP == ZERO) ASCALEP=ONE
              WRITE(IOUT,1010)ILEAKAGE, ASCALET, ASCALEP
              IPM(4,I)=ILEAKAGE
              PM(160,I)= ASCALET
              PM(161,I)= ASCALEP
              IPM(6,I)=IPM(6,I)+3
              NFUNC=IPM(10,I)+IPM(6,I)
c
              ! Read second card
              CALL HM_GET_FLOATV('Acoeft1'     ,FACP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_INTV  ('MAT_fct_IDE' ,IFTACP   ,IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_FLOATV('FScale11'    ,SCALEACP ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              ! Checking, writing and saving the values
              IF(SCALEACP == ZERO) SCALEACP=ONE
              IF(FACP > 0)  IFTACP=0
              IF(FACP > ONE) FACP=ONE
              IF(FACP < ZERO) FACP=ZERO
              PM(162,I)= FACP
              PM(163,I)= SCALEACP
              IPM(10+NFUNC,I)=IFTACP
              WRITE(IOUT,1020) FACP, IFTACP, SCALEACP
C
              ! First case
              IF(ILEAKAGE==1) THEN
                ! Read third card
                CALL HM_GET_FLOATV('Bcoeft1'     ,FLC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('Acoeft2'     ,FAC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                ! Checking, writing and saving the values
                PM(164,I)= FLC
                PM(165,I)= FAC
                WRITE(IOUT,2010) FLC, FAC
              ! Second, third and fourth case
              ELSEIF(ILEAKAGE==2.OR.ILEAKAGE==3.OR.ILEAKAGE==4) THEN
                ! Read third card
                CALL HM_GET_INTV  ('LEAK_FCT_IDLC' ,IFTLC   ,IS_AVAILABLE, LSUBMODEL)
                CALL HM_GET_INTV  ('FUN_B1'        ,IFTAC   ,IS_AVAILABLE, LSUBMODEL)
                CALL HM_GET_FLOATV('FScale22'      ,SCALELC ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('FScale33'      ,SCALEAC ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                ! Checking, writing and saving the values
                IF(SCALELC == ZERO) SCALELC=ONE
                IF(SCALEAC == ZERO) SCALEAC=ONE
                PM(164,I)= SCALELC
                PM(165,I)= SCALEAC
                IPM(10+NFUNC-1,I)=IFTLC
                IPM(10+NFUNC-2,I)=IFTAC
                WRITE(IOUT,2020) ILEAKAGE,IFTLC,SCALELC,IFTAC,SCALEAC
              ! Fifth case
              ELSEIF(ILEAKAGE==5) THEN
                ! Read third card
                CALL HM_GET_FLOATV('LENGTH'  ,LR1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('THICK1'  ,FTHK    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                ! Read fourth card
                CALL HM_GET_FLOATV('C1'      ,C1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('C2'      ,C2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('C3'      ,C3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                ! Checking, writing and saving the values
                IF (FTHK > LR1) THEN
                  FTHK = LR1
                  CALL ANCMSG(MSGID=885,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=IMAT,
     .                        C1=TITR)
                END IF
                IF (LR1  == ZERO) LR1  = ONE
                IF (FTHK == ZERO) FTHK = LR1
                IF (C2 == ZERO)   C2   = ONE
                PM(164,I)= LR1
                PM(166,I)= FTHK
                PM(167,I)= C1
                PM(168,I)= C2
                PM(169,I)= C3
                WRITE(IOUT,2050) LR1,FTHK,C1,C2,C3
              ! Sixth case
              ELSEIF(ILEAKAGE==6) THEN
                ! Read third case
                CALL HM_GET_FLOATV('X0'    ,X0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('VX1'   ,X1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('ex2'   ,X2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                CALL HM_GET_FLOATV('VX3'   ,X3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                ! Checking, writing and saving the values
                PM(164,I)= X0
                PM(165,I)= X1
                PM(166,I)= X2
                PM(167,I)= X3
                WRITE(IOUT,2060) X0,X1,X2,X3
              ELSE
                IPM(4,I)=0
              ENDIF
            ELSE
              CALL ANCMSG(MSGID=923,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=IMAT,
     .                    C1=TITR)
            ENDIF
          ENDIF
        ENDDO
        IF (IMID == 0) THEN
          CALL ANCMSG(MSGID=924,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=IMAT,
     .                C1=TITR)
        ENDIF
      ENDDO
C       
      RETURN
C
 1000 FORMAT(//
     & 5X,'    FABRIC LEAKAGE MODEL  ',/,
     & 5X,'    --------------------  ',/,
     & 5X, A  ,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10)
 1010 FORMAT(
     & 5X,'MODEL NUMBER . . . . . . . . .. . . . .=',I10/
     & 5X,'ABCISSA TIME SCALE FACTOR . . . . . . .=',1PG20.13/
     & 5X,'ABCISSA PRESSURE SCALE FACTOR . . . . .=',1PG20.13/)
 1020 FORMAT(
     & 5X,'AREA COEFFICIENT FOR FABRIC IN CONTACT:       ',/,
     & 5X,'CONSTANT COEFFICIENT FOR FABRIC IN CONTACT. .=',1PG20.13/
     & 5X,'COEFFICIENT TIME FUNCTION NUMBER. . . . . . .=',I10/
     & 5X,'                 FUNCTION SCALE FACTOR. . . .=',1PG20.13/)
 2010 FORMAT(
     & 5X,'    MODEL # 1  ',/,
     & 5X,'    ---------  ',/,
     & 5X,'FABRIC LEAKAGE COEFFICIENT LC . . . . .=',1PG20.13/
     & 5X,'FABRIC AREA COEFFICIENT AC. . . . . . .=',1PG20.13/)
 2020 FORMAT(
     & 5X,'    MODEL #', I2  /,
     & 5X,'    ------------',/,
     & 5X,'FABRIC LEAKAGE COEFFICIENT FUNCTION OF TIME . .=',I10/
     & 5X,'                   FUNCTION SCALE FACTOR. . . .=',1PG20.13/
     & 5X,'FABRIC AREA COEFFICIENT FUNCTION OF PRESSURE. .=',I10/
     & 5X,'                   FUNCTION SCALE FACTOR. . . .=',1PG20.13)
 2050 FORMAT(
     & 5X,'    AUTOLIV MODEL  ',/,
     & 5X,'    -------------  ',/,
     & 5X,'MESH SIZE L . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FIBER THICKNESS R . . . . . . . . . . .=',1PG20.13/
     & 5X,'FABRIC LEAKAGE COEFFICIENT C1 . . . . .=',1PG20.13/
     & 5X,'FABRIC LEAKAGE COEFFICIENT C2 . . . . .=',1PG20.13/
     & 5X,'FABRIC LEAKAGE COEFFICIENT C3 . . . . .=',1PG20.13/)
 2060 FORMAT(
     & 5X,'    ANAGONYE WANG MODEL  ',/,
     & 5X,'    -------------------  ',/,
     & 5X,'FABRIC LEAKAGE COEFFICIENT X0 . . . . .=',1PG20.13/
     & 5X,'FABRIC LEAKAGE COEFFICIENT X1 . . . . .=',1PG20.13/
     & 5X,'FABRIC LEAKAGE COEFFICIENT X2 . . . . .=',1PG20.13/
     & 5X,'FABRIC LEAKAGE COEFFICIENT X3 . . . . .=',1PG20.13/)
      END
