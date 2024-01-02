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
Chd|  HM_READ_MATGAS                source/materials/mat/matgas/hm_read_matgas.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MATGAS(PM, IPM, ID, TITR, KEY2, UNITAB, LSUBMODEL)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sysunit.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN) :: ID
      CHARACTER(LEN = nchartitle) :: TITR
      INTEGER, DIMENSION(NPROPMI), INTENT(INOUT) :: IPM
      my_real, DIMENSION(NPROPM), INTENT(INOUT) :: PM
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      CHARACTER(LEN = ncharline), INTENT(IN) :: KEY2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   MW, CPA, CPB, CPC, CPD, CPE, CPF, CV,
     .   FAC_L, FAC_T, FAC_M, R_IGC1, FAC_CP, FAC
      CHARACTER(LEN = ncharline) :: KEY, USERGAS_NAME, KEY2_TRIM
      CHARACTER(LEN = 20) :: GASTYPE
      INTEGER :: IGAS, IMOLE
      LOGICAL :: IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C     S o u r c e   l i n e s 
C-----------------------------------------------

!     **************
!     Initialization
      IS_AVAILABLE = .FALSE.
      IS_ENCRYPTED = .FALSE.
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      KEY2_TRIM = KEY2(1:LEN_TRIM(KEY2))
      SELECT CASE(KEY2_TRIM)
      CASE ('MOLE')
         IGAS = 1
         IMOLE = 1
      CASE ('MASS')
         IGAS = 2
         IMOLE = 0
      CASE ('CSTA')
         IGAS = 3
         IMOLE = 0
      CASE('PREDEF', 'PREDEFINED')
         IGAS = 4
         IMOLE = 1
      CASE DEFAULT
         
      END SELECT

      FAC = ONE

!     *********
!     Read card
      SELECT CASE (IGAS)
      CASE(1, 2)
         GASTYPE = 'User'
         CALL HM_GET_FLOATV('MASS', MW, IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF (MW <= ZERO) THEN
            CALL ANCMSG(MSGID = 710, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .           I1 = ID, C1 = TITR)
         END IF
         CALL HM_GET_FLOATV('ABG_cpai', CPA, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('ABG_cpbi', CPB, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('ABG_cpci', CPC, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_D', CPD, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_E1', CPE, IS_AVAILABLE, LSUBMODEL, UNITAB)

         CALL HM_GET_FLOATV_DIM('MASS', FAC, IS_AVAILABLE, LSUBMODEL, UNITAB)
         
         CPF = ZERO
         IF (IGAS == 2) THEN
            CALL HM_GET_FLOATV('MAT_F', CPF, IS_AVAILABLE, LSUBMODEL, UNITAB)
         END IF
      CASE (3)
         GASTYPE = 'User'
         CPB = ZERO
         CPC = ZERO
         CPD = ZERO
         CPE = ZERO
         CPF = ZERO
         CALL HM_GET_FLOATV('MAT_BSAT', CPA, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_RSAT', CV, IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF (CPA <= ZERO .OR. CV <= ZERO) THEN
            CALL ANCMSG(MSGID = 916, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .           I1 = ID, C1 = TITR)
         END IF
         IF (CPA <= CV) THEN
            CALL ANCMSG(MSGID = 917, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .           I1 = ID, C1 = TITR)
         END IF
      CASE(4)
         CALL HM_GET_STRING('GAS', USERGAS_NAME, ncharfield, IS_AVAILABLE)         
         IF (USERGAS_NAME(1:3) == 'N2O') THEN
            GASTYPE = 'Nitrous oxide'
            MW = 0.04401
            CPA = 27.67988
            CPB = 5.1149E-02
            CPC = -3.0645E-05
            CPD =  6.8479E-09
            CPE = -1.5791E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:2) == 'N2') THEN
            GASTYPE = 'Nitrogen'
            MW = 0.02801
            CPA = 26.0920000
            CPB = 8.2188E-03
            CPC = -1.9761E-06
            CPD = 1.5927E-10
            CPE = 4.4434E+04
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:2) == 'O2') THEN
            GASTYPE = 'Oxygen'
            MW = 0.032
            CPA = 29.659
            CPB = 6.1373E-03
            CPC = -1.1865E-06
            CPD = 9.5780E-11
            CPE = -2.1966E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:3) == 'CO2') THEN
            GASTYPE = 'Carbon dioxide'
            MW = 0.04401
            CPA = 24.997350
            CPB = 5.5187E-02
            CPC = -3.3691E-05
            CPD = 7.9484E-09
            CPE = -1.3664E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:2) == 'CO') THEN
            GASTYPE = 'Carbon monoxide'
            MW = 0.02801
            CPA = 25.567590
            CPB = 6.0961E-03
            CPC = 4.0547E-06
            CPD = -2.6713E-09
            CPE = 1.3102E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:2) == 'AR') THEN
            GASTYPE = 'Argon'
            MW = 0.03995
            CPA = 20.786
            CPB = 2.8259E-10
            CPC = -1.4642E-13
            CPD = 1.0921E-17
            CPE = -3.6614E-02
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:2) == 'NE') THEN
            GASTYPE = 'Neon'
            MW = 0.02018
            CPA = 20.786030
            CPB = 4.8506E-13
            CPC = -1.5829E-16
            CPD = 1.5251E-20
            CPE = 3.1963E-05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:2)  == 'HE') THEN
            GASTYPE = 'Helium'
            MW = 0.004
            CPA = 20.786030
            CPB = 4.8506E-13
            CPC = -1.5829E-16
            CPD = 1.5251E-20
            CPE = 3.1963E-05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:3)  == 'H2O') THEN
            GASTYPE = 'Water vapour'
            MW = 0.01802
            CPA = 30.092
            CPB = 6.8325E-03
            CPC = 6.7934E-06
            CPD = -2.5345E-09
            CPE =  8.2139E+04
            CPF =  0.0
         ELSE IF (USERGAS_NAME(1:2)  == 'H2') THEN
            GASTYPE = 'Hydrogen'
            MW = 0.00202
            CPA = 33.066178
            CPB = -1.1363E-02
            CPC = 1.1433E-05
            CPD = -2.7729E-09
            CPE = -1.5856E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:3)  == 'NH3') THEN
            GASTYPE = 'Ammonia'
            MW = 0.01703
            CPA = 19.995630
            CPB = 4.9771E-02
            CPC = -1.5376E-05
            CPD = 1.9212E-09
            CPE = 1.8917E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:3)  == 'H2S') THEN
            GASTYPE = 'Hydrogen sulphide'
            MW = 0.03408
            CPA = 26.884120
            CPB = 1.8678E-02
            CPC = 3.4342E-06
            CPD = -3.3787E-09
            CPE = 1.3588E+05
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:4)  == 'C6H6') THEN
            GASTYPE = 'Benzene'
            MW = 0.07811
            CPA = -36.220000
            CPB = 4.8475E-01
            CPC = -3.1570E-04
            CPD = 7.7620E-08
            CPE = 0.0000E+00
            CPF = 0.0
         ELSE IF (USERGAS_NAME(1:3)  == 'AIR') THEN
            GASTYPE = 'Air'
            MW = 0.02896
            CPA = 26.789065
            CPB = 7.7213E-03
            CPC = -1.8027E-06
            CPD = 1.4705E-10
            CPE = 1.1359E+04
            CPF = 0.0
         ELSE
            CALL ANCMSG(MSGID = 722, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .           I1 = ID, C1 = TITR, C2 = USERGAS_NAME)
         END IF
      END SELECT

      IF (IGAS /= 3.AND. MW <= ZERO) THEN
          CALL ANCMSG(MSGID = 710, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                I1 = ID, C1 = TITR)
      ENDIF

!     ***********************************
!     Units for hard-coded default values
      FAC_M = FAC_M_WORK
      FAC_L = FAC_L_WORK
      FAC_T = FAC_T_WORK
      FAC_CP = FAC_L * FAC_L / (FAC_T * FAC_T)

      R_IGC1 = R_IGC / FAC_M / FAC_L / FAC_L * (FAC_T * FAC_T)

      IF (IMOLE == 1) THEN
        CPA = CPA / MW * FAC
        CPB = CPB / MW * FAC
        CPC = CPC / MW * FAC
        CPD = CPD / MW * FAC
        CPE = CPE / MW * FAC
        CPF = CPF / MW * FAC
      ENDIF
      IF(IGAS == 4) THEN
        MW = MW  / FAC_M
        CPA = CPA / FAC_CP
        CPB = CPB / FAC_CP
        CPC = CPC / FAC_CP
        CPD = CPD / FAC_CP
        CPE = CPE / FAC_CP
        CPF = CPF / FAC_CP
      ENDIF
      IF (IGAS == 3) THEN
         MW = R_IGC1 / (CPA - CV)
      ENDIF
      
!     **************************
!     Store in IPM and PM arrays
      IPM(1) = ID
      PM(20) = MW
      PM(21) = CPA
      PM(22) = CPB
      PM(23) = CPC
      PM(24) = CPD
      PM(25) = CPE
      PM(26) = CPF
      PM(27) = R_IGC1 
c
!     *********
!     Print out
      WRITE (IOUT, 1001) TRIM(TITR), ID
      WRITE (IOUT,1000)
      IF (IS_ENCRYPTED)THEN
        WRITE (IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IGAS == 3) THEN
          WRITE (IOUT, 1100) GASTYPE, CPA, CV, MW, R_IGC1
        ELSE
          WRITE (IOUT, 1200) GASTYPE, MW, CPA, CPB, CPC, CPD, CPE, CPF, R_IGC1
        ENDIF
      ENDIF
C-----------------------------------------------
C     E n d   o f   S u b r o u t i n e
C-----------------------------------------------
      RETURN
 1000 FORMAT(
     & 7X,'GAS CHARACTERISTICS',/,
     & 7X,'-------------------',//)
 1001 FORMAT(//
     & 5X,A,/,
     & 5X,40HMATERIAL NUMBER . . . . . . . . . . . .=,I10/,
     & 5X,'GAS',/)
 1100 FORMAT(
     & 5X,42HGAS TYPE. . . . . . . . . . . . . . . .=  ,A/,
     & 5X,40HSPECIFIC HEAT AT CONSTANT PRESSURE CP .=,E12.4/,
     & 5X,40HSPECIFIC HEAT AT CONSTANT VOLUME CV. . =,E12.4/,
     & 5X,40HMOLECULAR WEIGHT  . . . . . . . . . . .=,E12.4/,
     & 5X,40HUNIVERSAL GAS CONSTANT. . . . . . . . .=,E12.4//)
 1200 FORMAT(
     & 5X,42HGAS TYPE. . . . . . . . . . . . . . . .=  ,A/,
     & 5X,40HMOLECULAR WEIGHT  . . . . . . . . . . .=,E12.4/,
     & 5X,40HCOEFFICIENT CPA . . . . . . . . . . . .=,E12.4/,
     & 5X,40HCOEFFICIENT CPB . . . . . . . . . . . .=,E12.4/,
     & 5X,40HCOEFFICIENT CPC . . . . . . . . . . . .=,E12.4/,
     & 5X,40HCOEFFICIENT CPD . . . . . . . . . . . .=,E12.4/,
     & 5X,40HCOEFFICIENT CPE . . . . . . . . . . . .=,E12.4/,
     & 5X,40HCOEFFICIENT CPF . . . . . . . . . . . .=,E12.4/,
     & 5X,40HUNIVERSAL GAS CONSTANT. . . . . . . . .=,E12.4//)
C-----------
      RETURN
      END
