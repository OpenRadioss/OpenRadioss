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
!||    hm_read_mpc                ../starter/source/constraints/general/mpc/hm_read_mpc.F
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    fretitl                    ../starter/source/starter/freform.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
!||    hm_sz_r2r                  ../starter/source/coupling/rad2rad/routines_r2r.F
!||    ifrontplus                 ../starter/source/spmd/node/frontplus.F
!||    kinset                     ../starter/source/constraints/general/kinset.F
!||    usr2sys                    ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    r2r_mod                    ../starter/share/modules1/r2r_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MPC(RBUF   ,IBUFNC ,IBUFNN  ,IBUFDL  ,IBUFSK  ,
     2                       ISKN   ,ITAB   ,ITABM   ,LAG_NCF ,LAG_NKF ,
     3                       LAG_NHF,IKINE  ,IKINE1LAG,NOM_OPT,ITAGND  ,
     4                       LSUBMODEL,UNITAB)
C-----------------------------------------------
      USE R2R_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LAG_NCF, LAG_NKF, LAG_NHF, ITAB(*), ITABM(*),
     .        ISKN(LISKN,*),IBUFNC(*),IBUFNN(*),IBUFDL(*),IBUFSK(*),
     .        IKINE(*),IKINE1LAG(*),ITAGND(*)
      my_real :: RBUF(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, J, ID, NOD, IDDL, ISKW, NUMC, KF, NOSYS, NMP
      my_real COEF
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARKEY) :: KEY
      CHARACTER :: MESS*40
      DATA MESS/'MULTI-POINT CONSTRAINTS          '/
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C======================================================================|
      WRITE(IOUT,1000)

      IS_AVAILABLE = .FALSE.
C
      CALL HM_OPTION_START('/MPC')
C
      KF = 0
      NMP = 0
      DO I=1,NUMMPC
        NMP=NMP+1
C----------Multidomaines --> on ignore les mpc non tages----------
        IF(NSUBDOM>0)THEN
            IF(TAGMPC(NMP)==0)CALL HM_SZ_R2R(TAGMPC,NMP,LSUBMODEL)
        END IF
C-----------------------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)

        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C
        CALL HM_GET_INTV('number_of_nodes',NUMC,IS_AVAILABLE,LSUBMODEL)
        DO II=1,NUMC
          CALL HM_GET_INT_ARRAY_INDEX('node_ID',NOD,II,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('Idof',IDDL,II,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('skew_ID',ISKW,II,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('alpha',COEF,II,IS_AVAILABLE,LSUBMODEL,UNITAB)
          KF   = KF   + 1
          IF (COEF==ZERO) COEF = ONE
          RBUF  (KF) = COEF
          NOSYS = USR2SYS(NOD,ITABM,MESS,ID)
          IF (NS10E>0) THEN 
            IF(ITAGND(NOSYS)/=0) THEN
C------- error out
           CALL ANCMSG(MSGID=1208,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(NOSYS),
     .                C1='MPC ',
     .                I2=ID,
     .                C2='MPC ')
            ENDIF
          END IF
          CALL KINSET(512,ITAB(NOSYS),IKINE(NOSYS),7,0,IKINE1LAG(NOSYS))
          IBUFNN(KF) = NOSYS
          CALL IFRONTPLUS(NOSYS,1)
          IBUFDL(KF) = IDDL
          IBUFSK(KF) = 0
          DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
            IF(ISKW==ISKN(4,J+1)) THEN
              IBUFSK(KF) = J+1
              GO TO 10
            ENDIF
          ENDDO
          CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                C1='MPC',
     .                C2='MPC',
     .                I1=ID,I2=ISKW,C3=TITR)
 10       CONTINUE
          IF (ISKW==0) THEN
            LAG_NKF = LAG_NKF + 1
          ELSE
            LAG_NKF = LAG_NKF + 3
          ENDIF
        ENDDO
        IBUFNC(I) = NUMC
        WRITE(IOUT,1101) ID,NUMC
        WRITE(IOUT,1102) (ITAB(IBUFNN(J)),IBUFDL(J),ISKN(4,IBUFSK(J)),RBUF(J),
     .                    J=KF-NUMC+1,KF)
      ENDDO
C---
      LAG_NHF = LAG_NHF + NUMMPC*(NUMMPC-1)
      LAG_NCF = LAG_NCF + NUMMPC
C---
      RETURN
 1000 FORMAT(//
     .'       MULTI-POINT CONSTRAINTS   '/
     . '      ----------------------   ')
 1101 FORMAT( 10X,'MPC ID. . . . . . . . . . . . . .',I10
     .       /10X,'NUMBER OF POINTS. . . . . . . . .',I10
     .       /10X,'CONSTRAINT LIST :'
     .       /5X, ' NODE       DDL      SKEW        COEFFICIENT'/)
 1102 FORMAT( 3I10,1PG20.13/)
C---
      RETURN
      END


!||====================================================================
!||    hm_read_mpc0           ../starter/source/constraints/general/mpc/hm_read_mpc.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MPC0 (LEN, LSUBMODEL)
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      TYPE(SUBMODEL_DATA), DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID, NUMC 
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL IS_AVAILABLE
C======================================================================|
      IS_AVAILABLE = .FALSE.
C
      ! Start reading /MPC card 
      CALL HM_OPTION_START('/MPC')

      LEN = 0
      DO I=1,NUMMPC
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('number_of_nodes',NUMC,IS_AVAILABLE,LSUBMODEL)
        LEN = LEN+NUMC
      ENDDO
C---
      RETURN
      END
