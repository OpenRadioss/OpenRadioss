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
Chd|  HM_READ_MAT75                 source/materials/mat/mat075/hm_read_mat75.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT75(UPARAM , MAXUPARAM, NUPARAM,
     .                         NUVAR  , IFUNC,     MAXFUNC, NFUNC, PARMAT,
     .                         UNITAB , MAT_ID,    TITR,    MTAG,  LSUBMODEL, PM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW75 WITH HM READER 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE ELBUFTAG_MOD
C-----------------------------------------------
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
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      my_real, INTENT(INOUT)                :: PARMAT(100), UPARAM(MAXUPARAM), PM(NPROPM)
      INTEGER, INTENT(INOUT)                :: IFUNC(MAXFUNC), NFUNC, MAXFUNC, MAXUPARAM,
     .     NUPARAM, NUVAR
      INTEGER, INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)   :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MATS,IFLAG1,IFLAG2,ITEMAX
      my_real
     .        CE, PE, PS, NN, TOL, YOUNG, NU, BULK, RHO0, RHOR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C     S o u r c e 
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C----------------------------------------------------------------
C     #RhoO rho_ref
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
      CALL HM_GET_FLOATV('MAT_E', YOUNG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU', NU, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV('MAT1', MATS, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('HFLAG1', IFLAG1, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('HFLAG2', IFLAG2, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Nppmax', ITEMAX, IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_FLOATV('MAT_PPRES', PE, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YPRES', PS, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP1', NN, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_Tol', TOL, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     
      IF(IFLAG1 <= 0) IFLAG1=1
      IF(IFLAG2 <= 0) IFLAG2=1
      IF(ITEMAX <= 0) ITEMAX=5
      IF(NN  <= ZERO) NN=TWO
      IF(TOL <= ZERO) TOL=EM08

      NUVAR = 4
      PM(1) = RHOR
      PM(89) = RHO0
      BULK=YOUNG/THREE/(ONE-TWO*NU)
      CE=SQRT(BULK/RHO0)
C     
      UPARAM(1) = BULK
      UPARAM(2) = PE
      UPARAM(3) = PS
      UPARAM(4) = NN
      UPARAM(5) = TOL
      UPARAM(6) = MATS
      UPARAM(7) = IFLAG1
      UPARAM(8) = IFLAG2
      UPARAM(9) = YOUNG/(ONE+NU)
      UPARAM(10)= HALF*UPARAM(9)
      UPARAM(17)= ITEMAX
C     
      NUPARAM= 17
C     
      PARMAT(1) = BULK
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU) ! ==TWO*G/(BULK+sFOUR_OVER_3*G)
C----------------------
      WRITE(IOUT, 900) TRIM(TITR),MAT_ID,75
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT,950)RHO0
         WRITE(IOUT,1100) YOUNG, NU, UPARAM(10), BULK
         WRITE(IOUT,1200) MATS, IFLAG1, IFLAG2, ITEMAX, PE, PS, NN
         WRITE(IOUT,1300) TOL, CE
      ENDIF
C--------
C     ERRORS
C--------
      IF (PE > PS) THEN
         CALL ANCMSG(MSGID=897,
     .        MSGTYPE=MSGERROR,
     .        ANMODE=ANINFO,
     .        I1=MAT_ID,
     .        C1=TITR)
      ENDIF
C     
      RETURN
C     
  900 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 950  FORMAT(
     &     5X,'INITIAL DENSITY    . . . . . . . . . . .=',1PG20.13/)
 1000 FORMAT(
     &     5X,'  POROUS MATERIAL',/,
     &     5X,'  ---------------',//)
 1100 FORMAT(
     &     5X,'YOUNG',1H','S MODULUS . . . . . . . . . . . . =',1PG20.13/,
     &5X,'POISSON',1H','S RATIO . . . . . . . . . . . . =',1PG20.13/,
     &     5X,'SHEAR MODULUS . . . . . . . . . . . . . =',1PG20.13/,
     &     5X,'BULK  MODULUS . . . . . . . . . . . . . =',1PG20.13//)
 1200 FORMAT(
     &     5X,'SOLID MATERIAL IDENTIFIER . . . . . . . =',I10/
     &     5X,'PRESSURE FORMULATION FLAG . . . . . . . =',I10/
     &     5X,'DEVIATORIC STRESSES FORMULATION FLAG. . =',I10/
     &     5X,'MAXIMUM NUMBER OF ITERATIONS. . . . . . =',I10//
     &     5X,'ELASTIC COMPACTION PRESSURE (PE). . . . =',1PG20.13/
     &     5X,'SOLID COMPACTION PRESSURE (PS). . . . . =',1PG20.13/
     &     5X,'EXPONENT (NN) . . . . . . . . . . . . . =',1PG20.13)
 1300 FORMAT(
     &     5X,'CONVERGENCE TOLERANCE (TOL) . . . . . . =',1PG20.13/
     &     5X,'INITIAL POROUS BULK SOUND SPEED . . . . =',1PG20.13)
      END
