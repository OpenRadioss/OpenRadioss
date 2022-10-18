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
Chd|  HM_READ_PROP33_PLAN_JNT       source/properties/spring/hm_read_prop33_plan_jnt.F
Chd|-- called by -----------
Chd|        HM_READ_PROP33                source/properties/spring/hm_read_prop33.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP33_PLAN_JNT(IOUT, ITYP, SKFLAG, PARGEO,IS_ENCRYPTED,
     .                       UNITAB,IUNIT,ID,TITR,LSUBMODEL)
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+------------------------------------------|
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT, ITYP, SKFLAG,IUNIT
      my_real PARGEO(*) 

      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL IS_ENCRYPTED
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,IDSK1,IDSK2,IFUN_YY,IFUN_ZZ,IFUN_RX,
     .        IFUN_CYY,IFUN_CZZ,IFUN_CRX, ZEROI,OFLAG
      my_real 
     .  XK,XTYP,XFLG,XSK1,XSK2,KNN,KYY,KZZ,KRX,CR,CYY,CZZ,CRX,MASS,INER,
     .   FAC_M,FAC_L,FAC_T,FAC_CT,FAC_CR,FAC_KT,FAC_KR,FAC_CTX,FAC_CRX,
     .   FAC_FF,FAC_MM
C-----------------------------------------------
      INTEGER SET_U_PNU,SET_U_GEO,KFUNC
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      DATA ZEROI/0/
      LOGICAL IS_AVAILABLE
C=======================================================================
C----   PLANAR JOINT
C=======================================================================
      FAC_M = UNITAB%FAC_M(IUNIT)
      FAC_L = UNITAB%FAC_L(IUNIT)
      FAC_T = UNITAB%FAC_T(IUNIT)
      FAC_FF = FAC_M / FAC_T
      FAC_MM = ONE / FAC_T
      FAC_CT = FAC_M / FAC_T
      FAC_CR = FAC_M * FAC_L**2 / FAC_T
      FAC_KT = FAC_CT / FAC_T
      FAC_KR = FAC_CR / FAC_T
      FAC_CTX = FAC_T / FAC_L
      FAC_CRX = FAC_T
      OFLAG = 0
C
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Idsk1',IDSK1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idsk2',IDSK2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Yt_fun',IFUN_YY,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Zt_fun',IFUN_ZZ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Xr_fun',IFUN_RX,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('Xk',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Cr',CR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Kn',KNN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Kty',KYY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Ktz',KZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Krx',KRX,IS_AVAILABLE,LSUBMODEL,UNITAB)
C---  viscosity
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Cty_Fun',IFUN_CYY,IS_AVAILABLE,LSUBMODEL)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
      CALL HM_GET_INTV('Ctz_Fun',IFUN_CZZ,IS_AVAILABLE,LSUBMODEL)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
      CALL HM_GET_INTV('Crx_Fun',IFUN_CRX,IS_AVAILABLE,LSUBMODEL)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('Cty',CYY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
      CALL HM_GET_FLOATV('Ctz',CZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
      CALL HM_GET_FLOATV('Crx',CRX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
C-----------------------
      IF (IDSK1<=0.OR.IDSK1<=0) THEN
         CALL ANCMSG(MSGID=386,
     .  	     MSGTYPE=MSGERROR,
     .  	     ANMODE=ANINFO_BLIND_1,
     .  	     I1=ID,
     .  	     C1=TITR)
      ENDIF
      IF (KNN==0.) THEN
         CALL ANCMSG(MSGID=387,
     .  	     MSGTYPE=MSGERROR,
     .  	     ANMODE=ANINFO_BLIND_1,
     .  	     I1=ID,
     .  	     C1=TITR)
      ENDIF
      IF (CR<ZERO.OR.CR>1.) THEN
         CALL ANCMSG(MSGID=388,
     .  	     MSGTYPE=MSGERROR,
     .  	     ANMODE=ANINFO_BLIND_1,
     .  	     I1=ID,
     .  	     C1=TITR)
      ENDIF
      IF (CR==ZERO) CR = FIVEEM2
C
      XTYP = ITYP
      XFLG = SKFLAG
      XSK1 = IDSK1
      XSK2 = IDSK2
      MASS = ZERO
      INER = ZERO
C
      IF(KYY==ZERO.AND.IFUN_YY/=0) KYY = ONE
      IF(KZZ==ZERO.AND.IFUN_ZZ/=0) KZZ = ONE
      IF(KRX==ZERO.AND.IFUN_RX/=0) KRX = ONE
      IF(CYY==ZERO.AND.IFUN_CYY/=0)CYY = ONE
      IF(CZZ==ZERO.AND.IFUN_CZZ/=0)CZZ = ONE
      IF(CRX==ZERO.AND.IFUN_CRX/=0)CRX = ONE
C-----------------------
      IF (IFUN_YY  /= 0) KYY = KYY * FAC_FF 
      IF (IFUN_ZZ  /= 0) KZZ = KZZ * FAC_FF
      IF (IFUN_RX  /= 0) KRX = KRX * FAC_MM
      IF (IFUN_CYY /= 0) CYY = CYY * FAC_FF 
      IF (IFUN_CZZ /= 0) CZZ = CZZ * FAC_FF 
      IF (IFUN_CRX /= 0) CRX = CRX * FAC_MM 
C-----------------------
      PARGEO(1) = 0
      PARGEO(2) = XK
      PARGEO(3) = 0
C-----------------------
      IERROR = SET_U_GEO(1,XTYP)
      IERROR = SET_U_GEO(2,XSK1)
      IERROR = SET_U_GEO(3,XSK2)
      IERROR = SET_U_GEO(4,KNN)
      IERROR = SET_U_GEO(5,KYY)
      IERROR = SET_U_GEO(6,KZZ)
      IERROR = SET_U_GEO(7,KRX)
      IERROR = SET_U_GEO(8,KNN)
      IERROR = SET_U_GEO(9,KNN)
      IERROR = SET_U_GEO(10,KNN)
      IERROR = SET_U_GEO(11,ZERO)
      IERROR = SET_U_GEO(12,MASS)
      IERROR = SET_U_GEO(13,INER)
      IERROR = SET_U_GEO(14,XFLG)
      IERROR = SET_U_GEO(15,CR)
      IERROR = SET_U_GEO(16,ZERO)
      IERROR = SET_U_GEO(17,ZERO)
      IERROR = SET_U_GEO(18,ZERO)
      IERROR = SET_U_GEO(19,CR)
      IERROR = SET_U_GEO(20,CR)
      IERROR = SET_U_GEO(21,ZERO)
      IERROR = SET_U_GEO(22,CYY)
      IERROR = SET_U_GEO(23,CZZ)
      IERROR = SET_U_GEO(24,CRX)
      IERROR = SET_U_GEO(25,ZERO)
      IERROR = SET_U_GEO(26,ZERO)
      IERROR = SET_U_GEO(27,FAC_CTX)		
      IERROR = SET_U_GEO(28,FAC_CRX)		
      IERROR = SET_U_PNU(1,ZEROI,KFUNC)
      IERROR = SET_U_PNU(2,IFUN_YY,KFUNC)
      IERROR = SET_U_PNU(3,IFUN_ZZ,KFUNC)
      IERROR = SET_U_PNU(4,IFUN_RX,KFUNC)
      IERROR = SET_U_PNU(5,ZEROI,KFUNC)
      IERROR = SET_U_PNU(6,ZEROI,KFUNC)
      IERROR = SET_U_PNU(7,ZEROI,KFUNC)
      IERROR = SET_U_PNU(8,IFUN_CYY,KFUNC)
      IERROR = SET_U_PNU(9,IFUN_CZZ,KFUNC)
      IERROR = SET_U_PNU(10,IFUN_CRX,KFUNC)
      IERROR = SET_U_PNU(11,ZEROI,KFUNC)
      IERROR = SET_U_PNU(12,ZEROI,KFUNC)
C-----------------------
      WRITE(IOUT,500)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
       IF (OFLAG==6) THEN
        WRITE(IOUT,1001) IDSK1,IDSK2,XK,CR,KNN,KYY,KZZ,KRX,
     .  		 IFUN_YY,IFUN_ZZ,IFUN_RX
       ELSE
        WRITE(IOUT,1000) IDSK1,IDSK2,XK,CR,KNN,KYY,KZZ,KRX,
     .  		 IFUN_YY,IFUN_ZZ,IFUN_RX,
     .  		 CYY,CZZ,CRX,IFUN_CYY,IFUN_CZZ,IFUN_CRX
       ENDIF
      ENDIF
C-----------------------
      RETURN
 500  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . . . PLANAR JOINT'//)
 1000 FORMAT(
     & 5X,'SKEW 1 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'SKEW 2 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'STIFFNESS FOR INTERFACE K=E*A/L. . . . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . =',1PG20.13/,
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR TRANSLATIONAL STIFFNESS KYY . . =',1PG20.13/,
     & 5X,'LINEAR TRANSLATIONAL STIFFNESS KZZ . . =',1PG20.13/,
     & 5X,'LINEAR TORSION STIFFNESS KRX . . . . . =',1PG20.13/,
     & 5X,'USER Y TRANSLATION FUNCTION. . . . . . =',I10/,
     & 5X,'USER Z TRANSLATION FUNCTION. . . . . . =',I10/,
     & 5X,'USER RX TORSION FUNCTION ID. . . . . . =',I10/,
     & 5X,'LINEAR DAMPING CYY . . . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR DAMPING CZZ . . . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR DAMPING CRX . . . . . . . . . . =',1PG20.13/,
     & 5X,'USER YY DAMPING FUNCTION . . . . . . . =',I10/,
     & 5X,'USER ZZ DAMPING FUNCTION . . . . . . . =',I10/,
     & 5X,'USER RX DAMPING FUNCTION . . . . . . . =',I10//)
 1001 FORMAT(
     & 5X,'SKEW 1 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'SKEW 2 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'STIFFNESS FOR INTERFACE K=E*A/L. . . . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . =',1PG20.13/,
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR TRANSLATIONAL STIFFNESS KYY . . =',1PG20.13/,
     & 5X,'LINEAR TRANSLATIONAL STIFFNESS KZZ . . =',1PG20.13/,
     & 5X,'LINEAR TORSION STIFFNESS KRX . . . . . =',1PG20.13/,
     & 5X,'USER Y TRANSLATION FUNCTION. . . . . . =',I10/,
     & 5X,'USER Z TRANSLATION FUNCTION. . . . . . =',I10/,
     & 5X,'USER RX TORSION FUNCTION ID. . . . . . =',I10//)
      RETURN
      END
