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
Chd|  HM_READ_PROP33_UNIV_JNT       source/properties/spring/hm_read_prop33_univ_jnt.F
Chd|-- called by -----------
Chd|        HM_READ_PROP33                source/properties/spring/hm_read_prop33.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP33_UNIV_JNT(IOUT, ITYP, PARGEO,IS_CRYPTED,
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
      INTEGER IOUT,ITYP,IUNIT
      my_real PARGEO(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL IS_CRYPTED
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,IDSK1,IDSK2,IFUN_RY,IFUN_RZ,IFUN_CRY,IFUN_CRZ,
     .        ZEROI,OFLAG
      my_real
     .   XK,XTYP,XFLG,XSK1,XSK2,KNN,KRY,KRZ,CR,CRY,CRZ,MASS,INER,
     .   FAC_M,FAC_L,FAC_T,FAC_CT,FAC_CR,FAC_KT,FAC_KR,FAC_CTX,FAC_CRX,
     .   FAC_MM
C-----------------------------------------------
      INTEGER SET_U_PNU,SET_U_GEO,KFUNC
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      DATA ZEROI/0/
      LOGICAL IS_AVAILABLE
C=======================================================================
C----   UNIVERSAL JOINT
C=======================================================================
      FAC_M = UNITAB%FAC_M(IUNIT)
      FAC_L = UNITAB%FAC_L(IUNIT)
      FAC_T = UNITAB%FAC_T(IUNIT)
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
      CALL HM_GET_INTV('Yr_fun',IFUN_RY,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Zr_fun',IFUN_RZ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('Xk',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Cr',CR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Kn',KNN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Kry',KRY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Krz',KRZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C---  viscosity
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Cry_Fun',IFUN_CRY,IS_AVAILABLE,LSUBMODEL)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
      CALL HM_GET_INTV('Crz_Fun',IFUN_CRZ,IS_AVAILABLE,LSUBMODEL)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('Cry',CRY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
      CALL HM_GET_FLOATV('Crz',CRZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF(.NOT.IS_AVAILABLE) OFLAG = OFLAG + 1
C-----------------------
      IF (IDSK1.LE.0.OR.IDSK2.LE.0.) THEN
         CALL ANCMSG(MSGID=386,
     .  	     MSGTYPE=MSGERROR,
     .  	     ANMODE=ANINFO_BLIND_1,
     .  	     I1=ID,
     .  	     C1=TITR)
      ENDIF
      IF (KNN.EQ.0.) THEN
         CALL ANCMSG(MSGID=387,
     .  	     MSGTYPE=MSGERROR,
     .  	     ANMODE=ANINFO_BLIND_1,
     .  	     I1=ID,
     .               C1=TITR)
      ENDIF
      IF (CR.LT.ZERO.OR.CR.GT.1.) THEN
         CALL ANCMSG(MSGID=388,
     .  	     MSGTYPE=MSGERROR,
     .  	     ANMODE=ANINFO_BLIND_1,
     .  	     I1=ID,
     .  	     C1=TITR)
      ENDIF
      IF (CR.EQ.ZERO) CR = FIVEEM2
C
      XTYP = ITYP
      XFLG = ZERO
      XSK1 = IDSK1
      XSK2 = IDSK2
      MASS = ZERO
      INER = ZERO
C
      IF(CRY.EQ.ZERO.AND.IFUN_CRY.NE.0)CRY = ONE
      IF(CRZ.EQ.ZERO.AND.IFUN_CRZ.NE.0)CRZ = ONE
      IF(KRY.EQ.ZERO.AND.IFUN_RY.NE.0) KRY = ONE
      IF(KRZ.EQ.ZERO.AND.IFUN_RZ.NE.0) KRZ = ONE
C-----------------------
      IF (IFUN_RY  /= 0) KRY = KRY * FAC_MM
      IF (IFUN_RZ  /= 0) KRZ = KRZ * FAC_MM
      IF (IFUN_CRY /= 0) CRY = CRY * FAC_MM 
      IF (IFUN_CRZ /= 0) CRZ = CRZ * FAC_MM 
C-----------------------
      PARGEO(1) = 0
      PARGEO(2) = XK
      PARGEO(3) = 0
C-----------------------
      IERROR = SET_U_GEO(1,XTYP)
      IERROR = SET_U_GEO(2,XSK1)
      IERROR = SET_U_GEO(3,XSK2)
      IERROR = SET_U_GEO(4,KNN)
      IERROR = SET_U_GEO(5,KNN)
      IERROR = SET_U_GEO(6,KNN)
      IERROR = SET_U_GEO(7,KNN)
      IERROR = SET_U_GEO(8,KRY)
      IERROR = SET_U_GEO(9,KRZ)
      IERROR = SET_U_GEO(10,KNN)
      IERROR = SET_U_GEO(11,ZERO)
      IERROR = SET_U_GEO(12,MASS)
      IERROR = SET_U_GEO(13,INER)
      IERROR = SET_U_GEO(14,XFLG)
      IERROR = SET_U_GEO(15,CR)
      IERROR = SET_U_GEO(16,CR)
      IERROR = SET_U_GEO(17,CR)
      IERROR = SET_U_GEO(18,CR)
      IERROR = SET_U_GEO(19,ZERO)
      IERROR = SET_U_GEO(20,ZERO)
      IERROR = SET_U_GEO(21,ZERO)
      IERROR = SET_U_GEO(22,ZERO)
      IERROR = SET_U_GEO(23,ZERO)
      IERROR = SET_U_GEO(24,ZERO)
      IERROR = SET_U_GEO(25,CRY)
      IERROR = SET_U_GEO(26,CRZ)
      IERROR = SET_U_GEO(27,FAC_CTX)		
      IERROR = SET_U_GEO(28,FAC_CRX)		
      IERROR = SET_U_PNU(1,ZEROI,KFUNC)
      IERROR = SET_U_PNU(2,ZEROI,KFUNC)
      IERROR = SET_U_PNU(3,ZEROI,KFUNC)
      IERROR = SET_U_PNU(4,ZEROI,KFUNC)
      IERROR = SET_U_PNU(5,IFUN_RY,KFUNC)
      IERROR = SET_U_PNU(6,IFUN_RZ,KFUNC)
      IERROR = SET_U_PNU(7,ZEROI,KFUNC)
      IERROR = SET_U_PNU(8,ZEROI,KFUNC)
      IERROR = SET_U_PNU(9,ZEROI,KFUNC)
      IERROR = SET_U_PNU(10,ZEROI,KFUNC)
      IERROR = SET_U_PNU(11,IFUN_CRY,KFUNC)
      IERROR = SET_U_PNU(12,IFUN_CRZ,KFUNC)
C-----------------------
      WRITE(IOUT,500)
      IF(IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
       IF (OFLAG.EQ.4) THEN
        WRITE(IOUT,1001) IDSK1,IDSK2,XK,CR,KNN,KRY,KRZ,
     .  		 IFUN_RY,IFUN_RZ
       ELSE
        WRITE(IOUT,1000) IDSK1,IDSK2,XK,CR,KNN,KRY,KRZ,
     .  		 IFUN_RY,IFUN_RZ,CRY,CRZ,IFUN_CRY,IFUN_CRZ
       ENDIF
      ENDIF
C-----------------------
      RETURN
C-----------------------
 500  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . UNIVERSAL JOINT'//)
 1000 FORMAT(
     & 5X,'SKEW 1 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'SKEW 2 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'STIFFNESS FOR INTERFACE K=E*A/L. . . . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . =',1PG20.13/,
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR ROTATIONAL STIFFNESS KYY. . . . =',1PG20.13/,
     & 5X,'LINEAR ROTATIONAL STIFFNESS KZZ. . . . =',1PG20.13/,
     & 5X,'USER RY TORSION FUNCTION ID. . . . . . =',I10/,
     & 5X,'USER RZ TORSION FUNCTION ID. . . . . . =',I10/,
     & 5X,'LINEAR DAMPING CRY . . . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR DAMPING CRZ . . . . . . . . . . =',1PG20.13/,
     & 5X,'USER RY DAMPING FUNCTION . . . . . . . =',I10/,
     & 5X,'USER RZ DAMPING FUNCTION . . . . . . . =',I10//)
 1001 FORMAT(
     & 5X,'SKEW 1 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'SKEW 2 FRAME ID. . . . . . . . . . . . =',I10/,
     & 5X,'STIFFNESS FOR INTERFACE K=E*A/L. . . . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . =',1PG20.13/,
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . . =',1PG20.13/,
     & 5X,'LINEAR ROTATIONAL STIFFNESS KYY. . . . =',1PG20.13/,
     & 5X,'LINEAR ROTATIONAL STIFFNESS KZZ. . . . =',1PG20.13/,
     & 5X,'USER RY TORSION FUNCTION ID. . . . . . =',I10/,
     & 5X,'USER RZ TORSION FUNCTION ID. . . . . . =',I10//)
 999  CALL FREERR(1)
      RETURN
      END
