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
Chd|  HM_READ_PROP45                source/properties/spring/hm_read_prop45.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        LEC_DOF_JNT                   source/properties/spring/hm_read_prop45.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP45(IOUT,NUVAR ,PARGEO,UNITAB,IGTYP,
     .                          ID,PROP_TAG,TITR,LSUBMODEL,IUNIT)
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
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
#include      "tablen_c.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+--------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,NUVAR,IGTYP,IUNIT
      my_real PARGEO(*)     

      INTEGER ID
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      CHARACTER*nchartitle,
     .   TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SENSOR,ZEROI,IERROR,JTYP     
      my_real KNN,CR,SCF,SENSR,SKEWR
      my_real XTYP,FAC_M,FAC_L,FAC_T,FAC_MM,FAC_FF,FAC_KT
      my_real KXX,KYY,KZZ,KRX,KRY,KRZ
      my_real CXX,CYY,CZZ,CRX,CRY,CRZ
      my_real CRXX,CRYY,CRZZ,CRRX,CRRY,CRRZ            
      my_real SDXMI,SDXMA,SDYMI,SDYMA,SDZMI,SDZMA
      my_real SAXMI,SAXMA,SAYMI,SAYMA,SAZMI,SAZMA
      my_real KFX,KFY,KFZ,KFRX,KFRY,KFRZ
      my_real FMX,FMY,FMZ,FMRX,FMRY,FMRZ,XIDSK1,XIDSK2
      my_real FCOMBX,FCOMBY,FCOMBZ,FCOMBRX,FCOMBRY,FCOMBRZ,SUMT,SUMR,ALPHA_PLUS,ALPHA_MOIN      
      INTEGER IFUN_XX,IFUN_YY,IFUN_ZZ,IFUN_RX,IFUN_RY,IFUN_RZ
      INTEGER IFUN_CXX,IFUN_CYY,IFUN_CZZ,IFUN_CRX,IFUN_CRY,IFUN_CRZ
      INTEGER IFUN_FMX,IFUN_FMY,IFUN_FMZ,IFUN_FMRX,IFUN_FMRY,IFUN_FMRZ      
      INTEGER RED,IDSK1,IDSK2,COMB_ERROR
      DATA ZEROI/0/
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SET_U_PNU,SET_U_GEO,KFUNC
      PARAMETER (KFUNC=29)
C=======================================================================
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('type',JTYP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',SENSOR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW1',IDSK1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW2',IDSK2,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
      CALL HM_GET_FLOATV('Kn',KNN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SCALE',SCF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Cr',CR,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C
      IF(.NOT. IS_ENCRYPTED)THEN
        WRITE(IOUT,1400) ID
      ELSE
        WRITE(IOUT,1500) ID
      ENDIF
C      
      NUVAR = 39
C     SKEWR - id of local joint - not used for joint type45
      SKEWR = 0
      SENSR = SENSOR
      FAC_M = UNITAB%FAC_M(IUNIT)
      FAC_L = UNITAB%FAC_L(IUNIT)
      FAC_T = UNITAB%FAC_T(IUNIT)
C
      FAC_FF = FAC_M * FAC_L / FAC_T**2      
      FAC_MM = FAC_M * FAC_L**2 / FAC_T**2
      FAC_KT = FAC_M / FAC_T**2
C     
      XTYP = JTYP
      XIDSK1 = IDSK1
      XIDSK2 = IDSK2  
C
      IF (CR<ZERO.OR.CR>1.) THEN
         CALL ANCMSG(MSGID=388,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
      ENDIF
      IF (CR==ZERO) CR = FIVEEM2
      IF (SCF<ZERO) THEN
         CALL ANCMSG(MSGID=939,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR)
      ENDIF
      IF (SCF==ZERO) THEN      
        IF (KNN==0) THEN
	  SCF = ONE
        ELSE
	  SCF = 10
        ENDIF      
      ENDIF
C      	
      KNN = KNN * FAC_KT
      
C  Mise a zero      
	 KXX = ZERO
	 KYY = ZERO
	 KZZ = ZERO
	 KRX = ZERO
	 KRY = ZERO
	 KRZ = ZERO
	 CXX = ZERO
	 CYY = ZERO
	 CZZ = ZERO
	 CRX = ZERO
	 CRY = ZERO
	 CRZ = ZERO
         CRXX = ZERO
	 CRYY = ZERO
	 CRZZ = ZERO
	 CRRX = ZERO
	 CRRY = ZERO
	 CRRZ = ZERO
C
         SDXMI = ZERO
         SDXMA = ZERO
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO
         SAXMI = ZERO
         SAXMA = ZERO
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C
         FCOMBX = ZERO
         FCOMBY = ZERO
         FCOMBZ = ZERO
         FCOMBRX = ZERO
         FCOMBRY = ZERO
         FCOMBRZ = ZERO
C         
         KFX = ZERO
         KFY = ZERO
         KFZ = ZERO
         KFRX = ZERO
         KFRY = ZERO
         KFRZ = ZERO
         FMX = ZERO
         FMY = ZERO
         FMZ = ZERO
         FMRX = ZERO
         FMRY = ZERO
         FMRZ = ZERO
C         
         IFUN_XX = ZEROI
         IFUN_YY = ZEROI
         IFUN_ZZ = ZEROI
         IFUN_CXX = ZEROI
         IFUN_CYY = ZEROI
         IFUN_CZZ = ZEROI
         IFUN_RX = ZEROI
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI
         IFUN_CRX = ZEROI
         IFUN_CRY = ZEROI
         IFUN_CRZ = ZEROI
         IFUN_FMX = ZEROI
         IFUN_FMY = ZEROI
         IFUN_FMZ = ZEROI	 	 
         IFUN_FMRX = ZEROI
         IFUN_FMRY = ZEROI
         IFUN_FMRZ = ZEROI              
C     
      IF (JTYP==1) THEN
C=======================================================================
C----   SPHERICAL JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,100)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 RED = 0	 
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRX,CRX,SAXMI,
     .                     SAXMA,FCOMBRX,KFRX,FMRX,IFUN_CRX,IFUN_RX,4,
     .                     FAC_MM,IFUN_FMRX,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRY,CRY,SAYMI,
     .                     SAYMA,FCOMBRY,KFRY,FMRY,IFUN_CRY,IFUN_RY,5,
     .                     FAC_MM,IFUN_FMRY,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRZ,CRZ,SAZMI,
     .                     SAZMA,FCOMBRZ,KFRZ,FMRZ,IFUN_CRZ,IFUN_RZ,6,
     .                     FAC_MM,IFUN_FMRZ,RED,ID,TITR,
     .                     LSUBMODEL)
C
         IF ((RED/=0).AND.(RED/=3)) THEN
            CALL ANCMSG(MSGID=973,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=RED,
     .                  I3=3)
	 ENDIF	 
C     
         KXX = KNN
	 KYY = KNN
	 KZZ = KNN
C
         CXX = ZERO
	 CYY = ZERO
	 CZZ = ZERO
C
         CRXX = CR
	 CRYY = CR
	 CRZZ = CR
	 CRRX = ZERO
	 CRRY = ZERO
	 CRRZ = ZERO
C
         SDXMI = ZERO
         SDXMA = ZERO
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO
C
         KFX = ZERO         
         KFY = ZERO
         KFZ = ZERO
C
         FMX = ZERO
         FMY = ZERO
         FMZ = ZERO 	  
C	 
         IFUN_XX = ZEROI
         IFUN_YY = ZEROI 
         IFUN_ZZ = ZEROI
         IFUN_CXX = ZEROI
         IFUN_CYY = ZEROI 
         IFUN_CZZ = ZEROI
         IFUN_FMX = ZEROI
         IFUN_FMY = ZEROI 
         IFUN_FMZ = ZEROI	 
C	 	  
      ELSEIF (JTYP==2) THEN
C=======================================================================
C----   REVOLUTE JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,200)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C      
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRX,CRX,SAXMI,
     .                     SAXMA,FCOMBRX,KFRX,FMRX,IFUN_CRX,IFUN_RX,4,
     .                     FAC_MM,IFUN_FMRX,RED,ID,TITR,
     .                     LSUBMODEL)        
C     
         KXX = KNN
	 KYY = KNN
	 KZZ = KNN
	 KRY = KNN
	 KRZ = KNN
C
         CXX = ZERO
	 CYY = ZERO
	 CZZ = ZERO
	 CRY = ZERO
	 CRZ = ZERO
C
         CRXX = CR
	 CRYY = CR
	 CRZZ = CR
	 CRRX = ZERO
	 CRRY = CR
	 CRRZ = CR	 
C
         SDXMI = ZERO
         SDXMA = ZERO
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO	 
C
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C
         KFX = ZERO         
         KFY = ZERO
         KFZ = ZERO
         KFRY = ZERO
         KFRZ = ZERO
C
         FMX = ZERO
         FMY = ZERO
         FMZ = ZERO
         FMRY = ZERO
         FMRZ = ZERO	 
C	 
         IFUN_XX = ZEROI
         IFUN_YY = ZEROI 
         IFUN_ZZ = ZEROI
         IFUN_CXX = ZEROI
         IFUN_CYY = ZEROI 
         IFUN_CZZ = ZEROI
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI 
         IFUN_CRY = ZEROI 
         IFUN_CRZ = ZEROI
         IFUN_FMX = ZEROI
         IFUN_FMY = ZEROI 
         IFUN_FMZ = ZEROI
         IFUN_FMRY = ZEROI 
         IFUN_FMRZ = ZEROI	 	 
C	 
      ELSEIF (JTYP==3) THEN
C=======================================================================
C----   CYLINDRICAL JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,300)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 RED = 0
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KXX,CXX,SDXMI,
     .                     SDXMA,FCOMBX,KFX,FMX,IFUN_CXX,IFUN_XX,1,
     .                     FAC_FF,IFUN_FMX,RED,ID,TITR,
     .                     LSUBMODEL)	 
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRX,CRX,SAXMI,
     .                     SAXMA,FCOMBRX,KFRX,FMRX,IFUN_CRX,IFUN_RX,4,
     .                     FAC_MM,IFUN_FMRX,RED,ID,TITR,
     .                     LSUBMODEL)
C
         IF ((RED/=0).AND.(RED/=2)) THEN
           CALL ANCMSG(MSGID=973,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=RED,
     .                 I3=2)
	 ENDIF 
C     
	 KYY = KNN
	 KZZ = KNN
	 KRY = KNN
	 KRZ = KNN
C
	 CYY = ZERO
	 CZZ = ZERO
	 CRY = ZERO
	 CRZ = ZERO
C
         CRXX = ZERO
	 CRYY = CR
	 CRZZ = CR
	 CRRX = ZERO
	 CRRY = CR
	 CRRZ = CR	 
C
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO
C
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C       
         KFY = ZERO
         KFZ = ZERO
         KFRY = ZERO
         KFRZ = ZERO
C
         FMY = ZERO
         FMZ = ZERO
         FMRY = ZERO
         FMRZ = ZERO	 
C	 
         IFUN_YY = ZEROI 
         IFUN_ZZ = ZEROI
         IFUN_CYY = ZEROI 
         IFUN_CZZ = ZEROI
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI 
         IFUN_CRY = ZEROI 
         IFUN_CRZ = ZEROI
         IFUN_FMY = ZEROI 
         IFUN_FMZ = ZEROI
         IFUN_FMRY = ZEROI 
         IFUN_FMRZ = ZEROI	 
C		 
      ELSEIF (JTYP==4) THEN
C=======================================================================
C----   PLANAR JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,400)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 RED = 0
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KYY,CYY,SDYMI,
     .                     SDYMA,FCOMBY,KFY,FMY,IFUN_CYY,IFUN_YY,2,
     .                     FAC_FF,IFUN_FMY,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KZZ,CZZ,SDZMI,
     .                     SDZMA,FCOMBZ,KFZ,FMZ,IFUN_CZZ,IFUN_ZZ,3,
     .                     FAC_FF,IFUN_FMZ,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRX,CRX,SAXMI,
     .                     SAXMA,FCOMBRX,KFRX,FMRX,IFUN_CRX,IFUN_RX,4,
     .                     FAC_MM,IFUN_FMRX,RED,ID,TITR,
     .                     LSUBMODEL)	 
C
         IF ((RED/=0).AND.(RED/=3)) THEN
           CALL ANCMSG(MSGID=973,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=RED,
     .                 I3=3)
	 ENDIF   	 
C     
	 KXX = KNN
	 KRY = KNN
	 KRZ = KNN
C
	 CXX = ZERO
	 CRY = ZERO
	 CRZ = ZERO
C
         CRXX = CR
	 CRYY = ZERO
	 CRZZ = ZERO
	 CRRX = ZERO
	 CRRY = CR
	 CRRZ = CR	 
C
         SDXMI = ZERO
         SDXMA = ZERO
C
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C
         KFX = ZERO
         KFRY = ZERO
         KFRZ = ZERO
C
         FMX = ZERO
         FMRY = ZERO
         FMRZ = ZERO 	 	 	 	 	 	 	 
C
         IFUN_XX = ZEROI 
         IFUN_CXX = ZEROI 
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI
         IFUN_CRY = ZEROI 
         IFUN_CRZ = ZEROI
         IFUN_FMX = ZEROI 
         IFUN_FMRY = ZEROI 
         IFUN_FMRZ = ZEROI                                    	 
C
      ELSEIF (JTYP==5) THEN
C=======================================================================
C----   UNIVERSAL  JOINT
C=================================================================	 			 
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,500)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 RED = 0	 
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRY,CRY,SAYMI,
     .                     SAYMA,FCOMBRY,KFRY,FMRY,IFUN_CRY,IFUN_RY,5,
     .                     FAC_MM,IFUN_FMRY,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRZ,CRZ,SAZMI,
     .                     SAZMA,FCOMBRZ,KFRZ,FMRZ,IFUN_CRZ,IFUN_RZ,6,
     .                     FAC_MM,IFUN_FMRZ,RED,ID,TITR,
     .                     LSUBMODEL)
C
         IF ((RED/=0).AND.(RED/=2)) THEN
            CALL ANCMSG(MSGID=973,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=RED,
     .                  I3=3)
	 ENDIF	 
C     
         KXX = KNN
	 KYY = KNN
	 KZZ = KNN
	 KRX = KNN  
C
         CXX = ZERO
	 CYY = ZERO
	 CZZ = ZERO
	 CRX = ZERO	 
C
         CRXX = CR
	 CRYY = CR
	 CRZZ = CR
	 CRRX = CR
	 CRRY = ZERO
	 CRRZ = ZERO
C
         SDXMI = ZERO
         SDXMA = ZERO
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO
C
         SAXMI = ZERO
         SAXMA = ZERO		 
C
         KFX = ZERO         
         KFY = ZERO
         KFZ = ZERO
         KFRX = ZERO		 
C
         FMX = ZERO
         FMY = ZERO
         FMZ = ZERO
         FMRX = ZERO  	 
C	 
         IFUN_XX = ZEROI
         IFUN_YY = ZEROI 
         IFUN_ZZ = ZEROI
         IFUN_RX = ZEROI
         IFUN_CXX = ZEROI
         IFUN_CYY = ZEROI 
         IFUN_CZZ = ZEROI
         IFUN_CRX = ZEROI
         IFUN_FMX = ZEROI 
         IFUN_FMY = ZEROI
         IFUN_FMZ = ZEROI 
         IFUN_FMRX = ZEROI	 
C	 	  
      ELSEIF (JTYP==6) THEN
C=======================================================================
C----   TRANSLATIONAL JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,600)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KXX,CXX,SDXMI,
     .                     SDXMA,FCOMBX,KFX,FMX,IFUN_CXX,IFUN_XX,1,
     .                     FAC_FF,IFUN_FMX,RED,ID,TITR,
     .                     LSUBMODEL)            
C     
	 KYY = KNN
	 KZZ = KNN
	 KRX = KNN
	 KRY = KNN
	 KRZ = KNN
C
	 CYY = ZERO
	 CZZ = ZERO
	 CRX = ZERO
	 CRY = ZERO
	 CRZ = ZERO
C
         CRXX = ZERO
	 CRYY = CR
	 CRZZ = CR
	 CRRX = CR
	 CRRY = CR
	 CRRZ = CR	 
C
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO	 
C
         SAXMI = ZERO
         SAXMA = ZERO
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C
         KFY = ZERO
         KFZ = ZERO
         KFRX = ZERO
         KFRY = ZERO
         KFRZ = ZERO
C
         FMY = ZERO
         FMZ = ZERO
         FMRX = ZERO
         FMRY = ZERO
         FMRZ = ZERO	 	 	 	 	 	 	 	 
C
         IFUN_YY = ZEROI 
         IFUN_ZZ = ZEROI
         IFUN_CYY = ZEROI 
         IFUN_CZZ = ZEROI
         IFUN_RX = ZEROI	 
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI
         IFUN_CRX = ZEROI	  
         IFUN_CRY = ZEROI 
         IFUN_CRZ = ZEROI
         IFUN_FMY = ZEROI 
         IFUN_FMZ = ZEROI
         IFUN_FMRX = ZEROI	 
         IFUN_FMRY = ZEROI 
         IFUN_FMRZ = ZEROI	 
C		 
      ELSEIF (JTYP==7) THEN
C=======================================================================
C----   OLDHAM JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,700)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 RED = 0
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KYY,CYY,SDYMI,
     .                     SDYMA,FCOMBY,KFY,FMY,IFUN_CYY,IFUN_YY,2,
     .                     FAC_FF,IFUN_FMY,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KZZ,CZZ,SDZMI,
     .                     SDZMA,FCOMBZ,KFZ,FMZ,IFUN_CZZ,IFUN_ZZ,3,
     .                     FAC_FF,IFUN_FMZ,RED,ID,TITR,
     .                     LSUBMODEL) 
C
         IF ((RED/=0).AND.(RED/=2)) THEN
           CALL ANCMSG(MSGID=973,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=RED,
     .                 I3=2)
	 ENDIF   	 
C     
	 KXX = KNN
	 KRX = KNN
	 KRY = KNN
	 KRZ = KNN
C
	 CXX = ZERO
	 CRX = ZERO
	 CRY = ZERO
	 CRZ = ZERO
C
         CRXX = CR
	 CRYY = ZERO
	 CRZZ = ZERO
	 CRRX = CR
	 CRRY = CR
	 CRRZ = CR
C
         SDXMI = ZERO
         SDXMA = ZERO
C
         SAXMI = ZERO
         SAXMA = ZERO
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C
         KFX = ZERO
	 KFRX = ZERO
         KFRY = ZERO
         KFRZ = ZERO
C
         FMX = ZERO
         FMRX = ZERO
         FMRY = ZERO
         FMRZ = ZERO		 
C
         IFUN_XX = ZEROI 
         IFUN_CXX = ZEROI
         IFUN_RX = ZEROI
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI
         IFUN_CRX = ZEROI
         IFUN_CRY = ZEROI
         IFUN_CRZ = ZEROI
         IFUN_FMX = ZEROI
         IFUN_FMRX = ZEROI
         IFUN_FMRY = ZEROI
         IFUN_FMRZ = ZEROI	 	 	 	 
C		 
      ELSEIF (JTYP==8) THEN
C=======================================================================
C----   RIGID JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,800)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 KXX = KNN
	 KYY = KNN
	 KZZ = KNN
	 KRX = KNN
	 KRY = KNN
	 KRZ = KNN
C
	 CXX = ZERO
	 CYY = ZERO
	 CZZ = ZERO
	 CRX = ZERO
	 CRY = ZERO
	 CRZ = ZERO
C
         CRXX = CR
	 CRYY = CR
	 CRZZ = CR
	 CRRX = CR
	 CRRY = CR
	 CRRZ = CR
C
         SDXMI = ZERO
         SDXMA = ZERO
         SDYMI = ZERO
         SDYMA = ZERO
         SDZMI = ZERO
         SDZMA = ZERO
C
         SAXMI = ZERO
         SAXMA = ZERO
         SAYMI = ZERO
         SAYMA = ZERO
         SAZMI = ZERO
         SAZMA = ZERO
C
         KFX = ZERO
         KFY = ZERO
         KFZ = ZERO
         KFRX = ZERO
         KFRY = ZERO
         KFRZ = ZERO
C
         FMX = ZERO
         FMY = ZERO
         FMZ = ZERO
         FMRX = ZERO
         FMRY = ZERO
         FMRZ = ZERO
C
         IFUN_XX = ZEROI
         IFUN_YY = ZEROI
         IFUN_ZZ = ZEROI
         IFUN_CXX = ZEROI
         IFUN_CYY = ZEROI
         IFUN_CZZ = ZEROI
         IFUN_RX = ZEROI
         IFUN_RY = ZEROI
         IFUN_RZ = ZEROI
         IFUN_CRX = ZEROI
         IFUN_CRY = ZEROI
         IFUN_CRZ = ZEROI
         IFUN_CRZ = ZEROI
         IFUN_FMX = ZEROI
         IFUN_FMY = ZEROI
         IFUN_FMZ = ZEROI	 	 
         IFUN_FMRX = ZEROI
         IFUN_FMRY = ZEROI
         IFUN_FMRZ = ZEROI	 
C		 
      ELSEIF (JTYP==9) THEN
C=======================================================================
C----   FREE JOINT
C=======================================================================
         IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,900)
	   IF ((IDSK1==0).AND.(IDSK2==0)) THEN
	     IF (KNN==0) THEN
               WRITE(IOUT,1100) SCF,CR,SENSOR
	     ELSE
               WRITE(IOUT,1000) KNN,SCF,CR,SENSOR	 
	     ENDIF
           ELSE
	     IF (KNN==0) THEN
               WRITE(IOUT,1300) SCF,CR,SENSOR,IDSK1,IDSK2
	     ELSE
               WRITE(IOUT,1200) KNN,SCF,CR,SENSOR,IDSK1,IDSK2	 
	     ENDIF
           ENDIF
         ENDIF
C
	 RED = 0
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KXX,CXX,SDXMI,
     .                     SDXMA,FCOMBX,KFX,FMX,IFUN_CXX,IFUN_XX,1,
     .                     FAC_FF,IFUN_FMX,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KYY,CYY,SDYMI,
     .                     SDYMA,FCOMBY,KFY,FMY,IFUN_CYY,IFUN_YY,2,
     .                     FAC_FF,IFUN_FMY,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KZZ,CZZ,SDZMI,
     .                     SDZMA,FCOMBZ,KFZ,FMZ,IFUN_CZZ,IFUN_ZZ,3,
     .                     FAC_FF,IFUN_FMZ,RED,ID,TITR,
     .                     LSUBMODEL)		 
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRX,CRX,SAXMI,
     .                     SAXMA,FCOMBRX,KFRX,FMRX,IFUN_CRX,IFUN_RX,4,
     .                     FAC_MM,IFUN_FMRX,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRY,CRY,SAYMI,
     .                     SAYMA,FCOMBRY,KFRY,FMRY,IFUN_CRY,IFUN_RY,5,
     .                     FAC_MM,IFUN_FMRY,RED,ID,TITR,
     .                     LSUBMODEL)
	 CALL LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRZ,CRZ,SAZMI,
     .                     SAZMA,FCOMBRZ,KFRZ,FMRZ,IFUN_CRZ,IFUN_RZ,6,
     .                     FAC_MM,IFUN_FMRZ,RED,ID,TITR,
     .                     LSUBMODEL)	 
C
         IF ((RED/=0).AND.(RED/=6)) THEN
           CALL ANCMSG(MSGID=973,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=RED,
     .                 I3=6)
	 ENDIF
C	 	 	 	  	 	          	 	 	 	         	  	 
      ELSE
C        WRITE(IOUT,*)' **ERROR WRONG JOINT TYPE'
         CALL ANCMSG(MSGID=938,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=JTYP)
      END IF

C=======================================================================
C----   Check of combined stopping displacements / angles
C=================================================================  
C
        COMB_ERROR = 0
        SUMT = FCOMBX+FCOMBY+FCOMBZ
        SUMR = FCOMBRX+FCOMBRY+FCOMBRZ
C
C--- Check for combined displacements - values must be the same for combined dof
C
        IF (SUMT == ONE) THEN
          CALL ANCMSG(MSGID=1599,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR)
        ELSEIF (SUMT > ONE) THEN
          ALPHA_PLUS = (FCOMBX*SDXMA+FCOMBY*SDYMA+FCOMBZ*SDZMA)/SUMT
          ALPHA_MOIN = (FCOMBX*SDXMI+FCOMBY*SDYMI+FCOMBZ*SDZMI)/SUMT
          IF ((FCOMBX>EM20).AND.(ABS(ALPHA_PLUS - SDXMA)/MAX(EM20,ABS(ALPHA_PLUS))>EM10)) COMB_ERROR = 1
          IF ((FCOMBY>EM20).AND.(ABS(ALPHA_PLUS - SDYMA)/MAX(EM20,ABS(ALPHA_PLUS))>EM10)) COMB_ERROR = 1
          IF ((FCOMBZ>EM20).AND.(ABS(ALPHA_PLUS - SDZMA)/MAX(EM20,ABS(ALPHA_PLUS))>EM10)) COMB_ERROR = 1
          IF ((FCOMBX>EM20).AND.(ABS(ALPHA_MOIN - SDXMI)/MAX(EM20,ABS(ALPHA_MOIN))>EM10)) COMB_ERROR = 1
          IF ((FCOMBY>EM20).AND.(ABS(ALPHA_MOIN - SDYMI)/MAX(EM20,ABS(ALPHA_MOIN))>EM10)) COMB_ERROR = 1
          IF ((FCOMBZ>EM20).AND.(ABS(ALPHA_MOIN - SDZMI)/MAX(EM20,ABS(ALPHA_MOIN))>EM10)) COMB_ERROR = 1
        ENDIF
C
C--- Check for combined angles - values must be the same for combined dof
C
        IF (SUMR == ONE) THEN
              CALL ANCMSG(MSGID=1600,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR)
        ELSEIF (SUMR > ONE) THEN
          ALPHA_PLUS = (FCOMBRX*SAXMA+FCOMBRY*SAYMA+FCOMBRZ*SAZMA)/SUMR
          ALPHA_MOIN = (FCOMBRX*SAXMI+FCOMBRY*SAYMI+FCOMBRZ*SAZMI)/SUMR
          IF ((FCOMBRX>EM20).AND.(ABS(ALPHA_PLUS - SAXMA)/MAX(EM20,ABS(ALPHA_PLUS))>EM10)) COMB_ERROR = 1
          IF ((FCOMBRY>EM20).AND.(ABS(ALPHA_PLUS - SAYMA)/MAX(EM20,ABS(ALPHA_PLUS))>EM10)) COMB_ERROR = 1
          IF ((FCOMBRZ>EM20).AND.(ABS(ALPHA_PLUS - SAZMA)/MAX(EM20,ABS(ALPHA_PLUS))>EM10)) COMB_ERROR = 1
          IF ((FCOMBRX>EM20).AND.(ABS(ALPHA_MOIN - SAXMI)/MAX(EM20,ABS(ALPHA_MOIN))>EM10)) COMB_ERROR = 1
          IF ((FCOMBRY>EM20).AND.(ABS(ALPHA_MOIN - SAYMI)/MAX(EM20,ABS(ALPHA_MOIN))>EM10)) COMB_ERROR = 1
          IF ((FCOMBRZ>EM20).AND.(ABS(ALPHA_MOIN - SAZMI)/MAX(EM20,ABS(ALPHA_MOIN))>EM10)) COMB_ERROR = 1
        ENDIF
C
        IF (COMB_ERROR==1) THEN
              CALL ANCMSG(MSGID=1598,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR)
        ENDIF
      
C=======================================================================
C----   Stockage des donnees
C=================================================================  
        PARGEO(1) = 0
        PARGEO(2) = 0
        PARGEO(3) = 0     
C-----------------------

        IERROR = SET_U_GEO(1,XTYP)
        IERROR = SET_U_GEO(2,SENSR)
        IERROR = SET_U_GEO(3,SKEWR)
        IERROR = SET_U_GEO(4,KXX)
        IERROR = SET_U_GEO(5,KYY)
        IERROR = SET_U_GEO(6,KZZ)
        IERROR = SET_U_GEO(7,KRX)
        IERROR = SET_U_GEO(8,KRY)
        IERROR = SET_U_GEO(9,KRZ)
        IERROR = SET_U_GEO(10,KNN)
        IERROR = SET_U_GEO(11,SCF)
C	
        IERROR = SET_U_GEO(12,CR)
        IERROR = SET_U_GEO(13,ZERO)
        IERROR = SET_U_GEO(14,ZERO)
        IERROR = SET_U_GEO(15,CRXX)
        IERROR = SET_U_GEO(16,CRYY)
        IERROR = SET_U_GEO(17,CRZZ)
        IERROR = SET_U_GEO(18,CRRX)
        IERROR = SET_U_GEO(19,CRRY)
        IERROR = SET_U_GEO(20,CRRZ)
        IERROR = SET_U_GEO(21,CXX)
        IERROR = SET_U_GEO(22,CYY)
        IERROR = SET_U_GEO(23,CZZ)
        IERROR = SET_U_GEO(24,CRX)
        IERROR = SET_U_GEO(25,CRY)
        IERROR = SET_U_GEO(26,CRZ)
C        IERROR = SET_U_GEO(27,FAC_CTX)            
C        IERROR = SET_U_GEO(28,FAC_CRX)
        IERROR = SET_U_GEO(27,FAC_L)            
        IERROR = SET_U_GEO(28,FAC_T)
C------------Blocking Angles/displacements
        IERROR = SET_U_GEO(29,SDXMI)
        IERROR = SET_U_GEO(30,SDXMA)
        IERROR = SET_U_GEO(31,SDYMI)
        IERROR = SET_U_GEO(32,SDYMA)
        IERROR = SET_U_GEO(33,SDZMI)
        IERROR = SET_U_GEO(34,SDZMA)
        IERROR = SET_U_GEO(35,SAXMI)            
        IERROR = SET_U_GEO(36,SAXMA)
        IERROR = SET_U_GEO(37,SAYMI)
        IERROR = SET_U_GEO(38,SAYMA)
        IERROR = SET_U_GEO(39,SAZMI)
        IERROR = SET_U_GEO(40,SAZMA)
C------------Friction parameters
        IERROR = SET_U_GEO(41,KFX)
        IERROR = SET_U_GEO(42,KFY)
        IERROR = SET_U_GEO(43,KFZ)
        IERROR = SET_U_GEO(44,KFRX)
        IERROR = SET_U_GEO(45,KFRY)
        IERROR = SET_U_GEO(46,KFRZ)
        IERROR = SET_U_GEO(47,FMX)
        IERROR = SET_U_GEO(48,FMY)
        IERROR = SET_U_GEO(49,FMZ)
        IERROR = SET_U_GEO(50,FMRX)
        IERROR = SET_U_GEO(51,FMRY)
        IERROR = SET_U_GEO(52,FMRZ)
C------------Skews for initial angles
        IERROR = SET_U_GEO(53,XIDSK1)
        IERROR = SET_U_GEO(54,XIDSK2)
C------------Combination flag for stopping angles / displacements
        IERROR = SET_U_GEO(55,FCOMBX)
        IERROR = SET_U_GEO(56,FCOMBY)
        IERROR = SET_U_GEO(57,FCOMBZ)
        IERROR = SET_U_GEO(58,FCOMBRX)
        IERROR = SET_U_GEO(59,FCOMBRY)
        IERROR = SET_U_GEO(60,FCOMBRZ)
C------------Functions	            
        IERROR = SET_U_PNU(1,IFUN_XX,KFUNC)
        IERROR = SET_U_PNU(2,IFUN_YY,KFUNC)
        IERROR = SET_U_PNU(3,IFUN_ZZ,KFUNC)
        IERROR = SET_U_PNU(4,IFUN_RX,KFUNC)
        IERROR = SET_U_PNU(5,IFUN_RY,KFUNC)
        IERROR = SET_U_PNU(6,IFUN_RZ,KFUNC)
        IERROR = SET_U_PNU(7,IFUN_CXX,KFUNC)
        IERROR = SET_U_PNU(8,IFUN_CYY,KFUNC)
        IERROR = SET_U_PNU(9,IFUN_CZZ,KFUNC)
        IERROR = SET_U_PNU(10,IFUN_CRX,KFUNC)
        IERROR = SET_U_PNU(11,IFUN_CRY,KFUNC)
        IERROR = SET_U_PNU(12,IFUN_CRZ,KFUNC)	
        IERROR = SET_U_PNU(13,IFUN_FMX,KFUNC)
        IERROR = SET_U_PNU(14,IFUN_FMY,KFUNC)
        IERROR = SET_U_PNU(15,IFUN_FMZ,KFUNC)
        IERROR = SET_U_PNU(16,IFUN_FMRX,KFUNC)
        IERROR = SET_U_PNU(17,IFUN_FMRY,KFUNC)
        IERROR = SET_U_PNU(18,IFUN_FMRZ,KFUNC)
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
C
        PROP_TAG(IGTYP)%G_FOR  = 3
        PROP_TAG(IGTYP)%G_MOM = 3
        PROP_TAG(IGTYP)%G_TOTDEPL = 3
        PROP_TAG(IGTYP)%G_TOTROT = 3
        PROP_TAG(IGTYP)%G_SKEW = 3
        PROP_TAG(IGTYP)%G_MASS = 1
        PROP_TAG(IGTYP)%G_NUVAR = NUVAR
        PROP_TAG(IGTYP)%G_LENGTH_ERR = 3
C		           
      RETURN
 100  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . SPHERICAL JOINT'/)  	  
 200  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . REVOLUTE JOINT'/)
 300  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . CYLINDRICAL JOINT'/)
 400  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . PLANAR JOINT'/)
 500  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . UNIVERSAL JOINT'/)
 600  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . TRANSLATIONAL JOINT'/)
 700  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . OLDHAM JOINT'/)
 800  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . RIGID JOINT'/)
 900  FORMAT(
     & 5X,'JOINT TYPE . . . . . . . . FREE JOINT'/)    	 
     
 1000 FORMAT(
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . .  . . =',1PG20.13/,
     & 5X,'SCAL. FACTOR FOR ROT. BLOCKING STIFFNESS . .=',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . .  . . =',1PG20.13/,     
     & 5X,'SENSOR ID . . . . . . . . . . . . . . . . . =',I10/)
     
 1100 FORMAT(
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . . .  . =',' AUTO'/,
     & 5X,'SCALING FACTOR FOR AUTOMATIC STIFFNESS    . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . . .  =',1PG20.13/,     
     & 5X,'SENSOR ID  . . . . . . . . . . . . . .  . . =',I10/)

 1200 FORMAT(
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . .  . . =',1PG20.13/,
     & 5X,'SCALING FACTOR FOR AUTOMATIC STIFFNESS    . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . . .  =',1PG20.13/,     
     & 5X,'SENSOR ID  . . . . . . . . . . . . . .  . . =',I10/,
     & 5X,'SKEW ID 1  . . . . . . . . . . . . . .  . . =',I10/,
     & 5X,'SKEW ID 2  . . . . . . . . . . . . . .  . . =',I10/)

 1300 FORMAT(
     & 5X,'BLOCKING STIFFNESS KNN . . . . . . . . .  . =',' AUTO'/,
     & 5X,'SCALING FACTOR FOR AUTOMATIC STIFFNESS    . =',1PG20.13/,
     & 5X,'CRITICAL DAMPING COEFFICIENT . . . . . . .  =',1PG20.13/,     
     & 5X,'SENSOR ID  . . . . . . . . . . . . . .  . . =',I10/,
     & 5X,'SKEW ID 1  . . . . . . . . . . . . . .  . . =',I10/,
     & 5X,'SKEW ID 2  . . . . . . . . . . . . . .  . . =',I10/)

 1400 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)

 1500 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,
     & 5X,'CONFIDENTIAL DATA'//)
                
 999  CALL FREERR(1)
      RETURN
      END
      
Chd|====================================================================
Chd|  LEC_DOF_JNT                   source/properties/spring/hm_read_prop45.F
Chd|-- called by -----------
Chd|        HM_READ_PROP45                source/properties/spring/hm_read_prop45.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LEC_DOF_JNT(IOUT,IS_ENCRYPTED,UNITAB,KRX,CRX,SAMI,
     .                        SAMA,FCOMB,KFR,FM,IFUN_CRX,IFUN_RX,IDOF,
     .                        FAC3,IFUN_FM,RED,ID,TITR,LSUBMODEL)
C-----------------------------------------------
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
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
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
      INTEGER IOUT,IFUN_CRX,IFUN_RX,IDOF,RED,IFUN_FM
      my_real KRX,CRX,SAMI,SAMA,KFR,FM,FAC3,FCOMB

      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL IS_ENCRYPTED
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SET_U_PNU,SET_U_GEO,ICOMB,READ_BLOCK,I
      EXTERNAL SET_U_PNU,SET_U_GEO
      LOGICAL IS_AVAILABLE(10)
C=======================================================================
C----  LECTURE DU BLOCK DE DONNEES POUR FREE DOF
C=======================================================================
C
      IF (IDOF == 1) THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Xt_fun',IFUN_RX,IS_AVAILABLE(1),LSUBMODEL)
        CALL HM_GET_INTV('Ctx_Fun',IFUN_CRX,IS_AVAILABLE(2),LSUBMODEL)
        CALL HM_GET_INTV('FUN_A1',IFUN_FM,IS_AVAILABLE(3),LSUBMODEL)
        CALL HM_GET_INTV('Icomb_tx',ICOMB,IS_AVAILABLE(4),LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
        CALL HM_GET_FLOATV('Ktx',KRX,IS_AVAILABLE(5),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('type12_XN',SAMI,IS_AVAILABLE(6),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('type12_Xc',SAMA,IS_AVAILABLE(7),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Ctx',CRX,IS_AVAILABLE(8),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Vx',KFR,IS_AVAILABLE(9),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Prop_X_F',FM,IS_AVAILABLE(10),LSUBMODEL,UNITAB)
C-------------------------
      ELSEIF (IDOF == 2) THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Yt_fun',IFUN_RX,IS_AVAILABLE(1),LSUBMODEL)
        CALL HM_GET_INTV('Cty_Fun',IFUN_CRX,IS_AVAILABLE(2),LSUBMODEL)
        CALL HM_GET_INTV('FUN_A2',IFUN_FM,IS_AVAILABLE(3),LSUBMODEL)
        CALL HM_GET_INTV('Icomb_ty',ICOMB,IS_AVAILABLE(4),LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
        CALL HM_GET_FLOATV('Kty',KRX,IS_AVAILABLE(5),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('type12_YN',SAMI,IS_AVAILABLE(6),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('type12_Yc',SAMA,IS_AVAILABLE(7),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Cty',CRX,IS_AVAILABLE(8),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Vy',KFR,IS_AVAILABLE(9),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Prop_Y_F',FM,IS_AVAILABLE(10),LSUBMODEL,UNITAB)
C-------------------------
      ELSEIF (IDOF == 3) THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Zt_fun',IFUN_RX,IS_AVAILABLE(1),LSUBMODEL)
        CALL HM_GET_INTV('Ctz_Fun',IFUN_CRX,IS_AVAILABLE(2),LSUBMODEL)
        CALL HM_GET_INTV('FUN_A3',IFUN_FM,IS_AVAILABLE(3),LSUBMODEL)
        CALL HM_GET_INTV('Icomb_tz',ICOMB,IS_AVAILABLE(4),LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
        CALL HM_GET_FLOATV('Ktz',KRX,IS_AVAILABLE(5),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('type12_ZN',SAMI,IS_AVAILABLE(6),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('type12_Zc',SAMA,IS_AVAILABLE(7),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Ctz',CRX,IS_AVAILABLE(8),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Vz',KFR,IS_AVAILABLE(9),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Prop_Z_F',FM,IS_AVAILABLE(10),LSUBMODEL,UNITAB)
C-------------------------
      ELSEIF (IDOF == 4) THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Xr_fun',IFUN_RX,IS_AVAILABLE(1),LSUBMODEL)
        CALL HM_GET_INTV('Crx_Fun',IFUN_CRX,IS_AVAILABLE(2),LSUBMODEL)
        CALL HM_GET_INTV('FUN_B1',IFUN_FM,IS_AVAILABLE(3),LSUBMODEL)
        CALL HM_GET_INTV('Icomb_rx',ICOMB,IS_AVAILABLE(4),LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
        CALL HM_GET_FLOATV('Krx',KRX,IS_AVAILABLE(5),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('X_A',SAMI,IS_AVAILABLE(6),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('X_B',SAMA,IS_AVAILABLE(7),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Crx',CRX,IS_AVAILABLE(8),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('VX1',KFR,IS_AVAILABLE(9),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('N_x',FM,IS_AVAILABLE(10),LSUBMODEL,UNITAB)
C-------------------------
      ELSEIF (IDOF == 5) THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Yr_fun',IFUN_RX,IS_AVAILABLE(1),LSUBMODEL)
        CALL HM_GET_INTV('Cry_Fun',IFUN_CRX,IS_AVAILABLE(2),LSUBMODEL)
        CALL HM_GET_INTV('FUN_B2',IFUN_FM,IS_AVAILABLE(3),LSUBMODEL)
        CALL HM_GET_INTV('Icomb_ry',ICOMB,IS_AVAILABLE(4),LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
        CALL HM_GET_FLOATV('Kry',KRX,IS_AVAILABLE(5),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Y_A',SAMI,IS_AVAILABLE(6),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Y_B',SAMA,IS_AVAILABLE(7),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Cry',CRX,IS_AVAILABLE(8),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('VY1',KFR,IS_AVAILABLE(9),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('N_y',FM,IS_AVAILABLE(10),LSUBMODEL,UNITAB)
C-------------------------
      ELSEIF (IDOF == 6) THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Zr_fun',IFUN_RX,IS_AVAILABLE(1),LSUBMODEL)
        CALL HM_GET_INTV('Crz_Fun',IFUN_CRX,IS_AVAILABLE(2),LSUBMODEL)
        CALL HM_GET_INTV('FUN_B3',IFUN_FM,IS_AVAILABLE(3),LSUBMODEL)
        CALL HM_GET_INTV('Icomb_rz',ICOMB,IS_AVAILABLE(4),LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
        CALL HM_GET_FLOATV('Krz',KRX,IS_AVAILABLE(5),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Z_A',SAMI,IS_AVAILABLE(6),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Z_B',SAMA,IS_AVAILABLE(7),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Crz',CRX,IS_AVAILABLE(8),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('VZ1',KFR,IS_AVAILABLE(9),LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('N_z',FM,IS_AVAILABLE(10),LSUBMODEL,UNITAB)
C-------------------------
      ENDIF
C
      READ_BLOCK = 0
      DO I=1,10
        IF (IS_AVAILABLE(I)) READ_BLOCK = READ_BLOCK + 1
      ENDDO
C
      IF (READ_BLOCK > 0) THEN      
        FCOMB = ICOMB
	RED = RED +1
      ELSE
	GOTO 350		
      ENDIF          	              
C
      IF(CRX==ZERO.AND.IFUN_CRX/=0) CRX = ONE * FAC3
      IF(KRX==ZERO.AND.IFUN_RX/=0) KRX = ONE * FAC3
      IF(FM==ZERO.AND.IFUN_FM/=0) FM = ONE * FAC3
C
C-----------------------
      IF(.NOT. IS_ENCRYPTED)THEN
        IF (IDOF==1) THEN
          WRITE(IOUT,100)       
        ELSEIF (IDOF==2) THEN
          WRITE(IOUT,200)
        ELSEIF (IDOF==2) THEN
          WRITE(IOUT,300)
        ELSEIF (IDOF==4) THEN
          WRITE(IOUT,400)
        ELSEIF (IDOF==5) THEN
          WRITE(IOUT,500) 	 	 	  	        
        ELSE
          WRITE(IOUT,600)       
        ENDIF
      ENDIF
C      
      IF (SAMI>ZERO) THEN
          CALL ANCMSG(MSGID=943,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR)
      ELSEIF (SAMA<ZERO) THEN
          CALL ANCMSG(MSGID=944,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR)
      ENDIF 
C      
      IF(.NOT. IS_ENCRYPTED)THEN
        IF (IDOF<4) THEN
          IF (IFUN_FM==0) THEN      
            WRITE(IOUT,1000) KRX,IFUN_RX,CRX,IFUN_CRX,SAMI,SAMA,ICOMB,KFR,FM
	  ELSE
            WRITE(IOUT,3000) KRX,IFUN_RX,CRX,IFUN_CRX,SAMI,SAMA,ICOMB,KFR,IFUN_FM	
	  ENDIF
        ELSE
          IF (IFUN_FM==0) THEN      	
            WRITE(IOUT,2000) KRX,IFUN_RX,CRX,IFUN_CRX,SAMI,SAMA,ICOMB,KFR,FM
	  ELSE
            WRITE(IOUT,4000) KRX,IFUN_RX,CRX,IFUN_CRX,SAMI,SAMA,ICOMB,KFR,IFUN_FM	
	  ENDIF  	
        ENDIF
      ENDIF
C
350   CONTINUE      
      	      
C-----------------------
      RETURN
 100  FORMAT(
     & 5X,'PARAMETERS FOR FREE TRANSLATION ALONG X AXIS'/)
 200  FORMAT(
     & 5X,'PARAMETERS FOR FREE TRANSLATION ALONG Y AXIS'/)
 300  FORMAT(
     & 5X,'PARAMETERS FOR FREE TRANSLATION ALONG Z AXIS'/)      
 400  FORMAT(
     & 5X,'PARAMETERS FOR FREE ROTATION AROUND X AXIS'/)
 500  FORMAT(
     & 5X,'PARAMETERS FOR FREE ROTATION AROUND Y AXIS'/)
 600  FORMAT(
     & 5X,'PARAMETERS FOR FREE ROTATION AROUND Z AXIS'/)
 1000 FORMAT(
     & 5X,'TRANSLATIONAL LINEAR STIFFNESS . . . . =',1PG20.13/,
     & 5X,'TRANSLATIONAL FUNCTION ID . . . . . . .=',I10/,
     & 5X,'LINEAR DAMPING . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'USER DAMPING FUNCTION. . . . . . . . . =',I10/,
     & 5X,'NEGATIVE STOP DISPLACEMENT . . . . . . =',1PG20.13/,   
     & 5X,'POSITIVE STOP DISPLACEMENT . . . . . . =',1PG20.13/,
     & 5X,'COMBINED STOP DISPLACEMENT . . . . . . =',I10/,      
     & 5X,'STIFF. FOR FRICTION AND STOP DISPL. . .=',1PG20.13/,   
     & 5X,'FRICTIONAL FORCE. . . . . . . . . . . .=',1PG20.13//)                     
 2000 FORMAT(
     & 5X,'LINEAR ROTATIONAL STIFFNESS . . . . . .=',1PG20.13/,
     & 5X,'ROTATIONAL FUNCTION ID   . . . . . . . =',I10/,
     & 5X,'LINEAR DAMPING . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'USER DAMPING FUNCTION. . . . . . . . . =',I10/,
     & 5X,'NEGATIVE STOP ANGLE . . . . . .. . . . =',1PG20.13/,   
     & 5X,'POSITIVE STOP ANGLE . . . . . .. . . . =',1PG20.13/,
     & 5X,'COMBINED STOP ANGLE . . . . . .. . . . =',I10/,     
     & 5X,'STIFF. FOR FRICTION AND STOP ANGLES. . =',1PG20.13/,   
     & 5X,'FRICTIONAL MOMENT. . . . . . . . . . . =',1PG20.13//)
 3000 FORMAT(
     & 5X,'TRANSLATIONAL LINEAR STIFFNESS . . . . =',1PG20.13/,
     & 5X,'TRANSLATIONAL FUNCTION ID . . . . . . .=',I10/,
     & 5X,'LINEAR DAMPING . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'USER DAMPING FUNCTION. . . . . . . . . =',I10/,
     & 5X,'NEGATIVE STOP DISPLACEMENT . . . . . . =',1PG20.13/,   
     & 5X,'POSITIVE STOP DISPLACEMENT . . . . . . =',1PG20.13/,
     & 5X,'COMBINED STOP DISPLACEMENT . . . . . . =',I10/,      
     & 5X,'STIFF. FOR FRICTION AND STOP DISPL. . .=',1PG20.13/,   
     & 5X,'FRICTIONAL FORCE FUNCTION. . .. . . . .=',I10//)     
 4000 FORMAT(
     & 5X,'LINEAR ROTATIONAL STIFFNESS . . . . . .=',1PG20.13/,
     & 5X,'ROTATIONAL FUNCTION ID   . . . . . . . =',I10/,
     & 5X,'LINEAR DAMPING . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'USER DAMPING FUNCTION. . . . . . . . . =',I10/,
     & 5X,'NEGATIVE STOP ANGLE . . . . . .. . . . =',1PG20.13/,   
     & 5X,'POSITIVE STOP ANGLE . . . . . .. . . . =',1PG20.13/,
     & 5X,'COMBINED STOP ANGLE . . . . . .. . . . =',I10/,      
     & 5X,'STIFF. FOR FRICTION AND STOP ANGLES. . =',1PG20.13/,   
     & 5X,'FRICTIONAL MOMENT FUNCTION. . .. . . . =',I10//)
     
 999  CALL FREERR(1)           
      RETURN      
      END

