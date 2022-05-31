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
Chd|  HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT19(IPM,PM,UNITAB,ID,TITR,LSUBMODEL,MTAG,ISRATE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW19 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      INTEGER,INTENT(IN)::ID
      CHARACTER*nchartitle,INTENT(IN)::TITR
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IPM(*),ISRATE
      my_real,
     .  INTENT(INOUT)::PM(*)
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   N12, N21, E11, E22, G12, G23, G31, VX, VY, VZ, PHI, DE, CA,
     .   CB, CN, EPSM, SIGM, CC, EPS0, AN, DETC, AMU, C1, SSP,
     .   DUM,AREAMIN1,AREAMIN2,FAC_L,FAC_T,FAC_M,FAC_C,ZEROSTRESS,
     .   POROSITY,RHOR,RHO0
      INTEGER ISENS
      LOGICAL IS_AVAILABLE,IS_CRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      AREAMIN1 = ZERO

      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO',  RHO0  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      
      CALL HM_GET_FLOATV('MAT_EA',   E11   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EB',   E22   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_PRAB', N12   ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_GAB',  G12   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC',  G23   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA',  G31   ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_REDFACT',   DE         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('M58_Zerostress',ZEROSTRESS ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_POROS'     ,POROSITY   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV('ISENSOR'         ,ISENS      ,IS_AVAILABLE,LSUBMODEL)
 
      IF(RHOR == ZERO ) RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
      
      VX=ZERO
      VY=ZERO
      VZ=ZERO
      PHI=ZERO
      CA=ZERO
      CB=ZERO
      CN=ZERO
      EPSM=ZERO
      SIGM=ZERO
      CC=ZERO
      EPS0=ZERO
      ISRATE = 0
C-----------------------------------------------------
      IF(E11.EQ.ZERO.OR. E22.EQ.ZERO .OR. G12.EQ.ZERO .OR. G23.EQ.ZERO .OR. G31.EQ.ZERO) THEN
         CALL ANCMSG(MSGID=306,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=19,
     .               I2=ID,
     .               C1=TITR,
     .               C2='E11, E22, G12, G23, or G31')
      ENDIF
      IF(N2D.GT.0) THEN
         CALL ANCMSG(MSGID=305,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=19,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
C-------------------------------------
      AN=SQRT(VX*VX+VY*VY+VZ*VZ)
       IF(AN .LT. EM10)THEN 
       VX=ONE
      ELSE
       VX=VX/AN
       VY=VY/AN
       VZ=VZ/AN
      ENDIF
        PHI=PHI/(HUNDRED80)*PI
C-------------------------------------
      IF(CA.EQ.ZERO)   CA    = EP14
      IF(CN.EQ.ZERO)   CN    = ONE
      IF(EPSM.EQ.ZERO) EPSM  = INFINITY
      IF(SIGM.EQ.ZERO) SIGM  = INFINITY
      IF(CC.EQ.ZERO)   EPS0  = ONE
C--------------------------------------------------------
      N21=N12*E22/E11
      DETC=ONE -N12*N21
      IF(DETC.LE.ZERO) THEN
         CALL ANCMSG(MSGID=307,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
      ENDIF
C--------------------------------------------------------
       IF(CN.GT.ONE) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=19,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
      IF(EPS0.EQ.ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=19,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
C-----------------------------------------
      AMU=PM(17)
      IF(AMU.EQ.ZERO)AMU=FOURTH
C-----------------------------------------
      IF(DE.EQ.ZERO)DE=ONE
       IF(DE<EM03) THEN
         CALL ANCMSG(MSGID=1572,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
        DE = EM03
       ENDIF
C-------------------------------------
      IF(POROSITY.LT.ZERO.OR.POROSITY.GT.ONE) THEN
         CALL ANCMSG(MSGID=775,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
      ENDIF
C--------------------------------------
      WRITE(IOUT,1001) TRIM(TITR),ID,19
      WRITE(IOUT,1000)
      IF(IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1050)RHO0
        WRITE(IOUT,1100)E11,E22,N12
        WRITE(IOUT,1300)G12,G23,G31
        IF(INVERS.LT.35)WRITE(IOUT,1400)VX,VY,VZ,PHI
        WRITE(IOUT,1450)DE,AREAMIN1,ZEROSTRESS,ISENS,POROSITY
      ENDIF
C-----------------------------------------
      C1  =  MAX(E11,E22)/DETC
      SSP = SQRT(C1/PM(1))

      PM(17)=AMU
      PM(20)=MAX(E11,E22)/DETC
      PM(21)=SQRT(N12*N21)
      PM(22)=MAX(G12,G23,G31)      
      PM(24)= PM(20)
      PM(25)=PM(21)*PM(24)
      PM(26)=FIVE_OVER_6
      PM(27)= SSP
      PM(32)= C1
      PM(33)=E11
      PM(34)=E22
      PM(35)=N12
      PM(36)=N21
      PM(37)=G12
      PM(38)=G23
      PM(39)=G31
      PM(40)=VX
      PM(41)=VY
      PM(42)=VZ
      PM(43)=PHI
      PM(44)=DE
      PM(45)=CA
      PM(46)=CB
      PM(47)=CN
      PM(48)=EPSM
      PM(49)=SIGM
      PM(50)=CC
      PM(51)=EPS0
C
      IPM(5) = ISENS
C-----------------------------------------
      PM(52)= ONEP414*AMU*PM(1)*SSP
      PM(53)= AREAMIN1
      AREAMIN2 = ONE + HALF*(AREAMIN1-ONE)
      IF(AREAMIN2 > AREAMIN1)THEN
        PM(54)= ONE / (AREAMIN2-AREAMIN1)
      ELSE
        PM(54)= ZERO
      ENDIF
      PM(55) = ZEROSTRESS
      PM(56) = POROSITY 
C----- Definition des variables internes (stockage elementaire)
c
      MTAG%G_SIGI = 3    
C----------------------------------------
      RETURN
 1000 FORMAT(
     & 5X,40H  ORTHOTROPIC MATERIAL FOR SHELLS       ,/,
     & 5X,40H  -------------------------------       ,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1050 FORMAT(
     & 5X,40HINITIAL DENSITY . . . . . . . . . . . .=,1PG20.13/)  
 1100 FORMAT(
     & 5X,40HYOUNG'S MODULUS E11 (TENSION) . . . . .=,E12.4/,
     & 5X,40HYOUNG'S MODULUS E22 (TENSION) . . . . .=,E12.4/,
     & 5X,40HPOISSON'S RATIO N12 . . . . . . . . . .=,E12.4/)
 1300 FORMAT(
     & 5X,40HSHEAR MODULUS   G12 . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS   G23 . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS   G31 . . . . . . . . . .=,E12.4/)
 1400 FORMAT(
     & 5X,40HGLOBAL REFERENCE DIRECTION VX . . . . .=,E12.4/,
     & 5X,40H. . . . . . . . . . . . .  VY . . . . .=,E12.4/,
     & 5X,40H. . . . . . . . . . . . .  VZ . . . . .=,E12.4/,
     & 5X,40HORTHOTROPY ANGLE WITH PROJ. OF REF. . .=,E12.4/)
 1450 FORMAT(
     & 5X,40HCOMPRESSION REDUCTION FACTOR  . . . . . ,/
     & 5X,40H. . . . DE=E11C/E11= E22C/E22 . . . . .=,E12.4,/
     & 5X,40HZERO STRESS RELATIVE AREA . . . . . . .=,E12.4/
     & 5X,40HREF-STATE STRESS RELAXATION FACTOR. . .=,E12.4/
     & 5X,40HREF-STATE ACTIVATION SENSOR ID. . . . .=,I10/
     & 5X,40HPOROSITY. . . . . . . . . . . . . . . .=,E12.4/)
 1500 FORMAT(
     & 5X,40HYIELD STRESS  (A) . . . . . . . . . . .=,E12.4/,
     & 5X,40HHARDENING PARAMETER  (B)  . . . . . . .=,E12.4/,
     & 5X,40HHARDENING PARAMETER  (N)  . . . . . . .=,E12.4/,
     & 5X,40HEPS-MAX . . . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSIG-MAX . . . . . . . . . . . . . . . .=,E12.4/)
  999 CALL FREERR(3)
C-----------
      RETURN
      END

  
