Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_MAT16                 source/materials/mat/mat016/hm_read_mat16.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT16(
     .              MTAG     ,PM    ,MAT_ID   ,TITR  ,IPM   ,
     .              LSUBMODEL,UNITAB,MATPARAM )
C-----------------------------------------------
C FAC_M FACL FAC_T : enable to convert (custom) input unit to working unit system
C FAC_MASS, FAC_LENGTH, FAC_TIME : enable to convert working unit system into International Unit system
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      INTEGER, INTENT(IN) :: MAT_ID
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT) :: IPM
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT) :: PM
      my_real, DIMENSION(LUNIT,NUNITS) ,INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILAW,J
      my_real
     .   YOUNG, ANU, CA, CB, CN, EPSM, SIGM, C0, C, S, GAM0, A, AW,
     .   PMIN, E0, CC, EPS0, CM, TMELT, TMAX, GAM0M, AM, GAME, GE, DS,
     .   TM0, VJ, VB, UNIT, E0H, AY, THET, G, RHO, ALPHA, R, XM, RP3,
     .   GP, DSP, ALPHAP, RP, PCC, E00, APY, XJ, X, ZJ, E0J, XP, TMJ,
     .   XLAMJ, D1, D2, D3, E, G0AX, P1J, C1, C2, C3,EPS0_UNIT,ENER_UNIT,DS_UNIT,
     .   FAC_M_bb,FAC_L_bb,FAC_T_bb,RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
!
      ILAW = 16
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
!-----------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!-----------------------
!     LINE 1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 2
      CALL HM_GET_FLOATV('MAT_E'        ,YOUNG      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,ANU        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 3
      CALL HM_GET_FLOATV('MAT_SIGY'     ,CA         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'     ,CB         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'     ,CN         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'      ,EPSM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'      ,SIGM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 4
      CALL HM_GET_FLOATV('MAT_P0'       ,C0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'        ,C          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_S'        ,S          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMA0'    ,GAM0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A'        ,A          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 5
      CALL HM_GET_FLOATV('MAT_AW'       ,AW         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'       ,PMIN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E0'       ,E0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 6
      CALL HM_GET_FLOATV('MAT_SRC'      ,CC         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'      ,EPS0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'        ,CM         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMELT'    ,TMELT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX'     ,TMAX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 7
      CALL HM_GET_FLOATV('MAT_GAMAm'    ,GAM0M      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft1'      ,AM         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('GAMMA'        ,GAME       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G0'       ,GE         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_dS'       ,DS         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 8
      CALL HM_GET_FLOATV('T_melt_0'     ,TM0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_VOL'      ,VJ         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Vb'       ,VB         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 9
!!  - no more within doc -      CALL HM_GET_FLOATV('MAT_U'        ,UNIT       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EOH'      ,E0H        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Ay'       ,AY         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Theta'    ,THET       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     DEFAULT UNITS
      CALL HM_GET_FLOATV_DIM('MAT_SRP'  ,EPS0_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_EOH'  ,ENER_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_dS'   ,DS_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!-----------------------
      IF (PMIN == ZERO) PMIN = -INFINITY
      IF (CN == ZERO .OR. CN == ONE) CN = ONEP0001
      IF (EPSM == ZERO) EPSM  = INFINITY
      IF (SIGM == ZERO) SIGM  = INFINITY
      IF (CC == ZERO) EPS0 = ONE*EPS0_UNIT
      IF (CM == ZERO) CM   = ONE  ! dimensionless
      IF (TMELT == ZERO) THEN
         TMELT = INFINITY
      ELSEIF (TM0 == ZERO) THEN
         TM0 = ONEP3*TMELT
      ENDIF
      IF (TMAX == ZERO) TMAX = INFINITY
C
      G=YOUNG/(TWO*(ONE+ANU))
C
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
      PM(20)= YOUNG
      PM(21)= ANU
      PM(22)= G
      PM(23)= E0
      PM(31)= C0
      PM(32)= RHOR*C**2
      PM(37)= PMIN
      PM(38)= CA
      PM(39)= CB
      PM(40)= CN
      PM(41)= EPSM
      PM(42)= SIGM
      PM(43)= CC
      PM(44)= EPS0
      PM(45)= CM
      PM(46)= TMELT
      PM(47)= TMAX
      PM(80)= TMELT
C
C--------------------------------------
C     GRAY
C--------------------------------------
!
      !translation from Working unit System to {big bang} unit system
      FAC_M_bb = FAC_MASS*EP03
      FAC_L_bb = FAC_LENGTH*EP02 
      FAC_T_bb = FAC_TIME*EP06
!
      IF (A == ZERO)    A = GAM0-HALF
      IF (GAM0M == ZERO) GAM0M = GAM0
      IF (AM == ZERO)   AM = A
      IF (GAME == ZERO) GAME = TWO_THIRD
      IF (THET == ZERO) THET = ONE
!
!!!      UNIT = ONE * EP05 * ENER_UNIT  !    (1.0 * Mbar * cm**3 en SI)
      UNIT = ONE * (FAC_T_bb * FAC_T_bb)/(FAC_M_bb*FAC_L_bb**2)
      IF (DS == ZERO)   DS = NINEP637EM5 * (FAC_T_bb * FAC_T_bb)/(FAC_L_bb * FAC_L_bb)
      IF (VB == ZERO)   VB=HALF/RHOR
      ALPHA=UNIT*ONEP3EM5
      R=UNIT*EIGHTP314EM5
      XM=NINEP38
C
      RP3=THREE*R/AW
      GP=GE/AW
      DSP=DS/AW
      ALPHAP=ALPHA/AW
      RP=R/AW
      PCC=THREE100**2*GP*(GAM0-GAME)*RHOR*HALF
      E00=-THREE100*(THREE*R + HUNDRED50*GE)/AW
      APY=AY/AW/AW
      XJ=ONE-RHOR*VJ
      X=XJ
      ZJ=VB/VJ
      E0J=(C**2*X**2/(2*(ONE-S*X)))*
     .    (ONE
     .     +S*X*THIRD
     .     +S**2*(ONE-GAM0/S)*X**2/SIX)
     .     +E00*(ONE+GAM0*X)+E0H
C
      XP=ZERO
      IF (X >= ZERO) XP=ONE
      TMJ=TM0*((ONE-XP)*(
     .     ONE
     .    +TWO*(GAM0M-FOUR_OVER_3)*X
     .    +((TWO*GAM0M-FIVE_OVER_3)*(GAM0M-FOUR_OVER_3)-AM)*X**2
     .                         )/(ONE-X)**2
     .             +XP*(
     .     ONE
     .    +(TWO*GAM0M-TWO_THIRD)*X
     .    +((GAM0M-THIRD)*(TWO*GAM0M+THIRD)-AM)*X**2))
      XLAMJ=TWO_THIRD-TWO*GAM0M+TWO*AM*X

C
      D1=E0J+TMJ*(DSP-HALF*ALPHAP*(ONE-XM**2))+APY*RHOR/(ONE-X)
      D2=THREE_HALF*RP-XM*ALPHAP
      D3=HALF*GP
      E=D1-APY/VJ
      G0AX=GAM0-A*X
      P1J  =PCC+C**2*X
     .     *(ONE-(ONE+HALF*GAM0)*X+HALF*A*X**2)*RHOR
     .     /((ONE-X)*(ONE-S*X)**2)
     .     +G0AX*(E-E0H)*RHOR/(ONE-X)
      C1=P1J-TMJ*(XLAMJ+G0AX)*(DSP-HALF*ALPHAP*(ONE-XM**2))*RHOR/(ONE-X)
     .      +APY*(RHOR/(ONE-X))**2
      C2=G0AX*(D2+THREE_HALF*RP)*RHOR/(ONE-X)
     .   +ALPHAP*XM*(XLAMJ+G0AX)*RHOR/(ONE-X)
     .   -RP*RHOR*((ONE+ZJ+ZJ**2-ZJ**3)/(ONE-ZJ)**3)/(ONE-X)
      C3=GAME*GP*RHOR*HALF/(ONE-X)      
C
      PM(24)=D2
      PM(25)=D3
      PM(26)=THET
      PM(27)=APY
      PM(28)=VB
      PM(29)=TM0
      PM(30)=E00
      PM(33)=C
      PM(34)=S
      PM(35)=PCC
      PM(36)=GAM0
      PM(48)=A
      PM(49)=GAM0M
      PM(50)=AM
      PM(51)=GAME
      PM(52)=GP
      PM(53)=DSP
      PM(54)=E0H
      PM(55)=RP3
      PM(56)=VJ
      PM(57)=C1
      PM(58)=C2
      PM(59)=C3
      PM(60)=D1
      PM(61)=ALPHAP
C--------------------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) =  (ONE -TWO*ANU)/(ONE - ANU)      
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      IF (ANU > 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
c-----------------
      WRITE(IOUT,1001)TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002)RHO0,RHOR  
        WRITE(IOUT,1300)YOUNG,ANU,G
        WRITE(IOUT,1400)CA,CB,CN,EPSM,SIGM
        WRITE(IOUT,1500)C0,C,S,GAM0,A,AW,PMIN,E0
        WRITE(IOUT,1600)CC,EPS0,CM,TMELT,TMAX
        WRITE(IOUT,1700)GAM0M,AM,GAME,GE,DS,TM0,VJ,VB
        WRITE(IOUT,1800)UNIT,E0H,AY,THET
C        WRITE(IOUT,1900)C1,C2,C3,D1,D2,D3
C        WRITE(IOUT,2000)E0J,TMJ,P1J
      ENDIF
C--------------------
      IF (EPS0 == ZERO) THEN
         CALL ANCMSG(MSGID=298,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .               I1=16,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA   = 1    
      MTAG%G_TEMP  = 1    
      MTAG%G_EPSD  = 1    
c
      MTAG%L_PLA   = 1 
      MTAG%L_TEMP  = 1
      MTAG%L_EPSD  = 1
      MTAG%L_XST   = 1
C--------------------
      RETURN
C--------------------
 1000 FORMAT(
     & 5X,40H  JOHNSON COOK - GRAY LAW               ,/,
     & 5X,40H  -----------------------               ,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,40HYOUNG'S MODULUS . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HPOISSON'S RATIO . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HSHEAR MODULUS . . . . . . . . . . . . .=,1PG20.13//)
 1400 FORMAT(
     & 5X,40HPLASTICITY Yield Stress CA. . . . . . .=,1PG20.13/,
     & 5X,40HPLASTICITY Hardening Parameter CB.. . .=,1PG20.13/,
     & 5X,40HPLASTICITY Hardening Exponent CN. . . .=,1PG20.13/,
     & 5X,40HEPS-MAX . . . . . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HSIG-MAX . . . . . . . . . . . . . . . .=,1PG20.13//)
 1500 FORMAT(
     & 5X,40HC0 INITIAL PRESSURE(NOT ACTIVE) . . . .=,1PG20.13/,
     & 5X,40HC HUGONIOT PARAMETERS . . . . . . . . .=,1PG20.13/,
     & 5X,40HS US=C+S UP . . . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HGAM0. . . . . . . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HA GAMA=GAM0-A X . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HATOMIC WEIGHT . . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HPRESSURE CUTOFF . . . . . . . . . . . .=,1PG20.13/,
     & 5X,40HINITIAL INTERNAL ENERGY PER UNIT VOLUME=,1PG20.13//)
 1600 FORMAT(
     & 5X,40HSTRAIN RATE COEFFICIENT CC. . . . . . .=,1PG20.13/,
     & 5X,40HREFERENCE STRAIN RATE . . . . . . . . .=,1PG20.13/,
     & 5X,40HTEMPERATURE EXPONENT. . . . . . . . . .=,1PG20.13/,
     & 5X,40HMELTING TEMPERATURE DEGREE K. . . . . .=,1PG20.13/,
     & 5X,40HTHETA-MAX . . . . . . . . . . . . . . .=,1PG20.13//)
 1700 FORMAT(
     & 5X,40HGAM0M MELTING GAMMA . . . . . . . . . .=,1PG20.13/,
     & 5X,40HAM GAMAM=GAM0M-AM X . . . . . . . . . .=,1PG20.13/,
     & 5X,40HGAME ELECTRONIC GAMMA . . . . . . . . .=,1PG20.13/,
     & 5X,40HGE ELECTRONIC ENERGY. . . . . . . . . .=,1PG20.13/,
     & 5X,40HDS ENTROPY OF MELTING . . . . . . . . .=,1PG20.13/,
     & 5X,40HTM0 MELTING TEMPERATURE PARAMETER . . .=,1PG20.13/,
     & 5X,40HVJ VOLUME WHERE EOS ARE JOINED. . . . .=,1PG20.13/,
     & 5X,40HVB EXCLUED VOLUME FOR VAPOR PHASE . . .=,1PG20.13//)
 1800 FORMAT(
     & 5X,40HUNIT (1. MBAR CM3)(1E5 SI). . . . . . .=,1PG20.13/,
     & 5X,40HE0H ENERGY AT V=V0 T=300K P=0 . . . . .=,1PG20.13/,
     & 5X,40HAY ATTRACTIVE POTENTIAL FOR VAPOR . . .=,1PG20.13/,
     & 5X,40HTHETA JOIN PARAMETER. . . . . . . . . .=,1PG20.13//)
 1900 FORMAT(
     & 5X,40HC1 . . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HC2 . . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HC3 . . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HD1 . . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HD2 . . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HD3 . . . . . . . . . . . .. . . . . . .=,1PG20.13//)
 2000 FORMAT(
     & 5X,40HE0J. . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HTMJ. . . . . . . . . . . .. . . . . . .=,1PG20.13/,
     & 5X,40HP1J. . . . . . . . . . . .. . . . . . .=,1PG20.13//)

      RETURN
      END
