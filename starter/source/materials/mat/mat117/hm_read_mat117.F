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
Chd|  HM_READ_MAT117                source/materials/mat/mat117/hm_read_mat117.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT117(MTAG     ,UPARAM    ,MAXUPARAM,NUPARAM  ,PM       ,
     .                          PARMAT   ,NUVAR     ,MAXFUNC  ,NFUNC    ,IFUNC,
     .                          UNITAB   ,MAT_ID    ,TITR     ,LSUBMODEL,MATPARAM )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW117 WITH HM READER - connection material
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(INOUT)          :: IFUNC(MAXFUNC), NFUNC, MAXFUNC
      INTEGER, INTENT(INOUT)          :: MAXUPARAM,
     .                                   NUPARAM,NUVAR
      INTEGER, INTENT(IN)             :: MAT_ID
      my_real, INTENT(INOUT)          :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)  :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NRATE,ILAW,IMASS,IDEL,IORDER1,IORDER2,
     .   IFAIL1,IFAIL2, IRUPT,ICRIT
      my_real :: RHO0,E,G,THICK,GC1_INI,GC2_INI,GC1_INF,GC2_INF,RATG1,RATG2,FSCALEX,FSCALEX_UNIT,
     .   FG1,FG2,SIGA1,SIGA2,SIGB1,SIGB2,RATE1,RATE2,UNIT_L,E_ELAS_N,E_ELAS_S,TMAX_N,TMAX_S,
     .                   GIC,GIIC,EXP_G,EXP_BK,DISP_0N,DISP_0S,DELTA0N,DELTA0S,UND,UTD,GAMA
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      ILAW = 117
!
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
!
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
Card1
      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IMASS'      ,IMASS        ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Fct_TN'     ,IFUNC(1)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Fct_TT'     ,IFUNC(2)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Fscale_x'   ,FSCALEX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF(FSCALEX == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Fscale_x'  ,FSCALEX_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          FSCALEX = FSCALEX_UNIT  
      ENDIF
      CALL HM_GET_FLOATV('MAT_E_ELAS_N'   ,E_ELAS_N     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E_ELAS_S'   ,E_ELAS_S     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX_N'     ,TMAX_N       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX_S'     ,TMAX_S       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('MAT_IRUPT'      ,IRUPT        ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_IDEL'       ,IDEL         ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_GIC'        ,GIC          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GIIC'       ,GIIC         ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_EXP_G'    ,EXP_G      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP_BK'   ,EXP_BK     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMMA'    ,GAMA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c---------------------------------------------------------------------------------
      PM(1) = RHO0  ! RHOR
      PM(89)= RHO0
C-------------------------------------------
C Number of User Element Variables and Curves
C-------------------------------------------
      NUVAR = 15
      NFUNC = 2
      NUPARAM = 18
c-------------------
c     Default Values
c-------------------
      IF (IDEL  == 0)   IDEL    = 1
      IF (IMASS == 0)   IMASS   = 1
      IF (IRUPT == 0)   IRUPT   = 1
      IF (GAMA  == ZERO)   GAMA   = ONE
      IF (EXP_G  == ZERO) EXP_G   = TWO
      IF (E_ELAS_S == ZERO) E_ELAS_S = E_ELAS_N
      IF ( GIC < (TMAX_N**2 / TWO/E_ELAS_N )  ) THEN
        GIC = TMAX_N**2 / TWO/E_ELAS_N
        CALL ANCMSG(MSGID=3016,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1 = MAT_ID,
     .                C1 = TITR )
      END IF
      IF ( GIIC < (TMAX_S**2 / TWO/E_ELAS_S )  ) THEN
        GIIC = TMAX_S**2 / TWO/E_ELAS_S
        CALL ANCMSG(MSGID=3017,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1 = MAT_ID,
     .                C1 = TITR )
      END IF




      DELTA0N  = TMAX_N/E_ELAS_N !single mode damage initiation normal
      DELTA0S  = TMAX_S/E_ELAS_S !single mode damage initiation tangential

      UND  = TWO*GIC /(DELTA0N *E_ELAS_N)! ultimate displacement in normal direction (UND)
      UTD  = TWO*GIIC/(DELTA0S *E_ELAS_S)! ultimate displacement in tangential direction (UTD)




C-------------------------------------------
C User Material Parameters Definition
C-------------------------------------------

      DISP_0N = TMAX_N/E_ELAS_N
      DISP_0S = TMAX_S/E_ELAS_S
      UPARAM(1)  = E_ELAS_N 
      UPARAM(2)  = E_ELAS_S 
      UPARAM(3)  = GAMA
      UPARAM(4)  = TMAX_N
      UPARAM(5)  = TMAX_S  
      UPARAM(6)  = IRUPT  
      UPARAM(7)  = GIC  
      UPARAM(8)  = GIIC
      UPARAM(9)  = IDEL  
      UPARAM(10) = EXP_G
      UPARAM(11) = EXP_BK  
      UPARAM(12) = IMASS
      UPARAM(13) = DELTA0N        !         DISP puremode 
      UPARAM(14) = DELTA0S        !         DISP puremode 
      UPARAM(15) = UND! ultimate displacement in normal direction (UND)
      UPARAM(16) = UTD! ultimate displacement in tangential direction (UTD)
      UPARAM(17) = FSCALEX

C-------------------------------------------
C Interface Contact
C-------------------------------------------
C----------------
      PARMAT(1)  = MAX(E_ELAS_N,E_ELAS_S) / THREE
      PARMAT(2)  = MAX(E_ELAS_N,E_ELAS_S)
      PARMAT(17) = ONE ! (ONE - TWO*NU)/(ONE - NU), NU=0      
c
      MTAG%L_EPE  = 3
      MTAG%L_DMG  = 1
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c 
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_COHESIVE")
c-------------------
c     STARTER Output
c-------------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE                                                     
        WRITE(IOUT,1200) RHO0,E_ELAS_N,E_ELAS_S,TMAX_N,TMAX_S,
     .                   IFUNC(1),IFUNC(2),FSCALEX,
     .                   IRUPT,GIC,GIIC,EXP_G,EXP_BK,GAMA
      ENDIF
c-----------
      RETURN
C-------------------------------------------
 1000 FORMAT(
     & 10X,' MIXED MODE COHESIVE LAW 117  ',/,
     & 10X,' ---------------------------  ',/)   
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS NORMAL TO THE PLANE OF THE COHESIVE ELEMENT .=',1PG20.13/,
     & 5X,'STIFFNESS IN TO THE PLANE OF THE COHESIVE ELEMENT . . .=',1PG20.13/,
     & 5X,'PEAK TRACTION IN NORMAL DIRECTION . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PEAK TRACTION IN TANGENTIAL DIRECTION . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION OF THE PEAK TRACTION IN NORMAL DIRECTION. . . =',I10/,
     & 5X,'FUNCTION OF THE PEAK TRACTION IN TANGENTIAL DIRECTION .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR FOR THE PEAK TRACTION FUNCTIONS .=',1PG20.13/,
c
     & 5X,'CHOICE OF PROPAGATION CRITERION . . . . . . . . . . . .=',I10/,
     & 5X,'             = 1 => POWER LAW                   '/,
     & 5X,'             = 2 => BENZEGGAGH-KENANE           '/,
c
     & 5X,'ENERGY RELEASE RATE FOR MODE I. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ENERGY RELEASE RATE FOR MODE II . . . . . . . . . . . .=',1PG20.13/,

     & 5X,'POWER LAW EXPONENT MU . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BENZEGGAGH-KENANE EXPONENT MU . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BENZEGGAGH-KENANE EXPONENT GAMMA. . . . . . . . . . . .=',1PG20.13/)

c--------
      END
