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
Chd|  HM_READ_MAT52                 source/materials/mat/mat052/hm_read_mat52.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT52(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM       ,LSUBMODEL,ISRAT    ,MAT_ID   ,TITR     ,
     .           ITABLE   ,MAXTABL  ,NTABLE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW50 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXFUNC,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRAT ,NTABLE,ITABLE(MAXTABL)
      INTEGER, INTENT(OUT)                         :: NUPARAM,NUVAR
      INTEGER, INTENT(INOUT)                       :: MFUNC
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT) :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      my_real 
     .      E, NU, YEILD, ET,CSD, VISP, Q1, Q2, Q3, SN, EPSN,
     .      F1,FN,FC, FF, FU,FI,G,C1,N,FAC_L,FAC_T,FAC_M,FAC_C,
     .      R,XFAC,YFAC,RHO0,RHOR,FCUT
      INTEGER NRATE,J,I,NFUNC, IFLAG,IYIELDTAB,ITAB, ISRATE
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRAT = 1 !IPM(3)
      IYIELDTAB = 0
      NTABLE = 0
      ITABLE(1) = 0
      XFAC = ZERO
      YFAC = ZERO
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_E'        ,E         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,NU        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iflag'    ,IFLAG     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Fsmooth'      ,ISRATE    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'         ,FCUT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iyield'   ,IYIELDTAB ,IS_AVAILABLE,LSUBMODEL)
      
      CALL HM_GET_FLOATV('MAT_A'        ,YEILD     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'        ,ET        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'        ,N         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'        ,CSD       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'       ,VISP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
 
      CALL HM_GET_FLOATV('MAT_q1'       ,Q1        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_q2'       ,Q2        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_q3'       ,Q3        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_S_N'      ,SN        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS_N'    ,EPSN      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_f_I'      ,FI        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_f_N'      ,FN        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_f_C'      ,FC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_f_F'      ,FF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)


      IF(IYIELDTAB > 0) THEN
        CALL HM_GET_INT_ARRAY_INDEX  ('MAT_Tab_ID'  ,ITABLE(1)  ,I ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_XFAC'    ,XFAC       ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_YFAC'    ,YFAC       ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)         
      ENDIF
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
      
      G = HALF*E/(1.+NU)
      C1 = E / THREE / (ONE - TWO*NU)
      IF( CSD > ZERO .AND. VISP > ZERO  .AND. FCUT == ZERO) THEN
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID, 
     .                C1=TITR)
      ENDIF
      IF (FCUT == ZERO) FCUT = INFINITY
      ISRATE = 1  
      IF(ITABLE(1) /= 0) NTABLE = 1     
      IF(XFAC == ZERO) XFAC = ONE
      IF(YFAC == ZERO) YFAC  = ONE          
C.....
C check of the parameters compatibility FI<Fc<FF
C
      IF(FF < FC  .OR. FF < FI .OR.  FC < FI  ) THEN
           CALL ANCMSG(MSGID=1745,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF
  
     

      UPARAM(1)=E
      UPARAM(2)=NU
      UPARAM(3)= YEILD
      UPARAM(4)= ET
      UPARAM(5)= N
      IF(CSD == ZERO) CSD = INFINITY
      UPARAM(6)= CSD
      IF(VISP == ZERO)VISP=ONE          
      VISP=1/VISP
      UPARAM(7)=VISP
      UPARAM(8)=Q1
      UPARAM(9)=Q2
      UPARAM(10)=Q3
      UPARAM(11)=SN
      UPARAM(12)=EPSN
      UPARAM(13)=FI
      UPARAM(14)=FN
      UPARAM(15)=FC
      UPARAM(16)=FF
      IF(Q1 == ZERO)Q1=EM20
      FU=ONE/Q1
      UPARAM(17)=FU
      UPARAM(18)=IFLAG
      UPARAM(19) = ISRATE 
      UPARAM(20) = FCUT 
      IF(NTABLE == 1) UPARAM(21) = 1
      UPARAM(22) = XFAC
      UPARAM(23) = YFAC
      
C
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
      NUPARAM = 23 
       NFUNC = 0
       NUVAR = 6
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = (ONE - TWO*NU)/(ONE + NU)
      
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
C          
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,52
      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100) E,NU,YEILD, ET, N,CSD, VISP,SN,EPSN, Q1,Q2,Q3,FI,FN,FC, FF, FU,ISRATE,FCUT, IFLAG 
      IF( IYIELDTAB  > 0) THEN
       write(IOUT,1200)ITABLE(1),YFAC,XFAC
      ENDIF
           
      ENDIF
C---
      RETURN
 1000 FORMAT(
     &5X,30H  TABULATED GURSON LAW         ,/,
     &5X,30H  --------------------        ,//)
 1001 FORMAT(/
     &5X,A,/,
     &5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     &5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     &5X,'INITIAL DENSITY . . . . . . . . . . . . . . . .  =',1PG20.13/)  
 1100 FORMAT(
     &5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . . .  =',1PG20.13,/
     &5X,'POISSON''S RATIO. . . . . . . . . . . . . . . .  =',1PG20.13,/
     &5X,'YIELD STRESS. . . . . . . . . . . . . . . . . .  =',1PG20.13,/
     &5X,'TANGENT MODULUS . . . . . . . . . . . . . . . .  =',1PG20.13,/
     &5X,'HARDENING EXPONENT. . . . . . . . . . . . . . .  =',1PG20.13,/ 
     &5X,'COEFFICIENT OF THE COWPER-SYMOND .. . . . . . .  =',1PG20.13,/
     &5X,'STRAIN RATE EXPONENT IN COWPER-SYMOND LAW . . .  =',1PG20.13,/
     &5X,'GAUSSIAN STANDARD  DEVIATION. . . . . . . . . .  =',1PG20.13,/
     &5X,'NUCLUETED EFFECTIVE PLASTIC STRAIN  . . . . . .  =',1PG20.13,/
     &5x,'MATERIAL PARAMETERS :',/
     &10x, 'Q1. . . . . . . . . . . . . . .  . . . . . .   =',1PG20.13,/
     &10x, 'Q2. . . . . . . . . . . . . . .  . . . . . . . =',1PG20.13,/
     &10x, 'Q3. . . . . . . . . . . . . . .  . . . . . . . =',1PG20.13,/
     &5x,'INITIAL VOID VOLUME FRACTION . . . . .  . . . .  =',1PG20.13,/
     &5x,'NUCLUETED VOID VOLUME FRACTION . . . .  . . . .  =',1PG20.13,/
     &5x,'CRITICAL VOID VOLUME FRACTION AT COALESCENCE. .  =',1PG20.13,/
     &5x,'CRITICAL VOID VOLUME FRACTION AT DUCTILE FRACTURE=',1PG20.13,/
     &5x,'VALUE OF THE COELESCENCE VALUE. . . . . . . . .  =',1PG20.13,/
     &5X,'SMOOTH STRAIN RATE OPTION. . . . . . . . . . .   =',I10/
     &5X,'STRAIN RATE CUTTING FREQUENCY. . . . . . . . .   =',1PG20.13/  
     &5x,'The CHOICE OF VISCOPLASTIC FLOW. . . . . . . . . =',I10)        
C
 1200 FORMAT(
     & 5X,'YIELD TABLE ID . . . . . . . . .. . . . . . . . =',I10/
     & 5X,'YIELD SCALE FACTOR . . . . . . . . . . . . . .  =',1PG20.13/
     & 5X,'PLASTIC STRAIN  SCALE FACTOR  . . . . . . .. .  =',1PG20.13)

      END




