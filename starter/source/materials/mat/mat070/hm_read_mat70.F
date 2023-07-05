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
Chd|  HM_READ_MAT70                 source/materials/mat/mat070/hm_read_mat70.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT70(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,ID       ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW70 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
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
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real E,NU,G,C1,RATE(MAXFUNC),VISC, VISCV,YFAC(MAXFUNC),EXPO,HYS,YFAC_UNIT(MAXFUNC),
     .        RHO0,RHOR,E0,EMAX,EPSMAX,FCUT,A1,A2,AA
      INTEGER NRATEN,J,I,NRATEP, IUNLOAD,IFLAG,IETANG,ISTIF,I2017_2,NL
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED         
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISTIF = 0
      I2017_2 = 0
      IPM(3) = 1
      IMATVIS = 2

      RATE(1:MAXFUNC) = ZERO
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E0'    ,E0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('E_Max'     ,EMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'   ,EPSMAX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Itens'       ,IFLAG    ,IS_AVAILABLE, LSUBMODEL)
      !line-3
      CALL HM_GET_FLOATV('MAT_asrate',FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('ISRATE'      ,ISRATE   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('NRATEP'      ,NRATEP   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('NRATEN'      ,NRATEN   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('MAT_Iflag'   ,IUNLOAD  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_SHAPE' ,EXPO     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HYST'  ,HYS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0

      IF(NRATEP == 0) THEN
          CALL ANCMSG(MSGID=866,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR)
      ENDIF 
      !--loading function     
      DO I=1,NRATEP
        CALL HM_GET_INT_ARRAY_INDEX('FUN_LOAD'           ,IFUNC(I)    ,I,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_LOAD'  ,RATE(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_LOAD'       ,YFAC(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !IF(I==1)CALL HM_GET_INT_ARRAY_INDEX('I2017_2'        ,I2017_2     ,IS_AVAILABLE, LSUBMODEL)
        !units
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_LOAD'  ,YFAC_UNIT(I) ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)         
        IF(YFAC(I) == ZERO) YFAC(I) = ONE * YFAC_UNIT(I)         
      ENDDO  
                         
      !--unloading function
      IF(NRATEN > 0 ) THEN
        DO I=NRATEP+1,NRATEP+NRATEN
          CALL HM_GET_INT_ARRAY_INDEX('FUN_UNLOAD'            ,IFUNC(I)    ,I-NRATEP,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_UNLOAD'   ,RATE(I)     ,I-NRATEP,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_UNLOAD'        ,YFAC(I)     ,I-NRATEP,IS_AVAILABLE, LSUBMODEL, UNITAB)   
          !units
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_UNLOAD'    ,YFAC_UNIT(I),I-NRATEP,IS_AVAILABLE, LSUBMODEL, UNITAB)         
          IF(YFAC(I) == ZERO) YFAC(I) = ONE * YFAC_UNIT(I)         
        ENDDO 
      ELSE 
        IF(NRATEN == 0 .AND. IUNLOAD <= 2 ) THEN
             NRATEN = 1
             IFUNC(NRATEP+1) = IFUNC(1)
             YFAC (NRATEP+1) = YFAC(1)
             RATE (NRATEP+1) = RATE(1)
             IUNLOAD = 0
C             
             CALL ANCMSG(MSGID=1226,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
         ENDIF
      ENDIF                     
C
      IF(RATE(1) /=  ZERO)THEN
          CALL ANCMSG(MSGID=1721,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR)       
       
        RATE(1) = ZERO
      ENDIF 
      IF(RATE(NRATEP + 1) /= ZERO) THEN
        RATE(NRATEP + 1) = ZERO 
          CALL ANCMSG(MSGID=1722,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR)      
      ENDIF  
CC
      DO I=1,NRATEP-1
        IF(RATE(I) >= RATE(I+1))THEN
          CALL ANCMSG(MSGID=478,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR)
          EXIT
         ENDIF
      ENDDO  
C 
      IF(NRATEP > 1 .OR. (IUNLOAD == 0 .AND. NRATEN > 1)) THEN      
        IF(ISRATE == 0 .OR. FCUT == ZERO ) THEN
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
       
        ENDIF
      ENDIF 
      IF (FCUT == 0) FCUT = INFINITY
      NL = NRATEP + NRATEN
      IF(IFLAG > 0) THEN
         NL = NRATEP + NRATEN  +1
         RATE(NL) = ZERO
         CALL HM_GET_INT_ARRAY_INDEX('FUN_A1'        ,IFUNC(NL)    ,NL,IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOAT_ARRAY_INDEX('FScale11'    ,YFAC(NL)     ,NL,IS_AVAILABLE, LSUBMODEL, UNITAB)   
         !units
         IF (YFAC(NL) == ZERO) YFAC(NL) = ONE
      ENDIF 
      
      DO I=1,NL
        IF(IFUNC(I) == 0)THEN
          CALL ANCMSG(MSGID=126,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
        ENDIF
      ENDDO
C      
      IF(EPSMAX == ZERO) EPSMAX = ONE
      UPARAM(1)=NL
      UPARAM(2)=E0
      AA = (EMAX-E0)/EPSMAX
      UPARAM(3)=AA
      UPARAM(4)=EPSMAX
      G = HALF*E0/(ONE + NU)
      UPARAM(5)=G
      UPARAM(6)=NU       
      UPARAM(7)= NRATEP
      UPARAM(8)= NRATEN
      DO I=1,NL
         UPARAM(I + 8)      = RATE(I)
         UPARAM(I + 8 + NL) = YFAC(I)
      END DO

      UPARAM(2*NL + 9)= IUNLOAD
      IF(EXPO == ZERO) EXPO = ONE
      IF(HYS  == ZERO) HYS = ONE
      UPARAM(2*NL + 10)= EXPO
      UPARAM(2*NL + 11) = HYS
      UPARAM(2*NL + 12) = EMAX
      UPARAM(2*NL + 13) = IFLAG
      UPARAM(2*NL + 14) = I2017_2
C
      C1=E0/THREE/(ONE - TWO*NU)
      NFUNC = NL 
      NUPARAM =15 + 2*NL  
      UPARAM(NUPARAM) = NFUNC
C
      PARMAT(1) = C1
      PARMAT(2) = E0
      PARMAT(3) = NU
C      
      IF(ISTIF > 0) THEN
        C1=EMAX/THREE/(ONE - TWO*NU)
        PARMAT(1) = C1
        PARMAT(2) = EMAX 
      ENDIF
C
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = (ONE-TWO*NU)/(ONE-NU)
C
      NUVAR = 10 + 6
      ISRATE = 1
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SMALL_STRAIN")
      IF (NU > 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c------------------------------------
      WRITE(IOUT,1001) TRIM(TITR),ID,70    
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1050)RHO0      
        WRITE(IOUT,1100)E0,NU,EMAX,EPSMAX
        WRITE(IOUT,1200)FCUT,ISRATE,NRATEP,NRATEN,IUNLOAD,EXPO,HYS,IFLAG
        WRITE(IOUT,1300)(IFUNC(J),RATE(J),YFAC(J),J=1,NRATEP)
        WRITE(IOUT,1400)(IFUNC(J+NRATEP),RATE(J+NRATEP),YFAC(J+NRATEP),
     .                 J=1,NRATEN)
        IF(IFLAG > 0) THEN
           WRITE(IOUT,1500)IFUNC(NL),YFAC(NL)        
        ENDIF
      ENDIF
c-----------
      RETURN
 1000 FORMAT(
     & 5X,40H  TABULATED NON LINEAR VISCO ELASTIC LAW,/,
     & 5X,40H  --------------------------------------,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'INITIAL YOUNG''S MODULUS. . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM YOUNG''S MODULUS. . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM STRAIN . . . . .. . . . . . . .=',1PG20.13)
 1200 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE       . . . .. . .=',I10/,
     & 5X,'NUMBER OF LOAD STRESS FUNCTIONS    .. .=',I10/,
     & 5X,'NUMBER OF UNLOAD STRESS FUNCTIONS  .. .=',I10/,
     & 5X,'CHOICE OF UNLOADING FORMULATION  . . . =',I10/,
     & 5X,'SHAPE FACTOR FOR UNLOADING  . . . . . .=',1PG20.13/,
     & 5X,'HYSTERETIC UNLOADING FACTOR  . . . . . =',1PG20.13/, 
     & 5X,'FLAG  FOR TENSION BEHAVIOR . . . . . . =',I10/)
CC     & 5X,'FLAG CHOICE FOR YOUNG MODULUS . . . .  =',I10    )
 1300 FORMAT(
     & 5X,'LOAD YIELD STRESS FUNCTION NUMBER.. . .=',I10/,
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/)

 1400 FORMAT(
     & 5X,'UNLOAD YIELD STRESS FUNCTION NUMBER.. .=',I10/,
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'PRESSURE DEPENDENT YIELD FUNCTION . . .=',I10/
     & 5X,'PRESSURE SCALE FACTOR. . . . . . . . . =',1PG20.13)     
 1050 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  

      END SUBROUTINE HM_READ_MAT70
