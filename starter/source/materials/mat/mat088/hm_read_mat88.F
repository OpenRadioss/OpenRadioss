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
!||    hm_read_mat88                  ../starter/source/materials/mat/mat088/hm_read_mat88.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat                    ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                         ../starter/source/output/message/message.F
!||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
!||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim              ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword               ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                   ../starter/share/modules1/elbuftag_mod.F
!||    message_mod                    ../starter/share/message_module/message_mod.F
!||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT88(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
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
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     . K,NU,G,RATE(MAXFUNC+1),VISC, VISCV,EXPO,HYS,
     . RHO0,RHOR,BULK,EMAX,FCUT,A1,A2,AA,YFAC(MAXFUNC+1),YFAC_UNL,
     . SHAPE,GS,E,ZEP495,YFAC_UNL_UNIT,YFAC_UNIT
      INTEGER
     .      J,I, II,IUNLOAD,IFLAG,IETANG,ISTIF,I2017_2,NL,IFUNC0(MAXFUNC),
     .     IFUNC_UNLOAD,ITENS,IUNL_FOR,ICASE,IADD,ILAW
      
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED         
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISTIF = 0
      IPM(3) = 1 ! 
      IMATVIS = 1 !
      ZEP495 = ZEP4 + NINE*EM02 + FIVE*EM03
      IADD = 0
      ILAW = 88

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('LAW88_Nu'   , NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW88_K'    , BULK     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW88_Fcut' , FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('LAW88_Fsmooth', ISRATE   ,IS_AVAILABLE, LSUBMODEL)      
      CALL HM_GET_INTV('LAW88_NL'      ,NL    ,IS_AVAILABLE, LSUBMODEL)
      !line-3
      CALL HM_GET_INTV('LAW88_fct_IDunL'   ,IFUNC_UNLOAD ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('LAW88_FscaleunL' ,YFAC_UNL     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW88_Hys'       ,Hys          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW88_Shape'     ,SHAPE         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('LAW88_Tension'     ,ITENS        ,IS_AVAILABLE, LSUBMODEL)

      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0

      IF(NL == 0) THEN
          CALL ANCMSG(MSGID=866,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF 
      !--loading function     
      DO I=1,NL
        CALL HM_GET_INT_ARRAY_INDEX('LAW88_arr1'           ,IFUNC(I)    ,I,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('LAW88_arr2'         ,YFAC(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('LAW88_arr3'         ,RATE(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
C unit 
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('LAW88_arr2'     ,YFAC_UNIT     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF(YFAC(I) == ZERO) YFAC(I) = YFAC_UNIT
      ENDDO 
C      
      CALL HM_GET_FLOATV_DIM('LAW88_FscaleunL' ,YFAC_UNL_UNIT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      IF(RATE(1) /= ZERO .AND. NL > 1) THEN
        DO I= NL,1, -1 
          IFUNC(I+1) = IFUNC(I)
          YFAC(I+1)  = YFAC(I)
          RATE(I+1)  = RATE(I)
        ENDDO
          IFUNC(1) = IFUNC(2)
          YFAC(1) = YFAC(2)
          RATE(1) = ZERO
          NL = NL + 1
        DO I=2,NL
          IF(RATE(I) < RATE(I-1) ) THEN
              CALL ANCMSG(MSGID=478,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=MAT_ID,
     .             C1=TITR)
           EXIT
          ENDIF
        ENDDO  
      ENDIF
      NFUNC = NL                         
C      
      IUNL_FOR = 0
      ICASE = 0 
      IF(YFAC_UNL == ZERO) YFAC_UNL = YFAC_UNL_UNIT
      IF(NL == 1) THEN ! no strain rate effect 
            IF(IFUNC_UNLOAD > 0 )THEN 
              NFUNC = NFUNC + 1
              IFUNC(NFUNC) = IFUNC_UNLOAD
              YFAC(NFUNC) = YFAC_UNL
              RATE(NFUNC) = ZERO
              IUNL_FOR = 1  ! using unloading curve
            ELSEIF(HYS /= ZERO) THEN
              IUNL_FOR = 2 ! based on the energy 
              HYS = ABS(HYS)
            ELSE
              IUNL_FOR = 0  ! no unloading curve, 
            ENDIF     
      ELSE ! strain rate effect
            IF(IFUNC_UNLOAD > 0) THEN
              NFUNC = NFUNC + 1
              IFUNC(NFUNC) = IFUNC_UNLOAD
              YFAC(NFUNC) = YFAC_UNL
              RATE(NFUNC) = ZERO
              IUNL_FOR = 1  ! using unloading curve
            ELSEIF(HYS /= ZERO )THEN
              IUNL_FOR = 3 ! based on the energy 
              HYS = ABS(HYS)
            ELSE ! using quasistatic curve for unloading 
              NFUNC = NFUNC + 1
              IFUNC(NFUNC) = IFUNC(1)
              YFAC(NFUNC) =  YFAC(1)
              RATE(NFUNC) = ZERO
              IUNL_FOR = 1  ! using unloading curve
            ENDIF   
      ENDIF 
C       
      IF(SHAPE == ZERO) SHAPE = ONE
      IF(HYS == ZERO) HYS = ONE
      IF(NU == ZERO) NU = ZEP495
      GS =  THREE_HALF*BULK*(ONE - TWO*NU)/(ONE + NU)
      E = TWO*GS*(ONE + NU)
      IF (GS<=0) THEN
        CALL ANCMSG(MSGID=828,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANSTOP,
     .              I1=MAT_ID,
     .              C1=TITR)
      END IF
      IF (FCUT == ZERO .AND. NL > 1 ) THEN
        FCUT = EP03*UNITAB%FAC_T_WORK
        ISRATE = 1  
      ENDIF
C
      UPARAM(1) = BULK
      UPARAM(2) = NU
      UPARAM(3) = GS
      UPARAM(4) = NL
      UPARAM(5) = IUNL_FOR
      UPARAM(6) = HYS
      UPARAM(7) = SHAPE
      UPARAM(8) = ITENS
      UPARAM(9) = ICASE
      NUPARAM = 9
C      
      DO I=1,NFUNC
         UPARAM( NUPARAM + 2*I - 1) = RATE(I) 
         UPARAM( NUPARAM + 2*I    ) = YFAC(I)
      ENDDO
      NUPARAM  = NUPARAM + 2*NFUNC 
      UPARAM(NUPARAM + 1 : NUPARAM + 5) = ZERO
      NUPARAM = NUPARAM + 5 ! used inside law88_upd.F  but not used in engine
C    
      NUVAR   =  32 
C
      PARMAT(1) = TWO*GS 
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*GS/(BULK + FOUR_OVER_3*GS)

      ! MTAG variable activation
      MTAG%L_EPSD = 1
      MTAG%G_EPSD = 1
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
c-----------------
      WRITE(IOUT,1010) TRIM(TITR),MAT_ID,88   
      WRITE(IOUT,1000)     
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
        WRITE(IOUT,1020)RHO0 
        WRITE(IOUT,1100)NU,BULK,ITENS,NL-IADD
        WRITE(IOUT,1200)(IFUNC(I),YFAC(I),RATE(I),I=1+IADD,NL)   
        WRITE(IOUT,1250) ISRATE,FCUT
        IF(IUNL_FOR == 1) THEN
         II = NL
         WRITE(IOUT,1300)IFUNC(NFUNC),YFAC_UNL 
        ELSEIF(IUNL_FOR == 2 .or. IUNL_FOR == 3) THEN
         write(IOUT,1400) HYS, SHAPE
        ENDIF
        WRITE(IOUT,1500) ITENS
      ENDIF     
C-----------------
      RETURN
C-----------------
 1000 FORMAT
     & (5X,'TABULATED OGDEN MATERIAL LAW-(LAW88)',/,
     &  5X,'------------------------------------',//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . .=',I10/) 
 1020 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . . .=',1PG20.13/) 
 1100 FORMAT
     &(5X,'POISSON RATIO. . . . . . . . . .  . . . . =',1PG20.13/
     &,5X,'BULK MODULUS. . . . . . . . . . . . . . . =',1PG20.13/
     &,5X,'STRAIN RATE EFFECT FLAG  . .. . . . . . . =',I10/
     &,5X,'NUMBER OF LOADING  FUNCTION . . .. . . . .=',I10//)
 1200 FORMAT(
     & 5X,'LOADING STRESS-STRAIN FUNCTION NUMBER. . .=',I10/
     & 5X,'STRESS SCALE FACTOR. . . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE . . . . . . . . . . . . . . . =',1PG20.13)   
 1250 FORMAT(
     & 5X,'STRAIN RATE FILTERING FLAG. . . . . . . . =',I10/  
     & 5X,'STRAIN RATE FILTERING CUTOFF FREQUENCY. . =',1PG20.13/)   
 1300 FORMAT(
     & 5X,'UNLOADING STRESS-STRAIN FUNCTION NUMBER. .=',I10/
     & 5X,'STRESS SCALE FACTOR. . . . . . . . . . . .=',1PG20.13/)    
 1400 FORMAT
     &(5X,'HYSTERETIC UNLOADING FACTOR. . . . .  . . =',1PG20.13/
     &,5X,'SHAPE UNLOADING FACTOR. . . . . . . . . . =',1PG20.13//) 
 1500 FORMAT
     &(5X,'ITENSION : PARAMETER FOR UNLOADING . . . .=',I10/)
C-----------------
      RETURN
   

      END SUBROUTINE HM_READ_MAT88
