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
Chd|  HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT66(UPARAM   ,MAXUPARAM,NUPARAM   ,NUVAR  ,IFUNC    , 
     .                         MAXFUNC  ,MFUNC    , PARMAT   ,UNITAB ,ID       , 
     .                         MTAG     ,TITR     , LSUBMODEL,PM     ,MATPARAM )                     
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
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
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE HM_OPTION_READ_MOD 
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
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: MFUNC,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: I,J,IU,NC,NT,VP,ILAW,ISRATE,IRATE
      my_real :: E,EC,NU,G,CP,EPSP0,SIGY,RHO0,RHOR,
     .           FISOKIN,YFAC_UNIT,FCUT,PC,PT,C1,RPCT
      my_real :: RATE(MAXFUNC+1),YFAC(MAXFUNC+1)
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 66
C initialization
      NC = 0
      NT = 0
      EPSP0 = EP20
      CP = ONE
      VP = 0
      SIGY = ZERO
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
Card1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
Card2
      CALL HM_GET_FLOATV('MAT_E'      ,E       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'   ,FISOKIN ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_asrate' ,FCUT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_INTV  ('Fsmooth'    ,ISRATE  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('ISRATE'     ,IRATE   ,IS_AVAILABLE,LSUBMODEL)
C-----------------------------------------------
Card3
      CALL HM_GET_FLOATV('MAT_PC'     ,PC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PT'     ,PT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EC'     ,EC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_RPCT'   ,RPCT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
C!!
      IF(IRATE == 0) IRATE = 1      
Card3 
      IF(IRATE < = 3) THEN
         CALL HM_GET_INTV  ('FUN_A1'       ,IFUNC(1)   ,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV  ('FUN_A2'       ,IFUNC(2)   ,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_FLOATV('FScale11'     ,YFAC(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('FScale22'     ,YFAC(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         MFUNC = 2
C unit         
         CALL HM_GET_FLOATV_DIM('FScale11'     ,YFAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         RATE(1) = ZERO
         IF(YFAC(1) == ZERO) YFAC(1)= YFAC_UNIT
         CALL HM_GET_FLOATV_DIM('FScale22'     ,YFAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF(YFAC(2) == ZERO) YFAC(2)= YFAC_UNIT
         RATE(2) = ZERO          
      ENDIF  
C        
      IF(IRATE < = 2) THEN   
         CALL HM_GET_FLOATV('Epsilon_0'    ,EPSP0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('MAT_C0'       ,CP         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('SIGMA_r'      ,SIGY       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_INTV  ('VP'           ,VP         ,IS_AVAILABLE,LSUBMODEL)
C
         IF(EPSP0 == ZERO .AND. CP == ZERO)EPSP0 = EP20
         IF(EPSP0 == ZERO) EPSP0 = ONE !!!
         
C
      ELSEIF(IRATE == 3) THEN
         CALL HM_GET_INTV  ('FUN_B1'       ,IFUNC(3)   ,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV  ('FUN_B2'       ,IFUNC(4)   ,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_FLOATV('FScale33'     ,YFAC(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('FScale12'     ,YFAC(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)           
         MFUNC = 4
         DO I=1,MFUNC
           RATE(I) = ZERO
         ENDDO 
C unit         
         CALL HM_GET_FLOATV_DIM('FScale33'     ,YFAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF(YFAC(3) == ZERO) YFAC(3)= YFAC_UNIT
         CALL HM_GET_FLOATV_DIM('FScale12'     ,YFAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF(YFAC(4) == ZERO) YFAC(4)= YFAC_UNIT
               
      ELSEIF(IRATE == 4) THEN
        CALL HM_GET_INTV  ('NFUNC'       ,NC   ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('TFUNC'       ,NT   ,IS_AVAILABLE,LSUBMODEL)
        DO I= 1,NC
          CALL HM_GET_INT_ARRAY_INDEX('ABG_IPt' ,IFUNC(I)    ,I,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Fp1'   ,YFAC(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('K_A1'  ,RATE(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
C       
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fp1' ,YFAC_UNIT ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF(YFAC(I) == ZERO) YFAC(I) = YFAC_UNIT   
        ENDDO
C        
        IF (NC == 1) THEN
          NC = NC +1
          IFUNC(2) = IFUNC(1)
          RATE(1) = ZERO
          RATE(2) = ONE
          YFAC(2) = YFAC(1)
        ELSEIF(RATE(1) /= ZERO)THEN
          NC = NC + 1
          DO J=NC,1,-1
            IFUNC(J+1)=IFUNC(J)
            RATE(J+1) =RATE(J)
            YFAC(J+1) =YFAC(J)
          ENDDO
          RATE(1)=ZERO
        ENDIF
        
        DO I= 1,NT
          CALL HM_GET_INT_ARRAY_INDEX('ABG_IPdel',IFUNC(NC + I) ,I,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Fp2'    ,YFAC(NC  + I) ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('K_B1'   ,RATE(NC  + I) ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
C unity      
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fp2'     ,YFAC_UNIT     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF(YFAC(NC + I) == ZERO) YFAC(NC + I) = YFAC_UNIT 
        ENDDO  
        IF (NT == 1) THEN
         NT = NT +1
         IFUNC(NC + 2) = IFUNC(NC + 1)
         RATE(NC + 2)  = ONE
         RATE(NC + 1)  = ZERO
         YFAC(NC + 2)  = YFAC(NC + 1)
        ELSEIF(RATE(NC + 1) /= ZERO)THEN
          NT = NT + 1
          DO J=NT,1,-1
            IFUNC(NC + J + 1) = IFUNC(NC + J)
            RATE(NC  + J + 1) = RATE(NC + J)
            YFAC(NC  + J + 1) = YFAC(NC + J)
          ENDDO
          RATE(NC + 1)=ZERO             
        ENDIF      
        MFUNC = NC + NT          
c
        DO I=1,NC-1                        
                   IF(RATE(I) > RATE(I+1))THEN        
               CALL ANCMSG(MSGID=478,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ID,
     .                     C1=TITR)
             GOTO 100                          
            ENDIF                               
        ENDDO                                 
 100    CONTINUE
 
        DO I=1,NT-1                        
                   IF(RATE(NC+I) >= RATE(NC+I+1))THEN        
               CALL ANCMSG(MSGID=478,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ID,
     .                     C1=TITR)
             GOTO 200                          
            ENDIF                               
        ENDDO                                 
 200    CONTINUE      
c-----
      ENDIF   ! IRATE        
c
c-----------------------------------------------------------------------
c
      RHOR=RHO0
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0      
C
      DO I=1,MFUNC
        IF(IFUNC(I) == 0)THEN
            CALL ANCMSG(MSGID=126,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IFUNC(I))
          ENDIF
      ENDDO     
C
      IF(NU == HALF)NU=ZEP499
      IF(CP == ZERO .AND. IRATE == 1) CP = ONE
      IF(IRATE == 1 ) CP  = ONE/CP
      G = HALF*E/( ONE + NU)
C
c-----------------------------------------------------------------------
      UPARAM(1)  = IRATE
      UPARAM(2)  = E
      UPARAM(3)  = E/(ONE - NU*NU)
      UPARAM(4)  = NU*UPARAM(3)
      UPARAM(5)  = G
      UPARAM(6)  = NU
      UPARAM(7)  = PC
      UPARAM(8)  = PT
      UPARAM(9)  = EPSP0
      UPARAM(10) = CP
      UPARAM(11) = NC
      UPARAM(12) = NT
      UPARAM(13) = FISOKIN
      DO I= 1,MFUNC
         UPARAM(13+I) = YFAC(I)
         UPARAM(13+I+MFUNC) = RATE(I)
      ENDDO
C    
      UPARAM(14 + 2*MFUNC) = SIGY 
      UPARAM(15 + 2*MFUNC) = VP
      UPARAM(16 + 2*MFUNC) = EC
      UPARAM(17 + 2*MFUNC) = RPCT
      NUPARAM = 17 + 2*MFUNC
c     
      C1=E/THREE/(ONE - TWO*NU)
C                      
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
C
      NUVAR = 7 + MFUNC 
C-----------------------      
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
C-----------------------      
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC") 
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH") 
C ----------------      
C
      WRITE(IOUT,1001) TRIM(TITR),ID,ILAW 
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100) E,EC,NU,PC,PT,RPCT,FISOKIN,ISRATE,FCUT
        WRITE(IOUT,1200) IRATE
        IF(IRATE <= 2) THEN
           WRITE(IOUT,1300) IFUNC(1),YFAC(1)
           write(IOUT,1400) IFUNC(2),YFAC(2)
           write(IOUT,1500) EPSP0
           IF (IRATE == 1) THEN 
              WRITE(IOUT,1510) CP
            ELSE
              WRITE(IOUT,1520) CP
           ENDIF
           write(IOUT,1530) VP,SIGY
         ELSEIF(IRATE == 3) THEN 
           WRITE(IOUT,1300) IFUNC(1),YFAC(1)
           write(IOUT,1400) IFUNC(2),YFAC(2)
           write(IOUT,1600) IFUNC(3),YFAC(3)
           WRITE(IOUT,1700) IFUNC(4),YFAC(4)
         ELSE
            DO I=1,NC
              WRITE(IOUT,2000) IFUNC(I),YFAC(I),RATE(I)        
            ENDDO 
            DO I=1,NT
              WRITE(IOUT,3000) IFUNC(I+NC),YFAC(I+NC),RATE(I+NC)        
            ENDDO
         ENDIF
      ENDIF
C
c-----------------------------------------------------------------      
 1000 FORMAT(
     & 5X,' TABULATED TENSION-COMPRESSION  PLASTIC LAW   ',/,
     & 5X,'      -----------------------------           ' ,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . =',1PG20.13/) 
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG''S MODULUS IN COMPRESSION . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'COMPRESSION MEAN STRESS.  . . . . . . . .=',1PG20.13/
     & 5X,'TRACTION MEAN STRESS  . . . . . . . . . .=',1PG20.13/
     & 5X,'FRACTION OF MEAN STRESSES . . . . . . . .=',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENNING FACTOR . . . . .=',1PG20.13/   
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . . .=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . . .=',1PG20.13/)
 1200 FORMAT(    
     & 5X,'STRAIN RATE FORMULATION OPTION  . . . . .=', I10/)
 1300 FORMAT(
     & 5X,'COMPRESSION YIELD STRESS FUNCTION NUMBER.=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . .=',1PG20.13)
 1400 FORMAT(
     & 5X,'TRACTION YIELD STRESS FUNCTION NUMBER . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . .=',1PG20.13)     
 1500 FORMAT(              
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . . .=',1PG20.13)
 1510 FORMAT(
     & 5X,'STRAIN RATE PARAMETER 1/C . . . . . . . .=',1PG20.13)
 1520 FORMAT(
     & 5X,'STRAIN RATE PARAMETER C . . . . . . . . .=',1PG20.13)
 1530 FORMAT(
     & 5X,'OPTIONAL STRAIN RATE EFFECT : . . . . . .=',I10/,
     & 5X,'          0 : STRAIN RATE EFFECT IS ACTIVED ',/,
     & 5X,'          1 : VISCPLASTIC OPTION IS ACTIVED ',/,
     & 5X,'OPTIONAL INITIAL YIELD STRESS . . . . . .=',1PG20.13/)      
 1600 FORMAT(
     & 5X,
     . 'COMPRESSION STRAIN RATE SCALING EFFECT FUNCTION NUMBER .=',I10/
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . . .=',1PG20.13) 
   
 1700 FORMAT(
     & 5X,
     . 'TRACTION STRAIN RATE SCALING EFFECT FUNCTION NUMBER . . .=',I10/
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . . .=',1PG20.13) 
 2000 FORMAT(
     & 5X,'COMPRESSION YIELD STRESS FUNCTION NUMBER.=',I10/
     & 5X,' YIELD SCALE FACTOR. . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE. . . . . . . . . .=',1PG20.13)
 3000 FORMAT(
     & 5X,'TRACTION YIELD STRESS FUNCTION NUMBER . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE . . . . . . . . . . . . . . .=',1PG20.13)  
c-----------------------------------------------------------------      
      RETURN
      END
