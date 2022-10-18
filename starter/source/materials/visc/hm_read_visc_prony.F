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
Chd|  HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|-- called by -----------
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        LM_LEAST_SQUARE_PRONY         source/materials/visc/hm_read_visc_prony.F
Chd|        LM_LEAST_SQUARE_PRONY_2       source/materials/visc/hm_read_visc_prony.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_VISC_PRONY(UPARAM ,MAXUPARAM,NUPARAM,
     .                  NUVAR,IFUNC,MAXFUNC,NFUNC,UNITAB,LSUBMODEL,
     .                  TABLE,MAT_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      INTEGER MAXUPARAM,NUPARAM,NUVAR,MAXFUNC,NFUNC,
     .        IFUNC(MAXFUNC),IUNIT,MAT_ID
      my_real   UPARAM(MAXUPARAM) 
      TYPE(TTABLE) TABLE(NTABLE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: FctID_G,FctID_K,ITAB,ISHAPE,
     .           FctID_Gs,FctID_Ks,FctID_Gl,FctID_Kl
      INTEGER I,NPRONY,IFLAG,IMOD
      my_real 
     . KV,G(100),BETA(100),K(100),BETAK(100),
     . XGscale,XKscale,XGscale_UNIT,XKscale_UNIT,
     . YGscale,YKscale,YGscale_UNIT,YKscale_UNIT,
     . XGs_scale,YGs_scale,XGs_scale_UNIT,YGs_scale_UNIT,
     . XGl_scale,YGl_scale,XGl_scale_UNIT,YGl_scale_UNIT,
     . XKs_scale,YKs_scale,XKs_scale_UNIT,YKs_scale_UNIT,
     . XKl_scale,YKl_scale,XKl_scale_UNIT,YKl_scale_UNIT,
     . COSTFG,COSTFK,DERIVG,DERIVK,GINFINI,KINFINI
C      
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================   
C     
      ! Table initialization
      G(1:100)     = ZERO
      BETA(1:100)  = ZERO
      K(1:100)     = ZERO
      BETAK(1:100) = ZERO
      
C
      !IFLAG = 0 this flag was dedicated to keep old fomrulation for version older than
      ! 1st Card - Flags and prony order 
      CALL HM_GET_INTV   ('Model_Order' ,NPRONY ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_FLOATV ('MAT_K'       ,KV     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('MAT_Itab'    ,ITAB   ,IS_AVAILABLE,LSUBMODEL) 
      IF (ITAB > 2) ITAB = 0
      CALL HM_GET_INTV   ('MAT_Ishape'  ,ISHAPE ,IS_AVAILABLE,LSUBMODEL) 
C
      IF (NPRONY == 0)CALL ANCMSG(MSGID=2026,MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,I1=MAT_ID)    
C
      IF (ISHAPE > 1) ISHAPE = 0
C
      ! 2nd Card - Input depending on flags
      ! =======================================================================================
      ! Tabulated inputs
      ! =======================================================================================
      !  -> Itab = 1 : functions of time
      IF (ITAB == 1) THEN 
        !----------------------------------------------------------------------------------
        ! -> Shear modulus VS time
        !----------------------------------------------------------------------------------
        CALL HM_GET_INTV   ('Fct_G'  ,FctID_G,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_FLOATV ('XGscale',XGscale,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (XGscale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('XGscale',XGscale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XGscale = ONE * XGscale_UNIT      
        ENDIF
        CALL HM_GET_FLOATV ('YGscale',YGscale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (YGscale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('YGscale' ,YGscale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YGscale = ONE * YGscale_UNIT      
        ENDIF
        !----------------------------------------------------------------------------------
        ! -> Bulk modulus VS time
        !----------------------------------------------------------------------------------
        CALL HM_GET_INTV   ('Fct_K'  ,FctID_K,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_FLOATV ('XKscale',XKscale,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (XKscale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('XKscale',XKscale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XKscale = ONE * XKscale_UNIT      
        ENDIF
        CALL HM_GET_FLOATV ('YKscale',YKscale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (YKscale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('YKscale' ,YKscale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YKscale = ONE * YKscale_UNIT      
        ENDIF
        !----------------------------------------------------------------------------------------
        ! Fit Prony series parameters to approximate tabulated functions by Least Squares method
        !----------------------------------------------------------------------------------------
        IF ((FctID_G > 0).AND.(NPRONY > 0)) THEN
          CALL LM_LEAST_SQUARE_PRONY(MAT_ID   ,NPRONY   ,TABLE    ,FctID_G  ,XGscale  ,YGscale  ,
     .                               G        ,BETA     ,COSTFG   ,DERIVG   ,ISHAPE   ,GINFINI  )
        ENDIF
        IF ((FctID_K > 0).AND.(KV == ZERO).AND.(NPRONY > 0)) THEN
          CALL LM_LEAST_SQUARE_PRONY(MAT_ID   ,NPRONY   ,TABLE    ,FctID_K  ,XKscale  ,YKscale  ,
     .                               K        ,BETAK    ,COSTFK   ,DERIVK   ,ISHAPE   ,KINFINI  )
        ENDIF
      !  -> Itab = 2 ! loss and storage moduli as functions of frequencies
      ELSEIF (ITAB == 2) THEN 
        !----------------------------------------------------------------------------------
        ! -> Shear storage modulus VS frequency
        !----------------------------------------------------------------------------------
        CALL HM_GET_INTV   ('Fct_Gs'    ,FctID_Gs  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV ('XGs_scale' ,XGs_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (XGs_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('XGs_scale',XGs_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XGs_scale = ONE * XGs_scale_UNIT      
        ENDIF
        CALL HM_GET_FLOATV ('YGs_scale' ,YGs_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (YGs_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('YGs_scale',YGs_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YGs_scale = ONE * YGs_scale_UNIT      
        ENDIF        
        !----------------------------------------------------------------------------------
        ! -> Shear loss modulus VS frequency
        !----------------------------------------------------------------------------------
        CALL HM_GET_INTV   ('Fct_Gl'    ,FctID_Gl  ,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_FLOATV ('XGl_scale' ,XGl_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (XGl_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('XGl_scale',XGl_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XGl_scale = ONE * XGl_scale_UNIT      
        ENDIF
        CALL HM_GET_FLOATV ('YGl_scale' ,YGl_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (YGl_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('YGl_scale',YGl_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YGl_scale = ONE * YGl_scale_UNIT      
        ENDIF 
        !----------------------------------------------------------------------------------
        ! -> Bulk storage modulus VS frequency
        !----------------------------------------------------------------------------------
        CALL HM_GET_INTV   ('Fct_Ks'    ,FctID_Ks  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV ('XKs_scale' ,XKs_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (XKs_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('XKs_scale',XKs_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XKs_scale = ONE * XKs_scale_UNIT      
        ENDIF
        CALL HM_GET_FLOATV ('YKs_scale' ,YKs_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (YKs_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('YKs_scale',YKs_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YKs_scale = ONE * YKs_scale_UNIT      
        ENDIF    
        !----------------------------------------------------------------------------------
        ! -> Bulk loss modulus VS frequency
        !----------------------------------------------------------------------------------
        CALL HM_GET_INTV   ('Fct_Kl'    ,FctID_Kl  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV ('XKl_scale' ,XKl_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (XKl_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('XKl_scale',XKl_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XKl_scale = ONE * XKl_scale_UNIT      
        ENDIF
        CALL HM_GET_FLOATV ('YKl_scale' ,YKl_scale ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (YKl_scale == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('YKl_scale',YKl_scale_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YKl_scale = ONE * YKl_scale_UNIT      
        ENDIF  
        !----------------------------------------------------------------------------------------
        ! Fit Prony series parameters to approximate tabulated functions by Least Squares method
        !----------------------------------------------------------------------------------------
        IF ((FctID_Gs > 0).AND.(FctID_Gl > 0).AND.(NPRONY > 0)) THEN
          CALL LM_LEAST_SQUARE_PRONY_2(MAT_ID   ,NPRONY   ,TABLE    ,FctID_Gs ,XGs_scale,YGs_scale,
     .                                 FctID_Gl ,XGl_scale,YGl_scale,G        ,BETA     ,COSTFG   ,
     .                                 DERIVG   ,ISHAPE   ,GINFINI  )      
        ENDIF
        IF ((FctID_Ks > 0).AND.(FctID_Kl > 0).AND.(NPRONY > 0).AND.(KV == ZERO)) THEN
          CALL LM_LEAST_SQUARE_PRONY_2(MAT_ID   ,NPRONY   ,TABLE    ,FctID_Ks ,XKs_scale,YKs_scale,
     .                                 FctID_Kl ,XKl_scale,YKl_scale,K        ,BETAK    ,COSTFK   ,
     .                                 DERIVK   ,ISHAPE   ,KINFINI  )      
        ENDIF
      ! =======================================================================================
      ! Classical input
      ! =======================================================================================
      !  -> Itab = 0 ! classical input of prony series
      ELSE 
        ISHAPE = 0
        IF(NPRONY > 0) THEN
          DO I=1,NPRONY
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fport1' ,G(I)    ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Fporp1' ,BETA(I) ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Ki'     ,K(I)    ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)          
            CALL HM_GET_FLOAT_ARRAY_INDEX('Beta_ki',BETAK(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          ENDDO
        ENDIF
      ENDIF
C     
      ! Taking into account the infinite value shape (+ 1 prony function with beta = 0)
      IF ((ITAB /= 0) .AND. (ISHAPE == 1)) THEN 
        NPRONY = NPRONY + 1
      ENDIF
C
      ! Storing parameters in UPARAM table
      IMOD = 0
      UPARAM(1) = KV
      UPARAM(2) = NPRONY
      DO I=1,NPRONY 
        UPARAM(2 + I) = G(I)
        UPARAM(2 + NPRONY + I) = BETA(I)  
        UPARAM(2 + 2*NPRONY + I) = K(I)
        UPARAM(2 + 3*NPRONY + I) = BETAK(I)
        IF(K(I) > ZERO) IMOD  = 1
      ENDDO 
      NUPARAM = 3 + 4*NPRONY 
      NUPARAM = 4 + 4*NPRONY 
      UPARAM(NUPARAM) = IMOD
      NUVAR = 6 + 7*NPRONY
C
      NFUNC = 0
C
      IF (IS_ENCRYPTED)THEN                                
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
C      
        IF(NPRONY > 0) THEN
         WRITE(IOUT,1000)
         IF(IMOD == 0) THEN 
          WRITE(IOUT,1100) KV,NPRONY-ISHAPE
          IF (ITAB > 0) THEN 
            WRITE(IOUT,1500) ITAB,ISHAPE
            IF (ISHAPE == 1) WRITE(IOUT,3000) GINFINI
            IF (ITAB == 1) THEN 
              WRITE(IOUT,1600) FctID_G,XGscale,YGscale,COSTFG,DERIVG
            ELSEIF (ITAB == 2) THEN
              WRITE(IOUT,2000) FctID_Gs,XGs_scale,YGs_scale,
     .                         FctID_Gl,XGl_scale,YGl_scale,
     .                         COSTFG,DERIVG
            ENDIF
          ENDIF
          DO I=1,NPRONY-ISHAPE
            WRITE(IOUT,1150) I
            WRITE(IOUT,1200) G(I+ISHAPE),BETA(I+ISHAPE)
          ENDDO
         ELSE
           WRITE(IOUT,1300) NPRONY-ISHAPE
           IF (ITAB > 0) THEN 
             WRITE(IOUT,1500) ITAB,ISHAPE
             IF (ITAB == 1) THEN 
               IF (ISHAPE == 1) WRITE(IOUT,3000) GINFINI
               WRITE(IOUT,1600) FctID_G,XGscale,YGscale,COSTFG,DERIVG
               IF (ISHAPE == 1) WRITE(IOUT,3500) KINFINI
               WRITE(IOUT,1800) FctID_K,XKscale,YKscale,COSTFK,DERIVK
             ELSEIF (ITAB == 2) THEN
               IF (ISHAPE == 1) WRITE(IOUT,3000) GINFINI
               WRITE(IOUT,2000) FctID_Gs,XGs_scale,YGs_scale,
     .                          FctID_Gl,XGl_scale,YGl_scale,
     .                          COSTFG,DERIVG
               IF (ISHAPE == 1) WRITE(IOUT,3500) KINFINI
               WRITE(IOUT,2500) FctID_Ks,XKs_scale,YKs_scale,
     .                          FctID_Kl,XKl_scale,YKl_scale,
     .                          COSTFK,DERIVK
             ENDIF
           ENDIF
           DO I=1,NPRONY-ISHAPE
              WRITE(IOUT,1150) I
              WRITE(IOUT,1200) G(I+ISHAPE),BETA(I+ISHAPE)
              WRITE(IOUT,1400) K(I+ISHAPE),BETAK(I+ISHAPE)
           ENDDO 
         ENDIF 
        ENDIF 
      ENDIF               
C-----------        
 1000 FORMAT(
     & 5X,'  PRONY SERIES MODEL  :'         ,/,
     & 5X,' --------------------- '         ,/)
 1100 FORMAT(
     & 5X,'BULK MODULUS FOR VISCO ELASTIC MATERIAL . . . . . . . . . . . . . . . =',1PG20.13/   
     & 5X,'ORDER OF PRONY SERIES . . . . . . . . . . . . . . . . . . . . . . . . =',I10/)    
 1150 FORMAT(
     & 5X,' ----------------------------------------------------------------------'/
     & 5X,' PARAMETERS FOR PRONY FUNCTION NUMBER #',I10/
     & 5X,' ----------------------------------------------------------------------'/)   
 1200 FORMAT(
     & 5X,'SHEAR RELAXATION G MODULUS  . . . . . . . . . . . . . . . . . . . . . = '1PG20.13/
     & 5X,'BETA DECAY SHEAR MODULUS  . . . . . . . . . . . . . . . . . . . . . . =',1PG20.13) 
 1300 FORMAT(   
     & 5X,'ORDER OF PRONY SERIES . . . . . . . . . . . . . . . . . . . . . . . . =',I10//) 
 1400 FORMAT(
     & 5X,'BULK RELAXATION K MODULUS . . . . . . . . . . . . . . . . . . . . . . = '1PG20.13/
     & 5X,'BETAK DECAY BULK MODULUS  . . . . . . . . . . . . . . . . . . . . . . =',1PG20.13//)  
 1500 FORMAT(   
     & 5X,'TABULATED PRONY SERIES FLAG  . . . . . . . . . . . . . . . . . . . . .=',I10/
     & 5X,'  ITAB=1  FITTING FROM MODULUS VS TIME CURVES'/
     & 5X,'  ITAB=2  FITTING FROM STORAGE AND LOSS MODULI VS FREQUENCY CURVES'/
     & 5X,'SHAPE PRONY SERIES FLAG  . . . . . . . . . . . . . . . . . . . . . . .=',I10/
     & 5X,'  ISHAPE=0 WITHOUT INFINITE MODULUS (DEFAULT)'/
     & 5X,'  ISHAPE=1 WITH INFINITE MODULUS'/)          
 1600 FORMAT(
     & 5X,'LEAST SQUARE FITTING FROM SHEAR MODULUS G FUNCTION ID. . . . . . . . .= 'I10/
     & 5X,'TIME SCALE FACTOR FOR SHEAR MODULUS  . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'SCALE FACTOR FOR SHEAR MODULUS . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL COST FUNCTION VALUE  . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL DERIVATIVE VALUE . . . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/)     
 1800 FORMAT(
     & 5X,'LEAST SQUARE FITTING FROM BULK MODULUS K FUNCTION ID . . . . . . . . .= 'I10/
     & 5X,'TIME SCALE FACTOR FOR BULK MODULUS . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'SCALE FACTOR FOR BULK MODULUS  . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL COST FUNCTION VALUE  . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL DERIVATIVE VALUE . . . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/)
 2000 FORMAT(
     & 5X,'LEAST SQUARE FITTING FROM STORAGE SHEAR MODULUS GL FUNCTION ID . . . .= 'I10/
     & 5X,'FREQUENCY SCALE FACTOR FOR STORAGE SHEAR MODULUS . . . . . . . . . . .= '1PG20.13/
     & 5X,'SCALE FACTOR FOR STORAGE SHEAR MODULUS . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'LEAST SQUARE FITTING FROM LOSS SHEAR MODULUS GS FUNCTION ID  . . . . .= 'I10/
     & 5X,'FREQUENCY SCALE FACTOR FOR LOSS SHEAR MODULUS  . . . . . . . . . . . .= '1PG20.13/
     & 5X,'SCALE FACTOR FOR LOSS SHEAR MODULUS FUNCTION . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL COST FUNCTION VALUE  . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL DERIVATIVE VALUE . . . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/)
 2500 FORMAT(
     & 5X,'LEAST SQUARE FITTING FROM STORAGE BULK MODULUS KL FUNCTION ID  . . . .= 'I10/
     & 5X,'FREQUENCY SCALE FACTOR FOR STORAGE BULK MODULUS  . . . . . . . . . . .= '1PG20.13/
     & 5X,'SCALE FACTOR FOR STORAGE BULK MODULUS  . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'LEAST SQUARE FITTING FROM LOSS BULK MODULUS GS FUNCTION ID . . . . . .= 'I10/
     & 5X,'FREQUENCY SCALE FACTOR FOR LOSS BULK MODULUS . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'SCALE FACTOR FOR LOSS BULK MODULUS FUNCTION  . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL COST FUNCTION VALUE  . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'FINAL DERIVATIVE VALUE . . . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/)
 3000 FORMAT(
     & 5X,'SHEAR MODULUS INFINITE VALUE GINF  . . . . . . . . . . . . . . . . . .= '1PG20.13/)
 3500 FORMAT(
     & 5X,'BULK MODULUS INFINITE VALUE KINF . . . . . . . . . . . . . . . . . . .= '1PG20.13/)
     
      RETURN
      END

Chd|====================================================================
Chd|  LM_LEAST_SQUARE_PRONY         source/materials/visc/hm_read_visc_prony.F
Chd|-- called by -----------
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LM_LEAST_SQUARE_PRONY(MAT_ID   ,NPRONY   ,TABLE    ,FCT_ID   ,XGSCALE  ,YGSCALE  ,
     .                                 G        ,BETA     ,COST     ,DERIV    ,ISHAPE   ,GINFINI  )  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   Dummy arguments
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPRONY,FCT_ID,MAT_ID,ISHAPE
      my_real
     .        XGSCALE,YGSCALE,COST,DERIV,GINFINI
      my_real, DIMENSION(100) :: G,BETA
      TYPE(TTABLE) TABLE(NTABLE)     
C-----------------------------------------------
C   Local arguments
C-----------------------------------------------
      INTEGER I,II,INFO,SIZE_TIME,NITER
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPIV
      DOUBLE PRECISION :: 
     .        RI2,RI2_OLD,MXVALT,MXVALY,LAMBDA,
     .        JACTDY_N2,RHO,NU,DENOM,XI_N2
      DOUBLE PRECISION, PARAMETER :: EPS1   = 1.0D-8
      DOUBLE PRECISION, PARAMETER :: EPS2   = 1.0D-10
      DOUBLE PRECISION, PARAMETER :: ALPHA  = 0.5D0
      DOUBLE PRECISION, PARAMETER :: MXITER = 10000
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XI,FXI,DELTAY,JACTDY,YI,TIME,
     .                                               XI_TR,FXI_TR,VECT,MU
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: JAC,JACTJAC,JACT,ID    
      LOGICAL :: CONV,FOUND
C
      ! Find the function and recover the experimental data
      FOUND = .FALSE.
      DO I = 1,NTABLE
        IF (TABLE(I)%NOTABLE == FCT_ID) THEN
          ! Number of data points
          SIZE_TIME = SIZE(TABLE(I)%X(1)%VALUES)
          ALLOCATE(YI(SIZE_TIME),TIME(SIZE_TIME))
          ! Recovering data (time and modulus)
          DO II = 1,SIZE_TIME
            TIME(II) = TABLE(I)%X(1)%VALUES(II)*XGSCALE
            YI(II)   = TABLE(I)%Y%VALUES(II)*YGSCALE
          ENDDO
          ! Normalizing the curves to improve the optimization
          MXVALT  = MAXVAL(TIME)
          MXVALY  = MAXVAL(YI)
          ! Considering infinite value shape
          IF (ISHAPE == 1) THEN
            GINFINI = YI(SIZE_TIME)/MXVALY
          ELSE
            GINFINI = ZERO
          ENDIF
          TIME    = TIME/MXVALT
          YI      = YI/MXVALY
          FOUND   = .TRUE.
          EXIT
        ENDIF        
      ENDDO  
      IF (.NOT.FOUND) THEN 
        CALL ANCMSG(MSGID=1928,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,I2=FCT_ID)     
      ENDIF
C
      ! Order of prony series to high for the number of data points
      IF (2*NPRONY >= SIZE_TIME) THEN
        CALL ANCMSG(MSGID=1921,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,I2=NPRONY,
     .              I3=SIZE_TIME,I4=FLOOR(SIZE_TIME/TWO))
      ENDIF
C      
      ! Allocation and initialization of the vectors
      ALLOCATE(XI(2*NPRONY),XI_TR(2*NPRONY),JAC(SIZE_TIME,2*NPRONY),FXI(SIZE_TIME),
     .         DELTAY(SIZE_TIME),JACTJAC(2*NPRONY,2*NPRONY),ID(2*NPRONY,2*NPRONY),
     .         JACTDY(2*NPRONY),IPIV(2*NPRONY),JACT(2*NPRONY,SIZE_TIME),FXI_TR(SIZE_TIME),
     .         VECT(2*NPRONY),MU(2*NPRONY))
C
      ! Initialization of parameters value
      DO I = 1,NPRONY
        XI(2*I-1) = ONE/(I+ISHAPE)
        XI(2*I)   = I-1+ISHAPE
      ENDDO
C
      ! Identity matrix
      ID = ZERO
      DO I = 1,2*NPRONY
        ID(I,I) = ONE
      ENDDO
C        
      ! Initialization of the residues
      FXI(:) = GINFINI
      DO I = 1,SIZE_TIME
        DO II = 1,NPRONY
          FXI(I)        = FXI(I) + XI(2*II-1)*EXP(-XI(2*II)*TIME(I))
          JAC(I,2*II-1) = -EXP(-XI(2*II)*TIME(I))
          JAC(I,2*II)   = TIME(I)*XI(2*II-1)*EXP(-XI(2*II)*TIME(I))
        ENDDO
      ENDDO     
      DELTAY = YI - FXI
C
      ! Computation of the squared sum of residues
      RI2 = DOT_PRODUCT(DELTAY,DELTAY)
C
      ! Initialization of the damping parameter and the convergence criterion
      LAMBDA = MAXVAL(JAC)
      NU     = TWO
      CONV   = .FALSE.
      NITER  = 0
C
      !-------------------------------------------------------------------------------------
      ! Levenberg     Marquardt Algorithm + Line-search
      !-------------------------------------------------------------------------------------
#ifndef WITHOUT_LINALG
      DO WHILE ((.NOT.CONV).AND.(NITER<MXITER))
C
        ! a) Compute the Jacobian matrix products
        !  -> Transpose of Jacobian matrix
        JACT    = TRANSPOSE(JAC)
        !  -> JtJ product
        JACTJAC = MATMUL(JACT,JAC)
        !  -> Adding the damping parameter
        JACTJAC = JACTJAC + LAMBDA*ID
        !  -> Compute the gradient of residues
        JACTDY  = -MATMUL(JACT,DELTAY)
c
        ! b) Compute the trial values of XI
        CALL DGESV(2*NPRONY, 1, JACTJAC, 2*NPRONY, IPIV, JACTDY , 2*NPRONY, INFO)
        XI_TR = XI + JACTDY     
C
        ! Checking convergence
        IF (SQRT(DOT_PRODUCT(JACTDY,JACTDY)) <= EPS2*(SQRT(DOT_PRODUCT(XI,XI))+EPS2)) THEN
          CONV = .TRUE.
        ELSE
          ! c) Compute the trial values of the function
          FXI_TR(:) = GINFINI
          DO I = 1,SIZE_TIME
            DO II = 1,NPRONY
              FXI_TR(I) = FXI_TR(I) + XI_TR(2*II-1)*EXP(-XI_TR(2*II)*TIME(I))
            ENDDO
          ENDDO    
          ! d) Computation of the squared sum of residues
          RI2_OLD = RI2
          RI2     = DOT_PRODUCT(YI-FXI_TR,YI-FXI_TR)          
C
          ! Compute gain ration
          VECT  = MATMUL(JACT,DELTAY)
          VECT  = LAMBDA*JACTDY - VECT
          DENOM = DOT_PRODUCT(JACTDY,VECT)
          RHO   = (RI2_OLD - RI2) / MAX(DENOM,EM20)
C          
          ! e) Update parameters
          !  -> Step accepted
          IF ((RHO > ZERO).AND.(INFO == 0)) THEN
            ! Update damping parameter
            LAMBDA = LAMBDA*MAX(THIRD,ONE-(TWO*RHO-ONE)**3)
            NU     = TWO
            ! Update XI vector with line search to be sure that XI >= ZERO
            !  -> Compute the MU vector
            DO I = 1,2*NPRONY
              IF (JACTDY(I)>=ZERO) THEN
                MU(I) = ONE
              ELSE
                MU(I) = MIN(ONE,((ONE-ALPHA)/(-JACTDY(I)))*XI(I))
              ENDIF
              XI(I) = XI(I) + MU(I)*JACTDY(I)
            ENDDO  
            ! Update FXI vector
            FXI(:) = GINFINI
            DO I = 1,SIZE_TIME
              DO II = 1,NPRONY
                FXI(I) = FXI(I) + XI(2*II-1)*EXP(-XI(2*II)*TIME(I))
              ENDDO
            ENDDO  
            ! Update residue vector
            DELTAY = YI - FXI
            RI2    = DOT_PRODUCT(DELTAY,DELTAY)
            ! Update the Jacobian matrix
            JAC = ZERO
            DO I = 1,SIZE_TIME
              DO II = 1,NPRONY
                JAC(I,2*II-1) = -EXP(-XI(2*II)*TIME(I))
                JAC(I,2*II)   = TIME(I)*XI(2*II-1)*EXP(-XI(2*II)*TIME(I))
              ENDDO
            ENDDO    
            JACT = TRANSPOSE(JAC)
          !  -> Step not accepted
          ELSE
            ! Update damping parameter            
            LAMBDA = LAMBDA*NU 
            NU     = NU*TWO
          ENDIF          
C          
          ! f) Computing the convergence criterion
          JACTDY    = TWO*MATMUL(JACT,DELTAY)
          JACTDY_N2 = SQRT(DOT_PRODUCT(JACTDY,JACTDY))
          IF (JACTDY_N2 < EPS1) CONV = .TRUE.
c
          ! g) Increasing the iteration number
          NITER = NITER + 1
c
        ENDIF
C
      ENDDO
#else
      CONV = .FALSE.
      WRITE(6,*) "Error: Blas/Lapack required" 
#endif

C      
      ! Algorithm has not converged
      IF (NITER == MXITER .OR. .NOT.CONV) THEN
        CALL ANCMSG(MSGID=1926,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID)
      ! Solutions values must be checked
      ELSEIF (ABS(JACTDY_N2)>EPS1 .OR. MAXVAL(XI)>EP20 .OR. MINVAL(XI)<EM20) THEN
        CALL ANCMSG(MSGID=1927,MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID)   
      ENDIF
C
      ! Storage of the results in Prony parameters (accounting for normalizing values)
      G    = ZERO
      BETA = ZERO
      IF (ISHAPE == 1) THEN 
        GINFINI = GINFINI*MXVALY
        G(1)    = GINFINI
        BETA(1) = ZERO
      ENDIF
      DO I = 1,NPRONY
        G(I+ISHAPE)    = XI(2*I-1)*MXVALY
        BETA(I+ISHAPE) = XI(2*I)/MXVALT
      ENDDO
      COST  = RI2
      DERIV = JACTDY_N2
C
      ! Deallocation of tables
      DEALLOCATE(XI,XI_TR,JAC,FXI,DELTAY,JACTJAC,ID,JACTDY,IPIV,JACT,FXI_TR,VECT,YI,TIME,MU)      
      END

Chd|====================================================================
Chd|  LM_LEAST_SQUARE_PRONY_2       source/materials/visc/hm_read_visc_prony.F
Chd|-- called by -----------
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LM_LEAST_SQUARE_PRONY_2(MAT_ID   ,NPRONY   ,TABLE    ,FCT_IDS  ,XGS_SCALE,YGS_SCALE,
     .                                   FCT_IDL  ,XGL_SCALE,YGL_SCALE,G        ,BETA     ,COST     ,
     .                                   DERIV    ,ISHAPE   ,GINFINI  )  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   Dummy arguments
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPRONY,FCT_IDS,FCT_IDL,MAT_ID,ISHAPE
      my_real
     .        XGS_SCALE,YGS_SCALE,XGL_SCALE,YGL_SCALE,COST,DERIV,GINFINI
      my_real, DIMENSION(100) :: G,BETA
      TYPE(TTABLE) TABLE(NTABLE)     
C-----------------------------------------------
C   Local arguments
C-----------------------------------------------
      INTEGER I,II,INFO,SIZE_FREQ,NITER,ITABS,ITABL,FLAG,
     .        SIZE_FREQS,SIZE_FREQL,K
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPIV,IDX
      DOUBLE PRECISION ::
     .        RI2,RI2_OLD,LAMBDA,JACTDY_N2,
     .        RHO,NU,DENOM,XI_N2,MXFREQ,MXNORM
      DOUBLE PRECISION, PARAMETER :: EPS1   = 1.0D-8
      DOUBLE PRECISION, PARAMETER :: EPS2   = 1.0D-10
      DOUBLE PRECISION, PARAMETER :: ALPHA  = 0.5D0
      DOUBLE PRECISION, PARAMETER :: MXITER = 10000
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XI,FXIS,FXIL,DELTAYS,DELTAYL,
     .                                        JACTDY,YIS,YIL,FREQ,XI_TR,FXIS_TR,
     .                                        FXIL_TR,VECT,MU,FREQS,FREQL,YIS_TEMP,
     .                                        YIL_TEMP
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: JACS,JACL,JACTJAC,JACST,JACLT,ID,WEIGHT    
      LOGICAL :: CONV   
C
      ! Finding the functions for Storage and Loss moduli
      ITABS = 0
      ITABL = 0
      DO I = 1,NTABLE
        IF (TABLE(I)%NOTABLE == FCT_IDS) ITABS = I  
        IF (TABLE(I)%NOTABLE == FCT_IDL) ITABL = I  
        IF (ITABS /= 0 .AND. ITABL /= 0) EXIT
      ENDDO  
      IF (ITABS == 0) THEN
        CALL ANCMSG(MSGID=1924,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,I2=FCT_IDS)      
      ENDIF
      IF (ITABL == 0) THEN
        CALL ANCMSG(MSGID=1925,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,I2=FCT_IDL)      
      ENDIF
C
      ! --------------------------------------------------------------------------
      ! Recover the experimental data and unifying the abscissa vector if needed
      ! --------------------------------------------------------------------------
      !  -> Temporary data storage
      ! --------------------------------------------------------------------
      SIZE_FREQS = SIZE(TABLE(ITABS)%X(1)%VALUES)
      SIZE_FREQL = SIZE(TABLE(ITABL)%X(1)%VALUES)
      ALLOCATE(FREQS(SIZE_FREQS),YIS_TEMP(SIZE_FREQS),
     .         FREQL(SIZE_FREQL),YIL_TEMP(SIZE_FREQL))
      FREQS(1:SIZE_FREQS)    = TABLE(ITABS)%X(1)%VALUES(1:SIZE_FREQS)*XGS_SCALE
      YIS_TEMP(1:SIZE_FREQS) = TABLE(ITABS)%Y%VALUES(1:SIZE_FREQS)*YGS_SCALE
      FREQL(1:SIZE_FREQL)    = TABLE(ITABL)%X(1)%VALUES(1:SIZE_FREQL)*XGL_SCALE
      YIL_TEMP(1:SIZE_FREQL) = TABLE(ITABL)%Y%VALUES(1:SIZE_FREQL)*YGL_SCALE
C
      ! -> Number of abscissa is the same 
      ! --------------------------------------------------------------------
      FLAG = 0
      IF (SIZE_FREQS == SIZE_FREQL) THEN 
        ! If there is no difference at all between abscissa
        IF (SQRT(DOT_PRODUCT(FREQS-FREQL,FREQS-FREQL))<EM08) THEN
          FLAG = 0
          SIZE_FREQ = SIZE_FREQS
          ALLOCATE(FREQ(SIZE_FREQ),YIS(SIZE_FREQ),YIL(SIZE_FREQ))
          MXFREQ = MAXVAL(FREQS)
          DO II = 1, SIZE_FREQ
            FREQ(II) = FREQS(II)*(HUNDRED/MXFREQ)
            YIS(II)  = YIS_TEMP(II)
            YIL(II)  = YIL_TEMP(II)
          ENDDO
        ! If they have the same number but different value
        ELSE
          FLAG = 1
        ENDIF
      ENDIF
      ! -> Abscissa are not the same
      ! --------------------------------------------------------------------
      IF (SIZE_FREQ /= SIZE_FREQL .OR. FLAG == 1) THEN
        ! -> Counting the total number of abscissa without duplicate identical values
        !----------------------------------------------------------------------------
        SIZE_FREQ = SIZE_FREQS + SIZE_FREQL
        DO I = 1,SIZE_FREQL
          FLAG = 0
          DO II = 1,SIZE_FREQS
            IF (ABS(FREQS(II)-FREQL(I))<EM08) THEN 
              FLAG = 1
            ENDIF
          ENDDO
          IF (FLAG == 1) SIZE_FREQ = SIZE_FREQ - 1
        ENDDO
        ! -> Allocating and filling tables
        !----------------------------------------------------------------------------        
        ALLOCATE(IDX(SIZE_FREQ),FREQ(SIZE_FREQ),YIS(SIZE_FREQ),YIL(SIZE_FREQ))
        FREQ(1:SIZE_FREQS) = FREQS(1:SIZE_FREQS)
        K = 0
        DO I = 1,SIZE_FREQL
          FLAG = 0
          DO II = 1,SIZE_FREQS
            IF (ABS(FREQS(II)-FREQL(I))<EM08) THEN 
              FLAG = 1
              K = K + 1
            ENDIF
          ENDDO
          IF (FLAG == 0) FREQ(SIZE_FREQS+I-K) = FREQL(I)
        ENDDO        
        ! -> Sorting abscissa
        !--------------------
        DO II = 1,SIZE_FREQ
          IDX(II) = II
        ENDDO
        CALL QSORT(FREQ,IDX,1,SIZE_FREQ)
        MXFREQ = MAXVAL(FREQ)
        FREQ   = FREQ*(HUNDRED/MXFREQ)
        FREQS  = FREQS*(HUNDRED/MXFREQ)
        FREQL  = FREQL*(HUNDRED/MXFREQ)
        ! -> Data interpolation with new abscissa
        !----------------------------------------------------------------------------         
        II = 1
        DO I = 1,SIZE_FREQ
          IF (FREQ(I)<FREQS(1)) THEN 
            YIS(I) = YIS_TEMP(1) + ((YIS_TEMP(2)-YIS_TEMP(1))/(FREQS(2)-FREQS(1)))*(FREQ(I)-FREQS(1))
          ELSEIF (FREQ(I)>FREQS(SIZE_FREQS)) THEN
            YIS(I) = YIS_TEMP(SIZE_FREQS-1) + ((YIS_TEMP(SIZE_FREQS)-YIS_TEMP(SIZE_FREQS-1))/
     .                                         (FREQS(SIZE_FREQS)-FREQS(SIZE_FREQS-1)))*(FREQ(I)-FREQS(SIZE_FREQS-1))
          ELSE
            IF (FREQ(I)>FREQS(II+1)) THEN
              II = II + 1
              II = MIN(II,SIZE_FREQS-1)
            ENDIF
            YIS(I) = YIS_TEMP(II) + ((YIS_TEMP(II+1)-YIS_TEMP(II))/(FREQS(II+1)-FREQS(II)))*(FREQ(I)-FREQS(II))    
          ENDIF
        ENDDO
        II = 1
        DO I = 1,SIZE_FREQ
          IF (FREQ(I)<FREQL(1)) THEN 
            YIL(I) = YIL_TEMP(1) + ((YIL_TEMP(2)-YIL_TEMP(1))/(FREQL(2)-FREQL(1)))*(FREQ(I)-FREQL(1))
          ELSEIF (FREQ(I)>FREQL(SIZE_FREQL)) THEN
            YIL(I) = YIL_TEMP(SIZE_FREQL-1) + ((YIL_TEMP(SIZE_FREQL)-YIL_TEMP(SIZE_FREQL-1))/
     .                                         (FREQL(SIZE_FREQL)-FREQL(SIZE_FREQL-1)))*(FREQ(I)-FREQL(SIZE_FREQL-1))
          ELSE
            IF (FREQ(I)>FREQL(II+1)) THEN
              II = II + 1
              II = MIN(II,SIZE_FREQL-1)
            ENDIF
            YIL(I) = YIL_TEMP(II) + ((YIL_TEMP(II+1)-YIL_TEMP(II))/(FREQL(II+1)-FREQL(II)))*(FREQ(I)-FREQL(II))    
          ENDIF
        ENDDO
      ENDIF 
C
      ! Order of prony series to high for the number of data points
      IF (2*NPRONY >= SIZE_FREQ) THEN
        CALL ANCMSG(MSGID=1921,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,I2=NPRONY,
     .              I3=SIZE_FREQ,I4=INT(SIZE_FREQ/TWO))
      ENDIF
C      
      ! Allocation and initialization of the vectors
      ALLOCATE(XI(2*NPRONY),XI_TR(2*NPRONY),JACS(SIZE_FREQ,2*NPRONY),JACL(SIZE_FREQ,2*NPRONY),
     .         FXIS(SIZE_FREQ),FXIL(SIZE_FREQ),DELTAYS(SIZE_FREQ),DELTAYL(SIZE_FREQ),
     .         JACTJAC(2*NPRONY,2*NPRONY),ID(2*NPRONY,2*NPRONY),JACTDY(2*NPRONY),IPIV(2*NPRONY),
     .         JACST(2*NPRONY,SIZE_FREQ),JACLT(2*NPRONY,SIZE_FREQ),FXIS_TR(SIZE_FREQ),
     .         FXIL_TR(SIZE_FREQ),VECT(2*NPRONY),MU(2*NPRONY),WEIGHT(SIZE_FREQ,SIZE_FREQ))
C
      ! Initialization of parameters value
      DO I = 1,NPRONY
        XI(2*I-1) = YIS(1)/(I+ISHAPE)
        XI(2*I)   = I-1+ISHAPE
      ENDDO
C
      ! Identity matrix
      ID = ZERO
      DO I = 1,2*NPRONY
        ID(I,I) = ONE
      ENDDO
C
      ! Considering infinite value shape
      IF (ISHAPE == 1) THEN
        GINFINI = YIS(1)
      ELSE
        GINFINI = ZERO
      ENDIF
C
      ! Weight matrix
      WEIGHT = ZERO
      MXNORM = MAX(MAXVAL(YIS),MAXVAL(YIL))
      DO I = 1,SIZE_FREQ
        WEIGHT(I,I) = ONE/(MXNORM**2)
      ENDDO      
C        
      ! Initialization of the Prony series and the residues
      FXIS(:) = GINFINI
      FXIL(:) = ZERO
      DO I = 1,SIZE_FREQ
        DO II = 1,NPRONY
          ! Storage modulus series
          FXIS(I)        = FXIS(I) + ((XI(2*II-1)*(FREQ(I)**2))/(XI(2*II)**2 + FREQ(I)**2)) 
          JACS(I,2*II-1) = -(FREQ(I)**2)/(XI(2*II)**2 + FREQ(I)**2)
          JACS(I,2*II)   = TWO*XI(2*II-1)*XI(2*II)*(FREQ(I)**2)/((XI(2*II)**2 + FREQ(I)**2)**2)
          ! Loss modulus series
          FXIL(I)        = FXIL(I) + ((XI(2*II-1)*XI(2*II)*FREQ(I))/(XI(2*II)**2 + FREQ(I)**2)) 
          JACL(I,2*II-1) = -(FREQ(I)*XI(2*II))/(XI(2*II)**2 + FREQ(I)**2)
          JACL(I,2*II)   = XI(2*II-1)*FREQ(I)*((XI(2*II)**2 - FREQ(I)**2)/(XI(2*II)**2 + FREQ(I)**2)**2)
        ENDDO
      ENDDO     
      DELTAYS = YIS - FXIS
      DELTAYL = YIL - FXIL
C
      ! Computation of the squared sum of residues
      RI2 = ((ONE/MXNORM)**2)*(DOT_PRODUCT(DELTAYS,DELTAYS) + DOT_PRODUCT(DELTAYL,DELTAYL))
C
      ! Initialization of the damping parameter and the convergence criterion
      LAMBDA = MAXVAL(JACS) + MAXVAL(JACL)
      NU     = TWO
      CONV   = .FALSE.
      NITER  = 0
C
      !-------------------------------------------------------------------------------------
      ! Levenberg     Marquardt Algorithm + Line-search
      !-------------------------------------------------------------------------------------
#ifndef WITHOUT_LINALG
      DO WHILE ((.NOT.CONV).AND.(NITER<MXITER))
C
        ! a) Compute the Jacobian matrix products
        !  -> Transpose of Jacobian matrix
        JACST   = TRANSPOSE(JACS)
        JACLT   = TRANSPOSE(JACL)
        !  -> JtJ product
        JACTJAC = MATMUL(JACST,MATMUL(WEIGHT,JACS))
        JACTJAC = JACTJAC + MATMUL(JACLT,MATMUL(WEIGHT,JACL))
        !  -> Adding the damping parameter
        JACTJAC = JACTJAC + LAMBDA*ID
        !  -> Compute the gradient of residues 
        JACTDY  = -MATMUL(JACST,MATMUL(WEIGHT,DELTAYS))
        JACTDY  = JACTDY - MATMUL(JACLT,MATMUL(WEIGHT,DELTAYL))
c
        ! b) Compute the trial values of XI
        CALL DGESV(2*NPRONY, 1, JACTJAC, 2*NPRONY, IPIV, JACTDY , 2*NPRONY, INFO)
        XI_TR = XI + JACTDY  
C
        ! Checking convergence
        IF (SQRT(DOT_PRODUCT(JACTDY,JACTDY)) <= EPS2*(SQRT(DOT_PRODUCT(XI,XI))+EPS2)) THEN
          CONV = .TRUE.
        ELSE
          ! c) Compute the trial values of the function
          FXIS_TR(:) = GINFINI
          FXIL_TR(:) = ZERO
          DO I = 1,SIZE_FREQ
            DO II = 1,NPRONY
              FXIS_TR(I) = FXIS_TR(I) + ((XI_TR(2*II-1)*(FREQ(I)**2))/(XI_TR(2*II)**2 + FREQ(I)**2)) 
              FXIL_TR(I) = FXIL_TR(I) + ((XI_TR(2*II-1)*XI_TR(2*II)*FREQ(I))/(XI_TR(2*II)**2 + FREQ(I)**2)) 
            ENDDO
          ENDDO    
          ! d) Computation of the squared sum of residues
          RI2_OLD = RI2
          RI2     = ((ONE/MXNORM)**2)*(DOT_PRODUCT((YIS-FXIS_TR),(YIS-FXIS_TR)) + 
     .                                DOT_PRODUCT((YIL-FXIL_TR),(YIL-FXIL_TR)))
C
          ! Compute gain ratio
          VECT  = MATMUL(JACST,MATMUL(WEIGHT,DELTAYS)) + MATMUL(JACLT,MATMUL(WEIGHT,DELTAYL))
          VECT  = LAMBDA*JACTDY - VECT
          DENOM = DOT_PRODUCT(JACTDY,VECT)
          RHO   = (RI2_OLD - RI2) / MAX(DENOM,EM20)
C          
          ! e) Update parameters
          !  -> Step accepted
          IF ((RHO > ZERO).AND.(INFO == 0)) THEN
            ! Update damping parameter
            LAMBDA = LAMBDA*MAX(THIRD,ONE-(TWO*RHO-ONE)**3)
            NU  = TWO
            ! Update XI vector
            !  -> Compute the MU vector for line search
            DO I = 1,2*NPRONY
              IF (JACTDY(I)>=ZERO) THEN
                MU(I) = ONE
              ELSE
                MU(I) = MIN(ONE,((ONE-ALPHA)/(-JACTDY(I)))*XI(I))
              ENDIF
              XI(I) = XI(I) + MU(I)*JACTDY(I)
            ENDDO  
            ! Update FXI vector
            FXIS(:) = GINFINI
            FXIL(:) = ZERO
            DO I = 1,SIZE_FREQ
              DO II = 1,NPRONY
                FXIS(I) = FXIS(I) + ((XI(2*II-1)*(FREQ(I)**2))/(XI(2*II)**2 + FREQ(I)**2)) 
                FXIL(I) = FXIL(I) + ((XI(2*II-1)*XI(2*II)*FREQ(I))/(XI(2*II)**2 + FREQ(I)**2))
              ENDDO
            ENDDO  
            ! Update residue vector
            DELTAYS = YIS - FXIS
            DELTAYL = YIL - FXIL
            RI2     = ((ONE/MXNORM)**2)*(DOT_PRODUCT(DELTAYS,DELTAYS) + DOT_PRODUCT(DELTAYL,DELTAYL))
            ! Update the Jacobian matrix
            JACS = ZERO
            DO I = 1,SIZE_FREQ
              DO II = 1,NPRONY
                JACS(I,2*II-1) = -(FREQ(I)**2)/(XI(2*II)**2 + FREQ(I)**2)
                JACS(I,2*II)   = TWO*XI(2*II-1)*XI(2*II)*(FREQ(I)**2)/((XI(2*II)**2 + FREQ(I)**2)**2)
                JACL(I,2*II-1) = -(FREQ(I)*XI(2*II))/(XI(2*II)**2 + FREQ(I)**2)
                JACL(I,2*II)   = XI(2*II-1)*FREQ(I)*((XI(2*II)**2 - FREQ(I)**2)/(XI(2*II)**2 + FREQ(I)**2)**2)
              ENDDO
            ENDDO    
            JACST = TRANSPOSE(JACS)
            JACLT = TRANSPOSE(JACL)
          !  -> Step not accepted
          ELSE
            ! Update damping parameter            
            LAMBDA = LAMBDA*NU 
            NU     = NU*TWO
          ENDIF          
C          
          ! f) Computing the convergence criterion
          JACTDY    = TWO*MATMUL(JACST,MATMUL(WEIGHT,DELTAYS)) + TWO*MATMUL(JACLT,MATMUL(WEIGHT,DELTAYL))
          JACTDY_N2 = SQRT(DOT_PRODUCT(JACTDY,JACTDY))
          IF (JACTDY_N2 < EPS1) CONV = .TRUE.
c
          ! g) Increasing the iteration number
          NITER = NITER + 1
c
        ENDIF
C
      ENDDO
#else
      CONV = .FALSE.
      WRITE(6,*) "Error: Blas/Lapack required" 
#endif

C      
      ! Algorithm has not converged
      IF (NITER == MXITER .OR. .NOT.CONV) THEN
        CALL ANCMSG(MSGID=1926,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID)
      ! Solutions values must be checked
      ELSEIF (ABS(JACTDY_N2)>EPS1 .OR. MAXVAL(XI)>EP10 .OR. MINVAL(XI)<EM10) THEN
        CALL ANCMSG(MSGID=1927,MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID)   
      ENDIF
C
      ! Storage of the results in Prony parameters (accounting for normalizing values)
      G    = ZERO
      BETA = ZERO
      IF (ISHAPE == 1) THEN 
        G(1)    = GINFINI
        BETA(1) = ZERO
      ENDIF
      DO I = 1,NPRONY
        G(I+ISHAPE)    = XI(2*I-1)
        BETA(I+ISHAPE) = XI(2*I)*(MXFREQ/HUNDRED)
      ENDDO
      COST  = RI2
      DERIV = JACTDY_N2
C
      ! Deallocation of tables
      DEALLOCATE(XI,XI_TR,JACS,JACL,FXIS,FXIL,DELTAYS,DELTAYL,JACTJAC,ID,JACTDY,IPIV,
     .           JACST,JACLT,FXIS_TR,FXIL_TR,VECT,MU,FREQS,YIS_TEMP,FREQL,YIL_TEMP)    
      IF (ALLOCATED(IDX)) DEALLOCATE(IDX)
      END


C---------------------------------------------------------------------------------------      
      RECURSIVE SUBROUTINE QSORT(A, IDX, FIRST, LAST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN)    :: FIRST, LAST
      INTEGER, INTENT(INOUT) :: IDX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION ::
     .           X, T
      INTEGER :: I, J, I1, I2
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(FIRST>LAST)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      X = A( (FIRST + LAST) / 2 )
      I = FIRST
      J = LAST
      DO
         DO WHILE (A(I) < X)
            I = I + 1
         ENDDO
         DO WHILE(X < A(J))
            J = J - 1
         ENDDO
         IF (I >= J) EXIT
         T = A(I)
         A(I) = A(J)
         A(J) = T
         I1 = IDX(i) 
         IDX(I) = IDX(J) 
         IDX(J) = I1
         I = I + 1
         J = J - 1
      ENDDO
      IF (FIRST < I - 1) CALL QSORT(A, IDX, FIRST, I - 1)
      IF (J + 1 < LAST)  CALL QSORT(A, IDX, J + 1, LAST)
      END SUBROUTINE 
