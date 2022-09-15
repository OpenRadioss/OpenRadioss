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
Chd|  HM_READ_MAT60                 source/materials/mat/mat060/hm_read_mat60.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT60(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW60 WITH HM READER 
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .    E,NU,G,C1,EPSMAX,EPSR1,EPSR2,FISOKIN,RATE(11),YFAC(11),
     .    PSCALE,FAC_L,FAC_T,FAC_M,FAC_C,ESCALE,EINF,CE,ASRATE
      INTEGER NRATE,J,I,IU,IFUNCE,ISRATE_LOC,MFUNC,OPTE,IPFUN
      my_real :: RHO0, RHOR, PSCALE_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 1
      IMATVIS = 0
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1

      !---READING
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'      ,E           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'    ,EPSMAX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST1'  ,EPSR1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2'  ,EPSR2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3    
      CALL HM_GET_INTV('NFUNC'        ,NRATE       ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Fsmooth'      ,ISRATE_LOC  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_HARD'   ,FISOKIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fcut'       ,ASRATE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4    
      CALL HM_GET_INTV('Xr_fun'       ,IPFUN       ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('fct_ID_k'     ,IFUNCE      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_FScale' ,PSCALE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('E_R'        ,EINF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C1'     ,CE          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !remaining lines
      CALL HM_GET_INTV('FUN_A1'       ,IFUNC(1)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1'       ,IFUNC(2)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A2'       ,IFUNC(3)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_B2'       ,IFUNC(4)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A3'       ,IFUNC(5)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_B3'       ,IFUNC(6)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A4'       ,IFUNC(7)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_B4'       ,IFUNC(8)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A5'       ,IFUNC(9)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_B5'       ,IFUNC(10)    ,IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_FLOATV('MAT_ALPHA1' ,YFAC(1)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA2' ,YFAC(2)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA3' ,YFAC(3)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA4' ,YFAC(4)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA5' ,YFAC(5)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA6' ,YFAC(6)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA7' ,YFAC(7)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA8' ,YFAC(8)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA9' ,YFAC(9)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('MAT_ALPHA0' ,YFAC(10)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)   

      CALL HM_GET_FLOATV('MAT_EPSR1'  ,RATE(1)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR2'  ,RATE(2)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR3'  ,RATE(3)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR4'  ,RATE(4)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR5'  ,RATE(5)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR6'  ,RATE(6)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR7'  ,RATE(7)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR8'  ,RATE(8)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR9'  ,RATE(9)      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('MAT_EPSR10' ,RATE(10)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      !units
      CALL HM_GET_FLOATV_DIM('MAT_FScale' ,PSCALE_UNIT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)   

C=======================================================================


      !---CHECK PARAMETERS
      IF((EPSR1 /= ZERO).AND.(EPSR2 /= ZERO))THEN  
       IF(EPSR1 >= EPSR2)THEN                  
        CALL ANCMSG(MSGID=480,                 
     .              MSGTYPE=MSGERROR,          
     .              ANMODE=ANINFO_BLIND_1,     
     .              I1=MAT_ID,                     
     .              C1=TITR)                   
       ENDIF                                   
      ENDIF                                    
                                               
      IF(NRATE > 10)THEN                       
        CALL ANCMSG(MSGID=215,                 
     .              MSGTYPE=MSGERROR,          
     .              ANMODE=ANINFO,             
     .              I1=60,                     
     .              I2=MAT_ID,                     
     .              C1=TITR)                   
      ELSEIF(NRATE < 4)THEN                    
         CALL ANCMSG(MSGID=529,                
     .               MSGTYPE=MSGERROR,         
     .               ANMODE=ANINFO,            
     .               I1=MAT_ID,                    
     .               C1=TITR,                  
     .               I2=NRATE)                 
      ENDIF                                    

      IF (IPFUN == 0) THEN
        PSCALE = ZERO
      ELSEIF (PSCALE == ZERO) THEN
        PSCALE = ONE*PSCALE_UNIT
      ELSE
        PSCALE = ONE/PSCALE
      ENDIF
      
      IF(NRATE == 5)THEN  !CFG file is forcing NRATE=5 whenever there is less than 5 functions
        DO I=5,2,-1
          IF(IFUNC(I) == 0)THEN
            NRATE=NRATE-1
          ELSE
            EXIT
          ENDIF
        ENDDO
      ENDIF
      
      DO I=1,NRATE-1
	 IF(RATE(I) >= RATE(I+1))THEN
          CALL ANCMSG(MSGID=478,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
          EXIT
         ENDIF
      ENDDO
 
      DO I=1,NRATE
        IF(YFAC(I) == ZERO) YFAC(I)=ONE*PSCALE_UNIT
      ENDDO
      
      DO I=1,NRATE
        IF(IFUNC(I) == 0)THEN
          CALL ANCMSG(MSGID=126,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
        ENDIF
      ENDDO
  
      IF(NRATE > 1 .AND. ISRATE_LOC == 0) THEN  
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF
      
      IF(NRATE == 1)THEN
        MFUNC=2
        IFUNC(2)=IFUNC(1)
        RATE(1) =ZERO
        RATE(2) =ONE
        YFAC(2) =YFAC(1)
      ELSEIF(RATE(1) == ZERO)THEN
        MFUNC=NRATE
      ELSE
        MFUNC=NRATE+1
        DO J=NRATE,1,-1
          IFUNC(J+1)=IFUNC(J)
          RATE(J+1) =RATE(J)
          YFAC(J+1) =YFAC(J)
        ENDDO
        RATE(1)=ZERO
      ENDIF
C
      !---DEFAULTS VALUES
      IF(RHOR == ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
      
      !---STORAGE
      UPARAM(1)=MFUNC
      UPARAM(2)=E
      UPARAM(3)=E/(ONE-NU*NU)
      UPARAM(4)=NU*UPARAM(3)
      G = HALF*E/(1.+NU)
      UPARAM(5)=G
      UPARAM(6)=NU
      
      DO J=1,MFUNC
        UPARAM(6+J)=RATE(J)
      ENDDO
      
      DO J=1,MFUNC
        UPARAM(6+MFUNC+J)=YFAC(J)
      ENDDO
      
      IF(EPSR1 == ZERO)EPSR1=INFINITY
      IF(EPSR2 == ZERO)EPSR2=TWO*INFINITY
      UPARAM(6+2*MFUNC+1)=EPSMAX
      UPARAM(6+2*MFUNC+2)=EPSR1
      UPARAM(6+2*MFUNC+3)=EPSR2
      UPARAM(6+2*MFUNC+4)=TWO*G
      UPARAM(6+2*MFUNC+5)=THREE*G
      C1=E/THREE/(ONE-TWO*NU)
      UPARAM(6+2*MFUNC+6)=C1
      UPARAM(6+2*MFUNC+7)=C1 + FOUR*G/THREE ! ssp
      UPARAM(6+2*MFUNC+8)=FISOKIN
      NFUNC = MFUNC + 1
      IFUNC(NFUNC) = IPFUN
      IU = 15 + 2*MFUNC
      IF (IPFUN == 0) THEN
        UPARAM(IU) = 0
      ELSE
        UPARAM(IU) = NFUNC
      ENDIF
      IU = IU+1
      UPARAM(IU) = PSCALE
C
c --------------
      OPTE = 0
      IF (IFUNCE > 0 )OPTE = 1
      NFUNC = NFUNC + 1 
      IFUNC(NFUNC)= IFUNCE
      UPARAM(IU+1) = NFUNC 
      UPARAM(IU+2) = OPTE 
      UPARAM(IU+3) = EINF 
      UPARAM(IU+4) = CE 
      IU=IU+4
c
      NUPARAM = IU
c------------------

      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE_LOC
      IF(ASRATE == ZERO)ASRATE=INFINITY
      PARMAT(5) = ASRATE
c------------------------------------
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(C1+FOUR_OVER_3*G) ! == (1-2*nu)/(1-nu)
C
      NUVAR = 5 + MFUNC 
C
       !---LISTING OUTPUT---!      
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,60
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100) E,NU,EPSMAX,EPSR1,EPSR2,FISOKIN,ISRATE_LOC,ASRATE
        WRITE(IOUT,1200)(IFUNC(J),YFAC(J),RATE(J),J=1,MFUNC)
        WRITE(IOUT,1300) IPFUN,PSCALE, IFUNCE,EINF,CE
        WRITE(IOUT,*)' '
      ENDIF
      

      RETURN
 1000 FORMAT(
     & 5X,40H  TABULATED ELASTIC PLASTIC LAW         ,/,
     & 5X,40H  -----------------------------         ,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO. . . . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM PLASTIC STRAIN . . . . . . . ..=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 1 . . . . . . ..=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 2 . . . . . . ..=',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENNING FACTOR. . . ..=',1PG20.13/
     & 5X,'SMOOTH STRAIN RATE OPTION. . . . . . ..=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13/)
 1200 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER. . . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13)
 1300 FORMAT(
     & 5X,'PRESSURE DEPENDENT YIELD FUNCTION . . .=',I10/
     & 5X,'PRESSURE SCALE FACTOR. . . . . . . . . =',1PG20.13/
     & 5X,'YOUNG MODULUS SCALE FACTOR FUNCTION . . .=',I10/
     & 5X,'YOUNG MODULUS EINF . . . . . . . . . . . =',1PG20.13/
     & 5X,'PARAMETER CE . . . . . . . . . . . . . . =',1PG20.13)
      END
