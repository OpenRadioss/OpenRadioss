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
Chd|  HM_READ_MAT84                 source/materials/mat/mat084/hm_read_mat84.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT84(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         ITABLE   ,MAXTABL  ,NTABLE,PM     ,IPM      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW84
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
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
     .                                         ,NTABLE,ITABLE(MAXTABL)
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID,MAXTABL
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILAW
      my_real
     .        YOUNG,YIELD,P12,P22,P33,QVOCE,BVOCE,NU,
     .        G12,G22,G33,K0,ALPHA,AN,EPS0,NN,CEPSP,DEPS0,
     .        ETA,CP,TINI,TREF,TMELT,MTEMP,DEPSAD,
     .        AFRAC,BFRAC,GAMA,CFRAC,NFRAC
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
!      
      MTAG%G_TEMP = 1
      MTAG%L_TEMP = 1
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
!      
      ISRATE=0
      IMATVIS=0
      ILAW=84 
C 
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'      ,YOUNG         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU            ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('Fcut'         ,P12         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CAP_END'  ,P22         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'       ,P33         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PR'       ,QVOCE       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_T0'       ,BVOCE       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MAT_c2_t'    ,G12          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A2'      ,G22          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_c1_c'    ,G33          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NUt'     ,K0           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_VOL'     ,ALPHA        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('FScale11'    ,AN           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'    ,EPS0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33'    ,NN           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale12'    ,CEPSP        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale23'    ,DEPS0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-6
      CALL HM_GET_FLOATV('scale1'      ,ETA          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale2'      ,CP           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale3'      ,TINI         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale4'      ,TREF         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale5'      ,TMELT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-7
      CALL HM_GET_FLOATV('FScale11_2'    ,MTEMP          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22_2'    ,DEPSAD         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !unit
      !========== DEFAULT VALUES=============!              
      RHOR=RHO0
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
!  
      AFRAC=ZERO
      NFRAC=ZERO
      BFRAC=ZERO
      CFRAC=ZERO
      GAMA=ZERO

C       
      IF(P12 == ZERO )P12 = -HALF
      IF(G12 == ZERO )G12 = P12
C
      IF(P22 == ZERO )P22 = ONE
      IF(G22 == ZERO )G22 = P22
C
      IF(P33 == ZERO )P33 = THREE
      IF(G33 == ZERO )G33 = P33
C           
      IF (DEPS0 == ZERO) DEPS0 = INFINITY ! no strain rate effect
c
       UPARAM(1) = YOUNG
       UPARAM(2) = NU

       UPARAM(3) = P12
       UPARAM(4) = P22
       UPARAM(5) = P33
       UPARAM(6) = G12
       UPARAM(7) = G22
       UPARAM(8) = G33

       UPARAM(9) = QVOCE
       UPARAM(10)= BVOCE
       UPARAM(11)= K0
       UPARAM(12)= ALPHA

       UPARAM(13)= AN
       UPARAM(14)= EPS0
       UPARAM(15)= NN
       UPARAM(16)= CEPSP
       UPARAM(17)= DEPS0

       UPARAM(18)= ETA
       UPARAM(19)= CP
       UPARAM(20)= TINI
       UPARAM(21)= TREF
       UPARAM(22)= TMELT
       UPARAM(23)= MTEMP
       UPARAM(24)= DEPSAD

       UPARAM(25) =AFRAC    
       UPARAM(26) =BFRAC   
       UPARAM(27) =GAMA     
       UPARAM(28) =CFRAC   
       UPARAM(29) =NFRAC    

C
       NUVAR   = 9
       NUPARAM = 29 
       PARMAT(1) = YOUNG/THREE/(ONE - TWO*NU)
       PARMAT(2) = YOUNG
       PARMAT(3) = NU
C      Formulation for solid elements time step computation.
       PARMAT(16) = 2
       PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
C------------------------------
       WRITE(IOUT,1001) TRIM(TITR),MAT_ID,ILAW
       WRITE(IOUT,1000)           
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
       WRITE(IOUT,1002)RHO0  
       WRITE(IOUT,1100) YOUNG,NU,P12,P22,P33,G12,G22,G33
       WRITE(IOUT,1200) QVOCE,BVOCE,K0,ALPHA
       WRITE(IOUT,1300) AN,NN,EPS0,CEPSP,DEPS0
       WRITE(IOUT,1400) ETA,CP,TINI,TREF,TMELT,MTEMP,DEPSAD
      ENDIF 
C
c-----------
 1000 FORMAT
     & (5X,20H  ------------------    ,/,
     &  5X,20H  ------------------     ,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/) 
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . .  . . . . =',E12.4/
     & 5X,'POISSON''S RATIO . . . . . . . . . . . . . . =',E12.4/
     & 5X,'YIELD PARAMETER P12 . . . . . . . .  . . . . =',E12.4/
     & 5X,'YIELD PARAMETER P22. . . . . . . . . . . . . =',E12.4/
     & 5X,'YIELD PARAMETER P33. . . . . . . . . . . . . =',E12.4/
     & 5X,'FLOW RULE PARAMETER G12 . . . . . . . . . . .=',E12.4/
     & 5X,'FLOW RULE PARAMETER G22. . . . . . . . . . . =',E12.4/
     & 5X,'FLOW RULE PARAMETER G33. . . . . . . . . . . =',E12.4)
 1200 FORMAT(
     & 5X,'YIELD VOCE PARAMETER Q. . . . . . . . . . . .=',E12.4/
     & 5X,'YIELD VOCE PARAMETER B . . . . . . . . . . . =',E12.4/
     & 5X,'YIELD VOCE PARAMETER K0. . . . . . . . . . . =',E12.4/
     & 5X,'YIELD GLOBAL PARAMETER ALPHA . . . . . . . . =',E12.4)    
 1300 FORMAT(//
     & 5X,'YIELD SWIFT PARAMETER A. . . . . . . . . . . =',E12.4/
     & 5X,'YIELD SWIFT EXPONENT N . . . . . . . . . . . =',E12.4/
     & 5X,'YIELD SWIFT REFERENCE STRAIN . . . . . . . . =',E12.4/
     & 5X,'STRAIN RATE COEFFIICIENT C . . . . . . . . . =',E12.4/
     & 5X,'REFERENCE STRAIN RATE . .. . . . . . . . . . =',E12.4)
 1400 FORMAT(//
     & 5X,'TAYLOR-QUNINNEY COEFFICIENT . . . . . . . . .=',E12.4/
     & 5X,'SPECIFIC HEAT. . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'INITIAL TEMPERATURE. . . . . . . . . . . . . =',E12.4/
     & 5X,'REFERENCE TEMPERATURE . . . . . . . . . . . .=',E12.4/
     & 5X,'MELTING TEMPERATURE . . . . . . . . . . . . .=',E12.4/
     & 5X,'TEMPERATURE EXPONENT. . . . . . . . . . . . .=',E12.4/
     & 5X,'OPTIMIZATION STRAIN RATE COEFFICIENT . . . . =',E12.4)     
c-----------
      RETURN
      END
C      
