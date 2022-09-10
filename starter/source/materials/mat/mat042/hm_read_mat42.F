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
Chd|  HM_READ_MAT42                 source/materials/mat/mat042/hm_read_mat42.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT42(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,IMATVIS  ,UNITAB   ,
     .           ID       ,TITR     ,LSUBMODEL,PM       ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW42 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
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
      INTEGER, INTENT(IN)    :: ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM     
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      INTEGER, INTENT(OUT)   :: IMATVIS,NUPARAM,NUVAR,NFUNC
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(OUT) :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
      INTEGER :: I,J,K,IFORM,IFLAG,IFUNC_BLK,NPRONY,IVISC,ILAW
      my_real :: RHO0,RHOR,BULK,SMAX,GS,MU,NU,GVMAX,C1,ZEP495,FSCAL,FSCAL_UNIT,
     .           AL1,AL2,AL3,AL4,AL5,AL6,AL7,AL8,AL9,AL10,
     .           MU1,MU2,MU3,MU4,MU5,MU6,MU7,MU8,MU9,MU10       
      my_real, DIMENSION(100) ::  GI,TAUX
C=======================================================================
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      NPRONY  = 0
      IFORM   = 0
      IFLAG   = 0
      IVISC   = 0  
      IMATVIS = 1
      ILAW    = 42
      ZEP495  = ZEP4 + NINE*EM02 + FIVE*EM03

      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGCUT',SMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('FUN_BULK'    ,IFUNC_BLK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_FScale',FSCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('ORDER'       ,NPRONY   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IFORM'       ,IFORM    ,IS_AVAILABLE,LSUBMODEL)      

      CALL HM_GET_FLOATV('MAT_MUE1',MU1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE2',MU2 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE3',MU3 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE4',MU4 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE5',MU5 ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      
      CALL HM_GET_FLOATV('MAT_ALPHA11',AL1,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA22',AL2,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA33',AL3,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA44',AL4,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA55',AL5,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      IF (NPRONY > 0) THEN
        DO J=1,NPRONY    
          CALL HM_GET_FLOAT_ARRAY_INDEX('Gamma_arr',GI(J)  ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
        DO J=1,NPRONY     
          CALL HM_GET_FLOAT_ARRAY_INDEX('Tau_arr'  ,TAUX(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
      ENDIF

c--------------------------------------------------
      IF (RHOR == ZERO)  RHOR  = RHO0
      IF (SMAX <= ZERO)  SMAX  = EP20
c
      CALL HM_GET_FLOATV_DIM('MAT_FScale',FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (FSCAL == ZERO) FSCAL = ONE*FSCAL_UNIT
c
      GVMAX = ZERO
      DO K=1,NPRONY
        GVMAX = GVMAX + GI(K)
      ENDDO      
      GS = MU1*AL1+MU2*AL2+MU3*AL3+MU4*AL4+MU5*AL5
      IF (GS <= ZERO) THEN
        CALL ANCMSG(MSGID=828,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR)
      END IF
      IF (NU == ZERO ) NU = ZEP495
      MU   = GS/TWO
      BULK = GS*(ONE+NU)/MAX(EM20,THREE*(ONE-TWO*NU))
      IF (NPRONY > 0 ) THEN
        IVISC   = 1
        IMATVIS = 3
      ENDIF
c---------------------
      UPARAM(1)  = MU1
      UPARAM(2)  = MU2
      UPARAM(3)  = MU3
      UPARAM(4)  = MU4
      UPARAM(5)  = MU5
      UPARAM(6)  = AL1
      UPARAM(7)  = AL2
      UPARAM(8)  = AL3
      UPARAM(9)  = AL4
      UPARAM(10) = AL5
      UPARAM(11) = BULK
      UPARAM(12) = SMAX
      UPARAM(13) = IFLAG
      UPARAM(14) = NU
      UPARAM(15) = FSCAL
      UPARAM(16) = NPRONY
      UPARAM(17) = IFORM  
c---------------------
C     Iform is only for solid to active old formulation 
C     the flag ivisc for shell is activated in engine
C     IVISC =0 (hyperelastic - New formulation  corresponding to iform 0
C     IVISC =1 (Hyperelastic + viscosity - Using Old iterative formulation) corresponding to iform=1
C
c-----------------
      NFUNC   = 2
      NUVAR   = 12
      NUPARAM = 17
c-----------------
      IF (NPRONY > 0) THEN
        DO K=1,NPRONY
          UPARAM(17 + K) = GI(K) 
          UPARAM(17 + NPRONY + K) = TAUX(K)
        ENDDO       
        NUVAR   = NUVAR   + 6*NPRONY 
        NUPARAM = NUPARAM + 2*NPRONY
      ENDIF
C
      IFUNC(1) = IFUNC_BLK
      IFUNC(2) = 0
      PARMAT(1)= GS
      PARMAT(2)= GS*(ONE+NU)
      PARMAT(3)= NU
      PARMAT(6)= BULK
C     Formulation for solid elements time step computation.
      C1 = THIRD*GS*(ONE + NU)/(ONE - TWO*NU)
      PARMAT(16) = 2
      PARMAT(17) = GS/(C1 + TWO_THIRD*GS)
c-----------------
      PM(1)  = RHOR
      PM(2)  = GS
      PM(89) = RHO0
      PM(100)= BULK      
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
c-----------------
      WRITE(IOUT,1100) TRIM(TITR),ID,42
      WRITE(IOUT,1000)
      IF (IS_CRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) NU,SMAX,IFUNC_BLK,FSCAL,MU1,MU2,MU3,MU4,MU5,
     .                   AL1,AL2,AL3,AL4,AL5,MU,BULK
        WRITE(IOUT,1400) NPRONY
        DO K =1, NPRONY
          WRITE(IOUT,1500) GI(K),TAUX(K) 
        ENDDO 
      ENDIF     
C-----------------
      RETURN
 999  CALL FREERR(3)
C-----------------
 1000 FORMAT
     & (5X,'MATERIAL MODEL : GREEN-ELASTIC (OGDEN)    ',/,
     &  5X,'------------------------------------------',/)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,   'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,   'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     &  5X,  'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT
     & (5X,  'POISSON RATIO. . . . . . . . . .  . . .=',1PG20.13/ 
     & ,5X,  'CUT-OFF STRESS IN TENSION . . . . . . .=',1PG20.13/
     & ,5X,  'BULK FUNCTION NUMBER. . . . . . . . . .=',I10/
     & ,5X,  'SCALE FACTOR FOR BULK FUNCTION. . . . .=',1PG20.13/ 
     & ,5X,  'MU1 . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & ,5X,  'MU2 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU3 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU4 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU5 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL2 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL3 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL4 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL5 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'INITIAL SHEAR MODULUS . . . . . . . . .=',1PG20.13/
     & ,5X,  'BULK MODULUS. . . . . . . . . . . . . .=',1PG20.13//) 
 1400 FORMAT(//
     & 5X,  'NUMBER OF TERMS IN PRONY SERIES . . . . =',I8 )
 1500 FORMAT(//
     & 5X,'SHEAR STIFFNESS. . . . . . . . . . .. . . =',E12.4/
     & 5X,'RELAXATION TIME . . . . . . . . . . .  . .=',E12.4)  
C-----------------
      RETURN
      END
