Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_MAT59                 source/materials/mat/mat059/hm_read_mat59.F
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
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT59(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,ID       ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW151 WITH HM READER ( TO BE COMPLETED )
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
      INTEGER, INTENT(INOUT)          :: ISRATE,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(NSUBMOD)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT  ! Number of declared materials
      INTEGER :: I,J
      INTEGER :: RHOFLAG,ICOMP,NRATE,IFILTR,IFUNN(MAXFUNC),IFUNT(MAXFUNC)
      my_real :: RHO0, RHOR,E,G,ECOMP,FCUT,RATE0,YFAC(MAXFUNC)
      my_real :: RATE(MAXFUNC),NU,FCUT_UNIT,YFAC_UNIT(MAXFUNC),RATE_UNIT(MAXFUNC)
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E'    ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G0'   ,G        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IMASS',RHOFLAG  ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('COMP_OPT' ,ICOMP    ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('Ecomp'    ,ECOMP    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_INTV  ('NFUNC'    ,NRATE    ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('Fsmooth'  ,IFILTR   ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('Fcut'     ,FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('Fcut' ,FCUT_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      DO I=1,NRATE 
        CALL HM_GET_INT_ARRAY_INDEX  ('ABG_IPt'  ,IFUNN(I)      ,I ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INT_ARRAY_INDEX  ('ABG_IPdel',IFUNT(I)      ,I ,IS_AVAILABLE, LSUBMODEL)        
        CALL HM_GET_FLOAT_ARRAY_INDEX('Fp1'      ,RATE(I)       ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fp1'  ,RATE_UNIT(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
        CALL HM_GET_FLOAT_ARRAY_INDEX('Fp2'      ,YFAC(I)       ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Fp2'  ,YFAC_UNIT(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      ENDDO                                                                              

      IF (ICOMP /= 1 .OR. ECOMP <= 0) THEN
        ECOMP = E
      ENDIF      
      IF (ICOMP /= 1) ICOMP = 0
      
      IF (FCUT == ZERO) FCUT = ONE * FCUT_UNIT
      
      NFUNC = NRATE*2          
      DO I=1,NRATE             
        J = (I-1)*2            
        IFUNC(J+1) = IFUNN(I)  
        IFUNC(J+2) = IFUNT(I)  
      ENDDO                    
C---
      DO I=1,NFUNC
        IF (IFUNC(I) == 0) THEN
           CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IFUNC(I))
        ENDIF
      ENDDO
      IF(NRATE > 1 .AND. IFILTR == 0) THEN  
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
      ENDIF      
      DO I=1,NRATE     
        IF (YFAC(I) == ZERO) YFAC(I) = ONE * YFAC_UNIT(I)
      ENDDO
      RATE0 = RATE(1)
      DO I=2,NRATE     
        IF (RATE(I) < RATE0) THEN
c         error
        ENDIF
        RATE0 = RATE(I)
      ENDDO
C----------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
C----------------
      UPARAM(1)=E
      UPARAM(2)=G
      UPARAM(3)=NRATE
      UPARAM(4)=IFILTR
      UPARAM(5)=FCUT
      UPARAM(6)=ECOMP*ICOMP
      UPARAM(7)=RHOFLAG
      DO I=1,NRATE
        UPARAM(7+I)=YFAC(I)
        UPARAM(7+NRATE+I)=RATE(I)
      ENDDO
C----------------
      NUPARAM = 7 + NRATE*2
      NUVAR   = 0
C----------------
      PARMAT(1) = E/THREE
      PARMAT(2) = MAX(E,G)
      PARMAT(5) = FCUT
C----------------
C     Formulation for solid elements time step computation.
!!      G= HALF*E/(ONE + NU)
!!      RBULK = THIRD*E/(ONE - TWO*NU)
      NU = HALF*E/G  - ONE
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU)
C-----------
      ISRATE = 1
      MTAG%G_GAMA=9
      MTAG%G_EPSD=1
      MTAG%G_EINS=2
      MTAG%L_EPE =3
      MTAG%L_PLA =2
C-----------------------  
      ! MATPARAM keyword
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c
      ! Properties compatibility       
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_COHESIVE") 
C-----------------------  
      WRITE(IOUT,1010) TRIM(TITR),ID,59  
      WRITE(IOUT,1000)          
      IF (IS_ENCRYPTED)THEN                                     
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE        
        WRITE(IOUT,1100)RHO0                                            
        WRITE(IOUT,1002) E,ECOMP,G,ICOMP,RHOFLAG,NRATE      
        IF (NRATE > 0) THEN                                                   
          WRITE(IOUT,1001)IFILTR,FCUT
          WRITE(IOUT,1200)(RATE(J),IFUNN(J),IFUNT(J),YFAC(J),J=1,NRATE)
        ENDIF                                                  
      ENDIF
C-----------        
      RETURN
C-----------
 1000 FORMAT(
     & 5X,40H  CONNECTION MATERIAL LAW 59            ,/,
     & 5X,40H  --------------------------            ,//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(     
     & 5X,'YOUNG MODULUS PER THICKNESS UNIT IN TENSION     =',1PG20.13/,
     & 5X,'YOUNG MODULUS PER THICKNESS UNIT IN COMPRESSION =',1PG20.13/,
     & 5X,'SHEAR MODULUS PER THICKNESS UNIT . . . .        =',1PG20.13/,
     & 5X,'LINEAR BEHAVIOUR IN COMPRESSION (FLAG) . . . . .=',I10/,
     & 5X,'DENSITY UNIT FLAG . . . . . . . . . . . . . .   =',I10/,
     & 5X,'             = 0 => VOLUME  DENSITY              '/,
     & 5X,'             = 1 => SURFACE DENSITY              '/,
     & 5X,'NUMBER OF YIELD(STRAIN RATE) FUNCTIONS .        =',I10/)
 1001 FORMAT(
     & 5X,'STRAIN RATE FILTERING FLAG . . . . . . . . .    =',I10/,
     & 5X,'CUT FREQ FOR STRAIN RATE FILTERING . . . . .    =',1PG20.13/)
 1100 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . =',1PG20.13/)  
 1200 FORMAT(
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .         =',1PG20.13/,
     &10X,'YIELD STRESS FUNCTION IN NORMAL DIRECTION  .    =',I10/,
     &10X,'YIELD STRESS FUNCTION IN TANGENT DIRECTION .    =',I10/,
     &10X,'SCALE FACTOR. . . . . . . . . . . . . .         =',1PG20.13)
C--------

      END SUBROUTINE HM_READ_MAT59
