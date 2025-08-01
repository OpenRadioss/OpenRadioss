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
!||    hm_read_mat69            ../starter/source/materials/mat/mat069/hm_read_mat69.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT69(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT, 
     .                         UNITAB ,ID       ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW59 WITH HM READER ( TO BE COMPLETED )
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
      INTEGER, INTENT(INOUT)                :: ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: ID
      INTEGER,INTENT(INOUT)                    :: IMATVIS
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,BULK_FNCT,NTABLE_,NMUAL,LAWID,ILAW
      my_real MU(5),AL(5),RHO0,RHOR
      my_real BULK,TENSCUT,GS,NU,FSCAL,ZEP495,FSCAL_UNIT
      INTEGER ICHECK, NSTART
      my_real ERRTOL
      INTEGER IDUMMY
      my_real RDUMMY
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IDUMMY = 0
      NSTART = 0
      ERRTOL = ZERO
      ILAW   = 69
      IMATVIS = 1

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_INTV('MAT_Iflag'    ,LAWID    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A1'       ,BULK_FNCT      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_FScale' ,FSCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_INTV('NIP'          ,NMUAL    ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV('Gflag'        ,ICHECK   ,IS_AVAILABLE, LSUBMODEL) 
      !-line3
      CALL HM_GET_INTV  ('FUN_B1'     ,NTABLE_  ,IS_AVAILABLE, LSUBMODEL) 

      !units
      CALL HM_GET_FLOATV_DIM('MAT_FScale' ,FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
                        
C-----------------------------------------------
      NUPARAM = 21
      NFUNC   = 2
      NUVAR   = 9
C-----------------------------------------------
      ZEP495 = ZEP4 + NINE*EM02 + FIVE*EM03
      IF(ICHECK == 0    ) ICHECK = -3
      IF(NSTART == 0    ) NSTART = 100
      IF(ERRTOL == ZERO ) ERRTOL = FIVEEM3
      IF(NMUAL  == 0    ) NMUAL  = 2
      IF(FSCAL  == ZERO ) FSCAL  = ONE*FSCAL_UNIT
      IF(LAWID  == 0    ) LAWID  = -1
      IF(LAWID == -1) THEN
       ICHECK = -3
       NSTART = 100
       ERRTOL = FIVEEM3
       NMUAL = 2
      ENDIF
      IF (NTABLE_ == 0) THEN
        CALL ANCMSG(MSGID=894,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR)
      ENDIF
      IF (LAWID == 0)  LAWID = 1
      IF (LAWID /= 1 .AND. LAWID /= 2 .AND.  LAWID /= -1 ) THEN ! New flag (automatic fitting)
        CALL ANCMSG(MSGID=882,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,I2=LAWID)
      ENDIF

      TENSCUT = EP20
      DO I=1,5
        MU(I)=TEN*FSCAL_UNIT  !pressure
        AL(I)=TEN*FSCAL_UNIT  !pressure
      ENDDO
      GS = ZERO
      IF (LAWID == 1) THEN
        DO I=1,5
          GS = GS + MU(I)*AL(I)
        ENDDO
      ELSEIF (LAWID == 2) THEN
        DO I=1,2
          GS = GS + MU(I)*AL(I)
        ENDDO
      ENDIF
C---
      IF (NU == ZERO ) NU = ZEP495
      BULK     = GS*(ONE+NU)/MAX(EM20,THREE*(ONE-TWO*NU))
      IFUNC(1) = BULK_FNCT
      IFUNC(2) = NTABLE_
      PARMAT(1)= GS
      PARMAT(2)= GS*(ONE+NU)
      PARMAT(3)= NU
      PARMAT(6)= BULK      
      MU = GS/TWO
C---
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1)  = RHOR
      PM(2)  = GS
      PM(89) = RHO0
      PM(100)= BULK
      
      UPARAM(1) = MU(1)    
      UPARAM(2) = MU(2)    
      UPARAM(3) = MU(3)    
      UPARAM(4) = MU(4)    
      UPARAM(5) = MU(5)    
      UPARAM(6) = AL(1)    
      UPARAM(7) = AL(2)    
      UPARAM(8) = AL(3)    
      UPARAM(9) = AL(4)    
      UPARAM(10)= AL(5)    
      UPARAM(11)= BULK       
      UPARAM(12)= TENSCUT     
      UPARAM(13)= LAWID    
      UPARAM(14)= NU       
      UPARAM(15)= FSCAL    
      UPARAM(17)= GS       
      UPARAM(18)= NMUAL    
      UPARAM(19)= ICHECK 
      UPARAM(20)= NSTART
      UPARAM(21) = ZERO
      IF(IS_ENCRYPTED)UPARAM(21)= ONE 
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
c-----------------
      WRITE(IOUT,1100) TRIM(TITR),ID,69
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        IF (NTABLE_ /= 0) THEN
          WRITE(IOUT,'(5X,A,//)')'HYPERELASTIC MATERIAL CONSTANTS WILL BE DETERMINED BY NONLINEAR LEAST SQUARE FITTING '
          WRITE(IOUT,1300) NU,LAWID,BULK_FNCT,FSCAL,NMUAL,ICHECK          
        ENDIF
      ENDIF
C-----     
      RETURN
C---------------------------------------------------------------       
 1000 FORMAT
     & (5X,29H  TABULATED HYPER-ELASTIC LAW,/,
     &  5X,29H  ---------------------------,//)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT
     &(5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/ 
     &,5X,'LAW TYPE. . . . . . . . . . . . . . . .=',I10/
     &,5X,'BULK FUNCTION ID. . . . . . . . . . . .=',I10/
     &,5X,'SCALE FACTOR FOR BULK FUNCTION. . . . .=',1PG20.13/ 
     &,5X,'FIT ORDER . . . . . . . . . . . . . . .=',I10/
     &,5X,'ICHECK = ', I10//)
C---------------------------------------------------------------       
      END SUBROUTINE HM_READ_MAT69
