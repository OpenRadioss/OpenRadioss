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
!||    hm_read_mat43              ../starter/source/materials/mat/mat043/hm_read_mat43.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat                ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim          ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword           ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod               ../starter/share/modules1/elbuftag_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT43(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW43 WITH HM READER ( TO BE COMPLETED )
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
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,
     .                                         MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NRATE,NPS,IR0
      MY_REAL 
     .         E,NU,G,C1,EPSMAX,EPSR1,EPSR2,RATE(11),YFAC(11),
     .         R0,R45,R90,R,H,FISOKIN,M,
     .         EINF,CE,FAC_PRES
      INTEGER IFUNCE, OPTE, NUM_FUNC,NUMCURVES
      my_real :: RHO0, RHOR, ASRATE
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      MTAG%G_PLA  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_PLA  = 1
      MTAG%L_SEQ  = 1
      MTAG%L_EPSD = 1
      MTAG%G_EPSD = 1
      ISRATE  = 0
      IMATVIS = 0
 
 
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'      ,E           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_INTV  ('Yr_fun'     ,IFUNCE      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EFIB'   ,EINF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'      ,CE          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MAT_R00'    ,R0          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R45'    ,R45         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R90'    ,R90         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CHARD'  ,FISOKIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iyield' ,IR0         ,IS_AVAILABLE, LSUBMODEL)
      !line-5
      CALL HM_GET_FLOATV('MAT_EPS'    ,EPSMAX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST1'  ,EPSR1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2'  ,EPSR2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('Fcut'       ,ASRATE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Fsmooth'    ,ISRATE      ,IS_AVAILABLE, LSUBMODEL)
      !unit
      CALL HM_GET_FLOATV_DIM('MAT_E'  ,FAC_PRES    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !num_curves      
      CALL HM_GET_INTV  ('NUM_CURVES' ,NUMCURVES   ,IS_AVAILABLE, LSUBMODEL)

      !========== DEFAULT VALUES=============!              
      RHOR=RHO0
      !IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
      NRATE = 0

      DO I=1,NUMCURVES
        CALL HM_GET_INT_ARRAY_INDEX      ('FunctionIds' ,IFUNC(I)  ,I ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX    ('ABG_cpa'     ,YFAC(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX    ('ABG_cpb'     ,RATE(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
        IF(IFUNC(I)/=0) NRATE = I
      ENDDO

      IF (NRATE == 0) THEN
         CALL ANCMSG(MSGID=366,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
C
      IF(R0 ==ZERO) R0    = ONE
      IF(R45==ZERO) R45   = ONE
      IF(R90==ZERO) R90   = ONE
      IF(EPSR1==ZERO)EPSR1=INFINITY
      IF(EPSR2==ZERO)EPSR2=TWO*INFINITY
      DO I=1,NRATE
       IF(YFAC(I)==ZERO) YFAC(I)=ONE * FAC_PRES
      ENDDO
C
      !----------------------------------------------------------
      ! Filtering frequency
      IF (ASRATE /= ZERO) THEN
        ! If a filtering frequency is given by the user
        ISRATE = 1
      ELSE
        ! If no filtering frequency is given but the flag is activated
        IF (ISRATE /= 0) THEN
          ASRATE  = 10000.0D0*UNITAB%FAC_T_WORK  
        ! If no filtering frequency and no flag is activated => no filtering
        ELSE
          ASRATE  = ZERO
        ENDIF
      ENDIF
      !----------------------------------------------------------           
C
      IF (NRATE == 1) THEN
       NUM_FUNC=2
       IFUNC(2)=IFUNC(1)
       RATE(1)=ZERO
       RATE(2)=ONE
       YFAC(2)=YFAC(1)
      ELSEIF (RATE(1) == 0) THEN
       NUM_FUNC=NRATE
      ELSE
       NUM_FUNC=NRATE+1
       DO J=NRATE,1,-1
        IFUNC(J+1)=IFUNC(J)
        RATE(J+1)=RATE(J)
        YFAC(J+1)=YFAC(J)
       ENDDO
       RATE(1)=ZERO
      ENDIF
C
      UPARAM(1)=NUM_FUNC
      UPARAM(2)=E
      UPARAM(3)=E/(ONE-NU*NU)
      UPARAM(4)=NU*UPARAM(3)
      G=HALF*E/(ONE+NU)
      UPARAM(5)=G
      UPARAM(6)=NU
      R=(R0+R45+R45+R90)*FOURTH
      H=R/(ONE+R)
C-----------A1,A2,A3,A12--------
      UPARAM(7)=H*(ONE+ONE/R0)
      UPARAM(8)=H*(ONE+ONE/R90)
      UPARAM(9)=H*TWO
      UPARAM(10)=(R45*TWO + ONE)*(UPARAM(7)+UPARAM(8)-UPARAM(9))
      IF (IR0 > 0) THEN
       UPARAM(8)=UPARAM(8)/UPARAM(7)
       UPARAM(9)=UPARAM(9)/UPARAM(7)
       UPARAM(10)=UPARAM(10)/UPARAM(7)
       UPARAM(7)=ONE
      END IF
      NPS=10
      DO J=1,NUM_FUNC
       UPARAM(J+NPS)=RATE(J)
      ENDDO
      DO J=1,NUM_FUNC
       UPARAM(J+NPS+NUM_FUNC)=YFAC(J)
      ENDDO
      UPARAM(NPS+2*NUM_FUNC+1)=EPSMAX
      UPARAM(NPS+2*NUM_FUNC+2)=EPSR1
      UPARAM(NPS+2*NUM_FUNC+3)=EPSR2
      UPARAM(NPS+2*NUM_FUNC+4)=TWO*G
      UPARAM(NPS+2*NUM_FUNC+5)=THREE*G
      C1=E/THREE/(ONE - TWO*NU)
      UPARAM(NPS+2*NUM_FUNC+6)=C1
      UPARAM(NPS+2*NUM_FUNC+7)=C1+ FOUR_OVER_3*G
      IF (FISOKIN>ONE.OR.FISOKIN<ZERO) THEN
        CALL ANCMSG(MSGID=913,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      END IF
      UPARAM(NPS+2*NUM_FUNC+8)=FISOKIN
C
c------------------------------------
      OPTE = 0
      IF (IFUNCE > 0 )OPTE = 1
      NFUNC = NUM_FUNC + 1
      IFUNC(NFUNC) = IFUNCE
      UPARAM(NPS+2*NUM_FUNC+9) = NFUNC
      UPARAM(NPS+2*NUM_FUNC+10) = OPTE
      UPARAM(NPS+2*NUM_FUNC+11) = EINF
      UPARAM(NPS+2*NUM_FUNC+12) = CE
c
      NUPARAM = NPS+2*NUM_FUNC+12
c------------------------------------
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = ASRATE
      NUVAR = 5+NUM_FUNC
C
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ORTHOTROPIC")        
C
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,43
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100)E,NU,G,R0,R45,R90,FISOKIN
        IF (IR0 >0) WRITE(IOUT,1110)
        WRITE(IOUT,1300)EPSMAX,EPSR1,EPSR2,IFUNCE,EINF,CE
        WRITE(IOUT,1400)ISRATE,ASRATE
        WRITE(IOUT,1200)(IFUNC(J),YFAC(J),RATE(J),J=1,NUM_FUNC)
        WRITE(IOUT,*)' '
      ENDIF
C
      RETURN
 1000 FORMAT(
     & 5X,40H  TABULATED HILL ORTHOTROPIC PLASTICITY,/,
     & 5X,40H  ------------------------------------- /)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R00. . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R45. . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R90. . . . . . . .=',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENING FACTOR. . . . .=',1PG20.13)
 1110 FORMAT(
     & 5X,'YIELD STRESS IS SUPPOSSD IN ORTHOTROPIC DIR. 1 '/)
 1200 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER. . . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13)
 1300 FORMAT(
     & 5X,'MAXIMUM PLASTIC STRAIN. . . . . . . . .=',1PG20.13/
     & 5X,'TENSILE FAILURE STRAIN 1. . . . . . . .=',1PG20.13/
     & 5X,'TENSILE FAILURE STRAIN 2. . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS SCALE FACTOR FUNCTION . .=',I10/
     & 5X,'YOUNG MODULUS EINF. . . . . . . . . . .=',1PG20.13/
     & 5X,'PARAMETER CE. . . . . . . . . . . . . .=',1PG20.13)
 1400 FORMAT(
     & 5X,'STRAIN RATE FILTERING FLAG  . . . . . .=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13)
c
      END


