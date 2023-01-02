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
Chd|  HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CALCULP2                      source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT57(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MFUNC    , 
     .                         MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   , 
     .                         ISRATE   ,PM       ,LSUBMODEL,ID       ,TITR     )                     
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
      TYPE(UNIT_TYPE_),INTENT(IN) :: UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)  :: PM     
      my_real, DIMENSION(100)    ,INTENT(OUT)    :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT) :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(OUT) :: IFUNC
      INTEGER, INTENT(INOUT)          :: MFUNC,NUPARAM,NUVAR,ISRATE
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: I,J,NRATE,NPS,OPTE,IFUNCE,ILAW,NFUNC
      my_real :: RHO0, RHOR,E,NU,G,C1,EPSMAX,EPSR1,EPSR2,RATE(11),YFAC(11),YFAC_UNIT(11),
     .           R0,R45,R90,R,H,FISOKIN,M,EINF,CE,ASRATE      
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0
      ILAW   = 57
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
Card1
      CALL HM_GET_FLOATV('MAT_E'    ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2
      CALL HM_GET_INTV  ('MAT_fct_IDE',IFUNCE  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EA'     ,EINF    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CE'     ,CE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_FLOATV('MAT_R00'    ,R0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R45'    ,R45      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R90'    ,R90      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CHARD'  ,FISOKIN  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'      ,M        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4
      CALL HM_GET_FLOATV('MAT_EPS'    ,EPSMAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST1'  ,EPSR1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2'  ,EPSR2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fcut'       ,ASRATE  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Fsmooth'    ,ISRATE  ,IS_AVAILABLE, LSUBMODEL)

      NRATE = 0
Card5 - 15
      DO J=1,10
        CALL HM_GET_INT_ARRAY_INDEX        ('FunctionIds',IFUNC(J)    ,J,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX      ('ABG_cpa'    ,YFAC(J)     ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF(YFAC(J) == ZERO) THEN
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('ABG_cpa'    ,YFAC_UNIT(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB) 
          YFAC(J)=ONE * YFAC_UNIT(J)
        ENDIF
        CALL HM_GET_FLOAT_ARRAY_INDEX      ('ABG_cpb'    ,RATE(J)     ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF(IFUNC(J) /= 0) NRATE = J                          
      ENDDO
c
      IF (NRATE == 0) THEN
         CALL ANCMSG(MSGID=366,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
      ENDIF
C
      IF(R0 ==ZERO) R0    = ONE
      IF(R45==ZERO) R45   = ONE
      IF(R90==ZERO) R90   = ONE
      IF(M==ZERO)   M     = SIX
      IF(EPSR1==ZERO)EPSR1=INFINITY
      IF(EPSR2==ZERO)EPSR2=TWO*INFINITY
C
      !----------------------------------------------------------
      ! Filtering frequency
      IF (ASRATE /= ZERO) THEN
        ! If a filtering frequency is given by the user
        ISRATE = 1
      ELSE
        ! If no filtering frequency is given but the flag is activated
        IF (ISRATE /= 0) THEN
          ASRATE  = 10000.0D0*FAC_T_WORK  
        ! If no filtering frequency and no flag is activated => no filtering
        ELSE
          ASRATE  = ZERO
        ENDIF
      ENDIF
      !----------------------------------------------------------           
C
      IF (NRATE == 1) THEN
        NFUNC=2
        IFUNC(2)=IFUNC(1)
        RATE(1)=ZERO
        RATE(2)=ONE
        YFAC(2)=YFAC(1)
      ELSEIF (RATE(1) == ZERO) THEN
        NFUNC=NRATE
      ELSE
        NFUNC=NRATE+1
        DO J=NRATE,1,-1
         IFUNC(J+1)=IFUNC(J)
         RATE(J+1)=RATE(J)
         YFAC(J+1)=YFAC(J)
        ENDDO
        RATE(1)=ZERO
      ENDIF

C------------------------------
C------UPARAM STORAGE----------
C------------------------------
      UPARAM(1)=NFUNC
      UPARAM(2)=E
      UPARAM(3)=E/(1.-NU*NU)
      UPARAM(4)=NU*UPARAM(3)
      G=HALF*E/(ONE+NU)
      UPARAM(5)=G
      UPARAM(6)=NU
      R=R0/(ONE+R0)
      H=R90/(ONE+R90)
C-----------A,C,H,P--------
      UPARAM(8)=TWO*SQRT(R*H)
      UPARAM(7)=TWO-UPARAM(8)
      UPARAM(9)=SQRT(R/H)
C-----------P is identified by iteration--------
      UPARAM(10)=ONE
      CALL CALCULP2(UPARAM(7),UPARAM(8),UPARAM(9),UPARAM(10),M,R45)
      NPS=10
      DO J=1,NFUNC
       UPARAM(J+NPS)=RATE(J)
      ENDDO
      DO J=1,NFUNC
       UPARAM(J+NPS+NFUNC)=YFAC(J)
      ENDDO
      UPARAM(NPS+2*NFUNC+1)=EPSMAX
      UPARAM(NPS+2*NFUNC+2)=EPSR1
      UPARAM(NPS+2*NFUNC+3)=EPSR2
      UPARAM(NPS+2*NFUNC+4)=M
      UPARAM(NPS+2*NFUNC+5)=THREE*G
      C1=THIRD*E/(ONE-TWO*NU)
      UPARAM(NPS+2*NFUNC+6)=C1
      UPARAM(NPS+2*NFUNC+7)=C1+FOUR_OVER_3*G
      UPARAM(NPS+2*NFUNC+8)=FISOKIN
c---------------------------------
      OPTE = 0
      IF (IFUNCE > 0 )OPTE = 1
      MFUNC = NFUNC + 1
      IFUNC(MFUNC) = IFUNCE
      UPARAM(NPS+2*NFUNC+9) = MFUNC
      UPARAM(NPS+2*NFUNC+10) = OPTE
      UPARAM(NPS+2*NFUNC+11) = EINF
      UPARAM(NPS+2*NFUNC+12) = CE
c---------------------------------
      NUPARAM=NPS+2*NFUNC+12
      NUVAR = 5+NFUNC
c---------------------------------
C      PM PARMAT
C---------------------------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1)  = RHOR
      PM(89) = RHO0    
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = ASRATE

C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(C1+FOUR_OVER_3*G) ! == (1-2*nu)/(1-nu)
C---------------------------------
      MTAG%G_PLA  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
      MTAG%L_PLA  = 1
      MTAG%L_EPSD = 1
      MTAG%G_EPSD = 1
C---------------------------------
      WRITE(IOUT,1001) TRIM(TITR),ID,57
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED)THEN                                     
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100)E,NU,G,R0,R45,R90,FISOKIN,M
        WRITE(IOUT,1400)UPARAM(7),UPARAM(8),UPARAM(9),UPARAM(10)
        WRITE(IOUT,1300)EPSMAX,EPSR1,EPSR2,IFUNCE,EINF,CE
        WRITE(IOUT,1500)ISRATE,ASRATE
        WRITE(IOUT,1200)(IFUNC(J),YFAC(J),RATE(J),J=1,NFUNC)
        WRITE(IOUT,*)' '
      ENDIF
c---------------------------------
      UPARAM(8)=UPARAM(8)*TWO**M
C
      RETURN
 1000 FORMAT(
     & 5X,40H TABULATED BARLAT 3 PARAMETER PLASTICITY,/,
     & 5X,40H  ---------------------------          ,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . =',1PG20.13/)  
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R00. . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R45. . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R90. . . . . . . .=',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENNING FACTOR. . . . =',1PG20.13/
     & 5X,'BARLAT YIELD EXPONENT M  . . . . . . . =',1PG20.13)
 1400 FORMAT(
     & 5X,'BARLAT COEFFICIENT A  . . . . . . . . .=',1PG20.13/
     & 5X,'BARLAT COEFFICIENT C  . . . . . . . . .=',1PG20.13/
     & 5X,'BARLAT COEFFICIENT H  . . . . . . . . .=',1PG20.13/
     & 5X,'BARLAT COEFFICIENT P  . . . . . . . . .=',1PG20.13)
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
 1500 FORMAT(
     & 5X,'STRAIN RATE FILTERING FLAG  . . . . . .=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13)
      END
C

Chd|====================================================================
Chd|  CALCULP2                      source/materials/mat/mat057/hm_read_mat57.F
Chd|-- called by -----------
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT78                 source/materials/mat/mat078/hm_read_mat78.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CALCULP2(A  ,C ,H , P,  M, R45 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real  A  ,C ,H , P,  M, R45 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NMAX
      MY_REAL GAMA,ALPHA,BETA,F,DF,C1,C2,C3,C4,C5,PP,M2,
     .        ABA2,ABB2,ABG1,CA,CB,CA1,CB1
      DATA NMAX/10/
C--------------------------------------------------------------
      M2= M-TWO
      C1=TWO**M*C
      C2=FOURTH*(1-H)
      C2=C2*C2
      C3=FOURTH*(1+H)
      C4=HALF*(1+H)
      PP=P
      DO I=1,NMAX
       GAMA=SQRT(C2+FOURTH*PP*PP)
       ALPHA=C3-GAMA
       BETA =C3+GAMA
       C5=TWO*C2/GAMA
       ABA2=ABS(ALPHA)**M2
       ABB2=ABS(BETA)**M2
       CA= ABA2*(C4-C5)*(ONE+R45)
       CB= ABB2*(C4+C5)*(ONE+R45)
       CA1= ABA2*ALPHA
       CB1= ABB2*BETA
       ABG1=C1*GAMA**(M-ONE)
       F = A*(ALPHA*(CA1-CA)+BETA*(CB1-CB))+GAMA*ABG1
       DF =A*((M-ONE)*(CA-CB)-(CA1-CB1)*(M+(ONE+R45)*C5/GAMA))
     .     +M*ABG1
       DF =HALF*DF*PP/GAMA
       PP=PP-F/DF
      ENDDO
      P=PP
C
      RETURN
      END






