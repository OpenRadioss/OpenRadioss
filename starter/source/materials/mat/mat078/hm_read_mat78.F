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
Chd|  HM_READ_MAT78                 source/materials/mat/mat078/hm_read_mat78.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CALCULP2                      source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT78(UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE   ,IMATVIS  ,
     .                         NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC    ,PARMAT   , 
     .                         UNITAB   ,MAT_ID   ,TITR     ,MTAG     ,LSUBMODEL,
     .                         PM       ,NVARTMP)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ YOSHIDA MATERIAL LAW (/MAT/LAW78)
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)                :: MAT_ID,MAXFUNC,MAXUPARAM
      TYPE (UNIT_TYPE_)   ,INTENT(IN)   :: UNITAB 
      TYPE(SUBMODEL_DATA) ,DIMENSION(*) ,INTENT(IN)  :: LSUBMODEL
      CHARACTER*nchartitle              ,INTENT(IN)  :: TITR
      INTEGER  ,DIMENSION(MAXFUNC)      ,INTENT(OUT) :: IFUNC       
      my_real  ,DIMENSION(NPROPM)       ,INTENT(OUT) :: PM       
      my_real  ,DIMENSION(100)          ,INTENT(OUT) :: PARMAT   
      my_real  ,DIMENSION(MAXUPARAM)    ,INTENT(OUT) :: UPARAM
      INTEGER         ,INTENT(OUT)     :: NFUNC,NUVAR,NVARTMP,NUPARAM,ISRATE,IMATVIS
      TYPE(MLAW_TAG_) ,INTENT(INOUT)   :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,NORDRE,ISMSTR,NMAXW,OPTE,OPTR,FUNCID,ILAW,Iplas
      my_real :: YOUNG,YIELD,BSAT,MYU,BYU,HYU,CYU,KYU,RSAT,
     .           NU,SUM,GS,P,VISCMAX,FAC_L,FAC_T,FAC_M,FAC_C,
     .           RBULK,SHEAR,LAMDA,EINF,COE,YFAC,RHO0,
     .           R00,R45,R90,CST,CSTT,P1,P2,P3,P4,N3,MEXP,C1_KH
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
c-----------------------------------------------
c   S o u r c e   L i n e s 
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE  = 0
      IMATVIS = 0
      ILAW    = 78
C
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'      ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_SIGY'   ,YIELD    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BSAT'   ,BYU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'   ,CYU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HYST'   ,HYU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'      ,BSAT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MAT_M'      ,MYU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_RSAT'   ,RSAT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_OptR'   ,OPTR     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('C1'         ,CST      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('C2'         ,CSTT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('MAT_R00'    ,R00      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R45'    ,R45      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R90'    ,R90      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MEXP'   ,MEXP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IPLAS'  ,Iplas    ,IS_AVAILABLE, LSUBMODEL)
      !line-6
      CALL HM_GET_INTV  ('MAT_fct_IDE',FUNCID   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EA'     ,EINF     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CE'     ,COE      ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C1KH'   ,C1_KH    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      !========== DEFAULT VALUES=============!              
      PM(1) = RHO0
      PM(89)= RHO0
C       
      OPTE = 0
      IF (FUNCID > 0 ) OPTE  = 1
      IF (R00 == ZERO) R00   = ONE
      IF (R45 == ZERO) R45   = ONE
      IF (R90 == ZERO) R90   = ONE
      IF (Iplas == 0)  Iplas = 1
      IF (NU < ZERO .OR. NU >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF ((HYU < ZERO).OR.(HYU > ONE)) THEN 
         CALL ANCMSG(MSGID=1886,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=HYU,
     .               I1=MAT_ID,
     .               C1=TITR)      
      ENDIF
      IF (C1_KH <= CYU) C1_KH = CYU
      ! Hill 48
      IF (Iplas == 1) THEN 
        MEXP = ZERO
        P1   = R00*(ONE+R90)/R90/(ONE+R00)
        P2   = R00/(R00+ONE)
        P3   = (R00+R90)*(TWO*R45+ONE)/R90/(ONE+R00)
        P4   = R00/R90/(ONE+R00)
        N3   = P1/(ONE+R90)
      ! Barlat 89
      ELSEIF (Iplas == 2) THEN
        ! Wrong value of M exponent
        IF ((MEXP > ZERO).AND.(MEXP < TWO)) THEN
          CALL ANCMSG(MSGID=1735,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR)          
        ENDIF
        ! Default value of M exponent
        IF (MEXP == ZERO) MEXP = SIX
        P1   = TWO - TWO*SQRT((R00/(ONE+R00))*(R90/(ONE+R90))) ! A
        P2   = TWO - P1                                       ! C
        P3   = SQRT((R00/(ONE+R00))*((ONE+R90)/R90))             ! H
        P4   = ONE                                              ! P
        CALL CALCULP2(P1,P2,P3,P4,MEXP,R45)                    ! USE ROUTINE OF HM_READ_MAT57
        N3   = MEXP                                            ! M
      ENDIF
c---------------------------------    
      NVARTMP = 0
      NUVAR   = 6
      NFUNC   = OPTE
      NUPARAM = 22
c
      IF (NFUNC == 1) THEN
        IFUNC(1) = FUNCID
        NVARTMP  = 1
      ENDIF
C           
      IF (BSAT < YIELD) THEN
        BSAT=YIELD 
        CALL ANCMSG(MSGID=922,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF 
c----------------------
      UPARAM(1)  = YOUNG
      UPARAM(2)  = NU
      UPARAM(3)  = YIELD
      UPARAM(4)  = BYU
      UPARAM(5)  = CYU
      UPARAM(6)  = HYU
      UPARAM(7)  = BSAT
      UPARAM(8)  = MYU
      UPARAM(9)  = RSAT
      UPARAM(10) = EINF
      UPARAM(11) = COE
      UPARAM(12) = OPTE
      UPARAM(13) = OPTR
      UPARAM(14) = P1
      UPARAM(15) = P2
      UPARAM(16) = P3
      UPARAM(17) = P4
      UPARAM(18) = N3
      UPARAM(19) = CST
      UPARAM(20) = CSTT
      UPARAM(21) = Iplas
      UPARAM(22) = C1_KH
c---------------------
      PARMAT(1)  = YOUNG/THREE/(ONE - TWO*NU)
      PARMAT(2)  = YOUNG
      PARMAT(3)  = NU
c     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
c------------------------------             
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%L_SIGA = 6
      MTAG%L_SIGB = 6
      MTAG%L_SIGC = 6
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
c------------------------------             
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
       WRITE(IOUT,1000) 
       WRITE(IOUT,1001) TRIM(TITR),MAT_ID,ILAW
       WRITE(IOUT,1002) RHO0   
       WRITE(IOUT,1100) YOUNG,NU,FUNCID,EINF,COE
       WRITE(IOUT,1200) YIELD,BYU,CYU,HYU,BSAT,C1_KH
       WRITE(IOUT,1300) MYU,RSAT,OPTR
       WRITE(IOUT,1400) Iplas,R00,R45,R90,CST,CSTT
       IF (Iplas == 2) WRITE(IOUT,1500) P1,P2,P3,P4,MEXP
      ENDIF 
c----------------------------------------------------------------
 1000 FORMAT
     & (5X,'   YOSHIDA-UEMORI MATERIAL LAW       ',/
     &  5X,'   ---------------------------       ',//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . =',1PG20.13/) 
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . . . .  . . . . =',1PG20.13/
     & 5X,'POISSON''S RATIO  . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'YOUNG MODULUS EVOLUTION FUNCTION  . . . . . . =',I10/
     & 5X,'MATERIAL PARAMETER (EINF ). . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (COE  ). . . . . . . . . . =',1PG20.13)
 1200 FORMAT(
     & 5X,'YIELD STRESS (YIELD). . . . . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (BYU ) . . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (CYU  ). . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (HYU  ). . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (BSAT  ) . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (C1_KH  )  . . . . . . . . =',1PG20.13)    
 1300 FORMAT(//
     & 5X,'MATERIAL PARAMETER (MYU  ). . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (RSAT ). . . . . . . . . . =',1PG20.13/
     & 5X,'FLAG ISOTROPIC HARDENING FUNC (OPTR)  . . . . =',I10)
 1400 FORMAT(//
     & 5X,'PLASTIC CRITERION FLAG  . . . . . . . . . . . =',I10/  
     & 5X,'  Icrit=1  HILL   1948 CRITERION'/
     & 5X,'  Icrit=2  BARLAT 1989 CRITERION'/
     & 5X,'LANKFORD COEFFICIENT R00. . . . . . . . . . . =',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R45. . . . . . . . . . . =',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R90. . . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (CST). . . . . . . . . . . =',1PG20.13/
     & 5X,'MATERIAL PARAMETER (CSTT) . . . . . . . . . . =',1PG20.13/)    
 1500 FORMAT(//
     & 5X,'BARLAT PARAMETER A. . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'BARLAT PARAMETER C. . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'BARLAT PARAMETER H. . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'BARLAT PARAMETER P. . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'BARLAT EXPONENT  M. . . . . . . . . . . . . . =',1PG20.13/)
c----------------------------------------------------------------
      RETURN
      END

