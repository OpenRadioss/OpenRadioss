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
Chd|  HM_READ_MAT82                 source/materials/mat/mat082/hm_read_mat82.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT82(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,IMATVIS  ,UNITAB   ,
     .           MAt_ID   ,TITR     ,LSUBMODEL,PM       ,MATPARAM )
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
C   READ MAT LAW82 WITH HM READER
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
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM     
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      INTEGER, INTENT(OUT)   :: IMATVIS,NUPARAM,NUVAR,NFUNC
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT) :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      my_real
     .        MU(100),AL(100),NU,NU0,RHO0,
     .        GS,P,D(100),ZEP495,RHOR   
      INTEGER I,NORDRE,ILAW
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      IMATVIS = 1
      ILAW    = 82
      ZEP495  = ZEP4 + NINE*EM02 + FIVE*EM03
      NFUNC = 0

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_NU'    ,NU0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('ORDER'    ,NORDRE    ,IS_AVAILABLE,LSUBMODEL)
   
      IF(NORDRE == 0)THEN
       CALL ANCMSG(MSGID=559,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=MAT_ID,
     .             C1=TITR)
      ENDIF  
      DO I=1,NORDRE         
        CALL HM_GET_FLOAT_ARRAY_INDEX('Mu_arr'            ,MU(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Alpha_arr'         ,AL(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Gamma_arr'         ,D(I)      ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ENDDO
C      
      GS = ZERO
      DO I=1,NORDRE 
       GS = GS + MU(I)
      ENDDO 
C      
      IF(GS < ZERO) THEN
        CALL ANCMSG(MSGID=846,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF           
C ...   
       IF(NU0 == HALF) NU0= ZEP495
       IF(NU0 == ZERO ) THEN
         IF( D(1) > ZERO) THEN
            P=TWO/D(1)
            NU = ONE/(SIX*P + TWO*GS)
            NU = (THREE*P - TWO*GS)*NU
            IF(NU == HALF) NU= ZEP499
            D(1) = THREE*(ONE - TWO*NU)/GS/(ONE + NU)
            P = TWO/D(1)
          ELSE
           NU = ZEP495
           D(1) = THREE*(ONE - TWO*NU)/GS/(ONE + NU)
           P = TWO/D(1) 
          ENDIF     
       ELSE
          NU = NU0
          D(1) = THREE*(ONE - TWO*NU)/GS/(ONE + NU)
          P = TWO/D(1)
       ENDIF   
C
       UPARAM(1) = NORDRE
       DO I=1,NORDRE
        UPARAM(1 + I  ) = MU(I)
        UPARAM(1 + NORDRE +   I) = AL(I)
        UPARAM(1 + 2*NORDRE +   I) = D(I)
       ENDDO  
       NUPARAM = 1 + 3*NORDRE 
       NUVAR  = 1
C nu =0 ---> K = 2*G/3 ???????????? je ne comprends pas GW
csm       PARMAT(1) = HALF*P
csm       PARMAT(2) = GS*(1 + NU)
csm       PARMAT(3) = NU
       PARMAT(1) = P
       PARMAT(2) = TWO*GS*(ONE + NU)
       PARMAT(3) = NU
       PARMAT(6) = P
C       
       IF(RHOR==ZERO)RHOR=RHO0
       PM(1) =RHOR
       PM(89)=RHO0 
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
c-----------------
       WRITE(IOUT,1010) TRIM(TITR),MAT_ID,ILAW
       WRITE(IOUT,1000)   
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1020)RHO0 
        WRITE(IOUT,1100)NORDRE,NU,GS
        IF (NORDRE >= 1) WRITE(IOUT,1201) MU(1) ,AL(1) ,D(1)
        IF (NORDRE >= 2) WRITE(IOUT,1202) MU(2) ,AL(2) ,D(2)
        IF (NORDRE >= 3) WRITE(IOUT,1203) MU(3) ,AL(3) ,D(3)
        IF (NORDRE >= 4) WRITE(IOUT,1204) MU(4) ,AL(4) ,D(4)
        IF (NORDRE >= 5) WRITE(IOUT,1205) MU(5) ,AL(5) ,D(5)
        IF (NORDRE >= 6) WRITE(IOUT,1206) MU(6) ,AL(6) ,D(6)
        IF (NORDRE >= 7) WRITE(IOUT,1207) MU(7) ,AL(7) ,D(7)
        IF (NORDRE >= 8) WRITE(IOUT,1208) MU(8) ,AL(8) ,D(8)
        IF (NORDRE >= 9) WRITE(IOUT,1209) MU(9) ,AL(9) ,D(9)
        IF (NORDRE ==10) WRITE(IOUT,1210) MU(10),AL(10),D(10)
      ENDIF
c-----------------------------------------------------------------------
 1000 FORMAT
     & (5X, 'OGDEN HYPERELASIC LAW' ,/,
     &  5X, ' ---------------',//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . .=',I10/) 
 1020 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . . .=',1PG20.13/) 
 1100 FORMAT
     & (5X,'ORDER OF STRAIN ENERGY . . . . . . . . . .=',I10/
     &  5X,'POISSON RATIO. . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,'INITIAL SHEAR MODULUS. . . . . . . . . . .=',E12.4/)
 1201 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU1). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA1) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D1) . . . . . . . . . .=',1PG20.13)    
 1202 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU2). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA2) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D2) . . . . . . . . . .=',1PG20.13)    
 1203 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU3). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA3) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D3) . . . . . . . . . .=',1PG20.13)    
 1204 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU4). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA4) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D4) . . . . . . . . . .=',1PG20.13)    
 1205 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU5). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA5) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D5) . . . . . . . . . .=',1PG20.13)    
 1206 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU6). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA6) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D6) . . . . . . . . . .=',1PG20.13)    
 1207 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU7). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA7) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D7) . . . . . . . . . .=',1PG20.13)    
 1208 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU8). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA8) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D8) . . . . . . . . . .=',1PG20.13)    
 1209 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU9). . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA9) . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D9) . . . . . . . . . .=',1PG20.13)    
 1210 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU10) . . . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (ALPHA10). . . . . . . .=',1PG20.13/
     & 5X,'MATERIAL PARAMETER (D10). . . . . . . . . .=',1PG20.13)    
c-----------------------------------------------------------------------
      RETURN
      END

