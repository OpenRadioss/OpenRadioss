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
Chd|  HM_READ_MAT119                source/materials/mat/mat119/hm_read_mat119.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT119(MTAG     ,UPARAM   ,MAXUPARAM,NUPARAM  ,PM       ,
     .                          MATPARAM ,PARMAT   ,NUVAR    ,MAT_ID   ,TITR     ,
     .                          MAXTABL  ,NUMTABL  ,ITABLE   ,UNITAB   ,LSUBMODEL,
     .                          ISRATE   )
C-----------------------------------------------
C   D e s c r i p t i o n
C   =====================
C   READ MAT LAW119 WITH HM READER
C-----------------------------------------------
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
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT)  :: NUPARAM,NUVAR,NUMTABL,ISRATE
      INTEGER ,INTENT(IN)   :: MAT_ID,MAXUPARAM,MAXTABL
      INTEGER, DIMENSION(MAXTABL) ,INTENT(INOUT) :: ITABLE
      CHARACTER*nchartitle,INTENT(IN)::TITR
      my_real ,INTENT(INOUT):: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      TYPE (UNIT_TYPE_)      ,INTENT(IN)    :: UNITAB 
      TYPE(SUBMODEL_DATA)    ,INTENT(IN)    :: LSUBMODEL(*)
      TYPE(MLAW_TAG_)        ,INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER FUNC1,FUNC2,IRELOAD
      my_real :: NU,N12,E11,E22,G12,G23,G31,RCOMP,DET,C1,SSP,
     .   A11,A22,A12,A1C,A2C,GC,RHO0,LMIN,DAMP,FSCALE1,FSCALE2,FSCALET,
     .   ECOAT,NUCOAT,TCOAT
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
c=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
card1
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE,LSUBMODEL,UNITAB)     
      CALL HM_GET_FLOATV('LMIN'     ,LMIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)     
card2
      CALL HM_GET_FLOATV('STIFF1'   ,E11      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP1'    ,DAMP     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('RE'       ,RCOMP    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card3
      CALL HM_GET_INTV  ('FUN_L'    ,FUNC1    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_UL'   ,FUNC2    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fcoeft1'  ,FSCALE1  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Fcoeft2'  ,FSCALE2  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('Ireload'  ,IRELOAD  ,IS_AVAILABLE, LSUBMODEL)
card4
      CALL HM_GET_FLOATV('E22'      ,E22      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('NU12'     ,N12      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('G12'      ,G12      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Fcoeft22' ,FSCALET ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card5
      CALL HM_GET_FLOATV('ECOAT'    ,ECOAT     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('NUCOAT'   ,NUCOAT    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TCOAT'    ,TCOAT     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (FUNC2 == FUNC1) FUNC2 = 0
C
c-----------------------------------------------------
      PM(1) = RHO0
      PM(89)= RHO0
c
      ISRATE = -1
      IF (N12 == ZERO) N12 = 0.19
C--   Values are replaced in create_seatbelt.F when section is available
      IF (E11 == ZERO) E11 = EM20
      IF (E22 == ZERO) E22 = EM20
      IF (G12 == ZERO) G12 = EM20
C
      IF (FSCALE1 == ZERO) FSCALE1 = ONE
      IF (FSCALE2 == ZERO) FSCALE2 = ONE
      IF (FSCALET == ZERO) FSCALET = EM01

C--   stiffness of the shell is 1% of the total stiffness
      FSCALE1 = EM02*FSCALE1
      FSCALE2 = EM02*FSCALE2
      FSCALET = EP02*FSCALET
C
C-----------------------------------------------------
      IF (FUNC1 == 0 .and. E11 == ZERO) THEN
         CALL ANCMSG(MSGID=306, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=119,
     .               I2=MAT_ID,
     .               C1=TITR,
     .               C2='E11')
      ENDIF
C-----------------------------------------
      IF (RCOMP == ZERO) RCOMP = ONE
      IF (RCOMP < EM03) THEN
          CALL ANCMSG(MSGID=1572, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR)
         RCOMP = EM03
      ENDIF
C--------------------------------------------------------
      IF (NUCOAT == ZERO) NUCOAT = N12
      A1C = ECOAT /  (ONE - NUCOAT**2)
      A2C = A1C * NUCOAT
      GC  = ECOAT * HALF / (ONE + NUCOAT)
c-----------------------------------------
      UPARAM(1)  = E11*EM02
      UPARAM(2)  = E22
      UPARAM(3)  = N12
      UPARAM(6)  = G12  
      UPARAM(7)  = ZERO      ! A11 - calculated by seatbelt
      UPARAM(8)  = ZERO      ! A22 - calculated by seatbelt
      UPARAM(9)  = ZERO      ! A12 - calculated by seatbelt
      UPARAM(10) = RCOMP                          
      UPARAM(11) = FSCALE1                           
      UPARAM(12) = FSCALE2  
      UPARAM(13) = FSCALET
      UPARAM(14) = A1C       ! coating
      UPARAM(15) = A2C       ! coating
      UPARAM(16) = TCOAT     ! coating
      UPARAM(18) = ZERO      ! Xint
      UPARAM(19) = ZERO      ! Yint
      UPARAM(20) = GC
      UPARAM(21) = IRELOAD
c
      NUPARAM = 22
      NUVAR   = 10
      NUMTABL = 2
c
      ITABLE(1) = FUNC1                
      ITABLE(2) = FUNC2                
c --------------------------
C--   Values of C1,E11,E22 and N21 are computed in create_seatbelt.F when section is available
C     PARMAT(1) = C1
C     PARMAT(2) = MAX(E11,E22)
C     PARMAT(3) = SQRT(N12*N21)
      PARMAT(4) = ISRATE
      PARMAT(5) = ZERO ! FCUT
c
c---- Definition of internal variables variables for seatbelts
c
      MTAG%G_INTVAR = 7
      MTAG%G_SLIPRING_ID = 1
      MTAG%G_SLIPRING_FRAM_ID = 2
      MTAG%G_UPDATE = 1
      MTAG%G_ADD_NODE = 5
      MTAG%G_POSX = 2
c
c--------------------- 
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SMALL_STRAIN") 
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCREMENTAL")
c----------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,119
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        IF (FUNC1 == 0) THEN
          WRITE(IOUT,1300) E11,E22,N12,G12,RCOMP,LMIN,DAMP
        ELSE
          WRITE(IOUT,1400) N12,G12,RCOMP,LMIN,DAMP,FUNC1,FUNC2,
     .    FSCALE1,FSCALE2,IRELOAD,FSCALET
        END IF
        IF (ECOAT > ZERO) THEN
          WRITE(IOUT,1500) ECOAT,NUCOAT,TCOAT
        END IF
      ENDIF
c--------------------------------------
 1000 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'ORTHOTROPIC MATERIAL FOR SEATBELTS (LAW119)       ',/,
     & 5X,'-------------------------------------------       ',/)
 1200 FORMAT(
     & 5X,'MASS PER UNIT LENGTH  . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'STIFFNESS PER UNIT LENGTH (TENSION) . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS E22 (TENSION) . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO NU12. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS G12 . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'COMPRESSION REDUCTION FACTOR. . . . . . . . . . . ',/
     & 5X,'     RCOMP=E11C/E11= E22C/E22 . . . . . . . . . .=',1PG20.13/
     & 5X,'MINIMUM LENGTH LMIN. . . . . . . . .. . . . . . .=',1PG20.13/
     & 5X,'DAMPING COEFFICIENT. . . . . . . . .. . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'POISSON RATIO NU12. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS G12 . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'COMPRESSION REDUCTION FACTOR. . . . . . . . . . . ',1PG20.13/
     & 5X,'MINIMUM LENGTH LMIN. . . . . . . . .. . . . . . .=',1PG20.13/
     & 5X,'DAMPING COEFFICIENT. . . . . . . . .. . . . . . .=',1PG20.13/
     & 5X,'LOADING CURVE ID. . . . . . . . . . . . . . . . .=',1I10/
     & 5X,'UNLOADING CURVE ID. . . . . . . . . . . . . . . .=',1I10/
     & 5X,'LOADING CURVE SCALE FACTOR. . . . . . . . . . . .=',1PG20.13/
     & 5X,'UNLOADING CURVE SCALE FACTOR. . . . . . . . . . .=',1PG20.13/
     & 5X,'RELOADING FLAG. . . . . . . . . . . . . . . . . .=',1I10/
     & 5X,'TRANSVERSE STIFFNESS SCALE FACTOR . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'YOUNG MODULUS OF COATING LAYER. . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO OF COATING LAYER. . . . . . . . . .=',1PG20.13/
     & 5X,'THICKNESS OF COATING LAYER. . . . . . . . . . . .=',1PG20.13/)
C-----------
      RETURN
      END
