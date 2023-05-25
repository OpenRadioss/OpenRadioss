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
Chd|  HM_READ_PROP27                source/properties/spring/hm_read_prop27.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP27(GEO      ,IGEO     ,PROP_TAG ,IGTYP    ,IG       ,
     .                          UNITAB   ,LSUBMODEL)
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "tablen_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)           :: UNITAB 
      INTEGER  IGEO(NPROPGI),IGTYP,IG
C     REAL
      my_real
     .   GEO(NPROPG)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)         :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC,IFUNC2,ISENS,IFL,IFAIL,J,ILENG,ITENS,Fsmooth
C     REAL
      my_real
     .   MASS,STIFF,DAMP,GAP,MIN_RUP,MAX_RUP,
     .   ASCALE1,ASCALE2,FSCALE1,FSCALE2,
     .   ASCALE1_UNIT,ASCALE2_UNIT,FSCALE1_UNIT,
     .   FSCALE2_UNIT,NEXP,FCUT 
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
C
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
C     1st card
      CALL HM_GET_FLOATV('MASS'    ,MASS     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV('ISENSOR'   ,ISENS    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISFLAG'    ,IFL      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ileng'     ,ILENG    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Itens'     ,ITENS    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifail'     ,IFAIL    ,IS_AVAILABLE,LSUBMODEL)
C     2nd card
      CALL HM_GET_FLOATV('STIFF'   ,STIFF    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP'    ,DAMP     ,IS_AVAILABLE,LSUBMODEL,UNITAB)   
      CALL HM_GET_FLOATV('NEXP'    ,NEXP     ,IS_AVAILABLE,LSUBMODEL,UNITAB)    
      CALL HM_GET_FLOATV('MIN_RUP' ,MIN_RUP  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP' ,MAX_RUP  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C     3rd card
      CALL HM_GET_FLOATV('GAP'     ,GAP      ,IS_AVAILABLE,LSUBMODEL,UNITAB)    
      CALL HM_GET_INTV('FSMOOTH'   ,Fsmooth  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('FCUT'    ,FCUT     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C     4th card 
      CALL HM_GET_INTV('FUN1'      ,IFUNC    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN2'      ,IFUNC2   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('ASCALE1' ,ASCALE1  ,IS_AVAILABLE,LSUBMODEL,UNITAB)   
      IF (ASCALE1 == ZERO) THEN 
        CALL HM_GET_FLOATV_DIM('ASCALE1' ,ASCALE1_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ASCALE1 = ONE * ASCALE1_UNIT
      ENDIF
      CALL HM_GET_FLOATV('FSCALE1' ,FSCALE1  ,IS_AVAILABLE,LSUBMODEL,UNITAB)    
      IF (FSCALE1 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('FSCALE1' ,FSCALE1_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        FSCALE1 = ONE * FSCALE1_UNIT
      ENDIF
      CALL HM_GET_FLOATV('ASCALE2' ,ASCALE2  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (ASCALE2 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('ASCALE2' ,ASCALE2_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ASCALE2 = ONE * ASCALE2_UNIT
      ENDIF
      CALL HM_GET_FLOATV('FSCALE2' ,FSCALE2  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (FSCALE2 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('FSCALE2' ,FSCALE2_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        FSCALE2 = ONE * FSCALE2_UNIT
      ENDIF
C
C-----------------------------
C       CHECKING VALUES
C-----------------------------
C
      ! Checking negative values of MIN_RUP and Gap
      MIN_RUP = -ABS(MIN_RUP)
      GAP     = -ABS(GAP)
      IF (GAP < ZERO)      ITENS   = 0
      ! Non-linear stiffness exponent
      IF (NEXP <= ZERO)    NEXP    = ONE
      ! Spring force filtering flag
      IF (Fsmooth /= 0)    Fsmooth = 1
      ! If functions are activated
      IF (IFUNC  /= 0)     STIFF   = ZERO
      IF (IFUNC2 /= 0)     DAMP    = ZERO
      ! Sensor flag
      IF (IFL == 1)        ISENS   = -ISENS
      ! Default failure limits
      IF (MIN_RUP == ZERO) MIN_RUP = -INFINITY
      IF (MAX_RUP == ZERO) MAX_RUP = INFINITY
      ! Filtering frequency
      IF (FCUT /= ZERO) THEN
        ! If a filtering frequency is given by the user
        Fsmooth = 1
      ELSE
        ! If no filtering frequency is given but the flag is activated
        IF (Fsmooth /= 0) THEN
          FCUT  = 100000.0D0*FAC_T_WORK  
        ! If no filtering frequency and no flag is activated => no filtering
        ELSE
          FCUT  = ZERO
        ENDIF
      ENDIF
C      
C-----------------------------
C       PROPERTY TABLES
C-----------------------------
      ! Filling IGEO table
      IGEO(1)   = IG
      IGEO(3)   = ISENS
      IGEO(11)  = IGTYP
      IGEO(101) = IFUNC                  
      IGEO(102) = IFUNC2
C
      ! Filling GEO table
      GEO(1)   = MASS
      GEO(2)   = STIFF
      GEO(3)   = DAMP
      GEO(5)   = EP06
      GEO(7)   = ZERO
      GEO(8)   = ONEP1 
      GEO(10)  = FSCALE1
      GEO(12)  = IGTYP+EM01
      GEO(15)  = MIN_RUP                          
      GEO(16)  = MAX_RUP
      GEO(18)  = ASCALE2
      GEO(19)  = GAP       
      GEO(25)  = ZERO ! NUVAR - Number of user variables
      GEO(39)  = ASCALE1
      GEO(43)  = IFAIL
      GEO(80)  = IFL
      GEO(93)  = ILENG
      GEO(132) = FSCALE2
      GEO(133) = ITENS
      GEO(134) = NEXP
      GEO(135) = Fsmooth
      GEO(136) = FCUT
C
C----------------------
C FROM LECGEO - GENERAL
C----------------------
      IF (GEO(171) /= ZERO .AND. IGEO(10) == 0) IGEO(10) = NINT(GEO(171))
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
      PROP_TAG(IGTYP)%G_EINT        = 1
      PROP_TAG(IGTYP)%G_FOR         = 1
      PROP_TAG(IGTYP)%G_LENGTH      = 1  ! X0 (AL0) - total length
      PROP_TAG(IGTYP)%G_TOTDEPL     = 1  ! DX - total deformation (translation)
      PROP_TAG(IGTYP)%G_DEP_IN_TENS = 1  ! DPX  (DPY,DPZ) - max displacement in tension
      PROP_TAG(IGTYP)%G_DEP_IN_COMP = 1  ! DPX2 (DPY2,DPZ2) - max displacement in compression
      PROP_TAG(IGTYP)%G_POSX        = 5
      PROP_TAG(IGTYP)%G_LENGTH_ERR  = 1
      PROP_TAG(IGTYP)%G_NUVAR       = MAX(PROP_TAG(IGTYP)%G_NUVAR,NINT(GEO(25))) ! additional internal variables for h=6
      PROP_TAG(IGTYP)%G_DEFINI      = 1
      PROP_TAG(IGTYP)%G_FORINI      = 1
      PROP_TAG(IGTYP)%G_RUPTCRIT = 1
C
C-----------------------------
C       PRINTING OUT DATA
C-----------------------------
      WRITE(IOUT,2000)
      IF (.NOT. IS_ENCRYPTED) THEN
        ! Property and spring mass
        IF (ILENG == 0) THEN
          WRITE(IOUT,1400) IG,MASS
        ELSE
          WRITE(IOUT,1410) IG,MASS
        ENDIF
        ! Spring stiffness
        !  -> Linear
        IF (IFUNC == 0) THEN 
          IF (ILENG == 0) THEN 
            WRITE(IOUT,1450) STIFF,NEXP
          ELSE
            WRITE(IOUT,1460) STIFF,NEXP
          ENDIF
        !  -> Non-linear tabulated
        ELSE
          IF (ILENG == 0) THEN 
            WRITE(IOUT,1500) IFUNC,ASCALE1,FSCALE1
          ELSE
            WRITE(IOUT,1510) IFUNC,ASCALE1,FSCALE1
          ENDIF
        ENDIF
        ! Spring damping
        !  -> Linear
        IF (IFUNC2 == 0) THEN
          IF (ILENG == 0) THEN 
            WRITE(IOUT,1550) DAMP
          ELSE
            WRITE(IOUT,1560) DAMP
          ENDIF
        !  -> Non-linear tabulated
        ELSE
          IF (ILENG == 0) THEN 
            WRITE(IOUT,1600) IFUNC2,ASCALE2,FSCALE2
          ELSE
            WRITE(IOUT,1610) IFUNC2,ASCALE2,FSCALE2
          ENDIF
        ENDIF
        ! Gap for activation
        IF (GAP /= ZERO) THEN 
          IF (ILENG == 0) THEN 
            WRITE(IOUT,1650) GAP
          ELSE
            WRITE(IOUT,1660) GAP
          ENDIF
        ENDIF
        ! Unit length flag
        WRITE(IOUT,1700) ILENG
        ! Tensile behavior flag
        WRITE(IOUT,1800) ITENS
        ! Velocity filtering
        WRITE(IOUT,1900) Fsmooth,FCUT
        ! Failure criterion
        IF (IFAIL /= 0) THEN 
          IF (ILENG == 0) THEN 
            WRITE(IOUT,1750) IFAIL,MIN_RUP,MAX_RUP
          ELSE
            WRITE(IOUT,1760) IFAIL,MIN_RUP,MAX_RUP
          ENDIF
        ENDIF
      ELSE
        WRITE(IOUT,1000) IG
      ENDIF
C
C-------------------------------------------------------------------------
2000  FORMAT(
     & 5X,'------------------------------------------------------'/,
     & 5X,'         BOUNDED DAMPER SPRING PROPERTY SET           '/,
     & 5X,'------------------------------------------------------'/)
1000  FORMAT(
     & 5X,'PROPERTY SET NUMBER  . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
1400  FORMAT(
     & 5X,'PROPERTY SET NUMBER  . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS  . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
1410  FORMAT(
     & 5X,'PROPERTY SET NUMBER  . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'SPRING LINEIC MASS . . . . . . . . . . . . . . . . . .=',1PG20.13/)
1450  FORMAT(
     & 5X,'LINEAR SPRING STIFFNESS  . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'NON-LINEAR EXPONENT  . . . . . . . . . . . . . . . . .=',1PG20.13/)
1460  FORMAT(
     & 5X,'LINEAR SPRING STIFFNESS PER UNIT LENGTH. . . . . . . .=',1PG20.13/,
     & 5X,'NON-LINEAR EXPONENT  . . . . . . . . . . . . . . . . .=',1PG20.13/)
1500  FORMAT(
     & 5X,'NON LINEAR STIFFNESS FORCE FUNCTION ID . . . . . . . .=',I10/,
     & 5X,'ELONGATION SCALE FACTOR FOR STIFFNESS FUNCTION . . . .=',1PG20.13/,
     & 5X,'FORCE SCALE FACTOR FOR STIFFNESS FUNCTION  . . . . . .=',1PG20.13/)
1510  FORMAT(
     & 5X,'NON LINEAR STIFFNESS FORCE FUNCTION ID . . . . . . . .=',I10/,
     & 5X,'STRAIN SCALE FACTOR FOR STIFFNESS FUNCTION . . . . . .=',1PG20.13/,
     & 5X,'FORCE SCALE FACTOR FOR STIFFNESS FUNCTION  . . . . . .=',1PG20.13/)
1550  FORMAT(
     & 5X,'LINEAR DAMPING COEFFICIENT . . . . . . . . . . . . . .=',1PG20.13/)
1560  FORMAT(
     & 5X,'LINEAR DAMPING COEFFICIENT PER UNIT LENGTH . . . . . .=',1PG20.13/)
1600  FORMAT(
     & 5X,'NON LINEAR DAMPING FORCE FUNCTION ID . . . . . . . . .=',I10/,
     & 5X,'VELOCITY SCALE FACTOR FOR DAMPING FUNCTION . . . . . .=',1PG20.13/,
     & 5X,'FORCE SCALE FACTOR FOR DAMPING FUNCTION  . . . . . . .=',1PG20.13/)
1610  FORMAT(
     & 5X,'NON LINEAR DAMPING FORCE FUNCTION ID . . . . . . . . .=',I10/,
     & 5X,'STRAIN-RATE SCALE FACTOR FOR DAMPING FUNCTION  . . . .=',1PG20.13/,
     & 5X,'FORCE SCALE FACTOR FOR DAMPING FUNCTION  . . . . . . .=',1PG20.13/)
1650  FORMAT(
     & 5X,'MINIMUM COMPRESSION GAP (LENGTH) FOR ACTIVATION  . . .=',1PG20.13/)
1660  FORMAT(
     & 5X,'MINIMUM COMPRESSION GAP (STRAIN) FOR ACTIVATION  . . .=',1PG20.13/)
1700  FORMAT(
     & 5X,'UNIT LENGTH FLAG . . . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'  ILENG = 0 : INPUT VALUES WITH CLASSICAL UNITS        ',/,
     & 5X,'  ILENG = 1 : INPUT VALUES PER UNIT LENGTH             ',/)
1750  FORMAT(
     & 5X,'FAILURE FLAG IFAIL . . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'  IFAIL = 1 : DISPLACEMENT FAILURE CRITERION           ',/,
     & 5X,'  IFAIL = 2 : FORCE FAILURE CRITERION                  ',/,
     & 5X,'NEGATIVE FAILURE LIMIT . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE LIMIT (IF ITENS = 1). . . . . . . . .=',1PG20.13/)
1760  FORMAT(
     & 5X,'FAILURE FLAG IFAIL . . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'  IFAIL = 1 : STRAIN FAILURE CRITERION                 ',/,
     & 5X,'  IFAIL = 2 : FORCE FAILURE CRITERION                  ',/,
     & 5X,'NEGATIVE FAILURE LIMIT . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE LIMIT (IF ITENS = 1). . . . . . . . .=',1PG20.13/)
1800  FORMAT(
     & 5X,'TENSILE BEHAVIOR FLAG  . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'  ITENS = 0 : NO STIFFNESS AND DAMPING IN TENSION      ',/,
     & 5X,'  ITENS = 1 : STIFFNESS AND DAMPING FOR TENSION        ',/)
1900  FORMAT(
     & 5X,'SPRING FORCE FILTERING FLAG  . . . . . . . . . . . . .=',I10/,
     & 5X,'  FSMOOTH = 0 : NO FILTERING                           ',/,
     & 5X,'  FSMOOTH = 1 : FILTERING ACTIVATED                    ',/,
     & 5X,'CUTOFF FREQUENCY . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
c-----------
      END
