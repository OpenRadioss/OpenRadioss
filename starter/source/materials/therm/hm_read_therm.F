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
Chd|  HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THERM(BUFMAT  ,BUFLEN     ,IPM     ,PM     ,
     .                         UNITAB  ,LSUBMODEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: BUFLEN
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(OUT)   :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(OUT)   :: PM
      my_real ,DIMENSION(BUFLEN)        , INTENT(INOUT) :: BUFMAT
      TYPE (UNIT_TYPE_) ,INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA) ,DIMENSION(*) , INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ITH,IFORM,FLAGMAT,FLAGUNIT,IUNIT,UID,MAT_ID,IMAT,ILAW,
     .   ALE,LAG,EUL,JALE,JTHE,JTUR,IAD_THERM,NTHERM
      INTEGER ,DIMENSION(NUMMAT) :: ITHERM_FOR
      my_real ::  UPWM,UPWO,T0,T1,RHO_CP,AS,BS,AL,BL,RHO_CPM1,EFRAC
      my_real ::  RCP_UNIT,AS_UNIT
      CHARACTER TITR*nchartitle,KEY*80
      LOGICAL IS_AVAILABLE
C=======================================================================
      ITHERM_FE = 0      ! com01_c.inc
      ITHERM_FOR(:) = 0
c--------------------------------------------------
c     COUNT EOS MODELS USING CFG FILES
c--------------------------------------------------
c
      CALL HM_OPTION_COUNT('/HEAT',NTHERM)
c
c--------------------------------------------------
c     START BROWSING EOS MODELS
c--------------------------------------------------
c
      CALL HM_OPTION_START('/HEAT')
c
c--------------------------------------------------
      DO ITH = 1,NTHERM
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = MAT_ID ,
     .                          OPTION_TITR = TITR   ,
     .                          UNIT_ID     = UID    ,
     .                          KEYWORD2    = KEY    )
c--------------------------------------------------
c       Check MAT_Id
c--------------------------------------------------
        FLAGMAT  = 0
        DO IMAT=1,NUMMAT-1
          IF (MAT_ID == IPM(1,IMAT)) THEN
            FLAGMAT = 1
            CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
            EXIT
          ENDIF
        ENDDO
        IF (MAT_ID > 0 .AND. FLAGMAT == 0) THEN
          CALL ANCMSG(MSGID=1663,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= MAT_ID, C1='HEAT/MAT', C2='HEAT/MAT', C3='')
          CYCLE
        ENDIF
c--------------------------------------------------
c       Check Unit_ID
c--------------------------------------------------
        FLAGUNIT = 0
        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            FLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID > 0 .AND. FLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     .                I1= MAT_ID,
     .                I2= UID,
     .                C1='HEAT/MAT',
     .                C2='HEAT/MAT',
     .                C3= '')
        ENDIF
c--------------------------------------------------
c
        CALL HM_GET_FLOATV('HEAT_T0'      ,T0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_RHocp'   ,RHO_CP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_AS'      ,AS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_BS'      ,BS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('HEAT_Iform'   ,IFORM    ,IS_AVAILABLE, LSUBMODEL)
c
        CALL HM_GET_FLOATV('HEAT_T1'      ,T1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_AL'      ,AL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_BL'      ,BL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_EFRAC'   ,EFRAC    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c        debug Unit conversion
c        CALL HM_GET_FLOATV_DIM('HEAT_RHocp' ,RCP_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c        CALL HM_GET_FLOATV_DIM('HEAT_AS' ,AS_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c        print*,'RCP_UNIT,AS_UNIT=', RCP_UNIT,AS_UNIT
c--------------------------------------------------
        ILAW = IPM(2,IMAT)
        PM(71,IMAT) = ONEP1
        PM(69,IMAT) = RHO_CP
        PM(75,IMAT) = AS
        PM(76,IMAT) = BS
        PM(77,IMAT) = AL
        PM(78,IMAT) = BL
        PM(79,IMAT) = T0
c
c--------------------------------------------------
        IF (ILAW == 2)THEN
          IF ((PM(54,IMAT) /= PM(79,IMAT)).AND.(NINT(PM(50,IMAT)) == 1)) THEN
            CALL ANCMSG(MSGID=764, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID, C1=TITR, I2=MAT_ID, I3=MAT_ID)
          END IF
          IF (RHO_CP <= ZERO) THEN
              RHO_CPM1 = ZERO
          ELSE
              RHO_CPM1 = ONE/RHO_CP
          ENDIF
          IF (PM(53,IMAT) /= RHO_CPM1) THEN
            CALL ANCMSG(MSGID=765, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID, C3=TITR, I2=MAT_ID, I3=MAT_ID)
          ENDIF
c
        ELSEIF (ILAW == 73)THEN
          IAD_THERM = IPM(7,IMAT)-1
          IF (BUFMAT(IAD_THERM+20) /= PM(79,IMAT) ) THEN
            CALL ANCMSG(MSGID=764, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID, C1=TITR, I2=MAT_ID, I3=MAT_ID)
          ENDIF
          IF (RHO_CP == ZERO) THEN
              RHO_CPM1 = ZERO
          ELSE
              RHO_CPM1 = ONE/RHO_CP
          ENDIF
          IF (BUFMAT(IAD_THERM+21) /= RHO_CPM1) THEN
            CALL ANCMSG(MSGID=765, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID, C1=TITR, I2=MAT_ID, I3=MAT_ID)
          ENDIF
        ENDIF
c-------
c       Check at least if the corresponding material is indeed lagrangian !!
        LAG = 0
        EUL = 0
        ALE = 0
        JALE = NINT(PM(72,IMAT))
        IF (JALE == 0 .AND. ILAW/=18 .AND. ILAW/=11) THEN
           ILAG= 1
           LAG = 1
        ELSEIF(JALE == 1)THEN
           IALE= 1
           ALE = 1
        ELSEIF(JALE == 2)THEN
           IEULER= 1
           EUL   = 1
        ELSEIF(JALE == 3) THEN
           ILAG= 1
           LAG = 1
        ENDIF
c
        IF (IFORM /= 0 .AND. LAG == 1) THEN
           PM(71,IMAT) = -ONEP1
        ELSE
           IFORM = 0
           CALL ANCMSG(MSGID=1609, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .          I1=MAT_ID, C1="WARNING", C2=TITR,
     .          C3="IFORM = 1 IS INCOMPATIBLE WITH /ALE AND /EULER MATERIALS, IT WILL BE IGNORED")
        ENDIF
c
        ITHERM_FOR(IMAT) = IFORM
        IF (RHO_CP == ZERO .AND. LAG == 0) THEN
           CALL ANCMSG(MSGID=1609, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .          I1=MAT_ID, C1="ERROR", C2=TITR,
     .          C3="RHO_0 Cp PARAMETER MUST BE GREATER THAN ZERO")
        ENDIF

c
        IF (T0 == ZERO)    T0 = PM(23,IMAT) / RHO_CP
        IF (T1 == ZERO)    T1 = EP20
        IF (EFRAC < ZERO)  EFRAC = ZERO			
        IF (EFRAC > ONE )   EFRAC = ONE
        IF (EFRAC == ZERO) EFRAC = ONE
        PM (80,IMAT) = T1
        PM (90,IMAT) = EFRAC			
c
        WRITE(IOUT,2000) MAT_ID,T0,RHO_CP,AS,BS,T1,AL,BL,IFORM,EFRAC
c
      ENDDO   ! I = 1,NTHERM
c-----------------------------------------
c
      DO IMAT=1,NUMMAT-1
        ILAW = IPM(2,IMAT)
        JTUR = NINT(PM(70,IMAT))
        JTHE = ABS(NINT(PM(71,IMAT)))
        JALE = NINT(PM(72,IMAT))
        IF (JALE == 0 .AND. ILAW/=18 .AND. ILAW/=11) THEN
          ILAG=1
        ELSEIF(JALE == 1)THEN
          IALE=1
        ELSEIF(JALE == 2)THEN
          IEULER=1
        ELSEIF(JALE == 3) THEN
          ILAG=1
        ENDIF
        IF (JALE /= 0) THEN
          PM(15,IMAT) = UPWMG
          PM(16,IMAT) = UPWOG
        ENDIF
        IF (ITHERM_FOR(IMAT) == 0) ITHERM    = MAX(ITHERM,JTHE)
        IF (ITHERM_FOR(IMAT) /= 0) ITHERM_FE = MAX(ITHERM_FE,JTHE)
        IF (ILAW /= 50)         ITURB = MAX(ITURB ,JTUR)
      ENDDO
c-----------------------------------------
c     ERROR IF MULTIMATERIAL IS LAGRANGIAN.
c-----------------------------------------
      DO IMAT=1,NUMMAT-1
        ILAW = IPM(2,IMAT)
       	JALE = NINT(PM(72,IMAT))   	
	       IF (JALE == 0) THEN
          IF (ILAW == 20 .OR. ILAW == 37 .OR. ILAW == 51 .OR. ILAW == 151) THEN
            CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
            MAT_ID = IPM(1,IMAT)
            CALL ANCMSG(MSGID=101, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                  I1=MAT_ID, C1=TITR, I2=ILAW)
          ENDIF
        ENDIF
      ENDDO
c-----------------------------------------
 2000 FORMAT(/
     & 5X,'    THERMAL MATERIAL  ',/,
     & 5X,'    ----------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . =',I10/,
     & 5X,'T0. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'SPECIFIC HEAT . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'AS. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BS. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'T1. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'AL. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BL. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'HEAT TRANSFER FORMULATION . . . . . . . . . . = ',I10/,
     & 10X,'    0: it is based on Finite Volume  Method',/
     & 10X,'    1: it is based on Finite Element Method '/
     & 5X,'FRACTION OF STRAIN ENERGY CONVERTED INTO HEAT =',1PG20.13/)
c-----------------------------------------
      RETURN
      END
