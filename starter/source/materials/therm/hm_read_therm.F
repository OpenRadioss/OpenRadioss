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
!||    hm_read_therm          ../starter/source/materials/therm/hm_read_therm.F
!||--- called by ------------------------------------------------------
!||    read_material_models   ../starter/source/materials/read_material_models.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_THERM(MAT_PARAM   ,BUFMAT  ,BUFLEN     ,IPM     ,PM     ,
     .                         UNITAB  ,LSUBMODEL  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Material Parameters (hm_read_therm.F)
C  PM(71) = JTHE is a material flag.  1: thermal parameters are input    0: no thermal parameter
C
C GLOBAL FLAGS (hm_read_part.F)
C  ITHERM_FE : 1 if at least one PART has thermal parameter and lagrangian framework
C  ITHERM    : 1 if at least one PART has thermal parameter and ale/euler framework
C
C GROUP FLAG (./source/element/....tails.F) IPARG(13) will be flag for thermics.
C  IN ALL CASES 
C    IPARG(13) :  0  if current group do not require temperature calculation
C
C   SOLIDS 
C    IPARG(13) : -1  if current group requires temperature calculation at nodes (FEM with Lagrange)
C    IPARG(13) : +1  if current group requires temperature calculation at centroids (FVM with ALE or Euler)
C
C   SHELLS  
C    IPARG(13) : +1  if current group requires temperature calculation at nodes (FEM with Lagrange)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE MATPARAM_DEF_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: BUFLEN
      INTEGER, DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT) :: IPM
      my_real, DIMENSION(NPROPM ,NUMMAT), INTENT(INOUT) :: PM
      my_real, DIMENSION(BUFLEN), INTENT(INOUT) :: BUFMAT
      TYPE (UNIT_TYPE_) ,INTENT(IN) :: UNITAB 
      TYPE(SUBMODEL_DATA)   ,DIMENSION(NSUBMOD) ,INTENT(IN)    :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,ITH,FLAGMAT,FLAGUNIT,IUNIT,UID,MAT_ID,IMAT,ILAW,ALE,
     .           LAG,EUL,JALE,JTUR,IAD_THERM,NTHERM,IFORM,LAW2_FORM
      my_real :: TINI,TMELT,RHO_CP,AS,BS,AL,BL,RHO_CPM1,EFRAC
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER :: KEY*80
      LOGICAL IS_AVAILABLE,IS_AVAILABLE_IFORM
C=======================================================================
c     COUNT /HEAT MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('/HEAT',NTHERM)
c
c--------------------------------------------------
c     START BROWSING /HEAT MODELS
c--------------------------------------------------
c
      CALL HM_OPTION_START('/HEAT')
c
c--------------------------------------------------
      DO ITH = 1,NTHERM
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=MAT_ID , OPTION_TITR=TITR , UNIT_ID=UID , KEYWORD2=KEY )
c--------------------------------------------------
c       Check MAT_Id
c--------------------------------------------------
        FLAGMAT  = 0
        ILAW     = 0
        IMAT     = 0
        DO I =1,NUMMAT-1
          IF (MAT_ID == MAT_PARAM(I)%MAT_ID) THEN
            FLAGMAT = 1
            IMAT    = I
            EXIT
          ENDIF
        ENDDO
        IF (MAT_ID > 0 .AND. FLAGMAT == 0) THEN
          CALL ANCMSG(MSGID=1663,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     &                I1= MAT_ID, C1='HEAT/MAT', C2='HEAT/MAT', C3='')
        ENDIF
        ILAW = MAT_PARAM(IMAT)%ILAW
c--------------------------------------------------
c       Check Unit_ID
c--------------------------------------------------
        FLAGUNIT = 0
        DO IUNIT=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            FLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID > 0 .AND. FLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     &                I1= MAT_ID,
     &                I2= UID,
     &                C1='HEAT/MAT',
     &                C2='HEAT/MAT',
     &                C3= '')                             
        ENDIF
c--------------------------------------------------
c       Reading user parameters
c--------------------------------------------------
        CALL HM_GET_FLOATV('HEAT_T0'      ,TINI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_RHocp'   ,RHO_CP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_AS'      ,AS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_BS'      ,BS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('HEAT_Iform'   ,IFORM    ,IS_AVAILABLE_IFORM, LSUBMODEL)
c
        CALL HM_GET_FLOATV('HEAT_T1'      ,TMELT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_AL'      ,AL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_BL'      ,BL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('HEAT_EFRAC'   ,EFRAC    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------
c       Specific Case : law2
c--------------------------------------------------
        IF (ILAW == 2) THEN
          LAW2_FORM = NINT(PM(50,IMAT))
          IF (TMELT /= PM(54,IMAT) .AND. LAW2_FORM == 1) THEN  ! zerilli
            CALL ANCMSG(MSGID=764, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     &                  I1=MAT_ID, C1=TITR, I2=MAT_ID, I3=MAT_ID)
          END IF
          IF (RHO_CP /= PM(69,IMAT)) THEN
            CALL ANCMSG(MSGID=765, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     &                  I1=MAT_ID, C3=TITR, I2=MAT_ID, I3=MAT_ID)
          ENDIF
c--------------------------------------------------
c       Specific Case : law73
c--------------------------------------------------
        ELSEIF (ILAW == 73)THEN
          IAD_THERM = IPM(7,IMAT)-1
          IF (BUFMAT(IAD_THERM+20) /= PM(79,IMAT) ) THEN
            CALL ANCMSG(MSGID=764, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     &                  I1=MAT_ID, C1=TITR, I2=MAT_ID, I3=MAT_ID)
          ENDIF
          IF (RHO_CP == ZERO) THEN
              RHO_CPM1 = ZERO
          ELSE
              RHO_CPM1 = ONE/RHO_CP
          ENDIF
          IF (BUFMAT(IAD_THERM+21) /= RHO_CPM1) THEN
            CALL ANCMSG(MSGID=765, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     &                  I1=MAT_ID, C1=TITR, I2=MAT_ID, I3=MAT_ID)
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
        IF(IS_AVAILABLE_IFORM)THEN        
          IF(IFORM /=0 .AND. LAG == 0)THEN
            IFORM = 0
            CALL ANCMSG(MSGID=1609, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     &           I1=MAT_ID, C1="WARNING", C2=TITR,
     &           C3="IFORM = 1 IS INCOMPATIBLE WITH /ALE AND /EULER MATERIALS, IT WILL BE IGNORED")
          ENDIF          
        ENDIF
c
        IF (RHO_CP == ZERO .AND. LAG == 0) THEN
           CALL ANCMSG(MSGID=1609, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     &          I1=MAT_ID, C1="ERROR", C2=TITR,
     &          C3="RHO_0 Cp PARAMETER MUST BE GREATER THAN ZERO")
        ENDIF       
c--------------------------------------------------
c       Default Values
c--------------------------------------------------
        IF (TINI  == ZERO) TINI  = PM(23,IMAT) / MAX(EM20,RHO_CP)     ! E0 / rho_cp        
        IF (TINI  == ZERO) TINI  = THREE100 
        IF (TMELT == ZERO) TMELT = EP20                              
        IF (EFRAC < ZERO)  EFRAC = ZERO        
        IF (EFRAC > ONE )  EFRAC = ONE
        IF (EFRAC == ZERO) EFRAC = ONE
        
        PM(71,IMAT) = ONEP1       ! jthe
        PM(69,IMAT) = RHO_CP                                            
        PM(75,IMAT) = AS                                             
        PM(76,IMAT) = BS                                             
        PM(77,IMAT) = AL                                             
        PM(78,IMAT) = BL                                             
        PM(79,IMAT) = TINI                                             
        PM(80,IMAT) = TMELT
        PM(90,IMAT) = EFRAC
        
        MAT_PARAM(IMAT)%ITHERM      = 1         ! setting jthe=1
        MAT_PARAM(IMAT)%THERM%RHOCP = RHO_CP
        MAT_PARAM(IMAT)%THERM%TINI  = TINI
        MAT_PARAM(IMAT)%THERM%TMELT = TMELT
        MAT_PARAM(IMAT)%THERM%AS    = AS
        MAT_PARAM(IMAT)%THERM%BS    = BS
        MAT_PARAM(IMAT)%THERM%AL    = AL
        MAT_PARAM(IMAT)%THERM%BL    = BL
        MAT_PARAM(IMAT)%THERM%EFRAC = EFRAC
c--------------------------------------------------
c       Output
c--------------------------------------------------
        WRITE(IOUT,2000) MAT_ID,TINI,TMELT,RHO_CP,AS,BS,AL,BL,EFRAC
C        
      ENDDO   ! I = 1,NTHERM     

c-----------------------------------------      
 2000 FORMAT(/
     & 5X,'    THERMAL PARAMETERS  ',/,
     & 5X,'    ------------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . =',I10/,
     & 5X,'T0 (INITIAL TEMPERATURE). . . . . . . . . . . =',1PG20.13/,
     & 5X,'TMELT (MELTING TEMPERATURE) . . . . . . . . . =',1PG20.13/,
     & 5X,'SPECIFIC HEAT . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'AS (SOLID PHASE). . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BS (SOLID PHASE). . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'AL (LIQUID PHASE) . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BL (LIQUID PHASE) . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'FRACTION OF STRAIN ENERGY CONVERTED INTO HEAT =',1PG20.13/)
c-----------------------------------------      
      RETURN
      END
