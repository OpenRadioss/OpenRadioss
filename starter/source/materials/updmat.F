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
Chd|  UPDMAT                        source/materials/updmat.F     
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        LAW100_UPD_AB                 source/materials/mat/mat100/law100_upd.F
Chd|        LAW100_UPD_NHT                source/materials/mat/mat100/law100_upd.F
Chd|        LAW104_UPD                    source/materials/mat/mat104/law104_upd.F
Chd|        LAW108_UPD                    source/materials/mat/mat108/law108_upd.F
Chd|        LAW111_UPD                    source/materials/mat/mat111/law111_upd.F
Chd|        LAW112_UPD                    source/materials/mat/mat112/law112_upd.F
Chd|        LAW113_UPD                    source/materials/mat/mat113/law113_upd.F
Chd|        LAW114_UPD                    source/materials/mat/mat114/law114_upd.F
Chd|        LAW119_UPD                    source/materials/mat/mat119/law119_upd.F
Chd|        LAW120_UPD                    source/materials/mat/mat120/law120_upd.F
Chd|        LAW158_UPD                    source/materials/mat/mat158/law158_upd.F
Chd|        LAW190_UPD                    source/materials/mat/mat190/law190_upd.F
Chd|        LAW36_UPD                     source/materials/mat/mat036/law36_upd.F
Chd|        LAW42_UPD                     source/materials/mat/mat042/law42_upd.F
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        LAW62_UPD                     source/materials/mat/mat062/law62_upd.F
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|        LAW70_UPD                     source/materials/mat/mat070/law70_upd.F
Chd|        LAW76_UPD                     source/materials/mat/mat076/law76_upd.F
Chd|        LAW77_UPD                     source/materials/mat/mat077/law77_upd.F
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|        LAW90_UPD                     source/materials/mat/mat090/law90_upd.F
Chd|        LAW92_UPD                     source/materials/mat/mat092/law92_upd.F
Chd|        MATFUN_USR2SYS                source/materials/tools/matfun_usr2sys.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        LAW190_UPD_MOD                source/materials/mat/mat190/law190_upd.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|        VISC_PARAM_MOD                ../common_source/modules/mat_elem/visc_param_mod.F
Chd|====================================================================
      SUBROUTINE UPDMAT(BUFMAT   ,PM    ,IPM       ,TABLE    ,FUNC_ID  ,
     .                  NPC      ,PLD   ,SENSORS   ,NLOC_DMG ,MLAW_TAG ,
     .                  MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE NLOCAL_REG_MOD
      USE ELBUFTAG_MOD  
      USE MESSAGE_MOD   
      USE MATPARAM_DEF_MOD
      USE SENSOR_MOD
      USE LAW190_UPD_MOD  
      USE VISC_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(SNPC), FUNC_ID(*)
      INTEGER, DIMENSION(NPROPMI,NUMMAT) ,INTENT(INOUT) :: IPM
      my_real PM(NPROPM,NUMMAT), PLD(STF),BUFMAT(SBUFMAT)
      TARGET IPM,BUFMAT
      TYPE(MLAW_TAG_), TARGET, DIMENSION(NUMMAT)  :: MLAW_TAG
      TYPE(TTABLE), DIMENSION(NTABLE) ,INTENT(INOUT) ::  TABLE
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(INOUT), TARGET :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,IMAT,ILAW,IRUP,MAT_ID,IADD,IFC,FLAG_HE,NFUNC,NFUNCL,NUMTABL,
     .        NPARAM,NUPARF,NUM_FAIL,IR,FLAG_FIT,IFAILG,NORDRE,IALGO,
     .        IADD_VISC , NPARAM_VISC,IVISC,NPRONY,J,ITYPE_VISC,FORM
      my_real :: GAMA_INF,EMAX
      INTEGER, DIMENSION(NUMMAT)     :: MULLINS,GURSON
      INTEGER, DIMENSION(MAXTAB)     :: TABLE_ID
      INTEGER, DIMENSION(:), POINTER :: IFUNC,ITABLE
      my_real, DIMENSION(:), POINTER :: UPARAM,UPARF,UPARAM_VISC
      TYPE(MLAW_TAG_), POINTER :: MTAG
      CHARACTER*nchartitle, TITR
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
      TYPE (VISC_PARAM_)     , POINTER :: VISC
C=======================================================================
c     TRAITEMENT OF MATERIAL PARAMETERS AND FUNCTIONS
c-----------------------------------------------------------------------

      MULLINS(1:NUMMAT) = 0
      GURSON (1:NUMMAT) = 0
      DO IMAT=1,NUMMAT
        ILAW     = NINT(PM(19,IMAT))
        NUM_FAIL = IPM(220,IMAT)
        IF (NUM_FAIL > 0) THEN
          MTAG => MLAW_TAG(IMAT)
          DO IR = 1,NUM_FAIL
            IRUP = IPM(111 + 15*(IR-1),IMAT)
            IF (IRUP == 33 .AND. ILAW /=100 .AND. ILAW /=95 .AND. ILAW/=92) THEN
                CALL ANCMSG(MSGID=1769, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .               I1=IMAT,
     .               I2=ILAW)
            ENDIF
            IF (IRUP == 33 .AND. (ILAW ==100 .OR. ILAW ==95 .OR. ILAW==92)) THEN
              MULLINS(IMAT) = IR
            ELSEIF (IRUP == 35 .and. ILAW==104) THEN
              IF (GURSON(IMAT) == 0) THEN
                GURSON(IMAT) = IR
              ELSE
                ! Error message
                CALL ANCMSG(MSGID=1650,MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,I1=IMAT)
              ENDIF
            ELSE IF (IRUP == 14) THEN  ! Hashin
              MTAG%G_DMG = 1
            ELSE IF (IRUP == 28) THEN  ! Alter
              MTAG%G_DMG = 2
            ! -> Isotropic stress softening 
            ELSE IF (IRUP == 10 .OR. IRUP == 11 .OR. IRUP == 41 .OR. IRUP == 42 .OR.
     .               IRUP == 44 .OR. IRUP == 45 .OR. IRUP == 46 .OR. IRUP == 47 .OR. 
     .               IRUP == 13 ) THEN             
              MTAG%L_DMGSCL = 1
              MTAG%G_DMGSCL = 1
            ! -> Orthotropic stress softening
            ELSE IF (IRUP == 48) THEN 
              MTAG%L_DMGSCL = 6
            ENDIF
          ENDDO
        ENDIF
      ENDDO
c-----------------------------------------------------------------------


      DO IMAT=1,NUMMAT
C
        MAT_ID  = IPM(1,IMAT)
        ILAW    = NINT(PM(19,IMAT))
c
        IF (ILAW < 29.or.ILAW==32.or.ILAW==43.or.ILAW==49.or.ILAW==52.or.
     .      ILAW==57.or.ILAW==59.or.ILAW==60.or.ILAW==65.or.ILAW==75.or.
     .      ILAW==77.or.ILAW==78 ) CYCLE
c
        CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
c
        IADD     = MAX(1,IPM(7,IMAT))
        NPARAM   = MIN(IPM(9,IMAT),SBUFMAT)
        UPARAM   => BUFMAT(IADD:IADD+NPARAM-1) 
        MATPARAM => MAT_PARAM(IMAT)
        !/visc/prony
        ITYPE_VISC = MATPARAM%IVISC
        GAMA_INF = ONE
        IF(ITYPE_VISC == 2) THEN
          VISC  => MATPARAM%VISC  
          NPRONY = VISC%IPARAM(1)
          FORM   = VISC%IPARAM(2)
          IF(FORM == 1) THEN
            GAMA_INF = ONE 
            DO J=1,NPRONY
              GAMA_INF = GAMA_INF - VISC%UPARAM(J)
            ENDDO
            GAMA_INF = MAX(EM20, GAMA_INF)
          ENDIF  
        ENDIF
        GAMA_INF = MIN(GAMA_INF, ONE)
c
c-----------------------------------------------------------------------
c       Replace Func ID -> function system number in IFUNC table
c-----------------------------------------------------------------------
c
        NFUNC   =  IPM(10 ,IMAT)
        NUMTABL =  IPM(226,IMAT)
        IFUNC   => IPM(10+1:10+NFUNC,IMAT)
        ITABLE  => IPM(226+1:226+NUMTABL,IMAT)
c        
        IF (NFUNC > 0 ) THEN 
          CALL MATFUN_USR2SYS(TITR,MAT_ID,NFUNC,IFUNC,FUNC_ID  )
        ENDIF
        IF (ILAW /= 73 .and. ILAW /= 74 .and. ILAW /= 80) THEN
          IF (NUMTABL > 0 ) THEN
            TABLE_ID(1:NUMTABL) = IPM(226+1:226+NUMTABL,IMAT)
            CALL MATTAB_USR2SYS(TITR,MAT_ID,TABLE,NUMTABL,ITABLE)
          ENDIF 
        ENDIF 
c
c-----------------------------------------------------------------------
        SELECT CASE (ILAW)
c-------------------------
          CASE (36) 
c------------------
           MTAG => MLAW_TAG(IMAT)
           CALL LAW36_UPD(IOUT   ,TITR   ,MAT_ID ,NPARAM,UPARAM ,
     .                     NFUNC  ,IFUNC  ,FUNC_ID,NPC    ,PLD  ,
     .                     MTAG  )        
c-------------------------
          CASE (42) 
c------------------
            CALL LAW42_UPD(IOUT,TITR,MAT_ID,UPARAM,PM(1,IMAT),GAMA_INF)        
c-------------------------
          CASE (58) 
c------------------
c
            NFUNCL =  IPM(6,IMAT)
            IFUNC  => IPM(10+1:10+NFUNC+NFUNCL,IMAT)
            CALL LAW58_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,NFUNCL,IFUNC,MAT_ID,FUNC_ID,
     .                     PM(1,IMAT),NPARAM,SENSORS)
c-------------------------
          CASE (62) 
c------------------
            CALL LAW62_UPD(IOUT,TITR,MAT_ID,NPARAM,UPARAM, PM(1,IMAT),GAMA_INF) 
c------------------
          CASE (69) 
c------------------
           CALL LAW69_UPD(IOUT, TITR    , MAT_ID, UPARAM, NFUNC, 
     .                    IFUNC, FUNC_ID , NPC   , PLD , 
     .                    PM(1,IMAT),IPM(1,IMAT),GAMA_INF)
c          
c------------------
          CASE (70) 
c------------------
            CALL LAW70_UPD(MATPARAM ,TITR     ,MAT_ID   ,NPARAM   ,UPARAM   ,
     .                     NFUNC    ,IFUNC    ,NPC      ,PLD      ,EMAX     ,
     .                     IOUT     )       
            PM(24,IMAT) = EMAX
c          
c------------------
          CASE (76) 
c------------------
            CALL LAW76_UPD(IOUT   ,TITR    ,MAT_ID   ,NPARAM ,MATPARAM ,
     .                     UPARAM ,NUMTABL ,ITABLE   ,TABLE   ,NFUNC    ,
     .                     IFUNC  ,NPC     ,PLD      )       
c
c------------------
          CASE (77) 
c------------------
            CALL LAW77_UPD(TITR    ,MAT_ID   ,NPARAM ,MATPARAM ,
     .                     UPARAM  ,NFUNC    ,IFUNC  ,NPC      ,PLD      )       
c
c          
c------------------
          CASE (87) !BARLAT 2D
c------------------
            NFUNC = UPARAM(25)
            FLAG_FIT = NINT ( UPARAM(25+2*NFUNC+9) )
            IF (FLAG_FIT == 1) THEN 
              CALL LAW87_UPD(IOUT,TITR,MAT_ID   ,UPARAM,NFUNC  ,
     .                       IFUNC,FUNC_ID,NPC,PLD,PM(1,IMAT))
            ENDIF
c-------------------------
          CASE (88) 
c------------------
c
            CALL LAW88_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,IFUNC,MAT_ID,FUNC_ID,PM(1,IMAT))
c------------------
          CASE (90) 
c------------------
c
            CALL LAW90_UPD(IOUT,TITR,MAT_ID   ,UPARAM,
     .                     IPM(1,IMAT),FUNC_ID,NPC,PLD,PM(1,IMAT))
c------------------
          CASE (92) ! Arruda-Boyce hyperelastic law
c------------------
c
            UPARAM(13) = MULLINS(IMAT)
            IFC = IPM(11,IMAT)
            IF (IFC > 0) THEN
              CALL LAW92_UPD(IOUT  ,TITR    ,MAT_ID ,UPARAM ,NFUNC     , 
     .                 IFUNC ,FUNC_ID ,NPC    ,PLD   ,PM(1,IMAT),IPM(1,IMAT))
            ENDIF
c------------------
          CASE (95) ! Bergstrom-Boyce visco-elastic law
            UPARAM(21) = MULLINS(IMAT)
c------------------

c------------------
          CASE (98)    ! 
c------------------

c
c------------------
          CASE (100)    ! parallel network model
c------------------
c
            FLAG_HE = UPARAM(2)
            UPARAM(3) = MULLINS(IMAT)
            IF (FLAG_HE == 2 )THEN
              IFC = IPM(11,IMAT)
              IF( IFC /=0 ) THEN 
                CALL LAW100_UPD_AB(IOUT, TITR    , MAT_ID, UPARAM, NFUNC, 
     .                        IFUNC, FUNC_ID , NPC   , PLD , PM(1,IMAT))
              ENDIF
            ELSEIF(FLAG_HE == 13 )THEN
                CALL LAW100_UPD_NHT(IOUT, TITR    , MAT_ID, UPARAM, NFUNC, 
     .                        IFUNC, FUNC_ID , NPC   , PLD , PM(1,IMAT))
            ENDIF
c------------------
          CASE (104)    ! Drucker-Gurson
c
            IR = GURSON(IMAT)
            IF (IR > 0) THEN
              IFAILG = 1
              II     = 112 + 15*(IR-1)
              NUPARF = IPM(II,IMAT)
              II     = 114 + 15*(IR-1)
              IADD   = IPM(II,IMAT)
              UPARF => BUFMAT(IADD:IADD+NUPARF-1) 
c
              CALL LAW104_UPD(IFAILG  ,NPARAM,NUPARF,UPARAM,UPARF   ,
     .                        NLOC_DMG,IMAT   ,MLAW_TAG(IMAT),IPM    ,
     .                        MATPARAM)
            ENDIF 
c------------------
          CASE (108)    ! Spring material------------------
            CALL LAW108_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,IFUNC,MAT_ID,FUNC_ID,
     .                     PM(1,IMAT))     
c------------------
          CASE (109)    ! ------------------  
            IF (IPM(253,IMAT) > 0) THEN 
              MLAW_TAG(IMAT)%NUVAR = 1
            ENDIF            
c------------------
          CASE (111) ! Marlow  hyperelastic law
c------------------
c
           !! UPARAM(13) = MULLINS(IMAT)
            IFC = IPM(11,IMAT)
            IF (IFC > 0) THEN
              CALL LAW111_UPD(IOUT  ,TITR    ,MAT_ID ,UPARAM ,NFUNC     , 
     .                 IFUNC ,FUNC_ID ,NPC    ,PLD   ,PM(1,IMAT),IPM(1,IMAT))
            ENDIF
c------------------
          CASE (112)    ! Paperboard material------------------
            CALL LAW112_UPD(TITR ,UPARAM,NPC    ,PLD   ,
     .                      NUMTABL,ITABLE,TABLE,MAT_ID)
c------------------
          CASE (113)    ! Spring material------------------
            CALL LAW113_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,IFUNC,MAT_ID,FUNC_ID,
     .                     PM(1,IMAT))

c------------------
          CASE (114)    ! Spring material------------------
            CALL LAW114_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,IFUNC,MAT_ID,FUNC_ID,
     .                     PM(1,IMAT))
c------------------
          CASE (119)    ! fabric material for seatbelts
            CALL LAW119_UPD(NPARAM ,NUMTABL ,ITABLE ,TABLE ,TABLE_ID,
     .                      UPARAM,PM(1,IMAT),TITR    ,MAT_ID )          
c------------------
          CASE (120)    ! TAPO material for adhesives
            CALL LAW120_UPD(NPARAM ,NUMTABL ,ITABLE ,TABLE ,UPARAM)          
c-------------------------
          CASE (158) 
            NFUNCL =  IPM(6,IMAT)
            IFUNC  => IPM(10+1:10+NFUNC+NFUNCL,IMAT)
            CALL LAW158_UPD(TITR,UPARAM,NPC,PLD,
     .                      NFUNC,NFUNCL,IFUNC,MAT_ID,FUNC_ID,
     .                      PM(1,IMAT),NPARAM,SENSORS)
c------------------
          CASE (190) !PAth dependent foam (dubois)
c------------------
            CALL LAW190_UPD(MATPARAM ,NUMTABL ,ITABLE   ,TABLE   )       
c------------------
c
        END SELECT ! (ILAW)  
c------------------
      ENDDO        ! IMAT=1,NUMMAT
c-----------
      RETURN
      END
