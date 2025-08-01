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
!||    updmat                 ../starter/source/materials/updmat.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    eosfun_usr2sys         ../starter/source/materials/tools/eosfun_usr2sys.F90
!||    fretitl2               ../starter/source/starter/freform.F
!||    law100_upd_ab          ../starter/source/materials/mat/mat100/law100_upd.F
!||    law100_upd_nht         ../starter/source/materials/mat/mat100/law100_upd.F
!||    law104_upd             ../starter/source/materials/mat/mat104/law104_upd.F
!||    law108_upd             ../starter/source/materials/mat/mat108/law108_upd.F
!||    law111_upd             ../starter/source/materials/mat/mat111/law111_upd.F
!||    law112_upd             ../starter/source/materials/mat/mat112/law112_upd.F
!||    law113_upd             ../starter/source/materials/mat/mat113/law113_upd.F
!||    law114_upd             ../starter/source/materials/mat/mat114/law114_upd.F
!||    law119_upd             ../starter/source/materials/mat/mat119/law119_upd.F
!||    law120_upd             ../starter/source/materials/mat/mat120/law120_upd.F
!||    law129_upd             ../starter/source/materials/mat/mat129/law129_upd.F90
!||    law133_upd             ../starter/source/materials/mat/mat133/law133_upd.F90
!||    law158_upd             ../starter/source/materials/mat/mat158/law158_upd.F
!||    law163_upd             ../starter/source/materials/mat/mat163/law163_upd.F90
!||    law190_upd             ../starter/source/materials/mat/mat190/law190_upd.F90
!||    law19_upd              ../starter/source/materials/mat/mat019/law19_upd.F90
!||    law36_upd              ../starter/source/materials/mat/mat036/law36_upd.F
!||    law42_upd              ../starter/source/materials/mat/mat042/law42_upd.F
!||    law58_upd              ../starter/source/materials/mat/mat058/law58_upd.F
!||    law62_upd              ../starter/source/materials/mat/mat062/law62_upd.F
!||    law69_upd              ../starter/source/materials/mat/mat069/law69_upd.F
!||    law70_upd              ../starter/source/materials/mat/mat070/law70_upd.F
!||    law76_upd              ../starter/source/materials/mat/mat076/law76_upd.F
!||    law77_upd              ../starter/source/materials/mat/mat077/law77_upd.F
!||    law81_upd              ../starter/source/materials/mat/mat081/law81_upd.F90
!||    law87_upd              ../starter/source/materials/mat/mat087/law87_upd.F90
!||    law88_upd              ../starter/source/materials/mat/mat088/law88_upd.F
!||    law90_upd              ../starter/source/materials/mat/mat090/law90_upd.F
!||    law92_upd              ../starter/source/materials/mat/mat092/law92_upd.F
!||    matfun_usr2sys         ../starter/source/materials/tools/matfun_usr2sys.F
!||    mattab_usr2sys         ../starter/source/materials/tools/mattab_usr2sys.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod           ../starter/share/modules1/elbuftag_mod.F
!||    eosfun_usr2sys_mod     ../starter/source/materials/tools/eosfun_usr2sys.F90
!||    law129_upd_mod         ../starter/source/materials/mat/mat129/law129_upd.F90
!||    law133_upd_mod         ../starter/source/materials/mat/mat133/law133_upd.F90
!||    law163_upd_mod         ../starter/source/materials/mat/mat163/law163_upd.F90
!||    law190_upd_mod         ../starter/source/materials/mat/mat190/law190_upd.F90
!||    law19_upd_mod          ../starter/source/materials/mat/mat019/law19_upd.F90
!||    law81_upd_mod          ../starter/source/materials/mat/mat081/law81_upd.F90
!||    law87_upd_mod          ../starter/source/materials/mat/mat087/law87_upd.F90
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
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
      USE LAW19_UPD_MOD  , ONLY : LAW19_UPD
      USE LAW81_UPD_MOD  , ONLY : LAW81_UPD
      USE LAW87_UPD_MOD  , ONLY : LAW87_UPD
      USE LAW129_UPD_MOD , ONLY : LAW129_UPD
      USE LAW133_UPD_MOD , ONLY : LAW133_UPD
      USE LAW163_UPD_MOD , ONLY : LAW163_UPD
      USE LAW190_UPD_MOD , ONLY : LAW190_UPD
      USE VISC_PARAM_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE EOSFUN_USR2SYS_MOD , ONLY : EOSFUN_USR2SYS
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
      INTEGER NPC(SNPC), FUNC_ID(NFUNCT)
      INTEGER, DIMENSION(NPROPMI,NUMMAT) ,INTENT(INOUT) :: IPM
      my_real PM(NPROPM,NUMMAT), PLD(STF),BUFMAT(SBUFMAT)
      TARGET IPM,BUFMAT
      TYPE(MLAW_TAG_), TARGET, DIMENSION(NUMMAT)  :: MLAW_TAG
      TYPE(TTABLE), DIMENSION(NTABLE) ,INTENT(INOUT) ::  TABLE
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(INOUT) ,TARGET :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT,ILAW,IRUP,MAT_ID,IADD,IFC,FLAG_HE,NFUNC,NFUNCL,NUMTABL,
     .        NPARAM,NUPARF,NFAIL,IR,FLAG_FIT,IFAILG,
     .        NPRONY,J,ITYPE_VISC,FORM
      my_real :: GAMA_INF
      INTEGER, DIMENSION(NUMMAT)     :: MULLINS,GURSON
      INTEGER, DIMENSION(MAXTAB)     :: TABLE_ID
      INTEGER, DIMENSION(:), POINTER :: IFUNC,ITABLE
      my_real, DIMENSION(:), POINTER :: UPARAM,UPARF
      TYPE(MLAW_TAG_), POINTER :: MTAG
      CHARACTER(LEN=NCHARTITLE)::TITR, TITLE
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
      TYPE (VISC_PARAM_)     , POINTER :: VISC
      INTEGER :: NFUNC_EOS
C=======================================================================
c     TRAITEMENT OF MATERIAL PARAMETERS AND FUNCTIONS
c-----------------------------------------------------------------------
      MULLINS(1:NUMMAT) = 0
      GURSON (1:NUMMAT) = 0
      DO IMAT=1,NUMMAT
        ILAW  = MAT_PARAM(IMAT)%ILAW
        NFAIL = MAT_PARAM(IMAT)%NFAIL
        IF (NFAIL > 0) THEN
          MTAG => MLAW_TAG(IMAT)
          DO IR = 1,NFAIL
            IRUP = MAT_PARAM(IMAT)%FAIL(IR)%IRUPT
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
            ELSE IF (IRUP == 10 .OR. IRUP == 11 .OR. IRUP == 13 .OR. IRUP == 30 .OR.
     .               IRUP == 41 .OR. IRUP == 42 .OR. IRUP == 44 .OR. IRUP == 45 .OR.
     .               IRUP == 46 .OR. IRUP == 47 .OR. IRUP == 50 .OR. IRUP == 51) THEN             
              MTAG%L_DMGSCL = 1
              MTAG%G_DMGSCL = 1
            ! -> Orthotropic stress softening
            ELSE IF (IRUP == 48) THEN 
              MTAG%G_DMGSCL = 6
              MTAG%L_DMGSCL = 6
            ENDIF
          ENDDO
        ENDIF
      ENDDO
c-----------------------------------------------------------------------


      DO IMAT=1,NUMMAT
C
        MAT_ID  = MAT_PARAM(IMAT)%MAT_ID
        ILAW    = MAT_PARAM(IMAT)%ILAW

        !EoS
        NFUNC_EOS = MAT_PARAM(IMAT)%EOS%NFUNC
        IF(NFUNC_EOS > 0)THEN
          title = MAT_PARAM(IMAT)%EOS%title
          CALL EOSFUN_USR2SYS(title,MAT_ID,NFUNC_EOS,MAT_PARAM(IMAT)%EOS%FUNC,FUNC_ID, NFUNCT  )
        ENDIF

c
        IF (ILAW < 29.and.ILAW/=19.or.ILAW==32.or.ILAW==43.or.ILAW==49.or.ILAW==52.or.
     .      ILAW==57.or.ILAW==59.or.ILAW==60.or.ILAW==65.or.ILAW==75.or.
     .      ILAW==78 ) CYCLE
c
        CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
c
        IADD     = MAX(1,IPM(7,IMAT))
        NPARAM   = MIN(IPM(9,IMAT),SBUFMAT)
        UPARAM   => BUFMAT(IADD:IADD+NPARAM-1)
        !/visc/prony
        ITYPE_VISC = MAT_PARAM(IMAT)%IVISC
        GAMA_INF = ONE
        IF (ITYPE_VISC == 2) THEN
          VISC  => MAT_PARAM(IMAT)%VISC
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
        IF (NFUNC > 0) THEN 
          CALL MATFUN_USR2SYS(TITR,MAT_ID,NFUNC,IFUNC,FUNC_ID  )
        ENDIF

        IF (ILAW /= 73 .and. ILAW /= 74 .and. ILAW /= 80) THEN
          IF (NUMTABL > 0 ) THEN
            TABLE_ID(1:NUMTABL) = IPM(226+1:226+NUMTABL,IMAT)
            CALL MATTAB_USR2SYS(TITR,MAT_ID,NTABLE,TABLE,NUMTABL,ITABLE)
          ENDIF 
        ENDIF

c-----------------------------------------------------------------------
        SELECT CASE (ILAW)
c-------------------------
          CASE (19) 
            CALL LAW19_UPD(MAT_PARAM(IMAT),SENSORS)
c-------------------------
          CASE (36) 
            MTAG => MLAW_TAG(IMAT)
            CALL LAW36_UPD(IOUT   ,TITR   ,MAT_ID ,NPARAM,UPARAM ,
     .                    NFUNC  ,IFUNC  ,FUNC_ID,NPC    ,PLD  ,
     .                    MTAG   ,NFUNCT )        
c-------------------------
          CASE (42) 
            CALL LAW42_UPD(MAT_PARAM(IMAT),IOUT,TITR,MAT_ID,PM(1,IMAT),GAMA_INF)        
c-------------------------
          CASE (58) 
            NFUNCL =  IPM(6,IMAT)
            IFUNC  => IPM(10+1:10+NFUNC+NFUNCL,IMAT)
            CALL LAW58_UPD(MAT_PARAM(IMAT),TITR,NPC,PLD,
     .                     NFUNC,NFUNCL,IFUNC,MAT_ID,FUNC_ID,
     .                     PM(1,IMAT),SENSORS)
c-------------------------
          CASE (62) 
            CALL LAW62_UPD(IOUT,TITR,MAT_ID,NPARAM,UPARAM, PM(1,IMAT),GAMA_INF) 
c------------------
          CASE (69) 
           CALL LAW69_UPD(IOUT, TITR    , MAT_ID, UPARAM, NFUNC,NFUNCT, 
     .                    IFUNC, FUNC_ID , NPC   , PLD , 
     .                    PM(1,IMAT),IPM(1,IMAT),GAMA_INF)
c          
c------------------
          CASE (70) 
c------------------
            CALL LAW70_UPD(MAT_PARAM(IMAT)    ,TITR     ,MAT_ID   ,NPARAM   ,UPARAM   ,
     .                     NFUNC    ,IFUNC    ,NPC      ,PLD      ,IOUT     ,
     .                     NFUNCT   ,FUNC_ID  ,NPROPM   ,PM(1,IMAT))       
c------------------
          CASE (76) 
            CALL LAW76_UPD(IOUT   ,TITR    ,MAT_ID   ,NPARAM ,MAT_PARAM(IMAT) ,
     .                     UPARAM ,NUMTABL ,ITABLE   ,TABLE   ,NFUNC    ,
     .                     IFUNC  ,NPC     ,PLD      )       
c
c------------------
          CASE (77) 
            CALL LAW77_UPD(TITR    ,MAT_ID   ,NPARAM ,MAT_PARAM(IMAT) ,
     .                     UPARAM  ,NFUNC    ,IFUNC  ,NPC      ,PLD      )
c
c------------------
          CASE (81) 
            CALL LAW81_UPD(MAT_PARAM(IMAT)   ,NFUNC    ,IFUNC  ,NPC      ,SNPC     ,
     .                     PLD     ,STF      ,PM(1,IMAT),NPROPM,IOUT     ,
     .                     MAT_ID  ,TITR     )
c          
c------------------
          CASE (87) !BARLAT 2D
            FLAG_FIT = MAT_PARAM(IMAT)%IPARAM(3)
            IF (FLAG_FIT == 1) THEN 
              CALL LAW87_UPD(IOUT   ,TITR    ,MAT_ID ,MAT_PARAM(IMAT)     )
            ENDIF
c-------------------------
          CASE (88) 
c
            CALL LAW88_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,IFUNC,MAT_ID,FUNC_ID,PM(1,IMAT),
     .                     NFUNCT )
c------------------
          CASE (90) 
c
            CALL LAW90_UPD(IOUT,TITR,MAT_ID   ,UPARAM,
     .                     IPM(1,IMAT),FUNC_ID,NPC,PLD,PM(1,IMAT),
     .                     NFUNCT)
c------------------
          CASE (92) ! Arruda-Boyce hyperelastic law
c------------------
c
            UPARAM(13) = MULLINS(IMAT)
            IFC = IPM(11,IMAT)
            IF (IFC > 0) THEN
              CALL LAW92_UPD(IOUT  ,TITR    ,MAT_ID ,UPARAM ,NFUNC ,NFUNCT, 
     .                 IFUNC ,FUNC_ID ,NPC    ,PLD   ,PM(1,IMAT),IPM(1,IMAT))
            ENDIF
c------------------
          CASE (95) ! Bergstrom-Boyce visco-elastic law
            UPARAM(21) = MULLINS(IMAT)
c------------------
          CASE (100)    ! parallel network model
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
              NUPARF = MAT_PARAM(IMAT)%FAIL(IR)%NUPARAM
              UPARF => MAT_PARAM(IMAT)%FAIL(IR)%UPARAM(1:NUPARF)
c
              CALL LAW104_UPD(IFAILG  ,NPARAM,NUPARF,UPARAM,UPARF   ,
     .                        NLOC_DMG,IMAT   ,MLAW_TAG(IMAT),IPM    ,
     .                        MAT_PARAM(IMAT))
            ENDIF 
c------------------
          CASE (108)    ! Spring material------------------
            CALL LAW108_UPD(IOUT,TITR,UPARAM,NPC,PLD,
     .                     NFUNC,IFUNC,MAT_ID,FUNC_ID,
     .                     PM(1,IMAT))     
c------------------
          CASE (109)    ! ------------------  
            IF (MAT_PARAM(IMAT)%NLOC > 0) THEN 
              MLAW_TAG(IMAT)%NUVAR = 1
            ENDIF            
c------------------
          CASE (111) ! Marlow  hyperelastic law
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
c------------------
          CASE (129)    
            CALL LAW129_UPD(MAT_PARAM(IMAT),SENSORS)          
c------------------
          CASE (133)
            CALL LAW133_UPD(MAT_PARAM(IMAT),PM(1,IMAT),NPROPM)
c-------------------------
          CASE (158) 
            NFUNCL =  IPM(6,IMAT)
            IFUNC  => IPM(10+1:10+NFUNC+NFUNCL,IMAT)
            CALL LAW158_UPD(MAT_PARAM(IMAT),TITR,NPC,PLD,
     .                      NFUNC,NFUNCL,IFUNC,MAT_ID,FUNC_ID,
     .                      PM(1,IMAT),SENSORS)
c------------------
          CASE (163)
            CALL LAW163_UPD(MAT_PARAM(IMAT),PM(1,IMAT),NPROPM)
c------------------
          CASE (190) !PAth dependent foam (dubois)
            CALL LAW190_UPD(MAT_PARAM(IMAT) ,NUMTABL ,ITABLE   ,TABLE   ,PM(1,IMAT),
     .                      NPROPM, NTABLE)
c------------------
c
        END SELECT ! (ILAW)  
c------------------
      ENDDO        ! IMAT=1,NUMMAT
c-----------
      RETURN
      END
