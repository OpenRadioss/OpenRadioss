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
      !||    hm_read_eos                      ../starter/source/materials/eos/hm_read_eos.F
      !||--- called by ------------------------------------------------------
      !||    read_material_models             ../starter/source/materials/read_material_models.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                           ../starter/source/output/message/message.F
      !||    hm_get_string                    ../starter/source/devtools/hm_reader/hm_get_string.F
      !||    hm_option_count                  ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_read_key               ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start                  ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    hm_read_eos_compaction           ../starter/source/materials/eos/hm_read_eos_compaction.F90
      !||    hm_read_eos_compaction2          ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||    hm_read_eos_compaction_tab       ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||    hm_read_eos_exponential          ../starter/source/materials/eos/hm_read_eos_exponential.F90
      !||    hm_read_eos_gruneisen            ../starter/source/materials/eos/hm_read_eos_gruneisen.F
      !||    hm_read_eos_ideal_gas            ../starter/source/materials/eos/hm_read_eos_ideal_gas.F
      !||    hm_read_eos_ideal_gas_vt         ../starter/source/materials/eos/hm_read_eos_ideal_gas_vt.F
      !||    hm_read_eos_linear               ../starter/source/materials/eos/hm_read_eos_linear.F
      !||    hm_read_eos_lszk                 ../starter/source/materials/eos/hm_read_eos_lszk.F
      !||    hm_read_eos_murnaghan            ../starter/source/materials/eos/hm_read_eos_murnaghan.F
      !||    hm_read_eos_nasg                 ../starter/source/materials/eos/hm_read_eos_nasg.F
      !||    hm_read_eos_noble_abel           ../starter/source/materials/eos/hm_read_eos_noble_abel.F
      !||    hm_read_eos_osborne              ../starter/source/materials/eos/hm_read_eos_osborne.F
      !||    hm_read_eos_polynomial           ../starter/source/materials/eos/hm_read_eos_polynomial.F
      !||    hm_read_eos_powderburn           ../starter/source/materials/eos/hm_read_eos_powderburn.F90
      !||    hm_read_eos_puff                 ../starter/source/materials/eos/hm_read_eos_puff.F
      !||    hm_read_eos_sesame               ../starter/source/materials/eos/hm_read_eos_sesame.F
      !||    hm_read_eos_stiffened_gas        ../starter/source/materials/eos/hm_read_eos_stiffened_gas.F
      !||    hm_read_eos_tabulated            ../starter/source/materials/eos/hm_read_eos_tabulated.F
      !||    hm_read_eos_tillotson            ../starter/source/materials/eos/hm_read_eos_tillotson.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod                     ../starter/share/modules1/elbuftag_mod.F
      !||    hm_option_read_mod               ../starter/share/modules1/hm_option_read_mod.F
      !||    hm_read_eos_compaction2_mod      ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||    hm_read_eos_compaction_mod       ../starter/source/materials/eos/hm_read_eos_compaction.F90
      !||    hm_read_eos_compaction_tab_mod   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||    message_mod                      ../starter/share/message_module/message_mod.F
      !||    submodel_mod                     ../starter/share/modules1/submodel_mod.F
      !||    table_mod                        ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_EOS(
     .           MAT_PARAM   ,IPM      ,PM       ,BUFMAT   ,NUMMAT   ,NFUNCT ,
     .           BUFLEN      ,IADBUF   ,EOS_TAG  ,UNITAB   ,LSUBMODEL,
     .           MLAW_TAG    ,NPC      ,TF       ,SNPC     ,NPTS     ,SBUFMAT,
     .           NTABLE      ,TABLE)
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
      USE HM_READ_EOS_COMPACTION_MOD , ONLY : HM_READ_EOS_COMPACTION
      USE HM_READ_EOS_COMPACTION2_MOD , ONLY : HM_READ_EOS_COMPACTION2
      USE HM_READ_EOS_COMPACTION_TAB_MOD , ONLY : HM_READ_EOS_COMPACTION_TAB
      USE TABLE_MOD , ONLY : TTABLE
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
      INTEGER,INTENT(IN) :: NUMMAT, NTABLE, NFUNCT
      TYPE(MLAW_TAG_) , DIMENSION(NUMMAT),INTENT(INOUT) :: MLAW_TAG
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  :: BUFLEN,IADBUF
      INTEGER,INTENT(IN) :: SBUFMAT
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(INOUT) :: IPM
      INTEGER,INTENT(IN) :: SNPC, NPTS
      INTEGER,INTENT(IN) :: NPC(SNPC)
      my_real,INTENT(IN) :: TF(NPTS)
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(INOUT) :: PM
      my_real ,DIMENSION(SBUFMAT),INTENT(INOUT) :: BUFMAT
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
      TYPE(EOS_TAG_),DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG
      TYPE(SUBMODEL_DATA) ,DIMENSION(NSUBMOD)  ,INTENT(IN)    :: LSUBMODEL
      TYPE(TTABLE) ,DIMENSION(NTABLE) ,INTENT(IN) :: TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   :: I,IMAT,IEOS,HM_NEOS,IUNIT,FLAGUNIT,FLAGMAT,IMID,EOS_uid,MTN,UNIT_ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER KEY2*32, EOS_key*32
      LOGICAL IS_AVAILABLE
      INTEGER ISFLUID
c--------------------------------------------------
c     Routine reading EOS models :
      !------------------------------------!                
      !  IEOS     !    EOS                 !             
      !-----------!------------------------!  
      !   0 => 18 (LINEAR)                 !
      !   1       !     POLYNOMIAL         !             
      !   2       !     GRUNEISEN          !             
      !   3       !     TILLOTSON          !             
      !   4       !     PUFF               !             
      !   5       !     SESAME             !             
      !   6       !     NOBLE-ABEL         ! 2017.0            
      !   7       !     IDEAL GAS          ! 2018.0            
      !   8       !     MUNAGHAN           ! 2018.0      
      !   9       !     OSBORNE            ! 2018.0      
      !  10       !     STIFFENED GAS      ! 2018.0      
      !  11       !     LSZK               ! 2018.0  
      !  12       !     POWDER-BURN        ! 2019.1
      !  13       !     COMPACTION         ! 2019.1 
      !  14       !     NASG               ! 2020.0   
      !  15       !     JWL                ! internal use : /INIMAP
      !  16       !     IDEALGAS_VT        ! 2022.0
      !  17       !     TABULATED          ! 2022.2
      !  18       !     LINEAR             ! 2019.0
      !  19       !     EXPONENTIAL        ! 2024.1
      !  20       !     COMPACTION2        ! 2025.1
      !  21       !     COMPACTION_TAB     ! 2026.0
      !------------------------------------!
c======================================================================-       
c     COUNT EOS MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('/EOS',HM_NEOS)
c
c--------------------------------------------------
c     START BROWSING EOS MODELS
c--------------------------------------------------
c
      CALL HM_OPTION_START('/EOS')
c--------------------------------------------------
c
      DO I = 1,HM_NEOS
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=EOS_uid, OPTION_TITR=TITR, UNIT_ID=UNIT_ID)
        CALL HM_GET_STRING('EOS_Keyword',KEY2, 32,IS_AVAILABLE)
        EOS_key = KEY2(1:LEN_TRIM(KEY2))
        ! Check MAT_Id
        FLAGMAT  = 0
        DO IMAT=1,NUMMAT-1
          IMID = IPM(1,IMAT)
          IF (IMID == EOS_uid) THEN
            FLAGMAT = 1
            EXIT
          ENDIF
        ENDDO
        IF (EOS_uid > 0 .AND. FLAGMAT == 0) THEN
          CALL ANCMSG(MSGID=1663, ANMODE=ANINFO, MSGTYPE=MSGERROR, I1=EOS_uid, C1='EOS', C2=EOS_key, C3=TITR)
        ENDIF

        ! Check Unit_ID
        FLAGUNIT = 0
        DO IUNIT=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UNIT_ID) THEN
            FLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UNIT_ID > 0 .AND. FLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=EOS_uid,I2=UNIT_ID,C1='MATERIAL',C2='EOS MODEL',C3=TITR)
        ENDIF

         !initialize %EOS member for current material
        MAT_PARAM(IMAT)%EOS%TITLE = TITR
        MAT_PARAM(IMAT)%EOS%NUPARAM = 0
        MAT_PARAM(IMAT)%EOS%NIPARAM = 0
        MAT_PARAM(IMAT)%EOS%NFUNC = 0
        MAT_PARAM(IMAT)%EOS%NTABLE = 0
        MAT_PARAM(IMAT)%EOS%ISFLUID = 0
        ISFLUID = 0

        WRITE(IOUT,1000) TRIM(TITR),EOS_uid
        ! READ EOS
        SELECT CASE (EOS_key)
c---
          CASE ('POLYNOMIAL')
            IEOS = 1     
            CALL HM_READ_EOS_POLYNOMIAL(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid,ISFLUID)
c---
          CASE ('GRUNEISEN')
            IEOS = 2                          
            CALL HM_READ_EOS_GRUNEISEN(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
c---
          CASE ('TILLOTSON')
            IEOS = 3  
            CALL HM_READ_EOS_TILLOTSON(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid,EOS_TAG,IEOS)
c---
          CASE ('PUFF')
            IEOS = 4  
            CALL HM_READ_EOS_PUFF(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
c---
          CASE ('SESAME')
            IEOS = 5                          
            CALL HM_READ_EOS_SESAME(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid,IMID,TITR,BUFMAT,BUFLEN,IADBUF)
c---
          CASE ('NOBLE-ABEL','NA')
            IEOS = 6  
            CALL HM_READ_EOS_NOBLE_ABEL(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
            ISFLUID = 1
c---
          CASE ('IDEAL-GAS','IDEAL-GAS-VE')
            IEOS = 7      
            CALL HM_READ_EOS_IDEAL_GAS(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid,MAT_PARAM(IMAT))
            ISFLUID = 1
c---
          CASE ('MURNAGHAN')
            IEOS = 8                          
            CALL HM_READ_EOS_MURNAGHAN(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
c---
          CASE ('OSBORNE')
            IEOS = 9                          
            CALL HM_READ_EOS_OSBORNE(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
c---
          CASE ('STIFF-GAS','SG')
            IEOS = 10                          
            CALL HM_READ_EOS_STIFFENED_GAS(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
            ISFLUID = 1
c---
          CASE ('LSZK')
            IEOS = 11                          
            CALL HM_READ_EOS_LSZK(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
            ISFLUID = 1
c---
          CASE ('POWDER-BURN')
            IEOS = 12
            CALL HM_READ_EOS_POWDERBURN(IOUT,PM(1,IMAT),IPM(1,IMAT),UNITAB,LSUBMODEL,EOS_uid, MAT_PARAM(IMAT),
     .                                  NPROPM, NPROPMI, MLAW_TAG(IMAT), EOS_TAG, IEOS  )
c---
          CASE ('COMPACTION')
            IEOS = 13                          
            CALL HM_READ_EOS_COMPACTION(IOUT,PM(1,IMAT),UNITAB,LSUBMODEL,EOS_uid,EOS_TAG,IEOS,NPROPM,MAXEOS)
c---
          CASE ('NASG')
            IEOS = 14                          
            CALL HM_READ_EOS_NASG(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
            ISFLUID = 1
c---
          CASE ('IDEAL-GAS-VT')
            IEOS = 16     
            CALL HM_READ_EOS_IDEAL_GAS_VT(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
            ISFLUID = 1
c---
          CASE ('TABULATED')
            IEOS = 17     
            CALL HM_READ_EOS_TABULATED(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
c---
          CASE ('LINEAR')
            IEOS = 18                          
            CALL HM_READ_EOS_LINEAR(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,EOS_uid)
c---
          CASE ('EXPONENTIAL')
            IEOS = 19
            CALL HM_READ_EOS_EXPONENTIAL(IOUT,PM(1,IMAT),UNITAB,LSUBMODEL,NPROPM)
c---
          CASE ('COMPACTION2')
            IEOS = 20
            CALL HM_READ_EOS_COMPACTION2(IOUT,PM(1,IMAT),UNITAB,LSUBMODEL,EOS_uid,EOS_TAG,IEOS,NPROPM,MAXEOS,
     .                                   MAT_PARAM(IMAT)%EOS, IUNIT, NFUNCT, NPC, TF ,SNPC , NPTS)
          CASE ('COMPACTION_TAB')
            IEOS = 21
            CALL HM_READ_EOS_COMPACTION_TAB(IOUT,PM(1,IMAT),UNITAB,LSUBMODEL,EOS_uid,EOS_TAG,IEOS,NPROPM,MAXEOS,
     .                                   MAT_PARAM(IMAT)%EOS, NTABLE, TABLE)
c---
          CASE DEFAULT
            IEOS = -1 
            CALL ANCMSG(MSGID=67,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1= EOS_uid,C1=KEY2,C2='EOS KEYWORD IS NOT VALID')
c---
        END SELECT

        IPM(4,IMAT) = IEOS
        MAT_PARAM(IMAT)%IEOS = IEOS
        MAT_PARAM(IMAT)%EOS%ISFLUID = ISFLUID
c--------------------------------------------------
      ENDDO      !  HM_NEOS
c--------------------------------------------------
      DO I=1,NUMMAT-1
        MTN = IPM(2,I)
        IF (MTN /= 42) PM(100,I) = PM(32,I)
      ENDDO  
c-----------       
      RETURN
c--------------------------------------------------
 1000 FORMAT(//
     & 5X,'EQUATION OF STATE ',/,
     & 5X,A,/,
     & 5X,'MATERIAL ID  . . . . . . . . . . . .=',I10/)
c--------------------------------------------------
      END
