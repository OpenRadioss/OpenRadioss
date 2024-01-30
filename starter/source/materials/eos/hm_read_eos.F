Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_EOS_COMPACTION        source/materials/eos/hm_read_eos_compaction.F
Chd|        HM_READ_EOS_GRUNEISEN         source/materials/eos/hm_read_eos_gruneisen.F
Chd|        HM_READ_EOS_IDEAL_GAS         source/materials/eos/hm_read_eos_ideal_gas.F
Chd|        HM_READ_EOS_IDEAL_GAS_VT      source/materials/eos/hm_read_eos_ideal_gas_vt.F
Chd|        HM_READ_EOS_LINEAR            source/materials/eos/hm_read_eos_linear.F
Chd|        HM_READ_EOS_LSZK              source/materials/eos/hm_read_eos_lszk.F
Chd|        HM_READ_EOS_MURNAGHAN         source/materials/eos/hm_read_eos_murnaghan.F
Chd|        HM_READ_EOS_NASG              source/materials/eos/hm_read_eos_nasg.F
Chd|        HM_READ_EOS_NOBLE_ABEL        source/materials/eos/hm_read_eos_noble_abel.F
Chd|        HM_READ_EOS_OSBORNE           source/materials/eos/hm_read_eos_osborne.F
Chd|        HM_READ_EOS_POLYNOMIAL        source/materials/eos/hm_read_eos_polynomial.F
Chd|        HM_READ_EOS_PUFF              source/materials/eos/hm_read_eos_puff.F
Chd|        HM_READ_EOS_SESAME            source/materials/eos/hm_read_eos_sesame.F
Chd|        HM_READ_EOS_STIFFENED_GAS     source/materials/eos/hm_read_eos_stiffened_gas.F
Chd|        HM_READ_EOS_TABULATED         source/materials/eos/hm_read_eos_tabulated.F
Chd|        HM_READ_EOS_TILLOTSON         source/materials/eos/hm_read_eos_tillotson.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS(
     .           IPM      ,PM       ,BUFMAT   ,BUFLEN   ,IADBUF   ,
     .           EOS_TAG  ,UNITAB   ,LSUBMODEL)
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  :: BUFLEN,IADBUF
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(INOUT) :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(INOUT) :: PM
      my_real ,DIMENSION(*)              ,INTENT(INOUT) :: BUFMAT
      TYPE(EOS_TAG_),DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG
      TYPE(SUBMODEL_DATA) ,DIMENSION(*)  ,INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   :: I,IMAT,IEOS,HM_NEOS,IUNIT,FLAGUNIT,FLAGMAT,IMID,IMIDEOS,MTN,UID
      CHARACTER :: TITR*nchartitle,KEY*ncharkey,KEY2*32, EOS_key*32
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
c--------------------------------------------------
c     Routine reading EOS models :
      !------------------------------------!                
      !  IEOS     !    EOS                 !             
      !-----------!------------------------!  
      !   0       !     LINEAR             !                 
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
      !  15       !     JWL                ! internal use : INIMAP   
      !  16       !     IDEALGAS_VT        ! 2022.0
      !  17       !     TABULATED          ! 2022.2
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
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = IMIDEOS ,
     .                          OPTION_TITR = TITR   ,
     .                          UNIT_ID     = UID    )   
     
        CALL HM_GET_STRING('EOS_Keyword',KEY2, 32,IS_AVAILABLE)
        !CALL HM_GET_INTV('_ID_',IMIDEOS,IS_AVAILABLE,LSUBMODEL)       
c--------------------------------------------------
c       Check MAT_Id
c--------------------------------------------------
        FLAGMAT  = 0
        DO IMAT=1,NUMMAT-1
          IMID = IPM(1,IMAT)
          IF (IMID == IMIDEOS) THEN
            FLAGMAT = 1
            EXIT
          ENDIF
        ENDDO
        
        IF (IMIDEOS > 0 .AND. FLAGMAT == 0) THEN
          CALL ANCMSG(MSGID=1663,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= IMIDEOS,
     .                C1='EOS',
     .                C2= KEY,
     .                C3= TITR)                              
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
          CALL ANCMSG(MSGID=659,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1= IMIDEOS,
     .                I2= UID,
     .                C1='MATERIAL',
     .                C2='EOS MODEL',
     .                C3= TITR)                             
        ENDIF

        MTN = IPM(2,IMAT)  
        IF (MTN/=3  .AND. MTN/=4   .AND. MTN/=6 .AND. MTN/=10 .AND. MTN/=12 .AND. 
     .      MTN/=49 .AND. MTN/=102 .AND. MTN/=103 .AND. MTN/=109) THEN       
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)                                                                        
          CALL ANCMSG(MSGID=824,                                                                                                  
     .                MSGTYPE=MSGERROR,                                                                                           
     .                ANMODE=ANINFO,                                                                                              
     .                I1=IPM(1,IMAT),                                                                                                
     .                C1=TITR,I2=MTN)                                                                                             
        ENDIF                                                                      
c--------------------------------------------------
c       Read EOS model
c--------------------------------------------------
        WRITE(IOUT,1000) TRIM(TITR),IMIDEOS
c
c-----------------------------
        EOS_key = KEY2(1:LEN_TRIM(KEY2))
        SELECT CASE (EOS_key)
c---
          CASE ('LINEAR')
            IEOS = 0                          
            CALL HM_READ_EOS_LINEAR(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
c---
          CASE ('POLYNOMIAL')
            IEOS = 1     
            CALL HM_READ_EOS_POLYNOMIAL(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                 
c---
          CASE ('GRUNEISEN')
            IEOS = 2                          
            CALL HM_READ_EOS_GRUNEISEN(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
c---
          CASE ('TILLOTSON')
            IEOS = 3  
            CALL HM_READ_EOS_TILLOTSON(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS,EOS_TAG,IEOS)
c---
          CASE ('PUFF')
            IEOS = 4  
            CALL HM_READ_EOS_PUFF(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
c---
          CASE ('SESAME')
            IEOS = 5                          
            CALL HM_READ_EOS_SESAME(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS,IMID,TITR,BUFMAT,BUFLEN,IADBUF)      
c---
          CASE ('NOBLE-ABEL','NA')
            IEOS = 6  
            CALL HM_READ_EOS_NOBLE_ABEL(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                    
c---
          CASE ('IDEAL-GAS','IDEAL-GAS-VE')
            IEOS = 7      
            CALL HM_READ_EOS_IDEAL_GAS(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                
c---
          CASE ('MURNAGHAN')
            IEOS = 8                          
            CALL HM_READ_EOS_MURNAGHAN(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                
c---
          CASE ('OSBORNE')
            IEOS = 9                          
            CALL HM_READ_EOS_OSBORNE(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                
c---
          CASE ('STIFF-GAS','SG')
            IEOS = 10                          
            CALL HM_READ_EOS_STIFFENED_GAS(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                    
c---
          CASE ('LSZK')
            IEOS = 11                          
            CALL HM_READ_EOS_LSZK(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                    
c---
          CASE ('POWDER-BURN')
            IEOS = 12 
            !none                        
c---
          CASE ('COMPACTION')
            IEOS = 13                          
            CALL HM_READ_EOS_COMPACTION(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS,EOS_TAG,IEOS)
c---
          CASE ('NASG')
            IEOS = 14                          
            CALL HM_READ_EOS_NASG(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS) 
c---
          CASE ('IDEAL-GAS-VT')
            IEOS = 16     
            CALL HM_READ_EOS_IDEAL_GAS_VT(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                
c---
          CASE ('TABULATED')
            IEOS = 17     
            CALL HM_READ_EOS_TABULATED(IOUT,PM(1,IMAT),UNITAB,IUNIT,LSUBMODEL,IMIDEOS)                                
                                               
c---
          CASE DEFAULT
            IEOS = -1 
            CALL ANCMSG(MSGID=67,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1= IMIDEOS,
     .                C1='EOS_key',
     .                C2='EOS TYPE IS NOT VALID')                     
c---
        END SELECT
c
        IPM(4,IMAT) = IEOS
c--------------------------------------------------
c
      ENDDO      !  HM_NEOS
c--------------------------------------------------
c
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
