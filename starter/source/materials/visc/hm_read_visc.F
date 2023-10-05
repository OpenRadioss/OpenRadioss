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
Chd|  HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_VISC_LPRONY           source/materials/visc/hm_read_visc_lprony.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_VISC(MAT_PARAM   ,IPM        ,BUFMAT      ,
     .                        UNITAB      ,LSUBMODEL  ,TABLE       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MATPARAM_DEF_MOD
      USE MESSAGE_MOD           
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE TABLE_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ,DIMENSION(NPROPMI,NUMMAT)    ,INTENT(INOUT) :: IPM
      my_real ,DIMENSION(*)                 ,INTENT(INOUT) :: BUFMAT
      TYPE(SUBMODEL_DATA) ,DIMENSION(*)     ,INTENT(IN)    :: LSUBMODEL
      TYPE(TTABLE) TABLE(NTABLE)
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NV,IMAT,MAT_ID,IMID,ILAW,IVISC,UID,IUNIT,FLAGUNIT,
     .   N_NET,IADD,HM_NVISC,FLAGMAT,IMATVIS
      CHARACTER KEY*ncharkey
c======================================================================-       
c     COUNT VISCOSITY MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('/VISC',HM_NVISC)
c
c--------------------------------------------------
c     START BROWSING VISCOSITY MODELS
c--------------------------------------------------
c
      CALL HM_OPTION_START('/VISC')
c
c--------------------------------------------------
c
      DO NV = 1,HM_NVISC
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = MAT_ID ,
     .                          UNIT_ID     = UID    ,
     .                          KEYWORD2    = KEY    )
c
c--------------------------------------------------
c       Check MAT_Id
c--------------------------------------------------
        FLAGMAT = 0
        DO IMAT=1,NUMMAT-1
          IMID = IPM(1,IMAT)
          IF (IMID == MAT_ID) THEN
            FLAGMAT = 1
            EXIT
          ENDIF
        ENDDO
        IF (MAT_ID > 0 .AND. FLAGMAT == 0) THEN
          CALL ANCMSG(MSGID=1663,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= MAT_ID,
     .                C1='VISCOSITY',
     .                C2= KEY,
     .                C3='')                              
        ENDIF
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
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= MAT_ID,
     .                I2= UID,
     .                C1='MATERIAL',
     .                C2='VISCOSITY MODEL',
     .                C3='')                              
        ENDIF
c--------------------------------------------------
c       Read Viscosity models
c--------------------------------------------   
        DO I=1,NUMMAT-1
          IMID  = IPM(1,I)
          ILAW  = IPM(2,I)
          IF (IMID == MAT_ID) THEN
            WRITE(IOUT,1000) TRIM(KEY),MAT_ID
c
            IMATVIS = IPM(216,I)
            CALL INIT_MAT_KEYWORD(MAT_PARAM(IMAT),"VISC") ! flag campatibility
c--------------------------------------------      
            IF (KEY(1:5) == 'PRONY')THEN 
c           -----------------------------------   
              IVISC = 1 
              CALL HM_READ_VISC_PRONY(
     .             MAT_PARAM(IMAT)%VISC,IVISC ,
     .             NTABLE   ,TABLE    ,MAT_ID   ,UNITAB  ,LSUBMODEL)

              IF (IMATVIS > 0) THEN
                IMATVIS = 3
              ELSE
                IMATVIS = 1
              END IF
c           -----------------------------------   
            ELSE IF (KEY(1:6) == 'LPRONY') THEN 
c           -----------------------------------   
              IVISC = 2
              CALL HM_READ_VISC_LPRONY(
     .             MAT_PARAM(IMAT)%VISC,IVISC ,MAT_ID   ,UNITAB  ,LSUBMODEL)
c              
            END IF  ! visc model
c-------------------------
            MAT_PARAM(IMAT)%IVISC = IVISC
c
            IF (IVISC == 1 .AND. ILAW == 100) THEN
              IADD = IPM(7,I)  ! number of networks in law 100
              N_NET = NINT(BUFMAT(IADD) )
              IF (N_NET /= 0) CALL ANCMSG(MSGID=1568 ,MSGTYPE=MSGERROR,  
     .                                    ANMODE=ANINFO_BLIND_2, I1=IMID    )
            ENDIF
c-----------------------------
c           duplicate storage for compatibility with old code and output until cleaning
c-----------------------------
            IPM(216 ,I) = IMATVIS
            IPM(222 ,I) = IVISC
            IPM(225 ,I) = MAT_PARAM(IMAT)%VISC%NUVAR
c--------------------------------------------------
          ENDIF   !IMID == MAT_ID
c--------------------------------------------------
        END DO ! NUMMAT
c--------------------------------------------------
c
      ENDDO      !  HM_NVISC
c-----------       
      RETURN
c--------------------------------------------------
 1000 FORMAT(//
     & 5X,'VISCOSITY MODEL: ',5X,A,/,
     & 5X,'MATERIAL ID  . . . . . . . . . . . .=',I10/)
c-----------       
      END
