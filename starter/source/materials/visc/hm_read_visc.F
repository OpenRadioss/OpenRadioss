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
Chd|  HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_VISC(
     .           VISC_TAG ,IPM      ,PM         ,BUFMAT   ,BUFLEN   ,
     .           IADBUF   ,UNITAB   ,LSUBMODEL  ,TABLE    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
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
#include      "sysunit.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER :: BUFLEN,IADBUF
      INTEGER ,DIMENSION(NPROPMI,NUMMAT)    ,INTENT(INOUT) :: IPM

      my_real ,DIMENSION(NPROPM ,NUMMAT)    ,INTENT(INOUT) :: PM
      my_real ,DIMENSION(*)                 ,INTENT(INOUT) :: BUFMAT
      TYPE(VISC_TAG_) ,DIMENSION(0:MAXVISC) ,INTENT(INOUT) :: VISC_TAG
      TYPE(SUBMODEL_DATA) ,DIMENSION(*)     ,INTENT(IN)    :: LSUBMODEL
      TYPE(TTABLE) TABLE(NTABLE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,MAT_ID,IMID,UID,IUNIT,FLAGUNIT,NFUNC,NUPARAM,NUVAR,
     .   MAXPARAM,MAXFUNC,NV,IMAT,IVISC,ILAW,MTN,N_NET,IADD,HM_NVISC,FLAGMAT
      PARAMETER (MAXPARAM = 1000)
      PARAMETER (MAXFUNC  = 100)   
      INTEGER ,DIMENSION(MAXFUNC)   :: IFUNC
      my_real ,DIMENSION(MAXPARAM)  :: UPARAM
      CHARACTER TITR*nchartitle,KEY*ncharkey
c======================================================================-       
      VISC_TAG(:)%NUVAR = 0
c
c--------------------------------------------------
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
        DO IUNIT=1,NUNITS
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
C--------------------------------------------------
C          CHECK VISCOSITY MODEL
C--------------------------------------------------
        IF(KEY(1:5) == 'PRONY')THEN
              IVISC=1 
        ELSE
              IVISC=0
        ENDIF      
c--------------------------------------------------
c       Read Viscosity model
c--------------------------------------------------
        UPARAM(:) = ZERO
        NFUNC   = 0 
        NUVAR   = 0
        NUPARAM = 0
c----      
c--------------------------------------------      
c       CALL Viscosity MODEL READING ROUTINES FOR EACH MATERIAL LAW
c--------------------------------------------      
        DO I=1,NUMMAT-1
          IMID  = IPM(1,I)
          IF (IMID == MAT_ID) THEN
            WRITE(IOUT,1000) TRIM(KEY),MAT_ID
c
c-----------------------------
c--------------------------------------------      
            SELECT CASE (IVISC)
c--------------------------------------------      
            CASE (1)     
              CALL HM_READ_VISC_PRONY( UPARAM ,MAXPARAM, NUPARAM, 
     .                                 NUVAR  ,IFUNC   , MAXFUNC,
     .                                 NFUNC  ,UNITAB  , LSUBMODEL,
     .                                 TABLE  ,MAT_ID  )

              IF (IPM(216  ,I)>0) THEN
               IPM(216  ,I) = 3
              ELSE
               IPM(216  ,I) = 1
              END IF
            CASE DEFAULT
              IVISC = 0        
            END SELECT
c-------------------------
            MTN = INT(PM(19,I))      
            IF (IVISC == 1 .AND . MTN == 100) THEN
             IADD    = IPM(7,I)
             N_NET = NINT(BUFMAT(IADD) )
             IF (N_NET /= 0) 
     .          CALL ANCMSG(MSGID=1568 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=IMID)

             ENDIF
c-----------------------------
          ILAW = NINT(PM(19,I))
          IPM(222 ,I) = IVISC
          IPM(223 ,I) = IADBUF
          IPM(224 ,I) = NUPARAM
          IPM(225 ,I) = NUVAR
c 
          VISC_TAG(IVISC)%NUVAR = NUVAR
c-------------------------  
          DO J=1,NUPARAM
            BUFMAT(IADBUF+J-1) = UPARAM(J) 
          END DO
          IADBUF  = IADBUF + NUPARAM  
          BUFLEN =  BUFLEN + NUPARAM
c--------------------------------------------------
         ENDIF   !IMID == MAT_ID
c--------------------------------------------------
        END DO !NUMMAT
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
