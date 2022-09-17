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
Chd|  HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        GET_LEMAX                     ../common_source/modules/nlocal_reg_mod.F
Chd|        GET_LENGTH                    ../common_source/modules/nlocal_reg_mod.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_NONLOCAL(NLOC_DMG ,MLAW_TAG ,IPM      ,UNITAB   ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NLOCAL_REG_MOD
      USE ELBUFTAG_MOD            
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
      TYPE(NLOCAL_STR_)                                 :: NLOC_DMG
      TYPE(MLAW_TAG_), DIMENSION(NUMMAT), INTENT(INOUT) :: MLAW_TAG
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(OUT)   :: IPM
      TYPE (UNIT_TYPE_) ,INTENT(IN)                     :: UNITAB 
      TYPE(SUBMODEL_DATA) ,DIMENSION(*) , INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: INL,FLAGMAT,FLAGUNIT,IUNIT,UID,MAT_ID,IMAT,ILAW,NNLOC
      my_real :: RLEN,LE_MAX
      CHARACTER TITR*nchartitle
      LOGICAL IS_AVAILABLE
C=======================================================================
      IS_AVAILABLE = .FALSE.
c--------------------------------------------------
c     COUNT NONLOCAL MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('/NONLOCAL',NNLOC)
      ! Allocation for non-local computation
      IF ((NNLOC > 0).OR.(NLOC_DMG%IMOD>0)) THEN 
        NLOC_DMG%IMOD = 1
        IF (.NOT.ALLOCATED(NLOC_DMG%LEN))    ALLOCATE(NLOC_DMG%LEN(NUMMAT))
        IF (.NOT.ALLOCATED(NLOC_DMG%LE_MAX)) ALLOCATE(NLOC_DMG%LE_MAX(NUMMAT))
        IF (.NOT.ALLOCATED(NLOC_DMG%DENS))   ALLOCATE(NLOC_DMG%DENS(NUMMAT))
        IF (.NOT.ALLOCATED(NLOC_DMG%DAMP))   ALLOCATE(NLOC_DMG%DAMP(NUMMAT))
        IF (.NOT.ALLOCATED(NLOC_DMG%SSPNL))  ALLOCATE(NLOC_DMG%SSPNL(NUMMAT))
        NLOC_DMG%LEN(1:NUMMAT)    = ZERO
        NLOC_DMG%LE_MAX(1:NUMMAT) = ZERO
        NLOC_DMG%DENS(1:NUMMAT)   = ZERO
        NLOC_DMG%DAMP(1:NUMMAT)   = ZERO
        NLOC_DMG%SSPNL(1:NUMMAT)  = ZERO
      ENDIF
c
c--------------------------------------------------
c     START BROWSING NONLOCAL MODELS
c--------------------------------------------------
c
      CALL HM_OPTION_START('/NONLOCAL')
c
c--------------------------------------------------
      DO INL = 1,NNLOC
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID = MAT_ID ,
     .                          UNIT_ID   = UID    )
c--------------------------------------------------
c       Check MAT_ID
c--------------------------------------------------
        FLAGMAT  = 0
        ! Loop over materials
        DO IMAT=1,NUMMAT-1
          ! If found
          IF (MAT_ID == IPM(1,IMAT)) THEN
            FLAGMAT = 1
            CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
            EXIT
          ENDIF
        ENDDO
        IF (MAT_ID > 0 .AND. FLAGMAT == 0) THEN
          CALL ANCMSG(MSGID=1663,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= MAT_ID, C1='NONLOCAL', C2='NONLOCAL', C3='')
          CYCLE                           
        ENDIF
c--------------------------------------------------
c       Check UNIT_ID
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
     .                C1='NONLOCAL',
     .                C2='NONLOCAL',
     .                C3= '')                             
        ENDIF
c--------------------------------------------------
c       Read card
c--------------------------------------------------
        CALL HM_GET_FLOATV('LENGTH' ,RLEN    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('LE_MAX' ,LE_MAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------
c       Check value
c--------------------------------------------------
        IF (LE_MAX > ZERO) THEN 
          ! Save maximal target element length
          NLOC_DMG%LE_MAX(IMAT) = LE_MAX
          ! Compute the equivalent non-local internal length
          CALL GET_LENGTH(NLOC_DMG%LEN(IMAT),NLOC_DMG%LE_MAX(IMAT))
        ELSE
          ! Save the non-local internal length
          NLOC_DMG%LEN(IMAT) = MAX(ZERO,RLEN)
          ! Compute the corresponding maximal target element length
          CALL GET_LEMAX(NLOC_DMG%LE_MAX(IMAT),NLOC_DMG%LEN(IMAT))
        ENDIF
        ! Flag for non-local material
        IPM(253,IMAT) = 1   
        ! Number of the material law
        ILAW = IPM(2,IMAT)       
        ! Flag of saving non-local plastic strain and strain rate in buffer
        MLAW_TAG(IMAT)%G_PLANL  = 1
        MLAW_TAG(IMAT)%L_PLANL  = 1
        MLAW_TAG(IMAT)%G_EPSDNL = 1
        MLAW_TAG(IMAT)%L_EPSDNL = 1                           
c--------------------------------------------------
c       Printout values
c--------------------------------------------------
        WRITE(IOUT,2000) MAT_ID,ILAW,NLOC_DMG%LEN(IMAT),NLOC_DMG%LE_MAX(IMAT)
c
      ENDDO
c-----------------------------------------      
 2000 FORMAT(/
     & 5X,'    NON-LOCAL REGULARIZATION  ',/,
     & 5X,'    ------------------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW  . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'NON-LOCAL INTERNAL LENGTH . . . . . . . . . . =',1PG20.13/,
     & 5X,'MAXIMAL ELEMENT LENGTH TARGET . . . . . . . . =',1PG20.13/)
c-----------------------------------------      
      RETURN
      END
