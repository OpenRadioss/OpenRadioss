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
Chd|  CHECK_MAT_ELEM_PROP_COMPATIBILITYsource/materials/mat/check_mat_elem_prop_compatibility.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CHECK_MAT_ELEM_PROP_COMPATIBILITY(
     .  ELBUF_STR,IPARG    ,IPM      ,IGEO     ,NUMMAT   ,NUMGEO   ,
     .  NGROUP   ,NPARG    ,NPROPMI  ,NPROPGI  ,MAT_PARAM,N2D      ,
     .  IXT      ,NUMELT   ,IXP      ,NUMELP   ,IXR      ,NUMELR   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
      USE STACK_MOD
      USE MESSAGE_MOD      
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPARG(NPARG,NGROUP),N2D,
     .                       IPM(NPROPMI,NUMMAT),IGEO(NPROPGI,NUMGEO),
     .                       NUMMAT,NUMGEO,NGROUP,NPARG,NPROPMI,NPROPGI,
     .                       IXT(NIXT,NUMELT),IXP(NIXP,NUMELP),
     .                       IXR(NIXR,NUMELR),NUMELT,NUMELP,NUMELR
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP),INTENT(IN) :: ELBUF_STR
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG,IGTYP,PID,PROP_ID,NLAY,IL,IMAT,ILAW,MAT_ID,K,ITY,ISNOD,NFT
      LOGICAL :: COMPAT_ELEM,COMPAT_PROP
      CHARACTER (LEN = 10) :: ELTYP
c
      !===============================================================================
      ! CHECK MATERIAL AND PROPERTY COMPATIBILITY
      !===============================================================================
      ! Loop over groups
      DO NG = 1,NGROUP
c
        ! Group parameters
        ITY   = IPARG(5,NG)         ! -> Element type 
        IGTYP = ELBUF_STR(NG)%IGTYP ! -> Property type 
        NLAY  = ELBUF_STR(NG)%NLAY  ! -> Number of layers 
c
        !-----------------------------------------------------------------
        ! Solid, Shell, Beam and SPH
        !-----------------------------------------------------------------
        IF (NLAY > 0) THEN
c
          ! Property parameters
          PID = IPARG(62,NG)    ! -> Property internal ID 
          PROP_ID = IGEO(1,PID) ! -> Property user ID
c 
          ! Loop over layers 
          DO IL = 1,NLAY
c              
            ! Layer parameters                                         
            IMAT   = ELBUF_STR(NG)%BUFLY(IL)%IMAT  ! -> Material internal ID 
            ILAW   = ELBUF_STR(NG)%BUFLY(IL)%ILAW  ! -> Material law type
            MAT_ID = IPM(1,IMAT) ! -> Material user ID
c
            ! ==============================================================
            ! CHECK MATERIAL LAW AND ELEMENT COMPATIBILITY
            ! ==============================================================
            COMPAT_ELEM = .TRUE. 
            SELECT CASE (ITY)
              ! ------------------------------------------------------------
              ! SOLID ELEMENTS
              ! ------------------------------------------------------------ 
              CASE (1)
                IF (MAT_PARAM(IMAT)%PROP_SOLID == 0) COMPAT_ELEM = .FALSE.
                ISNOD = IPARG(28,NG) ! -> Number of solid nodes 
                IF (ISNOD == 4) THEN 
                  ELTYP = 'TETRA4'
                ELSEIF (ISNOD == 6) THEN 
                  ELTYP = 'PENTA6'
                ELSEIF (ISNOD == 10) THEN 
                  ELTYP = 'TETRA10'
                ELSEIF (ISNOD == 16) THEN 
                  ELTYP = 'SHEL16'
                ELSEIF (ISNOD == 20) THEN
                  ELTYP = 'BRIC20'
                ELSE 
                  ELTYP = 'BRICK'
                ENDIF
              ! ------------------------------------------------------------
              ! QUAD
              ! ------------------------------------------------------------ 
              CASE (2)
                IF (MAT_PARAM(IMAT)%PROP_SOLID == 0) COMPAT_ELEM = .FALSE.
                ELTYP = 'QUAD'
              ! ------------------------------------------------------------
              ! SHELLS
              ! ------------------------------------------------------------
              ! 4 nodes shell elements 
              CASE (3)
                IF (MAT_PARAM(IMAT)%PROP_SHELL == 0) COMPAT_ELEM = .FALSE.
                ELTYP = 'SHELL'
              ! 3 nodes shell elements or triangles
              CASE (7,8)
                IF (N2D > 0) THEN 
                  IF (ILAW /= 151) COMPAT_ELEM = .FALSE.
                  ELTYP = 'TRIA'
                ELSE 
                  IF (MAT_PARAM(IMAT)%PROP_SHELL == 0) COMPAT_ELEM = .FALSE.
                  ELTYP = 'SH3N'
                ENDIF
              ! ------------------------------------------------------------
              ! BEAMS
              ! ------------------------------------------------------------ 
              CASE (5)
                IF (MAT_PARAM(IMAT)%PROP_BEAM == 0) COMPAT_ELEM = .FALSE.
                ELTYP = 'BEAM'   
              ! ------------------------------------------------------------
              ! SPH
              ! ------------------------------------------------------------ 
              CASE (51)
                IF (MAT_PARAM(IMAT)%PROP_SPH == 0) COMPAT_ELEM = .FALSE.
                ELTYP = 'SPH'            
            END SELECT
            ! ==============================================================
c
            ! ==============================================================
            ! CHECK MATERIAL LAW AND PROPERTY COMPATIBILITY
            ! ==============================================================
            COMPAT_PROP = .TRUE.
            SELECT CASE (IGTYP)
              ! ------------------------------------------------------------
              ! SHELL PROPERTIES
              ! ------------------------------------------------------------ 
              ! Isotropic shells property
              CASE (1)
                IF (MAT_PARAM(IMAT)%PROP_SHELL /= 1 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 5) COMPAT_PROP = .FALSE.
              ! Orthotropic shells property
              CASE (9)
                IF (MAT_PARAM(IMAT)%PROP_SHELL /= 1 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 2 .AND.                   
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 5) COMPAT_PROP = .FALSE.
              ! Composite shells property
              CASE (10,11,17,19,51,52)
                IF (MAT_PARAM(IMAT)%PROP_SHELL /= 1 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 2 .AND.   
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 3 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 4 .AND.                           
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 5) COMPAT_PROP = .FALSE.
              ! Anisotropic shells property
              CASE (16)
                IF (MAT_PARAM(IMAT)%PROP_SHELL /= 4 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SHELL /= 5) COMPAT_PROP = .FALSE. 
              ! ------------------------------------------------------------
              ! SOLID PROPERTIES
              ! ------------------------------------------------------------ 
              ! Isotropic solids property
              CASE (14,20)
                IF (MAT_PARAM(IMAT)%PROP_SOLID /= 1 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 5 .AND.
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 6) COMPAT_PROP = .FALSE.
              ! Orthotropic solids property
              CASE (6,21)
                IF (MAT_PARAM(IMAT)%PROP_SOLID /= 1 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 2 .AND.               
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 6) COMPAT_PROP = .FALSE.
              ! Composite solids property
              CASE (22) 
                IF (MAT_PARAM(IMAT)%PROP_SOLID /= 1 .AND. 
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 2 .AND.  
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 3 .AND.                   
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 6) COMPAT_PROP = .FALSE.
              ! Cohesive solids property
              CASE (43)
                IF (MAT_PARAM(IMAT)%PROP_SOLID /= 4 .AND.                   
     .              MAT_PARAM(IMAT)%PROP_SOLID /= 6) COMPAT_PROP = .FALSE.
              ! Porous solids property
              CASE (15)          
                IF (MAT_PARAM(IMAT)%PROP_SOLID /= 5) COMPAT_PROP = .FALSE.
              ! ------------------------------------------------------------
              ! BEAM PROPERTY
              ! ------------------------------------------------------------
              ! Integrated beams property
              CASE (18)
                IF (MAT_PARAM(IMAT)%PROP_BEAM /= 2 .AND.                   
     .              MAT_PARAM(IMAT)%PROP_BEAM /= 3) COMPAT_PROP = .FALSE.
              ! ------------------------------------------------------------
              ! SPH PROPERTY
              ! ------------------------------------------------------------
              ! SPH property
              CASE (34)
                IF (MAT_PARAM(IMAT)%PROP_SPH /= 1) COMPAT_PROP = .FALSE.
            END SELECT   
            ! ==============================================================
c
            ! ==============================================================
            ! SAVE ERROR MESSAGES IF NOT COMPATIBLE
            ! ==============================================================
            ! -> Not compatible with element type
            IF (.NOT. COMPAT_ELEM) THEN 
              CALL ANCMSG(MSGID=3046,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1=ELTYP, 
     .                    I1=MAT_ID,    
     .                    I2=ILAW,
     .                    PRMOD=MSG_CUMU)
            ! -> Not compatible with property type
            ELSEIF ((COMPAT_ELEM).AND.(.NOT. COMPAT_PROP)) THEN 
              CALL ANCMSG(MSGID=3047,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=PROP_ID, 
     .                    I2=IGTYP,
     .                    I3=MAT_ID,    
     .                    I4=ILAW,
     .                    PRMOD=MSG_CUMU)
            ENDIF
            ! ==============================================================
          ENDDO
c
        !-----------------------------------------------------------------
        ! Truss et Beam type 3
        !-----------------------------------------------------------------
        ELSE 
c
          ! First node of the group
          NFT = IPARG(3,NG)
c
          ! ==============================================================
          ! CHECK MATERIAL LAW AND ELEMENT COMPATIBILITY
          ! ==============================================================
          COMPAT_ELEM = .TRUE. 
          SELECT CASE (ITY)
            ! ------------------------------------------------------------
            ! TRUSS
            ! ------------------------------------------------------------ 
            CASE (4)
              IMAT = IXT(1,NFT+1) ! -> Material internal ID
              PID  = IXT(4,NFT+1) ! -> Property internal ID 
              IF (MAT_PARAM(IMAT)%PROP_TRUSS == 0) COMPAT_ELEM = .FALSE.
              ELTYP = 'TRUSS'  
            ! ------------------------------------------------------------
            ! BEAM
            ! ------------------------------------------------------------ 
            CASE (5)
              IMAT = IXP(1,NFT+1) ! -> Material internal ID
              PID  = IXP(5,NFT+1) ! -> Property internal ID  
              IF (MAT_PARAM(IMAT)%PROP_BEAM == 0) COMPAT_ELEM = .FALSE.
              ELTYP = 'BEAM'   
            ! ------------------------------------------------------------
            ! SPRING
            ! ------------------------------------------------------------   
            CASE (6)
              IMAT = IXR(5,NFT+1) ! -> Material internal ID
              PID  = IXR(1,NFT+1) ! -> Property internal ID 
              IF (IMAT > 0) THEN 
                IF (MAT_PARAM(IMAT)%PROP_SPRING == 0) COMPAT_ELEM = .FALSE.
              ENDIF      
              ELTYP = 'SPRING'            
          END SELECT
          ! ==============================================================
c
          ! ==============================================================
          ! CHECK MATERIAL LAW AND PROPERTY COMPATIBILITY
          ! ==============================================================
          COMPAT_PROP = .TRUE.
          SELECT CASE (IGTYP)
            ! ------------------------------------------------------------
            ! TRUSS PROPERTY
            ! ------------------------------------------------------------
            ! Truss property
            CASE (2)
              IF (MAT_PARAM(IMAT)%PROP_TRUSS /= 1) COMPAT_PROP = .FALSE.
            ! ------------------------------------------------------------
            ! BEAM PROPERTY
            ! ------------------------------------------------------------
            ! Classic beams property
            CASE (3)
              IF (MAT_PARAM(IMAT)%PROP_BEAM /= 1 .AND.                   
     .            MAT_PARAM(IMAT)%PROP_BEAM /= 3) COMPAT_PROP = .FALSE.
            ! ------------------------------------------------------------
            ! SPRING PROPERTIES
            ! ------------------------------------------------------------
            ! Predit springs property
            CASE (36)
              IF (IMAT > 0) THEN 
                IF (MAT_PARAM(IMAT)%PROP_SPRING /= 1 .AND.                   
     .              MAT_PARAM(IMAT)%PROP_SPRING /= 3) COMPAT_PROP = .FALSE.
              ENDIF
            ! Material springs property
            CASE (23)
              IF (MAT_PARAM(IMAT)%PROP_SPRING /= 2 .AND.                   
     .            MAT_PARAM(IMAT)%PROP_SPRING /= 3) COMPAT_PROP = .FALSE.
            ! ------------------------------------------------------------
            ! OTHER PROPERTIES WITH NO MATERIAL
            ! ------------------------------------------------------------  
            CASE DEFAULT 
              COMPAT_ELEM = .TRUE.
          END SELECT 
          ! ==============================================================  
c
          ! Property parameters
          PROP_ID = IGEO(1,PID) ! -> Property user ID
          MAT_ID  = IPM(1,IMAT) ! -> Material user ID
          ILAW    = IPM(2,IMAT) ! -> Material law type
c
          ! ==============================================================
          ! SAVE ERROR MESSAGES IF NOT COMPATIBLE
          ! ==============================================================
          ! -> Not compatible with element type
          IF (.NOT. COMPAT_ELEM) THEN 
            CALL ANCMSG(MSGID=3046,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1=ELTYP, 
     .                  I1=MAT_ID,    
     .                  I2=ILAW,
     .                  PRMOD=MSG_CUMU)
          ! -> Not compatible with property type
          ELSE IF ((COMPAT_ELEM).AND.(.NOT. COMPAT_PROP)) THEN 
            CALL ANCMSG(MSGID=3047,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=PROP_ID, 
     .                  I2=IGTYP,
     .                  I3=MAT_ID,    
     .                  I4=ILAW,
     .                  PRMOD=MSG_CUMU)
          ENDIF
          ! ==============================================================
        ENDIF
c
      ENDDO 
c
      ! ==============================================================
      ! PRINTING OUT ERROR MESSAGES
      ! ==============================================================
      CALL ANCMSG(MSGID=3046,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3047,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
      !===============================================================================
c
      END