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
Chd|  CREATE_MAP_TABLES             source/model/sets/map_tables.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MAP_ORDER                     source/model/sets/map_order.F 
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_MAP_TABLES ( MAP_TABLES ,MODE  ,
     *                               LSUBMODEL  ,SUBSET,
     *                               IPART,
     *                               IXS  ,IXQ, IXC ,IXTG ,
     *                               IXT  ,IXP ,IXR ,KXSP,LRIVET,
     *                               RBY_MSN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE MAPPING_OPTION_MOD
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE GROUPDEF_MOD 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, INTENT(in) :: MODE
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET

      INTEGER, INTENT(IN), DIMENSION(LIPART1,NPART)  :: IPART
      INTEGER, INTENT(IN), DIMENSION(NIXS,NUMELS)    :: IXS
      INTEGER, INTENT(IN), DIMENSION(NIXQ,NUMELQ)    :: IXQ
      INTEGER, INTENT(IN), DIMENSION(NIXC,NUMELC)    :: IXC
      INTEGER, INTENT(IN), DIMENSION(NIXTG,NUMELTG)  :: IXTG
      INTEGER, INTENT(IN), DIMENSION(NIXT,NUMELT)    :: IXT
      INTEGER, INTENT(IN), DIMENSION(NIXP,NUMELP)    :: IXP
      INTEGER, INTENT(IN), DIMENSION(NIXR,NUMELR)    :: IXR
      INTEGER, DIMENSION(NISP,NUMSPH), INTENT(in)    :: KXSP
      INTEGER, DIMENSION(4,NRIVET), INTENT(in)       :: LRIVET
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY)       :: RBY_MSN

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, SET_ID,NSET_G,NSET_COL,OFFSET,NSET1,NSET2
      INTEGER, DIMENSION(:),ALLOCATABLE :: ISORT,ISORT2,SAV,SAV2
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX_SORT,INDEX_SORT2
      INTEGER, DIMENSION(70000) :: IWORK
      CHARACTER KEYSET*ncharfield,SET_TYPE*ncharfield,KEY*ncharkey,KEY_TYPE*ncharfield
      CHARACTER TITLE*nchartitle,SET_TITLE*nchartitle,KEYPART*ncharkey,TITLE2*nchartitle
      CHARACTER MESS*40
      DATA MESS/'SET  GROUP DEFINITION               '/
C-----------------------------------------------
      IF(MODE==1) THEN
C Parts
          ALLOCATE( MAP_TABLES%IPARTM(NPART,2))

          ALLOCATE(ISORT(NPART))
          ALLOCATE(INDEX_SORT(2*NPART))
    
          DO I=1,NPART
             ISORT(I)=IPART(4,I)
             INDEX_SORT(I)=I
          ENDDO
          CALL MY_ORDERS(0,IWORK,ISORT,INDEX_SORT,NPART,1)
    
          DO I=1,NPART
             MAP_TABLES%IPARTM(I,1)=ISORT(INDEX_SORT(I))
             MAP_TABLES%IPARTM(I,2)=INDEX_SORT(I)
          ENDDO
    
          DEALLOCATE (ISORT)
          DEALLOCATE (INDEX_SORT)
    
C Sets
          NSET_G = 0
          NSET_COL = 0

          ! ISORT & INDEX_SORT are used for /SET/GENERAL
          ALLOCATE(ISORT(NSETS))
          ALLOCATE(SAV(NSETS))
          ALLOCATE(INDEX_SORT(2*NSETS))
    
          ! ISORT2 & INDEX_SORT2 are used for /SET/COLLECT
          ALLOCATE(ISORT2(NSETS))
          ALLOCATE(SAV2(NSETS))
          ALLOCATE(INDEX_SORT2(2*NSETS))

          CALL HM_OPTION_START('/SET')
          DO I=1,NSETS
              CALL HM_OPTION_READ_KEY (LSUBMODEL,
     .                             OPTION_ID   = SET_ID,
     .                             OPTION_TITR = SET_TITLE,
     .                             KEYWORD2    = KEY)

              IF(TRIM(KEY) == 'GENERAL')THEN

                NSET_G = NSET_G + 1
                ISORT(NSET_G)=SET_ID
                SAV(NSET_G)=I
                INDEX_SORT(NSET_G)=NSET_G

              ELSEIF (TRIM(KEY) == 'COLLECT') THEN
    
                NSET_COL = NSET_COL + 1
                ISORT2(NSET_COL)=SET_ID
                SAV2(NSET_COL)=I
                INDEX_SORT2(NSET_COL)=I
           
              ENDIF
          ENDDO

          ! Sorting SET/GENERAL
          ! -------------------
          CALL MY_ORDERS(0,IWORK,ISORT,INDEX_SORT,NSET_G,1)
          ALLOCATE( MAP_TABLES%ISETM(NSET_G,2))
          
          DO I=1,NSET_G
             MAP_TABLES%ISETM(I,1)=ISORT(INDEX_SORT(I))
             MAP_TABLES%ISETM(I,2)=SAV(INDEX_SORT(I))
          ENDDO

          ! check ID double
          IF(NSET_G > 0) THEN
            NSET1 = MAP_TABLES%ISETM(1,1)
            DO I=2,NSET_G
               NSET2 = MAP_TABLES%ISETM(I,1)
               IF (NSET2 == NSET1) THEN
               ! error
               CALL ANCMSG(MSGID=79,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1=MESS,
     .                     I1=NSET1)
               ELSE
                 NSET1 = NSET2
               ENDIF
            ENDDO
          ENDIF

          MAP_TABLES%NSET_GENERAL = NSET_G
    
    
          ! Sorting SET/COLLECT
          ! -------------------
          CALL MY_ORDERS(0,IWORK,ISORT2,INDEX_SORT2,NSET_COL,1)
          ALLOCATE( MAP_TABLES%ISETCOLM(NSET_COL,2))
          
          DO I=1,NSET_COL
             MAP_TABLES%ISETCOLM(I,1)=ISORT2(INDEX_SORT2(I))
             MAP_TABLES%ISETCOLM(I,2)=SAV2(INDEX_SORT2(I))
          ENDDO
          MAP_TABLES%NSET_COLLECT = NSET_COL
    
          ! print*,'NSETS=',NSETS
          ! print*,'SET_GENERAL : ',MAP_TABLES%NSET_GENERAL
          ! print*,'SET_COLLECT : ',MAP_TABLES%NSET_COLLECT

          ! DO I=1,NSET_COL
          !    print*,I, MAP_TABLES%ISETCOLM(I,1), MAP_TABLES%ISETCOLM(I,2)
          ! ENDDO
          ! print*,'--------------------------------------------------------'

          DEALLOCATE (ISORT)
          DEALLOCATE (INDEX_SORT)
          DEALLOCATE (SAV)

          DEALLOCATE (ISORT2)
          DEALLOCATE (INDEX_SORT2)
          DEALLOCATE (SAV2) 
C Subset
          ALLOCATE( MAP_TABLES%ISUBSM(NSUBS,2))

          ALLOCATE(ISORT(NSUBS))
          ALLOCATE(INDEX_SORT(2*NSUBS))
    
          DO I=1,NSUBS
             ISORT(I)=SUBSET(I)%ID
             INDEX_SORT(I)=I
          ENDDO
          CALL MY_ORDERS(0,IWORK,ISORT,INDEX_SORT,NSUBS,1)

          DO I=1,NSUBS
             MAP_TABLES%ISUBSM(I,1)=ISORT(INDEX_SORT(I))
             MAP_TABLES%ISUBSM(I,2)=INDEX_SORT(I)
          ENDDO

          DEALLOCATE (ISORT)
          DEALLOCATE (INDEX_SORT)
C Submodel
          ALLOCATE( MAP_TABLES%ISUBMM(NSUBMOD,2))
    
          ALLOCATE(ISORT(NSUBMOD))
          ALLOCATE(INDEX_SORT(2*NSUBMOD))

          DO I=1,NSUBMOD
             ISORT(I) = LSUBMODEL(I)%NOSUBMOD
             INDEX_SORT(I)=I
          ENDDO
          CALL MY_ORDERS(0,IWORK,ISORT,INDEX_SORT,NSUBMOD,1)

          DO I=1,NSUBMOD
             MAP_TABLES%ISUBMM(I,1)=ISORT(INDEX_SORT(I))
             MAP_TABLES%ISUBMM(I,2)=INDEX_SORT(I)
          ENDDO

          DEALLOCATE (ISORT)
          DEALLOCATE (INDEX_SORT)
C Rbody
          ALLOCATE( MAP_TABLES%IRBODYM(NRBODY,2))

          ALLOCATE(ISORT(NRBODY))
          ALLOCATE(INDEX_SORT(2*NRBODY))
    
          DO I=1,NRBODY
             ISORT(I)=RBY_MSN(1,I)
             INDEX_SORT(I)=I
          ENDDO
          CALL MY_ORDERS(0,IWORK,ISORT,INDEX_SORT,NRBODY,1)

          DO I=1,NRBODY
             MAP_TABLES%IRBODYM(I,1)=ISORT(INDEX_SORT(I))
             MAP_TABLES%IRBODYM(I,2)=INDEX_SORT(I)
          ENDDO

          DEALLOCATE (ISORT)
          DEALLOCATE (INDEX_SORT)
        ENDIF
!   ------------------------------------------------------------
!                       Element
C Solid
        IF(MODE==1) ALLOCATE( MAP_TABLES%ISOLM(NUMELS,2))
        CALL MAP_ORDER(IXS,NIXS,NIXS,NUMELS,MAP_TABLES%ISOLM)       
C Quad
        IF(MODE==1) ALLOCATE( MAP_TABLES%IQUADM(NUMELQ,2))
        CALL MAP_ORDER(IXQ,NIXQ,NIXQ,NUMELQ,MAP_TABLES%IQUADM)           
C Shell
        IF(MODE==1) ALLOCATE( MAP_TABLES%ISH4NM(NUMELC,2))
        CALL MAP_ORDER(IXC,NIXC,NIXC,NUMELC,MAP_TABLES%ISH4NM)  
C Sh3n
        IF(MODE==1) ALLOCATE( MAP_TABLES%ISH3NM(NUMELTG,2))
        CALL MAP_ORDER(IXTG,NIXTG,NIXTG,NUMELTG,MAP_TABLES%ISH3NM)      
C Tria
        IF(MODE==1) ALLOCATE( MAP_TABLES%ITRIAM(NUMELTG,2))
        CALL MAP_ORDER(IXTG,NIXTG,NIXTG,NUMELTG,MAP_TABLES%ITRIAM)
C Truss
        IF(MODE==1) ALLOCATE( MAP_TABLES%ITRUSSM(NUMELT,2))
        CALL MAP_ORDER(IXT,NIXT,NIXT,NUMELT,MAP_TABLES%ITRUSSM)
C Beam
        IF(MODE==1) ALLOCATE( MAP_TABLES%IBEAMM(NUMELP,2))
        CALL MAP_ORDER(IXP,NIXP,NIXP,NUMELP,MAP_TABLES%IBEAMM)     
C Spring
        IF(MODE==1) ALLOCATE( MAP_TABLES%ISPRINGM(NUMELR,2))
        CALL MAP_ORDER(IXR,NIXR,NIXR,NUMELR,MAP_TABLES%ISPRINGM)    
C SPH 
        IF(MODE==1) ALLOCATE( MAP_TABLES%ISPHM(NUMSPH,2))
        MAP_TABLES%ISPHM(1:NUMSPH,1:2) = 0
        CALL MAP_ORDER(KXSP,NISP,NISP,NUMSPH,MAP_TABLES%ISPHM)
!   ------------------------------------------------------------
        IF(MODE==2) THEN
C Rivet 
          ALLOCATE( MAP_TABLES%IRIVETM(NRIVET,2))
          CALL MAP_ORDER(LRIVET,4,4,NRIVET,MAP_TABLES%IRIVETM)
        ENDIF
!---
      RETURN
      END
