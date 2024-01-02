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
Chd|  HM_SURFGR2                    source/groups/hm_surfgr2.F    
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_SURFGR2(NGRELE  ,ELCHAR ,NUMEL ,ID   ,
     .                   IGRELEM ,TAGBUF ,TITR  ,TITR1,
     .                   INDX    ,NINDX  ,FLAG  ,NINDX_SOL,NINDX_SOL10,
     .                   INDX_SOL,INDX_SOL10 ,FLAG_GRBRIC,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL,NGRELE,INDX(*),NINDX,
     .      NEMAX,ID,TAGBUF(*),
     .      FLAG
      CHARACTER ELCHAR*(*)
      CHARACTER*nchartitle,
     .   TITR,TITR1
      LOGICAL :: FLAG_GRBRIC
      INTEGER :: NINDX_SOL, NINDX_SOL10
      INTEGER, DIMENSION(*) :: INDX_SOL, INDX_SOL10
!
      TYPE (GROUP_)  , DIMENSION(NGRELE) :: IGRELEM
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       
!       FLAG_GRBRIC : flag to initialize the INDX_SOL(10) arrays
!                     and optimize an old and expensive treatment in SSURFTAG      
!       NINDX_SOL(10) : number of the tagged solid(10) element
!                      --> need to split solid and solid10 
!                      for a treatment in the SSURFTAG routine
!                      only useful for /SURF/GRBRIC
!       INDX_SOL(10) : ID of the tagged solid(10) element
!                      --> need to split solid and solid10 
!                      for a treatment in the SSURFTAG routine
!                      only useful for /SURF/GRBRIC
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,L,IE,IADV,K,ISU, NENTITY, KK ,JJ, NENTITY_POS,NENTITY_NEG
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED
      INTEGER,DIMENSION(:),ALLOCATABLE :: TAG_ENTITY_POS, TAG_ENTITY_NEG,LIST_ENTITY
!     FLAG_GRBRIC : check if the option is /GCBRIC
C-----------------------------------------------
      INTERFACE
       SUBROUTINE GROUPS_GET_ELEM_LIST(arg1,arg2,arg3)
        USE SUBMODEL_MOD
        INTEGER,DIMENSION(:),ALLOCATABLE       :: arg1
        INTEGER,INTENT(INOUT)                  :: arg2
        TYPE(SUBMODEL_DATA)                    :: arg3(NSUBMOD)
       END SUBROUTINE  
      END INTERFACE     
C=======================================================================
      IF(ELCHAR(1:6)=='GRBRIC') FLAG_GRBRIC = .TRUE.

        CALL GROUPS_GET_ELEM_LIST(LIST_ENTITY, NENTITY, LSUBMODEL)       
        DO KK=1,NENTITY                                                              
          JJ=LIST_ENTITY(KK)      
          IF(JJ/=0)THEN
            ISU=0
            DO K=1,NGRELE
              IF (JJ == IGRELEM(K)%ID) ISU=K
              IF (JJ ==-IGRELEM(K)%ID) ISU=-K
            ENDDO
            IF (ISU > 0)THEN
              DO L=1,IGRELEM(ISU)%NENTITY
                IE=IGRELEM(ISU)%ENTITY(L)
                IF(TAGBUF(IE)==0)THEN
                  TAGBUF(IE)=1
                  NINDX=NINDX+1
                  INDX(NINDX)=IE
                  IF(FLAG_GRBRIC) THEN
                        IF(IE<=NUMELS8) THEN
                                NINDX_SOL=NINDX_SOL+1
                                INDX_SOL(NINDX_SOL)=IE
                        ELSE
                                NINDX_SOL10=NINDX_SOL10+1
                                INDX_SOL10(NINDX_SOL10)=IE
                        ENDIF
                  ENDIF
                END IF
              ENDDO
            ELSEIF (ISU < 0)THEN
              DO L=1,IGRELEM(-ISU)%NENTITY
                IE=IGRELEM(-ISU)%ENTITY(L)
                IF(TAGBUF(IE)==0)THEN
                  TAGBUF(IE)=-1
                  NINDX=NINDX+1
                  INDX(NINDX)=IE
                  IF(FLAG_GRBRIC) THEN
                        IF(IE<=NUMELS8) THEN
                                NINDX_SOL=NINDX_SOL+1
                                INDX_SOL(NINDX_SOL)=IE
                        ELSE
                                NINDX_SOL10=NINDX_SOL10+1
                                INDX_SOL10(NINDX_SOL10)=IE
                        ENDIF
                  ENDIF
                END IF
              ENDDO
            ELSEIF(FLAG==0)THEN
               CALL ANCMSG(MSGID=192,MSGTYPE=MSGWARNING,ANMODE=ANINFO,C1=TITR1,C2=TITR1,C3=TITR,C4=TITR1,I1=ID,C5=ELCHAR,I2=JJ)
            ENDIF
          ENDIF
        ENDDO
        IF(ALLOCATED(LIST_ENTITY))DEALLOCATE (LIST_ENTITY)        
C-----------
      RETURN
      END
