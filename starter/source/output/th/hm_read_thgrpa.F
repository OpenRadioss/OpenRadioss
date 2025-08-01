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
!||    hm_read_thgrpa           ../starter/source/output/th/hm_read_thgrpa.F
!||--- called by ------------------------------------------------------
!||    hm_read_thgrou           ../starter/source/output/th/hm_read_thgrou.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_thvarc                ../starter/source/output/th/hm_read_thvarc.F
!||    r2r_exist                ../starter/source/coupling/rad2rad/routines_r2r.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_THGRPA(
     .           IPASU    ,NPASU    ,ITHBUF   ,IAD      ,IFI      ,
     .           VARPA    ,NVARPA   ,VARG     ,NVARG    ,NUMTHPART,
     .           IVARPAG  ,PATHID   ,TAGP     ,IPARTH   ,NPARTH   , 
     .           NVPARTH  ,LSUBMODEL)
C-----------------------------------------------
        USE MESSAGE_MOD
        USE SUBMODEL_MOD
        USE HM_OPTION_READ_MOD
        USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "r2r_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD,IFI,NVARPA,NUMTHPART,NVARG,
     .        NPARTH,NVPARTH,TAGP,NPASU
      INTEGER ,DIMENSION(*)        :: ITHBUF,PATHID
      INTEGER ,DIMENSION(NPARTH,*) :: IPARTH
      INTEGER ,DIMENSION(NPASU,*)  :: IPASU
      INTEGER ,DIMENSION(18,*)     :: IVARPAG
      TYPE(SUBMODEL_DATA),DIMENSION(NSUBMOD) :: LSUBMODEL
      CHARACTER*10 VARPA(NVARPA),VARG(NVARG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,TH_ID,PART_ID,NVAR,ITYP,NUMOBJ,PART_ID_OBJ1
      CHARACTER(LEN=NCHARKEY)::KEY
      CHARACTER(LEN=NCHARTITLE)::TITR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER ,EXTERNAL :: R2R_EXIST,HM_THVARC
C=======================================================================
      IS_AVAILABLE = .FALSE.
      ITYP = 1001     ! type /th/part

      ! read option header
      CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=TH_ID, OPTION_TITR=TITR, KEYWORD2=KEY)   
c
      ! Number of variables counted in input line
      CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL) 
c
      ! Total number of stored variables
      IF (NVAR > 0) THEN
        NVAR = HM_THVARC(VARPA,NVARPA,ITHBUF(IAD),VARG,NVARG,IVARPAG,NVARPA,TH_ID,TITR,LSUBMODEL)
      END IF
c
      IF (NVAR == 0) THEN
          CALL ANCMSG(MSGID=1109, MSGTYPE=MSGERROR , ANMODE=ANINFO_BLIND_1,
     .         I1=TH_ID,
     .         C1=TITR )
      ELSE IF (KEY(1:4) == 'PART') THEN

        ! Number of Object (Part) IDs
        CALL HM_GET_INTV('idsmax',NUMOBJ,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_INT_ARRAY_INDEX('ids',PART_ID_OBJ1,1,IS_AVAILABLE,LSUBMODEL) 
        
        IF (NUMOBJ > 0 .AND. PART_ID_OBJ1 == 0) THEN

            NUMOBJ = NUMTHPART
            
            DO K = 1,NUMOBJ
              PART_ID = IPASU(4,K)
              IF (NSUBDOM > 0) THEN 
                IF (R2R_EXIST(ITYP,PART_ID) == 0) CYCLE
              ENDIF
              N = 0
              DO J = 1,NUMTHPART
                IF (PART_ID == IPASU(4,J))THEN
                  N = J
                  TAGP = TAGP+1
                  PATHID(TAGP) = PART_ID
                  EXIT
                ENDIF  
              ENDDO
C  
              IF (N == 0) THEN
                CALL ANCMSG(MSGID=1610, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                        I1=TH_ID,
     .                        C1=TITR ,
     .                        C2=KEY  ,
     .                        I2=PART_ID  )
              ELSE
                IPARTH(NVPARTH,N)  = NVAR
                IPARTH(NVPARTH+1,N)= IAD
              ENDIF           
            ENDDO  ! NUMOBJ 

        ELSE  
      
            ! Number of Object (Part) IDs
            CALL HM_GET_INTV('idsmax',NUMOBJ,IS_AVAILABLE,LSUBMODEL)
        
            DO K = 1,NUMOBJ
              CALL HM_GET_INT_ARRAY_INDEX('ids',PART_ID,K,IS_AVAILABLE,LSUBMODEL) 
              IF (NSUBDOM > 0) THEN 
                IF (R2R_EXIST(ITYP,PART_ID) == 0) CYCLE
              ENDIF
              N = 0
              DO J = 1,NUMTHPART
                IF (PART_ID == IPASU(4,J))THEN
                  N = J
                  TAGP = TAGP+1
                  PATHID(TAGP) = PART_ID
                  EXIT
                ENDIF  
              ENDDO
C
              IF (N == 0) THEN
                CALL ANCMSG(MSGID=1610, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                      I1=TH_ID,
     .                      C1=TITR ,
     .                      C2=KEY  ,
     .                      I2=PART_ID  )
              ELSE
                IPARTH(NVPARTH,N)  = NVAR
                IPARTH(NVPARTH+1,N)= IAD
              ENDIF           
            ENDDO  ! NUMOBJ 
         ENDIF   !NUMOBJ > 0 || PART_ID_OBJ1 == 0

      END IF   ! NVAR > 0
c
      IAD = IAD + NVAR
      IFI = IFI + NVAR
c-----------
      RETURN
      END
!||====================================================================
!||    hm_read_thgrpa_sub       ../starter/source/output/th/hm_read_thgrpa.F
!||--- called by ------------------------------------------------------
!||    hm_read_thgrou           ../starter/source/output/th/hm_read_thgrou.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_thvarc                ../starter/source/output/th/hm_read_thvarc.F
!||    r2r_exist                ../starter/source/coupling/rad2rad/routines_r2r.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_THGRPA_SUB(
     1      IAD      ,IFI      ,ITHBUF   ,NVARPA   ,VARPA    ,
     2      VARG     ,NVARG    ,IVARPAG  ,PATHID   ,
     3      SUTHID   ,TAGS     ,SUBSET   ,ITHFLAG  ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
        USE SUBMODEL_MOD
        USE GROUPDEF_MOD
        USE HM_OPTION_READ_MOD
        USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD,IFI,NVARPA,NVARG
      INTEGER NVPS,TAGS,ITHFLAG
      INTEGER ,DIMENSION(*)        :: ITHBUF,PATHID,SUTHID
      INTEGER ,DIMENSION(18,*)     :: IVARPAG
      CHARACTER*10 VARPA(NVARPA),VARG(NVARG)
      TYPE(SUBMODEL_DATA),DIMENSION(NSUBMOD) :: LSUBMODEL
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,TH_ID,SUBS_ID,NVAR,ITYP,NUMOBJ,SUBS_ID_OBJ1
      CHARACTER(LEN=NCHARTITLE)::TITR
      CHARACTER(LEN=NCHARKEY)::KEY
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER ,EXTERNAL :: R2R_EXIST,HM_THVARC
C-----------------------------------------------
C=======================================================================
      IS_AVAILABLE = .FALSE.
      ITYP = 1002     ! type /th/subs
c
      ! read option header
      CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=TH_ID, OPTION_TITR=TITR, KEYWORD2=KEY)
c
      ! Number of variables counted in input line
      CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL)
c
      ! Total number of stored variables
      IF (NVAR > 0) THEN
        NVAR = HM_THVARC(VARPA,NVARPA,ITHBUF(IAD),VARG,NVARG,IVARPAG,NVARPA,TH_ID,TITR,LSUBMODEL)
      END IF
c
      IF (NVAR == 0) THEN
          CALL ANCMSG(MSGID=1109, MSGTYPE=MSGERROR , ANMODE=ANINFO_BLIND_1,
     .         I1=TH_ID,
     .         C1=TITR )
      ELSE IF (KEY(1:4) == 'SUBS') THEN
c
        ! Number of Object (SUBS) IDs
        CALL HM_GET_INTV('idsmax',NUMOBJ,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_INT_ARRAY_INDEX('ids',SUBS_ID_OBJ1,1,IS_AVAILABLE,LSUBMODEL) 
        
        IF (NUMOBJ > 0 .AND. SUBS_ID_OBJ1 == 0) THEN
          NUMOBJ = NSUBS
          DO K = 1,NUMOBJ
            SUBS_ID = SUBSET(K)%ID
            IF (NSUBDOM > 0) THEN 
              IF (R2R_EXIST(ITYP,SUBS_ID) == 0) CYCLE
            ENDIF
            N = 0
            DO J = 1,NSUBS
              IF (SUBS_ID == SUBSET(J)%ID)THEN
                N = J
                TAGS = TAGS+1
                SUTHID(TAGS)= SUBS_ID
                EXIT
              ENDIF  
            ENDDO  
C
            IF (N == 0) THEN
              CALL ANCMSG(MSGID=257, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                    I1=TH_ID,
     .                    C1=TITR,
     .                    C2=KEY,
     .                    I2=SUBS_ID)
              NVAR = 0
            ELSE
              SUBSET(N)%NVARTH(ITHFLAG) = NVAR
              SUBSET(N)%THIAD = IAD
            ENDIF      
          ENDDO  ! NUMOBJ 
          
        ELSE
c
          DO K = 1,NUMOBJ
            CALL HM_GET_INT_ARRAY_INDEX('ids',SUBS_ID,K,IS_AVAILABLE,LSUBMODEL) 
            IF (NSUBDOM > 0) THEN 
              IF (R2R_EXIST(ITYP,SUBS_ID) == 0) CYCLE
            ENDIF
            N = 0
            DO J = 1,NSUBS
              IF (SUBS_ID == SUBSET(J)%ID)THEN
                N = J
                TAGS = TAGS+1
                SUTHID(TAGS)= SUBS_ID
                EXIT
              ENDIF  
            ENDDO  
C
            IF (N == 0) THEN
              CALL ANCMSG(MSGID=257, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                    I1=TH_ID,
     .                    C1=TITR,
     .                    C2=KEY,
     .                    I2=SUBS_ID)
              NVAR = 0
            ELSE
              SUBSET(N)%NVARTH(ITHFLAG) = NVAR
              SUBSET(N)%THIAD = IAD
            ENDIF      
          ENDDO  ! NUMOBJ 
        ENDIF   !NUMOBJ > 0 || SUBS_ID_OBJ1 == 0
      END IF   ! NVAR > 0
c
      IAD = IAD + NVAR
      IFI = IFI + NVAR
c-----------
      RETURN
      END