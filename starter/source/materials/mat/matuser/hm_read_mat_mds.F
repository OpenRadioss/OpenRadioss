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
Chd|  HM_READ_MAT_MDS               source/materials/mat/matuser/hm_read_mat_mds.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        ST_READ_MDS                   source/user_interface/mds_userlib.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT_MDS(ILAW,IL,MATID,
     .                           UPARAM,MAXUPARAM,NUPARAM,
     .                           NUVAR ,IFUNC,MAXFUNC,NFUNC,
     .                           LSUBMODEL,
     .                           PM )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Read user material 29 to 31 - generic routine
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     USERL_AVAIL   Flag if userlibrary was load
C     MATNUM        Number of Material 200
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C------------------------------------------------------------------
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILAW,IL,MATID
      INTEGER MAXUPARAM
      INTEGER NUPARAM 
      INTEGER NUVAR
      INTEGER NFUNC,MAXFUNC
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      INTEGER, DIMENSION(MAXFUNC)     :: IFUNC
      my_real, DIMENSION(MAXUPARAM)   :: UPARAM
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CLAW
      CHARACTER(LEN=nchartitle) :: TITLE
      LOGICAL :: IS_AVAILABLE
      INTEGER NLINES,J,LENGTH
      INTEGER SCR_FILE_NAME_LEN,OPEN_RESULT,LEN_FILNAM
      CHARACTER*256 MDS_FILES(3)
      CHARACTER(len=4096) :: TMP_NAME
      CHARACTER(len=512)  :: MDS_LIBNAME
      INTEGER, DIMENSION(:),ALLOCATABLE :: MDS_LABEL_SIZES
      INTEGER LF1,LF2,LF3,LLABEL,MDS_DEPVAR
      my_real :: MDS_RHO0,STIFINT,YOUNG,NU
      CHARACTER (LEN=64),DIMENSION(:),ALLOCATABLE :: MDS_LABEL
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
!
      IF (MDS_AVAIL == 1) THEN
      ! MDS material interface

        CALL HM_GET_INTV ('Number_of_datalines' ,NLINES ,IS_AVAILABLE, LSUBMODEL)

        ! Create tempo file  
        WRITE(CLAW,'(I4.4)')ILAW
        SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CLAW//'.scr'
        SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
        OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)

        IF(NLINES > 1)THEN
!
        ! Read & Dump in scratch file
          DO J=1,NLINES   
           CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
           WRITE(30,FMT='(A)')TRIM(RLINE)
          ENDDO
        ENDIF
        CLOSE(UNIT=30)


        MDS_FILES(1:3)=''

        IF ( .NOT. ALLOCATED(MDS_LABEL) ) ALLOCATE(MDS_LABEL(1024))
        IF ( .NOT. ALLOCATED(MDS_LABEL_SIZES) ) ALLOCATE(MDS_LABEL_SIZES(1024))
               
        CALL RAD_UMAT_INPUT_OPEN(SCR_FILE_NAME,SCR_FILE_NAME_LEN,OPEN_RESULT)

        CALL ST_READ_MDS(  UPARAM ,MAXUPARAM,NUPARAM,
     .                     NUVAR  ,IFUNC ,MAXFUNC,NFUNC   ,
     .                     STIFINT,YOUNG ,NU ,MDS_RHO0, MDS_FILES,MDS_LABEL, MDS_DEPVAR )

        PM( 1) = MDS_RHO0
        PM(89) = MDS_RHO0

        CALL RAD_UMAT_CLOSE_INPUT()

        LF1 = LEN_TRIM(MDS_FILES(1))
        LF2 = LEN_TRIM(MDS_FILES(2))
        LF3 = LEN_TRIM(MDS_FILES(3))

        DO J=1,MDS_DEPVAR
          MDS_LABEL_SIZES(J)=LEN_TRIM(MDS_LABEL(J))
        ENDDO

        CALL ADD_MDS_MATERIAL ( MATID,
     *                          MDS_FILES(1), LF1,
     *                          MDS_FILES(2), LF2,
     *                          MDS_FILES(3), LF3,
     *                          MDS_LABEL, MDS_LABEL_SIZES,
     *                          MDS_DEPVAR )

      ELSE
        CALL MDS_USERLIB_NAME_GET(MDS_LIBNAME,LENGTH)
        CALL ANCMSG(MSGID=1883,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANSTOP,
     .              C1=MDS_LIBNAME(1:LENGTH) )

      ENDIF ! IF(MDS_AVAIL == 1)
!----------------
      RETURN
 999  CALL ANCMSG(MSGID=55,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=KEY0(KCUR),
     .            C2=KLINE,
     .            C3=LINE)
      CALL ARRET(2)
      END
