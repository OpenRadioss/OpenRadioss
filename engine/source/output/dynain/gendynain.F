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
!||    gendynain          ../engine/source/output/dynain/gendynain.F
!||--- called by ------------------------------------------------------
!||    sortie_main        ../engine/source/output/sortie_main.F
!||--- calls      -----------------------------------------------------
!||    ancmsg             ../engine/source/output/message/message.F
!||    arret              ../engine/source/system/arret.F
!||    close_c            ../common_source/tools/input_output/write_routtines.c
!||    cur_fil_c          ../common_source/tools/input_output/write_routtines.c
!||    dynain_c_strag     ../engine/source/output/dynain/dynain_c_strag.F
!||    dynain_c_strsg     ../engine/source/output/dynain/dynain_c_strsg.F
!||    dynain_node        ../engine/source/output/dynain/dynain_node.F
!||    dynain_shel_mp     ../engine/source/output/dynain/dynain_shel_mp.F
!||    dynain_shel_spmd   ../engine/source/output/dynain/dynain_shel_spmd.F
!||    dynain_size_c      ../engine/source/output/dynain/dynain_size.F
!||    open_c             ../common_source/tools/input_output/write_routtines.c
!||    spmd_outpitab      ../engine/source/mpi/interfaces/spmd_outp.F
!||    strs_txt50         ../engine/source/output/sta/sta_txt.F
!||--- uses       -----------------------------------------------------
!||    drape_mod          ../engine/share/modules/drape_mod.F
!||    elbufdef_mod       ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    inoutfile_mod      ../common_source/modules/inoutfile_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    message_mod        ../engine/share/message_module/message_mod.F
!||    stack_mod          ../engine/share/modules/stack_mod.F
!||    state_mod          ../common_source/modules/state_mod.F
!||====================================================================
       SUBROUTINE GENDYNAIN(X  ,ELBUF_TAB, BUFEL  ,IXC      ,IXTG     ,
     2                IPARG    ,IPM      , IGEO   ,ITAB     ,IPART    ,
     3                PM       ,GEO      , IPARTC ,IPARTTG  ,LENG     ,
     4                LENGC    ,LENGTG   , WEIGHT ,NODGLOB  ,THKE     ,               
     5                NPBY     ,LPBY     , STACK  ,DRAPE_SH4N ,DRAPE_SH3N , 
     6                DYNAIN_DATA,DRAPEG ,MAT_PARAM) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MATPARAM_DEF_MOD
      USE MESSAGE_MOD    
      USE INOUTFILE_MOD 
      USE STACK_MOD
      USE DRAPE_MOD  
      USE STATE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(*),
     .   IXC(NIXC,*),IXTG(NIXTG,*),IPM(*),IGEO(*),
     .   ITAB(*) ,IPART(LIPART1,*) ,IPARTC(*) ,IPARTTG(*),
     .   WEIGHT(*), NODGLOB(*), NPBY(NNPBY,*), LPBY(*) 
      INTEGER LENG,LENGC,LENGTG
      my_real
     .   X(*), BUFEL(*),
     .   PM(NPROPM,*), GEO(NPROPG,*) ,THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_SH4N(NUMELC_DRAPE),DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER CHSTAT*4,FILNAM*100,T10*10,MES*40
      INTEGER FILEN,I,INNODA,IERR,J,N
      INTEGER LENR,SIZLOC,SIZP0
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: ITABG, NODTAG ,DYNAIN_INDXC ,
     .             DYNAIN_INDXTG
      INTEGER CTEXT(2149)
      double precision
     .      ,  DIMENSION(:),ALLOCATABLE :: WA,WAP0

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
      LOGICAL IS_FILE_TO_BE_WRITTEN
      CHARACTER*100 LINE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C===============================================|
C   OPEN FILE
C-----------------------------------------------
      IF(DYNAIN_DATA%IDYNAINF>=10000)DYNAIN_DATA%IDYNAINF=1
      WRITE(CHSTAT,'(I4.4)')DYNAIN_DATA%IDYNAINF
      IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN ! Not zipped file
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHSTAT//'.dynain'
        FILEN = ROOTLEN + 12
        LEN_TMP_NAME = OUTFILE_NAME_LEN + FILEN
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:FILEN)
        IF(ISPMD == 0) THEN
           OPEN(UNIT=IUDYNAIN,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')
           WRITE(IUDYNAIN,'(2A)')'$RADIOSS DYNAIN FILE ',FILNAM(1:FILEN)
        END IF
      ELSE ! zipped file
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHSTAT//'.dynain'
        FILEN = ROOTLEN + 12
        LEN_TMP_NAME = OUTFILE_NAME_LEN + FILEN
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:FILEN)
        DO I=1,LEN_TMP_NAME
           CTEXT(I)=ICHAR(TMP_NAME(I:I))
           CALL CUR_FIL_C(0)
        ENDDO
        IF(ISPMD == 0) THEN 
           CALL OPEN_C(CTEXT,LEN_TMP_NAME,6)
           WRITE(LINE,'(2A)') '$RADIOSS DYNAIN FILE ',FILNAM(1:FILEN)
           CALL STRS_TXT50(LINE,100)
        ENDIF
      ENDIF

c       

C-----------------------
C    Allocation Tabs
C-----------------------
      ALLOCATE(NODTAG(NUMNOD),STAT=IERR)
      ALLOCATE(ITABG(LENG),STAT=IERR)
      ALLOCATE(DYNAIN_INDXC(2*LENGC),STAT=IERR)
      ALLOCATE(DYNAIN_INDXTG(2*LENGTG),STAT=IERR)
C-----------------------------------------------
C   CONNECTIVITIES + NODAL COORDINATES
C-----------------------------------------------
c     
      IF (NSPMD > 1)CALL SPMD_OUTPITAB(ITAB,WEIGHT,NODGLOB,ITABG)

      NODTAG=0

      DYNAIN_DATA%DYNAIN_NUMELC =0
      DYNAIN_DATA%DYNAIN_NUMELTG =0

      IF(NSPMD == 1)THEN
        !  - shells -   
        CALL DYNAIN_SHEL_MP(ITAB   ,ITABG   ,LENG    ,IGEO     ,IXC    ,
     .                      IXTG   ,IPARTC  ,IPARTTG ,DYNAIN_DATA      ,
     .                      NODTAG ,DYNAIN_INDXC,DYNAIN_INDXTG,IPARG   ,
     .                      ELBUF_TAB,THKE  ,IPART   )                                 
        DYNAIN_DATA%DYNAIN_NUMELC_G =DYNAIN_DATA%DYNAIN_NUMELC 
        DYNAIN_DATA%DYNAIN_NUMELTG_G =DYNAIN_DATA%DYNAIN_NUMELTG 
      ELSE
        !  - shells -
        CALL DYNAIN_SHEL_SPMD(ITAB   ,ITABG   ,LENG    ,IGEO   ,IXC    ,
     .                          IXTG   ,IPARTC  ,IPARTTG ,DYNAIN_DATA      ,
     .                          NODTAG ,DYNAIN_INDXC,DYNAIN_INDXTG,IPARG   ,
     .                          ELBUF_TAB,THKE  ,LENGC   ,LENGTG  ,IPART   )
      END IF


C-----------------------------------------------
C   RIGID BODY'S PRIMARY NODE IS OUTPUTTED IF ONE OF ITS SECONDARY NODES ARE
C-----------------------------------------------
      DO I=1,NRBODY
          DO J=1,NPBY(2,I)
            N=LPBY(NPBY(11,I)+J)
            IF (NODTAG(N)/=0) THEN
              NODTAG(NPBY(1,I)) = 1
              EXIT
            END IF 
          ENDDO
       ENDDO
C-----------------------------------------------
      CALL DYNAIN_NODE(X,NUMNOD,ITAB,ITABG,LENG,NODGLOB,WEIGHT,NODTAG,DYNAIN_DATA)

C-----------------------------------------------
      CALL DYNAIN_SIZE_C(IPARG ,ELBUF_TAB, SIZP0 ,SIZLOC   ,DYNAIN_DATA  )

C-----------------------------------------------
C   ALLOCATION OF TABLES
C-----------------------------------------------
      IERR = 0
      IF(SIZLOC >= 1) THEN
        ALLOCATE(WA(SIZLOC),STAT=IERR)
      ELSE
        ALLOCATE(WA(1))
      ENDIF
      IF(IERR/=0)THEN
         CALL ANCMSG(MSGID=252,ANMODE=ANINFO,
     .        I1=IERR)
              CALL ARRET(2)
       END IF 

      IERR = 0
      SIZP0 = MAX(1,SIZP0)
      ALLOCATE(WAP0(SIZP0),STAT=IERR)
      IF(IERR/=0)THEN
         CALL ANCMSG(MSGID=252,ANMODE=ANINFO,
     .        I1=IERR)
              CALL ARRET(2)
       END IF 
C-----------------------------------------------
C   SHELL SCALAR
C-----------------------------------------------


      IF(DYNAIN_DATA%DYNAIN_C(4)==1) THEN
         CALL DYNAIN_C_STRSG(
     1        ELBUF_TAB  ,IPARG     ,IGEO  ,IXC    ,
     2        IXTG       ,WA        ,WAP0       ,IPARTC,IPARTTG,
     3        DYNAIN_DATA,DYNAIN_INDXC,DYNAIN_INDXTG,SIZP0    ,
     4        GEO        ,STACK     ,DRAPE_SH4N ,DRAPE_SH3N,X      ,
     5        THKE      , DRAPEG    ,NUMMAT     ,MAT_PARAM  )
      ENDIF
C------------------------------------------

      IF(DYNAIN_DATA%DYNAIN_C(5)==1) THEN
          CALL DYNAIN_C_STRAG(
     1         ELBUF_TAB  ,IPARG     ,IPM        ,IGEO  ,IXC    ,
     2         IXTG       ,WA        ,WAP0       ,IPARTC,IPARTTG,
     3         DYNAIN_DATA,DYNAIN_INDXC,DYNAIN_INDXTG,SIZP0    ,
     4         GEO        ,STACK     ,DRAPE_SH4N   ,DRAPE_SH3N,X    ,
     5         THKE       ,DRAPEG)
        ENDIF

C-----------------------------------------------

      IF(SIZLOC >= 1) DEALLOCATE(WA)
      IF(SIZP0 >= 1) DEALLOCATE(WAP0)
C-----------------------
C    DEAllocation Tabs
C-----------------------
      DEALLOCATE(NODTAG,ITABG,DYNAIN_INDXC,DYNAIN_INDXTG)
C-----------------------------------------------
C   END
C-----------------------------------------------
      IF(ISPMD==0) THEN
        IF(DYNAIN_DATA%ZIPDYNAIN==0) THEN
          WRITE(IUDYNAIN,'(A)')'*END   '
          CLOSE(UNIT=IUDYNAIN)
        ELSE
          CALL STRS_TXT50('*END   ',7)
          CALL CLOSE_C()
        ENDIF

        WRITE (IOUT,1000)  FILNAM(1:FILEN)
        WRITE (ISTDO,1000) FILNAM(1:FILEN)
      ENDIF
      
      
 1000 FORMAT (4X,' DYNAIN FILE:',1X,A,' WRITTEN')
C
      RETURN
      END
