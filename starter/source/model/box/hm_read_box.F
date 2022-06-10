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
Chd|  HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        READ_BOX_BOX                  source/model/box/read_box_box.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        NBOXLST                       source/model/box/nboxlist.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_BOX(IBOX    ,UNITAB  ,ITABM1   ,ISKN    ,SKEW    ,
     .                       X       ,RTRANS  ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,DIMENSION(LISKN,*)   ,INTENT(IN) :: ISKN
      INTEGER ,DIMENSION(NUMNOD)    ,INTENT(IN) :: ITABM1
      my_real ,DIMENSION(3,NUMNOD)  ,INTENT(IN) :: X
      my_real ,DIMENSION(LSKEW,*)   ,INTENT(IN) :: SKEW
      my_real ,DIMENSION(NTRANSF,*) ,INTENT(IN) :: RTRANS
      TYPE (UNIT_TYPE_)             ,INTENT(IN) :: UNITAB 
      TYPE(SUBMODEL_DATA),DIMENSION(*) ,INTENT(IN) :: LSUBMODEL
      TYPE (BOX_)  ,DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,UID,LEN,BOXID,IUNIT,FLAGUNIT,
     .        IAD,NBOX,NBOX_RECT,NBOX_CYL,NBOX_SPHER,NBOX_BOX,NLIST
      my_real :: BID
      INTEGER :: IWORK(70000)
      INTEGER INDEX(NBBOX*3),IX1(NBBOX),IX2(NBBOX)
      INTEGER, DIMENSION(:)   ,ALLOCATABLE :: BUFTMP,IBOXTMP
      CHARACTER(ncharkey)    :: KEY,KEY2
      CHARACTER(nchartitle)  :: TITR,MESS
      LOGICAL :: IS_AVAILABLE
c
      DATA MESS/'BOX DEFINITION                          '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER LISTCNT,NBOXLST
C-----------------------------------------------
C     IBOX(I)%ID       : BOX IDENTIFIER
C     IBOX(I)%TITLE    : BOX title
C     IBOX(I)%NBOXBOX  : NUMBER OF SUB BOXES (BOXES OF BOXES)
C     IBOX(I)%ISKBOX   : BOX SKEW_ID (/BOX/RECTA)
C     IBOX(I)%NOD1     : FIRST NODE for box limit definition  - N1
C     IBOX(I)%NOD2     : SECOND NODE for box limit definition - N2
C     IBOX(I)%TYPE     : BOX TYPE (0='BOX',1='RECTA',2='CYLIN' ,3='SPHER')
C     IBOX(I)%NBLEVELS : TEMPORARY LEVEL NB OF BOXES
C     IBOX(I)%LEVEL    : FLAG "SUBLEVEL DONE" FOR BOX OF BOXES
C     IBOX(I)%ACTIBOX  : FLAG FOR ACTIVATED BOX FOR (GRNOD,GRSHEL,LINE,SURF...)
C     IBOX(I)%NENTITY  : NUMBER OF BOX ENTITIES (NODES,ELEMS,LINES,SURF)
C                        WITHIN ACTIVATED BOX
C     IBOX(I)%SURFIAD  :temporary address for solid external surface (in box)
C     IBOX(I)%BOXIAD   : temporary address
C     IBOX(I)%DIAM     : BOX diameter (CYLIN + SPHER)
C     IBOX(I)%X1       : coord.X for N1
C     IBOX(I)%Y1       : coord.Y for N1
C     IBOX(I)%Z1       : coord.Z for N1
C     IBOX(I)%X2       : coord.X for N2
C     IBOX(I)%Y2       : coord.Y for N2
C     IBOX(I)%Z2       : coord.Z for N2
C     IBOX(I)%IBOXBOX(NBOXBOX)  : LIST OF BOXES (in /box/box)
C     IBOX(I)%ENTITY(NENTITY)   : LIST OF ENTITIES (NODES,ELEMS,LINES,SURF)
C=======================================================================
c
      CALL HM_OPTION_COUNT('/BOX/RECTA' ,NBOX_RECT  )
      CALL HM_OPTION_COUNT('/BOX/CYLIN' ,NBOX_CYL   )
      CALL HM_OPTION_COUNT('/BOX/SPHER' ,NBOX_SPHER )
      CALL HM_OPTION_COUNT('/BOX/BOX'   ,NBOX_BOX   )
c      
      NBOX = NBOX_RECT + NBOX_CYL + NBOX_SPHER + NBOX_BOX     
c-----------------------------------------------
      IAD = 0
      LEN = 5*NBBOX
      MY_ALLOCATE(BUFTMP  ,LEN)
c--------------------------------------------------
c
      CALL READ_BOX_SPHER(
     .     IBOX     ,IAD      ,NBOX_SPHER,ITABM1   ,X        ,
     .     RTRANS   ,UNITAB   ,LSUBMODEL )
c
      CALL READ_BOX_CYL(
     .     IBOX     ,IAD      ,NBOX_CYL  ,ITABM1   ,X        ,
     .     RTRANS   ,UNITAB   ,LSUBMODEL )
c
      CALL READ_BOX_RECT(
     .     IBOX     ,IAD      ,NBOX_RECT ,ISKN     ,SKEW     ,
     .     ITABM1   ,X        ,RTRANS    ,UNITAB   ,LSUBMODEL)
c
      CALL READ_BOX_BOX(IBOX     ,IAD      ,NBOX_BOX  ,LSUBMODEL)
c
c--------------------------------------------------
c     Recherche des ID doubles
c
      MY_ALLOCATE (IBOXTMP  ,NBOX )
      IBOXTMP(1:NBOX) = IBOX(1:NBOX)%ID
      CALL UDOUBLE_IGR(IBOXTMP,NBOX,MESS,0,ZERO)
c
c--------------------------------------------------
c     check /box/box
c--------------------------------------------------
      IF (NBOX_BOX > 0) THEN
        II = 0
        DO I = 1,NBBOX
          IF (IBOX(I)%TYPE == 0) THEN                  
            NLIST = IBOX(I)%NBOXBOX
            BOXID = IBOX(I)%ID
            TITR  = IBOX(I)%TITLE
            IF (NLIST > 0) THEN
               NLIST = NBOXLST(IBOX(I)%IBOXBOX,NLIST ,IBOXTMP  ,NBBOX,
     .                         BUFTMP ,BUFTMP(1+NBBOX),BUFTMP(1+2*NBBOX),
     .                         II,BOXID,TITR)
              II = 1
              IBOX(I)%NBOXBOX = NLIST
            ELSE
              IBOX(IAD)%NBOXBOX = 0
            ENDIF
          ENDIF
        ENDDO
      ENDIF      
C-----------------------------
      IF (ALLOCATED(IBOXTMP)) DEALLOCATE (IBOXTMP)
      IF (ALLOCATED(BUFTMP) ) DEALLOCATE (BUFTMP )
c-----------
      RETURN
      END
