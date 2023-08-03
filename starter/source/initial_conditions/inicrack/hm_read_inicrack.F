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
Chd|  HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INICRACK(ITABM1   ,INICRACK ,UNITAB   ,LSUBMODEL)
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD    
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABM1(*)
      TYPE (INICRACK_) , DIMENSION(NINICRACK) :: INICRACK
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,NSEG,LIST_INICRACK(NINICRACK)
      INTEGER J2(2)
      CHARACTER TITR*nchartitle,MESS*40
      my_real
     .        BID,RATIO
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'INITIAL CRACK DEFINITION                '/
C-----------------------------------------------
c     INICRACK            ! Crack storage array
C     INICRACK(IGS)%ID    : INICRACK IDENTIFIER
C     INICRACK(IGS)%NSEG  : INICRACK NUMBER OF SEGMENTS
C     INICRACK(IGS)%TITLE : INICRACK title
C     INICRACK(IGS)%RATIO : INICRACK RATIO OF ONE SEGMENT
C     INICRACK(IGS)%SEG   : SEGMENTS OF INICRACK
C=======================================================================
C     
      ! Variable initialization
      BID = ZERO
C      
      ! Start reading /INICRACK cards
      CALL HM_OPTION_START('/INICRACK')
C      
      ! Loop over INICRACK
      DO I = 1,NINICRACK
C      
        ! Reading title and keys
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = ID,
     .                          OPTION_TITR    = TITR)               
C        
        ! Storing IDs and Title
        INICRACK(I)%ID    = ID
        INICRACK(I)%TITLE = TITR
C
        ! Count number of segment
        CALL HM_GET_INTV('segmax',NSEG,IS_AVAILABLE,LSUBMODEL)
        INICRACK(I)%NSEG  = NSEG
C    
        ! Allocation of segment data structure
        ALLOCATE(INICRACK(I)%SEG(NSEG))
        DO J = 1,NSEG
          CALL MY_ALLOC(INICRACK(I)%SEG(J)%NODES,2)
        ENDDO
C
        ! Filling the segment data structure
        DO J = 1,NSEG      
          CALL HM_GET_INT_ARRAY_INDEX('node_ID1',J2(1),J,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('node_ID2',J2(2),J,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Ratio', RATIO,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          INICRACK(I)%SEG(J)%NODES(1) = USR2SYS(J2(1),ITABM1,MESS,ID)
          INICRACK(I)%SEG(J)%NODES(2) = USR2SYS(J2(2),ITABM1,MESS,ID)
          INICRACK(I)%SEG(J)%RATIO    = RATIO
        ENDDO
      ENDDO
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      DO J = 1,NINICRACK
        LIST_INICRACK(J) = INICRACK(J)%ID
      ENDDO
      CALL UDOUBLE_IGR(LIST_INICRACK,NINICRACK,MESS,0,BID)
C=======================================================================
c-----------
c-----------
      RETURN
      END
