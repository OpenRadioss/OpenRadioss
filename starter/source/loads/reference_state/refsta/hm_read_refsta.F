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
Chd|  HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        REFSTA_MOD                    share/modules1/refsta_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_REFSTA( LSUBMODEL,XRFILE )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE REFSTA_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr15_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      CHARACTER*ncharline, INTENT(INOUT) :: XRFILE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER TMP_NAME*ncharline
      INTEGER LEN_TMP_NAME,J, I, LEN_
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        IS_ENCRYPTED = .FALSE.
        IS_AVAILABLE = .FALSE.

        CALL HM_OPTION_COUNT('/REFSTA',IREFSTA)
        IF(IREFSTA == 0) RETURN
        CALL HM_OPTION_START('/REFSTA')
        CALL HM_OPTION_READ_KEY(LSUBMODEL)
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      
        RS0_FMT=0                                                                                                        
        NITRS=0     
        XRFILE(1:ncharline)=' '                                                                                                     
        CALL HM_GET_STRING('filename',XRFILE,ncharline,IS_AVAILABLE)                                                       
        CALL HM_GET_INTV('nitrs',NITRS,IS_AVAILABLE,LSUBMODEL)                                                           
        CALL HM_GET_INTV('refsta_fileformat',RS0_FMT,IS_AVAILABLE,LSUBMODEL)                                                        
                                                                                                                         
        IF (NITRS == 0) NITRS=100  
        LEN_ = LEN_TRIM(XRFILE)                                                                                       
        IF(LEN_ > 0 )XRFILE(LEN_+1:ncharline) = ' '
        !
        IF (XRFILE==' ') THEN                                                                                          
          XRFILE=ROOTNAM(1:ROOTLEN)//'RS0'                                                                               
        ELSE                                                                                                             
          J=0                                                                                                            
          DO I=1,ncharline                                                                                               
            IF(XRFILE(I:I)/=' ')THEN                                                                                   
              J=J+1                                                                                                      
              XRFILE(J:J) = XRFILE(I:I)                                                                                  
            ENDIF                                                                                                        
          ENDDO                                                                                                          
          DO I=J+1,ncharline                                                                                             
              XRFILE(I:I) = ' '                                                                                          
          ENDDO                                                                                                          
        ENDIF                                                                                                            
        REFSTA_FILENAME = XRFILE                                                                                         
        TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//XRFILE(1:len_trim(XRFILE))                                              
        LEN_TMP_NAME = INFILE_NAME_LEN+len_trim(XRFILE)                                                                  
        OPEN(UNIT=IIN6,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='OLD',ERR=111)          

C-----------------------------------------------
      RETURN

C-----------------------------------------------
 111      CONTINUE
          CALL ANCMSG(MSGID=517,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=XRFILE)
C-----------------------------------------------
      RETURN
      END SUBROUTINE









