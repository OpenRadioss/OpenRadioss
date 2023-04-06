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
Chd|  PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GRSIZEN                       source/system/nintrr.F        
Chd|        GRSIZE_ELE                    source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE PRELECSEC4BOLT(SNSTRF ,SSECBUF ,IGRNOD,ITABM1,FLAG_R2R,
     .                          NOM_OPT,IGRBRIC,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(INOUT) :: SNSTRF,SSECBUF
      INTEGER ,INTENT(IN)    :: ITABM1(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)   :: IGRNOD
      TYPE (GROUP_)  ,DIMENSION(NGRBRIC)  :: IGRBRIC
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,IBID,ISAV,IGU,IGUS,IGUQ,IGUC,IGUT,IGUP,IGUR,IGUTG,
     .   NNOD,NBINTER,NSEGQ,NSEGS,NSEGC,NSEGT,NSEGP,NSEGR,NSEGTG,NFRAM,
     .   ID,UID,FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,L,ISTYP,
     .   FLAG_R2R,N1,N2,N3,POS_SEC_R2R,NSEG0,NSEG,
     .   COMPT,NG
      CHARACTER TITR*nchartitle
      CHARACTER KEY2*ncharkey,MESS*40   
      LOGICAL IS_AVAILABLE  
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  GRSIZEN,USR2SYS,GRSIZE_R2R,GRSIZE_ELE
      EXTERNAL GRSIZEN,USR2SYS,GRSIZE_R2R,GRSIZE_ELE
C=======================================================================
      FLAG_FMT = 0
      NFRAM = 0
      SNSTRF  = 30
      SSECBUF = 20
      L = 7
      COMPT = 0
      NG = 0
      IGU = 0
      
      CALL HM_OPTION_START('/SECT')
                 
      DO I=1,NSECT
        
        NG=NG+1
        
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR, UNIT_ID=UID, KEYWORD2=KEY2)
        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
          
        CALL HM_GET_INTV('Axis_Origin_Node_N1', N1, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N2', N2, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N3', N3, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('ISAVE', ISAV, IS_AVAILABLE, LSUBMODEL)  
 
        IF(KEY2(1:5) == 'PARAL' .OR. KEY2(1:6) == 'CIRCLE') THEN
          ISTYP = 1
        ELSE
          ISTYP = 0
          CALL HM_GET_INTV('Grnod_ID', IGU, IS_AVAILABLE, LSUBMODEL)  
          CALL HM_GET_INTV('System_Id', NFRAM, IS_AVAILABLE, LSUBMODEL)  
        ENDIF
                                            
        CALL HM_GET_INTV('grbrick_id', IGUS, IS_AVAILABLE, LSUBMODEL)  

        IF (NFRAM == 0 .AND. ISTYP == 0) THEN
	         NNOD  = GRSIZEN(IGU,IGRNOD,NGRNOD)
       	ELSE
          NNOD = 20 * GRSIZE_ELE(IGUS,IGRBRIC,NGRBRIC)
        ENDIF
        NSEGS = GRSIZE_ELE(IGUS,IGRBRIC,NGRBRIC)
        SNSTRF = SNSTRF +30 + NNOD       !+ NBINTER              
     .         + 2*(NSEGS)   !+NSEGQ+NSEGC+NSEGT+NSEGP+NSEGR+NSEGTG)
        SSECBUF=SSECBUF+10

      ENDDO
            
C-----------
      RETURN
C-----------
      END
C-----------
