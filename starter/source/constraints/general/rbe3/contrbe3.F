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
Chd|  CONTRBE3                      source/constraints/general/rbe3/contrbe3.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CONTRBE3(ICR,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT)          :: ICR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL, NSLT, ITYP, NUSER,  NM,M, NI,
     .        ISK, ISENS, INGU, IGM, J, P,IAD,NS,NN,J6(6),JJ,II,
     .        IC,IC1,IC2,IROT,ISKS,IADS,IERR1,NC,ID,IRAD
      CHARACTER TITR*nchartitle,KEY*ncharkey
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
C=====================================================================|
      ICR =0
      CALL HM_OPTION_START('/RBE3')
      DO I=1,NRBE3
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NUSER,
     .                       OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('dependentnode',NM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LTX',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LTY',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LTZ',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LRX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LRY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LRZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('nset',NN,IS_AVAILABLE,LSUBMODEL)
        IF ((NM+NN)==0) CYCLE
C
        IF ((J6(1)+J6(2)+J6(3)+J6(4)+J6(5)+J6(6))==0) THEN
         J6(4)=1
         J6(5)=1
         J6(6)=1
        ENDIF
       ICR = J6(4) + J6(5) + J6(6)
       IF (ICR >0) RETURN
      ENDDO
C
      RETURN
C
      END SUBROUTINE CONTRBE3
