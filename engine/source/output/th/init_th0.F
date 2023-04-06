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
Chd|  INIT_TH0                      source/output/th/init_th0.F   
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|        TIME_HISTORY_MOD              ../common_source/modules/output/time_history_mod.F
Chd|====================================================================
        SUBROUTINE INIT_TH0( IPARG,ELBUF_TAB,IGEO,IXR,TH,
     .                       WEIGHT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD        
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE TH_MOD
      USE TIME_HISTORY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MBUFFER, NPARTL
      INTEGER IPARG(NPARG,*),IGEO(NPROPGI,*),IXR(NIXR,*)
      INTEGER, DIMENSION(NUMNOD), INTENT(in) :: WEIGHT
C     REAL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (TH_), INTENT(IN),TARGET :: TH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: BOOL
      INTEGER :: ID,LOCAL_ID      
      INTEGER, POINTER :: ITHGR,ITHB
      INTEGER, DIMENSION(10) :: ALREADY_DONE
      INTEGER :: SITHBUF,SITHGRP

C=======================================================================

C-------------------------------------------------------
C    TH GROUP
C-------------------------------------------------------
        ALREADY_DONE(1:10) = 0

        DO ID=1,9
            IF(NTHGRP01(ID)/=0) THEN
                IF(ID==1) THEN
                    ITHGR => TH%ITHGRPA(1)
                    ITHB=> TH%ITHBUFA(1)
                    SITHBUF = TH%SITHBUFA
                ENDIF
                IF(ID==2) THEN
                    ITHGR => TH%ITHGRPB(1)
                    ITHB=> TH%ITHBUFB(1)
                    SITHBUF = TH%SITHBUFB
                ENDIF
                IF(ID==3) THEN
                    ITHGR => TH%ITHGRPC(1)
                    ITHB  =>TH%ITHBUFC(1)
                    SITHBUF = TH%SITHBUFC
                ENDIF
                IF(ID==4) THEN
                    ITHGR => TH%ITHGRPD(1)
                    ITHB  => TH%ITHBUFD(1)
                    SITHBUF = TH%SITHBUFD
                ENDIF
                IF(ID==5) THEN
                    ITHGR => TH%ITHGRPE(1)
                    ITHB  => TH%ITHBUFE(1)
                    SITHBUF = TH%SITHBUFE
                ENDIF
                IF(ID==6) THEN
                    ITHGR => TH%ITHGRPF(1)
                    ITHB  => TH%ITHBUFF(1)
                    SITHBUF = TH%SITHBUFF
                ENDIF
                IF(ID==7) THEN
                    ITHGR => TH%ITHGRPG(1)
                    ITHB  => TH%ITHBUFG(1)
                    SITHBUF = TH%SITHBUFG
                ENDIF
                IF(ID==8) THEN
                    ITHGR => TH%ITHGRPH(1)
                    ITHB  => TH%ITHBUFH(1)
                    SITHBUF = TH%SITHBUFH
                ENDIF
                IF(ID==9) THEN
                    ITHGR => TH%ITHGRPI(1)
                    ITHB  => TH%ITHBUFI(1)
                    SITHBUF = TH%SITHBUFI
                ENDIF
                ALREADY_DONE(ID) = 1
                CALL INIT_TH(IPARG,ITHB,ELBUF_TAB,IGEO,IXR,
     .                       ITHGR,NTHGRP1(ID),ID,WEIGHT,SITHBUF)
            ENDIF
        ENDDO

        ID = 10
        ALREADY_DONE(ID) = 1
        CALL INIT_TH(IPARG,TH%ITHBUF,ELBUF_TAB,IGEO,IXR,
     .               TH%ITHGRP,NTHGRP,ID,WEIGHT,TH%SITHBUF)

        DO ID=1,9
            BOOL=.FALSE.
            IF(ABFILE(ID)/=0) THEN
                IF(ID==2.OR.ID==4.OR.ID==5.OR.ID==6.OR.ID==7.OR.ID==8.OR.ID==9) THEN
                    LOCAL_ID = 1
                    IF(ALREADY_DONE(LOCAL_ID)==0) THEN
                        ITHGR => TH%ITHGRPA(1)
                        ITHB=> TH%ITHBUFA(1)  
                        BOOL=.TRUE.
                    ENDIF
                
                ELSEIF(ID==3) THEN
                    LOCAL_ID = 2
                    IF(ALREADY_DONE(LOCAL_ID)==0) THEN
                        ITHGR => TH%ITHGRPB(1)
                        ITHB=> TH%ITHBUFB(1)  
                        BOOL=.TRUE.
                    ENDIF
                ENDIF
            ENDIF
            IF(BOOL.EQV..TRUE.) THEN
                ALREADY_DONE(LOCAL_ID)=1
                CALL INIT_TH(IPARG,ITHB,ELBUF_TAB,IGEO,IXR,
     .                       ITHGR,NTHGRP1(ID),ID,WEIGHT,TH%SITHBUF)
            ENDIF
        ENDDO
                        

        RETURN
        END SUBROUTINE INIT_TH0
