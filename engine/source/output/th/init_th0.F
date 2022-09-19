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
Chd|====================================================================
        SUBROUTINE INIT_TH0( IPARG,ELBUF_TAB,IGEO,IXR,
     .                       ITHGRP,ITHGRPA,ITHGRPB,ITHGRPC,ITHGRPD,
     .                       ITHGRPE,ITHGRPF,ITHGRPG,ITHGRPH,ITHGRPI,
     .                       ITHBUF,ITHBUFA,ITHBUFB,ITHBUFC,ITHBUFD,
     .                       ITHBUFE,ITHBUFF,ITHBUFG,ITHBUFH,ITHBUFI,
     .                       WEIGHT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE CLUSTER_MOD
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE TH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "scrfs_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "rad2r_c.inc"
#include      "thermal_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MBUFFER, NPARTL
      INTEGER IPARG(NPARG,*),IGEO(NPROPGI,*),IXR(NIXR,*)
      INTEGER, TARGET, INTENT(in) :: ITHGRP(NITHGR,*) ,ITHGRPA(NITHGR,*),ITHGRPB(NITHGR,*),
     .                       ITHGRPC(NITHGR,*),ITHGRPD(NITHGR,*),ITHGRPE(NITHGR,*),
     .                       ITHGRPF(NITHGR,*),ITHGRPG(NITHGR,*),ITHGRPH(NITHGR,*),
     .                       ITHGRPI(NITHGR,*)
      INTEGER, INTENT(in) :: ITHBUF(SITHBUF)
      INTEGER, TARGET, INTENT(in) :: ITHBUFA(*),ITHBUFB(*),ITHBUFC(*),ITHBUFD(*),
     .                       ITHBUFE(*),ITHBUFF(*),ITHBUFG(*),ITHBUFH(*),ITHBUFI(*)
      INTEGER, DIMENSION(NUMNOD), INTENT(in) :: WEIGHT
C     REAL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: BOOL
      INTEGER :: ID,LOCAL_ID
      INTEGER, POINTER :: ITHGR,ITHB
      INTEGER, DIMENSION(10) :: ALREADY_DONE

C=======================================================================

C-------------------------------------------------------
C    TH GROUP
C-------------------------------------------------------









        ALREADY_DONE(1:10) = 0

        DO ID=1,9
            IF(NTHGRP01(ID)/=0) THEN
                IF(ID==1) THEN
                    ITHGR => ITHGRPA(1,1)
                    ITHB=> ITHBUFA(1)
                ENDIF
                IF(ID==2) THEN
                    ITHGR => ITHGRPB(1,1)
                    ITHB=> ITHBUFB(1)
                ENDIF
                IF(ID==3) THEN
                    ITHGR => ITHGRPC(1,1)
                    ITHB=> ITHBUFC(1)
                ENDIF
                IF(ID==4) THEN
                    ITHGR => ITHGRPD(1,1)
                    ITHB=> ITHBUFD(1)
                ENDIF
                IF(ID==5) THEN
                    ITHGR => ITHGRPE(1,1)
                    ITHB=> ITHBUFE(1)
                ENDIF
                IF(ID==6) THEN
                    ITHGR => ITHGRPF(1,1)
                    ITHB=> ITHBUFF(1)
                ENDIF
                IF(ID==7) THEN
                    ITHGR => ITHGRPG(1,1)
                    ITHB=> ITHBUFG(1)
                ENDIF
                IF(ID==8) THEN
                    ITHGR => ITHGRPH(1,1)
                    ITHB=> ITHBUFH(1)
                ENDIF
                IF(ID==9) THEN
                    ITHGR => ITHGRPI(1,1)
                    ITHB=> ITHBUFI(1)
                ENDIF
                ALREADY_DONE(ID) = 1
                CALL INIT_TH(IPARG,ITHB,ELBUF_TAB,IGEO,IXR,
     .                       ITHGR,NTHGRP1(ID),ID,WEIGHT)
            ENDIF
        ENDDO

        ID = 10
        ALREADY_DONE(ID) = 1
        CALL INIT_TH(IPARG,ITHBUF,ELBUF_TAB,IGEO,IXR,
     .               ITHGRP,NTHGRP,ID,WEIGHT)

        DO ID=1,9
            BOOL=.FALSE.
            IF(ABFILE(ID)/=0) THEN
                IF(ID==2.OR.ID==4.OR.ID==5.OR.ID==6.OR.ID==7.OR.ID==8.OR.ID==9) THEN
                    LOCAL_ID = 1
                    IF(ALREADY_DONE(LOCAL_ID)==0) THEN
                        ITHGR => ITHGRPA(1,1)
                        ITHB=> ITHBUFA(1)
                        BOOL=.TRUE.
                    ENDIF

                ELSEIF(ID==3) THEN
                    LOCAL_ID = 2
                    IF(ALREADY_DONE(LOCAL_ID)==0) THEN
                        ITHGR => ITHGRPB(1,1)
                        ITHB=> ITHBUFB(1)
                        BOOL=.TRUE.
                    ENDIF
                ENDIF
            ENDIF
            IF(BOOL.EQV..TRUE.) THEN
                ALREADY_DONE(LOCAL_ID)=1
                CALL INIT_TH(IPARG,ITHB,ELBUF_TAB,IGEO,IXR,
     .                       ITHGR,NTHGRP1(ID),ID,WEIGHT)
            ENDIF
        ENDDO


        RETURN
        END SUBROUTINE INIT_TH0
