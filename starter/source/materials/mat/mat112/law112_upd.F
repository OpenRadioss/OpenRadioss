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
      !||====================================================================
      !||    law112_upd             ../starter/source/materials/mat/mat112/law112_upd.F
      !||--- called by ------------------------------------------------------
      !||    updmat                 ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    table_mod              ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE LAW112_UPD(TITR   ,UPARAM ,NPC    ,PLD    ,  
     .                      NUMTABL,ITABLE ,TABLE  ,MAT_ID )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE)  :: TITR
      INTEGER MAT_ID,NUMTABL,ITABLE(NUMTABL)
      INTEGER NPC(*)
      my_real UPARAM(*),PLD(*)
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ITAB
      my_real SLOPEMIN,DX,DY
C=======================================================================
      ! Recovering tabulated flag
      ITAB = NINT(UPARAM(14))
c
      ! Checking yield stress functions slope                                                         
      IF (ITAB > 0) THEN    
        DO I = 1,NUMTABL                                          
          SLOPEMIN = INFINITY
          DO K = 2,SIZE(TABLE(ITABLE(I))%X(1)%VALUES)
            DX = TABLE(ITABLE(I))%X(1)%VALUES(K) - TABLE(ITABLE(I))%X(1)%VALUES(K-1)
            DY = TABLE(ITABLE(I))%Y%VALUES(K) - TABLE(ITABLE(I))%Y%VALUES(K-1)
            SLOPEMIN = MIN(SLOPEMIN,DY/DX)
          ENDDO    
          IF (SLOPEMIN < ZERO) THEN
            CALL ANCMSG(MSGID=2059 ,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               I2=TABLE(ITABLE(I))%NOTABLE,
     .               C1=TITR)
          ENDIF
        ENDDO  
      ENDIF
c-----------
      RETURN
      END
