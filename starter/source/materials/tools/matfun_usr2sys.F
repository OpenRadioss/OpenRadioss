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
Chd|  MATFUN_USR2SYS                source/materials/tools/matfun_usr2sys.F
Chd|-- called by -----------
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE MATFUN_USR2SYS(TITR,MAT_ID, NFUNC  ,IFUNC  ,FUNC_ID  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle :: TITR
      INTEGER  , DIMENSION(NFUNC)  :: IFUNC
      INTEGER  , DIMENSION(NFUNCT) :: FUNC_ID
      INTEGER  :: MAT_ID, NFUNC
!      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
      INTENT(IN)    :: TITR,MAT_ID,FUNC_ID
      !INTENT(INOUT) :: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,OK
C=======================================================================
        !NFUNC =  IPM(10,IMAT)+IPM(6,IMAT)in updmat.f
        !IFUNC => IPM(10+1:10+NFUNC,IMAT) in updmat.f
c-----------------------------------------------------
      DO I=1,NFUNC                           
        ID = IFUNC(I)
        OK = 0
        IF (ID > 0) THEN
          DO J=1,NFUNCT     ! total number of functions                   
           IF (ID == FUNC_ID(J)) THEN           
              IFUNC(I) = J
              OK = 1
              EXIT                            
            ENDIF                              
          ENDDO                                
          IF (OK == 0) THEN
            CALL ANCMSG(MSGID = 126,             
     .                  MSGTYPE=MSGERROR,       
     .                  ANMODE=ANINFO_BLIND_1,  
     .                  I1 = MAT_ID,              
     .                  C1 = TITR  ,              
     .                  I2 = ID    )              
          ENDIF                                 
        ENDIF                                 
c-----------
      ENDDO  ! I=1,NFUNC 
c-----------
      RETURN
      END
