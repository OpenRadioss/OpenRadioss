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
!||    mattab_usr2sys         ../starter/source/materials/tools/mattab_usr2sys.F
!||--- called by ------------------------------------------------------
!||    eos_table_copy         ../starter/source/materials/tools/eos_table_copy.F90
!||    func_table_copy        ../starter/source/materials/tools/func_table_copy.F90
!||    mat_table_copy         ../starter/source/materials/tools/mat_table_copy.F90
!||    mat_table_table_copy   ../starter/source/materials/tools/mat_table_table_copy.F90
!||    updfail                ../starter/source/materials/updfail.F90
!||    updmat                 ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE MATTAB_USR2SYS(TITR,MAT_ID, NTABLE ,TABLE ,NTABL ,ITABLE  )
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: MAT_ID
      INTEGER ,INTENT(IN)  :: NTABLE  ! total number of function tables
      INTEGER ,INTENT(IN)  :: NTABL   ! number of function tables in material/failure law
      INTEGER      ,DIMENSION(NTABL)   :: ITABLE    ! Table IDs in material law
      TYPE(TTABLE) ,DIMENSION(NTABLE)  :: TABLE
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,TABL_ID,OK
C=======================================================================
      DO I=1,NTABL          ! number of tables in material                 
        TABL_ID = ITABLE(I)
        IF (TABL_ID > 0) THEN
          OK = 0
          DO J=1,NTABLE     ! total number of tables                
           IF (TABL_ID == TABLE(J)%NOTABLE) THEN           
              ITABLE(I) = J
              OK = 1
              EXIT                            
            ENDIF                              
          ENDDO                                
          IF (OK == 0) THEN
            CALL ANCMSG(MSGID=779, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                  I1=MAT_ID  ,
     .                  C1=TITR,
     .                  I2=TABL_ID )
            ITABLE(I) = 0
          ENDIF                                 
        ENDIF                                 
c
      ENDDO  ! I=1,NTABL 
c-----------
      RETURN
      END
