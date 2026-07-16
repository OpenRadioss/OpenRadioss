Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    stock_msg              ../engine/source/output/message/stock_msg.F
!||--- called by ------------------------------------------------------
!||    build_msg              ../common_source/tools/memory/my_alloc_tools.F90
!||--- uses       -----------------------------------------------------
!||    message_mod            ../engine/share/message_module/message_mod.F
!||    my_dealloc_mod         ../common_source/tools/memory/my_dealloc.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE STOCK_MSG(ID,ITYPE,SBUFMSG,BUFMSG)
C-----------------------------------------------
C  M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)              :: ID,ITYPE,SBUFMSG
      CHARACTER(LEN=NCHARLINE100), INTENT(IN) :: BUFMSG(SBUFMSG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JDX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (ALLOCATED(MESSAGES(ITYPE,ID)%MESSAGE)) THEN
        DEALLOCATE(MESSAGES(ITYPE,ID)%MESSAGE)
      END IF
      IF (SBUFMSG==0) THEN
        MESSAGES(ITYPE,ID)%SMESSAGE=1
        ALLOCATE(MESSAGES(ITYPE,ID)%MESSAGE(1))
        IF (ITYPE==1) THEN
          MESSAGES(ITYPE,ID)%MESSAGE(1)=' !!! EMPTY TITLE !!! '
        ELSE IF (ITYPE==2) THEN
          MESSAGES(ITYPE,ID)%MESSAGE(1)=' !!! EMPTY DESCRIPTION !!! '
        END IF
      ELSE
        ALLOCATE(MESSAGES(ITYPE,ID)%MESSAGE(SBUFMSG))
        MESSAGES(ITYPE,ID)%SMESSAGE=SBUFMSG
        DO I=1,SBUFMSG
          JDX=1
          MESSAGES(ITYPE,ID)%MESSAGE(I)=' '
          J=1
          DO WHILE(J<=NCHARLINE100)
            IF (BUFMSG(I)(J:J)==achar(92)) then !'\') THEN
C             => skip \ print next
              IF (J<NCHARLINE100) THEN
                J=J+1 
                IF (BUFMSG(I)(J:J)=='n') THEN
                  MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)=CHAR(10)
                  JDX=JDX+1
C add here other special cases
                ELSE
C                 keep back slash for further use
                  MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)= BUFMSG(I)(J-1:J-1)
                  JDX=JDX+1
                  MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)= BUFMSG(I)(J:J)
                  JDX=JDX+1
                END IF
              ELSE
C               => end of loop
                J=J+1
              END IF
            ELSE
              MESSAGES(ITYPE,ID)%MESSAGE(I)(JDX:JDX)=BUFMSG(I)(J:J)
              JDX=JDX+1
            END IF
            J=J+1
          END DO
        END DO
      END IF
      END SUBROUTINE 
