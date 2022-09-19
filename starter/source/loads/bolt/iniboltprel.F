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
Chd|  INIBOLTPREL                   source/loads/bolt/iniboltprel.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        UEL2SYS                       source/initial_conditions/inista/yctrl.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIBOLTPREL(IXS, IPRELOAD ,PRELOAD  ,VPRELOAD, IFLAG_BPRELOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
!      USE R2R_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
!#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sysunit.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPRELOAD(3,*), IFLAG_BPRELOAD(*)
      my_real
     .   PRELOAD(6,*), VPRELOAD(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IE, STAT, IPL, NE, J
      INTEGER WORKS(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE ::ITRIS
      INTEGER, DIMENSION(:), ALLOCATABLE ::INDEXS
      INTEGER, DIMENSION(:), ALLOCATABLE ::KSYSUSRS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER UEL2SYS
      EXTERNAL UEL2SYS
C=======================================================================
      ALLOCATE (ITRIS(NUMELS) ,STAT=stat)
      IF (STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                      C1='ITRIS')
        RETURN
      END IF
      ALLOCATE (INDEXS(2*NUMELS) ,STAT=stat)
      IF (STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                      C1='INDEXS')
        RETURN
      END IF
      ALLOCATE (KSYSUSRS(2*NUMELS),STAT=stat)
      IF (STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                      C1='KSYSUSRS')
        RETURN
      END IF
      ITRIS = 0
      INDEXS = 0
      KSYSUSRS = 0
c
      VPRELOAD(1:6,1:NUMELS) = ZERO
c
      DO IE = 1, NUMELS
        ITRIS(IE) = IXS(NIXS,IE)
      END DO
      CALL MY_ORDERS(0,WORKS,ITRIS,INDEXS,NUMELS,1)
      DO J = 1, NUMELS
        IE=INDEXS(J)
        KSYSUSRS(J)       =IXS(NIXS,IE)
        KSYSUSRS(NUMELS+J)=IE
      END DO
c
      DO IPL = 1,NUMPRELOAD
        NE = IPRELOAD(3,IPL)
C       No systeme dans le D00, de l'elt:
        IE=UEL2SYS(NE,KSYSUSRS,NUMELS)
        IF(IE/=0)THEN
          VPRELOAD(1,IE) = PRELOAD(1,IPL)
          VPRELOAD(2,IE) = PRELOAD(2,IPL)
          VPRELOAD(3,IE) = PRELOAD(3,IPL)
          VPRELOAD(4,IE) = PRELOAD(4,IPL)
          VPRELOAD(5,IE) = PRELOAD(5,IPL)
          VPRELOAD(6,IE) = PRELOAD(6,IPL)
          VPRELOAD(7,IE) = IPRELOAD(2,IPL)
        ENDIF
      ENDDO
!::: Creer l'equivalent de ptsol pour le BPREL
      DO IE=1,NUMELS
        NE = IXS(NIXS,IE)
        J=UEL2SYS(NE,KSYSUSRS,NUMELS)
        IFLAG_BPRELOAD(IE) =J
      END DO
!:::
C-----------
      DEALLOCATE(KSYSUSRS,INDEXS,ITRIS)
      RETURN
      END

