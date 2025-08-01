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
!||    admerr         ../engine/source/model/remesh/admerr.F
!||--- called by ------------------------------------------------------
!||    resol          ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    admthke        ../engine/source/model/remesh/admthke.F
!||    ancmsg         ../engine/source/output/message/message.F
!||    arret          ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    message_mod    ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE ADMERR(
     .           IXC      ,IXTG    ,X       ,IPARG       ,ELBUF_TAB   ,
     .           IPART    ,IPARTC  ,IPARTTG ,ERR_THK_SH4 ,ERR_THK_SH3 ,
     .           IAD_ELEM ,FR_ELEM ,WEIGHT  ,SH4TREE     ,SH3TREE ,
     .           AREA_SH4 ,AREA_SH3,  AREA_NOD,
     .           THICK_SH4, THICK_SH3, THICK_NOD)    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER  
     .   IXC(NIXC,*), IXTG(NIXTG,*),IPARG(NPARG,*),
     .   IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*), 
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
      my_real
     .   X(3,*), ERR_THK_SH4(*), ERR_THK_SH3(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      my_real AREA_SH4(NUMELC)
      my_real AREA_SH3(NUMELTG)
      my_real AREA_NOD(NUMNOD)
      my_real THICK_SH4(NUMELC)
      my_real THICK_SH3(NUMELTG)
      my_real THICK_NOD(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR
C     REAL
C-----------------------------------------------
C
      IF(IERROR/=0) THEN
        CALL ANCMSG(MSGID=153,ANMODE=ANINFO_BLIND,
     .            I1=IERROR)
        CALL ARRET(2)
      END IF
C
      AREA_SH4=ZERO
      AREA_SH3=ZERO
C
      AREA_NOD =ZERO
      THICK_NOD=ZERO
C
      ERR_THK_SH4(1:NUMELC) =ZERO
      ERR_THK_SH3(1:NUMELTG)=ZERO
C     elts belonging to adapted parts
      CALL ADMTHKE(
     .       IXC     ,IXTG    ,X       ,IPARG   ,ELBUF_TAB,
     .       IPART   ,IPARTC  ,IPARTTG ,IAD_ELEM,FR_ELEM ,
     .       WEIGHT  ,AREA_SH4,AREA_SH3,AREA_NOD,THICK_SH4 ,
     .       THICK_SH3 ,THICK_NOD , ERR_THK_SH4, ERR_THK_SH3,
     .       SH4TREE ,SH3TREE )

c-----------
      RETURN
      END
