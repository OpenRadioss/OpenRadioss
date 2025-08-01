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
!||    spadah        ../engine/source/elements/sph/spadah.F
!||--- called by ------------------------------------------------------
!||    forintp       ../engine/source/elements/forintp.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SPADAH(
     1    X       ,V       ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP    ,NOD2SP  ,WA      ,WASPACT ,
     3    ITASK   ,IPARTSP ,IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        WASPACT(*),ITASK,IPARTSP(*),IPART(LIPART1,*)      
C     REAL
      my_real
     .   X(3,*) ,V(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,WA(KWASPH,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,INOD,NVOIS,NS,IPRT,IPROP,H_FLAG
      my_real
     .       XI,YI,ZI,DI,DIVV,COEFF,GET_U_GEO,
     .       HMIN,HMAX,H_NEW,DI0
C-----------------------------------------------
      EXTERNAL GET_U_GEO      
C-----------------------------------------------
      DO NS=ITASK+1,NSPHACT,NTHREAD
        N=WASPACT(NS)
        INOD =KXSP(3,N)
        NVOIS=KXSP(4,N)
        XI=X(1,INOD)
        YI=X(2,INOD)
        ZI=X(3,INOD)
        DI   =SPBUF(1,N)
        DI0 = SPBUF(14,N)
        DIVV =WA(13,N)
        IPRT =IPARTSP(N)
        IPROP=IPART(2,IPRT)
        COEFF = GET_U_GEO(8,IPROP)
        H_FLAG=NINT(GET_U_GEO(9,IPROP))
        HMIN = GET_U_GEO(10,IPROP)
        HMAX = GET_U_GEO(11,IPROP)
C--     H_FLAG = 0 -> 3D dilatation of H : GET_U_GEO(8,IPROP) = 1/3 - HMIN=0 HMAX=EP20
C--     H_FLAG = 1 -> 1D dilatation of H : GET_U_GEO(8,IPROP) = 1   - HMIN=0 HMAX=EP20
C--     H_FLAG = 2 -> Constant H : GET_U_GEO(8,IPROP) = 0           - HMIN=0 HMAX=EP20      
C--     H_FLAG = 3 -> 3D dilatation of H with bounds hmin*h0 < H < hmax*h0     
        H_NEW=DI*(ONE+DIVV*DT1*COEFF)
        IF (H_FLAG==3) THEN
          H_NEW=MAX(HMIN*DI0,H_NEW)
          H_NEW=MIN(HMAX*DI0,H_NEW)
        ENDIF
        SPBUF(1,N)= H_NEW
        IF(SPBUF(1,N)<EM20)THEN
          CALL ANCMSG(MSGID=174,ANMODE=ANINFO,
     .            I1=KXSP(NISP,N))
          CALL ARRET(2)
        ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END
