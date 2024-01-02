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
Chd|  RCHECKMASS                    source/elements/spring/rcheckmass.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RCHECKMASS(
     .                  IXR      ,GEO      ,PM       ,MSR      ,INR    ,
     .                  MS       ,IN       ,ITAB     ,IGEO     ,IPM    ,
     .                  UPARAM   ,IPART    ,IPARTR   ,NPBY     ,LPBY   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), ITAB(*),
     .        IGEO(NPROPGI,*),IPM(NPROPMI,*),IPART(LIPART1,*),IPARTR(*),
     .        NPBY(NNPBY,*),LPBY(*)
C     REAL
      my_real
     .   GEO(NPROPG,*),PM(NPROPM,*),UPARAM(*),MSR(*),INR(*),MS(*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NR,N1,N2,IPID,IGTYP,IMAT,MTN,IADBUF,IEQUI,IP,IPREV,
     .        K1,K11,K12,K13,K14,IERR2,N,M,NSL,IAD,NS,NERR
      INTEGER WORK(70000)
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX,ITRI,TAGSLV
C     REAL
      my_real
     .   XKM, XCM, XKR, XCR
      CHARACTER  TITL*nchartitle
C-----------------------------------------------------
C     Check for springs with stiffness but no mass
C-----------------------------------------------------
      CALL MY_ALLOC(INDEX,2*NUMELR)
      CALL MY_ALLOC(ITRI ,NUMELR)
C
      CALL MY_ALLOC(TAGSLV,NUMNOD)
      TAGSLV(1:NUMNOD)=0
      DO N=1,NRBYKIN
        M  =NPBY(1,N)
        IF(NPBY(7,N)/=0.AND.MS(M)/=ZERO.AND.IN(M)/=ZERO)THEN
                             ! RBODY is active <=> not a rigid body activated with sensor
                             ! A node may be secnd of several rbodies (cf /RBODY/ON, /RBODY/OFF)
                             ! Then an error could be written when starting the engine
          NSL=NPBY(2,N)
          IAD=NPBY(11,N)
          DO I=1,NSL
            NS=LPBY(IAD+I)
            TAGSLV(NS)=1
          END DO
        END IF
      END DO
C
      DO I=1,NUMELR
        ITRI(I)=IPARTR(I)
      END DO
C
      CALL MY_ORDERS( 0, WORK, ITRI, INDEX, NUMELR , 1)
C
      IPREV=0
      NERR =0
      DO I=1,NUMELR
        NR=INDEX(I)
        IPID  = IXR(1,NR)
        IGTYP = IGEO(11,IPID)
        IMAT  = IXR(5,NR)
        IP    = IPARTR(NR)
        IERR2 = 0
        IF(IGTYP==23)THEN
C
          IADBUF = IPM(7,IMAT) - 1
          MTN    = IPM(2,IMAT)
C
          K1 = 4
          K11 = 64
          K12 = K11 + 6
          K13 = K12 + 6
          K14 = K13 + 6
C
          IF(MTN == 108) THEN
            IEQUI = UPARAM(IADBUF+2)
            N1  =IXR(2,NR)
            N2  =IXR(3,NR)
            IF((TAGSLV(N1)==0.AND.(MS(N1)==ZERO.OR.IN(N1)==ZERO)).OR.
     .         (TAGSLV(N2)==0.AND.(MS(N2)==ZERO.OR.IN(N2)==ZERO)))THEN
 
              IF(IP/=IPREV.AND.NERR/=0)THEN
                IPREV=IP
C
                CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,IP),LTITR)
                CALL ANCMSG(MSGID=1870,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IPART(4,IP),
     .                      C1=TITL)
C
C               Depile messages...
                CALL ANCMSG(MSGID=1871,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      PRMOD=MSG_PRINT)
C
                NERR = 0
C
              END IF
              XKM= MAX(UPARAM(IADBUF + K11 + 1)*UPARAM(IADBUF + K1 + 1),
     .                 UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3))   ! /XL(I)
              XCM= MAX(UPARAM(IADBUF + K12 + 1),UPARAM(IADBUF + K12 + 2),UPARAM(IADBUF + K12 + 3))
              XKR= MAX(UPARAM(IADBUF + K11 + 4)*UPARAM(IADBUF + K1 + 4),
     .                 UPARAM(IADBUF + K11 + 5)*UPARAM(IADBUF + K1 + 5),
     .                 UPARAM(IADBUF + K11 + 6)*UPARAM(IADBUF + K1 + 6))   ! /XL(I)
              XCR= MAX(UPARAM(IADBUF + K12 + 4),UPARAM(IADBUF + K12 + 5),UPARAM(IADBUF + K12 + 6))
              IF((TAGSLV(N1)==0.AND.MS(N1)==ZERO).OR.(TAGSLV(N2)==0.AND.MS(N2)==ZERO))THEN
                IF(XKM/=ZERO.OR.XCM/=ZERO)IERR2=IERR2+1
              END IF
              IF((TAGSLV(N1)==0.AND.IN(N1)==ZERO).OR.(TAGSLV(N2)==0.AND.IN(N2)==ZERO))THEN
                IF(XKR/=ZERO.OR.XCR/=ZERO.OR.(IEQUI/=0.AND.(XKM/=ZERO.OR.XCM/=ZERO)))IERR2=IERR2+1
              END IF
            END IF
          END IF
        END IF 
        IF(IERR2/=0)THEN
          NERR=NERR+1
          CALL ANCMSG(MSGID=1871,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IXR(NIXR,NR),
     .                I2=ITAB(N1),
     .                I3=ITAB(N2),
     .                PRMOD=MSG_CUMU)
        END IF
      END DO
C
      CALL ANCMSG(MSGID=1871,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
C
      DEALLOCATE(INDEX,ITRI,TAGSLV)
C------------------------------------------
      RETURN
      END      
