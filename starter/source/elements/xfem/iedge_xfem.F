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
Chd|  IEDGE_XFEM                    source/elements/xfem/iedge_xfem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IEDGE_XFEM(
     .           IBORDNODE ,IXC       ,IXTG    ,IEDGESH4,IEDGESH3,
     .           IBORDEDGE ,NODEDGE   ,IELCRKC ,IELCRKTG,IEDGE   ,
     .           CEP_CRK   ,IEDGE_TMP0)
C=======================================================================
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBORDNODE(*),IXC(NIXC,*),IXTG(NIXTG,*),IEDGESH4(4,*),
     . IEDGESH3(3,*),IBORDEDGE(*),NODEDGE(2,*),IELCRKC(*),IELCRKTG(*),
     . IEDGE(*),CEP_CRK(*),IEDGE_TMP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,JJ,LL,I1,I2,I1M,I2M,NL,IED,NLMAX,STAT,
     . NELALL,NEL,NIX,JCRK0,JCRK,P,PROC
      INTEGER NEXTK4(4),NEXTK3(3),IWORK(70000)
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::
     .   LINEIX,LINEIX2,IXWORK,IEDWORK4,IEDWORK3
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .   INDEX,TAGED,ITAGED,NIXEL,TAGEL,TAGEL_CRK,IEDGE_TMP
C
      DATA NEXTK4/2,3,4,1/
      DATA NEXTK3/2,3,1/
C=======================================================================
      NLMAX  = 4*ECRKXFEC + 3*ECRKXFETG   ! max edges
      NELALL = ECRKXFEC+ECRKXFETG         ! max eleemnts
C
      ALLOCATE (LINEIX(2,NLMAX)      ,STAT=stat)
      ALLOCATE (LINEIX2(2,NLMAX)     ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)       ,STAT=stat)
      ALLOCATE (IXWORK(5,NLMAX)      ,STAT=stat)
      ALLOCATE (IEDWORK4(4,ECRKXFEC) ,STAT=stat)
      ALLOCATE (IEDWORK3(3,ECRKXFETG),STAT=stat)
      ALLOCATE (TAGED(NLMAX)         ,STAT=stat)
      ALLOCATE (ITAGED(NLMAX)        ,STAT=stat)
      ALLOCATE (NIXEL(NELALL)        ,STAT=stat)
      ALLOCATE (TAGEL(NELALL)        ,STAT=stat)
      ALLOCATE (TAGEL_CRK(NELALL)    ,STAT=stat)
      LINEIX  = 0
      LINEIX2 = 0
      INDEX   = 0
      IXWORK  = 0
      IEDWORK4= 0
      IEDWORK3= 0
      TAGED   = 0
      ITAGED  = 0
      NIXEL   = 0
      TAGEL   = 0
      TAGEL_CRK   = 0
C
      IF (STAT /= 0) THEN
        CALL ANCMSG(MSGID=268 ,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1='EDGE XFEM')
      END IF
c---------------------------------------
c       recherche de toutes les lignes dans la surface (shells)
c---------------------------------------
      LL  = 0  ! nb edges
      NEL = 0  ! nb elements
      DO J=1,NUMELC
        IF (IELCRKC(J) > 0) THEN
          NEL = NEL + 1
          NIXEL(NEL) = 4
          TAGEL(NEL) = J
          TAGEL_CRK(NEL) = IELCRKC(J)
        ENDIF
      END DO
C
      DO J=1,NUMELTG
        IF (IELCRKTG(J) > 0) THEN
          NEL = NEL + 1
          NIXEL(NEL) = 3
          TAGEL(NEL) = J
          TAGEL_CRK(NEL) = IELCRKTG(J)-ECRKXFEC
        ENDIF
      END DO
C
      DO I=1,NEL
        J = TAGEL(I)
        NIX = NIXEL(I)
        IF (NIX == 4) THEN
c          I1=IXC(2,J)
c          I2=IXC(3,J)
c          I3=IXC(4,J)
c          I4=IXC(5,J)
          DO K=1,NIX
            I1 = IXC(K+1,J)
            I2 = IXC(NEXTK4(K)+1,J)
            LL = LL+1
            IF(I2 > I1)THEN
              LINEIX(1,LL) = I1
              LINEIX(2,LL) = I2
C              LINEIX2(1,LL) = J
              LINEIX2(1,LL) = I
              LINEIX2(2,LL) = K
            ELSE
              LINEIX(1,LL) = I2
              LINEIX(2,LL) = I1
C              LINEIX2(1,LL) = J
              LINEIX2(1,LL) = I
              LINEIX2(2,LL) = -K
            ENDIF
          ENDDO
        ELSE IF (NIX == 3) THEN
c          I1=IXTG(2,J)
c          I2=IXTG(3,J)
c          I3=IXTG(4,J)
          DO K=1,NIX
            I1 = IXTG(K+1,J)
            I2 = IXTG(NEXTK3(K)+1,J)
            LL = LL+1
            IF(I2 > I1)THEN
              LINEIX(1,LL) = I1
              LINEIX(2,LL) = I2
C              LINEIX2(1,LL) = J
              LINEIX2(1,LL) = I
              LINEIX2(2,LL) = K
            ELSE
              LINEIX(1,LL) = I2
              LINEIX(2,LL) = I1
C              LINEIX2(1,LL) = J
              LINEIX2(1,LL) = I
              LINEIX2(2,LL) = -K
            ENDIF
          ENDDO
        END IF
      END DO
C---
      CALL MY_ORDERS(0,IWORK,LINEIX,INDEX,LL,2)
c---------------------------------------
c     remove double edges (internal edges in fact)
c---------------------------------------
      NL = 1
      I1M = LINEIX(1,INDEX(1))
      I2M = LINEIX(2,INDEX(1))
      IXWORK(1,NL)=I1M
      IXWORK(2,NL)=I2M
      IXWORK(3,NL)=LINEIX2(1,INDEX(1))
      IXWORK(4,NL)=LINEIX2(2,INDEX(1))
      IXWORK(5,NL)=1
C
      J = IXWORK(3,NL)
      K = ABS(IXWORK(4,NL))
      NIX = NIXEL(J)
      I   = TAGEL(J)
      JJ  = TAGEL_CRK(J)
      IF (NIX == 4) THEN
        IEDWORK4(K,JJ) = NL
      ELSE IF (NIX == 3) THEN
        IEDWORK3(K,JJ) = NL
      END IF
C----
      DO L=2,LL
        I1 = LINEIX(1,INDEX(L))
        I2 = LINEIX(2,INDEX(L))
        IF(I2 /= I2M .or. I1 /= I1M)THEN
          NL = NL + 1
          IXWORK(1,NL)=I1
          IXWORK(2,NL)=I2
          IXWORK(3,NL)=LINEIX2(1,INDEX(L))
          IXWORK(4,NL)=LINEIX2(2,INDEX(L))
          IXWORK(5,NL)=1 ! bord
C
          J = IXWORK(3,NL)
          K = ABS(IXWORK(4,NL))
          NIX = NIXEL(J)
          I = TAGEL(J)
          JJ = TAGEL_CRK(J)
          IF(NIX == 4)THEN
            IEDWORK4(K,JJ) = NL
          ELSE IF(NIX == 3)THEN
            IEDWORK3(K,JJ) = NL
          END IF
        ELSE
          IXWORK(5,NL)=0 ! edge double
C
          J = LINEIX2(1,INDEX(L))
          K = ABS(LINEIX2(2,INDEX(L)))
          NIX = NIXEL(J)
          I = TAGEL(J)
          JJ = TAGEL_CRK(J)
          IF(NIX == 4)THEN
            IEDWORK4(K,JJ) = NL
          ELSE IF(NIX == 3)THEN
            IEDWORK3(K,JJ) = NL
          END IF
        ENDIF
        I1M = I1
        I2M = I2
      ENDDO
C
      NUMEDGES = NL
c---------------------------------------
c     build global shell element edges table (all edges)
c---------------------------------------
      NL = 0
      DO J=1,NEL
        NIX = NIXEL(J)
        I  = TAGEL(J)
        JJ = TAGEL_CRK(J)
        IF (NIX == 4) THEN
          DO K=1,NIX
            IED = IEDWORK4(K,JJ)
            IF (TAGED(IED) == 0) THEN
              NL = NL + 1
              ITAGED(IED) = NL
              TAGED( IED) = 1
              IEDGE(NL)   = NL
              IBORDEDGE(NL) = IXWORK(5,IED)
              IF(IXWORK(5,IED) == 1)THEN
                IBORDNODE(IXWORK(1,IED)) = 1
                IBORDNODE(IXWORK(2,IED)) = 1
              END IF
c             edge nodes
              NODEDGE(1,NL) = IXWORK(1,IED)
              NODEDGE(2,NL) = IXWORK(2,IED)
            END IF
            IEDGESH4(K,JJ) = ITAGED(IED)
          END DO
        ELSE IF (NIX == 3) THEN
          DO K=1,NIX
            IED = IEDWORK3(K,JJ)
            IF (TAGED(IED) == 0) THEN
              NL = NL + 1
              ITAGED(IED) = NL
              TAGED(IED) = 1
              IBORDEDGE(NL) = IXWORK(5,IED)
              IEDGE(NL) = NL
              IF(IXWORK(5,IED) == 1)THEN
                IBORDNODE(IXWORK(1,IED)) = 1
                IBORDNODE(IXWORK(2,IED)) = 1
              END IF
c             edge nodes
              NODEDGE(1,NL) = IXWORK(1,IED)
              NODEDGE(2,NL) = IXWORK(2,IED)
            END IF
            IEDGESH3(K,JJ) = ITAGED(IED)
          END DO
        END IF
      END DO
c---------------------------------------
c       keep boundary nodes (remove internal edges)
c---------------------------------------
c      LL = NL
c      NL = 0
c      DO L=1,LL
c        IF(IXWORK(5,L) == 1)THEN
c          NL = NL + 1
c          I1=IXWORK(1,NL)
c          I2=IXWORK(2,NL)
c          I3=IXWORK(3,NL)
c          I4=IXWORK(4,NL)
c          I5=IXWORK(5,NL)
c          IXWORK(1,NL)=IXWORK(1,L)
c          IXWORK(2,NL)=IXWORK(2,L)
c          IXWORK(3,NL)=IXWORK(3,L)
c          IXWORK(4,NL)=IXWORK(4,L)
c          IXWORK(5,NL)=1 ! bord on
c          IXWORK(1,L)=I1
c          IXWORK(2,L)=I2
c          IXWORK(3,L)=I3
c          IXWORK(4,L)=I4
c          IXWORK(5,L)=I5
C
c          I1=IXWORK(1,L)
c          I2=IXWORK(2,L)
c          I3=IXWORK(3,L)
c          I4=IXWORK(4,L)
c          I5=IXWORK(5,L)
c          IXWORK(1,NL)=I1
c          IXWORK(2,NL)=I2
c          IXWORK(3,NL)=I3
c          IXWORK(4,NL)=I4
c          IXWORK(5,NL)=I5 ! bord on
c        ENDIF
c      ENDDO
c---------------------------------------
c     fill border nodal table
c---------------------------------------
c      DO LL=1,NL
c        IBORDNODE(IXWORK(1,LL)) = 1				
c        IBORDNODE(IXWORK(2,LL)) = 1				
c      ENDDO
C
      ALLOCATE (IEDGE_TMP(NUMEDGES))
      IEDGE_TMP = 0
C
      DO P = 1,NSPMD
        ITAGED = 0
        DO I=1,NEL
c        J = TAGEL(I)
          NIX = NIXEL(I)
          JCRK0 = TAGEL_CRK(I)
          JCRK  = JCRK0
          IF(NIX == 3) JCRK = JCRK + ECRKXFEC
          PROC = CEP_CRK(JCRK) + 1
          IF(P == PROC)THEN
            IF(NIX==4)THEN
              DO K=1,NIX
                IED = IEDGESH4(K,JCRK0)
cc                IF(IED /= 0)THEN
cc                  IF(ITAGED(IED) == 0)THEN
cc                    ITAGED(IED) = 1
cc                    IEDGE_TMP0(IED) = IEDGE_TMP0(IED) + 1
cc                  ENDIF
cc                ENDIF
                IF(IED /= 0 .AND. IBORDEDGE(IED) == 0)THEN
                  IF(IEDGE_TMP(IED) >= 0)THEN
                    IEDGE_TMP(IED) = IEDGE_TMP(IED) + 1
                  ENDIF
                ENDIF
              ENDDO
            ELSEIF(NIX==3)THEN
              DO K=1,NIX
                IED = IEDGESH3(K,JCRK0)
cc                IF(IED /= 0)THEN
cc                  IF(ITAGED(IED) == 0)THEN
cc                    ITAGED(IED) = 1
cc                    IEDGE_TMP0(IED) = IEDGE_TMP0(IED) + 1
cc                  ENDIF
cc                ENDIF
                IF(IED /= 0 .AND. IBORDEDGE(IED) == 0)THEN
                  IF(IEDGE_TMP(IED) >= 0)THEN
                    IEDGE_TMP(IED) = IEDGE_TMP(IED) + 1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          END IF ! IF(P == PROC)THEN
        END DO
C---
        DO IED=1,NUMEDGES
          IF(IEDGE_TMP(IED) == 1) IEDGE_TMP(IED) = -1
        ENDDO
C---
      END DO
C---
      DO IED=1,NUMEDGES
        IF(IEDGE_TMP(IED) == -1) IEDGE_TMP0(IED) = IEDGE_TMP(IED)
      ENDDO
C---
C-----------
      DEALLOCATE (INDEX)
      DEALLOCATE (IXWORK)
      DEALLOCATE (LINEIX)
      DEALLOCATE (LINEIX2)
      DEALLOCATE (IEDWORK4)
      DEALLOCATE (IEDWORK3)
      DEALLOCATE (TAGED)
      DEALLOCATE (ITAGED)
      DEALLOCATE (NIXEL)
      DEALLOCATE (TAGEL)
      DEALLOCATE (TAGEL_CRK)
      DEALLOCATE (IEDGE_TMP)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  DISTFUNCC1                    source/elements/xfem/iedge_xfem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DISTFUNCC1(PLXN,PLYN,PLZN,FI,
     .      X,Y,Z,XINT1X,XINT1Y,XINT1Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      MY_REAL
     . X,Y,Z,XINT1X,XINT1Y,XINT1Z,
     . PLXN,PLYN,PLZN,FI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      MY_REAL
     . LEN
C-----------------------------------------------
      FI=PLXN*(X-XINT1X)+
     .   PLYN*(Y-XINT1Y)+
     .   PLZN*(Z-XINT1Z)
      LEN=SQRT(PLXN**2+PLYN**2+PLZN**2)
      IF(LEN/=ZERO)FI=FI/LEN
C
      RETURN
      END
C
Chd|====================================================================
Chd|  DISTFUNCC2                    source/elements/xfem/iedge_xfem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DISTFUNCC2(XN,YN,ZN,
     .           XT1,YT1,ZT1,XT2,YT2,ZT2,DIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      MY_REAL
     .   DIS,XN,YN,ZN,XT1,YT1,ZT1,XT2,YT2,ZT2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      MY_REAL
     .   AREA,XN1,YN1,ZN1,X12,Y12,Z12,XX,YY,ZZ
C-----------------------------------------------
      XN1=XN-XT1
      YN1=YN-YT1
      ZN1=ZN-ZT1
C
      X12=XT2-XT1
      Y12=YT2-YT1
      Z12=ZT2-ZT1
C
      XX=YN1*Z12-ZN1*Y12
      YY=ZN1*X12-XN1*Z12
      ZZ=XN1*Y12-YN1*X12
C
      AREA=-HALF*SQRT(XX**2+YY**2+ZZ**2)
      DIS=SQRT(X12**2+Y12**2+Z12**2)
      IF(DIS/=ZERO)DIS=AREA/DIS
C
      RETURN
      END
