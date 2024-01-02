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
C
Chd|====================================================================
Chd|  INVOI3                        source/interfaces/inter3d1/invoi3.F
Chd|-- called by -----------
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        NEAREST_SEG                   ../common_source/interf/nearest_seg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INVOI3(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,NSEG,NSN,NMN,ITAB,ID,TITR,NRT)
      USE MESSAGE_MOD
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: NSN, NMN, NRT, ID
      INTEGER, INTENT(IN)  :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), NSEG(*), ITAB(*)
      INTEGER, INTENT(INOUT) :: ILOC(*), IRTL(*)
      my_real, INTENT(IN)  :: X(3,*)
      CHARACTER*nchartitle, TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, M, N, II, JJ, KK, LL
      INTEGER LG, MG, NG, J1, J2, K1, K2, KKK, JNEW
      INTEGER KM1(4), KN1(4), LSEG, LSEG_NEW
      my_real CMS, DMS, EMS, FMS, BMIN, BMAX
C
      DATA KM1/2,3,4,1/
      DATA KN1/4,1,2,3/
C-------------------------------------------------------------------------------------
      IF(NRT==0) RETURN
C
      DO II=1,NSN
        I=NSV(II)
        J=ILOC(II)
        JNEW=J
        K=MSR(J)
        CMS=(X(1,I)-X(1,K))**2+(X(2,I)-X(2,K))**2+(X(3,I)-X(3,K))**2
        J1=NSEG(J)
        J2=NSEG(J+1)-1
        DO JJ=J1,J2
          LL=LMSR(JJ)
          IF(J==IRECT(1,LL)) THEN
            L=IRECT(2,LL)
            M=IRECT(3,LL)
            N=IRECT(4,LL)
          ELSEIF(J==IRECT(2,LL)) THEN
            L=IRECT(1,LL)
            M=IRECT(3,LL)
            N=IRECT(4,LL)
          ELSEIF(J==IRECT(3,LL)) THEN
            L=IRECT(1,LL)
            M=IRECT(2,LL)
            N=IRECT(4,LL)
          ELSEIF(J==IRECT(4,LL)) THEN
            L=IRECT(1,LL)
            M=IRECT(2,LL)
            N=IRECT(3,LL)
          ELSE
            CALL ANCMSG(MSGID=105,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ITAB(MSR(IRECT(1,LL))),
     .                  I3=ITAB(MSR(IRECT(2,LL))),
     .                  I4=ITAB(MSR(IRECT(3,LL))),
     .                  I5=ITAB(MSR(IRECT(4,LL))))
            L=IRECT(1,LL)
            M=IRECT(2,LL)
            N=IRECT(3,LL)
          ENDIF
C
          LG=MSR(L)
          MG=MSR(M)
          NG=MSR(N)
          DMS=(X(1,I)-X(1,LG))**2+(X(2,I)-X(2,LG))**2+(X(3,I)-X(3,LG))**2
          EMS=(X(1,I)-X(1,MG))**2+(X(2,I)-X(2,MG))**2+(X(3,I)-X(3,MG))**2
          FMS=(X(1,I)-X(1,NG))**2+(X(2,I)-X(2,NG))**2+(X(3,I)-X(3,NG))**2
          IF(DMS<=CMS) THEN
            CMS=DMS
            JNEW=L
            K=LG
          ENDIF
          IF(EMS<=CMS) THEN
            CMS=EMS
            JNEW=M
            K=MG
          ENDIF
          IF(FMS<=CMS) THEN
            CMS=FMS
            JNEW=N
            K=NG
          ENDIF
        ENDDO  !JJ=J1,J2
        J=JNEW
        ILOC(II)=J
C
C
        BMAX=-EP30
        LSEG_NEW=0
        L=IRTL(II)
        IF(L==0) GO TO 100
        LSEG=L
        DO KKK=1,4
          KK=KKK
          IF(IRECT(KK,L)==J) EXIT
        ENDDO
        J1=KM1(KK)
        J2=KN1(KK)
        IF(KK==3.AND.IRECT(3,L)==IRECT(4,L)) J1=1
        M=MSR(IRECT(J1,L))
        N=MSR(IRECT(J2,L))
        CALL NEAREST_SEG(X, I, K, M, N, LSEG, LSEG_NEW, BMIN, BMAX)
        IF(BMIN >= ZERO) GO TO 200
C
 100    CONTINUE   ! L=0
        J1=NSEG(J)
        J2=NSEG(J+1)-1
        DO JJ=J1,J2
          LL=LMSR(JJ)
          LSEG=LL
          IF(L==LL) CYCLE
          DO KKK=1,4
            KK=KKK
            IF(IRECT(KK,LL)==J) EXIT
          ENDDO
          K1=KM1(KK)
          K2=KN1(KK)
          IF(KK==3.AND.IRECT(3,LL)==IRECT(4,LL)) K1=1
          M=MSR(IRECT(K1,LL))
          N=MSR(IRECT(K2,LL))
          CALL NEAREST_SEG(X, I, K, M, N, LSEG, LSEG_NEW, BMIN, BMAX)
          IF(BMIN < ZERO) CYCLE
          IRTL(II)=LSEG_NEW
          GO TO 200
        ENDDO
        IRTL(II)=LSEG_NEW
 200    CONTINUE
      ENDDO   !II=1,NSN

      RETURN
      END
