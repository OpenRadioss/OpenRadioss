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
!||    pcoori        ../starter/source/elements/beam/pcoori.F
!||--- called by ------------------------------------------------------
!||    inivoid       ../starter/source/elements/initia/inivoid.F
!||    pinit3        ../starter/source/elements/beam/pinit3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE PCOORI(X,NCP, 
     .                  MXT,MXG,NC1,NC2,NC3,DELTAX,
     .                  X1,X2,X3, Y1,Y2,Y3, Z1,Z2,Z3,
     .                  IBEAM_VECTOR,RBEAM_VECTOR,IVECT,VECT)
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCP(NIXP,*),NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ),
     .        MXT(MVSIZ), MXG(MVSIZ)
      INTEGER , INTENT (IN)  :: IBEAM_VECTOR(MVSIZ)
      INTEGER , INTENT (OUT) :: IVECT(MVSIZ)
      my_real X1(MVSIZ), X2(MVSIZ), X3(MVSIZ),
     .        Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),X(3,*),DELTAX(MVSIZ)
      my_real , INTENT (IN)  :: RBEAM_VECTOR(3,MVSIZ)
      my_real , INTENT (OUT) :: VECT(3,MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real XP1, XP2, XP3, XNOR1, XP4, XP5, XP6,
     .        XNOR2, XNORM, DET1, DET2, DET3, DET, XX,YY,ZZ,TOL
C=======================================================================
C     CONNECTIVITES ET MATERIEL                   |
C--------------------------------------------------
      TOL=TWO*EM06
      DO I=LFT,LLT
        MXT(I)=NCP(1,I)
        NC1(I)=NCP(2,I)
        NC2(I)=NCP(3,I)
        NC3(I)=NCP(4,I)
        MXG(I)=NCP(5,I)
        IVECT(I)=IBEAM_VECTOR(I)
        VECT(1:3,I)=RBEAM_VECTOR(1:3,I)        
      END DO
C
C----------------------------
C     COORDINATES         |
C----------------------------
      DO I=LFT,LLT
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
      ENDDO
c
      DO I=LFT,LLT
        XX = (X1(I)-X2(I))*(X1(I)-X2(I)) 
        YY = (Y1(I)-Y2(I))*(Y1(I)-Y2(I)) 
        ZZ = (Z1(I)-Z2(I))*(Z1(I)-Z2(I)) 
        DELTAX(I) = SQRT(XX+YY+ZZ)
      ENDDO
C------------------------------
C     CONSISTENCY
C------------------------------     
      DO I=LFT,LLT
        XP1=X2(I)-X1(I)
        XP2=Y2(I)-Y1(I)
        XP3=Z2(I)-Z1(I)
        XNOR1=SQRT(XP1*XP1+XP2*XP2+XP3*XP3)
        IF(XNOR1<=EM20) THEN
          CALL ANCMSG(MSGID=222,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NCP(6,I))
        ENDIF
C
        IF (IVECT(I)>0) THEN
C         DIRECTION DEFINED BY VECTOR : CHECK THAT VECTOR NOT COLINEAR WITH N1N2       
          XP4=VECT(1,I)
          XP5=VECT(2,I)
          XP6=VECT(3,I)
          DET1=XP1*XP5-XP2*XP4
          DET2=XP2*XP6-XP3*XP5
          DET3=XP3*XP4-XP1*XP6
          DET= SQRT(DET1**2+DET2**2+DET3**2)
          IF (DET<TOL) THEN
C           IVECT swithed to -1 - Y or Z global axis will be used instead of the vector            
            IVECT(I) = -1
            CALL ANCMSG(MSGID=3090,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=NCP(6,I),
     .                  PRMOD=MSG_CUMU)
          ENDIF              
        ELSE  
C         DIRECTION DEFINED WITH N3 : CHECK THAT N1N2 and N1N3 ARE NOT COLINEAR
          IF (NC3(I)==NC2(I)) CYCLE
          XP4=X3(I)-X1(I)
          XP5=Y3(I)-Y1(I)
          XP6=Z3(I)-Z1(I)
          XNOR2=SQRT(XP4*XP4+XP5*XP5+XP6*XP6)
          IF(XNOR2<EM20) THEN
            CALL ANCMSG(MSGID=223,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=NCP(6,I))
          ELSE
            DET1=XP1*XP5-XP2*XP4
            DET2=XP2*XP6-XP3*XP5
            DET3=XP3*XP4-XP1*XP6
            DET= SQRT(DET1**2+DET2**2+DET3**2)
            IF (DET<TOL) THEN
              CALL ANCMSG(MSGID=3051,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=NCP(6,I),
     .                   PRMOD=MSG_CUMU)
              NC3(I)=NC2(I)
            ENDIF
          ENDIF
        ENDIF   
      ENDDO
      CALL ANCMSG(MSGID=3051,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3090,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  PRMOD=MSG_PRINT)
c-----------
      RETURN
      END
