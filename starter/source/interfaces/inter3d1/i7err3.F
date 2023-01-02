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
Chd|  I7ERR3                        source/interfaces/inter3d1/i7err3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I7ERR3(
     1         X       ,NRTM    ,IRECT   ,NOINT   ,ITAB,ID,TITR,
     2         IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3         X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4         Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5         Z4       ,N1     ,N2     ,N3     ,X0     ,
     6         Y0       ,Z0     ,XN1    ,YN1    ,ZN1    ,
     7         XN2      ,YN2    ,ZN2    ,XN3    ,YN3    ,
     8         ZN3      ,XN4    ,YN4    ,ZN4      )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, IRECT(4,*), NOINT, ITAB(*)
      my_real
     .        X(3,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(OUT) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XN1,YN1,ZN1
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XN2,YN2,ZN2
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XN3,YN3,ZN3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XN4,YN4,ZN4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IDEB
C     REAL
      my_real :: AN
      my_real, DIMENSION(MVSIZ) :: XX1,YY1,ZZ1
      my_real, DIMENSION(MVSIZ) :: XX2,YY2,ZZ2
      my_real, DIMENSION(MVSIZ) :: XX3,YY3,ZZ3
      my_real, DIMENSION(MVSIZ) :: XX4,YY4,ZZ4
C-----------------------------------------------
!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4))
C
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (X0,BV(1,31)) ,(Y0,BV(1,32)) ,(Z0,BV(1,33))
!      EQUIVALENCE (XX1,BV(1,34)),(YY1,BV(1,35)),(ZZ1,BV(1,36))
!      EQUIVALENCE (XX2,BV(1,37)),(YY2,BV(1,38)),(ZZ2,BV(1,39))
!      EQUIVALENCE (XX3,BV(1,40)),(YY3,BV(1,41)),(ZZ3,BV(1,42))
!      EQUIVALENCE (XX4,BV(1,43)),(YY4,BV(1,44)),(ZZ4,BV(1,45))
!      EQUIVALENCE (XN1,BV(1,58)),(YN1,BV(1,59)),(ZN1,BV(1,60))
!      EQUIVALENCE (XN2,BV(1,61)),(YN2,BV(1,62)),(ZN2,BV(1,63))
!      EQUIVALENCE (XN3,BV(1,64)),(YN3,BV(1,65)),(ZN3,BV(1,66))
!      EQUIVALENCE (XN4,BV(1,67)),(YN4,BV(1,68)),(ZN4,BV(1,69))
C
!      EQUIVALENCE (A12,X0),(A34,Z0)
C
      IDEB=0
      DO WHILE(IDEB<NRTM)
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        J=IDEB+I
        IX1(I)=IRECT(1,J)
        IX2(I)=IRECT(2,J)
        IX3(I)=IRECT(3,J)
        IX4(I)=IRECT(4,J)
        X1(I) =X(1,IX1(I))
        Y1(I) =X(2,IX1(I))
        Z1(I) =X(3,IX1(I))
        X2(I) =X(1,IX2(I))
        Y2(I) =X(2,IX2(I))
        Z2(I) =X(3,IX2(I))
        X3(I) =X(1,IX3(I))
        Y3(I) =X(2,IX3(I))
        Z3(I) =X(3,IX3(I))
        X4(I) =X(1,IX4(I))
        Y4(I) =X(2,IX4(I))
        Z4(I) =X(3,IX4(I))
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
        Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
        Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
C
        XX1(I) = X1(I)-X0(I)
        XX2(I) = X2(I)-X0(I)
        XX3(I) = X3(I)-X0(I)
        XX4(I) = X4(I)-X0(I)
        YY1(I) = Y1(I)-Y0(I)
        YY2(I) = Y2(I)-Y0(I)
        YY3(I) = Y3(I)-Y0(I)
        YY4(I) = Y4(I)-Y0(I)
        ZZ1(I) = Z1(I)-Z0(I)
        ZZ2(I) = Z2(I)-Z0(I)
        ZZ3(I) = Z3(I)-Z0(I)
        ZZ4(I) = Z4(I)-Z0(I)
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        XN1(I) = YY1(I)*ZZ2(I) - YY2(I)*ZZ1(I)
        YN1(I) = ZZ1(I)*XX2(I) - ZZ2(I)*XX1(I)
        ZN1(I) = XX1(I)*YY2(I) - XX2(I)*YY1(I)
        N1(I)=XN1(I)
        N2(I)=YN1(I)
        N3(I)=ZN1(I)
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        XN2(I) = YY2(I)*ZZ3(I) - YY3(I)*ZZ2(I)
        YN2(I) = ZZ2(I)*XX3(I) - ZZ3(I)*XX2(I)
        ZN2(I) = XX2(I)*YY3(I) - XX3(I)*YY2(I)
        N1(I)=N1(I)+XN2(I)
        N2(I)=N2(I)+YN2(I)
        N3(I)=N3(I)+ZN2(I)
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        IF(IX3(I)/=IX4(I)) THEN
         XN3(I) = YY3(I)*ZZ4(I) - YY4(I)*ZZ3(I)
         YN3(I) = ZZ3(I)*XX4(I) - ZZ4(I)*XX3(I)
         ZN3(I) = XX3(I)*YY4(I) - XX4(I)*YY3(I)
         N1(I)=N1(I)+XN3(I)
         N2(I)=N2(I)+YN3(I)
         N3(I)=N3(I)+ZN3(I)
        ELSE
         XN3(I)=ZERO
         YN3(I)=ZERO
         ZN3(I)=ZERO
        ENDIF
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        XN4(I) = YY4(I)*ZZ1(I) - YY1(I)*ZZ4(I)
        YN4(I) = ZZ4(I)*XX1(I) - ZZ1(I)*XX4(I)
        ZN4(I) = XX4(I)*YY1(I) - XX1(I)*YY4(I)
        N1(I)=N1(I)+XN4(I)
        N2(I)=N2(I)+YN4(I)
        N3(I)=N3(I)+ZN4(I)
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        AN= MAX(EM20,SQRT(N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)))
        N1(I)=N1(I)/AN
        N2(I)=N2(I)/AN
        N3(I)=N3(I)/AN
       ENDDO
C
       DO I=1,MIN(MVSIZ,NRTM-IDEB)
        X0(I)=(N1(I)*XN1(I)+N2(I)*YN1(I)+N3(I)*ZN1(I))
        Z0(I)=(N1(I)*XN3(I)+N2(I)*YN3(I)+N3(I)*ZN3(I))
C
        IF(Z0(I)==ZERO.AND.X0(I)==ZERO)THEN
          CALL ANCMSG(MSGID=558,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ITAB(IX1(I)),
     .                I3=ITAB(IX2(I)),
     .                I4=ITAB(IX3(I)),
     .                I5=ITAB(IX4(I)))
        ENDIF
       ENDDO
C
       IDEB=IDEB+MIN(MVSIZ,NRTM-IDEB)
      ENDDO
C
      RETURN
      END
