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
Chd|  RBY_IMPD                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        RBY_IMP2                      source/constraints/general/rbody/rby_impd.F
Chd|        RBY_IMP5                      source/constraints/general/rbody/rby_impd.F
Chd|        RBY_IMP7                      source/constraints/general/rbody/rby_impd.F
Chd|        RBY_IMRD                      source/constraints/general/rbody/rby_impd.F
Chd|====================================================================
      SUBROUTINE RBY_IMPD(NRBYAC,IRBYAC,X  ,RBY,LPBY,
     1                   NPBY,SKEW,ISKEW,ITAB,WEIGHT,
     2                   MS ,IN   ,NDOF ,D   ,
     3                   DR ,IXR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*)
      INTEGER NDOF(*),NRBYAC,IRBYAC(*),IXR(*)
C     REAL
      my_real
     .   X(3,*), RBY(NRBY,*), SKEW(LSKEW,*),
     .   IN(*),MS(*),D(3,*), DR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,K
C-----------------------------------------------
      IF( IMP_LR > 0 ) THEN
        DO I=1,NRBYAC
          N=IRBYAC(I)
          K=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMP7(X   ,RBY(1,N),LPBY(K),
     1                NPBY(1,N),SKEW,ISKEW,ITAB,WEIGHT,
     2                MS       ,IN  ,NDOF ,D   ,DR   )
        END DO
      ELSE
       IF (ITRMAX==0) THEN
        DO I=1,NRBYAC
         N=IRBYAC(I)
         K=IRBYAC(I+NRBYKIN)+1
         CALL RBY_IMP2(X   ,RBY(1,N),LPBY(K),
     1               NPBY(1,N),SKEW,ISKEW,ITAB,WEIGHT,
     2               MS       ,IN  ,NDOF ,D   ,DR   )
        ENDDO
       ELSE
        DO I=1,NRBYAC
         N=IRBYAC(I)
         K=IRBYAC(I+NRBYKIN)+1
         CALL RBY_IMP5(X   ,RBY(1,N),LPBY(K),ITRMAX  ,
     1               NPBY(1,N),SKEW,ISKEW,ITAB,WEIGHT,
     2               MS       ,IN  ,NDOF ,D   ,DR   )
        ENDDO
       END IF
      END IF

      IF (NUMELR>0) THEN
       CALL RBY_IMRD(NRBYAC,IRBYAC,X  ,RBY,LPBY,
     1               NPBY,SKEW,ISKEW,ITAB,WEIGHT,
     2               MS ,IN   ,NDOF ,D   ,
     3               DR ,IXR )
      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMP2                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        RBY_IMPD                      source/constraints/general/rbody/rby_impd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBY_IMP2(
     1                  X  ,RBY ,NOD  ,
     1                  NBY,SKEW,ISKEW,ITAB,WEIGHT,
     2                  MS ,IN  ,NDOF ,D     ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), ISKEW(*),ITAB(*), WEIGHT(*)
      INTEGER NDOF(*)
C     REAL
      my_real
     .   X(3,*), RBY(*),SKEW(LSKEW,*),IN(*),MS(*),D(3,*),DR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, NSN,  I, N
C     REAL
      my_real
     .   XS,YS,ZS
C-----------------------------------------------
      M    =NBY(1)
C optimisation spmd
      IF (IMACH==3.AND.M<0) RETURN
      NSN  =NBY(2)
C       IF (NDOF(M)<6) WRITE(*,*)'WARNING'
       DO I=1,NSN
        N = NOD(I)
        XS=X(1,N)-X(1,M)
        YS=X(2,N)-X(2,M)
        ZS=X(3,N)-X(3,M)
        D(1,N)=D(1,M)+DR(2,M)*ZS-DR(3,M)*YS
        D(2,N)=D(2,M)-DR(1,M)*ZS+DR(3,M)*XS
        D(3,N)=D(3,M)+DR(1,M)*YS-DR(2,M)*XS
        IF (NDOF(N)>3) THEN
         DR(1,N)= DR(1,M)
         DR(2,N)= DR(2,M)
         DR(3,N)= DR(3,M)
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMP3                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBY_IMP3(X   ,M   ,N   ,D   ,DR   ,
     .                    A   ,AR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M,N
C     REAL
      my_real
     .   X(3,*), D(3,*),DR(3,*), A(3,*), AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   XS,YS,ZS
C-----------------------------------------------
      IF (M<0) RETURN
        XS=X(1,N)-X(1,M)
        YS=X(2,N)-X(2,M)
        ZS=X(3,N)-X(3,M)
        D(1,N)=D(1,M)+DR(2,M)*ZS-DR(3,M)*YS
        D(2,N)=D(2,M)-DR(1,M)*ZS+DR(3,M)*XS
        D(3,N)=D(3,M)+DR(1,M)*YS-DR(2,M)*XS
        A(1,M)=ZERO
        A(2,M)=ZERO
        A(3,M)=ZERO
        A(1,N)=ZERO
        A(2,N)=ZERO
        A(3,N)=ZERO
        AR(1,M)=ZERO
        AR(2,M)=ZERO
        AR(3,M)=ZERO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMRD                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        RBY_IMPD                      source/constraints/general/rbody/rby_impd.F
Chd|-- calls ---------------
Chd|        RBY_IMP4                      source/constraints/general/rbody/rby_impd.F
Chd|====================================================================
      SUBROUTINE RBY_IMRD(NRBYAC,IRBYAC,X  ,RBY,LPBY,
     1                   NPBY,SKEW,ISKEW,ITAB,WEIGHT,
     2                   MS ,IN  ,NDOF ,D   ,
     3                   DR ,IXR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*)
      INTEGER NDOF(*),NRBYAC,IRBYAC(*),IXR(NIXR,*)
C     REAL
      my_real
     .   X(3,*), RBY(NRBY,*), SKEW(LSKEW,*),
     .   IN(*),MS(*),D(3,*), DR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,M, N,NSN,NS,K,ITAG(NUMNOD),N1,N2
C-----------------------------------------------
         DO I =1,NUMNOD
          ITAG(I) =0
         ENDDO
C
        DO I=1,NRBYAC
         N=IRBYAC(I)
         K=IRBYAC(I+NRBYKIN)
         M=NPBY(1,N)
         NSN=NPBY(2,N)
         IF (M>0) THEN
          DO J=1,NSN
           NS=LPBY(K+J)
           ITAG(NS) =M
          ENDDO
         ENDIF
        ENDDO
C
        DO I=1,NUMELR
         N1= IXR(2,I)
         N2= IXR(3,I)
         IF (ITAG(N1)>0.AND.ITAG(N1)==ITAG(N2)) THEN
          CALL RBY_IMP4( X  ,N1 ,N2  ,ITAG(N1),D  ,DR )
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMP4                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        RBY_IMRD                      source/constraints/general/rbody/rby_impd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBY_IMP4(X  ,N1 ,N2  ,M   ,D  ,DR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,M
C     REAL
      my_real
     .   X(3,*),D(3,*),DR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N ,K,IT,NP
C     REAL
      my_real
     .   XS,YS,ZS,X1(3),X2(3),DRX,DRY,DRZ,L0,L1,S
C-----------------------------------------------
       NP =10
       DRX = DR(1,M)/NP
       DRY = DR(2,M)/NP
       DRZ = DR(3,M)/NP
       X1(1)=X(1,N1)
       X1(2)=X(2,N1)
       X1(3)=X(3,N1)
       X2(1)=X(1,N2)
       X2(2)=X(2,N2)
       X2(3)=X(3,N2)
       DO IT = 1,NP
        XS=X1(1)-X(1,M)
        YS=X1(2)-X(2,M)
        ZS=X1(3)-X(3,M)
        X1(1)=X1(1)+DRY*ZS-DRZ*YS
        X1(2)=X1(2)-DRX*ZS+DRZ*XS
        X1(3)=X1(3)+DRX*YS-DRY*XS
        XS=X2(1)-X(1,M)
        YS=X2(2)-X(2,M)
        ZS=X2(3)-X(3,M)
        X2(1)=X2(1)+DRY*ZS-DRZ*YS
        X2(2)=X2(2)-DRX*ZS+DRZ*XS
        X2(3)=X2(3)+DRX*YS-DRY*XS
       ENDDO
C
       XS=X1(1)-X(1,N1)
       YS=X1(2)-X(2,N1)
       ZS=X1(3)-X(3,N1)
       D(1,N1)=D(1,M)+XS
       D(2,N1)=D(2,M)+YS
       D(3,N1)=D(3,M)+ZS
       XS=X2(1)-X(1,N2)
       YS=X2(2)-X(2,N2)
       ZS=X2(3)-X(3,N2)
       D(1,N2)=D(1,M)+XS
       D(2,N2)=D(2,M)+YS
       D(3,N2)=D(3,M)+ZS
C
       XS=X(1,N2)-X(1,N1)
       YS=X(2,N2)-X(2,N1)
       ZS=X(3,N2)-X(3,N1)
       L0 = SQRT(XS*XS+YS*YS+ZS*ZS)
       IF (L0<EM10) THEN
        D(1,N2)=D(1,N1)
        D(2,N2)=D(2,N1)
        D(3,N2)=D(3,N1)
       ELSE
        XS=XS+D(1,N2)-D(1,N1)
        YS=YS+D(2,N2)-D(2,N1)
        ZS=ZS+D(3,N2)-D(3,N1)
        L1 = SQRT(XS*XS+YS*YS+ZS*ZS)
        S = L0/L1
        DO K =1,3
         D(K,N1)=S*(X(K,N1)+D(K,N1))-X(K,N1)
         D(K,N2)=S*(X(K,N2)+D(K,N2))-X(K,N2)
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMP5                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        RBY_IMPD                      source/constraints/general/rbody/rby_impd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBY_IMP5(
     1                  X  ,RBY ,NOD  ,ITRMAX,
     1                  NBY,SKEW,ISKEW,ITAB,WEIGHT,
     2                  MS ,IN  ,NDOF ,D     ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), ISKEW(*),ITAB(*), WEIGHT(*)
      INTEGER NDOF(*),ITRMAX
C     REAL
      my_real
     .   X(3,*), RBY(*),SKEW(LSKEW,*),IN(*),MS(*),D(3,*),DR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, NSN,  I, N,NP,J
C     REAL
      my_real
     .   XS,YS,ZS,LMAX,RMAX,LSM1,DRX,DRY,DRZ,A,B
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: LSM
C-----------------------------------------------
      M    =NBY(1)
C optimisation spmd
      IF (IMACH==3.AND.M<0) RETURN
      NSN  =NBY(2)
C---determine iteration num.
      LMAX = ZERO
      RMAX = MAX(ABS(DR(1,M)),ABS(DR(2,M)),ABS(DR(3,M)))
       ALLOCATE(LSM(NSN))
       DO I=1,NSN
        N = NOD(I)
        XS=X(1,N)-X(1,M)
        YS=X(2,N)-X(2,M)
        ZS=X(3,N)-X(3,M)
	LSM(I) = XS*XS+YS*YS+ZS*ZS
        LMAX = MAX(LMAX,LSM(I))
       ENDDO
       NP = RMAX*SQRT(LMAX)*FIVEUANTE
       NP = MIN(ITRMAX,NP)
      IF (NP<=1) THEN
       DO I=1,NSN
        N = NOD(I)
        XS=X(1,N)-X(1,M)
        YS=X(2,N)-X(2,M)
        ZS=X(3,N)-X(3,M)
        D(1,N)=D(1,M)+DR(2,M)*ZS-DR(3,M)*YS
        D(2,N)=D(2,M)-DR(1,M)*ZS+DR(3,M)*XS
        D(3,N)=D(3,M)+DR(1,M)*YS-DR(2,M)*XS
        IF (NDOF(N)>3) THEN
         DR(1,N)= DR(1,M)
         DR(2,N)= DR(2,M)
         DR(3,N)= DR(3,M)
        ENDIF
        XS=X(1,N)-X(1,M)+D(1,N)-D(1,M)
        YS=X(2,N)-X(2,M)+D(2,N)-D(2,M)
        ZS=X(3,N)-X(3,M)+D(3,N)-D(3,M)
       ENDDO
      ELSE
        DO I=1,NSN
         N = NOD(I)
         D(1,N)=X(1,N)
         D(2,N)=X(2,N)
         D(3,N)=X(3,N)
        ENDDO
         DRX= DR(1,M)/NP
         DRY= DR(2,M)/NP
         DRZ= DR(3,M)/NP
        DO J=1,NP
         DO I=1,NSN
          N = NOD(I)
          XS=D(1,N)-X(1,M)
          YS=D(2,N)-X(2,M)
          ZS=D(3,N)-X(3,M)
          D(1,N)=D(1,N)+DRY*ZS-DRZ*YS
          D(2,N)=D(2,N)-DRX*ZS+DRZ*XS
          D(3,N)=D(3,N)+DRX*YS-DRY*XS
         ENDDO
        ENDDO
        DO I=1,NSN
          N = NOD(I)
          D(1,N)=D(1,M)+D(1,N)-X(1,N)
          D(2,N)=D(2,M)+D(2,N)-X(2,N)
          D(3,N)=D(3,M)+D(3,N)-X(3,N)
         IF (NDOF(N)>3) THEN
          DR(1,N)= DR(1,M)
          DR(2,N)= DR(2,M)
          DR(3,N)= DR(3,M)
         ENDIF
        ENDDO
      END IF !(NP<=1) THEN
C
       DO I=1,NSN
        IF (LSM(I)>EM10) THEN
         N = NOD(I)
         XS=X(1,N)-X(1,M)+D(1,N)-D(1,M)
         YS=X(2,N)-X(2,M)+D(2,N)-D(2,M)
         ZS=X(3,N)-X(3,M)+D(3,N)-D(3,M)
	 LSM1 =XS*XS+YS*YS+ZS*ZS
	 A = SQRT(LSM(I)/LSM1)-ONE
         D(1,N)=D(1,N)+A*XS
         D(2,N)=D(2,N)+A*YS
         D(3,N)=D(3,N)+A*ZS
        ENDIF
       ENDDO
        DEALLOCATE(LSM)
C
      RETURN
      END
Chd|====================================================================
Chd|  RBY_IMP7                      source/constraints/general/rbody/rby_impd.F
Chd|-- called by -----------
Chd|        RBY_IMPD                      source/constraints/general/rbody/rby_impd.F
Chd|-- calls ---------------
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBY_IMP7(
     1                  X  ,RBY ,NOD  ,
     1                  NBY,SKEW,ISKEW,ITAB,WEIGHT,
     2                  MS ,IN  ,NDOF ,D     ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), ISKEW(*),ITAB(*), WEIGHT(*)
      INTEGER NDOF(*),ITRMAX
C     REAL
      my_real
     .   X(3,*), RBY(*),SKEW(LSKEW,*),IN(*),MS(*),D(3,*),DR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C PURPOSE: calculate displacement increment of secnd node by displacement increment of main node.
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, NSN,  I, N,NP,J,K
C     REAL
      DOUBLE PRECISION
     . XS(3), DS(3), VRM(3)
C-----------------------------------------------

      M = NBY(1)
C optimisation spmd
      IF (IMACH==3.AND.M<0) RETURN

      DO I = 1, 3
        VRM(I) = DR(I,M)
      END DO

      NSN  =NBY(2)
      DO I=1,NSN
        N = NOD(I)
        DO J = 1 , 3
          XS(J) = X(J,N) - X(J,M)
        END DO
        CALL VELROT(VRM,XS,DS)
        DO J = 1 , 3
          D(J,N) = D(J,M) + DS(J)
        END DO

        IF (NDOF(N)>3) THEN
         DR(1,N)= DR(1,M)
         DR(2,N)= DR(2,M)
         DR(3,N)= DR(3,M)
        ENDIF

      END DO ! I=1,NSN

      RETURN

      END
