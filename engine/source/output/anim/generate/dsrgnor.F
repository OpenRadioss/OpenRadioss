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
Chd|  DSRGNOR                       source/output/anim/generate/dsrgnor.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     source/output/tools/sortie_c.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DSRGNOR(IGRSURF,BUFSF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     BUFSF(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I3000,NXX,NYY,NZZ
      INTEGER N,ADRBUF
      INTEGER INOE, I, J, K, DGR
      my_real XG,YG,ZG,A,B,C,ROT(9),
     1        AN,BN,CN
      my_real
     1   NCOR(3,384),
     2   NNOR(3,384),
     3   XA,XB,XC,LA,LB,LC,
     4   XL,YL,ZL,E,D,
     5   XLN,YLN,ZLN,NXL,NYL,NZL,NX,NY,NZ,NORMN
      my_real
     1   XX0,YY0,ZZ0,
     2   X0(6),Y0(6),Z0(6),DX0(6),DY0(6),DZ0(6),DX1(6),DY1(6),DZ1(6)
      REAL R4
      DATA DX0/ 0., 0., 0., 0., 0., 0./
      DATA DY0/ 1.,-1., 0., 0., 0., 0./
      DATA DZ0/ 0., 0.,-1., 1., 1.,-1./
      DATA DX1/ 1., 1., 1., 1., 0., 0./
      DATA DY1/ 0., 0., 0., 0., 1., 1./
      DATA DZ1/ 0., 0., 0., 0., 0., 0./
      DATA X0 /-3.5,-3.5,-3.5,-3.5,-3.5, 3.5/
      DATA Y0 /-3.5, 3.5,-3.5, 3.5,-3.5,-3.5/
      DATA Z0 /-3.5, 3.5, 3.5,-3.5,-3.5, 3.5/
C-----------------------------------------------
      I3000 = 3000
      IF (ISPMD/=0) GOTO 100
C
      DO 200 N=1,NSURF
       IF (IGRSURF(N)%TYPE/=101) GOTO 200
       ADRBUF=IGRSURF(N)%IAD_BUFR
C-------------------------------------------------------
c      Parametres de l'ellipsoide.
C-------------------------------------------------------
       DGR=BUFSF(ADRBUF+36)
       XG=BUFSF(ADRBUF+4)
       YG=BUFSF(ADRBUF+5)
       ZG=BUFSF(ADRBUF+6)
       A =BUFSF(ADRBUF+1)
       B =BUFSF(ADRBUF+2)
       C =BUFSF(ADRBUF+3)
       DO I=1,9
        ROT(I)=BUFSF(ADRBUF+7+I-1)
       END DO
C-------------------------------------------------------
C      - Calcul des noeuds sur le cube A,B,C.
C-------------------------------------------------------
       INOE=0
       DO I = 1,6
        XX0 = X0(I)
        YY0 = Y0(I)
        ZZ0 = Z0(I)
        DO J = 1,8
          XL = XX0
          YL = YY0
          ZL = ZZ0
          DO K = 1,8
            INOE=INOE+1
            NCOR(1,INOE) = A*XL * THIRD
            NCOR(2,INOE) = B*YL * THIRD
            NCOR(3,INOE) = C*ZL * THIRD            
            XL = XL + DX0(I)
            YL = YL + DY0(I)
            ZL = ZL + DZ0(I)
          ENDDO
          XX0 = XX0 + DX1(I)
          YY0 = YY0 + DY1(I)
          ZZ0 = ZZ0 + DZ1(I)
        ENDDO
       ENDDO
C-------------------------------------------------------
C      - Calcul de la normale : projection radiale.
C-------------------------------------------------------
       AN=A**DGR
       BN=B**DGR
       CN=C**DGR
       INOE=0
       DO I=1,384
        INOE=INOE+1
        XL=NCOR(1,INOE)
        YL=NCOR(2,INOE)
        ZL=NCOR(3,INOE)
C
        XLN=XL**DGR
        YLN=YL**DGR
        ZLN=ZL**DGR
        E=ABS(XLN)/AN+ABS(YLN)/BN+ABS(ZLN)/CN
        E=EXP(LOG(E)/DGR)
        XL=XL/E
        YL=YL/E
        ZL=ZL/E
C
        NXL=XL**(DGR-1)/AN
        IF (XL*NXL<ZERO) NXL=-NXL
        NYL=YL**(DGR-1)/BN
        IF (YL*NYL<ZERO) NYL=-NYL
        NZL=ZL**(DGR-1)/CN
        IF (ZL*NZL<ZERO) NZL=-NZL
        NX  =ROT(1)*NXL+ROT(4)*NYL+ROT(7)*NZL
        NY  =ROT(2)*NXL+ROT(5)*NYL+ROT(8)*NZL
        NZ  =ROT(3)*NXL+ROT(6)*NYL+ROT(9)*NZL
        NORMN  =SQRT(NX*NX+NY*NY+NZ*NZ)
        NX  =NX/NORMN
        NY  =NY/NORMN
        NZ  =NZ/NORMN
        NNOR(1,INOE)=THREE1000*NX
        NNOR(2,INOE)=THREE1000*NY
        NNOR(3,INOE)=THREE1000*NZ        
       END DO
C-------------------------------------------------------
C      Ecriture des normales aux noeuds.
C-------------------------------------------------------
       INOE=0
       DO I=1,384
        INOE=INOE+1
        NX = NNOR(1,INOE)
        CALL WRITE_S_C(NINT(NX),1)
        NY = NNOR(2,INOE)
        CALL WRITE_S_C(NINT(NY),1)
        NZ = NNOR(3,INOE)
        CALL WRITE_S_C(NINT(NZ),1)
       END DO
C-------------------------------------------------------
 200   CONTINUE
 100  CONTINUE
      RETURN
      END
