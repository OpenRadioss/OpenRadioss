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
Chd|  DXYZSECT                      source/output/anim/dxyzsect.F 
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        DXWALC                        source/output/anim/dxwalc.F   
Chd|        DXWALL                        source/output/anim/dxwall.F   
Chd|        DXWALP                        source/output/anim/dxwalp.F   
Chd|        DXWALS                        source/output/anim/dxwals.F   
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE DXYZSECT (NSTRF,RWBUF,NPRW  ,X    , XMIN,
     2                     YMIN ,ZMIN ,XMAX ,YMAX  , ZMAX,
     3                     ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*),NPRW(*),ITAB(*)
      my_real
     .  RWBUF(NRWLP,*),X(3,*),XMIN ,YMIN ,ZMIN  ,XMAX ,YMAX, ZMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I, K, K0, K1, N, NSEG, N1, N2, N3, N4,MSR, ITYP
      my_real
     .   XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, D13, XXC, YYC, ZZC, AL4,XWL,YWL,ZWL,
     .   PMAIN,LOC_PROC

      my_real
     .   XSEC(3,3,NSECT)
      REAL R4,SBUF(3*NSECT)
CC-----------------------------------------------
      K1=1
C
       K1 = 33
       DO I=1,NSECT
        N1 = NSTRF(K1+1)
        N2 = NSTRF(K1+2)
        N3 = NSTRF(K1+3)
        XX1=X(1,N1)
        YY1=X(2,N1)
        ZZ1=X(3,N1)
        XX2=X(1,N2)
        YY2=X(2,N2)
        ZZ2=X(3,N2)
        XX3=X(1,N3)
        YY3=X(2,N3)
        ZZ3=X(3,N3)
        XX4=XX2-XX1
        YY4=YY2-YY1
        ZZ4=ZZ2-ZZ1
        AL4=SQRT(XX4**2+YY4**2+ZZ4**2)
        XX4=XX4/MAX(AL4,EM20)
        YY4=YY4/MAX(AL4,EM20)
        ZZ4=ZZ4/MAX(AL4,EM20)
C
        D13=(XX3-XX1)*XX4+(YY3-YY1)*YY4+(ZZ3-ZZ1)*ZZ4
        XXC=XX1+D13*XX4
        YYC=YY1+D13*YY4
        ZZC=ZZ1+D13*ZZ4
C
        R4 = XXC
        CALL WRITE_R_C(R4,1)
        R4 = YYC
        CALL WRITE_R_C(R4,1)
        R4 = ZZC
        CALL WRITE_R_C(R4,1)
C

         K1= NSTRF(K1+22)+2
       ENDDO
C
        K=1
        DO N=1,NRWALL
         N2=N +NRWALL
         N3=N2+NRWALL
         N4=N3+NRWALL
         MSR = NPRW(N3)
         IF(MSR==0)THEN
          XWL=RWBUF(4,N)
          YWL=RWBUF(5,N)
          ZWL=RWBUF(6,N)
         ELSE
          XWL=X(1,MSR)
          YWL=X(2,MSR)
          ZWL=X(3,MSR)
         ENDIF
         ITYP= NPRW(N4)
         IF(ITYP==4)THEN
           XWL = XWL + HALF*(RWBUF(7,N)+RWBUF(10,N))
           YWL = YWL + HALF*(RWBUF(8,N)+RWBUF(11,N))
           ZWL = ZWL + HALF*(RWBUF(9,N)+RWBUF(12,N)) 
         ENDIF
         K=K+NPRW(N)
         IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
         R4 = XWL
         CALL WRITE_R_C(R4,1)
         R4 = YWL
         CALL WRITE_R_C(R4,1)
         R4 = ZWL
         CALL WRITE_R_C(R4,1)
        ENDDO
C
      K=1
      DO N=1,NRWALL
         N2=N +NRWALL
         N3=N2+NRWALL
         N4=N3+NRWALL
         ITYP= NPRW(N4)

         IF(IABS(ITYP)==1)THEN
            CALL DXWALL(X,RWBUF(1,N),NPRW(N3),XMIN ,YMIN ,
     .                  ZMIN  ,XMAX ,YMAX  , ZMAX)
         ELSEIF(ITYP==2)THEN
            CALL DXWALC(X,RWBUF(1,N),NPRW(N3),XMIN ,YMIN ,
     .                  ZMIN  ,XMAX ,YMAX  , ZMAX)
         ELSEIF(ITYP==3)THEN
            CALL DXWALS(X,RWBUF(1,N),NPRW(N3))
         ELSEIF(ITYP==4)THEN
            CALL DXWALP(X,RWBUF(1,N),NPRW(N3))
         ENDIF
         K=K+NPRW(N)
         IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
      ENDDO
C
      RETURN
      END
