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
Chd|  SECTION_C                     source/tools/sect/section_c.F 
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        SECTION_SKEW                  source/tools/sect/section_skew.F
Chd|        SECTION_SKEWP                 source/tools/sect/section_skewp.F
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTION_C(LFT,LLT,NFT,NSEG,N1,
     2                     N2,N3,NSTRF,X,V,VR,FSAV,
     3                     IXC,FOPTA,SECFCUM,
     4                     FX,FY,FZ,MX,MY,MZ,
     5                     TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     7                     XSEC,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,
     .        TYPE,NSINT,IPARSENS
      INTEGER NSTRF(2,*),IXC(NIXC,*),IFRAM,NNOD,NOD(*)
      my_real
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),
     .   V(3,*), VR(3,*),MS(*),XSEC(4,3)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(0:15,4),
     .   NSA, J, I, K, I1, I2, IPACK, N, JJ
      my_real
     .   FX(MVSIZ,4), FY(MVSIZ,4), FZ(MVSIZ,4), MX(MVSIZ,4),
     .   MY(MVSIZ,4), MZ(MVSIZ,4), FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   MX1(MVSIZ), MY1(MVSIZ), MZ1(MVSIZ), DX1(MVSIZ),
     .   DY1(MVSIZ), DZ1(MVSIZ),FST(16), DX11, DY11, DZ11,
     .   MSX, MSY, MSZ, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6, XXN, YYN, ZZN,
     .   D13, XXC, YYC, ZZC, FSX, FSY, FSZ, FN, FSNX, FSNY, FSNZ, FSTX,
     .   FSTY, FSTZ, DMX, DMY, DMZ
      my_real
     .   MSXPHI, MSYPHI, MSZPHI, FSXPHI,
     .   FSYPHI, FSZPHI, FNPHI, FSNXPHI, FSNYPHI, FSNZPHI, FSTXPHI,
     .   FSTYPHI, FSTZPHI, DMXPHI, DMYPHI, DMZPHI
      my_real
     .   XX1PHI,XX3PHI,XX4PHI,XX5PHI,XX6PHI
      my_real
     .   YY1PHI,YY3PHI,YY4PHI,YY5PHI,YY6PHI
      my_real
     .   ZZ1PHI,ZZ3PHI,ZZ4PHI,ZZ5PHI,ZZ6PHI
      my_real
     .   AL4PHI,AL5PHI,AL6PHI,XX5T,YY5T,ZZ5T
      my_real
     .  AL4,AL5,AL6
      my_real, DIMENSION(:,:), ALLOCATABLE :: FSTPARIT
C-----------------------------------------------
C
      DATA UNPACK/0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,
     .            0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,
     .            0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,
     .            0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1/
C---------------------------------------------------------
C---------------------------------------------------------
      IF(NSEG==0) RETURN
      IF(LFT+NFT>NSTRF(1,NSEG)) RETURN
      IF(LLT+NFT<NSTRF(1,1   )) RETURN
C---------------------------------------------------------
      NSA=0
C
       IF(IVECTOR==0) THEN
        DO 20 J=1,NSEG
        I=NSTRF(1,J)-NFT
        IF (LFT>I) GOTO 20
        IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20    CONTINUE
  30    CONTINUE
       ELSE
         IF (NSEG>15) THEN
           DO J=1,NSEG
             I=NSTRF(1,J)-NFT
             IF (LFT<=I.AND.LLT>=I) THEN
               NSA=NSA+1
               JJJ(NSA)=J
             ENDIF
           ENDDO
         ELSE
           DO J=1,NSEG
             I=NSTRF(1,J)-NFT
             IF (LFT<=I.AND.LLT>=I) THEN
               NSA=NSA+1
               JJJ(NSA)=J
             ENDIF
           ENDDO
         ENDIF
       ENDIF
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       IF(IPARSENS/=0) THEN
         ALLOCATE(FSTPARIT(12,NSA))
         DO J=1,NSA
           DO I=1,12
            FSTPARIT(I,J) = ZERO
           ENDDO
         ENDDO
       ENDIF
C
       DO I=1,16
         FST(I)=ZERO
       ENDDO
C
       IF(NSPMD==1) THEN
        CALL SECTION_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SECTION_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C

      IF((IPARSENS==0)) THEN      ! Parith/Off
       IF (NSA>15.OR.IVECTOR==0) THEN
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
          DO I1=1,4
           IF(UNPACK(IPACK,I1)/=0)THEN
C
             N = IXC(I1+1,NSTRF(1,J))
             DX11=X(1,N)
             DY11=X(2,N)
             DZ11=X(3,N)
C
             FSX=FX(I,I1)
             FSY=FY(I,I1)
             FSZ=FZ(I,I1)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX11=DX11-XXC
             DY11=DY11-YYC
             DZ11=DZ11-ZZC
C
             MSX =DY11*FSZ-DZ11*FSY
             MSY =DZ11*FSX-DX11*FSZ
             MSZ =DX11*FSY-DY11*FSX
C
             MSX =MSX+MX(I,I1)
             MSY =MSY+MY(I,I1)
             MSZ =MSZ+MZ(I,I1)
C
             DMX =MSX*XX4+MSY*YY4+MSZ*ZZ4
             DMY =MSX*XX5+MSY*YY5+MSZ*ZZ5
             DMZ =MSX*XX6+MSY*YY6+MSZ*ZZ6
C
              FST(1)=FST(1)+FSNX
              FST(2)=FST(2)+FSNY
              FST(3)=FST(3)+FSNZ
              FST(4)=FST(4)+FSTX
              FST(5)=FST(5)+FSTY
              FST(6)=FST(6)+FSTZ
              FST(7)=FST(7)+DMX
              FST(8)=FST(8)+DMY
              FST(9)=FST(9)+DMZ
              FST(10) = FST(10) + FSX
              FST(11) = FST(11) + FSY
              FST(12) = FST(12) + FSZ
              FST(13) = FST(13) + MSX
              FST(14) = FST(14) + MSY
              FST(15) = FST(15) + MSZ
              FST(16)=FST(16)
     .        +FSX*V(1,N) +FSY*V(2,N) +FSZ*V(3,N)
     .        +MSX*VR(1,N)+MSY*VR(2,N)+MSZ*VR(3,N)
C
           ENDIF
          ENDDO
        ENDDO
       ELSE
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
          DO I1=1,4
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXC(I1+1,NSTRF(1,J))
C
             DX11=X(1,N)
             DY11=X(2,N)
             DZ11=X(3,N)
C
             FSX=FX(I,I1)
             FSY=FY(I,I1)
             FSZ=FZ(I,I1)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX11=DX11-XXC
             DY11=DY11-YYC
             DZ11=DZ11-ZZC
C
             MSX =DY11*FSZ-DZ11*FSY
             MSY =DZ11*FSX-DX11*FSZ
             MSZ =DX11*FSY-DY11*FSX
C
             MSX =MSX+MX(I,I1)
             MSY =MSY+MY(I,I1)
             MSZ =MSZ+MZ(I,I1)
C
             DMX =MSX*XX4+MSY*YY4+MSZ*ZZ4
             DMY =MSX*XX5+MSY*YY5+MSZ*ZZ5
             DMZ =MSX*XX6+MSY*YY6+MSZ*ZZ6
C
              FST(1)=FST(1)+FSNX
              FST(2)=FST(2)+FSNY
              FST(3)=FST(3)+FSNZ
              FST(4)=FST(4)+FSTX
              FST(5)=FST(5)+FSTY
              FST(6)=FST(6)+FSTZ
              FST(7)=FST(7)+DMX
              FST(8)=FST(8)+DMY
              FST(9)=FST(9)+DMZ
              FST(10) = FST(10) + FSX
              FST(11) = FST(11) + FSY
              FST(12) = FST(12) + FSZ
              FST(13) = FST(13) + MSX
              FST(14) = FST(14) + MSY
              FST(15) = FST(15) + MSZ
              FST(16)=FST(16)
     .        +FSX*V(1,N) +FSY*V(2,N) +FSZ*V(3,N)
     .        +MSX*VR(1,N)+MSY*VR(2,N)+MSZ*VR(3,N)
           ENDIF
         ENDDO
        ENDDO
       ENDIF
      ELSE   ! Parith/On
       IF (NSA>15.OR.IVECTOR==0) THEN
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
          DO I1=1,4
           IF(UNPACK(IPACK,I1)/=0)THEN
C
             N = IXC(I1+1,NSTRF(1,J))
             DX11=X(1,N)
             DY11=X(2,N)
             DZ11=X(3,N)
C
             FSX=FX(I,I1)
             FSY=FY(I,I1)
             FSZ=FZ(I,I1)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX11=DX11-XXC
             DY11=DY11-YYC
             DZ11=DZ11-ZZC
C
             MSX =DY11*FSZ-DZ11*FSY
             MSY =DZ11*FSX-DX11*FSZ
             MSZ =DX11*FSY-DY11*FSX
C
             MSX =MSX+MX(I,I1)
             MSY =MSY+MY(I,I1)
             MSZ =MSZ+MZ(I,I1)
C
             DMX =MSX*XX4+MSY*YY4+MSZ*ZZ4
             DMY =MSX*XX5+MSY*YY5+MSZ*ZZ5
             DMZ =MSX*XX6+MSY*YY6+MSZ*ZZ6
C
              FST(1)=FST(1)+FSNX
              FST(2)=FST(2)+FSNY
              FST(3)=FST(3)+FSNZ
              FST(4)=FST(4)+FSTX
              FST(5)=FST(5)+FSTY
              FST(6)=FST(6)+FSTZ
              FST(7)=FST(7)+DMX
              FST(8)=FST(8)+DMY
              FST(9)=FST(9)+DMZ
              FST(10) = FST(10) + FSX
              FST(11) = FST(11) + FSY
              FST(12) = FST(12) + FSZ
              FST(13) = FST(13) + MSX
              FST(14) = FST(14) + MSY
              FST(15) = FST(15) + MSZ
              FST(16)=FST(16)
     .        +FSX*V(1,N) +FSY*V(2,N) +FSZ*V(3,N)
     .        +MSX*VR(1,N)+MSY*VR(2,N)+MSZ*VR(3,N)
C
              FSTPARIT(1,K)=FSTPARIT(1,K)+FSNX
              FSTPARIT(2,K)=FSTPARIT(2,K)+FSNY
              FSTPARIT(3,K)=FSTPARIT(3,K)+FSNZ
              FSTPARIT(4,K)=FSTPARIT(4,K)+FSTX
              FSTPARIT(5,K)=FSTPARIT(5,K)+FSTY
              FSTPARIT(6,K)=FSTPARIT(6,K)+FSTZ
              FSTPARIT(7,K)=FSTPARIT(7,K)+MSX
              FSTPARIT(8,K)=FSTPARIT(8,K)+MSY
              FSTPARIT(9,K)=FSTPARIT(9,K)+MSZ
              FSTPARIT(10,K)=FSTPARIT(10,K)     +
     .                 ( XX4*(FSNX+FSTX)  +
     .                 YY4*(FSNY+FSTY)          +
     .                 ZZ4*(FSNZ+FSTZ) )
              FSTPARIT(11,K)=FSTPARIT(11,K)     +
     .                 ( XX5*(FSNX+FSTX)  +
     .                 YY5*(FSNY+FSTY)          +
     .                 ZZ5*(FSNZ+FSTZ) )
              FSTPARIT(12,K)=FSTPARIT(12,K)     +
     .                 ( XX6*(FSNX+FSTX)  +
     .                 YY6*(FSNY+FSTY)          +
     .                 ZZ6*(FSNZ+FSTZ) )
C
           ENDIF
          ENDDO
        ENDDO
C
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
       ELSE
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
          DO I1=1,4
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXC(I1+1,NSTRF(1,J))
C
             DX11=X(1,N)
             DY11=X(2,N)
             DZ11=X(3,N)
C
             FSX=FX(I,I1)
             FSY=FY(I,I1)
             FSZ=FZ(I,I1)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX11=DX11-XXC
             DY11=DY11-YYC
             DZ11=DZ11-ZZC
C
             MSX =DY11*FSZ-DZ11*FSY
             MSY =DZ11*FSX-DX11*FSZ
             MSZ =DX11*FSY-DY11*FSX
C
             MSX =MSX+MX(I,I1)
             MSY =MSY+MY(I,I1)
             MSZ =MSZ+MZ(I,I1)
C
             DMX =MSX*XX4+MSY*YY4+MSZ*ZZ4
             DMY =MSX*XX5+MSY*YY5+MSZ*ZZ5
             DMZ =MSX*XX6+MSY*YY6+MSZ*ZZ6
C
              FST(1)=FST(1)+FSNX
              FST(2)=FST(2)+FSNY
              FST(3)=FST(3)+FSNZ
              FST(4)=FST(4)+FSTX
              FST(5)=FST(5)+FSTY
              FST(6)=FST(6)+FSTZ
              FST(7)=FST(7)+DMX
              FST(8)=FST(8)+DMY
              FST(9)=FST(9)+DMZ
              FST(10) = FST(10) + FSX
              FST(11) = FST(11) + FSY
              FST(12) = FST(12) + FSZ
              FST(13) = FST(13) + MSX
              FST(14) = FST(14) + MSY
              FST(15) = FST(15) + MSZ
              FST(16)=FST(16)
     .        +FSX*V(1,N) +FSY*V(2,N) +FSZ*V(3,N)
     .        +MSX*VR(1,N)+MSY*VR(2,N)+MSZ*VR(3,N)
C
              FSTPARIT(1,K)=FSTPARIT(1,K)+FSNX
              FSTPARIT(2,K)=FSTPARIT(2,K)+FSNY
              FSTPARIT(3,K)=FSTPARIT(3,K)+FSNZ
              FSTPARIT(4,K)=FSTPARIT(4,K)+FSTX
              FSTPARIT(5,K)=FSTPARIT(5,K)+FSTY
              FSTPARIT(6,K)=FSTPARIT(6,K)+FSTZ
              FSTPARIT(7,K)=FSTPARIT(7,K)+MSX
              FSTPARIT(8,K)=FSTPARIT(8,K)+MSY
              FSTPARIT(9,K)=FSTPARIT(9,K)+MSZ
              FSTPARIT(10,K)=FSTPARIT(10,K)     +
     .                 ( XX4*(FSNX+FSTX)  +
     .                 YY4*(FSNY+FSTY)          +
     .                 ZZ4*(FSNZ+FSTZ) )
              FSTPARIT(11,K)=FSTPARIT(11,K)     +
     .                 ( XX5*(FSNX+FSTX)  +
     .                 YY5*(FSNY+FSTY)          +
     .                 ZZ5*(FSNZ+FSTZ) )
              FSTPARIT(12,K)=FSTPARIT(12,K)     +
     .                 ( XX6*(FSNX+FSTX)  +
     .                 YY6*(FSNY+FSTY)          +
     .                 ZZ6*(FSNZ+FSTZ) )
C
           ENDIF
         ENDDO
        ENDDO
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
       ENDIF
      ENDIF
C
#include "lockon.inc"
              FSAV(1)=FSAV(1)+DT12*FST(1)
              FSAV(2)=FSAV(2)+DT12*FST(2)
              FSAV(3)=FSAV(3)+DT12*FST(3)
              FSAV(4)=FSAV(4)+DT12*FST(4)
              FSAV(5)=FSAV(5)+DT12*FST(5)
              FSAV(6)=FSAV(6)+DT12*FST(6)
              FSAV(7)=FSAV(7)+DT12*FST(7)
              FSAV(8)=FSAV(8)+DT12*FST(8)
              FSAV(9)=FSAV(9)+DT12*FST(9)
              FSAV(10)=FSAV(10)+DT12*FST(16)
              FSAV(31)=FSAV(31)+DT12*FST(13)
              FSAV(32)=FSAV(32)+DT12*FST(14)
              FSAV(33)=FSAV(33)+DT12*FST(15)
              FSAV(34)=FSAV(34) + DT12* (XX4*(FST(1)+FST(4)) +
     .                 YY4*(FST(2)+FST(5)) + ZZ4*(FST(3)+FST(6)))
              FSAV(35)=FSAV(35) + DT12* (XX5*(FST(1)+FST(4)) +
     .                 YY5*(FST(2)+FST(5)) + ZZ5*(FST(3)+FST(6)))
              FSAV(36)=FSAV(36) + DT12* (XX6*(FST(1)+FST(4)) +
     .                 YY6*(FST(2)+FST(5)) + ZZ6*(FST(3)+FST(6)))
              FSAV(37)=XXC
              FSAV(38)=YYC
              FSAV(39)=ZZC
              FOPTA(1) = FOPTA(1) + FST(10)
              FOPTA(2) = FOPTA(2) + FST(11)
              FOPTA(3) = FOPTA(3) + FST(12)
              FOPTA(4) = FOPTA(4) + FST(13)
              FOPTA(5) = FOPTA(5) + FST(14)
              FOPTA(6) = FOPTA(6) + FST(15)
#include "lockoff.inc"
C
      ELSE
C
#include "lockon.inc"
       DO I1=1,4
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXC(I1+1,NSTRF(1,J))
             SECFCUM(1,N)=SECFCUM(1,N)+FX(I,I1)
             SECFCUM(2,N)=SECFCUM(2,N)+FY(I,I1)
             SECFCUM(3,N)=SECFCUM(3,N)+FZ(I,I1)
             SECFCUM(5,N)=SECFCUM(5,N)+MX(I,I1)
             SECFCUM(6,N)=SECFCUM(6,N)+MY(I,I1)
             SECFCUM(7,N)=SECFCUM(7,N)+MZ(I,I1)
           ENDIF
         ENDDO
       ENDDO
#include "lockoff.inc"
      ENDIF
C
      IF((NSA/=0).AND.(IPARSENS/=0)) THEN
       DEALLOCATE(FSTPARIT)
      ENDIF
C
      RETURN
      END
