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
Chd|  SECTIOS6                      source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOS6 (
     1         LFT     ,LLT     ,NFT     ,NSEG       ,N1      ,
     2         N2      ,N3      ,NSTRF   ,X          ,V       ,
     3         FSAV    ,IXS     ,FOPTA   ,SECFCUM    ,FX      ,
     4         FY      ,FZ      ,TYPE    ,NSINT      ,IFRAM   ,
     5         NNOD    ,NOD     ,MS      ,XSEC       ,FBSAV6  , 
     6         IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,TYPE,NSINT
      INTEGER NSTRF(2,*),IXS(NIXS,*),IFRAM,NNOD,NOD(*)
      INTEGER IPARSENS
      my_real 
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),XSEC(4,3),
     .   FX(MVSIZ,8), FY(MVSIZ,8), FZ(MVSIZ,8), V(3,*) ,MS(*)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(0:255,8),
     .   NSA, J, I, K, I1, I2, IPACK, N,POWER2(8),IPERM(6),II,JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   DX1(MVSIZ),DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
     .   MSX, MSY, MSZ, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6, XXN, YYN, ZZN,
     .   D13, XXC, YYC, ZZC, FSX, FSY, FSZ, FN, FSNX, FSNY, FSNZ, FSTX,
     .   FSTY, FSTZ, DMX, DMY, DMZ,
     .   AL4,AL5,AL6
      DATA POWER2/1,2,4,8,16,32,64,128/
      DATA IPERM/1,2,3,5,6,7/
      my_real, DIMENSION(:,:), ALLOCATABLE :: FSTPARIT
C---------------------------------------------------------
#include "sync_dir.inc"
C---------------------------------------------------------
       IF(NSEG==0)RETURN
       IF(LFT+NFT>NSTRF(1,NSEG))RETURN
       IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
       NSA=0
C
      DO I=1,8
        DO J=0,255
          UNPACK(J,I)=MOD(J/POWER2(I),2)
        ENDDO
      ENDDO
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       DO I=1,16
         FST(I)=ZERO
       ENDDO
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
       IF(NSPMD==1) THEN
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
       IF(IPARSENS==0) THEN    ! Parith/Off
        DO K=1,NSA
         I1=IPERM(II)
         DO II=1,4
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=-FX(I,II)
             FY1(K)=-FY(I,II)
             FZ1(K)=-FZ(I,II)
C
             N = IXS(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
        ENDDO
       ELSE                ! Parith/On
        DO K=1,NSA
         I1=IPERM(II)
         DO II=1,4
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=-FX(I,II)
             FY1(K)=-FY(I,II)
             FZ1(K)=-FZ(I,II)
C
             N = IXS(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
        ENDDO  
C
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
       DEALLOCATE(FSTPARIT)
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
        DO II=1,4
         I1=IPERM(II)
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXS(I1+1,NSTRF(1,J))
             SECFCUM(1,N)=SECFCUM(1,N)- FX(I,II)
             SECFCUM(2,N)=SECFCUM(2,N)- FY(I,II)
             SECFCUM(3,N)=SECFCUM(3,N)- FZ(I,II)
           ENDIF
         ENDDO
        ENDDO
#include "lockoff.inc"
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SECTIOS4                      source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOS4 (LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,FSAV,
     3                   IXS,FOPTA,SECFCUM,FX,FY,
     4                   FZ,TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     6                   IXS10,ISOLNOD,XSEC,FBSAV6,
     7                   IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,TYPE,NSINT
      INTEGER NSTRF(2,*),IXS(NIXS,*),IFRAM,NNOD,NOD(*),
     .        IXS10(6,*),ISOLNOD
      INTEGER IPARSENS
      my_real 
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),XSEC(4,3),
     .   FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10), V(3,*) ,MS(*)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(0:511,10),
     .   NSA, J, I, K, I1, I2, IPACK, N,POWER2(14),IPERM(4),II,JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   DX1(MVSIZ),DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
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
     .  ZZ1PHI,ZZ3PHI,ZZ4PHI,ZZ5PHI,ZZ6PHI
      my_real 
     .  AL4PHI,AL5PHI,AL6PHI,XX5T,YY5T,ZZ5T
      my_real 
     .  AL4,AL5,AL6
      DATA POWER2/1,2,4,8,16,32,64,128,
     .            256,512,1024,2048,4096,8192/
      DATA IPERM/1,3,6,5/
      my_real, DIMENSION(:,:), ALLOCATABLE :: FSTPARIT
C---------------------------------------------------------
#include "sync_dir.inc"
C---------------------------------------------------------
       IF(NSEG==0)RETURN
       IF(LFT+NFT>NSTRF(1,NSEG))RETURN
       IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
       NSA=0
C
      DO I=1,8
        DO J=0,255
          UNPACK(J,I)=MOD(J/POWER2(I),2)
        ENDDO
      ENDDO
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
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
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
       IF(IPARSENS==0) THEN      ! Parith/Off
        DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = MOD(NSTRF(2,J),256)
         DO II=1,4
           I1=IPERM(II)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=-FX(I,II)
             FY1(K)=-FY(I,II)
             FZ1(K)=-FZ(I,II)
C
             N = IXS(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
        ENDDO
       ELSE       ! PArith/on
        DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = MOD(NSTRF(2,J),256)
         DO II=1,4
           I1=IPERM(II)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=-FX(I,II)
             FY1(K)=-FY(I,II)
             FZ1(K)=-FZ(I,II)
C
             N = IXS(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
        ENDDO
       ENDIF  
C
       IF(ISOLNOD==10)THEN
        IF(IPARSENS==0) THEN     ! Parith/Off
        DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO II=5,10
           I1=II+4
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             FX1(K)=-FX(I,II)
             FY1(K)=-FY(I,II)
             FZ1(K)=-FZ(I,II)
C
             N = IXS10(I1-8,NSTRF(1,J)-NUMELS8)
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
        ENDDO
        ELSE      ! Parith/On
         DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO II=5,10
           I1=II+4
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             FX1(K)=-FX(I,II)
             FY1(K)=-FY(I,II)
             FZ1(K)=-FZ(I,II)
C
             N = IXS10(I1-8,NSTRF(1,J)-NUMELS8)
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
        ENDDO
        ENDIF
       END IF
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
       IF(IPARSENS/=0) THEN      ! Parith/On
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
        DEALLOCATE(FSTPARIT)
       ENDIF

      ELSE
C
#include "lockon.inc"
        DO II=1,4
         I1=IPERM(II)
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = MOD(NSTRF(2,J),256)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXS(I1+1,NSTRF(1,J))
             SECFCUM(1,N)=SECFCUM(1,N)- FX(I,II)
             SECFCUM(2,N)=SECFCUM(2,N)- FY(I,II)
             SECFCUM(3,N)=SECFCUM(3,N)- FZ(I,II)
           ENDIF
         ENDDO
        ENDDO
        IF(ISOLNOD==10)THEN
         DO II=5,10
          I1=II+4
          DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             N = IXS10(I1-8,NSTRF(1,J)-NUMELS8)
             SECFCUM(1,N)=SECFCUM(1,N)- FX(I,II)
             SECFCUM(2,N)=SECFCUM(2,N)- FY(I,II)
             SECFCUM(3,N)=SECFCUM(3,N)- FZ(I,II)
           ENDIF
          ENDDO
         ENDDO
        END IF
#include "lockoff.inc"
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  SECTIOS                       source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOS (LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,FSAV,
     3                   IXS,FOPTA,SECFCUM,FX,FY,FZ,
     4                   TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     6                   IXS20,IXS16,ISOLNOD,XSEC,FBSAV6,
     7                   IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,TYPE,NSINT
      INTEGER NSTRF(2,*),IXS(NIXS,*),IFRAM,NNOD,NOD(*),
     .        IXS20(12,*),IXS16(8,*),ISOLNOD
      INTEGER IPARSENS
      my_real 
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),
     .   FX(MVSIZ,8), FY(MVSIZ,8), FZ(MVSIZ,8), V(3,*) ,MS(*),XSEC(4,3)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(0:511,10),
     .   NSA, J, I, K, I1, I2, IPACK, N,POWER2(20), JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   DX1(MVSIZ),DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
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
     .    ZZ1PHI,ZZ3PHI,ZZ4PHI,ZZ5PHI,ZZ6PHI
      my_real
     .    AL4PHI,AL5PHI,AL6PHI,XX5T,YY5T,ZZ5T
      my_real 
     .  AL4,AL5,AL6
      my_real, DIMENSION(:,:), ALLOCATABLE :: FSTPARIT
      DATA POWER2/1,2,4,8,16,32,64,128,256,512,
     .            1024,2048,4096,8192,16384,
     .            32768,65536,131072,262144,524288/
C---------------------------------------------------------
#include "sync_dir.inc"
C---------------------------------------------------------
       IF(NSEG==0)RETURN
       IF(LFT+NFT>NSTRF(1,NSEG))RETURN
       IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
       NSA=0
C
      DO I=1,8
        DO J=0,255
          UNPACK(J,I)=MOD(J/POWER2(I),2)
        ENDDO
      ENDDO
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       DO I=1,16
         FST(I)=ZERO
       ENDDO
C
       IF(IPARSENS==1) THEN
         ALLOCATE(FSTPARIT(12,NSA))
         DO J=1,NSA
           DO I=1,12
            FSTPARIT(I,J) = ZERO
           ENDDO
         ENDDO
       ENDIF
C
       IF(NSPMD==1) THEN
         CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2     XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3     XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4, ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6, ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1 , XSEC)
       END IF
C
       IF(IPARSENS==0) THEN     ! Parith/Off
       DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = MOD(NSTRF(2,J),256)
         DO I1=1,8
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=-FX(I,I1)
             FY1(K)=-FY(I,I1)
             FZ1(K)=-FZ(I,I1)
C
             N = IXS(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
       ENDDO
       ELSE               ! Parith/on
        DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = MOD(NSTRF(2,J),256)
         DO I1=1,8
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=-FX(I,I1)
             FY1(K)=-FY(I,I1)
             FZ1(K)=-FZ(I,I1)
C
             N = IXS(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
       ENDDO
      ENDIF
C
       IF(ISOLNOD==20)THEN
C       bricks 20
        IF(IPARSENS==0) THEN     ! Parith/Off
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           DO I1=9,20
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             FX1(K)=-FX(I,I1)
             FY1(K)=-FY(I,I1)
             FZ1(K)=-FZ(I,I1)
C
             N = IXS20(I1-8,NSTRF(1,J)-NUMELS8-NUMELS10)
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
       ENDDO
       ELSE      ! Parith/on
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           DO I1=9,20
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             FX1(K)=-FX(I,I1)
             FY1(K)=-FY(I,I1)
             FZ1(K)=-FZ(I,I1)
C
             N = IXS20(I1-8,NSTRF(1,J)-NUMELS8-NUMELS10)
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
       ENDDO
       ENDIF
       ELSE IF(ISOLNOD==16)THEN
C       shells 16
        IF(IPARSENS==0) THEN     ! Parith/Off
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           DO I1=9,16
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             FX1(K)=-FX(I,I1)
             FY1(K)=-FY(I,I1)
             FZ1(K)=-FZ(I,I1)
C
             N = IXS16(I1-8,NSTRF(1,J)-NUMELS8-NUMELS10-NUMELS20)
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
        ENDDO
        ELSE            ! Parith/on
        DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           DO I1=9,16
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             FX1(K)=-FX(I,I1)
             FY1(K)=-FY(I,I1)
             FZ1(K)=-FZ(I,I1)
C
             N = IXS16(I1-8,NSTRF(1,J)-NUMELS8-NUMELS10-NUMELS20)
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
        ENDDO
        ENDIF
       END IF
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
       IF(IPARSENS/=0) THEN      ! Parith/On
          CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
        DEALLOCATE(FSTPARIT)
       ENDIF
          
      ELSE
C
#include "lockon.inc"
       DO I1=1,8
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = MOD(NSTRF(2,J),256)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXS(I1+1,NSTRF(1,J))
             SECFCUM(1,N)=SECFCUM(1,N)-FX(I,I1)
             SECFCUM(2,N)=SECFCUM(2,N)-FY(I,I1)
             SECFCUM(3,N)=SECFCUM(3,N)-FZ(I,I1)
           ENDIF
         ENDDO
       ENDDO
       IF(ISOLNOD==20)THEN
C       bricks 20
        DO I1=9,20
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             N = IXS20(I1-8,NSTRF(1,J)-NUMELS8-NUMELS10)
             SECFCUM(1,N)=SECFCUM(1,N)-FX(I,I1)
             SECFCUM(2,N)=SECFCUM(2,N)-FY(I,I1)
             SECFCUM(3,N)=SECFCUM(3,N)-FZ(I,I1)
           ENDIF
         ENDDO
       ENDDO
       ELSE IF(ISOLNOD==16)THEN
C       shells 16
        DO I1=9,16
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(MOD(IPACK/POWER2(I1),2)/=0)THEN
             N = IXS16(I1-8,NSTRF(1,J)-NUMELS8-NUMELS10-NUMELS20)
             SECFCUM(1,N)=SECFCUM(1,N)-FX(I,I1)
             SECFCUM(2,N)=SECFCUM(2,N)-FY(I,I1)
             SECFCUM(3,N)=SECFCUM(3,N)-FZ(I,I1)
           ENDIF
         ENDDO
        ENDDO
       END IF
#include "lockoff.inc"
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  SECTIOC                       source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOC(LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,VR,FSAV,
     3                   IXC,FOPTA,SECFCUM,
     4                   FX,FY,FZ,MX,MY,MZ,
     5                   TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     7                   XSEC,FSAVSAV,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,
     .        TYPE,NSINT,IPARSENS
      INTEGER NSTRF(2,*),IXC(NIXC,*),IFRAM,NNOD,NOD(*)
      my_real 
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*), 
     .   V(3,*), VR(3,*),MS(*),XSEC(4,3), FSAVSAV(NTHVKI)
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
#include "sync_dir.inc"
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
#include "scalar.inc"
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
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
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
#include "scalar.inc"
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
#include "scalar.inc"
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
              IF(ISHSUB/=0)THEN
                FSAVSAV(1)=FSAVSAV(1)+FST(1)
                FSAVSAV(2)=FSAVSAV(2)+FST(2)
                FSAVSAV(3)=FSAVSAV(3)+FST(3)
                FSAVSAV(4)=FSAVSAV(4)+FST(4)
                FSAVSAV(5)=FSAVSAV(5)+FST(5)
                FSAVSAV(6)=FSAVSAV(6)+FST(6)
                FSAVSAV(7)=FSAVSAV(7)+FST(7)
                FSAVSAV(8)=FSAVSAV(8)+FST(8)
                FSAVSAV(9)=FSAVSAV(9)+FST(9)
              END IF
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

Chd|====================================================================
Chd|  SECTIOT                       source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOT (LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,VR,FSAV,
     3                   IXT, FOPTA,SECFCUM,
     4                   FX,FY,FZ,TYPE,NSINT,IFRAM,
     5                   NNOD,NOD,MS,
     6                   XSEC,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,
     4                   TYPE,NSINT
      INTEGER NSTRF(2,*),IXT(NIXT,*),IFRAM,NNOD,NOD(*)
      INTEGER IPARSENS
      my_real 
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),
     .   FX(MVSIZ,2), FY(MVSIZ,2), FZ(MVSIZ,2),
     .        V(3,*), VR(3,*),MS(*),XSEC(4,3)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(3,2),
     .   NSA, J, I, K, I1, I2, IPACK, N, JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   DX1(MVSIZ),
     .   DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
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
      DATA UNPACK/1,0,1,
     .            0,1,1/
C
      IF(NSEG==0)RETURN
      IF(LFT+NFT>NSTRF(1,NSEG))RETURN
      IF(LLT+NFT<NSTRF(1,1   ))RETURN
C--------------------------------------------------------
      NSA=0
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       DO I=1,16
         FST(I)=ZERO
       ENDDO
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
       IF(NSPMD==1) THEN
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
       IF(IPARSENS==0) THEN      ! Parith/Off	  
        DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,2
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             N = IXT(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
C
           ENDIF
         ENDDO
        ENDDO
       ELSE         ! Parith/on
         DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,2
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             N = IXT(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
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
     .        +FX1(K)*V(1,N)+FY1(K)*V(2,N)+FZ1(K)*V(3,N)
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
           ENDIF
         ENDDO
        ENDDO
C
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
        DEALLOCATE(FSTPARIT)
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
       DO I1 = 1,2
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXT(I1+1,NSTRF(1,J))
             SECFCUM(1,N)=SECFCUM(1,N)+FX(I,I1)
             SECFCUM(2,N)=SECFCUM(2,N)+FY(I,I1)
             SECFCUM(3,N)=SECFCUM(3,N)+FZ(I,I1)
           ENDIF
         ENDDO
       ENDDO
#include "lockoff.inc"
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  SECTIOP                       source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOP (LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,VR,FSAV,
     3                   IXP, FOPTA,SECFCUM,
     4                   FX,FY,FZ,MX,MY,MZ,
     5                   TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     7                   XSEC,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,
     4                   TYPE,NSINT
      INTEGER NSTRF(2,*),IXP(NIXP,*),IFRAM,NNOD,NOD(*)
      INTEGER IPARSENS
      my_real
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),
     .   FX(MVSIZ,2), FY(MVSIZ,2), FZ(MVSIZ,2), MX(MVSIZ,2),
     .   MY(MVSIZ,2), MZ(MVSIZ,2),
     .        V(3,*), VR(3,*),MS(*),XSEC(4,3)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(3,2),
     .   NSA, J, I, K, I1, I2, IPACK, N, JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   MX1(MVSIZ), MY1(MVSIZ), MZ1(MVSIZ), DX1(MVSIZ),
     .   DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
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
      DATA UNPACK/1,0,1,
     .            0,1,1/
C
      IF(NSEG==0)RETURN
      IF(LFT+NFT>NSTRF(1,NSEG))RETURN
      IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
      NSA=0
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       DO I=1,16
         FST(I)=ZERO
       ENDDO
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
       IF(NSPMD==1) THEN
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
       IF(IPARSENS==0) THEN      ! Parith/Off
       DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,2
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXP(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
     .        +FX1(K)*V(1,N) +FY1(K)*V(2,N) +FZ1(K)*V(3,N)
     .        +MX1(K)*VR(1,N)+MY1(K)*VR(2,N)+MZ1(K)*VR(3,N)
C
           ENDIF
         ENDDO
       ENDDO
       ELSE             ! Parith/on
       DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,2
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXP(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
     .        +FX1(K)*V(1,N) +FY1(K)*V(2,N) +FZ1(K)*V(3,N)
     .        +MX1(K)*VR(1,N)+MY1(K)*VR(2,N)+MZ1(K)*VR(3,N)
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
           ENDIF
         ENDDO
       ENDDO
C
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
        DEALLOCATE(FSTPARIT)
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
       DO I1 = 1,2
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXP(I1+1,NSTRF(1,J))
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
      RETURN
      END

Chd|====================================================================
Chd|  SECTIOR                       source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIOR (LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,VR,FSAV,
     3                   IXR, FOPTA,SECFCUM,
     4                   FX,FY,FZ,MX,MY,MZ,
     5                   TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     7                   XSEC,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,
     4                   TYPE,NSINT
      INTEGER NSTRF(2,*),IXR(NIXR,*),IFRAM,NNOD,NOD(*)
      INTEGER IPARSENS
      my_real
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),
     .   FX(MVSIZ,2), FY(MVSIZ,2), FZ(MVSIZ,2), MX(MVSIZ,2),
     .   MY(MVSIZ,2), MZ(MVSIZ,2),
     .        V(3,*), VR(3,*),MS(*),XSEC(4,3)
       DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(3,2),
     .   NSA, J, I, K, I1, I2, IPACK, N, JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   MX1(MVSIZ), MY1(MVSIZ), MZ1(MVSIZ), DX1(MVSIZ),
     .   DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
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
      DATA UNPACK/1,0,1,
     .            0,1,1/
C
      IF(NSEG==0)RETURN
      IF(LFT+NFT>NSTRF(1,NSEG))RETURN
      IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
      NSA=0
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       DO I=1,16
         FST(I)=0.
       ENDDO
C
       IF(IPARSENS/=0) THEN    
         ALLOCATE(FSTPARIT(12,NSA))
         DO J=1,NSA
             DO I=1,12
              FSTPARIT(I,J)=ZERO
             ENDDO
         ENDDO
       ENDIF

C
       IF(NSPMD==1) THEN
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
       IF(IPARSENS==0) THEN     ! Parith/Off
       DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,2
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXR(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
     .        +FX1(K)*V(1,N) +FY1(K)*V(2,N) +FZ1(K)*V(3,N)
     .        +MX1(K)*VR(1,N)+MY1(K)*VR(2,N)+MZ1(K)*VR(3,N)
C
           ENDIF
         ENDDO
       ENDDO
       ELSE            ! Parith/On
       DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,2
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXR(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
     .        +FX1(K)*V(1,N) +FY1(K)*V(2,N) +FZ1(K)*V(3,N)
     .        +MX1(K)*VR(1,N)+MY1(K)*VR(2,N)+MZ1(K)*VR(3,N)
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
           ENDIF
         ENDDO
       ENDDO
C
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
        DEALLOCATE(FSTPARIT)
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
       DO I1 = 1,2
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXR(I1+1,NSTRF(1,J))
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
      RETURN
      END

Chd|====================================================================
Chd|  SECTIO3N                      source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|        SUM_6_FLOAT_SECT              source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SECTIO3N (LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,V,VR,FSAV,
     3                   IXTG, FOPTA,SECFCUM,
     4                   FX,FY,FZ,MX,MY,MZ,
     5                   TYPE,NSINT,IFRAM,NNOD,NOD,MS,
     7                   XSEC,FSAVSAV,FBSAV6,IPARSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3,
     4                   TYPE,NSINT,IPARSENS
      INTEGER NSTRF(2,*),IXTG(NIXTG,*),IFRAM,NNOD,NOD(*)
      my_real
     .   X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*),
     .   FX(MVSIZ,3), FY(MVSIZ,3), FZ(MVSIZ,3), MX(MVSIZ,3),
     .   MY(MVSIZ,3), MZ(MVSIZ,3),
     .        V(3,*), VR(3,*),MS(*),XSEC(4,3), FSAVSAV(NTHVKI)
      DOUBLE PRECISION FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(0:7,3),
     .   NSA, J, I, K, I1, I2, IPACK, N, JJ
      my_real
     .   FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   MX1(MVSIZ), MY1(MVSIZ), MZ1(MVSIZ), DX1(MVSIZ),
     .   DY1(MVSIZ), DZ1(MVSIZ),FST(16), 
     .   MSX, MSY, MSZ,
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
      DATA UNPACK/0,1,0,1,0,1,0,1,
     .            0,0,1,1,0,0,1,1,
     .            0,0,0,0,1,1,1,1/
C
      IF(NSEG==0)RETURN
      IF(LFT+NFT>NSTRF(1,NSEG))RETURN
      IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
      NSA=0
C
      DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20  CONTINUE
  30  CONTINUE
C
      IF(NSA==0)RETURN
C
      IF(TYPE+NSINT==0)THEN
C
       DO I=1,16
         FST(I)=ZERO
       ENDDO
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
       IF(NSPMD==1) THEN
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
      IF(IPARSENS==0) THEN    ! Parith/Off
       DO K=1,NSA
        J  = JJJ(K)
        I  = NSTRF(1,J)-NFT
        IPACK = NSTRF(2,J)
         DO I1 = 1,3
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXTG(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
     .        +FX1(K)*V(1,N) +FY1(K)*V(2,N) +FZ1(K)*V(3,N)
     .        +MX1(K)*VR(1,N)+MY1(K)*VR(2,N)+MZ1(K)*VR(3,N)
C
           ENDIF
         ENDDO
       ENDDO
      ELSE        ! Parith/on
       DO K=1,NSA
         J  = JJJ(K)
         I  = NSTRF(1,J)-NFT
         IPACK = NSTRF(2,J)
         DO I1 = 1,3
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXTG(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
     .        +FX1(K)*V(1,N) +FY1(K)*V(2,N) +FZ1(K)*V(3,N)
     .        +MX1(K)*VR(1,N)+MY1(K)*VR(2,N)+MZ1(K)*VR(3,N)
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
           ENDIF
         ENDDO
       ENDDO
C
        CALL SUM_6_FLOAT_SECT(FSTPARIT,12,NSA,1,NSA,FBSAV6,12,6)
C
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
              IF(ISHSUB/=0)THEN
                FSAVSAV(1)=FSAVSAV(1)+FST(1)
                FSAVSAV(2)=FSAVSAV(2)+FST(2)
                FSAVSAV(3)=FSAVSAV(3)+FST(3)
                FSAVSAV(4)=FSAVSAV(4)+FST(4)
                FSAVSAV(5)=FSAVSAV(5)+FST(5)
                FSAVSAV(6)=FSAVSAV(6)+FST(6)
                FSAVSAV(7)=FSAVSAV(7)+FST(7)
                FSAVSAV(8)=FSAVSAV(8)+FST(8)
                FSAVSAV(9)=FSAVSAV(9)+FST(9)
              END IF
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
       DO I1 = 1,3
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             N = IXTG(I1+1,NSTRF(1,J))
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

Chd|====================================================================
Chd|  SECT_INI                      source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        READ_R_C                      source/output/tools/sortie_c.c
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SECT_INI (NSTRF,SECBUF,NOM_SECT,ISECTR,NSECT,IOLDSECT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "chara_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*),NOM_SECT(*),ISECTR,NSECT,IOLDSECT
      my_real 
     .   SECBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IR, N, IR1, IR2, IFILNAM(2548),LROOTLEN,LEN,K0,ID_SEC
      my_real 
     .   TT1, TT2, TT3, TMP(20)
      CHARACTER FILNAM*500,LCHRUN*2,LCHRUN_P1*2,CH_IDSEC*10
      LOGICAL FEXIST
      REAL*4 R4
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME
C-----------------------------------------------
      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IR1=NSTRF(4)
      IR2=NSTRF(5)
C-----------------------------------------------
C WRITE FILE
C-----------------------------------------------
      IF (ISPMD==0) THEN
        K0 = NSTRF(25)
        DO J=1,NSECT
         IF(NSTRF(K0)>=1 .AND. NSTRF(K0)<=10 )THEN
          WRITE(LCHRUN,'(I2.2)')IRUN 
          LROOTLEN=0 
          DO I=1,500
c ncharline of starter =500
            FILNAM(I:I)=CHAR(NOM_SECT((J-1)*500+I))
            IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
          ENDDO  
          IF (LROOTLEN == 0 .AND. ABS(IOLDSECT) == 1) THEN
            IOLDSECT = 1
          ELSEIF( LROOTLEN /= 0 .AND. (IOLDSECT >= 1)) THEN 
            IOLDSECT = 2
          ENDIF
         ENDIF
         K0  = NSTRF(K0+24)
        ENDDO
        IF(NSTRF(1)>=1 .AND. IOLDSECT == 1)THEN
Csm  FILNAM=ROOTNAM(1:ROOTLEN)//'SC'//CHRUN
          WRITE(LCHRUN,'(I2.2)')IRUN      
          FILNAM=ROOTNAM(1:ROOTLEN)//'SC'//LCHRUN
          LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 4      
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+4)
          DO I=1,LEN_TMP_NAME
            IFILNAM(I)=ICHAR(TMP_NAME(I:I))
          ENDDO
          CALL CUR_FIL_C(42)
          CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
        ELSEIF(NSTRF(1)>=1) THEN
          K0 = NSTRF(25)
          DO J=1,NSECT
           IF(NSTRF(K0)>=1 .AND. NSTRF(K0)<=10 )THEN
            WRITE(LCHRUN,'(I2.2)')IRUN 
            LROOTLEN=0 
            DO I=1,500
c ncharline of starter =500
              IF(CHAR(NOM_SECT((J-1)*500+I))/=' ')THEN
                LROOTLEN=LROOTLEN+1
                FILNAM(LROOTLEN:LROOTLEN)=CHAR(NOM_SECT((J-1)*500+I))
              ENDIF
            ENDDO 
            IF (LROOTLEN == 0) THEN 
              WRITE(CH_IDSEC,'(I10.10)')NSTRF(K0+23)     
              FILNAM=ROOTNAM(1:ROOTLEN)//CH_IDSEC//'SC'//LCHRUN
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 14      
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+14)
              DO I=1,LEN_TMP_NAME
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              CALL CUR_FIL_C(41+J)
              CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
            ELSE 
              FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
              LEN_TMP_NAME = OUTFILE_NAME_LEN + LROOTLEN + 4      
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)
              DO I=1,LEN_TMP_NAME
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              CALL CUR_FIL_C(41+J)
              CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
            ENDIF
           ENDIF
          K0  = NSTRF(K0+24)
          ENDDO
        ENDIF
C-----------------------------------------------
C READ FILES 
C-----------------------------------------------
       IF(NSTRF(2)>=1)THEN
        LROOTLEN=0
        DO I=1,500
c ncharline of starter =500
          FILNAM(I:I)=CHAR(NOM_SECT((ISECTR-1)*500+I))
          IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
        ENDDO 
        IF(TT==ZERO)THEN
          TT1=ZERO
          TT2=ZERO
          FILNAM=FILNAM(1:LROOTLEN)//'SC01'
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LROOTLEN + 4      
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)
          INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME = LROOTLEN + 4      
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LROOTLEN+4)
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO
            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT1=R4
            IR1=1
            IR2=1
          ELSE
            CALL ANCMSG(MSGID=188,ANMODE=ANINFO,
     .                  C1=FILNAM)
            IERR=IERR+1
            TSTOP = TT
          ENDIF
          FILNAM=FILNAM(1:LROOTLEN)//'SC02'
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LROOTLEN + 4      
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)

          INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME = LROOTLEN + 4     
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LROOTLEN+4)
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            IR2=2
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME!LROOTLEN+4
                  IFILNAM(I)=ICHAR(FILNAM(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT3=R4
          ELSE
            TT3=EP30
          ENDIF
        ELSE
          TT1=ZERO
          TT2=ZERO
          WRITE(LCHRUN,'(I2.2)')IRUN
          FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)  
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

          INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME = LEN_TRIM(FILNAM)    
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TRIM(FILNAM))
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT1=R4
            IR1=IRUN
            IR2=IRUN
          ELSE
            CALL ANCMSG(MSGID=188,ANMODE=ANINFO,
     .                  C1=FILNAM)
            IERR=IERR+1
            TSTOP = TT
          ENDIF
          WRITE(LCHRUN_P1,'(I2.2)')IRUN+1
          FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN_P1
          LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)  
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

          INQUIRE(FILE=TMP_NAME,EXIST=FEXIST)

          IF(.NOT.FEXIST) THEN
              LEN_TMP_NAME =  LEN_TRIM(FILNAM)   
              TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TMP_NAME)
              INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
          ENDIF

          IF(FEXIST)THEN
            IR2=IRUN + 1
            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
                  IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
            CALL READ_R_C(R4,1)
            CALL CLOSE_C()
            TT3=R4
          ELSE
            TT3=EP30
          ENDIF
        ENDIF
C
        WRITE(LCHRUN,'(I2.2)')IR1
        FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
        LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)  
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

        CALL CUR_FIL_C(4)
        DO I=1,LEN_TMP_NAME
            IFILNAM(I)=ICHAR(TMP_NAME(I:I))
        ENDDO

        CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
       ENDIF
      ENDIF
C-----------------------------------------------
      SECBUF(2) = TT1
      SECBUF(3) = TT2
      SECBUF(4) = TT3
C
      NSTRF(4) = IR1
      NSTRF(5) = IR2
C
C MAJ SPMD sur proc remote
C
      IF (NSPMD > 1) THEN
       IF(ISPMD==0) THEN
         TMP(1) = NSTRF(4)
         TMP(2) = NSTRF(5)
         TMP(3) = SECBUF(2)
         TMP(4) = SECBUF(3)
         TMP(5) = SECBUF(4)
         LEN = 5
Cel 2*LEN necessaire pour communication !!!
         CALL SPMD_RBCAST(TMP,TMP,LEN,1,0,2)
       ELSE
         LEN = 5
Cel 2*LEN necessaire pour communication !!!
         CALL SPMD_RBCAST(TMP,TMP,LEN,1,0,2)
         NSTRF(4) = TMP(1)
         NSTRF(5) = TMP(2)
         SECBUF(2)= TMP(3)
         SECBUF(3)= TMP(4)
         SECBUF(4)= TMP(5)
       ENDIF
      END IF
C
      RETURN
      END

Chd|====================================================================
Chd|  SECT_IO                       source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        FLU_FIL_C                     source/output/tools/sortie_c.c
Chd|        SECTIO                        source/tools/sect/sectio.F    
Chd|        SECT_READ                     source/tools/sect/sectio.F    
Chd|        SECT_READP                    source/tools/sect/sectio.F    
Chd|        SPMD_EXCH_CUT                 source/mpi/sections/spmd_section.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_WRT_CUTD                 source/mpi/sections/spmd_section.F
Chd|        SPMD_WRT_CUTF                 source/mpi/sections/spmd_section.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE SECT_IO (
     1    NSTRF  ,D      ,DR    ,V     ,VR      ,FSAV   ,
     2    SECFCUM,A      ,AR    ,SECBUF,MS      ,IN     ,
     3    X      ,FANI   ,WEIGHT,XSEC  ,IAD_ELEM,FR_ELEM,
     4    RG_CUT ,IAD_CUT,FR_CUT,WEIGHT_MD      ,IOLDSECT,
     5    STABSEN,DIMFB  ,TABS   ,FBSAV6  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "chara_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*), WEIGHT(*), IAD_ELEM(2,*), FR_ELEM(*),
     .   RG_CUT(*), IAD_CUT(NSPMD+2,*), FR_CUT(*),WEIGHT_MD(*),
     .   IOLDSECT, STABSEN,DIMFB,TABS(STABSEN)
      my_real 
     .   D(3,*), DR(3,*), V(3,*), VR(3,*), A(3,*), AR(3,*), MS(*),  
     .   FSAV(NTHVKI,*), SECFCUM(7,NUMNOD,*), SECBUF(*), IN(*),
     .   FANI(3,*), X(3,*), XSEC(4,3,*)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,
     .   IFRL1, IFRL2, L, ID_SEC,TYPE, LREC, NNOD,KR11,KR12, LENR,
     .   KR21,KR22,NBINTER, NN, LEN, KC, NSIZE, NNODG, SIZE, NNODT,
     .   ISECT
      my_real
     .   DW, TT1, TT2, TT3, VI, DD, D1, D2,TFEXTL,
     .   AOLD, TNEXT, DELTAT,ERR(8), FF, FOLD, ALPHA, AA, DV, WA(10)
      REAL*4 R4
C-----------------------------------------------
C
C---------------------------------------------------------
      IF(NSECT==0)RETURN
C-----------------------------------------------
C BILAN SUR SECTION AVEC SECFCUM
C---------------------------------------------------------
      K0=NSTRF(25)
      DO I=1,NSECT
         IF(NSTRF(K0)+NSTRF(K0+14)>0)THEN
C
C traitement des noeuds frontieres : 
C cumul de SECFCUM sur proc main puis mise a zero ailleurs
C
           K2 = K0 + 30 + NSTRF(K0+14)
           IF(IRODDL/=0)THEN
             SIZE = 6
           ELSE
             SIZE = 3
           END IF
           IF (NSPMD > 1) THEN
             LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
             CALL SPMD_EXCH_CUT(
     1         NSTRF(K2),SECFCUM(1,1,I),IAD_ELEM,FR_ELEM,SIZE,
     2         LENR     ,NSTRF(K0+6),WEIGHT)
           END IF
C
           K2 = K0 + 30 + NSTRF(K0+14)
           CALL SECTIO(
     1     NSTRF(K0+6),NSTRF(K0+3),NSTRF(K0+4),NSTRF(K0+5),NSTRF(K2),X,
     2     V       ,VR  ,FSAV(1,I),FANI(1,1+2*(I-1)),SECFCUM(1,1,I),MS,
     3     IN      ,NSTRF(K0),NSTRF(K0+26),XSEC(1,1,I) )
         ENDIF
         K0=NSTRF(K0+24)
      ENDDO
      IF(NSTRF(1)==0.AND.NSTRF(2)==0)RETURN
C-----------------------------------------------
C WRITE FILE
C-----------------------------------------------
      TNEXT  = SECBUF(5)
      DELTAT = SECBUF(1)
      LREC   = NSTRF(6)
      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IF(NSTRF(1)>=1.AND.TNEXT<=TT)THEN
        SECBUF(5) = TNEXT + DELTAT
C
        K0  = NSTRF(25)
C
        KC = 1
        IF(ISPMD==0 .AND. IOLDSECT == 1) THEN
          CALL CUR_FIL_C(42)
          R4 = TT
          CALL WRITE_R_C(R4,1)
          CALL WRITE_I_C(LREC,1)
          CALL WRITE_I_C(NSTRF(1),1)
        ENDIF
        DO N=1,NSECT
          TYPE=NSTRF(K0)
          IF(ISPMD==0 .AND. IOLDSECT /= 1 .AND. TYPE >= 1) THEN
            CALL CUR_FIL_C(41+N)
            R4 = TT
            CALL WRITE_R_C(R4,1)
            CALL WRITE_I_C(1,1)
            CALL WRITE_I_C(1,1)
          ENDIF
          NBINTER = NSTRF(K0+14)
          K1 = K0+30
          K2=K1+NBINTER
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          IF(TYPE>=1)THEN
C ecriture deplacements
            ID_SEC=NSTRF(K0+23)
            IF(NSPMD==1) THEN
              CALL WRITE_I_C(ID_SEC,1)
              CALL WRITE_I_C(TYPE,1)
              CALL WRITE_I_C(NNOD,1)
            ELSEIF(ISPMD==0) THEN
              CALL WRITE_I_C(ID_SEC,1)
              CALL WRITE_I_C(TYPE,1)
              NNODG = IAD_CUT(NSPMD+2,N)
              CALL WRITE_I_C(NNODG,1)
            ENDIF
            IF(IRODDL/=0)THEN
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTD(
     1            NNOD        ,NSTRF(K2),D    ,DR    ,RG_CUT(KC),
     2            IAD_CUT(1,N),NSIZE    ,NNODG,WEIGHT,2         )
              ELSE
               DO I=1,NNOD
                R4 = D(1,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(2,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(3,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = DR(1,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = DR(2,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = DR(3,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ELSE 
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTD(
     1            NNOD        ,NSTRF(K2),D    ,DR    ,RG_CUT(KC),
     2            IAD_CUT(1,N),NSIZE    ,NNODG,WEIGHT,1         )
              ELSE
               DO I=1,NNOD
                R4 = D(1,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(2,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = D(3,NSTRF(K2+I-1))
                CALL WRITE_R_C(R4,1)
                R4 = ZERO
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ENDIF
          ENDIF
          IF(TYPE>=2)THEN
C ecriture forces
            IF(IRODDL/=0)THEN
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTF(
     1           NNOD ,NSTRF(K2),SECFCUM(1,1,N),RG_CUT(KC),IAD_CUT(1,N),
     2           NSIZE,NNODG    ,WEIGHT        ,2         )
              ELSE
               DO I=1,NNOD
                R4 = SECFCUM(1,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(2,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(3,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(5,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(6,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(7,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ELSE
C-----------------------------------------------------
C Comm SPMD + Ecriture
C-----------------------------------------------------
              IF(NSPMD>1) THEN
               IF (ISPMD==0) THEN
                NSIZE = IAD_CUT(NSPMD+1,N)
                NNODG = IAD_CUT(NSPMD+2,N)
               ELSE
                NSIZE = 0
                NNODG = 0
               ENDIF
               CALL SPMD_WRT_CUTF(
     1           NNOD ,NSTRF(K2),SECFCUM(1,1,N),RG_CUT(KC),IAD_CUT(1,N),
     2           NSIZE,NNODG    ,WEIGHT        ,1         )
              ELSE
               DO I=1,NNOD
                R4 = SECFCUM(1,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(2,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = SECFCUM(3,NSTRF(K2+I-1),N)
                CALL WRITE_R_C(R4,1)
                R4 = ZERO
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
                CALL WRITE_R_C(R4,1)
               ENDDO
              ENDIF
            ENDIF            
          ENDIF

          K0  = NSTRF(K0+24)
          IF(TYPE>=1) KC = KC + NNOD
        ENDDO
        IF(ISPMD==0) CALL FLU_FIL_C
      ENDIF
C-----------------------------------------------
C FORCES ERROR 
C T = TT
C-----------------------------------------------
      IF(NSTRF(2)>=1)THEN
C-----------------------------------------------
C Calcul erreur locale cummulee dans FSAVE
C-----------------------------------------------
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        K0  = NSTRF(25)
        KR0 = NSTRF(26)
        DO N=1,NSECT
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          NBINTER = NSTRF(K0+14)
          IF(TYPE>=101)THEN
            K2 = K0 + 30 + NBINTER
            KR1 = KR0 + 10
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            KR21 = KR2 + IFRL2*6*NNOD
            KR22 = KR2 + IFRL1*6*NNOD
            ERR(4) = ZERO
            ERR(8) = ZERO
            DO K=1,3
              ERR(K) = ZERO
              ERR(K+4) = ZERO 
              DO I=1,NNOD
                II = NSTRF(K2+I-1)
                IF(WEIGHT_MD(II)==1)THEN
                  FOLD = SECFCUM(K,II,N)
                  D2 = SECBUF(KR22+6*I-7+K)
                  D1 = SECBUF(KR21+6*I-7+K)
                  FF = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                  ERR(K) = ERR(K) + (FF - FOLD)
                  ERR(4) = ERR(4) + (FF - FOLD)**2
                END IF
              ENDDO            
              IF(IRODDL/=0)THEN
                DO I=1,NNOD
                  II = NSTRF(K2+I-1)
                  IF(WEIGHT_MD(II)==1)THEN
                    FOLD = SECFCUM(K+4,II,N)
                    D2 = SECBUF(KR22+6*I-4+K)
                    D1 = SECBUF(KR21+6*I-4+K)
                    FF = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                    ERR(K+4) = ERR(K+4) + (FF - FOLD)
                    ERR(8) = ERR(8) + (FF - FOLD)**2
                  END IF
                ENDDO 
              ENDIF           
            ENDDO            
            FSAV(11,N) = FSAV(11,N) + ERR(1)*DT12
            FSAV(12,N) = FSAV(12,N) + ERR(2)*DT12
            FSAV(13,N) = FSAV(13,N) + ERR(3)*DT12
            FSAV(14,N) = ERR(4)
            FSAV(16,N) = FSAV(16,N) + ERR(5)*DT12
            FSAV(17,N) = FSAV(17,N) + ERR(6)*DT12
            FSAV(18,N) = FSAV(18,N) + ERR(7)*DT12
            FSAV(19,N) = ERR(8)
          ENDIF
          KR0 = NSTRF(K0+25)
          K0  = NSTRF(K0+24)
        ENDDO
      ENDIF
C-----------------------------------------------
C READ FILE dans l ordre des sections lues sur le fichier
C  T = TT + DT2
C-----------------------------------------------
       IF(NSPMD==1) THEN
         CALL SECT_READ (TT+DT2 ,NSTRF  ,SECBUF)
       ELSE
         NNODT = 0
         IF(ISPMD==0) THEN
           K0  = NSTRF(25)
           DO I = 1, NSECT
             IF(NSTRF(K0)>=100) NNODT = NNODT + IAD_CUT(NSPMD+2,I)
             K0  = NSTRF(K0+24)
           END DO
         END IF
C
C SPMD SPECIFIC : MAJ MODIF NSTRF et SECBUF dans SECT_READP
C         
         CALL SECT_READP(TT+DT2,NSTRF,SECBUF,NNODT,IAD_CUT,FR_CUT)
       END IF
C-----------------------------------------------
C IMPOSED VELOCITY
C  T = TT + DT2
C-----------------------------------------------
       TT1 = SECBUF(2)
       TT2 = SECBUF(3)
       TT3 = SECBUF(4)
       IF(NSTRF(2)>=1)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        K0  = NSTRF(25)
        KR0 = NSTRF(26)
        DO N=1,NSECT
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          NBINTER = NSTRF(K0+14)
          ALPHA = 1.-SECBUF(KR0+2)
          IF(TYPE>=100.AND.ALPHA/=0.0)THEN
            K2 = K0 + 30 + NBINTER
            KR1 = KR0 + 10
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            KR11 = KR1 + IFRL2*6*NNOD
            KR12 = KR1 + IFRL1*6*NNOD
            DW   = SECBUF(KR0+1)
            IF(ISPMD==0) THEN
              TFEXTL=DW*DT1
            ELSE
              TFEXTL=ZERO
            ENDIF
            TFEXT=TFEXT + TFEXTL
            DW=ZERO
            ERR(4) = ZERO
            ERR(8) = ZERO    
            DO K=1,3
              ERR(K) = ZERO
              ERR(K+4) = ZERO      
              DO I=1,NNOD
               II = NSTRF(K2+I-1)
               D2 = SECBUF(KR12+6*I-7+K)
               D1 = SECBUF(KR11+6*I-7+K)
               DD = ((TT+DT2)*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
               VI = (DD-D(K,II))/DT2
               AA = ALPHA*((VI-V(K,II))/DT12 - A(K,II))
               A(K,II) = A(K,II) + AA
               IF(WEIGHT(II)==1) THEN
                DV = DT12*A(K,II)
                DW = DW + HALF*(V(K,II)+HALF*DV)*MS(II)*AA
                ERR(K)=ERR(K)+WEIGHT_MD(II)*MS(II)*(VI-V(K,II)-DV)
                ERR(4)=ERR(4)
     .               + WEIGHT_MD(II)*MS(II)*(VI**2-(V(K,II)+DV)**2)
               ENDIF
              ENDDO            
              IF(IRODDL/=0)THEN
                DO I=1,NNOD
                 II = NSTRF(K2+I-1)
                 D2 = SECBUF(KR12+6*I-4+K)
                 D1 = SECBUF(KR11+6*I-4+K)
                 DD = ((TT+DT2)*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                 VI = (DD-DR(K,II))/DT2
                 AA = ALPHA*((VI-VR(K,II))/DT12 - AR(K,II))
                 AR(K,II) = AR(K,II) + AA
                 IF(WEIGHT(II)==1) THEN
                  DV = DT12*AR(K,II)
                  DW = DW + HALF*(VR(K,II)+HALF*DV)*IN(II)*AA
                  ERR(K+4)=ERR(K+4)
     .                   + WEIGHT_MD(II)*IN(II)*(VI-VR(K,II) - DV)
                  ERR(8)=ERR(8)
     .                 + WEIGHT_MD(II)*IN(II)*(VI**2-(VR(K,II)+DV)**2)
                 ENDIF
                ENDDO 
              ENDIF           
            ENDDO            
            TFEXTL=TFEXTL + DT1*DW
            TFEXT=TFEXT + DT1*DW
            SECBUF(KR0+1) = DW
C-----------------------------------------------
C SPMD SPECIFIC : MAJ DW
C-----------------------------------------------
            IF(NSPMD>1) THEN
             IF (ISPMD==0) THEN
              WA(1) = SECBUF(KR0+1)
              WA(2) = SECBUF(KR0+3)
              WA(3) = SECBUF(KR0+4)
              LEN = 3
              CALL SPMD_GLOB_DSUM9(WA,LEN)
              SECBUF(KR0+1) = WA(1)
              SECBUF(KR0+3) = WA(2)
              SECBUF(KR0+4) = WA(3)

             ELSE
              LEN = 3
              WA(1) = SECBUF(KR0+1)
              WA(2) = SECBUF(KR0+3)
              WA(3) = SECBUF(KR0+4)
              CALL SPMD_GLOB_DSUM9(WA,LEN)
              SECBUF(KR0+1) = ZERO
              SECBUF(KR0+3) = ZERO
              SECBUF(KR0+4) = ZERO    
             ENDIF
            ENDIF
C-----------------------------------------------
            FSAV(22,N) = ERR(1)
            FSAV(23,N) = ERR(2)
            FSAV(24,N) = ERR(3)
            FSAV(25,N) = HALF*ERR(4)
            FSAV(26,N) = ERR(5)
            FSAV(27,N) = ERR(6)
            FSAV(28,N) = ERR(7)
            FSAV(29,N) = HALF*ERR(8)
            FSAV(30,N) = FSAV(30,N) + TFEXTL + SECBUF(KR0+4)
            ISECT=0
            IF(STABSEN/=0) ISECT=TABS(N+1)-TABS(N)
            IF(ISECT/=0) THEN
              FBSAV6(7,2:6,ISECT) = ZERO
              FBSAV6(7,1,ISECT)=FSAV(30,N)
            ENDIF
          ENDIF
          KR0 = NSTRF(K0+25)
          K0  = NSTRF(K0+24)
        ENDDO
       ENDIF
C---------------------------------------------------------
        K0=NSTRF(25)
        DO I=1,NSECT
          NNOD = NSTRF(K0+6)
          K2 = K0 + 30 + NSTRF(K0+14)
          DO K = 1, NNOD
            N = NSTRF(K2+K-1)
            SECFCUM(1,N,I)=ZERO
            SECFCUM(2,N,I)=ZERO
            SECFCUM(3,N,I)=ZERO
            SECFCUM(5,N,I)=ZERO
            SECFCUM(6,N,I)=ZERO
            SECFCUM(7,N,I)=ZERO    
          ENDDO
          K0=NSTRF(K0+24)
        ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SECT_FIO                      source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SECT_READ                     source/tools/sect/sectio.F    
Chd|        SECT_READP                    source/tools/sect/sectio.F    
Chd|====================================================================
      SUBROUTINE SECT_FIO(NSTRF  ,V      ,VR    ,FSAV ,
     2                    A      ,AR     ,SECBUF,MS   ,IN ,
     3                    WEIGHT ,IAD_CUT,FR_CUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "chara_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*),WEIGHT(*), IAD_CUT(NSPMD+2,*), FR_CUT(*)   
      my_real 
     .   V(3,*), VR(3,*), A(3,*), AR(3,*), MS(*),  
     .   FSAV(NTHVKI,*), SECBUF(*), IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,
     .   IFRL1, IFRL2, L,TYPE, NNOD,KR11,KR12,
     .   KR21,KR22,NBINTER,LEN,NNODT
      my_real
     .   DW, TT1, TT2, TT3, VI, DD, D1, D2,TFEXTL,
     .   TNEXT, DELTAT,ERR(8), FF, FOLD, ALPHA,AA,DTINV
C
      IF(NSECT==0)RETURN
      IF(NSTRF(2)==0)RETURN
C-----------------------------------------------
C READ FILE dans l'ordre des sections lues sur le fichier
C  T = TT 
C-----------------------------------------------
       IF(NSPMD==1) THEN
         CALL SECT_READ (TT ,NSTRF  ,SECBUF)
       ELSE
         NNODT = 0
         IF(ISPMD==0) THEN
           K0  = NSTRF(25)
           DO I = 1, NSECT
             IF(NSTRF(K0)>=100) NNODT = NNODT + IAD_CUT(NSPMD+2,I)
             K0  = NSTRF(K0+24)
           END DO
         END IF
C
C SPMD SPECIFIC : MAJ MODIF NSTRF et SECBUF dans SECT_READP
C         
         CALL SECT_READP(TT,NSTRF,SECBUF,NNODT,IAD_CUT,FR_CUT)
       END IF
C-----------------------------------------------
C IMPOSED FORCES
C-----------------------------------------------
       TT1 = SECBUF(2)
       TT2 = SECBUF(3)
       TT3 = SECBUF(4)
       DTINV=ZERO
       IF(DT1>ZERO)DTINV=ONE/DT1
       IF(NSTRF(2)>=1)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        K0  = NSTRF(25)
        KR0 = NSTRF(26)
        DO N=1,NSECT
          NNOD = NSTRF(K0+6)
          TYPE=NSTRF(K0)
          NBINTER = NSTRF(K0+14)
          ALPHA = SECBUF(KR0+2)
          IF(TYPE>=101.AND.ALPHA/=0.0)THEN
            K2 = K0 + 30 + NBINTER
            KR1 = KR0 + 10
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            KR11 = KR1 + IFRL2*6*NNOD
            KR12 = KR1 + IFRL1*6*NNOD
            KR21 = KR2 + IFRL2*6*NNOD
            KR22 = KR2 + IFRL1*6*NNOD
            DW   = SECBUF(KR0+3)
            IF(ISPMD==0) THEN
              TFEXTL=DW*DT1
            ELSE
              TFEXTL=ZERO
            ENDIF
            TFEXT=TFEXT + TFEXTL
            DW=0.
            DO K=1,3
              DO I=1,NNOD
                II = NSTRF(K2+I-1)
                D2 = SECBUF(KR22+6*I-7+K)
                D1 = SECBUF(KR21+6*I-7+K)
                AA = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                D2 = SECBUF(KR12+6*I-7+K)
                D1 = SECBUF(KR11+6*I-7+K)
                DD = MS(II)*(D2-D1) / (TT2-TT1) 
                AA = DD*DTINV + AA
                A(K,II) = A(K,II) + AA
                IF(WEIGHT(II)==1) THEN
                  DW = DW + HALF*V(K,II)*AA
                ENDIF
              ENDDO            
              IF(IRODDL/=0)THEN
                DO I=1,NNOD
                  II = NSTRF(K2+I-1)
                  D2 = SECBUF(KR22+6*I-4+K)
                  D1 = SECBUF(KR21+6*I-4+K)
                  AA = (TT*(D2-D1)+TT2*D1-TT1*D2) / (TT2-TT1)
                  D2 = SECBUF(KR12+6*I-4+K)
                  D1 = SECBUF(KR11+6*I-4+K)
                  DD = IN(II)*(D2-D1) / (TT2-TT1) 
                  AA = DD*DTINV + AA
                  AR(K,II) = AR(K,II) + AA
                  IF(WEIGHT(II)==1) THEN
                    DW = DW + HALF*VR(K,II)*AA
                  ENDIF
                ENDDO
              ENDIF            
            ENDDO            
            TFEXTL=TFEXTL + DT1*DW
            TFEXT=TFEXT + DT1*DW
            SECBUF(KR0+3) = DW
            SECBUF(KR0+4) = TFEXTL
          ENDIF
          KR0 = NSTRF(K0+25)
          K0  = NSTRF(K0+24)
        ENDDO
       ENDIF
C---------------------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  SECTIO                        source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F    
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|====================================================================
      SUBROUTINE SECTIO(NNOD,N1  ,N2   ,N3   ,NSTRF  ,X  ,
     2                  V   ,VR  ,FSAV ,FOPTA,SECFCUM,MS ,
     3                  IN  ,TYPE,IFRAM,XSEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNOD,N1, N2, N3,TYPE,IFRAM
      INTEGER NSTRF(*)
      my_real X(3,*), FSAV(NTHVKI), FOPTA(6), SECFCUM(7,*), 
     .        V(3,*), VR(3,*),MS(*),IN(*),XSEC(4,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   J, I, K, I1, I2, N
      my_real
     .   FX1, FY1, FZ1,MX1, MY1, MZ1, DX1,DY1, DZ1,FST(35), 
     .   MSX, MSY, MSZ, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6, XXN, YYN, ZZN,
     .   D13, XXC, YYC, ZZC, FSX, FSY, FSZ, FN, FSNX, FSNY, FSNZ, FSTX,
     .   FSTY, FSTZ, DMX, DMY, DMZ
C-----------------------------------------------
C
C---------------------------------------------------------
C
       DO I=1,35
         FST(I)=ZERO
       ENDDO
C
       IF(NSPMD==1) THEN
        CALL SEC_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2    XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3    XXN, YYN, ZZN,IFRAM,NNOD,NSTRF,MS)
       ELSE
         CALL SEC_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2                  XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3                  XXN, YYN, ZZN, IFRAM, N1  , XSEC)
       END IF
C
       IF(IRODDL/=0)THEN
         DO I=1,NNOD
             N=NSTRF(I)
C
             FX1=SECFCUM(1,N)
             FY1=SECFCUM(2,N)
             FZ1=SECFCUM(3,N)
C
             MX1=SECFCUM(5,N)
             MY1=SECFCUM(6,N)
             MZ1=SECFCUM(7,N)
C
             DX1=X(1,N)
             DY1=X(2,N)
             DZ1=X(3,N)
C
             FSX=FX1
             FSY=FY1
             FSZ=FZ1
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1=DX1-XXC
             DY1=DY1-YYC
             DZ1=DZ1-ZZC
C
             MSX =DY1*FZ1-DZ1*FY1
             MSY =DZ1*FX1-DX1*FZ1
             MSZ =DX1*FY1-DY1*FX1
C
             MSX =MSX+MX1
             MSY =MSY+MY1
             MSZ =MSZ+MZ1
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
             FST(30) = FST(30) + FSX 
             FST(31) = FST(31) + FSY 
             FST(32) = FST(32) + FSZ
             FST(33) = FST(33) + MSX
             FST(34) = FST(34) + MSY
             FST(35) = FST(35) + MSZ
             FST(10)=FST(10)
     .        +FX1*V(1,N) +FY1*V(2,N) +FZ1*V(3,N)
             FST(15)=FST(15)
     .        +MX1*VR(1,N)+MY1*VR(2,N)+MZ1*VR(3,N)
             FST(20)=FST(20)
     .        +MS(N)*(V(1,N)*V(1,N)+V(2,N)*V(2,N)+V(3,N)*V(3,N))
             FST(21)=FST(21)
     .        +IN(N)*(VR(1,N)*VR(1,N)+VR(2,N)*VR(2,N)+VR(3,N)*VR(3,N))
C
         ENDDO
       ELSE
c on ne traite pas les VR car IRODDL vaut 0
         DO I=1,NNOD
             N=NSTRF(I)
C
             FX1=SECFCUM(1,N)
             FY1=SECFCUM(2,N)
             FZ1=SECFCUM(3,N)
C
             MX1=SECFCUM(5,N)
             MY1=SECFCUM(6,N)
             MZ1=SECFCUM(7,N)
C
             DX1=X(1,N)
             DY1=X(2,N)
             DZ1=X(3,N)
C
             FSX=FX1
             FSY=FY1
             FSZ=FZ1
C
             FN=FSX*XXN+FSY*YYN+FSZ*ZZN
             FSNX=FN*XXN
             FSNY=FN*YYN
             FSNZ=FN*ZZN
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1=DX1-XXC
             DY1=DY1-YYC
             DZ1=DZ1-ZZC
C
             MSX =DY1*FZ1-DZ1*FY1
             MSY =DZ1*FX1-DX1*FZ1
             MSZ =DX1*FY1-DY1*FX1
C
             MSX =MSX+MX1
             MSY =MSY+MY1
             MSZ =MSZ+MZ1
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
             FST(30) = FST(30) + FSX 
             FST(31) = FST(31) + FSY 
             FST(32) = FST(32) + FSZ
             FST(33) = FST(33) + MSX
             FST(34) = FST(34) + MSY
             FST(35) = FST(35) + MSZ
             FST(10)=FST(10)
     .        +FX1*V(1,N) +FY1*V(2,N) +FZ1*V(3,N)
             FST(20)=FST(20)
     .        +MS(N)*(V(1,N)*V(1,N)+V(2,N)*V(2,N)+V(3,N)*V(3,N))
C
         ENDDO
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
              FSAV(10)=FSAV(10)+DT12*(FST(10)+FST(15))
              FSAV(15)=FSAV(15)+DT12*FST(15)
              FSAV(20)=HALF*FST(20)
              FSAV(21)=HALF*FST(21)      
              FSAV(31)=FSAV(31)+DT12*FST(33) 
              FSAV(32)=FSAV(32)+DT12*FST(34)
              FSAV(33)=FSAV(33)+DT12*FST(35)
              FSAV(34)=FSAV(34) + DT12* (XX4*(FST(1)+FST(4)) + 
     .                 YY4*(FST(2)+FST(5)) + ZZ4*(FST(3)+FST(6)))
              FSAV(35)=FSAV(35) + DT12* (XX5*(FST(1)+FST(4)) +
     .                 YY5*(FST(2)+FST(5)) + ZZ5*(FST(3)+FST(6)))
              FSAV(36)=FSAV(36) + DT12* (XX6*(FST(1)+FST(4)) +
     .                 YY6*(FST(2)+FST(5)) + ZZ6*(FST(3)+FST(6)))
              FSAV(37)=XXC
              FSAV(38)=YYC
              FSAV(39)=ZZC
C
              FOPTA(1) = FOPTA(1) + FST(30) 
              FOPTA(2) = FOPTA(2) + FST(31)  
              FOPTA(3) = FOPTA(3) + FST(32) 
              FOPTA(4) = FOPTA(4) + FST(33) 
              FOPTA(5) = FOPTA(5) + FST(34) 
              FOPTA(6) = FOPTA(6) + FST(35) 
#include "lockoff.inc"
C
      RETURN
      END

Chd|====================================================================
Chd|  SECT_READ                     source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        SECT_FIO                      source/tools/sect/sectio.F    
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        READ_R_C                      source/output/tools/sortie_c.c
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SECT_READ (TTT,NSTRF  ,SECBUF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*)
      my_real 
     .   TTT, SECBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,
     .   IR1, IR2, IFRL1, IFRL2, FOUND, NR, L, NSECR, ID_SEC,
     .   TYPE, IFILNAM(2148), LROOTLEN, LREC, NNOD,IR, NNODR,KR11,KR12,
     .   KR21,KR22,NBINTER,IEXTRA
      my_real 
     .   TT1, TT2, TT3
      CHARACTER FILNAM*12,LCHRUN*2
      LOGICAL FEXIST
      REAL*4 R4
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME

C-----------------------------------------------
C READ FILE dans l'ordre des sections lues sur le fichier
C  TTT = TT ou TT + DT2
C-----------------------------------------------

      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IEXTRA=NSTRF(3)
      IF(NSTRF(2)>=1.AND.TTT>=TT2.AND.IEXTRA==0
     .       .AND. TTT <= TSTOP)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        L=1
        CALL CUR_FIL_C(4)
        DOWHILE(TT2<=TTT)
          IFRL1=IFRL2
          IFRL2=MOD(IFRL1+1,2)
          CALL READ_R_C(R4,1)
C test EOF-------------------------------------------------------------------
          IF(R4>=0.0)THEN
            TT1=TT2
            TT2=R4
          ELSEIF(TT3==EP30)THEN
            CALL CLOSE_C()
            IEXTRA=1
            NSTRF(3)=IEXTRA
            GOTO 100
          ELSE
            CALL CLOSE_C()
            IR2=NSTRF(5)
            IR1=IR2
            IR=IR1
            LROOTLEN=0
            DO I=1,8
                FILNAM(I:I)=CHAR(NSTRF(15+I))
                IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
            ENDDO
            DOWHILE(TT3<=TTT.AND.IR<100)
              IR=IR+1
              WRITE(LCHRUN,'(I2.2)')IR
              FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
              LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM) 
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
              INQUIRE(FILE=TMP_NAME(1:LEN_TMP_NAME),EXIST=FEXIST)

              IF(.NOT.FEXIST) THEN
                LEN_TMP_NAME =  LEN_TRIM(FILNAM)   
                TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TMP_NAME)
                INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
              ENDIF            

              IF(FEXIST)THEN
                IR2=IR
                CALL CUR_FIL_C(4)
                DO I=1,LEN_TMP_NAME
                  IFILNAM(I)=ICHAR(TMP_NAME(I:I))
                ENDDO
                CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
                CALL READ_R_C(R4,1)
                CALL CLOSE_C()
                TT3=R4
              ENDIF
            ENDDO
            IF(IR==100)THEN
              TT3=EP30
              IEXTRA=1
              NSTRF(3)=IEXTRA
              GOTO 100
            ENDIF
            WRITE(LCHRUN,'(I2.2)')IR1
            FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
            LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM) 
            TMP_NAME(1:LEN_TMP_NAME)=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
            INQUIRE(FILE=TMP_NAME(1:LEN_TMP_NAME),EXIST=FEXIST)

            IF(.NOT.FEXIST) THEN
                LEN_TMP_NAME =  LEN_TRIM(FILNAM)   
                TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TMP_NAME)
                INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
            ENDIF 

            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
C
            SECBUF(4) = TT3
C
            NSTRF(4) = IR1
            NSTRF(5) = IR2
C
            CALL READ_R_C(R4,1)
            TT1=TT2
            TT2=R4
          ENDIF          
C-----------------------------------------------
          CALL READ_I_C(L,1)
          CALL READ_I_C(NSECR,1)
          DO NR=1,NSECR
            CALL READ_I_C(ID_SEC,1)
            K0  = NSTRF(25)
            KR0 = NSTRF(26)
            FOUND=0
            N=0
            DOWHILE(FOUND==0.AND.N<NSECT)
                N=N+1
                IF(ID_SEC==NSTRF(K0+23))THEN
                  FOUND=1
                ELSE
                  KR0 = NSTRF(K0+25)
                  K0  = NSTRF(K0+24)
                ENDIF
            ENDDO
            NNOD = NSTRF(K0+6)
            KR1 = KR0 + 10 + IFRL1*6*NNOD
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            CALL READ_I_C(TYPE,1)
            CALL READ_I_C(NNODR,1)
            IF (NNOD/=NNODR .AND. FOUND == 1) THEN
              CALL ANCMSG(MSGID=35,ANMODE=ANINFO_BLIND,
     .                    I1=ID_SEC,I2=NNODR,I3=NNOD)
              CALL ARRET(2)
            END IF
            IF(FOUND==0.OR.NSTRF(K0)<100)THEN
C skip deplacements et forces 
              IF(TYPE>=1)THEN
                DO I=1,6*NNODR
                  CALL READ_R_C(R4,1)
                ENDDO
              ENDIF            
              IF(TYPE>=2)THEN
                DO I=1,6*NNODR
                  CALL READ_R_C(R4,1)
                ENDDO            
              ENDIF
            ELSEIF(NSTRF(K0)==100)THEN
C lecture deplacements
              IF(TYPE>=1)THEN
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-6)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-1)=R4
                ENDDO 
              ELSE
C Pb de compatibilite type_new>=100 et type_old<1
              ENDIF           
              IF(TYPE>=2)THEN
C skip forces
                DO I=1,6*NNOD
                  CALL READ_R_C(R4,1)
                ENDDO            
              ENDIF
            ELSEIF(NSTRF(K0)==101)THEN
C lecture deplacements
              IF(TYPE>=1)THEN
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-6)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR1+6*I-1)=R4
                ENDDO            
              ELSE
C Pb de compatibilite type_new>=101 et type_old<1
              ENDIF
              IF(TYPE>=2)THEN
C lecture forces
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUF(KR2+6*I-6)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR2+6*I-5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR2+6*I-4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR2+6*I-3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR2+6*I-2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUF(KR2+6*I-1)=R4
                ENDDO            
              ELSE
C Pb de compatibilite type_new>=101 et type_old<2
              ENDIF
            ELSEIF(NSTRF(K0)>=102)THEN
C a faire
            ENDIF
          ENDDO
        ENDDO
C-----------------------------------------------
        SECBUF(2) = TT1
        SECBUF(3) = TT2
C
        NSTRF(7) = IFRL1
      ENDIF
 100  CONTINUE
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SECT_READP                    source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        SECT_FIO                      source/tools/sect/sectio.F    
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        READ_R_C                      source/output/tools/sortie_c.c
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SD_CUT                   source/mpi/sections/spmd_section.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SECT_READP(TTT,NSTRF,SECBUF,NNODT,IAD_CUT,FR_CUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNODT, NSTRF(*), IAD_CUT(NSPMD+2,*), FR_CUT(*)
      my_real 
     .   TTT, SECBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   J, I, K, II, I1, I2, N, KR1,KR2,KR3,K0,KR0,K1,K2,KC,IFLG,
     .   IR1, IR2, IFRL1, IFRL2, FOUND, NR, L, LL, NSECR, ID_SEC,NNODG,
     .   TYPE, IFILNAM(2148), LROOTLEN, LREC, NNOD,IR, NNODR,KR11,KR12,
     .   KR21,KR22,NBINTER,IEXTRA, ADDSEC(2*NSECT)
      my_real 
     .   TT1, TT2, TT3, BUFCOM(3*NSECT+7), SECBUFG(24*NNODT)
      CHARACTER FILNAM*12,LCHRUN*2
      LOGICAL FEXIST
      REAL*4 R4

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME
C-----------------------------------------------
C READ FILE dans l'ordre des sections lues sur le fichier
C  TTT = TT ou TT + DT2
C-----------------------------------------------
Cel debranchement pi <> p0
      IF(ISPMD/=0) GO TO 100
Cel Init buffer communication 
      DO I = 1, NSECT
        BUFCOM(I) = ZERO
        BUFCOM(I+NSECT) = ZERO
        BUFCOM(I+2*NSECT) = ZERO
        ADDSEC(I) = ZERO
        ADDSEC(I+NSECT) = ZERO
      END DO
C
      TT1 = SECBUF(2)
      TT2 = SECBUF(3)
      TT3 = SECBUF(4)
      IEXTRA=NSTRF(3)
      IF(NSTRF(2)>=1.AND.TTT>=TT2.AND.IEXTRA==0
     .       .AND. TTT <= TSTOP)THEN
        IFRL1=NSTRF(7)
        IFRL2=MOD(IFRL1+1,2)
        LL=1
        IF(ISPMD==0) THEN
          CALL CUR_FIL_C(4)
        END IF
        L = 0
        DO WHILE(TT2<=TTT)
          IFRL1=IFRL2
          IFRL2=MOD(IFRL1+1,2)
          CALL READ_R_C(R4,1)
C test EOF-------------------------------------------------------------------
          IF(R4>=0.0)THEN
            TT1=TT2
            TT2=R4
          ELSEIF(TT3==EP30)THEN
            CALL CLOSE_C()
            IEXTRA=1
            NSTRF(3)=IEXTRA
            GOTO 100
          ELSE
            CALL CLOSE_C()
            IR2=NSTRF(5)
            IR1=IR2
            IR=IR1
            LROOTLEN=0
            DO I=1,8
                FILNAM(I:I)=CHAR(NSTRF(15+I))
                IF(FILNAM(I:I)/=' ')LROOTLEN=LROOTLEN+1
            ENDDO
            DOWHILE(TT3<=TTT.AND.IR<100)
              IR=IR+1
              WRITE(LCHRUN,'(I2.2)')IR
              FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
              INQUIRE(FILE=FILNAM,EXIST=FEXIST)

              IF(.NOT.FEXIST) THEN
                LEN_TMP_NAME =  OUTFILE_NAME_LEN +LEN_TRIM(FILNAM)   
                TMP_NAME(1:LEN_TMP_NAME)=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LROOTLEN+4)
                INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
              ENDIF 

              IF(FEXIST)THEN
                IR2=IR
                CALL CUR_FIL_C(4)
                DO I=1,LEN_TMP_NAME
                  IFILNAM(I)=ICHAR(TMP_NAME(I:I))
                ENDDO

                CALL OPEN_C(IFILNAM,TMP_NAME,1)
                CALL READ_R_C(R4,1)
                CALL CLOSE_C()
                TT3=R4
              ENDIF
            ENDDO
            IF(IR==100)THEN
              TT3=EP30
              IEXTRA=1
              NSTRF(3)=IEXTRA
              GOTO 100
            ENDIF
            WRITE(LCHRUN,'(I2.2)')IR1
            FILNAM=FILNAM(1:LROOTLEN)//'SC'//LCHRUN
            LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)      
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
            INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)

            IF(.NOT.FEXIST) THEN
                LEN_TMP_NAME =  LEN_TRIM(FILNAM)   
                TMP_NAME(1:LEN_TMP_NAME)=FILNAM(1:LEN_TMP_NAME)
                INQUIRE(FILE=TMP_NAME(1:LEN_TRIM(TMP_NAME)),EXIST=FEXIST)
            ENDIF 

            CALL CUR_FIL_C(4)
            DO I=1,LEN_TMP_NAME
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO

            CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
C
            SECBUF(4) = TT3
C
            NSTRF(4) = IR1
            NSTRF(5) = IR2
C
            CALL READ_R_C(R4,1)
            IF(R4 /= ZERO) L = 0
            TT1=TT2
            TT2=R4
          ENDIF          
C-----------------------------------------------
          CALL READ_I_C(LL,1)
          CALL READ_I_C(NSECR,1)
          DO NR=1,NSECR
            CALL READ_I_C(ID_SEC,1)
            K0  = NSTRF(25)
            KR0 = NSTRF(26)
            FOUND=0
            N=0
            DOWHILE(FOUND==0.AND.N<NSECT)
                N=N+1
                IF(ID_SEC==NSTRF(K0+23))THEN
                  FOUND=1
                ELSE
C                  KR0 = NSTRF(K0+25)
                  K0  = NSTRF(K0+24)
                ENDIF
            ENDDO
            IF(FOUND==1) THEN
              NNOD = IAD_CUT(NSPMD+2,N)
            END IF
C            NNOD = NSTRF(K0+6)
C            KR1 = KR0 + 10 + IFRL1*6*NNOD
C            KR2 = KR1 + 12*NNOD
C            KR3 = KR2 + 12*NNOD
            CALL READ_I_C(TYPE,1)
            CALL READ_I_C(NNODR,1)
            IF (NNOD/=NNODR .AND. FOUND == 1) THEN
              CALL ANCMSG(MSGID=35,ANMODE=ANINFO_BLIND,
     .                    I1=ID_SEC,I2=NNODR,I3=NNOD)
              CALL ARRET(2)
            END IF
            IF(FOUND==0.OR.NSTRF(K0)<100)THEN
C skip deplacements et forces 
              IF(TYPE>=1)THEN
                DO I=1,6*NNODR
                  CALL READ_R_C(R4,1)
                ENDDO
              ENDIF            
              IF(TYPE>=2)THEN
                DO I=1,6*NNODR
                  CALL READ_R_C(R4,1)
                ENDDO            
              ENDIF
            ELSEIF(NSTRF(K0)==100)THEN
C lecture deplacements
              IF(TYPE>=1)THEN
                BUFCOM(N) = 1
                BUFCOM(N+NSECT+IFRL1*NSECT) = 1
                ADDSEC(N+IFRL1*NSECT) = L+1
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+1)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+6)=R4
                  L = L + 6
                ENDDO 
              ELSE
C Pb de compatibilite type_new>=100 et type_old<1
              ENDIF           
              IF(TYPE>=2)THEN
C skip forces
                DO I=1,6*NNOD
                  CALL READ_R_C(R4,1)
                ENDDO            
              ENDIF
            ELSEIF(NSTRF(K0)==101)THEN
C lecture deplacements
              IF(TYPE>=1)THEN
                BUFCOM(N) = 1
                BUFCOM(N+NSECT+IFRL1*NSECT) = 1
                ADDSEC(N+IFRL1*NSECT) = L+1
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+1)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+6)=R4
                  L = L + 6
                ENDDO            
              ELSE
C Pb de compatibilite type_new>=101 et type_old<1
              ENDIF
              IF(TYPE>=2)THEN
C lecture forces
                BUFCOM(N) = 2
c                BUFCOM(N+NSECT+IFRL1*NSECT) = 1
c                ADDSEC(N+IFRL1*NSECT) = L+1
                DO I=1,NNOD
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+1)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+2)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+3)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+4)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+5)=R4
                  CALL READ_R_C(R4,1)
                  SECBUFG(L+6)=R4
                  L = L + 6
                ENDDO            
              ELSE
C Pb de compatibilite type_new>=101 et type_old<2
              ENDIF
            ELSEIF(NSTRF(K0)>=102)THEN
C a faire
            ENDIF
          ENDDO
        ENDDO
C-----------------------------------------------
        SECBUF(2) = TT1
        SECBUF(3) = TT2
C
        NSTRF(7) = IFRL1
      ENDIF
      BUFCOM(3*NSECT+1) = NSTRF(3)
      BUFCOM(3*NSECT+2) = NSTRF(4)
      BUFCOM(3*NSECT+3) = NSTRF(5)
      BUFCOM(3*NSECT+4) = NSTRF(7)
      BUFCOM(3*NSECT+5) = SECBUF(2)
      BUFCOM(3*NSECT+6) = SECBUF(3)
      BUFCOM(3*NSECT+7) = SECBUF(4)
 100  CONTINUE
      CALL SPMD_RBCAST(BUFCOM,BUFCOM,3*NSECT+7,1,0,2)
      IF(ISPMD/=0) THEN
         NSTRF(3) = NINT(BUFCOM(3*NSECT+1))
         NSTRF(4) = NINT(BUFCOM(3*NSECT+2))
         NSTRF(5) = NINT(BUFCOM(3*NSECT+3))
         NSTRF(7) = NINT(BUFCOM(3*NSECT+4))
         SECBUF(2) = BUFCOM(3*NSECT+5)
         SECBUF(3) = BUFCOM(3*NSECT+6)
         SECBUF(4) = BUFCOM(3*NSECT+7)
      END IF
C
C Traitement Passage de SECBUFG a SECBUF local
C
      L = 1
      KC = 1
      K0  = NSTRF(25)
      KR0 = NSTRF(26)
      DO I = 1, NSECT
        IF(NINT(BUFCOM(I))>0) THEN
          IF(ISPMD==0) THEN
            NNODG = IAD_CUT(NSPMD+2,I)
          ELSE
            NNODG = 0
          END IF
          NNOD = NSTRF(K0+6)
          IFLG = NINT(BUFCOM(I))
          IF(NINT(BUFCOM(NSECT+I))==1) THEN
C remplissage secbuf avec ifrl1 = 0
            IFRL1 = 0
            KR1 = KR0 + 10 + IFRL1*6*NNOD
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            IF(ISPMD==0) THEN
              L = ADDSEC(I+IFRL1*NSECT)
            END IF
            CALL SPMD_SD_CUT(
     1        SECBUFG(L),NNODG       ,SECBUF(KR1),SECBUF(KR2),NNOD,
     2        FR_CUT(KC),IAD_CUT(1,I),IFLG                        )
          END IF
          IF(NINT(BUFCOM(2*NSECT+I))==1) THEN
C remplissage secbuf avec ifrl1 = 1
            IFRL1 = 1
            KR1 = KR0 + 10 + IFRL1*6*NNOD
            KR2 = KR1 + 12*NNOD
            KR3 = KR2 + 12*NNOD
            IF(ISPMD==0) THEN
              L = ADDSEC(I+IFRL1*NSECT)
            END IF
            CALL SPMD_SD_CUT(
     1        SECBUFG(L),NNODG       ,SECBUF(KR1),SECBUF(KR2),NNOD,
     2        FR_CUT(KC),IAD_CUT(1,I),IFLG                        )
          END IF
        END IF
        IF(NSTRF(K0)>=100.AND.ISPMD==0) THEN
          KC = KC + IAD_CUT(NSPMD+1,I)
        END IF
        KR0 = NSTRF(K0+25)
        K0  = NSTRF(K0+24)
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SECTIOC41                     source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SECTIOC41(LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,FSAV,
     3                   IXC,FOPTA,
     4                   FX ,FY ,FZ ,MX ,MY ,MZ  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3
      INTEGER NSTRF(3,*),IXC(NIXC,*)
      my_real 
     .   X(3,*), FOPTA(6),FSAV(NTHVKI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJJ(MVSIZ), UNPACK(0:15,4),
     .   NSA, J, I, K, I1, I2,IPACK, N
      my_real
     .   FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10), MX(MVSIZ,4),
     .   MY(MVSIZ,4), MZ(MVSIZ,4),
     .   DX1, DY1, DZ1, FST(15), 
     .   MSX, MSY, MSZ, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6, XXN, YYN, ZZN,
     .   D13, XXC, YYC, ZZC, FSX, FSY, FSZ, FN, FSNX, FSNY, FSNZ, FSTX,
     .   FSTY, FSTZ, DMX, DMY, DMZ, AL4, AL5, AL6
C-----------------------------------------------
C
C
      DATA UNPACK/0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,
     .            0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,
     .            0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,
     .            0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1/
C---------------------------------------------------------
#include "sync_dir.inc"
C---------------------------------------------------------
       IF(NSEG==0)RETURN
       IF(LFT+NFT>NSTRF(1,NSEG))RETURN
       IF(LLT+NFT<NSTRF(1,1   ))RETURN
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
#include "scalar.inc"
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
       DO I=1,15
         FST(I)=ZERO
       ENDDO
C
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
       XX5=XX3-XX1
       YY5=YY3-YY1
       ZZ5=ZZ3-ZZ1
       AL4=SQRT(XX4**2+YY4**2+ZZ4**2)
       XX4=XX4/MAX(AL4,EM20)
       YY4=YY4/MAX(AL4,EM20)
       ZZ4=ZZ4/MAX(AL4,EM20)
       XX6=YY4*ZZ5-ZZ4*YY5
       YY6=ZZ4*XX5-XX4*ZZ5
       ZZ6=XX4*YY5-YY4*XX5
       AL6=SQRT(XX6**2+YY6**2+ZZ6**2)
       XX6=XX6/MAX(AL6,EM20)
       YY6=YY6/MAX(AL6,EM20)
       ZZ6=ZZ6/MAX(AL6,EM20)
       XX5=YY6*ZZ4-ZZ6*YY4
       YY5=ZZ6*XX4-XX6*ZZ4
       ZZ5=XX6*YY4-YY6*XX4
       AL5=SQRT(XX5**2+YY5**2+ZZ5**2)
       XX5=XX5/MAX(AL5,EM20)
       YY5=YY5/MAX(AL5,EM20)
       ZZ5=ZZ5/MAX(AL5,EM20)
C
       D13=(XX3-XX1)*XX4+(YY3-YY1)*YY4+(ZZ3-ZZ1)*ZZ4
       XXC=XX1+D13*XX4
       YYC=YY1+D13*YY4
       ZZC=ZZ1+D13*ZZ4
C
       DO I1=1,4
        IF (NSA>15.OR.IVECTOR==0) THEN
         DO K=1,NSA
           J  =JJJ(K)
           I  =NSTRF(1,J)-NFT
           IPACK =NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FSX=FX(I,I1)
             FSY=FY(I,I1)
             FSZ=FZ(I,I1)
C
C
             N = IXC(I1+1,NSTRF(1,J))
             DX1=X(1,N)
             DY1=X(2,N)
             DZ1=X(3,N)
C
             FN=FSX*XX6+FSY*YY6+FSZ*ZZ6
             FSNX=FN*XX6
             FSNY=FN*YY6
             FSNZ=FN*ZZ6
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1=DX1-XXC
             DY1=DY1-YYC
             DZ1=DZ1-ZZC
C
             MSX =DY1*FSZ-DZ1*FSY
             MSY =DZ1*FSX-DX1*FSZ
             MSZ =DX1*FSY-DY1*FSX
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
C
           ENDIF
         ENDDO
        ELSE
#include "scalar.inc"
         DO K=1,NSA
           J  =JJJ(K)
           I  =NSTRF(1,J)-NFT
           IPACK =NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FSX=FX(I,I1)
             FSY=FY(I,I1)
             FSZ=FZ(I,I1)
C
             N = IXC(I1+1,NSTRF(1,J))
C
             DX1=X(1,N)
             DY1=X(2,N)
             DZ1=X(3,N)
C
             FN=FSX*XX6+FSY*YY6+FSZ*ZZ6
             FSNX=FN*XX6
             FSNY=FN*YY6
             FSNZ=FN*ZZ6
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1=DX1-XXC
             DY1=DY1-YYC
             DZ1=DZ1-ZZC
C
             MSX =DY1*FSZ-DZ1*FSY
             MSY =DZ1*FSX-DX1*FSZ
             MSZ =DX1*FSY-DY1*FSX
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
C
           ENDIF
         ENDDO
        ENDIF
       ENDDO
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
      RETURN
      END

Chd|====================================================================
Chd|  SECTIO3N41                    source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SECTIO3N41(LFT,LLT,NFT,NSEG,N1,
     2                   N2,N3,NSTRF,X,FSAV,
     3                   IXTG, FOPTA,
     4                   FX ,FY ,FZ ,MX ,MY ,MZ  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT, NFT, NSEG, N1, N2, N3
      INTEGER NSTRF(2,*),IXTG(NIXTG,*)
      my_real 
     .   X(3,*), FSAV(NTHVKI), FOPTA(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(MVSIZ,4), JJJ(MVSIZ), UNPACK(7,3),
     .   NSA, J, I, K, I1, I2, IPACK, N
      my_real
     .   FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10), MX(MVSIZ,4),
     .   MY(MVSIZ,4), MZ(MVSIZ,4), FX1(MVSIZ), FY1(MVSIZ), FZ1(MVSIZ),
     .   MX1(MVSIZ), MY1(MVSIZ), MZ1(MVSIZ), DX1(MVSIZ),
     .   DY1(MVSIZ), DZ1(MVSIZ),FST(15), 
     .   MSX, MSY, MSZ, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6, XXN, YYN, ZZN,
     .   D13, XXC, YYC, ZZC, FSX, FSY, FSZ, FN, FSNX, FSNY, FSNZ, FSTX,
     .   FSTY, FSTZ, DMX, DMY, DMZ, AL4, AL5, AL6
C-----------------------------------------------
C
C
      DATA UNPACK/1,0,1,0,1,0,1,
     .            0,1,1,0,0,1,1,
     .            0,0,0,1,1,1,1/
C
       IF(NSEG==0)RETURN
       IF(LFT+NFT>NSTRF(1,NSEG))RETURN
       IF(LLT+NFT<NSTRF(1,1   ))RETURN
C---------------------------------------------------------
       NSA=0
C
       DO 20 J=1,NSEG
       I=NSTRF(1,J)-NFT
       IF (LFT>I) GOTO 20
       IF (LLT<I) GOTO 30
        NSA=NSA+1
        JJJ(NSA)=J
  20   CONTINUE
  30   CONTINUE
C
       IF(NSA==0)RETURN
C
       DO I=1,15
         FST(I)=ZERO
       ENDDO
C
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
       XX5=XX3-XX1
       YY5=YY3-YY1
       ZZ5=ZZ3-ZZ1
       AL4=SQRT(XX4**2+YY4**2+ZZ4**2)
       XX4=XX4/MAX(AL4,EM20)
       YY4=YY4/MAX(AL4,EM20)
       ZZ4=ZZ4/MAX(AL4,EM20)
       XX6=YY4*ZZ5-ZZ4*YY5
       YY6=ZZ4*XX5-XX4*ZZ5
       ZZ6=XX4*YY5-YY4*XX5
       AL6=SQRT(XX6**2+YY6**2+ZZ6**2)
       XX6=XX6/MAX(AL6,EM20)
       YY6=YY6/MAX(AL6,EM20)
       ZZ6=ZZ6/MAX(AL6,EM20)
       XX5=YY6*ZZ4-ZZ6*YY4
       YY5=ZZ6*XX4-XX6*ZZ4
       ZZ5=XX6*YY4-YY6*XX4
       AL5=SQRT(XX5**2+YY5**2+ZZ5**2)
       XX5=XX5/MAX(AL5,EM20)
       YY5=YY5/MAX(AL5,EM20)
       ZZ5=ZZ5/MAX(AL5,EM20)
C
       D13=(XX3-XX1)*XX4+(YY3-YY1)*YY4+(ZZ3-ZZ1)*ZZ4
       XXC=XX1+D13*XX4
       YYC=YY1+D13*YY4
       ZZC=ZZ1+D13*ZZ4
C
       DO I1 = 1,3
         DO K=1,NSA
           J  = JJJ(K)
           I  = NSTRF(1,J)-NFT
           IPACK = NSTRF(2,J)
           IF(UNPACK(IPACK,I1)/=0)THEN
             FX1(K)=FX(I,I1)
             FY1(K)=FY(I,I1)
             FZ1(K)=FZ(I,I1)
C
             MX1(K)=MX(I,I1)
             MY1(K)=MY(I,I1)
             MZ1(K)=MZ(I,I1)
C
             N = IXTG(I1+1,NSTRF(1,J))
             DX1(K)=X(1,N)
             DY1(K)=X(2,N)
             DZ1(K)=X(3,N)
C
             FSX=FX1(K)
             FSY=FY1(K)
             FSZ=FZ1(K)
C
             FN=FSX*XX6+FSY*YY6+FSZ*ZZ6
             FSNX=FN*XX6
             FSNY=FN*YY6
             FSNZ=FN*ZZ6
             FSTX=FSX-FSNX
             FSTY=FSY-FSNY
             FSTZ=FSZ-FSNZ
C
             DX1(K)=DX1(K)-XXC
             DY1(K)=DY1(K)-YYC
             DZ1(K)=DZ1(K)-ZZC
C
             MSX =DY1(K)*FZ1(K)-DZ1(K)*FY1(K)
             MSY =DZ1(K)*FX1(K)-DX1(K)*FZ1(K)
             MSZ =DX1(K)*FY1(K)-DY1(K)*FX1(K)
C
             MSX =MSX+MX1(K)
             MSY =MSY+MY1(K)
             MSZ =MSZ+MZ1(K)
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
C
           ENDIF
         ENDDO
       ENDDO
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
      RETURN
      END
C
Chd|====================================================================
Chd|  SEC_SKEW                      source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        SECTIO                        source/tools/sect/sectio.F    
Chd|        SECTIO3N                      source/tools/sect/sectio.F    
Chd|        SECTIOC                       source/tools/sect/sectio.F    
Chd|        SECTIOP                       source/tools/sect/sectio.F    
Chd|        SECTIOR                       source/tools/sect/sectio.F    
Chd|        SECTIOS                       source/tools/sect/sectio.F    
Chd|        SECTIOS4                      source/tools/sect/sectio.F    
Chd|        SECTIOS6                      source/tools/sect/sectio.F    
Chd|        SECTIOT                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SEC_SKEW(
     1   N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3   XXN, YYN, ZZN,IFRAM,NNOD,NOD,MS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1, N2, N3, NNOD,IFRAM, NOD(*)
      my_real
     .   X(3,*),XXC, YYC, ZZC,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     .   XXN, YYN, ZZN, MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
      my_real
     .   XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   AL4, AL6, AL5, D13, MAS, X1N, Y1N, Z1N
C
C
      IF(IFRAM<10)THEN
C      CALCUL DES MOMEMTS/REPERE N1 N2 N3
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
       XX5=XX3-XX1
       YY5=YY3-YY1
       ZZ5=ZZ3-ZZ1
       XX6=YY4*ZZ5-ZZ4*YY5
       YY6=ZZ4*XX5-XX4*ZZ5
       ZZ6=XX4*YY5-YY4*XX5
       AL6=SQRT(XX6**2+YY6**2+ZZ6**2)
       XX6=XX6/MAX(AL6,EM20)
       YY6=YY6/MAX(AL6,EM20)
       ZZ6=ZZ6/MAX(AL6,EM20)
       XX5=YY6*ZZ4-ZZ6*YY4
       YY5=ZZ6*XX4-XX6*ZZ4
       ZZ5=XX6*YY4-YY6*XX4
       AL5=SQRT(XX5**2+YY5**2+ZZ5**2)
       XX5=XX5/MAX(AL5,EM20)
       YY5=YY5/MAX(AL5,EM20)
       ZZ5=ZZ5/MAX(AL5,EM20)
C
       XXN=XX6
       YYN=YY6
       ZZN=ZZ6
C
       ELSEIF (N1/=0) THEN
C      CALCUL DES MOMEMTS/REPERE GLOBAL
       XX4=X(1,N2)-X(1,N1)
       YY4=X(2,N2)-X(2,N1)
       ZZ4=X(3,N2)-X(3,N1)
       XX5=X(1,N3)-X(1,N1)
       YY5=X(2,N3)-X(2,N1)
       ZZ5=X(3,N3)-X(3,N1)
       XXN=YY4*ZZ5-ZZ4*YY5
       YYN=ZZ4*XX5-XX4*ZZ5
       ZZN=XX4*YY5-YY4*XX5
       AL6=SQRT(XXN**2+YYN**2+ZZN**2)
       XXN=XXN/MAX(AL6,EM20)
       YYN=YYN/MAX(AL6,EM20)
       ZZN=ZZN/MAX(AL6,EM20)
C
       XX4=ONE
       YY4=ZERO
       ZZ4=ZERO
       XX5=ZERO
       YY5=ONE
       ZZ5=ZERO
       XX6=ZERO
       YY6=ZERO
       ZZ6=ONE     
       ELSE
       XXN=ZERO
       YYN=ZERO
       ZZN=ONE
       XX4=ONE
       YY4=ZERO
       ZZ4=ZERO
       XX5=ZERO
       YY5=ONE
       ZZ5=ZERO
       XX6=ZERO
       YY6=ZERO
       ZZ6=ONE    
C
      ENDIF
C
C     CALCUL DE L'ORIGINE DU REPERE
C
      IF(IFRAM==0)THEN
        D13=(XX3-XX1)*XX4+(YY3-YY1)*YY4+(ZZ3-ZZ1)*ZZ4
        XXC=XX1+D13*XX4
        YYC=YY1+D13*YY4
        ZZC=ZZ1+D13*ZZ4
      ELSEIF(IFRAM==10)THEN
        X1N=X(1,N2)-X(1,N1)
        Y1N=X(2,N2)-X(2,N1)
        Z1N=X(3,N2)-X(3,N1)
        AL4=SQRT(X1N**2+Y1N**2+Z1N**2)
        X1N=X1N/MAX(AL4,EM20)
        Y1N=Y1N/MAX(AL4,EM20)
        Z1N=Z1N/MAX(AL4,EM20)
        D13=(X(1,N3)-X(1,N1))*X1N
     .     +(X(2,N3)-X(2,N1))*Y1N
     .     +(X(3,N3)-X(3,N1))*Z1N
        XXC=X(1,N1)+D13*X1N
        YYC=X(2,N1)+D13*Y1N
        ZZC=X(3,N1)+D13*Z1N
      ELSEIF(MOD(IFRAM,10)==1)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
Cel attention : il faut penser a maj de X avant forint
        DO I=1,NNOD
          N=NOD(I)
          XXC=XXC+X(1,N)
          YYC=YYC+X(2,N)
          ZZC=ZZC+X(3,N)
        ENDDO
        XXC=XXC/NNOD
        YYC=YYC/NNOD
        ZZC=ZZC/NNOD
      ELSEIF(MOD(IFRAM,10)==2)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
        MAS=1.E-30
Cel attention : il faut penser a maj de X & M avant forint
        DO I=1,NNOD
          N=NOD(I)
          XXC=XXC+X(1,N)*MS(N)
          YYC=YYC+X(2,N)*MS(N)
          ZZC=ZZC+X(3,N)*MS(N)
          MAS=MAS+MS(N)
        ENDDO
        XXC=XXC/MAS
        YYC=YYC/MAS
        ZZC=ZZC/MAS
      ELSEIF(MOD(IFRAM,10)==3)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SEC_SKEWP                     source/tools/sect/sectio.F    
Chd|-- called by -----------
Chd|        SECTIO                        source/tools/sect/sectio.F    
Chd|        SECTIO3N                      source/tools/sect/sectio.F    
Chd|        SECTIOC                       source/tools/sect/sectio.F    
Chd|        SECTIOP                       source/tools/sect/sectio.F    
Chd|        SECTIOR                       source/tools/sect/sectio.F    
Chd|        SECTIOS                       source/tools/sect/sectio.F    
Chd|        SECTIOS4                      source/tools/sect/sectio.F    
Chd|        SECTIOS6                      source/tools/sect/sectio.F    
Chd|        SECTIOT                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SEC_SKEWP(
     1   XXC, YYC, ZZC, XX4  , YY4 , ZZ4 , 
     2   XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3   XXN, YYN, ZZN, IFRAM, N1  , XSEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFRAM, N1
      my_real
     .   XXC, YYC, ZZC,
     .   XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     .   XXN, YYN, ZZN, XSEC(4,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
      my_real
     .   XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   AL4, AL6, AL5, D13, MAS, X1N, Y1N, Z1N
C
C
      IF(IFRAM<10)THEN
C      CALCUL DES MOMEMTS/REPERE N1 N2 N3
       XX1=XSEC(1,1)
       YY1=XSEC(1,2)
       ZZ1=XSEC(1,3)
       XX2=XSEC(2,1)
       YY2=XSEC(2,2)
       ZZ2=XSEC(2,3)
       XX3=XSEC(3,1)
       YY3=XSEC(3,2)
       ZZ3=XSEC(3,3)
       XX4=XX2-XX1
       YY4=YY2-YY1
       ZZ4=ZZ2-ZZ1
       AL4=SQRT(XX4**2+YY4**2+ZZ4**2)
       XX4=XX4/MAX(AL4,EM20)
       YY4=YY4/MAX(AL4,EM20)
       ZZ4=ZZ4/MAX(AL4,EM20)
       XX5=XX3-XX1
       YY5=YY3-YY1
       ZZ5=ZZ3-ZZ1
       XX6=YY4*ZZ5-ZZ4*YY5
       YY6=ZZ4*XX5-XX4*ZZ5
       ZZ6=XX4*YY5-YY4*XX5
       AL6=SQRT(XX6**2+YY6**2+ZZ6**2)
       XX6=XX6/MAX(AL6,EM20)
       YY6=YY6/MAX(AL6,EM20)
       ZZ6=ZZ6/MAX(AL6,EM20)
       XX5=YY6*ZZ4-ZZ6*YY4
       YY5=ZZ6*XX4-XX6*ZZ4
       ZZ5=XX6*YY4-YY6*XX4
       AL5=SQRT(XX5**2+YY5**2+ZZ5**2)
       XX5=XX5/MAX(AL5,EM20)
       YY5=YY5/MAX(AL5,EM20)
       ZZ5=ZZ5/MAX(AL5,EM20)
C
       XXN=XX6
       YYN=YY6
       ZZN=ZZ6
C
       ELSEIF (N1/=0) THEN
C      CALCUL DES MOMEMTS/REPERE GLOBAL
       XX4=XSEC(2,1)-XSEC(1,1)
       YY4=XSEC(2,2)-XSEC(1,2)
       ZZ4=XSEC(2,3)-XSEC(1,3)
       XX5=XSEC(3,1)-XSEC(1,1)
       YY5=XSEC(3,2)-XSEC(1,2)
       ZZ5=XSEC(3,3)-XSEC(1,3)
       XXN=YY4*ZZ5-ZZ4*YY5
       YYN=ZZ4*XX5-XX4*ZZ5
       ZZN=XX4*YY5-YY4*XX5
       AL6=SQRT(XXN**2+YYN**2+ZZN**2)
       XXN=XXN/MAX(AL6,EM20)
       YYN=YYN/MAX(AL6,EM20)
       ZZN=ZZN/MAX(AL6,EM20)
C
       XX4=ONE
       YY4=ZERO
       ZZ4=ZERO
       XX5=ZERO
       YY5=ONE
       ZZ5=ZERO
       XX6=ZERO
       YY6=ZERO
       ZZ6=ONE     
       ELSE
       XXN=ZERO
       YYN=ZERO
       ZZN=ONE
       XX4=ONE
       YY4=ZERO
       ZZ4=ZERO
       XX5=ZERO
       YY5=ONE
       ZZ5=ZERO
       XX6=ZERO
       YY6=ZERO
       ZZ6=ONE    
C
      ENDIF
C
C     CALCUL DE L'ORIGINE DU REPERE
C
      IF(IFRAM==0)THEN
        D13=(XX3-XX1)*XX4+(YY3-YY1)*YY4+(ZZ3-ZZ1)*ZZ4
        XXC=XX1+D13*XX4
        YYC=YY1+D13*YY4
        ZZC=ZZ1+D13*ZZ4
      ELSEIF(IFRAM==10)THEN
        X1N=XSEC(2,1)-XSEC(1,1)
        Y1N=XSEC(2,2)-XSEC(1,2)
        Z1N=XSEC(2,3)-XSEC(1,3)
        AL4=SQRT(X1N**2+Y1N**2+Z1N**2)
        X1N=X1N/MAX(AL4,EM20)
        Y1N=Y1N/MAX(AL4,EM20)
        Z1N=Z1N/MAX(AL4,EM20)
        D13=(XSEC(3,1)-XSEC(1,1))*X1N
     .     +(XSEC(3,2)-XSEC(1,2))*Y1N
     .     +(XSEC(3,3)-XSEC(1,3))*Z1N
        XXC=XSEC(1,1)+D13*X1N
        YYC=XSEC(1,2)+D13*Y1N
        ZZC=XSEC(1,3)+D13*Z1N
      ELSEIF(MOD(IFRAM,10)==1)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
C        DO I=1,NNOD
C             N=NOD(I)
C             XXC=XXC+X(1,N)
C             YYC=YYC+X(2,N)
C             ZZC=ZZC+X(3,N)
C        ENDDO
C Deja fait dans spmd_exch_sec
        XXC=XSEC(4,1)
        YYC=XSEC(4,2)
        ZZC=XSEC(4,3)
      ELSEIF(MOD(IFRAM,10)==2)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
        MAS=1.E-30
C        DO I=1,NNOD
C             N=NOD(I)
C             XXC=XXC+X(1,N)*MS(N)
C             YYC=YYC+X(2,N)*MS(N)
C             ZZC=ZZC+X(3,N)*MS(N)
C             MAS=MAS+MS(N)
C        ENDDO
C Deja fait dans spmd_exch_sec
        XXC=XSEC(4,1)
        YYC=XSEC(4,2)
        ZZC=XSEC(4,3)
      ELSEIF(MOD(IFRAM,10)==3)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
      ENDIF
C
      RETURN
      END
