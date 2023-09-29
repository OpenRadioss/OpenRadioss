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
Chd|  SECTIO                        source/tools/sect/sectio.F
Chd|-- called by -----------
Chd|        SECT_IO                       source/tools/sect/sectio.F
Chd|-- calls ---------------
Chd|        SEC_SKEW                      source/tools/sect/sectio.F
Chd|        SEC_SKEWP                     source/tools/sect/sectio.F
Chd|====================================================================
      SUBROUTINE SECTION(NNOD,N1  ,N2   ,N3   ,NSTRF  ,X  ,
     2                   V   ,VR  ,FSAV ,FOPTA,SECFCUM,MS ,
     3                   IN  ,IFRAM,XSEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNOD,N1, N2, N3,IFRAM
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
        CALL SECTION_SKEW(N1 ,N2 ,N3 ,X , XXC, YYC, ZZC,
     2    XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     3    XXN, YYN, ZZN,IFRAM,NNOD,NSTRF,MS)
       ELSE
         CALL SECTION_SKEWP(XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
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
