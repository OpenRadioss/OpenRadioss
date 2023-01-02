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
Cel|         INTTI12F                        /interf/intti12.F
Chd|====================================================================
Chd|  I12FOR3                       source/interfaces/interf/i12for3.F
Chd|-- called by -----------
Chd|        INTTI12F                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I12FOR3(
     1  NSN  ,NMN  ,A      ,IRECT,CRST  ,
     2  MSR  ,NSV  ,IRTL   ,MS   ,WEIGHT,
     3  STIFN,MMASS,TAGKINE,SKEW ,WA    ,
     4  TETS , TETM,ILEV   ,IREF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),TAGKINE(*)
      INTEGER ILEV,IREF
      my_real
     .   A(*), CRST(2,*), MS(*), STIFN(*), MMASS(*),WA(3,*),
     .   SKEW(LSKEW,*),TETM(*),TETS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, II, L, JJ,JL
      my_real
     .   H(4), XMSJ, SS, TT, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,
     .   P(9),CST,SST,FXR, FYR, FZR
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
      IF(ILEV==1)THEN
        DO I=1,9
          P(I)=SKEW(I,IREF+1)
        ENDDO
        DO II=1,NMN
          WA(1,II)=ZERO
          WA(2,II)=ZERO
          WA(3,II)=ZERO
        ENDDO
      ENDIF

       !Cel sauvegarde de la masse initiale
        DO II=1,NMN
          J=MSR(II)
          MMASS(II)=MS(J)
        ENDDO

      DO II=1,NSN
        IF(TAGKINE(II)>0)THEN
          I=NSV(II)
          L=IRTL(II)
          SS=CRST(1,II)
          TT=CRST(2,II)
          I3=3*I
          I2=I3-1
          I1=I2-1
          XMSI=MS(I)
          FXI=A(I1)
          FYI=A(I2)
          FZI=A(I3)
          SP=ONE+SS
          SM=ONE-SS
          TP=FOURTH*(ONE+TT)
          TM=FOURTH*(ONE-TT)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM
          IF(ILEV==1)THEN
           IF(TETS(II)<10000. )THEN
            CST=COS(TETS(II))
            SST=SIN(TETS(II))
            FXR=FXI*P(1)+FYI*P(2)+FZI*P(3)
            FYR=FXI*P(4)+FYI*P(5)+FZI*P(6)
            FZR=FXI*P(7)+FYI*P(8)+FZI*P(9)
            FXI=FXR
            FYI=    FYR*CST+FZR*SST
            FZI=   -FYR*SST+FZR*CST
            DO JJ=1,NIR
              JL=IRECT(JJ,L)
              J=MSR(JL)
              WA(1,JL)=WA(1,JL)+FXI*H(JJ)
              WA(2,JL)=WA(2,JL)+FYI*H(JJ)
              WA(3,JL)=WA(3,JL)+FZI*H(JJ)
              MS(J)=MS(J)+XMSI*H(JJ)
              STIFN(J)=STIFN(J)+STIFN(I)*H(JJ)
            ENDDO
           ELSE
            DO JJ=1,NIR
              J=MSR(IRECT(JJ,L))
              J3=3*J
              J2=J3-1
              J1=J2-1
              A(J1)=A(J1)+FXI*H(JJ)
              A(J2)=A(J2)+FYI*H(JJ)
              A(J3)=A(J3)+FZI*H(JJ)
              MS(J)=MS(J)+XMSI*H(JJ)
              STIFN(J)=STIFN(J)+STIFN(I)*H(JJ)
            ENDDO
           ENDIF
          ELSE
            DO JJ=1,NIR
              J=MSR(IRECT(JJ,L))
              J3=3*J
              J2=J3-1
              J1=J2-1
              A(J1)=A(J1)+FXI*H(JJ)
              A(J2)=A(J2)+FYI*H(JJ)
              A(J3)=A(J3)+FZI*H(JJ)
              MS(J)=MS(J)+XMSI*H(JJ)
              STIFN(J)=STIFN(J)+STIFN(I)*H(JJ)

            ENDDO
          ENDIF !(ILEV==1)
        STIFN(I)=EM20
        A(I1)=ZERO
        A(I2)=ZERO
        A(I3)=ZERO
        ENDIF
      ENDDO
      
      IF(ILEV==1 )THEN
        DO II=1,NMN
          CST=COS(TETM(II))
          SST=SIN(TETM(II))
          FXR=WA(1,II)
          FYR=WA(2,II)*CST-WA(3,II)*SST
          FZR=WA(2,II)*SST+WA(3,II)*CST
          WA(1,II)=FXR*P(1)+FYR*P(4)+FZR*P(7)
          WA(2,II)=FXR*P(2)+FYR*P(5)+FZR*P(8)
          WA(3,II)=FXR*P(3)+FYR*P(6)+FZR*P(9)
          J=MSR(II)
          J3=3*J
          J2=J3-1
          J1=J2-1
          A(J1)=A(J1)+WA(1,II)
          A(J2)=A(J2)+WA(2,II)
          A(J3)=A(J3)+WA(3,II)
        ENDDO
      ENDIF

      RETURN
      END
