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
Chd|  I7NORM                        source/interfaces/int07/i7curv.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7NORM(NRTM,IRECT,NUMNOD,X,NOD_NORMAL,
     .                  NMN ,MSR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NRTM,NUMNOD,IRECT(4,NRTM),NMN,MSR(*)
C     REAL
        my_real
     .     X(3,NUMNOD), NOD_NORMAL(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,J ,N1,N2,N3,N4
        my_real
     .     SURFX,SURFY,SURFZ,X13,Y13,Z13,X24,Y24,Z24
C-----------------------------------------------

C optimisable en spmd si ajout flag pour routine de comm, spmd_exchange_n
        DO N1=1,NUMNOD
          NOD_NORMAL(1,N1) = ZERO
          NOD_NORMAL(2,N1) = ZERO
          NOD_NORMAL(3,N1) = ZERO
        END DO

        DO I=1,NRTM
          N1 = IRECT(1,I)
          N2 = IRECT(2,I)
          N3 = IRECT(3,I)
          N4 = IRECT(4,I)

          X13 = X(1,N3) - X(1,N1)
          Y13 = X(2,N3) - X(2,N1)
          Z13 = X(3,N3) - X(3,N1)

          X24 = X(1,N4) - X(1,N2)
          Y24 = X(2,N4) - X(2,N2)
          Z24 = X(3,N4) - X(3,N2)

          SURFX = Y13*Z24 - Z13*Y24
          SURFY = Z13*X24 - X13*Z24
          SURFZ = X13*Y24 - Y13*X24

          NOD_NORMAL(1,N1) = NOD_NORMAL(1,N1) + SURFX
          NOD_NORMAL(2,N1) = NOD_NORMAL(2,N1) + SURFY
          NOD_NORMAL(3,N1) = NOD_NORMAL(3,N1) + SURFZ
          NOD_NORMAL(1,N2) = NOD_NORMAL(1,N2) + SURFX
          NOD_NORMAL(2,N2) = NOD_NORMAL(2,N2) + SURFY
          NOD_NORMAL(3,N2) = NOD_NORMAL(3,N2) + SURFZ
          NOD_NORMAL(1,N3) = NOD_NORMAL(1,N3) + SURFX
          NOD_NORMAL(2,N3) = NOD_NORMAL(2,N3) + SURFY
          NOD_NORMAL(3,N3) = NOD_NORMAL(3,N3) + SURFZ
          NOD_NORMAL(1,N4) = NOD_NORMAL(1,N4) + SURFX
          NOD_NORMAL(2,N4) = NOD_NORMAL(2,N4) + SURFY
          NOD_NORMAL(3,N4) = NOD_NORMAL(3,N4) + SURFZ
        ENDDO

        RETURN
      END
C
Chd|====================================================================
Chd|  I7NORMP                       source/interfaces/int07/i7curv.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPMD_I7CURVCOM                source/mpi/interfaces/spmd_i7curvcom.F
Chd|====================================================================
      SUBROUTINE I7NORMP(NRTM  ,IRECT   ,NUMNOD ,X    ,NOD_NORMAL,
     .                   NMN   ,MSR     ,LENT   ,MAXCC,ISDSIZ    ,
     .                   IRCSIZ,IAD_ELEM,FR_ELEM,ITAG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NRTM,NUMNOD,NMN,MAXCC,LENT,
     .          IRECT(4,NRTM),MSR(*),
     .          IAD_ELEM(2,*),FR_ELEM(*),ISDSIZ(*),IRCSIZ(*),ITAG(*)
C     REAL
        my_real
     .     X(3,NUMNOD), NOD_NORMAL(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,J ,N1,N2,N3,N4, IAD, LENR, LENS, CC, ERROR,
     .          ADSKYT(0:NUMNOD+1)
        my_real
     .          SURFX,SURFY,SURFZ,X13,Y13,Z13,X24,Y24,Z24,
     .          FSKYT(3,LENT), FSKYT2(MAXCC), PERM(MAXCC)
C-----------------------------------------------
        ADSKYT(0) = 1
        ADSKYT(1) = 1
        DO N1=1,NUMNOD
          ADSKYT(N1+1) = ADSKYT(N1)+ITAG(N1)
          ITAG(N1) = ADSKYT(N1)
          NOD_NORMAL(1,N1) = ZERO
          NOD_NORMAL(2,N1) = ZERO
          NOD_NORMAL(3,N1) = ZERO
        END DO

        DO I=1,NRTM
          N1 = IRECT(1,I)
          N2 = IRECT(2,I)
          N3 = IRECT(3,I)
          N4 = IRECT(4,I)

          X13 = X(1,N3) - X(1,N1)
          Y13 = X(2,N3) - X(2,N1)
          Z13 = X(3,N3) - X(3,N1)

          X24 = X(1,N4) - X(1,N2)
          Y24 = X(2,N4) - X(2,N2)
          Z24 = X(3,N4) - X(3,N2)

          SURFX = Y13*Z24 - Z13*Y24
          SURFY = Z13*X24 - X13*Z24
          SURFZ = X13*Y24 - Y13*X24

          IAD = ADSKYT(N1)
          ADSKYT(N1) = ADSKYT(N1)+1
          FSKYT(1,IAD) =  SURFX
          FSKYT(2,IAD) =  SURFY
          FSKYT(3,IAD) =  SURFZ
          IAD = ADSKYT(N2)
          ADSKYT(N2) = ADSKYT(N2)+1
          FSKYT(1,IAD) =  SURFX
          FSKYT(2,IAD) =  SURFY
          FSKYT(3,IAD) =  SURFZ
          IAD = ADSKYT(N3)
          ADSKYT(N3) = ADSKYT(N3)+1
          FSKYT(1,IAD) =  SURFX
          FSKYT(2,IAD) =  SURFY
          FSKYT(3,IAD) =  SURFZ
          IAD = ADSKYT(N4)
          ADSKYT(N4) = ADSKYT(N4)+1
          FSKYT(1,IAD) =  SURFX
          FSKYT(2,IAD) =  SURFY
          FSKYT(3,IAD) =  SURFZ
        END DO
C
        IF(NSPMD>1) THEN
          LENR = IRCSIZ(NSPMD+1)*3+IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          LENS = ISDSIZ(NSPMD+1)*3+IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_I7CURVCOM(IAD_ELEM,FR_ELEM,ADSKYT,FSKYT,
     .                        ISDSIZ,IRCSIZ,ITAG  ,LENR    ,LENS   )
        END IF
C
C     tri par packet des normales
C
        DO N1 = 1, NUMNOD
          N2 = ADSKYT(N1-1)
          N3 = ADSKYT(N1)-1
          N4 = N3-N2+1
          IF(N4>1)THEN        ! cas N contribution => tri
            DO J = 1, 3
              DO CC = N2, N3
                FSKYT2(CC-N2+1) = FSKYT(J,CC)
              END DO
C            IF(N4>MAXCC)print*,'error cc:',n4,maxcc
              CALL MYQSORT(N4,FSKYT2,PERM,ERROR)
              DO CC = N2, N3
                NOD_NORMAL(J,N1) = NOD_NORMAL(J,N1) + FSKYT2(CC-N2+1)
              END DO
            END DO
          ELSEIF(N4==1)THEN    ! cas 1 seule contribution => direct
            NOD_NORMAL(1,N1) = FSKYT(1,N2)
            NOD_NORMAL(2,N1) = FSKYT(2,N2)
            NOD_NORMAL(3,N1) = FSKYT(3,N2)
          END IF
        END DO
C
        RETURN
      END
C
Chd|====================================================================
Chd|  I7CURVSZ                      source/interfaces/int07/i7curv.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7CURVSZ(NRTM,IRECT,NUMNOD,ITAG,LENT,MAXCC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NRTM,NUMNOD,LENT,MAXCC,
     .          IRECT(4,NRTM),ITAG(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,J ,N1, N2, N3, N4
C-----------------------------------------------
C
        DO N1=1,NUMNOD
          ITAG(N1) = 0
        END DO

        DO I=1,NRTM
          N1 = IRECT(1,I)
          N2 = IRECT(2,I)
          N3 = IRECT(3,I)
          N4 = IRECT(4,I)
          ITAG(N1) = ITAG(N1) + 1
          ITAG(N2) = ITAG(N2) + 1
          ITAG(N3) = ITAG(N3) + 1
          ITAG(N4) = ITAG(N4) + 1
        END DO
C
        LENT = 0
        MAXCC = 0
        DO N1=1,NUMNOD
          LENT = LENT + ITAG(N1)
          MAXCC = MAX(MAXCC,ITAG(N1))
        END DO
C
        RETURN
      END

Chd|====================================================================
Chd|  I7CMAJ                        source/interfaces/int07/i7curv.F
Chd|-- called by -----------
Chd|        I7DST3                        source/interfaces/int07/i7dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7CMAJ(JLT    ,CMAJ   ,IRECT  ,NOD_NORMAL,CAND_E,
     2                  X1     ,X2     ,X3     ,X4        ,
     3                  Y1     ,Y2     ,Y3     ,Y4        ,
     4                  Z1     ,Z2     ,Z3     ,Z4        ,
     5                  NNX1   ,NNX2   ,NNX3   ,NNX4      ,
     6                  NNY1   ,NNY2   ,NNY3   ,NNY4      ,
     7                  NNZ1   ,NNZ2   ,NNZ3   ,NNZ4      )
c
c             + tmaj
c            / \
c       tmax/__ \
c          /   \_\
c         0-------0---> r
c
c
c       borne max:
c          (r+1)t1' = (r-1)t'2
c          r = - (t1' + t'2)/(t1'-t'2)
c          r = e
c          tmaj = (e+1)t1' = (e-1)t'2
c    => en 2D      t = t'max => Z = (Lmax/2)t'max
c
c       vecteurs:Z,L,N
c            Z^2 = (L/2.N)^2(L/2)^2 / (N^2(L/2)^2 - (L/2.N)^2)
c            Z^2 = (L.N)^2 L^2 / 4(N^2 L^2 - (L.N)^2)
c
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
        INTEGER JLT ,IRECT(4,*),CAND_E(*)
C     REAL
        my_real
     .      NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .      NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .      NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .       X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .       Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .       Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .       CMAJ(MVSIZ),NOD_NORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I , IX,L
        my_real
     .     X12,Y12,Z12,X23,Y23,Z23,X34,Y34,Z34,X41,Y41,Z41,
     .     L12L12,L23L23,L34L34,L41L41,N1N1,N2N2,N3N3,N4N4,NL,
     .     N1L12N1L12,N2L12N2L12,N2L23N2L23,N3L23N3L23,
     .     N3L34N3L34,N4L34N4L34,N4L41N4L41,N1L41N1L41,AAA
C-----------------------------------------------

        DO I=1,JLT
          L  = CAND_E(I)

          IX=IRECT(1,L)
          NNX1(I)=NOD_NORMAL(1,IX)
          NNY1(I)=NOD_NORMAL(2,IX)
          NNZ1(I)=NOD_NORMAL(3,IX)

          IX=IRECT(2,L)
          NNX2(I)=NOD_NORMAL(1,IX)
          NNY2(I)=NOD_NORMAL(2,IX)
          NNZ2(I)=NOD_NORMAL(3,IX)

          IX=IRECT(3,L)
          NNX3(I)=NOD_NORMAL(1,IX)
          NNY3(I)=NOD_NORMAL(2,IX)
          NNZ3(I)=NOD_NORMAL(3,IX)

          IX=IRECT(4,L)
          NNX4(I)=NOD_NORMAL(1,IX)
          NNY4(I)=NOD_NORMAL(2,IX)
          NNZ4(I)=NOD_NORMAL(3,IX)

          X12 = X2(I) - X1(I)
          Y12 = Y2(I) - Y1(I)
          Z12 = Z2(I) - Z1(I)

          X23 = X3(I) - X2(I)
          Y23 = Y3(I) - Y2(I)
          Z23 = Z3(I) - Z2(I)

          X34 = X4(I) - X3(I)
          Y34 = Y4(I) - Y3(I)
          Z34 = Z4(I) - Z3(I)

          X41 = X1(I) - X4(I)
          Y41 = Y1(I) - Y4(I)
          Z41 = Z1(I) - Z4(I)

          L12L12 = X12*X12 + Y12*Y12 + Z12*Z12
          L23L23 = X23*X23 + Y23*Y23 + Z23*Z23
          L34L34 = X34*X34 + Y34*Y34 + Z34*Z34
          L41L41 = X41*X41 + Y41*Y41 + Z41*Z41

          N1N1   = NNX1(I)*NNX1(I)
     +           + NNY1(I)*NNY1(I)
     +           + NNZ1(I)*NNZ1(I)
          N2N2   = NNX2(I)*NNX2(I)
     +           + NNY2(I)*NNY2(I)
     +           + NNZ2(I)*NNZ2(I)
          N3N3   = NNX3(I)*NNX3(I)
     +           + NNY3(I)*NNY3(I)
     +           + NNZ3(I)*NNZ3(I)
          N4N4   = NNX4(I)*NNX4(I)
     +           + NNY4(I)*NNY4(I)
     +           + NNZ4(I)*NNZ4(I)

          NL  = NNX1(I)*X12
     +        + NNY1(I)*Y12
     +        + NNZ1(I)*Z12
          N1L12N1L12 = NL*NL
          NL = NNX2(I)*X12
     +       + NNY2(I)*Y12
     +       + NNZ2(I)*Z12
          N2L12N2L12  = NL*NL
          NL = NNX2(I)*X23
     +       + NNY2(I)*Y23
     +       + NNZ2(I)*Z23
          N2L23N2L23 = NL*NL
          NL = NNX3(I)*X23
     +       + NNY3(I)*Y23
     +       + NNZ3(I)*Z23
          N3L23N3L23  = NL*NL
          NL = NNX3(I)*X34
     +       + NNY3(I)*Y34
     +       + NNZ3(I)*Z34
          N3L34N3L34 = NL*NL
          NL = NNX4(I)*X34
     +       + NNY4(I)*Y34
     +       + NNZ4(I)*Z34
          N4L34N4L34 = NL*NL
          NL = NNX4(I)*X41
     +       + NNY4(I)*Y41
     +       + NNZ4(I)*Z41
          N4L41N4L41 = NL*NL
          NL = NNX1(I)*X41
     +       + NNY1(I)*Y41
     +       + NNZ1(I)*Z41
          N1L41N1L41 = NL*NL

c            Z^2 = (L.N)^2 L^2 / 4(N^2 L^2 - (L.N)^2)
          AAA = MAX(N1L12N1L12*L12L12 / (N1N1*L12L12 - N1L12N1L12),
     .              N2L12N2L12*L12L12 / (N2N2*L12L12 - N2L12N2L12),
     .              N2L23N2L23*L23L23 / (N2N2*L23L23 - N2L23N2L23),
     .              N3L23N3L23*L23L23 / (N3N3*L23L23 - N3L23N3L23),
     .              N3L34N3L34*L34L34 / (N3N3*L34L34 - N3L34N3L34),
     .              N4L34N4L34*L34L34 / (N4N4*L34L34 - N4L34N4L34),
     .              N4L41N4L41*L41L41 / (N4N4*L41L41 - N4L41N4L41),
     .              N1L41N1L41*L41L41 / (N1N1*L41L41 - N1L41N1L41))
          CMAJ(I) = HALF*SQRT(AAA)

        ENDDO

        RETURN
      END
Chd|====================================================================
Chd|  I7CURV                        source/interfaces/int07/i7curv.F
Chd|-- called by -----------
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I7FOR3                        source/interfaces/int07/i7for3.F
Chd|-- calls ---------------
Chd|        I7CUBIC                       source/interfaces/int07/i7curv.F
Chd|====================================================================
      SUBROUTINE I7CURV(JLT    ,PENE   ,N1     ,N2        ,
     1                  N3     ,GAPV   ,X      ,NOD_NORMAL,
     2                  IX1    ,IX2    ,IX3    ,IX4       ,
     3                  H1     ,H2     ,H3     ,H4        ,
     4                  X1     ,X2     ,X3     ,X4    ,Y1   ,
     5                  Y2     ,Y3     ,Y4     ,Z1    ,Z2   ,
     6                  Z3     ,Z4     ,XI     ,YI    ,ZI   )
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
        INTEGER JLT,
     .    IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ)
C     REAL
        my_real
     .      X(3,*),NOD_NORMAL(3,*),
     .      H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .      PENE(MVSIZ),GAPV(MVSIZ),N1(MVSIZ),N2(MVSIZ),N3(MVSIZ),
     .      X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .      Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .      Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .      XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
        my_real
     .     XD, YD, ZD,
     .     NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .     NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .     NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .     X12(MVSIZ),Y12(MVSIZ),Z12(MVSIZ),
     .     X43(MVSIZ),Y43(MVSIZ),Z43(MVSIZ),
     .     X23(MVSIZ),Y23(MVSIZ),Z23(MVSIZ),
     .     X14(MVSIZ),Y14(MVSIZ),Z14(MVSIZ),
     .     NNX12(MVSIZ),NNY12(MVSIZ),NNZ12(MVSIZ),
     .     NNX43(MVSIZ),NNY43(MVSIZ),NNZ43(MVSIZ),
     .     NNX23(MVSIZ),NNY23(MVSIZ),NNZ23(MVSIZ),
     .     NNX14(MVSIZ),NNY14(MVSIZ),NNZ14(MVSIZ),
     .     XA(MVSIZ),YA(MVSIZ),ZA(MVSIZ),
     .     XB(MVSIZ),YB(MVSIZ),ZB(MVSIZ),
     .     NNXA(MVSIZ),NNYA(MVSIZ),NNZA(MVSIZ),
     .     NNXB(MVSIZ),NNYB(MVSIZ),NNZB(MVSIZ),
     .     CSX(MVSIZ),CSY(MVSIZ),CSZ(MVSIZ),
     .     CRX(MVSIZ),CRY(MVSIZ),CRZ(MVSIZ),
     .     R(MVSIZ),S(MVSIZ),TT(MVSIZ),TA(MVSIZ),TB(MVSIZ),AAA,
     .     BBB,CCC,DDD,EEE,RM,RP,SM,SP,RPMM,SPMM,RPPM,SPPM,
     .     NX,NY,NZ,DIST,EPS,XP,YP,ZP,XX,YY,ZZ,XXP,YYP,ZZP,A,B
C-----------------------------------------------
C     Interpolation bicubique
c     avec continuit  des coordonn s,
c     des d riv s tagente et normale sur les bords
C-----------------------------------------------

        DO I=1,JLT
          NNX1(I)=NOD_NORMAL(1,IX1(I))
          NNY1(I)=NOD_NORMAL(2,IX1(I))
          NNZ1(I)=NOD_NORMAL(3,IX1(I))

          NNX2(I)=NOD_NORMAL(1,IX2(I))
          NNY2(I)=NOD_NORMAL(2,IX2(I))
          NNZ2(I)=NOD_NORMAL(3,IX2(I))

          NNX3(I)=NOD_NORMAL(1,IX3(I))
          NNY3(I)=NOD_NORMAL(2,IX3(I))
          NNZ3(I)=NOD_NORMAL(3,IX3(I))

          NNX4(I)=NOD_NORMAL(1,IX4(I))
          NNY4(I)=NOD_NORMAL(2,IX4(I))
          NNZ4(I)=NOD_NORMAL(3,IX4(I))

C-----------------------------------------------
c       calcul de r,s   partir des Hi
C-----------------------------------------------
          RM = (H2(I)-H1(I))/MAX(EM20,H2(I)+H1(I))
          RP = (H3(I)-H4(I))/MAX(EM20,H3(I)+H4(I))
          EPS = EM4
          IF(H2(I)+H1(I)<EPS) RM = RP
          IF(H3(I)+H4(I)<EPS) RP = RM

          SM = (H4(I)-H1(I))/MAX(EM20,H4(I)+H1(I))
          SP = (H3(I)-H2(I))/MAX(EM20,H3(I)+H2(I))
          IF(H4(I)+H1(I)<EPS) SM = SP
          IF(H3(I)+H2(I)<EPS) SP = SM

          RPMM = RP - RM
          SPMM = SP - SM
          AAA = FOUR  - RPMM*SPMM
          RPPM = RP + RM
          SPPM = SP + SM

          R(I)= (RPPM + RPPM + SPPM*RPMM) / AAA
          S(I)= (SPPM + SPPM + RPPM*SPMM) / AAA

        ENDDO
cote 12
        CALL I7CUBIC(JLT,R,TT,
     .             X1,NNX1,X2,NNX2,X12,NNX12,CSX,
     .             Y1,NNY1,Y2,NNY2,Y12,NNY12,CSY,
     .             Z1,NNZ1,Z2,NNZ2,Z12,NNZ12,CSZ)
cote 34
        CALL I7CUBIC(JLT,R,TT,
     .             X4,NNX4,X3,NNX3,X43,NNX43,CSX,
     .             Y4,NNY4,Y3,NNY3,Y43,NNY43,CSY,
     .             Z4,NNZ4,Z3,NNZ3,Z43,NNZ43,CSZ)
c ligne s
        CALL I7CUBIC(JLT,S,TA,
     .             X12,NNX12,X43,NNX43,XA,NNXA,CSX,
     .             Y12,NNY12,Y43,NNY43,YA,NNYA,CSY,
     .             Z12,NNZ12,Z43,NNZ43,ZA,NNZA,CSZ)
cote 14
        CALL I7CUBIC(JLT,S,TT,
     .             X1,NNX1,X4,NNX4,X14,NNX14,CRX,
     .             Y1,NNY1,Y4,NNY4,Y14,NNY14,CRY,
     .             Z1,NNZ1,Z4,NNZ4,Z14,NNZ14,CRZ)
cote 23
        CALL I7CUBIC(JLT,S,TT,
     .             X2,NNX2,X3,NNX3,X23,NNX23,CRX,
     .             Y2,NNY2,Y3,NNY3,Y23,NNY23,CRY,
     .             Z2,NNZ2,Z3,NNZ3,Z23,NNZ23,CRZ)
c ligne r
        CALL I7CUBIC(JLT,R,TB,
     .             X14,NNX14,X23,NNX23,XB,NNXB,CRX,
     .             Y14,NNY14,Y23,NNY23,YB,NNYB,CRY,
     .             Z14,NNZ14,Z23,NNZ23,ZB,NNZB,CRZ)

        DO I=1,JLT

C-----------------------------------------------
c       normale   la surface cubique en r,s
c       produit vectoriel des vecteurs directeurs cr et cs
C-----------------------------------------------
          NX = CRY(I)*CSZ(I)-CRZ(I)*CSY(I)
          NY = CRZ(I)*CSX(I)-CRX(I)*CSZ(I)
          NZ = CRX(I)*CSY(I)-CRY(I)*CSX(I)
          AAA = ONE / SQRT(NX*NX+NY*NY+NZ*NZ)
          NX = NX*AAA
          NY = NY*AAA
          NZ = NZ*AAA

C-----------------------------------------------
c       point calcul  sur la surface cubique (r,s)
C-----------------------------------------------
          XA(I) = HALF*(XA(I)+XB(I))
          YA(I) = HALF*(YA(I)+YB(I))
          ZA(I) = HALF*(ZA(I)+ZB(I))

C-----------------------------------------------
c       vecteur: point calcul  -> noeud second.
C-----------------------------------------------
          XD = XI(I) - XA(I)
          YD = YI(I) - YA(I)
          ZD = ZI(I) - ZA(I)
C-----------------------------------------------
c       distance: surface cubique -> noeud second.
C-----------------------------------------------
          DIST = NX*XD + NY*YD + NZ*ZD

C-----------------------------------------------
c       projection du noeud second. sur la surface cubique
C-----------------------------------------------
          XP = XI(I) - DIST*NX
          YP = YI(I) - DIST*NY
          ZP = ZI(I) - DIST*NZ
          XX = X43(I) - X12(I)
          YY = Y43(I) - Y12(I)
          ZZ = Z43(I) - Z12(I)

          XXP = XP - X12(I)
          YYP = YP - Y12(I)
          ZZP = ZP - Z12(I)

          BBB = XXP*XX + YYP*YY + ZZP*ZZ
          CCC = XX*XX + YY*YY + ZZ*ZZ

C        IF(BBB<ZERO.OR.BBB>CCC)THEN
C-----------------------------------------------
c         s<-1 ou s>1
c         on conserve la normale et la p n tration ancienne
C-----------------------------------------------
C        ELSE
          XX = X23(I) - X14(I)
          YY = Y23(I) - Y14(I)
          ZZ = Z23(I) - Z14(I)

          XXP = XP - X14(I)
          YYP = YP - Y14(I)
          ZZP = ZP - Z14(I)

          DDD = XXP*XX + YYP*YY + ZZP*ZZ
          EEE = XX*XX + YY*YY + ZZ*ZZ
C          IF(DDD<ZERO.OR.DDD>EEE)THEN
C-----------------------------------------------
c           r<-1 ou r>1
c           on conserve la normale et la p n tration ancienne
C-----------------------------------------------
C          ELSE
C-----------------------------------------------
c           -1<r<+1 et -1<s<+1
C-----------------------------------------------
c           r et s peuvent  tres recalcul s
c           R(I) = TWO*DDD/EEE - ONE
c           S(I) = TWO*BBB/CCC - ONE
C-----------------------------------------------
          IF(DIST > ZERO)THEN
            PENE(I) = GAPV(I) - DIST
            N1(I) = NX
            N2(I) = NY
            N3(I) = NZ
          ELSE
            PENE(I) = GAPV(I) + DIST
            N1(I) = -NX
            N2(I) = -NY
            N3(I) = -NZ
          ENDIF
C          ENDIF
C        ENDIF

        ENDDO

        RETURN
      END
Chd|====================================================================
Chd|  I7CUBIC                       source/interfaces/int07/i7curv.F
Chd|-- called by -----------
Chd|        I7CURV                        source/interfaces/int07/i7curv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7CUBIC(JLT,R,TT,
     .                   X1,NNX1,X2,NNX2,X12,NNX12,CX,
     .                   Y1,NNY1,Y2,NNY2,Y12,NNY12,CY,
     .                   Z1,NNZ1,Z2,NNZ2,Z12,NNZ12,CZ)
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
        INTEGER JLT
C     REAL
        my_real
     .     X1(MVSIZ), X2(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ),
     .     NNX1(MVSIZ), NNX2(MVSIZ),
     .     NNY1(MVSIZ), NNY2(MVSIZ),
     .     NNZ1(MVSIZ), NNZ2(MVSIZ),
     .     X12(MVSIZ),Y12(MVSIZ),Z12(MVSIZ),
     .     NNX12(MVSIZ),NNY12(MVSIZ),NNZ12(MVSIZ),
     .     CX(MVSIZ),CY(MVSIZ),CZ(MVSIZ),
     .     R(MVSIZ),TT(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K
        my_real
     .      AAA,A1,A2,A3,RR,DTDR,DTDR1,DTDR2,NR,NS,NT,NX,NY,NZ,R2,
     .      UPR2,UMR2,A3RR2,A2RR,
     .      ERX,ERY,ERZ,ESX,ESY,ESZ,ETX,ETY,ETZ
c---------------------------------------------------
c       vecteur directeur en r
c---------------------------------------------------
c
c         ^t
c         |  ___
c     N1^ | /   \    ^N2
c        \|/     \    \ 2
c       1 0       \___-0------->r
c        -1.          +1.
c         0.           R2
c
c   t = a3 rr^3 + a2 rr^2 + a1 rr + a0
c
c   t'r = 3 a3 rr^2 + 2 a2 rr + a1
c
c   rr=0   t = 0     t'r = dr1
c   rr=r2  t = 0     t'r = dr2
c
c   a0 = 0
c   0 =  a3 r2^2 + a2 r2 + a1
c   a1 = dr1
c   dr2 = 3 a3 r2^2 + 2 a2 r2 + a1
c
c   0 =   3 a3 r2^2 + 3 a2 r2 + 3 a1
c   dr2 = 3 a3 r2^2 + 2 a2 r2 + a1
c   a2 = - (dr2 + 2 a1)/r2
c   a3 = - (a2 r2 + a1)/r2^2
c
c   a0 =  0
c   a1 = dr1
c   a2 = - (dr2 + 2 dr1)/r2
c   a3 = + (dr2 + dr1  )/r2^2
c
        DO I=1,JLT
          ERX = X2(I)-X1(I)
          ERY = Y2(I)-Y1(I)
          ERZ = Z2(I)-Z1(I)

          R2  = SQRT(ERX*ERX+ERY*ERY+ERZ*ERZ)
          AAA = ONE / R2
          ERX = ERX*AAA
          ERY = ERY*AAA
          ERZ = ERZ*AAA

          ETX = NNX1(I)+NNX2(I)
          ETY = NNY1(I)+NNY2(I)
          ETZ = NNZ1(I)+NNZ2(I)

          AAA = ERX*ETX + ERY*ETY + ERZ*ETZ
          ETX = ETX - ERX*AAA
          ETY = ETY - ERY*AAA
          ETZ = ETZ - ERZ*AAA

          AAA = ONE / SQRT(ETX*ETX+ETY*ETY+ETZ*ETZ)
          ETX = ETX*AAA
          ETY = ETY*AAA
          ETZ = ETZ*AAA

          ESX = ETY*ERZ-ETZ*ERY
          ESY = ETZ*ERX-ETX*ERZ
          ESZ = ETX*ERY-ETY*ERX

c---------------------------------------------------
c       d riv  calcul e   partir des normales(non norm es)
c---------------------------------------------------
          DTDR1 = -(NNX1(I)*ERX+NNY1(I)*ERY+NNZ1(I)*ERZ)
     .          /  (NNX1(I)*ETX+NNY1(I)*ETY+NNZ1(I)*ETZ)

          DTDR2 = -(NNX2(I)*ERX+NNY2(I)*ERY+NNZ2(I)*ERZ)
     .          /  (NNX2(I)*ETX+NNY2(I)*ETY+NNZ2(I)*ETZ)
c---------------------------------------------------
c       la d riv  est born e   +- 1 pour limiter la taille
c       des boites dans i7tri
c---------------------------------------------------
          DTDR1 = DTDR1/MAX(ONE,ABS(DTDR1))
          DTDR2 = DTDR2/MAX(ONE,ABS(DTDR2))

          UPR2 = HALF*(ONE+R(I))
          UMR2 = HALF*(ONE-R(I))
          RR = UPR2*R2

c   a0 =  0
c   a1 = dr1
c   a2 = - (dr2 + 2 a1)/r2
c   a3 = - (a2 r2 + a1)/r2^2
c      A1 = DTDR1
c      A2 = -(DTDR2 + TWO*DTDR1)/R2
c      A3 = (DTDR2 + DTDR1)/R2/R2

c      TT = ((A3*RR + A2)*RR + A1)*RR
c      DTDR = (THREE*A3*RR + TWO*A2)*RR + A1

          A1 = DTDR1
          A2RR = -(DTDR2 + DTDR1 + DTDR1)*UPR2
          A3RR2 = (DTDR2 + DTDR1)*UPR2*UPR2

          TT(I) = ((A3RR2 + A2RR) + A1) * RR

c---------------------------------------------------
c       coordonn s en r
c---------------------------------------------------
          X12(I) = X1(I) + RR*ERX + TT(I)*ETX
          Y12(I) = Y1(I) + RR*ERY + TT(I)*ETY
          Z12(I) = Z1(I) + RR*ERZ + TT(I)*ETZ

          DTDR = THREE*A3RR2 + A2RR + A2RR + A1

c---------------------------------------------------
c       vecteur directeur en r
c---------------------------------------------------
          CX(I) = ERX + DTDR*ETX
          CY(I) = ERY + DTDR*ETY
          CZ(I) = ERZ + DTDR*ETZ

          NX = UMR2*NNX1(I) + UPR2*NNX2(I)
          NY = UMR2*NNY1(I) + UPR2*NNY2(I)
          NZ = UMR2*NNZ1(I) + UPR2*NNZ2(I)

          AAA = (NX*CX(I)+NY*CY(I)+NZ*CZ(I))
     .        / (CX(I)*CX(I)+CY(I)*CY(I)+CZ(I)*CZ(I))

c---------------------------------------------------
c       normale en r
c---------------------------------------------------
          NNX12(I) = NX - CX(I)*AAA
          NNY12(I) = NY - CY(I)*AAA
          NNZ12(I) = NZ - CZ(I)*AAA
        ENDDO

        RETURN
      END
c
c         ^t
c         |  ___
c     N1^ | /   \    ^N2
c        \|/     \    \
c         0       \___-0------->r
c
c         1            2
c
c
c   t = a3 rr^3 + a2 rr^2 + a1 rr + a0
c
c   t'r = 3 a3 rr^2 + 2 a2 rr + a1
c
c   rr=0   t = 0     t'r = dr1
c   rr=r2  t = 0     t'r = dr2
c
c   a0 =  0
c   a1 = dr1
c   a2 = - (dr2 + 2 dr1)/r2
c   a3 = + (dr2 + dr1  )/r2^2
c--------------------------------------
c calcul tmax
c
c   t'r = 3 a3 rr^2 + 2 a2 rr + a1 = 0
c
c   rr = (-a2 +- sqrt(a2^2-3 a1 a3))/ 3*a3
c   rr = ((dr2 + 2 dr1)/r2
c      +- sqrt((dr2 + 2 dr1)^2/r2^2 - 3 dr1(dr2 + dr1  )/r2^2))
c      / (3(dr2 + dr1  )/r2^2)
c   rr/r2 = ((dr2 + 2 dr1)
c             +- sqrt((dr2 + 2 dr1)^2 - 3 dr1(dr2 + dr1  )))
c           / (3(dr2 + dr1))
c   rr/r2 = ((dr2 + 2 dr1) +- sqrt(dr2^2 + dr1^2 + dr1 dr2))
c           / (3(dr2 + dr1))
c
c   R = rr/r2
c   T = t/r2
c   T = A3 R^3 + A2 R^2 + A1 R
c   A1 = dr1
c   A2 = - (dr2 + 2 dr1)
c   A3 = + (dr2 + dr1  )
c
c   R = ((dr2 + 2 dr1) +- sqrt(dr2^2 + dr1^2 + dr1 dr2))
c           / (3(dr2 + dr1))
c   R = Rn / (3(dr2 + dr1))
c   Rn = (dr2 + 2 dr1) +- sqrt(dr2^2 + dr1^2 + dr1 dr2)
c   Rn^2 = ((dr2 + 2 dr1)^2 + (dr2^2 + dr1^2 + dr1 dr2)
c        +- 2(dr2 + 2 dr1)sqrt(dr2^2 + dr1^2 + dr1 dr2))
c   Rn^2 = (2 dr2^2 + 5 dr1^2 + 5 dr1 dr2)
c        +- 2(dr2 + 2 dr1)sqrt(dr2^2 + dr1^2 + dr1 dr2))
c   Rn^3 = [(2 dr2^2 + 5 dr1^2 + 5 dr1 dr2)
c        +- 2(dr2 + 2 dr1)sqrt(dr2^2 + dr1^2 + dr1 dr2))]
c        *[(dr2 + 2 dr1) +- sqrt(dr2^2 + dr1^2 + dr1 dr2)]
c   Rn^3 =
c         + 2 dr2^3 + 5 dr1^2 dr2 + 5 dr1 dr2^2
c         + 4 dr1 dr2^2 + 10 dr1^3 + 10 dr1^2 dr2
c         + 2 dr2^3 + 2 dr1^2 dr2 + 2 dr1 dr2^2
c         + 4 dr1 dr2^2 +  4 dr1^3 +  4 dr1^2 dr2
c         +-sqrt(dr2^2 + dr1^2 + dr1 dr2)
c               [13 dr1^2 + 4 dr2^2 + 13 dr1 dr2]
c   Rn^3 =
c         + 14 dr1^3
c         + 21 dr1^2 dr2
c         + 15 dr1 dr2^2
c         + 4 dr2^3
c         +-sqrt(dr2^2 + dr1^2 + dr1 dr2)
c               [13 dr1^2 + 4 dr2^2 + 13 dr1 dr2]
c
c   si 0 < R < 1
c        T = (dr2 + dr1) Rn^3 / (3^3(dr2 + dr1)^3)
c          - (dr2 + 2 dr1) Rn / (3^2(dr2 + dr1)^2)
c          + dr1 Rn / (3(dr2 + dr1))
c
c        T = Rn^3 / (27(dr2 + dr1)^2)
c          - 3(dr2 + 2 dr1) Rn^2 / (27(dr2 + dr1)^2)
c          + 9 (dr2 + dr1)dr1 Rn / (27(dr2 + dr1)^2)
c
c        T = (Rn^3
c          - 3(dr2 + 2 dr1) Rn^2
c          + 9(dr2 + dr1)dr1 Rn ) / (27(dr2 + dr1)^2)
c
c
c       T (27(dr2 + dr1)^2) =
c         + 14 dr1^3
c         + 21 dr1^2 dr2
c         + 15 dr1 dr2^2
c         + 4 dr2^3
c         +-sqrt(dr2^2 + dr1^2 + dr1 dr2)
c               [13 dr1^2 + 4 dr2^2 + 13 dr1 dr2]
c         - 3(dr2 + 2 dr1)[(2 dr2^2 + 5 dr1^2 + 5 dr1 dr2)
c                  +- 2(dr2 + 2 dr1)sqrt(dr2^2 + dr1^2 + dr1 dr2))]
c         + 9(dr2 + dr1)dr1 [(dr2 + 2 dr1) +- sqrt(dr2^2 + dr1^2 + dr1 dr2)]
c
c       T (27(dr2 + dr1)^2) =
c         - 16 dr1^3
c         + 3 dr1^2 dr2
c         - 3 dr1 dr2^2
c         + 16 dr2^3
c         +-sqrt(dr2^2 + dr1^2 + dr1 dr2)
c               [13 dr1^2 + 4 dr2^2 + 13 dr1 dr2
c               - 3(dr2 + 2 dr1)2(dr2 + 2 dr1))
c               + 9(dr2 + dr1)dr1 ]
c
c       T (27(dr2 + dr1)^2) =
c         - 16 dr1^3 + 3 dr1^2 dr2 - 3 dr1 dr2^2 + 16 dr2^3
c         -+ 2 [dr1^2 + dr2^2 + dr1dr2]^(3/2)
c
c       T (27(dr2 + dr1)^2) =
c         - 15 (dr1^3 - dr2^3)
c         - (dr1 - dr2)^3
c         -+ 2 [dr1^2 + dr2^2 + dr1dr2]^(3/2)
c
c  dr2=-dr1
c           T (27(-dr1+dr1)^2) =
c             - 38 dr1^3





