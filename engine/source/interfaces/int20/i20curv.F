Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I20NORMS                      source/interfaces/int20/i20curv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NORMS(NRTM,IRECT,NUMNOD,X,NOD_NORMAL,
     2                  NMN ,MSR  ,NLN,NLG,GAP_SH,
     3                  IAD_ELEM,FR_ELEM,NSV,NSN)
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NUMNOD,IRECT(4,NRTM),NMN,MSR(*),NLN,NLG(NLN)
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),NOD_NORMAL(3,NUMNOD),NSV(*),NSN
C     REAL ou REAL*8
      my_real
     .   X(3,NUMNOD), GAP_SH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N1,N2,N3,N4
      my_real
     .   SURFX,SURFY,SURFZ,X12,Y12,Z12,X23,Y23,Z23,
     .   X34,Y34,Z34,X41,Y41,Z41,AAA,SI,CO
C-----------------------------------------------

      DO I=1,NMN
          N1 = NLG(MSR(I))
          NOD_NORMAL(1,N1) = 0
          NOD_NORMAL(2,N1) = 0
          NOD_NORMAL(3,N1) = 0
      END DO
      DO I=1,NSN
         N1 = NLG(NSV(I))
         NOD_NORMAL(1,N1) = 0
         NOD_NORMAL(2,N1) = 0
         NOD_NORMAL(3,N1) = 0
      ENDDO
      IF(NSPMD > 1)THEN
        DO I=1,NSPMD
           DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N1 = FR_ELEM(J)
            NOD_NORMAL(1,N1) = 0
            NOD_NORMAL(2,N1) = 0
            NOD_NORMAL(3,N1) = 0
           END DO
        END DO
      END IF

      DO I=1,NRTM
c test pour ne prendre en compte que les solides 
        IF(GAP_SH(I)/=ZERO)THEN
          N1 = NLG(IRECT(1,I))
          N2 = NLG(IRECT(2,I))
          N3 = NLG(IRECT(3,I))
          N4 = NLG(IRECT(4,I))


          X41 = X(1,N1) - X(1,N4)
          Y41 = X(2,N1) - X(2,N4)
          Z41 = X(3,N1) - X(3,N4)

          X12 = X(1,N2) - X(1,N1)
          Y12 = X(2,N2) - X(2,N1)
          Z12 = X(3,N2) - X(3,N1)

          X23 = X(1,N3) - X(1,N2)
          Y23 = X(2,N3) - X(2,N2)
          Z23 = X(3,N3) - X(3,N2)

          SURFX = Y41*Z12 - Z41*Y12
          SURFY = Z41*X12 - X41*Z12
          SURFZ = X41*Y12 - Y41*X12
          CO = X41*X12 + Y41*Y12 + Z41*Z12
          SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
c          AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
          AAA = EP07*ATAN2(SI,-CO)/MAX(EM30,SI)
          NOD_NORMAL(1,N1) = NOD_NORMAL(1,N1) + NINT(SURFX*AAA)
          NOD_NORMAL(2,N1) = NOD_NORMAL(2,N1) + NINT(SURFY*AAA)
          NOD_NORMAL(3,N1) = NOD_NORMAL(3,N1) + NINT(SURFZ*AAA)

          SURFX = Y12*Z23 - Z12*Y23
          SURFY = Z12*X23 - X12*Z23
          SURFZ = X12*Y23 - Y12*X23
          CO = X12*X23 + Y12*Y23 + Z12*Z23
          SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
c          AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
          AAA = EP07*ATAN2(SI,-CO)/MAX(EM30,SI)
          NOD_NORMAL(1,N2) = NOD_NORMAL(1,N2) + NINT(SURFX*AAA)
          NOD_NORMAL(2,N2) = NOD_NORMAL(2,N2) + NINT(SURFY*AAA)
          NOD_NORMAL(3,N2) = NOD_NORMAL(3,N2) + NINT(SURFZ*AAA)

          IF(N3 /= N4)THEN
            X34 = X(1,N4) - X(1,N3)
            Y34 = X(2,N4) - X(2,N3)
            Z34 = X(3,N4) - X(3,N3)
            SURFX = Y23*Z34 - Z23*Y34
            SURFY = Z23*X34 - X23*Z34
            SURFZ = X23*Y34 - Y23*X34
            CO = X23*X34 + Y23*Y34 + Z23*Z34
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
c            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
            AAA = EP07*ATAN2(SI,-CO)/MAX(EM30,SI)
            NOD_NORMAL(1,N3) = NOD_NORMAL(1,N3) + NINT(SURFX*AAA)
            NOD_NORMAL(2,N3) = NOD_NORMAL(2,N3) + NINT(SURFY*AAA)
            NOD_NORMAL(3,N3) = NOD_NORMAL(3,N3) + NINT(SURFZ*AAA)

            SURFX = Y34*Z41 - Z34*Y41
            SURFY = Z34*X41 - X34*Z41
            SURFZ = X34*Y41 - Y34*X41
            CO = X34*X41 + Y34*Y41 + Z34*Z41
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
c            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
            AAA = EP07*ATAN2(SI,-CO)/MAX(EM30,SI)
            NOD_NORMAL(1,N4) = NOD_NORMAL(1,N4) + NINT(SURFX*AAA)
            NOD_NORMAL(2,N4) = NOD_NORMAL(2,N4) + NINT(SURFY*AAA)
            NOD_NORMAL(3,N4) = NOD_NORMAL(3,N4) + NINT(SURFZ*AAA)
          ELSE
            SURFX = Y23*Z41 - Z23*Y41
            SURFY = Z23*X41 - X23*Z41
            SURFZ = X23*Y41 - Y23*X41
            CO = X41*X23 + Y41*Y23 + Z41*Z23
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
c            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
            AAA = EP07*ATAN2(SI,-CO)/MAX(EM30,SI)
            NOD_NORMAL(1,N3) = NOD_NORMAL(1,N3) + NINT(SURFX*AAA)
            NOD_NORMAL(2,N3) = NOD_NORMAL(2,N3) + NINT(SURFY*AAA)
            NOD_NORMAL(3,N3) = NOD_NORMAL(3,N3) + NINT(SURFZ*AAA)
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  I20NORMSP                     source/interfaces/int20/i20curv.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPMD_I7CURVCOM                source/mpi/interfaces/spmd_i7curvcom.F
Chd|====================================================================
      SUBROUTINE I20NORMSP(NRTM  ,IRECT   ,NUMNOD ,X    ,NOD_NORMAL,
     .                   NMN   ,MSR     ,LENT   ,MAXCC,ISDSIZ    ,
     .                   IRCSIZ,IAD_ELEM,FR_ELEM,ITAG ,NLN,NLG,
     .                   GAP_SH)
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NUMNOD,NMN,MAXCC,LENT,
     .        IRECT(4,NRTM),MSR(*),NLN,NLG(NLN),
     .        IAD_ELEM(2,*),FR_ELEM(*),ISDSIZ(*),IRCSIZ(*),ITAG(*)
C     REAL ou REAL*8
      my_real
     .   X(3,NUMNOD), NOD_NORMAL(3,NUMNOD),GAP_SH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N1,N2,N3,N4, IAD, LENR, LENS, CC, ERROR,
     .        ADSKYT(0:NUMNOD+1)
      my_real
     .        SURFX,SURFY,SURFZ,X12,Y12,Z12,X23,Y23,Z23,
     .        X34,Y34,Z34,X41,Y41,Z41,AAA,SI,CO,
     .        FSKYT(3,LENT), FSKYT2(MAXCC), PERM(MAXCC)
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
        N1 = NLG(IRECT(1,I))
        N2 = NLG(IRECT(2,I))
        N3 = NLG(IRECT(3,I))
        N4 = NLG(IRECT(4,I))
c test pour ne prendre en compte que les solides 
        IF(GAP_SH(I)/=ZERO)THEN

c          X13 = X(1,N3) - X(1,N1)
c          Y13 = X(2,N3) - X(2,N1)
c          Z13 = X(3,N3) - X(3,N1)

c          X24 = X(1,N4) - X(1,N2)
c          Y24 = X(2,N4) - X(2,N2)
c          Z24 = X(3,N4) - X(3,N2)

c          SURFX = Y13*Z24 - Z13*Y24
c          SURFY = Z13*X24 - X13*Z24
c          SURFZ = X13*Y24 - Y13*X24

          X41 = X(1,N1) - X(1,N4)
          Y41 = X(2,N1) - X(2,N4)
          Z41 = X(3,N1) - X(3,N4)

          X12 = X(1,N2) - X(1,N1)
          Y12 = X(2,N2) - X(2,N1)
          Z12 = X(3,N2) - X(3,N1)

          X23 = X(1,N3) - X(1,N2)
          Y23 = X(2,N3) - X(2,N2)
          Z23 = X(3,N3) - X(3,N2)

          SURFX = Y41*Z12 - Z41*Y12
          SURFY = Z41*X12 - X41*Z12
          SURFZ = X41*Y12 - Y41*X12
          CO = X41*X12 + Y41*Y12 + Z41*Z12
          SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
          AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)

          IAD = ADSKYT(N1)
          ADSKYT(N1) = ADSKYT(N1)+1
          FSKYT(1,IAD) =  SURFX*AAA
          FSKYT(2,IAD) =  SURFY*AAA
          FSKYT(3,IAD) =  SURFZ*AAA

          SURFX = Y12*Z23 - Z12*Y23
          SURFY = Z12*X23 - X12*Z23
          SURFZ = X12*Y23 - Y12*X23
          CO = X12*X23 + Y12*Y23 + Z12*Z23
          SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
          AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)

          IAD = ADSKYT(N2)
          ADSKYT(N2) = ADSKYT(N2)+1
          FSKYT(1,IAD) =  SURFX*AAA
          FSKYT(2,IAD) =  SURFY*AAA
          FSKYT(3,IAD) =  SURFZ*AAA

          IF(N3 /= N4)THEN

            X34 = X(1,N4) - X(1,N3)
            Y34 = X(2,N4) - X(2,N3)
            Z34 = X(3,N4) - X(3,N3)
            SURFX = Y23*Z34 - Z23*Y34
            SURFY = Z23*X34 - X23*Z34
            SURFZ = X23*Y34 - Y23*X34
            CO = X23*X34 + Y23*Y34 + Z23*Z34
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)

            IAD = ADSKYT(N3)
            ADSKYT(N3) = ADSKYT(N3)+1
            FSKYT(1,IAD) =  SURFX*AAA
            FSKYT(2,IAD) =  SURFY*AAA
            FSKYT(3,IAD) =  SURFZ*AAA

            SURFX = Y34*Z41 - Z34*Y41
            SURFY = Z34*X41 - X34*Z41
            SURFZ = X34*Y41 - Y34*X41
            CO = X34*X41 + Y34*Y41 + Z34*Z41
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)

            IAD = ADSKYT(N4)
            ADSKYT(N4) = ADSKYT(N4)+1
            FSKYT(1,IAD) =  SURFX*AAA
            FSKYT(2,IAD) =  SURFY*AAA
            FSKYT(3,IAD) =  SURFZ*AAA

          ELSE

            SURFX = Y23*Z41 - Z23*Y41
            SURFY = Z23*X41 - X23*Z41
            SURFZ = X23*Y41 - Y23*X41
            CO = X41*X23 + Y41*Y23 + Z41*Z23
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)

            IAD = ADSKYT(N3)
            ADSKYT(N3) = ADSKYT(N3)+1
            FSKYT(1,IAD) =  SURFX*AAA
            FSKYT(2,IAD) =  SURFY*AAA
            FSKYT(3,IAD) =  SURFZ*AAA

            IAD = ADSKYT(N4)
            ADSKYT(N4) = ADSKYT(N4)+1
            FSKYT(1,IAD) =  ZERO
            FSKYT(2,IAD) =  ZERO
            FSKYT(3,IAD) =  ZERO

          END IF

        ELSE

          IAD = ADSKYT(N1)
          ADSKYT(N1) = ADSKYT(N1)+1
          FSKYT(1,IAD) =  ZERO
          FSKYT(2,IAD) =  ZERO
          FSKYT(3,IAD) =  ZERO
          IAD = ADSKYT(N2)
          ADSKYT(N2) = ADSKYT(N2)+1
          FSKYT(1,IAD) =  ZERO
          FSKYT(2,IAD) =  ZERO
          FSKYT(3,IAD) =  ZERO
          IAD = ADSKYT(N3)
          ADSKYT(N3) = ADSKYT(N3)+1
          FSKYT(1,IAD) =  ZERO
          FSKYT(2,IAD) =  ZERO
          FSKYT(3,IAD) =  ZERO
          IAD = ADSKYT(N4)
          ADSKYT(N4) = ADSKYT(N4)+1
          FSKYT(1,IAD) =  ZERO
          FSKYT(2,IAD) =  ZERO
          FSKYT(3,IAD) =  ZERO

        ENDIF

      END DO
C
      IF(NSPMD > 1) THEN
        LENR = IRCSIZ(NSPMD+1)*3+IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        LENS = ISDSIZ(NSPMD+1)*3+IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_I7CURVCOM(IAD_ELEM,FR_ELEM,ADSKYT,FSKYT,
     .                      ISDSIZ,IRCSIZ,ITAG  ,LENR    ,LENS   )
      END IF
C
C     tri par packet des normales
C
      DO N1 = 1, NUMNOD
        N2 = ADSKYT(N1-1)
        N3 = ADSKYT(N1)-1
        N4 = N3-N2+1
        IF(N4.GT.1)THEN        ! cas N contribution => tri
          DO J = 1, 3
            DO CC = N2, N3
              FSKYT2(CC-N2+1) = FSKYT(J,CC)
            END DO
            CALL MYQSORT(N4,FSKYT2,PERM,ERROR)
            DO CC = N2, N3
              NOD_NORMAL(J,N1) = NOD_NORMAL(J,N1) + FSKYT2(CC-N2+1)
            END DO
          END DO
        ELSEIF(N4.EQ.1)THEN    ! cas 1 seule contribution => direct
          NOD_NORMAL(1,N1) = FSKYT(1,N2)
          NOD_NORMAL(2,N1) = FSKYT(2,N2)
          NOD_NORMAL(3,N1) = FSKYT(3,N2)
        END IF
      END DO
      RETURN
      END
Chd|====================================================================
Chd|  I20NORM                       source/interfaces/int20/i20curv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NORM(NRTM,IRECT,NUMNOD,X,NOD_NORMAL,
     .                  NMN ,MSR  ,NLN,NLG)
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
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NUMNOD,IRECT(4,NRTM),NMN,MSR(*),NLN,NLG(NLN)
C     REAL ou REAL*8
      my_real
     .   X(3,NUMNOD), NOD_NORMAL(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N1,N2,N3,N4
      my_real
     .   SURFX,SURFY,SURFZ,X13,Y13,Z13,X24,Y24,Z24,AAA
C-----------------------------------------------

      IF(IMACH.NE.3)THEN
        DO I=1,NMN
          N1 = MSR(I)
          IF(NLN/=0)N1 = NLG(N1)
          NOD_NORMAL(1,N1) = ZERO
          NOD_NORMAL(2,N1) = ZERO
          NOD_NORMAL(3,N1) = ZERO
        END DO
      ELSE
C optimisable en spmd si ajout flag pour routine de comm, spmd_exchange_n
        DO N1=1,NUMNOD
          NOD_NORMAL(1,N1) = ZERO
          NOD_NORMAL(2,N1) = ZERO
          NOD_NORMAL(3,N1) = ZERO
        END DO
      END IF

      DO I=1,NRTM
        N1 = IRECT(1,I)
        N2 = IRECT(2,I)
        N3 = IRECT(3,I)
        N4 = IRECT(4,I)
        IF(NLN/=0)THEN
          N1 = NLG(N1)
          N2 = NLG(N2)
          N3 = NLG(N3)
          N4 = NLG(N4)
        ENDIF

        X13 = X(1,N3) - X(1,N1)
        Y13 = X(2,N3) - X(2,N1)
        Z13 = X(3,N3) - X(3,N1)

        X24 = X(1,N4) - X(1,N2)
        Y24 = X(2,N4) - X(2,N2)
        Z24 = X(3,N4) - X(3,N2)

        SURFX = Y13*Z24 - Z13*Y24
        SURFY = Z13*X24 - X13*Z24
        SURFZ = X13*Y24 - Y13*X24

        AAA=ONE/MAX(EM30,SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ))
        SURFX = SURFX * AAA
        SURFY = SURFY * AAA
        SURFZ = SURFZ * AAA

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
Chd|  I20NORMP                      source/interfaces/int20/i20curv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPMD_I7CURVCOM                source/mpi/interfaces/spmd_i7curvcom.F
Chd|====================================================================
      SUBROUTINE I20NORMP(NRTM  ,IRECT   ,NUMNOD ,X    ,NOD_NORMAL,
     .                   NMN   ,MSR     ,LENT   ,MAXCC,ISDSIZ    ,
     .                   IRCSIZ,IAD_ELEM,FR_ELEM,ITAG ,NLN,NLG)
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NUMNOD,NMN,MAXCC,LENT,
     .        IRECT(4,NRTM),MSR(*),NLN,NLG(NLN),
     .        IAD_ELEM(2,*),FR_ELEM(*),ISDSIZ(*),IRCSIZ(*),ITAG(*)
C     REAL ou REAL*8
      my_real
     .   X(3,NUMNOD), NOD_NORMAL(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N1,N2,N3,N4, IAD, LENR, LENS, CC, ERROR,
     .        ADSKYT(0:NUMNOD+1)
      my_real
     .        SURFX,SURFY,SURFZ,X13,Y13,Z13,X24,Y24,Z24,AAA,
     .        FSKYT(3,LENT), FSKYT2(MAXCC), PERM(MAXCC)
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
        IF(NLN/=0)THEN
          N1 = NLG(N1)
          N2 = NLG(N2)
          N3 = NLG(N3)
          N4 = NLG(N4)
        ENDIF

        X13 = X(1,N3) - X(1,N1)
        Y13 = X(2,N3) - X(2,N1)
        Z13 = X(3,N3) - X(3,N1)

        X24 = X(1,N4) - X(1,N2)
        Y24 = X(2,N4) - X(2,N2)
        Z24 = X(3,N4) - X(3,N2)

        SURFX = Y13*Z24 - Z13*Y24
        SURFY = Z13*X24 - X13*Z24
        SURFZ = X13*Y24 - Y13*X24

        AAA=ONE/MAX(EM30,SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ))

        IAD = ADSKYT(N1)
        ADSKYT(N1) = ADSKYT(N1)+1
        FSKYT(1,IAD) =  SURFX * AAA
        FSKYT(2,IAD) =  SURFY * AAA
        FSKYT(3,IAD) =  SURFZ * AAA
        IAD = ADSKYT(N2)
        ADSKYT(N2) = ADSKYT(N2)+1
        FSKYT(1,IAD) =  SURFX * AAA
        FSKYT(2,IAD) =  SURFY * AAA
        FSKYT(3,IAD) =  SURFZ * AAA
        IAD = ADSKYT(N3)
        ADSKYT(N3) = ADSKYT(N3)+1
        FSKYT(1,IAD) =  SURFX * AAA
        FSKYT(2,IAD) =  SURFY * AAA
        FSKYT(3,IAD) =  SURFZ * AAA
        IAD = ADSKYT(N4)
        ADSKYT(N4) = ADSKYT(N4)+1
        FSKYT(1,IAD) =  SURFX * AAA
        FSKYT(2,IAD) =  SURFY * AAA
        FSKYT(3,IAD) =  SURFZ * AAA
      END DO
C
      IF(NSPMD.GT.1) THEN
        LENR = IRCSIZ(NSPMD+1)*3+IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        LENS = ISDSIZ(NSPMD+1)*3+IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_I7CURVCOM(IAD_ELEM,FR_ELEM,ADSKYT,FSKYT,
     .                      ISDSIZ,IRCSIZ,ITAG  ,LENR    ,LENS   )
      END IF
C
C     tri par packet des normales
C
      DO N1 = 1, NUMNOD
        N2 = ADSKYT(N1-1)
        N3 = ADSKYT(N1)-1
        N4 = N3-N2+1
        IF(N4.GT.1)THEN        ! cas N contribution => tri
          DO J = 1, 3
            DO CC = N2, N3
              FSKYT2(CC-N2+1) = FSKYT(J,CC)
            END DO
            CALL MYQSORT(N4,FSKYT2,PERM,ERROR)
            DO CC = N2, N3
              NOD_NORMAL(J,N1) = NOD_NORMAL(J,N1) + FSKYT2(CC-N2+1)
            END DO
          END DO
        ELSEIF(N4.EQ.1)THEN    ! cas 1 seule contribution => direct
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
Chd|  I20CMAJ                       source/interfaces/int20/i20curv.F
Chd|-- called by -----------
Chd|        I20DST3                       source/interfaces/int20/i20dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20CMAJ(JLT    ,CMAJ   ,IRECT  ,NOD_NORMAL,CAND_E,
     2                  X1     ,X2     ,X3     ,X4        ,NLN    ,
     3                  Y1     ,Y2     ,Y3     ,Y4        ,NLG    ,
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT ,IRECT(4,*),CAND_E(*),NLN,NLG(NLN)
C     REAL ou REAL*8
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     CMAJ(MVSIZ),NOD_NORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , IX,L
      my_real
     .   X12,Y12,Z12,X23,Y23,Z23,X34,Y34,Z34,X41,Y41,Z41,
     .   L12L12,L23L23,L34L34,L41L41,N1N1,N2N2,N3N3,N4N4,NL,
     .   N1L12N1L12,N2L12N2L12,N2L23N2L23,N3L23N3L23,
     .   N3L34N3L34,N4L34N4L34,N4L41N4L41,N1L41N1L41,AAA
C-----------------------------------------------

      DO I=1,JLT
        L  = CAND_E(I)                


        IX = NLG(IRECT(1,L))
        NNX1(I)=NOD_NORMAL(1,IX)  
        NNY1(I)=NOD_NORMAL(2,IX)  
        NNZ1(I)=NOD_NORMAL(3,IX)  

        IX = NLG(IRECT(2,L))
        NNX2(I)=NOD_NORMAL(1,IX)  
        NNY2(I)=NOD_NORMAL(2,IX)  
        NNZ2(I)=NOD_NORMAL(3,IX)  

        IX = NLG(IRECT(3,L))
        NNX3(I)=NOD_NORMAL(1,IX)  
        NNY3(I)=NOD_NORMAL(2,IX)  
        NNZ3(I)=NOD_NORMAL(3,IX)  

        IX = NLG(IRECT(4,L))
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
     +         + NNY1(I)*NNY1(I)
     +         + NNZ1(I)*NNZ1(I)
        N2N2   = NNX2(I)*NNX2(I) 
     +         + NNY2(I)*NNY2(I)
     +         + NNZ2(I)*NNZ2(I)
        N3N3   = NNX3(I)*NNX3(I) 
     +         + NNY3(I)*NNY3(I)
     +         + NNZ3(I)*NNZ3(I)
        N4N4   = NNX4(I)*NNX4(I) 
     +         + NNY4(I)*NNY4(I)
     +         + NNZ4(I)*NNZ4(I)

        NL  = NNX1(I)*X12
     +      + NNY1(I)*Y12   
     +      + NNZ1(I)*Z12   
        N1L12N1L12 = NL*NL
        NL = NNX2(I)*X12
     +     + NNY2(I)*Y12
     +     + NNZ2(I)*Z12
        N2L12N2L12  = NL*NL
        NL = NNX2(I)*X23
     +     + NNY2(I)*Y23
     +     + NNZ2(I)*Z23
        N2L23N2L23 = NL*NL
        NL = NNX3(I)*X23
     +     + NNY3(I)*Y23
     +     + NNZ3(I)*Z23
        N3L23N3L23  = NL*NL
        NL = NNX3(I)*X34
     +     + NNY3(I)*Y34
     +     + NNZ3(I)*Z34
        N3L34N3L34 = NL*NL
        NL = NNX4(I)*X34
     +     + NNY4(I)*Y34
     +     + NNZ4(I)*Z34
        N4L34N4L34 = NL*NL
        NL = NNX4(I)*X41
     +     + NNY4(I)*Y41    
     +     + NNZ4(I)*Z41    
        N4L41N4L41 = NL*NL
        NL = NNX1(I)*X41
     +     + NNY1(I)*Y41    
     +     + NNZ1(I)*Z41    
        N1L41N1L41 = NL*NL

c            Z^2 = (L.N)^2 L^2 / 4(N^2 L^2 - (L.N)^2)
        AAA = MAX(N1L12N1L12*L12L12 / (N1N1*L12L12 - N1L12N1L12),
     .            N2L12N2L12*L12L12 / (N2N2*L12L12 - N2L12N2L12),
     .            N2L23N2L23*L23L23 / (N2N2*L23L23 - N2L23N2L23),
     .            N3L23N3L23*L23L23 / (N3N3*L23L23 - N3L23N3L23),
     .            N3L34N3L34*L34L34 / (N3N3*L34L34 - N3L34N3L34),
     .            N4L34N4L34*L34L34 / (N4N4*L34L34 - N4L34N4L34),
     .            N4L41N4L41*L41L41 / (N4N4*L41L41 - N4L41N4L41),
     .            N1L41N1L41*L41L41 / (N1N1*L41L41 - N1L41N1L41))
        CMAJ(I) = HALF*SQRT(AAA)

      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  I20NORMCNT                    source/interfaces/int20/i20curv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NORMCNT(NRTM  ,IRECT ,NUMNOD,ITAG  ,LENT   ,
     2                      MAXCC ,NLN   ,NLG   )
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NUMNOD,LENT,MAXCC,NLN,NLG(NLN),
     .        IRECT(4,NRTM),ITAG(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N1, N2, N3, N4, IERROR, IAD, SIZ, NB,
     .        MSGTYP
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
        N1 = NLG(N1)
        N2 = NLG(N2)
        N3 = NLG(N3)
        N4 = NLG(N4)
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
