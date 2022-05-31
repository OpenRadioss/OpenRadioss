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
Chd|  I20NORMN                      source/interfaces/int20/i20rcurv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NORMN(NRTM,IRECT,NUMNOD,X,NOD_NORMAL,
     .                   NMN ,MSR  ,NLN,NLG)
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
Chd|  I20NORMNP                     source/interfaces/int20/i20rcurv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPMD_I7CURVCOM                source/mpi/interfaces/spmd_i7curvcom.F
Chd|====================================================================
      SUBROUTINE I20NORMNP(NRTM  ,IRECT   ,NUMNOD ,X    ,NOD_NORMAL,
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
        SURFX = SURFX * AAA
        SURFY = SURFY * AAA
        SURFZ = SURFZ * AAA

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
C            IF(N4.GT.MAXCC)print*,'error cc:',n4,maxcc
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
Chd|  I20NORME                      source/interfaces/int20/i20rcurv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NORME(NMNFT, NMNLT, NOD_NORMAL, MSR  ,NLN,NLG)
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
      INTEGER NMNFT, NMNLT, MSR(*),NLN,NLG(NLN)
C     REAL ou REAL*8
      my_real
     .   NOD_NORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N1
      my_real
     .   SURFX,SURFY,SURFZ,AAA
C-----------------------------------------------

      DO I=NMNFT,NMNLT
     	N1 = MSR(I)
        IF(NLN/=0)N1 = NLG(N1)
     	SURFX = NOD_NORMAL(1,N1)
     	SURFY = NOD_NORMAL(2,N1)
     	SURFZ = NOD_NORMAL(3,N1)

        AAA=ONE/MAX(EM30,SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ))
        SURFX = SURFX * AAA
        SURFY = SURFY * AAA
        SURFZ = SURFZ * AAA

     	NOD_NORMAL(1,N1) = SURFX
     	NOD_NORMAL(2,N1) = SURFY
     	NOD_NORMAL(3,N1) = SURFZ
      END DO

      RETURN
      END
C
Chd|====================================================================
Chd|  I20RCURV                      source/interfaces/int20/i20rcurv.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20RCURV(NRTMFT,NRTMLT ,X ,NOD_NORMAL ,IRECT ,
     .                   RCURV ,NRADM  ,ANGLM ,ANGLT ,NLN,NLG)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTMFT, NRTMLT , IRECT(4,*), NRADM,NLN,NLG(NLN)
C     REAL ou REAL*8
      my_real
     .    X(3,*), NOD_NORMAL(3,*), RCURV(*), ANGLM(*), ANGLT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N1, N2, N3, N4
      my_real
     .   X1, X2, X3, X4,
     .   Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4,
     .   NNX1, NNX2, NNX3, NNX4,
     .   NNY1, NNY2, NNY3, NNY4,
     .   NNZ1, NNZ2, NNZ3, NNZ4,
     .   SURFX, SURFY, SURFZ, 
     .   ERX, ERY, ERZ, DNX, DNY, DNZ, DNT, LL, AAA, RR,
     .   X13, Y13, Z13, X24, Y24, Z24, NX, NY, NZ, CC
C-----------------------------------------------
      RCURV(NRTMFT:NRTMLT) = EP30
      ANGLM(NRTMFT:NRTMLT) = EP30

      DO I=NRTMFT, NRTMLT
        N1=IRECT(1,I)
        N2=IRECT(2,I)
        N3=IRECT(3,I)
        N4=IRECT(4,I)
        IF(NLN/=0)THEN
          N1 = NLG(N1)
          N2 = NLG(N2)
          N3 = NLG(N3)
          N4 = NLG(N4)
        ENDIF

        X1=X(1,N1)  
        Y1=X(2,N1)  
        Z1=X(3,N1)  

        X2=X(1,N2)  
        Y2=X(2,N2)  
        Z2=X(3,N2)  

        X3=X(1,N3)  
        Y3=X(2,N3)  
        Z3=X(3,N3)  

        X4=X(1,N4)  
        Y4=X(2,N4)  
        Z4=X(3,N4)  

        NNX1=NOD_NORMAL(1,N1)
        NNY1=NOD_NORMAL(2,N1)
        NNZ1=NOD_NORMAL(3,N1)

        NNX2=NOD_NORMAL(1,N2)
        NNY2=NOD_NORMAL(2,N2)
        NNZ2=NOD_NORMAL(3,N2)

        NNX3=NOD_NORMAL(1,N3)
        NNY3=NOD_NORMAL(2,N3)
        NNZ3=NOD_NORMAL(3,N3)

        NNX4=NOD_NORMAL(1,N4)
        NNY4=NOD_NORMAL(2,N4)
        NNZ4=NOD_NORMAL(3,N4)

C-------
        ERX = (X2+X3)-(X1+X4)
        ERY = (Y2+Y3)-(Y1+Y4)
        ERZ = (Z2+Z3)-(Z1+Z4)

C       Longueur vraie = LL/2
        LL  = SQRT(ERX*ERX+ERY*ERY+ERZ*ERZ)
        AAA = ONE / LL
        ERX = ERX*AAA
        ERY = ERY*AAA
        ERZ = ERZ*AAA

        DNX= (NNX2+NNX3)-(NNX1+NNX4)
        DNY= (NNY2+NNY3)-(NNY1+NNY4)
        DNZ= (NNZ2+NNZ3)-(NNZ1+NNZ4)
C
C       DN vraie = DNT/2
        DNT=(DNX*ERX+DNY*ERY+DNZ*ERZ)

        RR=LL/MAX(EM20,ABS(DNT))
        RCURV(I)=MIN(RCURV(I),RR)
C-------
        ERX = (X4+X3)-(X1+X2)
        ERY = (Y4+Y3)-(Y1+Y2)
        ERZ = (Z4+Z3)-(Z1+Z2)

C       Longueur vraie = LL/2
        LL  = SQRT(ERX*ERX+ERY*ERY+ERZ*ERZ)
        AAA = ONE / LL
        ERX = ERX*AAA
        ERY = ERY*AAA
        ERZ = ERZ*AAA

        DNX= (NNX4+NNX3)-(NNX1+NNX2)
        DNY= (NNY4+NNY3)-(NNY1+NNY2)
        DNZ= (NNZ4+NNZ3)-(NNZ1+NNZ2)
C
C       DN vraie = DNT/2
        DNT=(DNX*ERX+DNY*ERY+DNZ*ERZ)

        RR=LL/(NRADM*MAX(EM20,ABS(DNT)))
        RCURV(I)=MIN(RCURV(I),RR)
C-------
C-------
C-------
C       Angles.
C-------
        X13 = X3 - X1
        Y13 = Y3 - Y1
        Z13 = Z3 - Z1

        X24 = X4 - X2
        Y24 = Y4 - Y2
        Z24 = Z4 - Z2

        SURFX = Y13*Z24 - Z13*Y24
        SURFY = Z13*X24 - X13*Z24
        SURFZ = X13*Y24 - Y13*X24

        AAA=ONE/MAX(EM30,SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ))
        SURFX = SURFX * AAA
        SURFY = SURFY * AAA
        SURFZ = SURFZ * AAA

        CC=(SURFX*NNX1+SURFY*NNY1+SURFZ*NNZ1)/MAX(EM20,ANGLT)
        ANGLM(I)=MIN(ANGLM(I),CC)

        CC=(SURFX*NNX2+SURFY*NNY2+SURFZ*NNZ2)/MAX(EM20,ANGLT)
        ANGLM(I)=MIN(ANGLM(I),CC)

        CC=(SURFX*NNX3+SURFY*NNY3+SURFZ*NNZ3)/MAX(EM20,ANGLT)
        ANGLM(I)=MIN(ANGLM(I),CC)

        CC=(SURFX*NNX4+SURFY*NNY4+SURFZ*NNZ4)/MAX(EM20,ANGLT)
        ANGLM(I)=MIN(ANGLM(I),CC)
      ENDDO

      RETURN
      END
