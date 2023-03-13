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
Chd|  CNDINT                        source/model/remesh/cndint.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE CNDINT(IXC ,IPARTC ,IXTG ,IPARTTG,IPART,
     2              ITASK   ,A      ,V    ,AR     ,VR   , 
     3              MS      ,IN     ,NODFT,NODLT  ,X    ,
     4              SH4TREE ,SH3TREE,ITAB ,STIFN ,STIFR ,
     5              MSCND   ,INCND  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IPARTC(*), IXTG(NIXTG,*), IPARTTG(*),
     .        IPART(LIPART1,*), ITASK, NODFT, NODLT,SH4TREE(KSH4TREE,*),
     .        SH3TREE(KSH3TREE,*), ITAB(*)
       my_real
     .        A(3,*),V(3,*),
     .        AR(3,*),VR(3,*), MS(*), IN(*), X(3,*),
     .        STIFN(*), STIFR(*), MSCND(*), INCND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SH4FT, SH4LT, SH3FT, SH3LT
      INTEGER N, NN, LEVEL, IP, NLEV, LL, IERR
      INTEGER SON,M1,M2,M3,M4,MC,N1,N2,N3,N4,J,NA,NB
      my_real
     .        VV, AX(3,NUMNOD), ARX(3,NUMNOD), FAC,
     .        DT2P, MAS, INER, DTN
C-----------------------------------------------
C
C     allocation tag
      TAGNOD(NODFT:NODLT)=0
C
C     Retrieve forces on nodes (static nodes would be enough).
      AX (1:3,NODFT:NODLT)=ACND (1:3,NODFT:NODLT)
      ARX(1:3,NODFT:NODLT)=ARCND(1:3,NODFT:NODLT)
C
      CALL MY_BARRIER
C
      LL=PSH4UPL(1)
      SH4FT = 1+ITASK*LL/ NTHREAD
      SH4LT = (ITASK+1)*LL/NTHREAD

      DO NN=SH4FT,SH4LT
        N    =LSH4UPL(NN)
C
        N1=IXC(2,N)
        N2=IXC(3,N)
        N3=IXC(4,N)
        N4=IXC(5,N)
C
        IF(TAGNOD(N1)==0)THEN
          TAGNOD(N1)=1
          DO J=1,3
            ACND(J,N1)  =A(J,N1)
          END DO
          DO J=1,3
            ARCND(J,N1) =AR(J,N1)
          END DO
        END IF
C
        IF(TAGNOD(N2)==0)THEN
          TAGNOD(N2)=1
          DO J=1,3
            ACND(J,N2)  =A(J,N2)
          END DO
          DO J=1,3
            ARCND(J,N2) =AR(J,N2)
          END DO
        END IF
C
        IF(TAGNOD(N3)==0)THEN
          TAGNOD(N3)=1
          DO J=1,3
            ACND(J,N3)  =A(J,N3)
          END DO
          DO J=1,3
            ARCND(J,N3) =AR(J,N3)
          END DO
        END IF
C
        IF(TAGNOD(N4)==0)THEN
          TAGNOD(N4)=1
          DO J=1,3
            ACND(J,N4)  =A(J,N4)
          END DO
          DO J=1,3
            ARCND(J,N4) =AR(J,N4)
          END DO
        END IF
C
      END DO
C
      LL=PSH3UPL(1)
      SH3FT = 1+ITASK*LL/ NTHREAD
      SH3LT = (ITASK+1)*LL/NTHREAD

      DO NN=SH3FT,SH3LT
        N    =LSH3UPL(NN)
C
        N1=IXTG(2,N)
        N2=IXTG(3,N)
        N3=IXTG(4,N)
C
        IF(TAGNOD(N1)==0)THEN
          TAGNOD(N1)=1
          DO J=1,3
            ACND(J,N1)  =A(J,N1)
          END DO
          DO J=1,3
            ARCND(J,N1) =AR(J,N1)
          END DO
        END IF
C
        IF(TAGNOD(N2)==0)THEN
          TAGNOD(N2)=1
          DO J=1,3
            ACND(J,N2)  =A(J,N2)
          END DO
          DO J=1,3
            ARCND(J,N2) =AR(J,N2)
          END DO
        END IF
C
        IF(TAGNOD(N3)==0)THEN
          TAGNOD(N3)=1
          DO J=1,3
            ACND(J,N3)  =A(J,N3)
          END DO
          DO J=1,3
            ARCND(J,N3) =AR(J,N3)
          END DO
        END IF
C
      END DO
C
      CALL MY_BARRIER
C
      TAGNOD(NODFT:NODLT)=0
C
      CALL MY_BARRIER
C
C-------
C     Interpolation de v,a
      DO LEVEL=0,LEVELMAX-1

        LL=PSH4UPL(LEVEL+1)-PSH4UPL(LEVEL)
        SH4FT = PSH4UPL(LEVEL)+ 1+ITASK*LL/ NTHREAD
        SH4LT = PSH4UPL(LEVEL)+ (ITASK+1)*LL/NTHREAD

        DO NN=SH4FT,SH4LT
          N    =LSH4UPL(NN)
C
          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          SON=SH4TREE(2,N)
C
          MC=IXC(3,SON+3)

          IF(TAGNOD(MC)==0)THEN

             TAGNOD(MC)=1
             DO J=1,3
              VV = 
     .           FOURTH*(ACND(J,N1)+ACND(J,N2)+ACND(J,N3)+ACND(J,N4))
              ACND(J,MC) =VV
            END DO

            DO J=1,3
              VV = 
     .           FOURTH*(ARCND(J,N1)+ARCND(J,N2)+ARCND(J,N3)+ARCND(J,N4))
              ARCND(J,MC)=VV
            END DO

          END IF
C
          M1=IXC(3,SON  )
          M2=IXC(4,SON+1)
          M3=IXC(5,SON+2)
          M4=IXC(2,SON+3)

          IF(TAGNOD(M1)==0)THEN
             TAGNOD(M1)=1
             NA=MIN(N1,N2)
             NB=MAX(N1,N2)

             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M1) =VV
             END DO

            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M1)=VV
            END DO

          END IF

          IF(TAGNOD(M2)==0)THEN
             TAGNOD(M2)=1
             NA=MIN(N2,N3)
             NB=MAX(N2,N3)

             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M2) =VV
            END DO

            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M2)=VV
            END DO

          END IF

          IF(TAGNOD(M3)==0)THEN
             TAGNOD(M3)=1
             NA=MIN(N3,N4)
             NB=MAX(N3,N4)

             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M3) =VV
            END DO

            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M3)=VV
            END DO

          END IF

          IF(TAGNOD(M4)==0)THEN
             TAGNOD(M4)=1
             NA=MIN(N4,N1)
             NB=MAX(N4,N1)

             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M4) =VV
            END DO

            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M4)=VV
            END DO

          END IF

        END DO
C
        LL=PSH3UPL(LEVEL+1)-PSH3UPL(LEVEL)
        SH3FT = PSH3UPL(LEVEL)+ 1+ITASK*LL/ NTHREAD
        SH3LT = PSH3UPL(LEVEL)+ (ITASK+1)*LL/NTHREAD

        DO NN=SH3FT,SH3LT
          N    =LSH3UPL(NN)
C
          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          SON=SH3TREE(2,N)
C
          M1=IXTG(4,SON+3)
          M2=IXTG(2,SON+3)
          M3=IXTG(3,SON+3)

          IF(TAGNOD(M1)==0)THEN
            TAGNOD(M1)=1
            NA=MIN(N1,N2)
            NB=MAX(N1,N2)

             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M1) =VV
            END DO
            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M1)=VV
            END DO

          END IF

          IF(TAGNOD(M2)==0)THEN
            TAGNOD(M2)=1
            NA=MIN(N2,N3)
            NB=MAX(N2,N3)
             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M2) =VV
            END DO
            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M2)=VV
            END DO

          END IF

          IF(TAGNOD(M3)==0)THEN
            TAGNOD(M3)=1
            NA=MIN(N3,N1)
            NB=MAX(N3,N1)
             DO J=1,3
              VV     = HALF*(ACND(J,NA)+ACND(J,NB))
              ACND(J,M3) =VV
            END DO
            DO J=1,3
              VV     = HALF*(ARCND(J,NA)+ARCND(J,NB))
              ARCND(J,M3)=VV
            END DO

          END IF

        END DO
C
        CALL MY_BARRIER
C
C-------
      END DO
C
C-------
      IF(NODADT /= 0.OR.I7KGLO/=0.AND.(IDTMIN(11)==3.OR.IDTMIN(11)==8))THEN
        DT2P = DTMIN1(11)/DTFAC1(11)
        DO N=NODFT,NODLT
          IF(TAGNOD(N)/=0)THEN
            MAS     = HALF * STIFN(N) * DT2P * DT2P * ONEP00001
            MSCND(N)=MAX(MSCND(N),MAS)
          END IF
        END DO
      END IF
      IF(NODADT /= 0.AND.(IDTMIN(11)==3.OR.IDTMIN(11)==8))THEN
        DT2P = DTMIN1(11)/DTFAC1(11)
        DO N=NODFT,NODLT
          IF(TAGNOD(N)/=0)THEN
            INER = HALF * STIFR(N) * DT2P * DT2P * ONEP00001
            INCND(N)=MAX(INCND(N),INER)
          END IF
        END DO
      END IF

c pour debug
c      DT2P=DT2*ZEP999
c      DO N=NODFT,NODLT
c        IF(TAGNOD(N)/=0)THEN
c          DTN     = DTFAC1(11)*SQRT(2. * MSCND(N) / STIFN(N))
c          IF(DTN < DT2P)THEN
c#include "lockon.inc"
c            WRITE(IOUT,*)
c     .' **WARNING : TIME STEP LESS OR EQUAL DT2 FOR CONDENSED NODE N=',
c     .              ITAB(N),DT2,DTN
c            WRITE(ISTDO,*)
c     .' **WARNING : TIME STEP LESS OR EQUAL DT2 FOR CONDENSED NODE N=',
c     .              ITAB(N),DT2,DTN
c#include "lockoff.inc"
c          END IF
c          DTN     = DTFAC1(11)*SQRT(2. * INCND(N) / STIFR(N))
c          IF(DTN < DT2P)THEN
c#include "lockon.inc"
c            WRITE(IOUT,*)
c     .' **WARNING : TIME STEP LESS OR EQUAL DT2 FOR CONDENSED NODE N=',
c     .              ITAB(N),DT2,DTN
c            WRITE(ISTDO,*)
c     .' **WARNING : TIME STEP LESS OR EQUAL DT2 FOR CONDENSED NODE N=',
c     .              ITAB(N),DT2,DTN
c#include "lockoff.inc"
c          END IF
c        END IF
c      END DO
C
C-------
      DO N=NODFT,NODLT
        IF(TAGNOD(N)/=0)THEN

          FAC=ONE/MAX(MSCND(N),EM20)
          A(1,N) = AX(1,N)*FAC+ACND(1,N)
          A(2,N) = AX(2,N)*FAC+ACND(2,N)
          A(3,N) = AX(3,N)*FAC+ACND(3,N)

          FAC=ONE/MAX(INCND(N),EM20)
          AR(1,N) = ARX(1,N)*FAC+ARCND(1,N)
          AR(2,N) = ARX(2,N)*FAC+ARCND(2,N)
          AR(3,N) = ARX(3,N)*FAC+ARCND(3,N)

        END IF
      END DO
  
      RETURN
      END     
