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
Chd|  ANI_FASOLFR1                  source/output/anim/ani_fasolfr.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE ANI_FASOLFR1(IXS ,IXC     ,IXTG    ,FASTAG, ISOLNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER
     .   IXS(NIXS,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .   FASTAG(*),ISOLNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NELENOD
      INTEGER, DIMENSION(:), ALLOCATABLE :: ELSNOD,ELCNOD,ELTGNOD,
     .                                      NODTAG,NODTAG_1
      INTEGER N,NI,I,J,K,II,JJ,KK,LL,NN,JS,KS,
     .        IERROR
      INTEGER FACES(4,6),PWR(7)
      DATA FACES/1,2,3,4,
     .           2,1,5,6,
     .           1,5,8,4,
     .           5,6,7,8,
     .           3,4,8,7,
     .           2,6,7,3/
      DATA PWR/1,2,4,8,16,32,64/
C     REAL
C-----------------------------------------------
C
      ALLOCATE(NODTAG(NUMNOD), STAT=IERROR)
      IF (IERROR/=0) THEN
        WRITE(ISTDO,'(A)') ' ANIM ...'
        WRITE(ISTDO,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY'
        WRITE(IOUT,'(A)') ' ANIM ...'
        WRITE(IOUT,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY NODTAG'
        CALL ARRET(2)
      END IF
      ALLOCATE(NODTAG_1(NUMNOD), STAT=IERROR)
      IF (IERROR/=0) THEN
        WRITE(ISTDO,'(A)') ' ANIM ...'
        WRITE(ISTDO,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY'
        WRITE(IOUT,'(A)') ' ANIM ...'
        WRITE(IOUT,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY NODTAG_1'
        CALL ARRET(2)
      END IF
      ALLOCATE(NELENOD(3,NUMNOD+1), STAT=IERROR)
      IF (IERROR/=0) THEN
        WRITE(ISTDO,'(A)') ' ANIM ...'
        WRITE(ISTDO,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY'
        WRITE(IOUT,'(A)') ' ANIM ...'
        WRITE(IOUT,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY NELENOD'
        CALL ARRET(2)
      END IF
      ALLOCATE(ELSNOD(8*NUMELS), STAT=IERROR)
      IF (IERROR/=0) THEN
        WRITE(ISTDO,'(A)') ' ANIM ...'
        WRITE(ISTDO,'(A)')
     .     ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY'
        WRITE(IOUT,'(A)') ' ANIM ...'
        WRITE(IOUT,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY ELSNOD'
        CALL ARRET(2)
      END IF
      ALLOCATE(ELCNOD(4*NUMELC), STAT=IERROR)
      IF (IERROR/=0) THEN
        WRITE(ISTDO,'(A)') ' ANIM ...'
        WRITE(ISTDO,'(A)')
     .     ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY'
        WRITE(IOUT,'(A)') ' ANIM ...'
        WRITE(IOUT,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY ELCNOD'
        CALL ARRET(2)
      END IF
      ALLOCATE(ELTGNOD(3*NUMELTG), STAT=IERROR)
      IF (IERROR/=0) THEN
        WRITE(ISTDO,'(A)') ' ANIM ...'
        WRITE(ISTDO,'(A)')
     .     ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY'
        WRITE(IOUT,'(A)') ' ANIM ...'
        WRITE(IOUT,'(A)')
     .    ' UNABLE TO ALLOCATE MEMORY FOR WORK ARRAY ELTGNOD'
        CALL ARRET(2)
      END IF
C
      DO N=1,NUMELS
        FASTAG(N)=0
      END DO
C
      DO N=1,NUMNOD+1
        NELENOD(1,N)=0
        NELENOD(2,N)=0
        NELENOD(3,N)=0
      END DO
C
C     node -> solid
      DO N=1,NUMELS
        DO I=1,8
          NI=IXS(I+1,N)
          NODTAG_1(NI) = 0
        ENDDO
        DO I=1,8
          NI=IXS(I+1,N)
          IF (NODTAG_1(NI) == 0) NELENOD(1,NI+1)=NELENOD(1,NI+1)+1
          NODTAG_1(NI) = 1
        END DO
      END DO
C
      DO N=1,NUMNOD
        NELENOD(1,N+1)=NELENOD(1,N+1)+NELENOD(1,N)
      END DO
C
      DO N=1,NUMELS
        DO I=1,8
          NI=IXS(I+1,N)
          NODTAG_1(NI) = 0
        ENDDO
        DO I=1,8
          NI=IXS(I+1,N)
          IF (NODTAG_1(NI) == 0) THEN
            NELENOD(1,NI)=NELENOD(1,NI)+1
            ELSNOD(NELENOD(1,NI))=N
            NODTAG_1(NI) = 1
          ENDIF
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        NELENOD(1,N+1)=NELENOD(1,N)
      END DO
      NELENOD(1,1)=0
C
C     node -> 4node shell
      DO N=1,NUMELC
        DO I=1,4
          NI=IXC(I+1,N)
          NELENOD(2,NI+1)=NELENOD(2,NI+1)+1
        END DO
      END DO
C
      DO N=1,NUMNOD
        NELENOD(2,N+1)=NELENOD(2,N+1)+NELENOD(2,N)
      END DO
C
      DO N=1,NUMELC
        DO I=1,4
          NI=IXC(I+1,N)
          NELENOD(2,NI)=NELENOD(2,NI)+1
          ELCNOD(NELENOD(2,NI))=N
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        NELENOD(2,N+1)=NELENOD(2,N)
      END DO
      NELENOD(2,1)=0
C
C     node -> 3node shell
      DO N=1,NUMELTG
        DO I=1,3
          NI=IXTG(I+1,N)
          NELENOD(3,NI+1)=NELENOD(3,NI+1)+1
        END DO
      END DO
C
      DO N=1,NUMNOD
        NELENOD(3,N+1)=NELENOD(3,N+1)+NELENOD(3,N)
      END DO
C
      DO N=1,NUMELTG
        DO I=1,3
          NI=IXTG(I+1,N)
          NELENOD(3,NI)=NELENOD(3,NI)+1
          ELTGNOD(NELENOD(3,NI))=N
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        NELENOD(3,N+1)=NELENOD(3,N)
      END DO
      NELENOD(3,1)=0
C
      DO N=1,NUMNOD
        DO J=NELENOD(1,N)+1,NELENOD(1,N+1)
          JS=ELSNOD(J)
          DO K=NELENOD(1,N)+1,NELENOD(1,N+1)
           IF(K/=J)THEN
            DO II=1,8
              IF(IXS(II+1,JS)/=0) NODTAG(IXS(II+1,JS))=0
            END DO
            KS=ELSNOD(K)
            DO II=1,8
              NI=IXS(II+1,KS)
              NODTAG_1(NI) = 0
            ENDDO
            DO II=1,8
              NI=IXS(II+1,KS)
              IF (NODTAG_1(NI) == 0) THEN
                NODTAG(IXS(II+1,KS))=NODTAG(IXS(II+1,KS))+1
                NODTAG_1(NI) = 1
              ENDIF
            END DO
            NN=0
            DO JJ=1,6
              LL=FASTAG(JS)
              IF(MOD(LL,PWR(JJ+1))/PWR(JJ)==0)THEN
                NN=0
                DO KK=1,4
          	  IF(IXS(FACES(KK,JJ)+1,JS)/=0)
     .              NN=NN+NODTAG(IXS(FACES(KK,JJ)+1,JS))
                END DO
                IF(NN == 4)THEN
C                 FACTAG(JS) moins face jj
                  FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                END IF
              END IF
            END DO
           END IF
          END DO
C
          DO K=NELENOD(2,N)+1,NELENOD(2,N+1)
            DO II=1,8
              IF(IXS(II+1,JS)/=0) NODTAG(IXS(II+1,JS))=0
            END DO
            KS=ELCNOD(K)
            DO II=1,4
              IF(IXC(II+1,KS)/=0)
     .          NODTAG(IXC(II+1,KS))=NODTAG(IXC(II+1,KS))+1
            END DO
            NN=0
            DO JJ=1,6
              LL=FASTAG(JS)
              IF(MOD(LL,PWR(JJ+1))/PWR(JJ)==0)THEN
                NN=0
                DO KK=1,4
          	  IF(IXS(FACES(KK,JJ)+1,JS)/=0)
     .              NN=NN+NODTAG(IXS(FACES(KK,JJ)+1,JS))
                END DO
                IF(NN==4)THEN
C                 FACTAG(JS) moins face jj
                  FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                END IF
              END IF
            END DO
          END DO
C
          DO K=NELENOD(3,N)+1,NELENOD(3,N+1)
            DO II=1,8
              IF(IXS(II+1,JS)/=0) NODTAG(IXS(II+1,JS))=0
            END DO
            KS=ELTGNOD(K)
            DO II=1,4
              IF(IXTG(II+1,KS)/=0)
     .          NODTAG(IXTG(II+1,KS))=NODTAG(IXTG(II+1,KS))+1
            END DO
            NN=0
            DO JJ=1,6
              LL=FASTAG(JS)
              IF(MOD(LL,PWR(JJ+1))/PWR(JJ)==0)THEN
                NN=0
                DO KK=1,4
          	  IF(IXS(FACES(KK,JJ)+1,JS)/=0)
     .              NN=NN+NODTAG(IXS(FACES(KK,JJ)+1,JS))
                END DO
                IF(NN==4)THEN
C                 FACTAG(JS) moins face jj
                  FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                END IF
              END IF
            END DO
          END DO
        END DO
      END DO
C
      NFASOLFR=0
      DO N=1,NUMELS
        LL=FASTAG(N)
        DO JJ=1,6
          IF(MOD(LL,PWR(JJ+1))/PWR(JJ)==0)THEN
            NFASOLFR=NFASOLFR+1
C            FASOLFR(1,NFASOLFR)=N
C            FASOLFR(2,NFASOLFR)=JJ
          END IF
        END DO
      END DO
      DEALLOCATE(ELTGNOD, ELCNOD, ELSNOD, NELENOD,NODTAG,NODTAG_1)
C     
      RETURN
      END
Chd|====================================================================
Chd|  ANI_FASOLFR2                  source/output/anim/ani_fasolfr.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ANI_FASOLFR2(FASTAG  ,FASOLFR, ISOLNOD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER
     .   FASTAG(*), FASOLFR(2,*), ISOLNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,JJ,LL
      INTEGER PWR(7)
      DATA PWR/1,2,4,8,16,32,64/
C     REAL
C-----------------------------------------------
C
      NFASOLFR=0
      DO N=1,NUMELS
       LL=FASTAG(N)
        DO JJ=1,6
          IF(MOD(LL,PWR(JJ+1))/PWR(JJ)==0)THEN
            NFASOLFR=NFASOLFR+1
            FASOLFR(1,NFASOLFR)=N
            FASOLFR(2,NFASOLFR)=JJ
          END IF
        END DO
      END DO
      RETURN
      END
