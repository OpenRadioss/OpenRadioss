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
Chd|  RBE3V                         source/constraints/general/rbe3/rbe3v.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|====================================================================
      SUBROUTINE RBE3V(IRBE3 ,LRBE3 ,X    ,A     ,AR    ,
     1                 V     ,VR    ,FRBE3,SKEW  )
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IRBE3(NRBE3L,*),LRBE3(*)
C     REAL
        my_real
     .     X(3,*), A(3,*), AR(3,*), FRBE3(*),SKEW(*),
     .     V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .          JT(3,NRBE3),JR(3,NRBE3),NM,NN,K
C     REAL
        my_real
     .          VS(3),VRS(3),AS(3),ARS(3)
        my_real,
     .           DIMENSION(:,:,:),ALLOCATABLE :: FDSTNB ,MDSTNB

C======================================================================|
        IADS = SLRBE3/2
        CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
        ALLOCATE(FDSTNB(3,6,MAX_M))
        IF (IROTG>0) ALLOCATE(MDSTNB(3,6,MAX_M))
        DO N= NRBE3,1,-1
          IAD = IRBE3(1,N)
          NS  = IRBE3(3,N)
          NML = IRBE3(5,N)
          IF (NS==0) CYCLE
          IROT =MIN(IRBE3(6,N),IRODDL)
          CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X      ,
     .                FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .                MDSTNB  ,IRBE3(2,N))
          DO J = 1,3
            VS(J) = ZERO
            VRS(J) = ZERO
            AS(J) = ZERO
            ARS(J) = ZERO
          ENDDO
          DO I=1,NML
            M = LRBE3(IAD+I)
            DO J = 1,3
              DO K = 1,3
                VS(J) = VS(J)+FDSTNB(K,J,I)*V(K,M)
                AS(J) = AS(J)+FDSTNB(K,J,I)*A(K,M)
                VRS(J) = VRS(J)+FDSTNB(K,J+3,I)*V(K,M)
                ARS(J) = ARS(J)+FDSTNB(K,J+3,I)*A(K,M)
              ENDDO
            ENDDO
          ENDDO
          IF (IROT>0) THEN
            DO I=1,NML
              M = LRBE3(IAD+I)
              DO J = 1,3
                DO K = 1,3
                  VS(J) = VS(J)+MDSTNB(K,J,I)*VR(K,M)
                  AS(J) = AS(J)+MDSTNB(K,J,I)*AR(K,M)
                  VRS(J) = VRS(J)+MDSTNB(K,J+3,I)*VR(K,M)
                  ARS(J) = ARS(J)+MDSTNB(K,J+3,I)*AR(K,M)
                ENDDO
              ENDDO
            ENDDO
          ENDIF
          DO J = 1,3
            V(J,NS) = VS(J) *JT(J,N)
            A(J,NS) = AS(J) *JT(J,N)
          ENDDO
          IF ((JR(1,N)+JR(2,N)+JR(3,N))>0) THEN
            DO J = 1,3
              VR(J,NS) = VRS(J) *JR(J,N)
              AR(J,NS) = ARS(J) *JR(J,N)
            ENDDO
          ENDIF
        ENDDO
C
        DEALLOCATE(FDSTNB)
        IF (IROTG>0) DEALLOCATE(MDSTNB)
C---
        RETURN
      END
Chd|====================================================================
Chd|  RBE3_IMPD                     source/constraints/general/rbe3/rbe3v.F
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F
Chd|-- calls ---------------
Chd|        PRERBE3                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|====================================================================
      SUBROUTINE RBE3_IMPD(IRBE3 ,LRBE3 ,X    ,D     ,DR    ,
     1                     FRBE3 ,SKEW  )
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IRBE3(NRBE3L,*),LRBE3(*)
C     REAL
        my_real
     .     X(3,*), D(3,*), DR(3,*),  FRBE3(*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,MAX_M,IROTG,
     .          JT(3,NRBE3),JR(3,NRBE3),NM,NN,K
C     REAL
        my_real
     .          VS(3),VRS(3)
        my_real,
     .           DIMENSION(:,:,:),ALLOCATABLE :: FDSTNB ,MDSTNB

C======================================================================|
        IADS = SLRBE3/2
        CALL PRERBE3(IRBE3 ,MAX_M , IROTG,JT  ,JR   )
        ALLOCATE(FDSTNB(3,6,MAX_M))
        IF (IROTG>0) ALLOCATE(MDSTNB(3,6,MAX_M))
        DO N=1,NRBE3
          IAD = IRBE3(1,N)
          NS  = IRBE3(3,N)
          IF (NS==0) CYCLE
          NML = IRBE3(5,N)
          IROT =MIN(IRBE3(6,N),IRODDL)
          CALL RBE3CL(LRBE3(IAD+1),LRBE3(IADS+IAD+1),NS     ,X    ,
     .                FRBE3(6*IAD+1),SKEW    ,NML     ,IROT   ,FDSTNB ,
     .                MDSTNB  ,IRBE3(2,N))
          DO J = 1,3
            VS(J) = ZERO
            VRS(J) = ZERO
          ENDDO
          DO I=1,NML
            M = LRBE3(IAD+I)
            DO J = 1,3
              DO K = 1,3
                VS(J) = VS(J)+FDSTNB(K,J,I)*D(K,M)
                VRS(J) = VRS(J)+FDSTNB(K,J+3,I)*D(K,M)
              ENDDO
            ENDDO
          ENDDO
          IF (IROT>0) THEN
            DO I=1,NML
              M = LRBE3(IAD+I)
              DO J = 1,3
                DO K = 1,3
                  VS(J) = VS(J)+MDSTNB(K,J,I)*DR(K,M)
                  VRS(J) = VRS(J)+MDSTNB(K,J+3,I)*DR(K,M)
                ENDDO
              ENDDO
            ENDDO
          ENDIF
          DO J = 1,3
            D(J,NS) = VS(J) *JT(J,N)
          ENDDO
          IF ((JR(1,N)+JR(2,N)+JR(3,N))>0) THEN
            DO J = 1,3
              DR(J,NS) = VRS(J) *JR(J,N)
            ENDDO
          ENDIF
        ENDDO
C
        DEALLOCATE(FDSTNB)
        IF (IROTG>0) DEALLOCATE(MDSTNB)
C---
        RETURN
      END
Chd|====================================================================
Chd|  RBE3_FRD                      source/constraints/general/rbe3/rbe3v.F
Chd|-- called by -----------
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE3_FRD(NML    ,IML    ,NS    ,D     ,DR    ,
     1                    FDSTNB ,MDSTNB ,JT    ,JR    ,IROT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NML,IML(*),NS,IROT,JT(*),JR(*)
        my_real
     .     D(3,*), DR(3,*),  FDSTNB(3,6,*) ,MDSTNB(3,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, N, M, IAD,JJ,NM,NN,K
        my_real
     .          VS(3),VRS(3)

C======================================================================|
        DO J = 1,3
          VS(J) = ZERO
          VRS(J) = ZERO
        ENDDO
        DO I=1,NML
          M = IML(I)
          DO J = 1,3
            DO K = 1,3
              VS(J) = VS(J)+FDSTNB(K,J,I)*D(K,M)
              VRS(J) = VRS(J)+FDSTNB(K,J+3,I)*D(K,M)
            ENDDO
          ENDDO
        ENDDO
        IF (IROT>0) THEN
          DO I=1,NML
            M = IML(I)
            DO J = 1,3
              DO K = 1,3
                VS(J) = VS(J)+MDSTNB(K,J,I)*DR(K,M)
                VRS(J) = VRS(J)+MDSTNB(K,J+3,I)*DR(K,M)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        DO J = 1,3
          D(J,NS) = VS(J) *JT(J)
        ENDDO
        IF ((JR(1)+JR(2)+JR(3))>0) THEN
          DO J = 1,3
            DR(J,NS) = VRS(J) *JR(J)
          ENDDO
        ENDIF
C---
        RETURN
      END

