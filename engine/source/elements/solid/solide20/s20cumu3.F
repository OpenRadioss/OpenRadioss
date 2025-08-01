Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    s20cumu3   ../engine/source/elements/solid/solide20/s20cumu3.F
!||--- called by ------------------------------------------------------
!||    s16forc3   ../engine/source/elements/thickshell/solide16/s16forc3.F
!||    s20forc3   ../engine/source/elements/solid/solide20/s20forc3.F
!||====================================================================
      SUBROUTINE S20CUMU3(
     1   OFFG,    A,       NC,      STIFN,
     2   STIG,    FX,      FY,      FZ,
     3   IPERM1,  IPERM2,  NPE,     THEM,
     4   FTHE,    CONDN,   CONDEG,  NEL,
     5   JTHE     ,NODADT_THERM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: NODADT_THERM
      INTEGER NPE
      INTEGER IPERM1(NPE),IPERM2(NPE),NC(MVSIZ,NPE)
C     REAL
      my_real
     .   OFFG(*),A(3,*),STIFN(*),STIG(MVSIZ,NPE),
     .   FX(MVSIZ,NPE), FY(MVSIZ,NPE), FZ(MVSIZ,NPE),
     .   THEM(MVSIZ,NPE),FTHE(*),CONDN(*),CONDEG(MVSIZ,NPE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N, N1,N2,NN,J
      my_real OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=1,NEL
Cf small3b        IF(OFF(I)<1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO N=1,NPE
          DO I=1,NEL
            IF(OFFG(I)<ZERO)THEN
              FX(I,N)=ZERO
              FY(I,N)=ZERO
              FZ(I,N)=ZERO
              STIG(I,N)=ZERO
           ENDIF
          ENDDO
        ENDDO
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L<=ZERO)THEN
        DO J=1,NPE
         DO I=1,NEL
          IF(OFFG(I)<=ZERO)THEN
             THEM(I,J)=ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(NODADT_THERM == 1) THEN
        IF(OFF_L<ZERO)THEN
         DO N=1,NPE
          DO I=1,NEL
           IF(OFFG(I)<ZERO)THEN
              CONDEG(I,N)=ZERO
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDIF  
      ENDIF
C
      IF(JTHE < 0 ) THEN
       IF(NODADT_THERM == 1 ) THEN  
         DO N=1,8
          DO I=1,NEL
            NN = NC(I,N)
            A(1,NN)=A(1,NN)+FX(I,N)
            A(2,NN)=A(2,NN)+FY(I,N)
            A(3,NN)=A(3,NN)+FZ(I,N)
            STIFN(NN)=STIFN(NN)+STIG(I,N)
            FTHE(NN) = FTHE(NN)+ THEM(I,N)
            CONDN(NN)=CONDN(NN)+CONDEG(I,N)
          ENDDO
         ENDDO
C
         DO N=9,NPE
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
              A(1,NN)=A(1,NN)+FX(I,N)
              A(2,NN)=A(2,NN)+FY(I,N)
              A(3,NN)=A(3,NN)+FZ(I,N)
              STIFN(NN)=STIFN(NN)+STIG(I,N)
              FTHE(NN) = FTHE(NN)+ THEM(I,N)
              CONDN(NN)=CONDN(NN)+CONDEG(I,N)
            ELSE
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              STIFN(N1)=STIFN(N1)+HALF*STIG(I,N)
              FTHE(N1) = FTHE(N1)+ HALF*THEM(I,N)
              CONDN(N1)= CONDN(N1)+HALF*CONDEG(I,N)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
              STIFN(N2)=STIFN(N2)+HALF*STIG(I,N)
              FTHE(N2) = FTHE(N2)+ HALF*THEM(I,N)
              CONDN(N2)= CONDN(N2)+HALF*CONDEG(I,N)
            ENDIF
          ENDDO
         ENDDO
        ELSE
         DO N=1,8
          DO I=1,NEL
            NN = NC(I,N)
            A(1,NN)=A(1,NN)+FX(I,N)
            A(2,NN)=A(2,NN)+FY(I,N)
            A(3,NN)=A(3,NN)+FZ(I,N)
            STIFN(NN)=STIFN(NN)+STIG(I,N)
            FTHE(NN) = FTHE(NN)+ THEM(I,N)
          ENDDO
         ENDDO
C
         DO N=9,NPE
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
              A(1,NN)=A(1,NN)+FX(I,N)
              A(2,NN)=A(2,NN)+FY(I,N)
              A(3,NN)=A(3,NN)+FZ(I,N)
              STIFN(NN)=STIFN(NN)+STIG(I,N)
              FTHE(NN) = FTHE(NN)+ THEM(I,N)
            ELSE
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              STIFN(N1)=STIFN(N1)+HALF*STIG(I,N)
              FTHE(N1) = FTHE(N1)+ HALF*THEM(I,N)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
              STIFN(N2)=STIFN(N2)+HALF*STIG(I,N)
              FTHE(N2) = FTHE(N2)+ HALF*THEM(I,N)
            ENDIF
          ENDDO
         ENDDO
       ENDIF
      ELSE
          DO N=1,8
           DO I=1,NEL
             NN = NC(I,N)
             A(1,NN)=A(1,NN)+FX(I,N)
             A(2,NN)=A(2,NN)+FY(I,N)
             A(3,NN)=A(3,NN)+FZ(I,N)
             STIFN(NN)=STIFN(NN)+STIG(I,N)
           ENDDO
          ENDDO
C
          DO N=9,NPE
           DO I=1,NEL
             NN = NC(I,N)
             IF(NN/=0)THEN
               A(1,NN)=A(1,NN)+FX(I,N)
               A(2,NN)=A(2,NN)+FY(I,N)
               A(3,NN)=A(3,NN)+FZ(I,N)
               STIFN(NN)=STIFN(NN)+STIG(I,N)
             ELSE
               N1=NC(I,IPERM1(N))
               N2=NC(I,IPERM2(N))
               A(1,N1)=A(1,N1)+HALF*FX(I,N)
               A(2,N1)=A(2,N1)+HALF*FY(I,N)
               A(3,N1)=A(3,N1)+HALF*FZ(I,N)
               STIFN(N1)=STIFN(N1)+HALF*STIG(I,N)
               A(1,N2)=A(1,N2)+HALF*FX(I,N)
               A(2,N2)=A(2,N2)+HALF*FY(I,N)
               A(3,N2)=A(3,N2)+HALF*FZ(I,N)
               STIFN(N2)=STIFN(N2)+HALF*STIG(I,N)   
             ENDIF
           ENDDO
          ENDDO 
      ENDIF 
      IF(NSECT>0)THEN
       DO N=9,NPE
        DO I=1,NEL
          NN = NC(I,N)
          IF(NN==0)THEN
            N1=IPERM1(N)
            N2=IPERM2(N)
            FX(I,N1)=FX(I,N1)+HALF*FX(I,N)
            FY(I,N1)=FY(I,N1)+HALF*FY(I,N)
            FZ(I,N1)=FZ(I,N1)+HALF*FZ(I,N)
            FX(I,N2)=FX(I,N2)+HALF*FX(I,N)
            FY(I,N2)=FY(I,N2)+HALF*FY(I,N)
            FZ(I,N2)=FZ(I,N2)+HALF*FZ(I,N)
C           FX(I,N)=0.
C           FY(I,N)=0.
C           FZ(I,N)=0.
          END IF
        END DO
       END DO
      END IF
      RETURN
      END
