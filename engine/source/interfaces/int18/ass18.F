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
Chd|  ASS18                         source/interfaces/int18/ass18.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE ASS18(NRTM  ,IRECT,A    ,X   ,SURF   ,
     2                 PRES  ,FCONT,IBAG ,FSAV,FSAVBAG,
     3                 FR_I18,ISKY ,FSKYI,H3D_DATA )
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C===============================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,IBAG,IRECT(4,*),FR_I18(*),ISKY(*)
      my_real 
     .    A(3,*),X(3,*),SURF(3,*),PRES(*),FCONT(3,*),FSAV(*),
     .    FSAVBAG(NTHVKI,*), FSKYI(LSKYI,*)
c     .    SURFN(NUMNOD),CDGX,CDGY,CDGZ,VOLUME,PBAG,ALPHA
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  ERROR,NSKYI18G,PMASTER,I,J,K,L,N,NN,NBAG,
     .         PERM(NRTM)
      my_real 
     .    PRESMED,PRESMOY,SURFACE,SURFLOC,AAA,BBB,PRESM,PRESVOL,
     .    CDGX,CDGY,CDGZ,VOLUME,PBAG,ALPHA,
     .   F1(NRTM), F2(NRTM), F3(NRTM), F4(NRTM), F5(NRTM)
       DOUBLE PRECISION I18F6(5,6), I18FB6(2,6)
C-----------------------------------------------
C
c      PRESMOY = ZERO
c      SURFACE = ZERO
c      VOLUME  = ZERO
c      PRESVOL = ZERO
c      CDGX = ZERO
c      CDGY = ZERO
c      CDGZ = ZERO
      NBAG = 1
c      PBAG = FSAVBAG(3,NBAG)-101325.
      DO I = 1, NRTM
        SURF(1,I) = SURF(1,I)*UNDEMI
        SURF(2,I) = SURF(2,I)*UNDEMI
        SURF(3,I) = SURF(3,I)*UNDEMI
        SURFLOC = SQRT(SURF(1,I)*SURF(1,I)
     .                +SURF(2,I)*SURF(2,I)
     .                +SURF(3,I)*SURF(3,I))
        NN = 4
        IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
        BBB = UN/NN
        AAA = SURFLOC*BBB
        CDGX = ZERO
        CDGY = ZERO
        CDGZ = ZERO
        DO J=1,NN
          N = IRECT(J,I)
c          SURFN(N) = SURFN(N) + AAA
          CDGX = CDGX + X(1,N)*AAA
          CDGY = CDGY + X(2,N)*AAA
          CDGZ = CDGZ + X(3,N)*AAA
        ENDDO
        F1(I) = SURFLOC
        F2(I) = PRES(I)
        F3(I) = CDGX
        F4(I) = CDGY
        F5(I) = CDGZ
c        SURFACE  = SURFACE  + SURFLOC
c        PRESMOY  = PRESMOY + PRES(I)
        PRES(I)  = PRES(I) / MAX(SURFLOC,EM20)
        SURF(1,I) = SURF(1,I)*BBB
        SURF(2,I) = SURF(2,I)*BBB
        SURF(3,I) = SURF(3,I)*BBB
      ENDDO
C
C Traitement Parith/ON avant echange
C
      DO K = 1, 6
        I18F6(1,K) = ZERO
        I18F6(2,K) = ZERO
        I18F6(3,K) = ZERO
        I18F6(4,K) = ZERO
        I18F6(5,K) = ZERO
      END DO

      CALL SUM_6_FLOAT(1 ,NRTM ,F1, I18F6(1,1), 5)
      CALL SUM_6_FLOAT(1 ,NRTM ,F2, I18F6(2,1), 5)
      CALL SUM_6_FLOAT(1 ,NRTM ,F3, I18F6(3,1), 5)
      CALL SUM_6_FLOAT(1 ,NRTM ,F4, I18F6(4,1), 5)
      CALL SUM_6_FLOAT(1 ,NRTM ,F5, I18F6(5,1), 5)

      IF(NSPMD.GT.1) THEN
        CALL SPMD_EXCH_FR6(FR_I18,I18F6,5*6)
      ENDIF

      SURFACE = I18F6(1,1)+I18F6(1,2)+I18F6(1,3)+
     +  	I18F6(1,4)+I18F6(1,5)+I18F6(1,6)
      PRESMOY = I18F6(2,1)+I18F6(2,2)+I18F6(2,3)+
     +  	I18F6(2,4)+I18F6(2,5)+I18F6(2,6)
      CDGX    = I18F6(3,1)+I18F6(3,2)+I18F6(3,3)+
     +  	I18F6(3,4)+I18F6(3,5)+I18F6(3,6)
      CDGY    = I18F6(4,1)+I18F6(4,2)+I18F6(4,3)+
     +  	I18F6(4,4)+I18F6(4,5)+I18F6(4,6)
      CDGZ    = I18F6(5,1)+I18F6(5,2)+I18F6(5,3)+
     +  	I18F6(5,4)+I18F6(5,5)+I18F6(5,6)

C
      PRESMOY = PRESMOY / SURFACE
C
      CDGX = CDGX / SURFACE
      CDGY = CDGY / SURFACE
      CDGZ = CDGZ / SURFACE
C
      DO I = 1, NRTM
        N = IRECT(1,I)
        NN = 4
        IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
        VOLUME = ZERO
        PRESVOL = ZERO
        DO J=1,NN
          N = IRECT(J,I)
          AAA = SURF(1,I) * (X(1,N)-CDGX)
     .        + SURF(2,I) * (X(2,N)-CDGY)
     .        + SURF(3,I) * (X(3,N)-CDGZ) 
          VOLUME  = VOLUME + AAA
          PRESVOL = PRESVOL + PRES(I) * AAA
        ENDDO
        F1(I) = VOLUME
        F2(I) = PRESVOL
      ENDDO
C
C Traitement Parith/ON avant echange
C
      DO K = 1, 6
        I18FB6(1,K) = ZERO
        I18FB6(2,K) = ZERO
      END DO

      CALL SUM_6_FLOAT(1 ,NRTM ,F1, I18FB6(1,1),2)
      CALL SUM_6_FLOAT(1 ,NRTM ,F2, I18FB6(2,1),2)

      IF(NSPMD.GT.1) THEN
        CALL SPMD_EXCH_FR6(FR_I18,I18FB6,2*6)
      ENDIF
C
      VOLUME  = VOLUME / TROIS
      PRESVOL = PRESVOL / TROIS
C
      PRESVOL = PRESVOL / VOLUME
C
      ALPHA = UN
c      IF(IBAG==5)THEN
c        IF(PRESVOL>ZERO.AND.PBAG>PRESVOL)THEN
c          ALPHA = PBAG/PRESVOL
c        ENDIF
c        DO I = 1, NRTM
c          NN = 4
c          IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
c          DO J=1,NN
c              N = IRECT(J,I)
c              AAA = (ALPHA-UN)*PRES(I)-PBAG
c              A(1,N) = A(1,N) + AAA*SURF(1,I)
c              A(2,N) = A(2,N) + AAA*SURF(2,I)
c              A(3,N) = A(3,N) + AAA*SURF(3,I)
c          ENDDO
c        ENDDO
c      ENDIF
      
c      IF(ANIM_V(4)+OUTP_V(4).GT.0.AND.
c     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.
c     .              (MANIM.GE.4.AND.MANIM.LE.15)))THEN
c       DO I=1,NRTM
c        NN = 4
c        IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
c         DO J=1,NN
c           N = IRECT(J,I)
c           AAA = ALPHA*PRES(I)/MAX(SURFN(N),EM20)
c           FCONT(1,N) = FCONT(1,N) + AAA*SURF(1,I)
c           FCONT(2,N) = FCONT(2,N) + AAA*SURF(2,I)
c           FCONT(3,N) = FCONT(3,N) + AAA*SURF(3,I)
c         ENDDO
c       ENDDO
c      ENDIF
C
      PRESMED = ZERO
C
      IF(IBAG==1)THEN
         CALL MYQSORT(NRTM,PRES,PERM,ERROR)
         PRESMED = PRES((NRTM+1)/2)
         PRESM = PRESMED
      ELSEIF(IBAG==2)THEN
         PRESM = PRESMOY
      ELSEIF(IBAG==3)THEN
         PRESM = PRESVOL
c     ELSEIF(IBAG==4)THEN
c         IF(PRESMED<ZERO)THEN
c           PRESM = PBAG
c         ELSEIF(PRESMED<PRESVOL)THEN
c           PRESM = PBAG * (UN-PRESMED/PRESVOL) + PRESMED
c         ELSE
c           PRESM = PRESVOL
c         ENDIF
c     ELSEIF(IBAG==5)THEN
c           PRESM = ALPHA
      ENDIF
C------------------------------------------------------
c      IF(IBAG.NE.5)THEN
      IF(IPARIT.EQ.0)THEN
        DO I = 1, NRTM
          NN = 4
          IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
          DO J=1,NN
            N = IRECT(J,I)
            A(1,N) = A(1,N) - PRESM*SURF(1,I)
            A(2,N) = A(2,N) - PRESM*SURF(2,I)
            A(3,N) = A(3,N) - PRESM*SURF(3,I)
          ENDDO
        ENDDO
      ELSE
        IF(KDTINT.EQ.0)THEN
         DO I = 1, NRTM
           NN = 4
           IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
           DO J=1,NN
             N = IRECT(J,I)
             NISKY = NISKY + 1
             ISKY(NISKY) = N
             FSKYI(NISKY,1)= -PRESM*SURF(1,I)	    ! addition en P/ON idem interface
             FSKYI(NISKY,2)= -PRESM*SURF(2,I)
             FSKYI(NISKY,3)= -PRESM*SURF(3,I)
             FSKYI(NISKY,4)= ZERO
           ENDDO
         ENDDO
        ELSE
         DO I = 1, NRTM
           NN = 4
           IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
           DO J=1,NN
             N = IRECT(J,I)
             NISKY = NISKY + 1
             ISKY(NISKY) = N
             FSKYI(NISKY,1)= -PRESM*SURF(1,I)	    ! addition en P/ON idem interface
             FSKYI(NISKY,2)= -PRESM*SURF(2,I)
             FSKYI(NISKY,3)= -PRESM*SURF(3,I)
             FSKYI(NISKY,4)= ZERO
             FSKYI(NISKY,5)= ZERO
           ENDDO
         ENDDO
        END IF
      END IF
C------------------------------------------------------
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT .GT.0.AND.
     .              (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D .OR.
     .                  (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
C
            DO I=1,NRTM
              NN = 4
              IF(IRECT(3,I).EQ.IRECT(4,I))NN=3
              DO J=1,NN
                N = IRECT(J,I)
c                AAA = PRESM/MAX(SURFN(N),EM20)
c                FCONT(1,N) = FCONT(1,N) - AAA*SURF(1,I)
c                FCONT(2,N) = FCONT(2,N) - AAA*SURF(2,I)
c                FCONT(3,N) = FCONT(3,N) - AAA*SURF(3,I)
                FCONT(1,N) = FCONT(1,N) - PRESM*SURF(1,I)
                FCONT(2,N) = FCONT(2,N) - PRESM*SURF(2,I)
                FCONT(3,N) = FCONT(3,N) - PRESM*SURF(3,I)
              ENDDO
            ENDDO
      ENDIF
C-------------------------------------------------------
      IF(ISPMD.EQ.0)THEN
        FSAV(16)=PRESVOL
        FSAV(17)=PRESMOY
        FSAV(18)=PRESMED
        FSAV(19)=PRESM
        FSAV(20)=VOLUME
        FSAV(21)=SURFACE
      END IF
C
      RETURN
      END
