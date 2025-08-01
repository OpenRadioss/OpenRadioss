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
!||    outri          ../starter/source/materials/time_step/outri.F
!||--- called by ------------------------------------------------------
!||    lectur         ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod    ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE OUTRI(DTELEM,IXS   ,IXQ   ,IXC   ,IXT   ,
     .                 IXP   ,IXR   ,IXTG  ,KXX   ,
     .                 KXSP  ,KXIG3D,IGEO  ,NUMEL)
C
C     TRI DES DTEL (POUR CHAQUE TYPE D'ELEMENT) ET IMPRESSIONS
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*) 
      INTEGER IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*) 
      INTEGER KXX(NIXX,*), KXSP(NISP,*) ,KXIG3D(NIXIG3D,*),
     .        IGEO(NPROPGI,*)
      my_real
     .   DTELEM(2*NUMEL)
      INTEGER,INTENT(IN) :: NUMEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NUM2, I, NUMIMP, NUMELO, NUM1, IS_PROP45     
      REAL*4 VINGTR4, TEMPO
      INTEGER :: IERROR
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERM
      DATA VINGTR4 /20./
C=======================================================================
C     INITIALISATION DES NOS INTERNES DES ELEMENTS AVANT TRI
C
      CALL MY_ALLOC(PERM,NUMEL)
      ! --------------------
      ! solid and quad
      NUM2 = 0
      DO I=1,NUMELS+NUMELQ
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! shell
      NUM2=NUMELS+NUMELQ
      DO I=1,NUMELC
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! truss
      NUM2=NUM2+NUMELC
      DO I=1,NUMELT
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! beam
      NUM2=NUM2+NUMELT
      DO I=1,NUMELP
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! spring
      NUM2=NUM2+NUMELP
      DO I=1,NUMELR
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! triangle
      NUM2=NUM2+NUMELR
      DO I=1,NUMELTG
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! the X element :)
      NUM2=NUM2+NUMELTG
      DO  I=1,NUMELX
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! sph
      NUM2=NUM2+NUMELX
      DO I=1,NUMSPH
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
      ! igeo element
      NUM2=NUM2+NUMSPH
      DO  I=1,NUMELIG3D
        PERM(NUM2+I)=I
      ENDDO
      ! --------------------
C
C     TRIS DES ELEMENTS EN FONCTION DU PAS DE TEMPS
C
      IF (NUMELS>1) THEN
        NUM2 = 1
        CALL MYQSORT(NUMELS,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELQ>1) THEN
        NUM2 = 1
        CALL MYQSORT(NUMELQ,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELC>1) THEN
        NUM2 = NUMELS+1
        CALL MYQSORT(NUMELC,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELT>1) THEN
        NUM2 = NUMELS+NUMELC+1
        CALL MYQSORT(NUMELT,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELP>1) THEN
        NUM2 = NUMELS+NUMELC+NUMELT+1
        CALL MYQSORT(NUMELP,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELR>1) THEN
        NUM2 = NUMELS+NUMELC+NUMELT+NUMELP+1
        CALL MYQSORT(NUMELR,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELTG>1) THEN
          NUM2=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+1
          CALL MYQSORT(NUMELTG,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELX>1) THEN
          NUM2=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG+1
          CALL MYQSORT(NUMELX,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMSPH>1) THEN
        NUM2=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG+NUMELX+1
        CALL MYQSORT(NUMSPH,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      IF (NUMELIG3D>1) THEN
        NUM2=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG+NUMELX+
     .       NUMSPH+1
        CALL MYQSORT(NUMELIG3D,DTELEM(NUM2),PERM(NUM2),IERROR)
      ENDIF
      
      DTELEM(NUMEL+1:2*NUMEL) = PERM(1:NUMEL)

C
C     IMPRESSIONS PAR GROUPES DES ELEMENTS TRIES
C
      IF (NUMELS>0) THEN
        TEMPO = NUMELS*TWOEM2
        NUMIMP=MIN0(NUMELS,MAX1(VINGTR4,TEMPO))
        WRITE(IOUT,1000)
        WRITE(IOUT,1001)
        DO I=1,NUMIMP
          NUMELO=NINT(DTELEM(NUMEL+I))
          WRITE(IOUT,1002)DTELEM(I),IXS(11,NUMELO)
        END DO
      ENDIF

      IF (NUMELQ>0) THEN
        TEMPO = NUMELQ*TWOEM2
        NUMIMP=MIN0(NUMELQ,MAX1(VINGTR4,TEMPO))
        WRITE(IOUT,1000)
        WRITE(IOUT,1001)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUMEL+I))
           WRITE(IOUT,1002)DTELEM(I),IXQ(7,NUMELO)
        END DO
      ENDIF

      IF(NUMELC>0) THEN
        TEMPO = NUMELC*TWOEM2
        NUMIMP=MIN0(NUMELC,MAX1(VINGTR4,TEMPO))
        NUM2=NUMEL+NUMELS
        WRITE(IOUT,2000)
        WRITE(IOUT,1001)
c        IF(NUMELC>1)THEN
c          CALL ANCHECK(92)
c        END IF
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUMELS+I),IXC(7,NUMELO)
        END DO
      ENDIF

      IF(NUMELT>0) THEN
        TEMPO = NUMELT*TWOEM2
        NUMIMP=MIN0(NUMELT,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELQ+NUMELC
        NUM2=NUM1+NUMEL
        WRITE(IOUT,3000)
        WRITE(IOUT,1001)
c        CALL ANCHECK(94)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),
     .                     IXT(5,NUMELO)
        END DO
      ENDIF

      IF(NUMELP>0) THEN
        TEMPO = NUMELP*TWOEM2
        NUMIMP=MIN0(NUMELP,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT
        NUM2=NUM1+NUMEL
        WRITE(IOUT,4000)
        WRITE(IOUT,1001)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),IXP(6,NUMELO)
        END DO
      ENDIF

      IS_PROP45 = 0
      IF(NUMELR>0) THEN
        TEMPO = NUMELR*TWOEM2
        NUMIMP=MIN0(NUMELR,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT+NUMELP
        NUM2=NUM1+NUMEL
        WRITE(IOUT,5000)
        WRITE(IOUT,1001)
c        CALL ANCHECK(95)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           IF( IGEO(11,IXR(1,NUMELO)) == 45) THEN
             IS_PROP45 = 1
           ELSE
             WRITE(IOUT,1002)DTELEM(NUM1+I),IXR(6,NUMELO)
           ENDIF
        END DO
        IF (IS_PROP45 == 1)
     .       WRITE(IOUT,5001)
      ENDIF

      IF(NUMELTG>0 .AND. N2D == 0) THEN
        TEMPO = NUMELTG*TWOEM2
        NUMIMP=MIN0(NUMELTG,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR
        NUM2=NUM1+NUMEL
        WRITE(IOUT,6000)
        WRITE(IOUT,1001)
c        CALL ANCHECK(93)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),IXTG(6,NUMELO)
        END DO
      ENDIF

      IF(NUMELTG>0 .AND. N2D /= 0) THEN
        TEMPO = NUMELTG*TWOEM2
        NUMIMP=MIN0(NUMELTG,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR
        NUM2=NUM1+NUMEL
        WRITE(IOUT,10000)
        WRITE(IOUT,1001)
c        CALL ANCHECK(93)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),IXTG(6,NUMELO)
        END DO
      ENDIF
     
      IF(NUMELX>0) THEN
        TEMPO = NUMELX*TWOEM2
        NUMIMP=MIN0(NUMELX,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
        NUM2=NUM1+NUMEL
        WRITE(IOUT,7000)
        WRITE(IOUT,1001)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),KXX(5,NUMELO)
        END DO
      ENDIF
      
      IF(NUMSPH>0) THEN
        TEMPO = NUMSPH*TWOEM2
        NUMIMP=MIN0(NUMSPH,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG+NUMELX
        NUM2=NUM1+NUMEL
        WRITE(IOUT,8000)
        WRITE(IOUT,1001)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),KXSP(NISP,NUMELO)
        END DO
      ENDIF

      IF(NUMELIG3D>0) THEN
        TEMPO = NUMELIG3D*TWOEM2
        NUMIMP=MIN0(NUMELIG3D,MAX1(VINGTR4,TEMPO))
        NUM1=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG+NUMELX+
     .       NUMSPH
        NUM2=NUM1+NUMEL
        WRITE(IOUT,9000)
        WRITE(IOUT,1001)
        DO I=1,NUMIMP
           NUMELO=NINT(DTELEM(NUM2+I))
           WRITE(IOUT,1002)DTELEM(NUM1+I),KXIG3D(5,NUMELO)
        END DO
      ENDIF
      DEALLOCATE( PERM )
C--------------------------------------------------------
 1000 FORMAT(//,'         SOLID ELEMENTS TIME STEP')
 1001 FORMAT(  '         ------------------------',//,
     .         '      TIME STEP       ELEMENT NUMBER')
 1002 FORMAT(1X,1PG20.13,5X,I10)
 2000 FORMAT(/,'         SHELL ELEMENTS TIME STEP')
 3000 FORMAT(/,'         TRUSS ELEMENTS TIME STEP')
 4000 FORMAT(/,'         BEAM  ELEMENTS TIME STEP')
 5000 FORMAT(/,'         SPRING ELEMENTS TIME STEP')
 5001 FORMAT(/,'   Info : spring TYPE45 (KJOINT2) time step is evaluated at the beginning of the engine')
 6000 FORMAT(/,'         TRIANGULAR SHELL ELEMENTS TIME STEP')
50000 FORMAT(/,'         USER RNUR ELEMENTS TIME STEP')
 7000 FORMAT(/,'         MULTI-PURPOSE ELEMENTS TIME STEP')
 8000 FORMAT(/,'         SMOOTH PARTICLES TIME STEP')
 9000 FORMAT(/,'         ISO GEOMETRIC ELEMENTS TIME STEP')
10000 FORMAT(/,'         2D TRIA ELEMENTS TIME STEP')
C--------------------------------------------------------

      RETURN
      END
C
!||====================================================================
!||    outrin           ../starter/source/materials/time_step/outri.F
!||--- called by ------------------------------------------------------
!||    lectur           ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.F
!||    r2r_mod          ../starter/share/modules1/r2r_mod.F
!||====================================================================
      SUBROUTINE OUTRIN(MS,IN,STIFN,STIFR,ITAB,DTNODA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE R2R_MOD
      USE PLOT_CURVE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C     TRI DES DT NODAUX ET IMPRESSIONS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*)
      my_real
     .   MS(NUMNOD),IN(NUMNOD),STIFN(NUMNOD),STIFR(NUMNOD),DTNODA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,IMAX, OLD_NUMB, P80, NB_OF_COLUM, STORAGE, OLD_COMPT, COMPT
      my_real, DIMENSION(:), ALLOCATABLE :: DT
      my_real
     .   DTNODA_STAT(20),NB_NOD_STAT(20),CHUNK, DT_MAX, DT_MIN, SEUIL
      INTEGER :: IERROR
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERM
C=======================================================================
      CALL MY_ALLOC(PERM,NUMNOD)
      CALL MY_ALLOC(DT,NUMNOD)
      DTNODA = EP30
C 
      DO I=1,NUMNOD
        IF((MS(I)/=ZERO).AND.(STIFN(I)>EM20))THEN
          DT(I)=MS(I)/STIFN(I) 
        ELSE
          DT(I)=EP30       ! -1- free nodes dt=1e30 instead of dt=1416=sqrt(1e6).
                           ! -2- nodal time step from VOID elem (stifn<em20) is dt=EP30 too.
        ENDIF
      ENDDO
           
      IF(IRODDL/=0)THEN
        DO I=1,NUMNOD
          IF(IN(I)/=ZERO)THEN
            DT(I)=MIN(DT(I),IN(I)/STIFR(I))
          ENDIF
        ENDDO
      ENDIF

      DO I=1,NUMNOD
             IF(DT(I)/=EP30)DT(I)=SQRT(ABS(TWO*DT(I)))
      END DO
C---- Multidomains : Nodal time step is deactivated for split RBODY
      IF(NSUBDOM>0) THEN
        DO I=1,NUMNOD
          IF(TAGNO(I+N_PART)==3) DT(I) = EP30
        END DO
      END IF
C
      DO I=1,NUMNOD
        PERM(I)=I
      ENDDO

      CALL MYQSORT(NUMNOD,DT,PERM,IERROR)
C
      DTNODA = MIN(DTNODA,DT(1))
C
      IF ( N2D/=1) THEN        
        WRITE(IOUT,1000)
        WRITE(IOUT,1001)
        N=PERM(1)
c        CALL ANCHECK(96)      
        DO I=1,MIN(NUMNOD0,MAX(100,NUMNOD0/50))
          N=PERM(I) 
          WRITE(IOUT,1002)DT(I),ITAB(N)
        ENDDO
C
C
C----- Curve of nodal time step distribution
C
C     Determination of the scale of the graph (max and min)
C
        DT_MAX = EP30
        DO I=1,NUMNOD
          IF (DT(NUMNOD-I+1) < 1E7) THEN
            IMAX = NUMNOD-I+1
            DT_MAX = DT(NUMNOD-I+1)
            EXIT
          ENDIF
        ENDDO
        P80 = NINT(0.8*NUMNOD)
        DT_MAX = MIN(DT_MAX,DT(P80))
        DT_MIN = DT(1)
C
        NB_NOD_STAT(:)=ZERO
        DTNODA_STAT(:)=ZERO
        CHUNK = (DT_MAX-DT_MIN)/18.0
        COMPT = 2
        OLD_COMPT = 2
        OLD_NUMB = 1
        SEUIL = DT_MIN + CHUNK
C
C       Determination of the columns
C 
        DO I=1,NUMNOD
          STORAGE = 0
          IF (DT(I) > DT_MAX) EXIT
          DO WHILE ((DT(I) > SEUIL).AND.(COMPT<19))
            COMPT = COMPT+1
            SEUIL = SEUIL + CHUNK
            STORAGE = 1
          ENDDO
          IF (STORAGE == 1) THEN
            NB_NOD_STAT(OLD_COMPT) = (100.0*(I-OLD_NUMB))/(ONE*NUMNOD)
            OLD_NUMB = I
            OLD_COMPT = COMPT
          ENDIF
        ENDDO
C
        NB_NOD_STAT(COMPT) = (100.0*(I-OLD_NUMB))/(ONE*NUMNOD)
        NB_OF_COLUM = COMPT+1
C
C       Determination of time axis - DT(1) and DT(COMPT+1) are used for printout of the scale - 
C       dt is divided by dt_scale for agreement with nodal time step prinout
C
        DTNODA_STAT(1) = DT_MIN*(ONE-EM10)
        DTNODA_STAT(NB_OF_COLUM) = DT_MAX*(ONE+EM10)
        DO I=2,COMPT
          DTNODA_STAT(I) = (DT_MIN+CHUNK*(I-2)+HALF*CHUNK)  
        ENDDO
C
C----- Visual output of nodal time stemp distribution
C
        WRITE(IOUT,2003)
        WRITE(IOUT,2004)
        CALL PLOT_CURVE(DTNODA_STAT, NB_NOD_STAT, NB_OF_COLUM, INPUT_SIZE_X=60, INPUT_SIZE_Y=24, INPUT_CURVE_TYPE = 1,
     .                  INPUT_TXT_X="NODAL TIME STEP",INPUT_TXT_Y="% OF NODES")
      ENDIF
C

      DEALLOCATE( PERM )
      DEALLOCATE( DT )

C-----------
 1000 FORMAT(//,'         NODAL TIME STEP (estimation)')
 1001 FORMAT(   '         ---------------',//,
     .          '      TIME STEP       NODE NUMBER')
 1002 FORMAT(1X,1PG20.13,5X,I10)
 2003 FORMAT(//,'         NODAL TIME STEP DISTRIBUTION ')
 2004 FORMAT(   '         ----------------------------',//)
C-----------

      RETURN
      END
