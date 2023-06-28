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
Chd|  FVBAG2                        source/airbag/fvbag2.F        
Chd|-- called by -----------
Chd|        FVBAG0                        source/airbag/fvbag0.F        
Chd|-- calls ---------------
Chd|        SPMD_IBCAST_SUBCOMM           source/mpi/generic/spmd_ibcast_subcomm.F
Chd|        SPMD_RBCAST_SUBCOMM           source/mpi/generic/spmd_rbcast_subcomm.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVBAG2(IFV,   ITYP, NNA,  NVENT, NJET, IVOLU, IBAGHOL, RBAGHOL,
     .                  X,    RVOLU, XXXA, NCONA, RBAGJET ,
     .                  A,    LGAUGE , GAUGE , NNT   ,FEXT   , NSKIP,
     .                  H3D_DATA     ) 
C
C Broadcast data from PMAIN to other processors
C Arrays sent:
C - GGG, GGA
C - AAA
C - XXXA
C - IVOLU(49) (number of volumes, needed to switch to UP 
C - RVOLU, RBAGJET, IBAGHOL(1,:) broadcasted if switch to UP
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFV, ITYP, NNA, NVENT, NJET, IVOLU(*), NCONA(16,*), IBAGHOL(NIBHOL,*)
      my_real RVOLU(*), X(3,*), XXXA(3,*), RBAGJET(NRBJET,*), RBAGHOL(NRBHOL,*)
      INTEGER NNT,LGAUGE(3,*) 
      INTEGER NSKIP          
      my_real
     .        A(3,*),
     .        FEXT(3,*), 
     .        GAUGE(LLGAUGE,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,J,N21,N22,N1
      INTEGER I, I1, I2, IFVENT, NPOLH  
      INTEGER IDEF(NVENT) 
      my_real TTF
      LOGICAL :: UP_SWITCH, AUTO_SWITCH


      IF(NSKIP >= 1 ) GOTO 90
C Communications only if FVBAG1 has not been skipped 
      IF(NBGAUGE > 0)THEN
        IF(NSPMD > 1 .AND. FVSPMD(IFV)%RANK > -1)THEN
          CALL SPMD_RBCAST_SUBCOMM(
     .         FVSPMD(IFV)%GGG,3*NNT,0,FVSPMD(IFV)%MPI_COMM)
          CALL SPMD_RBCAST_SUBCOMM(
     .         FVSPMD(IFV)%GGA,3*NNA,0,FVSPMD(IFV)%MPI_COMM)
        ENDIF
C
       II=0
       DO I=1,NBGAUGE
         IF(LGAUGE(1,I) <=0 .AND. LGAUGE(1,I) >= -NUMELS) II=1
       ENDDO
       IF(II == 0) GO TO 1200

       DO I=1,NBGAUGE
          IF(LGAUGE(1,I) > 0 .OR. LGAUGE(1,I) < -NUMELS) CYCLE
          IF(LGAUGE(1,I) == 0 . AND. LGAUGE(3,I) > 0) THEN
C Node input
            N1=LGAUGE(3,I)
            DO J=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
              N21=FVSPMD(IFV)%IBUF_L(1,J)
              N22=FVSPMD(IFV)%IBUF_L(2,J)
              IF(N22/=N1) CYCLE
              GAUGE(30,I)=FVSPMD(IFV)%GGG(1,N21)
              GAUGE(31,I)=FVSPMD(IFV)%GGG(2,N21)
              GAUGE(32,I)=FVSPMD(IFV)%GGG(3,N21)
              GO TO 800
            ENDDO
            DO J=1,FVSPMD(IFV)%NNA_L
              N21=FVSPMD(IFV)%IBUFA_L(1,J)
              N22=FVSPMD(IFV)%IBUFA_L(2,J)
              IF(N22/=N1) CYCLE
              GAUGE(30,I)=FVSPMD(IFV)%GGA(1,N21)
              GAUGE(31,I)=FVSPMD(IFV)%GGA(2,N21)
              GAUGE(32,I)=FVSPMD(IFV)%GGA(3,N21)
              GO TO 800
            ENDDO
            GAUGE(30,I)=ZERO
            GAUGE(31,I)=ZERO
            GAUGE(32,I)=ZERO
          ELSEIF( LGAUGE(3,I) < 0 )THEN
C Shell input -not available
          ELSEIF(LGAUGE(1,I) == 0 . AND. LGAUGE(3,I) == 0)THEN
C Point input (by coordinates) -not available
          ENDIF
  800  CONTINUE
       ENDDO

 1200  CONTINUE
      ENDIF
       IF(NSPMD > 1 .AND. FVSPMD(IFV)%RANK > -1)THEN
        CALL SPMD_RBCAST_SUBCOMM(FVSPMD(IFV)%AAA,3*NNT ,0,
     .       FVSPMD(IFV)%MPI_COMM)
        CALL SPMD_IBCAST_SUBCOMM(IVOLU(49),1,0,FVSPMD(IFV)%MPI_COMM)
        CALL SPMD_RBCAST_SUBCOMM(FVDATA(IFV)%PDISP_OLD,1,0,FVSPMD(IFV)%MPI_COMM)
        CALL SPMD_RBCAST_SUBCOMM(FVDATA(IFV)%PDISP,1,0,FVSPMD(IFV)%MPI_COMM)
      ENDIF 
      DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
         I1=FVSPMD(IFV)%IBUF_L(1,I)
         I2=FVSPMD(IFV)%IBUF_L(2,I)
         A(1,I2)=A(1,I2)+FVSPMD(IFV)%AAA(1,I1)
         A(2,I2)=A(2,I2)+FVSPMD(IFV)%AAA(2,I1)
         A(3,I2)=A(3,I2)+FVSPMD(IFV)%AAA(3,I1)
         IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT
     .     +ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
           FEXT(1,I2) = FEXT(1,I2)+FVSPMD(IFV)%AAA(1,I1)
           FEXT(2,I2) = FEXT(2,I2)+FVSPMD(IFV)%AAA(2,I1)
           FEXT(3,I2) = FEXT(3,I2)+FVSPMD(IFV)%AAA(3,I1)
         ENDIF
      ENDDO     

90    CONTINUE
C

C----------------------------
C POSITION FOR VISUALISATION
C----------------------------
      IF( FVSPMD(IFV)%NNA_L_GLOB > 0) THEN
      ! The BCAST is necessary if at least one proc. has NNA_L > 0
        IF(NSPMD > 1 .AND. FVSPMD(IFV)%RANK > -1 )THEN
     
          CALL SPMD_RBCAST_SUBCOMM(XXXA,3*NNA,0,
     .         FVSPMD(IFV)%MPI_COMM)
     
        ENDIF 
        IF (KMESH(IFV) < 2) THEN
           DO I=1,FVSPMD(IFV)%NNA_L
              I1=FVSPMD(IFV)%IBUFA_L(1,I)
              IF(NCONA(2,I1) /= 0) CYCLE
              I2=FVSPMD(IFV)%IBUFA_L(2,I)
              X(1,I2)=XXXA(1,I1)
              X(2,I2)=XXXA(2,I1)
              X(3,I2)=XXXA(3,I1)
           ENDDO
        ENDIF
      ENDIF

      IF(NBGAUGE > 0) THEN
        DEALLOCATE(FVSPMD(IFV)%GGG)
        DEALLOCATE(FVSPMD(IFV)%GGA)
      ENDIF
      DEALLOCATE(FVSPMD(IFV)%AAA)

      IF(ITYP /= 8 ) RETURN
C -------------------
C   SWITCH TO UP
C -------------------
      TTF  =RVOLU(49)
      NPOLH=IVOLU(49)
      IF (IVOLU(74) <= 0) THEN
         UP_SWITCH = TT-TTF >= RVOLU(70) .OR. NPOLH <= IVOLU(37)
         AUTO_SWITCH = .FALSE.
      ENDIF
      IF (IVOLU(74) > 0) THEN
C     Automatic switch to uniform pressure when dispersion of pressure is low
         AUTO_SWITCH = (FVDATA(IFV)%PDISP < FVDATA(IFV)%PDISP_OLD) .AND. 
     .        (FVDATA(IFV)%PDISP < RVOLU(73))
         UP_SWITCH = TT-TTF >= RVOLU(70)
         UP_SWITCH = UP_SWITCH .OR. AUTO_SWITCH
         FVDATA(IFV)%PDISP_OLD = FVDATA(IFV)%PDISP
      ENDIF
      
      IF (UP_SWITCH .AND. IVOLU(74)==2)THEN
        !Iswitch=2 : full merging request on Tswitch/Pswitch criteria
        IVOLU(74) = 0  ! Iswitch reset to 0
        IVOLU(60) = -1 ! Igmerg/Ivmin => VOLUMIN=EP20 in fvupd.F => merge erverything     
        RVOLU(70) = EP20 ! TSWITCH=infinity to prevent from any switch to Uniform Pressure
        RVOLU(73) = ZERO ! PSWITCH RATIO=0 to prevent from any switch to Uniform Pressure
        UP_SWITCH = .FALSE.
        IF(ISPMD+1 == FVSPMD(IFV)%PMAIN) THEN
               WRITE(IOUT,'(A,I10,A,E12.4/)') 
     .              ' ** MONITORED VOLUME ID: ',IVOLU(1),
     .              ' ALL POLYHEDRA ARE GOING TO BE MERGED ',TT
        ENDIF
      ENDIF
            
      IF (UP_SWITCH) THEN
         IF (IVOLU(74) == 0 .OR. IVOLU(74) == 1) THEN
            DO I=1,NVENT
               IF(IBAGHOL(1,I) == 2) IBAGHOL(1,I) = 0                     
               IDEF(I) = IBAGHOL(1,I) 
            ENDDO
            IF(NSPMD > 1 .AND. FVSPMD(IFV)%RANK > -1 )THEN
               CALL SPMD_RBCAST_SUBCOMM(RVOLU,NRVOLU,0,FVSPMD(IFV)%MPI_COMM)
               CALL SPMD_RBCAST_SUBCOMM(RBAGJET,NRBJET*NJET,0,FVSPMD(IFV)%MPI_COMM)
               CALL SPMD_IBCAST_SUBCOMM(IDEF,NVENT,0,FVSPMD(IFV)%MPI_COMM)
            ENDIF 
            IVOLU(2) = 7
            IVOLU(15)=-1
            IF(ISPMD+1 == FVSPMD(IFV)%PMAIN) THEN
               WRITE(IOUT,'(A,I10,A,E12.4/)') 
     .              ' ** MONITORED VOLUME ID: ',IVOLU(1),
     .              ' IS SWITCHED TO UNIFORM PRESSURE AT TIME',TT
               IF (AUTO_SWITCH) THEN
                  WRITE(IOUT, '(A,E12.4)')
     .                 '->AUTO SWITCH DUE TO LOW STANDARD DEVIATION OF PRESSURE AROUND ITS AVERAGE:', 
     .                 FVDATA(IFV)%PDISP
               ENDIF
            ENDIF
            DO I=1,NVENT
               IFVENT=IBAGHOL(10,I)
               IBAGHOL(1,I) = IDEF(I)
               IF(IFVENT == 2) IBAGHOL(10,I)=1
               IF(IFVENT == 3) IBAGHOL(10,I)=2
            ENDDO
         ELSE IF (IVOLU(74) == 2) THEN
!     Cmerg
            RVOLU(31) = EP30
!     Tswitch 
            RVOLU(70) = EP30
!     Iswitch
            IVOLU(74) = -2
            IF(NSPMD > 1 .AND. FVSPMD(IFV)%RANK > -1 )THEN
               CALL SPMD_RBCAST_SUBCOMM(RVOLU,NRVOLU,0,FVSPMD(IFV)%MPI_COMM)
               CALL SPMD_RBCAST_SUBCOMM(RBAGJET,NRBJET*NJET,0,FVSPMD(IFV)%MPI_COMM)
               CALL SPMD_IBCAST_SUBCOMM(IDEF,NVENT,0,FVSPMD(IFV)%MPI_COMM)
            ENDIF 
            
            IF(ISPMD+1 == FVSPMD(IFV)%PMAIN) THEN
               WRITE(IOUT,'(A,I10,A,E12.4/)') 
     .              ' ** MONITORED VOLUME ID: ',IVOLU(1),
     .              ' IS SWITCHED TO 1 FINITE VOLUME AT TIME',TT
               IF (AUTO_SWITCH) THEN
                  WRITE(IOUT, '(A,E12.4)')
     .                 '->AUTO SWITCH DUE TO LOW STANDARD DEVIATION OF PRESSURE AROUND ITS AVERAGE:', 
     .                 FVDATA(IFV)%PDISP
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END

