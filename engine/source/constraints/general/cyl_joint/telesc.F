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
Chd|  TELESC                        source/constraints/general/cyl_joint/telesc.F
Chd|-- called by -----------
Chd|        CJOINT                        source/constraints/general/cyl_joint/cjoint.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_ALLREDUCE_DB_COMM        source/mpi/generic/spmd_allreduce_db_comm.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        JOINT_MOD                     share/modules/joint_mod.F     
Chd|====================================================================
        SUBROUTINE TELESC(N_JOINT,A,AR,V,VR,X,FS,MS,IN,ITASK)
!$COMMENT
!       TELESC description
!       telesc computes the inertia/force/acc/... of the secondary node on each processor
!       then reduces the inertia/force/acc/... and update the force/acc of the main & secondary nodes
!
!       TELESC organization :
!        - mass / position computations for the secondary nodes
!           --> done by the processor with weight = 1
!        - reduction of mass / position (omp reduction + mpi reduction)
!        - inertia / acc / force computations for the secondary nodes
!           --> done by the processor with weight = 1
!        - reduction of inertia / acc / force (omp reduction + mpi reduction)
!        - all the processor (weight = 1 or 0) computes the acc / ... of the secondary nodes
!
!$ENDCOMMENT
        USE JOINT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: N_JOINT,ITASK
C     REAL
        my_real
     .      A(3,*), AR(3,*), V(3,*), VR(3,*), X(3,*), FS(*), MS(*),
     .      IN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr06_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NSN, NA, NB, I, N
C     REAL
        my_real :: N1,N2,N3,S,XX, YY, ZZ, RR,A0
        my_real :: MASSE, INER
        my_real :: AX,AY,AZ,AXX,AYY,AZZ
        my_real :: VX,VY,VZ,VXX,VYY,VZZ
        my_real :: XCDG, YCDG, ZCDG

        INTEGER :: NUMBER_NODE,NUMBER_NODE_WEIGHT
        INTEGER :: FIRST,LAST

        REAL(kind=8), DIMENSION(:), ALLOCATABLE :: BUF_S,BUF_R
C-----------------------------------------------
        NUMBER_NODE = CYL_JOIN(N_JOINT)%NUMBER_NODE
        NUMBER_NODE_WEIGHT = CYL_JOIN(N_JOINT)%NUMBER_NODE_WEIGHT
C----------------------------
C     DIRECTION LIBRE
C----------------------------
        !   --------------------
        !   initializaton
        NA=CYL_JOIN(N_JOINT)%MAIN_NODE(1) !NOD(1)
        NB=CYL_JOIN(N_JOINT)%MAIN_NODE(2) !NOD(2)

        N1=X(1,NB)-X(1,NA)
        N2=X(2,NB)-X(2,NA)
        N3=X(3,NB)-X(3,NA)
        S=SQRT(N1**2+N2**2+N3**2)

        N1=N1/S
        N2=N2/S
        N3=N3/S

        MASSE=ZERO
        INER=ZERO

        AX= ZERO
        AY= ZERO
        AZ= ZERO

        AXX= ZERO
        AYY= ZERO
        AZZ= ZERO

        VX= ZERO
        VY= ZERO
        VZ= ZERO

        VXX= ZERO
        VYY= ZERO
        VZZ= ZERO

        XCDG=ZERO
        YCDG=ZERO
        ZCDG=ZERO
        !   --------------------

        !   --------------------
        !   allocation
        IF(ITASK==0) THEN
            ALLOCATE( MASS(NUMBER_NODE_WEIGHT) )
            ALLOCATE( X_MS(NUMBER_NODE_WEIGHT),Y_MS(NUMBER_NODE_WEIGHT),Z_MS(NUMBER_NODE_WEIGHT) )

            ALLOCATE( INER_VEC(NUMBER_NODE_WEIGHT) )
            ALLOCATE(AX_MS(NUMBER_NODE_WEIGHT),AY_MS(NUMBER_NODE_WEIGHT),AZ_MS(NUMBER_NODE_WEIGHT))
            ALLOCATE(AXX_VEC(NUMBER_NODE_WEIGHT),AYY_VEC(NUMBER_NODE_WEIGHT),AZZ_VEC(NUMBER_NODE_WEIGHT))
            ALLOCATE(VX_MS(NUMBER_NODE_WEIGHT),VY_MS(NUMBER_NODE_WEIGHT),VZ_MS(NUMBER_NODE_WEIGHT))
            ALLOCATE(VXX_VEC(NUMBER_NODE_WEIGHT),VYY_VEC(NUMBER_NODE_WEIGHT),VZZ_VEC(NUMBER_NODE_WEIGHT))

            ALLOCATE( MASS_6(6,NTHREAD) )
            ALLOCATE( X_MS_6(6,NTHREAD),Y_MS_6(6,NTHREAD),Z_MS_6(6,NTHREAD) )
            ALLOCATE( INER_6(6,NTHREAD) )
            ALLOCATE( AX_MS_6(6,NTHREAD),AY_MS_6(6,NTHREAD),AZ_MS_6(6,NTHREAD) )
            ALLOCATE( AXX_6(6,NTHREAD),AYY_6(6,NTHREAD),AZZ_6(6,NTHREAD) )
            ALLOCATE( VX_MS_6(6,NTHREAD),VY_MS_6(6,NTHREAD),VZ_MS_6(6,NTHREAD) )
            ALLOCATE( VXX_6(6,NTHREAD),VYY_6(6,NTHREAD),VZZ_6(6,NTHREAD) )

            MASSE_GLOBAL=ZERO
            INER_GLOBAL=ZERO

            AX_GLOBAL= ZERO
            AY_GLOBAL= ZERO
            AZ_GLOBAL= ZERO
C
            AXX_GLOBAL= ZERO
            AYY_GLOBAL= ZERO
            AZZ_GLOBAL= ZERO
C
            VX_GLOBAL= ZERO
            VY_GLOBAL= ZERO
            VZ_GLOBAL= ZERO
C
            VXX_GLOBAL= ZERO
            VYY_GLOBAL= ZERO
            VZZ_GLOBAL= ZERO
C
            XCDG_GLOBAL=ZERO
            YCDG_GLOBAL=ZERO
            ZCDG_GLOBAL=ZERO
        ENDIF
        !   --------------------

        CALL MY_BARRIER

        MASS_6(1:6,ITASK+1) = ZERO
        X_MS_6(1:6,ITASK+1) = ZERO
        Y_MS_6(1:6,ITASK+1) = ZERO
        Z_MS_6(1:6,ITASK+1) = ZERO

        INER_6(1:6,ITASK+1) = ZERO

        AX_MS_6(1:6,ITASK+1) = ZERO
        AY_MS_6(1:6,ITASK+1) = ZERO
        AZ_MS_6(1:6,ITASK+1) = ZERO

        AXX_6(1:6,ITASK+1) = ZERO
        AYY_6(1:6,ITASK+1) = ZERO
        AZZ_6(1:6,ITASK+1) = ZERO

        VX_MS_6(1:6,ITASK+1) = ZERO
        VY_MS_6(1:6,ITASK+1) = ZERO
        VZ_MS_6(1:6,ITASK+1) = ZERO

        VXX_6(1:6,ITASK+1) = ZERO
        VYY_6(1:6,ITASK+1) = ZERO
        VZZ_6(1:6,ITASK+1) = ZERO

C----------------------------
C     CALCUL DU CDG + MASSE
C----------------------------
!       partial reduction of masse / XCDG / YCDG / ZCDG
!           --> done by the processor with weight = 1

        IF(NUMBER_NODE_WEIGHT>0) THEN
            !   --------------------
            !   loop over the secondary nodes with weight = 1
            FIRST = 1 + ITASK * NUMBER_NODE_WEIGHT / NTHREAD
            LAST = (ITASK+1) * NUMBER_NODE_WEIGHT / NTHREAD
            DO I=FIRST,LAST
                N = CYL_JOIN(N_JOINT)%NODE_WEIGHT(I) ! NOD(I)
                MASS(I) = MS(N)
                X_MS(I) = X(1,N)*MS(N)
                Y_MS(I) = X(2,N)*MS(N)
                Z_MS(I) = X(3,N)*MS(N)
            ENDDO
            !   --------------------
            !   parith/on ensures with sum_6_float
            CALL SUM_6_FLOAT(FIRST,LAST,MASS,MASS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,X_MS,X_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,Y_MS,Y_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,Z_MS,Z_MS_6(1,ITASK+1),1)
            !   --------------------
        ENDIF

        CALL MY_BARRIER

        !   --------------------
        !   mpi communication : reduction
        IF(ITASK==0) THEN
            ALLOCATE( BUF_S(13*6) )
            ALLOCATE( BUF_R(13*6) )
            BUF_S(1:6) = MASS_6(1:6,1)
            BUF_S(7:12) = X_MS_6(1:6,1)
            BUF_S(13:18) = Y_MS_6(1:6,1)
            BUF_S(19:24) = Z_MS_6(1:6,1)
            IF(NTHREAD>1) THEN
                DO I=2,NTHREAD
                    BUF_S(1:6) = BUF_S(1:6) + MASS_6(1:6,I)
                    BUF_S(7:12) = BUF_S(7:12) + X_MS_6(1:6,I)
                    BUF_S(13:18) = BUF_S(13:18) + Y_MS_6(1:6,I)
                    BUF_S(19:24) = BUF_S(19:24) + Z_MS_6(1:6,I)
                ENDDO
            ENDIF
            BUF_R(1:24) = ZERO
            IF(NSPMD>1) THEN
                CALL SPMD_ALLREDUCE_DB_COMM(BUF_S,BUF_R,4*6,"SUM",CYL_JOIN(N_JOINT)%COMM_MPI%COMM)
            ELSE
                BUF_R(1:24) = BUF_S(1:24)
            ENDIF

            DO I=1,6
                MASSE_GLOBAL = MASSE_GLOBAL + BUF_R(I)
                XCDG_GLOBAL = XCDG_GLOBAL + BUF_R(6+I)
                YCDG_GLOBAL = YCDG_GLOBAL + BUF_R(12+I)
                ZCDG_GLOBAL = ZCDG_GLOBAL + BUF_R(18+I)
            ENDDO
        ENDIF
        !   --------------------
        CALL MY_BARRIER
        !   --------------------
        !   load *_GOBAL global variables into a local one
        MASSE = MASSE_GLOBAL
        XCDG = XCDG_GLOBAL
        YCDG = YCDG_GLOBAL
        ZCDG = ZCDG_GLOBAL

        IF (MASSE>ZERO) THEN
            XCDG=XCDG/MASSE
            YCDG=YCDG/MASSE
            ZCDG=ZCDG/MASSE
        ENDIF

C----------------------------
C     CALCUL FORCES,MOMENTS,INERTIE(PTS ALIGNES SUR N)
C----------------------------

        !   --------------------
        !   partial reduction of INER / AX / AY / AZ / AXX / AYY / AZZ / VXX / VYY / VZZ
        !   VX / VY / VZ  --> done by 1 proc with weight = 1
        IF(NUMBER_NODE_WEIGHT>0) THEN
            !   --------------------
            !   loop over the secondary nodes with weight = 1
            FIRST = 1 + ITASK * NUMBER_NODE_WEIGHT / NTHREAD
            LAST = (ITASK+1) * NUMBER_NODE_WEIGHT / NTHREAD
            DO I=FIRST,LAST !NSN
                N = CYL_JOIN(N_JOINT)%NODE_WEIGHT(I) !NOD(I)

                XX=X(1,N)-XCDG
                YY=X(2,N)-YCDG
                ZZ=X(3,N)-ZCDG

                RR=N1*XX+N2*YY+N3*ZZ
                XX=N1*RR
                YY=N2*RR
                ZZ=N3*RR

                INER_VEC(I) = RR**2*MS(N)+IN(N)

                AX_MS(I) = A(1,N)*MS(N)
                AY_MS(I) = A(2,N)*MS(N)
                AZ_MS(I) = A(3,N)*MS(N)

                AXX_VEC(I) = AR(1,N)*IN(N)+YY*A(3,N)*MS(N)-ZZ*A(2,N)*MS(N)
                AYY_VEC(I) = AR(2,N)*IN(N)+ZZ*A(1,N)*MS(N)-XX*A(3,N)*MS(N)
                AZZ_VEC(I) = AR(3,N)*IN(N)+XX*A(2,N)*MS(N)-YY*A(1,N)*MS(N)

                VX_MS(I) = V(1,N)*MS(N)
                VY_MS(I) = V(2,N)*MS(N)
                VZ_MS(I) = V(3,N)*MS(N)
!
                VXX_VEC(I) = VR(1,N)*IN(N)+YY*V(3,N)*MS(N)-ZZ*V(2,N)*MS(N)
                VYY_VEC(I) = VR(2,N)*IN(N)+ZZ*V(1,N)*MS(N)-XX*V(3,N)*MS(N)
                VZZ_VEC(I) = VR(3,N)*IN(N)+XX*V(2,N)*MS(N)-YY*V(1,N)*MS(N)
            ENDDO
            !   --------------------
            !   parith/on ensures with sum_6_float
            CALL SUM_6_FLOAT(FIRST,LAST,INER_VEC,INER_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,AX_MS,AX_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,AY_MS,AY_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,AZ_MS,AZ_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,AXX_VEC,AXX_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,AYY_VEC,AYY_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,AZZ_VEC,AZZ_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,VX_MS,VX_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,VY_MS,VY_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,VZ_MS,VZ_MS_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,VXX_VEC,VXX_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,VYY_VEC,VYY_6(1,ITASK+1),1)
            CALL SUM_6_FLOAT(FIRST,LAST,VZZ_VEC,VZZ_6(1,ITASK+1),1)
            !   --------------------
        ENDIF
        !   --------------------
        CALL MY_BARRIER
        !   --------------------
        !   mpi communication : reduction
        IF(ITASK==0) THEN
            BUF_S(1:6) = INER_6(1:6,ITASK+1)
            BUF_S(7:12)  = AX_MS_6(1:6,ITASK+1)
            BUF_S(13:18) = AY_MS_6(1:6,ITASK+1)
            BUF_S(19:24) = AZ_MS_6(1:6,ITASK+1)

            BUF_S(25:30) = AXX_6(1:6,ITASK+1)
            BUF_S(31:36) = AYY_6(1:6,ITASK+1)
            BUF_S(37:42) = AZZ_6(1:6,ITASK+1)

            BUF_S(43:48) = VX_MS_6(1:6,ITASK+1)
            BUF_S(49:54) = VY_MS_6(1:6,ITASK+1)
            BUF_S(55:60) = VZ_MS_6(1:6,ITASK+1)

            BUF_S(61:66) = VXX_6(1:6,ITASK+1)
            BUF_S(67:72) = VYY_6(1:6,ITASK+1)
            BUF_S(73:78) = VZZ_6(1:6,ITASK+1)

            BUF_R(1:78) = ZERO

            IF(NTHREAD>1) THEN
                DO I=2,NTHREAD
                    BUF_S(1:6) = BUF_S(1:6) + INER_6(1:6,I)
                    BUF_S(7:12)  = BUF_S(7:12) + AX_MS_6(1:6,I)
                    BUF_S(13:18) = BUF_S(13:18) + AY_MS_6(1:6,I)
                    BUF_S(19:24) = BUF_S(19:24) + AZ_MS_6(1:6,I)

                    BUF_S(25:30) = BUF_S(25:30) + AXX_6(1:6,I)
                    BUF_S(31:36) = BUF_S(31:36) + AYY_6(1:6,I)
                    BUF_S(37:42) = BUF_S(37:42) + AZZ_6(1:6,I)

                    BUF_S(43:48) = BUF_S(43:48) + VX_MS_6(1:6,I)
                    BUF_S(49:54) = BUF_S(49:54) + VY_MS_6(1:6,I)
                    BUF_S(55:60) = BUF_S(55:60) + VZ_MS_6(1:6,I)

                    BUF_S(61:66) = BUF_S(61:66) + VXX_6(1:6,I)
                    BUF_S(67:72) = BUF_S(67:72) + VYY_6(1:6,I)
                    BUF_S(73:78) = BUF_S(73:78) + VZZ_6(1:6,I)
                ENDDO
            ENDIF

            IF(NSPMD>1) THEN
                CALL SPMD_ALLREDUCE_DB_COMM(BUF_S,BUF_R,13*6,"SUM",CYL_JOIN(N_JOINT)%COMM_MPI%COMM)
            ELSE
                BUF_R(1:78) = BUF_S(1:78)
            ENDIF

            DO I=1,6
                INER_GLOBAL = INER_GLOBAL + BUF_R(I)
                AX_GLOBAL = AX_GLOBAL + BUF_R(6+I)
                AY_GLOBAL = AY_GLOBAL + BUF_R(12+I)
                AZ_GLOBAL = AZ_GLOBAL + BUF_R(18+I)

                AXX_GLOBAL = AXX_GLOBAL + BUF_R(24+I)
                AYY_GLOBAL = AYY_GLOBAL + BUF_R(30+I)
                AZZ_GLOBAL = AZZ_GLOBAL + BUF_R(36+I)

                VX_GLOBAL = VX_GLOBAL + BUF_R(42+I)
                VY_GLOBAL = VY_GLOBAL + BUF_R(48+I)
                VZ_GLOBAL = VZ_GLOBAL + BUF_R(54+I)

                VXX_GLOBAL = VXX_GLOBAL + BUF_R(60+I)
                VYY_GLOBAL = VYY_GLOBAL + BUF_R(66+I)
                VZZ_GLOBAL = VZZ_GLOBAL + BUF_R(72+I)
            ENDDO
        ENDIF
        !   --------------------

        CALL MY_BARRIER

        !   load **_GLOBAL global variables into loacl ones
        INER = INER_GLOBAL
        AX = AX_GLOBAL
        AY = AY_GLOBAL
        AZ = AZ_GLOBAL

        AXX = AXX_GLOBAL
        AYY = AYY_GLOBAL
        AZZ = AZZ_GLOBAL

        VX = VX_GLOBAL
        VY = VY_GLOBAL
        VZ = VZ_GLOBAL

        VXX = VXX_GLOBAL
        VYY = VYY_GLOBAL
        VZZ = VZZ_GLOBAL

        A0=N1*AX+N2*AY+N3*AZ
        AX=AX-N1*A0
        AY=AY-N2*A0
        AZ=AZ-N3*A0
        A0=N1*AXX+N2*AYY+N3*AZZ
        AXX=AXX-N1*A0
        AYY=AYY-N2*A0
        AZZ=AZZ-N3*A0
C
        A0=N1*VX+N2*VY+N3*VZ
        VX=VX-N1*A0
        VY=VY-N2*A0
        VZ=VZ-N3*A0
        A0=N1*VXX+N2*VYY+N3*VZZ
        VXX=VXX-N1*A0
        VYY=VYY-N2*A0
        VZZ=VZZ-N3*A0
        !   --------------------
        !   main proc updates the FSAV array for /TH
        IF(CYL_JOIN(N_JOINT)%PROC_MAIN==ISPMD+1) THEN
#include "lockon.inc"
            FS(1)=FS(1)+AX*DT12
            FS(2)=FS(2)+AY*DT12
            FS(3)=FS(3)+AZ*DT12
            FS(4)=FS(4)+AXX*DT12
            FS(5)=FS(5)+AYY*DT12
            FS(6)=FS(6)+AZZ*DT12
#include "lockoff.inc"
        ENDIF
        !   --------------------
        IF (MASSE>ZERO) THEN
            AX=AX/MASSE
            AY=AY/MASSE
            AZ=AZ/MASSE
        ENDIF
        IF (INER>ZERO) THEN
            AXX=AXX/INER
            AYY=AYY/INER
            AZZ=AZZ/INER
        ENDIF
        IF (MASSE>ZERO) THEN
            VX=VX/MASSE
            VY=VY/MASSE
            VZ=VZ/MASSE
        ENDIF
        IF (INER>ZERO) THEN
            VXX=VXX/INER
            VYY=VYY/INER
            VZZ=VZZ/INER
        ENDIF
C----------------------------
C     CALCUL ACCELERATIONS
C----------------------------

        !   --------------------
        !   loop over the secondary nodes, done by every proc (weight= 1 or 0)
        FIRST = 1 + ITASK * NUMBER_NODE / NTHREAD
        LAST = (ITASK+1) * NUMBER_NODE / NTHREAD
        DO I=FIRST,LAST !NSN
            N = CYL_JOIN(N_JOINT)%NODE(I) !NOD(I)
C
            XX=X(1,N)-XCDG
            YY=X(2,N)-YCDG
            ZZ=X(3,N)-ZCDG
C
            RR=N1*XX+N2*YY+N3*ZZ
            XX=N1*RR
            YY=N2*RR
            ZZ=N3*RR
C
            A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
            A(1,N)=AX-YY*AZZ+ZZ*AYY+N1*A0
            A(2,N)=AY-ZZ*AXX+XX*AZZ+N2*A0
            A(3,N)=AZ-XX*AYY+YY*AXX+N3*A0
C
            A0=N1*AR(1,N)+N2*AR(2,N)+N3*AR(3,N)
            AR(1,N)=AXX+N1*A0
            AR(2,N)=AYY+N2*A0
            AR(3,N)=AZZ+N3*A0
C
            A0=N1*V(1,N)+N2*V(2,N)+N3*V(3,N)
            V(1,N)=VX-YY*VZZ+ZZ*VYY+N1*A0
            V(2,N)=VY-ZZ*VXX+XX*VZZ+N2*A0
            V(3,N)=VZ-XX*VYY+YY*VXX+N3*A0
C
            A0=N1*VR(1,N)+N2*VR(2,N)+N3*VR(3,N)
            VR(1,N)=VXX+N1*A0
            VR(2,N)=VYY+N2*A0
            VR(3,N)=VZZ+N3*A0
        ENDDO
        !   --------------------

        CALL MY_BARRIER

        !   --------------------
        !   deallocation
        IF(ITASK==0) THEN
            DEALLOCATE( MASS )
            DEALLOCATE( X_MS,Y_MS,Z_MS )

            DEALLOCATE( INER_VEC )
            DEALLOCATE(AX_MS,AY_MS,AZ_MS)
            DEALLOCATE(AXX_VEC,AYY_VEC,AZZ_VEC)
            DEALLOCATE(VX_MS,VY_MS,VZ_MS)
            DEALLOCATE(VXX_VEC,VYY_VEC,VZZ_VEC)

            DEALLOCATE( MASS_6 )
            DEALLOCATE( X_MS_6,Y_MS_6,Z_MS_6 )
            DEALLOCATE( INER_6 )
            DEALLOCATE( AX_MS_6,AY_MS_6,AZ_MS_6 )
            DEALLOCATE( AXX_6,AYY_6,AZZ_6 )
            DEALLOCATE( VX_MS_6,VY_MS_6,VZ_MS_6 )
            DEALLOCATE( VXX_6,VYY_6,VZZ_6 )

            DEALLOCATE( BUF_S,BUF_R)
        ENDIF
        !   --------------------

        CALL MY_BARRIER
C
        RETURN
        END
