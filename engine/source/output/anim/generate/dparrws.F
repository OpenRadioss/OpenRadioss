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
Chd|  DPARRWS                       source/output/anim/generate/dparrws.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GATHER_WA                source/mpi/anim/spmd_gather_wa.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE DPARRWS(NESBW,NSTRF,IXC   ,
     2                  IXTG ,X ,NODCUT,RWBUF,NPRW,
     3                  NODGLOB,BUF,IXS   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "spmd_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NESBW,NSTRF(*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        NODCUT,NPRW(*), IXS(NIXS,*),BUF,NODGLOB(*)
      my_real
     .  X(3,*),RWBUF(NRWLP,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, JJ, LEN, I, K, L, KK, K0, K1, K5, K9, N,
     .   N0, N1, N2, N3, N4, N10, NSEG, NSEGC, NSEGTG, ITYP,
     .   UNPACK(15,4), II(8), N5, N6, N7, N8, NSEGS, K3,OW,
     .   WA(6*BUF+4)
      my_real
     .   XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, D13, XXC, YYC, ZZC
      INTEGER POWER2(8),IPACK
      INTEGER :: INDICE
      INTEGER :: MODE,SIZE_BUFFER_S,SIZE_BUFFER00_R
      INTEGER, DIMENSION(NSPMD) :: SHIFT_R,NB_ELEM_R
      INTEGER, DIMENSION(NSECT,3,NSPMD) :: SHIFT_SECT
      INTEGER, DIMENSION(NSECT+1,3) ::SINDEX
      INTEGER, DIMENSION(NSECT+1,3,NSPMD) :: RINDEX_PROC
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFFER_S
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFFER00_R

      DATA POWER2/1,2,4,8,16,32,64,128/
C-----------------------------------------------
      DATA UNPACK/1,2,1,3,1,2,1,4,1,2,1,3,1,2,1,
     .            0,0,2,0,3,3,2,0,4,4,2,4,3,3,2,
     .            0,0,0,0,0,0,3,0,0,0,4,0,4,4,3,
     .            0,0,0,0,0,0,0,0,0,0,0,0,0,0,4/

!       1st step : count the number of value send by each proc 
!       2nd step : allocate the sended buffer for each proc and the received buffer (proc = 0)
!       3rd step : initialize and send the sended buffer to proc 0
!       4th step : write the data (proc=0)
!
!
!       Structure of the buffer :
!       *******************
!
!       sended buffer (local on each proc) :
!       sh = shell ; t = shell3n ; s = solid
!
!       
!       sh | t | s | sh | t | s | sh | t | s | sh | t | s | sh | t | s | sh | t | s | sh | t | s | sh | t | s | ...
!      \__________/ \__________/ \__________/ \__________/ \__________/ \__________/ \__________/ \__________/  ...
! nsect =    1            2            3            4    ...
!
!
!
!
!       received buffer (only on proc 0) :
!       
!       sh | t | s | sh | t | s |... sh | t | s | sh | t | s | sh | t | s |... sh | t | s | ... sh | t | s | sh | t | s | ...
!      \__________/ \__________/     \__________/ \__________/ \__________/   \__________/  ...\__________/ \__________/  ...
! nsect =    1            2      ...      n     ||     1            2      ...      n     |    ||     1            2      ...       
!     ||________________________________________||________________________________________| ...||_____________________________
!                          |                                           |                                      |
! proc =                   0                                           1                    ...               m

!  Index arrays : SHIFT_R, SHIFT_SECT, RINDEX_PROC
!       SHIFT_R --> index for the processor
!       SHIFT_SECT --> index for the section for each processor
!       RINDEX_PROC --> index for the element types (shell, shell3n or solid) for each processor and each section
!
!

!  SHIFT_R(1)                                 SHIFT_R(2)                                      SHIFT_R(m+1)     
!  
!     ||________________________________________||________________________________________| ...||_____________________________
!                          |                                           |                                      |
! proc =                   0                                           1                    ...               m

!  SHIFT_SECT :
!    (1,1)       (2,1)             (n,1)         (1,2) ...
!      \__________/ \__________/     \__________/ \__________/ \__________/   \__________/  ...\__________/ \__________/  ...
! nsect =    1            2      ...      n     ||     1            2      ...      n     |    ||     1            2      ...       
!
!   RINDEX_PROC
!            (1)       (2)       (3)
!       sh    |    t    |    s    |
!
!
!       -----------------------------------------------

!       ----------------------
!       count the number of data
!       ----------------------
        SINDEX(1:NSECT+1,1:3) = 0
        SIZE_BUFFER00_R = 0
        IF(ISPMD==0) RINDEX_PROC(1:NSECT,1:3,1:NSPMD) = 0
        JJ = 0

        IF (NSECT>0) THEN
                K0 = NSTRF(25)
                DO I=1,NSECT
                        N0 = NUMNOD + NODCUT + I - 1
                        K5=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1                     + 2*NSTRF(K0+7) +NSTRF(K0+8)*2
                        NSEGC = NSTRF(K0+9)

!                       ----------------
!                       SHELL
!                       ----------------
                        DO J=1,NSEGC
                                KK = K5+2*(J-1)
                                N  = NSTRF(KK)
                                IF(NSTRF(KK+1)/=0) THEN
                                        JJ = JJ + 4
                                ENDIF
                        ENDDO
                        SINDEX(I,1) = JJ
                        K9 = K5+2*NSTRF(K0+9) +2*NSTRF(K0+10)
     1                       +2*NSTRF(K0+11)+2*NSTRF(K0+12)
                        NSEGTG = NSTRF(K0+13)
!                       ----------------
!                       SHELL3N
!                       ----------------
                        DO J=1,NSEGTG
                                KK = K9+2*(J-1)
                                N  = NSTRF(KK)
                                IF(NSTRF(KK+1)/=0) THEN
                                        JJ = JJ + 4
                                ENDIF
                        ENDDO
                        SINDEX(I,2) = JJ

                        K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                        NSEGS=NSTRF(K0+7)

!                       ----------------
!                       SOLID
!                       ----------------
                        IF(NSEGS/=0)THEN
                                JJ = JJ + 4
                        END IF
                        DO J=1,NSEGS
                                KK=K3+2*(J-1)
                                IPACK=NSTRF(KK+1)
                                IF(IPACK/=0)THEN
                                        N =NSTRF(KK)
                                        IF (NSPMD == 1) THEN
                                                II(1)=IXS(2,N)-1
                                                II(2)=IXS(3,N)-1
                                                II(3)=IXS(4,N)-1
                                                II(4)=IXS(5,N)-1
                                                II(5)=IXS(6,N)-1
                                                II(6)=IXS(7,N)-1
                                                II(7)=IXS(8,N)-1
                                                II(8)=IXS(9,N)-1
                                        ELSE
                                                II(1)=NODGLOB(IXS(2,N))-1
                                                II(2)=NODGLOB(IXS(3,N))-1
                                                II(3)=NODGLOB(IXS(4,N))-1
                                                II(4)=NODGLOB(IXS(5,N))-1
                                                II(5)=NODGLOB(IXS(6,N))-1
                                                II(6)=NODGLOB(IXS(7,N))-1
                                                II(7)=NODGLOB(IXS(8,N))-1
                                                II(8)=NODGLOB(IXS(9,N))-1
                                        ENDIF

                                        IF(     II(2)==II(1).AND.II(4)==II(3)
     .                                          .AND.II(8)==II(5).AND.II(7)==II(6))THEN
C tetra4, tetra10
                                                N1=MOD(IPACK/POWER2(1),2)
                                                N2=MOD(IPACK/POWER2(3),2)
                                                N3=MOD(IPACK/POWER2(5),2)
                                                N4=MOD(IPACK/POWER2(6),2)
                                                IF(N1/=0.AND.N2/=0.AND.N3/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N1/=0.AND.N2/=0.AND.N4/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N2/=0.AND.N3/=0.AND.N4/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N3/=0.AND.N1/=0.AND.N4/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                        ELSE
C brick, shell16, brick20
                                                N1=MOD(IPACK/POWER2(1),2)
                                                N2=MOD(IPACK/POWER2(2),2)
                                                N3=MOD(IPACK/POWER2(3),2)
                                                N4=MOD(IPACK/POWER2(4),2)
                                                N5=MOD(IPACK/POWER2(5),2)
                                                N6=MOD(IPACK/POWER2(6),2)
                                                N7=MOD(IPACK/POWER2(7),2)
                                                N8=MOD(IPACK/POWER2(8),2)
                                                IF(N1/=0.AND.N2/=0.AND.N3/=0.AND.N4/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N5/=0.AND.N6/=0.AND.N7/=0.AND.N8/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N1/=0.AND.N5/=0.AND.N6/=0.AND.N2/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N4/=0.AND.N8/=0.AND.N7/=0.AND.N3/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N1/=0.AND.N4/=0.AND.N8/=0.AND.N5/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                                IF(N2/=0.AND.N3/=0.AND.N7/=0.AND.N6/=0)THEN
                                                        JJ=JJ+4
                                                END IF
                                        END IF
                                END IF
                        END DO
                        K0=NSTRF(K0+24)
                        SINDEX(I,3) = JJ
                ENDDO
        
                SIZE_BUFFER_S = JJ
                SINDEX(NSECT+1,1:3) = SIZE_BUFFER_S
!               -----------------------------------------------
!               ----------------------
!               Allocate the sended buffer
!               ----------------------
                ALLOCATE( BUFFER_S(SIZE_BUFFER_S) )
                MODE = 0

                ALLOCATE( BUFFER00_R(0) )

                IF(NSPMD>1) THEN
                        CALL SPMD_GATHER_WA(MODE,SIZE_BUFFER_S,SIZE_BUFFER00_R,SINDEX,RINDEX_PROC,
     1                                          BUFFER_S,BUFFER00_R,SHIFT_R,NB_ELEM_R)
                ELSE
                        SIZE_BUFFER00_R = SIZE_BUFFER_S
        !       shift for the _gatherv comm
                        SHIFT_R(1) = 0
                        RINDEX_PROC(1:NSECT,1:3,1) = SINDEX(1:NSECT,1:3)
                ENDIF    

                IF(ISPMD==0) THEN                
                        DEALLOCATE( BUFFER00_R )
                        ALLOCATE( BUFFER00_R(SIZE_BUFFER00_R) )
                ENDIF

!               ----------------------
!               initialize the sended buffer
!               ----------------------
                K0 = NSTRF(25)
                JJ = 0
                DO I=1,NSECT
                        N0 = NUMNOD + NODCUT + I - 1
                        K5=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1                     + 2*NSTRF(K0+7) +NSTRF(K0+8)*2
                        NSEGC = NSTRF(K0+9)

                        DO J=1,NSEGC
                                KK = K5+2*(J-1)
                                N  = NSTRF(KK)
                                IF(NSTRF(KK+1)/=0) THEN
                                        N1 = UNPACK(NSTRF(KK+1),1)
                                        N2 = UNPACK(NSTRF(KK+1),2)
                                        IF(N2==0)THEN
                                                N2 = N1
                                                N3 = N1
                                        ELSE
                                                N3 = UNPACK(NSTRF(KK+1),3)
                                                IF(N3==0)N3 = N2
                                        ENDIF
                                        IF (NSPMD == 1) THEN
                                                BUFFER_S(JJ+1) = N0
                                                BUFFER_S(JJ+2) = IXC(1+N1,N)-1
                                                BUFFER_S(JJ+3) = IXC(1+N2,N)-1
                                                BUFFER_S(JJ+4) = IXC(1+N3,N)-1
                                                JJ = JJ + 4
                                        ELSE
                                                BUFFER_S(JJ+1) = NUMNODG + NODCUT + I - 1
                                                BUFFER_S(JJ+2) = NODGLOB(IXC(1+N1,N))-1
                                                BUFFER_S(JJ+3) = NODGLOB(IXC(1+N2,N))-1
                                                BUFFER_S(JJ+4) = NODGLOB(IXC(1+N3,N))-1
                                                JJ = JJ + 4
                                        ENDIF
                                ENDIF
                        ENDDO

                        K9=K5+2*NSTRF(K0+9) +2*NSTRF(K0+10)
     1                     + 2*NSTRF(K0+11)+2*NSTRF(K0+12)
                        NSEGTG = NSTRF(K0+13)
                        DO J=1,NSEGTG
                                KK = K9+2*(J-1)
                                N  = NSTRF(KK)
                                IF(NSTRF(KK+1)/=0) THEN
                                        N1 = UNPACK(NSTRF(1+KK),1)
                                        N2 = UNPACK(NSTRF(1+KK),2)
                                        IF(N2==0)THEN
                                                N2 = N1
                                                N3 = N1
                                        ELSE
                                                N3 = UNPACK(NSTRF(1+KK),3)
                                                IF(N3==0)N3 = N2
                                        ENDIF
                                        IF (NSPMD == 1) THEN
                                                BUFFER_S(JJ+1) = N0
                                                BUFFER_S(JJ+2) = IXTG(1+N1,N)-1
                                                BUFFER_S(JJ+3) = IXTG(1+N2,N)-1
                                                BUFFER_S(JJ+4) = IXTG(1+N3,N)-1
                                                JJ = JJ + 4
                                        ELSE
                                                BUFFER_S(JJ+1) = NUMNODG + NODCUT + I - 1
                                                BUFFER_S(JJ+2) = NODGLOB(IXTG(1+N1,N))-1
                                                BUFFER_S(JJ+3) = NODGLOB(IXTG(1+N2,N))-1
                                                BUFFER_S(JJ+4) = NODGLOB(IXTG(1+N3,N))-1
                                                JJ = JJ + 4
                                        ENDIF
                                ENDIF
                        ENDDO

                        K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                        NSEGS=NSTRF(K0+7)

                        IF(NSEGS/=0)THEN
                                IF (NSPMD == 1) THEN
                                        BUFFER_S(JJ+1) = N0
                                        BUFFER_S(JJ+2) = N0
                                        BUFFER_S(JJ+3) = N0
                                        BUFFER_S(JJ+4) = N0
                                        JJ = JJ + 4
                                ELSE
                                        BUFFER_S(JJ+1) = NUMNODG + NODCUT + I - 1
                                        BUFFER_S(JJ+2) = NUMNODG + NODCUT + I - 1
                                        BUFFER_S(JJ+3) = NUMNODG + NODCUT + I - 1
                                        BUFFER_S(JJ+4) = NUMNODG + NODCUT + I - 1
                                        JJ = JJ + 4
                                ENDIF
                        END IF
C
                        DO J=1,NSEGS
                                KK=K3+2*(J-1)
                                IPACK=NSTRF(KK+1)
                                IF(IPACK/=0)THEN
                                        N =NSTRF(KK)
                                        IF (NSPMD == 1) THEN
                                                II(1)=IXS(2,N)-1
                                                II(2)=IXS(3,N)-1
                                                II(3)=IXS(4,N)-1
                                                II(4)=IXS(5,N)-1
                                                II(5)=IXS(6,N)-1
                                                II(6)=IXS(7,N)-1
                                                II(7)=IXS(8,N)-1
                                                II(8)=IXS(9,N)-1
                                        ELSE
                                                II(1)=NODGLOB(IXS(2,N))-1
                                                II(2)=NODGLOB(IXS(3,N))-1
                                                II(3)=NODGLOB(IXS(4,N))-1
                                                II(4)=NODGLOB(IXS(5,N))-1
                                                II(5)=NODGLOB(IXS(6,N))-1
                                                II(6)=NODGLOB(IXS(7,N))-1
                                                II(7)=NODGLOB(IXS(8,N))-1
                                                II(8)=NODGLOB(IXS(9,N))-1
                                        ENDIF

                                        IF( II(2)==II(1).AND.II(4)==II(3)
     .                                     .AND.II(8)==II(5).AND.II(7)==II(6))THEN
C tetra4, tetra10
                                                N1=MOD(IPACK/POWER2(1),2)
                                                N2=MOD(IPACK/POWER2(3),2)
                                                N3=MOD(IPACK/POWER2(5),2)
                                                N4=MOD(IPACK/POWER2(6),2)
                                                IF(N1/=0.AND.N2/=0.AND.N3/=0)THEN
                                                        BUFFER_S(JJ+1) =II(1)
                                                        BUFFER_S(JJ+2) =II(3)
                                                        BUFFER_S(JJ+3) =II(5)
                                                        BUFFER_S(JJ+4) =II(5)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N1/=0.AND.N2/=0.AND.N4/=0)THEN
                                                        BUFFER_S(JJ+1) =II(1)
                                                        BUFFER_S(JJ+2) =II(3)
                                                        BUFFER_S(JJ+3) =II(6)
                                                        BUFFER_S(JJ+4) =II(6)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N2/=0.AND.N3/=0.AND.N4/=0)THEN
                                                        BUFFER_S(JJ+1) =II(3)
                                                        BUFFER_S(JJ+2) =II(5)
                                                        BUFFER_S(JJ+3) =II(6)
                                                        BUFFER_S(JJ+4) =II(6)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N3/=0.AND.N1/=0.AND.N4/=0)THEN
                                                        BUFFER_S(JJ+1) =II(5)
                                                        BUFFER_S(JJ+2) =II(1)
                                                        BUFFER_S(JJ+3) =II(6)
                                                        BUFFER_S(JJ+4) =II(6)
                                                        JJ=JJ+4
                                                END IF
                                        ELSE
C brick, shell16, brick20
                                                N1=MOD(IPACK/POWER2(1),2)
                                                N2=MOD(IPACK/POWER2(2),2)
                                                N3=MOD(IPACK/POWER2(3),2)
                                                N4=MOD(IPACK/POWER2(4),2)
                                                N5=MOD(IPACK/POWER2(5),2)
                                                N6=MOD(IPACK/POWER2(6),2)
                                                N7=MOD(IPACK/POWER2(7),2)
                                                N8=MOD(IPACK/POWER2(8),2)

                                                IF(N1/=0.AND.N2/=0.AND.N3/=0.AND.N4/=0)THEN
                                                        BUFFER_S(JJ+1) =II(1)
                                                        BUFFER_S(JJ+2) =II(2)
                                                        BUFFER_S(JJ+3) =II(3)
                                                        BUFFER_S(JJ+4) =II(4)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N5/=0.AND.N6/=0.AND.N7/=0.AND.N8/=0)THEN
                                                        BUFFER_S(JJ+1) =II(5)
                                                        BUFFER_S(JJ+2) =II(6)
                                                        BUFFER_S(JJ+3) =II(7)
                                                        BUFFER_S(JJ+4) =II(8)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N1/=0.AND.N5/=0.AND.N6/=0.AND.N2/=0)THEN
                                                        BUFFER_S(JJ+1) =II(1)
                                                        BUFFER_S(JJ+2) =II(5)
                                                        BUFFER_S(JJ+3) =II(6)
                                                        BUFFER_S(JJ+4) =II(2)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N4/=0.AND.N8/=0.AND.N7/=0.AND.N3/=0)THEN
                                                        BUFFER_S(JJ+1) =II(4)
                                                        BUFFER_S(JJ+2) =II(8)
                                                        BUFFER_S(JJ+3) =II(7)
                                                        BUFFER_S(JJ+4) =II(3)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N1/=0.AND.N4/=0.AND.N8/=0.AND.N5/=0)THEN
                                                        BUFFER_S(JJ+1) =II(1)
                                                        BUFFER_S(JJ+2) =II(4)
                                                        BUFFER_S(JJ+3) =II(8)
                                                        BUFFER_S(JJ+4) =II(5)
                                                        JJ=JJ+4
                                                END IF
                                                IF(N2/=0.AND.N3/=0.AND.N7/=0.AND.N6/=0)THEN
                                                        BUFFER_S(JJ+1) =II(2)
                                                        BUFFER_S(JJ+2) =II(3)
                                                        BUFFER_S(JJ+3) =II(7)
                                                        BUFFER_S(JJ+4) =II(6)
                                                        JJ=JJ+4
                                                END IF
                                        END IF
                                END IF
                        END DO
                K0=NSTRF(K0+24)
                ENDDO

!               ----------------------
!               send buffer
!               ----------------------

                MODE = 1
                IF(NSPMD>1) THEN
                        CALL SPMD_GATHER_WA(MODE,SIZE_BUFFER_S,SIZE_BUFFER00_R,SINDEX,RINDEX_PROC,
     1                                           BUFFER_S,BUFFER00_R,SHIFT_R,NB_ELEM_R)
                ELSE
                        BUFFER00_R(1:SIZE_BUFFER00_R) = BUFFER_S(1:SIZE_BUFFER_S)
                ENDIF
                DEALLOCATE( BUFFER_S )
        ENDIF
        
!               ----------------------
!               write the received buffer
!               ----------------------



!       received buffer (only on proc 0) :
!       
!       sh | t | s | sh | t | s |... sh | t | s | sh | t | s | sh | t | s |... sh | t | s | ... sh | t | s | sh | t | s | ...
!      \__________/ \__________/     \__________/ \__________/ \__________/   \__________/  ...\__________/ \__________/  ...
! nsect =    1            2      ...      n     ||     1            2      ...      n     |    ||     1            2      ...       
!     ||________________________________________||________________________________________| ...||_____________________________
!                          |                                           |                                      |
! proc =                   0                                           1                    ...               m


        IF (ISPMD==0.AND.NSECT>0) THEN
                DO I=1,NSPMD
                        SHIFT_SECT(1,1,I) = 0
                        SHIFT_SECT(1,2,I) = RINDEX_PROC(1,1,I)
                        SHIFT_SECT(1,3,I) = RINDEX_PROC(1,2,I)
                        DO JJ=2,NSECT
                                SHIFT_SECT(JJ,1,I) = RINDEX_PROC(JJ-1,3,I)
                                SHIFT_SECT(JJ,2,I) = RINDEX_PROC(JJ,1,I)
                                SHIFT_SECT(JJ,3,I) = RINDEX_PROC(JJ,2,I)
                        ENDDO
                ENDDO
        
                DO JJ=1,NSECT
        !       ----------------
        !       SHELL
        !       ----------------
                        DO I=1,NSPMD
                                LEN = RINDEX_PROC(JJ,1,I) - SHIFT_SECT(JJ,1,I)
                                IF(LEN>0) THEN
                                        INDICE = 1 + SHIFT_R(I) + SHIFT_SECT(JJ,1,I)
                                        CALL WRITE_I_C(BUFFER00_R(INDICE),LEN)
                                ENDIF
                        ENDDO
        !       ----------------
        !       SHELL3N
        !       ----------------
                        DO I=1,NSPMD
                                LEN = RINDEX_PROC(JJ,2,I) - RINDEX_PROC(JJ,1,I)
                                IF(LEN>0) THEN
                                        INDICE = 1 + SHIFT_R(I) + SHIFT_SECT(JJ,2,I)
                                        CALL WRITE_I_C(BUFFER00_R(INDICE),LEN)
                                ENDIF
                        ENDDO
        !       ----------------
        !       SOLID
        !       ----------------
                        DO I=1,NSPMD
                                LEN = RINDEX_PROC(JJ,3,I) - RINDEX_PROC(JJ,2,I)
                                IF(LEN>0) THEN
                                        INDICE = 1 + SHIFT_R(I) + SHIFT_SECT(JJ,3,I)
                                        CALL WRITE_I_C(BUFFER00_R(INDICE),LEN)
                                ENDIF
                        ENDDO
                ENDDO
        ENDIF

        

        IF (ISPMD==0) THEN
                N0 = NUMNODG + NODCUT + NSECT
                N1 = NUMNODG + NODCUT + NSECT + NRWALL

                DO N=1,NRWALL
                        II(1) = N0
                        II(2) = N0
                        II(3) = N0
                        II(4) = N0
                        CALL WRITE_I_C(II,4)
                        N0 = N0 + 1
                        K=1
                        N2=N +NRWALL
                        N3=N2+NRWALL
                        N4=N3+NRWALL
                        ITYP= NPRW(N4)

                        IF(IABS(ITYP)==1.OR.ITYP==4)THEN
                                II(1) = N1
                                II(2) = N1 + 3
                                II(3) = N1 + 2
                                II(4) = N1 + 1
                                CALL WRITE_I_C(II,4)
                                N1 = N1 + 4
                        ELSEIF(ITYP==2)THEN
                                N10 = N1
                                DO J = 1,23
                                        II(1) = N1
                                        II(2) = N1 + 2
                                        II(3) = N1 + 3
                                        II(4) = N1 + 1
                                        CALL WRITE_I_C(II,4)
                                        N1 = N1 + 2
                                ENDDO
                                II(1) = N1
                                II(2) = N10
                                II(3) = N10 + 1
                                II(4) = N1 + 1
                                CALL WRITE_I_C(II,4)
                                N1 = N1 + 2
                        ELSEIF(ITYP==3)THEN
                                DO I = 1,6
                                        DO J = 1,6
                                                DO L = 1,6
                                                        II(1) = N1
                                                        II(2) = N1 + 1
                                                        II(3) = N1 + 8
                                                        II(4) = N1 + 7
                                                        CALL WRITE_I_C(II,4)
                                                        N1 = N1 + 1
                                                ENDDO
                                                N1 = N1 + 1
                                        ENDDO
                                        N1 = N1 + 7
                                ENDDO
                        ENDIF
                        K=K+NPRW(N)
                        IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
                ENDDO
        ENDIF

        IF(ALLOCATED(BUFFER00_R)) DEALLOCATE( BUFFER00_R )

        RETURN
        END SUBROUTINE DPARRWS
