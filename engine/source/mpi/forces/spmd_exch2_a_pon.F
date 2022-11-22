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
Chd|  SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|        SPMD_EXCHI_A_PON              source/mpi/forces/spmd_exchi_a_pon.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        FSKYI_MOD                     share/modules/restart_mod.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        HEAT_MOD                      share/modules/heat_mod.F      
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_EXCH2_A_PON(
     1   IAD_ELEM,FR_ELEM,ADDCNE,PROCNE,FR_NBCC,
     2   SIZE    ,LENR   ,LENS  ,FSKY  ,FSKYV  ,
     3   FSKYM   ,IFSUBM ,SIZI  ,LENI  ,IADSDP ,
     4   IADRCP  ,ISENDP ,IRECVP,FFSKY ,PROCNE_PXFEM,
     5   FR_NBCC1,IADSDP_PXFEM,IADRCP_PXFEM, ISENDP_PXFEM,
     6   IRECVP_PXFEM,LENR1  ,LENS1 ,IADSDP_CRK,IADRCP_CRK,
     7   ISENDP_CRK,IRECVP_CRK, FSKYD,CRKNODIAD,CRKSKY,
     8   FORNEQSKY,NFACNIT  ,LENC  ,FCONT   ,H3D_DATA,
     9   FNCONT   ,FTCONT   )
C--------------------------------------
       USE PLYXFEM_MOD
       USE FSKYI_MOD
       USE CRACKXFEM_MOD
       USE PLYXFEM_MOD
       USE HEAT_MOD
       USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "sphcom.inc"
#include      "scr18_c.inc"
#include      "scr14_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),FR_NBCC(2,*),
     .        ADDCNE(*), PROCNE(*),
     .        IADSDP(*), IADRCP(*), ISENDP(*), IRECVP(*),
     .        SIZE ,SIZI, LENI ,LENR ,LENS, IFSUBM, LENR1  ,LENS1,
     .        PROCNE_PXFEM(*), FR_NBCC1(2,*),IADSDP_PXFEM(*),
     .        IADRCP_PXFEM(*), ISENDP_PXFEM(*),IRECVP_PXFEM(*),
     .        IADSDP_CRK(*),IADRCP_CRK(*),ISENDP_CRK(*),
     .        IRECVP_CRK(*),CRKNODIAD(*),NFACNIT,
     .        LENC 
      my_real
     .        FSKY(8,LSKY),FSKYV(LSKY,8),FSKYM(*),
     .        FFSKY(3,LSKY),FSKYD(*),FORNEQSKY(3*NFACNIT,*)
      my_real  , INTENT(INOUT) :: FCONT(3,NUMNOD),FNCONT(3,NUMNOD),
     .        FTCONT(3,NUMNOD)
      TYPE(XFEM_SKY_), DIMENSION(*) :: CRKSKY
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,INDEX, NISKYF, N, IDEB,
     .        SIZ,J,K,L,NB_NOD,CC,NBIRECV, II, JJ, NN, IPT, MSGOFF,
     .        IAD_RECV(NSPMD+1),
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),IRINDEX(NSPMD),
     .        nbi,nbirct,nbisdt,l0,nbircp(NSPMD),nbisdp(NSPMD),
     .        LSKYI_OLD,SISKY_OLD

       INTEGER, DIMENSION(:), ALLOCATABLE :: ISKYFT,ISKYF,ITAGX,ADSKYI
       my_real, DIMENSION(:), ALLOCATABLE :: RBUF,SBUF, FSKYT,FTHESKYIF,
     . CONDNSKYIF
       my_real, DIMENSION(:,:), ALLOCATABLE :: FSKYIF
       my_real, DIMENSION(:,:), ALLOCATABLE :: FSKYIF_PXFEM

      DATA MSGOFF/169/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------



C === Allocate local arrays
      ALLOCATE(ISKYF(NISKY))
      ALLOCATE(ISKYFT(NISKY))
      ALLOCATE(ITAGX(NUMNOD))
      ALLOCATE(ADSKYI(0:NUMNOD+1))

      ALLOCATE(RBUF(SIZE*LENR + 4*NPLYMAX*LENR1 +
     .        (17*NLEVMAX+1)*LENR1+ NSPMD + 3*NFACNIT*LENR+LENC))

      ALLOCATE(FSKYIF(NFSKYI,NISKY))
      ALLOCATE(FSKYT(NISKY))

      IF(INTHEAT /=  0) THEN
        ALLOCATE(FTHESKYIF(NISKY))
        ALLOCATE(CONDNSKYIF(NISKY))
      ELSE
        ALLOCATE(FTHESKYIF(1))
        ALLOCATE(CONDNSKYIF(1))
      ENDIF

      IF( INTPLYXFEM > 0) THEN
        ALLOCATE(FSKYIF_PXFEM(5,NISKY))
      ELSE
        ALLOCATE(FSKYIF_PXFEM(5,1))
      ENDIF

C=======================================================================


!        NISKYF = 0
      LOC_PROC = ISPMD + 1
C
      NISKYF  = 0
      NBIRECV = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        NBISDP(I)=0
        NBIRCP(I) = 0
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
          SIZ = SIZE*FR_NBCC(2,I)+1
          IF(IPLYXFEM > 0) SIZ = SIZ + 4*NPLYMAX*FR_NBCC1(2,I)
          IF(ICRACK3D > 0) SIZ = SIZ + 
     .                      (17*NLEVMAX+1)*FR_NBCC1(2,I)
          SIZ = SIZ + 3*NFACNIT*FR_NBCC(2,I)               !NITSCHE METHOD
          IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) 
     .         SIZ = SIZ + 3*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))    !Output max contact forces
          IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) 
     .         SIZ = SIZ + 6*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))    !Output max contact pressure    
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      ENDDO
C
C Pre Traitement interfaces
C
      IF(NISKY/=0) THEN
        DO I = 1, NUMNOD
          ITAGX(I) = 0
        ENDDO
        DO I = 1, NISKY
          ITAGX(ISKY(I)) = -1
        ENDDO
        DO I = 1, NSPMD
          IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              IF(ITAGX(NOD)==-1)THEN
                ITAGX(NOD) = 1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C NISKYF : forces d'interfaces frontiere a echanger
        NISKYF = 0
C
        IF(INTPLYXFEM == 0) THEN
         IF(INTHEAT == 0 ) THEN
          IF(NFSKYI==4) THEN
           DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
             ENDIF
           ENDDO
          ELSE
           DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FSKYIF(5,NISKYF) = FSKYI(I,5)
             ENDIF
           ENDDO
          ENDIF
C + la t hermique
         ELSE
          IF(NODADT_THERM == 1) THEN
           IF(NFSKYI==4) THEN
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
               CONDNSKYIF(NISKYF) = CONDNSKYI(I)
             ENDIF
            ENDDO
           ELSE
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FSKYIF(5,NISKYF) = FSKYI(I,5)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
               CONDNSKYIF(NISKYF) = CONDNSKYI(I)
             ENDIF
            ENDDO
           ENDIF
          ELSE
           IF(NFSKYI==4) THEN
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
             ENDIF
            ENDDO
           ELSE
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FSKYIF(5,NISKYF) = FSKYI(I,5)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
             ENDIF
            ENDDO
           ENDIF
          ENDIF
         ENDIF ! INTHEAT        
C! + Plyxfem + type 24        
        ELSE
         IF(INTHEAT == 0 ) THEN
          IF(NFSKYI==4) THEN
           DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
C               
               FSKYIF_PXFEM(1,NISKYF) = PLYSKYI%FSKYI(I,1)
               FSKYIF_PXFEM(2,NISKYF) = PLYSKYI%FSKYI(I,2)
               FSKYIF_PXFEM(3,NISKYF) = PLYSKYI%FSKYI(I,3)
               FSKYIF_PXFEM(4,NISKYF) = PLYSKYI%FSKYI(I,4)
               FSKYIF_PXFEM(5,NISKYF) = PLYSKYI%FSKYI(I,5)
             ENDIF
           ENDDO
          ELSE
           DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FSKYIF(5,NISKYF) = FSKYI(I,5)
C               
               FSKYIF_PXFEM(1,NISKYF) = PLYSKYI%FSKYI(I,1)
               FSKYIF_PXFEM(2,NISKYF) = PLYSKYI%FSKYI(I,2)
               FSKYIF_PXFEM(3,NISKYF) = PLYSKYI%FSKYI(I,3)
               FSKYIF_PXFEM(4,NISKYF) = PLYSKYI%FSKYI(I,4)
               FSKYIF_PXFEM(5,NISKYF) = PLYSKYI%FSKYI(I,5)
             ENDIF
           ENDDO
          ENDIF
C + la t hermique
         ELSE
          IF(NODADT_THERM == 1) THEN
           IF(NFSKYI==4) THEN
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
               CONDNSKYIF(NISKYF) = CONDNSKYI(I)
C               
               FSKYIF_PXFEM(1,NISKYF) = PLYSKYI%FSKYI(I,1)
               FSKYIF_PXFEM(2,NISKYF) = PLYSKYI%FSKYI(I,2)
               FSKYIF_PXFEM(3,NISKYF) = PLYSKYI%FSKYI(I,3)
               FSKYIF_PXFEM(4,NISKYF) = PLYSKYI%FSKYI(I,4)
               FSKYIF_PXFEM(5,NISKYF) = PLYSKYI%FSKYI(I,5)
             ENDIF
            ENDDO
           ELSE
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FSKYIF(5,NISKYF) = FSKYI(I,5)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
               CONDNSKYIF(NISKYF) = CONDNSKYI(I)
C               
               FSKYIF_PXFEM(1,NISKYF) = PLYSKYI%FSKYI(I,1)
               FSKYIF_PXFEM(2,NISKYF) = PLYSKYI%FSKYI(I,2)
               FSKYIF_PXFEM(3,NISKYF) = PLYSKYI%FSKYI(I,3)
               FSKYIF_PXFEM(4,NISKYF) = PLYSKYI%FSKYI(I,4)
               FSKYIF_PXFEM(5,NISKYF) = PLYSKYI%FSKYI(I,5) 
             ENDIF
            ENDDO
           ENDIF
          ELSE
           IF(NFSKYI==4) THEN
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
C               
               FSKYIF_PXFEM(1,NISKYF) = PLYSKYI%FSKYI(I,1)
               FSKYIF_PXFEM(2,NISKYF) = PLYSKYI%FSKYI(I,2)
               FSKYIF_PXFEM(3,NISKYF) = PLYSKYI%FSKYI(I,3)
               FSKYIF_PXFEM(4,NISKYF) = PLYSKYI%FSKYI(I,4)
               FSKYIF_PXFEM(5,NISKYF) = PLYSKYI%FSKYI(I,5) 
             ENDIF
            ENDDO
           ELSE
            DO I = 1, NISKY
             IF(ITAGX(ISKY(I))==1)THEN
               NISKYF=NISKYF+1
               ISKYF(NISKYF) = ISKY(I)
               FSKYIF(1,NISKYF) = FSKYI(I,1)
               FSKYIF(2,NISKYF) = FSKYI(I,2)
               FSKYIF(3,NISKYF) = FSKYI(I,3)
               FSKYIF(4,NISKYF) = FSKYI(I,4)
               FSKYIF(5,NISKYF) = FSKYI(I,5)
               FTHESKYIF(NISKYF) = FTHESKYI(I)
C               
               FSKYIF_PXFEM(1,NISKYF) = PLYSKYI%FSKYI(I,1)
               FSKYIF_PXFEM(2,NISKYF) = PLYSKYI%FSKYI(I,2)
               FSKYIF_PXFEM(3,NISKYF) = PLYSKYI%FSKYI(I,3)
               FSKYIF_PXFEM(4,NISKYF) = PLYSKYI%FSKYI(I,4)
               FSKYIF_PXFEM(5,NISKYF) = PLYSKYI%FSKYI(I,5)
             ENDIF
            ENDDO
           ENDIF
          ENDIF
         ENDIF ! INTHEAT
       ENDIF 
C
C tri suivant no noeud
        DO N = 1, NUMNOD+1
          ADSKYI(N) = 0
        ENDDO
C
        DO I=1,NISKYF
          N = ISKYF(I)+1
          ADSKYI(N) = ADSKYI(N)+1
        ENDDO
C
        ADSKYI(0) = 1
        ADSKYI(1) = 1
        DO N = 1, NUMNOD
          NN = N+1
          ADSKYI(NN) = ADSKYI(NN) + ADSKYI(N)
        ENDDO
C
        DO I=1,NISKYF
          N = ISKYF(I)
          J  = ADSKYI(N)
          ISKYFT(J)=ISKYF(I)
          ISKYF(I)  = J
          ADSKYI(N) = ADSKYI(N) + 1
        ENDDO
C
        DO L = 1, NFSKYI
          DO I=1,NISKYF
            J = ISKYF(I)
            FSKYT(J) = FSKYIF(L,I)
          ENDDO
          DO I=1,NISKYF
            FSKYIF(L,I) = FSKYT(I)
          ENDDO
        ENDDO
        IF(INTPLYXFEM > 0) THEN
          DO L = 1, 5
            DO I=1,NISKYF
               J = ISKYF(I)
               FSKYT(J) = FSKYIF_PXFEM(L,I)
           ENDDO
           DO I=1,NISKYF
              FSKYIF_PXFEM(L,I) = FSKYT(I)
           ENDDO
         ENDDO
        ENDIF
C
        IF(INTHEAT > 0 ) THEN
         DO I=1,NISKYF
            J = ISKYF(I)
            FSKYT(J) = FTHESKYIF(I)
         ENDDO
         DO I=1,NISKYF
            FTHESKYIF(I) = FSKYT(I)
         ENDDO
         IF (NODADT_THERM == 1 )THEN
          DO I=1,NISKYF
            J = ISKYF(I)
            FSKYT(J) = CONDNSKYIF(I)
          ENDDO
          DO I=1,NISKYF
            CONDNSKYIF(I) = FSKYT(I)
          ENDDO
         ENDIF
        ENDIF
C
!        DO L = 1, NFSKYI
!          DO I=1,NISKYF
!            J = ISKYF(I)
!            FSKYT(J) = FSKYIF(L,I)
!          ENDDO
!          DO I=1,NISKYF
!            FSKYIF(L,I) = FSKYT(I)
!          ENDDO
!        ENDDO
!C
C
        DO I=1,NISKYF
          ISKYF(I) = ISKYFT(I)
        ENDDO
      ELSE
C tag a 0 de itagx sur noeud frontiere
        DO I = 1, NSPMD
          IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              ITAGX(NOD) = 0
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      DEALLOCATE(ISKYF)
      DEALLOCATE(FSKYT)
      DEALLOCATE(ISKYFT)
      ALLOCATE(SBUF(SIZE*LENS + 4*NPLYMAX*LENS1 +
     .        (17*NLEVMAX+1)*LENS1+ NSPMD+ 3*NFACNIT*LENS+LENC))


C
      NBISDT = 0
      L = 1

      DO I=1,NSPMD

       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
        L0 = L
C
C a optimiser
        IF(SOL2SPH_FLAG/=0)THEN
#include "vectorize.inc"
          DO J=IADSDP(I),IADSDP(I+1)-1
            CC = ISENDP(J)
            SBUF(L+SIZE-1)   = FSKYD(CC)
            L = L + SIZE
          END DO
          L = L0
        END IF

        IF(IALELAG == 0) THEN
         IF(ITHERM_FE == 0 .AND. INTHEAT == 0 ) THEN
          IF(IFSUBM==0)THEN
C prepar  ation envoi a proc I partie fixe (elem)
           IF(IVECTOR==1) THEN
Cel cas 3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 L = L + SIZE
               ENDDO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 L = L + SIZE
               END DO
             ENDIF
           ELSE
Cel ivec  tor = 0
Cel cas   3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 L = L + SIZE       
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 L = L + SIZE
               END DO
             ENDIF
           ENDIF
          ELSE
C
C code A  LE-CFD Avec Parit/ON MASSE
C
C
C prepar  ation envoi partie fixe (elem) a proc I
C
           IF(IVECTOR==1) THEN
Cel cas   3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 SBUF(L+8) = FSKYM(CC)
                 L = L + SIZE
                END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 SBUF(L+4) = FSKYM(CC)
                 L = L + SIZE
                END DO
             ENDIF
           ELSE
Cel ivec  tor = 0
Cel cas   3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L) = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ENDIF
           ENDIF
          ENDIF
C
C --- plus la thermque
C
        ELSE
          IF(IFSUBM==0)THEN
C prepar  ation envoi a proc I partie fixe (elem)
           IF(IVECTOR==1) THEN
Cel cas 3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 SBUF(L+8) = FTHESKY(CC)
                 L = L + SIZE
               ENDDO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 SBUF(L+4) = FTHESKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
           ELSE
            IF(NODADT_THERM == 1) THEN
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FTHESKY(CC)
                 SBUF(L+9) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FTHESKY(CC)
                 SBUF(L+5) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ELSE
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FTHESKY(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FTHESKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ENDIF
           ENDIF
          ELSE
C
C code A  LE-CFD Avec Parit/ON MASSE
C
C
C prepar  ation envoi partie fixe (elem) a proc I
C
           IF(IVECTOR==1) THEN
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 SBUF(L+8) = FSKYM(CC)
                 SBUF(L+9) = FTHESKY(CC)
                 L = L + SIZE
                END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 SBUF(L+4) = FSKYM(CC)
                 SBUF(L+5) = FTHESKY(CC)
                 L = L + SIZE
                END DO
             ENDIF
           ELSE
            IF(NODADT_THERM == 1) THEN
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FSKYM(CC)
                 SBUF(L+9) = FTHESKY(CC)
                 SBUF(L+10)= CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L) = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FSKYM(CC)
                 SBUF(L+5) = FTHESKY(CC)
                 SBUF(L+6) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ELSE
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FSKYM(CC)
                 SBUF(L+9) = FTHESKY(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L) = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FSKYM(CC)
                 SBUF(L+5) = FTHESKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ENDIF
           ENDIF
          ENDIF
        ENDIF
       ELSE
C
C + ale + lag
C
        IF(ITHERM_FE == 0 .AND. INTHEAT == 0 ) THEN
          IF(IFSUBM==0)THEN
C prepar  ation envoi a proc I partie fixe (elem)
           IF(IVECTOR==1) THEN
Cel cas 3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
C
                 SBUF(L+8)  = FFSKY(1,CC)
                 SBUF(L+9)  = FFSKY(2,CC)
                 SBUF(L+10) = FFSKY(3,CC)
                 SBUF(L+11) = FSKYM(CC)
                 L = L + SIZE
               ENDDO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
C
                 SBUF(L+4) = FFSKY(1,CC)
                 SBUF(L+5) = FFSKY(2,CC)
                 SBUF(L+6) = FFSKY(3,CC)
                 SBUF(L+7) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ENDIF
           ELSE
Cel ivec  tor = 0
Cel cas   3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
C
                 SBUF(L+8) =  FFSKY(1,CC)
                 SBUF(L+9) = FFSKY(2,CC)
                 SBUF(L+10) = FFSKY(3,CC)
                 SBUF(L+11) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
C
                 SBUF(L+4) = FFSKY(1,CC)
                 SBUF(L+5) = FFSKY(2,CC)
                 SBUF(L+6) = FFSKY(3,CC)
                 SBUF(L+7) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ENDIF
           ENDIF
          ELSE
C
C code A  LE-CFD Avec Parit/ON MASSE
C
C
C prepar  ation envoi partie fixe (elem) a proc I
C
           IF(IVECTOR==1) THEN
Cel cas   3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 SBUF(L+8) = FSKYM(CC)
C
                 SBUF(L+9) = FFSKY(1,CC)
                 SBUF(L+10) = FFSKY(2,CC)
                 SBUF(L+11) = FFSKY(3,CC)
cc                 SBUF(L+12) = FSKYM(CC)
                 L = L + SIZE
                END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 SBUF(L+4) = FSKYM(CC)
C
                 SBUF(L+5) = FFSKY(1,CC)
                 SBUF(L+6) = FFSKY(2,CC)
                 SBUF(L+7) = FFSKY(3,CC)
cc                 SBUF(L+8) = FSKYM(CC)
                 L = L + SIZE
                END DO
             ENDIF
           ELSE
Cel ivec  tor = 0
Cel cas   3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FSKYM(CC)
C
                 SBUF(L+9) = FFSKY(1,CC)
                 SBUF(L+10) = FFSKY(2,CC)
                 SBUF(L+11) = FFSKY(3,CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L) = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FSKYM(CC)
C
                 SBUF(L+5) = FFSKY(1,CC)
                 SBUF(L+6) = FFSKY(2,CC)
                 SBUF(L+7) = FFSKY(3,CC)
                 L = L + SIZE
               END DO
             ENDIF
           ENDIF
          ENDIF
C
C --- plus la thermque
C
        ELSE
          IF(IFSUBM==0)THEN
C prepar  ation envoi a proc I partie fixe (elem)
           IF(IVECTOR==1) THEN
Cel cas 3D seul possible
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 SBUF(L+8) = FTHESKY(CC)
C
                 SBUF(L+9) = FFSKY(1,CC)
                 SBUF(L+10) = FFSKY(2,CC)
                 SBUF(L+11) = FFSKY(3,CC)
                 SBUF(L+12) = FSKYM(CC)
                 L = L + SIZE
               ENDDO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 SBUF(L+4) = FTHESKY(CC)
C
                 SBUF(L+5) = FFSKY(1,CC)
                 SBUF(L+6) = FFSKY(2,CC)
                 SBUF(L+7) = FFSKY(3,CC)
                 SBUF(L+8) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ENDIF
           ELSE
            IF(NODADT_THERM == 1) THEN 
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FTHESKY(CC)
C
                 SBUF(L+9) = FFSKY(1,CC)
                 SBUF(L+10) = FFSKY(2,CC)
                 SBUF(L+11) = FFSKY(3,CC)
                 SBUF(L+12) = FSKYM(CC)
C
                 SBUF(L+13) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FTHESKY(CC)
C
                 SBUF(L+5) = FFSKY(1,CC)
                 SBUF(L+6) = FFSKY(2,CC)
                 SBUF(L+7) = FFSKY(3,CC)
                 SBUF(L+8) = FSKYM(CC)
C
                 SBUF(L+9) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ELSE
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FTHESKY(CC)
C
                 SBUF(L+9) = FFSKY(1,CC)
                 SBUF(L+10) = FFSKY(2,CC)
                 SBUF(L+11) = FFSKY(3,CC)
                 SBUF(L+12) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FTHESKY(CC)
C
                 SBUF(L+5) = FFSKY(1,CC)
                 SBUF(L+6) = FFSKY(2,CC)
                 SBUF(L+7) = FFSKY(3,CC)
                 SBUF(L+8) = FSKYM(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ENDIF
           ENDIF
          ELSE
C
C code A  LE-CFD Avec Parit/ON MASSE
C
C
C prepar  ation envoi partie fixe (elem) a proc I
C
           IF(IVECTOR==1) THEN
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,4)
                 SBUF(L+4) = FSKYV(CC,5)
                 SBUF(L+5) = FSKYV(CC,6)
                 SBUF(L+6) = FSKYV(CC,7)
                 SBUF(L+7) = FSKYV(CC,8)
                 SBUF(L+8) = FSKYM(CC)
                 SBUF(L+9) = FTHESKY(CC)
C
                 SBUF(L+10) = FFSKY(1,CC)
                 SBUF(L+11) = FFSKY(2,CC)
                 SBUF(L+12) = FFSKY(3,CC)
                 L = L + SIZE
                END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKYV(CC,1)
                 SBUF(L+1) = FSKYV(CC,2)
                 SBUF(L+2) = FSKYV(CC,3)
                 SBUF(L+3) = FSKYV(CC,7)
                 SBUF(L+4) = FSKYM(CC)
                 SBUF(L+5) = FTHESKY(CC)
C
                 SBUF(L+6) = FFSKY(1,CC)
                 SBUF(L+7) = FFSKY(2,CC)
                 SBUF(L+8) = FFSKY(3,CC)
                 L = L + SIZE
                END DO
             ENDIF
           ELSE
            IF(NODADT_THERM == 1) THEN
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FSKYM(CC)
                 SBUF(L+9) = FTHESKY(CC)
C
                 SBUF(L+10) = FFSKY(1,CC)
                 SBUF(L+11) = FFSKY(2,CC)
                 SBUF(L+12) = FFSKY(3,CC)
C
                 SBUF(L+13) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L) = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FSKYM(CC)
                 SBUF(L+5) = FTHESKY(CC)
C
                 SBUF(L+6) = FFSKY(1,CC)
                 SBUF(L+7) = FFSKY(2,CC)
                 SBUF(L+8) = FFSKY(3,CC)
C
                 SBUF(L+9) = CONDNSKY(CC)
                 L = L + SIZE
               END DO
             ENDIF
            ELSE
             IF(IRODDL/=0) THEN
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L)   = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(4,CC)
                 SBUF(L+4) = FSKY(5,CC)
                 SBUF(L+5) = FSKY(6,CC)
                 SBUF(L+6) = FSKY(7,CC)
                 SBUF(L+7) = FSKY(8,CC)
                 SBUF(L+8) = FSKYM(CC)
                 SBUF(L+9) = FTHESKY(CC)
C
                 SBUF(L+10) = FFSKY(1,CC)
                 SBUF(L+11) = FFSKY(2,CC)
                 SBUF(L+12) = FFSKY(3,CC)
                 L = L + SIZE
               END DO
             ELSE
#include "vectorize.inc"
               DO J=IADSDP(I),IADSDP(I+1)-1
                 CC = ISENDP(J)
                 SBUF(L) = FSKY(1,CC)
                 SBUF(L+1) = FSKY(2,CC)
                 SBUF(L+2) = FSKY(3,CC)
                 SBUF(L+3) = FSKY(7,CC)
                 SBUF(L+4) = FSKYM(CC)
                 SBUF(L+5) = FTHESKY(CC)
C
                 SBUF(L+6) = FFSKY(1,CC)
                 SBUF(L+7) = FFSKY(2,CC)
                 SBUF(L+8) = FFSKY(3,CC)
                 L = L + SIZE
               END DO
             ENDIF
            ENDIF
           ENDIF
          ENDIF
        ENDIF
C
       ENDIF ! ialelag
C
      IF(IPLYXFEM > 0) THEN
#include "vectorize.inc"
              DO J=IADSDP_PXFEM(I),IADSDP_PXFEM(I+1)-1
                 CC = ISENDP_PXFEM(J)
                 DO IPT = 1, NPLYMAX
                     SBUF(L)  =  PLYSKY(IPT)% FSKY(1,CC)
                     SBUF(L+1) = PLYSKY(IPT)% FSKY(2,CC)
                     SBUF(L+2) = PLYSKY(IPT)% FSKY(3,CC)
                     SBUF(L+3) = PLYSKY(IPT)% FSKY(4,CC)
                     L = L + 4
                 END DO
               ENDDO
      ENDIF
C
      IF(ICRACK3D > 0)THEN
#include "vectorize.inc"
        DO J=IADSDP_CRK(I),IADSDP_CRK(I+1)-1
          CC = ISENDP_CRK(J)
          DO IPT = 1, NLEVMAX
            SBUF(L)   = CRKSKY(IPT)% FSKY(1,CC)
            SBUF(L+1) = CRKSKY(IPT)% FSKY(2,CC)
            SBUF(L+2) = CRKSKY(IPT)% FSKY(3,CC)
            SBUF(L+3) = CRKSKY(IPT)% FSKY(4,CC)
            SBUF(L+4) = CRKSKY(IPT)% FSKY(5,CC)
            SBUF(L+5) = CRKSKY(IPT)% FSKY(6,CC)
            SBUF(L+6) = CRKLVSET(IPT)%ENR0(1,CC)
            SBUF(L+7) = CRKLVSET(IPT)%ENR0(2,CC)
c 
            SBUF(L+8)  = CRKAVX(IPT)%X(1,CC)
            SBUF(L+9)  = CRKAVX(IPT)%X(2,CC)
            SBUF(L+10) = CRKAVX(IPT)%X(3,CC)
            SBUF(L+11) = CRKAVX(IPT)%V(1,CC)
            SBUF(L+12) = CRKAVX(IPT)%V(2,CC)
            SBUF(L+13) = CRKAVX(IPT)%V(3,CC)
            SBUF(L+14) = CRKAVX(IPT)%VR(1,CC)
            SBUF(L+15) = CRKAVX(IPT)%VR(2,CC)
            SBUF(L+16) = CRKAVX(IPT)%VR(3,CC)
c
            L = L + 17
          END DO
          SBUF(L) = CRKNODIAD(CC)
          L = L + 1
        ENDDO
      ENDIF
C
      IF (NITSCHE > 0 ) THEN
#include "vectorize.inc"
         DO J=IADSDP(I),IADSDP(I+1)-1
           CC = ISENDP(J)
           DO K=1,NFACNIT
             SBUF(L+3*(K-1))   = FORNEQSKY(3*(K-1)+1,CC)
             SBUF(L+3*(K-1)+1) = FORNEQSKY(3*(K-1)+2,CC)
             SBUF(L+3*(K-1)+2) = FORNEQSKY(3*(K-1)+3,CC)
           ENDDO
           L = L + 3*NFACNIT
         END DO
      ENDIF

C --- /CONT/MAX output
      IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             SBUF(L  ) =  FCONT(1,NOD)
             SBUF(L+1) =  FCONT(2,NOD)
             SBUF(L+2) =  FCONT(3,NOD)
             L = L + 3
          END DO

      ENDIF

C --- /PCONT/MAX output
      IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             SBUF(L  ) =  FNCONT(1,NOD)
             SBUF(L+1) =  FNCONT(2,NOD)
             SBUF(L+2) =  FNCONT(3,NOD)
             SBUF(L+3) =  FTCONT(1,NOD)
             SBUF(L+4) =  FTCONT(2,NOD)
             SBUF(L+5) =  FTCONT(3,NOD)
             L = L + 6
          END DO
      ENDIF

C partie interface : calcul et envoi du nb de noeuds a envoyer
        nbi = 0
        IF(NISKYF/=0) THEN
#include "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            IF(ITAGX(NOD)==1)THEN
              nbi = nbi +  ADSKYI(NOD)-ADSKYI(NOD-1)
            END IF
          END DO
        END IF
        SBUF(L)=nbi
        L = L + 1
        nbisdp(i)=nbi
        nbisdt = nbisdt + nbi
C
C   echange messages
C
        IF(IPLYXFEM ==0 . AND. ICRACK3D == 0)THEN
           SIZ = (IADSDP(I+1)-IADSDP(I))*SIZE+1
        ELSE IF(IPLYXFEM > 0)THEN
           SIZ =  (IADSDP(I+1)-IADSDP(I))*SIZE +
     .            (IADSDP_PXFEM(I+1)-IADSDP_PXFEM(I))*4*NPLYMAX   + 1
        ELSE IF(ICRACK3D > 0)THEN
           SIZ =  (IADSDP(I+1)-IADSDP(I))*SIZE +
     .            (IADSDP_CRK(I+1)-IADSDP_CRK(I))*
     .                                    (17*NLEVMAX+1)   + 1
        ENDIF
        SIZ = SIZ + (IADSDP(I+1)-IADSDP(I))*3*NFACNIT    
        IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) 
     .         SIZ = SIZ + 3*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))    !Output max contact forces
        IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) 
     .         SIZ = SIZ + 6*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))    !Output max contact forces         
C
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(L0),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
      ENDDO
C
C decompactage
C
      NBIRCT = 0
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
C
        L = IAD_RECV(I)
C
C a optimiser
        IF(SOL2SPH_FLAG/=0)THEN
#include "vectorize.inc"
          DO J=IADRCP(I),IADRCP(I+1)-1
            CC = IRECVP(J)
            FSKYD(CC) = RBUF(L+SIZE-1)
            L = L + SIZE
          END DO
          L = IAD_RECV(I)
        END IF
C
        IF(IALELAG == 0) THEN
         IF(ITHERM_FE == 0 .AND. INTHEAT == 0  ) THEN
           IF(IFSUBM==0)THEN
             IF(IVECTOR==1) THEN
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,4) = RBUF(L+3)
                  FSKYV(CC,5) = RBUF(L+4)
                  FSKYV(CC,6) = RBUF(L+5)
                  FSKYV(CC,7) = RBUF(L+6)
                  FSKYV(CC,8) = RBUF(L+7)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,7) = RBUF(L+3)
                  L = L + SIZE
                 END DO
               ENDIF
             ELSE         ! ivector = 0
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(4,CC) = RBUF(L+3)
                  FSKY(5,CC) = RBUF(L+4)
                  FSKY(6,CC) = RBUF(L+5)
                  FSKY(7,CC) = RBUF(L+6)
                  FSKY(8,CC) = RBUF(L+7)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(7,CC) = RBUF(L+3)
                  L = L + SIZE
                 END DO
               ENDIF
             ENDIF
           ELSE
Cel46b5    N2D==0 .AND. IFSUBM==1
             IF(IVECTOR==1) THEN
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,4) = RBUF(L+3)
                  FSKYV(CC,5) = RBUF(L+4)
                  FSKYV(CC,6) = RBUF(L+5)
                  FSKYV(CC,7) = RBUF(L+6)
                  FSKYV(CC,8) = RBUF(L+7)
                  FSKYM(CC)   = RBUF(L+8)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,7) = RBUF(L+3)
                  FSKYM(CC)   = RBUF(L+4)
                  L = L + SIZE
                 END DO
               ENDIF
             ELSE          ! ivector = 0
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(4,CC) = RBUF(L+3)
                  FSKY(5,CC) = RBUF(L+4)
                  FSKY(6,CC) = RBUF(L+5)
                  FSKY(7,CC) = RBUF(L+6)
                  FSKY(8,CC) = RBUF(L+7)
                  FSKYM(CC)  = RBUF(L+8)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(7,CC) = RBUF(L+3)
                  FSKYM(CC)  = RBUF(L+4)
                  L = L + SIZE
                 END DO
               ENDIF
             ENDIF
           ENDIF
C
C--- plus la thermique
C
         ELSE
          IF(IFSUBM==0)THEN
           IF(IVECTOR==1) THEN
C cas 3D seul possible
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,4) = RBUF(L+3)
               FSKYV(CC,5) = RBUF(L+4)
               FSKYV(CC,6) = RBUF(L+5)
               FSKYV(CC,7) = RBUF(L+6)
               FSKYV(CC,8) = RBUF(L+7)
               FTHESKY(CC) = RBUF(L+8)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,7) = RBUF(L+3)
               FTHESKY(CC) = RBUF(L+4)
               L = L + SIZE
              END DO
            ENDIF
          ELSE         ! ivector = 0
C cas 3D seul possible
           IF(NODADT_THERM == 1) THEN 
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FTHESKY(CC) = RBUF(L+8)
               CONDNSKY(CC)= RBUF(L+9)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FTHESKY(CC) = RBUF(L+4)
               CONDNSKY(CC)= RBUF(L+5)
               L = L + SIZE
              END DO
            ENDIF
           ELSE
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FTHESKY(CC) = RBUF(L+8)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FTHESKY(CC) = RBUF(L+4)
               L = L + SIZE
              END DO
            ENDIF
           ENDIF
          ENDIF
        ELSE
          IF(IVECTOR==1) THEN
C cas 3D seul possible
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,4) = RBUF(L+3)
               FSKYV(CC,5) = RBUF(L+4)
               FSKYV(CC,6) = RBUF(L+5)
               FSKYV(CC,7) = RBUF(L+6)
               FSKYV(CC,8) = RBUF(L+7)
               FSKYM(CC)   = RBUF(L+8)
               FTHESKY(CC) = RBUF(L+9)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,7) = RBUF(L+3)
               FSKYM(CC)   = RBUF(L+4)
               FTHESKY(CC) = RBUF(L+5)
               L = L + SIZE
              END DO
            ENDIF
          ELSE          ! ivector = 0
C cas 3D seul possible
           IF(NODADT_THERM == 1) THEN
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FSKYM(CC)  = RBUF(L+8)
               FTHESKY(CC) = RBUF(L+9)
               CONDNSKY(CC)= RBUF(L+10)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FSKYM(CC)  = RBUF(L+4)
               FTHESKY(CC) = RBUF(L+5)
               CONDNSKY(CC)= RBUF(L+6)
               L = L + SIZE
              END DO
            ENDIF
           ELSE
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FSKYM(CC)  = RBUF(L+8)
               FTHESKY(CC) = RBUF(L+9)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FSKYM(CC)  = RBUF(L+4)
               FTHESKY(CC) = RBUF(L+5)
               L = L + SIZE
              END DO
            ENDIF
           ENDIF
          ENDIF
        ENDIF
       ENDIF
      ELSE
C
C ialelag > 0
C
      IF(ITHERM_FE == 0 .AND. INTHEAT == 0  ) THEN
           IF(IFSUBM==0)THEN
             IF(IVECTOR==1) THEN
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,4) = RBUF(L+3)
                  FSKYV(CC,5) = RBUF(L+4)
                  FSKYV(CC,6) = RBUF(L+5)
                  FSKYV(CC,7) = RBUF(L+6)
                  FSKYV(CC,8) = RBUF(L+7)
C
                  FFSKY(1,CC) = RBUF(L+8)
                  FFSKY(2,CC) = RBUF(L+9)
                  FFSKY(3,CC) = RBUF(L+10)
                  FSKYM(CC)   = RBUF(L+11)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,7) = RBUF(L+3)
C
                  FFSKY(1,CC) = RBUF(L+4)
                  FFSKY(2,CC) = RBUF(L+5)
                  FFSKY(3,CC) = RBUF(L+6)
                  FSKYM(CC)   = RBUF(L+7)
                  L = L + SIZE
                 END DO
               ENDIF
             ELSE         ! ivector = 0
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(4,CC) = RBUF(L+3)
                  FSKY(5,CC) = RBUF(L+4)
                  FSKY(6,CC) = RBUF(L+5)
                  FSKY(7,CC) = RBUF(L+6)
                  FSKY(8,CC) = RBUF(L+7)
C
                  FFSKY(1,CC) = RBUF(L+8)
                  FFSKY(2,CC) = RBUF(L+9)
                  FFSKY(3,CC) = RBUF(L+10)
                  FSKYM(CC)   = RBUF(L+11)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(7,CC) = RBUF(L+3)
C
                  FFSKY(1,CC) = RBUF(L+4)
                  FFSKY(2,CC) = RBUF(L+5)
                  FFSKY(3,CC) = RBUF(L+6)
                  FSKYM(CC)   = RBUF(L+7)
                  L = L + SIZE
                 END DO
               ENDIF
             ENDIF
           ELSE
Cel46b5    N2D==0 .AND. IFSUBM==1
             IF(IVECTOR==1) THEN
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,4) = RBUF(L+3)
                  FSKYV(CC,5) = RBUF(L+4)
                  FSKYV(CC,6) = RBUF(L+5)
                  FSKYV(CC,7) = RBUF(L+6)
                  FSKYV(CC,8) = RBUF(L+7)
                  FSKYM(CC)   = RBUF(L+8)
C
                  FFSKY(1,CC) = RBUF(L+9)
                  FFSKY(2,CC) = RBUF(L+10)
                  FFSKY(3,CC) = RBUF(L+11)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,7) = RBUF(L+3)
                  FSKYM(CC)   = RBUF(L+4)
C
                  FFSKY(1,CC) = RBUF(L+5)
                  FFSKY(2,CC) = RBUF(L+6)
                  FFSKY(3,CC) = RBUF(L+7)
                  L = L + SIZE
                 END DO
               ENDIF
             ELSE          ! ivector = 0
C cas 3D    seul possible
               IF(IRODDL/=0) THEN
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(4,CC) = RBUF(L+3)
                  FSKY(5,CC) = RBUF(L+4)
                  FSKY(6,CC) = RBUF(L+5)
                  FSKY(7,CC) = RBUF(L+6)
                  FSKY(8,CC) = RBUF(L+7)
                  FSKYM(CC)  = RBUF(L+8)
C
                  FFSKY(1,CC) = RBUF(L+9)
                  FFSKY(2,CC) = RBUF(L+10)
                  FFSKY(3,CC) = RBUF(L+11)
                  L = L + SIZE
                 END DO
               ELSE
#include    "vectorize.inc"
                 DO J=IADRCP(I),IADRCP(I+1)-1
                  CC = IRECVP(J)
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(7,CC) = RBUF(L+3)
                  FSKYM(CC)  = RBUF(L+4)
C
                  FFSKY(1,CC) = RBUF(L+5)
                  FFSKY(2,CC) = RBUF(L+6)
                  FFSKY(3,CC) = RBUF(L+7)
                  L = L + SIZE
                 END DO
               ENDIF
             ENDIF
           ENDIF
C
C--- plus la thermique
C
         ELSE
          IF(IFSUBM==0)THEN
           IF(IVECTOR==1) THEN
C cas 3D seul possible
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,4) = RBUF(L+3)
               FSKYV(CC,5) = RBUF(L+4)
               FSKYV(CC,6) = RBUF(L+5)
               FSKYV(CC,7) = RBUF(L+6)
               FSKYV(CC,8) = RBUF(L+7)
               FTHESKY(CC) = RBUF(L+8)
C
               FFSKY(1,CC) = RBUF(L+9)
               FFSKY(2,CC) = RBUF(L+10)
               FFSKY(3,CC) = RBUF(L+11)
               FSKYM(CC)   = RBUF(L+12)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,7) = RBUF(L+3)
               FTHESKY(CC) = RBUF(L+4)
C
               FFSKY(1,CC) = RBUF(L+5)
               FFSKY(2,CC) = RBUF(L+6)
               FFSKY(3,CC) = RBUF(L+7)
               FSKYM(CC)   = RBUF(L+8)
               L = L + SIZE
              END DO
            ENDIF
          ELSE         ! ivector = 0
C cas 3D seul possible
           IF(NODADT_THERM == 1) THEN
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FTHESKY(CC) = RBUF(L+8)
C
               FFSKY(1,CC) = RBUF(L+9)
               FFSKY(2,CC) = RBUF(L+10)
               FFSKY(3,CC) = RBUF(L+11)
               FSKYM(CC)   = RBUF(L+12)
C
               CONDNSKY(CC) = RBUF(L+13)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FTHESKY(CC) = RBUF(L+4)
C
               FFSKY(1,CC) = RBUF(L+5)
               FFSKY(2,CC) = RBUF(L+6)
               FFSKY(3,CC) = RBUF(L+7)
               FSKYM(CC)   = RBUF(L+8)
C
               CONDNSKY(CC) = RBUF(L+9)
               L = L + SIZE
              END DO
            ENDIF
           ELSE
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FTHESKY(CC) = RBUF(L+8)
C
               FFSKY(1,CC) = RBUF(L+9)
               FFSKY(2,CC) = RBUF(L+10)
               FFSKY(3,CC) = RBUF(L+11)
               FSKYM(CC)   = RBUF(L+12)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FTHESKY(CC) = RBUF(L+4)
C
               FFSKY(1,CC) = RBUF(L+5)
               FFSKY(2,CC) = RBUF(L+6)
               FFSKY(3,CC) = RBUF(L+7)
               FSKYM(CC)   = RBUF(L+8)
               L = L + SIZE
              END DO
            ENDIF
           ENDIF
          ENDIF
        ELSE
          IF(IVECTOR==1) THEN
C cas 3D seul possible
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,4) = RBUF(L+3)
               FSKYV(CC,5) = RBUF(L+4)
               FSKYV(CC,6) = RBUF(L+5)
               FSKYV(CC,7) = RBUF(L+6)
               FSKYV(CC,8) = RBUF(L+7)
               FSKYM(CC)   = RBUF(L+8)
               FTHESKY(CC) = RBUF(L+9)
C
               FFSKY(1,CC) = RBUF(L+10)
               FFSKY(2,CC) = RBUF(L+11)
               FFSKY(3,CC) = RBUF(L+12)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKYV(CC,1) = RBUF(L)
               FSKYV(CC,2) = RBUF(L+1)
               FSKYV(CC,3) = RBUF(L+2)
               FSKYV(CC,7) = RBUF(L+3)
               FSKYM(CC)   = RBUF(L+4)
               FTHESKY(CC) = RBUF(L+5)
C
               FFSKY(1,CC) = RBUF(L+6)
               FFSKY(2,CC) = RBUF(L+7)
               FFSKY(3,CC) = RBUF(L+8)
               FSKYM(CC)   = RBUF(L+9)
               L = L + SIZE
              END DO
            ENDIF
          ELSE          ! ivector = 0
C cas 3D seul possible
           IF(NODADT_THERM == 1) THEN
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FSKYM(CC)  = RBUF(L+8)
               FTHESKY(CC) = RBUF(L+9)
C
               FFSKY(1,CC) = RBUF(L+10)
               FFSKY(2,CC) = RBUF(L+11)
               FFSKY(3,CC) = RBUF(L+12)
C
               CONDNSKY(CC)= RBUF(L+13)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FSKYM(CC)  = RBUF(L+4)
               FTHESKY(CC) = RBUF(L+5)
C
               FFSKY(1,CC) = RBUF(L+6)
               FFSKY(2,CC) = RBUF(L+7)
               FFSKY(3,CC) = RBUF(L+8)
               FSKYM(CC)   = RBUF(L+9)
C
               CONDNSKY(CC)= RBUF(L+10)
               L = L + SIZE
              END DO
            ENDIF
           ELSE
            IF(IRODDL/=0) THEN
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(4,CC) = RBUF(L+3)
               FSKY(5,CC) = RBUF(L+4)
               FSKY(6,CC) = RBUF(L+5)
               FSKY(7,CC) = RBUF(L+6)
               FSKY(8,CC) = RBUF(L+7)
               FSKYM(CC)  = RBUF(L+8)
               FTHESKY(CC) = RBUF(L+9)
C
               FFSKY(1,CC) = RBUF(L+10)
               FFSKY(2,CC) = RBUF(L+11)
               FFSKY(3,CC) = RBUF(L+12)
               L = L + SIZE
              END DO
            ELSE
#include "vectorize.inc"
              DO J=IADRCP(I),IADRCP(I+1)-1
               CC = IRECVP(J)
               FSKY(1,CC) = RBUF(L)
               FSKY(2,CC) = RBUF(L+1)
               FSKY(3,CC) = RBUF(L+2)
               FSKY(7,CC) = RBUF(L+3)
               FSKYM(CC)  = RBUF(L+4)
               FTHESKY(CC) = RBUF(L+5)
C
               FFSKY(1,CC) = RBUF(L+6)
               FFSKY(2,CC) = RBUF(L+7)
               FFSKY(3,CC) = RBUF(L+8)
               FSKYM(CC)   = RBUF(L+9)
               L = L + SIZE
              END DO
            ENDIF
           ENDIF
          ENDIF
        ENDIF
       ENDIF
      ENDIF ! IALELAG
C
      IF(IPLYXFEM > 0) THEN
#include "vectorize.inc"
          DO J=IADRCP_PXFEM(I),IADRCP_PXFEM(I+1)-1
            CC = IRECVP_PXFEM(J)
            DO IPT = 1,NPLYMAX
              PLYSKY(IPT)% FSKY(1,CC) = RBUF(L)
              PLYSKY(IPT)% FSKY(2,CC) = RBUF(L+1)
              PLYSKY(IPT)% FSKY(3,CC) = RBUF(L+2)
              PLYSKY(IPT)% FSKY(4,CC) = RBUF(L+3)
              L = L + 4
            END DO
          END DO
      ENDIF
C
      IF(ICRACK3D > 0)THEN
#include "vectorize.inc"
        DO J=IADRCP_CRK(I),IADRCP_CRK(I+1)-1
          CC = IRECVP_CRK(J)
          DO IPT = 1,NLEVMAX
            CRKSKY(IPT)% FSKY(1,CC) = RBUF(L)
            CRKSKY(IPT)% FSKY(2,CC) = RBUF(L+1)
            CRKSKY(IPT)% FSKY(3,CC) = RBUF(L+2)
            CRKSKY(IPT)% FSKY(4,CC) = RBUF(L+3)
            CRKSKY(IPT)% FSKY(5,CC) = RBUF(L+4)
            CRKSKY(IPT)% FSKY(6,CC) = RBUF(L+5)
            CRKLVSET(IPT)%ENR0(1,CC) = RBUF(L+6)
            CRKLVSET(IPT)%ENR0(2,CC) = RBUF(L+7)

            CRKAVX(IPT)%X(1,CC)  = RBUF(L+8)                  
            CRKAVX(IPT)%X(2,CC)  = RBUF(L+9)                  
            CRKAVX(IPT)%X(3,CC)  = RBUF(L+10)                 
            CRKAVX(IPT)%V(1,CC)  = RBUF(L+11)                 
            CRKAVX(IPT)%V(2,CC)  = RBUF(L+12)                 
            CRKAVX(IPT)%V(3,CC)  = RBUF(L+13)                 
            CRKAVX(IPT)%VR(1,CC) = RBUF(L+14)                 
            CRKAVX(IPT)%VR(2,CC) = RBUF(L+15)                 
            CRKAVX(IPT)%VR(3,CC) = RBUF(L+16)             

            L = L + 17
          END DO
          CRKNODIAD(CC) = RBUF(L)
          L = L + 1
        END DO
      ENDIF
C
      IF (NITSCHE > 0 ) THEN
#include "vectorize.inc"
        DO J=IADRCP(I),IADRCP(I+1)-1
          CC = IRECVP(J)
          DO K=1,NFACNIT
            FORNEQSKY(3*(K-1)+1,CC) = RBUF(L+3*(K-1)) 
            FORNEQSKY(3*(K-1)+2,CC) = RBUF(L+3*(K-1)+1)
            FORNEQSKY(3*(K-1)+3,CC) = RBUF(L+3*(K-1)+2)
          ENDDO
          L = L + 3*NFACNIT
        END DO
      ENDIF

C --- /CONT/MAX output
      IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             FCONT(1,NOD) =  FCONT(1,NOD) + RBUF(L)  
             FCONT(2,NOD) =  FCONT(2,NOD) + RBUF(L+1)
             FCONT(3,NOD) =  FCONT(3,NOD) + RBUF(L+2) 
             L = L + 3
          END DO
       ENDIF

C --- /PCONT/MAX output
      IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             FNCONT(1,NOD) =  FNCONT(1,NOD) + RBUF(L)  
             FNCONT(2,NOD) =  FNCONT(2,NOD) + RBUF(L+1)
             FNCONT(3,NOD) =  FNCONT(3,NOD) + RBUF(L+2) 
             FTCONT(1,NOD) =  FTCONT(1,NOD) + RBUF(L+3)  
             FTCONT(2,NOD) =  FTCONT(2,NOD) + RBUF(L+4)
             FTCONT(3,NOD) =  FTCONT(3,NOD) + RBUF(L+5) 
             L = L + 6
          END DO
       ENDIF

C partie interface : recupere le nb de noeuds a recevoir
        NBI = NINT(RBUF(L))
C       L = L + 1
        NBIRCT = NBIRCT + NBI
        NBIRCP(I) = NBI
      END DO


C ----------------------------------------------------
C Check if ISKY & FSKYI are sufficiently allocate
C If not reallocate them
C ----------------------------------------------------
C NISKY : current counter stored stuff in ISKY & FSKYI
C SISKY - LSKYI : ISKY Size
C SFSKYI : FSKYI size (LSKYI*NFSKYI)

      IF ( NISKY+NBIRCT > SISKY) THEN
         CALL REALLOCATE_I_SKYLINE(NBIRCT,2)
      ENDIF
C
C Traitement interface si concerne le proc
C
      IF(NBIRCT>0.OR.NBISDT>0) THEN
        CALL SPMD_EXCHI_A_PON(
     1    IAD_ELEM,FR_ELEM,SIZI     ,ISKY  ,FSKYI ,
     2    FSKYIF  ,ITAGX  ,ADSKYI(0),NBIRCT,NBISDT,
     3    NBIRCP  ,NBISDP ,FTHESKYI, FTHESKYIF,CONDNSKYI,
     4    CONDNSKYIF,FSKYIF_PXFEM)
      END IF
C      
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO

      DEALLOCATE(RBUF)
      DEALLOCATE(SBUF)
      DEALLOCATE(FSKYIF)
      DEALLOCATE(FTHESKYIF)
      DEALLOCATE(CONDNSKYIF)
      DEALLOCATE(FSKYIF_PXFEM)
      DEALLOCATE(ITAGX)
      DEALLOCATE(ADSKYI)


C
#endif
      RETURN
      END
C

