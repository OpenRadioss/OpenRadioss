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
C
Chd|====================================================================
Chd|  SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CONVERSION11                  source/mpi/interfaces/spmd_i7tool.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI22VOX(
     1   IRECTM  ,NRTM   ,X      ,V       ,BMINMAL  ,
     2   STIFE   ,NIN    ,ISENDTO,IRCVFROM,IAD_ELEM ,
     3   FR_ELEM ,NSHELR ,ITAB   ,ITASK          )  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE I22TRI_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, INACTI, IGAP, NRTM,
     .        IRECTM(4,NRTM), NSHELR,
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*) , ITASK

      my_real
     .        X(3,*), V(3,*), BMINMAL(6), 
     .        STIFE(NRTM), TZINF
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I, LOC_PROC,P,IDEB,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4,
     .        J, L, LEN, NB_, NRTMR, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NRTM),NBOX(NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ, N1, N2, N3, N4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2, NOD
      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
      TYPE(r8_pointer), DIMENSION(NSPMD) :: BUF
      my_real ::
     .        DX, DY, DZ,
     .        XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
      LOGICAL ::
     .        TEST
      DATA MSGOFF/138/
      DATA MSGOFF2/139/
      DATA MSGOFF3/140/
      DATA MSGOFF4/141/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     Generation of candidates list from lagrangian shells
C     by testing Voxel marking
C=======================================================================
      LOC_PROC = ISPMD + 1
      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL
      !-------------------------------------------!
      !    Domain Bounds from i22xsave            !
      !-------------------------------------------!
      IF(IRCVFROM(NIN,LOC_PROC)==0.AND.
     .   ISENDTO(NIN,LOC_PROC)==0) RETURN
      IF (IMONM > 0) CALL STARTIME(25,1)
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
      !-------------------------------------------!
      !     Voxel Sending                         !
      !     + Min-Max Boxes Sending               !
      !-------------------------------------------!
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          CRVOXEL(0,0,LOC_PROC),
     .          (LRVOXEL+1)*(LRVOXEL+1),
     .          MPI_INTEGER,
     .          IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_SC(P),IERROR)
              MSGTYP = MSGOFF2 
              CALL MPI_ISEND(
     .          BMINMA(1,LOC_PROC),6        ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      !-------------------------------------------!
      !     Voxel Reception                       !
      !     + Min-Max Boxes Reception             !
      !-------------------------------------------!
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        NBIRECV=0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              NBIRECV=NBIRECV+1
              IRINDEXI(NBIRECV)=P
              MSGTYP = MSGOFF + NSPMD*ISPMD + P +NIN
              CALL MPI_IRECV(
     .          CRVOXEL(0,0,P),
     .         (LRVOXEL+1)*(LRVOXEL+1),
     .          MPI_INTEGER,
     .          IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_RC(NBIRECV),IERROR)
              MSGTYP = MSGOFF2
              CALL MPI_IRECV(
     .          BMINMA(1,P)   ,6              ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RB(NBIRECV),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      !-------------------------------------------!
      !     XREM sending                          !
      !     (remote lagrangian shells)            !
      !-------------------------------------------!
      IDEB = 1   
      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
          CALL MPI_WAIT(REQ_RC(INDEXI),STATUS,IERROR)
          L = IDEB
          NBOX(P) = 0
          NB_ = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)
          DX = XMAXB-XMINB
          DY = YMAXB-YMINB
          DZ = ZMAXB-ZMINB
         !-------------------------------------------!
         !     Voxel Testing and                     !
         !     Remote Shell List Generation          !
         !-------------------------------------------!
          DO I=1,NRTM
           IF(STIFE(I)==ZERO) CYCLE
           IX1=INT(NBX*(XMINE(I)-XMINB)/DX)
           IX2=INT(NBX*(XMAXE(I)-XMINB)/DX)
           IX1=MAX(0,IX1)
           IX2=MIN(IX2,NBX)
           IF(IX2 < 0.OR.IX1 > NBX) CYCLE      
           IY1=INT(NBY*(YMINE(I)-YMINB)/DY)
           IY2=INT(NBY*(YMAXE(I)-YMINB)/DY) 
           IY1=MAX(0,IY1)
           IY2=MIN(IY2,NBY)
           IF(IY2 < 0.OR.IY1 > NBY) CYCLE  
           IZ1=INT(NBZ*(ZMINE(I)-ZMINB)/DZ)
           IZ2=INT(NBZ*(ZMAXE(I)-ZMINB)/DZ)
           IZ1=MAX(0,IZ1)
           IZ2=MIN(IZ2,NBZ)
           IF(IZ2 < 0.OR.IZ1 > NBZ) CYCLE  
           DO IY=IY1,IY2
             DO IZ=IZ1,IZ2
                DO IX=IX1,IX2
                  TEST = BTEST(CRVOXEL(IY,IZ,P),IX)
                  IF(TEST) THEN
                   NB_ = NB_ + 1
                   INDEX(NB_) = I
                   GOTO 111 !next I
                  END IF
                END DO !IX
              END DO !IZ
            END DO !IY
  111     CONTINUE       
          ENDDO !I=1,NRTM 
          NBOX(P) = NB_
          !NSHELR = NB_
         !-------------------------------------------!
         !     Message Length for Sending            !
         !-------------------------------------------!
          MSGTYP = MSGOFF3 
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
         !-------------------------------------------!
         !     Buffer Allocation                     !
         !-------------------------------------------!
          IF (NB_>0) THEN
            ALLOCATE(BUF(P)%P(SIZ_XREM*NB_),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
         !-------------------------------------------!
         !     Buffer Affectation                    !
         !-------------------------------------------!
            DO J = 1, NB_
              I = INDEX(J) 
              BUF(P)%p(L+1:L+4)  = ITAB(IRECTM(1:4,I))
              BUF(P)%p(L+5:L+8)  = X(1,IRECTM(1:4,I))
              BUF(P)%p(L+9:L+12) = X(2,IRECTM(1:4,I))
              BUF(P)%p(L+13:L+16)= X(3,IRECTM(1:4,I))            
              BUF(P)%p(L+17:L+19)= (/XMINE(I),YMINE(I),ZMINE(I)/)
              BUF(P)%p(L+20:L+22)= (/XMAXE(I),YMAXE(I),ZMAXE(I)/)
              BUF(P)%p(L+23)       = STIFE(I)
              BUF(P)%p(L+24)       = SUM(V(1,IRECTM(1:4,I)))/FOUR
              BUF(P)%p(L+25)       = SUM(V(2,IRECTM(1:4,I)))/FOUR
              BUF(P)%p(L+26)       = SUM(V(3,IRECTM(1:4,I)))/FOUR
              L = L + SIZ_XREM ! attention SIZ_XREM a mettre a jour dans tri22_mod si modif
            END DO
            MSGTYP = MSGOFF4
            CALL MPI_ISEND(
     1        BUF(P)%P(1),L,MPI_DOUBLE_PRECISION,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
          ENDIF
        ENDDO
      ENDIF
      !-------------------------------------------!
      !     XREM data reception                   !
      !-------------------------------------------!
      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        NSHELR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(LOC_PROC/=P) THEN
              MSGTYP = MSGOFF3 
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P)>0) THEN
                L=L+1
                ISINDEXI(L)=P
                NSHELR = NSHELR + NSNFI(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
      !-------------------------------------------!
      !     Allocating total size                 !
      !-------------------------------------------!
        IF(NSHELR>0) THEN
          IF (IR4R8 == 2) THEN
            ALLOCATE(XREM(SIZ_XREM,NSHELR),STAT=IERROR)
          ELSE
            ALLOCATE(XREM(SIZ_XREM,2*NSHELR),STAT=IERROR)
            ALLOCATE(IREM(2,NSHELR),STAT=IERROR1)
            IERROR=IERROR+IERROR1
          END IF
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*SIZ_XREM
            MSGTYP = MSGOFF4 
            IAD = IDEB
            ! correction adresse pour passage tableau XREM SP utilise en DP ds la routine de comm
            IF(IR4R8 == 1) IAD = 2*IDEB-1
            CALL MPI_IRECV(
     1        XREM(1,IAD),LEN,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
            IDEB = IDEB + NSNFI(NIN)%P(P)
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
          ENDDO
          IF(IR4R8 == 1)THEN
            CALL CONVERSION11(XREM,XREM,IREM,SIZ_XREM,IDEB-1)
          END IF
        ENDIF
      ENDIF

      IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SC(P),STATUS,IERROR)
              CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
              ENDIF
          ENDIF
        ENDDO
      ENDIF

      IF(ISENDTO(NIN,LOC_PROC)/=0) THEN
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P)/=0) THEN
            IF(P/=LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
              IF(NBOX(P)/=0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(BUF(P)%p)
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      IF (IMONM > 0) CALL STOPTIME(25,1)
#endif
      RETURN
      END
