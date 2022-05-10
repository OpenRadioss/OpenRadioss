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
C
Chd|====================================================================
Chd|  SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        PUTDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD11_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD17_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20E_POFF            source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD25E_POFF            source/mpi/interfaces/spmd_fiadd25e_poff.F
Chd|        SPMD_FIADD_POFF               source/mpi/interfaces/spmd_i7tool.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I7FCOM_POFF(
     1   IPARI   ,A       ,STIFN   ,VISCN   ,
     2   INTLIST ,NBINTC  ,ICODT   ,SECFCUM ,NSTRF   ,
     3   ICONTACT,FCONT   ,ISLEN7  ,IRLEN7  ,ISLEN11 ,
     4   IRLEN11 ,ISLEN17 ,IRLEN17 ,IGRBRIC ,
     5   IXS     ,IXS16   ,FTHE    ,IRLEN7T ,
     6   ISLEN7T ,IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,
     7   IRLEN20E,ISLEN20E,CONDN   ,IFLAG   ,INTBUF_TAB, 
     8   H3D_DATA, MULTI_FVM,TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
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
#include      "i25edge_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG, NBINTC,ISLEN7, IRLEN7, ISLEN11, IRLEN11,
     .        ISLEN17, IRLEN17,IRLEN7T,ISLEN7T,
     .        IRLEN20, ISLEN20, IRLEN20T, ISLEN20T, IRLEN20E, ISLEN20E,
     .        IPARI(NPARI,*), INTLIST(*), IXS(*), IXS16(*),
     .        ICODT(*), ICONTACT(*), NSTRF(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        KLOADPINTER(NINTER+1),LOADPINTER((NINTER+1)*NLOADP_HYD),
     .        LOADP_HYD_INTER(NLOADP_HYD)
      my_real
     .        A(*), STIFN(*), VISCN(*),
     .        SECFCUM(7,NUMNOD,NSECT), FCONT(3,*), FTHE(*),CONDN(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, LL, NB, LEN, SIZ, LOC_PROC, MULTIMP, II,
     .        NIN, IDEB, N, MSGTYP, IERROR, NI, NOD, INTTH,
     .        IBC, ISECIN, IBAG, NOINT, NTY, LEN11, N1, N2,LENI,INACTI,
     .        IADM,IALLOCS, IALLOCR, LEN17, IGN, IGE, NMES, NME,  NAD,
     .        LEN7T,LEN20,LEN20T, LEN20E, INC, MSGOFF,LEN11T,
     .        JD(50), KD(50),  STATUS(MPI_STATUS_SIZE),LEN7TH,
     .        DEBUT(NINTER), DEBUTE(NINTER),LL0,
c parasiz car variable en save
     .        ADDS(PARASIZ+1), ADDR(PARASIZ+1),
     .        REQ_SI(PARASIZ),REQ_RI(PARASIZ)
      INTEGER, PARAMETER :: LEN25E = 11 
      INTEGER :: NEDGE
      DATA MSGOFF/143/
      LOGICAL ITEST
      my_real       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
      my_real
     .   BID
      SAVE ADDS,ADDR,REQ_SI,REQ_RI,IALLOCS,IALLOCR,BBUFS,BBUFR

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      BID = ZERO
      LOC_PROC = ISPMD + 1
C
      LEN = 5
      IF(KDTINT.NE.0) LEN = LEN+1
      IF(NODADT_THERM == 1) LEN = LEN+1

C type11 => 2 impacts pour une facette
      LEN11 = 2*(LEN-1)+1
      LEN11T = 2*LEN+1
C  type 7 + heat
      LEN7T = LEN + 1
C type17 => 16 impacts
      LEN17 = 69
C  type 20
      LEN20 = LEN + 18*(1+IRESP) + 1
C  type 20 + heat
      LEN20T = LEN20 + 1
C  type 20 edge
      LEN20E = LEN11 + 18*2*(1+IRESP) + 2

c
      IF(IFLAG.EQ.1)THEN
C
C Partie 1 envoi et preparation buffer reception
C

C Init
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUT(NIN) = 0
          DEBUTE(NIN)= 0
        ENDDO
        IALLOCS = LEN*(IRLEN7+IRLEN25) + LEN7T*(IRLEN7T+IRLEN25T)
     .          + LEN11T*IRLEN11 + LEN17*IRLEN17
     .          + LEN20*IRLEN20 + LEN20T*IRLEN20T + LEN20E*IRLEN20E
     .          + LEN25E*IRLEN25E
        IERROR=0
        IF(IALLOCS.GT.0)
     +    ALLOCATE(BBUFS(IALLOCS+NBINTC*NSPMD*2),STAT=IERROR) ! nbintc*NSPMD*2 majorant place supplementaire bufs
        IF(IERROR.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C
        IALLOCR = LEN*(ISLEN7+ISLEN25)  + LEN7T*(ISLEN7T+ISLEN25T)
     .          + LEN11T*ISLEN11 + LEN17*ISLEN17
     .          + LEN20*ISLEN20 + LEN20T*ISLEN20T + LEN20E*ISLEN20E
     .          + LEN25E*ISLEN25E


        IERROR=0
        IF(IALLOCR.GT.0)
     +    ALLOCATE(BBUFR(IALLOCR+NBINTC*NSPMD*2),STAT=IERROR) ! nbintc*NSPMD*2 majorant place supplementaire bufs
        IF(IERROR.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C
C Receive
C
        L = 0
        DO P = 1, NSPMD
          ADD = L+1
          ADDR(P) = ADD
          SIZ = 0
          IF(P.NE.LOC_PROC)THEN
Cel test en plus pour savoir si com globale necessaire entre les 2 procs
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NB = NSNSI(NIN)%P(P)
              NTY = IPARI(7,NIN)
              INTTH  = IPARI(47,NIN)
C             WRITE(6,*) NIN,"INTTH=",INTTH
              LENI = LEN
              IF(NTY.EQ.11)LENI=LEN11
              IF(NTY.EQ.17)LENI=LEN17
              IF(NTY.EQ.20)LENI=LEN20
              IF(NTY == 7 .AND. INTTH > 0) LENI = LEN7T
              IF(NTY == 22 .AND. INTTH > 0) LENI = LEN7T
              IF(NTY == 23 .AND. INTTH > 0) LENI = LEN7T
              IF(NTY == 20 .AND. INTTH > 0) LENI = LEN20T
              IF(NTY == 11 .AND. INTTH > 0) LENI = LEN11T
              IF(NTY == 25 .AND. INTTH > 0) LENI = LEN7T

              IF(NB.GT.0) THEN
                L = L + 1 + NB*LENI
              ENDIF
C Partie Edge
              IF(NTY.EQ.20)THEN
                NB = NSNSIE(NIN)%P(P)
                IF(NB.GT.0) THEN
                  L = L + 1 + NB*LEN20E
                END IF
              END IF
              IF(NTY.EQ.25 .AND. IPARI(58,NIN) > 0)THEN
                NB = NSNSIE(NIN)%P(P)
                IF(NB.GT.0) THEN
                  L = L + 1 + NB*LEN25E
                END IF
              END IF
            ENDDO
            SIZ = L+1-ADD
            IF(SIZ.GT.0)THEN
              MSGTYP = MSGOFF
              CALL MPI_IRECV(
     .          BBUFR(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RI(P),IERROR    )
            ENDIF
          ENDIF
        ENDDO
        ADDR(NSPMD+1) = ADDR(NSPMD)+SIZ
C
C Send
C
        L = 0
        DO P = 1, NSPMD
          ADD = L+1
          ADDS(P) = ADD
          SIZ = 0
          IF(P.NE.LOC_PROC)THEN
Cel test en plus pour savoir si com globale necessaire entre les 2 procs
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IDEB = DEBUT(NIN)
              NB = NSNFI(NIN)%P(P)
              NTY  = IPARI(7,NIN)
              INTTH =IPARI(47,NIN)
              IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.22.OR.NTY.EQ.23
     *       .OR.NTY.EQ.24.OR.NTY.EQ.25 ) THEN
               LENI = LEN
               IF(NB.GT.0) THEN
                LL = L+1
                L = L + 1
                LL0 = LL
                IF(INTTH == 0 ) THEN
c
                 IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      L = L + LEN
                    ENDIF
                   ENDDO
                 ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = VSCFI(NIN)%P(IDEB+N)
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      VSCFI(NIN)%P(IDEB+N) = ZERO
                      L = L + LEN
                    ENDIF
                   ENDDO
                 ENDIF
C
C--- interface 7 + la thermique
C
                ELSE
c
                 IF(NODADT_THERM == 1) THEN ! Thermal Time Step
                  LENI = LEN7T
                  IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = FTHEFI(NIN)%P(IDEB+N)
                      BBUFS(L+7) = CONDNFI(NIN)%P(IDEB+N)
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      FTHEFI(NIN)%P(IDEB+N)= ZERO
                      CONDNFI(NIN)%P(IDEB+N)= ZERO
                      L = L + LEN7T
                    ENDIF
                   ENDDO
                 ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = VSCFI(NIN)%P(IDEB+N)
                      BBUFS(L+7) = FTHEFI(NIN)%P(IDEB+N)
                      BBUFS(L+8) = CONDNFI(NIN)%P(IDEB+N)
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      VSCFI(NIN)%P(IDEB+N) = ZERO
                      FTHEFI(NIN)%P(IDEB+N)= ZERO
                      CONDNFI(NIN)%P(IDEB+N)= ZERO
                      L = L + LEN7T
                    ENDIF
                   ENDDO
                  ENDIF
c
                 ELSE ! NODADTTHERM
c
                  LENI = LEN7T
                  IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = FTHEFI(NIN)%P(IDEB+N)
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      FTHEFI(NIN)%P(IDEB+N)= ZERO
                      L = L + LEN7T
                    ENDIF
                   ENDDO
                 ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = VSCFI(NIN)%P(IDEB+N)
                      BBUFS(L+7) = FTHEFI(NIN)%P(IDEB+N)
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      VSCFI(NIN)%P(IDEB+N) = ZERO
                      FTHEFI(NIN)%P(IDEB+N)= ZERO
                      L = L + LEN7T
                    ENDIF
                   ENDDO
                  ENDIF
c
                 ENDIF
                ENDIF
C
                BBUFS(LL) = (L-LL0)/LENI
                DEBUT(NIN) = DEBUT(NIN) + NB
              END IF

              ELSEIF(NTY.EQ.11)THEN
C type 11
               IF(NB.GT.0) THEN
                LL = L+1
                L = L + 1
                IF(INTTH == 0) THEN
                 LENI=LEN11
c
                 IF(KDTINT.EQ.0)THEN
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      N1 = 2*(IDEB+N-1)+1
                      N2 = 2*(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,N1)
                      BBUFS(L+3) = AFI(NIN)%P(2,N1)
                      BBUFS(L+4) = AFI(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFI(NIN)%P(N1)
                      BBUFS(L+6) = AFI(NIN)%P(1,N2)
                      BBUFS(L+7) = AFI(NIN)%P(2,N2)
                      BBUFS(L+8) = AFI(NIN)%P(3,N2)
                      BBUFS(L+9) = STNFI(NIN)%P(N2)
                      AFI(NIN)%P(1,N1) = ZERO
                      AFI(NIN)%P(2,N1) = ZERO
                      AFI(NIN)%P(3,N1) = ZERO
                      STNFI(NIN)%P(N1) = ZERO
                      AFI(NIN)%P(1,N2) = ZERO
                      AFI(NIN)%P(2,N2) = ZERO
                      AFI(NIN)%P(3,N2) = ZERO
                      STNFI(NIN)%P(N2) = ZERO
                      L = L + LENI
                    ENDIF
                  ENDDO
                 ELSE
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      N1 = 2*(IDEB+N-1)+1
                      N2 = 2*(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(N1)
                      BBUFS(L+2) = AFI(NIN)%P(1,N1)
                      BBUFS(L+3) = AFI(NIN)%P(2,N1)
                      BBUFS(L+4) = AFI(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFI(NIN)%P(N1)
                      BBUFS(L+6) = VSCFI(NIN)%P(N1)
                      BBUFS(L+7) = NSVFI(NIN)%P(N2)
                      BBUFS(L+8) = AFI(NIN)%P(1,N2)
                      BBUFS(L+9) = AFI(NIN)%P(2,N2)
                      BBUFS(L+10) = AFI(NIN)%P(3,N2)
                      BBUFS(L+11) = STNFI(NIN)%P(N2)
                      BBUFS(L+12) = VSCFI(NIN)%P(N2)
                      AFI(NIN)%P(1,N1) = ZERO
                      AFI(NIN)%P(2,N1) = ZERO
                      AFI(NIN)%P(3,N1) = ZERO
                      STNFI(NIN)%P(N1) = ZERO
                      VSCFI(NIN)%P(N1) = ZERO
                      AFI(NIN)%P(1,N2) = ZERO
                      AFI(NIN)%P(2,N2) = ZERO
                      AFI(NIN)%P(3,N2) = ZERO
                      STNFI(NIN)%P(N2) = ZERO
                      VSCFI(NIN)%P(N2) = ZERO
                      L = L + LENI
                    ENDIF
                  ENDDO
                 ENDIF
c
                ELSE
C
C     Type 11 + Thermal exchange
C
                 IF(NODADT_THERM == 1) THEN ! Thermal Time Step
                  LENI=LEN11T
c
                  IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      N1 = 2*(IDEB+N-1)+1
                      N2 = 2*(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,N1)
                      BBUFS(L+3) = AFI(NIN)%P(2,N1)
                      BBUFS(L+4) = AFI(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFI(NIN)%P(N1)
                      BBUFS(L+6) = FTHEFI(NIN)%P(N1)
                      BBUFS(L+7) = CONDNFI(NIN)%P(N1)
                      BBUFS(L+8) = AFI(NIN)%P(1,N2)
                      BBUFS(L+9) = AFI(NIN)%P(2,N2)
                      BBUFS(L+10) = AFI(NIN)%P(3,N2)
                      BBUFS(L+11) = STNFI(NIN)%P(N2)
                      BBUFS(L+12) = FTHEFI(NIN)%P(N2)
                      BBUFS(L+13) = CONDNFI(NIN)%P(N2)
                      AFI(NIN)%P(1,N1) = ZERO
                      AFI(NIN)%P(2,N1) = ZERO
                      AFI(NIN)%P(3,N1) = ZERO
                      STNFI(NIN)%P(N1) = ZERO
                      AFI(NIN)%P(1,N2) = ZERO
                      AFI(NIN)%P(2,N2) = ZERO
                      AFI(NIN)%P(3,N2) = ZERO
                      STNFI(NIN)%P(N2) = ZERO
                      FTHEFI(NIN)%P(N1)= ZERO
                      FTHEFI(NIN)%P(N2)= ZERO
                      CONDNFI(NIN)%P(N1)= ZERO
                      CONDNFI(NIN)%P(N2)= ZERO
                      L = L + LENI
                    ENDIF
                   ENDDO
                  ELSE
C KINDT /= 0 Broken LENI should be 16
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      N1 = 2*(IDEB+N-1)+1
                      N2 = 2*(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(N1)
                      BBUFS(L+2) = AFI(NIN)%P(1,N1)
                      BBUFS(L+3) = AFI(NIN)%P(2,N1)
                      BBUFS(L+4) = AFI(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFI(NIN)%P(N1)
                      BBUFS(L+6) = VSCFI(NIN)%P(N1)
                      BBUFS(L+7) = FTHEFI(NIN)%P(N1)
                      BBUFS(L+8) = CONDNFI(NIN)%P(N1)
                      BBUFS(L+9) = NSVFI(NIN)%P(N2)
                      BBUFS(L+10) = AFI(NIN)%P(1,N2)
                      BBUFS(L+11) = AFI(NIN)%P(2,N2)
                      BBUFS(L+12) = AFI(NIN)%P(3,N2)
                      BBUFS(L+13) = STNFI(NIN)%P(N2)
                      BBUFS(L+14) = VSCFI(NIN)%P(N2)
                      BBUFS(L+15) = FTHEFI(NIN)%P(N2)
                      BBUFS(L+16) = CONDNFI(NIN)%P(N2)
                      AFI(NIN)%P(1,N1) = ZERO
                      AFI(NIN)%P(2,N1) = ZERO
                      AFI(NIN)%P(3,N1) = ZERO
                      STNFI(NIN)%P(N1) = ZERO
                      VSCFI(NIN)%P(N1) = ZERO
                      AFI(NIN)%P(1,N2) = ZERO
                      AFI(NIN)%P(2,N2) = ZERO
                      AFI(NIN)%P(3,N2) = ZERO
                      STNFI(NIN)%P(N2) = ZERO
                      VSCFI(NIN)%P(N2) = ZERO
                      FTHEFI(NIN)%P(N1)= ZERO
                      FTHEFI(NIN)%P(N2)= ZERO
                      CONDNFI(NIN)%P(N1)= ZERO
                      CONDNFI(NIN)%P(N2)= ZERO
                      L = L + LENI 
                    ENDIF
                   ENDDO
                  ENDIF
c
                 ELSE
                  LENI=LEN11T
c
                  IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      N1 = 2*(IDEB+N-1)+1
                      N2 = 2*(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,N1)
                      BBUFS(L+3) = AFI(NIN)%P(2,N1)
                      BBUFS(L+4) = AFI(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFI(NIN)%P(N1)
                      BBUFS(L+6) = FTHEFI(NIN)%P(N1)
                      BBUFS(L+7) = AFI(NIN)%P(1,N2)
                      BBUFS(L+8) = AFI(NIN)%P(2,N2)
                      BBUFS(L+9) = AFI(NIN)%P(3,N2)
                      BBUFS(L+10) = STNFI(NIN)%P(N2)
                      BBUFS(L+11) = FTHEFI(NIN)%P(N2)
                      AFI(NIN)%P(1,N1) = ZERO
                      AFI(NIN)%P(2,N1) = ZERO
                      AFI(NIN)%P(3,N1) = ZERO
                      STNFI(NIN)%P(N1) = ZERO
                      AFI(NIN)%P(1,N2) = ZERO
                      AFI(NIN)%P(2,N2) = ZERO
                      AFI(NIN)%P(3,N2) = ZERO
                      STNFI(NIN)%P(N2) = ZERO
                      FTHEFI(NIN)%P(N1)= ZERO
                      FTHEFI(NIN)%P(N2)= ZERO
                      L = L + LENI 
                    ENDIF
                   ENDDO
                  ELSE
C KINDT /= 0 Broken LENI should be 14
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      N1 = 2*(IDEB+N-1)+1
                      N2 = 2*(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(N1)
                      BBUFS(L+2) = AFI(NIN)%P(1,N1)
                      BBUFS(L+3) = AFI(NIN)%P(2,N1)
                      BBUFS(L+4) = AFI(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFI(NIN)%P(N1)
                      BBUFS(L+6) = VSCFI(NIN)%P(N1)
                      BBUFS(L+7) = FTHEFI(NIN)%P(N1)
                      BBUFS(L+8) = NSVFI(NIN)%P(N2)
                      BBUFS(L+9) = AFI(NIN)%P(1,N2)
                      BBUFS(L+10) = AFI(NIN)%P(2,N2)
                      BBUFS(L+11) = AFI(NIN)%P(3,N2)
                      BBUFS(L+12) = STNFI(NIN)%P(N2)
                      BBUFS(L+13) = VSCFI(NIN)%P(N2)
                      BBUFS(L+14) = FTHEFI(NIN)%P(N2)
                      AFI(NIN)%P(1,N1) = ZERO
                      AFI(NIN)%P(2,N1) = ZERO
                      AFI(NIN)%P(3,N1) = ZERO
                      STNFI(NIN)%P(N1) = ZERO
                      VSCFI(NIN)%P(N1) = ZERO
                      AFI(NIN)%P(1,N2) = ZERO
                      AFI(NIN)%P(2,N2) = ZERO
                      AFI(NIN)%P(3,N2) = ZERO
                      STNFI(NIN)%P(N2) = ZERO
                      VSCFI(NIN)%P(N2) = ZERO
                      FTHEFI(NIN)%P(N1)= ZERO
                      FTHEFI(NIN)%P(N2)= ZERO
                      L = L + LENI 
                    ENDIF
                   ENDDO
                  ENDIF
c
                 ENDIF                 
                ENDIF
                BBUFS(LL) = (L-LL)/LENI
                DEBUT(NIN) = DEBUT(NIN) + NB
               END IF
C fin type11
              ELSEIF(NTY.EQ.17)THEN
C type 17
                LENI=LEN17
                IF(NB.GT.0) THEN
                  LL = L+1
                  L = L + 1
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI17(NIN)%P(1,1,IDEB+N)
                      BBUFS(L+3) = AFI17(NIN)%P(2,1,IDEB+N)
                      BBUFS(L+4) = AFI17(NIN)%P(3,1,IDEB+N)
                      BBUFS(L+5) = STNFI17(NIN)%P(1,IDEB+N)
                      AFI17(NIN)%P(1,1,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,1,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,1,IDEB+N) = ZERO
                      STNFI17(NIN)%P(1,IDEB+N) = ZERO
C
                      BBUFS(L+6) = AFI17(NIN)%P(1,2,IDEB+N)
                      BBUFS(L+7) = AFI17(NIN)%P(2,2,IDEB+N)
                      BBUFS(L+8) = AFI17(NIN)%P(3,2,IDEB+N)
                      BBUFS(L+9)= STNFI17(NIN)%P(2,IDEB+N)
                      AFI17(NIN)%P(1,2,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,2,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,2,IDEB+N) = ZERO
                      STNFI17(NIN)%P(2,IDEB+N) = ZERO
C
                      BBUFS(L+10)= AFI17(NIN)%P(1,3,IDEB+N)
                      BBUFS(L+11)= AFI17(NIN)%P(2,3,IDEB+N)
                      BBUFS(L+12)= AFI17(NIN)%P(3,3,IDEB+N)
                      BBUFS(L+13)= STNFI17(NIN)%P(3,IDEB+N)
                      AFI17(NIN)%P(1,3,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,3,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,3,IDEB+N) = ZERO
                      STNFI17(NIN)%P(3,IDEB+N) = ZERO
C
                      BBUFS(L+14)= AFI17(NIN)%P(1,4,IDEB+N)
                      BBUFS(L+15)= AFI17(NIN)%P(2,4,IDEB+N)
                      BBUFS(L+16)= AFI17(NIN)%P(3,4,IDEB+N)
                      BBUFS(L+17)= STNFI17(NIN)%P(4,IDEB+N)
                      AFI17(NIN)%P(1,4,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,4,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,4,IDEB+N) = ZERO
                      STNFI17(NIN)%P(4,IDEB+N) = ZERO
C
                      BBUFS(L+18)= AFI17(NIN)%P(1,5,IDEB+N)
                      BBUFS(L+19)= AFI17(NIN)%P(2,5,IDEB+N)
                      BBUFS(L+20)= AFI17(NIN)%P(3,5,IDEB+N)
                      BBUFS(L+21)= STNFI17(NIN)%P(5,IDEB+N)
                      AFI17(NIN)%P(1,5,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,5,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,5,IDEB+N) = ZERO
                      STNFI17(NIN)%P(5,IDEB+N) = ZERO
C
                      BBUFS(L+22)= AFI17(NIN)%P(1,6,IDEB+N)
                      BBUFS(L+23)= AFI17(NIN)%P(2,6,IDEB+N)
                      BBUFS(L+24)= AFI17(NIN)%P(3,6,IDEB+N)
                      BBUFS(L+25)= STNFI17(NIN)%P(6,IDEB+N)
                      AFI17(NIN)%P(1,6,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,6,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,6,IDEB+N) = ZERO
                      STNFI17(NIN)%P(6,IDEB+N) = ZERO
C
                      BBUFS(L+26)= AFI17(NIN)%P(1,7,IDEB+N)
                      BBUFS(L+27)= AFI17(NIN)%P(2,7,IDEB+N)
                      BBUFS(L+28)= AFI17(NIN)%P(3,7,IDEB+N)
                      BBUFS(L+29)= STNFI17(NIN)%P(7,IDEB+N)
                      AFI17(NIN)%P(1,7,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,7,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,7,IDEB+N) = ZERO
                      STNFI17(NIN)%P(7,IDEB+N) = ZERO
C
                      BBUFS(L+30)= AFI17(NIN)%P(1,8,IDEB+N)
                      BBUFS(L+31)= AFI17(NIN)%P(2,8,IDEB+N)
                      BBUFS(L+32)= AFI17(NIN)%P(3,8,IDEB+N)
                      BBUFS(L+33)= STNFI17(NIN)%P(8,IDEB+N)
                      AFI17(NIN)%P(1,8,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,8,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,8,IDEB+N) = ZERO
                      STNFI17(NIN)%P(8,IDEB+N) = ZERO
C
                      BBUFS(L+34)= AFI17(NIN)%P(1,9,IDEB+N)
                      BBUFS(L+35)= AFI17(NIN)%P(2,9,IDEB+N)
                      BBUFS(L+36)= AFI17(NIN)%P(3,9,IDEB+N)
                      BBUFS(L+37)= STNFI17(NIN)%P(9,IDEB+N)
                      AFI17(NIN)%P(1,9,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,9,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,9,IDEB+N) = ZERO
                      STNFI17(NIN)%P(9,IDEB+N) = ZERO
C
                      BBUFS(L+38)= AFI17(NIN)%P(1,10,IDEB+N)
                      BBUFS(L+39)= AFI17(NIN)%P(2,10,IDEB+N)
                      BBUFS(L+40)= AFI17(NIN)%P(3,10,IDEB+N)
                      BBUFS(L+41)= STNFI17(NIN)%P(10,IDEB+N)
                      AFI17(NIN)%P(1,10,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,10,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,10,IDEB+N) = ZERO
                      STNFI17(NIN)%P(10,IDEB+N) = ZERO
C
                      BBUFS(L+42)= AFI17(NIN)%P(1,11,IDEB+N)
                      BBUFS(L+43)= AFI17(NIN)%P(2,11,IDEB+N)
                      BBUFS(L+44)= AFI17(NIN)%P(3,11,IDEB+N)
                      BBUFS(L+45)= STNFI17(NIN)%P(11,IDEB+N)
                      AFI17(NIN)%P(1,11,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,11,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,11,IDEB+N) = ZERO
                      STNFI17(NIN)%P(11,IDEB+N) = ZERO
C
                      BBUFS(L+46)= AFI17(NIN)%P(1,12,IDEB+N)
                      BBUFS(L+47)= AFI17(NIN)%P(2,12,IDEB+N)
                      BBUFS(L+48)= AFI17(NIN)%P(3,12,IDEB+N)
                      BBUFS(L+49)= STNFI17(NIN)%P(12,IDEB+N)
                      AFI17(NIN)%P(1,12,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,12,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,12,IDEB+N) = ZERO
                      STNFI17(NIN)%P(12,IDEB+N) = ZERO
C
                      BBUFS(L+50)= AFI17(NIN)%P(1,13,IDEB+N)
                      BBUFS(L+51)= AFI17(NIN)%P(2,13,IDEB+N)
                      BBUFS(L+52)= AFI17(NIN)%P(3,13,IDEB+N)
                      BBUFS(L+53)= STNFI17(NIN)%P(13,IDEB+N)
                      AFI17(NIN)%P(1,13,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,13,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,13,IDEB+N) = ZERO
                      STNFI17(NIN)%P(13,IDEB+N) = ZERO
C
                      BBUFS(L+54)= AFI17(NIN)%P(1,14,IDEB+N)
                      BBUFS(L+55)= AFI17(NIN)%P(2,14,IDEB+N)
                      BBUFS(L+56)= AFI17(NIN)%P(3,14,IDEB+N)
                      BBUFS(L+57)= STNFI17(NIN)%P(14,IDEB+N)
                      AFI17(NIN)%P(1,14,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,14,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,14,IDEB+N) = ZERO
                      STNFI17(NIN)%P(14,IDEB+N) = ZERO
C
                      BBUFS(L+58)= AFI17(NIN)%P(1,15,IDEB+N)
                      BBUFS(L+59)= AFI17(NIN)%P(2,15,IDEB+N)
                      BBUFS(L+60)= AFI17(NIN)%P(3,15,IDEB+N)
                      BBUFS(L+61)= STNFI17(NIN)%P(15,IDEB+N)
                      AFI17(NIN)%P(1,15,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,15,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,15,IDEB+N) = ZERO
                      STNFI17(NIN)%P(15,IDEB+N) = ZERO
C
                      BBUFS(L+62)= AFI17(NIN)%P(1,16,IDEB+N)
                      BBUFS(L+63)= AFI17(NIN)%P(2,16,IDEB+N)
                      BBUFS(L+64)= AFI17(NIN)%P(3,16,IDEB+N)
                      BBUFS(L+65)= STNFI17(NIN)%P(16,IDEB+N)
                      AFI17(NIN)%P(1,16,IDEB+N) = ZERO
                      AFI17(NIN)%P(2,16,IDEB+N) = ZERO
                      AFI17(NIN)%P(3,16,IDEB+N) = ZERO
                      STNFI17(NIN)%P(16,IDEB+N) = ZERO
C
                      BBUFS(L+66)= FROTSFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+67)= FROTSFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+68)= FROTSFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+69)= FROTSFI(NIN)%P(4,IDEB+N)
                      FROTSFI(NIN)%P(1,IDEB+N) = ZERO
                      FROTSFI(NIN)%P(2,IDEB+N) = ZERO
                      FROTSFI(NIN)%P(3,IDEB+N) = ZERO
                      FROTSFI(NIN)%P(4,IDEB+N) = ZERO
C
                      L = L + LEN17
                    ENDIF
                  ENDDO
                  BBUFS(LL) = (L-LL)/LEN17
                  DEBUT(NIN) = DEBUT(NIN) + NB
                END IF
C fin type17
              ELSEIF(NTY.EQ.20) THEN
C type20
               LENI = LEN20
               IF(NB.GT.0) THEN
                LL = L+1
                L = L + 1
                IF(INTTH == 0 ) THEN
                 IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = ALPHAKFI(NIN)%P(IDEB+N)
C
                      CALL PUTDPDAANC(
     .                 DAANC6FI(NIN)%P(1,1,IDEB+N),BBUFS(L+7),IRESP,INC)
C                      L  = L + INC
C
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
C
C
                      L = L + LEN20
                    ENDIF
                   ENDDO
                 ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = VSCFI(NIN)%P(IDEB+N)
                      BBUFS(L+7) = ALPHAKFI(NIN)%P(IDEB+N)
C
                      CALL PUTDPDAANC(
     .                 DAANC6FI(NIN)%P(1,1,IDEB+N),BBUFS(L+8),IRESP,INC)
C                      L  = L + INC
C
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      VSCFI(NIN)%P(IDEB+N) = ZERO
C
                      L = L + LEN20
                    ENDIF
                   ENDDO
                 ENDIF
C
C--- interface 20 + la thermique
C
                ELSE
                 LENI = LEN20T
                 IF(KDTINT.EQ.0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = FTHEFI(NIN)%P(IDEB+N)
                      BBUFS(L+7) = ALPHAKFI(NIN)%P(IDEB+N)
C
                      CALL PUTDPDAANC(
     .                 DAANC6FI(NIN)%P(1,1,IDEB+N),BBUFS(L+8),IRESP,INC)
C                      L  = L + INC
C
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      FTHEFI(NIN)%P(IDEB+N)= ZERO
C
                      L = L + LEN20T
                    ENDIF
                   ENDDO
                 ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+1) = NSVFI(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFI(NIN)%P(1,IDEB+N)
                      BBUFS(L+3) = AFI(NIN)%P(2,IDEB+N)
                      BBUFS(L+4) = AFI(NIN)%P(3,IDEB+N)
                      BBUFS(L+5) = STNFI(NIN)%P(IDEB+N)
                      BBUFS(L+6) = VSCFI(NIN)%P(IDEB+N)
                      BBUFS(L+7) = FTHEFI(NIN)%P(IDEB+N)
                      BBUFS(L+8) = ALPHAKFI(NIN)%P(IDEB+N)
C
                      CALL PUTDPDAANC(
     .                 DAANC6FI(NIN)%P(1,1,IDEB+N),BBUFS(L+9),IRESP,INC)
C                      L  = L + INC
C
                      AFI(NIN)%P(1,IDEB+N) = ZERO
                      AFI(NIN)%P(2,IDEB+N) = ZERO
                      AFI(NIN)%P(3,IDEB+N) = ZERO
                      STNFI(NIN)%P(IDEB+N) = ZERO
                      VSCFI(NIN)%P(IDEB+N) = ZERO
                      FTHEFI(NIN)%P(IDEB+N)= ZERO
C
                      L = L + LEN20T
                    ENDIF
                   ENDDO
                 ENDIF
                ENDIF
C
                 BBUFS(LL) = (L-LL)/LENI
                 DEBUT(NIN) = DEBUT(NIN) + NB
               END IF

              END IF
C
C Partie supplementaire type 20 edge
C
              IF(NTY.EQ.20) THEN
                NB = NSNFIE(NIN)%P(P)
                IDEB = DEBUTE(NIN)
                LENI = LEN20E
                IF(NB.GT.0) THEN
                  LL = L+1
                  L = L + 1
                  IF(KDTINT.EQ.0)THEN
                    DO N = 1, NB
                      IF(NSVFIE(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                        NSVFIE(NIN)%P(IDEB+N)=-NSVFIE(NIN)%P(IDEB+N)
                        N1 = 2*(N+IDEB-1)+1
                        N2 = 2*(N+IDEB)
                        BBUFS(L+1) = NSVFIE(NIN)%P(IDEB+N)
C
                        BBUFS(L+2) = AFIE(NIN)%P(1,N1)
                        BBUFS(L+3) = AFIE(NIN)%P(2,N1)
                        BBUFS(L+4) = AFIE(NIN)%P(3,N1)
                        BBUFS(L+5) = STNFIE(NIN)%P(N1)
                        BBUFS(L+6) = AFIE(NIN)%P(1,N2)
                        BBUFS(L+7) = AFIE(NIN)%P(2,N2)
                        BBUFS(L+8) = AFIE(NIN)%P(3,N2)
                        BBUFS(L+9) = STNFIE(NIN)%P(N2)
                        BBUFS(L+10) = ALPHAKFIE(NIN)%P(N1)
                        BBUFS(L+11) = ALPHAKFIE(NIN)%P(N2)
C
                        CALL PUTDPDAANC(
     .                  DAANC6FIE(NIN)%P(1,1,N1),BBUFS(L+12),IRESP,INC)
C                       L  = L + INC
                        CALL PUTDPDAANC(
     .                  DAANC6FIE(NIN)%P(1,1,N2),BBUFS(L+12+INC),IRESP,
     .                  INC)
C                       L  = L + INC
C
                        AFIE(NIN)%P(1,N1) = ZERO
                        AFIE(NIN)%P(2,N1) = ZERO
                        AFIE(NIN)%P(3,N1) = ZERO
                        STNFIE(NIN)%P(N1) = ZERO
                        AFIE(NIN)%P(1,N2) = ZERO
                        AFIE(NIN)%P(2,N2) = ZERO
                        AFIE(NIN)%P(3,N2) = ZERO
                        STNFIE(NIN)%P(N2) = ZERO
C
                        L = L + LEN20E
                      END IF
                    END DO
                  ELSE  ! KDTIN .NE. 0
                    DO N = 1, NB
                      IF(NSVFIE(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant une force
                        NSVFIE(NIN)%P(IDEB+N)=-NSVFIE(NIN)%P(IDEB+N)
                        N1 = 2*(N+IDEB-1)+1
                        N2 = 2*(N+IDEB)
                        BBUFS(L+1) = NSVFIE(NIN)%P(IDEB+N)
C
                        BBUFS(L+2) = AFIE(NIN)%P(1,N1)
                        BBUFS(L+3) = AFIE(NIN)%P(2,N1)
                        BBUFS(L+4) = AFIE(NIN)%P(3,N1)
                        BBUFS(L+5) = STNFIE(NIN)%P(N1)
                        BBUFS(L+6) = VSCFIE(NIN)%P(N1)
                        BBUFS(L+7) = AFIE(NIN)%P(1,N2)
                        BBUFS(L+8) = AFIE(NIN)%P(2,N2)
                        BBUFS(L+9) = AFIE(NIN)%P(3,N2)
                        BBUFS(L+10)= STNFIE(NIN)%P(N2)
                        BBUFS(L+11)= VSCFIE(NIN)%P(N2)
                        BBUFS(L+12) = ALPHAKFIE(NIN)%P(N1)
                        BBUFS(L+13) = ALPHAKFIE(NIN)%P(N2)
C
                        CALL PUTDPDAANC(
     .                  DAANC6FIE(NIN)%P(1,1,N1),BBUFS(L+14),IRESP,INC)
C                       L  = L + INC
                        CALL PUTDPDAANC(
     .                  DAANC6FIE(NIN)%P(1,1,N2),BBUFS(L+14+INC),IRESP,
     .                  INC)
C                       L  = L + INC
C
                        AFIE(NIN)%P(1,N1) = ZERO
                        AFIE(NIN)%P(2,N1) = ZERO
                        AFIE(NIN)%P(3,N1) = ZERO
                        STNFIE(NIN)%P(N1) = ZERO
                        VSCFIE(NIN)%P(N1) = ZERO
                        AFIE(NIN)%P(1,N2) = ZERO
                        AFIE(NIN)%P(2,N2) = ZERO
                        AFIE(NIN)%P(3,N2) = ZERO
                        STNFIE(NIN)%P(N2) = ZERO
                        VSCFIE(NIN)%P(N2) = ZERO
C
                        L = L + LEN20E
                      END IF
                    END DO
                  END IF ! KDTIN .NE. 0
                  BBUFS(LL) = (L-LL)/LEN20E
                  DEBUTE(NIN) = DEBUTE(NIN) + NB
                END IF
              END IF

C
C Partie supplementaire type 25 edge
C
              IF(NTY.EQ.25 .AND. IPARI(58,NIN) > 0) THEN
                NB = NSNFIE(NIN)%P(P)
                IDEB = DEBUTE(NIN)
                LENI = LEN25E
                IF(NB.GT.0) THEN
                  LL = L+1
                  L = L + 1
                  DO N = 1, NB
                    IF(NSVFIE(NIN)%P(IDEB+N).LT.0)THEN
C noeud generant u force
                      NSVFIE(NIN)%P(IDEB+N)=-NSVFIE(NIN)%P(IDEB+N)
                      N1 = 2*(N+IDEB-1)+1
                      N2 = 2*(N+IDEB)
                      BBUFS(L+1) = NSVFIE(NIN)%P(IDEB+N)
                      BBUFS(L+2) = AFIE(NIN)%P(1,N1)
                      BBUFS(L+3) = AFIE(NIN)%P(2,N1)
                      BBUFS(L+4) = AFIE(NIN)%P(3,N1)
                      BBUFS(L+5) = STNFIE(NIN)%P(N1)
                      BBUFS(L+7 ) = AFIE(NIN)%P(1,N2)
                      BBUFS(L+8 ) = AFIE(NIN)%P(2,N2)
                      BBUFS(L+9 ) = AFIE(NIN)%P(3,N2)
                      BBUFS(L+10) = STNFIE(NIN)%P(N2)
                      IF(KDTINT .NE. 0) THEN
                        BBUFS(L+6) = VSCFIE(NIN)%P(N1)
                        VSCFIE(NIN)%P(N1) = ZERO
                        BBUFS(L+11)= VSCFIE(NIN)%P(N2)
                        VSCFIE(NIN)%P(N2) = ZERO
                      ELSE
                        BBUFS(L+6) = ZERO
                        BBUFS(L+11) = ZERO
                      ENDIF

                      IF(INTTH .NE. 0) THEN
C                        FTHE(NOD1) = FTHE(NOD1) + BUFR(12,I)     
C                        FTHE(NOD2) = FTHE(NOD2) + BUFR(13,I)     
                         IF(NODADT_THERM == 1) THEN
C                           CONDN(NOD1) = CONDN(NOD1) + BUFR(14,I)    
C                           CONDN(NOD2) = CONDN(NOD2) + BUFR(15,I) 
                         ENDIF
                      ENDIF

                      AFIE(NIN)%P(1,N1) = ZERO
                      AFIE(NIN)%P(2,N1) = ZERO
                      AFIE(NIN)%P(3,N1) = ZERO
                      STNFIE(NIN)%P(N1) = ZERO
                      AFIE(NIN)%P(1,N2) = ZERO
                      AFIE(NIN)%P(2,N2) = ZERO
                      AFIE(NIN)%P(3,N2) = ZERO
                      STNFIE(NIN)%P(N2) = ZERO
                      L = L + LEN25E
                    ENDIF
                  ENDDO
                  BBUFS(LL) = (L-LL)/LEN25E
                  DEBUTE(NIN) = DEBUTE(NIN) + NB
                END IF
              END IF
C fin edge
            ENDDO
            SIZ = L+1-ADD
            IF(SIZ.GT.0)THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          BBUFS(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
            ENDIF
          ENDIF
        ENDDO
        ADDS(NSPMD+1)=ADDS(NSPMD)+SIZ
C
C Attente reception buffer et decompactage
C
      ELSEIF(IFLAG.EQ.2)THEN
C
C Attente IRECV
C
        DO P = 1, NSPMD
          IF(ADDR(P+1)-ADDR(P).GT.0) THEN
            CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
            L = ADDR(P)
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NTY   =IPARI(7,NIN)
              IF(NSNSI(NIN)%P(P).GT.0)THEN
                NB = NINT(BBUFR(L))
                L = L + 1
                IBC   =IPARI(11,NIN)
                NOINT =IPARI(15,NIN)
                INACTI=IPARI(22,NIN)
                ISECIN=IPARI(28,NIN)
                IBAG  =IPARI(32,NIN)
                IADM  =IPARI(44,NIN)
C
                IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.22.OR.NTY.EQ.23
     *       .OR.NTY.EQ.24.OR.NTY.EQ.25 )THEN
                  INTTH = IPARI(47,NIN)
                  IF(INTTH == 0 ) THEN
                   CALL SPMD_FIADD_POFF(
     1              NB      ,LEN     ,BBUFR(L),INTBUF_TAB(NIN)%NSV,A ,
     2              STIFN   ,VISCN   ,IBC     ,ISECIN       ,NOINT   ,
     3              IBAG    ,ICODT   ,SECFCUM , NSTRF       ,ICONTACT,
     4              FCONT   ,INACTI  ,IADM    ,INTTH , FTHE ,CONDN   ,
     5              H3D_DATA, MULTI_FVM,NIN   ,TAGNCONT,KLOADPINTER  ,
     6              LOADPINTER,LOADP_HYD_INTER)
                    L = L + NB*LEN
                  ELSE
                     CALL SPMD_FIADD_POFF(
     1              NB      ,LEN7T   ,BBUFR(L),INTBUF_TAB(NIN)%NSV,A ,
     2              STIFN   ,VISCN   ,IBC     ,ISECIN       ,NOINT   ,
     3              IBAG    ,ICODT   ,SECFCUM , NSTRF       ,ICONTACT,
     4              FCONT   ,INACTI  ,IADM    ,INTTH , FTHE ,CONDN   ,
     5              H3D_DATA, MULTI_FVM,NIN   ,TAGNCONT,KLOADPINTER  ,
     6              LOADPINTER,LOADP_HYD_INTER)
                    L = L + NB*LEN7T
                  ENDIF
                ELSEIF(NTY.EQ.11)THEN
                  INTTH = IPARI(47,NIN)
                 IF(INTTH==0) THEN
                  CALL SPMD_FIADD11_POFF(
     1              NB      ,LEN11   ,BBUFR(L),INTBUF_TAB(NIN)%IRECTS,A,
     2              STIFN   ,VISCN   ,IBC     ,ISECIN       ,NOINT   ,
     3              IBAG    ,ICODT   ,SECFCUM ,NSTRF        ,ICONTACT,
     4              FCONT   ,INTTH   ,FTHE    ,CONDN        ,H3D_DATA,
     5              TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
                  L = L + NB*LEN11
                 ELSE
                  CALL SPMD_FIADD11_POFF(
     1              NB      ,LEN11T   ,BBUFR(L),INTBUF_TAB(NIN)%IRECTS,A,
     2              STIFN   ,VISCN   ,IBC     ,ISECIN       ,NOINT   ,
     3              IBAG    ,ICODT   ,SECFCUM ,NSTRF        ,ICONTACT,
     4              FCONT   ,INTTH   ,FTHE    ,CONDN        ,H3D_DATA,
     5              TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
                  L = L + NB*LEN11T
                 ENDIF
                ELSEIF(NTY.EQ.17)THEN
                  IGE = IPARI(34,NIN)
                  IGN = IPARI(36,NIN)
                  NME =IGRBRIC(IGE)%NENTITY
                  NMES=IGRBRIC(IGN)%NENTITY
C
                  CALL SPMD_FIADD17_POFF(
     1              NB     ,LEN17 ,BBUFR(L),IGRBRIC(IGN)%ENTITY,A             ,
     2              STIFN  ,FCONT ,IXS     ,IXS16       ,INTBUF_TAB(NIN)%FROTS,
     3              H3D_DATA)
                  L = L + NB*LEN17
                ELSEIF(NTY.EQ.20)THEN
                  INTTH = IPARI(47,NIN)
                  IF(INTTH == 0 ) THEN
                   CALL SPMD_FIADD20_POFF(
     1              NB    ,LEN20 ,BBUFR(L),INTBUF_TAB(NIN)%NSV,A      ,
     2              STIFN ,VISCN ,IBC     ,ISECIN       ,NOINT        ,
     3              IBAG  ,ICODT ,SECFCUM ,NSTRF        ,ICONTACT     ,
     4              FCONT ,INACTI,IADM    ,INTTH        ,INTBUF_TAB(NIN)%DAANC6,
     5              BID   ,INTBUF_TAB(NIN)%NLG  ,INTBUF_TAB(NIN)%ALPHAK,H3D_DATA )
                    L = L + NB*LEN20
                  ELSE
                   CALL SPMD_FIADD20_POFF(
     1              NB    ,LEN20T,BBUFR(L),INTBUF_TAB(NIN)%NSV,A              ,
     2              STIFN ,VISCN ,IBC     ,ISECIN       ,NOINT        ,
     3              IBAG  ,ICODT ,SECFCUM ,NSTRF        ,ICONTACT     ,
     4              FCONT ,INACTI,IADM    ,INTTH        ,INTBUF_TAB(NIN)%DAANC6,
     5              FTHE  ,INTBUF_TAB(NIN)%NLG  ,INTBUF_TAB(NIN)%ALPHAK,H3D_DATA)
                    L = L + NB*LEN20T
                  ENDIF
                END IF
              END IF
C
C Partie supplementaire type 20 edge
C
              IF(NTY.EQ.20)THEN
                IF(NSNSIE(NIN)%P(P).GT.0)THEN
                  NB = NINT(BBUFR(L))
                  L = L + 1
                  IBC   =IPARI(11,NIN)
                  NOINT =IPARI(15,NIN)
                  INACTI=IPARI(22,NIN)
                  ISECIN=IPARI(28,NIN)
                  IBAG  =IPARI(32,NIN)
                  IADM  =IPARI(44,NIN)
                  CALL SPMD_FIADD20E_POFF(
     1              NB    ,LEN20E ,BBUFR(L),INTBUF_TAB(NIN)%IXLINS,A           ,
     2              STIFN ,VISCN  ,IBC     ,ISECIN       ,NOINT        ,
     3              IBAG  ,ICODT  ,SECFCUM ,NSTRF        ,ICONTACT     ,
     4           FCONT ,INTBUF_TAB(NIN)%DAANC6,INTBUF_TAB(NIN)%NLG,INTBUF_TAB(NIN)%ALPHAK,H3D_DATA)
                    L = L + NB*LEN20E
                ENDIF
              ENDIF
C
C Partie supplementaire type 25 edge
C
              IF(NTY.EQ.25 .AND. IPARI(58,NIN) > 0)THEN
                IF(NSNSIE(NIN)%P(P).GT.0)THEN
                  NB = NINT(BBUFR(L))
                  L = L + 1
                  IBC   =IPARI(11,NIN)
                  NOINT =IPARI(15,NIN)
                  INACTI=IPARI(22,NIN)
                  ISECIN=IPARI(28,NIN)
                  IBAG  =IPARI(32,NIN)
                  IADM  =IPARI(44,NIN)
                  INTTH  = IPARI(47,NIN)
                  NEDGE =IPARI(68,NIN)

                  CALL SPMD_FIADD25E_POFF(
     1              NB      ,LEN25E     ,BBUFR(L),INTBUF_TAB(NIN)%NSV,A ,
     2              STIFN   ,VISCN   ,IBC     ,ISECIN       ,NOINT   ,
     3              IBAG    ,ICODT   ,SECFCUM , NSTRF       ,ICONTACT,
     4              FCONT   ,INACTI  ,IADM    ,INTTH , FTHE ,CONDN   ,
     5              H3D_DATA, MULTI_FVM,INTBUF_TAB(NIN)%LEDGE,NEDGE  ,
     6              NIN     ,TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
                    L = L + NB*LEN25E
                ENDIF ! NSNSIE
              ENDIF ! NTY + IEDGE
            ENDDO ! NBINTC
          ENDIF !FLAG
        ENDDO ! NSPMD

C Deallocation R
        IF(IALLOCR.GT.0) THEN
          DEALLOCATE(BBUFR)
          IALLOCR=0
        END IF
C
C Attente ISEND
C
        DO P = 1, NSPMD
          IF(ADDS(P+1)-ADDS(P).GT.0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
        ENDDO
C Deallocation S
        IF(IALLOCS.GT.0) THEN
          DEALLOCATE(BBUFS)
          IALLOCS=0
        END IF
      END IF
C
#endif
      RETURN
      END
