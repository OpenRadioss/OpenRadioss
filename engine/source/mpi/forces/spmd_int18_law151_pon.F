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
Chd|  SPMD_INT18_LAW151_PON         source/mpi/forces/spmd_int18_law151_pon.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
        SUBROUTINE SPMD_INT18_LAW151_PON(     
     1   IPARI   ,ISLEN7  ,IRLEN7  ,IFLAG   ,INTBUF_TAB,
     2   MULTI_FVM)
!$COMMENT
!       SPMD_INT18_LAW151_PON description
!       communication of the remote values of the 
!       phantom nodes 
!       
!       SPMD_INT18_LAW151_PON organization :
!           part 1 = received part & send part :
!                    - 6*3 values per phantom nodes
!                    - 1 value for the number of send values (=nb)
!
!       buffer (send & received) organization
!           | 1  |  2 | ... |  6 |  7 |  8 |... |  nb | nb+1 | nb+2| ...
!           | NB1| x1 |     | x1 | x1 | y1 |... | znb | NB2  | ...
!           |---------|-------------------------------|----------|---------------|
!                   proc1                                     proc2
!           
!           part 2 = accumulation of received values in the local array
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE MESSAGE_MOD
        USE INTBUFDEF_MOD
        USE MULTI_FVM_MOD
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
        INTEGER, INTENT(in) :: IFLAG, ISLEN7, IRLEN7
        INTEGER, DIMENSION(NPARI,*), INTENT(in) :: IPARI

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER :: P,LOC_PROC
        INTEGER :: IBRIC
        INTEGER :: L,II,IJ,LL,LL0,N,NI
        INTEGER :: ADD,NB,LEN,LENI,SIZ,IDEB
        INTEGER :: NIN,NTY,NB_INT18
        INTEGER :: IERROR,IALLOCS, IALLOCR
        INTEGER :: MSGTYP,MSGOFF
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS
        INTEGER, DIMENSION(NINTER) :: DEBUT,DEBUTE
        INTEGER, DIMENSION(PARASIZ) :: REQ_SI,REQ_RI
        INTEGER, DIMENSION(PARASIZ+1) :: ADDS,ADDR
c parasiz car variable en save
        DATA MSGOFF/14141/
        REAL(kind=8),DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
        SAVE ADDS,ADDR,REQ_SI,REQ_RI,IALLOCS,IALLOCR,BBUFS,BBUFR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        LOC_PROC = ISPMD + 1
        NB_INT18 = MULTI_FVM%NUMBER_INT18
C
        !   18 values for the forces (6 per direction x/y/z)
        !   1 value for the 
        LEN = 3*6 + 1  

        IF(IFLAG.EQ.1)THEN
C
C Partie 1 envoi et preparation buffer reception
C
            
C Init
            DO II=1,MULTI_FVM%NUMBER_INT18
                NIN = MULTI_FVM%INT18_LIST(II)
                DEBUT(NIN) = 0
                DEBUTE(NIN)= 0
            ENDDO
            IALLOCS = LEN*IRLEN7
            IERROR=0
            IF(IALLOCS.GT.0)
     +      ALLOCATE(BBUFS(IALLOCS+NB_INT18*NSPMD*2),STAT=IERROR) ! nbintc*NSPMD*2 majorant place supplementaire bufs
            IF(IERROR.NE.0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
            END IF
C
            IALLOCR = LEN*ISLEN7

            IERROR=0
            IF(IALLOCR.GT.0)
     +      ALLOCATE(BBUFR(IALLOCR+NB_INT18*NSPMD*2),STAT=IERROR) ! nbintc*NSPMD*2 majorant place supplementaire bufs
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
                    DO II=1,MULTI_FVM%NUMBER_INT18
                        NIN = MULTI_FVM%INT18_LIST(II)
                        NB = NSNSI(NIN)%P(P)
                        NTY = IPARI(7,NIN)
                        LENI = LEN

                        IF(NB.GT.0) THEN
                            L = L + 1 + NB*LENI
                        ENDIF
                    ENDDO
                    SIZ = L+1-ADD
                    IF(SIZ.GT.0)THEN
                        MSGTYP = MSGOFF
                        CALL MPI_IRECV( BBUFR(ADD),SIZ,MPI_DOUBLE_PRECISION,
     .                                  IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_RI(P),IERROR )
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
                    DO II=1,MULTI_FVM%NUMBER_INT18
                        NIN = MULTI_FVM%INT18_LIST(II)
                        IDEB = DEBUT(NIN)
                        NB = NSNFI(NIN)%P(P)
                        NTY  = IPARI(7,NIN)
                        LENI = LEN
                        IF(NB.GT.0) THEN
                            LL = L+1
                            L = L + 1
                            LL0 = LL
                            DO N = 1, NB
                                IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
                                    L = L + 1
                                    BBUFS(L) = -NSVFI(NIN)%P(IDEB+N)
                                    DO IJ=1,6
                                       BBUFS(L+IJ)  = MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(1,IJ,IDEB+N)
                                       BBUFS(L+6+IJ)  = MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(2,IJ,IDEB+N)
                                       BBUFS(L+12+IJ) = MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(3,IJ,IDEB+N)
                                    ENDDO
                                    MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(1:3,1:6,IDEB+N) = 0.d+00   
                                    L = L + 18
                                ENDIF
                            ENDDO
                            BBUFS(LL) = (L-LL0)/LENI
                            DEBUT(NIN) = DEBUT(NIN) + NB
                        END IF
                    ENDDO ! ni = 1,ninter

                    SIZ = L+1-ADD
                    IF(SIZ.GT.0)THEN
                        MSGTYP = MSGOFF
                        CALL MPI_ISEND( BBUFS(ADD),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     .                                 MSGTYP,MPI_COMM_WORLD,REQ_SI(P),IERROR )
                    ENDIF ! siz > 0
                ENDIF ! p /= proc
            ENDDO ! p=1,nspmd
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
                    DO II=1,MULTI_FVM%NUMBER_INT18
                        NIN = MULTI_FVM%INT18_LIST(II)
                        NTY   =IPARI(7,NIN)
                        IF(NSNSI(NIN)%P(P).GT.0)THEN
                            NB = NINT(BBUFR(L))                   
                            L = L + 1
                            DO IJ=1,NB
                                IBRIC = INTBUF_TAB(NIN)%NSV( NINT(BBUFR(L)) ) !   id of the phantom node
                                MULTI_FVM%FORCE_INT_PON(1,1:6,IBRIC) = 
     .                          MULTI_FVM%FORCE_INT_PON(1,1:6,IBRIC) + BBUFR(L+1:L+6)
                                MULTI_FVM%FORCE_INT_PON(2,1:6,IBRIC) = 
     .                          MULTI_FVM%FORCE_INT_PON(2,1:6,IBRIC) + BBUFR(L+7:L+12)
                                MULTI_FVM%FORCE_INT_PON(3,1:6,IBRIC) = 
     .                          MULTI_FVM%FORCE_INT_PON(3,1:6,IBRIC) + BBUFR(L+13:L+18)
                                L = L + LEN
                            ENDDO
!                            L = L + NB*LEN
                        END IF   ! NSNSI > 0           
                    ENDDO ! DO II=1,MULTI_FVM%NUMBER_INT18
                ENDIF !FLAG adress
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
        END IF  !   IFLAG = 1 or 2
#endif
        RETURN
        END SUBROUTINE SPMD_INT18_LAW151_PON
