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

#define TO1D(i,j,k,s1,s2) i+(j-1)*s1+(k-1)*s1*s2
Chd|====================================================================
Chd|  SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        PUTDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        PUTDPZERO                     source/mpi/interfaces/spmd_i7tool.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I7XVCOM2(
     1   IPARI   ,X       ,V       ,MS      ,
     2   IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3   ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4   IRLEN17 ,IXS    ,IXS16   ,
     5   IGRBRIC ,TEMP   ,IFLAG   ,IRLEN7T ,ISLEN7T ,
     6   IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7   ISLEN20E,IKINE  ,DIAG_SMS,SENSOR  ,INTBUF_TAB,INT24E2EUSE,
     8   FORNEQS ,MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
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
#include      "assert.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "sms_c.inc"
#include      "rad2r_c.inc"
#include      "i25edge_c.inc"
#include      "macro.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMSCH, I2MSCH, IFLAG, NBINTC,
     .        ISLEN7, IRLEN7, ISLEN11, IRLEN11, ISLEN17, IRLEN17,
     .        IRLEN7T, ISLEN7T,IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,
     .        IRLEN20E,ISLEN20E,
     .        IPARI(NPARI,*),  INTLIST(*),
     .        IXS(NIXS,*), IXS16(8,*),IKINE(*),INT24E2EUSE
      my_real
     .        X(3,*), V(3,*), MS(*), TEMP(*),
     .        DT2PREV, DIAG_SMS(*), SENSOR(NSENSR,*),
     .        FORNEQS(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, NB, LEN, SIZ, LOC_PROC, I, NOD, NSN,
     .        NIN, IDEB, N, MSGTYP, IERROR, DT_CST, II,
     .        N1, N2, LENI, LEN11, NTY, IALLOCS, IALLOCR, INACTI,
     .        LEN17, IGN, IGE, NMES, NME, NAD, NN, IES, LEN7T,
     .        LEN20, LEN20T, NLN, LEN20E, INC,LEN11T,
     .        IL, IL1, IL2, MSGOFF,INTNITSCHE,LENNIT,
     .        STATUS(MPI_STATUS_SIZE),
C parasiz car variable en save
     .        DEBUT(NINTER),  DEBUTE(NINTER),
     .        ADDS(PARASIZ+1), ADDR(PARASIZ+1),
     .        REQ_SI(PARASIZ),REQ_RI(PARASIZ),ISTOCOM,INTTH,IRBY,
     .        ISENS,INTERACT,POINT,LSAV
       INTEGER :: LOC_NOD
C Type 25 E2E:
       INTEGER IAM,JAM,M1,M2,NRTM
C
      my_real
     .        VDT, X1, X2, X3, X4,X9, X10, X11, X12, XC, AN, AX,
     .        D4, BN, BX, CN, CX, D8, DN, DX, STARTT, STOPT, TS
      my_real       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
C type 25
      INTEGER IEDGE,LEN25E
      INTEGER I1,I2,IE,JE
      DOUBLE PRECISION
     *   ZERODP
      DATA MSGOFF/161/
      DATA ISTOCOM/0/
      SAVE ADDS,ADDR,REQ_SI,REQ_RI,ISTOCOM,
     .     IALLOCS,IALLOCR,BBUFS,BBUFR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ZERODP=0.0
C
      LOC_PROC = ISPMD + 1
      DT_CST = 0
      IF(NBI18 /= 0)DT_CST = 1
      IF (IDTMIN(11).EQ.3.OR.IDTMIN(11)==8.OR.IDTMIN(10).EQ.3) THEN
        IF(IMSCH.GT.0) DT_CST = 1
      ENDIF
      IF ((IRAD2R.EQ.1).AND.(NCYCLE.LT.2)) DT_CST = 1
      IF(I2MSCH.EQ.1) DT_CST = 1
      LEN = 3 + 3*MAX(IRESP,IMPL_S,MIN(NBI18,1),INT24E2EUSE) + DT_CST
C type 7 + heat
      LEN7T  = LEN + 1
C  NITSCHE
      IF(NITSCHE > 0) LEN =  LEN +3
      IF(NITSCHE > 0) LEN7T =  LEN7T +3
C
      LEN11 = 2*LEN
C type 11 + heat
      LEN11T = 2*LEN+2
      LEN17 = 100
C  type 20
      LEN20 = 3 + 3 + DT_CST + 18*(1+IRESP) + 6 + 3 +1      ! car X tjrs envoye
      IF(IDTMINS > 0 .OR. IDTMINS_INT /= 0) LEN20 = LEN20 + 1
C  type 20 + heat
      LEN20T = LEN20 + 1
C type 20 edge
      LEN20E = 2 * (3 + 3 + DT_CST + 18*(1+IRESP) + 6 + 3 +1) + 3  ! base sur 2 fois taille int11 + specifique type 20
      IF(IDTMINS > 0 .OR. IDTMINS_INT /= 0 ) LEN20E = LEN20E + 2

C type 25
      LEN25E = 14 ! 2 x [  X(3) +  V(3) +  MS(1) ] +

C
      IF(IFLAG.EQ.1)THEN
C
C Partie 1 envoi et preparation buffer reception
C

C
C Init
C
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUT(NIN) = 0
          DEBUTE(NIN) = 0
        ENDDO
C
        IALLOCS = LEN *(ISLEN25+ISLEN7) + LEN7T*(ISLEN25T+ISLEN7T)
     .                       + LEN11T*ISLEN11 + LEN17*ISLEN17
     .          + LEN20*ISLEN20 + LEN20T*ISLEN20T + LEN20E*ISLEN20E
     .          + LEN25E*ISLEN25E
        IERROR=0
        IF(IALLOCS.GT.0)
     +    ALLOCATE(BBUFS(IALLOCS),STAT=IERROR)
        IF(IERROR.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C
        IALLOCR = LEN*(IRLEN7+IRLEN25) + LEN7T*(IRLEN7T+IRLEN25T)
     .                       + LEN11T*IRLEN11 + LEN17*IRLEN17
     .          + LEN20*IRLEN20 + LEN20T*IRLEN20T + LEN20E*IRLEN20E
     .          + LEN25E*IRLEN25E

        IERROR=0
        IF(IALLOCR.GT.0)
     +    ALLOCATE(BBUFR(IALLOCR),STAT=IERROR)
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
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NB  = NSNFI(NIN)%P(P)
C
              NTY =IPARI(7,NIN)
              INTTH = IPARI(47,NIN)
              INACTI = IPARI(22,NIN)
              INTNITSCHE = IPARI(86,NIN)
              IEDGE = IPARI(58,NIN)
C
              IF(NTY/=25.AND.(NTY.EQ.23.OR.
     .           INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7))THEN
C Attention aux interfaces inactives
               ISENS = 0
               INTERACT = 1
               IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.
     .            NTY == 21.OR.NTY == 5.OR.NTY == 19 .OR.
     .            NTY == 25) ISENS = IPARI(64,NIN)  
               IF(ISENS.NE.0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
                  TS = SENSOR(2,ISENS)
                  IF (TS.GT.TT-DT2)  INTERACT = 0
               ELSE
                  STARTT = INTBUF_TAB(NIN)%VARIABLES(3)
                  STOPT  = INTBUF_TAB(NIN)%VARIABLES(11)              
                  IF (STARTT.GT.TT-DT2) INTERACT = 0
               ENDIF   
C on force le nombre d entite a envoyer a 0 tant que l'interface est inactive (jusqu au retri suivant)
                IF (INTERACT.EQ.0) NB=0
              END IF
              LENI = LEN
              IF(NTY.EQ.11)LENI=LEN11
              IF(NTY.EQ.17)LENI=LEN17
              IF(NTY == 7 .AND. INTTH > 0 ) LENI = LEN7T
              IF(NTY == 22 .AND. INTTH > 0 ) LENI = LEN7T
              IF(NTY == 23 .AND. INTTH > 0 ) LENI = LEN7T
              IF(NTY == 24 .AND. INTTH > 0 ) LENI = LEN7T
              IF(NTY == 25 .AND. INTTH > 0 ) LENI = LEN7T
              IF(NTY.EQ.20) LENI = LEN20
              IF(NTY == 20 .AND. INTTH > 0) LENI = LEN20T
              IF(NTY.EQ.11 .AND. INTTH > 0) LENI=LEN11T
c              IF(NTY == 24 .AND. INTNITSCHE > 0 ) LENI = LENI + 3
              IF(NB.GT.0) THEN
                L = L + NB*LENI
              ENDIF
C ajout partie edge pour type 20
              IF(NTY.EQ.20)THEN
                NB = NSNFIE(NIN)%P(P)
                IF(NB.GT.0) L = L + NB*LEN20E
              END IF
C ajout partie edge pour type 25
              IF(NTY.EQ.25 .AND. IEDGE .NE. 0)THEN
                NB = NSNFIE(NIN)%P(P)
                IF(NB.GT.0) L = L + NB*LEN25E
              END IF

            ENDDO
            SIZ = L+1-ADD
            IF(SIZ.GT.0)THEN
              MSGTYP = MSGOFF
C BUFS : envoi partie force et reception partie vitesse
              CALL MPI_IRECV(
     .          BBUFR(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RI(P),IERROR    )
            ENDIF
          ENDIF
        ENDDO
        ADDR(NSPMD+1) = ADDR(NSPMD)+SIZ
        IF(L.GT.0) THEN
          ISTOCOM = 1
          DT2PREV =DT2
        ENDIF
C
C Send
C
        L = 0
        DO P = 1, NSPMD
          ADD = L+1
          ADDS(P) = ADD
          SIZ = 0
          IF(P.NE.LOC_PROC)THEN
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IDEB = DEBUT(NIN)
              NB = NSNSI(NIN)%P(P)
              IF(NB.GT.0) THEN
                NTY   =IPARI(7,NIN)
                INTTH = IPARI(47,NIN)
                INACTI = IPARI(22,NIN)           
                INTNITSCHE = IPARI(86,NIN)
                LSAV=L
                IF((NTY.EQ.7 .AND. INTTH ==0 ).OR.NTY.EQ.10.OR.
     .             (NTY.EQ.22 .AND. INTTH ==0) .OR.
     .             (NTY.EQ.23 .AND. INTTH ==0) .OR.
     .             (NTY.EQ.24 .AND. INTTH ==0.AND.INTNITSCHE == 0) .OR.
     .             (NTY.EQ.25 .AND. INTTH ==0)) THEN
                 ! ---------------------------------
                 IF(DT_CST.NE.0)THEN
                  ! ---------------------------------
                  IF(IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      ! ---------------------------------
                      ! Type 24 
                      IF(NOD > NUMNOD)THEN
C This is for Type24 E2E
C T24 E2E fictive nodes have ID higher than Numnod. Position & Mass are stored in interface buffer
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+7) = INTBUF_TAB(NIN)%MSFIC(NOD-NUMNOD)
                      ! ---------------------------------
                      ELSEIF( MULTI_FVM%IS_INT18_LAW151.AND. ((NTY==7).AND.(INACTI ==7)) ) THEN
                      ! Type 18 + Law 151
                        LOC_NOD = NOD + NUMNOD - 1
                        BBUFS(L+1) = MULTI_FVM%X_APPEND( 3*LOC_NOD+1 )
                        BBUFS(L+2) = MULTI_FVM%X_APPEND( 3*LOC_NOD+2 )
                        BBUFS(L+3) = MULTI_FVM%X_APPEND( 3*LOC_NOD+3 )
                        BBUFS(L+4) = MULTI_FVM%V_APPEND( 3*LOC_NOD+1 )
                        BBUFS(L+5) = MULTI_FVM%V_APPEND( 3*LOC_NOD+2 )
                        BBUFS(L+6) = MULTI_FVM%V_APPEND( 3*LOC_NOD+3 )
                        BBUFS(L+7) = MULTI_FVM%MASS_APPEND( LOC_NOD+1 )
                      ! ---------------------------------
                      ELSE 
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                        BBUFS(L+7) = MS(NOD)
                      ENDIF
                      ! ---------------------------------
                      L = L + LEN
                    ENDDO
                    POINT = 7
                  ! ---------------------------------
                  ELSE ! IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.I ...
                    DO I = 1, NB
                      N =NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = MS(NOD)
                      L = L + LEN
                    ENDDO
                    POINT = 4
                  ENDIF
                 ! ---------------------------------
                 ELSE   ! dt_cst/=0
                  ! ---------------------------------
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      ! ---------------------------------
                      ! Type 24
                      IF(NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                      ! ---------------------------------
                      ELSEIF( MULTI_FVM%IS_INT18_LAW151.AND. ((NTY==7).AND.(INACTI ==7)) ) THEN
                      ! Type 18 + Law 151
                        LOC_NOD = NOD + NUMNOD - 1
                        BBUFS(L+1) = MULTI_FVM%X_APPEND( 3*LOC_NOD+1 )
                        BBUFS(L+2) = MULTI_FVM%X_APPEND( 3*LOC_NOD+2 )
                        BBUFS(L+3) = MULTI_FVM%X_APPEND( 3*LOC_NOD+3 )
                        BBUFS(L+4) = MULTI_FVM%V_APPEND( 3*LOC_NOD+1 )
                        BBUFS(L+5) = MULTI_FVM%V_APPEND( 3*LOC_NOD+2 )
                        BBUFS(L+6) = MULTI_FVM%V_APPEND( 3*LOC_NOD+3 )
                      ! ---------------------------------
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                      ENDIF
                      ! ---------------------------------
                      L = L + LEN
                    ENDDO
                      POINT = 6
                  ! ---------------------------------
                  ELSE ! IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      ! ---------------------------------
                      ! Type 24 
                      IF (NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                      ! ---------------------------------
                      ELSE
                        BBUFS(L+1) = V(1,NOD)
                        BBUFS(L+2) = V(2,NOD)
                        BBUFS(L+3) = V(3,NOD)
                      ENDIF
                      ! ---------------------------------
                      L = L + LEN
                    ENDDO
                      POINT = 3
                  ENDIF
                  ! ---------------------------------
                 ENDIF
C
                ELSEIF((NTY== 7.AND.INTTH>0).OR.
     .                 (NTY==25.AND.INTTH>0).OR.
     .                 (NTY==24.AND.INTTH>0.AND.INTNITSCHE == 0).OR.
     .                 (NTY==23.AND.INTTH>0).OR.
     .                 (NTY==22.AND.INTTH>0) )THEN
                 ! ---------------------------------
                 IF(DT_CST.NE.0)THEN
                  ! ---------------------------------
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      ! ---------------------------------
                      ! Type 24 
                      IF(NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+7) = INTBUF_TAB(NIN)%MSFIC(NOD-NUMNOD)
                      ! ---------------------------------
                      ELSEIF( MULTI_FVM%IS_INT18_LAW151.AND. ((NTY==7).AND.(INACTI ==7)) ) THEN
                      ! Type 18 + Law 151
                        LOC_NOD = NOD + NUMNOD - 1
                        BBUFS(L+1) = MULTI_FVM%X_APPEND( 3*LOC_NOD+1 )
                        BBUFS(L+2) = MULTI_FVM%X_APPEND( 3*LOC_NOD+2 )
                        BBUFS(L+3) = MULTI_FVM%X_APPEND( 3*LOC_NOD+3 )
                        BBUFS(L+4) = MULTI_FVM%V_APPEND( 3*LOC_NOD+1 )
                        BBUFS(L+5) = MULTI_FVM%V_APPEND( 3*LOC_NOD+2 )
                        BBUFS(L+6) = MULTI_FVM%V_APPEND( 3*LOC_NOD+3 )
                        BBUFS(L+7) = MULTI_FVM%MASS_APPEND( LOC_NOD+1 )
                      ! ---------------------------------
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                        BBUFS(L+7) = MS(NOD)
                      ENDIF
                      ! ---------------------------------
                      BBUFS(L+8) = TEMP(NOD)
                      L = L + LEN7T
                    ENDDO
                     POINT=8
                  ! ---------------------------------
                  ELSE ! IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT
                    DO I = 1, NB
                      N =NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = MS(NOD)
                      BBUFS(L+5) = TEMP(NOD)
                      L = L + LEN7T
                    ENDDO
                    POINT = 5
                  ENDIF
                  ! ---------------------------------
                 ! ---------------------------------
                 ELSE ! dt_cst /=0
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                  ! ---------------------------------
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      ! ---------------------------------
                      ! Type 24 
                      IF(NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                      ! ---------------------------------
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                      ENDIF
                      ! ---------------------------------
                      BBUFS(L+7) = TEMP(NOD)
                      L = L + LEN7T
                    ENDDO
                    POINT = 7
                  ! ---------------------------------
                  ELSE ! IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = TEMP(NOD)
                      L = L + LEN7T
                    ENDDO
                    POINT = 4
                  ENDIF
                  ! ---------------------------------
                 ENDIF ! dst_cst /=0
                 ! ---------------------------------
C--------Nitsche
                ELSEIF(NTY.EQ.24 .AND. INTTH ==0.AND.INTNITSCHE > 0) THEN
                 IF(DT_CST.NE.0)THEN
                  IF(IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      IF(NOD > NUMNOD)THEN
C This is for Type24 E2E
C T24 E2E fictive nodes have ID higher than Numnod. Position & Mass are stored in interface buffer
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+7) = INTBUF_TAB(NIN)%MSFIC(NOD-NUMNOD)
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                        BBUFS(L+7) = MS(NOD)
                      ENDIF
                      BBUFS(L+8)  = FORNEQS(1,NOD)
                      BBUFS(L+9)  = FORNEQS(2,NOD)
                      BBUFS(L+10) = FORNEQS(3,NOD)
                      L = L + LEN !+ 3
                    ENDDO
                    POINT = 10
                  ELSE
                    DO I = 1, NB
                      N =NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = MS(NOD)
                      BBUFS(L+5) = FORNEQS(1,NOD)
                      BBUFS(L+6) = FORNEQS(2,NOD)
                      BBUFS(L+7) = FORNEQS(3,NOD)
                      L = L + LEN !+ 3
                    ENDDO
                    POINT = 7
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      IF(NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                      ENDIF
                      BBUFS(L+7) = FORNEQS(1,NOD)
                      BBUFS(L+8) = FORNEQS(2,NOD)
                      BBUFS(L+9) = FORNEQS(3,NOD)
                      L = L + LEN !+ 3
                    ENDDO
                      POINT = 9
                  ELSE
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      IF (NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                      ELSE
                        BBUFS(L+1) = V(1,NOD)
                        BBUFS(L+2) = V(2,NOD)
                        BBUFS(L+3) = V(3,NOD)
                      ENDIF
                      BBUFS(L+4) = FORNEQS(1,NOD)
                      BBUFS(L+5) = FORNEQS(2,NOD)
                      BBUFS(L+6) = FORNEQS(3,NOD)
                      L = L + LEN !+ 3
                    ENDDO
                      POINT = 6
                  ENDIF
                 ENDIF
C

C----Nitsche
C
                ELSEIF(NTY==24.AND.INTTH>0.AND.INTNITSCHE > 0) THEN
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      IF(NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+7) = INTBUF_TAB(NIN)%MSFIC(NOD-NUMNOD)
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                        BBUFS(L+7) = MS(NOD)
                      ENDIF
                      BBUFS(L+8)  = TEMP(NOD)
                      BBUFS(L+9)  = FORNEQS(1,NOD)
                      BBUFS(L+10) = FORNEQS(2,NOD)
                      BBUFS(L+11) = FORNEQS(3,NOD)
                      L = L + LEN7T !+ 3
                    ENDDO
                     POINT=11
                  ELSE
                    DO I = 1, NB
                      N =NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = MS(NOD)
                      BBUFS(L+5) = TEMP(NOD)
                      BBUFS(L+6) = FORNEQS(1,NOD)
                      BBUFS(L+7) = FORNEQS(2,NOD)
                      BBUFS(L+8) = FORNEQS(3,NOD)
                      L = L + LEN7T !+ 3
                    ENDDO
                    POINT = 8
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      IF(NOD > NUMNOD)THEN
                        BBUFS(L+1) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+2) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+3) = INTBUF_TAB(NIN)%XFIC(3*(NOD-NUMNOD-1)+3)
                        BBUFS(L+4) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+1)
                        BBUFS(L+5) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+2)
                        BBUFS(L+6) = INTBUF_TAB(NIN)%VFIC(3*(NOD-NUMNOD-1)+3)
                      ELSE
                        BBUFS(L+1) = X(1,NOD)
                        BBUFS(L+2) = X(2,NOD)
                        BBUFS(L+3) = X(3,NOD)
                        BBUFS(L+4) = V(1,NOD)
                        BBUFS(L+5) = V(2,NOD)
                        BBUFS(L+6) = V(3,NOD)
                      ENDIF
                      BBUFS(L+7) = TEMP(NOD)
                      BBUFS(L+8) = FORNEQS(1,NOD)
                      BBUFS(L+9) = FORNEQS(2,NOD)
                      BBUFS(L+10)= FORNEQS(3,NOD)
                      L = L + LEN7T !+ 3
                    ENDDO
                    POINT = 10
                  ELSE
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%NSV(N)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = TEMP(NOD)
                      BBUFS(L+5) = FORNEQS(1,NOD)
                      BBUFS(L+6) = FORNEQS(2,NOD)
                      BBUFS(L+7) = FORNEQS(3,NOD)
                      L = L + LEN7T !+ 3
                    ENDDO
                    POINT = 7
                  ENDIF
                 ENDIF


C
                ELSEIF(NTY.EQ.11.AND.INTTH==0)THEN
C type 11
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP .EQ. 1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+8) = X(1,NOD)
                      BBUFS(L+9) = X(2,NOD)
                      BBUFS(L+10) = X(3,NOD)
                      BBUFS(L+11) = V(1,NOD)
                      BBUFS(L+12) = V(2,NOD)
                      BBUFS(L+13) = V(3,NOD)
                      BBUFS(L+14) = MS(NOD)
                      L = L + LEN11
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N =NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = MS(NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+5) = V(1,NOD)
                      BBUFS(L+6) = V(2,NOD)
                      BBUFS(L+7) = V(3,NOD)
                      BBUFS(L+8) = MS(NOD)
                      L = L + LEN11
                    ENDDO
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+7) = X(1,NOD)
                      BBUFS(L+8) = X(2,NOD)
                      BBUFS(L+9) = X(3,NOD)
                      BBUFS(L+10) = V(1,NOD)
                      BBUFS(L+11) = V(2,NOD)
                      BBUFS(L+12) = V(3,NOD)
                      L = L + LEN11
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      L = L + LEN11
                    ENDDO
                  ENDIF
                 ENDIF
                ELSEIF(NTY.EQ.11.AND.INTTH > 0)THEN
C type 11 + thermal exchange
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP .EQ. 1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
                      BBUFS(L+8) = TEMP(NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+9) = X(1,NOD)
                      BBUFS(L+10) = X(2,NOD)
                      BBUFS(L+11) = X(3,NOD)
                      BBUFS(L+11) = V(1,NOD)
                      BBUFS(L+12) = V(2,NOD)
                      BBUFS(L+13) = V(3,NOD)
                      BBUFS(L+14) = MS(NOD)
                      BBUFS(L+15) = TEMP(NOD)
                      L = L + LEN11T
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N =NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = MS(NOD)
                      BBUFS(L+5) = TEMP(NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+6) = V(1,NOD)
                      BBUFS(L+7) = V(2,NOD)
                      BBUFS(L+8) = V(3,NOD)
                      BBUFS(L+9) = MS(NOD)
                      BBUFS(L+10) = TEMP(NOD)
                      L = L + LEN11T
                    ENDDO
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = TEMP(NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+8) = X(1,NOD)
                      BBUFS(L+9) = X(2,NOD)
                      BBUFS(L+10) = X(3,NOD)
                      BBUFS(L+11) = V(1,NOD)
                      BBUFS(L+12) = V(2,NOD)
                      BBUFS(L+13) = V(3,NOD)
                      BBUFS(L+14) = TEMP(NOD)
                      L = L + LEN11T
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+1)
                      BBUFS(L+1) = V(1,NOD)
                      BBUFS(L+2) = V(2,NOD)
                      BBUFS(L+3) = V(3,NOD)
                      BBUFS(L+4) = TEMP(NOD)
                      NOD = INTBUF_TAB(NIN)%IRECTS(2*(N-1)+2)
                      BBUFS(L+5) = V(1,NOD)
                      BBUFS(L+6) = V(2,NOD)
                      BBUFS(L+7) = V(3,NOD)
                      BBUFS(L+8) = TEMP(NOD)
                      L = L + LEN11T
                    ENDDO
                  ENDIF
                 ENDIF

C type 17
                ELSEIF(NTY.EQ.17)THEN
                  IGE = IPARI(34,NIN)
                  IGN = IPARI(36,NIN)
                  NMES=IGRBRIC(IGN)%NENTITY
                  NME =IGRBRIC(IGE)%NENTITY
C
                  DO I = 1, NB
                    N = NSVSI(NIN)%P(IDEB+I)
                    IES = IGRBRIC(IGN)%ENTITY(N)
                    NOD = IXS(2,IES)
                    BBUFS(L+1) = X(1,NOD)
                    BBUFS(L+2) = X(2,NOD)
                    BBUFS(L+3) = X(3,NOD)
                    BBUFS(L+4) = V(1,NOD)
                    BBUFS(L+5) = V(2,NOD)
                    BBUFS(L+6) = V(3,NOD)
                    NOD = IXS(3,IES)
                    BBUFS(L+7) = X(1,NOD)
                    BBUFS(L+8) = X(2,NOD)
                    BBUFS(L+9) = X(3,NOD)
                    BBUFS(L+10)= V(1,NOD)
                    BBUFS(L+11)= V(2,NOD)
                    BBUFS(L+12)= V(3,NOD)
                    NOD = IXS(4,IES)
                    BBUFS(L+13) = X(1,NOD)
                    BBUFS(L+14) = X(2,NOD)
                    BBUFS(L+15) = X(3,NOD)
                    BBUFS(L+16)= V(1,NOD)
                    BBUFS(L+17)= V(2,NOD)
                    BBUFS(L+18)= V(3,NOD)
                    NOD = IXS(5,IES)
                    BBUFS(L+19)= X(1,NOD)
                    BBUFS(L+20)= X(2,NOD)
                    BBUFS(L+21)= X(3,NOD)
                    BBUFS(L+22)= V(1,NOD)
                    BBUFS(L+23)= V(2,NOD)
                    BBUFS(L+24)= V(3,NOD)
                    NOD = IXS(6,IES)
                    BBUFS(L+25)= X(1,NOD)
                    BBUFS(L+26)= X(2,NOD)
                    BBUFS(L+27)= X(3,NOD)
                    BBUFS(L+28)= V(1,NOD)
                    BBUFS(L+29)= V(2,NOD)
                    BBUFS(L+30)= V(3,NOD)
                    NOD = IXS(7,IES)
                    BBUFS(L+31)= X(1,NOD)
                    BBUFS(L+32)= X(2,NOD)
                    BBUFS(L+33)= X(3,NOD)
                    BBUFS(L+34)= V(1,NOD)
                    BBUFS(L+35)= V(2,NOD)
                    BBUFS(L+36)= V(3,NOD)
                    NOD = IXS(8,IES)
                    BBUFS(L+37)= X(1,NOD)
                    BBUFS(L+38)= X(2,NOD)
                    BBUFS(L+39)= X(3,NOD)
                    BBUFS(L+40)= V(1,NOD)
                    BBUFS(L+41)= V(2,NOD)
                    BBUFS(L+42)= V(3,NOD)
                    NOD = IXS(9,IES)
                    BBUFS(L+43)= X(1,NOD)
                    BBUFS(L+44)= X(2,NOD)
                    BBUFS(L+45)= X(3,NOD)
                    BBUFS(L+46)= V(1,NOD)
                    BBUFS(L+47)= V(2,NOD)
                    BBUFS(L+48)= V(3,NOD)
                    NOD = IXS16(1,IES)
                    BBUFS(L+49)= X(1,NOD)
                    BBUFS(L+50)= X(2,NOD)
                    BBUFS(L+51)= X(3,NOD)
                    BBUFS(L+52)= V(1,NOD)
                    BBUFS(L+53)= V(2,NOD)
                    BBUFS(L+54)= V(3,NOD)
                    NOD = IXS16(2,IES)
                    BBUFS(L+55)= X(1,NOD)
                    BBUFS(L+56)= X(2,NOD)
                    BBUFS(L+57)= X(3,NOD)
                    BBUFS(L+58)= V(1,NOD)
                    BBUFS(L+59)= V(2,NOD)
                    BBUFS(L+60)= V(3,NOD)
                    NOD = IXS16(3,IES)
                    BBUFS(L+61)= X(1,NOD)
                    BBUFS(L+62)= X(2,NOD)
                    BBUFS(L+63)= X(3,NOD)
                    BBUFS(L+64)= V(1,NOD)
                    BBUFS(L+65)= V(2,NOD)
                    BBUFS(L+66)= V(3,NOD)
                    NOD = IXS16(4,IES)
                    BBUFS(L+67)= X(1,NOD)
                    BBUFS(L+68)= X(2,NOD)
                    BBUFS(L+69)= X(3,NOD)
                    BBUFS(L+70)= V(1,NOD)
                    BBUFS(L+71)= V(2,NOD)
                    BBUFS(L+72)= V(3,NOD)
                    NOD = IXS16(5,IES)
                    BBUFS(L+73)= X(1,NOD)
                    BBUFS(L+74)= X(2,NOD)
                    BBUFS(L+75)= X(3,NOD)
                    BBUFS(L+76)= V(1,NOD)
                    BBUFS(L+77)= V(2,NOD)
                    BBUFS(L+78)= V(3,NOD)
                    NOD = IXS16(6,IES)
                    BBUFS(L+79)= X(1,NOD)
                    BBUFS(L+80)= X(2,NOD)
                    BBUFS(L+81)= X(3,NOD)
                    BBUFS(L+82)= V(1,NOD)
                    BBUFS(L+83)= V(2,NOD)
                    BBUFS(L+84)= V(3,NOD)
                    NOD = IXS16(7,IES)
                    BBUFS(L+85)= X(1,NOD)
                    BBUFS(L+86)= X(2,NOD)
                    BBUFS(L+87)= X(3,NOD)
                    BBUFS(L+88)= V(1,NOD)
                    BBUFS(L+89)= V(2,NOD)
                    BBUFS(L+90)= V(3,NOD)
                    NOD = IXS16(8,IES)
                    BBUFS(L+91)= X(1,NOD)
                    BBUFS(L+92)= X(2,NOD)
                    BBUFS(L+93)= X(3,NOD)
                    BBUFS(L+94)= V(1,NOD)
                    BBUFS(L+95)= V(2,NOD)
                    BBUFS(L+96)= V(3,NOD)
C FROTS(1..4)
                    BBUFS(L+97) = INTBUF_TAB(NIN)%FROTS((N-1)*7+1)
                    BBUFS(L+98) = INTBUF_TAB(NIN)%FROTS((N-1)*7+2)
                    BBUFS(L+99) = INTBUF_TAB(NIN)%FROTS((N-1)*7+3)
                    BBUFS(L+100)= INTBUF_TAB(NIN)%FROTS((N-1)*7+4)
C
                    L = L + LEN17
                  ENDDO
C fin type 17
                ELSEIF(NTY.EQ.20 .AND. INTTH == 0 )THEN
                 NLN = IPARI(35,NIN)
                 IF(DT_CST.NE.0)THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN

C besoin de tjrs envoyer X
C                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1) THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+8)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+10) = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+17) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)
C
C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+18),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 8) = ZERO
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+18),IRESP,INC)
                      ENDIF
C                      L  = L + INC
                      L = L + LEN20
                    ENDDO
C AMS
                   ELSE
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+8)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+10) = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)

C Alphak(2)
                        BBUFS(L+17) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)
C
C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+18),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 8) = ZERO
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO

                        CALL PUTDPZERO(ZERODP,BBUFS(L+18),IRESP,INC)
                      ENDIF
C                     L  = L + INC
                      BBUFS(L+18+INC) = DIAG_SMS(NOD)
                      L = L + LEN20
                    ENDDO
                   END IF
C fin AMS
                 ELSE
C                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1) THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+7)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+8)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+9) = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+10)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+16) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)

C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+17),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 7) = ZERO
                        BBUFS(L+ 8) = ZERO
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+17),IRESP,INC)
                      ENDIF
C                      L  = L + INC
                      L = L + LEN20
                    ENDDO
C AMS
                   ELSE
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+7)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+8)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+10)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+16) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)

C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+17),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 7) = ZERO
                        BBUFS(L+ 8) = ZERO
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+17),IRESP,INC)
                      ENDIF
C                     L  = L + INC
                      BBUFS(L+17+INC) = DIAG_SMS(NOD)
                      L = L + LEN20
                    ENDDO
                   END IF
C fin AMS
                 ENDIF
C
                ELSEIF( NTY == 20 .AND.  INTTH > 0 ) THEN
                 IF(DT_CST.NE.0)THEN
C                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1) THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
                      BBUFS(L+8) = TEMP(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+10)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+11) = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)

C DAANC
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+17)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+18) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)
C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+19),IRESP,
     .                    INC)
                      ELSE
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        BBUFS(L+18) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+19),IRESP,INC)
                      ENDIF
C                      L  = L + INC
                      L = L + LEN20T
                    ENDDO
C AMS
                   ELSE
                    DO I = 1, NB
                      N = NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
                      BBUFS(L+8) = TEMP(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+10)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+17)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+18) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)
C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+19),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        BBUFS(L+18) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+19),IRESP,INC)
                      ENDIF
C                     L  = L + INC
                      BBUFS(L+19+INC) = DIAG_SMS(NOD)
                      L = L + LEN20T
                    ENDDO
                   END IF
C fin AMS
                 ELSE
C                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1) THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = TEMP(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+8)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+10) = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+17) = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)
C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+18),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 8) = ZERO
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+18),IRESP,INC)
                      ENDIF
C                      L  = L + INC
                      L = L + LEN20T
                    ENDDO
C AMS
                   ELSE
                    DO I = 1, NB
                      N=NSVSI(NIN)%P(IDEB+I)
c                     IL  = NSV(N)
                      IL = INTBUF_TAB(NIN)%NSV(N)
c                     NOD = NLG(IL)
                      NOD = INTBUF_TAB(NIN)%NLG(IL)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = TEMP(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C DX
                        BBUFS(L+8)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+1)
                        BBUFS(L+9)  = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+2)
                        BBUFS(L+10) = INTBUF_TAB(NIN)%AVX_ANCR((IL-1)*3+3)
C DV
                        BBUFS(L+11)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+1)
                        BBUFS(L+12)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+2)
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL-1)*3+3)
C DAANC
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+1)
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+2)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL-1)*3+3)
C Alphak(2)
                        BBUFS(L+17)  = INTBUF_TAB(NIN)%ALPHAK((IL-1)*3+2)
C DA
C adresse dans INBUF entier => buffer double precision de taille 18*2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL-1)*3*6*2),BBUFS(L+18),IRESP,
     .                         INC)
                      ELSE
                        BBUFS(L+ 8) = ZERO
                        BBUFS(L+ 9) = ZERO
                        BBUFS(L+10) = ZERO
                        BBUFS(L+11) = ZERO
                        BBUFS(L+12) = ZERO
                        BBUFS(L+13) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+18),IRESP,INC)
                      ENDIF
C                     L  = L + INC
                      BBUFS(L+18+INC) = DIAG_SMS(NOD)
                      L = L + LEN20T
                    ENDDO
                   END IF
C fin AMS

                ENDIF
C
                END IF
                DEBUT(NIN) = DEBUT(NIN) + NB
              ENDIF
C
C Extra Code for Type20 Edge
C
              IF(IPARI(7,NIN).EQ.20) THEN
                NB = NSNSIE(NIN)%P(P)
                IDEB = DEBUTE(NIN)
                IF(NB.GT.0) THEN
C besoin si partie non edge pas traite
                  NTY   =IPARI(7,NIN)
                  NLN = IPARI(35,NIN)
                  IF(DT_CST.NE.0)THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N = NSVSIE(NIN)%P(IDEB+I)
c                     IL1 = ISLINS(1,N)
                      IL1 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+1)
c                     NOD = NLG(IL1)
                      NOD = INTBUF_TAB(NIN)%NLG(IL1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
c                     IL2 = ISLINS(1,N)
                      IL2 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+2)
c                     NOD = NLG(IL2)
                      NOD = INTBUF_TAB(NIN)%NLG(IL2)
                      BBUFS(L+8) = X(1,NOD)
                      BBUFS(L+9) = X(2,NOD)
                      BBUFS(L+10) = X(3,NOD)
                      BBUFS(L+11) = V(1,NOD)
                      BBUFS(L+12) = V(2,NOD)
                      BBUFS(L+13) = V(3,NOD)
                      BBUFS(L+14) = MS(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C
C DX IL1
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+1)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+2)
                        BBUFS(L+17) = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+3)
C DV IL1
                        BBUFS(L+18)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+1)
                        BBUFS(L+19)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+2)
                        BBUFS(L+20)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+3)
C DX IL2
                        BBUFS(L+21)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+1)
                        BBUFS(L+22)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+2)
                        BBUFS(L+23)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+3)
C DV IL2
                        BBUFS(L+24)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+1)
                        BBUFS(L+25)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+2)
                        BBUFS(L+26)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+3)
C DAANC IL1
                        BBUFS(L+27)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+1)
                        BBUFS(L+28)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+2)
                        BBUFS(L+29)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+3)
C DAANC IL2
                        BBUFS(L+30)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+1)
                        BBUFS(L+31)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+2)
                        BBUFS(L+32)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+3)
C Alphak(2) IL1
                        BBUFS(L+33)  = INTBUF_TAB(NIN)%ALPHAK((IL1-1)*3+2)
C Alphak(2) IL2
                        BBUFS(L+34) = INTBUF_TAB(NIN)%ALPHAK((IL2-1)*3+2)
C DA IL1
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL1-1)*3*6*2),BBUFS(L+35),IRESP,
     .                         INC)
C                       L  = L + INC
C DA IL2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL2-1)*3*6*2),BBUFS(L+35),IRESP,
     .                         INC)
C                       L  = L + INC
                      ELSE
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        BBUFS(L+18) = ZERO
                        BBUFS(L+19) = ZERO
                        BBUFS(L+20) = ZERO
                        BBUFS(L+21) = ZERO
                        BBUFS(L+22) = ZERO
                        BBUFS(L+23) = ZERO
                        BBUFS(L+24) = ZERO
                        BBUFS(L+25) = ZERO
                        BBUFS(L+26) = ZERO
                        BBUFS(L+27) = ZERO
                        BBUFS(L+28) = ZERO
                        BBUFS(L+29) = ZERO
                        BBUFS(L+30) = ZERO
                        BBUFS(L+31) = ZERO
                        BBUFS(L+32) = ZERO
                        BBUFS(L+33) = ZERO
                        BBUFS(L+34) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+35),IRESP,INC)
                        CALL PUTDPZERO(ZERODP,BBUFS(L+35+INC),IRESP,INC)
                      ENDIF
                      L = L + LEN20E
                    ENDDO
C AMS

                   ELSE
                    DO I = 1, NB
                      N = NSVSIE(NIN)%P(IDEB+I)
c                     IL1 = ISLINS(1,N)
                      IL1 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+1)
c                     NOD = NLG(IL1)
                      NOD = INTBUF_TAB(NIN)%NLG(IL1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
c                     IL2 = ISLINS(1,N)
                      IL2 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+2)
c                     NOD = NLG(IL2)
                      NOD = INTBUF_TAB(NIN)%NLG(IL2)
                      BBUFS(L+8) = X(1,NOD)
                      BBUFS(L+9) = X(2,NOD)
                      BBUFS(L+10) = X(3,NOD)
                      BBUFS(L+11) = V(1,NOD)
                      BBUFS(L+12) = V(2,NOD)
                      BBUFS(L+13) = V(3,NOD)
                      BBUFS(L+14) = MS(NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN
C
C DX IL1
                        BBUFS(L+15)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+1)
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+2)
                        BBUFS(L+17) = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+3)
C DV IL1
                        BBUFS(L+18)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+1)
                        BBUFS(L+19)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+2)
                        BBUFS(L+20)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+3)
C DX IL2
                        BBUFS(L+21)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+1)
                        BBUFS(L+22)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+2)
                        BBUFS(L+23)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+3)
C DV IL2
                        BBUFS(L+24)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+1)
                        BBUFS(L+25)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+2)
                        BBUFS(L+26)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+3)
C DAANC IL1
                        BBUFS(L+27)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+1)
                        BBUFS(L+28)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+2)
                        BBUFS(L+29)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+3)
C DAANC IL2
                        BBUFS(L+30)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+1)
                        BBUFS(L+31)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+2)
                        BBUFS(L+32)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+3)
C Alphak(2) IL1
                        BBUFS(L+33) = INTBUF_TAB(NIN)%ALPHAK((IL1-1)*3+2)
C Alphak(2) IL2
                        BBUFS(L+34) = INTBUF_TAB(NIN)%ALPHAK((IL2-1)*3+2)
C DA IL1
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL1-1)*3*6*2),BBUFS(L+35),IRESP,
     .                         INC)
C                       L  = L + INC
C DA IL2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL2-1)*3*6*2),BBUFS(L+35+INC),IRESP,
     .                         INC)
C                       L  = L + INC
                      ELSE
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        BBUFS(L+18) = ZERO
                        BBUFS(L+19) = ZERO
                        BBUFS(L+20) = ZERO
                        BBUFS(L+21) = ZERO
                        BBUFS(L+22) = ZERO
                        BBUFS(L+23) = ZERO
                        BBUFS(L+24) = ZERO
                        BBUFS(L+25) = ZERO
                        BBUFS(L+26) = ZERO
                        BBUFS(L+27) = ZERO
                        BBUFS(L+28) = ZERO
                        BBUFS(L+29) = ZERO
                        BBUFS(L+30) = ZERO
                        BBUFS(L+31) = ZERO
                        BBUFS(L+32) = ZERO
                        BBUFS(L+33) = ZERO
                        BBUFS(L+34) = ZERO

                        CALL PUTDPZERO(ZERODP,BBUFS(L+35),IRESP,INC)
                        CALL PUTDPZERO(ZERODP,BBUFS(L+35+INC),IRESP,INC)
                      ENDIF
                      NOD = INTBUF_TAB(NIN)%NLG(IL1)
                      BBUFS(L+35+2*INC) = DIAG_SMS(NOD)
                      NOD = INTBUF_TAB(NIN)%NLG(IL2)
                      BBUFS(L+36+2*INC) = DIAG_SMS(NOD)
                      L = L + LEN20E
                    ENDDO
                   END IF
C fin AMS

                  ELSE
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N=NSVSIE(NIN)%P(IDEB+I)
c                     IL1  = ISLINS(1,N)
                      IL1 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+1)
c                     NOD = NLG(IL1)
                      NOD = INTBUF_TAB(NIN)%NLG(IL1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
c                     IL2  = ISLINS(1,N)
                      IL2 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+2)
c                     NOD = NLG(IL2)
                      NOD = INTBUF_TAB(NIN)%NLG(IL2)
                      BBUFS(L+7) = X(1,NOD)
                      BBUFS(L+8) = X(2,NOD)
                      BBUFS(L+9) = X(3,NOD)
                      BBUFS(L+10) = V(1,NOD)
                      BBUFS(L+11) = V(2,NOD)
                      BBUFS(L+12) = V(3,NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN

C DX IL1
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+1)
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+2)
                        BBUFS(L+15) = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+3)
C DV IL1
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+1)
                        BBUFS(L+17)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+2)
                        BBUFS(L+18)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+3)
C DX IL2
                        BBUFS(L+19)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+1)
                        BBUFS(L+20)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+2)
                        BBUFS(L+21)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+3)
C DV IL2
                        BBUFS(L+22)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+1)
                        BBUFS(L+23)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+2)
                        BBUFS(L+24)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+3)
C DAANC IL11
                        BBUFS(L+25)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+1)
                        BBUFS(L+26)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+2)
                        BBUFS(L+27)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+3)
C DAANC IL2
                        BBUFS(L+28)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+1)
                        BBUFS(L+29)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+2)
                        BBUFS(L+30)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+3)
C Alphak(2) IL1
                        BBUFS(L+31) = INTBUF_TAB(NIN)%ALPHAK((IL1-1)*3+2)
C Alphak(2) IL2
                        BBUFS(L+32) = INTBUF_TAB(NIN)%ALPHAK((IL2-1)*3+2)
C DA IL1
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL1-1)*3*6*2),BBUFS(L+33),IRESP,
     .                         INC)
C                      L  = L + INC
C DA IL2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL2-1)*3*6*2),BBUFS(L+33+INC),IRESP,
     .                         INC)
C                      L  = L + INC
                      ELSE
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        BBUFS(L+18) = ZERO
                        BBUFS(L+19) = ZERO
                        BBUFS(L+20) = ZERO
                        BBUFS(L+21) = ZERO
                        BBUFS(L+22) = ZERO
                        BBUFS(L+23) = ZERO
                        BBUFS(L+24) = ZERO
                        BBUFS(L+25) = ZERO
                        BBUFS(L+26) = ZERO
                        BBUFS(L+27) = ZERO
                        BBUFS(L+28) = ZERO
                        BBUFS(L+29) = ZERO
                        BBUFS(L+30) = ZERO
                        BBUFS(L+31) = ZERO
                        BBUFS(L+32) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+33),IRESP,INC)
                        CALL PUTDPZERO(ZERODP,BBUFS(L+33+INC),IRESP,INC)
                      ENDIF

                      L = L + LEN20E
                    ENDDO
C AMS

                   ELSE
                    DO I = 1, NB
                      N=NSVSIE(NIN)%P(IDEB+I)
c                     IL1  = ISLINS(1,N)
                      IL1 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+1)
c                     NOD = NLG(IL1)
                      NOD = INTBUF_TAB(NIN)%NLG(IL1)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
c                     IL2  = ISLINS(1,N)
                      IL2 = INTBUF_TAB(NIN)%IXLINS(2*(N-1)+2)
c                     NOD = NLG(IL2)
                      NOD = INTBUF_TAB(NIN)%NLG(IL2)
                      BBUFS(L+7) = X(1,NOD)
                      BBUFS(L+8) = X(2,NOD)
                      BBUFS(L+9) = X(3,NOD)
                      BBUFS(L+10) = V(1,NOD)
                      BBUFS(L+11) = V(2,NOD)
                      BBUFS(L+12) = V(3,NOD)

                      IRBY = IKINE(NOD) - (IKINE(NOD)/2)*2
                      IF(IRBY==0)THEN

C DX IL1
                        BBUFS(L+13)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+1)
                        BBUFS(L+14)  = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+2)
                        BBUFS(L+15) = INTBUF_TAB(NIN)%AVX_ANCR((IL1-1)*3+3)
C DV IL1
                        BBUFS(L+16)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+1)
                        BBUFS(L+17)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+2)
                        BBUFS(L+18)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL1-1)*3+3)
C DX IL2
                        BBUFS(L+19)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+1)
                        BBUFS(L+20)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+2)
                        BBUFS(L+21)  = INTBUF_TAB(NIN)%AVX_ANCR((IL2-1)*3+3)
C DV IL2
                        BBUFS(L+22)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+1)
                        BBUFS(L+23)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+2)
                        BBUFS(L+24)  = INTBUF_TAB(NIN)%AVX_ANCR(3*NLN + (IL2-1)*3+3)
C DAANC IL1
                        BBUFS(L+25)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+1)
                        BBUFS(L+26)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+2)
                        BBUFS(L+27)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL1-1)*3+3)
C DAANC IL2
                        BBUFS(L+28)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+1)
                        BBUFS(L+29)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+2)
                        BBUFS(L+30)  = INTBUF_TAB(NIN)%AVX_ANCR(6*NLN + (IL2-1)*3+3)
C Alphak(2) IL1
                        BBUFS(L+31) = INTBUF_TAB(NIN)%ALPHAK((IL1-1)*3+2)
C Alphak(2) IL2
                        BBUFS(L+32) = INTBUF_TAB(NIN)%ALPHAK((IL2-1)*3+2)
C DA IL1
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL1-1)*3*6*2),BBUFS(L+33),IRESP,
     .                         INC)
C                      L  = L + INC
C DA IL2
                        CALL PUTDPDAANC(
     .                    INTBUF_TAB(NIN)%DAANC6(1+(IL2-1)*3*6*2),BBUFS(L+33+INC),IRESP,
     .                         INC)
C                      L  = L + INC
                      ELSE
                        BBUFS(L+13) = ZERO
                        BBUFS(L+14) = ZERO
                        BBUFS(L+15) = ZERO
                        BBUFS(L+16) = ZERO
                        BBUFS(L+17) = ZERO
                        BBUFS(L+18) = ZERO
                        BBUFS(L+19) = ZERO
                        BBUFS(L+20) = ZERO
                        BBUFS(L+21) = ZERO
                        BBUFS(L+22) = ZERO
                        BBUFS(L+23) = ZERO
                        BBUFS(L+24) = ZERO
                        BBUFS(L+25) = ZERO
                        BBUFS(L+26) = ZERO
                        BBUFS(L+27) = ZERO
                        BBUFS(L+28) = ZERO
                        BBUFS(L+29) = ZERO
                        BBUFS(L+30) = ZERO
                        BBUFS(L+31) = ZERO
                        BBUFS(L+32) = ZERO
                        CALL PUTDPZERO(ZERODP,BBUFS(L+33),IRESP,INC)
                        CALL PUTDPZERO(ZERODP,BBUFS(L+33+INC),IRESP,INC)
                      ENDIF
                      NOD = INTBUF_TAB(NIN)%NLG(IL1)
                      BBUFS(L+33+2*INC) = DIAG_SMS(NOD)
                      NOD = INTBUF_TAB(NIN)%NLG(IL2)
                      BBUFS(L+34+2*INC) = DIAG_SMS(NOD)
                      L = L + LEN20E
                    ENDDO
C fin AMS
                   ENDIF

                  ENDIF
                  DEBUTE(NIN) = DEBUTE(NIN) + NB
                END IF
              END IF    !  end i20 edge

C
C Extra Code for Type25 Edge
C
              IEDGE = IPARI(58,NIN)
              IF(IPARI(7,NIN).EQ.25 .AND. IEDGE .NE. 0) THEN
                NB = NSNSIE(NIN)%P(P)
                IDEB = DEBUTE(NIN)
                IF(NB.GT.0) THEN
                    DO I = 1, NB
                      N = NSVSIE(NIN)%P(IDEB+I)
                      ASSERT(N > 0)
C                     PRINTIF((N-1)*NLEDGE < 0,N)
C                     PRINTIF(N > IPARI(68,NIN),N)
                      NOD = INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+5)
                      BBUFS(L+1) = X(1,NOD)
                      BBUFS(L+2) = X(2,NOD)
                      BBUFS(L+3) = X(3,NOD)
                      BBUFS(L+4) = V(1,NOD)
                      BBUFS(L+5) = V(2,NOD)
                      BBUFS(L+6) = V(3,NOD)
                      BBUFS(L+7) = MS(NOD)
                      NOD = INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+6)
                      BBUFS(L+8) = X(1,NOD)
                      BBUFS(L+9) = X(2,NOD)
                      BBUFS(L+10) = X(3,NOD)
                      BBUFS(L+11) = V(1,NOD)
                      BBUFS(L+12) = V(2,NOD)
                      BBUFS(L+13) = V(3,NOD)
                      BBUFS(L+14) = MS(NOD)

c                     IAM=INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+1)
c                     JAM=INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+2)
c                     M1 =INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+5)
c                     M2 =INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+6)

c                     IF(INTBUF_TAB(NIN)%IRECTM(JAM+(IAM-1)*4)==M1
c    .                 .AND.INTBUF_TAB(NIN)%IRECTM(MOD(JAM,4)+1+(IAM-1)*4)==M2)THEN
c                       BBUFS(L+15) = UN 
c                     ELSEIF(INTBUF_TAB(NIN)%IRECTM(JAM+(IAM-1)*4)==M2
c    .                  .AND.INTBUF_TAB(NIN)%IRECTM(MOD(JAM,4)+1+(IAM-1)*4)==M1)THEN
c                       BBUFS(L+15) = - UN
c                     ELSE
c                       ASSERT(.FALSE.)
c                     ENDIF

CC C verifier la portabilite:
CC C LONG = TRANSFER(court) => les deriners bits de LONG pas specifies dans
CC C la norme
CC                       BBUFS(L+16) = 0
CC                       BBUFS(L+17) = 0
CC                       BBUFS(L+18) = 0
CC                       BBUFS(L+19) = 0
CC                       BBUFS(L+16) = TRANSFER(INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+1),UN)
CC                       BBUFS(L+17) = TRANSFER(INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+2),UN)
CC                       BBUFS(L+18) = TRANSFER(INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+3),UN)
CC                       BBUFS(L+19) = TRANSFER(INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+4),UN)

C                     WRITE(6,"(A,4I10)") "--",INTBUF_TAB(NIN)%LEDGE((N-1)*NLEDGE+1:(N-1)*NLEDGE+4)


C                     BBUFS(L+15) = INTBUF_TAB(NIN)%EDGE_BISECTOR(TO1D(1,JE,IE,3,4))
C                     BBUFS(L+16) = INTBUF_TAB(NIN)%EDGE_BISECTOR(TO1D(2,JE,IE,3,4))
C                     BBUFS(L+17) = INTBUF_TAB(NIN)%EDGE_BISECTOR(TO1D(3,JE,IE,3,4))
C                     BBUFS(L+15) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(1,1,I1,3,2))
C                     BBUFS(L+16) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(2,1,I1,3,2))
C                     BBUFS(L+17) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(3,1,I1,3,2))
C                     BBUFS(L+18) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(1,2,I1,3,2))
C                     BBUFS(L+19) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(2,2,I1,3,2))
C                     BBUFS(L+20) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(3,2,I1,3,2))
C                     BBUFS(L+21) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(1,1,I2,3,2))
C                     BBUFS(L+22) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(2,1,I2,3,2))
C                     BBUFS(L+23) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(3,1,I2,3,2))
C                     BBUFS(L+24) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(1,2,I2,3,2))
C                     BBUFS(L+25) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(2,2,I2,3,2))
C                     BBUFS(L+26) = INTBUF_TAB(NIN)%VTX_BISECTOR(TO1D(3,2,I2,3,2))

C                     BBUFS(L+27) = 1 ! master of the edge

                      L = L + LEN25E
                    ENDDO
                    DEBUTE(NIN) = DEBUTE(NIN) + NB
                 ENDIF
               ENDIF
            ENDDO
            SIZ = L+1-ADD
            IF(SIZ.GT.0)THEN
              MSGTYP = MSGOFF
C BUFR : reception partie force et envoi partie vitesse
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
      ELSEIF(IFLAG.EQ.2.OR.IFLAG.EQ.3)THEN
       IF(ISTOCOM.EQ.1)THEN
C
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUT(NIN) = 0
          DEBUTE(NIN) = 0
        ENDDO
C
C Attente IRECV
C
        DO P = 1, NSPMD
          IF(ADDR(P+1)-ADDR(P).GT.0) THEN
            CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
            L = ADDR(P)-1
C            DO NIN = 1, NINTER
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NB = NSNFI(NIN)%P(P)
              LSAV=L
              IF(NB.GT.0)THEN
                IDEB = DEBUT(NIN)
                NTY =IPARI(7,NIN)
                INTTH = IPARI(47,NIN)
                INTNITSCHE = IPARI(86,NIN)
                IF((NTY.EQ.7.AND.INTTH == 0).OR.NTY.EQ.10.OR.
     .             (NTY.EQ.22.AND.INTTH == 0).OR.
     .             (NTY.EQ.23.AND.INTTH == 0).OR.
     .             (NTY.EQ.24.AND.INTTH == 0.AND.INTNITSCHE == 0).OR.
     .             (NTY.EQ.25.AND.INTTH == 0)  )THEN
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                      L = L + LEN
                    ENDDO
                    POINT=7
                  ELSE
                    DO I = 1, NB
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      MSFI(NIN)%P(I+IDEB)  = BBUFR(L+4)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB) +VDT
                      L = L + LEN
                    ENDDO
                    POINT=4
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      L = L + LEN
                    ENDDO
                    POINT=6
                  ELSE
                    DO I = 1, NB
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB) +VDT
                      L = L + LEN
                    ENDDO
                    POINT=3
                  ENDIF
                 ENDIF
C
                ELSEIF((NTY ==7  .AND. INTTH > 0 ).OR.
     .                 (NTY ==25 .AND. INTTH > 0 ).OR.
     .                 (NTY ==24 .AND. INTTH > 0 .AND.INTNITSCHE == 0).OR.
     .                 (NTY ==23 .AND. INTTH > 0 ).OR.
     .                 (NTY ==22 .AND. INTTH > 0 )) THEN
                   IF(DT_CST.NE.0)THEN
                    IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+8)
                        L = L + LEN7T
                      ENDDO
                      POINT=8
                    ELSE
                      DO I = 1, NB
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        MSFI(NIN)%P(I+IDEB)  = BBUFR(L+4)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+5)
                        VDT = DT2PREV*BBUFR(L+1)
                        XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB) +VDT
                        VDT = DT2PREV*BBUFR(L+2)
                        XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB) +VDT
                        VDT = DT2PREV*BBUFR(L+3)
                        XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB) +VDT
                        L = L + LEN7T
                      ENDDO
                      POINT=5
                    ENDIF
                   ELSE
                    IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+7)
                        L = L + LEN7T
                      ENDDO
                      POINT=7
                    ELSE
                      DO I = 1, NB
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+4)
                        VDT = DT2PREV*BBUFR(L+1)
                        XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB)+VDT
                        VDT = DT2PREV*BBUFR(L+2)
                        XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB)+VDT
                        VDT = DT2PREV*BBUFR(L+3)
                        XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB)+VDT
                        L = L + LEN7T
                      ENDDO
                      POINT=4
                    ENDIF
                   ENDIF



C--- Nitsche
                ELSEIF(NTY.EQ.24.AND.INTTH == 0.AND.INTNITSCHE > 0)THEN
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                      FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+8)
                      FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+9)
                      FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+10)
                      L = L + LEN !+ 3
                    ENDDO
                    POINT=7
                  ELSE
                    DO I = 1, NB
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      MSFI(NIN)%P(I+IDEB)  = BBUFR(L+4)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB) +VDT
                      FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+5)
                      FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+6)
                      FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+7)
                      L = L + LEN 
                    ENDDO
                    POINT=6
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0.OR.INT24E2EUSE==1) THEN
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+7)
                      FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+8)
                      FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+9)
                      L = L + LEN 
                    ENDDO
                    POINT=9
                  ELSE
                    DO I = 1, NB
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB) +VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB) +VDT
                      FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      L = L + LEN 
                    ENDDO
                    POINT=6
                  ENDIF
                 ENDIF
C
                ELSEIF(NTY.EQ.24.AND.INTTH > 0.AND.INTNITSCHE > 0) THEN
                   IF(DT_CST.NE.0)THEN
                    IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+8)
                        FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+9)
                        FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+10)
                        FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+11)
                        L = L + LEN7T 
                      ENDDO
                      POINT=11
                    ELSE
                      DO I = 1, NB
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        MSFI(NIN)%P(I+IDEB)  = BBUFR(L+4)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+5)
                        VDT = DT2PREV*BBUFR(L+1)
                        XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB) +VDT
                        VDT = DT2PREV*BBUFR(L+2)
                        XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB) +VDT
                        VDT = DT2PREV*BBUFR(L+3)
                        XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB) +VDT
                        FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+6)
                        FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+7)
                        FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+8)
                        L = L + LEN7T 
                      ENDDO
                      POINT=8
                    ENDIF
                   ELSE
                    IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+7)
                        FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+8)
                        FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+9)
                        FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+10)
                        L = L + LEN7T 
                      ENDDO
                      POINT=10
                    ELSE
                      DO I = 1, NB
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+4)
                        VDT = DT2PREV*BBUFR(L+1)
                        XFI(NIN)%P(1,I+IDEB) = XFI(NIN)%P(1,I+IDEB)+VDT
                        VDT = DT2PREV*BBUFR(L+2)
                        XFI(NIN)%P(2,I+IDEB) = XFI(NIN)%P(2,I+IDEB)+VDT
                        VDT = DT2PREV*BBUFR(L+3)
                        XFI(NIN)%P(3,I+IDEB) = XFI(NIN)%P(3,I+IDEB)+VDT
                        FORNEQSFI(NIN)%P(1,I+IDEB) = BBUFR(L+5)
                        FORNEQSFI(NIN)%P(2,I+IDEB) = BBUFR(L+6)
                        FORNEQSFI(NIN)%P(3,I+IDEB) = BBUFR(L+7)
                        L = L + LEN7T 
                      ENDDO
                      POINT=7
                    ENDIF
                   ENDIF



C----
                ELSEIF(NTY.EQ.11.AND.INTTH == 0)THEN
C type 11
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFI(NIN)%P(1,N1) = BBUFR(L+1)
                      XFI(NIN)%P(2,N1) = BBUFR(L+2)
                      XFI(NIN)%P(3,N1) = BBUFR(L+3)
                      VFI(NIN)%P(1,N1) = BBUFR(L+4)
                      VFI(NIN)%P(2,N1) = BBUFR(L+5)
                      VFI(NIN)%P(3,N1) = BBUFR(L+6)
                      MSFI(NIN)%P(N1)  = BBUFR(L+7)
                      XFI(NIN)%P(1,N2) = BBUFR(L+8)
                      XFI(NIN)%P(2,N2) = BBUFR(L+9)
                      XFI(NIN)%P(3,N2) = BBUFR(L+10)
                      VFI(NIN)%P(1,N2) = BBUFR(L+11)
                      VFI(NIN)%P(2,N2) = BBUFR(L+12)
                      VFI(NIN)%P(3,N2) = BBUFR(L+13)
                      MSFI(NIN)%P(N2)  = BBUFR(L+14)
                      L = L + LEN11
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      VFI(NIN)%P(1,N1) = BBUFR(L+1)
                      VFI(NIN)%P(2,N1) = BBUFR(L+2)
                      VFI(NIN)%P(3,N1) = BBUFR(L+3)
                      MSFI(NIN)%P(N1)  = BBUFR(L+4)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,N1) = XFI(NIN)%P(1,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,N1) = XFI(NIN)%P(2,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,N1) = XFI(NIN)%P(3,N1) + VDT
                      VFI(NIN)%P(1,N2) = BBUFR(L+5)
                      VFI(NIN)%P(2,N2) = BBUFR(L+6)
                      VFI(NIN)%P(3,N2) = BBUFR(L+7)
                      MSFI(NIN)%P(N2)  = BBUFR(L+8)
                      VDT = DT2PREV*BBUFR(L+5)
                      XFI(NIN)%P(1,N2) = XFI(NIN)%P(1,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+6)
                      XFI(NIN)%P(2,N2) = XFI(NIN)%P(2,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+7)
                      XFI(NIN)%P(3,N2) = XFI(NIN)%P(3,N2) + VDT
                      L = L + LEN11
                    ENDDO
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFI(NIN)%P(1,N1) = BBUFR(L+1)
                      XFI(NIN)%P(2,N1) = BBUFR(L+2)
                      XFI(NIN)%P(3,N1) = BBUFR(L+3)
                      VFI(NIN)%P(1,N1) = BBUFR(L+4)
                      VFI(NIN)%P(2,N1) = BBUFR(L+5)
                      VFI(NIN)%P(3,N1) = BBUFR(L+6)
                      XFI(NIN)%P(1,N2) = BBUFR(L+7)
                      XFI(NIN)%P(2,N2) = BBUFR(L+8)
                      XFI(NIN)%P(3,N2) = BBUFR(L+9)
                      VFI(NIN)%P(1,N2) = BBUFR(L+10)
                      VFI(NIN)%P(2,N2) = BBUFR(L+11)
                      VFI(NIN)%P(3,N2) = BBUFR(L+12)
                      L = L + LEN11
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      VFI(NIN)%P(1,N1) = BBUFR(L+1)
                      VFI(NIN)%P(2,N1) = BBUFR(L+2)
                      VFI(NIN)%P(3,N1) = BBUFR(L+3)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,N1) = XFI(NIN)%P(1,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,N1) = XFI(NIN)%P(2,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,N1) = XFI(NIN)%P(3,N1) + VDT
                      VFI(NIN)%P(1,N2) = BBUFR(L+4)
                      VFI(NIN)%P(2,N2) = BBUFR(L+5)
                      VFI(NIN)%P(3,N2) = BBUFR(L+6)
                      VDT = DT2PREV*BBUFR(L+4)
                      XFI(NIN)%P(1,N2) = XFI(NIN)%P(1,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+5)
                      XFI(NIN)%P(2,N2) = XFI(NIN)%P(2,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+6)
                      XFI(NIN)%P(3,N2) = XFI(NIN)%P(3,N2) + VDT
                      L = L + LEN11
                    ENDDO
                  ENDIF
                 ENDIF
C type 11+ thermal modelling
                ELSEIF(NTY.EQ.11.AND.INTTH > 0)THEN
                 IF(DT_CST.NE.0)THEN
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFI(NIN)%P(1,N1) = BBUFR(L+1)
                      XFI(NIN)%P(2,N1) = BBUFR(L+2)
                      XFI(NIN)%P(3,N1) = BBUFR(L+3)
                      VFI(NIN)%P(1,N1) = BBUFR(L+4)
                      VFI(NIN)%P(2,N1) = BBUFR(L+5)
                      VFI(NIN)%P(3,N1) = BBUFR(L+6)
                      MSFI(NIN)%P(N1)  = BBUFR(L+7)
                      TEMPFI(NIN)%P(N1)= BBUFR(L+8)
                      XFI(NIN)%P(1,N2) = BBUFR(L+9)
                      XFI(NIN)%P(2,N2) = BBUFR(L+10)
                      XFI(NIN)%P(3,N2) = BBUFR(L+11)
                      VFI(NIN)%P(1,N2) = BBUFR(L+12)
                      VFI(NIN)%P(2,N2) = BBUFR(L+13)
                      VFI(NIN)%P(3,N2) = BBUFR(L+14)
                      MSFI(NIN)%P(N2)  = BBUFR(L+15)
                      TEMPFI(NIN)%P(N2)= BBUFR(L+16)
                      L = L + LEN11T
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      VFI(NIN)%P(1,N1) = BBUFR(L+1)
                      VFI(NIN)%P(2,N1) = BBUFR(L+2)
                      VFI(NIN)%P(3,N1) = BBUFR(L+3)
                      MSFI(NIN)%P(N1)  = BBUFR(L+4)
                      TEMPFI(NIN)%P(N1)= BBUFR(L+5)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,N1) = XFI(NIN)%P(1,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,N1) = XFI(NIN)%P(2,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,N1) = XFI(NIN)%P(3,N1) + VDT
                      VFI(NIN)%P(1,N2) = BBUFR(L+6)
                      VFI(NIN)%P(2,N2) = BBUFR(L+7)
                      VFI(NIN)%P(3,N2) = BBUFR(L+8)
                      MSFI(NIN)%P(N2)  = BBUFR(L+9)
                      TEMPFI(NIN)%P(N2)= BBUFR(L+10)
                      VDT = DT2PREV*BBUFR(L+6)
                      XFI(NIN)%P(1,N2) = XFI(NIN)%P(1,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+7)
                      XFI(NIN)%P(2,N2) = XFI(NIN)%P(2,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+8)
                      XFI(NIN)%P(3,N2) = XFI(NIN)%P(3,N2) + VDT
                      L = L + LEN11T
                    ENDDO
                  ENDIF
                 ELSE
                  IF (IRESP.EQ.1.OR.IMPL_S.EQ.1.OR.NBI18.GT.0) THEN
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFI(NIN)%P(1,N1) = BBUFR(L+1)
                      XFI(NIN)%P(2,N1) = BBUFR(L+2)
                      XFI(NIN)%P(3,N1) = BBUFR(L+3)
                      VFI(NIN)%P(1,N1) = BBUFR(L+4)
                      VFI(NIN)%P(2,N1) = BBUFR(L+5)
                      VFI(NIN)%P(3,N1) = BBUFR(L+6)
                      TEMPFI(NIN)%P(N1)= BBUFR(L+7)
                      XFI(NIN)%P(1,N2) = BBUFR(L+8)
                      XFI(NIN)%P(2,N2) = BBUFR(L+9)
                      XFI(NIN)%P(3,N2) = BBUFR(L+10)
                      VFI(NIN)%P(1,N2) = BBUFR(L+11)
                      VFI(NIN)%P(2,N2) = BBUFR(L+12)
                      VFI(NIN)%P(3,N2) = BBUFR(L+13)
                      TEMPFI(NIN)%P(N1)= BBUFR(L+14)
                      L = L + LEN11T
                    ENDDO
                  ELSE
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      VFI(NIN)%P(1,N1) = BBUFR(L+1)
                      VFI(NIN)%P(2,N1) = BBUFR(L+2)
                      VFI(NIN)%P(3,N1) = BBUFR(L+3)
                      VDT = DT2PREV*BBUFR(L+1)
                      XFI(NIN)%P(1,N1) = XFI(NIN)%P(1,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+2)
                      XFI(NIN)%P(2,N1) = XFI(NIN)%P(2,N1) + VDT
                      VDT = DT2PREV*BBUFR(L+3)
                      XFI(NIN)%P(3,N1) = XFI(NIN)%P(3,N1) + VDT
                      TEMPFI(NIN)%P(N1)= BBUFR(L+4)
                      VFI(NIN)%P(1,N2) = BBUFR(L+5)
                      VFI(NIN)%P(2,N2) = BBUFR(L+6)
                      VFI(NIN)%P(3,N2) = BBUFR(L+7)
                      TEMPFI(NIN)%P(N2)= BBUFR(L+8)
                      VDT = DT2PREV*BBUFR(L+5)
                      XFI(NIN)%P(1,N2) = XFI(NIN)%P(1,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+6)
                      XFI(NIN)%P(2,N2) = XFI(NIN)%P(2,N2) + VDT
                      VDT = DT2PREV*BBUFR(L+7)
                      XFI(NIN)%P(3,N2) = XFI(NIN)%P(3,N2) + VDT
                      L = L + LEN11T
                    ENDDO
                  ENDIF
                 ENDIF


                ELSEIF(NTY.EQ.17)THEN
C type 17
                  DO I = 1, NB
                    NN = I+IDEB
                    XFI17(NIN)%P(1,1,NN) = BBUFR(L+1)
                    XFI17(NIN)%P(2,1,NN) = BBUFR(L+2)
                    XFI17(NIN)%P(3,1,NN) = BBUFR(L+3)
                    VFI17(NIN)%P(1,1,NN) = BBUFR(L+4)
                    VFI17(NIN)%P(2,1,NN) = BBUFR(L+5)
                    VFI17(NIN)%P(3,1,NN) = BBUFR(L+6)
                    XFI17(NIN)%P(1,2,NN) = BBUFR(L+7)
                    XFI17(NIN)%P(2,2,NN) = BBUFR(L+8)
                    XFI17(NIN)%P(3,2,NN) = BBUFR(L+9)
                    VFI17(NIN)%P(1,2,NN) = BBUFR(L+10)
                    VFI17(NIN)%P(2,2,NN) = BBUFR(L+11)
                    VFI17(NIN)%P(3,2,NN) = BBUFR(L+12)
                    XFI17(NIN)%P(1,3,NN) = BBUFR(L+13)
                    XFI17(NIN)%P(2,3,NN) = BBUFR(L+14)
                    XFI17(NIN)%P(3,3,NN) = BBUFR(L+15)
                    VFI17(NIN)%P(1,3,NN) = BBUFR(L+16)
                    VFI17(NIN)%P(2,3,NN) = BBUFR(L+17)
                    VFI17(NIN)%P(3,3,NN) = BBUFR(L+18)
                    XFI17(NIN)%P(1,4,NN) = BBUFR(L+19)
                    XFI17(NIN)%P(2,4,NN) = BBUFR(L+20)
                    XFI17(NIN)%P(3,4,NN) = BBUFR(L+21)
                    VFI17(NIN)%P(1,4,NN) = BBUFR(L+22)
                    VFI17(NIN)%P(2,4,NN) = BBUFR(L+23)
                    VFI17(NIN)%P(3,4,NN) = BBUFR(L+24)
                    XFI17(NIN)%P(1,5,NN) = BBUFR(L+25)
                    XFI17(NIN)%P(2,5,NN) = BBUFR(L+26)
                    XFI17(NIN)%P(3,5,NN) = BBUFR(L+27)
                    VFI17(NIN)%P(1,5,NN) = BBUFR(L+28)
                    VFI17(NIN)%P(2,5,NN) = BBUFR(L+29)
                    VFI17(NIN)%P(3,5,NN) = BBUFR(L+30)
                    XFI17(NIN)%P(1,6,NN) = BBUFR(L+31)
                    XFI17(NIN)%P(2,6,NN) = BBUFR(L+32)
                    XFI17(NIN)%P(3,6,NN) = BBUFR(L+33)
                    VFI17(NIN)%P(1,6,NN) = BBUFR(L+34)
                    VFI17(NIN)%P(2,6,NN) = BBUFR(L+35)
                    VFI17(NIN)%P(3,6,NN) = BBUFR(L+36)
                    XFI17(NIN)%P(1,7,NN) = BBUFR(L+37)
                    XFI17(NIN)%P(2,7,NN) = BBUFR(L+38)
                    XFI17(NIN)%P(3,7,NN) = BBUFR(L+39)
                    VFI17(NIN)%P(1,7,NN) = BBUFR(L+40)
                    VFI17(NIN)%P(2,7,NN) = BBUFR(L+41)
                    VFI17(NIN)%P(3,7,NN) = BBUFR(L+42)
                    XFI17(NIN)%P(1,8,NN) = BBUFR(L+43)
                    XFI17(NIN)%P(2,8,NN) = BBUFR(L+44)
                    XFI17(NIN)%P(3,8,NN) = BBUFR(L+45)
                    VFI17(NIN)%P(1,8,NN) = BBUFR(L+46)
                    VFI17(NIN)%P(2,8,NN) = BBUFR(L+47)
                    VFI17(NIN)%P(3,8,NN) = BBUFR(L+48)
                    XFI17(NIN)%P(1,9,NN) = BBUFR(L+49)
                    XFI17(NIN)%P(2,9,NN) = BBUFR(L+50)
                    XFI17(NIN)%P(3,9,NN) = BBUFR(L+51)
                    VFI17(NIN)%P(1,9,NN) = BBUFR(L+52)
                    VFI17(NIN)%P(2,9,NN) = BBUFR(L+53)
                    VFI17(NIN)%P(3,9,NN) = BBUFR(L+54)
                    XFI17(NIN)%P(1,10,NN) =BBUFR(L+55)
                    XFI17(NIN)%P(2,10,NN) =BBUFR(L+56)
                    XFI17(NIN)%P(3,10,NN) =BBUFR(L+57)
                    VFI17(NIN)%P(1,10,NN) =BBUFR(L+58)
                    VFI17(NIN)%P(2,10,NN) =BBUFR(L+59)
                    VFI17(NIN)%P(3,10,NN) =BBUFR(L+60)
                    XFI17(NIN)%P(1,11,NN) =BBUFR(L+61)
                    XFI17(NIN)%P(2,11,NN) =BBUFR(L+62)
                    XFI17(NIN)%P(3,11,NN) =BBUFR(L+63)
                    VFI17(NIN)%P(1,11,NN) =BBUFR(L+64)
                    VFI17(NIN)%P(2,11,NN) =BBUFR(L+65)
                    VFI17(NIN)%P(3,11,NN) =BBUFR(L+66)
                    XFI17(NIN)%P(1,12,NN) =BBUFR(L+67)
                    XFI17(NIN)%P(2,12,NN) =BBUFR(L+68)
                    XFI17(NIN)%P(3,12,NN) =BBUFR(L+69)
                    VFI17(NIN)%P(1,12,NN) =BBUFR(L+70)
                    VFI17(NIN)%P(2,12,NN) =BBUFR(L+71)
                    VFI17(NIN)%P(3,12,NN) =BBUFR(L+72)
                    XFI17(NIN)%P(1,13,NN) =BBUFR(L+73)
                    XFI17(NIN)%P(2,13,NN) =BBUFR(L+74)
                    XFI17(NIN)%P(3,13,NN) =BBUFR(L+75)
                    VFI17(NIN)%P(1,13,NN) =BBUFR(L+76)
                    VFI17(NIN)%P(2,13,NN) =BBUFR(L+77)
                    VFI17(NIN)%P(3,13,NN) =BBUFR(L+78)
                    XFI17(NIN)%P(1,14,NN) =BBUFR(L+79)
                    XFI17(NIN)%P(2,14,NN) =BBUFR(L+80)
                    XFI17(NIN)%P(3,14,NN) =BBUFR(L+81)
                    VFI17(NIN)%P(1,14,NN) =BBUFR(L+82)
                    VFI17(NIN)%P(2,14,NN) =BBUFR(L+83)
                    VFI17(NIN)%P(3,14,NN) =BBUFR(L+84)
                    XFI17(NIN)%P(1,15,NN) =BBUFR(L+85)
                    XFI17(NIN)%P(2,15,NN) =BBUFR(L+86)
                    XFI17(NIN)%P(3,15,NN) =BBUFR(L+87)
                    VFI17(NIN)%P(1,15,NN) =BBUFR(L+88)
                    VFI17(NIN)%P(2,15,NN) =BBUFR(L+89)
                    VFI17(NIN)%P(3,15,NN) =BBUFR(L+90)
                    XFI17(NIN)%P(1,16,NN) =BBUFR(L+91)
                    XFI17(NIN)%P(2,16,NN) =BBUFR(L+92)
                    XFI17(NIN)%P(3,16,NN) =BBUFR(L+93)
                    VFI17(NIN)%P(1,16,NN) =BBUFR(L+94)
                    VFI17(NIN)%P(2,16,NN) =BBUFR(L+95)
                    VFI17(NIN)%P(3,16,NN) =BBUFR(L+96)
C
                    FROTSFI(NIN)%P(1,NN) = BBUFR(L+97)
                    FROTSFI(NIN)%P(2,NN) = BBUFR(L+98)
                    FROTSFI(NIN)%P(3,NN) = BBUFR(L+99)
                    FROTSFI(NIN)%P(4,NN) = BBUFR(L+100)
                    L = L + LEN17
C
C Recompute FROTS
C
                    AX = UN/MAX(EM20,FROTSFI(NIN)%P(4,NN))
                    FROTSFI(NIN)%P(5,NN)=AX*FROTSFI(NIN)%P(1,NN)
                    FROTSFI(NIN)%P(6,NN)=AX*FROTSFI(NIN)%P(2,NN)
                    FROTSFI(NIN)%P(7,NN)=AX*FROTSFI(NIN)%P(3,NN)
                    FROTSFI(NIN)%P(1,NN)=ZERO
                    FROTSFI(NIN)%P(2,NN)=ZERO
                    FROTSFI(NIN)%P(3,NN)=ZERO
                    FROTSFI(NIN)%P(4,NN)=ZERO
C
C Recompute EMINX like in I16BOX (verifier compilation ne casse pas P/ON)
C
                    EMINXFI(NIN)%P(1,NN) = EP30
                    EMINXFI(NIN)%P(2,NN) = EP30
                    EMINXFI(NIN)%P(3,NN) = EP30
                    EMINXFI(NIN)%P(4,NN) = -EP30
                    EMINXFI(NIN)%P(5,NN) = -EP30
                    EMINXFI(NIN)%P(6,NN) = -EP30
C Face 1
C DIR X
                    X1 = XFI17(NIN)%P(1,1,NN)
                    X2 = XFI17(NIN)%P(1,2,NN)
                    X3 = XFI17(NIN)%P(1,3,NN)
                    X4 = XFI17(NIN)%P(1,4,NN)
                    X9 = XFI17(NIN)%P(1,9,NN)
                    X10= XFI17(NIN)%P(1,10,NN)
                    X11= XFI17(NIN)%P(1,11,NN)
                    X12= XFI17(NIN)%P(1,12,NN)
C
                    XC = UNDEMI*(X9+X10+X11+X12) - QUART*(X1+X2+X3+X4)
                    D4 = QUART * ABS(X1-X2)
                    AN = MIN( X1 , X2 , X9-D4 )
                    AX = MAX( X1 , X2 , X9+D4 )
                    D4 = QUART * ABS(X3-X4)
                    BN = MIN( X3 , X4 , X11-D4 )
                    BX = MAX( X3 , X4 , X11+D4 )
                    D4 = QUART * ABS(X12-X10)
                    CN = MIN( X12 , X10 , XC-D4 )
                    CX = MAX( X12 , X10 , XC+D4 )
                    D8 = UNHUIT * MAX( AX-BN , BX-AN )
                    D4 = D8 + D8
                    DN = MAX(MIN(AN,BN,CN-D4),MIN(AN,BN,CN)-D8)
                    DX = MIN(MAX(AX,BX,CX+D4),MAX(AX,BX,CX)+D8)
C
                    EMINXFI(NIN)%P(1,NN) = MIN(EMINXFI(NIN)%P(1,NN),DN)
                    EMINXFI(NIN)%P(4,NN) = MAX(EMINXFI(NIN)%P(4,NN),DX)
C DIR Y
                    X1 = XFI17(NIN)%P(2,1,NN)
                    X2 = XFI17(NIN)%P(2,2,NN)
                    X3 = XFI17(NIN)%P(2,3,NN)
                    X4 = XFI17(NIN)%P(2,4,NN)
                    X9 = XFI17(NIN)%P(2,9,NN)
                    X10= XFI17(NIN)%P(2,10,NN)
                    X11= XFI17(NIN)%P(2,11,NN)
                    X12= XFI17(NIN)%P(2,12,NN)
C
                    XC = UNDEMI*(X9+X10+X11+X12) - QUART*(X1+X2+X3+X4)
                    D4 = QUART * ABS(X1-X2)
                    AN = MIN( X1 , X2 , X9-D4 )
                    AX = MAX( X1 , X2 , X9+D4 )
                    D4 = QUART * ABS(X3-X4)
                    BN = MIN( X3 , X4 , X11-D4 )
                    BX = MAX( X3 , X4 , X11+D4 )
                    D4 = QUART * ABS(X12-X10)
                    CN = MIN( X12 , X10 , XC-D4 )
                    CX = MAX( X12 , X10 , XC+D4 )
                    D8 = UNHUIT * MAX( AX-BN , BX-AN )
                    D4 = D8 + D8
                    DN = MAX(MIN(AN,BN,CN-D4),MIN(AN,BN,CN)-D8)
                    DX = MIN(MAX(AX,BX,CX+D4),MAX(AX,BX,CX)+D8)
C
                    EMINXFI(NIN)%P(2,NN) = MIN(EMINXFI(NIN)%P(2,NN),DN)
                    EMINXFI(NIN)%P(5,NN) = MAX(EMINXFI(NIN)%P(5,NN),DX)
C DIR Z
                    X1 = XFI17(NIN)%P(3,1,NN)
                    X2 = XFI17(NIN)%P(3,2,NN)
                    X3 = XFI17(NIN)%P(3,3,NN)
                    X4 = XFI17(NIN)%P(3,4,NN)
                    X9 = XFI17(NIN)%P(3,9,NN)
                    X10= XFI17(NIN)%P(3,10,NN)
                    X11= XFI17(NIN)%P(3,11,NN)
                    X12= XFI17(NIN)%P(3,12,NN)
C
                    XC = UNDEMI*(X9+X10+X11+X12) - QUART*(X1+X2+X3+X4)
                    D4 = QUART * ABS(X1-X2)
                    AN = MIN( X1 , X2 , X9-D4 )
                    AX = MAX( X1 , X2 , X9+D4 )
                    D4 = QUART * ABS(X3-X4)
                    BN = MIN( X3 , X4 , X11-D4 )
                    BX = MAX( X3 , X4 , X11+D4 )
                    D4 = QUART * ABS(X12-X10)
                    CN = MIN( X12 , X10 , XC-D4 )
                    CX = MAX( X12 , X10 , XC+D4 )
                    D8 = UNHUIT * MAX( AX-BN , BX-AN )
                    D4 = D8 + D8
                    DN = MAX(MIN(AN,BN,CN-D4),MIN(AN,BN,CN)-D8)
                    DX = MIN(MAX(AX,BX,CX+D4),MAX(AX,BX,CX)+D8)
C
                    EMINXFI(NIN)%P(3,NN) = MIN(EMINXFI(NIN)%P(3,NN),DN)
                    EMINXFI(NIN)%P(6,NN) = MAX(EMINXFI(NIN)%P(6,NN),DX)
C Face 2
C DIR X
                    X1 = XFI17(NIN)%P(1,5,NN)
                    X2 = XFI17(NIN)%P(1,6,NN)
                    X3 = XFI17(NIN)%P(1,7,NN)
                    X4 = XFI17(NIN)%P(1,8,NN)
                    X9 = XFI17(NIN)%P(1,13,NN)
                    X10= XFI17(NIN)%P(1,14,NN)
                    X11= XFI17(NIN)%P(1,15,NN)
                    X12= XFI17(NIN)%P(1,16,NN)
C
                    XC = UNDEMI*(X9+X10+X11+X12) - QUART*(X1+X2+X3+X4)
                    D4 = QUART * ABS(X1-X2)
                    AN = MIN( X1 , X2 , X9-D4 )
                    AX = MAX( X1 , X2 , X9+D4 )
                    D4 = QUART * ABS(X3-X4)
                    BN = MIN( X3 , X4 , X11-D4 )
                    BX = MAX( X3 , X4 , X11+D4 )
                    D4 = QUART * ABS(X12-X10)
                    CN = MIN( X12 , X10 , XC-D4 )
                    CX = MAX( X12 , X10 , XC+D4 )
                    D8 = UNHUIT * MAX( AX-BN , BX-AN )
                    D4 = D8 + D8
                    DN = MAX(MIN(AN,BN,CN-D4),MIN(AN,BN,CN)-D8)
                    DX = MIN(MAX(AX,BX,CX+D4),MAX(AX,BX,CX)+D8)
C
                    EMINXFI(NIN)%P(1,NN) = MIN(EMINXFI(NIN)%P(1,NN),DN)
                    EMINXFI(NIN)%P(4,NN) = MAX(EMINXFI(NIN)%P(4,NN),DX)
C DIR Y
                    X1 = XFI17(NIN)%P(2,5,NN)
                    X2 = XFI17(NIN)%P(2,6,NN)
                    X3 = XFI17(NIN)%P(2,7,NN)
                    X4 = XFI17(NIN)%P(2,8,NN)
                    X9 = XFI17(NIN)%P(2,13,NN)
                    X10= XFI17(NIN)%P(2,14,NN)
                    X11= XFI17(NIN)%P(2,15,NN)
                    X12= XFI17(NIN)%P(2,16,NN)
C
                    XC = UNDEMI*(X9+X10+X11+X12) - QUART*(X1+X2+X3+X4)
                    D4 = QUART * ABS(X1-X2)
                    AN = MIN( X1 , X2 , X9-D4 )
                    AX = MAX( X1 , X2 , X9+D4 )
                    D4 = QUART * ABS(X3-X4)
                    BN = MIN( X3 , X4 , X11-D4 )
                    BX = MAX( X3 , X4 , X11+D4 )
                    D4 = QUART * ABS(X12-X10)
                    CN = MIN( X12 , X10 , XC-D4 )
                    CX = MAX( X12 , X10 , XC+D4 )
                    D8 = UNHUIT * MAX( AX-BN , BX-AN )
                    D4 = D8 + D8
                    DN = MAX(MIN(AN,BN,CN-D4),MIN(AN,BN,CN)-D8)
                    DX = MIN(MAX(AX,BX,CX+D4),MAX(AX,BX,CX)+D8)
C
                    EMINXFI(NIN)%P(2,NN) = MIN(EMINXFI(NIN)%P(2,NN),DN)
                    EMINXFI(NIN)%P(5,NN) = MAX(EMINXFI(NIN)%P(5,NN),DX)
C DIR Z
                    X1 = XFI17(NIN)%P(3,5,NN)
                    X2 = XFI17(NIN)%P(3,6,NN)
                    X3 = XFI17(NIN)%P(3,7,NN)
                    X4 = XFI17(NIN)%P(3,8,NN)
                    X9 = XFI17(NIN)%P(3,13,NN)
                    X10= XFI17(NIN)%P(3,14,NN)
                    X11= XFI17(NIN)%P(3,15,NN)
                    X12= XFI17(NIN)%P(3,16,NN)
C
                    XC = UNDEMI*(X9+X10+X11+X12) - QUART*(X1+X2+X3+X4)
                    D4 = QUART * ABS(X1-X2)
                    AN = MIN( X1 , X2 , X9-D4 )
                    AX = MAX( X1 , X2 , X9+D4 )
                    D4 = QUART * ABS(X3-X4)
                    BN = MIN( X3 , X4 , X11-D4 )
                    BX = MAX( X3 , X4 , X11+D4 )
                    D4 = QUART * ABS(X12-X10)
                    CN = MIN( X12 , X10 , XC-D4 )
                    CX = MAX( X12 , X10 , XC+D4 )
                    D8 = UNHUIT * MAX( AX-BN , BX-AN )
                    D4 = D8 + D8
                    DN = MAX(MIN(AN,BN,CN-D4),MIN(AN,BN,CN)-D8)
                    DX = MIN(MAX(AX,BX,CX+D4),MAX(AX,BX,CX)+D8)
C
                    EMINXFI(NIN)%P(3,NN) = MIN(EMINXFI(NIN)%P(3,NN),DN)
                    EMINXFI(NIN)%P(6,NN) = MAX(EMINXFI(NIN)%P(6,NN),DX)
                  ENDDO
C fin type 17
                ELSEIF(NTY == 20 .AND. INTTH == 0 ) THEN
C type 20
                 IF(DT_CST.NE.0)THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                      DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+8)
                      DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+9)
                      DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+10)
                      DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+11)
                      DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+12)
                      DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+13)
C DAANCFI
                      DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+14)
                      DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+15)
                      DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+16)
C Alphakfi
                      ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+17)
C
c                print*,ispmd,I+IDEB, BBUFR(L+14),BBUFR(L+15),BBUFR(L+16)
                      CALL GETDPDAANC(
     .                DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+18),IRESP,INC)
C                     L  = L + INC
                      L = L + LEN20
                    ENDDO
C AMS
                   ELSE
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                      DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+8)
                      DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+9)
                      DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+10)
                      DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+11)
                      DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+12)
                      DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+13)
C DAANCFI
                      DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+14)
                      DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+15)
                      DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+16)
C Alphakfi
                      ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+17)

                      CALL GETDPDAANC(
     .                DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+18),IRESP,INC)
C                      L  = L + INC
                      DIAG_SMSFI(NIN)%P(I+IDEB)  = BBUFR(L+18+INC)
                      L = L + LEN20
                    ENDDO
                   END IF
C fin AMS
                 ELSE
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+7)
                      DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+8)
                      DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+9)
                      DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+10)
                      DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+11)
                      DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+12)
C DAANCFI
                      DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+13)
                      DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+14)
                      DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+15)
C Alphakfi
                      ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+16)
CC
                      CALL GETDPDAANC(
     .                DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+17),IRESP,INC)
C                     L  = L + INC
                      L = L + LEN20
                    ENDDO
C AMS

                  ELSE
                    DO I = 1, NB
                      XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                      XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                      XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                      VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                      VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                      VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                      DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+7)
                      DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+8)
                      DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+9)
                      DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+10)
                      DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+11)
                      DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+12)
C DAANCFI
                      DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+13)
                      DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+14)
                      DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+15)
C Alphakfi
                      ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+16)
                      CALL GETDPDAANC(
     .                DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+17),IRESP,INC)
C                      L  = L + INC
                      DIAG_SMSFI(NIN)%P(I+IDEB)  = BBUFR(L+17+INC)
                      L = L + LEN20
                    ENDDO
                   END IF
C fin AMS
                 ENDIF
C
                ELSEIF(NTY == 20 .AND. INTTH > 0 ) THEN
                   IF(DT_CST.NE.0)THEN
                     IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+8)
                        DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+9)
                        DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+10)
                        DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+11)
                        DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+12)
                        DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+13)
                        DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+14)
C DAANCFI
                        DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+15)
                        DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+16)
                        DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+17)
C Alphakfi
                        ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+18)
C
                        CALL GETDPDAANC(
     .                  DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+19),IRESP,
     .                  INC)
C                       L  = L + INC
                        L = L + LEN20T
                      ENDDO
C AMS
                     ELSE
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        MSFI(NIN)%P(I+IDEB)  = BBUFR(L+7)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+8)
                        DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+9)
                        DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+10)
                        DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+11)
                        DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+12)
                        DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+13)
                        DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+14)
C DAANCFI
                        DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+15)
                        DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+16)
                        DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+17)
C Alphakfi
                        ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+18)
C
                        CALL GETDPDAANC(
     .                  DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+19),IRESP,
     .                  INC)
C                       L  = L + INC
                        DIAG_SMSFI(NIN)%P(I+IDEB)  = BBUFR(L+19+INC)
                        L = L + LEN20T
                      ENDDO
C fin AMS
                     END IF
                   ELSE
                     IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+7)
                        DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+8)
                        DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+9)
                        DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+10)
                        DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+11)
                        DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+12)
                        DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+13)
C DAANCFI
                        DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+15)
                        DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+16)
                        DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+17)
C Alphakfi
                        ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+18)

                        CALL GETDPDAANC(
     .                  DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+19),IRESP,
     .                  INC)
C                       L  = L + INC
                        L = L + LEN20T
                      ENDDO
C AMS
                     ELSE
                      DO I = 1, NB
                        XFI(NIN)%P(1,I+IDEB) = BBUFR(L+1)
                        XFI(NIN)%P(2,I+IDEB) = BBUFR(L+2)
                        XFI(NIN)%P(3,I+IDEB) = BBUFR(L+3)
                        VFI(NIN)%P(1,I+IDEB) = BBUFR(L+4)
                        VFI(NIN)%P(2,I+IDEB) = BBUFR(L+5)
                        VFI(NIN)%P(3,I+IDEB) = BBUFR(L+6)
                        TEMPFI(NIN)%P(I+IDEB)= BBUFR(L+7)

                        DXANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+8)
                        DXANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+9)
                        DXANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+10)
                        DVANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+11)
                        DVANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+12)
                        DVANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+13)
C DAANCFI
                        DAANCFI(NIN)%P(1,I+IDEB) = BBUFR(L+14)
                        DAANCFI(NIN)%P(2,I+IDEB) = BBUFR(L+15)
                        DAANCFI(NIN)%P(3,I+IDEB) = BBUFR(L+16)
C Alphakfi
                        ALPHAKFI(NIN)%P(I+IDEB) = BBUFR(L+17)
                        CALL GETDPDAANC(
     .                  DAANC6FI(NIN)%P(1,1,I+IDEB),BBUFR(L+18),IRESP,
     .                  INC)
C                       L  = L + INC
                        DIAG_SMSFI(NIN)%P(I+IDEB)  = BBUFR(L+18+INC)
                        L = L + LEN20T
                      ENDDO
                     END IF
C fin AMS
                   ENDIF
C fin type 20
                ENDIF
                DEBUT(NIN) = DEBUT(NIN) + NB
              ENDIF
C
C Extra Code for Type20 Edge
C
              IF(IPARI(7,NIN).EQ.20) THEN
                NB = NSNFIE(NIN)%P(P)
                IF(NB.GT.0)THEN
                  IDEB = DEBUTE(NIN)
                  IF(DT_CST.NE.0)THEN
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFIE(NIN)%P(1,N1) = BBUFR(L+1)
                      XFIE(NIN)%P(2,N1) = BBUFR(L+2)
                      XFIE(NIN)%P(3,N1) = BBUFR(L+3)
                      VFIE(NIN)%P(1,N1) = BBUFR(L+4)
                      VFIE(NIN)%P(2,N1) = BBUFR(L+5)
                      VFIE(NIN)%P(3,N1) = BBUFR(L+6)
                      MSFIE(NIN)%P(N1)  = BBUFR(L+7)
                      XFIE(NIN)%P(1,N2) = BBUFR(L+8)
                      XFIE(NIN)%P(2,N2) = BBUFR(L+9)
                      XFIE(NIN)%P(3,N2) = BBUFR(L+10)
                      VFIE(NIN)%P(1,N2) = BBUFR(L+11)
                      VFIE(NIN)%P(2,N2) = BBUFR(L+12)
                      VFIE(NIN)%P(3,N2) = BBUFR(L+13)
                      MSFIE(NIN)%P(N2)  = BBUFR(L+14)
C
                      DXANCFIE(NIN)%P(1,N1) = BBUFR(L+15)
                      DXANCFIE(NIN)%P(2,N1) = BBUFR(L+16)
                      DXANCFIE(NIN)%P(3,N1) = BBUFR(L+17)
                      DVANCFIE(NIN)%P(1,N1) = BBUFR(L+18)
                      DVANCFIE(NIN)%P(2,N1) = BBUFR(L+19)
                      DVANCFIE(NIN)%P(3,N1) = BBUFR(L+20)
                      DXANCFIE(NIN)%P(1,N2) = BBUFR(L+21)
                      DXANCFIE(NIN)%P(2,N2) = BBUFR(L+22)
                      DXANCFIE(NIN)%P(3,N2) = BBUFR(L+23)
                      DVANCFIE(NIN)%P(1,N2) = BBUFR(L+24)
                      DVANCFIE(NIN)%P(2,N2) = BBUFR(L+25)
                      DVANCFIE(NIN)%P(3,N2) = BBUFR(L+26)
C
                      DAANCFIE(NIN)%P(1,N1) = BBUFR(L+27)
                      DAANCFIE(NIN)%P(2,N1) = BBUFR(L+28)
                      DAANCFIE(NIN)%P(3,N1) = BBUFR(L+29)
C
                      DAANCFIE(NIN)%P(1,N2) = BBUFR(L+30)
                      DAANCFIE(NIN)%P(2,N2) = BBUFR(L+31)
                      DAANCFIE(NIN)%P(3,N2) = BBUFR(L+32)
C
                      ALPHAKFIE(NIN)%P(N1)  = BBUFR(L+33)
C
                      ALPHAKFIE(NIN)%P(N2)  = BBUFR(L+34)
C
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N1),BBUFR(L+35),IRESP,INC)
C                     L  = L + INC
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N2),BBUFR(L+35+INC),IRESP,
     .                INC)
C                     L  = L + INC
                      L = L + LEN20E
                    ENDDO
C AMS
                   ELSE
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFIE(NIN)%P(1,N1) = BBUFR(L+1)
                      XFIE(NIN)%P(2,N1) = BBUFR(L+2)
                      XFIE(NIN)%P(3,N1) = BBUFR(L+3)
                      VFIE(NIN)%P(1,N1) = BBUFR(L+4)
                      VFIE(NIN)%P(2,N1) = BBUFR(L+5)
                      VFIE(NIN)%P(3,N1) = BBUFR(L+6)
                      MSFIE(NIN)%P(N1)  = BBUFR(L+7)
                      XFIE(NIN)%P(1,N2) = BBUFR(L+8)
                      XFIE(NIN)%P(2,N2) = BBUFR(L+9)
                      XFIE(NIN)%P(3,N2) = BBUFR(L+10)
                      VFIE(NIN)%P(1,N2) = BBUFR(L+11)
                      VFIE(NIN)%P(2,N2) = BBUFR(L+12)
                      VFIE(NIN)%P(3,N2) = BBUFR(L+13)
                      MSFIE(NIN)%P(N2)  = BBUFR(L+14)
C
                      DXANCFIE(NIN)%P(1,N1) = BBUFR(L+15)
                      DXANCFIE(NIN)%P(2,N1) = BBUFR(L+16)
                      DXANCFIE(NIN)%P(3,N1) = BBUFR(L+17)
                      DVANCFIE(NIN)%P(1,N1) = BBUFR(L+18)
                      DVANCFIE(NIN)%P(2,N1) = BBUFR(L+19)
                      DVANCFIE(NIN)%P(3,N1) = BBUFR(L+20)
                      DXANCFIE(NIN)%P(1,N2) = BBUFR(L+21)
                      DXANCFIE(NIN)%P(2,N2) = BBUFR(L+22)
                      DXANCFIE(NIN)%P(3,N2) = BBUFR(L+23)
                      DVANCFIE(NIN)%P(1,N2) = BBUFR(L+24)
                      DVANCFIE(NIN)%P(2,N2) = BBUFR(L+25)
                      DVANCFIE(NIN)%P(3,N2) = BBUFR(L+26)
C
                      DAANCFIE(NIN)%P(1,N1) = BBUFR(L+27)
                      DAANCFIE(NIN)%P(2,N1) = BBUFR(L+28)
                      DAANCFIE(NIN)%P(3,N1) = BBUFR(L+29)
C
                      DAANCFIE(NIN)%P(1,N2) = BBUFR(L+30)
                      DAANCFIE(NIN)%P(2,N2) = BBUFR(L+31)
                      DAANCFIE(NIN)%P(3,N2) = BBUFR(L+32)
C
                      ALPHAKFIE(NIN)%P(N1)  = BBUFR(L+33)
C
                      ALPHAKFIE(NIN)%P(N2)  = BBUFR(L+34)
C
C
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N1),BBUFR(L+35),IRESP,INC)
C                     L  = L + INC
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N2),BBUFR(L+35+INC),IRESP,
     .                INC)
C                     L  = L + INC
                      DIAG_SMSFIE(NIN)%P(N1)  = BBUFR(L+35+2*INC)
                      DIAG_SMSFIE(NIN)%P(N2)  = BBUFR(L+26+2*INC)
                      L = L + LEN20E
                    ENDDO
                   END IF
C fin AMS
                  ELSE
                   IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFIE(NIN)%P(1,N1) = BBUFR(L+1)
                      XFIE(NIN)%P(2,N1) = BBUFR(L+2)
                      XFIE(NIN)%P(3,N1) = BBUFR(L+3)
                      VFIE(NIN)%P(1,N1) = BBUFR(L+4)
                      VFIE(NIN)%P(2,N1) = BBUFR(L+5)
                      VFIE(NIN)%P(3,N1) = BBUFR(L+6)
                      XFIE(NIN)%P(1,N2) = BBUFR(L+7)
                      XFIE(NIN)%P(2,N2) = BBUFR(L+8)
                      XFIE(NIN)%P(3,N2) = BBUFR(L+9)
                      VFIE(NIN)%P(1,N2) = BBUFR(L+10)
                      VFIE(NIN)%P(2,N2) = BBUFR(L+11)
                      VFIE(NIN)%P(3,N2) = BBUFR(L+12)
C
                      DXANCFIE(NIN)%P(1,N1) = BBUFR(L+13)
                      DXANCFIE(NIN)%P(2,N1) = BBUFR(L+14)
                      DXANCFIE(NIN)%P(3,N1) = BBUFR(L+15)
                      DVANCFIE(NIN)%P(1,N1) = BBUFR(L+16)
                      DVANCFIE(NIN)%P(2,N1) = BBUFR(L+17)
                      DVANCFIE(NIN)%P(3,N1) = BBUFR(L+18)
                      DXANCFIE(NIN)%P(1,N2) = BBUFR(L+19)
                      DXANCFIE(NIN)%P(2,N2) = BBUFR(L+20)
                      DXANCFIE(NIN)%P(3,N2) = BBUFR(L+21)
                      DVANCFIE(NIN)%P(1,N2) = BBUFR(L+22)
                      DVANCFIE(NIN)%P(2,N2) = BBUFR(L+23)
                      DVANCFIE(NIN)%P(3,N2) = BBUFR(L+24)
C
                      DAANCFIE(NIN)%P(1,N1) = BBUFR(L+25)
                      DAANCFIE(NIN)%P(2,N1) = BBUFR(L+26)
                      DAANCFIE(NIN)%P(3,N1) = BBUFR(L+27)
C
                      DAANCFIE(NIN)%P(1,N2) = BBUFR(L+28)
                      DAANCFIE(NIN)%P(2,N2) = BBUFR(L+29)
                      DAANCFIE(NIN)%P(3,N2) = BBUFR(L+30)
C
                      ALPHAKFIE(NIN)%P(N1)  = BBUFR(L+31)
C
                      ALPHAKFIE(NIN)%P(N2)  = BBUFR(L+32)
C
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N1),BBUFR(L+33),IRESP,INC)
C                     L  = L + INC
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N2),BBUFR(L+33+INC),IRESP,
     .                INC)
C                     L  = L + INC
                      L = L + LEN20E
                    END DO
C AMS
                   ELSE
                    DO I = 1, NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFIE(NIN)%P(1,N1) = BBUFR(L+1)
                      XFIE(NIN)%P(2,N1) = BBUFR(L+2)
                      XFIE(NIN)%P(3,N1) = BBUFR(L+3)
                      VFIE(NIN)%P(1,N1) = BBUFR(L+4)
                      VFIE(NIN)%P(2,N1) = BBUFR(L+5)
                      VFIE(NIN)%P(3,N1) = BBUFR(L+6)
                      XFIE(NIN)%P(1,N2) = BBUFR(L+7)
                      XFIE(NIN)%P(2,N2) = BBUFR(L+8)
                      XFIE(NIN)%P(3,N2) = BBUFR(L+9)
                      VFIE(NIN)%P(1,N2) = BBUFR(L+10)
                      VFIE(NIN)%P(2,N2) = BBUFR(L+11)
                      VFIE(NIN)%P(3,N2) = BBUFR(L+12)
C
                      DXANCFIE(NIN)%P(1,N1) = BBUFR(L+13)
                      DXANCFIE(NIN)%P(2,N1) = BBUFR(L+14)
                      DXANCFIE(NIN)%P(3,N1) = BBUFR(L+15)
                      DVANCFIE(NIN)%P(1,N1) = BBUFR(L+16)
                      DVANCFIE(NIN)%P(2,N1) = BBUFR(L+17)
                      DVANCFIE(NIN)%P(3,N1) = BBUFR(L+18)
                      DXANCFIE(NIN)%P(1,N2) = BBUFR(L+19)
                      DXANCFIE(NIN)%P(2,N2) = BBUFR(L+20)
                      DXANCFIE(NIN)%P(3,N2) = BBUFR(L+21)
                      DVANCFIE(NIN)%P(1,N2) = BBUFR(L+22)
                      DVANCFIE(NIN)%P(2,N2) = BBUFR(L+23)
                      DVANCFIE(NIN)%P(3,N2) = BBUFR(L+24)
C
                      DAANCFIE(NIN)%P(1,N1) = BBUFR(L+25)
                      DAANCFIE(NIN)%P(2,N1) = BBUFR(L+26)
                      DAANCFIE(NIN)%P(3,N1) = BBUFR(L+27)
C
                      DAANCFIE(NIN)%P(1,N2) = BBUFR(L+28)
                      DAANCFIE(NIN)%P(2,N2) = BBUFR(L+29)
                      DAANCFIE(NIN)%P(3,N2) = BBUFR(L+30)
C
                      ALPHAKFIE(NIN)%P(N1)  = BBUFR(L+31)
C
                      ALPHAKFIE(NIN)%P(N2)  = BBUFR(L+32)
C
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N1),BBUFR(L+33),IRESP,INC)
C                     L  = L + INC
                      CALL GETDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N2),BBUFR(L+33+INC),IRESP,
     .                INC)
C                     L  = L + INC
                      DIAG_SMSFIE(NIN)%P(N1)  = BBUFR(L+33+2*INC)
                      DIAG_SMSFIE(NIN)%P(N2)  = BBUFR(L+24+2*INC)
                      L = L + LEN20E
                    END DO
                   END IF
C fin AMS
                  ENDIF
                END IF
                DEBUTE(NIN) = DEBUTE(NIN) + NB
              END IF
C end extra for i20 edge

C Extra Code for Type25 Edge
C
             IEDGE = IPARI(58,NIN)

              IF(IPARI(7,NIN).EQ.25 .AND. IEDGE .NE. 0) THEN
                NB = NSNFIE(NIN)%P(P)
                IF(NB.GT.0)THEN
                  IDEB = DEBUTE(NIN)
                  DO I = 1,NB
                      N1 = 2*(I+IDEB-1)+1
                      N2 = 2*(I+IDEB)
                      XFIE(NIN)%P(1,N1) = BBUFR(L+1)
                      XFIE(NIN)%P(2,N1) = BBUFR(L+2)
                      XFIE(NIN)%P(3,N1) = BBUFR(L+3)
                      VFIE(NIN)%P(1,N1) = BBUFR(L+4)
                      VFIE(NIN)%P(2,N1) = BBUFR(L+5)
                      VFIE(NIN)%P(3,N1) = BBUFR(L+6)
                      MSFIE(NIN)%P(N1)  = BBUFR(L+7)
                      XFIE(NIN)%P(1,N2) = BBUFR(L+8)
                      XFIE(NIN)%P(2,N2) = BBUFR(L+9)
                      XFIE(NIN)%P(3,N2) = BBUFR(L+10)
                      VFIE(NIN)%P(1,N2) = BBUFR(L+11)
                      VFIE(NIN)%P(2,N2) = BBUFR(L+12)
                      VFIE(NIN)%P(3,N2) = BBUFR(L+13)
                      MSFIE(NIN)%P(N2)  = BBUFR(L+14)
c                     IF(BBUFR(L+15) < 0) THEN
c                       LEDGE_FIE(NIN)%P(E_IM,I+IDEB) = -1 
c                     ELSE
c                       LEDGE_FIE(NIN)%P(E_IM,I+IDEB) = 1 
c                     ENDIF

C                     LEDGE_FIE(NIN)%P(E_LEFT_SEG,I+IDEB) = TRANSFER(BBUFR(L+16),L)
C                     LEDGE_FIE(NIN)%P(E_LEFT_ID,I+IDEB) = TRANSFER(BBUFR(L+17),L)
C                     LEDGE_FIE(NIN)%P(E_RIGHT_SEG,I+IDEB) = TRANSFER(BBUFR(L+18),L)
C                     LEDGE_FIE(NIN)%P(E_RIGHT_ID,I+IDEB) = TRANSFER(BBUFR(L+19),L)
            
C                     WRITE(6,"(A,4I10)") "++",LEDGE_FIE(NIN)%P(1:4,NN)


c                     VTX_BISECTOR_FIE(NIN)%P(1,1,I+IDEB) = BBUFR(L+18)
c                     VTX_BISECTOR_FIE(NIN)%P(2,1,I+IDEB) = BBUFR(L+19)
c                     VTX_BISECTOR_FIE(NIN)%P(3,1,I+IDEB) = BBUFR(L+20)
c                     VTX_BISECTOR_FIE(NIN)%P(1,2,I+IDEB) = BBUFR(L+21)
c                     VTX_BISECTOR_FIE(NIN)%P(2,2,I+IDEB) = BBUFR(L+22)
c                     VTX_BISECTOR_FIE(NIN)%P(3,2,I+IDEB) = BBUFR(L+23)
c                     VTX_BISECTOR_FIE(NIN)%P(1,3,I+IDEB) = BBUFR(L+24)
c                     VTX_BISECTOR_FIE(NIN)%P(2,3,I+IDEB) = BBUFR(L+25)
c                     VTX_BISECTOR_FIE(NIN)%P(3,3,I+IDEB) = BBUFR(L+26)
c                     MASTER_FIE(NIN)%P(I+IDEB) = BBUFR(L+27)



                      L = L + LEN25E
                   ENDDO
                ENDIF
                DEBUTE(NIN) = DEBUTE(NIN) + NB
C               IF(ISPMD==4) write(6,*) __FILE__," increN ",NB
              ENDIF

            END DO
          END IF
        END DO
        ISTOCOM = 0
       END IF
C cas interface inactive
       IF(IALLOCR.GT.0)THEN
         DEALLOCATE(BBUFR)
         IALLOCR=0
       END IF
C
C Attente ISEND
C
C flag 3 utile pour restart en cours de run
       IF(IFLAG.NE.3) THEN
         DO P = 1, NSPMD
           IF(ADDS(P+1)-ADDS(P).GT.0) THEN
             CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
           ENDIF
         ENDDO
         IF(IALLOCS.GT.0)THEN
           DEALLOCATE(BBUFS)
           IALLOCS=0
         END IF
       END IF
      END IF
C
#endif
      RETURN
      END
