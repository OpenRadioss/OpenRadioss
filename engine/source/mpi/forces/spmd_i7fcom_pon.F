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
Chd|  SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ADDCOMI20                     source/mpi/interfaces/spmd_i7tool.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTCONTP                      source/mpi/interfaces/spmd_i7tool.F
Chd|        INTCONTP25E                   source/mpi/interfaces/intcontp25e.F
Chd|        PUTDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        PUTDPZERO                     source/mpi/interfaces/spmd_i7tool.F
Chd|        REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|        SORTI11                       source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTI11T                      source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTI11TT                     source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTI17                       source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTI20                       source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTI25                       source/mpi/interfaces/sorti25.F
Chd|        SORTINT                       source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD11_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD17_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20E_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20FE_PON            source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20F_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD25E_PON             source/mpi/interfaces/spmd_fiadd25e_pon.F
Chd|        SPMD_FIADD_PON                source/mpi/interfaces/spmd_i7tool.F
Chd|        FSKYI_MOD                     share/modules/restart_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        HEAT_MOD                      share/modules/heat_mod.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I7FCOM_PON(
     1                       IPARI   ,INTLIST,
     2                       NBINTC  ,NISKYFI,ICODT   ,SECFCUM ,NSTRF  ,
     3                       ICONTACT,FCONT  ,IGRBRIC ,IXS     ,
     4                       IXS16   ,NISKYFIE,NBINT20,
     5                       IFLAG, INTBUF_TAB,  SFSKYI ,
     6                       SISKY,H3D_DATA ,MULTI_FVM  ,                    
     7                       TAGNCONT  ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE FSKYI_MOD
      USE HEAT_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "macro.inc"
#include      "assert.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
#include      "i25edge_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG, NBINTC, NBINT20,
     .        IPARI(NPARI,*),  INTLIST(*), NISKYFI(*),
     .        ICODT(*), ICONTACT(*), NSTRF(*),
     .        IXS(*), IXS16(*), NISKYFIE(*),  
     .        TAGNCONT(NLOADP_HYD_INTER,*),KLOADPINTER(*),
     .        LOADPINTER(*),LOADP_HYD_INTER(*),
     .        SFSKYI , SISKY
      my_real
     .        SECFCUM(7,NUMNOD,NSECT), FCONT(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, L, ADD, LL, NB, LEN, SIZ, KFI, LOC_PROC, MULTIMP, II,
     .        NIN, IDEB, N, MSGTYP, IERROR, IDEBI, NI, NOD, NIE,
     .        IBC, ISECIN, IBAG, NOINT, NTY, LEN11, N1, N2,LENI,INACTI,
     .        IALLOCS, IALLOCR, LEN17, JJ, NAD, IGN, IGE, NMES, NME,
     .        IADM, IES,INTTH,LEN7T,LEN20, LEN20E, INC,LEN11T,
     .        MSGOFF, MSGOFF2,J,K,
     .        JD(50), KD(50),  STATUS(MPI_STATUS_SIZE),
     .        DEBUT(NINTER), DEBUTI(NINTER),
     .        DEBUTE(NINTER), DEBUTIE(NINTER),
C parasiz car variable en save
     .        REQ_SI(PARASIZ),REQ_S(PARASIZ),REQ_R(PARASIZ),
     .        ISIZRCV(2,PARASIZ),ISIZENV(2,PARASIZ),
     .        NSNFITOT(PARASIZ),NSNSITOT(PARASIZ)
      INTEGER :: LEN25E ! edge2edge additional length
              INTEGER SISKY_OLD,LSKYI_OLD,LSKYI_CT 
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX
      my_real, DIMENSION(:,:), ALLOCATABLE :: TEMPO
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FTEMP
      INTEGER INDEXSIZ,MAX_ARRAYS,NUM_ARRAYS,TEMP_SIZ
      INTEGER :: THOFFSET
      INTEGER :: NB_TOT_EDGES
      LOGICAL :: CONDITION
      DATA MSGOFF/144/
      DATA MSGOFF2/145/

      LOGICAL ITEST
      my_real       ,DIMENSION(:), ALLOCATABLE :: BBUFS, BBUFR
      my_real
     .     BID,RBID(1)
      DOUBLE PRECISION
     .     ZERODP
      SAVE    REQ_SI,REQ_S,REQ_R,ISIZRCV,ISIZENV,
     .        NSNFITOT,NSNSITOT,BBUFS,IALLOCS
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ZERODP = 0.0
      BID = ZERO
      RBID = ZERO
      LOC_PROC = ISPMD + 1

C
      LEN = 5
      IF(KDTINT/=0) LEN = LEN+1
      IF(NODADT_THERM == 1) LEN = LEN+1
      LEN7T = LEN + 1
C type11 => 2 impacts pour une factte
      LEN11 = 2*(LEN-1)+1
      LEN11T = 2*LEN+1
C type17 => 8 impacts
      LEN17 = 41
C type20 => place additionnelle DAANC6
      LEN20 = 18*(1+IRESP) + 1
C type20 => place additionnelle DAANC6E (edge)
      LEN20E = 36*(1+IRESP) + 2
      LEN25E = 10 !+ 2  
C
      IF(IFLAG==1)THEN
C
C Init + ireceive sur taille communication
C
        DO P = 1, NSPMD
          ISIZRCV(1,P)=0
          ISIZRCV(2,P)=0
          ISIZENV(1,P) = 0
          ISIZENV(2,P) = 0
          NSNFITOT(P) = 0
          NSNSITOT(P) = 0
          IF(P/=LOC_PROC)THEN
            SIZ = 0
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IF(MULTI_FVM%INT18_GLOBAL_LIST(NIN)) CYCLE
              SIZ = SIZ + NSNSI(NIN)%P(P)
C ajout partie egde pour type 20
              IF(IPARI(7,NIN)==20)THEN
                SIZ = SIZ + NSNSIE(NIN)%P(P)
              END IF
              IF(IPARI(7,NIN)==25)THEN
                SIZ = SIZ + 2*NSNSIE(NIN)%P(P)
              END IF
            ENDDO
            IF(SIZ>0)THEN
              NSNSITOT(P) = SIZ
              MSGTYP = MSGOFF 
              CALL MPI_IRECV(
     .          ISIZRCV(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_R(P),IERROR   )
            ENDIF
          ENDIF
        ENDDO
C
C Partie 1 envoi et preparation buffer reception
C
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          NI = NISKYFI(NIN)
          NTY  = IPARI(7,NIN)
          IF(MULTI_FVM%INT18_GLOBAL_LIST(NIN)) CYCLE
          IF(NI>0) THEN
            INTTH = IPARI(47,NIN)
            IF(NTY==7.OR.NTY==10.OR.NTY==22.OR.NTY==23.OR.
     .         NTY==24.OR.NTY==25) THEN
     
              ALLOCATE(INDEX(NI),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
              
              IF(INTTH > 0)THEN
                 ALLOCATE( TEMPO(LEN7T,NI),STAT=IERROR)
              ELSE
                 ALLOCATE( TEMPO(LEN,NI),STAT=IERROR)
              ENDIF
              
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
                            
              DO J=1,NI
                 INDEX(J)=J
                 TEMPO(1,J)=FSKYFI(NIN)%P(1,J)
                 TEMPO(2,J)=FSKYFI(NIN)%P(2,J)
                 TEMPO(3,J)=FSKYFI(NIN)%P(3,J)
                 TEMPO(4,J)=FSKYFI(NIN)%P(4,J)
              ENDDO
              
              TEMP_SIZ=5
C KDTINT > 0 option
              IF(NFSKYI==5)THEN
                DO J=1,NI
                  TEMPO(TEMP_SIZ,J)=FSKYFI(NIN)%P(TEMP_SIZ,J)
                ENDDO
                TEMP_SIZ=TEMP_SIZ+1
              ENDIF
C Thermique Option
              IF(INTTH >0)THEN
                DO J=1,NI
                  TEMPO(TEMP_SIZ,J)=FTHESKYFI(NIN)%P(J)
                ENDDO
                TEMP_SIZ=TEMP_SIZ+1
                
                IF (NODADT_THERM == 1)THEN 
                  DO J=1,NI
                    TEMPO(TEMP_SIZ,J)=CONDNSKYFI(NIN)%P(J)
                  ENDDO
                  TEMP_SIZ=TEMP_SIZ+1
                ENDIF
              ENDIF
              CALL SORTINT(NI,ISKYFI(NIN)%P(1),INDEX)
              
              DO J=1,NI
                 K=INDEX(J) 
                 FSKYFI(NIN)%P(1,J)=TEMPO(1,K)
                 FSKYFI(NIN)%P(2,J)=TEMPO(2,K)
                 FSKYFI(NIN)%P(3,J)=TEMPO(3,K)
                 FSKYFI(NIN)%P(4,J)=TEMPO(4,K)
              ENDDO
              TEMP_SIZ=5 
              
C KDTINT > 0 option
              IF(NFSKYI==5)THEN
                DO J=1,NI
                 K=INDEX(J)
                 FSKYFI(NIN)%P(TEMP_SIZ,J)=TEMPO(TEMP_SIZ,K)
                ENDDO
                TEMP_SIZ=TEMP_SIZ+1
              ENDIF
C Thermique Option
              IF(INTTH >0)THEN
                DO J=1,NI
                  K=INDEX(J)
                  FTHESKYFI(NIN)%P(J)=TEMPO(TEMP_SIZ,K)
                ENDDO
                TEMP_SIZ=TEMP_SIZ+1
                
                IF (NODADT_THERM == 1)THEN
                  DO J=1,NI
                    K=INDEX(J)
                    CONDNSKYFI(NIN)%P(J)=TEMPO(TEMP_SIZ,K)
                  ENDDO
                  TEMP_SIZ=TEMP_SIZ+1
               ENDIF
              ENDIF
              LENI = LEN
              IF((NTY == 7 .AND. INTTH > 0 ).OR.(NTY == 25 .AND. INTTH > 0 ).OR.
     +            (NTY == 22 .AND. INTTH > 0 )) LENI = LEN7T
            ELSEIF(NTY==11) THEN
              IF(INTTH > 0) THEN
               IF(NODADT_THERM == 1)THEN
                  CALL SORTI11TT(NI,ISKYFI(NIN)%P(1),FSKYFI(NIN)%P(1,1),
     +             FTHESKYFI(NIN)%P(1),CONDNSKYFI(NIN)%P(1),NFSKYI)
               ELSE
                   CALL SORTI11T(NI,ISKYFI(NIN)%P(1),FSKYFI(NIN)%P(1,1),
     +                    FTHESKYFI(NIN)%P(1),NFSKYI)
               ENDIF
              ELSE
                 CALL SORTI11(NI,ISKYFI(NIN)%P(1),FSKYFI(NIN)%P(1,1),
     +                    NFSKYI)
              ENDIF
c              LENI = LEN
              LENI = LEN11
              IF(INTTH >0 ) LENI = LEN11T
            ELSEIF(NTY==17) THEN
              CALL SORTI17(NI,ISKYFI(NIN)%P(1),FSKYFI(NIN)%P(1,1))
              LENI = LEN17
            ELSEIF(NTY==20) THEN
              CALL SORTI20(NI,ISKYFI(NIN)%P(1),FSKYFI(NIN)%P(1,1),
     +                    NFSKYI)
              LENI =  LEN
              IF(INTTH > 0 ) LENI=LEN7T
C routine calcul place supplementaire int20
              CALL ADDCOMI20(
     +          NSNFI(NIN)%P(1),NSVFI(NIN)%P(1),ISIZENV,LEN20)
            END IF
          ELSEIF(IPARI(7,NIN)==20)THEN ! pour le moment on envoie tjrs
              CALL ADDCOMI20(
     +          NSNFI(NIN)%P(1),NSVFI(NIN)%P(1),ISIZENV,LEN20)
          END IF
C precomptage du nombre de contacts par processeur+calcul nsnfi total
          CALL INTCONTP(
     +      NI,ISKYFI(NIN)%P(1),NSNFI(NIN)%P(1),ISIZENV,NSNFITOT,LENI)
C ajout partie edge
          IF(NTY==20)THEN
            NI = NISKYFIE(NIN)
            CALL SORTI11(NI,ISKYFIE(NIN)%P(1),FSKYFIE(NIN)%P(1,1),
     +                   NFSKYI)
            LENI = LEN11
C routine calcul place supplementaire int20
            CALL ADDCOMI20(
     +        NSNFIE(NIN)%P(1),NSVFIE(NIN)%P(1),ISIZENV,LEN20E)
C precomptage du nombre de contacts par processeur+calcul nsnfi total
            CALL INTCONTP(
     +        NI  ,ISKYFIE(NIN)%P(1),NSNFIE(NIN)%P(1),ISIZENV,NSNFITOT,
     2        LENI)
          ELSEIF (NTY == 25) THEN
            IF(IPARI(MACRO_NEDGE,NIN) > 0) THEN
              NIE = NISKYFIE(NIN)
              CALL SORTI25(NIE,ISKYFIE(NIN)%P(1),FSKYFIE(NIN)%P(1,1),
     +                   4) ! 4 =>  4+1 si thermique
C precomptage du nombre de contacts par processeur+calcul nsnfi total
              LENI = LEN25E
              CALL INTCONTP25E(
     +          NIE  ,ISKYFIE(NIN)%P(1),NSNFIE(NIN)%P(1),ISIZENV,NSNFITOT,
     2          LENI)
            ENDIF
          END IF
C
          IF(NTY==7.OR.NTY==10.OR.NTY==22.OR.NTY==23.OR.
     .       NTY==24.OR.NTY==25) THEN
             IF (NI > 0 ) THEN
               DEALLOCATE(TEMPO,INDEX)
             ENDIF
          ENDIF

        ENDDO
C
        IALLOCS = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC.AND.NSNFITOT(P)>0) THEN
            MSGTYP = MSGOFF
             CALL MPI_ISEND(
     .        ISIZENV(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD,REQ_S(P),IERROR    )
            IALLOCS = IALLOCS + ISIZENV(1,P)
          ENDIF
        END DO
        IERROR=0
        IF(IALLOCS>0)
     +    ALLOCATE(BBUFS(IALLOCS+NBINTC*NSPMD*2),STAT=IERROR) ! nbintc*NSPMD*2 majorant place supplementaire bufs
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUT(NIN)  = 0
          DEBUTI(NIN) = 1
          DEBUTE(NIN) = 0
          DEBUTIE(NIN)= 1
        END DO
C
C Send
C
        L = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC.AND.ISIZENV(1,P)>0)THEN
            ADD = L+1
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IDEB = DEBUT(NIN)
              IDEBI= DEBUTI(NIN)
              NB = NSNFI(NIN)%P(P)
              NTY  = IPARI(7,NIN)
              INTTH = IPARI(47,NIN)
            IF(MULTI_FVM%INT18_GLOBAL_LIST(NIN)) CYCLE
              IF(NTY==7.OR.NTY==10.OR.NTY==20.OR.
     *          NTY==22.OR.NTY==23.OR.NTY==24.OR.
     *          NTY==25) THEN
               LENI = LEN
               IF(NB>0) THEN
C
C rajout comm supplementaire int20
C
                IF(NTY == 20) THEN
                  DO N = 1, NB
                    BBUFS(L+1) = ALPHAKFI(NIN)%P(IDEB+N)
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      CALL PUTDPDAANC(
     .                 DAANC6FI(NIN)%P(1,1,IDEB+N),BBUFS(L+2),IRESP,INC)
C                      L  = L + INC
                    ELSE                  ! A optimiser
                      CALL PUTDPZERO(ZERODP,BBUFS(L+2),IRESP,INC)
                    ENDIF
                    L = L + LEN20
                  ENDDO
                END IF
C
                LL = L+1
                L = L + 1
C
                IF(INTTH == 0 ) THEN
                 IF(KDTINT==0)THEN
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                 ELSE
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                 ENDIF
C
C --- interface type 7 + la thermique
C
                ELSE
                 IF(NODADT_THERM ==1) THEN
                  LENI = LEN7T
                  IF(KDTINT==0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FTHESKYFI(NIN)%P(IDEBI)
                        BBUFS(L+7) = CONDNSKYFI(NIN)%P(IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN7T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                   ENDDO
                  ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FTHESKYFI(NIN)%P(IDEBI)
                        BBUFS(L+8) = CONDNSKYFI(NIN)%P(IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN7T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                   ENDDO
                  ENDIF
                 
                 ELSE ! NODADT_THERM

                  LENI = LEN7T
                  IF(KDTINT==0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FTHESKYFI(NIN)%P(IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN7T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                   ENDDO
                  ELSE
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FTHESKYFI(NIN)%P(IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN7T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                   ENDDO
                  ENDIF
                 ENDIF
                ENDIF
C
                BBUFS(LL) = (L-LL)/LENI
                DEBUT(NIN) = DEBUT(NIN) + NB
                DEBUTI(NIN)= IDEBI
               END IF
              ELSEIF(NTY==11) THEN
C type 11
               IF(INTTH == 0 ) THEN
                LENI=LEN11
                IF(NB>0) THEN
                 LL = L+1
                 L = L + 1
                 IF(KDTINT==0)THEN
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FSKYFI(NIN)%P(6,IDEBI)
                        BBUFS(L+8) = FSKYFI(NIN)%P(7,IDEBI)
                        BBUFS(L+9) = FSKYFI(NIN)%P(8,IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN11
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ELSE
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FSKYFI(NIN)%P(6,IDEBI)
                        BBUFS(L+8) = FSKYFI(NIN)%P(7,IDEBI)
                        BBUFS(L+9) = FSKYFI(NIN)%P(8,IDEBI)
                        BBUFS(L+10)= FSKYFI(NIN)%P(9,IDEBI)
                        BBUFS(L+11)= FSKYFI(NIN)%P(10,IDEBI)
                        IDEBI = IDEBI + 1
                        L = L + LEN11
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                 ENDIF
                 BBUFS(LL) = (L-LL)/LEN11
                 DEBUT(NIN) = DEBUT(NIN) + NB
                 DEBUTI(NIN)= IDEBI
                END IF
C   Type 11 + thermal modelling
               ELSE ! INTTH
                LENI=LEN11T
                IF(NODADT_THERM == 1)THEN ! Thermal time step
                 IF(NB>0) THEN
                  LL = L+1
                  L = L + 1
                  IF(KDTINT==0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FSKYFI(NIN)%P(6,IDEBI)
                        BBUFS(L+8) = FSKYFI(NIN)%P(7,IDEBI)
                        BBUFS(L+9) = FSKYFI(NIN)%P(8,IDEBI)
                        BBUFS(L+10)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+1)
                        BBUFS(L+11)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+2)
                        BBUFS(L+12)= CONDNSKYFI(NIN)%P(2*(IDEBI-1)+1)
                        BBUFS(L+13)= CONDNSKYFI(NIN)%P(2*(IDEBI-1)+2)
                        IDEBI = IDEBI + 1
                        L = L + LEN11T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ELSE
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FSKYFI(NIN)%P(6,IDEBI)
                        BBUFS(L+8) = FSKYFI(NIN)%P(7,IDEBI)
                        BBUFS(L+9) = FSKYFI(NIN)%P(8,IDEBI)
                        BBUFS(L+10)= FSKYFI(NIN)%P(9,IDEBI)
                        BBUFS(L+11)= FSKYFI(NIN)%P(10,IDEBI)
                        BBUFS(L+12)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+1)
                        BBUFS(L+13)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+2)
                        BBUFS(L+14)= CONDNSKYFI(NIN)%P(2*(IDEBI-1)+1)
                        BBUFS(L+15)= CONDNSKYFI(NIN)%P(2*(IDEBI-1)+2)
                        IDEBI = IDEBI + 1
                        L = L + LEN11T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                   ENDDO
                  ENDIF
                  BBUFS(LL) = (L-LL)/LEN11T
                  DEBUT(NIN) = DEBUT(NIN) + NB
                  DEBUTI(NIN)= IDEBI
                 END IF
               ELSE !NODADTHERM
                IF(NB>0) THEN
                  LL = L+1
                  L = L + 1
                  IF(KDTINT==0)THEN
                   DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FSKYFI(NIN)%P(6,IDEBI)
                        BBUFS(L+8) = FSKYFI(NIN)%P(7,IDEBI)
                        BBUFS(L+9) = FSKYFI(NIN)%P(8,IDEBI)
                        BBUFS(L+10)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+1)
                        BBUFS(L+11)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+2)
                        IDEBI = IDEBI + 1
                        L = L + LEN11T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ELSE
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      NOD = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=NOD
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = NOD
                        BBUFS(L+2) = FSKYFI(NIN)%P(1,IDEBI)
                        BBUFS(L+3) = FSKYFI(NIN)%P(2,IDEBI)
                        BBUFS(L+4) = FSKYFI(NIN)%P(3,IDEBI)
                        BBUFS(L+5) = FSKYFI(NIN)%P(4,IDEBI)
                        BBUFS(L+6) = FSKYFI(NIN)%P(5,IDEBI)
                        BBUFS(L+7) = FSKYFI(NIN)%P(6,IDEBI)
                        BBUFS(L+8) = FSKYFI(NIN)%P(7,IDEBI)
                        BBUFS(L+9) = FSKYFI(NIN)%P(8,IDEBI)
                        BBUFS(L+10)= FSKYFI(NIN)%P(9,IDEBI)
                        BBUFS(L+11)= FSKYFI(NIN)%P(10,IDEBI)
                        BBUFS(L+12)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+1)
                        BBUFS(L+13)= FTHESKYFI(NIN)%P(2*(IDEBI-1)+2)
                        IDEBI = IDEBI + 1
                        L = L + LEN11T
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                   ENDDO
                  ENDIF
                  BBUFS(LL) = (L-LL)/LEN11T
                  DEBUT(NIN) = DEBUT(NIN) + NB
                  DEBUTI(NIN)= IDEBI
                 END IF
                ENDIF
               ENDIF

              ELSEIF(NTY==17) THEN
C type 17
               LENI=LEN17
               IF(NB>0) THEN
                 LL = L+1
                 L = L + 1
                 DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N)<0)THEN
C facette element generant une force
                      IES = -NSVFI(NIN)%P(IDEB+N)
                      NSVFI(NIN)%P(IDEB+N)=IES
                      IF(IDEBI<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                        BBUFS(L+1) = IES
                        DO JJ=1,40
                          BBUFS(L+JJ+1)=FSKYFI(NIN)%P(JJ,IDEBI)
                        END DO
                        IDEBI = IDEBI + 1
                        L = L + LEN17
                        IF(IDEBI<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                    ENDIF
                 ENDDO
                 BBUFS(LL) = (L-LL)/LEN17
                 DEBUT(NIN) = DEBUT(NIN) + NB
                 DEBUTI(NIN)= IDEBI
               END IF
              END IF
C
C Partie supplementaire type 20 edge
C
              IF(NTY==20) THEN
                NB = NSNFIE(NIN)%P(P)
                IDEB = DEBUTE(NIN)
                IDEBI= DEBUTIE(NIN)
                IF(NB>0) THEN
                  DO N = 1, NB
                    N1 = 2*(N+IDEB-1)+1
                    N2 = 2*(N+IDEB)
                    BBUFS(L+1) = ALPHAKFIE(NIN)%P(N1)
                    BBUFS(L+2) = ALPHAKFIE(NIN)%P(N2)
                    IF(NSVFIE(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                      CALL PUTDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N1),BBUFS(L+3),IRESP,INC)
C                     L  = L + INC
                      CALL PUTDPDAANC(
     .                DAANC6FIE(NIN)%P(1,1,N2),BBUFS(L+3+INC),IRESP,
     .                INC)
C                      L  = L + INC
                    ELSE                  ! A optimiser
                      CALL PUTDPZERO(ZERODP,BBUFS(L+3),IRESP,INC)
                      CALL PUTDPZERO(ZERODP,BBUFS(L+3+INC),IRESP,INC)
                    END IF
                    L = L + LEN20E
                  END DO
C
                  LL = L+1
                  L = L + 1
                  IF(KDTINT==0)THEN
                    DO N = 1, NB
                      IF(NSVFIE(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                        NOD = -NSVFIE(NIN)%P(IDEB+N)
                        NSVFIE(NIN)%P(IDEB+N)=NOD
                        IF(IDEBI<=NISKYFIE(NIN)) THEN
                          ITEST = ISKYFIE(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        END IF
                        DO WHILE(ITEST)
                          BBUFS(L+1) = NOD
                          BBUFS(L+2) = FSKYFIE(NIN)%P(1,IDEBI)
                          BBUFS(L+3) = FSKYFIE(NIN)%P(2,IDEBI)
                          BBUFS(L+4) = FSKYFIE(NIN)%P(3,IDEBI)
                          BBUFS(L+5) = FSKYFIE(NIN)%P(4,IDEBI)
                          BBUFS(L+6) = FSKYFIE(NIN)%P(5,IDEBI)
                          BBUFS(L+7) = FSKYFIE(NIN)%P(6,IDEBI)
                          BBUFS(L+8) = FSKYFIE(NIN)%P(7,IDEBI)
                          BBUFS(L+9) = FSKYFIE(NIN)%P(8,IDEBI)
                          IDEBI = IDEBI + 1
                          L = L + LEN11
                          IF(IDEBI<=NISKYFIE(NIN)) THEN
                            ITEST = ISKYFIE(NIN)%P(IDEBI)==IDEB+N
                          ELSE
                            ITEST = .FALSE.
                          END IF
                        END DO
                      END IF
                    END DO
                  ELSE
                    DO N = 1, NB
                      IF(NSVFIE(NIN)%P(IDEB+N)<0)THEN
C noeud generant une force
                        NOD = -NSVFIE(NIN)%P(IDEB+N)
                        NSVFIE(NIN)%P(IDEB+N)=NOD
                        IF(IDEBI<=NISKYFIE(NIN)) THEN
                          ITEST = ISKYFIE(NIN)%P(IDEBI)==IDEB+N
                        ELSE
                          ITEST = .FALSE.
                        END IF
                        DO WHILE(ITEST)
                          BBUFS(L+1) = NOD
                          BBUFS(L+2) = FSKYFIE(NIN)%P(1,IDEBI)
                          BBUFS(L+3) = FSKYFIE(NIN)%P(2,IDEBI)
                          BBUFS(L+4) = FSKYFIE(NIN)%P(3,IDEBI)
                          BBUFS(L+5) = FSKYFIE(NIN)%P(4,IDEBI)
                          BBUFS(L+6) = FSKYFIE(NIN)%P(5,IDEBI)
                          BBUFS(L+7) = FSKYFIE(NIN)%P(6,IDEBI)
                          BBUFS(L+8) = FSKYFIE(NIN)%P(7,IDEBI)
                          BBUFS(L+9) = FSKYFIE(NIN)%P(8,IDEBI)
                          BBUFS(L+10)= FSKYFIE(NIN)%P(9,IDEBI)
                          BBUFS(L+11)= FSKYFIE(NIN)%P(10,IDEBI)
                          IDEBI = IDEBI + 1
                          L = L + LEN11
                          IF(IDEBI<=NISKYFIE(NIN)) THEN
                            ITEST = ISKYFIE(NIN)%P(IDEBI)==IDEB+N
                          ELSE
                            ITEST = .FALSE.
                          END IF
                        END DO
                      END IF
                    END DO
                  END IF
                  BBUFS(LL) = (L-LL)/LEN11
                  DEBUTE(NIN) = DEBUTE(NIN) + NB
                  DEBUTIE(NIN)= IDEBI
                END IF
              END IF
C Fin type 20 edge
C
C TYPE25 edge
C
              IF(NTY == 25) THEN
                IF( NSNFIE(NIN)%P(P) > 0) THEN
                  NB = NSNFIE(NIN)%P(P)
                  IDEB = DEBUTE(NIN)
                  IDEBI= DEBUTIE(NIN)
C                 L0 = L
                  LL = L + 1
                  L = L  + 1
                  NB_TOT_EDGES = 0 
                  IF(NB>0) THEN
                      DO N = 1, NB
                        DEBUG_E2E(LEDGE_FIE(NIN)%P(E_GLOBAL_ID,IDEB+N) == D_ES,IDEB)
                        IF(NSVFIE(NIN)%P(IDEB+N)<0)THEN
C Node generating the force
                          NOD = -NSVFIE(NIN)%P(IDEB+N)
                          NSVFIE(NIN)%P(IDEB+N)=NOD
                          IF(IDEBI<=NISKYFIE(NIN)) THEN
                            ITEST = ISKYFIE(NIN)%P(IDEBI)==IDEB+N
                          ELSE
                            ITEST = .FALSE.
                          END IF
                          THOFFSET = 0
                          IF(INTTH > 0) THEN
                            THOFFSET = 1
                            ! Not available yet
                            ASSERT(.FALSE.)
                          ENDIF
                          DO WHILE(ITEST)
                            BBUFS(L+1)  = NOD
                            BBUFS(L+2)  = FSKYFIE(NIN)%P(1,IDEBI)
                            BBUFS(L+3)  = FSKYFIE(NIN)%P(2,IDEBI)
                            BBUFS(L+4)  = FSKYFIE(NIN)%P(3,IDEBI)
                            BBUFS(L+5)  = FSKYFIE(NIN)%P(4,IDEBI)
                            BBUFS(L+6+THOFFSET)  = NOD
                            BBUFS(L+7+THOFFSET)  = FSKYFIE(NIN)%P(5+THOFFSET,IDEBI)
                            BBUFS(L+8+THOFFSET)  = FSKYFIE(NIN)%P(6+THOFFSET,IDEBI)
                            BBUFS(L+9+THOFFSET)  = FSKYFIE(NIN)%P(7+THOFFSET,IDEBI)
                            BBUFS(L+10+THOFFSET) = FSKYFIE(NIN)%P(8+THOFFSET,IDEBI)
                            IDEBI = IDEBI + 1
                            NB_TOT_EDGES = NB_TOT_EDGES + 1 
                            L = L + LEN25E ! + 2*THOFFSET
                            IF(IDEBI<=NISKYFIE(NIN)) THEN
                              ITEST = ISKYFIE(NIN)%P(IDEBI)==IDEB+N
                            ELSE
                              ITEST = .FALSE.
                            END IF
                          END DO
                        END IF
                     END DO
                  END IF
                  ! LL = L0 +1 
                  ! L = L0 +1 + LEN25 * (NBEDGES_WITH_FORCES)
                  ! L - LL = LEN25 * NB_TOT_EDGES / LEN25 = NB_TOT_EDGES
                  ! BBUFS(L0 + 1 ) = NB_TOT_EDGES
                  BBUFS(LL) = NB_TOT_EDGES
                  ASSERT( (L-LL)/LEN25E == NB_TOT_EDGES)
                  DEBUTE(NIN) = DEBUTE(NIN) + NB
                  DEBUTIE(NIN)= IDEBI
                END IF !TYPE 25 E2E
              ENDIF
            END DO
            SIZ = L+1-ADD
            MSGTYP = MSGOFF2
            CALL MPI_ISEND(
     .        BBUFS(ADD),SIZ,REAL     ,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD,REQ_SI(P),IERROR    )
          ELSEIF(P/=LOC_PROC)THEN
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              DEBUT(NIN) = DEBUT(NIN) + NSNFI(NIN)%P(P)
              IF(IPARI(7,NIN)==20)
     .          DEBUTE(NIN) = DEBUTE(NIN) + NSNFIE(NIN)%P(P)
             IF(IPARI(7,NIN)==25)
     .          DEBUTE(NIN) = DEBUTE(NIN) + NSNFIE(NIN)%P(P)
            ENDDO
          ENDIF
        ENDDO
C
C mise a 0 de niskyfi une fois utilisee
C
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          NISKYFI(NIN)  = 0
          NISKYFIE(NIN) = 0
        ENDDO
C
      ELSEIF(IFLAG==2)THEN
C
C Utile int20
C
        IF(NBINT20>0)THEN
          DO II = 1, NBINTC
            NIN = INTLIST(II)
            DEBUT(NIN)  = 0
            DEBUTE(NIN) = 0
          END DO
        END IF
C
C Receive 1er message : taille communication
C
        IALLOCR = 0
        DO P = 1, NSPMD
          IF(NSNSITOT(P)>0)THEN
            CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
            IALLOCR = MAX(IALLOCR,ISIZRCV(1,P))   ! pour comm bloquantes
C           IALLOCR = IALLOCR + ISIZRCV(P)     ! pour comm non bloquantes
          END IF
        END DO
C
        IERROR=0
        IF(IALLOCR>0)
     .    ALLOCATE(BBUFR(IALLOCR+NBINTC*2),STAT=IERROR)
C     .    ALLOCATE(BBUFR(IALLOCR+NBINTC*NSPMD*2),STAT=IERROR) ! si comm non bloquantes reactivees

        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF

C ------------------------------------------------------------
C ISKY / FSKYI Space Verification. If not enough increase it.
C ------------------------------------------------------------
        LSKYI_CT=0
        DO P=1,NSPMD
          LSKYI_CT=LSKYI_CT+ISIZRCV(2,P)
        ENDDO
C
        IF ( NISKY+LSKYI_CT > SISKY) THEN
           CALL REALLOCATE_I_SKYLINE(LSKYI_CT,3)
        ENDIF
C
C Reception buffer et decompactage
C
C        L = 1       ! a reactiver si envoi non bloquant
        DO P = 1, NSPMD
          IF(ISIZRCV(1,P)>0) THEN
            MSGTYP = MSGOFF2
            L = 1           ! envoi bloquant + opti alloc memoire sur max des comm
            CALL MPI_RECV(
     .        BBUFR(L),ISIZRCV(1,P)+NBINTC*2,REAL  ,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD            ,STATUS,IERROR    )
             
C
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NTY =IPARI(7,NIN)

              IF(MULTI_FVM%INT18_GLOBAL_LIST(NIN)) CYCLE
              CONDITION = (NSNSI(NIN)%P(P) > 0)
              IF((NTY == 25) .AND.  (.NOT. CONDITION) ) THEN
                IF(IPARI(58,NIN) /= 0) THEN
                   CONDITION = (NSNSIE(NIN)%P(P) > 0)
                ENDIF
              ENDIF

              IF(CONDITION) THEN
                IBC   =IPARI(11,NIN)
                NOINT =IPARI(15,NIN)
                INACTI=IPARI(22,NIN)
                ISECIN=IPARI(28,NIN)
                IBAG  =IPARI(32,NIN)
                IADM  =IPARI(44,NIN)
                INTTH = IPARI(47,NIN)
C type int20 (non edge)
                IF(NTY == 20) THEN
                  NB = NSNSI(NIN)%P(P)
                  IDEB = DEBUT(NIN)
                  CALL SPMD_FIADD20_PON(
     1              NB,LEN20,NSVSI(NIN)%P(IDEB+1),BBUFR(L),
     2              INTBUF_TAB(NIN)%DAANC6,INTBUF_TAB(NIN)%NSV,INTBUF_TAB(NIN)%ALPHAK)
                  L = L + NB*LEN20
                  DEBUT(NIN) = DEBUT(NIN) + NB

                  NB = NINT(BBUFR(L))
                  L = L + 1
                  IF(INTTH == 0) THEN
                     CALL SPMD_FIADD20F_PON(
     1                NB      ,LEN    ,BBUFR(L),INTBUF_TAB(NIN)%NSV,FSKYI  ,
     2                ISKY    ,IBC    ,ISECIN  ,NOINT        ,IBAG   ,
     3                ICODT   ,SECFCUM,NSTRF   ,ICONTACT     ,FCONT  ,
     4                INACTI  ,IADM   ,INTTH  ,FTHESKYI,INTBUF_TAB(NIN)%NLG,
     5                H3D_DATA)
                     L = L + NB*LEN
                  ELSE
                     CALL SPMD_FIADD20F_PON(
     1                NB      ,LEN7T    ,BBUFR(L),INTBUF_TAB(NIN)%NSV,FSKYI ,
     2                ISKY    ,IBC    ,ISECIN  ,NOINT        ,IBAG   ,
     3                ICODT   ,SECFCUM,NSTRF   ,ICONTACT     ,FCONT  ,
     4                INACTI  ,IADM   ,INTTH   ,FTHESKYI ,INTBUF_TAB(NIN)%NLG,
     5                H3D_DATA)
                      L = L + NB*LEN7T
                  ENDIF
                ELSE IF(NTY==7.OR.NTY==10.OR.NTY==22.OR.
     *                  NTY==23.OR.NTY==24.OR.NTY==25)THEN


                    IF(NSNSI(NIN)%P(P) > 0) THEN
                    NB = NINT(BBUFR(L))


                    L = L + 1
                    IF(INTTH == 0) THEN
                     CALL SPMD_FIADD_PON(
     1                NB      ,LEN    ,BBUFR(L),INTBUF_TAB(NIN)%NSV, FSKYI,
     2                ISKY    ,IBC    ,ISECIN  ,NOINT        ,IBAG   ,
     3                ICODT   ,SECFCUM,NSTRF   ,ICONTACT     ,FCONT  ,
     4                INACTI  ,IADM   ,INTTH   , FTHESKYI    ,CONDNSKYI,
     5                H3D_DATA,NIN    ,TAGNCONT,KLOADPINTER  ,LOADPINTER,
     6                LOADP_HYD_INTER)
                     L = L + NB*LEN
                    ELSE
                     CALL SPMD_FIADD_PON(
     1                NB      ,LEN7T    ,BBUFR(L),INTBUF_TAB(NIN)%NSV, FSKYI,
     2                ISKY    ,IBC    ,ISECIN  ,NOINT        ,IBAG   ,
     3                ICODT   ,SECFCUM,NSTRF   ,ICONTACT     ,FCONT  ,
     4                INACTI  ,IADM   ,INTTH   , FTHESKYI    ,CONDNSKYI,
     5                H3D_DATA,NIN    ,TAGNCONT, KLOADPINTER ,LOADPINTER,
     6                LOADP_HYD_INTER)
                      L = L + NB*LEN7T
                    ENDIF
                    ENDIF ! NSNSI

                    IF(NTY == 25 ) THEN
                      IF( NSNSIE(NIN)%P(P) > 0 ) THEN
                        NB = NINT(BBUFR(L)) ! number of EDGES
                        L = L + 1
                        LENI = LEN25E
                        IF(NB > 0) THEN
                          CALL SPMD_FIADD25E_PON(
     1                     NB      ,LENI    ,BBUFR(L),NSVSIE(NIN)%P, FSKYI,
     2                     ISKY    ,IBC    ,ISECIN  ,NOINT        ,IBAG   ,
     3                     ICODT   ,SECFCUM,NSTRF   ,ICONTACT     ,FCONT  ,
     4                     INACTI  ,IADM   ,INTTH   , FTHESKYI    ,CONDNSKYI,
     5                     H3D_DATA,INTBUF_TAB(NIN)%LEDGE,NLEDGE,IPARI(68,NIN),
     6                     NIN     ,TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
                           L = L + NB*LENI
                        ENDIF
                      ENDIF
                    ENDIF

                
                ELSEIF(NTY==11)THEN
                    NB = NINT(BBUFR(L))
                    L = L + 1
                  IF(INTTH == 0) THEN
                    CALL SPMD_FIADD11_PON(
     1                NB     ,LEN11   ,BBUFR(L),INTBUF_TAB(NIN)%IRECTS,FSKYI  ,
     2                ISKY   ,IBC     ,ISECIN  ,NOINT        ,IBAG   ,
     3                ICODT  ,SECFCUM ,NSTRF   ,ICONTACT     ,FCONT  ,
     4                INTTH  ,FTHESKYI,CONDNSKYI,H3D_DATA    ,NIN    ,TAGNCONT ,
     5                 KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
                    L = L + NB*LEN11
                  ELSE
                     CALL SPMD_FIADD11_PON(
     1                NB     ,LEN11T   ,BBUFR(L),INTBUF_TAB(NIN)%IRECTS,FSKYI  ,
     2                ISKY   ,IBC     ,ISECIN  ,NOINT        ,IBAG   ,
     3                ICODT  ,SECFCUM ,NSTRF   ,ICONTACT     ,FCONT  ,
     4                INTTH  ,FTHESKYI,CONDNSKYI,H3D_DATA    ,NIN    ,TAGNCONT,
     5                KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
                    L = L + NB*LEN11T
                  ENDIF
                ELSEIF(NTY==17)THEN
                    NB = NINT(BBUFR(L))
                    L = L + 1
                    IGE = IPARI(34,NIN)
                    IGN = IPARI(36,NIN)
                    NME = IGRBRIC(IGE)%NENTITY
                    NMES= IGRBRIC(IGN)%NENTITY
C
                    CALL SPMD_FIADD17_PON(
     1                NB    ,LEN17  ,BBUFR(L),IGRBRIC(IGN)%ENTITY,FSKYI ,
     2                ISKY  ,FCONT  ,IXS     ,IXS16       ,H3D_DATA)
                    L = L + NB*LEN17
                END IF
              ENDIF
C
C Partie supplementaire type 20 edge
C
              IF(NTY == 20) THEN
                NB = NSNSIE(NIN)%P(P)
                IF(NB>0)THEN
                  IBC   =IPARI(11,NIN)
                  NOINT =IPARI(15,NIN)
                  ISECIN=IPARI(28,NIN)
C                  IBAG  =IPARI(32,NIN)
C IBAG force a 0 pour la partie edge
                  IBAG=0
                  IDEB = DEBUTE(NIN)
                  CALL SPMD_FIADD20E_PON(
     1              NB,LEN20E,NSVSIE(NIN)%P(IDEB+1),BBUFR(L),
     2              INTBUF_TAB(NIN)%DAANC6,INTBUF_TAB(NIN)%IXLINS,INTBUF_TAB(NIN)%ALPHAK)
                  L = L + NB*LEN20E
                  DEBUTE(NIN) = DEBUTE(NIN) + NB
C
                  NB = NINT(BBUFR(L))
                  L = L + 1
                  IF(NB > 0) THEN
                  CALL SPMD_FIADD20FE_PON(
     1              NB     ,LEN11   ,BBUFR(L),INTBUF_TAB(NIN)%IXLINS,FSKYI  ,
     2              ISKY   ,IBC     ,ISECIN  ,NOINT        ,IBAG   ,
     3              ICODT  ,SECFCUM ,NSTRF   ,ICONTACT     ,FCONT  ,
     4              INTBUF_TAB(NIN)%NLG,H3D_DATA)
                  ENDIF
                  L = L + NB*LEN11
                END IF
C Fin type 20 edge
              END IF
            ENDDO
          ENDIF
        ENDDO
        IF(IALLOCR>0) DEALLOCATE(BBUFR)
C
C Attente ISEND
C
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            IF(NSNFITOT(P)>0) THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
            END IF
            IF(ISIZENV(1,P)>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            END IF
          END IF
        END DO
        IF(IALLOCS>0) DEALLOCATE(BBUFS)
      ENDIF
C
#endif
      RETURN
      END
