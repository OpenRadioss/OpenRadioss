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
Chd|  W_FI                          source/restart/ddsplit/w_fi.F 
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        I25_FIE_MOD                   share/modules1/i25_fie_mod.F  
Chd|        I7I10SPLITMOD                 share/modules1/i710split_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE W_FI(IPARI,PROC,LEN_IA,
     1                INTERCEP ,INTBUF_TAB,ITAB,MULTI_FVM,TAG,
     2                NINDX_TAG,INDX_TAG  ,NODLOCAL,NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I7I10SPLITMOD 
      USE FRONT_MOD
      USE INTBUFDEF_MOD 
      USE MULTI_FVM_MOD
      USE I25_FIE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "assert.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
      TYPE INTERMASURFEP
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE INTERMASURFEP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  PROC, IPARI(NPARI,*), LEN_IA, ITAB(*)
      INTEGER :: NINDX_TAG,NUMNOD_L
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL
      INTEGER, DIMENSION(*), INTENT(INOUT) :: TAG,INDX_TAG
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NINDX_TAG : integer, 
!                   number of non-zero value of TAG
!       TAG       : integer, dimension=NUMNOD + I24MAXNSNE + SIZE_FVM
!                                                  |            |
!                       max NSNE for TYP24     ____|            |
!          SIZE_FVM=NUMELS for INACTI=7 and FVM, otherwise 0 ___|
!                   tagged array, TAG is TAG_SCRATCH array, allocated in lectur.F 
!       INDX_TAG  : integer, dimension = dimension(TAG)
!                   index array, used to flush TAG to 0
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NI, P, K, L, ITYP, INACTI, NSN, NMN, NRTS, NRTM,
     .        N, N1, N2, N3, N4, NRTM_L,
     .        I_STOK, NODFI, E, MULTIMP, IDEB, IFQ,
     .        NISUB, NISUBS, NISUBM, INTTH, NL, N1L, N2L, N3L, N4L,
     .        NLINS, NLINM, NSNE,  NMNE, NLN, NN,INTFRIC,
     .        NSNFI(NSPMD), NUMP(NSPMD),NMNFI(NSPMD),
     .        WORK(70000),KD(50),JD(50),I,J,IEDGE4,INTNITSCHE,MY_NODE,
     .        CPT1,CPT2,P1,P2,PROC1,PROC2,SE1,PLOC,ND,FLAGREMN,LREMNORMAX
      INTEGER :: IEDGE,NEDGE,NEDGE_KEPT,ILEV
      INTEGER, PARAMETER :: E_IBUF_SIZE = 13
      LOGICAL INACTI_CASE

      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGE, NSNLOCAL, NSNP, NSVFI,  
     .                                     NRTSLOCAL, NRTSP, INDEX, PFI,
     .                                     ITAFI, ITAFI2
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: CLEF,IRTLMFI2,IRTLMFI

      INTEGER, DIMENSION(:),ALLOCATABLE :: PLIST
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAB1,TAB2
      INTEGER SPLIST
      INTEGER, DIMENSION(:),ALLOCATABLE :: TABZERO
      INTEGER, DIMENSION(:), ALLOCATABLE :: LEDGE_FIE
C-----------------------------------------------
C

      DO NI =1, NINTER
        NINDX_TAG = 0
        ITYP = IPARI(7,NI)
C
        IF(ITYP==7.OR.ITYP==10.OR.ITYP==11.OR.
     .    (ITYP==17.AND.IPARI(33,NI)==0).OR.ITYP==20.OR.
     .    ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.ITYP==25)THEN
C   ecriture nsnsi : 0
          DO P = 1, NSPMD
            NSNFI(P) = 0
          END DO
          CALL WRITE_I_C(NSNFI,NSPMD) ! NSNSI at engine
          LEN_IA = LEN_IA + NSPMD
          INACTI = IPARI(22,NI)
          IF(((INACTI/=5.AND.INACTI/=6.AND.INACTI/=7).OR.
     .        ITYP==10).AND.ITYP/=23.AND.INACTI/=-1) THEN
C   ecriture nsnfi : 0 si inacti # 5, 6 et 7
            CALL WRITE_I_C(NSNFI,NSPMD) ! nsnfi at engine
            LEN_IA = LEN_IA + NSPMD
          ELSE
           NRTS   = IPARI(3,NI)
           NRTM   = IPARI(4,NI)
           NSN    = IPARI(5,NI)
           NMN    = IPARI(6,NI)
           MULTIMP= IPARI(23,NI)
           INTTH  = IPARI(47,NI)
           INTFRIC = IPARI(72,NI)
           FLAGREMN =IPARI(63,NI)
           LREMNORMAX =IPARI(82,NI)
           INTNITSCHE = IPARI(86,NI)
  
           NSNE  = 0
           IEDGE4 = 0
! +++++++++++++++++++++++++++++++++++++++++++++++
           IF(ITYP==7.OR.ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     .        ITYP==25)THEN

            IF(ITYP==24) THEN
               NSNE  = IPARI(55,NI)
               IEDGE4 = IPARI(59,NI)
            ENDIF
            I_STOK = INTBUF_TAB(NI)%I_STOK(1)
!            IF (MULTI_FVM%IS_USED .AND. INACTI == 7) THEN
!C     Interface type 18 for law151
!               ALLOCATE(TAG(NUMNOD+NSNE+NUMELS))
!               TAG(1:NUMNOD+NSNE+NUMELS) = 0
!            ELSE
!               ALLOCATE(TAG(NUMNOD+NSNE))
!               TAG(1:NUMNOD+NSNE) = 0
!            ENDIF
            ALLOCATE(NSNLOCAL(NSN))
            ALLOCATE(NSNP(NSN))
            ALLOCATE(NSVFI(NSN))
            ALLOCATE(PFI(NSN))
            ALLOCATE(ITAFI(NSN))
            ALLOCATE(TAGE(NRTM))
            IF (ITYP==24) ALLOCATE(IRTLMFI(2,NSN))
            IF (ITYP==25) THEN
              ALLOCATE(IRTLMFI(4,NSN))
              IRTLMFI(1:4,1:NSN)=0
            END IF
            DO P = 1, NSPMD
              NUMP(P) = 0
            END DO
C
C
C
! optimize loop with PLIST tool
            ALLOCATE(PLIST(NSPMD)) 
            PLIST(1:NSPMD) = -1
               DO K=1,NSN
                  N = INTBUF_TAB(NI)%NSV(K)
                  NSNLOCAL(K) = 0
                  IF(TAG(N)==0) THEN 
                     SPLIST=0 
                     IF(N<=NUMNOD) THEN
                        CALL PLIST_IFRONT(PLIST,N,SPLIST) 
                        PLOC = 0
                        IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) PLOC = 1
                     ELSE
C     T24 E2E Fictive nodes are only on 1 domain (But they are not nodes
                        PLOC = 0
                        SE1 = INTBUF_TAB(NI)%IS2SE(2*(N-NUMNOD-1)+1)
                        PLIST(1)=INTERCEP(2,NI)%P(SE1)
                        SPLIST=1
                        IF(PLIST(1)==PROC+1)PLOC=1
                     ENDIF
                     DO I=1,SPLIST
                        P=PLIST(I)
                        NUMP(P) = NUMP(P)+1
                     ENDDO

                     IF(PLOC==1) THEN
                        NSNLOCAL(K) = NUMP(PROC+1)
                        NSNP(K) = PROC+1
                     ELSE
                        P = PLIST(1)
                        NSNLOCAL(K) = NUMP(P)
                        NSNP(K) = P
                     ENDIF
                     TAG(N) = 1
                     NINDX_TAG = NINDX_TAG + 1
                     INDX_TAG(NINDX_TAG) = N
                  ENDIF
               ENDDO

            DEALLOCATE(PLIST) 

            NRTM_L = 0
            DO K=1,NRTM
C TAGE flag servant pour inacti
              TAGE(K) = 0
              IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
                NRTM_L = NRTM_L + 1
                TAGE(K) = NRTM_L
              ENDIF  
              IF (ITYP==25) THEN 
                IF(INTERCEP(2,NI)%P(K) > 0.AND.INTERCEP(2,NI)%P(K)/=INTERCEP(1,NI)%P(K)) THEN
                  IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN
                    NRTM_L = NRTM_L + 1
                    TAGE(K) = NRTM_L
                  ENDIF 
                ENDIF    
              ENDIF    
            ENDDO
C
            NODFI = 0
            DO P = 1, NSPMD
              NSNFI(P) = 0
            END DO
C
            DO K = 1, I_STOK
              E = INTBUF_TAB(NI)%CAND_E(K)
              IF (TAGE(E)/=0) THEN
                N = INTBUF_TAB(NI)%CAND_N(K)
C Add boolean in order to treat INACTI=5 Candidates
                INACTI_CASE = .FALSE.
                ND = INTBUF_TAB(NI)%NSV(N)
                IF (ND <= NUMNOD)THEN
                  MY_NODE = INTBUF_TAB(NI)%NSV(N)
                  IF(NODLOCAL( MY_NODE )==0.OR.NODLOCAL( MY_NODE )>NUMNOD_L ) INACTI_CASE=.TRUE.
                ELSE
                  SE1 = INTBUF_TAB(NI)%IS2SE(2*(ND-NUMNOD-1)+1)
                  IF (INTERCEP(2,NI)%P(SE1)/=(PROC+1) ) INACTI_CASE=.TRUE.
                ENDIF 
c                IF (INTBUF_TAB(NI)%NSV(N) > NUMNOD) CYCLE
                 
                IF(INACTI_CASE .EQV. .TRUE.) THEN
C ne traiter qu une seule fois les noeuds
                  IF(NSNP(N)>0) THEN
                    P = NSNP(N)
                    NSNP(N) = -P
                    NSNFI(P) = NSNFI(P) + 1
                    NODFI = NODFI + 1
                    NSVFI(NODFI) = NSNLOCAL(N)
                    ND = INTBUF_TAB(NI)%NSV(N)
                    IF(ND<=NUMNOD)THEN
                      ITAFI(NODFI) = ITAB(ND)
                    ELSE
                      ITAFI(NODFI) = INTBUF_TAB(NI)%IS2ID(ND-NUMNOD)
                    ENDIF
                    IF(ITYP==24)THEN
                      IRTLMFI(1,NODFI)=INTBUF_TAB(NI)%IRTLM(2*(N-1)+1)
                      IRTLMFI(2,NODFI)=INTBUF_TAB(NI)%IRTLM(2*(N-1)+2)
                    ENDIF
                    IF(ITYP==25)THEN
                      IRTLMFI(1,NODFI)=INTBUF_TAB(NI)%IRTLM(4*(N-1)+1)
                      IRTLMFI(2,NODFI)=INTBUF_TAB(NI)%IRTLM(4*(N-1)+2)
                      IRTLMFI(3,NODFI)=INTBUF_TAB(NI)%IRTLM(4*(N-1)+3)
                      IRTLMFI(4,NODFI)=INTBUF_TAB(NI)%IRTLM(4*(N-1)+4)
                    ENDIF
                    PFI(NODFI) = P
                  END IF
                END IF
              END IF
            END DO
C

            IF(NODFI>0) THEN
              ALLOCATE(INDEX(2*NODFI))
              ALLOCATE(CLEF(2,NODFI))
              IF(ITYP==24)ALLOCATE(IRTLMFI2(2,NODFI))
              IF(ITYP==25)ALLOCATE(IRTLMFI2(4,NODFI))
              ALLOCATE(ITAFI2(NODFI))
              DO K = 1, NODFI
                CLEF(1,K)=PFI(K)
                CLEF(2,K)=NSVFI(K)
                ITAFI2(K) = ITAFI(K)
                IF(ITYP==24)THEN
                  IRTLMFI2(1,K)=  IRTLMFI(1,K)
                  IRTLMFI2(2,K)=  IRTLMFI(2,K)
                ENDIF
                IF(ITYP==25)THEN
                  IRTLMFI2(1,K)=  IRTLMFI(1,K)
                  IRTLMFI2(2,K)=  IRTLMFI(2,K)
                  IRTLMFI2(3,K)=  IRTLMFI(3,K)
                  IRTLMFI2(4,K)=  IRTLMFI(4,K)
                ENDIF
              END DO
              CALL MY_ORDERS(0,WORK,CLEF,INDEX,NODFI,2)
              DO K = 1, NODFI
                NSVFI(K) = CLEF(2,INDEX(K))
                ITAFI(K) = ITAFI2(INDEX(K))
                PFI(K) = CLEF(1,INDEX(K))
                IF(ITYP==24)THEN
                  IRTLMFI(1,K)=IRTLMFI2(1,INDEX(K))
                  IRTLMFI(2,K)=IRTLMFI2(2,INDEX(K))  
                ENDIF
                IF(ITYP==25)THEN
                  IRTLMFI(1,K)=IRTLMFI2(1,INDEX(K))
                  IRTLMFI(2,K)=IRTLMFI2(2,INDEX(K))
                  IRTLMFI(3,K)=IRTLMFI2(3,INDEX(K))
                  IRTLMFI(4,K)=IRTLMFI2(4,INDEX(K))
                ENDIF
              END DO
              DEALLOCATE(INDEX)
              DEALLOCATE(CLEF)
              DEALLOCATE(ITAFI2)
              IF(ITYP==24.OR.ITYP==25) DEALLOCATE(IRTLMFI2)
            END IF
            DEALLOCATE(PFI)
C
            CALL WRITE_I_C(NSNFI,NSPMD) ! NSNFI
            LEN_IA = LEN_IA + NSPMD
            CALL WRITE_I_C(NSVFI,NODFI) !NSVFI
            LEN_IA = LEN_IA + NODFI

            CALL WRITE_I_C(ITAFI,NODFI) !ITAFI
            LEN_IA = LEN_IA + NODFI
C   ecriture bidon kinfi
            CALL WRITE_I_C(NSVFI,NODFI)
            LEN_IA = LEN_IA + NODFI
C   Void writing for MATSFI
            IF (INTTH>0) THEN
               CALL WRITE_I_C(NSVFI,NODFI)
               LEN_IA = LEN_IA + NODFI
            ENDIF
C   Void writing for IPARTFRICSFI
            IF (INTFRIC>0) THEN
               CALL WRITE_I_C(NSVFI,NODFI)
               LEN_IA = LEN_IA + NODFI
            ENDIF
            IF(ITYP==24)THEN
C   write dummy irtlm_fi = 2xnodfi 
              CALL WRITE_I_C(IRTLMFI,NODFI*2)
              LEN_IA = LEN_IA + NODFI*2
C   Write dummy ICONT_I_FI
              CALL WRITE_I_C(NSVFI,NODFI)
              LEN_IA = LEN_IA + NODFI
C Write Dummy ISEDGE_FI
              CALL WRITE_I_C(NSVFI,NODFI)
              LEN_IA = LEN_IA + NODFI
             IF(IEDGE4>0)THEN
C Write dummy IRTSE_FI : 5*NODFI
                CALL WRITE_I_C(NSVFI,NODFI)
                CALL WRITE_I_C(NSVFI,NODFI)
                CALL WRITE_I_C(NSVFI,NODFI)
                CALL WRITE_I_C(NSVFI,NODFI)
                CALL WRITE_I_C(NSVFI,NODFI)
              LEN_IA = LEN_IA + 5*NODFI
            
C Write dummy IS2PT_FI
                CALL WRITE_I_C(NSVFI,NODFI)
                LEN_IA = LEN_IA + NODFI
C Write dummy ISPT2_FI   - to be modified in case INACT - ISPT2 must also be initialized
                CALL WRITE_I_C(NSVFI,NODFI)
                LEN_IA = LEN_IA + NODFI
C Write dummy ISEGPT_FI
                CALL WRITE_I_C(NSVFI,NODFI)
                LEN_IA = LEN_IA + NODFI
C Write dummy IS2SE_FI : 2*NODFI
                CALL WRITE_I_C(NSVFI,NODFI)
                CALL WRITE_I_C(NSVFI,NODFI)
                LEN_IA = LEN_IA + 2* NODFI
                 
              ENDIF
             IF(INTNITSCHE>0) CALL WRITE_I_C(NSVFI,3*NODFI)
            ENDIF

            IF(ITYP==25)THEN
C   write dummy PMAINFI
              CALL WRITE_I_C(NSVFI,NODFI)
              LEN_IA = LEN_IA + NODFI
C   write dummy irtlm_fi = 4xnodfi 
              CALL WRITE_I_C(IRTLMFI,NODFI*4)
              LEN_IA = LEN_IA + NODFI*4
C   Write dummy ICONT_I_FI
              CALL WRITE_I_C(NSVFI,NODFI)
              LEN_IA = LEN_IA + NODFI
C   Write dummy KREMNORF REMNORFI
              IF(FLAGREMN == 2.AND.NODFI>0) THEN
                 ALLOCATE(TABZERO(NODFI+1))  
                 TABZERO(1:NODFI+1) = 0
                 CALL WRITE_I_C(TABZERO,NODFI+1)
                 LEN_IA = LEN_IA + NODFI + 1
                 DEALLOCATE(TABZERO)
              ENDIF

            ENDIF
C
!            DEALLOCATE(TAG)
            DEALLOCATE(NSNLOCAL)
            DEALLOCATE(NSNP)
            DEALLOCATE(NSVFI)
            DEALLOCATE(ITAFI)
            DEALLOCATE(TAGE)
            IF (ITYP==24.OR.ITYP==25)DEALLOCATE(IRTLMFI)
C fin type 7
! +++++++++++++++++++++++++++++++++++++++++++++++
           ELSEIF(ITYP==11)THEN

            I_STOK = INTBUF_TAB(NI)%I_STOK(1)
            ALLOCATE(NRTSLOCAL(NRTS))
            ALLOCATE(NRTSP(NRTS))
            ALLOCATE(NSVFI(NRTS))
            ALLOCATE(PFI(NRTS))
            ALLOCATE(TAGE(NRTM))
            DO P = 1, NSPMD
              NUMP(P) = 0
            END DO
!
            ALLOCATE(TAB1(NSPMD),TAB2(NSPMD))
            DO K=1,NRTS
              N1 = INTBUF_TAB(NI)%IRECTS(2*(K-1)+1)
              N2 = INTBUF_TAB(NI)%IRECTS(2*(K-1)+2)

              CALL PLIST_IFRONT(TAB1,N1,CPT1)
              CALL PLIST_IFRONT(TAB2,N2,CPT2)

              NRTSLOCAL(K) = 0
              IF(CPT1>0.AND.CPT2>0) THEN
               DO P1 = 1,CPT1
                PROC1 = TAB1(P1) 
                DO P2 = 1,CPT2
                 PROC2 = TAB1(P2)
                 IF((PROC1==PROC+1).AND.(PROC2==PROC+1)) THEN
                  NUMP(PROC+1) = NUMP(PROC+1) + 1
                  NRTSLOCAL(K) = NUMP(PROC+1)
                  NRTSP(K) = PROC+1
                 ELSEIF((PROC1==PROC2).AND.(NRTSLOCAL(K)==0)) THEN
                  NUMP(PROC1) = NUMP(PROC1) + 1
                  NRTSLOCAL(K) = NUMP(PROC1)
                  NRTSP(K) = PROC1
                 ENDIF
                ENDDO
               ENDDO
              ENDIF                 
C
            END DO
            DEALLOCATE(TAB1,TAB2)
C
            NRTM_L = 0
            DO K=1,NRTM
              TAGE(K) = 0
              IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
                NRTM_L = NRTM_L + 1
                TAGE(K) = NRTM_L
              ENDIF
            ENDDO
C
            NODFI = 0
            DO P = 1, NSPMD
              NSNFI(P) = 0
            END DO
C
            DO K = 1, I_STOK
              E = INTBUF_TAB(NI)%CAND_E(K)
              IF (TAGE(E)/=0) THEN
                L  = INTBUF_TAB(NI)%CAND_N(K)
                N1 = INTBUF_TAB(NI)%IRECTS((L-1)*2+1)
                N2 = INTBUF_TAB(NI)%IRECTS((L-1)*2+2)
                 IF((NODLOCAL( N1 )==0.OR.NODLOCAL( N1 )>NUMNOD_L).AND.
     +             (NODLOCAL( N2 )==0.OR.NODLOCAL( N2 )>NUMNOD_L) ) THEN
                 IF(NRTSP(L)>0)THEN
                   P = NRTSP(L)
                   NRTSP(L) = -P
                   NSNFI(P) = NSNFI(P) + 1
                   NODFI = NODFI + 1
                   NSVFI(NODFI) = NRTSLOCAL(L)
                   PFI(NODFI) = P
                 END IF
                END IF
              END IF
            END DO
C
            IF(NODFI>0) THEN
              ALLOCATE(INDEX(2*NODFI))
              ALLOCATE(CLEF(2,NODFI))
              DO K = 1, NODFI
                CLEF(1,K)=PFI(K)
                CLEF(2,K)=NSVFI(K)
              END DO
              CALL MY_ORDERS(0,WORK,CLEF,INDEX,NODFI,2)
              DO K = 1, NODFI
                NSVFI(K) = CLEF(2,INDEX(K))
                PFI(K) = CLEF(1,INDEX(K)) 
              END DO
              DEALLOCATE(INDEX)
              DEALLOCATE(CLEF)
            END IF
            DEALLOCATE(PFI)
C
            CALL WRITE_I_C(NSNFI,NSPMD)
            CALL WRITE_I_C(NSVFI,NODFI)
C   ecriture bidon itafi n1
            CALL WRITE_I_C(NSVFI,NODFI)
C   ecriture bidon itafi n2
            CALL WRITE_I_C(NSVFI,NODFI)
C   Void writing for MATSFI
            IF (INTTH>0) THEN
               CALL WRITE_I_C(NSVFI,NODFI)
               LEN_IA = LEN_IA + NODFI
            ENDIF
C   Void writing for IPARTSFI
            IF (INTFRIC>0) THEN
               CALL WRITE_I_C(NSVFI,NODFI)
               LEN_IA = LEN_IA + NODFI
            ENDIF
C
            DEALLOCATE(NRTSLOCAL)
            DEALLOCATE(NRTSP)
            DEALLOCATE(NSVFI)
            DEALLOCATE(TAGE)
C fin type 11
           ELSEIF(ITYP==20)THEN
C type 20 (partie non edge)
            IFQ    = IPARI(31,NI)
            NLN    = IPARI(35,NI)
            NISUB  = IPARI(36,NI)
            NISUBS = IPARI(37,NI)
            NISUBM = IPARI(38,NI)
            INTTH  = IPARI(47,NI)
C
            I_STOK = INTBUF_TAB(NI)%I_STOK(1)
            ALLOCATE(NSNLOCAL(NSN))
            ALLOCATE(NSNP(NSN))
            ALLOCATE(NSVFI(NSN))
            ALLOCATE(PFI(NSN))
            ALLOCATE(TAGE(NRTM))

            DO P = 1, NSPMD
              NUMP(P) = 0
            END DO
C
            DO K=1,NSN
              NL= INTBUF_TAB(NI)%NSV(K)
              N = INTBUF_TAB(NI)%NLG(NL)
C
              NSNLOCAL(K) = 0
              IF(TAG(N)==0) THEN
                 IF(NODLOCAL( N )/=0.AND.NODLOCAL( N )<=NUMNOD_L) THEN
                 NUMP(PROC+1) = NUMP(PROC+1) + 1
                 NSNLOCAL(K) = NUMP(PROC+1)
                 NSNP(K) = PROC+1
               ENDIF
C
               DO P = 1, NSPMD
                IF(P/=PROC+1.AND.NLOCAL(N,P)==1) THEN
                  NUMP(P) = NUMP(P) + 1
                  IF(NSNLOCAL(K)==0) THEN
                    NSNLOCAL(K) = NUMP(P)
                    NSNP(K) = P
                  END IF
                END IF
               END DO

               TAG(N) = 1
               NINDX_TAG = NINDX_TAG + 1
               INDX_TAG(NINDX_TAG) = N
              END IF
            END DO
C
            NRTM_L = 0
            DO K=1,NRTM
              TAGE(K) = 0
              IF(INTERCEP(1,NI)%P(K)==PROC+1) THEN
                NRTM_L = NRTM_L + 1
                TAGE(K) = NRTM_L
              ENDIF
            ENDDO
C
            NODFI = 0
            DO P = 1, NSPMD
              NSNFI(P) = 0
            END DO
C
            DO K = 1, I_STOK
              E = INTBUF_TAB(NI)%CAND_E(K)
              IF (TAGE(E)/=0) THEN
                N = INTBUF_TAB(NI)%CAND_N(K)
                NL = INTBUF_TAB(NI)%NSV(N)
                NN = INTBUF_TAB(NI)%NLG(NL)
                IF(NODLOCAL( NN )==0.OR.NODLOCAL( NN )>NUMNOD_L) THEN
C ne traiter qu une seule fois les noeuds
                  IF(NSNP(N)>0) THEN
                    P = NSNP(N)
                    NSNP(N) = -P
                    NSNFI(P) = NSNFI(P) + 1
                    NODFI = NODFI + 1
                    NSVFI(NODFI) = NSNLOCAL(N)
                    PFI(NODFI) = P
                  END IF
                END IF
              END IF
            END DO
C
            IF(NODFI>0) THEN
              ALLOCATE(INDEX(2*NODFI))
              ALLOCATE(CLEF(2,NODFI))
              DO K = 1, NODFI
                CLEF(1,K)=PFI(K)
                CLEF(2,K)=NSVFI(K)
              END DO
              CALL MY_ORDERS(0,WORK,CLEF,INDEX,NODFI,2)
              DO K = 1, NODFI
                NSVFI(K) = CLEF(2,INDEX(K))
                PFI(K) = CLEF(1,INDEX(K))
              END DO
              DEALLOCATE(INDEX)
              DEALLOCATE(CLEF)
            END IF
            DEALLOCATE(PFI)
C
            CALL WRITE_I_C(NSNFI,NSPMD)
            LEN_IA = LEN_IA + NSPMD
            CALL WRITE_I_C(NSVFI,NODFI)
            LEN_IA = LEN_IA + NODFI
C   ecriture bidon itafi
            CALL WRITE_I_C(NSVFI,NODFI)
            LEN_IA = LEN_IA + NODFI
C   ecriture bidon kinfi
            CALL WRITE_I_C(NSVFI,NODFI)
            LEN_IA = LEN_IA + NODFI
C   ecriture bidon nbinflfi
            CALL WRITE_I_C(NSVFI,NODFI)
            LEN_IA = LEN_IA + NODFI
C   Void writing for MATSFI
            IF (INTTH>0) THEN
               CALL WRITE_I_C(NSVFI,NODFI)
               LEN_IA = LEN_IA + NODFI
            ENDIF
C
            DEALLOCATE(NSNLOCAL)
            DEALLOCATE(NSNP)
            DEALLOCATE(NSVFI)
            DEALLOCATE(TAGE)
           END IF ! fin type20
C fin interface de contact inacti
          END IF
C Type20 ajout pour edge
          IF (ITYP==20)THEN
C   ecriture nsnsie : 0
             DO P = 1, NSPMD
               NSNFI(P) = 0
             END DO
             CALL WRITE_I_C(NSNFI,NSPMD)
             INACTI = IPARI(22,NI)
             IF(INACTI/=5.AND.INACTI/=6.AND.INACTI/=7) THEN
C   ecriture nsnfie : 0 si inacti # 5, 6 et 7
               CALL WRITE_I_C(NSNFI,NSPMD)
             ELSE
               NRTS   = IPARI(3,NI)
               NRTM   = IPARI(4,NI)
               NSN    = IPARI(5,NI)
               NMN    = IPARI(6,NI)
               MULTIMP= IPARI(23,NI)
               IFQ    = IPARI(31,NI)
               NLN    = IPARI(35,NI)
               NISUB  = IPARI(36,NI)
               NISUBS = IPARI(37,NI)
               NISUBM = IPARI(38,NI)
C
               INTTH  = IPARI(47,NI)
C
               NLINS  = IPARI(51,NI)
               NLINM  = IPARI(52,NI)
               NSNE   = IPARI(55,NI)
               NMNE   = IPARI(56,NI)
C
               I_STOK = INTBUF_TAB(NI)%I_STOK_E(1)
               ALLOCATE(NRTSLOCAL(NLINS))
               ALLOCATE(NRTSP(NLINS))
               ALLOCATE(NSVFI(NLINS))
               ALLOCATE(PFI(NLINS))
               ALLOCATE(TAGE(NLINM))
               DO P = 1, NSPMD
                 NUMP(P) = 0
               END DO
               DO K=1,NLINS
                 N1L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+1)
                 N2L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+2)
                 N1 =  INTBUF_TAB(NI)%NLG(N1L)
                 N2 =  INTBUF_TAB(NI)%NLG(N2L)
C
                 NRTSLOCAL(K) = 0
                 IF( (NODLOCAL( N1 )/=0.AND.NODLOCAL( N1 )<=NUMNOD_L).AND.
     +               (NODLOCAL( N2 )/=0.AND.NODLOCAL( N2 )<=NUMNOD_L) ) THEN
                   NUMP(PROC+1) = NUMP(PROC+1) + 1
                   NRTSLOCAL(K) = NUMP(PROC+1)
                   NRTSP(K) = PROC+1
                 END IF
C
                 DO P = 1, NSPMD
                   IF(P/=PROC+1.AND.NLOCAL(N1,P)==1.AND.
     .                NLOCAL(N2,P)==1) THEN
                     IF(NRTSLOCAL(K)==0) THEN
                       NUMP(P) = NUMP(P) + 1
                       NRTSLOCAL(K) = NUMP(P)
                       NRTSP(K) = P
                       GOTO 2400
                     END IF
                   END IF
                 END DO
 2400            CONTINUE
               END DO
C
               NRTM_L = 0
               DO K=1,NLINM
                 TAGE(K) = 0
                 IF(INTERCEP(2,NI)%P(K)==PROC+1) THEN
                   NRTM_L = NRTM_L + 1
                   TAGE(K) = NRTM_L
                 END IF
               END DO
C
               NODFI = 0
               DO P = 1, NSPMD
                 NSNFI(P) = 0
               END DO
C
               DO K = 1, I_STOK
                 E = INTBUF_TAB(NI)%LCAND_S(K)
                 IF (TAGE(E)/=0) THEN
                   L = INTBUF_TAB(NI)%IXLINS(K)
                   N1L = INTBUF_TAB(NI)%IXLINS((L-1)*2+1)
                   N2L = INTBUF_TAB(NI)%IXLINS((L-1)*2+2)
                   N1 =  INTBUF_TAB(NI)%NLG(N1L)
                   N2 =  INTBUF_TAB(NI)%NLG(N2L)
                 IF( (NODLOCAL( N1 )==0.OR.NODLOCAL( N1 )>NUMNOD_L).AND.
     +               (NODLOCAL( N2 )==0.OR.NODLOCAL( N2 )>NUMNOD_L) ) THEN
                     IF(NRTSP(L)>0)THEN
                       P = NRTSP(L)
                       NRTSP(L) = -P
                       NSNFI(P) = NSNFI(P) + 1
                       NODFI = NODFI + 1
                       NSVFI(NODFI) = NRTSLOCAL(L)
                       PFI(NODFI) = P
                     END IF
                   END IF
                 END IF
               END DO
C
               IF(NODFI>0) THEN
                 ALLOCATE(INDEX(2*NODFI))
                 ALLOCATE(CLEF(2,NODFI))
                 DO K = 1, NODFI
                   CLEF(1,K)=PFI(K)
                   CLEF(2,K)=NSVFI(K)
                 END DO
                 CALL MY_ORDERS(0,WORK,CLEF,INDEX,NODFI,2)
                 DO K = 1, NODFI
                   NSVFI(K) = CLEF(2,INDEX(K))
                   PFI(K) = CLEF(1,INDEX(K))
                 END DO
                 DEALLOCATE(INDEX)
                 DEALLOCATE(CLEF)
               END IF
               DEALLOCATE(PFI)
C
               CALL WRITE_I_C(NSNFI,NSPMD)
               CALL WRITE_I_C(NSVFI,NODFI)
C   ecriture bidon itafi n1
               CALL WRITE_I_C(NSVFI,NODFI)
C   ecriture bidon itafi n2
               CALL WRITE_I_C(NSVFI,NODFI)
C
               DEALLOCATE(NRTSLOCAL)
               DEALLOCATE(NRTSP)
               DEALLOCATE(NSVFI)
               DEALLOCATE(TAGE)
             END IF ! fin inacti
          END IF   ! Type 20 edge

          IEDGE = IPARI(58,NI)
          ILEV  = IPARI(20,NI)

          IF( ITYP == 25 .AND. IEDGE /= 0) THEN
            NEDGE = IPARI(68,NI)
            NRTM = IPARI(4,NI) 
            NSNFI(1:NSPMD) = 0
            CALL WRITE_I_C(NSNFI,NSPMD) ! NSNSIE = 0
            CALL WRITE_I_C(I25_FIE(NI,PROC+1)%NEDGE(1:NSPMD),NSPMD) ! NSNFIE 
            NEDGE_KEPT = I25_FIE(NI,PROC+1)%NEDGE_TOT
            NODFI = 2 * NEDGE_KEPT
            ALLOCATE(LEDGE_FIE(NODFI*E_IBUF_SIZE)) !working array, max size 
            LEDGE_FIE(1:NODFI*E_IBUF_SIZE) = 0
            IF(NEDGE_KEPT > 0) THEN
              CALL WRITE_I_C(I25_FIE(NI,PROC+1)%ID(1:NEDGE_KEPT),NEDGE_KEPT) ! NSVFIE
C             CALL WRITE_I_C(TAGE(1:NODFI),NODFI) ! ITAFIE
C             CALL WRITE_I_C(MAIN_FIE(1:NEDGE_KEPT),NEDGE_KEPT)!MAIN_FIE

C LEDGE_FIE
              DO K = 1, NEDGE_KEPT
                J = I25_FIE(NI,PROC+1)%ID(K) 
                ASSERT(J > 0)
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 1) = INTBUF_TAB(NI)%LEDGE(8 + (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 2) = INTBUF_TAB(NI)%LEDGE(1 + (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 3) = INTBUF_TAB(NI)%LEDGE(2 + (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 4) = INTBUF_TAB(NI)%LEDGE(3+ (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 5) = INTBUF_TAB(NI)%LEDGE(4+ (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 6) = INTBUF_TAB(NI)%LEDGE(5+ (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 7) = INTBUF_TAB(NI)%LEDGE(6+ (J-1)*NLEDGE) 
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 8) = INTBUF_TAB(NI)%LEDGE(7+ (J-1)*NLEDGE) 
                ! global id node 1
                LEDGE_FIE(E_IBUF_SIZE*(K-1) + 9) = ITAB(LEDGE_FIE(E_IBUF_SIZE*(K-1) + 6))
                !global id node 2
                LEDGE_FIE(E_IBUF_SIZE*(K-1) +10) = ITAB(LEDGE_FIE(E_IBUF_SIZE*(K-1) + 7))
                ! IM
                LEDGE_FIE(E_IBUF_SIZE*(K-1) +11) = INTBUF_TAB(NI)%LEDGE(10 + (J-1)*NLEDGE) 
                ! LocalID
                LEDGE_FIE(E_IBUF_SIZE*(K-1) +12) = 0         
                ! BINFLG
                IF(ILEV == 2) THEN
                  LEDGE_FIE(E_IBUF_SIZE*(K-1) +13) = INTBUF_TAB(NI)%EBINFLG(J)
                ELSE
                  LEDGE_FIE(E_IBUF_SIZE*(K-1) +13) = 0
                ENDIF
              ENDDO
              NSNE =  E_IBUF_SIZE*NEDGE_KEPT
              CALL WRITE_I_C(LEDGE_FIE,NSNE) ! LEDGE_FIE
C   Void writing for IPARTFRIC_FIE
              IF (INTFRIC>0) THEN
                  CALL WRITE_I_C(I25_FIE(NI,PROC+1)%ID(1:NEDGE_KEPT),NEDGE_KEPT)
               ENDIF
            ENDIF
            DEALLOCATE(LEDGE_FIE)
           
          ENDIF ! TYPE25+EDGE
C
          IF(IPARI(36,NI)>0.AND.ITYP/=17) THEN
C init a 0 des structures de sous interfaces
            NUMP(1:NSPMD) = 0
            CALL WRITE_I_C(NUMP,NSPMD)
            IF(ITYP == 25 .AND. IPARI(58,NI) > 0) THEN
              ! fake NISUBSFIE
              CALL WRITE_I_C(NUMP,NSPMD)
            ENDIF
          END IF
C
        END IF


        IF(ITYP==21 ) THEN
          INTTH = IPARI(47,NI)
          IF(INTTH==2.OR.IPARI(95,NI) > 0) THEN
C   ecriture NMNSI : 0
          DO P = 1, NSPMD
            NMNFI(P) = 0
          END DO
          CALL WRITE_I_C(NMNFI,NSPMD)
          LEN_IA = LEN_IA + NSPMD
C   ecriture nmnfi : 0
          CALL WRITE_I_C(NMNFI,NSPMD)
          LEN_IA = LEN_IA + NSPMD
C
         ENDIF
        ENDIF
!       ----------------------------------
!       Flush arrays to 0
        IF(NINDX_TAG>0) TAG(INDX_TAG(1:NINDX_TAG)) = 0
!       ----------------------------------

      END DO
C
      RETURN
      END
