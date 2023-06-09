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
Chd|  IPARI_L_INI                   source/restart/ddsplit/ipari_l_ini.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE IPARI_L_INI(IPARI ,PROC   ,NUMNOD_L,
     +                NBDDI2M  ,I2NSN_L,PROBINT,IMAXIMP_L,NBI18_L ,
     +                NSKYI18_L,NSNT_L ,NMNT_L ,NSNT2_L  ,NMNT2_L ,
     +                CEP      ,IGRBRIC,IPARI_L ,
     +                NODLOCAL ,I2NSN25_L,INTERCEP, INTBUF_TAB,NUMNOR_L,
     +                I24MAXNSNE, MULTI_FVM,TAG,INDEX0,NINDEX0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
      USE INTBUFDEF_MOD  
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NUMNOD_L,
     .        NBDDI2M, I2NSN_L, IMAXIMP_L, NBI18_L, NSKYI18_L,
     .        IPARI(NPARI,*), CEP(*),
     .        NSNT_L,NMNT_L,NSNT2_L,NMNT2_L,
     .        IPARI_L(NPARI,NINTER),NODLOCAL(*),I2NSN25_L,
     .        NUMNOR_L,I24MAXNSNE
c INTER22
      INTEGER NBRIC_L, ISU1, NBRIC
      INTEGER,INTENT(INOUT) :: NINDEX0
      INTEGER, DIMENSION(*),INTENT(INOUT) :: INDEX0,TAG
c     
      my_real
     .        PROBINT
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC)  :: IGRBRIC
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
      INTEGER NI, NP, ITY, NRTS, NRTM, NSN, NMN,NMT,NMT_L,
     .        NLINS, NLINM,NLINSA,NLINMA,NSNE,  NMNE, NLN, NLN_L,
     .        I_STOK_L, NSN_L, NMN_L, NRTM_L, NRTS_L, NLINS_L,
     .        NLINM_L, NSNE_L, NMNE_L, N1L, N2L, N3L, N4L, NL,
     .        NIR, N, L, K, JJ, M, N1, N2, N3, N4, KK, P, E, I, J,
     .        INACTI,L24ADD,L24ADD_L,
     .        MULTIMP, IGAP, IFQ, IFORM, IOK, IEDGE, NCONTE, MULTIMPE, MULTIMPS,
     .        I_STOK, NCONT, NISUB, NISUBS_L, NISUBM_L,NISUBS, NISUBM, NISUBE_L,
     .        IE, IGN, IGE, NAD, EAD,NUVAR,ILEV,INTTH,NCONT1,NCONT2,
     .        MTYP,NSNR,ICURV,NRTM0,NRTM_SH,NRTM_SH_L,
     .        II_STOK,II_STOK_L,NREMNODE_L,FLAGREMNODE, 
     .        KD_L(50),JD_L(50),KFI_L,JFI_L,NLINSA_L,NLINMA_L,I22LEN1,
     .        NRTSE,NRTSE_L,NSN0,NADMSR,NADMSR_L,SE1,NN,
     .        NRTM_FE,NRTS_FE, NSN_FE ,NMN_FE ,NREMNOR_L,
     .        NEDGE, NEDGE_L, II_STOK_E, II_STOK_EL, K1, K2,
     .        INTNITSCHE, MY_NODE,NREMNODE_E2S_L,
     .        NSN_IGE,SIZ,NREMNODE_EDG_L,EM1,EM2,ES1,ES2,IK,KM1,KM2,KS1,KS2
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGE, TAGE2
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGI2,TAGG
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SM
      LOGICAL :: IS_LOCAL

      my_real
     .        RCONT
      INTEGER SECND_SURFACE_ON_DOMAIN
      EXTERNAL SECND_SURFACE_ON_DOMAIN
C-----------------------------------------------
!      IF (MULTI_FVM%IS_USED) THEN
!         ALLOCATE(TAG(NUMELS + NUMNOD))
!         ALLOCATE(INDEX0(NUMELS + NUMNOD))
!         TAG(1:NUMELS + NUMNOD) = 0
!      ELSE
!         ALLOCATE(TAG(NUMNOD+I24MAXNSNE))
!         ALLOCATE(INDEX0(NUMNOD+I24MAXNSNE))
!         TAG(1:NUMNOD+I24MAXNSNE) = 0
!      ENDIF
      ALLOCATE(TAGI2(NUMNOD))
      ALLOCATE(TAGG(NUMNOD))
C-----------------------------------------------
C Construction de IPARI_L - recopie initiale
C-----------------------------------------------
      IPARI_L(1:NPARI,1:NINTER)=IPARI(1:NPARI,1:NINTER)
C-----------------------------------------------
C decompte nb total main nodes et noeuds main int 2 frontieres
      NBDDI2M = 0
      I2NSN_L = 0
C decompte nb total noeuds secnd int 2 penalty
      I2NSN25_L = 0
C decompte impact interface type 7, 10, 11
      IMAXIMP_L = 0
C
      NBI18_L = 0
      NSKYI18_L = 0
          
C tag noeud interface I2
      DO I = 1, NUMNOD
        TAGI2(I) = 0
      END DO
C
      NUMNOR_L = 0
      DO NI=1,NINTER
        NINDEX0 = 0
C
        ITY = IPARI(7,NI)
        IF(ITY/=2.AND.ITY/=7.AND.ITY/=8.AND.ITY/=9.AND.
     .     ITY/=10.AND.ITY/=11.AND.
     .    (ITY/=17.OR.IPARI(33,NI)/=0).AND.ITY/=20.AND.
     .     ITY/=21.AND.ITY/=22.AND.ITY/=23.AND.ITY/=24.AND.
     .     ITY/=25)THEN
!       *********************************
C calcul IMAXIMP_L suivant interface
          IF(ITY==3) THEN
            NSN = IPARI(5,NI)
            NMN = IPARI(6,NI)
            IMAXIMP_L = IMAXIMP_L + 2*NINT((NMN+NSN)/PROBINT)
          ELSEIF(ITY==5.OR.ITY==6) THEN
            NSN = IPARI(5,NI)
            IMAXIMP_L = IMAXIMP_L + 2*NINT(NSN/PROBINT)
          ELSEIF(ITY==14) THEN
            NSN = IPARI(5,NI)
            IMAXIMP_L = IMAXIMP_L + 2*NINT(NSN/PROBINT/5)
          ELSEIF(ITY==15) THEN
            NRTS = IPARI(3,NI)
            IMAXIMP_L = IMAXIMP_L + 2*NINT(NRTS*4/PROBINT/5)
          END IF
        ELSEIF (ITY==2) THEN
!       *********************************
          NRTS   = IPARI(3,NI)
          NRTM   = IPARI(4,NI)
          NSN    = IPARI(5,NI)
          NMN    = IPARI(6,NI)
          ILEV   = IPARI(20,NI)

          ALLOCATE(TAGE(NRTM))
          DO I = 1, NRTM
            TAGE(I) = 0
          ENDDO
C calcul nombre de noeuds secnds locaux
          NSN_L = 0
          DO K=1,NSN
            MY_NODE = INTBUF_TAB(NI)%NSV(K)
            IF( NODLOCAL(MY_NODE)/=0.AND.NODLOCAL(MY_NODE)<=NUMNOD_L ) THEN
              NSN_L = NSN_L + 1
              L = INTBUF_TAB(NI)%IRTLM(K)
              TAGE(L) = 1
              DO P = 1, PROC
                IF(NLOCAL(INTBUF_TAB(NI)%NSV(K),P)==1) GO TO 102
              ENDDO
              I2NSN_L = I2NSN_L + 1
 102          CONTINUE
            ENDIF
          END DO
          IPARI_L(5,NI) = NSN_L
          NMN_L = 0
C calcul nombre total secnd penalty
          IF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) THEN
            DO K=1,NSN
              MY_NODE = INTBUF_TAB(NI)%NSV(K)
              IF( NODLOCAL(MY_NODE)/=0.AND.NODLOCAL(MY_NODE)<=NUMNOD_L ) THEN
                    I2NSN25_L = I2NSN25_L + 1
              ENDIF
            END DO
          END IF
C calcul des mains independant des facettes pour resolution pb de cumul p/off
          DO K = 1, NMN
            MY_NODE = INTBUF_TAB(NI)%MSR(K)
            IF( NODLOCAL(MY_NODE)/=0.AND.NODLOCAL(MY_NODE)<=NUMNOD_L ) THEN
              NMN_L = NMN_L + 1
              IF(TAGI2(INTBUF_TAB(NI)%MSR(K))==0) THEN
C decompte des frontieres de main nodes
                DO P = 1, NSPMD
                  IF(NLOCAL(INTBUF_TAB(NI)%MSR(K),P)==1)THEN
                    NBDDI2M = NBDDI2M + 1
                  END IF
                END DO
C on ne se compte pas soi-meme
                NBDDI2M = NBDDI2M - 1
C pour ne pas prendre en compte 2 fois des noeuds main d'interfaces differentes ds les frontieres
                TAGI2(INTBUF_TAB(NI)%MSR(K)) = 1
              END IF
            ENDIF
          END DO
          IPARI_L(6,NI) = NMN_L
C
          NRTM_L = 0
          DO I = 1, NRTM
            IF(TAGE(I)==1) NRTM_L = NRTM_L + 1
          ENDDO
          IPARI_L(4,NI) = NRTM_L
C
          DEALLOCATE(TAGE)
C
          NSNT2_L = NSNT2_L + NSN_L
          NMNT2_L = NMNT2_L + NMN_L
C Idel2 : allocation j23 et j24
          ILEV = IPARI(20,NI)
          IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12) THEN
            NUVAR = IPARI(35,NI)
          ELSEIF (ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN
            NUVAR = IPARI(35,NI)
          ENDIF
C-------------------------
C IPARI_L Interface type 2
C-------------------------
          IPARI_L(4,NI) = NRTM_L
          IPARI_L(5,NI) = NSN_L
          IPARI_L(6,NI) = NMN_L
C
        ELSEIF(ITY==7.OR.ITY==8.OR.ITY==10.OR.
     .         ITY==22.OR.ITY==23.OR.ITY==24.OR. 
     .         ITY==25) THEN
!       *********************************
C
          NRTS     = IPARI(3,NI)
          NRTM     = IPARI(4,NI)
          NRTM_FE  = IPARI(74,NI)
          NSN    = IPARI(5,NI)
          NSN_FE = IPARI(78,NI)
          NSN_IGE= IPARI(77,NI)
          NMN    = IPARI(6,NI)
          NMN_FE = IPARI(80,NI)
          NMT    = IPARI(9,NI)
          IGAP   = IPARI(21,NI)
          INACTI = IPARI(22,NI)
          MULTIMP= IPARI(23,NI)
          IFORM  = IPARI(30,NI)
          IFQ    = IPARI(31,NI)
          NISUB  = IPARI(36,NI)
          ICURV  = IPARI(39,NI)
          INTTH  = IPARI(47,NI)
          IEDGE  = IPARI(58,NI)
          L24ADD = IPARI(59,NI)
          NRTSE  = IPARI(52,NI)
          NSNE   = IPARI(55,NI)
          NADMSR = IPARI(67,NI)
          NEDGE  = IPARI(68,NI)
          INTNITSCHE = IPARI(86,NI)
          MULTIMPE   = IPARI(87,NI)
          MULTIMPS   = IPARI(89,NI)
          IF (ITY == 24) THEN
            NSN0 = NSN - NSNE
          ELSEIF (ITY == 7) THEN
            NSN0 = NSN - NSN_IGE
          ELSE
            NSN0 = NSN 
          ENDIF
C
          FLAGREMNODE =  IPARI(63,NI)
C
          NRTM_L = 0
          NMN_L  = 0
          NSN_L = 0
          NSNE_L = 0
          NREMNODE_L = 0
          NADMSR_L   = 0
          NEDGE_L    = 0
          NREMNOR_L  = 0
          NRTS_L     = 0
          NCONTE     = 0
          NREMNODE_EDG_L = 0
          NREMNODE_E2S_L = 0

C
          IF(ITY/=7) THEN
            NRTM_FE = NRTM
            NSN_FE = NSN
            NMN_FE = NMN
          ENDIF
C
          ALLOCATE(TAGE(NRTM))
C
          DO K=1,NRTM
C TAGE flag servant pour inacti
            TAGE(K) = 0
            IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
              N1 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
              N2 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
              N3 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
              N4 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
              IF(ITY == 8) THEN
               !conversion from main surface number
               ! to global number
               N1 = INTBUF_TAB(NI)%MSR(N1)
               N2 = INTBUF_TAB(NI)%MSR(N2)
               N3 = INTBUF_TAB(NI)%MSR(N3)
               N4 = INTBUF_TAB(NI)%MSR(N4)
              ENDIF
c             IF(NI==1) THEN
c             WRITE(600+PROC,*) NI,K,INTERCEP(1,NI)%P(K),PROC+1
c             WRITE(600+PROC,*) N1,N2,N3,N4 ,NMN_L,NRTM_L
c             ENDIF
              NRTM_L = NRTM_L + 1
              TAGE(K) = NRTM_L           
              IF (TAG(N1)==0) THEN
                NMN_L = NMN_L + 1
                TAG(N1) = 1     
              ENDIF
              IF (TAG(N2)==0) THEN
                NMN_L = NMN_L + 1
                TAG(N2) = 1
              ENDIF
              IF (TAG(N3)==0) THEN
                NMN_L = NMN_L + 1
                TAG(N3) = 1
              ENDIF
              IF (TAG(N4)==0) THEN
                NMN_L = NMN_L + 1
                TAG(N4) = 1
              ENDIF
              IF (FLAGREMNODE == 2)  NREMNODE_L = NREMNODE_L + 
     .            INTBUF_TAB(NI)%KREMNODE(K+1)-INTBUF_TAB(NI)%KREMNODE(K)
            ENDIF
            IF (ITY==25) THEN 
              IF(INTERCEP(2,NI)%P(K) > 0.AND.INTERCEP(2,NI)%P(K)/=INTERCEP(1,NI)%P(K)) THEN
                IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN
                  N1 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
                  N2 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
                  N3 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
                  N4 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
                  NRTM_L = NRTM_L + 1
                  TAGE(K) = NRTM_L
                  IF (TAG(N1)==0) THEN
                    NMN_L = NMN_L + 1
                    TAG(N1) = 1     
                  ENDIF
                  IF (TAG(N2)==0) THEN
                    NMN_L = NMN_L + 1
                    TAG(N2) = 1
                  ENDIF
                  IF (TAG(N3)==0) THEN
                    NMN_L = NMN_L + 1
                    TAG(N3) = 1
                  ENDIF
                  IF (TAG(N4)==0) THEN
                    NMN_L = NMN_L + 1
                    TAG(N4) = 1
                  ENDIF
                  IF (FLAGREMNODE == 2)  NREMNODE_L = NREMNODE_L + 
     .            INTBUF_TAB(NI)%KREMNODE(K+1)-INTBUF_TAB(NI)%KREMNODE(K)
                ENDIF 
              ENDIF    
            ENDIF 
          ENDDO
C
          DO K=1,NRTM
            IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
              N1 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
              N2 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
              N3 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
              N4 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
              IF(ITY == 8) THEN
               N1 = INTBUF_TAB(NI)%MSR(N1)
               N2 = INTBUF_TAB(NI)%MSR(N2)
               N3 = INTBUF_TAB(NI)%MSR(N3)
               N4 = INTBUF_TAB(NI)%MSR(N4)
              ENDIF
              TAG(N1) = 0
              TAG(N2) = 0
              TAG(N3) = 0
              TAG(N4) = 0
            ENDIF
            IF (ITY==25) THEN 
              IF(INTERCEP(2,NI)%P(K) > 0.AND.INTERCEP(2,NI)%P(K)/=INTERCEP(1,NI)%P(K)) THEN
                IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN
                  N1 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
                  N2 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
                  N3 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
                  N4 = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
                  TAG(N1) = 0
                  TAG(N2) = 0
                  TAG(N3) = 0
                  TAG(N4) = 0                  
                ENDIF 
              ENDIF    
            ENDIF
          ENDDO
C
          NINDEX0 = 0
          IF (MULTI_FVM%IS_USED .AND. INACTI == 7) THEN
C     Interface type 18 for law151
             DO K = 1, NSN0
                N = INTBUF_TAB(NI)%NSV(K)
                IF (CEP(N) == PROC .AND. TAG(N) == 0) THEN
                   NSN_L = NSN_L + 1
                   TAG(N) = 1
                   NINDEX0= NINDEX0 + 1
                   INDEX0(NINDEX0) = N
                   IF (FLAGREMNODE == 2.AND.ITY==25)  THEN
                      L = INTBUF_TAB(NI)%KREMNOR(K+1)-INTBUF_TAB(NI)%KREMNOR(K)
                      NREMNOR_L = NREMNOR_L + L
                   ENDIF
                ENDIF
             ENDDO
          ELSE
             DO K=1,NSN0
                N = INTBUF_TAB(NI)%NSV(K)
                IF(TAG(N)==0) THEN
                 IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
                   NSN_L = NSN_L + 1
                   TAG(N) = 1
                   NINDEX0= NINDEX0 + 1
                   INDEX0(NINDEX0) = N
                   IF (FLAGREMNODE == 2.AND.ITY==25)  THEN
                      L = INTBUF_TAB(NI)%KREMNOR(K+1)-INTBUF_TAB(NI)%KREMNOR(K)
                      NREMNOR_L = NREMNOR_L + L
                   ENDIF
                 ENDIF
                ENDIF
             END DO
             DO K=NSN0+1,NSN0 + NSN_IGE ! specifique aux surfaces IGEO
                N = INTBUF_TAB(NI)%NSV(K)
                IF(TAG(N)==0) THEN
                  NSN_L = NSN_L + 1
                  TAG(N) = 1
                  NINDEX0= NINDEX0 + 1
                  INDEX0(NINDEX0) = N
                ENDIF
             END DO
          ENDIF

#include "vectorize.inc"
        DO K=1,NINDEX0
                TAG(INDEX0(k)) = 0
        ENDDO
        NINDEX0= 0
C
C candidats initiaux pour inacti=5 ou restart type 10
          II_STOK = INTBUF_TAB(NI)%I_STOK(1)
          II_STOK_L = 0
          IF(ITY==23.OR.
     *       INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ/=0.OR.
     *       (ITY==24.AND.INACTI==-1).OR.
     *       (ITY==25.AND.(INACTI==-1.OR.INACTI==5))) THEN
C traitement pour eviter bug si ts les noeuds secnd stfn == zero
            IOK=0
            DO K = 1, II_STOK
              N = INTBUF_TAB(NI)%CAND_N(K)
              IF(INTBUF_TAB(NI)%STFNS(N)/=ZERO)IOK=1
            ENDDO
            IF(IOK==0)THEN
              INTBUF_TAB(NI)%I_STOK(1)=0
              II_STOK = 0
            END IF
            II_STOK_L = 0
            DO K = 1, II_STOK
              E = INTBUF_TAB(NI)%CAND_E(K)
              IF (TAGE(E)/=0) THEN
                N = INTBUF_TAB(NI)%CAND_N(K)
                II_STOK_L = II_STOK_L + 1
              ENDIF
            ENDDO
C
C Reperage des candidats se trouvant sur des procs distants
C

            NSNR = 0
            NINDEX0 = 0
            IF (MULTI_FVM%IS_USED .AND. INACTI == 7) THEN
C     Interface type 18 for law151
               DO K = 1, II_STOK
                  E = INTBUF_TAB(NI)%CAND_E(K)
                  IF (TAGE(E)/=0) THEN
                     N = INTBUF_TAB(NI)%CAND_N(K)
                     IF(TAG(N)==0) THEN
                        TAG(N) = 1
                        NINDEX0= NINDEX0 + 1
                        INDEX0(NINDEX0) = N
                        IF(INTBUF_TAB(NI)%NSV(N)<=NUMELS)THEN
                           IF(CEP(INTBUF_TAB(NI)%NSV(N)) /= PROC) THEN
                              NSNR = NSNR + 1
                           END IF
                        ELSE
                           NN = INTBUF_TAB(NI)%NSV(N)-NUMELS
                           SE1 = INTBUF_TAB(NI)%IS2SE(2*(NN-1)+1)
                           IF(INTERCEP(2,NI)%P(SE1)/=(PROC+1))THEN
                              NSNR = NSNR + 1
                           ENDIF
                        END IF  !(INTBUF_TAB(NI)%NSV(N)<=NUMNOD)THEN
                     END IF
                  ENDIF
               ENDDO
            ELSE
               DO K = 1, II_STOK
                  E = INTBUF_TAB(NI)%CAND_E(K)
                  IF (TAGE(E)/=0) THEN
                     N = INTBUF_TAB(NI)%CAND_N(K)
                     IF(TAG(N)==0) THEN
                        TAG(N) = 1
                        NINDEX0= NINDEX0 + 1
                        INDEX0(NINDEX0) = N
                        IF(INTBUF_TAB(NI)%NSV(N)<=NUMNOD)THEN
                           MY_NODE = INTBUF_TAB(NI)%NSV(N)
                           IF( NODLOCAL(MY_NODE)==0.OR.NODLOCAL(MY_NODE)>NUMNOD_L ) THEN
                              NSNR = NSNR + 1
                           END IF
                        ELSE
                           NN = INTBUF_TAB(NI)%NSV(N)-NUMNOD
                           SE1 = INTBUF_TAB(NI)%IS2SE(2*(NN-1)+1)
                           IF(INTERCEP(2,NI)%P(SE1)/=(PROC+1))THEN
                              NSNR = NSNR + 1
                           ENDIF
                        END IF  !(INTBUF_TAB(NI)%NSV(N)<=NUMNOD)THEN
                     END IF
                  ENDIF
               ENDDO
            ENDIF
#include "vectorize.inc"
                DO K=1,NINDEX0
                        TAG(INDEX0(k)) = 0
                ENDDO
                NINDEX0= 0

          ENDIF
C
          IF(NISUB>0) THEN
            NISUBS_L = 0
            IF (MULTI_FVM%IS_USED .AND. INACTI == 7) THEN
C     Interface type 18 for law151
               DO K=1,NSN_FE
                  N = INTBUF_TAB(NI)%NSV(K)
                  IF (N >NUMELS) CYCLE
                  IF(CEP(N) == PROC) THEN
                     NISUBS_L = NISUBS_L + INTBUF_TAB(NI)%ADDSUBS(1+K) -
     .                    INTBUF_TAB(NI)%ADDSUBS(K)
                  END IF
               END DO
            ELSE
               DO K=1,NSN_FE
                  N = INTBUF_TAB(NI)%NSV(K)
                  IF (N >NUMNOD) CYCLE                  
                  IF( NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L ) THEN
                     NISUBS_L = NISUBS_L + INTBUF_TAB(NI)%ADDSUBS(1+K) -
     .                    INTBUF_TAB(NI)%ADDSUBS(K)
                  END IF
               END DO
            ENDIF
C
            NISUBM_L = 0
            DO K=1,NRTM_FE
              IF (TAGE(K)/=0) THEN
                NISUBM_L = NISUBM_L + INTBUF_TAB(NI)%ADDSUBM(1+K) -
     .             INTBUF_TAB(NI)%ADDSUBM(K)
              END IF
            END DO
          END IF
C
          IF(NMN/=0) THEN
C            NCONT = NSN*NMN_L/NMN
            RCONT = NMN_L
            RCONT = RCONT/NMN
            NCONT = NINT(NSN0*RCONT)
            IF(NMN_L>0.AND.NSN>0) NCONT = MAX(NCONT,1)

            IF(NCONT > 0 .AND.II_STOK_L > MULTIMP*NCONT) THEN
              DO WHILE(II_STOK_L>MULTIMP*NCONT)
               MULTIMP=MULTIMP+4
              ENDDO
            ENDIF

          ELSE
            NCONT = 0
          ENDIF
C
          NSNT_L = NSNT_L + NSN_L
          NMNT_L = NMNT_L + NMN_L
          IF(ITY==7.OR.ITY==22.OR.ITY==23)THEN
            IF(INACTI==7)THEN
              NBI18_L = NBI18_L+1
              NSKYI18_L = NSKYI18_L+NRTM_L
            END IF
          ELSEIF(ITY==24)THEN
C
C----NLS1_L,NLS2_L, NLN_L will be calculated after MBINFLG in w_ipari
C  s a jour dans w_ipari          
c          IPARI_L(35,NI) = NLN_L
c          IPARI_L(51,NI) = NLINS_L
c          IPARI_L(52,NI) = NLINM_L
c          IPARI_L(59,NI) = L24ADD_L

c compute IPARI_L(42,NI) = NRTM_SH_L
            NRTM_SH_L=0
            DO K=1,NRTM
              IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
                MTYP = INTBUF_TAB(NI)%MSEGTYP24(K)
                IF(MTYP<0) NRTM_SH_L=NRTM_SH_L+1
              ENDIF
            ENDDO
            IPARI_L(42,NI)= NRTM_SH_L

C NLN_L NLINS_L NLINM_L (edge2edge) TBD!!

c!!!!!!!!!!!!!!!!!!  to be cleaned  !!!!!!!!!!!!!!!!!
            L24ADD_L = L24ADD 
            IPARI_L(59,NI)= L24ADD_L
c-----------everywhere for the moment
          NRTSE_L = 0
          DO I=1,NRTSE
            IF (INTERCEP(2,NI)%P(I)==PROC+1) THEN
               NRTSE_L = NRTSE_L +1
            ENDIF
          ENDDO

          NSNE_L   = 0
          DO I = 1,NSNE
            SE1 = INTBUF_TAB(NI)%IS2SE(2*(I-1)+1)
            IF (INTERCEP(2,NI)%P(SE1)==PROC+1) THEN        
               NSNE_L = NSNE_L + 1
            ENDIF
          ENDDO
          IPARI_L(52,NI) = NRTSE_L
          IPARI_L(55,NI) = NSNE_L


C
          IF(INTNITSCHE  > 0) THEN
            NRTS_L = 0
            DO K=1,NRTS
              IF(INTERCEP(3,NI)%P(K)==PROC+1)THEN
                NRTS_L = NRTS_L + 1
              ENDIF
           ENDDO
            IPARI_L(3,NI) = NRTS_L
          ENDIF
C

C
          ELSEIF(ITY==25)THEN
C
C compute IPARI_L(42,NI) = NRTM_SH_L
            NRTM_SH_L=0
            DO K=1,NRTM
              IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
                MTYP = INTBUF_TAB(NI)%MSEGTYP24(K)
                IF(MTYP<0) NRTM_SH_L=NRTM_SH_L+1
              ENDIF
            ENDDO
            IPARI_L(42,NI)= NRTM_SH_L

            ALLOCATE(TAG_SM(NADMSR))
            TAG_SM(1:NADMSR)=0
            DO K=1,NRTM
              IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
                N1 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+1)
                N2 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+2)
                N3 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+3)
                N4 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+4) 
                IF(TAG_SM(N1)==0)THEN
                  NADMSR_L=NADMSR_L+1
                  TAG_SM(N1)=NADMSR_L
                END IF
                IF(TAG_SM(N2)==0)THEN
                  NADMSR_L=NADMSR_L+1
                  TAG_SM(N2)=NADMSR_L
                END IF
                IF(TAG_SM(N3)==0)THEN
                  NADMSR_L=NADMSR_L+1
                  TAG_SM(N3)=NADMSR_L
                END IF
                IF(TAG_SM(N4)==0)THEN
                  NADMSR_L=NADMSR_L+1
                  TAG_SM(N4)=NADMSR_L
                END IF
              ENDIF
              IF(INTERCEP(2,NI)%P(K) > 0.AND.INTERCEP(2,NI)%P(K)/=INTERCEP(1,NI)%P(K)) THEN
                IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN
                  N1 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+1)
                  N2 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+2)
                  N3 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+3)
                  N4 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+4) 
                  IF(TAG_SM(N1)==0)THEN
                    NADMSR_L=NADMSR_L+1
                    TAG_SM(N1)=NADMSR_L
                  END IF
                  IF(TAG_SM(N2)==0)THEN
                    NADMSR_L=NADMSR_L+1
                    TAG_SM(N2)=NADMSR_L
                  END IF
                  IF(TAG_SM(N3)==0)THEN
                    NADMSR_L=NADMSR_L+1
                    TAG_SM(N3)=NADMSR_L
                  END IF
                  IF(TAG_SM(N4)==0)THEN
                    NADMSR_L=NADMSR_L+1
                    TAG_SM(N4)=NADMSR_L
                  END IF
                ENDIF
              ENDIF                  
            ENDDO 

            TAG_SM(1:NADMSR)=0
            DO N=1,NEDGE
              IS_LOCAL = .FALSE.
              K1=INTBUF_TAB(NI)%LEDGE(NLEDGE*(N-1)+1)
              K2=INTBUF_TAB(NI)%LEDGE(NLEDGE*(N-1)+3)
              IF(K1 /= 0) THEN
                IF(INTERCEP(1,NI)%P(K1)==PROC+1) THEN
                  IS_LOCAL = .TRUE.
                ENDIF
              ENDIF
              IF(K2 /= 0) THEN
                IF(INTERCEP(1,NI)%P(K2)==PROC+1) THEN
                  IS_LOCAL = .TRUE.
                ENDIF
              ENDIF
              IF(IS_LOCAL) THEN
                NEDGE_L=NEDGE_L+1
              END IF
            ENDDO 
            DEALLOCATE(TAG_SM)

C
            NCONTE   = 0
            NISUBE_L = 0
            IF(IEDGE/=0) THEN
C
C             NCONT  = NEDGE*NEDGE_L/NEDGE ! cf NCONT=NEDGE , cest a voir ...
              RCONT  = NEDGE_L
              RCONT  = RCONT/NEDGE
              NCONTE = NINT(NEDGE*RCONT)
C
              II_STOK_E  = INTBUF_TAB(NI)%I_STOK_E(1)
              II_STOK_EL = 0
              IF(NEDGE_L>0.AND.NEDGE>0) NCONTE = MAX(NCONTE,1)
              DO K = 1, II_STOK_E
                N = INTBUF_TAB(NI)%CANDM_E2E(K)
                E = INTBUF_TAB(NI)%LEDGE(NLEDGE*(N-1)+1)
                IF (TAGE(E)/=0) THEN ! Candidate on the proc where 1st segment sharing the main edge, belongs
                  II_STOK_EL = II_STOK_EL + 1
                ENDIF
              ENDDO
              IF(NCONTE > 0 .AND.II_STOK_EL > MULTIMPE*NCONTE) THEN
                DO WHILE(II_STOK_EL > MULTIMPE*NCONTE)
                  MULTIMPE=MULTIMPE+4
                ENDDO
              ENDIF

              II_STOK_E  = INTBUF_TAB(NI)%I_STOK_E(2)
              II_STOK_EL = 0
              DO K = 1, II_STOK_E
                E = INTBUF_TAB(NI)%CANDM_E2S(K)
                IF (TAGE(E)/=0) THEN ! Candidate on the proc where 1st segment sharing the main edge, belongs
                  II_STOK_EL = II_STOK_EL + 1
                ENDIF
              ENDDO
              IF(NCONTE > 0 .AND.II_STOK_EL > MULTIMPS*NCONTE) THEN
                DO WHILE(II_STOK_EL > MULTIMPS*NCONTE)
                  MULTIMPS=MULTIMPS+4
                ENDDO
              ENDIF
C
              IF(NISUB/=0)THEN
                DO K=1,NEDGE
                  E = INTBUF_TAB(NI)%LEDGE(NLEDGE*(K-1)+1)
                  IF (TAGE(E)/=0) THEN
                    NISUBE_L = NISUBE_L + INTBUF_TAB(NI)%ADDSUBE(1+K) -
     .                 INTBUF_TAB(NI)%ADDSUBE(K)
                  END IF
                END DO
              END IF


C-- Iremgap/IremI2 - computation of NREMNODE_EDG_L /NREMNODE_E2S_L-

             IF (FLAGREMNODE == 2) THEN
              ! NREMNODE_EDG_L
                DO K=1,NEDGE
                   EM1 = INTBUF_TAB(NI)%LEDGE(NLEDGE*(K-1)+1)
                   KM1 = 0
                   IF(EM1/=0) KM1=TAGE(EM1)
                   EM2 = INTBUF_TAB(NI)%LEDGE(NLEDGE*(K-1)+3)
                   KM2 = 0
                   IF(EM2/=0) KM2=TAGE(EM2)
                   IF (KM1/=0.OR.KM2/=0) THEN
                      SIZ = INTBUF_TAB(NI)%KREMNODE_EDG(K+1)-INTBUF_TAB(NI)%KREMNODE_EDG(K)
                      IK = INTBUF_TAB(NI)%KREMNODE_EDG(K)
                      DO M=1,SIZ
                         N = INTBUF_TAB(NI)%REMNODE_EDG(IK+M-1)
                         ES1 = INTBUF_TAB(NI)%LEDGE(NLEDGE*(N-1)+1)
                         KS1 = 0
                         IF(ES1/=0) KS1=TAGE(ES1)
                         ES2 = INTBUF_TAB(NI)%LEDGE(NLEDGE*(N-1)+3)
                         KS2 = 0
                         IF(ES2/=0) KS2=TAGE(ES2)
                         IF(KM1/=0.AND.KM2/=0.AND.KS1/=0.AND.KS2/=0)THEN
C--                 Local segment - local id is stored
                            NREMNODE_EDG_L = NREMNODE_EDG_L + 1
                         ELSE
C--                 Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
                            NREMNODE_EDG_L = NREMNODE_EDG_L + 2
                         ENDIF
                      END DO
                   ENDIF
                 ENDDO
              ! NREMNODE_E2S_L
                 DO K=1,NRTM
                   IF(TAGE(K) > 0) THEN
                      SIZ = INTBUF_TAB(NI)%KREMNODE_E2S(K+1)-INTBUF_TAB(NI)%KREMNODE_E2S(K)
                      IK = INTBUF_TAB(NI)%KREMNODE_E2S(K)
                      DO M=1,SIZ
                         N = INTBUF_TAB(NI)%REMNODE_E2S(IK+M-1)
                         E = INTBUF_TAB(NI)%LEDGE(NLEDGE*(N-1)+1)  
                         IF (TAGE(E)/=0) THEN ! Candidate on the proc where 1st segment sharing the master edge, belongs
C--                 Local segment - local id is stored
                            NREMNODE_E2S_L = NREMNODE_E2S_L + 1
                         ELSE
C--                 Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
                            NREMNODE_E2S_L = NREMNODE_E2S_L + 2
                         ENDIF
                      ENDDO
                   ENDIF
                 ENDDO
               ENDIF
C

            ENDIF
            IPARI_L(88,NI)= NCONTE
            IPARI_L(87,NI)= MULTIMPE
            IPARI_L(89,NI)= MULTIMPS
            IPARI_L(90,NI)= NISUBE_L
            IPARI_L(94,NI)= NREMNODE_EDG_L
            IPARI_L(96,NI)= NREMNODE_E2S_L


          END IF ! ELSEIF(ITY==25)THEN
C
          DEALLOCATE(TAGE)
C
          NSN_L = NSN_L + NSNE_L
              
          NMT_L = 0
          IF (ITY==8) THEN
            DO K = 1,NMT
              JJ = INTBUF_TAB(NI)%LMSR(k)
              IF(INTERCEP(1,NI)%P(JJ) == PROC+1) THEN
              NMT_L = NMT_L + 1 
              ENDIF
            ENDDO
          ENDIF
C
          IMAXIMP_L = IMAXIMP_L + MULTIMP*NCONT + MULTIMPE*NCONTE + MULTIMPS*NCONTE
C -------------------------------
C IPARI_L Interface type 7, 8,10, 22, 23, 24, 25
C -------------------------------
          !for auto-impactant interfaces (secnd nodes=main surface nodes)
          !IRECTS is used in lecins but no need in engine, set to 0 
          !to save memory consumption at engine 
          IF(INTNITSCHE == 0) IPARI_L(3,NI) = 0
          IPARI_L(4,NI) = NRTM_L ! TOTAL
          IPARI_L(74,NI) = NRTM_L ! FE
          IPARI_L(5,NI) = NSN_L ! TOTAL
          IPARI_L(78,NI) = NSN_L ! FE
          IPARI_L(6,NI) = NMN_L ! TOTAL
          IPARI_L(80,NI) = NMN_L ! FE
          IPARI_L(9,NI) = NMT_L
          IPARI_L(18,NI)= NCONT
          IPARI_L(23,NI)= MULTIMP

           IPARI_L(24,NI)= 0

          IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.ITY==23)
     .      IPARI_L(24,NI)= NSNR
     
          IF(INACTI==-1.AND.ITY==24) IPARI_L(24,NI)= NSNR
          IF((INACTI==-1.OR.INACTI==5).AND.ITY==25) IPARI_L(24,NI)= NSNR

          IPARI_L(25,NI)= 0

          IF(NISUB>0) THEN
            IPARI_L(37,NI)= NISUBS_L
            IPARI_L(38,NI)= NISUBM_L
          END IF

          IF(ICURV==1)THEN
            IPARI_L(40,NI)=NODLOCAL(IPARI(40,NI))
          ELSEIF(ICURV==2)THEN
            IPARI_L(40,NI)=NODLOCAL(IPARI(40,NI))
            IPARI_L(41,NI)=NODLOCAL(IPARI(41,NI))
          END IF
          IPARI_L(62,NI)=NREMNODE_L
          IPARI_L(81,NI)=NREMNOR_L

          IPARI_L(67,NI)=NADMSR_L
          NUMNOR_L = NUMNOR_L + NADMSR_L

          IPARI_L(68,NI)=NEDGE_L
c INT 22
          IF(ITY==22)THEN
            ISU1  = IPARI_L(45,NI)
            NBRIC = IGRBRIC(ISU1)%NENTITY ! global value ; update IGRN which does not exist here
            NBRIC_L = 0
            DO K = 1, NBRIC
              IE = IGRBRIC(ISU1)%ENTITY(K)
              IF(CEP(IE)==PROC) NBRIC_L = NBRIC_L + 1
              !balayer le grbric, si bric \in spmd_id then nbric ++
              !CEP contient les Brique en premiere section                  
              !CEP : id_elem |-> spmd_id ;  id_elem \in (|---brique---|---coque---|---...)              
            END DO 
            I22LEN1  = MAX(100     ,NINT(82*NBRIC_L**HALF))
            I22LEN1  = MIN(I22LEN1 ,NBRIC_L                 )
            I22LEN1  = NINT(JMULT22*I22LEN1)  
            I22LEN_L = MAX(I22LEN_L,I22LEN1)  !local value to determine :    (ARGUMENT INTENT(INOUT))
          ENDIF
          
C Fin cas interfaces type 7,8, 10 ,22 ,23 ,24, 25
C
        ELSEIF (ITY==11) THEN
!       *********************************
C
C
          NRTS   = IPARI(3,NI)
          NRTM   = IPARI(4,NI)
          NSN    = IPARI(5,NI)
          NMN    = IPARI(6,NI)
          IGAP   = IPARI(21,NI)
          INACTI = IPARI(22,NI)
          MULTIMP= IPARI(23,NI)
          IFORM  = IPARI(30,NI)
          NISUB  = IPARI(36,NI)
          FLAGREMNODE  = IPARI(63,NI)
C
          NRTM_L = 0
          NRTS_L = 0
          NMN_L  = 0
          NSN_L = 0
          NREMNODE_L = 0
C
          DO K=1,NRTS
            IF(INTERCEP(2,NI)%P(K)==PROC+1)THEN
              NRTS_L = NRTS_L + 1
            ENDIF
          ENDDO
C
          ALLOCATE(TAGE(NRTM))
C
          DO K=1,NRTM
C TAGE flag servant pour inacti
            TAGE(K) = 0
            IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
              N1 = INTBUF_TAB(NI)%IRECTM(1+2*(K-1))
              N2 = INTBUF_TAB(NI)%IRECTM(2+2*(K-1))
              NRTM_L = NRTM_L + 1
              TAGE(K) = NRTM_L
              IF (TAG(N1)==0) THEN
                NMN_L = NMN_L + 1
                TAG(N1) = 1
              ENDIF
              IF (TAG(N2)==0) THEN
                NMN_L = NMN_L + 1
                TAG(N2) = 1
              ENDIF
            ENDIF
          ENDDO
C
          DO K=1,NRTM
            IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
             N1 = INTBUF_TAB(NI)%IRECTM(1+2*(K-1))
             N2 = INTBUF_TAB(NI)%IRECTM(2+2*(K-1))
             TAG(N1) = 0
             TAG(N2) = 0
            ENDIF
          ENDDO
C
C-- Iremp - computation of NREMNODE_L -
          IF (FLAGREMNODE == 2) THEN
            DO K=1,NRTM
              IF(INTERCEP(1,NI)%P(K)==PROC+1)THEN
                SIZ = INTBUF_TAB(NI)%KREMNODE(K+1)-INTBUF_TAB(NI)%KREMNODE(K)
                E = INTBUF_TAB(NI)%KREMNODE(K)
                DO M=1,SIZ
                  N = INTBUF_TAB(NI)%REMNODE(E+M-1)
                  IF(INTERCEP(2,NI)%P(N)==PROC+1)THEN
C--                 Local segment - local id is stored
                    NREMNODE_L = NREMNODE_L + 1
                  ELSE
C--                 Remote segment - line is stored as ITAB1 / ITAB2 (2 values)
                    NREMNODE_L = NREMNODE_L + 2
                  ENDIF
                END DO
              ENDIF
            ENDDO
          ENDIF
C
          NINDEX0 = 0
          DO K=1,NSN
            N = INTBUF_TAB(NI)%NSV(K)
            IF(TAG(N)==0) THEN
             IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
              NSN_L = NSN_L + 1
              TAG(N) = 1
              NINDEX0= NINDEX0 + 1
              INDEX0(NINDEX0) = n
             ENDIF
            ENDIF
          ENDDO
          
#include "vectorize.inc"
          DO K=1,NINDEX0
                TAG(INDEX0(K)) = 0
          ENDDO
C
C candidats initiaux pour inacti=5
          I_STOK_L = 0
          I_STOK = INTBUF_TAB(NI)%I_STOK(1)
          IF (INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN
            DO K = 1, I_STOK
              E = INTBUF_TAB(NI)%CAND_E(K)
              IF (TAGE(E)/=0) THEN
                N = INTBUF_TAB(NI)%CAND_N(K)
                I_STOK_L = I_STOK_L + 1
              ENDIF
            ENDDO
          ENDIF
C
          IF(NISUB>0) THEN
            NISUBS_L = 0
            DO K=1,NRTS
              IF (TAGE(K)/=0) THEN
                NISUBS_L = NISUBS_L + INTBUF_TAB(NI)%ADDSUBS(1+K) -
     .             INTBUF_TAB(NI)%ADDSUBS(K)
              END IF
            END DO
C
            NISUBM_L = 0
            DO K=1,NRTM
              IF (TAGE(K)/=0) THEN
                NISUBM_L = NISUBM_L + INTBUF_TAB(NI)%ADDSUBM(1+K) -
     .             INTBUF_TAB(NI)%ADDSUBM(K)
              END IF
            END DO
          END IF
C
          DEALLOCATE(TAGE)

          IF(NMN/=0) THEN
            RCONT = NMN_L
            RCONT = RCONT/NMN
            NCONT = NINT(NSN*RCONT)
            IF(NMN_L>0.AND.NSN>0) NCONT = MAX(NCONT,1)

            IF(NCONT > 0 .AND.I_STOK_L > MULTIMP*NCONT) THEN
              DO WHILE(I_STOK_L>MULTIMP*NCONT)
               MULTIMP=MULTIMP+4
              ENDDO
            ENDIF

          ELSE
            NCONT = 0
          ENDIF
C
          NSNT_L = NSNT_L + NSN_L
          NMNT_L = NMNT_L + NMN_L

          IMAXIMP_L = IMAXIMP_L + MULTIMP*NCONT
C--------------------------
C IPARI_L TYPE 11
C--------------------------
          IPARI_L(3,NI) = NRTS_L
          IPARI_L(4,NI) = NRTM_L
          IPARI_L(5,NI) = NSN_L
          IPARI_L(6,NI) = NMN_L
          IPARI_L(18,NI)= NCONT
          IPARI_L(23,NI)= MULTIMP
          IPARI_L(24,NI) = 0
          IPARI_L(30,NI)= IFORM
          IPARI_L(62,NI)= NREMNODE_L

C Fin cas interface type 11

        ELSEIF (ITY==9) THEN
!       *********************************
C
          NMN = IPARI(6,NI)
          IMAXIMP_L = IMAXIMP_L + 2*NINT(NMN/PROBINT)       
C
        ELSEIF (ITY==17) THEN
!       *********************************
C
          NMN = IPARI(4,NI)
          NSN = IPARI(5,NI)
          MULTIMP = IPARI(23,NI)
          IGN = IPARI(36,NI)
          IGE = IPARI(34,NI)
C cote main
          NMN_L = 0
          DO K = 1, NMN
            IE = IGRBRIC(IGE)%ENTITY(K)
C pas d'offset car solide
            IF(CEP(IE)==PROC) NMN_L = NMN_L + 1
          END DO
C cote secnd
          NSN_L = 0
          DO K = 1, NSN
            IE = IGRBRIC(IGN)%ENTITY(K)
C pas d'offset car solide
            IF(CEP(IE)==PROC) NSN_L = NSN_L + 1
          END DO
          IF(NMN/=0) THEN
            RCONT = NMN_L
            RCONT = RCONT/NMN
            NCONT = NINT(NSN*RCONT)
            IF(NMN_L>0.AND.NSN>0) NCONT = MAX(NCONT,1)
          ELSE
            NCONT = 0
          ENDIF
C
          IMAXIMP_L = IMAXIMP_L + (MULTIMP*NCONT*16)/5 + 1
C
cc          IPARI_L(4,NI)  = NMN_L
cc          IPARI_L(5,NI)  = NSN_L
          IPARI_L(18,NI) = NCONT                          
C
C Fin cas interface type17
C
        ELSEIF(ITY==20) THEN
!       *********************************
          NRTS  = IPARI(3,NI)
          NRTM  = IPARI(4,NI)
          NSN    = IPARI(5,NI)
          NMN    = IPARI(6,NI)
          IGAP   = IPARI(21,NI)
          INACTI = IPARI(22,NI)
          MULTIMP= IPARI(23,NI)
          IFORM  = IPARI(30,NI)
          IFQ    = IPARI(31,NI)
          NLN    = IPARI(35,NI)
          NISUB  = IPARI(36,NI)
          NISUBS = IPARI(37,NI)
          NISUBM = IPARI(38,NI)
          ICURV  = IPARI(39,NI)
C
          INTTH  =  IPARI(47,NI)
C
          NLINS  = IPARI(51,NI)
          NLINM  = IPARI(52,NI)
          NLINSA = IPARI(53,NI)
          NLINMA = IPARI(54,NI)
          NSNE   = IPARI(55,NI)
          NMNE   = IPARI(56,NI)
C
C
          NRTM_L = 0
          NMN_L  = 0
          NSN_L = 0
          NLN_L = 0
C
          DO I=1,NUMNOD
            TAGG(I) = 0
          END DO
C
          ALLOCATE(TAGE(NRTM))

          DO K=1,NRTM
            TAGE(K) = 0
            IF(INTERCEP(1,NI)%P(K)==PROC+1) THEN
             N1L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+1)
             N2L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+2)
             N3L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+3)
             N4L = INTBUF_TAB(NI)%IRECTM(4*(K-1)+4)
             N1 = INTBUF_TAB(NI)%NLG(N1L)
             N2 = INTBUF_TAB(NI)%NLG(N2L)
             N3 = INTBUF_TAB(NI)%NLG(N3L)
             N4 = INTBUF_TAB(NI)%NLG(N4L)
              NRTM_L = NRTM_L + 1
              TAGE(K) = NRTM_L
              IF (TAGG(N1)==0) THEN
                NMN_L = NMN_L + 1
                NLN_L = NLN_L + 1
                TAGG(N1) = 1
              ENDIF
              IF (TAGG(N2)==0) THEN
                NMN_L = NMN_L + 1
                NLN_L = NLN_L + 1
                TAGG(N2) = 1
              ENDIF
              IF (TAGG(N3)==0) THEN
                NMN_L = NMN_L + 1
                NLN_L = NLN_L + 1
                TAGG(N3) = 1
              ENDIF
              IF (TAGG(N4)==0) THEN
                NMN_L = NMN_L + 1
                NLN_L = NLN_L + 1
                TAGG(N4) = 1
              ENDIF
            ENDIF
          ENDDO
C
C
          NINDEX0 = 0
          DO K=1,NSN
            NL = INTBUF_TAB(NI)%NSV(K)
            N = INTBUF_TAB(NI)%NLG(NL)
            IF(TAG(N)==0) THEN
             IF( NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L ) THEN
              NSN_L = NSN_L + 1
              TAG(N) = 1
              NINDEX0= NINDEX0 + 1
              INDEX0(NINDEX0) = N
              IF(TAGG(N)==0)THEN
                NLN_L = NLN_L + 1
                TAGG(N) = 1
              END IF
             ENDIF
            ENDIF
          END DO
C
C candidats initiaux pour inacti=5 ou restart type 10
          I_STOK_L = 0
          I_STOK = INTBUF_TAB(NI)%I_STOK(1)
          IF(INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN
            DO K = 1, I_STOK
              E = INTBUF_TAB(NI)%CAND_E(K)
              IF (TAGE(E)/=0) THEN
                N = INTBUF_TAB(NI)%CAND_N(K)
                I_STOK_L = I_STOK_L + 1
              ENDIF
            ENDDO
          ENDIF
C
          IF(NISUB>0) THEN
            NISUBS_L = 0
            DO K=1,NSN
              NL = INTBUF_TAB(NI)%NSV(K)
              N = INTBUF_TAB(NI)%NLG(NL)
              IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
                NISUBS_L = NISUBS_L + INTBUF_TAB(NI)%ADDSUBS(K+1) -INTBUF_TAB(NI)%ADDSUBS(K)
              END IF
            END DO
C
            NISUBM_L = 0
            DO K=1,NRTM
              IF (TAGE(K)/=0) THEN
                NISUBS_L = NISUBS_L + INTBUF_TAB(NI)%ADDSUBM(K+1) -INTBUF_TAB(NI)%ADDSUBM(K)
              END IF
            END DO
          END IF
C
          DEALLOCATE(TAGE)
C
c          IF(NMN/=0) THEN
C            NCONT = NSN*NMN_L/NMN
c            RCONT = NMN_L
c            RCONT = RCONT/NMN
c            NCONT = NINT(NSN*RCONT)
c            IF(NMN_L>0.AND.NSN>0) NCONT = MAX(NCONT,1)
c          ELSE
c            NCONT = 0
c          ENDIF
C
C Partie ligne
C
          NLINS_L = 0
          NLINM_L = 0
          NMNE_L  = 0
          NSNE_L = 0
C
#include "vectorize.inc"
          DO K=1,NINDEX0
                TAG(INDEX0(K)) = 0
          ENDDO
          NINDEX0 = 0
C
          NLINSA_L= 0
          DO K=1,NLINS
            N1L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+1)
            N2L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+2)
            N1 = INTBUF_TAB(NI)%NLG(N1L)
            N2 = INTBUF_TAB(NI)%NLG(N2L)
            IF(INTERCEP(3,NI)%P(K)==PROC+1) THEN
              NLINS_L = NLINS_L + 1
C comptage ligne active
              IF(K<=NLINSA)NLINSA_L = NLINSA_L + 1
              IF (TAG(N1)==0) THEN
                NSNE_L = NSNE_L + 1
                TAG(N1) = 1
                IF(TAGG(N1)==0)THEN
                  NLN_L = NLN_L + 1
                  TAGG(N1) = 1
                END IF
              ENDIF
              IF (TAG(N2)==0) THEN
                NSNE_L = NSNE_L + 1
                TAG(N2) = 1
                IF(TAGG(N2)==0)THEN
                  NLN_L = NLN_L + 1
                  TAGG(N2) = 1
                END IF
              ENDIF
            ENDIF
          ENDDO
C
          DO K=1,NLINS
            IF(INTERCEP(3,NI)%P(K)==PROC+1) THEN
              N1L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+1)
              N2L = INTBUF_TAB(NI)%IXLINS(2*(K-1)+2)
              N1 = INTBUF_TAB(NI)%NLG(N1L)
              N2 = INTBUF_TAB(NI)%NLG(N2L)
              TAG(N1) = 0
              TAG(N2) = 0
            ENDIF
          ENDDO
C
          ALLOCATE(TAGE2(NLINM))
C
          NLINMA_L= 0
          NINDEX0 = 0
          DO K=1,NLINM
C TAGE flag servant pour inacti
            TAGE2(K) = 0
            N1L = INTBUF_TAB(NI)%IXLINM(2*(K-1)+1)
            N2L = INTBUF_TAB(NI)%IXLINM(2*(K-1)+2) 
            N1 = INTBUF_TAB(NI)%NLG(N1L)
            N2 = INTBUF_TAB(NI)%NLG(N2L)
            IF(INTERCEP(2,NI)%P(K)==PROC+1) THEN
               NLINM_L = NLINM_L + 1
C comptage ligne active
              IF(K<=NLINMA)NLINMA_L = NLINMA_L + 1
               TAGE2(K) = NLINM_L
               IF (TAG(N1)==0) THEN
                 NMNE_L = NMNE_L + 1
                 TAG(N1) = 1
                 NINDEX0= NINDEX0 + 1
                 INDEX0(NINDEX0) = N1
                 IF(TAGG(N1)==0)THEN
                   NLN_L = NLN_L + 1
                   TAGG(N1) = 1
                 END IF
               ENDIF
               IF (TAG(N2)==0) THEN
                 NMNE_L = NMNE_L + 1
                 TAG(N2) = 1
                 NINDEX0= NINDEX0 + 1
                 INDEX0(NINDEX0) = n2
                 IF(TAGG(N2)==0)THEN
                   NLN_L = NLN_L + 1
                   TAGG(N2) = 1
                 END IF
               ENDIF
            ENDIF
          ENDDO
#include "vectorize.inc"
          DO K=1,NINDEX0
                TAG(INDEX0(K)) = 0
          ENDDO
          NINDEX0 = 0
C
C
c          DO I = 1, NLN
c            N = INTBUF_TAB(NI)%NLG(I)
c            IF(MOD(FRONT(N,PROC+1),10)==1) THEN
c              NSNE_L = NSNE_L + 1
c            END IF
c          END DO
C
C candidats initiaux pour inacti=5 A REVOIR si besoin !!!
c          I_STOK = INTBUF_TAB(NI)%i_STOK(1)
c          IF (INACTI==5.OR.INACTI==6.OR.INACTI==7) THEN
c            I_STOK_L = 0
c            DO K = 1, I_STOK
c              E = INTBUF_TAB(NI)%CAND_E(K)
c              IF (TAGE(E)/=0) THEN
c                N = INTBUF_TAB(NI)%CAND_N(K)
c                I_STOK_L = I_STOK_L + 1
c              ENDIF
c            ENDDO
c          ENDIF
C
          DEALLOCATE(TAGE2)
C
C Calcul de NCONT en prenant le max des edge et non edge
C
          NCONT1 = 0
          IF(NMN/=0) THEN
C            NCONT = NSN*NMN_L/NMN
            RCONT = NMN_L
            RCONT = RCONT/NMN
            NCONT = NINT(NSN*RCONT)
            IF(NMN_L>0.AND.NSN>0) NCONT1 = MAX(NCONT,1)

            IF(NCONT1 > 0 .AND.I_STOK_L > MULTIMP*NCONT1) THEN
              DO WHILE(I_STOK_L>MULTIMP*NCONT1)
               MULTIMP=MULTIMP+4
              ENDDO
            ENDIF

          ENDIF

          NCONT2 = 0
          IF(NMNE/=0) THEN
            RCONT = NMNE_L
            RCONT = RCONT/NMNE
            NCONT = NINT(NSNE*RCONT)
            IF(NMNE_L>0.AND.NSNE>0) NCONT2 = MAX(NCONT,1)

            IF(NCONT2 > 0 .AND.I_STOK_L > MULTIMP*NCONT2) THEN
              DO WHILE(I_STOK_L>MULTIMP*NCONT2)
               MULTIMP=MULTIMP+4
              ENDDO
            ENDIF
          ENDIF
          NCONT = MAX(NCONT1,NCONT2)
C
C
C
          NSNT_L = NSNT_L + NSN_L + NSNE_L
          NMNT_L = NMNT_L + NMN_L + NMNE_L
          IMAXIMP_L = IMAXIMP_L + MULTIMP*NCONT
C-----------------
C IPARI_L TYPE 20
C-----------------
          !for auto-impactant interfaces (secnd nodes=main surface nodes)
          !IRECTS is used in lecins but no need in engine, set to 0 
          !to save memory consumption at engine 
          IPARI_L(3,NI) = 0
          IPARI_L(4,NI)  = NRTM_L
          IPARI_L(5,NI)  = NSN_L
          IPARI_L(6,NI)  = NMN_L
          IPARI_L(18,NI) = NCONT
          IPARI_L(23,NI)= MULTIMP
          IPARI_L(24,NI) = 0
C  s a jour dans w_ipari          IF(INACTI==5.OR.INACTI==6.OR.INACTI==7)
C  s a jour dans w_ipari     .      IPARI_L(24,NI)= NSNR
C  s a jour dans w_ipari          IPARI_L(25,NI) = 0
          IPARI_L(35,NI) = NLN_L
          IPARI_L(51,NI) = NLINS_L
          IPARI_L(52,NI) = NLINM_L
C  s a jour dans w_ipari          IPARI_L(53,NI) = NLINSA_L
C  s a jour dans w_ipari          IPARI_L(54,NI) = NLINMA_L
          IPARI_L(55,NI) = NSNE_L
          IPARI_L(56,NI) = NMNE_L
C  s a jour dans w_ipari          IF(INACTI==5.OR.INACTI==6.OR.INACTI==7)
C  s a jour dans w_ipari     .      IPARI_L(57,NI)= NLINSR
          IF(NISUB>0) THEN
            IPARI_L(37,NI)= NISUBS_L
            IPARI_L(38,NI)= NISUBM_L
          END IF

          IF(ICURV==1)THEN
            IPARI_L(40,NI)=NODLOCAL(IPARI(40,NI))
          ELSEIF(ICURV==2)THEN
            IPARI_L(40,NI)=NODLOCAL(IPARI(40,NI))
            IPARI_L(41,NI)=NODLOCAL(IPARI(41,NI))
          END IF
          IPARI_L(53,NI) = NLINSA_L
          IPARI_L(54,NI) = NLINMA_L
          IPARI_L(25,NI)= 0

C Fin cas interfaces type 20
C
        ELSEIF(ITY==21) THEN
!       *********************************
C
C interface type21
C
          NRTS  = IPARI(3,NI)
          NRTM  = IPARI(4,NI)
          NSN    = IPARI(5,NI)
          NMN    = IPARI(6,NI)
          IGAP   = IPARI(21,NI)
          INACTI = IPARI(22,NI)
          MULTIMP= IPARI(23,NI)
          IFORM  = IPARI(30,NI)
          IFQ    = IPARI(31,NI)
C
          INTTH =  IPARI(47,NI)
C
          NRTM_L = NRTM
          NMN_L  = 0
          NRTS_L = 0
          NSN_L  = 0
C
C
          NINDEX0 = 0
          DO K=1,NMN
            N =INTBUF_TAB(NI)%MSR(K)
            IF(TAG(N)==0) THEN 
              IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
               NMN_L = NMN_L + 1
               TAG(N) = 1
               NINDEX0= NINDEX0 + 1
               INDEX0(NINDEX0) = N
              ENDIF
            ENDIF
          END DO
C
#include "vectorize.inc"
          DO I=1,NINDEX0
            N = INDEX0(I)
            TAG(N) = 0
          END DO
C
          NINDEX0 = 0
          DO K=1,NSN
            N = INTBUF_TAB(NI)%NSV(K)
            IF(TAG(N)==0) THEN            
              IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
              NINDEX0 = NINDEX0 + 1 
              INDEX0(NINDEX0) = N
              NSN_L = NSN_L + 1
              TAG(N) = 1
             ENDIF
            ENDIF
          END DO
#include "vectorize.inc"
          DO K=1,NINDEX0
                TAG(INDEX0(K)) = 0
          ENDDO
          NINDEX0 = 0
C
C Prepare MULTIMP in any case
          I_STOK   = INTBUF_TAB(NI)%I_STOK(1)
          I_STOK_L = 0
          DO K = 1, I_STOK
            M = INTBUF_TAB(NI)%CAND_N(K)
            N = INTBUF_TAB(NI)%NSV(K)
            IF(NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L) THEN
              I_STOK_L = I_STOK_L + 1
            ENDIF
          ENDDO
C
          DO K=1,NRTS
            IF(INTERCEP(1,NI)%P(K)==PROC+1) THEN
              NRTS_L = NRTS_L + 1
            END IF
          END DO
C
          NCONT = NSN_L
C
          IF(NCONT > 0 .AND.I_STOK_L > MULTIMP*NCONT) THEN
            DO WHILE(I_STOK_L>MULTIMP*NCONT)
             MULTIMP=MULTIMP+4
            ENDDO
          ENDIF
C
          IPARI_L(3,NI) = NRTS_L
          IPARI_L(4,NI) = NRTM_L
          IPARI_L(5,NI) = NSN_L
          IPARI_L(6,NI) = NMN_L
          IPARI_L(8,NI) = NMN
          IPARI_L(18,NI)= NCONT
          IPARI_L(23,NI)= MULTIMP
          IPARI_L(24,NI)= 0
          IPARI_L(25,NI)= 0
C
C
          NSNT_L = NSNT_L + NSN_L
          NMNT_L = NMNT_L + NMN_L

C
          IMAXIMP_L = IMAXIMP_L + NCONT
          
C Fin cas interfaces type 21
C
        ENDIF
      ENDDO
C
      DEALLOCATE(TAGI2)
      DEALLOCATE(TAGG)
      RETURN
      END
