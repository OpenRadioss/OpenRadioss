Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I25OPTCD_E2S                  source/interfaces/intsort/i25optcd_e2s.F
Chd|-- called by -----------
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        INT_CHECKSUM                  share/modules/debug_mod.F     
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25OPTCD_E2S(CAND_M,CAND_S,X     ,I_STOK,IRECT   ,
     2                        NIN   ,V     ,GAP_M,IGAP  ,ITASK ,
     3                        STF   ,GAP_M_L,COUNT_REMSLVE,DRAD  ,
     4                        IEDGE ,NEDGE  ,LEDGE  ,MVOISIN,NSV,
     5                        NRTM  ,GAPE   ,GAP_E_L,IGAP0, 
     6                        STFE, S_STFE  ,IFQ    ,IFPEN,
     7                        CAND_FX,CAND_FY,CAND_FZ,DGAPLOAD)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
#ifdef WITH_ASSERT
      USE DEBUG_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "assert.inc"
#include      "i25edge_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: S_STFE
      INTEGER IRECT(4,*),CAND_M(*), CAND_S(*), IFPEN(*),
     .        I_STOK, NIN,IGAP ,ITASK, COUNT_REMSLVE(*), 
     .        IEDGE, NEDGE, LEDGE(NLEDGE,*), MVOISIN(4,*), NSV(*), NRTM, IGAP0,IFQ
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .        X(3,*),GAP_M(*),V(3,*),STF(*),GAP_M_L(*), GAPE(*), GAP_E_L(*),
     .        STFE(S_STFE), CAND_FX(4,*),CAND_FY(4,*),CAND_FZ(4,*) 
     
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , L, M, E, IE, JE, NN1, NN2, IL, JL, I1, I2, SOL_EDGE, SH_EDGE, 
     .        NRTMFT, NRTMLT
      my_real
     .        XI,YI,ZI,X1S,X2S,Y1S,Y2S,Z1S,Z2S,
     .        X1,X2,X3,X4,
     .        Y1,Y2,Y3,Y4,
     .        Z1,Z2,Z3,Z4,
     .        X5,X6,X7,X8,
     .        Y5,Y6,Y7,Y8,
     .        Z5,Z6,Z7,Z8,
     .        XMINS,XMAXS,YMINS,YMAXS,ZMINS,ZMAXS,
     .        XMINM,XMAXM,YMINM,YMAXM,ZMINM,ZMAXM,
     .        XMINM_1,XMAXM_1,YMINM_1,YMAXM_1,ZMINM_1,ZMAXM_1,
     .        XMINM_2,XMAXM_2,YMINM_2,YMAXM_2,ZMINM_2,ZMAXM_2,DXM,DYM,DZM,
     .        V12,V22,V32,V42,VV,GAPVD,S
      INTEGER MSEG,CT
      my_real
     .        GAPV(MVSIZ),DTTI(MVSIZ)
      INTEGER LIST(MVSIZ), LISTI(MVSIZ)
      INTEGER IS,JS,LS,NLS,NLT,NSEG,NLF,II,NLS2,NLSAV
      INTEGER N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      INTEGER SG, FIRST, LAST,COUNT_CAND
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGM
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: BOXM
      SAVE TAGM,BOXM
      INTEGER :: IDS(4)
      INTEGER :: J
      INTEGER :: MY_SIZE
      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_REAL_CANDIDATE

C-----------------------------------------------
      SOL_EDGE=IEDGE/10 ! solids
      SH_EDGE =IEDGE-10*SOL_EDGE ! shells
C-----------------------------------------------
      COUNT_CAND = 0
      CT = 0
      MSEG = NVSIZ
      FIRST = 1 + I_STOK*ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      MY_SIZE = LAST - FIRST + 1
      ALLOCATE( LIST_REAL_CANDIDATE(MY_SIZE) )
C-----------------------------------------------
      IF(SOL_EDGE/=0)THEN

        IF(ITASK==0)THEN
          ALLOCATE(TAGM(NRTM),BOXM(6,NRTM))
          TAGM(1:NRTM)=0
        END IF

        CALL MY_BARRIER

        DO I=FIRST,LAST
          L  = CAND_M(I)
          IF(STF(L)/=ZERO)TAGM(L)=1
        END DO

        CALL MY_BARRIER

        NRTMFT=1 + NRTM*ITASK / NTHREAD
        NRTMLT=NRTM*(ITASK+1) / NTHREAD

        DO L=NRTMFT,NRTMLT

          BOXM(1,L)= EP30
          BOXM(2,L)=-EP30
          BOXM(3,L)= EP30
          BOXM(4,L)=-EP30
          BOXM(5,L)= EP30
          BOXM(6,L)=-EP30

          IF(TAGM(L)/=0)THEN

            X1=X(1,IRECT(1,L))
            Y1=X(2,IRECT(1,L))
            Z1=X(3,IRECT(1,L))
            X2=X(1,IRECT(2,L))
            Y2=X(2,IRECT(2,L))
            Z2=X(3,IRECT(2,L))
            X3=X(1,IRECT(3,L))
            Y3=X(2,IRECT(3,L))
            Z3=X(3,IRECT(3,L))
            X4=X(1,IRECT(4,L))
            Y4=X(2,IRECT(4,L))
            Z4=X(3,IRECT(4,L))

            XMINM = MIN(X1,X2,X3,X4)
            XMAXM = MAX(X1,X2,X3,X4)
            DXM   = EM02*(XMAXM-XMINM)
            XMINM = XMINM-DXM
            XMAXM = XMAXM+DXM

            YMINM = MIN(Y1,Y2,Y3,Y4)
            YMAXM = MAX(Y1,Y2,Y3,Y4)
            DYM   = EM02*(YMAXM-YMINM)
            YMINM = YMINM-DYM
            YMAXM = YMAXM+DYM

            ZMINM = MIN(Z1,Z2,Z3,Z4)
            ZMAXM = MAX(Z1,Z2,Z3,Z4)
            DZM   = EM02*(ZMAXM-ZMINM)
            ZMINM = ZMINM-DZM
            ZMAXM = ZMAXM+DZM

            BOXM(1,L)=XMINM
            BOXM(2,L)=XMAXM
            BOXM(3,L)=YMINM
            BOXM(4,L)=YMAXM
            BOXM(5,L)=ZMINM
            BOXM(6,L)=ZMAXM

          END IF
        END DO

        CALL MY_BARRIER

      END IF ! IF(SOL_EDGE/0)THEN
C-----
      JS = FIRST-1
      DO SG = FIRST,LAST,MSEG
       NSEG = MIN(MSEG,LAST-JS)
C-----------------------------------------------
C      solid edges on main side
C-----------------------------------------------
       IF(NSPMD>1) THEN
C
C Partage cand_n local / frontiere
C
         NLS = 0
         NLS2 = NSEG+1
         DO IS = 1, NSEG
           I=JS+IS
           IF(CAND_S(I)<=NEDGE)THEN

             IF(SH_EDGE==1.AND.
     .          LEDGE(7,CAND_S(I))/=1.AND.
     .          LEDGE(3,CAND_S(I))/=0) CYCLE ! Shell edge is not a free edge
             NLS=NLS+1
             LISTI(NLS)=IS

#ifdef WITH_ASSERT
C Debug only
             IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
             IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
             IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
             IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
             DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,I)
#endif

           ELSE ! REMOTE
             IF(SH_EDGE==1.AND.
     .          LEDGE_FIE(NIN)%P(E_TYPE,CAND_S(I)-NEDGE)/=1.AND.
     .          LEDGE_FIE(NIN)%P(E_RIGHT_SEG,CAND_S(I)-NEDGE)/=0) CYCLE ! Shell edge is not a free edge
             NLS2=NLS2-1
             ASSERT(IS <= MVSIZ) 
             ASSERT(IS > 0)
             LISTI(NLS2) = IS
#ifdef WITH_ASSERT
C Debug only
             IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
             IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
             IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
             IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
             DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE),I)                           
#endif
           ENDIF
         ENDDO
C LISTI(1:NLS) = Local
C LISTI(NLS+1:NSEG) = REMOTE

CC LOCAL OF SPMD
C#include      "vectorize.inc"
C         DO LS = 1, NLS
C           IS = LISTI(LS)
C           I=JS+IS
C           IE=CAND_M(I) ! segment, zero gap
C           JE=CAND_S(I) ! edge
C           GAPV(IS)=GAPE(JE)
C           IF(SH_EDGE /= 0 .AND. IGAP0 /= 0) GAPV(IS)=TWO*GAPV(IS)
C           IF(IGAP==3)
C     .       GAPV(IS)=MIN(GAPV(IS),GAP_M_L(IE)+GAP_E_L(JE))
C           GAPV(IS)=MAX(GAPV(IS),DRAD)
C         ENDDO

       ELSE ! NSPMD == 1
         NLS = 0
C Build LISTI
         DO IS=1,NSEG
           I=JS+IS
           IF(SH_EDGE==1.AND.
     .        LEDGE(7,CAND_S(I))/=1.AND.
     .        LEDGE(3,CAND_S(I))/=0) CYCLE ! Shell edge is not a free edge


#ifdef WITH_ASSERT
         IDS(1) = ITAB_DEBUG(IRECT(1,CAND_M(I)))
         IDS(2) = ITAB_DEBUG(IRECT(2,CAND_M(I)))
         IDS(3) = ITAB_DEBUG(IRECT(3,CAND_M(I)))
         IDS(4) = ITAB_DEBUG(IRECT(4,CAND_M(I)))
         DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM.AND.LEDGE(8,CAND_S(I)) == D_ES,I)
#endif

           NLS=NLS+1
           LISTI(NLS)=IS
         ENDDO
       ENDIF ! END IF SPMD


C========== LOCAL
#include      "vectorize.inc"
       DO LS = 1, NLS
         IS = LISTI(LS)
         I=JS+IS
         IE=CAND_M(I) ! segment, zero gap
         JE=CAND_S(I) ! edge
         GAPV(IS)=GAPE(JE)
         IF(SH_EDGE /= 0 .AND. IGAP0 /= 0) GAPV(IS)=TWO*GAPV(IS)
         IF(IGAP==3)
     .     GAPV(IS)=MIN(GAPV(IS),GAP_M_L(IE)+GAP_E_L(JE))
         GAPV(IS)=MAX(GAPV(IS)+DGAPLOAD,DRAD)
       ENDDO

C
       NLF = 1
       NLT = NLS
       NLS = 0
C ======== LOCAL TIRM 1
#include      "vectorize.inc"
       DO LS = NLF, NLT
         IS = LISTI(LS)
         I = JS + IS
C LEDGE(1) can be < 0 if:
C - the edge it on a boundary between two domains
C - The logal segment is broken
         L  = LEDGE(1,CAND_S(I))
         S = ZERO
#ifdef WITH_ASSERT
         IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
         IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
         IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
         IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
         DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,L)
         DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,LEDGE(LEDGE_WEIGHT,CAND_S(I)))
#endif

         IF( L > 0 ) THEN
           S = STFE(CAND_S(I))
         ELSEIF (L < 0) THEN
C Boundary edge, owned by ISPMD, but local segment broken
C In that case, we assume that the segment on the 
C other side of the boundary is not broken
           S = ONE
C          S = ZERO
         ENDIF

#ifdef WITH_ASSERT
         DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,S)
#endif

         IF (S/=ZERO) THEN
           N1(IS)= LEDGE(5,CAND_S(I))
           N2(IS)= LEDGE(6,CAND_S(I))
           Z1S=X(3,N1(IS))
           Z2S=X(3,N2(IS))
           GAPVD = GAPV(IS)
           ZMINS = MIN(Z1S,Z2S)-GAPVD
           ZMAXS = MAX(Z1S,Z2S)+GAPVD
           ZMINM = BOXM(5,CAND_M(I))
           ZMAXM = BOXM(6,CAND_M(I))
           ZMINM = ZMINM-GAPVD
           ZMAXM = ZMAXM+GAPVD
#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,ZMAXS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,ZMINS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,ZMAXM)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,ZMINM)
#endif

           IF (ZMAXS>=ZMINM.AND.ZMAXM>=ZMINS) THEN
             NLS=NLS+1
             LIST(NLS)=IS
           ENDIF
         ENDIF
       ENDDO
C
       NLT=NLS
       NLS=0
C ======== LOCAL TIRM 2
#include      "vectorize.inc"
       DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          Y1S=X(2,N1(IS))
          Y2S=X(2,N2(IS))
          GAPVD = GAPV(IS)
          YMINS = MIN(Y1S,Y2S)-GAPVD
          YMAXS = MAX(Y1S,Y2S)+GAPVD
          YMINM = BOXM(3,CAND_M(I))
          YMAXM = BOXM(4,CAND_M(I))
          YMINM = YMINM-GAPVD
          YMAXM = YMAXM+GAPVD
#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,YMAXS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,YMINS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,YMAXM)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,YMINM)
#endif

          IF (YMAXS>=YMINM.AND.YMAXM>=YMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
       ENDDO
C
       NLT = NLS
       NLS=0
C ======== LOCAL TIRM 3
#include      "vectorize.inc"
       DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          X1S=X(1,N1(IS))
          X2S=X(1,N2(IS))
          GAPVD = GAPV(IS)
          XMINS = MIN(X1S,X2S)-GAPVD
          XMAXS = MAX(X1S,X2S)+GAPVD
          XMINM = BOXM(1,CAND_M(I))
          XMAXM = BOXM(2,CAND_M(I))
          XMINM = XMINM-GAPVD
          XMAXM = XMAXM+GAPVD
#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,XMAXS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,XMINS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,XMAXM)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,XMINM)
#endif

          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
            LIST_REAL_CANDIDATE(COUNT_CAND) = I
          ENDIF
       ENDDO
C
       IF(NSPMD>1)THEN
         NLF = NLS2
         NLT = NSEG
C =========  REMOTE 
         DO LS = NLF, NLT
           IS = LISTI(LS)
           I=JS+IS
           IE=CAND_M(I)
           GAPV(IS)=GAPFIE(NIN)%P(CAND_S(I)-NEDGE)
           IF(SH_EDGE /= 0 .AND. IGAP0 /= 0) GAPV(IS)=TWO*GAPV(IS)

           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),GAPE_L_FIE(NIN)%P(CAND_S(I)-NEDGE)+GAP_M_L(IE))
           GAPV(IS)=MAX(GAPV(IS)+DGAPLOAD,DRAD)
         ENDDO
C
         NLS=0
C =========  REMOTE TRIM 1
#include      "vectorize.inc"
         DO LS = NLF, NLT
C   conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          II = CAND_S(I)-NEDGE
#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,STIFIE(NIN)%P(II))
#endif

          IF (STIFIE(NIN)%P(II)/=ZERO) THEN
           NN1 = 2*(II-1)+1
           NN2 = 2*II
           Z1S=XFIE(NIN)%P(3,NN1)
           Z2S=XFIE(NIN)%P(3,NN2)
C          L  = LEDGE(1,CAND_M(I))
C          IF (STF(L)/=ZERO) THEN
           GAPVD = GAPV(IS)
           ZMINS = MIN(Z1S,Z2S)-GAPVD
           ZMAXS = MAX(Z1S,Z2S)+GAPVD
           ZMINM = BOXM(5,CAND_M(I))
           ZMAXM = BOXM(6,CAND_M(I))
           ZMINM = ZMINM-GAPVD
           ZMAXM = ZMAXM+GAPVD
#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,ZMAXS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,ZMINS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,ZMAXM)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,ZMINM)
#endif

           IF (ZMAXS>=ZMINM.AND.ZMAXM>=ZMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
           ENDIF
C          ENDIF
          ENDIF
        ENDDO
C
        NLF=1
        NLT=NLS
        NLS=0
C ============= REMOTE TRIM 2
#include      "vectorize.inc"
        DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          II = CAND_S(I)-NEDGE
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          Y1S=XFIE(NIN)%P(2,NN1)
          Y2S=XFIE(NIN)%P(2,NN2)


          GAPVD = GAPV(IS)
          YMINS = MIN(Y1S,Y2S)-GAPVD
          YMAXS = MAX(Y1S,Y2S)+GAPVD
          YMINM = BOXM(3,CAND_M(I))
          YMAXM = BOXM(4,CAND_M(I))
          YMINM = YMINM-GAPVD
          YMAXM = YMAXM+GAPVD

#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,YMAXS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,YMINS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,YMAXM)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,YMINM)
#endif

          IF (YMAXS>=YMINM.AND.YMAXM>=YMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
        ENDDO
C ================ REMOTE TRIM 3
#include      "vectorize.inc"
        DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          II = CAND_S(I)-NEDGE
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          X1S=XFIE(NIN)%P(1,NN1)
          X2S=XFIE(NIN)%P(1,NN2)
          GAPVD = GAPV(IS)
          XMINS = MIN(X1S,X2S)-GAPVD
          XMAXS = MAX(X1S,X2S)+GAPVD
          XMINM = BOXM(1,CAND_M(I))
          XMAXM = BOXM(2,CAND_M(I))
          XMINM = XMINM-GAPVD
          XMAXM = XMAXM+GAPVD
#ifdef WITH_ASSERT
            IDS(1) =  ITAB_DEBUG(IRECT(1,CAND_M(I)))
            IDS(2) =  ITAB_DEBUG(IRECT(2,CAND_M(I)))
            IDS(3) =  ITAB_DEBUG(IRECT(3,CAND_M(I)))
            IDS(4) =  ITAB_DEBUG(IRECT(4,CAND_M(I)))
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,XMAXS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,XMINS)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,XMAXM)
            DEBUG_E2E(INT_CHECKSUM(IDS,4,1)==D_EM .AND. LEDGE_FIE(NIN)%P(E_GLOBAL_ID,CAND_S(I)-NEDGE) == D_ES,XMINM)
#endif

          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN

            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
            CT = CT+1
            LIST_REAL_CANDIDATE(COUNT_CAND) = I

          ENDIF
        ENDDO
        CALL SYNC_DATA(NLS2)
       END IF
       JS = JS + NSEG
      ENDDO
      IF (IFQ > 0) THEN
#include "vectorize.inc"
        DO J=1,COUNT_CAND
          I = LIST_REAL_CANDIDATE(J)
          IF(IFPEN(I) == 0 ) THEN
            CAND_FX(1:4,I) = ZERO
            CAND_FY(1:4,I) = ZERO
            CAND_FZ(1:4,I) = ZERO
          ENDIF
        ENDDO
        DO I=FIRST,LAST
          IFPEN(I) = 0
        ENDDO
      ENDIF
C
#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5 
      COUNT_REMSLVE(NIN)=COUNT_REMSLVE(NIN)+CT
#include "lockoff.inc"

C
C
      IF(SOL_EDGE/=0)THEN

        CALL MY_BARRIER

        IF(ITASK==0)DEALLOCATE(TAGM,BOXM)

      END IF

      DEALLOCATE( LIST_REAL_CANDIDATE )
C
      RETURN
      END

