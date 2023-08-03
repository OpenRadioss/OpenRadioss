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
Chd|  SPMD_TRI25EGAT                source/mpi/interfaces/spmd_tri25egat.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI25EGAT(
     1   RESULT    ,NIN        , NEDGE   ,CANDS_E2E  ,I_STOK_E2E  ,
     2   CANDS_E2S  ,I_STOK_E2S,IGAP     ,INTFRIC    ,ISTIF_MSDT  )

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
#include      "i25edge_c.inc"
#include      "my_allocate.inc"
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: RESULT,NIN,NEDGE
      INTEGER :: I_STOK_E2E,I_STOK_E2S
      INTEGER :: CANDS_E2E(*),CANDS_E2S(*)
      INTEGER :: IGAP , INTFRIC
      INTEGER , INTENT(IN) :: ISTIF_MSDT
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER :: LOC_PROC,P 
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX,INDEX
      INTEGER :: N,NN,I,J,NNP
      INTEGER :: N1,N2
      INTEGER :: NEDGE_KEPT ! number of kept remote edges
      INTEGER :: IDEB
      INTEGER :: NODFI
      INTEGER :: LSKYFI
      INTEGER :: L2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C Test succes du tri ?
C


      IF(RESULT==0) THEN
C
C Reperage des candidats
C
        NEDGE_KEPT = 0
        DO I = 1, I_STOK_E2E
          N = CANDS_E2E(I)
          NN = N-NEDGE
          IF(NN>0)THEN
            IF(IREM_EDGE(1,NN)>0)THEN
              NEDGE_KEPT =  NEDGE_KEPT + 1
              IREM_EDGE(1,NN) = -IREM_EDGE(1,NN)
            ENDIF
          ENDIF
        ENDDO

        DO I = 1, I_STOK_E2S
          N = CANDS_E2S(I)
          NN = N-NEDGE
C         WRITE(6,*) "CAND(",I,")=",N,NN
          IF(NN>0)THEN
            IF(IREM_EDGE(1,NN)>0)THEN
              NEDGE_KEPT =  NEDGE_KEPT + 1
              IREM_EDGE(1,NN) = -IREM_EDGE(1,NN)
            ENDIF
          ENDIF
        ENDDO

C
C Allocation des tableaux de frontieres interfaces
C
        NODFI = NEDGE_KEPT * 2
C       IF(NEDGE_KEPT > ???) THEN
C  On pourrait eviter de faire des trous dans la memoire
          IF(ASSOCIATED(NSVFIE(NIN)%P)) DEALLOCATE(NSVFIE(NIN)%P)
C         WRITE(6,*) __FILE__,"NSVFIE allocated size:",NEDGE_KEPT
          ALLOCATE(NSVFIE(NIN)%P(NEDGE_KEPT))
          IF(ASSOCIATED(XFIE(NIN)%P)) DEALLOCATE(XFIE(NIN)%P)
          ALLOCATE(XFIE(NIN)%P(3,NODFI))
          IF(ASSOCIATED(VFIE(NIN)%P)) DEALLOCATE(VFIE(NIN)%P)
          ALLOCATE(VFIE(NIN)%P(3,NODFI))
          IF(ASSOCIATED(MSFIE(NIN)%P)) DEALLOCATE(MSFIE(NIN)%P)
          ALLOCATE(MSFIE(NIN)%P(NODFI))
          IF(ASSOCIATED(ITAFIE(NIN)%P)) DEALLOCATE(ITAFIE(NIN)%P)
          ALLOCATE(ITAFIE(NIN)%P(NODFI))

          IF(ASSOCIATED(GAPFIE(NIN)%P)) DEALLOCATE(GAPFIE(NIN)%P)
          ALLOCATE(GAPFIE(NIN)%P(NEDGE_KEPT))
C         IF(ASSOCIATED(MAIN_FIE(NIN)%P)) DEALLOCATE(MAIN_FIE(NIN)%P)
C         ALLOCATE(MAIN_FIE(NIN)%P(NEDGE_KEPT))
          IF( IGAP == 3) THEN
            IF(ASSOCIATED(GAPE_L_FIE(NIN)%P)) DEALLOCATE(GAPE_L_FIE(NIN)%P)
            ALLOCATE(GAPE_L_FIE(NIN)%P(NEDGE_KEPT))
          ENDIF
          IF(ASSOCIATED(STIFIE(NIN)%P)) DEALLOCATE(STIFIE(NIN)%P)
          ALLOCATE(STIFIE(NIN)%P(NEDGE_KEPT))

          IF(ISTIF_MSDT > 0) THEN
             IF(ASSOCIATED(STIFE_MSDT_FI(NIN)%P))DEALLOCATE(STIFE_MSDT_FI(NIN)%P)
             ALLOCATE(STIFE_MSDT_FI(NIN)%P(NODFI))        
          ENDIF

          IF(ASSOCIATED(EDG_BISECTOR_FIE(NIN)%P)) DEALLOCATE(EDG_BISECTOR_FIE(NIN)%P)
          ALLOCATE(EDG_BISECTOR_FIE(NIN)%P(3,3,NEDGE_KEPT))


          IF(ASSOCIATED(VTX_BISECTOR_FIE(NIN)%P)) DEALLOCATE(VTX_BISECTOR_FIE(NIN)%P)
          ALLOCATE(VTX_BISECTOR_FIE(NIN)%P(3,4,NEDGE_KEPT))

          IF(ASSOCIATED(LEDGE_FIE(NIN)%P)) DEALLOCATE(LEDGE_FIE(NIN)%P)
          ALLOCATE(LEDGE_FIE(NIN)%P(E_LEDGE_SIZE,NEDGE_KEPT))

C         Only for solids
          IF(ASSOCIATED(X_SEG_FIE(NIN)%P)) DEALLOCATE(X_SEG_FIE(NIN)%P)
          ALLOCATE(X_SEG_FIE(NIN)%P(3,4,NEDGE_KEPT))


        IF(IDTMINS == 2) THEN
          IF(ASSOCIATED(NODNXFIE(NIN)%P)) DEALLOCATE(NODNXFIE(NIN)%P)
          ALLOCATE(NODNXFIE(NIN)%P(NODFI))
          IF(ASSOCIATED(NODAMSFIE(NIN)%P)) DEALLOCATE(NODAMSFIE(NIN)%P)
          ALLOCATE(NODAMSFIE(NIN)%P(NODFI))
          IF(ASSOCIATED(PROCAMSFIE(NIN)%P)) DEALLOCATE(PROCAMSFIE(NIN)%P)
          ALLOCATE(PROCAMSFIE(NIN)%P(NODFI))
        ELSEIF(IDTMINS_INT /= 0) THEN
          IF(ASSOCIATED(NODAMSFIE(NIN)%P)) DEALLOCATE(NODAMSFIE(NIN)%P)
          ALLOCATE(NODAMSFIE(NIN)%P(NODFI))
          IF(ASSOCIATED(PROCAMSFIE(NIN)%P)) DEALLOCATE(PROCAMSFIE(NIN)%P)
          ALLOCATE(PROCAMSFIE(NIN)%P(NODFI))
        ENDIF 

        IF(INTFRIC > 0) THEN
          IF(ASSOCIATED(IPARTFRIC_FIE(NIN)%P)) DEALLOCATE(IPARTFRIC_FIE(NIN)%P)
          ALLOCATE(IPARTFRIC_FIE(NIN)%P(NEDGE_KEPT))
        ENDIF




C       ENDIF

        ALLOCATE(INDEX(NEDGE_REMOTE))
C
C Compactage des candidats
C
        IDEB = 0
        NN = 0
        DO P = 1, NSPMD
          NNP = NN
! number of remote edges received from processor P in XREM
          NEDGE_REMOTE_OLD = NSNFIE(NIN)%P(P)         
C         WRITE(6,*) ISPMD,"EDGE REMOTE TOTAL=",NEDGE_REMOTE_OLD
          IF(NEDGE_REMOTE_OLD/=0) THEN
            DO I = 1, NEDGE_REMOTE_OLD
              IF(IREM_EDGE(1,I+IDEB)<0) THEN          
                 IREM_EDGE(1,I+IDEB) = - IREM_EDGE(1,I+IDEB)                 
! edge that are candidates     
                NN = NN + 1
                INDEX(I+IDEB) = NN
                ASSERT(IREM_EDGE(E_LOCAL_ID,I+IDEB) > 0)
c               WRITE(6,*) "KEEP",IREM_EDGE(E_LOCAL_ID,I+IDEB)
                NSVFIE(NIN)%P(NN) = IREM_EDGE(E_LOCAL_ID,I+IDEB)      
                LEDGE_FIE(NIN)%P(1:E_LEDGE_SIZE,NN) = IREM_EDGE(1:E_LEDGE_SIZE ,I+IDEB)

C====================== AMS
                IF(IDTMINS /= 0) THEN
                  N1 = 2*(NN-1)+1
                  N2 = 2*NN
                  IF(IDTMINS/=2 .AND. IDTMINS_INT == 0) THEN

                  ELSEIF(IDTMINS==2) THEN
                    NODNXFIE(NIN)%P(N1)   = IREM_EDGE(E_NODNX1,I+IDEB)
                    NODAMSFIE(NIN)%P(N1)  = IREM_EDGE(E_NODAMS1,I+IDEB)
                    PROCAMSFIE(NIN)%P(N1) = P
                    NODNXFIE(NIN)%P(N2)   = IREM_EDGE(E_NODNX2,I+IDEB)
                    NODAMSFIE(NIN)%P(N2)  = IREM_EDGE(E_NODAMS2,I+IDEB)
                    PROCAMSFIE(NIN)%P(N2) = P

                    ASSERT(NODNXFIE(NIN)%P(N1) >= 0)
                    ASSERT(NODNXFIE(NIN)%P(N2) >= 0)
C                   IF(NODNXFIE(NIN)%P(N1) < 0 .OR. NODNXFIE(NIN)%P(N2)<0 ) THEN
C                     WRITE(6,"(A,X,4I10)") __FILE__,NN,I+IDEB, NODNXFIE(NIN)%P(N1), NODNXFIE(NIN)%P(N2)
C                   ENDIF

                  ELSE ! IDTMINS_INT == 0
                    NODAMSFIE(NIN)%P(N1)  = IREM_EDGE(E_NODAMS1,I+IDEB)
                    PROCAMSFIE(NIN)%P(N1) = P
                    NODAMSFIE(NIN)%P(N2)  = IREM_EDGE(E_NODAMS2,I+IDEB)
                    PROCAMSFIE(NIN)%P(N2) = P
                  ENDIF
                ENDIF ! IDTMINS /= 0

                IF(INTFRIC > 0) THEN
                   IPARTFRIC_FIE(NIN)%P(NN) = IREM_EDGE(E_IPARTFRIC_E,I+IDEB)      
                ENDIF
C=======================================================================

                DEBUG_E2E(LEDGE_FIE(NIN)%P(E_GLOBAL_ID,NN)==D_ES,NN)

                N1 = 2*(NN-1)+1
                XFIE(NIN)%P(1,N1) = XREM_EDGE(E_X1,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM_EDGE(E_Y1,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM_EDGE(E_Z1,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM_EDGE(E_VX1,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM_EDGE(E_VY1,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM_EDGE(E_VZ1,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM_EDGE(E_MS1,I+IDEB)
                ITAFIE(NIN)%P(N1) = IREM_EDGE(E_NODE1_GLOBID,I+IDEB)
                N2 = 2*NN
                XFIE(NIN)%P(1,N2) = XREM_EDGE(E_X2,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM_EDGE(E_Y2,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM_EDGE(E_Z2,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM_EDGE(E_VX2,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM_EDGE(E_VY2,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM_EDGE(E_VZ2,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM_EDGE(E_MS2,I+IDEB)
                ITAFIE(NIN)%P(N2) = IREM_EDGE(E_NODE2_GLOBID,I+IDEB)
           
                GAPFIE(NIN)%p(NN) = XREM_EDGE(E_GAP,I+IDEB)

                IF(IGAP == 3) THEN 
                  GAPE_L_FIE(NIN)%P(NN) = XREM_EDGE(E_GAPL,I+IDEB)
                ENDIF
                 
                STIFIE(NIN)%p(NN) = XREM_EDGE(E_STIFE,I+IDEB)

                IF(ISTIF_MSDT > 0) THEN
                   STIFE_MSDT_FI(NIN)%P(NN) = XREM_EDGE(E_STIFE_MSDT_FI,I+IDEB)      
                ENDIF

C               MAIN_FIE(NIN)%P(NN) = XREM_EDGE(E_MAIN,I+IDEB)
                
                L2 = E_EDG_BIS 
C Simple -> double -> simple = pb PON? 
                EDG_BISECTOR_FIE(NIN)%p(1:3,1,NN) = XREM_EDGE(L2:L2+2,I+IDEB)
                L2 = E_VTX_BIS
                VTX_BISECTOR_FIE(NIN)%p(1:3,1,NN) = XREM_EDGE(L2:L2+2,I+IDEB)
                L2 = L2 + 3
                VTX_BISECTOR_FIE(NIN)%p(1:3,2,NN) = XREM_EDGE(L2:L2+2,I+IDEB)
                L2 = L2 + 3
                VTX_BISECTOR_FIE(NIN)%p(1:3,3,NN) = XREM_EDGE(L2:L2+2,I+IDEB)
                L2 = L2 + 3
                VTX_BISECTOR_FIE(NIN)%p(1:3,4,NN) = XREM_EDGE(L2:L2+2,I+IDEB)
                L2 = L2 + 3
                EDG_BISECTOR_FIE(NIN)%p(1:3,2,NN) = XREM_EDGE(L2:L2+2,I+IDEB)
                L2 = L2 + 3
                EDG_BISECTOR_FIE(NIN)%p(1:3,3,NN) = XREM_EDGE(L2:L2+2,I+IDEB)



              ENDIF ! Kept edge
            ENDDO  ! NEDGE_REMOTE_OLD 
            IDEB = IDEB + NEDGE_REMOTE_OLD    
          ENDIF !IF(NEDGE_REMOTE_OLD/=0) 
          ASSERT(NN - NNP >= 0)
          NSNFIE(NIN)%P(P) = NN-NNP 
        ENDDO  ! end do NSPMD           
        LSKYFI = NN*MULTIMAX
Cel nsnr nouveau utile pour inacti
        NEDGE_REMOTE = NN
C      WRITE(6,*) ISPMD,"EDGE REMOTE kept=",NEDGE_REMOTE

      ENDIF
      NEDGE_REMOTE_OLD = NEDGE_REMOTE

C
C Deallocation de XREM IREM
C
      IF(ALLOCATED(IREM_EDGE)) DEALLOCATE(IREM_EDGE)
      IF(ALLOCATED(XREM_EDGE)) DEALLOCATE(XREM_EDGE)


C
C Allocation Parith/OFF
C
      IF(IPARIT==0) THEN
        IF(ASSOCIATED(AFIE(NIN)%P)) DEALLOCATE(AFIE(NIN)%P)
        IF(ASSOCIATED(STNFIE(NIN)%P)) DEALLOCATE(STNFIE(NIN)%P)
        IF(NODFI>0)ALLOCATE(AFIE(NIN)%P(3,NODFI*NTHREAD))
        IF(NODFI>0)ALLOCATE(STNFIE(NIN)%P(NODFI*NTHREAD))
C Init a 0
        DO I = 1, NODFI*NTHREAD
          AFIE(NIN)%P(1,I) = ZERO
          AFIE(NIN)%P(2,I) = ZERO
          AFIE(NIN)%P(3,I) = ZERO
          STNFIE(NIN)%P(I) = ZERO
        ENDDO
C
        IF(KDTINT/=0)THEN
          IF(ASSOCIATED(VSCFIE(NIN)%P)) DEALLOCATE(VSCFIE(NIN)%P)
          IF(NODFI>0)ALLOCATE(VSCFIE(NIN)%P(NODFI*NTHREAD))
C Init a 0
          DO I = 1, NODFI*NTHREAD
            VSCFIE(NIN)%P(I) = ZERO
          ENDDO
        ENDIF
C
        NLSKYFIE(NIN) = NODFI
C
      ELSE
C
C Allocation Parith/ON Done in upgrade_rem_slv
C
          IF(ASSOCIATED(FSKYFIE(NIN)%P)) DEALLOCATE(FSKYFIE(NIN)%P)
          IF(ASSOCIATED(ISKYFIE(NIN)%P)) DEALLOCATE(ISKYFIE(NIN)%P)
          NULLIFY(FSKYFIE(NIN)%P)
          NULLIFY(ISKYFIE(NIN)%P)
          NLSKYFIE(NIN) = LSKYFI
          IF(LSKYFI>0) THEN
            ALLOCATE(ISKYFIE(NIN)%P(LSKYFI))
            ISKYFIE(NIN)%P(1:LSKYFI) = 0
c           IF(KDTINT==0) THEN
c             ALLOCATE(FSKYFIE(NIN)%P(8,LSKYFI))
c           ELSE
              NFSKYIE = 8
C             WRITE(6,*) "ALLOCATE FSKYFIE",NFSKYIE,LSKYFI
              ALLOCATE(FSKYFIE(NIN)%P(NFSKYIE,LSKYFI))
              FSKYFIE(NIN)%P(1:NFSKYIE,1:LSKYFI) = 0
c           ENDIF
          ENDIF

      ENDIF

C
C Renumerotation des candidats
C
      DO I = 1, I_STOK_E2E
        N = CANDS_E2E(I)
        NN = N-NEDGE
        IF(NN>0)THEN
          CANDS_E2E(I) = INDEX(NN)+NEDGE
        ENDIF
      ENDDO
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK_E2S
        N = CANDS_E2S(I)
        NN = N-NEDGE
        IF(NN>0)THEN
          CANDS_E2S(I) = INDEX(NN)+NEDGE
        ENDIF
      ENDDO


      DEALLOCATE(INDEX)
C
#endif
      RETURN
      END

