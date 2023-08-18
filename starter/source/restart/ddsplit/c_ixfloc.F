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
Chd|  C_IXFLOC                      source/restart/ddsplit/c_ixfloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE C_IXFLOC(NUMEL,IXSF,IXQF,IXTGF,NSVOIS,
     +                    NQVOIS,NTGVOIS,PROC,IPARG,CEP,CEL,
     +                    ALE_CONNECTIVITY,ee_connect_l,IXS,IXQ,IXTG,NODLOCAL,NUMELS_L,NUMELQ_L,NUMELTG_L,MULTI_FVM,
     +                    ID_GLOBAL_VOIS,INDX_S,INDX_Q,INDX_TG,FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,FACE_VOIS)
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSVOIS, NQVOIS, NTGVOIS, PROC, NUMEL, NSEGFL_L,
     .        NUMELS_L,NUMELQ_L,NUMELTG_L,
     .        IXSF(NIXS,NSVOIS) ,IXQF(NIXQ,NQVOIS),IXTGF(NIXTG,NTGVOIS),
     .        IPARG(NPARG,*),CEP(*),CEL(*),
     .        IXS(NIXS,*), IXQ(NIXQ,*), IXTG(NIXTG,*),NODLOCAL(*)
      INTEGER, DIMENSION(*), INTENT(OUT) :: ID_GLOBAL_VOIS,FACE_VOIS
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      ! index for id of the remote connected element
      INTEGER, DIMENSION(*), INTENT(in) :: INDX_S
      INTEGER, DIMENSION(*), INTENT(in) :: INDX_Q
      INTEGER, DIMENSION(*), INTENT(in) :: INDX_TG
      ! id of the remote connected element
      INTEGER, DIMENSION(6*NUMELS,*), INTENT(in) :: FACE_ELM_S
      INTEGER, DIMENSION(4*NUMELQ,*), INTENT(in) :: FACE_ELM_Q
      INTEGER, DIMENSION(3*NUMELTG,*), INTENT(in) :: FACE_ELM_TG
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE(t_connectivity_ext1), INTENT(INOUT) :: ee_connect_l
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NG, IFS, IFQ, IFTG,IE_LOC, IV_LOC,
     .        ISOLNOD,ILAW,JTUR,JTHE,ITY,IE,NFT,NEL,IV,PROC2,NFT_LOC,IAD1, LGTH, IAD2
      INTEGER :: NUMEL_L, TMP, IALEUL
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGE, nb_connect_l
!
!     Number of local elements
      NUMEL_L = 0 
      DO NG = 1, NGROUP
         IF (IPARG(32, NG) == PROC) THEN
            NEL = IPARG(2, NG)
            NUMEL_L = NUMEL_L + NEL
         ENDIF
      ENDDO
      ALLOCATE(ee_connect_l%iad_connect(NUMEL_L + 1))
      ee_connect_l%iad_connect(1:NUMEL_L + 1) = 0
      ALLOCATE(nb_connect_l(NUMEL_L))
      nb_connect_l(1:NUMEL_L) = 0

!     Counting elements connected to local elements
      NFT_LOC = 0
      DO NG = 1, NGROUP
         IF (IPARG(32, NG) == PROC) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            IALEUL = IPARG(7, NG) + IPARG(11, NG)
            IF (IALEUL /= 0) THEN
               DO I = 1, NEL
!     Index of the element
                  IE = I + NFT
!     Local index of the element
                  IE_LOC = I + NFT_LOC
!     Number of neighbors
                  LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                  nb_connect_l(IE_LOC) = LGTH
               ENDDO
            ENDIF
            NFT_LOC = NFT_LOC + NEL
         ENDIF
      ENDDO
      ee_connect_l%iad_connect(1) = 1
      DO I = 2, NUMEL_L + 1
         ee_connect_l%iad_connect(I) = ee_connect_l%iad_connect(I - 1) + nb_connect_l(I - 1)
      ENDDO
      TMP = ee_connect_l%iad_connect(NUMEL_L + 1) - 1
      ALLOCATE(ee_connect_l%connected(TMP))
      ALLOCATE(ee_connect_l%type(TMP))
      ALLOCATE(ee_connect_l%iface2(TMP))
C
      ALLOCATE(TAGE(NUMEL))
      DO I = 1, NUMEL
        TAGE(I) = 0
      ENDDO
      IFS = 0
      IFQ = 0
      IFTG = 0
      NFT_LOC = 0
C
      DO NG=1,NGROUP
       JTUR=IPARG(12,NG)
       JTHE=IPARG(13,NG)
       IF(IPARG(32,NG)==PROC) THEN
         NEL = IPARG(2,NG)
         NFT = IPARG(3,NG)
         ITY = IPARG(5,NG)
C   voir autres types solides
         ISOLNOD = IPARG(28,NG)
C   3D
         IF(ITY==1) THEN
           DO I = 1, NEL
             IE = I+NFT
             IE_lOC = I+NFT_LOC
             IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             TAGE(IE) = CEL(IE)
C   IVOIS
             DO J = 1, LGTH
               IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
               IAD2 = ee_connect_l%iad_connect(IE_LOC) + J - 1
               IF (IV>0) THEN
                 PROC2 = CEP(IV)
                 IV_LOC = CEL(IV)
                 IF(PROC2/=PROC) THEN
C   element frontiere
                   IF(TAGE(IV)==0) THEN
                     IFS = IFS + 1
                     TAGE(IV) = NUMELS_L+IFS
C   recopie IXS + chgt no local des noeuds
                     DO K = 1,1
                       IXSF(K,IFS) = IXS(K,IV)
                     ENDDO
                     DO K = 2,9
                       IXSF(K,IFS) = NODLOCAL(IXS(K,IV))
                     ENDDO
                     DO K = 10,NIXS
                       IXSF(K,IFS) = IXS(K,IV)
                     ENDDO
                   ENDIF
C   ivois     
                   ee_connect_l%connected(IAD2) = TAGE(IV)
                   
                   ID_GLOBAL_VOIS( (IE_LOC-1)*6+J ) = IXS(NIXS,IV) ! pas sur le proc courant
                   DO K=1,INDX_S(IE)
                        IF( IXS(NIXS,IV)==FACE_ELM_S(6*(IE-1)+K,2) ) THEN
                            FACE_VOIS( (IE_LOC-1)*6+J ) = FACE_ELM_S(6*(IE-1)+K,1) ! pas sur le proc courant
                        ENDIF                     
                   ENDDO
                 ELSE
C   element interne
                   ee_connect_l%connected(IAD2) = IV_LOC
                   ID_GLOBAL_VOIS( (IE_LOC-1)*6+J ) = IXS(NIXS,IV) ! sur le proc courant
                 ENDIF
               ELSE
                 ee_connect_l%connected(IAD2) = IV
                 ID_GLOBAL_VOIS( (IE_LOC-1)*6+J ) = 0  ! pas de voisin
               ENDIF
               ee_connect_l%type(IAD2) = ALE_CONNECTIVITY%ee_connect%type(IAD1 + J - 1)
               ee_connect_l%iface2(IAD2) = ALE_CONNECTIVITY%ee_connect%iface2(IAD1 + J - 1)  
             ENDDO
           ENDDO
           NFT_LOC = NFT_LOC + NEL
         ELSEIF(ITY==2) THEN
C   2D
           DO I = 1, NEL
             IE = I+NFT
             IE_lOC = I+NFT_LOC
             IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             TAGE(IE) = CEL(IE)
C   IVOIS
             DO J = 1, LGTH
               IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
               IAD2 = ee_connect_l%iad_connect(IE_LOC) + J - 1
               IF (IV>0) THEN
                 PROC2  = CEP(IV)
                 IV_LOC = CEL(IV)
                 IF(PROC2/=PROC) THEN
C   element frontiere
                   IF(TAGE(IV)==0) THEN
                     IFQ = IFQ + 1
                     TAGE(IV) = NUMELQ_L+IFQ
C   recopie IXQ + chgt no local des noeuds
                     DO K = 1,1
                       IXQF(K,IFQ) = IXQ(K,IV)
                     ENDDO
                     DO K = 2,5
                       IXQF(K,IFQ) = NODLOCAL(IXQ(K,IV))
                     ENDDO
                     DO K = 6,NIXQ
                       IXQF(K,IFQ) = IXQ(K,IV)
                     ENDDO
                   ENDIF
C   ivois
                   ee_connect_l%connected(IAD2) = TAGE(IV)
                   ID_GLOBAL_VOIS( (IE_LOC-1)*4+J ) = IXQ(NIXQ,IV)
                   DO K=1,INDX_Q(IE)
                        IF( IXQ(NIXQ,IV)==FACE_ELM_Q(4*(IE-1)+K,2) ) THEN
                            FACE_VOIS( (IE_LOC-1)*4+J ) = FACE_ELM_Q(4*(IE-1)+K,1) ! pas sur le proc courant
                        ENDIF                       
                   ENDDO
                 ELSE
C   element interne
                   ee_connect_l%connected(IAD2) = IV_LOC 
                   ID_GLOBAL_VOIS( (IE_LOC-1)*4+J ) = IXQ(NIXQ,IV)
                 ENDIF
               ELSE
                 ee_connect_l%connected(IAD2) = IV
                 ID_GLOBAL_VOIS( (IE_LOC-1)*4+J ) = 0
               ENDIF
               ee_connect_l%type(IAD2) = ALE_CONNECTIVITY%ee_connect%type(IAD1 + J - 1)
               ee_connect_l%iface2(IAD2) = ALE_CONNECTIVITY%ee_connect%iface2(IAD1 + J - 1)
             ENDDO
           ENDDO
           NFT_LOC = NFT_LOC + NEL
         ELSEIF(ITY==7 .AND. (N2D /= 0 .AND. MULTI_FVM%IS_USED)) THEN
C   2D
           DO I = 1, NEL
             IE = I+NFT
             IE_lOC = I+NFT_LOC
             IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             TAGE(IE) = CEL(IE)
C   IVOIS
             DO J = 1, 3
               IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
               IAD2 = ee_connect_l%iad_connect(IE_LOC) + J - 1
               IF (IV>0) THEN
                 PROC2  = CEP(IV)
                 IV_LOC = CEL(IV)
                 IF(PROC2/=PROC) THEN
C   element frontiere
                   IF(TAGE(IV)==0) THEN
                     IFTG = IFTG + 1
                     TAGE(IV) = NUMELTG_L+IFTG
C   recopie IXQ + chgt no local des noeuds
                     DO K = 1,1
                       IXTGF(K,IFTG) = IXTG(K,IV)
                     ENDDO
                     DO K = 2,4
                       IXTGF(K,IFTG) = NODLOCAL(IXTG(K,IV))
                     ENDDO
                     DO K = 5,NIXTG
                       IXTGF(K,IFTG) = IXTG(K,IV)
                     ENDDO
                   ENDIF
C   ivois
                   ee_connect_l%connected(IAD2) = TAGE(IV)
                   ID_GLOBAL_VOIS( (IE_LOC-1)*3+J ) = IXTG(NIXTG,IV)

                   DO K=1,INDX_TG(IE)
                        IF( IXTG(NIXTG,IV)==FACE_ELM_TG(3*(IE-1)+K,2) ) THEN
                            FACE_VOIS( (IE_LOC-1)*3+J ) = FACE_ELM_TG(3*(IE-1)+K,1) ! pas sur le proc courant
                        ENDIF                      
                   ENDDO

                 ELSE
C   element interne
                   ee_connect_l%connected(IAD2) = IV_LOC
                   ID_GLOBAL_VOIS( (IE_LOC-1)*3+J ) = IXTG(NIXTG,IV)
                 ENDIF
               ELSE
                 ee_connect_l%connected(IAD2) = IV
                 ID_GLOBAL_VOIS( (IE_LOC-1)*3+J ) = 0
               ENDIF
               ee_connect_l%type(IAD2) = ALE_CONNECTIVITY%ee_connect%type(IAD1 + J - 1)
               ee_connect_l%iface2(IAD2) = ALE_CONNECTIVITY%ee_connect%iface2(IAD1 + J - 1)
             ENDDO
           ENDDO
           NFT_LOC = NFT_LOC + NEL
         ENDIF
       ENDIF
      ENDDO
      DEALLOCATE(TAGE)
      DEALLOCATE(nb_connect_l)
      RETURN
      END
