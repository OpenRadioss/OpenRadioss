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
Chd|  PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|-- calls ---------------
Chd|        COMPUT_COINKNOT               source/elements/ige3d/comput_coinknot.F
Chd|        COMPUT_MESH_NEIGHBOUR         source/elements/ige3d/comput_mesh_neighbour.F
Chd|        FIND_NEWKNOT                  source/elements/ige3d/find_newknot.F
Chd|        RAFIG3D                       source/elements/ige3d/rafig3d.F
Chd|        REBUILD_IG3D                  source/elements/ige3d/rebuild_ig3d.F
Chd|        REORDER_IG3D                  source/elements/ige3d/reorder_ig3d.F
Chd|        TEST_SUPPORT_FCT              source/elements/ige3d/test_support_fct.F
Chd|        TEST_SUPPORT_NEWFCT           source/elements/ige3d/test_support_newfct.F
Chd|        MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|====================================================================
      SUBROUTINE PRERAFIG3D(KNOT,KNOTLOCPC,KNOTLOCEL,KXIG3D,IXIG3D,
     .                      IGEO,IPARTIG3D,X        ,V     ,D     ,
     .                      MS  ,WIGE     ,TABCONPATCH,FLAG_PRE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESHSURFIG3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXIG3D(*),KXIG3D(NIXIG3D,*),IGEO(NPROPGI,*),
     .        IPARTIG3D(*)
      INTEGER FLAG_PRE
      TYPE(TABCONPATCH_IG3D_), DIMENSION(*) :: TABCONPATCH
      my_real KNOT(*),KNOTLOCPC(DEG_MAX,3,*),KNOTLOCEL(2,3,*)
      my_real X(*),V(*),D(*),MS(*),WIGE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(MESHSURFIG3D_), DIMENSION(:), ALLOCATABLE, TARGET :: MESHSURF
      TYPE(MESHSURFIG3D_), POINTER :: PMESHSURF, P2MESHSURF
      INTEGER I,J,K,L,M,N,P,ITNCTRL,INCTRL,ITKSI,ITETA,ITZETA,
     .        IPID,IAD_KNOT,ITTEST,ITEL,OFFSET_KNOT,
     .        PX,PY,PZ,IDX,IDY,IDZ,IEL,
     .        N1,N2,N3,NKNOT1,NKNOT2,NKNOT3,NELX,NELY,NELZ,
     .        DIR,DECALGEO_TMP,DECALGEOFINAL,NBCUT,IDNBCUT,
     .        IDKNOT1,IDKNOT2,PDIR,PTANG1,PTANG2,
     .        NELDIR,NELTANG1,NELTANG2,
     .        P2DIR,P2TANG1,P2TANG2,L_TAB_NEWFCTCUT,ITPATCH,
     .        NBPATCH_IG3D,FLAG_DEBUG
      my_real, DIMENSION(:), ALLOCATABLE :: GAMA
C-----------------------------------------------
C   TABLEAUX DE REPRESENTATION DE LA STRUCTURE ELEMENTAIRE DES PATCHS
C-----------------------------------------------
      INTEGER, DIMENSION(:,:,:), POINTER :: MESHIGE
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: MESHIGEX,MESHIGEY,MESHIGEZ
      INTEGER, DIMENSION(:), ALLOCATABLE, TARGET :: IDXEL,IDYEL,IDZEL
      INTEGER, DIMENSION(:), POINTER :: IDDIR,IDTANG1,IDTANG2
C-----------------------------------------------
C   TABLEAUX DE TRAVAIL
C-----------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IDFILS,TAB_INITIAL_CUT
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAB_REMOVE,TAB_STAY,TAB_NEWFCT,
     .                                      TAB_NEWFCTCUT,EL_CONNECT
C-----------------------------------------------
C   ARGUMENTS UTILISES NON PAS DANS CONTRL.F, MAIS DANS LECTUR.F
C-----------------------------------------------
      my_real, DIMENSION(:), ALLOCATABLE :: X_TMP,V_TMP,D_TMP,MS_TMP,WIGE_TMP
C=======================================================================
      FLAG_DEBUG=0
c ------------------------------------------------------------------------------------------
C  DEFINITION DES TAILLES DES TABLEAUX DE TRAVAIL EN FONCTION DU NOMBRE D'ELEMENTS
C  AJOUTES, DU NOMBRE ESTIME DE FONCTION AJOUTES DANS ONE PREMIER TEMPS
C  PUIS AUX TAILLES EXACTES NECESSAIRES DANS ONE SECOND TEMPS
c ------------------------------------------------------------------------------------------
c
      IF(FLAG_PRE==0) THEN
        NBNEWX_TMP = ADDELIG3D*20 ! nombre de points qu'on s'accord en decalage pour le travail (pour le multipatch) ! le definir en fonction du nombre total de point
c
        ALLOCATE (TAB_INITIAL_CUT(3,NUMELIG3D0))
        ALLOCATE (IDFILS(NBFILSMAX,NUMELIG3D0))
        ALLOCATE (TAB_REMOVE(ADDELIG3D*27)) ! DEVRAIT TENIR EN COMPTE DU NOMBRE D'ELEMENT DE BASE ET DU NOMBRE D'ELEMENT AJOUTES
        ALLOCATE (TAB_STAY(NINT(ADDELIG3D*27*0.5))) ! DEVRAIT TENIR EN COMPTE DU NOMBRE D'ELEMENT DE BASE ET DU NOMBRE D'ELEMENT AJOUTES
        ALLOCATE (TAB_NEWFCT(NINT(ADDELIG3D*27*1.2))) ! POURRAIENT   TRE MIEUX DIMENSIONNES
        ALLOCATE (MESHSURF(NBMESHSURF))
        ALLOCATE (GAMA(NUMNODIGE0+2*ADDELIG3D*27))
      ELSE
        NBNEWX_TMP = L_TAB_NEWFCT ! pour la longueur de travail et le calcul de decalgeo_tmp
        ALLOCATE (TAB_INITIAL_CUT(3,NUMELIG3D))
        ALLOCATE (IDFILS(NBFILSMAX,NUMELIG3D))
        ALLOCATE (TAB_REMOVE(L_TAB_REMOVE))
        ALLOCATE (TAB_STAY(L_TAB_STAY))
        ALLOCATE (TAB_NEWFCT(L_TAB_NEWFCT))
        ALLOCATE (MESHSURF(NBMESHSURF))
        ALLOCATE (X_TMP(3*(NUMNODIGE0+L_TAB_NEWFCT)))
        ALLOCATE (D_TMP(3*(NUMNODIGE0+L_TAB_NEWFCT)))
        ALLOCATE (V_TMP(3*(NUMNODIGE0+L_TAB_NEWFCT)))
        ALLOCATE (MS_TMP(NUMNODIGE0+L_TAB_NEWFCT))
        ALLOCATE (WIGE_TMP(NUMNODIGE0+L_TAB_NEWFCT))
        ALLOCATE (GAMA(NUMNODIGE0+L_TAB_NEWFCT))
        DO I=1,NUMNODIGE0
          DO J=1,3
            X_TMP((I-1)*3+J) = X((I-1)*3+J)
            V_TMP((I-1)*3+J) = V((I-1)*3+J)
            D_TMP((I-1)*3+J) = D((I-1)*3+J)
          ENDDO
          MS_TMP(I) = MS(I)
          WIGE_TMP(I) = WIGE(I)
        ENDDO
        DO I=NUMNODIGE0+1,NUMNODIGE0+L_TAB_NEWFCT
          DO J=1,3
            X_TMP((I-1)*3+J) = 0
            V_TMP((I-1)*3+J) = 0
            D_TMP((I-1)*3+J) = 0
          ENDDO
          MS_TMP(I) = 0
          WIGE_TMP(I) = 0
        ENDDO
      ENDIF
      ALLOCATE (EL_CONNECT(NUMELIG3D0+ADDELIG3D)) ! TABLEAU QUI CONTIENT ONE FLAG POUR LES ELEMENTS QUI ONT ETE MODIFIES
      EL_CONNECT(:)=0

ccc ATTENTION, S'IL Y A DES ELEMENTS CLASSIQUES ET DES ELEMENT ISOGEOMETRIQUES, IL VA Y AVOIR ONE SOUCIS
CC  IL FAUT DONC FAIRE ONE EL_CONNECT DANS LA STRUCTURE TABCONPATCH ET REFAIRE LES TABLES DE CONNECTIVITE PAR PATCH
c
c ------------------------------------------------------------------------------------------
C  INITIALISATION DES TAILLES REELLES DES TABLEAUX ET DES VARIABLES DE TRAVAIL
c ------------------------------------------------------------------------------------------
C
      NBMESHSURF = 0
      ADDELIG3D=0
      ADDSIXIG3D=0
      L_TAB_REMOVE=0
      L_TAB_STAY=0
      L_TAB_NEWFCT=0
      TAB_NEWFCT = 0
      TAB_REMOVE = 0
      TAB_STAY = 0
      IDFILS(:,:)=0
      OFFSET_NEWFCT = 0
c
c ------------------------------------------------------------------------------------------
C CONSTRUCTION DES ETENDUES KNOT LOCALES POUR CHAQUE POINT DE CONTROLE, POUR TOUT LES PATCHS
c DES CONNECTIVITES DES ELEMENTS, ET DES PATCHS
C NB : OPTIMISER CES STRUCTURES PAR PATCH POUR KNOTLOCP SANS CASES VIDES
c ------------------------------------------------------------------------------------------
c
c! vectorize possible with some modifications
c
      DO P=1,NBPART_IG3D
        IPID=TABCONPATCH(P)%PID
        IAD_KNOT = IGEO(40,IPID)
        PX = IGEO(41,IPID)
        PY = IGEO(42,IPID)
        PZ = IGEO(43,IPID)
        N1 = IGEO(44,IPID)
        N2 = IGEO(45,IPID)
        N3 = IGEO(46,IPID)
        NKNOT1 = N1+PX
        NKNOT2 = N2+PY
        NKNOT3 = N3+PZ
        DECALGEO_TMP=(IPID-1)*(NUMNOD+NBNEWX_TMP)
        DO I=1,TABCONPATCH(P)%L_TAB_IG3D
          IEL=TABCONPATCH(P)%TAB_IG3D(I)
          ITNCTRL=0
          IDX = KXIG3D(6,IEL)
          IDY = KXIG3D(7,IEL)
          IDZ = KXIG3D(8,IEL)
          DO ITZETA=1,PZ
            DO ITETA=1,PY
              DO ITKSI=1,PX
                ITNCTRL=ITNCTRL+1
                INCTRL = IXIG3D(KXIG3D(4,IEL)+ITNCTRL-1)
                DO L=0,PX
                  KNOTLOCPC(L+1,1,DECALGEO_TMP+INCTRL)=KNOT(IAD_KNOT+IDX-ITKSI+L+1)
                ENDDO
                DO M=0,PY
                  KNOTLOCPC(M+1,2,DECALGEO_TMP+INCTRL)=KNOT(IAD_KNOT+NKNOT1+IDY-ITETA+M+1)
                ENDDO
                DO N=0,PZ
                  KNOTLOCPC(N+1,3,DECALGEO_TMP+INCTRL)=KNOT(IAD_KNOT+NKNOT1+NKNOT2+IDZ-ITZETA+N+1)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
c
          KXIG3D(9,IEL)=IDX+1
          KXIG3D(10,IEL)=IDY+1
          KXIG3D(11,IEL)=IDZ+1
          DO WHILE (KNOT(IAD_KNOT+KXIG3D(9,IEL))==KNOT(IAD_KNOT+KXIG3D(9,IEL)+1))
            KXIG3D(9,IEL)=KXIG3D(9,IEL)+1
          ENDDO
          DO WHILE (KNOT(IAD_KNOT+NKNOT1+KXIG3D(10,IEL))==KNOT(IAD_KNOT+NKNOT1+KXIG3D(10,IEL)+1))
            KXIG3D(10,IEL)=KXIG3D(10,IEL)+1
          ENDDO
          DO WHILE (KNOT(IAD_KNOT+NKNOT1+NKNOT2+KXIG3D(11,IEL))==KNOT(IAD_KNOT+NKNOT1+NKNOT2+KXIG3D(11,IEL)+1))
            KXIG3D(11,IEL)=KXIG3D(11,IEL)+1
          ENDDO
c
          KNOTLOCEL(1,1,IEL) = KNOT(IAD_KNOT+KXIG3D(6,IEL))
          KNOTLOCEL(2,1,IEL) = KNOT(IAD_KNOT+KXIG3D(9,IEL))
          KNOTLOCEL(1,2,IEL) = KNOT(IAD_KNOT+NKNOT1+KXIG3D(7,IEL))
          KNOTLOCEL(2,2,IEL) = KNOT(IAD_KNOT+NKNOT1+KXIG3D(10,IEL))
          KNOTLOCEL(1,3,IEL) = KNOT(IAD_KNOT+NKNOT1+NKNOT2+KXIG3D(8,IEL))
          KNOTLOCEL(2,3,IEL) = KNOT(IAD_KNOT+NKNOT1+NKNOT2+KXIG3D(11,IEL))
c
c ------------------------------------------------------------------------------------------
C  MISE EN MEMOIRE DU NOMBRE DE RAFFINEMENT INITIAL, PERMETTANT LE RAFFINEMENT PROGRESSIF
C  NB : TABLEAU GLOBAL ICI, MAIS SERAIT MIEUX PAR PATCH (ATTENTION AU ID DES ELEMENTS)
c ------------------------------------------------------------------------------------------
c
          TAB_INITIAL_CUT(1,IEL) = KXIG3D(12,IEL)
          TAB_INITIAL_CUT(2,IEL) = KXIG3D(13,IEL)
          TAB_INITIAL_CUT(3,IEL) = KXIG3D(14,IEL)
c
c          TABCONPATCH(P)%INITIAL_CUT(1,I) = KXIG3D(12,IEL)
c          TABCONPATCH(P)%INITIAL_CUT(2,I) = KXIG3D(13,IEL)
c          TABCONPATCH(P)%INITIAL_CUT(3,I) = KXIG3D(14,IEL)
c
        ENDDO
      ENDDO
c
c ------------------------------------------------------------------------------------------
C  PROCEDURE DE RAFFINEMENT DES ELEMENTS, QUI FONCTIONNE PAR PATCH
c ------------------------------------------------------------------------------------------
c
c! vectorize possible with some modifications
c
      DO P=1,NBPART_IG3D
        IPID=TABCONPATCH(P)%PID
        IAD_KNOT = IGEO(40,IPID)
        PX = IGEO(41,IPID)
        PY = IGEO(42,IPID)
        PZ = IGEO(43,IPID)
        N1 = IGEO(44,IPID)
        N2 = IGEO(45,IPID)
        N3 = IGEO(46,IPID)
        NKNOT1 = N1+PX
        NKNOT2 = N2+PY
        NKNOT3 = N3+PZ
        NELX=0
        NELY=0
        NELZ=0
        ALLOCATE(IDXEL(NKNOT1))
        ALLOCATE(IDYEL(NKNOT2))
        ALLOCATE(IDZEL(NKNOT3))
        IDXEL=0
        IDYEL=0
        IDZEL=0
        DO I=1,NKNOT1-1
          IF(KNOT(IAD_KNOT+I)/=KNOT(IAD_KNOT+I+1)) THEN
            NELX=NELX+1
            IDXEL(I)=NELX
          ENDIF
        ENDDO
        DO I=1,NKNOT2-1
          IF(KNOT(IAD_KNOT+NKNOT1+I)/=KNOT(IAD_KNOT+NKNOT1+I+1)) THEN
            NELY=NELY+1
            IDYEL(I)=NELY
          ENDIF
        ENDDO
        DO I=1,NKNOT3-1
          IF(KNOT(IAD_KNOT+NKNOT1+NKNOT2+I)/=KNOT(IAD_KNOT+NKNOT1+NKNOT2+I+1)) THEN
            NELZ=NELZ+1
            IDZEL(I)=NELZ
          ENDIF
        ENDDO
c
c ------------------------------------------------------------------------------------------
C  CONSTRUCTION DES TABLEAUX PERMETTANT LA LOCALISATION LE PATCH
c ------------------------------------------------------------------------------------------
c
        ALLOCATE(MESHIGEX(NELY,NELZ,NELX))
        ALLOCATE(MESHIGEY(NELZ,NELX,NELY))
        ALLOCATE(MESHIGEZ(NELX,NELY,NELZ))
        MESHIGEX(:,:,:)=0
        MESHIGEY(:,:,:)=0
        MESHIGEZ(:,:,:)=0
c
        DO L=1,TABCONPATCH(P)%L_TAB_IG3D ! nb d'elements du patch P
          IEL = TABCONPATCH(P)%TAB_IG3D(L)
          MESHIGEX(IDYEL(KXIG3D(7,IEL)),IDZEL(KXIG3D(8,IEL)),IDXEL(KXIG3D(6,IEL)))=IEL
          MESHIGEY(IDZEL(KXIG3D(8,IEL)),IDXEL(KXIG3D(6,IEL)),IDYEL(KXIG3D(7,IEL)))=IEL
          MESHIGEZ(IDXEL(KXIG3D(6,IEL)),IDYEL(KXIG3D(7,IEL)),IDZEL(KXIG3D(8,IEL)))=IEL
        ENDDO
c
c ------------------------------------------------------------------------------------------
c   DEFINITION DES VARIABLES EN FONCTION DE LA DIRECTION DE COUPE
c ------------------------------------------------------------------------------------------
c
        GAMA=1
        DO DIR=1,3
          SELECT CASE (DIR)
           CASE(1)
            IDNBCUT=12 ! where the nb of cut wished is located
            IDKNOT1=6  ! index of 1st knot in the Xknot vector
            IDKNOT2=9  ! index of 2nd knot in the Xknot vector
            OFFSET_KNOT=IAD_KNOT
            PDIR=PX
            PTANG1=PY
            PTANG2=PZ
            NELDIR=NELX
            NELTANG1=NELY
            NELTANG2=NELZ
            IDDIR => IDXEL
            IDTANG1 => IDYEL
            IDTANG2 => IDZEL
            MESHIGE => MESHIGEX
           CASE(2)
            IDNBCUT=13 ! where the nb of cut wished is located
            IDKNOT1=7  ! index of 1st knot in the Yknot vector
            IDKNOT2=10 ! index of 2nd knot in the Yknot vector
            OFFSET_KNOT=IAD_KNOT+NKNOT1
            PDIR=PY
            PTANG1=PZ
            PTANG2=PX
            NELDIR=NELY
            NELTANG1=NELZ
            NELTANG2=NELX
            IDDIR => IDYEL
            IDTANG1 => IDZEL
            IDTANG2 => IDXEL
            MESHIGE => MESHIGEY
           CASE(3)
            IDNBCUT=14 ! where the nb of cut wished is located
            IDKNOT1=8  ! index of 1st knot in the Zknot vector
            IDKNOT2=11 ! index of 2nd knot in the Zknot vector
            OFFSET_KNOT=IAD_KNOT+NKNOT1+NKNOT2
            PDIR=PZ
            PTANG1=PX
            PTANG2=PY
            NELDIR=NELZ
            NELTANG1=NELX
            NELTANG2=NELY
            IDDIR => IDZEL
            IDTANG1 => IDXEL
            IDTANG2 => IDYEL
            MESHIGE => MESHIGEZ
          END SELECT
c
c ------------------------------------------------------------------------------------------
c   DECOUPAGE DES ELEMENTS DU PATCH
c ------------------------------------------------------------------------------------------
c
          DO L=1,TABCONPATCH(P)%L_TAB_IG3D
            IEL = TABCONPATCH(P)%TAB_IG3D(L)
            DECALGEO_TMP=(KXIG3D(2,IEL)-1)*(NUMNOD+NBNEWX_TMP)
            IF(KXIG3D(IDNBCUT,IEL)>1) THEN
              NBCUT=TAB_INITIAL_CUT(DIR,IEL)
c            NBCUT=TABCONPATCH(P)%INITIAL_CUT(DIR,IEL)
              DO I=(TAB_INITIAL_CUT(DIR,IEL)-KXIG3D(IDNBCUT,IEL))+1,TAB_INITIAL_CUT(DIR,IEL)-1
c            DO I=(TABCONPATCH(P)%INITIAL_CUT(DIR,IEL)-KXIG3D(IDNBCUT,IEL))+1,TABCONPATCH(P)%INITIAL_CUT(DIR,IEL)-1
                NBMESHSURF = NBMESHSURF + 1
                NEWFCT = 0
                PMESHSURF => MESHSURF(NBMESHSURF)
                PMESHSURF%DIR=DIR
                PMESHSURF%ID_MESHSURF=NBMESHSURF
                PMESHSURF%ID_PID=IPID
C
                CALL FIND_NEWKNOT(IEL   ,KXIG3D,KNOT  ,DIR  ,IAD_KNOT,
     .                            NKNOT1,NKNOT2,NKNOT3,I,PMESHSURF%KNOT_INSERE)
C
                CALL COMPUT_COINKNOT(IEL, IXIG3D ,KXIG3D   ,MESHIGE  ,PTANG1     ,PTANG2      ,
     .                          IDDIR   ,IDTANG1  ,IDTANG2   ,
     .                          NELDIR  ,NELTANG1 ,NELTANG2  ,PMESHSURF%DIR    ,
     .                          PMESHSURF%TAB_COINKNOT,PMESHSURF%L_TAB_COINKNOT,
     .                          PMESHSURF%TAB_ELCUT,PMESHSURF%L_TAB_ELCUT,
     .                          PMESHSURF%TAB_NEWEL,PMESHSURF%L_TAB_NEWEL,
     .                          KNOT,IAD_KNOT,NKNOT1,NKNOT2,NKNOT3,IDFILS,
     .                          KNOTLOCEL,PMESHSURF%KNOT_INSERE,IPARTIG3D,TAB_INITIAL_CUT,I,0)
C
                ALLOCATE(PMESHSURF%TAB_COINKNOT(2,PMESHSURF%L_TAB_COINKNOT))
                ALLOCATE(PMESHSURF%TAB_ELCUT(PMESHSURF%L_TAB_ELCUT))
                ALLOCATE(PMESHSURF%TAB_NEWEL(PMESHSURF%L_TAB_NEWEL))
C
                CALL COMPUT_COINKNOT(IEL, IXIG3D ,KXIG3D   ,MESHIGE  ,PTANG1     ,PTANG2      ,
     .                          IDDIR   ,IDTANG1  ,IDTANG2   ,
     .                          NELDIR  ,NELTANG1 ,NELTANG2  ,PMESHSURF%DIR    ,
     .                          PMESHSURF%TAB_COINKNOT,PMESHSURF%L_TAB_COINKNOT,
     .                          PMESHSURF%TAB_ELCUT,PMESHSURF%L_TAB_ELCUT,
     .                          PMESHSURF%TAB_NEWEL,PMESHSURF%L_TAB_NEWEL,
     .                          KNOT,IAD_KNOT,NKNOT1,NKNOT2,NKNOT3,IDFILS,
     .                          KNOTLOCEL,PMESHSURF%KNOT_INSERE,IPARTIG3D,TAB_INITIAL_CUT,I,1)
C
                PMESHSURF%L_TAB_MESHSURFCUT = 0
C
                DO ITTEST=1,NBMESHSURF-1
                  P2MESHSURF => MESHSURF(ITTEST)
                  CALL COMPUT_MESH_NEIGHBOUR(PMESHSURF%DIR, P2MESHSURF%DIR,
     .                            PMESHSURF%ID_PID, P2MESHSURF%ID_PID, P2MESHSURF%ID_MESHSURF,
     .                            PMESHSURF%TAB_COINKNOT,PMESHSURF%L_TAB_COINKNOT,
     .                            P2MESHSURF%TAB_COINKNOT,P2MESHSURF%L_TAB_COINKNOT,
     .                            PMESHSURF%KNOT_INSERE,P2MESHSURF%KNOT_INSERE,
     .                            PMESHSURF%TAB_MESHSURFCUT,PMESHSURF%L_TAB_MESHSURFCUT,
     .                            P2MESHSURF%TAB_MESHSURFCUT,P2MESHSURF%L_TAB_MESHSURFCUT,0)
                ENDDO
C
                ALLOCATE(PMESHSURF%TAB_MESHSURFCUT(PMESHSURF%L_TAB_MESHSURFCUT))
                PMESHSURF%L_TAB_MESHSURFCUT = 0
C
                DO ITTEST=1,NBMESHSURF-1
                  P2MESHSURF => MESHSURF(ITTEST)
                  CALL COMPUT_MESH_NEIGHBOUR(PMESHSURF%DIR, P2MESHSURF%DIR,
     .                            PMESHSURF%ID_PID, P2MESHSURF%ID_PID, P2MESHSURF%ID_MESHSURF,
     .                            PMESHSURF%TAB_COINKNOT,PMESHSURF%L_TAB_COINKNOT,
     .                            P2MESHSURF%TAB_COINKNOT,P2MESHSURF%L_TAB_COINKNOT,
     .                            PMESHSURF%KNOT_INSERE,P2MESHSURF%KNOT_INSERE,
     .                            PMESHSURF%TAB_MESHSURFCUT,PMESHSURF%L_TAB_MESHSURFCUT,
     .                            P2MESHSURF%TAB_MESHSURFCUT,P2MESHSURF%L_TAB_MESHSURFCUT,1)
                ENDDO
C
                CALL TEST_SUPPORT_FCT(IXIG3D, KXIG3D, KNOTLOCPC, PTANG1, PTANG2, PMESHSURF%DIR,
     .                                PMESHSURF%TAB_ELCUT,PMESHSURF%L_TAB_ELCUT,
     .                                PMESHSURF%TAB_COINKNOT,PMESHSURF%L_TAB_COINKNOT,
     .                                PMESHSURF%TAB_FCTCUT,PMESHSURF%L_TAB_FCTCUT,DECALGEO_TMP,0)
C
                ALLOCATE(PMESHSURF%TAB_FCTCUT(PMESHSURF%L_TAB_FCTCUT))
C
                CALL TEST_SUPPORT_FCT(IXIG3D, KXIG3D, KNOTLOCPC, PTANG1, PTANG2, PMESHSURF%DIR,
     .                                PMESHSURF%TAB_ELCUT,PMESHSURF%L_TAB_ELCUT,
     .                                PMESHSURF%TAB_COINKNOT,PMESHSURF%L_TAB_COINKNOT,
     .                                PMESHSURF%TAB_FCTCUT,PMESHSURF%L_TAB_FCTCUT,DECALGEO_TMP,1)
C
                CALL RAFIG3D(KNOTLOCPC,
     .                       PDIR,PTANG1,PTANG2,IAD_KNOT,NKNOT1,NKNOT2,NKNOT3,
     .                       GAMA,PMESHSURF%DIR,PMESHSURF%KNOT_INSERE,
     .                       X_TMP,D_TMP,V_TMP,MS_TMP,WIGE_TMP,
     .                       PMESHSURF%TAB_FCTCUT,PMESHSURF%L_TAB_FCTCUT,
     .                       TAB_REMOVE,TAB_NEWFCT,DECALGEO_TMP,TABCONPATCH,P,
     .                       KXIG3D,IXIG3D,TAB_STAY,FLAG_PRE)
C
                DO ITTEST=1,PMESHSURF%L_TAB_MESHSURFCUT
                  P2MESHSURF => MESHSURF(PMESHSURF%TAB_MESHSURFCUT(ITTEST))
                  IF(P2MESHSURF%DIR==2) THEN
                    P2DIR=PY
                    P2TANG1=PZ
                    P2TANG2=PX
                  ELSEIF(P2MESHSURF%DIR==1) THEN
                    P2DIR=PX
                    P2TANG1=PY
                    P2TANG2=PZ
                  ENDIF
                  L_TAB_NEWFCTCUT = 0
                  CALL TEST_SUPPORT_NEWFCT(KNOTLOCPC, P2DIR, P2TANG1, P2TANG2,
     .                                P2MESHSURF%DIR, P2MESHSURF%KNOT_INSERE,
     .                                P2MESHSURF%TAB_COINKNOT,P2MESHSURF%L_TAB_COINKNOT, TAB_NEWFCT,
     .                                TAB_NEWFCTCUT,L_TAB_NEWFCTCUT,DECALGEO_TMP,TAB_REMOVE,0)
                  IF(L_TAB_NEWFCTCUT>0) THEN
                    ALLOCATE(TAB_NEWFCTCUT(L_TAB_NEWFCTCUT))
                    CALL TEST_SUPPORT_NEWFCT(KNOTLOCPC, P2DIR, P2TANG1, P2TANG2,
     .                                  P2MESHSURF%DIR, P2MESHSURF%KNOT_INSERE,
     .                                  P2MESHSURF%TAB_COINKNOT,P2MESHSURF%L_TAB_COINKNOT, TAB_NEWFCT,
     .                                  TAB_NEWFCTCUT,L_TAB_NEWFCTCUT,DECALGEO_TMP,TAB_REMOVE,1)
                    CALL RAFIG3D(KNOTLOCPC,
     .                           P2DIR,P2TANG1,P2TANG2,IAD_KNOT,NKNOT1,NKNOT2,NKNOT3,
     .                           GAMA,P2MESHSURF%DIR,P2MESHSURF%KNOT_INSERE,
     .                           X_TMP,D_TMP,V_TMP,MS_TMP,WIGE_TMP,
     .                           TAB_NEWFCTCUT,L_TAB_NEWFCTCUT,
     .                           TAB_REMOVE,TAB_NEWFCT,DECALGEO_TMP,TABCONPATCH,P,
     .                           KXIG3D,IXIG3D,TAB_STAY,FLAG_PRE)

                    DEALLOCATE(TAB_NEWFCTCUT)

                  ENDIF
                ENDDO
C
                CALL REBUILD_IG3D(IXIG3D, KXIG3D,PMESHSURF%DIR,PDIR,PTANG1,PTANG2,
     .                            KNOTLOCPC,KNOTLOCEL,
     .                            PMESHSURF%TAB_ELCUT,PMESHSURF%L_TAB_ELCUT,
     .                            PMESHSURF%TAB_NEWEL,PMESHSURF%L_TAB_NEWEL,
     .                            PMESHSURF%TAB_FCTCUT,PMESHSURF%L_TAB_FCTCUT,
     .                            TAB_REMOVE,TAB_NEWFCT,EL_CONNECT,TABCONPATCH(P),
     .                            IDFILS,FLAG_PRE,FLAG_DEBUG)

                OFFSET_NEWFCT = OFFSET_NEWFCT + NEWFCT  ! "SNEW REDEVIENT S" les nouvelles fonctions ne le sont plus

              ENDDO ! boucle sur le nombre de coupe de l'element
            ENDIF
          ENDDO ! boucle sur les elements du patch
        ENDDO ! boucle sur les direction
        DEALLOCATE(IDXEL,IDYEL,IDZEL)
        DEALLOCATE(MESHIGEX,MESHIGEY,MESHIGEZ)
      ENDDO ! boucle sur les patchs
c
CC----------------------------------------------------------------------------------------------
CC    DEALLOCATION DES STRUCTURES DEFINISSANT LES COUPES
CC----------------------------------------------------------------------------------------------
c
      DO I=1,NBMESHSURF
        IF(MESHSURF(I)%L_TAB_COINKNOT/=0) DEALLOCATE(MESHSURF(I)%TAB_COINKNOT)
        IF(MESHSURF(I)%L_TAB_ELCUT/=0) DEALLOCATE(MESHSURF(I)%TAB_ELCUT)
        IF(MESHSURF(I)%L_TAB_FCTCUT/=0) DEALLOCATE(MESHSURF(I)%TAB_FCTCUT)
        IF(MESHSURF(I)%L_TAB_NEWEL/=0) DEALLOCATE(MESHSURF(I)%TAB_NEWEL)
        IF(MESHSURF(I)%L_TAB_MESHSURFCUT/=0) DEALLOCATE(MESHSURF(I)%TAB_MESHSURFCUT)
      ENDDO
      DEALLOCATE(MESHSURF)
c
CC----------------------------------------------------------------------------------------------
CC     TEST A POSTERIORI DES FONCTIONS ET DES ELEMENTS. NE TESTE PAS DIRECTEMENT S'IL EXISTE
CC     DES ELEMENTS AVEC TROP DE FONCTIONS, MAIS PERMET DE VOIR S'IL Y A ONE QUELCONQUE SOUCI
CC----------------------------------------------------------------------------------------------
c
      IF(FLAG_DEBUG==1) THEN
        DO I=1,NUMELIG3D0+ADDELIG3D
          J=1
          DECALGEO_TMP=(KXIG3D(2,I)-1)*(NUMNOD+NBNEWX_TMP)
          DO J=1,KXIG3D(3,I)
            INCTRL=IXIG3D(KXIG3D(4,I)+J-1)
            IF(INCTRL==0) THEN
              print*,'ELEMENT',I,'point',INCTRL
            ELSEIF(KNOTLOCEL(1,1,I)<KNOTLOCPC(1,1,DECALGEO_TMP+INCTRL)-EM06 .OR.
     .         KNOTLOCEL(2,1,I)>KNOTLOCPC(4,1,DECALGEO_TMP+INCTRL)+EM06 .OR.
     .         KNOTLOCEL(1,2,I)<KNOTLOCPC(1,2,DECALGEO_TMP+INCTRL)-EM06 .OR.
     .         KNOTLOCEL(2,2,I)>KNOTLOCPC(4,2,DECALGEO_TMP+INCTRL)+EM06 .OR.
     .         KNOTLOCEL(1,3,I)<KNOTLOCPC(1,3,DECALGEO_TMP+INCTRL)-EM06 .OR.
     .         KNOTLOCEL(2,3,I)>KNOTLOCPC(4,3,DECALGEO_TMP+INCTRL)+EM06) THEN
              print*,'ELEMENT',I,'point',INCTRL
              print*,KNOTLOCEL(1,1,I),'<',KNOTLOCPC(1,1,DECALGEO_TMP+INCTRL)
              print*,KNOTLOCEL(2,1,I),'>',KNOTLOCPC(4,1,DECALGEO_TMP+INCTRL)
              print*,KNOTLOCEL(1,2,I),'<',KNOTLOCPC(1,2,DECALGEO_TMP+INCTRL)
              print*,KNOTLOCEL(2,2,I),'>',KNOTLOCPC(4,2,DECALGEO_TMP+INCTRL)
              print*,KNOTLOCEL(1,3,I),'<',KNOTLOCPC(1,3,DECALGEO_TMP+INCTRL)
              print*,KNOTLOCEL(2,3,I),'>',KNOTLOCPC(4,3,DECALGEO_TMP+INCTRL)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c
      IF(NBMESHSURF/=0) THEN
c
c ------------------------------------------------------------------------------------------
c   REORGANISATION DES TABLEAUX ELEMENTAIRES ET DES CONNECTIVITES AVEC LE RECOMPACTAGE DES POINTS
c ------------------------------------------------------------------------------------------
c
        CALL REORDER_IG3D(IXIG3D, KXIG3D,KNOTLOCPC,KNOTLOCEL,
     .                    X_TMP,D_TMP,V_TMP,MS_TMP,WIGE_TMP,
     .                    TAB_REMOVE,TAB_NEWFCT,EL_CONNECT,
     .                    IPARTIG3D,IGEO,TAB_STAY,FLAG_PRE,FLAG_DEBUG)
c
c ------------------------------------------------------------------------------------------
c   ON RAMENE NOS TABLEAUX TEMPORAIRES DANS LES TABLEAUX FINAUX
c ------------------------------------------------------------------------------------------
c
        IF(FLAG_PRE==1) THEN
          DO I=1,NUMNOD
            DO J=1,3
              X((I-1)*3+J) = X_TMP((I-1)*3+J)
              V((I-1)*3+J) = V_TMP((I-1)*3+J)
              D((I-1)*3+J) = D_TMP((I-1)*3+J)
            ENDDO
            MS(I) = MS_TMP(I)
            WIGE(I) = WIGE_TMP(I)
          ENDDO
          DO P=1,NBPART_IG3D
            IPID=TABCONPATCH(P)%PID
            DECALGEOFINAL=(IPID-1)*NUMNOD
            DECALGEO_TMP=(IPID-1)*(NUMNOD+NBNEWX_TMP)
            DO I=1,NUMNOD
              KNOTLOCPC(:,1,DECALGEOFINAL+I) = KNOTLOCPC(:,1,DECALGEO_TMP+I)
              KNOTLOCPC(:,2,DECALGEOFINAL+I) = KNOTLOCPC(:,2,DECALGEO_TMP+I)
              KNOTLOCPC(:,3,DECALGEOFINAL+I) = KNOTLOCPC(:,3,DECALGEO_TMP+I)
            ENDDO
          ENDDO
          DEALLOCATE(X_TMP,V_TMP,D_TMP,MS_TMP,WIGE_TMP)

          IF(FLAG_DEBUG==1) THEN
            DO I=1,NUMELIG3D0+ADDELIG3D
              J=1
              DECALGEOFINAL=(KXIG3D(2,I)-1)*(NUMNOD)
              DO J=1,KXIG3D(3,I)
                INCTRL=IXIG3D(KXIG3D(4,I)+J-1)
                IF(INCTRL==0) THEN
                  print*,'ELEMENT',I,'point',INCTRL
                ELSEIF(KNOTLOCEL(1,1,I)<KNOTLOCPC(1,1,DECALGEOFINAL+INCTRL)-EM06 .OR.
     .             KNOTLOCEL(2,1,I)>KNOTLOCPC(4,1,DECALGEOFINAL+INCTRL)+EM06 .OR.
     .             KNOTLOCEL(1,2,I)<KNOTLOCPC(1,2,DECALGEOFINAL+INCTRL)-EM06 .OR.
     .             KNOTLOCEL(2,2,I)>KNOTLOCPC(4,2,DECALGEOFINAL+INCTRL)+EM06 .OR.
     .             KNOTLOCEL(1,3,I)<KNOTLOCPC(1,3,DECALGEOFINAL+INCTRL)-EM06 .OR.
     .             KNOTLOCEL(2,3,I)>KNOTLOCPC(4,3,DECALGEOFINAL+INCTRL)+EM06) THEN
                  print*,'ELEMENT',I,'point',INCTRL
                  print*,KNOTLOCEL(1,1,I),'<',KNOTLOCPC(1,1,DECALGEOFINAL+INCTRL)
                  print*,KNOTLOCEL(2,1,I),'>',KNOTLOCPC(4,1,DECALGEOFINAL+INCTRL)
                  print*,KNOTLOCEL(1,2,I),'<',KNOTLOCPC(1,2,DECALGEOFINAL+INCTRL)
                  print*,KNOTLOCEL(2,2,I),'>',KNOTLOCPC(4,2,DECALGEOFINAL+INCTRL)
                  print*,KNOTLOCEL(1,3,I),'<',KNOTLOCPC(1,3,DECALGEOFINAL+INCTRL)
                  print*,KNOTLOCEL(2,3,I),'>',KNOTLOCPC(4,3,DECALGEOFINAL+INCTRL)
                ENDIF
              ENDDO
            ENDDO
          ENDIF

        ENDIF
      ENDIF

c
c ------------------------------------------------------------------------------------------
c
      DEALLOCATE(TAB_REMOVE)
      DEALLOCATE(TAB_STAY)
      DEALLOCATE(TAB_NEWFCT)
      DEALLOCATE(GAMA)
      DEALLOCATE(IDFILS)
      DEALLOCATE(EL_CONNECT)
      DEALLOCATE(TAB_INITIAL_CUT)

      RETURN
      END

