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
Chd|  I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|-- called by -----------
Chd|        I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7TRIVOX(
     1      NSN    ,RENUM       ,NSNR    ,ISZNSNR  ,I_MEM    ,
     2      IRECT  ,X           ,STF     ,STFN     ,XYZM     ,
     3      NSV    ,II_STOK     ,CAND_N  ,ESHIFT   ,CAND_E   ,
     4      MULNSN ,NOINT       ,TZINF   ,GAP_S_L  ,GAP_M_L  ,
     5      VOXEL  ,NBX         ,NBY     ,NBZ      ,INTTH    ,
     6      INACTI ,IFQ         ,CAND_A  ,CAND_P   ,IFPEN    ,
     7      NRTM   ,NSNROLD     ,IGAP    ,GAP      ,GAP_S    ,
     8      GAP_M  ,GAPMIN      ,GAPMAX  ,MARGE    ,CURV_MAX ,
     9      NIN    ,ITASK       ,BGAPSMX ,KREMNOD  ,REMNOD   ,
     A      ITAB   ,FLAGREMNODE ,DRAD    ,ITIED    ,CAND_F   ,
     B      DGAPLOAD,REMOTE_S_NODE,LIST_REMOTE_S_NODE)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
c     parameter setting the size for the vector (orig version is 128)
      INTEGER NVECSZ
      PARAMETER (NVECSZ = MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES NOEUDS DANS DES BOITES
C   RECHERCHE POUR CHAQUE FACETTE DES BOITES CONCERNES
C   RECHERCHE DES CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     IRECT(4,*)   TABLEAU DES CONEC FACETTES        E
C     X(3,*)       COORDONNEES NODALES               E
C     NSV          NOS SYSTEMES DES NOEUDS           E
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     I_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     CAND_N       boites resultats noeuds
C     CAND_E       adresses des boites resultat elements
C                  MULNSN = MULTIMP*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C
C     PROV_N       CAND_N provisoire (variable static dans i7tri)
C     PROV_E       CAND_E provisoire (variable static dans i7tri)

C     VOXEL(ix,iy,iz) contient le numero local du premier noeud de
C                  la boite
C     NEXT_NOD(i)  noeud suivant dans la meme boite (si /= 0)
C     LAST_NOD(i)  dernier noeud dans la meme boite (si /= 0)
C                  utilise uniquement pour aller directement du premier
C                       noeud au dernier
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NSNROLD,NIN,ITASK,
     .        MULNSN,NOINT,INACTI,IFQ,NSNR,IGAP,NBX,NBY,NBZ,
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        INTTH,IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),II_STOK,
     .        KREMNOD(*),REMNOD(*),ITAB(*),FLAGREMNODE,ITIED
      my_real
     .   X(3,*),XYZM(12),CAND_P(*),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MARGE,GAP,GAPMIN,GAPMAX,BGAPSMX,
     .   CURV_MAX(*),GAP_S_L(*),GAP_M_L(*),CAND_F(*)
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      INTEGER, INTENT(inout) :: REMOTE_S_NODE
      INTEGER, DIMENSION(NSNR), INTENT(inout) :: LIST_REMOTE_S_NODE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,J_STOK,II,JJ,
     .        PROV_N(MVSIZ),PROV_E(MVSIZ),
     .        OLDNUM(ISZNSNR), NSNF, NSNL,DELNOD,M
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGREMNODE
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS
c provisoire
      INTEGER  LAST_NOD(NSN+NSNR)
      INTEGER  IX,IY,IZ,NEXT,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA
      INTEGER CPT_VOX0
      LOGICAL TEST_V0
      INTEGER FIRST,NEW,LAST,IERROR
      LOGICAL DBG_type18_fvm
      SAVE IIX,IIY,IIZ
C-----------------------------------------------
      IF(ITASK == 0)THEN
        ALLOCATE(NEXT_NOD(NSN+NSNR),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .        C1='(/INTER/TYPE7)')
          CALL ARRET(2)
        ENDIF
        ALLOCATE(IIX(NSN+NSNR),IIY(NSN+NSNR),IIZ(NSN+NSNR),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .        C1='(/INTER/TYPE7)')
          CALL ARRET(2)
        ENDIF
      END IF

      CALL MY_BARRIER !Barrier to wait init voxel and allocation NEX_NOD

! Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI

      XMIN = XYZM(1)
      YMIN = XYZM(2)
      ZMIN = XYZM(3)
      XMAX = XYZM(4)
      YMAX = XYZM(5)
      ZMAX = XYZM(6)

      !dev future: xminb plus grand que xmin...
      IF( TO_TRIM(NIN) ) THEN
        XMINB = XYZM(7)
        YMINB = XYZM(8)
        ZMINB = XYZM(9)
        XMAXB = XYZM(10)
        YMAXB = XYZM(11)
        ZMAXB = XYZM(12)
      ELSE
! we use the global bounding box
! the box reduction seemed inefficient
! at the first sorting
! because too many SECONDARY nodes were outside
! the reduced box
        XMINB = XMIN
        YMINB = YMIN
        ZMINB = ZMIN
        XMAXB = XMAX
        YMAXB = YMAX
        ZMAXB = ZMAX
      ENDIF

      ! En SPMD, pour inacti ou IFQ, retrouve ancienne numerotation des candidats non locaux
      IF(NSPMD>1.AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.ITIED/=0)) THEN
        CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD)
      END IF

C=======================================================================
C 1   NODE CLUSTERING
C=======================================================================
      IF(ITASK == 0)THEN
        !==========LAGRANGIAN and STAGGERED ALE==========!
        CPT_VOX0 = 0
        TEST_V0 = FIRST_TEST(NIN)
        DO I=1,NSN
          IIX(I)=0
          IIY(I)=0
          IIZ(I)=0
          IF(STFN(I) == ZERO)CYCLE
          J=NSV(I)
          !C Optimisation // recherche les noeuds compris dans xmin xmax des
          !C elements du processeur
          IF(X(1,J) < XMIN)  CYCLE
          IF(X(1,J) > XMAX)  CYCLE
          IF(X(2,J) < YMIN)  CYCLE
          IF(X(2,J) > YMAX)  CYCLE
          IF(X(3,J) < ZMIN)  CYCLE
          IF(X(3,J) > ZMAX)  CYCLE
          IIX(I)=INT(NBX*(X(1,J)-XMINB)/(XMAXB-XMINB))
          IIY(I)=INT(NBY*(X(2,J)-YMINB)/(YMAXB-YMINB))
          IIZ(I)=INT(NBZ*(X(3,J)-ZMINB)/(ZMAXB-ZMINB))
          IIX(I)=MAX(1,2+MIN(NBX,IIX(I)))
          IIY(I)=MAX(1,2+MIN(NBY,IIY(I)))
          IIZ(I)=MAX(1,2+MIN(NBZ,IIZ(I)))
          FIRST = VOXEL(IIX(I),IIY(I),IIZ(I))
          IF(TEST_V0) THEN
! count the number of SECONDARY nodes outiside the reduced box
            IF(IIX(I) ==  1    .OR.  IIY(I) ==  1    .OR. IIZ(I) == 1 .AND.
     .         IIX(I) ==  NBX+2 .OR. IIY(I) == NBY+2 .OR. IIZ(I) == NBZ+2) THEN
              CPT_VOX0 = CPT_VOX0 +1
            ENDIF
          ENDIF
          IF(FIRST == 0)THEN
            !empty cell
            VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
            NEXT_NOD(I)                 = 0 ! last one
            LAST_NOD(I)                 = 0 ! no last
          ELSEIF(LAST_NOD(FIRST) == 0)THEN
            !cell containing one node
            !add as next node
            NEXT_NOD(FIRST) = I ! next
            LAST_NOD(FIRST) = I ! last
            NEXT_NOD(I)     = 0 ! last one
          ELSE
            !
            !jump to the last node of the cell
            LAST = LAST_NOD(FIRST) ! last node in this voxel
            NEXT_NOD(LAST)  = I ! next
            LAST_NOD(FIRST) = I ! last
            NEXT_NOD(I)     = 0 ! last one
          ENDIF
        ENDDO

C=======================================================================
C 2   REMOTE NODE CLUSTERING
C=======================================================================
        REMOTE_S_NODE = 0
        DO J = 1, NSNR

          IF(XREM(1,J) < XMIN)  CYCLE
          IF(XREM(1,J) > XMAX)  CYCLE
          IF(XREM(2,J) < YMIN)  CYCLE
          IF(XREM(2,J) > YMAX)  CYCLE
          IF(XREM(3,J) < ZMIN)  CYCLE
          IF(XREM(3,J) > ZMAX)  CYCLE

          REMOTE_S_NODE = REMOTE_S_NODE + 1
          LIST_REMOTE_S_NODE( REMOTE_S_NODE ) = J
          IIX(NSN+J)=INT(NBX*(XREM(1,J)-XMINB)/(XMAXB-XMINB))
          IIY(NSN+J)=INT(NBY*(XREM(2,J)-YMINB)/(YMAXB-YMINB))
          IIZ(NSN+J)=INT(NBZ*(XREM(3,J)-ZMINB)/(ZMAXB-ZMINB))
          IIX(NSN+J)=MAX(1,2+MIN(NBX,IIX(NSN+J)))
          IIY(NSN+J)=MAX(1,2+MIN(NBY,IIY(NSN+J)))
          IIZ(NSN+J)=MAX(1,2+MIN(NBZ,IIZ(NSN+J)))

          FIRST = VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J))
          IF(TEST_V0) THEN
! count the number of SECONDARY nodes outiside the reduced box
            IF(IIX(J+NSN) ==  1     .OR. IIY(J+NSN) ==  1     .OR. IIZ(J+NSN) == 1 .AND.
     .         IIX(J+NSN) ==  NBX+2 .OR. IIY(J+NSN) ==  NBY+2 .OR. IIZ(J+NSN) == NBZ+2) THEN
              CPT_VOX0 = CPT_VOX0 +1
            ENDIF
          ENDIF

          IF(FIRST == 0)THEN
            ! voxel vide
            VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J)) = NSN+J ! first
            NEXT_NOD(NSN+J)     = 0 ! last one
            LAST_NOD(NSN+J)     = 0 ! no last
          ELSEIF(LAST_NOD(FIRST) == 0)THEN
            ! cell containing one node add as next node
            NEXT_NOD(FIRST) = NSN+J  ! next
            LAST_NOD(FIRST) = NSN+J  ! last
            NEXT_NOD(NSN+J)  = 0     ! last one
          ELSE
            ! , jump to the last node of the cell
            LAST = LAST_NOD(FIRST)  ! last node in this voxel
            NEXT_NOD(LAST)  = NSN+J ! next
            LAST_NOD(FIRST) = NSN+J ! last
            NEXT_NOD(NSN+J)     = 0 ! last one
          ENDIF
        ENDDO
      END IF !ITASK == 0

      CALL MY_BARRIER ! Barrier to wait task0 treatment

      IF(FIRST_TEST(NIN)) THEN
!During the first sorting, we use the reduced box
!If too many (>5 %) nodes are outside the reduced box
!then the box is not reduced anymore
        CALL MY_BARRIER
        IF(ITASK == 0) THEN
          IF(CPT_VOX0 > 5*(NSNR + NSN)/100) TO_TRIM(NIN) = .FALSE.
          FIRST_TEST(NIN) = .FALSE.
        ENDIF
        CALL MY_BARRIER
      ENDIF


C=======================================================================
C 3   FACE RECOVERY AND ENUMERATION OF CANDIDATE COUPLES
C=======================================================================
      J_STOK = 0
      IF(FLAGREMNODE == 2) THEN
        ALLOCATE(TAGREMNODE(NUMNOD+NUMFAKENODIGEO))
        DO I=1,NUMNOD+NUMFAKENODIGEO
          TAGREMNODE(I) = 0
        ENDDO
      ENDIF
      DO NE=1,NRTM
        IF(STF(NE) == ZERO)CYCLE ! on ne retient pas les facettes detruites
        IF(FLAGREMNODE == 2) THEN
          K = KREMNOD(2*(NE-1)+1)+1
          L = KREMNOD(2*(NE-1)+2)
          DO I=K,L
            TAGREMNODE(REMNOD(I)) = 1
          ENDDO
        ENDIF
        IF(IGAP == 0)THEN
          AAA = TZINF+CURV_MAX(NE)
        ELSE
          AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE)))+DGAPLOAD,DRAD)
        ENDIF
        !il est possible d'ameliorer l'algo en decoupant la facette
        !en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee
        M1 = IRECT(1,NE)
        M2 = IRECT(2,NE)
        M3 = IRECT(3,NE)
        M4 = IRECT(4,NE)

        XX1=X(1,M1)
        XX2=X(1,M2)
        XX3=X(1,M3)
        XX4=X(1,M4)
        XMAXE=MAX(XX1,XX2,XX3,XX4)
        XMINE=MIN(XX1,XX2,XX3,XX4)

        YY1=X(2,M1)
        YY2=X(2,M2)
        YY3=X(2,M3)
        YY4=X(2,M4)
        YMAXE=MAX(YY1,YY2,YY3,YY4)
        YMINE=MIN(YY1,YY2,YY3,YY4)

        ZZ1=X(3,M1)
        ZZ2=X(3,M2)
        ZZ3=X(3,M3)
        ZZ4=X(3,M4)
        ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
        ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)

        !calcul de la surface (pour elimination future de candidats)
        SX = (YY3-YY1)*(ZZ4-ZZ2) - (ZZ3-ZZ1)*(YY4-YY2)
        SY = (ZZ3-ZZ1)*(XX4-XX2) - (XX3-XX1)*(ZZ4-ZZ2)
        SZ = (XX3-XX1)*(YY4-YY2) - (YY3-YY1)*(XX4-XX2)
        S2 = SX*SX + SY*SY + SZ*SZ

        !indice des voxels occupes par la facette
        IF(NBX>1) THEN
          IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
          IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
        ELSE
          IX1=-2
          IX2=1
        ENDIF

        IF(NBY>1) THEN
          IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
          IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
        ELSE
          IY1=-2
          IY2=1
        ENDIF

        IF(NBZ>1) THEN
          IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))
          IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))
        ELSE
          IZ1=-2
          IZ2=1
        ENDIF

        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))

        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))

        !nbpelem = 0
        !nnpelem = 0
        !nnr0pelem = 0
        !nnrpelem = 0

        DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
            DO IX = IX1,IX2

              ! nbpelem = nbpelem + 1

              JJ = VOXEL(IX,IY,IZ)
              DO WHILE(JJ /= 0)
                DELNOD = 0

                !nnpelem = nnpelem + 1
                IF(JJ<=NSN)THEN
                  NN=NSV(JJ)

                  IF(NN == M1)GOTO 200
                  IF(NN == M2)GOTO 200
                  IF(NN == M3)GOTO 200
                  IF(NN == M4)GOTO 200

                  IF(FLAGREMNODE == 2) THEN
                    IF( TAGREMNODE(NSV(JJ)) == 1)GOTO 200
                  ENDIF
                  XS = X(1,NN)
                  YS = X(2,NN)
                  ZS = X(3,NN)
                  IF(IGAP /= 0)THEN
                    AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,GAP_S(JJ)+GAP_M(NE)))+DGAPLOAD,DRAD)
                  ENDIF
                ELSE
                  J=JJ-NSN
                  IF(FLAGREMNODE == 2) THEN
                    NN = IREM(1,J)
                    K = KREMNOD(2*(NE-1)+2) + 1
                    L = KREMNOD(2*(NE-1)+3)
                    DO M=K,L
                      IF(REMNOD(M) == -IREM(2,J) ) THEN
                        DELNOD = DELNOD + 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF(DELNOD /= 0)GOTO 200
                  ENDIF

                  XS = XREM(1,J)
                  YS = XREM(2,J)
                  ZS = XREM(3,J)
                  IF(IGAP /= 0)THEN
                    AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,XREM(9,J)+GAP_M(NE)))+DGAPLOAD,DRAD)
                  ENDIF
                ENDIF

                IF(XS<=XMINE-AAA)GOTO 200
                IF(XS>=XMAXE+AAA)GOTO 200
                IF(YS<=YMINE-AAA)GOTO 200
                IF(YS>=YMAXE+AAA)GOTO 200
                IF(ZS<=ZMINE-AAA)GOTO 200
                IF(ZS>=ZMAXE+AAA)GOTO 200

                !sousestimation de la distance**2 pour elimination de candidats
                !nnr0pelem = nnr0pelem + 1

                D1X = XS - XX1
                D1Y = YS - YY1
                D1Z = ZS - ZZ1
                D2X = XS - XX2
                D2Y = YS - YY2
                D2Z = ZS - ZZ2
                DD1 = D1X*SX+D1Y*SY+D1Z*SZ
                DD2 = D2X*SX+D2Y*SY+D2Z*SZ
                IF(DD1*DD2 > ZERO)THEN
                  D2 = MIN(DD1*DD1,DD2*DD2)
                  A2 = AAA*AAA*S2
                  IF(D2 > A2)GOTO 200
                ENDIF

                !nnrpelem = nnrpelem + 1

                J_STOK = J_STOK + 1
                PROV_N(J_STOK) = JJ
                PROV_E(J_STOK) = NE
                IF(J_STOK == NVSIZ)THEN
                  CALL I7STO(
     1               NVSIZ ,IRECT  ,X     ,NSV   ,II_STOK,
     2               CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3               I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4               IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5               OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6               GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7               GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8               CAND_F ,DGAPLOAD)
                  IF(I_MEM==2)GOTO 100
                  J_STOK = 0
                ENDIF
  200           CONTINUE
                JJ = NEXT_NOD(JJ)
              ENDDO ! WHILE(JJ /= 0)
            ENDDO ! X
          ENDDO  ! Y
        ENDDO   ! Z
        IF(FLAGREMNODE == 2) THEN
          K = KREMNOD(2*(NE-1)+1)+1
          L = KREMNOD(2*(NE-1)+2)
          DO I=K,L
            TAGREMNODE(REMNOD(I)) = 0
          ENDDO
        ENDIF
      ENDDO
C=======================================================================
C 4   STORAGE
C=======================================================================
      IF(J_STOK/=0)CALL I7STO(
     1              J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2              CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3              I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4              IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5              OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6              GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7              GAP_S_L,GAP_M_L,INTTH,DRAD  ,ITIED    ,
     8              CAND_F ,DGAPLOAD)

C=======================================================================
C 5   VOXEL CLUSTERING  RESET
C=======================================================================
  100 CONTINUE
      ! Barrier to avoid reinitialization before end of sorting
      CALL MY_BARRIER
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      DO I=NSNF,NSNL
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
      NSNF = 1 + ITASK*REMOTE_S_NODE / NTHREAD
      NSNL = (ITASK+1)*REMOTE_S_NODE / NTHREAD
      IF(ITASK+1==NTHREAD) NSNL=REMOTE_S_NODE
      DO JJ = NSNF, NSNL
        J = LIST_REMOTE_S_NODE(JJ)
        VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J))=0
      ENDDO
      CALL MY_BARRIER()

C=======================================================================
C 6   DEBUG OUTPUT
C=======================================================================

      DBG_type18_fvm=.false.
      if(inacti==7 .AND. DBG_type18_fvm)then
        write(*,FMT='(A)')"------------------------------------------"
        write(*,*)"RESULT : Search Algorithm with VOXEL partitioning"
        write(*,*)"  Number of couples =", II_STOK
        if(II_STOK>0)then
          write(*,FMT='(A,(I10))')"  --> SECONDARY Node ids: ", CAND_N(1:II_STOK)
          write(*,FMT='(A,(I10))')"  --> Local Face ids: ", CAND_E(1:II_STOK)
        endif
        write(*,*)" Structure domain :"
        write(*,FMT='(A,F30.16,A,F30.16)')" Xmin=",XMIN,"  Xmax=",XMAX
        write(*,FMT='(A,F30.16,A,F30.16)')" Ymin=",YMIN,"  Ymax=",YMAX
        write(*,FMT='(A,F30.16,A,F30.16)')" Zmin=",ZMIN,"  Zmax=",ZMAX
        write(*,*)" Partitioning domain :"
        write(*,*)"   TZINF,AAA=",TZINF,AAA
        write(*,FMT='(A,F30.16,A,F30.16)')" Xmin=",XMIN-AAA,"  Xmax=",XMAX+AAA
        write(*,FMT='(A,F30.16,A,F30.16)')" Ymin=",YMIN-AAA,"  Ymax=",YMAX+AAA
        write(*,FMT='(A,F30.16,A,F30.16)')" Zmin=",ZMIN-AAA,"  Zmax=",ZMAX+AAA
        write(*,FMT='(A)')"------------------------------------------"
      endif

C=======================================================================
C 7   DEALLOCATE
C=======================================================================
      IF(ITASK == 0)THEN
        DEALLOCATE(NEXT_NOD)
        DEALLOCATE(IIX)
        DEALLOCATE(IIY)
        DEALLOCATE(IIZ)
      ENDIF
      IF(FLAGREMNODE == 2) THEN
        IF(ALLOCATED(TAGREMNODE)) DEALLOCATE(TAGREMNODE)
      ENDIF


      RETURN
      END



