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
Chd|  DOMETIS2                      source/spmd/domain_decomposition/domdecs.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE DOMETIS2(
     1  IXS   , IXQ   , IXC     , IXT     , IXP   ,
     2  IXR   , IXTG    , CEP     , GEO   ,
     3  WD    , NELEM , IDDLEVEL, NELEMINT, IXINT ,
     4  PM    , X     , KXX     , IXX     , NSUB  ,
     5  NUMNOD, NUMELS, NUMELQ  , NUMELC  , NUMELT,
     6  NUMELP, NUMELR, NUMELTG , NUMELX,
     7  ADSKY , IGEO  , DSARCH  , ISOLNOD , IWCONT,
     8  IWCIN2, DSDOF , IPM     ,BUFMAT,
     9  NUMMAT,NUMGEO ,TAILLE   ,POIN_UMP ,TAB_UMP,
     1  POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     2  TABMP_L,IPART,IPARTC,IPARTG,
     3  IPARTS ,NPART,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     4  MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL)
      USE MESSAGE_MOD
      USE MID_PID_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr12_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),
     .        CEP(*), NELEM,IDDLEVEL, NELEMINT, IXINT(6,*), ISOLNOD(*),
     .        KXX(NIXX,*),IXX(*), NSUB, NUMNOD, NUMELS, NUMELQ,  
     .        NUMELC, NUMELT, NUMELP, NUMELR, NUMELTG,  NUMELX,
     .        ADSKY(0:*), IGEO(NPROPGI,NUMGEO), IWCONT(*), IWCIN2(*),
     .        DSDOF(*), DSARCH,times,IPM(*),TABMP_L,NPART
      INTEGER, DIMENSION(LIPART1,*), INTENT(IN) :: IPART
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC,IPARTG,IPARTS
      my_real 
     .        GEO(NPROPG,*),  PM(NPROPM,*), X(3,*),BUFMAT(*)
      REAL    WD(*)
      INTEGER, DIMENSION(2,NPART), INTENT(IN) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(IN) :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(IN) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(IN) :: MID_PID_SOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSEG, I, J, UTIL, K, NUSE,
     .        LCNE,ISH1,ISH2,
     .        NEDGES, ELK, OFF,CC1, CC2, NUMG1, NUMG2,
     .        IEND,INED,L,M,N,NEWEDGE,NEDGES_OLD,
     .        LENWORK,NOD1, NOD2, MODE, NELEM0, MM,
     .        WORK(70000), NUML, OK1, OK2,
     .        ELEMD, CEPD, IMMNUL, NEDDEL, ITYPINT,
     .        MAXI, MAXJ, MAX, I1, I2, I3, N1, N2, NUMG3, NUMG4,
     .        NELX,ADDX,NCOND,NFLAG,IWFLG,NODC,ICUR,IERR1,NEC,
     .        IFSI,MID,PID,JALE,MLN,OPTIONS(40),
     .        NUMMAT    ,NUMGEO   ,TAILLE, JALE_FROM_MAT, JALE_FROM_PROP
      INTEGER, DIMENSION(NUMMAT_OLD+NUMGEO_OLD) :: POIN_UMP_OLD
      INTEGER, DIMENSION(7,TAILLE_OLD) :: TAB_UMP_OLD
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      INTEGER, DIMENSION(:),ALLOCATABLE :: XADJ, ADJNCY, IWD,
     .        IENDT, CNE, ITRI, INDEX, EDGE,ITRIM,INDEXM
      CHARACTER FILNAM*13, KEYA*80
C     REAL OU REAL*8
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD
      REAL    UBVEC(15), W, WI, WI2, WDDL, WS, WFSI
C metis5 null pointers
      INTEGER, POINTER :: adjwgt(:)=>null(),vsize(:)=>null()
      REAL, POINTER :: tpwgts(:)=>null()
      INTEGER METIS_PartGraphKway, METIS_PartGraphRecursive
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL METIS_PartGraphKway, METIS_PartGraphRecursive
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C----------------------------------
C comptage NEDGE global
C----------------------------------
      DO I=1,NUMNOD+1
        ADSKY(I) = 0
      END DO
C.....memoire necessaire
      DO 110 K=2,9
        DO 110 I=1,NUMELS
          N = IXS(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
 110  CONTINUE
C
      DO 120 K=2,5
        DO 120 I=1,NUMELQ
          N = IXQ(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
 120  CONTINUE
C
      DO 130 K=2,5
        DO 130 I=1,NUMELC
          N = IXC(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
 130  CONTINUE
C
      DO 140 K=2,3
        DO 140 I=1,NUMELT
          N = IXT(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
 140  CONTINUE
C
      DO 150 K=2,3
        DO 150 I=1,NUMELP
          N = IXP(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
 150  CONTINUE
C
C traitement a part du 3eme noeud optionnel sauf type 12
      DO K=2,3
        DO I=1,NUMELR
          N = IXR(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      DO I=1,NUMELR
        N = IXR(4,I) + 1
        IF(NINT(GEO(12,IXR(1,I)))==12) THEN
          ADSKY(N) = ADSKY(N) + 1
        ENDIF
      ENDDO
C
      DO 170 K=2,4
        DO 170 I=1,NUMELTG
          N = IXTG(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
 170  CONTINUE

C
       DO I=1,NUMELX
        NELX=KXX(3,I) 
         DO K=1,NELX
           ADDX = KXX(4,I)+K-1
           N=IXX(ADDX)+1
           ADSKY(N)= ADSKY(N)+1
         ENDDO
      ENDDO
        ADSKY(1) = 1
        DO I=2,NUMNOD+1
          ADSKY(I) = ADSKY(I) + ADSKY(I-1)
        END DO
        LCNE = ADSKY(NUMNOD+1)
        ALLOCATE(CNE(LCNE),STAT=IERR1)
C
        IF(IERR1/=0)THEN
          CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
        END IF
C
C-----------------------------------------------
C   CALCUL DE CNE
C-----------------------------------------------
C poids sous forme reel pour compatibilite RSB ancienne
      DO I = 1, NELEM
        WD(I) = 0.
      ENDDO
      ELEMD = 0
C NCOND : nombre de contraintes multiniveaux a equilibrer
C poids 1 => elt ; poids 2 => interf 7,10,11 ; poids 3 => int 2; poids 4 => ddl
      NCOND = 3
C implicite : poids 1 => ddl ; poids 2 => interf 7,10,11 ; poids 3 => int 2; poids 4 => elt
      IF(DECTYP==5.OR.DECTYP==6)THEN
        NCOND=4
      ELSE
C poids supplementaire si fluide structure interaction => ilag=1 et iale=1 ou ieuler=1
        IF(ILAG==1.AND.(IALE==1.OR.IEULER==1))THEN
          IFSI = 1
        ELSE
          IFSI = 0
        END IF
C
C si fsi poids 1 => elt LAG ; poids 4 => elt ALE
        IF(IFSI==1)NCOND=NCOND+1
c      NCOND = 4
C poids sur element delete si necessaire (poids delete en derniere position ncond)
        IF(ELEMD>0) NCOND = NCOND+1
      END IF
C
      ALLOCATE(IWD(NELEM*NCOND),STAT=IERR1)
C poids Metis suivant 
      DO I = 1, NCOND*NELEM
        IWD(I) = 0
      ENDDO
C optimisation des poids par defaut
      CALL INITWG(WD,PM,GEO,IXS,IXQ,
     .            IXC,IXT,IXP,IXR,IXTG,
     .            KXX,IGEO,ISOLNOD,DSARCH,
     .            NUMELS, NUMELQ,NUMELC,NUMELT,NUMELP, 
     .            NUMELR, NUMELTG,NUMELX,IPM,
     .            BUFMAT,NUMMAT,NUMGEO,TAILLE,POIN_UMP,
     .            TAB_UMP,POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     .            TABMP_L,IPART,IPARTC,IPARTG,
     .            IPARTS,NPART,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     .            MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,IDDLEVEL)
      DO I=1,NUMELS
        DO K=1,8
          N = IXS(K+1,I)
          CNE(ADSKY(N)) = I
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I-1)+1) = IWD(NCOND*(I-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I-1)+2) = IWD(NCOND*(I-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I-1)+3) = IWD(NCOND*(I-1)+3)+IWCIN2(N)
          END IF
        ENDDO
      ENDDO
C
      OFF = NUMELS
C
      DO I = 1, NUMELQ
        DO K=1,4
          N = IXQ(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFF = OFF + NUMELQ
C
      DO I = 1, NUMELC
       DO K=1,4
          N = IXC(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDDO
      ENDDO
C
      OFF = OFF + NUMELC
C
      DO I = 1, NUMELT
        DO K=1,2
          N = IXT(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDDO
       ENDDO
C
       OFF = OFF + NUMELT
C
      DO I = 1, NUMELP
        DO K=1,2
          N = IXP(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDDO
      ENDDO
C
      OFF = OFF + NUMELP 
C
      DO I = 1, NUMELR
        DO K=1,2
          N = IXR(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDDO
        N = IXR(4,I)
        IF(NINT(GEO(12,IXR(1,I)))==12) THEN
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDIF
      ENDDO
C
      OFF = OFF + NUMELR
C
      DO I = 1, NUMELTG
        DO K=1,3
          N = IXTG(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDDO
      ENDDO
C
      OFF = OFF + NUMELTG
C
      DO I=1, NUMELX
        NELX=KXX(3,I)
        DO K=1,NELX
          ADDX = KXX(4,I)+K-1
          N=IXX(ADDX)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
          IF(N/=0)THEN
C poids ddl
            IWD(NCOND*(I+OFF-1)+1) = IWD(NCOND*(I+OFF-1)+1)+DSDOF(N)
C poids interface
            IWD(NCOND*(I+OFF-1)+2) = IWD(NCOND*(I+OFF-1)+2)+IWCONT(N)
C poids int2
            IWD(NCOND*(I+OFF-1)+3) = IWD(NCOND*(I+OFF-1)+3)+IWCIN2(N)
          END IF
        ENDDO
      ENDDO
C
      OFF = OFF + NUMELX
C     remise au debut des adresses
      DO I=NUMNOD+1,2,-1
        ADSKY(I) = ADSKY(I-1)
      END DO
      ADSKY(1) = 1
C.....construction des couples Ei Ej connectes par un point

      NEDGES = 0
      DO N = 1, NUMNOD
        DO CC1 = ADSKY(N), ADSKY(N+1)-1
          NUMG1 = CNE(CC1)
          DO CC2 = CC1+1, ADSKY(N+1)-1
            NUMG2 = CNE(CC2)
            NEDGES = NEDGES + 1
          ENDDO
        ENDDO
      ENDDO
*      WRITE(*,*) 'LCNE ',LCNE,' CNE '
*      WRITE(*,'(10I8)') (CNE(I),I=1,LCNE)
*      WRITE(*,*) 'ADSKY '
*      WRITE(*,'(10I8)') (ADSKY(I),I=0,NUMNOD+1)
      IF (IDDLEVEL==1) NEDGES = NEDGES+NELEMINT
      ALLOCATE(IENDT(2*NEDGES),STAT=IERR1)
C
      IF(IERR1/=0)THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
      END IF
      NEDGES_OLD = NEDGES
      NEDGES = 0
      DO N = 1, NUMNOD
        DO CC1 = ADSKY(N), ADSKY(N+1)-1
          NUMG1 = CNE(CC1)
          DO CC2 = CC1+1, ADSKY(N+1)-1
            NUMG2 = CNE(CC2)
            NEDGES = NEDGES + 1
            IENDT(NEDGES*2-1) = NUMG1
            IENDT(NEDGES*2) = NUMG2
          ENDDO
        ENDDO
      ENDDO
      IF (IDDLEVEL==1) THEN
C-----------------------------------------------
C CONNECTIVITES INTERFACES
C-----------------------------------------------
C
C seulement rajouter une connectivite entre noeud et facette
C
C eviter conflit entre type 2 et type 7
C
C cep temporairement utilise comme flag
        DO I = 1, NELEM
          CEP(I) = 0
        ENDDO
C
        DO J = 1, 2
C hierarchie type 2, type 7
         DO I = 1, NELEMINT
          N=IXINT(5,I)
          NUMG1=CNE(ADSKY(N))
C initialisation numg2 pour cas ou facette non trouvee (erreur)
          NUMG2=NUMG1
          ITYPINT=ABS(IXINT(6,I))
          IF((ITYPINT==2.AND.J==1).OR.
     .       (ITYPINT==7.AND.J==2)) THEN
           IF(ADSKY(N+1)-ADSKY(N)>0)THEN
            N=IXINT(1,I)
            N1=IXINT(2,I)
            N2=IXINT(3,I)
            DO I1 = ADSKY(N), ADSKY(N+1)-1
              NUMG2=CNE(I1)
              DO I2 = ADSKY(N1), ADSKY(N1+1)-1
                NUMG3=CNE(I2)
                IF(NUMG3==NUMG2) THEN
                  DO I3 = ADSKY(N2), ADSKY(N2+1)-1
                    NUMG4=CNE(I3)
                    IF(NUMG4==NUMG2) GOTO 100
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
 100        CONTINUE
            IF(ITYPINT==2.OR.
     .         (CEP(NUMG1)==0.AND.CEP(NUMG2)==0)) THEN
              NEDGES = NEDGES + 1
              IENDT(NEDGES*2-1) = NUMG1
              IENDT(NEDGES*2) = NUMG2
            ENDIF
            CEP(NUMG1) = 1
            CEP(NUMG2) = 1
           ENDIF
          ENDIF
         ENDDO 
        ENDDO
      ENDIF
C
      IF (NEDGES>0) THEN
        ALLOCATE(ITRI(2*NEDGES),STAT=IERR1)
        IF(IERR1/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
        ALLOCATE(INDEX(2*NEDGES),STAT=IERR1)
        IF(IERR1/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
C
        DO I = 1, NEDGES
          ITRI(2*I-1) = IENDT(2*I-1)
          ITRI(2*I) = IENDT(2*I)
          INDEX(I) = I
        ENDDO
        MODE = 0
        CALL MY_ORDERS(MODE,WORK,ITRI,INDEX,NEDGES,2)
        DO I = 1, NEDGES
          IENDT(2*I-1) = ITRI(2*INDEX(I)-1)
          IENDT(2*I) = ITRI(2*INDEX(I))
        ENDDO
        I = 1
        NOD1 = IENDT(2*I-1)
        NOD2 = IENDT(2*I)
        NEWEDGE = 1
        DO I = 2, NEDGES
          IF ((NOD1/=IENDT(2*I-1).OR.NOD2/=IENDT(2*I)).AND.
     .         IENDT(2*I-1)/=IENDT(2*I)) THEN
            NEWEDGE = NEWEDGE + 1
            IENDT(2*NEWEDGE-1) = IENDT(2*I-1)
            IENDT(2*NEWEDGE) = IENDT(2*I)
            NOD1 = IENDT(2*I-1)
            NOD2 = IENDT(2*I)
          ENDIF
        ENDDO
        DEALLOCATE(ITRI)
        DEALLOCATE(INDEX)
C
        NEDGES = NEWEDGE
C structures addtionnelles temporaires
        ALLOCATE(EDGE(4*NEDGES))
        ALLOCATE(ITRIM(4*NEDGES))
        ALLOCATE(INDEXM(4*NEDGES))
C structures Metis
        ALLOCATE(XADJ(NELEM+1),STAT=IERR1)
        ALLOCATE(ADJNCY(2*NEDGES),STAT=IERR1)
C       NCOND=2
C toutes les arretes
        DO I = 1, NEDGES
          NOD1=IENDT(2*I-1)
          NOD2=IENDT(2*I)
          EDGE(2*I-1)=NOD1
          EDGE(2*I)=NOD2
          EDGE(2*NEDGES+2*I-1)=NOD2
          EDGE(2*NEDGES+2*I)=NOD1
        END DO
        DO I = 1, 2*NEDGES
          ITRIM(2*I-1) = EDGE(2*I-1)
          ITRIM(2*I) = EDGE(2*I)
          INDEXM(I) = I
        ENDDO
        MODE = 0
        CALL MY_ORDERS(MODE,WORK,ITRIM,INDEXM,2*NEDGES,2)
        DO I = 1, 2*NEDGES
          EDGE(2*I-1)= ITRIM(2*INDEXM(I)-1)
          EDGE(2*I)  = ITRIM(2*INDEXM(I))
        ENDDO
C
        XADJ(1) = 1
        NODC=1
        ICUR=1
        DO I = 1, 2*NEDGES
          NOD1=EDGE(2*I-1)
          NOD2=EDGE(2*I)
          DO WHILE (NOD1/=NODC)
            NODC=NODC+1
            XADJ(NODC)=ICUR
          END DO
          ADJNCY(ICUR)=NOD2
          ICUR=ICUR+1
        END DO
        DEALLOCATE(EDGE,ITRIM,INDEXM)
        DO I = NODC+1,NELEM+1         !  cas elements non connectes
          XADJ(I) = ICUR
        END DO
C          XADJ(NODC+1)=ICUR
      END IF
C      
      DEALLOCATE(IENDT)
      DEALLOCATE(CNE)
C
      IF (NEDGES>0.AND.NSUB>1) THEN
        IWFLG=2
        NFLAG=1
        OPTIONS(1)=0
C new Metis5 Definition
         DO I = 1, 40
           OPTIONS(I) = -1
         END DO
         OPTIONS(18)=1
C Domain decomposition CRASH ou FSI
        IF(DECTYP==3.OR.DECTYP==4)THEN
          IF(IFSI==0)THEN
            DO I = 1, NELEM
C normalisation des poids (elem delete a 0)
              IWD(NCOND*(I-1)+1) = INT(WD(I)*1000)
C poids interface deja calcul
            END DO
          ELSE
            DO I = 1, NELEM
              IF(I<=NUMELS)THEN
                MID = ABS(IXS(1,I))
                PID = ABS(IXS(10,I))
                JALE_FROM_MAT = NINT(PM(72,MID))
                JALE_FROM_PROP = IGEO(62,PID)
                JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader
                MLN = NINT(PM(19,MID))
                IF(JALE==0.AND.MLN/=18)THEN
                  IWD(NCOND*(I-1)+1) = INT(WD(I)*1000)
                  IWD(NCOND*(I-1)+4) = 0
                ELSE
                  IWD(NCOND*(I-1)+1) = 0
                  IWD(NCOND*(I-1)+4) = INT(WD(I)*1000)
                END IF
              ELSE
C normalisation des poids (elem delete a 0)
                IWD(NCOND*(I-1)+1) = INT(WD(I)*1000)
              END IF
C poids interface deja calcul
            END DO 
          END IF
          IF(ELEMD>0)THEN
            DO I = 1, NELEM
Cem delete
              IF(WD(I)==0.0001)THEN
                IWD(NCOND*(I-1)+NCOND) = 1
              END IF
C poids interface deja calcul
            END DO
          END IF

          IF(DECTYP==3)THEN
C KWAY METIS
            UBVEC(1) = 1.03   ! tolerance elt lagrangien
            UBVEC(2) = 1.10   ! tolerance interf contact
            UBVEC(3) = 1.10   ! tolerance int2
            IF(IFSI/=0) UBVEC(4) = 1.05   ! tolerance elt ale
            IF(ELEMD/=0) UBVEC(NCOND) = 1.10  ! tolerance elements deletes si present
            IERR1 = METIS_PartGraphKway(
     1        NELEM,NCOND,XADJ,ADJNCY,
     2        IWD,vsize,adjwgt,NSUB,tpwgts,
     3        UBVEC,OPTIONS,NEC,CEP)
C RSB METIS
          ELSEIF(DECTYP==4)THEN
          IERR1 = METIS_PartGraphRecursive(
     1      NELEM,NCOND,XADJ,ADJNCY,
     2      IWD,vsize,adjwgt,NSUB,tpwgts,
     3      UBVEC,OPTIONS,NEC,CEP)
          END IF
C Domain decomposition pour implicit
        ELSE
          DO I = 1, NELEM
C normalisation des poids (elem delete a 0)
            IWD(NCOND*(I-1)+4) = INT(WD(I)*1000)
          END DO
          IF(DECTYP==5)THEN
C KWAY METIS IMPLICIT
            UBVEC(1) = 1.03     ! tolerance ddl
            UBVEC(2) = 1.10   ! tolerance interf contact
            UBVEC(3) = 1.10   ! tolerance int2
            UBVEC(4) = 1.05   ! tolerance elements
            IERR1 = METIS_PartGraphKway(
     1        NELEM,NCOND,XADJ,ADJNCY,
     2        IWD,vsize,adjwgt,NSUB,tpwgts,
     3        UBVEC,OPTIONS,NEC,CEP)
C RSB METIS IMPLICIT
          ELSEIF(DECTYP==6)THEN
            IERR1 = METIS_PartGraphRecursive(
     1        NELEM,NCOND,XADJ,ADJNCY,
     2        IWD,vsize,adjwgt,NSUB,tpwgts,
     3        UBVEC,OPTIONS,NEC,CEP)
          END IF
        END IF
c      if(times==1247)then
c        return
c      endif
        DO I = 1, NELEM
          CEP(I) = CEP(I)-1
        END DO
        DEALLOCATE(XADJ,ADJNCY)
      ELSE
C un seul element ou elements non connectes
        DO I = 1, NELEM
          CEP(I) = 0
        ENDDO
      ENDIF
C
      DEALLOCATE(IWD)
C
      RETURN
      END
