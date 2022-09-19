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
Chd|====================================================================
Chd|  I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I22GET_PREV_DATA              source/interfaces/int22/i22get_prev_data.F
Chd|        I22IDENT                      source/interfaces/int22/i22ident.F
Chd|        I22INTERSECT                  source/interfaces/int22/i22intersect.F
Chd|        I22TRIVOX                     source/interfaces/intsort/i22trivox.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I22BUCE(
     1   X        ,IRECT   ,NSV          ,INACTI   ,ISKIP     ,
     2   NMN      ,NSHEL_T ,NSN          ,CAND_E   ,CAND_B    ,
     3   GAP      ,NOINT   ,II_STOK      ,NCONTACT ,BMINMA    ,
     4   TZINF    ,MAXBOX  ,MINBOX       ,MWAG     ,CURV_MAX  ,
     5   NB_N_B   ,ESHIFT  ,ILD          ,IFQ      ,IFPEN     ,
     8   STFN     ,NIN     ,STF          ,IGAP     ,
     A   NSHELR_L ,NCONT   ,RENUM        ,NSNROLD  ,
     B   GAPMIN   ,GAPMAX  ,CURV_MAX_MAX ,NUM_IMP  ,
     C   INTTH    ,ITASK   ,BGAPSMX      ,I_MEM    ,
     D   IXS      ,BUFBRIC ,NBRIC        ,ITAB     ,NSHEL_L   ,
     E   ALE_CONNECTIVITY ,IPARI)
C============================================================================
C  cette routine est appelee par : I22MAINCT(/int7/i22mainct.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I22TRI(/int7/i22trivox.F)
C                          ARRET(/sortie/arret.F)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE I22TRI_MOD
      USE MESSAGE_MOD
      USE I22EDGE_MOD
      USE I22BUFBRIC_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI), ISKIP
      INTEGER NMN,  NSN, NOINT,IDT,INACTI,IFQ, NIN, NSNR,NSNROLD
      INTEGER N_CAND_B
      INTEGER IRECT(4,*),NSV(*),MWAG(*), RENUM(*),NUM_IMP, ITASK
      INTEGER CAND_E(*),CAND_B(*),IFPEN(*), IXS(NIXS,*), BUFBRIC(NBRIC)
      INTEGER NCONTACT,ESHIFT,ILD,NB_N_B,IGAP,NCONT,INTTH,I_MEM,NBRIC
      INTEGER ITAB(*),NSHEL_T,NSHEL_L, NSHELR_L, II_STOK
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,CURV_MAX_MAX,
     .   GAPMIN, GAPMAX, BMINMA(6),CURV_MAX(NSHEL_T),BGAPSMX
      my_real
     .   X(3,*),  STFN(*),
     .   STF(*)
      INTEGER :: CANDB, CANDE, NB_SHORT, IPOS_, IREF,ILEN,IVAL
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(BRICK_ENTITY),  DIMENSION(:),ALLOCATABLE :: BRICK_GRID
      TYPE(EDGE_ENTITY),   DIMENSION(:),ALLOCATABLE ::  EDGE_GRID

      INTEGER I_ADD_MAX,ICUR
      PARAMETER (I_ADD_MAX = 1001)

      INTEGER I, J,  I_ADD, IP0, IP1, MAXSIZ,
     .        ADD(2,I_ADD_MAX), LOC_PROC, N, ISZNSNR,
     .        NSNFIOLD(NSPMD)

      my_real MARGE, AAA, BID

      CHARACTER*8 KEY

      INTEGER                            :: NCAND, NBF, NBL, SOMB, SOME, IPA
      INTEGER                            :: TMP1, TMP2, IPOS
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IFIRST, ILAST
      CHARACTER*12 ::filename

      INTEGER, ALLOCATABLE, DIMENSION(:) :: order, VALUE

      INTEGER R2,MIN2

C-----------------------------------------------
C     PROV
C-----------------------------------------------
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IP0     = 1
      ISZNSNR = 0
      I_MEM   = 0
      MARGE   = 1.1 * TZINF-GAP

      AAA = SQRT(NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
      NBX = MAX(NBX,1)+2
      NBY = MAX(NBY,1)+2
      NBZ = MAX(NBZ,1)+2

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL

      IF(RES8 > LVOXEL8) THEN
        if(itask==0.and.ibug22_tri==1)print *, "redim Voxel"
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)

      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
        print *, "stop 678"
c       stop 678
      end if

       !     initialisation complete de VOXEL
       ! (en // SMP il y a possibilite de redondance de traitement mais no pb)
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0 !---reset voxel brick---!
      ENDDO

      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)

      if(itask==0.and.ibug22_tri==1)print *, "call i22trivox"

      CALL I22TRIVOX(
     1   NSN     ,RENUM    ,NSHELR_L ,ISZNSNR  ,I_MEM   ,
     2   IRECT   ,X        ,STF      ,STFN     ,BMINMA  ,
     3   NSV     ,II_STOK  ,CAND_B   ,ESHIFT   ,CAND_E  ,
     4   NCONTACT,NOINT    ,TZINF    ,
     5   VOXEL1  ,NBX      ,NBY      ,NBZ      ,
     6   BID     ,
     7   NSHEL_T ,
     8   MARGE    ,
     9   NIN     ,ITASK    ,IXS      ,BUFBRIC ,
     A   NBRIC   ,ITAB     ,NSHEL_L )

C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .            I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
        ISKIP=1
      ELSEIF(I_MEM==2) THEN
        IF(DEBUG(1)>=1) THEN
          IWARN = IWARN+1
#include "lockon.inc"
          WRITE(ISTDO,*)' **WARNING INTERFACE/MEMORY'
          WRITE(IOUT,*)' **WARNING INTERFACE NB:',NOINT
          WRITE(IOUT,*)'       TOO MANY POSSIBLE IMPACTS'
          WRITE(IOUT,*)'       SIZE OF INFLUENCE ZONE IS'
          WRITE(IOUT,*)'       EXAPNDED'
#include "lockoff.inc"
        ENDIF
C        TZINF = THREE_OVER_4*TZINF   !non sinon on perd les candidates voisines
        ILD = 1
        ISKIP=1
      ELSEIF(I_MEM==3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=100,ANMODE=ANINFO,
     .            I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
        ISKIP=1
      ENDIF


!-----------------debug
      if(itask==0.and.ibug22_tri==1)then
      print *, "  |------------i22buce.F----------|"
      print *, "  |       LISTE DES CANDIDATS     |"
      print *, "  |-------------------------------|"
        allocate(order(ii_stok) ,VALUE(II_STOK))
        MIN2 = MINVAL(ABS(CAND_E(1:II_STOK)))
        R2 = MAXVAL(ABS(CAND_E(1:II_STOK)))-MIN2
        DO I=1,II_STOK
          VALUE(I) = CAND_B(I)*(R2+1)+ABS(CAND_E(I))-MIN2
        ENDDO
        order=0
        !CALL QUICKSORT_I2 !(ORDER,II_STOK,VALUE)
        DO I=1,II_STOK
          if(CAND_E(ORDER(I))>0)then
            print *,I,IXS(11,bufbric(CAND_B(ORDER(I)))),
     .           "avec+",NINT(IRECT_L(1:4,IABS(CAND_E(ORDER(I)))))   !negative value means that there is no intersection at all for this couple.
          else
            print *,I,IXS(11,bufbric(CAND_B(ORDER(I)))),
     .           "avec-",NINT(IRECT_L(1:4,IABS(CAND_E(ORDER(I)))))   !negative value means that there is no intersection at all for this couple.
          endif
        END DO
        deallocate(order,value)
      end if
!-----------------debug

!exemple de liste de candidats
!    II_STOK    IXS(11,bufbric(CAND_B(I))) )  CAND_B(I)                  NINT(IRECT_L(1:4,IABS(CAND_E(I)))   CAND_E(I)    OCCURENCE BRIQUE
!          01                            39         (3)          avec-  1176806  1176814  1176859  1176876        (12)      <- IFIRST = 01
!          02                            39         (3)          avec-  1176941  1176789  1176791  1176934        (11)      <- ILAST  = 02
!          03                            40         (5)          avec-      211      210  1176779  1176777        (02)      <- IFIRST = 01
!          04                            40         (5)          avec+  1176874  1176777  1176779  1176841        (06)            .
!          05                            40         (5)          avec+  1176874  1176841  1176814  1176806        (14)            .
!          06                            40         (5)          avec+  1176806  1176814  1176859  1176876        (12)            .
!          07                            40         (5)          avec+  1176876  1176859      207      206        (03)            .
!          08                            40         (5)          avec-      209  1176772  1176774      208        (01)       <- ILAST  = 08
 !
!                                NCAND  = 8 (II_STOK)
!  LIST_B = {3,5}              , NCANDB = 2
!  LIST_E = {1,2,3,6,11,12,14} , NCANDE = 7
!
!
      CALL MY_BARRIER

       IF(ISKIP==1)THEN
        NB=NCANDB
        RETURN
      ENDIF

      ! ##########################################
      ! #       CREATING BRICK LIST IDs          #
      ! # fill LIST_B                            #
      ! # fill IADF,IADL                         #
      ! ##########################################
      ! II_STOK est le nombre de couple candidats, y compris les facettes non intersectante dans le voisinage
      ! Ces derniers sont necessaires pour pouvoir y charger les forces de contact appropriees.
      IF(ITASK==0)THEN
        ALLOCATE(ITAGB(1:NBRIC))
        ALLOCATE(IFIRST(1:NBRIC))  !premiere occurence de la brique dans la liste des candidats
        ALLOCATE(ILAST(1:NBRIC))   !derniere occurence de la brique dans la liste des candidats
        ITAGB(:)  = 0
        IFIRST(:) = 0
        ILAST(:)  = 0
        !boucle sur la liste des candidats et tag des briques presentes
        DO I=1,II_STOK
          !pas encore marque
          IF(ITAGB(CAND_B(I)) == 0)THEN
            IFIRST(CAND_B(I)) = I
            ILAST(CAND_B(I))  = I !premier et dernier si aucune autre occurence
            ITAGB(CAND_B(I))  = 1
          !deja marque
          ELSE
            ILAST(CAND_B(I)) = I
          ENDIF
        ENDDO!next I
        NCANDB = SUM(ITAGB(:))    !toutes les briques intersectees ou non sans occurence multiple
        ALLOCATE(LIST_B(NCANDB))
        ALLOCATE(IADF(NCANDB))   !adresse debut dans CAND_B
        ALLOCATE(IADL(NCANDB))   !adresse fin dans CAND_B
        IPOS = 0
        DO I=1,NBRIC
          IF(ITAGB(I) == 0)CYCLE
          IPOS = IPOS + 1
          LIST_B(IPOS) = I
          IADF(IPOS)   = IFIRST(I)
          IADL(IPOS)   = ILAST(I)
        ENDDO
      ENDIF!(ITASK==0)THEN

      ! ##########################################
      ! #       CREATING FACE LIST IDs           #
      ! # fill LIST_E                            #
      ! ##########################################
      ! II_STOK est le nombre de couple candidats, y compris les facettes non intersectante dans le voisinage
      ! Ces derniers sont necessaires pour pouvoir y charger les forces de contact appropriees.
      ! Les ids de facettes negatives designe des facette non intersectante pour la brique concernee
      IF(ITASK==0)THEN
        ALLOCATE(ITAGE(1:NIRECT_L))
        ITAGE(:) = 0
        !boucle sur la liste des candidats et tag des briques presentes
        DO I=1,II_STOK
            ITAGE(IABS(CAND_E(I))) = 1
        ENDDO
        NCANDE = SUM(ITAGE(:))    !toutes les briques intersectees ou non sans occurence multiple
        ALLOCATE(LIST_E(NCANDE))
        IPOS = 0
        DO I=1,NIRECT_L
          IF(ITAGE(I) == 0)CYCLE
          IPOS         = IPOS + 1
          LIST_E(IPOS) = I
          ITAGE(I)     = IPOS  !le tag traite devient la position dans LIST_E
        ENDDO
      ENDIF!(ITASK==0)THEN


! La liste des candidats (CAND_B,CAND_E) contient des occurences multiples et desordonnees de CAND_E.
! La liste LIST_E est la liste ordonne sans repetition.
! pour un couple de candidat donne (input index IDX1) , on associe la position dans LIST_E correspondante ( resultat index IDX2)
!
!    II_STOK                                              GET_LIST_E_POS_FROM_CAND_E_POS                     CAND_E(I)
!          01                                                                   pos   6   DANS LIST_E(:)    <---  (12)
!          02                                                                   pos   5   DANS LIST_E(:)    <---  (11)
!          03                                                                   pos   2   DANS LIST_E(:)    <---  (02)
!          04                                                                   pos   4   DANS LIST_E(:)    <---  (06)
!          05                                                                   pos   7   DANS LIST_E(:)    <---  (14)
!          06                                                                   pos   6   DANS LIST_E(:)    <---  (12)
!          07                                                                   pos   3   DANS LIST_E(:)    <---  (03)
!          08                                                                   pos   1   DANS LIST_E(:)    <---  (01)

!                        CAND_E   12 11 02 06 12 14 03 01
!                        ITAG_E   01 02 03 04 05 06 07 08 09 10 11 12 13 14
!                                  x  x  x        x              x  x     x
!                        LIST_E   01 02 03 06 11 12 14
!
!GET_LIST_E_POS_FROM_CAND_E_POS   06 05 02 04 07 06 03 01


      ! ##########################################
      ! #  SURJECTIVE APP     IDX1 |-> IDX2      #
      ! #  LINK CAND_E(IDX1) TO LIST_E(IDX2)     #
      ! ##########################################
      IF(ITASK==0)THEN
        ALLOCATE(GET_LIST_E_POS_FROM_CAND_E_POS(II_STOK))
        DO I=1,II_STOK
          GET_LIST_E_POS_FROM_CAND_E_POS(I) = ITAGE(IABS(CAND_E(I)))
        ENDDO
      ENDIF!(ITASK==0)THEN

      CALL MY_BARRIER

      NCAND = II_STOK

      if(itask==0.AND.ibug22_tri==1)then

        allocate(order(NCANDB) ,VALUE(NCANDB))
        order = 0
        !CALL QUICKSORT_I2 !(ORDER,NCANDB,list_b)

        print *, ""
        print *, "  |------------i22buce.F----------|"
        print *, "  |     SYNTHESE DES CANDIDATS    |"
        print *, "  |-------------------------------|"
        print *, NCAND , "couples candidats avec :"
        print *, NCANDB , "briques differentes, et"
        print *, NCANDE , "facettes differentes."
        print *, ""
        print *, "  |------------i22buce.F----------|"
        print *, "  |     BRIQUES RETENUES          |"
        print *, "  |     FOR CUT CELL BUFFER       |"
        print *, "  |-------------------------------|"
        print *, (IXS(11,bufbric(list_b(order(j)))),j=1,NCANDB)
        print *, ""
        !print *, "  |------------i22buce.F----------|"
        !print *, "  |       ADRESSES DANS CAND_B    |"
        !print *, "  |-------------------------------|"
        !print *, "  IADF=", IADF
        !print *, "  IADL=", IADL
        !print *, ""
        deallocate(order,VALUE)

      end if

      !NCANDB is now the number of bricks in CAND_B(1:NCAND)
      !LIST_B is now the list of these bricks
      !IADF(I)- IADL(I) is index spectra of a given brick from list_b inside candidate list 1:II_STOK
      !   from IADF(J) to IADL(J) CAND_B(:) is the same one (see lock on/off in i22sto)


C -------------------------------------------------------------
C     RECUPERATION DES DONNEES DE LA CUT CELL DU CYCLE PRECEDENT
C     Indispensable d'avoir un historique pour le calcul de
C     certaines evolutions ou le suivi topologique par exemple.
C -------------------------------------------------------------

      CALL I22GET_PREV_DATA(
     1   X       ,II_STOK  ,CAND_B   ,CAND_E   ,ITASK   ,
     2   NBRIC   ,ITAB     ,BUFBRIC  ,NCAND   ,
     3   IXS     ,NIN)

      !This block must be after I22GET_PREV_DATA OTHERWISE OLD BUFFER IS ERASED

      !s'assurer que cela soit fait dans i22intersect
!      IF(ITASK==0)THEN
!        DO I=1,NCANDB_ADD
!          BRICK_LIST(NIN,NCANDB+I)%ID               = BUFBRIC(LIST_B_ADD(I))
!          BRICK_LIST(NIN,NCANDB+I)%ICODE            = 0
!          BRICK_LIST(NIN,NCANDB+I)%IDBLE            = 0
!          BRICK_LIST(NIN,NCANDB+I)%NBCUT            = 0
!          DO J=1,12
!            BRICK_LIST(NIN,NCANDB+I)%EDGE(J)%NBCUT  = 0
!          ENDDO
!        ENDDO
!      ENDIF

      CALL MY_BARRIER


C -------------------------------------------------------------
C     CALCUL DES POINTS D'INTERSECTION
C  on peut prendre la liste des candidats II_STOK telle quelle
C et la traiter en multi-threading.s
C -------------------------------------------------------------
      ! ##########################################
      ! #       POINTS INTERSECTIONS             #
      ! ##########################################
      CALL I22INTERSECT(
     1   X       ,II_STOK  ,CAND_B   ,CAND_E   ,ITASK   ,
     2   NBRIC   ,ITAB     ,BUFBRIC  ,NCAND   ,
     3   IXS     ,NIN)

      CALL MY_BARRIER

      ! ##########################################
      ! #  INTERPRETATIONS DU PARTITIONNEMENT    #
      ! ##########################################
      CALL I22IDENT(
     1   IXS     ,X        ,ITASK,   NIN, BUFBRIC)

      NB = NCANDB
      CALL MY_BARRIER


      ! ##########################################
      ! #       DECHARGEMENT MEMOIRE             #
      ! ##########################################
      IF(ITASK==0)THEN
        DEALLOCATE(ITAGB)
        DEALLOCATE(ITAGE)
        DEALLOCATE(IFIRST)
        DEALLOCATE(ILAST)
        DEALLOCATE(IADF)
        DEALLOCATE(IADL)
        DEALLOCATE(LIST_B)
        DEALLOCATE(LIST_E)
        DEALLOCATE(GET_LIST_E_POS_FROM_CAND_E_POS)
      ENDIF



  999 CONTINUE


      RETURN
      END
