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
Chd|  DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C_IDDCONNECT                  source/spmd/node/ddtools.F    
Chd|        DD_BFS                        source/spmd/domain_decomposition/grid2mat.F
Chd|        FIND_NODES                    source/spmd/domain_decomposition/grid2mat.F
Chd|        FVBAG_VERTEX                  source/spmd/domain_decomposition/grid2mat.F
Chd|        IDDCONNECTPLUS                source/spmd/node/frontplus.F  
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|        INI_IDDCONNECT                source/spmd/node/ddtools.F    
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_BFS                     source/spmd/node/ddtools.F    
Chd|        PLIST_IDDCONNECT              source/spmd/node/ddtools.F    
Chd|        STAT_DOMDEC                   source/spmd/domain_decomposition/grid2mat.F
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE DOMETIS(
     1  IXS    ,IXQ    ,IXC    ,IXT     ,IXP     ,
     2  IXR    ,IXUR   ,IXTG   ,CEP     ,GEO     ,
     3  ITRI1  ,ITRI2  ,INDEX1 ,INDEX2  ,NUM     ,
     4  WD     ,IWCONT ,NELEM  ,IDDLEVEL,NELEMINT,
     5  IXINT  ,PM     ,X      ,KXX     ,IXX     ,
     6  ADSKY  ,IGEO   ,ISOLNOD,IWCIN2  ,DSDOF   ,
     7  ISOLOFF,ISHEOFF,ITRIOFF,ITRUOFF ,IPOUOFF ,
     8  IRESOFF,IELEM21,IPM    ,IXS10   ,IKINE   , 
     9  CLUSTERS,KXIG3D ,IXIG3D,COST_R2R,BUFMAT,
     1  TAILLE,POIN_UMP,TAB_UMP,
     2  POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     3  NSNT, NMNT,TABMP_L,IQUAOFF,
     4  IGRSURF,FVMAIN,
     5  ITAB  ,IPART  ,IPARTC  ,IPARTG ,IPARTS  ,
     6  POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     7  MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,T_MONVOL,
     8  EBCS_TAG_CELL_SPMD,NPBY,LPBY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE CLUSTER_MOD
      USE FRONT_MOD
      USE REORDER_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE GROUPDEF_MOD
      USE MID_PID_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr12_c.inc"
#include      "rnur_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr15_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "sms_c.inc"
#include      "r2r_c.inc"
#include      "kincod_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        CEP(*), ITRI1(*), ITRI2(*), INDEX1(*),INDEX2(*),
     .        NUM(*), NELEM,IDDLEVEL, NELEMINT, IXINT(6,*),
     .        KXX(NIXX,NUMELX),IXX(*), ADSKY(0:*),IGEO(NPROPGI,NUMGEO),
     .        ISOLNOD(*), IWCONT(5,*), IWCIN2(2,*), DSDOF(*),
     .        ISOLOFF(*), ISHEOFF(*), ITRIOFF(*), IKINE(*),
     .        ITRUOFF(*), IPOUOFF(*), IRESOFF(*), IELEM21(*),
     .        IPM(NPROPMI,NUMMAT),IXS10(6,*),KXIG3D(NIXIG3D,NUMELIG3D),
     .        IQUAOFF(*),
     .        IXIG3D(*),NSNT, NMNT,TABMP_L,
     .        FVMAIN(NVOLU)
      INTEGER :: ITAB(*)
      INTEGER, DIMENSION(LIPART1,*), INTENT(IN) :: IPART
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC,IPARTG,IPARTS
      TYPE (CLUSTER_) ,DIMENSION(*) :: CLUSTERS
      my_real GEO(NPROPG,NUMGEO),  PM(NPROPM,NUMMAT), X(3,*), COST_R2R,BUFMAT(*)
      REAL    WD(*)
      INTEGER TAILLE
      INTEGER, DIMENSION(NUMMAT_OLD) :: POIN_UMP_OLD
      INTEGER, DIMENSION(7,TAILLE_OLD) :: TAB_UMP_OLD
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD
      INTEGER, DIMENSION(2,NPART), INTENT(IN) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(IN) :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(IN) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(IN) :: MID_PID_SOL
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      INTEGER,INTENT(IN) :: EBCS_TAG_CELL_SPMD(NUMELQ+NUMELTG+NUMELS)
      INTEGER, DIMENSION(NNPBY,*), INTENT(in) :: NPBY
      INTEGER, DIMENSION(*), INTENT(in) :: LPBY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NCRITMAX
      PARAMETER (NCRITMAX = 20)
      INTEGER NSEG, I, J, UTIL, K, NUSE, ELEMD_OLD,
     .        LCNE,IO_ERR1,ISH1,ISH2,II, NNC, IT,
     .        NEDGES, ELK, OFF,CC1, CC2, NUMG1, NUMG2,
     .        INED,L,M,N,NEWEDGE,NEDGES_OLD,
     .        LENWORK,NOD1, NOD2, MODE, NELEM0, MM,
     .        WORK(70000), NUML, IERROR,
     .        ELEMD, IMMNUL, NEDDEL, ITYPINT, IWARN1,
     .        MAXI, MAXJ, MAX, I1, I2, I3, N1, N2, NUMG3, NUMG4,
     .        NELX,ADDX,MID,PID,JALE,MLN,NSHIFT,NNODE, NN,
     .        OPTIONS(40),NCOND,NFLAG,IWFLG,NODC,ICUR,IERR1,NEC,
     .        INWDCOUNT,ICCAND,ICNOD_SMS,ISOLBAR, ICKIN, NK, NKI,
     .        ICELEM, ICINTS, ICINTM, ICINT2, ICDDL, ICFSI, ICDEL, ICSOL,
     .        IWKIN(NUMNOD),ICR2R,NUMEL_R2R, CEPCLUSTER,
     .        NCONNX, CURR, PREV, NEXT, I1OLD, I2OLD, INC, IDB_METIS,
     .        NELIG3D,NCOND2,LSMS,  
     .        OFFC,OFFTG,K0,ITYP,                     
     .        NN_L,IS,IAD,ITY,KAD,JALE_FROM_MAT, JALE_FROM_PROP
      INTEGER, DIMENSION(:),ALLOCATABLE :: XADJ, ADJNCY,IWD,IWD2, 
     .        IENDT,ITRI,INDEX,DOMCLUSTER,ELEMCLUST,
     .        XADJ_OLD, ADJNCY_OLD, COLORS, ROOTS,
     .        POINTER_NEIB,CONNECT_WEIGHT,TAGELEM,CNE,
     .        IWD_COPY
      INTEGER TAILLE_LOCAL,PREV_NEIGH,C_NEIGH,POINT_DELETE,
     .        ELEMNODES(20),OFFELEM(10),WGHT 
      REAL, DIMENSION(:),ALLOCATABLE :: RWD,WD_COPY
      CHARACTER FILNAM*109, KEYA*80, CHLEVEL*1
      REAL    FAC, UBVEC(15), SCAL
      DOUBLE PRECISION 
     .        AVERAGE(NCRITMAX), DEVIATION(NCRITMAX), DMIN(NCRITMAX), DMAX(NCRITMAX),
     .        W(NSPMD), WIS(NSPMD),WIM(NSPMD),WI2(NSPMD), WDDL(NSPMD), 
     .        WFSI(NSPMD), WCAND(NSPMD), WSOL(NSPMD), WKIN(NSPMD),
     .        WDEL(NSPMD), WR2R(NSPMD), WNOD_SMS(NSPMD)
      DOUBLE PRECISION :: WS, WD_MAX,WD_MAX0
C metis5 null pointers

      INTEGER METIS_PartGraphKway, METIS_PartGraphRecursive,
     .        METIS_SetDefaultOptions,Wrap_METIS_PartGraphKway,
     .        WRAP_METIS_PARTGRAPHRECURSIVE
      INTEGER NNO,NNS,NTG,NNI,NTGT,NTGI
      INTEGER NELMIN
      INTEGER NFVMBAG,NB_FVMBAG_TRIM,DD_FVMBAG_TRY
      INTEGER FVM_ELEM(NVOLU),AVG,MAX_TRY
      INTEGER WD_MAX_FACTOR
      INTEGER NB_ELEM_ALE,MAIN_TARGET
      CHARACTER (LEN=255) :: STR
      LOGICAL :: FVM_DOMDEC,DD_UNBALANCED
      LOGICAL, DIMENSION(:), ALLOCATABLE :: TAGGED_ELEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISORT,INDEX_SORT

      INTEGER (kind=8) :: NEDGES_8
      INTEGER :: CLUSTER_TYP,OFFSET_CLUSTER
      my_real, DIMENSION(:,:), ALLOCATABLE :: COORDS
      my_real, DIMENSION(:), ALLOCATABLE :: MIN_DIST
      my_real :: DIST
      my_real :: XMIN(3),XMAX(3)
      INTEGER :: CEP_MIN
      INTEGER :: C1,C2
      INTEGER :: OFFSET


C ---- statistics for edges added for contact interface
        INTEGER :: number_of_added_edges
        INTEGER :: refused_cep0, refused_numg,refused_numg0
        INTEGER :: switch_tried, switch_done 

        integer, pointer :: null_int(:)
        real, pointer :: null_real(:)
        integer :: int_bidon
        real :: real_bidon

        INTEGER :: IJK
        INTEGER :: NSN
        INTEGER :: NUMBER_OF_ELEMENT_RBODY,NUMEL
        INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_ELEMENT_RBODY
        LOGICAL :: BOOL_RBODY
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL METIS_PartGraphKway, METIS_PartGraphRecursive,
     .         METIS_SetDefaultOptions,Wrap_METIS_PartGraphKway,
     .         WRAP_METIS_PARTGRAPHRECURSIVE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      number_of_added_edges = 0
      refused_numg = 0
      refused_numg0 = 0
               
      refused_cep0 = 0
      switch_tried = 0
      switch_done = 0
               
      NEC=0
      NFVMBAG = 0  
      FVMAIN(1:NVOLU) = -1
      FVM_ELEM(1:NVOLU) = 0
      FVM_DOMDEC = .FALSE.
      WD_MAX = 0.0D0
      WD_MAX0= 0.0D0
      NNODE = NSPMD

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
 
C add Tetra10
      IF(NUMELS10.GT.0) THEN
        DO J=1,NUMELS10
          DO K=1,6
            N = IXS10(K,J) + 1
            ADSKY(N) = ADSKY(N) + 1
          ENDDO
        ENDDO
      ENDIF
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
Cel traitement a part du 3eme noeud optionnel sauf type 12
      DO K=2,3
        DO I=1,NUMELR
          N = IXR(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      DO I=1,NUMELR
        N = IXR(4,I) + 1
        IF(NINT(GEO(12,IXR(1,I))).EQ.12) THEN
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
      DO K=2,5
        DO I=1,NUMELUR
          N = IXUR(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
C Elements Multibrins
      DO I=1,NUMELX
         NELX=KXX(3,I) 
         DO K=1,NELX
           ADDX = KXX(4,I)+K-1
           N=IXX(ADDX)+1
           ADSKY(N)= ADSKY(N)+1
         ENDDO
      ENDDO
C
C Elements Iso-geo
      DO I=1,NUMELIG3D
         NELIG3D=KXIG3D(3,I) 
         DO K=1,NELIG3D
           ADDX = KXIG3D(4,I)+K-1
           N=IXIG3D(ADDX)+1
           ADSKY(N)= ADSKY(N)+1
         ENDDO
      ENDDO
C
      ADSKY(1) = 1
      DO I=2,NUMNOD+1
        ADSKY(I) = ADSKY(I) + ADSKY(I-1)
      END DO
C 
      LCNE = ADSKY(NUMNOD+1)
      ALLOCATE(CNE(LCNE),STAT=IERR1)
C
      IF(IERR1.NE.0)THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
      END IF
C
C-----------------------------------------------
C Optimisation sur elements deletes du _0001.rad
C-----------------------------------------------
C poids sous forme reel pour compatibilite RSB ancienne
      DO I = 1, NELEM
        WD(I) = 0.
      ENDDO
      ELEMD = 0
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'
      OPEN(UNIT=71,FILE=FILNAM(1:ROOTLEN+9),
     .   ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
C
      IF (IO_ERR1.NE.0) THEN
         FILNAM=ROOTNAM(1:ROOTLEN)//'D01'
         OPEN(UNIT=71,FILE=FILNAM(1:ROOTLEN+3),
     .      ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
      ENDIF
C
      IF (IO_ERR1.EQ.0) THEN
         OPEN(UNIT=72,FORM='FORMATTED',STATUS='SCRATCH')
         ELEMD = 0
 10      READ(71,'(A)',END=20) KEYA
 11      CONTINUE
           IF(KEYA(1:12).EQ.'/DEL/SHELL/1') THEN
 30          READ(71,'(A)',END=20) KEYA
             IF(KEYA(1:1).EQ.'#')GOTO 30
             IF(KEYA(1:1).EQ.'$')GOTO 30
               IF(KEYA(1:1).EQ.'/')GOTO 11
Cel ko sur cray               READ(KEYA,*,END=20)ISH1,ISH2
             REWIND(72)
             WRITE(72,'(A)')KEYA
             REWIND(72)
             READ(72,*,END=20)ISH1,ISH2
             DO I = 1, NUMELC
               IF(IXC(NIXC,I).GE.ISH1.AND.IXC(NIXC,I).LE.ISH2) THEN
                 DO J = ISH1, ISH2
                   IF(IXC(NIXC,I).EQ.J) THEN
                     WD(I+NUMELS+NUMELQ) = 0.0001
                     ELEMD = ELEMD + 1
                     GOTO 35
                   ENDIF
                 ENDDO
               ENDIF
 35            CONTINUE
             ENDDO
             GOTO 30
           ELSEIF(KEYA(1:12).EQ.'/DEL/BRICK/1') THEN
 60          READ(71,'(A)',END=20) KEYA
             IF(KEYA(1:1).EQ.'#')GOTO 60
             IF(KEYA(1:1).EQ.'$')GOTO 60
             IF(KEYA(1:1).EQ.'/')GOTO 11
Cel ko sur cray               READ(KEYA,*,END=20)ISH1,ISH2
             REWIND(72)
             WRITE(72,'(A)')KEYA
             REWIND(72)
             READ(72,*,END=20)ISH1,ISH2
             DO I = 1, NUMELS
               IF(IXS(NIXS,I).GE.ISH1.AND.IXS(NIXS,I).LE.ISH2) THEN
                 DO J = ISH1, ISH2
                   IF(IXS(NIXS,I).EQ.J) THEN
                     WD(I) = 0.0001
                     ELEMD = ELEMD + 1
                     GOTO 65
                   ENDIF
                 ENDDO
               ENDIF
 65            CONTINUE
             ENDDO
             GOTO 60
C
           ELSEIF(KEYA(1:12).EQ.'/DEL/SH_3N/1') THEN
 90          READ(71,'(A)',END=20) KEYA
             IF(KEYA(1:1).EQ.'#')GOTO 90
             IF(KEYA(1:1).EQ.'$')GOTO 90
             IF(KEYA(1:1).EQ.'/')GOTO 11
Cel ko sur cray               READ(KEYA,*,END=20)ISH1,ISH2
             REWIND(72)
             WRITE(72,'(A)')KEYA
             REWIND(72)
             READ(72,*,END=20)ISH1,ISH2
             DO I = 1, NUMELTG
               IF(IXTG(NIXTG,I).GE.ISH1
     .            .AND.IXTG(NIXTG,I).LE.ISH2) THEN
                 DO J = ISH1, ISH2
                   IF(IXTG(NIXTG,I).EQ.J) THEN
                     WD(I+NUMELS+NUMELQ+NUMELC+NUMELT
     .                   +NUMELP+NUMELR) = 0.0001
                     ELEMD = ELEMD + 1
                     GOTO 95
                   ENDIF
                 ENDDO
               ENDIF
 95            CONTINUE
             ENDDO
             GOTO 90
           ENDIF
           GOTO 10
 20      CONTINUE
         CLOSE(71)
         CLOSE(72)
C msg sur D01 lu (delete optimise)
         IF(IDDLEVEL.EQ.0) THEN
            WRITE(IOUT,*)' '
            WRITE(IOUT,'(A)')
     .   ' SPMD IS CHECKING FOR ELEMENT DELETION IN : ',' '//FILNAM
         ENDIF
C
      ELSE
C msg sur D01 non lu (delete non optimise)
         IF(IDDLEVEL.EQ.0) THEN
            WRITE(IOUT,*)' '
            WRITE(IOUT,'(A)')
     .   ' SPMD IS NOT ABLE TO CHECK FOR ELEMENT DELETION IN'//
     .   ' RADIOSS ENGINE INPUT FILE'
         ENDIF
      ENDIF

C-----------------------------------------------
C Optimisation sur RBYON du _0000.rad
C-----------------------------------------------
      ELEMD_OLD = ELEMD
      ISOLBAR=0
      DO II = 1, NUMELS
        IF((ISOLOFF(II).EQ.1.OR.ISOLOFF(II).EQ.3).AND.
     *      WD(II).NE.0.0001)THEN
          WD(II) = 0.0001
          ELEMD = ELEMD + 1
        END IF
C additional test for barrier
        MID = ABS(IXS(1,II))
        PID = ABS(IXS(10,II))
        JALE_FROM_MAT = NINT(PM(72,MID))
        JALE_FROM_PROP = IGEO(62,PID)
        JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if unconsistent, error message was displayed in PART reader
        MLN = NINT(PM(19,MID))
        IF(JALE.EQ.0.AND.(MLN.EQ.28.OR.MLN.EQ.68))THEN
          ISOLBAR=ISOLBAR+1
        ENDIF
      END DO
C
      DO II = 1, NUMELQ
        IF((IQUAOFF(II).EQ.1.OR.IQUAOFF(II).EQ.3).AND.
     *      WD(II+NUMELS).NE.0.0001)THEN
          WD(II+NUMELS) = 0.0001
          ELEMD = ELEMD + 1
        END IF
      END DO
C
      DO II = 1, NUMELC
        IF((ISHEOFF(II).EQ.1.OR.ISHEOFF(II).EQ.3).AND.
     *      WD(II+NUMELS+NUMELQ).NE.0.0001)THEN
          WD(II+NUMELS+NUMELQ) = 0.0001
          ELEMD = ELEMD + 1
        END IF
      END DO
C
      DO II = 1, NUMELT
        IF((ITRUOFF(II).EQ.3 ).AND.
     *      WD(II+NUMELS+NUMELQ+NUMELC).NE.0.0001 )THEN
          WD(II+NUMELS+NUMELQ+NUMELC) = 0.0001
          ELEMD = ELEMD + 1
        END IF
      END DO
C
      DO II = 1, NUMELP
        IF((IPOUOFF(II).EQ.3 ).AND.
     *      WD(II+NUMELS+NUMELQ+NUMELC+NUMELT).NE.0.0001 )THEN
          WD(II+NUMELS+NUMELQ+NUMELC+NUMELT) = 0.0001
          ELEMD = ELEMD + 1
        END IF
      END DO
C
      DO II = 1, NUMELR
        IF((IRESOFF(II).EQ.3 ).AND.
     *      WD(II+NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP).NE.0.0001 )THEN
          WD(II+NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP) = 0.0001
          ELEMD = ELEMD + 1
        END IF
      END DO
C
      DO II = 1, NUMELTG
        IF(ITRIOFF(II).EQ.1.AND.WD(II+NUMELS+NUMELQ+NUMELC+NUMELT
     .                               +NUMELP+NUMELR).NE.0.0001)THEN
          WD(II+NUMELS+NUMELQ+NUMELC+NUMELT
     .         +NUMELP+NUMELR) = 0.0001
          ELEMD = ELEMD + 1
        END IF
      END DO
C
Cel test pour by-passer la creation du niveau "elem deletes" et eviter cas de plantage si elemd=1
C
      IF (NELEM .GT. 0) THEN
       IF(FLOAT(NELEM-ELEMD)/FLOAT(NELEM).GT.ZEP95) ELEMD = 0
      END IF
      IF(IDDLEVEL.EQ.0.AND.ELEMD.GT.ELEMD_OLD) THEN
            WRITE(IOUT,*)' '
            WRITE(IOUT,'(A)')
     .   ' DOMAIN DECOMPOSITION OPTIMIZED FOR ELEMENT DEACTIVATION'//
     .   ' IN /RBODY OPTIONS'
      ENDIF
C
C-----------------------------------------------
      IF (IDDLEVEL.EQ.1) THEN        
        WRITE(IOUT,'(A)')' '
        WRITE(IOUT,'(A)')
     .  ' --------------------------------------'
        WRITE(IOUT,'(A)')
     .  ' NEW DOMAIN DECOMPOSITION FOR OPTIMIZATION'
        WRITE(IOUT,'(A)')
     .  ' --------------------------------------'
      ENDIF
      WRITE(ISTDO,'(A)')' .. DOMAIN DECOMPOSITION'
      WRITE(IOUT,'(A)')' '
      IF(DECTYP.EQ.3)THEN
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION USING MULTILEVEL KWAY'
      ELSEIF(DECTYP.EQ.4)THEN
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION USING MULTILEVEL RSB'
      ELSEIF(DECTYP.EQ.5)THEN
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION USING MULTILEVEL KWAY FOR IMPLICIT AND AMS'
      ELSEIF(DECTYP.EQ.4)THEN
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION USING MULTILEVEL RSB FOR IMPLICIT'
      END IF
      WRITE(IOUT,'(A)')
     .  ' ------------------------------------------'
      IF (IPARI0.EQ.1) THEN
          WRITE(IOUT,'(A)')
     .    ' DOMAIN DECOMPOSITION OPTIMIZED FOR PARALLEL ARITHMETIC ON'
      ELSE
          WRITE(IOUT,'(A)')
     .    ' DOMAIN DECOMPOSITION OPTIMIZED FOR PARALLEL ARITHMETIC OFF'
      ENDIF

      IF(IDDLEVEL == 1 .AND. DDNOD_SMS /= 0)THEN
        WRITE(IOUT,'(A)')
     .  ' ADDITIONAL OPTIMIZATION OF DOMAIN DECOMPOSITION FOR AMS (DOMDEC=7)'
      END IF
C-----------------------------------------------
C   CALCUL DE CNE
C-----------------------------------------------
C We have to tag elements for know theirs number of nodes
      ALLOCATE(TAGELEM(NELEM))
      DO I = 1,NELEM
        TAGELEM(I)=0
      END DO 
      DO I=1,NUMELS
        TAGELEM(I)=1
        DO K=1,8
          N = IXS(K+1,I)
          IF(N /= 0) THEN
            CNE(ADSKY(N)) = I
            ADSKY(N) = ADSKY(N) + 1
          END IF
        ENDDO 
      ENDDO
C add Tetra10
      IF(NUMELS10.GT.0) THEN
        DO J=1,NUMELS10
          TAGELEM(ABS(-(NUMELS8+J)))=2
          DO K=1,6
            N = IXS10(K,J)
            IF(N /= 0) THEN
              CNE(ADSKY(N)) = -(NUMELS8+J) ! to treat extra node only for contacts
              ADSKY(N) = ADSKY(N) + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C-----------------------------------------------
C
      OFFELEM(1)=NUMELS
      OFF = NUMELS
C
      DO I = 1, NUMELQ
        TAGELEM(I+OFF)=3
        DO K=1,4
          N = IXQ(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFFELEM(2)=NUMELQ
      OFF = OFF + NUMELQ
C
      DO I = 1, NUMELC
        TAGELEM(I+OFF)=4
        DO K=1,4
          N = IXC(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C     

      OFFELEM(3)=NUMELC
      OFF = OFF + NUMELC
C
      DO I = 1, NUMELT
        TAGELEM(I+OFF)=5
        DO K=1,2
          N = IXT(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
       ENDDO
C
       OFFELEM(4)= NUMELT
       OFF = OFF + NUMELT
C
      DO I = 1, NUMELP
        TAGELEM(I+OFF)=6
        DO K=1,2
          N = IXP(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFFELEM(5) = NUMELP
      OFF = OFF + NUMELP 
C     
      DO I = 1, NUMELR
        TAGELEM(I+OFF)=7
        DO K=1,2
          N = IXR(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
        IF(NINT(GEO(12,IXR(1,I))).EQ.12) THEN
          N = IXR(4,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDIF
      ENDDO
C
      OFFELEM(6)=NUMELR
      OFF = OFF + NUMELR
C
      DO I = 1, NUMELTG
        TAGELEM(I+OFF)=8  
        DO K=1,3
          N = IXTG(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFFELEM(7)=NUMELTG
      OFF = OFF + NUMELTG
C
      DO I = 1, NUMELUR
        TAGELEM(I+OFF)=9
        DO K=1,4
          N = IXUR(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFFELEM(8)=NUMELUR
      OFF = OFF + NUMELUR
C
      DO I=1, NUMELX
        TAGELEM(I+OFF)=10
        NELX=KXX(3,I)
        DO K=1,NELX
          ADDX = KXX(4,I)+K-1
          N=IXX(ADDX)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFFELEM(9)=NUMELX
      OFF = OFF + NUMELX
C
      DO I=1, NUMELIG3D
        TAGELEM(I+OFF)=11
        NELIG3D=KXIG3D(3,I)
        DO K=1,NELIG3D
          ADDX = KXIG3D(4,I)+K-1
          N=IXIG3D(ADDX)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      OFFELEM(10)=NUMELIG3D
      OFF = OFF + NUMELIG3D
C
C     remise au debut des adresses
      DO I=NUMNOD+1,2,-1
        ADSKY(I) = ADSKY(I-1)
      END DO

      ADSKY(1) = 1
Calcul des poids en prennant en compte le ratio connectivite

      ICELEM=1
      ICINTS=0
      ICINTM=0
      ICINT2=0
      ICCAND=0
      ICNOD_SMS=0
      ICDDL=0
      ICFSI=0
      ICSOL=0
      ICDEL=0
      ICR2R=0
      ICKIN=0
      NCOND=1
C
      DO I = 1, NELEMINT
        ITYPINT=ABS(IXINT(6,I))
        IF(ITYPINT == 2)THEN
          ICINT2 = ICINT2+1
        ELSEIF(ITYPINT == 7 .OR. ITYPINT == 11)THEN
          ICINTS = ICINTS+1
          ICINTM = ICINTM+1
          ICCAND = ICCAND+1
        ELSEIF(ITYPINT == 24 .OR. ITYPINT == 25)THEN
          ICINTS = ICINTS+1
          ICINTM = ICINTM+1
          ICCAND = ICCAND+1
        END IF
      END DO
C 
      IF(DDNOD_SMS/=0)THEN
        NCOND=NCOND+1
        ICNOD_SMS=NCOND
      ELSE
        ICNOD_SMS=0
      END IF
C 
      IF((ICINTS+ICINTM>100) .AND.
     +   (NELEM < ICINTS+ICINTM .OR.
     +    FLOAT(NELEM-ICINTS-ICINTM)/FLOAT(NELEM).LE.ZEP95)) THEN
        NCOND=NCOND+1
        ICINTS=NCOND
        NCOND=NCOND+1
        ICINTM=NCOND
      ELSE
        IF(NSNT+NMNT>100) THEN
         NCOND=NCOND+1
         ICINTS=NCOND
         NCOND=NCOND+1
         ICINTM=NCOND
        ELSE         
         ICINTS=0           
         ICINTM=0           
        ENDIF
      END IF
      IF((ICINT2>100) .AND.
     +   (NELEM < ICINT2 .OR.
     +    FLOAT(NELEM-ICINT2)/FLOAT(NELEM).LE.ZEP98)) THEN
        NCOND=NCOND+1
        ICINT2=NCOND
      ELSE
        ICINT2=0
      END IF
C test bypass contact for small test cases
      IF((ICCAND>100) .AND.
     +   (NELEM < ICCAND .OR.
     +    FLOAT(NELEM-ICCAND)/FLOAT(NELEM).LE.ZEP95)) THEN
        NCOND=NCOND+1
        ICCAND=NCOND
      ELSE
        ICCAND=0
      END IF
C
      NK=0
c      IF(NK > 0) THEN ! Test To bypass load-balancing of Kin.Cond. temporarily
      IF(ELEMD == 0) THEN ! Test To bypass load-balancing of Kin.Cond. if element deletion already active (large RB)
        DO I = 1, NUMNOD
c          NKI=IWL(IKINE(I))+2*IRB(IKINE(I))+2*IRB2(IKINE(I))
c     +       +2*IRBM(IKINE(I))+2*IRLK(IKINE(I))+2*IJO(IKINE(I))
c     +       +2*IKRBE2(IKINE(I))+2*IKRBE3(IKINE(I))
c          IWKIN(I)=NKI
c          NK = NK+NKI
          NKI=IWL(IKINE(I))+IRB(IKINE(I))+IRB2(IKINE(I))
     +       +IRBM(IKINE(I))+IRLK(IKINE(I))+IJO(IKINE(I))
     +       +IKRBE2(IKINE(I))+IKRBE3(IKINE(I))
          IWKIN(I)=NKI
          NK = NK+MIN(NKI,1)
        END DO
c        print *,'n cond=',NK,FLOAT(NUMNOD-NK)/FLOAT(NUMNOD)
        IF(FLOAT(NUMNOD-NK)/FLOAT(NUMNOD).GT.ZEP95) NK = 0   
        IF(NK > 20000) THEN ! needs a sufficient number of kin.cond.
          NCOND = NCOND+1
          ICKIN = NCOND
        END IF
      END IF
C      
      IF(DECTYP.EQ.5.OR.DECTYP.EQ.6)THEN
C permutation poids element <=> ddl
        NCOND = NCOND+1
        ICDDL=1
        ICELEM=NCOND
        IF(ELEMD.GT.0) THEN
          NCOND = NCOND+1
          ICDEL = NCOND
        END IF

      ELSE
        IF(ILAG.EQ.1.AND.(IALE.EQ.1.OR.IEULER.EQ.1))THEN
C if FSI 
          NCOND = NCOND+1
          NB_ELEM_ALE = 0
          DO I = 1, NUMELS
            MID = ABS(IXS(1,I))
            JALE = NINT(PM(72,MID))
            MLN = NINT(PM(19,MID))
            IF(JALE.EQ.0.AND.MLN.NE.18)THEN

            ELSE
              NB_ELEM_ALE = NB_ELEM_ALE + 1
            END IF
          ENDDO

          IF (NELEM - NB_ELEM_ALE < 128 * NSPMD) THEN 
C Priority is FSI
            ICFSI = 1 
            ICELEM = NCOND
              WRITE(IOUT,'(A)')
     .     ' DOMAIN DECOMPOSITION OPTIMIZED FOR ALE (1)'
          ELSEIF( NB_ELEM_ALE*2 > NELEM ) THEN
C Priority is FSI then ELEM
            ICFSI = 1 
            ICELEM = 2
            IF(ICDDL/=0)  ICDDL  = ICDDL  + 1 
            IF(ICINTS/=0) ICINTS = ICINTS + 1
            IF(ICINTM/=0) ICINTM = ICINTM + 1
            IF(ICINT2/=0) ICINT2 = ICINT2 + 1
            IF(ICKIN/=0)  ICKIN  = ICKIN  + 1
            IF(ICNOD_SMS/=0) ICNOD_SMS = ICNOD_SMS +1 
            IF(ICDEL/=0)  ICDEL = ICDEL + 1                    
            IF(ICCAND/=0) ICCAND = ICCAND + 1 
              WRITE(IOUT,'(A)')
     .     ' DOMAIN DECOMPOSITION OPTIMIZED FOR ALE (2)'
          ELSEIF  ( NB_ELEM_ALE*4 > NELEM) THEN
C Priority is  ELEM then FSI
            ICFSI = 2 
            ICELEM = 1
            IF(ICDDL/=0)  ICDDL  = ICDDL  + 1 
            IF(ICINTS/=0) ICINTS = ICINTS + 1
            IF(ICINTM/=0) ICINTM = ICINTM + 1
            IF(ICINT2/=0) ICINT2 = ICINT2 + 1
            IF(ICKIN/=0)  ICKIN  = ICKIN  + 1
            IF(ICNOD_SMS/=0) ICNOD_SMS = ICNOD_SMS +1 
            IF(ICDEL/=0)  ICDEL = ICDEL + 1                    
            IF(ICCAND/=0) ICCAND = ICCAND + 1 
              WRITE(IOUT,'(A)')
     .     ' DOMAIN DECOMPOSITION OPTIMIZED FOR FSI (3)'
          ELSE
          ICFSI = NCOND
        END IF
        END IF
        IF(ISOLBAR > 10000 .AND. ICFSI == 0 .AND. NUMELC > NUMELS)THEN
C if more than 10K solid law28/LAW68, decompose solid like there is a barrier
C          IF(IDDLEVEL.EQ.1.) THEN
              WRITE(IOUT,'(A)')
     .     ' DOMAIN DECOMPOSITION OPTIMIZED FOR BARRIER '
C          ENDIF
          NCOND = NCOND+1
          ICSOL=NCOND
        END IF
        IF(ELEMD.GT.0) THEN
          NCOND = NCOND+1
          ICDEL = NCOND
        END IF
      END IF        
      IF(NSUBDOM.GT.0)THEN
        NUMEL_R2R = 0
        DO I = 1, NUMELS
          IF (TAG_ELSF(I) /= 0)  NUMEL_R2R =  NUMEL_R2R+1
        END DO
        DO I = 1, NUMELC
          IF (TAG_ELCF(I) /= 0)  NUMEL_R2R =  NUMEL_R2R+1
        END DO
        IF (NUMEL_R2R.GE.NSPMD) THEN         
          WRITE(IOUT,'(A)')
     .     ' DOMAIN DECOMPOSITION OPTIMIZED FOR MULTIDOMAINS '
          NCOND = NCOND+1
          ICR2R=NCOND
        ENDIF  
      END IF     
C
      ALLOCATE(RWD(NELEM*NCOND),STAT=IERR1)
C poids Metis suivant 
      DO I = 1, NCOND*NELEM
        RWD(I) = 0
      ENDDO
Cel optimisation des poids par defaut
      CALL INITWG(WD,PM,GEO,IXS,IXQ,
     .            IXC,IXT,IXP,IXR,IXTG,
     .            IXUR,KXX,IGEO,ISOLNOD,IARCH,
     .             NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,
     .             NUMELR,NUMELTG,NUMELUR,NUMELX,IPM,
     .             BUFMAT,NUMMAT,NUMGEO,TAILLE,POIN_UMP,
     .             TAB_UMP,POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     .             TABMP_L,IPART,IPARTC,IPARTG,
     .             IPARTS,NPART,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     .             MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,IDDLEVEL)
C
      IF(NSUBDOM.GT.0)THEN
        COST_R2R = ZERO
        DO I=1,NELEM
          SCAL = ONE
          IF (I.LE.NUMELS) THEN
              JALE = NINT(PM(72,MID))
              MLN = NINT(PM(19,MID))
              IF (JALE.NE.0) SCAL = 2.5
              IF (MLN.EQ.51) SCAL = 4.5
          ENDIF
          COST_R2R = COST_R2R + WD(I)
        END DO
      ENDIF
C
      DO I=1,NUMELS
       NNC=0
       IF ((ICR2R /= 0)) THEN
         IF((TAG_ELSF(I) /= 0))THEN
           RWD(NCOND*(I-1)+ICR2R) = 1     
         ENDIF
       ENDIF
       IF(ICSOL /= 0) RWD(NCOND*(I-1)+ICSOL) = 1
       IF(ISOLNOD(I)==4.OR.ISOLNOD(I)==10)THEN
         DO K=1,8
           N = IXS(K+1,I)
           IF(N.NE.0)THEN
             FAC=ONE/(ADSKY(N+1)-ADSKY(N))
             NNC = NNC+ADSKY(N+1)-ADSKY(N)
             IF(ICDDL/=0)RWD(NCOND*(I-1)+ICDDL)=RWD(NCOND*(I-1)+ICDDL)
     +                                         +DSDOF(N)*FAC
             IF(ICINTS/=0)
     +         RWD(NCOND*(I-1)+ICINTS)=RWD(NCOND*(I-1)+ICINTS)
     +                                +IWCONT(1,N)*FAC
             IF(ICINTM/=0)
     +         RWD(NCOND*(I-1)+ICINTM) = RWD(NCOND*(I-1)+ICINTM)
     +                                 + IWCONT(2,N)*FAC
             IF(ICINT2/=0)
     +         RWD(NCOND*(I-1)+ICINT2)=RWD(NCOND*(I-1)+ICINT2)
     +                                +(IWCIN2(1,N)+IWCIN2(2,N))*FAC
             IF(ICKIN/=0)RWD(NCOND*(I-1)+ICKIN)=RWD(NCOND*(I-1)+ICKIN)
     +                                         +IWKIN(N)*FAC             
             IF(ICNOD_SMS/=0)RWD(NCOND*(I-1)+ICNOD_SMS)=RWD(NCOND*(I-1)+ICNOD_SMS)
     +                                                   +MIN(DSDOF(N),1)*FAC
           END IF
         END DO
         IF(ISOLNOD(I)==10)THEN
           II = I-NUMELS8
           DO K=1,6
             N = IXS10(K,II)
             IF(N.NE.0)THEN
C take care of non connected node
               FAC=ONE/MAX(ADSKY(N+1)-ADSKY(N),1)
               NNC = NNC+ADSKY(N+1)-ADSKY(N)
               IF(ICDDL/=0)RWD(NCOND*(I-1)+ICDDL)=RWD(NCOND*(I-1)+ICDDL)
     +                                           +DSDOF(N)*FAC
               IF(ICINTS/=0)
     +           RWD(NCOND*(I-1)+ICINTS)=RWD(NCOND*(I-1)+ICINTS)
     +                                  +IWCONT(1,N)*FAC
               IF(ICINTM/=0)
     +           RWD(NCOND*(I-1)+ICINTM) = RWD(NCOND*(I-1)+ICINTM)
     +                                   + IWCONT(2,N)*FAC
               IF(ICINT2/=0)
     +           RWD(NCOND*(I-1)+ICINT2)=RWD(NCOND*(I-1)+ICINT2)
     +                                  +(IWCIN2(1,N)+IWCIN2(2,N))*FAC
               IF(ICKIN/=0)RWD(NCOND*(I-1)+ICKIN)=RWD(NCOND*(I-1)+ICKIN)
     +                                           +IWKIN(N)*FAC             
               IF(ICNOD_SMS/=0)RWD(NCOND*(I-1)+ICNOD_SMS)=RWD(NCOND*(I-1)+ICNOD_SMS)
     +                                                     +MIN(DSDOF(N),1)*FAC
             ENDIF
           ENDDO
cc           IF(ICNOD /= 0) RWD(NCOND*(I-1)+ICNOD) = ONE ! a definir si besoin
         ELSE
cc           IF(ICNOD /= 0) RWD(NCOND*(I-1)+ICNOD) = ONE/FIVE ! a definir si besoin
         ENDIF
       ELSE
        DO K=1,8
          N = IXS(K+1,I)
          IF(N.NE.0)THEN
C take care of non connected node
            FAC=ONE/MAX(ADSKY(N+1)-ADSKY(N),1)
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I-1)+ICDDL)=RWD(NCOND*(I-1)+ICDDL)
     +                                        +DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I-1)+ICINTS)=RWD(NCOND*(I-1)+ICINTS)
     +                               +IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I-1)+ICINTM) = RWD(NCOND*(I-1)+ICINTM)
     +                                + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I-1)+ICINT2)=RWD(NCOND*(I-1)+ICINT2)
     +                               +(IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I-1)+ICKIN)=RWD(NCOND*(I-1)+ICKIN)
     +                                        +IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I-1)+ICNOD_SMS)=RWD(NCOND*(I-1)+ICNOD_SMS)
     +                                                  +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0) RWD(NCOND*(I-1)+ICNOD) = (EIGHT*EIGHT)/NNC
       ENDIF
      ENDDO
C
C-----------------------------------------------
C
      OFF = NUMELS
Cel poids interface = 0 en 2D
C
      OFF = OFF + NUMELQ
C
      DO I = 1, NUMELC
        NNC=0
        IF (ICR2R /= 0) THEN
          IF (TAG_ELCF(I) /= 0) THEN
            RWD(NCOND*(I+OFF-1)+ICR2R) = 1     
          ENDIF 
        ENDIF
        DO K=1,4
          N = IXC(K+1,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) =
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0) RWD(NCOND*(I+OFF-1)+ICNOD)=(FOUR*FOUR)/NNC
      ENDDO
C
      OFF = OFF + NUMELC
C
      DO I = 1, NUMELT
        NNC=0
        DO K=1,2
          N = IXT(K+1,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL)  = 
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0) RWD(NCOND*(I+OFF-1)+ICNOD) = (TWO*TWO)/NNC
      ENDDO
C
      OFF = OFF + NUMELT
C
      DO I = 1, NUMELP
        NNC=0
        DO K=1,2
          N = IXP(K+1,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) = 
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)= 
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0) RWD(NCOND*(I+OFF-1)+ICNOD) = (TWO*TWO)/NNC
      ENDDO
C
      OFF = OFF + NUMELP 
C
      DO I = 1, NUMELR
        NNC=0
        DO K=1,2
          N = IXR(K+1,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) =
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
        IF(NINT(GEO(12,IXR(1,I))).EQ.12) THEN
          N = IXR(4,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) =
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDIF
cc        IF(ICNOD /= 0) RWD(NCOND*(I+OFF-1)+ICNOD) = (TWO*TWO)/NNC
      ENDDO
C
      OFF = OFF + NUMELR
C
      DO I = 1, NUMELTG
        NNC=0
        DO K=1,3
          N = IXTG(K+1,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) =
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0)
cc     +    RWD(NCOND*(I+OFF-1)+ICNOD) = (ONE/TWO)*(SIX*THREE)/NNC
      ENDDO
C
      OFF = OFF + NUMELTG
C
      DO I = 1, NUMELUR
        NNC=0
        DO K=1,4
          N = IXUR(K+1,I)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) =
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2) 
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0) RWD(NCOND*(I+OFF-1)+ICNOD)=ONE
      ENDDO
C
      OFF = OFF + NUMELUR
C
      DO I=1, NUMELX
        NELX=KXX(3,I)
        NNC=0
        DO K=1,NELX
          ADDX = KXX(4,I)+K-1
          N=IXX(ADDX)
          IF(N.NE.0)THEN
            FAC=ONE/(ADSKY(N+1)-ADSKY(N))
            NNC = NNC+ADSKY(N+1)-ADSKY(N)
            IF(ICDDL/=0)RWD(NCOND*(I+OFF-1)+ICDDL) =
     +                  RWD(NCOND*(I+OFF-1)+ICDDL) + DSDOF(N)*FAC
            IF(ICINTS/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTS) = RWD(NCOND*(I+OFF-1)+ICINTS)
     +                                    + IWCONT(1,N)*FAC
            IF(ICINTM/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINTM) = RWD(NCOND*(I+OFF-1)+ICINTM)
     +                                    + IWCONT(2,N)*FAC
            IF(ICINT2/=0)
     +        RWD(NCOND*(I+OFF-1)+ICINT2) = RWD(NCOND*(I+OFF-1)+ICINT2)
     +                                    + (IWCIN2(1,N)+IWCIN2(2,N))*FAC
            IF(ICKIN/=0)RWD(NCOND*(I+OFF-1)+ICKIN)=
     +                  RWD(NCOND*(I+OFF-1)+ICKIN) + IWKIN(N)*FAC             
            IF(ICNOD_SMS/=0)RWD(NCOND*(I+OFF-1)+ICNOD_SMS)=RWD(NCOND*(I+OFF-1)+ICNOD_SMS)
     +                                                      +MIN(DSDOF(N),1)*FAC
          END IF
        ENDDO
cc        IF(ICNOD /= 0) RWD(NCOND*(I+OFF-1)+ICNOD) = ONE   ! a definir si besoin
      ENDDO
C
      OFF = OFF + NUMELX
      
C normalisation des poids interface & dof

      ALLOCATE(IWD(NELEM*NCOND),STAT=IERR1)

      DO I = 1, NCOND*NELEM
        IWD(I) = 0
      ENDDO
      DO I = 1, NELEM
C plus besoin de normalisation a 1
        IF(ICINTS/=0)
     .    IWD(NCOND*(I-1)+ICINTS) = NINT(RWD(NCOND*(I-1)+ICINTS))
        IF(ICINTM/=0)
     .    IWD(NCOND*(I-1)+ICINTM) = NINT(RWD(NCOND*(I-1)+ICINTM))
        IF(ICCAND/=0)
     .    IWD(NCOND*(I-1)+ICCAND) = NINT(RWD(NCOND*(I-1)+ICCAND))
        IF(ICINT2/=0)
     .    IWD(NCOND*(I-1)+ICINT2) = NINT(RWD(NCOND*(I-1)+ICINT2))
        IF(ICDDL/=0)
     .    IWD(NCOND*(I-1)+ICDDL)= NINT(RWD(NCOND*(I-1)+ICDDL))
        IF(ICSOL/=0)
     .    IWD(NCOND*(I-1)+ICSOL)= NINT(RWD(NCOND*(I-1)+ICSOL))
        IF(ICKIN/=0)
     .    IWD(NCOND*(I-1)+ICKIN)= NINT(RWD(NCOND*(I-1)+ICKIN))
        IF(ICR2R/=0)
     .    IWD(NCOND*(I-1)+ICR2R)= NINT(RWD(NCOND*(I-1)+ICR2R))
c        IF(ICNOD/=0)
c     .    IWD(NCOND*(I-1)+ICNOD)= NINT(RWD(NCOND*(I-1)+ICNOD)*10)
        IF(ICNOD_SMS/=0)
     .    IWD(NCOND*(I-1)+ICNOD_SMS) = NINT(RWD(NCOND*(I-1)+ICNOD_SMS))
      END DO

      DEALLOCATE(RWD)

C.....construction des couples Ei Ej connectes par un point

      NEDGES = 0
      DO N = 1, NUMNOD
        DO CC1 = ADSKY(N), ADSKY(N+1)-1
          NUMG1 = CNE(CC1)
          IF(NUMG1 > 0) THEN ! by-pass extra nodes
            DO CC2 = CC1+1, ADSKY(N+1)-1
              NUMG2 = CNE(CC2)
              IF(NUMG2 > 0 .AND. NUMG1 /= NUMG2) THEN ! by-pass extra nodes
                NEDGES = NEDGES + 1
              END IF
            ENDDO
          END IF
        ENDDO
      ENDDO
C
      IF (IDDLEVEL.EQ.1) NEDGES = NEDGES+NELEMINT
C
C----------------------------------------------
!     SIDDCONNECT minimum size NELEM. Value set to 10*NELEM
      IF(NELEM < 100 000 000) THEN
        SIDDCONNECT = 2*10*NELEM      
      ELSE
        ! For very large model
        ! edge filtering is forced
        SIDDCONNECT = 2 000 000 000  
        EDGE_FILTERING = 1
      ENDIF
!     Linked-list IDDCONNECT      
!     IDDCONNECT%IENTRYDOM : entry in IDDCONNECT for element N
!     IDDCONNECT%PDOM(1,N) : connected element to element N
!     IDDCONNECT%PDOM(2,N) : next index in IDDCONNECT for element N
!     allocation IDDCONNECT % PDOM et % IENTRYDOM
      ALLOCATE(IDDCONNECT%PDOM(2,SIDDCONNECT),STAT=IERR1)
      ALLOCATE(IDDCONNECT%IENTRYDOM(2,NELEM),STAT=IERR1)    
!     initialisation de IDDCONNECT%IENTRYDOM
      CALL INI_IDDCONNECT(NELEM)
C
      NEDGES_OLD = NEDGES

      IF(EDGE_FILTERING == 1 .AND. (NUMELS > NELEM / 3 .OR. ICFSI > 0 )) THEN 
        WRITE(IOUT,'(A)') "** INFO: SIMPLIFIED DOMAIN DECOMPOSITION" 
C+------------------------------------------------------------+
C| Domain decomposition for very big models                   |
C|      dectivated via Domdec = -3 in /SPMD card               |
C+------------------------------------------------------------+
        ALLOCATE(CONNECT_WEIGHT(NELEM))
        ALLOCATE(POINTER_NEIB(NELEM))
        DO I =1,NELEM
           CONNECT_WEIGHT(I)=0
           POINTER_NEIB(I)=0
        ENDDO
        NELMIN = 0
        DO I = 1 , NELEM
          CALL FIND_NODES(I     ,ELEMNODES,TAGELEM,IXS,IXS10,
     1                    IXQ   ,IXC      ,IXT    ,IXP,IXR,
     2                    IXTG  ,IXUR     ,KXX    ,IXX,KXIG3D,
     3                    IXIG3D,GEO      ,OFFELEM,NELMIN)
          PREV_NEIGH = 0
          C_NEIGH = 0
          J = 0 
          DO K=1,20
            IF ( ELEMNODES(K)/=0 ) THEN
              DO L=ADSKY(ELEMNODES(K)), ADSKY(ELEMNODES(K)+1)-1
                IF( CNE(L)  > 0) THEN
                  CONNECT_WEIGHT(CNE(L)) =
     .            CONNECT_WEIGHT(CNE(L)) + 1
                  IF( CONNECT_WEIGHT(CNE(L)) == 1 ) THEN
                    POINTER_NEIB(CNE(L))=PREV_NEIGH
                    C_NEIGH = C_NEIGH + 1
                    PREV_NEIGH = CNE(L)
                  ENDIF  
                ENDIF
              ENDDO 
              J=J+1
            ENDIF
          ENDDO
          ! if NELMIN is not defined by FIND_NODES, we keep edges 
          ! between elements that have 3 or more nodes in common
          IF(NELMIN == 0) NELMIN = 3 
C         C_NEIGH_MAX = MAX(C_NEIGH_MAX,C_NEIGH)
          IF (C_NEIGH > 0 ) THEN 
            DO J=1,C_NEIGH
              IF(I .NE. PREV_NEIGH) THEN
c              WRITE(6,*) I,"--",PREV_NEIGH,"=",CONNECT_WEIGHT(PREV_NEIGH)
                IF(CONNECT_WEIGHT(PREV_NEIGH).GE.NELMIN) THEN
                  CALL IDDCONNECTPLUS(I,PREV_NEIGH,NELEM)
                  CALL IDDCONNECTPLUS(PREV_NEIGH,I,NELEM)
                ENDIF
              ENDIF
              POINT_DELETE=PREV_NEIGH
              PREV_NEIGH = POINTER_NEIB(PREV_NEIGH)
              POINTER_NEIB(POINT_DELETE) = 0
              CONNECT_WEIGHT(POINT_DELETE) = 0
            ENDDO
          ENDIF
        ENDDO
        DEALLOCATE(CONNECT_WEIGHT)
        DEALLOCATE(POINTER_NEIB)
      ELSE                                               
C+------------------------------------------------------------+
C| Classical domain decomposition without edge filtering      |
C+------------------------------------------------------------+
        DO N = 1, NUMNOD
          DO CC1 = ADSKY(N), ADSKY(N+1)-1
            NUMG1 = CNE(CC1)
            IF(NUMG1 > 0) THEN ! by-pass extra nodes
              DO CC2 = CC1+1, ADSKY(N+1)-1
                NUMG2 = CNE(CC2)
                IF(NUMG2 > 0 .AND. NUMG1 /= NUMG2) THEN ! by-pass extra nodes
                  CALL IDDCONNECTPLUS(NUMG1,NUMG2,NELEM)
                  CALL IDDCONNECTPLUS(NUMG2,NUMG1,NELEM)              
                END IF
              ENDDO
            END IF
          ENDDO
        ENDDO
      ENDIF  !(EDGE_FILTERING == 0 )

        NEDGES = 0
      NEDGES_8 = 0
        DO I=1,NELEM
          CALL C_IDDCONNECT(I,TAILLE_LOCAL)
          NEDGES = NEDGES + TAILLE_LOCAL
        NEDGES_8 = NEDGES_8 + TAILLE_LOCAL 
        ENDDO
        NEDGES = NEDGES/2

       
C     DEALLOCATE(TAGELEM)
      IF (IDDLEVEL.EQ.1) THEN
C-----------------------------------------------
C by pass des elements outils
C-----------------------------------------------
        IWARN1 = 0
        DO I = 1, NELEM
          IF(IELEM21(I).EQ.1)THEN
            IF(WD(I).GT.0.01)THEN
              IWARN1 = 1
            END IF
          END IF
        END DO
        IF(IWARN1.NE.0)THEN
            WRITE(IOUT,*)' '
            WRITE(IOUT,'(A)')
     .  '  ONE OR MORE ELEMENT OF MAIN SIDE OF INTERF. TYPE21',
     .  '  NEEDS TO BE DEACTIVATED'
        END IF

C=======================================================================
C      FVMBAG Super ELEMENT Connectivity
C=======================================================================
        WD_MAX = 0
        IF(NVOLU > 0 .AND. IDDLEVEL == 1 .AND. ICFSI == 0) THEN 
          CALL FVBAG_VERTEX(IXC   ,IXTG    ,NELEM, WD,
     .                      WD_MAX,FVM_ELEM,FVM_DOMDEC,ITAB,IGRSURF,T_MONVOL)
        ENDIF 


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
Cel hierarchy : type 2 now, contacts after (type 7 & others)
        DO I = 1, NELEMINT
          N=IXINT(5,I)
          IF (N.LE.NUMNOD) THEN  !  POUR LES ELEMENTS ISOGEOMETRIQUES (PARTIE FICTIVE A NE PAS CONSIDERER)
            NUMG1=ABS(CNE(ADSKY(N)))
Cel initialisation numg2 pour cas ou facette non trouvee (erreur)
            NUMG2=NUMG1
            ITYPINT=ABS(IXINT(6,I))
            IF(ITYPINT.EQ.2) THEN
             IF(ADSKY(N+1)-ADSKY(N).GT.0)THEN
              N=IXINT(1,I)
              N1=IXINT(2,I)
              N2=IXINT(3,I)
              DO I1 = ADSKY(N), ADSKY(N+1)-1
                NUMG2=ABS(CNE(I1))
                DO I2 = ADSKY(N1), ADSKY(N1+1)-1
                  NUMG3=ABS(CNE(I2))
                  IF(NUMG3.EQ.NUMG2) THEN
                    DO I3 = ADSKY(N2), ADSKY(N2+1)-1
                      NUMG4=ABS(CNE(I3))
                      IF(NUMG4.EQ.NUMG2) GOTO 100
                    ENDDO
                  ENDIF
                ENDDO
              ENDDO
 100          CONTINUE
              IF(NUMG1 /= NUMG2) THEN
                CALL IDDCONNECTPLUS(NUMG1,NUMG2,NELEM)
                CALL IDDCONNECTPLUS(NUMG2,NUMG1,NELEM)
                CEP(NUMG1) = 1
                CEP(NUMG2) = 1
              ENDIF
             ENDIF
            ENDIF
          ENDIF
        ENDDO


        IF(ICCAND > 0) THEN
          DO N = 1,NUMNOD
            IF( IWCONT(4,N) > 0) THEN 
                DO I1 = ADSKY(N), ADSKY(N+1)-1
                   NUMG2=ABS(CNE(I1))
                   IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+IWCONT(4,N)
                ENDDO
            ENDIF
          ENDDO
        ENDIF



        ALLOCATE(ISORT(NELEMINT))
        ALLOCATE(INDEX_SORT(2*NELEMINT))

C sorting of NELEMINT : negative value for IXINT(6) means that distance is small
        DO I=1,NELEMINT
            ISORT(I)=(-IXINT(6,I)) + 100
            INDEX_SORT(I)=I
            ITYPINT=ABS(IXINT(6,I))
C             IF(ITYPINT.EQ.7 .OR. ITYPINT == 25 .AND IXINT(6,I) < 0) THEN
CC Add even more priority on edges connecting different colors
C             IF(COLORS(CNE(ADSKY(IXINT(5,I)))) .NE.
C      .           COLORS(CNE(ADSKY(IXINT(1,I))))) THEN
C                 ISORT(I) = ISORT(I) - 1 
C             ENDIF
C             ENDIF
        ENDDO
         CALL MY_ORDERS(0,WORK,ISORT,INDEX_SORT,NELEMINT,1)


C
Cel hierarchy : type 2 before, contact now
        DO II = 1, NELEMINT
          I = INDEX_SORT(II)
          N=IXINT(5,I)
          IF (N.LE.NUMNOD) THEN  !  POUR LES ELEMENTS ISOGEOMETRIQUES (PARTIE FICTIVE A NE PAS CONSIDERER)
            NUMG1=-1
C searching for the first connected element not seen
            CEP_MIN = HUGE(CEP_MIN)
            DO I1 = ADSKY(N), ADSKY(N+1)-1
              NUMG3=ABS(CNE(I1))
              IF(CEP_MIN > CEP(NUMG3)) THEN
                NUMG1 = NUMG3 
                CEP_MIN = CEP(NUMG1)
              ENDIF
              IF(CEP_MIN == 0) EXIT
            END DO
Cel initialisation numg2 pour cas ou facette non trouvee (erreur)
            NUMG2=-1
            ITYPINT=ABS(IXINT(6,I))
            IF(ITYPINT.EQ.7) THEN
             IF(ADSKY(N+1)-ADSKY(N).GT.0)THEN
              N=IXINT(1,I)
              N1=IXINT(2,I)
              N2=IXINT(3,I)
              IF (N.LE.NUMNOD) THEN !  POUR LES ELEMENTS ISOGEOMETRIQUES (PARTIE FICTIVE A NE PAS CONSIDERER)
               DO I1 = ADSKY(N), ADSKY(N+1)-1
                 NUMG2=ABS(CNE(I1))
                 IF(NUMG2 == NUMG1) THEN
                   GOTO 107
! Avoid adding edges between element already connected
                 ELSE
                   DO I2 = ADSKY(N1), ADSKY(N1+1)-1
                     NUMG3=ABS(CNE(I2))
                     IF(NUMG3 == NUMG1)  GOTO 107
                     IF(NUMG3.EQ.NUMG2) THEN
                       DO I3 = ADSKY(N2), ADSKY(N2+1)-1
                         NUMG4=ABS(CNE(I3))
                         IF(NUMG4 == NUMG1) GOTO 107
                         IF(NUMG4.EQ.NUMG2) GOTO 107
                       ENDDO
                     ENDIF
                   ENDDO
                 END IF
               ENDDO
              ENDIF
 107          CONTINUE

              IF(NUMG1 /= NUMG2 .AND. (NUMG1 >0 ) .AND. (NUMG2 > 0)) THEN
                IF(CEP(NUMG1).EQ.0.OR.CEP(NUMG2).EQ.0) THEN
                  number_of_added_edges = number_of_added_edges + 1
C test to limit number of edges added for contact interfaces
                  CALL IDDCONNECTPLUS(NUMG1,NUMG2,NELEM)
                  CALL IDDCONNECTPLUS(NUMG2,NUMG1,NELEM)
Cel                  IF(IXINT(6,I)<0)THEN   
Cel                    EDGEWT(NEWEDGE) = 1   ! near contact
Cel                  ELSE
Cel                    EDGEWT(NEWEDGE) = 1   ! candidate only
Cel                  END IF
                  CEP(NUMG1) = CEP(NUMG1) +  1
                  CEP(NUMG2) = CEP(NUMG2) +  1
                ELSE 
                  refused_cep0 = refused_cep0 + 1
                ENDIF
              ELSE
                if(NUMG1 == NUMG2) refused_numg = refused_numg + 1
                if(NUMG1<=0 .OR. NUMG2<=0) refused_numg0 = refused_numg0 + 1

              ENDIF
              IF(ICCAND > 0 .AND. NUMG2 > 0) THEN
C load-balancing contact force -- incremental needed for multiple interface
cc                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                IF(IXINT(6,I)<0)THEN
C 5:1 ratio between potential impact and just candidat 
                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+5
                ELSE
                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                ENDIF
              END IF

             ENDIF
            ELSEIF(ITYPINT.EQ.11) THEN
             IF(ADSKY(N+1)-ADSKY(N).GT.0)THEN
              N1=IXINT(3,I)
              N2=IXINT(4,I)
              DO I1 = ADSKY(N1), ADSKY(N1+1)-1
                NUMG2=ABS(CNE(I1))
                IF(NUMG2 /= NUMG1) THEN
                  DO I2 = ADSKY(N2), ADSKY(N2+1)-1
                    NUMG3=ABS(CNE(I2))
                    IF(NUMG3.EQ.NUMG2) GOTO 111
                  ENDDO
                END IF
              ENDDO
 111          CONTINUE
              IF(NUMG1 /= NUMG2 .AND.(NUMG1>0 .AND. NUMG2 > 0)) THEN
                IF(CEP(NUMG1).EQ.0.OR.CEP(NUMG2).EQ.0) THEN
C test to limit number of edges added for contact interfaces
                  number_of_added_edges = number_of_added_edges + 1

                  CALL IDDCONNECTPLUS(NUMG1,NUMG2,NELEM)
                  CALL IDDCONNECTPLUS(NUMG2,NUMG1,NELEM)
                  CEP(NUMG1) = CEP(NUMG1) +  1
                  CEP(NUMG2) = CEP(NUMG2) +  1
                ELSE 
                  refused_cep0 = refused_cep0 + 1
                ENDIF
              ELSE
                if(NUMG1 == NUMG2) refused_numg = refused_numg + 1
                if(NUMG1<=0 .OR. NUMG2<=0) refused_numg0 = refused_numg0 + 1
              ENDIF
              IF(ICCAND > 0 .AND. NUMG2 > 0) THEN
C load-balancing contact force -- incremental needed for multiple interface
cc                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                IF(IXINT(6,I)<0)THEN
                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                ELSE
                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                ENDIF
              END IF

             ENDIF
            ELSEIF(ITYPINT.EQ.24.OR.ITYPINT.EQ.25)THEN
             IF(ADSKY(N+1)-ADSKY(N).GT.0)THEN
              N=IXINT(1,I)
              N1=IXINT(2,I)
              N2=IXINT(3,I)
              DO I1 = ADSKY(N), ADSKY(N+1)-1
                NUMG2=ABS(CNE(I1))
               IF(NUMG2 == NUMG1) GOTO 124
                IF(NUMG2 /= NUMG1) THEN
                  DO I2 = ADSKY(N1), ADSKY(N1+1)-1
                    NUMG3=ABS(CNE(I2))
                    IF(NUMG3 == NUMG1) GOTO 124
                    IF(NUMG3.EQ.NUMG2) THEN
                      DO I3 = ADSKY(N2), ADSKY(N2+1)-1
                        NUMG4=ABS(CNE(I3))
                        IF(NUMG4 == NUMG1) GOTO 124
                        IF(NUMG4.EQ.NUMG2) GOTO 124
                      ENDDO
                    ENDIF
                  ENDDO
                END IF
              ENDDO
 124          CONTINUE
              IF(NUMG1 /= NUMG2 .AND. (NUMG1>0 .AND. NUMG2 > 0)) THEN
                IF(CEP(NUMG1).EQ.0.OR.CEP(NUMG2).EQ.0) THEN
                  number_of_added_edges = number_of_added_edges + 1

                  CALL IDDCONNECTPLUS(NUMG1,NUMG2,NELEM)
                  CALL IDDCONNECTPLUS(NUMG2,NUMG1,NELEM)
C test to limit number of edges added for contact interfaces
Cel                  IF(IXINT(6,I)<0 .AND. ICCAND > 0)THEN
Cel                    IWD(NCOND*(NUMG2-1)+ICCAND)=1
Cel     +              IWD(NCOND*(NUMG2-1)+ICCAND)+2 ! contact proche effectif
Cel                  ELSEIF(ICCAND > 0)THEN
Cel                    IWD(NCOND*(NUMG2-1)+ICCAND)=1
Cel     +              IWD(NCOND*(NUMG2-1)+ICCAND)+1 ! contact potentiel
C les autres elt sont a 1
Cel                  END IF
                  CEP(NUMG1) = CEP(NUMG1) +  1
                  CEP(NUMG2) = CEP(NUMG2) +  1
                ELSE 
                  refused_cep0 = refused_cep0 + 1
                ENDIF
              ELSE
                if(NUMG1 == NUMG2) refused_numg = refused_numg + 1
                if(NUMG1<=0 .OR. NUMG2<=0) refused_numg0 = refused_numg0 + 1
              ENDIF
              IF(ICCAND > 0 .AND. NUMG2 > 0) THEN
cc                IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                IF(IXINT(6,I)<0)THEN
                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+5 
                ELSE
                  IWD(NCOND*(NUMG2-1)+ICCAND)=IWD(NCOND*(NUMG2-1)+ICCAND)+1
                END IF
              END IF

             ENDIF            
            ENDIF
          ENDIF ! si element isogeometrique
        ENDDO

C ================================================================
C  Add connectivity between disconnected parts
C           according to the distance
        ALLOCATE(COLORS(NELEM+1),STAT=IERR1)
        ALLOCATE(ROOTS(NELEM),STAT=IERR1)
        CALL PLIST_BFS(NELEM,NCONNX,COLORS,ROOTS)

        !NUMG1 id of the root element of the larest connected part
        ALLOCATE(MIN_DIST(NCONNX))
        ALLOCATE(COORDS(3,NCONNX))
        DO I = 1,NCONNX

          CALL FIND_NODES(ROOTS(I)   ,ELEMNODES,TAGELEM,IXS,IXS10,
     1                    IXQ   ,IXC      ,IXT    ,IXP,IXR,
     2                    IXTG  ,IXUR     ,KXX    ,IXX,KXIG3D,
     3                    IXIG3D,GEO      ,OFFELEM,NELMIN)

          IF(ELEMNODES(1) /= 0) THEN
           COORDS(1:3,I) = X(1:3,ELEMNODES(1))
          ELSE
           COORDS(1:3,I) = ZERO
          ENDIF
        ENDDO

        DO I = 1, NCONNX
          NUMG1 = ROOTS(I)
          MIN_DIST(1:NCONNX) = HUGE(1.0)
          DO J = 1, NCONNX
            NUMG2 = ROOTS(J)
            IF(NUMG1 /= NUMG2) THEN
              MIN_DIST(J) = (COORDS(1,I)-COORDS(1,J))**2
     .                    + (COORDS(2,I)-COORDS(2,J))**2
     .                    + (COORDS(3,I)-COORDS(3,J))**2

            ENDIF
          ENDDO
          DIST = MINVAL(MIN_DIST(1:NCONNX))
          K = 0
          DO J = 1, NCONNX
            NUMG2 = ROOTS(J)
            IF(NUMG1 /= NUMG2 .AND. MIN_DIST(J) < 2.0*DIST) THEN
C connectivity added between roots of the distance is < 2 x the minimum
C distance between the current root, and its closest neighboor
              CALL IDDCONNECTPLUS(NUMG1,NUMG2,NELEM)
              CALL IDDCONNECTPLUS(NUMG2,NUMG1,NELEM)
              K = K + 1
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(MIN_DIST)
        DEALLOCATE(COORDS)
        DEALLOCATE(INDEX_SORT,ISORT)
C ================================================================
C       WRITE(6,*) "STATISTIC ON CONTACT INTERFACE"
C       WRITE(6,"(6(A,X,I10))") " added:",number_of_added_edges,
C    .                        " refused_numg: ",refused_numg,
C    .                        " refused_numg0: ",refused_numg0,
C    .                        " refused_cep0: ",refused_cep0,
C    .                        " switch_tried: ",switch_tried,
C    .                        " switch_done: ",switch_done
C
!       nombre de edge 
        NEDGES = 0
        NEDGES_8 = 0
        DO I=1,NELEM
          CALL C_IDDCONNECT(I,TAILLE_LOCAL)
          NEDGES = NEDGES + TAILLE_LOCAL
          NEDGES_8 = NEDGES_8 + TAILLE_LOCAL
        ENDDO
        NEDGES = NEDGES/2
        NEDGES_8 = NEDGES_8 / 2
      ENDIF

      IF(ALLOCATED(TAGELEM)) DEALLOCATE(TAGELEM)


!   ----------------------------------------------------------------
!   Check if there are some small rigid bodies (ie. with less than 40 secondary nodes)
!   in order to force the rigid body elements on a  given processor
!   loop over the rigid bodies
!       if small rigid body : save the element list in a vector (c_prevent_decomposition_rbody function)
!   if more than 1 small rigid body : force the domain decomposition (c_enforce_constraints_rbody function)
!
!   if more than 1 small rigid body : BOOL_RBODY logical = true
!
      BOOL_RBODY=.FALSE.

      IF(IDDLEVEL/=0) THEN
        NUMEL   = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELUR
     .        + NUMELTG+NUMELX+NUMSPH+NUMELIG3D

!       ------------------------
        K = 0
        DO N = 1, NRBYKIN
            NSN = NPBY(2,N) ! number of secondary nodes

            IF(NSN<40) THEN
                M   = NPBY(1,N) ! main nodes
                !   -----------------------------
                !   find the number of element in the rigid body
                NUMBER_OF_ELEMENT_RBODY = 0 ! number of element in the current RBODY
                ! ----------------
                !   secondary nodes
                DO J=1,NSN
                    I = LPBY(J+K)
                    DO IJK = ADSKY(I),ADSKY(I+1)-1
                        NUMBER_OF_ELEMENT_RBODY = NUMBER_OF_ELEMENT_RBODY + 1
                    ENDDO
                ENDDO
                ! ----------------
                !   main node
                DO IJK = ADSKY(M),ADSKY(M+1)-1
                    NUMBER_OF_ELEMENT_RBODY = NUMBER_OF_ELEMENT_RBODY + 1
                ENDDO
                ! ----------------
                ALLOCATE( LIST_ELEMENT_RBODY(NUMBER_OF_ELEMENT_RBODY) )
                !   -----------------------------

                NUMBER_OF_ELEMENT_RBODY = 0 ! number of element in the current RBODY
                ! ----------------
                !   secondary nodes
                DO J=1,NSN
                    I = LPBY(J+K)
                    DO IJK = ADSKY(I),ADSKY(I+1)-1
                        CC2 = IJK
                        NUMG2 = CNE(CC2)
                        NUMBER_OF_ELEMENT_RBODY = NUMBER_OF_ELEMENT_RBODY + 1
                        LIST_ELEMENT_RBODY( NUMBER_OF_ELEMENT_RBODY ) = NUMG2
                        BOOL_RBODY=.TRUE.
                    ENDDO
                ENDDO
                ! ----------------
                !   main node
                DO IJK = ADSKY(M),ADSKY(M+1)-1
                    CC2 = IJK
                    NUMG2 = CNE(CC2)
                    NUMBER_OF_ELEMENT_RBODY = NUMBER_OF_ELEMENT_RBODY + 1
                    LIST_ELEMENT_RBODY( NUMBER_OF_ELEMENT_RBODY ) = NUMG2
                ENDDO
                ! ----------------
                ! save the element list
                IF(NUMBER_OF_ELEMENT_RBODY>0) 
     .          CALL C_PREVENT_DECOMPOSITION_RBODY(NUMBER_OF_ELEMENT_RBODY,LIST_ELEMENT_RBODY)
                DEALLOCATE( LIST_ELEMENT_RBODY )
                ! ----------------
            ENDIF
            K = K + NSN
         ENDDO

!       ------------------------
       ENDIF
!   ----------------------------------------------------------------

      IF (NEDGES.GT.0 .AND. NSPMD .GT. 1) THEN      
!       structures Metis 1/2
        ALLOCATE(XADJ(NELEM+1),STAT=IERR1)
!       init XADJ
        XADJ(1:NELEM+1)=0
!       deallocation de CNE
        DEALLOCATE(CNE)
!       Nombre de edges
        NEDGES = 0
        DO I=1,NELEM
          CALL C_IDDCONNECT(I,TAILLE_LOCAL)
          NEDGES = NEDGES + TAILLE_LOCAL
        ENDDO
        NEDGES = NEDGES/2
!       structures Metis 2/2
        ALLOCATE(ADJNCY(2*NEDGES),STAT=IERR1)

        XADJ(1) = 1
        DO I=1,NELEM
          CALL C_IDDCONNECT(I,TAILLE_LOCAL)
          XADJ(I+1) = XADJ(I) + TAILLE_LOCAL
          IF(TAILLE_LOCAL>0) THEN
           CALL PLIST_IDDCONNECT(ADJNCY,XADJ,I)
          ENDIF
        ENDDO
!       deallocation de IDDCONNECT % PDOM et % IENTRYDOM
        DEALLOCATE(IDDCONNECT%PDOM)
        DEALLOCATE(IDDCONNECT%IENTRYDOM)

C Determine connectivity components
        IF(ALLOCATED(COLORS)) DEALLOCATE(COLORS)
        IF(ALLOCATED(ROOTS)) DEALLOCATE(ROOTS)
        ALLOCATE(COLORS(NELEM+1),STAT=IERR1)
        ALLOCATE(ROOTS(NELEM),STAT=IERR1)
        CALL DD_BFS(XADJ,ADJNCY,NELEM,NEDGES,NCONNX,COLORS,ROOTS)
        IF(NCONNX > 1) THEN
          WRITE(IOUT,'(A,I8)')
     .  ' NUMBER OF DISCONNECTED COMPONENTS FIXED FOR DOMAIN DECOMP:'
     .  ,NCONNX
C Metis Workaround to create additional connectivities between non connected graphs
          ALLOCATE(XADJ_OLD(NELEM+1),STAT=IERR1)
          ALLOCATE(ADJNCY_OLD(2*NEDGES),STAT=IERR1)
          XADJ_OLD(1:NELEM+1)=XADJ(1:NELEM+1)
          ADJNCY_OLD(1:2*NEDGES)=ADJNCY(1:2*NEDGES)
          NEWEDGE = NEDGES+NCONNX-1         
          DEALLOCATE(ADJNCY)
          ALLOCATE(ADJNCY(2*NEWEDGE),STAT=IERR1)
C 1) recompute new XADJ and fill new ADJCNY
          INC=0
          DO I = 1, NCONNX
            CURR=ROOTS(I) ! roots(1)=1
            I1=XADJ(CURR)
            I1OLD=XADJ_OLD(CURR)
            I2OLD=XADJ_OLD(CURR+1)-1
            IF(I>1)THEN
C insert 1 edge to previous connex component
              PREV=ROOTS(I-1) ! PREV < CURR < NEXT
              IF(I1OLD <= 2*NEDGES) THEN
                DO WHILE ((I1OLD <= I2OLD) .AND. 
     +                    (ADJNCY_OLD(I1OLD) < PREV))
                  ADJNCY(I1) = ADJNCY_OLD(I1OLD)
                  I1 = I1+1
                  I1OLD=I1OLD+1
                  IF(I1OLD > 2*NEDGES) EXIT 
                END DO 
              ENDIF
              ADJNCY(I1) = PREV
              I1=I1+1
              INC=INC+1 ! recall to swap INC+1 next addresses in XADJ
            END IF
            IF(I<NCONNX)THEN
C insert 1 edge to next connex component
              NEXT=ROOTS(I+1)
              IF(I1OLD <= 2*NEDGES) THEN
                DO WHILE ((I1OLD <= I2OLD) .AND. 
     +                    (ADJNCY_OLD(I1OLD) < NEXT))
                  ADJNCY(I1) = ADJNCY_OLD(I1OLD)
                  I1 = I1+1
                  I1OLD=I1OLD+1
                  IF(I1OLD > 2*NEDGES) EXIT 
                END DO               
              ENDIF
              ADJNCY(I1) = NEXT
              I1=I1+1
              INC=INC+1 ! increase shift value for next addresses in XADJ
            ELSE
              NEXT = NELEM+1 ! special value to stop recopy of remaining edges
            END IF
C finish to recopy the rest of the edges for CURR
            DO WHILE (I1OLD <= I2OLD)
              ADJNCY(I1) = ADJNCY_OLD(I1OLD)
              I1 = I1+1
              I1OLD=I1OLD+1
            END DO
C recopy the rest of the edges for all the vertices till NEXT or NELEM+1
            N=CURR+1
            DO WHILE (N /= NEXT)            
              XADJ(N)=XADJ(N)+INC
              I1=XADJ(N)
              I1OLD=XADJ_OLD(N)
              I2OLD=XADJ_OLD(N+1)-1
              DO WHILE (I1OLD <= I2OLD)
                ADJNCY(I1) = ADJNCY_OLD(I1OLD)
                I1 = I1+1
                I1OLD=I1OLD+1              
              END DO
              N = N+1
            END DO
C set correct XADJ for NEXT of NELEM+1
            XADJ(NEXT)=XADJ(NEXT)+INC
          END DO
C
          NEDGES=NEWEDGE
          DEALLOCATE(XADJ_OLD,ADJNCY_OLD)
C 2) recompute connexity to verify it is ok now
          CALL DD_BFS(XADJ,ADJNCY,NELEM,NEDGES,NCONNX,COLORS,ROOTS)
          IF(NCONNX > 1) THEN
            WRITE(IOUT,'(A,I8)')
     .      '** INFO: REMAINING DISCONNECTED COMPONENTS:',NCONNX
          END IF
        END IF
        DEALLOCATE(COLORS,ROOTS)

        WRITE(IOUT,*)' '
        WRITE(IOUT,FMT=FMW_A_I)
     .  '  ELEMENT NUMBER = ',NELEM
        WRITE(IOUT,FMT=FMW_A_I)'  EDGES FOUND    = ',NEDGES
        WRITE(IOUT,*)' '

        IWFLG=2
        NFLAG=1
C old metis option kept for compatibility
        OPTIONS(1)=0
#if defined(METIS5)
C new Metis5 Definition
        IERROR = METIS_SetDefaultOptions(options)
c         DO I = 1, 40
c           OPTIONS(I) = -1
c         END DO
C        OPTIONS(METIS_OPTION_NUMBERING) = 1 ! Fortran numbering -- position 17 en 5.0.2 et 18 en 5.1

         OPTIONS(18)=1
!         OPTIONS(8) = 3  ! METIS NCUTS options

C        OPTIONS(METIS OPTION CONTIG) = 1 ! Option for contiguous sub domains
C        OPTIONS(12)=1 
C        OPTIONS(METIS OPTION OBJTYPE) = 1 
C        OPTIONS(2)=0 ! 0 => CUT (default); 1 => VOL ;
C        OPTIONS(METIS OPTION CTYPE) = 1 
C        OPTIONS(3)=1 ! 0 => METIS CTYPE RM (default); 1 => METIS CTYPE SHEM (default) ;
C        OPTIONS(METIS OPTION IPTYPE) = 1 ! ignore
C        OPTIONS(4)=2 ! 0 => GROW ; 1 => RANDOM to try ; 2 => EDGE ; 3 => NODE
C        OPTIONS(METIS OPTION IRTYPE) = 1 ! ignore
C        OPTIONS(5)=0 ! 0 => FM ; 1 => Greedy to try ; 2 => 1 S NODE ; 3 => 2 S NODE
C        OPTIONS(METIS OPTION NITER) = 10 (default) !
C        OPTIONS(7)=20 ! 20 tres leger mieux
C        OPTIONS(METIS MIN CONN) = 0 (default) !
C        OPTIONS(11)=1 ! 1 minimize max connectivity
C        OPTIONS(METIS NO2HOP) = 0 (default) !
C        OPTIONS(10)=1 ! 20 ! ignore
C        OPTIONS(METIS UFACTOR) = 30 (default) !
C        OPTIONS(17)=1 ! ignore
C        OPTIONS(METIS NCUTS) = 1 (default) !
C         OPTIONS(8) = 4              
C        OPTIONS(8)=1 
#endif
C Domain decomposition CRASH ou FSI
        IF(ICFSI.EQ.0)THEN
          DO I = 1, NELEM
C normalisation des poids (elem delete a 0)
            IWD(NCOND*(I-1)+ICELEM) = NINT(WD(I)*100)
C poids interface deja calcul
          END DO
        ELSE
          DO I = 1, NELEM
            IF(I.LE.NUMELS)THEN
              MID = ABS(IXS(1,I))
              JALE = NINT(PM(72,MID))
              MLN = NINT(PM(19,MID))
              IF(JALE.EQ.0.AND.MLN.NE.18)THEN
                IWD(NCOND*(I-1)+ICELEM) = NINT(WD(I)*100)
                IWD(NCOND*(I-1)+ICFSI) = 0
              ELSE
                IWD(NCOND*(I-1)+ICELEM) = 0
                IWD(NCOND*(I-1)+ICFSI) = NINT(WD(I)*100)
              END IF
            ELSE
C normalisation des poids (elem delete a 0)
              IWD(NCOND*(I-1)+ICELEM) = NINT(WD(I)*100)
            END IF
C poids interface deja calcul
          END DO 
        END IF
        IF(ICDEL.GT.0)THEN
          DO I = 1, NELEM
Celem delete
            IF(WD(I).EQ.0.0001)THEN
              IWD(NCOND*(I-1)+ICDEL) = 1
            ELSE
              IWD(NCOND*(I-1)+ICDEL) = 0
            END IF
C poids interface deja calcul
          END DO
        END IF


C In case of cluster, transfer the weight to the first cluster element
        IF(NCLUSTER > 0) THEN 
          DO I = 1, NCLUSTER 
            CLUSTER_TYP = CLUSTERS(I)%TYPE
            OFFSET_CLUSTER = 0
            IF(CLUSTER_TYP==2.OR.CLUSTER_TYP==3) OFFSET_CLUSTER = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP
            DO J = 2, CLUSTERS(I)%NEL
              DO K =1, NCOND
               IWD((CLUSTERS(I)%ELEM(1)-1) * NCOND+K +OFFSET_CLUSTER) =
     .         IWD((CLUSTERS(I)%ELEM(1)-1) * NCOND+K +OFFSET_CLUSTER) + 
     .         IWD((CLUSTERS(I)%ELEM(J)-1) * NCOND+K +OFFSET_CLUSTER) 
               IWD((CLUSTERS(I)%ELEM(J)-1) * NCOND+K +OFFSET_CLUSTER) = 0 
              ENDDO
            END DO
          END DO         
        ENDIF


C
C Traitement specifique debordement poids entier
C 
        DO I = 1, NCOND
 1024     CONTINUE
          WS = ZERO
          DO J = 1, NELEM
            WS = WS + IWD(NCOND*(J-1)+I)
          END DO
          IF(WS.GT.2*EP9)THEN
            WRITE(IOUT,'(A,I4)')
     .      ' WEIGHT PRECISION DECREASED TO ENABLE CRITERION',I
            DO J = 1, NELEM
              IWD(NCOND*(J-1)+I) = IWD(NCOND*(J-1)+I)/10
            END DO
            GO TO 1024
          END IF
        END DO

C
        UBVEC(1:15) = 0
        UBVEC(ICELEM) = 1.02
        IF(ICINTS/=0) UBVEC(ICINTS) = 1.05
        IF(ICINTM/=0) UBVEC(ICINTM) = 1.05
        IF(ICINT2/=0) UBVEC(ICINT2) = 1.05
        IF(ICDDL/=0)  UBVEC(ICDDL)  = 1.02
        IF(ICSOL/=0)  UBVEC(ICSOL)  = 1.05
        IF(ICFSI/=0)  UBVEC(ICFSI)  = 1.02
        IF(ICDEL/=0)  UBVEC(ICDEL)  = 1.10
        IF(ICCAND/=0) UBVEC(ICCAND) = 1.10
        IF(ICKIN/=0)  UBVEC(ICKIN)  = 1.10
        IF(ICR2R/=0)  UBVEC(ICR2R) = 1.30  ! weird !!!
        IF(ICNOD_SMS/=0) UBVEC(ICNOD_SMS) = 1.05
c        i=0
c        call METIS_EstimateMemory(NELEM,XADJ,ADJNCY,0,2,I)
c        print *,'estimate memory=',i,nelem,XADJ(NELEM+1)
 1999    CONTINUE
         IF(DECTYP.EQ.3.OR.DECTYP.EQ.5)THEN
C KWAY METIS
#if defined(METIS5)
      
          IERR1 = Wrap_METIS_PartGraphKway(
     1      NELEM,NCOND,XADJ,ADJNCY,
     2      IWD,NNODE,
     3      UBVEC,OPTIONS,NEC,CEP)
          IDB_METIS = 0

          IF(IDB_METIS == 1) THEN
C write graph for Metis debug
            it=0
            WRITE(CHLEVEL,'(I1)')IDDLEVEL
C weight only on vertices
            OPEN(99,file="input.graph"//CHLEVEL,FORM='FORMATTED',RECL=8192)
            write(99,*) nelem,nedges,"010",ncond
            do i = 1, nelem
              write(99,*)iwd(NCOND*(I-1)+1:NCOND*(I-1)+ncond),
     +                    adjncy(xadj(i):xadj(i+1)-1)
              it = it + xadj(i+1)-xadj(i)
            end do
            print *,'writing graph with check:',it,'/',nedges*2
            CLOSE(99)
          END IF     
#elif 1
          CALL METIS_mCPartGraphKway(
     1      NELEM,NCOND,XADJ,ADJNCY,IWD,0,
     2      IWFLG,NFLAG,NNODE,UBVEC,OPTIONS,NEC,CEP)
#endif
         ELSEIF(DECTYP.EQ.4.OR.DECTYP.EQ.6)THEN
C RSB METIS
#if defined(METIS5)
          IERR1 = Wrap_METIS_PartGraphRecursive(
     1      NELEM,NCOND,XADJ,ADJNCY,
     2      IWD,NNODE,
     3      UBVEC,OPTIONS,NEC,CEP)
#elif 1
          CALL METIS_mCPartGraphRecursive(
     1      NELEM,NCOND,XADJ,ADJNCY,IWD,0,
     2      IWFLG,NFLAG,NNODE,OPTIONS,NEC,CEP)
#endif
        END IF
C       1234567123456789012345123456789012345123456789012345123456789012345
            CALL STAT_DOMDEC(
     1           WIS      ,WI2     ,WFSI      ,WDEL      ,WDDL     ,
     2           WCAND    ,WSOL    ,WR2R      ,WKIN      ,IWD      ,
     3           NCOND    ,ICELEM  ,ICINTS    ,ICINT2    ,ICCAND   ,
     4           ICDDL    ,ICSOL   ,ICFSI     ,ICDEL     ,ICR2R    ,
     5           ICKIN    ,AVERAGE ,DEVIATION ,DMAX      ,DMIN     ,
     6           CEP      ,NELEM   ,W         ,ICINTM    ,WIM      ,
     7           NCRITMAX ,WNOD_SMS,ICNOD_SMS)
      

    

      IF(ICFSI > 0 .AND.  ICFSI < ICELEM) THEN 
! the order in DMIN,DMAX is independant of the orther of the constrains
        MAIN_TARGET = 7
      ELSE
        MAIN_TARGET = 1
      ENDIF

C     CHECK Quality of Domain Decomp on elements
C If ( ALE .or. first domdec) .and. (first try)) then check load balance
        IF( ( MAIN_TARGET == 7 .OR. IDDLEVEL.EQ.1) .AND. (DECTYP==3 .OR. DECTYP==5) )THEN
          IF(DMIN(MAIN_TARGET) < AVERAGE(MAIN_TARGET)*0.90  )THEN
            WRITE(IOUT,'(A)')
     .      '** INFO: DECOMPOSITION UNBALANCING DETECTED'
            WRITE(IOUT,'(A,I5,A,2X,I8,2X,I8,2X,I8)')
     .      ' DOMAINS:',NSPMD,'  MIN/MAX/AVERAGE:',
     .      NINT(DMIN(MAIN_TARGET)),NINT(DMAX(MAIN_TARGET)),NINT(AVERAGE(MAIN_TARGET))
c           IF(.NOT. FVM_DOMDEC) THEN
               WRITE(IOUT,'(A)')' REVERT TO RECURSIVE BISSECTION'
c           ENDIF
            DECTYP=DECTYP+1

            IF(FVM_DOMDEC) THEN
              UBVEC(ICELEM) = 1.01
              IF(ICINTS/=0) UBVEC(ICINTS) = 1.02
              IF(ICINTM/=0) UBVEC(ICINTM) = 1.02
              IF(ICINT2/=0) UBVEC(ICINT2) = 1.02
              IF(ICDDL/=0)  UBVEC(ICDDL)  = 1.05
              IF(ICSOL/=0)  UBVEC(ICSOL)  = 1.05
              IF(ICFSI/=0)  UBVEC(ICFSI)  = 1.05
              IF(ICDEL/=0)  UBVEC(ICDEL)  = 1.05
              IF(ICCAND/=0) UBVEC(ICCAND) = 1.05
              IF(ICKIN/=0)  UBVEC(ICKIN)  = 1.05
              IF(ICR2R/=0)  UBVEC(ICR2R) = 1.30
              IF(ICNOD_SMS/=0) UBVEC(ICNOD_SMS) = 1.0
            ELSE
              UBVEC(ICELEM) = 1.001
              IF(ICINTS/=0) UBVEC(ICINTS) = 1.02
              IF(ICINTM/=0) UBVEC(ICINTM) = 1.02
              IF(ICINT2/=0) UBVEC(ICINT2) = 1.02
              IF(ICDDL/=0)  UBVEC(ICDDL)  = 1.01
              IF(ICSOL/=0)  UBVEC(ICSOL)  = 1.03
              IF(ICFSI/=0)  UBVEC(ICFSI)  = 1.01
              IF(ICDEL/=0)  UBVEC(ICDEL)  = 1.03
              IF(ICCAND/=0) UBVEC(ICCAND) = 1.03
              IF(ICKIN/=0)  UBVEC(ICKIN)  = 1.03
              IF(ICR2R/=0)  UBVEC(ICR2R) = 1.30
             IF(ICNOD_SMS/=0) UBVEC(ICNOD_SMS) = 1.0
            ENDIF
            GOTO 1999
          END IF
        END IF
C---------------------------------------------------------------------
C    Loop over domain decomposition until satisfactory load balancing for element
C---------------------------------------------------------------------
       MAX_TRY       = 3
       WD_MAX_FACTOR = 2
#if defined(METIS5)
        ALLOCATE(IWD_COPY(NCOND*NELEM))
        ALLOCATE(WD_COPY(NELEM))
        IF((DECTYP==4 .OR. DECTYP==6) .AND. IDDLEVEL==1 .AND. NELEM>10*NSPMD )THEN

          IF(ICDEL /= 0 ) THEN 
            IF(ELEMD > 9*NELEM/10 .AND. DMIN(MAIN_TARGET) <  AVERAGE(MAIN_TARGET)*0.80  ) THEN
            ! If the model is mainly deleted elements
            ! Then we equilibrate first on deleted elements
             DO I= 1, NELEM
               WGHT=IWD(NCOND*(I-1)+1)
               IWD(NCOND*(I-1)+1) = IWD(NCOND*(I-1)+ICDEL)
               IWD(NCOND*(I-1)+ICDEL)=WGHT
             ENDDO 
            ENDIF
          ENDIF

          NCOND2=NCOND 
          DD_FVMBAG_TRY = 0
          WD_MAX0 = WD_MAX
          WD_COPY(1:NELEM) = WD(1:NELEM)
          IWD_COPY(1:NCOND * NELEM) = IWD(1:NCOND*NELEM)
  
          DD_UNBALANCED =  (DMIN(MAIN_TARGET) <  AVERAGE(MAIN_TARGET)*0.80)
          IF(FVM_DOMDEC) THEN
            DD_UNBALANCED = DD_UNBALANCED .OR.  (DMAX(MAIN_TARGET) >  AVERAGE(MAIN_TARGET)*1.1)
            WD_MAX0 = 0.0
            DO N = 1, NVOLU 
              IF(FVM_ELEM(N) .NE. 0) THEN  
                WD_MAX0= MAX(WD_MAX0,DBLE(WD(FVM_ELEM(N))))
              ENDIF
            ENDDO
            WD_MAX0 = MIN(WD_MAX,WD_MAX0)
            WD_MAX = WD_MAX0
          ENDIF

          DO WHILE(DD_UNBALANCED .AND.  NCOND2 > 1 )
C      CHECK Quality of Domain Decomp on elements
            WRITE(IOUT,'(A)')
     .      '** INFO: DECOMPOSITION UNBALANCING DETECTED'
            WRITE(IOUT,'(A,I5,A,2X,I8,2X,I8,2X,I8)')
     .      ' DOMAINS:',NSPMD,'  MIN/MAX/AVERAGE:',
     .      NINT(DMIN(MAIN_TARGET)),NINT(DMAX(MAIN_TARGET)),NINT(AVERAGE(MAIN_TARGET))
      
            !==========================================
            !    REVIEW WEIGHTS OF FVMBAGS
            ! 
            ! Try to trim the weight of FVMBAG
            ! if the domain decomposition fails
            NB_FVMBAG_TRIM = 0
            IF(FVM_DOMDEC .AND. DD_FVMBAG_TRY <= MAX_TRY) THEN
              WD_MAX = WD_MAX / (0.1D0 * WD_MAX_FACTOR)
              DO N = 1, NVOLU 
                IF(FVM_ELEM(N) .NE. 0) THEN  
                  IF(WD(FVM_ELEM(N)) > WD_MAX) THEN
                     WD(FVM_ELEM(N)) = WD_MAX
                     IWD(NCOND*(FVM_ELEM(N)-1)+ICELEM) = NINT(WD_MAX*100)
                     NB_FVMBAG_TRIM = NB_FVMBAG_TRIM + 1 
                  ENDIF
                ENDIF
              ENDDO
            ENDIF 
            IF(NB_FVMBAG_TRIM > 0) THEN 
              ! Try to reduce the weight of the FVMBAG vertex
              ! before reducing the number of constrains
              DD_FVMBAG_TRY = DD_FVMBAG_TRY + 1
            ELSE
              ! Reducing the number of constrains
              ! Reseting weights
              NCOND2= NCOND2 - 1
              DD_FVMBAG_TRY = 0
              MAX_TRY = MAX_TRY + 1
              WD_MAX = WD_MAX0
              WD(1:NELEM) = WD_COPY(1:NELEM) 
              IWD(1:NCOND*NELEM) = IWD_COPY(1:NCOND*NELEM) 
            ENDIF
            !==============================================



            WRITE(IOUT,'(A,I5)') 'RETRY KWAY WITH NCOND =',NCOND2

            ALLOCATE(IWD2(NCOND2*NELEM))
            DO I= 1, NELEM
              DO J = 1, NCOND2
                IWD2( NCOND2*(I-1) +J ) = IWD ( NCOND*(I-1) + J)
              ENDDO
            ENDDO 
C  KWAY METIS
            IERR1 = WRAP_METIS_PartGraphKway(
     1      NELEM,NCOND2,XADJ,ADJNCY,
     2      IWD2,NNODE,
     3      UBVEC,OPTIONS,NEC,CEP)    
            CALL STAT_DOMDEC(
     1           WIS      ,WI2     ,WFSI      ,WDEL      ,WDDL     ,
     2           WCAND    ,WSOL    ,WR2R      ,WKIN      ,IWD      ,
     3           NCOND    ,ICELEM  ,ICINTS    ,ICINT2    ,ICCAND   ,
     4           ICDDL    ,ICSOL   ,ICFSI     ,ICDEL     ,ICR2R    ,
     5           ICKIN    ,AVERAGE ,DEVIATION ,DMAX      ,DMIN     ,
     6           CEP      ,NELEM   ,W         ,ICINTM    ,WIM      ,
     7           NCRITMAX ,WNOD_SMS,ICNOD_SMS)

!      CHECK Quality of Domain Decomp on elements
            DD_UNBALANCED =  (DMIN(MAIN_TARGET) <  AVERAGE(MAIN_TARGET)*0.80)
            IF(FVM_DOMDEC) THEN
              DD_UNBALANCED = DD_UNBALANCED .OR.  (DMAX(MAIN_TARGET) >  AVERAGE(MAIN_TARGET)*1.1)
            ENDIF


            IF(DD_UNBALANCED)THEN

              WRITE(IOUT,'(A)')
     .        '** INFO: DECOMPOSITION UNBALANCING DETECTED'
              WRITE(IOUT,'(A,I5,A,2X,I8,2X,I8,2X,I8)')
     .        ' DOMAINS:',NSPMD,'  MIN/MAX/AVERAGE:',
     .        NINT(DMIN(MAIN_TARGET)),NINT(DMAX(MAIN_TARGET)),NINT(AVERAGE(MAIN_TARGET))

C  RSB METIS

              IERR1 = WRAP_METIS_PartGraphRecursive(
     1        NELEM,NCOND2,XADJ,ADJNCY,
     2        IWD2,NNODE,
     3        UBVEC,OPTIONS,NEC,CEP)
              CALL STAT_DOMDEC(
     1             WIS      ,WI2     ,WFSI      ,WDEL      ,WDDL     ,
     2             WCAND    ,WSOL    ,WR2R      ,WKIN      ,IWD      ,
     3             NCOND    ,ICELEM  ,ICINTS    ,ICINT2    ,ICCAND   ,
     4             ICDDL    ,ICSOL   ,ICFSI     ,ICDEL     ,ICR2R    ,
     5             ICKIN    ,AVERAGE ,DEVIATION ,DMAX      ,DMIN     ,
     6             CEP      ,NELEM   ,W         ,ICINTM    ,WIM      ,
     7             NCRITMAX ,WNOD_SMS,ICNOD_SMS) 
        
            ENDIF       
            DEALLOCATE(IWD2)                                 

            DD_UNBALANCED =  (DMIN(MAIN_TARGET) <  AVERAGE(MAIN_TARGET)*0.80)
            IF(FVM_DOMDEC) THEN
              DD_UNBALANCED = DD_UNBALANCED .OR.  (DMAX(MAIN_TARGET) >  AVERAGE(MAIN_TARGET)*1.1)
            ENDIF

          ENDDO  !  ( DMIN(MAIN_TARGET) <  AVERAGE(MAIN_TARGET)*0.80  .AND.  NCOND2 > 1 )
        ENDIF 
        DEALLOCATE(IWD_COPY)
        DEALLOCATE(WD_COPY)

#endif
C---------------------------------------------------------------------
C       End of loop over domain decomposition                                             
C---------------------------------------------------------------------
        ! stick the list of rigid body element on a given processor
        IF(IDDLEVEL/=0.AND.BOOL_RBODY) CALL C_ENFORCE_CONSTRAINTS_RBODY(CEP,NSPMD,NRBYKIN)

        ! make sure that lists of elements in C_PREVENT_DECOMPOSITION are on the same domain
        CALL C_ENFORCE_CONSTRAINTS(CEP)

C       Put all the elements of the cluster on the same proc
        IF (NCLUSTER > 0) THEN
          DO I = 1, NCLUSTER  
           CLUSTER_TYP = CLUSTERS(I)%TYPE
           OFFSET_CLUSTER = 0
           IF(CLUSTER_TYP==2.OR.CLUSTER_TYP==3) OFFSET_CLUSTER = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP
           CEPCLUSTER=CEP(  CLUSTERS(I)%ELEM(1)+OFFSET_CLUSTER  )
           DO J = 2,CLUSTERS(I)%NEL  
              CEP(  CLUSTERS(I)%ELEM(J)+OFFSET_CLUSTER  ) = CEPCLUSTER
           END DO        
          END DO ! I = 1, NCLUSTER  
        END IF ! NCLUSTER > 0


C                                                         
C       Save the PMAIN in FVMAIN 
        IF(NVOLU > 0 .AND. IDDLEVEL==1 .AND. FVM_DOMDEC) THEN

           OFFC = NUMELS+NUMELQ
           OFFTG =NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR
           NN_L = 0
           CEPCLUSTER = 1
           NFVMBAG = 0
           DO N = 1, NVOLU 
              ITYP = T_MONVOL(N)%TYPE
              NN = T_MONVOL(N)%NNS
!     find location of the first element
!     i.e. the element with all the weight
              IF(ITYP == 6 .OR. ITYP == 8) NFVMBAG = NFVMBAG + 1

              IF(NN > 0 .AND. (ITYP == 6 .OR. ITYP == 8)) THEN
                 CEPCLUSTER = CEP(FVM_ELEM(N))
                 FVMAIN(NFVMBAG) = CEPCLUSTER 
              ENDIF 
           ENDDO         
        ENDIF

C
        DEALLOCATE(XADJ,ADJNCY)
!        IF(ASSOCIATED(ADJWGT2))  DEALLOCATE(ADJWGT2) 
       
        DO I = 1, NELEM
          CEP(I) = CEP(I)-1
        END DO
        
        !---------------------!       
        !2D - EBCS : send boundary cells in domain 1
        DO I=1,NUMELQ
          IF(EBCS_TAG_CELL_SPMD(I)==1)THEN
            CEP(NUMELS+I)=0
          ENDIF
        ENDDO
        DO I=1,NUMELTG
          IF(EBCS_TAG_CELL_SPMD(NUMELQ+I)==1)THEN
            CEP(NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+I)=0
          ENDIF
        ENDDO 
        !3D - EBCS : send boundary cells in domain 1        
        DO I=1,NUMELS
          IF(EBCS_TAG_CELL_SPMD(NUMELQ+NUMELTG+I)==1)THEN
            CEP(I)=0
          ENDIF
        ENDDO         
        !---------------------!       
C
        IF(DECTYP.EQ.5.OR.DECTYP.EQ.6)THEN
           IF(DDNOD_SMS==0)THEN
             WRITE(IOUT,1000)
           ELSE
             WRITE(IOUT,1100)
           END IF
        ELSEIF(ICFSI.EQ.0) THEN
          IF(ICSOL.EQ.0.AND.ICDEL.EQ.0)THEN
            WRITE(IOUT,2000)
          ELSEIF(ICSOL.NE.0.AND.ICDEL.EQ.0)THEN
            WRITE(IOUT,3000)
          ELSEIF(ICSOL.NE.0.AND.ICDEL.NE.0)THEN
            WRITE(IOUT,4000)
          ELSEIF(ICSOL.EQ.0.AND.ICDEL.NE.0)THEN
            WRITE(IOUT,5000)
          END IF
        ELSEIF(ICFSI.NE.0)THEN
          IF(ICDEL.EQ.0)THEN
            WRITE(IOUT,6000)
          ELSE
            WRITE(IOUT,7000)
          END IF
        END IF
        DO I = 1, NSPMD
          IF(DECTYP.EQ.5.OR.DECTYP.EQ.6)THEN
            IF(DDNOD_SMS==0)THEN
              WRITE(IOUT,'(I4,8F15.0)')
     .        I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WDDL(I)
            ELSE
              WRITE(IOUT,'(I4,8F15.0)')
     .        I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WDDL(I),WNOD_SMS(I)
            END IF
          ELSEIF(ICFSI.EQ.0)THEN
            IF(ICSOL.EQ.0.AND.ICDEL.EQ.0)THEN
              WRITE(IOUT,'(I4,8F15.0)')
     .        I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WKIN(I)
            ELSEIF(ICSOL.NE.0.AND.ICDEL.EQ.0)THEN
              WRITE(IOUT,'(I4,8F15.0)')
     .        I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WSOL(I),WKIN(I)
            ELSEIF(ICSOL.NE.0.AND.ICDEL.NE.0)THEN
              WRITE(IOUT,'(I4,8F15.0)')
     .        I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WSOL(I),WDEL(I),WKIN(I)
            ELSEIF(ICSOL.EQ.0.AND.ICDEL.NE.0)THEN
              WRITE(IOUT,'(I4,8F15.0)')
     .        I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WDEL(I),WKIN(I)
            ENDIF
          ELSEIF(ICFSI.NE.0.AND.ICDEL.EQ.0)THEN
            WRITE(IOUT,'(I4,8F15.0)')
     .      I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WFSI(I)
          ELSEIF(ICFSI.NE.0.AND.ICDEL.NE.0)THEN
            WRITE(IOUT,'(I4,8F15.0)')
     .      I,W(I),WIS(I),WIM(I),WCAND(I),WI2(I),WFSI(I),WDEL(I)
          ENDIF
        ENDDO
        WRITE(IOUT,*)' '               
        DEALLOCATE(IWD)
        WRITE(IOUT,*)'STATISTICS ON DECOMPOSITION WEIGHTS'
        WRITE(IOUT,*)'-----------------------------------'
        WRITE(IOUT,8000)    
        WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' ELEMENTS   ',
     .    NINT(DMIN(1)),NINT(DMAX(1)),
     .    NINT(AVERAGE(1)),NINT(DEVIATION(1))
        IF(ICINTS/=0) WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' SECO. NODES',
     .    NINT(DMIN(2)),NINT(DMAX(2)),
     .    NINT(AVERAGE(2)),NINT(DEVIATION(2))
        IF(ICINTM/=0) WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' MAIN NODES',
     .    NINT(DMIN(11)),NINT(DMAX(11)),
     .    NINT(AVERAGE(11)),NINT(DEVIATION(11))
        IF(ICCAND/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' CONT. CAND.',
     .    NINT(DMIN(4)),NINT(DMAX(4)),
     .    NINT(AVERAGE(4)),NINT(DEVIATION(4))
        IF(ICINT2/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' INT. TYPE2 ',
     .    NINT(DMIN(3)),NINT(DMAX(3)),
     .    NINT(AVERAGE(3)),NINT(DEVIATION(3))
        IF(ICSOL/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' SOLID BAR. ',
     .    NINT(DMIN(6)),NINT(DMAX(6)),
     .    NINT(AVERAGE(6)),NINT(DEVIATION(6))
        IF(ICDEL/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' ELT. DEL.  ',
     .    NINT(DMIN(8)),NINT(DMAX(8)),
     .    NINT(AVERAGE(8)),NINT(DEVIATION(8))
        IF(ICKIN/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' KIN. COND. ',
     .    NINT(DMIN(10)),NINT(DMAX(10)),
     .    NINT(AVERAGE(10)),NINT(DEVIATION(10))
        IF(ICDDL/=0)THEN
          IF(ISMS==0)THEN ! Implicit
            WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .        ' DOF (IMPL) ',
     .        NINT(DMIN(5)),NINT(DMAX(5)),
     .        NINT(AVERAGE(5)),NINT(DEVIATION(5))
          ELSE ! AMS
            WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .        ' AMS MATRIX ',
     .        NINT(DMIN(5)),NINT(DMAX(5)),
     .        NINT(AVERAGE(5)),NINT(DEVIATION(5))
          END IF
        END IF
        IF(ICFSI/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' ALE ELTS.  ',
     .    NINT(DMIN(7)),NINT(DMAX(7)),
     .    NINT(AVERAGE(7)),NINT(DEVIATION(7))
        IF(ICR2R/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' R2R        ',
     .    NINT(DMIN(9)),NINT(DMAX(9)),
     .    NINT(AVERAGE(9)),NINT(DEVIATION(9))
        IF(ICNOD_SMS/=0)       WRITE(IOUT,'(A,I8,2X,I8,2X,I8,4X,I8)')
     .    ' AMS NODES  ',
     .    NINT(DMIN(12)),NINT(DMAX(12)),
     .    NINT(AVERAGE(12)),NINT(DEVIATION(12))
      ELSE
Cel un seul element ou elements non connectes ou un seul proc
        DEALLOCATE(CNE)
        DEALLOCATE(IDDCONNECT%PDOM)
        DEALLOCATE(IDDCONNECT%IENTRYDOM) 
        DO I = 1, NELEM
          CEP(I) = 0
        ENDDO
      ENDIF
C
 1000 FORMAT('#PROC    ELEMENT W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.         DOF W.') 
 1100 FORMAT('#PROC    ELEMENT W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.         DOF W. AMS CONT ELT W') 
 2000 FORMAT('#PROC    ELEMENT W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.    KIN COND W.')
 3000 FORMAT('#PROC    ELEMENT W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.         SOL W.    KIN COND W.')
 4000 FORMAT('#PROC    ELEMENT W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.         SOL W.     ELT DEL W.',
     .       '    KIN COND W.')
 5000 FORMAT('#PROC    ELEMENT W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.     ELT DEL W.    KIN COND W.')
 6000 FORMAT('#PROC    ELT LAG W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.        ELT ALE W.')
 7000 FORMAT('#PROC    ELT LAG W.    SECND NOD W.   MAST NOD W.    CONT ELT W.',
     .       '        INT2 W.     ELT ALE W.     ELT DEL W.')
 8000 FORMAT(' METRIC      MINIMUM   MAXIMUM   AVERAGE',
     .       '   STANDARD DEVIATION')
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I7NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|====================================================================
      SUBROUTINE I7NINTDD(NELEMINT,IXINT ,IRECT ,NSV  ,CAND_E ,
     2                    CAND_N  ,I_STOK,IFIEND,X    ,GAP    ,
     3                    DRAD    ,IGAP  ,GAP_S ,GAP_M,GAP_S_L,
     4                    GAP_M_L ,GAPMIN,GAPMAX,DGAPLOAD)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEMINT, IXINT(6,*), IRECT(4,*),NSV(*),
     .   CAND_E(*),CAND_N(*),I_STOK ,IFIEND, IGAP
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   GAP, X(3,*), GAPMAX, GAPMIN,
     .   GAP_M(*), GAP_S(*), GAP_S_L(*), GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, L, I,a,b,
     .   IG,IX1,IX2,IX3,IX4
      my_real
     .   XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,SEUIL,
     .   XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4
C-----------------------------------------------
      IFIEND = IFIEND + I_STOK*6
      CALL ZEROIN (1,I_STOK*6,IXINT)
C calcul du facteur de poids relatif
      SEUIL = MAX(GAP+DGAPLOAD,DRAD)
      DO I = 1, I_STOK
          IG = NSV(CAND_N(I))
          L  = CAND_E(I)
          IXINT(1,I)=IRECT(1,L)
          IXINT(2,I)=IRECT(2,L)
          IXINT(3,I)=IRECT(3,L)
          IXINT(4,I)=IRECT(4,L)
          IXINT(5,I)=IG
          IXINT(6,I)=7
          IX1=IRECT(1,L)
          IX2=IRECT(2,L)
          IX3=IRECT(3,L)
          IX4=IRECT(4,L)
          ZI = X(3,IG)
          Z1=X(3,IX1)
          Z2=X(3,IX2)
          Z3=X(3,IX3)
          Z4=X(3,IX4)
          IF(IGAP==0)THEN
            SEUIL = GAP
          ELSE
            SEUIL=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
            IF(IGAP==3)
     .        SEUIL=MIN(SEUIL,
     .             GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
            SEUIL=MIN(SEUIL,GAPMAX)
            SEUIL=MAX(SEUIL,GAPMIN)
          ENDIF
          SEUIL = MAX(SEUIL+DGAPLOAD,DRAD)
 
          ZMIN = MIN(Z1,Z2,Z3,Z4)-SEUIL
          ZMAX = MAX(Z1,Z2,Z3,Z4)+SEUIL
          IF (ZMIN.LE.ZI.AND.ZMAX.GE.ZI) THEN
            YI = X(2,IG)
            Y1 = X(2,IX1)
            Y2 = X(2,IX2)
            Y3 = X(2,IX3)
            Y4 = X(2,IX4)
            YMIN = MIN(Y1,Y2,Y3,Y4)-SEUIL
            YMAX = MAX(Y1,Y2,Y3,Y4)+SEUIL
            IF (YMIN.LE.YI.AND.YMAX.GE.YI) THEN
              XI = X(1,IG)
              X1 = X(1,IX1)
              X2 = X(1,IX2)
              X3 = X(1,IX3)
              X4 = X(1,IX4)
              XMIN = MIN(X1,X2,X3,X4)-SEUIL
              XMAX = MAX(X1,X2,X3,X4)+SEUIL
              IF (XMIN.LE.XI.AND.XMAX.GE.XI) THEN
                IXINT(6,I)=-7
              ENDIF
            ENDIF
          ENDIF          
      ENDDO
C
      NELEMINT=NELEMINT+I_STOK
C
      RETURN
      END
Chd|====================================================================
Chd|  I11NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|====================================================================
      SUBROUTINE I11NINTDD(NELEMINT,IXINT ,IRECTS ,IRECTM,CAND_S,
     .                     CAND_M   ,I_STOK,IFIEND,X     ,GAP   ,
     .                     DRAD     ,DGAPLOAD)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEMINT, IXINT(6,*), IRECTS(2,*), IRECTM(2,*),
     .   CAND_S(*), CAND_M(*),I_STOK ,IFIEND

      my_real
     .   GAP, X(3,*)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, L, I,a,b,
     .   IG,IX1,IX2,IX3,IX4
      my_real
     .   XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,SEUIL,
     .   XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4
C-----------------------------------------------
      IFIEND = IFIEND + I_STOK*6
      CALL ZEROIN (1,I_STOK*6,IXINT)
C calcul du facteur de poids relatif
      SEUIL = MAX(GAP+DGAPLOAD,DRAD)
      DO I = 1, I_STOK
        IXINT(1,I)=IRECTS(1,CAND_S(I))
        IXINT(2,I)=IRECTS(2,CAND_S(I))
        IXINT(3,I)=IRECTM(1,CAND_M(I))
        IXINT(4,I)=IRECTM(2,CAND_M(I))
        IXINT(5,I)=IRECTS(1,CAND_S(I)) ! 1er noeud retenu arbitrairement
        IXINT(6,I)=11
      ENDDO
C
      NELEMINT=NELEMINT+I_STOK
C
      RETURN
      END
      
Chd|====================================================================
Chd|  I24NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|====================================================================
      SUBROUTINE I24NINTDD(NELEMINT,IXINT ,IRECT ,NSV,CAND_E,
     .                     CAND_N  ,I_STOK,IFIEND,X  ,GAP   ,
     .                     DRAD    ,GAP_S ,GAP_M ,DGAPLOAD)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEMINT, IXINT(6,*), IRECT(4,*),NSV(*),
     .   CAND_E(*),CAND_N(*),I_STOK ,IFIEND

      my_real
     .   GAP, X(3,*), GAP_M(*), GAP_S(*)
      my_real , INTENT(IN) :: DGAPLOAD , DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, L, I,IG,IX1,IX2,IX3,IX4,II
      my_real
     .   XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,SEUIL,
     .   XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4
C-----------------------------------------------
C      IFIEND = IFIEND + I_STOK*6
      CALL ZEROIN (1,I_STOK*6,IXINT)
C calcul du facteur de poids relatif
c      SEUIL = MAX(GAP +DGAPLOAD,DRAD)
      II = 0
      DO I = 1, I_STOK
          IG = NSV(CAND_N(I))
          L  = CAND_E(I)
          if (IG >NUMNOD) CYCLE
          II = II +1
          IXINT(1,II)=IRECT(1,L)
          IXINT(2,II)=IRECT(2,L)
          IXINT(3,II)=IRECT(3,L)
          IXINT(4,II)=IRECT(4,L)
          IXINT(5,II)=IG
          IXINT(6,II)=24
          IX1=IRECT(1,L)
          IX2=IRECT(2,L)
          IX3=IRECT(3,L)
          IX4=IRECT(4,L)
          ZI = X(3,IG)
          Z1=X(3,IX1)
          Z2=X(3,IX2)
          Z3=X(3,IX3)
          Z4=X(3,IX4)
          SEUIL=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))+DGAPLOAD
          ZMIN = MIN(Z1,Z2,Z3,Z4)-SEUIL
          ZMAX = MAX(Z1,Z2,Z3,Z4)+SEUIL
          IF (ZMIN.LE.ZI.AND.ZMAX.GE.ZI) THEN
            YI = X(2,IG)
            Y1 = X(2,IX1)
            Y2 = X(2,IX2)
            Y3 = X(2,IX3)
            Y4 = X(2,IX4)
            YMIN = MIN(Y1,Y2,Y3,Y4)-SEUIL
            YMAX = MAX(Y1,Y2,Y3,Y4)+SEUIL
            IF (YMIN.LE.YI.AND.YMAX.GE.YI) THEN
              XI = X(1,IG)
              X1 = X(1,IX1)
              X2 = X(1,IX2)
              X3 = X(1,IX3)
              X4 = X(1,IX4)
              XMIN = MIN(X1,X2,X3,X4)-SEUIL
              XMAX = MAX(X1,X2,X3,X4)+SEUIL
              IF (XMIN.LE.XI.AND.XMAX.GE.XI) THEN
                IXINT(6,II)=-24
              ENDIF
            ENDIF
          ENDIF          
      ENDDO
C
      IFIEND = IFIEND + II*6
      NELEMINT=NELEMINT+II
C
      RETURN
      END

      
Chd|====================================================================
Chd|  I25NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|====================================================================
      SUBROUTINE I25NINTDD(NELEMINT,IXINT ,IRECT ,NSV,CAND_E,
     .                     CAND_N  ,I_STOK,IFIEND,X  ,GAP   ,
     .                     DRAD    ,GAP_S ,GAP_M ,DGAPLOAD)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEMINT, IXINT(6,*), IRECT(4,*),NSV(*),
     .   CAND_E(*),CAND_N(*),I_STOK ,IFIEND

      my_real
     .   GAP, X(3,*), GAP_M(*), GAP_S(*) 
      my_real , INTENT(IN) :: DGAPLOAD , DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, L, I,IG,IX1,IX2,IX3,IX4,II
      my_real
     .   XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,SEUIL,
     .   XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4
C-----------------------------------------------
C      IFIEND = IFIEND + I_STOK*6
      CALL ZEROIN (1,I_STOK*6,IXINT)
C calcul du facteur de poids relatif
C      SEUIL = MAX(GAP+DGAPLOAD,DRAD)
      II = 0
      DO I = 1, I_STOK
          IG = NSV(CAND_N(I))
          L  = CAND_E(I)
          if (IG >NUMNOD) CYCLE
          II = II +1
          IXINT(1,II)=IRECT(1,L)
          IXINT(2,II)=IRECT(2,L)
          IXINT(3,II)=IRECT(3,L)
          IXINT(4,II)=IRECT(4,L)
          IXINT(5,II)=IG
          IXINT(6,II)=25
          IX1=IRECT(1,L)
          IX2=IRECT(2,L)
          IX3=IRECT(3,L)
          IX4=IRECT(4,L)
          ZI = X(3,IG)
          Z1=X(3,IX1)
          Z2=X(3,IX2)
          Z3=X(3,IX3)
          Z4=X(3,IX4)
          SEUIL=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))+DGAPLOAD
          ZMIN = MIN(Z1,Z2,Z3,Z4)-SEUIL
          ZMAX = MAX(Z1,Z2,Z3,Z4)+SEUIL
          IF (ZMIN.LE.ZI.AND.ZMAX.GE.ZI) THEN
            YI = X(2,IG)
            Y1 = X(2,IX1)
            Y2 = X(2,IX2)
            Y3 = X(2,IX3)
            Y4 = X(2,IX4)
            YMIN = MIN(Y1,Y2,Y3,Y4)-SEUIL
            YMAX = MAX(Y1,Y2,Y3,Y4)+SEUIL
            IF (YMIN.LE.YI.AND.YMAX.GE.YI) THEN
              XI = X(1,IG)
              X1 = X(1,IX1)
              X2 = X(1,IX2)
              X3 = X(1,IX3)
              X4 = X(1,IX4)
              XMIN = MIN(X1,X2,X3,X4)-SEUIL
              XMAX = MAX(X1,X2,X3,X4)+SEUIL
              IF (XMIN.LE.XI.AND.XMAX.GE.XI) THEN
                IXINT(6,II)=-25
              ENDIF
            ENDIF
          ENDIF          
      ENDDO
C
      IFIEND = IFIEND + II*6
      NELEMINT=NELEMINT+II
C
      RETURN
      END

Chd|====================================================================
Chd|  I2NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2NINTDD(NELEMINT,IXINT,IRECT,NSV,IRTL,
     .                    NSN,IFIEND)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEMINT, IXINT(6,*), IRECT(4,*),NSV(*),
     .   IRTL(*),NSN ,IFIEND, IFITMP, NN    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, L, I, JJ, NIR 
C-----------------------------------------------
C
      IFITMP = IFIEND + NSN*6
C
      NIR=2
      IF(N2D.EQ.0)NIR=4
C
      NN = 0
      DO N = 1, NSN
        I = NSV(N)
        L = IRTL(N)
        IF(L.NE.0)THEN         ! cas erreur segment non trouve
          NN = NN + 1
          IXINT(1,NN)=IRECT(1,L)
          IXINT(2,NN)=IRECT(2,L)
          IF (NIR.EQ.2) THEN
            IXINT(3,NN)=IRECT(1,L)
            IXINT(4,NN)=IRECT(2,L)
          ELSE
            IXINT(3,NN)=IRECT(3,L)
            IXINT(4,NN)=IRECT(4,L)                
          END IF
          IXINT(5,NN)=I
          IXINT(6,NN)=2
        END IF
      ENDDO
      NELEMINT = NELEMINT+NN
      IFIEND   = IFIEND + NN*6
C
      RETURN
      END
      
Chd|====================================================================
Chd|  SPDOMETIS                     source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE SPDOMETIS(KXSP, IXSP, NOD2SP, CEPSP, RESERVEP,
     .                     SPH2SOL, CEP)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "sphcom.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*), CEPSP(*),
     .        SPH2SOL(*), CEP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NEDGES, CC, N, N1, N2, NI, I, J, IW, ADDX,
     .        NUMSPHA, P, NCOND, NNODE, NEC, IERR1, MODE, NOD1, NOD2,
     .        IWFLG, NFLAG, NEWEDGE
      INTEGER IWD(NUMSPH), RESERVEP(NBPARTINLET),
     .        WORK(70000), OPTIONS(40), CEPSL(NUMSPH)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IEND, XADJ, ADJNCY,
     .                                     ITRIM,INDEXM, EDGE
      REAL    UBVEC(15)
C metis5 null pointers
      INTEGER, POINTER :: adjwgt(:)=>null(),vsize(:)=>null()
      REAL, POINTER :: tpwgts(:)=>null()
      INTEGER METIS_PartGraphKway, METIS_PartGraphRecursive,
     .        METIS_SetDefaultOptions,Wrap_METIS_PartGraphKway,
     .        Wrap_METIS_PartGraphRecursive
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL METIS_PartGraphKway, METIS_PartGraphRecursive,
     .         METIS_SetDefaultOptions,Wrap_METIS_PartGraphKway,
     .         Wrap_METIS_PartGraphRecursive
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C Connectivite cellules SPH
C
c NUMSPH ACTIVE less SPH2SOL/=0
      NUMSPHA = NUMSPH - NSPHRES - NSPHSOL

      NEDGES = 0
      DO N = 1, NUMSPHA
        DO CC = 1, MIN(12,KXSP(4,N))
          N2 = NOD2SP(IXSP(CC,N))
          IF (N/=N2)THEN
            IF ((N2 < FIRST_SPHSOL .OR. N2 >= FIRST_SPHSOL+NSPHSOL)) THEN
              NEDGES = NEDGES + 1
            END IF
          ENDIF
        END DO
      END DO
C

      IF (NEDGES.GT.0) THEN
        ALLOCATE(IEND(2*NEDGES))
        NI = 0
        DO N = 1, NUMSPHA
          DO CC = 1, MIN(12,KXSP(4,N))
            N2 = NOD2SP(IXSP(CC,N))
            IF (N/=N2)THEN
              IF((N2 < FIRST_SPHSOL .OR. N2 >= FIRST_SPHSOL+NSPHSOL)) THEN
                IF ( N < N2 ) THEN
                  NI = NI + 1
                  IEND(2*NI-1)=N
                  IEND(2*NI)=N2
                ELSE     
                  NI = NI + 1
                  IEND(2*NI-1)=N2
                  IEND(2*NI)=N
                END IF 
              END IF
            ENDIF
          END DO
        END DO
C
C METIS ADDITIONAL TREATMENT
C
        ALLOCATE(ITRIM(2*NEDGES),STAT=IERR1)
        ALLOCATE(INDEXM(2*NEDGES),STAT=IERR1)

        DO I = 1, NEDGES
          ITRIM(2*I-1) = IEND(2*I-1)
          ITRIM(2*I)   = IEND(2*I)
          INDEXM(I)    = I
        ENDDO
        MODE = 0
        CALL MY_ORDERS(MODE,WORK,ITRIM,INDEXM,NEDGES,2)

        DO I = 1, NEDGES
          IEND(2*I-1)= ITRIM(2*INDEXM(I)-1)
          IEND(2*I)  = ITRIM(2*INDEXM(I))
        ENDDO
 
C structures Metis 1/2
        ALLOCATE(XADJ(NUMSPHA+1),STAT=IERR1)
C init XADJ
        XADJ(1:NUMSPHA+1)=0
C first node
        I = 1
        NOD1 = IEND(2*I-1)
        NOD2 = IEND(2*I)
        NEWEDGE = 1
        xadj(nod1+1)=xadj(nod1+1)+1
        xadj(nod2+1)=xadj(nod2+1)+1
        DO I = 2, NEDGES
C test to suppress duplicate entry
          IF (NOD1.NE.IEND(2*I-1).OR.NOD2.NE.IEND(2*I)) THEN
            NEWEDGE = NEWEDGE + 1
            IEND(2*NEWEDGE-1) = IEND(2*I-1)
            IEND(2*NEWEDGE) = IEND(2*I)
            NOD1 = IEND(2*I-1)
            NOD2 = IEND(2*I)
C count xadj
            xadj(nod1+1)=xadj(nod1+1)+1
            xadj(nod2+1)=xadj(nod2+1)+1
          ENDIF
        ENDDO

        DEALLOCATE(ITRIM)
        DEALLOCATE(INDEXM)
C
        NEDGES = NEWEDGE

C structures Metis 2/2
        ALLOCATE(ADJNCY(2*NEDGES),STAT=IERR1)

C build xadj & adjcny in a simple pass

C compute XADJ addresses
        XADJ(1)=1
        DO I=1,NUMSPHA
          XADJ(I+1)=XADJ(I+1)+XADJ(I)
        END DO
C fill adjncy
        DO I=1,NEDGES
          NOD1=IEND(2*I-1)
          NOD2=IEND(2*I)
          ADDX=XADJ(NOD1)
          ADJNCY(ADDX)=NOD2
          XADJ(NOD1)=XADJ(NOD1)+1
          ADDX=XADJ(NOD2)
          ADJNCY(ADDX)=NOD1
          XADJ(NOD2)=XADJ(NOD2)+1
        END DO 
C reset XADJ
        DO I=NUMSPHA+1,2,-1
          XADJ(I)=XADJ(I-1)
        END DO
        XADJ(1)=1     
        DEALLOCATE(IEND)
      ENDIF
C----------------------    
C
C Init poids uniformes
c init for every SPH cells
C
      DO N = 1, NUMSPHA
        IWD(N) = 1
      END DO
C
      IWFLG=2
      NFLAG=1
C old metis option kept for compatibility
      OPTIONS(1)=0
      NCOND=1
      NNODE=NSPMD
      UBVEC(1)=1.01 ! tolerance on loadbalancing SPH cell
#if defined(METIS5)
C new Metis5 Definition
      IERR1 = METIS_SetDefaultOptions(options)
C     OPTIONS(METIS_OPTION_NUMBERING) = 1 ! Fortran numbering -- position 17 en 5.0.2 et 18 en 5.1
      OPTIONS(18)=1
#endif
C
C Proc attribution on NUMSPA cells
C
      IF (NEDGES > 0 .AND. NSPMD > 1) THEN
C KWAY METIS
        IF(DECTYP.EQ.3.OR.DECTYP.EQ.5)THEN
#if defined(METIS5)
          IERR1 = Wrap_METIS_PartGraphKway(
     1      NUMSPHA,NCOND,XADJ,ADJNCY,
     2      IWD,NNODE,
     3      UBVEC,OPTIONS,NEC,CEPSL)
#elif 1
          CALL METIS_mCPartGraphKway(
     1      NUMSPHA,NCOND,XADJ,ADJNCY,IWD,0,
     2      IWFLG,NFLAG,NNODE,UBVEC,OPTIONS,NEC,CEPSL)
#endif
        ELSEIF(DECTYP.EQ.4.OR.DECTYP.EQ.6)THEN
C RSB METIS
#if defined(METIS5)
          IERR1 = Wrap_METIS_PartGraphRecursive(
     1      NUMSPHA,NCOND,XADJ,ADJNCY,
     2      IWD,NNODE,
     3      UBVEC,OPTIONS,NEC,CEPSL)
#elif 1
          CALL METIS_mCPartGraphRecursive(
     1      NUMSPHA,NCOND,XADJ,ADJNCY,IWD,0,
     2      IWFLG,NFLAG,NNODE,OPTIONS,NEC,CEPSL)
#endif
        END IF
C
        DO N = 1, NUMSPHA
          CEPSP(N) = CEPSL(N)-1
        END DO
        DEALLOCATE(XADJ,ADJNCY)          
      ELSE IF (NSPMD == 1) THEN
        DO N = 1, NUMSPHA
          CEPSP(N) = 0
        END DO
      ELSE
C Could be improved by geometric domain decomposition
        DO N = 1, NUMSPHA
         CEPSP(N) = INT( (DBLE(N-1)/DBLE(NUMSPHA))*DBLE(NSPMD) )
         CEPSP(N) = MAX(0,MIN(CEPSP(N),NSPMD-1))
        END DO
      END IF
 
C Repartition by part
C for each part, KRESERV was saved, we put KRESERV RESERVE by proc by part
      N = FIRST_SPHRES     
 
      DO I = 1, NBPARTINLET
        DO P = 0, NSPMD-1
          DO J = 1, RESERVEP(I)
            CEPSP(N) = P
            N = N+1
          ENDDO
        ENDDO
      ENDDO
C
C SPH generated from solids are enforced on the same proc as the solid
      DO N = FIRST_SPHSOL, FIRST_SPHSOL+NSPHSOL-1
        CEPSP(N) = CEP(SPH2SOL(N))
      END DO
C
      WRITE(IOUT,'(A)')' '
      IF(DECTYP.EQ.3.OR.DECTYP.EQ.5)THEN
        WRITE(IOUT,'(A)')
     .  ' SPH DOMAIN DECOMPOSITION USING MULTILEVEL KWAY'
      ELSEIF(DECTYP.EQ.4.OR.DECTYP.EQ.6)THEN
        WRITE(IOUT,'(A)')
     .  ' SPH DOMAIN DECOMPOSITION USING MULTILEVEL RSB'
      END IF
      WRITE(IOUT,*)' '
      WRITE(IOUT,FMT=FMW_A_I)
     .  '  CELLS NUMBER = ',NUMSPH
      WRITE(IOUT,FMT=FMW_A_I)
     .  '  EDGES FOUND  = ',NEDGES
      WRITE(IOUT,*)' '
      WRITE(IOUT,*)'#PROC   ELT WEIGHT'
      DO I = 1, NSPMD
        IW = 0
        DO J = 1, NUMSPH
          IF (CEPSP(J)+1.EQ.I) THEN
            IW = IW + IWD(J)
          ENDIF
        ENDDO
        WRITE(IOUT,'(I4,I8)')I,IW
      END DO
      WRITE(IOUT,*)' '               
C
      RETURN
      END
      
C NOUVELLE ROUTINE
Chd|====================================================================
Chd|  INTERLAGRAN                   source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        INITWG_SHELL                  source/spmd/domain_decomposition/initwg_shell.F
Chd|        INITWG_TRI                    source/spmd/domain_decomposition/initwg_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERLAGRAN(TAB,LX,LTAB,X,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      REAL TAB(LTAB),LX(LTAB),X,Y
      INTEGER LTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      REAL MUL,ALPHA
      Y = 0

      IF (X.LE.10)THEN

        DO I=1,LTAB

        MUL = 1.
        DO J=1,LTAB
          IF (I.NE.J) THEN
            MUL= MUL * (X-LX(J))/(LX(I)-LX(J))
          ENDIF
        ENDDO
     
        Y = Y + TAB(I)*MUL     

        ENDDO
      ENDIF
      IF(X.GT.10.or.Y.LE.0)THEN
        Alpha = (TAB(3)-TAB(1))/(LX(3)-LX(1))
        Y = X*ALPHA + TAB(3)-ALPHA*LX(3)
      ENDIF
      END
C
Chd|====================================================================
Chd|  I2WCONTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2WCONTDD(NSV,MSR,NSN,NMN,IWCONT,NSNT,NMNT)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSV(*), MSR(*), IWCONT(2,*), NSN, NMN, NSNT, NMNT
      INTEGER :: COST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C-----------------------------------------------
      DO I = 1, NSN
        N = NSV(I)
        IWCONT(1,N) = IWCONT(1,N)+1
        NSNT = NSNT + 1
      ENDDO
C
      DO I = 1, NMN
        N = MSR(I)
C        IWCONT(1,N) = IWCONT(1,N)+1
        IWCONT(2,N) = IWCONT(2,N)+1
        NMNT = NMNT + 1
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IWCONTDD_NEW                  source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IWCONTDD_NEW(NSV,MSR,NSN,NMN,IWCONT,COST)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSV(*), MSR(*), IWCONT(5,*), NSN, NMN
      INTEGER :: COST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C-----------------------------------------------
      DO I = 1, NSN
        N = NSV(I)
        IWCONT(3,N) = IWCONT(3,N)+COST
      ENDDO
C
      DO I = 1, NMN
        N = MSR(I)
        IWCONT(4,N) = IWCONT(4,N)+COST
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  IWCONTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IWCONTDD(NSV,MSR,NSN,NMN,IWCONT,NSNT,NMNT)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSV(*), MSR(*), IWCONT(5,*), NSN, NMN, NSNT, NMNT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C-----------------------------------------------
      DO I = 1, NSN
        N = NSV(I)
        IWCONT(1,N) = IWCONT(1,N)+1
        NSNT = NSNT + 1
      ENDDO
C
      DO I = 1, NMN
        N = MSR(I)
C        IWCONT(1,N) = IWCONT(1,N)+1
        IWCONT(2,N) = IWCONT(2,N)+1
        NMNT = NMNT + 1
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I20WCONTDD                    source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20WCONTDD(NSV,MSR,NSN,NMN,IWCONT,NSNT,NMNT)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSV(*), MSR(*), IWCONT(5,*), NSN, NMN, NSNT, NMNT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C-----------------------------------------------
      DO I = 1, NSN
        N = NSV(I)
        IWCONT(1,N) = IWCONT(1,N)+2
        NSNT = NSNT + 1
      ENDDO
C
      DO I = 1, NMN
        N = MSR(I)
C        IWCONT(1,N) = IWCONT(1,N)+2
        IWCONT(2,N) = IWCONT(2,N)+2
        NMNT = NMNT + 1
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DD_BFS                        source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DD_BFS(XADJ,ADJNCY,NELEM,NEDGES,NCONNX,COLORS,ROOTS)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELEM, NEDGES, NCONNX,
     .        XADJ(NELEM+1), ADJNCY(2*NEDGES),
     .        COLORS(NELEM), ROOTS(NELEM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVISIT, N, I
      INTEGER FILE_V(NELEM), FILE_NEXT, ROOT, CURRENT
C-----------------------------------------------
      
      DO N = 1, NELEM
        COLORS(N)=0
      END DO
      NVISIT=0
      ROOT=1 ! first element of the graph == first vertex available
      NCONNX=0
      
      DO WHILE (NVISIT < NELEM) ! loop until all vertices are visited
        NCONNX = NCONNX+1
        DO WHILE ((ROOT <= NELEM) .AND. (COLORS(ROOT) /= 0))
          ROOT = ROOT + 1
        END DO
c        IF (ROOT > NELEM) THEN
c          print*,'** FATAL ERROR DURING BFS'
c          NCONNX=-1
c          EXIT
c        END IF
        ROOTS(NCONNX)=ROOT ! record roots for fatest treatments
        FILE_V(1)=ROOT
        FILE_NEXT=2 ! new file initialized with root
        COLORS(ROOT)=NCONNX ! root marked
        NVISIT=NVISIT+1
        DO WHILE (FILE_NEXT > 1) ! test file not nill
          CURRENT = FILE_V(FILE_NEXT-1)
          FILE_NEXT = FILE_NEXT-1
          DO N = XADJ(CURRENT), XADJ(CURRENT+1)-1
            I = ADJNCY(N)
            IF(COLORS(I) == 0) THEN ! vertex not treated before
              FILE_V(FILE_NEXT)=I
              FILE_NEXT = FILE_NEXT+1
              COLORS(I) = NCONNX
              NVISIT=NVISIT+1
            END IF
          END DO            
        END DO
      END DO
      
      RETURN
      END

Chd|====================================================================
Chd|  PRELEC_DDW                    source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRELEC_DDW(FILNAM,LEN_FILNAM,MARQUEUR3)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      LOGICAL MARQUEUR3
C Dynamical User Library
      CHARACTER FILNAM*512
      INTEGER LEN_FILNAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MARQUEUR,MARQUEUR2
C-----------------------------------------------     
      FILNAM  =ROOTNAM(1:ROOTLEN)//'_0001.ddw'
      LEN_FILNAM=LEN_TRIM(FILNAM)         
      LINE=' '
      MARQUEUR2 = 0
      TEST_POIDS = 0
      INQUIRE(FILE=FILNAM(1:LEN_FILNAM), EXIST=MARQUEUR3)

      IF(MARQUEUR3) THEN
         TEST_POIDS = 1
C        Nombre de lignes
         OPEN(UNIT=30,FILE=FILNAM(1:LEN_FILNAM),FORM='FORMATTED')
         DO WHILE(LINE(1:12) /= '     POINTER')
          MARQUEUR2=MARQUEUR2+1
          READ(30,FMT='(A)')LINE
         ENDDO
         CLOSE(UNIT=30)
         MARQUEUR2 = MARQUEUR2 - 3

         MARQUEUR = 0       
C        Prelecture des poids couple Mat/Prop
         OPEN(UNIT=30,FILE=FILNAM(1:LEN_FILNAM),FORM='FORMATTED',
     .     POSITION='REWIND')
         LINE = ' '
         DO WHILE(LINE(1:12) /= '    LAW_NUMB')
          MARQUEUR=MARQUEUR+1
          READ(30,FMT='(A)')LINE
         ENDDO
         CLOSE(UNIT=30)
         MARQUEUR = MARQUEUR + 1
         TAILLE_OLD= MARQUEUR2 - MARQUEUR
      ELSE
         NUMMAT_OLD = 0
         NUMGEO_OLD = 0
         TAILLE_OLD = 0
      ENDIF   
       RETURN
       END
Chd|====================================================================
Chd|  LEC_DDW                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LEC_DDW(FILNAM,LEN_FILNAM,TAB_UMP_OLD,CPUTIME_MP_OLD)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(7,TAILLE_OLD) :: TAB_UMP_OLD
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD
C Dynamical User Library
      CHARACTER FILNAM*512
      INTEGER LEN_FILNAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
C-----------------------------------------------       
C     Lecture des poids couple Mat/Prop
      OPEN(UNIT=30,FILE=FILNAM(1:LEN_FILNAM),FORM='FORMATTED')
        LINE = ' '
       DO WHILE(LINE(1:12) /= '    LAW_NUMB')
          READ(30,FMT='(A)')LINE
       ENDDO
       DO J=1,TAILLE_OLD
         READ(30,'(I12,A,I12,A,I12,A,
     .            I12,A,I12,A,I12,A,
     .            I12,A,E15.5)')
     .   TAB_UMP_OLD(6,J),LINE(1:2),TAB_UMP_OLD(7,J),LINE(1:2),TAB_UMP_OLD(5,J),LINE(1:2),
     .   TAB_UMP_OLD(3,J),LINE(1:2),TAB_UMP_OLD(1,J),LINE(1:2),TAB_UMP_OLD(4,J),LINE(1:2),
     .   TAB_UMP_OLD(2,J),LINE(1:2),CPUTIME_MP_OLD(J)

       ENDDO
      CLOSE(UNIT=30)
      RETURN
      END
Chd|====================================================================
Chd|  PRELEC_DDW_POIN               source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRELEC_DDW_POIN(FILNAM,LEN_FILNAM)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C Dynamical User Library
      CHARACTER FILNAM*512
      INTEGER LEN_FILNAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------   
C     Lecture du pointer
      OPEN(UNIT=30,FILE=FILNAM(1:LEN_FILNAM),FORM='FORMATTED')
         LINE = ' '
         DO WHILE(LINE(1:12) /= '     POINTER')
          READ(30,FMT='(A)')LINE
         ENDDO
         READ(30,'(A,I10)') LINE(1:47),NUMMAT_OLD
         READ(30,'(A,I10)') LINE(1:47),NUMGEO_OLD   
      CLOSE(UNIT=30)
      RETURN
      END 
Chd|====================================================================
Chd|  LEC_DDW_POIN                  source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LEC_DDW_POIN(FILNAM,LEN_FILNAM,POIN_UMP_OLD)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C Dynamical User Library
      CHARACTER FILNAM*512
      INTEGER LEN_FILNAM
      INTEGER, DIMENSION(NUMMAT_OLD) :: POIN_UMP_OLD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------   
      INTEGER I
C-----------------------------------------------   
C     Lecture du pointer 
      OPEN(UNIT=30,FILE=FILNAM(1:LEN_FILNAM),FORM='FORMATTED')
         LINE = ' '
         DO WHILE(LINE(1:12) /= '     POINTER')
          READ(30,FMT='(A)')LINE
         ENDDO
         READ(30,'(A,I10)') LINE(1:47),NUMMAT_OLD
         READ(30,'(A,I10)') LINE(1:47),NUMGEO_OLD
         DO I=1,NUMMAT_OLD
          READ(30,'(I8)')   POIN_UMP_OLD(I)
         ENDDO     
      CLOSE(UNIT=30)
      RETURN
      END      
Chd|====================================================================
Chd|  REINI_MATPROP                 source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REINI_MATPROP(TAILLE,TAILLE2,TAB_UMP_LOC,TAB_UMP_LOC2,
     .           IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,ISOLNOD,POIN_UMP)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAILLE,TAILLE2
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7+6,TAILLE2,2) :: TAB_UMP_LOC2
      INTEGER, DIMENSION(5,NPART) :: TAB_UMP_LOC
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),ISOLNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MARQUEUR,K1,K2,K3,K4,
     .        I,J,ITY,TEST
C-----------------------------------------------
      TAB_UMP_LOC2 = 0
      IF(NUMELS>0) THEN
       ITY = 1
       DO I=1,NUMELS
         K1 = IXS(1,I)
         K2 = IXS(10,I)
         K3 = POIN_UMP(K1)! + POIN_UMP(NUMMAT+K2)
         MARQUEUR = ISOLNOD(I)
         K4 = 0
         IF(MARQUEUR==4) THEN
          K4 = 1
         ELSEIF(MARQUEUR==6) THEN
          K4 = 2
         ELSEIF(MARQUEUR==8) THEN
          K4 = 3
         ELSEIF(MARQUEUR==10) THEN
          K4 = 4
         ELSEIF(MARQUEUR==16) THEN
          K4 = 5
         ELSEIF(MARQUEUR==20) THEN
          K4 = 6
         ENDIF
C
         IF(K3/=0) THEN
           TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(3,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
             TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
             TAB_UMP_LOC2(ITY+K4,K3,2) = 1
             TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
         ENDIF
       ENDDO
      ENDIF
      
!      stop
      IF(NUMELQ>0) THEN
       K4 = 6
       ITY = 2
       DO I =1,NUMELQ      
         K1 = IXQ(1,I)
         K2 = IXQ(6,I)
         K3 = 1!POIN_UMP(K1) !+ POIN_UMP(NUMMAT+K2)
         IF(K3/=0) THEN
           TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(3,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
             TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
             TAB_UMP_LOC2(ITY+K4,K3,2) = 1
             TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
         ENDIF
       ENDDO
      ENDIF
      IF(NUMELC>0) THEN
         K4 = 6
         ITY = 3
         DO I=1,NUMELC         
          K1 = IXC(1,I)
          K2 = IXC(6,I)
          K3 = POIN_UMP(K1)! + POIN_UMP(NUMMAT+K2)
          IF(K3/=0) THEN
            TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(3,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
             TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
             TAB_UMP_LOC2(ITY+K4,K3,2) = 1
             TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
          ENDIF
         ENDDO
      ENDIF
      IF(NUMELT>0) THEN
         K4 = 6
         ITY = 4
         DO I=1,NUMELT         
           K1 = IXT(1,I)
           K2 = IXT(4,I)
           K3 = POIN_UMP(K1)! + POIN_UMP(NUMMAT+K2)
           IF(K3/=0) THEN
            TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(3,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
             TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
             TAB_UMP_LOC2(ITY+K4,K3,2) = 1
             TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
           ENDIF
         ENDDO
      ENDIF
      IF(NUMELP>0) THEN
         K4 = 6
         ITY = 5
         DO I=1,NUMELP         
          K1 = IXP(1,I)
          K2 = IXP(5,I)
          K3 = 1!POIN_UMP(K1)! + POIN_UMP(NUMMAT+K2)
          IF(K3/=0) THEN
            TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(3,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
            TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
            TAB_UMP_LOC2(ITY+K4,K3,2) = 1
            TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
           ENDIF
         ENDDO
      ENDIF
      IF(NUMELR>0) THEN
         K4 = 6         
         K1 = 0
         ITY = 6
         DO I=1,NUMELR
          K2 = IXR(1,I)
          K3 = K1 + 1
          IF(K3/=0) THEN
            TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(1,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
            TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
            TAB_UMP_LOC2(ITY+K4,K3,2) = 1
            TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
           ENDIF
         ENDDO
      ENDIF
      IF(NUMELTG>0) THEN       
         K4 = 6
         ITY = 7
         DO I=1,NUMELTG         
          K1 = IXTG(1,I)
          K2 = IXTG(5,I)
          K3 = 1!POIN_UMP(K1) !+ POIN_UMP(NUMMAT+K2)
          IF(K3/=0) THEN
            TEST=0
           DO WHILE((K3<=TAILLE2).AND.(TEST==0))
            IF((TAB_UMP_LOC(3,K3)==K1).AND.(TAB_UMP_LOC(4,K3)==K2)) THEN
            TAB_UMP_LOC2(ITY+K4,K3,1) = TAB_UMP_LOC2(ITY+K4,K3,1) + 1
            TAB_UMP_LOC2(ITY+K4,K3,2) = 1
            TEST=1
            ELSE
             K3=K3+1
            ENDIF
           ENDDO
          ENDIF
         ENDDO
      ENDIF

      TAILLE = 0
      DO J=1,TAILLE2
       MARQUEUR = 0
       DO I=1,13
         IF(TAB_UMP_LOC2(I,J,2)>0) THEN
          MARQUEUR = MARQUEUR + 1
         ENDIF
       ENDDO
       TAILLE = TAILLE + MARQUEUR
      ENDDO

      RETURN
      END
      
Chd|====================================================================
Chd|  REINI_MATPROP2                source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REINI_MATPROP2(TAILLE,TAILLE2,
     .           TAB_UMP_LOC,TAB_UMP_LOC2,TAB_UMP,TAB_SOL,
     .           POIN_UMP)
C-----------------------------------------------
C            A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAILLE,TAILLE2,MARQUEUR2
      INTEGER, DIMENSION(7+6,TAILLE2,2) :: TAB_UMP_LOC2
      INTEGER, DIMENSION(6) :: TAB_SOL
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      INTEGER, DIMENSION(5,NPART) :: TAB_UMP_LOC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MARQUEUR,I,J,K
C-----------------------------------------------
    
      TAB_SOL(1) = 1004
      TAB_SOL(2) = 1006
      TAB_SOL(3) = 1008
      TAB_SOL(4) = 1010
      TAB_SOL(5) = 1016
      TAB_SOL(6) = 1020
      MARQUEUR = 1
      DO J=1,TAILLE2
       DO I=1,13
       IF(TAB_UMP_LOC2(I,J,2)>0) THEN
         DO K=1,4
          TAB_UMP(K,MARQUEUR) = TAB_UMP_LOC(K,J)
         ENDDO
         TAB_UMP(5,MARQUEUR)   = TAB_UMP_LOC2(I,J,1)
         TAB_UMP(6,MARQUEUR)   = TAB_UMP_LOC(5,J)
         IF(I>7) THEN
          TAB_UMP(7,MARQUEUR)   = I-6
         ELSEIF(I==1) THEN
          TAB_UMP(7,MARQUEUR)   = I
         ELSE
          TAB_UMP(7,MARQUEUR)   = TAB_SOL(I-1)
         ENDIF         
         MARQUEUR = MARQUEUR + 1 
       ENDIF
       ENDDO       
      ENDDO
      
      POIN_UMP(TAB_UMP(3,1)) = 1
      DO I=2,TAILLE
         IF(TAB_UMP(3,I-1)/=TAB_UMP(3,I)) THEN
          POIN_UMP(TAB_UMP(3,I)) = I
        ENDIF
      ENDDO

      RETURN
      END                    
Chd|====================================================================
Chd|  STAT_DOMDEC                   source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  STAT_DOMDEC(WIS   ,WI2     ,WFSI      ,WDEL   ,WDDL     ,
     2                        WCAND ,WSOL    ,WR2R      ,WKIN   ,IWD      ,
     3                        NCOND ,ICELEM  ,ICINTS    ,ICINT2 ,ICCAND   ,
     4                        ICDDL ,ICSOL   ,ICFSI     ,ICDEL  ,ICR2R    ,
     5                        ICKIN ,AVERAGE ,DEVIATION ,DMAX   ,DMIN     ,
     6                        CEP   ,NELEM   ,W         ,ICINTM ,WIM      ,
     7                        NCRITMAX ,WNOD_SMS,ICNOD_SMS)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
           INTEGER ICKIN,ICR2R,ICDEL,ICFSI,ICSOL,ICDDL,ICCAND,
     .             ICINTS,ICINTM,ICINT2,NCOND,NELEM,ICELEM,NCRITMAX,ICNOD_SMS,
     .             CEP(NELEM),IWD(NCOND*NELEM)
           DOUBLE PRECISION AVERAGE(NCRITMAX), DEVIATION(NCRITMAX), DMIN(NCRITMAX),DMAX(NCRITMAX),
     .                      W(NSPMD), WIS(NSPMD), WI2(NSPMD), WDDL(NSPMD),
     .                      WFSI(NSPMD),WCAND(NSPMD),WSOL(NSPMD),WKIN(NSPMD),
     .                      WDEL(NSPMD), WR2R(NSPMD), WIM(NSPMD),WNOD_SMS(NSPMD)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
           INTEGER  I,J
C----------------------------------------------

            DO I = 1, NSPMD
              W(I) = ZERO
              WIS(I) = ZERO
              WIM(I) = ZERO
              WI2(I) = ZERO
              WFSI(I) = ZERO
              WDEL(I) = ZERO
              WDDL(I) = ZERO
              WCAND(I) = ZERO
              WSOL(I) = ZERO
              WR2R(I) = ZERO
              WKIN(I) = ZERO
              WNOD_SMS(I) = ZERO
            END DO
            DO J = 1, NELEM
              I = CEP(J)
              W(I) = W(I) + IWD(NCOND*(J-1)+ICELEM)
              IF(ICINTS/=0)WIS(I) = WIS(I) + IWD(NCOND*(J-1)+ICINTS)
              IF(ICINTM/=0)WIM(I) = WIM(I) + IWD(NCOND*(J-1)+ICINTM)
              IF(ICINT2/=0)WI2(I) = WI2(I) + IWD(NCOND*(J-1)+ICINT2)
              IF(ICCAND/=0)WCAND(I) = WCAND(I) + IWD(NCOND*(J-1)+ICCAND)
              IF(ICDDL/=0)WDDL(I) = WDDL(I) + IWD(NCOND*(J-1)+ICDDL)
              IF(ICSOL/=0)WSOL(I) = WSOL(I) + IWD(NCOND*(J-1)+ICSOL)
              IF(ICFSI/=0)WFSI(I) = WFSI(I) + IWD(NCOND*(J-1)+ICFSI)
              IF(ICDEL/=0)WDEL(I) = WDEL(I) + IWD(NCOND*(J-1)+ICDEL)
              IF(ICR2R/=0)WR2R(I) = WR2R(I) + IWD(NCOND*(J-1)+ICR2R)
              IF(ICKIN/=0)WKIN(I) = WKIN(I) + IWD(NCOND*(J-1)+ICKIN)
              IF(ICNOD_SMS/=0)WNOD_SMS(I) = WNOD_SMS(I) + IWD(NCOND*(J-1)+ICNOD_SMS)
            ENDDO
C        
C compute Average and Standard deviation   
C
            DO I=1,NCRITMAX
              AVERAGE(I)=ZERO
              DEVIATION(I)=ZERO
              DMAX(I)=ZERO
              DMIN(I)=2147483647
            END DO
            DO I = 1, NSPMD
              AVERAGE(1)=AVERAGE(1)+W(I)
              AVERAGE(2)=AVERAGE(2)+WIS(I)
              AVERAGE(3)=AVERAGE(3)+WI2(I)
              AVERAGE(4)=AVERAGE(4)+WCAND(I)
              AVERAGE(5)=AVERAGE(5)+WDDL(I)
              AVERAGE(6)=AVERAGE(6)+WSOL(I)
              AVERAGE(7)=AVERAGE(7)+WFSI(I)
              AVERAGE(8)=AVERAGE(8)+WDEL(I)
              AVERAGE(9)=AVERAGE(9)+WR2R(I)
              AVERAGE(10)=AVERAGE(10)+WKIN(I)
              AVERAGE(11)=AVERAGE(11)+WIM(I)
              AVERAGE(12)=AVERAGE(12)+WNOD_SMS(I)
              DMIN(1)=MIN(DMIN(1),W(I))
              DMIN(2)=MIN(DMIN(2),WIS(I))
              DMIN(3)=MIN(DMIN(3),WI2(I))
              DMIN(4)=MIN(DMIN(4),WCAND(I))
              DMIN(5)=MIN(DMIN(5),WDDL(I))
              DMIN(6)=MIN(DMIN(6),WSOL(I))
              DMIN(7)=MIN(DMIN(7),WFSI(I))
              DMIN(8)=MIN(DMIN(8),WDEL(I))
              DMIN(9)=MIN(DMIN(9),WR2R(I))
              DMIN(10)=MIN(DMIN(10),WKIN(I))
              DMIN(11)=MIN(DMIN(11),WIM(I))
              DMIN(12)=MIN(DMIN(12),WNOD_SMS(I))
              DMAX(1)=MAX(DMAX(1),W(I))
              DMAX(2)=MAX(DMAX(2),WIS(I))
              DMAX(3)=MAX(DMAX(3),WI2(I))
              DMAX(4)=MAX(DMAX(4),WCAND(I))
              DMAX(5)=MAX(DMAX(5),WDDL(I))
              DMAX(6)=MAX(DMAX(6),WSOL(I))
              DMAX(7)=MAX(DMAX(7),WFSI(I))
              DMAX(8)=MAX(DMAX(8),WDEL(I))
              DMAX(9)=MAX(DMAX(9),WR2R(I))
              DMAX(10)=MAX(DMAX(10),WKIN(I))
              DMAX(11)=MAX(DMAX(11),WIM(I))
              DMAX(12)=MAX(DMAX(12),WNOD_SMS(I))
            END DO
            DO I=1,NCRITMAX
              AVERAGE(I)=AVERAGE(I)/NSPMD
            END DO
            DO I = 1, NSPMD
              DEVIATION(1)=DEVIATION(1)+(W(I)     -AVERAGE(1))**2
              DEVIATION(2)=DEVIATION(2)+(WIS(I)    -AVERAGE(2))**2
              DEVIATION(3)=DEVIATION(3)+(WI2(I)   -AVERAGE(3))**2
              DEVIATION(4)=DEVIATION(4)+(WCAND(I) -AVERAGE(4))**2
              DEVIATION(5)=DEVIATION(5)+(WDDL(I)  -AVERAGE(5))**2
              DEVIATION(6)=DEVIATION(6)+(WSOL(I)  -AVERAGE(6))**2
              DEVIATION(7)=DEVIATION(7)+(WFSI(I)  -AVERAGE(7))**2
              DEVIATION(8)=DEVIATION(8)+(WDEL(I)  -AVERAGE(8))**2
              DEVIATION(9)=DEVIATION(9)+(WR2R(I)  -AVERAGE(9))**2
              DEVIATION(10)=DEVIATION(10)+(WKIN(I)-AVERAGE(10))**2
              DEVIATION(11)=DEVIATION(11)+(WIM(I)-AVERAGE(11))**2
              DEVIATION(12)=DEVIATION(12)+(WNOD_SMS(I)-AVERAGE(12))**2
            END DO      
            DO I=1,NCRITMAX
              DEVIATION(I)=SQRT(DEVIATION(I)/NSPMD)
            END DO
        END
Chd|====================================================================
Chd|  FIND_NODES                    source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FIND_NODES(ELEMN0,ELEMNODES,TAGELEM,IXS, IXS10 ,
     2                      IXQ   ,IXC      ,IXT    ,IXP, IXR   , 
     3                      IXTG  ,IXUR     ,KXX    ,IXX, KXIG3D,
     4                      IXIG3D,GEO      ,OFFELEM,NELMIN)
C-----------------------------------------------
C            M o d u l e s
C-----------------------------------------------
     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr12_c.inc"
#include      "rnur_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr15_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "sms_c.inc"
#include      "r2r_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        KXX(NIXX,NUMELX),IXX(*),IXS10(6,*),KXIG3D(NIXIG3D,NUMELIG3D),
     .        IXIG3D(*),ELEMNODES(20),TAGELEM(*),OFFELEM(10)
      INTEGER ELEMN0,NELMIN
      my_real  GEO(NPROPG,*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------------------------------
      INTEGER K,N,ADDX,NELX,J,NELIG3D,I,ELEM
C---------------------------------------------
      NELMIN = 3
      DO I=1,20 
        ELEMNODES(I)=0
      ENDDO

      ELEM = ELEMN0
      SELECT CASE(TAGELEM(ELEM))

      CASE(1)
        DO K=1,8
          N = IXS(K+1,ELEM)
          ELEMNODES(K)= N 
        ENDDO
      CASE(2)
! Tetra 10 nodes
        ELEMNODES(1) = IXS(2,ELEM)
        ELEMNODES(2) = IXS(6,ELEM)
        ELEMNODES(3) = IXS(4,ELEM)
        ELEMNODES(4) = IXS(7,ELEM)
      CASE(3)
        NELMIN = 1
        ELEM = ELEM - OFFELEM(1)
        DO K=1,4
          N = IXQ(K+1,ELEM)
          ELEMNODES(K) = N
        ENDDO
      CASE(4)
        NELMIN = 2 
        DO I=1,2
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        DO K=1,4
          N = IXC(K+1,ELEM) 
          ELEMNODES(K) = N 
        ENDDO
      CASE(5)
        NELMIN = 1
        DO I=1,3
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        DO K=1,2
          N = IXT(K+1,ELEM)
          ELEMNODES(K) = N
        ENDDO
      CASE(6)
        NELMIN = 1
        DO I=1,4
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        DO K=1,2
          N = IXP(K+1,ELEM)
          ELEMNODES(K) = N
        ENDDO
      CASE(7)
        NELMIN = 1
        DO I=1,5
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        DO K=1,2
          N = IXR(K+1,ELEM)
          ELEMNODES(K) = N
        ENDDO
        IF(NINT(GEO(12,IXR(1,ELEM))).EQ.12) THEN
          N = IXR(4,ELEM)
          ELEMNODES(3) = N
        ENDIF
      CASE(8)
        NELMIN = 2
        DO I=1,6
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        DO K=1,3
          N = IXTG(K+1,ELEM)
          ELEMNODES(K) = N
        ENDDO
      CASE(9)
        NELMIN = 1
        DO I=1,7
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        DO K=1,4
          N = IXUR(K+1,ELEM)
          ELEMNODES(K) = N
        ENDDO
      CASE(10)
        NELMIN = 1
        DO I=1,8
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        NELX=KXX(3,ELEM)
        DO K=1,NELX
          ADDX = KXX(4,ELEM)+K-1
          N=IXX(ADDX)
          ELEMNODES(K) = N
        ENDDO
      CASE(11) 
        DO I=1,9
           ELEM = ELEM - OFFELEM(I)
        ENDDO
        NELIG3D=KXIG3D(3,ELEM)
        DO K=1,NELIG3D
          ADDX = KXIG3D(4,ELEM)+K-1
          N=IXIG3D(ADDX)
          ELEMNODES(K) = N
        ENDDO
      END SELECT
C     TRI ELEMNODES + UNIQ 

      DO K=2,20 
      DO I=1,K-1
         IF(ELEMNODES(K) == ELEMNODES(I)) ELEMNODES(K) = 0
      ENDDO
      ENDDO

      END








Chd|====================================================================
Chd|  FVBAG_VERTEX                  source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|-- calls ---------------
Chd|        FVELSURF                      source/airbag/fvelsurf.F      
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE FVBAG_VERTEX(IXC   ,IXTG    , NELEM, WD,
     .                        WD_MAX,FVM_ELEM, FVM_DOMDEC, ITAB, 
     .     IGRSURF, T_MONVOL)
C Description: computes a weight for each FVMBAG
C this weight is added to an element of the skin of the airbag
C PMAIN will be the processor in charge of this element 
C FVM_DOMDEC set to .TRUE. if an FVMBAGS contains less than NUMNOD/2
C nodes. Specific domain decomposition parameters will be used in that
C case
C-----------------------------------------------
C            M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr12_c.inc"
#include      "rnur_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr15_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER, INTENT(IN) :: IXC(NIXC,*), IXTG(NIXTG,*)
       INTEGER,INTENT(IN) :: NELEM
       INTEGER, INTENT(INOUT) :: FVM_ELEM(NVOLU)
       DOUBLE PRECISION ,INTENT(INOUT) :: WD_MAX ! maximum weight for fvmbags
       REAL,INTENT(INOUT) :: WD(*) ! weights
       LOGICAL, INTENT(OUT) :: FVM_DOMDEC 
       INTEGER,INTENT(IN) :: ITAB(*)
       TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
       TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------------------------------
       LOGICAL, DIMENSION(:), ALLOCATABLE :: TAGGED_ELEM
       INTEGER, DIMENSION(:), ALLOCATABLE :: ELTG,MATTG
       INTEGER :: I,J,K,N,K0,K1,K2,K6
       INTEGER :: ITYP,S
       INTEGER :: OFFC,OFFTG
       INTEGER :: NNS,NTG,NNI,NTGI,NNO,NN,NTGT
       INTEGER :: RADALE, IBID
       
C---------------------------------------------
C                                
C---------------------------------------------


C WD_MAX is the maximum weight of the super elt
C In order to be able to compute a partition
          OFFC = NUMELS+NUMELQ
          OFFTG =NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR
          I = 0
          DO N = 1, NVOLU 
             ITYP = T_MONVOL(N)%TYPE
             NN = T_MONVOL(N)%NNS
             IF(NN > 0 .AND. (ITYP == 8 .OR. ITYP==6)) THEN
                IF(2 * NN < NUMNOD) FVM_DOMDEC = .TRUE. 
                I = I + 1
             ENDIF              
          ENDDO                 ! 1,NVOLU

          WD_MAX = 0.0d0
          DO N = 1,NELEM
             WD_MAX = WD_MAX + (1.0D0*WD(N)) / (1.0D0 * NSPMD)
          ENDDO
C================================================================================
C Arbitrary: limit the weight of one FVMBAG to 25% of the weight of one subdoamin
          WD_MAX = WD_MAX / 4.0D0
C================================================================================
                    
          IF(I > 0)  WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION OPTIMIZED FOR FVMBAGS  '

          ALLOCATE(TAGGED_ELEM(NELEM))
          OFFC =  NUMELS+NUMELQ
          OFFTG = NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR
          DO N = 1, NVOLU 
            TAGGED_ELEM(1:NELEM) = .FALSE.
            ITYP = T_MONVOL(N)%TYPE
            NN = T_MONVOL(N)%NNS

            IF(NN > 0 .AND. (ITYP == 8 .OR. ITYP==6)) THEN
              ! Tag elements of the fvmbag
               NNS  =  T_MONVOL(N)%NNS
               NTG  =  T_MONVOL(N)%NTG
               NNI  =  T_MONVOL(N)%NNI
               NTGI =  T_MONVOL(N)%NTGI
               NNO=3
               NTGT=NTG+NTGI

               ALLOCATE(ELTG(NTGT))
               ALLOCATE(MATTG(NTGT))               
               
               CALL FVELSURF(
     .              T_MONVOL(N)%NODES, T_MONVOL(N)%ELEM, IBID, IXC, IXTG, NTGT,
     .              ELTG, MATTG, MAX(NUMNOD, NB_TOTAL_NODE), .FALSE.)
                               
              IF(NTGT > 1) THEN
                ! The first element of the FVMBAG will become the super-element
                ! i.e. the element that holds the weight of the finite volumes
                J = ELTG(1) ! ELTG 
            
                K0 = 0
                IF ( J<= NUMELC) THEN
                  K0 =  J - NUMELQ
                  K0 = OFFC +K0
                ELSE                     
                  K0 = (J-NUMELC-NUMELQ)
                  K0 = OFFTG+ K0

                ENDIF

                FVM_ELEM(N) = K0

                IF(K0 == 0) THEN

                ELSE
                  TAGGED_ELEM(K0) = .TRUE.
                  DO I=2,NTGT
                     J = ELTG(I)! ELTG 
                     IF (J.LE.NUMELC) THEN
                       K = OFFC + J
                         ! The weight of the super element is incremented
                        IF(.NOT.(TAGGED_ELEM(K))) THEN
                         ! the weight of each element is doubled
c                        CALL IDDCONNECTPLUS(K,K0,NELEM)
                         WD(K0) = WD(K0) +1.0d0* WD(K)
                         TAGGED_ELEM(K) = .TRUE.
                       ENDIF
                     ELSEIF (J.GT.NUMELC) THEN
                       K = OFFTG+ (J-NUMELC)
                         ! The weight of the super element is incremented
                       IF(.NOT.(TAGGED_ELEM(K))) THEN
                         WD(K0) = WD(K0) + 1.0d0*WD(K)
                         TAGGED_ELEM(K) = .TRUE.
                       ENDIF
                     ENDIF
                  ENDDO !2,NTGT
                ENDIF
              ENDIF ! NTGT > 0

              IF(WD(FVM_ELEM(N)) > WD_MAX) THEN
                 WD(FVM_ELEM(N)) = WD_MAX
              ENDIF
              DEALLOCATE(ELTG)
              DEALLOCATE(MATTG)
            ENDIF 
          ENDDO ! 1,NVOLU
          DEALLOCATE(TAGGED_ELEM)

      RETURN
      END
