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
Chd|  SGRHEAD                       source/elements/solid/solide/sgrhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE SGRHEAD(
     1       IXS        ,PM            ,GEO       ,INUM    ,ISEL   ,
     2       ITR1       ,EADD          ,INDEX     ,ITRI    ,IPARTS ,      
     3       ND         ,IGRSURF       ,IGRBRIC   ,ISOLNOD ,
     4       CEP        ,XEP           ,IXS10     ,IXS20   ,IXS16  ,
     5       IGEO       ,IPM           ,NOD2ELS   ,ISOLOFF ,
     6       TAGPRT_SMS ,SPH2SOL       ,SOL2SPH   ,MAT_PARAM,
     7       SOL2SPH_TYP,IFLAG_BPRELOAD,CLUSTERS  ,RNOISE)
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXS(11,NUMELS)       ARRAY: CONECS+PID+MID+NOS SOLIDS          E
C     PM(NPROPM,NUMMAT)    ARRAY: MATERIAL PARAMETERS (real)         E
C     IPM(NPROPMI,NUMMAT)  ARRAY: MATERIAL PARAMETERS (integer)      E
C     GEO(NPROPG,NUMGEO)   ARRAY: PROPERTY PARAMETERS (real)         E
C     IGEO(NPROPGI,NUMGEO) ARRAY: PROPERTY PARAMETERS (integer)      E
C     INUM(13,NUMELS)      ARRAY: WORKING                            E/S
C     ISEL(NSELS)          ARRAY: DES SOLIDES CHOISIS POUR TH        E/S
C     ITR1(NSELS)          ARRAY: WORKING                            E/S
C     EADD(NUMELS)         ARRAY: IDAM INDEXES / checkboard            S
C     INDEX(NUMELS)        ARRAY: WORKING                            E/S
C     ITRI(6,NUMELS)       ARRAY: WORKING                            E/S
C     IPARTS(NUMELS)       ARRAY: PART                               E/S
C     CEP(NUMELS)          ARRAY: WORKING                            E/S
C     XEP(NUMELS)          ARRAY: WORKING                            E/S
C     NOD2ELS(8*NUMELS+6*NUMELS10+12*NUMELS20+16*NUMELS16)           E/S
C     ISOLOFF(NUMELS)      FLAG ELEM RBY ON/OFF                      E/S
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE CLUSTER_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr21_c.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS),ISEL(*),INUM(16,*),IPARTS(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(7,*),
     .        ND, ISOLNOD(*), CEP(*),
     .        XEP(*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .        NOD2ELS(*), ISOLOFF(*),
     .        TAGPRT_SMS(*), SPH2SOL(*), 
     .        SOL2SPH(2,*),SOL2SPH_TYP(*),IFLAG_BPRELOAD(*)
      INTEGER,INTENT(IN) :: IPM(NPROPMI,NUMMAT)
      INTEGER,INTENT(INOUT) :: IGEO(NPROPGI,NUMGEO)
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO)
      my_real,INTENT(INOUT) ::  RNOISE(NPERTURB,NUMELS)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
      TYPE(MATPARAM_STRUCT_) , TARGET, DIMENSION(NUMMAT),INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .        I,J,K,L,IL,MLN, NG, ISSN, NPN, NN, N, MID, PID ,IREP,
     .        II,II0,JJ0,II1,JJ1,II2,JJ2,JJ,II3,JJ3,II4,JJ4,II5,JJ5,
     .        JHBE,ISO,IGT,IINT,MODE,IEOS,IVISC,IVISC0,TSHELL, 
     .        IPLAST, IALEL,MT,NFAIL,NFAIL0,ITET4,ICPRE,ICSTR,IRB ,
     .        NLAY,NPTR,NPTS,NPTT,IMAT,INUM_R2R(1+R2R_SIU*NUMELS),
     .        NSPHDIR,IPARTSPH,NUVAR,ISVIS,IBOLTP,ITET10,NLOC_FAIL,
     .        IPERT,STAT
      INTEGER WORK(70000)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER ID, JALE_FROM_MAT, JALE_FROM_PROP
      CHARACTER*nchartitle :: TITR
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2
      INTEGER :: CLUSTER_TYP,CLUSTER_NEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_CLUSTER
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
C-----------------------------------------------
C   S O U R C E   L I N E S
C-----------------------------------------------

C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
  
      CALL MY_ALLOC(INDEX2,NUMELS) 
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELS),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ENDIF
C
      DO I=1,NUMELS
        INDEX2(I)=PERMUTATION%SOLID(I)
        EADD(I)=1
Clef 5---------------------------------
        ITRI(5,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTS(I)
        INUM(2,I)=IXS(1,I)
        INUM(3,I)=IXS(2,I)
        INUM(4,I)=IXS(3,I)
        INUM(5,I)=IXS(4,I)
        INUM(6,I)=IXS(5,I)
        INUM(7,I)=IXS(6,I)
        INUM(8,I)=IXS(7,I)
        INUM(9,I)=IXS(8,I)
        INUM(10,I)=IXS(9,I)
        INUM(11,I)=IXS(10,I)
        INUM(12,I)=IXS(11,I)
        INUM(13,I)=ISOLNOD(I)
        IF (NSUBDOM>0) INUM_R2R(I) = TAG_ELSF(I)
        IF (NPERTURB > 0) THEN
          DO IPERT = 1, NPERTURB
            XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I) 
          ENDDO
        ENDIF
      ENDDO
      IF(NSPHSOL /= 0)THEN
        DO I=1,NUMELS8
          INUM(14,I)=SOL2SPH(1,I)
          INUM(15,I)=SOL2SPH(2,I)
          INUM(16,I)=SOL2SPH_TYP(I)
        ENDDO
      END IF
C
      IF (IMACH==3) THEN
        DO I=1,NUMELS
          XEP(I)=CEP(I)
        ENDDO
      ENDIF
C
      DO I = 1, NUMELS
        II = I
        NPN = 1
        JHBE=IHBE_D
        JPOR=0
        MID = IXS(1,II)
        MLN = NINT(PM(19,ABS(MID)))
        IF (MID < 0) THEN
          IF (MLN==6.AND.JPOR/=2) MLN=17
          IF (MLN==46) MLN=47
          MID = IABS(MID)
        ENDIF
        IF (MLN == 36 .or. MLN == 47) THEN
          NUVAR = IPM(8,MID)
        ELSE
          NUVAR = 0   ! no matter what
        ENDIF
        PID= IXS(10,II)
        ISO= ISOLNOD(II)
        IPLAST=IPLA_DS-1
        ICPRE = 0
        ICSTR = 0
        IREP = 0
        ISTRAIN = 0        
        NFAIL0    = MAT_PARAM(MID)%NFAIL
        NLOC_FAIL = MAT_PARAM(MID)%NLOC
        IEOS = 0
        IVISC0 = 0
        NLAY  = 1
        TSHELL = 0
        ISVIS = 0
        IF (PID/=0) THEN
          NPN  = IGEO(4,PID)
          ISSN = IABS(IGEO(5,PID))
          IREP = IGEO(6,PID)
          JHBE = IGEO(10,PID)
          IGT  = IGEO(11,PID)
          ISTRAIN = IGEO(12,PID)
          ICPRE  = IABS(IGEO(13,PID))     
          ICSTR  = IGEO(14,PID)    
          IINT = IGEO(15,PID)
          JCVT = IABS(IGEO(16,PID))
          ITET4  = IGEO(20,PID)
          ITET10 = IGEO(50,PID)
          IF (IGT == 22) THEN                   
            NLAY = IGEO(30,PID)                 
            DO IL=1,NLAY                        
              IMAT = IGEO(100+IL,PID)           
              NFAIL0 = MAX(NFAIL0,MAT_PARAM(IMAT)%NFAIL)
              IF (MAT_PARAM(IMAT)%IVISC > 0) IVISC0 = 1
            ENDDO
          ELSEIF (MAT_PARAM(MID)%IVISC > 0) THEN
            IVISC0 = 1
          ENDIF
C IVISCO0 cannot put in the PID because is for MID (it can be used in IGEO)
          IGEO(34,PID) = IVISC0
c----------            
          IF (IGT /= 15) IPLAST = IGEO(9,PID)
          IF (IGT==15) JPOR=2*NINT(GEO(28,PID))
          JCLOS=0
          IF (GEO(130,PID)>0.) JCLOS=1
C--------------Navier Storks Vis          
          IF (GEO(16,PID)/=ZERO.OR.GEO(17,PID)/=ZERO) ISVIS=1
        ENDIF
        IF((JHBE == 14 .OR. JHBE == 222).AND.ISO==8) NUMELS8A=NUMELS8A+1
        IF (JHBE == 12)  JHBE = 4
        IF (JHBE==2)   JHBE = 0
c        
        JALE_FROM_MAT = NINT(PM(72,MID))
        JALE_FROM_PROP = IGEO(62,PID)
        JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader
        JLAG=0
        IF(JALE == 0 .AND. MLN /= 18)JLAG=1
        JEUL=0
        IF(JALE==2)THEN
          JALE=0
          JEUL=1
        ELSEIF(JALE == 3) THEN
          JLAG = 1
          JALE = 1
        ENDIF
        IF(MLN/=50)JTUR=NINT(PM(70,MID))
        JTHE=NINT(PM(71,MID))
        IF (JLAG==0 .AND. PID/=0) ISSN=4
C tri sur elem delete des rigidbody
C IRB = 0 : elem actif
C IRB = 1 : elem inactif et optimise pour en SPMD
C IRB = 2 : elem inactif mais optimise pour etre actif en SPMD
        IRB = ISOLOFF(I)
C
        JSMS=0
        IF(ISMS/=0)THEN
          IF(IDTGRS/=0)THEN
            IF(TAGPRT_SMS(IPARTS(II))/=0)JSMS=1
          ELSE
            JSMS=1
          END IF
        END IF
        IEOS  = IPM(4,MID)
C
        NSPHDIR =IGEO(37,PID)
        IPARTSPH=IGEO(38,PID)
        IGEO(35,PID) = ISVIS
C Bolt preloading flag
        IBOLTP = 0
        IF(NPRELOAD > 0)THEN
          IBOLTP = IFLAG_BPRELOAD(II)
        ENDIF
        
C
Clef 1---------------------------------
C
C     classer les shell16 apres les brick20
        IF(ISO==16)ISO=21
C       IGT =MY_SHIFTL(IGT,0)
        JSMS=MY_SHIFTL(JSMS,26)
        ISO =MY_SHIFTL(ISO,27)
C       attention ISO doit rester en poids le plus fort
        ITRI(1,I)=ISO+JSMS+IGT
Clef 2---------------------------------
C       IPARTSPH=MY_SHIFTL(IPARTSPH,0)
        ITRI(2,I)=IPARTSPH ! must remain alone for this key (part index)
Clef 3---------------------------------
C       JTHE=MY_SHIFTL(JTHE,0)
        JTUR=MY_SHIFTL(JTUR,1)
        JEUL=MY_SHIFTL(JEUL,2)
        JLAG=MY_SHIFTL(JLAG,3)
        JALE=MY_SHIFTL(JALE,4)
        ISSN=MY_SHIFTL(ISSN,5)
        JHBE=MY_SHIFTL(JHBE,9)
        JPOR=MY_SHIFTL(JPOR,13)
        IRB=MY_SHIFTL(IRB,18)
        MLN =MY_SHIFTL(MLN,22)
        ITRI(3,I)=MLN+JHBE+ISSN+JALE+JLAG+JEUL+JTUR+JTHE+JPOR+IRB
C
Clef 4---------------------------------
C       JCLOS=MY_SHIFTL(JCLOS,0)
        NPN   =MY_SHIFTL(NPN,3)
        IPLAST=MY_SHIFTL(IPLAST,13)
        ICPRE =MY_SHIFTL(ICPRE,16)
        ICSTR =ICSTR/100+2*MOD(ICSTR/10,10)+4*MOD(ICSTR,10)
        ICSTR =MY_SHIFTL(ICSTR,18)
        IREP=MY_SHIFTL(IREP,20)
        JCVT=MY_SHIFTL(JCVT,22)
        IINT=MY_SHIFTL(IINT,24)
        ISTRAIN=MY_SHIFTL(ISTRAIN,26)
        ITET4=MY_SHIFTL(ITET4,27)
        NFAIL  =  MY_SHIFTL(NFAIL0,29) 
C       next=MY_SHIFTL(next,32) ---> pas de place disp
        ITRI(4,I)=JCLOS+NPN+IPLAST+ICPRE+ICSTR+IREP+IINT+JCVT+ISTRAIN
     .           +ITET4+NFAIL
Clef 5---------------------------------
        ITRI(5,I)=MID
Clef 6---------------------------------
        ITRI(6,I)=PID
Clef 7---------------------------------
        IEOS      = MY_SHIFTL(IEOS,0) 
        IVISC     = MY_SHIFTL(IVISC0,4)
        NUVAR     = MY_SHIFTL(NUVAR,5) 
        ISVIS     = MY_SHIFTL(ISVIS,15) 
        IBOLTP    = MY_SHIFTL(IBOLTP,16)
        ITET10    = MY_SHIFTL(ITET10,17)
        NLOC_FAIL = MY_SHIFTL(NLOC_FAIL,19)
C       next  = MY_SHIFTL(next,21)
        ITRI(7,I)=IEOS+IVISC+NUVAR+ISVIS+IBOLTP+ITET10+NLOC_FAIL
c        
      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELS , 7)
C
      DO I=1,NUMELS
        IPARTS(I) =INUM(1,INDEX(I))
        ISOLNOD(I)=INUM(13,INDEX(I))
        IF (NSUBDOM>0) TAG_ELSF(I)=INUM_R2R(INDEX(I))
        IF (NPERTURB > 0) THEN
          DO IPERT = 1, NPERTURB
            RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
          ENDDO
        ENDIF
      ENDDO
c     IF (IMACH==3) THEN
        DO I=1,NUMELS
          CEP(I)=XEP(INDEX(I))
          PERMUTATION%SOLID(I)=INDEX2(INDEX(I))
        ENDDO
c     ENDIF
      DO  K=1,11
        DO  I=1,NUMELS
          IXS(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO
C
C ISOLOFF
C
      DO I = 1, NUMELS
        INUM(3,I) = ISOLOFF(I)
      END DO
C
      DO I = 1, NUMELS
        ISOLOFF(I) = INUM(3,INDEX(I))
      END DO
C
C BOLT_PRELOAD
C
      IF (NPRELOAD > 0) THEN
       DO I = 1, NUMELS
         INUM(4,I) = IFLAG_BPRELOAD(I)
       END DO
C
       DO I = 1, NUMELS
         IFLAG_BPRELOAD(I) = INUM(4,INDEX(I))
       END DO
      ENDIF

C
      IF (NUMELS10+NUMELS20+NUMELS16 > 0) THEN
        DO I = 1, NUMELS10
          II = I + NUMELS8
          INUM(1,II)=IXS10(1,I)
          INUM(2,II)=IXS10(2,I)
          INUM(3,II)=IXS10(3,I)
          INUM(4,II)=IXS10(4,I)
          INUM(5,II)=IXS10(5,I)
          INUM(6,II)=IXS10(6,I)
        ENDDO
C
        DO I = 1, NUMELS10
          II = I + NUMELS8
          IXS10(1,I)=INUM(1,INDEX(II))
          IXS10(2,I)=INUM(2,INDEX(II))
          IXS10(3,I)=INUM(3,INDEX(II))
          IXS10(4,I)=INUM(4,INDEX(II))
          IXS10(5,I)=INUM(5,INDEX(II))
          IXS10(6,I)=INUM(6,INDEX(II))
        ENDDO
C
        DO I = 1, NUMELS20
          II = I + NUMELS8 + NUMELS10
          INUM(1,II) =IXS20(1,I)
          INUM(2,II) =IXS20(2,I)
          INUM(3,II) =IXS20(3,I)
          INUM(4,II) =IXS20(4,I)
          INUM(5,II) =IXS20(5,I)
          INUM(6,II) =IXS20(6,I)
          INUM(7,II) =IXS20(7,I)
          INUM(8,II) =IXS20(8,I)
          INUM(9,II) =IXS20(9,I)
          INUM(10,II)=IXS20(10,I)
          INUM(11,II)=IXS20(11,I)
          INUM(12,II)=IXS20(12,I)
        ENDDO
C
        DO I = 1, NUMELS20
          II = I + NUMELS8 + NUMELS10
          IXS20(1,I)=INUM(1,INDEX(II))
          IXS20(2,I)=INUM(2,INDEX(II))
          IXS20(3,I)=INUM(3,INDEX(II))
          IXS20(4,I)=INUM(4,INDEX(II))
          IXS20(5,I)=INUM(5,INDEX(II))
          IXS20(6,I)=INUM(6,INDEX(II))
          IXS20(7,I)=INUM(7,INDEX(II))
          IXS20(8,I)=INUM(8,INDEX(II))
          IXS20(9,I)=INUM(9,INDEX(II))
          IXS20(10,I)=INUM(10,INDEX(II))
          IXS20(11,I)=INUM(11,INDEX(II))
          IXS20(12,I)=INUM(12,INDEX(II))
        ENDDO
C
        DO I = 1, NUMELS16
          II = I + NUMELS8 + NUMELS10 + NUMELS20
          INUM(1,II) =IXS16(1,I)
          INUM(2,II) =IXS16(2,I)
          INUM(3,II) =IXS16(3,I)
          INUM(4,II) =IXS16(4,I)
          INUM(5,II) =IXS16(5,I)
          INUM(6,II) =IXS16(6,I)
          INUM(7,II) =IXS16(7,I)
          INUM(8,II) =IXS16(8,I)
        ENDDO
C
        DO I = 1, NUMELS16
          II = I + NUMELS8 + NUMELS10 + NUMELS20
          IXS16(1,I)=INUM(1,INDEX(II))
          IXS16(2,I)=INUM(2,INDEX(II))
          IXS16(3,I)=INUM(3,INDEX(II))
          IXS16(4,I)=INUM(4,INDEX(II))
          IXS16(5,I)=INUM(5,INDEX(II))
          IXS16(6,I)=INUM(6,INDEX(II))
          IXS16(7,I)=INUM(7,INDEX(II))
          IXS16(8,I)=INUM(8,INDEX(II))
        ENDDO
C      
      ENDIF
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELS
        ITR1(INDEX(I))=I
      ENDDO
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF (IGRSURF(I)%ELTYP(J) == 1)
     .        IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SOLIDES
C
      DO I=1,NGRBRIC
        NN=IGRBRIC(I)%NENTITY
        DO J=1,NN
          IGRBRIC(I)%ENTITY(J) = ITR1(IGRBRIC(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR SPH CONVERSION
C
      IF(NSPHSOL /= 0)THEN
        DO I=1,NUMSPH
          IF(SPH2SOL(I) /= 0)SPH2SOL(I)=ITR1(SPH2SOL(I))
        ENDDO
C
C rebuild SOL2SPH
        DO I=1,NUMELS8
          SOL2SPH(1,I)=INUM(14,INDEX(I))
          SOL2SPH(2,I)=INUM(15,INDEX(I))
          SOL2SPH_TYP(I)=INUM(16,INDEX(I))
        END DO
      END IF
C
C renumerotation CONNECTIVITE INVERSE
C
      DO I=1,8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16
        IF(NOD2ELS(I) /= 0)NOD2ELS(I)=ITR1(NOD2ELS(I))
      END DO

!   -----------------------
!   reordering for cluster typ=1 (solid cluster)
      DO I=1,NCLUSTER
        CLUSTER_TYP = CLUSTERS(I)%TYPE
        IF(CLUSTER_TYP==1) THEN
            CLUSTER_NEL = CLUSTERS(I)%NEL
            ALLOCATE( SAVE_CLUSTER( CLUSTER_NEL ) )
            SAVE_CLUSTER( 1:CLUSTER_NEL ) = CLUSTERS(I)%ELEM( 1:CLUSTER_NEL )
            DO J=1,CLUSTER_NEL
                CLUSTERS(I)%ELEM(J) = ITR1( SAVE_CLUSTER( J ) )
            ENDDO
            DEALLOCATE( SAVE_CLUSTER )
        ENDIF
      ENDDO
!   -----------------------
C
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELS
        II0=ITRI(1,INDEX(I))
        JJ0=ITRI(1,INDEX(I-1))
        II=ITRI(2,INDEX(I))
        JJ=ITRI(2,INDEX(I-1))
        II1=ITRI(3,INDEX(I))
        JJ1=ITRI(3,INDEX(I-1))
        II2=ITRI(4,INDEX(I))
        JJ2=ITRI(4,INDEX(I-1))
        II3=ITRI(5,INDEX(I))
        JJ3=ITRI(5,INDEX(I-1))
        II4=ITRI(6,INDEX(I))
        JJ4=ITRI(6,INDEX(I-1))
        II5=ITRI(7,INDEX(I))
        JJ5=ITRI(7,INDEX(I-1))
        IF(II0/=JJ0.OR.II/=JJ.OR.II1/=JJ1.OR.II2/=JJ2.OR.
     .     II5/=JJ5.OR.II3/=JJ3.OR.II4/=JJ4) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELS+1
      DEALLOCATE(INDEX2)
c
      IF (NPERTURB > 0) THEN
        IF (ALLOCATED(XNUM_RNOISE)) DEALLOCATE(XNUM_RNOISE) 
      ENDIF
C-----------
      RETURN
      END
