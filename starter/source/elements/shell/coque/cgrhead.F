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
Chd|  CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CGRHEAD(
     1       IXC     ,PM      ,GEO       ,INUM   ,ISEL   ,
     2       ITR1    ,EADD    ,INDEX     ,ITRI   ,XNUM   ,
     3       IPARTC  ,ND      ,THK       ,IGRSURF,IGRSH4N,
     4       CEP     ,XEP     ,IGEO      ,IPM    ,
     5       IPART   ,SH4TREE ,NOD2ELC   ,ISHEOFF,SH4TRIM,
     6       TAGPRT_SMS, LGAUGE,IWORKSH,
     7       STACK   ,DRAPE    ,RNOISE ,SH4ANG)
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXC(NIXC,NUMELC)  TABLEAU MID(1)+CONECS(2-5)+PID(6)+         E
C                       N GLOBAL(7)                               E
C     PM(NPROPM,NUMMAT) TABLEAU DES CARACS DES MATERIAUX           E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                 E
C     INUM(9,NUMELC)    TABLEAU DE TRAVAIL                         E/S
C     ISEL(NSELC)       TABLEAU DES COQUES CHOISIES POUR TH        E/S
C     ITR1(NSELC)       TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELC)      TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELC)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(7,NUMELC)    TABLEAU DE TRAVAIL                         E/S
C     IPARTC(NUMELC)    TABLEAU PART                               E/S
C     CEP(NUMELC)    TABLEAU PROC                                  E/S
C     XEP(NUMELC)    TABLEAU PROC                                  E/S
C     NOD2ELC(4*NUMELC)                                            E/S
C     ISHEOFF(NUMELC)   FLAG ELEM RBY ON/OFF                       E/S
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE STACK_MOD
      USE MESSAGE_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "scr03_c.inc"
#include      "remesh_c.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "drape_c.inc"
#include       "my_allocate.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*),ISEL(*),INUM(9,*),IPARTC(*), ISHEOFF(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(7,*),
     .        ND, CEP(*), XEP(*),
     .        IGEO(NPROPGI,*),IPM(NPROPMI,*),IPART(LIPART1,*), 
     .        SH4TREE(KSH4TREE,*), NOD2ELC(*), SH4TRIM(*),
     .        TAGPRT_SMS(*) ,LGAUGE(3,*),
     .        IWORKSH(3,*)
C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*),XNUM(*),THK(*), RNOISE(NPERTURB,*),
     .    SH4ANG(*)
C-----------------------------------------------
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)    :: DRAPE (*)
      TYPE (DRAPE_) , DIMENSION(:), ALLOCATABLE :: XNUM_DRAPE
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL)  :: IGRSH4N
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISTOR,INUM_DRAPE
      INTEGER WORK(70000)
      INTEGER I, K,  MLN, NG, ISSN, NPN, IFIO, NN,L,IGTYP,
     .  MLN0, ISSN0, IC, N, MID, MID0, PID, PID0, ISTR0,
     .  IHBE, IHBE0, II, J, MIDN, PIDN, NSG, NEL, NE1,
     .  ITHK, ITHK0, IPLA, IPLA0,II1,JJ1,II2,JJ2,JJ,II3,JJ3,NGROU,
     .  MSKMLN,MSKNPN,MSKIHB,MSKISN,MSKIRB,MODE,ICSEN,IRB,
     .  MSKIST,MSKIPL,MSKITH,MSKMID,MSKPID,MSKIRP,MSKTYP,IREP,
     .  IPT,IMATLY,II0,JJ0,ILEV,PRT,IADM,DIR,II4,JJ4,N1,
     .  NFAIL,IFAIL,IXFEM,INUM_R2R(1+R2R_SIU*NUMELC),
     .  INUM_WORKC(3,NUMELC),II5,JJ5,
     .  ISUBSTACK,IIGEO,IADI ,IPPID,NB_LAW58,IPMAT,
     .  IPERT,STAT
      my_real
     .  ANGLE(NUMELC)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND,IPIDL
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2
C     REAL OU REAL*8
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Clef 1---------------------------------
      DATA MSKMLN /O'07770000000'/
      DATA MSKTYP /O'00007770000'/
      DATA MSKIHB /O'00000007000'/
      DATA MSKISN /O'00000000700'/
      DATA MSKIST /O'00000000070'/
      DATA MSKIPL /O'00000000007'/
Clef 2---------------------------------
      DATA MSKITH /O'10000000000'/
      DATA MSKIRP /O'07000000000'/
      DATA MSKNPN /O'00777000000'/
C     DATA MSKICS /O'10000000000'/
C     DATA MSKJTH /O'00000000000'/
C     DATA MSKIEX /O'00000000000'/
      DATA MSKIRB /O'00000000007'/
Clef 3---------------------------------
      DATA MSKMID /O'07777777777'/
Clef 4---------------------------------
      DATA MSKPID /O'07777777777'/
C======================================================================|
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
      IF(NADMESH /= 0)THEN
        ALLOCATE( ISTOR(KSH4TREE+1,NUMELC) )
      ELSE
        ALLOCATE( ISTOR(0,0) )
      ENDIF
      IF (NDRAPE > 0) THEN
        ALLOCATE( XNUM_DRAPE(NUMELC))
        DO I =1, NUMELC
           NPT = DRAPE(I)%NPT
           ALLOCATE(XNUM_DRAPE(I)%RDRAPE(NPT,2),XNUM_DRAPE(I)%IDRAPE(NPT))
           XNUM_DRAPE(I)%RDRAPE = 0
           XNUM_DRAPE(I)%IDRAPE = 0
        ENDDO 
      ELSE
        ALLOCATE( XNUM_DRAPE(0) )
      ENDIF
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELC),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ENDIF
C    
      MY_ALLOCATE(INDEX2,NUMELC)

      DO I=1,NUMELC
        INDEX2(I)=PERMUTATION%SHELL(I)
        EADD(I)=1
        ITRI(7,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTC(I)
        INUM(2,I)=ISHEOFF(I)
        INUM(3,I)=IXC(1,I)
        INUM(4,I)=IXC(2,I)
        INUM(5,I)=IXC(3,I)
        INUM(6,I)=IXC(4,I)
        INUM(7,I)=IXC(5,I)
        INUM(8,I)=IXC(6,I)
        INUM(9,I)=IXC(7,I)
        XNUM(I)=THK(I)
        IF (NSUBDOM>0) INUM_R2R(I) = TAG_ELCF(I)
        INUM_WORKC(1,I) = IWORKSH(1,I)
        INUM_WORKC(2,I) = IWORKSH(2,I)
        INUM_WORKC(3,I) = IWORKSH(3,I)
        IF (NDRAPE > 0) THEN
          NPT = DRAPE(I)%NPT
          XNUM_DRAPE(I)%NPT = DRAPE(I)%NPT
          DO IPT = 1, NPT
            XNUM_DRAPE(I)%IDRAPE(IPT)  =  DRAPE(I)%IDRAPE(IPT)
            XNUM_DRAPE(I)%RDRAPE(IPT,1) = DRAPE(I)%RDRAPE(IPT,1)
            XNUM_DRAPE(I)%RDRAPE(IPT,2) = DRAPE(I)%RDRAPE(IPT,2)
          ENDDO
        ENDIF
        IF (NPERTURB > 0) THEN
          DO IPERT = 1, NPERTURB
            XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I) 
          ENDDO
        ENDIF
        ANGLE(I)=SH4ANG(I) 
      ENDDO
C
      IF(NADMESH /= 0)THEN
        DO  K=1,KSH4TREE
          DO  I=1,NUMELC
            ISTOR(K,I)=SH4TREE(K,I)
          ENDDO
        ENDDO
        IF(LSH4TRIM/=0)THEN
          DO  I=1,NUMELC
            ISTOR(KSH4TREE+1,I)=SH4TRIM(I)
          ENDDO
        END IF
      END IF
C
      DO I=1,NUMELC
        XEP(I)=CEP(I)
      ENDDO
C      
      DO I = 1, NUMELC
        II = I
C
        IF(NADMESH == 0)THEN
          ITRI(1,I)=0
        ELSE
C
C         ILEV doit etre de poids fort sur 1ere clef
          PRT = IPARTC(II)
          IADM= IPART(10,PRT)
          IF(IADM==0)THEN
C          not the same group as if adaptivity.
            ITRI(1,I)=0
          ELSE
            ILEV= SH4TREE(3,I)
            IF(ILEV<0)ILEV=-ILEV-1
            ITRI(1,I)=ILEV+1
          END IF
        END IF
C
        MID= IXC(1,II)
        PID= IXC(6,II)
        MLN  = NINT(PM(19,MID))
        IGTYP= IGEO(11,PID)
        JTHE = NINT(PM(71,MID))
        NPN  = IGEO(4,PID)
        IHBE = NINT(GEO(171,PID))
        ITHK = NINT(GEO(35,PID))
        IPLA = NINT(GEO(39,PID))
        IREP = IGEO(6,PID)
        ISHXFEM_PLY = IGEO(19,PID)
        NFAIL = 0
        IFAIL = 0
        IXFEM = 0
        IF (IGTYP == 11) THEN
          DO IPT = 1, NPN
            IMATLY  = IGEO(100+IPT,PID)
            NFAIL = MAX(NFAIL,IPM(220, IMATLY))
          ENDDO 
          IF(ICRACK3D > 0)THEN
C-          new multilayer -
            IXFEM = IPM(236,MID)
          ENDIF
        ELSEIF(IGTYP == 17) THEN
          NPN = IWORKSH(1,II)
          ISUBSTACK =IWORKSH(3, II)
!!          IIGEO   =  40 + 5*(ISUBSTACK - 1) 
!!          IADI    = IGEO(IIGEO + 3,PID)
!!          IPPID   = IADI    
          IPPID = 2     
          DO IPT = 1, NPN
!!            IPIDL = IGEO(IPPID+IPT,PID)
            IPIDL = STACK%IGEO(IPPID  + IPT  ,ISUBSTACK)
            IMATLY  = IGEO(101,IPIDL)
            NFAIL = MAX(NFAIL,IPM(220, IMATLY))
          ENDDO   
        ELSEIF(IGTYP == 51 ) THEN
C---
C new shell property (variable NPT through each layer)
C---
          NB_LAW58 = 0
          NPN = IWORKSH(1,II)
          ISUBSTACK = IWORKSH(3, II)
          IPPID = 2
          DO IPT = 1,NPN  ! nb of plys
            IPIDL  = STACK%IGEO(IPPID + IPT,ISUBSTACK)
            IMATLY = IGEO(101,IPIDL)
            NFAIL  = MAX(NFAIL,IPM(220,IMATLY))
C --- PID 51 combined with LAW58 ---
            IF (NINT(PM(19,IMATLY)) == 58) NB_LAW58 = NB_LAW58 + 1
          ENDDO
C --- set IREP for tri criteria:
          IF (NB_LAW58 == NPN) THEN
            IREP = 2
          ELSEIF (NB_LAW58 > 0) THEN
            IREP = IREP + 3
          ENDIF
        ELSEIF(IGTYP == 52) THEN
C---
C new shell property (PCOMPP + STACK + PLY )
C---
          NB_LAW58 = 0
          NPN = IWORKSH(1,II)
          ISUBSTACK = IWORKSH(3, II)
          IPPID = 2
          IPMAT = IPPID + NPN 
          DO IPT = 1,NPN  ! nb of plys
            IPIDL  = STACK%IGEO(IPPID + IPT,ISUBSTACK)
            IMATLY = STACK%IGEO(IPMAT + IPT,ISUBSTACK)
            NFAIL  = MAX(NFAIL,IPM(220,IMATLY))
C --- PID 51 combined with LAW58 ---
            IF (NINT(PM(19,IMATLY)) == 58) NB_LAW58 = NB_LAW58 + 1
          ENDDO
C --- set IREP for tri criteria:
          IF (NB_LAW58 == NPN) THEN
            IREP = 2
          ELSEIF (NB_LAW58 > 0) THEN
            IREP = IREP + 3
          ENDIF           
C
        ELSE ! IGTYP == 1
          NFAIL = IPM(220, MID)
          IF(ICRACK3D > 0)THEN
C -         new monolayer -
            IXFEM = IPM(236,MID)
            IF (IXFEM == 1) THEN
              IXFEM = 2
              ICRACK3D = IXFEM
            ENDIF
          END IF
        ENDIF
        IF (NFAIl > 0) IFAIL = 1
c        
C thermal material expansion
        IEXPAN  = IPM(218, MID)        
        ICSEN= IGEO(3,PID)
        IF (ICSEN > 0) ICSEN=1
        IF(NPN == 0.AND.(MLN == 36.OR.MLN == 86))THEN
          IF(IPLA == 0) IPLA=1
          IF(IPLA == 2) IPLA=0
C         IF(IPLA == 3) IPLA=2
        ELSEIF(NPN == 0.AND.MLN == 2)THEN
          IF(IPLA == 2) IPLA=0
        ELSE
         IF(IPLA == 2) IPLA=0
         IF(IPLA == 3) IPLA=2
        ENDIF
        IF(ITHK == 2)THEN
          ITHK = 0
        ELSEIF(MLN == 32)THEN
          ITHK = 1
        ENDIF
        IPLA = IABS(IPLA)
        ITHK = IABS(ITHK)
        ISTRAIN = NINT(GEO(11,PID))
        IF(MLN == 19.OR.MLN>=25.OR.MLN == 15)ISTRAIN = 1
        ISSN = IABS(NINT(GEO(3,PID)))
C tri sur elem delete des rigidbody
C IRB = 0 : elem actif
C IRB = 1 : elem inactif et optimise pour en SPMD
C IRB = 2 : elem inactif mais optimise pour etre actif en SPMD
        IRB = ISHEOFF(I)
C
C---     Clef2 
        JSMS = 0
        IF(ISMS/=0)THEN
          IF(IDTGRS/=0)THEN
            IF(TAGPRT_SMS(IPARTC(II))/=0)JSMS=1
          ELSE
            JSMS=1
          END IF
        END IF
C       JSMS=MY_SHIFTL(JSMS,0)
        ITRI(2,I) = JSMS
C       NEXT=MY_SHIFTL(NEXT,1)
C
C---     Clef3 
C       IPLA   = MY_SHIFTL(IPLA,0)
        ISTRAIN= MY_SHIFTL(ISTRAIN,3)
        ISSN   = MY_SHIFTL(ISSN,6)
        IHBE   = MY_SHIFTL(IHBE,9)
        IGTYP  = MY_SHIFTL(IGTYP,12)
        MLN    = MY_SHIFTL(MLN,21)
        ITRI(3,I)=IPLA+ISTRAIN+ISSN+IHBE+IGTYP+MLN
C
C---     Clef4 
C
C       IRB    = MY_SHIFTL(IRB,0)
C
        ISHXFEM_PLY  = MY_SHIFTL(ISHXFEM_PLY,10)
        IFAIL  = MY_SHIFTL(IFAIL,11)
        IEXPAN = MY_SHIFTL(IEXPAN,14)
        JTHE   = MY_SHIFTL(JTHE,15)
        ICSEN= MY_SHIFTL(ICSEN,16)
        NPN  = MY_SHIFTL(NPN,17)
        IREP = MY_SHIFTL(IREP,26)
        ITHK = MY_SHIFTL(ITHK,30)
        IF(IXFEM > 0) IXFEM  = MY_SHIFTL(IXFEM,9)
C
        ITRI(4,I)=ITHK+IREP+NPN+ICSEN+JTHE+IEXPAN+IRB+IFAIL+ISHXFEM_PLY
     .           +IXFEM
     
C---     Clef5 
C       MID=MY_SHIFTL(MID,0)
        ITRI(5,I)=MID
C---     Clef6 
C       PID=MY_SHIFTL(PID,0)
        ITRI(6,I)=PID
C --- clef7 used for type17 iworkc=0 with/out type17 (or type51) PID  
        ITRI(7,I) =  IWORKSH(2,I) 
      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELC , 7)
C
       DO I=1,NUMELC
        IPARTC(I) =INUM(1,INDEX(I))
        ISHEOFF(I)=INUM(2,INDEX(I))
        IF (NSUBDOM>0) TAG_ELCF(I)=INUM_R2R(INDEX(I))
        THK(I)    =XNUM(INDEX(I))
      ENDDO

      DO I=1,NUMELC
        CEP(I)=XEP(INDEX(I))
        PERMUTATION%SHELL(I)=INDEX2(INDEX(I))
      ENDDO

      DO  K=1,7
        DO  I=1,NUMELC
          IXC(K,I)=INUM(K+2,INDEX(I))
        ENDDO
      ENDDO
      DO I=1,NUMELC
       IWORKSH(1,I)= INUM_WORKC(1,INDEX(I))
       IWORKSH(2,I)= INUM_WORKC(2,INDEX(I))
       IWORKSH(3,I)= INUM_WORKC(3,INDEX(I))
       IF (NDRAPE > 0) THEN
         NPT = XNUM_DRAPE(INDEX(I))%NPT 
         IF(NPT /= DRAPE(I)%NPT ) THEN
            DEALLOCATE(DRAPE(I)%IDRAPE,DRAPE(I)%RDRAPE)
            ALLOCATE(DRAPE(I)%IDRAPE(NPT),DRAPE(I)%RDRAPE(NPT,2))
         ENDIF 
         DRAPE(I)%NPT = XNUM_DRAPE(INDEX(I))%NPT
         DO IPT = 1, NPT
           DRAPE(I)%IDRAPE(IPT)   = XNUM_DRAPE(INDEX(I))%IDRAPE(IPT)
           DRAPE(I)%RDRAPE(IPT,1) = XNUM_DRAPE(INDEX(I))%RDRAPE(IPT,1)
           DRAPE(I)%RDRAPE(IPT,2) = XNUM_DRAPE(INDEX(I))%RDRAPE(IPT,2)
         ENDDO
       ENDIF
       IF (NPERTURB > 0) THEN
         DO IPERT = 1, NPERTURB
           RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
         ENDDO
       ENDIF
        SH4ANG(I)=ANGLE(INDEX(I))
      ENDDO
C
      IF(NADMESH /= 0)THEN
        DO  K=1,KSH4TREE
          DO  I=1,NUMELC
            SH4TREE(K,I)=ISTOR(K,INDEX(I))
          ENDDO
        ENDDO
        IF(LSH4TRIM/=0)THEN
          DO  I=1,NUMELC
            SH4TRIM(I)=ISTOR(KSH4TREE+1,INDEX(I))
          ENDDO
        END IF
      END IF
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELC
        ITR1(INDEX(I))=I
      ENDDO
C
C RENUMEROTATION DE L'ARBRE
      IF(NADMESH /= 0)THEN
        DO  I=1,NUMELC
          IF(SH4TREE(1,I) /= 0)
     .       SH4TREE(1,I)=ITR1(SH4TREE(1,I))
          IF(SH4TREE(2,I) /= 0)
     .       SH4TREE(2,I)=ITR1(SH4TREE(2,I))
        ENDDO
      END IF
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP(J) == 3)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C RENUMEROTATION POUR shell in Accel (gauge)
C
      DO I=1,NBGAUGE
         N1 = LGAUGE(1,I)
         IF(N1 <= 0) THEN
            N1=-LGAUGE(3,I)
            IF(N1 > 0) LGAUGE(3,I)=-ITR1(N1)
         ENDIF
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL
C
      DO I=1,NGRSHEL
        NN=IGRSH4N(I)%NENTITY
        DO J=1,NN
          IGRSH4N(I)%ENTITY(J) = ITR1(IGRSH4N(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumerotation CONNECTIVITE INVERSE
C
      DO I=1,4*NUMELC
        IF (NOD2ELC(I) /= 0) NOD2ELC(I)=ITR1(NOD2ELC(I))
      END DO
C
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELC
        II0=ITRI(1,INDEX(I))
        JJ0=ITRI(1,INDEX(I-1))
        II =ITRI(2,INDEX(I))
        JJ =ITRI(2,INDEX(I-1))
        II1=ITRI(3,INDEX(I))
        JJ1=ITRI(3,INDEX(I-1))
        II2=ITRI(4,INDEX(I))
        JJ2=ITRI(4,INDEX(I-1))
        II3=ITRI(5,INDEX(I))
        JJ3=ITRI(5,INDEX(I-1))
        II4=ITRI(6,INDEX(I))
        JJ4=ITRI(6,INDEX(I-1))
C for stack/ply pid
        II5=ITRI(7,INDEX(I))
        JJ5=ITRI(7,INDEX(I-1))
        IF (II0/=JJ0 .or. 
     *   II/=JJ .or. 
     *   II1/=JJ1 .or.
     *   II2/=JJ2.OR.II3 /= JJ3.OR.II4 /= JJ4.OR.II5 /= JJ5) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELC+1
C-----------
c
      IF (NPERTURB > 0) THEN
        IF (ALLOCATED(XNUM_RNOISE)) DEALLOCATE(XNUM_RNOISE) 
      ENDIF
c
      DEALLOCATE(INDEX2)
      DEALLOCATE( ISTOR )
      DEALLOCATE( XNUM_DRAPE )
      RETURN
      END
