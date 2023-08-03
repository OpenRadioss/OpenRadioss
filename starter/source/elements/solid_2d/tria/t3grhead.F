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
Chd|  T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE T3GRHEAD(
     1       IXTG    ,PM      ,GEO     ,INUM    ,ISEL    ,
     2       ITR1    ,EADD    ,INDEX   ,ITRI    ,XNUM    ,
     3       IPARTTG ,ND      ,THK     ,IGRSURF ,IGRSH3N ,
     4       CEP     ,XEP     ,IXTG1   ,ICNOD   ,
     5       IGEO    ,IPM     ,IPART   ,SH3TREE ,NOD2ELTG,
     6       ITRIOFF ,SH3TRIM ,TAGPRT_SMS,
     7       IWORKSH , STACK  ,DRAPE   ,RNOISE,
     8       MULTI_FVM ,SH3ANG,DRAPEG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE MULTI_FVM_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXTG(NIXTG,NUMELTG)TABLEAU CONECS+PID+MID+NOS TRIANGLES       E
C     PM(NPROPM,NUMMAT)  TABLEAU DES CARACS DES MATERIAUX           E
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     INUM(9,NUMELTG)     TABLEAU DE TRAVAIL                         E/S
C     ISEL(NSELTG)        TABLEAU DES TRI CHOISIES POUR TH          E/S
C     ITR1(NSELTG)        TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELTG)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S 
C     INDEX(NUMELTG)      TABLEAU DE TRAVAIL                         E/S
C     ITRI(7,NUMELTG)     TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELTG)    TABLEAU PROC                               E/S
C     XEP(NUMELTG)    TABLEAU PROC                               E/S
C     NOD2ELTG(3*NUMELT )                                 E/S
C     ITRIOFF(NUMELTG)  FLAG ELEM RBY ON/OFF                         E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "remesh_c.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER
     .    IXTG(NIXTG,*),ISEL(*),INUM(10,*),ND,ICNOD(*),IXTG1(4,*),
     .    EADD(*), ITR1(*), INDEX(*), ITRI(7,*),IPARTTG(*),
     .    CEP(*), XEP(*),
     .    ITRIOFF(*),
     .    IGEO(NPROPGI,*),IPM(NPROPMI,*), IPART(LIPART1,*), 
     .    SH3TREE(KSH3TREE,*), NOD2ELTG(*), SH3TRIM(*),
     .    TAGPRT_SMS(*),IWORKSH(3,*)
C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*), XNUM(*), THK(*), RNOISE(NPERTURB,*),
     .    SH3ANG(*)
C-----------------------------------------------
      TYPE (STACK_PLY) :: STACK
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE (DRAPE_) , TARGET     :: DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_)                            :: DRAPEG
      TYPE (DRAPE_) ,DIMENSION(:)  ,ALLOCATABLE :: XNUM_DRAPE
      TYPE (DRAPEG_)               ,ALLOCATABLE :: XNUM_DRAPEG
      TYPE (DRAPE_PLY_)            ,POINTER    :: DRAPE_PLY
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSH3N)  :: IGRSH3N
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISTOR,INUM_DRAPE
      INTEGER WORK(70000)
      INTEGER I, K, MLN, NG, ISSN, NPN, IFIO,NN,ICO,ID,
     .   MLN0, ISSN0, IC, N, MID, MID0, PID, PID0, ISTR0,
     .   IHBE, IHBE0, J, MIDN, NSG, NEL, NE1, ITHK,
     .   ITHK0, IPLA, IPLA0, II1, JJ1, II2, JJ2, II, JJ, 
     .   L, IGTYP, II3, JJ3,NGROU,NELTG3,
     .   MSKMLN, MSKNPN, MSKIHB, MSKISN, MODE,ICSEN,IFAIL,NFAIL,
     .   MSKIST, MSKIPL, MSKITH, MSKMID,MSKPID,MSKIRP,MSKTYP,IREP,
     .   II0,JJ0,ILEV,PRT,IADM,DIR,MSKIRB,IRB, II4, JJ4,
     .   IRUP,IXFEM,IWARNHB,IPT,IMATLY,IPID,ISH3N,
     .   INUM_WORKC(3,NUMELTG),II5,JJ5,ISUBSTACK,IIGEO,IADI,IPPID,
     .   NB_LAW58,IPMAT,IPERT,STAT,IALEL, MT,IP,NSLICE,KK,NPT_DRP,
     .   IEL, IEL0
       my_real
     . ANGLE(NUMELTG)
C     REAL OU REAL*8
      CHARACTER*nchartitle,
     .   TITR
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2

      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
C
Clef 1---------------------------------
      DATA MSKMLN /O'00777000000'/
      DATA MSKTYP /O'00000777000'/
      DATA MSKISN /O'00000000700'/
      DATA MSKIST /O'00000000070'/
      DATA MSKIPL /O'00000000007'/
Clef 2---------------------------------
      DATA MSKITH /O'10000000000'/
      DATA MSKIRP /O'07000000000'/
      DATA MSKNPN /O'00777000000'/
      DATA MSKIRB /O'00000000007'/
Clef 3---------------------------------
      DATA MSKMID /O'07777777777'/
Clef 4---------------------------------
      DATA MSKPID /O'07777777777'/
C-----------------------------------------------
C-----------------------------------------------

C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       IWARNHB=0
      IF(NADMESH/=0)THEN
        ALLOCATE( ISTOR(KSH3TREE+1,NUMELTG) )
      ELSE
        ALLOCATE( ISTOR(0,0) )
      ENDIF
        IF (NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN
         ALLOCATE(XNUM_DRAPE(NUMELTG))                                                  
         ALLOCATE(XNUM_DRAPEG%INDX(NUMELTG))
         XNUM_DRAPEG%INDX = 0                                             
         DO I =1, NUMELTG
           IEL = DRAPEG%INDX(NUMELC + I)
           IF(IEL == 0) CYCLE                                                                         
           NPT_DRP = DRAPE(IEL)%NPLY_DRAPE
           NPT = IWORKSH(1,NUMELC + I)                                               
           ALLOCATE(XNUM_DRAPE(I)%INDX_PLY(NPT))                                              
           ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(NPT_DRP))        
            XNUM_DRAPE(I)%INDX_PLY= 0                                                       
           DO J = 1,NPT_DRP                            
                NSLICE = DRAPE(IEL)%DRAPE_PLY(J)%NSLICE    
                ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%RDRAPE(NSLICE,2))    
                ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%IDRAPE(NSLICE,2))                                       
                XNUM_DRAPE(I)%DRAPE_PLY(J)%RDRAPE = 0             
                XNUM_DRAPE(I)%DRAPE_PLY(J)%IDRAPE = 0                               
           ENDDO                                                                           
         ENDDO      
        ELSE
            ALLOCATE( XNUM_DRAPE(0) )
        ENDIF
C
C----------------------------------------------------------
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELTG),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ENDIF
C
      CALL MY_ALLOC(INDEX2,NUMELTG)

      IF(NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN 
         DO I=1,NUMELTG                                                          
            INDEX2(I)= PERMUTATION%TRIANGLE(I)                                
            EADD(I)=1                                                         
            ITRI(7,I)=I                                                       
            INDEX(I)=I                                                        
            INUM(1,I)=IPARTTG(I)                                              
            INUM(2,I)=ITRIOFF(I)                                              
            XNUM(I)  = THK(I)                                                 
            INUM(3,I)=IXTG(1,I)                                               
            INUM(4,I)=IXTG(2,I)                                               
            INUM(5,I)=IXTG(3,I)                                               
            INUM(6,I)=IXTG(4,I)                                               
            INUM(7,I)=IXTG(5,I)                                               
            INUM(8,I)=IXTG(6,I)                                               
            INUM(9,I)=ICNOD(I)                                                
            INUM(10,I)=IXTG(1,I)                                              
            INUM_WORKC(1,I) = IWORKSH(1,NUMELC + I)                           
            INUM_WORKC(2,I) = IWORKSH(2,NUMELC + I)                           
            INUM_WORKC(3,I) = IWORKSH(3,NUMELC + I)                           
            IF (NPERTURB > 0) THEN                                               
             DO IPERT = 1, NPERTURB                                             
               XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I)                           
             ENDDO                                                              
             ENDIF                                                              
             ANGLE(I)=SH3ANG(I)                                                 
             !drape structure                                                   
             IEL = DRAPEG%INDX(NUMELC + I)  
             XNUM_DRAPEG%INDX(I) = IEL  
             IF(IEL == 0) CYCLE
             NPT = IWORKSH(1,NUMELC + I)                                        
             XNUM_DRAPE(I)%INDX_PLY(1:NPT) = DRAPE(IEL)%INDX_PLY(1:NPT)          
             NPT = DRAPE(IEL)%NPLY_DRAPE 
             XNUM_DRAPE(I)%NPLY_DRAPE = NPT                                     
             DO JJ = 1, NPT                                                     
               DRAPE_PLY => DRAPE(IEL)%DRAPE_PLY(JJ)
               NSLICE =  DRAPE_PLY%NSLICE                                       
               XNUM_DRAPE(I)%DRAPE_PLY(JJ)%NSLICE =  NSLICE                     
               XNUM_DRAPE(I)%DRAPE_PLY(JJ)%IPID = DRAPE_PLY%IPID               
               DO KK = 1,NSLICE                                                 
                 XNUM_DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(KK,1)=DRAPE_PLY%IDRAPE(KK,1)
                 XNUM_DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(KK,2)=DRAPE_PLY%IDRAPE(KK,2)
                 XNUM_DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(KK,1)=DRAPE_PLY%RDRAPE(KK,1)
                 XNUM_DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(KK,2)=DRAPE_PLY%RDRAPE(KK,2)
               ENDDO  
               DEALLOCATE(DRAPE_PLY%IDRAPE, DRAPE_PLY%RDRAPE)                                                           
             ENDDO
             DEALLOCATE(DRAPE(IEL)%DRAPE_PLY)  
             DEALLOCATE(DRAPE(IEL)%INDX_PLY)                                                                   
          ENDDO  
        ELSE  
          DO I=1,NUMELTG                                                      
            INDEX2(I)= PERMUTATION%TRIANGLE(I)                                
            EADD(I)=1                                                         
            ITRI(7,I)=I                                                       
            INDEX(I)=I                                                        
            INUM(1,I)=IPARTTG(I)                                              
            INUM(2,I)=ITRIOFF(I)                                              
            XNUM(I)  = THK(I)                                                 
            INUM(3,I)=IXTG(1,I)                                               
            INUM(4,I)=IXTG(2,I)                                               
            INUM(5,I)=IXTG(3,I)                                               
            INUM(6,I)=IXTG(4,I)                                               
            INUM(7,I)=IXTG(5,I)                                               
            INUM(8,I)=IXTG(6,I)                                               
            INUM(9,I)=ICNOD(I)                                                
            INUM(10,I)=IXTG(1,I)                                              
            INUM_WORKC(1,I) = IWORKSH(1,NUMELC + I)                           
            INUM_WORKC(2,I) = IWORKSH(2,NUMELC + I)                           
            INUM_WORKC(3,I) = IWORKSH(3,NUMELC + I)      
            IF (NPERTURB > 0) THEN                                            
              DO IPERT = 1, NPERTURB                                          
                XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I)                        
              ENDDO                                                           
            ENDIF                                                             
            ANGLE(I)=SH3ANG(I)                                                
          ENDDO
      ENDIF                                                                 
C
      IF(NADMESH/=0)THEN
        DO  K=1,KSH3TREE
          DO  I=1,NUMELTG
            ISTOR(K,I)=SH3TREE(K,I)
          ENDDO
        ENDDO
        IF(LSH3TRIM/=0)THEN
          DO  I=1,NUMELTG
            ISTOR(KSH3TREE+1,I)=SH3TRIM(I)
          ENDDO
        END IF
      END IF
C
      DO I=1,NUMELTG
        XEP(I)=CEP(I)
      ENDDO
C
      DO 100 I = 1, NUMELTG
        II = I
C
        IF(NADMESH==0)THEN
          ITRI(1,I)=0
        ELSE
C
C       ILEV doit etre de poids fort sur 1ere clef
          PRT = IPARTTG(II)
          IADM= IPART(10,PRT)
          IF(IADM==0)THEN
C         not the same group as if adaptivity.
            ITRI(1,I)=0
          ELSE
            ILEV=SH3TREE(3,I)
            IF(ILEV<0)ILEV=-ILEV-1
            ITRI(1,I)=ILEV+1
          END IF
        END IF
C
        MID= IXTG(1,II)
        PID= IXTG(5,II)
        MLN = NINT(PM(19,MID))
C
        JTHE = NINT(PM(71,MID))        
        IGTYP = IGEO(11,PID)
        NPN   = IGEO(4,PID)
        ISH3N  = IGEO(18,PID)
        IXFEM = 0
	       NFAIL = IPM(220,MID)
        IFAIL = 0
C
        IF (IGTYP == 11) THEN
          DO IPT = 1, NPN
           IMATLY  = IGEO(100+IPT,PID)
            NFAIL  = MAX(NFAIL, IPM(220, IMATLY)) 
          ENDDO 
          IF(ICRACK3D > 0)THEN
C -         new multilayer -
            IXFEM = IPM(236,MID)
          ENDIF
        ELSEIF (IGTYP == 17) THEN
          NPN = IWORKSH(1,NUMELC + II)
          ISUBSTACK =IWORKSH(3,NUMELC + II)
!!          IIGEO   =  40 + 5*(ISUBSTACK - 1) 
!!          IADI    = IGEO(IIGEO + 3,PID)
!!          IPPID   = IADI
          IPPID = 2
          DO IPT = 1, NPN
            IPID = STACK%IGEO(IPPID+IPT,ISUBSTACK)
            IMATLY = IGEO(101, IPID)
            NFAIL  = MAX(NFAIL, IPM(220, IMATLY)) 
          ENDDO       
        ELSEIF (IGTYP == 51  ) THEN
C---
C new shell property (multiple NPT through each layer)
C---
          NB_LAW58 = 0
          NPN = IWORKSH(1,NUMELC + II)
          ISUBSTACK =IWORKSH(3,NUMELC + II)
          IPPID = 2
          DO IPT = 1, NPN
            IPID = STACK%IGEO(IPPID+IPT,ISUBSTACK)
            IMATLY = IGEO(101, IPID)
            NFAIL  = MAX(NFAIL, IPM(220, IMATLY)) 
C --- PID 51 combined with LAW58 ---
            IF (NINT(PM(19,IMATLY)) == 58) NB_LAW58 = NB_LAW58 + 1
          ENDDO
C --- set IREP for tri criteria:
          IF (NB_LAW58 == NPN) THEN
            IREP = 2
          ELSEIF (NB_LAW58 > 0) THEN
            IREP = IREP + 3
          ENDIF
        ELSEIF ( IGTYP == 52 ) THEN
C---
C new shell property (multiple NPT through each layer)
C---
          NB_LAW58 = 0
          NPN = IWORKSH(1,NUMELC + II)
          ISUBSTACK =IWORKSH(3,NUMELC + II)
          IPPID = 2
          IPMAT = IPPID + NPN
          DO IPT = 1, NPN
            IPID   = STACK%IGEO(IPPID + IPT,ISUBSTACK)
            IMATLY = STACK%IGEO(IPMAT + IPT,ISUBSTACK)
            NFAIL  = MAX(NFAIL, IPM(220, IMATLY)) 
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
          IF(ICRACK3D > 0)THEN
C -         new monolayer -
            IXFEM = IPM(236,MID)
            IF (IXFEM == 1) THEN
              IXFEM = 2
              ICRACK3D = IXFEM
            ENDIF
          END IF
        ENDIF
        IF (NFAIL > 0) IFAIL = 1
c        
C thermal material expansion
        IEXPAN  = IPM(218, MID)       
        ICO = ICNOD(II)
        IF(ISH3N>3.AND.ISH3N<=29)THEN
          ID = IGEO(1,PID)
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
          CALL ANCMSG(MSGID=435,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ISH3N,
     .                I3=IXTG(NIXTG,II))
          IWARNHB=IWARNHB+1
          ISH3N=2
        ENDIF
        ITHK = NINT(GEO(35,PID))
        IPLA = NINT(GEO(39,PID))
        IREP = IGEO(6,PID)
        ICSEN= IGEO(3,PID)
        IF (ICSEN > 0) ICSEN=1
C
        IF(NPN==0.AND.(MLN==36.OR.MLN==86))THEN
          IF(IPLA==0) IPLA=1
          IF(IPLA==2) IPLA=0
        ELSEIF(NPN==0.AND.MLN==2)THEN
          IF(IPLA==2) IPLA=0
        ELSE
         IF(IPLA==2) IPLA=0
         IF(IPLA==3) IPLA=2
        ENDIF
        IF(ITHK==2)THEN
          ITHK = 0
        ELSEIF(MLN==32)THEN
          ITHK = 1
        ENDIF
        ISTRAIN = NINT(GEO(11,PID))
        IF(MLN==19.OR.MLN>=25.OR.MLN==15)ISTRAIN = 1
        ISSN = NINT(GEO(3,PID))
C tri sur elem delete des rigidbody
C IRB = 0 : elem actif
C IRB = 1 : elem inactif et optimise pour en SPMD
C IRB = 2 : elem inactif mais optimise pour etre actif en SPMD
        IRB = ITRIOFF(I)
C
C---     Clef2 
        JSMS = 0
        IF(ISMS/=0)THEN
          IF(IDTGRS/=0)THEN
            IF(TAGPRT_SMS(IPARTTG(II))/=0)JSMS=1
          ELSE
            JSMS=1
          END IF
        END IF
C       JSMS=MY_SHIFTL(JSMS,0)
        ITRI(2,I) = JSMS
C       NEXT=MY_SHIFTL(NEXT,1)
C
C---     Clef3 
C
C       IPLA   = MY_SHIFTL(IPLA,0)
        ISTRAIN= MY_SHIFTL(ISTRAIN,3)
        ISSN   = MY_SHIFTL(ISSN,6)
C
        IGTYP  = MY_SHIFTL(IGTYP,9)
        MLN    = MY_SHIFTL(MLN,18)    
C       attention cle pleine ;
C       ICO doit rester en poids le plus fort dans cette cle.
        ICO   = MY_SHIFTL(ICO,29) 
        ITRI(3,I)=IPLA+ISTRAIN+ISSN+IGTYP+MLN+ICO 
C
C---clef4
C
C       IRB    = MY_SHIFTL(IRB,0)
        IFAIL  = MY_SHIFTL(IFAIL,4)
        IEXPAN = MY_SHIFTL(IEXPAN,5)
        JTHE =  MY_SHIFTL(JTHE,6)
        ISH3N  = MY_SHIFTL(ISH3N,11)
        ICSEN = MY_SHIFTL(ICSEN,16)
        NPN   = MY_SHIFTL(NPN,17)
        IREP  = MY_SHIFTL(IREP,26)
        ITHK  = MY_SHIFTL(ITHK,30)
        IF(IXFEM > 0)IXFEM  = MY_SHIFTL(IXFEM,9)
C
        ITRI(4,I)=ITHK+IREP+NPN+ICSEN+ISH3N+JTHE+IRB+IFAIL+IXFEM
C---     Clef3 
C       MID=MY_SHIFTL(MID,0)
        ITRI(5,I)=MID
C---     Clef4 
C       PID=MY_SHIFTL(PID,0)
        ITRI(6,I)=PID
C --- clef7 used for type17 iworkc=0 with/out type17 PID
        ITRI(7,I) =  IWORKSH(2,NUMELC + I)
 100  CONTINUE
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELTG , 7)
C
      DO  I=1,NUMELTG
        IPARTTG(I)=INUM(1,INDEX(I))
        THK(I)   =XNUM(INDEX(I))
        ITRIOFF(I)=INUM(2,INDEX(I))
        ICNOD(I) = INUM(9,INDEX(I))
      ENDDO

      DO I=1,NUMELTG
        CEP(I)=XEP(INDEX(I))
        PERMUTATION%TRIANGLE(I)=INDEX2(INDEX(I))
      ENDDO

      DO  K=1,NIXTG
        DO  I=1,NUMELTG
          IXTG(K,I)=INUM(K+2,INDEX(I))
        ENDDO
      ENDDO
C
C 
      IF(NDRAPE == 0 .AND. NUMELTG_DRAPE > 0) THEN
         IEL = DRAPEG%NUMSH4
         DO I=1,NUMELTG
          IWORKSH(1,NUMELC + I)= INUM_WORKC(1,INDEX(I))
          IWORKSH(2,NUMELC + I)= INUM_WORKC(2,INDEX(I))
          IWORKSH(3,NUMELC + I)= INUM_WORKC(3,INDEX(I))
          IF (NPERTURB > 0) THEN                                                          
            DO IPERT = 1, NPERTURB                                                        
              RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I))                               
            ENDDO                                                                         
          ENDIF                                                                           
          SH3ANG(I)=ANGLE(INDEX(I))                                                       
          !                                                                               
          IEL0 = XNUM_DRAPEG%INDX(INDEX(I))  
          DRAPEG%INDX(NUMELC + I)= 0                                              
          IF(IEL0 == 0) CYCLE                                                              
          IEL = IEL + 1                                                                     
          NPT = IWORKSH(1,NUMELC + I) ! number of layer shell                             
          DRAPEG%INDX(NUMELC + I)= IEL
          ALLOCATE(DRAPE(IEL)%INDX_PLY(NPT))                                
          DRAPE(IEL)%INDX_PLY(1:NPT) =  XNUM_DRAPE(INDEX(I))%INDX_PLY(1:NPT)                                 
          NPT = XNUM_DRAPE(INDEX(I))%NPLY_DRAPE       ! NPT_DRP                                     
          DRAPE(IEL)%NPLY_DRAPE= NPT 
          ALLOCATE(DRAPE(IEL)%DRAPE_PLY(NPT))                                                      
          DO JJ = 1, NPT                                                                  
            DRAPE_PLY => DRAPE(IEL)%DRAPE_PLY(JJ)                                          
            NSLICE = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%NSLICE                            
            DRAPE_PLY%NSLICE = NSLICE                                                     
            DRAPE_PLY%IPID =  XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%IPID 
            ALLOCATE(DRAPE_PLY%IDRAPE(NSLICE,2), DRAPE_PLY%RDRAPE(NSLICE,2))
            DRAPE_PLY%IDRAPE = 0
            DRAPE_PLY%RDRAPE = ZERO                     
            DO KK = 1,NSLICE                                                              
             DRAPE_PLY%IDRAPE(KK,1) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%IDRAPE(KK,1)     
             DRAPE_PLY%IDRAPE(KK,2) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%IDRAPE(KK,2)     
             DRAPE_PLY%RDRAPE(KK,1) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%RDRAPE(KK,1)     
             DRAPE_PLY%RDRAPE(KK,2) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%RDRAPE(KK,2)     
            ENDDO                                                                         
          ENDDO                                                                           
        ENDDO
      ELSE ! ndrape = 0     
        DO I=1,NUMELTG
         IWORKSH(1,NUMELC + I)= INUM_WORKC(1,INDEX(I))
         IWORKSH(2,NUMELC + I)= INUM_WORKC(2,INDEX(I))
         IWORKSH(3,NUMELC + I)= INUM_WORKC(3,INDEX(I))
         IF (NPERTURB > 0) THEN
           DO IPERT = 1, NPERTURB
             RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
           ENDDO
         ENDIF
          SH3ANG(I)=ANGLE(INDEX(I))
        ENDDO
      ENDIF ! ndrape
      IF(NADMESH/=0)THEN
        DO  K=1,KSH3TREE
          DO  I=1,NUMELTG
            SH3TREE(K,I)=ISTOR(K,INDEX(I))
          ENDDO
        ENDDO
        IF(LSH3TRIM/=0)THEN
          DO  I=1,NUMELTG
            SH3TRIM(I)=ISTOR(KSH3TREE+1,INDEX(I))
          ENDDO
        END IF
      END IF

C     
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELTG
        ITR1(INDEX(I))=I
      ENDDO      

C RENUMEROTATION DE L'ARBRE
      IF(NADMESH/=0)THEN
        DO  I=1,NUMELTG
          IF(SH3TREE(1,I)/=0)
     .       SH3TREE(1,I)=ITR1(SH3TREE(1,I))
          IF(SH3TREE(2,I)/=0)
     .       SH3TREE(2,I)=ITR1(SH3TREE(2,I))
        ENDDO
      END IF
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP(J) == 7)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL3N
C
      DO I=1,NGRSH3N
        NN=IGRSH3N(I)%NENTITY
        DO J=1,NN
          IGRSH3N(I)%ENTITY(J) = ITR1(IGRSH3N(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumerotation CONNECTIVITE INVERSE
C
      DO I=1,3*NUMELTG
        IF(NOD2ELTG(I) /= 0)NOD2ELTG(I)=ITR1(NOD2ELTG(I))
      END DO
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELTG
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
        IF (II0/=JJ0.OR.
     .      II/=JJ.OR.
     .      II1/=JJ1.OR.
     .      II2/=JJ2.OR.
     .      II3/=JJ3.OR.
     .      II4/=JJ4.OR.
     .      II5 /= JJ5) THEN
               ND=ND+1
               EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELTG+1
      DO I=1,NUMELTG
      IF(IWARNHB/=0)THEN
         PID = IXTG(NIXTG-1,I)
         ID=IGEO(1,PID)
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
         CALL ANCMSG(MSGID=436,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
         IWARN=IWARN-1
      ENDIF
      ENDDO
c
      IF (NPERTURB > 0) THEN
        IF (ALLOCATED(XNUM_RNOISE)) DEALLOCATE(XNUM_RNOISE) 
      ENDIF
      IF(NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN
        DO I =1, NUMELTG       
           IEL0 = XNUM_DRAPEG%INDX(I)
           IF(IEL0 == 0) CYCLE                                
           NPT_DRP = XNUM_DRAPE(I)%NPLY_DRAPE  
           DO J = 1,NPT_DRP                                              
              DEALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%RDRAPE)          
              DEALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%IDRAPE)  
           ENDDO   
           DEALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY,XNUM_DRAPE(I)%INDX_PLY)
       ENDDO
        DEALLOCATE(XNUM_DRAPE,XNUM_DRAPEG%INDX)
      ELSE
        DEALLOCATE( XNUM_DRAPE)      
      ENDIF
C
      DEALLOCATE(INDEX2)
      DEALLOCATE( ISTOR )
      RETURN
      END
