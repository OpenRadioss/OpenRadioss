Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    cgrhead            ../starter/source/elements/shell/coque/cgrhead.F
!||--- called by ------------------------------------------------------
!||    lectur             ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg             ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    drape_mod          ../starter/share/modules1/drape_mod.F
!||    message_mod        ../starter/share/message_module/message_mod.F
!||    r2r_mod            ../starter/share/modules1/r2r_mod.F
!||    reorder_mod        ../starter/share/modules1/reorder_mod.F
!||    stack_mod          ../starter/share/modules1/stack_mod.F
!||====================================================================
      SUBROUTINE CGRHEAD(
     1       IXC     ,PM      ,GEO       ,INUM   ,ISEL   ,
     2       ITR1    ,EADD    ,INDEX     ,ITRI   ,XNUM   ,
     3       IPARTC  ,ND      ,THK       ,IGRSURF,IGRSH4N,
     4       CEP     ,XEP     ,IGEO      ,IPM    ,
     5       IPART   ,SH4TREE ,NOD2ELC   ,ISHEOFF,SH4TRIM,
     6       TAGPRT_SMS, LGAUGE,IWORKSH  ,MAT_PARAM,
     7       STACK   ,DRAPE  ,RNOISE ,SH4ANG,DRAPEG, PTSHEL,
     8       DAMP_RANGE_PART)
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXC(NIXC,NUMELC)  ARRAY MID(1)+CONECS(2-5)+PID(6)+           E
C                       N GLOBAL(7)                               E
C     PM(NPROPM,NUMMAT) ARRAY MATERIAL CHARACTERISTICS             E
C     GEO(NPROPG,NUMGEO)ARRAY PID CHARACTERISTICS                  E
C     INUM(9,NUMELC)    WORKING ARRAY                              E/S
C     ISEL(NSELC)       ARRAY SELECTED SHELLS FOR TH               E/S
C     ITR1(NSELC)       WORKING ARRAY                              E/S
C     EADD(NUMELC)      ARRAY ADDRESSES IN IDAM CHECKBOARD         S
C     INDEX(NUMELC)     WORKING ARRAY                              E/S
C     ITRI(7,NUMELC)    WORKING ARRAY                              E/S
C     IPARTC(NUMELC)    PART ARRAY                                 E/S
C     CEP(NUMELC)    PROC ARRAY                                    E/S
C     XEP(NUMELC)    PROC ARRAY                                    E/S
C     NOD2ELC(4*NUMELC)                                            E/S
C     ISHEOFF(NUMELC)   FLAG ELEM RBY ON/OFF                       E/S
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE STACK_MOD
      USE MESSAGE_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE DRAPE_MOD
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "drape_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*),ISEL(*),INUM(9,*),IPARTC(*), ISHEOFF(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(8,*),
     .        ND, CEP(*), XEP(*),
     .        IGEO(NPROPGI,*),IPM(NPROPMI,*),IPART(LIPART1,*), 
     .        SH4TREE(KSH4TREE,*), NOD2ELC(*), SH4TRIM(*),
     .        TAGPRT_SMS(*) ,LGAUGE(3,*),
     .        IWORKSH(3,*)
      INTEGER , DIMENSION(NUMELC) , INTENT(INOUT):: PTSHEL
      INTEGER , INTENT(IN) :: DAMP_RANGE_PART(NPART)     !< flag to compute the damping range
C     REAL OR REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*),XNUM(*),THK(*), RNOISE(NPERTURB,*),
     .    SH4ANG(*)
C-----------------------------------------------
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)   , TARGET    :: DRAPE (NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_)                             :: DRAPEG
      TYPE (DRAPE_)  , DIMENSION(:), ALLOCATABLE :: XNUM_DRAPE
      TYPE (DRAPEG_)                             :: XNUM_DRAPEG
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT),INTENT(IN) :: MAT_PARAM
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
     .  II5,JJ5,II6,JJ6,
     .  ISUBSTACK,IIGEO,IADI ,IPPID,NB_LAW58,IPMAT,
     .  IPERT,STAT,IP,NSLICE,KK,NPT_DRP,IE,IE0
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INUM_WORKC !(3,NUMELC)
      my_real, DIMENSION(:), ALLOCATABLE :: ANGLE !(NUMELC)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND,IPIDL
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2, INUM_PTSHEL
C     REAL OR REAL*8
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
C
      TYPE (DRAPE_PLY_)             , POINTER   :: DRAPE_PLY
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C1---------------------------------
      DATA MSKMLN /O'07770000000'/
      DATA MSKTYP /O'00007770000'/
      DATA MSKIHB /O'00000007000'/
      DATA MSKISN /O'00000000700'/
      DATA MSKIST /O'00000000070'/
      DATA MSKIPL /O'00000000007'/
C2---------------------------------
      DATA MSKITH /O'10000000000'/
      DATA MSKIRP /O'07000000000'/
      DATA MSKNPN /O'00777000000'/
      DATA MSKIRB /O'00000000007'/
C3---------------------------------
      DATA MSKMID /O'07777777777'/
C4---------------------------------
      DATA MSKPID /O'07777777777'/
C======================================================================|
C   GLOBAL SORTING ON ALL CRITERIA FOR ALL ELEMENTS
C----------------------------------------------------------
      ALLOCATE(ANGLE(NUMELC))
      ALLOCATE(INUM_WORKC(3,NUMELC))
      IF(NADMESH /= 0)THEN
        ALLOCATE( ISTOR(KSH4TREE+1,NUMELC) )
      ELSE
        ALLOCATE( ISTOR(0,0) )
      ENDIF
        IF (NDRAPE > 0 .AND. NUMELC_DRAPE > 0) THEN
         ALLOCATE(XNUM_DRAPE(NUMELC))  
         ALLOCATE(XNUM_DRAPEG%INDX(NUMELC))
         XNUM_DRAPEG%INDX = 0                                             
         DO I =1, NUMELC 
           IE = DRAPEG%INDX(I)
           IF(IE == 0) CYCLE                                                                      
           NPT_DRP = DRAPE(IE)%NPLY_DRAPE  
           NPT     = DRAPE(IE)%NPLY                                             
           ALLOCATE(XNUM_DRAPE(I)%INDX_PLY(NPT))                                              
           ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(NPT_DRP))        
           XNUM_DRAPE(I)%INDX_PLY = 0
           XNUM_DRAPE(I)%INDX_PLY = 0
           DO J = 1,NPT_DRP
                NSLICE = DRAPE(IE)%DRAPE_PLY(J)%NSLICE    
                ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%RDRAPE(NSLICE,2))    
                ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%IDRAPE(NSLICE,2)) 
                XNUM_DRAPE(I)%DRAPE_PLY(J)%RDRAPE = 0             
                XNUM_DRAPE(I)%DRAPE_PLY(J)%IDRAPE = 0  
           ENDDO 
         ENDDO      
        ELSE
            ALLOCATE( XNUM_DRAPE(0) )
        ENDIF
      IF(ABS(ISIGI) == 3 .OR. ABS(ISIGI) == 4 .OR. ABS(ISIGI) == 5) THEN
        ALLOCATE(INUM_PTSHEL(NUMELC))
        INUM_PTSHEL = 0
      ELSE
        ALLOCATE(INUM_PTSHEL(0))
      ENDIF  
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELC),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ENDIF
C    
      CALL MY_ALLOC(INDEX2,NUMELC)

      IF(NDRAPE > 0 .AND. NUMELC_DRAPE > 0) THEN
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
            IF (NPERTURB > 0) THEN
              DO IPERT = 1, NPERTURB
                XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I) 
              ENDDO
            ENDIF
            ANGLE(I)=SH4ANG(I) 
            !drape structure
            IE = DRAPEG%INDX(I)
            XNUM_DRAPEG%INDX(I) = DRAPEG%INDX(I)                                
            IF(IE == 0) CYCLE                                                   
            NPT = DRAPE(IE)%NPLY 
            XNUM_DRAPE(I)% NPLY  = NPT                        
            XNUM_DRAPE(I)%INDX_PLY(1:NPT) = DRAPE(IE)%INDX_PLY(1:NPT)
            NPT = DRAPE(IE)%NPLY_DRAPE                                      
            XNUM_DRAPE(I)%NPLY_DRAPE = NPT   
            XNUM_DRAPE(I)%THICK  =  DRAPE(IE)%THICK    
            DO JJ = 1, NPT                                                  
              DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(JJ)                              
              NSLICE =  DRAPE_PLY%NSLICE                                        
              XNUM_DRAPE(I)%DRAPE_PLY(JJ)%NSLICE =  NSLICE                      
              XNUM_DRAPE(I)%DRAPE_PLY(JJ)%IPID   = DRAPE_PLY%IPID
              DO KK = 1,NSLICE                                                  
                XNUM_DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(KK,1)=DRAPE_PLY%IDRAPE(KK,1) 
                XNUM_DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(KK,2)=DRAPE_PLY%IDRAPE(KK,2) 
                XNUM_DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(KK,1)=DRAPE_PLY%RDRAPE(KK,1) 
                XNUM_DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(KK,2)=DRAPE_PLY%RDRAPE(KK,2)
              ENDDO    
              DEALLOCATE(DRAPE_PLY%IDRAPE, DRAPE_PLY%RDRAPE)
            ENDDO
             DEALLOCATE(DRAPE(IE)%DRAPE_PLY)  
             DEALLOCATE(DRAPE(IE)%INDX_PLY)                  
         ENDDO
       ELSE
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
            IF (NPERTURB > 0) THEN
              DO IPERT = 1, NPERTURB
                XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I) 
              ENDDO
            ENDIF
            ANGLE(I)=SH4ANG(I) 
         ENDDO
       ENDIF   
       IF(ABS(ISIGI)==3.OR.ABS(ISIGI)== 4.OR.ABS(ISIGI)==5)THEN
         INUM_PTSHEL(1:NUMELC) = PTSHEL(1:NUMELC)
       ENDIF
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
C         ILEV must have strong weight on 1st key
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
            NFAIL = MAX(NFAIL,MAT_PARAM(IMATLY)%NFAIL)
          ENDDO 
          IF(ICRACK3D > 0)THEN
C-          new multilayer -
            IXFEM = MAT_PARAM(MID)%IXFEM
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
            NFAIL = MAX(NFAIL,MAT_PARAM(IMATLY)%NFAIL)
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
            NFAIL  = MAX(NFAIL,MAT_PARAM(IMATLY)%NFAIL)
C --- PID 51 combined with LAW58 ---
            IF (NINT(PM(19,IMATLY)) == 58) NB_LAW58 = NB_LAW58 + 1
          ENDDO
C --- set IREP for sorting criteria:
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
            NFAIL  = MAX(NFAIL,MAT_PARAM(IMATLY)%NFAIL)
C --- PID 51 combined with LAW58 ---
            IF (NINT(PM(19,IMATLY)) == 58) NB_LAW58 = NB_LAW58 + 1
          ENDDO
C --- set IREP for sorting criteria:
          IF (NB_LAW58 == NPN) THEN
            IREP = 2
          ELSEIF (NB_LAW58 > 0) THEN
            IREP = IREP + 3
          ENDIF           
C
        ELSE ! IGTYP == 1
          NFAIL = MAT_PARAM(MID)%NFAIL
          IF(ICRACK3D > 0)THEN
C -         new monolayer -
            IXFEM = MAT_PARAM(MID)%IXFEM
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
C sorting on elem delete for rigidbody
C IRB = 0 : active elem
C IRB = 1 : inactive elem and optimized for SPMD
C IRB = 2 : inactive elem but optimized to be active in SPMD
        IRB = ISHEOFF(I)
C
C---     Key2 
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
C---     Key3 
C       IPLA   = MY_SHIFTL(IPLA,0)
        ISTRAIN= MY_SHIFTL(ISTRAIN,3)
        ISSN   = MY_SHIFTL(ISSN,6)
        IHBE   = MY_SHIFTL(IHBE,9)
        IGTYP  = MY_SHIFTL(IGTYP,12)
        MLN    = MY_SHIFTL(MLN,21)
        ITRI(3,I)=IPLA+ISTRAIN+ISSN+IHBE+IGTYP+MLN
C
C---     Key4 
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
     
C---     Key5 
C       MID=MY_SHIFTL(MID,0)
        ITRI(5,I)=MID
C---     Key6 
C       PID=MY_SHIFTL(PID,0)
        ITRI(6,I)=PID
C --- key7 used for type17 iworkc=0 with/out type17 (or type51) PID  
        ITRI(7,I) =  IWORKSH(2,I) 
C --- key 8---------------------------------          
        ITRI(8,I )= DAMP_RANGE_PART(IPARTC(II))
      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELC , 8)
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
      IF(NDRAPE > 0 .AND. NUMELC_DRAPE > 0 ) THEN
        IE = 0
        DO I=1,NUMELC
         IWORKSH(1,I)= INUM_WORKC(1,INDEX(I))
         IWORKSH(2,I)= INUM_WORKC(2,INDEX(I))
         IWORKSH(3,I)= INUM_WORKC(3,INDEX(I))
         IF (NPERTURB > 0) THEN
           DO IPERT = 1, NPERTURB
             RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
           ENDDO
         ENDIF
         SH4ANG(I)=ANGLE(INDEX(I))
         !
         IE0 = XNUM_DRAPEG%INDX(INDEX(I))
         DRAPEG%INDX(I)= 0
         IF(IE0 == 0) CYCLE
         IE = IE + 1
         NPT = XNUM_DRAPE(INDEX(I))% NPLY    ! number of layer shell 
         DRAPE(IE)%NPLY = NPT
         DRAPEG%INDX(I)= IE
         ALLOCATE(DRAPE(IE)%INDX_PLY(NPT)) 
         DRAPE(IE)%INDX_PLY(1:NPT) =  XNUM_DRAPE(INDEX(I))%INDX_PLY(1:NPT)                                  
         NPT = XNUM_DRAPE(INDEX(I))%NPLY_DRAPE       ! NPT_DRP                                      
         ALLOCATE(DRAPE(IE)%DRAPE_PLY(NPT))  
         DRAPE(IE)%NPLY_DRAPE= NPT   
         DRAPE(IE)%THICK = XNUM_DRAPE(INDEX(I))%THICK     
         DO JJ = 1, NPT         
           DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(JJ)                                  
           NSLICE = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%NSLICE                            
           DRAPE_PLY%NSLICE = NSLICE   
           DRAPE_PLY%IPID =  XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%IPID
           ALLOCATE(DRAPE_PLY%IDRAPE(NSLICE,2), DRAPE_PLY%RDRAPE(NSLICE,2))
           DO KK = 1,NSLICE                                                               
            DRAPE_PLY%IDRAPE(KK,1) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%IDRAPE(KK,1)      
            DRAPE_PLY%IDRAPE(KK,2) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%IDRAPE(KK,2)      
            DRAPE_PLY%RDRAPE(KK,1) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%RDRAPE(KK,1)      
            DRAPE_PLY%RDRAPE(KK,2) = XNUM_DRAPE(INDEX(I))%DRAPE_PLY(JJ)%RDRAPE(KK,2)      
           ENDDO                                                                          
         ENDDO 
        ENDDO
      ELSE
        DO I=1,NUMELC
         IWORKSH(1,I)= INUM_WORKC(1,INDEX(I))
         IWORKSH(2,I)= INUM_WORKC(2,INDEX(I))
         IWORKSH(3,I)= INUM_WORKC(3,INDEX(I))
         IF (NPERTURB > 0) THEN
           DO IPERT = 1, NPERTURB
             RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
           ENDDO
         ENDIF
          SH4ANG(I)=ANGLE(INDEX(I))
        ENDDO
      ENDIF  
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)== 4.OR.ABS(ISIGI)==5)THEN
         DO I=1,NUMELC
          PTSHEL(I) = INUM_PTSHEL(INDEX(I))
         ENDDO  
       ENDIF
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
C INVERSION OF INDEX (IN ITR1)
C
      DO I=1,NUMELC
        ITR1(INDEX(I))=I
      ENDDO
C
C RENUMBERING OF THE TREE
      IF(NADMESH /= 0)THEN
        DO  I=1,NUMELC
          IF(SH4TREE(1,I) /= 0)
     .       SH4TREE(1,I)=ITR1(SH4TREE(1,I))
          IF(SH4TREE(2,I) /= 0)
     .       SH4TREE(2,I)=ITR1(SH4TREE(2,I))
        ENDDO
      END IF
C
C RENUMBERING FOR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP(J) == 3)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C RENUMBERING FOR shell in Accel (gauge)
C
      DO I=1,NBGAUGE
         N1 = LGAUGE(1,I)
         IF(N1 <= 0) THEN
            N1=-LGAUGE(3,I)
            IF(N1 > 0) LGAUGE(3,I)=-ITR1(N1)
         ENDIF
      ENDDO
C
C RENUMBERING FOR SHELL GROUPS
C
      DO I=1,NGRSHEL
        NN=IGRSH4N(I)%NENTITY
        DO J=1,NN
          IGRSH4N(I)%ENTITY(J) = ITR1(IGRSH4N(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumbering INVERSE CONNECTIVITY
C
      DO I=1,4*NUMELC
        IF (NOD2ELC(I) /= 0) NOD2ELC(I)=ITR1(NOD2ELC(I))
      END DO
C
C--------------------------------------------------------------
C         DETERMINATION OF SUPER_GROUPS
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
C damp freq range        
        II6=ITRI(8,INDEX(I))
        JJ6=ITRI(8,INDEX(I-1))       
        IF (II0/=JJ0 .or. 
     *   II/=JJ .or. 
     *   II1/=JJ1 .or.
     *   II2/=JJ2.OR.II3 /= JJ3.OR.II4 /= JJ4.OR.II5 /= JJ5 .or.
     *   II6 /= JJ6) THEN
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
      IF(NDRAPE > 0 .AND. NUMELC_DRAPE > 0) THEN
         DO I =1, NUMELC       
           IE = XNUM_DRAPEG%INDX(I)
           IF(IE == 0) CYCLE                                
           NPT_DRP = XNUM_DRAPE(I)%NPLY_DRAPE  
           DO J = 1,NPT_DRP                                              
              DEALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%RDRAPE)          
              DEALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(J)%IDRAPE)  
           ENDDO   
           DEALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY,XNUM_DRAPE(I)%INDX_PLY) 
         ENDDO
         DEALLOCATE( XNUM_DRAPE ,XNUM_DRAPEG%INDX)
      ELSE
         DEALLOCATE( XNUM_DRAPE )      
      ENDIF
      IF(ALLOCATED(INUM_PTSHEL))DEALLOCATE(INUM_PTSHEL)

      DEALLOCATE(ANGLE,INUM_WORKC)
      RETURN
      END
