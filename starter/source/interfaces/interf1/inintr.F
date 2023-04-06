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
Chd|  ININTR                        source/interfaces/interf1/inintr.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        IWCONTDD_NEW                  source/spmd/domain_decomposition/grid2mat.F
Chd|        RESET_GAP                     source/interfaces/interf1/reset_gap.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_REMNODE               source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG           source/interfaces/interf1/upgrade_remnode.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|        INTBUFSCRATCH_MOD             source/interfaces/interf1/intbufscratch_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTR(IPARI     ,INSCR    ,X,V       ,IXS      ,IXQ      ,
     2                  IXC       ,PM       ,GEO     ,ITAB     ,MS       ,
     3                  MWA       ,RWA      ,IXTG    ,IKINE    ,IXT      ,
     4                  IXP       ,IXR      ,ALE_CONNECTIVITY  ,NELEMINT ,IDDLEVEL ,
     5                  IFIEND    ,IGRBRIC  ,IWCONT  ,IWCIN2   ,KNOD2ELS ,
     6                  KNOD2ELC  ,KNOD2ELTG,NOD2ELS ,NOD2ELC  ,NOD2ELTG ,
     8                  IGRSURF   ,IELEM21  ,SH4TREE ,SH3TREE  ,IPART    ,
     9                  IPARTC    ,IPARTTG  ,THK     ,THK_PART ,NOD2EL1D ,
     A                  KNOD2EL1D ,IXS10    ,SIXINT  ,FRIGAP   ,IXS16    ,
     B                  IXS20     ,IPM      ,NOM_OPT           ,IPARTS   ,
     C                  KXX       ,IXX      ,IGEO    ,INTERCEP ,LELX     ,
     D                  INTBUF_TAB,FILLSOL  ,PM_STACK,IWORKSH  ,NSNT     ,
     E                  NMNT      ,KXIG3D   ,IXIG3D  ,KNOD2ELQ ,NOD2ELQ  ,
     F                  SEGQUADFR,TAGPRT_FRIC,INTBUF_FRIC_TAB  ,IPARTT   ,
     G                  IPARTP    ,IPARTX   ,IPARTR  ,NSN_MULTI_CONNEC,T2_NB_CONNEC,
     H                  ICODE     ,ISKEW    )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFMOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD 
!!      USE STACK_MOD 
      USE INTBUFSCRATCH_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
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
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr15_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), IXS(*), IXQ(*),
     .   IXC(*), ITAB(*), MWA(*), IXTG(*), IKINE(*),
     .   IWCONT(5,*),IWCIN2(2,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), 
     .   NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   IXT(*), IXP(*), IXR(*), NELEMINT,  IDDLEVEL,IFIEND,
     .   IELEM21(*),IPM(NPROPMI,*),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*), IPART(*),
     .   IPARTC(*), IPARTTG(*),NOD2EL1D(*),KNOD2EL1D(*), IXS10(*),I_MEM,
     .   RESORT  ,SIXINT, IXS16(8,*), IXS20(12,*),IPARTS(*),IGEO(*),
     .   IWORKSH(*),NSNT, NMNT,KXIG3D(NIXIG3D,*),IXIG3D(*),
     .   KNOD2ELQ(*),NOD2ELQ(*),SEGQUADFR(2,*),TAGPRT_FRIC(*),IPARTT(*),
     .   IPARTP(*),IPARTX(*),IPARTR(*),NSN_MULTI_CONNEC,T2_NB_CONNEC(*),
     .   ICODE(*), ISKEW(*)
      my_real
     .   X(3,*),V(3,*), PM(*), GEO(*), MS(*), RWA(6,*),
     .   THK(*),THK_PART(*),FRIGAP(NPARIR,*),
     .   LELX(*), FILLSOL(*),PM_STACK(*)
      INTEGER NOM_OPT(LNOPT1,*),KXX(*),IXX(*)
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
!!      TYPE (STACK_PLY) :: STACK
      TYPE(SCRATCH_STRUCT_) INSCR(*)
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC)  :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JINSCR, NIN, ITABM1, IWRN, I,NINT2
      INTEGER NTY, NSN2T, NMN2T,IKINE1(3*NUMNOD),ID,
     .        NSNET  ,NMNET  ,MULTIMP, IREMNODE, NREMNODE,
     .        NREMN(NINTER),NREMN_OLD(NINTER),IEDG,IEDGN,ST2_CONNEC,
     .        REMNODE_SIZE,LEN_FILNAM,REMNODE_SIZE_EDG,IREMNODE_EDG
      CHARACTER*(2148) FILNAM
      CHARACTER*nchartitle,
     .        TITR
      INTEGER, DIMENSION(:),ALLOCATABLE :: REMNODE,KREMNODE,T2_ADD_CONNEC,T2_CONNEC
      
      INTEGER :: NS
      INTEGER :: NSN,NMN
      LOGICAL :: CONDITION(NINTER)
      my_real :: v1(3),v2(3)
      INTEGER :: f1,f2
      my_real :: displacement,displacement_max
      INTEGER :: NRTM
      INTEGER :: MAIN_INTERFACE_SIZE  
      INTEGER :: ID_MAIN_INTERFACE
      INTEGER :: CPT,NODE_ID,J
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG
      DOUBLE PRECISION :: avg_stiff(NINTER)
      DOUBLE PRECISION :: main_stiff
      DOUBLE PRECISION :: min_stiff

       
C
      I_MEM = 0
      RESORT = 0
      NREMNODE = 0
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C-----------------------------------------------
      IF(((IMACH==3.AND.IDDLEVEL==0)).AND.
     +   (DECTYP>=3.AND.DECTYP<=6).AND.N2D==0)THEN
        NSNT = 0
        NMNT = 0
        NSN2T = 0
        NMN2T = 0
        NSNET = 0
        NMNET = 0
        DO I = 1, NUMNOD
          IWCONT(1,I) = 0
          IWCONT(2,I) = 0
          IWCONT(3,I) = 0
          IWCONT(4,I) = 0
          IWCIN2(1,I) = 0
          IWCIN2(2,I) = 0
        END DO
      END IF

C----------------------Treatment for TYPE2 spt27/28 compatibility  check - computation of size and allocation of array
       IF (NSN_MULTI_CONNEC > 0) THEN
         ALLOCATE (T2_ADD_CONNEC(NUMNOD))
         T2_ADD_CONNEC(1:NUMNOD) = 0
         ST2_CONNEC = 0
         T2_ADD_CONNEC(1) = 1
         IF (T2_NB_CONNEC(1)>1) ST2_CONNEC = 1 + 5*T2_NB_CONNEC(1)
         DO I=2,NUMNOD
C--        only potential multiple connections are counted - nodes with only one connections are not counted -> nb of connections set to 0
           IF (T2_NB_CONNEC(I) == 1) T2_NB_CONNEC(I) = 0
C--
           ST2_CONNEC = ST2_CONNEC + 1 + 5*T2_NB_CONNEC(I)
           T2_ADD_CONNEC(I) = T2_ADD_CONNEC(I-1) + 1 + 5*T2_NB_CONNEC(I-1)
         ENDDO
         ALLOCATE (T2_CONNEC(ST2_CONNEC))
         T2_CONNEC(1:ST2_CONNEC) = 0
       ELSE
         ST2_CONNEC = 0
         ALLOCATE (T2_ADD_CONNEC(0),T2_CONNEC(0))
       ENDIF

C-----------------------------------------------     
       IWRN = 0
       NINT2 = 0
       DO 100 N=1,NINTER
       IREMNODE = 0
       IREMNODE_EDG = 0
       NREMNODE = 0
       NTY=IPARI(7,N)
       IF ((NTY == 7.OR.NTY == 25) .AND. IPARI(63,N) == 2 .AND. IDDLEVEL == 1)THEN
C---     Initial dimension of REMNODE arrays
         IREMNODE = 1
         NREMNODE = 16*IPARI(4,N)
         IF(NREMNODE < 0)THEN
           NREMNODE=1073741824  ! decrease initial value of NREMNODE to fit into integer storage
         END IF
         CALL UPGRADE_REMNODE(IPARI(1,N),NREMNODE,INTBUF_TAB(N),NTY)
       ENDIF
       IF (NTY == 11.AND. IPARI(63,N) == 2 .AND. IDDLEVEL == 1)THEN
C---     Initial dimension of REMNODE arrays
         IREMNODE = 1
         REMNODE_SIZE = 5*IPARI(4,N)
         CALL UPGRADE_REMNODE(IPARI(1,N),REMNODE_SIZE,INTBUF_TAB(N),NTY)
       ENDIF
       IF (NTY == 25.AND.IPARI(58,N) >0 .AND. IPARI(63,N) == 2 .AND. IDDLEVEL == 1)THEN
C---     Initial dimension of REMNODE arrays
         IREMNODE_EDG = 1
         REMNODE_SIZE_EDG = 5*IPARI(68,N)
         CALL UPGRADE_REMNODE_EDG(IPARI(1,N),REMNODE_SIZE_EDG,INTBUF_TAB(N))
       ENDIF

       IF (NTY == 2)  NINT2=NINT2+1
       RESORT = 0
       IF (NTY == 14.OR.NTY == 15.OR.NTY == 16.
     .     OR.NTY == 18.OR.NTY==0) GOTO 100
C
 200   CONTINUE
C
       IF (I_MEM == 2)THEN
        IF(NTY == 11) THEN
          MULTIMP = MAX(IPARI(23,N)+8,NINT(IPARI(23,N)*1.75))
c         MULTIMP = MAX(MULTIMP,IPARI(23,N)+2500000/MAX(1,IPARI(18,N)))
          MULTIMP = MAX(MULTIMP,INTBUF_TAB(N)%S_CAND_MAX / MAX(1,IPARI(18,N))) 
          INTBUF_TAB(N)%S_CAND_MAX = 
     .             MAX(MULTIMP*IPARI(18,N),INTBUF_TAB(N)%S_CAND_MAX)
        ELSE
          MULTIMP = MAX(IPARI(23,N)+8,NINT(IPARI(23,N)*1.5))
        ENDIF

        CALL RESET_GAP(N,IPARI,INTBUF_TAB(N),FRIGAP)
        CALL UPGRADE_MULTIMP(N,MULTIMP,INTBUF_TAB(N))
        I_MEM = 0
        RESORT = 1
       ENDIF
C
       JINSCR=IPARI(10,N)
C reset GAP values (no more double read of interface)
c       IF(IDDLEVEL > 0)THEN
c         CALL RESET_GAP(N,IPARI,INTBUF_TAB(N),FRIGAP)
c       END IF

       NIN=N
       ID=NOM_OPT(1,NIN)
       CALL FRETITL2(TITR,
     .               NOM_OPT(LNOPT1-LTITR+1,NIN),LTITR)

       IF(N2D==0)THEN

        CALL ININT3(INSCR(N)%WA  ,X              ,IXS             ,IXC          ,PM       ,
     1              GEO          ,IPARI(1,N)     ,NIN             ,ITAB         ,MS       ,
     2              MWA          ,RWA            ,IXTG            ,IWRN         ,IKINE    ,
     3              IXT          ,IXP            ,IXR             ,NELEMINT     ,IDDLEVEL ,
     4              IFIEND       ,ALE_CONNECTIVITY ,NSNET           ,NMNET        ,IGRBRIC  ,
     5              IWCONT       ,NSNT           ,NMNT            ,NSN2T        ,NMN2T    ,
     6              IWCIN2       ,KNOD2ELS       ,KNOD2ELC        ,KNOD2ELTG    ,NOD2ELS  ,
     7              NOD2ELC      ,NOD2ELTG       ,IGRSURF         ,IKINE1       ,IELEM21  ,
     8              SH4TREE      ,SH3TREE        ,IPART           ,IPARTC       ,IPARTTG  ,
     9              THK          ,THK_PART       ,NOD2EL1D        ,KNOD2EL1D    ,IXS10    ,
     A              I_MEM        ,RESORT         ,SIXINT          ,IXS16        ,IXS20    ,
     B              ID           ,TITR           ,IREMNODE        ,NREMNODE     ,IPARTS   ,
     C              KXX          ,IXX            ,IGEO            ,INTERCEP     ,LELX     , 
     D              INTBUF_TAB(N),FILLSOL        ,PM_STACK        ,IWORKSH      ,KXIG3D   ,
     E              IXIG3D       ,TAGPRT_FRIC    ,INTBUF_FRIC_TAB ,IPARTT       ,IPARTP   ,
     F              IPARTX       ,IPARTR         ,NSN_MULTI_CONNEC,T2_ADD_CONNEC,T2_NB_CONNEC,
     F              T2_CONNEC    ,NOM_OPT        ,ICODE           ,ISKEW        ,IREMNODE_EDG)

        IF (I_MEM /= 0) GOTO 200
       ELSE
        CALL ININT2(
     1  INTBUF_TAB(N),INSCR(N)%WA  ,X         ,IXQ,
     2  PM           ,GEO          ,IPARI(1,N),NIN       ,ITAB     ,
     3  ITABM1       ,NUMNOD       ,IKINE     ,MWA       ,IPM      ,
     4  ID           ,TITR         ,KNOD2ELQ  ,NOD2ELQ   ,SEGQUADFR)
       ENDIF
  100 CONTINUE
C      
C--- IREM_I2 treatment has been removed at end of ININTR2 to take into account 
C---- the compaction of type2 w/ Itetra10=2  
C
      IF(IWRN/=0) THEN
        LEN_FILNAM = OUTFILE_NAME_LEN + ROOTLEN + 6
        FILNAM = OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:ROOTLEN)//'.coord'
        OPEN(UNIT=IOU2,FILE=FILNAM(1:LEN_FILNAM),STATUS='UNKNOWN',
     .       FORM='FORMATTED')
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(A)')'# NEW NODES COORDINATES'
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(I10,1P3G20.13)')
     .               (ITAB(I),X(1,I),X(2,I),X(3,I),I=1,NUMNOD)
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(A)')'# END OF NEW NODES COORDINATES'
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        CLOSE(UNIT=IOU2)
      ENDIF

C =============================================================
C DETECT INTERFACES WITH HIGH CPU COST 
C - auto-impacting interface on solid 
C - with low stiffness
C - included in the *main interface*
C
C *main interface* : interface that concern a lot of nodes with
C significantly different velocities between secnd and main
      displacement_max = 0.0
      NS = 0
      MAIN_INTERFACE_SIZE = -1 
      ID_MAIN_INTERFACE = -1
      IF(N2D==0)THEN
      IF((IDDLEVEL==0).AND.(DECTYP>=3.AND.DECTYP<=6))THEN

        CONDITION(1:NINTER) = .FALSE.
        avg_stiff(1:NINTER) = HUGE(1.0D0)
        ALLOCATE(TAG(NUMNOD))
        TAG(1:NUMNOD) = 0
        DO N=1,NINTER
          NTY=IPARI(7,N)
          IF( NTY == 7 ) THEN
            NMN      = IPARI(6,N)
            NSN      = IPARI(5,N)
            NRTM     = IPARI(4,N)
            NS = 0
C           CALL COUNT_SOLID_NODES(NOD2EL1D,KNOD2EL1D,INTBUF_TAB(N),NMN,NSN,NS)
C ----------- Count solid nodes
            DO I = 1,NMN
              NODE_ID =  INTBUF_TAB(N)%MSR(I)
              DO J = KNOD2ELS(NODE_ID)+1,KNOD2ELS(NODE_ID+1) 
                 NS = NS +1 
                 EXIT
              ENDDO
            ENDDO
C
            IF (NS > 9*(NMN) / 10) THEN
            ! Interface concerns mainly solids
              CPT = 0
              DO I = 1,NSN
                TAG(INTBUF_TAB(N)%NSV(I)) = 1
              ENDDO
              DO I = 1,NMN
                IF(TAG(INTBUF_TAB(N)%MSR(I)) == 1) CPT = CPT + 1
              ENDDO
              DO I = 1,NSN
                TAG(INTBUF_TAB(N)%NSV(I)) = 0
              ENDDO
              IF( abs(NSN - NMN) < NSN / 50 .AND. abs(NMN - CPT) < NMN/50) THEN
! Heursitic to find auto-impacting interface 
                 CONDITION(N) = .TRUE.
                 avg_stiff(N) = 0.0D0
                 DO I = 1,NRTM 
                   avg_stiff(N) = avg_stiff(N) + INTBUF_TAB(N)%STFM(I)/dble(NRTM)
                 ENDDO
                 IF(avg_stiff(N) == 0) THEN
                   DO I = 1,NSN 
                     avg_stiff(N) = avg_stiff(N) + INTBUF_TAB(N)%STFNS(I)/dble(NSN)
                   ENDDO
                 ENDIF
              ENDIF
            ENDIF
            CALL C_COMPUTE_VELOCITY(V, NUMNOD, INTBUF_TAB(N)%NSV, NSN, v1, f1)
            CALL C_COMPUTE_VELOCITY(V, NUMNOD, INTBUF_TAB(N)%MSR, NMN, v2, f2)
            displacement = (v1(1) - v2(1))**2 
     +                   + (v1(2) - v2(2))**2
     +                   + (v1(3) - v2(3))**2
            IF(f1 > NSN / 2 .AND. f2 > NMN / 2) THEN
              IF(displacement > displacement_max / 10.0 .AND.  
     +           NMN + NSN > MAIN_INTERFACE_SIZE) THEN
                IF( NMN + NSN > NUMNOD / 100 ) THEN
C main interface = interface that has the maximum displacement velocity
C between main and secnd, and that contains at least 1% of the nodes 
                  MAIN_INTERFACE_SIZE = NMN + NSN
                  ID_MAIN_INTERFACE = N
C                 CONDITION(N) = .FALSE.
                  displacement_max = displacement
                ENDIF ! NMN+ NSN
              ENDIF ! Displacement
            ENDIF ! f1 & f2
          ENDIF ! NTY
        ENDDO ! N

        TAG(1:NUMNOD) = 0
        IF(ID_MAIN_INTERFACE > 0) THEN
          NSN      = IPARI(5,ID_MAIN_INTERFACE)
          NRTM     = IPARI(4,ID_MAIN_INTERFACE)
          NMN      = IPARI(6,ID_MAIN_INTERFACE)
          main_stiff = 0.0D0
          DO I = 1,NRTM 
            main_stiff = main_stiff + INTBUF_TAB(ID_MAIN_INTERFACE)%STFM(I) / dble(NRTM)
          ENDDO
          IF(main_stiff == 0) THEN
            DO I = 1,NSN 
              main_stiff = main_stiff + INTBUF_TAB(ID_MAIN_INTERFACE)%STFNS(I) / dble(NSN)
            ENDDO
          ENDIF
c         WRITE(,*) "main stiff=",main_stiff
          DO I = 1,NSN
            TAG(INTBUF_TAB(ID_MAIN_INTERFACE)%NSV(I)) = 1
          ENDDO
          DO I = 1,NMN
            TAG(INTBUF_TAB(ID_MAIN_INTERFACE)%MSR(I)) = 1
          ENDDO
          min_stiff = HUGE(0.0D0) 
          DO N=1,NINTER
            IPARI(69,N) = 0
            IF(CONDITION(N) .AND. N /= ID_MAIN_INTERFACE) THEN
!            auto-impacting interface mainly made of solids
              CPT = 0
              NMN      = IPARI(6,N)
              NSN      = IPARI(5,N)
              DO I = 1,NSN
                IF(TAG(INTBUF_TAB(N)%NSV(I)) == 1) CPT = CPT +1
              ENDDO
              IF( CPT > (NSN)/3 ) THEN  
              ! the nodes of this interface are included in the main
              ! interface
                 min_stiff = MIN(min_stiff,avg_stiff(N)) 
                 IPARI(69,N) = 1
C Appel   routine poids noeuds interfaces
              ENDIF !CPT
            ENDIF ! CONDITION
          ENDDO ! NINTER
          DO N=1,NINTER
            IF(IPARI(69,N) ==  1) THEN
              NMN      = IPARI(6,N)
              NSN      = IPARI(5,N)
              IF(avg_stiff(N) < main_stiff / 10.0) THEN
                I =  0
                IF(avg_stiff(N) <= 3.0*min_stiff .AND. avg_stiff(N) < main_stiff / 200.0) I = 1
                IF(avg_stiff(N) <= 2.0*min_stiff .AND. avg_stiff(N) < main_stiff / 500.0) I = 4
c               WRITE(6,*) "Interface",IPARI(15,N),"weight=",I 
c               WRITE(6,*) "Stiff=",min_stiff,avg_stiff(N),main_stiff
                IF(I > 0) THEN 
                  WRITE(IOUT,*)"INFO: WEIGHT OF INTERFACE"
     .          ,IPARI(15,N), "INCREASED"
                  CALL IWCONTDD_NEW(INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%MSR,NSN,NMN,IWCONT,I)
                ENDIF
              ENDIF
            ENDIF ! CONDITION
          ENDDO ! NINTER
        ENDIF ! main interface

        DEALLOCATE(TAG)
      ENDIF
      ENDIF ! N2D

C
      DEALLOCATE(T2_ADD_CONNEC,T2_CONNEC)
C
      RETURN
      END
