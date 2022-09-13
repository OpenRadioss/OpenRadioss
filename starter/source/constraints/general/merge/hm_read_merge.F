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
Chd|  RBMERGE_MOD                   source/constraints/general/merge/hm_read_merge.F
Chd|-- called by -----------
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        RETRIRBY                      source/constraints/general/merge/hm_read_merge.F
Chd|        TRIRBMERGE                    source/constraints/general/merge/hm_read_merge.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE RBMERGE_MOD
c=======================================================================   
!                           RIGID BODY MERGE
c=======================================================================   
C-----------------------------------------------------------------------
      TYPE RBMERGE_
        INTEGER   :: ID ! - RIGID BODY identifier
        INTEGER   :: NBSECONDARY   ! Number of SECONDARY rigid body
        INTEGER, DIMENSION(:) , ALLOCATABLE :: IDSECONDARY ! SECONDARY rigid bodys attached to the MAIN
        INTEGER   :: IMAIN   ! 0 if this rigid body doesn't have a MAIN
!                              ! The Rigid body's MAIN 
        INTEGER   :: LEVEL   ! Rigid body level
!                                 = 0  ! if the rigid body is only a MAIN
!                                 = -1 ! if the rigid body has a MAIN
!                                 = -2 ! if the MAIN of the rigid body has a MAIN
!                                 = -3 ! and again
        INTEGER   :: FLAG_MAIN   ! Flag relating the rigid option merge to the MAIN
!                                 = -1 ! if the rigid body is only a MAIN
!                                 =  0 ! default value
!                                 =  1 ! option 1
!                                 =  2 ! option 2
!                                 =  3 ! option 3
        INTEGER   :: NNODE   ! Number of SECONDARY node
        INTEGER, DIMENSION(:) , ALLOCATABLE :: NODE ! SECONDARY node attached to the MAIN
        INTEGER, DIMENSION(:) , ALLOCATABLE :: FLAG_NODE ! Flag relating the rigid option merge to the MAIN
!                                 =  0 ! default value
!                                 =  1 ! option 1
!                                 =  2 ! option 2
!                                 =  3 ! option 3
!--------------
      END TYPE RBMERGE_
      END MODULE RBMERGE_MOD
C
Chd|====================================================================
Chd|  HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        TRIRBMERGE                    source/constraints/general/merge/hm_read_merge.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RBMERGE_MOD                   source/constraints/general/merge/hm_read_merge.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MERGE(
     .              MGRBY,SMGRBY,NPBY,LPBY,SLRBODY,
     .              RBY  ,NOM_OPT ,PTR_NOPT_RBMERGE,IGRNOD,
     .              ITAB,ITABM1,IBGR,IGRV, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE RBMERGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
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
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MGRBY(NMGRBY,*),NPBY(NNPBY,*),LPBY(*),
     .        SLRBODY,SMGRBY,ITABM1(*),ITAB(*)
C     REAL
      my_real
     .   RBY(NRBY,*)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_RBMERGE
      INTEGER IGRV(NIGRV,*),IBGR(*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,ID,IRBM,IRBS,NBMERGE,IGS,
     .        N,NOPT,UID, II, NOBJ
      INTEGER, DIMENSION(:) , ALLOCATABLE :: INDEX
      INTEGER, DIMENSION(:,:) , ALLOCATABLE :: INUM

      INTEGER IMAIN,ISECONDARY,FLAGG_OPT, FLAG_BOUCLE, FLAG_ERROR, 
     .        FLAG_DOUBLON, FLAG_DOUBLEMAIN, M_TYPE, S_TYPE, ID_OPT,
     .        IDNODE, NN, PRT_OPT
      INTEGER TABRB(NRBYKIN,2)
      INTEGER NB_MAIN(NRBYKIN),NSECONDARY(NRBYKIN),
     .        TAG1(NRBYKIN),TAG2(NRBYKIN)
      INTEGER TABBOUCLE(NRBYKIN+1), IDBOUCLE, LEVEL
      CHARACTER MYSTRING*100,MYLOOP*200
      CHARACTER MESS*40,MESS2*40,TITR*nchartitle,KEY2*ncharkey
      TYPE(RBMERGE_), DIMENSION(:), ALLOCATABLE :: RBMERGE
      INTEGER, DIMENSION(NUMNOD) :: ITAG, WORK
      INTEGER IWORK(70000)
      LOGICAL   IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR5
C-----------------------------------------------
      DATA MESS/'RIGID BODY MERGE DEFINITION             '/
C-----------------------------------------------
      J = 0
      N = 0
      NOPT = 0
      NOBJ = 0
C--------------------------------------------------
C START BROWSING MODEL RBODY
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/MERGE/RBODY')
C--------------------------------------------------
      DO I=1,NRBMERGE
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY2)

C--------------------------------------------------
C WRITE TITLE IN OUT FILE
C--------------------------------------------------
        NOPT=NOPT+1
        NOM_OPT(1,PTR_NOPT_RBMERGE+NOPT)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RBMERGE+NOPT),LTITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('NB_SUBOBJVE',NOBJ,IS_AVAILABLE,LSUBMODEL)
C
        DO J=1,NOBJ
            N = N + 1
            CALL HM_GET_INT_ARRAY_INDEX('Main_ID',IMAIN,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('M_type',M_TYPE,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Secon_ID',ISECONDARY,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('S_type',S_TYPE,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iflag',FLAGG_OPT,J,IS_AVAILABLE,LSUBMODEL)
            IF (IMAIN /= 0) THEN 
              IF(M_TYPE == 0) M_TYPE=1                   
              IF(S_TYPE == 0) S_TYPE=1 
              IF(FLAGG_OPT == 0) FLAGG_OPT=2  
              MGRBY(1,N)=IMAIN
              MGRBY(2,N)=M_TYPE
              MGRBY(3,N)=ISECONDARY
              MGRBY(4,N)=S_TYPE
              MGRBY(5,N)=FLAGG_OPT
              MGRBY(6,N)=ID!NOPT
              MGRBY(7,N)=NOPT
            ENDIF  ! IMAIN /= 0
        ENDDO

      ENDDO
C
      ALLOCATE (RBMERGE(NRBYKIN))
      DO I=1,NRBYKIN
        ALLOCATE (RBMERGE(I)%IDSECONDARY(NRBYKIN))
        RBMERGE(I)%NBSECONDARY=0
        ALLOCATE (RBMERGE(I)%NODE(NXTRA_NODE))
        ALLOCATE (RBMERGE(I)%FLAG_NODE(NXTRA_NODE))
        RBMERGE(I)%NNODE=0
        RBMERGE(I)%LEVEL=0
        RBMERGE(I)%FLAG_MAIN = 0
      ENDDO

c      
      NBMERGE = 0
      TABRB(:,:)=0
      TAG1(:)=0
      TAG2(:)=0
      FLAG_ERROR = 0                          
      NB_MAIN(:)=0
      ITAG(1:NUMNOD) = 0
      FLAG_DOUBLON=0
c
      WRITE(IOUT,1000)
      PRT_OPT=0
c
      DO I=1,SMGRBY ! 1ERE PASSE POUR LES MERGE RBODY/RBODY
        IMAIN = MGRBY(1,I)
        M_TYPE = MGRBY(2,I)
        ISECONDARY = MGRBY(3,I)
        S_TYPE = MGRBY(4,I)
        FLAGG_OPT = MGRBY(5,I)
        ID_OPT = MGRBY(6,I)
        IF(S_TYPE == 1) THEN ! SECONDARY IS A RBODY   
c
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RBMERGE+MGRBY(7,I)),LTITR) 
c
          IF(PRT_OPT /= MGRBY(6,I)) THEN
            WRITE(IOUT,1100) MGRBY(6,I),TRIM(TITR)
            PRT_OPT = MGRBY(6,I)
          ENDIF
C------------------------------------
c  TESTS D'EXISTENCE SUR LES MAIN ET SECONDARYS
C------------------------------------
          IRBM=0
          DO K=1,NRBYKIN
            IF (IMAIN == NPBY(6,K)) THEN
              IRBM=K                     
              EXIT
            ENDIF 
          ENDDO
          IF (IRBM == 0)THEN  !  l'ID du rbody n'existe pas    
            CALL ANCMSG(MSGID=1636,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID_OPT,
     .                  C1=TITR,
     .                  I2=IMAIN)                           
          ENDIF 
         
          IRBS=0                                  
          DO K=1,NRBYKIN  
            IF (ISECONDARY == NPBY(6,K)) THEN
              IRBS=K                     
              EXIT
            ENDIF                                                     
          ENDDO
          IF (IRBS == 0)THEN  !  l'ID du rbody n'existe pas    
            CALL ANCMSG(MSGID=1636,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID_OPT,
     .                  C1=TITR,
     .                  I2=ISECONDARY)                              
          ENDIF 
C------------------------------------
c  TRI DES RELATIONS LUES
C------------------------------------
          IF((IRBM /= 0).AND.(IRBS /= 0)) THEN 

            WRITE(IOUT,1200) IMAIN, ISECONDARY, FLAGG_OPT
            MESS2 ='SECONDARY RIGID BODY '

            FLAG_DOUBLON=0
            FLAG_DOUBLEMAIN = 0                    
            IF(NB_MAIN(IRBS) >= 1) THEN 
              DO L=1,NBMERGE
                IF(TABRB(L,2) == IRBS) THEN
                  IF(TABRB(L,1) /= IRBM) THEN ! le SECONDARY est deja SECONDARY d'un autre
                    CALL ANCMSG(MSGID=1028,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID_OPT,
     .                          C1=TITR,
     .                          I2=ISECONDARY)
                    FLAG_DOUBLEMAIN = 1  
                    FLAG_ERROR = 1
                  ELSE ! la relation de merge est en double
                    CALL ANCMSG(MSGID=1027,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID_OPT,
     .                          C1=TITR,
     .                          I2=IMAIN,
     .                          C2=MESS2,
     .                          I3=ISECONDARY,
     .                          I4=FLAGG_OPT)
                    FLAG_DOUBLON = 1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
            IF((FLAG_DOUBLON + FLAG_DOUBLEMAIN) == 0) THEN
              NBMERGE = NBMERGE + 1
              NB_MAIN(IRBS) = NB_MAIN(IRBS) + 1 
              NSECONDARY(IRBM) = NSECONDARY(IRBM) + 1
              TABRB(NBMERGE,1) = IRBM ! MAIN
              TABRB(NBMERGE,2) = IRBS ! SECONDARY
c                    
              RBMERGE(IRBS)%ID = ISECONDARY
              RBMERGE(IRBS)%IMAIN = IRBM
              RBMERGE(IRBS)%FLAG_MAIN = FLAGG_OPT 
c                      
              RBMERGE(IRBM)%ID = IMAIN
              RBMERGE(IRBM)%NBSECONDARY = RBMERGE(IRBM)%NBSECONDARY+1
              RBMERGE(IRBM)%IDSECONDARY(RBMERGE(IRBM)%NBSECONDARY) = IRBS                      
            ELSEIF(FLAG_DOUBLON == 1) THEN ! Le flag est celui qu'on lit en dernier 
              RBMERGE(IRBS)%FLAG_MAIN = FLAGG_OPT
            ENDIF
          ENDIF ! IRBM /= 0 IRBS /= 0
        ENDIF ! S_TYPE == 1
      ENDDO
C
      DO I=1,SMGRBY ! 2ERE PASSE POUR LES MERGE RBODY/NODE & SET OF NODES
        IMAIN = MGRBY(1,I)
        M_TYPE = MGRBY(2,I)
        ISECONDARY = MGRBY(3,I)
        S_TYPE = MGRBY(4,I)
        FLAGG_OPT = MGRBY(5,I)
        ID_OPT = MGRBY(6,I)
        IF((S_TYPE == 2).OR.(S_TYPE == 3)) THEN
c
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RBMERGE+MGRBY(7,I)),LTITR) 
c
          IF(PRT_OPT /= MGRBY(6,I)) THEN
            WRITE(IOUT,1100) MGRBY(6,I),TRIM(TITR)
            PRT_OPT = MGRBY(6,I)
          ENDIF
C------------------------------------
c  TESTS D'EXISTENCE SUR LES MAIN ET SECONDARYS
C------------------------------------
          IRBM=0
          DO K=1,NRBYKIN
            IF (IMAIN == NPBY(6,K)) THEN
              IRBM=K                     
              EXIT
            ENDIF 
          ENDDO
          IF (IRBM == 0)THEN  !  l'ID du rbody n'existe pas    
            CALL ANCMSG(MSGID=1636,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID_OPT,
     .                  C1=TITR,
     .                  I2=IMAIN)                           
          ENDIF 
         
          IDNODE=0
          NN=0
          IF(S_TYPE == 2) THEN ! SECONDARY IS A NODE
            IDNODE = USR2SYS(MGRBY(3,I),ITABM1,MESS,ID)
          ELSEIF(S_TYPE == 3) THEN ! SECONDARY IS A GRNOD
            NN = NODGRNR5(MGRBY(3,I),IGS,WORK,IGRNOD,ITABM1,MESS) 
          ENDIF    


          IF((IRBM /= 0).AND.(IDNODE /= 0)) THEN ! SECONDARY IS A NODE
 
            WRITE(IOUT,1300) IMAIN, ISECONDARY, FLAGG_OPT
            MESS2 ='SECONDARY NODE '

            IF(ITAG(IDNODE) == 0) THEN
              ITAG(IDNODE) = IRBM
            ELSEIF(ITAG(IDNODE) == IRBM) THEN
              FLAG_DOUBLON  = 1
              CALL ANCMSG(MSGID=1027,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID_OPT,
     .                    C1=TITR,
     .                    I2=IMAIN,
     .                    C2=MESS2,
     .                    I3=ISECONDARY,
     .                    I4=FLAGG_OPT)
            ENDIF

            IF(FLAG_DOUBLON == 0) THEN
              RBMERGE(IRBM)%ID = IMAIN
              RBMERGE(IRBM)%NNODE = RBMERGE(IRBM)%NNODE+1
              RBMERGE(IRBM)%NODE(RBMERGE(IRBM)%NNODE) = IDNODE
              RBMERGE(IRBM)%FLAG_NODE(RBMERGE(IRBM)%NNODE) = FLAGG_OPT
            ELSE ! Le flag est celui qu'on lit en dernier 
              DO II=1,RBMERGE(IRBM)%NNODE
                IF(RBMERGE(IRBM)%NODE(II) == IDNODE) THEN
                  RBMERGE(IRBM)%FLAG_NODE(II) = FLAGG_OPT
                ENDIF
              ENDDO
            ENDIF
          ENDIF 

          IF((IRBM /= 0).AND.(NN /= 0)) THEN   ! SECONDARY IS A SET OF NODE

            WRITE(IOUT,1400) IMAIN, ISECONDARY, FLAGG_OPT
            WRITE(IOUT,1410) (ITAB(WORK(J)),J=1,NN)
            MESS2 ='SECONDARY NODE '

            RBMERGE(IRBM)%ID = IMAIN
            DO J=1,NN
              FLAG_DOUBLON  = 0
              FLAG_DOUBLEMAIN = 0
              IF(ITAG(WORK(J)) == 0) THEN
                ITAG(WORK(J)) = IRBM
              ELSEIF(ITAG(WORK(J)) == IRBM) THEN
                FLAG_DOUBLON  = 1
                CALL ANCMSG(MSGID=1027,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID_OPT,
     .                      C1=TITR,
     .                      I2=IMAIN,
     .                      C2=MESS2,
     .                      I3=ITAB(WORK(J)),
     .                      I4=FLAGG_OPT)
              ENDIF

              IF(FLAG_DOUBLON == 0) THEN
                RBMERGE(IRBM)%ID = IMAIN
                RBMERGE(IRBM)%NNODE = RBMERGE(IRBM)%NNODE+1
                RBMERGE(IRBM)%NODE(RBMERGE(IRBM)%NNODE) = WORK(J)
                RBMERGE(IRBM)%FLAG_NODE(RBMERGE(IRBM)%NNODE) = FLAGG_OPT
              ELSE ! Le flag est celui qu'on lit en dernier 
                DO II=1,RBMERGE(IRBM)%NNODE
                  IF(RBMERGE(IRBM)%NODE(II) == WORK(J)) THEN
                    RBMERGE(IRBM)%FLAG_NODE(II) = FLAGG_OPT
                  ENDIF
                ENDDO
              ENDIF
            ENDDO  

          ENDIF 
        ENDIF ! S_TYPE = 2 OR 3
C-----             
      ENDDO
C------------------------------------
C TRI DES XTRA_NODES PAR FLAGS CROISSANTS
C------------------------------------
      DO I=1,NRBYKIN
        IF(RBMERGE(I)%NNODE > 0) THEN
          ALLOCATE(INDEX(2*RBMERGE(I)%NNODE))
          INDEX(1:2*RBMERGE(I)%NNODE) = 0
          ALLOCATE(INUM(RBMERGE(I)%NNODE,2))
          DO J=1,RBMERGE(I)%NNODE
            INDEX(J) = J
            INUM(J,1) = RBMERGE(I)%FLAG_NODE(J)
            INUM(J,2) = RBMERGE(I)%NODE(J)
          ENDDO
          CALL MY_ORDERS(0,IWORK,RBMERGE(I)%FLAG_NODE,INDEX,RBMERGE(I)%NNODE,1)
          DO J=1,RBMERGE(I)%NNODE
            RBMERGE(I)%FLAG_NODE(J) = INUM(INDEX(J),1)
            RBMERGE(I)%NODE(J) = INUM(INDEX(J),2)
          ENDDO
          DEALLOCATE(INDEX)
          DEALLOCATE(INUM)       
        ENDIF
      ENDDO
C------------------------------------
C  TOUTES LES RELATIONS ONT ETE TRIEES (RBODY, NODE, GRNOD)
C------------------------------------
      WRITE(IOUT,2000)
C------------------------------------
C  TAG ET RECHERCHE DES LOOPS
C------------------------------------
      DO I=1,NRBYKIN
        IF(NB_MAIN(I) == 0) THEN
           CALL RBTAG1DOWN(TABRB,I,TAG1,NBMERGE)   
        ENDIF
      ENDDO
C      
      DO I=1,NRBYKIN
        IF((TAG1(I) == 0).AND.(TAG2(I) == 0).AND.(RBMERGE(I)%NBSECONDARY>0)) THEN
          FLAG_BOUCLE = 0
          IDBOUCLE = 0
          TABBOUCLE(:) = 0
          CALL RBTAG2DOWN(I,TAG2,RBMERGE,FLAG_BOUCLE,TABBOUCLE,IDBOUCLE)  
          IF (FLAG_BOUCLE == 1) THEN
            WRITE(MYLOOP,*) TABBOUCLE(1)
            MYLOOP = ADJUSTL(MYLOOP)
            DO J=2,NRBYKIN+1
              IF(TABBOUCLE(J) == 0) EXIT
              WRITE(MYSTRING,*) TABBOUCLE(J)
              MYSTRING = ADJUSTL(MYSTRING)              
              MYLOOP = MYLOOP(1:LEN(TRIM(MYLOOP))) //' -> '// MYSTRING
            ENDDO
            CALL ANCMSG(MSGID=1029,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1=MYLOOP,
     .                  PRMOD=MSG_CUMU)
            FLAG_ERROR = 1
          ENDIF
        ENDIF
      ENDDO
      IF(FLAG_ERROR == 1) THEN
        CALL ANCMSG(MSGID=1029,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              PRMOD=MSG_PRINT)
      ENDIF
C------------------------------------
C SI PAS DE LOOP, CONSTRUCTION DES HIERARCHIES
C------------------------------------
      IF(FLAG_ERROR == 0) THEN 
        DO I=1,NRBYKIN  ! RB level calculation      
          IF(NB_MAIN(I) == 0) THEN
            LEVEL = 0
            CALL RBLEVELDOWN(NPBY,RBMERGE,I,LEVEL) 
          ENDIF
        ENDDO
C------------------------------------
C ORDONNANCEMENT DES NPBY, LPBY ET RBY, OUTPUTS DES HIERARCHIES
C------------------------------------
        CALL TRIRBMERGE(RBMERGE,NPBY  ,LPBY   ,SLRBODY,
     .                  RBY    ,NOM_OPT, ITAB,IBGR,IGRV)
      ENDIF
c
      DEALLOCATE(RBMERGE)
cC-----------
      RETURN
C
1000  FORMAT(/
     . '      RIGID BODY MERGE DEFINITIONS '/
     . '      ---------------------- '/)
1100  FORMAT( /5X,'RIGID BODY MERGE ID ',I10,1X,A)
1200  FORMAT(/10X,'MAIN RIGID BODY ID                    ',I10
     .       /10X,'SECONDARY RIGID BODY ID                     ',I10
     .       /10X,'IFLAG                                   ',I10)
1300  FORMAT(/10X,'MAIN RIGID BODY ID                    ',I10
     .       /10X,'SECONDARY NODE ID                           ',I10
     .       /10X,'IFLAG                                   ',I10)
1400  FORMAT(/10X,'MAIN RIGID BODY ID                    ',I10
     .       /10X,'SECONDARY SET OF NODE ID                    ',I10
     .       /10X,'IFLAG                                   ',I10
     .       /10X,'SET OF NODES                            ')
1410  FORMAT( 10X,10I10)
2000  FORMAT(/
     . '      RIGID BODY MERGE CONSTRUCTION '/
     . '      ---------------------- '/)
C
      END SUBROUTINE HM_READ_MERGE
C
C-----------------------------------------------
      RECURSIVE SUBROUTINE RBTAG1DOWN(TABRB,IDRB,TAG1,NBMERGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDRB,NBMERGE, TAG1(NRBYKIN),TABRB(NRBYKIN,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDRBS
C-----------------------------------------------
      TAG1(IDRB) = 1
c     
      DO I=1,NBMERGE
        IF(TABRB(I,1) == IDRB) THEN
           IDRBS = TABRB(I,2)
           CALL RBTAG1DOWN(TABRB,IDRBS,TAG1,NBMERGE)    
        ENDIF
      ENDDO
C
      RETURN
      END
C 
C-----------------------------------------------
      RECURSIVE SUBROUTINE RBTAG2DOWN(IDRB,TAG2,RBMERGE,
     .                                FLAG_BOUCLE,TABBOUCLE,IDBOUCLE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RBMERGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDRB,NBMERGE, TAG2(NRBYKIN),FLAG_BOUCLE
      INTEGER TABBOUCLE(NRBYKIN+1), IDBOUCLE
      TYPE (RBMERGE_)   , DIMENSION(NRBYKIN)   :: RBMERGE      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDRBS
C-----------------------------------------------
      TAG2(IDRB) = 1
c     
      DO I=1,RBMERGE(IDRB)%NBSECONDARY
        IF(TAG2(RBMERGE(IDRB)%IDSECONDARY(I)) == 0) THEN
           IDRBS = RBMERGE(IDRB)%IDSECONDARY(I)
           CALL RBTAG2DOWN(IDRBS,TAG2,RBMERGE,FLAG_BOUCLE,
     .                     TABBOUCLE,IDBOUCLE)
        ELSE   ! on a trouve la boucle
           FLAG_BOUCLE = 1
           IDRBS = RBMERGE(IDRB)%IDSECONDARY(I)
           IDBOUCLE = IDBOUCLE+1
           TABBOUCLE(IDBOUCLE)=RBMERGE(IDRBS)%ID
           EXIT
        ENDIF
      ENDDO
      
      IF(FLAG_BOUCLE == 1) THEN
        IDBOUCLE = IDBOUCLE+1
        TABBOUCLE(IDBOUCLE)=RBMERGE(IDRB)%ID       
      ELSE
        TAG2(IDRB) = 0
      ENDIF
C
      RETURN
      END   
C
C-----------------------------------------------
      RECURSIVE SUBROUTINE RBLEVELDOWN(NPBY,RBMERGE,IDRB,LEVEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RBMERGE_MOD
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
      INTEGER NPBY(NNPBY,*),IDRB, LEVEL
      TYPE (RBMERGE_), DIMENSION(NRBYKIN)   :: RBMERGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISECONDARY, NBSECONDARY
C-----------------------------------------------
!   RBMERGE(IRBM)%ID   :: Rigid body identifier
!   RBMERGE(IRBM)%NBSECONDARY   :: Number of SECONDARY rigid body
!   RBMERGE(IRBM)%IMAIN   :: 0 if this rigid body doesn't have a MAIN
!                              X : The Rigid body's MAIN 
!   RBMERGE(IRBM)%LEVEL   :: Rigid body level
!   RBMERGE(IRBM)%FLAG_MAIN   :: Flag relating the rigid option merge to the MAIN
!   RBMERGE(IRBM)%IDSECONDARY(J) :: SECONDARY rigid bodys attached to the MAIN
C-----------------------------------------------
      NBSECONDARY = RBMERGE(IDRB)%NBSECONDARY
      ISECONDARY = 0
      NPBY(12,IDRB) = LEVEL
      LEVEL = LEVEL - 1
      NPBY(13,IDRB) = RBMERGE(IDRB)%FLAG_MAIN
c
      DO WHILE (ISECONDARY < NBSECONDARY) 
        ISECONDARY = ISECONDARY + 1
        RBMERGE(RBMERGE(IDRB)%IDSECONDARY(ISECONDARY))%LEVEL=RBMERGE(IDRB)%LEVEL-1
        CALL RBLEVELDOWN(NPBY,RBMERGE,RBMERGE(IDRB)%IDSECONDARY(ISECONDARY),LEVEL)
      ENDDO
C
      LEVEL = LEVEL + 1

      RETURN
      END
C  
Chd|====================================================================
Chd|  TRIRBMERGE                    source/constraints/general/merge/hm_read_merge.F
Chd|-- called by -----------
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SPMDSET                       source/constraints/general/rbody/spmdset.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RBMERGE_MOD                   source/constraints/general/merge/hm_read_merge.F
Chd|====================================================================
      SUBROUTINE TRIRBMERGE(RBMERGE,NPBY   ,LPBY    ,SLRBODY,
     .                      RBY    ,NOM_OPT,ITAB    ,IBGR   ,IGRV   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE RBMERGE_MOD
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
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
#include      "tabsiz_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*),SLRBODY,ITAB(*)
      TYPE (RBMERGE_)   , DIMENSION(NRBYKIN)   :: RBMERGE
C     REAL
      my_real
     .   RBY(NRBY,*)
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER IGRV(NIGRV,*),IBGR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,KK,M,N,ID,OFFSET,OFFSETID,FLAG_RB
      INTEGER PILE(NRBYKIN), IDPILE, NPBY_TMP(NNPBY,NRBYKIN),
     .        INDEX(NRBYKIN),LPBY_TMP(SLRBODY),
     .        NOM_OPT_TMP(LNOPT1,NRBYKIN),INODE, IKREM,ISPHER,
     .        ICDG,CNT,NN,IAD
C     REAL
      my_real
     .   RBY_TMP(NRBY,NRBYKIN)
      my_real
     .     BID, DX, DY, DZ, DMSTR, DELT, DTMP
      INTEGER NBSECONDARY, IDIR, IKINE1(3*NUMNOD), NSL, NSL_XTRA
      INTEGER, DIMENSION(NUMNOD) :: ITAG 
      INTEGER, DIMENSION(NXTRA_NODE) :: LIST_XTRA 
      CHARACTER MESS*40,TITR*nchartitle    
C-----------------------------------------------
!   RBMERGE(IRBM)%ID   :: Rigid body identifier
!   RBMERGE(IRBM)%NBSECONDARY   :: Number of SECONDARY rigid body
!   RBMERGE(IRBM)%IMAIN   :: 0 if this rigid body doesn't have a MAIN
!                              X : The Rigid body's MAIN 
!   RBMERGE(IRBM)%LEVEL   :: Rigid body level
!   RBMERGE(IRBM)%FLAG_MAIN   :: Flag relating the rigid option merge to the MAIN
!   RBMERGE(IRBM)%IDSECONDARY(J) :: SECONDARY rigid bodys attached to the MAIN
C-----------------------------------------------
      INDEX(1:NRBYKIN) = 0
      OFFSET = 0
      OFFSETID = 0
      ITAG(1:NUMNOD) = 0
      LPBY_TMP(1:SLRBODY)=0
      DO N=1,NRBYKIN    
        IF(RBMERGE(N)%LEVEL == 0) THEN
          IF(RBMERGE(N)%NBSECONDARY > 0) THEN
            PILE(:)=0
            IDPILE=0
            CALL RB_EXPLORE(N,RBMERGE,PILE,IDPILE)
            DO K=1,IDPILE
              INDEX(OFFSETID+K)=PILE(IDPILE+1-K)
            ENDDO
            OFFSETID = OFFSETID+IDPILE
          ELSE
            INDEX(NRBYKIN-OFFSET)=N
            OFFSET = OFFSET + 1
          ENDIF
        ENDIF  
        NOM_OPT_TMP(1:LNOPT1,N)=NOM_OPT(1:LNOPT1,N)
        DO J=1,NNPBY
          NPBY_TMP(J,N)=NPBY(J,N)
        ENDDO
        DO J=1,NRBY
          RBY_TMP(J,N)=RBY(J,N) 
        ENDDO
        K=NPBY(11,N)
        DO J=1,NPBY(2,N)
          LPBY_TMP(K+J)=LPBY(K+J) 
        ENDDO
      ENDDO 
      LPBY(1:SLRBODY)=0
      K=0      
      DO N=1,NRBYKIN
        NOM_OPT(1:LNOPT1,N)=NOM_OPT_TMP(1:LNOPT1,INDEX(N))
        DO J=1,NNPBY
          NPBY(J,N)=NPBY_TMP(J,INDEX(N))
        ENDDO
        NPBY(11,N)=K
        DO J=1,NRBY
          RBY(J,N)=RBY_TMP(J,INDEX(N)) 
        ENDDO
        DO J=1,NPBY_TMP(2,INDEX(N))
          LPBY(K+J)=LPBY_TMP(NPBY_TMP(11,INDEX(N))+J)
        ENDDO
        K=K+NPBY_TMP(2,INDEX(N))
      ENDDO
c
C------------------------------------
c     Ajout des XTRA_NODES en triant les doublons 
C     entre les RB qui seront fusionnes et ceux 
C     qui ne le seront pas
C------------------------------------
      INODE = 0
      LPBY_TMP(1:SLRBODY)=0
      DO N=1,NRBYKIN
        K=NPBY(11,N)
        NPBY(11,N)=INODE                  
        DO J=1,NPBY(2,N)
          IF(ITAG(LPBY(K+J)) == 0) THEN 
            INODE = INODE+1
            ITAG(LPBY(K+J)) = 1
            LPBY_TMP(INODE)=LPBY(K+J)             
          ELSE
            NPBY(2,N)=NPBY(2,N)-1
          ENDIF
        ENDDO  
        NSL_XTRA = 0
        DO J=1,RBMERGE(INDEX(N))%NNODE
          IF(ITAG(RBMERGE(INDEX(N))%NODE(J)) <= 0) THEN 
            NSL_XTRA = NSL_XTRA+1
            INODE = INODE+1
            LPBY_TMP(INODE)=RBMERGE(INDEX(N))%NODE(J) 
            IF(RBMERGE(INDEX(N))%FLAG_NODE(J) == 1) NPBY(14,N)=NPBY(14,N)+1
            IF(RBMERGE(INDEX(N))%FLAG_NODE(J) == 2) NPBY(15,N)=NPBY(15,N)+1
            IF(RBMERGE(INDEX(N))%FLAG_NODE(J) == 3) NPBY(16,N)=NPBY(16,N)+1
            IF(ITAG(RBMERGE(INDEX(N))%NODE(J)) == -1) THEN
              CALL ANCMSG(MSGID=1644,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITAB(RBMERGE(INDEX(N))%NODE(J)),
     .                    PRMOD=MSG_CUMU)
            ENDIF
            ITAG(RBMERGE(INDEX(N))%NODE(J)) = 1
          ENDIF
        ENDDO
C
        NPBY(2,N)=NPBY(2,N)+NSL_XTRA 
        IF(NPBY(12,N) == 0) THEN    
          NSL_XTRA=NPBY(14,N)+NPBY(15,N)+NPBY(16,N)
          K=NPBY(11,N) 
          DO J=1,NPBY(2,N)-NSL_XTRA
            ITAG(LPBY_TMP(K+J)) = 0
          ENDDO
          DO J=NPBY(2,N)-NSL_XTRA+1,NPBY(2,N)
            ITAG(LPBY_TMP(K+J)) = -1
          ENDDO
          DO I=N-1,1,-1
            IF((NPBY(12,I)) < 0) THEN
              K=NPBY(11,I) 
              NSL_XTRA=NPBY(14,I)+NPBY(15,I)+NPBY(16,I) 
              DO J=1,NPBY(2,I)-NSL_XTRA
                ITAG(LPBY_TMP(K+J)) = 0
              ENDDO
              DO J=NPBY(2,I)-NSL_XTRA+1,NPBY(2,I)
                ITAG(LPBY_TMP(K+J)) = -1
              ENDDO
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      CALL ANCMSG(MSGID=1644,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)

      LPBY(1:SLRBODY)=0
      DO N=1,NRBYKIN
        K=NPBY(11,N)
        DO J=1,NPBY(2,N)
          LPBY(K+J)=LPBY_TMP(K+J)
        ENDDO
      ENDDO
C
      DO N=1,NRBYKIN
        NSL_XTRA = NPBY(14,N)+NPBY(15,N)+NPBY(16,N)
        K = NPBY(11,N)+NPBY(2,N)-NSL_XTRA
C------------------------------------
C  SPMD TREATMENT ONLY FOR XTRA_NODES
C------------------------------------
        IF (IMACH == 3) THEN
          CALL SPMDSET(N,NPBY,NNPBY,LPBY,NSL_XTRA,K)
        ENDIF
      ENDDO
C------------------------------------
C     tag des xtra noeuds SECONDARYs rby avec gravite
C     pour calcul du travail des forces externes
C-------------------------------------
      DO I=1,NUMNOD
        ITAG(I)=0
      ENDDO
      K=0
      DO N=1,NRBYKIN
        NSL_XTRA = NPBY(14,N)+NPBY(15,N)+NPBY(16,N)
        K = NPBY(11,N)+NPBY(2,N)-NSL_XTRA
        IF(NPBY(7,N)/=0)THEN
         DO I=1,NSL_XTRA
          ITAG(LPBY(I+K))=1
         ENDDO
        ENDIF
      ENDDO
      DO K=1,NGRAV
        NN =IGRV(1,K)
        IAD=IGRV(4,K)
        DO I=1,NN
          N=IBGR(I+IAD-1)
          IF(ITAG(N) == 1)IBGR(I+IAD-1) = -N
        ENDDO
      ENDDO
C------------------------------------
C     Sorties dans 0.out des bilans des fusions
C     pour les top level MAIN
C-------------------------------------
      DO N=1,NRBYKIN
        NBSECONDARY=0
        LIST_XTRA(:)=0
        KK=0

        IF(NPBY(12,N) == 0) THEN
          NSL_XTRA=NPBY(14,N)+NPBY(15,N)+NPBY(16,N)
          K = NPBY(11,N) + NPBY(2,N) - NSL_XTRA
          DO J=1,NSL_XTRA
            LIST_XTRA(KK+J) = LPBY(K+J)
          ENDDO
          KK=KK+NSL_XTRA

          DO I=N-1,1,-1
            IF(NPBY(12,I) == 0) EXIT
            NPBY(4,I) = NPBY(4,N) ! Same ISENS for MAIN RB's SECONDARYs
            NBSECONDARY=NBSECONDARY+1

            NSL_XTRA=NPBY(14,I)+NPBY(15,I)+NPBY(16,I)
            K = NPBY(11,I) + NPBY(2,I) - NSL_XTRA
            DO J=1,NSL_XTRA
              LIST_XTRA(KK+J) = LPBY(K+J)
            ENDDO
            KK=KK+NSL_XTRA

          ENDDO
        ENDIF

        IF((NBSECONDARY + KK) > 0) THEN
          WRITE(IOUT,1000) NPBY(6,N)
          IF(NBSECONDARY > 0) THEN
            WRITE(IOUT,1100) (NPBY(6,N-I),I=1,NBSECONDARY)
          ENDIF
          IF(KK > 0) THEN
            WRITE(IOUT,1200) (ITAB(LIST_XTRA(J)),J=1,NSL_XTRA)
          ENDIF
        ENDIF

      ENDDO
C
      RETURN
C
1000  FORMAT(/5X,'MAIN RIGID BODY ID ',I10)
1100  FORMAT(5X,'SECONDARY RIGID BODIES ID',10I10)
1200  FORMAT(5X,'SECONDARY EXTRA NODES ID ',10I10)
      END
C
      RECURSIVE SUBROUTINE RB_EXPLORE(IDRB,RBMERGE,PILE,IDPILE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RBMERGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PILE(NRBYKIN), IDPILE, IDRB
      TYPE (RBMERGE_)   , DIMENSION(*)   :: RBMERGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISECONDARY, NBSECONDARY
C-----------------------------------------------
!   RBMERGE(IRBM)%ID   :: Rigid body identifier
!   RBMERGE(IRBM)%NBSECONDARY   :: Number of SECONDARY rigid body
!   RBMERGE(IRBM)%IMAIN   :: 0 if this rigid body doesn't have a MAIN
!                              X : The Rigid body's MAIN 
!   RBMERGE(IRBM)%LEVEL   :: Rigid body level
!   RBMERGE(IRBM)%FLAG_MAIN   :: Flag relating the rigid option merge to the MAIN
!   RBMERGE(IRBM)%IDSECONDARY(J) :: SECONDARY rigid bodys attached to the MAIN
C-----------------------------------------------
      IDPILE = IDPILE + 1
      PILE(IDPILE) = IDRB
      NBSECONDARY = RBMERGE(IDRB)%NBSECONDARY
      ISECONDARY = 0
c
      DO WHILE (ISECONDARY < NBSECONDARY) 
        ISECONDARY = ISECONDARY + 1
        CALL RB_EXPLORE(RBMERGE(IDRB)%IDSECONDARY(ISECONDARY),RBMERGE,PILE,IDPILE)
      ENDDO
C
      RETURN
      END     
C
Chd|====================================================================
Chd|  RETRIRBY                      source/constraints/general/merge/hm_read_merge.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RBMERGE_MOD                   source/constraints/general/merge/hm_read_merge.F
Chd|====================================================================
      SUBROUTINE RETRIRBY(NPBY ,LPBY ,RBY ,NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE RBMERGE_MOD
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
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
#include      "tabsiz_c.inc"
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*)
C     REAL
      my_real
     .   RBY(NRBY,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,OFFSET,OFFSETEND,NRBYKINM
      INTEGER NPBY_TMP(NNPBY,NRBYKIN+NRBYLAG),
     .        INDEX(NRBYKIN+NRBYLAG),
     .        NOM_OPT_TMP(LNOPT1,NRBYKIN+NRBYLAG),
     .        NEW_SIZE,CPT_RBY,CPT_RBY_SECONDARY
C     REAL
      my_real
     .   RBY_TMP(NRBY,NRBYKIN+NRBYLAG)
C-----------------------------------------------
C
      NEW_SIZE = 0       
      DO I=1,NRBYKIN+NRBYLAG
        NOM_OPT_TMP(1:LNOPT1,I)=NOM_OPT(1:LNOPT1,I)
        DO J=1,NNPBY
          NPBY_TMP(J,I)=NPBY(J,I)
        ENDDO
        DO J=1,NRBY
          RBY_TMP(J,I)=RBY(J,I) 
        ENDDO
        IF (NPBY(12,I)==0) NEW_SIZE = NEW_SIZE+1
      ENDDO
C
      CPT_RBY = NEW_SIZE
      CPT_RBY_SECONDARY = NRBYKIN+NRBYLAG
      DO I=NRBYKIN+NRBYLAG,1,-1  
        IF (NPBY_TMP(12,I)==0) THEN
          NOM_OPT(1:LNOPT1,CPT_RBY)=NOM_OPT_TMP(1:LNOPT1,I)
          NPBY(1:NNPBY,CPT_RBY)=NPBY_TMP(1:NNPBY,I)
          RBY(1:NRBY,CPT_RBY)=RBY_TMP(1:NRBY,I)
          CPT_RBY = CPT_RBY-1
        ELSE
C--  SECONDARY RBODY - put at the end of the list
          NOM_OPT(1:LNOPT1,CPT_RBY_SECONDARY)=NOM_OPT_TMP(1:LNOPT1,I)
          NPBY(1:NNPBY,CPT_RBY_SECONDARY)=0!NPBY_TMP(1:NNPBY,I)
          RBY(1:NRBY,CPT_RBY_SECONDARY)=0!RBY_TMP(1:NRBY,I)
          NPBY(13,CPT_RBY_SECONDARY) = CPT_RBY+1
          CPT_RBY_SECONDARY = CPT_RBY_SECONDARY-1 
        ENDIF
      ENDDO
C
      NRBYKIN=NEW_SIZE-NRBYLAG
C
      RETURN
      END   
C
