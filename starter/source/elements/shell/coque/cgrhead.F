Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    cgrhead                  ../starter/source/elements/shell/coque/cgrhead.F
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    get_sort_key_shell       ../starter/source/elements/shell/get_sort_key_shell.F90
!||--- uses       -----------------------------------------------------
!||    drape_mod                ../starter/share/modules1/drape_mod.F
!||    get_sort_key_shell_mod   ../starter/source/elements/shell/get_sort_key_shell.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    r2r_mod                  ../starter/share/modules1/r2r_mod.F
!||    reorder_mod              ../starter/share/modules1/reorder_mod.F
!||    stack_mod                ../starter/share/modules1/stack_mod.F
!||====================================================================
      SUBROUTINE CGRHEAD(
     1       IXC     ,PM      ,GEO       ,INUM   ,
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
      use element_mod , only : nixc
      use get_sort_key_shell_mod , only : get_sort_key_shell
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
      integer, parameter :: nb_key = 8
      INTEGER IXC(NIXC,*),INUM(9,*),IPARTC(*), ISHEOFF(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(nb_key,*),
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
      TYPE (MATPARAM_STRUCT_), DIMENSION(NUMMAT),INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL)  :: IGRSH4N
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISTOR
      INTEGER WORK(70000)
      INTEGER I, K,   NN,
     .  II, J,
     .  II1,JJ1,II2,JJ2,JJ,II3,JJ3,
     .  MODE,
     .  II0,JJ0,II4,JJ4,N1,
     .  INUM_R2R(1+R2R_SIU*NUMELC),
     .  II5,JJ5,II6,JJ6,
     .  IPERT,STAT,NSLICE,KK,NPT_DRP,IE,IE0
      integer :: sh4tree_dim1,sh4tree_dim2
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INUM_WORKC
      my_real, DIMENSION(:), ALLOCATABLE :: ANGLE !(NUMELC)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2, INUM_PTSHEL
C     REAL OR REAL*8
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
C
      TYPE (DRAPE_PLY_)             , POINTER   :: DRAPE_PLY
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--

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
      ELSE
        ALLOCATE(XNUM_RNOISE(NPERTURB,0),STAT=stat)
      ENDIF
C    
      CALL MY_ALLOC(INDEX2,NUMELC)
      DO I=1,NUMELC
        INDEX2(I)=PERMUTATION%SHELL(I)
        EADD(I)=1
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
      IF(NDRAPE > 0 .AND. NUMELC_DRAPE > 0) THEN
        DO I=1,NUMELC        
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
      
      if(nadmesh==0) then
        sh4tree_dim1 = 0
        sh4tree_dim2 = 0
      else
        sh4tree_dim1 = ksh4tree
        sh4tree_dim2 = numelc
      endif
      itri(1:nb_key,1:numelc) = 0
      call get_sort_key_shell(numelc,numeltg,nb_key,nummat,numgeo, 
     .                        npart,nadmesh,lipart1,npropgi, 
     .                        npropmi,npropm,npropg,icrack3d, 
     .                        isms,idtgrs,sh4tree_dim1,sh4tree_dim2, 
     .                        ipartc,isheoff,damp_range_part,tagprt_sms, 
     .                        ipart,iworksh,itri,ixc,igeo, 
     .                        ipm,sh4tree,pm,geo, 
     .                        stack,mat_param)

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
