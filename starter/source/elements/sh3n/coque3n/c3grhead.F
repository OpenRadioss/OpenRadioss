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
!||    c3grhead                   ../starter/source/elements/sh3n/coque3n/c3grhead.F
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    fretitl2                   ../starter/source/starter/freform.F
!||    get_sort_key_shell3n       ../starter/source/elements/sh3n/get_sort_key_shell3n.F90
!||--- uses       -----------------------------------------------------
!||    drape_mod                  ../starter/share/modules1/drape_mod.F
!||    get_sort_key_shell3n_mod   ../starter/source/elements/sh3n/get_sort_key_shell3n.F90
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    reorder_mod                ../starter/share/modules1/reorder_mod.F
!||    stack_mod                  ../starter/share/modules1/stack_mod.F
!||====================================================================
      SUBROUTINE C3GRHEAD(
     1       IXTG    ,PM      ,GEO     ,INUM    ,
     2       ITR1    ,EADD    ,INDEX   ,ITRI    ,XNUM    ,
     3       IPARTTG ,ND      ,THK     ,IGRSURF ,IGRSH3N ,
     4       CEP     ,XEP     ,IXTG1   ,ICNOD   ,
     5       IGEO    ,IPM     ,IPART   ,SH3TREE ,NOD2ELTG,
     6       ITRIOFF ,SH3TRIM ,TAGPRT_SMS,
     7       IWORKSH , STACK  ,DRAPE   ,RNOISE,
     8       SH3ANG,DRAPEG, PTSH3N ,MAT_PARAM,
     9       DAMP_RANGE_PART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE DRAPE_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use element_mod , only : nixtg
      use get_sort_key_shell3n_mod , only : get_sort_key_shell3n
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXTG(NIXTG,NUMELTG)TABLEAU CONECS+PID+MID+NOS TRIANGLES       E
C     PM(NPROPM,NUMMAT)  ARRAY OF MATERIAL CHARACTERISTICS           E
C     GEO(NPROPG,NUMGEO) ARRAY OF PID CHARACTERISTICS                 E
C     INUM(9,NUMELTG)     TABLEAU DE TRAVAIL                         E/S
C     ISEL(NSELTG)        TABLEAU DES TRI CHOISIES POUR TH          E/S
C     ITR1(NSELTG)        TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELTG)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S 
C     INDEX(NUMELTG)      TABLEAU DE TRAVAIL                         E/S
C     ITRI(7,NUMELTG)     TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELTG)    TABLEAU PROC                               E/S
C     XEP(NUMELTG)    TABLEAU PROC                               E/S
C     NOD2ELTG(3*NUMELTG+3*NUMELTG6)                                 E/S
C     ITRIOFF(NUMELTG)  FLAG ELEM RBY ON/OFF                         E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
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
      integer, parameter :: nb_key = 8
      INTEGER
     .    IXTG(NIXTG,*),INUM(10,*),ND,ICNOD(*),IXTG1(4,*),
     .    EADD(*), ITR1(*), INDEX(*), ITRI(nb_key,*),IPARTTG(*),
     .    CEP(*), XEP(*),ITRIOFF(*),
     .    IGEO(NPROPGI,*),IPM(NPROPMI,*), IPART(LIPART1,*), 
     .    SH3TREE(KSH3TREE,*), NOD2ELTG(*), SH3TRIM(*),
     .    TAGPRT_SMS(*),IWORKSH(3,*)
      integer :: sh3tree_dim1,sh3tree_dim2     
      INTEGER , DIMENSION(NUMELTG) , INTENT(INOUT):: PTSH3N
      INTEGER , INTENT(IN) :: DAMP_RANGE_PART(NPART)     !< flag to compute the damping range
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*), XNUM(*), THK(*), RNOISE(NPERTURB,*),
     .    SH3ANG(*)
C-----------------------------------------------
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  ,TARGET     :: DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_)                             :: DRAPEG,XNUM_DRAPEG
      TYPE (DRAPE_)  , DIMENSION(:), ALLOCATABLE :: XNUM_DRAPE
      TYPE (DRAPE_PLY_)             , POINTER    :: DRAPE_PLY
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSH3N)  :: IGRSH3N
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISTOR
      INTEGER WORK(70000)
      INTEGER I, K, NN,ID,PID,J, 
     .   II1, JJ1, II2, JJ2, II, JJ, 
     .   II3, JJ3,NELTG3,
     .   MODE,
     .   II0,JJ0, II4, JJ4,
     .   IWARNHB,
     .   II5,JJ5,II6,JJ6,
     .   IPERT,STAT,NSLICE,KK,NPT_DRP,IE,
     .   IE0
C     REAL OR REAL*8
      CHARACTER(LEN=NCHARTITLE) :: TITR
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2, INUM_PTSH3N
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INUM_WORKSH

      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
      my_real, DIMENSION(:), ALLOCATABLE :: ANGLE ! NUMELTG
C-----------------------------------------------
C-----------------------------------------------
      ALLOCATE(ANGLE(NUMELTG))
      ALLOCATE(INUM_WORKSH(3,NUMELTG))
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       IWARNHB=0
        IF(NADMESH /= 0)THEN
            ALLOCATE( ISTOR(KSH3TREE+1,NUMELTG) )
        ELSE
            ALLOCATE( ISTOR(0,0) )
        ENDIF
        IF (NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN
          ALLOCATE(XNUM_DRAPE(NUMELTG))                                                  
          ALLOCATE(XNUM_DRAPEG%INDX(NUMELTG))
          XNUM_DRAPEG%INDX = 0                                         
          DO I =1, NUMELTG
            IE = DRAPEG%INDX(NUMELC + I)
            IF(IE == 0) CYCLE   
            NPT_DRP = DRAPE(IE)%NPLY_DRAPE
            NPT     = DRAPE(IE)%NPLY   
            ALLOCATE(XNUM_DRAPE(I)%INDX_PLY(NPT)) 
            ALLOCATE(XNUM_DRAPE(I)%DRAPE_PLY(NPT_DRP))        
            XNUM_DRAPE(I)%INDX_PLY= 0     
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
           ALLOCATE(INUM_PTSH3N(NUMELTG))
           INUM_PTSH3N = 0
        ELSE
           ALLOCATE(INUM_PTSH3N(0))
        ENDIF 
C
C----------------------------------------------------------
C   GLOBAL SORTING ON ALL CRITERIA FOR ALL ELEMENTS
C----------------------------------------------------------
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELTG),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
        ELSE
          ALLOCATE(XNUM_RNOISE(NPERTURB,0),STAT=stat)
      ENDIF
C
      CALL MY_ALLOC(INDEX2,NUMELTG)
      IF(NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN
         DO I=1,NUMELTG
           INDEX2(I)= PERMUTATION%TRIANGLE(I)
           EADD(I)=1
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
           INUM_WORKSH(1,I) = IWORKSH(1,NUMELC + I)
           INUM_WORKSH(2,I) = IWORKSH(2,NUMELC + I)
           INUM_WORKSH(3,I) = IWORKSH(3,NUMELC + I)
           IF (NPERTURB > 0) THEN                                               
             DO IPERT = 1, NPERTURB                                             
               XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I)                           
             ENDDO                                                              
           ENDIF                                                                
           ANGLE(I)=SH3ANG(I)                                                   
           !drape structure                                                     
           IE = DRAPEG%INDX(NUMELC + I)  
           XNUM_DRAPEG%INDX(I) = IE                                             
           IF(IE == 0) CYCLE                                                    
           NPT = DRAPE(IE)%NPLY   
           XNUM_DRAPE(I)%NPLY = NPT                                       
           XNUM_DRAPE(I)%INDX_PLY(1:NPT) = DRAPE(IE)%INDX_PLY(1:NPT)            
           NPT = DRAPE(IE)%NPLY_DRAPE                                           
           XNUM_DRAPE(I)%NPLY_DRAPE = NPT                                           
           XNUM_DRAPE(I)%THICK =  DRAPE(IE)%THICK                                  
           DO JJ = 1, NPT                                                       
             DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(JJ)                               
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
             DEALLOCATE(DRAPE(IE)%DRAPE_PLY)  
             DEALLOCATE(DRAPE(IE)%INDX_PLY)  
         ENDDO
      ELSE
         DO I=1,NUMELTG
           INDEX2(I)= PERMUTATION%TRIANGLE(I)
           EADD(I)=1
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
           INUM_WORKSH(1,I) = IWORKSH(1,NUMELC + I)
           INUM_WORKSH(2,I) = IWORKSH(2,NUMELC + I)
           INUM_WORKSH(3,I) = IWORKSH(3,NUMELC + I)
           IF (NPERTURB > 0) THEN
             DO IPERT = 1, NPERTURB
               XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I) 
             ENDDO
           ENDIF
           ANGLE(I)=SH3ANG(I)
         ENDDO
      ENDIF
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)== 4.OR.ABS(ISIGI)==5)THEN
         INUM_PTSH3N(1:NUMELTG) = PTSH3N(1:NUMELTG)
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

      if(nadmesh==0) then
        sh3tree_dim1 = 0
        sh3tree_dim2 = 0
      else
        sh3tree_dim1 = ksh3tree
        sh3tree_dim2 = numeltg
      endif      
      itri(1:nb_key,1:numeltg) = 0
      call get_sort_key_shell3n(numelc,numeltg,nb_key,nummat,numgeo,
     .                          npart,nadmesh,lipart1,npropgi,
     .                          npropmi,npropm,npropg,icrack3d,
     .                          isms,idtgrs,sh3tree_dim1,sh3tree_dim2,ltitr,trimat,iwarnhb,
     .                          iparttg,itrioff,damp_range_part,icnod,tagprt_sms,
     .                          ipart,iworksh,itri,ixtg,igeo,
     .                          ipm,sh3tree,pm,geo,
     .                          stack,mat_param)

      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELTG , 8)
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
      IF (NUMELTG6>0) THEN
        NELTG3 = NUMELTG-NUMELTG6
        DO I = 1, NUMELTG6
          II = I + NELTG3
          INUM(1,II)=IXTG1(1,I)
          INUM(2,II)=IXTG1(2,I)
          INUM(3,II)=IXTG1(3,I)
        ENDDO
        DO I = 1, NUMELTG6
          II = I + NELTG3
          IXTG1(1,I)=INUM(1,INDEX(II))
          IXTG1(2,I)=INUM(2,INDEX(II))
          IXTG1(3,I)=INUM(3,INDEX(II))
        ENDDO
      ENDIF 
C 
      IF(NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN
        IE = DRAPEG%NUMSH4
        DO I=1,NUMELTG
         IWORKSH(1,NUMELC + I)= INUM_WORKSH(1,INDEX(I))
         IWORKSH(2,NUMELC + I)= INUM_WORKSH(2,INDEX(I))
         IWORKSH(3,NUMELC + I)= INUM_WORKSH(3,INDEX(I))
         IF (NPERTURB > 0) THEN
           DO IPERT = 1, NPERTURB
             RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
           ENDDO
         ENDIF
         SH3ANG(I)=ANGLE(INDEX(I))
         !
         IE0 = XNUM_DRAPEG%INDX(INDEX(I))
         DRAPEG%INDX(NUMELC + I) = 0
         IF(IE0 == 0) CYCLE
         IE = IE + 1
         NPT = XNUM_DRAPE(INDEX(I))%NPLY ! number of layer shell
         DRAPE(IE)%NPLY = NPT
         DRAPEG%INDX(NUMELC + I)= IE
         ALLOCATE(DRAPE(IE)%INDX_PLY(NPT))   
         DRAPE(IE)%INDX_PLY(1:NPT) =  XNUM_DRAPE(INDEX(I))%INDX_PLY(1:NPT)                                  
         NPT = XNUM_DRAPE(INDEX(I))%NPLY_DRAPE
         ALLOCATE(DRAPE(IE)%DRAPE_PLY(NPT))                                      
         DRAPE(IE)%NPLY_DRAPE= NPT                
         DRAPE(IE)%THICK =  XNUM_DRAPE(INDEX(I))%THICK               
         DO JJ = 1, NPT         
           DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(JJ)                                  
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
      ELSE
        DO I=1,NUMELTG
         IWORKSH(1,NUMELC + I)= INUM_WORKSH(1,INDEX(I))
         IWORKSH(2,NUMELC + I)= INUM_WORKSH(2,INDEX(I))
         IWORKSH(3,NUMELC + I)= INUM_WORKSH(3,INDEX(I))
         IF (NPERTURB > 0) THEN
           DO IPERT = 1, NPERTURB
             RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
           ENDDO
         ENDIF
          SH3ANG(I)=ANGLE(INDEX(I))
        ENDDO 
      ENDIF
      !
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)== 4.OR.ABS(ISIGI)==5)THEN
         DO I=1,NUMELTG
          PTSH3N(I) = INUM_PTSH3N(INDEX(I))
         ENDDO  
       ENDIF
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
C INDEX INVERSION (IN ITR1)
C
      DO I=1,NUMELTG
        ITR1(INDEX(I))=I
      ENDDO
C RENAMING OF THE TREE
      IF(NADMESH/=0)THEN
        DO  I=1,NUMELTG
          IF(SH3TREE(1,I)/=0)
     .       SH3TREE(1,I)=ITR1(SH3TREE(1,I))
          IF(SH3TREE(2,I)/=0)
     .       SH3TREE(2,I)=ITR1(SH3TREE(2,I))
        ENDDO
      END IF
C
C RENAMING FOR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP(J) == 7)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENAMING FOR SHELL3N GROUPS
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
      DO I=1,3*NUMELTG+3*NUMELTG6
        IF(NOD2ELTG(I) /= 0)NOD2ELTG(I)=ITR1(NOD2ELTG(I))
      END DO
C--------------------------------------------------------------
C         DETERMINATION OF SUPER_GROUPS
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
C damp freq range        
        II6=ITRI(8,INDEX(I))
        JJ6=ITRI(8,INDEX(I-1))            
        IF (II0/=JJ0.OR.
     .      II/=JJ.OR.
     .      II1/=JJ1.OR.
     .      II2/=JJ2.OR.
     .      II3/=JJ3.OR.
     .      II4/=JJ4.OR.
     .      II5/=JJ5.OR.
     .      II6/=JJ6) THEN
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
C
      DEALLOCATE(INDEX2)
      DEALLOCATE( ISTOR )
      IF(NDRAPE > 0 .AND. NUMELTG_DRAPE > 0) THEN
        DO I =1, NUMELTG       
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
      IF(ALLOCATED(INUM_PTSH3N))DEALLOCATE(INUM_PTSH3N)


      DEALLOCATE(INUM_WORKSH, ANGLE)
      RETURN
      END
