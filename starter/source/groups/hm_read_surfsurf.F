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
Chd|  HM_READ_SURFSURF              source/groups/hm_read_surfsurf.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SURFSURF(IGRSURF ,INSEG  ,FLAG  ,ICOUNT  ,ITER ,NSETS, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INSEG,FLAG,ICOUNT,ITER ,NSETS
!
      TYPE (SURF_)   , DIMENSION(NSURF+NSETS)   :: IGRSURF
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,ID,IGS,IGRS,JREC,IADV,NSEG,NSEGV,SRFTYP,
     .        SKIPFLAG,UID,IAD_TMP,BUFTMP_1,NSEG_TOT,
     .        IWORK(70000),IERROR, II
      INTEGER, DIMENSION(:,:) , ALLOCATABLE :: ITRI
      INTEGER, DIMENSION(:) , ALLOCATABLE :: INDEX,BUFTMP

      CHARACTER TITR*nchartitle,KEY*ncharkey,KEY2*ncharkey,
     .          KEY3*ncharkey
      INTEGER :: NB_IDS, NB_NEG_IDS
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDS
      LOGICAL :: IS_AVAILABLE
      INTEGER :: NN(4),NF,IMIN,NMIN,INOD(4),NPERM(4,4),ISIGN_NOD(4),IORD
      DATA NPERM/1,2,3,4,
     .           2,3,4,1,
     .           3,4,1,2,
     .           4,1,2,3/
C-----------------------------------------------
!   IGRSURF(IGS)%ID   :: SURFACE identifier
!   IGRSURF(IGS)%TITLE   :: SURF title
!   IGRSURF(IGS)%NSEG   :: Number of surfaces within  /SURF
!   IGRSURF(IGS)%NSEG_IGE   :: Number of iso-surfaces
!   IGRSURF(IGS)%TYPE   ::  OPEN / CLOSED surface flag
!                        SURF_TYPE = 0         : SEGMENTS
!                        SURF_TYPE = 100       : HYPER-ELLIPSOIDE MADYMO.
!                        SURF_TYPE = 101       : HYPER-ELLIPSOIDE RADIOSS.
!                        SURF_TYPE = 200       : INFINITE PLANE
!   IGRSURF(IGS)%ID_MADYMO :: Coupled madimo surface identifier
!                             (computed in Radioss Engine, when receiving Datas from MaDyMo).
!                             ID MaDyMo - for entity type which impose surface movement:
!                             No systeme MaDyMo for entity type which impose surface movement
!   IGRSURF(IGS)%NB_MADYMO   :: No de l'entite qui impose le mvt de la surface.
!                                --> No systeme Radioss ou MaDyMO.
!   IGRSURF(IGS)%TYPE_MADYMO   :: Entity type which impose surface movement.
!                                  = 1 : Rigid Body.
!                                  = 2 : MADYMO Hyper-ellipsoide.
!   IGRSURF(IGS)%IAD_BUFR   :: Analytical Surfaces address (reals BUFSF - temp)
!   IGRSURF(IGS)%LEVEL   :: FLAG "SUBLEVEL DONE" FOR SURFACES OF SURFACES
!                                 = 0 ! initialized surface
!                                 = 1 ! uninitialized surface
!   IGRSURF(IGS)%TH_SURF   :: FLAG for /TH/SURF
!                                 = 0 ! unsaved surface for /TH/SURF
!                                 = 1 ! saved surface for /TH/SURF
!   IGRSURF(IGS)%ISH4N3N   ::  FLAG = 1 (only SH4N and SH3N considered - for airbags)
!   IGRSURF(IGS)%NSEG_R2R_ALL   :: Multidomaines -> number of segments before split
!   IGRSURF(IGS)%NSEG_R2R_SHARE :: shared on boundary subdomain segments
!   IGRSURF(IGS)%ELTYP(J)   :: type of element attached to the segment of the surface
!                           ITYP = 0  - surf of segments
!                           ITYP = 1  - surf of solids
!                           ITYP = 2  - surf of quads
!                           ITYP = 3  - surf of SH4N
!                           ITYP = 4  - line of trusses
!                           ITYP = 5  - line of beams
!                           ITYP = 6  - line of springs
!                           ITYP = 7  - surf of SH3N
!                           ITYP = 8  - line of XELEM (nstrand element)
!                           ITYP = 101 - ISOGEOMETRIQUE
!   IGRSURF(IGS)%ELEM(J) :: element attached to the segment of the surface
!   IGRSURF(IGS)%NODES(J,4) :: 4 nodes of the segment for /SURF
C-----------------------------------------------
C SURFACES FORMEE DE SURFACES
C=======================================================================
      ALLOCATE(ITRI(5,INSEG),STAT=IERROR)
      IF(IERROR/=0)CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                         MSGTYPE=MSGERROR,
     .                         C1='SURFSURF')

      ALLOCATE(INDEX(2*INSEG),STAT=IERROR)
      IF(IERROR/=0)CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                         MSGTYPE=MSGERROR,
     .                         C1='SURFSURF')

      ALLOCATE(BUFTMP(INSEG),STAT=IERROR)
      IF(IERROR/=0)CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                         MSGTYPE=MSGERROR,
     .                         C1='SURFSURF')

      IF (FLAG == 0) ICOUNT=0
      IGS =0
C     boucle sur les surfaces
      CALL HM_OPTION_START('/SURF')
      DO I = 1, NSURF
         CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .        OPTION_ID   = ID,
     .        OPTION_TITR = TITR  ,
     .        UNIT_ID     = UID,
     .        KEYWORD2    = KEY   ,
     .        KEYWORD3    = KEY2)
             
         SKIPFLAG = 0                    
         NSEG=0                                        
         KLINE=LINE                                    
         
         IGS=IGS+1  
         IF (KEY(1:4) == 'SURF') THEN
            NB_IDS = 0
            NB_NEG_IDS = 0
            CALL HM_GET_INTV('idsmax', NB_IDS, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('negativeIdsmax', NB_NEG_IDS, IS_AVAILABLE, LSUBMODEL)
            IF (NB_IDS + NB_NEG_IDS == 0) CYCLE
            ALLOCATE(IDS(NB_IDS + NB_NEG_IDS))
            DO II = 1, NB_IDS
               CALL HM_GET_INT_ARRAY_INDEX('ids', IDS(II), II, IS_AVAILABLE, LSUBMODEL)
            ENDDO
            DO II = 1, NB_NEG_IDS
               CALL HM_GET_INT_ARRAY_INDEX('negativeIds', IDS(II + NB_IDS), II, IS_AVAILABLE, LSUBMODEL)
               IDS(II + NB_IDS) = - IDS(II + NB_IDS)
            ENDDO
C-----------
            IF (FLAG == 0 .AND. IGRSURF(IGS)%NSEG == -1) THEN
               DO II = 1, NB_IDS + NB_NEG_IDS
                  ! Get surf internal id
                  IGRS = 0
                  DO K = 1, NSURF
                     IF (IABS(IDS(II)) == IGRSURF(K)%ID) THEN
                        IGRS = K                     
                        EXIT
                     ENDIF              
                  ENDDO
                  IF (IGRS == 0)THEN                                   
                     CALL ANCMSG(MSGID=188, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .                    I1=ID, C1=TITR, I2=IDS(II))
                  ELSE IF (IGRSURF(IGRS)%TYPE==100 .OR. IGRSURF(IGRS)%TYPE==101) THEN
                     CALL ANCMSG(MSGID=187, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                    I1=ID, C1=TITR, I2=IDS(II))
                  ELSEIF (IGRSURF(IGRS)%LEVEL == 0) THEN
                     IF (ITER > NSURF) THEN
                        CALL ANCMSG(MSGID=189, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                       C1='SURFACE', C2='SURFACE', C3='SURFACE', C4=TITR, C5='SURFACE',
     .                       I1=ID, I2=IGRSURF(IGS)%ID)
                        IF(ALLOCATED(ITRI)) DEALLOCATE(ITRI)
                        IF(ALLOCATED(INDEX)) DEALLOCATE(INDEX)
                        IF(ALLOCATED(BUFTMP)) DEALLOCATE(BUFTMP)
                        RETURN
                     ENDIF
                     IGRSURF(IGS)%NSEG=-1
                     IGRSURF(IGS)%LEVEL=0
                     ICOUNT=1                                    
                     NSEG = 0                                    
                     SKIPFLAG = 1
                     EXIT
                  ELSE                         
                     NSEGV=IGRSURF(IGRS)%NSEG
                     NSEG =NSEG+NSEGV                    
                  ENDIF   
               ENDDO     
C-----
               IF (SKIPFLAG == 0) THEN             
                  INSEG=INSEG+NISX*NSEG    
                  IGRSURF(IGS)%NSEG=NSEG
                  MY_ALLOCATE2D(IGRSURF(IGS)%NODES,NSEG,4)
                  IGRSURF(IGS)%NODES(1:NSEG,1:4) = 0
                  MY_ALLOCATE(IGRSURF(IGS)%ELTYP,NSEG)
                  IGRSURF(IGS)%ELTYP(1:NSEG) = 0
                  MY_ALLOCATE(IGRSURF(IGS)%ELEM,NSEG)
                  IGRSURF(IGS)%ELEM(1:NSEG) = 0
               ENDIF                                 
C-----------
            ELSEIF (FLAG == 1 .AND. IGRSURF(IGS)%LEVEL == 0 .AND.
     .              IGRSURF(IGS)%NSEG > -1) THEN
               NSEG_TOT = 0
               DO II = 1, NB_IDS + NB_NEG_IDS
!     Get surf internal id
                  IGRS = 0
                  DO K = 1, NSURF
                     IF (IABS(IDS(II)) == IGRSURF(K)%ID) THEN
                        IGRS = K                     
                        EXIT
                     ENDIF              
                  ENDDO
                  IF (IGRS == 0) CYCLE
                  IF (IGRSURF(IGRS)%NSEG == -1) THEN 
                     EXIT
                  ELSE
                     NSEGV=IGRSURF(IGRS)%NSEG
                     IF(IDS(II) > 0)THEN                                
                        DO L=1,NSEGV
                           NSEG_TOT = NSEG_TOT + 1 
                           IGRSURF(IGS)%NODES(NSEG_TOT,1) = IGRSURF(IGRS)%NODES(L,1)
                           IGRSURF(IGS)%NODES(NSEG_TOT,2) = IGRSURF(IGRS)%NODES(L,2)
                           IGRSURF(IGS)%NODES(NSEG_TOT,3) = IGRSURF(IGRS)%NODES(L,3)
                           IGRSURF(IGS)%NODES(NSEG_TOT,4) = IGRSURF(IGRS)%NODES(L,4)
                           IGRSURF(IGS)%ELTYP(NSEG_TOT)   = IGRSURF(IGRS)%ELTYP(L)
                           IGRSURF(IGS)%ELEM(NSEG_TOT)	 = IGRSURF(IGRS)%ELEM(L)
                        ENDDO
                     ELSE   
                        IF(N2D==0)THEN
                          DO L=1,NSEGV                               
                             NSEG_TOT = NSEG_TOT + 1
                             IGRSURF(IGS)%NODES(NSEG_TOT,1) = IGRSURF(IGRS)%NODES(L,4)
                             IGRSURF(IGS)%NODES(NSEG_TOT,2) = IGRSURF(IGRS)%NODES(L,3)
                             IGRSURF(IGS)%NODES(NSEG_TOT,3) = IGRSURF(IGRS)%NODES(L,2)
                             IGRSURF(IGS)%NODES(NSEG_TOT,4) = IGRSURF(IGRS)%NODES(L,1)
                             IGRSURF(IGS)%ELTYP(NSEG_TOT)   = IGRSURF(IGRS)%ELTYP(L)
                             IGRSURF(IGS)%ELEM(NSEG_TOT)   = IGRSURF(IGRS)%ELEM(L)
                          ENDDO
                        ELSE
                           DO L=1,NSEGV                               
                              NSEG_TOT = NSEG_TOT + 1
                              IGRSURF(IGS)%NODES(NSEG_TOT,1) = IGRSURF(IGRS)%NODES(L,2)
                              IGRSURF(IGS)%NODES(NSEG_TOT,2) = IGRSURF(IGRS)%NODES(L,1)
                              IGRSURF(IGS)%NODES(NSEG_TOT,3) = IGRSURF(IGRS)%NODES(L,3)
                              IGRSURF(IGS)%NODES(NSEG_TOT,4) = IGRSURF(IGRS)%NODES(L,4)
                              IGRSURF(IGS)%ELTYP(NSEG_TOT)   = IGRSURF(IGRS)%ELTYP(L)
                              IGRSURF(IGS)%ELEM(NSEG_TOT)   = IGRSURF(IGRS)%ELEM(L)
                           ENDDO
                        ENDIF
                     ENDIF    
                  ENDIF        
               ENDDO
               IGRSURF(IGS)%LEVEL=1
            ENDIF    
            DEALLOCATE(IDS)
         ELSEIF (KEY(1:5) == 'DSURF') THEN                  
            NB_IDS = 0
            NB_NEG_IDS = 0
            CALL HM_GET_INTV('idsmax', NB_IDS, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('negativeIdsmax', NB_NEG_IDS, IS_AVAILABLE, LSUBMODEL)
            IF (NB_IDS + NB_NEG_IDS == 0) CYCLE
            ALLOCATE(IDS(NB_IDS + NB_NEG_IDS))
            DO II = 1, NB_IDS
               CALL HM_GET_INT_ARRAY_INDEX('ids', IDS(II), II, IS_AVAILABLE, LSUBMODEL)
            ENDDO
            DO II = 1, NB_NEG_IDS
               CALL HM_GET_INT_ARRAY_INDEX('negativeIds', IDS(II + NB_IDS), II, IS_AVAILABLE, LSUBMODEL)
               IDS(II + NB_IDS) = - IDS(II + NB_IDS)
            ENDDO

            IF (FLAG == 0 .AND. IGRSURF(IGS)%NSEG == -1) THEN
               DO II = 1, NB_IDS + NB_NEG_IDS
!     Get surf internal id
                  IGRS = 0
                  DO K = 1, NSURF
                     IF (IABS(IDS(II)) == IGRSURF(K)%ID) THEN
                        IGRS = K                     
                        EXIT
                     ENDIF              
                  ENDDO
                  IF (IGRS == 0)THEN                                   
                     CALL ANCMSG(MSGID=188, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .                    I1=ID, C1=TITR, I2=IDS(II))
                  ELSE IF (IGRSURF(IGRS)%TYPE==100 .OR. IGRSURF(IGRS)%TYPE==101) THEN
                     CALL ANCMSG(MSGID=187, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                    I1=ID, C1=TITR, I2=IDS(II))
                  ELSEIF (IGRSURF(IGRS)%LEVEL == 0) THEN
                     IF (ITER > NSURF) THEN
                        CALL ANCMSG(MSGID=189, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                       C1='SURFACE', C2='SURFACE', C3='SURFACE', C4=TITR, C5='SURFACE',
     .                       I1=ID, I2=IGRSURF(IGS)%ID)
                        IF(ALLOCATED(ITRI)) DEALLOCATE(ITRI)
                        IF(ALLOCATED(INDEX)) DEALLOCATE(INDEX)
                        IF(ALLOCATED(BUFTMP)) DEALLOCATE(BUFTMP)
                        RETURN
                     ENDIF
                     IGRSURF(IGS)%NSEG=-1
                     IGRSURF(IGS)%LEVEL=0
                     ICOUNT=1                                    
                     NSEG = 0                                    
                     SKIPFLAG = 1
                     EXIT
                  ELSE                         
                     NSEGV=IGRSURF(IGRS)%NSEG
                     NSEG =NSEG+NSEGV                    
                  ENDIF   
               ENDDO 
               
               IF (SKIPFLAG == 0) THEN               
                  INSEG=INSEG+NISX*NSEG    
                  IGRSURF(IGS)%NSEG=NSEG
                  MY_ALLOCATE2D(IGRSURF(IGS)%NODES,NSEG,4)
                  IGRSURF(IGS)%NODES(1:NSEG,1:4) = 0
                  MY_ALLOCATE(IGRSURF(IGS)%ELTYP,NSEG)
                  IGRSURF(IGS)%ELTYP(1:NSEG) = 0
                  MY_ALLOCATE(IGRSURF(IGS)%ELEM,NSEG)
                  IGRSURF(IGS)%ELEM(1:NSEG) = 0
               ENDIF  
C-----------
            ELSEIF (FLAG == 1 .AND. IGRSURF(IGS)%LEVEL == 0 .AND. 
     .                              IGRSURF(IGS)%NSEG > -1) THEN
              NSEG = 0
              NSEG_TOT = 0
              IAD_TMP = 1
              DO II = 1, NB_IDS + NB_NEG_IDS
!     Get surf internal id
                 IGRS = 0
                 DO K = 1, NSURF
                    IF (IABS(IDS(II)) == IGRSURF(K)%ID) THEN
                       IGRS = K                     
                       EXIT
                    ENDIF              
                 ENDDO
                 IF (IGRS == 0) CYCLE
                 IF (IGRSURF(IGRS)%NSEG == -1) THEN
                    EXIT
                 ELSE                                             
                    NSEGV=IGRSURF(IGRS)%NSEG
                    IF (IDS(II) > 0)THEN                                        
                       DO L=1,NSEGV
                          BUFTMP(IAD_TMP)=IGRSURF(IGRS)%NODES(L,1)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)=IGRSURF(IGRS)%NODES(L,2)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)=IGRSURF(IGRS)%NODES(L,3)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)=IGRSURF(IGRS)%NODES(L,4)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)=IGRSURF(IGRS)%ELTYP(L)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)=IGRSURF(IGRS)%ELEM(L)
                          IAD_TMP=IAD_TMP+1
                       ENDDO
                    ELSE                                      
                       DO L=1,NSEGV
                          BUFTMP(IAD_TMP)= -IGRSURF(IGRS)%NODES(L,1)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)= -IGRSURF(IGRS)%NODES(L,2)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)= -IGRSURF(IGRS)%NODES(L,3)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)= -IGRSURF(IGRS)%NODES(L,4)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)= -IGRSURF(IGRS)%ELTYP(L)
                          IAD_TMP=IAD_TMP+1
                          BUFTMP(IAD_TMP)= -IGRSURF(IGRS)%ELEM(L)
                          IAD_TMP=IAD_TMP+1
                       ENDDO
                    ENDIF 
                    NSEG=NSEG+NSEGV
                 ENDIF
              ENDDO
! --------------
! pretreatment of surface node permutation
! --------------
              DO  L=1,NSEG
               IF (BUFTMP((L-1)*NISX+1) /= 0) THEN
                 ! nodes of surface segment
                 INOD(1) = IABS(BUFTMP((L-1)*NISX+1))
                 INOD(2) = IABS(BUFTMP((L-1)*NISX+2))
                 INOD(3) = IABS(BUFTMP((L-1)*NISX+3))
                 INOD(4) = IABS(BUFTMP((L-1)*NISX+4))
                 ! sign of nodes
                 ISIGN_NOD(1) = ISIGN(1,BUFTMP((L-1)*NISX+1))
                 ISIGN_NOD(2) = ISIGN(1,BUFTMP((L-1)*NISX+2))
                 ISIGN_NOD(3) = ISIGN(1,BUFTMP((L-1)*NISX+3))
                 ISIGN_NOD(4) = ISIGN(1,BUFTMP((L-1)*NISX+4))
                 ! check valid nodes
                 NF=0
                 DO J=1,4
                   K=INOD(J)
                   IF (K /= 0) THEN
                     NF=NF+1
                     INOD(NF)=K
                   ENDIF
                 ENDDO
                 ! check for min node ID
                 IMIN = 1
                 NMIN = INOD(IMIN)
                 DO J=2,NF
                   IF (NMIN > INOD(J)) IMIN = J
                   NMIN = MIN(NMIN,INOD(J))
                 ENDDO
                 ! start node pemutation
                 NN(1) = INOD(NPERM(IMIN,1))
                 NN(2) = INOD(NPERM(IMIN,2))
                 NN(3) = INOD(NPERM(IMIN,3))
                 NN(4) = INOD(NPERM(IMIN,4))
                 ! permuted nodes temporary storage for further treatments (sorting, double removing)
                 BUFTMP((L-1)*NISX+1) = NN(1)*ISIGN_NOD(1)
                 BUFTMP((L-1)*NISX+2) = NN(2)*ISIGN_NOD(2)
                 BUFTMP((L-1)*NISX+3) = NN(3)*ISIGN_NOD(3)
                 BUFTMP((L-1)*NISX+4) = NN(4)*ISIGN_NOD(4)
               ENDIF ! IF (BUFTMP((L-1)*NISX+1) /= 0)
             ENDDO
C-------
             !---------
             !  3 node element surface rearrangement ( N4 = N3 ) after permutation
             !---------
             DO L=1,NSEG
               INOD(1) = BUFTMP((L-1)*NISX+1)
               INOD(2) = BUFTMP((L-1)*NISX+2)
               INOD(3) = BUFTMP((L-1)*NISX+3)
               INOD(4) = BUFTMP((L-1)*NISX+4)
!
               IORD = 0
!
               IF ( INOD(1) /= 0 .OR. INOD(2) /= 0 .OR.
     .              INOD(3) /= 0 .OR. INOD(4) /= 0 ) THEN
!
                 IF (INOD(4) == 0) INOD(4)=INOD(3)
!
                 IF (INOD(1) == INOD(4)) THEN
                   INOD(4)=INOD(3)
                   IORD = IORD + 1
                 ELSEIF (INOD(2) == INOD(3)) THEN
                   INOD(3)=INOD(4)
                   IORD = IORD + 1
                 ELSEIF(INOD(1) == INOD(2)) THEN
                   INOD(2)=INOD(3)
                   INOD(3)=INOD(4)
                   IORD = IORD + 1
                 ENDIF
               ENDIF
!
               IF (IORD > 0) THEN
                 BUFTMP((L-1)*NISX+1) = INOD(1)
                 BUFTMP((L-1)*NISX+2) = INOD(2)
                 BUFTMP((L-1)*NISX+3) = INOD(3)
                 BUFTMP((L-1)*NISX+4) = INOD(4)
               ENDIF ! IF (IORD > 0)
             ENDDO ! DO L=1,NSEG
C-------
C------------------------------ 
C  sorting  
C------------------------------ 
              DO  L=1,NSEG
                INDEX(L)=L
                IF(BUFTMP((L-1)*NISX+1) /= 0) THEN
                  ITRI(1,L) = IABS(BUFTMP((L-1)*NISX+1))
                  ITRI(2,L) = IABS(BUFTMP((L-1)*NISX+2))
                  ITRI(3,L) = IABS(BUFTMP((L-1)*NISX+3))
                  ITRI(4,L) = IABS(BUFTMP((L-1)*NISX+4))
                  ITRI(5,L) = BUFTMP((L-1)*NISX+1) / IABS(BUFTMP((L-1)*NISX+1))
                ENDIF
              ENDDO
              CALL MY_ORDERS(0,IWORK,ITRI,INDEX,NSEG,5)  
C------------------------------  
C Segment deletion
C------------------------------  
              L = 1
              DO WHILE( L < NSEG)
                IF( IABS(BUFTMP( (INDEX(L)-1) * NISX + 1)) == IABS(BUFTMP( (INDEX(L+1)-1) * NISX + 1)) .AND.
     .              IABS(BUFTMP( (INDEX(L)-1) * NISX + 2)) == IABS(BUFTMP( (INDEX(L+1)-1) * NISX + 2)).AND.
     .              IABS(BUFTMP( (INDEX(L)-1) * NISX + 3)) == IABS(BUFTMP( (INDEX(L+1)-1) * NISX + 3)).AND.
     .              IABS(BUFTMP( (INDEX(L)-1) * NISX + 4)) == IABS(BUFTMP( (INDEX(L+1)-1) * NISX + 4)) ) THEN
                  IF( ITRI(5,INDEX(L)) +  ITRI(5,INDEX(L+1)) == 0)THEN
                    DO J=1,NISX
                      BUFTMP((INDEX(L)-1) *NISX+J) = 0
                      BUFTMP((INDEX(L+1)-1)*NISX+J) = -IABS(BUFTMP((INDEX(L+1)-1)*NISX+J))
                    ENDDO 
                  ELSEIF( ITRI(5,INDEX(L)) +  ITRI(5,INDEX(L+1)) /= 0)THEN
                    DO J=1,NISX
                      BUFTMP((INDEX(L)-1) *NISX+J) = 0
                      BUFTMP((INDEX(L+1)-1)*NISX+J) = BUFTMP((INDEX(L+1)-1)*NISX+J)
                    ENDDO 
                  ENDIF
                END IF
                L = L + 1
              ENDDO 
C------ for re-allocation %NODES 
              NSEGV = 0             
              DO L=1,NSEG
                IF((BUFTMP( (INDEX(L)-1) *NISX+1) > 0) .OR. 
     .             (BUFTMP( (INDEX(L)-1) *NISX+2) > 0) .OR.
     .             (BUFTMP( (INDEX(L)-1) *NISX+3) > 0) .OR.
     .             (BUFTMP( (INDEX(L)-1) *NISX+4) > 0)  )THEN 
                  NSEGV=NSEGV+1 
                ENDIF 
              ENDDO             
              IF (NSEGV /= NSEG) THEN               
                DEALLOCATE(IGRSURF(IGS)%NODES)
                MY_ALLOCATE2D(IGRSURF(IGS)%NODES,NSEGV,4)
                IGRSURF(IGS)%NODES(1:NSEGV,1:4) = 0
              ENDIF  
              DO L=1,NSEG
                IF((BUFTMP( (INDEX(L)-1) *NISX+1) > 0) .OR. 
     .             (BUFTMP( (INDEX(L)-1) *NISX+2) > 0) .OR.
     .             (BUFTMP( (INDEX(L)-1) *NISX+3) > 0) .OR.
     .             (BUFTMP( (INDEX(L)-1) *NISX+4) > 0)  )THEN 
                  NSEG_TOT=NSEG_TOT+1 
                  IGRSURF(IGS)%NODES(NSEG_TOT,1) = BUFTMP((INDEX(L)-1) *NISX+1)
                  IGRSURF(IGS)%NODES(NSEG_TOT,2) = BUFTMP((INDEX(L)-1) *NISX+2)
                  IGRSURF(IGS)%NODES(NSEG_TOT,3) = BUFTMP((INDEX(L)-1) *NISX+3)
                  IGRSURF(IGS)%NODES(NSEG_TOT,4) = BUFTMP((INDEX(L)-1) *NISX+4)
                  IGRSURF(IGS)%ELTYP(NSEG_TOT)   = BUFTMP((INDEX(L)-1) *NISX+5)
                  IGRSURF(IGS)%ELEM(NSEG_TOT)	 = BUFTMP((INDEX(L)-1) *NISX+6)
                ENDIF 
              ENDDO             
              IGRSURF(IGS)%NSEG=NSEG_TOT
              IGRSURF(IGS)%LEVEL=1
C-----------                                        
            ENDIF
           DEALLOCATE(IDS)
         ENDIF                   ! IF (FLAG == 0 .AND. ISURF(2,IGS) == -1)
      ENDDO                     ! DO I=1,NSURF
C-----------
      IF(ALLOCATED(ITRI)) DEALLOCATE(ITRI)
      IF(ALLOCATED(INDEX)) DEALLOCATE(INDEX)
      IF(ALLOCATED(BUFTMP)) DEALLOCATE(BUFTMP)

      RETURN
 900  CONTINUE
      CALL ANCMSG(MSGID=189,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1='SURFACE',
     .            C2='SURFACE',
     .            I1=ID,
     .            C3='SURFACE',
     .            C4=TITR,
     .            C5='SURFACE',
     .            I2=IGRSURF(IGS)%ID)
      IF(ALLOCATED(ITRI)) DEALLOCATE(ITRI)
      IF(ALLOCATED(INDEX)) DEALLOCATE(INDEX)
      IF(ALLOCATED(BUFTMP)) DEALLOCATE(BUFTMP)

      RETURN
C-----------
      IF(ALLOCATED(ITRI)) DEALLOCATE(ITRI)
      IF(ALLOCATED(INDEX)) DEALLOCATE(INDEX)
      IF(ALLOCATED(BUFTMP)) DEALLOCATE(BUFTMP)
      RETURN
      END
