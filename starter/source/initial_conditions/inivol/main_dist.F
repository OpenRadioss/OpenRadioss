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
!#define SIZE_X1 1220000 
!#define SIZE_X2 5000
!#define my_real REAL(KIND=8)
!      program MAIN
!       IMPLICIT NONE
Chd|====================================================================
Chd|  MAIN_DIST                     source/initial_conditions/inivol/main_dist.F
Chd|-- called by -----------
Chd|        GETPHASE                      source/initial_conditions/inivol/getphase.F
Chd|-- calls ---------------
Chd|        FIND_CLOSEST_NODE             source/initial_conditions/inivol/main_dist.F
Chd|====================================================================
      SUBROUTINE MAIN_DIST(X1    ,ID_X2   ,TAGN  ,SIZE_X2 ,NSOLTOSF   ,
     .                     DIST  ,NBCONTY ,IDC   ,IDSURF  ,IVOLSURF,
     .                     NSURF_INVOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ID_X2(*),TAGN(*),SIZE_X2,NBCONTY,NSOLTOSF(NBCONTY,*),IDC,
     .         IDSURF,IVOLSURF(NSURF),NSURF_INVOL
       my_real X1(3,*)
!!      INTEGER, parameter  :: SIZE_X1 = 1220000
      INTEGER  :: SIZE_X1
!!      INTEGER, parameter  :: SIZE_X2 = 5000
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!!       my_real, DIMENSION(:,:), ALLOCATABLE :: X1
!!       my_real, DIMENSION(:), ALLOCATABLE   :: DIST
       my_real EPS
       my_real, INTENT(INOUT) ::  DIST(NSURF_INVOL,*)   ! for each X1, distance to the closest root node
!!       INTEGER         , DIMENSION(:), ALLOCATABLE   :: ID_LIST,ID_X2
       INTEGER         , DIMENSION(:), ALLOCATABLE   :: ID_LIST
       INTEGER I,J
       my_real r
       my_real DMIN,DX,DY,DZ,D
C-----------------------------------------------
C
       SIZE_X1 = NUMNOD
C

!!       ALLOCATE( X1(3,SIZE_X1),ID_X2(SIZE_X2) )
!!       ALLOCATE( DIST(SIZE_X1) )
       ALLOCATE( ID_LIST(SIZE_X1) )

!!       CALL RANDOM_NUMBER(X1)

!!       DO I = 1,SIZE_X2
!!        CALL RANDOM_NUMBER(r)
!!        ID_X2(I) = INT(r*(SIZE_X2-1)) + 1
!!c       WRITE(6,*) ID_X2(I)
!!       ENDDO
!
       EPS = 1D-6
!!       WRITE(6,*) "1 - SEARCH"
C      X1 contient toutes les coordonnees: c'est X(1:NUMNOD) 
C      ID_X2 contient les ID des coordonnes des noeuds 'root' (ceux dont
C      l'index va etre mis dans ID_LIST) Cela correspond aux IPL sans
C      doublons
C      On a pas besoin de construire explictement XPL, vu que les
C      coordonnes sont deja dans X 

       CALL FIND_CLOSEST_NODE(X1    ,ID_X2      ,SIZE_X1 ,SIZE_X2 ,ID_LIST   ,
     .                        DIST  ,EPS        ,TAGN    ,NBCONTY ,IDC       ,
     .                        IDSURF,IVOLSURF,NSURF_INVOL)
!!       WRITE(6,*) "2 - CHECK"
! update error message through Radioss error message printing
!       DO I = 1,SIZE_X1
!         DX = ABS (X1(1,I)-X1(1,ID_LIST(I)))
!         DY = ABS (X1(2,I)-X1(2,ID_LIST(I)))
!         DZ = ABS (X1(3,I)-X1(3,ID_LIST(I)))
!         DMIN = SQRT(DX*DX+DY*DY+DZ*DZ)
!         IF (DMIN /= DIST(I) .OR. (DIST(I) < EPS)) THEN
!           WRITE(6,*) "ERROR",DMIN,DIST(I),I,ID_LIST(I)
!         ENDIF
!         DO J = 1,SIZE_X2
!          DX =ABS (X1(1,I)-X1(1,ID_X2(J)))
!          DY =ABS (X1(2,I)-X1(2,ID_X2(J)))
!          DZ =ABS (X1(3,I)-X1(3,ID_X2(J)))
!          D = SQRT(DX*DX+DY*DY+DZ*DZ)
!          IF (D < DMIN .AND. D > EPS) THEN
!             WRITE(6,*) I,J,ID_LIST(I)
!             WRITE(6,*) X1(1:3,I)
!          ENDIF
!         ENDDO
!       ENDDO
c      DO J = 1,SIZE_X2
c        IF(ID_LIST(ID_X2(J)) /= ID_X2(J)) WRITE(6,*) "problem"
c      ENDDO
!---
      DO I = 1,SIZE_X1
        IF (TAGN(I) == 0)  NSOLTOSF(IDC,I) = ID_LIST(I)
      ENDDO
C---

!       WRITE(6,*) "3 - DONE"

!
      IF (ALLOCATED (ID_LIST)) DEALLOCATE(ID_LIST)
!
      RETURN
!!!      end program MAIN
      END SUBROUTINE MAIN_DIST
!---
Chd|====================================================================
Chd|  FIND_CLOSEST_NODE             source/initial_conditions/inivol/main_dist.F
Chd|-- called by -----------
Chd|        MAIN_DIST                     source/initial_conditions/inivol/main_dist.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|====================================================================
      SUBROUTINE FIND_CLOSEST_NODE(X1    ,ID_X2       ,N1    ,N2      ,ID_LIST,
     .                             DIST  ,EPS         ,TAGN  ,NBCONTY ,IDC    ,
     .                             IDSURF,IVOLSURF,NSURF_INVOL)
!---
C  Nearest Neighbor search
C  This routines returns ID_LIST and DIST such as
C  ID_LIST(I=1:N2) is the id of the closest node in X1(ID_X2(1:N2))
C
C This NNS algorithm is not optimal
C - worst case O(N1 x N2) 
C - best case O(N1 x LOG(N1)) + O(N1)
C - Expected in practice: O(N1 log(N1)) + O (N1 x (N2)^(1/3)) 
C      which is good enough for cases where N2 << N1
       IMPLICIT NONE
#include      "com04_c.inc"
       
       INTEGER, INTENT(IN)    ::  NBCONTY,IDC,IDSURF,IVOLSURF(NSURF),NSURF_INVOL
       my_real, INTENT(IN)    ::  X1(3,N1) ! Coordinates 
       my_real, INTENT(IN)    ::  EPS  ! Minimum distance between two disctinct points
       INTEGER, INTENT(IN)    ::  ID_X2(N2)  ! Indexes of 'root' coordinates
       INTEGER, INTENT(IN)    ::  N1,N2      ! N1: total number of nodes
                                             ! N2: number of root nodes
       my_real, INTENT(INOUT) ::  DIST(NSURF_INVOL,N1)   ! for each X1, distance to the closest root node
       INTEGER, INTENT(INOUT) ::  ID_LIST(N1)!              id of the closest root node
       INTEGER, INTENT(IN)    ::  TAGN(*)

C      LOCAL VARIABLES
       INTEGER :: DIR,I,J,ID,info,JMIN
       INTEGER :: LAST_POSITION
       INTEGER, DIMENSION(:), ALLOCATABLE :: KEY1, KEY2
       my_real, DIMENSION(:), ALLOCATABLE :: XTMP1,XTMP2
       my_real :: XMIN(3),XMAX(3)
       my_real :: L
       my_real :: DELTA,DX,DY,DZ,DXL,DYL,DZL,DL


       ! Bounding boxes of both sets
       XMIN(1:3) = MINVAL(X1,dim=2)
       XMAX(1:3) = MAXVAL(X1,dim=2)


       ! Pick a direction
       DIR = 1
       L = (XMAX(1) - XMIN(1)) 
       DO I = 2,3
          IF (L < XMAX(1) - XMIN(1) ) THEN 
            DIR = I 
            L = (XMAX(I) - XMIN(I)) 
          ENDIF
       ENDDO

       ALLOCATE (XTMP1(N1),KEY1(N1))
       ALLOCATE (XTMP2(N2),KEY2(N2))

       DO I=1,N1 
         XTMP1(I) = X1(DIR,I)
         KEY1(I) = I
       ENDDO
       DO I=1,N2 
         XTMP2(I) = X1(DIR,ID_X2(I))
         KEY2(I) = I
       ENDDO

       ! Sort according to chosen direction
       CALL MYQSORT(N1,XTMP1,KEY1,info)
       CALL MYQSORT(N2,XTMP2,KEY2,info)

       LAST_POSITION = 1 
       JMIN = 1
       DO I = 1,N1

         DX =ABS(X1(1,ID_X2(KEY2(LAST_POSITION))) - X1(1,KEY1(I)))   
         DY =ABS(X1(2,ID_X2(KEY2(LAST_POSITION))) - X1(2,KEY1(I)))   
         DZ =ABS(X1(3,ID_X2(KEY2(LAST_POSITION))) - X1(3,KEY1(I)))   
         DELTA = SQRT(DX*DX+DY*DY+DZ*DZ)

C        Avoid nodes closer than EPS
         IF (DELTA < EPS) DELTA = HUGE(DELTA)

C        Upward search
         J = LAST_POSITION + 1
         JMIN = LAST_POSITION
         ID = KEY2(LAST_POSITION)
         DXL = DX
         DO WHILE (J <= N2 .AND. (DXL <= DELTA .OR. DELTA < EPS))
           DXL =ABS(X1(1,ID_X2(KEY2(J))) - X1(1,KEY1(I)))   
           DYL =ABS(X1(2,ID_X2(KEY2(J))) - X1(2,KEY1(I)))   
           DZL =ABS(X1(3,ID_X2(KEY2(J))) - X1(3,KEY1(I)))   
           DL = SQRT(DXL*DXL+DYL*DYL+DZL*DZL)
           IF (DL  < DELTA .AND. DL  > EPS) THEN
             DELTA = DL
             ID = KEY2(J)
             JMIN = J
           ENDIF
           J = J + 1
           IF (J <= N2) DXL =ABS(X1(DIR,ID_X2(KEY2(J)))-X1(DIR,KEY1(I)))  
         ENDDO

C        Backward search
         J = LAST_POSITION
         DXL = DX
         DO WHILE (J > 0 .AND. (DXL <= DELTA .OR. DELTA < EPS))
           DXL =ABS(X1(1,ID_X2(KEY2(J))) - X1(1,KEY1(I)))   
           DYL =ABS(X1(2,ID_X2(KEY2(J))) - X1(2,KEY1(I)))   
           DZL =ABS(X1(3,ID_X2(KEY2(J))) - X1(3,KEY1(I)))   
           DL = SQRT(DXL*DXL+DYL*DYL+DZL*DZL)
           IF (DL  < DELTA .AND. DL > EPS) THEN
             DELTA = DL
             ID = KEY2(J)
             JMIN = J
           ENDIF
           J = J - 1
           IF (J > 0)  DXL =ABS(X1(DIR,ID_X2(KEY2(J)))-X1(DIR,KEY1(I)))  
         ENDDO
         LAST_POSITION = JMIN 
         ID_LIST(KEY1(I)) = ID
!!         DIST(IDC,KEY1(I)) = DELTA
         DIST(IVOLSURF(IDSURF),KEY1(I)) = DELTA
       ENDDO

       DO I = 1,N1
         ID_LIST(I) = ID_X2(ID_LIST(I))
       ENDDO

       DEALLOCATE(XTMP1,XTMP2)
      RETURN
!---
      END SUBROUTINE
