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
Chd|  STACKGROUP                    source/stack/stackgroup.F     
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE STACKGROUP( 
     .                     IGRSH3N    ,IGRSH4N  ,IXC        ,IXTG ,
     .                     IGEO       ,GEO      ,IWORKSH    ,THK  ,
     .                     STACK      ,IPM      ,IGEO_STACK ,GEO_STACK ,
     .                     STACK_INFO ,NUMGEO_STACK,NPROP_STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE STACK_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),
     .        IXTG(NIXTG,*),IGEO(NPROPGI,*),IWORKSH(3,*),IPM(NPROPMI,*),
     .        IGEO_STACK(NPROPGI,*),NUMGEO_STACK(*),
     .        NPROP_STACK
      my_real
     .       GEO(NPROPG,*),THK(*),GEO_STACK(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,NSTACK,NPLY,IGTYP,ID,JD,IDPLY,NEL,
     .        IAD,ITY,IDSHEL,PID,IS,IDS,NSH,MODE,NS,JJ,NGEO_STACK,
     .        IGRTYP,N1,IPMAT,IPANG,IPTHK,IIGEO,NSS,IPPOS,NPT,IIS,NP,
     .        JJPID,JSTACK,JPID,ITG,IPMAT_IPLY,ISH3N,J4N,J3N,IPOS,
     .        MAT_LY,NLAY,NPTT,IPIDL,IT,ILAY,IPTHK_NPTT,IPPOS_NPTT,
     .        IINT,IPID_LY,IPDIR ,NS_STACK0 ,NPT_STACK0,IS0,JS,PIDS,IP,
     .        II1,II2,JJ1,JJ2
      INTEGER  IPIDPLY(NUMGEO+NUMPLY),IDGR4N(NUMGEO+NUMPLY),WORK(70000),
     .         INDX_SH(NUMELC+NUMELTG),PID_SH(NUMELC+NUMELTG),
     .         NFIRST(NUMELC+NUMELTG),
     .         NLAST(NUMELC+NUMELTG),IDGR3N(NUMGEO+NUMPLY),
!!     .         IADI(NUMGEO),IADR(NUMGEO),
     .         ISUBSTACK(NUMGEO+NUMSTACK),IPTPLY(1000),NBFI,IPPID,ITAG(1000),
     .         NGL,IPID_1,NUMS,IPWEIGHT,IPTHKLY,NSHQ4,NSHT3,INDEX_SH4(NUMELC),
     .         INDEX_T3(NUMELTG)
      my_real 
     .         GEO0(1000,NUMGEO)
      my_real
     .         THICKT,ZSHIFT,TMIN,TMAX,DT,THK_LY,POS_LY,THK_IT(100),
     .         POS_IT(100),POS_NPTT,THK_NPTT,POS_0,THINNING,POS
     
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI
      INTEGER, DIMENSION (:) ,ALLOCATABLE ::ICSH,INDX,
     .                                     ICSH_STACK,IDSTACK
      TYPE (STACK_PLY) :: STACK, IWORKS
      TYPE(STACK_INFO_ ) , DIMENSION (1:NPROP_STACK) :: STACK_INFO
      CHARACTER*nchartitle,
     .   TITR,TITR1
C-----------------------------------------------
      my_real
     .  A_GAUSS(9,9),W_GAUSS(9,9)
C-----------------------------------------------
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
C----------------------------f-------------------    
c=======================================================================      
c define temporary work structure
c=======================================================================      
c
      TYPE TMP_WORK
        integer, DIMENSION(:)  , POINTER ::  IPT
      END TYPE TMP_WORK
      TYPE(TMP_WORK)     , DIMENSION(:), POINTER :: IWORK_T
C=======================================================================
C    For Shell
C-----------------------------------------------   
       NS_STACK = 0
       NPT_STACK = 0
C
       IF(IPART_STACK > 0) THEN
         NPLY = 0
         NSTACK = 0
C
         DO I = 1, NUMGEO
!!           ISUBSTACK(I)= 0
           IGTYP=IGEO(11,I)
           NSTACK = IGEO(42,I)  ! number of stack where ply is attached
           IF (IGTYP == 19 .AND. NSTACK > 0) THEN
               NPLY = NPLY+1
               IPIDPLY(NPLY) = I
               IDGR4N(NPLY)  = IGEO(40,I) ! groupe shell 4N id
               IDGR3N(NPLY)  = IGEO(41,I) ! groupe shell 3N id
!!               IF(IDGRS(NPLY) > 0) IMIXTE = 1
!!            ELSEIF(IGTYP == 17) THEN
!!               NS0 = NS0 + 1
!!               PIDSTACK(NS0) =  I  
           ENDIF
         ENDDO
C  transformation d'id groupe     
          DO 10 I=1,NPLY
C shell 4N id group          
            ID = IDGR4N(I)
            IF(ID > 0) THEN   
                DO J=1,NGRSHEL
                   JD = IGRSH4N(J)%ID
                   IF(JD == ID)THEN
                     IDGR4N(I) = J
                     GOTO 20
                   ENDIF  
                ENDDO
             ENDIF ! ID > 0
C !GR  T3       
 20          CONTINUE
             ID = IDGR3N(I) 
             IF(ID > 0) THEN
                DO J=1,NGRSH3N
                  JD = IGRSH3N(J)%ID
                  IF(JD == ID)THEN
                     IDGR3N(I) = J
                     GOTO 10
                  ENDIF  
                ENDDO
             ENDIF  ! ID > 0 
10      CONTINUE       
C  
C stockage de Geo
C 
!old         DO I=1,NUMGEO
!old           IGTYP=IGEO(11,I)
!old           IF(IGTYP == 17 .OR. IGTYP == 51) THEN
!old              IPPID  = 100
!old              IPMAT  = 300
!old              IPMAT_IPLY = 500
!old              IPANG  = 200
!old              IPTHK  = 400
!old              IPPOS  = 600
!old              IPDIR  = 800
!old              N1 = INT(GEO(6,I))
!old              IADI(I) = IPPID
!old              IADR(I) = IPANG
!old              DO J=1,N1
!old                  GEO0(J,I)        = GEO(IPANG + J,I)
!old                  GEO0(N1 + J,I)   = GEO(IPTHK + J,I) 
!old                  GEO0(2*N1 + J,I) = GEO(IPPOS + J,I)
!old                  GEO0(3*N1 + J,I) = GEO(IPDIR + J,I)  ! angle (DIR1,DIR2) - for compatibility of l58 with PID51)
!old                  IGEO0(J,I)       = IGEO(IPPID+J,I)
!old                  IGEO0(N1+J,I)    = IGEO(IPMAT+J,I)
!old                  IGEO0(2*N1+J,I)  = IGEO(IPMAT_IPLY+J,I)
!old              ENDDO
!old           ENDIF 
!old         ENDDO
C tag o  f ply element 
         NSHQ4 = 0
         DO I=1,NUMELC
             PID = IXC(6,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 17 .OR. IGTYP == 51)THEN
                 NSHQ4 = NSHQ4 +  1
                 INDEX_SH4(NSHQ4) = I
            ENDIF  
         ENDDO 
C 
         NSHT3 = 0
         DO I=1,NUMELTG
             PID = IXTG(5,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 17 .OR. IGTYP == 51)THEN
                 NSHT3 = NSHT3 +  1
                 INDEX_T3(NSHT3) = I
            ENDIF  
         ENDDO 
C  number of ply belong to the element         
            DO I=1,NPLY 
              J   = IDGR4N(I)
              J4N = J
              IDPLY = IPIDPLY(I) 
              NSTACK = IGEO(42, IDPLY)
              IF(J > 0 .AND. NSTACK > 0 ) THEN
                NEL = IGRSH4N(J)%NENTITY
C eleme  nt type Q4 or T3 
                ITY = IGRSH4N(J)%GRTYPE
                   DO  100 II = 1,NEL
                     IDSHEL = IGRSH4N(J)%ENTITY(II)
                     PID = IXC(6,IDSHEL)
                     IGTYP = IGEO(11,PID)
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS, IDPLY)
                             IF (IDS == PID) THEN
                                IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                                GOTO 100
                             ENDIF 
                          ENDDO
                      ENDIF 
 100               CONTINUE         
                ENDIF 
                 J   = IDGR3N(I)
                 J3N = J
                 IF(J > 0 .AND. NSTACK > 0 ) THEN
                  NEL = IGRSH3N(J)%NENTITY
C eleme  nt type T3 
                  ITY = IGRSH3N(J)%GRTYPE
                   DO 200 II = 1,NEL
! c a v  erifier l'id du triangle

                     ISH3N = IGRSH3N(J)%ENTITY(II)
                     PID = IXTG(5,ISH3N)
                     IGTYP = IGEO(11,PID)
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS,IDPLY)
                             IF (IDS == PID) THEN
                                IDSHEL = ISH3N + NUMELC
                                IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                                GOTO 200
                             ENDIF 
                          ENDDO
                      ENDIF   
 200                CONTINUE
               ENDIF 
               IF(J4N == 0 .AND. J3N == 0 .AND. NSTACK > 0 ) THEN
C 
                    DO 300 II = 1,NUMELC
                     PID = IXC(6,II)
                     IGTYP = IGEO(11,PID)
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS,IDPLY)
                             IF (IDS == PID) THEN
                                IWORKSH(1,II) = IWORKSH(1,II) + 1
                                GOTO 300
                             ENDIF 
                          ENDDO
                      ENDIF   
 300                CONTINUE        
                    DO 400 II = 1,NUMELTG
                       PID = IXTG(5,II)
                       IGTYP = IGEO(11,PID)
                       ITG = NUMELC + II
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS,IDPLY)
                             IF (IDS == PID) THEN
                                IWORKSH(1,ITG) = IWORKSH(1,ITG) + 1
                                GOTO 400
                             ENDIF 
                          ENDDO
                      ENDIF   
 400                CONTINUE 
              ENDIF
C
         ENDDO  ! iply  
C #####################################################"
           ALLOCATE(IWORK_T(NUMELC+NUMELTG),ICSH(NUMELC+NUMELTG))
           IF(NUMELC + NUMELTG > 0)ICSH = 0
           DO I=1,NUMELC + NUMELTG
             NULLIFY(IWORK_T(I)%IPT)
             NPT = IWORKSH(1,I)
             ALLOCATE(IWORK_T(I)%IPT(NPT))
             IWORK_T(I)%IPT = 0
             IWORKSH(1,I) = 0
           ENDDO  
!!
!    ply to element
!!
            DO I=1,NPLY 
              J   = IDGR4N(I)
              J4N = J
              IDPLY = IPIDPLY(I) 
              NSTACK = IGEO(42, IDPLY)
              IF(J > 0 .AND. NSTACK > 0 ) THEN
                NEL = IGRSH4N(J)%NENTITY
C eleme  nt type Q4 or T3 
                ITY = IGRSH4N(J)%GRTYPE
                   DO  101 II = 1,NEL
                     IDSHEL = IGRSH4N(J)%ENTITY(II)
                     PID = IXC(6,IDSHEL)
                     IGTYP = IGEO(11,PID)
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS, IDPLY)
                             IF (IDS == PID) THEN
                                 IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                                 NPT = IWORKSH (1,IDSHEL)
                                 IWORK_T(IDSHEL)%IPT(NPT) = IDPLY
                                 ICSH(IDSHEL) = ICSH(IDSHEL) + IDPLY*IDPLY
                                GOTO 101
                             ENDIF 
                          ENDDO
                      ENDIF 
 101               CONTINUE        
                ENDIF 
                 J   = IDGR3N(I)
                 J3N = J
                 IF(J > 0 .AND. NSTACK > 0 ) THEN
                  NEL = IGRSH3N(J)%NENTITY
C eleme  nt type T3 
                  ITY = IGRSH3N(J)%GRTYPE
                   DO 202 II = 1,NEL
                     ISH3N = IGRSH3N(J)%ENTITY(II)
                     PID = IXTG(5,ISH3N)
                     IGTYP = IGEO(11,PID)
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS,IDPLY)
                             IF (IDS == PID) THEN
                                IDSHEL = ISH3N + NUMELC
                                IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                                NPT = IWORKSH(1,IDSHEL)
                                 IWORK_T(IDSHEL)%IPT(NPT) = IDPLY
                                 ICSH(IDSHEL) = ICSH(IDSHEL) + IDPLY*IDPLY
                                GOTO 202
                             ENDIF 
                          ENDDO
                      ENDIF   
 202                CONTINUE
               ENDIF 
               IF(J4N == 0 .AND. J3N == 0 .AND. NSTACK > 0 ) THEN
C 
                    DO 333 II = 1,NUMELC
                     PID = IXC(6,II)
                     IGTYP = IGEO(11,PID)
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS,IDPLY)
                             IF (IDS == PID) THEN
                                IWORKSH(1,II) = IWORKSH(1,II) + 1
                                NPT = IWORKSH(1,II)
                                IWORK_T(II)%IPT(NPT) = IDPLY
                                ICSH(II) = ICSH(II) + IDPLY*IDPLY
                                GOTO 333
                             ENDIF 
                          ENDDO
                      ENDIF   
 333                CONTINUE        
                    DO 404 II = 1,NUMELTG
                       PID = IXTG(5,II)
                       IGTYP = IGEO(11,PID)
                       ITG = NUMELC + II
                     IF(IGTYP == 17 .OR. IGTYP == 51) THEN
                         DO IS = 1,NSTACK
                             IDS = IGEO(200 + IS,IDPLY)
                             IF (IDS == PID) THEN
                                 IWORKSH(1,ITG) = IWORKSH(1,ITG) + 1
                                 NPT = IWORKSH(1,ITG)
                                 IWORK_T(ITG)%IPT(NPT) = IDPLY
                                 ICSH(ITG) = ICSH(ITG) + IDPLY*IDPLY
                                GOTO 404
                             ENDIF 
                          ENDDO
                      ENDIF   
 404                CONTINUE 
              ENDIF
C
         ENDDO  ! iply                       
!!###########################################################"
C 
C   mise en groupe des stack
C           
         NSH = 0
         INDX_SH = 0
         PID_SH = 0
C                
         DO I=1,NUMELC
             PID = IXC(6,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 17 .OR. IGTYP == 51)THEN
                 NSH = NSH +  1
                 INDX_SH(NSH) = I
                 PID_SH(NSH) = PID
            ENDIF  
         ENDDO 
C
         DO I=1,NUMELTG
             PID = IXTG(5,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 17 .OR. IGTYP == 51)THEN
                 NSH = NSH +  1
                 INDX_SH(NSH) = I+NUMELC
                 PID_SH(NSH) = PID
            ENDIF  
         ENDDO   
C        
C
C   
         ALLOCATE(INDX(2*NSH),ITRI(3,NSH))
         INDX = 0  
         ITRI = 0
         DO I = 1,NSH
           INDX(I) = I
           II = INDX_SH(I)
           ITRI(1,I) = PID_SH(I)
           ITRI(2,I) = ICSH(II)
           ITRI(3,I) = IWORKSH(1,II)
         ENDDO
C        
         MODE = 0
C            
          CALL MY_ORDERS(MODE, WORK, ITRI, INDX, NSH , 3)
          NS = 1 
          NFIRST(1) = 1
          NLAST(1) = 1
          DO I=2,NSH
           II=ITRI(1,INDX(I))
           JJ=ITRI(1,INDX(I-1))
           II1=ITRI(2,INDX(I))
           JJ1=ITRI(2,INDX(I-1))
           II2=ITRI(3,INDX(I))
           JJ2=ITRI(3,INDX(I-1))
           IF(II /= JJ .OR. II1 /= JJ1  .OR.  II2 /= JJ2) THEN    
               NS = NS + 1
               NFIRST(NS) = I
               NLAST(NS) = NFIRST(NS) 
            ELSE
               NLAST(NS) = NLAST(NS) + 1
            ENDIF
          ENDDO
C
C  substack 
C 
          NPT_STACK =  0
          NS_STACK = NS
C
          DO IS = 1,NS 
            ID  = NFIRST(IS)
            I = INDX(ID)
            II  = INDX_SH(I)
            NPT = IWORKSH(1,II)
            NPT_STACK = MAX(NPT_STACK,NPT)
          ENDDO   
C allocation  
          ALLOCATE(IWORKS%IGEO(3*NPT_STACK+2,NS_STACK))
          ALLOCATE(IWORKS%GEO(6*NPT_STACK+1,NS_STACK))
C         
          IWORKS%IGEO = 0
          IWORKS%GEO = ZERO
C         
          DO IS = 1,NS
            NGEO_STACK = NUMGEO + IS
            ID  = NFIRST(IS)
C          
            I = INDX(ID)
            II  = INDX_SH(I)
            PID =  PID_SH(I)
!!            MAIN_PID(1,IS) =  PID_SH(II)  ! pid
!!            MAIN_PID(2,IS) =  IWORKSH(1,II) ! npt
            NPT = IWORKSH(1,II)
!C            ISUBSTACK(PID) = ISUBSTACK(PID) + 1
            IIS = II 
C          
            DO I= NFIRST(IS) , NLAST(IS)
              ID = INDX(I)
              II = INDX_SH(ID)
              IWORKSH(2,II) = NGEO_STACK 
              IWORKSH(3,II) = IS  ! compter for all stack
!!            IWORKSH(3,II) = ISUBSTACK(PID)   ! computer by stack ! old conception
            ENDDO            
C Geo and Igeo organisation
!!          I  = NFIRST(IS)
!!          II = INDX_Q4(I)
!!          PID = PID_Q4(II)
!!          NSS =  ISUBSTACK(PID) ! number of substack in each Pid       
C geometric property for each sub stack        
!!!          IGEO(   ,PID)  = NGEO_STACK
            N1 = INT(GEO(6,PID))
            NP = 0
            NUMS = NUMGEO_STACK(PID)
            DO 700 J = 1,N1
!!                JPID = IGEO(100 + J, PID)
                JPID  = STACK_INFO(NUMS)%PID(J)
                IF(NP <= NPT) THEN
                 DO JJ = 1,NPT
                   JJPID =  IWORK_T(IIS)%IPT(JJ)
                   IF(JJPID == JPID) THEN
                     NP = NP + 1
                     IPTPLY(NP) = J 
                     GOTO 700
                   ENDIF
                 ENDDO
               ENDIF  
 700         CONTINUE
C geometric property
             IWORKS%IGEO(1,IS) = NPT
             IWORKS%IGEO(2,IS) = PID
             IPPID = 2
             IPMAT = IPPID + NPT
             IPMAT_IPLY = IPMAT + NPT
             IPANG = 1
             IPTHK  = IPANG + NPT
             IPPOS  = IPTHK + NPT
             IPDIR  = IPPOS + NPT
             IPTHKLY  = IPDIR + NPT
             IPWEIGHT  = IPTHKLY + NPT
             NUMS= NUMGEO_STACK(PID)
             DO J=1,NPT
              JSTACK = IPTPLY(J)
              IWORKS%IGEO(IPPID  + J     ,IS)  = STACK_INFO(NUMS)%PID(JSTACK) 
              IWORKS%IGEO(IPMAT  + J     ,IS)  = STACK_INFO(NUMS)%MID(JSTACK) 
              IWORKS%IGEO(IPMAT_IPLY + J ,IS)  = STACK_INFO(NUMS)%MID_IP(JSTACK)
              IWORKS%GEO(IPANG   + J     ,IS)  = STACK_INFO(NUMS)%ANG(JSTACK)
              IWORKS%GEO(IPTHK   + J     ,IS)  = STACK_INFO(NUMS)%THK(JSTACK)
              IWORKS%GEO(IPPOS   + J     ,IS)  = STACK_INFO(NUMS)%POS(JSTACK)
              IWORKS%GEO(IPDIR   + J     ,IS)  = STACK_INFO(NUMS)%DIR(JSTACK)
              IWORKS%GEO(IPTHKLY  + J    ,IS)  = STACK_INFO(NUMS)%THKLY(JSTACK)
              IWORKS%GEO(IPWEIGHT  + J   ,IS)  = STACK_INFO(NUMS)%WEIGHT(JSTACK)
             ENDDO
C                      
C to be  clarified later IPOS > 0 
            IPOS = IGEO(99,PID) 
            ZSHIFT = GEO(199,PID)
            IF(IPOS == 1)THEN                                              
             TMIN = EP20                                                    
             TMAX = -EP20                                                   
             DO J=1,NPT                                                     
              DT = HALF*IWORKS%GEO(IPTHK   + J     ,IS)                    
              TMIN = MIN(TMIN,IWORKS%GEO(IPPOS   + J     ,IS)-DT)            
              TMAX = MAX(TMAX,IWORKS%GEO(IPPOS   + J     ,IS)+DT)            
             ENDDO                                                           
             THICKT = TMAX - TMIN               
             DO J=1,NPT  
              IWORKS%GEO(IPTHK+J,IS)=IWORKS%GEO(IPTHK+J,IS)/MAX(THICKT,EM20) 
              IWORKS%GEO(IPPOS+J,IS)=IWORKS%GEO(IPPOS+J,IS)/MAX(THICKT,EM20)  
             ENDDO 
                                                                        
          ELSE                                                          
               THICKT = ZERO                                       
               DO J=1,NPT                                
                 THICKT = THICKT + IWORKS%GEO(IPTHK+J,IS)
               ENDDO           
               DO J=1,NPT 
                 IWORKS%GEO(IPTHK+J,IS) =                                   
     .                    IWORKS%GEO(IPTHK+J,IS)/MAX(THICKT,EM20)
               ENDDO
C                
               IF(IPOS == 0 )THEN
                 ZSHIFT = - HALF
               ELSEIF(IPOS == 2 )THEN
                 ZSHIFT =  -  ZSHIFT /MAX(THICKT,EM20)
               ELSEIF(IPOS == 3) THEN
                 ZSHIFT = -ONE
               ELSEIF(IPOS == 4) THEN
                 ZSHIFT = ZERO
               ENDIF                   
C---             calcul automatique de position des couches             
                 IWORKS%GEO(IPPOS+1,IS) = ZSHIFT + HALF*IWORKS%GEO(IPTHK+1,IS)
                 DO J=2,NPT                                               
                   IWORKS%GEO(IPPOS+J,IS) = IWORKS%GEO(IPPOS+J-1,IS)            
     .               + HALF*(IWORKS%GEO(IPTHK+J,IS)+IWORKS%GEO(IPTHK+J-1,IS))   
                 ENDDO
C               
            ENDIF ! IPOS =0,1,2,3,4                                            
C calcul du thk by shell
            IWORKS%GEO(1,IS) = THICKT
C============================================================================
C---
C  update the shell thickness   NDRAPE =0         
C---
              DO I= NFIRST(IS) , NLAST(IS)
                ID = INDX(I)
                II = INDX_SH(ID)
                IF (THK(II) == ZERO) THK(II) = THICKT
              ENDDO             
C============================================================================
cc          IF (IGTYP == 51) THEN
cc            NLAY = MAX(1,NPT)
C---
C   - various nb of integration points through each layer
C---
C
C---  TEST de calcul automatique de position des NPTT dans les couches
C
cc            IPPID = 2
cc            DO ILAY=1,NLAY
cc!!            IPIDL = IGEO(IPPID + ILAY,PID)
cc              THK_LY  = IWORKS%GEO(IPTHK  + ILAY,IS)  ! layer thickness ratio
cc              POS_LY  = IWORKS%GEO(IPPOS  + ILAY,IS)  ! layer position ratio
cc              IPID_LY = IWORKS%IGEO(IPPID + ILAY,IS)  ! layer PID (igtyp = 19)
cc              MAT_LY  = IWORKS%IGEO(IPMAT + ILAY,IS)  ! layer material
cccc              IINT  = IGEO(47,IPID_LY)
cc              IINT  = IGEO(47,PID)
cc              NPTT  = IGEO(4,IPID_LY)
cc              THICKT = IWORKS%GEO(1,IS)
cc              IF (IINT == 1) THEN  !  uniform distribution - by default
cc                DO IT=1,NPTT
cc                  THK_IT(IT) = THK_LY/NPTT  ! equally distribution of NPTT through layer
cc                ENDDO 
cc                POS_0 = POS_LY - HALF*THK_LY
cc                IF (NLAY == 1) POS_0 = - HALF !! special case
cc                POS_IT(1) = POS_0 + HALF*THK_IT(1)
cccc                POS_IT(1) = POS_LY - HALF*THK_LY + HALF*THK_IT(1)
cc                DO IT=2,NPTT
cc                  POS_IT(IT) = POS_IT(IT-1) + HALF*(THK_IT(IT) + THK_IT(IT-1))
cc                ENDDO
cc              ELSEIF (IINT == 2) THEN  !  Gauss distribution
cc                DO IT=1,NPTT
cc                  THK_IT(IT) = HALF*THK_LY*W_GAUSS(IT,NPTT)
cc                  POS_IT(IT) = POS_LY + HALF*THK_LY*A_GAUSS(IT,NPTT)
cc                ENDDO
cc              ENDIF
cc            ENDDO  !  DO ILAY=1,NLAY
cc          ENDIF  !  IF (IGTYP == 51) THEN
C============================================================================
        ENDDO ! DO IS = 1,NS 
C           
        DEALLOCATE(ICSH,INDX,ITRI)
         DO I=1,NUMELC + NUMELTG
             DEALLOCATE(IWORK_T(I)%IPT)
         ENDDO
         DEALLOCATE(IWORK_T)
       ENDIF
C 
C   pccommp part
C        
C---       
       NS_STACK0 = NS_STACK
       NPT_STACK0 = NPT_STACK
C       
       IF(IPART_PCOMPP > 0) THEN
         NPLY = 0
         NSTACK = 0
         DO I = 1, NUMPLY
!! Only one stack by ply
           IDS  = IGEO_STACK(42,NUMSTACK + I)
           IF (IDS > 0) THEN 
               NPLY = NPLY+1
               IPIDPLY(NPLY) = NUMSTACK + I
               IDGR4N(NPLY)  = IGEO_STACK(40,NUMSTACK + I) ! groupe shell 4N id
               IDGR3N(NPLY)  = IGEO_STACK(41,NUMSTACK + I) ! groupe shell 3N id
           ENDIF  
         ENDDO 
C                 
C  transformation d'id groupe 
C       
          DO 11 I=1,NPLY
C shell 4N id group                       
            ID = IDGR4N(I)
            IF(ID > 0) THEN   
               DO J=1,NGRSHEL
                  JD = IGRSH4N(J)%ID
                  IF(JD == ID)THEN
                    IDGR4N(I) = J
                    GOTO 22
                  ENDIF  
               ENDDO
             ENDIF ! ID > 0
C !GR  T3       
 22        CONTINUE
            ID = IDGR3N(I)
            IF(ID > 0) THEN
              DO J=1,NGRSH3N
                JD = IGRSH3N(J)%ID
                IF(JD == ID)THEN
                  IDGR3N(I) = J
                  GOTO 11
                ENDIF  
              ENDDO
             ENDIF  ! ID > 0           
11      CONTINUE 
        !!  stop
C 
C 
C tag of ply element        
          ALLOCATE(ICSH_STACK(NUMELC + NUMELTG) )
          IF(NUMELC + NUMELTG > 0)ICSH_STACK = 0
C   compteur by element               
            DO I= 1,NPLY
              J   = IDGR4N(I)
              J4N = J
              IDPLY = IPIDPLY(I) 
              IDS = IGEO_STACK(42, IDPLY)
             IF(J > 0 .AND. IDS > 0 ) THEN
              NEL = IGRSH4N(J)%NENTITY
C element type Q4 
!!              ITY = IGRN(4,J)
              ITY = IGRSH4N(J)%GRTYPE
                 DO  111 II = 1,NEL
                   IDSHEL = IGRSH4N(J)%ENTITY(II)
                   PID = IXC(6,IDSHEL)
                   IGTYP = IGEO(11,PID)
                   IF(IGTYP == 52) THEN
                     IF(ICSH_STACK(IDSHEL) == 0) THEN 
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                          ICSH_STACK(IDSHEL) = IDS
                      ELSEIF(ICSH_STACK(IDSHEL) == IDS) THEN
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                      ELSE
C  message d'erreur      
                        IPID_1=IGEO_STACK(1,ICSH_STACK(IDSHEL))
                        NGL =IXC(NIXC,IDSHEL)
                        CALL ANCMSG(MSGID=1152,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NGL,
!!     .                    C2='SHELL',
     .                    I2= IGEO_STACK(1,IDS),
     .                    I3=  IGEO_STACK(1,IPID_1) )
                      ENDIF
                    ENDIF 
 111             CONTINUE
              ENDIF 
               J   = IDGR3N(I)
               J3N = J
               IF(J > 0 .AND. IDS > 0 ) THEN
                NEL = IGRSH3N(J)%NENTITY
C element type T3 
                ITY = IGRSH3N(J)%GRTYPE
                 DO 222 II = 1,NEL
! c a verifier l'id du triangle

                   ISH3N = IGRSH3N(J)%ENTITY(II)
                   PID = IXTG(5,ISH3N)
                   IGTYP = IGEO(11,PID)
                   IF(IGTYP == 52) THEN
                       IDSHEL = ISH3N + NUMELC
                      IF(ICSH_STACK(IDSHEL) == 0) THEN  
                          IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                          ICSH_STACK(IDSHEL) = IDS
                      ELSEIF(ICSH_STACK(IDSHEL) == IDS) THEN
                          IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                      ELSE
C  message d'erreur
                        IPID_1=IGEO_STACK(1,ICSH_STACK(IDSHEL))
                        NGL =IXTG(NIXTG,IDSHEL)
                        CALL ANCMSG(MSGID=1152,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NGL,
!!     .                    C2='SHE3N',
     .                    I2= IGEO_STACK(1,IDS),
     .                    I3=  IGEO_STACK(1,IPID_1) )
                      ENDIF
                   ENDIF  
 222            CONTINUE
             ENDIF 
           ENDDO !  I   ply groupe
C
!!         ENDDO  ! iply    
!!!------------------------------------------------
           ALLOCATE(IWORK_T(NUMELC+NUMELTG),ICSH(NUMELC+NUMELTG))
           IF(NUMELC+NUMELTG > 0) ICSH = 0
           IF(NUMELC+NUMELTG > 0) ICSH_STACK = 0
           DO I=1,NUMELC + NUMELTG
             NULLIFY(IWORK_T(I)%IPT)
             NPT = IWORKSH(1,I)
             ALLOCATE(IWORK_T(I)%IPT(NPT))
             IWORK_T(I)%IPT = 0
             IWORKSH(1,I) = 0
           ENDDO
C
            DO I= 1,NPLY
              J   = IDGR4N(I)
              J4N = J
              IDPLY = IPIDPLY(I) 
              IDS = IGEO_STACK(42, IDPLY)
             IF(J > 0 .AND. IDS > 0 ) THEN
              NEL = IGRSH4N(J)%NENTITY
C element type Q4 
!!              ITY = IGRN(4,J)
              ITY = IGRSH4N(J)%GRTYPE
                 DO  II = 1,NEL
                   IDSHEL = IGRSH4N(J)%ENTITY(II)
                   PID = IXC(6,IDSHEL)
                   IGTYP = IGEO(11,PID)
                   IF(IGTYP == 52) THEN
                     IF(ICSH_STACK(IDSHEL) == 0) THEN 
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                          NPT = IWORKSH (1,IDSHEL)
                          IWORK_T(IDSHEL)%IPT(NPT) = IDPLY
                          ICSH(IDSHEL) = ICSH(IDSHEL) + IDPLY*IDPLY 
                          ICSH_STACK(IDSHEL) = IDS
                      ELSEIF(ICSH_STACK(IDSHEL) == IDS) THEN
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                          NPT = IWORKSH (1,IDSHEL)
                          IWORK_T(IDSHEL)%IPT(NPT) = IDPLY
                          ICSH(IDSHEL) = ICSH(IDSHEL) + IDPLY*IDPLY
                      ELSE
C  message d'erreur      
                        IPID_1=IGEO_STACK(1,ICSH_STACK(IDSHEL))
                        NGL =IXC(NIXC,IDSHEL)
                        CALL ANCMSG(MSGID=1152,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NGL,
!!     .                    C2='SHELL',
     .                    I2= IGEO_STACK(1,IDS),
     .                    I3=  IGEO_STACK(1,IPID_1) )                 
                      ENDIF
                    ENDIF 
                ENDDO
              ENDIF 
               J   = IDGR3N(I)
               J3N = J
               IF(J > 0 .AND. IDS > 0 ) THEN
                NEL = IGRSH3N(J)%NENTITY
C element type T3 
                ITY = IGRSH3N(J)%GRTYPE
                 DO  II = 1,NEL
! c a verifier l'id du triangle

                   ISH3N = IGRSH3N(J)%ENTITY(II)
                   PID = IXTG(5,ISH3N)
                   IGTYP = IGEO(11,PID)
                   IF(IGTYP == 52) THEN
                      IDSHEL = ISH3N + NUMELC
                      IF(ICSH_STACK(IDSHEL) == 0) THEN  
                          IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                          NPT = IWORKSH(1,IDSHEL)
                          IWORK_T(IDSHEL)%IPT(NPT) = IDPLY
                          ICSH(IDSHEL) = ICSH(IDSHEL) + IDPLY*IDPLY
                          ICSH_STACK(IDSHEL) = IDS
                      ELSEIF(ICSH_STACK(IDSHEL) == IDS) THEN
                           IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                           NPT = IWORKSH(1,IDSHEL)
                           IWORK_T(IDSHEL)%IPT(NPT) = IDPLY
                           ICSH(IDSHEL) = ICSH(IDSHEL) + IDPLY*IDPLY
                      ELSE
C  message d'erreur
                        IPID_1=IGEO_STACK(1,ICSH_STACK(IDSHEL))
                        NGL =IXTG(NIXTG,IDSHEL)
                        CALL ANCMSG(MSGID=1152,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NGL,
!!     .                    C2='SHE3N',
     .                    I2= IGEO_STACK(1,IDS),
     .                    I3=  IGEO_STACK(1,IPID_1) )
                      ENDIF
                   ENDIF  
                ENDDO ! II 
             ENDIF 
           ENDDO !  I   ply groupe
!!!------------------------------------------
C 
C   mise en groupe des stack
C            
          NSH = 0
          INDX_SH = 0
          PID_SH = 0       
C                  
          DO I=1,NUMELC
               PID = IXC(6,I)
               IGTYP = IGEO(11,PID)
C               
               IS = ICSH_STACK(I)
               IF(IGTYP == 52 ) THEN
                   NSH = NSH +  1
                   INDX_SH(NSH) = I
                   PID_SH(NSH) = PID 
              ENDIF  
          ENDDO 
C
          DO I=1,NUMELTG
               PID = IXTG(5,I)
               IGTYP = IGEO(11,PID)
C               
               IS = ICSH_STACK(NUMELC + I)
               IF(IGTYP == 52 )THEN
                   NSH = NSH +  1
                   INDX_SH(NSH) = I + NUMELC
                   PID_SH(NSH) = PID
              ENDIF  
          ENDDO   
C       
C
C
          ALLOCATE(INDX(2*NSH),ITRI(3,NSH))
          INDX = 0  
          ITRI = 0
          DO I = 1,NSH
            INDX(I) = I
            II = INDX_SH(I)
            ITRI(1,I) = PID_SH(I)
            ITRI(2,I) = ICSH(II)
            ITRI(3,I) = IWORKSH(1,II)
          ENDDO
C          
           MODE = 0
!!         WORK = 0 
           CALL MY_ORDERS(MODE, WORK, ITRI, INDX, NSH , 3)
C       
           NS = 1
           NFIRST(1) = 1
           NLAST(1) = 1
           DO I=2,NSH
             II=ITRI(1,INDX(I))
             JJ=ITRI(1,INDX(I-1))
             II1=ITRI(2,INDX(I))
             JJ1=ITRI(2,INDX(I-1))
             II2=ITRI(3,INDX(I))
             JJ2=ITRI(3,INDX(I-1))
             IF(II /= JJ .OR. II1 /= JJ1  .OR.  II2 /= JJ2) THEN    
                NS = NS + 1
                NFIRST(NS) = I
                NLAST(NS) = NFIRST(NS) 
             ELSE
                NLAST(NS) = NLAST(NS) + 1
             ENDIF
           ENDDO
C
C  sous stack 
C    
           ALLOCATE(IDSTACK(NS))
           IDSTACK = 0
           NS_STACK = NS_STACK + NS 
           DO IS = 1,NS
             ID  = NFIRST(IS)
             I = INDX(ID)
             II  = INDX_SH(I)
             NPT = IWORKSH(1,II)
             NPT_STACK = MAX(NPT_STACK,NPT)
C             
             IDS = ICSH_STACK(II)
             IDSTACK(IS) = IDS
           ENDDO   
C        
C allocation
C  
           ALLOCATE(STACK%IGEO(4*NPT_STACK+2,NS_STACK))
           ALLOCATE(STACK%GEO(6*NPT_STACK+1,NS_STACK))
           ALLOCATE(STACK%PM(20,NS_STACK))
C           
           STACK%IGEO = 0
           STACK%GEO = ZERO
           STACK%PM = ZERO
C         
           DO IS = 1,NS
C a changer        
              NGEO_STACK = NUMGEO + NUMSTACK + NUMPLY +  IS  !!!!!!! limit id i will change it 
              ID  = NFIRST(IS)
C              
              I = INDX(ID)
              II  = INDX_SH(I)
              PID =  PID_SH(I)
!!              MAIN_PID(1,IS) =  PID_SH(II)  ! pid
!!              MAIN_PID(2,IS) =  IWORKSH(1,II) ! npt
              NPT = IWORKSH(1,II)
              IIS = II               
C              
             DO I= NFIRST(IS) , NLAST(IS)
                 ID = INDX(I)
                 II = INDX_SH(ID)
                 IWORKSH(2,II) = NGEO_STACK 
                 IWORKSH(3,II) = NS_STACK0 + IS   ! compter for all stack
             ENDDO            
C Cp igeo_stack and Geo_stack dans IGEO, GEO --ppccomp 
             IGTYP = IGEO(11,PID)
             DO J=2,NPROPGI - LTITR
               IGEO(J,PID) = IGEO_STACK(J,IDSTACK(IS))
             ENDDO   
             IGEO(11,PID) = IGTYP        
!        
             DO J=1,NPROPG
               GEO(J,PID) = GEO_STACK(J,IDSTACK(IS)) 
             ENDDO 
C
             N1 = INT(GEO(6,PID))
             NP = 0
             NUMS = NUMGEO_STACK(NUMGEO + IDSTACK(IS))
             DO 777 J = 1,N1
                 JPID = STACK_INFO(NUMS)%PID(J)
                 IF(NP <= NPT) THEN
                  DO JJ = 1,NPT
                    JJPID =  IWORK_T(IIS)%IPT(JJ)
                    IF(JJPID == JPID) THEN
                      NP = NP + 1
                      IPTPLY(NP) = J 
                      GOTO 777
                    ENDIF
                  ENDDO
                ENDIF  
 777       CONTINUE
C geometric property
C          
          IIS = NS_STACK0 + IS
          STACK%IGEO(1,IIS) = NPT
          STACK%IGEO(2,IIS) = PID
          IPPID = 2
          IPMAT = IPPID + NPT
          IPMAT_IPLY = IPMAT + NPT
C
          IPANG = 1
          IPTHK  = IPANG + NPT
          IPPOS  = IPTHK + NPT
          IPDIR  = IPPOS + NPT
          IPTHKLY = IPDIR + NPT
          IPWEIGHT =IPTHKLY + NPT
C stack id
!old           IPPIDS  = 100
!old           IPMATS  = 300
!old           IPMAT_IPLYS = 500
!old           IPANGS  = 200
!old           IPTHKS  = 400
!old           IPPOSS  = 600
!old           IPDIRS  = 800
           PIDS = IDSTACK(IS)
           NUMS = NUMGEO_STACK(NUMGEO + PIDS)
           DO J=1,NPT
            JS = IPTPLY(J)
            STACK%IGEO(IPPID+J       ,IIS)  = STACK_INFO(NUMS)%PID(JS)
            STACK%IGEO(IPMAT + J     ,IIS)  = STACK_INFO(NUMS)%MID(JS)
            STACK%IGEO(IPMAT_IPLY+J  ,IIS)  = STACK_INFO(NUMS)%MID_IP(JS) 
            STACK%GEO(IPANG + J      ,IIS)  = STACK_INFO(NUMS)%ANG(JS)
            STACK%GEO(IPTHK + J      ,IIS)  = STACK_INFO(NUMS)%THK(JS)
            STACK%GEO(IPPOS + J      ,IIS)  = STACK_INFO(NUMS)%POS(JS)
            STACK%GEO(IPDIR + J      ,IIS)  = STACK_INFO(NUMS)%DIR(JS)
            STACK%GEO(IPTHKLY  + J   ,IIS)  = STACK_INFO(NUMS)%THKLY(JS)
            STACK%GEO(IPWEIGHT  + J  ,IIS)  = STACK_INFO(NUMS)%WEIGHT(JS)
           ENDDO 
C                      
C to be  clarified later IPOS > 0 
           IPOS = IGEO(99,PID) 
           ZSHIFT = GEO(199,PID)
           IF(IPOS == 1)THEN                                             
            TMIN = EP20                                                     
            TMAX = -EP20                                                    
            DO J=1,NPT                                                      
             DT = HALF*STACK%GEO(IPTHK   + J  ,IIS)                     
             TMIN = MIN(TMIN,STACK%GEO(IPPOS   + J ,IIS)-DT)             
             TMAX = MAX(TMAX,STACK%GEO(IPPOS   + J ,IIS)+DT)             
            ENDDO                                                            
            THICKT = TMAX - TMIN               
            DO J=1,NPT  
               STACK%GEO(IPTHK+J,IIS)=
     .                 STACK%GEO(IPTHK+J,IIS)/MAX(THICKT,EM20)  
               STACK%GEO(IPPOS+J,IIS)=
     .                 STACK%GEO(IPPOS+J,IIS)/MAX(THICKT,EM20)   
            ENDDO 
                                                                       
          ELSE                                                          
               THICKT = ZERO                                       
               DO J=1,NPT                                
                 THICKT = THICKT + STACK%GEO(IPTHK+J,IIS)
               ENDDO           
               DO J=1,NPT 
                 STACK%GEO(IPTHK+J,IIS) =                                   
     .                STACK%GEO(IPTHK+J,IIS)/MAX(THICKT,EM20)
               ENDDO
C                
               IF(IPOS == 0 )THEN
                 ZSHIFT = - HALF
               ELSEIF(IPOS == 2 )THEN
                 ZSHIFT =  -  ZSHIFT /MAX(THICKT,EM20)
               ELSEIF(IPOS == 3) THEN
                 ZSHIFT = -ONE
               ELSEIF(IPOS == 4) THEN
                 ZSHIFT = ZERO
               ENDIF                   
C---             calcul automatique de position des couches             
                 STACK%GEO(IPPOS+1,IIS) = ZSHIFT +
     .                          HALF*STACK%GEO(IPTHK+1,IIS)
                 DO J=2,NPT                                               
                  STACK%GEO(IPPOS+J,IIS) =   
     .                    STACK%GEO(IPPOS+J-1,IIS)      +
     .                    HALF*(STACK%GEO(IPTHK+J,IIS)+
     .                            STACK%GEO(IPTHK+J-1,IIS))   
                 ENDDO
C               
            ENDIF ! IPOS =0,1,2,3,4                                                 
C calcul du thk by shell
              STACK%GEO(1,IIS) = THICKT
C============================================================================
C---
C  update the shell thickness if /DRAPE defined          
C---
!!  (NDRAPE == 0) 
              DO I= NFIRST(IS) , NLAST(IS)
                ID = INDX(I)
                II = INDX_SH(ID)
                IF (THK(II) == ZERO) THK(II) = THICKT
              ENDDO
C============================================================================
cc          IF (IGTYP == 52) THEN
cc            NLAY = MAX(1,NPT)
C---
C   - various nb of integration points through each layer
C---
C
C---  TEST de calcul automatique de position des NPTT dans les couches
C
cc            IPPID = 2
cc            DO ILAY=1,NLAY
cc!!            IPIDL = IGEO(IPPID + ILAY,PID)
cc              THK_LY  = STACK%GEO(IPTHK  + ILAY,IIS)  ! layer thickness ratio
cc              POS_LY  = STACK%GEO(IPPOS  + ILAY,IIS)  ! layer position ratio
cc              IPID_LY = STACK%IGEO(IPPID + ILAY,IIS)  ! layer PID (igtyp = 19)
cc              MAT_LY  = STACK%IGEO(IPMAT + ILAY,IIS)  ! layer material
cccc              IINT  = IGEO(47,IPID_LY)
cc              IINT  = IGEO_STACK(47,PIDS)
cc              NPTT  = IGEO_STACK(4,PIDS)
cc              THICKT = STACK%GEO(1,IIS)
cc              IF (IINT == 1) THEN  !  uniform distribution - by default
cc                DO IT=1,NPTT
cc                  THK_IT(IT) = THK_LY/NPTT  ! equally distribution of NPTT through layer
cc                ENDDO 
cc                POS_0 = POS_LY - HALF*THK_LY
cc                IF (NLAY == 1) POS_0 = - HALF !! special case
cc                POS_IT(1) = POS_0 + HALF*THK_IT(1)
cccc                POS_IT(1) = POS_LY - HALF*THK_LY + HALF*THK_IT(1)
cc                DO IT=2,NPTT
cc                  POS_IT(IT) = POS_IT(IT-1) + HALF*(THK_IT(IT) + THK_IT(IT-1))
cc                ENDDO
cc              ELSEIF (IINT == 2) THEN  !  Gauss distribution
cc                DO IT=1,NPTT
cc                  THK_IT(IT) = HALF*THK_LY*W_GAUSS(IT,NPTT)
cc                  POS_IT(IT) = POS_LY + HALF*THK_LY*A_GAUSS(IT,NPTT)
cc                ENDDO
cc              ENDIF
cc            ENDDO  !  DO ILAY=1,NLAY
cc          ENDIF  !  IF (IGTYP == 52) THEN
C============================================================================
C---
            IPPID = 2
            DO ILAY=1,NPT
                  PIDS =  STACK%IGEO(IPPID + ILAY      ,IIS)
                  NPTT  = IGEO_STACK(4,PIDS)
                  IGEO(4,PID) = MAX(IGEO(4,PID),NPTT)
            ENDDO   
        ENDDO ! DO IS = 1,NS
C---
         DEALLOCATE(ICSH,INDX,ITRI,IDSTACK, ICSH_STACK)
         DO I=1,NUMELC + NUMELTG
             DEALLOCATE(IWORK_T(I)%IPT)
           ENDDO
           DEALLOCATE(IWORK_T)
       ENDIF
C
       IF(IPART_STACK > 0) THEN  
         IF(IPART_PCOMPP == 0) THEN
            ALLOCATE(STACK%IGEO(4*NPT_STACK0+2,NS_STACK0))
            ALLOCATE(STACK%GEO(6*NPT_STACK0+1,NS_STACK0))
            ALLOCATE(STACK%PM(20,NS_STACK0))
            STACK%IGEO = 0
            STACK%GEO = ZERO
            STACK%PM = ZERO 
         ENDIF
         DO IS = 1,     NS_STACK0 
              DO J = 1, 3*NPT_STACK0 + 2
                STACK%IGEO(J, IS ) = IWORKS%IGEO(J,IS)
              ENDDO
              DO J = 1, 6*NPT_STACK0+1
                STACK%GEO(J, IS ) = IWORKS%GEO(J,IS)
                
              ENDDO
         ENDDO
         DEALLOCATE(IWORKS%IGEO, IWORKS%GEO) 
       ENDIF
C ---     update of sub-stack
       IF(NS_STACK > 0) THEN
           DO IS = 1,NS_STACK
              NPT = STACK%IGEO(1,IS) 
              PID = STACK%IGEO(2,IS)
              THICKT = STACK%GEO(1,IS)  
              ID = IGEO(1,PID)
              IGTYP = IGEO(11,PID)
C
              WRITE(IOUT,1000)ID, IS
              WRITE(IOUT,1100) THICKT,NPT
!!              IPANG = 1            
!!              IPTHK  = IPANG + NPT 
!!              IPPOS  = IPTHK + NPT 
              IPPOS  = 1 + 2*NPT 
              IPPID = 2
              IF(IGTYP == 52) THEN
                DO J = 1,NPT
                    PID = STACK%IGEO(IPPID + J      ,IS)
                    POS = STACK%GEO( IPPOS + J      ,IS)
                    POS = POS*THICKT
                    ID = IGEO_STACK(1,PID) 
                    WRITE(IOUT,2000)J, ID , POS
                ENDDO
              ELSE
                 DO J = 1,NPT
                     PID = STACK%IGEO(IPPID + J      ,IS)
                     POS = STACK%GEO( IPPOS + J      ,IS)
                     POS = POS*THICKT
                     ID = IGEO(1,PID) 
                     WRITE(IOUT,2000)J, ID , POS
                 ENDDO
              ENDIF 
           ENDDO
        ENDIF 
C for restart       
       IF(IPART_PCOMPP > 0 .AND. IPART_STACK == 0) IPART_STACK = 1
C--------
     
      RETURN
 1000    FORMAT(//,
     & 5X,'COMPOSITE STACK SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'//,
     &    7X,'PROPERTY SET NUMBER . . . . . . . . .  . ..=',I10/,
     &    7X,'SUB PROPERTY SET NUMBER . . . . . . . . . .=',I10/)
 1100   FORMAT(
     & 8X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/
     & 8X,'NUMBER OF PLIES. . . . . . . . . . . . =',I10/)
 2000 FORMAT(
     & 8X,'    PLY ',I3/,
     & 8X,'      PLY PID NUMBER  . . . . . . . . .=',I10/
     & 8X,'      POSITION. . . . . . . . . . . . .=',1PG20.13/)

      END
