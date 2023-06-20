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
Chd|  STACKGROUP_DRAPE              source/stack/stackgroup_drape.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE STACKGROUP_DRAPE(DRAPE ,DRAPEG , IWORK_T, IWORKSH , 
     .                     IGRSH3N    ,IGRSH4N  ,IXC   ,IXTG ,
     .                     IGEO       ,GEO      ,THK    ,STACK   ,
     .                     IGEO_STACK ,GEO_STACK ,  STACK_INFO ,
     .                     NUMGEO_STACK,NPROP_STACK , PLY_INFO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE STACK_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE DRAPE_MOD
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
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,NUMELC),
     .        IXTG(NIXTG,NUMELTG),IGEO(NPROPGI,NUMGEO),IWORKSH(3,NUMELC+NUMELTG),
     .        IGEO_STACK(NPROPGI,NUMSTACK + NUMPLY),NUMGEO_STACK(NUMGEO+NUMSTACK),
     .        NPROP_STACK
      INTEGER  , INTENT(INOUT) :: PLY_INFO(2,NUMPLY)
      my_real
     .       GEO(NPROPG,NUMGEO),THK(NUMELC+NUMELTG),GEO_STACK(NPROPG,NUMSTACK + NUMPLY)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (DRAPE_)  , DIMENSION(NUMELC + NUMELTG)  , TARGET :: DRAPE
      TYPE (DRAPEG_)                                 , TARGET :: DRAPEG
      TYPE(DRAPE_WORK_) , DIMENSION(NUMELC + NUMELTG) , TARGET :: IWORK_T
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IGTYP,ID,JD,IDPLY,NEL,
     .        IAD,ITY,IDSHEL,PID,IS,IDS,NSH,MODE,NS,JJ,NGEO_STACK,
     .        IGRTYP,N1,IPMAT,IPANG,IPTHK,IIGEO,NSS,IPPOS,NPT,IIS,NP,
     .        JJPID,JSTACK,JPID,ITG,IPMAT_IPLY,ISH3N,J4N,J3N,IPOS,
     .        MAT_LY,NLAY,NPTT,IPIDL,IT,ILAY,IPTHK_NPTT,IPPOS_NPTT,
     .        IINT,IPID_LY,IPDIR ,NS_STACK0 ,NPT_STACK0,IS0,JS,PIDS,IP,
     .       II1,II2,JJ1,JJ2,NSLICE,IE_DRP,NPT_LAY,IPNPT_LAY
      INTEGER , DIMENSION(:), ALLOCATABLE ::  WORK,INDX_SH,PID_SH,
     .                                        NFIRST,NLAST,ISUBSTACK,IPTPLY
      INTEGER  :: NBFI,IPPID,NGL,IPID_1,NUMS,IPWEIGHT,IPTHKLY,NSHQ4,NSHT3  
      my_real
     .         THICKT,ZSHIFT,TMIN,TMAX,DT,THK_LY,POS_LY,THK_IT(100),
     .         POS_IT(100),POS_NPTT,THK_NPTT,POS_0,THINNING,POS
     
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI
      INTEGER, DIMENSION (:) ,ALLOCATABLE ::ICSH,INDX,IDSTACK
      TYPE (STACK_PLY) :: STACK, IWORKS
      TYPE(STACK_INFO_ ) , DIMENSION (1:NPROP_STACK) :: STACK_INFO
      TYPE (DRAPE_PLY_),  POINTER :: DRAPE_PLY
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
C=======================================================================
C    For Shell
C-----------------------------------------------   
       NS_STACK = 0
       NPT_STACK = 0
C
       ALLOCATE (INDX_SH(NUMELC+NUMELTG),PID_SH(NUMELC+NUMELTG),
     .            NFIRST(NUMELC+NUMELTG) ,NLAST(NUMELC+NUMELTG),
     .            ISUBSTACK(NUMGEO+NUMSTACK),
     .            IPTPLY(NUMGEO+NUMPLY), WORK(70000) ) 
         
       WORK = 0       
       INDX_SH = 0    
       PID_SH = 0     
       NFIRST = 0     
       NLAST = 0      
       ISUBSTACK = 0  
       IPTPLY = 0     
       INDX_SH = 0    
       PID_SH = 0 
       !! Type51 and TYPE17    
       IF(IPART_STACK > 0) THEN
         NSH = 0                  
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
                 INDX_SH(NSH) = I + NUMELC
                 PID_SH(NSH) = PID
            ENDIF  
         ENDDO  
C #####################################################" 
           ALLOCATE(ICSH(NUMELC + NUMELTG))
           IF(( NUMELC + NUMELTG) > 0)ICSH = 0
           DO I=1,NSH
             II = INDX_SH(I) 
             PID = PID_SH(I)
             IGTYP = IGEO(11,PID)
             NPT = IWORKSH(1,II)                                      
             IE_DRP = DRAPEG%INDX(II)
             IF(IGTYP == 17) THEN
              IF(NPT > 0) THEN
                 ALLOCATE(IWORK_T(II)%NPT_LAY(NPT))
                 IWORK_T(II)%NPT_LAY = 0
                 DO J=1,NPT
                   IDPLY = IWORK_T(II)%PLYID(J)
                   ICSH(II) = ICSH(II) + IDPLY*IDPLY
                   IWORK_T(II)%NPT_LAY(J) = 1
                 ENDDO
              ENDIF 
             ELSEIF(IGTYP == 51) THEN
              ALLOCATE(IWORK_T(II)%NPT_LAY(NPT))
              IWORK_T(II)%NPT_LAY = 0
              IF(IE_DRP > 0 .AND. NPT > 0) THEN
                DO J=1,NPT
                 IP = DRAPE(IE_DRP)%INDX_PLY(J)
                 IF(IP > 0 ) THEN                            
                    DRAPE_PLY => DRAPE(IE_DRP)%DRAPE_PLY(IP)
                    NSLICE = DRAPE_PLY%NSLICE
                    IDPLY = IWORK_T(II)%PLYID(J) 
                    ICSH(II) = ICSH(II) + NSLICE*IDPLY*IDPLY
                    IWORK_T(II)%NPT_LAY(J) = NSLICE
                    IGEO(44,IDPLY) = MAX(IGEO(4,IDPLY),NSLICE)
                 ELSE
                    IDPLY = IWORK_T(II)%PLYID(J)
                    NPT_LAY = IGEO(4,IDPLY) 
                    ICSH(II) = ICSH(II) + NPT_LAY*IDPLY*IDPLY
                    IWORK_T(II)%NPT_LAY(J) = NPT_LAY
                 ENDIF
                ENDDO  
              ELSE
               IF(NPT > 0) THEN
                 DO J=1,NPT
                   IDPLY = IWORK_T(II)%PLYID(J)
                   NPT_LAY  = IGEO(4,IDPLY)  
                   ICSH(II) = ICSH(II) + NPT_LAY*IDPLY*IDPLY
                   IWORK_T(II)%NPT_LAY(J) = NPT_LAY
                 ENDDO
               ENDIF 
              ENDIF 
              ENDIF ! IGTYP   
             ENDDO
!!###########################################################"  
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
          ALLOCATE(IWORKS%IGEO(4*NPT_STACK+2,NS_STACK))
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
                   JJPID =  IWORK_T(IIS)%PLYID(JJ)
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
             IPNPT_LAY = IPMAT_IPLY + NPT
             IPANG = 1
             IPTHK  = IPANG + NPT
             IPPOS  = IPTHK + NPT
             IPDIR  = IPPOS + NPT
             IPTHKLY  = IPDIR + NPT
             IPWEIGHT  = IPTHKLY + NPT
             NUMS= NUMGEO_STACK(PID)
             DO J=1,NPT
              JSTACK = IPTPLY(J)
              IWORKS%IGEO(IPPID      + J,IS)  = STACK_INFO(NUMS)%PID(JSTACK) 
              IWORKS%IGEO(IPMAT      + J,IS)  = STACK_INFO(NUMS)%MID(JSTACK) 
              IWORKS%IGEO(IPMAT_IPLY + J,IS)  = STACK_INFO(NUMS)%MID_IP(JSTACK) 
              IWORKS%IGEO(IPNPT_LAY  + J,IS)  = IWORK_T(II)%NPT_LAY(J) ! npt per laye
              IWORKS%GEO(IPANG       + J,IS)  = STACK_INFO(NUMS)%ANG(JSTACK)
              IWORKS%GEO(IPTHK       + J,IS)  = STACK_INFO(NUMS)%THK(JSTACK)
              IWORKS%GEO(IPPOS       + J,IS)  = STACK_INFO(NUMS)%POS(JSTACK)
              IWORKS%GEO(IPDIR       + J,IS)  = STACK_INFO(NUMS)%DIR(JSTACK)
              IWORKS%GEO(IPTHKLY     + J,IS)  = STACK_INFO(NUMS)%THKLY(JSTACK)
              IWORKS%GEO(IPWEIGHT    + J,IS)  = STACK_INFO(NUMS)%WEIGHT(JSTACK)
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
        ENDDO ! DO IS = 1,NS 
C           
        DEALLOCATE(ICSH,INDX,ITRI)
         DO I=1,NUMELC + NUMELTG
             DEALLOCATE(IWORK_T(I)%PLYID)
         ENDDO
       ENDIF
C 
C   pccommp part
C        
C---       
       NS_STACK0 = NS_STACK
       NPT_STACK0 = NPT_STACK
C       
       IF(IPART_PCOMPP > 0) THEN
!------------------------------------------------
        ALLOCATE(ICSH(NUMELC+NUMELTG))
        IF(NUMELC+NUMELTG > 0) ICSH = 0
!!!------------------------------------------
       !!          
         NSH = 0
C                
         DO I=1,NUMELC
             PID = IXC(6,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 52)THEN
                 NSH = NSH +  1
                 INDX_SH(NSH) = I
                 PID_SH(NSH) = PID
            ENDIF  
         ENDDO 
C
         DO I=1,NUMELTG
             PID = IXTG(5,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 52)THEN
                 NSH = NSH +  1
                 INDX_SH(NSH) = I + NUMELC
                 PID_SH(NSH) = PID
            ENDIF  
         ENDDO  
C #####################################################"
           DO I=1,NSH
             II = INDX_SH(I) 
             NPT = IWORKSH(1,II)
             IE_DRP = DRAPEG%INDX(II)             
             ALLOCATE(IWORK_T(II)%NPT_LAY(NPT))
             IWORK_T(II)%NPT_LAY = 0
             IF(IE_DRP > 0 .AND. NPT > 0) THEN
               DO J=1,NPT
                 IP = DRAPE(IE_DRP)%INDX_PLY(J) 
                 IF(IP > 0 ) THEN                            
                    DRAPE_PLY => DRAPE(IE_DRP)%DRAPE_PLY(IP)
                    NSLICE = DRAPE_PLY%NSLICE
                    IDPLY = IWORK_T(II)%PLYID(J) 
                    ICSH(II) = ICSH(II) + NSLICE*IDPLY*IDPLY
                    IWORK_T(II)%NPT_LAY(J) = NSLICE
                    PLY_INFO(2,IDPLY - NUMSTACK)  =  MAX(PLY_INFO(2,IDPLY - NUMSTACK),NSLICE)
                 ELSE
                    IDPLY = IWORK_T(II)%PLYID(J) 
                    NPT_LAY = IGEO_STACK(4,IDPLY)
                    ICSH(II) = ICSH(II) + NPT_LAY*IDPLY*IDPLY
                    IWORK_T(II)%NPT_LAY(J) = NPT_LAY
                 ENDIF
               ENDDO  
             ELSE
               IF(NPT > 0) THEN
                 DO J=1,NPT
                    IDPLY = IWORK_T(II)%PLYID(J)
                    NPT_LAY = IGEO_STACK(4,IDPLY)
                    ICSH(II) = ICSH(II) + NPT_LAY*IDPLY*IDPLY
                    IWORK_T(II)%NPT_LAY(J) = NPT_LAY
                 ENDDO
               ENDIF 
             ENDIF   
            ENDDO  
!!###########################################################"  
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
             IDS = IWORK_T(II)%IDSTACK
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
                    JJPID =  IWORK_T(IIS)%PLYID(JJ)
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
          IPNPT_LAY = IPMAT_IPLY + NPT
C
          IPANG = 1
          IPTHK  = IPANG + NPT
          IPPOS  = IPTHK + NPT
          IPDIR  = IPPOS + NPT
          IPTHKLY = IPDIR + NPT
          IPWEIGHT =IPTHKLY + NPT
C stack id
           PIDS = IDSTACK(IS)
           NUMS = NUMGEO_STACK(NUMGEO + PIDS)
           DO J=1,NPT
            JS = IPTPLY(J)
            STACK%IGEO(IPPID+J       ,IIS)  = STACK_INFO(NUMS)%PID(JS)
            STACK%IGEO(IPMAT + J     ,IIS)  = STACK_INFO(NUMS)%MID(JS)
            STACK%IGEO(IPMAT_IPLY+J  ,IIS)  = STACK_INFO(NUMS)%MID_IP(JS)
            STACK%IGEO(IPNPT_LAY  + J,IIS)  = IWORK_T(II)%NPT_LAY(J) ! npt per laye 
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
C---
            IPPID = 2
            DO ILAY=1,NPT
                  PIDS =  STACK%IGEO(IPPID + ILAY      ,IIS)
                  NPTT  = IGEO_STACK(4,PIDS)
                  IGEO(4,PID) = MAX(IGEO(4,PID),NPTT)
            ENDDO   
        ENDDO ! DO IS = 1,NS
C---
         DEALLOCATE(ICSH,INDX,ITRI,IDSTACK)
         DO I=1,NUMELC + NUMELTG
             DEALLOCATE(IWORK_T(I)%PLYID)
           ENDDO
           !!DEALLOCATE(IWORK_T)
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
              DO J = 1, 4*NPT_STACK0 + 2
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
       
       DEALLOCATE (INDX_SH,PID_SH,NFIRST ,NLAST,ISUBSTACK,
     .                IPTPLY, WORK ) 
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
