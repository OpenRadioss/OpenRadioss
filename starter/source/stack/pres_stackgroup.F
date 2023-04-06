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
Chd|  PRE_STACKGROUP                source/stack/pres_stackgroup.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE PRE_STACKGROUP( 
     .                     IGRSH3N    ,IGRSH4N  ,IXC        ,IXTG ,
     .                     IGEO       ,GEO      ,IGEO_STACK ,IWORKSH    ,
     .                     IWORK_T)
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
     .        IGEO_STACK(NPROPGI,NUMSTACK + NUMPLY)
      my_real
     .       GEO(NPROPG,NUMGEO)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE(DRAPE_WORK_) , DIMENSION(NUMELC + NUMELTG) , TARGET :: IWORK_T
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,NSTACK,NPLY,IGTYP,ID,JD,IDPLY,NEL,
     .        IAD,ITY,IDSHEL,PID,IS,IDS,NSH,MODE,NS,JJ,NGEO_STACK,
     .        IGRTYP,N1,IIGEO,NSS,IPPOS,NPT,IIS,NP,
     .        JJPID,JSTACK,JPID,ITG,IPMAT_IPLY,ISH3N,J4N,J3N,IPOS,
     .        MAT_LY,NLAY,NPTT,IPIDL,IT,ILAY,IPTHK_NPTT,IPPOS_NPTT,
     .        IINT,IPID_LY,IPDIR ,NS_STACK0 ,NPT_STACK0,IS0,JS,PIDS,IP,
     .        II1,II2,JJ1,JJ2
     
      INTEGER , DIMENSION(NUMGEO+NUMPLY)  :: IPIDPLY,IDGR4N,IDGR3N
     
      INTEGER :: NBFI,IPPID, NGL,IPID_1,NUMS,IPWEIGHT,IPTHKLY,NSHQ4,NSHT3
     
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI
      INTEGER, DIMENSION (:) ,ALLOCATABLE ::ICSH,INDX,ICSH_STACK
      CHARACTER*nchartitle,
     .   TITR,TITR1
C----------------------------f-------------------    
C=======================================================================
C    For Shell
C-----------------------------------------------   
       IF(IPART_STACK > 0) THEN
         NPLY = 0
         NSTACK = 0
C
         IPIDPLY   = 0
         IDGR4N    = 0
         IDGR3N    = 0
         DO I = 1, NUMGEO
!!           ISUBSTACK(I)= 0
           IGTYP=IGEO(11,I)
           NSTACK = IGEO(42,I)  ! number of stack where ply is attached
           IF (IGTYP == 19 .AND. NSTACK > 0) THEN
               NPLY = NPLY+1
               IPIDPLY(NPLY) = I
               IDGR4N(NPLY)  = IGEO(40,I) ! groupe shell 4N id
               IDGR3N(NPLY)  = IGEO(41,I) ! groupe shell 3N id
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
C tag o  f ply element 
         NSHQ4 = 0
         DO I=1,NUMELC
             PID = IXC(6,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 17 .OR. IGTYP == 51)THEN
                 NSHQ4 = NSHQ4 +  1
            ENDIF  
         ENDDO 
C 
         NSHT3 = 0
         DO I=1,NUMELTG
             PID = IXTG(5,I)
             IGTYP = IGEO(11,PID)
             IF(IGTYP == 17 .OR. IGTYP == 51)THEN
                 NSHT3 = NSHT3 +  1
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
           DO I=1,NUMELC + NUMELTG
             NPT = IWORKSH(1,I)
             ALLOCATE(IWORK_T(I)%PLYID(NPT))
             IWORK_T(I)%PLYID = 0
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
                                 IWORK_T(IDSHEL)%PLYID(NPT) = IDPLY
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
                                 IWORK_T(IDSHEL)%PLYID(NPT) = IDPLY
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
                                IWORK_T(II)%PLYID(NPT) = IDPLY
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
                                 IWORK_T(ITG)%PLYID(NPT) = IDPLY
                                GOTO 404
                             ENDIF 
                          ENDDO
                      ENDIF   
 404                CONTINUE 
              ENDIF
C
         ENDDO  ! iply  
       ENDIF
C 
C   pccommp part
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
!
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
                     IF(IWORK_T(IDSHEL)%IDSTACK == 0) THEN 
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                          IWORK_T(IDSHEL)%IDSTACK = IDS
                          ICSH_STACK(IDSHEL) = IDS
                      ELSEIF(ICSH_STACK(IDSHEL) == IDS) THEN
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                      ELSE
C  message d'erreur      
                        IPID_1=IGEO_STACK(1,IWORK_T(IDSHEL)%IDSTACK)
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
                      IF(IWORK_T(IDSHEL)%IDSTACK == 0) THEN  
                          IWORKSH(1,IDSHEL)      =   IWORKSH(1,IDSHEL ) + 1
                          IWORK_T(IDSHEL)%IDSTACK=   IDS
                      ELSEIF(IWORK_T(IDSHEL)%IDSTACK == IDS) THEN
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
!!!------------------------------------------------
           DO I=1,NUMELC + NUMELTG
             NPT = IWORKSH(1,I)
             ALLOCATE(IWORK_T(I)%PLYID(NPT) )
             IWORK_T(I)%PLYID = 0
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
                     IF(IWORK_T(IDSHEL)%IDSTACK == 0) THEN 
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                          NPT = IWORKSH (1,IDSHEL)
                          IWORK_T(IDSHEL)%PLYID(NPT) = IDPLY
                          IWORK_T(IDSHEL)%IDSTACK = IDS
                      ELSEIF(IWORK_T(IDSHEL)%IDSTACK == IDS) THEN
                          IWORKSH (1,IDSHEL) = IWORKSH(1,IDSHEL) + 1
                          NPT = IWORKSH (1,IDSHEL)
                          IWORK_T(IDSHEL)%PLYID(NPT) = IDPLY
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
                      IF(IWORK_T(IDSHEL)%IDSTACK == 0) THEN  
                          IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                          NPT = IWORKSH(1,IDSHEL)
                          IWORK_T(IDSHEL)%PLYID(NPT) = IDPLY
                          IWORK_T(IDSHEL)%IDSTACK= IDS
                      ELSEIF(ICSH_STACK(IDSHEL) == IDS) THEN
                           IWORKSH(1,IDSHEL) =   IWORKSH(1,IDSHEL ) + 1
                           NPT = IWORKSH(1,IDSHEL)
                           IWORK_T(IDSHEL)%PLYID(NPT) = IDPLY
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
       ENDIF    
C--------
     
      RETURN
      END
