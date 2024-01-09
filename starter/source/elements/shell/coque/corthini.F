Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CORTHINI                      source/elements/shell/coque/corthini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CORTHINI(
     1           JFT     ,JLT     ,NFT     ,NLAY    ,NUMEL   ,
     2           NSIGSH  ,NIX     ,IX      ,IGEO    ,GEO     ,
     3           SKEW    ,SIGSH   ,PTSH    ,PHI1    ,PHI2    ,
     4           VX      ,VY      ,VZ      ,COOR1   ,COOR2   ,
     5           COOR3   ,COOR4   ,IORTHLOC,ISUBSTACK, STACK ,
     6           IREP   ,ELBUF_STR,DRAPE  ,ANGLE   ,X        ,
     7           GEO_STACK,E3X  ,E3Y     ,E3Z     ,
     8           BETAORTH ,X1     ,X2      ,Y1      ,Y2      ,
     9           Z1       ,Z2     ,NEL     ,G_ADD_NODE,ADD_NODE,
     A           NPT_ALL , IDRAPE ,INDX)
c---     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NLAY,IPT,ID,NIX,NUMEL,NSIGSH,
     .        ISUBSTACK,IREP,NPT_ALL,IDRAPE
      INTEGER IX(NIX,*),IGEO(NPROPGI,*),PTSH(*),IORTHLOC(*)
      INTEGER, INTENT(IN) :: NEL,G_ADD_NODE,ADD_NODE(G_ADD_NODE*NEL) 
      INTEGER, DIMENSION(*)  :: INDX  
      my_real
     . GEO(NPROPG,*),SKEW(LSKEW,*),SIGSH(NSIGSH,*),VX(*),VY(*),VZ(*),
     . PHI1(NPT_ALL,*),PHI2(NPT_ALL,*),COOR1(NPT_ALL,MVSIZ),COOR2(NPT_ALL,MVSIZ),
     . COOR3(NPT_ALL,MVSIZ),COOR4(NPT_ALL,MVSIZ),
     . ANGLE(*),GEO_STACK(NPROPG,*),X(3,*),BETAORTH(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: E3X,E3Y,E3Z,X1,X2,Y1,Y2,Z1,Z2
C------------------------------------------------------     
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY):: STACK
      TYPE (DRAPE_)  , DIMENSION(*), TARGET :: DRAPE 
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,JJ,N,NPT,NPTI,IGTYP,IPID,PID,ISK,IPANG,IPPHI,
     . IIGEO,IADR,IPTHK,IPPOS,IPDIR,IMAT_LY,ILAW_LY,IPPID,IPMAT,ILAY,
     . DEF_ORTH(MVSIZ),N1,N2,IRP,POS,NOD,IL,IT,NSLICE,IPT_ALL,NPTT,
     . IE, IP,IPID_PLY
      my_real V(MVSIZ),E11,E12,E13,NE1,VX0,VY0,VZ0
      CHARACTER*nchartitle, TITR1
      
      TYPE (DRAPE_PLY_)  , POINTER :: DRAPE_PLY 
C=======================================================================
      PID = IX(NIX-1,1)
      IGTYP = IGEO(11,PID)
      IRP = IGEO(14,PID)
      DEF_ORTH(1:MVSIZ) = NLAY
      IPDIR = 0
C
      IF (IGTYP == 1) THEN
C       non orthotropic property
        RETURN
      ELSE
        IPANG = 200      
        IPPHI = 500
        IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
          IPANG  =  1
          IPTHK  =  IPANG + NLAY
          IPPOS  =  IPTHK + NLAY
          IPDIR = IPPOS + NLAY
        ENDIF
        SELECT CASE (IRP)
          CASE (0) ! 
           ISK = IGEO(2,PID)
           DO I=JFT,JLT
            IF (ISK == 0) THEN
              VX(I) = GEO(7,PID)
              VY(I) = GEO(8,PID)
              VZ(I) = GEO(9,PID)
            ELSE
              VX(I) = SKEW(1,ISK)
              VY(I) = SKEW(2,ISK)
              VZ(I) = SKEW(3,ISK)
            ENDIF
           ENDDO
          CASE (20)  ! N1---> N2 (nodes)
            DO I=JFT,JLT
              N1=IX(2,I)
              N2=IX(3,I)
              VX(I) = X(1,N2)-X(1,N1)
              VY(I) = X(2,N2)-X(2,N1)
              VZ(I) = X(3,N2)-X(3,N1)
            ENDDO
          CASE (22)   ! Iskew
            ISK = IGEO(2,PID)  ! 
            DO I=JFT,JLT
              VX(I) = SKEW(1,ISK)
              VY(I) = SKEW(2,ISK)
              VZ(I) = SKEW(3,ISK)
            ENDDO
          CASE (23) !  Proj on the element  V x normal_eleemt
              VX0 = GEO(7,PID)
              VY0 = GEO(8,PID)
              VZ0 = GEO(9,PID)
            DO I=JFT,JLT
              VX(I) = E3Y(I)*VZ0 - E3Z(I)*VY0
              VY(I) = E3Z(I)*VX0 - E3X(I)*VZ0
              VZ(I) = E3X(I)*VY0 - E3Y(I)*VX0
            ENDDO
          CASE (24)
C--         seatbelt elements - dir1 defined by N1 and ADD_NODE (can be either N2 or N4)
            DO I=JFT,JLT 
              N1=IX(2,I)
              NOD=ADD_NODE(I)
              VX(I) = X(1,NOD)-X(1,N1)
              VY(I) = X(2,NOD)-X(2,N1)
              VZ(I) = X(3,NOD)-X(3,N1)
            ENDDO    
          CASE (25)
C--         y' of cylintrical sys (using only x1,y1,z1)
            ISK = IGEO(2,PID)  ! 
            DO I=JFT,JLT 
              E11 = X1(I)-SKEW(10,ISK)
              E12 = Y1(I)-SKEW(11,ISK)
              E13 = Z1(I)-SKEW(12,ISK)
              VX(I) = SKEW(8,ISK)*E13 - SKEW(9,ISK)*E12
              VY(I) = SKEW(9,ISK)*E11 - SKEW(7,ISK)*E13
              VZ(I) = SKEW(7,ISK)*E12 - SKEW(8,ISK)*E11
            ENDDO    
         END SELECT
C---    read property data
        IF (IGTYP == 9) THEN
          DO I=JFT,JLT
            PHI1(1,I)= GEO(10,PID)
          ENDDO
        ELSEIF (IGTYP == 10) THEN
          DO I=JFT,JLT
            DO J=1,NLAY
              PHI1(J,I)= GEO(IPANG+J,PID)
            ENDDO
          ENDDO
        ELSEIF (IGTYP == 11) THEN
          DO I=JFT,JLT
            DO J=1,NLAY
              PHI1(J,I)= GEO(IPANG+J,PID)
            ENDDO
          ENDDO
         ELSEIF (IGTYP == 17 .AND. IRP /= 24) THEN !
          IF(IDRAPE > 0) THEN
           DO I=JFT,JLT
            IPANG   = 1
            IE = INDX(NFT + I)
            IF(IE == 0) THEN
               DO J=1,NLAY 
                IPID_PLY = STACK%IGEO(2 + J,ISUBSTACK)
                IF(IPID_PLY > 0) THEN 
                  PHI1(J,I)   = ANGLE(I)  +   GEO(2,IPID_PLY) + STACK%GEO(IPANG+J,ISUBSTACK)    ! + stack_angle 
                  IF (IREP >= 2) PHI2(J,I)= STACK%GEO(IPDIR+J,ISUBSTACK) 
                  DEF_ORTH(I) = DEF_ORTH(I) - 1  
                ENDIF                                       
              ENDDO
           ELSE ! ie > 0
               DO J=1,NLAY 
                 IPID_PLY = STACK%IGEO(2+J,ISUBSTACK)
                 IF(IPID_PLY > 0) THEN 
                   PHI1(J,I)   =   ANGLE(I)  + GEO(2,IPID_PLY)  +   STACK%GEO(IPANG+J,ISUBSTACK)  ! + stack_angle                 
                   IF (IREP >= 2) PHI2(J,I)= STACK%GEO(IPDIR+J,ISUBSTACK) 
                   DEF_ORTH(I) =   DEF_ORTH(I) - 1 
                   IP = DRAPE(IE)%INDX_PLY(J)  
                   IF(IP > 0) THEN                                                                            
                      DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                      PHI1(J,I) = PHI1(J,I) + DRAPE_PLY%RDRAPE(1,2)
                   ENDIF  
                  ENDIF                                     
               ENDDO
           ENDIF                                                                                 
          ENDDO  
         ELSE ! idrape== 0
           DO I=JFT,JLT
            IPANG   = 1
            DO J=1,NLAY 
               IPID_PLY = STACK%IGEO(2+J,ISUBSTACK)
               IF(IPID_PLY > 0) THEN
                 PHI1(J,I)   = ANGLE(I)  +    GEO(2,IPID_PLY) + STACK%GEO(IPANG+J,ISUBSTACK)
                 DEF_ORTH(I) = DEF_ORTH(I) - 1
                 IF (IREP >= 2) PHI2(J,I)= STACK%GEO(IPDIR+J,ISUBSTACK)
               ENDIF  
            ENDDO
          ENDDO  
         ENDIF  ! idrape 
        ELSEIF(IGTYP == 51 .AND. IRP /= 24 ) THEN  !
         IF(IDRAPE > 0) THEN
           DO I=JFT,JLT
            IPANG   = 1
            IPT_ALL = 0
            IE = INDX(NFT + I)
            IF(IE > 0) THEN
                DO IL=1,NLAY
                   NPTT = ELBUF_STR%BUFLY(IL)%NPTT
                   IP = DRAPE(IE)%INDX_PLY(IL)
                   IPID_PLY = STACK%IGEO(2 + IL,ISUBSTACK)
                   IF(IPID_PLY > 0) THEN
                      IF(IP > 0) THEN
                         DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                         NSLICE = DRAPE_PLY%NSLICE ! NPTT
                         DEF_ORTH(I) = DEF_ORTH(I) - 1  !
                         IF(IREP >= 2) THEN                                     
                            DO IT = 1,NPTT                                                                    
                               IPT =  IPT_ALL + IT                                                            
                               PHI1(IPT,I)   = ANGLE(I) + GEO(2,IPID_PLY) + STACK%GEO(IPANG+IL,ISUBSTACK)     
     .                                                  + DRAPE_PLY%RDRAPE(IT,2) 
                               PHI2(IPT,I)   = STACK%GEO(IPDIR + IL,ISUBSTACK)                                
                            ENDDO ! NPTT 
                         ELSE                                                           
                            DO IT = 1,NPTT                                                                     
                              IPT =  IPT_ALL + IT                                    
                              PHI1(IPT,I)   = ANGLE(I) + GEO(2,IPID_PLY) + STACK%GEO(IPANG+IL,ISUBSTACK)     
     .                                                 + DRAPE_PLY%RDRAPE(IT,2)                                    
                            ENDDO ! NPTT                                                                       
                         ENDIF ! IREP                                                                               
                      ELSE !IP == 0
                            DEF_ORTH(I) = DEF_ORTH(I) - 1
                            IF(IREP >= 2) THEN
                              DO IT = 1,NPTT                                                             
                                  IPT =  IPT_ALL + IT                                                     
                                  PHI1(IPT,I)   = ANGLE(I) + GEO(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK) 
                                  PHI2(IPT,I)   = STACK%GEO(IPDIR+IL,ISUBSTACK)
                              ENDDO ! NPTT 
                             ELSE 
                              DO IT = 1,NPTT
                                 IPT =  IPT_ALL + IT 
                                 PHI1(IPT,I)   = ANGLE(I) + GEO(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK) 
                              ENDDO ! NPTT 
                            ENDIF
                       ENDIF ! IP
                     ENDIF ! IPID_PLY > 0  
                   IPT_ALL = IPT_ALL + NPTT
                ENDDO !  NLAY
             ELSE !IE == 0
                DO IL=1,NLAY
                   NPTT = ELBUF_STR%BUFLY(IL)%NPTT
                   IPID_PLY = STACK%IGEO(2 + IL,ISUBSTACK)
                   IF(IPID_PLY > 0) THEN
                          DEF_ORTH(I) = DEF_ORTH(I) - 1
                          IF(IREP >= 2) THEN
                            DO IT = 1,NPTT                                                             
                               IPT =  IPT_ALL + IT                                                     
                               PHI1(IPT,I)   = ANGLE(I) + GEO(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK)
                               PHI2(IPT,I)   = STACK%GEO(IPDIR+IL,ISUBSTACK)
                            ENDDO ! NPTT
                          ELSE
                            DO IT = 1,NPTT
                               IPT =  IPT_ALL + IT
                               PHI1(IPT,I)   = ANGLE(I) + GEO(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK)
                            ENDDO ! NPTT 
                          ENDIF                        
                   ENDIF  
                   IPT_ALL = IPT_ALL + NPTT
                 ENDDO !  NLAY     
             ENDIF    !IE
          ENDDO ! JFT:JLT 
         ELSE ! IDRAPE = 0
           DO I=JFT,JLT
             IPANG   = 1
             DO IL=1,NLAY
                IPID_PLY = STACK%IGEO(2 + IL,ISUBSTACK)
                IF(IPID_PLY > 0 ) THEN 
                  PHI1(IL,I)   = ANGLE(I) + GEO(2,IPID_PLY)  + STACK%GEO(IPANG + IL,ISUBSTACK)
                  DEF_ORTH(I)  = DEF_ORTH(I) - 1
                ENDIF      
                IF (IREP >= 2) PHI2(IL,I)= STACK%GEO(IPDIR + IL,ISUBSTACK)
             ENDDO
           ENDDO  
         ENDIF   ! idrape
        ELSEIF(IGTYP == 52 .AND. IRP /= 24 ) THEN
         IF(IDRAPE > 0) THEN
           DO I=JFT,JLT
            IPANG   = 1
            IPT_ALL = 0
            IE = INDX(NFT + I)
            IF(IE > 0) THEN
                DO IL=1,NLAY
                   NPTT = ELBUF_STR%BUFLY(IL)%NPTT
                   IP = DRAPE(IE)%INDX_PLY(IL)
                   IPID_PLY = STACK%IGEO(2+IL,ISUBSTACK)
                   IF( IPID_PLY > 0) THEN
                     IF(IP > 0) THEN       ! 
                         DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                         NSLICE = DRAPE_PLY%NSLICE ! NPTT
                         DEF_ORTH(I) = DEF_ORTH(I) - 1
                         IF(IREP >=  2) THEN
                           DO IT = 1,NPTT                                                             
                               IPT =  IPT_ALL + IT                                                     
                               PHI1(IPT,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY) 
     .                                                  + STACK%GEO(IPANG + IL,ISUBSTACK) +   DRAPE_PLY%RDRAPE(IT,2) 
                               PHI2(IPT,I)= STACK%GEO(IPDIR+IL,ISUBSTACK)
                           ENDDO ! NPTT 
                         ELSE
                            DO IT = 1,NPTT                                                             
                               IPT =  IPT_ALL + IT                                                     
                               PHI1(IPT,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY) 
     .                                                  + STACK%GEO(IPANG + IL,ISUBSTACK) +   DRAPE_PLY%RDRAPE(IT,2) 
                           ENDDO ! NPTT
                         ENDIF
                     ELSE !IP == 0
                          DEF_ORTH(I) = DEF_ORTH(I) - 1
                          IF(IREP >= 2) THEN
                            DO IT = 1,NPTT                                                           
                               IPT =  IPT_ALL + IT                                                   
                               PHI1(IPT,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK) 
                               PHI2(IPT,I)   = STACK%GEO(IPDIR+IL,ISUBSTACK)
                            ENDDO ! NPTT  
                           ELSE
                             DO IT = 1,NPTT
                               IPT =  IPT_ALL + IT  
                               PHI1(IPT,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK) 
                             ENDDO ! NPTT 
                           ENDIF ! IREP
                      ENDIF    !IP       
                   ENDIF ! IPID_PLY
                   IPT_ALL = IPT_ALL + NPTT
                 ENDDO !  NLAY
             ELSE !IE == 0
                DO IL=1,NLAY
                    NPTT = ELBUF_STR%BUFLY(IL)%NPTT
                    IPID_PLY = STACK%IGEO(2+IL,ISUBSTACK)
                    IF(IPID_PLY > 0) THEN 
                        DEF_ORTH(I) = DEF_ORTH(I) - 1
                        IF(IREP >= 2)THEN
                          DO IT = 1,NPTT                                                             
                             IPT =  IPT_ALL + IT                                                     
                             PHI1(IPT,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK) 
                             PHI2(IPT,I)= STACK%GEO(IPDIR+IL,ISUBSTACK)
                          ENDDO ! NPTT                                                          
                        ELSE
                          DO IT = 1,NPTT                                                             
                             IPT =  IPT_ALL + IT                                                     
                             PHI1(IPT,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY) + STACK%GEO(IPANG + IL,ISUBSTACK)
                          ENDDO ! NPT
                        ENDIF ! IREP
                    ENDIF  
                   IPT_ALL = IPT_ALL + NPTT
                 ENDDO !  NLAY     
             ENDIF    !IE
          ENDDO ! JFT:JLT 
         ELSE ! IDRAPE = 0
           DO I=JFT,JLT
             IPANG   = 1
             DO IL=1,NLAY
               IPID_PLY = STACK%IGEO(2+IL,ISUBSTACK)
               IF(IPID_PLY > 0) THEN
                    DEF_ORTH(I) = DEF_ORTH(I) - 1                                                    
                    PHI1(IL,I)   = ANGLE(I) + GEO_STACK(2,IPID_PLY)  + STACK%GEO(IPANG + IL,ISUBSTACK)   
                    IF(IREP >= 2)  PHI2(IL,I)= STACK%GEO(IPDIR+IL,ISUBSTACK)                                
               ENDIF ! IPID_PLY 
             ENDDO
           ENDDO  
         ENDIF   ! idrape     
        ELSEIF (IGTYP == 16) THEN
          DO I=JFT,JLT
            DO J=1,NLAY
              PHI1(J,I)= GEO(IPANG+J,PID)
              PHI2(J,I)= GEO(IPPHI+J,PID)
            ENDDO
          ENDDO
        ENDIF
C---    Overwrite with optional element data
        IF (IORTSHEL == 1) THEN
          DO I=JFT,JLT
            IF (ABS(ISIGI) /= 3 .AND. ABS(ISIGI)/=4 .AND. ABS(ISIGI)/=5) THEN
              !!II = I + NFT
              ID = IX(NIX,I)
              II = PTSH(I + NFT)
              IF(II == 0) GOTO 100
              N  = NINT(SIGSH(1,II))
              IF (N == ID) THEN
                CONTINUE
              ELSE
                DO J = 1,NUMEL
                  II = J
                  N  = NINT(SIGSH(1,II))
                  IF (N == ID) GOTO 60
                 IF (N == 0) GOTO 100
                ENDDO
                GOTO 100
 60             CONTINUE
              ENDIF
            ELSE
              JJ=NFT+I
              N =IX(NIX,I)
              II=PTSH(JJ)
              IF (II == 0) GOTO 100
            END IF
            IF(SIGSH(NVSHELL + NUSHELL + 5,II) == ZERO) CYCLE 
C
            NPTI = NINT(SIGSH(NVSHELL + NUSHELL + 4,II))
            IF(IGTYP == 9) NPTI = 1
            IF (NLAY /= NPTI) THEN
              IPID   = IX(NIX-1,I)
              PID    = IGEO(1,IPID)
              CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
              IF (NPTI == 0) THEN
                CALL ANCMSG(MSGID=355,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=N,
     .                      I2=PID,
     .                      C1=TITR1)
              ELSE
                CALL ANCMSG(MSGID=26,
     .                      ANMODE=ANINFO,
     .                      MSGTYPE=MSGERROR,
     .                      I2=N,
     .                      I1=PID,
     .                      C1=TITR1)
              ENDIF
            ENDIF
C
            IPT   = NVSHELL + NUSHELL
            VX(I) = SIGSH(IPT+1,II)
            VY(I) = SIGSH(IPT+2,II)
            VZ(I) = SIGSH(IPT+3,II)
            IPT = IPT + 5
            IF ( IGTYP == 9) THEN
                PHI1(1,I) = SIGSH(IPT+1,II)
                PHI2(1,I) = SIGSH(IPT+2,II)
                IPT = IPT + 2
            ELSE
              DO J=1,NPTI
                PHI1(J,I) = SIGSH(IPT+1,II)
                PHI2(J,I) = SIGSH(IPT+2,II)
                IPT = IPT + 2
              ENDDO
            ENDIF
 100        CONTINUE
          ENDDO
        ENDIF
C
C---    Overwrite with optional element data__ ORTH_LOC
        IF (IORTSHEL == 2) THEN
          DO I=JFT,JLT
            IE = I + NFT
            IF (ABS(ISIGI) /= 3 .AND. ABS(ISIGI) /= 4 .AND. ABS(ISIGI) /= 5) THEN
              ID = IX(NIX,I)
              II = PTSH(IE)
              IF(II == 0) GOTO 110
              N  = NINT(SIGSH(1,II))
              IF (N == ID) THEN
                CONTINUE
              ELSE
                DO J = 1,NUMEL
                  II = J
                  N  = NINT(SIGSH(1,II))
                  IF (N == ID) GOTO 70
                 IF (N == 0) GOTO 110
                ENDDO
                GOTO 110
 70             CONTINUE
              ENDIF
            ELSE
              JJ=NFT+I
              N =IX(NIX,I)
              II=PTSH(JJ)
              IF (II == 0) GOTO 110
            END IF
            IF(SIGSH(NVSHELL + NUSHELL + 5,II) == ZERO) CYCLE 
            NPTI = NINT(SIGSH(NVSHELL + NUSHELL + 4,II))
C
            NPT  = NINT(GEO(6,IX(NIX-1,I)))
            IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP==52)) THEN
              NPT = NPT_ALL
            ELSEIF (IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT = NLAY
            ENDIF 
            IF (NPT /= NPTI) THEN
              IPID   = IX(NIX-1,I)
              PID    = IGEO(1,IPID)
              CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
              IF (NPTI == 0) THEN
                 CALL ANCMSG(MSGID=355,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=N,
     .                       I2=PID,
     .                       C1=TITR1)
              ELSE
                CALL ANCMSG(MSGID=26,
     .                      ANMODE=ANINFO,
     .                      MSGTYPE=MSGERROR,
     .                      I2=N,
     .                      I1=PID,
     .                      C1=TITR1)
              ENDIF
            ENDIF
C
            IPT   = NVSHELL + NUSHELL + 5
            IF (IGTYP == 9) THEN
              COOR1(1,I) = SIGSH(IPT+1,II)
              COOR2(1,I) = SIGSH(IPT+2,II)
              IPT = IPT + 2
              IORTHLOC(I) = 1
            ELSEIF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR.
     .              IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
              DO J=1,NPTI
                ILAW_LY = ELBUF_STR%BUFLY(J)%ILAW
                IF (IGTYP == 16 .OR.(IGTYP == 51 .AND. ILAW_LY == 58) 
     .                          .OR.(IGTYP == 52 .AND. ILAW_LY == 58) ) THEN
            COOR1(J,I) = SIGSH(IPT+1,II)
            COOR2(J,I) = SIGSH(IPT+2,II)
            COOR3(J,I) = SIGSH(IPT+3,II)
            COOR4(J,I) = SIGSH(IPT+4,II)
            IPT = IPT + 4
                ELSE
            COOR1(J,I) = SIGSH(IPT+1,II)
            COOR2(J,I) = SIGSH(IPT+2,II)
            IPT = IPT + 2
                ENDIF
              ENDDO
              IORTHLOC(I) = 1
            ENDIF
 110        CONTINUE
          ENDDO
        ENDIF
C
C---    Check projection
        DO I=JFT,JLT
          V(I) =VX(I)*E3X(I)+VY(I)*E3Y(I)+VZ(I)*E3Z(I)
          VX(I)=VX(I)-V(I)*E3X(I)
          VY(I)=VY(I)-V(I)*E3Y(I)
          VZ(I)=VZ(I)-V(I)*E3Z(I)
          V(I) =SQRT(VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I))
        ENDDO
C
        DO I=JFT,JLT
          IF (V(I) < EM3 .AND. IORTHLOC(I) == 0 .AND.
     .                                   DEF_ORTH(I) /=  0)THEN
            PID = IX(NIX-1,I)
            V(I)= MAX(V(I),EM20)
            CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,PID),LTITR)
            CALL ANCMSG(MSGID=197,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IGEO(1,PID),
     .                  C1=TITR1,
     .                  I2=IX(NIX,I))
          ENDIF
          VX(I)=VX(I)/V(I)
          VY(I)=VY(I)/V(I)
          VZ(I)=VZ(I)/V(I)
        ENDDO
C
C----Beta angle computation(dyna input format)
C
        DO I=JFT,JLT
           
           E11= X2(I)-X1(I)
           E12= Y2(I)-Y1(I)
           E13= Z2(I)-Z1(I)
           NE1 = SQRT(E11*E11+E12*E12+E13*E13)

           BETAORTH(I) = (VX(I)*E11 + VY(I)*E12 +VZ(I)*E13 )/MAX(NE1,EM20)
        ENDDO 
C----
      ENDIF
C-----------
      RETURN
      END

