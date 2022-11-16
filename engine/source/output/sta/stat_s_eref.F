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
Chd|  STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        GETCONFIG                     source/output/sta/stat_s_eref.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE STAT_S_EREF(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXS ,
     1                        IXS10,IXS16,IXS20,X   ,DR  ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,
     3                        STAT_INDXS,IPART,SIZP0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTS(*), IPART_STATE(*), STAT_INDXS(*),IPART(LIPART1,*)
      my_real
     .   X(3,*), DR(SDR)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,II,JJ,LEN,NLAY,NPTR,NPTS,NPTT,ISOLNOD0,
     .   ISOLNOD,ISTRAIN,NG, NEL, MLW, ID, IPRT0, IPRT,IE, 
     .   NPG,IPG,IPT,IL,IR,IS,IT,IPID,PID,IOFF,KK(8),NC(20),
     .   NN1,NN,NSROT
      INTEGER PTWA(STAT_NUMELS), PTWA_P0(0:MAX(1,STAT_NUMELS_G))
      my_real
     .   X0(MVSIZ,20), Y0(MVSIZ,20), Z0(MVSIZ,20)
      CHARACTER*100 DELIMIT,LINE
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C----  
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C======================================================================|
      JJ = 0
      IF(STAT_NUMELS==0) GOTO 200

      IE=0
C----- not output all solid element      
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        MLW   =IPARG(1,NG) 
        NEL   =IPARG(2,NG) 
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ISTRAIN = IPARG(44,NG)          
        LFT = 1
        LLT = NEL
!
        DO I=1,8  ! length max of GBUF%G_STRA = 8
          KK(I) = NEL*(I-1)
        ENDDO
!
        IF (ITY == 1) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          IF (JHBE==17.AND.IINT==2) JHBE = 18
          ISOLNOD0 = ISOLNOD
          NSROT = 0
          IF (ISOLNOD0==4 .AND. ISROT==1) THEN
            ISOLNOD=10
            NSROT = 4
          END IF
          GBUF => ELBUF_TAB(NG)%GBUF
          IPRT=IPARTS(LFT+NFT)
          PID = IPART(2,IPRT)
c------          
          IF(ISMSTR==1.OR.ISMSTR>=10) THEN
            CALL GETCONFIG(LFT,LLT,ISOLNOD,ISMSTR,X0,Y0,Z0,
     1                     GBUF%SMSTR,NEL)
          END IF
            DO I=LFT,LLT                                                    
              N  = I + NFT                                                  
              IPRT=IPARTS(N)                                                
              IF(IPART_STATE(IPRT)==0)CYCLE                                 
              WA(JJ+ 1)= IPRT                                               
              WA(JJ+ 2)= IXS(NIXS,N) 
              WA(JJ+ 3)= ISOLNOD                                               
              WA(JJ+ 4)= JHBE                                               
              WA(JJ+ 5)= ISMSTR 
              WA(JJ+ 6)= GBUF%OFF(I)                                                                                                      
              WA(JJ+ 7)= NSROT                                                                                                      
              JJ = JJ + 7
              IF(ISMSTR==1.OR.ISMSTR>=10) THEN
                IF(ISOLNOD == 8)THEN
	             DO J = 1,ISOLNOD
	               NC(J) = IXS(J+1,N)
                 ENDDO
                ELSEIF(ISOLNOD0== 4)THEN
                  NC(1)=IXS(2,N)
                  NC(2)=IXS(4,N)
                  NC(3)=IXS(7,N)
                  NC(4)=IXS(6,N)
                ELSEIF(ISOLNOD == 6)THEN
                  NC(1)=IXS(2,N)
                  NC(2)=IXS(3,N)
                  NC(3)=IXS(4,N)
                  NC(4)=IXS(6,N)
                  NC(5)=IXS(7,N)
                  NC(6)=IXS(8,N)
                ELSEIF(ISOLNOD0== 10)THEN
                  NC(1)=IXS(2,N)
                  NC(2)=IXS(4,N)
                  NC(3)=IXS(7,N)
                  NC(4)=IXS(6,N)
                  NN1 = N - NUMELS8
                  DO J=1,6
                    NC(J+4) = IXS10(J,NN1)
                  ENDDO
                ELSEIF(ISOLNOD == 16)THEN
	              NC(1:8) = IXS(2:9,N)
                  NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
                  DO J=1,8
                   NC(J+8) = IXS16(J,NN1)
                  ENDDO
                ELSEIF(ISOLNOD == 20)THEN
	              NC(1:8) = IXS(2:9,N)
                  NN1 = N - (NUMELS8+NUMELS10)
                  DO J=1,12
                    NC(J+8) = IXS20(J,NN1)
                  ENDDO
                ENDIF
               DO J= 1, ISOLNOD
                JJ = JJ + 1
                WA(JJ)= X0(I,J)                                        
                JJ = JJ + 1
                WA(JJ)= Y0(I,J)                                               
                JJ = JJ + 1
                WA(JJ)= Z0(I,J)                                        
               END DO              
               DO J= 1, NSROT
                NN = 3*(NC(J)-1)
                JJ = JJ + 1
                WA(JJ)= DR(1+NN)                                        
                JJ = JJ + 1
                WA(JJ)= DR(2+NN)                                               
                JJ = JJ + 1
                WA(JJ)= DR(3+NN)                                        
               END DO              
              END IF    ! ISMSTR==1.OR.ISMSTR>=10
              IE=IE+1                                                       
C             pointeur de fin de zone dans WA                                 
              PTWA(IE)=JJ                                                   
            END DO                                                
C                                                                                       
        ENDIF    ! ITY == 1
      ENDDO      ! NG=1,NGROUP 
 200  CONTINUE
c-----------------------------------------------------------
      IF(NSPMD == 1)THEN
C       recopies inutiles pour simplification du code.
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELS
          PTWA_P0(N)=PTWA(N)
        END DO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELS,PTWA_P0,STAT_NUMELS_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF
c-----------------------------------------------------------
      IF(ISPMD == 0.AND.LEN>0) THEN

        IPRT0=0
        DO N=1,STAT_NUMELS_G

C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXS(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
          IOFF  = NINT(WAP0(J + 6))      
          IPRT  = NINT(WAP0(J + 1)) 
          ISMSTR = NINT(WAP0(J + 5)) 
          IF (IOFF >= 1.AND.(ISMSTR==1.OR.ISMSTR>=10)) THEN
            IF(IPRT /= IPRT0)THEN
             IF (IZIPSTRS == 0) THEN
             WRITE(IUGEO,'(A)') DELIMIT
             WRITE(IUGEO,'(A)')'/INIBRI/EREF'
             WRITE(IUGEO,'(A)')
     .      '#------------------------ REPEAT -------------------------'
             WRITE(IUGEO,'(A)')
     .      '#  BRICKID              ISOLNOD    ISOLID    ISMSTR     NSROT' 
             WRITE(IUGEO,'(A/A)')
     .      '# REPEAT K=1,ISOLNOD ',
     .      '#     X,   Y,   Z'
                   WRITE(IUGEO,'(A)')
     .      '#------------------------ REPEAT -------------------------'
              WRITE(IUGEO,'(A)') DELIMIT
             ELSE
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INIBRI/EREF'
                CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .      '#------------------------ REPEAT -------------------------'
               CALL STRS_TXT50(LINE,100)  
               WRITE(LINE,'(A)')
     .      '#  BRICKID              ISOLNOD    ISOLID    ISMSTR     NSROT'
               CALL STRS_TXT50(LINE,100)  
               WRITE(LINE,'(A)')
     .      '# REPEAT K=1,ISOLNOD '
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'#     X,   Y,   Z'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .      '# REPEAT K=1,NSROT '
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'#    RX,  RY,  RZ'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .      '#------------------------ REPEAT -------------------------'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100) 
             END IF
              IPRT0=IPRT
            END IF
            ID      = NINT(WAP0(J + 2))          
            ISOLNOD = NINT(WAP0(J + 3))          
            JHBE    = NINT(WAP0(J + 4))          
            NSROT   = NINT(WAP0(J + 7))          
c
            J = J + 7
c------------------------------------------------
            IF (IZIPSTRS == 0) THEN 
              WRITE(IUGEO,'(I10,10X,4I10)') ID,ISOLNOD,JHBE,ISMSTR,NSROT
            ELSE
              WRITE(LINE,'(I10,10X,4I10)') ID,ISOLNOD,JHBE,ISMSTR,NSROT
              CALL STRS_TXT50(LINE,100)
            ENDIF
            DO IPT = 1, ISOLNOD+NSROT                                 
              IF (IZIPSTRS == 0) THEN                        
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3) 
              ELSE                                           
                CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)    
              ENDIF                                          
              J = J + 3                                     
            ENDDO                  
          ENDIF  !  IF (IOFF == 1)
c---
        ENDDO		    
      ENDIF
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  GETCONFIG                     source/output/sta/stat_s_eref.F
Chd|-- called by -----------
Chd|        STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETCONFIG(IFT,ILT,NPE,ISMSTR,X0, Y0, Z0, SAV,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,NEL,IFT,ILT,ISMSTR
C     REAL
      my_real
     .   X0(MVSIZ,*), Y0(MVSIZ,*), Z0(MVSIZ,*)
      DOUBLE PRECISION 
     .  SAV(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N,N2,N3
C     REAL
C-----------------------------------------------
C
      NPE1=NPE-1
      IF (ISMSTR==1.AND.NPE<10) THEN
        DO N=1,NPE1
         N2 = 3*(N -1) +1
         DO I=IFT,ILT
          X0(I,N)=SAV(I,N2)
          Y0(I,N)=SAV(I,N2+1)
          Z0(I,N)=SAV(I,N2+2)
         ENDDO
        ENDDO
      ELSEIF (NPE<10) THEN
        DO N=1,NPE1
         N2 = N + NPE1
         N3 = N2 + NPE1
         DO I=IFT,ILT
          X0(I,N)=SAV(I,N)
          Y0(I,N)=SAV(I,N2)
          Z0(I,N)=SAV(I,N3)
         ENDDO
        ENDDO
      END IF
C---------diff stockage for quadratic elements
      IF (NPE>=10) THEN
        DO N=1,NPE
          N2 = N + NPE
          N3 = N2 + NPE
          DO I=IFT,ILT
           X0(I,N) =SAV(I,N)
           Y0(I,N) =SAV(I,N2)
           Z0(I,N) =SAV(I,N3)
          ENDDO
        ENDDO
      ELSE      
       DO I=IFT,ILT
        X0(I,NPE)=ZERO
        Y0(I,NPE)=ZERO
        Z0(I,NPE)=ZERO
       ENDDO
      END IF !(NPE==10) THEN
C
      RETURN
      END
