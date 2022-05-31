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
Chd|  STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        GET_Q4LSYS                    source/output/sta/sta_c_get_q4lsys.F
Chd|        GET_T3LSYS                    source/output/sta/sta_c_get_t3lsys.F
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        ORTH2LOC                      source/output/sta/stat_c_strsfg.F
Chd|        SHELL2G                       source/output/sta/stat_c_strafg.F
Chd|        SHEML2G                       source/output/sta/stat_c_strsfg.F
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE STAT_C_STRSFG(ELBUF_TAB,
     1                        X    ,IPARG ,IPM ,IGEO ,IXC ,
     2                        IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                        IPART_STATE,STAT_INDXC,STAT_INDXTG,THKE,SIZP0,
     4                        GEO   ,STACK,DRAPE_Q4,DRAPE_T3)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .        STAT_INDXC(*), STAT_INDXTG(*)
      my_real   
     .   THKE(*),X(3,*),GEO(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_Q4, DRAPE_T3
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,NPT,
     .   LLT,ITY,MLW,IH,IHBE, ID, IPRT0, IPRT,IR,IS,IT,J1,J2,
     .   NPG,IPG,MPT,IPT,NPTR,NPTS,NPTT,NLAY,L_PLA,ITHK,NF3,
     .   IGTYP,NPT_ALL,IL,KK(12),NF1,IREL,IBID0,MAT_1,PID_1,ILAY
      INTEGER PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .        PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
      double precision   
     .  THK, EM, EB, H1, H2, H3
      my_real   
     .   PG,MPG,QPG(2,4),THKQ,
     .   SK(2),ST(2),MK(2),MT(2),SHK(2),SHT(2),ZZ
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      INTEGER LAYNPT_MAX,LAY_MAX,ISUBSTACK,IPT_ALL,JDIR,L_DIRA,L_DIRB,IREP
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER MATLY(MVSIZ*LAY_MAX),ILAW
      my_real, 
     .   DIMENSION(:),POINTER  :: DIR_A,DIR_B    
      my_real 
     .   QT(9,MVSIZ),TENS(6),ZH,THKP ,THK0(MVSIZ)   
      my_real
     .   THKLY(MVSIZ*LAY_MAX*LAYNPT_MAX),POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX),
     .   THK_LY(MVSIZ,LAY_MAX*LAYNPT_MAX)
      my_real, ALLOCATABLE, DIMENSION(:) , TARGET :: DIRA,DIRB
C-----------------------------------------------
      PARAMETER (PG = .577350269189626)
      PARAMETER (MPG=-.577350269189626)
      DATA QPG/MPG,MPG,PG,MPG,PG,PG,MPG,PG/
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELC==0) GOTO 200
C
      IE=0
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 3) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW  = IPARG(1,NG)
          NEL  = IPARG(2,NG)
          NFT  = IPARG(3,NG)
          MPT  = IPARG(6,NG)
          IHBE = IPARG(23,NG)
          ITHK = IPARG(28,NG)
          IGTYP= IPARG(38,NG)
          IREP = IPARG(35,NG)
          ISUBSTACK=IPARG(71,NG)
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          IF (IHBE == 23) NPG=4
          LFT=1
          LLT=NEL
         NF1 = NFT+1
         IF (IHBE>10.OR.IGTYP==16.OR.ISHFRAM ==0) THEN
           IREL=0
         ELSEIF (ISHFRAM ==1) THEN
           IREL=2
         ELSE
           IREL=1
         END IF
!
          DO I=1,12  ! lenght max of GBUF%G_HOURG = 12
            KK(I) = NEL*(I-1)
          ENDDO
!
         IBID0 = 0
         MAT_1 = IXC(1,NF1)
         PID_1 = IXC(6,NF1)
         IF (ITHK >0 ) THEN
          THK0(LFT:LLT) = GBUF%THK(LFT:LLT)
         ELSE
          THK0(LFT:LLT) = THKE(LFT+NFT:LLT+NFT)
         END IF
         CALL LAYINI(ELBUF_TAB(NG),LFT      ,LLT      ,GEO      ,IGEO    ,
     .              MAT_1    ,PID_1    ,THKLY    ,MATLY    ,POSLY    , 
     .              IGTYP    ,IBID0    ,IBID0    ,NLAY     ,MPT      ,   
     .              ISUBSTACK,STACK    ,DRAPE_Q4 ,NFT      ,THK0     ,
     .              NEL      ,THK_LY   )
         L_DIRA = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRA
         L_DIRB = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRB
         ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
         ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
         DIRA=ZERO
         DIRB=ZERO
         IF (L_DIRA == 0) THEN
           CONTINUE
         ELSEIF (IREP == 0) THEN
           DO J=1,NLAY
             J1 = 1+(J-1)*L_DIRA*NEL
             J2 = J*L_DIRA*NEL
             DIRA(J1:J2) = ELBUF_TAB(NG)%BUFLY(J)%DIRA(1:NEL*L_DIRA)
           ENDDO
         ENDIF
         DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
         DIR_B => DIRB(1:NLAY*NEL*L_DIRB)
         CALL GET_Q4LSYS(LFT ,LLT ,IXC(1,NF1),X ,GBUF%OFF,
     .                  IREL ,QT  ,NLAY   ,IREP ,NEL    ,
     .                  DIR_A ,DIR_B,ELBUF_TAB(NG))
C
C pre counting of all NPTT (especialy for PID_51)
C
          NPT_ALL = 0
          DO IL=1,NLAY
            NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
          ENDDO
          MPT  = MAX(1,NPT_ALL)
          IF (IPARG(6,NG) == 0) MPT=0
C
c------- loop over 4 node shell elements
C
          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTC(N)
            IF (IPART_STATE(IPRT)==0) CYCLE
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%OFF(I)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            WA(JJ) = IPRT
            JJ = JJ + 1
            WA(JJ) = IXC(NIXC,N)
            JJ = JJ + 1
            WA(JJ) = MPT
            JJ = JJ + 1
            WA(JJ) = NPG
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = THK0(I)
              THKQ = WA(JJ)
            ELSE
              WA(JJ) = ZERO
              THKQ = GBUF%THK(I)
            ENDIF
            JJ = JJ + 1
            IF (MLW /=  0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%EINT(I)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%EINT(I+LLT)
            ELSE
              WA(JJ) = ZERO
            ENDIF
c ----    Hourglass          
            IF (IHBE==11 .or. IHBE==23 .or. MLW == 0 .or. MLW == 13) THEN
              JJ = JJ + 1
              WA(JJ) = ZERO
              JJ = JJ + 1
              WA(JJ) = ZERO
              JJ = JJ + 1
              WA(JJ) = ZERO
            ELSE  ! not Batoz & not QEPH
              JJ = JJ + 1
              WA(JJ) = GBUF%HOURG(KK(1)+I) 
              JJ = JJ + 1
              WA(JJ) = GBUF%HOURG(KK(2)+I)  
              JJ = JJ + 1
              WA(JJ) = GBUF%HOURG(KK(3)+I)  
            ENDIF
c---------6 x2 +1(eps) for MPT=0 
            IF (IHBE /= 23) THEN
              IF (MPT == 0) THEN  ! global integration
                IF (MLW == 0 .or. MLW == 13) THEN
                  DO IPG=1,NPG 
                    DO J=1,13        ! forces and moments           	
                      JJ = JJ + 1
                      WA(JJ) = ZERO
                    ENDDO                                                               
                  ENDDO
                ELSEIF (NPG == 1) THEN
                  TENS(1:5) = GBUF%FOR(KK(1:5)+I)
                  CALL SHELL2G(TENS,QT(1,I))
                  DO J =1,6
                   JJ = JJ + 1                                      
                   WA(JJ) = TENS(J)             
                  END DO
c
                  TENS(1:3) = GBUF%MOM(KK(1:3)+I)
                  CALL SHEML2G(TENS,QT(1,I))
                  DO J =1,6
                   JJ = JJ + 1                                      
                   WA(JJ) = TENS(J)             
                  END DO
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF                        
                ELSE  ! NPG > 1
                  DO IR=1,NPTR                                                         
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                      IPG = NPTR*(IS-1) + IR
                      K = (IPG-1)*NEL*5
C                      
                      TENS(1:5) = GBUF%FORPG(K+KK(1:5)+I)
                      CALL SHELL2G(TENS,QT(1,I))
                      DO J =1,6
                       JJ = JJ + 1                                      
                       WA(JJ) = TENS(J)             
                      END DO
c
                      JJ = JJ + 1
                      IF (GBUF%G_PLA > 0) THEN    
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF                        
c
                      K = (IPG-1)*NEL*3
                      TENS(1:3) = GBUF%MOMPG(K+KK(1:3)+I)
                      CALL SHEML2G(TENS,QT(1,I))
                      DO J =1,6
                       JJ = JJ + 1                                      
                       WA(JJ) = TENS(J)             
                      END DO
                    ENDDO                                                               
                  ENDDO
                ENDIF  !  IF (MLW == 0 .or. MLW == 13)
C           (MPT >0 ):
              ELSEIF (MLW == 0 .or. MLW == 13) THEN
                DO K=1,MPT
                  DO IPG=1,NPG
                    DO J=1,8     ! Stress + plastic strain + T
                      JJ = JJ + 1
                      WA(JJ) = ZERO
                    ENDDO                                                               
                  ENDDO                                                                 
                ENDDO
              ELSE  !  NLAY >= 1, 
                IPT_ALL = 0
                DO IL = 1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  ILAW = BUFLY%ILAW
                  NPTT = BUFLY%NPTT
                  JDIR = 1 + (IL-1)*NEL*2
                  II = JDIR + I-1
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT        
                    DO IS=1,NPTS 
                      DO IR=1,NPTR                                                         
                        LBUF => BUFLY%LBUF(IR,IS,IT)
                        TENS(1:5) = LBUF%SIG(KK(1:5)+I)                        
                        CALL ORTH2LOC(TENS,DIR_A,DIR_B,II,ILAW,IGTYP,NEL)
                        CALL SHELL2G(TENS,QT(1,I))
                        DO J =1,6
                         JJ = JJ + 1                                      
                         WA(JJ) = TENS(J)             
                        END DO
                        JJ = JJ + 1
                        IF (BUFLY%L_PLA > 0) THEN
            	          WA(JJ) = LBUF%PLA(I)
                        ELSE
                          WA(JJ) = ZERO
                        ENDIF
                        JJ    = JJ + 1
                        WA(JJ) = POSLY(I,IPT)*TWO
                      ENDDO
                    ENDDO	    
                  ENDDO	    
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO
              ENDIF    ! MPT, NLAY
c---------
            ELSE ! IHBE = 23   (QEPH)
c---------
              IF (MLW==0 .or. MLW==13) THEN
                ST(1) = ZERO
                ST(2) = ZERO
                MT(1) = ZERO
                MT(2) = ZERO
                SK(1) = ZERO
                SK(2) = ZERO
                MK(1) = ZERO
                MK(2) = ZERO
                SHT(1)= ZERO
                SHT(2)= ZERO
                SHK(1)= ZERO
                SHK(2)= ZERO
                IF (MPT == 0) THEN
                  DO IPG=1,NPG
                    DO J=1,13           
                      JJ = JJ + 1
                      WA(JJ) = ZERO     
                    ENDDO                                                               
                  ENDDO               
                ELSE
                  DO IPG=1,NPG
              	   DO J=1,8         
                     JJ = JJ + 1      
                     WA(JJ) = ZERO
                   ENDDO                                                               
                  ENDDO
                ENDIF
              ELSE   ! MLW /= 0
                ST(1) = GBUF%HOURG(KK(1)+I) 
                ST(2) =-GBUF%HOURG(KK(2)+I) 
                MT(1) = GBUF%HOURG(KK(3)+I) 
                MT(2) =-GBUF%HOURG(KK(4)+I) 
                SK(1) =-GBUF%HOURG(KK(7)+I) 
                SK(2) = GBUF%HOURG(KK(8)+I) 
                MK(1) =-GBUF%HOURG(KK(9)+I) 
                MK(2) = GBUF%HOURG(KK(10)+I) 
                SHT(1)= GBUF%HOURG(KK(5)+I) 
                SHT(2)=-GBUF%HOURG(KK(6)+I) 
                SHK(1)=-GBUF%HOURG(KK(11)+I)
                SHK(2)= GBUF%HOURG(KK(12)+I)
              ENDIF
c
              IF (MPT == 0 .and. MLW /= 0 .and. MLW /= 13) THEN
                DO IPG=1,NPG
                  TENS(1:2) = GBUF%FOR(KK(1:2)+I)
     .	    	             + ST(1:2)*QPG(2,IPG)+SK(1:2)*QPG(1,IPG)
                  TENS(3) = GBUF%FOR(KK(3)+I)
                  TENS(4) = GBUF%FOR(KK(4)+I)
     .	    	           + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)
                  TENS(5) = GBUF%FOR(KK(5)+I)
     .	    	           + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)
                  CALL SHELL2G(TENS,QT(1,I))
                  DO J =1,6
                   JJ = JJ + 1                                      
                   WA(JJ) = TENS(J)             
                  END DO
                  TENS(1:2) = GBUF%MOM(KK(1:2)+I)
     .	    	            + MT(1:2)*QPG(2,IPG)+MK(1:2)*QPG(1,IPG)
                  TENS(3) = GBUF%MOM(KK(3)+I)
                  CALL SHEML2G(TENS,QT(1,I))
                  DO J =1,6
                   JJ = JJ + 1                                      
                   WA(JJ) = TENS(J)             
                  END DO
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF                        
                ENDDO
              ELSEIF (MLW /= 0 .and. MLW /= 13) THEN   ! NPT > 0
                IPT_ALL = 0
                DO IL = 1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  ILAW = BUFLY%ILAW
                  NPTT = BUFLY%NPTT
                  JDIR = 1 + (IL-1)*NEL*2
                  II = JDIR + I-1
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT        
                    LBUF => BUFLY%LBUF(1,1,IT)
                    L_PLA = BUFLY%L_PLA
                    ZZ = POSLY(I,IPT)*THKQ
                    DO IPG=1,NPG
                      TENS(1:2) = LBUF%SIG(KK(1:2)+I)                        
     .                           + (ST(1:2)+ZZ*MT(1:2))*QPG(2,IPG)              
     .                           + (SK(1:2)+ZZ*MK(1:2))*QPG(1,IPG)              
                      TENS(3) = LBUF%SIG(KK(3)+I)
                      TENS(4) = LBUF%SIG(KK(4)+I)
     .	    	               + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)
                      TENS(5) = LBUF%SIG(KK(5)+I)
     .	    	               + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)
                      CALL ORTH2LOC(TENS,DIR_A,DIR_B,II,ILAW,IGTYP,NEL)
                      CALL SHELL2G(TENS,QT(1,I))
                      DO J =1,6
                       JJ = JJ + 1                                      
                       WA(JJ) = TENS(J)             
                      END DO
                      JJ = JJ + 1                                      
                      IF (L_PLA > 0) THEN    
            	        WA(JJ) = LBUF%PLA(I)       
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF                        
                      JJ    = JJ + 1
                      WA(JJ) = POSLY(I,IPT)*TWO
                    ENDDO  !  DO IPG=1,NPG      
                  ENDDO  !  DO IT=1,NPTT
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO  !  DO IL=1,NLAY           
              ENDIF  !  IF (MPT == 0 .and. MLW /= 0 .and. MLW /= 13)
            ENDIF
C
            IE=IE+1
C         pointeur de fin de zone dans WA
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT
c------- end loop over 4 node shell elements
        ENDIF ! ITY == 3
        IF (ALLOCATED(DIRB)) DEALLOCATE(DIRB)                                                          
        IF (ALLOCATED(DIRA)) DEALLOCATE(DIRA)                                                          
      ENDDO   ! NG = 1, NGROUP
C
 200  CONTINUE
c-----------------------------------------------------------------------
c     4N SHELLS - WRITE
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELC
          PTWA_P0(N)=PTWA(N)
        ENDDO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELC,PTWA_P0,STAT_NUMELC_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
c-------------------------------------
      IF (ISPMD == 0.AND.LEN > 0) THEN
        IPRT0=0
        DO N=1,STAT_NUMELC_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXC(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)

          IOFF = NINT(WAP0(J + 1))
          IF (IOFF >= 1) THEN
            IPRT = NINT(WAP0(J + 2)) 
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISHE/STRS_F/GLOB'
                WRITE(IUGEO,'(A)')
     .          '#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .          '#  SHELLID       NPT       NPG                 THK' 
                WRITE(IUGEO,'(A)') '# EM, EB, H1, H2, H3' 
                WRITE(IUGEO,'(A/A/A/A/A)')
     .          '# IF(NPT == 0), REPEAT I=1,NPG :',
     .          '#   N1,  N2,  N3 ',
     .          '#   N12, N23, N31',
     .          '#   M1, M2, M3 ',
     .          '#   M12,M23,M31,EPSP '
                WRITE(IUGEO,'(A/A/A)')
     .          '# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :',
     .          '#   S1,  S2,  S3 ',
     .          '#   S12, S23, S31, EPSP, T '
                WRITE(IUGEO,'(A)')
     .          '#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISHE/STRS_F/GLOB'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#------------------------ REPEAT --------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#  SHELLID       NPT       NPG                 THK' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') '# EM, EB, H1, H2, H3' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') '# IF(NPT == 0), REPEAT I=1,NPG :'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   N1, N2, N3 '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   N12, N23, N31'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   M1, M2, M3 '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   M12, M23, M31, EPSP'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S1, S2, S3'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S12,S23,S31, EPSP, T '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#---------------------- END REPEAT ------------------------'
                CALL STRS_TXT50(LINE,100) 
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF
              IPRT0=IPRT
            ENDIF
c
            ID  = NINT(WAP0(J + 3)) 
            NPT = NINT(WAP0(J + 4)) 
            NPG = NINT(WAP0(J + 5)) 
            THK = WAP0(J + 6) 
            EM  = WAP0(J + 7) 
            EB  = WAP0(J + 8) 
            H1  = WAP0(J + 9) 
            H2  = WAP0(J + 10) 
            H3  = WAP0(J + 11) 
            J = J + 11
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              WRITE(IUGEO,'(1P5E20.13)')EM,EB,H1,H2,H3
            ELSE
              WRITE(LINE,'(3I10,1PE20.13)')ID,NPT,NPG,THK
               CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(1P5E20.13)')EM,EB,H1,H2,H3
               CALL STRS_TXT50(LINE,100)
            ENDIF
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,9)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=10,13)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),9,J,SIZP0,3)
                  CALL TAB_STRS_TXT50(WAP0(10),4,J,SIZP0,4)
                ENDIF
              ENDDO
            ELSE
              DO IT=1,NPT
                DO IPG=1,NPG
                  IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=4,8)
                  ELSE
                    CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                    CALL TAB_STRS_TXT50(WAP0(4),5,J,SIZP0,5)
                  ENDIF
                 J = J + 8
                END DO
              END DO
            ENDIF  !  IF (NPT == 0)
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,STAT_NUMELC_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELTG==0) GOTO 300
      IE=0
C
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 7) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW  = IPARG(1,NG)
          NEL  = IPARG(2,NG)
          NFT  = IPARG(3,NG)
          MPT  = IPARG(6,NG)
          IHBE = IPARG(23,NG)
          ITHK = IPARG(28,NG)
          IGTYP= IPARG(38,NG)
          IREP = IPARG(35,NG)
          ISUBSTACK=IPARG(71,NG)
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          LFT=1
          LLT=NEL
         NF1 = NFT+1
         IF (IHBE>=30) THEN
           IREL=0
         ELSE
           IREL=2
         END IF
!
          DO I=1,5
            KK(I) = NEL*(I-1)
          ENDDO
         IBID0 = 0
         MAT_1 = IXTG(1,NF1)
         PID_1 = IXTG(NIXTG-1,NF1)
         IF (ITHK >0 ) THEN
          THK0(LFT:LLT) = GBUF%THK(LFT:LLT)
         ELSE
          NF3 = NFT+NUMELC
          THK0(LFT:LLT) = THKE(LFT+NF3:LLT+NF3)
         END IF
         CALL LAYINI(ELBUF_TAB(NG),LFT      ,LLT      ,GEO      ,IGEO    ,
     .              MAT_1    ,PID_1    ,THKLY    ,MATLY    ,POSLY    , 
     .              IGTYP    ,IBID0    ,IBID0    ,NLAY     ,MPT      ,   
     .              ISUBSTACK,STACK    ,DRAPE_T3 ,NFT      ,THK0     ,
     .              NEL      ,THK_LY   )
!
         L_DIRA = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRA
         L_DIRB = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRB
         ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
         ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
         DIRA=ZERO
         DIRB=ZERO
         IF (L_DIRA == 0) THEN
           CONTINUE
         ELSEIF (IREP == 0) THEN
           DO J=1,NLAY
             J1 = 1+(J-1)*L_DIRA*NEL
             J2 = J*L_DIRA*NEL
             DIRA(J1:J2) = ELBUF_TAB(NG)%BUFLY(J)%DIRA(1:NEL*L_DIRA)
           ENDDO
         ENDIF
         DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
         DIR_B => DIRB(1:NLAY*NEL*L_DIRB)
         CALL GET_T3LSYS(LFT    ,LLT    ,IXTG(1,NF1),X    ,GBUF%OFF,
     .                   IREL   ,QT     ,NLAY   ,IREP ,NEL    ,
     .                   DIR_A   ,DIR_B,ELBUF_TAB(NG))
C
C pre counting of all NPTT (especialy for PID_51)
C
          NPT_ALL = 0
          DO IL=1,NLAY
            NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
          ENDDO
          MPT  = MAX(1,NPT_ALL)
          IF (IPARG(6,NG) == 0) MPT=0
C
c------- loop over 3 node shell elements
C
          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTTG(N)
            IF (IPART_STATE(IPRT) == 0) CYCLE
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%OFF(I)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            WA(JJ) = IPRT
            JJ = JJ + 1
            WA(JJ) = IXTG(NIXTG,N)
            JJ = JJ + 1
            WA(JJ) = MPT
            JJ = JJ + 1
            WA(JJ) = NPG
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = THK0(I)
              THKQ = WA(JJ)
            ELSE
              WA(JJ) = ZERO
              THKQ = GBUF%THK(I)
            ENDIF
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%EINT(I)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%EINT(I+LLT)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            WA(JJ) = ZERO
            JJ = JJ + 1
            WA(JJ) = ZERO
            JJ = JJ + 1
            WA(JJ) = ZERO
c----          
            IF (MPT == 0) THEN  ! global integration
              IF (MLW == 0 .or. MLW == 13) THEN
                DO IPG=1,NPG 
                  DO J=1,13                                     
                    JJ = JJ + 1                                
                    WA(JJ) = ZERO                              
                  ENDDO                                        
                ENDDO                                          
              ELSEIF (NPG == 1) THEN
                  TENS(1:5) = GBUF%FOR(KK(1:5)+I)
                  CALL SHELL2G(TENS,QT(1,I))
                  DO J =1,6
                   JJ = JJ + 1                                      
                   WA(JJ) = TENS(J)             
                  END DO
c
                  TENS(1:3) = GBUF%MOM(KK(1:3)+I)
                  CALL SHEML2G(TENS,QT(1,I))
                  DO J =1,6
                   JJ = JJ + 1                                      
                   WA(JJ) = TENS(J)             
                  END DO
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF                        
              ELSE                                              
                  DO IR=1,NPTR                                                         
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                      IPG = NPTR*(IS-1) + IR
                      K = (IPG-1)*NEL*5
C                      
                      TENS(1:5) = GBUF%FORPG(K+KK(1:5)+I)
                      CALL SHELL2G(TENS,QT(1,I))
                      DO J =1,6
                       JJ = JJ + 1                                      
                       WA(JJ) = TENS(J)             
                      END DO
c
                      JJ = JJ + 1
                      IF (GBUF%G_PLA > 0) THEN    
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF                        
c
                      K = (IPG-1)*NEL*3
                      TENS(1:3) = GBUF%MOMPG(K+KK(1:3)+I)
                      CALL SHEML2G(TENS,QT(1,I))
                      DO J =1,6
                       JJ = JJ + 1                                      
                       WA(JJ) = TENS(J)             
                      END DO
                    ENDDO                                                               
                  ENDDO
              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
            ELSE ! MPT > 0
              IF (MLW == 0 .or. MLW == 13) THEN
               DO K=1,MPT
                DO IPG=1,NPG
                  DO J=1,8
                    JJ = JJ + 1
                    WA(JJ) = ZERO
                  ENDDO
                ENDDO
               ENDDO
              ELSE
                IPT_ALL = 0
                DO IL = 1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  ILAW = BUFLY%ILAW
                  NPTT = BUFLY%NPTT
                  JDIR = 1 + (IL-1)*NEL*2
                  II = JDIR + I-1
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT        
                    DO IPG=1,NPG
                      LBUF => BUFLY%LBUF(IPG,1,IT)
                      TENS(1:5) = LBUF%SIG(KK(1:5)+I)                        
                      CALL ORTH2LOC(TENS,DIR_A,DIR_B,II,ILAW,IGTYP,NEL)
                      CALL SHELL2G(TENS,QT(1,I))
                      DO J =1,6
                       JJ = JJ + 1                                      
                       WA(JJ) = TENS(J)             
                      END DO
                      JJ = JJ + 1
                      IF (BUFLY%L_PLA > 0) THEN
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE
                        WA(JJ) = ZERO
                      ENDIF
                      JJ    = JJ + 1
                      WA(JJ) = POSLY(I,IPT)*TWO
                    ENDDO !IPG=1,NPG	    
                  ENDDO	    
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO
              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
            ENDIF  !  IF (MPT == 0)
C
            IE=IE+1
C         pointeur de fin de zone
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT
        ENDIF  !  IF (ITY == 7)
        IF (ALLOCATED(DIRB)) DEALLOCATE(DIRB)                                                          
        IF (ALLOCATED(DIRA)) DEALLOCATE(DIRA)                                                          
      ENDDO  !  DO NG=1,NGROUP
C
 300  CONTINUE
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELTG
          PTWA_P0(N)=PTWA(N)
        ENDDO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELTG,PTWA_P0,STAT_NUMELTG_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF

      IF (ISPMD == 0.AND.LEN > 0) THEN
        IPRT0=0
        DO N=1,STAT_NUMELTG_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXTG(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF = NINT(WAP0(J + 1))
          IF (IOFF >= 1) THEN
            IPRT  = NINT(WAP0(J + 2)) 
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISH3/STRS_F/GLOB'
                WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
                WRITE(IUGEO,'(A)')
     .'# EM, EB, H1, H2, H3' 
                WRITE(IUGEO,'(A/A/A/A/A)')
     .'# IF(NPT == 0), REPEAT I=1,NPG :',
     .'#   N1, N2, N3',
     .'#   N12,N23,N31',
     .'#   M1, M2, M3 ',
     .'#   M12,M23,M31,EPSP '
                WRITE(IUGEO,'(A/A/A)')
     .'# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :',
     .'#   S1, S2, S3 ',
     .'#   S12,S23,S31, EPSP, T '
                WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISH3/STRS_F/GLOB'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------'
                CALL STRS_TXT50(LINE,100) 
                WRITE(LINE,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# EM, EB, H1, H2, H3'
                CALL STRS_TXT50(LINE,100) 
                WRITE(LINE,'(A)')
     .'# IF(NPT == 0), REPEAT I=1,NPG :'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   N1, N2, N3'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   N12, N23, N31'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   M1, M2, M3 '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   M12, M23, M31,EPSP '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S1, S2, S3 '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S12, S23, S31, EPSP, T '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF  !  IF (IZIPSTRS == 0)
              IPRT0=IPRT
            ENDIF  !  IF (IPRT /= IPRT0)
            ID  = NINT(WAP0(J + 3)) 
            NPT = NINT(WAP0(J + 4)) 
            NPG = NINT(WAP0(J + 5)) 
            THK = WAP0(J + 6) 
            EM  = WAP0(J + 7) 
            EB  = WAP0(J + 8) 
            H1  = WAP0(J + 9) 
            H2  = WAP0(J + 10) 
            H3  = WAP0(J + 11) 
            J = J + 11
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              WRITE(IUGEO,'(1P5E20.13)')EM,EB,H1,H2,H3
            ELSE
              WRITE(LINE,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(1P5E20.13)')EM,EB,H1,H2,H3
              CALL STRS_TXT50(LINE,100)
            ENDIF
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,9)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=10,13)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),9,J,SIZP0,3)
                  CALL TAB_STRS_TXT50(WAP0(10),4,J,SIZP0,4)
                ENDIF
              ENDDO
            ELSE
              DO IT=1,NPT
                DO IPG=1,NPG
                  IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=4,8)
                  ELSE
                    CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                    CALL TAB_STRS_TXT50(WAP0(4),5,J,SIZP0,5)
                  ENDIF
                 J = J + 8
                END DO
              END DO
            ENDIF  !  IF (NPT == 0)
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,STAT_NUMELTG_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C
      RETURN
      END
c                        TENS(JFT:JLT,1) = SIGNXX(JFT:JLT)                     
c                        TENS(JFT:JLT,2) = SIGNYY(JFT:JLT)
c                        TENS(JFT:JLT,3) = SIGNXY(JFT:JLT)
c                        TENS(JFT:JLT,4) = SIGNYZ(JFT:JLT)
c                        TENS(JFT:JLT,5) = SIGNZX(JFT:JLT)
Chd|====================================================================
Chd|  ORTH2LOC                      source/output/sta/stat_c_strsfg.F
Chd|-- called by -----------
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|-- calls ---------------
Chd|        UROTOVS                       source/output/sta/stat_c_strsfg.F
Chd|====================================================================
      SUBROUTINE ORTH2LOC(TENS,DIR_A,DIR_B,II,ILAW,IGTYP,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER II,ILAW,IGTYP,NEL
C     REAL ou REAL*8
      my_real
     .   TENS(5), DIR_A(*),DIR_B(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
C     REAL ou REAL*8
      my_real
     .   R1,R2,R3,S1,S2,S3,R12A,R22A,S12B,S22B,RS1,RS2,RS3,
     .   T1,T2,T3,PHI,SUM1,SUM2,FACT,R3R3,S3S3 
c------------------------------------------------
          IF (IGTYP /= 1) THEN                                                     
c------------------------------------------------
            IF (IGTYP == 16) THEN                                                  
c                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
                                           
                RS1= R1*S1                                                         
                RS2= R2*S2                                                         
                R12A = R1*R1                                                       
                R22A = R2*R2                                                       
                S12B = S1*S1                                                       
                S22B = S2*S2                                                       

                RS3 = S1*S2-R1*R2                                                  
                R3R3= ONE+S1*R2+R1*S2                                               
                R3R3= HALF*R3R3                                                  
                S3S3= ONE-S1*R2-R1*S2                                               
                S3S3= HALF*S3S3                                                  
                T1 = TENS(1)                                                     
                T2 = TENS(2)                                                     
                T3 = TENS(3)                                                     
                TENS(1) = R12A*T1 + R22A*T2 - RS3*T3                             
                TENS(2) = S12B*T1 + S22B*T2 + RS3*T3                             
                TENS(3) = RS1*T1  + RS2*T2 + (R3R3 - S3S3)*T3                    
c
            ELSEIF ((IGTYP == 51 .OR. IGTYP == 52) .AND. ILAW == 58) THEN           
c                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
c
                RS1= R1*S1                                                         
                RS2= R2*S2                                                         
                R12A = R1*R1                                                       
                R22A = R2*R2                                                       
                S12B = S1*S1                                                       
                S22B = S2*S2                                                       
                RS3 = S1*S2-R1*R2                                                  
                R3R3= ONE+S1*R2+R1*S2                                               
                R3R3= HALF*R3R3                                                  
                S3S3= ONE-S1*R2-R1*S2                                               
                S3S3= HALF*S3S3                                                  
                T1 = TENS(1)                                                     
                T2 = TENS(2)                                                     
                T3 = TENS(3)                                                     
c
                TENS(1) = R12A*T1 + R22A*T2 - RS3*T3                             
                TENS(2) = S12B*T1 + S22B*T2 + RS3*T3                             
                TENS(3) = RS1*T1  + RS2*T2  + (R3R3 - S3S3)*T3                   
            ELSE                                                             
              IF (ILAW /= 1 .and. ILAW /= 2 .and. ILAW /= 19 .and. ILAW /= 27 .and. ILAW /= 32)
     .          CALL UROTOVS(TENS,DIR_A(II),DIR_A(II+NEL))
            ENDIF                                                                  
          ENDIF   ! IGTYP                                                          
C
      RETURN
      END
Chd|====================================================================
Chd|  UROTOVS                       source/output/sta/stat_c_strsfg.F
Chd|-- called by -----------
Chd|        ORTH2LOC                      source/output/sta/stat_c_strsfg.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UROTOVS(SIG,DIR1,DIR2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   SIG(5), DIR1,DIR2
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .   S1, S2, S3, S4, S5
C-----------------------------------------------
       S1 = DIR1*DIR1*SIG(1)
     .    + DIR2*DIR2*SIG(2)-TWO*DIR1*DIR2*SIG(3)
       S2 = DIR2*DIR2*SIG(1)
     .    + DIR1*DIR1*SIG(2)+TWO*DIR2*DIR1*SIG(3)
       S3 = DIR1*DIR2*SIG(1)
     .    - DIR2*DIR1*SIG(2)
     .    +(DIR1*DIR1-DIR2*DIR2)*SIG(3)
       S4 = DIR2*SIG(5)+DIR1*SIG(4)
       S5 = DIR1*SIG(5)-DIR2*SIG(4)
       SIG(1)=S1
       SIG(2)=S2
       SIG(3)=S3
       SIG(4)=S4
       SIG(5)=S5
C
      RETURN
      END
Chd|====================================================================
Chd|  SHEML2G                       source/output/sta/stat_c_strsfg.F
Chd|-- called by -----------
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHEML2G(MOM,QT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   MOM(6),QT(3,3)  
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real 
     .       TXX,TYY,TZZ,TXY,TYZ,TZX,UXX,UYY,UZZ,UXY,UYZ,UZX,A,B,C
C--convention input MOM : mxx,myy,mxy,0,0,0; output mxx,myy,mzz,mxy,myz,mzx
             TXX = MOM(1)
             TYY = MOM(2)
             TZZ = ZERO
             TXY = MOM(3)
             TYZ = ZERO
             TZX = ZERO
C
              A = QT(1,1)*TXX + QT(2,1)*TXY    
              B = QT(1,1)*TXY + QT(2,1)*TYY    
c              C = ZERO   
              UXX = A*QT(1,1) + B*QT(1,2)    
              UXY = A*QT(2,1) + B*QT(2,2)    
              UZX = A*QT(3,1) + B*QT(3,2)    
              A = QT(1,2)*TXX + QT(2,2)*TXY    
              B = QT(1,2)*TXY + QT(2,2)*TYY    
c              C = ZERO    
              UYY = A*QT(2,1) + B*QT(2,2)    
              UYZ = A*QT(3,1) + B*QT(3,2)    
              A = QT(1,3)*TXX + QT(2,3)*TXY    
              B = QT(1,3)*TXY + QT(2,3)*TYY    
c              C = ZERO   
              UZZ = A*QT(3,1) + B*QT(3,2)  
C              
             MOM(1) = UXX  
             MOM(2) = UYY  
             MOM(3) = UZZ  
             MOM(4) = UXY 
             MOM(5) = UYZ 
             MOM(6) = UZX 
C             
      RETURN
      END
