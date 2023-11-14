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
Chd|  STAT_C_STRSF                  source/output/sta/stat_c_strsf.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_STRSF(
     1                  ELBUF_TAB  ,IPARG     ,IPM        ,IGEO  ,IXC    ,
     2                  IXTG       ,WA        ,WAP0       ,IPARTC,IPARTTG,
     3                  IPART_STATE,STAT_INDXC,STAT_INDXTG,THKE  ,SIZP0) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
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
     .   THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,NPT,
     .   LLT,ITY,MLW,IH,IHBE, ID, IPRT0, IPRT,IR,IS,IT,
     .   NPG,IPG,MPT,IPT,NPTR,NPTS,NPTT,NLAY,L_PLA,ITHK,
     .   IGTYP,NPT_ALL,IL,KK(12)
      INTEGER PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .        PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
      double precision   
     .  THK, EM, EB, H1, H2, H3
      my_real   
     .   PG,MPG,QPG(2,4),THKQ,
     .   SK(2),ST(2),MK(2),MT(2),SHK(2),SHT(2),Z01(11,11),ZZ
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
C-----------------------------------------------
      PARAMETER (PG = .577350269189626)
      PARAMETER (MPG=-.577350269189626)
      DATA QPG/MPG,MPG,PG,MPG,PG,PG,MPG,PG/
      DATA  Z01/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,0.       ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.1666667,0.1666667,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.25     ,0.       ,0.25     ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,
     7 0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,
     8 0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,
     9 -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,
     9 0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,
     A -.5      ,-.3888889,-.2777778,-.1666667,0.0555555,
     A 0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,
     B -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,
     B 0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /
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
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          IF (IHBE == 23) NPG=4
          LFT=1
          LLT=NEL
!
          DO I=1,12  ! length max of GBUF%G_HOURG = 12
            KK(I) = NEL*(I-1)
          ENDDO
!
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52 ) THEN
            NPT_ALL = 0
            DO IL=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF
          IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0
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
              IF (ITHK > 0) THEN
                WA(JJ) = GBUF%THK(I)
              ELSE
                WA(JJ) = THKE(N)
              ENDIF
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
c---------
            IF (IHBE /= 23) THEN
              IF (MPT == 0) THEN  ! global integration
                IF (MLW == 0 .or. MLW == 13) THEN
                  DO IPG=1,NPG 
                    DO J=1,8        ! forces and moments           
                      JJ = JJ + 1
                      WA(JJ) = ZERO
                    ENDDO                                                               
                  ENDDO
                ELSEIF (NPG == 1) THEN
                  JJ = JJ + 1                                      
                  WA(JJ) = GBUF%FOR(KK(1)+I)             
                  JJ = JJ + 1                                
                  WA(JJ) = GBUF%FOR(KK(2)+I)             
                  JJ = JJ + 1                                
                  WA(JJ) = GBUF%FOR(KK(3)+I)             
                  JJ = JJ + 1                                
                  WA(JJ) = GBUF%FOR(KK(4)+I)             
                  JJ = JJ + 1                                
                  WA(JJ) = GBUF%FOR(KK(5)+I)             
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
              WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF                        
c
                  JJ = JJ + 1
                  WA(JJ) = GBUF%MOM(KK(1)+I)             
                  JJ = JJ + 1                                
                  WA(JJ) = GBUF%MOM(KK(2)+I)             
                  JJ = JJ + 1                                
                  WA(JJ) = GBUF%MOM(KK(3)+I)             
                ELSE  ! NPG > 1
                  DO IR=1,NPTR                                                         
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                      IPG = NPTR*(IS-1) + IR
                      K = (IPG-1)*NEL*5
                      JJ = JJ + 1
                      WA(JJ) = GBUF%FORPG(K + KK(1) + I)         
                      JJ = JJ + 1               
                      WA(JJ) = GBUF%FORPG(K + KK(2) + I)         
                      JJ = JJ + 1               
                      WA(JJ) = GBUF%FORPG(K + KK(3) + I)         
                      JJ = JJ + 1               
                      WA(JJ) = GBUF%FORPG(K + KK(4) + I)         
                      JJ = JJ + 1               
                      WA(JJ) = GBUF%FORPG(K + KK(5) + I)         
c
                      JJ = JJ + 1
                      IF (GBUF%G_PLA > 0) THEN    
                  WA(JJ) = LBUF%PLA(I)
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF                        
c
                      K = (IPG-1)*NEL*3
                      JJ = JJ + 1
                      WA(JJ) = GBUF%MOMPG(K + KK(1) + I)         
                      JJ = JJ + 1               
                      WA(JJ) = GBUF%MOMPG(K + KK(2) + I)         
                      JJ = JJ + 1                
                      WA(JJ) = GBUF%MOMPG(K + KK(3) + I)       
                    ENDDO                                                               
                  ENDDO
                ENDIF  !  IF (MLW == 0 .or. MLW == 13)
C           (MPT /=0 ):
              ELSEIF (MLW == 0 .or. MLW == 13) THEN
                DO K=1,MPT
                  DO IPG=1,NPG
                    DO J=1,6     ! Stress + plastic strain
                      JJ = JJ + 1
                      WA(JJ) = ZERO
                    ENDDO                                                               
                  ENDDO                                                                 
                ENDDO
              ELSEIF (NLAY == 1) THEN    ! PID1
                BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                NPTT = BUFLY%NPTT
                DO IT=1,NPTT
                  DO IS=1,NPTS 
                    DO IR=1,NPTR                                                         
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      IPG = NPTR*(IS-1) + IR
                      JJ = JJ + 1
                      WA(JJ) = LBUF%SIG(KK(1)+I)
                      JJ = JJ + 1
                      WA(JJ) = LBUF%SIG(KK(2)+I)
                      JJ = JJ + 1
                      WA(JJ) = LBUF%SIG(KK(3)+I)
                      JJ = JJ + 1
                      WA(JJ) = LBUF%SIG(KK(4)+I)
                      JJ = JJ + 1
                      WA(JJ) = LBUF%SIG(KK(5)+I)
                JJ = JJ + 1
                      IF (BUFLY%L_PLA > 0) THEN
                        WA(JJ) = LBUF%PLA(I)
                      ELSE
                        WA(JJ) = ZERO
                      ENDIF
                    ENDDO      
                  ENDDO      
                ENDDO  !  DO IPT = 1,NPTT
              ELSE  !  NLAY > 1, PID10,PID11,PID16,PID17,PID51
                II = 5*(I-1)
                DO IL = 1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  DO IT=1,NPTT
                    DO IS=1,NPTS 
                      DO IR=1,NPTR                                                         
                        LBUF => BUFLY%LBUF(IR,IS,IT)
                        JJ = JJ + 1
                        WA(JJ) = LBUF%SIG(KK(1)+I)
                        JJ = JJ + 1
                        WA(JJ) = LBUF%SIG(KK(2)+I)
                        JJ = JJ + 1
                        WA(JJ) = LBUF%SIG(KK(3)+I)
                        JJ = JJ + 1
                        WA(JJ) = LBUF%SIG(KK(4)+I)
                        JJ = JJ + 1
                        WA(JJ) = LBUF%SIG(KK(5)+I)
                        JJ = JJ + 1
                        IF (BUFLY%L_PLA > 0) THEN
                    WA(JJ) = LBUF%PLA(I)
                        ELSE
                          WA(JJ) = ZERO
                        ENDIF
                      ENDDO
                    ENDDO      
                  ENDDO      
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
                    DO J=1,8           
                      JJ = JJ + 1
                      WA(JJ) = ZERO     
                    ENDDO                                                               
                  ENDDO               
                ELSE
                  DO IPG=1,NPG
             DO J=1,6         
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
                  JJ = JJ + 1
                  WA(JJ) = GBUF%FOR(KK(1)+I)
     .                    + ST(1)*QPG(2,IPG) + SK(1)*QPG(1,IPG)
                  JJ = JJ + 1
                  WA(JJ) = GBUF%FOR(KK(2)+I)
     .                    + ST(2)*QPG(2,IPG)+SK(2)*QPG(1,IPG)
                  JJ = JJ + 1
                  WA(JJ) = GBUF%FOR(KK(3)+I)
                  JJ = JJ + 1
                  WA(JJ) = GBUF%FOR(KK(4)+I)
     .                   + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)
                  JJ = JJ + 1
                  WA(JJ) = GBUF%FOR(KK(5)+I)
     .                   + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)
c
                  JJ = JJ + 1
                  WA(JJ) = GBUF%MOM(KK(1)+I)
     .                    + MT(1)*QPG(2,IPG)+MK(1)*QPG(1,IPG)
                  JJ = JJ + 1
                  WA(JJ) = GBUF%MOM(KK(2)+I)
     .                    + MT(2)*QPG(2,IPG)+MK(2)*QPG(1,IPG)
                  JJ = JJ + 1
                  WA(JJ) = GBUF%MOM(KK(3)+I)
                ENDDO
              ELSEIF (MLW /= 0 .and. MLW /= 13) THEN   ! NPT > 0
                DO IL=1,NLAY
                  BUFLY =>ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  DO IT=1,NPTT
                    LBUF => BUFLY%LBUF(1,1,IT)
                    L_PLA = BUFLY%L_PLA
C
                    IPT = NPTT*(IL-1) + IT
                    ZZ = GBUF%THK(I)*Z01(IPT,MAX(NLAY,NPT))
C
                    DO IPG=1,NPG
                      JJ = JJ + 1                                       
                      WA(JJ) = LBUF%SIG(KK(1)+I)
     .                       + (ST(1)+ZZ*MT(1))*QPG(2,IPG)              
     .                       + (SK(1)+ZZ*MK(1))*QPG(1,IPG)              
C
                      JJ = JJ + 1                                       
                      WA(JJ) = LBUF%SIG(KK(2)+I)
     .                       + (ST(2)+ZZ*MT(2))*QPG(2,IPG)              
     .                       + (SK(2)+ZZ*MK(2))*QPG(1,IPG)              
C
                      JJ = JJ + 1                                       
                      WA(JJ) = LBUF%SIG(KK(3)+I)
C
                      JJ = JJ + 1                                       
                      WA(JJ) = LBUF%SIG(KK(4)+I)
     .                       + SHT(2)*QPG(2,IPG) + SHK(2)*QPG(1,IPG)    
C
                      JJ = JJ + 1                                       
                      WA(JJ) = LBUF%SIG(KK(5)+I)
     .                       + SHT(1)*QPG(2,IPG) + SHK(1)*QPG(1,IPG)    
C
                      JJ = JJ + 1 
                      IF (L_PLA > 0) THEN                               
                        WA(JJ) = LBUF%PLA(I) 
                      ELSE                                              
                        WA(JJ) = ZERO                                   
                      ENDIF                                              
                    ENDDO  !  DO IPG=1,NPG      
                  ENDDO  !  DO IT=1,NPTT
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
                WRITE(IUGEO,'(A)')'/INISHE/STRS_F'
                WRITE(IUGEO,'(A)')
     .          '#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .          '#  SHELLID       NPT       NPG                 THK' 
                WRITE(IUGEO,'(A)') '# EM, EB, H1, H2, H3' 
                WRITE(IUGEO,'(A/A/A)')
     .          '# IF(NPT == 0), REPEAT I=1,NPG :',
     .          '#   N1, N2, N12, N23, N31',
     .          '#   EPSP, M1, M2, M12'
                         WRITE(IUGEO,'(A/A/A)')
     .          '# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :',
     .          '#   S1, S2, S12',
     .          '#   S23, S31, EPSP'
                WRITE(IUGEO,'(A)')
     .          '#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISHE/STRS_F'
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
                WRITE(LINE,'(A)')'#   N1, N2, N12, N23, N31'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   EPSP, M1, M2, M12'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S1, S2, S12'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S23, S31, EPSP'
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
                  WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,5)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=6,9)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),5,J,SIZP0,5)
                  CALL TAB_STRS_TXT50(WAP0(6),4,J,SIZP0,4)
                ENDIF
              ENDDO
            ELSE
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6*NPT*NPG)
              ELSE
                CALL TAB_STRS_TXT50(WAP0(1),6*NPT*NPG,J,SIZP0,3)
              ENDIF
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
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          LFT=1
          LLT=NEL
!
          DO I=1,5
            KK(I) = NEL*(I-1)
          ENDDO
!
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT_ALL = 0
            DO K=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(K)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF
          IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0
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
              IF (ITHK > 0) THEN
                WA(JJ) = GBUF%THK(I)
              ELSE
                WA(JJ) = THKE(N+NUMELC)
              ENDIF
            ELSE
              WA(JJ) = ZERO
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
                  DO J=1,9                                     
                    JJ = JJ + 1                                
                    WA(JJ) = ZERO                              
                  ENDDO                                        
                ENDDO                                          
              ELSEIF (NPG == 1) THEN
                JJ = JJ + 1                                      
                WA(JJ) = GBUF%FOR(KK(1) + I)                 
                JJ = JJ + 1                                 
                WA(JJ) = GBUF%FOR(KK(2) + I)                 
                JJ = JJ + 1                                
                WA(JJ) = GBUF%FOR(KK(3) + I)                 
                JJ = JJ + 1                                  
                WA(JJ) = GBUF%FOR(KK(4) + I)                 
                JJ = JJ + 1                                  
                WA(JJ) = GBUF%FOR(KK(5) + I)                 
c
                JJ = JJ + 1                                        
                IF (GBUF%G_PLA > 0) THEN           
                  WA(JJ) = GBUF%PLA(I)            
                ELSE                               
                  WA(JJ) = ZERO                    
                ENDIF                              
c
                JJ = JJ + 1                                      
                WA(JJ) = GBUF%MOM(KK(1) + I)                 
                JJ = JJ + 1                                
                WA(JJ) = GBUF%MOM(KK(2) + I)                 
                JJ = JJ + 1                                
                WA(JJ) = GBUF%MOM(KK(3) + I)                 
              ELSE                                              
                DO IPG=1,NPG                                     
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPG,1,1)
                  K = (IPG-1)*NEL*5
                  JJ = JJ + 1                                    
                  WA(JJ) = GBUF%FORPG(K + KK(1) + I)               
                  JJ = JJ + 1                                      
                  WA(JJ) = GBUF%FORPG(K + KK(2) + I)               
                  JJ = JJ + 1                                    
                  WA(JJ) = GBUF%FORPG(K + KK(3) + I)               
                  JJ = JJ + 1                                      
                  WA(JJ) = GBUF%FORPG(K + KK(4) + I)               
                  JJ = JJ + 1                                      
                  WA(JJ) = GBUF%FORPG(K + KK(5) + I)               
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN         
              WA(JJ) = LBUF%PLA(I)       
                  ELSE                             
                    WA(JJ) = ZERO                  
                  ENDIF                            
c
                  K = (IPG-1)*NEL*3                              
                  JJ = JJ + 1                                    
                  WA(JJ) = GBUF%MOMPG(K + KK(1) + I)               
                  JJ = JJ + 1                                    
                  WA(JJ) = GBUF%MOMPG(K + KK(2) + I)               
                  JJ = JJ + 1                                    
                  WA(JJ) = GBUF%MOMPG(K + KK(3) + I)               
                ENDDO  !  DO IPG=1,NPG                                         
              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
            ELSE ! MPT > 0
              IF (MLW == 0 .or. MLW == 13) THEN
                DO IPG=1,NPG
                  DO J=1,6
                    JJ = JJ + 1
                    WA(JJ) = ZERO
                  ENDDO
                ENDDO
              ELSE
                DO IL=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  DO IT=1,NPTT
                    DO IPG=1,NPG
                      LBUF => BUFLY%LBUF(IPG,1,IT)
                      L_PLA = BUFLY%L_PLA
                      DO J=1,5
                        JJ = JJ + 1
                        WA(JJ) = LBUF%SIG(KK(J)+I)    
                      ENDDO      
                      JJ = JJ + 1
                      IF (L_PLA > 0) THEN                                   
                        WA(JJ) = LBUF%PLA(I)
                      ELSE
                        WA(JJ) = ZERO
                      ENDIF
                    ENDDO !  DO IPG=1,NPG
                  ENDDO !  DO IT=1,NPTT   
                ENDDO  !  DO IL=1,NLAY
              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
            ENDIF  !  IF (MPT == 0)
C
            IE=IE+1
C         pointeur de fin de zone
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT
        ENDIF  !  IF (ITY == 7)
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
                WRITE(IUGEO,'(A)')'/INISH3/STRS_F'
                WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
                WRITE(IUGEO,'(A)')
     .'# EM, EB, H1, H2, H3' 
                WRITE(IUGEO,'(A/A/A)')
     .'# IF(NPT == 0), REPEAT I=1,NPG :',
     .'#   N1, N2, N12, N23, N31',
     .'#   EPSP, M1, M2, M12'
                WRITE(IUGEO,'(A/A/A)')
     .'# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :',
     .'#   S1, S2, S12',
     .'#   S23, S31, EPSP'
                WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISH3/STRS_F'
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
                WRITE(LINE,'(A)')'#   N1, N2, N12, N23, N31'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   EPSP, M1, M2, M12'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S1, S2, S12'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'#   S23, S31, EPSP'
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
                  WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,5)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=6,9)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),5,J,SIZP0,5)
                  CALL TAB_STRS_TXT50(WAP0(6),4,J,SIZP0,4)
                ENDIF
              ENDDO
            ELSE
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6*NPT*NPG)
              ELSE
                CALL TAB_STRS_TXT50(WAP0(1),6*NPT*NPG,J,SIZP0,3)
              ENDIF
            ENDIF  !  IF (NPT == 0)
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,STAT_NUMELTG_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C
      RETURN
      END
