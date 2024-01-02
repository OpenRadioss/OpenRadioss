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
Chd|  STAT_C_EPSPF                  source/output/sta/stat_c_epspf.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_EPSPF(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXC ,
     2                        IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                        IPART_STATE,STAT_INDXC,STAT_INDXTG,THKE,
     4                        SIZP0)
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
#include      "scr14_c.inc"
#include      "task_c.inc"
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
      INTEGER I,N,J,K,JJ,LEN,IOFF,NG, NEL, NFT, ITY, LFT,LLT,IHBE,
     .   MLW, NPTR,NPTS,NPTT,NLAY,NPG,NPT,IR,IS,ID,IPRT0,IPRT,
     .   IPG,MPT,NPTM,IPT,IE,ITHK,IT,IGTYP,NPT_ALL
      INTEGER PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .        PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
      double precision   
     .    THK
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
c
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELC == 0) GOTO 200
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
          NPT  = NLAY*NPTT 
          NPG  = NPTR*NPTS
          IF (IHBE == 23) NPG=4
          LFT  = 1
          LLT  = NEL
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP ==52) THEN
            NPT_ALL = 0
            DO K=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(K)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF
C
          DO I=LFT,LLT
            N  = I + NFT
            IPRT=IPARTC(N)
            IF (IPART_STATE(IPRT) == 0) CYCLE
C
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
              IF (ITHK >0 ) THEN
                WA(JJ) = GBUF%THK(I)
              ELSE
                WA(JJ) = THKE(N)
              END IF
            ELSE
              WA(JJ) = ZERO
            ENDIF
c
            IF (NLAY > 1) THEN
              IF (IHBE == 23) THEN
                DO K=1,NLAY                                            
                  BUFLY => ELBUF_TAB(NG)%BUFLY(K)
                  NPTT = BUFLY%NPTT
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IT=1,NPTT
                      DO IR=1,NPG                                           
                        JJ = JJ + 1
                        WA(JJ) = BUFLY%LBUF(1,1,IT)%PLA(I)
                      ENDDO
                    ENDDO
                  ELSE
                    DO IT=1,NPTT
                      DO IR=1,NPG                                           
                        JJ = JJ + 1         
                        WA(JJ)=ZERO         
                      ENDDO                   
                    ENDDO
                  ENDIF
                ENDDO   
              ELSE  !  (IHBE /= 23)
                DO K=1,NLAY                                            
                  BUFLY => ELBUF_TAB(NG)%BUFLY(K)
                  NPTT = BUFLY%NPTT
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IT=1,NPTT
                      DO IS=1,NPTS
                        DO IR=1,NPTR                                            
                          JJ = JJ + 1
                          WA(JJ) = BUFLY%LBUF(IR,IS,IT)%PLA(I)
                        ENDDO                                                  
                      ENDDO
                    ENDDO
                  ELSE
                    DO IT=1,NPTT
                      DO IS=1,NPTS                                          
                        DO IR=1,NPTR                                            
                          JJ = JJ + 1         
                          WA(JJ)=ZERO         
                        ENDDO                                                  
                      ENDDO                   
                    ENDDO
                  ENDIF
                ENDDO   
              ENDIF ! IHBE
c           NLAY = 1 :
            ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) THEN
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)
              NPTT = BUFLY%NPTT
              IF (IHBE == 23) THEN
                DO IT=1,NPTT                                       
                  DO IR=1,NPG                                        
                    JJ = JJ + 1                                            
                    WA(JJ) = BUFLY%LBUF(1,1,IT)%PLA(I)
                  ENDDO                                        
                ENDDO                                      
              ELSE
                DO IT=1,NPTT                                       
                  DO IS=1,NPTS                                            
                    DO IR=1,NPTR                                        
                      JJ = JJ + 1                                            
                      WA(JJ) = BUFLY%LBUF(IR,IS,IT)%PLA(I)         
                    ENDDO                                     
                  ENDDO                                         
                ENDDO                                         
              ENDIF ! IHBE
            ELSE                       
              IF (IHBE == 23) THEN
                DO K=1,NPTT                                            
                  DO IR=1,NPG                                              
                    JJ = JJ + 1          
                    WA(JJ) = ZERO        
                  ENDDO                    
                ENDDO                    
              ELSE
                DO K=1,NPTT                                            
                  DO IS=1,NPTS                                            
                    DO IR=1,NPTR                                              
                      JJ = JJ + 1          
                      WA(JJ) = ZERO        
                    ENDDO                                                    
                  ENDDO                    
                ENDDO
              ENDIF ! IHBE
            ENDIF ! (NLAY > 1)
C
            IE=IE+1
C         pointeur de fin de zone dans WA
            PTWA(IE)=JJ
          ENDDO  !  I=LFT,LLT
        ENDIF  !  IF (ITY == 3)
      ENDDO  !  DO NG=1,NGROUP
C
 200  CONTINUE
C
      IF (NSPMD == 1)THEN
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELC
          PTWA_P0(N)=PTWA(N)
        END DO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELC,PTWA_P0,STAT_NUMELC_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
C
      IF (ISPMD == 0 .AND. LEN > 0) THEN
C
        IPRT0=0
        DO N=1,STAT_NUMELC_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXC(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)

          IOFF  = NINT(WAP0(J + 1))
          IF (IOFF == 1) THEN
            IPRT  = NINT(WAP0(J + 2)) 
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISHE/EPSP_F'
                WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .      '#  SHELLID, NPT, NPG, THK' 
                WRITE(IUGEO,'(A)')
     .'# REPEAT IPT=1,MAX(1,NPT) : REPEAT IPG=1,NPG : EPSP(IPG,IPT)'
                WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') '/INISHE/EPSP_F'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .      '#  SHELLID, NPT, NPG, THK'
                CALL STRS_TXT50(LINE,100) 
                WRITE(LINE,'(A)')
     .'# REPEAT IPT=1,MAX(1,NPT) : REPEAT IPG=1,NPG : EPSP(IPG,IPT)'
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
            J = J + 6
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(3I10,1PE20.13)')ID,NPT,NPG,THK
            ELSE
              WRITE(LINE,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              CALL STRS_TXT50(LINE,100)
            ENDIF
            MPT  = IABS(NPT)
            NPTM = MAX(1,MPT)
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,NPTM*NPG)
            ELSE
              CALL TAB_STRS_TXT50(WAP0,NPTM*NPG,J,SIZP0,5)
            ENDIF
          ENDIF  !  IF (IOFF == 1)
        ENDDO  !  DO N=1,STAT_NUMELC_G      
      ENDIF  !  IF (ISPMD == 0 .AND. LEN > 0)

C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELTG==0) GOTO 300
C
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
C
          DO I=LFT,LLT
            N = I + NFT
C
            IPRT=IPARTTG(N)
            IF (IPART_STATE(IPRT) == 0) CYCLE
C
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
c
            IF (NLAY > 1) THEN
              DO K=1,NLAY                                              
                BUFLY => ELBUF_TAB(NG)%BUFLY(K)
                NPTT = BUFLY%NPTT
                IF (BUFLY%L_PLA > 0) THEN
                  DO IT=1,NPTT
                    DO IR=1,NPTR                                              
                      DO IS=1,NPTS
                        JJ = JJ + 1
                        WA(JJ) = BUFLY%LBUF(IR,IS,IT)%PLA(I)
                      ENDDO                                                    
                    ENDDO
                  ENDDO
                ELSE
                  DO IT=1,NPTT
                    DO IR=1,NPTR                                               
                      DO IS=1,NPTS                                             
                        JJ = JJ + 1          
                        WA(JJ)=ZERO          
                      ENDDO                                                     
                    ENDDO
                  ENDDO                    
                ENDIF
              ENDDO 
c           NLAY = 1 :
            ELSEIF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA > 0) THEN
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)
              NPTT = BUFLY%NPTT
              DO IT=1,NPTT                                              
                DO IR=1,NPTR                                                
                  DO IS=1,NPTS                                              
                    JJ = JJ + 1                              
                    WA(JJ) = BUFLY%LBUF(IR,IS,IT)%PLA(I)        
                  ENDDO    
                ENDDO                                        
              ENDDO
            ELSE                       
              DO K=1,NPTT                                              
                DO IR=1,NPTR                                                
                  DO IS=1,NPTS                                              
                    JJ = JJ + 1          
                    WA(JJ) = ZERO        
                  ENDDO                                                      
                ENDDO                    
              ENDDO
            ENDIF  !  IF (NLAY > 1)
C
            IE=IE+1
C         pointeur de fin de zone
            PTWA(IE)=JJ
          ENDDO  !  I=LFT,LLT
        ENDIF  !  IF (ITY == 7)
      ENDDO  !  DO NG=1,NGROUP
C
 300  CONTINUE
C
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
C
      IF(ISPMD == 0.AND.LEN>0) THEN
C
        IPRT0=0
        DO N=1,STAT_NUMELTG_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXTG(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF = NINT(WAP0(J + 1))
          IF (IOFF == 1) THEN
            IPRT = NINT(WAP0(J + 2)) 
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISH3/EPSP_F'
                WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
                WRITE(IUGEO,'(A)')
     .'# REPEAT IPT=1,MAX(1,NPT) : REPEAT IPG=1,NPG : EPSP(IPG,IPT)'
                WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISH3/EPSP_F'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# REPEAT IPT=1,MAX(1,NPT) : REPEAT IPG=1,NPG : EPSP(IPG,IPT)'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF  !  IF (IZIPSTRS == 0)
              IPRT0=IPRT
            ENDIF  ! IF (IPRT /= IPRT0)
            ID  = NINT(WAP0(J + 3)) 
            NPT = NINT(WAP0(J + 4)) 
            NPG = NINT(WAP0(J + 5)) 
            THK = WAP0(J + 6) 
            J = J + 6
            IF (IZIPSTRS == 0) THEN 
              WRITE(IUGEO,'(3I10,1PE20.13)')ID,NPT,NPG,THK
            ELSE
              WRITE(LINE,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              CALL STRS_TXT50(LINE,100)
            ENDIF
            MPT  = IABS(NPT)
            NPTM = MAX(1,MPT)
            IF (IZIPSTRS == 0) THEN 
              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,NPTM*NPG)
            ELSE
              CALL TAB_STRS_TXT50(WAP0,NPTM*NPG,J,SIZP0,5)
            ENDIF
          ENDIF  !  IF (IOFF == 1)
        ENDDO  !  DO N=1,STAT_NUMELTG_G      
      ENDIF  !  IF(ISPMD == 0.AND.LEN>0)
C
      RETURN
      END
