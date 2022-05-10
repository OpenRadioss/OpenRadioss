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
Chd|  STAT_C_ORTH_LOC               source/output/sta/stat_c_orth_loc.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        CLSCONV3                      source/output/anim/generate/dfuncc.F
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_ORTH_LOC(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXC ,
     2                           IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                           IPART_STATE,STAT_INDXC,STAT_INDXTG,X,IDEL,
     4                           SIZP0)
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
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .        STAT_INDXC(*), STAT_INDXTG(*)
      my_real
     .   X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,LEN, IOFF, IREP,NG, NEL, NFT, ITY, LFT, NPT,
     .        LLT, MLW,NDIR,NLAY,IHBE,ISH3N,
     .        IGTYP, ID, IPRT0, IPRT,NPG,IPG,IE, FLAGDEG,IDEL,ILAY,ILAW
      INTEGER PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G)),
     .        PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),IFRAM_OLD
      my_real
     .   THK, EM, EB, H1, H2, H3, ANGLE1,
     .   ANGLE2,DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,S1,S2,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA
      my_real 
     .   E1X, E1Y, E1Z, E2X,E2Y, E2Z, E3X, E3Y, E3Z, RX,RY,RZ,SX,SY,SZ,
     .   X2L
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
C-----------------------------------------------
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      FLAGDEG = 1
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
         NPT  = IPARG(6,NG)
         IGTYP= IPARG(38,NG)
         IHBE = IPARG(23,NG)
         NPG  = ELBUF_TAB(NG)%NPTR*ELBUF_TAB(NG)%NPTS
         IREP = IPARG(35,NG)
         NLAY = ELBUF_TAB(NG)%NLAY
         NDIR = 1
         IF (IREP > 1) NDIR = 2                  
         LFT=1
         LLT=NEL
c------------------------------------------
         DO I=LFT,LLT
          N  = I + NFT  
C
          X21 = X(1,IXC(3,N))-X(1,IXC(2,N))               
          X32 = X(1,IXC(4,N))-X(1,IXC(3,N))                  
          X34 = X(1,IXC(4,N))-X(1,IXC(5,N))                  
          X41 = X(1,IXC(5,N))-X(1,IXC(2,N))                  
C
          Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))
          Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))
          Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))
          Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))
C
          Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))
          Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))
          Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))
          Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))
C
          E1X = (X21+X34)                  
          E1Y = (Y21+Y34)                   
          E1Z = (Z21+Z34)                   
C
          E2X = (X32+X41)                   
          E2Y = (Y32+Y41)                   
          E2Z = (Z32+Z41)                   
C
          E3X = E1Y*E2Z-E1Z*E2Y  
          E3Y = E1Z*E2X-E1X*E2Z  
          E3Z = E1X*E2Y-E1Y*E2X
          IF (IREP > 0) THEN
            RX = E1X
            RY = E1Y
            RZ = E1Z
            SX = E2X
            SY = E2Y
            SZ = E2Z
          ENDIF
          IF (ISHFRAM == 0 .OR. IGTYP == 16) THEN
C----       Repere convecte symetrique - version 5 (default)
            SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z
            SUMA   = UN / MAX(SQRT(SUMA),EM20)                  
            E3X = E3X * SUMA                            
            E3Y = E3Y * SUMA                            
            E3Z = E3Z * SUMA                            
C
            S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z 
            S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z 
            SUMA   = SQRT(S1/S2)                
            E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA
            E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA
            E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA
C
            SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z  
            SUMA   = UN / MAX(SQRT(SUMA),EM20)                  
            E1X = E1X * SUMA
            E1Y = E1Y * SUMA
            E1Z = E1Z * SUMA
C
            E2X = E3Y * E1Z - E3Z * E1Y
            E2Y = E3Z * E1X - E3X * E1Z
            E2Z = E3X * E1Y - E3Y * E1X
          ELSEIF (ISHFRAM == 2) THEN
C----       Repere convecte nonsymetrique - version 4
            SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z
            E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y
            E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z
            E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X
            SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z
            SUMA   = UN/MAX(SQRT(SUMA),EM20)
            E1X = E1X*SUMA
            E1Y = E1Y*SUMA
            E1Z = E1Z*SUMA
C
            SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z
            SUMA   = UN / MAX(SQRT(SUMA),EM20)                  
            E3X = E3X * SUMA                            
            E3Y = E3Y * SUMA                            
            E3Z = E3Z * SUMA                            
C
            E2X = E3Y*E1Z-E3Z*E1Y
            E2Y = E3Z*E1X-E3X*E1Z
            E2Z = E3X*E1Y-E3Y*E1X
            SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z
            SUMA   = UN/MAX(SQRT(SUMA),EM20)
            E2X = E2X*SUMA
            E2Y = E2Y*SUMA
            E2Z = E2Z*SUMA
          ENDIF
C
          IPRT=IPARTC(N)
          IF (IPART_STATE(IPRT) == 0) CYCLE
C
          JJ = JJ + 1
          IF (MLW /=  0 .AND. MLW /=  13) THEN
            WA(JJ) = GBUF%OFF(I)
          ELSE
            WA(JJ) = ZERO
          ENDIF
          JJ = JJ + 1
          WA(JJ) = IPRT
          JJ = JJ + 1
          WA(JJ) = IXC(NIXC,N)
          JJ = JJ + 1
          WA(JJ) = NPT
          JJ = JJ + 1
          WA(JJ) = NPG
          JJ = JJ + 1
          WA(JJ) = IHBE
          JJ = JJ + 1
          WA(JJ) = IGTYP
          JJ = JJ + 1
          WA(JJ) = NDIR
          JJ = JJ + 1
          WA(JJ) = IREP
          IF (IGTYP == 9  .OR. IGTYP == 10 .OR. IGTYP == 11 .OR.
     .        IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR.
     .        IGTYP == 52) THEN
            DO J=1,NLAY                                                            
              DIR1_1 = ELBUF_TAB(NG)%BUFLY(J)%DIRA(I)
              DIR1_2 = ELBUF_TAB(NG)%BUFLY(J)%DIRA(I + NEL)
              ILAW = ELBUF_TAB(NG)%BUFLY(J)%ILAW
              IF (IREP == 1) THEN
                AA = DIR1_1
                BB = DIR1_2
                V1 = AA*RX + BB*SX         
                V2 = AA*RY + BB*SY         
                V3 = AA*RZ + BB*SZ         
                VR = V1*E1X+ V2*E1Y + V3*E1Z
                VS = V1*E2X+ V2*E2Y + V3*E2Z
                SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                DIR1_1 = VR/SUMA                                                   
                DIR1_2 = VS/SUMA
              ELSEIF (IREP == 2) THEN
C---        Axe I
                AA = DIR1_1
                BB = DIR1_2
                V1 = AA*RX + BB*SX         
                V2 = AA*RY + BB*SY         
                V3 = AA*RZ + BB*SZ         
                VR = V1*E1X+ V2*E1Y + V3*E1Z
                VS = V1*E2X+ V2*E2Y + V3*E2Z
                SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                DIR1_1 = VR/SUMA                                                   
                DIR1_2 = VS/SUMA
C---        Axe II
                AA = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I)	 			    
                BB = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I + NEL)			   
                V1 = AA*RX + BB*SX                                               
                V2 = AA*RY + BB*SY                                               
                V3 = AA*RZ + BB*SZ                                               
                VR = V1*E1X+ V2*E1Y + V3*E1Z                                     
                VS = V1*E2X+ V2*E2Y + V3*E2Z                                     
                SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                DIR2_1 = VR/SUMA                                                 
                DIR2_2 = VS/SUMA
              ELSEIF (IREP == 3) THEN
C   mixing law58 with other user laws with IREP = 0 within PID51
                IF (ILAW == 58) THEN
C---        Axe I
                  AA = DIR1_1
                  BB = DIR1_2
                  V1 = AA*RX + BB*SX         
                  V2 = AA*RY + BB*SY         
                  V3 = AA*RZ + BB*SZ         
                  VR = V1*E1X+ V2*E1Y + V3*E1Z
                  VS = V1*E2X+ V2*E2Y + V3*E2Z
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR1_1 = VR/SUMA                                                   
                  DIR1_2 = VS/SUMA
C---        Axe II
                  AA = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I)	   			       
                  BB = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I + NEL)			      
                  V1 = AA*RX + BB*SX                                               
                  V2 = AA*RY + BB*SY                                               
                  V3 = AA*RZ + BB*SZ                                               
                  VR = V1*E1X+ V2*E1Y + V3*E1Z                                     
                  VS = V1*E2X+ V2*E2Y + V3*E2Z                                     
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR2_1 = VR/SUMA                                                 
                  DIR2_2 = VS/SUMA
                ELSE  ! IREP = 0 within PID51
C     DIR1_1 and DIT1_2 already got
                ENDIF
              ELSEIF (IREP == 4) THEN
C   mixing law58 with other user laws with IREP = 1 within PID51
                IF (ILAW == 58) THEN
C---        Axe I
                  AA = DIR1_1
                  BB = DIR1_2
                  V1 = AA*RX + BB*SX         
                  V2 = AA*RY + BB*SY         
                  V3 = AA*RZ + BB*SZ         
                  VR = V1*E1X+ V2*E1Y + V3*E1Z
                  VS = V1*E2X+ V2*E2Y + V3*E2Z
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR1_1 = VR/SUMA                                                   
                  DIR1_2 = VS/SUMA
C---        Axe II
                  AA = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I)	   			       
                  BB = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I + NEL)			      
                  V1 = AA*RX + BB*SX                                               
                  V2 = AA*RY + BB*SY                                               
                  V3 = AA*RZ + BB*SZ                                               
                  VR = V1*E1X+ V2*E1Y + V3*E1Z                                     
                  VS = V1*E2X+ V2*E2Y + V3*E2Z                                     
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR2_1 = VR/SUMA                                                 
                  DIR2_2 = VS/SUMA
                ELSE  ! IREP = 1 within PID51
                  AA = DIR1_1
                  BB = DIR1_2
                  V1 = AA*RX + BB*SX         
                  V2 = AA*RY + BB*SY         
                  V3 = AA*RZ + BB*SZ         
                  VR = V1*E1X+ V2*E1Y + V3*E1Z
                  VS = V1*E2X+ V2*E2Y + V3*E2Z
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR1_1 = VR/SUMA                                                   
                  DIR1_2 = VS/SUMA
                ENDIF
              ENDIF
C
              JJ = JJ + 1
              WA(JJ) = ILAW
C  charging all directions
              JJ = JJ + 1                                                          
              IF (MLW /= 0 .AND. MLW /= 13) THEN                               
                WA(JJ) = DIR1_1                                                    
              ELSE                                                                 
                WA(JJ) = ZERO                                                      
              ENDIF                                                                
              JJ = JJ + 1                                                          
              IF (MLW /= 0 .AND. MLW /= 13) THEN                               
                WA(JJ) = DIR1_2                                                    
              ELSE                                                                 
                WA(JJ) = ZERO                                                      
              ENDIF
              IF (IREP > 1 .AND. ILAW == 58) THEN
                JJ = JJ + 1                                                        
                IF (MLW /= 0 .AND. MLW /= 13) THEN                             
                  WA(JJ) = DIR2_1                                                  
                ELSE                                                               
                  WA(JJ) = ZERO                                                    
                ENDIF                                                              
                JJ = JJ + 1                                                        
                IF (MLW /= 0 .AND. MLW /= 13) THEN                             
                  WA(JJ) = DIR2_2                                                  
                ELSE                                                               
                  WA(JJ) = ZERO                                                    
                ENDIF
              ENDIF !  IF (IREP > 1) THEN
            ENDDO  !  DO J=1,NLAY
          ENDIF  !  IF ( IGTYP )
c
          IE=IE+1
C         pointeur de fin de zone dans WA
          PTWA(IE)=JJ
c
         ENDDO  !  DO I=LFT,LLT
       ENDIF  !  IF (ITY == 3)
      ENDDO  !  DO NG=1,NGROUP
C
 200  CONTINUE
c------------------------------------------------------
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
c------------------------------------------------------
      IF (ISPMD == 0.AND.LEN > 0) THEN
        IPRT0=0
        DO N=1,STAT_NUMELC_G

C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXC(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF = NINT(WAP0(J + 1))
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >=1))THEN
           IPRT  = NINT(WAP0(J + 2)) 
           IF (IPRT /= IPRT0) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(A)') DELIMIT
              WRITE(IUGEO,'(A)')'/INISHE/ORTH_LOC'
              WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
              WRITE(IUGEO,'(A)')
     .   '#  SHELLID       NIP                NDIR'
              WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
              WRITE(IUGEO,'(A)') DELIMIT
            ELSE
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')'/INISHE/ORTH_LOC'
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------'
              CALL STRS_TXT50(LINE,100)  
              WRITE(LINE,'(A)')
     .   '#  SHELLID       NIP                NDIR'
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100) 
            ENDIF
            IPRT0=IPRT
          ENDIF  ! IF (IPRT /= IPRT0)
          ID    = NINT(WAP0(J + 3)) 
          NPT   = NINT(WAP0(J + 4)) 
          NPG   = NINT(WAP0(J + 5)) 
          IHBE  = NINT(WAP0(J + 6)) 
          IGTYP = NINT(WAP0(J + 7))
          NDIR  = NINT(WAP0(J + 8))
          IREP  = NINT(WAP0(J + 9))
          J = J + 9
          IF (IGTYP == 9) THEN
            NPT = 1
C   skip ILAW = WAP0(J + 1)  ! not used for IGTYP = 9
            J = J + 1
            ANGLE1 = ATAN2(WAP0(J + 2), WAP0(J + 1))
            IF(FLAGDEG == 1) ANGLE1=ANGLE1*CENT80/PI
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
              WRITE(IUGEO,'(1PE20.13)')ANGLE1
            ELSE
              WRITE(LINE,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(1PE20.13)')ANGLE1
              CALL STRS_TXT50(LINE,20)
            ENDIF
            J = J + 2
          ELSEIF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16.OR.
     .            IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
            ELSE
              WRITE(LINE,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
              CALL STRS_TXT50(LINE,100)
            ENDIF
            DO I=1,NPT
C---
              ILAW = WAP0(J + 1)
              J = J + 1
C---
              IF (IREP == 2 .OR. (IREP > 2 .AND. ILAW == 58)) THEN
C IGTYP = 16 or (IGTYP = 51 + ILAW 58)
                ANGLE1 = ATAN2(WAP0(J + 2), WAP0(J + 1))
                ANGLE2 = ATAN2(WAP0(J + 4), WAP0(J + 3))
                ANGLE2 = MOD(ANGLE2 - ANGLE1,DEUX*PI)
                IF (FLAGDEG == 1) ANGLE1=ANGLE1*CENT80/PI
                IF (FLAGDEG == 1) ANGLE2=ANGLE2*CENT80/PI
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P2E20.13)')ANGLE1,ANGLE2
                ELSE
                  WRITE(LINE,'(1P2E20.13)')ANGLE1,ANGLE2
                  CALL STRS_TXT50(LINE,40)
                ENDIF
                J = J + 4
              ELSE
                ANGLE1 = ATAN2(WAP0(J + 2), WAP0(J + 1))
                IF (FLAGDEG == 1) ANGLE1=ANGLE1*CENT80/PI
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1PE20.13)')ANGLE1
                ELSE
                  WRITE(LINE,'(1PE20.13)')ANGLE1
                  CALL STRS_TXT50(LINE,20)
                ENDIF
                J = J + 2
              ENDIF ! IF (IREP == 2 .OR. (IREP > 2 .AND. ILAW == 58))
C---------------
            ENDDO  !  DO I=1,NPT
          ENDIF  !  IF (IGTYP)
          ENDIF
        ENDDO  ! DO N=1,STAT_NUMELC_G
      ENDIF  ! IF (ISPMD == 0.AND.LEN > 0)
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
         NPT  = IPARG(6,NG)
         ISH3N= IPARG(23,NG)
         IGTYP= IPARG(38,NG)
         NPG  = ELBUF_TAB(NG)%NPTR*ELBUF_TAB(NG)%NPTS
         IREP = IPARG(35,NG)
         NLAY = ELBUF_TAB(NG)%NLAY
         NDIR = 1
         IF (ISH3N==3.AND.ISH3NFRAM==0) THEN
          IFRAM_OLD =0
         ELSE
          IFRAM_OLD =1
         END IF
         IF (IREP > 1) NDIR = 2
         LFT=1
         LLT=NEL
c------------------------------------------
         DO I=LFT,LLT
          N  = I + NFT
C
          X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))               
          X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                  
          X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                  
C
          Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))
          Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))
          Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))
C
          Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))
          Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))
          Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))
C
          IF (IREP > 0) THEN
cc            SX = X21
cc            SY = Y21
cc            SZ = Z21
cc            RX = X31
cc            RY = Y31
cc            RZ = Z31
            RX = X21
            RY = Y21
            RZ = Z21
            SX = X31
            SY = Y31
            SZ = Z31
          ENDIF
         IF(IFRAM_OLD ==0 ) THEN
	    CALL CLSCONV3(X21,Y21,Z21,X31,Y31,Z31,
     +	                     E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z)
         ELSE		
          E1X= X21
          E1Y= Y21
          E1Z= Z21
          X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)
          E1X=E1X/X2L
          E1Y=E1Y/X2L
          E1Z=E1Z/X2L
C
          E3X=Y31*Z32-Z31*Y32
          E3Y=Z31*X32-X31*Z32
          E3Z=X31*Y32-Y31*X32
          SUM = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          E3X=E3X/SUM
          E3Y=E3Y/SUM
          E3Z=E3Z/SUM
          AREA = UNDEMI * SUM
          E2X=E3Y*E1Z-E3Z*E1Y
          E2Y=E3Z*E1X-E3X*E1Z
          E2Z=E3X*E1Y-E3Y*E1X
          SUM = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)
          E2X=E2X/SUM
          E2Y=E2Y/SUM
          E2Z=E2Z/SUM
         END IF !(ISH3NFRAM ==0 ) THEN
C
          IPRT=IPARTTG(N)
          IF (IPART_STATE(IPRT)==0) CYCLE
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
          WA(JJ) = NPT
          JJ = JJ + 1
          WA(JJ) = NPG
          JJ = JJ + 1
          WA(JJ) = ISH3N
          JJ = JJ + 1
          WA(JJ) = IGTYP
          JJ = JJ + 1
          WA(JJ) = NDIR
          JJ = JJ + 1
          WA(JJ) = IREP
          IF (IGTYP == 9  .OR. IGTYP == 10 .OR. IGTYP == 11 .OR.
     .        IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR.
     .        IGTYP == 52 ) THEN
            DO J=1,NLAY                                                            
              DIR1_1 = ELBUF_TAB(NG)%BUFLY(J)%DIRA(I)
              DIR1_2 = ELBUF_TAB(NG)%BUFLY(J)%DIRA(I + NEL)
              ILAW = ELBUF_TAB(NG)%BUFLY(J)%ILAW
              IF (IREP == 1) THEN
                AA = DIR1_1
                BB = DIR1_2
                V1 = AA*RX + BB*SX         
                V2 = AA*RY + BB*SY         
                V3 = AA*RZ + BB*SZ         
                VR = V1*E1X+ V2*E1Y + V3*E1Z
                VS = V1*E2X+ V2*E2Y + V3*E2Z
                SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                DIR1_1 = VR/SUMA                                                   
                DIR1_2 = VS/SUMA
              ELSEIF (IREP == 2) THEN
C---        Axe I
                AA = DIR1_1
                BB = DIR1_2
                V1 = AA*RX + BB*SX         
                V2 = AA*RY + BB*SY         
                V3 = AA*RZ + BB*SZ         
                VR = V1*E1X+ V2*E1Y + V3*E1Z
                VS = V1*E2X+ V2*E2Y + V3*E2Z
                SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                DIR1_1 = VR/SUMA                                                   
                DIR1_2 = VS/SUMA
C---        Axe II
                AA = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I)	 			    
                BB = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I + NEL)			   
                V1 = AA*RX + BB*SX                                               
                V2 = AA*RY + BB*SY                                               
                V3 = AA*RZ + BB*SZ                                               
                VR = V1*E1X+ V2*E1Y + V3*E1Z                                     
                VS = V1*E2X+ V2*E2Y + V3*E2Z                                     
                SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                DIR2_1 = VR/SUMA                                                 
                DIR2_2 = VS/SUMA
              ELSEIF (IREP == 3) THEN
C   mixing law58 with other user laws with IREP = 0 within PID51
                IF (ILAW == 58) THEN
C---        Axe I
                  AA = DIR1_1
                  BB = DIR1_2
                  V1 = AA*RX + BB*SX         
                  V2 = AA*RY + BB*SY         
                  V3 = AA*RZ + BB*SZ         
                  VR = V1*E1X+ V2*E1Y + V3*E1Z
                  VS = V1*E2X+ V2*E2Y + V3*E2Z
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR1_1 = VR/SUMA                                                   
                  DIR1_2 = VS/SUMA
C---        Axe II
                  AA = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I)	   			     
                  BB = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I + NEL)			    
                  V1 = AA*RX + BB*SX                                               
                  V2 = AA*RY + BB*SY                                               
                  V3 = AA*RZ + BB*SZ                                               
                  VR = V1*E1X+ V2*E1Y + V3*E1Z                                     
                  VS = V1*E2X+ V2*E2Y + V3*E2Z                                     
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR2_1 = VR/SUMA                                                 
                  DIR2_2 = VS/SUMA
                ELSE  ! IREP = 0 within PID51
C     DIR1_1 and DIT1_2 already got
                ENDIF
              ELSEIF (IREP == 4) THEN
C   mixing law58 with other user laws with IREP = 1 within PID51
                IF (ILAW == 58) THEN
C---        Axe I
                  AA = DIR1_1
                  BB = DIR1_2
                  V1 = AA*RX + BB*SX         
                  V2 = AA*RY + BB*SY         
                  V3 = AA*RZ + BB*SZ         
                  VR = V1*E1X+ V2*E1Y + V3*E1Z
                  VS = V1*E2X+ V2*E2Y + V3*E2Z
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR1_1 = VR/SUMA                                                   
                  DIR1_2 = VS/SUMA
C---        Axe II
                  AA = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I)	   			      
                  BB = ELBUF_TAB(NG)%BUFLY(J)%DIRB(I + NEL)			     
                  V1 = AA*RX + BB*SX                                               
                  V2 = AA*RY + BB*SY                                               
                  V3 = AA*RZ + BB*SZ                                               
                  VR = V1*E1X+ V2*E1Y + V3*E1Z                                     
                  VS = V1*E2X+ V2*E2Y + V3*E2Z                                     
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR2_1 = VR/SUMA                                                 
                  DIR2_2 = VS/SUMA
                ELSE  ! IREP = 1 within PID51
                  AA = DIR1_1
                  BB = DIR1_2
                  V1 = AA*RX + BB*SX         
                  V2 = AA*RY + BB*SY         
                  V3 = AA*RZ + BB*SZ         
                  VR = V1*E1X+ V2*E1Y + V3*E1Z
                  VS = V1*E2X+ V2*E2Y + V3*E2Z
                  SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                  DIR1_1 = VR/SUMA                                                   
                  DIR1_2 = VS/SUMA
                ENDIF
              ENDIF
C
              JJ = JJ + 1
              WA(JJ) = ILAW
C  charging all directions
              JJ = JJ + 1                                                          
              IF (MLW /= 0 .AND. MLW /= 13) THEN                               
                WA(JJ) = DIR1_1                                                    
              ELSE                                                                 
                WA(JJ) = ZERO                                                      
              ENDIF                                                                
              JJ = JJ + 1                                                          
              IF (MLW /= 0 .AND. MLW /= 13) THEN                               
                WA(JJ) = DIR1_2                                                    
              ELSE                                                                 
                WA(JJ) = ZERO                                                      
              ENDIF
              IF (IREP > 1 .AND. ILAW == 58) THEN
                JJ = JJ + 1                                                        
                IF (MLW /= 0 .AND. MLW /= 13) THEN                             
                  WA(JJ) = DIR2_1                                                  
                ELSE                                                               
                  WA(JJ) = ZERO                                                    
                ENDIF                                                              
                JJ = JJ + 1                                                        
                IF (MLW /= 0 .AND. MLW /= 13) THEN                             
                  WA(JJ) = DIR2_2                                                  
                ELSE                                                               
                  WA(JJ) = ZERO                                                    
                ENDIF
              ENDIF !  IF (IREP > 1) THEN
            ENDDO  !  DO J=1,NLAY
          ENDIF  !  IF ( IGTYP )
c
          IE=IE+1
C         pointeur de fin de zone dans WA
          PTWA(IE)=JJ
c
         ENDDO  !  DO I=LFT,LLT
       ENDIF  !  IF (ITY == 7)
      ENDDO  !  DO NG=1,NGROUP
C
 300  CONTINUE
c-----------------------------------------------------
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
c-----------------------------------------------------
      IF (ISPMD == 0.AND.LEN > 0) THEN
C
        IPRT0=0
        DO N=1,STAT_NUMELTG_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXTG(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF  = NINT(WAP0(J + 1))
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >=1))THEN
          IPRT  = NINT(WAP0(J + 2)) 
          IF (IPRT /= IPRT0) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(A)') DELIMIT
              WRITE(IUGEO,'(A)')'/INISH3/ORTH_LOC'
              WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
              WRITE(IUGEO,'(A)')
     .   '#  SHELLID       NIP                NDIR'
              WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
              WRITE(IUGEO,'(A)') DELIMIT
            ELSE
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')'/INISH3/ORTH_LOC'
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------' 
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .   '#  SHELLID       NIP                NDIR'
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------'
              CALL STRS_TXT50(LINE,100)  
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100) 
            ENDIF  !  IF (IZIPSTRS == 0)
            IPRT0=IPRT
          ENDIF  ! IF (IPRT /= IPRT0)
          ID    = NINT(WAP0(J + 3)) 
          NPT   = NINT(WAP0(J + 4)) 
          NPG   = NINT(WAP0(J + 5)) 
          ISH3N = NINT(WAP0(J + 6)) 
          IGTYP = NINT(WAP0(J + 7))
          NDIR  = NINT(WAP0(J + 8))
          IREP  = NINT(WAP0(J + 9))
          J = J + 9
          IF (IGTYP == 9) THEN
            NPT = 1
C   skip ILAW = WAP0(J + 1)  ! not used for IGTYP = 9
            J = J + 1
            ANGLE1 = ATAN2(WAP0(J + 2), WAP0(J + 1))
            IF (FLAGDEG == 1) ANGLE1=ANGLE1*CENT80/PI
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
              WRITE(IUGEO,'(1PE20.13)')ANGLE1
            ELSE
              WRITE(LINE,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(1PE20.13)')ANGLE1
              CALL STRS_TXT50(LINE,20)
            ENDIF
            J = J + 2
          ELSEIF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR.
     .            IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
            ELSE
              WRITE(LINE,'(5I10)')ID,NPT,NPG,NDIR,FLAGDEG
              CALL STRS_TXT50(LINE,100)
            ENDIF
            DO I=1,NPT
              ILAW = WAP0(J + 1)
              J = J + 1
              IF (IREP == 2 .OR. (IREP > 2 .AND. ILAW == 58)) THEN
C IGTYP = 16 or (IGTYP = 51 + ILAW 58)
                ANGLE1 = ATAN2(WAP0(J + 2), WAP0(J + 1))
                ANGLE2 = ATAN2(WAP0(J + 4), WAP0(J + 3))
                ANGLE2 = MOD(ANGLE2 - ANGLE1,DEUX*PI)
                IF (FLAGDEG == 1) ANGLE1=ANGLE1*CENT80/PI
                IF (FLAGDEG == 1) ANGLE2=ANGLE2*CENT80/PI
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P2E20.13)')ANGLE1,ANGLE2
                ELSE
                  WRITE(LINE,'(1P2E20.13)')ANGLE1,ANGLE2
                  CALL STRS_TXT50(LINE,40)
                ENDIF
                J = J + 4
              ELSE
                ANGLE1 = ATAN2(WAP0(J + 2), WAP0(J + 1))
                IF (FLAGDEG == 1) ANGLE1=ANGLE1*CENT80/PI
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1PE20.13)')ANGLE1
                ELSE
                  WRITE(LINE,'(1PE20.13)')ANGLE1
                  CALL STRS_TXT50(LINE,20)
                ENDIF
                J = J + 2
              ENDIF
            ENDDO  ! DO I=1,NPT
          ENDIF  !  IF (IGTYP == 9)
         ENDIF
        ENDDO  !  DO N=1,STAT_NUMELTG_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
c----------
      RETURN
      END
