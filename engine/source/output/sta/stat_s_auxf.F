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
Chd|  STAT_S_AUXF                   source/output/sta/stat_s_auxf.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE STAT_S_AUXF(ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXS(NIXS,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTS(*), IPART_STATE(*), STAT_INDXS(*),IPART(LIPART1,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,JJ,LEN,ISOLNOD,IUS,NLAY,NPTR,NPTS,NPTT,NPTG,
     .        NG, NEL, MLW,ID, IPRT0, IPRT, NPG,IPG,IPT, NUVAR,IE,
     .        IL,IR,IS,IT,PID,IOFF
      INTEGER PTWA(STAT_NUMELS), PTWA_P0(0:MAX(1,STAT_NUMELS_G))
      CHARACTER*100 DELIMIT,LINE
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C----  
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF     
C-----------------------------------------------
C     8 NODES BRICK
C======================================================================|
      JJ = 0
      IE = 0
      IF(STAT_NUMELS==0) GOTO 200
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        MLW   =IPARG(1,NG)  
        NEL   =IPARG(2,NG)    
        NFT   =IPARG(3,NG)   
        IAD   =IPARG(4,NG)
        LFT=1
        LLT = NEL
        IPRT=IPARTS(LFT+NFT)
        PID = IPART(2,IPRT)
c
        IF (ITY == 1) THEN
          CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          IPRT=IPARTS(LFT+NFT)
          PID = IPART(2,IPRT)
C          JHBE   = IGEO(10,PID)
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
          NLAY = ELBUF_TAB(NG)%NLAY                       
          NPTR = ELBUF_TAB(NG)%NPTR                        
          NPTS = ELBUF_TAB(NG)%NPTS                        
          NPTT = ELBUF_TAB(NG)%NPTT                        
          NPT  = NPTR * NPTS * NPTT * NLAY
          IF (JHBE==17.AND.IINT==2) JHBE = 18
          IF (MLW < 28) THEN 
            NUVAR = 0
          ELSE
            NUVAR = IPM(8,IXS(1,NFT+1))   
          ENDIF
c-------------------------------
          IF (ISOLNOD == 16) THEN
c
            DO I=LFT,LLT                                       
              N = I + NFT                                      
              IPRT=IPARTS(N)                                   
              IF (IPART_STATE(IPRT)==0) CYCLE                    
              WA(JJ+1) = GBUF%VOL(I)             
              WA(JJ+2) = IPRT                                  
              WA(JJ+3) = IXS(NIXS,N)                           
              WA(JJ+4) = NLAY                                          
              WA(JJ+5) = NPTR                                
              WA(JJ+6) = NPTS                                
              WA(JJ+7) = NPTT                                
              WA(JJ+8) = ISOLNOD                                
              WA(JJ+9) = NUVAR                                  
              WA(JJ+10)= JHBE   
              WA(JJ+11) = GBUF%OFF(I)                                                                                                                 
              JJ = JJ + 11  
              IS = 1 
              DO IT=1,NPTT                                         
               DO IL=1,NLAY                                               
                DO IR=1,NPTR                                             
                   MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                   DO IUS = 1,NUVAR   
                     WA(JJ + 1) = MBUF%VAR(I + (IUS-1)*NEL)
                     JJ = JJ +1                                     
                   ENDDO                                             
                ENDDO                                                 
               ENDDO                                                    
              ENDDO                 
              IE=IE+1                          
C             pointeur de fin de zone dans WA  
              PTWA(IE)=JJ                      
            ENDDO  !  I=LFT,LLT                                     
          ELSE
            DO I=LFT,LLT                                       
              N = I + NFT                                      
              IPRT=IPARTS(N)                                   
              IF (IPART_STATE(IPRT)==0) CYCLE                    
              WA(JJ+1) = GBUF%VOL(I)             
              WA(JJ+2) = IPRT                                  
              WA(JJ+3) = IXS(NIXS,N)                           
              WA(JJ+4) = NLAY                                          
              WA(JJ+5) = NPTR                                
              WA(JJ+6) = NPTS                                
              WA(JJ+7) = NPTT                                
              WA(JJ+8) = ISOLNOD                                
              WA(JJ+9) = NUVAR                                  
              WA(JJ+10)= JHBE   
              WA(JJ+11) = GBUF%OFF(I)                                                                                                                 
              JJ = JJ + 11  
              DO IL=1,NLAY                                               
               DO IT=1,NPTT
                DO IS=1,NPTS
                 DO IR=1,NPTR
                    MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                    DO IUS = 1,NUVAR   
                      WA(JJ + 1) = MBUF%VAR(I + (IUS-1)*NEL)
                      JJ = JJ +1                                     
                    ENDDO                                             
                 ENDDO                                                   
                ENDDO                                                    
               ENDDO                 
              ENDDO                 
              IE=IE+1                          
C             pointeur de fin de zone dans WA  
              PTWA(IE)=JJ                      
            ENDDO  !  I=LFT,LLT                                     
          ENDIF !  ISOLNOD == 16
        ENDIF    !  ITY = 1
      ENDDO
 200  CONTINUE
c------------------------------------------------------------
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
c------------------------------------------------------------
      IF(ISPMD==0.AND.LEN>0) THEN
        IPRT0=0
        DO N=1,STAT_NUMELS_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXS(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
          IPRT  = NINT(WAP0(J + 2))
          IOFF    = NINT(WAP0(J + 11)) 
          IF (IOFF >= 1) THEN              
            IF(IPRT /= IPRT0)THEN 
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(A)') DELIMIT
               WRITE(IUGEO,'(A)')'/INIBRI/AUX'
               WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
               WRITE(IUGEO,'(A)')
     .      '#  BRICKID       NPT' 
               WRITE(IUGEO,'(A/A/A)')
     .'# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :',
     .'#    S1,  S2,  S3',
     .'#   S12, S23, S31'
               WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
               WRITE(IUGEO,'(A)') DELIMIT
             ELSE
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'/INIBRI/AUX'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------' 
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .      '#  BRICKID       NPT' 
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .'# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'#    S1,  S2,  S3'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'#   S12, S23, S31'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100) 
             END IF
             IPRT0=IPRT
            END IF
c
            ID     = NINT(WAP0(J + 3)) 
            NLAY   = NINT(WAP0(J+4))                                
            NPTR   = NINT(WAP0(J+5))                      
            NPTS   = NINT(WAP0(J+6))                      
            NPTT   = NINT(WAP0(J+7))                      
            ISOLNOD= NINT(WAP0(J+8))                         
            NUVAR  = NINT(WAP0(J+9))                         
            JHBE   = NINT(WAP0(J+10))
            NPTG   = NLAY*NPTR*NPTS*NPTT
            J = J + 11
c
            IF(ISOLNOD==8.AND.JHBE==14 )THEN 
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(7I10)') ID,NPTG,ISOLNOD,JHBE,0,0,NUVAR
              ELSE
                WRITE(LINE,'(7I10)') ID,NPTG,ISOLNOD,JHBE,0,0,NUVAR
                CALL STRS_TXT50(LINE,100)
              ENDIF 
              IF (NUVAR /= 0) THEN
               IF (IZIPSTRS == 0) THEN
                DO IPT=1,NPTG
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,NUVAR)
                  J = J + NUVAR
                ENDDO
               ELSE
                DO IPT=1,NPTG
                  CALL TAB_STRS_TXT50(WAP0(1),NUVAR,J,SIZP0,3)
                  J = J + NUVAR
                ENDDO
               ENDIF
              ENDIF
            ELSEIF(ISOLNOD==8 .OR. ISOLNOD==6 .OR. ISOLNOD==4 .OR.
     .             ISOLNOD==10 .OR. ISOLNOD==16 .OR. ISOLNOD==20)THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(7I10)') ID,NPTG,ISOLNOD,JHBE,0,0,NUVAR
              ELSE
                WRITE(LINE,'(7I10)') ID,NPTG,ISOLNOD,JHBE,0,0,NUVAR
                CALL STRS_TXT50(LINE,100)
              ENDIF 
              IF (NUVAR /= 0) THEN
               IF (IZIPSTRS == 0) THEN
                DO IPT=1,NPTG
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,NUVAR)
                  J = J + NUVAR
                ENDDO
               ELSE
                DO IPT=1,NPTG
                  CALL TAB_STRS_TXT50(WAP0(1),NUVAR,J,SIZP0,3)
                  J = J + NUVAR
                ENDDO
               ENDIF
              ENDIF
            ENDIF
         ENDIF  !  IF (IOFF == 1)              
        ENDDO		    
      ENDIF
c-----------
      RETURN
      END
