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
Chd|  STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        SROTORTH                      source/elements/solid/srotorth.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE STAT_S_ORTHO(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,
     3                        STAT_INDXS ,X,IGLOB ,IPART,IDEL ,SIZP0)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0,IGLOB,IDEL
      INTEGER IXS(NIXS,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTS(*), IPART_STATE(*), STAT_INDXS(*),IPART(LIPART1,*)
      my_real
     .   X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,II(6),JJ,LEN,ISOLNOD,IUS, NPTR, NPTS, NPTT, NPTG,
     .        NG, NEL, MLW,ID, IPRT0, IPRT, NPG,IPG,IPT, NUVAR,IE,
     .        NLAY,IP,IL,IR,IS,IT,PID,ICSIG,IOFF
      INTEGER PTWA(STAT_NUMELS), PTWA_P0(0:MAX(1,STAT_NUMELS_G))
      my_real
     .   GAMA(6)
      CHARACTER*100 DELIMIT,LINE
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C----  
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C-----------------------------------------------
C     8 NODES BRICK
C======================================================================|
      JJ = 0
      IF(STAT_NUMELS==0) GOTO 200

      IE=0
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        MLW   =IPARG(1,NG)  
        NEL   =IPARG(2,NG)    
        NFT   =IPARG(3,NG)   
        IAD   =IPARG(4,NG) 
        ICSIG =IPARG(17,NG)
        LFT=1
        LLT = NEL
c
        IF (ITY == 1) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

          IPRT=IPARTS(LFT+NFT)
          PID = IPART(2,IPRT)
C          JHBE   = IGEO(10,PID)
c
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          IF(IGTYP == 22) THEN
             NLAY = ELBUF_TAB(NG)%NLAY  
          ELSE
            NLAY = 1
          ENDIF                        
          NPTR = ELBUF_TAB(NG)%NPTR                        
          NPTS = ELBUF_TAB(NG)%NPTS                        
          NPTT = ELBUF_TAB(NG)%NPTT                        
          NPT  = NPTR * NPTS * NPTT * NLAY
!
          DO J=1,6
            II(J) = NEL*(J-1)
          ENDDO
!
c
          DO I=LFT,LLT
            N = I + NFT 
            IPRT=IPARTS(N)
            IF(IPART_STATE(IPRT)==0)CYCLE
c
            WA(JJ+ 1)= IPRT
            WA(JJ+ 2)= IXS(NIXS,N)
            WA(JJ+ 3)= ISORTH
            WA(JJ+ 4)= NLAY                                           
            WA(JJ+ 5)= NPTR                                 
            WA(JJ+ 6)= NPTS                                 
            WA(JJ+ 7)= NPTT                                 
            WA(JJ+ 8)= ISOLNOD                                              
            WA(JJ+ 9)= JHBE                                             
            WA(JJ+10)= IGTYP
            WA(JJ+11)= GBUF%OFF(I)
            JJ = JJ + 11                                    
c
            DO J=1,NLAY
              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(1,1,1)
              IF (ISORTH == 1) THEN
                IF(IGTYP == 21 .OR. IGTYP == 22) THEN
                  IF (IGLOB == 1)THEN        ! Orthotropic directions in global reference
                   IF (IGTYP == 22) THEN                   
                      GAMA(1)= LBUF%GAMA(II(1)+I)
                      GAMA(2)= LBUF%GAMA(II(2)+I)
                    ELSEIF (IGTYP == 21) THEN  
                      GAMA(1)= GBUF%GAMA(II(1)+I)
                      GAMA(2)= GBUF%GAMA(II(2)+I)
                    ENDIF           
                    GAMA(3)= ZERO
                    GAMA(4)= ZERO
                    GAMA(5)= ZERO
                    GAMA(6)= ZERO
                    CALL SROTORTH(X,IXS(1,N),GAMA,JHBE,IGTYP,
     .                           ICSIG)     ! Transformation : COS(PHI), SIN(PHI) -> Orthotropic Axes in global reference
                    WA(JJ+1)=GAMA(1)
                    WA(JJ+2)=GAMA(2)
                    WA(JJ+3)=GAMA(3)
                    WA(JJ+4)=GAMA(4)
                    WA(JJ+5)=GAMA(5)
                    WA(JJ+6)=GAMA(6)                  
                  ELSE 
                   IF (IGTYP == 22) THEN 
                     WA(JJ+1)= LBUF%GAMA(II(1)+I)
                     WA(JJ+2)= LBUF%GAMA(II(2)+I)
                    ELSEIF (IGTYP == 21) THEN    
                      WA(JJ+1)= GBUF%GAMA(II(1)+I)
                      WA(JJ+2)= GBUF%GAMA(II(2)+I)
                    ENDIF
                      WA(JJ+3)= ZERO
                      WA(JJ+4)= ZERO
                      WA(JJ+5)= ZERO
                      WA(JJ+6)= ZERO
                 ENDIF
                ELSEIF (JHBE == 1 .OR. 
     .                  JHBE == 2  .OR. JHBE == 12) THEN
                  WA(JJ+1)= GBUF%GAMA(II(1)+I)
                  WA(JJ+2)= GBUF%GAMA(II(2)+I)
                  WA(JJ+3)= GBUF%GAMA(II(3)+I)
                  WA(JJ+4)= GBUF%GAMA(II(4)+I)
                  WA(JJ+5)= GBUF%GAMA(II(5)+I)
                  WA(JJ+6)= GBUF%GAMA(II(6)+I)
                ELSE
                  GAMA(1) = GBUF%GAMA(II(1)+I)
                  GAMA(2) = GBUF%GAMA(II(2)+I)
                  GAMA(3) = GBUF%GAMA(II(3)+I)
                  GAMA(4) = GBUF%GAMA(II(4)+I)
                  GAMA(5) = GBUF%GAMA(II(5)+I)
                  GAMA(6) = GBUF%GAMA(II(6)+I)
                  CALL SROTORTH(X,IXS(1,N),GAMA,JHBE,IGTYP,
     .                           ICSIG)
                  WA(JJ+1)=GAMA(1)
                  WA(JJ+2)=GAMA(2)
                  WA(JJ+3)=GAMA(3)
                  WA(JJ+4)=GAMA(4)
                  WA(JJ+5)=GAMA(5)
                  WA(JJ+6)=GAMA(6)
               ENDIF
              ELSE     
                WA(JJ+1)= ZERO
                WA(JJ+2)= ZERO
                WA(JJ+3)= ZERO
                WA(JJ+4)= ZERO
                WA(JJ+5)= ZERO
                WA(JJ+6)= ZERO
              ENDIF
              JJ = JJ + 6
            ENDDO
            IE=IE+1
C         pointeur de fin de zone dans WA
              PTWA(IE)=JJ
          ENDDO
        ENDIF 
      ENDDO
 200  CONTINUE
c-----------------------
      IF(NSPMD == 1)THEN
Clm     recopies inutiles pour simplification du code.
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
      IF(ISPMD.EQ.0.AND.LEN.GT.0) THEN
        IPRT0=0
        DO N=1,STAT_NUMELS_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXS(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
c
          IPRT  = NINT(WAP0(J + 1))
          ID    = NINT(WAP0(J + 2))
          ISORTH = NINT(WAP0(J + 3))
          NLAY   = NINT(WAP0(J + 4))
          NPTR   = NINT(WAP0(J + 5))
          NPTS   = NINT(WAP0(J + 6))
          NPTT   = NINT(WAP0(J + 7))
          ISOLNOD= NINT(WAP0(J + 8))
          JHBE   = NINT(WAP0(J + 9))
          IGTYP  = NINT(WAP0(J +10))
          IOFF  = NINT(WAP0(J + 11))
          IF(IDEL==0.OR.(IDEL==1.AND.IOFF >=1))THEN
c
            IF(IPRT /= IPRT0 .AND. ISORTH .NE. 0)THEN 
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(A)') DELIMIT
               IF(IGLOB.EQ.1.) THEN
                  WRITE(IUGEO,'(A)')'/INIBRI/ORTHO_FGLO'
               ELSE
                  WRITE(IUGEO,'(A)')'/INIBRI/ORTHO'
               ENDIF
               WRITE(IUGEO,'(A)')
     .      '#  BRICKID    NLAY  ISOLNOD  IGTYP  JJHBE' 
               WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
              IF(IGLOB.EQ.1.OR.(IGTYP .NE. 21 .AND. IGTYP .NE. 22)) THEN
                WRITE(IUGEO,'(A)')
     .            '#  X1, Y1, Z1, X2, Y2' 
                WRITE(IUGEO,'(A)')
     .            '#  Z2' 
              ELSE
                WRITE(IUGEO,'(A)')
     .            '#  COS(PHI), SIN(PHI)'
              ENDIF
               WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
               WRITE(IUGEO,'(A)') DELIMIT
             ELSE
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100) 
               IF(IGLOB.EQ.1.) THEN
                  WRITE(LINE,'(A)')'/INIBRI/ORTHO_FGLO'
               ELSE
                  WRITE(LINE,'(A)')'/INIBRI/ORTHO'
               ENDIF
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .    '#------------------------ REPEAT --------------------------' 
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .      '#  BRICKID    NLAY  ISOLNOD  IGTYP  JJHBE' 
               CALL STRS_TXT50(LINE,100) 
              IF(IGTYP .NE. 21 .AND. IGTYP .NE. 22) THEN
                WRITE(LINE,'(A)')
     .            '#  X1, Y1, Z1, X2, Y2' 
               CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .            '#  Z2' 
               CALL STRS_TXT50(LINE,100)
              ELSE
                WRITE(LINE,'(A)')
     .            '#  COS(PHI), SIN(PHI)'
               CALL STRS_TXT50(LINE,100)
              ENDIF
               WRITE(LINE,'(A)')
     .    '#------------------------ REPEAT --------------------------' 
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100) 
             END IF
             IPRT0=IPRT
            END IF
            IF(ISORTH == 1)THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(5I10)') ID,NLAY,ISOLNOD,IGTYP,JHBE
              ELSE
                WRITE(LINE,'(5I10)') ID,NLAY,ISOLNOD,IGTYP,JHBE
                CALL STRS_TXT50(LINE,100)
              ENDIF
              J = J + 11
              IF(IGLOB.EQ.1.OR.(IGTYP .NE. 21 .AND. IGTYP .NE. 22)) THEN
                JJ = J
                DO I=1,NLAY
                  IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(JJ + K),K=1,5)
                    WRITE(IUGEO,'(1PE20.13)')(WAP0(JJ + K),K=6,6)
                  ELSE
                    CALL TAB_STRS_TXT50(WAP0(1),5,JJ,SIZP0,5)
                    CALL TAB_STRS_TXT50(WAP0(6),1,JJ,SIZP0,1)
                  ENDIF
                  JJ = JJ + 6
                ENDDO
              ELSE
                JJ = J
                DO I=1,NLAY
                  IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(1P2E20.13)')(WAP0(JJ + K),K=1,2)
                  ELSE
                    CALL TAB_STRS_TXT50(WAP0(1),2,JJ,SIZP0,2)
                  ENDIF
                  JJ = JJ + 6
                ENDDO

              ENDIF
            ENDIF
          ENDIF !IOFF
        ENDDO		    
      ENDIF
c-----------
      RETURN
      END
