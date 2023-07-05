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
Chd|  OUTP_C_S                      source/output/sty/outp_c_s.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        S_USER                        source/output/sty/s_user.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTP_C_S(NBXX,KEY,TEXT,ELBUF_TAB,IPARG ,EANI,
     .                    IPM ,IGEO,IXC ,IXTG ,DD_IAD,SIZLOC,SIZP0,THKE,SIZ_WR)
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
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*11 KEY
      CHARACTER*40 TEXT
      INTEGER NBXX,SIZLOC,SIZP0
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),DD_IAD(NSPMD+1,*) ,
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        SIZ_WR
      my_real
     .   EANI(*),THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,JJ,IGTYP,NUVAT,NBB(100),RESP0,WRTLEN,RES
      INTEGER NG, NEL, NFT, IAD, ITY, LFT, NPT,NLAY,NPTR,NPTS,NPTT,
     .        LLT, MLW, ISTRAIN,N, K1, K2,IL,IR,IS,IT,
     .        IHBE, JJ_OLD, NGF, NGL, NN, LEN, IMX,NUVAR,L,
     .        NBX,NPG,MPT,IPT,I1,NU,KK,NS,NVAR,ITHK,I5,COMPTEUR,
     .        IJ(3)
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)

      my_real   
     .   FAC,S1, S2, S12, VONM2,AA,MEAN_GAUSS
      my_real   
     .   FUNC(6),FUNC1(100)
      TYPE(BUF_LAY_) ,POINTER :: BUFLY     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF1,LBUF2,LBUF3,LBUF4
C-----------------------------------------------
      NBX = NBXX
      IF (NBX < 0) THEN
        NBX = -NBX
        IMX = 1
      ELSE
        IMX = 0
      ENDIF
      IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SHELL     /SCALAR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF(NBX == 26) THEN
            IF (OUTYY_FMT == 2) THEN
             WRITE(IUGEO,'(A)')'#FORMAT: (1P6E12.5)
     .       (((VAR(NU,IPT,I),NU=1,NUVAR),IPT=1,NPT),I=1,NUMSHL)' 
            ELSE
            WRITE(IUGEO,'(A)')'#FORMAT: (1P6E20.13)
     .       (((VAR(NU,IPT,I),NU=1,NUVAR),IPT=1,NPT),I=1,NUMSHL)' 
            ENDIF
          ELSEIF( NBX >= 27 .OR.NBX >= 86) THEN
            IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(A)')'#FORMAT: (1P6E12.5)
     .       (((UVAR(IPT,I)),IPT=1,NPT),I=1,NUMSHL)'
            ELSE
            WRITE(IUGEO,'(A)')'#FORMAT: (1P6E20.13)
     .       (((UVAR(IPT,I)),IPT=1,NPT),I=1,NUMSHL)'
            ENDIF
          ELSE
            IF (OUTYY_FMT == 2) THEN
              WRITE(IUGEO,'(A)')'#FORMAT: (1P6E12.5) (VAR(I),I=1,NUMSHL)'
            ELSE
              WRITE(IUGEO,'(A)')'#FORMAT: (1P6E20.13) (VAR(I),I=1,NUMSHL)'
            ENDIF
          ENDIF 
      ENDIF
C
      JJ_OLD = 1
      RESP0 = 1
      NGF = 1
      NGL = 0
      JJ = 0
      COMPTEUR = 0
      DO NN=1,NSPGROUP
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO NG = NGF, NGL
            ITY   =IPARG(5,NG)
            IF (ITY == 3 .or. ITY == 7) THEN
              MLW    =IPARG(1,NG)
              NEL    =IPARG(2,NG)
              NFT    =IPARG(3,NG)
              ISTRAIN=IPARG(44,NG)
              IHBE   =IPARG(23,NG)
              ITHK   =IPARG(28,NG)
              NFT    =IPARG(3,NG)
              LFT=1
              LLT=NEL
!
              DO K=1,3
                IJ(K) = NEL*(K-1)
              ENDDO
!
              IF (MLW == 25.OR.MLW == 27.OR.MLW == 32) ISTRAIN=1
              IF( (MLW/=0).AND.(MLW/=13) ) THEN
               GBUF => ELBUF_TAB(NG)%GBUF
               NLAY =  ELBUF_TAB(NG)%NLAY                    
               NPTR =  ELBUF_TAB(NG)%NPTR                     
               NPTS =  ELBUF_TAB(NG)%NPTS                     
               NPG  =  NPTR*NPTS                   
               NPT   = IPARG(6,NG)
               MPT   = MAX(1,NPT)
              ENDIF
C
              IF(((NBX>=20.AND.NBX<=24).OR.(NBX>=26.AND.NBX<=83)).AND.
     .            (MLW == 1.OR.MLW == 2.OR.MLW == 3.OR.MLW == 19.OR.
     .             MLW == 22.OR.MLW == 15.OR.MLW == 23.OR.MLW == 25.OR.
     .             MLW == 27.OR.MLW == 32)) THEN
C Ecriture de 0. si loi non user et outp user demande
                DO I=LFT,LLT
                  JJ=JJ+1
                  WA(JJ) = ZERO
                ENDDO         
C-----------
              ELSEIF (NBX == 1) THEN                                
                DO I=LFT,LLT
                  JJ = JJ + 1
                  IF( (MLW/=0).AND.(MLW/=13) ) THEN
                   S1 = GBUF%FOR(IJ(1)+I)                         
                   S2 = GBUF%FOR(IJ(2)+I)                         
                   S12= GBUF%FOR(IJ(3)+I)                         
                   VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                   WA(JJ) = SQRT(VONM2)
                  ELSE
                   WA(JJ) = ZERO
                  ENDIF
                ENDDO
c-----------
              ELSEIF (NBX == 3) THEN    ! thickness
                IF (ITHK > 0) THEN
                  DO I=LFT,LLT
                    JJ=JJ+1
                    IF (MLW /= 0 .AND. MLW /= 13) THEN
                      WA(JJ) = GBUF%THK(I)
                    ELSE
                      WA(JJ) = ZERO
                    ENDIF
                  ENDDO
                ELSE
                  DO I=LFT,LLT
                    JJ=JJ+1
                    N  = I + NFT
                    WA(JJ) = THKE(N)
                  ENDDO
                ENDIF
c-----------
              ELSEIF (NBX == 5) THEN    ! EINT
                DO I=LFT,LLT
                  JJ = JJ + 1
                  IF (MLW /= 0 .AND. MLW /= 13) THEN
                    WA(JJ) = GBUF%EINT(I) + GBUF%EINT(I+LLT)
                  ELSE
                    WA(JJ) = ZERO
                  ENDIF
                ENDDO
c-----------
              ELSEIF (NBX == 6) THEN    ! OFF
                DO I=LFT,LLT
                  JJ = JJ + 1
                  IF( (MLW/=0).AND.(MLW/=13) ) THEN
                   WA(JJ) = GBUF%OFF(I)
                  ELSE
                   WA(JJ) = ZERO
                  ENDIF
                ENDDO
c-----------
              ELSEIF (NBXX == 15) THEN    ! plastic strain
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ) = ZERO
                  IF ( (MLW/=0).AND.(MLW/=13) ) THEN
                   IF (GBUF%G_PLA > 0) WA(JJ) = GBUF%PLA(I)
                  ENDIF
                ENDDO
c-----------
              ELSEIF (NBXX == -15) THEN    ! max plastic strain
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ) = ZERO
                  IF (MLW /= 0 .AND. MLW /= 13) THEN
                    IF (GBUF%G_PLA > 0) THEN
                      DO IL=1,NLAY                                                 
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                        NPTT = BUFLY%NPTT
                        IF (BUFLY%L_PLA > 0) THEN
                          DO IS=1,NPTS
                            DO IR=1,NPTR
                              DO IT=1,NPTT                                           
                                LBUF => BUFLY%LBUF(IR,IS,IT)
                                WA(JJ) = MAX(WA(JJ),LBUF%PLA(I))
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDIF
                ENDDO ! DO I=LFT,LLT
c-----------
              ELSEIF (NBX == 25) THEN  ! Hourglass 
                DO I=LFT,LLT                       
                  JJ=JJ+1                          
                  IF (ITY == 7) THEN               
                    WA(JJ)=ZERO
                  ELSE
                    WA(JJ)=EANI(NFT + I + NUMELS)
                  ENDIF
                ENDDO
C-----------
              ELSEIF (NBX>=20.AND.NBX<=24.AND.IHBE == 11) THEN  
                CALL S_USER(NBX,IMX,IHBE,NEL,NPT,MLW,IPM,IGEO, IXC,
     .                      ITY ,JJ,ELBUF_TAB(NG),WA, NFT, FUNC,
     .                      NLAY,NPTR,NPTS)
C-----------
              ELSEIF (NBX == 26) THEN
                IF ((MLW>=29.AND.MLW<=31).OR.MLW == 35.OR.
     .               MLW == 36.OR.MLW == 43.OR.MLW == 44.OR.
     .               MLW == 45.OR.MLW == 48.OR.MLW>=50) THEN
                  NPG=0
                  IF (IHBE == 11) THEN   ! BATOZ
                    IF (ITY == 3) THEN
                      NPG =4
                      FAC = FOURTH
                    ELSEIF (ITY == 7) THEN          
                      NPG =3
                      FAC = THIRD
                    ENDIF
c
                    IF (NLAY > 1) THEN
cc                      IT = 1
                      DO I=1,NEL   
                        WA(JJ + 1) = IHBE
                        WA(JJ + 2) = NPT
                        WA(JJ + 3) = NPG
                        WA(JJ + 4) = NUVAR
                        JJ = JJ + 4 
                        DO IL=1,NLAY                                           
                          NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                          DO IR=1,NPTR                                           
                            DO IS=1,NPTS                                         
                              DO IT=1,NPTT 
                                MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)  
                                NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT       
                                DO NU= 1,NUVAR 
                                  JJ= JJ + 1
                                  I1 = (NU -1)*NEL                      
                                  WA(JJ) = MBUF%VAR(I1 + I)             
                                ENDDO
                              ENDDO   
                            ENDDO
                          ENDDO           
                        ENDDO                                                
                      ENDDO ! I=1,NEL     
                    ELSE    ! NLAY = 1
                      IL = 1
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      DO I=1,NEL
                        WA(JJ + 1) = IHBE
                        WA(JJ + 2) = NPT
                        WA(JJ + 3) = NPG
                        WA(JJ + 4) = NUVAR
                        JJ = JJ + 4 
                        DO IS=1,NPTS                                         
                          DO IR=1,NPTR                                           
                            DO IT=1,NPTT
                              MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)  
                              NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT       
                              DO NU= 1,NUVAR
                                JJ= JJ + 1
                                I1 = (NU-1)*NEL                             
                                WA(JJ) = MBUF%VAR(I1 + I)                    
                              ENDDO 
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDDO ! I=1,NEL
                    ENDIF   ! NLAY
                  ELSE  ! not Batoz
                    DO I=LFT,LLT
                      WA(JJ + 1) = IHBE
                      WA(JJ + 2) = NPT
                      WA(JJ + 3) = NPG
                      WA(JJ + 4) = NUVAR
                      JJ = JJ + 4 
                      DO IL=1,NLAY                                         
                        NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                        DO IT=1,NPTT                                       
                          MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(1,1,IT)      
                          NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT         
                          DO NU=1,NUVAR
                            JJ = JJ + 1
                            I1 = (NU-1)*NEL                             
                            WA(JJ)= MBUF%VAR(I1 + I) 
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF  ! IHBE
                ENDIF    ! MLW
c----------
              ELSEIF(NBX >= 27 .AND. NBX <= 86 )THEN
                IF ((MLW>=29.AND.MLW<=31).OR.
     .               MLW == 35.OR.MLW == 36.OR.MLW == 43.OR.
     .               MLW == 44.OR.MLW == 45.OR.MLW == 48.OR.MLW>=50) THEN
                  NPG=0
                  NU = NBX - 26
                  I1 = (NU -1)*NEL
                  IF (IHBE == 11) THEN  ! BATOZ
                    IF (ITY == 3) THEN                                            
                      NPG = 4
                      FAC = FOURTH
                    ELSEIF(ITY == 7)THEN                                          
                      NPG = 3
                      FAC = THIRD
                    ENDIF
C
                    WA(JJ + 1) = IHBE
                    WA(JJ + 2) = NPT
                    WA(JJ + 3) = NPG
                    WA(JJ + 4) = NEL
                    JJ = JJ + 4
                    IF (NLAY > 1) THEN                                            
cc                      IT = 1                                                      
                      DO I=1,NEL   
                        DO IL=1,NLAY
                          NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                          DO IS=1,NPTS                                            
                            DO IR=1,NPTR                                              
                              DO IT=1,NPTT
                                MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)       
                                JJ= JJ + 1
                                WA(JJ) = MBUF%VAR(I1 + I)                         
                              ENDDO   
                            ENDDO
                          ENDDO                                         
                        ENDDO                                                     
                      ENDDO                                                       
                    ELSE  ! NLAY = 1                                              
                      IL = 1
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      DO I=1,NEL
                        DO IS=1,NPTS                                            
                          DO IR=1,NPTR                                              
                            DO IT=1,NPTT                                          
                              NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT            
                              MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)       
                              JJ= JJ + 1
                              WA(JJ) = MBUF%VAR(I1 + I)                            
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDDO                                                       
                    ENDIF ! NLAY                                                  
                  ELSE  ! not Batoz
                    WA(JJ + 1) = IHBE
                    WA(JJ + 2) = NPT
                    WA(JJ + 3) = NPG
                    WA(JJ + 4) = NEL
                    JJ = JJ + 4    
                    DO I=LFT,LLT
                      DO IL=1,NLAY                                           
                        NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                        DO IT=1,NPTT                                         
                          MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(1,1,IT)        
                          JJ = JJ + 1
                          WA(JJ)= MBUF%VAR(I1 + I)                           
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF ! IHBE
                ENDIF   ! MLW                                                    
c---------
              ELSEIF (NBX == 87) THEN
C  equivalent stress - other then VON MISES
                IF ( (MLW/=0).AND.(MLW/=13) ) THEN
                 IF (GBUF%G_SEQ > 0) THEN
                  IF (NLAY > 1) THEN
                    IL = IABS(NLAY)/2 + 1
                    BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                    NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                    IF (NPG > 1) THEN
                      DO I=LFT,LLT  
                        JJ = JJ + 1
                        DO IT=1,NPTT
                          LBUF1 => BUFLY%LBUF(1,1,IT)
                          LBUF2 => BUFLY%LBUF(2,1,IT)
                          LBUF3 => BUFLY%LBUF(1,2,IT)
                          LBUF4 => BUFLY%LBUF(2,2,IT)
                          MEAN_GAUSS  = FOURTH*(LBUF1%SEQ(I) + LBUF2%SEQ(I) +
     .                                         LBUF3%SEQ(I) + LBUF4%SEQ(I))
                          WA(JJ) = WA(JJ) + MEAN_GAUSS/NPTT
cc                      WA(JJ) = FOURTH*(LBUF1%SEQ(I) + LBUF2%SEQ(I) +
cc     .                                LBUF3%SEQ(I) + LBUF4%SEQ(I))
                        ENDDO
                      ENDDO
                    ELSE
                      DO I=LFT,LLT
                        JJ = JJ + 1
                        DO IT=1,NPTT
                          WA(JJ) =  WA(JJ) + BUFLY%LBUF(1,1,IT)%SEQ(I)/NPTT
                        ENDDO
cc                        WA(JJ) = BUFLY%LBUF(1,1,1)%SEQ(I)
                      ENDDO
                    ENDIF
                  ELSEIF (NPG > 1) THEN  !  (NLAY = 1)
                    NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                    IPT = IABS(NPTT)/2 + 1
cc                    IPT = IABS(NPT)/2 + 1
                    BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                    DO I=LFT,LLT
                      JJ = JJ + 1
                      LBUF1 => BUFLY%LBUF(1,1,IPT)
                      LBUF2 => BUFLY%LBUF(2,1,IPT)
                      LBUF3 => BUFLY%LBUF(1,2,IPT)
                      LBUF4 => BUFLY%LBUF(2,2,IPT)
                      WA(JJ) = FOURTH*(LBUF1%SEQ(I) + LBUF2%SEQ(I) +
     .                                LBUF3%SEQ(I) + LBUF4%SEQ(I))
                    ENDDO
                  ELSE  !  (NLAY = 1 .and. NPG = 1)
                    NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                    IPT = IABS(NPTT)/2 + 1
cc                    IPT = IABS(NPT)/2 + 1
                    BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                    DO I=LFT,LLT
                      JJ = JJ + 1
                      WA(JJ) = BUFLY%LBUF(1,1,IPT)%SEQ(I)
                    ENDDO
                  ENDIF  !  IF (NLAY > 1) THEN
                 ELSE  ! VON MISES
                  DO I=LFT,LLT
                   JJ = JJ + 1
                   S1 = GBUF%FOR(IJ(1)+I)
                   S2 = GBUF%FOR(IJ(2)+I)
                   S12= GBUF%FOR(IJ(3)+I)
                   VONM2 = S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                   WA(JJ) = SQRT(VONM2)
                  ENDDO
                 ENDIF  !  IF (GBUF%G_SEQ > 0) THEN
                ELSE  ! MLW /=0 or 13
                 DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ) = ZERO
                 ENDDO
                ENDIF ! MLW /=0 or 13
c---------
              ELSE   ! other not recognized NBX
                DO I=LFT,LLT
                 JJ = JJ + 1
                 WA(JJ) = ZERO
                ENDDO
              ENDIF  ! NBX
            ENDIF  ! IF (ITY == 3 .or. ITY == 7) THEN
          ENDDO  ! DO NG = NGF, NGL
c-----------------------------------------------------------------
          NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
      ENDDO
!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD==0) THEN
         RESP0 = 0
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR+RESP0) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd

          JJ_OLD = COMPTEUR+RESP0
c-----------------------------------------------------------------
          IF (JJ_OLD > 0) THEN
c-------
            IF (NBX == 26) THEN    ! all User variables           
              J = 1
              DO WHILE (J<JJ_OLD+1)
                IHBE  = NINT(WAP0(J )) 
                NPT   = NINT(WAP0(J + 1)) 
                NPG   = NINT(WAP0(J + 2)) 
                NUVAR = NINT(WAP0(J + 3))
                J = J + 4
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(4I8)')IHBE,NPT,NPG,NUVAR
                ELSE
                  WRITE(IUGEO,'(4I10)')IHBE,NPT,NPG,NUVAR
                ENDIF                       
c
                IF (NPG == 0) THEN
                  IF (NPT == 0) THEN
                    IF (OUTYY_FMT == 2) THEN
                      WRITE(IUGEO,'(1P6E12.5)')(WAP0(J + K - 1),K=1,NUVAR)
                    ELSE
                      WRITE(IUGEO,'(1P6E20.13)')(WAP0(J + K - 1),K=1,NUVAR)
                    ENDIF
                    J = J + NUVAR
                  ELSE
                    DO IPT = 1,NPT  
                      IF (OUTYY_FMT == 2) THEN
                        WRITE(IUGEO,'(1P6E12.5)')(WAP0(J + K - 1),K=1,NUVAR)
                      ELSE
                        WRITE(IUGEO,'(1P6E20.13)')(WAP0(J + K - 1),K=1,NUVAR)
                      ENDIF
                      J = J + NUVAR
                    ENDDO 
                  ENDIF
                ELSE  ! IF (NPG == 0)
                  IF (NPT == 0) THEN
                    DO KK = 1,NPG
                      IF (OUTYY_FMT == 2) THEN
                        WRITE(IUGEO,'(1P6E12.5)')(WAP0(J + K - 1),K=1,NUVAR)
                      ELSE
                        WRITE(IUGEO,'(1P6E20.13)')(WAP0(J + K - 1),K=1,NUVAR)
                      ENDIF
                      J = J + NUVAR
                    ENDDO
                  ELSE
                    DO KK = 1,NPG
                      DO IPT = 1,NPT
                        IF(OUTYY_FMT == 2) THEN
                          WRITE(IUGEO,'(1P6E12.5)')(WAP0(J + K - 1),K=1,NUVAR)
                        ELSE
                          WRITE(IUGEO,'(1P6E20.13)')(WAP0(J + K - 1),K=1,NUVAR)
                        ENDIF
                        J = J + NUVAR
                      ENDDO
                    ENDDO 
                  ENDIF 
                ENDIF        
              ENDDO    ! DO WHILE (J<JJ_OLD)
c-------
            ELSEIF (NBX >= 27 .AND. NBX <= 86) THEN  ! single User variables          
              J = 1
              DO WHILE (J<JJ_OLD+1)
                IHBE  = NINT(WAP0(J )) 
                NPT   = NINT(WAP0(J + 1)) 
                NPG   = NINT(WAP0(J + 2)) 
                NEL   = NINT(WAP0(J + 3))
                NVAR  = NEL*MAX(1,NPT)*MAX(1,NPG)
                J = J + 4
                WRITE(IUGEO,'(A)')'#FORMAT:IHBE,NPT,NPG'           
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(3I8)')IHBE,NPT,NPG
                ELSE
                  WRITE(IUGEO,'(3I10)')IHBE,NPT,NPG
                ENDIF                       
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(1P6E12.5)')(WAP0(J + K - 1),K=1,NVAR)
                ELSE
                  WRITE(IUGEO,'(1P6E20.13)')(WAP0(J + K - 1),K=1,NVAR)
                ENDIF
                J = J + NVAR
              ENDDO  ! DO WHILE (J<JJ_OLD)
c-------
            ELSE  ! Other NBX
              RES=MOD(JJ_OLD,6)
              WRTLEN=JJ_OLD-RES
              IF (WRTLEN > 0) THEN
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,WRTLEN)
                ELSE
                  WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,WRTLEN)
                ENDIF  
              ENDIF
              DO I=1,RES
                WAP0(I)=WAP0(WRTLEN+I)
              ENDDO
              RESP0=RES
            ENDIF   ! NBX       
c
          ENDIF   ! IF (JJ_OLD > 0) THEN    
c-------
        ENDDO  ! DO NN=1,NSPGROUP
c---
        IF (RESP0>0) THEN
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)
          ELSE
            WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)
          ENDIF
        ENDIF
c---
       ENDIF ! ispmd = 0
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  COUNT_ARSZ_CS                 source/output/sty/outp_c_s.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_CS                  source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_CS(IPARG,IXC,IXTG,IGEO,IPM,DD_IAD,WASZ,SIZ_WRITE_LOC) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),DD_IAD(NSPMD+1,*) ,
     .        IPARG(NPARG,*),IPM(NPROPMI,*),WASZ(2),IGEO(NPROPGI,*),
     ,        IUSER_FULL,SIZ_WRITE_LOC(2*NSPGROUP+2)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,NGF,NGL,NN,ITY,IAD,NFT,LFT,LLT,NPT,
     .        IHBE,IGTYP,NUVAR,MLW,NG,NEL,NBX,I,NPG,MPT,
     .        WASZ1,WASZ26,J
C-----------------------------------------------
      WASZ1 = 0
      SIZ_WRITE_LOC = 0

      IF ( OUTP_CS( 1) == 1.OR.OUTP_CS( 2) == 1.OR.OUTP_CS( 3) == 1
     . .OR.OUTP_CS( 4) == 1.OR.OUTP_CS( 7) == 1.OR.OUTP_CS(25) == 1
     . .OR.OUTP_CS(20) == 1.OR.OUTP_CS(21) == 1.OR.OUTP_CS(22) == 1
     . .OR.OUTP_CS(23) == 1.OR.OUTP_CS(24) == 1) THEN

        NGF = 1
        NGL = 0

        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          NEL   =IPARG(2,NG)
          IF(ITY == 3.OR.ITY == 7) THEN
            JJ = JJ+NEL
          ENDIF
         ENDDO
         WASZ1 = WASZ1+JJ
         NGF = NGL + 1
         SIZ_WRITE_LOC(NN) = JJ
        ENDDO
      ENDIF
      WASZ26 = 0

      IF (OUTP_CS(26) == 1) THEN

        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 3.OR.ITY == 7) THEN
           MLW   =IPARG(1,NG)
           NEL   =IPARG(2,NG)
           NFT   =IPARG(3,NG)
           LFT=1
           LLT=NEL
           NPT   =IPARG(6,NG)
           IHBE   =IPARG(23,NG)
           NUVAR   = 0
           IF(ITY == 3)THEN
               IGTYP=IGEO(11,IXC(NIXC-1,1+NFT))
               DO I=LFT,LLT
                 NUVAR  = MAX(NUVAR,IPM(8,IXC(1,I+NFT)))
               ENDDO
           ELSE
               IGTYP=IGEO(11,IXTG(NIXTG-1,1+NFT))
               DO I=LFT,LLT
                 NUVAR  = MAX(NUVAR,IPM(8,IXTG(1,I+NFT)))
               ENDDO
           ENDIF

           IF ((MLW>=29.AND.MLW<=31).OR.
     .        MLW == 35.OR.MLW == 36.OR.MLW == 43.OR.
     .        MLW == 44.OR.MLW == 45.OR.MLW == 48.OR.MLW>=50) THEN

              NPG=0

              IF(IHBE == 11) THEN         
               IF (ITY == 3) THEN
                 NPG = 4
               ELSEIF(ITY == 7)THEN             
                 NPG =3
               ENDIF
               MPT=IABS(NPT)
               IF (MPT == 0) THEN
                  JJ = JJ + NEL*(4+NPG*NUVAR)
               ELSE
                  JJ = JJ + NEL*(4+NPG*MPT*NUVAR)
               END IF
              ELSE
               MPT=IABS(NPT)
               IF (MPT == 0) THEN
                  JJ = JJ + NEL*(4+NUVAR)
               ELSE
                  JJ = JJ + NEL*(4+MPT*NUVAR)
               END IF
              END IF
           END IF
          END IF
         END DO
          WASZ26 = WASZ26+JJ
          NGF = NGL + 1
          SIZ_WRITE_LOC(NSPGROUP+NN) = JJ
        END DO
      ENDIF

      IUSER_FULL = 0
      DO J=1,60
        IF(OUTP_CS(26 + J) == 1) IUSER_FULL = 1
      ENDDO
      IF ( IUSER_FULL == 1 ) THEN
        NGF = 1
        NGL = 0
C
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 3.OR.ITY == 7) THEN
           MLW   =IPARG(1,NG)
           NEL   =IPARG(2,NG)
           NFT   =IPARG(3,NG)
           LFT=1
           LLT=NEL
           NPT   =IPARG(6,NG)
           IHBE   =IPARG(23,NG)
           NUVAR   = 0
           IF(ITY == 3)THEN
               IGTYP=IGEO(11,IXC(NIXC-1,1+NFT))
               DO I=LFT,LLT
                 NUVAR  = MAX(NUVAR,IPM(8,IXC(1,I+NFT)))
               ENDDO
           ELSE
               IGTYP=IGEO(11,IXTG(NIXTG-1,1+NFT))
               DO I=LFT,LLT
                 NUVAR  = MAX(NUVAR,IPM(8,IXTG(1,I+NFT)))
               ENDDO
           ENDIF
             IF ((MLW>=29.AND.MLW<=31).OR.
     .        MLW == 35.OR.MLW == 36.OR.MLW == 43.OR.
     .        MLW == 44.OR.MLW == 45.OR.MLW == 48.OR.MLW>=50) THEN

              NPG=0

              IF(IHBE == 11) THEN         
               IF (ITY == 3) THEN
                 NPG = 4
               ELSEIF(ITY == 7)THEN             
                 NPG =3
               ENDIF
               MPT=IABS(NPT)
               IF (MPT == 0) THEN
                  JJ = JJ + 4 + NEL*(NPG)
               ELSE
                  JJ = JJ + 4 + NEL*(NPG*MPT)
               END IF               
              ELSE
               MPT=IABS(NPT)
               IF (MPT == 0) THEN
                  JJ = JJ + 4 + NEL
               ELSE
                  JJ = JJ + 4 + NEL*(MPT)
               END IF                  
              END IF
             END IF
          END IF
         END DO
          WASZ26 = WASZ26+JJ
          NGF = NGL + 1
          SIZ_WRITE_LOC(NSPGROUP+NN) = SIZ_WRITE_LOC(NSPGROUP+NN) + JJ
        END DO
      ENDIF   
      WASZ(1) = WASZ1
      WASZ(2) = WASZ26
      SIZ_WRITE_LOC(2*NSPGROUP+1) = WASZ(1)
      SIZ_WRITE_LOC(2*NSPGROUP+2) = WASZ(2)
      

      RETURN
      END
