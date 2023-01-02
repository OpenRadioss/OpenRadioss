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
Chd|  OUTP_S_T                      source/output/sty/outp_s_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE OUTP_S_T(NBX   ,KEY,TEXT,ELBUF_TAB,IPARG,
     .                    DD_IAD,SIZLOC,SIZP0,SIZ_WR)
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
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER NBX,SIZLOC,SIZP0,SIZ_WR
      INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NBB(20),RESP0,WRTLEN,RES
      INTEGER NG, NEL, IADD, N,ISOLNOD,MLW,
     .        II,JJ,JJ_OLD, NGF, NGL, NN, LEN,FWAP0,LENWAP0,
     .        COMPTEUR,L,K,KK(6)
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   FUNC(6)
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C=======================================================================
      IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SOLID     /TENSOR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(2A)') '#FORMAT: (1P6E12.5) ',
     .      '(TX(I),TY(I),TZ(I),TXY(I),TYZ(I),TZX(I),I=1,NUMSOL)'
          ELSE
            WRITE(IUGEO,'(2A)') '#FORMAT: (1P6E20.13) ',
     .      '(TX(I),TY(I),TZ(I),TXY(I),TYZ(I),TZX(I),I=1,NUMSOL)'
          ENDIF
      ENDIF
C
      NGF = 1
      NGL = 0
      JJ = 0
      COMPTEUR = 0
      DO NN=1,NSPGROUP
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO  NG = NGF, NGL
          ITY = IPARG(5,NG)
          IF (ITY == 1 .OR. ITY == 2) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
           GBUF => ELBUF_TAB(NG)%GBUF
           LFT=1
           LLT=NEL
!
           DO I=1,6
             KK(I) = NEL*(I-1)
           ENDDO
!
           DO I=LFT,LLT
             II = (I-1)*6
             WA(JJ + II + 1) = GBUF%SIG(KK(1)+I)
             WA(JJ + II + 2) = GBUF%SIG(KK(2)+I)
             WA(JJ + II + 3) = GBUF%SIG(KK(3)+I)
             WA(JJ + II + 4) = GBUF%SIG(KK(4)+I)
             WA(JJ + II + 5) = GBUF%SIG(KK(5)+I)
             WA(JJ + II + 6) = GBUF%SIG(KK(6)+I)
           ENDDO
           JJ = JJ + 6*LLT
          ENDIF
         ENDDO
         NGF = NGL + 1
         JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
         COMPTEUR = JJ                         
      ENDDO     ! do nn=1,nspgroup
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
         JJ_OLD = 0 
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
          IF (JJ_OLD>0) THEN
           RES=MOD(JJ_OLD,6)
           WRTLEN=JJ_OLD-RES
           IF (WRTLEN>0) THEN
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
          ENDIF   ! if(jj_old>0)
         ENDDO    ! do nn=1,nspgroup
         IF ( RESP0>0 ) THEN
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)
          ELSE
            WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)
          ENDIF
         ENDIF
       ENDIF
c-----------
      RETURN
      END

Chd|====================================================================
Chd|  OUTP_S_TT                     source/output/sty/outp_s_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE OUTP_S_TT(NBX   ,KEY ,TEXT,ELBUF_TAB,IPARG,
     2                    DD_IAD  ,IPM ,IXS,SIZLOC,SIZP0,SIZ_WR )
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
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER IXS(NIXS,*),IPM(NPROPMI,*),IPARG(NPARG,*),
     .   DD_IAD(NSPMD+1,*)
      INTEGER NBX,SIZLOC,SIZP0,SIZ_WR
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,NLAY,NPTR,NPTS,NPTT,IL,IR,IS,IT,IPT,
     .        NG, NEL, IADD, MLW,JJ_OLD, NGF, NGL, NN, LEN, ICAS_OLD,
     .        ISOLNOD,KHBE,ITENS,TSHELL,COMPTEUR,L,KK(6)
      my_real
     .   FUNC(6)
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C=======================================================================
      ITENS = NBX
c-----------------------  
      IF (ISPMD == 0) THEN        
        WRITE(IUGEO,'(2A)')'/SOLID     /TENSOR    /',KEY
        WRITE(IUGEO,'(A)')TEXT
      ENDIF     
C
       JJ_OLD = 1
       NGF = 1
       NGL = 0
       JJ = 0
       COMPTEUR = 0
       DO NN=1,NSPGROUP
        NGL = NGL + DD_IAD(ISPMD+1,NN)
        DO  NG = NGF, NGL
         ITY   =IPARG(5,NG)
         IF (ITY == 1.OR.ITY == 2) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,KHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          GBUF => ELBUF_TAB(NG)%GBUF
          NLAY = ELBUF_TAB(NG)%NLAY                       
          NPTR = ELBUF_TAB(NG)%NPTR                       
          NPTS = ELBUF_TAB(NG)%NPTS                       
          NPTT = ELBUF_TAB(NG)%NPTT                       
          NPT  = NPTR * NPTS * NPTT * NLAY
          LFT=1
          LLT=NEL           
          ISOLNOD=IPARG(28,NG)
          TSHELL = 0
          IF (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22) TSHELL = 1
!
          DO I=1,6
            KK(I) = NEL*(I-1)
          ENDDO
!
C--------------------------------------          
        IF (ITENS == 2)THEN   
C         /outp/brick/stress/full          
C------------------------------------
          IF (TSHELL == 1) THEN
            IF (KHBE == 14 .OR. KHBE == 16) THEN
              DO I=LFT,LLT              
                WA(JJ+1) = NLAY  
                WA(JJ+2) = NPTR  
                WA(JJ+3) = NPTS  
                WA(JJ+4) = NPTT  
                WA(JJ+5) = ABS(ISOLNOD) 
                WA(JJ+6) = IABS(KHBE)   
                JJ = JJ + 6                
                DO IR=1,NPTR                                                
                  DO IS=1,NPTS                                                  
                   DO IT=1,NPTT                                                  
                    DO IL=1,NLAY
                     LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                     WA(JJ +  1) = LBUF%SIG(KK(1)+I)				 
                     WA(JJ +  2) = LBUF%SIG(KK(2)+I)				 
                     WA(JJ +  3) = LBUF%SIG(KK(3)+I)				 
                     WA(JJ +  4) = LBUF%SIG(KK(4)+I)				 
                     WA(JJ +  5) = LBUF%SIG(KK(5)+I)				 
                     WA(JJ +  6) = LBUF%SIG(KK(6)+I)				 
                     IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                                      
                       WA(JJ +  7) = ZERO                                       
                     ELSE                                                       
                       WA(JJ +  7) = LBUF%PLA(I)                                
                     ENDIF                                                      
                     WA(JJ+8) = LBUF%EINT(I) 
                     WA(JJ+9) = LBUF%RHO(I)  
                     JJ = JJ + 9                                                 
                   ENDDO  ! IT=1,NPTT                                                     
                  ENDDO   ! IS=1,NPTS                                                    
                 ENDDO    ! IR=1,NPTR                                                    
                ENDDO     ! IL=1,NLAY                                                  
              ENDDO       ! I=LFT,LLT  
            ELSEIF (KHBE == 15) THEN
              DO I=LFT,LLT              
                WA(JJ+1) = NLAY  
                WA(JJ+2) = NPTR  
                WA(JJ+3) = NPTS  
                WA(JJ+4) = NPTT  
                WA(JJ+5) = ABS(ISOLNOD) 
                WA(JJ+6) = IABS(KHBE)   
                JJ = JJ + 6                
                DO IL=1,NLAY                                                   
                  DO IR=1,NPTR                                                
                   DO IS=1,NPTS                                                
                    DO IT=1,NPTT                                                 
                     LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)            
                     WA(JJ +  1) = LBUF%SIG(KK(1)+I)				
                     WA(JJ +  2) = LBUF%SIG(KK(2)+I)				
                     WA(JJ +  3) = LBUF%SIG(KK(3)+I)				
                     WA(JJ +  4) = LBUF%SIG(KK(4)+I)				
                     WA(JJ +  5) = LBUF%SIG(KK(5)+I)				
                     WA(JJ +  6) = LBUF%SIG(KK(6)+I)				
                     IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                                      
                       WA(JJ +  7) = ZERO                                      
                     ELSE                                                      
                       WA(JJ +  7) = LBUF%PLA(I)                               
                     ENDIF                                                     
                     WA(JJ+8) = LBUF%EINT(I) 
                     WA(JJ+9) = LBUF%RHO(I)  
                     JJ = JJ + 9                                                
                   ENDDO  ! IT=1,NPTT                                                     
                  ENDDO   ! IS=1,NPTS                                                    
                 ENDDO    ! IR=1,NPTR                                                    
                ENDDO     ! IL=1,NLAY                                                  
              ENDDO       ! I=LFT,LLT  
            ENDIF
          ELSEIF (KHBE == 14 .OR. KHBE == 17 .OR. ISOLNOD == 20 .OR.
     .           ISOLNOD == 16) THEN 
            DO I=LFT,LLT              
              WA(JJ+1) = NLAY  
              WA(JJ+2) = NPTR  
              WA(JJ+3) = NPTS  
              WA(JJ+4) = NPTT  
              WA(JJ+5) = ABS(ISOLNOD) 
              WA(JJ+6) = IABS(KHBE)   
              JJ = JJ + 6                
              DO IL=1,NLAY                                                     
               DO IT=1,NPTT                                                   
                DO IS=1,NPTS                                                  
                 DO IR=1,NPTR            
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                   WA(JJ +  1) = LBUF%SIG(KK(1)+I)			       
                   WA(JJ +  2) = LBUF%SIG(KK(2)+I)			       
                   WA(JJ +  3) = LBUF%SIG(KK(3)+I)			       
                   WA(JJ +  4) = LBUF%SIG(KK(4)+I)			       
                   WA(JJ +  5) = LBUF%SIG(KK(5)+I)			       
                   WA(JJ +  6) = LBUF%SIG(KK(6)+I)			       
                   IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                                        
                     WA(JJ +  7) = ZERO                                       
                   ELSE                                                       
                     WA(JJ +  7) = LBUF%PLA(I)                                
                   ENDIF                                                      
                   WA(JJ+8) = LBUF%EINT(I) 
                   WA(JJ+9) = LBUF%RHO(I)  
                   JJ = JJ + 9                                                  
                 ENDDO  ! IT=1,NPTT                                                       
                ENDDO   ! IS=1,NPTS                                                      
               ENDDO    ! IR=1,NPTR                                                      
              ENDDO     ! IL=1,NLAY                                                    
            ENDDO       ! I=LFT,LLT  
c
          ELSEIF (ISOLNOD == 8 .AND. NPT == 8 .AND.
     .         KHBE /= 14  .AND. KHBE /= 15 ) THEN          
            DO I=LFT,LLT              
              WA(JJ+1) = NLAY  
              WA(JJ+2) = NPTR  
              WA(JJ+3) = NPTS  
              WA(JJ+4) = NPTT  
              WA(JJ+5) = ABS(ISOLNOD) 
              WA(JJ+6) = IABS(KHBE)   
              WA(JJ+7) = GBUF%EINT(I) 
              WA(JJ+8) = GBUF%RHO(I)  
              JJ = JJ + 8                
              DO IL=1,NLAY                                                     
               DO IR=1,NPTR                                                   
                DO IS=1,NPTS                                                  
                 DO IT=1,NPTT
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                   WA(JJ +  1) = LBUF%SIG(KK(1)+I)			       
                   WA(JJ +  2) = LBUF%SIG(KK(2)+I)			       
                   WA(JJ +  3) = LBUF%SIG(KK(3)+I)			       
                   WA(JJ +  4) = LBUF%SIG(KK(4)+I)			       
                   WA(JJ +  5) = LBUF%SIG(KK(5)+I)			       
                   WA(JJ +  6) = LBUF%SIG(KK(6)+I)			       
                   IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                                        
                     WA(JJ +  7) = ZERO                                       
                   ELSE                                                       
                     WA(JJ +  7) = LBUF%PLA(I)                                
                   ENDIF   
                   JJ = JJ + 7                                                   
                 ENDDO  ! IT=1,NPTT                                                       
                ENDDO   ! IS=1,NPTS                                                      
               ENDDO    ! IR=1,NPTR                                                      
              ENDDO     ! IL=1,NLAY                                                    
            ENDDO       ! I=LFT,LLT  
c
          ELSEIF (ISOLNOD == 10) THEN
            DO I=LFT,LLT              
              WA(JJ+1) = NLAY  
              WA(JJ+2) = NPTR  
              WA(JJ+3) = NPTS  
              WA(JJ+4) = NPTT  
              WA(JJ+5) = ABS(ISOLNOD) 
              WA(JJ+6) = IABS(KHBE)   
              JJ = JJ + 6                
              DO IL=1,NLAY                                                     
               DO IR=1,NPTR                                                   
                DO IS=1,NPTS                                                  
                 DO IT=1,NPTT                                                 
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                   WA(JJ +  1) = LBUF%SIG(KK(1)+I)			       
                   WA(JJ +  2) = LBUF%SIG(KK(2)+I)			       
                   WA(JJ +  3) = LBUF%SIG(KK(3)+I)			       
                   WA(JJ +  4) = LBUF%SIG(KK(4)+I)			       
                   WA(JJ +  5) = LBUF%SIG(KK(5)+I)			       
                   WA(JJ +  6) = LBUF%SIG(KK(6)+I)			       
                   IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                                        
                     WA(JJ +  7) = ZERO                                       
                   ELSE                                                       
                     WA(JJ +  7) = LBUF%PLA(I)                                
                   ENDIF                                                      
                   WA(JJ+8) = LBUF%EINT(I) 
                   WA(JJ+9) = LBUF%RHO(I)  
                   JJ = JJ + 9                                                  
                 ENDDO  ! IT=1,NPTT                                                       
                ENDDO   ! IS=1,NPTS                                                      
               ENDDO    ! IR=1,NPTR                                                      
              ENDDO     ! IL=1,NLAY                                                    
            ENDDO       ! I=LFT,LLT  
c
          ELSEIF ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.
     .             KHBE == 15) THEN
            DO I=LFT,LLT              
              WA(JJ+1) = NLAY  
              WA(JJ+2) = NPTR  
              WA(JJ+3) = NPTS  
              WA(JJ+4) = NPTT  
              WA(JJ+5) = ABS(ISOLNOD) 
              WA(JJ+6) = IABS(KHBE)   
              JJ = JJ + 6                
              DO IL=1,NLAY                                                     
               DO IR=1,NPTR                                                   
                DO IS=1,NPTS                                                  
                 DO IT=1,NPTT                                                 
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                   WA(JJ +  1) = LBUF%SIG(KK(1)+I)			       
                   WA(JJ +  2) = LBUF%SIG(KK(2)+I)			       
                   WA(JJ +  3) = LBUF%SIG(KK(3)+I)			       
                   WA(JJ +  4) = LBUF%SIG(KK(4)+I)			       
                   WA(JJ +  5) = LBUF%SIG(KK(5)+I)			       
                   WA(JJ +  6) = LBUF%SIG(KK(6)+I)			       
                   IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                                        
                     WA(JJ +  7) = ZERO                                       
                   ELSE                                                       
                     WA(JJ +  7) = LBUF%PLA(I)                                
                   ENDIF                                                      
                   WA(JJ+8) = LBUF%EINT(I) 
                   WA(JJ+9) = LBUF%RHO(I)  
                   JJ = JJ + 9                                                  
                 ENDDO  ! IT=1,NPTT                                                       
                ENDDO   ! IS=1,NPTS                                                      
               ENDDO    ! IR=1,NPTR                                                      
              ENDDO     ! IL=1,NLAY                                                    
            ENDDO       ! I=LFT,LLT  
c-------------
          ELSE  ! one integration pt, Isolid = 0,1,2,24
c-------------
            DO I=LFT,LLT              
              WA(JJ+1) = NLAY  
              WA(JJ+2) = NPTR  
              WA(JJ+3) = NPTS  
              WA(JJ+4) = NPTT  
              WA(JJ+5) = ABS(ISOLNOD) 
              WA(JJ+6) = IABS(KHBE)   
              WA(JJ+7) = GBUF%EINT(I) 
              WA(JJ+8) = GBUF%RHO(I)  
              JJ = JJ + 8                
              WA(JJ + 1) = GBUF%SIG(KK(1)+I)				 
              WA(JJ + 2) = GBUF%SIG(KK(2)+I)				 
              WA(JJ + 3) = GBUF%SIG(KK(3)+I)				 
              WA(JJ + 4) = GBUF%SIG(KK(4)+I)				 
              WA(JJ + 5) = GBUF%SIG(KK(5)+I)				 
              WA(JJ + 6) = GBUF%SIG(KK(6)+I)  
              IF (GBUF%G_PLA == 0) THEN                                        
                WA(JJ +  7) = ZERO                                       
              ELSE                                                       
                WA(JJ +  7) = GBUF%PLA(I)                                
              ENDIF                                                      
              JJ = JJ + 7                
            ENDDO       ! I=LFT,LLT  
          ENDIF
C--------------------------------------          
        ELSEIF (ITENS == 3)THEN
c         /outp/brick/strain/full
C--------------------------------------          
          WA(JJ+1) = NPT  
          WA(JJ+2) = ISOLNOD  
          WA(JJ+3) = NEL  
          JJ = JJ+3                
          IF (ELBUF_TAB(NG)%BUFLY(1)%L_STRA == 0) THEN
            DO I=LFT,LLT              
              WA(JJ + 1) = ZERO                                       
              WA(JJ + 2) = ZERO                                       
              WA(JJ + 3) = ZERO                                       
              WA(JJ + 4) = ZERO                                       
              WA(JJ + 5) = ZERO                                       
              WA(JJ + 6) = ZERO                                       
              JJ=JJ + 6                                                       
            ENDDO   ! I=LFT,LLT 
          ELSEIF (MLW == 14) THEN
            DO I=LFT,LLT              
              DO IL=1,NLAY                                                     
               DO IR=1,NPTR                                                    
                DO IS=1,NPTS                                                   
                 DO IT=1,NPTT                                                  
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)           
                   WA(JJ + 1) = LBUF%EPE(KK(1)+I)			       
                   WA(JJ + 2) = LBUF%EPE(KK(2)+I)			       
                   WA(JJ + 3) = LBUF%EPE(KK(3)+I)			       
                   WA(JJ + 4) = ZERO                                        
                   WA(JJ + 5) = ZERO                                        
                   WA(JJ + 6) = ZERO                                        
                   JJ=JJ + 6                                                   
                 ENDDO                                                         
                ENDDO                                                          
               ENDDO                                                           
              ENDDO                                                         
            ENDDO
          ELSEIF (TSHELL == 1) THEN                                                              
            DO I=LFT,LLT              
              DO IR=1,NPTR                                                     
               DO IS=1,NPTS                                                    
                DO IT=1,NPTT                                                   
                 DO IL=1,NLAY                                                     
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)           
                   WA(JJ + 1) = LBUF%STRA(KK(1)+I)				
                   WA(JJ + 2) = LBUF%STRA(KK(2)+I)				
                   WA(JJ + 3) = LBUF%STRA(KK(3)+I)				
                   WA(JJ + 4) = LBUF%STRA(KK(4)+I)				
                   WA(JJ + 5) = LBUF%STRA(KK(5)+I)				
                   WA(JJ + 6) = LBUF%STRA(KK(6)+I)				
                   JJ=JJ + 6                                                   
                 ENDDO                                                         
                ENDDO                                                          
               ENDDO                                                           
              ENDDO                                                         
            ENDDO   ! I=LFT,LLT 
          ELSE                                                           
            DO I=LFT,LLT              
              DO IL=1,NLAY                                                     
               DO IT=1,NPTT                                            
                DO IS=1,NPTS                                              
                 DO IR=1,NPTR                                                
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)           
                   WA(JJ + 1) = LBUF%STRA(KK(1)+I)				
                   WA(JJ + 2) = LBUF%STRA(KK(2)+I)				
                   WA(JJ + 3) = LBUF%STRA(KK(3)+I)				
                   WA(JJ + 4) = LBUF%STRA(KK(4)+I)				
                   WA(JJ + 5) = LBUF%STRA(KK(5)+I)				
                   WA(JJ + 6) = LBUF%STRA(KK(6)+I)				
                   JJ=JJ + 6                                                   
                 ENDDO                                                         
                ENDDO                                                          
               ENDDO                                                           
              ENDDO                                                         
            ENDDO   ! I=LFT,LLT 
          ENDIF  
         ENDIF ! ITENS = 3
C           
        ENDIF  ! ITY == 1.OR.ITY == 2
        ENDDO   ! NG = NGF, NGL
        NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
       ENDDO         ! nn=1,nspgroup
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
       DO NN=1,NSPGROUP
        COMPTEUR = 0
        DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
        ENDDO    ! k=1,nspmd

        JJ_OLD = COMPTEUR+1
c--------------------------------
        IF (JJ_OLD > 1) THEN
C       ICAS_OLD sert a connaitre le "type" du dernier groupe traite
C       on ajoute un commentaire de format a chaque changement
         ICAS_OLD = 0
         J = 1
         DO WHILE (J < JJ_OLD)
C         valeur absolue deja stockee dans WA
          IF (ITENS == 2) THEN
           NLAY = NINT(WAP0(J))
           NPTR = NINT(WAP0(J+1))
           NPTS = NINT(WAP0(J+2))
           NPTT = NINT(WAP0(J+3))
           ISOLNOD=NINT(WAP0(J+4))
           KHBE = NINT(WAP0(J+5))
           NPT  = NPTR * NPTS * NPTT * NLAY
           J = J + 6
           TSHELL = 0
           IF (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22) TSHELL = 1
c
           IF (TSHELL == 1) THEN
            IF (KHBE == 14 .OR. KHBE == 16) THEN
              IF (ICAS_OLD /= 1) THEN
               ICAS_OLD = 1
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')                                       
     .          '#FORMAT:(NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT',              
     .          '#(NPT = NPTR * NPTS * NPTT),(5I8),I=1,NUMSOL'           
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/3E12.5) ',        
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),', 
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'     
               ELSE
                WRITE(IUGEO,'(A)')                                       
     .          '#FORMAT:(NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT',              
     .          '#(NPT = NPTR * NPTS * NPTT),(5I10),I=1,NUMSOL'          
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/3E20.13) ',      
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),', 
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'     
               ENDIF
              ENDIF ! ICAS_OLD
              IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(6I8)') NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT 
C               ecriture sur 2 lignes cf smp !
                DO I = 1, NPT
                  WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                  J = J + 6
                  WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3)
                  J = J + 3
                ENDDO
              ELSE
                WRITE(IUGEO,'(6I10)')NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT 
C               ecriture sur 2 lignes cf smp !
                DO I = 1, NPT
                  WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                  J = J + 6
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                  J = J + 3
                ENDDO
              ENDIF
c
            ELSEIF (KHBE == 15) THEN
             IF (ICAS_OLD /= 2) THEN
               ICAS_OLD = 2
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')'#FORMAT:(NPT,ISOLNOD, KHBE',
     .          '#(NPT ),(3I8),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/3E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ELSE
                WRITE(IUGEO,'(A)')'#FORMAT:(NPT,ISOLNOD, KHBE',
     .          '#(NPT ),(3I10),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/3E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ENDIF
             ENDIF
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(3I8)') NPT, ISOLNOD, KHBE
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ELSE
               WRITE(IUGEO,'(3I10)') NPT, ISOLNOD, KHBE
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ENDIF
            ENDIF
c          solid properties
           ELSEIF (ISOLNOD == 8 .AND. NPT == 8 .AND.
     .         KHBE /= 14 .AND. KHBE /= 15 .AND. KHBE/=17) THEN
C            test cas precedent et ecriture format si besoin
             IF (ICAS_OLD /= 4) THEN
               ICAS_OLD = 4
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT, ISOLNOD (2I8/2E12.5),
     .          EINT(I),RHO(I),,I=1,NUMSOL '
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),J=1,NPT),I=1,NUMSOL)'
               ELSE
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT, ISOLNOD (2I10/2E20.13),
     .          EINT(I),RHO(I),,I=1,NUMSOL '
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),J=1,NPT),I=1,NUMSOL)'
               ENDIF
             ENDIF     
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(2I8)')NPT,ISOLNOD 
               WRITE(IUGEO,'(1P2E12.5)')(WAP0(J-1+K),K=1,2)
               J=J+2
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P1E12.5)')WAP0(J)
                 J = J + 1
               ENDDO
             ELSE
               WRITE(IUGEO,'(2I10)')NPT,ISOLNOD 
               WRITE(IUGEO,'(1P2E20.13)')(WAP0(J-1+K),K=1,2)
               J=J+2
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P1E20.13)')WAP0(J)
                 J = J + 1
               ENDDO
             ENDIF
           ELSEIF (ISOLNOD == 8 .AND. (KHBE == 14 .OR. KHBE == 17)) THEN
c
             IF (ICAS_OLD /= 3) THEN
               ICAS_OLD = 3
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT',
     .          '#(NPT = NPTR * NPTS * NPTT),(5I8),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/3E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ELSE
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT',
     .          '#(NPT = NPTR * NPTS * NPTT),(5I10),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/3E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ENDIF
             ENDIF
c
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(6I8)') NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT 
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ELSE
               WRITE(IUGEO,'(6I10)')NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT 
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ENDIF
c
           ELSEIF (ISOLNOD == 20) THEN
             IF(ICAS_OLD /= 6) THEN
               ICAS_OLD = 6
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')'#FORMAT:(NPT,ISOLNOD,NPTR,NPTS,NPTT',
     .          '#(NPT = NPTR * NPTS * NPTT),(5I8),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/3E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ELSE
                WRITE(IUGEO,'(A)')'#FORMAT:(NPT,ISOLNOD,NPTR,NPTS,NPTT',
     .          '#(NPT = NPTR * NPTS * NPTT),(5I10),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/3E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ENDIF
             ENDIF
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(5I8)')NPT,ISOLNOD,NPTR,NPTS,NPTT 
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ELSE
               WRITE(IUGEO,'(5I10)')NPT,ISOLNOD,NPTR,NPTS,NPTT 
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ENDIF
c
           ELSEIF ((ISOLNOD == 8 .OR. NPT == 1) .AND.
     .              KHBE /= 14 .AND. KHBE /= 15 .AND. KHBE /= 17) THEN
             IF (ICAS_OLD /= 5) THEN
               ICAS_OLD = 5
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT, ISOLNOD (2I8/2E12.5),
     .          EINT(I),RHO(I),,I=1,NUMSOL '
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),J=1,NPT),I=1,NUMSOL,NPT=1)'
               ELSE
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT, ISOLNOD (2I10/2E20.13),
     .          EINT(I),RHO(I),,I=1,NUMSOL '
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),J=1,NPT),I=1,NUMSOL,NPT=1)'
               ENDIF
             ENDIF     
                IF (OUTYY_FMT == 2) THEN
                 WRITE(IUGEO,'(2I8)')NPT,ISOLNOD 
                 WRITE(IUGEO,'(1P2E12.5)')(WAP0(J-1+K),K=1,2)
                J=J+2
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J=J+6
                 WRITE(IUGEO,'(1P1E12.5)')WAP0(J)
                 J = J + 1
               ELSE
                 WRITE(IUGEO,'(2I10)')NPT,ISOLNOD 
                 WRITE(IUGEO,'(1P2E20.13)')(WAP0(J-1+K),K=1,2)
                 J=J+2
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J=J+6
                 WRITE(IUGEO,'(1P1E20.13)')WAP0(J)
                 J = J + 1
               ENDIF
C           le test remis coherent par rapport a SMP
c
           ELSEIF (ISOLNOD == 10) THEN
             IF(ICAS_OLD /= 7) THEN
               ICAS_OLD = 7
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT,ISOLNOD,(2I8),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/3E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ELSE
                WRITE(IUGEO,'(A)')
     .          '#FORMAT:(NPT,ISOLNOD,(2I10),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/3E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ENDIF
             ENDIF
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(2I8)')NPT,ISOLNOD
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ELSE
               WRITE(IUGEO,'(2I10)')NPT,ISOLNOD
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ENDIF
           ELSE
             IF (ICAS_OLD /= 8) THEN
               ICAS_OLD = 8
               IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(A)')'#FORMAT:(NPT,ISOLNOD, KHBE',
     .          '#(NPT ),(3I8),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/3E12.5) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ELSE
                WRITE(IUGEO,'(A)')'#FORMAT:(NPT,ISOLNOD, KHBE',
     .          '#(NPT ),(3I10),I=1,NUMSOL'
                WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/3E20.13) ',
     .          '((TX(I,J),TY(I,J),TZ(I,J),TXY(I,J),TYZ(I,J),TZX(I,J),',
     .          '#EPSP(J,I),EINT(J,I),RHO(J,I),J=1,NPT),I=1,NUMSOL)'
               ENDIF
             ENDIF
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(3I8)') NPT, ISOLNOD, KHBE
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ELSE
               WRITE(IUGEO,'(3I10)') NPT, ISOLNOD, KHBE
C              ecriture sur 2 lignes cf smp !
               DO I = 1, NPT
                 WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
                 WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                 J = J + 3
               ENDDO
             ENDIF
           ENDIF
c-------------------------------
c          /STRAIN
c-------------------------------
          ELSEIF(ITENS == 3)THEN
            NPT    = NINT(WAP0(J))
            ISOLNOD= NINT(WAP0(J+1))
            NEL    = NINT(WAP0(J+2))
            J=J+3
            IF (ICAS_OLD /= 10) THEN
              ICAS_OLD = 10
              IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(A)')
     .        '#FORMAT:(NPT, ISOLNOD, NUMSOL (3I8)'
               WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5) ',
     .       '((EXX(I,J),EYY(I,J),EZZ(I,J),EXY(I,J),EYZ(I,J),EZX(I,J),',
     .         '#J=1,NPT),I=1,NUMSOL)'             
              ELSE
               WRITE(IUGEO,'(A)')
     .        '#FORMAT:(NPT, ISOLNOD, NUMSOL (3I10)'
               WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13) ',
     .       '((EXX(I,J),EYY(I,J),EZZ(I,J),EXY(I,J),EYZ(I,J),EZX(I,J),',
     .        '#J=1,NPT),I=1,NUMSOL)'             
              ENDIF
            ENDIF 
c
            IF(OUTYY_FMT == 2)THEN
              WRITE(IUGEO,'(3I8)') NPT, ISOLNOD,NEL
               DO I = 1,NEL
                DO IPT = 1, NPT
                 WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                 J = J + 6
               ENDDO
              ENDDO       
            ELSE
              WRITE(IUGEO,'(3I10)') NPT,ISOLNOD,NEL
              DO I=1,NEL
                DO IPT = 1, NPT
                  WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                  J = J + 6
                ENDDO
              ENDDO  
            ENDIF                         
c
          ENDIF ! ITENS
c-------------------------------
         ENDDO  ! WHILE (J < JJ_OLD)
       ENDIF    ! jj_old>1
      ENDDO     ! nn=1,nspgroup
      ENDIF     ! ispmd==0
C-----------              
      RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_ST                 source/output/sty/outp_s_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_ST                  source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_ST(IPARG,DD_IAD,WASZ,SZP0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),WASZ(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL,ITY,NRL,KHBE,MLW,NPT,ISOLNOD,NGF,NN,NEL,
     .        NG,NLAY,NPTR,NPTS,NPTT,NPTG,JJ,
     .        SZP0(3*NSPGROUP+3)
C=======================================================================
      WASZ = 0
      SZP0 = 0
c------------------------------
      IF (OUTP_ST(1) == 1) THEN
       NGF = 1
       NGL = 0
       DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO  NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF (ITY == 1.OR.ITY == 2) THEN
           NEL   =IPARG(2,NG)
         
           JJ = JJ + 6*NEL
          ENDIF
         ENDDO
         NGF = NGL + 1
         WASZ(1) = WASZ(1)+JJ
         SZP0(NN)=JJ
        ENDDO
        SZP0(3*NSPGROUP+1) = WASZ(1)
      END IF
c------------------------------
      IF (OUTP_ST(2) == 1) THEN
        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
          JJ = 0
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO  NG = NGF, NGL
            ITY   =IPARG(5,NG)
            IF (ITY == 1 .OR. ITY == 2) THEN
              MLW  = IPARG(1,NG)
              NEL  = IPARG(2,NG)
              KHBE = IPARG(23,NG)    
              NPT  = ABS(IPARG(6,NG))
              ISOLNOD=IPARG(28,NG)
C      
              IF (ISOLNOD == 8 .AND. (KHBE == 14.OR.KHBE == 17)) THEN 
                JJ = JJ + NEL*(6+9*NPT)
              ELSEIF (ISOLNOD == 8 .AND. NPT == 8 .AND.
     .            KHBE /= 14  .AND. KHBE /= 15) THEN          
                JJ = JJ + NEL*(8+7*NPT)
              ELSEIF ((ISOLNOD == 8 .OR. NPT == 1) .AND.
     .                 KHBE /= 14.AND.KHBE /= 15.AND.KHBE /= 17) THEN
                JJ = JJ + NEL*(8+7*NPT)
              ELSE
                JJ = JJ + NEL*(6+9*NPT)
              ENDIF
            ENDIF
          ENDDO
          SZP0(NSPGROUP+NN)=JJ
          WASZ(2) = WASZ(2)+JJ
          NGF = NGL + 1
        ENDDO
        SZP0(3*NSPGROUP+2)=WASZ(2)
      ENDIF
C
C strain/full
      IF (OUTP_ST(3) == 1) THEN
       NGF = 1
       NGL = 0
       DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO  NG = NGF, NGL
           ITY   =IPARG(5,NG)
           IF(ITY == 1.OR.ITY == 2) THEN
             MLW   =IPARG(1,NG)
             NEL   =IPARG(2,NG)
             KHBE  =IPARG(23,NG)      
             NPT   =ABS(IPARG(6,NG))
             ISOLNOD=IPARG(28,NG)
             JJ = JJ + 3 + NEL*NPT*6
           ENDIF
         ENDDO
         SZP0(2*NSPGROUP+NN)=JJ
         WASZ(3) = WASZ(3)+JJ
         NGF = NGL + 1
       ENDDO
         SZP0(3*NSPGROUP+3) = WASZ(3)
      ENDIF
C      
      RETURN
      END
