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
Chd|  H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_SH_TENSOR           source/output/h3d/h3d_results/h3d_write_sh_tensor.F
Chd|        SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        SH4_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        SIGROTA                       source/output/anim/generate/sigrota.F
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SHELL_TENSOR(ELBUF_TAB,SHELL_TENSOR,IPARG ,ITENS ,INVERT,NELCUT,
     2                   EL2FA    ,NBF   ,TENS  ,EPSDOT,IADP  ,
     3                   NBF_L    ,NBPART,IADG  ,X     ,IXC   ,
     4                   IGEO     ,IXTG  ,IPM   ,STACK,ID_ELEM   ,ITY_ELEM  ,INFO1,
     5                   INFO2    ,IS_WRITTEN_SHELL,IPARTC ,IPARTTG     ,LAYER_INPUT ,IPT_INPUT  ,
     6                   PLY_INPUT,GAUSS_INPUT,IUVAR_INPUT,H3D_PART, KEYWORD,D    ,
     7                   ID       ,BUFMAT   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS,INVERT(*),
     .   EL2FA(*),IXC(NIXC,*), IGEO(NPROPGI,*), 
     .   NELCUT,NBF,IADP(*),NBF_L,NBPART,IADG(NSPMD,*),
     .   IXTG(NIXTG,*),IPM(NPROPMI,*),ID_ELEM(*),ITY_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_SHELL(*),IPARTC(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,ID
      my_real, INTENT(IN),TARGET :: BUFMAT(SBUFMAT)
C     REAL
      my_real
     .   TENS(3,*),EPSDOT(6,*),X(3,*),SHELL_TENSOR(3,*),D(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   A1,A2,A3,THK,CHARD
      my_real
     .   SIGE(MVSIZ,5)
      my_real
     .   VALUE(5)
      REAL R4(18)
      INTEGER I,NG,NEL,NFT,ITY,LFT,NPT,MPT,IPT,NBFUNCT,NCHARD,
     .        N,J,LLT,MLW,ISTRAIN,IL,IR,IS,IT,NPTR,NPTS,NLAY,
     .        IPID,I1,I2,NS1,NS2,ISTRE,IADBUF,NUPARAM,IMAT,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        IHBE,IREP,BUF,NPG,K,ISROT,NUVARV,IVISC,
     .        IPMAT,IGTYP,MATLY,ISUBSTACK,IIGEO,IADI,IPMAT_IPLY,
     .        NPT_ALL,NPTT,ILAY,IUS,ID_PLY,IPANG,IPPOS,IPTHK,OFFSET,ISELECT,
     .        IPLY,IUVAR,IDRAPE
      INTEGER PID(MVSIZ),MAT(MVSIZ),IOK_PART(MVSIZ),JJ(15)
      my_real 
     .   XN(4*MVSIZ) , YN(4*MVSIZ) , ZN(4*MVSIZ), 
     .   DXN(4*MVSIZ) ,DYN(4*MVSIZ) , DZN(4*MVSIZ),STRAIN(3,MVSIZ) 
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
C
      my_real,
     .   DIMENSION(:), POINTER :: DIR_A
C-----------------------------------------------
      
       OFFSET = 0
      SIGE(1:MVSIZ,1:5) = ZERO
      DO J=1,18
        R4(J) = ZERO
      ENDDO
      VALUE(1:5) = ZERO
      ISELECT = 1
C
      NPG = 1
      NN1 = 1
      NN2 = NN1 
      NN3 = NN2 
      NN4 = NN3 + NUMELQ
      NN5 = NN4 + NUMELC
      NN6 = NN5 + NUMELTG
      NN7 = NN6 
      NN8 = NN7 
      NN9 = NN8 
      NN10= NN9 
C
      DO I=1,NUMELC+NUMELTG
         IS_WRITTEN_SHELL(I) = 0
      ENDDO
C
      DO 490 NG=1,NGROUP
C       IF(ANIM_K == 0.AND.IPARG(8,NG) == 1)GOTO 490
        MLW     = IPARG(1,NG)
        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        ITY     = IPARG(5,NG)
        IGTYP   = IPARG(38,NG)
        ISROT   = IPARG(41,NG)
        ISTRAIN = IPARG(44,NG)
        ISUBSTACK = IPARG(71,NG)
        IDRAPE = ELBUF_TAB(NG)%IDRAPE
        LFT=1
        LLT=NEL
        IOK_PART(1:NEL) = 0   
!
        DO I=1,15
          JJ(I) = NEL*(I-1)
        ENDDO
!
        IF (MLW /= 13) THEN
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
          IF(ITY == 2)THEN
            DO I=1,NEL
              N = I + NFT
              SHELL_TENSOR(1,OFFSET+NFT+I) = ZERO
              SHELL_TENSOR(2,OFFSET+NFT+I) = ZERO
              SHELL_TENSOR(3,OFFSET+NFT+I) = ZERO
            ENDDO
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
          ELSEIF (ITY == 3 .OR. ITY == 7) THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NLAY = ELBUF_TAB(NG)%NLAY
            NPG  = NPTR*NPTS
C
            IHBE = IPARG(23,NG)
            IF (ITY == 3) THEN
              N0 = 0
              NNI = NN4
              IF (IHBE == 11) NPG = 4
            ELSE
              N0 = NUMELC
              NNI = NN5
              IF (IHBE == 30) NPG = 3 !dkt18
            ENDIF
c
            IF (ITY == 3) OFFSET = 0
            IF (ITY == 7) OFFSET = NUMELC
c
            DO  I=1,NEL 
              IF (ITY == 3) THEN
                ID_ELEM(OFFSET+NFT+I) = IXC(NIXC,NFT+I)
                ITY_ELEM(OFFSET+NFT+I) = 3
                IF( H3D_PART(IPARTC(NFT+I)) == 1) IOK_PART(I) = 1
              ELSEIF (ITY == 7) THEN 
                ID_ELEM(OFFSET+NFT+I) = IXTG(NIXTG,NFT+I)
                ITY_ELEM(OFFSET+NFT+I) = 7
                IF( H3D_PART(IPARTTG(NFT+I)) == 1) IOK_PART(I) = 1
              ENDIF
            ENDDO

c                      
C
            IF (MLW == 0) GOTO 490
C
            NPT   = IABS(IPARG(6,NG))
C
            A1    = ZERO
            A2    = ZERO
            A3    = ZERO         
            ISTRE = 1
            IPT   = 1

            MPT   = MAX(1,NPT)
            IF(NPT == 0) MPT = 0
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IPT=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
              ENDDO
              IF (NLAY == 1) MPT  = MAX(1,NPT_ALL)
            ENDIF
c
            ILAY = LAYER_INPUT
            IPT = IPT_INPUT
            IPLY = PLY_INPUT
c            IG = IGAUSS_INPUT
            IUVAR = IUVAR_INPUT
            IF (ILAY == -2) ILAY = 1   !Lower
            IF (ILAY == -3) ILAY = NLAY  !Upper
            IF (IPT == -2) IPT = 1
            IF (IPT == -3) IPT = NPT 
  
c
            DO I=1,NEL      
              DO K=1,5      
              SIGE(I,K) = ZERO  
              ENDDO      
            ENDDO
C------------------------
C        STRESS
C------------------------
            IF (KEYWORD == 'TENS/STRESS/MEMB' .OR.
     .          KEYWORD == 'TENS/STRESS/BEND' .OR.
     .          KEYWORD == 'TENS/STRESS' ) THEN
              IF (ITY == 3) THEN
                IPID = IXC(6,NFT+1)
                DO I=1,NEL
                  MAT(I)=IXC(1,NFT+I)
                  PID(I)=IXC(6,NFT+I)
                ENDDO
              ELSE  ! ITY == 7
                IPID = IXTG(5,NFT+1)
                DO I=1,NEL
                  MAT(I)=IXTG(1,NFT+I)
                  PID(I)=IXTG(5,NFT+I)
                ENDDO
              ENDIF
c
              IREP = IGEO(6,IPID)
              IVISC = 0
              NUVARV = 0
              IF (MLW == 25) THEN
                IF (IGTYP == 11) THEN                                
                  IPMAT = 100                                     
                  DO N=1,MPT                                     
                    DO I=1,NEL                                   
                      MATLY = IGEO(IPMAT+N,PID(I))          
                      NUVARV = MAX(NUVARV, IPM(225,MATLY ))   
                      IF (IPM(222,MATLY) > 0 ) IVISC = 1       
                    ENDDO                                       
                  ENDDO                                          
                ELSEIF (IGTYP == 9 .OR. IGTYP == 10) THEN            
                  DO N=1,MPT
                    DO I=1,NEL                                  
                      MATLY = MAT(I)                            
                      NUVARV = MAX(NUVARV, IPM(225,MATLY))     
                      IF (IPM(222,MATLY) > 0 ) IVISC = 1         
                    ENDDO                                      
                  ENDDO                                         
                ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR.IGTYP == 52)THEN  
                  IPMAT   = 2 + NLAY
                  IPMAT_IPLY   = IPMAT + NLAY
                  NUVARV = 0                                      
                  DO N=1,NLAY                                      
                    DO I=1,NEL                                  
                      MATLY = STACK%IGEO(IPMAT+N,ISUBSTACK)            
                      NUVARV = MAX(NUVARV, IPM(225,MATLY))     
                      IF(IPM(222,MATLY) > 0 ) IVISC = 1         
                    END DO                                      
                  ENDDO                                          
                ENDIF                                             
              ENDIF ! mlw == 25                                 
            ENDIF 
C--------------------------------------------------
            IF (KEYWORD == 'TENS/STRESS/MEMB') THEN
C--------------------------------------------------
              NS1 = 5
              NS2 = 3
              A1  = ONE
              A2  = ZERO
              DO I=1,NEL
                VALUE(1) = A1 * GBUF%FOR(JJ(1)+I) + A2 * GBUF%MOM(JJ(1)+I)
                VALUE(2) = A1 * GBUF%FOR(JJ(2)+I) + A2 * GBUF%MOM(JJ(2)+I)
                VALUE(3) = A1 * GBUF%FOR(JJ(3)+I) + A2 * GBUF%MOM(JJ(3)+I)
                CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                 VALUE)    
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/STRESS/BEND') THEN
C--------------------------------------------------
              NS1 = 5
              NS2 = 3
              A1  = ZERO
              A2  = ONE
              DO I=1,NEL
                VALUE(1) = A1 * GBUF%FOR(JJ(1)+I) + A2 * GBUF%MOM(JJ(1)+I)
                VALUE(2) = A1 * GBUF%FOR(JJ(2)+I) + A2 * GBUF%MOM(JJ(2)+I)
                VALUE(3) = A1 * GBUF%FOR(JJ(3)+I) + A2 * GBUF%MOM(JJ(3)+I)
                CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                 VALUE)    
              ENDDO

C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/STRESS') THEN
C--------------------------------------------------
              NS1 = 5
              NS2 = 3
              A1  = ONE
              A2  = ZERO
c ILAYER=NULL NPT=NULL
              IF(MPT == 0 .AND. IPT == 1 ) THEN
                DO I=1,NEL
                  VALUE(1) =  GBUF%FOR(JJ(1)+I) - SIX*GBUF%MOM(JJ(1)+I) 
                  VALUE(2) =  GBUF%FOR(JJ(2)+I) - SIX*GBUF%MOM(JJ(2)+I) 
                  VALUE(3) =  GBUF%FOR(JJ(3)+I) - SIX*GBUF%MOM(JJ(3)+I)
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE)    
                ENDDO  
              ELSEIF(MPT == 0 .AND. IPT_INPUT == -3 ) THEN
                DO I=1,NEL
                  VALUE(1) =  GBUF%FOR(JJ(1)+I) + SIX*GBUF%MOM(JJ(1)+I) 
                  VALUE(2) =  GBUF%FOR(JJ(2)+I) + SIX*GBUF%MOM(JJ(2)+I) 
                  VALUE(3) =  GBUF%FOR(JJ(3)+I) + SIX*GBUF%MOM(JJ(3)+I)
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE)    
                ENDDO  
              
              ELSEIF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
                DO I=1,NEL 
c                 VALUE(I) =
                ENDDO  
c PLY=IPLY NPT=IPT             
              ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN 
c
                IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                  DO J=1,NLAY
                    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    IF (ID_PLY  == IPLY) THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      NPTT = BUFLY%NPTT
                      IF (IHBE == 11 .AND. IPT <= NPTT) THEN
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,5           
                      VALUE(K) = VALUE(K) + LBUF%SIG(JJ(K) + I)/NPG
                          ENDDO
                        ENDDO                 
                      ENDDO
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE) 
                    ENDDO
                      ELSEIF (IPT <= NPTT) THEN ! no batoz
                    LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(1,1,IPT)   
                    DO I=1,NEL  
                      DO K=1,5            
                        VALUE(K) = LBUF%SIG(JJ(K) + I)
                      ENDDO 
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE) 
                        ENDDO
                       ELSE  ! initialisation
                         DO I=1,NEL  
                      DO K=1,5            
                        VALUE(K) = ZERO
                      ENDDO 
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE) 
                         ENDDO   
                       ENDIF   
                      ENDIF          
                   ENDDO
c
                ELSEIF (IGTYP == 52) THEN
                  DO J=1,NLAY
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK) - NUMSTACK)
                    IF (ID_PLY  == IPLY) THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      NPTT = BUFLY%NPTT
                      IF (IHBE == 11 .AND. IPT <= NPTT) THEN
                    DO I=1,NEL    
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,5           
                      VALUE(K) = VALUE(K) + LBUF%SIG(JJ(K) + I)/NPG
                          ENDDO
                        ENDDO                 
                      ENDDO  
                           CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                   VALUE) 
                    ENDDO
                      ELSEIF (IPT <= NPTT) THEN ! no batoz
                    LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(1,1,IPT)   
                    DO I=1,NEL     
                      DO K=1,5            
                        VALUE(K) = LBUF%SIG(JJ(K) + I)
                      ENDDO
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                   VALUE)                
                    ENDDO
                      ELSE  ! initialisation 
                         DO I=1,NEL  
                      DO K=1,5            
                        VALUE(K) = ZERO
                      ENDDO 
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE) 
                         ENDDO     
                      ENDIF
                    ENDIF  ! ID_PLY
                  ENDDO ! NLAY
                ENDIF   
c PLY=NULL ILAYER=ILAY NPT=IPT 
              ELSEIF (IPLY == -1 .AND. ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN       
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  IF (IGTYP == 51) THEN
                    DO J=1,NLAY
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      NPTT = BUFLY%NPTT
                      IF (IHBE == 11 .AND. IPT <= NPTT) THEN
                        DO I=1,NEL  
                          DO IR=1,NPTR                
                            DO IS=1,NPTS 
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                              DO K=1,5           
                          VALUE(K) = VALUE(K) + LBUF%SIG(JJ(K) + I)/NPG
                              ENDDO
                            ENDDO                 
                          ENDDO   
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                         VALUE) 
                        ENDDO
                      ELSEIF (IPT <= NPTT) THEN ! no batoz
                        LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(1,1,IPT)   
                        DO I=1,NEL  
                          DO K=1,5            
                            VALUE(K) = LBUF%SIG(JJ(K) + I)
                          ENDDO 
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                         VALUE)             
                        ENDDO
                      ENDIF
                    ENDDO
c
                  ELSEIF (IGTYP == 52) THEN
                    DO J=1,NLAY
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      NPTT = BUFLY%NPTT
                      IF (IHBE == 11 .AND. IPT <= NPTT) THEN
                        DO I=1,NEL    
                          DO IR=1,NPTR                
                            DO IS=1,NPTS 
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                              DO K=1,5           
                          VALUE(K) = VALUE(K) + LBUF%SIG(JJ(K) + I)/NPG
                              ENDDO
                            ENDDO                 
                          ENDDO    
                        ENDDO
                      ELSEIF (IPT <= NPTT) THEN ! no batoz
                        LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(1,1,IPT)   
                        DO I=1,NEL     
                          DO K=1,5            
                            VALUE(K) = LBUF%SIG(JJ(K) + I)
                          ENDDO
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                       VALUE)                
                        ENDDO
                      ENDIF
                    ENDDO
                  ENDIF 
                ENDIF
c PLY=NULL ILAYER=IL NPT=NULL
              ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN               
                  IF (MLW == 1)THEN      
                    DO I=1,NEL     
                      DO J=1,5          
                        VALUE(J) = GBUF%FOR(JJ(J) + I)
                      ENDDO
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                            VALUE)             
                    ENDDO
                  ELSE             
                    IF ( (MLW == 25.OR.MLW == 15.OR.MLW>=28) .AND. IREP == 1)THEN
                         CALL SIGROTA(1  ,NEL ,NFT  ,ILAY     ,NEL    ,
     2                          NS1  ,X   ,IXC  ,ELBUF_TAB(NG),
     3                          SIGE ,ITY ,IXTG ,IHBE     ,ISTRAIN,
     4                          IVISC)
                       DO I=1,NEL
                         VALUE(1) = A1 * SIGE(I,1)
                         VALUE(2) = A1 * SIGE(I,2)
                         VALUE(3) = A1 * SIGE(I,3)
                         CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                          VALUE)
                       ENDDO      
            
                    ELSE IF ( (MLW == 25.OR.MLW == 15.OR.MLW>=28) .AND. IREP == 0)THEN
c
                      IF (IHBE == 11) THEN  ! Batoz shell
                        DO I=1,NEL
                          DO J=1,5
                            SIGE(I,J) = ZERO
                          ENDDO
                        ENDDO
C
                        DO I=1,NEL
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              IF (NLAY > 1) THEN
                          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                              ELSE
                          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                              ENDIF
                              DO J=1,5
                          SIGE(I,J) = SIGE(I,J) + LBUF%SIG(JJ(J) + I)/NPG
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO
C
C                  viscous stress added for animation
C 
                        IF (IVISC > 0) THEN
                          DO I=1,NEL
                            DO IR=1,NPTR
                              DO IS=1,NPTS
                          IF (NLAY > 1) THEN
                            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                          ELSE
                            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                          ENDIF
                          DO J=1,5             
                            SIGE(I,J) = SIGE(I,J) + LBUF%VISC(JJ(J) + I)/NPG 
                          ENDDO            
                              ENDDO
                            ENDDO  
                          ENDDO  
                        ENDIF  !  IF (IVISC > 0)
                      ELSE  ! not Batoz shell
                        DO I=1,NEL
                          IF (NLAY > 1) THEN             
                            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)   
                          ELSE                 
                            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)   
                          ENDIF       
                          DO J=1,5
                            SIGE(I,J) = LBUF%SIG(JJ(J) + I)
                          ENDDO
                        ENDDO

                        IF (IVISC > 0) THEN  
                          IF (NLAY > 1) THEN             
                            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)   
                          ELSE                 
                            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)   
                          ENDIF                
                          DO I=1,NEL    
                            DO J=1,5              
                              SIGE(I,J) = SIGE(I,J) +  LBUF%VISC(JJ(J) + I)
                            ENDDO           
                          ENDDO           
                        ENDIF  !  IF (IVISC > 0)               
                      ENDIF  !  IF (IHBE == 11)
C
                      IF (IHBE == 11) THEN
                        IF (IGTYP == 9) THEN
                          DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA  
                          CALL UROTOV(1,NEL,SIGE,DIR_A,NEL)
                        ELSE
                         IF(IDRAPE > 0 .AND. IGTYP /= 17) THEN
                           IF (NLAY > 1) THEN 
                               DIR_A => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF_DIR(1)%DIRA
                            ELSE                                            
                               DIR_A => ELBUF_TAB(NG)%BUFLY(1)%LBUF_DIR(1)%DIRA
                            ENDIF                       
                         ELSE 
                            IF (NLAY > 1) THEN          
                              DIR_A => ELBUF_TAB(NG)%BUFLY(ILAY)%DIRA
                            ELSE              
                              DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA
                            ENDIF
                          ENDIF 
                          CALL UROTOV(1,NEL,SIGE,DIR_A,NEL)
                        ENDIF
                      ELSE
                        IF(IDRAPE > 0 .AND. IGTYP /= 17) THEN
                           IF (NLAY > 1) THEN                              
                              DIR_A => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF_DIR(1)%DIRA
                           ELSE                                            
                               DIR_A => ELBUF_TAB(NG)%BUFLY(1)%LBUF_DIR(1)%DIRA
                           ENDIF         
                        ELSE         
                          IF (NLAY > 1) THEN        
                             DIR_A => ELBUF_TAB(NG)%BUFLY(ILAY)%DIRA
                          ELSE            
                            DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA
                          ENDIF
                        ENDIF ! idrape                 
                        CALL UROTOV(1,NEL,SIGE,DIR_A,NEL)
                      ENDIF  !  IF (IHBE == 11)
c
                      DO I=1,NEL
                        VALUE(1) = A1 * SIGE(I,1)
                        VALUE(2) = A1 * SIGE(I,2)
                        VALUE(3) = A1 * SIGE(I,3)
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                           VALUE)
                      ENDDO
                    ELSE
                      IF(ILAY <= NLAY)THEN
                        IF (IHBE == 11) THEN             
                          DO I=1,NEL   
                            DO IR=1,NPTR            
                              DO IS=1,NPTS            
                                LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)  
                                DO J=1,5            
                                  VALUE(J) = VALUE(J) + LBUF%SIG(JJ(J) + I)/NPG
                                ENDDO
                              ENDDO              
                            ENDDO
                            CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                               VALUE)    
                          ENDDO               
                        ELSE !ihbe           
                          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1) 
                          DO I=1,NEL   
                            DO J=1,5              
                              VALUE(J) = LBUF%SIG(JJ(J) + I)
                            ENDDO  
                            CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                             VALUE)               
                          ENDDO
                        ENDIF !IHBE
                      ENDIF !ILAY
                    ENDIF !MLAW
                  ENDIF!MLAW 1

                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN              
                  IF (MLW == 1)THEN      
                    DO I=1,NEL     
                      DO J=1,5          
                        VALUE(J) = GBUF%FOR(JJ(J) + I)
                      ENDDO
                       CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                            VALUE)             
                    ENDDO
                  ELSE
                    IF (IHBE == 11) THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                      DO I=1,NEL          
                        I1 = (I-1)*5    
                        DO J=1,5             
                          VALUE(J) = BUFLY%SIGPT(I1+J)
                        ENDDO
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                               VALUE)
                      ENDDO
                    ELSE ! no batoz
                      BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                      NPTT = BUFLY%NPTT
                      DO J=1,5
                        DO I=1,NEL
                          DO IT=1,NPTT
                            VALUE(J) = VALUE(J) + BUFLY%LBUF(1,1,IT)%SIG(JJ(J) + I)/NPTT
                          ENDDO
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                       VALUE)
                        ENDDO
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN

                IF (IGTYP == 1 .OR. IGTYP == 9) THEN              
                  IF (MLW == 1)THEN      
                    DO I=1,NEL     
                      DO J=1,5          
                        VALUE(J) = GBUF%FOR(JJ(J) + I)
                      ENDDO
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                         VALUE)             
                    ENDDO
                  ELSE !(MLW == 1)
c
                    IF(IGTYP == 9 .AND. (MLW == 25.OR.MLW == 15.OR.MLW>=28) .AND. IREP == 1)THEN
                      CALL SIGROTA(1  ,NEL ,NFT  ,IPT     ,NEL    ,
     2                       NS1  ,X   ,IXC  ,ELBUF_TAB(NG),
     3                       SIGE ,ITY ,IXTG ,IHBE     ,ISTRAIN,
     4                       IVISC)
                      DO I=1,NEL
                        VALUE(1) = A1 * SIGE(I,1)
                        VALUE(2) = A1 * SIGE(I,2)
                        VALUE(3) = A1 * SIGE(I,3)
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                           VALUE)
                      ENDDO

                    ELSE IF(IGTYP == 9 .AND. (MLW == 25.OR.MLW == 15.OR.MLW>=28) .AND. IREP == 0)THEN
c
                      IF (IHBE == 11) THEN  ! Batoz shell
                        DO I=1,NEL
                          DO J=1,5
                            SIGE(I,J) = ZERO
                          ENDDO
                        ENDDO
C
                        DO I=1,NEL
                          DO IR=1,NPTR
                            DO IS=1,NPTS
                              IF (NLAY > 1) THEN
                          LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,1)
                              ELSE
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)
                              ENDIF
                              DO J=1,5
                          SIGE(I,J) = SIGE(I,J) + LBUF%SIG(JJ(J) + I)/NPG
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO
C
C               viscous stress added for animation
C 
                        IF (IVISC > 0) THEN
                          DO I=1,NEL
                            DO IR=1,NPTR
                              DO IS=1,NPTS
                          IF (NLAY > 1) THEN
                            LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,1)
                          ELSE
                            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)
                          ENDIF
                          DO J=1,5             
                            SIGE(I,J) = SIGE(I,J) + LBUF%VISC(JJ(J) + I)/NPG 
                          ENDDO            
                              ENDDO
                            ENDDO  
                          ENDDO  
                        ENDIF  !  IF (IVISC > 0)
                      ELSE  ! not Batoz shell
                        DO I=1,NEL
                          IF (NLAY > 1) THEN             
                            LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1)   
                          ELSE                 
                            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)   
                          ENDIF       
                          DO J=1,5
                            SIGE(I,J) = LBUF%SIG(JJ(J) + I)
                          ENDDO
                        ENDDO

                        IF (IVISC > 0) THEN  
                          IF (NLAY > 1) THEN             
                            LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1)   
                          ELSE                 
                            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)   
                          ENDIF                
                          DO I=1,NEL    
                            DO J=1,5              
                              SIGE(I,J) = SIGE(I,J) +  LBUF%VISC(JJ(J) + I)
                            ENDDO           
                          ENDDO           
                        ENDIF  !  IF (IVISC > 0)               
                      ENDIF  !  IF (IHBE == 11)
C
                      IF (IHBE == 11) THEN
                        IF (IGTYP == 9) THEN
                          DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA  
                          CALL UROTOV(1,NEL,SIGE,DIR_A,NEL)
                        ELSE
                         IF(IDRAPE > 0 .AND. IGTYP /= 17) THEN
                           IF (NLAY > 1) THEN                              
                                 DIR_A => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF_DIR(1)%DIRA
                           ELSE                                            
                                 DIR_A => ELBUF_TAB(NG)%BUFLY(1)%LBUF_DIR(1)%DIRA
                          ENDIF
                        ELSE                         
                          IF (NLAY > 1) THEN          
                            DIR_A => ELBUF_TAB(NG)%BUFLY(IPT)%DIRA
                          ELSE              
                            DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA
                          ENDIF
                        ENDIF  
                          CALL UROTOV(1,NEL,SIGE,DIR_A,NEL)
                        ENDIF
                      ELSE
                        IF(IDRAPE > 0 .AND. IGTYP /= 17) THEN
                          IF (NLAY > 1) THEN                              
                               DIR_A => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF_DIR(1)%DIRA
                          ELSE                                            
                               DIR_A => ELBUF_TAB(NG)%BUFLY(1)%LBUF_DIR(1)%DIRA
                          ENDIF
                        ELSE                         
                          IF (NLAY > 1) THEN        
                             DIR_A => ELBUF_TAB(NG)%BUFLY(IPT)%DIRA
                          ELSE            
                             DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA
                          ENDIF 
                        ENDIF ! idrape                  
                        CALL UROTOV(1,NEL,SIGE,DIR_A,NEL)
                      ENDIF  !  IF (IHBE == 11)
c
                      DO I=1,NEL
                        VALUE(1) = A1 * SIGE(I,1)
                        VALUE(2) = A1 * SIGE(I,2)
                        VALUE(3) = A1 * SIGE(I,3)
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                           VALUE)
                      ENDDO

                    ELSE
                      IF(IPT <= MPT)THEN
                        IF (IHBE == 11) THEN             
                          DO I=1,NEL   
                            DO IR=1,NPTR            
                              DO IS=1,NPTS            
                                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)  
                                DO J=1,5            
                                  VALUE(J) = VALUE(J) + LBUF%SIG(JJ(J) + I)/NPG
                                ENDDO
                              ENDDO              
                            ENDDO
                            CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                                VALUE)    
                          ENDDO               
                        ELSE           
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                          DO I=1,NEL   
                            DO J=1,5              
                              VALUE(J) = LBUF%SIG(JJ(J) + I)
                            ENDDO  
                            CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                               VALUE)               
                          ENDDO
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSE IF (KEYWORD == 'TENS/STRAIN/MEMB') THEN
C--------------------------------------------------
              NS1 = 8
              NS2 = 8
              A1 = ONE
              A2 = ZERO
              IF (ISTRAIN == 1) THEN
                DO I=1,NEL
                  N = I + NFT
                  THK = GBUF%THK(I)
                  J  = EL2FA(NNI+N)
                  VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                  VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                  VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                  VALUE(3) = VALUE(3) * HALF 
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE)
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/STRAIN/BEND') THEN  ! bend
C--------------------------------------------------
              NS1 = 8
              NS2 = 8
              A1 = ZERO
              A2 = ONE
              IF (ISTRAIN == 1) THEN
                DO I=1,NEL
                  N = I + NFT
                  THK = GBUF%THK(I)
                  J  = EL2FA(NNI+N)
                  VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                  VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                  VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                  VALUE(3) = VALUE(3) * HALF 
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE)    
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/STRAIN') THEN  
C--------------------------------------------------
              NS1 = 8
              NS2 = 8
c ILAYER=NULL NPT=NULL
              IF(MPT == 0 .AND. IPT == 1 ) THEN
                  DO I=1,NEL
                    A2 = -HALF*GBUF%THK(I)
                    VALUE(1) =  GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) 
                    VALUE(2) =  GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) 
                    VALUE(3) =  GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I)
                    VALUE(3) = VALUE(3) * HALF 
                    CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                       VALUE)    
                  ENDDO  
              ELSEIF(MPT == 0 .AND. IPT_INPUT == -3 ) THEN
                  DO I=1,NEL
                    A2 = HALF*GBUF%THK(I)
                    VALUE(1) =  GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) 
                    VALUE(2) =  GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) 
                    VALUE(3) =  GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I)
                    VALUE(3) = VALUE(3) * HALF 
                    CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                       VALUE)    
                  ENDDO  
        ELSEIF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
                A1 = ONE
                A2 = ZERO
                IF (ISTRAIN == 1) THEN
                  DO I=1,NEL
                    N = I + NFT
                    THK = GBUF%THK(I)
                    J  = EL2FA(NNI+N)
                    VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                    VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                    VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                    VALUE(3) = VALUE(3) * HALF 
                    CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                     VALUE)
                  ENDDO
                ENDIF 
c PLY=IPLY NPT=IPT
        ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN     
          IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
               IPANG = 1
               IPTHK  = IPANG + NLAY
               IPPOS  = IPTHK + NLAY
               DO J=1,NLAY
                 BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                 NPTT = BUFLY%NPTT
                 IF (IGTYP == 17 .OR. IGTYP == 51) THEN
             ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                 ELSEIF (IGTYP == 52) THEN
                   ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                 ENDIF
                 IF (ID_PLY  == IPLY .AND.
     .               IPT <= NPTT ) THEN
                   A1 = ONE
                   A2 = STACK%GEO(IPPOS+J,ISUBSTACK)+ 
     .                  HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .                     STACK%GEO(IPTHK+J,ISUBSTACK)
                      IF (ISTRAIN == 1) THEN
                       DO I=1,NEL
                         N = I + NFT
                         THK = GBUF%THK(I)
                         VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                         VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                         VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                         VALUE(3) = VALUE(3) * HALF 
                         CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                          VALUE)
                       ENDDO
                      ENDIF  
                    ENDIF  
               ENDDO
        ENDIF
c PLY=NULL ILAYER=ILAY NPT=IPT 
        ELSEIF (IPLY == -1 .AND. ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN       
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
               IPANG = 1
               IPTHK  = IPANG + NLAY
               IPPOS  = IPTHK + NLAY
               BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
               NPTT = BUFLY%NPTT
               A1 = ONE
               A2 = STACK%GEO(IPPOS+ILAY,ISUBSTACK)+ 
     .                    HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .                 STACK%GEO(IPTHK+ILAY,ISUBSTACK)
                  IF (ISTRAIN == 1) THEN
                    DO I=1,NEL
                      N = I + NFT
                      THK = GBUF%THK(I)
                      VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                      VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                      VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                      VALUE(3) = VALUE(3) * HALF 
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                             VALUE)
                    ENDDO
                  ENDIF 
          ENDIF
c PLY=NULL ILAYER=IL NPT=NULL
        ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
                A1 = ONE
                A2 = HALF*(((2*ILAY-ONE)/NLAY)-ONE)
           IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17 .OR.
     .              IGTYP == 51 .OR. IGTYP == 52) THEN
                  IF (ISTRAIN == 1) THEN
                    DO I=1,NEL
                      N = I + NFT
                      THK = GBUF%THK(I)
                      J  = EL2FA(NNI+N)
                      VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                      VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                      VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                      VALUE(3) = VALUE(3) * HALF 
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                        VALUE)
                    ENDDO
                  ENDIF  
         ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
        ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
                A1 = ONE
                A2 = HALF*(((2*IPT-ONE)/MPT)-ONE)
           IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  IF (ISTRAIN == 1) THEN
                    DO I=1,NEL
                      N = I + NFT
                      THK = GBUF%THK(I)
                      J  = EL2FA(NNI+N)
                      VALUE(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                      VALUE(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                      VALUE(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                      VALUE(3) = VALUE(3) * HALF 
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                        VALUE)
                    ENDDO
                  ENDIF 
           ENDIF
        ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/EPSDOT/MEMB') THEN  
C--------------------------------------------------
              A1 = ONE
              A2 = ZERO
              DO I=1,NEL
                THK = GBUF%THK(I)  
                VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                   VALUE)
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/EPSDOT/BEND') THEN  
C--------------------------------------------------
              DO I=1,NEL
                THK = GBUF%THK(I)
                VALUE(1) = EPSDOT(4,I+NFT+OFFSET)
                VALUE(2) = EPSDOT(5,I+NFT+OFFSET)
                VALUE(3) = EPSDOT(6,I+NFT+OFFSET) * HALF 
                CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                   VALUE)
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/EPSDOT') THEN  
C--------------------------------------------------
c ILAYER=NULL NPT=NULL
        IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
                A1 = ONE
                A2 = ZERO
                DO I=1,NEL
                  THK = GBUF%THK(I)    
                  VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                  VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                  VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                   VALUE)
                ENDDO
c PLY=IPLY NPT=IPT
        ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN     
          IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
            A1 = ZERO
            A2 = ZERO
            NS1 = 8
            NS2 = 8
            IPANG = 1
            IPTHK  = IPANG + NLAY
            IPPOS  = IPTHK + NLAY
            DO J=1,NLAY
              IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                  ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
              ELSEIF (IGTYP == 52) THEN
                ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(J)
              NPTT = BUFLY%NPTT
              IF (ID_PLY  == IPLY .AND. IPT <= NPTT) THEN
                A1 = ONE
                A2 = STACK%GEO(IPPOS+J,ISUBSTACK)+ 
     .            HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .               STACK%GEO(IPTHK+J,ISUBSTACK)
                      DO I=1,NEL
                        THK = GBUF%THK(I)  
                        VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                        VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                        VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                              VALUE)
                      ENDDO
              ENDIF
            ENDDO
          ENDIF

c PLY=NULL ILAYER=ILAY NPT=IPT 
        ELSEIF (IPLY == -1 .AND. ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN       
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
            A1 = ZERO
            A2 = ZERO
            NS1 = 8
            NS2 = 8
            IPANG = 1
            IPTHK  = IPANG + NLAY
            IPPOS  = IPTHK + NLAY
            IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                ID_PLY = IGEO(1,STACK%IGEO(2+ILAY,ISUBSTACK))
            ELSEIF (IGTYP == 52) THEN
              ID_PLY = PLY_INFO(1,STACK%IGEO(2+ILAY,ISUBSTACK)-NUMSTACK) 
            ENDIF
            BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
            NPTT = BUFLY%NPTT
            IF (IPT <= NPTT) THEN
              A1 = ONE
              A2 = STACK%GEO(IPPOS+ILAY,ISUBSTACK)+ 
     .            HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .           STACK%GEO(IPTHK+ILAY,ISUBSTACK)
                    DO I=1,NEL
                      N = I + NFT
                      THK = GBUF%THK(I)       
                      VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                      VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                      VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                         VALUE)
                    ENDDO
            ENDIF
         ENDIF
c PLY=NULL ILAYER=IL NPT=NULL
        ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
          IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  A1 = ONE
                  A2 = HALF*(((2*ILAY-ONE)/NLAY)-ONE)
                  DO I=1,NEL
                    N = I + NFT
                    THK = GBUF%THK(I)      
                    VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                    VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                    VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                    CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                           VALUE)
                  ENDDO
          ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  A1 = ONE
                  A2 = STACK%GEO(IPPOS+ILAY,ISUBSTACK)+ 
     .                       HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .                       STACK%GEO(IPTHK+ILAY,ISUBSTACK)
                  DO I=1,NEL
                    N = I + NFT
                    THK = GBUF%THK(I)      
                    VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                    VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                    VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                    CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                           VALUE)
                  ENDDO
         ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
        ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
                A1 = ONE
                A2 = HALF*(((2*IPT-ONE)/MPT)-ONE)
            IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  DO I=1,NEL
                    THK = GBUF%THK(I)      
                    VALUE(1) = A1*EPSDOT(1,I+NFT+OFFSET) + A2*EPSDOT(4,I+NFT+OFFSET)*THK
                    VALUE(2) = A1*EPSDOT(2,I+NFT+OFFSET) + A2*EPSDOT(5,I+NFT+OFFSET)*THK
                    VALUE(3) = (A1*EPSDOT(3,I+NFT+OFFSET) + A2*EPSDOT(6,I+NFT+OFFSET)*THK)* HALF 
                    CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                           VALUE)
                  ENDDO
         ENDIF
        ENDIF
C--------------------------------------------------
            ELSE IF (KEYWORD == 'TENS/STRAIN_ENG') THEN
C--------------------------------------------------
              IF (ITY == 3 ) THEN !4n
                DO I=1,NEL
                  N = I + NFT
                  NNI = IXC(2,N)
                  J = 4*(I-1) +1
                  XN(J)=X(1,NNI)
                  YN(J)=X(2,NNI)
                  ZN(J)=X(3,NNI)
                  DXN(J)=D(1,NNI)
                  DYN(J)=D(2,NNI)
                  DZN(J)=D(3,NNI)
                  NNI = IXC(3,N)
                  XN(J+1)=X(1,NNI)
                  YN(J+1)=X(2,NNI)
                  ZN(J+1)=X(3,NNI)
                  DXN(J+1)=D(1,NNI)
                  DYN(J+1)=D(2,NNI)
                  DZN(J+1)=D(3,NNI)
                  NNI = IXC(4,N)
                  XN(J+2)=X(1,NNI)
                  YN(J+2)=X(2,NNI)
                  ZN(J+2)=X(3,NNI)
                  DXN(J+2)=D(1,NNI)
                  DYN(J+2)=D(2,NNI)
                  DZN(J+2)=D(3,NNI)
                  NNI = IXC(5,N)
                  XN(J+3)=X(1,NNI)
                  YN(J+3)=X(2,NNI)
                  ZN(J+3)=X(3,NNI)
                  DXN(J+3)=D(1,NNI)
                  DYN(J+3)=D(2,NNI)
                  DZN(J+3)=D(3,NNI)
                  STRAIN(1:3,I)=ZERO
                ENDDO
                CALL SH4_TSTRAIN(XN,YN,ZN,DXN,DYN,DZN,STRAIN,NEL)
                DO I=1,NEL
                  VALUE(1:3)= STRAIN(1:3,I)
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE)
                ENDDO
              ELSEIF (ITY == 7) THEN
                DO I=1,NEL
                  N = I + NFT
                  NNI = IXTG(2,N)
                  J = 3*(I-1) +1
                  XN(J)=X(1,NNI)
                  YN(J)=X(2,NNI)
                  ZN(J)=X(3,NNI)
                  DXN(J)=D(1,NNI)
                  DYN(J)=D(2,NNI)
                  DZN(J)=D(3,NNI)
                  NNI = IXTG(3,N)
                  XN(J+1)=X(1,NNI)
                  YN(J+1)=X(2,NNI)
                  ZN(J+1)=X(3,NNI)
                  DXN(J+1)=D(1,NNI)
                  DYN(J+1)=D(2,NNI)
                  DZN(J+1)=D(3,NNI)
                  NNI = IXTG(4,N)
                  XN(J+2)=X(1,NNI)
                  YN(J+2)=X(2,NNI)
                  ZN(J+2)=X(3,NNI)
                  DXN(J+2)=D(1,NNI)
                  DYN(J+2)=D(2,NNI)
                  DZN(J+2)=D(3,NNI)
                  STRAIN(1:3,I)=ZERO
                ENDDO
                CALL SH3_TSTRAIN(XN,YN,ZN,DXN,DYN,DZN,STRAIN,NEL,IHBE)
                DO I=1,NEL
                  VALUE(1:3)= STRAIN(1:3,I)
                  CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                                     VALUE)
                ENDDO
              END IF
C--------------------------------------------------
C-----------------------------------------------
           ELSEIF (KEYWORD == 'TENS/STRESS/TMAX') THEN
C-----------------------------------------------
             DO I=1,NEL
               VALUE(1:3) = GBUF%TM_SIG1(JJ(1:3) + I)
               CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                 VALUE)    
             ENDDO
C-----------------------------------------------
           ELSEIF (KEYWORD == 'TENS/STRESS/TMIN') THEN
C-----------------------------------------------
             DO I=1,NEL
               VALUE(1:3) = GBUF%TM_SIG3(JJ(1:3) + I)
               CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                 VALUE)    
             ENDDO
C-----------------------------------------------
           ELSEIF (KEYWORD == 'TENS/STRAIN/TMAX') THEN
C-----------------------------------------------
             DO I=1,NEL
               VALUE(1:3) = GBUF%TM_STRA1(JJ(1:3) + I)
               CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                 VALUE)    
             ENDDO
C-----------------------------------------------
           ELSEIF (KEYWORD == 'TENS/STRAIN/TMIN') THEN
C-----------------------------------------------
             DO I=1,NEL
               VALUE(1:3) = GBUF%TM_STRA3(JJ(1:3) + I)
               CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,
     .                 VALUE)    
             ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/BSTRESS') THEN
C--------------------------------------------------
              IF (MLW == 87) THEN
               IMAT = IXC(1,NFT+1)                    
               IADBUF = IPM(7,IMAT)                       
               NUPARAM= IPM(9,IMAT)                       
               UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
               NBFUNCT = UPARAM(25) 
               NCHARD = 34 + 2*NBFUNCT +  22
               CHARD  = UPARAM (NCHARD)
              ELSEIF (MLW == 36) THEN
               IMAT = IXC(1,NFT+1)                        
               IADBUF = IPM(7,IMAT)                       
               NUPARAM= IPM(9,IMAT)                       
               UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
               NBFUNCT = UPARAM(1) 
               NCHARD = 2*NBFUNCT + 14
               CHARD  = UPARAM (NCHARD)
              ENDIF
              IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  !global value = mean on gauss points IPs and layers
                IF(ID == -1) THEN ! somme of all backstresses
                  IF(MLW == 36 .AND. CHARD > ZERO) THEN
                    DO I=1,NEL  
                      DO J=1,NLAY
                        BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                        NPTT = BUFLY%NPTT
                        DO IR=1,NPTR                
                          DO IS=1,NPTS 
                             DO IT=1,NPTT  
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG/NPTT/NLAY
                               ENDDO !K
                             ENDDO !IT                 
                           ENDDO !IS
                         ENDDO !IR
                      ENDDO !Jlay  
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE ) 
                    ENDDO !I              
                  ELSEIF(MLW == 78) THEN
                    DO I=1,NEL  
                      DO J=1,NLAY
                        BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                        NPTT = BUFLY%NPTT
                        DO IR=1,NPTR                
                           DO IS=1,NPTS 
                             DO IT=1,NPTT  
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + (LBUF%SIGA(JJ(K) + I)+LBUF%SIGB(JJ(K) + I))/NPG/NPTT/NLAY
                               ENDDO !K
                             ENDDO !IT             
                           ENDDO !IS
                         ENDDO !IR
                      ENDDO !Jlay  
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                  
                    ENDDO !I              
                  ELSEIF(MLW == 87 .AND. CHARD > ZERO) THEN
                   !SIGBXX(I) = SIGB(I,1) + SIGB(I,4) + SIGB(I,7) + SIGB(I,10)
                   !SIGBYY(I) = SIGB(I,2) + SIGB(I,5) + SIGB(I,8) + SIGB(I,11)
                   !SIGBXY(I) = SIGB(I,3) + SIGB(I,6) + SIGB(I,9) + SIGB(I,12)
                    DO I=1,NEL  
                      DO J=1,NLAY
                        BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                        NPTT = BUFLY%NPTT
                        DO IR=1,NPTR                
                           DO IS=1,NPTS 
                             DO IT=1,NPTT  
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + (LBUF%SIGB(JJ(K)   + I ) 
     .                                                     +LBUF%SIGB(JJ(K+3) + I ) 
     .                                                     +LBUF%SIGB(JJ(K+6) + I )
     .                                                     +LBUF%SIGB(JJ(K+9) + I ))/NPG/NPTT/NLAY
                               ENDDO !K
                             ENDDO !IT                 
                           ENDDO !IS
                         ENDDO !IR
                      ENDDO !Jlay  
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                  
                    ENDDO !I              
                  ENDIF ! MLW==
                ELSEIF(ID >0  ) THEN !!
                  IF(MLW == 36.AND. CHARD > ZERO) THEN ! forcement ID=1 y a qu une BS
                    DO I=1,NEL  
                      DO J=1,NLAY
                        BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                        NPTT = BUFLY%NPTT
                        DO IR=1,NPTR                
                          DO IS=1,NPTS 
                             DO IT=1,NPTT  
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG/NPTT/NLAY
                               ENDDO !K
                              ENDDO !IT            
                           ENDDO !IS
                         ENDDO !IR
                      ENDDO !Jlay  
                      CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                  
                    ENDDO !I              
                  ELSEIF(MLW == 78) THEN
                    IF(ID == 1) THEN
                     DO I=1,NEL  
                       DO J=1,NLAY
                         BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                         NPTT = BUFLY%NPTT
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                             DO IT=1,NPTT  
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGA(JJ(K) + I) /NPG/NPTT/NLAY
                               ENDDO !K
                             ENDDO !IT                 
                           ENDDO !IS
                          ENDDO !IR
                       ENDDO !Jlay  
                       CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                   
                     ENDDO !I              
                    ELSEIF(ID == 2) THEN
                      DO I=1,NEL  
                        DO J=1,NLAY
                          BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                          NPTT = BUFLY%NPTT
                          DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               DO IT=1,NPTT  
                                 LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                                 DO K=1,3           
                                    VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I) /NPG/NPTT/NLAY
                                 ENDDO !K
                               ENDDO !IT                 
                             ENDDO !IS
                           ENDDO !IR
                        ENDDO !Jlay  
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                  
                      ENDDO !I              
                    ELSEIF(ID == 3) THEN 
                      DO I=1,NEL  
                        DO J=1,NLAY
                          BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                          NPTT = BUFLY%NPTT
                          DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               DO IT=1,NPTT  
                                 LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                                 DO K=1,3           
                                    VALUE(K) = VALUE(K) + LBUF%SIGC(JJ(K) + I) /NPG/NPTT/NLAY
                                 ENDDO !K
                               ENDDO !IT          
                             ENDDO !IS
                           ENDDO !IR
                        ENDDO !Jlay  
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                   
                      ENDDO !I  
                    ENDIF !ID ==             
                  ELSEIF(MLW == 87.AND. CHARD > ZERO) THEN
                    IF(ID == 1) THEN
                      DO I=1,NEL  
                        DO J=1,NLAY
                          BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                          NPTT = BUFLY%NPTT
                          DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               DO IT=1,NPTT  
                                 LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                                 DO K=1,3           
                                    VALUE(K) = VALUE(K) +  LBUF%SIGB(JJ(K)   + I ) /NPG/NPTT/NLAY
                                 ENDDO !K
                               ENDDO !IT                 
                             ENDDO !IS
                           ENDDO !IR
                        ENDDO !Jlay  
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                 
                      ENDDO !I                
                    ELSEIF(ID == 2) THEN
                      DO I=1,NEL  
                        DO J=1,NLAY
                          BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                          NPTT = BUFLY%NPTT
                          DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               DO IT=1,NPTT  
                                 LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                                 DO K=1,3           
                                    VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+3) + I) /NPG/NPTT/NLAY
                                 ENDDO !K
                               ENDDO !IT                 
                             ENDDO !IS
                           ENDDO !IR
                        ENDDO !Jlay  
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                 
                      ENDDO !I  
                    ELSEIF(ID == 3) THEN 
                      DO I=1,NEL  
                        DO J=1,NLAY
                          BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                          NPTT = BUFLY%NPTT
                          DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               DO IT=1,NPTT  
                                 LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                                 DO K=1,3           
                                    VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+6) + I)  /NPG/NPTT/NLAY
                                 ENDDO !K
                               ENDDO !IT                 
                             ENDDO !IS
                           ENDDO !IR
                        ENDDO !Jlay  
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                 
                      ENDDO !I  
                    ELSEIF( ID == 4) THEN
                      DO I=1,NEL  
                        DO J=1,NLAY
                          BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                          NPTT = BUFLY%NPTT
                          DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               DO IT=1,NPTT  
                                 LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                                 DO K=1,3           
                                    VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+9) + I)/NPG/NPTT/NLAY
                                 ENDDO !K
                               ENDDO !IT                 
                             ENDDO !IS
                          ENDDO !IR
                        ENDDO !Jlay  
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE )                                
                      ENDDO !I  
                    ENDIF !(ID == ) 
                  ENDIF!(MLW ==  
                ENDIF !(ID >0 ) 
C--------------------------------------------------
c PLY=IPLY NPT=IPT pour prop avec ply 17-51-52
C--------------------------------------------------
              ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN 
                DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))  
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK) - NUMSTACK)
                  ENDIF
                  IF (ID_PLY  == IPLY) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    !--------
                    !  LAW36
                    !--------
                    IF (MLW == 36 .AND.( ID == -1 .OR. ID == 1).AND. CHARD > ZERO) THEN
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                             LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                             DO K=1,3           
                                VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG
                             ENDDO !k
                           ENDDO !IS                 
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    !--------
                    !  LAW78
                    !--------
                    ELSEIF (MLW == 78) THEN
                      IF(ID == -1) THEN ! somme of all backstresses
                          DO I=1,NEL  
                            DO IR=1,NPTR                
                              DO IS=1,NPTS 
                                LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                                DO K=1,3           
                                    VALUE(K) = VALUE(K) + (LBUF%SIGA(JJ(K) + I)+LBUF%SIGB(JJ(K) + I))/NPG
                                ENDDO !k
                              ENDDO !IS                 
                            ENDDO !IR
                           CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                          ENDDO ! I
                      ELSEIF(ID ==1 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGA(JJ(K) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==2 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I

                      ELSEIF(ID ==3 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                   VALUE(K) = VALUE(K) + LBUF%SIGC(JJ(K) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ENDIF !ID == -1
                      !--------
                      !  LAW87
                      !--------
                    ELSEIF( MLW == 87 .AND. CHARD > ZERO) THEN
                      IF(ID == -1) THEN ! somme of all backstresses
                        DO I=1,NEL                                                         
                          DO IR=1,NPTR                                                        
                            DO IS=1,NPTS                                                  
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT)              
                              DO K=1,3                                                        
                                 VALUE(K) = VALUE(K) + (LBUF%SIGB(JJ(K)   + I )            
     .                                                     +LBUF%SIGB(JJ(K+3) + I )      
     .                                                     +LBUF%SIGB(JJ(K+6) + I )      
     .                                                     +LBUF%SIGB(JJ(K+9) + I ))/NPG 

                              ENDDO !k                                                    
                            ENDDO !IS                                                          
                          ENDDO !IR                                                       
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==1 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K)   + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==2 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                   VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+3) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==3 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+6) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==4 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+9) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ENDIF !ID == -1
                    ENDIF !(MLW == 
                  END IF !(ID_PLY  == IPLY
                ENDDO !JLAY
C--------------------------------------------------
c PLY=NULL ILAYER=IL NPT=IPT 
C--------------------------------------------------
              ELSEIF (ILAY > 0 .AND. ILAY <= NLAY .AND.  IPT <= MPT .AND. IPT > 0 ) THEN       
               J = ILAY                                                                                               
               IF(IGTYP == 9) J = 1                                                                                   
               BUFLY => ELBUF_TAB(NG)%BUFLY(J)                                                                        
               !--------                                                                                              
               !  LAW36                                                                                               
               !--------                                                                                              
               IF (MLW == 36.AND. (ID==-1 . OR .ID==1).AND. CHARD > ZERO) THEN                                                          
                 DO I=1,NEL                                                                                             
                   DO IR=1,NPTR                                                                                            
                     DO IS=1,NPTS                                                                                      
                       LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT)                                                  
                       DO K=1,3                                                                                            
                          VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG                                                
                       ENDDO !k                                                                                        
                     ENDDO !IS                                                                                              
                   ENDDO !IR                                                                                           
                   CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE)        
                 ENDDO ! I                                                                                             
                !--------                                                                                             
                !  LAW78                                                                                              
                !--------                                                                                             
               ELSEIF (MLW == 78) THEN                                                                                
                 IF(ID == -1) THEN ! somme of all backstresses
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + (LBUF%SIGA(JJ(K) + I)+LBUF%SIGB(JJ(K) + I))/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==1 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGA(JJ(K) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==2 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==3 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGC(JJ(K) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ENDIF !ID == -1
                 !--------   
                 !  LAW87    
                 !--------   
               ELSEIF( MLW == 87 .AND. CHARD > ZERO) THEN
                 IF(ID == -1) THEN ! somme of all backstresses
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                              VALUE(K) = VALUE(K) + (LBUF%SIGB(JJ(K)   + I )
     .                                                +LBUF%SIGB(JJ(K+3) + I ) 
     .                                                +LBUF%SIGB(JJ(K+6) + I )
     .                                                +LBUF%SIGB(JJ(K+9) + I ))/NPG

                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==1 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==2 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+3) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==3 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+6) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ELSEIF(ID ==4 ) THEN  
                    DO I=1,NEL  
                      DO IR=1,NPTR                
                        DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                          DO K=1,3           
                             VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+9) + I)/NPG
                          ENDDO !k
                        ENDDO !IS                 
                      ENDDO !IR
                     CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                    ENDDO ! I
                 ENDIF !ID == -1
               ENDIF !(MLW == 
C--------------------------------------------------
c PLY=NULL ILAYER=IL NPT=NULL ! prop 9-10-11 have layers and not PLY
C--------------------------------------------------
              ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
                IF (IGTYP == 9 .OR.IGTYP == 10 .OR. IGTYP == 11 ) THEN             
                  ! output in  orthotopic frame / elementary                     
                  J = ILAY                                                         
                  IF(IGTYP == 9) J = 1                                             
                  BUFLY => ELBUF_TAB(NG)%BUFLY(J)                                  
                  NPTT = BUFLY%NPTT                                                
                  !--------                                                        
                  !  LAW36                                                         
                  !--------                                                        
                  IF (MLW == 36.AND. (ID==-1 .OR. ID==1) .AND. CHARD > ZERO) THEN ! only one Bstress  
                    DO I=1,NEL                                                       
                       DO IR=1,NPTR
                          DO IS=1,NPTS
                            DO IT=1,NPTT
                             LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                             DO K=1,3           
                                 VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K)   + I)/NPG/NPTT                        
                             ENDDO !K
                            ENDDO!IT
                          ENDDO !IS
                        ENDDO!IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE)
                       VALUE(1:3) = ZERO
                    ENDDO !I
                    !--------
                    !  LAW78
                    !--------
                  ELSEIF (MLW == 78) THEN
                    IF(ID == -1) THEN ! somme of all backstresses
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + (LBUF%SIGA(JJ(K) + I)+LBUF%SIGB(JJ(K) + I))/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                              ENDDO !IS           
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ELSEIF(ID ==1 ) THEN  
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGA(JJ(K) + I)/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                           ENDDO !IS              
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ELSEIF(ID ==2 ) THEN  
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                           ENDDO !IS              
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ELSEIF(ID ==3 ) THEN  
                       DO I=1,NEL                  
                         DO IR=1,NPTR                 
                           DO IS=1,NPTS           
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGC(JJ(K) + I)/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                            ENDDO !IS              
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ENDIF !ID == -1
                    !--------
                    !  LAW87
                    !--------
                  ELSEIF( MLW == 87 .AND. CHARD > ZERO) THEN
                    IF(ID == -1) THEN ! somme of all backstresses
                       DO I=1,NEL  
                          DO IR=1,NPTR            
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                                LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT)    
                                DO K=1,3                                             
                                   VALUE(K) = VALUE(K) + (LBUF%SIGB(JJ(K)   + I )
     .                                                +LBUF%SIGB(JJ(K+3) + I ) 
     .                                                +LBUF%SIGB(JJ(K+6) + I )
     .                                                +LBUF%SIGB(JJ(K+9) + I ))/NPG/NPTT

                                ENDDO !k
                              ENDDO!IT
                           ENDDO !IS             
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                      ENDDO ! I
                    ELSEIF(ID ==1 ) THEN  
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K)   + I)/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                           ENDDO !IS              
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ELSEIF(ID ==2 ) THEN  
                       DO I=1,NEL  
                         DO IR=1,NPTR             
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                   VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+3) + I )/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                           ENDDO !IS              
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ELSEIF(ID ==3 ) THEN  
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+6) + I )/NPG/NPTT
                               ENDDO !k
                              ENDDO!IT
                           ENDDO !IS              
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ELSEIF(ID ==4 ) THEN  
                       DO I=1,NEL  
                         DO IR=1,NPTR             
                           DO IS=1,NPTS 
                              DO IT=1,NPTT
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT) 
                               DO K=1,3           
                                   VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+9) + I )/NPG
                               ENDDO !k
                              ENDDO!IT
                             ENDDO !IS            
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                    ENDIF !ID == -1  
                  ENDIF !(MLW ==     
                ENDIF  !IGTYP ==        
C---------------------------------------------------------------------------
c ILAYER=-1 NPT=IPT IPLY=-1 ID=-1
C---------------------------------------------------------------------------
               ELSE IF(ILAY == -1 .AND. IPT > 0 .AND. IPT<=MPT .AND. IPLY == -1 ) THEN ! output for each layer/PLY as if LAYER==ALL
                DO J=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                  NPTT = BUFLY%NPTT
                  IF (IPT <= NPTT ) THEN            
                    !--------
                    !  LAW36
                    !--------
                    IF (MLW == 36.AND. (ID==-1 .OR. ID==1) .AND. CHARD > ZERO) THEN
                       DO I=1,NEL  
                         DO IR=1,NPTR                
                           DO IS=1,NPTS 
                             LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                             DO K=1,3           
                                VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG
                             ENDDO !k
                           ENDDO !IS                 
                         ENDDO !IR
                        CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                       ENDDO ! I
                      !--------
                      !  LAW78
                      !--------
                    ELSEIF (MLW == 78) THEN
                      IF(ID == -1) THEN ! somme of all backstresses
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + (LBUF%SIGA(JJ(K) + I)+LBUF%SIGB(JJ(K) + I))/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==1 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGA(JJ(K) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==2 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                   VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==3 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGC(JJ(K) + I)/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ENDIF !ID == -1
                      !--------
                      !  LAW87
                      !--------
                    ELSEIF( MLW == 87 .AND. CHARD > ZERO) THEN
                      IF(ID == -1) THEN ! somme of all backstresses
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + (LBUF%SIGB(JJ(K)   + I )
     .                                                     +LBUF%SIGB(JJ(K+3) + I ) 
     .                                                     +LBUF%SIGB(JJ(K+6) + I )
     .                                                     +LBUF%SIGB(JJ(K+9) + I ))/NPG

                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==1 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K)   + I )/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==2 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+3) + I )/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==3 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+6) + I )/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ELSEIF(ID ==4 ) THEN  
                         DO I=1,NEL  
                           DO IR=1,NPTR                
                             DO IS=1,NPTS 
                               LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                               DO K=1,3           
                                  VALUE(K) = VALUE(K) + LBUF%SIGB(JJ(K+9) + I )/NPG
                               ENDDO !k
                             ENDDO !IS                 
                           ENDDO !IR
                          CALL H3D_WRITE_SH_TENSOR(IOK_PART,ISELECT,IS_WRITTEN_SHELL,SHELL_TENSOR,I,OFFSET,NFT,VALUE) 
                         ENDDO ! I
                      ENDIF !ID == -1
                    ENDIF !(MLW == 
                  ENDIF! (IPT <= NPTT )
                ENDDO !JLAY

              END IF
c ........
C---------------------------------------------------------------------------
c            ELSEIF (KEYWORD == 'NEWKEY') THEN ! New Output Example
C---------------------------------------------------------------------------
c ILAYER=NULL NPT=NULL
c              IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
c                  DO I=1,NEL 
c                    VALUE(I) =
c                  ENDDO  
c PLY=IPLY NPT=IPT
c              ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN      
c                IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
c
c                ENDIF
c
c PLY=NULL ILAYER=ILAY NPT=IPT 
c              ELSEIF (IPLY == -1 .AND. ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN       
c                IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
c
c                ENDIF
c PLY=NULL ILAYER=IL NPT=NULL
c              ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
c                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
c
c                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
c
c                ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
c              ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
c                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
c
c                ENDIF
c              ENDIF
            ENDIF

C-----------------------------------------------
C       RNUR
C-----------------------------------------------
          ELSEIF (ITY == 50) THEN
c            DO I=1,NEL
c              N = I + NFT
c              TENS(1,EL2FA(NN9+N)) = ZERO
c              TENS(2,EL2FA(NN9+N)) = ZERO
c              TENS(3,EL2FA(NN9+N)) = ZERO 
c            ENDDO
C-----------------------------------------------
          ELSE
          ENDIF ! IF (MLW /= 13)
        ENDIF ! IF(ITY == 2)
 490  CONTINUE
 500  CONTINUE
C-----------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  SH4_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        C4SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
        SUBROUTINE SH4_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  4*NEL  | R | R | X-coordinate ARRAY (4n quad)
C YN      |  4*NEL  | R | R | Y-coordinate ARRAY (4n quad)
C ZN      |  4*NEL  | R | R | Z-coordinate ARRAY (4n quad)
C DX      |  4*NEL  | R | R | X-Displ ARRAY (4n quad)
C DY      |  4*NEL  | R | R | Y-Displ ARRAY (4n quad)
C DZ      |  4*NEL  | R | R | D-Displ ARRAY (4n quad)
C STRAIN  |  3*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(4,NEL) , YN(4,NEL) , ZN(4,NEL), 
     .   DX(4,NEL) , DY(4,NEL) , DZ(4,NEL),STRAIN(3,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 4)
      my_real 
     .   X0N(NNOD,NEL) , Y0N(NNOD,NEL) , Z0N(NNOD,NEL), 
     .   LXYZ0(3),DETA1(NEL),COREL(2,NNOD),XX,YY,ZZ,
     .   XL2(NEL),XL3(NEL),XL4(NEL),YL2(NEL),
     .   YL3(NEL),YL4(NEL),ZL1(NEL),ZL(NEL),
     .   X13,X24,Y13,Y24,UX1,UY1,UX2,UX3,UX4,UY2,UY3,UY4,
     .   PX1(NEL),PX2(NEL),PY1(NEL),PY2(NEL),
     .   UX13(NEL),UX24(NEL),UY13(NEL),UY24(NEL),
     .   X0L2(NEL),X0L3(NEL),X0L4(NEL),Y0L2(NEL),
     .   Y0L3(NEL),Y0L4(NEL),AREA(NEL),AREA_I(NEL),FXX,FYY,FXY,FYX
C----------------------------------------------
C------Compute coordinates in elementary local sys: actual configuration first
        CALL C4SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,XL4,YL4,ZL1,AREA,NEL)
C------initial configuration :
        DO I=1,NEL
           X0N(1:NNOD,I) = XN(1:NNOD,I)-DX(1:NNOD,I)
           Y0N(1:NNOD,I) = YN(1:NNOD,I)-DY(1:NNOD,I)
           Z0N(1:NNOD,I) = ZN(1:NNOD,I)-DZ(1:NNOD,I)
        ENDDO
        CALL C4SYSG2L(X0N,Y0N,Z0N,X0L2,Y0L2,X0L3,Y0L3,X0L4,Y0L4,ZL,AREA,NEL)
C-------[B0]---remove the origine to the center--------------
       DO I=1,NEL
C-----------EM20=1.0E-20,FOURTH=0.25,HALF=0.5,ZERO=0.--------------
        AREA_I(I) = ONE/MAX(EM20,AREA(I))
        LXYZ0(1)=FOURTH*(X0L2(I)+X0L3(I)+X0L4(I))
        LXYZ0(2)=FOURTH*(Y0L2(I)+Y0L3(I)+Y0L4(I))
        COREL(1,1)=-LXYZ0(1)
        COREL(1,2)=X0L2(I)-LXYZ0(1)
        COREL(1,3)=X0L3(I)-LXYZ0(1)
        COREL(1,4)=X0L4(I)-LXYZ0(1)
        COREL(2,1)=-LXYZ0(2)
        COREL(2,2)=Y0L2(I)-LXYZ0(2)
        COREL(2,3)=Y0L3(I)-LXYZ0(2)
        COREL(2,4)=Y0L4(I)-LXYZ0(2)
C----
        X13 =(COREL(1,1)-COREL(1,3))*HALF
        X24 =(COREL(1,2)-COREL(1,4))*HALF
        Y13 =(COREL(2,1)-COREL(2,3))*HALF
        Y24 =(COREL(2,2)-COREL(2,4))*HALF
        PY2(I) =X13*AREA_I(I)
        PY1(I) =-X24*AREA_I(I)
        PX2(I) =-Y13*AREA_I(I)
        PX1(I) =Y24*AREA_I(I)
       ENDDO
C------ objective disp (or projected disp to free rigide rotation)
         UX1= ZERO
         UY1= ZERO
        DO I=1,NEL
          UX2 = XL2(I)-X0L2(I)
          UY2 = YL2(I)-Y0L2(I)
          UX3 = XL3(I)-X0L3(I)
          UY3 = YL3(I)-Y0L3(I)
          UX4 = XL4(I)-X0L4(I)
          UY4 = YL4(I)-Y0L4(I)
          UX13(I)=UX1-UX3
          UX24(I)=UX2-UX4
          UY13(I)=UY1-UY3
          UY24(I)=UY2-UY4
        ENDDO 
C---------------
C  MEMBRANE [F]-1 = [B0]{d}, [e] ({d} has been projected to free rigide rotation)
C---------------
      DO I=1,NEL
        FXX = PX1(I)*UX13(I)+PX2(I)*UX24(I)
        FYY = PY1(I)*UY13(I)+PY2(I)*UY24(I)
        FYX = PX1(I)*UY13(I)+PX2(I)*UY24(I)
        FXY = PY1(I)*UX13(I)+PY2(I)*UX24(I)
        STRAIN(1,I) = FXX
        STRAIN(2,I) = FYY
        STRAIN(3,I) = 0.5*(FXY+FYX)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C4SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        SH4_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
      SUBROUTINE C4SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,XL4,YL4,ZL1,AREA,NEL) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  4*NEL  | R | R | X-coordinate ARRAY (4n quad)
C YN      |  4*NEL  | R | R | Y-coordinate ARRAY (4n quad)
C ZN      |  4*NEL  | R | R | Z-coordinate ARRAY (4n quad)
C XL2     |    NEL  | R | W | Local X-coordinate of N2 (relative to N1)
C YL2     |    NEL  | R | W | Local Y-coordinate of N2 (relative to N1)
C ZL1     |    NEL  | R | W | Local Z-coordinate of N1 (Z1 -Z1 Z1 -Z1)
C XL3     |    NEL  | R | W | Local X-coordinate of N3 (relative to N1)
C YL3     |    NEL  | R | W | Local Y-coordinate of N3 (relative to N1)
C XL4     |    NEL  | R | W | Local X-coordinate of N4 (relative to N1)
C YL4     |    NEL  | R | W | Local Y-coordinate of N4 (relative to N1)
C AREA    |    NEL  | R | W | AREA of quad
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   XN(4,*) , YN(4,*) , ZN(4,*), 
     .   XL2(NEL),XL3(NEL),XL4(NEL),YL2(NEL),
     .   YL3(NEL),YL4(NEL),ZL1(NEL),AREA(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),
     .   VQ(3,3,NEL), LXYZ0(3),DETA1(NEL),XX,YY,ZZ
C-----------------------------------------------
        DO I=1,NEL
          RX(I)=XN(2,I)+XN(3,I)-XN(1,I)-XN(4,I)
          RY(I)=YN(2,I)+YN(3,I)-YN(1,I)-YN(4,I)
          RZ(I)=ZN(2,I)+ZN(3,I)-ZN(1,I)-ZN(4,I)
          SX(I)=XN(3,I)+XN(4,I)-XN(1,I)-XN(2,I)
          SY(I)=YN(3,I)+YN(4,I)-YN(1,I)-YN(2,I)
          SZ(I)=ZN(3,I)+ZN(4,I)-ZN(1,I)-ZN(2,I)
        ENDDO 
C------Local elem. base:
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,NEL)
C------ Global -> Local Coordinate  FOURTH=0.25 ;
        DO I=1,NEL
          LXYZ0(1)=FOURTH*(XN(1,I)+XN(2,I)+XN(3,I)+XN(4,I))
          LXYZ0(2)=FOURTH*(YN(1,I)+YN(2,I)+YN(3,I)+YN(4,I))
          LXYZ0(3)=FOURTH*(ZN(1,I)+ZN(2,I)+ZN(3,I)+ZN(4,I))
          XX=XN(2,I)-XN(1,I)
          YY=YN(2,I)-YN(1,I)
          ZZ=ZN(2,I)-ZN(1,I)
          XL2(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL2(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          XX=XN(2,I)-LXYZ0(1)
          YY=YN(2,I)-LXYZ0(2)
          ZZ=ZN(2,I)-LXYZ0(3)
          ZL1(I)=VQ(1,3,I)*XX+VQ(2,3,I)*YY+VQ(3,3,I)*ZZ
C          
          XX=XN(3,I)-XN(1,I)
          YY=YN(3,I)-YN(1,I)
          ZZ=ZN(3,I)-ZN(1,I)
          XL3(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL3(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
C
          XX=XN(4,I)-XN(1,I)
          YY=YN(4,I)-YN(1,I)
          ZZ=ZN(4,I)-ZN(1,I)
          XL4(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL4(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          AREA(I)=FOURTH*DETA1(I)
        ENDDO 
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        C3SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        C4SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        SDLENSH                       source/elements/thickshell/solidec/scdtdc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CLSYS3(RX, RY, RZ, SX, SY, SZ, VQ, DET,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   RX(*) , RY(*) , RZ(*), 
     .   SX(*) , SY(*) , SZ(*),
     .   DET(*),VQ(3,3,*)
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C RX      |    NEL  | R | R | X-of covariant vecter g1
C RY      |    NEL  | R | R | Y-of covariant vecter g1
C RZ      |    NEL  | R | R | Z-of covariant vecter g1
C SX      |    NEL  | R | R | X-of covariant vecter g2
C SY      |    NEL  | R | R | Y-of covariant vecter g2
C SZ      |    NEL  | R | R | Z-of covariant vecter g2
C VQ      |3*3*NEL  | R | W | Local elem sys bases
C DET     |    NEL  | R | W | det of g1 ^ g2
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   E1X(NEL), E1Y(NEL), E1Z(NEL),
     .   E2X(NEL), E2Y(NEL), E2Z(NEL),
     .   E3X(NEL), E3Y(NEL), E3Z(NEL), 
     .   C1,C2,CC,C1C1,C2C2,C1_1,C2_1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=1,NEL
C---------E3------------
       E3X(I) = RY(I) * SZ(I) - RZ(I) * SY(I) 
       E3Y(I) = RZ(I) * SX(I) - RX(I) * SZ(I) 
       E3Z(I) = RX(I) * SY(I) - RY(I) * SX(I) 
       DET(I) = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
C ----- EM20=1.0E-20      
       DET(I) = MAX(EM20,DET(I))
       E3X(I) = E3X(I) / DET(I) 
       E3Y(I) = E3Y(I) / DET(I) 
       E3Z(I) = E3Z(I) / DET(I) 
      ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=1,NEL
        C1C1 = RX(I)*RX(I) + RY(I)*RY(I) + RZ(I)*RZ(I)
        C2C2 = SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I)
C ----- ZERO=0., ONE=1.0    
        IF(C1C1 /= ZERO) THEN
          C2_1 = SQRT(C2C2/MAX(EM20,C1C1))
          C1_1 = ONE
        ELSEIF(C2C2 /= ZERO)THEN
          C2_1 = ONE
          C1_1 = SQRT(C1C1/MAX(EM20,C2C2))
        END IF 
        E1X(I) = RX(I)*C2_1+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))*C1_1
        E1Y(I) = RY(I)*C2_1+(SZ(I)*E3X(I)-SX(I)*E3Z(I))*C1_1 
        E1Z(I) = RZ(I)*C2_1+(SX(I)*E3Y(I)-SY(I)*E3X(I))*C1_1
       ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=1,NEL
        C1 = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
        IF ( C1 /= ZERO) C1 = ONE / MAX(EM20,C1)
        E1X(I) = E1X(I)*C1
        E1Y(I) = E1Y(I)*C1
        E1Z(I) = E1Z(I)*C1
        E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
        E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
        E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
       ENDDO 
      DO I=1,NEL
        VQ(1,1,I)=E1X(I)
        VQ(2,1,I)=E1Y(I)
        VQ(3,1,I)=E1Z(I)
        VQ(1,2,I)=E2X(I)
        VQ(2,2,I)=E2Y(I)
        VQ(3,2,I)=E2Z(I)
        VQ(1,3,I)=E3X(I)
        VQ(2,3,I)=E3Y(I)
        VQ(3,3,I)=E3Z(I)
      ENDDO 
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        C3SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        U_FROM_F2                     source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
        SUBROUTINE SH3_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL,IHBE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  3*NEL  | R | R | X-coordinate ARRAY (3n tria)
C YN      |  3*NEL  | R | R | Y-coordinate ARRAY (3n tria)
C ZN      |  3*NEL  | R | R | Z-coordinate ARRAY (3n tria)
C DX      |  3*NEL  | R | R | X-Displ ARRAY (3n tria)
C DY      |  3*NEL  | R | R | Y-Displ ARRAY (3n tria)
C DZ      |  3*NEL  | R | R | D-Displ ARRAY (3n tria)
C STRAIN  |  3*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL,IHBE
      my_real 
     .   XN(3,NEL) , YN(3,NEL) , ZN(3,NEL), 
     .   DX(3,NEL) , DY(3,NEL) , DZ(3,NEL),STRAIN(3,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 3)
      my_real 
     .   X0N(NNOD,NEL) , Y0N(NNOD,NEL) , Z0N(NNOD,NEL), 
     .   DETA1(NEL),XX,YY,ZZ,
     .   XL2(NEL),XL3(NEL),YL2(NEL),YL3(NEL),
     .   UX1,UY1,UX2,UX3,UY2,UY3,
     .   PX2(NEL),PX3(NEL),PY2(NEL),PY3(NEL),
     .   UX21(NEL),UX31(NEL),UY21(NEL),UY31(NEL),
     .   X0L2(NEL),X0L3(NEL),Y0L2(NEL),
     .   Y0L3(NEL),AREA(NEL),AREA_I(NEL),FXX,FYY,FXY,FYX,F(2,2,NEL) 
C----------------------------------------------
C------Compute coordinates in elementary local sys: actual configuration first
        CALL C3SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,AREA,NEL)
C------initial configuration :
        DO I=1,NEL
           X0N(1:NNOD,I) = XN(1:NNOD,I)-DX(1:NNOD,I)
           Y0N(1:NNOD,I) = YN(1:NNOD,I)-DY(1:NNOD,I)
           Z0N(1:NNOD,I) = ZN(1:NNOD,I)-DZ(1:NNOD,I)
        ENDDO
        CALL C3SYSG2L(X0N,Y0N,Z0N,X0L2,Y0L2,X0L3,Y0L3,AREA,NEL)
C-----------[B0]-----------------
       DO I=1,NEL
        AREA_I(I)=0.5/AREA(I)
        PX2(I)= Y0L3(I)*AREA_I(I)
        PY2(I)=-X0L3(I)*AREA_I(I)
        PX3(I)=-Y0L2(I)*AREA_I(I)
        PY3(I)= X0L2(I)*AREA_I(I)
       ENDDO
C------ objective disp (free rigide rotation)
         UX1= ZERO
         UY1= ZERO
        DO I=1,NEL
          UX2 = XL2(I)-X0L2(I)
          UY2 = YL2(I)-Y0L2(I)
          UX3 = XL3(I)-X0L3(I)
          UY3 = YL3(I)-Y0L3(I)
          UX21(I)=UX2-UX1
          UX31(I)=UX3-UX1
          UY21(I)=UY2-UY1
          UY31(I)=UY3-UY1
        ENDDO 
C---------------
C  MEMBRANE [F]-1 = [B0]{d}, [e] ({d} has been projected to free rigide rotation)
      DO I=1,NEL
        F(1,1,I) = PX2(I)*UX21(I)+PX3(I)*UX31(I)
        F(2,2,I) = PY2(I)*UY21(I)+PY3(I)*UY31(I)
        F(2,1,I) = PX2(I)*UY21(I)+PX3(I)*UY31(I)
        F(1,2,I) = PY2(I)*UX21(I)+PY3(I)*UX31(I)
        STRAIN(1,I) = F(1,1,I)
        STRAIN(2,I) = F(2,2,I)
        STRAIN(3,I) = 0.5*(F(1,2,I) + F(2,1,I))
      ENDDO
C--- local sys for 3N isn't really material ----       
      IF (IHBE==3) CALL U_FROM_F2(F,STRAIN,NEL )
C
      RETURN
      END
Chd|====================================================================
Chd|  C3SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
      SUBROUTINE C3SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,AREA,NEL) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  3*NEL  | R | R | X-coordinate ARRAY (3n tria)
C YN      |  3*NEL  | R | R | Y-coordinate ARRAY (3n tria)
C ZN      |  3*NEL  | R | R | Z-coordinate ARRAY (3n tria)
C XL2     |    NEL  | R | W | Local X-coordinate of N2 (relative to N1)
C YL2     |    NEL  | R | W | Local Y-coordinate of N2 (relative to N1)
C XL3     |    NEL  | R | W | Local X-coordinate of N3 (relative to N1)
C YL3     |    NEL  | R | W | Local Y-coordinate of N3 (relative to N1)
C AREA    |    NEL  | R | W | AREA of tria
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   XN(3,*) , YN(3,*) , ZN(3,*), 
     .   XL2(NEL),XL3(NEL),YL2(NEL),YL3(NEL),AREA(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),
     .   VQ(3,3,NEL), DETA1(NEL),XX,YY,ZZ
        DO I=1,NEL
          RX(I)=XN(2,I)-XN(1,I)
          RY(I)=YN(2,I)-YN(1,I)
          RZ(I)=ZN(2,I)-ZN(1,I)
          SX(I)=XN(3,I)-XN(1,I)
          SY(I)=YN(3,I)-YN(1,I)
          SZ(I)=ZN(3,I)-ZN(1,I)
        ENDDO 
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,NEL)
C------ Global -> Local Coordinate  
        DO I=1,NEL
          XX=RX(I)
          YY=RY(I)
          ZZ=RZ(I)
          XL2(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL2(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
C          
          XX=SX(I)
          YY=SY(I)
          ZZ=SZ(I)
          XL3(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL3(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          AREA(I)=0.5*DETA1(I)
        ENDDO 
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  U_FROM_F2                     source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE U_FROM_F2(F,STRAIN,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   F(2,2,NEL), STRAIN(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION 
     .         FMAT(2,2),UM(2,2),IC,I2C,I3C,IU,I2U,I3U,
     .         C11,C21,C31,C12,C22,C32,C13,C23,C33,DETJ0,DETM1,
     .         CC11,CC21,CC31,CC12,CC22,CC32,CC13,CC23,CC33,
     .         A,B,ZZ,A1,A2,A3,A4,ALPHA,C_A,S_A,C_A2,S_A2
C-----------------------------------------------
      DO I=1,NEL
        FMAT(1,1)= F(1,1,I)+1.0
        FMAT(2,2)= F(2,2,I)+1.0
        FMAT(1,2)= F(1,2,I)
        FMAT(2,1)= F(2,1,I)
        C11 = FMAT(1,1)*FMAT(1,1)+FMAT(2,1)*FMAT(2,1)
        C12 = FMAT(1,1)*FMAT(1,2)+FMAT(2,1)*FMAT(2,2)
        C22 = FMAT(1,2)*FMAT(1,2)+FMAT(2,2)*FMAT(2,2)
        CC12 = 0.5*(C11-C22)
        IC  = 0.5*(C11+C22)
        B = SQRT(CC12*CC12+C12*C12)
        CC11 = SQRT(IC + B)
        CC22 = SQRT(IC - B)
        IF (ABS(CC12)<EM20) THEN
         C_A = 0.0
         S_A = 1.0
        ELSE
         ALPHA =0.5*(atan(C12/CC12))
         C_A = COS(ALPHA)
         S_A = SIN(ALPHA)
        END IF
        C_A2 = C_A*C_A
        S_A2 = S_A*S_A
        UM(1,1) = CC11*C_A2+CC22*S_A2
        UM(2,2) = CC11*S_A2+CC22*C_A2
        UM(1,2) = (CC11-CC22)*S_A*C_A
        STRAIN(1,I) = UM(1,1)-ONE
        STRAIN(2,I) = UM(2,2)-ONE
        STRAIN(3,I) = UM(1,2)
       END DO
C
      RETURN
      END
        
