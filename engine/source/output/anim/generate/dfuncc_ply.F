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
Chd|  DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SIGROTA                       source/output/anim/generate/sigrota.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE DFUNCC_PLY(ELBUF_TAB, FUNC , IFUNC,IPARG,GEO,
     .                      IXC  , IXTG , MASS ,PM   ,EL2FA,
     .                      NBF  , IADP , NBF_L,EHOUR,ANIM ,
     .                      NBPART,IADG , IPM  ,IGEO ,THKE ,
     .                      ERR_THK_SH4,ERR_THK_SH3,
     .                      NBF_PXFEMG ,X, STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE PLYXFEM_MOD
      USE ELBUFDEF_MOD     
      USE STACK_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),EL2FA(*),
     .   IFUNC,NBF,NBF_L, NBPART,NBF_PXFEMG,
     .   IADP(*),IADG(NSPMD,*),IPM(NPROPMI,*),
     .   IGEO(NPROPGI,*)
C     REAL
      my_real
     .   FUNC(*), MASS(*) , GEO(NPROPG,*),
     .   EHOUR(*),ANIM(*),PM(NPROPM,*),THKE(*),
     .   ERR_THK_SH4(*), ERR_THK_SH3(*), X(3,*)
      REAL WAL(NBF_L)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3, FAC, DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ), DMAX(MVSIZ),WPMAX(MVSIZ),
     .   FAIL(MVSIZ),SIGE(MVSIZ,5)
      INTEGER I, NG, NEL, ISS, ISC,N, J, MLW, NUVAR, IUS,
     .        ISTRAIN,NN, K1, K2,JTURB,MT,IMID, IALEL,IPID,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NF,
     .        LLL,NINTLAY,NFAIL,
     .        OFFSET,K,II,II_L,INC,KK,IHBE, N13, N14, N15, N16,
     .        NPTM,NPG, NBVU, I1, MPT, NEL5, NEL8,
     .        IPT,BUF,NPTR,NPTS,NPTT,NLAY,IR,IS,PTF,LENF,IL,
     .        IADR,IPMAT,PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),
     .        NEL_PLY,ILAYER,IFLAG,JJ(5)
      INTEGER IE,ISHPLYXFEM,IP,JPID,IPPID,IPLY0,ILAST,ION,NUVARV,
     .        IVISC,IPMAT_IPLY,NUVARD,MAT_IPLY,
     .        MATPLY,LL,IPLYC,I3,I2
      INTEGER PLYS,IPLY,PLYELEMS(NUMELC),ELC,NS1,MATL,
     .   IIGEO,IADI,ISUBSTACK
      REAL R4
      TYPE(G_BUFEL_)    ,POINTER :: GBUF     
      TYPE(BUF_LAY_)    ,POINTER :: BUFLY     
      TYPE(L_BUFEL_)    ,POINTER :: LBUF     
      TYPE(BUF_INTLAY_) ,POINTER :: INTLAY 
      TYPE(BUF_INTLOC_) ,POINTER :: ILBUF
      TYPE(BUF_FAIL_)   ,POINTER :: FBUF
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR 
C-----------------------------------------------
!
      LL = 0
C      
      NEL_PLY = 0
      DO PLYS = 1,NPLYPART
         IPLY = INDX_PLY(PLYS)
         PLYELEMS=0
        DO I=1,PLYSHELL(IPLY)%PLYNUMSHELL
           IPT = PLYSHELL(IPLY)%SHELLIPT(I)
           ELC = PLYSHELL(IPLY)%SHID(I)
           PLYELEMS(ELC)=IPT
        ENDDO
C
      NN1 = 1
      NN3 = 1
      NN4 = NN3 + NUMELQ
      NN5 = NN4 + NUMELC
      NN6 = NN5 + NUMELTG
      IE = 0
      ILAYER = 0
      IFLAG = 0
C
      DO 900 NG=1,NGROUP
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS)
        DO OFFSET = 0,NEL-1,NVSIZ
          NFT = IPARG(3,NG) + OFFSET
          LFT = 1
          LLT = MIN(NVSIZ,NEL-OFFSET)
          ISHPLYXFEM = IPARG(50,NG)
          ISUBSTACK  = IPARG(71,NG)
C-----------------------------------------------
C         COQUES 3 N 4 N
C-----------------------------------------------
         IF (ISHPLYXFEM > 0 .AND.(ITY == 3.OR.ITY == 7))THEN
          GBUF => ELBUF_TAB(NG)%GBUF
          NPT    =IPARG(6,NG)
          ISS    =IPARG(9,NG)
          IHBE   =IPARG(23,NG)
          NPTR = ELBUF_TAB(NG)%NPTR  
          NPTS = ELBUF_TAB(NG)%NPTS  
          NPTT = ELBUF_TAB(NG)%NPTT  
          NLAY = ELBUF_TAB(NG)%NLAY  
          NINTLAY = ELBUF_TAB(NG)%NINTLAY
          NPG  = NPTR*NPTS
          MPT  = IABS(NPT)
!
          DO J=1,5
            JJ(J) = NEL*(J-1)
          ENDDO
!
         DO I=LFT,LLT          
           DO J=1,5            
             SIGE(I,J) = ZERO  
           ENDDO               
         ENDDO
!
c---------
          DO I=LFT,LLT
            EVAR(I) = ZERO
          ENDDO
c---------
C          
C test sur un seul element du groupe          
C
          N = 1 + NFT
          DO  I=LFT,LLT
            N = I + NFT 
            ILAYER = PLYELEMS(N)
            IF (ILAYER > 0) IFLAG = 1
          ENDDO
C          
          IF (IFLAG  == 0) GO TO 900
          ILAYER = IFLAG
          IFLAG = 1
c          
c            IF(ITYP == 3) THEN
c             DO I=LFT,LLT
c                IE = IE + 1
c                 FUNC(EL2FA(NEL_PLY + IE)) = ZERO
c             ENDDO
c           ENDIF
cc            GO TO 900
cc          ENDIF  
C          
cc          IFLAG = 1
c
c---------
          IF (IFUNC == 1)THEN   ! plastic strain
c---------
C 
            DO  I=LFT,LLT
C             for law25, plastic work is negative 
C                     if the layer has reached failure-p
              N = I + NFT 
              ILAYER = PLYELEMS(N)
              BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)
              LBUF  => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(1,1,1)
              IF(ILAYER > 0) THEN
                ! for law25, plastic work < 0 if the layer has reached failure-p
                IF (NPG > 1 .and. BUFLY%LY_PLAPT > 0) THEN
                  EVAR(I) = ABS(BUFLY%PLAPT(I))
                ELSEIF (NPG == 1 .and. BUFLY%L_PLA > 0) THEN
                  EVAR(I) = ABS(LBUF%PLA(I))
                ENDIF 
              ENDIF 
            ENDDO
          ELSEIF (IFUNC == 3) THEN   ! EINT
            DO I=LFT,LLT
c              K1 = 2*(I-1)+1
c              K2 = 2*(I-1)+2
c              EVAR(I) = GBUF%EINT(K1) + GBUF%EINT(K2)
            ENDDO
c---------
          ELSEIF(IFUNC == 5)THEN  ! THK
c---------
            DO I=LFT,LLT
              EVAR(I) =ZERO
            ENDDO
c---------
          ELSEIF(IFUNC == 7)THEN   ! Von Mises
c---------
            DO I=LFT,LLT
               N = I + NFT 
               II = (I-1)*5
              ILAYER = PLYELEMS(N)
               BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)
               IF(ILAYER > 0) THEN
                 S1 = BUFLY%SIGPT(II + 1)
                 S2 = BUFLY%SIGPT(II + 2)
                 S12= BUFLY%SIGPT(II + 3)
                 VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                 EVAR(I) = SQRT(VONM2)
               ENDIF  
            ENDDO
c---------
          ELSEIF (IFUNC == 11)THEN  
c---------
!            IF(MLW == 25.OR.MLW == 15)THEN 
!            NB13 = NB12 + 2*NEL*MAX(1,NPT)
!            NB15 = NB12 + 2*NEL*MAX(1,NPT)
!            NB12 = NB12 + 2*OFFSET*MAX(1,NPT)
!            NB13 = NB13 + 4*OFFSET*MAX(1,NPT)
!cc            DO J=1,NPT
!              N = (ILAYER - 1)*NEL
!              DO I=LFT,LLT
!                N = I + NFT 
!                ILAYER = PLYELEMS(N)
!                IF(ILAYER > 0) THEN
!                   N = (ILAYER - 1)*NEL
!                   K1 = NB13 + N+I
!                   K2 = NB15 + 2*N + 2*I-1 
!                   EVAR(I)=EVAR(I)+BUFEL(K1)*BUFEL(K2)
!                ENDIF 
!              ENDDO 
!            ENDIF 
c---------
          ELSEIF(IFUNC == 12)THEN   
c---------
!            IF(MLW == 25.OR.MLW == 15)THEN 
!              NB13 = NB12 + 2*NEL*MAX(1,NPT)
!              NB15 = NB12 + 2*NEL*MAX(1,NPT)
!              NB12 = NB12 + 2*OFFSET*MAX(1,NPT)
!              NB13 = NB13 + 4*OFFSET*MAX(1,NPT)
!                DO I=LFT,LLT
!                  N = I + NFT 
!                  ILAYER = PLYELEMS(N)
!                  IF(ILAYER > 0) THEN
!                    N = (ILAYER - 1)*NEL  
!                    K1 = NB13 + N+I
!                    K2 = NB15 + 2*N+2*I 
!                    EVAR(I)=EVAR(I)+BUFEL(K1)*BUFEL(K2)
!                  ENDIF  
!                ENDDO 
!            ENDIF  
c---------
          ELSEIF(IFUNC == 13)THEN  ! DAM3
c---------
           IF(MLW == 25.OR.MLW == 15)THEN 
!            IADD  = NB12 + 6*NEL*MAX(1,NPT)
!            IADD  = IADD + OFFSET
!            DO I=LFT,LLT
!              EVAR(I) = BUFEL(IADD+I)
!            ENDDO
           ENDIF
c---------
          ELSEIF (IFUNC>=14.AND.IFUNC<=15) THEN
c---------
C
Cit's available just for law25 and batoz shell
            IPID = IXC(6,NFT+1)
            IREP = IGEO(6,IPID)           
            IF (MLW == 25.AND. IREP == 1) THEN
C +
             IF(ITY == 3)THEN
              DO I=1,NEL
            	MAT(I)=IXC(1,NFT+I)
            	PID(I)=IXC(6,NFT+I)
              END DO				    
            ELSE
              DO I=1,NEL
            	MAT(I)=IXTG(1,NFT+I)
            	PID(I)=IXTG(5,NFT+I)
              END DO				    
            END IF   
             IVISC = 0
             IF(MLW == 25) THEN
               IF(IGTYP == 17)THEN
!                  IIGEO   =  40 + 5*(ISUBSTACK - 1)   
!                  IADI    = IGEO(IIGEO + 3,PID(1))   
!                  IPPID = 2 
                   IPMAT   = 2 + MPT
! old stack organisation           IPMAT = 300
                  NUVARV = 0 
                  DO N=1,NPT
                     IADR = (N-1)*NEL                       
                      DO I=1,NEL                          
                        MATL   = STACK%IGEO(IPMAT+N,ISUBSTACK)
                        IF(IPM(222,MATL) > 0 ) IVISC = 1
                      END DO                                
                  END DO                                       
               END IF  
             ENDIF 
              NS1 = 5
             CALL SIGROTA(LFT ,LLT     ,NFT     ,ILAYER    ,NEL     ,
     2                NS1     ,X       ,IXC     ,ELBUF_TAB(NG) ,
     3                SIGE    ,ITY     ,IXTG    ,IHBE    ,ISTRAIN ,
     4                IVISC   )
              DO I=LFT,LLT
                EVAR(I) = SIGE(I,IFUNC - 13)
              ENDDO
            ELSEIF (MLW == 25 .AND. IREP == 0) THEN
              IUS = IFUNC-13
              IF (IHBE == 11) THEN  
               LENF = NEL*GBUF%G_FORPG/NPG
               DO I=LFT,LLT 
                 EVAR(I) = ZERO
                 N = I + NFT 
                 ILAYER = PLYELEMS(N)
                 IF(ILAYER > 0) THEN 
                    DO  IR=1,NPTR                                       
                      DO  IS=1,NPTS                                     
                        K = NPTR*(IS-1) + IR                            
                        PTF = (K-1)*LENF+1                              
                        EVAR(I) = EVAR(I)+GBUF%FORPG(PTF+JJ(IUS)+I)/NPG
                      ENDDO                                           
                   ENDDO 
                 ENDIF  
               ENDDO 
              ELSE
                DO I=LFT,LLT
                  EVAR(I) = ZERO
                   N = I + NFT 
                   ILAYER = PLYELEMS(N)
                   IF(ILAYER > 0) THEN 
                   EVAR(I) = EVAR(I)+GBUF%FORPG(JJ(IUS)+I)/NPG
                   ENDIF  
                ENDDO
              ENDIF
            ELSE 
                DO I=LFT,LLT
                  N = I + NFT 
                  ILAYER = PLYELEMS(N)
                  IF(ILAYER > 0) THEN
                    EVAR(I) = GBUF%FORPG(JJ(IUS)+I)
                  ENDIF  
                ENDDO
            ENDIF
CCC
c---------
          ELSEIF(IFUNC>=17.AND.IFUNC<=19)THEN           
c---------
            IUS = IFUNC-14
Cit's available just for law25 and batoz shell
            IPID = IXC(6,NFT+1)
            IREP = IGEO(6,IPID)
c            
            IF (MLW == 25.AND. IREP == 1) THEN
             IF(ITY == 3)THEN
              DO I=1,NEL
            	MAT(I)=IXC(1,NFT+I)
            	PID(I)=IXC(6,NFT+I)
              END DO				    
            ELSE
              DO I=1,NEL
            	MAT(I)=IXTG(1,NFT+I)
            	PID(I)=IXTG(5,NFT+I)
              END DO				    
            END IF   
              IVISC = 0
              NUVARV = 0
              IF(MLW == 25) THEN
                IF(IGTYP == 17)THEN
!!                  IIGEO   =  40 + 5*(ISUBSTACK - 1)   
!!                  IADI    = IGEO(IIGEO + 3,PID(1))   
!!                  IPMAT   = IADI + MPT
                IPMAT   = 2 + MPT
! old stack organisation           IPMAT = 300
                  NUVARV = 0
                  DO N=1,NPT
                     IADR = (N-1)*NEL                       
                      DO I=1,NEL                          
                        MATL   = STACK%IGEO(IPMAT+N,ISUBSTACK)
                        IF(IPM(222,MATL) > 0 ) IVISC = 1
                      END DO                                
                  END DO                                       
                END IF  
               ENDIF 
C -               
              NS1 = 5
             CALL SIGROTA(LFT ,LLT     ,NFT     ,ILAYER    ,NEL     ,
     2                NS1     ,X       ,IXC     ,ELBUF_TAB(NG) ,
     3                SIGE    ,ITY     ,IXTG    ,IHBE    ,ISTRAIN ,
     4                IVISC)
              DO I=LFT,LLT
                EVAR(I) = SIGE(I,IFUNC - 14)
              ENDDO
            ELSEIF (MLW == 25 .AND. IREP == 0) THEN
              IF (IHBE == 11) THEN  
               NB13= NB12 + 8*NPG*NEL
               NB14= NB13 + 8*NPG*NEL*ISTRAIN
               NB15= NB14 + MAX(1,NPT)*NPG*NEL
               DO I=LFT,LLT 
                 EVAR(I) = ZERO
                 N = I + NFT 
                 ILAYER = PLYELEMS(N)
                 IF(ILAYER > 0) THEN
                    DO K=1,NPG
                     I1 = NB15+NPT*5*NEL*(K-1)+5*LLT*(ILAYER-1)+(I-1)*5
c                     EVAR(I) = EVAR(I) + BUFEL(I1+ IFUNC - 14)/NPG
                    ENDDO  
                 ENDIF
                ENDDO 
              ELSE
c                IADD  = NB10 + 5 * NEL * (ILAYER - 1)
                DO I=LFT,LLT
                  N = I + NFT 
                  ILAYER = PLYELEMS(N)
                  IF(ILAYER > 0) THEN
c                   IADD  = NB10 + 5 * NEL * (ILAYER - 1)
c                   EVAR(I) = BUFEL(IADD + (I-1)*5 + IFUNC - 14)
                  ENDIF 
                ENDDO
              ENDIF
            ELSE 
cc                IADD  = NB10 + 5 * NEL * (ILAYER - 1)
                DO I=LFT,LLT
                  N = I + NFT 
                  ILAYER = PLYELEMS(N)
                  IF(ILAYER > 0) THEN
c                    IADD  = NB10 + 5 * NEL * (ILAYER - 1)
c                    EVAR(I) = BUFEL(IADD + (I-1)*5 + IFUNC - 14)
                  ENDIF  
                ENDDO
            ENDIF  
          ELSEIF(IFUNC == 26)THEN
            DO I=LFT,LLT
              EVAR(I) = ZERO
            ENDDO         
          ELSEIF(IFUNC == 2155)THEN
c            IADD  = NB3
            DO I=LFT,LLT
              IF (ITY == 3) THEN
                EVAR(I) = ZERO
              ENDIF
              IF (ITY == 7) THEN
                EVAR(I) = ZERO
              ENDIF
            ENDDO
c---------
         ELSEIF(IFUNC>=20.AND.IFUNC<=24)THEN
c          USER VARIABLES from 1 to 5)THEN
c---------
           IUS = IFUNC - 20                                
CCC
           IF(IHBE == 11.AND.
     .       (MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.MLW>=33))THEN 
                NPG=0
C------QBAT----
               IF (ITY == 3.AND.IHBE == 11) THEN
                NPG =4
                FAC = FOURTH
               ENDIF
C------DKT18----
               IF (ITY == 7.AND.IHBE == 11) THEN
                NPG =3
                FAC = THIRD
               ENDIF
C------------------------ 
              NPTM = MAX(1,MPT)
              NEL5 = NEL*5
              NEL8 = NEL*8    
C-------QBAT,DKT18,
              N13=NB12 + NPG*NEL8
              N14=N13  + NPG*NEL8*ISTRAIN
              N15=N14  + NPTM*NPG*NEL
              N16=N15  + NPG*MPT*NEL5
C
              IF (ITY == 7) THEN
               IGTYP = NINT(GEO(12,IXTG(6,NFT+1)))
              ELSE
               IGTYP = NINT(GEO(12,IXC(6,NFT+1)))
              ENDIF
              IF(MPT == 0)THEN
               DO I = LFT, LLT
                EVAR(I) =ZERO
               ENDDO
              ELSE  
               I1  = IUS*NEL
               DO I=LFT,LLT
               N = I + NFT
               IF(NUVAR>=IUS)THEN
                   EVAR(I) = ZERO
                  N = I + NFT 
                  ILAYER = PLYELEMS(N)
                  IF(ILAYER > 0) THEN 
                     NUVAR = ELBUF_TAB(NG)%BUFLY(IPT)%NVAR_MAT
                    IPT = ILAYER
                     DO IR = 1, NPTR
                       DO IS = 1, NPTS                                    
                        UVAR=>ELBUF_TAB(NG)%BUFLY(IPT)%MAT(IR,IS,1)%VAR
                        EVAR(I) = EVAR(I) + UVAR(I1 + I)*FAC             
                   ENDDO 
                   ENDDO 
                  ENDIF 
                ENDIF              
               ENDDO
              ENDIF
           ELSEIF (MLW == 29 .OR. MLW == 30.OR.
     .             MLW == 31.OR.MLW>=33) THEN
C---        USER VARIABLES
             IUS = IFUNC - 20
             DO I=LFT,LLT
               N = I + NFT
                IF (IPM(8,IXC(1,N))>IUS) THEN
               ENDIF
             ENDDO
           ENDIF
c---------
          ELSEIF(IFUNC>=27.AND.IFUNC<=39) THEN
c---------
            IF (MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.MLW>=33)THEN 
              IF (MPT > 0)THEN
                IUS = IFUNC - 22
                DO I=LFT,LLT               
                  N = I + NFT
                  EVAR(I) = ZERO
                  ILAYER = PLYELEMS(N)
                  IF (ILAYER > 0) THEN
                    IPT = ILAYER
                    NUVAR = ELBUF_TAB(NG)%BUFLY(IPT)%NVAR_MAT
                    I1 = IUS*NEL
                    IF (NUVAR>=IUS)THEN
                      DO IR = 1, NPTR
                       DO IS = 1, NPTS
                        UVAR=>ELBUF_TAB(NG)%BUFLY(IPT)%MAT(IR,IS,1)%VAR
                        EVAR(I) = EVAR(I) + UVAR(I1 + I)*FAC
                       ENDDO
                      ENDDO
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDIF 
c---------
          ELSEIF((IFUNC>=40.AND.IFUNC<=2039).OR.
     .           (IFUNC>=2240.AND.IFUNC<=10139)) THEN
c---------
            IF (IFUNC>=40.AND.IFUNC<=2039) THEN
              IUS = (IFUNC - 39)/100
              IPT = MOD ((IFUNC - 39), 100)  
            ELSEIF (IFUNC>=2240.AND.IFUNC<=10139) THEN 
              IUS = ((IFUNC - 2239)/100) +20  
              IPT = MOD ((IFUNC - 2239), 100)  
            ENDIF  
            IF(IPT == 0)THEN
              IPT = 100
              IUS = IUS -1 
            ENDIF
            IF (NLAY > 1) THEN  
              IL  = IPT         
              IPT = 1           
            ELSE                
              IL  = 1           
            ENDIF               
            IPT = ILAYER
            IF (MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.MLW>=33)THEN 
                NPG=0
C              ------QBAT----
               IF (ITY == 3.AND.IHBE == 11) THEN
                NPG =4
                FAC = FOURTH
               ENDIF
C               ------DKT18----
               IF (ITY == 7.AND.IHBE == 11) THEN
                NPG =3
                FAC = THIRD
               ENDIF
C              ----------------------- 
               IF (MPT> 0) THEN
                DO I=LFT,LLT               
                 N = I + NFT
                 IF (ITY == 7) THEN
                   NUVAR  = MAX(NUVAR,IPM(8,IXTG(1,NFT+1)))
                 ELSE
                   NUVAR  = MAX(NUVAR,IPM(8,IXC(1,NFT+1)))
                 ENDIF
                 IF (NUVAR>=IUS.AND.NPT>=IPT)THEN
                   EVAR(I) = ZERO
                   ILAYER = PLYELEMS(N)
                   I1  = IUS*NEL
                   IF (ILAYER > 0) THEN
                     IPT = ILAYER
                     DO IR = 1, NPTR                                      
                       DO IS = 1, NPTS                                    
                        UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR
                        EVAR(I) = EVAR(I) + UVAR(I1 + I)*FAC             
                       ENDDO                                              
                     ENDDO                                                
                   ENDIF 
                 ENDIF
                ENDDO
               ENDIF
            ENDIF
c---------
          ELSEIF (IFUNC == 10240 .OR. IFUNC == 10669) THEN  
C           interply damage           
c---------
            IF (IHBE == 11) THEN                                               
              IF (IFUNC == 10240 ) THEN                                         
                DO I=LFT,LLT                                                   
                  N = I + NFT                                                   
                  ILAYER = PLYELEMS(N)
                  NFAIL = 0
                  IF(ILAYER /= 0.AND. ILAYER <= ELBUF_TAB(NG)%NINTLAY) 
     .                NFAIL  = ELBUF_TAB(NG)%INTLAY(ILAYER)%NFAIL
                  IF (ILAYER > 0 .AND. ILAYER <= ELBUF_TAB(NG)%NINTLAY .AND. NFAIL > 0) THEN
                    NUVAR = ELBUF_TAB(NG)%INTLAY(ILAYER)%FAIL(1,1)%FLOC(1)%NVAR
                    IF (NUVAR > 0) THEN
                      EVAR(I) = EP30                                             
                      DO IR=1,NPTR                                               
                        DO IS=1,NPTS
                          FBUF => ELBUF_TAB(NG)%INTLAY(ILAYER)%FAIL(IR,IS)                                           
                          EVAR(I) = MIN(EVAR(I), FBUF%FLOC(1)%VAR(I))
                        ENDDO                                                    
                      ENDDO                                                    
                    ENDIF                                                         
                  ENDIF                                                         
                ENDDO                                                           
              ELSEIF (IFUNC == 10669 ) THEN                                         
                DO I=LFT,LLT                                                   
                  N = I + NFT                                                                     
                  ILAYER = PLYELEMS(N)
                  NFAIL = 0
                  IF(ILAYER /= 0.AND. ILAYER <= ELBUF_TAB(NG)%NINTLAY) 
     .                NFAIL  = ELBUF_TAB(NG)%INTLAY(ILAYER)%NFAIL
                  IF (ILAYER > 0 .AND. ILAYER <= ELBUF_TAB(NG)%NINTLAY .AND. NFAIL > 0) THEN
                    NUVAR = ELBUF_TAB(NG)%INTLAY(ILAYER)%FAIL(1,1)%FLOC(1)%NVAR
                    IF (NUVAR > 0) THEN
                      DO IR=1,NPTR                                               
                        DO IS=1,NPTS
                          FBUF => ELBUF_TAB(NG)%INTLAY(ILAYER)%FAIL(IR,IS)
                          EVAR(I) = MAX(EVAR(I), FBUF%FLOC(1)%VAR(I))
                        ENDDO                                                    
                      ENDDO                                                    
                    ENDIF                                                         
                  ENDIF                                                         
                ENDDO                                                           
              ENDIF                                                           
            ENDIF                                               
c---------
          ELSEIF((IFUNC>=10241.AND.IFUNC<=10243)) THEN 
C           interply stress
c---------
            LL = IFUNC - 10240 
            IF (IHBE == 11) THEN                                                      
                DO I=LFT,LLT                                                          
                 N = I + NFT                                                          
                 ILAYER = PLYELEMS(N)                                                 
                 IF (ILAYER > 0 .and. ILAYER <= ELBUF_TAB(NG)%NINTLAY) THEN                                          
                   DO IR=1,NPTR                                                       
                     DO IS=1,NPTS                                                     
                       ILBUF => ELBUF_TAB(NG)%INTLAY(ILAYER)%ILBUF(IR,IS)             
                       EVAR(I) = EVAR(I) + ILBUF%SIG(NEL*(LL-1) + I) / NPG
                      ENDDO                                                           
                    ENDDO                                                             
                 ENDIF                                                                
               ENDDO                                                                  
             ENDIF   ! interply                                                       
c---------
          ELSEIF((IFUNC>=10244.AND.IFUNC<=10246)) THEN  
C           interply strain
c---------
            LL = IFUNC - 10243
            IF (IHBE == 11) THEN                                            
              DO I=LFT,LLT                                                       
                N = I + NFT                                                      
                ILAYER = PLYELEMS(N)                                             
                IF(ILAYER > 0 .and. ILAYER <= ELBUF_TAB(NG)%NINTLAY) THEN                                              
                  DO IR=1,NPTR                                                       
                    DO IS=1,NPTS                                                     
                      ILBUF => ELBUF_TAB(NG)%INTLAY(ILAYER)%ILBUF(IR,IS)             
                      EVAR(I) = EVAR(I) + ILBUF%EPS((I-1)*3 + LL) / NPG              
                    ENDDO                                                           
                  ENDDO                                                             
                ENDIF                                                            
              ENDDO                                                        
            ENDIF  
c---------
          ELSEIF(IFUNC == 10247) THEN 
C           Internal energy
            IF (IHBE == 11) THEN                                            
              DO I=LFT,LLT                                                       
                N = I + NFT                                                      
                ILAYER = PLYELEMS(N)                                             
                IF(ILAYER > 0 .and. ILAYER <= ELBUF_TAB(NG)%NINTLAY) THEN                                              
                  EVAR(I) = ELBUF_TAB(NG)%INTLAY(ILAYER)%EINT(I)         
                ENDIF                                                            
              ENDDO                                                        
            ENDIF  
c---------
          ELSEIF (IFUNC == 2040) THEN  ! EPSP/UPPER
c---------
              IF (NLAY > 1) THEN                   
                IL  = MAX(1,NPT)            
                IPT = 1                            
              ELSE                                 
                IL  = 1                            
                IPT = MAX(1,NPT)                  
              ENDIF
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              IF (BUFLY%L_PLA > 0) THEN
                DO  I=LFT,LLT
                  EVAR(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))
                ENDDO
              ELSE
                DO  I=LFT,LLT
                  EVAR(I) = ZERO
                ENDDO 
              ENDIF 
c---------
         ELSEIF (IFUNC == 2041) THEN  ! EPSP/LOWER
c---------
           BUFLY => ELBUF_TAB(NG)%BUFLY(1)               
           IF (BUFLY%L_PLA > 0) THEN                     
             DO  I=LFT,LLT                               
               EVAR(I) = ABS(BUFLY%LBUF(1,1,1)%PLA(I))   
             ENDDO                                       
           ENDIF                                         
c---------
         ELSEIF(IFUNC>=2042.AND.IFUNC<=2141) THEN   
c---------
            IF(MLW/=1)THEN
              IPT = MOD ((IFUNC - 2041), 100)     
              IF(IPT == 0)IPT = 100
              IF(NPT>=IPT)THEN 
c                 IADD  = NB11 + LLT*(ILAYER-1)
                 DO  I=LFT,LLT
                  ILAYER = PLYELEMS(N)
                  IF(ILAYER > 0) THEN
c                   IADD  = NB11 + LLT*(ILAYER-1)                   
c                   EVAR(I) = ABS(BUFEL(IADD+I))
                  ENDIF
                 END DO  
              ELSE IF(NPT == 0)THEN
c                IADD=NB11
                DO  I=LFT,LLT
c                   EVAR(I) = ABS(BUFEL(IADD+I))
                END DO    
              END IF
           ENDIF 
c---------
          ELSE IF(IFUNC == 2142)THEN
c---------Thuis subroutine is used only with PID17 for Plyxfem. 
C it should be activated in this cas for this option.
!!           IF ((MLW == 25.OR.MLW == 15).AND.(IGTYP == 10.OR.IGTYP == 11)) THEN 
!!              DO I=LFT,LLT                 
!!                DAM1(I)=ZERO               
!!                DAM2(I)=ZERO               
!!                WPLA(I)=ZERO               
!!                FAIL(I)=ZERO               
!!              END DO                                 
c
!!              IF (IFAILURE == 0) THEN
!!                IF (NLAY > 1) THEN                       
!!                  IL  = MAX(1,NPT)                     
!!                  IPT = 1                                
!!                ELSE                                     
!!                  IL  = 1                                
!!                  IPT = MAX(1,NPT)                       
!!                ENDIF                                  
!!                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)       
! 
!!               DO N=1,NPT          
!!                  LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IPT)   
!!                  IADR = (N-1)*NEL                                
!!                  DO I=LFT,LLT                                      
!!                    J = IADR + I                                    
!!                    K1 = NB15 + 2*IADR+2*I-1                        
!!                    K2 = NB15 + 2*IADR+2*I                          
!!                    DAM1(I) = LBUF%DAM(K1)                                 
!!                    DAM2(I) = LBUF%DAM(K2)                                 
!!                    WPLA(I) = LBUF%PLA(I)                           
!!                    DMAX(I) = PM(64,MATLY(J))                       
!!                    WPMAX(I)= PM(41,MATLY(J))                       
!!                    IF(DAM1(I)>=DMAX(I).OR.DAM2(I)>=DMAX(I).OR. 
!!     .                 WPLA(I)<ZERO.OR.WPLA(I)>=WPMAX(I))      
!!     .              FAIL(I) =  FAIL(I) + ONE                         
!!                  ENDDO                                               
!!                ENDDO                                               
!!              ELSE
!!                IF(IFAILA == 1) THEN
!!                  DO  I=LFT,LLT
c                    OFF = BUFEL(IADD+I)
!!                    IF(OFF<ZERO)THEN
!!                      FAIL(I)= -ONE
!!                    ELSEIF(OFF>ZERO)THEN
!!                      FAIL(I)=  ONE
!!                    ELSE
!!                      FAIL(I)=  ZERO
!!                    END IF
!!                     EVAR(I)=FAIL(I)
!!                  END DO  
!!                ENDIF 
!!              ENDIF 
!!           ELSE 
             IF(IFAILURE == 0 .OR.(IFAILURE /=0 .AND.IFAILA ==1))THEN  
               DO  I=LFT,LLT                                          
                 OFF = GBUF%OFF(I)                                     
                 IF(OFF < ZERO)THEN                                   
                   FAIL(I) = -ONE                                        
                 ELSEIF(OFF > ZERO)THEN                               
                   FAIL(I) =  ONE                                        
                 ELSE                                                  
                   FAIL(I) =  ZERO                                      
                 END IF                                                
                  EVAR(I)=FAIL(I)                                      
               END DO                                                  
             ENDIF                                                    
!!           END IF
        
          ELSE IF(IFUNC == 2156)THEN
c
          ENDIF
C
          IF(MLW == 0 .OR. MLW == 13)THEN
            IF(ITY == 3)THEN
            ELSE
              DO I=LFT,LLT
                ILAYER = PLYELEMS(N)
                IF(ILAYER > 0) THEN
                  IE = IE + 1
                  FUNC(EL2FA(NEL_PLY + IE)) = ZERO
                ENDIF   
              ENDDO
           ENDIF
          ELSEIF(IFUNC == 3)THEN
C-------------------
C energie specifique
C-------------------
           IF(ITY == 3)THEN
            DO I=LFT,LLT
              N = I + NFT 
              ILAYER = PLYELEMS(N)
              IF(ILAYER > 0) THEN
                 IE = IE + 1
                 FUNC(EL2FA(NEL_PLY + IE)) = ZERO
              ENDIF   
            ENDDO
           ELSE
            DO I=LFT,LLT
               N = I + NFT 
              ILAYER = PLYELEMS(N)
              IF(ILAYER > 0) THEN
                IE = IE + 1
                FUNC(EL2FA(NEL_PLY + IE)) = ZERO
               ENDIF 
            ENDDO
           ENDIF
          ELSEIF(IFUNC == 25.AND.ITY == 3)THEN
C-------------------
C energie hourglass
C-------------------
            DO I=LFT,LLT
               N = I + NFT 
              ILAYER = PLYELEMS(N)
              IF(ILAYER > 0) THEN
                IE = IE + 1
                FUNC(EL2FA(NEL_PLY + IE)) = ZERO
              ENDIF  
            ENDDO
          ELSE
C-------------------
C cas general
C-------------------           
           IF(ITY == 3)THEN
               DO I=LFT,LLT
                  N = I + NFT 
                 ILAYER = PLYELEMS(N)
                IF(ILAYER > 0) THEN
                   IE = IE + 1
                   FUNC(EL2FA(NEL_PLY + IE)) = EVAR(I)
                ENDIF   
              ENDDO
           ENDIF
          ENDIF
        ENDIF
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES OFFSET
C-----------------------------------------------
       END DO
 900  CONTINUE
C-----------------------------------------------=
C
       IF(IFLAG > 0) THEN
         IF (NSPMD == 1) THEN
            ILAST = MAX(NEL_PLY,1)
            DO I=1,IE
               N = EL2FA(NEL_PLY + I)
               R4 = FUNC(N)
               CALL WRITE_R_C(R4,1)
            ENDDO
         ELSE
            DO I=1,IE
               N = EL2FA(NEL_PLY + I)
               WAL(I+NEL_PLY) = FUNC(N)
            ENDDO
         ENDIF

      ENDIF
        NEL_PLY = NEL_PLY + PLYSHELL(IPLY)%PLYNUMSHELL
      ENDDO
      IF (NSPMD > 1 ) THEN
        IF (ISPMD == 0) THEN
          BUF = NBF_PXFEMG
        ELSE
          BUF=1
        ENDIF
          CALL SPMD_R4GET_PARTN(1,NBF_L,NBPART,IADG,WAL,BUF)
      ENDIF

 
      RETURN
      END
