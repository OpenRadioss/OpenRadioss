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
Chd|  DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        OUTPUT_SCHLIEREN              source/output/anim/generate/output_schlieren.F
Chd|        SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        SROTORTH                      source/elements/solid/srotorth.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|====================================================================
      SUBROUTINE DFUNCS(ELBUF_TAB  ,FUNC      ,IFUNC     ,IPARG    ,GEO     ,
     2                  IXS        ,MASS      ,PM        ,EL2FA    ,NBF     ,
     3                  IPM        ,IGEO      ,NBPART    ,EHOUR    ,ANIM    ,
     4                  IADG       ,SPBUF     ,IPART     ,IPARTSP  ,ISPH3D  ,
     5                  X          ,V         ,W         ,ALE_CONNECTIVITY,
     6                  NERCVOIS   ,NESDVOIS  ,LERCVOIS  ,LESDVOIS ,BUFMAT  ,
     7                  FANI_CELL  ,MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE SCHLIEREN_MOD  
      USE I22TRI_MOD , only:INT22_FCELL_ANIM  
      USE ALEFVM_MOD  
      USE MULTI_FVM_MOD   
      USE ALE_CONNECTIVITY_MOD
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
#include      "sphcom.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "mmale51_c.inc"
#include      "inter22.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   FUNC(*), MASS(*) ,PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO),
     .   EHOUR(*),ANIM(*), SPBUF(*),X(3,NUMNOD),V(3,NUMNOD), W(3,NUMNOD),BUFMAT(*)
      my_real, INTENT(IN) :: FANI_CELL(SFANI_CELL)
      INTEGER IPARG(NPARG,*),EL2FA(*),IXS(NIXS,NUMELS),IFUNC,NBF,ISPH3D,
     .        NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .        IPART(LIPART1,*),IPARTSP(*),BUF,IGEO(NPROPGI,NUMGEO)
      REAL  WAL(NBF)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,N, NG, NEL, MLW, 
     .        NN, K1, K2,JTURB,MT, IMID, IALEL,IRUPT,           
     .        NN1,NN2,NN3,NN4,
     .        OFFSET,K,II, IUS, NUVAR,TSHELL,TSH_ORT,
     .        ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, NLAY, IPT,
     .        IL,IS,IR,IT, NPTG, ICSIG,
     .        PID, NPG_PLANE,NFAIL,NUMLAY,IJK,IIR,IOFF,IALEFVM_FLG,
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .        IDEB, IPOS, ITRIMAT,IVISC,JJ(6),IFRAC,IMAT,IADBUF,
     .        NUPARAM,IDX,ISUBMAT,IU(4),NFRAC,IS_ALE,IS_EULER,
     .        IMAT_TILLOTSON,NTILLOTSON,FAC,NVAREOS,IEOS
      my_real
     .   EVAR(MVSIZ), USER(MVSIZ),
     .   OFF, P, VONM2, VONM, S1, S2, S3, VALUE,VALUES(MVSIZ),GAMA(6),
     .   T11,T21,T31,T12,T22,T32,T13,T23,T33,
     .   PHI,TETA,PSI,DAMMAX,S11,S22,S33,S4,S5,S6,
     .   SIG1(MVSIZ),SIG2(MVSIZ),SIG3(MVSIZ),SIG4(MVSIZ),SIG5(MVSIZ),
     .   SIG6(MVSIZ),FF0,GG0,HH0,LL0,MM0,NN0,CRIT,VEL(0:4),VFRAC(MVSIZ,21),TMP(3,8)
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF    
      TYPE(BUF_MAT_)  ,POINTER :: MBUF  
      TYPE(BUF_EOS_)  ,POINTER :: EBUF
      
      my_real, 
     .   DIMENSION(:),POINTER  :: 
     .   UVARF, DAMF,DFMAX,TDELE
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      TARGET :: BUFMAT
      my_real :: MY_VALUE,MY_ONE
C=======================================================================
      NN1  = 1
      NN2  = 1
      NN3  = NN2 + NUMELS
      NN4  = NN3 + ISPH3D*(NUMSPH+MAXPJET)
      GAMA = ZERO
      IOFF = 0
C-----------------------------------------------

      !-------------------------------------------------------!
      !     INITIALIZATION IF SCHLIEREN DEFINED               !
      !-------------------------------------------------------!      
      IF(IFUNC==4892)THEN
        CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM)
      ENDIF!(IFUNC==4892)
   

C-----------------------------------------------
      DO 900 NG=1,NGROUP
        CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF (MLW /= 13) THEN
        DO OFFSET = 0,NEL-1,NVSIZ
          NFT = IPARG(3,NG) + OFFSET
          ISOLNOD = IPARG(28,NG)
          IVISC = IPARG(61,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-OFFSET)
          IS_ALE=IPARG(7,NG)
          IS_EULER=IPARG(11,NG)
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO
!
C-----------------------------------------------
          IF (ITY == 1) THEN
c           SOLID ELEMENTS
            IF (JCVT==1.AND.ISORTH/=0) JCVT=2
C-----------------------------------------------
            GBUF => ELBUF_TAB(NG)%GBUF
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)       
            NLAY = ELBUF_TAB(NG)%NLAY                    
            NPTR = ELBUF_TAB(NG)%NPTR                     
            NPTS = ELBUF_TAB(NG)%NPTS                     
            NPTT = ELBUF_TAB(NG)%NPTT                     
            NPTG = NPTT*NPTS*NPTR*NLAY
            TSHELL  = 0                                             
            TSH_ORT = 0  
            IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1 
            IF (IGTYP==21 .OR. IGTYP==22) TSH_ORT = 1               
                     PID=IXS(10,1 + NFT)
c
            DO I=LFT,LLT
              EVAR(I) = ZERO   
              SIG1(I) = ZERO
              SIG2(I) = ZERO
              SIG3(I) = ZERO
              SIG4(I) = ZERO
              SIG5(I) = ZERO
              SIG6(I) = ZERO
            ENDDO              
C-----------
            IF (MLW /= 0 .and. MLW /= 13 .and. IGTYP /= 0) THEN
              JTURB=IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))
C-----------
              IF(IFUNC == 1 .AND. (MLW /= 12 .AND. MLW /=14 .AND. MLW /= 25))THEN
                DO I=LFT,LLT     
                  IF (MLW == 10 .OR. MLW == 21) THEN
                    EVAR(I) = LBUF%EPSQ(I)
                  ELSEIF (GBUF%G_PLA > 0) THEN  
                    EVAR(I) = GBUF%PLA(I)
                  ENDIF
                ENDDO                         
c-----------
              ELSEIF(IFUNC == 2)THEN
                DO I=LFT,LLT                                            
                  EVAR(I) = GBUF%RHO(I)                        
                ENDDO                                                   
c-----------
              ELSEIF(IFUNC == 3)THEN
                DO I=LFT,LLT                                            
                  N = I + NFT                                           
                  IALEL=IPARG(7,NG)+IPARG(11,NG)                        
                  IF (IALEL == 0) THEN                                    
                    MT=IXS(1,N)                                         
                    EVAR(I) = GBUF%EINT(I)/MAX(EM30,PM(1,MT))          
                  ELSE                                                  
                    EVAR(I) = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                  ENDIF 
                  IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .              EVAR(I) = EVAR(I) * GBUF%FILL(I)                                                
                ENDDO                                                   
c-----------
              ELSEIF(IFUNC == 4)THEN
                DO I=LFT,LLT                                            
                  IF (IPARG(1,NG)  ==  2 .AND. JTHE  ==  0) THEN       
                    EVAR(I) = PM(54,IXS(1,NFT+I)) + GBUF%EINT(I)*     
     .                        GBUF%VOL(I) * PM(53,IXS(1,NFT+I))      
                  ELSEIF (GBUF%G_TEMP > 0) THEN
                    IF (MLW == 104) THEN 
                      DO IL=1,NLAY                                                           
                        DO IS=1,NPTS                                                         
                          DO IT=1,NPTT                                                       
                            DO IR=1,NPTR            
                              LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)                                                                    
                              EVAR(I) = EVAR(I) + (ONE/NPTG)*(LBUF%TEMP(I))               
                            ENDDO                                                          
                          ENDDO                                                            
                        ENDDO                                                              
                      ENDDO                                                                      
                    ELSE
                      EVAR(I) = GBUF%TEMP(I)
                    ENDIF
                  ENDIF                                                 
                ENDDO                                                   
c-----------
              ELSEIF(IFUNC == 6 .OR. IFUNC == 7)THEN
                  DO I=LFT,LLT
                      N = I + NFT
                      S11 = GBUF%SIG(JJ(1) + I)
                      S22 = GBUF%SIG(JJ(2) + I)
                      S33 = GBUF%SIG(JJ(3) + I)
                      S4  = GBUF%SIG(JJ(4) + I) 
                      S5  = GBUF%SIG(JJ(5) + I) 
                      S6  = GBUF%SIG(JJ(6) + I) 
                      IF (IVISC > 0) THEN
                        S11 = S11 + LBUF%VISC(JJ(1) + I)
                        S22 = S22 + LBUF%VISC(JJ(2) + I)
                        S33 = S33 + LBUF%VISC(JJ(3) + I)
                        S4  = S4  + LBUF%VISC(JJ(4) + I)
                        S5  = S5  + LBUF%VISC(JJ(5) + I)
                        S6  = S6  + LBUF%VISC(JJ(6) + I)
                      ENDIF
                      P = - (S11  + S22 + S33 ) * THIRD
                      VALUE = P 
                      IF (IFUNC == 7) THEN
                        S1= S11 + P
                        S2= S22 + P
                        S3= S33 + P
                        VONM2 = THREE*(S4*S4 + S5*S5 + S6*S6 +
     .                         HALF*(S1*S1 + S2*S2 + S3*S3))
                        VONM = SQRT(VONM2)
                        VALUE = VONM
                        IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    VALUE = VALUE * GBUF%FILL(I) 
                      ENDIF
                      EVAR(I) = VALUE
                  ENDDO
C
c-----------
              ELSEIF(IFUNC == 8 .and. JTURB /= 0)THEN
C                 ENERGIE TURBULENTE     
                  DO I=LFT,LLT           
                    EVAR(I) = GBUF%RK(I) 
                  ENDDO                  
c-----------
              ELSEIF(IFUNC == 9)THEN
C               VISCOSITE TURBULENTE
                DO I=LFT,LLT                                        
                   N = I + NFT                                      
                   IF((MLW == 6 .OR. MLW == 17).AND.JTURB/=0)THEN 
                     MT=IXS(1,N)                                    
                     EVAR(I) = PM(81,MT) * GBUF%RK(I)**2              
     .                                 / MAX(EM15,GBUF%RE(I))         
                    ELSEIF(MLW == 46 .OR. MLW == 47)THEN            
                      EVAR(I) = MBUF%VAR(I)     
                    ELSE
                                     
                   ENDIF                                            
                ENDDO                                               
c-----------
              ELSEIF(IFUNC == 10)THEN
C               VORTICITE-X
                DO I=LFT,LLT     
                  EVAR(I) = FANI_CELL(I+NFT)                 
                ENDDO
C
c-----------
              ELSEIF((IFUNC == 11.OR.IFUNC == 12.OR.IFUNC == 13)
     .                                      .AND.MLW == 24)THEN
C                dam 1 2 3
                 DO I=LFT,LLT
                   EVAR(I) = LBUF%DAM(JJ(IFUNC-10) + I)
                 ENDDO
C
c-----------
              ELSEIF(IFUNC>=14.AND.IFUNC<=19)THEN
                 DO I=LFT,LLT
                    EVAR(I) = GBUF%SIG(JJ(IFUNC - 13) + I)
                    IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .                    EVAR(I) = EVAR(I) * GBUF%FILL(I) 
                 ENDDO
                 IF(IVISC > 0) THEN
                   DO I=LFT,LLT
                    EVAR(I) = EVAR(I) + LBUF%VISC(JJ(IFUNC - 13)+I)
                   ENDDO
                 ENDIF 
c-----------  
              ELSEIF(IFUNC>=20 .AND. IFUNC<=24) THEN
                IF(MLW >= 28)THEN                                                        
c                 USER VARIABLES per Gauss point from 1 to 5                               
                  IUS = IFUNC - 20                                                         
                  DO I=LFT,LLT                                                             
                    USER(I) = ZERO                                                         
                  ENDDO                                                                    
                  IF (ISOLNOD == 8 .AND. MLW == 59) THEN ! exception for connect rupture   
                   !output = global damage variables of /fail/connect                      
                    MT = IXS(1,NFT+1)                                                      
                    IRUPT = IPM(111,MT)                                                    
                    NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                   
                    IF (IRUPT == 20) THEN                                                  
                      NPTG = 4                                                             
                      DO IR=1,NFAIL                                                        
                        DO IPT = 1,NPTG                                                    
                         UVARF =>                                                          
     .                   ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%VAR                 
                         DO I=LFT,LLT                                                      
                           USER(I) = MAX(USER(I),UVARF(IUS*NEL + I))                       
                         ENDDO                                                             
                        ENDDO                                                              
                      ENDDO                                                                
                    ENDIF                                                                  
                  ELSE                                                                     
                    DO IL=1,NLAY                                                           
                      DO IS=1,NPTS                                                         
                        DO IT=1,NPTT                                                       
                          DO IR=1,NPTR                                                     
                            MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)                  
                            DO I=LFT,LLT                                                   
                               N = I + NFT                                                 
                               MT=IXS(1,N)                                                 
                               NUVAR = IPM(8,MT)                                           
                               IF (NUVAR > IUS) USER(I) = USER(I)                          
     .                                        + MBUF%VAR(I+IUS*NEL)/NPTG                   
                            ENDDO                                                          
                          ENDDO                                                            
                        ENDDO                                                              
                      ENDDO                                                                
                    ENDDO                                                                  
                  ENDIF                                                                    
                  DO I=LFT,LLT                                                             
                    N = I + NFT                                                            
                    MT=IXS(1,N)                                                            
                    NUVAR = IPM(8,MT)                                                      
                    IF (ISOLNOD == 8 .AND. MLW == 59) THEN                                 
                      EVAR(I) = USER(I)                                                    
                    ELSEIF (NUVAR > IUS) THEN                                              
                      EVAR(I) = USER(I)                                                    
                    ELSE                                                                   
                      EVAR(I) = ZERO                                                       
                    ENDIF                                                                  
                  ENDDO                                                                    
                ENDIF!IF(MLW >= 28)THEN                                                                   
c-----------
              ELSEIF(IFUNC == 25)THEN
                 DO I=LFT,LLT
                   N = I + NFT
                   EVAR(I) = EHOUR(N)
                 ENDDO
c-----------
              ELSEIF(IFUNC == 26) THEN
                 IF (GBUF%G_EPSD > 0) THEN
                   DO I=LFT,LLT
                     EVAR(I) = GBUF%EPSD(I)
                   ENDDO
                 ELSE
                   DO I=LFT,LLT
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF
c-----------
              ELSEIF(IFUNC == 28 .AND. INT22>0) THEN
                   DO I=LFT,LLT
                     EVAR(I) = INT22_FCELL_ANIM(I+NFT)
                   ENDDO
c-----------
              ELSEIF(IFUNC>=27.AND.IFUNC<=81.AND.MLW>=28.AND.MLW/=51) THEN  !anim user 1-60 no longer used with law51 => automatic phase outputs using usual keywords
C               USER VARIABLES from 6 to 60
C               IUS = (n-1)'th user variable in UVAR
                IUS = IFUNC - 22                                            
                DO I=LFT,LLT                                                
                  USER(I) = ZERO                                            
                ENDDO                                                       
                IF (ISOLNOD == 8 .AND. MLW == 59) THEN                      
c                 output = global damage variables of /fail/connect         
                  MT = IXS(1,NFT+1)                                         
                  IRUPT = IPM(111,MT)                                       
                  NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL                      
                  IF (IRUPT == 20) THEN                                     
                    NPTG = 4                                                
                    DO IR=1,NFAIL                                           
                      DO IPT = 1,NPTG                                       
                       UVARF =>                                             
     .                 ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%VAR    
                       DO I=LFT,LLT                                         
                         USER(I) = MAX(USER(I),UVARF(IUS*NEL + I))          
                       ENDDO                                                
                      ENDDO                                                 
                    ENDDO                                                   
                  ENDIF                                                     
                ELSE                                                        
                  DO IL=1,NLAY                                              
                    DO IS=1,NPTS                                            
                      DO IT=1,NPTT                                          
                        DO IR=1,NPTR                                        
                          MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)     
                          DO I=LFT,LLT                                      
                             N = I + NFT                                    
                             MT=IXS(1,N)                                    
                             NUVAR = IPM(8,MT)                              
                             IF (NUVAR > IUS) USER(I) = USER(I)             
     .                                      + MBUF%VAR(I+IUS*NEL)/NPTG      
                          ENDDO                                             
                        ENDDO                                               
                      ENDDO                                                 
                    ENDDO                                                   
                  ENDDO                                                     
                ENDIF                                                       
                DO I=LFT,LLT                                                
                  N = I + NFT                                               
                  MT=IXS(1,N)                                               
                  NUVAR = IPM(8,MT)                                         
                  IF (ISOLNOD == 8 .AND. MLW == 59) THEN                    
                    EVAR(I) = USER(I)                                       
                  ELSEIF (NUVAR > IUS) THEN                                 
                    EVAR(I)= USER(I)                                        
                  ELSE                                                      
                    EVAR(I) = ZERO                                          
                  ENDIF                                                     
                ENDDO                                                                                                                                                                                                                              
c-------------
              ELSEIF(IFUNC>=283.AND.IFUNC<=286) THEN
C              USER VARIABLES from 6 to 60
C              IUS = (n-1)'th user variable in UVAR

               USER(LFT:LLT) = ZERO   

               IF(MLW==37)THEN
                 IUS=3+(IFUNC-283) !law37 user4 and user5   for vfrac              
               ELSEIF(MLW==51)THEN
                 IMAT = IXS(1,NFT+1)                        
                 IADBUF = IPM(7,IMAT)                       
                 NUPARAM= IPM(9,IMAT)                       
                 UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM) 
                 ISUBMAT = (IFUNC-282)
                 ISUBMAT = UPARAM(276+ISUBMAT) !bijective order
                 IUS=N0PHAS+(ISUBMAT-1)*NVPHAS 
!               ELSEIF(MLW==20)THEN
!                 IUS=1+(IFUNC-10248)
               ENDIF
               IFRAC=IFUNC-283+1

c--------------                
               IF (MLW==51 .OR. (MLW==37.AND.IFRAC<=2))THEN                                                                       
                 DO IL=1,NLAY                                              
                   DO IS=1,NPTS                                            
                     DO IT=1,NPTT                                          
                       DO IR=1,NPTR                                        
                         MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)     
                         DO I=LFT,LLT                                                                 
                           USER(I) = USER(I) + MBUF%VAR(I+IUS*NEL)/NPTG 
                         ENDDO                                             
                       ENDDO                                               
                     ENDDO                                                 
                   ENDDO                                                   
                 ENDDO  
               ELSEIF (MLW == 151) THEN 
                  IF(IFRAC<=NLAY)THEN
                    LBUF => ELBUF_TAB(NG)%BUFLY(IFUNC-282)%LBUF(1,1,1)
                    DO I=1,NEL
                       USER(I) = LBUF%VOL(I) / GBUF%VOL(I)         
                    ENDDO
                   ELSE
                     USER(LFT:LLT) = ZERO
                   ENDIF 
!               ELSEIF(MLW==20)THEN    
!                 USER(I) = ELBUF_TAB(NG)%BUFLY(IUS)%LBUF(1,1,1)%FRAC(I)                                                                                   
                ELSE
                  USER(LFT:LLT) = ZERO  ! in case of law37 and law51 in same input file. Then submat 3,4 does not exist for law37 and are set to 0.
               ENDIF  
c-------------    
               EVAR(LFT:LLT) = USER(LFT:LLT)  
c-------------   
c-----------
              ELSEIF(IFUNC>=82.AND.IFUNC<=281.AND.MLW == 25) THEN
C               WPLA par couche pour loi 25
                DO I=LFT,LLT
                  EVAR(I) = ZERO 
                ENDDO               
C                
                IUS = IFUNC - 81
                IF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .             (ISOLNOD == 8.AND.JHBE == 14).OR.
     .             ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15))THEN
            IF (IUS <= NPTG) THEN      
                    DO IL=1,NLAY                                          
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                        DO IS=1,NPTS                                        
                         DO IT=1,NPTT                                      
                          DO IR=1,NPTR                                    
                            LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            DO I=LFT,LLT                                 
                              EVAR(I) = EVAR(I) + LBUF%PLA(I)              
                            ENDDO
                           ENDDO                                           
                          ENDDO                                             
                        ENDDO                                               
                      ENDIF                                        
                    ENDDO                                                 
            ENDIF                                              
                ENDIF
c-----------
              ELSEIF (IFUNC == 282 .AND. MLW == 25) THEN
C---            FAILED LAYERS par elem pour loi 25
                DO I=LFT,LLT
                  EVAR(I) = ZERO
                ENDDO
                IF( ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .             (ISOLNOD == 8.AND.JHBE == 14).OR.
     .            ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15))THEN
c
                  NPG_PLANE = NPTR * NPTS * NPTT
                  DO I=LFT,LLT
                    DO IL=1,NLAY
                      VALUE   = ZERO
                      DO J=1,NPTR
                        DO K=1,NPTS
                          DO L=1,NPTT
                            LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(J,K,L)
                            IF (LBUF%OFF(I) == 0) VALUE = VALUE + ONE
                            IF(INT(VALUE)>=NPG_PLANE) EVAR(I)=EVAR(I)+ONE
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDIF
c-----------
              ELSEIF (IFUNC >= 287 .AND. IFUNC < 887)THEN
C           EULER ANGLE : rotation of ORTHOTROPIC SYSTEM wrt GLOBAL SYSTEM
                NUMLAY = ((IFUNC - 287)/3)+1
                IF(NUMLAY <= NLAY)THEN
                  LBUF => ELBUF_TAB(NG)%BUFLY(NUMLAY)%LBUF(1,1,1)
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(ISORTH ==1) THEN
C                    pour JHBE=14, valeurs moyennes est dans rep. corota.
                      IF(IGTYP == 22) THEN    
                        GAMA(1)= LBUF%GAMA(JJ(1)+I)
                        GAMA(2)= LBUF%GAMA(JJ(2)+I)
                        GAMA(3)= ZERO
                        GAMA(4)= ZERO
                        GAMA(5)= ZERO
                        GAMA(6)= ZERO
                      ELSEIF(IGTYP == 21) THEN    
                        GAMA(1)= GBUF%GAMA(JJ(1)+I)
                        GAMA(2)= GBUF%GAMA(JJ(2)+I)
                        GAMA(3)= ZERO
                        GAMA(4)= ZERO
                        GAMA(5)= ZERO
                        GAMA(6)= ZERO
                      ELSE
                        GAMA(1) = GBUF%GAMA(JJ(1)+I)
                        GAMA(2) = GBUF%GAMA(JJ(2)+I)
                        GAMA(3) = GBUF%GAMA(JJ(3)+I)
                        GAMA(4) = GBUF%GAMA(JJ(4)+I)
                        GAMA(5) = GBUF%GAMA(JJ(5)+I)
                        GAMA(6) = GBUF%GAMA(JJ(6)+I)
                      ENDIF
                      CALL SROTORTH(X,IXS(1,N),
     .                            GAMA,JHBE,IGTYP,IPARG(17,NG) )
C--------
                      T11=GAMA(1)
                      T21=GAMA(2)
                      T31=GAMA(3)
                      T12=GAMA(4)
                      T22=GAMA(5)
                      T32=GAMA(6)
                      T13=T21*T32-T31*T22
                      T23=T31*T12-T11*T32
                      T33=T11*T22-T21*T12
                      IF (ABS(T31) - ONE  < EM20)THEN
c                      IF( ABS(COS(-ASIN(T31))) > EM20)THEN
                        TETA = -ASIN(T31)
                        MY_ONE = ONE
                        MY_VALUE = MAX(abs(COS(TETA)),EM20) * SIGN(MY_ONE,COS(TETA))
                        IF(T32==ZERO.AND.T33==ZERO) THEN
                            PSI  = 0
                        ELSE
                            PSI  = ATAN2( T32/MY_VALUE,T33/MY_VALUE )
                        ENDIF
                        IF(T21==ZERO.AND.T11==ZERO) THEN
                            PHI  = 0
                        ELSE
                            PHI  = ATAN2(T21/MY_VALUE,T11/MY_VALUE)
                        ENDIF
                      ELSE
                        PHI = ZERO
                        IF(T31 == -ONE)THEN
                          TETA = PI / TWO
                          PSI = ATAN2(T12,T13)
                        ELSE
                          TETA = - PI / TWO
                          PSI = ATAN2(-T12,-T13)
                        ENDIF
                      ENDIF
                      IF (MOD(IFUNC - 287,3) == 0) 
     .                  EVAR(I) = PSI*HUNDRED80/PI
                      IF (MOD(IFUNC - 287,3) == 1) 
     .                  EVAR(I) = TETA*HUNDRED80/PI
                      IF (MOD(IFUNC - 287,3) == 2) 
     .                  EVAR(I) = PHI*HUNDRED80/PI
                    ELSE
                      EVAR(I) = ZERO 
                    ENDIF
                  ENDDO
                ELSE
                  DO I=LFT,LLT
                    EVAR(I) = ZERO
                  ENDDO
                ENDIF
c-----------
              ELSEIF (IFUNC == 887 )THEN
              !BURN FRACTION JWL EOS 
                IF(GBUF%G_BFRAC > 0) THEN
                  IF (MLW==151)THEN
                    DO I=LFT,LLT
                      EVAR(I)=-EP30
                      DO IFRAC=1,NLAY
                        EVAR(I) =  MAX(EVAR(I),MULTI_FVM%BFRAC(IFRAC,I+NFT))
                      ENDDO
                    ENDDO
                   ELSE
                      EVAR(LFT:LLT) = GBUF%BFRAC(LFT:LLT)                   
                   ENDIF
                 ELSEIF (MLW == 41) THEN ! LEE TARVER
                    DO I = LFT, LLT
                       EVAR(I) = MBUF%VAR(7 * NEL + I)
                    ENDDO
                 ELSE
                  EVAR(LFT:LLT) = ZERO 
                ENDIF       
c--------------output vdam1, vdam2,vdam3, for failure prop connect    
              ELSEIF(IFUNC>= 888 .AND.IFUNC<= 3888 .AND. MLW>=28) THEN
C               FAILURE VARIABLES 
                DO I=LFT,LLT                                          
                  EVAR(I) = ZERO                                      
                ENDDO                                                 
c--------------    
                IF (ISOLNOD == 8 .AND. MLW == 83) THEN 
c                 output = damage variables of /fail/snconnect  
                  MT = IXS(1,NFT+1)
                  IRUPT = IPM(111,MT)
                  NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL! ng= ngroup
                  IF (IRUPT == 26) THEN 
                   IF(IFUNC <= 890 ) THEN
                    IUS = IFUNC - 888
C                   IUS = (n-1)'th user variable IN DAM 
                    NPTG = 4
                    DO IR=1,NFAIL
                      DO IPT = 1,NPTG
                       DAMF => 
     .                ELBUF_TAB(NG)%BUFLY(1)%FAIL(IPT,1,1)%FLOC(IR)%DAM
                       DO I=LFT,LLT                                 
                         EVAR(I) = MAX(EVAR(I) ,DAMF(IUS*NEL + I))      
                       ENDDO                                           
                      ENDDO
                    ENDDO
                   ELSEIF(IFUNC <= 1890 )THEN !vdam1_ijk
                    IJK = IFUNC - 890
                    IIR = IJK/100
                    IS = (MOD(IJK,100)-MOD(IJK,10))/10
                    IT = MOD(IJK,10)
                    DO IR=1,NFAIL
                    DAMF => 
     .               ELBUF_TAB(NG)%BUFLY(1)%FAIL(IIR,1,1)%FLOC(IR)%DAM
                     DO I=LFT,LLT                                 
                      EVAR(I) = DAMF(I)   
                     ENDDO                                           
                    ENDDO
                   ELSEIF(IFUNC <= 2890 )THEN !vdam2_ijk
                    IJK = IFUNC - 1890
                    IIR = IJK/100
                    IS = (MOD(IJK,100)-MOD(IJK,10))/10
                    IT = MOD(IJK,10)
                    DO IR=1,NFAIL
                     DAMF => 
     .               ELBUF_TAB(NG)%BUFLY(1)%FAIL(IIR,1,1)%FLOC(IR)%DAM
                     DO I=LFT,LLT                                 
                      EVAR(I) = DAMF(NEL+I)            
                     ENDDO                                           
                    ENDDO
                   ELSE !vdam3_ijk
                    IJK = IFUNC - 2890
                    IIR = IJK/100
                    IS = (MOD(IJK,100)-MOD(IJK,10))/10
                    IT = MOD(IJK,10)
                    DO IR=1,NFAIL
                     DAMF => 
     .               ELBUF_TAB(NG)%BUFLY(1)%FAIL(IIR,1,1)%FLOC(IR)%DAM
                     DO I=LFT,LLT                                 
                      EVAR(I) = DAMF(2*NEL+I)                
                     ENDDO                                           
                    ENDDO
                   ENDIF !IFUNC <= 890                       
                  ENDIF !IRUPT
                ENDIF ! ISOLNOD == 8 .AND. MLW == 83
c-----------
              ELSEIF (IFUNC >= 3891.AND.IFUNC <= 4889 )THEN
                DO I=LFT,LLT                                          
                  EVAR(I) = ZERO                                      
                ENDDO 
                IJK = IFUNC - 3890
          IF (TSHELL>0) THEN
                  IIR = IJK/100
                  IL = (MOD(IJK,100)-MOD(IJK,10))/10
                  IS = MOD(IJK,10)
            IT =1
          ELSE
                  IIR = IJK/100
                  IS = (MOD(IJK,100)-MOD(IJK,10))/10
                  IT = MOD(IJK,10)
            IL =1
          END IF
                IUS = NLAY*IIR*IS*IT
                DAMMAX = ZERO
                   IF (IIR <= NPTR.AND.IS <= NPTS.AND.IT <= NPTT) THEN                            
                     NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                          
                     DO IR=1,NFAIL                                                                                            
                        DFMAX=>
     .                    ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%DAMMX
                          DO I=LFT,LLT                                 
                              EVAR(I) = MAX(EVAR(I),DFMAX(I))                
                          ENDDO
                     ENDDO                                           
                 ENDIF                                        
              ELSEIF (IFUNC >= 5911.AND.IFUNC <= 9920 .AND. TSHELL>0)THEN
                DO I=LFT,LLT                                          
                  EVAR(I) = ZERO                                      
                ENDDO 
                IJK = IFUNC - 3890
                IF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
            IL = MOD(ABS(IJK)/10,201)
                  IIR = 1
                  IS = 1
                  IT = 1
                ELSEIF (ISOLNOD == 16.OR.(ISOLNOD == 8 .AND.JHBE == 14)) THEN
c----------- 
                  ICSIG = IPARG(17,NG) 
                  IIR=ABS(IJK)/2010
                  IL=MOD(ABS(IJK)/10,201)
                  IS=MOD(ABS(IJK),10)
                  IT = 1
          END IF
                DAMMAX = ZERO
                   IF (IIR <= NPTR.AND.IS <= NPTS.AND.IT <= NPTT) THEN                            
                     NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                          
                     DO IR=1,NFAIL                                                                                            
                        DFMAX=>
     .                    ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%DAMMX
                          DO I=LFT,LLT                                 
                              EVAR(I) = MAX(EVAR(I),DFMAX(I))                
                          ENDDO
                     ENDDO                                           
                 ENDIF                                        
              ELSEIF(IFUNC == 3890) THEN
                DO I=LFT,LLT                                          
                  EVAR(I) = ZERO                                      
                ENDDO 

                DO IL=1,NLAY 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                        
                    DO IT=1,NPTT                                      
                      DO IIR=1,NPTR   
                        DO IR=1,NFAIL                                                                                            
                          DFMAX=>
     .                      ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%DAMMX
                          DO I=LFT,LLT                                 
                              EVAR(I) = MAX(EVAR(I),DFMAX(I))                
                          ENDDO
                        ENDDO 
                      ENDDO
                    ENDDO
                   ENDDO
                 ENDDO   
              ELSEIF(IFUNC == 4890) THEN
                DO I=LFT,LLT                                          
                  EVAR(I) = ZERO                                      
                ENDDO 
                DO IL=1,NLAY 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                  DO IS=1,NPTS                                        
                    DO IT=1,NPTT                                      
                      DO IIR=1,NPTR   
                        DO IR=1,NFAIL                                                                                            
                          TDELE=>
     .                      ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%TDEL
                          DO I=LFT,LLT                                 
                              EVAR(I) = MAX(EVAR(I),TDELE(I))  
                          ENDDO
                        ENDDO 
                      ENDDO
                    ENDDO
                   ENDDO
                 ENDDO             
c-----------  
              ELSEIF(IFUNC == 4891) THEN  
               IF (MLW == 151) THEN
                  DO I = 1, NEL                                         
                     EVAR(I) = MULTI_FVM%SOUND_SPEED(I + NFT)
                  ENDDO
               ELSE
                  L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
                  IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN              
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                     DO  I=1,NEL                                         
                        EVAR(I) = LBUF%SSP(I) 
                     ENDDO                                                 
                  ENDIF       
               ENDIF
              ELSEIF(IFUNC == 4892) THEN  
                 IALEL=IPARG(7,NG)+IPARG(11,NG)
                 IF(IALEL == 0)THEN
                   EVAR(LFT:LLT) = ZERO
                 ELSE
                   CALL OUTPUT_SCHLIEREN(
     1                            EVAR    ,IXS    ,X           ,
     2                            IPARG   ,WA_L   ,ELBUF_TAB   ,ALE_CONNECTIVITY   ,GBUF%VOL,
     3                            NG      ,NIXS   ,ITY)
                 ENDIF          
c-----------  
              ELSEIF(IFUNC == 4893) THEN  
                  DO I=LFT,LLT
                    EVAR(I) = ISPMD
                  ENDDO
c-----------  
              ELSEIF(IFUNC == 4894) THEN  
                  DO I=LFT,LLT
                    EVAR(I) = GBUF%FILL(I)
                  ENDDO
c-----------
              ELSEIF (IFUNC == 4895) THEN  !  /ANIM/ELEM/SIGEQ
            !  equivalent stress - other then VON MISES
                IF (GBUF%G_SEQ > 0) THEN  !  non VON MISES
!                  DO I=LFT,LLT
!                    EVAR_TMP = ZERO
!                    NPTG = NLAY*NPTR*NPTS*NPTT
!                    DO IL=1,NLAY
!                      DO IT=1,NPTT
!                        DO IR=1,NPTR
!                          DO IS=1,NPTS
!                            LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
!                            IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0) THEN
!                              EVAR_TMP = EVAR_TMP + LBUF%SEQ(I)/NPTG
!                            ELSE
!                              S11 = LBUF%SIG(JJ(1) + I)
!                              S22 = LBUF%SIG(JJ(2) + I)
!                              S33 = LBUF%SIG(JJ(3) + I)
!                              S4  = LBUF%SIG(JJ(4) + I) 
!                              S5  = LBUF%SIG(JJ(5) + I) 
!                              S6  = LBUF%SIG(JJ(6) + I) 
!                              IF (IVISC > 0) THEN
!                                S11 = S11 + LBUF%VISC(JJ(1) + I)
!                                S22 = S22 + LBUF%VISC(JJ(2) + I)
!                                S33 = S33 + LBUF%VISC(JJ(3) + I)
!                                S4  = S4  + LBUF%VISC(JJ(4) + I)
!                                S5  = S5  + LBUF%VISC(JJ(5) + I)
!                                S6  = S6  + LBUF%VISC(JJ(6) + I)
!                              ENDIF                                 
!                              P = - (S11 + S22 + S33) * THIRD
!                              S1 = S11 + P
!                              S2 = S22 + P
!                              S3 = S33 + P
!                              VONM2 =  THREE*(S4*S4 + S5*S5 + S6*S6 +
!     .                                HALF*(S1*S1 + S2*S2 + S3*S3))
!                              VONM = SQRT(VONM2)
!                              EVAR_TMP = EVAR_TMP + VONM/NPTG
!                            ENDIF ! IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0)
!                          ENDDO ! DO IS=1,NPTS
!                        ENDDO ! DO IR=1,NPTR
!                      ENDDO ! DO IT=1,NPTT
!                    ENDDO ! DO IL=1,NLAY
!                    EVAR(I) = EVAR_TMP
!                  ENDDO ! DO I=LFT,LLT
!!!!!!
                  IMAT = IXS(1,NFT+1)
                  IADBUF = IPM(7,IMAT)
                  NUPARAM= IPM(9,IMAT)       
                  UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
!---
                  IF (MLW == 72) THEN
!
                    DO I=LFT,LLT
                      EVAR(I) = GBUF%SEQ(I)
                    ENDDO ! DO I=LFT,LLT
!
                  ELSEIF (MLW == 74) THEN
!                 (ILAW = 74) -- Thermal Hill Orthotropic 3D Material
                    FF0 = UPARAM(7)
                    GG0 = UPARAM(8)
                    HH0 = UPARAM(9)
                    LL0 = UPARAM(10)
                    MM0 = UPARAM(11)
                    NN0 = UPARAM(12)
                    DO I=LFT,LLT
                      S11 = GBUF%SIG(JJ(1) + I)
                      S22 = GBUF%SIG(JJ(2) + I)
                      S33 = GBUF%SIG(JJ(3) + I)
                      S4  = GBUF%SIG(JJ(4) + I) 
                      S5  = GBUF%SIG(JJ(5) + I) 
                      S6  = GBUF%SIG(JJ(6) + I)
                      IF (IVISC > 0) THEN
                        S11 = S11 + LBUF%VISC(JJ(1) + I)
                        S22 = S22 + LBUF%VISC(JJ(2) + I)
                        S33 = S33 + LBUF%VISC(JJ(3) + I)
                        S4  = S4  + LBUF%VISC(JJ(4) + I)
                        S5  = S5  + LBUF%VISC(JJ(5) + I)
                        S6  = S6  + LBUF%VISC(JJ(6) + I)
                      ENDIF
                      P = - (S11 + S22 + S33) * THIRD
                      S1 = S11 + P
                      S2 = S22 + P
                      S3 = S33 + P
!
                      CRIT = FF0*(S2 - S3)**2
     .                     + GG0*(S3 - S1)**2
     .                     + HH0*(S1 - S2)**2
     .                     + TWO*LL0*S5**2
     .                     + TWO*MM0*S6**2
     .                     + TWO*NN0*S4**2
!
                      EVAR(I) = SQRT(CRIT)
                    ENDDO ! DO I=LFT,LLT
                  ELSEIF (MLW == 93) THEN
!
                    DO I=LFT,LLT
                      EVAR(I) = GBUF%SEQ(I)
                    ENDDO ! DO I=LFT,LLT
                  ELSEIF (MLW == 104) THEN
                    DO I = LFT, LLT
                      EVAR(I) = ZERO
                    ENDDO
                    DO IL=1,NLAY                                              
                      DO IS=1,NPTS                                            
                        DO IT=1,NPTT                                          
                          DO IR=1,NPTR    
                            LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            DO I=LFT,LLT
                              EVAR(I) = EVAR(I) + LBUF%SEQ(I)/NPTG  
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ELSEIF (MLW == 115) THEN
!
                    DO I=LFT,LLT
                      EVAR(I) = GBUF%SEQ(I)
                    ENDDO ! DO I=LFT,LLT
                  ENDIF
!---
                ELSE                      ! VON MISES
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  DO I=LFT,LLT
                    S11 = GBUF%SIG(JJ(1) + I)
                    S22 = GBUF%SIG(JJ(2) + I)
                    S33 = GBUF%SIG(JJ(3) + I)
                    S4  = GBUF%SIG(JJ(4) + I) 
                    S5  = GBUF%SIG(JJ(5) + I) 
                    S6  = GBUF%SIG(JJ(6) + I) 
                    IF (IVISC > 0) THEN
                      S11 = S11 + LBUF%VISC(JJ(1) + I)
                      S22 = S22 + LBUF%VISC(JJ(2) + I)
                      S33 = S33 + LBUF%VISC(JJ(3) + I)
                      S4  = S4  + LBUF%VISC(JJ(4) + I)
                      S5  = S5  + LBUF%VISC(JJ(5) + I)
                      S6  = S6  + LBUF%VISC(JJ(6) + I)
                    ENDIF                                 
                    P = - (S11 + S22 + S33) * THIRD
                    S1 = S11 + P
                    S2 = S22 + P
                    S3 = S33 + P
                    VONM2 = THREE*(S4*S4 + S5*S5 + S6*S6 +
     .                     HALF*(S1*S1 + S2*S2 + S3*S3))
                    VONM = SQRT(VONM2)
                    EVAR(I) = VONM
                  ENDDO ! DO I=LFT,LLT
                ENDIF ! IF (GBUF%G_SEQ > 0)
c-----------                 
              ELSEIF (IFUNC == 4896) THEN  !  /ANIM/ELEM/QVIS
                 IF (GBUF%G_QVIS > 0) THEN
                   DO I=LFT,LLT
                     EVAR(I) = GBUF%QVIS(I)
                   ENDDO
                 ELSE
                   DO I=LFT,LLT
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF
              ELSEIF (IFUNC >= 4931 .AND. IFUNC <= 4934) THEN  !  /ANIM/ELEM/QVIS - law51 phases
                 IF (MLW == 51) THEN
                   ITRIMAT   = IFUNC - 4930                                                
                   !bijection for iform=12
                   IMAT = IXS(1,NFT+1)                        
                   IADBUF = IPM(7,IMAT)                       
                   NUPARAM= IPM(9,IMAT)                       
                   UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM) 
                   ISUBMAT = ITRIMAT
                   ISUBMAT = UPARAM(276+ISUBMAT) !bijective order
                   IUS=N0PHAS+(ISUBMAT-1)*NVPHAS 
                   !
                   LLT       = IPARG(2,NG)                                                 
                   IPOS      = 10                                                          
                   K         = LLT * ((IUS )+IPOS-1)                               
                   DO I=LFT,LLT                                                            
                     EVAR(I) = MBUF%VAR(K+I)                                               
                   ENDDO 
                 ELSE
                   DO I=LFT,LLT
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF                 
c-----------  
              ELSEIF (IFUNC == 4921) THEN  !  /ANIM/ELEM/VOLU
                 IF (GBUF%G_VOL > 0) THEN
                   IALEL=IPARG(7,NG)+IPARG(11,NG)
                   IF(IALEL==0)THEN
                     DO I=LFT,LLT                                           
                       MT      = IXS(1,NFT+1)                               
                       EVAR(I) = PM(1,MT)*GBUF%VOL(I)                       
                       IF(GBUF%RHO(I)>ZERO)EVAR(I)=EVAR(I)/GBUF%RHO(I)   
                     ENDDO                                                  
                   ELSE
                     DO I=LFT,LLT
                      EVAR(I) = GBUF%VOL(I)
                     ENDDO
                   ENDIF
                 ELSE
                   DO I=LFT,LLT
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF                 
c-----------  
              ELSEIF(IFUNC>=4897 .AND. IFUNC<=4929 .AND. IFUNC/=4921)THEN
                IF(MLW  == 51)THEN
                !law51 phases
                   IPOS = 0
                   IF(    IFUNC>=4897 .AND. IFUNC<=4900)THEN
                     IDEB = 4896
                     IPOS = 12   !dens
                   ELSEIF(IFUNC>=4901 .AND. IFUNC<=4904)THEN
                     IDEB = 4900
                     IPOS = 08   !ener (eint/V)
                   ELSEIF(IFUNC>=4905 .AND. IFUNC<=4908)THEN
                     IDEB = 4904
                     IPOS = 16   !temp
                   ELSEIF(IFUNC>=4909 .AND. IFUNC<=4912)THEN
                     IDEB = 4908
                     IPOS = 18   !pres
                   ELSEIF(IFUNC>=4913 .AND. IFUNC<=4916)THEN
                     IDEB = 4912
                     IPOS = 15   !epsp
                   ELSEIF(IFUNC>=4917 .AND. IFUNC<=4920)THEN                                                     
                     IDEB = 4916
                     IPOS = 14   !ssp
                   ELSEIF(IFUNC>=4922 .AND. IFUNC<=4925)THEN                                                     
                     IDEB = 4921
                     IPOS = 11   !volume 
                   ELSEIF(IFUNC>=4926 .AND. IFUNC<=4929)THEN                                                     
                     IDEB = 4925
                     IPOS = 0   !mass                                        
                   ENDIF
                   IMAT      = IXS(1,NFT+1)                        
                   IADBUF    = IPM(7,IMAT)                       
                   NUPARAM   = IPM(9,IMAT)                       
                   UPARAM    => BUFMAT(IADBUF:IADBUF+NUPARAM) 
                     ITRIMAT   = IFUNC - IDEB
                     !bijection iform=12
                     ISUBMAT   = ITRIMAT
                     ISUBMAT   = UPARAM(276+ISUBMAT) !bijective order
                     IUS       = N0PHAS+(ISUBMAT-1)*NVPHAS 
                     !
                     LLT       = IPARG(2,NG)    
                   !all output expect mass en specific energy by mass : output value * vol
                   IF(IPOS /=0 .AND. IPOS /= 08 )THEN
                     K         = LLT * ((IUS )+IPOS-1)                                                                            
                     DO I=LFT,LLT
                       EVAR(I) = MBUF%VAR(K+I)
                     ENDDO 
                   !specific energy by mass
                   ELSEIF(IPOS == 08)THEN
                     K1        = LLT * ((IUS )+08-1)
                     K2        = LLT * ((IUS )+12-1) 
                     EVAR(LFT:LLT) = ZERO                                                             
                     DO I=LFT,LLT
                       IF(MBUF%VAR(K2+I) /= ZERO) EVAR(I) = MBUF%VAR(K1+I) / MBUF%VAR(K2+I) ! (eint/v) / rho
                     ENDDO
                   ELSEIF(IPOS==0)THEN
                   ! mass
                     ITRIMAT   = IFUNC - IDEB
                     LLT       = IPARG(2,NG)      
                     K1        = LLT * ((IUS )+12-1)
                     K2        = LLT * ((IUS )+11-1)                     
                     DO I=LFT,LLT
                       EVAR(I) = MBUF%VAR(K1+I) * MBUF%VAR(K2+I) !dens*vol
                     ENDDO
                   ELSE 
                     !should not happen
                     EVAR(LFT:LLT) = ZERO
                   ENDIF
                ELSE
                  DO I=LFT,LLT                                          
                    EVAR(I)  = ZERO                                      
                  ENDDO                    
                ENDIF!IF(MLW  == 51)                         
c-----------
              ELSEIF (IFUNC == 4930) THEN  !  /ANIM/ELEM/TDET
                 IF (MLW  /= 51 .AND. GBUF%G_TB > 0) THEN
                   DO I=LFT,LLT
                     EVAR(I) = -GBUF%TB(I)
                   ENDDO
                 ELSEIF (MLW == 51)THEN
                   IPOS      = 15
                   ITRIMAT   = 4
                   LLT       = IPARG(2,NG)      
                   K         = LLT * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                             
                   DO I=LFT,LLT
                     EVAR(I) = -MBUF%VAR(K+I)
                   ENDDO 
                 ELSE
                   DO I=LFT,LLT
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF   
c-----------
              ELSEIF (IFUNC == 4935 .OR. IFUNC == 4936) THEN  !  /ANIM/ELEM/DENS for law37 submaterials
                 IF (MLW  /= 37) THEN
                   EVAR(LFT:LLT) = ZERO
                 ELSE
                   USER(LFT:LLT) = ZERO
                   IUS=3-IFUNC+4935 !law37 user4 and user5                                                                                    
                   DO IL=1,NLAY                                              
                     DO IS=1,NPTS                                            
                       DO IT=1,NPTT                                          
                         DO IR=1,NPTR                                        
                           MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)     
                           DO I=LFT,LLT                                                                 
                             USER(I) = USER(I) + MBUF%VAR(I+(IUS-1)*NEL)/NPTG 
                           ENDDO                                             
                         ENDDO                                               
                       ENDDO                                                 
                     ENDDO                                                   
                   ENDDO  
                   EVAR(LFT:LLT) = USER(LFT:LLT)
                 ENDIF
c-----------
              ELSEIF (IFUNC == 4937) THEN  !  /ANIM/ELEM/DT
                IF(GBUF%G_DT>0)THEN
                  DO I=LFT,LLT
                    EVAR(I) = GBUF%DT(I)
                  ENDDO
                ENDIF
c-----------
              !/ANIM/ELEM/MOM || MOMX || MOMY || MOMZ || MOMXY || MOMYZ || MOMXZ ||   |MOM|
              ELSEIF (IFUNC>=4938 .AND. IFUNC<=4944)THEN              
                MT          = IXS(1,NFT+1)                                                                   
                IALEFVM_FLG = IPM(251,MT)                                                                    
                IF(IALEFVM_FLG >= 2)THEN                                                                   
                    IF (ISOLNOD == 8)THEN 
                      IF(IFUNC>=4938 .AND. IFUNC<=4940)THEN                                                                 
                        DO I=LFT,LLT                                                                           
                          EVAR(I) = GBUF%MOM(JJ(IFUNC-4937) + I)                                                
                        ENDDO 
                      ELSEIF(IFUNC==4941)THEN
                        DO I=LFT,LLT   
!                          IAD0     = (I-1)*3                                                                                                                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     +                                     GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I) ) 
                        ENDDO 
                      ELSEIF(IFUNC==4942)THEN
                        DO I=LFT,LLT   
!                          IAD0     = (I-1)*3                                                                                                                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I)+ 
     +                                     GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) ) 
                        ENDDO 
                      ELSEIF(IFUNC==4943)THEN
                        DO I=LFT,LLT   
!                          IAD0     = (I-1)*3                                                                                                                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     +                                     GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) ) 
                        ENDDO 
                      ELSEIF(IFUNC==4944)THEN
                        DO I=LFT,LLT  
!                          IAD0     = (I-1)*3                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     +                                     GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I)+
     +                                     GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )                 
                        ENDDO 
                      ENDIF    
                    ENDIF                                                                                    
                ELSE
                  EVAR(LFT:LLT)=ZERO                                                                      
                ENDIF!IF(IALEFVM_FLG >= 2)                                                                               
c-----------  
              !/ANIM/ELEM/VEL || VELX || VELY || VELZ || VELXY || VELYZ || VELXZ || |VEL|
              ELSEIF (IFUNC>=4945 .AND. IFUNC<=4951)THEN              
                MT          = IXS(1,NFT+1)                                                                   
                IALEFVM_FLG = IPM(251,MT)                                                                    
                IF(IALEFVM_FLG >= 2)THEN                                                                   
                    IF (ISOLNOD == 8)THEN 
                      IF(IFUNC>=4945 .AND. IFUNC<=4947)THEN                                                                 
                        DO I=LFT,LLT                                                                           
                          EVAR(I) =       GBUF%MOM(JJ(IFUNC-4944)+I)                 / GBUF%RHO(I)                                               
                        ENDDO 
                      ELSEIF(IFUNC==4948)THEN
                        DO I=LFT,LLT  
!                          IAD0     = (I-1)*3                                                                                                                                                                          
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     +                                     GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I) )       / GBUF%RHO(I)                                                
                        ENDDO 
                      ELSEIF(IFUNC==4949)THEN
                        DO I=LFT,LLT   
!                          IAD0     = (I-1)*3                                                                                                                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I)+ 
     +                                     GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )       / GBUF%RHO(I)                                               
                        ENDDO 
                      ELSEIF(IFUNC==4950)THEN
                        DO I=LFT,LLT                                                                                                   
!                          IAD0     = (I-1)*3                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     +                                     GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )       / GBUF%RHO(I)                                               
                        ENDDO 
                      ELSEIF(IFUNC==4951)THEN
                        DO I=LFT,LLT  
!                          IAD0     = (I-1)*3                                                                         
                          EVAR(I) = SQRT ( GBUF%MOM(JJ(1)+I)*GBUF%MOM(JJ(1)+I)+ 
     +                                     GBUF%MOM(JJ(2)+I)*GBUF%MOM(JJ(2)+I)+
     +                                     GBUF%MOM(JJ(3)+I)*GBUF%MOM(JJ(3)+I) )       / GBUF%RHO(I)
                        ENDDO 
                      ENDIF    
                    ENDIF                                                                                    
                ELSE
                  EVAR(LFT:LLT)=ZERO                                                                      
                ENDIF!IF(IALEFVM_FLG >= 2)    
c-----------
              !/ANIM/ELEM/FINT || FINTX || FINTY || FINXY || FINYZ || FINXZ
              ELSEIF (IFUNC>=4952 .AND. IFUNC<=4958)THEN              
                MT          = IXS(1,NFT+1)                                                                   
                IALEFVM_FLG = IPM(251,MT)                                                                    
                IF(IALEFVM_FLG >= 2)THEN                                                                   
                    IF (ISOLNOD == 8)THEN 
                      IF(IFUNC>=4952 .AND. IFUNC<=4954)THEN                                                                 
                        DO I=LFT,LLT 
                          II      = I+NFT                                                                          
                          EVAR(I) = ALEFVM_Buffer%FINT_CELL(IFUNC-4951,II)                                              
                        ENDDO 
                      ELSEIF(IFUNC==4955)THEN
                        DO I=LFT,LLT  
                          II      = I+NFT                                                                                                                                                                        
                          EVAR(I) = SQRT (  ALEFVM_Buffer%FINT_CELL(1,II)* ALEFVM_Buffer%FINT_CELL(1,II)+ 
     +                                      ALEFVM_Buffer%FINT_CELL(2,II)* ALEFVM_Buffer%FINT_CELL(2,II) )                                 
                        ENDDO 
                      ELSEIF(IFUNC==4956)THEN
                        DO I=LFT,LLT   
                          II      = I+NFT                                                                                                                                      
                          EVAR(I) = SQRT ( ALEFVM_Buffer%FINT_CELL(2,II)* ALEFVM_Buffer%FINT_CELL(2,II)+ 
     +                                     ALEFVM_Buffer%FINT_CELL(3,II)* ALEFVM_Buffer%FINT_CELL(3,II) )                                
                        ENDDO 
                      ELSEIF(IFUNC==4957)THEN
                        DO I=LFT,LLT                                                                
                          II      = I+NFT                                         
                          EVAR(I) = SQRT ( ALEFVM_Buffer%FINT_CELL(1,II)* ALEFVM_Buffer%FINT_CELL(1,II)+ 
     +                                     ALEFVM_Buffer%FINT_CELL(3,II)* ALEFVM_Buffer%FINT_CELL(3,II) )                                
                        ENDDO 
                      ELSEIF(IFUNC==4958)THEN
                        DO I=LFT,LLT  
                          II      = I+NFT                                         
                          EVAR(I) = SQRT ( ALEFVM_Buffer%FINT_CELL(1,II)* ALEFVM_Buffer%FINT_CELL(1,II)+ 
     +                                     ALEFVM_Buffer%FINT_CELL(2,II)* ALEFVM_Buffer%FINT_CELL(2,II)+ 
     +                                     ALEFVM_Buffer%FINT_CELL(3,II)* ALEFVM_Buffer%FINT_CELL(3,II) )                             
                        ENDDO 
                      ENDIF    
                    ENDIF                                                                                    
                ELSE
                  EVAR(LFT:LLT)=ZERO                                                                      
                ENDIF!IF(IALEFVM_FLG >= 2)
c-----------
              ELSEIF (IFUNC == 4959) THEN  !  /ANIM/ELEM/AMS
                IF(GBUF%G_ISMS>0)THEN
                  DO I=LFT,LLT
                    EVAR(I) = GBUF%ISMS(I)
                  ENDDO
                ENDIF  
c-----------
              ELSEIF(IFUNC == 4960)THEN
                !VORTICITE-Y
                IDX = ANIM_SE(10)*NUMELS 
                DO I=LFT,LLT
                  EVAR(I) = FANI_CELL(IDX+I+NFT)                 
                ENDDO     
c-----------
              ELSEIF(IFUNC == 4961)THEN
                !VORTICITE-Z
                IDX = (ANIM_SE(10)+ANIM_SE(4960))*NUMELS 
                DO I=LFT,LLT
                  EVAR(I) = FANI_CELL(IDX+I+NFT)                 
                ENDDO   
c-----------
              ELSEIF(IFUNC == 4962)THEN
                !VORTICITE-NORM
                DO I=LFT,LLT
                  IF(MLW == 6 .OR. MLW == 17)THEN      
                    EVAR(I) = LBUF%VK(I)                 
                  ELSEIF(MLW == 46 .OR. MLW == 47)THEN 
                     EVAR(I) = MBUF%VAR(NEL+I)      ! UVAR(I,2)  
                  ENDIF                                
                ENDDO                                                                                       
c-----------
              ELSEIF(IFUNC == 4963)THEN
                !Internal Energy (J) = rho.e.V     /ANIM/ELEM/ENER is sipmply "e" (J/kg)
                DO I=LFT,LLT
                  EVAR(I) = GBUF%EINT(I)*GBUF%VOL(I)                          
                ENDDO 
c-----------                
              ELSEIF(IFUNC == 4964 .AND. (MLW == 12 .OR. MLW ==14 .OR. MLW == 25))THEN
C              
                DO I=LFT,LLT                                          
                    EVAR(I) = ZERO                                      
                ENDDO  
                IF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .             (ISOLNOD == 8.AND.JHBE == 14).OR.
     .             ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15))THEN
                    DO IL=1,NLAY                          
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                        DO IS=1,NPTS                                        
                         DO IT=1,NPTT                                      
                          DO IR=1,NPTR                                    
                            LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            DO I=LFT,LLT                                 
                              EVAR(I) = EVAR(I) + LBUF%PLA(I)/NPTG          
                            ENDDO
                           ENDDO                                           
                          ENDDO                                             
                        ENDDO                                               
                      ENDIF                                        
                    ENDDO            
                ELSE
                  DO I=LFT,LLT
                   IF (GBUF%G_PLA > 0) EVAR(I) = GBUF%PLA(I)
                  ENDDO
                ENDIF ! Isolid ...  
c-----------OFF       
              ELSEIF(IFUNC == 4965)THEN
                DO I=LFT,LLT
                  IF (GBUF%G_OFF > 0) THEN
                    IF(GBUF%OFF(I) > ONE) THEN
                      EVAR(I) = GBUF%OFF(I) - ONE
                    ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                      EVAR(I) = GBUF%OFF(I)
                    ELSE
                      EVAR(I) = -ONE
                    ENDIF
                  ENDIF
                ENDDO  
c-----------Mach Number                     
              ELSEIF(IFUNC == 4966) THEN  
                IF (MLW == 151) THEN                                                                      
                   DO I = 1, NEL                                                                          
                      VEL(1) = MULTI_FVM%VEL(1, I + NFT)                                                  
                      VEL(2) = MULTI_FVM%VEL(2, I + NFT)                                                  
                      VEL(3) = MULTI_FVM%VEL(3, I + NFT)                                                  
                      VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                            
                      EVAR(I) = VEL(0)/MULTI_FVM%SOUND_SPEED(I + NFT)                                    
                   ENDDO                                                                                  
                ELSEIF(ALEFVM_Param%ISOLVER>1)THEN                                                                  
                     L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                                                     
                     IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                          
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                        
                        DO  I=1,NEL                                                                       
                           VEL(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I)                                     
                           VEL(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)                                     
                           VEL(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)                                     
                           VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                                    
                           EVAR(I) = VEL(0)/LBUF%SSP(I)                                                  
                        ENDDO                                                                             
                     ENDIF                                                                                
                ELSE
                  IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                          
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                     
                    IF(IS_ALE /= 0)THEN
                    !ale
                      DO  I=1,NEL    
                        TMP(1,1:8)=V(1,IXS(2:9,I+NFT))-W(1,IXS(2:9,I+NFT))
                        TMP(2,1:8)=V(2,IXS(2:9,I+NFT))-W(2,IXS(2:9,I+NFT))
                        TMP(3,1:8)=V(3,IXS(2:9,I+NFT))-W(3,IXS(2:9,I+NFT))                                    
                        VEL(1) = SUM(TMP(1,1:8))*ONE_OVER_8
                        VEL(2) = SUM(TMP(2,1:8))*ONE_OVER_8
                        VEL(3) = SUM(TMP(3,1:8))*ONE_OVER_8 
                        EVAR(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)                                   
                      ENDDO
                    ELSE
                    !euler and lagrange
                      DO  I=1,NEL                        
                        TMP(1,1:8)=V(1,IXS(2:9,I+NFT))
                        TMP(2,1:8)=V(2,IXS(2:9,I+NFT))
                        TMP(3,1:8)=V(3,IXS(2:9,I+NFT))                                    
                        VEL(1) = SUM(TMP(1,1:8))*ONE_OVER_8
                        VEL(2) = SUM(TMP(2,1:8))*ONE_OVER_8
                        VEL(3) = SUM(TMP(3,1:8))*ONE_OVER_8 
                        EVAR(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)                                   
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF                                  
c------------------------------------ Color Function
            ELSEIF(IFUNC == 4967)THEN                                
              GBUF => ELBUF_TAB(NG)%GBUF 
              IF (MLW == 151) THEN 
                 NFRAC=NLAY
                 DO IMAT=1,NLAY
                    LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)
                    DO I=1,NEL
                       VFRAC(I,IMAT) = LBUF%VOL(I) / GBUF%VOL(I)         
                    ENDDO
                  ENDDO
              ELSEIF(MLW == 20)THEN 
                NFRAC=2
                DO I=1,NEL
                  VFRAC(I,1) = ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%VOL(I) / GBUF%VOL(I)  
                  VFRAC(I,2) = ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)%VOL(I) / GBUF%VOL(I)                           
                ENDDO 
              ELSEIF(MLW == 37)THEN 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1) 
                NFRAC=2                  
                DO  I=1,NEL
                  VFRAC(I,1) = MBUF%VAR(I+3*NEL)
                  VFRAC(I,2) = MBUF%VAR(I+4*NEL)
                ENDDO 
              ELSEIF(MLW == 51)THEN
                !get UPARAM
                IMAT = IXS(1,NFT+1)                        
                IADBUF = IPM(7,IMAT)                       
                NUPARAM= IPM(9,IMAT)                       
                UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM) 
                !bijective order           !indexes                  
                ISUBMAT = UPARAM(276+1);   IU(1)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                ISUBMAT = UPARAM(276+2);   IU(2)=N0PHAS+(ISUBMAT-1)*NVPHAS
                ISUBMAT = UPARAM(276+3);   IU(3)=N0PHAS+(ISUBMAT-1)*NVPHAS
                ISUBMAT = UPARAM(276+4);   IU(4)=N0PHAS+(ISUBMAT-1)*NVPHAS 
                MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)  
                NFRAC=4   
                DO  I=1,NEL
                  VFRAC(I,1) = MBUF%VAR(I+IU(1)*NEL)
                  VFRAC(I,2) = MBUF%VAR(I+IU(2)*NEL)   
                  VFRAC(I,3) = MBUF%VAR(I+IU(3)*NEL)   
                  VFRAC(I,4) = MBUF%VAR(I+IU(4)*NEL)                                       
                ENDDO 
              ELSE
                NFRAC = 0
                VFRAC(1:NEL,1:21)=ZERO     
              ENDIF 
              IF(NFRAC>0)THEN
                DO I=1,NEL
                  VALUES(I)=ZERO
                  DO IMAT=1,NFRAC
                    VALUES(I) = VALUES(I) + VFRAC(I,IMAT)*IMAT
                  ENDDO
                  EVAR(I)=VALUES(I) 
                ENDDO  
              ELSE
                EVAR(1:NEL)=ZERO     
              ENDIF       
c------------------------------------ Damage
            ELSEIF ((IFUNC == 4968).AND.GBUF%G_DMG>0) THEN  !  /ANIM/ELEM/DAMG
                DO I = LFT, LLT
                  EVAR(I) = ZERO
                ENDDO
                DO IL=1,NLAY                                              
                  DO IS=1,NPTS                                            
                    DO IT=1,NPTT                                          
                      DO IR=1,NPTR    
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                        DO I=LFT,LLT
                          EVAR(I) = EVAR(I) + LBUF%DMG(I)/NPTG  
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO 
c------------------------------------ Non-local plastic strain
            ELSEIF ((IFUNC == 4969).AND.GBUF%G_PLANL>0) THEN  !  /ANIM/ELEM/NL_EPSP
                DO I = LFT, LLT
                  EVAR(I) = ZERO
                ENDDO
                ! Only 1 layer is supported by non-local for now                                              
                DO IS=1,NPTS                                            
                  DO IT=1,NPTT                                          
                    DO IR=1,NPTR    
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                      DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%PLANL(I)/NPTG  
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO 
c------------------------------------ Non-local plastic strain
            ELSEIF ((IFUNC == 4970).AND.GBUF%G_EPSDNL>0) THEN  !  /ANIM/ELEM/NL_EPSD
                DO I = LFT, LLT
                  EVAR(I) = ZERO
                ENDDO
                ! Only 1 layer is supported by non-local for now                                              
                DO IS=1,NPTS                                            
                  DO IT=1,NPTT                                          
                    DO IR=1,NPTR    
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                      DO I=LFT,LLT
                         EVAR(I) = EVAR(I) + LBUF%EPSDNL(I)/NPTG  
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO 
c------------------------------------ Tsai-Wu criterion          
            ! -- Mean value     
            ELSEIF(IFUNC == 4971 .AND. GBUF%G_TSAIWU > 0)THEN
C              
                DO I=LFT,LLT                                          
                  EVAR(I) = ZERO                                      
                ENDDO  
                DO IL=1,NLAY                          
                  IF (ELBUF_TAB(NG)%BUFLY(IL)%L_TSAIWU > 0) THEN
                    DO IS=1,NPTS                                        
                      DO IT=1,NPTT                                      
                        DO IR=1,NPTR                                    
                          LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                          DO I=LFT,LLT                                 
                            EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)/NPTG          
                          ENDDO
                        ENDDO                                           
                      ENDDO                                             
                    ENDDO                                               
                  ENDIF                                        
                ENDDO   
C
            ! -- Layer value
            ELSEIF(IFUNC >= 4971+1 .AND. IFUNC<= 4971+200 .AND. GBUF%G_TSAIWU > 0) THEN
                DO I=LFT,LLT
                  EVAR(I) = ZERO 
                ENDDO               
                IUS = IFUNC - 4971
                IF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .             (ISOLNOD == 8.AND.JHBE == 14).OR.
     .             ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15))THEN
                  IF (IUS <= NPTG) THEN      
                    DO IL=1,NLAY                                          
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_TSAIWU > 0) THEN
                        DO IS=1,NPTS                                        
                         DO IT=1,NPTT                                      
                          DO IR=1,NPTR                                    
                            LBUF=>ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            DO I=LFT,LLT                                 
                              EVAR(I) = EVAR(I) + LBUF%TSAIWU(I)              
                            ENDDO
                           ENDDO                                           
                          ENDDO                                             
                        ENDDO                                               
                      ENDIF                                        
                    ENDDO                                                 
                  ENDIF                                              
                ENDIF  

c------------------------------------ Tillotson Region id
            ELSEIF( IFUNC == 5172 ) THEN
              DO I=LFT,LLT
                EVAR(I) = ZERO
              ENDDO 
              MT = IXS(1,NFT+1)
              IF (MLW == 151) THEN 
                NLAY = ELBUF_TAB(NG)%NLAY 
                !count number of submaterial based on /EOS/TILLOTSON (IEOS=3)            
                NTILLOTSON = 0 
                DO IMAT=1,NLAY
                  IEOS =  IPM(4, IPM(20 + IMAT,MT) )
                  IF(IEOS == 3)THEN 
                    NTILLOTSON = NTILLOTSON + 1 
                    IMAT_TILLOTSON = IMAT 
                  ENDIF 
                ENDDO 
                !several Tillotson EoS   Value= sum ( Region_i*10**(i-1),  i=1,imat)       
                IF(NTILLOTSON > 1)THEN 
                  FAC=ONE
                  DO IMAT=1,NLAY
                    IEOS =  IPM(4,    IPM(20 + IMAT,MT) )
                    IF(IEOS == 3)THEN 
                      EBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%EOS(1,1,1) 
                      NVAREOS = ELBUF_TAB(NG)%BUFLY(IMAT)%NVAR_EOS
                      DO I=1,NEL
                        EVAR(I) = EVAR(I) + EBUF%VAR(I) * FAC
                      ENDDO 
                    ENDIF 
                    FAC=FAC*TEN 
                  ENDDO 
                !single Tillotson EoS   Value=  Region_i                                 
                ELSEIF(NTILLOTSON == 1)THEN
                      EBUF => ELBUF_TAB(NG)%BUFLY(IMAT_TILLOTSON)%EOS(1,1,1)
                      NVAREOS = ELBUF_TAB(NG)%BUFLY(IMAT_TILLOTSON)%NVAR_EOS 
                      DO I=1,NEL
                        EVAR(I) = EBUF%VAR(I)
                      ENDDO 
                ENDIF 
              ELSE 
                !monomaterial law                                                        
                IEOS = IPM(4,MT)
                IF(IEOS == 3)THEN
                  EBUF => ELBUF_TAB(NG)%BUFLY(1)%EOS(1,1,1)                              
                  NVAREOS = ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS                              
                  DO  I=1,NEL
                    EVAR(I) = EBUF%VAR(I)
                  ENDDO
                ENDIF
              ENDIF
                       
c------------------------------------ 
              !OTHER IFUNC VALUES
              ELSE 
                  DO I=LFT,LLT                                          
                    EVAR(I) = ZERO                                      
                  ENDDO                                       
              ENDIF    ! IFUNC general
c-----------                 
            ENDIF  !  IF (MLW /= 0 .and. MLW /= 13 .and. IGTYP /= 0)   
c--------------------------------

            IF (ISOLNOD == 16)THEN
               DO I=LFT,LLT
                N = NN2 + I + NFT
                IF(EL2FA(N)/=0)THEN
                  FUNC(EL2FA(N))   = EVAR(I)
                  FUNC(EL2FA(N)+1) = EVAR(I)
                  FUNC(EL2FA(N)+2) = EVAR(I)
                  FUNC(EL2FA(N)+3) = EVAR(I)
                ENDIF
               ENDDO
             ELSE
               DO I=LFT,LLT
                N = NN2 + I + NFT
                IF(EL2FA(N)/=0)THEN
                  FUNC(EL2FA(N)) = EVAR(I)
                ENDIF
               ENDDO
            ENDIF
C        
C-----------------------------------------------
          ELSEIF (ISPH3D == 1.AND.ITY == 51) THEN
C           TETRAS SPH.
C-----------------------------------------------
            GBUF => ELBUF_TAB(NG)%GBUF
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            NLAY = ELBUF_TAB(NG)%NLAY                    
            NPTR = ELBUF_TAB(NG)%NPTR                     
            NPTS = ELBUF_TAB(NG)%NPTS                     
            NPTT = ELBUF_TAB(NG)%NPTT                     
            NPTG = NPTT*NPTS*NPTR*NLAY
            JTURB= IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))
C-----------
            IF(IFUNC == 1)THEN
              DO I=LFT,LLT
                N = I + NFT
                VALUE = ZERO
                IF (EL2FA(NN3+N)/=0)THEN
                  IF (MLW == 21)THEN
                    VALUE = LBUF%EPSQ(I)  
                  ELSEIF (GBUF%G_PLA > 0) THEN 
                    VALUE = GBUF%PLA(I)     
                  ENDIF                     
                  FUNC(EL2FA(NN3+N))  = VALUE
                ENDIF
              ENDDO
C-----------
            ELSEIF(IFUNC == 2)THEN
              DO I=LFT,LLT
                N = I + NFT
                IF(EL2FA(NN3+N)/=0)THEN
                  VALUE = GBUF%RHO(I)
                  FUNC(EL2FA(NN3+N)) = VALUE
                ENDIF
              ENDDO
C-----------
            ELSEIF(IFUNC == 3)THEN
              DO I=LFT,LLT
                N = I + NFT
                IALEL=IPARG(7,NG)+IPARG(11,NG)
                IF(IALEL == 0)THEN
                  IPRT=IPARTSP(N)
                  MT  =IPART(1,IPRT)
                  VALUE = GBUF%EINT(I)/MAX(EM30,PM(1,MT))
                ELSE
                  VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                ENDIF
                FUNC(EL2FA(NN3+N)) = VALUE
              ENDDO
C-----------
            ELSEIF(IFUNC == 4)THEN
             DO I=LFT,LLT
              N = I + NFT
              IF(EL2FA(NN3+N)/=0)THEN
                IF (GBUF%G_TEMP > 0) THEN
                  VALUE = GBUF%TEMP(I)
                ELSE
                  VALUE = ZERO
                ENDIF
                FUNC(EL2FA(NN3+N)) = VALUE
              ENDIF
             ENDDO
C-----------
            ELSEIF(IFUNC == 6.OR.IFUNC == 7)THEN
             DO I=LFT,LLT
              N = I + NFT
              IF(EL2FA(NN3+N)/=0)THEN
               S11 = GBUF%SIG(JJ(1) + I)
               S22 = GBUF%SIG(JJ(2) + I)
               S33 = GBUF%SIG(JJ(3) + I)
               S4  = GBUF%SIG(JJ(4) + I) 
               S5  = GBUF%SIG(JJ(5) + I) 
               S6  = GBUF%SIG(JJ(6) + I) 
               IF(IVISC > 0 ) THEN
                  S11 =S11 + LBUF%VISC(JJ(1) + I)
                  S22 =S22 + LBUF%VISC(JJ(2) + I)
                  S33 =S33 + LBUF%VISC(JJ(3) + I)
                  S4  =S4  + LBUF%VISC(JJ(4) + I)
                  S5  =S5  + LBUF%VISC(JJ(5) + I)
                  S6  =S6  + LBUF%VISC(JJ(6) + I)
               ENDIF
                 P = - (S11 + S22 + S33 ) * THIRD
                 VALUE = P
                 IF(IFUNC == 7) THEN
                   S1=S11 + P
                   S2=S22 + P
                   S3=S33 + P
                   VONM2= THREE*(S4*S4 + S5*S5 + S6*S6 +
     .                    HALF*(S1*S1+S2*S2+S3*S3) )
                   VONM= SQRT(VONM2)
                   VALUE = VONM
                 ENDIF
                 FUNC(EL2FA(NN3+N)) = VALUE
              ENDIF
             ENDDO
C-----------
            ELSEIF(IFUNC == 8.AND.JTURB/=0)THEN
C             ENERGIE TURBULENTE
              DO I=LFT,LLT
               NN = EL2FA(NN3 + I + NFT)
               IF(NN/=0)THEN
                 FUNC(NN) = GBUF%RK(I)
               ENDIF
              ENDDO
C-----------
            ELSEIF(IFUNC == 9)THEN
C             VISCOSITE TURBULENTE
              DO I=LFT,LLT
                N = I + NFT
                NN = EL2FA(NN3 + I + NFT)
                IF(NN/=0)THEN
                  IF((MLW == 6 .OR. MLW == 17).AND.JTURB/=0)THEN
                    IPRT=IPARTSP(N)
                    MT  =IPART(1,IPRT)
                    VALUE=PM(81,MT)*GBUF%RK(I)**2/
     .                     MAX(EM15,GBUF%RE(I))
                  ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                    VALUE = MBUF%VAR(I)
                  ELSE
                    VALUE = ZERO
                  ENDIF
                  FUNC(NN) = VALUE
                ENDIF
              ENDDO
C-----------
            ELSEIF(IFUNC == 10)THEN
C             VORTICITE
              DO I=LFT,LLT
                NN = EL2FA(NN3 + I + NFT)
                IF(NN/=0)THEN
                  IF(MLW == 6 .OR. MLW == 17)THEN
                    VALUE = LBUF%VK(I)
                  ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                    VALUE = MBUF%VAR(NEL+I) 
                  ELSE
                    VALUE = ZERO
                  ENDIF
                  FUNC(NN) = VALUE
                ENDIF
              ENDDO
C-----------
            ELSEIF((IFUNC == 11.OR.IFUNC == 12.OR.IFUNC == 13)
     .          .AND.MLW == 24)THEN
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN3+N)) = LBUF%DAM(JJ(IFUNC-10) + I)
              ENDDO
C-----------
            ELSEIF(IFUNC>=14.AND.IFUNC<=19)THEN
             IF(IVISC == 0) THEN
               DO I=LFT,LLT
                N = I + NFT
                IF(EL2FA(NN3+N)/=0)THEN
                  VALUE = GBUF%SIG(JJ(IFUNC - 13) + I)
                    FUNC(EL2FA(NN3+N)) = VALUE
                ENDIF
               ENDDO
             ELSE
               DO I=LFT,LLT
                N = I + NFT
                IF(EL2FA(NN3+N)/=0)THEN
                  VALUE = GBUF%SIG(JJ(IFUNC - 13) + I) +
     .                    LBUF%VISC(JJ(IFUNC - 13) + I)
                    FUNC(EL2FA(NN3+N)) = VALUE
                ENDIF
               ENDDO
             ENDIF  
             
C-----------
            ELSEIF(IFUNC>=20.AND.IFUNC<=24)THEN
              IUS = IFUNC - 20                                      
              NUVAR = IPM(8,MT)                                   
              IF (NUVAR > 0) THEN
                DO I=LFT,LLT
                 N = I + NFT
                 IF(EL2FA(NN3+N)/=0 . AND. IUS <= NUVAR)THEN
                   VALUE = MBUF%VAR(I + IUS*NEL)
                   FUNC(EL2FA(NN3+N)) = VALUE
                 ENDIF
                ENDDO
             ENDIF
C-----------
             ELSEIF(IFUNC == 25)THEN
              DO I=LFT,LLT
               N = I + NFT
               IF(EL2FA(NN3+N)/=0)THEN
C                  FUNC(EL2FA(NN3+N)) = EHOUR(N)
                   VALUE=0.
                   FUNC(EL2FA(NN3+N)) = VALUE
               ENDIF
             ENDDO
C-----------
            ELSEIF(IFUNC == 887)THEN
              DO I=LFT,LLT
                N = I + NFT
                VALUE = ZERO
                IF (EL2FA(NN3+N)/=0)THEN
                  IF (GBUF%G_BFRAC > 0)THEN
                    VALUE = GBUF%BFRAC(I)  
                  ENDIF                     
                  FUNC(EL2FA(NN3+N))  = VALUE
                ENDIF
              ENDDO 
C-----------                 
              ELSEIF(IFUNC == 3890) THEN

                  NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL  
                  DO IR=1,NFAIL                          
                    DFMAX=>
     .                ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)%FLOC(IR)%DAMMX
                    DO I=LFT,LLT   
          N = I + NFT         
          FUNC(EL2FA(NN3+N)) = DFMAX(I)    
                    ENDDO
                  ENDDO   
C-----------        
            ELSEIF(IFUNC == 4893)THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (EL2FA(NN3+N)/=0)THEN 
                  FUNC(EL2FA(NN3+N))  = ISPMD
                ENDIF
              ENDDO            
C-----------        
            ELSEIF(IFUNC == 4894)THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (EL2FA(NN3+N)/=0)THEN 
                  FUNC(EL2FA(NN3+N))  = GBUF%FILL(I)
                ENDIF
              ENDDO            
C-----------
            ELSEIF (IFUNC == 4895) THEN  !  /ANIM/ELEM/SIGEQ
            !  equivalent stress - other then VON MISES
              IF (GBUF%G_SEQ > 0) THEN  !  non VON MISES
!                DO I=LFT,LLT
!                  EVAR_TMP = ZERO
!                  N = I + NFT
!                  IF (EL2FA(NN3+N) /= 0) THEN
!                    NPTG = NLAY*NPTR*NPTS*NPTT
!                    DO IL=1,NLAY
!                      DO IT=1,NPTT
!                        DO IR=1,NPTR
!                          DO IS=1,NPTS
!                            LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
!                            IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0) THEN
!                              EVAR_TMP = EVAR_TMP + LBUF%SEQ(I)/NPTG
!                            ELSE
!                              S11 = LBUF%SIG(JJ(1) + I)
!                              S22 = LBUF%SIG(JJ(2) + I)
!                              S33 = LBUF%SIG(JJ(3) + I)
!                              S4  = LBUF%SIG(JJ(4) + I) 
!                              S5  = LBUF%SIG(JJ(5) + I) 
!                              S6  = LBUF%SIG(JJ(6) + I) 
!                              IF (IVISC > 0) THEN
!                                S11 = S11 + LBUF%VISC(JJ(1) + I)
!                                S22 = S22 + LBUF%VISC(JJ(2) + I)
!                                S33 = S33 + LBUF%VISC(JJ(3) + I)
!                                S4  = S4  + LBUF%VISC(JJ(4) + I)
!                                S5  = S5  + LBUF%VISC(JJ(5) + I)
!                                S6  = S6  + LBUF%VISC(JJ(6) + I)
!                              ENDIF                                 
!                              P = - (S11 + S22 + S33) * THIRD
!                              S1 = S11 + P
!                              S2 = S22 + P
!                              S3 = S33 + P
!                              VONM2 =  THREE*(S4*S4 + S5*S5 + S6*S6 +
!     .                                HALF*(S1*S1 + S2*S2 + S3*S3))
!                              VONM = SQRT(VONM2)
!                              EVAR_TMP = EVAR_TMP + VONM/NPTG
!                            ENDIF ! IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0)
!                          ENDDO ! DO IS=1,NPTS
!                        ENDDO ! DO IR=1,NPTR
!                      ENDDO ! DO IT=1,NPTT
!                    ENDDO ! DO IL=1,NLAY
!                  ENDIF ! IF (EL2FA(NN3+N) /= 0)
!                  FUNC(EL2FA(NN3+N)) = EVAR_TMP
!                ENDDO ! DO I=LFT,LLT
!!!!!!
                  IPRT = IPARTSP(NFT+1)
                  IMAT = IPART(1,IPRT)
                  IADBUF = IPM(7,IMAT)
                  NUPARAM= IPM(9,IMAT)       
                  UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
!---
                IF (MLW == 72) THEN
!               (ILAW = 74) -- Hill MMC (anisotropic)
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN3+N) /= 0) THEN
                      FUNC(EL2FA(NN3+N)) = GBUF%SEQ(I)
                    ENDIF ! IF (EL2FA(NN3+N) /= 0)
                  ENDDO ! DO I=LFT,LLT
                ELSEIF (MLW == 74) THEN
!                 (ILAW = 74) -- Thermal Hill Orthotropic 3D Material
                  FF0 = UPARAM(7)
                  GG0 = UPARAM(8)
                  HH0 = UPARAM(9)
                  LL0 = UPARAM(10)
                  MM0 = UPARAM(11)
                  NN0 = UPARAM(12)
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN3+N) /= 0) THEN
                      S11 = GBUF%SIG(JJ(1) + I)
                      S22 = GBUF%SIG(JJ(2) + I)
                      S33 = GBUF%SIG(JJ(3) + I)
                      S4  = GBUF%SIG(JJ(4) + I) 
                      S5  = GBUF%SIG(JJ(5) + I) 
                      S6  = GBUF%SIG(JJ(6) + I)
                      IF (IVISC > 0) THEN
                        S11 = S11 + LBUF%VISC(JJ(1) + I)
                        S22 = S22 + LBUF%VISC(JJ(2) + I)
                        S33 = S33 + LBUF%VISC(JJ(3) + I)
                        S4  = S4  + LBUF%VISC(JJ(4) + I)
                        S5  = S5  + LBUF%VISC(JJ(5) + I)
                        S6  = S6  + LBUF%VISC(JJ(6) + I)
                      ENDIF
                      P = - (S11 + S22 + S33) * THIRD
                      S1 = S11 + P
                      S2 = S22 + P
                      S3 = S33 + P
!
                      CRIT = FF0*(S2 - S3)**2
     .                     + GG0*(S3 - S1)**2
     .                     + HH0*(S1 - S2)**2
     .                     + TWO*LL0*S5**2
     .                     + TWO*MM0*S6**2
     .                     + TWO*NN0*S4**2
!
                      FUNC(EL2FA(NN3+N)) = SQRT(CRIT)
                    ENDIF ! IF (EL2FA(NN3+N) /= 0)
                  ENDDO ! DO I=LFT,LLT
                ELSEIF (MLW == 93) THEN
!               (ILAW = 93) -- orth Hill MMC
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN3+N) /= 0) THEN
                      FUNC(EL2FA(NN3+N)) = GBUF%SEQ(I)
                    ENDIF ! IF (EL2FA(NN3+N) /= 0)
                  ENDDO ! DO I=LFT,LLT
                ELSEIF (MLW == 104) THEN
                  DO IL=1,NLAY                                              
                    DO IS=1,NPTS                                            
                      DO IT=1,NPTT                                          
                        DO IR=1,NPTR    
                          LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                          DO I=LFT,LLT
                            N = I + NFT
                            IF (EL2FA(NN3+N) /= 0) THEN
                              FUNC(EL2FA(NN3+N)) = FUNC(EL2FA(NN3+N)) + LBUF%SEQ(I)/NPTG  
                            ENDIF
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ELSEIF (MLW == 115) THEN
!               (ILAW = 74) -- Hill MMC (anisotropic)
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN3+N) /= 0) THEN
                      FUNC(EL2FA(NN3+N)) = GBUF%SEQ(I)
                    ENDIF ! IF (EL2FA(NN3+N) /= 0)
                  ENDDO ! DO I=LFT,LLT
                ENDIF ! IF (MLW == 72)
!---
              ELSE                      ! VON MISES
                IF (IVISC == 0) THEN
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN3+N) /= 0) THEN
                      P = - (GBUF%SIG(JJ(1) + I)
     .                    +  GBUF%SIG(JJ(2) + I)
     .                    +  GBUF%SIG(JJ(3) + I)) * THIRD
                      S1 = GBUF%SIG(JJ(1) + I)+P
                      S2 = GBUF%SIG(JJ(2) + I)+P
                      S3 = GBUF%SIG(JJ(3) + I)+P
                      VONM2 = THREE*(GBUF%SIG(JJ(4) + I)**2 +
     .                               GBUF%SIG(JJ(5) + I)**2 +
     .                               GBUF%SIG(JJ(6) + I)**2 +
     .                         HALF*(S1*S1+S2*S2+S3*S3))
                      VONM = SQRT(VONM2)
                      FUNC(EL2FA(NN3+N)) = VONM
                    ENDIF ! IF (EL2FA(NN3+N) /= 0)
                  ENDDO ! DO I=LFT,LLT
                ELSE
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN3+N) /= 0) THEN
                      S11 =  GBUF%SIG(JJ(1) + I) + LBUF%VISC(JJ(1) + I)
                      S22 =  GBUF%SIG(JJ(2) + I) + LBUF%VISC(JJ(2) + I)
                      S33 =  GBUF%SIG(JJ(3) + I) + LBUF%VISC(JJ(3) + I)
                      S4  =  GBUF%SIG(JJ(4) + I) + LBUF%VISC(JJ(4) + I)
                      S5  =  GBUF%SIG(JJ(5) + I) + LBUF%VISC(JJ(5) + I)
                      S6  =  GBUF%SIG(JJ(6) + I) + LBUF%VISC(JJ(6) + I)
                      P = - (S11  + S22 + S33) * THIRD
                      S1 = S11 + P
                      S2 = S22 + P
                      S3 = S33 + P
                      VONM2 =  THREE*(S4*S4 + S5*S5 + S6*S6 +
     .                        HALF*(S1*S1 + S2*S2 + S3*S3))
                      VONM = SQRT(VONM2)
                      FUNC(EL2FA(NN3+N)) = VONM
                    ENDIF ! IF (EL2FA(NN3+N) /= 0)
                  ENDDO  ! DO I=LFT,LLT            
                ENDIF  ! IF(IVISC == 0)
              ENDIF ! IF (GBUF%G_SEQ > 0)
c-----------OFF       
              ELSEIF(IFUNC == 4965)THEN
                IF (GBUF%G_OFF > 0) THEN
                  DO I=LFT,LLT
                  N = I + NFT
                    IF(GBUF%OFF(I) > ONE) THEN
                      FUNC(EL2FA(NN3+N)) = GBUF%OFF(I) - ONE
                    ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                      FUNC(EL2FA(NN3+N)) = GBUF%OFF(I)
                    ELSE
                      FUNC(EL2FA(NN3+N)) = -ONE
                    ENDIF
                  ENDDO  
                ENDIF
C-----------
            ELSE
              DO I=LFT,LLT
                N = I + NFT
                IF(EL2FA(NN3+N)/=0)THEN
                  FUNC(EL2FA(NN3+N)) = ZERO
                ENDIF
              ENDDO
            ENDIF   ! IFUNC
C-----------------------------------------------
          ELSEIF (ITY==101) THEN
C           ISOGEOMETRIC ELEMENT  A VERIFIER
C-----------------------------------------------
            GBUF => ELBUF_TAB(NG)%GBUF
c-----------
              IF(IFUNC == 1)THEN
                DO I=LFT,LLT     
                  IF (MLW == 10 .OR. MLW == 21) THEN
                    EVAR(I) = LBUF%EPSQ(I)
                  ELSEIF (GBUF%G_PLA > 0) THEN  
                    EVAR(I) = GBUF%PLA(I)
                  ENDIF
                ENDDO
c
              ELSEIF(IFUNC==6.OR.IFUNC==7)THEN   
                  DO I=LFT,LLT
                     N = I + NFT
                      S11 =  GBUF%SIG(JJ(1) + I) 
                      S22 =  GBUF%SIG(JJ(2) + I) 
                      S33 =  GBUF%SIG(JJ(3) + I) 
                      S4  =  GBUF%SIG(JJ(4) + I) 
                      S5  =  GBUF%SIG(JJ(5) + I) 
                      S6  =  GBUF%SIG(JJ(6) + I) 
                      IF(IVISC > 0)THEN
                        S11 =  S11 + LBUF%VISC(JJ(1) + I)
                        S22 =  S22 + LBUF%VISC(JJ(2) + I)
                        S33 =  S33 + LBUF%VISC(JJ(3) + I)
                        S4  =  S4  + LBUF%VISC(JJ(4) + I)
                        S5  =  S5  + LBUF%VISC(JJ(5) + I)
                        S6  =  S6  + LBUF%VISC(JJ(6) + I)
                      ENDIF 
                      P = - (S11 + S22 + S33) * THIRD
                      VALUE = P 
                      IF (IFUNC==7) THEN
                        S1= S11 + P
                        S2= S22 + P
                        S3= S33 + P
                        VONM2= THREE*(S4*S4 + S5*S5 + S6*S6+
     .                         HALF*(S1*S1+S2*S2+S3*S3) )
                        VONM= SQRT(VONM2)
                        VALUE = VONM
                      ENDIF
                      EVAR(I) = VALUE
                  ENDDO
              ELSEIF(IFUNC==2)THEN
                DO I=LFT,LLT                                            
                  EVAR(I) = GBUF%RHO(I)  
                ENDDO 
              ELSEIF(IFUNC==3)THEN
                DO I=LFT,LLT                                                                             
                  VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                  EVAR(I) = VALUE 
                ENDDO  
c                  IALEL=IPARG(7,NG)+IPARG(11,NG)                        
c                  IF (IALEL == 0) THEN                                    
c                    MT=IXS(1,N)                                         
c                    VALUE = GBUF%EINT(I)/MAX(EM30,PM(1,MT))          
c                  ELSE                                                  
c                    VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
c                  ENDIF                                                 
c                  EVAR(I) = VALUE 
              ELSEIF(IFUNC == 26) THEN
                 IF (ISRAT > 0) THEN
                   DO I=LFT,LLT
                     EVAR(I) = GBUF%EPSD(I)
                   ENDDO
                 ELSE
                   DO I=LFT,LLT
                     EVAR(I) = ZERO
                   ENDDO
                 ENDIF 
              ELSE
                  DO I=LFT,LLT
                      N = I + NFT
                      EVAR(I) = ZERO
                  ENDDO
              ENDIF   ! IFUNC
C
              DO I=LFT,LLT
                N = I + NFT
                DO J=1,27
                  FUNC(EL2FA(NN4+N)+J-1) = EVAR(I)
                ENDDO
              ENDDO
          ELSE
            CONTINUE
          ENDIF     ! ITY
C-----------------------------------------------
        ENDDO       ! FIN DE BOUCLE SUR LES OFFSET
       ENDIF        ! mlw /= 13
 900  CONTINUE 
C-----------------------------------------------
       
      IF (NSPMD == 1) THEN
        DO N=1,NBF
           R4 = FUNC(N)
           CALL WRITE_R_C(R4,1)
        ENDDO
      ELSE
        DO N = 1, NBF
          WAL(N) = FUNC(N)
        ENDDO
        IF (ISPMD == 0) THEN
          BUF = NUMELSG+3*NUMELS16G+NUMSPHG
        ELSE
          BUF=1
        ENDIF
        CALL SPMD_R4GET_PARTN(1,NBF,NBPART,IADG,WAL,BUF)
      ENDIF
c----------- 
      IF(ALLOCATED(WA_L))DEALLOCATE(WA_L)
      RETURN
      END
