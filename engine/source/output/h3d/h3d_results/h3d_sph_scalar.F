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
Chd|  H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_SCALAR              source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SPH_SCALAR(
     .                  ELBUF_TAB ,SPH_SCALAR      ,IFUNC     ,IPARG       ,GEO        ,
     .                  KXSP      ,PM        ,IPART       ,
     .                  EL2FA     ,NBF       ,IXUR      ,IADP        ,
     .                  NBF_L     ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM       ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT    ,X         ,V         ,W           ,
     .                  NV46      ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK     ,ID_ELEM   ,IPARTSP   ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SPH,INFO1,KEYWORD    ,SPBUF       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD               
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
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
#include      "scr17_c.inc"
#include      "scr25_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "sphcom.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SPH_SCALAR(*),X(3,*),V(3,*),W(3,*),THKE(*),EHOUR(*),GEO(NPROPG,*),
     .   ANIM(*),PM(NPROPM,*),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,*),KXSP(NISP,*),EL2FA(*),IFUNC,NBF,IXUR(NIXUR,*),
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,*),
     .   IGEO(NPROPGI,*),INVERT(*), NV46,ID_ELEM(*),IPARTSP(*),
     .   H3D_PART(*),IS_WRITTEN_SPH(*),INFO1,IUVAR_INPUT,IPART(LIPART1,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   EVAR(MVSIZ),DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .   EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .   USER(NUMELS),VALUE(MVSIZ),SPBUF(NSPBUF,*),MASS(MVSIZ),PRES(MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),S11,S22,S33,S4,S5,S6,VONM,GAMA(6),
     .   T11,T21,T31,T12,T22,T32,T13,T23,T33,
     .   PHI,TETA,PSI,DAMMAX,EVAR_TMP,FF0,GG0,HH0,LL0,MM0,NN0,CRIT,MASS0,VOL
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,SWA_L,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),NB_PLYOFF,IUVAR,IPRT,IADBUF,
     .        NUPARAM,IMAT
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),LENCOM,IOFF,NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,ISOLNOD,NPTG,TSHELL,TSH_ORT,
     .        IOK_PART(MVSIZ),JJ(6),IRUPT,IOK,NPG_PLANE,NUMLAY,IJK,IIR,
     .        IS_WRITTEN_VALUE(MVSIZ),IPOS,ITRIMAT
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_MAT_)  ,POINTER :: MBUF      
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      my_real,
     .  DIMENSION(:), POINTER  :: UVARF,DAMF,DFMAX,TDELE
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
C-----------------------------------------------
      DO I=1,NUMSPH
         IS_WRITTEN_SPH(I) = 0
      ENDDO
c
      DO 900 NG=1,NGROUP

        CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF (MLW /= 13) THEN
          NFT = IPARG(3,NG)
          IOK_PART(1:NEL) = 0 
c
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          DO I=1,NEL
            VALUE(I) = ZERO
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	     
C-----------------------------------------------
          IF (ITY == 51) THEN
c           SPH ELEMENTS
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
            JTURB= IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))

            OFFSET = 0
c
            DO  I=1,NEL 
              IF (ITY == 51) THEN
                ID_ELEM(OFFSET+NFT+I) = KXSP(NISP,NFT+I)
                IF( H3D_PART(IPARTSP(NFT+I)) == 1) IOK_PART(I) = 1
              ENDIF
            ENDDO  
c
            IUVAR = IUVAR_INPUT
C-----------------------------------------------
C Mass computation
C-----------------------------------------------
            IF (KEYWORD == 'MASS') THEN
     	      GBUF => ELBUF_TAB(NG)%GBUF
     	      DO I=1,NEL
     	        N = I + NFT
   	        IPRT=IPARTSP(N)                
     	        MT  =IPART(1,IPRT)             
     	        MASS(I)=PM(89,MT)*GBUF%VOL(I)  
     	      ENDDO
            ENDIF
C-----------
            IF (MLW /= 0 .and. MLW /= 13 .and. IGTYP /= 0) THEN
              JTURB=IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))
C--------------------------------------------------
              IF (KEYWORD == 'MASS') THEN   ! MASS
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = MASS(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DIAMETER')THEN
C--------------------------------------------------
             	DO I=1,NEL
             	  VALUE(I) = SPBUF(1,NFT+I)  
                  IS_WRITTEN_VALUE(I) = 1  
             	ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'EPSP')THEN
C--------------------------------------------------
                IF( GBUF%G_PLA > 0)THEN
                  DO I=1,NEL
                    VALUE(I) = GBUF%PLA(I)  
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDDO
                ENDIF
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DENS')THEN
C----------------------------------------------------
             	DO I=1,NEL
             	  VALUE(I) = GBUF%RHO(I)
                  IS_WRITTEN_VALUE(I) = 1 
             	ENDDO
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'EINTM' .OR. KEYWORD == 'ENER')THEN
C-------------------------------------------------- 
               !LAG: GBUF%VOL = V0,    GBUF%EINT=rho0.e
                DO I=1,NEL                                              
                   N = I + NFT                                          
                   IPRT=IPARTSP(N)                                   
                   MT  =IPART(1,IPRT)                                
                   VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT))   !   
                   IS_WRITTEN_VALUE(I) = 1                              
                ENDDO                                                   
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'EINTV')THEN
C-------------------------------------------------- 
                DO I=1,NEL
                   N = I + NFT
                   IPRT=IPARTSP(N)                                           
                   MT  =IPART(1,IPRT)                                        
                   VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT))*GBUF%RHO(I)   
                   IS_WRITTEN_VALUE(I) = 1
                ENDDO
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'EINT')THEN
C-------------------------------------------------- 
                DO I=1,NEL                                               
                   N = I + NFT                                           
                   IPRT=IPARTSP(N)                                       
                   MT  =IPART(1,IPRT)                                    
                   VOL=GBUF%VOL(I)*PM(89,MT)/GBUF%RHO(I)                 
                   VALUE(I) = GBUF%EINT(I)/PM(89,MT)*GBUF%RHO(I)*VOL     
                   IS_WRITTEN_VALUE(I) = 1                               
                ENDDO                                                    
C-------------------------------------------------- 
              ELSEIF (KEYWORD(1:4) == 'ENTH')THEN
C-------------------------------------------------- 
               DO I=1,NEL
                 PRES(I) = - (GBUF%SIG(JJ(1) + I)+ GBUF%SIG(JJ(2) + I) + GBUF%SIG(JJ(3) + I))*THIRD
               ENDDO
               !GBUF%EINT is rho.e           
               IF(KEYWORD == 'ENTH')THEN
                 DO I=1,NEL                                                             
                    N = I + NFT                                                         
                    IPRT=IPARTSP(N)                                                       
                    MT  =IPART(1,IPRT)                                                    
                    MASS0=GBUF%VOL(I)*PM(89,MT)                                           
                    VOL=MASS0/MAX(EM20,GBUF%RHO(I))                                       
                    VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT)) + PRES(I)*VOL   !         
                    IS_WRITTEN_VALUE(I) = 1                                             
                 ENDDO                                                                  
               ELSEIF(KEYWORD == 'ENTHV')THEN
                 DO I=1,NEL                                                            
                    N = I + NFT                                                        
                    IPRT=IPARTSP(N)                                                    
                    MT  =IPART(1,IPRT)                                                 
                    MASS0=GBUF%VOL(I)*PM(89,MT)                                        
                    VOL=MASS0/MAX(EM20,GBUF%RHO(I))                                    
                    VALUE(I) = GBUF%EINT(I)/MAX(EM20,PM(89,MT))/VOL + PRES(I)  !       
                    IS_WRITTEN_VALUE(I) = 1                                            
                 ENDDO                                                                 
               ELSEIF(KEYWORD == 'ENTHM')THEN
                 DO I=1,NEL                                                                      
                    N = I + NFT                                                                  
                    IPRT=IPARTSP(N)                                                               
                    MT  =IPART(1,IPRT)                                                            
                    MASS0=GBUF%VOL(I)*PM(89,MT)                                                   
                    VOL=MASS0/MAX(EM20,GBUF%RHO(I))                                               
                    MASS(I)=MASS0                                                                 
                    VALUE(I) = (GBUF%EINT(I)/MAX(EM20,PM(89,MT)) + PRES(I)*VOL)/MASS(I)  !        
                    IS_WRITTEN_VALUE(I) = 1                                                      
                 ENDDO                                                                           
               ENDIF                     
C--------------------------------------------------
              ELSEIF(KEYWORD == 'TEMP')THEN
C--------------------------------------------------
               DO I=1,NEL
                IF (GBUF%G_TEMP > 0) THEN
                  VALUE(I) = GBUF%TEMP(I)
                  IS_WRITTEN_VALUE(I) = 1 
                ENDIF
               ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'P')THEN
C--------------------------------------------------
                DO I=1,NEL
                  S11 = GBUF%SIG(JJ(1) + I)
                  S22 = GBUF%SIG(JJ(2) + I)
                  S33 = GBUF%SIG(JJ(3) + I)
                  S4  = GBUF%SIG(JJ(4) + I) 
                  S5  = GBUF%SIG(JJ(5) + I) 
                  S6  = GBUF%SIG(JJ(6) + I) 
                  P = - (S11 + S22 + S33 ) * THIRD
                  VALUE(I) = P
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VONM')THEN
C--------------------------------------------------
                DO I=1,NEL
                  S11 = GBUF%SIG(JJ(1) + I)
                  S22 = GBUF%SIG(JJ(2) + I)
                  S33 = GBUF%SIG(JJ(3) + I)
                  S4  = GBUF%SIG(JJ(4) + I) 
                  S5  = GBUF%SIG(JJ(5) + I) 
                  S6  = GBUF%SIG(JJ(6) + I)
                  P = - (S11 + S22 + S33 ) * THIRD
                  VALUE(I) = P
                  S1=S11 + P
                  S2=S22 + P
                  S3=S33 + P
                  VONM2= THREE*(S4*S4 + S5*S5 + S6*S6 +
     .            	 HALF*(S1*S1+S2*S2+S3*S3) )
                  VONM= SQRT(VONM2)
                  VALUE(I) = VONM
                  IS_WRITTEN_VALUE(I) = 1 
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'K' .AND.JTURB/=0)THEN
C--------------------------------------------------
C             ENERGIE TURBULENTE
                DO I=1,NEL 
                  VALUE(I) = GBUF%RK(I)
                  IS_WRITTEN_VALUE(I) = 1 
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'TVIS')THEN
C--------------------------------------------------
C             VISCOSITE TURBULENTE
                DO I=1,NEL
                  IF((MLW == 6 .OR. MLW == 17).AND.JTURB/=0)THEN
                    IPRT=IPARTSP(N)
      		    MT  =IPART(1,IPRT)
                    VALUE(I)=PM(81,MT)*GBUF%RK(I)**2/
     .                     MAX(EM15,GBUF%RE(I))
                    IS_WRITTEN_VALUE(I) = 1  
                  ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                    VALUE(I) = MBUF%VAR(I)
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDIF
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'VORTX')THEN
C--------------------------------------------------
C             VORTICITE
                DO I=1,NEL
                  IF(MLW == 6 .OR. MLW == 17)THEN
                    VALUE(I) = LBUF%VK(I)
                    IS_WRITTEN_VALUE(I) = 1  
                  ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                    VALUE(I) = MBUF%VAR(NEL+I) 
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDIF
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DAM1' .AND.MLW == 24)THEN
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = LBUF%DAM(JJ(1) + I)
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DAM2' .AND.MLW == 24)THEN
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = LBUF%DAM(JJ(2) + I)
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DAM3' .AND.MLW == 24)THEN
C--------------------------------------------------
                DO I=1,NEL
                  N = I + NFT
                  VALUE(I) = LBUF%DAM(JJ(3) + I)
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGX')THEN
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = GBUF%SIG(JJ(1) + I)
                  IS_WRITTEN_VALUE(I) = 1 
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGY')THEN
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = GBUF%SIG(JJ(2) + I)
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGZ')THEN
C--------------------------------------------------
                DO I=1,NEL
                   VALUE(I) = GBUF%SIG(JJ(3) + I)
                   IS_WRITTEN_VALUE(I) = 1  
                ENDDO 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGXY')THEN
C--------------------------------------------------
                DO I=1,NEL
                   VALUE(I) = GBUF%SIG(JJ(4) + I)
                   IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGYZ')THEN
C--------------------------------------------------
                DO I=1,NEL
                   VALUE(I) = GBUF%SIG(JJ(5) + I)
                   IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'SIGZX')THEN
C--------------------------------------------------
                DO I=1,NEL
                   VALUE(I) = GBUF%SIG(JJ(6) + I)
                   IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'USER')THEN
C--------------------------------------------------
                IUS = IFUNC - 20				      
                NUVAR = IPM(8,MT)				    
                IF (NUVAR > 0) THEN
                  DO I=1,NEL
                   IF(IUS <= NUVAR)THEN
                     VALUE(I) = MBUF%VAR(I + IUS*NEL)
                     IS_WRITTEN_VALUE(I) = 1  
                   ENDIF
                  ENDDO
                ENDIF
C--------------------------------------------------
              ELSEIF(KEYWORD == 'HOURGLASS')THEN
C--------------------------------------------------
c
C--------------------------------------------------
              ELSEIF(KEYWORD == 'BFRAC')THEN
C--------------------------------------------------
                DO I=1,NEL
                  VALUE = ZERO
                  IF (MLW == 5)THEN
                    VALUE(I) = GBUF%BFRAC(I) 
                    IS_WRITTEN_VALUE(I) = 1  
                  ENDIF
                ENDDO 
C--------------------------------------------------        
              ELSEIF(KEYWORD == 'DAMA') THEN
C--------------------------------------------------
                 NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL  
                 DO IR=1,NFAIL  											  
                   DFMAX=>
     .               ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)%FLOC(IR)%DAMMX
                   DO I=1,NEL					
                       VALUE(I) = DFMAX(I)
                       IS_WRITTEN_VALUE(I) = 1  	
                   ENDDO
                 ENDDO 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'DOMAIN')THEN
C--------------------------------------------------
                DO I=1,NEL
                 VALUE(I)  = ISPMD
                 IS_WRITTEN_VALUE(I) = 1  
                ENDDO 
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'FILL')THEN
C--------------------------------------------------
                DO I=1,NEL
                  VALUE(I) = GBUF%FILL(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO  
C--------------------------------------------------
              ELSEIF (KEYWORD == 'SIGEQ') THEN 
C--------------------------------------------------
                IF (GBUF%G_SEQ > 0) THEN  !  non VON MISES
                  DO I=1,NEL
                    VALUE(I) = GBUF%SEQ(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ELSE			  ! VON MISES
                  DO I=1,NEL
                    P = -(GBUF%SIG(JJ(1) + I)		     
     .              	+ GBUF%SIG(JJ(2) + I)		     
     .              	+ GBUF%SIG(JJ(3) + I)) * THIRD
                    S1=GBUF%SIG(JJ(1) + I) + P  		
                    S2=GBUF%SIG(JJ(2) + I) + P  		
                    S3=GBUF%SIG(JJ(3) + I) + P  		
                    VONM2= THREE*(GBUF%SIG(JJ(4) + I)**2 +	
     .              		  GBUF%SIG(JJ(5) + I)**2 +	
     .              		  GBUF%SIG(JJ(6) + I)**2 +	
     .              	    HALF*(S1*S1+S2*S2+S3*S3)) 	  
                    VONM= SQRT(VONM2) 
                    VALUE(I) = VONM
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF
C-------------------------------------------------- 
              ELSEIF (KEYWORD == 'TDET') THEN  !  /H3D/ELEM/TDET
C-------------------------------------------------- 
                 IF (MLW  /= 51 .AND. GBUF%G_TB > 0) THEN
                   DO I=1,NEL
                     VALUE(I) = -GBUF%TB(I)
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO
                 ELSEIF (MLW == 51)THEN
                   IPOS      = 15
                   ITRIMAT   = 4     
                   K         = IPARG(2,NG) * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                             
                   DO I=1,IPARG(2,NG)
                     VALUE(I) = -MBUF%VAR(K+I)
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO 
                 ENDIF 
C--------------------------------------------------
              ELSEIF(KEYWORD == 'GROUP')THEN 
C-------------------------------------------------- 
                DO I=1,NEL
                  VALUE(I) = NG
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'INTERNAL.ID')THEN 
C-------------------------------------------------- 
                DO I=1,NEL
                  VALUE(I) = I+NFT
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
C--------------------------------------------------
              ELSEIF(KEYWORD == 'LOCAL.ID')THEN 
C-------------------------------------------------- 
                DO I=1,NEL
                  VALUE(I) = I
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO          
C--------------------------------------------------
              ELSEIF(KEYWORD == 'OFF')THEN  
C--------------------------------------------------                     
                DO I=1,NEL
                  IF (GBUF%G_OFF > 0) THEN
                    IF(GBUF%OFF(I) > ONE) THEN
                      VALUE(I) = GBUF%OFF(I) - ONE
                    ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                      VALUE(I) = GBUF%OFF(I)
                    ELSE
                      VALUE(I) = -ONE
                    ENDIF
                  ENDIF
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF   ! IFUNC
C-------------------------------------------------- 
              CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_SPH,SPH_SCALAR,NEL,OFFSET,NFT,
     .   			  VALUE,IS_WRITTEN_VALUE)
            ENDIF
          ENDIF
       ENDIF

 900  CONTINUE   ! NG 
C-----------------------------------------------
      RETURN
      END
