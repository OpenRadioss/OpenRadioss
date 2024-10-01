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
      !||====================================================================
      !||    h3d_sph_scalar         ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||--- called by ------------------------------------------------------
      !||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_write_scalar       ../engine/source/output/h3d/h3d_results/h3d_write_scalar.F
      !||    initbuf                ../engine/share/resol/initbuf.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    initbuf_mod            ../engine/share/resol/initbuf.F
      !||    multimat_param_mod     ../common_source/modules/multimat_param_mod.F90
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||    schlieren_mod          ../engine/share/modules/schlieren_mod.F
      !||    stack_mod              ../engine/share/modules/stack_mod.F
      !||====================================================================
      SUBROUTINE H3D_SPH_SCALAR(
     .                  ELBUF_TAB ,SPH_SCALAR      ,IFUNC     ,IPARG       ,
     .                  KXSP      ,PM        ,IPART       ,
     .                  IPM       ,
     .                  ID_ELEM   ,IPARTSP   ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SPH,KEYWORD    ,SPBUF       ,ID          )

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
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
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SPH_SCALAR(*),
     .   PM(NPROPM,*)
      INTEGER IPARG(NPARG,*),KXSP(NISP,*),IFUNC,
     .   IPM(NPROPMI,*),
     .   ID_ELEM(*),IPARTSP(*),
     .   H3D_PART(*),IS_WRITTEN_SPH(*),IUVAR_INPUT,IPART(LIPART1,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER(NCHARLINE100) :: KEYWORD
      INTEGER, INTENT(IN) :: ID
      my_real :: SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   VALUE(MVSIZ),MASS(MVSIZ),PRES(MVSIZ)
      my_real
     .   P,VONM2,S1,S2,S3,
     .   S11,S22,S33,S4,S5,S6,VONM,
     .   MASS0,VOL
      INTEGER I,NG,NEL,NPTR,NPTS,NPTT,NLAY,
     .        IR,MLW, NUVAR,IUS,NFAIL,
     .        N,K,JTURB,MT,
     .        OFFSET, 
     .        IUVAR,IPRT,
     .        IEOS
      INTEGER            
     .        NPTG,
     .        IOK_PART(MVSIZ),JJ(6),
     .        IS_WRITTEN_VALUE(MVSIZ),IPOS,ITRIMAT,NVAREOS
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_MAT_)  ,POINTER :: MBUF      
      TYPE(BUF_EOS_)  ,POINTER :: EBUF
      my_real, DIMENSION(:), POINTER  :: DFMAX
C-----------------------------------------------
      DO I=1,NUMSPH
         IS_WRITTEN_SPH(I) = 0
      ENDDO
c
      DO NG=1,NGROUP

        CALL INITBUF(IPARG    ,NG      ,                    
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
                 PRES(I) = -(GBUF%SIG(JJ(1)+I)+ GBUF%SIG(JJ(2)+I) + GBUF%SIG(JJ(3)+I))*THIRD
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
     .               HALF*(S1*S1+S2*S2+S3*S3) )
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
                 DO I = 1,NEL
                   VALUE(I) = ZERO
                 ENDDO
                 NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL  
                 DO IR=1,NFAIL                          
                   DFMAX=>
     .               ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)%FLOC(IR)%DAMMX
                   DO I=1,NEL
                     VALUE(I) = MAX(DFMAX(I),VALUE(I))
                     IS_WRITTEN_VALUE(I) = 1  
                   ENDDO
                 ENDDO 
C--------------------------------------------------        
              ELSEIF(KEYWORD == 'FAILURE') THEN
C--------------------------------------------------
                 DO I = 1,NEL
                   VALUE(I) = ZERO
                 ENDDO
                 NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL  
                 DO IR=1,NFAIL 
                   IF (ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)%FLOC(IR)%IDFAIL == ID) THEN
                     DFMAX=>
     .                ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)%FLOC(IR)%DAMMX
                     DO I=1,NEL
                       VALUE(I) = DFMAX(I)
                       IS_WRITTEN_VALUE(I) = 1  
                     ENDDO
                   ENDIF
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
                ELSE        ! VON MISES
                  DO I=1,NEL
                    P = -(GBUF%SIG(JJ(1) + I)         
     .                + GBUF%SIG(JJ(2) + I)         
     .                + GBUF%SIG(JJ(3) + I)) * THIRD
                    S1=GBUF%SIG(JJ(1) + I) + P  
                    S2=GBUF%SIG(JJ(2) + I) + P  
                    S3=GBUF%SIG(JJ(3) + I) + P  
                    VONM2= THREE*(GBUF%SIG(JJ(4) + I)**2 +
     .                    GBUF%SIG(JJ(5) + I)**2 +
     .                    GBUF%SIG(JJ(6) + I)**2 +
     .                    HALF*(S1*S1+S2*S2+S3*S3))     
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
                   K         = IPARG(2,NG) * ((M51_N0PHAS + (ITRIMAT-1)*M51_NVPHAS )+IPOS-1)                             
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
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'TILLOTSON') THEN  
C-------------------------------------------------- 
                 N = I + NFT
                 IPRT=IPARTSP(N)                                           
                 MT  =IPART(1,IPRT) 
                 IEOS = IPM(4,MT)                             
                 IF(IEOS == 3)THEN                            
                   EBUF => ELBUF_TAB(NG)%BUFLY(1)%EOS(1,1,1)   
                   NVAREOS = ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS  
                   DO  I=1,NEL                                
                     VALUE(I) = EBUF%VAR(I)                   
                     IS_WRITTEN_VALUE(I) = 1                  
                   ENDDO                                      
                 ENDIF                                                       
C-------------------------------------------------- 
              ENDIF   ! IFUNC
C-------------------------------------------------- 
              CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_SPH,SPH_SCALAR,NEL,OFFSET,NFT,
     .           VALUE,IS_WRITTEN_VALUE)
            ENDIF
          ENDIF
       ENDIF

      ENDDO  ! NG 
C-----------------------------------------------
      RETURN
      END
