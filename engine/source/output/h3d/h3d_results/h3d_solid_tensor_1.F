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
Chd|====================================================================
Chd|  H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR              source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_TENSOR              source/output/h3d/h3d_results/h3d_write_tensor.F
Chd|        H3D_WRITE_TENSOR_CORNER       source/output/h3d/h3d_results/h3d_write_tensor_corner.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        STRN_TENSCOR3                 source/output/h3d/h3d_results/h3d_strn_tenscor3.F
Chd|        STRS_TENSCOR3                 source/output/h3d/h3d_results/strs_tenscor3.F
Chd|        T4_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SOLID_TENSOR_1(
     .                  ELBUF_TAB,SOLID_TENSOR, IPARG   ,ITENS   ,IXS     ,PM       ,
     2                   EL2FA    ,NBF     ,TENS    ,EPSDOT  ,
     3                   NBPART   ,X       ,IADG    ,IPART   ,
     4                   IPARTSP  ,IPARTS  ,ISPH3D  ,IPM     ,IGEO    ,ID_ELEM   ,ITY_ELEM  ,
     5                   IS_WRITTEN_SOLID  ,LAYER_INPUT ,IR_INPUT  ,IS_INPUT  ,IT_INPUT,
     6                   H3D_PART,INFO1   ,KEYWORD, NG  ,D      ,SOLID_TENSOR_CORNER,
     7                   IS_CORNER_DATA   ,IXS10  ,MAXNNOD      ,ID  )
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
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
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SOLID_TENSOR(6,*), TENS(6,*),EPSDOT(6,*),PM(NPROPM,*),X(3,*),D(3,*),
     .   SOLID_TENSOR_CORNER(6,*)
      INTEGER IPARG(NPARG,*),ITENS, MAXNNOD,
     .   IXS(NIXS,*),EL2FA(*),IADG(NSPMD,*),IPM(NPROPMI,*),IXS10(6,*),
     .   NBF,NBPART,IPART(LIPART1,*),IPARTSP(*),IPARTS(*),
     .   ISPH3D,IGEO(NPROPGI,*),IS_WRITTEN_SOLID(*),ID_ELEM(*),ITY_ELEM(*),
     .   H3D_PART(*),INFO1,LAYER_INPUT,IR_INPUT,IS_INPUT,IT_INPUT,NG,IS_CORNER_DATA,ID  
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER(NCHARLINE100) :: KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      my_real
     .   EVAR(6,MVSIZ),EVAR_CORNER(6,20,MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S12,S3,VALUE,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),S11,S22,S33,S4,S5,S6,VONM, GAMA(6),EVAR_TMP(6),
     .   A1
      my_real 
     .   XN(8*MVSIZ) , YN(8*MVSIZ) , ZN(8*MVSIZ), 
     .   DXN(8*MVSIZ) ,DYN(8*MVSIZ) , DZN(8*MVSIZ),STRAIN(6,MVSIZ) 
      INTEGER I,I1,II,J,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,
     .        ID_PLY,NB_PLYOFF
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,
     .        ISTRAIN,KCVT,IOR_TSH,MT1,ICSIG,PTI,IOK,IPRT,IOK_PART(MVSIZ),
     .        JJ(6),IS_WRITTEN_TENSOR(MVSIZ),KK(8)

      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
        GBUF => ELBUF_TAB(NG)%GBUF
        ISTRAIN = IPARG(44,NG)
        ISOLNOD = IPARG(28,NG)
        IVISC = IPARG(61,NG)
        CALL INITBUF(IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
       DO I=1,6
         JJ(I) = NEL*(I-1)
       ENDDO
!
       IF(MLW /= 13 .AND. MLW /= 0) THEN          
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1) THEN


          DO  I=1,NEL 
            ID_ELEM(NFT+I) = IXS(NIXS,NFT+I)
            ITY_ELEM(NFT+I) = 1
            IOK_PART(I) = 0
            IF( H3D_PART(IPARTS(NFT+I)) == 1) IOK_PART(I) = 1
            IS_WRITTEN_TENSOR(I) = 0
          ENDDO

          TSHELL  = 0
          IOR_TSH = 0
          IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1
          IF (IGTYP == 21.OR.IGTYP == 22) IOR_TSH = 1
          NLAY = ELBUF_TAB(NG)%NLAY                
          NPTR = ELBUF_TAB(NG)%NPTR                 
          NPTS = ELBUF_TAB(NG)%NPTS                 
          NPTT = ELBUF_TAB(NG)%NPTT
          NPTG = NPTT*NPTS*NPTR
          NPT  = NPTG*NLAY
          PID=IXS(10,1 + NFT)
          MT1=IXS(1,1 + NFT)
C
          IF (KCVT==1.AND.ISORTH/=0) KCVT=2
          NUVAR = IPM(8,MT1)
          IF (IGTYP /= 22) THEN
            IF (ISORTH > 0) ISORTHG = 0
          END IF  
            ILAY = LAYER_INPUT
            ILAY = -1          
            IR = IR_INPUT
            IS = IS_INPUT
            IT = IT_INPUT
            IF (ILAY == -2) ILAY = 1
            IF (ILAY == -3) ILAY = NLAY
C-------- LAYER_INPUT not available yet fix in function of JHBE+IJK
          IF (TSHELL == 1.AND.(IR_INPUT/=-1.AND.IS_INPUT/=-1.AND.IT_INPUT/=-1)) THEN
           IF (JHBE==15 ) THEN
            ILAY = IS_INPUT
            IR = 1
            IS = 1
            IT = 1
           ELSEIF (JHBE==14 ) THEN
             ICSIG = IPARG(17,NG)
             IF (ICSIG==100) THEN
               IR = IS_INPUT
               IS = IT_INPUT
               ILAY = IR_INPUT
             ELSEIF (ICSIG==10) THEN
               ILAY = IS_INPUT
               IR = IT_INPUT
               IS = IR_INPUT
             ELSEIF (ICSIG==1) THEN
               ILAY = IT_INPUT
             END IF
            IT = 1
           ELSE
             ILAY = IS_INPUT
             IS = 1
           END IF
          END IF
C-----------------------------------------------
          IF (KEYWORD == 'TENS/STRESS') THEN
C-----------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
            IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
             DO I=1,NEL
               II = 6*(I-1)
               EVAR(1,I) = GBUF%SIG(JJ(1) + I)
               EVAR(2,I) = GBUF%SIG(JJ(2) + I)
               EVAR(3,I) = GBUF%SIG(JJ(3) + I)
               EVAR(4,I) = GBUF%SIG(JJ(4) + I)
               EVAR(5,I) = GBUF%SIG(JJ(5) + I)
               EVAR(6,I) = GBUF%SIG(JJ(6) + I)
               IS_WRITTEN_TENSOR(I) = 1
             ENDDO
             IF(IVISC > 0) THEN
              DO I=1,NEL
                DO IR=1,NPTR                    
                  DO IS=1,NPTS            
                    DO IT=1,NPTT           
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                      EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)/NPTG
                      EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)/NPTG
                      EVAR(3,I) =EVAR(3,I)+ LBUF%VISC(JJ(3) + I)/NPTG
                      EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)/NPTG
                      EVAR(5,I) =EVAR(5,I)+ LBUF%VISC(JJ(5) + I)/NPTG
                      EVAR(6,I) =EVAR(6,I)+ LBUF%VISC(JJ(6) + I)/NPTG
                     ENDDO !IT                   
                  ENDDO !IS                
                ENDDO !IR              
              ENDDO
             ENDIF
c
             IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
               DO I=1,NEL
           EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
           EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
           EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
           EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
           EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
           EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
               ENDDO
             ENDIF
c
             IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C             STRESS TENSOR IN GLOBAL SYSTEM
              DO I=1,NEL
               N = I + NFT
C            pour JHBE=14, valeurs moyennes est dans rep. corota.
          IF(KCVT==2.AND.JHBE/=14)THEN
            II = 6*(I-1)
            GAMA(1)=GBUF%GAMA(JJ(1) + I)
            GAMA(2)=GBUF%GAMA(JJ(2) + I)
            GAMA(3)=GBUF%GAMA(JJ(3) + I)
            GAMA(4)=GBUF%GAMA(JJ(4) + I)
            GAMA(5)=GBUF%GAMA(JJ(5) + I)
            GAMA(6)=GBUF%GAMA(JJ(6) + I)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
            CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
              ENDDO
             ENDIF !KCVT /= 
            ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
c IR= IS= IT=
c-----------
              IF (ISOLNOD ==  8 .AND. IGTYP == 43) THEN
c-----------
                DO I=1,NEL                
                  EVAR(1,I) = ZERO
                  EVAR(2,I) = ZERO
                  EVAR(3,I) = ZERO
                  EVAR(4,I) = ZERO
                  EVAR(5,I) = ZERO
                  EVAR(6,I) = ZERO
                ENDDO
                IF(IVISC == 0) THEN
                 DO I=1,NEL             
                   II = 6*(I-1)
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,1,1)
                   EVAR(3,I) = EVAR(3,I) + LBUF%SIG(JJ(3) + I)
                   EVAR(2,I) = EVAR(2,I) + LBUF%SIG(JJ(5) + I)
                   EVAR(1,I) = EVAR(1,I) + LBUF%SIG(JJ(6) + I)
                   IS_WRITTEN_TENSOR(I) = 1          
                 ENDDO  
                ELSE
                 DO I=1,NEL           
                   II = 6*(I-1)
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,1,1) 
                   EVAR(3,I)= EVAR(3,I)+ LBUF%SIG(JJ(3) + I)+ LBUF%VISC(JJ(3) + I)
                   EVAR(2,I)= EVAR(2,I)+ LBUF%SIG(JJ(5) + I)+ LBUF%VISC(JJ(5) + I)
                   EVAR(1,I)= EVAR(1,I)+ LBUF%SIG(JJ(6) + I)+ LBUF%VISC(JJ(6) + I)
                   IS_WRITTEN_TENSOR(I) = 1
                 ENDDO 
                ENDIF                   
                DO I=1,NEL        
                  N = I + NFT          
                 CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO              
c-----------
              ELSEIF (ISOLNOD == 8.AND.NPT == 8.AND.
     .          JHBE /= 14.AND.JHBE /= 24.AND.JHBE /= 15) THEN
c-----------
                IF (IR == 0 .AND. IT == 0)THEN
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ELSE
                  IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR   
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)    
                  IF (IPT <= 8 )THEN       
                    DO I=1,NEL
                      II = 6*(I-1)              
                      EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                      EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                      EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                      EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                      EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                      EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                    IF(IVISC > 0) THEN
                      DO I=1,NEL
          II = 6*(I-1)       
          EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
          EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
          EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
          EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
          EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
          EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                      ENDDO
                    ENDIF 
                     
                  ELSE
                    DO I=1,NEL
                      EVAR(1,I) = ZERO
                      EVAR(2,I) = ZERO
                      EVAR(3,I) = ZERO
                      EVAR(4,I) = ZERO
                      EVAR(5,I) = ZERO
                      EVAR(6,I) = ZERO
                    ENDDO
                  ENDIF
                  IF (KCVT /= 0) THEN
C                   STRESS TENSOR IN GLOBAL SYSTEM
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)= GBUF%GAMA(JJ(1) + I)
            GAMA(2)= GBUF%GAMA(JJ(2) + I)
            GAMA(3)= GBUF%GAMA(JJ(3) + I)
            GAMA(4)= GBUF%GAMA(JJ(4) + I)
            GAMA(5)= GBUF%GAMA(JJ(5) + I)
            GAMA(6)= GBUF%GAMA(JJ(6) + I)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                  ENDIF
                ENDIF
c-----------
              ELSEIF((ISOLNOD == 8.OR.NPT == 1) .AND.
     .              JHBE /= 14.AND.JHBE /= 15.AND.JHBE /= 17)THEN
c-----------
                NPTR= ONE
                NPTS= ONE
                NPTT= ONE
                IF (IR == 0 .AND. IT == 0)THEN
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ELSE
                  IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                  IF (IPT == 1 )THEN       
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)    
                    DO I=1,NEL
                      II = 6*(I-1)            
                      EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                      EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                      EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                      EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                      EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                      EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                    IF(IVISC > 0) THEN
                      DO I=1,NEL
          II = 6*(I-1)       
          EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
          EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
          EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
          EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
          EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
          EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                      ENDDO
                    ENDIF
                  ELSE
                    DO I=1,NEL
                      EVAR(1,I) = ZERO
                      EVAR(2,I) = ZERO
                      EVAR(3,I) = ZERO
                      EVAR(4,I) = ZERO
                      EVAR(5,I) = ZERO
                      EVAR(6,I) = ZERO
                    ENDDO
                  ENDIF
                  IF (KCVT /= 0) THEN
C                   STRESS TENSOR IN GLOBAL SYSTEM
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=GBUF%GAMA(JJ(1) + I)
            GAMA(2)=GBUF%GAMA(JJ(2) + I)
            GAMA(3)=GBUF%GAMA(JJ(3) + I)
            GAMA(4)=GBUF%GAMA(JJ(4) + I)
            GAMA(5)=GBUF%GAMA(JJ(5) + I)
            GAMA(6)=GBUF%GAMA(JJ(6) + I)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                  ENDIF
                ENDIF
c-----------
              ELSEIF (ISOLNOD == 20 .OR. ISOLNOD == 16) THEN 
c-----------
                IF (IR == 0 .AND. IT == 0)THEN
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ELSE
                  IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                  IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS
     .                .AND. IT <= NPTT) THEN
                    IF (TSHELL == 1) THEN
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
                    ELSE                       
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    END IF                       
                    DO I=1,NEL 
                      II = 6*(I-1)            
                      EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                      EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                      EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                      EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                      EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                      EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                     IF(IVISC > 0) THEN
                      DO I=1,NEL
          II = 6*(I-1)       
          EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
          EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
          EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
          EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
          EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
          EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                      ENDDO
                    ENDIF
                  ELSE  
                    DO I=1,NEL
                      EVAR(1,I) = ZERO
                      EVAR(2,I) = ZERO
                      EVAR(3,I) = ZERO
                      EVAR(4,I) = ZERO
                      EVAR(5,I) = ZERO
                      EVAR(6,I) = ZERO
                    ENDDO
                  ENDIF 
                  IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C                   STRESS TENSOR IN GLOBAL SYSTEM
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=GBUF%GAMA(JJ(1) + I)
            GAMA(2)=GBUF%GAMA(JJ(2) + I)
            GAMA(3)=GBUF%GAMA(JJ(3) + I)
            GAMA(4)=GBUF%GAMA(JJ(4) + I)
            GAMA(5)=GBUF%GAMA(JJ(5) + I)
            GAMA(6)=GBUF%GAMA(JJ(6) + I)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                  ENDIF
                ENDIF
c-----------
              ELSEIF (ISOLNOD == 8 .AND. (JHBE == 14.OR.JHBE == 17) )THEN
c-----------    voir !!!
                ICSIG = IPARG(17,NG)
                NPTG = NPTR * NPTS * NPTT * NLAY
                IPID = IXS(10,1 + NFT)
                IF (IR == 0 .AND. IT == 0)THEN
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ELSE 
            IF (IOR_TSH >0) THEN
                    IF (ICSIG == 10) THEN
                IR=IT_INPUT
                IS=IR_INPUT
                IT=IS_INPUT
                    ELSEIF (ICSIG == 1) THEN
                      IR=IS_INPUT
                      IS=IT_INPUT
                      IT=IR_INPUT
              ELSE
                IR=IR_INPUT
                IS=IS_INPUT
                IT=IT_INPUT
              ENDIF
            ENDIF
                    IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                    IOK = 0
                    IF(IR <= NPTR .AND. IS <= NPTS .AND. IT <= NPTT) THEN
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                      IOK = 1
                    ENDIF    
                    IF ( IPT <= NPTG .AND. IOK == 1) THEN
                      DO I=1,NEL 
          II = 6*(I-1)              
          EVAR(1,I) = LBUF%SIG(JJ(1) + I)
          EVAR(2,I) = LBUF%SIG(JJ(2) + I)
          EVAR(3,I) = LBUF%SIG(JJ(3) + I)
          EVAR(4,I) = LBUF%SIG(JJ(4) + I)
          EVAR(5,I) = LBUF%SIG(JJ(5) + I)
          EVAR(6,I) = LBUF%SIG(JJ(6) + I)
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                       IF(IVISC > 0) THEN
                      DO I=1,NEL
          II = 6*(I-1)       
          EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
          EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
          EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
          EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
          EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
          EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                      ENDDO
                    ENDIF
                  ELSE  
                      DO I=1,NEL
          EVAR(1,I) = ZERO
          EVAR(2,I) = ZERO
          EVAR(3,I) = ZERO
          EVAR(4,I) = ZERO
          EVAR(5,I) = ZERO
          EVAR(6,I) = ZERO
                      ENDDO
                    ENDIF
                    IF (KCVT /= 0) THEN
C               STRESS TENSOR IN GLOBAL SYSTEM
C--------------thick shells----only pid21,irep=0--works--------
                    IF (ICSIG >0) THEN
                    SELECT CASE (ICSIG)               
                    CASE (1)                  
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=ZERO
            GAMA(2)=LBUF%GAMA(JJ(1) + I)
            GAMA(3)=LBUF%GAMA(JJ(2) + I)
            GAMA(4)=ZERO
            GAMA(5)=-GAMA(2)
            GAMA(6)=GAMA(1)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                    CASE (10)         
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=LBUF%GAMA(JJ(1) + I)
            GAMA(2)=LBUF%GAMA(JJ(2) + I)
            GAMA(3)=ZERO
            GAMA(4)=-GAMA(2)
            GAMA(5)=GAMA(1)
            GAMA(6)=ZERO
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                    CASE (100)                    
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=LBUF%GAMA(JJ(2) + I)
            GAMA(2)=ZERO
            GAMA(3)=LBUF%GAMA(JJ(1) + I)
            GAMA(4)=GAMA(3)
            GAMA(5)=ZERO
            GAMA(6)=-GAMA(1)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                    END SELECT 
                 ELSE                 
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=GBUF%GAMA(JJ(1) + I)
            GAMA(2)=GBUF%GAMA(JJ(2) + I)
            GAMA(3)=GBUF%GAMA(JJ(3) + I)
            GAMA(4)=GBUF%GAMA(JJ(4) + I)
            GAMA(5)=GBUF%GAMA(JJ(5) + I)
            GAMA(6)=GBUF%GAMA(JJ(6) + I)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                  ENDIF !(ICSIG >0)
                 ENDIF
               ENDIF
c-----------
              ELSEIF(ISOLNOD == 10.OR.(ISOLNOD == 4 .AND. ISROT == 1))THEN
c-----------
                IF (IR == 0 .AND. IT == 0)THEN
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ELSE
                  IPT = 0
                  IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
                  IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
                  IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
                  IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
                  IF (IPT > 0) THEN
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
                    DO I=1,NEL
                      II = 6*(I-1)            
                      EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                      EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                      EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                      EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                      EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                      EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO 
                     IF(IVISC > 0) THEN
                      DO I=1,NEL
          II = 6*(I-1)       
          EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
          EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
          EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
          EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
          EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
          EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                      ENDDO
                    ENDIF
                  ELSE
                    DO I=1,NEL
                      EVAR(1,I) = ZERO
                      EVAR(2,I) = ZERO
                      EVAR(3,I) = ZERO
                      EVAR(4,I) = ZERO
                      EVAR(5,I) = ZERO
                      EVAR(6,I) = ZERO
                    ENDDO
                  ENDIF 
                  IF (KCVT /= 0) THEN
C                   STRESS TENSOR IN GLOBAL SYSTEM
                    DO I=1,NEL
                      N = I + NFT
          IF(KCVT==2)THEN
            II = 6*(I-1)                
            GAMA(1)=GBUF%GAMA(JJ(1) + I)
            GAMA(2)=GBUF%GAMA(JJ(2) + I)
            GAMA(3)=GBUF%GAMA(JJ(3) + I)
            GAMA(4)=GBUF%GAMA(JJ(4) + I)
            GAMA(5)=GBUF%GAMA(JJ(5) + I)
            GAMA(6)=GBUF%GAMA(JJ(6) + I)
          ELSE
            GAMA(1)=ONE
            GAMA(2)=ZERO
            GAMA(3)=ZERO
            GAMA(4)=ZERO
            GAMA(5)=ONE
            GAMA(6)=ZERO
          END IF
          CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF

c-----------
CiLAY = ir= is= 
c-----------
            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS) THEN
c-----------
              IF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
c-----------
                IPT = IS
                IF ( ILAY <= NPT) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1) 
                  DO I=1,NEL
                    EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                    EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                    EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                    EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                    EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                    EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO 
                  IF(IVISC > 0) THEN
                    DO I=1,NEL
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF
                ELSE
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ENDIF
                IF (KCVT /= 0 .AND. ILAY <= NPT) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)    
C                 STRESS TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL
                    N = I + NFT
                    IF(KCVT==2)THEN
                      II = 6*(I-1)            
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF
              ELSEIF (ISOLNOD == 16.OR.(ISOLNOD ==8.AND.(JHBE == 14.OR.JHBE == 17)))THEN
c-----------
                ICSIG = IPARG(17,NG)
                NPTG = NPTR * NPTS * NPTT * NLAY
                IPID = IXS(10,1 + NFT)
                IF (IR == 0 .OR. IS == 0 .OR. IT == 0)THEN
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ELSE
                  IF (TSHELL == 1 ) THEN
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                    IF (ISOLNOD == 16) LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,1,IT)
                    DO I=1,NEL 
                      EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                      EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                      EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                      EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                      EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                      EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                    IF(IVISC > 0) THEN
                      DO I=1,NEL
                        EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                        EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                        EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                        EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                        EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                        EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                      ENDDO
                    ENDIF
                    IF (KCVT /= 0) THEN
C               STRESS TENSOR IN GLOBAL SYSTEM
C--------------thick shells----only pid21,irep=0--works--------
                      IF (ICSIG >0) THEN
                        SELECT CASE (ICSIG)                
                        CASE (1)                  
                          DO I=1,NEL
                            N = I + NFT
                            IF(KCVT==2.AND.IGTYP == 22)THEN
                              GAMA(1)=ZERO
                              GAMA(2)=LBUF%GAMA(JJ(1) + I)
                              GAMA(3)=LBUF%GAMA(JJ(2) + I)
                              GAMA(4)=ZERO
                              GAMA(5)=-GAMA(2)
                              GAMA(6)=GAMA(1)
                            ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                              GAMA(1)=ZERO
                              GAMA(2)=GBUF%GAMA(JJ(1) + I)
                              GAMA(3)=GBUF%GAMA(JJ(2) + I)
                              GAMA(4)=ZERO
                              GAMA(5)=-GAMA(2)
                              GAMA(6)=GAMA(1)
                            ELSE
                              GAMA(1)=ONE
                              GAMA(2)=ZERO
                              GAMA(3)=ZERO
                              GAMA(4)=ZERO
                              GAMA(5)=ONE
                              GAMA(6)=ZERO
                            END IF
                            CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                          ENDDO
                        CASE (10)         
                          DO I=1,NEL
                            N = I + NFT
                            IF(KCVT==2.AND.IGTYP == 22)THEN
                GAMA(1)=LBUF%GAMA(JJ(1) + I)
                GAMA(2)=LBUF%GAMA(JJ(2) + I)
                GAMA(3)=ZERO
                GAMA(4)=-GAMA(2)
                GAMA(5)=GAMA(1)
                GAMA(6)=ZERO
                            ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                GAMA(1)=GBUF%GAMA(JJ(1) + I)
                GAMA(2)=GBUF%GAMA(JJ(2) + I)
                GAMA(3)=ZERO
                GAMA(4)=-GAMA(2)
                GAMA(5)=GAMA(1)
                GAMA(6)=ZERO
              ELSE
                GAMA(1)=ONE
                GAMA(2)=ZERO
                GAMA(3)=ZERO
                GAMA(4)=ZERO
                GAMA(5)=ONE
                GAMA(6)=ZERO
              END IF
              CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                          ENDDO
                        CASE (100)
                          DO I=1,NEL
                            N = I + NFT
                            IF(KCVT==2.AND.IGTYP == 22)THEN
                              GAMA(1)=LBUF%GAMA(JJ(2) + I)
                              GAMA(2)=ZERO
                              GAMA(3)=LBUF%GAMA(JJ(1) + I)
                              GAMA(4)=GAMA(3)
                              GAMA(5)=ZERO
                              GAMA(6)=-GAMA(1)
                            ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                              GAMA(1)=GBUF%GAMA(JJ(2) + I)
                              GAMA(2)=ZERO
                              GAMA(3)=GBUF%GAMA(JJ(1) + I)
                              GAMA(4)=GAMA(3)
                              GAMA(5)=ZERO
                              GAMA(6)=-GAMA(1)
                            ELSE
                              GAMA(1)=ONE
                              GAMA(2)=ZERO
                              GAMA(3)=ZERO
                              GAMA(4)=ZERO
                              GAMA(5)=ONE
                              GAMA(6)=ZERO
                            END IF
                            CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                          ENDDO
                        END SELECT 
                      ELSE                   
          DO I=1,NEL
            N = I + NFT
            IF(KCVT==2)THEN
              GAMA(1)=GBUF%GAMA(JJ(1) + I)
              GAMA(2)=GBUF%GAMA(JJ(2) + I)
              GAMA(3)=GBUF%GAMA(JJ(3) + I)
              GAMA(4)=GBUF%GAMA(JJ(4) + I)
              GAMA(5)=GBUF%GAMA(JJ(5) + I)
              GAMA(6)=GBUF%GAMA(JJ(6) + I)
            ELSE
              GAMA(1)=ONE
              GAMA(2)=ZERO
              GAMA(3)=ZERO
              GAMA(4)=ZERO
              GAMA(5)=ONE
              GAMA(6)=ZERO
            END IF
            CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
          ENDDO
                      ENDIF !(ICSIG >0)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF

c-----------
            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY) THEN 
c ILAY= IS= IT=
              EVAR(1:6,1:NEL) = ZERO
c-----------
              IF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
c-----------
                IPT = IS
                IF ( ILAY <= NPT) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1) 
                  DO I=1,NEL
                    II = 6*(I-1)
                    EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                    EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                    EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                    EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                    EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                    EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO 
                  IF(IVISC > 0) THEN
                    DO I=1,NEL
                      II = 6*(I-1)     
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF
                ELSE
                  DO I=1,NEL
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ENDIF
                IF (KCVT /= 0 .AND. ILAY <= NPT) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)    
C                 STRESS TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL
                    N = I + NFT
                    IF(KCVT==2)THEN
                      II = 6*(I-1)            
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
c
            IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
              DO I=1,NEL
                EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
                EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
                EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
                EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
                EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
                EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
              ENDDO
            ENDIF
C
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN') THEN
C-----------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
            IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
              DO I=1,NEL
          EVAR(1,I) = ZERO
          EVAR(2,I) = ZERO
          EVAR(3,I) = ZERO
          EVAR(4,I) = ZERO
          EVAR(5,I) = ZERO
          EVAR(6,I) = ZERO
              ENDDO
c-----------
              IF (ISOLNOD == 8 .AND. IGTYP == 43) THEN
c-----------
               DO I=1,NEL               
                 II = 3*(I-1)
                 DO IPT= 1,NPTR
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)             
                   EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(1) + I)/NPT
                   EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT
                   EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(3) + I)/NPT
                   IS_WRITTEN_TENSOR(I) = 1
                 ENDDO  
               ENDDO                   
               DO I=1,NEL       
                 N = I + NFT         
                 CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
               ENDDO             
c-----------
              ELSEIF (ISOLNOD == 8 .AND. NPT == 8 .AND. JHBE /= 14.AND.
     .          JHBE /= 24.AND.JHBE /= 15.AND.JHBE /= 17 )THEN
c-----------
                NVAUX =IPARG(18,NG)      
                IF (MLW>=28) THEN
                  DO I=1,NEL
                    II = 6*(I-1)
                    N = I + NFT
                    DO J=1,8
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,J)
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)*ONE_OVER_8
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)*ONE_OVER_8
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)*ONE_OVER_8
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*ONE_OVER_8
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*ONE_OVER_8
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*ONE_OVER_8
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ENDIF
c-----------
              ELSEIF ((ISOLNOD==8.OR.(ISOLNOD==4 .AND. (ISROT==0.OR.ISROT==3))).AND.
     .               NPT==1 .AND. JHBE /= 14 .AND. JHBE /= 15) THEN
c-----------
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)               
                IF (ISORTH > 0) ISORTHG = 1
c
                IF (MLW>=28.AND.MLW /= 49) THEN
                  DO I=1,NEL
                    N = I + NFT
                    II = 6*(I-1)
                    EVAR(1,I) =  LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) =  LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) =  LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) =  LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) =  LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) =  LBUF%STRA(JJ(6) + I)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                  IF (ISORTH > 0) KCVT = 2
                ELSEIF (MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
                    N = I + NFT            
                    II = 3*(I-1)
                    EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)
                    EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                  IF (ISORTH > 0) KCVT = 2
                ELSEIF (MLW == 24 .OR. MLW == 25)THEN
                  DO I=1,NEL
                    N = I + NFT            
                    II = 6*(I-1)
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                  IF (ISORTH > 0) KCVT = 2
                ELSEIF (ISTRAIN > 0) THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28.OR.
     .                MLW == 49) THEN 
                    DO I=1,NEL
                      N = I + NFT  
                      II = 6*(I-1)
                      EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDIF
                ENDIF
                IF (KCVT /= 0) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL
                    N = I + NFT
                    IF(KCVT==2)THEN
                      II = 6*(I-1)
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF
c-----------
              ELSEIF(ISOLNOD == 16.OR.ISOLNOD == 20 .OR.
     .            (ISOLNOD == 8.AND.(JHBE == 14.OR.JHBE == 17)))THEN
c-----------
                IF (MLW>=28.AND.MLW /= 49)THEN
                  DO I=1,NEL
                   N = I + NFT   
                   II = 6*(I-1)
                   DO IL=1,NLAY         
                    DO IS=1,NPTS         
                     DO IT=1,NPTT
                      DO IR=1,NPTR
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                       EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)/NPT
                       EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)/NPT
                       EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)/NPT
                       EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF/NPT
                       EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF/NPT
                       EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF/NPT
                       IS_WRITTEN_TENSOR(I) = 1
                      ENDDO 
                     ENDDO
                    ENDDO
                   ENDDO
                  ENDDO
                ELSEIF (MLW == 12 .OR. MLW == 14) THEN
                  DO I=1,NEL              
                   N = I + NFT                
                   II = 3*(I-1)               
                   DO IL=1,NLAY               
                    DO IS=1,NPTS              
                     DO IT=1,NPTT              
                      DO IR=1,NPTR              
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
          EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/NPT 
          EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT 
          EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/NPT   
          IS_WRITTEN_TENSOR(I) = 1      
                      ENDDO                
                     ENDDO                
                    ENDDO                
                   ENDDO                
                  ENDDO                 
                ELSEIF(MLW == 24 .OR. MLW == 25)THEN
                  DO I=1,NEL
                   N = I + NFT   
                   II = 6*(I-1)
                   DO IL=1,NLAY         
                    DO IS=1,NPTS         
                     DO IT=1,NPTT
                      DO IR=1,NPTR
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                       IF (ELBUF_TAB(NG)%BUFLY(IL)%L_STRA > 0) THEN
          EVAR_TMP(1) =  LBUF%STRA(JJ(1) + I)/NPT
          EVAR_TMP(2) =  LBUF%STRA(JJ(2) + I)/NPT
          EVAR_TMP(3) =  LBUF%STRA(JJ(3) + I)/NPT
          EVAR_TMP(4) =  LBUF%STRA(JJ(4) + I)*HALF/NPT
          EVAR_TMP(5) =  LBUF%STRA(JJ(5) + I)*HALF/NPT
          EVAR_TMP(6) =  LBUF%STRA(JJ(6) + I)*HALF/NPT
              ICSIG=IPARG(17,NG)  
              IF (KCVT /= 0 .AND.ICSIG > 0) THEN
C                    STRAIN TENSOR IN GLOBAL SYSTEM
                IF (JHBE == 14) THEN
                  SELECT CASE (ICSIG)              
                  CASE (1)     
                    IF(KCVT==2 .AND. IGTYP ==22)THEN
                      GAMA(1)= ZERO
                      GAMA(2)= LBUF%GAMA(JJ(1) + I)
                      GAMA(3)= LBUF%GAMA(JJ(2) + I)
                      GAMA(4)= ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)= GAMA(1)
                    ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                      GAMA(1)= ZERO
                      GAMA(2)= GBUF%GAMA(JJ(1) + I)
                      GAMA(3)= GBUF%GAMA(JJ(2) + I)
                      GAMA(4)= ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)= GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  CASE (10)      
                    IF(KCVT==2 .AND. IGTYP ==22)THEN
                      GAMA(1)= LBUF%GAMA(JJ(1) + I)
                      GAMA(2)= LBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  CASE (100)     
                    IF(KCVT==2 .AND. IGTYP ==22)THEN
                      GAMA(1)= LBUF%GAMA(JJ(2) + I)
                      GAMA(2)= ZERO
                      GAMA(3)= LBUF%GAMA(JJ(1) + I)
                      GAMA(4)= GAMA(3)
                      GAMA(5)= ZERO
                      GAMA(6)=-GAMA(1)
                    ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                      GAMA(1)= GBUF%GAMA(JJ(2) + I)
                      GAMA(2)= ZERO
                      GAMA(3)= GBUF%GAMA(JJ(1) + I)
                      GAMA(4)= GAMA(3)
                      GAMA(5)= ZERO
                      GAMA(6)=-GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  END SELECT                 
                ENDIF
              ENDIF      
          EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
          EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
          EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
          EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
          EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
          EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6)
          IS_WRITTEN_TENSOR(I) = 1
                       ENDIF
                      ENDDO 
                     ENDDO
                    ENDDO
                   ENDDO
                  ENDDO
                ELSEIF(ISTRAIN > 0)THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN         
                    DO I=1,NEL
                     N = I + NFT   
                     II = 6*(I-1)
                     DO IL=1,NLAY          
                      DO IS=1,NPTS           
                       DO IT=1,NPTT
          DO IR=1,NPTR        
           LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT) 
           EVAR_TMP(1) = LBUF%STRA(JJ(1) + I)/NPT
           EVAR_TMP(2) = LBUF%STRA(JJ(2) + I)/NPT
           EVAR_TMP(3) = LBUF%STRA(JJ(3) + I)/NPT
           EVAR_TMP(4) = LBUF%STRA(JJ(4) + I)*HALF/NPT
           EVAR_TMP(5) = LBUF%STRA(JJ(5) + I)*HALF/NPT
           EVAR_TMP(6) = LBUF%STRA(JJ(6) + I)*HALF/NPT
c
               ICSIG=IPARG(17,NG)  
               IF (KCVT /= 0 .AND.ICSIG > 0) THEN
C                     STRAIN TENSOR IN GLOBAL SYSTEM
                 IF (JHBE == 14) THEN
                   SELECT CASE (ICSIG)              
                   CASE (1)      
                               IF(KCVT==2.AND.IGTYP == 22)THEN
                       GAMA(1)= ZERO
                       GAMA(2)= LBUF%GAMA(JJ(1) + I)
                       GAMA(3)= LBUF%GAMA(JJ(2) + I)
                       GAMA(4)= ZERO
                       GAMA(5)=-GAMA(2)
                       GAMA(6)= GAMA(1)
                               ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                       GAMA(1)= ZERO
                       GAMA(2)= GBUF%GAMA(JJ(1) + I)
                       GAMA(3)= GBUF%GAMA(JJ(2) + I)
                       GAMA(4)= ZERO
                       GAMA(5)=-GAMA(2)
                       GAMA(6)= GAMA(1)
                     ELSE
                       GAMA(1)=ONE
                       GAMA(2)=ZERO
                       GAMA(3)=ZERO
                       GAMA(4)=ZERO
                       GAMA(5)=ONE
                       GAMA(6)=ZERO
                     END IF
                     CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   CASE (10)       
                               IF(KCVT==2.AND.IGTYP == 22)THEN
                       GAMA(1)= LBUF%GAMA(JJ(1) + I)
                       GAMA(2)= LBUF%GAMA(JJ(2) + I)
                       GAMA(3)= ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)= GAMA(1)
                       GAMA(6)= ZERO
                               ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                       GAMA(1)= GBUF%GAMA(JJ(1) + I)
                       GAMA(2)= GBUF%GAMA(JJ(2) + I)
                       GAMA(3)= ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)= GAMA(1)
                       GAMA(6)= ZERO
                     ELSE
                       GAMA(1)=ONE
                       GAMA(2)=ZERO
                       GAMA(3)=ZERO
                       GAMA(4)=ZERO
                       GAMA(5)=ONE
                       GAMA(6)=ZERO
                     END IF
                     CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   CASE (100)     
                                IF(KCVT==2.AND.IGTYP == 22)THEN
                  GAMA(1)= LBUF%GAMA(JJ(2) + I)
                  GAMA(2)= ZERO
                  GAMA(3)= LBUF%GAMA(JJ(1) + I)
                  GAMA(4)= GAMA(3)
                  GAMA(5)= ZERO
                  GAMA(6)=-GAMA(1)
                                ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                  GAMA(1)= GBUF%GAMA(JJ(2) + I)
                  GAMA(2)= ZERO
                  GAMA(3)= GBUF%GAMA(JJ(1) + I)
                  GAMA(4)= GAMA(3)
                  GAMA(5)= ZERO
                  GAMA(6)=-GAMA(1)
                ELSE
                  GAMA(1)=ONE
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=ONE
                  GAMA(6)=ZERO
                END IF
                CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   END SELECT 
                 ENDIF
               ENDIF       
           EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
           EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
           EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
           EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
           EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
           EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6)
           IS_WRITTEN_TENSOR(I) = 1
          ENDDO 
                       ENDDO
                      ENDDO
                     ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c---
                ICSIG=IPARG(17,NG) 
                IF (JHBE == 17) THEN 
                  IF (MLW == 12 .OR. MLW == 14 .OR. MLW == 24 .OR.  
     .                MLW == 25 .OR.(MLW >= 28 .AND.MLW /= 49)) THEN 
                    IF (ISORTH > 0) KCVT = 2
                  ENDIF
                ENDIF
                IF (KCVT /= 0 .AND.ICSIG == 0 .AND. JHBE /= 16) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL               
                    N = I + NFT              
                    IF(KCVT==2)THEN           
                      II = 6*(I-1)           
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE             
                      GAMA(1)=ONE           
                      GAMA(2)=ZERO           
                      GAMA(3)=ZERO           
                      GAMA(4)=ZERO           
                      GAMA(5)=ONE           
                      GAMA(6)=ZERO           
                    END IF             
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO 
                ENDIF
c-----------
              ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
                IF (MLW>=28.AND.MLW /= 49)THEN 
                  DO I=1,NEL
                    N = I + NFT  
                    II = 6*(I-1)         
                    DO IPT=1,NPT
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
                    N = I + NFT  
                    II = 3*(I-1)         
                    DO IPT=1,NPT
           LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
           EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/NPT
           EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT
           EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/NPT 
           IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO    
                ELSEIF ((MLW == 24 .OR. MLW == 25) .and. ISTRAIN > 0) THEN
                  DO I=1,NEL
                    N = I + NFT  
                    II = 6*(I-1)         
                    DO IPT=1,NPT
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ELSEIF(ISTRAIN > 0)THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN
                    DO I=1,NEL
                      N = I + NFT  
                      II = 6*(I-1)           
                      DO IPT=1,NPT
          LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)       
          EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
          EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
          EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
          EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
          EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
          EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                    ENDDO        
                  ENDIF 
                ENDIF 
                IF (KCVT /= 0) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL          
                    N = I + NFT             
                    IF (KCVT==2) THEN          
                      II = 6*(I-1)          
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE          
                      GAMA(1)=ONE        
                      GAMA(2)=ZERO        
                      GAMA(3)=ZERO        
                      GAMA(4)=ZERO        
                      GAMA(5)=ONE        
                      GAMA(6)=ZERO        
                    ENDIF         
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO             
                ENDIF
c-----------
              ELSEIF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
c-----------
                IF (MLW>=28.AND.MLW /= 49.AND.ISTRAIN > 0) THEN
                  DO I=1,NEL               
                    N = I + NFT                
                    II = 6*(I-1)               
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG                 
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)  
          EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)       
          EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)       
          EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)       
          EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                      HALF/(NPTG*NLAY)       
          EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                      HALF/(NPTG*NLAY)       
          EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                      HALF/(NPTG*NLAY) 
          IS_WRITTEN_TENSOR(I) = 1  
                      ENDDO                   
                    ENDDO                 
                  ENDDO                  
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
                    II = 3*(I-1)         
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)
          EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/(NPTG*NLAY)
          EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/(NPTG*NLAY)
          EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/(NPTG*NLAY)
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                    ENDDO
                  ENDDO    
                ELSEIF ((MLW == 24 .OR. MLW == 25) .and. ISTRAIN > 0)THEN
                  DO I=1,NEL               
                    N = I + NFT                
                    II = 6*(I-1)               
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG                 
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)  
          EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)
          EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)
          EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)
          EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                  HALF/(NPTG*NLAY)      
          EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                  HALF/(NPTG*NLAY) 
          EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                  HALF/(NPTG*NLAY)   
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO                   
                    ENDDO                 
                  ENDDO                  
                ELSEIF (ISTRAIN > 0) THEN           
                  IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN     
                    DO I=1,NEL                 
                      N = I + NFT                 
                      II = 6*(I-1)                 
                      DO IL= 1,NLAY
          DO IPT=1,NPTG             
           LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1) 
            EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)       
            EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)      
            EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)    
            EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                  HALF/(NPTG*NLAY)
            EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                  HALF/(NPTG*NLAY)
            EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                  HALF/(NPTG*NLAY)
            IS_WRITTEN_TENSOR(I) = 1
          ENDDO               
                      ENDDO                   
                    ENDDO                 
                  ENDIF                    
                ENDIF             
                IF (KCVT /= 0) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL            
                    N = I + NFT         
                    IF (KCVT==2)THEN           
                      II = 6*(I-1)                
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)  
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)  
                      GAMA(3)= ZERO           
                      GAMA(4)=-GAMA(2)           
                      GAMA(5)= GAMA(1)           
                      GAMA(6)= ZERO           
                    ELSE             
                      GAMA(1)=ONE           
                      GAMA(2)=ZERO           
                      GAMA(3)=ZERO           
                      GAMA(4)=ZERO           
                      GAMA(5)=ONE           
                      GAMA(6)=ZERO           
                    END IF             
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO 
                ENDIF
c-----------
              ENDIF  ! ISOLNOD & ......

            ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
c IR= IS= IT=

C-----------------------------------------------
            DO I=1,NEL
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO
            ENDDO
c-----------
            IF (ISOLNOD == 8.AND.NPT == 8 .AND.
     .     JHBE /= 14.AND.JHBE /= 24.AND.JHBE /= 15.AND.JHBE /= 17) THEN
c-----------
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR 
              IF (IPT <= 8) THEN         
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)          
                IF (MLW >= 28) THEN
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)   
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)   
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)   
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)   
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)   
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I) 
                    IS_WRITTEN_TENSOR(I) = 1  
                  ENDDO
                ENDIF
              ENDIF
              IF (KCVT /= 0) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=1,NEL
            N = I + NFT
                  IF(KCVT==2)THEN
                    GAMA(1)=GBUF%GAMA(JJ(1) + I)
                    GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=GBUF%GAMA(JJ(3) + I)
                    GAMA(4)=GBUF%GAMA(JJ(4) + I)
                    GAMA(5)=GBUF%GAMA(JJ(5) + I)
                    GAMA(6)=GBUF%GAMA(JJ(6) + I)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              ENDIF
c-----------
            ELSEIF ((ISOLNOD == 8 .OR.NPT == 1 .OR.
     .              (ISOLNOD == 4 .AND. ISROT == 0)) .AND.
     .               JHBE /= 14.AND.JHBE /= 15.AND.JHBE /= 17) THEN 
c-----------
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              IF (IPT == 1 ) THEN          
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
                IF (MLW>=28.AND.MLW /= 49 .OR. MLW == 24) THEN 
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  END DO
                ELSEIF(MLW == 12 .OR. MLW == 14) THEN 
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)        
                    EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)        
                    EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I) 
                    IS_WRITTEN_TENSOR(I) = 1       
                  ENDDO  
                ELSEIF (ISTRAIN > 0)THEN 
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28.OR.
     .               MLW == 49) THEN                          
                    DO I=1,NEL 
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF  
C
              IF (KCVT /= 0) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=1,NEL
             N = I + NFT
                  IF(KCVT==2)THEN
                    GAMA(1)=GBUF%GAMA(JJ(1) + I)
                    GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=GBUF%GAMA(JJ(3) + I)
                    GAMA(4)=GBUF%GAMA(JJ(4) + I)
                    GAMA(5)=GBUF%GAMA(JJ(5) + I)
                    GAMA(6)=GBUF%GAMA(JJ(6) + I)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              ENDIF     
c-----------
           ELSEIF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.(ISOLNOD == 8.AND.
     .            (JHBE == 14 .OR. JHBE == 17))) THEN
c-----------
           ICSIG = IPARG(17,NG) 
           IF (IOR_TSH >0) THEN
               IF (ICSIG == 10) THEN
               IR=IT_INPUT
               IS=IR_INPUT
               IT=IS_INPUT
               ELSEIF (ICSIG == 1) THEN
                 IR=IS_INPUT
                 IS=IT_INPUT
                 IT=IR_INPUT
             ELSE
               IR=IR_INPUT
               IS=IS_INPUT
               IT=IT_INPUT
             ENDIF
           ENDIF
           IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
           IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS
     .            .AND. IT <= NPTT .AND. IR*IS*IT >= 1) THEN
             IF (TSHELL == 1) THEN                              
               IF (ISOLNOD == 16) THEN                              
                 LBUF =>  ELBUF_TAB(NG)%BUFLY(IS)%LBUF(IR,1,IT) 
               ELSE                 
                LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1) 
               END IF                
             ELSE                                               
               LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)   
             ENDIF                                          
             IF(MLW>=28.AND.MLW /= 49)THEN        
               DO I=1,NEL
C                3*9*3 points d'integration (r*s*t)    
                  EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                  EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                  EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                  EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                  EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                  EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                  IS_WRITTEN_TENSOR(I) = 1
               ENDDO
             ELSEIF(MLW == 12 .OR. MLW == 14)THEN
               DO I=1,NEL
C              3*9*3 points d'integration (r*s*t)       
                 EVAR(1,I) = LBUF%EPE(JJ(1) + I)   
                 EVAR(2,I) = LBUF%EPE(JJ(2) + I)   
                 EVAR(3,I) = LBUF%EPE(JJ(3) + I)   
                 IS_WRITTEN_TENSOR(I) = 1
               ENDDO       
             ELSEIF(MLW == 24 .OR. MLW == 25)THEN
               DO I=1,NEL
C                   3*9*3 points d'integration (r*s*t)         
                 EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                 EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                 EVAR(3,I) = LBUF%STRA(JJ(3) + I)        
                 EVAR(4,I) = LBUF%STRA(JJ(4) + I) * HALF
                 EVAR(5,I) = LBUF%STRA(JJ(5) + I) * HALF
                 EVAR(6,I) = LBUF%STRA(JJ(6) + I) * HALF
                 IS_WRITTEN_TENSOR(I) = 1 
               ENDDO   
             ELSEIF (MLW == 25) THEN
                 DO I=1,NEL 
                 EVAR(1,I) =  LBUF%STRA(JJ(1) + I)      
                 EVAR(2,I) =  LBUF%STRA(JJ(2) + I)      
                 EVAR(3,I) =  LBUF%STRA(JJ(3) + I)      
                 EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF    
                 EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF       
                 EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF
                 IS_WRITTEN_TENSOR(I) = 1    
                         ENDDO  
               ELSEIF(ISTRAIN > 0)THEN
                 IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN
                   DO I=1,NEL 
C  3*9*3 points d'integration (r*s*t)    
                    EVAR(1,I) =  LBUF%STRA(JJ(1) + I)          
                    EVAR(2,I) =  LBUF%STRA(JJ(2) + I)          
                    EVAR(3,I) =  LBUF%STRA(JJ(3) + I)          
                    EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF        
                    EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF          
                    EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF 
                    IS_WRITTEN_TENSOR(I) = 1       
                   ENDDO 
                 ENDIF
               ENDIF
C
               IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C              STRAIN TENSOR IN GLOBAL SYSTEM
            ICSIG=IPARG(17,NG)  
            IF (JHBE == 14.AND.ICSIG > 0) THEN
             SELECT CASE (ICSIG)               
             CASE (1)                 
             DO I=1,NEL
               N = I + NFT
                     IF(KCVT==2.AND.IGTYP == 22)THEN
                 GAMA(1)=ZERO
                         GAMA(2)=LBUF%GAMA(JJ(1) + I)
                       GAMA(3)=LBUF%GAMA(JJ(2) + I)
                 GAMA(4)=ZERO
                 GAMA(5)=-GAMA(2)
                 GAMA(6)=GAMA(1)
                     ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                 GAMA(1)=ZERO
                         GAMA(2)=GBUF%GAMA(JJ(1) + I)
                       GAMA(3)=GBUF%GAMA(JJ(2) + I)
                 GAMA(4)=ZERO
                 GAMA(5)=-GAMA(2)
                 GAMA(6)=GAMA(1)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
               CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
             CASE (10)                  
             DO I=1,NEL
               N = I + NFT
                     IF(KCVT==2.AND.IGTYP == 22)THEN
                       GAMA(1)=LBUF%GAMA(JJ(1) + I)
                       GAMA(2)=LBUF%GAMA(JJ(2) + I)
                 GAMA(3)=ZERO
                 GAMA(4)=-GAMA(2)
                 GAMA(5)=GAMA(1)
                 GAMA(6)=ZERO
                     ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                       GAMA(1)=GBUF%GAMA(JJ(1) + I)
                       GAMA(2)=GBUF%GAMA(JJ(2) + I)
                 GAMA(3)=ZERO
                 GAMA(4)=-GAMA(2)
                 GAMA(5)=GAMA(1)
                 GAMA(6)=ZERO
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
               CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
             CASE (100)                   
             DO I=1,NEL
               N = I + NFT
                     IF(KCVT==2.AND.IGTYP == 22)THEN
                       GAMA(1)=LBUF%GAMA(JJ(2) + I)
                 GAMA(2)=ZERO
                       GAMA(3)=LBUF%GAMA(JJ(1) + I)
                 GAMA(4)=GAMA(3)
                 GAMA(5)=ZERO
                 GAMA(6)=-GAMA(1)
                     ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                       GAMA(1)=GBUF%GAMA(JJ(2) + I)
                 GAMA(2)=ZERO
                       GAMA(3)=GBUF%GAMA(JJ(1) + I)
                 GAMA(4)=GAMA(3)
                 GAMA(5)=ZERO
                 GAMA(6)=-GAMA(1)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
               CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
             END SELECT 
            ELSE  
              DO I=1,NEL
                N = I + NFT
                IF(KCVT==2)THEN
                  GAMA(1)=GBUF%GAMA(JJ(1) + I)
                  GAMA(2)=GBUF%GAMA(JJ(2) + I)
                  GAMA(3)=GBUF%GAMA(JJ(3) + I)
                  GAMA(4)=GBUF%GAMA(JJ(4) + I)
                  GAMA(5)=GBUF%GAMA(JJ(5) + I)
                  GAMA(6)=GBUF%GAMA(JJ(6) + I)
                ELSE
                  GAMA(1)=ONE
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=ONE
                  GAMA(6)=ZERO
                END IF
                CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
              ENDDO
             ENDIF  !(JHBE == 14.AND.ICSIG > 0)   
               ENDIF 
             ENDIF

c-----------
            ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
              IPT = IR
c              IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
c              IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
c              IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
c              IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
              IF ( IPT > 0) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
                IF (MLW>=28.AND.MLW /= 49 .OR. MLW == 24) THEN 
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ELSEIF (MLW == 12 .OR. MLW == 14) THEN
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)       
                    EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)       
                    EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I) 
                    IS_WRITTEN_TENSOR(I) = 1      
                  ENDDO 
                ELSEIF (ISTRAIN > 0) THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN               
                    DO I=1,NEL
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF  
c
              IF (KCVT /= 0) THEN
                DO I=1,NEL
                  N = I + NFT
                  IF(KCVT==2)THEN
                    GAMA(1)=GBUF%GAMA(JJ(1) + I)
                    GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=GBUF%GAMA(JJ(3) + I)
                    GAMA(4)=GBUF%GAMA(JJ(4) + I)
                    GAMA(5)=GBUF%GAMA(JJ(5) + I)
                    GAMA(6)=GBUF%GAMA(JJ(6) + I)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              ENDIF  

            ENDIF                          
c-----------
            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY  .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS) THEN 

              EVAR(1:6,1:NEL) = ZERO
              IF ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15) THEN
c-----------
                IF ( ILAY <= NPT ) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1) 
                  IF (MLW>=28.AND.MLW /= 49 .OR. MLW == 24) THEN
                    DO I=1,NEL
                      EVAR(1,I) =  LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) =  LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) =  LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) =  LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) =  LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) =  LBUF%STRA(JJ(6) + I)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                    DO I=1,NEL
                      N = I + NFT 
                      EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I) 
                      IS_WRITTEN_TENSOR(I) = 1      
                    ENDDO 
                  ELSEIF (ISTRAIN > 0) THEN           
                    IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN        
                      DO I=1,NEL                
          EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
          EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
          EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
          EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
          EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
          EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                    ENDIF
                  ENDIF 
                ENDIF          
                IF (KCVT /= 0 .AND. IS <= NPT) THEN      
C                 STRAIN TENSOR IN GLOBAL SYSTEM  
c                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IS)%LBUF(1,1,1)  
                  DO I=1,NEL
                    N = I + NFT
                    IF(KCVT==2)THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF  
              ELSEIF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.(ISOLNOD == 8.AND.
     .            (JHBE == 14 .OR. JHBE == 17))) THEN
c-----------
               IF (TSHELL == 1.AND.IR > 0 .AND. IS > 0 .AND. IT > 0) THEN                              
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)  
                  IF (ISOLNOD == 16) LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,1,IT) 
                IF(MLW>=28.AND.MLW /= 49)THEN        
                  DO I=1,NEL
C                   3*9*3 points d'integration (r*s*t)    
                     EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                     EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                     EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                     EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                     EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                     EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                     IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
C                 3*9*3 points d'integration (r*s*t)     
                    EVAR(1,I) = LBUF%EPE(JJ(1) + I)   
                    EVAR(2,I) = LBUF%EPE(JJ(2) + I)   
                    EVAR(3,I) = LBUF%EPE(JJ(3) + I)   
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO       
                ELSEIF(MLW == 24 .OR. MLW == 25)THEN
                  DO I=1,NEL
C                      3*9*3 points d'integration (r*s*t)    
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)           
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I) * HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I) * HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I) * HALF
                    IS_WRITTEN_TENSOR(I) = 1 
                  ENDDO   
                ELSEIF (MLW == 25) THEN
                  DO I=1,NEL 
                    EVAR(1,I) =  LBUF%STRA(JJ(1) + I)         
                    EVAR(2,I) =  LBUF%STRA(JJ(2) + I)         
                    EVAR(3,I) =  LBUF%STRA(JJ(3) + I)         
                    EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF       
                    EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF          
                    EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF
                    IS_WRITTEN_TENSOR(I) = 1       
                    ENDDO  
                ELSEIF(ISTRAIN > 0)THEN
                  IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN
                    DO I=1,NEL 
C  3*9*3 points d'integration (r*s*t)    
                    EVAR(1,I) =  LBUF%STRA(JJ(1) + I)          
                    EVAR(2,I) =  LBUF%STRA(JJ(2) + I)          
                    EVAR(3,I) =  LBUF%STRA(JJ(3) + I)          
                    EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF        
                    EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF          
                    EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF 
                    IS_WRITTEN_TENSOR(I) = 1      
                    ENDDO 
                  ENDIF
                ENDIF
C
                IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C                STRAIN TENSOR IN GLOBAL SYSTEM
            ICSIG=IPARG(17,NG)  
            IF (JHBE == 14.AND.ICSIG > 0) THEN
              SELECT CASE (ICSIG)               
              CASE (1)                   
                DO I=1,NEL
                  N = I + NFT
              IF(KCVT==2 .AND. IGTYP ==22)THEN
                   GAMA(1)=ZERO
                           GAMA(2)=LBUF%GAMA(JJ(1) + I)
            GAMA(3)=LBUF%GAMA(JJ(2) + I)
                   GAMA(4)=ZERO
                   GAMA(5)=-GAMA(2)
                   GAMA(6)=GAMA(1)
              ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                   GAMA(1)=ZERO
                           GAMA(2)=GBUF%GAMA(JJ(1) + I)
            GAMA(3)=GBUF%GAMA(JJ(2) + I)
                   GAMA(4)=ZERO
                   GAMA(5)=-GAMA(2)
                   GAMA(6)=GAMA(1)
                 ELSE
                   GAMA(1)=ONE
                   GAMA(2)=ZERO
                   GAMA(3)=ZERO
                   GAMA(4)=ZERO
                   GAMA(5)=ONE
                   GAMA(6)=ZERO
                 END IF
                 CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              CASE (10)                    
                DO I=1,NEL
                  N = I + NFT
              IF(KCVT==2 .AND. IGTYP ==22)THEN
                          GAMA(1)=LBUF%GAMA(JJ(1) + I)
                          GAMA(2)=LBUF%GAMA(JJ(2) + I)
                    GAMA(3)=ZERO
                    GAMA(4)=-GAMA(2)
                    GAMA(5)=GAMA(1)
                    GAMA(6)=ZERO
              ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                          GAMA(1)=GBUF%GAMA(JJ(1) + I)
                          GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=ZERO
                    GAMA(4)=-GAMA(2)
                    GAMA(5)=GAMA(1)
                    GAMA(6)=ZERO
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              CASE (100)                    
                DO I=1,NEL
                  N = I + NFT
              IF(KCVT==2 .AND. IGTYP ==22)THEN
                          GAMA(1)=LBUF%GAMA(JJ(2) + I)
                    GAMA(2)=ZERO
                          GAMA(3)=LBUF%GAMA(JJ(1) + I)
                    GAMA(4)=GAMA(3)
                    GAMA(5)=ZERO
                    GAMA(6)=-GAMA(1)
              ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                          GAMA(1)=GBUF%GAMA(JJ(2) + I)
                    GAMA(2)=ZERO
                          GAMA(3)=GBUF%GAMA(JJ(1) + I)
                    GAMA(4)=GAMA(3)
                    GAMA(5)=ZERO
                    GAMA(6)=-GAMA(1)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              END SELECT 
            ELSE                
              DO I=1,NEL
                N = I + NFT
                IF(KCVT==2)THEN
                  GAMA(1)=GBUF%GAMA(JJ(1) + I)
                  GAMA(2)=GBUF%GAMA(JJ(2) + I)
                  GAMA(3)=GBUF%GAMA(JJ(3) + I)
                  GAMA(4)=GBUF%GAMA(JJ(4) + I)
                  GAMA(5)=GBUF%GAMA(JJ(5) + I)
                  GAMA(6)=GBUF%GAMA(JJ(6) + I)
                ELSE
                  GAMA(1)=ONE
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=ONE
                  GAMA(6)=ZERO
                END IF
                CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
              ENDDO
            ENDIF  !(JHBE == 14.AND.ICSIG > 0)    
                ENDIF 
               END IF ! (TSHELL == 1.AND.IR                                        
              ENDIF

            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY) THEN 
c ILAY= IS= IT=
              EVAR(1:6,1:NEL) = ZERO
              IF ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15) THEN
c-----------
                IF ( ILAY <= NPT ) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)  
                  IF (MLW>=28.AND.MLW /= 49 .OR. MLW == 24) THEN
                    DO I=1,NEL
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                    DO I=1,NEL
                      N = I + NFT 
                      EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I) 
                      IS_WRITTEN_TENSOR(I) = 1      
                    ENDDO 
                  ELSEIF (ISTRAIN > 0) THEN           
                    IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN        
                      DO I=1,NEL                
          EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
          EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
          EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
          EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
          EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
          EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                    ENDIF
                  ENDIF 
                ENDIF          
                IF (KCVT /= 0 .AND. IS <= NPT) THEN      
C                 STRAIN TENSOR IN GLOBAL SYSTEM  
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IS)%LBUF(1,1,1)  
                  DO I=1,NEL
                    N = I + NFT
                    IF(KCVT==2)THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF  
              ENDIF
            ENDIF

            IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
              DO I=1,NEL
                EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
                EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
                EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
                EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
                EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
                EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
              ENDDO
            ENDIF
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN_ENG') THEN
C-----------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL average value at elem center only      
            IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
              EVAR(1:6,1:NEL) = ZERO
               DO I=1,8
                  KK(I) = NEL*(I-1)
               END DO
              IF (ISOLNOD == 4 .OR. ISOLNOD == 10) THEN
                IS_WRITTEN_TENSOR(1:NEL) = 1
                DO I=1,NEL
                  N = I + NFT
                  NNI = IXS(2,N)
                  J = I
                  XN(J)=X(1,NNI)
                  YN(J)=X(2,NNI)
                  ZN(J)=X(3,NNI)
                  DXN(J)=D(1,NNI)
                  DYN(J)=D(2,NNI)
                  DZN(J)=D(3,NNI)
                  NNI = IXS(4,N)
                  J = KK(2)+I
                  XN(J)=X(1,NNI)
                  YN(J)=X(2,NNI)
                  ZN(J)=X(3,NNI)
                  DXN(J)=D(1,NNI)
                  DYN(J)=D(2,NNI)
                  DZN(J)=D(3,NNI)
                  NNI = IXS(7,N)
                  J = KK(3)+I
                  XN(J)=X(1,NNI)
                  YN(J)=X(2,NNI)
                  ZN(J)=X(3,NNI)
                  DXN(J)=D(1,NNI)
                  DYN(J)=D(2,NNI)
                  DZN(J)=D(3,NNI)
                  NNI = IXS(6,N)
                  J = KK(4)+I
                  XN(J)=X(1,NNI)
                  YN(J)=X(2,NNI)
                  ZN(J)=X(3,NNI)
                  DXN(J)=D(1,NNI)
                  DYN(J)=D(2,NNI)
                  DZN(J)=D(3,NNI)
                END DO
                CALL T4_TSTRAIN(XN,YN,ZN,DXN,DYN,DZN,EVAR,NEL)
c-----------
              ELSE IF (ISOLNOD == 8 .OR. ISOLNOD == 16 .OR. ISOLNOD == 20) THEN
c-----------
                IS_WRITTEN_TENSOR(1:NEL) = 1
                DO I=1,NEL
                  N = I + NFT
                  DO II = 1, 8
                    NNI = IXS(II+1,N)
                    JJ = KK(II)+I
                    XN(JJ)=X(1,NNI)
                    YN(JJ)=X(2,NNI)
                    ZN(JJ)=X(3,NNI)
                    DXN(JJ)=D(1,NNI)
                    DYN(JJ)=D(2,NNI)
                    DZN(JJ)=D(3,NNI)
                  END DO
                END DO
                CALL S8_TSTRAIN(XN,YN,ZN,DXN,DYN,DZN,EVAR,NEL)
c-----------
              ELSE IF (ISOLNOD == 6 )THEN
                IS_WRITTEN_TENSOR(1:NEL) = 1
                DO I=1,NEL
                  N = I + NFT
                  DO II = 1, 3
                    NNI = IXS(II+1,N)
                    JJ = KK(II)+I
                    XN(JJ)=X(1,NNI)
                    YN(JJ)=X(2,NNI)
                    ZN(JJ)=X(3,NNI)
                    DXN(JJ)=D(1,NNI)
                    DYN(JJ)=D(2,NNI)
                    DZN(JJ)=D(3,NNI)
                    NNI = IXS(II+5,N)
                    JJ = KK(II+3)+I
                    XN(JJ)=X(1,NNI)
                    YN(JJ)=X(2,NNI)
                    ZN(JJ)=X(3,NNI)
                    DXN(JJ)=D(1,NNI)
                    DYN(JJ)=D(2,NNI)
                    DZN(JJ)=D(3,NNI)
                  END DO
                END DO
                CALL S6_TSTRAIN(XN,YN,ZN,DXN,DYN,DZN,EVAR,NEL)
c-----------
              END IF
            END IF
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/DAMA') THEN
C-----------------------------------------------
C         CRACKS
C-----------------------------------------------
            IF (MLW == 24 .AND. NINT(PM(56,MT1)) == 1) THEN
c-----------
              DO I=1,NEL
          EVAR(1,I) = ZERO
          EVAR(2,I) = ZERO
          EVAR(3,I) = ZERO
          EVAR(4,I) = ZERO
          EVAR(5,I) = ZERO
          EVAR(6,I) = ZERO
              ENDDO
c----------- not work for the moment
              IF (ISOLNOD == 8 .AND.(JHBE == 14 .OR. JHBE == 15)) THEN
c-----------
                IF (TSHELL == 1 ) THEN
                   LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)
                  DO I=1,NEL              
                    EVAR(1,I) = LBUF%DGLO(JJ(1) + I)
                    EVAR(2,I) = LBUF%DGLO(JJ(2) + I)
                    EVAR(3,I) = LBUF%DGLO(JJ(3) + I)
                    EVAR(4,I) = LBUF%DGLO(JJ(4) + I)
                    EVAR(5,I) = LBUF%DGLO(JJ(5) + I)
                    EVAR(6,I) = LBUF%DGLO(JJ(6) + I)
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ELSE                  
c----------- avoid crash
c                 DO IPT=1,NPTG             
c                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)c%LBUF(IPT,1,1)    
c                  DO I=1,NEL              
c                    EVAR(1,I) = EVAR(1,I)+LBUF%DGLO(JJ(1) + I)/NPTG
c                    EVAR(2,I) = EVAR(2,I)+LBUF%DGLO(JJ(2) + I)/NPTG
c                    EVAR(3,I) = EVAR(3,I)+LBUF%DGLO(JJ(3) + I)/NPTG
c                    EVAR(4,I) = EVAR(4,I)+LBUF%DGLO(JJ(4) + I)/NPTG
c                    EVAR(5,I) = EVAR(5,I)+LBUF%DGLO(JJ(5) + I)/NPTG
c                    EVAR(6,I) = EVAR(6,I)+LBUF%DGLO(JJ(6) + I)/NPTG
c                    IS_WRITTEN_TENSOR(I) = 1
c                  ENDDO                
c                 ENDDO                 
                END IF !(TSHELL == 1 ) THEN
              ELSE
          LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)       
          DO I=1,NEL              
            EVAR(1,I) = EVAR(1,I)+LBUF%DGLO(JJ(1) + I)
            EVAR(2,I) = EVAR(2,I)+LBUF%DGLO(JJ(2) + I)
            EVAR(3,I) = EVAR(3,I)+LBUF%DGLO(JJ(3) + I)
            EVAR(4,I) = EVAR(4,I)+LBUF%DGLO(JJ(4) + I)
            EVAR(5,I) = EVAR(5,I)+LBUF%DGLO(JJ(5) + I)
            EVAR(6,I) = EVAR(6,I)+LBUF%DGLO(JJ(6) + I)
                  IS_WRITTEN_TENSOR(I) = 1
          ENDDO                 
                ENDIF
              IF (KCVT /= 0) THEN
C               DAMAGE IN GLOBAL SYSTEM
                DO I=1,NEL
                  N = I + NFT
                    IF (KCVT==2) THEN
                II = 6*(I-1)              
                GAMA(1)= GBUF%GAMA(JJ(1) + I)
                GAMA(2)= GBUF%GAMA(JJ(2) + I)
                GAMA(3)= ZERO
                GAMA(4)=-GAMA(2)
                GAMA(5)= GAMA(1)
                GAMA(6)= ZERO
                    ELSE
                GAMA(1)=ONE
                GAMA(2)=ZERO
                GAMA(3)=ZERO
                GAMA(4)=ZERO
                GAMA(5)=ONE
                GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              ENDIF
            ELSE
              DO I=1,NEL
          EVAR(1,I) = ZERO
          EVAR(2,I) = ZERO
          EVAR(3,I) = ZERO
          EVAR(4,I) = ZERO
          EVAR(5,I) = ZERO
          EVAR(6,I) = ZERO
              ENDDO
            ENDIF

            IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
              DO I=1,NEL
          EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
          EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
          EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
          EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
          EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
          EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
              ENDDO
            ENDIF
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/EPSP') THEN
C-----------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
            IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
              DO I=1,NEL
          EVAR(1,I) = ZERO
          EVAR(2,I) = ZERO
          EVAR(3,I) = ZERO
          EVAR(4,I) = ZERO
          EVAR(5,I) = ZERO
          EVAR(6,I) = ZERO
              ENDDO
c-----------
              IF ((ISOLNOD==8.OR.(ISOLNOD==4 .AND. (ISROT==0.OR.ISROT==3))).AND.
     .               NPT==1 .AND. JHBE /= 14 .AND. JHBE /= 15) THEN
c-----------
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)               
                IF (ISORTH > 0) ISORTHG = 1
!
                IF (MLW == 24) THEN
                  DO I=1,NEL
                    EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)
                    EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ENDIF ! IF (MLW == 24)
!
                IF (KCVT /= 0) THEN
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL
                    N = I + NFT
                    IF (KCVT == 2) THEN
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF
c-----------
              ELSEIF(ISOLNOD == 16.OR.ISOLNOD == 20 .OR.
     .            (ISOLNOD == 8.AND.(JHBE == 14.OR.JHBE == 17)))THEN
c-----------
                IF (MLW == 24) THEN
                  DO I=1,NEL
                   N = I + NFT   
                   DO IL=1,NLAY         
                    DO IS=1,NPTS         
                     DO IT=1,NPTT
                      DO IR=1,NPTR
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                       IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
          EVAR_TMP(1) =  LBUF%PLA(JJ(1) + I + NEL)/NPT
          EVAR_TMP(2) =  LBUF%PLA(JJ(2) + I + NEL)/NPT
          EVAR_TMP(3) =  LBUF%PLA(JJ(3) + I + NEL)/NPT
          EVAR_TMP(4) =  LBUF%PLA(JJ(4) + I + NEL)*HALF/NPT
          EVAR_TMP(5) =  LBUF%PLA(JJ(5) + I + NEL)*HALF/NPT
          EVAR_TMP(6) =  LBUF%PLA(JJ(6) + I + NEL)*HALF/NPT
              ICSIG=IPARG(17,NG)  
              IF (KCVT /= 0 .AND.ICSIG > 0) THEN
C                  PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                IF (JHBE == 14) THEN
                  SELECT CASE (ICSIG)              
                  CASE (1)     
                    IF (KCVT == 2.AND.IGTYP == 22) THEN
                      GAMA(1)= ZERO
                      GAMA(2)= LBUF%GAMA(JJ(1) + I)
                      GAMA(3)= LBUF%GAMA(JJ(2) + I)
                      GAMA(4)= ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)= GAMA(1)
                    ELSEIF (KCVT == 2.AND.IGTYP == 21) THEN
                      GAMA(1)= ZERO
                      GAMA(2)= GBUF%GAMA(JJ(1) + I)
                      GAMA(3)= GBUF%GAMA(JJ(2) + I)
                      GAMA(4)= ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)= GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  CASE (10)      
                    IF (KCVT == 2.AND.IGTYP == 22) THEN
                      GAMA(1)= LBUF%GAMA(JJ(1) + I)
                      GAMA(2)= LBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSEIF (KCVT == 2.AND.IGTYP == 21) THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  CASE (100)     
                    IF (KCVT == 2.AND.IGTYP == 22) THEN
                      GAMA(1)= LBUF%GAMA(JJ(2) + I)
                      GAMA(2)= ZERO
                      GAMA(3)= LBUF%GAMA(JJ(1) + I)
                      GAMA(4)= GAMA(3)
                      GAMA(5)= ZERO
                      GAMA(6)=-GAMA(1)
                    ELSEIF (KCVT == 2.AND.IGTYP == 21) THEN
                      GAMA(1)= GBUF%GAMA(JJ(2) + I)
                      GAMA(2)= ZERO
                      GAMA(3)= GBUF%GAMA(JJ(1) + I)
                      GAMA(4)= GAMA(3)
                      GAMA(5)= ZERO
                      GAMA(6)=-GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  END SELECT                 
                ENDIF
              ENDIF      
          EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
          EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
          EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
          EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
          EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
          EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6)
          IS_WRITTEN_TENSOR(I) = 1
                       ENDIF
                      ENDDO 
                     ENDDO
                    ENDDO
                   ENDDO
                  ENDDO
                ENDIF ! IF (MLW == 24)
c---
                ICSIG=IPARG(17,NG) 
                IF (KCVT /= 0 .AND.ICSIG == 0 .AND. JHBE /= 16) THEN
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL               
                    N = I + NFT              
                    IF (KCVT == 2) THEN           
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE             
                      GAMA(1)=ONE           
                      GAMA(2)=ZERO           
                      GAMA(3)=ZERO           
                      GAMA(4)=ZERO           
                      GAMA(5)=ONE           
                      GAMA(6)=ZERO           
                    END IF             
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO 
                ENDIF
c-----------
              ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
                IF (MLW == 24 .and. ISTRAIN > 0) THEN
                  DO I=1,NEL
                    DO IPT=1,NPT
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
                      EVAR(1,I) = EVAR(1,I)+LBUF%PLA(JJ(1) + I + NEL)/NPT
                      EVAR(2,I) = EVAR(2,I)+LBUF%PLA(JJ(2) + I + NEL)/NPT
                      EVAR(3,I) = EVAR(3,I)+LBUF%PLA(JJ(3) + I + NEL)/NPT
                      EVAR(4,I) = EVAR(4,I)+LBUF%PLA(JJ(4) + I + NEL)*HALF/NPT
                      EVAR(5,I) = EVAR(5,I)+LBUF%PLA(JJ(5) + I + NEL)*HALF/NPT
                      EVAR(6,I) = EVAR(6,I)+LBUF%PLA(JJ(6) + I + NEL)*HALF/NPT
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ENDIF ! IF (MLW == 24 .and. ISTRAIN > 0)
                IF (KCVT /= 0) THEN
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL          
                    N = I + NFT             
                    IF (KCVT == 2) THEN          
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE          
                      GAMA(1)=ONE        
                      GAMA(2)=ZERO        
                      GAMA(3)=ZERO        
                      GAMA(4)=ZERO        
                      GAMA(5)=ONE        
                      GAMA(6)=ZERO        
                    ENDIF         
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO             
                ENDIF
c-----------
              ELSEIF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
c-----------
                IF (MLW == 24 .and. ISTRAIN > 0) THEN
                  DO I=1,NEL               
                      DO IL= 1,NLAY
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,1)  
          EVAR(1,I) = EVAR(1,I)+LBUF%PLA(JJ(1) + I + NEL)/NLAY
          EVAR(2,I) = EVAR(2,I)+LBUF%PLA(JJ(2) + I + NEL)/NLAY
          EVAR(3,I) = EVAR(3,I)+LBUF%PLA(JJ(3) + I + NEL)/NLAY
          EVAR(4,I) = EVAR(4,I)+LBUF%PLA(JJ(4) + I + NEL)*HALF/NLAY      
          EVAR(5,I) = EVAR(5,I)+LBUF%PLA(JJ(5) + I + NEL)*HALF/NLAY 
          EVAR(6,I) = EVAR(6,I)+LBUF%PLA(JJ(6) + I + NEL)*HALF/NLAY   
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO                   
                  ENDDO                  
                ENDIF ! IF (MLW == 24 .and. ISTRAIN > 0)             
                IF (KCVT /= 0) THEN
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL            
                    N = I + NFT         
                    IF (KCVT == 2) THEN           
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)  
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)  
                      GAMA(3)= ZERO           
                      GAMA(4)=-GAMA(2)           
                      GAMA(5)= GAMA(1)           
                      GAMA(6)= ZERO           
                    ELSE             
                      GAMA(1)=ONE           
                      GAMA(2)=ZERO           
                      GAMA(3)=ZERO           
                      GAMA(4)=ZERO           
                      GAMA(5)=ONE           
                      GAMA(6)=ZERO           
                    END IF             
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO 
                ENDIF
c-----------
              ENDIF  ! ISOLNOD & ......

            ELSEIF ( ILAY == -1 .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS .AND. IT >= 0 .AND. IT <= NPTT) THEN 
c IR= IS= IT=

C-----------------------------------------------
            DO I=1,NEL
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO
            ENDDO
c-----------
            IF ((ISOLNOD == 8 .OR.NPT == 1 .OR.
     .              (ISOLNOD == 4 .AND. ISROT == 0)) .AND.
     .               JHBE /= 14.AND.JHBE /= 15.AND.JHBE /= 17) THEN 
c-----------
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              IF (IPT == 1 ) THEN          
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
                IF (MLW == 24) THEN
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = EVAR(3,I) + LBUF%PLA(JJ(3) + I + NEL)
                    EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%PLA(JJ(6) + I + NEL)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  END DO
                ENDIF ! IF (MLW == 24)
              ENDIF ! IF (IPT == 1 )
C
              IF (KCVT /= 0) THEN
C             PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=1,NEL
            N = I + NFT
                  IF(KCVT==2)THEN
                    GAMA(1)=GBUF%GAMA(JJ(1) + I)
                    GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=GBUF%GAMA(JJ(3) + I)
                    GAMA(4)=GBUF%GAMA(JJ(4) + I)
                    GAMA(5)=GBUF%GAMA(JJ(5) + I)
                    GAMA(6)=GBUF%GAMA(JJ(6) + I)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              ENDIF     
c-----------
           ELSEIF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.(ISOLNOD == 8.AND.
     .            (JHBE == 14 .OR. JHBE == 17))) THEN
c-----------
           ICSIG = IPARG(17,NG) 
           IF (IOR_TSH >0) THEN
               IF (ICSIG == 10) THEN
               IR=IT_INPUT
               IS=IR_INPUT
               IT=IS_INPUT
               ELSEIF (ICSIG == 1) THEN
                 IR=IS_INPUT
                 IS=IT_INPUT
                 IT=IR_INPUT
             ELSE
               IR=IR_INPUT
               IS=IS_INPUT
               IT=IT_INPUT
             ENDIF
           ENDIF
           IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
           IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS
     .            .AND. IT <= NPTT .AND. IR*IS*IT >= 1) THEN
             IF (TSHELL == 1) THEN                              
               LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)   
             ELSE                                               
               LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)   
             ENDIF                                          
             IF (MLW == 24) THEN
               DO I=1,NEL
C                   3*9*3 points d'integration (r*s*t)         
                 EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                 EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                 EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)        
                 EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL) * HALF
                 EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL) * HALF
                 EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL) * HALF
                 IS_WRITTEN_TENSOR(I) = 1 
               ENDDO   
             ENDIF ! IF (MLW == 24)
C
             IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C              PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
            ICSIG=IPARG(17,NG)  
            IF (JHBE == 14.AND.ICSIG > 0) THEN
             SELECT CASE (ICSIG)               
             CASE (1)                 
             DO I=1,NEL
               N = I + NFT
               IF (KCVT == 2.AND.IGTYP == 22) THEN
                 GAMA(1)=ZERO
                         GAMA(2)=LBUF%GAMA(JJ(1) + I)
                       GAMA(3)=LBUF%GAMA(JJ(2) + I)
                 GAMA(4)=ZERO
                 GAMA(5)=-GAMA(2)
                 GAMA(6)=GAMA(1)
               ELSEIF (KCVT == 2.AND.IGTYP == 21) THEN
                 GAMA(1)=ZERO
                         GAMA(2)=GBUF%GAMA(JJ(1) + I)
                       GAMA(3)=GBUF%GAMA(JJ(2) + I)
                 GAMA(4)=ZERO
                 GAMA(5)=-GAMA(2)
                 GAMA(6)=GAMA(1)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
               CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
             CASE (10)                  
             DO I=1,NEL
               N = I + NFT
               IF (KCVT == 2.AND.IGTYP == 22) THEN
                       GAMA(1)=LBUF%GAMA(JJ(1) + I)
                       GAMA(2)=LBUF%GAMA(JJ(2) + I)
                 GAMA(3)=ZERO
                 GAMA(4)=-GAMA(2)
                 GAMA(5)=GAMA(1)
                 GAMA(6)=ZERO
               ELSEIF (KCVT == 2.AND.IGTYP == 21) THEN
                       GAMA(1)=GBUF%GAMA(JJ(1) + I)
                       GAMA(2)=GBUF%GAMA(JJ(2) + I)
                 GAMA(3)=ZERO
                 GAMA(4)=-GAMA(2)
                 GAMA(5)=GAMA(1)
                 GAMA(6)=ZERO
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
               CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
             CASE (100)                   
             DO I=1,NEL
               N = I + NFT
               IF (KCVT == 2.AND.IGTYP == 22) THEN
                       GAMA(1)=LBUF%GAMA(JJ(2) + I)
                 GAMA(2)=ZERO
                       GAMA(3)=LBUF%GAMA(JJ(1) + I)
                 GAMA(4)=GAMA(3)
                 GAMA(5)=ZERO
                 GAMA(6)=-GAMA(1)
               ELSEIF (KCVT == 2.AND.IGTYP == 21) THEN
                       GAMA(1)=GBUF%GAMA(JJ(2) + I)
                 GAMA(2)=ZERO
                       GAMA(3)=GBUF%GAMA(JJ(1) + I)
                 GAMA(4)=GAMA(3)
                 GAMA(5)=ZERO
                 GAMA(6)=-GAMA(1)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
               CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
             END SELECT 
            ELSE  
              DO I=1,NEL
                N = I + NFT
                IF (KCVT == 2) THEN
                  GAMA(1)=GBUF%GAMA(JJ(1) + I)
                  GAMA(2)=GBUF%GAMA(JJ(2) + I)
                  GAMA(3)=GBUF%GAMA(JJ(3) + I)
                  GAMA(4)=GBUF%GAMA(JJ(4) + I)
                  GAMA(5)=GBUF%GAMA(JJ(5) + I)
                  GAMA(6)=GBUF%GAMA(JJ(6) + I)
                ELSE
                  GAMA(1)=ONE
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=ONE
                  GAMA(6)=ZERO
                END IF
                CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
              ENDDO
             ENDIF  !(JHBE == 14.AND.ICSIG > 0)   
               ENDIF ! IF (KCVT /= 0 .AND. JHBE /= 16)
             ENDIF

c-----------
            ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
              IPT = 0
              IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
              IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
              IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
              IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
              IF ( IPT > 0) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
                IF (MLW == 24) THEN
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = EVAR(3,I) + LBUF%PLA(JJ(3) + I + NEL)
                    EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%PLA(JJ(6) + I + NEL)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ENDIF ! IF (MLW == 24)
              ENDIF ! IF ( IPT > 0)
c
              IF (KCVT /= 0) THEN
                DO I=1,NEL
                  N = I + NFT
                  IF (KCVT == 2) THEN
                    GAMA(1)=GBUF%GAMA(JJ(1) + I)
                    GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=GBUF%GAMA(JJ(3) + I)
                    GAMA(4)=GBUF%GAMA(JJ(4) + I)
                    GAMA(5)=GBUF%GAMA(JJ(5) + I)
                    GAMA(6)=GBUF%GAMA(JJ(6) + I)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              ENDIF  

            ENDIF                          
c-----------
            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY  .AND. IR >= 0 .AND. IR <= NPTR .AND. 
     .                   IS >= 0 .AND.  IS <= NPTS) THEN 
              EVAR(1:6,1:NEL)=ZERO
              IF ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15) THEN
c-----------
                IF ( ILAY <= NPT ) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)  
                  IF (MLW == 24) THEN
                    DO I=1,NEL
                      EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)
                      EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)
                      EVAR(3,I) = EVAR(3,I) + LBUF%PLA(JJ(3) + I + NEL)
                      EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%PLA(JJ(5) + I + NEL)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%PLA(JJ(6) + I + NEL)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDIF ! IF (MLW == 24)
                ENDIF ! IF ( ILAY <= NPT )        
                IF (KCVT /= 0 .AND. IS <= NPT) THEN      
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM  
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IS)%LBUF(1,1,1)  
                  DO I=1,NEL
                    N = I + NFT
                    IF (KCVT == 2) THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF  
              ELSEIF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.(ISOLNOD == 8.AND.
     .            (JHBE == 14 .OR. JHBE == 17))) THEN
c-----------
                ICSIG = IPARG(17,NG) 
c               IF (IOR_TSH >0) THEN
c                  IF (ICSIG == 10) THEN
c                   IR=IT_INPUT
c                   IS=IR_INPUT
c                   IT=IS_INPUT
c                  ELSEIF (ICSIG == 1) THEN
c                    IR=IS_INPUT
c                    IS=IT_INPUT
c                    IT=IR_INPUT
c                 ELSE
c                   IR=IR_INPUT
c                   IS=IS_INPUT
c                   IT=IT_INPUT
c                 ENDIF
c               ENDIF
                IF (TSHELL == 1) THEN                              
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)  
                ENDIF                                          
                IF (MLW == 24) THEN
                  DO I=1,NEL
C                      3*9*3 points d'integration (r*s*t)    
                    EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)           
                    EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL) * HALF
                    EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL) * HALF
                    EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL) * HALF
                    IS_WRITTEN_TENSOR(I) = 1 
                  ENDDO   
                ENDIF ! IF (MLW == 24)
C
                IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
            ICSIG=IPARG(17,NG)  
            IF (JHBE == 14.AND.ICSIG > 0) THEN
              SELECT CASE (ICSIG)               
              CASE (1)                   
                DO I=1,NEL
                  N = I + NFT
                 IF (KCVT == 2.AND. IGTYP==22) THEN
                   GAMA(1)=ZERO
                           GAMA(2)=LBUF%GAMA(JJ(1) + I)
            GAMA(3)=LBUF%GAMA(JJ(2) + I)
                   GAMA(4)=ZERO
                   GAMA(5)=-GAMA(2)
                   GAMA(6)=GAMA(1)
                 ELSEIF (KCVT == 2.AND. IGTYP==21) THEN
                   GAMA(1)=ZERO
                           GAMA(2)=GBUF%GAMA(JJ(1) + I)
            GAMA(3)=GBUF%GAMA(JJ(2) + I)
                   GAMA(4)=ZERO
                   GAMA(5)=-GAMA(2)
                   GAMA(6)=GAMA(1)
                 ELSE
                   GAMA(1)=ONE
                   GAMA(2)=ZERO
                   GAMA(3)=ZERO
                   GAMA(4)=ZERO
                   GAMA(5)=ONE
                   GAMA(6)=ZERO
                 END IF
                 CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              CASE (10)                    
                DO I=1,NEL
                  N = I + NFT
                 IF (KCVT == 2.AND. IGTYP==22) THEN
                          GAMA(1)=LBUF%GAMA(JJ(1) + I)
                          GAMA(2)=LBUF%GAMA(JJ(2) + I)
                    GAMA(3)=ZERO
                    GAMA(4)=-GAMA(2)
                    GAMA(5)=GAMA(1)
                    GAMA(6)=ZERO
                 ELSEIF (KCVT == 2.AND. IGTYP==21) THEN
                          GAMA(1)=GBUF%GAMA(JJ(1) + I)
                          GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=ZERO
                    GAMA(4)=-GAMA(2)
                    GAMA(5)=GAMA(1)
                    GAMA(6)=ZERO
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              CASE (100)                    
                DO I=1,NEL
                  N = I + NFT
                 IF (KCVT == 2.AND. IGTYP==22) THEN
                          GAMA(1)=LBUF%GAMA(JJ(2) + I)
                    GAMA(2)=ZERO
                          GAMA(3)=LBUF%GAMA(JJ(1) + I)
                    GAMA(4)=GAMA(3)
                    GAMA(5)=ZERO
                    GAMA(6)=-GAMA(1)
                 ELSEIF (KCVT == 2.AND. IGTYP==21) THEN
                          GAMA(1)=GBUF%GAMA(JJ(2) + I)
                    GAMA(2)=ZERO
                          GAMA(3)=GBUF%GAMA(JJ(1) + I)
                    GAMA(4)=GAMA(3)
                    GAMA(5)=ZERO
                    GAMA(6)=-GAMA(1)
                  ELSE
                    GAMA(1)=ONE
                    GAMA(2)=ZERO
                    GAMA(3)=ZERO
                    GAMA(4)=ZERO
                    GAMA(5)=ONE
                    GAMA(6)=ZERO
                  END IF
                  CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                ENDDO
              END SELECT 
            ELSE                
              DO I=1,NEL
                N = I + NFT
                IF (KCVT == 2) THEN
                  GAMA(1)=GBUF%GAMA(JJ(1) + I)
                  GAMA(2)=GBUF%GAMA(JJ(2) + I)
                  GAMA(3)=GBUF%GAMA(JJ(3) + I)
                  GAMA(4)=GBUF%GAMA(JJ(4) + I)
                  GAMA(5)=GBUF%GAMA(JJ(5) + I)
                  GAMA(6)=GBUF%GAMA(JJ(6) + I)
                ELSE
                  GAMA(1)=ONE
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=ONE
                  GAMA(6)=ZERO
                END IF
                CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
              ENDDO
            ENDIF  !(JHBE == 14.AND.ICSIG > 0)    
                ENDIF 
              ENDIF

            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY) THEN 
c ILAY= IS= IT=
              EVAR(1:6,1:NEL)=ZERO
              IF ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15) THEN
c-----------
                IF ( ILAY <= NPT ) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)  
                  IF (MLW == 24) THEN
                    DO I=1,NEL
                      EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)
                      EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)
                      EVAR(3,I) = EVAR(3,I) + LBUF%PLA(JJ(3) + I + NEL)
                      EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%PLA(JJ(5) + I + NEL)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%PLA(JJ(6) + I + NEL)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDIF ! IF (MLW == 24)
                ENDIF ! IF ( ILAY <= NPT )        
                IF (KCVT /= 0 .AND. IS <= NPT) THEN      
C               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM  
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IS)%LBUF(1,1,1)  
                  DO I=1,NEL
                    N = I + NFT
                    IF (KCVT == 2) THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF  
              ENDIF
            ENDIF
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRESS/CORNER_DATA') THEN
C-----------------------------------------------
c ELEMENT DATA
            DO I=1,NEL
              II = 6*(I-1)
              EVAR(1,I) = GBUF%SIG(JJ(1) + I)
              EVAR(2,I) = GBUF%SIG(JJ(2) + I)
              EVAR(3,I) = GBUF%SIG(JJ(3) + I)
              EVAR(4,I) = GBUF%SIG(JJ(4) + I)
              EVAR(5,I) = GBUF%SIG(JJ(5) + I)
              EVAR(6,I) = GBUF%SIG(JJ(6) + I)
              IS_WRITTEN_TENSOR(I) = 1
            ENDDO
            IF(IVISC > 0) THEN
               LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
               DO I=1,NEL
                 II = 6*(I-1)
                 EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)
                 EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)
                 EVAR(3,I) =EVAR(3,I)+ LBUF%VISC(JJ(3) + I)
                 EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)
                 EVAR(5,I) =EVAR(5,I)+ LBUF%VISC(JJ(5) + I)
                 EVAR(6,I) =EVAR(6,I)+ LBUF%VISC(JJ(6) + I)
               ENDDO
            ENDIF
c
            IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C            STRESS TENSOR IN GLOBAL SYSTEM
             DO I=1,NEL
              N = I + NFT
C              pour JHBE=14, valeurs moyennes est dans rep. corota.
               IF(KCVT==2.AND.JHBE/=14)THEN
                 II = 6*(I-1)
                 GAMA(1)=GBUF%GAMA(JJ(1) + I)
                 GAMA(2)=GBUF%GAMA(JJ(2) + I)
                 GAMA(3)=GBUF%GAMA(JJ(3) + I)
                 GAMA(4)=GBUF%GAMA(JJ(4) + I)
                 GAMA(5)=GBUF%GAMA(JJ(5) + I)
                 GAMA(6)=GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
                 CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
             ENDDO
            ENDIF
c
            IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
              DO I=1,NEL
                EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
                EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
                EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
                EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
                EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
                EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
              ENDDO
            ENDIF
c CORNER DATA 
            CALL STRS_TENSCOR3(ELBUF_TAB(NG),IPARG(1,NG),IXS      ,IXS10   ,X        ,
     .                         PM        ,KCVT ,NEL ,EVAR_CORNER )
            IF (ISOLNOD <= 10) IS_WRITTEN_TENSOR(1:NEL) = 1
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN/CORNER_DATA') THEN
C-----------------------------------------------
              EVAR(1:6,1:NEL)=ZERO
C---- Element Data first
              IF (ISOLNOD == 8 .AND. IGTYP == 43) THEN
c-----------
               DO I=1,NEL               
                 II = 3*(I-1)
                 DO IPT= 1,NPTR
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)             
                   EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(1) + I)/NPT
                   EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT
                   EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(3) + I)/NPT
                   IS_WRITTEN_TENSOR(I) = 1
                 ENDDO  
               ENDDO                   
               DO I=1,NEL       
                 N = I + NFT         
                 CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
               ENDDO             
c-----------
              ELSEIF (ISOLNOD == 8 .AND. NPT == 8 .AND. JHBE /= 14.AND.
     .          JHBE /= 24.AND.JHBE /= 15.AND.JHBE /= 17 )THEN
c-----------
                NVAUX =IPARG(18,NG)      
                IF (MLW>=28) THEN
                  DO I=1,NEL
                    II = 6*(I-1)
                    N = I + NFT
                    DO J=1,8
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,J)
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)*ONE_OVER_8
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)*ONE_OVER_8
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)*ONE_OVER_8
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*ONE_OVER_8
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*ONE_OVER_8
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*ONE_OVER_8
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ENDIF
c-----------
              ELSEIF ((ISOLNOD==8.OR.(ISOLNOD==4 .AND. (ISROT==0.OR.ISROT==3))).AND.
     .               NPT==1 .AND. JHBE /= 14 .AND. JHBE /= 15) THEN
c-----------
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)               
                IF (ISORTH > 0) ISORTHG = 1
c
                IF (MLW>=28.AND.MLW /= 49) THEN
                  DO I=1,NEL
                    N = I + NFT
                    II = 6*(I-1)
                    EVAR(1,I) =  LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) =  LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) =  LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) =  LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) =  LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) =  LBUF%STRA(JJ(6) + I)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO   
                  IF (ISORTH > 0) THEN
C                   STRAIN TENSOR IN GLOBAL SYSTEM
                    KCVT = 2
                    DO I=1,NEL
                      N = I + NFT
                      II = 3*(I-1)
                      GAMA(1) = GBUF%GAMA(JJ(1) + I)
                      GAMA(2) = GBUF%GAMA(JJ(2) + I)
                      GAMA(3) = GBUF%GAMA(JJ(3) + I)
                      GAMA(4) = GBUF%GAMA(JJ(4) + I)
                      GAMA(5) = GBUF%GAMA(JJ(5) + I)
                      GAMA(6) = GBUF%GAMA(JJ(6) + I)
                      CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                    ENDDO
                  ENDIF
                ELSEIF (MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
                    N = I + NFT            
                    II = 3*(I-1)
                    EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)
                    EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ELSEIF (MLW == 24 .OR. MLW == 25)THEN
                  DO I=1,NEL
                    N = I + NFT            
                    II = 6*(I-1)
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO
                ELSEIF (ISTRAIN > 0) THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28.OR.
     .                MLW == 49) THEN 
                    DO I=1,NEL
                      N = I + NFT  
                      II = 6*(I-1)
                      EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDIF
                ENDIF
                IF (KCVT /= 0) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL
                    N = I + NFT
                    IF(KCVT==2)THEN
                      II = 6*(I-1)
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO
                ENDIF
c-----------
              ELSEIF(ISOLNOD == 16.OR.ISOLNOD == 20 .OR.
     .            (ISOLNOD == 8.AND.(JHBE == 14.OR.JHBE == 17)))THEN
c-----------
                IF (MLW>=28.AND.MLW /= 49)THEN
                  DO I=1,NEL
                   N = I + NFT   
                   II = 6*(I-1)
                   DO IL=1,NLAY         
                    DO IS=1,NPTS         
                     DO IT=1,NPTT
                      DO IR=1,NPTR
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                       EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)/NPT
                       EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)/NPT
                       EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)/NPT
                       EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF/NPT
                       EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF/NPT
                       EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF/NPT
                       IS_WRITTEN_TENSOR(I) = 1
                      ENDDO 
                     ENDDO
                    ENDDO
                   ENDDO
                  ENDDO
                ELSEIF (MLW == 12 .OR. MLW == 14) THEN
                  DO I=1,NEL              
                   N = I + NFT                
                   II = 3*(I-1)               
                   DO IL=1,NLAY               
                    DO IS=1,NPTS              
                     DO IT=1,NPTT              
                      DO IR=1,NPTR              
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
          EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/NPT 
          EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT 
          EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/NPT   
          IS_WRITTEN_TENSOR(I) = 1      
                      ENDDO                
                     ENDDO                
                    ENDDO                
                   ENDDO                
                  ENDDO                 
                ELSEIF(MLW == 24 .OR. MLW == 25)THEN
                  DO I=1,NEL
                   N = I + NFT   
                   II = 6*(I-1)
                   DO IL=1,NLAY         
                    DO IS=1,NPTS         
                     DO IT=1,NPTT
                      DO IR=1,NPTR
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                       IF (ELBUF_TAB(NG)%BUFLY(IL)%L_STRA > 0) THEN
          EVAR_TMP(1) =  LBUF%STRA(JJ(1) + I)/NPT
          EVAR_TMP(2) =  LBUF%STRA(JJ(2) + I)/NPT
          EVAR_TMP(3) =  LBUF%STRA(JJ(3) + I)/NPT
          EVAR_TMP(4) =  LBUF%STRA(JJ(4) + I)*HALF/NPT
          EVAR_TMP(5) =  LBUF%STRA(JJ(5) + I)*HALF/NPT
          EVAR_TMP(6) =  LBUF%STRA(JJ(6) + I)*HALF/NPT
              ICSIG=IPARG(17,NG)  
              IF (KCVT /= 0 .AND.ICSIG > 0) THEN
C                    STRAIN TENSOR IN GLOBAL SYSTEM
                IF (JHBE == 14) THEN
                  SELECT CASE (ICSIG)              
                  CASE (1)     
                    IF(KCVT==2 .AND. IGTYP ==22)THEN
                      GAMA(1)= ZERO
                      GAMA(2)= LBUF%GAMA(JJ(1) + I)
                      GAMA(3)= LBUF%GAMA(JJ(2) + I)
                      GAMA(4)= ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)= GAMA(1)
                    ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                      GAMA(1)= ZERO
                      GAMA(2)= GBUF%GAMA(JJ(1) + I)
                      GAMA(3)= GBUF%GAMA(JJ(2) + I)
                      GAMA(4)= ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)= GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  CASE (10)      
                    IF(KCVT==2 .AND. IGTYP ==22)THEN
                      GAMA(1)= LBUF%GAMA(JJ(1) + I)
                      GAMA(2)= LBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  CASE (100)     
                    IF(KCVT==2 .AND. IGTYP ==22)THEN
                      GAMA(1)= LBUF%GAMA(JJ(2) + I)
                      GAMA(2)= ZERO
                      GAMA(3)= LBUF%GAMA(JJ(1) + I)
                      GAMA(4)= GAMA(3)
                      GAMA(5)= ZERO
                      GAMA(6)=-GAMA(1)
                    ELSEIF(KCVT==2 .AND. IGTYP ==21)THEN
                      GAMA(1)= GBUF%GAMA(JJ(2) + I)
                      GAMA(2)= ZERO
                      GAMA(3)= GBUF%GAMA(JJ(1) + I)
                      GAMA(4)= GAMA(3)
                      GAMA(5)= ZERO
                      GAMA(6)=-GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                  END SELECT                 
                ENDIF
              ENDIF      
          EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
          EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
          EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
          EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
          EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
          EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6)
          IS_WRITTEN_TENSOR(I) = 1
                       ENDIF
                      ENDDO 
                     ENDDO
                    ENDDO
                   ENDDO
                  ENDDO
                ELSEIF(ISTRAIN > 0)THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN         
                    DO I=1,NEL
                     N = I + NFT   
                     II = 6*(I-1)
                     DO IL=1,NLAY          
                      DO IS=1,NPTS           
                       DO IT=1,NPTT
          DO IR=1,NPTR        
           LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT) 
           EVAR_TMP(1) = LBUF%STRA(JJ(1) + I)/NPT
           EVAR_TMP(2) = LBUF%STRA(JJ(2) + I)/NPT
           EVAR_TMP(3) = LBUF%STRA(JJ(3) + I)/NPT
           EVAR_TMP(4) = LBUF%STRA(JJ(4) + I)*HALF/NPT
           EVAR_TMP(5) = LBUF%STRA(JJ(5) + I)*HALF/NPT
           EVAR_TMP(6) = LBUF%STRA(JJ(6) + I)*HALF/NPT
c
               ICSIG=IPARG(17,NG)  
               IF (KCVT /= 0 .AND.ICSIG > 0) THEN
C                     STRAIN TENSOR IN GLOBAL SYSTEM
                 IF (JHBE == 14) THEN
                   SELECT CASE (ICSIG)              
                   CASE (1)      
                               IF(KCVT==2.AND.IGTYP == 22)THEN
                       GAMA(1)= ZERO
                       GAMA(2)= LBUF%GAMA(JJ(1) + I)
                       GAMA(3)= LBUF%GAMA(JJ(2) + I)
                       GAMA(4)= ZERO
                       GAMA(5)=-GAMA(2)
                       GAMA(6)= GAMA(1)
                               ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                       GAMA(1)= ZERO
                       GAMA(2)= GBUF%GAMA(JJ(1) + I)
                       GAMA(3)= GBUF%GAMA(JJ(2) + I)
                       GAMA(4)= ZERO
                       GAMA(5)=-GAMA(2)
                       GAMA(6)= GAMA(1)
                     ELSE
                       GAMA(1)=ONE
                       GAMA(2)=ZERO
                       GAMA(3)=ZERO
                       GAMA(4)=ZERO
                       GAMA(5)=ONE
                       GAMA(6)=ZERO
                     END IF
                     CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   CASE (10)       
                               IF(KCVT==2.AND.IGTYP == 22)THEN
                       GAMA(1)= LBUF%GAMA(JJ(1) + I)
                       GAMA(2)= LBUF%GAMA(JJ(2) + I)
                       GAMA(3)= ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)= GAMA(1)
                       GAMA(6)= ZERO
                               ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                       GAMA(1)= GBUF%GAMA(JJ(1) + I)
                       GAMA(2)= GBUF%GAMA(JJ(2) + I)
                       GAMA(3)= ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)= GAMA(1)
                       GAMA(6)= ZERO
                     ELSE
                       GAMA(1)=ONE
                       GAMA(2)=ZERO
                       GAMA(3)=ZERO
                       GAMA(4)=ZERO
                       GAMA(5)=ONE
                       GAMA(6)=ZERO
                     END IF
                     CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   CASE (100)     
                                IF(KCVT==2.AND.IGTYP == 22)THEN
                  GAMA(1)= LBUF%GAMA(JJ(2) + I)
                  GAMA(2)= ZERO
                  GAMA(3)= LBUF%GAMA(JJ(1) + I)
                  GAMA(4)= GAMA(3)
                  GAMA(5)= ZERO
                  GAMA(6)=-GAMA(1)
                                ELSEIF(KCVT==2.AND.IGTYP == 21)THEN
                  GAMA(1)= GBUF%GAMA(JJ(2) + I)
                  GAMA(2)= ZERO
                  GAMA(3)= GBUF%GAMA(JJ(1) + I)
                  GAMA(4)= GAMA(3)
                  GAMA(5)= ZERO
                  GAMA(6)=-GAMA(1)
                ELSE
                  GAMA(1)=ONE
                  GAMA(2)=ZERO
                  GAMA(3)=ZERO
                  GAMA(4)=ZERO
                  GAMA(5)=ONE
                  GAMA(6)=ZERO
                END IF
                CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   END SELECT 
                 ENDIF
               ENDIF       
           EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
           EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
           EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
           EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
           EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
           EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6)
           IS_WRITTEN_TENSOR(I) = 1
          ENDDO 
                       ENDDO
                      ENDDO
                     ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c---
                ICSIG=IPARG(17,NG) 
                IF (KCVT /= 0 .AND.ICSIG == 0 .AND. JHBE /= 16) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL               
                    N = I + NFT              
                    IF(KCVT==2)THEN           
                      II = 6*(I-1)           
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE             
                      GAMA(1)=ONE           
                      GAMA(2)=ZERO           
                      GAMA(3)=ZERO           
                      GAMA(4)=ZERO           
                      GAMA(5)=ONE           
                      GAMA(6)=ZERO           
                    END IF             
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO 
                ENDIF
c-----------
              ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
                IF (MLW>=28.AND.MLW /= 49)THEN 
                  DO I=1,NEL
                    N = I + NFT  
                    II = 6*(I-1)         
                    DO IPT=1,NPT
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
                    N = I + NFT  
                    II = 3*(I-1)         
                    DO IPT=1,NPT
           LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
           EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/NPT
           EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT
           EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/NPT 
           IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO    
                ELSEIF ((MLW == 24 .OR. MLW == 25) .and. ISTRAIN > 0) THEN
                  DO I=1,NEL
                    N = I + NFT  
                    II = 6*(I-1)         
                    DO IPT=1,NPT
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                      IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                  ENDDO
                ELSEIF(ISTRAIN > 0)THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN
                    DO I=1,NEL
                      N = I + NFT  
                      II = 6*(I-1)           
                      DO IPT=1,NPT
          LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)       
          EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
          EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
          EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
          EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
          EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
          EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                    ENDDO        
                  ENDIF 
                ENDIF 
                IF (KCVT /= 0) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL          
                    N = I + NFT             
                    IF (KCVT==2) THEN          
                      II = 6*(I-1)          
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=GBUF%GAMA(JJ(3) + I)
                      GAMA(4)=GBUF%GAMA(JJ(4) + I)
                      GAMA(5)=GBUF%GAMA(JJ(5) + I)
                      GAMA(6)=GBUF%GAMA(JJ(6) + I)
                    ELSE          
                      GAMA(1)=ONE        
                      GAMA(2)=ZERO        
                      GAMA(3)=ZERO        
                      GAMA(4)=ZERO        
                      GAMA(5)=ONE        
                      GAMA(6)=ZERO        
                    ENDIF         
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO             
                ENDIF
c-----------
              ELSEIF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
c-----------
                IF (MLW>=28.AND.MLW /= 49.AND.ISTRAIN > 0) THEN
                  DO I=1,NEL               
                    N = I + NFT                
                    II = 6*(I-1)               
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG                 
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)  
          EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)       
          EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)       
          EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)       
          EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                      HALF/(NPTG*NLAY)       
          EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                      HALF/(NPTG*NLAY)       
          EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                      HALF/(NPTG*NLAY) 
          IS_WRITTEN_TENSOR(I) = 1  
                      ENDDO                   
                    ENDDO                 
                  ENDDO                  
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=1,NEL
                    II = 3*(I-1)         
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)
          EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/(NPTG*NLAY)
          EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/(NPTG*NLAY)
          EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/(NPTG*NLAY)
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO
                    ENDDO
                  ENDDO    
                ELSEIF ((MLW == 24 .OR. MLW == 25) .and. ISTRAIN > 0)THEN
                  DO I=1,NEL               
                    N = I + NFT                
                    II = 6*(I-1)               
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG                 
          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)  
          EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)
          EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)
          EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)
          EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                  HALF/(NPTG*NLAY)      
          EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                  HALF/(NPTG*NLAY) 
          EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                  HALF/(NPTG*NLAY)   
          IS_WRITTEN_TENSOR(I) = 1
                      ENDDO                   
                    ENDDO                 
                  ENDDO                  
                ELSEIF (ISTRAIN > 0) THEN           
                  IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN     
                    DO I=1,NEL                 
                      N = I + NFT                 
                      II = 6*(I-1)                 
                      DO IL= 1,NLAY
          DO IPT=1,NPTG             
           LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1) 
            EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)       
            EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)      
            EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)    
            EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                  HALF/(NPTG*NLAY)
            EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                  HALF/(NPTG*NLAY)
            EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                  HALF/(NPTG*NLAY)
            IS_WRITTEN_TENSOR(I) = 1
          ENDDO               
                      ENDDO                   
                    ENDDO                 
                  ENDIF                    
                ENDIF             
                IF (KCVT /= 0) THEN
C                 STRAIN TENSOR IN GLOBAL SYSTEM
                  DO I=1,NEL            
                    N = I + NFT         
                    IF (KCVT==2)THEN           
                      II = 6*(I-1)                
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)  
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)  
                      GAMA(3)= ZERO           
                      GAMA(4)=-GAMA(2)           
                      GAMA(5)= GAMA(1)           
                      GAMA(6)= ZERO           
                    ELSE             
                      GAMA(1)=ONE           
                      GAMA(2)=ZERO           
                      GAMA(3)=ZERO           
                      GAMA(4)=ZERO           
                      GAMA(5)=ONE           
                      GAMA(6)=ZERO           
                    END IF             
                    CALL SROTA6(
     1   X,        IXS(1,N), KCVT,     EVAR(1,I),
     2   GAMA,     JHBE,     IGTYP,    ISORTH)
                  ENDDO 
                ENDIF
c-----------
              ENDIF  ! ISOLNOD & ......
C---- Corner Data :
            CALL STRN_TENSCOR3(ELBUF_TAB(NG),IPARG(1,NG),IXS      ,IXS10   ,X        ,
     .                         PM        ,KCVT ,NEL ,EVAR_CORNER )
            IF (ISOLNOD <= 10) IS_WRITTEN_TENSOR(1:NEL) = 1
C--------------------------------------------------
c           back stress tensor
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TENS/BSTRESS') THEN  
C--------------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
C--------------------------------------------------
            IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN ! mean values since II not precised
              ! not for thick shells
             !--------
             !  LAW36
             !--------
             IF(MLW == 36 .AND. ( ID == -1 .OR. ID == 1)) THEN
               DO I=1,NEL
                DO IR=1,NPTR                    
                  DO IS=1,NPTS            
                    DO IT=1,NPTT           
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    EVAR(1,I) = EVAR(1,I) + LBUF%SIGB(JJ(1) + I)/NPTG
                    EVAR(2,I) = EVAR(2,I) + LBUF%SIGB(JJ(2) + I)/NPTG
                    EVAR(3,I) = EVAR(3,I) + LBUF%SIGB(JJ(3) + I)/NPTG
                    EVAR(4,I) = EVAR(4,I) + LBUF%SIGB(JJ(4) + I)/NPTG
                    EVAR(5,I) = EVAR(5,I) + LBUF%SIGB(JJ(5) + I)/NPTG
                    EVAR(6,I) = EVAR(6,I) + LBUF%SIGB(JJ(6) + I)/NPTG
                    ENDDO !IT                   
                  ENDDO !IS                
                ENDDO !IR                  
               ENDDO 
               !!IF (KCVT /= 0 .AND. JHBE /= 16) THEN ????
             !--------                                                                                               
             !  LAW78                                                                                                
             !--------                                                                                               
             ELSEIF (MLW == 78) THEN                                                                                 
               IF(ID == -1) THEN ! sum of all backstresses                                                         
                 DO I=1,NEL
                  DO IR=1,NPTR                   
                   DO IS=1,NPTS            
                    DO IT=1,NPTT           
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    EVAR(1,I) = EVAR(1,I) +( LBUF%SIGA(JJ(1) + I) + LBUF%SIGB(JJ(1) + I) )/NPTG
                    EVAR(2,I) = EVAR(2,I) +( LBUF%SIGA(JJ(2) + I) + LBUF%SIGB(JJ(2) + I) )/NPTG
                    EVAR(3,I) = EVAR(3,I) +( LBUF%SIGA(JJ(3) + I) + LBUF%SIGB(JJ(3) + I) )/NPTG
                    EVAR(4,I) = EVAR(4,I) +( LBUF%SIGA(JJ(4) + I) + LBUF%SIGB(JJ(4) + I) )/NPTG
                    EVAR(5,I) = EVAR(5,I) +( LBUF%SIGA(JJ(5) + I) + LBUF%SIGB(JJ(5) + I) )/NPTG
                    EVAR(6,I) = EVAR(6,I) +( LBUF%SIGA(JJ(6) + I) + LBUF%SIGB(JJ(6) + I) )/NPTG
                    ENDDO !IT                   
                   ENDDO !IS                
                  ENDDO !IR                  
                 ENDDO 

               ELSEIF(ID ==1 ) THEN                                                                                  
                 DO I=1,NEL
                  DO IR=1,NPTR                   
                   DO IS=1,NPTS            
                    DO IT=1,NPTT           
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    EVAR(1,I) = EVAR(1,I) + LBUF%SIGA(JJ(1) + I) /NPTG
                    EVAR(2,I) = EVAR(2,I) + LBUF%SIGA(JJ(2) + I) /NPTG
                    EVAR(3,I) = EVAR(3,I) + LBUF%SIGA(JJ(3) + I) /NPTG
                    EVAR(4,I) = EVAR(4,I) + LBUF%SIGA(JJ(4) + I) /NPTG
                    EVAR(5,I) = EVAR(5,I) + LBUF%SIGA(JJ(5) + I) /NPTG
                    EVAR(6,I) = EVAR(6,I) + LBUF%SIGA(JJ(6) + I) /NPTG
                    ENDDO !IT                   
                   ENDDO !IS                
                  ENDDO !IR                  
                 ENDDO 

               ELSEIF(ID ==2 ) THEN                                                                                  
                 DO I=1,NEL
                  DO IR=1,NPTR                   
                   DO IS=1,NPTS            
                    DO IT=1,NPTT           
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    EVAR(1,I) = EVAR(1,I) + LBUF%SIGB(JJ(1) + I) /NPTG
                    EVAR(2,I) = EVAR(2,I) + LBUF%SIGB(JJ(2) + I) /NPTG
                    EVAR(3,I) = EVAR(3,I) + LBUF%SIGB(JJ(3) + I) /NPTG
                    EVAR(4,I) = EVAR(4,I) + LBUF%SIGB(JJ(4) + I) /NPTG
                    EVAR(5,I) = EVAR(5,I) + LBUF%SIGB(JJ(5) + I) /NPTG
                    EVAR(6,I) = EVAR(6,I) + LBUF%SIGB(JJ(6) + I) /NPTG
                    ENDDO !IT                   
                   ENDDO !IS                
                  ENDDO !IR                  
                 ENDDO 
               ELSEIF(ID ==3 ) THEN                                                                                  
                 DO I=1,NEL
                  DO IR=1,NPTR                   
                   DO IS=1,NPTS            
                    DO IT=1,NPTT           
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    EVAR(1,I) = EVAR(1,I) + LBUF%SIGC(JJ(1) + I) /NPTG
                    EVAR(2,I) = EVAR(2,I) + LBUF%SIGC(JJ(2) + I) /NPTG
                    EVAR(3,I) = EVAR(3,I) + LBUF%SIGC(JJ(3) + I) /NPTG
                    EVAR(4,I) = EVAR(4,I) + LBUF%SIGC(JJ(4) + I) /NPTG
                    EVAR(5,I) = EVAR(5,I) + LBUF%SIGC(JJ(5) + I) /NPTG
                    EVAR(6,I) = EVAR(6,I) + LBUF%SIGC(JJ(6) + I) /NPTG
                    ENDDO !IT                   
                   ENDDO !IS                
                  ENDDO !IR                  
                 ENDDO 
               ENDIF !ID == -1                                                                                       
             ENDIF !(MLW ==                                                                                          
C--------------------------------------------                                                                  
c IR= IS= IT                                                                                                    =
C--------------------------------------------------
            ELSEIF ( ILAY == -1 .AND. IR >  0 .AND.  IR <= NPTR .AND. 
     .                                IS >  0 .AND.  IS <= NPTS .AND. 
     .                                IT >  0 .AND.  IT <= NPTT) THEN 
                    
              DO I=1,NEL              
                EVAR(1,I) = ZERO      
                EVAR(2,I) = ZERO      
                EVAR(3,I) = ZERO      
                EVAR(4,I) = ZERO      
                EVAR(5,I) = ZERO      
                EVAR(6,I) = ZERO      
              ENDDO
              IF(ISOLNOD == 10.OR.(ISOLNOD == 4 .AND. ISROT == 1))THEN
                  IPT = 0
                  IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
                  IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
                  IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
                  IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
                  IF (IPT > 0)  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)     
              ELSEIF (ISOLNOD == 8 .AND. (JHBE == 14.OR.JHBE == 17) )THEN !idem stress
                ICSIG = IPARG(17,NG)
                NPTG = NPTR * NPTS * NPTT * NLAY
                IPID = IXS(10,1 + NFT) 
            IF (IOR_TSH >0) THEN
                    IF (ICSIG == 10) THEN
                  IR=IT_INPUT
                  IS=IR_INPUT
                  IT=IS_INPUT
                    ELSEIF (ICSIG == 1) THEN
                      IR=IS_INPUT
                      IS=IT_INPUT
                      IT=IR_INPUT
                ELSE
                  IR=IR_INPUT
                  IS=IS_INPUT
                  IT=IT_INPUT
                ENDIF
            ENDIF
                IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR                                                                                   
                IF(IR <= NPTR .AND. IS <= NPTS .AND. IT <= NPTT) THEN    
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                ELSE         
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                               
                ENDIF                                                                        
              ELSE                                    
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)       
              ENDIF !ISOLNOD
              !--------                                                                 
              !  LAW36                                                                  
              !--------                                                                 
              IF(MLW == 36 .AND. ( ID == -1 .OR. ID == 1)) THEN                         
                DO I=1,NEL                                                              
                EVAR(1,I) = LBUF%SIGB(JJ(1) + I)                                     
                EVAR(2,I) = LBUF%SIGB(JJ(2) + I)                                     
                EVAR(3,I) = LBUF%SIGB(JJ(3) + I)                                     
                EVAR(4,I) = LBUF%SIGB(JJ(4) + I)                                     
                EVAR(5,I) = LBUF%SIGB(JJ(5) + I)                                     
                EVAR(6,I) = LBUF%SIGB(JJ(6) + I)                                     
                  IS_WRITTEN_TENSOR(I) = 1                                             
                ENDDO                                                                   
              !--------                                                                                               
              !  LAW78                                                                                                
              !--------                                                                                               
              ELSEIF (MLW == 78) THEN                                                                                 
                IF(ID == -1) THEN ! somme of all backstresses                                                         
                  DO I=1,NEL                                                             
                  EVAR(1,I) = ( LBUF%SIGA(JJ(1) + I) + LBUF%SIGB(JJ(1) + I) )          
                  EVAR(2,I) = ( LBUF%SIGA(JJ(2) + I) + LBUF%SIGB(JJ(2) + I) )          
                  EVAR(3,I) = ( LBUF%SIGA(JJ(3) + I) + LBUF%SIGB(JJ(3) + I) )          
                  EVAR(4,I) = ( LBUF%SIGA(JJ(4) + I) + LBUF%SIGB(JJ(4) + I) )          
                  EVAR(5,I) = ( LBUF%SIGA(JJ(5) + I) + LBUF%SIGB(JJ(5) + I) )          
                  EVAR(6,I) = ( LBUF%SIGA(JJ(6) + I) + LBUF%SIGB(JJ(6) + I) )          
                    IS_WRITTEN_TENSOR(I) = 1                                             
                  ENDDO                                                                  
                ELSEIF(ID ==1 ) THEN                                                                                  
                  DO I=1,NEL                                                             
                  EVAR(1,I) = LBUF%SIGA(JJ(1) + I)                                     
                  EVAR(2,I) = LBUF%SIGA(JJ(2) + I)                                     
                  EVAR(3,I) = LBUF%SIGA(JJ(3) + I)                                     
                  EVAR(4,I) = LBUF%SIGA(JJ(4) + I)                                     
                  EVAR(5,I) = LBUF%SIGA(JJ(5) + I)                                     
                  EVAR(6,I) = LBUF%SIGA(JJ(6) + I)                                     
                    IS_WRITTEN_TENSOR(I) = 1                                             
                  ENDDO                                                                  
                ELSEIF(ID ==2 ) THEN                                                                                  
                  DO I=1,NEL                                                             
                  EVAR(1,I) =  LBUF%SIGB(JJ(1) + I)                                    
                  EVAR(2,I) =  LBUF%SIGB(JJ(2) + I)                                    
                  EVAR(3,I) =  LBUF%SIGB(JJ(3) + I)                                    
                  EVAR(4,I) =  LBUF%SIGB(JJ(4) + I)                                    
                  EVAR(5,I) =  LBUF%SIGB(JJ(5) + I)                                    
                  EVAR(6,I) =  LBUF%SIGB(JJ(6) + I)                                    
                    IS_WRITTEN_TENSOR(I) = 1                                             
                  ENDDO                                                                  
                ELSEIF(ID ==3 ) THEN                                                                     
                  DO I=1,NEL                                                             
                  EVAR(1,I) =  LBUF%SIGC(JJ(1) + I)                                    
                  EVAR(2,I) =  LBUF%SIGC(JJ(2) + I)                                    
                  EVAR(3,I) =  LBUF%SIGC(JJ(3) + I)                                    
                  EVAR(4,I) =  LBUF%SIGC(JJ(4) + I)                                    
                  EVAR(5,I) =  LBUF%SIGC(JJ(5) + I)                                    
                  EVAR(6,I) =  LBUF%SIGC(JJ(6) + I)                                    
                    IS_WRITTEN_TENSOR(I) = 1                                             
                  ENDDO                                                                  
                ENDIF !ID == -1                                                                                       
              ENDIF !(MLW ==                                                                                          
C--------------------------------------------------
CiLAY = ir= is=  
C--------------------------------------------------
            ELSEIF ( ILAY >  0 .AND. ILAY <= NLAY  .AND.
     .                 IR >  0 .AND. IR   <= NPTR  .AND. 
     .                 IS >  0 .AND. IS   <= NPTS) THEN
C--------------------------------------------------
              EVAR(1:6,1:NEL) = ZERO
              LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1) 
              IF (TSHELL == 1 ) THEN                                                    
                LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)                        
                IF (ISOLNOD == 16) LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,1,IT)     !! 
              ENDIF
c-----------
              !--------                                                                                                
              !  LAW36                                                                                                 
              !--------                                                                                                
              IF(MLW == 36 .AND. ( ID == -1 .OR. ID == 1)) THEN                                                        
                DO I=1,NEL                                                                                             
                EVAR(1,I) = LBUF%SIGB(JJ(1) + I)                                                                    
                EVAR(2,I) = LBUF%SIGB(JJ(2) + I)                                                                    
                EVAR(3,I) = LBUF%SIGB(JJ(3) + I)                                                                    
                EVAR(4,I) = LBUF%SIGB(JJ(4) + I)                                                                    
                EVAR(5,I) = LBUF%SIGB(JJ(5) + I)                                                                    
                EVAR(6,I) = LBUF%SIGB(JJ(6) + I)                                                                    
                  IS_WRITTEN_TENSOR(I) = 1                                                                            
                ENDDO                                                                                                  
              !--------                                                                                                
              !  LAW78                                                                                                 
              !--------                                                                                                
              ELSEIF (MLW == 78) THEN                                                                                  
                IF(ID == -1) THEN ! somme of all backstresses                                                          
                  DO I=1,NEL                                                                                            
                  EVAR(1,I) = ( LBUF%SIGA(JJ(1) + I) + LBUF%SIGB(JJ(1) + I) )                                         
                  EVAR(2,I) = ( LBUF%SIGA(JJ(2) + I) + LBUF%SIGB(JJ(2) + I) )                                         
                  EVAR(3,I) = ( LBUF%SIGA(JJ(3) + I) + LBUF%SIGB(JJ(3) + I) )                                         
                  EVAR(4,I) = ( LBUF%SIGA(JJ(4) + I) + LBUF%SIGB(JJ(4) + I) )                                         
                  EVAR(5,I) = ( LBUF%SIGA(JJ(5) + I) + LBUF%SIGB(JJ(5) + I) )                                         
                  EVAR(6,I) = ( LBUF%SIGA(JJ(6) + I) + LBUF%SIGB(JJ(6) + I) )                                         
                    IS_WRITTEN_TENSOR(I) = 1                                                                            
                  ENDDO                                                                                                 
                ELSEIF(ID ==1 ) THEN                                                                                   
                  DO I=1,NEL                                                                                            
                  EVAR(1,I) = LBUF%SIGA(JJ(1) + I)                                                                    
                  EVAR(2,I) = LBUF%SIGA(JJ(2) + I)                                                                    
                  EVAR(3,I) = LBUF%SIGA(JJ(3) + I)                                                                    
                  EVAR(4,I) = LBUF%SIGA(JJ(4) + I)                                                                    
                  EVAR(5,I) = LBUF%SIGA(JJ(5) + I)                                                                    
                  EVAR(6,I) = LBUF%SIGA(JJ(6) + I)                                                                    
                    IS_WRITTEN_TENSOR(I) = 1                                                                            
                  ENDDO                                                                                                 
                ELSEIF(ID ==2 ) THEN                                                                                   
                  DO I=1,NEL                                                                                            
                  EVAR(1,I) =  LBUF%SIGB(JJ(1) + I)                                                                   
                  EVAR(2,I) =  LBUF%SIGB(JJ(2) + I)                                                                   
                  EVAR(3,I) =  LBUF%SIGB(JJ(3) + I)                                                                   
                  EVAR(4,I) =  LBUF%SIGB(JJ(4) + I)                                                                   
                  EVAR(5,I) =  LBUF%SIGB(JJ(5) + I)                                                                   
                  EVAR(6,I) =  LBUF%SIGB(JJ(6) + I)                                                                   
                    IS_WRITTEN_TENSOR(I) = 1                                                                            
                  ENDDO                                                                                                 
                ELSEIF(ID ==3 ) THEN                                                                                   
                  DO I=1,NEL                                                                                            
                  EVAR(1,I) =  LBUF%SIGC(JJ(1) + I)                                                                   
                  EVAR(2,I) =  LBUF%SIGC(JJ(2) + I)                                                                   
                  EVAR(3,I) =  LBUF%SIGC(JJ(3) + I)                                                                   
                  EVAR(4,I) =  LBUF%SIGC(JJ(4) + I)                                                                   
                  EVAR(5,I) =  LBUF%SIGC(JJ(5) + I)                                                                   
                  EVAR(6,I) =  LBUF%SIGC(JJ(6) + I)                                                                   
                    IS_WRITTEN_TENSOR(I) = 1                                                                            
                  ENDDO                                                                                                 
                ENDIF !ID == -1                                                                                        
              ENDIF !(MLW ==                                                                                           

c-----------
c ILAY=  
c-----------
            ELSEIF ( ILAY >= 0 .AND. ILAY <= NLAY) THEN 
c-----------
              EVAR(1:6,1:NEL) = ZERO
              IF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
                IPT = IS                                           
                IF ( ILAY <= NPT) THEN                             
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)   
                  IF(MLW == 36 .AND. ( ID == -1 .OR. ID == 1)) THEN                         
                     DO I=1,NEL                                                            
                      EVAR(1,I) = LBUF%SIGB(JJ(1) + I)                                   
                      EVAR(2,I) = LBUF%SIGB(JJ(2) + I)                                   
                      EVAR(3,I) = LBUF%SIGB(JJ(3) + I)                                   
                      EVAR(4,I) = LBUF%SIGB(JJ(4) + I)                                   
                      EVAR(5,I) = LBUF%SIGB(JJ(5) + I)                                   
                      EVAR(6,I) = LBUF%SIGB(JJ(6) + I)                                   
                        IS_WRITTEN_TENSOR(I) = 1                                           
                     ENDDO                                                                 
                  !--------                                                                                            
                  !  LAW78                                                                                             
                  !--------                                                                                            
                  ELSEIF (MLW == 78) THEN                                                                              
                    IF(ID == -1) THEN ! somme of all backstresses                                                     
                     DO I=1,NEL                                                             
                     EVAR(1,I) = ( LBUF%SIGA(JJ(1) + I) + LBUF%SIGB(JJ(1) + I) )          
                     EVAR(2,I) = ( LBUF%SIGA(JJ(2) + I) + LBUF%SIGB(JJ(2) + I) )          
                     EVAR(3,I) = ( LBUF%SIGA(JJ(3) + I) + LBUF%SIGB(JJ(3) + I) )          
                     EVAR(4,I) = ( LBUF%SIGA(JJ(4) + I) + LBUF%SIGB(JJ(4) + I) )          
                     EVAR(5,I) = ( LBUF%SIGA(JJ(5) + I) + LBUF%SIGB(JJ(5) + I) )          
                     EVAR(6,I) = ( LBUF%SIGA(JJ(6) + I) + LBUF%SIGB(JJ(6) + I) )          
                       IS_WRITTEN_TENSOR(I) = 1                                             
                     ENDDO                                                                  
                    ELSEIF(ID ==1 ) THEN                                                                              
                     DO I=1,NEL                                                             
                     EVAR(1,I) = LBUF%SIGA(JJ(1) + I)                                     
                     EVAR(2,I) = LBUF%SIGA(JJ(2) + I)                                     
                     EVAR(3,I) = LBUF%SIGA(JJ(3) + I)                                     
                     EVAR(4,I) = LBUF%SIGA(JJ(4) + I)                                     
                     EVAR(5,I) = LBUF%SIGA(JJ(5) + I)                                     
                     EVAR(6,I) = LBUF%SIGA(JJ(6) + I)                                     
                       IS_WRITTEN_TENSOR(I) = 1                                             
                     ENDDO                                                                  

                    ELSEIF(ID ==2 ) THEN                                                                              
                     DO I=1,NEL                                                             
                     EVAR(1,I) =  LBUF%SIGB(JJ(1) + I)                                    
                     EVAR(2,I) =  LBUF%SIGB(JJ(2) + I)                                    
                     EVAR(3,I) =  LBUF%SIGB(JJ(3) + I)                                    
                     EVAR(4,I) =  LBUF%SIGB(JJ(4) + I)                                    
                     EVAR(5,I) =  LBUF%SIGB(JJ(5) + I)                                    
                     EVAR(6,I) =  LBUF%SIGB(JJ(6) + I)                                    
                       IS_WRITTEN_TENSOR(I) = 1                                             
                     ENDDO                                                                  
                    ELSEIF(ID ==3 ) THEN                                                                 
                     DO I=1,NEL                                                             
                     EVAR(1,I) =  LBUF%SIGC(JJ(1) + I)                                    
                     EVAR(2,I) =  LBUF%SIGC(JJ(2) + I)                                    
                     EVAR(3,I) =  LBUF%SIGC(JJ(3) + I)                                    
                     EVAR(4,I) =  LBUF%SIGC(JJ(4) + I)                                    
                     EVAR(5,I) =  LBUF%SIGC(JJ(5) + I)                                    
                     EVAR(6,I) =  LBUF%SIGC(JJ(6) + I)                                    
                       IS_WRITTEN_TENSOR(I) = 1                                             
                     ENDDO                                                                  
                    ENDIF !ID == -1                                                                                   
                  ENDIF !(MLW ==                                                                                          
            
                ENDIF !( ILAY <= NPT)                              
              ENDIF!((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)  
            END IF
c ........
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRESS/TMAX') THEN
C-----------------------------------------------
             DO I=1,NEL
               EVAR(1:6,I) = GBUF%TM_SIG1(JJ(1:6) + I)
               IS_WRITTEN_TENSOR(I) = 1
             ENDDO
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRESS/TMIN') THEN
C-----------------------------------------------
             DO I=1,NEL
               EVAR(1:6,I) = GBUF%TM_SIG3(JJ(1:6) + I)
               IS_WRITTEN_TENSOR(I) = 1
             ENDDO
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN/TMAX') THEN
C-----------------------------------------------
             DO I=1,NEL
               EVAR(1:6,I) = GBUF%TM_STRA1(JJ(1:6) + I)
               IS_WRITTEN_TENSOR(I) = 1
             ENDDO
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN/TMIN') THEN
C-----------------------------------------------
             DO I=1,NEL
               EVAR(1:6,I) = GBUF%TM_STRA3(JJ(1:6) + I)
               IS_WRITTEN_TENSOR(I) = 1
             ENDDO
          ELSE
C-----------------------------------------------
C          
C-----------------------------------------------
            DO I=1,NEL
              N = I + NFT
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO       
            ENDDO
          ENDIF

          CALL H3D_WRITE_TENSOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_TENSOR,NEL,0,NFT,
     .                                    EVAR,IS_WRITTEN_TENSOR)

          IF (IS_CORNER_DATA == 1)CALL H3D_WRITE_TENSOR_CORNER(IOK_PART,IS_WRITTEN_SOLID,
     .                 SOLID_TENSOR_CORNER,NEL,0,NFT,EVAR_CORNER,MAXNNOD,IS_WRITTEN_TENSOR)
C
c-----------
         ISORTHG = ISORTH   ! pour precaution
C-----------------------------------------------
        ELSE
c
        ENDIF
C           
        ENDIF ! mlw /= 13 
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  T4_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|        T4DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|====================================================================
        SUBROUTINE T4_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  4*NEL  | R | R | X-coordinate ARRAY (4n tetra)
C YN      |  4*NEL  | R | R | Y-coordinate ARRAY (4n tetra)
C ZN      |  4*NEL  | R | R | Z-coordinate ARRAY (4n tetra)
C DX      |  4*NEL  | R | R | X-Displ ARRAY (4n tetra)
C DY      |  4*NEL  | R | R | Y-Displ ARRAY (4n tetra)
C DZ      |  4*NEL  | R | R | D-Displ ARRAY (4n tetra)
C STRAIN  |  6*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(NEL,4) , YN(NEL,4) , ZN(NEL,4), 
     .   DX(NEL,4) , DY(NEL,4) , DZ(NEL,4),STRAIN(6,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 4)
      my_real 
     .   X0N(NEL,NNOD) , Y0N(NEL,NNOD) , Z0N(NEL,NNOD), 
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL),
     .   VOL(NEL),F(3,3,NEL)
C----------------------------------------------
C------initial configuration :
        DO I=1,NEL
          X0N(I,1:NNOD) = XN(I,1:NNOD)-DX(I,1:NNOD)
          Y0N(I,1:NNOD) = YN(I,1:NNOD)-DY(I,1:NNOD)
          Z0N(I,1:NNOD) = ZN(I,1:NNOD)-DZ(I,1:NNOD)
        END DO
        CALL T4DERI3(
     .   X0N(1,1),X0N(1,2),X0N(1,3),X0N(1,4),
     .   Y0N(1,1),Y0N(1,2),Y0N(1,3),Y0N(1,4), 
     .   Z0N(1,1),Z0N(1,2),Z0N(1,3),Z0N(1,4),
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, 
     .   VOL  ,NEL )
C---------------
C  MEMBRANE [F]-1 
C---------------
       DO I=1,NEL
         F(1,1,I)=PX1(I)*DX(I,1)+PX2(I)*DX(I,2)+PX3(I)*DX(I,3)+PX4(I)*DX(I,4)
         F(2,2,I)=PY1(I)*DY(I,1)+PY2(I)*DY(I,2)+PY3(I)*DY(I,3)+PY4(I)*DY(I,4)
         F(3,3,I)=PZ1(I)*DZ(I,1)+PZ2(I)*DZ(I,2)+PZ3(I)*DZ(I,3)+PZ4(I)*DZ(I,4)
         F(1,2,I)=PY1(I)*DX(I,1)+PY2(I)*DX(I,2)+PY3(I)*DX(I,3)+PY4(I)*DX(I,4)
         F(1,3,I)=PZ1(I)*DX(I,1)+PZ2(I)*DX(I,2)+PZ3(I)*DX(I,3)+PZ4(I)*DX(I,4)
         F(2,1,I)=PX1(I)*DY(I,1)+PX2(I)*DY(I,2)+PX3(I)*DY(I,3)+PX4(I)*DY(I,4)
         F(2,3,I)=PZ1(I)*DY(I,1)+PZ2(I)*DY(I,2)+PZ3(I)*DY(I,3)+PZ4(I)*DY(I,4)
         F(3,1,I)=PX1(I)*DZ(I,1)+PX2(I)*DZ(I,2)+PX3(I)*DZ(I,3)+PX4(I)*DZ(I,4)
         F(3,2,I)=PY1(I)*DZ(I,1)+PY2(I)*DZ(I,2)+PY3(I)*DZ(I,3)+PY4(I)*DZ(I,4)
       END DO
C---------------
C  MEMBRANE [e]=[U]-1 
C---------------
       CALL U_FROM_F3(F,STRAIN,NEL)
C
      RETURN
      END
Chd|====================================================================
Chd|  S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|        T8DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|====================================================================
        SUBROUTINE S8_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  8*NEL  | R | R | X-coordinate ARRAY (8n Hexa)
C YN      |  8*NEL  | R | R | Y-coordinate ARRAY (8n Hexa)
C ZN      |  8*NEL  | R | R | Z-coordinate ARRAY (8n Hexa)
C DX      |  8*NEL  | R | R | X-Displ ARRAY (8n Hexa)
C DY      |  8*NEL  | R | R | Y-Displ ARRAY (8n Hexa)
C DZ      |  8*NEL  | R | R | D-Displ ARRAY (8n Hexa)
C STRAIN  |  6*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(NEL,8) , YN(NEL,8) , ZN(NEL,8), 
     .   DX(NEL,8) , DY(NEL,8) , DZ(NEL,8),STRAIN(6,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 8)
      my_real 
     .   X0N(NEL,NNOD) , Y0N(NEL,NNOD) , Z0N(NEL,NNOD), 
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL),
     .   VOL(NEL),F(3,3,NEL),
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46
C----------------------------------------------
C------initial configuration :
        DO I=1,NEL
           X0N(I,1:NNOD) = XN(I,1:NNOD)-DX(I,1:NNOD)
           Y0N(I,1:NNOD) = YN(I,1:NNOD)-DY(I,1:NNOD)
           Z0N(I,1:NNOD) = ZN(I,1:NNOD)-DZ(I,1:NNOD)
        END DO
        CALL T8DERI3(
     .   X0N(1,1),X0N(1,2),X0N(1,3),X0N(1,4),
     .   X0N(1,5),X0N(1,6),X0N(1,7),X0N(1,8),
     .   Y0N(1,1),Y0N(1,2),Y0N(1,3),Y0N(1,4), 
     .   Y0N(1,5),Y0N(1,6),Y0N(1,7),Y0N(1,8), 
     .   Z0N(1,1),Z0N(1,2),Z0N(1,3),Z0N(1,4),
     .   Z0N(1,5),Z0N(1,6),Z0N(1,7),Z0N(1,8),
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, 
     .   VOL  ,NEL )
C---------------
C  MEMBRANE [F]-[1] 
C---------------
       DO I=1,NEL
         VX17=DX(I,1)-DX(I,7)
         VX28=DX(I,2)-DX(I,8)
         VX35=DX(I,3)-DX(I,5)
         VX46=DX(I,4)-DX(I,6)
         VY17=DY(I,1)-DY(I,7)
         VY28=DY(I,2)-DY(I,8)
         VY35=DY(I,3)-DY(I,5)
         VY46=DY(I,4)-DY(I,6)
         VZ17=DZ(I,1)-DZ(I,7)
         VZ28=DZ(I,2)-DZ(I,8)
         VZ35=DZ(I,3)-DZ(I,5)
         VZ46=DZ(I,4)-DZ(I,6)
C
         F(1,1,I)=PX1(I)*VX17+PX2(I)*VX28+PX3(I)*VX35+PX4(I)*VX46
         F(2,2,I)=PY1(I)*VY17+PY2(I)*VY28+PY3(I)*VY35+PY4(I)*VY46
         F(3,3,I)=PZ1(I)*VZ17+PZ2(I)*VZ28+PZ3(I)*VZ35+PZ4(I)*VZ46
         F(1,2,I)=PY1(I)*VX17+PY2(I)*VX28+PY3(I)*VX35+PY4(I)*VX46
         F(1,3,I)=PZ1(I)*VX17+PZ2(I)*VX28+PZ3(I)*VX35+PZ4(I)*VX46
         F(2,1,I)=PX1(I)*VY17+PX2(I)*VY28+PX3(I)*VY35+PX4(I)*VY46
         F(2,3,I)=PZ1(I)*VY17+PZ2(I)*VY28+PZ3(I)*VY35+PZ4(I)*VY46
         F(3,1,I)=PX1(I)*VZ17+PX2(I)*VZ28+PX3(I)*VZ35+PX4(I)*VZ46
         F(3,2,I)=PY1(I)*VZ17+PY2(I)*VZ28+PY3(I)*VZ35+PY4(I)*VZ46
         
       END DO
C---------------
C  MEMBRANE [e]=[U]-1 
C---------------
       CALL U_FROM_F3(F,STRAIN,NEL)
C
      RETURN
      END
Chd|====================================================================
Chd|  S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|        T6DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|====================================================================
        SUBROUTINE S6_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  6*NEL  | R | R | X-coordinate ARRAY (6n Penta)
C YN      |  6*NEL  | R | R | Y-coordinate ARRAY (6n Penta)
C ZN      |  6*NEL  | R | R | Z-coordinate ARRAY (6n Penta)
C DX      |  6*NEL  | R | R | X-Displ ARRAY (6n Penta)
C DY      |  6*NEL  | R | R | Y-Displ ARRAY (6n Penta)
C DZ      |  6*NEL  | R | R | D-Displ ARRAY (6n Penta)
C STRAIN  |  6*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(NEL,6) , YN(NEL,6) , ZN(NEL,6), 
     .   DX(NEL,6) , DY(NEL,6) , DZ(NEL,6),STRAIN(6,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 6)
      my_real 
     .   X0N(NEL,NNOD) , Y0N(NEL,NNOD) , Z0N(NEL,NNOD), 
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL),
     .   VOL(NEL),F(3,3,NEL)
      my_real
     .   VX14, VY14, VZ14,
     .   VX25, VY25, VZ25,
     .   VX36, VY36, VZ36,
     .   VXHI, VYHI, VZHI
C----------------------------------------------
C------initial configuration :
        DO I=1,NEL
           X0N(I,1:NNOD) = XN(I,1:NNOD)-DX(I,1:NNOD)
           Y0N(I,1:NNOD) = YN(I,1:NNOD)-DY(I,1:NNOD)
           Z0N(I,1:NNOD) = ZN(I,1:NNOD)-DZ(I,1:NNOD)
        END DO
        CALL T6DERI3(
     .   X0N(1,1),X0N(1,2),X0N(1,3),X0N(1,4),X0N(1,5),X0N(1,6),
     .   Y0N(1,1),Y0N(1,2),Y0N(1,3),Y0N(1,4),Y0N(1,5),Y0N(1,6), 
     .   Z0N(1,1),Z0N(1,2),Z0N(1,3),Z0N(1,4),Z0N(1,5),Z0N(1,6),
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, 
     .   VOL  ,NEL )
C---------------
C  MEMBRANE [F]-[1] 
C---------------
       DO I=1,NEL
         VX14=DX(I,1)+DX(I,4)
         VX25=DX(I,2)+DX(I,5)
         VX36=DX(I,3)+DX(I,6)
         VXHI=DX(I,4)+DX(I,5)+DX(I,6)-DX(I,1)-DX(I,2)-DX(I,3)
         VY14=DY(I,1)+DY(I,4)
         VY25=DY(I,2)+DY(I,5)
         VY36=DY(I,3)+DY(I,6)
         VYHI=DY(I,4)+DY(I,5)+DY(I,6)-DY(I,1)-DY(I,2)-DY(I,3)
         VZ14=DZ(I,1)+DZ(I,4)
         VZ25=DZ(I,2)+DZ(I,5)
         VZ36=DZ(I,3)+DZ(I,6)
         VZHI=DZ(I,4)+DZ(I,5)+DZ(I,6)-DZ(I,1)-DZ(I,2)-DZ(I,3)
C
         F(1,1,I)=PX1(I)*VX14+PX2(I)*VX25+PX3(I)*VX36+PX4(I)*VXHI
         F(2,2,I)=PY1(I)*VY14+PY2(I)*VY25+PY3(I)*VY36+PY4(I)*VYHI
         F(3,3,I)=PZ1(I)*VZ14+PZ2(I)*VZ25+PZ3(I)*VZ36+PZ4(I)*VZHI
         F(1,2,I)=PY1(I)*VX14+PY2(I)*VX25+PY3(I)*VX36+PY4(I)*VXHI
         F(1,3,I)=PZ1(I)*VX14+PZ2(I)*VX25+PZ3(I)*VX36+PZ4(I)*VXHI
         F(2,1,I)=PX1(I)*VY14+PX2(I)*VY25+PX3(I)*VY36+PX4(I)*VYHI
         F(2,3,I)=PZ1(I)*VY14+PZ2(I)*VY25+PZ3(I)*VY36+PZ4(I)*VYHI
         F(3,1,I)=PX1(I)*VZ14+PX2(I)*VZ25+PX3(I)*VZ36+PX4(I)*VZHI
         F(3,2,I)=PY1(I)*VZ14+PY2(I)*VZ25+PY3(I)*VZ36+PY4(I)*VZHI
       END DO
C---------------
C  MEMBRANE [e]=[U]-1 
C---------------
       CALL U_FROM_F3(F,STRAIN,NEL)
C
      RETURN
      END
Chd|====================================================================
Chd|  T4DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        T4_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE T4DERI3(
     .   X1,  X2,  X3,  X4,  Y1,  Y2,  Y3,  Y4, 
     .   Z1,  Z2,  Z3,  Z4,  PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4, 
     .   DET  ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
     
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER I
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,
     .   A1, A2, A3, A4, D, AREAMX2, 
     .   B1(NEL), B2(NEL), B3(NEL), B4(NEL), 
     .   C1(NEL), C2(NEL), C3(NEL), C4(NEL),
     .   D1(NEL), D2(NEL), D3(NEL), D4(NEL)
C-----------------------------------------------
C
      DO I=1,NEL
       X43 = X4(I) - X3(I)
       Y43 = Y4(I) - Y3(I)
       Z43 = Z4(I) - Z3(I)
       X41 = X4(I) - X1(I)
       Y41 = Y4(I) - Y1(I)
       Z41 = Z4(I) - Z1(I)
       X42 = X4(I) - X2(I)
       Y42 = Y4(I) - Y2(I)
       Z42 = Z4(I) - Z2(I)
C
       B1(I) =  Y43*Z42 - Y42*Z43
       B2(I) =  Y41*Z43 - Y43*Z41
       B3(I) =  Y42*Z41 - Y41*Z42
       B4(I) =  -(B1(I) + B2(I) + B3(I))
C
       C1(I) =  Z43*X42 - Z42*X43
       C2(I) =  Z41*X43 - Z43*X41
       C3(I) =  Z42*X41 - Z41*X42
       C4(I) =  -(C1(I) + C2(I) + C3(I))
C
       D1(I) =  X43*Y42 - X42*Y43
       D2(I) =  X41*Y43 - X43*Y41
       D3(I) =  X42*Y41 - X41*Y42
       D4(I) =  -(D1(I) + D2(I) + D3(I))
C----------SIX = 6.
       DET(I) = (X41*B1(I) + Y41*C1(I) + Z41*D1(I))/SIX
C
      ENDDO
C
      DO I=1,NEL
        D = ONE/DET(I)/SIX
        PX1(I)=-B1(I)*D
        PY1(I)=-C1(I)*D
        PZ1(I)=-D1(I)*D
        PX2(I)=-B2(I)*D
        PY2(I)=-C2(I)*D
        PZ2(I)=-D2(I)*D
        PX3(I)=-B3(I)*D
        PY3(I)=-C3(I)*D
        PZ3(I)=-D3(I)*D
        PX4(I)=-B4(I)*D
        PY4(I)=-C4(I)*D
        PZ4(I)=-D4(I)*D
       END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  T8DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE T8DERI3(
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   DET  ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
     
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   AJ1(NEL),AJ2(NEL),AJ3(NEL),
     .   AJ4(NEL),AJ5(NEL),AJ6(NEL),
     .   DETT ,  AJ7(NEL), AJ8(NEL) , AJ9(NEL),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ12, AJ45, AJ78,
     .   A17 , A28 ,
     .   B17 , B28 ,
     .   C17 , C28 ,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46
C-----------------------------------------------
C
      DO I=1,NEL
        X17=X7(I)-X1(I)
        X28=X8(I)-X2(I)
        X35=X5(I)-X3(I)
        X46=X6(I)-X4(I)
        Y17=Y7(I)-Y1(I)
        Y28=Y8(I)-Y2(I)
        Y35=Y5(I)-Y3(I)
        Y46=Y6(I)-Y4(I)
        Z17=Z7(I)-Z1(I)
        Z28=Z8(I)-Z2(I)
        Z35=Z5(I)-Z3(I)
        Z46=Z6(I)-Z4(I)
c             
        AJ1(I)=X17+X28-X35-X46
        AJ2(I)=Y17+Y28-Y35-Y46
        AJ3(I)=Z17+Z28-Z35-Z46
        A17=X17+X46
        A28=X28+X35
        B17=Y17+Y46
        B28=Y28+Y35
        C17=Z17+Z46
        C28=Z28+Z35

        AJ4(I)=A17+A28
        AJ5(I)=B17+B28
        AJ6(I)=C17+C28
        AJ7(I)=A17-A28
        AJ8(I)=B17-B28
        AJ9(I)=C17-C28
      ENDDO
C                  
C     JACOBIAN
C
      DO I=1,NEL      
        JAC_59_68=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
        DET(I)=AJ1(I)*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57
C        DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57)
        DETT=1.0/DET(I)
C       INVERSE DE LA MATRICE JACOBIENNE
        AJI1=DETT*JAC_59_68
        AJI4=DETT*JAC_67_49
        AJI7=DETT*JAC_48_57
        AJI2=DETT*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1-AJI2
        AJ45=AJI4-AJI5
        AJ78=AJI7-AJI8
        PX3(I)= AJ12+AJI3
        PY3(I)= AJ45+AJI6
        PZ3(I)= AJ78+AJI9
        PX4(I)= AJ12-AJI3
        PY4(I)= AJ45-AJI6
        PZ4(I)= AJ78-AJI9
!
        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8
!
        PX1(I)=-AJ12-AJI3
        PY1(I)=-AJ45-AJI6
        PZ1(I)=-AJ78-AJI9
        PX2(I)=-AJ12+AJI3
        PY2(I)=-AJ45+AJI6
        PZ2(I)=-AJ78+AJI9
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  T6DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE T6DERI3(
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   DET  ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),   
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*) 
     
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   DETT ,  AJ1(NEL), AJ2(NEL) ,AJ3(NEL), 
     .   AJ4(NEL)  ,  AJ5(NEL), AJ6(NEL),
     .   AJ7(NEL)  ,  AJ8(NEL), AJ9(NEL),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ12, AJ45, AJ78,
     .   FAC
C-----------------------------------------------
C
      DO I=1,NEL
        AJ1(I)=X2(I)-X1(I)+X5(I)-X4(I)
        AJ2(I)=Y2(I)-Y1(I)+Y5(I)-Y4(I)
        AJ3(I)=Z2(I)-Z1(I)+Z5(I)-Z4(I)
        AJ4(I)=X3(I)-X1(I)+X6(I)-X4(I)
        AJ5(I)=Y3(I)-Y1(I)+Y6(I)-Y4(I)
        AJ6(I)=Z3(I)-Z1(I)+Z6(I)-Z4(I)
        AJ7(I)=THIRD*(X4(I)-X1(I)+X5(I)-X2(I)+X6(I)-X3(I))
        AJ8(I)=THIRD*(Y4(I)-Y1(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
        AJ9(I)=THIRD*(Z4(I)-Z1(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
      END DO 
C
      DO I=1,NEL
        JAC_59_68=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
        DET(I)=AJ1(I)*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57
      END DO 
C      
      DO I=1,NEL
        DETT=1.0/DET(I)
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
        AJI1=DETT*JAC_59_68
        AJI4=DETT*JAC_67_49
        AJI7=DETT*JAC_48_57
        AJI2=DETT*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT*(AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT*(AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8
C
C ----------symtrie(a b c a b c)->P1-P3,anti-symtrie(-1 -1 -1 1 1 1)->P4
        PX1(I)=-AJ12
        PY1(I)=-AJ45
        PZ1(I)=-AJ78
        PX2(I)=AJI1
        PY2(I)=AJI4
        PZ2(I)=AJI7
        PX3(I)=AJI2
        PY3(I)=AJI5
        PZ3(I)=AJI8
        PX4(I)=THIRD*AJI3
        PY4(I)=THIRD*AJI6
        PZ4(I)=THIRD*AJI9
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        T4_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE U_FROM_F3(F,STRAIN,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   F(3,3,NEL), STRAIN(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      DOUBLE PRECISION 
     .         FMAT(3,3),UM(3,3),IC,I2C,I3C,IU,I2U,I3U,
     .         C11,C21,C31,C12,C22,C32,C13,C23,C33,DETJ0,DETM1,
     .         CC11,CC21,CC31,CC12,CC22,CC32,CC13,CC23,CC33,
     .         A,B,ZZ,A1,A2,A3,A4,X0(8),Y0(8),Z0(8)
C-----------------------------------------------
      DO I=1,NEL
        FMAT(1,1)= F(1,1,I)+1.0
        FMAT(2,2)= F(2,2,I)+1.0
        FMAT(3,3)= F(3,3,I)+1.0
        FMAT(1,2:3)= F(1,2:3,I)
        FMAT(2:3,1)= F(2:3,1,I)
        FMAT(2,3)= F(2,3,I)
        FMAT(3,2)= F(3,2,I)
        C11 = FMAT(1,1)*FMAT(1,1)+FMAT(2,1)*FMAT(2,1)+FMAT(3,1)*FMAT(3,1)
        C21 = FMAT(1,2)*FMAT(1,1)+FMAT(2,2)*FMAT(2,1)+FMAT(3,2)*FMAT(3,1)
        C31 = FMAT(1,3)*FMAT(1,1)+FMAT(2,3)*FMAT(2,1)+FMAT(3,3)*FMAT(3,1)
        C12 = C21
        C22 = FMAT(1,2)*FMAT(1,2)+FMAT(2,2)*FMAT(2,2)+FMAT(3,2)*FMAT(3,2)
        C32 = FMAT(1,3)*FMAT(1,2)+FMAT(2,3)*FMAT(2,2)+FMAT(3,3)*FMAT(3,2)
        C13 = C31
        C23 = C32
        C33 = FMAT(1,3)*FMAT(1,3)+FMAT(2,3)*FMAT(2,3)+FMAT(3,3)*FMAT(3,3)
        
        CC11 = C11*C11+C12*C21+C13*C31
        CC21 = C21*C11+C22*C21+C23*C31
        CC31 = C31*C11+C32*C21+C33*C31
        CC12 = CC21
        CC22 = C21*C12+C22*C22+C23*C32
        CC32 = C31*C12+C32*C22+C33*C32
        CC13 = CC31
        CC23 = CC32
        CC33 = C31*C13+C32*C23+C33*C33
        
        IC = C11+C22+C33
        I2C = C11*C22+C22*C33+C11*C33-C21*C12-C13*C31-C23*C32
        I3C = C11*C22*C33+C12*C23*C31+C13*C21*C32
     .      -(C13*C22*C31+C12*C21*C33+C11*C23*C32)
     
        A = (2.*IC*IC*IC-9.*IC*I2C+27*I3C)*32./27.
        B = (4*(I2C*I2C*I2C+IC*IC*IC*I3C)-IC*IC*I2C*I2C-18*IC*I2C*I3C
     .      +27*I3C*I3C)*1024./27.
        B = MAX(B,ZERO)
        A1 = A+SQRT(B)
        A2 = A-SQRT(B)
        ZZ = -2*IC/3.+SIGN(ABS(A1)**(1/3),A1)+SIGN(ABS(A2)**(1/3),A2)
        A = 2*IC+ZZ
        IF (A == ZERO) THEN
          IU = SQRT(IC+2*SQRT(I2C))
        ELSE
          A = SQRT(A)
          IU = 0.5*(A+SQRT(2*IC-ZZ+16*SQRT(I3C)/A))
        ENDIF
        I3U = SQRT(I3C)
        I2U = 0.5*(IU*IU-IC)
        A = IU*I2U-I3U
        B = I3U+IU*IC
        A1 = IU*A
        A2 = A*B
        A3 = I2U*I3U*B+IU*IU*(I2U*I2C+I3C)
        A4 = ONE/(I3U*I3U*B+IU*IU*(IU*I3C+I3U*I2C))
        UM(1,1) = A4*(A1*CC11-A2*C11+A3)
        UM(2,1) = A4*(A1*CC21-A2*C21)
        UM(3,1) = A4*(A1*CC31-A2*C31)
        UM(1,2) = A4*(A1*CC12-A2*C12)
        UM(2,2) = A4*(A1*CC22-A2*C22+A3)
        UM(3,2) = A4*(A1*CC32-A2*C32)
        UM(1,3) = A4*(A1*CC13-A2*C13)
        UM(2,3) = A4*(A1*CC23-A2*C23)
        UM(3,3) = A4*(A1*CC33-A2*C33+A3)
        STRAIN(1,I) = UM(1,1)-1.0
        STRAIN(2,I) = UM(2,2)-1.0
        STRAIN(3,I) = UM(3,3)-1.0
        STRAIN(4,I) = 0.5*(UM(1,2)+UM(2,1))
        STRAIN(5,I) = 0.5*(UM(2,3)+UM(3,2))
        STRAIN(6,I) = 0.5*(UM(1,3)+UM(3,1))
       END DO
C
      RETURN
      END
