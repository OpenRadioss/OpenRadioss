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
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "nchar_c.inc"
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
      CHARACTER*ncharline KEYWORD
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

      INTEGER :: NFT,IAD,ITY,NPT,JALE,ISMSTR,JEUL,JTUR,JTHE,JLAG,JMULT
      INTEGER :: JHBE,JIVF,NVAUX,JPOR,JCLOSE,JPLASOL,IREP,IINT,IGTYP,ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE,JSMS
C-----------------------------------------------


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
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1) THEN


          DO  I=1,NEL 
            ID_ELEM(NFT+I) = IXS(NIXS,NFT+I)
            ITY_ELEM(NFT+I) = 1
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
C           pour JHBE=14, valeurs moyennes est dans rep. corota.
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
C                   STRAIN TENSOR IN GLOBAL SYSTEM
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
C                    STRAIN TENSOR IN GLOBAL SYSTEM
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
C                 PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
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
            CALL STRS_TENSCOR3(
     1   ELBUF_TAB(NG),IPARG(1,NG),  IXS,          IXS10,
     2   X,            PM,           KCVT,         NEL,
     3   EVAR_CORNER,  NFT,          ITY,          NPT,
     4   JHBE,         IGTYP,        ISORTH)
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
C                   STRAIN TENSOR IN GLOBAL SYSTEM
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
C                    STRAIN TENSOR IN GLOBAL SYSTEM
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
            CALL STRN_TENSCOR3(
     1   ELBUF_TAB(NG),IPARG(1,NG),  IXS,          IXS10,
     2   X,            PM,           KCVT,         NEL,
     3   EVAR_CORNER,  NFT,          NPT,          JHBE,
     4   IGTYP,        ISORTH)
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
