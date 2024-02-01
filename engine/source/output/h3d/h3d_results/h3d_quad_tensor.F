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
Chd|  H3D_QUAD_TENSOR               source/output/h3d/h3d_results/h3d_quad_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_TENSOR              source/output/h3d/h3d_results/h3d_write_tensor.F
Chd|        QROTA_GROUP                   source/output/anim/generate/qrota_group.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_QUAD_TENSOR(ELBUF_TAB,QUAD_TENSOR,IPARG ,ITENS ,INVERT,NELCUT,
     2                   EL2FA    ,TENS  ,EPSDOT,IADP  ,
     3                   NBPART,IADG  ,X     ,IXQ   ,
     4                   IGEO     ,IXTG  ,IPM   ,STACK,ID_ELEM   ,INFO1,
     5                   INFO2    ,IS_WRITTEN_QUAD,IPARTQ ,IPARTTG     ,LAYER_INPUT ,IPT_INPUT  ,
     6                   PLY_INPUT,GAUSS_INPUT,IUVAR_INPUT,H3D_PART, KEYWORD,
     7                   IR_INPUT ,IS_INPUT     ,IT_INPUT )
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS,INVERT(*),
     .   EL2FA(*),IXQ(NIXQ,*), IGEO(NPROPGI,*), 
     .   NELCUT,IADP(*),NBPART,IADG(NSPMD,*),
     .   IXTG(NIXTG,*),IPM(NPROPMI,*),ID_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_QUAD(*),IPARTQ(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,II,
     .   IR_INPUT,IS_INPUT,IT_INPUT
C     REAL
      my_real
     .   TENS(3,*),EPSDOT(6,*),X(3,*),QUAD_TENSOR(6,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   A1,A2,A3,THK,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   SY,SZ,TY,TZ,SUMA,R11,R12,R13,R21,R22,
     .   R23,R31,R32,R33,S1,S2,S4,T1,T2,T3,T4,CT,CS,
     .   G22,G23,G32,G33,T22,T23,T32,T33
      my_real
     .   SIGE(MVSIZ,5)
      my_real
     .   EVAR(6,MVSIZ), GAMA(6,MVSIZ)
      REAL R4(18)
      INTEGER I,NG,NEL,NFT,ITY,LFT,NPT,MPT,IPT,
     .        N,J,LLT,MLW,ISTRAIN,IL,IR,IS,IT,NPTR,NPTS,NLAY,
     .        IPID,I1,I2,NS1,NS2,ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        IHBE,IREP,BUF,NPG,K,ISROT,NUVARV,IVISC,
     .        IPMAT,IGTYP,MATLY,ISUBSTACK,IIGEO,IADI,IPMAT_IPLY,
     .        NPT_ALL,NPTT,ILAY,IUS,ID_PLY,IPANG,IPPOS,IPTHK,OFFSET,ISELECT,
     .        IPLY,IUVAR,IAD,JALE,JTURB,JCVT,NC1,NC2,NC3,NC4,ISORTH
      INTEGER PID(MVSIZ),MAT(MVSIZ),IOK_PART(MVSIZ),JJ(6),IS_WRITTEN_TENSOR(MVSIZ)
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C
      my_real,
     .   DIMENSION(:), POINTER :: DIR_A
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR
C-----------------------------------------------      
      ILAY = LAYER_INPUT
      IUVAR = IUVAR_INPUT
      IR = IR_INPUT
      IS = IS_INPUT
      IT = IT_INPUT
c
      DO I=1,NUMELQ
         IS_WRITTEN_QUAD(I) = 0
      ENDDO
c a corriger
      NN3 = 0
c
      DO NG=1,NGROUP

        MLW     = IPARG(1,NG)
        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        NPT     = IPARG(6,NG)
        ITY     = IPARG(5,NG)
        IGTYP   = IPARG(38,NG)
        ISROT   = IPARG(41,NG)
        ISTRAIN = IPARG(44,NG)
        ISUBSTACK = IPARG(71,NG)
        ISORTH  = IPARG(42,NG)      
        JCVT    = IPARG(37,NG)
        LFT=1
        LLT=NEL
        IOK_PART(1:NEL) = 0   
c
        NPTR = ELBUF_TAB(NG)%NPTR                 
        NPTS = ELBUF_TAB(NG)%NPTS  
        NPTT = ELBUF_TAB(NG)%NPTT
c
        IF (MLW /= 13) THEN
          NFT   =IPARG(3,NG)
          IAD   =IPARG(4,NG)
          ISUBSTACK = IPARG(71,NG)
          IVISC = IPARG(61,NG)
          IOK_PART(1:NEL) = 0  
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          EVAR(1:6,1:NEL) = ZERO
          IS_WRITTEN_TENSOR(1:NEL) = 0
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
          IF(ITY == 2)THEN

            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
            JALE=(IPARG(7,NG)+IPARG(11,NG))
            JTURB=IPARG(12,NG)*JALE
c
            DO I=1,NEL 
              ID_ELEM(NFT+I) = IXQ(NIXQ,NFT+I)
              IF( H3D_PART(IPARTQ(NFT+I)) == 1) IOK_PART(I) = 1
            ENDDO
c
            DO I=1,NEL 
              IF (ISORTH == 0) THEN
                GAMA(1,I)=ONE
                GAMA(2,I)=ZERO
                GAMA(3,I)=ZERO
                GAMA(4,I)=ZERO
                GAMA(5,I)=ONE
                GAMA(6,I)=ZERO
              ELSE
                GAMA(1,I)=GBUF%GAMA(JJ(1) + I)
                GAMA(2,I)=GBUF%GAMA(JJ(2) + I)
                GAMA(3,I)=GBUF%GAMA(JJ(3) + I)
                GAMA(4,I)=GBUF%GAMA(JJ(4) + I)
                GAMA(5,I)=GBUF%GAMA(JJ(5) + I)
                GAMA(6,I)=GBUF%GAMA(JJ(6) + I)
              ENDIF
            ENDDO
C-----------------------------------------------
            IF (KEYWORD == 'TENS/STRESS') THEN
C-----------------------------------------------
C---------------------------------------------
C     EN 2D LES CONTRAINTES SONT :
C     1=YY 2=ZZ 3=TT 4=YZ 5=0 6=0
C     EN CONTRADICTION AVEC X=T
C---------------------------------------------
c ILAYER=NULL IR=NULL IS=NULL IT=NULL
              IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
                DO I=1,NEL
                  II = 6*(I-1)
                  EVAR(1,I) = GBUF%SIG(JJ(1) + I)
                  EVAR(2,I) = GBUF%SIG(JJ(2) + I)
                  EVAR(4,I) = GBUF%SIG(JJ(4) + I)
                  IS_WRITTEN_TENSOR(I) = 1
                ENDDO
c
                IF(IVISC > 0) THEN
                   LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
                   DO I=1,NEL
                     II = 6*(I-1)
                     EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)
                     EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)
                     EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)
                   ENDDO
                ENDIF
c
                IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
                    EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
                    EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
                  ENDDO
                ENDIF

                IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_GROUP(
     1   X,           IXQ(1,NFT+1),JCVT,        EVAR,
     2   GBUF%GAMA,   NEL,         ISORTH)
                ENDIF
c
              ELSEIF ( ILAY == -1 .AND. IABS(IT) == 1 .AND. IR >= 0 .AND. 
     .                IR <= NPTR .AND. IS >= 0 .AND. IS <= NPTS) THEN 
c
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)          
                DO I=1,NEL
                  II = 6*(I-1)
                  EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                  EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                  EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                  IS_WRITTEN_TENSOR(I) = 1
                ENDDO
c
                IF(IVISC > 0) THEN
                   DO I=1,NEL
                     II = 6*(I-1)
                     EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)
                     EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)
                     EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)
                   ENDDO
                ENDIF
c
                IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
                  DO I=1,NEL
                    EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
                    EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
                    EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
                  ENDDO
                ENDIF
c
                IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_GROUP(
     1   X,           IXQ(1,NFT+1),JCVT,        EVAR,
     2   GBUF%GAMA,   NEL,         ISORTH)
                ENDIF
c
              ENDIF
c
C-----------------------------------------------
            ELSEIF (KEYWORD == 'TENS/STRAIN') THEN
C-----------------------------------------------
              IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
c
                DO I=1,NEL
                  N = I + NFT   
                  DO IS=1,NPTS               
                   DO IT=1,NPTT
                    DO IR=1,NPTR
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)        
                     EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)/NPT
                     EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)/NPT
                     EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF/NPT
                     IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                   ENDDO
                  ENDDO
                ENDDO

                IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_GROUP(
     1   X,           IXQ(1,NFT+1),JCVT,        EVAR,
     2   GBUF%GAMA,   NEL,         ISORTH)
                ENDIF
c
              ELSEIF ( ILAY == -1 .AND. IABS(IT) == 1 .AND. IR >= 0 .AND. 
     .                IR <= NPTR .AND. IS >= 0 .AND. IS <= NPTS) THEN 
c
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)          
                DO I=1,NEL
                  N = I + NFT   
                  EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)   
                  EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)   
                  EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I) 
                  IS_WRITTEN_TENSOR(I) = 1  
                ENDDO

                IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_GROUP(
     1   X,           IXQ(1,NFT+1),JCVT,        EVAR,
     2   GBUF%GAMA,   NEL,         ISORTH)
                ENDIF
c
              ENDIF
c
C-----------------------------------------------
            ELSEIF (KEYWORD == 'TENS/DAMA') THEN
C-----------------------------------------------
              IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
c
                DO I=1,NEL
                  N = I + NFT   
                  DO IS=1,NPTS               
                   DO IT=1,NPTT
                    DO IR=1,NPTR
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)                                         
                     IF(ELBUF_TAB(NG)%BUFLY(1)%L_DGLO > 0) THEN
                       EVAR(1,I) = EVAR(1,I)+LBUF%DGLO(JJ(1) + I)/NPT
                       EVAR(2,I) = EVAR(2,I)+LBUF%DGLO(JJ(2) + I)/NPT
                       EVAR(4,I) = EVAR(4,I)+LBUF%DGLO(JJ(4) + I)/NPT
                       IS_WRITTEN_TENSOR(I) = 1
                     ENDIF
                   ENDDO                                                   
                  ENDDO                                                    
                 ENDDO                                                     
                ENDDO   
c                                                  
              ELSEIF ( ILAY == -1 .AND. IABS(IT) == 1 .AND. IR >= 0 .AND. 
     .                IR <= NPTR .AND. IS >= 0 .AND. IS <= NPTS) THEN 
c
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)        
                IF (ELBUF_TAB(NG)%BUFLY(1)%L_DGLO > 0) THEN
                  DO I=1,NEL
                    N = I + NFT                                                   
                    EVAR(1,I) = EVAR(1,I)+LBUF%DGLO(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I)+LBUF%DGLO(JJ(2) + I)
                    EVAR(4,I) = EVAR(4,I)+LBUF%DGLO(JJ(4) + I)
                    IS_WRITTEN_TENSOR(I) = 1
                  ENDDO                                                    
                ENDIF
c
              ENDIF
C-----------------------------------------------
            ELSEIF (KEYWORD == 'TENS/EPSP') THEN
C-----------------------------------------------
              IF( ILAY == -1 .AND. IR == -1 .AND. IS == -1 .AND. IT == -1 )THEN
c
                DO I=1,NEL
                  N = I + NFT   
                  DO IS=1,NPTS               
                   DO IT=1,NPTT
                    DO IR=1,NPTR
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)        
                     EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)/NPT
                     EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)/NPT
                     EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF/NPT
                     IS_WRITTEN_TENSOR(I) = 1
                    ENDDO
                   ENDDO
                  ENDDO
                ENDDO

                IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_GROUP(
     1   X,           IXQ(1,NFT+1),JCVT,        EVAR,
     2   GBUF%GAMA,   NEL,         ISORTH)
                ENDIF
c
              ELSEIF ( ILAY == -1 .AND. IABS(IT) == 1 .AND. IR >= 0 .AND. 
     .                IR <= NPTR .AND. IS >= 0 .AND. IS <= NPTS) THEN 
c
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)          
                DO I=1,NEL
                  N = I + NFT   
                  EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)   
                  EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)   
                  EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL) 
                  IS_WRITTEN_TENSOR(I) = 1  
                ENDDO

                IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_GROUP(
     1   X,           IXQ(1,NFT+1),JCVT,        EVAR,
     2   GBUF%GAMA,   NEL,         ISORTH)
                ENDIF
c
              ENDIF
            ENDIF
C-----------------------------------------------
            CALL H3D_WRITE_TENSOR(IOK_PART,IS_WRITTEN_QUAD,QUAD_TENSOR,NEL,0,NFT,
     .                                    EVAR,IS_WRITTEN_TENSOR)
C---------------------------------------------------------------------------
c            IF (KEYWORD == 'NEWKEY') THEN ! New Output Example
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
          ENDIF ! IF(ITY == 2)
        ENDIF ! IF (MLW /= 13)
      ENDDO
C-----------------------------------------------
C
      RETURN
      END
