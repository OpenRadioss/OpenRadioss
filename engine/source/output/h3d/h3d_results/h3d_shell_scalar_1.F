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
Chd|  H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|-- called by -----------
Chd|        H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_SCALAR              source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SCHLIEREN                     source/output/anim/generate/schlieren.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SHELL_SCALAR_1(
     .                  ELBUF_TAB   ,SHELL_SCALAR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM          ,BUFMAT     ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,ALE_CONNECT,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS   ,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                  D           ,NG         ,MULTI_FVM,IDMDS       ,IMDSVAR    ,
     .                  MDS_MATID   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD    
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchar_c.inc"
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
#include      "scr25_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "mmale51_c.inc"
#include      "alefvm.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN),TARGET :: BUFMAT(*)
      my_real
     .   SHELL_SCALAR(*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),D(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(*), ERR_THK_SH3(NUMELTG)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,NPART),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_SHELL(*),IPARTC(NUMELC),IPARTTG(NUMELTG),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,NG,IDMDS,
     .   MDS_MATID(*),IMDSVAR,NERCVOIS(SNERCVOIS),NESDVOIS(SNESDVOIS),LERCVOIS(SLERCVOIS),LESDVOIS(SLESDVOIS)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real EVAR(MVSIZ),DAM1(MVSIZ),DAM2(MVSIZ),WPLA(MVSIZ),DMAX(MVSIZ),WPMAX(MVSIZ),FAIL(MVSIZ),
     .        EPST1(MVSIZ),EPST2(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),VALUE(MVSIZ),VG(5),VLY(5),VE(5),MASS(MVSIZ),
     .        NINTY,VONM2,S1,S2,S12,DMGMX,A1,A2,A3,A4,DIR1_1,DIR1_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .        X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,Z31,E11,E12,E13,E21,E22,E23,SUM_,AREA,X2L,
     .        E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,S_X,S_Y,S_Z,RHO0(MVSIZ),THK0,XX1,XX2,XX3,YY1,YY2,YY3,ZZ1,ZZ2,ZZ3,A0,
     .        RINDX,VFRAC(MVSIZ,1:21),TMP(3,3),CUMUL(3),VX,VY,VZ,SURF,NX,NY,NZ,PHI,ERR,PRES(MVSIZ),VEL(0:3),MAXDAMINI
      INTEGER I,I1,II,J,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,NFAIL,
     .        N,K,K1,K2,JTURB,
     .        OFFSET,IHBE,NPG, MPT,IPT,IADR,IPMAT,
     .        ISUBSTACK,ITHK,ID_PLY,IOK,N1,N2,N3,N4,
     .        IMAT,IU(4),NFRAC,IPOS,ITRIMAT,NS,IAD2,IDRAPE
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        NPT_ALL,IPLY,
     .        IOK_PART(MVSIZ),JJ(5),NPGT,IUVAR,
     .        IS_WRITTEN_VALUE(MVSIZ),IV,KFACE,NB_FACE,IADBUF,NUPARAM,ISUBMAT,IS_EULER,IS_ALE,
     .        IPINCH,IPG,USER_OK,IALEL
      CHARACTER*5 BUFF
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
      TYPE(BUF_MAT_)  ,POINTER :: MBUF 
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
      my_real, DIMENSION(:), POINTER  :: UVAR            
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      DATA NS/10/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       NINTY = 90.
       CALL INITBUF (IPARG   ,NG      ,                    
     2               MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3               NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4               JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5               NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6               IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7               ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

       IF(MLW /= 13) THEN 
  
          NFT = IPARG(3,NG)
          IAD = IPARG(4,NG)  
          ISUBSTACK = IPARG(71,NG)
          IS_EULER = IPARG(11,NG)
          IS_ALE = IPARG(7,NG)

          IOK_PART(1:NEL) = 0  

          DO I=1,5
            JJ(I) = NEL*(I-1)
          ENDDO  

          DO I=1,NEL
            VALUE(I) = ZERO
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	
C----------------------------------------------- 
C           3-NODE-SHELL & 4-NODE-SHELL
C-----------------------------------------------
         IF (ITY == 3.OR.ITY == 7) THEN

            GBUF => ELBUF_TAB(NG)%GBUF
            NPT   = IPARG(6,NG)
            IHBE  = IPARG(23,NG)
            IREP  = IPARG(35,NG)
            IGTYP = IPARG(38,NG)
            ITHK  = IPARG(28,NG)
            MPT   = IABS(NPT)
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
            NPTT  = ELBUF_TAB(NG)%NPTT
            NLAY  = ELBUF_TAB(NG)%NLAY
            IDRAPE = ELBUF_TAB(NG)%IDRAPE
            
            NPG   = NPTR*NPTS
            NUVAR = 0
            IPINCH= IPARG(90,NG)
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
            USER_OK = 0
c
            IF (KEYWORD == 'MDS') IUVAR = IMDSVAR
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
              IF (ILAY == -2) ILAY = 1
              IF (ILAY == -3) ILAY = NPT
              IF (IPT  == -2) IPT  = 1
              IF (IPT  == -3) IPT  = MAX(1,ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT)
            ELSE
              IF (ILAY == -2) ILAY = 1
              IF (ILAY == -3) ILAY = NLAY
              IF (IPT  == -2) IPT  = 1
              IF (IPT  == -3) IPT  = MAX(1,NPT)    
            ENDIF                 
C---------------------    
            DO I=1,NEL
              VALUE(I) = ZERO
              SHELL_SCALAR(OFFSET+NFT+I) = ZERO   ! Default = zero in all cases !
            ENDDO
C-----------------------------------------------
C Mass computation
C-----------------------------------------------
            IF (KEYWORD == 'MASS'  .OR. KEYWORD == 'HOURGLASS' .OR. KEYWORD == 'ENER' .OR. KEYWORD(1:4) == 'EINT') THEN
C-----------------------------------------------
C        4-NODE-SHELL
C-----------------------------------------------
              IF(ITY==3)THEN
C
             	IF(IGTYP /= 17 . AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
             	  DO I=1,NEL
             	    N = I + NFT
             	    RHO0(I) = PM(1,IXC(1,N))
             	    THK0 = GEO(1,IXC(6,N))
             	    N1 = IXC(2,N)
             	    N2 = IXC(3,N)
             	    N3 = IXC(4,N)
             	    N4 = IXC(5,N)
             	    XX1 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
             	    YY1 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
             	    ZZ1 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
             	    XX2 = X(1,N4)-D(1,N4)-X(1,N2)+D(1,N2)
             	    YY2 = X(2,N4)-D(2,N4)-X(2,N2)+D(2,N2)
             	    ZZ2 = X(3,N4)-D(3,N4)-X(3,N2)+D(3,N2)
             	    XX3 = YY1*ZZ2 - ZZ1*YY2
             	    YY3 = ZZ1*XX2 - XX1*ZZ2
             	    ZZ3 = XX1*YY2 - YY1*XX2
             	    A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
             	    MASS(I) = RHO0(I)*THK0*A0
             	  ENDDO
             	ELSE
             	  DO I=1,NEL
             	    N = I + NFT
             	    RHO0(I) = PM(1,IXC(1,N))
             	    THK0 = STACK%GEO(1,ISUBSTACK)
             	    N1 = IXC(2,N)
             	    N2 = IXC(3,N)
             	    N3 = IXC(4,N)
             	    N4 = IXC(5,N)
             	    XX1 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
             	    YY1 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
             	    ZZ1 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
             	    XX2 = X(1,N4)-D(1,N4)-X(1,N2)+D(1,N2)
             	    YY2 = X(2,N4)-D(2,N4)-X(2,N2)+D(2,N2)
             	    ZZ2 = X(3,N4)-D(3,N4)-X(3,N2)+D(3,N2)
             	    XX3 = YY1*ZZ2 - ZZ1*YY2
             	    YY3 = ZZ1*XX2 - XX1*ZZ2
             	    ZZ3 = XX1*YY2 - YY1*XX2
             	    A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
             	    MASS(I) = RHO0(I)*THK0*A0
             	  ENDDO 
             	ENDIF 
C-----------------------------------------------
C       3-NODE-SHELL
C-----------------------------------------------
              ELSEIF(ITY==7)THEN
C       
            	IF(IGTYP /= 17 . AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
                  DO I=1,NEL
                    N = I + NFT
                    RHO0(I) = PM(1,IXTG(1,N))
                    THK0 = GEO(1,IXTG(5,N))
                    N1 = IXTG(2,N)
                    N2 = IXTG(3,N)
                    N3 = IXTG(4,N)
                    XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
                    YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
                    ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
                    XX2 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
                    YY2 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
                    ZZ2 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
                    XX3 = YY1*ZZ2 - ZZ1*YY2
                    YY3 = ZZ1*XX2 - XX1*ZZ2
                    ZZ3 = XX1*YY2 - YY1*XX2
                    A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
                    MASS(I) = RHO0(I)*THK0*A0
                  ENDDO
            	ELSE
            	  DO I=1,NEL
            	    N = I + NFT
            	    RHO0(I) = PM(1,IXTG(1,N))
            	    THK0 = STACK%GEO(1,ISUBSTACK)
            	    N1 = IXTG(2,N)
            	    N2 = IXTG(3,N)
            	    N3 = IXTG(4,N)
            	    XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
            	    YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
            	    ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
            	    XX2 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
            	    YY2 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
            	    ZZ2 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
            	    XX3 = YY1*ZZ2 - ZZ1*YY2
            	    YY3 = ZZ1*XX2 - XX1*ZZ2
            	    ZZ3 = XX1*YY2 - YY1*XX2
            	    A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
            	    MASS(I) = RHO0(I)*THK0*A0
            	  ENDDO       
            	ENDIF
              ENDIF
            ENDIF
C---------------------

C
            IF (MLW == 0 .OR. MLW == 13) THEN
              !nothing to do
C--------------------------------------------------
            ELSEIF (KEYWORD == 'MASS') THEN   ! MASS
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = MASS(I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'DENS') THEN   ! DENS
C--------------------------------------------------
               IF (MLW /= 151) THEN
                  IF (ITHK >0) THEN
                     DO I=1,NEL
                        VALUE(I) = GBUF%RHO(I) 
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ENDIF
               ELSE
                  DO I=1,NEL
                     VALUE(I) = MULTI_FVM%RHO(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'ENER') THEN   ! = EINTM
C--------------------------------------------------
               IF (MLW /= 151) THEN
                  DO I=1,NEL
                     VALUE(I) = (GBUF%EINT(I)+ GBUF%EINT(I+NEL))/MAX(EM20,MASS(I))
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C-------------------------------------------------- 
c temporary (tria entity is not yet created)
            ELSEIF (KEYWORD == 'EINT')THEN  !SI:J
C-------------------------------------------------- 
              IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT) * GBUF%VOL(I)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     N = I + NFT
                     IF(N2D == 0)THEN
                        VALUE(I) = (GBUF%EINT(I) + GBUF%EINT(I+NEL))
                     ELSE
                        VALUE(I) = GBUF%EINT(I)*GBUF%VOL(I)
                     ENDIF
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C-------------------------------------------------- 
c temporary (tria entity is not yet created)
            ELSEIF (KEYWORD == 'EINTM')THEN  !SI:J/kg
C-------------------------------------------------- 
               !LAG: GBUF%VOL = V0,    GBUF%EINT=rho0.e
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I)  !
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     N = I + NFT
                     IF(N2D == 0)THEN
                        VALUE(I) = (GBUF%EINT(I)+ GBUF%EINT(I+NEL))/MAX(EM20,MASS(I))
                     ELSE
                        VALUE(I) = GBUF%EINT(I)/GBUF%RHO(I)
                     ENDIF
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO                  
               ENDIF
C--------------------------------------------------
c temporary (tria entity is not yet created)
            ELSEIF (KEYWORD == 'EINTV')THEN  !SI:J/m^3
C-------------------------------------------------- 
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(I) = MULTI_FVM%EINT(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     N = I + NFT
                     IF(N2D == 0)THEN
                        VALUE(I) = (GBUF%EINT(I)+ GBUF%EINT(I+NEL))*RHO0(I)/MASS(I)
                     ELSE
                        VALUE(I) = GBUF%EINT(I)
                     ENDIF
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF                  
C--------------------------------------------------
c temporary (tria entity is not yet created)
C-------------------------------------------------- 
              ELSEIF (KEYWORD(1:4) == 'ENTH')THEN
C-------------------------------------------------- 
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     PRES(I) = MULTI_FVM%PRES(I + NFT)
                  ENDDO
               ELSE
                  DO I=1,NEL
                     PRES(I) = - (GBUF%SIG(JJ(1) + I)+ GBUF%SIG(JJ(2) + I) + GBUF%SIG(JJ(3) + I))*THIRD
                  ENDDO
               ENDIF    
               !GBUF%EINT is rho.e           
               IF(KEYWORD == 'ENTH')THEN
                 IF (MLW == 151) THEN
                     DO I = 1, NEL
                        VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT) + PRES(I)*GBUF%VOL(I) !
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ELSE
                     DO I=1,NEL
                        N = I + NFT
                        IF(N2D == 0)THEN
                        ELSE
                           VALUE(I) = GBUF%EINT(I)/GBUF%RHO(I) + PRES(I)*GBUF%VOL(I) ! 
                           IS_WRITTEN_VALUE(I) = 1
                        ENDIF
                     ENDDO
                  ENDIF                
               ELSEIF(KEYWORD == 'ENTHV')THEN
                 IF (MLW == 151) THEN
                     DO I = 1, NEL
                        VALUE(I) = MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT)/GBUF%VOL(I) + PRES(I) !
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ELSE
                     DO I=1,NEL
                        N = I + NFT
                        IF(N2D == 0)THEN
                        ELSE
                           VALUE(I) = GBUF%EINT(I)/GBUF%VOL(I)/GBUF%RHO(I) + PRES(I)  !
                           IS_WRITTEN_VALUE(I) = 1
                        ENDIF
                     ENDDO
                  ENDIF                                
               ELSEIF(KEYWORD == 'ENTHM')THEN
                 IF (MLW == 151) THEN
                     DO I = 1, NEL
                        MASS(I) = MULTI_FVM%RHO(I + NFT)*GBUF%VOL(I)
                        VALUE(I) = (MULTI_FVM%EINT(I + NFT) / MULTI_FVM%RHO(I + NFT) + PRES(I)*GBUF%VOL(I))/MASS(I) !
                        IS_WRITTEN_VALUE(I) = 1
                     ENDDO
                  ELSE
                     DO I=1,NEL
                        N = I + NFT
                        IF(N2D == 0)THEN
                        ELSE
                           MASS(I)=GBUF%RHO(I)*GBUF%VOL(I)
                           VALUE(I) = (GBUF%EINT(I)/GBUF%RHO(I) + PRES(I)*GBUF%VOL(I))/MASS(I)
                           IS_WRITTEN_VALUE(I) = 1
                        ENDIF
                     ENDDO
                  ENDIF                                 
               ENDIF                               
C--------------------------------------------------
            ELSEIF(KEYWORD == 'P')THEN
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I=1,NEL
                     VALUE(I) = MULTI_FVM%PRES(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1
                  ENDDO
               ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'THICK') THEN   ! THK
C--------------------------------------------------
              IF (ITHK >0) THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%THK(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSE
                IF (ITY == 3) THEN ! SHELL
                  DO I=1,NEL
                    VALUE(I)  = THKE(NFT+I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ELSEIF (ITY == 7) THEN ! SH3N
                  DO I=1,NEL
                    VALUE(I)  = THKE(NFT+I+NUMELC) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF ! IF (ITY == 3)
              END IF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'VONM') THEN   ! Von Mises
C--------------------------------------------------
              DO I=1,NEL
                S1 = GBUF%FOR(JJ(1)+I)
                S2 = GBUF%FOR(JJ(2)+I)
                S12= GBUF%FOR(JJ(3)+I)
                VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
		VALUE(I) = SQRT(VONM2)		 
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'DAM1') THEN ! DAM1
C--------------------------------------------------
c              IF (MLW == 25 .OR. MLW == 15 )THEN 
c              NB13 = NB12 + 2*NEL*MAX(1,NPT)
c              NB15 = NB12 + 2*NEL*MAX(1,NPT)
c              NB12 = NB12 + 2*OFFSET*MAX(1,NPT)
c              NB13 = NB13 + 4*OFFSET*MAX(1,NPT)
c              DO J=1,NPT
c                N = (IPT-1)*NEL
c                DO I=1,NEL
c                  K1 = NB13 + N+I
c                  K2 = NB15 + 2*N+2*I-1 
c                  IS_WRITTEN_VALUE(I) = 1
c                ENDDO 
c              ENDDO
c             ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'DAM2')THEN ! DAM2 
C--------------------------------------------------
c             IF(MLW == 25.OR.MLW == 15)THEN 
c              NB13 = NB12 + 2*NEL*MAX(1,NPT)
c              NB15 = NB12 + 2*NEL*MAX(1,NPT)
c              NB12 = NB12 + 2*OFFSET*MAX(1,NPT)
c              NB13 = NB13 + 4*OFFSET*MAX(1,NPT)
c              DO J=1,NPT
c                N = (IPT-1)*NEL
c                DO I=1,NEL
c                  K1 = NB13 + N+I
c                  K2 = NB15 + 2*N+2*I 
c                  IS_WRITTEN_VALUE(I) = 1
c                ENDDO 
c              ENDDO
c             ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'DAM3')THEN ! DAM3
C--------------------------------------------------
c              IF(MLW == 25.OR.MLW == 15)THEN 
c                IADD  = NB12 + 6*NEL*MAX(1,NPT)
c                IADD  = IADD + OFFSET
c                DO I=1,NEL
c                  IS_WRITTEN_VALUE(I) = 1
c                ENDDO
c              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGX') THEN   ! Sigx - global
C--------------------------------------------------
              DO I=1,NEL                      
                VALUE(I) = GBUF%FOR(JJ(1)+I)			 
                IS_WRITTEN_VALUE(I) = 1
              ENDDO 
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGY') THEN   ! Sigy - global
C--------------------------------------------------
              DO I=1,NEL                      
                VALUE(I) = GBUF%FOR(JJ(2)+I)			 
                IS_WRITTEN_VALUE(I) = 1
              ENDDO 
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGZ') THEN   ! Sigz - batoz pinching
C--------------------------------------------------
              IF(IHBE == 11 .AND. IPINCH == 1) THEN             
                DO I=1,NEL
                  VALUE(I) = ZERO
                  DO IPG=1,4 
                    VALUE(I) = VALUE(I) + FOURTH*GBUF%FORPGPINCH(NEL*(IPG-1)+I)
                  ENDDO 
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGXY') THEN   ! Sigxy - global
C--------------------------------------------------
              DO I=1,NEL                      
                VALUE(I) = GBUF%FOR(JJ(3)+I)			 
                IS_WRITTEN_VALUE(I) = 1
              ENDDO                      
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGYZ') THEN   ! Sigyz - global
C--------------------------------------------------
              DO I=1,NEL                      
                VALUE(I) = GBUF%FOR(JJ(4)+I)			 
                IS_WRITTEN_VALUE(I) = 1 
              ENDDO   
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGZX') THEN   ! Sigzx - global
C--------------------------------------------------
              DO I=1,NEL                      
                VALUE(I) = GBUF%FOR(JJ(5)+I)			 
                IS_WRITTEN_VALUE(I) = 1
              ENDDO 
C--------------------------------------------------
            ELSEIF (KEYWORD == 'HOURGLASS') THEN   ! HOUR
C--------------------------------------------------
              IF (ITY == 3) THEN
                DO I=1,NEL
                  VALUE(I) = EHOUR(NFT+I+NUMELS)/MAX(EM20,MASS(I))			 
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO  
            ENDIF     
C--------------------------------------------------
            ELSEIF (KEYWORD == 'EPSD')THEN   ! EPSD
C--------------------------------------------------
              IF( GBUF%G_EPSD > 0) THEN
                IF (MLW == 104) THEN 
                  NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  NPGT = NPTR*NPTS*NPTT
                  DO I = 1,NEL
                    VALUE(I) = ZERO
                  ENDDO
                  DO I=1,NEL
                    DO IT=1,NPTT  
                      DO IS=1,NPTS
                        DO IR=1,NPTR    
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                          VALUE(I) = VALUE(I) + LBUF%EPSD(I)/NPGT
                        ENDDO
                      ENDDO
                    ENDDO
                    IS_WRITTEN_VALUE(I) = 1 
                  ENDDO      
                ELSE
                  DO I=1,NEL
                    VALUE(I) = GBUF%EPSD(I)			 
                    IS_WRITTEN_VALUE(I) = 1			
                  ENDDO                                   
                ENDIF
              ENDIF  
C--------------------------------------------------
           ELSEIF(KEYWORD(1:9) == 'M151VFRAC') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A9,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    GBUF => ELBUF_TAB(NG)%GBUF
                    LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)
                    DO I=1,NEL
                       VALUE(I) = LBUF%VOL(I) / GBUF%VOL(I)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF  
C--------------------------------------------------
           ELSEIF(KEYWORD(1:8) == 'M151ENER') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    DO I=1,NEL
                       VALUE(I) = MULTI_FVM%PHASE_EINT(IMAT, I + NFT) / 
     .                      MULTI_FVM%PHASE_RHO(IMAT, I + NFT)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF    
C--------------------------------------------------
           ELSEIF(KEYWORD(1:8) == 'M151PRES') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    DO I=1,NEL
                       VALUE(I) = MULTI_FVM%PHASE_PRES(IMAT, I + NFT)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF  
C--------------------------------------------------
           ELSEIF(KEYWORD(1:8) == 'M151DENS') THEN
C--------------------------------------------------
              IF (MLW == 151) THEN 
                 READ(KEYWORD, '(A8,I10)') BUFF, IMAT
                 IF (IMAT > 0 .AND. IMAT <= NLAY) THEN
                    DO I=1,NEL
                       VALUE(I) = MULTI_FVM%PHASE_RHO(IMAT, I + NFT)
                       IS_WRITTEN_VALUE(I) = 1           
                    ENDDO
                 ENDIF
              ENDIF  
C--------------------------------------------------
           ELSEIF(KEYWORD == 'THIN')THEN
C--------------------------------------------------
              DO I=1,NEL
                VALUE(I) = HUNDRED *(GBUF%THK_I(I)-GBUF%THK(I))/GBUF%THK_I(I)			 
                IS_WRITTEN_VALUE(I) = 1			
              ENDDO
C--------------------------------------------------
            ELSEIF (KEYWORD == 'USER' .OR. KEYWORD == 'MDS') THEN
C--------------------------------------------------
              I1  = (IUVAR-1)*NEL
c ILAYER=NULL NPT=NULL (mid-layer)
              IF(IPT == -1 .AND. ILAY == -1 .AND. IPLY == -1 .AND. IUVAR > 0 ) THEN
                IF (MLW==29 .OR. MLW==30 .OR. MLW==31 .OR. MLW>=33) THEN
c							       
                  IF (NLAY > 1) THEN							     
                    IL  = IABS(NLAY)/2 + 1						     
                    IPT = 1								     
                  ELSE  								     
                    IL  = 1								     
                    IPT =  IABS(NPT)/2 + 1						     
                  ENDIF 
                  IMAT = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                  NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT					     
c
                  IF(KEYWORD == 'USER') USER_OK = 1
                  IF(IDMDS > 0)THEN
                    IF(IMAT == MDS_MATID(IDMDS))USER_OK = 1
                  ENDIF
c
                  IF(USER_OK == 1)THEN
                    IF(IUVAR <= NUVAR) THEN
                      IF (MLW == 58) THEN                                          
                        DO IR = 1, NPTR                                                        
                         DO IS = 1, NPTS                                                       
                          UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR                     
                          IF(IUVAR==3.OR.IUVAR==4)THEN                                                         
                           DO I=1,NEL                                              
                             VALUE(I) = VALUE(I) + LOG(UVAR(I1 + I)+ONE)/NPG                        
                             IS_WRITTEN_VALUE(I) = 1
                           ENDDO                          
                          ELSE                                                         
                           DO I=1,NEL                                                                                  
                             VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                        
                             IS_WRITTEN_VALUE(I) = 1
                           ENDDO                                                                          
                          ENDIF                                                                
                         ENDDO                                                                 
                        ENDDO                                                                    
                      ELSE                                                  
                        DO IR = 1, NPTR                                                       
                          DO IS = 1, NPTS                                                     
                            UVAR=>ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR                                                                   
                            DO I=1,NEL        
                              VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                             
                              IS_WRITTEN_VALUE(I) = 1                                                    
                            ENDDO                                                
                          ENDDO
                        ENDDO                                                                     
                      ENDIF !MLW=58                                                               
                    ENDIF                                                        
                  ENDIF                                                        
                ENDIF
c PLY=IPLY NPT=IPT
              ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 .AND. IUVAR > 0) THEN
c				     
	        DO J=1,NLAY
                  NUVAR = ELBUF_TAB(NG)%BUFLY(J)%NVAR_MAT
                  IF(IUVAR <= NUVAR) THEN
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	              ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF
c
                    IF (ID_PLY  == IPLY ) THEN
                      IMAT = ELBUF_TAB(NG)%BUFLY(IL)%IMAT					     
c
                      IF(KEYWORD == 'USER') USER_OK = 1
                      IF(IDMDS > 0)THEN
                        IF(IMAT == MDS_MATID(IDMDS))USER_OK = 1
                      ENDIF
c
                      IF(USER_OK == 1)THEN
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                        IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                          NPTT = BUFLY%NPTT 
                          IF( IPT <= NPTT) THEN
                            IF( NPG > 1 ) THEN
                              DO IR=1,NPTR
                                DO IS=1,NPTS   
                                  UVAR=>ELBUF_TAB(NG)%BUFLY(J)%MAT(IR,IS,IPT)%VAR
                                  DO I=1,NEL                         
                                    VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                 
                                    IS_WRITTEN_VALUE(I) = 1               
                                  ENDDO           
                                ENDDO  
                              ENDDO 
                            ELSE 
                              UVAR=>ELBUF_TAB(NG)%BUFLY(J)%MAT(1,1,1)%VAR  
                              DO  I=1,NEL                      
                                VALUE(I) =  UVAR(I1 + I)                     
                                IS_WRITTEN_VALUE(I) = 1                     
                              ENDDO
                            ENDIF
                          ENDIF
                        ENDIF 
                      ENDIF  
                    ENDIF  
                  ENDIF 
                ENDDO

c PLY=IPLY NPT=-1
              ELSEIF ( IPLY > 0  .AND. IUVAR > 0) THEN
c
	        DO J=1,NLAY
                  NUVAR = ELBUF_TAB(NG)%BUFLY(J)%NVAR_MAT
                  IF(IUVAR <= NUVAR) THEN
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	              ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF
c
                    IF (ID_PLY  == IPLY ) THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      IMAT = ELBUF_TAB(NG)%BUFLY(J)%IMAT					     
c
                      IF(KEYWORD == 'USER') USER_OK = 1
                      IF(IDMDS > 0)THEN
                        IF(IMAT == MDS_MATID(IDMDS))USER_OK = 1
                      ENDIF
c
                      IF(USER_OK == 1)THEN
                        IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                          NPTT = BUFLY%NPTT 
	                  DO IPT=1,NPTT
                            IF( NPG > 1 ) THEN
                              DO IR=1,NPTR
                                DO IS=1,NPTS
                                  UVAR=>ELBUF_TAB(NG)%BUFLY(J)%MAT(IR,IS,IPT)%VAR
                                  DO  I=1,NEL                        
                                    VALUE(I) = VALUE(I) + UVAR(I1 + I) / (NPG * NPTT)                      
                                    IS_WRITTEN_VALUE(I) = 1               
                                  ENDDO                   
                                ENDDO  
                              ENDDO
                            ELSE
                              UVAR=>ELBUF_TAB(NG)%BUFLY(J)%MAT(1,1,1)%VAR 
                              DO  I=1,NEL                              
                                VALUE(I) =  VALUE(I) + UVAR(I1 + I) / NPTT                   
                                IS_WRITTEN_VALUE(I) = 1                     
                              ENDDO
                            ENDIF
                          ENDDO
                        ENDIF 
                      ENDIF 
                    ENDIF 
                  ENDIF
                ENDDO
c ILAYER=IL NPT=IPT
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0  .AND. IUVAR > 0) THEN
                IF(IUVAR <= NUVAR) THEN
                  IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                    NUVAR = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_MAT
                    BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                    IMAT = ELBUF_TAB(NG)%BUFLY(ILAY)%IMAT					     
c
                    IF(KEYWORD == 'USER') USER_OK = 1
                    IF(IDMDS > 0)THEN
                      IF(IMAT == MDS_MATID(IDMDS))USER_OK = 1
                    ENDIF
c
                    IF(USER_OK == 1)THEN
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          UVAR=>ELBUF_TAB(NG)%BUFLY(ILAY)%MAT(IR,IS,IPT)%VAR 
                          DO I=1,NEL
                            VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                       
                            IS_WRITTEN_VALUE(I) = 1 
                          ENDDO                 
                        ENDDO
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
c ILAYER=IL NPT=NULL
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0 .AND. IUVAR > 0) THEN
                NUVAR = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_MAT
                IMAT = ELBUF_TAB(NG)%BUFLY(ILAY)%IMAT					     
c
                IF(KEYWORD == 'USER') USER_OK = 1
                IF(IDMDS > 0)THEN
                  IF(IMAT == MDS_MATID(IDMDS))USER_OK = 1
                ENDIF
c
                IF(USER_OK == 1)THEN
                  IF(IUVAR <= NUVAR) THEN
                    IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                      BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          UVAR=>ELBUF_TAB(NG)%BUFLY(ILAY)%MAT(IR,IS,1)%VAR 
                          DO I=1,NEL
                            VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                       
                            IS_WRITTEN_VALUE(I) = 1                     
                          ENDDO                 
                        ENDDO
                      ENDDO
                    ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                      BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                      DO IT=1,NPTT
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            UVAR=>ELBUF_TAB(NG)%BUFLY(ILAY)%MAT(IR,IS,IT)%VAR 
                            DO I=1,NEL
                              VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                     
                              IS_WRITTEN_VALUE(I) = 1                   
                            ENDDO       
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
c  ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0 .AND. IUVAR > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                  IF(IUVAR <= NUVAR) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        UVAR=>ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IPT)%VAR 
                        DO I=1,NEL
                          VALUE(I) = VALUE(I) + UVAR(I1 + I)/NPG                       
                          IS_WRITTEN_VALUE(I) = 1                     
                        ENDDO                 
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSEIF( KEYWORD == 'PHI' ) THEN
C--------------------------------------------------
c
              IF (ILAY <= NLAY .AND. ILAY > 0 .AND. IPLY == -1) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                IF (ITY == 3) THEN
                  IF (IGTYP == 9  .OR. IGTYP == 10 .OR.IGTYP == 11 .OR.
     .                IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR.
     .                IGTYP == 52 ) THEN
                    IF (MLW /= 0 .AND. MLW /= 13) THEN    
                     IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                      LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1) 
                      DO I=1,NEL                                                
                        N = I + NFT          
                        X21 = X(1,IXC(3,N))-X(1,IXC(2,N)) 
                        X32 = X(1,IXC(4,N))-X(1,IXC(3,N)) 
                        X34 = X(1,IXC(4,N))-X(1,IXC(5,N)) 
                        X41 = X(1,IXC(5,N))-X(1,IXC(2,N)) 
                        Y21 = X(2,IXC(3,N))-X(2,IXC(2,N)) 
                        Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))  
                        Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))  
                        Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))                         

                        Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))                         
                        Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))                         
                        Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))                         
                        Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))                         
                                                                                  
                        E1X = (X21+X34)                                           
                        E1Y = (Y21+Y34)                                           
                        E1Z = (Z21+Z34)                                           

                        E2X = (X32+X41)                                           
                        E2Y = (Y32+Y41)                                           
                        E2Z = (Z32+Z41)                                           
                 
                        E3X = E1Y*E2Z-E1Z*E2Y                                     
                        E3Y = E1Z*E2X-E1X*E2Z                                     
                        E3Z = E1X*E2Y-E1Y*E2X                                     
                        IF (IREP > 0) THEN                                        
                          RX = E1X                                                
                          RY = E1Y                                                
                          RZ = E1Z                                                
                          S_X = E2X                                                
                          S_Y = E2Y                                                
                          S_Z = E2Z                                                
                        ENDIF                                                     
                        IF (ISHFRAM == 0 ) THEN                  
C------                   Convected frame symmetric - version 5 (default)        
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = SQRT(S1/S2)                                    
                          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                      
                          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                      
                          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                      
C
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E1X = E1X * SUMA                                        
                          E1Y = E1Y * SUMA                                        
                          E1Z = E1Z * SUMA                                        
C
                          E2X = E3Y * E1Z - E3Z * E1Y                             
                          E2Y = E3Z * E1X - E3X * E1Z                             
                          E2Z = E3X * E1Y - E3Y * E1X                             
                        ELSEIF (ISHFRAM == 2) THEN                                
C------                   Convected frame non-symmetric - version 4               
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y                        
                          E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z                        
                          E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X                        
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E1X = E1X*SUMA                                          
                          E1Y = E1Y*SUMA                                          
                          E1Z = E1Z*SUMA                                          
C
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          E2X = E3Y*E1Z-E3Z*E1Y                                   
                          E2Y = E3Z*E1X-E3X*E1Z                                   
                          E2Z = E3X*E1Y-E3Y*E1X                                   
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E2X = E2X*SUMA                                          
                          E2Y = E2Y*SUMA                                          
                          E2Z = E2Z*SUMA                                          
                        ENDIF 
                         IF (IREP >= 1) THEN            
                           AA = LBUF_DIR%DIRA(I)           
                           BB = LBUF_DIR%DIRA(I+NEL)       
                           V1 = AA*RX + BB*S_X          
                           V2 = AA*RY + BB*S_Y          
                           V3 = AA*RZ + BB*S_Z          
                           VR = V1*E1X+ V2*E1Y + V3*E1Z 
                           VS = V1*E2X+ V2*E2Y + V3*E2Z 
                           SUMA=SQRT(VR*VR + VS*VS)     
                           DIR1_1 = VR/SUMA             
                           DIR1_2 = VS/SUMA             
                         ELSE                           
                           DIR1_1 = LBUF_DIR%DIRA(I)       
                           DIR1_2 = LBUF_DIR%DIRA(I+NEL)   
                         ENDIF      
c
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        VALUE(I) = PHI
                        IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                        IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                        IS_WRITTEN_VALUE(I) = 1				
                      ENDDO      
                     ELSE     ! IDRAPE               
                      DO I=1,NEL                                                
                        N = I + NFT                                               
                        X21 = X(1,IXC(3,N))-X(1,IXC(2,N))                         
                        X32 = X(1,IXC(4,N))-X(1,IXC(3,N))                         
                        X34 = X(1,IXC(4,N))-X(1,IXC(5,N))                         
                        X41 = X(1,IXC(5,N))-X(1,IXC(2,N))                         

                        Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))                         
                        Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))                         
                        Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))                         
                        Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))                         

                        Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))                         
                        Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))                         
                        Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))                         
                        Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))                         
                                                                                  
                        E1X = (X21+X34)                                           
                        E1Y = (Y21+Y34)                                           
                        E1Z = (Z21+Z34)                                           

                        E2X = (X32+X41)                                           
                        E2Y = (Y32+Y41)                                           
                        E2Z = (Z32+Z41)                                           
                 
                        E3X = E1Y*E2Z-E1Z*E2Y                                     
                        E3Y = E1Z*E2X-E1X*E2Z                                     
                        E3Z = E1X*E2Y-E1Y*E2X                                     
                        IF (IREP > 0) THEN                                        
                          RX = E1X                                                
                          RY = E1Y                                                
                          RZ = E1Z                                                
                          S_X = E2X                                                
                          S_Y = E2Y                                                
                          S_Z = E2Z                                                
                        ENDIF                                                     
                        IF (ISHFRAM == 0 .OR. IGTYP == 16 ) THEN                  
C------                   Convected frame symmetric - version 5 (default)        
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = SQRT(S1/S2)                                    
                          E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                      
                          E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                      
                          E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                      
C
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E1X = E1X * SUMA                                        
                          E1Y = E1Y * SUMA                                        
                          E1Z = E1Z * SUMA                                        
C
                          E2X = E3Y * E1Z - E3Z * E1Y                             
                          E2Y = E3Z * E1X - E3X * E1Z                             
                          E2Z = E3X * E1Y - E3Y * E1X                             
                        ELSEIF (ISHFRAM == 2) THEN                                
C------                   Convected frame non-symmetric - version 4               
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y                        
                          E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z                        
                          E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X                        
                          SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E1X = E1X*SUMA                                          
                          E1Y = E1Y*SUMA                                          
                          E1Z = E1Z*SUMA                                          
C
                          SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                        
                          SUMA   = ONE / MAX(SQRT(SUMA),EM20)                      
                          E3X = E3X * SUMA                                        
                          E3Y = E3Y * SUMA                                        
                          E3Z = E3Z * SUMA                                        
C
                          E2X = E3Y*E1Z-E3Z*E1Y                                   
                          E2Y = E3Z*E1X-E3X*E1Z                                   
                          E2Z = E3X*E1Y-E3Y*E1X                                   
                          SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z                        
                          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                        
                          E2X = E2X*SUMA                                          
                          E2Y = E2Y*SUMA                                          
                          E2Z = E2Z*SUMA                                          
                        ENDIF
                        IF (IREP >= 1) THEN            
                             AA = BUFLY%DIRA(I)           
                             BB = BUFLY%DIRA(I+NEL)       
                             V1 = AA*RX + BB*S_X          
                             V2 = AA*RY + BB*S_Y          
                             V3 = AA*RZ + BB*S_Z          
                             VR = V1*E1X+ V2*E1Y + V3*E1Z 
                             VS = V1*E2X+ V2*E2Y + V3*E2Z 
                             SUMA=SQRT(VR*VR + VS*VS)     
                             DIR1_1 = VR/SUMA             
                             DIR1_2 = VS/SUMA             
                        ELSE                           
                             DIR1_1 = BUFLY%DIRA(I)       
                             DIR1_2 = BUFLY%DIRA(I+NEL)   
                        ENDIF   
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        VALUE(I) = PHI
                        IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                        IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                        IS_WRITTEN_VALUE(I) = 1				
                      ENDDO      
                     ENDIF ! IDRAPE                                                  
                    ENDIF  ! MLW                                                  
                  ENDIF  ! IGTYP

                ELSEIF (ITY == 7) THEN
                  NPG = IPARG(48,NG)
                  IF (IGTYP == 9  .OR. IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .                IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR.
     .                IGTYP == 52 ) THEN
                    IF (MLW /= 0 .AND. MLW /= 13) THEN      
                     IF(IDRAPE > 0 . AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                      LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1)   
                      DO I=1,NEL                                                         
                        N = I + NFT                                                      
                        X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))                              
                        X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                              
                        X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                     

                        Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))                     
                        Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))                     
                        Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))                     

                        Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))                     
                        Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))                     
                        Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))                     
                        IF (IREP > 0) THEN                                      
                          E11 = X21                                             
                          E12 = Y21                                             
                          E13 = Z21                                             
                          E21 = X31                                             
                          E22 = Y31                                             
                          E23 = Z31                                             
                        ENDIF                                                   
                        E1X= X21                                                
                        E1Y= Y21                                                
                        E1Z= Z21                                                
                        X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)                     
                        E1X=E1X/X2L                                             
                        E1Y=E1Y/X2L                                             
                        E1Z=E1Z/X2L                                             
C
                        E3X=Y31*Z32-Z31*Y32                                     
                        E3Y=Z31*X32-X31*Z32                                     
                        E3Z=X31*Y32-Y31*X32                                     
                        SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)                     
                        E3X=E3X/SUM_                                             
                        E3Y=E3Y/SUM_                                             
                        E3Z=E3Z/SUM_                                             
                        AREA = HALF * SUM_                                     
                        E2X=E3Y*E1Z-E3Z*E1Y                                     
                        E2Y=E3Z*E1X-E3X*E1Z                                     
                        E2Z=E3X*E1Y-E3Y*E1X                                     
                        SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)                     
                        E2X=E2X/SUM_                                             
                        E2Y=E2Y/SUM_                                             
                        E2Z=E2Z/SUM_   
                        IF (IREP >= 1) THEN                                     
                           AA = LBUF_DIR%DIRA(I)                            
                           BB = LBUF_DIR%DIRA(I+NEL)                            
                           V1 = AA*E11 + BB*E21                                  
                           V2 = AA*E12 + BB*E22                                  
                           V3 = AA*E13 + BB*E23                                  
                           VR = V1*E1X + V2*E1Y + V3*E1Z                         
                           VS = V1*E2X + V2*E2Y + V3*E2Z                         
                           SUMA=SQRT(VR*VR + VS*VS)                              
                           DIR1_1 = VR/SUMA                                      
                           DIR1_2 = VS/SUMA                                      
                         ELSE                                                    
                           DIR1_1 = LBUF_DIR%DIRA(I)                        
                           DIR1_2 = LBUF_DIR%DIRA(I+NEL)                        
                         ENDIF   
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        VALUE(I) = PHI
                        IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                        IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                        IS_WRITTEN_VALUE(I) = 1				
                      ENDDO 
                    ELSE                
                      DO I=1,NEL                                              
                        N = I + NFT                                             
                        X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))                     
                        X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))                     
                        X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))                     

                        Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))                     
                        Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))                     
                        Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))                     

                        Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))                     
                        Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))                     
                        Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))                     
                        IF (IREP > 0) THEN                                      
                          E11 = X21                                             
                          E12 = Y21                                             
                          E13 = Z21                                             
                          E21 = X31                                             
                          E22 = Y31                                             
                          E23 = Z31                                             
                        ENDIF                                                   
                        E1X= X21                                                
                        E1Y= Y21                                                
                        E1Z= Z21                                                
                        X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)                     
                        E1X=E1X/X2L                                             
                        E1Y=E1Y/X2L                                             
                        E1Z=E1Z/X2L                                             
C
                        E3X=Y31*Z32-Z31*Y32                                     
                        E3Y=Z31*X32-X31*Z32                                     
                        E3Z=X31*Y32-Y31*X32                                     
                        SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)                     
                        E3X=E3X/SUM_                                             
                        E3Y=E3Y/SUM_                                             
                        E3Z=E3Z/SUM_                                             
                        AREA = HALF * SUM_                                     
                        E2X=E3Y*E1Z-E3Z*E1Y                                     
                        E2Y=E3Z*E1X-E3X*E1Z                                     
                        E2Z=E3X*E1Y-E3Y*E1X                                     
                        SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)                     
                        E2X=E2X/SUM_                                             
                        E2Y=E2Y/SUM_                                             
                        E2Z=E2Z/SUM_   
                        IF (IREP >= 1) THEN                                     
                           AA = BUFLY%DIRA(I)                            
                           BB = BUFLY%DIRA(I+NEL)                            
                           V1 = AA*E11 + BB*E21                                  
                           V2 = AA*E12 + BB*E22                                  
                           V3 = AA*E13 + BB*E23                                  
                           VR = V1*E1X + V2*E1Y + V3*E1Z                         
                           VS = V1*E2X + V2*E2Y + V3*E2Z                         
                           SUMA=SQRT(VR*VR + VS*VS)                              
                           DIR1_1 = VR/SUMA                                      
                           DIR1_2 = VS/SUMA                                      
                         ELSE                                                    
                           DIR1_1 = BUFLY%DIRA(I)                        
                           DIR1_2 = BUFLY%DIRA(I+NEL)                        
                         ENDIF   
                        PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                        ERR = (ABS(PHI) - NINTY)/NINTY
                        VALUE(I) = PHI
                        IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                        IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                        IS_WRITTEN_VALUE(I) = 1				
                      ENDDO      
                     ENDIF    ! IDRAPE                                            
                    ENDIF                                                       
                  ENDIF              
                ENDIF
c
              ELSEIF (IPLY > 0) THEN
	         DO J=1,NLAY
                   ID_PLY = 0
                   IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	     	     ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                   ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (ITY == 3) THEN
                     IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      IF (MLW /= 0 .AND. MLW /= 13) THEN
                       IF(IDRAPE > 0 . AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
                        LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1)   	
                	DO I=1,NEL						                        
                	  N = I + NFT						    
                	  X21 = X(1,IXC(3,N))-X(1,IXC(2,N))			    
                	  X32 = X(1,IXC(4,N))-X(1,IXC(3,N))			    
                	  X34 = X(1,IXC(4,N))-X(1,IXC(5,N))			    
                	  X41 = X(1,IXC(5,N))-X(1,IXC(2,N))			    

                	  Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))			    
                	  Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))			    
                	  Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))			    
                	  Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))			    

                	  Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))			    
                	  Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))			    
                	  Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))			    
                	  Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))			    
                								    
                	  E1X = (X21+X34)					    
                	  E1Y = (Y21+Y34)					    
                	  E1Z = (Z21+Z34)					    

                	  E2X = (X32+X41)					    
                	  E2Y = (Y32+Y41)					    
                	  E2Z = (Z32+Z41)					    
                   
                	  E3X = E1Y*E2Z-E1Z*E2Y 				    
                	  E3Y = E1Z*E2X-E1X*E2Z 				    
                	  E3Z = E1X*E2Y-E1Y*E2X 
			    
                	  IF (IREP > 0) THEN					    
                	    RX = E1X						    
                	    RY = E1Y						    
                	    RZ = E1Z						    
                	    S_X = E2X						    
                	    S_Y = E2Y						    
                	    S_Z = E2Z						    
                	  ENDIF 						    
                	  IF (ISHFRAM == 0 ) THEN		    
C------         	    Convected frame symmetric - version 5 (default)	    
                	    SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z			    
                	    SUMA   = ONE / MAX(SQRT(SUMA),EM20)  	    
                	    E3X = E3X * SUMA					    
                	    E3Y = E3Y * SUMA					    
                	    E3Z = E3Z * SUMA					    
C
                	    S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z			    
                	    S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z			    
                	    SUMA   = SQRT(S1/S2)				    
                	    E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA  		    
                	    E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA  		    
                	    E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA  		    
C
                	    SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z			    
                	    SUMA   = ONE / MAX(SQRT(SUMA),EM20)  		    
                	    E1X = E1X * SUMA					    
                	    E1Y = E1Y * SUMA					    
                	    E1Z = E1Z * SUMA					    
C
                	    E2X = E3Y * E1Z - E3Z * E1Y 			    
                	    E2Y = E3Z * E1X - E3X * E1Z 			    
                	    E2Z = E3X * E1Y - E3Y * E1X 			    
                	  ELSEIF (ISHFRAM == 2) THEN				    
C------         	    Convected frame non-symmetric - version 4		    
                	    SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z			    
                	    E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y			    
                	    E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z			    
                	    E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X			    
                	    SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z			    
                	    SUMA   = ONE/MAX(SQRT(SUMA),EM20)			    
                	    E1X = E1X*SUMA					    
                	    E1Y = E1Y*SUMA					    
                	    E1Z = E1Z*SUMA					    
C
                	    SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z			    
                	    SUMA   = ONE / MAX(SQRT(SUMA),EM20)  		    
                	    E3X = E3X * SUMA					    
                	    E3Y = E3Y * SUMA					    
                	    E3Z = E3Z * SUMA					    
C
                	    E2X = E3Y*E1Z-E3Z*E1Y				    
                	    E2Y = E3Z*E1X-E3X*E1Z				    
                	    E2Z = E3X*E1Y-E3Y*E1X				    
                	    SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z			    
                	    SUMA   = ONE/MAX(SQRT(SUMA),EM20)			    
                	    E2X = E2X*SUMA					    
                	    E2Y = E2Y*SUMA					    
                	    E2Z = E2Z*SUMA					    
                	  ENDIF	    
                	  IF (IREP >= 1) THEN					    
                	     AA = LBUF_DIR%DIRA(I)                            
                	     BB = LBUF_DIR%DIRA(I+NEL)                          
                             V1 = AA*RX + BB*S_X     
                	     V2 = AA*RY + BB*S_Y                                   
                	     V3 = AA*RZ + BB*S_Z                                   
                	     VR = V1*E1X+ V2*E1Y + V3*E1Z                          
                	     VS = V1*E2X+ V2*E2Y + V3*E2Z                          
                	     SUMA=SQRT(VR*VR + VS*VS)                              
                	     DIR1_1 = VR/SUMA                                      
                	     DIR1_2 = VS/SUMA                                      
                	  ELSE  						    
                	     DIR1_1 = LBUF_DIR%DIRA(I)			    
                	     DIR1_2 = LBUF_DIR%DIRA(I+NEL)	
                	  ENDIF 		      
c
                          PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                          ERR = (ABS(PHI) - NINTY)/NINTY
                          VALUE(I) = PHI
                          IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                          IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                          IS_WRITTEN_VALUE(I) = 1				
                	ENDDO	
                       ELSE		    
                	DO I=1,NEL						  
                	  N = I + NFT						    
                	  X21 = X(1,IXC(3,N))-X(1,IXC(2,N))			    
                	  X32 = X(1,IXC(4,N))-X(1,IXC(3,N))			    
                	  X34 = X(1,IXC(4,N))-X(1,IXC(5,N))			    
                	  X41 = X(1,IXC(5,N))-X(1,IXC(2,N))			    

                	  Y21 = X(2,IXC(3,N))-X(2,IXC(2,N))			    
                	  Y32 = X(2,IXC(4,N))-X(2,IXC(3,N))			    
                	  Y34 = X(2,IXC(4,N))-X(2,IXC(5,N))			    
                	  Y41 = X(2,IXC(5,N))-X(2,IXC(2,N))			    

                	  Z21 = X(3,IXC(3,N))-X(3,IXC(2,N))			    
                	  Z32 = X(3,IXC(4,N))-X(3,IXC(3,N))			    
                	  Z34 = X(3,IXC(4,N))-X(3,IXC(5,N))			    
                	  Z41 = X(3,IXC(5,N))-X(3,IXC(2,N))			    
                								    
                	  E1X = (X21+X34)					    
                	  E1Y = (Y21+Y34)					    
                	  E1Z = (Z21+Z34)					    

                	  E2X = (X32+X41)					    
                	  E2Y = (Y32+Y41)					    
                	  E2Z = (Z32+Z41)					    
                   
                	  E3X = E1Y*E2Z-E1Z*E2Y 				    
                	  E3Y = E1Z*E2X-E1X*E2Z 				    
                	  E3Z = E1X*E2Y-E1Y*E2X 
			    
                	  IF (IREP > 0) THEN					    
                	    RX = E1X						    
                	    RY = E1Y						    
                	    RZ = E1Z						    
                	    S_X = E2X						    
                	    S_Y = E2Y						    
                	    S_Z = E2Z						    
                	  ENDIF 						    
                	  IF (ISHFRAM == 0 .OR. IGTYP == 16 ) THEN		    
C------         	    Convected frame symmetric - version 5 (default)	    
                	    SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z			    
                	    SUMA   = ONE / MAX(SQRT(SUMA),EM20)  	    
                	    E3X = E3X * SUMA					    
                	    E3Y = E3Y * SUMA					    
                	    E3Z = E3Z * SUMA					    
C
                	    S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z			    
                	    S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z			    
                	    SUMA   = SQRT(S1/S2)				    
                	    E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA  		    
                	    E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA  		    
                	    E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA  		    
C
                	    SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z			    
                	    SUMA   = ONE / MAX(SQRT(SUMA),EM20)  		    
                	    E1X = E1X * SUMA					    
                	    E1Y = E1Y * SUMA					    
                	    E1Z = E1Z * SUMA					    
C
                	    E2X = E3Y * E1Z - E3Z * E1Y 			    
                	    E2Y = E3Z * E1X - E3X * E1Z 			    
                	    E2Z = E3X * E1Y - E3Y * E1X 			    
                	  ELSEIF (ISHFRAM == 2) THEN				    
C------         	    Convected frame non-symmetric - version 4		    
                	    SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z			    
                	    E1X = E1X*SUMA + E2Y*E3Z-E2Z*E3Y			    
                	    E1Y = E1Y*SUMA + E2Z*E3X-E2X*E3Z			    
                	    E1Z = E1Z*SUMA + E2X*E3Y-E2Y*E3X			    
                	    SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z			    
                	    SUMA   = ONE/MAX(SQRT(SUMA),EM20)			    
                	    E1X = E1X*SUMA					    
                	    E1Y = E1Y*SUMA					    
                	    E1Z = E1Z*SUMA					    
C
                	    SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z			    
                	    SUMA   = ONE / MAX(SQRT(SUMA),EM20)  		    
                	    E3X = E3X * SUMA					    
                	    E3Y = E3Y * SUMA					    
                	    E3Z = E3Z * SUMA					    
C
                	    E2X = E3Y*E1Z-E3Z*E1Y				    
                	    E2Y = E3Z*E1X-E3X*E1Z				    
                	    E2Z = E3X*E1Y-E3Y*E1X				    
                	    SUMA   = E2X*E2X+E2Y*E2Y+E2Z*E2Z			    
                	    SUMA   = ONE/MAX(SQRT(SUMA),EM20)			    
                	    E2X = E2X*SUMA					    
                	    E2Y = E2Y*SUMA					    
                	    E2Z = E2Z*SUMA					    
                	  ENDIF	    
                	  IF (IREP >= 1) THEN					    
                	     AA = BUFLY%DIRA(I)                            
                	     BB = BUFLY%DIRA(I+NEL)                          
                             V1 = AA*RX + BB*S_X     
                	     V2 = AA*RY + BB*S_Y                                   
                	     V3 = AA*RZ + BB*S_Z                                   
                	     VR = V1*E1X+ V2*E1Y + V3*E1Z                          
                	     VS = V1*E2X+ V2*E2Y + V3*E2Z                          
                	     SUMA=SQRT(VR*VR + VS*VS)                              
                	     DIR1_1 = VR/SUMA                                      
                	     DIR1_2 = VS/SUMA                                      
                	  ELSE  						    
                	     DIR1_1 = BUFLY%DIRA(I)			    
                	     DIR1_2 = BUFLY%DIRA(I+NEL)	
                	  ENDIF 		      
c
                          PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                          ERR = (ABS(PHI) - NINTY)/NINTY
                          VALUE(I) = PHI
                          IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                          IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                          IS_WRITTEN_VALUE(I) = 1				
                	ENDDO	
                       ENDIF ! IDRAPE 						    
                      ENDIF  ! MLW						    
                     ENDIF  ! IGTYP

                    ELSEIF (ITY == 7) THEN
                     IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      IF (MLW /= 0 .AND. MLW /= 13) THEN
                       IF(IDRAPE > 0 .AND. IGTYP == 17) THEN 
                        LBUF_DIR => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(1) 
                	DO I=1,NEL						
                	  N = I + NFT						  
                	  X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))			  
                	  X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))			  
                	  X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))			  

                	  Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))			  
                	  Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))			  
                	  Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))			  

                	  Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))			  
                	  Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))			  
                	  Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))			  
                	  IF (IREP > 0) THEN					  
                	    E11 = X21						  
                	    E12 = Y21						  
                	    E13 = Z21						  
                	    E21 = X31						  
                	    E22 = Y31						  
                	    E23 = Z31						  
                	  ENDIF 						  
                	  E1X= X21						  
                	  E1Y= Y21						  
                	  E1Z= Z21						  
                	  X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)			  
                	  E1X=E1X/X2L						  
                	  E1Y=E1Y/X2L						  
                	  E1Z=E1Z/X2L						  
C
                	  E3X=Y31*Z32-Z31*Y32					  
                	  E3Y=Z31*X32-X31*Z32					  
                	  E3Z=X31*Y32-Y31*X32					  
                	  SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)			  
                	  E3X=E3X/SUM_						  
                	  E3Y=E3Y/SUM_						  
                	  E3Z=E3Z/SUM_						  
                	  AREA = HALF * SUM_					  
                	  E2X=E3Y*E1Z-E3Z*E1Y					  
                	  E2Y=E3Z*E1X-E3X*E1Z					  
                	  E2Z=E3X*E1Y-E3Y*E1X					  
                	  SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)			  
                	  E2X=E2X/SUM_						  
                	  E2Y=E2Y/SUM_						  
                	  E2Z=E2Z/SUM_						  
                	  IF (IREP >= 1) THEN	 
                	    DIR1_1 = LBUF_DIR%DIRA(I)			    
                	    DIR1_2 = LBUF_DIR%DIRA(I+NEL)				  
                	    V1 = AA*E11 + BB*E21				  
                	    V2 = AA*E12 + BB*E22				  
                	    V3 = AA*E13 + BB*E23				  
                	    VR = V1*E1X + V2*E1Y + V3*E1Z			  
                	    VS = V1*E2X + V2*E2Y + V3*E2Z			  
                	    SUMA=SQRT(VR*VR + VS*VS)				  
                	    DIR1_1 = VR/SUMA					  
                	    DIR1_2 = VS/SUMA					  
                	  ELSE  			    
                	    DIR1_1 = LBUF_DIR%DIRA(I)			    
                	    DIR1_2 = LBUF_DIR%DIRA(I+NEL)				  
                	  ENDIF 					
                	  PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                          ERR = (ABS(PHI) - NINTY)/NINTY
                          VALUE(I) = PHI
                          IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                          IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                          IS_WRITTEN_VALUE(I) = 1				
                	ENDDO				                    
                       ELSE			  
                	DO I=1,NEL						
                	  N = I + NFT						  
                	  X21 = X(1,IXTG(3,N))-X(1,IXTG(2,N))			  
                	  X31 = X(1,IXTG(4,N))-X(1,IXTG(2,N))			  
                	  X32 = X(1,IXTG(4,N))-X(1,IXTG(3,N))			  

                	  Y21 = X(2,IXTG(3,N))-X(2,IXTG(2,N))			  
                	  Y31 = X(2,IXTG(4,N))-X(2,IXTG(2,N))			  
                	  Y32 = X(2,IXTG(4,N))-X(2,IXTG(3,N))			  

                	  Z21 = X(3,IXTG(3,N))-X(3,IXTG(2,N))			  
                	  Z31 = X(3,IXTG(4,N))-X(3,IXTG(2,N))			  
                	  Z32 = X(3,IXTG(4,N))-X(3,IXTG(3,N))			  
                	  IF (IREP > 0) THEN					  
                	    E11 = X21						  
                	    E12 = Y21						  
                	    E13 = Z21						  
                	    E21 = X31						  
                	    E22 = Y31						  
                	    E23 = Z31						  
                	  ENDIF 						  
                	  E1X= X21						  
                	  E1Y= Y21						  
                	  E1Z= Z21						  
                	  X2L = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)			  
                	  E1X=E1X/X2L						  
                	  E1Y=E1Y/X2L						  
                	  E1Z=E1Z/X2L						  
C
                	  E3X=Y31*Z32-Z31*Y32					  
                	  E3Y=Z31*X32-X31*Z32					  
                	  E3Z=X31*Y32-Y31*X32					  
                	  SUM_ = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)			  
                	  E3X=E3X/SUM_						  
                	  E3Y=E3Y/SUM_						  
                	  E3Z=E3Z/SUM_						  
                	  AREA = HALF * SUM_					  
                	  E2X=E3Y*E1Z-E3Z*E1Y					  
                	  E2Y=E3Z*E1X-E3X*E1Z					  
                	  E2Z=E3X*E1Y-E3Y*E1X					  
                	  SUM_ = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)			  
                	  E2X=E2X/SUM_						  
                	  E2Y=E2Y/SUM_						  
                	  E2Z=E2Z/SUM_						  
                	  IF (IREP >= 1) THEN	 
                	    DIR1_1 = BUFLY%DIRA(I)			    
                	    DIR1_2 = BUFLY%DIRA(I+NEL)				  
                	    V1 = AA*E11 + BB*E21				  
                	    V2 = AA*E12 + BB*E22				  
                	    V3 = AA*E13 + BB*E23				  
                	    VR = V1*E1X + V2*E1Y + V3*E1Z			  
                	    VS = V1*E2X + V2*E2Y + V3*E2Z			  
                	    SUMA=SQRT(VR*VR + VS*VS)				  
                	    DIR1_1 = VR/SUMA					  
                	    DIR1_2 = VS/SUMA					  
                	  ELSE  			    
                	    DIR1_1 = BUFLY%DIRA(I)			    
                	    DIR1_2 = BUFLY%DIRA(I+NEL)				  
                	  ENDIF 					
                	  PHI =(HUNDRED80/PI)*ATAN2(DIR1_2,DIR1_1)  
                          ERR = (ABS(PHI) - NINTY)/NINTY
                          VALUE(I) = PHI
                          IF(ABS(ERR) < EM02) VALUE(I) = SIGN(NINTY,PHI)
                          IF(ABS(VALUE(I)) < ONE) VALUE(I) = ZERO
                          IS_WRITTEN_VALUE(I) = 1				
                	ENDDO	
                       ENDIF ! IDRAPE 						  
                      ENDIF							  
                     ENDIF		
                    ENDIF
c
                 ENDIF
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'EPSP' .AND. MLW  /= 15 .AND. MLW /= 25 ) THEN  ! anim/shell/epsp/
C--------------------------------------------------
c ILAYER=NULL NPT=NULL
              IF(MPT == 0 .AND.GBUF%G_PLA > 0 .AND. IPT == 1) THEN 
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	LBUF => BUFLY%LBUF(IR,IS,1)
                    	DO I=1,NEL
                    	  VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
              ELSEIF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1.and. GBUF%G_PLA > 0) THEN
                ! for law25, plastic work < 0 if the layer has reached failure-p
                IF (NLAY > 1) THEN
                  IPT = IABS(NLAY)/2 + 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                 IF (BUFLY%L_PLA > 0) THEN
                  IF (NPG > 1) THEN
                    DO  I=1,NEL  
                      VALUE(I) = ABS(BUFLY%PLAPT(I))
                      IS_WRITTEN_VALUE(I) = 1				
                    ENDDO
                  ELSE
                    NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                    DO IT=1,NPTT
                      DO  I=1,NEL 
                        VALUE(I) = VALUE(I) + ABS(BUFLY%LBUF(1,1,IT)%PLA(I))/NPTT
                        IS_WRITTEN_VALUE(I) = 1	
                      ENDDO			
                    ENDDO
                  ENDIF
                 ENDIF
                ELSEIF (NPG > 1) THEN   ! NLAY = 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                  IPT = IABS(NPTT)/2 + 1
                  II = (IPT-1)*NEL
                  DO  I=1,NEL
                   VALUE(I) = ABS(BUFLY%PLAPT(II+I))
                   IS_WRITTEN_VALUE(I) = 1				
                  ENDDO
                 ENDIF
                ELSE   !  NLAY=1, NPG=1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
cc                  IPT = IABS(NPT)/2 + 1
                  IPT = IABS(NPTT)/2 + 1
                  DO  I=1,NEL
                    VALUE(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I)) 
                    IS_WRITTEN_VALUE(I) = 1				
                  ENDDO
                 ENDIF
                ENDIF

c PLY=IPLY NPT=IPT
              ELSEIF ( IPLY > 0 .AND. (IPT <= MPT .AND. IPT > 0 ) .AND. GBUF%G_PLA > 0) THEN
c
	        DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	            ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      NPTT = BUFLY%NPTT 
                      IF( IPT <= NPTT) THEN
                	IF( NPG > 1 ) THEN
                	  DO IR=1,NPTR
                	    DO IS=1,NPTS
                	      DO  I=1,NEL 			 
                	        VALUE(I) = VALUE(I) + ABS(BUFLY%LBUF(IR,IS,IPT)%PLA(I))/NPG  
                                IS_WRITTEN_VALUE(I) = 1	
                	      ENDDO  
                	    ENDDO  
                	  ENDDO
                	ELSE
                	  DO  I=1,NEL				   
                	    VALUE(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I)) 
                            IS_WRITTEN_VALUE(I) = 1				
                	  ENDDO
                	ENDIF
                      ENDIF
                    ENDIF 
                  ENDIF 
                ENDDO

c PLY=IPLY NPT=NULL
              ELSEIF ( IPLY > 0  .AND. IPT == -1 .AND. GBUF%G_PLA > 0) THEN
c
	        DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	            ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      NPTT = BUFLY%NPTT 
	              DO IPT=1,NPTT
                        IF( IPT <= NPTT) THEN
                     	  IF( NPG > 1 ) THEN
                     	    DO IR=1,NPTR
                     	      DO IS=1,NPTS
                     	    	DO  I=1,NEL			   
                     	          VALUE(I) = VALUE(I) + ABS(BUFLY%LBUF(IR,IS,IPT)%PLA(I))/NPG  
                            	  IS_WRITTEN_VALUE(I) = 1 
                                ENDDO			      
                     	      ENDDO  
                     	    ENDDO
                     	  ELSE
                     	    DO  I=1,NEL 			     
                     	      VALUE(I) =  VALUE(I) + ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))  
                              IS_WRITTEN_VALUE(I) = 1				
                     	    ENDDO
                     	  ENDIF
                        ENDIF
                      ENDDO
                    ENDIF 
                  ENDIF 
                ENDDO


c ILAYER= NPT=
              ELSEIF ( (ILAY <= NLAY .AND. ILAY > 0) .AND. (IPT <= MPT .AND. IPT > 0 ) .AND. GBUF%G_PLA > 0) THEN
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                    	DO I=1,NEL
                          VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                    	  IS_WRITTEN_VALUE(I) = 1
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c ILAYER=IL NPT=NULL
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0 .AND. GBUF%G_PLA > 0) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	LBUF => BUFLY%LBUF(IR,IS,1)
                    	DO I=1,NEL
                    	  VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IT=1,NPTT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => BUFLY%LBUF(IR,IS,IT)
                    	  DO I=1,NEL
                            VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                    	    IS_WRITTEN_VALUE(I) = 1
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF

                ENDIF
c  ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0 .AND. GBUF%G_PLA > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	LBUF => BUFLY%LBUF(IR,IS,IPT)
                    	DO I=1,NEL
                    	  VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
C--------------------------------------------------
c            ELSEIF (KEYWORD == '') THEN  ! output Damage IFUNC == 10253
C--------------------------------------------------
c              DO IL=1,NLAY							       
c            	NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL									
c            	DO IS=1,NPTS							       
c            	  DO IT=1,NPTT  						       
c            	    DO IR=1,NPTR						       
c            	      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)
c            	      DO IFAIL=1,NFAIL
c            		IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model
c            		  DO I=1,NEL						       
c            		    VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))			  
c            		  ENDDO
c            		ENDIF							       
c            	      ENDDO							       
c            	    ENDDO							       
c            	  ENDDO 							       
c            	ENDDO								       
c              ENDDO
C--------------------------------------------------
c            ELSEIF (KEYWORD == '') THEN  ! output Sig1 IFUNC == 10254
C-------------------------------------------------- 
c              DO IL=1,NLAY								
c            	NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL									
c            	DO IS=1,NPTS								
c            	  DO IT=1,NPTT  							
c            	    DO IR=1,NPTR						       
c            	      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)		    
c            	      DO IFAIL=1,NFAIL  					    
c            		IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model    
c            		  NVARF = FBUF%FLOC(IFAIL)%NVAR 			    
c            		  DO I=1,NEL						  
c            		    VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+1)  ! Sig1						   
c            		    VALUE(I) = MAX(VALUE(I), VAR)				    
c            		  ENDDO 						    
c            		ENDIF							       
c            	      ENDDO								
c            	    ENDDO								
c            	  ENDDO 								
c            	ENDDO								       
c              ENDDO
C--------------------------------------------------								    
c            ELSEIF (KEYWORD == '') THEN  ! output Sig2  IFUNC == 10255
C-------------------------------------------------- 
c              DO IL=1,NLAY								      
c            	NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL									
c            	DO IS=1,NPTS								      
c            	  DO IT=1,NPTT  							      
c            	    DO IR=1,NPTR							      
c            	      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)			      
c            	      DO IFAIL=1,NFAIL  						      
c            		IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model	      
c            		  NVARF = FBUF%FLOC(IFAIL)%NVAR 				      
c            		  DO I=1,NEL							    
c            		    VAR = FBUF%FLOC(IFAIL)%VAR(NVARF*(I-1)+2)  ! Sig2						  
c            		    VALUE(I) = MAX(VALUE(I), VAR)					    
c            		  ENDDO 							      
c            		ENDIF								      
c            	      ENDDO								      
c            	    ENDDO								      
c            	  ENDDO 								      
c            	ENDDO									      
c              ENDDO				   
C--------------------------------------------------
            ELSEIF (KEYWORD == 'WPLA' .AND.(MLW == 15 .OR. MLW == 25)  ) THEN  ! anim/shell/WPLA/
C--------------------------------------------------
c ILAYER=NULL NPT=NULL
              IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1.and. GBUF%G_PLA > 0) THEN
                ! for law25, plastic work < 0 if the layer has reached failure-p
                IF (NLAY > 1) THEN
                  IPT = IABS(NLAY)/2 + 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                 IF (BUFLY%L_PLA > 0) THEN
                  IF (NPG > 1) THEN
                    DO  I=1,NEL  
                      VALUE(I) = ABS(BUFLY%PLAPT(I))
                      IS_WRITTEN_VALUE(I) = 1				
                    ENDDO
                  ELSE
                    NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                    DO IT=1,NPTT
                      DO  I=1,NEL 
                        VALUE(I) = VALUE(I) + ABS(BUFLY%LBUF(1,1,IT)%PLA(I))/NPTT
                        IS_WRITTEN_VALUE(I) = 1	
                      ENDDO			
                    ENDDO
                  ENDIF
                 ENDIF
                ELSEIF (NPG > 1) THEN   ! NLAY = 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
                  IPT = IABS(NPTT)/2 + 1
                  II = (IPT-1)*NEL
                  DO  I=1,NEL
                   VALUE(I) = ABS(BUFLY%PLAPT(II+I))
                   IS_WRITTEN_VALUE(I) = 1				
                  ENDDO
                 ENDIF
                ELSE   !  NLAY=1, NPG=1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                 IF (BUFLY%L_PLA > 0) THEN
                  NPTT = BUFLY%NPTT  ! needed for PID51 (new shell prop)
cc                  IPT = IABS(NPT)/2 + 1
                  IPT = IABS(NPTT)/2 + 1
                  DO  I=1,NEL
                    VALUE(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I)) 
                    IS_WRITTEN_VALUE(I) = 1				
                  ENDDO
                 ENDIF
                ENDIF

c PLY=IPLY NPT=IPT
              ELSEIF ( IPLY > 0 .AND. (IPT <= MPT .AND. IPT > 0 ) .AND. GBUF%G_PLA > 0) THEN
c
	        DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	            ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      NPTT = BUFLY%NPTT 
                      IF( IPT <= NPTT) THEN
                	IF( NPG > 1 ) THEN
                	  DO IR=1,NPTR
                	    DO IS=1,NPTS
                	      DO  I=1,NEL 			 
                	        VALUE(I) = VALUE(I) + ABS(BUFLY%LBUF(IR,IS,IPT)%PLA(I))/NPG  
                                IS_WRITTEN_VALUE(I) = 1	
                	      ENDDO  
                	    ENDDO  
                	  ENDDO
                	ELSE
                	  DO  I=1,NEL				   
                	    VALUE(I) = ABS(BUFLY%LBUF(1,1,IPT)%PLA(I)) 
                            IS_WRITTEN_VALUE(I) = 1				
                	  ENDDO
                	ENDIF
                      ENDIF
                    ENDIF 
                  ENDIF 
                ENDDO

c PLY=IPLY NPT=NULL
              ELSEIF ( IPLY > 0  .AND. IPT == -1 .AND. GBUF%G_PLA > 0) THEN
c
	        DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	            ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      NPTT = BUFLY%NPTT 
	              DO IPT=1,NPTT
                        IF( IPT <= NPTT) THEN
                     	  IF( NPG > 1 ) THEN
                     	    DO IR=1,NPTR
                     	      DO IS=1,NPTS
                     	    	DO  I=1,NEL			   
                     	          VALUE(I) = VALUE(I) + ABS(BUFLY%LBUF(IR,IS,IPT)%PLA(I))/NPG  
                            	  IS_WRITTEN_VALUE(I) = 1 
                                ENDDO			      
                     	      ENDDO  
                     	    ENDDO
                     	  ELSE
                     	    DO  I=1,NEL 			     
                     	      VALUE(I) =  VALUE(I) + ABS(BUFLY%LBUF(1,1,IPT)%PLA(I))  
                              IS_WRITTEN_VALUE(I) = 1				
                     	    ENDDO
                     	  ENDIF
                        ENDIF
                      ENDDO
                    ENDIF 
                  ENDIF 
                ENDDO


c ILAYER= NPT=
              ELSEIF ( (ILAY <= NLAY .AND. ILAY > 0) .AND. (IPT <= 10 .AND. IPT > 0 ) .AND. GBUF%G_PLA > 0) THEN
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  NPTT  =  BUFLY%NPTT
                  IF ((BUFLY%L_PLA > 0).AND.(IPT <= NPTT)) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                    	DO I=1,NEL
                          VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                    	  IS_WRITTEN_VALUE(I) = 1
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c ILAYER=IL NPT=NULL
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0 .AND. GBUF%G_PLA > 0) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	LBUF => BUFLY%LBUF(IR,IS,1)
                    	DO I=1,NEL
                    	  VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IT=1,NPTT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => BUFLY%LBUF(IR,IS,IT)
                    	  DO I=1,NEL
                            VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                    	    IS_WRITTEN_VALUE(I) = 1
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF

                ENDIF
c  ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0 .AND. GBUF%G_PLA > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                  IF (BUFLY%L_PLA > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	LBUF => BUFLY%LBUF(IR,IS,IPT)
                    	DO I=1,NEL
                    	  VALUE(I) = VALUE(I) + ABS(LBUF%PLA(I))/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
C--------------------------------------
            ELSEIF (KEYWORD == 'NXTF') THEN ! Damage factor 
C--------------------------------------------------
            IOK = 0
c ILAYER=NULL NPT=NULL
              IF ( ILAY == -1 .AND. IPT == -1) THEN                                                         
                NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                                                   
                NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    DO IT=1,NPTT
                      IPT = IT
                      FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model 
                          IOK = 1               
                          DO I=1,NEL                                                          
                            VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))   
                            IS_WRITTEN_VALUE(I) = 1				                       
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO
                    ENDDO                                                                     
                  ENDDO                                                                       
                ENDDO  
c ILAYER=ILAY NPT=IPT 
              ELSEIF (ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN       
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN                                                         
                  NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL 								  
                  NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO IS=1,NPTS  								
                    DO IR=1,NPTR
                      FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IPT)				 
                      DO IFAIL=1,NFAIL  							
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model 
                          IOK = 1		
                          DO I=1,NEL							      
                            VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))
                            IS_WRITTEN_VALUE(I) = 1							  
                          ENDDO 								
                        ENDIF									
                      ENDDO									
                    ENDDO									
                  ENDDO  
                ENDIF
c ILAYER=IL NPT=NULL
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN                                                       
                  NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL 								  
                  NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO IS=1,NPTS  								
                    DO IR=1,NPTR
                      FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,1)				 
                      DO IFAIL=1,NFAIL  							
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model 
                          IOK = 1		
                          DO I=1,NEL							      
                            VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))	
                            IS_WRITTEN_VALUE(I) = 1						  
                          ENDDO 								
                        ENDIF									
                      ENDDO									
                    ENDDO									
                  ENDDO 
                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN                                                       
                  NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL 								  
                  NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO IS=1,NPTS  								
                    DO IR=1,NPTR								
                      DO IPT=1,NPTT
                        FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IPT)				 
                        DO IFAIL=1,NFAIL  							
                          IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model 
                            IOK = 1		
                            DO I=1,NEL							      
                              VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))
                              IS_WRITTEN_VALUE(I) = 1						  
                            ENDDO 								
                          ENDIF									
                        ENDDO									
                      ENDDO									
                    ENDDO									
                  ENDDO 
                ENDIF
c  ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN                                                       
                  NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL 								  
                  NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  DO IS=1,NPTS  								
                    DO IR=1,NPTR
                      FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)			       
                      DO IFAIL=1,NFAIL  						      
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model 
                          IOK = 1	      
                          DO I=1,NEL							    
                            VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))
                            IS_WRITTEN_VALUE(I) = 1			
                          ENDDO 							      
                        ENDIF							      
                      ENDDO									
                    ENDDO									
                  ENDDO 
                ENDIF 
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'NXTF/MEMB') THEN ! Damage factor - membrane
C--------------------------------------------------
            IOK = 0
              IF (NLAY > 1) THEN
                IL  = IABS(NLAY) / 2
                IPT = 1
              ELSE
                IL  = 1
                IPT = IABS(NPTT) / 2
              ENDIF						 
              NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL								      
              DO IS=1,NPTS								    
                DO IR=1,NPTR								    
                  DO IT=1,NPTT
                    IPT = IT
                    IF (NLAY == 1) IPT = IABS(NPTT) / 2
                    FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)			       
                    DO IFAIL=1,NFAIL							      
                      IF (FBUF%FLOC(IFAIL)%ILAWF == 25) THEN ! check NXT model    
                	IOK = 1 			  
                	DO I=1,NEL							    
                	  VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))  
                          IS_WRITTEN_VALUE(I) = 1 			
                	ENDDO								      
                      ENDIF								      
                    ENDDO								      
                  ENDDO
                ENDDO									    
              ENDDO 
C--------------------------------------------------
            ELSE IF (KEYWORD == 'FAIL') THEN   ! FAIL
C--------------------------------------------------
              IF ((MLW == 25.OR.MLW == 15).AND.(IGTYP == 10.
     .            OR.IGTYP == 11.OR.IGTYP == 17.OR.IGTYP == 51 
     .                                         .OR. IGTYP == 52)) THEN 
                  FAILG = 0
                  DO I=1,NEL
                    DAM1(I)=ZERO
                    DAM2(I)=ZERO 
                    WPLA(I)=ZERO
                    FAIL(I)=ZERO
                  END DO                                
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
                  IF (IGTYP == 11) THEN
                    IPMAT = 100                               
                    DO N=1,NPT
                      IADR = (N-1)*NEL                       
                      DO I=1,NEL
                        J = IADR+I                            
                        MATLY(J) = IGEO(IPMAT+N,PID(I))
                      END DO                                
                    END DO                                
                  ELSEIF (IGTYP == 10) THEN 
                    DO N=1,NPT
                      IADR = (N-1)*NEL
                      DO I=1,NEL
                        J = IADR+I 
                        MATLY(J)=MAT(I)
                      END DO
                    END DO 
                  ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                      IPMAT = 2 + NLAY                         
                    DO N=1,NLAY
                      IADR = (N-1)*NEL                       
                      DO I=1,NEL
                        J = IADR+I                            
                        MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                      END DO                                
                    END DO        
                  END IF
c
                  IF (IFAILURE == 0 .AND. IHBE == 11) THEN
                    DO IL=1,NLAY
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                      IADR = (IL-1)*NEL
                      DO IT=1,NPTT
                        DO I=1,NEL
                          WPLA(I) = ZERO
                        ENDDO
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                            IF (BUFLY%L_DAM > 0) THEN
                              DO I=1,NEL                                    
                                J = IADR + I                                  
                                DAM1(I)=LBUF%DAM(JJ(1)+I)                          
                                DAM2(I)=LBUF%DAM(JJ(2)+I)                          
                                WPLA(I) = WPLA(I) + ABS(LBUF%PLA(I))/NPG           
                                DMAX(I) = PM(64,MATLY(J))                     
                                WPMAX(I)= PM(41,MATLY(J))                     
                               IF (DAM1(I) >= DMAX(I).OR.DAM2(I) >= DMAX(I)  
     .                             .OR.WPLA(I) < ZERO.OR.WPLA(I) >= WPMAX(I))  
     .                             FAILG(IL,I) = FAILG(IL,I) + 1                 
                               IF (FAILG(IL,I) == NPG ) THEN                  
                                 FAIL(I) = FAIL(I) + ONE                      
                                 FAILG(IL,I) = NPG + 1                        
                               ENDIF                                         
                              ENDDO
                            ENDIF
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO  !  DO IL=1,NLAY
                    DO  I=1,NEL
                      VALUE(I) = FAIL(I)
                      IS_WRITTEN_VALUE(I) = 1
                    ENDDO
                  ELSEIF (IFAILURE == 0) THEN
                    DO IL=1,NLAY
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                      IADR = (IL-1)*NEL
                      DO IT=1,NPTT
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IT)
                        IF (BUFLY%L_DAM > 0) THEN
                          DO I=1,NEL                                       
                            J = IADR + I               
                            DAM1(I)=LBUF%DAM(JJ(1)+I)			       
                            DAM2(I)=LBUF%DAM(JJ(2)+I)  
                            WPLA(I) = ABS(LBUF%PLA(I))                         
                            DMAX(I) = PM(64,MATLY(J))                        
                            WPMAX(I)= PM(41,MATLY(J))                        
                            IF (DAM1(I) >= DMAX(I).OR.DAM2(I) >= DMAX(I).OR.  
     .                          WPLA(I) < ZERO.OR.WPLA(I) >= WPMAX(I))          
     .                      FAIL(I) = FAIL(I) + ONE  
                          ENDDO
                        ENDIF
                      ENDDO
                    ENDDO  !  DO IL=1,NLAY
                    DO  I=1,NEL
                      VALUE(I) = FAIL(I)
                      IS_WRITTEN_VALUE(I) = 1
                    ENDDO
c                  ELSE
c                    IF (IFAILA == 1) THEN
c                      DO  I=1,NEL
c                        OFF = GBUF%OFF(I)
c                        IF (OFF<ZERO) THEN
c                          FAIL(I)= -ONE
c                        ELSEIF (OFF>ZERO) THEN
c                          FAIL(I)=  ONE
c                        ELSE
c                          FAIL(I)=  ZERO
c                        END IF
c                        VALUE(I) = FAIL(I)
c                        IS_WRITTEN_VALUE(I) = 1
c                      END DO 
c                    ENDIF 
                  ENDIF
c                 
              ELSE  ! MLW /= 25 
c                IF (ITY == 3) THEN                                         
c                  DO I=1,NEL                                           
c                    MAT(I)=IXC(1,NFT+I)                                  
c                    PID(I)=IXC(6,NFT+I)                                  
c                  END DO                                                 
c                ELSE                                                     
c                  DO I=1,NEL                                           
c                    MAT(I)=IXTG(1,NFT+I)                                 
c                    PID(I)=IXTG(5,NFT+I)                                 
c                  END DO                                                 
c                END IF                                                   
c                IF (IGTYP == 11 .OR. IGTYP == 16) THEN                     
c                  IPMAT = 100                                            
c                  DO N=1,NPT                                             
c                    IADR = (N-1)*NEL                                     
c                    DO I=1,NEL                                         
c                      J = IADR+I                                         
c                      MATLY(J)   = IGEO(IPMAT+N,PID(I))                  
c                    END DO                                               
c                  END DO                                                 
c                ENDIF                                                    
c                IF (IFAILURE==0 .OR.(IFAILURE /=0 .AND.IFAILA==1)) THEN
c                  DO  I=1,NEL                                         
c                    OFF = GBUF%OFF(I)                                  
c                    IF (OFF<ZERO)THEN                                  
c                      FAIL(I)= -ONE                                       
c                    ELSEIF(OFF>ZERO)THEN                              
c                      FAIL(I)=  ONE                                       
c                    ELSE                                                 
c                      FAIL(I)=  ZERO                                     
c                    END IF    
c                    VALUE(I) = FAIL(I)
c                    IS_WRITTEN_VALUE(I) = 1                             
c                  END DO                                         
c                ENDIF                                                    
              ENDIF ! MLW , IGTYP
C-------------------------------------------------- 
            ELSE IF (KEYWORD == 'DAMA') THEN 
C-------------------------------------------------- 
              IF(MLW == 25 .AND. (IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .           IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 )) THEN   
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
                IF (IGTYP == 11) THEN
                  IPMAT = 100				    
                  DO N=1,NPT
                    IADR = (N-1)*NEL			   
                    DO I=1,NEL
                      J = IADR+I			    
                      MATLY(J) = IGEO(IPMAT+N,PID(I))
                    END DO				  
                  END DO				
                ELSEIF (IGTYP == 10) THEN 
                  DO N=1,NPT
                    IADR = (N-1)*NEL
                    DO I=1,NEL
                      J = IADR+I 
                      MATLY(J)=MAT(I)
                    END DO
                  END DO 
                ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                  IPMAT = 2 + NLAY			   
                  DO N=1,NLAY
                    IADR = (N-1)*NEL			   
                    DO I=1,NEL
                      J = IADR+I			    
                      MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                    END DO				  
                  END DO	
                END IF
              ENDIF
c
c IPLY=NULL ILAYER=NULL NPT=NULL
              IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
                IF(IFAILURE > 0) THEN
                  IF (NLAY > 1) THEN							      
                    DO I=1,NEL  							    
                      DO N = 1,NLAY
                	NPTT = ELBUF_TAB(NG)%BUFLY(N)%NPTT
                	DO IT = 1,NPTT
                	  DMGMX = ZERO
                	  DO IR = 1,NPTR						       
                	    DO IS = 1,NPTS						       
                	      FBUF => ELBUF_TAB(NG)%BUFLY(N)%FAIL(IR,IS,IT)			 
                	      DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(N)%NFAIL 					      
                		DMGMX = MAX(DMGMX,FBUF%FLOC(IFAIL)%DAMMX(I))
                	      ENDDO								
                	    ENDDO
                	  ENDDO
                	  VALUE(I) = VALUE(I) + DMGMX/NPTT
                	ENDDO  !  DO IT = 1,NPTT
                      ENDDO   !  N=1,NLAY							   
                      VALUE(I) = VALUE(I) / NLAY
                      IS_WRITTEN_VALUE(I) = 1						       
                    ENDDO                                                                     

                  ELSEIF (MPT > 0) THEN  ! NLAY = 1						    
                   NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                   DO I=1,NEL								   
                     DO IT = 1,NPTT
                       DMGMX = ZERO
                       DO IR = 1,NPTR							    
                         DO IS = 1,NPTS 						    
                           FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IT)		      
                           DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(1)%NFAIL						   
                             DMGMX = MAX(DMGMX, FBUF%FLOC(IFAIL)%DAMMX(I))			
                           ENDDO							     
                         ENDDO  							     
                       ENDDO								     
                       VALUE(I) = VALUE(I) + DMGMX  					       
                     ENDDO   !  N=1,NPTT							  
                     VALUE(I) = VALUE(I) / NPTT
                     IS_WRITTEN_VALUE(I) = 1
                   ENDDO     !  I=1,NEL
                  ENDIF     
                ENDIF
                IF(MLW == 25 .AND. (IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .            IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 )) THEN  
C
                  DO I=1,NEL  
                    VE(1:5) = ZERO
                    DO IL=1,NLAY
                     NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                     BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                     IADR = (IL-1)*NEL  			 
                     J = IADR + I 
                     VLY(1:5) = ZERO
                     DO IT=1,NPTT
                       VG(1:5)= ZERO
                       DO IR=1,NPTR
                  	 DO IS=1,NPTS
                  	   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                  	   DMAX(I) = ONE/PM(64,MATLY(J)) 			      
                  	   WPMAX(I)= ONE/PM(41,MATLY(J)) 			      
                  	   EPST1(I)= PM(60,MATLY(J))				   
                  	   EPST2(I)= PM(61,MATLY(J))				   
                  	   EPSF1(I)= ONE/PM(98,MATLY(J)) 			      
                  	   EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                  	   VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                  	   VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                  	   VG(3)=MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                  	   IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .            		   (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                  	   IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .            		   (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))		 
                  	ENDDO
                       ENDDO
                  	VLY(1) = VLY(1) + VG(1)
                  	VLY(2) = VLY(2) + VG(2)
                  	VLY(3) = VLY(3) + VG(3)
                  	VLY(4) = VLY(4) + VG(4)
                  	VLY(5) = VLY(5) + VG(5)
                     ENDDO ! NPTT
                       VE(1)  = VE(1) + VLY(1)/NPTT
                       VE(2)  = VE(2) + VLY(2)/NPTT
                       VE(3)  = VE(3) + VLY(3)/NPTT
                       VE(4)  = VE(4) + VLY(4)/NPTT
                       VE(5)  = VE(5) + VLY(5)/NPTT
                   ENDDO  !   DO IL=1,NLAY 
                       VE(1) = VE(1)/NLAY 
                       VE(2) = VE(2)/NLAY 
                       VE(3) = VE(3)/NLAY 
                       VE(4) = VE(4)/NLAY 
                       VE(5) = VE(5)/NLAY 
                       VALUE(I) = MAX(VALUE(I),VE(1),VE(2),VE(3),
     .            				   VE(4),VE(5)) 
                       IS_WRITTEN_VALUE(I) = 1
                  ENDDO  ! I=1,JLT
                ENDIF ! law 25  + SHell Composite  PID
              ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN
c PLY=IPLY NPT=IPT
                IF(IFAILURE > 0) THEN 
	          DO J=1,NLAY
                    NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	              ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY=PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF 
                    IF (ID_PLY  == IPLY )THEN
                      IF (IPT <= NPTT) THEN						
                        DO I=1,NEL							    
                          DO IR = 1, NPTR						    
                            DO IS = 1, NPTS							    
                              FBUF => ELBUF_TAB(NG)%BUFLY(J)%FAIL(IR,IS,IPT)			      
                              DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(J)%NFAIL  
                        	VALUE(I) = MAX(VALUE(I) , FBUF%FLOC(IFAIL)%DAMMX(I))	   
                              ENDDO								    
                            ENDDO							    
                          ENDDO 
                          IS_WRITTEN_VALUE(I) = 1							    
                        ENDDO 
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
c
                IF(MLW == 25 .AND. (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52)) THEN  
	          DO J=1,NLAY
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	              ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                     ID_PLY=PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF
c
                    IF (ID_PLY  == IPLY )THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      DO I=1,NEL  
                        NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                        IADR = (J - 1)*NEL
                        VLY(1:5) = ZERO
                        VG(1:5)= ZERO
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            LBUF=> ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT)  
                            DMAX(I) = ONE/PM(64,MATLY(IADR + I)) 		       
                            WPMAX(I)= ONE/PM(41,MATLY(IADR + I)) 		       
                            EPST1(I)= PM(60,MATLY(IADR + I))				    
                            EPST2(I)= PM(61,MATLY(IADR + I))				    
                            EPSF1(I)= ONE/PM(98,MATLY(IADR + I)) 		       
                            EPSF2(I)= ONE/PM(99,MATLY(IADR + I))
C   
                            VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                            VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                            VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                            IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .                  		(LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                            IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .                  		(LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))  
                          ENDDO
                        ENDDO
                        VLY(1) =VG(1) 
                        VLY(2) =VG(2) 
                        VLY(3) =VG(3) 
                        VLY(4) =VG(4) 
                        VLY(5) =VG(5) 
C                       
                        VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),VLY(4),VLY(5))
                	IS_WRITTEN_VALUE(I) = 1
                      ENDDO  ! I=1,JLT  				  

                    ENDIF 
                  ENDDO
                ENDIF
              ELSEIF ( IPLY > 0 .AND. IPT == -1 ) THEN
c PLY=IPLY NPT=-1
                IF(IFAILURE > 0) THEN 
	          DO J=1,NLAY
                    NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	              ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY=PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF 
                    IF (ID_PLY  == IPLY )THEN					
                      DO I=1,NEL							  
                        DO IR = 1, NPTR 						  
                          DO IS = 1, NPTS						  
                            DO IT = 1, NPTT							  
                              FBUF => ELBUF_TAB(NG)%BUFLY(J)%FAIL(IR,IS,IT)			    
                              DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(J)%NFAIL  
                                VALUE(I) = MAX(VALUE(I) , FBUF%FLOC(IFAIL)%DAMMX(I))	 
                              ENDDO								  
                            ENDDO 									  
                          ENDDO 							  
                        ENDDO 
                        IS_WRITTEN_VALUE(I) = 1 							  
                      ENDDO 
                    ENDIF
                  ENDDO
                ENDIF
c
                IF(MLW == 25 .AND. (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52)) THEN  
	          DO J=1,NLAY
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	              ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                     ID_PLY=PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF
c
                    IF (ID_PLY  == IPLY )THEN
                      BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                      DO I=1,NEL  
                        NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                        IADR = (J - 1)*NEL
                        VLY(1:5) = ZERO
                        VG(1:5)= ZERO
                        DO IR=1,NPTR
                          DO IS=1,NPTS
                            DO IT=1,NPTT
                              LBUF=> ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IT)  
                              DMAX(I) = ONE/PM(64,MATLY(IADR + I))			 
                              WPMAX(I)= ONE/PM(41,MATLY(IADR + I))			 
                              EPST1(I)= PM(60,MATLY(IADR + I))  			      
                              EPST2(I)= PM(61,MATLY(IADR + I))  			      
                              EPSF1(I)= ONE/PM(98,MATLY(IADR + I))			 
                              EPSF2(I)= ONE/PM(99,MATLY(IADR + I))
C   
                              VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                              VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                              VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I))
                              IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .                     	  	  (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                              IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .                     	  	  (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))  
                            ENDDO
                          ENDDO
                        ENDDO
                        VLY(1) =VG(1) 
                        VLY(2) =VG(2) 
                        VLY(3) =VG(3) 
                        VLY(4) =VG(4) 
                        VLY(5) =VG(5) 
C                       
                        VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),VLY(4),VLY(5))
                	IS_WRITTEN_VALUE(I) = 1
                      ENDDO  ! I=1,JLT  				  

                    ENDIF 
                  ENDDO
                ENDIF
c ILAYER=ILAY NPT=IPT 
              ELSEIF (ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN  
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN  				  
                  NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO I=1,NEL  
                    DMGMX = ZERO
                    DO IR = 1,NPTR							   
                      DO IS = 1,NPTS							   
                	FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IPT)		       
                	DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL						    
                	  VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAMMX(I))
                	  IS_WRITTEN_VALUE(I) = 1
                	ENDDO								    
                      ENDDO
                    ENDDO								  
                    VALUE(I) = VALUE(I) + DMGMX
                  ENDDO     !  I=1,NEL
                  IF(MLW == 25) THEN		       
                    DO I=1,NEL  					  
                      K1 = 2*I-1					      
                      K2 = 2*I 
                       NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                       BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                       IADR = (IPT - 1)*NEL			      
                       J = IADR + I 
                       VLY(1:5) = ZERO
                	 VG(1:5)= ZERO
                	 DO IR=1,NPTR
                	   DO IS=1,NPTS
                	     LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IPT)        
                	     DMAX(I) = ONE/PM(64,MATLY(J))				
                	     WPMAX(I)= ONE/PM(41,MATLY(J))				
                	     EPST1(I)= PM(60,MATLY(J))  			     
                	     EPST2(I)= PM(61,MATLY(J))  			     
                	     EPSF1(I)= ONE/PM(98,MATLY(J))				
                	     EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                	     VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                	     VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                	     VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                	     IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .          		     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                	     IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .          		     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                	  ENDDO
                	 ENDDO
                	 VLY(1) =VLY(1) + VG(1) 
                	 VLY(2) =VLY(2) + VG(2) 
                	 VLY(3) =VLY(3) + VG(3) 
                	 VLY(4) =VLY(4) + VG(4) 
                	 VLY(5) =VLY(5) + VG(5)
C               	 
                	VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),VLY(4),VLY(5))
                	IS_WRITTEN_VALUE(I) = 1
                    ENDDO  ! I=1,JLT
                  ENDIF
                ENDIF
c ILAYER=IL NPT=NULL
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0) THEN
                IPT = 1
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  DO I=1,NEL	      
                    DMGMX = ZERO
                    DO IR = 1,NPTR							   
                      DO IS = 1,NPTS							   
                	FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,1) 		       
                	DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL						    
                	  DMGMX = MAX(DMGMX,FBUF%FLOC(IFAIL)%DAMMX(I))
                	  IS_WRITTEN_VALUE(I) = 1
                	ENDDO								    
                      ENDDO
                    ENDDO								  
                    VALUE(I) = VALUE(I) + DMGMX
                  ENDDO     !  I=1,NEL
                  IF(MLW == 25) THEN		       
                    DO I=1,NEL  					  
                      K1 = 2*I-1					      
                      K2 = 2*I 
                       BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                       IADR = (IPT - 1)*NEL			      
                       J = IADR + I 
                       VLY(1:5) = ZERO
                	 VG(1:5)= ZERO
                	 DO IR=1,NPTR
                	   DO IS=1,NPTS
                	     LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,1)	    
                	     DMAX(I) = ONE/PM(64,MATLY(J))				
                	     WPMAX(I)= ONE/PM(41,MATLY(J))				
                	     EPST1(I)= PM(60,MATLY(J))  			     
                	     EPST2(I)= PM(61,MATLY(J))  			     
                	     EPSF1(I)= ONE/PM(98,MATLY(J))				
                	     EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                	     VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                	     VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                	     VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                	     IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .          		     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                	     IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .          		     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                	  ENDDO
                	 ENDDO
                	  VLY(1) =VLY(1) + VG(1) 
                	  VLY(2) =VLY(2) + VG(2) 
                	  VLY(3) =VLY(3) + VG(3) 
                	  VLY(4) =VLY(4) + VG(4) 
                	  VLY(5) =VLY(5) + VG(5) 
                	VLY(1) =VLY(1)/NPTT
                	VLY(2) =VLY(2)/NPTT
                	VLY(3) =VLY(3)/NPTT
                	VLY(4) =VLY(4)/NPTT
                	VLY(5) =VLY(5)/NPTT
C               	 
                	VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),
     .          				   VLY(4),VLY(5))
                	IS_WRITTEN_VALUE(I) = 1
                    ENDDO  ! I=1,JLT 
                  ENDIF

                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN				      
                  NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO I=1,NEL								  
                    DO IT = 1,NPTT
                      DMGMX = ZERO
                      DO IR = 1,NPTR							     
                	DO IS = 1,NPTS  						     
                	  FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)			 
                	  DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL  					      
                	    DMGMX = MAX(DMGMX,FBUF%FLOC(IFAIL)%DAMMX(I))
                	  ENDDO 							      
                	ENDDO
                      ENDDO								    
                      VALUE(I) = VALUE(I) + DMGMX
                      IS_WRITTEN_VALUE(I) = 1
                    ENDDO  !  DO IT = 1,NPTT
                    VALUE(I) = VALUE(I) / NPTT
                  ENDDO     !  I=1,NEL
                  IF(MLW == 25) THEN		       
                    DO I=1,NEL  					  
                      K1 = 2*I-1					      
                      K2 = 2*I 
                       NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                       BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                       IADR = (IPT - 1)*NEL			      
                       J = IADR + I 
                       VLY(1:5) = ZERO
                       DO IT=1,NPTT
                	 VG(1:5)= ZERO
                	 DO IR=1,NPTR
                	   DO IS=1,NPTS
                	     LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)	     
                	     DMAX(I) = ONE/PM(64,MATLY(J))				
                	     WPMAX(I)= ONE/PM(41,MATLY(J))				
                	     EPST1(I)= PM(60,MATLY(J))  			     
                	     EPST2(I)= PM(61,MATLY(J))  			     
                	     EPSF1(I)= ONE/PM(98,MATLY(J))				
                	     EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                	     VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                	     VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                	     VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                	     IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .          		     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                	     IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .          		     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                	  ENDDO
                	 ENDDO
                	  VLY(1) =VLY(1) + VG(1) 
                	  VLY(2) =VLY(2) + VG(2) 
                	  VLY(3) =VLY(3) + VG(3) 
                	  VLY(4) =VLY(4) + VG(4) 
                	  VLY(5) =VLY(5) + VG(5) 
                       ENDDO ! NPTT
                	VLY(1) =VLY(1)/NPTT
                	VLY(2) =VLY(2)/NPTT
                	VLY(3) =VLY(3)/NPTT
                	VLY(4) =VLY(4)/NPTT
                	VLY(5) =VLY(5)/NPTT
C               	 
                	VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),
     .          				   VLY(4),VLY(5))
                	IS_WRITTEN_VALUE(I) = 1
                    ENDDO  ! I=1,JLT
                  ENDIF
                ENDIF
c NPT=IPT
              ELSEIF ( IPT <= NPT .AND. IPT > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9 ) THEN 
                  DO I=1,NEL	      
                    DMGMX = ZERO
                    DO IR = 1,NPTR							   
                      DO IS = 1,NPTS							   
                	FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)  		       
                	DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(1)%NFAIL						    
                	  DMGMX = MAX(DMGMX,FBUF%FLOC(IFAIL)%DAMMX(I))
                	  IS_WRITTEN_VALUE(I) = 1
                	ENDDO								    
                      ENDDO
                    ENDDO								  
                    VALUE(I) = VALUE(I) + DMGMX
                  ENDDO     !  I=1,NEL
                  IF(MLW == 25) THEN		       
                    DO I=1,NEL  					  
                      K1 = 2*I-1					      
                      K2 = 2*I 
                       BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                       IADR = (IPT - 1)*NEL			      
                       J = IADR + I 
                       VLY(1:5) = ZERO
                	 VG(1:5)= ZERO
                	 DO IR=1,NPTR
                	   DO IS=1,NPTS
                	     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)	   
                	     DMAX(I) = ONE/PM(64,MATLY(J))				
                	     WPMAX(I)= ONE/PM(41,MATLY(J))				
                	     EPST1(I)= PM(60,MATLY(J))  			     
                	     EPST2(I)= PM(61,MATLY(J))  			     
                	     EPSF1(I)= ONE/PM(98,MATLY(J))				
                	     EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                	     VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                	     VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                	     VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                	     IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .          		     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                	     IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .          		     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                	  ENDDO
                	 ENDDO
                	  VLY(1) =VLY(1) + VG(1) 
                	  VLY(2) =VLY(2) + VG(2) 
                	  VLY(3) =VLY(3) + VG(3) 
                	  VLY(4) =VLY(4) + VG(4) 
                	  VLY(5) =VLY(5) + VG(5) 
                	VLY(1) =VLY(1)/NPTT
                	VLY(2) =VLY(2)/NPTT
                	VLY(3) =VLY(3)/NPTT
                	VLY(4) =VLY(4)/NPTT
                	VLY(5) =VLY(5)/NPTT
C               	 
                	VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),
     .          				   VLY(4),VLY(5))
                	IS_WRITTEN_VALUE(I) = 1
                    ENDDO  ! I=1,JLT 
                  ENDIF
                ENDIF
              ENDIF
C-------------------------------------------------- 
            ELSE IF (KEYWORD == 'DAMA/MEMB') THEN 
C-------------------------------------------------- 
              IPT = IABS(NPT)/2 + 1        
c              
              IF(IFAILURE > 0) THEN                                     
                IF (NLAY > 1) THEN                                                          
                  NPTT = ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
                  DO I=1,NEL                                                              
                    DO IT = 1,NPTT
                      DMGMX = ZERO
                      DO IR = 1,NPTR                                                         
                        DO IS = 1,NPTS                                                       
                          FBUF => ELBUF_TAB(NG)%BUFLY(IPT)%FAIL(IR,IS,IT)                        
                          DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(IPT)%NFAIL                                                 
                            VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAMMX(I))
                            IS_WRITTEN_VALUE(I) = 1
                          ENDDO                                                               
                        ENDDO
                      ENDDO                                                                 
                      VALUE(I) = VALUE(I) + DMGMX
                    ENDDO  !  DO IT = 1,NPTT
                    VALUE(I) = VALUE(I) / NPTT
                  ENDDO     !  I=1,NEL
                 ELSEIF (MPT > 0) THEN  ! NLAY = 1                                                 
                  DO I=1,NEL                                                              
                    DO IR = 1, NPTR                                                         
                      DO IS = 1, NPTS                                                       
                        FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)                        
                        DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                                 
                          VALUE(I) = MAX(VALUE(I), FBUF%FLOC(IFAIL)%DAMMX(I))
                          IS_WRITTEN_VALUE(I) = 1
                        ENDDO                                                               
                      ENDDO                                                                 
                    ENDDO                                                                   
                  ENDDO     !  I=1,NEL                                                       
                ENDIF          
             ENDIF ! IFAILURE
C
C for outp of dam inside law25
C                             
             IF(MLW == 25 .AND. (IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .          IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 )) THEN   
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
               IF (IGTYP == 11) THEN
                 IPMAT = 100				   
                 DO N=1,NPT
                   IADR = (N-1)*NEL			  
                   DO I=1,NEL
                     J = IADR+I 			   
                     MATLY(J) = IGEO(IPMAT+N,PID(I))
                   END DO				 
                 END DO 			       
               ELSEIF (IGTYP == 10) THEN 
                 DO N=1,NPT
                   IADR = (N-1)*NEL
                   DO I=1,NEL
                     J = IADR+I 
                     MATLY(J)=MAT(I)
                   END DO
                 END DO 
               ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                 IPMAT = 2 + NLAY			  
                 DO N=1,NLAY
                   IADR = (N-1)*NEL			  
                   DO I=1,NEL
                     J = IADR+I 			   
                     MATLY(J) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                   END DO				 
                 END DO        
               END IF
C
               IF(MPT >= IPT) THEN		      
!!                    DO IL=1,NLAY  ! we are in the case IL=IPT 		 
                  DO I=1,NEL						
                    K1 = 2*I-1  					    
                    K2 = 2*I 
                     NPTT = ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
                     BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                     IADR = (IPT - 1)*NEL			    
                     J = IADR + I 
                     VLY(1:5) = ZERO
                     DO IT=1,NPTT
                       VG(1:5)= ZERO
                       DO IR=1,NPTR
                	 DO IS=1,NPTS
                	   LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,IT)	  
                	   DMAX(I) = ONE/PM(64,MATLY(J)) 			      
                	   WPMAX(I)= ONE/PM(41,MATLY(J)) 			      
                	   EPST1(I)= PM(60,MATLY(J))				   
                	   EPST2(I)= PM(61,MATLY(J))				   
                	   EPSF1(I)= ONE/PM(98,MATLY(J)) 			      
                	   EPSF2(I)= ONE/PM(99,MATLY(J))
C   
                	   VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                	   VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                	   VG(3)= MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                	   IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .          		   (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                	   IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .          		   (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                	ENDDO
                       ENDDO
                	VLY(1) =VLY(1) + VG(1) 
                	VLY(2) =VLY(2) + VG(2) 
                	VLY(3) =VLY(3) + VG(3) 
                	VLY(4) =VLY(4) + VG(4) 
                	VLY(5) =VLY(5) + VG(5) 
                     ENDDO ! NPTT
                      VLY(1) =VLY(1)/NPTT
                      VLY(2) =VLY(2)/NPTT
                      VLY(3) =VLY(3)/NPTT
                      VLY(4) =VLY(4)/NPTT
                      VLY(5) =VLY(5)/NPTT
C                      
                      VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),VLY(4),VLY(5))
                      IS_WRITTEN_VALUE(I) = 1
                 ENDDO  ! I=1,JLT
               ENDIF
              ENDIF ! law 25  + SHell Composite  PID
C--------------------------------------------------
            ELSEIF (KEYWORD == 'DAMG/MEMB') THEN 
C--------------------------------------------------
              IF (GBUF%G_DMG > 0) THEN
c
                ! Number of integration points in the thickness
                NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                NPGT = NPTR*NPTS
c
                ! Resetting values
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
c
                ! Odd number of thickness integration points
                IF ((MOD(NPTT,2)/=0).AND.(NPTT>1)) THEN
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,CEILING(NPTT/TWO))     
                        VALUE(I) = VALUE(I) + LBUF%DMG(I)/NPGT                                                           
                      ENDDO                                                               
                    ENDDO                                                                 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO 
c
                ! Even number of thickness integration points
                ELSEIF ((MOD(NPTT,2)==0).AND.(NPTT>1)) THEN
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,NPTT/2)                       
                        VALUE(I) = VALUE(I) + LBUF%DMG(I)/(TWO*NPGT)                                                          
                      ENDDO                                                               
                    ENDDO
                  ENDDO 
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,(NPTT/2)+1)                       
                        VALUE(I) = VALUE(I) + LBUF%DMG(I)/(TWO*NPGT)                                                          
                      ENDDO                                                               
                    ENDDO                                                                 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO  
                ! Number of thickness integration point = 1
                ELSE
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)                       
                        VALUE(I) = VALUE(I) + LBUF%DMG(I)/NPGT                                                           
                      ENDDO                                                               
                    ENDDO                                                                 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'DAMG') THEN 
C--------------------------------------------------
              IF (GBUF%G_DMG > 0) THEN
c
                ! Resetting values
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
c
                ! Mean value through all integration points in the thickness
                IF (IPT == -1) THEN  
                  NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  NPGT = NPTR*NPTS*NPTT
                  DO I=1,NEL                                                              
                    DO IT = 1,NPTT
                      DO IR = 1,NPTR                                                       
                        DO IS = 1,NPTS                                                     
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)                       
                          VALUE(I) = VALUE(I) + LBUF%DMG(I)/NPGT                                                           
                        ENDDO                                                               
                      ENDDO                                                                 
                    ENDDO
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
c
                ! Value at a given integration point in the thickness
                ELSEIF ( IPT <= NPT .AND. IPT > 0) THEN
                  NPGT = NPTR*NPTS
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)                       
                        VALUE(I) = VALUE(I) + LBUF%DMG(I)/NPGT                                                           
                      ENDDO                                                               
                    ENDDO                                                                 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO              
c
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'DAMINI') THEN 
C--------------------------------------------------
              IF (IFAILURE > 0) THEN
c
                ! Resetting values
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
c
c IPLY=NULL ILAYER=NULL NPT=NULL
                IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1) THEN  
                  IF (NLAY > 1) THEN    
                    DO I=1,NEL   
                      DO N = 1,NLAY
                        NPTT = ELBUF_TAB(NG)%BUFLY(N)%NPTT
                        DO IT = 1,NPTT
                          DO IR = 1,NPTR     
                            DO IS = 1,NPTS     
                              FBUF => ELBUF_TAB(NG)%BUFLY(N)%FAIL(IR,IS,IT)
                              MAXDAMINI = ZERO
                              DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(N)%NFAIL
                                IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN
                                  MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                                ENDIF
                              ENDDO
                              VALUE(I) = VALUE(I) + MAXDAMINI/(NPTT*NPTR*NPTS)
                           ENDDO
                         ENDDO  
                       ENDDO  !  DO IT = 1,NPTT
                      ENDDO   !  N=1,NLAY 
                      VALUE(I) = VALUE(I) / NLAY
                      IS_WRITTEN_VALUE(I) = 1  
                    ENDDO                                                                     
                  ELSEIF (MPT > 0) THEN  ! NLAY = 1   
                    NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                    DO I = 1,NEL
                      DO IT = 1,NPTT
                        DO IR = 1,NPTR 
                          DO IS = 1,NPTS    
                            FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IT)
                            MAXDAMINI = ZERO      
                            DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(1)%NFAIL
                              IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN
                                MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                              ENDIF
                            ENDDO
                            VALUE(I) = VALUE(I) + MAXDAMINI/(NPTT*NPTR*NPTS)
                          ENDDO
                        ENDDO
                      ENDDO
                      IS_WRITTEN_VALUE(I) = 1
                    ENDDO
                  ENDIF  
c PLY=IPLY NPT=IPT   
                ELSEIF ( IPLY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN
                  DO J=1,NLAY
                    NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                      ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF 
                    IF (ID_PLY == IPLY) THEN
                      IF (IPT <= NPTT) THEN
                        DO I = 1,NEL  
                          DO IR = 1,NPTR  
                            DO IS = 1,NPTS 
                              FBUF => ELBUF_TAB(NG)%BUFLY(J)%FAIL(IR,IS,IPT)
                              MAXDAMINI = ZERO
                              DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(J)%NFAIL 
                                IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN
                                  MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                                ENDIF
                              ENDDO
                              VALUE(I) = VALUE(I) + MAXDAMINI/(NPTR*NPTS)
                            ENDDO
                          ENDDO
                          IS_WRITTEN_VALUE(I) = 1
                        ENDDO
                      ENDIF
                    ENDIF
                  ENDDO
c PLY=IPLY NPT=-1
                ELSEIF ( IPLY > 0 .AND. IPT == -1 ) THEN
                  DO J = 1,NLAY
                    NPTT = ELBUF_TAB(NG)%BUFLY(J)%NPTT
                    ID_PLY = 0
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                      ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF 
                    IF (ID_PLY == IPLY) THEN
                      DO I = 1,NEL
                        DO IR = 1,NPTR
                          DO IS = 1,NPTS
                            DO IT = 1,NPTT
                              FBUF => ELBUF_TAB(NG)%BUFLY(J)%FAIL(IR,IS,IT)
                              MAXDAMINI = ZERO
                              DO IFAIL = 1, ELBUF_TAB(NG)%BUFLY(J)%NFAIL
                                IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN
                                  MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                                ENDIF
                              ENDDO
                              VALUE(I) = VALUE(I) + MAXDAMINI/(NPTR*NPTS*NPTT)
                            ENDDO
                          ENDDO
                        ENDDO 
                        IS_WRITTEN_VALUE(I) = 1
                      ENDDO 
                    ENDIF
                  ENDDO
c ILAYER=ILAY NPT=IPT 
                ELSEIF (ILAY <= NLAY .AND. ILAY > 0 .AND. IPT <= MPT .AND. IPT > 0 ) THEN  
                  IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                    DO I=1,NEL  
                      DO IR = 1,NPTR
                        DO IS = 1,NPTS
                          FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IPT)
                          MAXDAMINI = ZERO     
                          DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL
                            IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN
                              MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                            ENDIF
                          ENDDO
                          VALUE(I) = VALUE(I) + MAXDAMINI/(NPTR*NPTS)
                          IS_WRITTEN_VALUE(I) = 1
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
c ILAYER=IL NPT=NULL
                ELSEIF ( ILAY <= NLAY .AND. ILAY > 0) THEN
                  IPT = 1
                  IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                    DO I=1,NEL
                      DO IR = 1,NPTR
                        DO IS = 1,NPTS
                          FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,1)
                          MAXDAMINI = ZERO      
                          DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL
                            IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN                
                             MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                            ENDIF
                          ENDDO  
                          VALUE(I) = VALUE(I) + MAXDAMINI/(NPTR*NPTS)      
                        ENDDO
                      ENDDO  
                      IS_WRITTEN_VALUE(I) = 1          
                    ENDDO     !  I=1,NEL
                  ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN          
                    NPTT = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                    DO I=1,NEL          
                      DO IT = 1,NPTT
                        DO IR = 1,NPTR            
                         DO IS = 1,NPTS             
                           FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)  
                            MAXDAMINI = ZERO            
                            DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL    
                              IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN                     
                               MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                              ENDIF
                            ENDDO    
                            VALUE(I) = VALUE(I) + MAXDAMINI/(NPTT*NPTR*NPTS)
                          ENDDO
                        ENDDO            
                      ENDDO  !  DO IT = 1,NPTT
                      IS_WRITTEN_VALUE(I) = 1
                    ENDDO     !  I=1,NEL
                  ENDIF
c NPT=IPT
                ELSEIF ( IPT <= NPT .AND. IPT > 0) THEN
                  IF (IGTYP == 1 .OR. IGTYP == 9 ) THEN 
                    DO I=1,NEL       
                      DO IR = 1,NPTR          
                        DO IS = 1,NPTS          
                          FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)   
                          MAXDAMINI = ZERO                    
                          DO IFAIL = 1,ELBUF_TAB(NG)%BUFLY(1)%NFAIL   
                            IF (FBUF%FLOC(IFAIL)%LF_DAMINI > 0) THEN       
                             MAXDAMINI = MAX(MAXDAMINI,FBUF%FLOC(IFAIL)%DAMINI(I))
                            ENDIF
                          ENDDO
                          VALUE(I) = VALUE(I) + MAXDAMINI/(NPTR*NPTS)
                          IS_WRITTEN_VALUE(I) = 1           
                        ENDDO
                      ENDDO          
                    ENDDO     !  I=1,NEL
                  ENDIF
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSE IF (KEYWORD == 'TDEL') THEN   
C           FAIL TIME : ELEMENT DELETED
C--------------------------------------------------
c
              DO IL=1,NLAY                                                               
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                     
                DO IS=1,NPTS                                                             
                  DO IT=1,NPTT                                                           
                    DO IR=1,NPTR                                                        
                      FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                      
                      DO IFAIL=1,NFAIL                                                                                              
                        DO I=1,NEL                                                     
                          VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%TDEL(I))
                          IS_WRITTEN_VALUE(I) = 1                            
                        ENDDO                                                            
                      ENDDO                                                              
                    ENDDO                                                                
                  ENDDO                                                                  
                ENDDO                                                                   
              ENDDO    
C--------------------------------------------------
            ELSE IF (KEYWORD == 'SSP') THEN   
C           SOUND SPEED
            ! /ANIM/ELEM/SSP
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL                                         
                     VALUE(I) = MULTI_FVM%SOUND_SPEED(I + NFT)
                     IS_WRITTEN_VALUE(I) = 1		                 
                  ENDDO
               ELSE
                  L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
                  IF(L /= 0)THEN                                       
                     LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                     DO  I=1,NEL                                 			     
                        VALUE(I) = LBUF%SSP(I) 
                        IS_WRITTEN_VALUE(I) = 1                
                     ENDDO                                                 
                  ENDIF   
               ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'SCHLIEREN') THEN     
C--------------------------------------------------                                                   
               IALEL=IPARG(7,NG)+IPARG(11,NG)
               IF(IALEL /= 0)THEN
                 IF(ITY ==7 .AND. N2D /= 0)THEN 
                   EVAR(1:NEL)=ZERO  
                   LFT=1
                   LLT=NEL                                      
                   CALL SCHLIEREN(
     1                   EVAR  , IXTG , X         , V           , W       ,
     2                   IPARG , WA_L , ELBUF_TAB , ALE_CONNECT , GBUF%VOL,
     3                   PM    , NG   , NIXTG     , ITY) 
                   DO  I=1,NEL 
                     VALUE(I) = EVAR(I) 
	  	   IS_WRITTEN_VALUE(I) = 1
                   ENDDO
                 ENDIF  
               ENDIF                     
C--------------------------------------------------
            ELSE IF ( KEYWORD == 'ERROR/THICK') THEN
C--------------------------------------------------
              IF (ITY == 3) THEN
                DO  I=1,NEL                                       
                  VALUE(I) = ERR_THK_SH4(I)    
                  IS_WRITTEN_VALUE(I) = 1
                END DO
              ELSE
                DO  I=1,NEL                             
                  VALUE(I) = ERR_THK_SH3(I)  
                  IS_WRITTEN_VALUE(I) = 1
                END DO
              ENDIF
C--------------------------------------------------
            ELSE IF (KEYWORD == 'DOMAIN') THEN   
C           SPMD DOMAIN
C--------------------------------------------------                     
              DO  I=1,NEL                                         
                VALUE(I) = ISPMD    
                IS_WRITTEN_VALUE(I) = 1            
              ENDDO   
C--------------------------------------------------
            ELSEIF (KEYWORD == 'SIGEQ') THEN  ! equiv stress (mid layer : npt/2 + 1)
C  equivalent stress - other than VON MISES
C--------------------------------------------------
              IF (GBUF%G_SEQ > 0) THEN
C------------------
                ! Total number of integration points
                NPGT = 0
                DO IL=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPGT = NPGT + BUFLY%NPTT*NPTR*NPTS
                ENDDO
                ! Average equivalent stress on integration points
                VALUE(1:NEL) = ZERO
                DO IL=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  DO IT=1,BUFLY%NPTT
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)     
                        DO I=1,NEL   
                          VALUE(I) = VALUE(I) + LBUF%SEQ(I)/NPGT
                          IS_WRITTEN_VALUE(I) = 1  
                        ENDDO
                      ENDDO
                    ENDDO 
                  ENDDO 
                ENDDO 
C------------------
              ELSE  !  VON MISES
                DO I=1,NEL
                  S1 = GBUF%FOR(JJ(1)+I)
                  S2 = GBUF%FOR(JJ(2)+I)
                  S12= GBUF%FOR(JJ(3)+I)
                  VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                  VALUE(I) = SQRT(VONM2)
                  IS_WRITTEN_VALUE(I) = 1   
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'NL_EPSP') THEN 
              IF (GBUF%G_PLANL > 0) THEN
                ! Resetting values
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
                ! Mean value through all integration points in the thickness
                IF (IPT == -1) THEN  
                  NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  NPGT = NPTR*NPTS*NPTT
                  DO I=1,NEL                                                              
                    DO IT = 1,NPTT
                      DO IR = 1,NPTR                                                       
                        DO IS = 1,NPTS                                                     
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)                       
                          VALUE(I) = VALUE(I) + LBUF%PLANL(I)/NPGT                                                           
                        ENDDO                                                               
                      ENDDO                                                                 
                    ENDDO
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ! Value at a given integration point in the thickness
                ELSEIF ( IPT <= NPT .AND. IPT > 0) THEN
                  NPGT = NPTR*NPTS
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)                       
                        VALUE(I) = VALUE(I) + LBUF%PLANL(I)/NPGT                                                           
                      ENDDO                                                               
                    ENDDO                                                                 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO              
                ENDIF
              ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'NL_EPSD') THEN 
C--------------------------------------------------
              IF (GBUF%G_EPSDNL > 0) THEN
C------------------
                ! Resetting values
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
                ! Mean value through all integration points in the thickness
                IF (IPT == -1) THEN  
                  NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  NPGT = NPTR*NPTS*NPTT
                  DO I=1,NEL                                                              
                    DO IT = 1,NPTT
                      DO IR = 1,NPTR                                                       
                        DO IS = 1,NPTS                                                     
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)                       
                          VALUE(I) = VALUE(I) + LBUF%EPSDNL(I)/NPGT                                                           
                        ENDDO                                                               
                      ENDDO                                                                 
                    ENDDO
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ! Value at a given integration point in the thickness
                ELSEIF ( IPT <= NPT .AND. IPT > 0) THEN
                  NPGT = NPTR*NPTS
                  DO I=1,NEL                                                              
                    DO IR = 1,NPTR                                                       
                      DO IS = 1,NPTS                                                     
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IPT)                       
                        VALUE(I) = VALUE(I) + LBUF%EPSDNL(I)/NPGT                                                           
                      ENDDO                                                               
                    ENDDO                                                                 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO              
                ENDIF
              ENDIF      
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TSAIWU') THEN 
              ! ---
              IF ( ILAY == -1 .AND. IPT == -1 .AND. IPLY == -1 .AND. GBUF%G_TSAIWU > 0) THEN
                IF (NLAY > 1) THEN
                  IPT = IABS(NLAY)/2 + 1
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)
                  IF (BUFLY%L_TSAIWU > 0) THEN
                    NPTT = BUFLY%NPTT
                    DO IR = 1,NPTR
                      DO IS = 1,NPTS
                        DO IT = 1,NPTT
                          DO I=1,NEL 
                            VALUE(I) = VALUE(I) + BUFLY%LBUF(IR,IS,IT)%TSAIWU(I)/(NPTT*NPTR*NPTS)
                            IS_WRITTEN_VALUE(I) = 1	
                          ENDDO			
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ELSE
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                  IF (BUFLY%L_TSAIWU > 0) THEN
                    NPTT = BUFLY%NPTT
                    IPT  = IABS(NPTT)/2 + 1
                    DO IR = 1,NPTR
                      DO IS = 1,NPTS
                        DO I=1,NEL 
                          VALUE(I) = VALUE(I) + BUFLY%LBUF(IR,IS,IPT)%TSAIWU(I)/(NPTR*NPTS)
                          IS_WRITTEN_VALUE(I) = 1				
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c
              ! -- PLY=IPLY NPT=IPT
              ELSEIF ( IPLY > 0 .AND. (IPT <= MPT .AND. IPT > 0 ) .AND. GBUF%G_TSAIWU > 0) THEN
c
	              DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	                  ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      NPTT = BUFLY%NPTT 
                      IF( IPT <= NPTT) THEN
                	      IF( NPG > 1 ) THEN
                	        DO IR=1,NPTR
                	          DO IS=1,NPTS
                	            DO I=1,NEL 			 
                	              VALUE(I) = VALUE(I) + BUFLY%LBUF(IR,IS,IPT)%TSAIWU(I)/NPG  
                                IS_WRITTEN_VALUE(I) = 1	
                	            ENDDO  
                	          ENDDO  
                	        ENDDO
                	      ELSE
                	        DO I=1,NEL				   
                	          VALUE(I) = BUFLY%LBUF(1,1,IPT)%TSAIWU(I)
                            IS_WRITTEN_VALUE(I) = 1				
                	        ENDDO
                	      ENDIF
                      ENDIF
                    ENDIF 
                  ENDIF 
                ENDDO
c
              ! -- PLY=IPLY NPT=NULL
              ELSEIF ( IPLY > 0  .AND. IPT == -1 .AND. GBUF%G_TSAIWU > 0) THEN
c
	              DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	                  ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                      NPTT = BUFLY%NPTT 
	                    DO IPT=1,NPTT
                        IF (IPT <= NPTT) THEN
                     	    IF (NPG > 1) THEN
                     	      DO IR=1,NPTR
                     	        DO IS=1,NPTS
                     	    	    DO I=1,NEL			   
                     	            VALUE(I) = VALUE(I) + BUFLY%LBUF(IR,IS,IPT)%TSAIWU(I)/(NPG*NPTT)
                            	    IS_WRITTEN_VALUE(I) = 1 
                                ENDDO			      
                     	        ENDDO  
                     	      ENDDO
                     	    ELSE
                     	      DO  I=1,NEL 			     
                     	        VALUE(I) = VALUE(I) + BUFLY%LBUF(1,1,IPT)%TSAIWU(I)/NPTT
                              IS_WRITTEN_VALUE(I) = 1				
                     	      ENDDO
                     	    ENDIF
                        ENDIF
                      ENDDO
                    ENDIF 
                  ENDIF 
                ENDDO
c
              ! -- ILAYER= NPT=
              ELSEIF ( (ILAY <= NLAY .AND. ILAY > 0) .AND. (IPT <= 10 .AND. IPT > 0 ) .AND. GBUF%G_TSAIWU > 0) THEN
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  IF ((BUFLY%L_TSAIWU > 0).AND.(IPT <= NPTT)) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        LBUF => BUFLY%LBUF(IR,IS,IPT)
                    	  DO I=1,NEL
                          VALUE(I) = VALUE(I) + LBUF%TSAIWU(I)/NPG
                          IS_WRITTEN_VALUE(I) = 1
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c
              ! -- ILAYER=IL NPT=NULL
              ELSEIF ( ILAY <= NLAY .AND. ILAY > 0 .AND. GBUF%G_TSAIWU > 0) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  IF (BUFLY%L_TSAIWU > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	  LBUF => BUFLY%LBUF(IR,IS,1)
                        DO I=1,NEL
                    	    VALUE(I) = VALUE(I) + LBUF%TSAIWU(I)/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	  ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  IF (BUFLY%L_TSAIWU > 0) THEN
                    DO IT=1,NPTT
                      DO IR=1,NPTR
                        DO IS=1,NPTS
                          LBUF => BUFLY%LBUF(IR,IS,IT)
                    	    DO I=1,NEL
                            VALUE(I) = VALUE(I) + LBUF%TSAIWU(I)/(NPG*NPTT)
                    	      IS_WRITTEN_VALUE(I) = 1
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
c
              ! -- ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0 .AND. GBUF%G_TSAIWU > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                  IF (BUFLY%L_TSAIWU > 0) THEN
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                    	  LBUF => BUFLY%LBUF(IR,IS,IPT)
                    	  DO I=1,NEL
                    	    VALUE(I) = VALUE(I) + LBUF%TSAIWU(I)/NPG
                          IS_WRITTEN_VALUE(I) = 1
                    	  ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF   
C--------------------------------------------------
            ELSEIF (KEYWORD == 'TEMP') THEN 
              IF ((GBUF%G_TEMP > 0).AND.(MLW == 104)) THEN
                NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                NPGT = NPTR*NPTS*NPTT
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
                DO I=1,NEL
                  DO IT=1,NPTT   
                    DO IS=1,NPTS                                            
                      DO IR=1,NPTR    
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                        VALUE(I) = VALUE(I) + LBUF%TEMP(I)/NPGT
                      ENDDO
                    ENDDO
                  ENDDO
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
              ELSEIF ((GBUF%G_TEMP > 0).AND.(MLW == 110)) THEN
                NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                NPGT = NPTR*NPTS*NPTT
                DO I = 1,NEL
                  VALUE(I) = ZERO
                ENDDO
                DO I=1,NEL
                  DO IT=1,NPTT   
                    DO IS=1,NPTS                                            
                      DO IR=1,NPTR    
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                        VALUE(I) = VALUE(I) + LBUF%TEMP(I)/NPGT
                      ENDDO
                    ENDDO
                  ENDDO
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'BULK')THEN  
            !/ANIM/ELEM/BULK (QVIS)
C--------------------------------------------------                                      
              IF (GBUF%G_QVIS > 0) THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%QVIS(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO          
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'DT' )THEN
C--------------------------------------------------
              IF(GBUF%G_DT>0)THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%DT(I) 			 
                  IS_WRITTEN_VALUE(I) = 1				
                ENDDO
              ENDIF
C--------------------------------------------------
            ELSEIF(KEYWORD == 'AMS' )THEN
C--------------------------------------------------
              IF(GBUF%G_ISMS>0)THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%ISMS(I) 			 
                  IS_WRITTEN_VALUE(I) = 1			
                ENDDO 
              ENDIF   
C--------------------------------------------------
            ELSEIF(KEYWORD == 'TDET' )THEN
C--------------------------------------------------
               IF (MLW  /= 51 .AND. GBUF%G_TB > 0) THEN                                                     
                 DO I=1,NEL                                                                                 
                   VALUE(I) = -GBUF%TB(I)                                                                   
                   IS_WRITTEN_VALUE(I) = 1                                                                  
                 ENDDO                                                                                      
               ELSEIF (MLW == 51)THEN                                                                       
                 IPOS      = 15                                                                             
                 ITRIMAT   = 4   
                 MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)                                                                          
                 K         = IPARG(2,NG) * ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)                               
                 DO I=1,IPARG(2,NG)                                                                         
                   VALUE(I) = -MBUF%VAR(K+I)                                                                
                   IS_WRITTEN_VALUE(I) = 1                                                                  
                 ENDDO                                                                                      
               ENDIF                                                                                        
C--------------------------------------------------
            ELSEIF(KEYWORD == 'BFRAC' )THEN
C--------------------------------------------------
              IF(GBUF%G_BFRAC>0)THEN
                DO I=1,NEL
                  VALUE(I) = GBUF%BFRAC(I) 			 
                  IS_WRITTEN_VALUE(I) = 1			
                ENDDO 
              ENDIF  
c---------------------------------------------------------------------------
            ELSEIF (KEYWORD == 'ALPHA') THEN   ! shear angle - law58
c---------------------------------------------------------------------------
              IF ( IPLY == -1 .and. ILAY == -1) THEN  ! output mid-layer
                IF (NLAY > 1) THEN							     
                  IL  = IABS(NLAY)/2 + 1						     
                  IPT = 1								     
                ELSE  								     
                  IL  = 1								     
                  IPT =  IABS(NPT)/2 + 1						     
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
c
                IF (BUFLY%L_ANG > 0)THEN
                  IF (NPG > 1) THEN
                    LBUF1 => BUFLY%LBUF(1,1,IPT)
                    LBUF2 => BUFLY%LBUF(2,1,IPT)
                    LBUF3 => BUFLY%LBUF(1,2,IPT)
                    LBUF4 => BUFLY%LBUF(2,2,IPT)
                    DO I=1,NEL 
                      A1 =  ABS( ATAND(LBUF1%ANG(I) ))
                      A2 =  ABS( ATAND(LBUF2%ANG(I) ))
                      A3 =  ABS( ATAND(LBUF3%ANG(I) ))
                      A4 =  ABS( ATAND(LBUF4%ANG(I) ))
                      VALUE(I) = FOURTH*(A1 + A2 + A3 + A4)
                      IS_WRITTEN_VALUE(I) = 1	
                    ENDDO 
                  ELSE
                    DO I=1,NEL 
                      VALUE(I) = ABS( ATAND(BUFLY%LBUF(1,1,IPT)%ANG(I) ))
                      IS_WRITTEN_VALUE(I) = 1	
                    ENDDO 
                  ENDIF
                ENDIF
c
              ELSEIF (IPLY > 0) THEN  ! PLY=IPLY     
	               DO J=1,NLAY
                  ID_PLY = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
	                   ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IF (ID_PLY  == IPLY ) THEN
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    IF (BUFLY%L_ANG > 0)THEN
                      IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
                        NPTT = BUFLY%NPTT
                        IPT  = IABS(NPTT)/2 + 1	  ! middle intergation point					     
                	       IF ( NPG > 1 ) THEN
                          LBUF1 => BUFLY%LBUF(1,1,IPT)
                          LBUF2 => BUFLY%LBUF(2,1,IPT)
                          LBUF3 => BUFLY%LBUF(1,2,IPT)
                          LBUF4 => BUFLY%LBUF(2,2,IPT)
                          DO I=1,NEL 
                            A1 =  ABS( ATAND(LBUF1%ANG(I) ))
                            A2 =  ABS( ATAND(LBUF2%ANG(I) ))
                            A3 =  ABS( ATAND(LBUF3%ANG(I) ))
                            A4 =  ABS( ATAND(LBUF4%ANG(I) ))
                            VALUE(I) = FOURTH*(A1 + A2 + A3 + A4)
                            IS_WRITTEN_VALUE(I) = 1	
                          ENDDO 
                	       ELSE
                	         DO  I=1,NEL				   
                            VALUE(I) = ABS( ATAND(BUFLY%LBUF(1,1,IPT)%ANG(I) ))
                            IS_WRITTEN_VALUE(I) = 1				
                	         ENDDO
                	       ENDIF
                      ENDIF
                    ENDIF 
                  ENDIF 
                ENDDO
c
              ELSEIF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
                IF (BUFLY%L_ANG > 0) THEN
                  NPTT = BUFLY%NPTT
                  IPT  = IABS(NPTT)/2 + 1	  ! middle intergation point					     
                	 IF ( NPG > 1 ) THEN
                    LBUF1 => BUFLY%LBUF(1,1,IPT)
                    LBUF2 => BUFLY%LBUF(2,1,IPT)
                    LBUF3 => BUFLY%LBUF(1,2,IPT)
                    LBUF4 => BUFLY%LBUF(2,2,IPT)
                    DO I=1,NEL 
                      A1 =  ABS( ATAND(LBUF1%ANG(I) ))
                      A2 =  ABS( ATAND(LBUF2%ANG(I) ))
                      A3 =  ABS( ATAND(LBUF3%ANG(I) ))
                      A4 =  ABS( ATAND(LBUF4%ANG(I) ))
                      VALUE(I) = FOURTH*(A1 + A2 + A3 + A4)
                      IS_WRITTEN_VALUE(I) = 1	
                    ENDDO 
                	 ELSE
                	   DO  I=1,NEL				   
                      VALUE(I) = ABS( ATAND(BUFLY%LBUF(1,1,IPT)%ANG(I) ))
                      IS_WRITTEN_VALUE(I) = 1				
                	   ENDDO
                	 ENDIF
                ENDIF

              ENDIF
c---------------------------------------------------------------------------
            ELSEIF (KEYWORD == 'FLDF/MEMB') THEN 
c---------------------------------------------------------------------------
              IL = NLAY/2 + 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NPTT  = BUFLY%NPTT
              NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
              NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              IPT   = NPTT/2 + 1
              DO IS=1,NPTS                                                                  
                DO IR=1,NPTR                                                                
                  FBUF => BUFLY%FAIL(IR,IS,IPT)                            
                  DO IFAIL=1,NFAIL                                                          
                    IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                      DO I=1,NEL                                                      
                        VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I)) 
                        IS_WRITTEN_VALUE(I) = 1	                         
                      ENDDO                                                                 
                    ENDIF                                                                   
                  ENDDO
                ENDDO                                                                       
              ENDDO  
c---------------------------------------------------------------------------
            ELSEIF (KEYWORD == 'FLDF') THEN 
c---------------------------------------------------------------------------
c PLY=NULL ILAYER=IL NPT=NULL
              IF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
                  NPTT  = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      DO IT=1,NPTT
                        IPT = IT
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                        DO IFAIL=1,NFAIL                                                          
                          IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                            DO I=1,NEL
                              VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))
                              IS_WRITTEN_VALUE(I) = 1	                         
                            ENDDO                                                                 
                          ENDIF                                                                   
                        ENDDO
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO  
                ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                                                   
                  NPTT  = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR 
                      FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                          DO I=1,NEL 
                            VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I))
                            IS_WRITTEN_VALUE(I) = 1	                       
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                    
                    ENDDO                                                                       
                  ENDDO         

                ENDIF
              ENDIF
c---------------------------------------------------------------------------
            ELSEIF (KEYWORD == 'FLDZ/MEMB') THEN 
c---------------------------------------------------------------------------
                IL = NLAY/2 + 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT  = BUFLY%NPTT
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL                                                                   
                NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                IPT   = NPTT/2 + 1
                DO IS=1,NPTS                                                                  
                  DO IR=1,NPTR                                                                
                    FBUF => BUFLY%FAIL(IR,IS,IPT)                            
                    DO IFAIL=1,NFAIL                                                          
                      IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                        DO I=1,NEL                                                           
                          RINDX = FBUF%FLOC(IFAIL)%INDX(I)
                          VALUE(I) = MAX(VALUE(I),RINDX) 
                          IS_WRITTEN_VALUE(I) = 1				                         
                        ENDDO                                                                 
                      ENDIF                                                                   
                    ENDDO
                  ENDDO                                                                       
                ENDDO  
c---------------------------------------------------------------------------
            ELSEIF (KEYWORD == 'FLDZ') THEN 
c---------------------------------------------------------------------------
c PLY=NULL ILAYER=IL NPT=NULL
              IF (IPLY == -1 .AND.  ILAY <= NLAY .AND. ILAY > 0 .AND. IPT == -1 ) THEN
                IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17) THEN 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
                  NPTT  = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR                                                                
                      DO IT=1,NPTT
                        IPT = IT
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IPT)                            
                        DO IFAIL=1,NFAIL                                                          
                          IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                            DO I=1,NEL                                                       
                              RINDX = FBUF%FLOC(IFAIL)%INDX(I)
                              VALUE(I) = MAX(VALUE(I),RINDX) 
                              IS_WRITTEN_VALUE(I) = 1	                      
                            ENDDO                                                                 
                          ENDIF                                                                   
                        ENDDO
                      ENDDO                                                                     
                    ENDDO                                                                       
                  ENDDO  
                ENDIF
c PLY=NULL ILAYER=NULL NPT=IPT
              ELSEIF ( IPT <= MPT .AND. IPT > 0) THEN
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN 
                  NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL                                                                   
                  NPTT  = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  DO IS=1,NPTS                                                                  
                    DO IR=1,NPTR 
                      FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(IR,IS,IPT)                            
                      DO IFAIL=1,NFAIL                                                          
                        IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                          DO I=1,NEL                                                    
                            RINDX = FBUF%FLOC(IFAIL)%INDX(I)
                            VALUE(I) = MAX(VALUE(I),RINDX) 
                            IS_WRITTEN_VALUE(I) = 1                          
                          ENDDO                                                                 
                        ENDIF                                                                   
                      ENDDO                                                                    
                    ENDDO                                                                       
                  ENDDO         

                ENDIF
              ENDIF                                                         


c---------------------------------------------------------------------------
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
C---------------------------------------------------------------------------
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
           ELSEIF(KEYWORD == 'MACH')THEN  
C--------------------------------------------------                     
!to be moved in h3d_tria_scalar when implemented.
            IF(N2D/=0)THEN
                IF (MLW == 151) THEN
                   DO I = 1, NEL                                         
                      VEL(1) = MULTI_FVM%VEL(1, I + NFT)
                      VEL(2) = MULTI_FVM%VEL(2, I + NFT)
                      VEL(3) = MULTI_FVM%VEL(3, I + NFT)
                      VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))
                      VALUE(I) = VEL(0)/MULTI_FVM%SOUND_SPEED(I + NFT)
                      IS_WRITTEN_VALUE(I) = 1                             
                   ENDDO
                ELSEIF(I_ALE_SOLVER>1)THEN
                     L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                        
                     IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN              
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)            
                        DO  I=1,NEL     
                           VEL(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                           VEL(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                           VEL(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                           VEL(0) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))                                    
                           VALUE(I) = VEL(0)/LBUF%SSP(I) 
                           IS_WRITTEN_VALUE(I) = 1                          
                        ENDDO    
                     ENDIF                                               
                ELSE                                                                                 
                  L = ELBUF_TAB(NG)%BUFLY(1)%L_SSP                                                     
                  IF(ELBUF_TAB(NG)%BUFLY(1)%L_SSP /= 0)THEN                                               
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)  
                    IF(IS_ALE /= 0)THEN    
                      !ale                                        
                      DO  I=1,NEL                                                                           
                        TMP(1,1:3)=V(1,IXTG(2:4,I+NFT))-W(1,IXTG(2:4,I+NFT))                                                     
                        TMP(2,1:3)=V(2,IXTG(2:4,I+NFT))-W(2,IXTG(2:4,I+NFT))                                                     
                        TMP(3,1:3)=V(3,IXTG(2:4,I+NFT))-W(3,IXTG(2:4,I+NFT))                                                     
                        VEL(1) = SUM(TMP(1,1:3))*THIRD                                                     
                        VEL(2) = SUM(TMP(2,1:3))*THIRD                                                     
                        VEL(3) = SUM(TMP(3,1:3))*THIRD                                                     
                        VALUE(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I)  
                        IS_WRITTEN_VALUE(I) = 1                                   
                      ENDDO 
                     ELSE
                      !euler and lagrange
                      DO  I=1,NEL                                                                           
                        TMP(1,1:3)=V(1,IXTG(2:4,I+NFT))                                                    
                        TMP(2,1:3)=V(2,IXTG(2:4,I+NFT))                                                    
                        TMP(3,1:3)=V(3,IXTG(2:4,I+NFT))                                                    
                        VEL(1) = SUM(TMP(1,1:3))*THIRD                                                     
                        VEL(2) = SUM(TMP(2,1:3))*THIRD                                                     
                        VEL(3) = SUM(TMP(3,1:3))*THIRD                                                     
                        VALUE(I) = SQRT(VEL(1)*VEL(1)+VEL(2)*VEL(2)+VEL(3)*VEL(3))/LBUF%SSP(I) 
                        IS_WRITTEN_VALUE(I) = 1                                    
                      ENDDO 
                    ENDIF                                                                                  
                  ENDIF                                                                                     
                ENDIF                                   
            ENDIF!N2D
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'COLOR') THEN  
C--------------------------------------------------                 
!to be moved in h3d_tria_scalar when implemented.
              GBUF => ELBUF_TAB(NG)%GBUF 
              IF (MLW == 151) THEN 
                 NFRAC=MULTI_FVM%NBMAT
                 DO IMAT=1,NFRAC
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
                IMAT = IXTG(1,NFT+1)                        
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
                NFRAC=0
                !VFRAC(1:NEL,1:21)=ZERO     
              ENDIF 
              IF(NFRAC>0)THEN
                DO I=1,NEL
                  VALUE(I)=ZERO
                  DO IMAT=1,NFRAC
                    VALUE(I) = VALUE(I) + VFRAC(I,IMAT)*IMAT
                  ENDDO
                  IS_WRITTEN_VALUE(I) = 1  
                ENDDO            
              ENDIF
C-------------------------------------------------- 
              ELSEIF(KEYWORD == 'VORTX') THEN  
C--------------------------------------------------                 
              IF (MLW == 6 .OR. MLW == 17) THEN
                DO I=1,NEL
                  VALUE(I) = LBUF%VK(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                DO I=1,NEL
                  VALUE(I) = UVAR(NEL+I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ELSEIF(MLW == 151)THEN
                !ITY = IPARG(5, NG)
                NB_FACE=3
                DO I=1,NEL
                  II = I + NFT
                  IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
                  CUMUL(1:3)=ZERO
                  DO KFACE = 1, NB_FACE
                    IV = ALE_CONNECT%ee_connect%connected(IAD2 + KFACE - 1)
                    NX = ZERO !MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, II)                    
                    NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, II)                          
                    NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, II)                          
                    SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, II)                             
                    VX = ZERO !MULTI_FVM%VEL(1, II)                                        
                    VY = MULTI_FVM%VEL(2, II)                                              
                    VZ = MULTI_FVM%VEL(3, II)                                              
                    IF(IV /=0)THEN                                                         
                      VX = ZERO ! HALF(VX + MULTI_FVM%VEL(1, IV))                        
                      VY = HALF*(VY + MULTI_FVM%VEL(2, IV))                              
                      VZ = HALF*(VZ + MULTI_FVM%VEL(3, IV))                              
                    ENDIF                                                                  
                    CUMUL(1)=CUMUL(1)+SURF*(NY*VZ-NZ*VY)                                   
                    !CUMUL(2)=CUMUL(2)+NZ*VX-NX*VZ                                         
                    !CUMUL(3)=CUMUL(3)+NX*VY-NY*VX                                         
                  ENDDO
                  CUMUL(1)=CUMUL(1)/GBUF%VOL(I)                   
                  VALUE(I) = CUMUL(1)
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
              ELSEIF(KEYWORD == 'VONM/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_YIELD(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'SIGEQ/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_SEQ(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'ENER/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_EINT(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------  
              ELSEIF(KEYWORD == 'DAMA/TMAX') THEN  
C--------------------------------------------------                 
                  DO I=1,NEL         
                    VALUE(I) = GBUF%TM_DMG(I) 
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO   
C--------------------------------------------------                                   
           ENDIF  ! KEYWORD
C-------------------------------------------------- 
           CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_SHELL,SHELL_SCALAR,NEL,OFFSET,NFT,
     .   			       VALUE,IS_WRITTEN_VALUE)
         ENDIF  ! ITY 
c
C-----------------------------------------------
       ENDIF     ! MLW /= 13  
C-----------------------------------------------
      RETURN
      END
