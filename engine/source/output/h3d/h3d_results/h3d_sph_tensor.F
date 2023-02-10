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
Chd|  H3D_SPH_TENSOR                source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_TENSOR              source/output/h3d/h3d_results/h3d_write_tensor.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SPH_TENSOR(
     .                  ELBUF_TAB,SPH_TENSOR, IPARG   ,ITENS   ,KXSP  ,PM       ,
     2                   EL2FA    ,NBF     ,TENS    ,EPSDOT  ,
     3                   NBPART   ,X       ,IADG    ,IPART   ,
     4                   IPARTSP  ,ISPH3D  ,IPM     ,IGEO    ,ID_ELEM    ,
     5                   IS_WRITTEN_SPH,  H3D_PART,KEYWORD)
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "spmd_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SPH_TENSOR(6,*), TENS(6,*),EPSDOT(6,*),PM(NPROPM,*),X(3,*)
      INTEGER IPARG(NPARG,*),ITENS, 
     .   KXSP(NISP,*),EL2FA(*),IADG(NSPMD,*),IPM(NPROPMI,*),
     .   NBF,NBPART,IPART(LIPART1,*),IPARTSP(*),
     .   ISPH3D,IGEO(NPROPGI,*),IS_WRITTEN_SPH(*),ID_ELEM(*),
     .   H3D_PART(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      my_real
     .   EVAR(6,MVSIZ)
      my_real
     .   OFF, P,VONM2,S1,S2,S3,VALUE,DMGMX,FAC,
     .   DIR1_1,DIR1_2,DIR2_1,DIR2_2,AA,BB,V1,V2,V3,X21,X32,X34,
     .   X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,SUMA,VR,VS,X31,Y31,
     .   Z31,E11,E12,E13,E21,E22,E23,SUM,AREA,X2L,VAR,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,RX,RY,RZ,SX,SY,SZ,
     .   VG(5),VLY(5),VE(5),S4,S5,S6,VONM, GAMA(6),EVAR_TMP(6),
     .   A1
      my_real
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   L11,L22,L33,L12,L21,L23,L32,L13,L31,
     .   S11,S22,S33,S12,S21,S23,S32,S13,S31
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,
     .        ID_PLY,NB_PLYOFF,NUVARR
      INTEGER PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),FAILG(100,MVSIZ),
     .        PTE(4),PTP(4),PTMAT(4),PTVAR(4),NPT_ALL,IPLY,
     .        ID_ELEM_TMP(MVSIZ),NIX,ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,
     .        ISTRAIN,KCVT,IOR_TSH,MT1,ICSIG,PTI,IOK,IPRT,IOK_PART(MVSIZ),
     .        JJ(6),IS_WRITTEN_TENSOR(MVSIZ)

      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR
      TYPE(L_BUFEL_) ,POINTER  :: LBUF1,LBUF2,LBUF3,LBUF4
C-----------------------------------------------  
      NN1 = 1
      NN2 = 1
      NN3 = NN2 + NUMELS
      NN4 = NN3 + ISPH3D*(NUMSPH+MAXPJET)
C-----------------------------------------------
       DO 490 NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        ISTRAIN = IPARG(44,NG)
        ISOLNOD = IPARG(28,NG)
        IVISC = IPARG(61,NG)
        CALL INITBUF (IPARG    ,NG      ,                      
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
       IF(MLW /= 13) THEN 
C-----------------------------------------------
C       SPH
C-----------------------------------------------
        IF (ITY == 51) THEN

          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          IPRT=IPARTSP(1 + NFT)
          MT1 =IPART(1,IPRT)

          DO  I=1,NEL 
            ID_ELEM(NFT+I) = KXSP(NISP,NFT+I)
            IF( H3D_PART(IPARTSP(NFT+I)) == 1) IOK_PART(I) = 1
            IS_WRITTEN_TENSOR(I) = 0
          ENDDO

          DO I=1,NEL
            EVAR(1,I) = ZERO
            EVAR(2,I) = ZERO
            EVAR(3,I) = ZERO
            EVAR(4,I) = ZERO
            EVAR(5,I) = ZERO
            EVAR(6,I) = ZERO
          ENDDO

C-----------------------------------------------
          IF (KEYWORD == 'TENS/STRESS') THEN
C-----------------------------------------------
C          STRESS
            DO I=1,NEL
              EVAR(1,I) = GBUF%SIG(JJ(1) + I)
              EVAR(2,I) = GBUF%SIG(JJ(2) + I)
              EVAR(3,I) = GBUF%SIG(JJ(3) + I)
              EVAR(4,I) = GBUF%SIG(JJ(4) + I)
              EVAR(5,I) = GBUF%SIG(JJ(5) + I)
              EVAR(6,I) = GBUF%SIG(JJ(6) + I)
              IS_WRITTEN_TENSOR(I) = 1
            ENDDO
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN') THEN
C-----------------------------------------------
C          STRAIN
            IPRT=IPARTSP(1 + NFT)
            MT1 =IPART(1,IPRT)
            ISTRAIN= IPARG(44,NG)    
            NUVAR  = IPM(8,MT1)
            NUVARR = IPM(221,MT1) 
            IF (MLW>=28.AND.MLW/=49)THEN
              DO I=1,NEL
                EVAR(1,I) =  LBUF%STRA(JJ(1) + I)
                EVAR(2,I) =  LBUF%STRA(JJ(2) + I)
                EVAR(3,I) =  LBUF%STRA(JJ(3) + I)
                EVAR(4,I) =  LBUF%STRA(JJ(4) + I)*HALF
                EVAR(5,I) =  LBUF%STRA(JJ(5) + I)*HALF
                EVAR(6,I) =  LBUF%STRA(JJ(6) + I)*HALF
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO   
            ELSEIF(MLW == 14)THEN
              DO I=1,NEL                                   
                EVAR(1,I) = LBUF%EPE(JJ(1) + I)
                EVAR(2,I) = LBUF%EPE(JJ(2) + I)
                EVAR(3,I) = LBUF%EPE(JJ(3) + I)
                EVAR(4,I) =  ZERO
                EVAR(5,I) =  ZERO
                EVAR(6,I) =  ZERO
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO
            ELSEIF(MLW == 24)THEN
              DO I=1,NEL                                  
                EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO
            ELSEIF(ISTRAIN == 1)THEN
              IF(MLW/=14.AND.MLW/=24.AND.MLW<28.OR.
     .            MLW == 49)THEN               
                DO I=1,NEL
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
C-----------------------------------------------
          ELSEIF (KEYWORD == 'TENS/DAMA') THEN
C-----------------------------------------------
C           CRACKS
            IF (MLW == 24. AND. NINT(PM(56,MT1)) == 1) THEN
              IF(ISORTH==0)THEN
             	 DO I=1,NEL
             	   EVAR(1,I) =  LBUF%DGLO(JJ(1) + I)
             	   EVAR(2,I) =  LBUF%DGLO(JJ(2) + I)
             	   EVAR(3,I) =  LBUF%DGLO(JJ(3) + I)
             	   EVAR(4,I) =  LBUF%DGLO(JJ(4) + I)
             	   EVAR(5,I) =  LBUF%DGLO(JJ(5) + I)
             	   EVAR(6,I) =  LBUF%DGLO(JJ(6) + I)
                   IS_WRITTEN_TENSOR(I) = 1
             	 ENDDO
              ELSE
             	DO I=1,NEL	
             	  L11 = LBUF%DGLO(JJ(1) + I)		 
             	  L21 = LBUF%DGLO(JJ(2) + I)		 
             	  L31 = LBUF%DGLO(JJ(3) + I)		 
             	  L12 = LBUF%DGLO(JJ(4) + I)	  
             	  L22 = LBUF%DGLO(JJ(5) + I)	  
             	  L32 = LBUF%DGLO(JJ(6) + I)
             	  L13 = L21*L32-L31*L22
             	  L23 = L31*L12-L11*L32
             	  L33 = L11*L22-L21*L12
             	  G11 = GBUF%GAMA(JJ(1) + I)
             	  G21 = GBUF%GAMA(JJ(2) + I)
             	  G31 = GBUF%GAMA(JJ(3) + I)
             	  G12 = GBUF%GAMA(JJ(4) + I)
             	  G22 = GBUF%GAMA(JJ(5) + I)
             	  G32 = GBUF%GAMA(JJ(6) + I)
             	  G13 = G21*G32-G31*G22
             	  G23 = G31*G12-G11*G32
             	  G33 = G11*G22-G21*G12
             	  S11 =L11*G11+L12*G12+L13*G13 
             	  S12 =L11*G21+L12*G22+L13*G23 
             	  S13 =L11*G31+L12*G32+L13*G33
             	  S21 =L12*G11+L22*G12+L23*G13
             	  S22 =L12*G21+L22*G22+L23*G23
             	  S23 =L12*G31+L22*G32+L23*G33 
             	  S31 =L13*G11+L23*G12+L33*G13
             	  S32 =L13*G21+L23*G22+L33*G23
             	  S33 =L13*G31+L23*G32+L33*G33
             	  EVAR(1,I) = G11*S11+G12*S21+G13*S31
             	  EVAR(2,I) = G21*S12+G22*S22+G23*S32
             	  EVAR(3,I) = G31*S13+G32*S23+G33*S33
             	  EVAR(4,I) = G11*S12+G12*S22+G13*S32
             	  EVAR(5,I) = G21*S13+G22*S23+G23*S33
             	  EVAR(6,I) = G11*S13+G12*S23+G13*S33
                  IS_WRITTEN_TENSOR(I) = 1
             	ENDDO							 
              END IF								 
            END IF	
          ENDIF

          CALL H3D_WRITE_TENSOR(IOK_PART,IS_WRITTEN_SPH,SPH_TENSOR,NEL,0,NFT,
     .                                    EVAR,IS_WRITTEN_TENSOR)
C
C-----------------------------------------------
        ENDIF
C           
        ENDIF ! mlw /= 13 
490    CONTINUE
C-----------
      RETURN
      END
