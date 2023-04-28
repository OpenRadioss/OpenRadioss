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
Chd|  H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_FLD_STRAIN                source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|        H3D_FLD_TSH                   source/output/h3d/h3d_results/h3d_fld_tsh.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SOL_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE H3D_SKIN_SCALAR(
     .                  ELBUF_TAB       ,SKIN_SCALAR  ,IFUNC     ,IPARG       ,GEO         ,
     .                  IXS             ,IXS10 ,IXS16 , IXS20   ,PM          ,
     .                  IPM             ,IGEO         ,X            ,V         ,W          ,
     .                  IPARTS          ,H3D_PART    ,
     .                  IS_WRITTEN_SKIN ,INFO1        ,KEYWORD   , H3D_DATA  ,
     6                  IAD_ELEM        ,FR_ELEM     , WEIGHT    ,TAG_SKINS6,
     7                  NPF   ,TF    ,BUFMAT,IBCL    ,ILOADP     ,LLOADP    ,FAC    ,
     8                  SENSOR_TAB,TAGNCONT    ,LOADP_HYD_INTER,XFRAME,FORC    ,
     9                  NODAL_IPART ,IMAPSKP ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE H3D_MOD        
      USE MULTI_FVM_MOD
      USE SENSOR_MOD
      USE H3D_INC_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SKIN_SCALAR(*),X(3,*),V(3,*),W(3,*),GEO(NPROPG,*),PM(NPROPM,*),
     .   TF(*),BUFMAT(*)
      INTEGER , DIMENSION(NUMSKINP0), INTENT(IN) ::  IMAPSKP
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IFUNC,IXS10(*),IXS16(*), IXS20(*),
     .   IPM(NPROPMI,*),IGEO(NPROPGI,*),IPARTS(*),
     .   H3D_PART(*),IS_WRITTEN_SKIN(*),INFO1,
     .   IAD_ELEM(*),FR_ELEM(*), WEIGHT(*),TAG_SKINS6(*),NPF(*)
      INTEGER LLOADP(*)
      INTEGER ILOADP(SIZLOADP,*),IBCL(NIBCLD,*),NODAL_IPART(*)
      INTEGER TAGNCONT(NLOADP_HYD_INTER,NUMNOD),LOADP_HYD_INTER(NLOADP_HYD)
      my_real
     .   FAC(LFACCLD,*),XFRAME(NXFRAME,*),FORC(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   VALUE(MVSIZ),RINDX,STRAIN(3,MVSIZ),F_EXP,F_GAUSS(9)
      INTEGER I,I1,II,J,NG,NEL,NPTR,NPTS,NPTT,NLAY,L,IFAIL,ILAY,
     .        IR,IS,IT,IL,MLW, NUVAR,IUS,LENF,PTF,PTM,PTS,NFAIL,
     .        N,NN,K,K1,K2,JTURB,MT,IMID,IALEL,IPID,ISH3N,NNI,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN9,NF,BUF,NVARF,
     .        OFFSET,IHBE,NPTM,NPG, MPT,IPT,IADD,IADR,IPMAT,IFAILT,
     .        IIGEO,IADI,ISUBSTACK,ITHK,NB_PLYOFF,IUVAR,IDX,IPOS,ITRIMAT,
     .        IALEFVM_FLG, IMAT,IADBUF,NUPARAM,IOK_PART(MVSIZ),
     .        MLWI,PID,MID,IP,MX,KCVT,IOR_TSH,ICSTR
      INTEGER 
     .        IS_WRITTEN_VALUE(MVSIZ),NFRAC,IU(4),IV,NB_FACE,KFACE,NSKIN
      INTEGER NGL(MVSIZ) 
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_MAT_)  ,POINTER :: MBUF      
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      DATA F_GAUSS / 
     9 1.000000000000000,1.732050807568877,1.290994448735806,
     9 1.161256338324528,1.103533701926633,1.072421119155361,
     9 1.053620970803647,1.041352247171806,1.032886870574820/
      INTEGER :: NFT,IAD,ITY,NPT,JALE,ISMSTR,JEUL,JTUR,JTHE,JLAG,JMULT
      INTEGER :: JHBE,JIVF,NVAUX,JPOR,JCLOSE,JPLASOL,IREP,IINT,IGTYP
      INTEGER :: ISORTH,ISORTHG,ISRAT,ISROT,ICSEN,IFAILURE,JSMS

C-----------------------------------------------
      NSKIN = 0
      IS_WRITTEN_SKIN(1:NUMSKIN) = 0
      IF (NUMSKIN> NUMSKINP) THEN      
      DO NG=1,NGROUP
C      
        CALL INITBUF(   IPARG   ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
C     
       IF (MLW == 13 .OR. MLW == 0) CYCLE
C-----------------------------------------------
C       THICK-SHELL 
C-----------------------------------------------
!                8--------------7
!               / |            /|
!              5--------------|6
!              |  |           | |
!              |  4-----------|-3
!              | /            |/     
!              1--------------2
        IF (ITY == 1.AND.(IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22)) THEN
          NFT = IPARG(3,NG)
          ICSTR = IPARG(17,NG)
          NLAY = ELBUF_TAB(NG)%NLAY                
          NPTR = ELBUF_TAB(NG)%NPTR                 
          NPTS = ELBUF_TAB(NG)%NPTS                 
          NPTT = ELBUF_TAB(NG)%NPTT
          IOR_TSH = 0
          IF (IGTYP == 21) THEN
           IOR_TSH = 1
          ELSEIF (IGTYP == 22) THEN
           IOR_TSH = 2
          END IF
          IF (KCVT==1.AND.IOR_TSH/=0) KCVT=2
c
          DO I=1,NEL
            VALUE(I) = ZERO
            IS_WRITTEN_VALUE(I) = 0
            IOK_PART(I) = 0 
            IF( H3D_PART(IPARTS(NFT+I)) == 1) IOK_PART(I) = 1
          ENDDO	     
          MLWI = MLW
          IF (IGTYP == 22 .AND. NLAY>9) THEN
           F_EXP = ONE
          ELSE
           F_EXP = F_GAUSS(NLAY)
          END IF
          IF (JHBE==14.OR.JHBE==16)   F_EXP = F_EXP/(NPTR*NPTS)
C-----------------------------------------------
          IF (KEYWORD == 'FLDZ/OUTER') THEN
            IS_WRITTEN_VALUE(1:NEL) = 1
            MX = IXS(1,1 + NFT)
            NGL(1:NEL) =IXS(NIXS,1 + NFT:NEL + NFT) 
            IT = 1
C-----------------------------------------------
              ILAY=1
C-------- grp skin_inf first
            IF (IGTYP == 22) THEN
             PID = IXS(NIXS-1,1 + NFT)
             MID = IGEO(100+ILAY,PID)
             MLWI=NINT(PM(19,MID))
            END IF
            CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
C----------    F.I. uses also average strain to be consisting
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                   DO I=1,NEL 
                    RINDX = FBUF%FLOC(IFAIL)%INDX(I)                  
                    VALUE(I) = MAX(VALUE(I),RINDX) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
C------           
             DO I=1,NEL
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C-------- grp skin_up
              ILAY=NLAY
              VALUE(1:NEL) = ZERO
             IF (IGTYP == 22) THEN
              PID = IXS(NIXS-1,1 + NFT)
              MID = IGEO(100+ILAY,PID)
              MLWI=NINT(PM(19,MID))
             END IF
             CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  DO I=1,NEL                                                      
                     CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                    RINDX = FBUF%FLOC(IFAIL)%INDX(I)                  
                    VALUE(I) = MAX(VALUE(I),RINDX) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
             DO I=1,NEL
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C-----------------------------------------------
          ELSEIF (KEYWORD == 'FLDZ/OUTER_AVERAGE') THEN
            IS_WRITTEN_VALUE(1:NEL) = 1
            MX = IXS(1,1 + NFT)
            NGL(1:NEL) =IXS(NIXS,1 + NFT:NEL + NFT) 
            IT = 1
C-----------------------------------------------
            ILAY=(1+NLAY)/2
C-------- grp skin_inf first
            IF (IGTYP == 22) THEN
             PID = IXS(NIXS-1,1 + NFT)
             MID = IGEO(100+ILAY,PID)
             MLWI=NINT(PM(19,MID))
            END IF
            CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
C------
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                   DO I=1,NEL 
                    RINDX = FBUF%FLOC(IFAIL)%INDX(I)                  
                    VALUE(I) = MAX(VALUE(I),RINDX) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
C------           
             DO I=1,NEL
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C-------- grp skin_up
              ILAY=(1+NLAY)/2
              VALUE(1:NEL) = ZERO
             IF (IGTYP == 22) THEN
              PID = IXS(NIXS-1,1 + NFT)
              MID = IGEO(100+ILAY,PID)
              MLWI=NINT(PM(19,MID))
             END IF
             CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  DO I=1,NEL                                                      
                     CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                    RINDX = FBUF%FLOC(IFAIL)%INDX(I)                  
                    VALUE(I) = MAX(VALUE(I),RINDX) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
             DO I=1,NEL
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C-----------------------------------------------
          ELSEIF (KEYWORD == 'FLDF/OUTER') THEN
            IS_WRITTEN_VALUE(1:NEL) = 1
            MX = IXS(1,1 + NFT)
            NGL(1:NEL) =IXS(NIXS,1 + NFT:NEL + NFT) 
C-----------------------------------------------
              ILAY=1
              IT = 1
            IF (IGTYP == 22) THEN
             PID = IXS(NIXS-1,1 + NFT)
             MID = IGEO(100+ILAY,PID)
             MLWI=NINT(PM(19,MID))
            END IF
            CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
C-------- grp skin_inf first
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                  DO I=1,NEL                                                      
                    VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I)) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
C------           
             DO I=1,NEL
               N = I + NFT
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C-------- grp skin_up
              ILAY=NLAY
              IT = 1
             VALUE(1:NEL) = ZERO
             IF (IGTYP == 22) THEN
              PID = IXS(NIXS-1,1 + NFT)
              MID = IGEO(100+ILAY,PID)
              MLWI=NINT(PM(19,MID))
             END IF
             CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                  DO I=1,NEL                                                      
                    VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I)) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
             DO I=1,NEL
               N = I + NFT
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C------------to get right NSKIN for next case          
          ELSEIF (KEYWORD == 'FLDF/OUTER_AVERAGE') THEN
            IS_WRITTEN_VALUE(1:NEL) = 1
            MX = IXS(1,1 + NFT)
            NGL(1:NEL) =IXS(NIXS,1 + NFT:NEL + NFT) 
C-----------------------------------------------
              ILAY=(1+NLAY)/2
              IT = 1
            IF (IGTYP == 22) THEN
             PID = IXS(NIXS-1,1 + NFT)
             MID = IGEO(100+ILAY,PID)
             MLWI=NINT(PM(19,MID))
            END IF
            CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
C-------- grp skin_inf first
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                  DO I=1,NEL                                                      
                    VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I)) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
C------           
             DO I=1,NEL
               N = I + NFT
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C-------- grp skin_up
              ILAY=(1+NLAY)/2
              IT = 1
             VALUE(1:NEL) = ZERO
             IF (IGTYP == 22) THEN
              PID = IXS(NIXS-1,1 + NFT)
              MID = IGEO(100+ILAY,PID)
              MLWI=NINT(PM(19,MID))
             END IF
             CALL H3D_FLD_STRAIN(
     1   ELBUF_TAB(NG),X,            IXS,          JHBE,
     2   MLWI,         ILAY,         KCVT,         IOR_TSH,
     3   ICSTR,        NPTR,         NPTS,         NEL,
     4   F_EXP,        STRAIN,       IREP)
              IR = 1
              IS = 1
              FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
              NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL                                                                   
             DO IFAIL=1,NFAIL                                                          
               IF (FBUF%FLOC(IFAIL)%ILAWF == 7) THEN ! check /FLD model                
                  IP=(IFAIL -1)*15         
                  CALL H3D_FLD_TSH(ELBUF_TAB(NG),
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,STRAIN,NEL )
                  DO I=1,NEL                                                      
                    VALUE(I) = MAX(VALUE(I),FBUF%FLOC(IFAIL)%DAM(I)) 
                    IS_WRITTEN_VALUE(I) = 1	                         
                  ENDDO                                                                 
               ENDIF
             END DO               
             DO I=1,NEL
               N = I + NFT
               SKIN_SCALAR(NSKIN+I) = VALUE(I)
               IF(IOK_PART(I) == 1 ) IS_WRITTEN_SKIN(NSKIN+I) = IS_WRITTEN_VALUE(I)
             END DO
             NSKIN = NSKIN + NEL
C------------to get right NSKIN for next case          
          ELSE
            NSKIN = NSKIN + 2*NEL
          END IF !(KEYWORD
        END IF !(ITY == 1.AND.(IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22)) THEN
      END DO !NG=1,NGROUP
      END IF !(NUMSKIN> NUMSKINP) THEN      
C------for solid elements
       IF (NUMSKIN> (NSKIN+NUMSKINP))       
     .  CALL H3D_SOL_SKIN_SCALAR(
     .                   ELBUF_TAB,SKIN_SCALAR, IPARG   ,IXS     ,X     ,PM  ,
     4                   IPARTS  ,IPM     ,IGEO    ,IXS10 ,IXS16 , IXS20  ,
     5                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD ,NSKIN ,
     6                   IAD_ELEM        ,FR_ELEM     , WEIGHT   ,TAG_SKINS6,
     7                   NPF  ,TF   ,BUFMAT)
C------for solid elements
       IF (NUMSKINP> 0)       
     .  CALL H3D_PRE_SKIN_SCALAR(SKIN_SCALAR,NODAL_IPART,
     .                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD ,
     .                   IBCL,ILOADP,LLOADP,FAC ,NPF,TF ,SENSOR_TAB,
     .                   TAGNCONT,LOADP_HYD_INTER,FORC,XFRAME ,X ,V ,
     .                   IMAPSKP,NSKIN )
C-----------------------------------------------
      RETURN
      END
