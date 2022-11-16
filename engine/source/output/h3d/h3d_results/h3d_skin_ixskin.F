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
Chd|  H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_PRE_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SOL_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_IXSKIN(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                           ITAB ,IXSKIN  ,TAG_SKINS6,IBCL  ,
     .                           ILOADP,LLOADP ,NODAL_IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "spmd_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IPARTS(*),ITAB(*),IXSKIN(NIXQ,*),
     .        IXS10(6,*) ,TAG_SKINS6(*),IBCL(*),ILOADP(*),LLOADP(*),
     .        NODAL_IPART(*)  
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NSKIN,ISOLNOD,ICS,NG,N,J
      INTEGER 
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    , 
     .        NN,NN1,N1,IDB
C-----------------------------------------------
       NSKIN =0
      IF (NUMSKIN> NUMSKINP) THEN      
       DO NG=1,NGROUP
        ISOLNOD = IPARG(28,NG)
        ICS     = IPARG(17,NG)                                        
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        IF(MLW == 13 .OR. MLW == 0) CYCLE          
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

C-------- grp skin_inf first
            DO I=1,NEL
              N = I + NFT
              IXSKIN(1,NSKIN+I) = IPARTS(N)
              IXSKIN(7,NSKIN+I) = NSKIN+I
            END DO
           IF (JHBE==14) THEN
             SELECT CASE (ICS)
               CASE(1) 
                  DO I=1,NEL
                    N = I + NFT
C-----------skin_inf 1,4,8,5              
                    IXSKIN(2,NSKIN+I) = ITAB(IXS(2,N))
                    IXSKIN(3,NSKIN+I) = ITAB(IXS(5,N))
                    IXSKIN(4,NSKIN+I) = ITAB(IXS(9,N))
                    IXSKIN(5,NSKIN+I) = ITAB(IXS(6,N))
                    IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c                    IXSKIN(7,NSKIN+I) = IXS(NIXS,N) +3
                  END DO
               CASE(10)                                             
                  DO I=1,NEL
                    N = I + NFT
C-----------skin_inf 1,2,3,4              
                    IXSKIN(2,NSKIN+I) = ITAB(IXS(2,N))
                    IXSKIN(3,NSKIN+I) = ITAB(IXS(3,N))
                    IXSKIN(4,NSKIN+I) = ITAB(IXS(4,N))
                    IXSKIN(5,NSKIN+I) = ITAB(IXS(5,N))
                    IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c                    IXSKIN(7,NSKIN+I) = IXS(NIXS,N) +1
                  ENDDO
               CASE(100)                                             
                  DO I=1,NEL
                    N = I + NFT
C-----------skin_inf 1,5,6,2              
                    IXSKIN(2,NSKIN+I) = ITAB(IXS(2,N))
                    IXSKIN(3,NSKIN+I) = ITAB(IXS(6,N))
                    IXSKIN(4,NSKIN+I) = ITAB(IXS(7,N))
                    IXSKIN(5,NSKIN+I) = ITAB(IXS(3,N))
                    IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c                    IXSKIN(7,NSKIN+I) = IXS(NIXS,N) +5
                  END DO
             END SELECT 
           ELSEIF (ISOLNOD==6) THEN !  penta
            DO I=1,NEL
              N = I + NFT
C-----------skin_inf 1,2,3,4              
              IXSKIN(2,NSKIN+I) = ITAB(IXS(2,N))
              IXSKIN(3,NSKIN+I) = ITAB(IXS(3,N))
              IXSKIN(4,NSKIN+I) = ITAB(IXS(4,N))
              IXSKIN(5,NSKIN+I) = ITAB(IXS(4,N))
              IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c              IXSKIN(7,NSKIN+I) = IXS(NIXS,N) + 1
            ENDDO
C---------            
           ELSE ! hexa 15,16
            DO I=1,NEL
              N = I + NFT
C-----------skin_inf 1,2,3,4              
              IXSKIN(2,NSKIN+I) = ITAB(IXS(2,N))
              IXSKIN(3,NSKIN+I) = ITAB(IXS(3,N))
              IXSKIN(4,NSKIN+I) = ITAB(IXS(4,N))
              IXSKIN(5,NSKIN+I) = ITAB(IXS(5,N))
              IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c              IXSKIN(7,NSKIN+I) = IXS(NIXS,N) + 1
            ENDDO
           END IF               
           NSKIN = NSKIN + NEL
C-------- grp skin_sup 
            DO I=1,NEL
              N = I + NFT
              IXSKIN(1,NSKIN+I) = IPARTS(N)
              IXSKIN(7,NSKIN+I) = NSKIN+I
            END DO
           IF (JHBE==14) THEN
             SELECT CASE (ICS)
               CASE(1) 
                  DO I=1,NEL
                    N = I + NFT
C-----------skin_sup 2,3,7,6              
                    IXSKIN(2,NSKIN+I) = ITAB(IXS(3,N))
                    IXSKIN(3,NSKIN+I) = ITAB(IXS(4,N))
                    IXSKIN(4,NSKIN+I) = ITAB(IXS(8,N))
                    IXSKIN(5,NSKIN+I) = ITAB(IXS(7,N))
                    IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c                    IXSKIN(7,NSKIN+I) = IXS(NIXS,N) +4
                  END DO
               CASE(10)                                             
                  DO I=1,NEL
                    N = I + NFT
C-----------skin_sup 5,6,7,8              
                    IXSKIN(2:5,NSKIN+I) = ITAB(IXS(6:9,N))
                    IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c                    IXSKIN(7,NSKIN+I) = IXS(NIXS,N) +2
                  ENDDO
               CASE(100)                                             
                  DO I=1,NEL
                    N = I + NFT
C-----------skin_sup 4,8,7,3              
                    IXSKIN(2,NSKIN+I) = ITAB(IXS(5,N))
                    IXSKIN(3,NSKIN+I) = ITAB(IXS(9,N))
                    IXSKIN(4,NSKIN+I) = ITAB(IXS(8,N))
                    IXSKIN(5,NSKIN+I) = ITAB(IXS(4,N))
                    IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c                    IXSKIN(7,NSKIN+I) = IXS(NIXS,N) +6
                  END DO
             END SELECT 
           ELSEIF (ISOLNOD==6) THEN !  penta
            DO I=1,NEL
              N = I + NFT
C-----------skin_sup 5,6,7,8              
              IXSKIN(2:4,NSKIN+I) = ITAB(IXS(6:8,N))
              IXSKIN(5,NSKIN+I) = IXSKIN(4,NSKIN+I)
              IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c              IXSKIN(7,NSKIN+I) = IXS(NIXS,N) + IXS(NIXS,N)
            ENDDO
           ELSE ! 15,16
            DO I=1,NEL
              N = I + NFT
C-----------skin_sup 5,6,7,8              
              IXSKIN(2:5,NSKIN+I) = ITAB(IXS(6:9,N))
              IXSKIN(6,NSKIN+I) = IXS(NIXS-1,N)
c              IXSKIN(7,NSKIN+I) = IXS(NIXS,N) + IXS(NIXS,N)
            ENDDO
           END IF               
           NSKIN = NSKIN + NEL
C-----------------------------------------------
        ENDIF !IF (ITY == 1.AND.(IGTYP==20 
       END DO ! NG=1,NGROUP
      END IF !(NUMSKIN> NUMSKINP) THEN      
C------SOLID elements 
       NFT = NSKIN
      IF (NUMSKIN> (NSKIN+NUMSKINP)) THEN      
       CALL H3D_SOL_SKIN_IXSKIN(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                           IXSKIN  ,TAG_SKINS6,NSKIN )
       DO I=NFT+1,NSKIN
         IXSKIN(2:5,I) = ITAB(IXSKIN(2:5,I))
       END DO 
      END IF !(NUMSKIN> (NSKIN+NUMSKINP)) THEN      
C------to show pressure 
       NFT = NSKIN
      IF (NUMSKINP>0) THEN      
       CALL H3D_PRE_SKIN_IXSKIN(NODAL_IPART,IBCL ,ILOADP,LLOADP,
     .                          IXSKIN  ,NSKIN )
       DO I=NFT+1,NSKIN
         IXSKIN(2:5,I) = ITAB(IXSKIN(2:5,I))
       END DO 
      END IF      
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_SKIN_PRE_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_SOL_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_TSH_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|====================================================================
      SUBROUTINE H3D_SKIN_DIM(IPARG ,NUMSKIN, NUMSKING,TAG_SKINS6,IBCL,
     .   	                  ILOADP,LLOADP ,NUMSKINP,IFLAG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NUMSKIN, NUMSKING ,IPARG(*),TAG_SKINS6(*),
     .        IBCL(*),ILOADP(*),LLOADP(*),NUMSKINP,IFLAG	  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I, LEN ,N0  

C-----------------------------------------------
       NUMSKIN = 0
C--- thick-shell first, solid after
       IF (IFLAG==1.OR.IFLAG==3) THEN
        CALL  H3D_SKIN_TSH_DIM(IPARG    ,TAG_SKINS6, NUMSKIN)     
        CALL  H3D_SKIN_SOL_DIM(IPARG    ,TAG_SKINS6, NUMSKIN)
       END IF        
C--- followed by pressure: pblast, pfluid,force
       N0 = NUMSKIN
C------!!!!create NODAL_IPART(numnod) before       
       IF (IFLAG>1) CALL H3D_SKIN_PRE_DIM (IBCL   ,ILOADP    ,LLOADP , NUMSKIN)
	   NUMSKINP =NUMSKIN- N0
C
       LEN = 1       
       IF (NSPMD>1) THEN
         NUMSKING = NUMSKIN
         CALL SPMD_ALLGLOB_ISUM9(NUMSKING,LEN)
       ELSE
        NUMSKING = NUMSKIN
       END IF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  H3D_SKIN_TSH_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_TSH_DIM(IPARG    ,TAG_SKINS6, NUMSKIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),NUMSKIN, TAG_SKINS6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NG,
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS   ,
     .        NN,ICS,ISOLNOD
C-----------------------------------------------
       DO NG=1,NGROUP
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        IF(MLW == 13 .OR. MLW == 0.OR.ITY /= 1) CYCLE   
C------ possible to remove(w/ TAG_SKINS6) case of several layers in mesh (even not recommended)        
        IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) THEN
            NUMSKIN = NUMSKIN + 2*NEL
        ENDIF !IF (IGTYP==20 
       END DO ! NG=1,NGROUP
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  H3D_SKIN_SOL_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_SOL_DIM(IPARG   ,TAG_SKINS6, NUMSKIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),NUMSKIN, TAG_SKINS6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NG,N,J,N1,N2,
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS   ,
     .        NN,ICS,ISOLNOD,NN1,JJ,II,K1,K2,NF(3),NSKIN2,IDB
      INTEGER FACES(4,6),NS(4),PWR(7),LL
      DATA PWR/1,2,4,8,16,32,64/
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
C----tetra4:-------------------------------------------
c              N8=N4   FACES : 2 2 1 1
c              N7=N3           4 3 3 4
c              N6=N3           1 1 3 4
c              N5=N4           2 2 4 3
c              N4=N2           1 2 3 3
c              N3=N2           1 4 4 2
C-----------------------------------------------
       DO NG=1,NGROUP
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        IF(MLW == 13 .OR. MLW == 0.OR.ITY /= 1) CYCLE   
C------         
        IF (IGTYP==6 .OR. IGTYP==14 ) THEN
           ISOLNOD = IPARG(28,NG)
           ICS     = IPARG(17,NG)                                        
           IF(ISOLNOD == 4)THEN
             DO I=1,NEL
               N = I + NFT
               LL=TAG_SKINS6(N)
               DO JJ=3,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                NUMSKIN = NUMSKIN + 1
               END DO
             ENDDO
           ELSEIF(ISOLNOD == 6)THEN
           ELSEIF(ISOLNOD == 10)THEN
             DO I=1,NEL
               N = I + NFT
               LL=TAG_SKINS6(N)
               DO JJ=3,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                NUMSKIN = NUMSKIN + 4
               END DO
             ENDDO
           ELSE
             DO I=1,NEL
               N = I + NFT
               LL=TAG_SKINS6(N)
               DO JJ=1,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                NUMSKIN = NUMSKIN + 1
               END DO
             ENDDO
           ENDIF
        ENDIF !IF (IGTYP== 
       END DO ! NG=1,NGROUP
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  H3D_SKIN_PRE_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE H3D_SKIN_PRE_DIM (IB     ,ILOADP    ,LLOADP , NSKIN)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE PINCHTYPE_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLOADP(SLLOADP),NSKIN
      INTEGER ILOADP(SIZLOADP,*),IB(NIBCLD,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, 
     .        IAD ,NP ,IFUNC ,NPRES ,N
C=======================================================================
C-----Force (pressure) first 
       DO NL=1,NCONLD-NPLOADPINCH
         N1      = IB(1,NL)
         N2      = IB(2,NL)
         N3      = IB(3,NL)
         N4      = IB(4,NL)
C        
		 IF (N1>0.AND.N2>0.AND.N3>0.AND.N4/=-1) NSKIN = NSKIN + 1
       END DO 
C----------load_pressure
       DO  NL=1,NLOADP_HYD
           IAD = ILOADP(4,NL)
           DO N=1, ILOADP(1,NL)/4
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)
		     IF (N1>0.AND.N2>0.AND.N3>0) NSKIN = NSKIN + 1
           ENDDO
       END DO !NP=1,NLOADP_HYD
C---------pfluid+pblast	   
       DO NL=1,NLOADP_F+NLOADP_B
         IAD = ILOADP(4,NL)
           DO N=1, ILOADP(1,NL)/4
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)
		     IF (N1>0.AND.N2>0.AND.N3>0) NSKIN = NSKIN + 1
           ENDDO
       END DO 
C
      RETURN
      END
Chd|====================================================================
Chd|  H3D_PRE_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE H3D_PRE_SKIN_IXSKIN(NODAL_IPART,IB ,ILOADP,LLOADP,
     .                               IXSKIN  ,NSKIN )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE PINCHTYPE_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLOADP(SLLOADP),NSKIN,IXSKIN(NIXQ,*)
      INTEGER ILOADP(SIZLOADP,*),IB(NIBCLD,*),NODAL_IPART(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, 
     .        IAD ,NP ,IFUNC ,NPRES,NSKIN0,NSKIN1,N,I,PID0
C=======================================================================
C-----IXSKIN(1,*):ipart;IXSKIN(2:5,*):N1-4,IXSKIN(6,*):pid;IXSKIN(7,*):usr_id
       PID0 =0
       NSKIN0 = NSKIN
C-----Force (pressure) first 
       DO NL=1,NCONLD-NPLOADPINCH
         N1      = IB(1,NL)
         N2      = IB(2,NL)
         N3      = IB(3,NL)
         N4      = IB(4,NL)
		 IF (N1==0.OR.N2==0.OR.N3==0.OR.N4==-1) CYCLE
C        
		 NSKIN = NSKIN + 1
		 IXSKIN(2,NSKIN)=N1
		 IXSKIN(3,NSKIN)=N2
		 IXSKIN(4,NSKIN)=N3
		 IF (N4==0) THEN
		  IXSKIN(5,NSKIN)=N3
		 ELSEIF(N4>0) THEN
		  IXSKIN(5,NSKIN)=N4
		 END IF
		 IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		 IXSKIN(6,NSKIN)=NSKIN0+NL
       END DO 
       NSKIN1 = NSKIN
C----------load_pressure	   
       DO NL=1,NLOADP_HYD
         IAD = ILOADP(4,NL)
         DO N=1, ILOADP(1,NL)/4
           N1 = LLOADP(IAD+4*(N-1))
           N2 = LLOADP(IAD+4*(N-1)+1)
           N3 = LLOADP(IAD+4*(N-1)+2)
           N4 = LLOADP(IAD+4*(N-1)+3)
		   IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
C          
		   NSKIN = NSKIN + 1
		   IXSKIN(2,NSKIN)=N1
		   IXSKIN(3,NSKIN)=N2
		   IXSKIN(4,NSKIN)=N3
		   IF (N4==0) THEN
		    IXSKIN(5,NSKIN)=N3
		   ELSEIF(N4>0) THEN
		    IXSKIN(5,NSKIN)=N4
		   END IF
		   IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		   IXSKIN(6,NSKIN)=NSKIN1+NL
         ENDDO
       END DO !NP=1,NLOADP_HYD
C---------pfluid	   
       NSKIN1 = NSKIN
       DO NL=1,NLOADP_F
         IAD = ILOADP(4,NL)
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(IAD+4*(I-1))
           N2=LLOADP(IAD+4*(I-1)+1)
           N3=LLOADP(IAD+4*(I-1)+2)
           N4=LLOADP(IAD+4*(I-1)+3)
		   IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
C          
		   NSKIN = NSKIN + 1
		   IXSKIN(2,NSKIN)=N1
		   IXSKIN(3,NSKIN)=N2
		   IXSKIN(4,NSKIN)=N3
		   IF (N4==0) THEN
		    IXSKIN(5,NSKIN)=N3
		   ELSEIF(N4>0) THEN
		    IXSKIN(5,NSKIN)=N4
		   END IF
		   IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		   IXSKIN(6,NSKIN)=NSKIN1+NL
         END DO 
       END DO 
C---------pblast	   
       NSKIN1 = NSKIN
       DO NL=1+NLOADP_F,NLOADP_F+NLOADP_B
         IAD = ILOADP(4,NL)
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(IAD+4*(I-1))
           N2=LLOADP(IAD+4*(I-1)+1)
           N3=LLOADP(IAD+4*(I-1)+2)
           N4=LLOADP(IAD+4*(I-1)+3)
		   IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
C          
		   NSKIN = NSKIN + 1
		   IXSKIN(2,NSKIN)=N1
		   IXSKIN(3,NSKIN)=N2
		   IXSKIN(4,NSKIN)=N3
		   IF (N4==0) THEN
		    IXSKIN(5,NSKIN)=N3
		   ELSEIF(N4>0) THEN
		    IXSKIN(5,NSKIN)=N4
		   END IF
		   IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		   IXSKIN(6,NSKIN)=NSKIN1+NL
         END DO 
       END DO 
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_NODAL_IPART               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE GET_NODAL_IPART(ELBUF_TAB,IPARG,IPARTC,IPARTTG,IPARTS,
     .                           IXC, IXTG, IXS,IXS10,IXS16,IXS20,
     .                           NODAL_IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IPARTS(*),
     .        IXC(NIXC,*),IXTG(NIXTG,*),IXS10(6,*),IXS16(8,*) ,
     .        IXS20(12,*),IPARTC(*),IPARTTG(*),NODAL_IPART(*)  
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NSKIN,ISOLNOD,ICS,NG,N,J,NJ
      INTEGER 
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    , 
     .        NN,NN1,N1,IDB
C-----------------------------------------------
       DO NG=1,NGROUP
        ISOLNOD = IPARG(28,NG)
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        IF(MLW == 13 .OR. MLW == 0) CYCLE          
C----------------------------------------------- 
C           COQUES 3 N 4 N
C-----------------------------------------------
        IF (ITY == 3 ) THEN
            DO I=1,NEL
              N = I + NFT
              NODAL_IPART(IXC(2:5,N))=IPARTC(N)
            END DO
        ELSEIF (ITY == 7) THEN 
            DO I=1,NEL
              N = I + NFT
              NODAL_IPART(IXTG(2:4,N))=IPARTTG(N)
            END DO
C-----------------------------------------------
C       SOLID 
C-----------------------------------------------
        ELSEIF (ITY == 1) THEN

          SELECT CASE (ISOLNOD)
             CASE(4) 
              DO I=1,NEL
               N = I + NFT
               NODAL_IPART(IXS(2,N)) =IPARTS(N) 
               NODAL_IPART(IXS(4,N)) =IPARTS(N) 
               NODAL_IPART(IXS(7,N)) =IPARTS(N) 
               NODAL_IPART(IXS(6,N)) =IPARTS(N) 
              END DO 
             CASE(6) 
              DO I=1,NEL
               N = I + NFT
               NODAL_IPART(IXS(2,N)) =IPARTS(N) 
               NODAL_IPART(IXS(3,N)) =IPARTS(N) 
               NODAL_IPART(IXS(4,N)) =IPARTS(N) 
               NODAL_IPART(IXS(7,N)) =IPARTS(N) 
               NODAL_IPART(IXS(6,N)) =IPARTS(N) 
               NODAL_IPART(IXS(8,N)) =IPARTS(N) 
              END DO 
             CASE(8) 
              DO I=1,NEL
               N = I + NFT
               NODAL_IPART(IXS(2:9,N)) =IPARTS(N) 
              END DO 
             CASE(10) 
              DO I=1,NEL
               N = I + NFT
               NODAL_IPART(IXS(2,N)) =IPARTS(N) 
               NODAL_IPART(IXS(4,N)) =IPARTS(N) 
               NODAL_IPART(IXS(7,N)) =IPARTS(N) 
               NODAL_IPART(IXS(6,N)) =IPARTS(N)
               NN1 = N - NUMELS8
               DO J=1,6
                NJ=IXS10(J,NN1)
                IF (NJ>0) NODAL_IPART(NJ) =IPARTS(N) 
               END DO               
              END DO 
             CASE(16) 
               NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
              DO I=1,NEL
               N = I + NFT
               NODAL_IPART(IXS(2:9,N)) =IPARTS(N) 
               DO J=1,8
                NJ=IXS16(J,NN1)
                IF (NJ>0) NODAL_IPART(NJ) =IPARTS(N) 
               END DO               
              END DO 
             CASE(20) 
               NN1 = N - (NUMELS8+NUMELS10)
              DO I=1,NEL
               N = I + NFT
               NODAL_IPART(IXS(2:9,N)) =IPARTS(N) 
               DO J=1,12
                NJ=IXS20(J,NN1)
                IF (NJ>0) NODAL_IPART(NJ) =IPARTS(N) 
               END DO               
              END DO 
          END SELECT 
C-----------------------------------------------
        ENDIF 
       END DO ! NG=1,NGROUP
C-----------
      RETURN
      END
