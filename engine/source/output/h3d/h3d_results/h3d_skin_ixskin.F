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
Chd|  H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_PRE_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SOL_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_IXSKIN(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                           ITAB ,IXSKIN  ,TAG_SKINS6,IBCL  ,
     .                           ILOADP,LLOADP ,NODAL_IPART,IMAPSKP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE H3D_INC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NUMSKINP0), INTENT(IN)  :: IMAPSKP
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
        CALL INITBUF(IPARG    ,NG      ,                      
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
     .                          IXSKIN  ,NSKIN ,IMAPSKP)
       DO I=NFT+1,NSKIN
         IXSKIN(2:5,I) = ITAB(IXSKIN(2:5,I))
       END DO 
      END IF      
C-----------
      RETURN
      END
