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
