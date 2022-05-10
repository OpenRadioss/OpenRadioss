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
Chd|  AETURB                        source/ale/aeturb.F           
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        ADIFF2                        source/ale/ale2d/adiff2.F     
Chd|        ADIFF3                        source/ale/ale3d/adiff3.F     
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE AETURB(
     1  IPARG   ,ELBUF_TAB,FLUX    ,VAL2    ,XE    ,
     2  ALE_CONNECT   ,IXS      ,IXQ     ,PM      ,ITASK ,
     3  NERCVOIS,NESDVOIS ,LERCVOIS,LESDVOIS,LENCOM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IXS(NIXS,*), IXQ(7,*),
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .        LENCOM
      my_real
     .  FLUX(*), VAL2(*), XE(*), PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, MT, ITASK
      my_real
     .   RE, R
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG) .EQ. 1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF(JALE+JEUL==0)   CYCLE
        IF(IPARG(8,NG)==1) CYCLE
        LFT=1
        GBUF => ELBUF_TAB(NG)%GBUF
c
        IF(JTUR==1)THEN
         IF(N2D==0)THEN
          DO 10 I=LFT,LLT
          J=I+NFT
          MT=IXS(1,J)
          RE=GBUF%RE(I)
          R =GBUF%RHO(I)
          XE(J)=RE/R
          VAL2(J)=VAL2(J)*PM(85,MT)/PM(86,MT)
   10     CONTINUE
         ELSE
          DO 15 I=LFT,LLT
          J=I+NFT
          MT=IXQ(1,J)
          RE=GBUF%RE(I)
          R =GBUF%RHO(I)
          XE(J)=RE/R
          VAL2(J)=VAL2(J)*PM(85,MT)/PM(86,MT)
   15     CONTINUE
         ENDIF
        ELSE
         DO 20 I=LFT,LLT
         J=I+NFT
   20    XE(J)=ZERO
        ENDIF
        IF(JPOR==2)THEN
C POROSITE OPTION TURBULENCE IMPOSEE ET PAS DE FLUX DIFFUSIFS 
         DO30 I=LFT,LLT
          J=I+NFT
          VAL2(J)=ZERO
   30    CONTINUE
         ENDIF
      ENDDO
C
      CALL MY_BARRIER
C-----------------------------
C       ECHANGES SPMD XE, VAL2 SUR ELTS VOISINS
C-----------------------------
      IF (NSPMD > 1) THEN
C
C Partie non parallele smt
C
!$OMP SINGLE
        CALL SPMD_EVOIS(XE      ,VAL2  ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                  LESDVOIS,LENCOM)
C Fin Partie non parallele smt
!$OMP END SINGLE

      END IF
C-----------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG) .EQ. 1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF(JALE+JEUL==0)    CYCLE
        IF(JTUR/=1)         CYCLE
        IF(MTN==11)         CYCLE
        IF(MTN==17)         CYCLE
        IF(IPARG(8,NG)==1)  CYCLE
c
        GBUF => ELBUF_TAB(NG)%GBUF
        LFT=1
        IF(N2D==0)THEN
         CALL ADIFF3(GBUF%RE,XE,FLUX(6*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL)
        ELSE
         CALL ADIFF2(GBUF%RE,XE,FLUX(4*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL)
        ENDIF
      ENDDO
C-----------
      CALL MY_BARRIER
C-----------
      RETURN
      END
