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
Chd|  SPWFVIS                       source/elements/sph/spwfvis.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE SPWFVIS(SPBUF, IPARTSP, PARTSAV, IPARG, ELBUF_TAB,
     .                   KXSP , WASPACT)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARTSP(*), IPARG(NPARG,*), KXSP(NISP,*), WASPACT(*)
      my_real SPBUF(NSPBUF,*) , PARTSAV(NPSAV,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,IPRT, NG, NEL, NS
      my_real
     .        VOLO,DT05
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C===============================================
      DT05=HALF*DT2
      DO NS=1,NSPHACT
        N=WASPACT(NS)
        IF (KXSP(2,N)>0)THEN
C         pour sorties par cellule.
          SPBUF(10,N)=SPBUF(10,N)+DT05*SPBUF(11,N)
C         le travail des forces de viscosite artificielle est 
C         reinjecte dans l'energie interne.
C
          NG=MOD(KXSP(2,N),NGROUP+1)
          GBUF => ELBUF_TAB(NG)%GBUF
          CALL INITBUF (IPARG    ,NG      ,                      
     2                  MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,       
     3                  NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,       
     4                  JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,       
     5                  NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,       
     6                  IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,       
     7                  ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
c
          VOLO=GBUF%VOL(N-NFT)
          GBUF%EINT(N-NFT) = GBUF%EINT(N-NFT)       
     .                       - DT12*SPBUF(11,N)/MAX(EM20,VOLO)
          IPRT=IPARTSP(N)
          PARTSAV(1,IPRT)=PARTSAV(1,IPRT)-DT12*SPBUF(11,N)
        END IF
      ENDDO
C-----------------------------------------------
      RETURN
      END
