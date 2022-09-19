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
C remplit le tableau segvar avec les valeurs des variables de  l ments voisins
C sert pour les interfaces et pour les ebc
Chd|====================================================================
Chd|  SEGGETV                       source/interfaces/interf/seggetv.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        VARCONDEC                     source/interfaces/interf/varcondec.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE SEGGETV(IPARG,ELBUF_TAB,ALE_CONNECTIVITY,ITASK,SEGVAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "conv_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,ngroup), ITASK
      TYPE(t_segvar),TARGET :: SEGVAR
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, IV, KVAR, KK, JCODV(LCONV),CODTOT, SEGAD, IAD2, LGTH
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(BUF_MAT_)  ,POINTER :: MBUF
C=======================================================================
C
      CALL MY_BARRIER
C
      DO 100 NG=ITASK+1,NGROUP,NTHREAD
         IF (IPARG(8,NG) == 1) GO TO 100
c
         CALL VARCONDEC(JCODV,IPARG(34,NG),CODTOT)
         IF (CODTOT == 0) GOTO 100
         CALL INITBUF (IPARG    ,NG      ,
     2         MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4         JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
         LFT=1
c         NBB(1) =  NB4
c         NBB(2) =  NB3
c         NBB(3) =  NB10
c         NBB(4) =  NB12
         GBUF => ELBUF_TAB(NG)%GBUF
         MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)

         DO  I=LFT,LLT
           J=I+NFT
           IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(J)
           LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(J+1)-
     .          ALE_CONNECTIVITY%ee_connect%iad_connect(J)
           DO IV=1,LGTH
             IF (ALE_CONNECTIVITY%ee_connect%connected(IAD2 + IV - 1) < 0) THEN
               KK=-ALE_CONNECTIVITY%ee_connect%connected(IAD2 + IV - 1)
c
               KVAR = 1
               IF (JCODV(KVAR) == 1) THEN
                 SEGVAR%RHO(KK) = GBUF%RHO(I)
               ENDIF
c
               KVAR = 2
               IF (JCODV(KVAR) == 1) THEN
                 SEGVAR%EINT(KK) = GBUF%EINT(I)
               ENDIF
c
               KVAR = 3
               IF (JCODV(KVAR) == 1) THEN
                 SEGVAR%RK(KK) = GBUF%RK(I)
               ENDIF
c
               KVAR = 4
               IF (JCODV(KVAR) == 1) THEN
                 SEGVAR%RE(KK) = GBUF%RE(I)
               ENDIF
c
               KVAR = 5
               IF (JCODV(KVAR) == 1) THEN    !  UVAR(1)
                 SEGVAR%UVAR(KK) = MBUF%VAR(LLT*(I-1)+1)
               ENDIF

             ENDIF
           ENDDO
   50    CONTINUE
         ENDDO
  100  CONTINUE
c
c      write(iout,*)'NVCONV =',NVCONV,' NSEGFLU=',NSEGFLU
c      write(iout,'(2(1pE10.3))')(segvar(i),i=1,NVCONV*NSEGFLU)
C
      CALL MY_BARRIER
c-----------
      RETURN
      END

