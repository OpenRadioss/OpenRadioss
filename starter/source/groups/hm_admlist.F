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
Chd|  HM_ADMLIST                    source/groups/hm_admlist.F    
Chd|-- called by -----------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_ADMLIST(NIX     ,IX      ,NUMEL   ,IPARTEL ,IPART   ,
     .                   KELTREE ,ELTREE  ,KSONTREE,NSONTREE,KLEVTREE,
     .                   NLIST   ,MESS    ,IX1     ,IX2     ,INDEX   ,
     .                   KK      ,NEL     ,NELT    ,ELKEY    ,ID      ,
     .                   TITR    ,LSUBMODEL)
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX, IX(NIX,*), NUMEL, IPARTEL(*), IPART(LIPART1,*),
     .        KELTREE, ELTREE(KELTREE,*), KSONTREE, NSONTREE, KLEVTREE,
     .        NLIST, IX1(*), IX2(*), INDEX(*), KK, NEL, NELT(*)
      CHARACTER MESS*40
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER ELKEY*4
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LIST(NLIST), IL, JJ, NENTITY, II
      INTEGER I, J, NOLD, K, IWORK(70000)
      INTEGER LELT, LELT1, LELT2, NE, KE, IP, LEVEL, NLEV,
     .        LELTMP, NELTMP(NSONTREE**(LEVELMAX+1))
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C CONSTITUTION DE LIST
C-----------------------------------------------
      IL   =0

      CALL HM_GET_INTV  ('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)

      DO II=1,NENTITY
          CALL HM_GET_INT_ARRAY_INDEX ('ids'    ,JJ    ,II,IS_AVAILABLE,LSUBMODEL)
          IF(JJ /= 0)THEN
            IL = IL + 1
            LIST(IL)=JJ
          ENDIF
      ENDDO! NEXT II
      
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C AVEC SUPPRESSION DES No DOUBLES
C-----------------------
      CALL MY_ORDERS(0,IWORK,LIST,INDEX,NLIST,1)
      DO I=1,NLIST
        INDEX(NLIST+I) = LIST(INDEX(I))
      ENDDO
      K=1
      NOLD = INDEX(NLIST+1)
      DO I=1,NLIST
        IF(NOLD.NE.INDEX(NLIST+I))K=K+1
        LIST(K) = INDEX(NLIST+I)
        NOLD    = INDEX(NLIST+I)
      ENDDO
      NEL=K
C-----------------------
C TRI DE IX EN ORDRE CROISSANT si KK = 0
C-----------------------
      IF(KK.EQ.0)THEN
        DO I=1,NUMEL
          IX2(I) = IX(NIX,I)
        ENDDO
        CALL MY_ORDERS(0,IWORK,IX2,INDEX,NUMEL,1)
        DO I=1,NUMEL
          IX1(I) = IX2(INDEX(I))
        ENDDO
        DO I=1,NUMEL
          IX2(I) = INDEX(I)
        ENDDO
      ENDIF
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IX()
C  ALGO < NLIST+NUMEL
C-----------------------
      I=1
      J=1
      DO I=1,NEL
        DO WHILE(LIST(I).GT.IX1(J).AND.J.LT.NUMEL)
          J=J+1
        ENDDO
        IF(LIST(I).EQ.IX1(J))THEN
          LIST(I)=IX2(J)
        ELSE
           CALL ANCMSG(MSGID=70,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=ELKEY,
     .                 I1=ID,
     .                 C2=TRIM(TITR),
     .                 I2=LIST(I))
           RETURN
        ENDIF
      ENDDO
C-----------------------
C
C-----------------------
      LELT = 0
C
      DO I=1,NEL
        NE=LIST(I)

        IP=IPARTEL(NE)
        NLEV =IPART(10,IP)

        IF(NLEV==0)THEN
          LELT=LELT+1
          NELT(LELT)=NE
        ELSE

          LELTMP   =1
          NELTMP(1)=NE

          LEVEL    =ELTREE(KLEVTREE,NE)
          IF(LEVEL < 0) LEVEL=-(LEVEL+1)

          LELT1=0
          LELT2=LELTMP

          DO WHILE(LEVEL < NLEV)
            DO KE=LELT1+1,LELT2
              DO K=0,NSONTREE-1
                LELTMP=LELTMP+1
                NELTMP(LELTMP)=ELTREE(KSONTREE,NELTMP(KE))+K
              END DO
            END DO

            LELT1=LELT2
            LELT2=LELTMP

            LEVEL=LEVEL+1
          END DO
           
          DO KE=LELT1+1,LELT2
            LELT=LELT+1
            NELT(LELT)=NELTMP(KE)
          END DO

        END IF
      END DO
C
      NEL=LELT
C-----------------------
      RETURN
      END     
