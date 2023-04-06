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
Chd|  STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NINTLST2                      source/system/nintrr.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE STATE_ADMESH(IPART ,IPARTC,IPARTTG,IXC   ,IXTG  ,
     .                      SH4TREE,SH3TREE,SH4TRIM,SH3TRIM,LSUBMODEL)
C----------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .        IXC(NIXC,*), IXTG(NIXTG,*),
     .        SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .        SH4TRIM(*), SH3TRIM(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,ID1,ID2,ID3,ID4,II,I1,I2,I3,I4,NLIST,N,LEVEL,NN,
     .        ITRIM,I
      INTEGER IERROR, NINTLST2,ERRORADJ,NSHELL,NSH3N
      INTEGER, DIMENSION(:),ALLOCATABLE :: LIST,INDEXL
      INTEGER IX1(MAX(NUMELC,NUMELTG)), 
     .        IX2(MAX(NUMELC,NUMELTG)), 
     .        INDEX(2*MAX(NUMELC,NUMELTG))
C     REAL
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey,KEY2*ncharkey
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
      DATA MESS /'ADAPTIVE MESHING STATE DEFINITION       '/
C-----------------------------------------------
      NLIST   =0

C-------------------------------------------------------------
C    READING /ADMESH/STATE/SHELL : 4-NODE SHELLS => Counting
C------------------------------------------------------------
C
      CALL HM_OPTION_START('/ADMESH/STATE')
C
      DO N =1,NADMESHSTAT
        TITR = ''
C
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

        IF(KEY2(1:LEN_TRIM(KEY2))=='SHELL')THEN

C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NSHELL',NSHELL,IS_AVAILABLE,LSUBMODEL)
          NLIST = NLIST + NSHELL
C
        ENDIF

      ENDDO
C------
      ALLOCATE(LIST  (5*NLIST),STAT=IERROR)
      IF(IERROR/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='LIST')     
      ALLOCATE(INDEXL(10*NLIST),STAT=IERROR)
      IF(IERROR/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='INDEXL')     

      NLIST=0

C-------------------------------------------------------------------------------
C    READING /ADMESH/STAT/SHELL : 4-NODE SHELLS => Storing and Tri( local Ids)
C-------------------------------------------------------------------------------
C
      CALL HM_OPTION_START('/ADMESH/STATE')

      DO N =1,NADMESHSTAT
        TITR = ''

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

        IF(KEY2(1:LEN_TRIM(KEY2))=='SHELL')THEN

C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NSHELL',NSHELL,IS_AVAILABLE,LSUBMODEL)
          
          DO I=1,NSHELL

              CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID1',ID1,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID2',ID2,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID3',ID3,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID4',ID4,I,IS_AVAILABLE,LSUBMODEL)

              NLIST=NLIST+1
              LIST(NLIST)=ID
              NLIST=NLIST+1
              LIST(NLIST)=ID1
              NLIST=NLIST+1
              LIST(NLIST)=ID2
              NLIST=NLIST+1
              LIST(NLIST)=ID3
              NLIST=NLIST+1
              LIST(NLIST)=ID4
C
          ENDDO
        ENDIF
      ENDDO

      NN=NINTLST2(LIST,NLIST,INDEXL,IXC,NIXC,NUMELC,
     .       MESS,IX1,IX2,INDEX,0)

      NLIST=0

C---------------------------------------------------------------------
C    READING /ADMESH/STAT/SHELL : 4-NODE SHELLS => Storing in SH4TREE
C---------------------------------------------------------------------

C
      CALL HM_OPTION_START('/ADMESH/STATE')

      DO N =1,NADMESHSTAT
        TITR = ''

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

        IF(KEY2(1:LEN_TRIM(KEY2))=='SHELL')THEN

C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NSHELL',NSHELL,IS_AVAILABLE,LSUBMODEL)
          
          DO I=1,NSHELL


              CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID1',ID1,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID2',ID2,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID3',ID3,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('shell_ID4',ID4,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('Actlev',LEVEL,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('IMapping',ITRIM,I,IS_AVAILABLE,LSUBMODEL)

              NLIST=NLIST+1
              II=LIST(NLIST)
              NLIST=NLIST+1
              I1=LIST(NLIST)
              NLIST=NLIST+1
              I2=LIST(NLIST)
              NLIST=NLIST+1
              I3=LIST(NLIST)
              NLIST=NLIST+1
              I4=LIST(NLIST)
              IF(I1+I2+I3+I4 /=0 .AND.
     .           (I2-I1/=1 .OR. I3-I1 /= 2 .OR. I4-I1 /= 3))THEN
                CALL ANCMSG(MSGID=654,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID1,
     .                  I2=ID2,
     .                  I3=ID3,
     .                  I4=ID4,
     .                  I5=ID)
              END IF
C
              IF(I1+I2+I3+I4 /=0) THEN
                ERRORADJ =0 
                IF(IXC(2,II) /= IXC(2,I1).OR.IXC(3,II) /= IXC(3,I2)
     .    .OR.IXC(4,II) /= IXC(4,I3).OR.IXC(5,II) /= IXC(5,I4)) THEN
                 ERRORADJ =1                 
                ELSEIF(IXC(4,I1) /= IXC(5,I2).OR.IXC(5,I2) /= IXC(2,I3)
     .    .OR.IXC(2,I3) /= IXC(3,I4).OR.IXC(4,I1) /= IXC(3,I4)) THEN
                 ERRORADJ =1 
                ELSEIF(IXC(3,I1) /= IXC(2,I2).OR.IXC(4,I2) /= IXC(3,I3)
     .    .OR.IXC(5,I3) /= IXC(4,I4).OR.IXC(5,I1) /= IXC(2,I4)) THEN
                 ERRORADJ =1 
                ENDIF
              ENDIF
              IF(ERRORADJ ==1.AND.ABS(LEVEL)<LEVELMAX) THEN 
                CALL ANCMSG(MSGID=1023,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID1,
     .                  I2=ID2,
     .                  I3=ID3,
     .                  I4=ID4,
     .                  I5=ID)
              END IF
C
              IF(LEVEL<-LEVELMAX-1.OR.LEVEL>LEVELMAX)THEN
            CALL ANCMSG(MSGID=656,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID)
              END IF
              SH4TREE(2,II)=I1
              SH4TREE(3,II)=LEVEL
              IF(I1/=0)THEN
                SH4TREE(1,I1)=II
                SH4TREE(1,I2)=II
                SH4TREE(1,I3)=II
                SH4TREE(1,I4)=II
              END IF
              SH4TRIM(II)=ITRIM
C
          ENDDO
        ENDIF
      ENDDO

      DEALLOCATE(LIST)
      DEALLOCATE(INDEXL)

C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      NLIST=0

C-----------------------------------------------------------
C    READING /ADMESH/STAT/SH3N : 3-NODE SHELLS => Counting
C----------------------------------------------------------

      CALL HM_OPTION_START('/ADMESH/STATE')

      DO N =1,NADMESHSTAT
        TITR = ''

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

        IF(KEY2(1:LEN_TRIM(KEY2))=='SH3N')THEN

C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NSH3N',NSH3N,IS_AVAILABLE,LSUBMODEL)
          NLIST = NLIST + NSH3N
C

        ENDIF

      ENDDO

C------
      ALLOCATE(LIST(5*NLIST),STAT=IERROR)
      IF(IERROR/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='LIST')     
      ALLOCATE(INDEXL(10*NLIST),STAT=IERROR)
      IF(IERROR/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='INDEXL')     

C---------

      NLIST=0

C------------------------------------------------------------------------------
C    READING /ADMESH/STAT/SH3N : 3-NODE SHELLS => Storing and Tri( local Ids)
C------------------------------------------------------------------------------

      CALL HM_OPTION_START('/ADMESH/STATE')

      DO N =1,NADMESHSTAT
        TITR = ''

        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

        IF(KEY2(1:LEN_TRIM(KEY2))=='SH3N')THEN
C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NSH3N',NSH3N,IS_AVAILABLE,LSUBMODEL)

          DO I=1,NSH3N

              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID1',ID1,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID2',ID2,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID3',ID3,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID4',ID4,I,IS_AVAILABLE,LSUBMODEL)

              NLIST=NLIST+1
              LIST(NLIST)=ID
              NLIST=NLIST+1
              LIST(NLIST)=ID1
              NLIST=NLIST+1
              LIST(NLIST)=ID2
              NLIST=NLIST+1
              LIST(NLIST)=ID3
              NLIST=NLIST+1
              LIST(NLIST)=ID4
          ENDDO
C

        ENDIF

      ENDDO

      NN=NINTLST2(LIST,NLIST,INDEXL,IXTG,NIXTG,NUMELTG,
     .       MESS,IX1,IX2,INDEX,0)

      NLIST=0
C---------------------------------------------------------------------
C    READING /ADMESH/STAT/SH3N : 3-NODE SHELLS => Storing in SH3TREE
C---------------------------------------------------------------------

      CALL HM_OPTION_START('/ADMESH/STATE')

      DO N =1,NADMESHSTAT
        TITR = ''
C
C--------* EXTRACT DATAS OF /ADMESH/... LINE *------
C
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)

        IF(KEY2(1:LEN_TRIM(KEY2))=='SH3N')THEN

C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NSH3N',NSH3N,IS_AVAILABLE,LSUBMODEL)

          DO I=1,NSH3N

              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID1',ID1,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID2',ID2,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID3',ID3,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('sh3n_ID4',ID4,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('Actlev',LEVEL,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('IMapping',ITRIM,I,IS_AVAILABLE,LSUBMODEL)


              NLIST=NLIST+1
              II=LIST(NLIST)
              NLIST=NLIST+1
              I1=LIST(NLIST)
              NLIST=NLIST+1
              I2=LIST(NLIST)
              NLIST=NLIST+1
              I3=LIST(NLIST)
              NLIST=NLIST+1
              I4=LIST(NLIST)
C
              IF(I1+I2+I3+I4 /=0 .AND.
     .           (I2-I1/=1 .OR. I3-I1 /= 2 .OR. I4-I1 /= 3))THEN
            CALL ANCMSG(MSGID=655,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID1,
     .                  I2=ID2,
     .                  I3=ID3,
     .                  I4=ID4,
     .                  I5=ID)
              END IF
C
              IF(I1+I2+I3+I4 /=0) THEN
                ERRORADJ =0 
                IF(IXTG(2,II) /= IXTG(2,I1).OR.IXTG(3,II) /= IXTG(3,I2)
     .    .OR.IXTG(4,II) /= IXTG(4,I3)) THEN
                 ERRORADJ =1                 
                ELSEIF(IXTG(3,I1) /= IXTG(2,I2).OR.IXTG(4,I2) /= IXTG(3,I3)
     .    .OR.IXTG(2,I3) /= IXTG(3,I4)) THEN
                 ERRORADJ =1 
                ELSEIF(IXTG(4,I1) /= IXTG(2,I3).OR.IXTG(4,I1) /= IXTG(3,I4)
     .    .OR.IXTG(4,I2) /= IXTG(2,I4).OR.IXTG(4,I4) /= IXTG(2,I2).OR.
     .           IXTG(4,I4) /= IXTG(3,I1)) THEN
                 ERRORADJ =1 
                ENDIF
              ENDIF
              IF(ERRORADJ ==1.AND.ABS(LEVEL)<LEVELMAX) THEN 
                CALL ANCMSG(MSGID=1023,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID1,
     .                  I2=ID2,
     .                  I3=ID3,
     .                  I4=ID4,
     .                  I5=ID)
              END IF
C
              IF(LEVEL<-LEVELMAX-1.OR.LEVEL>LEVELMAX)THEN
            CALL ANCMSG(MSGID=657,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID)
              END IF
              SH3TREE(2,II)=I1
              SH3TREE(3,II)=LEVEL
              IF(I1/=0)THEN
                SH3TREE(1,I1)=II
                SH3TREE(1,I2)=II
                SH3TREE(1,I3)=II
                SH3TREE(1,I4)=II
              END IF
              SH3TRIM(II)=ITRIM
           ENDDO

        ENDIF

      ENDDO

      DEALLOCATE(LIST)
      DEALLOCATE(INDEXL)

      RETURN
      END
