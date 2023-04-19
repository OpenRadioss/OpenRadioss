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
Chd|  HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SOLTOSPHX4                    source/elements/sph/soltosph.F
Chd|        SOLTOSPHX8                    source/elements/sph/soltosph.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SPHCEL(ITAB    ,ITABM1  ,IPART ,
     2                  IPARTSP ,IPM     ,IGEO  ,KXSP    ,IXSP    ,
     3                  NOD2SP, RESERVEP ,IXS   ,IPARTS  ,ISOLNOD ,
     4                  SPH2SOL ,SOL2SPH ,IRST  ,X       ,SOL2SPH_TYP,
     5                  LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /SPHCEL ELEMENTS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IXQ             /QUAD ARRAY : CONNECTIVITY, ID, MID PID
C     ITAB            USER ID OF NODES         
C     ITABM1          REVERSE TAB ITAB
C     IPART           PART ARRAY      
C     IPARTSP          INTERNAL PART ID OF A GIVEN QUAD (INTERNAL ID)   
C     IPM             MATERIAL ARRAY (INTEGER) 
C     IGEO            PROP ARRAY (INTEGER) 
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C     KXSP(1,*)              :INUTILISE
C     KXSP(2,*)              :NG     :   +/- NO DU GROUPE
C     KXSP(3,*)              :IPRC   :   NO SYSTEME DU NOEUD ASSOCIE
C     KXSP(4,*)              :NVOIS  :   NOMBRE DE VOISINS.
C     KXSP(5,*)              :NVOIS  :   NOMBRE DE CANDIDATS RETENUS PAR LE BUCKET.
C     KXSP(6,*)              :NVOISS :   NOMBRE DE VOISINS DANS LA PARTIE SYMETRIQUE.
C     KXSP(7,*)              :NVOISS :   NOMBRE DE CANDIDATS RETENUS DANS LA PARTIE SYMETRIQUE.
C     KXSP(NISP,*)           :ID     :   ID DE LA CELLULE.
C-----------------------------------------------
C     IXSP(1:KVOISPH,*)      :IVOIS  :   NOS DES VOISINS. 
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),
     .        NOD2SP(*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        ITAB(*),ITABM1(*),IPART(LIPART1,*),IPARTSP(*),
     .        RESERVEP(NBPARTINLET), IXS(NIXS,*), IPARTS(*), ISOLNOD(*),
     .        SPH2SOL(*), SOL2SPH(2,*), IRST(3,NSPHSOL),SOL2SPH_TYP(*)
      my_real
     .        X(3,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,ID,IDS,K,
     .        MID,PID,IPRT,IPIDS,NSPHDIR,
     .        NSPHCEL,NCELL,IDNOD,INOD,IDMAX,KSPHRES,
     .        NBP,ITAG(NUMNOD),IT,NT,NP,NN,ITOPO,STAT,
     .        INDEX_PART,UID,IFLAGUNIT
C     REAL
      my_real
     .       BID
      CHARACTER MESS*40,KEY*ncharkey
      LOGICAL IS_AVAILABLE

      LOGICAL :: CHECK_LAW
      INTEGER :: MID_SPH,MID_SOL
      INTEGER :: LAW_SPH,LAW_SOL
      INTEGER :: ERROR_NUM
      INTEGER :: I1,I2,I3,I4,I5
      CHARACTER(len=nchartitle) :: C1
      CHARACTER(len=nchartitle) :: TITR
      INTEGER :: USER_PART_SPH,USER_PART_SOL
      INTEGER :: USER_MID_SPH,USER_MID_SOL
      LOGICAL, DIMENSION(NPART) :: TAG_PART
      INTEGER, DIMENSION(NPART) :: PART_ID_SPH,PART_ID_SOL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_SPH
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
      DATA MESS /'SPH CONNECTIVITIES DEFINITION           '/
C--------------------------------------------------
C      ALLOCS & INITS
c      use NUMELQ IN PLACE OF NUMELC ( NBADMESH routine is modifying NUMELC )
C--------------------------------------------------
      ALLOCATE (SUB_SPH(NUMSPH),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUB_SPH')
      SUB_SPH(1:NUMSPH) = 0
      INDEX_PART = 1
      UID = -1 

C--------------------------------------------------
C      READING QUADS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_SPHCEL_READ(KXSP,NISP,IPARTSP,SUB_SPH)
C--------------------------------------------------
      CALL HM_OPTION_COUNT('/SPHCEL',NSPHCEL)
      NCELL=0
      IDMAX=0
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C--------------------------------------------------
      DO I=1,NSPHCEL
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPART(4,INDEX_PART) /= IPARTSP(I) )THEN  
          DO J=1,NPART                            
            IF(IPART(4,J)== IPARTSP(I) ) INDEX_PART = J 
          ENDDO  
        ENDIF
        IF(IPART(4,INDEX_PART) /= IPARTSP(I)) THEN
          CALL ANCMSG(MSGID=402,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1="SPHCEL",
     .                I1=IPARTSP(I),
     .                I2=IPARTSP(I),
     .                PRMOD=MSG_CUMU)
        ENDIF 
        IDNOD = KXSP(3,I)
        INOD=USR2SYS(IDNOD,ITABM1,MESS,ID)
        KXSP(3,I)=INOD
        NCELL=NCELL+1
        IPARTSP(NCELL)=INDEX_PART
        NOD2SP(INOD) =NCELL
C         meme identifiant que le noeud.
        KXSP(NISP,NCELL)=IDNOD
        IDMAX=MAX(IDMAX,IDNOD)
C--------------------------------------------------
        IF (KXSP(NISP,I)>ID_LIMIT)THEN
            CALL ANCMSG(MSGID=509,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=KXSP(NISP,I),C1=LINE,C2='/SPHCEL')
        ENDIF
      ENDDO
C-------------------------------------
       IF(ALLOCATED(SUB_SPH)) DEALLOCATE(SUB_SPH)
C-------------------------------------
       FIRST_SPHRES=NCELL+1
       IF(NSPHRES/=0)THEN
        KCUR   =KSPHOPT
        IREC   =KOPTAD(KCUR)-1
        INOD   =ISPHRES
        NBP = 1

        CALL HM_OPTION_START('/SPH/RESERVE')
        DO N=1,NBPARTINLET 
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID,
     .                         KEYWORD2 = KEY)
          IDS=0
          DO J=1,NPART
            IF(IPART(4,J)==ID) THEN
             IF(IGEO(11,IPART(2,J))/=34)THEN
               CALL ANCMSG(MSGID=1068,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     I2=ID)
             ELSE
               IDS=J
             END IF
             GOTO 175
            ENDIF
          ENDDO
          CALL ANCMSG(MSGID=441,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                I2=ID)
175       CONTINUE
          CALL HM_GET_INTV('Np',KSPHRES,IS_AVAILABLE,LSUBMODEL)
C if SPH part not found reserve is emptied to prevent additional errors 
          IF (IDS==0) THEN
            NSPHRES = NSPHRES - KSPHRES*NSPMD
            NUMSPH = NUMSPH - KSPHRES*NSPMD
            KSPHRES = 0
          ENDIF 
c store nb of reserve for this part  
          RESERVEP(NBP)=KSPHRES
          NBP=NBP+1
c KSPHRES by proc  
          KSPHRES = KSPHRES*NSPMD
          DO J=1,KSPHRES
            NCELL=NCELL+1
            IPARTSP(NCELL)=IDS
            INOD =INOD+1
            KXSP(3,NCELL) =INOD
            NOD2SP(INOD)  =NCELL
            KXSP(2,NCELL)=-1
            IDMAX=IDMAX+1
            KXSP(NISP,NCELL)=IDMAX
          ENDDO
        ENDDO
       ENDIF
C-------------------------------------
       FIRST_SPHSOL=NCELL+1
       IF(NSPHSOL/=0)THEN
C
        KCONTACT=1
C
        INOD   =FIRSTNOD_SPHSOL-1
        DO N=1,NUMELS8
          SOL2SPH(1,N)=0
          SOL2SPH(2,N)=0
          IPIDS  =IPART(2,IPARTS(N))
          NSPHDIR=IGEO(37,IPIDS)
          IDS    =IGEO(38,IPIDS)
          IF(NSPHDIR/=0)THEN
            IF(ISOLNOD(N)==8)THEN
              DO J=1,8
                ITAG(IXS(1+J,N))=0
              END DO
              NN=0
              DO J=1,8
                IF(ITAG(IXS(1+J,N))==0)THEN
                   NN=NN+1
                   ITAG(IXS(1+J,N))=1
                 END IF
              END DO
              IF(NN==4)THEN
                ITOPO=4
                NP=0
                NT=0
                DO IT=1,NSPHDIR
                  NT=NT+IT
                  NP=NP+NT
                END DO
              ELSE
                ITOPO=8
                NP=NSPHDIR*NSPHDIR*NSPHDIR
              END IF
            ELSEIF(ISOLNOD(N)==6)THEN
              ITOPO=6
              NP=0
              NT=0
              DO IT=1,NSPHDIR
                NT=NT+IT
              END DO
              NP=NP+NSPHDIR*NT
            ELSEIF(ISOLNOD(N)==4)THEN
              ITOPO=4
              NP=0
              NT=0
              DO IT=1,NSPHDIR
                NT=NT+IT
                NP=NP+NT
              END DO
            END IF
C---
C           SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
            SOL2SPH(1,N)=NCELL
            SOL2SPH(2,N)=NCELL+NP
            SOL2SPH_TYP(N)=ITOPO
            DO I=1,NP
              SPH2SOL(NCELL+I)=N
            END DO
C---
            IF(ITOPO==4)THEN
C-- Tetra
              CALL SOLTOSPHX4(NSPHDIR,NCELL   ,INOD    ,IDS    ,IDMAX   ,
     .                       X      ,IXS(1,N),KXSP    ,IPARTSP,NOD2SP  ,
     .                       IRST   )
            ELSEIF (ITOPO==8) THEN
C-- Hexa + degenerated penta6
              CALL SOLTOSPHX8(NSPHDIR,NCELL   ,INOD    ,IDS    ,IDMAX   ,
     .                       X      ,IXS(1,N),KXSP    ,IPARTSP,NOD2SP  ,
     .                       IRST   )
            ENDIF
          ENDIF
        ENDDO
       ENDIF
! ------------------------------------
!   check the material consistency between
!   the solide and the sph (SOL2PSH)
       CHECK_LAW = .FALSE.
       ERROR_NUM = 0
       TAG_PART(1:NPART) = .FALSE.
       IF(NSPHSOL/=0) THEN
            DO I =1,NUMSPH
                N = SPH2SOL(I) 
                IF(N/=0) THEN
                    MID_SPH = IPART(1,IPARTSP(I))
                    MID_SOL = IPART(1,IPARTS(N))            
                    LAW_SPH = IPM(2,MID_SPH)
                    LAW_SOL = IPM(2,MID_SOL)                                 
                    IF(LAW_SPH/=LAW_SOL) THEN
                        CHECK_LAW = .TRUE.
                        IF( .NOT.TAG_PART(IPARTSP(I)) ) THEN
                            ERROR_NUM = ERROR_NUM + 1
                            TAG_PART(IPARTSP(I)) = .TRUE.
                            PART_ID_SPH(ERROR_NUM) = IPARTSP(I)
                            PART_ID_SOL(ERROR_NUM) = IPARTS(N)
                        ENDIF
                    ENDIF
                ENDIF
            ENDDO
       ENDIF
    
       IF(CHECK_LAW) THEN
            DO I=1,ERROR_NUM
                TITR(1:nchartitle) =''
                CALL FRETITL2(TITR,IPART(LIPART1-LTITR+1,PART_ID_SPH(I)),LTITR-1)
                USER_PART_SPH = IPART(4,PART_ID_SPH(I))
                USER_PART_SOL = IPART(4,PART_ID_SOL(I))
                USER_MID_SPH = IPART(5,PART_ID_SPH(I))
                USER_MID_SOL = IPART(5,PART_ID_SOL(I))
                CALL ANCMSG(MSGID=1911,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=USER_PART_SPH,C1=titr(1:len_trim(titr)),
     .                     I2=USER_MID_SPH,I3=USER_PART_SPH,
     .                     I4=USER_MID_SOL,I5=USER_PART_SOL )
            ENDDO
       ENDIF
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(KXSP(NISP,1),NISP,NUMSPH,MESS,0,BID)
C-------------------------------------
C Print
C-------------------------------------
      I1=1
      I2=MIN0(50,NUMSPH)
C
   90 WRITE (IOUT,300)
      DO 100 I=I1,I2
       IPRT=IPARTSP(I)
       MID =IPM(1,IPART(1,IPRT))
       PID =IGEO(1,IPART(2,IPRT))
       WRITE (IOUT,'(6(I10,1X))') I,KXSP(NISP,I),MID,PID,
     .                           KXSP(3,I),ITAB(KXSP(3,I))
  100 CONTINUE
      IF(I2==NUMSPH)GOTO 200
      I1=I1+50
      I2=MIN0(I2+50,NUMSPH)
      GOTO 90
C
 200  CONTINUE
      WRITE (IOUT,'(A)') 'END OF CELL TRACEBACK'
C
 300  FORMAT(/' SPH CELLS             '/
     +        ' ----------------------'/
     + '   LOC-CEL    GLO-CEL      MATER ',
     + '      GEOM    LOC-NOD    GLO-NOD ')
      RETURN
C-------------------------------------
      END
