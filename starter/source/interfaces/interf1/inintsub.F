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
Chd|  ININTSUB                      source/interfaces/interf1/inintsub.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ININTSUB_11                   source/output/subinterface/inintsub_11.F
Chd|        ININTSUB_25                   source/output/subinterface/inintsub_25.F
Chd|        ININTSUB_7                    source/output/subinterface/inintsub_7.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTSUB(ITAB      ,IGRNOD ,IGRSURF ,
     .                    IPARI     ,MAXRTM ,NOM_OPT ,
     .                    INTBUF_TAB,MAXRTMS,IGRSLIN )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),IPARI(NPARI,*)
      INTEGER MAXRTM,MAXRTMS
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: NOD2NSV,NOD2RTM,NOD2RTMS,NOD2RTMM,KAD,TAGNOD,TAGRTM
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGLINS,TAGLINM
      TARGET NOD2NSV
       INTEGER, DIMENSION(:), POINTER :: IADD
      INTEGER IOK(NINTER)
      INTEGER I,J,K,JGRN,ISU,ISU1,ISU2,
     .   NI,NOINT,NTY,NRTS,NRTM,NSN,NMN,MULTIMP,IFQ,NRTM_SH,NRTM0,
     .   NISUB, NISUBS, NISUBM, JSUB, KSUB, NNE, IS, ISV, CUR,
     .   NEXT, IM, KM, JAD, IN, II, N,STAT,K1,K2,NT19,INOD
      CHARACTER MESS*40
      INTEGER ID,ID1
      CHARACTER*nchartitle,
     .   TITR,TITR1
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA MESS/'SUB-INTERFACES FOR TH INITIALIZATIONS   '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER BITSET
      EXTERNAL BITSET
C=======================================================================
      ALLOCATE (NOD2NSV(NUMNOD+1)      ,STAT=stat)
      ALLOCATE (NOD2RTM(4*MAXRTM)      ,STAT=stat)
      ALLOCATE (NOD2RTMS(2*MAXRTMS)      ,STAT=stat)
      ALLOCATE (NOD2RTMM(2*MAXRTMS)      ,STAT=stat)
      ALLOCATE (KAD(MAX(NUMNOD,MAXRTM,MAXRTMS)),STAT=stat)
      ALLOCATE (TAGNOD(NUMNOD)      ,STAT=stat)
      ALLOCATE (TAGRTM(MAXRTM)      ,STAT=stat)
      ALLOCATE (TAGLINS(MAXRTMS)      ,STAT=stat)
      ALLOCATE (TAGLINM(MAXRTMS)      ,STAT=stat)
C
      IADD => NOD2NSV(1:NUMNOD+1)
      DO NI=1,NINTER
        NTY  =IPARI(7,NI)
        NOINT=IPARI(15,NI)
        NT19 =IPARI(71,NI)
        ID=NOM_OPT(1,NI)
C
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C
C------------------------------------------
C         Interface type 25
C------------------------------------------
        IF (NTY==25)THEN
C
            NRTS   =IPARI(3,NI)
            NRTM   =IPARI(4,NI)
            NSN    =IPARI(5,NI)
            NMN    =IPARI(6,NI)
            MULTIMP=IPARI(23,NI)
            IFQ    =IPARI(31,NI)
            NISUB  =IPARI(36,NI)
            NISUBS =IPARI(37,NI)
            NISUBM =IPARI(38,NI)
            NRTM_SH=IPARI(42,NI)
            NRTM0  =NRTM-NRTM_SH
C
            IF(NISUB/=0)THEN
               CALL ININTSUB_25(ITAB      ,IGRNOD ,IGRSURF ,NOM_OPT ,INTBUF_TAB,
     .                        NRTM      ,NRTM0  ,NSN     ,NISUBS  ,NISUBM    ,
     .                        NOINT     ,NI     ,NOD2NSV ,NOD2RTM ,KAD       ,
     .                        TAGNOD    ,TAGRTM ,IADD    )
            END IF
C
C--
C
C------------------------------------------
C         Interface type 7, 24
C------------------------------------------
        ELSEIF (NTY==7.OR.NTY==10.OR.NTY==22.OR.
     .          NTY==24)THEN
C
            NRTS   =IPARI(3,NI)
            NRTM   =IPARI(4,NI)
            NSN    =IPARI(5,NI)
            NMN    =IPARI(6,NI)
            MULTIMP=IPARI(23,NI)
            IFQ    =IPARI(31,NI)
            NISUB  =IPARI(36,NI)
            NISUBS =IPARI(37,NI)
            NISUBM =IPARI(38,NI)
            IF(NTY == 24 ) THEN
               NRTM_SH=IPARI(42,NI)
               NRTM0  =NRTM-NRTM_SH
            ELSE
               NRTM0  =NRTM
            ENDIF
C
            IF(NISUB/=0)THEN
C
              CALL ININTSUB_7 (ITAB      ,IGRNOD ,IGRSURF ,NOM_OPT ,INTBUF_TAB,
     .                         NRTM      ,NRTM0  ,NSN     ,NISUBS  ,NISUBM    ,
     .                         NOINT     ,NI     ,NOD2NSV ,NOD2RTM ,KAD       ,
     .                         TAGNOD    ,TAGRTM ,IADD    ,NT19    )
C
            END IF
C
C------------------------------------------
C         Interface type 11
C------------------------------------------
        ELSEIF (NTY==11) THEN
C   interface type11 - subinterface input by lines
C
          NRTS   =IPARI(3,NI)
          NRTM   =IPARI(4,NI)
          NSN    =IPARI(5,NI)
          NMN    =IPARI(6,NI)
          MULTIMP=IPARI(23,NI)
          IFQ    =IPARI(31,NI)
          NISUB  =IPARI(36,NI)
          NISUBS =IPARI(37,NI)
          NISUBM =IPARI(38,NI)
C
          IF(NISUB/=0)THEN

            CALL ININTSUB_11 (ITAB      ,IGRSLIN ,IGRSURF ,NOM_OPT ,INTBUF_TAB,
     .                       NRTM      ,NRTM0  ,NSN     ,NISUBS   ,NISUBM    ,
     .                       NOINT     ,NI     ,NOD2RTMS,NOD2RTMM ,KAD       ,
     .                       TAGLINS   ,TAGLINM,IADD    ,NT19     ,MAXRTMS   ,
     .                       NRTS      ,NTY    )

          ENDIF
C
        END IF
      END DO
C-------------------------------------
      IF(IPRI<6) RETURN
C
      WRITE(IOUT,1000)
      DO NI=1,NINTER
        NTY = 0
        IF (IPARI(71,NI)==0) THEN
          NTY  =IPARI(7,NI)
        ELSEIF (IPARI(71,NI)==-1) THEN
          NTY  = 19
        ENDIF
C
        NOINT=IPARI(15,NI)
        IF (NTY==7.OR.NTY==10.OR.NTY==22.OR.
     .      NTY==24.OR.NTY==25)THEN
C
          NRTS   =IPARI(3,NI)
          NRTM   =IPARI(4,NI)
          NSN    =IPARI(5,NI)
          NMN    =IPARI(6,NI)
          MULTIMP=IPARI(23,NI)
          IFQ    =IPARI(31,NI)
          NISUB  =IPARI(36,NI)
          NISUBS =IPARI(37,NI)
          NISUBM =IPARI(38,NI)
          IF(NISUB/=0)THEN
C
C           SUR LES ENTIERS ON ECONOMISE +- 2NSN+2NMN+NST / AUTRES TYPES
C           K14 = ELEMS CANDIDATS A L'IMPACT...
C           K15 = NOEUDS CANDIDATS A L'IMPACT + ADRESSE ELEMS CORRES.
C
            WRITE(IOUT,1010)NOINT
            WRITE(IOUT,'(10I10)')
     .           (NOM_OPT(1,NINTER+INTBUF_TAB(NI)%LISUB(JSUB)),JSUB=1,NISUB)
            WRITE(IOUT,1030)
            DO IS=1,NSN
              JSUB=INTBUF_TAB(NI)%ADDSUBS(IS)
              N   =INTBUF_TAB(NI)%ADDSUBS(IS+1)-INTBUF_TAB(NI)%ADDSUBS(IS)
              IF(N>0)THEN
                WRITE(IOUT,'(2I10)')IS,ITAB(INTBUF_TAB(NI)%NSV(IS))
                WRITE(IOUT,'(20X,8I10)')
     .            (INTBUF_TAB(NI)%LISUBS(JSUB-1+K),K=1,N)
              END IF
            END DO
            WRITE(IOUT,1040)
            DO IM=1,NRTM
              JSUB=INTBUF_TAB(NI)%ADDSUBM(IM)
              N   =INTBUF_TAB(NI)%ADDSUBM(IM+1)-INTBUF_TAB(NI)%ADDSUBM(IM)
              IF(N>0)THEN
                WRITE(IOUT,'(5I10)')IM,
     .            (ITAB(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+J)),J=1,4)
                WRITE(IOUT,'(50X,5I10)')
     .           (INTBUF_TAB(NI)%LISUBM(JSUB-1+K),K=1,N)
              END IF
            END DO
          END IF
C
        ELSEIF (NTY==11)THEN
C
          NRTS   =IPARI(3,NI)
          NRTM   =IPARI(4,NI)
          NSN    =IPARI(5,NI)
          NMN    =IPARI(6,NI)
          MULTIMP=IPARI(23,NI)
          IFQ    =IPARI(31,NI)
          NISUB  =IPARI(36,NI)
          NISUBS =IPARI(37,NI)
          NISUBM =IPARI(38,NI)
          IF(NISUB/=0)THEN
C
            WRITE(IOUT,1010)NOINT
            WRITE(IOUT,'(10I10)')
     .           (NOM_OPT(1,NINTER+INTBUF_TAB(NI)%LISUB(JSUB)),JSUB=1,NISUB)
            WRITE(IOUT,1050)
            DO IS=1,NRTS
              JSUB=INTBUF_TAB(NI)%ADDSUBS(IS)
              N   =INTBUF_TAB(NI)%ADDSUBS(IS+1)-INTBUF_TAB(NI)%ADDSUBS(IS)
              IF(N>0)THEN
                WRITE(IOUT,'(5I10)')IS,
     .            (ITAB(INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)),J=1,2)
                WRITE(IOUT,'(50X,5I10)')
     .           (INTBUF_TAB(NI)%LISUBS(JSUB-1+K),K=1,N)
              END IF
            END DO
            WRITE(IOUT,1060)
            DO IM=1,NRTM
              JSUB=INTBUF_TAB(NI)%ADDSUBM(IM)
              N   =INTBUF_TAB(NI)%ADDSUBM(IM+1)-INTBUF_TAB(NI)%ADDSUBM(IM)
              IF(N>0)THEN
                WRITE(IOUT,'(5I10)')IM,
     .            (ITAB(INTBUF_TAB(NI)%IRECTM(2*(IM-1)+J)),J=1,2)
                WRITE(IOUT,'(50X,5I10)')
     .           (INTBUF_TAB(NI)%LISUBM(JSUB-1+K),K=1,N)
              END IF
            END DO
          END IF
C
        ELSEIF (NTY==19)THEN
C
          NRTS   =IPARI(3,NI)
          NRTM   =IPARI(4,NI)
          NSN    =IPARI(5,NI)
          NMN    =IPARI(6,NI)
          MULTIMP=IPARI(23,NI)
          IFQ    =IPARI(31,NI)
          NISUB  =IPARI(36,NI)
          NISUBS =IPARI(37,NI)
          NISUBM =IPARI(38,NI)
          IF(NISUB/=0)THEN
C
            WRITE(IOUT,1010)NOINT
            WRITE(IOUT,'(10I10)')
     .           (NOM_OPT(1,NINTER+INTBUF_TAB(NI)%LISUB(JSUB)),JSUB=1,NISUB)
C
            WRITE(IOUT,1030)
            DO IS=1,NSN
              JSUB=INTBUF_TAB(NI)%ADDSUBS(IS)
              N   =INTBUF_TAB(NI)%ADDSUBS(IS+1)-INTBUF_TAB(NI)%ADDSUBS(IS)
              IF(N>0)THEN
                WRITE(IOUT,'(2I10)')IS,ITAB(INTBUF_TAB(NI)%NSV(IS))
                WRITE(IOUT,'(20X,8I10)')
     .            (INTBUF_TAB(NI)%LISUBS(JSUB-1+K),K=1,N)
              END IF
            END DO
            WRITE(IOUT,1040)
            DO IM=1,NRTM
              JSUB=INTBUF_TAB(NI)%ADDSUBM(IM)
              N   =INTBUF_TAB(NI)%ADDSUBM(IM+1)-INTBUF_TAB(NI)%ADDSUBM(IM)
              IF(N>0)THEN
                WRITE(IOUT,'(5I10)')IM,
     .            (ITAB(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+J)),J=1,4)
                WRITE(IOUT,'(50X,5I10)')
     .           (INTBUF_TAB(NI)%LISUBM(JSUB-1+K),K=1,N)
              END IF
            END DO
          END IF
C
        END IF
C
      END DO
C
      DEALLOCATE (KAD)
      DEALLOCATE (NOD2RTM)
      DEALLOCATE (NOD2RTMS)
      DEALLOCATE (NOD2NSV)
      DEALLOCATE (TAGRTM)
      DEALLOCATE (TAGNOD)
      DEALLOCATE (TAGLINS,TAGLINM)
C 
C-------------------------------------
 1000 FORMAT(    /1X,'   STRUCTURE OF SUB-INTERFACES OUTPUT TO TH'/
     .            1X,'   ----------------------------------------'// )
 1010 FORMAT(    /1X,'   INTERFACE ID . . . . . . . . . . . . . .',I10/,
     .               '    -> LIST OF SUB-INTERFACES IDS :        ')
 1030 FORMAT(/,'  SECONDARY     SECONDARY   '/
     .         '  NODE      NODE    '/
     .         '  NUMBER      ID    '/
     .         '                    ',
     .       ' -> LIST OF SUB-INTERFACES (LOCAL NUMBERS IN INTERFACE)'/)
 1040 FORMAT('  MAIN    MAIN  '/
     .       ' SEGMENT   SEGMENT  '/
     .       '  NUMBER    NODES   '/
     .       '                    ',
     .       ' -> LIST OF SUB-INTERFACES (LOCAL NUMBERS IN INTERFACE)'/)
C
 1050 FORMAT('  SECONDARY     SECONDARY   '/
     .       '   LINE      LINE   '/
     .       '  NUMBER    NODES   '/
     .       '                    ',
     .       ' -> LIST OF SUB-INTERFACES (LOCAL NUMBERS IN INTERFACE)'/)
 1060 FORMAT('  MAIN    MAIN  '/
     .       '   LINE      LINE   '/
     .       '  NUMBER    NODES   '/
     .       '                    ',
     .       ' -> LIST OF SUB-INTERFACES (LOCAL NUMBERS IN INTERFACE)'/)
C-------------------------------------
      RETURN
      END
