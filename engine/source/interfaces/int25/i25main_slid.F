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
Chd|  I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ALLOCATE_COMM_STRUCT          share/modules/mpi_comm_mod.F  
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DEALLOCATE_COMM_STRUCT        share/modules/mpi_comm_mod.F  
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|        I25PREP_ADD                   source/interfaces/int25/i25slid.F
Chd|        I25PREP_NINDEX                source/interfaces/int25/i25slid.F
Chd|        I25PREP_SEND                  source/interfaces/int25/i25slid.F
Chd|        I25PREP_SIZBUFS               source/interfaces/int25/i25slid.F
Chd|        I25PREP_SLID_2                source/interfaces/int25/i25slid.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_I25_SLIDE_EXCH           source/mpi/interfaces/spmd_i25slide.F
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|        UPGRADE_CAND_OPT              ../common_source/interf/upgrade_multimp.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25MAIN_SLID(
     1       IPARI    ,IAD_ELEM ,FR_ELEM  ,ITAB     ,SENSOR_TAB,
     2       INTLIST25,INTBUF_TAB ,IAD_FRNOR,FR_NOR ,
     3       X        ,V       ,MS      ,TEMP   ,KINET   ,
     4       NODNX_SMS,JTASK   ,NB_DST2, MAIN_PROC,
     5       NEWFRONT ,ISENDTO ,IRCVFROM ,NBINTC,
     6       INTLIST ,ISLEN7   ,IRLEN7  ,IRLEN7T  ,ISLEN7T,
     7       NB_DST1 ,H3D_DATA, 
     8       ICODT,
     9       ISKEW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE MESSAGE_MOD
      USE TRI7BOX
      USE INTBUFDEF_MOD  
      USE MPI_COMMOD  
      USE H3D_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*), INTLIST25(*), JTASK,
     .        IAD_ELEM(2,*) ,FR_ELEM(*), IAD_FRNOR(NINTER25,NSPMD+1), FR_NOR(*),
     .        KINET(*), NODNX_SMS(*), NB_DST1(PARASIZ), NB_DST2(PARASIZ)
      INTEGER, INTENT(IN) :: ICODT(*),ISKEW(*)
      my_real :: X(3,*), V(3,*), MS(*), TEMP(*)
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
      INTEGER MAIN_PROC(NUMNOD)
      INTEGER NBINTC,ISLEN7,IRLEN7,
     .        IRLEN7T,ISLEN7T,
     .        NEWFRONT(*), INTLIST(*),
     .        ISENDTO(NINTER+1,*) ,IRCVFROM(NINTER+1,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, NIN, NI25, ISENS, LENT25, IERROR, ITYP,
     .        IFQ, IGAP, INTTH, ILEV, IVIS2,
     .        I_STOK_GLO, I_STOK, I,J, LINDMAX, NSNR, INACTI, NADMSR,
     .        LENADD, MG, L, N, N_OLD_IMPACT,
     .        P, RSIZ(NINTER25), ISIZ(NINTER25), SIZBUFS(NSPMD), IADBUFR(NSPMD+1),
     .        NADMAX, LADMAX, NSLIDMX, NSENDTOT, NSNF, NSNL, NSNRF, NSNRL,INTFRIC ,
     .        FLAGREMN, LREMNORMAX, ISTIF_MSDT
      INTEGER SIZOPT, K_STOK, I_OPT_STOK
      my_real
     .       TS, STARTT, STOPT
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2, BUFR, NADD, KADD,
     .                                      NSLIDE, FR_SLIDE, INDXTOSEND
C
      TYPE(real_pointer),    DIMENSION(NSPMD,NINTER25) :: RBUFS,RBUFR
      TYPE(int_pointer) ,    DIMENSION(NSPMD,NINTER25) :: IBUFS,IBUFR  

C     rassembler les trois structures + COMM_PATTERN 
      TYPE(MPI_COMM_STRUCT) :: COMM_INT
      TYPE(MPI_COMM_STRUCT) :: COMM_REAL
      TYPE(MPI_COMM_STRUCT) :: COMM_SIZ

      INTEGER COMM_PATTERN(NSPMD,NINTER25)  !COMM_PATTERN(K,NIN) = 1 <=> ISPMD and K have to communicate
      INTEGER SIZBUFS_GLOB(NSPMD,NINTER25)  !number of nodes to send per proc per interf
      INTEGER SIZBUFR_GLOB(NSPMD,NINTER25)  !number of nodes to recv per proc per interf
      INTEGER NB_TOT
      INTEGER :: NRTM, NSN
C-----------------------------------------------




      DO NI25=1,NINTER25
C
        NIN = INTLIST25(NI25)
        NSN   =IPARI(5,NIN)
C
C       Initialisation
        NSNF = NSN*(JTASK-1) / NTHREAD
        NSNL = NSN*JTASK / NTHREAD
C
        INTBUF_TAB(NIN)%ISLIDE(4*NSNF+1:4*NSNL)=0

      END DO
C
      IF(NSPMD > 1)THEN
        DO NI25=1,NINTER25
          NIN = INTLIST25(NI25)
          NSNR  =IPARI(24,NIN)
          NSNRF = 1 + NSNR*(JTASK-1) / NTHREAD
          NSNRL = NSNR*JTASK / NTHREAD
          ISLIDE_FI(NIN)%P(1:4,NSNRF:NSNRL)=0
        END DO
      END IF
C
      CALL MY_BARRIER
C
C-----------------------------------------------------------------------
C       Secnd node leaving its previous impacted segment ...
C-----------------------------------------------------------------------
      DO NI25=1,NINTER25
C
        NIN = INTLIST25(NI25)

        IPARI(29,NIN) = 0
C
        CALL I25COMP_1(
     1    IPARI   ,INTBUF_TAB(NIN),X   ,ITAB      ,NIN       ,
     2    KINET     ,JTASK    ,NB_DST1(JTASK),V   ,SENSOR_TAB)
      ENDDO
C
      CALL MY_BARRIER
C
C-----------------------------------------------------------------------
!$OMP SINGLE
        CALL ALLOCATE_COMM_STRUCT(COMM_INT,NSPMD,NINTER25) 
        CALL ALLOCATE_COMM_STRUCT(COMM_REAL,NSPMD,NINTER25) 
        CALL ALLOCATE_COMM_STRUCT(COMM_SIZ,NSPMD,NINTER25) 

C
        NSLIDMX=0
        NADMAX =0

        SIZBUFR_GLOB(1:NSPMD,1:NINTER25) = 0
        SIZBUFS_GLOB(1:NSPMD,1:NINTER25) = 0
        ISIZ(1:NINTER25) = 0
        RSIZ(1:NINTER25) = 0


!        : remplir COMM_PATTERN tel que COMM_PATTERN(P,NIN) = 1 => comm entre ISPMD et P
!       pour l interface NIN (symmetrie necessaire entre processeurs)
        COMM_PATTERN(1:NSPMD,1:NINTER25) = 0
        DO NI25=1,NINTER25
C
          NIN = INTLIST25(NI25)
C
          IPARI(29,NIN) = 0
C
          DO P = 1,NSPMD
             LENT25 = IAD_FRNOR(NI25,P+1)-IAD_FRNOR(NI25,P)
             IF(P /= ISPMD +1 .AND. LENT25 /= 0) THEN
                COMM_PATTERN(P,NI25) = 1
             ENDIF
          ENDDO
        ENDDO

        DO NI25=1,NINTER25
          SIZBUFS(1:NSPMD) = 0
C
          NIN = INTLIST25(NI25)
C
          IPARI(29,NIN) = 0
C
C         STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
C         STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
C         IF(STARTT>TT) CYCLE
C         IF(TT>STOPT)  CYCLE
C
C         Look if interface is activated 
C         ISENS = IPARI(64,NIN)  
C         IF(ISENS/=0)  THEN         ! Interface activated by sensor
C            TS = SENSOR_TAB(ISENS)%TSTART
C         ELSE
C            TS = TT
C         ENDIF
C         IF(TT<TS) CYCLE
C
C precalculer LENS
          LENT25 = IAD_FRNOR(NI25,NSPMD+1)-IAD_FRNOR(NI25,1)

          NSN   =IPARI(5,NIN)
          NSNR  =IPARI(24,NIN)
          NADMSR=IPARI(67,NIN)

          NSLIDMX =MAX(NSLIDMX,NSN+NSNR)
          NADMAX  =MAX(NADMAX ,NADMSR)

        END DO

        LENT25 = LENT25 + NSNT25
!       ALLOCATE(BUFR(LENT25),STAT=IERROR)

C-----------------------------------------------------------------------
        IF(NSPMD == 1) THEN

          ALLOCATE(NADD(1),KADD(1))
          ALLOCATE(NSLIDE(1),FR_SLIDE(1),INDXTOSEND(1))

        ELSE
C-----------------------------------------------------------------------
C         SPMD only <=> envoi de nds secnds qui glissent sur un sommet frontire
C-----------------------------------------------------------------------
C
          ALLOCATE(NADD(NADMAX+1),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
C
          LADMAX = 4*NSLIDMX
          ALLOCATE(KADD(LADMAX),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
C
          DO NI25=1,NINTER25
C
             NIN = INTLIST25(NI25)

             IPARI(29,NIN) = 0
C
C ==========================================================================
C            Post Asynchronous reception of the sizes
             CALL SPMD_I25_SLIDE_EXCH(IBUFR    ,RBUFR      ,ISIZ(NI25)     ,RSIZ(NI25) 
     .                            ,SIZBUFR_GLOB,COMM_INT,COMM_REAL,COMM_SIZ
     .                            ,2                ,NI25, COMM_PATTERN)

C ==========================================================================

C
             NSN   =IPARI(5,NIN)
             NSNR  =IPARI(24,NIN)
             INACTI=IPARI(22,NIN)
             NADMSR=IPARI(67,NIN)
             ILEV  =IPARI(20,NIN)
             IGAP  =IPARI(21,NIN)
             IFQ   =IPARI(31,NIN) 
             INTTH =IPARI(47,NIN)
             INTFRIC =IPARI(72,NIN)
             IVIS2 =IPARI(14,NIN)
             ISTIF_MSDT = IPARI(97,NIN)
             ITYP  = 25
C
C            NADD(1:NADMSR+1), KADD <=> liste sky line des noeuds secnd concerns / tous les sommets frontires
             NADD(1:NADMSR+1)=0
             CALL I25PREP_ADD(
     1      NIN    ,NI25     ,NSN          ,NSNR      ,ITAB     ,
     2      NADMSR ,INTBUF_TAB(NIN)%ADMSR  ,IAD_FRNOR ,FR_NOR   ,NADD   ,
     3      KADD   ,INTBUF_TAB(NIN)%ISLIDE) 
C
C           compute size of buffer for send
            CALL I25PREP_NINDEX(
     1        NIN    ,NI25   ,NSN    ,NSNR   ,
     3        ITAB   ,INTBUF_TAB(NIN)%NSV,IAD_FRNOR,FR_NOR ,NADD ,
     4        KADD   ,SIZBUFS,NSENDTOT)
C
            ALLOCATE(FR_SLIDE(4*NSENDTOT),INDXTOSEND(NSENDTOT),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            FR_SLIDE(1:4*NSENDTOT)=0
C
C           compute index of nodes to send to the all domains
            CALL I25PREP_SIZBUFS(
     1        NIN    ,NI25   ,NSN    ,NSNR   ,ITYP     ,
     2        IFQ    ,INACTI ,IGAP   ,INTTH  ,ILEV     ,
     3        ITAB   ,INTBUF_TAB(NIN)%NSV,IAD_FRNOR,FR_NOR ,NADD ,
     4        KADD   ,RSIZ(NI25)    ,ISIZ(NI25),SIZBUFS,FR_SLIDE ,
     5        INDXTOSEND,INTFRIC   , IVIS2   ,ISTIF_MSDT)
C
            DO P=1,NSPMD
              NULLIFY(RBUFS(P,NI25)%P)
              NULLIFY(IBUFS(P,NI25)%P)
              IF(SIZBUFS(P) > 0) THEN 
               ALLOCATE(RBUFS(P,NI25)%P(RSIZ(NI25)*SIZBUFS(P)),STAT=IERROR)
               ALLOCATE(IBUFS(P,NI25)%P(ISIZ(NI25)*SIZBUFS(P)),STAT=IERROR)
               IBUFS(P,NI25)%P(1:ISIZ(NI25)*SIZBUFS(P)) = -1
               RBUFS(P,NI25)%P(1:RSIZ(NI25)*SIZBUFS(P)) = -1
              ENDIF
              SIZBUFS_GLOB(P,NI25)=SIZBUFS(P)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
            END DO

C ==========================================================================
C           Send sizes
            CALL SPMD_I25_SLIDE_EXCH(IBUFS    ,RBUFS      ,ISIZ(NI25)     ,RSIZ(NI25) 
     .                    ,SIZBUFS_GLOB       ,COMM_INT,COMM_REAL,COMM_SIZ
     .                    ,0                  ,NI25, COMM_PATTERN)

C ==========================================================================

            CALL I25PREP_SEND(
     1        NIN    ,NI25     ,NSN      ,NSNR   ,ITYP    ,
     2        IFQ    ,INACTI   ,IGAP     ,INTTH  ,ILEV    ,
     3        ITAB   ,IAD_FRNOR,FR_NOR   ,
     4        LENT25 ,NADD     ,KADD     ,KINET    ,
     5        NODNX_SMS ,X     ,V        ,MS    ,TEMP  ,
     .        INTBUF_TAB(NIN)  ,RBUFS, IBUFS,
     6        RSIZ(NI25),      ISIZ(NI25),  SIZBUFS, FR_SLIDE,INDXTOSEND, 
     7        MAIN_PROC,INTFRIC ,IVIS2  , ICODT  ,ISKEW ,
     8        ISTIF_MSDT) 


C ==========================================================================
C           Send buffers 
            CALL SPMD_I25_SLIDE_EXCH(IBUFS    ,RBUFS      ,ISIZ(NI25)     ,RSIZ(NI25) 
     .                    ,SIZBUFS_GLOB       ,COMM_INT,COMM_REAL,COMM_SIZ
     .                    ,1                  ,NI25, COMM_PATTERN)
C ==========================================================================

            DEALLOCATE(FR_SLIDE)
            DEALLOCATE(INDXTOSEND)


          END DO ! NI25 = 1:NINTER25
         
        END IF ! IF(NSPMD > 1)THEN
C-----------------------------------------------------------------------
C
C       Decompactage
        DO NI25=1,NINTER25
C
          NIN = INTLIST25(NI25)

          IPARI(29,NIN) = 0
C
C         Receive buffer ( + wait for sizes)
          IF(NSPMD > 1) THEN
            CALL SPMD_I25_SLIDE_EXCH(IBUFR    ,RBUFR      ,ISIZ(NI25)     ,RSIZ(NI25) 
     .                    ,SIZBUFR_GLOB       ,COMM_INT,COMM_REAL,COMM_SIZ
     .                    ,3                  ,NI25, COMM_PATTERN)
          ENDIF
C
          NRTM  =IPARI(4,NIN)
          NSN   =IPARI(5,NIN)
          IVIS2 =IPARI(14,NIN)
          NSNR  =IPARI(24,NIN)
          INACTI=IPARI(22,NIN)
          NADMSR=IPARI(67,NIN)
          ILEV  =IPARI(20,NIN)
          IGAP  =IPARI(21,NIN)
          IFQ   =IPARI(31,NIN) 
          INTTH =IPARI(47,NIN)
          INTFRIC =IPARI(72,NIN)
          FLAGREMN =IPARI(63,NIN)
          LREMNORMAX =IPARI(82,NIN)
          ISTIF_MSDT = IPARI(97,NIN)
          ITYP  = 25

C         filtrer => ne garder que les anciens impacts + les candidats au glissement 
          IF(NSPMD > 1) THEN
C           Output : Pour cette interface , liste de nds a ajouter a NSNFI


C ============================================================================
C Deux approches possibles:
C                    1                                2
C    -Wait toutes les procs (synchro)   |  - Wait sur une taille
C    -Allocation de XREM/IREM           |  - Reception dans dans RBUFR/IBUFR 
C    -Reception dans XREM/IREM          |  - Copie de *BUFR -> *REM
C
C  Ici on teste (2)  
            NB_TOT = 0
            DO P =1,NSPMD
               SIZBUFS(P) = SIZBUFR_GLOB(P,NI25)
               NB_TOT = NB_TOT +SIZBUFS(P)
            ENDDO
            IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
            IF(ALLOCATED(IREM)) DEALLOCATE(IREM)
            ALLOCATE(XREM(RSIZ(NI25),NB_TOT))
            ALLOCATE(IREM(ISIZ(NI25),NB_TOT))


            NB_TOT = 0
            DO P = 1,NSPMD
               DO I = 1,SIZBUFS(P)
         !       IF(IREM(3,NB_TOT+1) == ISPMD + 1) THEN 
         !        ! If PMAIN is the current processor: adding to local structures
         !        ! If current proc is PMAIN, then local number is in IREM
         !        ! i.e. no search necessary
                  
         !       ELSE
                   NB_TOT = NB_TOT + 1 
                   DO J =1,RSIZ(NI25)
                     XREM(J,NB_TOT) = RBUFR(P,NI25)%P((I-1)*RSIZ(NI25)+J)
                   ENDDO
                   DO J =1,ISIZ(NI25)
                     IREM(J,NB_TOT) = IBUFR(P,NI25)%P((I-1)*ISIZ(NI25)+J)
                   ENDDO
         !       ENDIF
               ENDDO
            ENDDO
C================================================================================ 
             
           
           I_STOK_GLO = INTBUF_TAB(NIN)%I_STOK(2)

           IF(NSPMD > 1) THEN
C : on ne change pas NSNR dans IPARI depuis SPMD_I25_SLIDE_GAT, donc NSNR n est plus la taille
C : des structures FI 
C
             CALL SPMD_I25_SLIDE_GAT(NSN  ,NIN, NI25,
     2                               IGAP  ,NSNR   ,INTTH   ,ILEV, INTBUF_TAB(NIN), 
     3                               FR_NOR,IAD_FRNOR, SIZBUFS, ITAB, H3D_DATA    ,
     4                               INTFRIC,FLAGREMN,LREMNORMAX,NRTM,IVIS2       ,
     5                               ISTIF_MSDT)
        ! Passer intbuftab(nin)
C : Les noeuds qui glissent de P0 vers P1 et qui sont locaux a P1 se trouvent quand
C : meme dans la structure frontiere *FI(NIN)%P a la position du proc P1
C : Soit  - remplir les structures locales au lieu des structures *FI directement dans SPMD_I25_SLIDE_GAT
C :       - remplir les structures locales dans une routine a partir des *FI(NIN)%P([P0 ; P1 ; P2 ... ... ]) 
C :       - Retirer ces NS de XREM et IREM lors de la copie des [IR]BUFR vers [IX]REM  

              IPARI(24,NIN) = NSNR
           ENDIF



          END IF
C
C         add new candidates wrt sliding to CAND_OPT_N, CAND_OPT_E 
 50       CONTINUE

          SIZOPT     = INTBUF_TAB(NIN)%S_CAND_OPT_N
          I_OPT_STOK = INTBUF_TAB(NIN)%I_STOK(2)
          CALL I25PREP_SLID_2(
     1   INTBUF_TAB(NIN)%CAND_OPT_N,INTBUF_TAB(NIN)%CAND_OPT_E,NIN ,NI25 ,NSN ,
     2   NSNR   ,NRTM ,SIZOPT ,K_STOK ,INTBUF_TAB(NIN)%MSEGLO,
     3   INTBUF_TAB(NIN)%MSEGTYP24,I_OPT_STOK  ,ITAB ,INTBUF_TAB(NIN)%IRECTM,NADMSR ,
     4   INTBUF_TAB(NIN)%ADMSR,INTBUF_TAB(NIN)%ISLIDE,INTBUF_TAB(NIN)%NSV,
     .      INTBUF_TAB(NIN)%KNOR2MSR,INTBUF_TAB(NIN)%NOR2MSR,
     5   INTBUF_TAB(NIN)%IRTLM,INTBUF_TAB(NIN)%STFM,FLAGREMN,INTBUF_TAB(NIN)%KREMNOR,
     .      INTBUF_TAB(NIN)%REMNOR) 
C
C         Merge NSNFI, etc 
C
C         Upgrade SIZE of CAND_OPT_N, CAND_OPT_E, ...
C         Warning : Arrays are reallocated in UPGRADE_CAND_OPT routine !!!!
          IF(INTBUF_TAB(NIN)%I_STOK(2)+K_STOK > SIZOPT)THEN
            CALL UPGRADE_CAND_OPT(NIN,K_STOK,INTBUF_TAB(NIN))
            GOTO 50
          END IF
C
          INTBUF_TAB(NIN)%I_STOK(2)=I_OPT_STOK ! == INTBUF_TAB(NIN)%I_STOK(2)+K_STOK
          IF (DEBUG(3)>=1) THEN
              NB_DST1(JTASK)  = NB_DST1(JTASK)  + K_STOK
              NB_DST2(JTASK)  = NB_DST2(JTASK)  - K_STOK
          ENDIF
          
!         !deallocate reception buffer
          IF(NSPMD > 1) THEN
            CALL SPMD_I25_SLIDE_EXCH(IBUFR    ,RBUFR    ,ISIZ(NI25) ,RSIZ(NI25) 
     .                      ,SIZBUFR_GLOB     ,COMM_INT ,COMM_REAL,COMM_SIZ
     .                      ,4                ,NI25     ,COMM_PATTERN)
          END IF
C
        ENDDO !Loop on NI
C-----------------------------------------------------------------------
        IF(NSPMD > 1) THEN
          DO NI25=1,NINTER25
          NIN = INTLIST25(NI25)
C
          ! Wait and deallocate Send buffers
          CALL SPMD_I25_SLIDE_EXCH(IBUFS    ,RBUFS      ,ISIZ(NI25)     ,RSIZ(NI25) 
     .                    ,SIZBUFS_GLOB       ,COMM_INT,COMM_REAL,COMM_SIZ
     .                    ,5                  ,NI25, COMM_PATTERN)

          ENDDO
        ENDIF
!       DEALLOCATE(BUFR)
        DEALLOCATE(NADD)
        DEALLOCATE(KADD)

C
C-----------------------------------------------------------------------
      CALL DEALLOCATE_COMM_STRUCT(COMM_INT,NSPMD,NINTER25) 
      CALL DEALLOCATE_COMM_STRUCT(COMM_REAL,NSPMD,NINTER25) 
      CALL DEALLOCATE_COMM_STRUCT(COMM_SIZ,NSPMD,NINTER25) 


!$OMP END SINGLE


C-----------------------------------------------------------------------
      RETURN
      END
C
