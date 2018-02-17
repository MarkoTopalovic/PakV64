C$DEBUG      
C=========================================================================
      SUBROUTINE RACN3D(TT1,SILE,
     1ID,NZAD,ZADVRE,NGPSIL,MAXA,CORD,SKEF,SKEFN,KONT,
     2FZAPR,VREME,TABF,TT10,UBRZ,UBRZ0,AK,
     3VECTJ,IVECT,POVSIL,GRADJN,
     4ITFMAX,AKONST,NASLOV,ICUR,VG,GG,KOJK)
      USE PPR
      USE STIFFNESS
      USE ELEMENTS
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      include 'paka.inc'
      INCLUDE 'mpif.h'
c      COMMON A(17000)
c      REAL A
      COMMON /TRENT3/ ZVHX(21),ZVHY(21),ZVHZ(21),CK(3,21),H(21),
     1 FS2,DETJS,DETJ,NBREL
      COMMON /KRITER/ IDOKL,MAXCUR
      COMMON /TREN13/ FPS1,FPS2,FPS3,FPS4
      COMMON /REPERI/ LCORD,LID,LMAXA,LMHT
      COMMON /BLOCKS/ NBMAX,IBLK,NBLOCK,LMNQ,LICPL,LLREC,KC,LR
      COMMON /LSK1/ LSK
      COMMON /JEDANP/ INDJED,NBRF,NGL,INDTLO
      COMMON /FLUX/ KISA
      COMMON /TACNOS/ EPSTR,MAXIT,NJRAP
      COMMON /ZADPO/ LNZADJ
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /DUZINA/ LMAX,MTOT,LMAXM,LRAD,NRAD
      COMMON /SCRATC/ ISCRC
      COMMON /KONTKT/ ICONT,NEQC,NEQ,NWKC,LMAXAC,LRCTDT
      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /NDESUK/ NDES,IDPRIT,IFORM
      COMMON /PENALL/ PENALT,PRESS,IBRGT
      COMMON /POCETN/ IPOCU,IPOCV,IPOCP,IPOCT,POCU,POCV,POCP,POCT,GAMA
      COMMON /VREPER/ NPER,NTABFT
      COMMON /PROMEN/ NJUTN,INDOPT,INDPRO
      COMMON /DODAT/ NDIMM
      COMMON /DYNAM/ NDIN
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /STAC/ NSTAC,NSLP,IBROJ
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /VDP/ DT,NKORP,NN,NZAV
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
      COMMON /ICITANJE/ INPT
C
      COMMON /SILZP3/ FZAP(3,21)
C
      COMMON /PRIKAZ/ INDSC,IZPOT
C
      COMMON /PIJEZO/ CPOR(3,1000),NPIJEZ(20,100),NODP(100),NPIJ,NPOR,
     1                NPORCV(1000),NPOREL(1000),NEPOR,NPORE,NEPIJ
      COMMON /PORCEL/ RMIN(34),SMIN(34),TMIN(34)
      COMMON /NXNAST/ NXNASTRAN
      COMMON /SOLVER/ ISOLVER
      COMMON /NELDIM/ NDIMEL
      COMMON /DJERDAP/ IDJERDAP
C
      CHARACTER*80 NASLOV
      DIMENSION TT1(*),SILE(*),ZADVRE(*),CORD(3,*)
      DIMENSION VG(3,NET,*),GG(3,NET,*)
      DIMENSION TT10(*),UBRZ(*),UBRZ0(*)
      DIMENSION ID(1,*),NZAD(3,*),NGPSIL(12,*),MAXA(*)
      DIMENSION SKEF(NDES,*),AKONST(3,5,*),SKEFN(*)
      DIMENSION POVSIL(4,*),PR1(21),ITFMAX(*)
C
      DIMENSION KONT(9,MAXLIN,*),KOJK(*)

      DIMENSION TT21(44),TT210(44),QUK(1000),QUM(1000)
      DIMENSION R(3,3),S(3,3),T(3,3),W(3,3)
      DIMENSION RK(3,3),SK(3,3),TK(3,3),WK(3,3)
      DIMENSION FZAPR(3,*),VECTJ(3,*),GRADJN(3,*)
      DIMENSION IVECT(*),LM2(44),ICUR(*)
      DIMENSION PERM(3,4)
      DIMENSION VREME(NPER,950)
      DIMENSION TABF(2,NTABFT,*)
      DIMENSION AM1(21,21),AK(NDES,*)

      DIMENSION RS2(21)
      DIMENSION RS3(21)
      DIMENSION F36(44)
C      DIMENSION PORNIEL(100000,8)
C
      DIMENSION AFIFI1(21,21),AFIFI(21,21)
      DIMENSION PUS(21,21),AJX(20),AJY(20),AJZ(20)
      DIMENSION AJX1(20),AJY1(20),AJZ1(20),NRED(8),NREDK(8)
      DIMENSION NRED1(3),NREDT4(4),NREDT10(10),NRED26(6),NRED28(8)
      DIMENSION HT(9,3),CKL(3,8),TTE(2,3),WTET(3)
!       promenjena duzina NRED sa 8 na 10 i dodata 2 clana
      DATA NRED/8,4,2,6,7,3,1,5/  
      DATA NREDK/27,9,3,21,25,7,1,19/
      DATA NRED1/2,1,12/
      DATA NRED26/1,2,3,12,23,31/
      DATA NRED28/1,2,3,4,12,23,34,41/
      DATA NREDT4/1,2,3,4/
      DATA NREDT10/1,2,3,4,12,23,13,14,24,34/
      integer ierr, myid
      integer*8 LM2,NGPSIL
      integer Dtime(8)
C
      CHARACTER*3 STATTT
      DIMENSION NDJEL(21),NP3D1(5)
      logical OLDNEWW

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)

      IF (myid.ne.0) goto 1234
  
      if (.not.allocated(PORNIEL)) allocate(PORNIEL(NET,10),STAT=istat) 
      if(.not.allocated(IDJSTAMP1))allocate(IDJSTAMP1(5,NPT),STAT=istat) 
      DO I=1,5
        DO JJ=1,NPT
          IDJSTAMP1(I,JJ)=0
        ENDDO
      ENDDO
C
      CALL ICLEAR(NP3D1,5)
!       CALL ICLEAR(IDJSTAMP1,NPT)
C      IDJERDAP=1
!          write(*,*) 'IDJERDAP',IDJERDAP
      IF(IDJERDAP.EQ.1)THEN
!           STATTT='NEW'
!           INQUIRE(FILE='DJ3D1',EXIST=OLDNEWW)
!           IF(OLDNEWW) STATTT='OLD'
!            OPEN (91,FILE='DJ3D1',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
C   program za citanje cvorova u kojima se stampaju filtracione sile
              CALL DJERDAPREAD(IDJSTAMP1,NP3D1)
C
!            READ(91,4111,err=9999) NDJ3D1,IT1,NCVE
! !           IF(STATTT.EQ.'OLD') THEN
! !             OPEN (91,FILE='DJ3D1',STATUS='OLD',FORM='FORMATTED',
! !      1      ACCESS='SEQUENTIAL')
! !             READ(91,4111) NDJ3D1,IT1,NCVE
! !               write(*,*) 'NDJ3D1',NDJ3D1
!            DO I=1,NDJ3D1
!                 READ(91,*)
!                 IF(INPT.EQ.1) THEN
!                    READ(91,4111) (NDJEL(J),J=1,IT1)
!                    READ(91,4111) (NDJEL(J),J=IT1+1,NCVE)
!                 ELSE
!                    READ(91,1111) (NDJEL(J),J=1,IT1)
!                    READ(91,1111) (NDJEL(J),J=IT1+1,NCVE)
!                 ENDIF
!                 DO K=1,NCVE
!                    IDJSTAMP1(NDJEL(K))=KK
!                 ENDDO
!              ENDDO
! C            NP3D1=0
!              DO I=1,NPT
!                 IF(IDJSTAMP1(I).EQ.1) NP3D1(KK)=NP3D1(KK)+1
!              ENDDO
!              write(*,*) 'NP3D1',NP3D1(KK)
!              go to 9998
! !           ELSE
! !              STOP ' NEMA FAJLA DJ3D1'
! !           ENDIF
       ENDIF
! ! C 
!  9999     write(*,*) ' NE POSTOJI FAJL DJ3D1 '         
!           write(3,*) ' NE POSTOJI FAJL DJ3D1 '
!  1111 FORMAT(14I5)
!  4111 FORMAT(13I10)
C INDIKATOR ZA OSU TEZINE
 9998   IF (IOSA.EQ.0) IOSA=3

C INDIKATOR ZA TRAZENJE IZOHIPSI PO FAJLOVIMA:
       INDJED=0

C ZBOG PAK-a NEQ-BROJ JEDNACINA .EQ. JEDN
       NEQ=JEDN
       ICONT=1

C IDOKL JE BROJ CVOROVA KOJI SE UZIMAJU KAO AKTIVNI NA LINIJI CURENJA 
C ICUR JE NIZ U KOME SE NALAZE CVOROVI NA LINIJI CURENJA
C
C  INICIJALIZACIJA KONVERGENCIJE ZA LINIJU CURENJA
       KONV1=1  

       MAXCUR=0
       IDOKL=0
       CALL INIDOK(NZAD,ICUR,CORD)

C UTVRDJIVANJE KOD KOLIKO ELEMENATA SE POJAVLJUJE JEDAN CVOR
      CALL MVECT(IVECT)

      DO I=1,JEDN
       TT1(I)=0.D0
       UBRZ(I)=0.D0
      ENDDO      
       
C DEFINISANJE CVOROVA ZA PROTOKE
      DO KK=1,NKONT
	DO II=1,LIN(KK)
	  KOJK(KONT(2,II,KK))=KOJK(KONT(2,II,KK))+1
	  KOJK(KONT(3,II,KK))=KOJK(KONT(3,II,KK))+1
	  KOJK(KONT(4,II,KK))=KOJK(KONT(4,II,KK))+1
	  KOJK(KONT(5,II,KK))=KOJK(KONT(5,II,KK))+1
	  KOJK(KONT(6,II,KK))=KOJK(KONT(6,II,KK))+1
	  KOJK(KONT(7,II,KK))=KOJK(KONT(7,II,KK))+1
	  KOJK(KONT(8,II,KK))=KOJK(KONT(8,II,KK))+1
	  KOJK(KONT(9,II,KK))=KOJK(KONT(9,II,KK))+1
        ENDDO
      ENDDO	  
C
      R(3,1)=-0.7745966692415
      R(3,2)=0.0
      R(3,3)=0.77459666924148
      S(3,1)=-0.7745966692415
      S(3,2)=0.0
      S(3,3)=0.77459666924148
      T(3,1)=-0.7745966692415
      T(3,2)=0.0
      T(3,3)=0.77459666924148
      W(3,1)=0.55555555555556
      W(3,2)=0.88888888888889
      W(3,3)=0.55555555555556
C
      RK(3,1)=-1.0
      RK(3,2)=0.0
      RK(3,3)=1.0
      SK(3,1)=-1.0
      SK(3,2)=0.0
      SK(3,3)=1.0
      TK(3,1)=-1.0
      TK(3,2)=0.0
      TK(3,3)=1.0
      WK(3,1)=0.33333333333333
      WK(3,2)=1.33333333333333
      WK(3,3)=0.33333333333333
C 
      R(2,1)=-0.5773502691896
      R(2,2)=0.57735026918963
      S(2,1)=-0.5773502691896
      S(2,2)=0.57735026918963
      T(2,1)=-0.5773502691896
      T(2,2)=0.57735026918963
      W(2,1)=1.000000000000000
      W(2,2)=1.000000000000000

      R(1,1)=0.0
      S(1,1)=0.0
      T(1,1)=0.0
      W(1,1)=2.0
      
      PI=4.D0*DATAN(1.D0)
            
C===========================================================================
      IAXIS=6
C
      INULTO=0
       KKORAK=1
      VVREME=0.D0
C indikator LINTE=1 - linearan proracun
!       LINTE=1
C=========================================================================
c petlja po periodima
      DO 600 NNPER=1,NPER
      KORAK=0
c petlja po koracima
   35 KORAK=KORAK+1
C
C=========================================================================   
C  RAD SA STACIONARNIM STANJEM U PRVOM KORAKU I NESTACIONARNIM U OSTALIM
C  ( GEOLOSKI PROFILI ZA CERNE)
C
      IF (KKORAK.EQ.1) then
        NNSTAC=NSTAC
C       WRITE(19) KKORAK
      endif
cz uslov da prvi korak radi stacionarno
      IF (KKORAK.EQ.1.AND.NNSTAC.EQ.2) NSTAC=1
      IF (KKORAK.GT.1.AND.NNSTAC.EQ.2) NSTAC=0
C
cz      IF (KKORAK.GT.0) THEN
      TIME=VREME(NNPER,KORAK)
      VVREME=VVREME+TIME
cz      ELSE
cz       TIME=1.D7
cz       KORAK=KORAK-1
cz      ENDIF

      DO I=1,NPT 
       DO J=1,3
        VECTJ(J,I)=0.D0
        GRADJN(J,I)=0.D0
       ENDDO
      ENDDO

      DO I=1,JEDN
        TT10(I)=TT1(I)
        UBRZ0(I)=UBRZ(I)
      ENDDO

      ITER=0
      ITER1=0
      INDSK=1
      INDPR=0

C==========================================================================
C LABELA 100:
C==========================================================================
 100  IF (ITER.GT.MAXIT) THEN
        WRITE(IIZLAZ,3001) MAXIT
 3001 FORMAT(//
     1,'DOSTIGNUT JE MAKSIMALAN BROJ OD ',I5,' ITERACIJA BEZ POSTIZANJA 
     1KONVERGENCIJE!!'//)
       STOP
       ENDIF

	IF (NBLOCK.GT.1) THEN
       OPEN (ISCRC,FILE='ZSKLIN',FORM='UNFORMATTED',STATUS='UNKNOWN')
       REWIND ISCRC
       ELSE
        KC=NWK
      ENDIF
C===========================================================================
C ZA CVOROVE NA LINIJI PROCURIVANJA INICIJALIZACIJA ZA NESTACIONARNO
C STRUJANJE - START JE UVEK OD PRVOG NAJNIZEG CVORA PA NA VISE
C      IF (ITER.EQ.0.AND.MAXCUR.NE.0) IDOKL=2
      IF (ITER.EQ.0.AND.MAXCUR.NE.0) IDOKL=MAXCUR
C===========================================================================
      IF(NXNASTRAN.EQ.0) THEN
	IF (MAXCUR.GT.0) THEN
          if(ISOLVER.NE.-11) CALL CLEAR(A(LSK),KC)
        ELSE
          IF (ITER.EQ.0.and.ISOLVER.NE.-11) CALL CLEAR(A(LSK),KC)
        ENDIF
      ENDIF
       
	DO I=1,JEDN
	 SILE(I)=0.D0
	 UBRZ(I)=0.D0
	ENDDO 

C
      IF((KKORAK.EQ.1).AND.(ITER.EQ.0))THEN
C petlja po pornim celijama
      do II=1,NPORE
C odredjivanje lokalnih koordinata porne celije
      deltta=0.1
      RP=-1.02
      SP=-1.02
      TP=-1.02
      DMIN=100.0
      RMIN(II)=100.0
      SMIN(II)=100.0
      TMIN(II)=100.0
      DRST=0.02
      DO  IR=1,100
        RP=RP+DRST
        SP=-1.02
      DO  JS=1,100
        SP=SP+DRST
        TP=-1.02
      DO  LT=1,100
        TP=TP+DRST
c        CALL INTERST(RP,SP,TP)
          RPP=1.0+RP
          SPP=1.0+SP
          TPP=1.0+TP
          RM=1.0-RP
          TM=1.0-TP
          SM=1.0-SP
          RR=1.0-RP*RP
          SS=1.0-SP*SP
          TT=1.0-TP*TP
          DO I=1,8
            H(I)=0.
          ENDDO
          H(1)=0.125*RPP*SPP*TPP
          H(2)=0.125*RM*SPP*TPP
          H(3)=0.125*RM*SM*TPP
          H(4)=0.125*RPP*SM*TPP
          H(5)=0.125*RPP*SPP*TM
          H(6)=0.125*RM*SPP*TM
          H(7)=0.125*RM*SM*TM
          H(8)=0.125*RPP*SM*TM
        XT=0.
        YT=0.
        ZT=0.
        DO JJ=1,8
          XT=XT + H(JJ)*CORD(1,NEL(JJ,NPOREL(II)))
          YT=YT + H(JJ)*CORD(2,NEL(JJ,NPOREL(II)))
          ZT=ZT + H(JJ)*CORD(3,NEL(JJ,NPOREL(II)))
        enddo
        DX=XT-CPOR(1,II)
        DY=YT-CPOR(2,II)
        DZ=ZT-CPOR(3,II)
        dxyz=DX*DX+DY*DY+DZ*DZ
        DXYZ=SQRT(dxyz)
        IF (DXYZ.LE.DMIN) THEN
            DMIN=DXYZ
            RMIN(II)=RP
            SMIN(II)=SP
            TMIN(II)=TP
         ENDIF
      enddo
      enddo
      enddo
C  kraj petlje po pornim celijama
         iel=NPOREL(II)
         NMAT=NEL(NDIM+1,iel)
       WRITE (3,*)"PORNA CELIJA", II, NPOREL(II),(CPOR(ijk,II), ijk=1,3)
       WRITE(3,*) DMIN,RMIN(II),SMIN(II),TMIN(II),NMAT 
      enddo
      
        IF ((ISOLVER.EQ.-11).OR.(ISOLVER.EQ.1)) THEN
C         MUMPS
          CALL sparseassembler_init(1)
        ENDIF
      ENDIF     
C GLAVNA PETLJA PO ELEMENTIMA
      IF(NXNASTRAN.EQ.1) GO TO 999
      DO 400 NBREL=1,NET
      IF(elemtip(NBREL).gt.100) THEN
        NTYPE=elemtip(NBREL)/100
        NTIMES=100
      ELSE
        NTYPE=elemtip(NBREL)/10
        NTIMES=10
      ENDIF
      NDIMEL=elemtip(NBREL)-NTIMES*NTYPE
C ODEDIVANJE BROJA GAUSOVIH TACAKA PRILIKOM INTEGRACIJE
      IBRGT=3
      IF (NDIMEL.LE.8) IBRGT=2

      NMAT=NEL(NDIM+1,NBREL)
      NEMA=NEL(NDIM+2,NBREL)

      IF (NMAT.LE.0.OR.NEMA.GT.0) THEN
         WRITE(3,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
         WRITE(*,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
!          STOP
      ENDIF
      IF (NMAT.LE.0) GOTO 400
      IF (NEMA.GT.0) GOTO 400
      
       PERM(1,1)=AKONST(3,1,NMAT)
       PERM(1,2)=0.
       PERM(1,3)=0.
       PERM(2,2)=AKONST(3,2,NMAT)
       PERM(2,1)=0.
       PERM(2,3)=0.
       PERM(3,3)=AKONST(3,3,NMAT)
       PERM(3,1)=0.
       PERM(3,2)=0.

      IF (PERM(1,1).LT.1.D-15.OR.PERM(2,2).LT.1.D-15.OR.
     1    PERM(3,3).LT.1.D-15) THEN
         WRITE(3,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
         WRITE(*,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
         WRITE(*,*) 'PERM',PERM(1,1),PERM(2,2),PERM(3,3)
         STOP
      ENDIF
C=========================================================================
      DO 130 KLM=1,NDIMEL
      CK(1,KLM)=CORD(1,NEL(KLM,NBREL))
      CK(2,KLM)=CORD(2,NEL(KLM,NBREL))
      CK(3,KLM)=CORD(3,NEL(KLM,NBREL))
C
      LM2(KLM)=ID(1,NEL(KLM,NBREL))
 130  CONTINUE
    
      if(NTYPE.eq.2) CALL LCK2D(CK,CKL,TTE,NDIMEL)
C=======================================================================
      CALL CLEARD(TT21,NDIM)
      CALL PREB(TT21,TT1,ID,NEL,NBREL)
      CALL CLEARD(TT210,NDIM)
      CALL PREB(TT210,TT10,ID,NEL,NBREL)
 
C=======================================================================
      DO 163 K=1,NDIM
      DO 162 N=1,NDIM
        AM1(K,N)=0.D0    
        AFIFI(K,N)=0.D0    
        AFIFI1(K,N)=0.D0    
        PUS(K,N)=0.D0    
  162 CONTINUE
  163 CONTINUE

C POVRSINSKE SILE I ZAPREMINSKE SILE:
      DO 199 I=1,NDIM
      RS2(I)=0.
      RS3(I)=0.
 199  CONTINUE
C
C INTEGRACIJA U GAUSOVIM TACKAMA

C		ZASIC=0.D0

C sneza 11.02.2012
c       DO II=1,NDIM
c         PR1(II)=TT21(II)-CORD(IOSA,NEL(II,NBREL))
c        ENDDO

      NGAUS=0
      POVREL=0.
      IBRGT1=IBRGT
      IBRGT2=IBRGT
      if(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
        IBRGT2=1
        IBRGT1=4
      ENDIF
      IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) IBRGT2=1
      IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) IBRGT1=1
      IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
        IBRGT2=1
        IBRGT1=3
      ENDIF
      DO 180 I=1,IBRGT1
      DO 170 J=1,IBRGT2
      DO 160 L=1,IBRGT2
      NGAUS=NGAUS+1
      IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
       CALL GETRST(RT,ST,TT,WTET,NGAUS,IBRGT1)
       CALL INTERPTETRA(RT,ST,TT,0,0,NDIMEL)
       WDT=WTET(1)*WTET(2)*WTET(3)*DETJ
      ELSE IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
        CALL INTERP1D(R(IBRGT,I),0,0,NDIMEL,AJS)  
        WDT=W(IBRGT,I)*thick(NBREL)*AJS
      ELSE IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) THEN
        CALL INTERP2D48(R(IBRGT,J),S(IBRGT,L),0,0,NDIMEL,CKL,TTE)
        WDT=W(IBRGT,J)*W(IBRGT,L)*DETJ*thick(NBREL)
      ELSE IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
        RT=1./6
        ST=1./6
        WT=1./3
        IF(NGAUS.EQ.2) RT=2./3
        IF(NGAUS.EQ.3) ST=2./3
        CALL INTERP2D36(RT,ST,0,0,NDIMEL,CKL,TTE)
        WDT = WT**2*thick(NBREL)*DETJ
      ELSE
        CALL INTER3(R(IBRGT,I),S(IBRGT,J),T(IBRGT,L),0,0)
        WDT=W(IBRGT,I)*W(IBRGT,J)*W(IBRGT,L)*DETJ
      ENDIF
      
      POVREL=POVREL+WDT
      
Cs ovaj uslov nije isti za stacionarno i nestacionarno
Cs za nestacionarno proverava u svakoj iteraciji 
      IF ( (((ITER.GT.0).AND.(kkorak.EQ.1))
     1.OR.KKORAK.GT.1) .AND.(INDFS.EQ.1)) THEN
C      IF ( (((ITER.GT.0).AND.(NSTAC.EQ.1))
C     1.OR.((KKORAK.GT.0).AND.(NSTAC.EQ.0))) .AND.(INDFS.EQ.1)) THEN
C
       PR=0.D0
C okvasenost u gausovoj tacki
        DO II=1,NDIMEL
         PR=PR+H(II)*(TT21(II)-CORD(IOSA,NEL(II,NBREL)))
        ENDDO

           IF( PR.LT.0.D0 .AND. INDFS.EQ.1 )THEN
            ANPUS=AKONST(3,4,NMAT)*ZASIC
            PERM(1,1)=ZASIC*AKONST(3,1,NMAT)
            PERM(2,2)=ZASIC*AKONST(3,2,NMAT)
            PERM(3,3)=ZASIC*AKONST(3,3,NMAT)
           ELSE
            ANPUS=0.D0
            PERM(1,1)=0.0D0
            PERM(2,2)=0.0D0
            PERM(3,3)=0.0D0
           ENDIF
       ELSE
        PERM(1,1)=0.0D0
        PERM(2,2)=0.0D0
        PERM(3,3)=0.0D0
        ANPUS=0.D0
       ENDIF

      DO 165 K=1,NDIMEL
      DO 164 N=1,NDIMEL
cz S nije podeljeno sa dt; podeljeno sa dt prilikom pakovanja u matricu SKEF
         AM1(K,N)=AM1(K,N)+AKONST(3,4,NMAT)*H(K)*H(N)*WDT
         AFIFI(K,N)=AFIFI(K,N)+(AKONST(3,1,NMAT)*ZVHX(K)*ZVHX(N)+
     1AKONST(3,2,NMAT)*ZVHY(K)*ZVHY(N)+AKONST(3,3,NMAT)*ZVHZ(K)*ZVHZ(N))
     2*WDT
C
         IF (INDFS.EQ.1) THEN
         PUS(K,N)=PUS(K,N)+ANPUS*H(K)*H(N)*WDT
         AFIFI1(K,N)=AFIFI1(K,N)+(PERM(1,1)*ZVHX(K)*ZVHX(N)+
     1PERM(2,2)*ZVHY(K)*ZVHY(N)+PERM(3,3)*ZVHZ(K)*ZVHZ(N))*WDT
         ENDIF
!       if(NBREL.eq.1.AND.I.eq.IBRGT) then
!          WRITE(*,*) 'NBREL',NBREL
!          WRITE(*,*) 'H(K)',H(K)
!          WRITE(*,*) 'H(N)',H(N)
!          WRITE(*,*) 'ZVHX(K)',ZVHX(K)
!          WRITE(*,*) 'ZVHY(K)',ZVHY(K)
!          WRITE(*,*) 'ZVHZ(K)',ZVHZ(K)
!          WRITE(*,*) 'WDT',WDT
!          WRITE(*,*) 'AKONST(3,1,NMAT)',AKONST(3,1,NMAT)
!          WRITE(*,*) 'AKONST(3,2,NMAT)',AKONST(3,2,NMAT)
!          WRITE(*,*) 'AKONST(3,3,NMAT)',AKONST(3,3,NMAT)
!          write(*,*) 'AM1(',K,',',N,')',AM1(K,N)
!          write(*,*) 'AFIFI(',K,',',N,')',AFIFI(K,N)
!       ENDIF
C
  164 CONTINUE
  165 CONTINUE
  160 CONTINUE
  170 CONTINUE
  180 CONTINUE
!       WRITE(*,*) 'POVREL',POVREL
C INCIJALIZACIJA MATRICE SKEF I F36
      DO 260 I=1,NDES
      DO 258 J=1,NDES
      SKEF(I,J)=0.
 258  CONTINUE
      F36(I)=0.
 260  CONTINUE
C=========================================================================
C UNULJAVANJE VEKTORA RS2
	INDPUN=0
       DO I=1,NDIM
         RS2(I)=0.D0
       ENDDO
C KADA KISA PADA BAS NA LINIJU SLOBODNE POVRSINE:
CZ      IF ((DABS(ZASIC).GT.1.D-10).AND.(INDFS.EQ.1).AND.(MAXSIL.GT.0)) 
CZ     &THEN
CZ        IVRF=NGPSIL(6,1)
CZ        CALL TIMFUN (TABF,FK1,VVREME,ITFMAX(IVRF),IVRF)
CZ        CALL PROCEN(RS2,PR1,FK1,NEL,NBREL,CORD) 
CZ        INDPUN=1
CZ       ENDIF
C============================================================================
C POVRSINSKE SILE
C=================
      IF (INDPUN.EQ.1) GOTO 265

      DO 250 JBRPS=1,MAXSIL
      IF (NBREL.EQ.NGPSIL(1,JBRPS)) THEN
      NODE1=NGPSIL(2,JBRPS)
      NODE2=NGPSIL(3,JBRPS)
      NODE3=NGPSIL(4,JBRPS)
      NODE4=NGPSIL(5,JBRPS)
      NODE5=NGPSIL(6,JBRPS)
      NODE6=NGPSIL(7,JBRPS)
      NODE7=NGPSIL(8,JBRPS)
      NODE8=NGPSIL(9,JBRPS)
      N1=NEL(1,NBREL)
      N2=NEL(2,NBREL)
      N3=NEL(3,NBREL)
      N4=NEL(4,NBREL)
      N5=NEL(5,NBREL)
      N6=NEL(6,NBREL)
      N7=NEL(7,NBREL)
      N8=NEL(8,NBREL)
!       N9=NEL(9,NBREL)
!       N10=NEL(10,NBREL)
      IVRF=NGPSIL(10,JBRPS)
      CALL TIMFUN (TABF,FK1,VVREME,ITFMAX(IVRF),IVRF)
      FPS1=POVSIL(1,JBRPS)*FK1
      FPS2=POVSIL(2,JBRPS)*FK1
      FPS3=POVSIL(3,JBRPS)*FK1
      FPS4=POVSIL(4,JBRPS)*FK1
C
      NUMGAU=IBRGT
      
      IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
        DO I=1,NUMGAU
          CALL INTERP1D(R(IBRGT,I),0,0,NDIMEL,AJS)  
          WDTV=W(IBRGT,I)*thick(NBREL)*AJS
          DO K=1,NDIMEL
          DUM=TT21(K)-CORD(IOSA,NEL(K,NBREL))
          IF ( (((ITER.GT.0).AND.(NSTAC.EQ.1))
     1          .OR.((KKORAK.GT.0).AND.(NSTAC.EQ.0)))
     1          .AND.INDFS.EQ.1.AND.DUM.LT.0.) THEN
          ELSE
            RS2(K)=RS2(K)+H(K)*FPS1*WDTV
          ENDIF
        ENDDO
        ENDDO
        GOTO 250
      ENDIF
      
      IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
         DO NGXT = 1,3
             RT=1./6
             ST=1./6
             WT=1./3
             IF(NGXT.EQ.2) RT=2./3
             IF(NGXT.EQ.3) ST=2./3

            CALL JACTP33(RT,ST,HT,1,NGPSIL,JBRPS,CORD,NDIMEL)
            WDTS = WT*DETJS/2
            DO K=2,9
              DO j=1,NDIMEL
                IF(NGPSIL(K,JBRPS).EQ.NEL(J,NBREL)) THEN
                  DUM=TT21(J)-CORD(IOSA,NEL(J,NBREL))
                  IF ( (((ITER.GT.0).AND.(NSTAC.EQ.1))
     1                .OR.((KKORAK.GT.0).AND.(NSTAC.EQ.0)))
     1                .AND.INDFS.EQ.1.AND.DUM.LT.0.) THEN
                  ELSE
                    RS2(J)=RS2(J)+HT(K-1,1)*FS2*WDTS
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
         ENDDO 
      
      ELSE
      
      DO 225 I=1,NUMGAU
      DO 225 J=1,NUMGAU
C
C KONSTANTNO T (ZETA)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N2.AND.NODE3.EQ.N3.AND.NODE4.EQ.N4).
     1OR.(NODE1.EQ.N2.AND.NODE2.EQ.N3.AND.NODE3.EQ.N4.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N3.AND.NODE2.EQ.N4.AND.NODE3.EQ.N1.AND.NODE4.EQ.N2).
     3OR.(NODE1.EQ.N4.AND.NODE2.EQ.N1.AND.NODE3.EQ.N2.AND.NODE4.EQ.N3).
C
     4OR.(NODE1.EQ.N1.AND.NODE2.EQ.N4.AND.NODE3.EQ.N3.AND.NODE4.EQ.N2).
     5OR.(NODE1.EQ.N4.AND.NODE2.EQ.N3.AND.NODE3.EQ.N2.AND.NODE4.EQ.N1).
     6OR.(NODE1.EQ.N3.AND.NODE2.EQ.N2.AND.NODE3.EQ.N1.AND.NODE4.EQ.N4).
     7OR.(NODE1.EQ.N2.AND.NODE2.EQ.N1.AND.NODE3.EQ.N4.AND.NODE4.EQ.N3))
     8THEN
      CALL INTER3(R(NUMGAU,I),S(NUMGAU,J),1.D0,3,1)
       GOTO 220
      ENDIF 
C
C KONSTANTNO T (ZETA)
C 
      IF((NODE1.EQ.N5.AND.NODE2.EQ.N6.AND.NODE3.EQ.N7.AND.NODE4.EQ.N8).
     1OR.(NODE1.EQ.N6.AND.NODE2.EQ.N7.AND.NODE3.EQ.N8.AND.NODE4.EQ.N5).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N8.AND.NODE3.EQ.N5.AND.NODE4.EQ.N6).
     3OR.(NODE1.EQ.N8.AND.NODE2.EQ.N5.AND.NODE3.EQ.N6.AND.NODE4.EQ.N7).
C
     5OR.(NODE1.EQ.N5.AND.NODE2.EQ.N8.AND.NODE3.EQ.N7.AND.NODE4.EQ.N6).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N7.AND.NODE3.EQ.N6.AND.NODE4.EQ.N5).
     7OR.(NODE1.EQ.N7.AND.NODE2.EQ.N6.AND.NODE3.EQ.N5.AND.NODE4.EQ.N8).
     8OR.(NODE1.EQ.N6.AND.NODE2.EQ.N5.AND.NODE3.EQ.N8.AND.NODE4.EQ.N7))
     8THEN
      CALL INTER3(R(NUMGAU,I),S(NUMGAU,J),-1.D0,3,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO R (KSI)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N4.AND.NODE3.EQ.N8.AND.NODE4.EQ.N5).
     1OR.(NODE1.EQ.N4.AND.NODE2.EQ.N8.AND.NODE3.EQ.N5.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N8.AND.NODE2.EQ.N5.AND.NODE3.EQ.N1.AND.NODE4.EQ.N4).
     3OR.(NODE1.EQ.N5.AND.NODE2.EQ.N1.AND.NODE3.EQ.N4.AND.NODE4.EQ.N8).
C
     4OR.(NODE1.EQ.N1.AND.NODE2.EQ.N5.AND.NODE3.EQ.N8.AND.NODE4.EQ.N4).
     5OR.(NODE1.EQ.N5.AND.NODE2.EQ.N8.AND.NODE3.EQ.N4.AND.NODE4.EQ.N1).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N4.AND.NODE3.EQ.N1.AND.NODE4.EQ.N5).
     7OR.(NODE1.EQ.N4.AND.NODE2.EQ.N1.AND.NODE3.EQ.N5.AND.NODE4.EQ.N8))
     8THEN
      CALL INTER3(1.D0,S(NUMGAU,I),T(NUMGAU,J),1,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO R (KSI)
C
      IF((NODE1.EQ.N2.AND.NODE2.EQ.N3.AND.NODE3.EQ.N7.AND.NODE4.EQ.N6).
     1OR.(NODE1.EQ.N3.AND.NODE2.EQ.N7.AND.NODE3.EQ.N6.AND.NODE4.EQ.N2).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N6.AND.NODE3.EQ.N2.AND.NODE4.EQ.N3).
     3OR.(NODE1.EQ.N6.AND.NODE2.EQ.N2.AND.NODE3.EQ.N3.AND.NODE4.EQ.N7).
C
     4OR.(NODE1.EQ.N2.AND.NODE2.EQ.N6.AND.NODE3.EQ.N7.AND.NODE4.EQ.N3).
     5OR.(NODE1.EQ.N6.AND.NODE2.EQ.N7.AND.NODE3.EQ.N3.AND.NODE4.EQ.N2).
     6OR.(NODE1.EQ.N7.AND.NODE2.EQ.N3.AND.NODE3.EQ.N2.AND.NODE4.EQ.N6).
     7OR.(NODE1.EQ.N3.AND.NODE2.EQ.N2.AND.NODE3.EQ.N6.AND.NODE4.EQ.N7))
     8THEN
      CALL INTER3(-1.D0,S(NUMGAU,I),T(NUMGAU,J),1,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO S (ETA)
C
      IF((NODE1.EQ.N4.AND.NODE2.EQ.N3.AND.NODE3.EQ.N7.AND.NODE4.EQ.N8).
     1OR.(NODE1.EQ.N3.AND.NODE2.EQ.N7.AND.NODE3.EQ.N8.AND.NODE4.EQ.N4).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N8.AND.NODE3.EQ.N4.AND.NODE4.EQ.N3).
     3OR.(NODE1.EQ.N8.AND.NODE2.EQ.N4.AND.NODE3.EQ.N3.AND.NODE4.EQ.N7).
C
     4OR.(NODE1.EQ.N3.AND.NODE2.EQ.N4.AND.NODE3.EQ.N8.AND.NODE4.EQ.N7).
     5OR.(NODE1.EQ.N4.AND.NODE2.EQ.N8.AND.NODE3.EQ.N7.AND.NODE4.EQ.N3).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N7.AND.NODE3.EQ.N3.AND.NODE4.EQ.N4).
     7OR.(NODE1.EQ.N7.AND.NODE2.EQ.N3.AND.NODE3.EQ.N4.AND.NODE4.EQ.N8))
     8THEN
      CALL INTER3(R(NUMGAU,I),-1.D0,T(NUMGAU,J),2,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO S (ETA)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N2.AND.NODE3.EQ.N6.AND.NODE4.EQ.N5).
     1OR.(NODE1.EQ.N2.AND.NODE2.EQ.N6.AND.NODE3.EQ.N5.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N6.AND.NODE2.EQ.N5.AND.NODE3.EQ.N1.AND.NODE4.EQ.N2).
     3OR.(NODE1.EQ.N5.AND.NODE2.EQ.N1.AND.NODE3.EQ.N2.AND.NODE4.EQ.N6).
C
     4OR.(NODE1.EQ.N2.AND.NODE2.EQ.N1.AND.NODE3.EQ.N5.AND.NODE4.EQ.N6).
     5OR.(NODE1.EQ.N1.AND.NODE2.EQ.N5.AND.NODE3.EQ.N6.AND.NODE4.EQ.N2).
     6OR.(NODE1.EQ.N5.AND.NODE2.EQ.N6.AND.NODE3.EQ.N2.AND.NODE4.EQ.N1).
     7OR.(NODE1.EQ.N6.AND.NODE2.EQ.N2.AND.NODE3.EQ.N1.AND.NODE4.EQ.N5))
     8THEN
      CALL INTER3(R(NUMGAU,I),1.D0,T(NUMGAU,J),2,1)
       GOTO 220
      ENDIF
 
 220    IF (KISA.EQ.1) THEN
C           DOGRADITI KASNIJE 
C KADA PADA KISA ONDA FLUX DELUJE U PRAVCU -IOSA OSE 
C TAKO DA MORA DA SE PROJEKTUJE NA NORMALU LINIJE PO KOJOJ DELUJE
C          FAK=DABS(CORD(1,NODE1)-CORD(1,NODE2))
C          WDTS=FAK
          WDTS=W(NUMGAU,I)*W(NUMGAU,J)*DETJS
        ELSE
          WDTS=W(NUMGAU,I)*W(NUMGAU,J)*DETJS
        ENDIF

       DO K=1,NDIMEL
        DUM=TT21(K)-CORD(IOSA,NEL(K,NBREL))
        IF ( (((ITER.GT.0).AND.(NSTAC.EQ.1))
     1.OR.((KKORAK.GT.0).AND.(NSTAC.EQ.0)))
     1.AND.INDFS.EQ.1.AND.DUM.LT.0.) THEN
        ELSE
         RS2(K)=RS2(K)+H(K)*FS2*WDTS
        ENDIF
       ENDDO

C     write(3,*) nbrel,i,j,R(IBRGT,I),S(IBRGT,J)
C     write(3,*) 'fs2,wdts,detjs',fs2,wdts,detjs
C     call wrr(h,ndim,'h   ')
C     call wrr(rs2,ndim,'rs2 ')
 225  CONTINUE
      ENDIF
 
      ENDIF
 250  CONTINUE

C      ENDIF
C KRAJ PETLJE AKO JE ITER>0
C=======================================================================

C PAKOVANJE POVRSINSKIH SILA SA DESNE STRANE
 265   DO I=1,NDIMEL
         F36(I)=F36(I)+RS2(I)
       ENDDO

C=========================================================================
      DO I=1,NDIMEL
      DO J=1,NDIMEL
       AK(I,J)=0.D0
      ENDDO
      ENDDO
C PAKOVANJE MATRICA AMUU,AMUW,AMWW,AKUU,AKUW,AKWW U MATRICE AMM,AC,AK
      DO I=1,NDIMEL
      DO J=1,NDIMEL
c sneza 10.02.2012
c         AK(I,J)=AFIFI(I,J)-AFIFI1(I,J)
         AK(I,J)=AFIFI(I,J)
        IF ((NSTAC.EQ.0).AND.(KKORAK.GT.0)) THEN
c sneza 10.02.2012
c         AK(I,J)=AK(I,J)+(AM1(I,J)-PUS(I,J))/TIME
         AK(I,J)=AK(I,J)+AM1(I,J)/TIME
        ENDIF
      ENDDO
      ENDDO
C============================================================================
C PAKOVANJE MATRICE AK U MATRICU SKEF
      DO 263 I=1,NDIMEL
      DO 262 J=1,NDIMEL
      SKEF(I,J)=AK(I,J)
  262 CONTINUE
  263 CONTINUE
C============================================================================
C PAKOVANJE Kfifi*fi(i-1) SA DESNE STRANE
      DO I=1,NDIMEL
       DO J=1,NDIMEL
        F36(I)=F36(I)-(AFIFI(I,J)-AFIFI1(I,J))*TT21(J)
       IF ( (NSTAC.EQ.0).AND.(KKORAK.GT.0) ) THEN
         F36(I)=F36(I)-((AM1(I,J)-PUS(I,J))*(TT21(J)-TT210(J)))/TIME
       ENDIF
       ENDDO
      ENDDO
C============================================================================
C VEKTOR PROTOKA PO CVOROVIMA
      DO I=1,NDIMEL
       N=NEL(I,NBREL)
       JJ=ID(1,N)
       IF(JJ.GT.0) THEN

         POT=0.D0
        DO J=1,NDIMEL

         POT=POT+(AFIFI(I,J)-AFIFI1(I,J))*TT21(J)
cz uporediti resenja sa ovim uslovom za nestacionarni protok
cz proveriti resenja za zasic=0.
      	IF (NSTAC.NE.1) THEN
          POT=POT+((AM1(I,J)-PUS(I,J))*(TT21(J)-TT210(J)))/TIME
        ENDIF

        ENDDO
         UBRZ(JJ)=UBRZ(JJ)+POT
       ENDIF
      ENDDO
C PAKOVANJE MATRICE ELEMENTA I VEKTRA DESNE STRANE U SISTEM
       CALL PSKEFN(SKEF,SKEFN,NDIMEL)
       CALL SPAKDE(SILE,F36,LM2,NDIMEL)
      IF (ISOLVER.EQ.0) THEN
        IF (MAXCUR.GT.0) THEN
          CALL SPAKUJ(A(LSK),MAXA,SKEFN,LM2,NDIMEL)
        ELSE
cz ovaj uslov ne valja jer ne radi levu stranu za full newton      
C        IF (ITER.EQ.0.OR.NJRAP.EQ.2) 
          IF (ITER.EQ.0) 
     +                  CALL SPAKUJ(A(LSK),MAXA,SKEFN,LM2,NDIMEL)
        ENDIF
      ELSEIF ((ISOLVER.EQ.-11).OR.(ISOLVER.EQ.1)) THEN
C      IMSL Sparse OR MUMPS
        IF(ITER.EQ.0) THEN
C         CALL SPARSE_ELEM_LEFT(SKEF,NEL,ID,NDES,NBREL,NDIM,1)
          CALL sparseassembler_addelemmatrix(NDES,LM2,SKEF)
        ENDIF
      ENDIF
C=======================================================================
C KRAJ PETLJE PO ELEMENTIMA
C=======================================================================
 400  CONTINUE
C=======================================================================
         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'vreme kraj petlje po elementima', (Dtime(i),i=5,7)

C CVOROVI NA LINIJI PROCURIVANJA       
C=======================================================================
  	 IF (MAXCUR.GT.0) THEN
        CALL CURI1(CORD,A(LSK),SILE,MAXA,TT1,ID,ICUR)
C       ELSE
C        IF(ITER.EQ.0) CALL CURI1(CORD,A(LSK),SILE,MAXA,TT1,ID,ICUR)
       ENDIF
C==========================================================================
C MUMPS alociranje matrice krutosti
1234  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(ITER,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(ISOLVER,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(NBLOCK,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(MAXCUR,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(KKORAK,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        IF (ITER.EQ.0) THEN

         IF(ISOLVER.EQ.0) THEN
          if(myid.eq.0) then
          IF (NBLOCK.GT.1) THEN
           LLM =LRAD
           LSKE=LLM+100
           CALL SPAKUA(A(LSK),MAXA,A(LSKE),LM2,NDES,0,
     &                        A(LMNQ),A(LLREC),NBLOCK,LR,IBLK,H,NZAD)
           CLOSE (ISCRC,STATUS='KEEP')
          ENDIF
          endif
         ELSEIF((ISOLVER.EQ.-11).OR.(ISOLVER.EQ.1)) THEN
          
          if(.not.allocated(rows)) then
            if(myid.eq.0) call sparseassembler_getnz(nonzeros)
            CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
            CALL MPI_BCAST(nonzeros,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            allocate(rows(nonzeros),STAT=istat)
            if(istat.ne.0) stop 'error allocating rows'
            allocate(columns(nonzeros),STAT=istat)
            if(istat.ne.0) stop 'error allocating columns'
            allocate(stiff(nonzeros),STAT=istat)
            if(istat.ne.0) stop 'error allocating stiff'
          endif
C           IMSL Sparse
c            CALL SPARSE_BUILD(ISPARSE_N,ISPARSE_NZ,
c     1                DSPARSE_A,ISPARSE_ROWS,ISPARSE_COLS)
          if(myid.eq.0) CALL sparseassembler_getsparse(nonzeros,
     1                 rows,columns,stiff)
          if(myid.eq.0) CALL sparseassembler_kill()
         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'vreme posle sparse kill', (Dtime(i),i=5,7)

         IF(KKORAK.EQ.1) THEN
              stiff_n = JEDN
          ENDIF
         ENDIF
        ENDIF
C==========================================================================
	IF (MAXCUR.GT.0) THEN
	 IF(ISOLVER.EQ.0) THEN
          if(myid.eq.0) CALL ZADLEV(A(LSK),MAXA,A(LNZADJ),NUMZAD,NZAD)
         ELSE
          if(myid.eq.0) CALL SPARS_ZADLEV(A(LNZADJ),NUMZAD,NZAD,
     1                                CORD,VVREME,TABF,NTABFT,ITFMAX)
         ENDIF
         CALL RESEN(A(LSK),TT10,MAXA,JEDN,1)
        ELSE
cz proveriti i ovde uslov za full newton        
C        IF (ITER.EQ.0.OR.NJRAP.EQ.2) THEN
        IF (ITER.EQ.0) THEN
cz proveriti rad sa blokovima i zadlev
          if(myid.eq.0) then
          IF (NBLOCK.GT.1) THEN
            LLM =LRAD
            LSKE=LLM+100
            CALL SPAKUA(A(LSK),MAXA,A(LSKE),LM2,NDES,0,
     &          A(LMNQ),A(LLREC),NBLOCK,LR,IBLK,H,NZAD)
            CLOSE (ISCRC,STATUS='KEEP')

          ENDIF
          endif
          
          IF(ISOLVER.EQ.0) THEN
            if(myid.eq.0) CALL ZADLEV(A(LSK),MAXA,A(LNZADJ),NUMZAD,NZAD)
          ELSE
           
           if(myid.eq.0) CALL SPARS_ZADLEV(A(LNZADJ),NUMZAD,NZAD,
     1                                  CORD,VVREME,TABF,NTABFT,ITFMAX)
          ENDIF
         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'pre resen 1', (Dtime(i),i=5,7)
          CALL RESEN(A(LSK),TT10,MAXA,JEDN,1)
         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'posle resen 1', (Dtime(i),i=5,7)
        ENDIF
        ENDIF

       TOL=1.0D-7
C=======================================================================
         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'vreme pre pisanja desne strane', (Dtime(i),i=5,7)

       DO 410 I=1,NUMZAD
c       write(iizlaz,*)'fk1=',fk1
c       do ii=1,2
c        write(iizlaz,*)TABF(1,1,Ii),TABF(2,1,Ii)
c       enddo
       CALL TIMFUN (TABF,FK1,VVREME,ITFMAX(NZAD(3,I)),NZAD(3,I))

       IF(NZAD(2,I).EQ.6.AND.CORD(IOSA,NZAD(1,I)).GT.FK1) GOTO 410
c       write(iizlaz,*)'fk1=',fk1, vvreme,ITFMAX(NZAD(3,I)),NZAD(3,I)
c       do ii=1,2
c        write(iizlaz,*)TABF(1,1,Ii),TABF(2,1,Ii)
c       enddo
       IF (NSTAC.EQ.0) THEN
C STRUJANJE NESTACIONARNO
C===========================================================================
C KONSTANTNI POTENCIJALI
        IF( (NZAD(2,I).EQ.1.OR.NZAD(2,I).EQ.6.OR.
     &        (NZAD(2,I).EQ.3.AND.KKORAK.EQ.1))
     &        .AND.(ITER.EQ.0).AND.(ID(1,NZAD(1,I)).GT.0)) THEN
cz oduzimanje zadate vrednosti od prethodno izracunate             
            SILE(ID(1,NZAD(1,I)))=1.D035*
     1                            ((ZADVRE(I)*FK1)-TT1(ID(1,NZAD(1,I))))
       ENDIF
C ZPREMINSKI IZVOR, PONOR       
       IF ( (ID(1,NZAD(1,I)).GT.0) .AND. (NZAD(2,I).EQ.5)) THEN
          SILE(ID(1,NZAD(1,I)))=ZADVRE(I)*FK1
       ENDIF

cz ovo ima smisla kod izcurivanja na slobodnoj povrsini (PROVERI USLOVE?)
      IF ((KKORAK.EQ.1).AND.(NZAD(2,I).EQ.-1).AND.(INDFS.EQ.1)) 
     1      SILE(ID(1,NZAD(1,I)))=1.0D35*CORD(IOSA,NZAD(1,I))
c     
cz zadavanje promenljivog potencijala
        IF( (NZAD(2,I).EQ.2).AND.(ID(1,NZAD(1,I)).GT.0).AND.
     1     (ITER.EQ.0)) then
cc          if((CORD(IOSA,NZAD(1,I))-FK1).LE.TOL ) THEN
            SILE(ID(1,NZAD(1,I)))=1.D035*(FK1-TT1(ID(1,NZAD(1,I))))
cc         else
c proba sta raditi iznad
c            SILE(ID(1,NZAD(1,I)))=1.D035*(-TT1(ID(1,NZAD(1,I))))
cc         endif
        ENDIF
C===========================================================================
       ELSE
C===========================================================================
C KADA JE STRUJANJE STACIONARNO
C
C  ZA VDP TREBA OVAKO
C
        IF (NZAD(2,I).eq.1.OR.NZAD(2,I).EQ.6) THEN
         IF ( (ID(1,NZAD(1,I)).GT.0) .AND. (ITER.EQ.0) ) THEN
          SILE(ID(1,NZAD(1,I)))=1.0D35*ZADVRE(I)*FK1
c         ELSE 
c          SILE(ID(1,NZAD(1,I)))=0.D0
         ENDIF
        ENDIF
C ZPREMINSKI IZVOR, PONOR       
          IF ( (ID(1,NZAD(1,I)).GT.0) .AND. (NZAD(2,I).EQ.5)) THEN
!               WRITE(*,*) 'ZADVRE(',I,')',ZADVRE(I)
!               WRITE(*,*) 'FK1',FK1
              SILE(ID(1,NZAD(1,I)))=SILE(ID(1,NZAD(1,I)))+ZADVRE(I)*FK1
          ENDIF

C===========================================================================
       ENDIF
  410  CONTINUE
         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'vreme pre presen,2', (Dtime(i),i=5,7)

c         call wrr(SILE,JEDN,'s-p')
      CALL RESEN(A(LSK),SILE,MAXA,JEDN,2)
c         call wrr(SILE,JEDN,'s-k')
c     OVDE UCITATI RESENJA SA DISKA U VEKTOR SILE
c
  999 IF(NXNASTRAN.EQ.1) THEN
C
            NXNAS=99    
            OPEN (NXNAS,FILE='POTNXNAS',STATUS='OLD',FORM='FORMATTED',
     1            ACCESS='SEQUENTIAL')
            DO I=1,JEDN
               READ(NXNAS,*) IDUM,SILE(I)
               write(3,4000) IDUM,SILE(I)
 4000 format(i5,f15.5)               
               IF(IDUM.NE.I) STOP 'IDUM.NE.I  PAKV3D.FOR'
            ENDDO
            CLOSE(NXNAS,STATUS='KEEP')
      ENDIF

         CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'vreme posle presen,2', (Dtime(i),i=5,7)
C
           DO 440 I=1,JEDN
                TT1(I)=TT1(I)+SILE(I)
 440       CONTINUE
 
!       WRITE(*,*)'ITER= ',ITER
!       WRITE(*,*)'PERIOD= ',NNPER
!       WRITE(*,*)'STEP= ',KKORAK
      IF(NXNASTRAN.EQ.1) go to 491
       IF(INDFS.EQ.0) GO TO 491
C
      CALL KONVTF(TT1,SILE,KONVV2,1,ID,ITER)
C
        IF (KKORAK.GT.0) THEN
C
C         CALL CKONV1(ICUR,TT1,CORD,ID,KONV1,UBRZ)
c         proba da nasilno ne radi iteracije
cz          nasilu=0
cz          if(nstac.eq.1.and.iter.eq.0) nasilu=0
cz          write(*,*) ' nasilu',nasilu
cz          if(nasilu.eq.0) then
          IF (KONVV2*KONV1.EQ.0) THEN
            ITER=ITER+1
            WRITE(IIZLAZ,*)'ITER=',ITER
            GO TO 100
          ENDIF
          endif

        IF ((NJUTN.NE.0).AND.(INDOPT.EQ.0)) RETURN

 491     CALL DATE_AND_TIME(VALUES=Dtime)
         WRITE(*,*) 'vreme pre racunanja gradijenta', (Dtime(i),i=5,7)
C======================================================================= 
C RACUNANJE GRADIJENTA POTENCIJALA, BRZINA I ZAPREMINSKIH SILA
C=======================================================================
        DO JJ=1,NPT
          FZAPR(1,JJ)=0.D0
          FZAPR(2,JJ)=0.D0
          FZAPR(3,JJ)=0.D0
	ENDDO	
C
C RACUNANJE PROTOKA DUZ KONTURE UNUTAR MREZE /Sneza april 2013. racuna se iza izracunavanja protoka po konturi/
C
C      IF (NKONT.GT.0) THEN
C      CALL RKONT3(TT1,ID,KONT,NEL,AKONST,QUK,QUM,CORD,KKORAK)
C      ENDIF
C
C  PETLJA PO ELEMENTIMA
C
      DO 500 NBREL=1,NET
      IF(elemtip(NBREL).gt.100) THEN
        NTYPE=elemtip(NBREL)/100
        NTIMES=100
      ELSE
        NTYPE=elemtip(NBREL)/10
        NTIMES=10
      ENDIF
      NDIMEL=elemtip(NBREL)-NTIMES*NTYPE
      
      VZAPR=0.
      NMAT=NEL(NDIM+1,NBREL)
      NEMA=NEL(NDIM+2,NBREL)

      IF (NMAT.LE.0) GOTO 500
      IF (NEMA.GT.0) GOTO 500
 
      CALL CLEARD(TT21,NDIM)
      CALL PREB(TT21,TT1,ID,NEL,NBREL)
      CALL CLEARD(TT210,NDIM)
      CALL PREB(TT210,TT10,ID,NEL,NBREL)
C
      DO KLM=1,NDIMEL
       CK(1,KLM)=CORD(1,NEL(KLM,NBREL))
       CK(2,KLM)=CORD(2,NEL(KLM,NBREL))
       CK(3,KLM)=CORD(3,NEL(KLM,NBREL))
      ENDDO
      
      if(NTYPE.eq.2) CALL LCK2D(CK,CKL,TTE,NDIMEL)

      DO KL=1,NDIMEL
       FZAP(1,KL)=0.D0
       FZAP(2,KL)=0.D0
       FZAP(3,KL)=0.D0
      ENDDO
C      write(iizlaz,*)"pakv3d"
C
C INDIKATOR NJUTN-KOTESOVE INTEGRACIJE (0-NE,1-DA)
      KOTES=0
      NGAUS=0
      IBRGT=2
      IBRKT=2
      IF(KOTES.EQ.1) IBRKT=3
      
        IBRKT1=IBRKT
        IBRKT2=IBRKT
        if(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
          IBRKT2=1
          IBRKT1=4
        ENDIF
        IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) IBRKT2=1
        IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) IBRKT1=1
        IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
          IBRKT2=1
          IBRKT1=3
        ENDIF
        DO  320 I=1,IBRKT1
        DO  320 J=1,IBRKT2
        DO  320 K=1,IBRKT2
        NGAUS=NGAUS+1
        PORNIEL(NBREL,NGAUS)=0.D0
        
	IF(KOTES.EQ.1) THEN
          IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) then
            CALL GETRST(RT,ST,TT,WTET,NGAUS,IBRKT1)
            CALL INTERPTETRA(RT,ST,TT,0,0,NDIMEL)
            WDT=WTET(1)*WTET(2)*WTET(3)*DETJ
          ELSE IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
            CALL INTERP1D(RK(IBRKT1,I),0,0,NDIMEL,AJS)
            WDT=WK(IBRKT1,I)*thick(NBREL)*AJS
          ELSE IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) THEN
            CALL INTERP2D48(R(IBRGT,J),S(IBRGT,K),0,0,NDIMEL,CKL,TTE)
            WDT=W(IBRGT,J)*W(IBRGT,K)*DETJ*thick(NBREL)
          ELSE IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
            RT=1./6
            ST=1./6
            WT=1./3
            IF(NGAUS.EQ.2) RT=2./3
            IF(NGAUS.EQ.3) ST=2./3
            CALL INTERP2D36(RT,ST,0,0,NDIMEL,CKL,TTE)
            WWDT = WT*thick(NBREL)*DETJ/2
          ELSE
            CALL INTER3(RK(IBRGT,I),SK(IBRGT,J),TK(IBRGT,K),0,0)
            WDT=WK(IBRKT,I)*WK(IBRKT,J)*WK(IBRKT,K)*DETJ
          ENDIF
	ELSE
          IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) then
            CALL GETRST(RT,ST,TT,WTET,NGAUS,IBRKT1)
            CALL INTERPTETRA(RT,ST,TT,0,0,NDIMEL)
            WDT=WTET(1)*WTET(2)*WTET(3)*DETJ
          ELSE IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
            CALL INTERP1D(R(IBRKT1,I),0,0,NDIMEL,AJS)
            WDT=W(IBRKT1,I)*thick(NBREL)*AJS
          ELSE IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) THEN
!             CALL INTERP2D48(R(IBRGT,I),S(IBRGT,J),0,0,NDIMEL,CKL,TTE)
!             WDT=W(IBRGT,I)*W(IBRGT,J)*DETJ*thick(NBREL)
              CALL INTERP2D48(R(IBRGT,J),S(IBRGT,K),0,0,NDIMEL,CKL,TTE)
              WDT=W(IBRGT,J)*W(IBRGT,K)*DETJ*thick(NBREL)
          ELSE IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
            RT=1./6
            ST=1./6
            WT=1./3
            IF(NGAUS.EQ.2) RT=2./3
            IF(NGAUS.EQ.3) ST=2./3
            CALL INTERP2D36(RT,ST,0,0,NDIMEL,CKL,TTE)
            WDT = WT*thick(NBREL)*DETJ/2
          ELSE
            CALL INTER3(R(IBRGT,I),S(IBRGT,J),T(IBRGT,K),0,0)
            WDT=W(IBRKT,I)*W(IBRKT,J)*W(IBRKT,K)*DETJ
          ENDIF
	ENDIF
        VZAPR=VZAPR+WDT
C
C OKVASENOST U GAUS TACKI
        PR=0.D0
        DO II=1,NDIMEL
           PR=PR+H(II)*(TT21(II)-CORD(IOSA,NEL(II,NBREL)))
       ENDDO
        OKVAS=0.
        IF(PR.GT.0.D0) OKVAS=1.
c porni pritisak u gaus tacki
        porni=pr*gama
        IF(PR.GT.0.D0) PORNIEL(NBREL,NGAUS)=porni
C      write(iizlaz,*)NBREL,NGAUS,PORNIEL(NBREL,NGAUS)
C=======================================================================
C RACUNANJE GRADIJENTA POTENCIJALA:
C
      GRADFX=0.D0
      GRADFY=0.D0
      GRADFZ=0.D0
C ISKLJUCENO DA NE RACUNA FILTRACIONE SILE I SILE OD PORNIH PRITISAKA      
      IF(PR.GT.0.D0) THEN
C      IF(PR.GT.0.D0.OR.ISIL.EQ.0) THEN
C RACUNANJE GRADIJENTA POTENCIJALA
          DO II=1,NDIMEL
            GRADFX=GRADFX+ZVHX(II)*TT21(II)
            GRADFY=GRADFY+ZVHY(II)*TT21(II)
            GRADFZ=GRADFZ+ZVHZ(II)*TT21(II)
          ENDDO
C
C  GRADIJENT PRITISKA (FILTRACIONE SILE PO JEDINICI ZAPREMINE U GAUS TACKI)
C
        DO KL=1,NDIMEL
         IF (IFIL.EQ.1)THEN
           GRADPX=GAMA*GRADFX
           GRADPY=GAMA*GRADFY
           GRADPZ=GAMA*GRADFZ
C           WRITE(*,*) 'NBREL',NBREL
C           WRITE(*,*) 'GAMA',GAMA
C           WRITE(*,*) 'GRADPX',GRADPX
CC           WRITE(*,*) 'GRADPY',GRADPY
C           WRITE(*,*) 'GRADPZ',GRADPZ
C           WRITE(*,*) 'WDT',WDT
C           WRITE(*,*) 'H(',KL,')',H(KL)
C           IF(DABS(AKONST(3,5,NMAT)).LT.1.E-10) THEN
C                FZAP(1,KL)=FZAP(1,KL)-H(KL)*GRADPX*WDT
C                FZAP(2,KL)=FZAP(2,KL)-H(KL)*GRADPY*WDT
C                FZAP(3,KL)=FZAP(3,KL)-H(KL)*GRADPZ*WDT
C           ELSE
              FZAP(1,KL)=FZAP(1,KL)-H(KL)*GRADPX*WDT*AKONST(3,5,NMAT)
              FZAP(2,KL)=FZAP(2,KL)-H(KL)*GRADPY*WDT*AKONST(3,5,NMAT)
              FZAP(3,KL)=FZAP(3,KL)-H(KL)*GRADPZ*WDT*AKONST(3,5,NMAT)
C           ENDIF
         ELSEIF(IFIL.EQ.2)THEN
        FZAP(1,KL)=FZAP(1,KL)+ZVHX(KL)*PORNI*WDT*(1.D0-AKONST(3,5,NMAT))
        FZAP(2,KL)=FZAP(2,KL)+ZVHY(KL)*PORNI*WDT*(1.D0-AKONST(3,5,NMAT))
        FZAP(3,KL)=FZAP(3,KL)+ZVHZ(KL)*PORNI*WDT*(1.D0-AKONST(3,5,NMAT))
C PRITISAK OD SOPSTVENE TEZINE
C           GRADPX=GAMA*DELTA(IOSA,1)*OKVAS
C           GRADPY=GAMA*DELTA(IOSA,2)*OKVAS
C           GRADPZ=GAMA*DELTA(IOSA,3)*OKVAS
         ELSEIF(IFIL.EQ.0)THEN
           GRADPX=GAMA*GRADFX
           GRADPY=GAMA*GRADFY
           GRADPZ=GAMA*GRADFZ
C           IF(DABS(AKONST(3,5,NMAT)).LT.1.E-10) THEN
C              FZAP(1,KL)=FZAP(1,KL)-H(KL)*GRADPX*WDT
C              FZAP(2,KL)=FZAP(2,KL)-H(KL)*GRADPY*WDT
C              FZAP(3,KL)=FZAP(3,KL)-H(KL)*GRADPZ*WDT
C           ELSE
              FZAP(1,KL)=FZAP(1,KL)-H(KL)*GRADPX*WDT*AKONST(3,5,NMAT)
              FZAP(2,KL)=FZAP(2,KL)-H(KL)*GRADPY*WDT*AKONST(3,5,NMAT)
              FZAP(3,KL)=FZAP(3,KL)-H(KL)*GRADPZ*WDT*AKONST(3,5,NMAT)
C           ENDIF
        FZAP(1,KL)=FZAP(1,KL)+ZVHX(KL)*PORNI*WDT*(1.D0-AKONST(3,5,NMAT))
        FZAP(2,KL)=FZAP(2,KL)+ZVHY(KL)*PORNI*WDT*(1.D0-AKONST(3,5,NMAT))
        FZAP(3,KL)=FZAP(3,KL)+ZVHZ(KL)*PORNI*WDT*(1.D0-AKONST(3,5,NMAT))
        ENDIF
C        
         ENDDO
C         
      ENDIF

      IF ( (PR.LT.0.D0) .AND. (INDFS.EQ.1) )THEN
         AKX=(1.D0-ZASIC)*AKONST(3,1,NMAT)
         AKY=(1.D0-ZASIC)*AKONST(3,2,NMAT)
         AKZ=(1.D0-ZASIC)*AKONST(3,3,NMAT)
      ELSE
         AKX=AKONST(3,1,NMAT)
         AKY=AKONST(3,2,NMAT)
         AKZ=AKONST(3,3,NMAT)
      ENDIF

C GRADIJENT U GAUS TACKI
       AJX1(NGAUS)=GRADFX
       AJY1(NGAUS)=GRADFY
       AJZ1(NGAUS)=GRADFZ
       GG(1,NBREL,NGAUS)=GRADFX
       GG(2,NBREL,NGAUS)=GRADFY
       GG(3,NBREL,NGAUS)=GRADFZ
C BRZINA U GAUS TACKI
       AJX(NGAUS)=-AKX*GRADFX
       AJY(NGAUS)=-AKY*GRADFY
       AJZ(NGAUS)=-AKZ*GRADFZ
       VG(1,NBREL,NGAUS)=-AKX*GRADFX
       VG(2,NBREL,NGAUS)=-AKY*GRADFY
       VG(3,NBREL,NGAUS)=-AKZ*GRADFZ
C
  320   CONTINUE
!       CASE DEFAULT
!         WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
!       END SELECT
      
      IMCVOR=0
      IVAL=0
      SELECT CASE(elemtip(NBREL))
      CASE(12)
        IVAL=2
      CASE(13)
        IVAL=2
        IMCVOR=1
      CASE(23)
        IVAL=3
      CASE(26)
        IVAL=3
        IMCVOR=1
      CASE(24)
        IVAL=4
      CASE(28)
        IVAL=4
        IMCVOR=1
      CASE(34)
        IVAL=4
      CASE(38)
        IVAL=8
      CASE(310)
        IVAL=4
        IMCVOR=1
      CASE(320)
        IVAL=8
        IMCVOR=1
      CASE DEFAULT
!         WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
      END SELECT
C       
 	DO KK=1,NDIMEL
	  NCVOR=NEL(KK,NBREL)
	  KK1=NRED(KK)
	  SELECT CASE(elemtip(NBREL))
          CASE(12:13)
            KK1=NRED1(KK)
          CASE(23,26)
            KK1=NRED26(KK)
          CASE(24,28)
            KK1=NRED28(KK)
          CASE(34,310)
            KK1=NREDT10(KK)
          CASE DEFAULT
!          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
          END SELECT
	  IF(KOTES.EQ.1) KK1=NREDK(KK)
	  IF(KK.LE.IVAL.OR.IMCVOR.EQ.0) THEN
C BRZINE U CVOROVIMA
          VECTJ(1,NCVOR)=VECTJ(1,NCVOR)+AJX(KK1)/IVECT(NCVOR)
          VECTJ(2,NCVOR)=VECTJ(2,NCVOR)+AJY(KK1)/IVECT(NCVOR)
          VECTJ(3,NCVOR)=VECTJ(3,NCVOR)+AJZ(KK1)/IVECT(NCVOR)
C GRADIJENT POTENCIJALA U CVOROVIMA
	  GRADJN(1,NCVOR)=GRADJN(1,NCVOR)+AJX1(KK1)/IVECT(NCVOR)
	  GRADJN(2,NCVOR)=GRADJN(2,NCVOR)+AJY1(KK1)/IVECT(NCVOR)
	  GRADJN(3,NCVOR)=GRADJN(3,NCVOR)+AJZ1(KK1)/IVECT(NCVOR)
C FILTRACIONE SILE U CVOROVIMA	
          FZAPR(1,NCVOR)=FZAPR(1,NCVOR)+FZAP(1,KK)
          FZAPR(2,NCVOR)=FZAPR(2,NCVOR)+FZAP(2,KK)
          FZAPR(3,NCVOR)=FZAPR(3,NCVOR)+FZAP(3,KK)
c ovaj uslov nije jasan          
c          FZAPR(1,NCVOR)=FZAPR(1,NCVOR)+FZAP(1,KK)*8./VZAPR
c          FZAPR(2,NCVOR)=FZAPR(2,NCVOR)+FZAP(2,KK)*8./VZAPR
c          FZAPR(3,NCVOR)=FZAPR(3,NCVOR)+FZAP(3,KK)*8./VZAPR
          ELSE IF(KK.GT.IVAL.AND.IMCVOR.EQ.1) THEN
          KK2=KK1/10
          KK3=KK1-KK2*10
C BRZINE U CVOROVIMA
          VECTJ(1,NCVOR)=VECTJ(1,NCVOR)+(AJX(KK2)/IVECT(NCVOR))/2
     1                                 +(AJX(KK3)/IVECT(NCVOR))/2
          VECTJ(2,NCVOR)=VECTJ(2,NCVOR)+(AJY(KK2)/IVECT(NCVOR))/2
     1                                 +(AJY(KK3)/IVECT(NCVOR))/2
          VECTJ(3,NCVOR)=VECTJ(3,NCVOR)+(AJZ(KK2)/IVECT(NCVOR))/2
     1                                 +(AJZ(KK3)/IVECT(NCVOR))/2
C GRADIJENT POTENCIJALA U CVOROVIMA
          GRADJN(1,NCVOR)=GRADJN(1,NCVOR)+(AJX1(KK2)/IVECT(NCVOR))/2
     1                                   +(AJX1(KK3)/IVECT(NCVOR))/2
          GRADJN(2,NCVOR)=GRADJN(2,NCVOR)+(AJY1(KK2)/IVECT(NCVOR))/2
     1                                   +(AJY1(KK3)/IVECT(NCVOR))/2
          GRADJN(3,NCVOR)=GRADJN(3,NCVOR)+(AJZ1(KK2)/IVECT(NCVOR))/2
     1                                   +(AJZ1(KK3)/IVECT(NCVOR))/2
C FILTRACIONE SILE U CVOROVIMA  
          FZAPR(1,NCVOR)=FZAPR(1,NCVOR)+FZAP(1,KK)
          FZAPR(2,NCVOR)=FZAPR(2,NCVOR)+FZAP(2,KK)
          FZAPR(3,NCVOR)=FZAPR(3,NCVOR)+FZAP(3,KK)
          
          ENDIF
	ENDDO
C======================================================================= 
 500  CONTINUE
C
C
C======================================================================= 
 480   CALL PRKONT1(TT1,ID,UBRZ,CORD,NZAD,IOSA,IVECT,ZADVRE,KOJK)
C========================================================================
C RACUNANJE PROTOKA DUZ KONTURE UNUTAR MREZE
C
      IF (NKONT.GT.0) THEN
        CALL RKON3V(VECTJ,KONT,CORD,VG)
      ENDIF
C
C RACUNANJE PROTOKA DUZ KONTURE UNUTAR MREZE
C
      IF (NKONT.GT.0) THEN
C      CALL RKONT3(TT1,ID,KONT,NEL,AKONST,QUK,QUM,CORD,KKORAK)
C racuna gluposti za tetre - sneza
!        CALL RKONT3V(VG,KONT,QUK,QUM,CORD,KKORAK)
      ENDIF
C
C
       CALL DATE_AND_TIME(VALUES=Dtime)
       WRITE(*,*) 'vreme pre stampanja', (Dtime(i),i=5,7)
      WRITE(IIZLAZ,*)'PERIOD NUMBER,KKORAK ',NNPER, KKORAK         
      WRITE(*,*)'PERIOD NUMBER,KKORAK ',NNPER, KKORAK         
      IF (KKORAK.GT.0) THEN
! C Sneza 24.06.2011 dodato da stampa rezultate samo za odredjene cvorove
! C stampanje potencijala
!         CALL STAU09c(TT1,CORD,ID,NPT,47,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1,
!      1               NEL)
! C        CALL PRKONT1c(TT1,ID,UBRZ,CORD,NZAD,IOSA,IVECT,ZADVRE,KOJK)
! C stampanje dubine vode
! C       CALL STAU09c(TT1,CORD,ID,NPT,49,11,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
! C stampa rezultata u neu za sve cvorove
!        CALL STAS09(TT1,CORD,ID,NPT,49,10,1,KKORAK,NZAD,NUMZAD,KONT)
!        CALL STAU09(TT1,CORD,ID,NPT,49,9,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
!        CALL STAU09(TT1,CORD,ID,NPT,49,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
!        CALL STAU09(TT1,CORD,ID,NPT,49,11,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
!       CALL STAU09(VECTJ,CORD,ID,NPT,49,41,3,KKORAK,NZAD,NUMZAD,KONT,TT1)
!        CALL STAU09(GRADJN,CORD,ID,NPT,49,51,3,KKORAK,NZAD,NUMZAD,KONT,
!      1            TT1)
!       CALL STAU09(FZAPR,CORD,ID,NPT,49,61,3,KKORAK,NZAD,NUMZAD,KONT,TT1)
! C stampanje pornih pritisaka 
! !!! PREPRAVITI STAU35 da radi sa svim tipovima elemenata i onda odkomentarisati
!        CALL STAU35(PORNIEL,KKORAK)
! c stampanje zapreminskih sila
! C stampanje rezultata u svim cvorovima
!       CALL IZLL3D(ID,TT1,KKORAK,VECTJ,QUK,QUM,VVREME,FZAPR,VG,GG,
!      1            KOTES,IDJSTAMP1,NP3D1)
!         IF (INDSC.EQ.0) THEN
!       CALL STAG3D(TT1,ID,NASLOV,VVREME,KKORAK,1,NPT,18,0,NET,NEL,
!      1            KORAK,VECTJ,CORD,GRADJN,NZAD,FZAPR)
!         ENDIF
!       ENDIF
! C
! 
! 
C stampanje potencijala
      IF (INDSC.EQ.0.OR.INDSC.EQ.3.OR.INDSC.EQ.4) THEN
        CALL STAU09c(TT1,CORD,ID,NPT,47,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1,
     1               NEL)
C        CALL PRKONT1c(TT1,ID,UBRZ,CORD,NZAD,IOSA,IVECT,ZADVRE,KOJK)
C stampanje dubine vode
C       CALL STAU09c(TT1,CORD,ID,NPT,49,11,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
C stampa rezultata u neu za sve cvorove
       CALL STAS09(TT1,CORD,ID,NPT,49,10,1,KKORAK,NZAD,NUMZAD,KONT)
       CALL STAU09(TT1,CORD,ID,NPT,49,9,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
       CALL STAU09(TT1,CORD,ID,NPT,49,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
       CALL STAU09(TT1,CORD,ID,NPT,49,11,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
      CALL STAU09(VECTJ,CORD,ID,NPT,49,41,3,KKORAK,NZAD,NUMZAD,KONT,TT1)
       CALL STAU09(GRADJN,CORD,ID,NPT,49,51,3,KKORAK,NZAD,NUMZAD,KONT,
     1            TT1)
      CALL STAU09(FZAPR,CORD,ID,NPT,49,61,3,KKORAK,NZAD,NUMZAD,KONT,TT1)
C stampanje pornih pritisaka 
!!! PREPRAVITI STAU35 da radi sa svim tipovima elemenata i onda odkomentarisati
       CALL STAU35(PORNIEL,KKORAK)
c stampanje zapreminskih sila
C stampanje rezultata u svim cvorovima
      CALL IZLL3D(ID,TT1,KKORAK,VECTJ,QUK,QUM,VVREME,FZAPR,VG,GG,
     1            KOTES,IDJSTAMP1,NP3D1)
      ENDIF
        IF (INDSC.EQ.0) THEN
      CALL STAG3D(TT1,ID,NASLOV,VVREME,KKORAK,1,NPT,18,0,NET,NEL,
     1            KORAK,VECTJ,CORD,GRADJN,NZAD,FZAPR)
        ENDIF
      IF (INDSC.EQ.1.OR.INDSC.EQ.3) THEN
        CALL STAU09c(TT1,CORD,ID,NPT,47,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1,
     1               NEL)
      ENDIF
      IF(INDSC.EQ.2.OR.INDSC.EQ.4) THEN
        CALL STAU09CT(TT1,CORD,47,VVREME,KKORAK,NEL)
      ENDIF
      ENDIF
! 
! 
      IF(NXNASTRAN.EQ.1) RETURN
C
      KKORAK=KKORAK+1
c uslova da sledeci korak u tom periodu ne postoji i da ide na sledeci period
      IF (DABS(VREME(NNPER,KORAK+1)).LT.1.D-10) THEN
        GOTO 600
      ELSE
cz ide na sledeci korak      
        GOTO 35
      ENDIF
  600 CONTINUE
      IF(NSTAC.EQ.1.AND.KKORAK.EQ.2) 
     1  CALL IZBACI(TT1,CORD,ID,NPT,NEL,NET,IOSA,NDIM,IIZLAZ)
       CALL DATE_AND_TIME(VALUES=Dtime)
       WRITE(*,*) 'vreme posle stampanja', (Dtime(i),i=5,7)
      RETURN      
      END
C=======================================================================
C=======================================================================
      SUBROUTINE IZLL3D(ID,TT1,KORAK,VECTJ,QUK,QUM,VVREME,FZAPR,VG,GG,
     1                  KOTES,IDJSTAMP1,NP3D1)
      USE ELEMENTS
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /NDESUK/ NDES,IDPRIT,IFORM
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /PENALL/ PENALT,PRESS,IBRGT

      COMMON /SRPSKI/ ISRPS

      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /POCETN/ IPOCU,IPOCV,IPOCP,IPOCT,POCU,POCV,POCP,POCT,GAMA
      COMMON /DODAT/ NDIMM
      COMMON /ICITANJE/INPT
      COMMON /PRIKAZ/ INDSC,IZPOT
      COMMON /DJERDAP/ IDJERDAP

      DIMENSION D4(4),TT1(*),VECTJ(3,*),ID(1,*),N4(4)
      DIMENSION FZAPR(3,*),NRED(8),NREDK(8),NRED1(3)
      DIMENSION VG(3,NET,*),GG(3,NET,*),QUK(1000),QUM(1000),NREDT(4)
      DIMENSION NREDT10(10),VGT(3),NRED26(6),NRED28(8)
      DATA NRED/8,4,2,6,7,3,1,5/  
      DATA NREDK/27,9,3,21,25,7,1,19/
      DATA NRED1/2,1,3/
      DATA NRED26/1,2,3,12,23,31/
      DATA NRED28/1,2,3,4,12,23,34,41/
      DATA NREDT/1,2,3,4/
      DATA NREDT10/1,2,3,4,12,23,13,14,24,34/
      DIMENSION IDJSTAMP1(5,*),NP3D1(*)
C
      if (indsc.eq.0) then
C      
CS  POTENCIJAL U CVOROVIMA 
CE  NODE POTENTIAL 
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2001)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6001)
      WRITE (IIZLAZ,*)'STEP NUMBER  ',KORAK

      KG = 0
   20 DO 50 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 100
      N4(I) = KG
      JJEDN=ID(1,KG)
      IF(JJEDN.EQ.0) THEN
        D4(I) = 0.
      ELSE
        D4(I) = TT1(JJEDN)
      ENDIF
   50 CONTINUE
C
  100 I = I - 1
      IF(IFORM.EQ.1) GO TO 88
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 77
C
   88 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
   77 IF(KG.LT.NPT) THEN
      GO TO 20
      ENDIF
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2028)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6028)
C
CS  GUSTINE STRUJE U PRAVCU X1     
CE  CURRENT DESNITIES IN DIRECTION X1
C

      KG = 0
  620 DO 650 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 1600
      N4(I) = KG
         D4(I) = VECTJ(1,KG)
  650 CONTINUE
C
 1600 I = I - 1
      IF(IFORM.EQ.1) GO TO 688
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 677
C
  688 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
  677 IF(KG.LT.NPT) THEN
      GO TO 620
      ENDIF

      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2039)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6039)
C
CS  GUSTINE STRUJE U PRAVCU X2     
CE  CURRENT DESNITIES IN DIRECTION X2
C

      KG = 0
  720 DO 750 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 1700
      N4(I) = KG
         D4(I) = VECTJ(2,KG)
  750 CONTINUE
C
 1700 I = I - 1
      IF(IFORM.EQ.1) GO TO 788
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 777
C
  788 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
  777 IF(KG.LT.NPT) THEN
      GO TO 720
      ENDIF
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2049)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6049)
C
CS  GUSTINE STRUJE U PRAVCU X3     
CE  CURRENT DESNITIES IN DIRECTION X3
C

      KG = 0
  820 DO 850 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 1800
      N4(I) = KG
         D4(I) = VECTJ(3,KG)
  850 CONTINUE
C
 1800 I = I - 1
      IF(IFORM.EQ.1) GO TO 888
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 877
C
  888 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
  877 IF(KG.LT.NPT) THEN
      GO TO 820
      ENDIF

C
CS  BRZINE
C
      NGAUS=IBRGT*IBRGT*IBRGT
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2059)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6059)
      IF (INDSC.EQ.0)THEN
       DO I=1,NET
         IF(elemtip(I).gt.100) THEN
          NTYPE=elemtip(I)/100
          NTIMES=100
        ELSE
          NTYPE=elemtip(I)/10
          NTIMES=10
        ENDIF
        NDIMEL=elemtip(I)-NTIMES*NTYPE
         WRITE(IIZLAZ,5000) I
       DO J=1,NDIMEL
         NC=NEL(J,I)
         NG=NRED(J)
         SELECT CASE(elemtip(I))
          CASE(12:13)
            NG=NRED1(J)
          CASE(23:26)
            NG=NRED26(J)
          CASE(34,310)
            NG=NREDT10(J)
          CASE DEFAULT
!          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
         END SELECT
!          IF(NDIMEL.eq.2) NG=NRED1(J)
!          IF(NDIMEL.eq.10) NG=NREDT10(J)
	 IF(KOTES.EQ.1) NG=NREDK(J)
	     IF(INPT.EQ.1) THEN
              IF(NG.GT.10) THEN
                NG1=NG/10
                NG2=NG-NG1*10
                DO K=1,3
                  VGT(K)=(VG(K,I,NG1)+(VG(K,I,NG2)))/2
                ENDDO
                WRITE(IIZLAZ,5017) NC,(VGT(K),K=1,3)
              ELSE
                WRITE(IIZLAZ,5017) NC,(VG(K,I,NG),K=1,3)
              ENDIF
	     ELSE
              IF(NG.GT.10) THEN
                NG1=NG/10
                NG2=NG-NG1*10
                DO K=1,3
                  VGT(K)=(VG(K,I,NG1)+(VG(K,I,NG2)))/2
                ENDDO
                WRITE(IIZLAZ,5007) NC,(VGT(K),K=1,3)
              ELSE
                WRITE(IIZLAZ,5007) NC,(VG(K,I,NG),K=1,3)
              ENDIF
	     ENDIF
       ENDDO
       ENDDO
      ENDIF
C
CS  GRADIJENTI
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2069)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6069)
      IF (INDSC.EQ.0)THEN
      DO I=1,NET
         IF(elemtip(I).gt.100) THEN
          NTYPE=elemtip(I)/100
          NTIMES=100
        ELSE
          NTYPE=elemtip(I)/10
          NTIMES=10
        ENDIF
        NDIMEL=elemtip(I)-NTIMES*NTYPE
         WRITE(IIZLAZ,5000) I
        DO J=1,NDIMEL
         NC=NEL(J,I)
         NG=NRED(J)
         SELECT CASE(elemtip(I))
          CASE(12:13)
            NG=NRED1(J)
          CASE(23:26)
            NG=NRED26(J)
          CASE(34,310)
            NG=NREDT10(J)
          CASE DEFAULT
!          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
         END SELECT
         IF(NDIMEL.eq.2) NG=NRED1(J)
         IF(NDIMEL.eq.10) NG=NREDT10(J)
	   IF(KOTES.EQ.1) NG=NREDK(J)
	   IF(INPT.EQ.1) THEN
                IF(NG.GT.10) THEN
                  NG1=NG/10
                  NG2=NG-NG1*10
                  DO K=1,3
                    VGT(K)=(GG(K,I,NG1)+(GG(K,I,NG2)))/2
                  ENDDO
                  WRITE(IIZLAZ,5017) NC,(VGT(K),K=1,3)
                 ELSE
                  WRITE(IIZLAZ,5017) NC,(GG(K,I,NG),K=1,3)
	         ENDIF
	   ELSE
	     IF(NG.GT.10) THEN
                NG1=NG/10
                NG2=NG-NG1*10
                DO K=1,3
                  VGT(K)=(GG(K,I,NG1)+(GG(K,I,NG2)))/2
                ENDDO
                WRITE(IIZLAZ,5007) NC,(VGT(K),K=1,3)
             ELSE
                WRITE(IIZLAZ,5007) NC,(GG(K,I,NG),K=1,3)
             ENDIF
	   ENDIF
        ENDDO
      ENDDO
      ENDIF
c
      endif      
C
CS  ZAPREMINSKE SILE
CE  BODY FORCES
C
C	WRITE(25,*)'C  N,    FX,         FY,      FZ'
!         write(*,*) 'IZLST, IDJERDAP, NP3D1',IDJERDAP,NP3D1(1)
      IF (GAMA.GT.0.D0) THEN
!         IF(IDJERDAP.EQ.1) WRITE(25,5017) NP3D1(1)
! 	    DO NN=1,NPT
! 	     IF(IDJERDAP.EQ.1) THEN
! 	        IF(IDJSTAMP1(NN).EQ.1) THEN
! 	           IF(INPT.EQ.1) THEN
!                 WRITE(25,5017) NN,(FZAPR(KK,NN),KK=1,3)
! 	           ELSE
!                 WRITE(25,5007) NN,(FZAPR(KK,NN),KK=1,3)
! 	           ENDIF
! 	        ENDIF 
! 	     ELSE
! 	        IF(INPT.EQ.1) THEN
!              WRITE(25,5017) NN,(FZAPR(KK,NN),KK=1,3)
! 	        ELSE
!              WRITE(25,5007) NN,(FZAPR(KK,NN),KK=1,3)
! 	        ENDIF
! 	     ENDIF
! 	    ENDDO
           IF(IDJERDAP.EQ.1) THEN
             IFILE=105
             DO IK=1,5
               IFILE=IFILE+1
               WRITE(IFILE,5017) NP3D1(IK)
               DO NN=1,NPT
	        IF(IDJSTAMP1(IK,NN).EQ.IK) THEN
	           IF(INPT.EQ.1) THEN
                WRITE(IFILE,5017) NN,(FZAPR(KK,NN),KK=1,3)
	           ELSE
                WRITE(IFILE,5007) NN,(FZAPR(KK,NN),KK=1,3)
	           ENDIF
	        ENDIF 
	       ENDDO
             ENDDO
           ELSE
             DO NN=1,NPT
               IF(INPT.EQ.1) THEN
             WRITE(25,5017) NN,(FZAPR(KK,NN),KK=1,3)
	        ELSE
             WRITE(25,5007) NN,(FZAPR(KK,NN),KK=1,3)
	        ENDIF
             ENDDO
           ENDIF
!            DO NN=1,NPT
! 	     IF(IDJERDAP.EQ.1) THEN
! 	        IF(IDJSTAMP1(NN).EQ.5) THEN
! 	           IF(INPT.EQ.1) THEN
!                 WRITE(25,5017) NN,(FZAPR(KK,NN),KK=1,3)
! 	           ELSE
!                 WRITE(25,5007) NN,(FZAPR(KK,NN),KK=1,3)
! 	           ENDIF
! 	        ENDIF 
! 	     ELSE
! 	        IF(INPT.EQ.1) THEN
!              WRITE(25,5017) NN,(FZAPR(KK,NN),KK=1,3)
! 	        ELSE
!              WRITE(25,5007) NN,(FZAPR(KK,NN),KK=1,3)
! 	        ENDIF
! 	     ENDIF
! 	  ENDDO
      ENDIF
C
CS  PROTOK KROZ KONTURU     
CE  CURRENT FLUX WITHIN CONTOUR
C
      IF(NKONT.GT.0) THEN
       QK=0.
       QM=0.
       DO L=1,NKONT
          QK=QK+QUK(L)
          QM=QM+QUM(L) 
       ENDDO
       IF(INDSC.EQ.0) THEN
        WRITE(IIZLAZ,7023) VVREME
        DO L=1,NKONT
         WRITE(IIZLAZ,5007) L,QUK(L),QUM(L),QUK(L)+QUM(L)
        ENDDO
        WRITE(IIZLAZ,5008) QK,QM,QK+QM
        WRITE(21,7023) VVREME
        DO L=1,NKONT
         WRITE(21,5007) L,QUK(L),QUM(L),QUK(L)+QUM(L)
        ENDDO 
        WRITE(21,5008) QK,QM,QK+QM
       ENDIF
      ENDIF
C
      RETURN
 5000 FORMAT(/' ELEMENT',I10)     
 5002 FORMAT(4(I6,1PD13.5))
 5003 FORMAT(4(I6,3X,F10.3))
 5005 FORMAT(F10.3,10(1PE13.5))
 5007 FORMAT(I6,3(1PE13.5))
 5008 FORMAT('UKUPNO',3(1PE13.5))
 5012 FORMAT(4(I10,1PD13.5))
 5013 FORMAT(4(I10,3X,F10.3))
 5017 FORMAT(I10,3(1PE13.5))
 2001 FORMAT(/'    U K U P N I    P O T E N C I J A L                  '
     1,'             '//
     1' CVOR  POMERANJE   CVOR  POMERANJE   CVOR  POMERANJE   CVOR POMER 
     1NJE   '/' BROJ',3(14X,'BROJ'))
 2028 FORMAT(/'     C V O R N E   B R Z I N E   U   X 1  P R A V C U '//
     1' CVOR    BRZINA    CVOR    BRZINA    CVOR    BRZINA    CVOR   BRZ       
     1INA   '/' BROJ',3(14X,'BROJ'))
 2039 FORMAT(/'     C V O R N E   B R Z I N E   U   X 2  P R A V C U '//
     1' CVOR    BRZINA    CVOR    BRZINA    CVOR    BRZINA    CVOR   BRZ       
     1INA   '/' BROJ',3(14X,'BROJ'))
 2049 FORMAT(/'     C V O R N E   B R Z I N E   U   X 3  P R A V C U '//
     1' CVOR    BRZINA    CVOR    BRZINA    CVOR    BRZINA    CVOR   BRZ       
     1INA   '/' BROJ',3(14X,'BROJ'))
 2059 FORMAT(//'     C V O R N E   B R Z I N E'/
     1' CVOR    BRZINA VX    BRZINA VY    BRZINA VZ')
 2069 FORMAT(//'     C V O R N I   G R A D I J E N T I'/
     1' CVOR GRADIJENT GX GRADIJENT GY GRADIJENT GZ')
 6001 FORMAT(/'    N O D A L    T O T A L     P O T E N C I A L        '
     1,'                 '//
     1' NODE  DISPLAC.    NODE  DISPLAC.    NODE  DISPLAC.    NODE DISPL
     1AC.   '/'  No.',3(15X,'No.'))
 6028 FORMAT(/'    N O D A L    V E L O C I T I E S   I N   D I R E C T'
     1,' I O N   X 1 '//
     1' NODE  VELOCITY    NODE  VELOCITY    NODE  VELOCITY    NODE VELOC
     1ITY   '/'  No.',3(15X,'No.'))
 6039 FORMAT(/'    N O D A L    V E L O C I T I E S   I N   D I R E C T'
     1,' I O N   X 2 '//
     1' NODE  VELOCITY    NODE  VELOCITY    NODE  VELOCITY    NODE VELOC
     1ITY   '/'  No.',3(15X,'No.'))
 6049 FORMAT(/'    N O D A L    V E L O C I T I E S   I N   D I R E C T'
     1,' I O N   X 3 '//
     1' NODE  VELOCITY    NODE  VELOCITY    NODE  VELOCITY    NODE VELOC
     1ITY   '/'  No.',3(15X,'No.'))
 6059 FORMAT(//'    N O D A L    V E L O C I T I E S'/
     1' NODE  VELOCITY VX  VELOCITY VY  VELOCITY VZ')
 6069 FORMAT(//'    N O D A L    G R A D I E N T S  '/
     1' NODE  GRADIENT GX  GRADIENT GY  GRADIENT GZ')
 7023 FORMAT(//'  F L U X   W I T H I N  C O N T O U R, TIME =',1PD13.5/
     1' KONTURA     ULAZNI       IZLAZNI      UKUPNI PROTOK-1')
      END
C=======================================================================
C
C RACUNANJE PROTOKA DUZ KONTURE ZA 3D ELEMENTE
C
C=========================================================================
      SUBROUTINE RKONT3(TT1,ID,KONT,AKONST,QUK,QUM,CORD,KOR)
      USE ELEMENTS
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /TRENT3/ ZVHX(21),ZVHY(21),ZVHZ(21),CK(3,21),H(21),
     1 FS2,DETJS,DETJ,NBREL
      COMMON /STAC/ NSTAC,NSLP,IBROJ
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /DODAT/ NDIMM
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
C
      DIMENSION R(3,3),S(3,3),T(3,3),W(3,3)
      DIMENSION TT21(44)
C
      DIMENSION KONT(9,MAXLIN,*),ID(1,*)
      DIMENSION AFIFI(21,21),AM1(21,21),AK(21,21),TT1(*),CORD(3,*)
      DIMENSION AKONST(3,5,*),QPR(21),QUK(1000),QUM(1000)
      DIMENSION A12(3),A14(3),DN(3)
C
C*    BLOK ZA STAMPANJE PROTOKA PO ELEMENTIMA ZA GRAFIKU - PRIVREMENO
      JEDAN=1
      NULA=0
      ZERO=0.
      MJ=-1
      M8=8
C
      IN=15
      IGRAF=48
      WRITE(IGRAF,1000) KOR,IN,JEDAN
      WRITE(IGRAF,3003) 
      WRITE(IGRAF,1010) ZERO,ZERO,ZERO
      WRITE(IGRAF,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(IGRAF,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
      WRITE(IGRAF,1000) NULA,NULA,M8,M8
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      WRITE(IGRAF,1000) JEDAN,NULA,JEDAN
 1000 FORMAT(10I5)
 1010 FORMAT(3(1PD12.4))
 3003 FORMAT(' PROTOK PO JEDINICI POVRSINE')
C*
      R(3,1)=-0.7745966692415
      R(3,2)=0.0
      R(3,3)=0.77459666924148
      S(3,1)=-0.7745966692415
      S(3,2)=0.0
      S(3,3)=0.77459666924148
      T(3,1)=-0.7745966692415
      T(3,2)=0.0
      T(3,3)=0.77459666924148
      W(3,1)=0.55555555555556
      W(3,2)=0.88888888888889
      W(3,3)=0.55555555555556


      R(2,1)=-0.57735026918963
      R(2,2)=0.5773502691896
      S(2,1)=-0.57735026918963
      S(2,2)=0.5773502691896
      T(2,1)=-0.57735026918963
      T(2,2)=0.5773502691896
      W(2,1)=1.000000000000000
      W(2,2)=1.000000000000000

      R(1,1)=0.0
      S(1,1)=0.0
      T(1,1)=0.0
      W(1,1)=2.0
C
! C ODEDIVANJE BROJA GAUSOVIH TACAKA PRILIKOM INTEGRACIJE
!       IBRGT=3
!       IF (NDIM.EQ.8) IBRGT=2
C
      NLM=0
      DO 723 KK=1,NKONT
        QUK(KK)=0.D0
        QUM(KK)=0.D0
C
	DO 134 II=1,LIN(KK)
	NLM=NLM+1
C	
      NBREL=KONT(1,II,KK)
      NMAT=NEL(NDIM+1,NBREL)
      NEMA=NEL(NDIM+2,NBREL)
      
      IF(elemtip(NBREL).gt.100) THEN
        NTYPE=elemtip(NBREL)/100
        NTIMES=100
      ELSE
        NTYPE=elemtip(NBREL)/10
        NTIMES=10
      ENDIF
      NDIMEL=elemtip(NBREL)-NTIMES*NTYPE
C ODEDIVANJE BROJA GAUSOVIH TACAKA PRILIKOM INTEGRACIJE
      IBRGT=3
      IF (NDIMEL.LE.8) IBRGT=2
      IBRGT1=IBRGT
      IF(elemtip(NBREL).eq.310.or.elemtip(NBREL).eq.34) then
        IBRGT=4
        IBRGT1=1
      endif

      IF (NMAT.LE.0) GOTO 134
      IF (NEMA.GT.0) GOTO 134
C
      CALL CLEARD(TT21,NDIM)
      CALL PREB(TT21,TT1,ID,NEL,NBREL)
C
      DO KLM=1,NDIM
       CK(1,KLM)=CORD(1,NEL(KLM,NBREL))
       CK(2,KLM)=CORD(2,NEL(KLM,NBREL))
       CK(3,KLM)=CORD(3,NEL(KLM,NBREL))
      ENDDO
C
        ICV1=KONT(2,II,KK)
        ICV2=KONT(3,II,KK)
        ICV3=KONT(4,II,KK)
        ICV4=KONT(5,II,KK)
        ICV5=KONT(6,II,KK)
        ICV6=KONT(7,II,KK)
        ICV7=KONT(8,II,KK)
        ICV8=KONT(9,II,KK)
C
      DO 163 K=1,NDIMEL
      DO 162 N=1,NDIMEL
         AM1(K,N)=0.D0    
         AFIFI(K,N)=0.D0    
  162 CONTINUE
  163 CONTINUE
C
      DO 180 I=1,IBRGT
      DO 170 J=1,IBRGT
      DO 160 L=1,IBRGT
C     
      CALL INTER3(R(IBRGT,I),S(IBRGT,J),T(IBRGT,L),0,0)
C  
      WDT=W(IBRGT,I)*W(IBRGT,J)*W(IBRGT,L)*DETJ
C
C OKVASENOST U GAUS TACKI
        PR=0.D0
        DO IK=1,NDIMEL
           PR=PR+H(IK)*(TT21(IK)-CORD(IOSA,NEL(IK,NBREL)))
        ENDDO
      IF ( PR.LT.0.D0 .AND. INDFS.EQ.1 )THEN
         AKX=(1.D0-ZASIC)*AKONST(3,1,NMAT)
         AKY=(1.D0-ZASIC)*AKONST(3,2,NMAT)
         AKZ=(1.D0-ZASIC)*AKONST(3,3,NMAT)
         AKS=(1.D0-ZASIC)*AKONST(3,4,NMAT)
      ELSE
         AKX=AKONST(3,1,NMAT)
         AKY=AKONST(3,2,NMAT)
         AKZ=AKONST(3,3,NMAT)
         AKS=AKONST(3,4,NMAT)
      ENDIF
C
      DO 165 K=1,NDIMEL
      DO 164 N=1,NDIMEL
C
         AM1(K,N)=AM1(K,N)+AKS*H(K)*H(N)*WDT
         AFIFI(K,N)=AFIFI(K,N)+(AKX*ZVHX(K)*ZVHX(N)+
     1                      AKY*ZVHY(K)*ZVHY(N)+AKZ*ZVHZ(K)*ZVHZ(N))*WDT
C
  164 CONTINUE
  165 CONTINUE
C
  160 CONTINUE
  170 CONTINUE
  180 CONTINUE
C
      DO I=1,NDIMEL
      DO J=1,NDIMEL
        AK(I,J)=AFIFI(I,J)
      ENDDO
      ENDDO
C
      DO IK=1,NDIMEL
        QPR(IK)=0.D0
        DO J=1,NDIMEL
          if((TT21(IK)-CORD(IOSA,NEL(IK,NBREL))).gt.0)then
              QPR(IK)=QPR(IK)+AK(IK,J)*TT21(J)
          endif
        ENDDO
      ENDDO
C
      QPRI=0.
      QPRU=0.
      DO J=1,NDIMEL
!         IF(NEL(J,NBREL).EQ.ICV1.OR.NEL(J,NBREL).EQ.ICV2.
!      &     OR.NEL(J,NBREL).EQ.ICV3.OR.NEL(J,NBREL).EQ.ICV4) THEN
           IF(QPR(J).LT.0.) THEN
              QPRI=QPRI+QPR(J)
           ELSE
              QPRU=QPRU+QPR(J)
           ENDIF
!         ENDIF
      ENDDO
      
      QUM(KK)=QUM(KK)+QPRI
      QUK(KK)=QUK(KK)+QPRU
      
C*    STAMPANJE PROTOKA PO ELEMENTU
      IF(ICV4.EQ.0) THEN
        DO I=1,3
          A12(I)=CORD(I,ICV2)-CORD(I,ICV1)
          A14(I)=CORD(I,ICV3)-CORD(I,ICV1)
        ENDDO
      ELSE
        DO I=1,3
          A12(I)=CORD(I,ICV2)-CORD(I,ICV1)
          A14(I)=CORD(I,ICV4)-CORD(I,ICV1)
        ENDDO
      ENDIF
C     DODATAK ZA PROTOK PO JEDINICI POVRSINE      
      CALL AXBV(A12,A14,DN)
      DUM=DSQRT(DN(1)*DN(1)+DN(2)*DN(2)+DN(3)*DN(3))
      QPRUK= -QPRI/DUM
C      QPRUK= (QPRI+QPRU)/DUM
C      
      WRITE(IGRAF,5000) NLM,QPRUK
C*
  134 CONTINUE
C
  723 CONTINUE
C
C*    KRAJ BLOKA ZA STAMPANJE PROTOKA PO ELEMENTU
      WRITE(IGRAF,5000) MJ,ZERO
 5000 FORMAT(I5,3(1PD12.4))
C*
      RETURN
      END
C=======================================================================
      SUBROUTINE REDOSLED(NRED,NREDEL,NDIMEL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION NRED(*),NREDEL(*)
      
      CALL ICLEAR(NRED,20)
      DO I=1,NDIMEL
        NRED(I)=NREDEL(I)
      ENDDO
      
      RETURN
      END
C=========================================================================
C
C RACUNANJE PROTOKA DUZ KONTURE UNUTAR MREZE
C
C=========================================================================
      SUBROUTINE RKON3V(VECTJ,KONT,CORD,VG)
      USE ELEMENTS
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /STAC/ NSTAC,NSLP,IBROJ
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /DODAT/ NDIMM
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /PRIKAZ/ INDSC,IZPOT
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
C
      DIMENSION R42(3),R13(3),R12(3),EN(3),EN1(3),V(3)
C
      DIMENSION KONT(9,MAXLIN,*)
      DIMENSION VG(3,NET,*)
      DIMENSION VECTJ(3,*),CORD(3,*)
      DIMENSION QUK(1000),QUM(1000),NRED(10),NRED26(6),NRED28(8)
      DIMENSION NRED8(8),NRED1(3),NREDT4(4),NREDT10(10)
      DATA NRED8/8,4,2,6,7,3,1,5/
      DATA NRED1/2,1,12/
      DATA NRED26/1,2,3,12,23,31/
      DATA NRED28/1,2,3,4,12,23,34,41/
      DATA NREDT4/1,2,3,4/
      DATA NREDT10/1,2,3,4,12,23,13,14,24,34/
C
      ONE=1.
      PI=4.*DATAN(ONE)
C
      DO 723 KK=1,NKONT
        QUK(KK)=0.D0
        QUM(KK)=0.D0
C
	DO 134 II=1,LIN(KK)
C
          NBREL=KONT(1,II,KK)
          NMAT=NEL(NDIM+1,NBREL)
          NEMA=NEL(NDIM+2,NBREL)
          
          IF(elemtip(NBREL).gt.100) THEN
            NTYPE=elemtip(NBREL)/100
            NTIMES=100
          ELSE
            NTYPE=elemtip(NBREL)/10
            NTIMES=10
          ENDIF
          NDIMEL=elemtip(NBREL)-NTIMES*NTYPE
          
          SELECT CASE(elemtip(NBREL))   
          CASE(12)
            NSCV=2
            CALL REDOSLED(NRED,NRED1,NDIMEL)
          CASE(13)
            NSCV=3
            CALL REDOSLED(NRED,NRED1,NDIMEL)
          CASE(23)
            NSCV=2
            CALL REDOSLED(NRED,NRED26,NDIMEL)
          CASE(26)
            NSCV=3
            CALL REDOSLED(NRED,NRED26,NDIMEL)
          CASE(24)
            NSCV=2
            CALL REDOSLED(NRED,NRED28,NDIMEL)
          CASE(28)
            NSCV=3
            CALL REDOSLED(NRED,NRED28,NDIMEL)
          CASE(34)
            NSCV=3
            CALL REDOSLED(NRED,NREDT10,NDIMEL)
          CASE(310)
            NSCV=6
            CALL REDOSLED(NRED,NREDT10,NDIMEL)
          CASE(38)
            NSCV=4
            CALL REDOSLED(NRED,NRED8,NDIMEL)
          CASE DEFAULT
            WRITE(*,*) 'GRESKA, NEMA PRIKAZA ZA DATI ELEMENT'
          END SELECT
          
          IF (NMAT.LE.0) GOTO 134
          IF (NEMA.GT.0) GOTO 134
C
	  ICV1=KONT(2,II,KK)
	  ICV2=KONT(3,II,KK)
	  ICV3=KONT(4,II,KK)
	  ICV4=KONT(5,II,KK)   
	  ICV5=KONT(6,II,KK)
          ICV6=KONT(7,II,KK)
          ICV7=KONT(8,II,KK)
          ICV8=KONT(9,II,KK)
          IC1=0
          IC2=0
          IC3=0
          IC31=1
          IC32=1
          IC33=1
          IC34=1
          IC4=1
          IC41=0
          IC5=0
          IC51=1
          IC52=1
          IC6=0
          IC61=1
          IC62=1
          IC7=0
          IC71=1
          IC72=1
          IC8=0
!  za 1d elemente ucitava elemente i cvor za koji se racuna protok          
!            write(*,*) "NBREL",NBREL
!              write(*,*) "ICV1",ICV1
!             write(*,*) "ICV2",ICV2
!             write(*,*) "ICV3",ICV3
!           write(*,*) "ICV4",ICV4
!           write(*,*) "ICV5",ICV5
!           write(*,*) "ICV6",ICV6
!           write(*,*) "ICV7",ICV7
!           write(*,*) "ICV8",ICV8IC5
          DO KLM=1,NDIMEL
!             write(*,*) "NEL(",KLM,",",NBREL,")",NEL(KLM,NBREL)
            IF(ICV1.EQ.NEL(KLM,NBREL)) then
              IC1=KLM
              IF(NTYPE.EQ.1) go to 99 
            ENDIF
            IF(ICV2.EQ.NEL(KLM,NBREL)) IC2=KLM
            IF(ICV3.EQ.NEL(KLM,NBREL)) THEN
!               WRITE(*,*) 'elemtip(',NBREL,')',elemtip(NBREL)
!               IF(elemtip(NBREL).eq.13) THEN
!                 IC31=NRED(KLM)/10
!                 IC32=NRED(KLM)-IC31*10
!                 IC33=1
!                 IC34=2
!                 IC3=1
!              ELSEIF(elemtip(NBREL).eq.26.and.ICV6.eq.0) THEN
             IF(elemtip(NBREL).eq.26.and.ICV6.eq.0) THEN
                IC31=NRED(KLM)/10
                IC32=NRED(KLM)-IC31*10
                IC33=1
                IC34=2
                IC3=1
!             write(*,*) 'el',II,NBREL
             ELSE
                IC31=KLM
                IC32=1
                IC33=0
                IC34=1
                IC3=1
              ENDIF
            ENDIF
            IF(ICV4.EQ.NEL(KLM,NBREL)) THEN
              IC41=1
              IC4=KLM
            ENDIF
            IF(ICV5.EQ.NEL(KLM,NBREL)) THEN
              IC51=NRED(KLM)/10
              IC52=NRED(KLM)-IC51*10
              IC5=1
            ENDIF
            IF(ICV6.EQ.NEL(KLM,NBREL)) THEN
              IC61=NRED(KLM)/10
              IC62=NRED(KLM)-IC61*10
              IC6=1
            ENDIF
            IF(ICV7.EQ.NEL(KLM,NBREL)) THEN
              IC71=NRED(KLM)/10
              IC72=NRED(KLM)-IC71*10
              IC7=1
            ENDIF
            IF(ICV8.EQ.NEL(KLM,NBREL)) IC8=KLM
          ENDDO
 99       IF(NTYPE.EQ.1) THEN
            ICV1=NEL(1,NBREL)
            ICV2=NEL(2,NBREL)
            ICV3=NEL(3,NBREL)
          ELSEIF(NTYPE.EQ.2) THEN
            IF(IC1.EQ.1.AND.IC2.EQ.2) ICV3=NEL(3,NBREL)
            IF(IC1.EQ.2.AND.IC2.EQ.3) ICV3=NEL(1,NBREL)
            IF(IC1.EQ.3.AND.IC2.EQ.1) ICV3=NEL(2,NBREL)
          ENDIF
!              write(*,*) "ICV1,ICV2,ICV3",ICV1,ICV2,ICV3

 100      IF(NTYPE.LE.2.AND.IC5.EQ.0) THEN
            R12(1)=CORD(1,ICV1)-CORD(1,ICV2)
            R12(2)=CORD(2,ICV1)-CORD(2,ICV2)
            R12(3)=CORD(3,ICV1)-CORD(3,ICV2)
            RI=DSQRT(R12(1)*R12(1)+R12(2)*R12(2)+R12(3)*R12(3))
            EN(1)=R12(1)/RI
            EN(2)=R12(2)/RI
            EN(3)=R12(3)/RI
            IF(NTYPE.EQ.1) go to 200
!             GOTO 131
          ENDIF
C         
          IF(NSCV.eq.4) THEN
            DO I=1,3
              R13(I)=CORD(I,ICV3)-CORD(I,ICV1)
              R42(I)=CORD(I,ICV2)-CORD(I,ICV4)
            ENDDO
          ELSE
            DO I=1,3
              R13(I)=CORD(I,ICV3)-CORD(I,ICV1)
              R42(I)=CORD(I,ICV2)-CORD(I,ICV1)
            ENDDO
          ENDIF

          IF(NTYPE.EQ.2) THEN
           CALL AXBV(R42,R13,EN1)  
           CALL AXBV(R42,EN1,EN)  
          ELSE
           CALL AXBV(R42,R13,EN)  
          ENDIF
          AI=DSQRT(EN(1)**2+EN(2)**2+EN(3)**2)
          EN(1)=EN(1)/AI
          EN(2)=EN(2)/AI
          EN(3)=EN(3)/AI
          AI=AI/2.

          
!   131      DO i=1,NDIMEL
!             WRITE(*,*) 'NRED(',I,')',NRED(I)
!            ENDDO
!            do i = 1, 3
!              WRITE(*,*) 'VG(1,NBREL,',I,')',VG(1,NBREL,I)
!            enddo
!             write(*,*) "IC1",IC1
!            write(*,*) "IC2",IC2
!            write(*,*) "IC31",IC31
!            write(*,*) "IC32",IC32
!            write(*,*) "IC33",IC33
!            write(*,*) "IC34",IC34
!            write(*,*) "IC4",IC4
!            write(*,*) "IC41",IC41
!            write(*,*) "IC5",IC5
!            write(*,*) "IC51",IC51
!            write(*,*) "IC52",IC52
!            write(*,*) "IC6",IC6
!            write(*,*) "IC61",IC61
!            write(*,*) "IC62",IC62
!            write(*,*) "IC7",IC7
!            write(*,*) "IC71",IC71
!            write(*,*) "IC72",IC72
!    131     write(*,*) "pre racunanja brzina"
!             V(1)=0.0
!             V(2)=0.0
!             V(3)=0.0
!  131      IF(NTYPE.EQ.3) THEN
 131           DO I=1,3
               V(I)=(VG(I,NBREL,NRED(IC1))+VG(I,NBREL,NRED(IC2))+
     1          ((VG(I,NBREL,IC31)+VG(I,NBREL,IC32)*IC33)/IC34)*IC3+
     2          VG(I,NBREL,NRED(IC4))*IC41+
     3          (VG(I,NBREL,IC51)/2+VG(I,NBREL,IC52)/2)*IC5+
     4          (VG(I,NBREL,IC61)/2+VG(I,NBREL,IC62)/2)*IC6+
     5          (VG(I,NBREL,IC71)/2+VG(I,NBREL,IC72)/2)*IC7)/NSCV
            ENDDO
!           ELSEIF (NTYPE.EQ.2) THEN
!            DO I=1,3
!               V(I)=(VG(I,NBREL,NRED(IC1))+VG(I,NBREL,NRED(IC2)))/2
! !                V(I)=(VG(I,NBREL,NRED(IC1))+VG(I,NBREL,NRED(IC2))+
! !      1         ((VG(I,NBREL,IC31)+VG(I,NBREL,IC32)*IC33)/IC34)*IC3
! !      5          )/NSCV
!            ENDDO
!           ENDIF
!           write(*,*) "posle racunanja brzina"
C      V(1)=(VECTJ(1,ICV1)+VECTJ(1,ICV2)+VECTJ(1,ICV3)+VECTJ(1,ICV4))/4.
C      V(2)=(VECTJ(2,ICV1)+VECTJ(2,ICV2)+VECTJ(2,ICV3)+VECTJ(2,ICV4))/4.
C      V(3)=(VECTJ(3,ICV1)+VECTJ(3,ICV2)+VECTJ(3,ICV3)+VECTJ(3,ICV4))/4.
!       WRITE(*,*) 'NSCV',NSCV
!         DO I=1,3
!            WRITE(*,*) 'V(',I,')=',V(I)
!            WRITE(*,*) 'EN(',I,')=',EN(I)
!         ENDDO
!            write(3,*) "EN",EN(1),EN(2),EN(3)
!            write(3,*) "V",V(1),V(2),V(3)
          VN=EN(1)*V(1)+EN(2)*V(2)+EN(3)*V(3)
          VINT=V(1)**2+V(2)**2+V(3)**2
!            write(3,*) "VI",V(1),V(2),V(3)
!            write(3,*) "VINT",VINT
           VI=0.0
          IF(VINT.GT.1.D-14) THEN
            VI=DSQRT(VINT)
            AL=DACOS(VN/VI)*180./PI
          ENDIF
!           write(*,*) "IC1",IC1
!            WRITE(*,*) 'VN,VINT,',VN,VINT
!           write(*,*) "pre uslov ntype"
 200      IF(NTYPE.EQ.1) THEN
             VN=EN(1)*VG(1,NBREL,NRED(IC1))+EN(2)*
     1        VG(2,NBREL,NRED(IC1))+EN(3)*VG(3,NBREL,NRED(IC1))
             QN=-VN*thick(NBREL)
!                 WRITE(*,*) 'VI,VG',VN,VG(1,NBREL,NRED(IC1)),EN(1)
          ELSEIF(NTYPE.EQ.2) THEN
!             QN=-VI*RI*thick(NBREL)
            QN=-VN*RI*thick(NBREL)
!              write(3,*) "QN,VN,RI",QN,VN,RI
!             write(3,*) "QN,",QN
          ELSE
            QN=-VN*AI
	  ENDIF
	  IF(QN.LT.0.) THEN
            QUM(KK)=QUM(KK)+QN
          ELSE
            QUK(KK)=QUK(KK)+QN
          ENDIF
!            WRITE(3,*) 'NBREL,thick(NBREL),VN,',NBREL,thick(NBREL),VN
  134   CONTINUE
!            WRITE(*,*) 'QUM,QUK,AI',QUM(1),QUK(1),AI
C
  723 CONTINUE

      QK=0.
      QM=0.
      DO L=1,NKONT
         QK=QK+QUK(L)
         QM=QM+QUM(L)
      ENDDO
       WRITE(IIZLAZ,*)' KONTURA, ULAZNI,      IZLAZNI,    UKUPNI PROTOK'
      DO L=1,NKONT
       WRITE(IIZLAZ,5055) L,QUK(L),QUM(L),(QUK(L)+QUM(L))
      ENDDO
       WRITE(IIZLAZ,*)' SUMARNI  ULAZNI,      IZLAZNI,    UKUPNI PROTOK'
       WRITE(IIZLAZ,5056) QK,QM,QK+QM
       WRITE(21,*)' KONTURA, ULAZNI,      IZLAZNI,    UKUPNI PROTOK'
      DO L=1,NKONT
       WRITE(21,5055) L,QUK(L),QUM(L),(QUK(L)+QUM(L))
      ENDDO
       WRITE(21,*)' SUMARNI  ULAZNI,      IZLAZNI,    UKUPNI PROTOK'
       WRITE(21,5056) QK,QM,QK+QM
C stampanje protoka po 1 konturi u POT.TXT
        If(INDSC.EQ.1)then
            WRITE(47,*) QUK(1)
            WRITE(47,*) -1
        endif
 5005 FORMAT(10X,10(1PE13.5))  	
 5055 FORMAT(I6,10(1PE13.5))  	
 5056 FORMAT(6X,10(1PE13.5))  	
C
      RETURN
      END
C=======================================================================
C
C RACUNANJE PROTOKA DUZ KONTURE ZA 3D ELEMENTE - preko brzina / Sneza april 2013./
C
C=========================================================================
!       SUBROUTINE RKONT3V(VG,KONT,NEL,QUK,QUM,CORD,KOR)
      SUBROUTINE RKONT3V(VG,KONT,QUK,QUM,CORD,KOR)
      USE ELEMENTS
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /TRENT3/ ZVHX(21),ZVHY(21),ZVHZ(21),CK(3,21),H(21),
     1 FS2,DETJS,DETJ,NBREL
      COMMON /STAC/ NSTAC,NSLP,IBROJ
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /DODAT/ NDIMM
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
C
      COMMON /PRIKAZ/ INDSC,IZPOT
C
      DIMENSION R42(3),R13(3),EN(3),V(3)
C
C      DIMENSION KONT(9,MAXLIN,*),NEL(NDIMM,*)
      DIMENSION KONT(9,MAXLIN,*)
      DIMENSION VG(3,NET,*),CORD(3,*)
      DIMENSION QUK(1000),QUM(1000),NRED(8)
      DATA NRED/8,4,2,6,7,3,1,5/  
C
C*    BLOK ZA STAMPANJE PROTOKA PO ELEMENTIMA ZA GRAFIKU - PRIVREMENO
      JEDAN=1
      NULA=0
      ZERO=0.
      MJ=-1
      M8=8
C
      IN=15
      IGRAF=48
      WRITE(IGRAF,1000) KOR,IN,JEDAN
      WRITE(IGRAF,3003) 
      WRITE(IGRAF,1010) ZERO,ZERO,ZERO
      WRITE(IGRAF,1000)NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(IGRAF,1000)NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
      WRITE(IGRAF,1000) NULA,NULA,M8,M8
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      WRITE(IGRAF,1000) JEDAN,NULA,JEDAN
 1000 FORMAT(10I5)
 1010 FORMAT(3(1PD12.4))
 3003 FORMAT(' PROTOK PO JEDINICI POVRSINE')
C*
      ONE=1.
      PI=4.*DATAN(ONE)
C
      NLM=0
C
      DO 723 KK=1,NKONT
        QUK(KK)=0.D0
        QUM(KK)=0.D0
C
	DO 134 II=1,LIN(KK)
C
          NBREL=KONT(1,II,KK)
          NMAT=NEL(NDIM+1,NBREL)
          NEMA=NEL(NDIM+2,NBREL)
          NLM=NLM+1

          IF (NMAT.LE.0) GOTO 134
          IF (NEMA.GT.0) GOTO 134
C
	  ICV1=KONT(2,II,KK)
	  ICV2=KONT(3,II,KK)
	  ICV3=KONT(4,II,KK)
	  ICV4=KONT(5,II,KK)

          DO KLM=1,NDIM
            IF(ICV1.EQ.NEL(KLM,NBREL)) IC1=KLM
            IF(ICV2.EQ.NEL(KLM,NBREL)) IC2=KLM
            IF(ICV3.EQ.NEL(KLM,NBREL)) IC3=KLM
            IF(ICV4.EQ.NEL(KLM,NBREL)) IC4=KLM
          ENDDO
C
          DO I=1,3
            R13(I)=CORD(I,ICV3)-CORD(I,ICV1)
            R42(I)=CORD(I,ICV2)-CORD(I,ICV4)
          ENDDO

          CALL AXBV(R42,R13,EN)   
          AI=DSQRT(EN(1)*EN(1)+EN(2)*EN(2)+EN(3)*EN(3))
          EN(1)=EN(1)/AI
          EN(2)=EN(2)/AI
          EN(3)=EN(3)/AI
          AI=AI/2.
          V(1)=(VG(1,NBREL,NRED(IC1))+VG(1,NBREL,NRED(IC2))+
     1          VG(1,NBREL,NRED(IC3))+VG(1,NBREL,NRED(IC4)))/4. 
          V(2)=(VG(2,NBREL,NRED(IC1))+VG(2,NBREL,NRED(IC2))+
     1          VG(2,NBREL,NRED(IC3))+VG(2,NBREL,NRED(IC4)))/4. 
          V(3)=(VG(3,NBREL,NRED(IC1))+VG(3,NBREL,NRED(IC2))+
     1          VG(3,NBREL,NRED(IC3))+VG(3,NBREL,NRED(IC4)))/4. 
C      V(1)=(VECTJ(1,ICV1)+VECTJ(1,ICV2)+VECTJ(1,ICV3)+VECTJ(1,ICV4))/4.
C      V(2)=(VECTJ(2,ICV1)+VECTJ(2,ICV2)+VECTJ(2,ICV3)+VECTJ(2,ICV4))/4.
C      V(3)=(VECTJ(3,ICV1)+VECTJ(3,ICV2)+VECTJ(3,ICV3)+VECTJ(3,ICV4))/4.
C brzina u pravcu normale
          VN=EN(1)*V(1)+EN(2)*V(2)+EN(3)*V(3)
          VINT=V(1)*V(1)+V(2)*V(2)+V(3)*V(3)
          IF(VINT.GT.1.D-15) THEN
C intezitet vektora brzine
            VI=DSQRT(VINT)
            AL=DACOS(VN/VI)*180./PI
          ENDIF
C
C*    STAMPANJE PROTOKA PO ELEMENTU
C      
      WRITE(IGRAF,5000) NLM,-VN
C*
  134 CONTINUE
C
  723 CONTINUE
C
C*    KRAJ BLOKA ZA STAMPANJE PROTOKA PO ELEMENTU
      WRITE(IGRAF,5000) MJ,ZERO
 5000 FORMAT(I5,3(1PD12.4))
C*
      RETURN
      END
C=======================================================================
      SUBROUTINE SPARS_ZADLEV(NZADJ,NZADP,NZAD,CORD,VVREME,TABF,
     1                                              NTABFT,ITFMAX)
      USE STIFFNESS
      USE PREDISCRIBED
      IMPLICIT NONE
C
C ......................................................................
C .
CE.   P R O G R A M
CE.      TO PRESCRIBED POTENTIAL. - UPDATING OF LEFT SIDE WITH BLOCKS
C .
C ......................................................................
      COMMON /BLOCKS/ NBMAX,IBLK,NBLOCK,LMNQ,LICPL,LLREC,KC,LR
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
      COMMON /CDEBUG/ IDEBUG

      DOUBLE PRECISION PENALTY_FACTOR

      INTEGER NBMAX,IBLK,NBLOCK,LMNQ,LICPL,LLREC,KC,LR
      INTEGER IULAZ,IIZLAZ,IPAKT,ISIL,IFIL
      INTEGER IDEBUG

      INTEGER NZADJ,NZADP,NZAD,IOSA,ITFMAX,NTABFT
      INTEGER I,J,K, NJ, NDIJ, istat
      DOUBLE PRECISION A,CORD,FK1,ZASIC,TABF,VVREME

      DIMENSION NZADJ(*),NZAD(3,*),CORD(3,*)
      DIMENSION TABF(2,NTABFT,*),ITFMAX(*)
        
      PENALTY_FACTOR=1.0D35
      
      IF(NBLOCK.GT.1) THEN
       WRITE(IIZLAZ,*) "IMSL SPARSE WITH BLOCKS IS NOT SUPPORTED"
       STOP
      ENDIF
      
      if (.not.allocated(NZADC)) then 
        allocate(NZADC(NZADP),STAT=istat)
      
        DO J = 1, nonzeros
          IF(rows(J).EQ.columns(J)) THEN
            DO I=1,NZADP
              IF (NZAD(2,I).EQ.4.OR.NZAD(2,I).EQ.5) EXIT

              NJ=NZADJ(I)
              IF(NJ.eq.rows(J)) THEN
                NZADC(I)=J
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      endif

       DO 10 I=1,NZADP
          IF(IPAKT.NE.1) THEN
           CALL TIMFUN (TABF,FK1,VVREME,ITFMAX(NZAD(3,I)),NZAD(3,I))
           IF(NZAD(2,I).EQ.6.AND.CORD(IOSA,NZAD(1,I)).GT.FK1) goto 10
          ENDIF
          IF (NZAD(2,I).EQ.4.OR.NZAD(2,I).EQ.5) GOTO 10
         
          stiff(NZADC(I)) = PENALTY_FACTOR

   10  CONTINUE
   
      RETURN
      END

C$DEBUG      
C=========================================================================
      SUBROUTINE RACN3DT(TT1,SILE,
     1ID,NZAD,ZADVRE,NGPSIL,MAXA,CORD,SKEF,SKEFN,
     2FZAPR,VREME,TABF,TT10,UBRZ,UBRZ0,AK,
     3VECTJ,IVECT,POVSIL,GRADJN,
     4ITFMAX,AKONST,NASLOV,ICUR,VG,GG,INDPT)
      USE PPR
      USE STIFFNESS
      USE ELEMENTS
      USE PREDISCRIBED
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      include 'paka.inc'
      INCLUDE 'mpif.h'
c      COMMON A(17000)
c      REAL A
      COMMON /TRENT3/ ZVHX(21),ZVHY(21),ZVHZ(21),CK(3,21),H(21),
     1 FS2,DETJS,DETJ,NBREL
      COMMON /KRITER/ IDOKL,MAXCUR
      COMMON /TREN13/ FPS1,FPS2,FPS3,FPS4
      COMMON /REPERI/ LCORD,LID,LMAXA,LMHT
      COMMON /BLOCKS/ NBMAX,IBLK,NBLOCK,LMNQ,LICPL,LLREC,KC,LR
      COMMON /LSK1/ LSK
      COMMON /JEDANP/ INDJED,NBRF,NGL,INDTLO
      COMMON /FLUX/ KISA
      COMMON /TACNOS/ EPSTR,MAXIT,NJRAP
      COMMON /ZADPO/ LNZADJ
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /DUZINA/ LMAX,MTOT,LMAXM,LRAD,NRAD
      COMMON /SCRATC/ ISCRC
      COMMON /KONTKT/ ICONT,NEQC,NEQ,NWKC,LMAXAC,LRCTDT
      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /NDESUK/ NDES,IDPRIT,IFORM
      COMMON /PENALL/ PENALT,PRESS,IBRGT
      COMMON /POCETN/ IPOCU,IPOCV,IPOCP,IPOCT,POCU,POCV,POCP,POCT,GAMA
      COMMON /VREPER/ NPER,NTABFT
      COMMON /PROMEN/ NJUTN,INDOPT,INDPRO
      COMMON /DODAT/ NDIMM
      COMMON /DYNAM/ NDIN
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /STAC/ NSTAC,NSLP,IBROJ
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /VDP/ DT,NKORP,NN,NZAV
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
C
       COMMON /SILZP3/ FZAP(3,21)
C
      COMMON /PRIKAZ/ INDSC,IZPOT
C
      COMMON /PIJEZO/ CPOR(3,1000),NPIJEZ(20,100),NODP(100),NPIJ,NPOR,
     1                NPORCV(1000),NPOREL(1000),NEPOR,NPORE,NEPIJ
      COMMON /PORCEL/ RMIN(34),SMIN(34),TMIN(34)
      COMMON /NXNAST/ NXNASTRAN
      COMMON /SOLVER/ ISOLVER
      COMMON /NELDIM/ NDIMEL
      COMMON /DJERDAP/ IDJERDAP
      COMMON /ICITANJE/ INPT
C
      CHARACTER*80 NASLOV
      DIMENSION TT1(*),SILE(*),ZADVRE(*),CORD(3,*)
      DIMENSION VG(3,NET,*),GG(3,NET,*)
      DIMENSION TT10(*),UBRZ(*),UBRZ0(*)
      DIMENSION ID(1,*),NZAD(3,*),NGPSIL(12,*),MAXA(*)
      DIMENSION SKEF(NDES,*),AKONST(3,5,*),SKEFN(*)
      DIMENSION POVSIL(4,*),PR1(21),ITFMAX(*)
C
!       DIMENSION KONT(9,MAXLIN,*),KOJK(*)

      DIMENSION TT21(44),TT210(44),QUK(1000),QUM(1000)
      DIMENSION R(3,3),S(3,3),T(3,3),W(3,3)
      DIMENSION RK(3,3),SK(3,3),TK(3,3),WK(3,3)
      DIMENSION FZAPR(3,*),VECTJ(3,*),GRADJN(3,*)
      DIMENSION IVECT(*),LM2(44),ICUR(*)
      DIMENSION PERM(3,4)
      DIMENSION VREME(NPER,950)
      DIMENSION TABF(2,NTABFT,*)
      DIMENSION AM1(21,21),AK(NDES,*)

      DIMENSION RS2(21)
      DIMENSION RS3(21)
      DIMENSION F36(44)
C
      DIMENSION AFIFI1(21,21),AFIFI(21,21)
      DIMENSION PUS(21,21),AJX(20),AJY(20),AJZ(20)
      DIMENSION AJX1(20),AJY1(20),AJZ1(20),NRED(8),NREDK(8)
      DIMENSION NRED1(2),NREDT4(4),NREDT10(10),NRED26(6),NRED28(8)
      DIMENSION HT(9,3),CKL(3,8),TTE(2,3),WTET(3),INDEKS(10)
!       promenjena duzina NRED sa 8 na 10 i dodata 2 clana
      DATA NRED/8,4,2,6,7,3,1,5/  
      DATA NREDK/27,9,3,21,25,7,1,19/
      DATA NRED1/2,1/
      DATA NRED26/1,2,3,12,23,31/
      DATA NRED28/1,2,3,4,12,23,34,41/
      DATA NREDT4/1,2,3,4/
      DATA NREDT10/1,2,3,4,12,23,13,14,24,34/
      integer ierr, myid
      integer*8 LM2,IPROM,NGPSIL

      CHARACTER*3 STATTT
      DIMENSION NDJEL(21),NP3D1(5)
      logical OLDNEWW

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)

      IF (myid.ne.0) goto 1235
  
      if (.not.allocated(PORNIEL)) allocate(PORNIEL(NET,10),STAT=istat) 
      if(.not.allocated(IDJSTAMP1))allocate(IDJSTAMP1(5,NPT),STAT=istat) 
      DO I=1,5
        DO JJ=1,NPT
          IDJSTAMP1(I,JJ)=0
        ENDDO
      ENDDO
C
       CALL ICLEAR(NP3D1,5)
!       CALL ICLEAR(IDJSTAMP1,NPT)
C      IDJERDAP=1
!          write(*,*) 'IDJERDAP',IDJERDAP
      IF(IDJERDAP.EQ.1)THEN
C   program za citanje cvorova u kojima se stampaju filtracione sile
              CALL DJERDAPREADT(IDJSTAMP1,NP3D1)
C
       ENDIF
C INDIKATOR ZA OSU TEZINE - ne treba za pakt
!  9998   IF (IOSA.EQ.0) IOSA=3

C INDIKATOR ZA TRAZENJE IZOHIPSI PO FAJLOVIMA:
       INDJED=0

C ZBOG PAK-a NEQ-BROJ JEDNACINA .EQ. JEDN
       NEQ=JEDN
!        ICONT=1

C IDOKL JE BROJ CVOROVA KOJI SE UZIMAJU KAO AKTIVNI NA LINIJI CURENJA 
C ICUR JE NIZ U KOME SE NALAZE CVOROVI NA LINIJI CURENJA
C
C  INICIJALIZACIJA KONVERGENCIJE ZA LINIJU CURENJA
       KONV1=1  

!        MAXCUR=0
!        IDOKL=0
!        CALL INIDOK(NZAD,ICUR,CORD)

C UTVRDJIVANJE KOD KOLIKO ELEMENATA SE POJAVLJUJE JEDAN CVOR
      CALL MVECT(IVECT)

      DO I=1,JEDN
       TT1(I)=0.D0
!        UBRZ(I)=0.D0
      ENDDO      
       
! C DEFINISANJE CVOROVA ZA PROTOKE   ne treba za pakt
!       DO KK=1,NKONT
! 	DO II=1,LIN(KK)
! 	  KOJK(KONT(2,II,KK))=KOJK(KONT(2,II,KK))+1
! 	  KOJK(KONT(3,II,KK))=KOJK(KONT(3,II,KK))+1
! 	  KOJK(KONT(4,II,KK))=KOJK(KONT(4,II,KK))+1
! 	  KOJK(KONT(5,II,KK))=KOJK(KONT(5,II,KK))+1
! 	  KOJK(KONT(6,II,KK))=KOJK(KONT(6,II,KK))+1
! 	  KOJK(KONT(7,II,KK))=KOJK(KONT(7,II,KK))+1
! 	  KOJK(KONT(8,II,KK))=KOJK(KONT(8,II,KK))+1
! 	  KOJK(KONT(9,II,KK))=KOJK(KONT(9,II,KK))+1
!         ENDDO
!       ENDDO	  
C
      R(3,1)=-0.7745966692415
      R(3,2)=0.0
      R(3,3)=0.77459666924148
      S(3,1)=-0.7745966692415
      S(3,2)=0.0
      S(3,3)=0.77459666924148
      T(3,1)=-0.7745966692415
      T(3,2)=0.0
      T(3,3)=0.77459666924148
      W(3,1)=0.55555555555556
      W(3,2)=0.88888888888889
      W(3,3)=0.55555555555556
C
      RK(3,1)=-1.0
      RK(3,2)=0.0
      RK(3,3)=1.0
      SK(3,1)=-1.0
      SK(3,2)=0.0
      SK(3,3)=1.0
      TK(3,1)=-1.0
      TK(3,2)=0.0
      TK(3,3)=1.0
      WK(3,1)=0.33333333333333
      WK(3,2)=1.33333333333333
      WK(3,3)=0.33333333333333
C 
      R(2,1)=-0.5773502691896
      R(2,2)=0.57735026918963
      S(2,1)=-0.5773502691896
      S(2,2)=0.57735026918963
      T(2,1)=-0.5773502691896
      T(2,2)=0.57735026918963
      W(2,1)=1.000000000000000
      W(2,2)=1.000000000000000

      R(1,1)=0.0
      S(1,1)=0.0
      T(1,1)=0.0
      W(1,1)=2.0
      
      PI=4.D0*DATAN(1.D0)
            
C===========================================================================
      IAXIS=6
C
      INULTO=0
      KKORAK=1
      VVREME=0.D0
C indikator LINTE=0 - linearan proracun
      LINTE=0
C=========================================================================
c petlja po periodima
      DO 600 NNPER=1,NPER
      KORAK=0
c petlja po koracima
   35 KORAK=KORAK+1
C
C=========================================================================   
C  RAD SA STACIONARNIM STANJEM U PRVOM KORAKU I NESTACIONARNIM U OSTALIM
C  ( GEOLOSKI PROFILI ZA CERNE)
C
      IF (KKORAK.EQ.1) then
        NNSTAC=NSTAC
C       WRITE(19) KKORAK
      endif
cz uslov da prvi korak radi stacionarno
      IF (KKORAK.EQ.1.AND.NNSTAC.EQ.2) NSTAC=1
      IF (KKORAK.GT.1.AND.NNSTAC.EQ.2) NSTAC=0
C
cz      IF (KKORAK.GT.0) THEN
      TIME=VREME(NNPER,KORAK)
      VVREME=VVREME+TIME
cz      ELSE
cz       TIME=1.D7
cz       KORAK=KORAK-1
cz      ENDIF

      DO I=1,NPT 
       DO J=1,3
!         VECTJ(J,I)=0.D0
        GRADJN(J,I)=0.D0
       ENDDO
      ENDDO

C     ZADAVANJE POCETNE TEMPERATURE
      IF (KKORAK.EQ.1.AND.NNSTAC.EQ.0) THEN
          IF(INDPT.EQ.O) THEN
             DO I=1,JEDN
                TT10(I)=POCT
             ENDDO
          ELSE
C            UCITAVANJE TEMPERATURA KOJE CE BITI POCETNE
               OPEN(99,FILE='ZIPOCT.UL',STATUS='OLD',FORM='UNFORMATTED',
     1              ACCESS='SEQUENTIAL')
             CALL POCETNET(TT10,NPT,99,1)
             CLOSE (99)		  
          ENDIF
      ELSE
         DO I=1,JEDN
            TT10(I)=TT1(I)
         ENDDO
      ENDIF
C TT1 - poslednja poznata temperatura   
       
C
      ITER=0
      ITER1=0
      INDSK=1
      INDPR=0
C==========================================================================
C LABELA 100:
C==========================================================================
 100  IF (ITER.GT.MAXIT) THEN
        WRITE(IIZLAZ,3001) MAXIT
 3001 FORMAT(//
     1,'DOSTIGNUT JE MAKSIMALAN BROJ OD ',I5,' ITERACIJA BEZ POSTIZANJA 
     1KONVERGENCIJE!!'//)
       STOP
       ENDIF

	IF (NBLOCK.GT.1) THEN
       OPEN (ISCRC,FILE='ZSKLIN',FORM='UNFORMATTED',STATUS='UNKNOWN')
       REWIND ISCRC
       ELSE
        KC=NWK
      ENDIF
C===========================================================================
C ZA CVOROVE NA LINIJI PROCURIVANJA INICIJALIZACIJA ZA NESTACIONARNO
C STRUJANJE - START JE UVEK OD PRVOG NAJNIZEG CVORA PA NA VISE
C      IF (ITER.EQ.0.AND.MAXCUR.NE.0) IDOKL=2
!        IF (ITER.EQ.0.AND.MAXCUR.NE.0) IDOKL=MAXCUR
C===========================================================================
      IF(NXNASTRAN.EQ.0) THEN
! 	IF (MAXCUR.GT.0) THEN
!           if(ISOLVER.NE.-11) CALL CLEAR(A(LSK),KC)
!         ELSE
!   samo za modifikovan Njutn
          IF (ITER.EQ.0.and.ISOLVER.NE.-11) CALL CLEAR(A(LSK),KC)
!         ENDIF
      ENDIF
       
      DO I=1,JEDN
          SILE(I)=0.D0
!           UBRZ(I)=0.D0
      ENDDO 

C
      IF((KKORAK.EQ.1).AND.(ITER.EQ.0))THEN
C petlja po pornim celijama
!       do II=1,NPORE
! C odredjivanje lokalnih koordinata porne celije - termometra
!       deltta=0.1
!       RP=-1.02
!       SP=-1.02
!       TP=-1.02
!       DMIN=100.0
!       RMIN(II)=100.0
!       SMIN(II)=100.0
!       TMIN(II)=100.0
!       DRST=0.02
!       DO  IR=1,100
!         RP=RP+DRST
!         SP=-1.02
!       DO  JS=1,100
!         SP=SP+DRST
!         TP=-1.02
!       DO  LT=1,100
!         TP=TP+DRST
! c        CALL INTERST(RP,SP,TP)
!           RPP=1.0+RP
!           SPP=1.0+SP
!           TPP=1.0+TP
!           RM=1.0-RP
!           TM=1.0-TP
!           SM=1.0-SP
!           RR=1.0-RP*RP
!           SS=1.0-SP*SP
!           TT=1.0-TP*TP
!           DO I=1,8
!             H(I)=0.
!           ENDDO
!           H(1)=0.125*RPP*SPP*TPP
!           H(2)=0.125*RM*SPP*TPP
!           H(3)=0.125*RM*SM*TPP
!           H(4)=0.125*RPP*SM*TPP
!           H(5)=0.125*RPP*SPP*TM
!           H(6)=0.125*RM*SPP*TM
!           H(7)=0.125*RM*SM*TM
!           H(8)=0.125*RPP*SM*TM
!         XT=0.
!         YT=0.
!         ZT=0.
!         DO JJ=1,8
!           XT=XT + H(JJ)*CORD(1,NEL(JJ,NPOREL(II)))
!           YT=YT + H(JJ)*CORD(2,NEL(JJ,NPOREL(II)))
!           ZT=ZT + H(JJ)*CORD(3,NEL(JJ,NPOREL(II)))
!         enddo
!         DX=XT-CPOR(1,II)
!         DY=YT-CPOR(2,II)
!         DZ=ZT-CPOR(3,II)
!         dxyz=DX*DX+DY*DY+DZ*DZ
!         DXYZ=SQRT(dxyz)
!         IF (DXYZ.LE.DMIN) THEN
!             DMIN=DXYZ
!             RMIN(II)=RP
!             SMIN(II)=SP
!             TMIN(II)=TP
!          ENDIF
!       enddo
!       enddo
!       enddo
! C  kraj petlje po pornim celijama
!          iel=NPOREL(II)
!          NMAT=NEL(NDIM+1,iel)
!        WRITE (3,*)"PORNA CELIJA", II, NPOREL(II),(CPOR(ijk,II), ijk=1,3)
!        WRITE(3,*) DMIN,RMIN(II),SMIN(II),TMIN(II),NMAT 
!       enddo
        IF ((ISOLVER.EQ.-11).OR.(ISOLVER.EQ.1)) THEN
C         MUMPS - full Njutn
          CALL sparseassembler_init(1)
        ENDIF
      ENDIF     
C -------------------------------------
C GLAVNA PETLJA PO ELEMENTIMA
C -------------------------------------
!       IF(NXNASTRAN.EQ.1) GO TO 999
      DO 400 NBREL=1,NET
      IF(elemtip(NBREL).gt.100) THEN
        NTYPE=elemtip(NBREL)/100
        NTIMES=100
      ELSE
        NTYPE=elemtip(NBREL)/10
        NTIMES=10
      ENDIF
      NDIMEL=elemtip(NBREL)-NTIMES*NTYPE
C ODEDIVANJE BROJA GAUSOVIH TACAKA PRILIKOM INTEGRACIJE
      IBRGT=3
      IF (NDIMEL.LE.8) IBRGT=2

      NMAT=NEL(NDIM+1,NBREL)
      NEMA=NEL(NDIM+2,NBREL)

      IF (NMAT.LE.0.OR.NEMA.GT.0) THEN
         WRITE(3,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
         WRITE(*,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
!          STOP
      ENDIF
      IF (NMAT.LE.0) GOTO 400
      IF (NEMA.GT.0) GOTO 400
      
!        PERM(1,1)=AKONST(3,1,NMAT)
!        PERM(1,2)=0.
!        PERM(1,3)=0.
!        PERM(2,2)=AKONST(3,2,NMAT)
!        PERM(2,1)=0.
!        PERM(2,3)=0.
!        PERM(3,3)=AKONST(3,3,NMAT)
!        PERM(3,1)=0.
!        PERM(3,2)=0.
! 
!       IF (PERM(1,1).LT.1.D-15.OR.PERM(2,2).LT.1.D-15.OR.
!      1    PERM(3,3).LT.1.D-15) THEN
!          WRITE(3,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
!          WRITE(*,*) 'NBREL,NMAT,NEMA',NBREL,NMAT,NEMA
!          WRITE(*,*) 'PERM',PERM(1,1),PERM(2,2),PERM(3,3)
!          STOP
!       ENDIF
C=========================================================================
      DO 130 KLM=1,NDIMEL
      CK(1,KLM)=CORD(1,NEL(KLM,NBREL))
      CK(2,KLM)=CORD(2,NEL(KLM,NBREL))
      CK(3,KLM)=CORD(3,NEL(KLM,NBREL))
C
      LM2(KLM)=ID(1,NEL(KLM,NBREL))
 130  CONTINUE
    
      if(NTYPE.eq.2) CALL LCK2D(CK,CKL,TTE,NDIMEL)
C=======================================================================
      CALL CLEARD(TT21,NDIM)
      CALL PREB(TT21,TT1,ID,NEL,NBREL)
      CALL CLEARD(TT210,NDIM)
      CALL PREB(TT210,TT10,ID,NEL,NBREL)
 
C=======================================================================
      DO 163 K=1,NDIM
      DO 162 N=1,NDIM
        IF(NSTAC.EQ.0) AM1(K,N)=0.D0    
        AFIFI(K,N)=0.D0    
        AFIFI1(K,N)=0.D0    
!         PUS(K,N)=0.D0    
  162 CONTINUE
  163 CONTINUE
C Fluks I Toplotni izvor:
      DO 199 I=1,NDIM
      RS2(I)=0.
      RS3(I)=0.
 199  CONTINUE
C INCIJALIZACIJA MATRICE SKEF I F36
      DO 260 I=1,NDES
      DO 258 J=1,NDES
      SKEF(I,J)=0.
 258  CONTINUE
      F36(I)=0.
 260  CONTINUE
C
C INTEGRACIJA U GAUSOVIM TACKAMA
C
C sneza 11.02.2012
c       DO II=1,NDIM
c         PR1(II)=TT21(II)-CORD(IOSA,NEL(II,NBREL))
c        ENDDO

      NGAUS=0
      POVREL=0.
      IBRGT1=IBRGT
      IBRGT2=IBRGT
      if(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
        IBRGT2=1
        IBRGT1=4
      ENDIF
      IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) IBRGT2=1
      IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) IBRGT1=1
      IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
        IBRGT2=1
        IBRGT1=3
      ENDIF
      DO 180 I=1,IBRGT1
      DO 170 J=1,IBRGT2
      DO 160 L=1,IBRGT2
      NGAUS=NGAUS+1
      IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
       CALL GETRST(RT,ST,TT,WTET,NGAUS,IBRGT1)
       CALL INTERPTETRA(RT,ST,TT,0,0,NDIMEL)
       WDT=WTET(1)*WTET(2)*WTET(3)*DETJ
      ELSE IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
        CALL INTERP1D(R(IBRGT,I),0,0,NDIMEL,AJS)  
        WDT=W(IBRGT,I)*thick(NBREL)*AJS
      ELSE IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) THEN
        CALL INTERP2D48(R(IBRGT,J),S(IBRGT,L),0,0,NDIMEL,CKL,TTE)
        WDT=W(IBRGT,J)*W(IBRGT,L)*DETJ*thick(NBREL)
      ELSE IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
        RT=1./6
        ST=1./6
        WT=1./3
        IF(NGAUS.EQ.2) RT=2./3
        IF(NGAUS.EQ.3) ST=2./3
        CALL INTERP2D36(RT,ST,0,0,NDIMEL,CKL,TTE)
        WDT = WT**2*thick(NBREL)*DETJ
      ELSE
        CALL INTER3(R(IBRGT,I),S(IBRGT,J),T(IBRGT,L),0,0)
        WDT=W(IBRGT,I)*W(IBRGT,J)*W(IBRGT,L)*DETJ
      ENDIF
      
      POVREL=POVREL+WDT
      
Cs ovaj uslov nije isti za stacionarno i nestacionarno
Cs za nestacionarno proverava u svakoj iteraciji 
!       IF ( (((ITER.GT.0).AND.(kkorak.EQ.1))
!      1.OR.KKORAK.GT.1) .AND.(INDFS.EQ.1)) THEN
C      IF ( (((ITER.GT.0).AND.(NSTAC.EQ.1))
C     1.OR.((KKORAK.GT.0).AND.(NSTAC.EQ.0))) .AND.(INDFS.EQ.1)) THEN
C
!        PR=0.D0
! C okvasenost u gausovoj tacki
!         DO II=1,NDIMEL
!          PR=PR+H(II)*(TT21(II)-CORD(IOSA,NEL(II,NBREL)))
!         ENDDO
! 
!            IF( PR.LT.0.D0 .AND. INDFS.EQ.1 )THEN
!             ANPUS=AKONST(3,4,NMAT)*ZASIC
!             PERM(1,1)=ZASIC*AKONST(3,1,NMAT)
!             PERM(2,2)=ZASIC*AKONST(3,2,NMAT)
!             PERM(3,3)=ZASIC*AKONST(3,3,NMAT)
!            ELSE
!             ANPUS=0.D0
!             PERM(1,1)=0.0D0
!             PERM(2,2)=0.0D0
!             PERM(3,3)=0.0D0
! !            ENDIF
!        ELSE
!         PERM(1,1)=0.0D0
!         PERM(2,2)=0.0D0
!         PERM(3,3)=0.0D0
!         ANPUS=0.D0
!        ENDIF

      DO 165 K=1,NDIMEL
      DO 164 N=1,NDIMEL
cz S - AM1 nije podeljeno sa dt; podeljeno sa dt prilikom pakovanja u matricu SKEF
!          IF(LINTE.EQ.0) THEN
!  matrica toplotnog kapaciteta  (1.27), (1.38)
!  konstantne u j-ni (1.27) zavise od temperature
         IF(NSTAC.EQ.0) 
     1 AM1(K,N)=AM1(K,N)+AKONST(3,4,NMAT)*AKONST(3,5,NMAT)*H(K)*H(N)*WDT
!     (1.24), (1.36) matrica kondukcije 
!  konstantne u j-ni (1.24) zavise od temperature
         AFIFI(K,N)=AFIFI(K,N)+(AKONST(3,1,NMAT)*ZVHX(K)*ZVHX(N)+
     1AKONST(3,2,NMAT)*ZVHY(K)*ZVHY(N)+AKONST(3,3,NMAT)*ZVHZ(K)*ZVHZ(N))
     2*WDT 
!          ELSE
!          ENDIF
!         write(*,*) 'AKONST(3,4,NMAT),AKONST(3,5,NMAT)',AKONST(3,4,NMAT),
!      2  AKONST(3,5,NMAT)
!         stop
C
!          IF (INDFS.EQ.1) THEN
!          PUS(K,N)=PUS(K,N)+ANPUS*H(K)*H(N)*WDT
!          AFIFI1(K,N)=AFIFI1(K,N)+(PERM(1,1)*ZVHX(K)*ZVHX(N)+
!      1PERM(2,2)*ZVHY(K)*ZVHY(N)+PERM(3,3)*ZVHZ(K)*ZVHZ(N))*WDT
!          ENDIF
!       if(NBREL.eq.1.AND.I.eq.IBRGT) then
!          WRITE(*,*) 'NBREL',NBREL
!          WRITE(*,*) 'H(K)',H(K)
!          WRITE(*,*) 'H(N)',H(N)
!          WRITE(*,*) 'ZVHX(K)',ZVHX(K)
!          WRITE(*,*) 'ZVHY(K)',ZVHY(K)
!          WRITE(*,*) 'ZVHZ(K)',ZVHZ(K)
!          WRITE(*,*) 'WDT',WDT
!          WRITE(*,*) 'AKONST(3,1,NMAT)',AKONST(3,1,NMAT)
!          WRITE(*,*) 'AKONST(3,2,NMAT)',AKONST(3,2,NMAT)
!          WRITE(*,*) 'AKONST(3,3,NMAT)',AKONST(3,3,NMAT)
!           write(3,*) 'AM1(',K,',',N,')',AM1(K,N)
!           write(3,*) 'AFIFI(',K,',',N,')',AFIFI(K,N)
!       ENDIF
C
  164 CONTINUE
  165 CONTINUE
  160 CONTINUE
  170 CONTINUE
  180 CONTINUE
!       WRITE(*,*) 'POVREL',POVREL
C=========================================================================
C POVRSINSKE SILE - FLUKS
C=================
!       IF (INDPUN.EQ.1) GOTO 265
!  MAXSIL - broj elemenata na kojima je zadat fluks
      DO 250 JBRPS=1,MAXSIL
      IF (NBREL.EQ.NGPSIL(1,JBRPS)) THEN
      NODE1=NGPSIL(2,JBRPS)
      NODE2=NGPSIL(3,JBRPS)
      NODE3=NGPSIL(4,JBRPS)
      NODE4=NGPSIL(5,JBRPS)
      NODE5=NGPSIL(6,JBRPS)
      NODE6=NGPSIL(7,JBRPS)
      NODE7=NGPSIL(8,JBRPS)
      NODE8=NGPSIL(9,JBRPS)
      N1=NEL(1,NBREL)
      N2=NEL(2,NBREL)
      N3=NEL(3,NBREL)
      N4=NEL(4,NBREL)
      N5=NEL(5,NBREL)
      N6=NEL(6,NBREL)
      N7=NEL(7,NBREL)
      N8=NEL(8,NBREL)
!       N9=NEL(9,NBREL)
!       N10=NEL(10,NBREL)
      IVRF=NGPSIL(10,JBRPS)
      CALL TIMFUN (TABF,FK1,VVREME,ITFMAX(IVRF),IVRF)
      FPS1=POVSIL(1,JBRPS)*FK1
      FPS2=POVSIL(2,JBRPS)*FK1
      FPS3=POVSIL(3,JBRPS)*FK1
      FPS4=POVSIL(4,JBRPS)*FK1
C
      NUMGAU=IBRGT
!  nije dobro uradjen fuks za 1D elemente !!!!      
      IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
        DO I=1,NUMGAU
          CALL INTERP1D(R(IBRGT,I),0,0,NDIMEL,AJS)  
          WDTV=W(IBRGT,I)*thick(NBREL)*AJS
          DO K=1,NDIMEL
               RS2(K)=RS2(K)+H(K)*FPS1*WDTV
        ENDDO
        ENDDO
        GOTO 250
      ENDIF
! 
!  dodati zadati fluks za 2D elemente
! 
!  fluks za tetra elemente
      IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
         DO NGXT = 1,3
             RT=1./6
             ST=1./6
             WT=1./3
             IF(NGXT.EQ.2) RT=2./3
             IF(NGXT.EQ.3) ST=2./3

            CALL JACTP33(RT,ST,HT,1,NGPSIL,JBRPS,CORD,NDIMEL)
            WDTS = WT*DETJS/2
            DO K=2,9
              DO J=1,NDIMEL
                 IF(NGPSIL(K,JBRPS).EQ.NEL(J,NBREL)) THEN
!   povrsinski fulks  j-na (1.30)                 
                    RS2(J)=RS2(J)+HT(K-1,1)*FS2*WDTS
                 ENDIF
              ENDDO
            ENDDO
         ENDDO 
!  fluks za heksa elemente      
      ELSEIF (elemtip(NBREL).eq.38.or.elemtip(NBREL).eq.320) THEN
      
      DO 225 I=1,NUMGAU
      DO 225 J=1,NUMGAU
C
C KONSTANTNO T (ZETA)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N2.AND.NODE3.EQ.N3.AND.NODE4.EQ.N4).
     1OR.(NODE1.EQ.N2.AND.NODE2.EQ.N3.AND.NODE3.EQ.N4.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N3.AND.NODE2.EQ.N4.AND.NODE3.EQ.N1.AND.NODE4.EQ.N2).
     3OR.(NODE1.EQ.N4.AND.NODE2.EQ.N1.AND.NODE3.EQ.N2.AND.NODE4.EQ.N3).
C
     4OR.(NODE1.EQ.N1.AND.NODE2.EQ.N4.AND.NODE3.EQ.N3.AND.NODE4.EQ.N2).
     5OR.(NODE1.EQ.N4.AND.NODE2.EQ.N3.AND.NODE3.EQ.N2.AND.NODE4.EQ.N1).
     6OR.(NODE1.EQ.N3.AND.NODE2.EQ.N2.AND.NODE3.EQ.N1.AND.NODE4.EQ.N4).
     7OR.(NODE1.EQ.N2.AND.NODE2.EQ.N1.AND.NODE3.EQ.N4.AND.NODE4.EQ.N3))
     8THEN
      CALL INTER3(R(NUMGAU,I),S(NUMGAU,J),1.D0,3,1)
       GOTO 220
      ENDIF 
C
C KONSTANTNO T (ZETA)
C 
      IF((NODE1.EQ.N5.AND.NODE2.EQ.N6.AND.NODE3.EQ.N7.AND.NODE4.EQ.N8).
     1OR.(NODE1.EQ.N6.AND.NODE2.EQ.N7.AND.NODE3.EQ.N8.AND.NODE4.EQ.N5).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N8.AND.NODE3.EQ.N5.AND.NODE4.EQ.N6).
     3OR.(NODE1.EQ.N8.AND.NODE2.EQ.N5.AND.NODE3.EQ.N6.AND.NODE4.EQ.N7).
C
     5OR.(NODE1.EQ.N5.AND.NODE2.EQ.N8.AND.NODE3.EQ.N7.AND.NODE4.EQ.N6).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N7.AND.NODE3.EQ.N6.AND.NODE4.EQ.N5).
     7OR.(NODE1.EQ.N7.AND.NODE2.EQ.N6.AND.NODE3.EQ.N5.AND.NODE4.EQ.N8).
     8OR.(NODE1.EQ.N6.AND.NODE2.EQ.N5.AND.NODE3.EQ.N8.AND.NODE4.EQ.N7))
     8THEN
      CALL INTER3(R(NUMGAU,I),S(NUMGAU,J),-1.D0,3,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO R (KSI)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N4.AND.NODE3.EQ.N8.AND.NODE4.EQ.N5).
     1OR.(NODE1.EQ.N4.AND.NODE2.EQ.N8.AND.NODE3.EQ.N5.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N8.AND.NODE2.EQ.N5.AND.NODE3.EQ.N1.AND.NODE4.EQ.N4).
     3OR.(NODE1.EQ.N5.AND.NODE2.EQ.N1.AND.NODE3.EQ.N4.AND.NODE4.EQ.N8).
C
     4OR.(NODE1.EQ.N1.AND.NODE2.EQ.N5.AND.NODE3.EQ.N8.AND.NODE4.EQ.N4).
     5OR.(NODE1.EQ.N5.AND.NODE2.EQ.N8.AND.NODE3.EQ.N4.AND.NODE4.EQ.N1).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N4.AND.NODE3.EQ.N1.AND.NODE4.EQ.N5).
     7OR.(NODE1.EQ.N4.AND.NODE2.EQ.N1.AND.NODE3.EQ.N5.AND.NODE4.EQ.N8))
     8THEN
      CALL INTER3(1.D0,S(NUMGAU,I),T(NUMGAU,J),1,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO R (KSI)
C
      IF((NODE1.EQ.N2.AND.NODE2.EQ.N3.AND.NODE3.EQ.N7.AND.NODE4.EQ.N6).
     1OR.(NODE1.EQ.N3.AND.NODE2.EQ.N7.AND.NODE3.EQ.N6.AND.NODE4.EQ.N2).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N6.AND.NODE3.EQ.N2.AND.NODE4.EQ.N3).
     3OR.(NODE1.EQ.N6.AND.NODE2.EQ.N2.AND.NODE3.EQ.N3.AND.NODE4.EQ.N7).
C
     4OR.(NODE1.EQ.N2.AND.NODE2.EQ.N6.AND.NODE3.EQ.N7.AND.NODE4.EQ.N3).
     5OR.(NODE1.EQ.N6.AND.NODE2.EQ.N7.AND.NODE3.EQ.N3.AND.NODE4.EQ.N2).
     6OR.(NODE1.EQ.N7.AND.NODE2.EQ.N3.AND.NODE3.EQ.N2.AND.NODE4.EQ.N6).
     7OR.(NODE1.EQ.N3.AND.NODE2.EQ.N2.AND.NODE3.EQ.N6.AND.NODE4.EQ.N7))
     8THEN
      CALL INTER3(-1.D0,S(NUMGAU,I),T(NUMGAU,J),1,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO S (ETA)
C
      IF((NODE1.EQ.N4.AND.NODE2.EQ.N3.AND.NODE3.EQ.N7.AND.NODE4.EQ.N8).
     1OR.(NODE1.EQ.N3.AND.NODE2.EQ.N7.AND.NODE3.EQ.N8.AND.NODE4.EQ.N4).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N8.AND.NODE3.EQ.N4.AND.NODE4.EQ.N3).
     3OR.(NODE1.EQ.N8.AND.NODE2.EQ.N4.AND.NODE3.EQ.N3.AND.NODE4.EQ.N7).
C
     4OR.(NODE1.EQ.N3.AND.NODE2.EQ.N4.AND.NODE3.EQ.N8.AND.NODE4.EQ.N7).
     5OR.(NODE1.EQ.N4.AND.NODE2.EQ.N8.AND.NODE3.EQ.N7.AND.NODE4.EQ.N3).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N7.AND.NODE3.EQ.N3.AND.NODE4.EQ.N4).
     7OR.(NODE1.EQ.N7.AND.NODE2.EQ.N3.AND.NODE3.EQ.N4.AND.NODE4.EQ.N8))
     8THEN
      CALL INTER3(R(NUMGAU,I),-1.D0,T(NUMGAU,J),2,1)
       GOTO 220
      ENDIF 
C 
C KONSTANTNO S (ETA)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N2.AND.NODE3.EQ.N6.AND.NODE4.EQ.N5).
     1OR.(NODE1.EQ.N2.AND.NODE2.EQ.N6.AND.NODE3.EQ.N5.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N6.AND.NODE2.EQ.N5.AND.NODE3.EQ.N1.AND.NODE4.EQ.N2).
     3OR.(NODE1.EQ.N5.AND.NODE2.EQ.N1.AND.NODE3.EQ.N2.AND.NODE4.EQ.N6).
C
     4OR.(NODE1.EQ.N2.AND.NODE2.EQ.N1.AND.NODE3.EQ.N5.AND.NODE4.EQ.N6).
     5OR.(NODE1.EQ.N1.AND.NODE2.EQ.N5.AND.NODE3.EQ.N6.AND.NODE4.EQ.N2).
     6OR.(NODE1.EQ.N5.AND.NODE2.EQ.N6.AND.NODE3.EQ.N2.AND.NODE4.EQ.N1).
     7OR.(NODE1.EQ.N6.AND.NODE2.EQ.N2.AND.NODE3.EQ.N1.AND.NODE4.EQ.N5))
     8THEN
      CALL INTER3(R(NUMGAU,I),1.D0,T(NUMGAU,J),2,1)
       GOTO 220
      ENDIF
 
 220         WDTS=W(NUMGAU,I)*W(NUMGAU,J)*DETJS
!   povrsinski fulks  j-na (1.30)                 
       DO K=1,NDIMEL
         RS2(K)=RS2(K)+H(K)*FS2*WDTS
       ENDDO

C     write(3,*) nbrel,i,j,R(IBRGT,I),S(IBRGT,J)
C     write(3,*) 'fs2,wdts,detjs',fs2,wdts,detjs
C     call wrr(h,ndim,'h   ')
C     call wrr(rs2,ndim,'rs2 ')
 225  CONTINUE
      ENDIF
 
      ENDIF
 250  CONTINUE

C      ENDIF
C KRAJ PETLJE AKO JE ITER>0
C=======================================================================
C zadata PRELAZNOST na povrsini elementa
C ----------------------------------------
!      MAXTQE - broj elemenata na kojima je zadata prelaznost
       IVODE=0
      DO 255 JBRPS=1,MAXTQE
      IF (NBREL.EQ.NELTOK(1,JBRPS)) THEN
      NODE1=NELTOK(2,JBRPS)
      NODE2=NELTOK(3,JBRPS)
      NODE3=NELTOK(4,JBRPS)
      NODE4=NELTOK(5,JBRPS)
      NODE5=NELTOK(6,JBRPS)
      NODE6=NELTOK(7,JBRPS)
      NODE7=NELTOK(8,JBRPS)
      NODE8=NELTOK(9,JBRPS)
      N1=NEL(1,NBREL)
      N2=NEL(2,NBREL)
      N3=NEL(3,NBREL)
      N4=NEL(4,NBREL)
      N5=NEL(5,NBREL)
      N6=NEL(6,NBREL)
      N7=NEL(7,NBREL)
      N8=NEL(8,NBREL)
      IPROM=NELTOK(12,JBRPS)
!       WRITE(3,*) "JBRPS,IPROM",JBRPS,IPROM
      IF(IPROM.EQ.1.OR.IPROM.EQ.3) THEN
        IVODE=NELTOK(10,JBRPS)/1000000
        IVRFH1=NELTOK(10,JBRPS)/1000-IVODE*1000
        IVRFH2=NELTOK(10,JBRPS)-IVODE*1000000-IVRFH1*1000
        
        IF(NELTOK(11,JBRPS)/1000.GE.1) THEN
         IVRFTOK1=NELTOK(11,JBRPS)/1000
         IVRFTOK2=NELTOK(11,JBRPS)-IVRFTOK1*1000
        ELSE
         IVRFTOK1=NELTOK(11,JBRPS)
         IVRFTOK2=0
        ENDIF
        IVRFVODE=WATER(3,IVODE)
        CALL TIMFUN (TABF,FWATER,VVREME,ITFMAX(IVRFVODE),IVRFVODE)
        IF(HFACE(JBRPS).GT.FWATER.OR.IVRFTOK2.EQ.0) THEN
            CALL TIMFUN (TABF,FK1TOK,VVREME,ITFMAX(IVRFTOK1),IVRFTOK1)
        ELSE
            CALL TIMFUN (TABF,FK1TOK,VVREME,ITFMAX(IVRFTOK2),IVRFTOK2)
        ENDIF
        IF(WATER(2,IVODE).NE.0) THEN
            IPOM111=WATER(2,IVODE)
            IVRFVODE_DEP=WATER(3,IPOM111)
!             WRITE(3,*) "IPOM111",IPOM111
!              WRITE(3,*) "IVRFVODE_DEP",IVRFVODE_DEP
!             IVRFVODE_DEP=WATER(3,WATER(2,IVODE))
            CALL TIMFUN (TABF,FWATER_DEP,VVREME,ITFMAX(IVRFVODE_DEP),
     1       IVRFVODE_DEP)
        ENDIF
      ELSE
        IVODE=NELTOK(10,JBRPS)/1000000
!         WRITE(*,*) "IPROM",IPROM
!           WRITE(3,*) "IVODE",IVODE
        IF(IVODE.LT.1) THEN
          IVRFH=NELTOK(10,JBRPS)
        ELSE
          IVRFH=NELTOK(10,JBRPS)-IVODE*1000000
          IF(WATER(2,IVODE).EQ.0) THEN
            IVRFVODE=WATER(3,IVODE)
            CALL TIMFUN (TABF,FWATER,VVREME,ITFMAX(IVRFVODE),IVRFVODE)
          ELSE
            IVRFVODE=WATER(3,IVODE)
!               WRITE(3,*) "IVRFVODE",IVRFVODE
            CALL TIMFUN (TABF,FWATER,VVREME,ITFMAX(IVRFVODE),IVRFVODE)
            IPOM111=WATER(2,IVODE)
            IVRFVODE_DEP=WATER(3,IPOM111)
!              WRITE(3,*) "IPOM111",IPOM111
!              WRITE(3,*) "IVRFVODE_DEP",IVRFVODE_DEP
!             IVRFVODE_DEP=WATER(3,WATER(2,IVODE))
            CALL TIMFUN (TABF,FWATER_DEP,VVREME,ITFMAX(IVRFVODE_DEP),
     1       IVRFVODE_DEP)
          ENDIF
        ENDIF
        IVRFTOK1=NELTOK(11,JBRPS)
        CALL TIMFUN (TABF,FK1TOK,VVREME,ITFMAX(IVRFTOK1),IVRFTOK1)
      ENDIF
!       WRITE(*,*) "pre racunanja bofanga"
!       
C Temperatura okoline je u funkciji vremena
!       
      IF(IPROM.EQ.2.OR.IPROM.EQ.3.AND.HFACE(JBRPS).LE.FWATER) THEN
!         IF(WATER(2,IVODE).EQ.0) THEN
!           IDWATER=WATER(1,IVODE)
!         ELSE
!           IDWATER=WATER(2,IVODE)
!         ENDIF
       
!         IDSENSOR(1)=0
!         IDSENSOR(2)=0
!         DISTSENSOR(1)=1.0D10
!         DISTSENSOR(2)=1.0D10
!         DO IBRBS=1,2
!           DO IBRSENSOR=1,NSENSORS
!             IF(IDWATER.EQ.SENSOR(2,IBRSENSOR)) THEN
!               IF(ABS(HSENSOR(IBRSENSOR)-HFACE(JBRPS)).LT.
!      1                            DISTSENSOR(IBRBS)) THEN  
!                 IF(IDSENSOR(1).EQ.0.OR.IBRBS.EQ.2.AND.
!      1                               IDSENSOR(1).NE.IBRSENSOR) THEN
!                   IDSENSOR(I)=IBRSENSOR
!                   DISTSENSOR(I)=ABS(HSENSOR(IBRSENSOR)-HFACE(JBRPS))
!                 ENDIF
!               ENDIF  
!             ENDIF
!           ENDDO
!         ENDDO
        IVRFSENSOR=SENSOR(3,1)
        CALL TIMFUN (TABF,FSENSOR(1),VVREME,ITFMAX(IVRFSENSOR)
     1                   ,IVRFSENSOR)
        IVRFSENSOR=SENSOR(3,2)
        CALL TIMFUN (TABF,FSENSOR(2),VVREME,ITFMAX(IVRFSENSOR)
     1                   ,IVRFSENSOR)
        CALL TIMFUN (TABF,FK1TOK,VVREME,ITFMAX(IVRFTOK1),IVRFTOK1)
        IF(WATER(2,IVODE).EQ.0) THEN
!           WRITE(3,*) "FWATER,HSENSOR(1),HSENSOR(2),HFACE(JBRPS)",FWATER
!      1            ,HSENSOR(1),HSENSOR(2),HFACE(JBRPS)
          CALL LININTGV(FWATER-HSENSOR(1),FWATER-HSENSOR(2),
     1     FSENSOR(1),FSENSOR(2),FK1TOK,
     2                          0,FWATER-HFACE(JBRPS))
!         WRITE(3,*) "FK1TOK,TBOFANG",FK1TOK,TBOFANG
        ELSE
!           WRITE(3,*) "FWATER_DEP,HSENSOR(1),HSENSOR(2),D_BOFANG",
!      1            FWATER_DEP,HSENSOR(1),HSENSOR(2),D_BOFANG
!           WRITE(3,*) "FSENSOR(1),FSENSOR(2)",FSENSOR(1),FSENSOR(2)
          CALL LININTGV(FWATER_DEP-HSENSOR(1),FWATER_DEP-HSENSOR(2),
     1     FSENSOR(1),FSENSOR(2),FK1TOK,
     2                      1,0)
!           WRITE(3,*) "TBOFANG,CBOFANG",TBOFANG,CBOFANG
!           CALL TEMP_DEMPENDANCE(FK1TOK,TBOFANG,CBOFANG)
!           WRITE(3,*) "FK1TOK",FK1TOK
        ENDIF
      ENDIF
!       WRITE(*,*) "posle racunanja bofanga"
!        write(3,*) "HSENSOR(1)",HSENSOR(1)
!        write(3,*) "HSENSOR(2)",HSENSOR(2)
!        write(3,*) "FSENSOR(1)",FSENSOR(1)
!        write(3,*) "FSENSOR(2)",FSENSOR(2)
!        write(3,*) "Zavisna voda",WATER(2,IVODE)
!        write(3,*) "FK1TOK",FK1TOK
!       FPS1=POVSIL(1,JBRPS)*FK1
!       FPS2=POVSIL(2,JBRPS)*FK1
!       FPS3=POVSIL(3,JBRPS)*FK1
!       FPS4=POVSIL(4,JBRPS)*FK1
C
          TEMP=0.0
          TTT=0.0
!       write(3,*) "NBREL",NBREL
!       write(3,*) "IVRFH",IVRFH
!       write(3,*) "IVRFTOK",IVRFTOK
!       write(3,*) "FK1TOK",FK1TOK
      NUMGAU=IBRGT
!  proveriti prelaznost za 1D element      
      IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
        DO I=1,NUMGAU
          CALL INTERP1D(R(IBRGT,I),0,0,NDIMEL,AJS)  
          WDTV=W(IBRGT,I)*thick(NBREL)*AJS
C odredjivanje temperature u Gausovoj tacki i Tok-T
          TEMP=0.0
          TTT=0.0
          DO K=1,NDIMEL
            TEMP=TEMP+H(K)*TT210(K)
            TTT=TTT+H(K)*FK1TOK
!             TTT=TTT+H(K)*(FK1TOK-TT21(K))
          ENDDO
C odredjivanje koeficijenta prelaznosti HHP u zavisnosti od temperature
          IF(IPROM.EQ.1.AND.IPROM.EQ.3) THEN
            IF(HFACE(JBRPS).GT.FWATER) THEN
                CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH1),IVRFH1)
            ELSE
                CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH2),IVRFH2)
            ENDIF
          ELSE
            CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH),IVRFH)
          ENDIF
          DO K=1,NDIMEL
            RS2(K)=RS2(K)+H(K)*HHP*WDTV*TTT
          ENDDO
        ENDDO
C  (1.25)  
        DO K=1,NDIMEL
         DO N=1,NDIMEL
          AFIFI1(K,N)=AFIFI1(K,N)+HHP*H(K)*H(N)*WDTV
         ENDDO 
        ENDDO 
        GOTO 255
      ENDIF
C  
!  ugraditi prelaznost za 2D elemente
!  prelaznost za tetra elemente
      IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
         DO NGXT = 1,3
             RT=1./6
             ST=1./6
             WT=1./3
             IF(NGXT.EQ.2) RT=2./3
             IF(NGXT.EQ.3) ST=2./3
C
!               CALL INTERPTETRAF(RT,ST,TT,KFIX,NDIMEL)
              CALL JACTP33(RT,ST,HT,1,NELTOK,JBRPS,CORD,NDIMEL)
              WDTS = WT*DETJS/2
! C odredjivanje temperature u Gausovoj tacki i Tok-T
             TEMP=0.0
             TTT=0.0
             DO K=2,8
                DO J=1,NDIMEL
                  IF(NELTOK(K,JBRPS).EQ.NEL(J,NBREL)) THEN
                    IF(LINTE.EQ.0) THEN
                     TEMP=TEMP+HT(K-1,1)*TT210(J)
!  za (1.40)                    
                     TTT=TTT+HT(K-1,1)*FK1TOK
                    ELSE
                     TEMP=TEMP+HT(K-1,1)*TT21(J)
!  za  (1.31)                    
                     TTT=TTT+HT(K-1,1)*(FK1TOK-TT21(J))
                    ENDIF
                  ENDIF
                ENDDO
             ENDDO
             
! C odredjivanje koeficijenta prelWRITE(*,*) "pre racunanja bofanga"aznosti HHP u zavisnosti od temperature
C odredjivanje koeficijenta prelaznosti HHP u zavisnosti od temperature
          IF(IPROM.EQ.1.OR.IPROM.EQ.3) THEN
            IF(HFACE(JBRPS).GT.FWATER) THEN
                CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH1),IVRFH1)
            ELSE
                CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH2),IVRFH2)
            ENDIF
          ELSE
            CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH),IVRFH)
          ENDIF
C
             DO K=2,8
                DO J=1,NDIMEL
                    IF(NELTOK(K,JBRPS).EQ.NEL(J,NBREL)) THEN
!  vektor konvekcije (1.31), (1.40)                    
                        RS2(J)=RS2(J)+HT(K-1,1)*TTT*HHP*WDTS
                    ENDIF
                ENDDO
            ENDDO
! (1.25) i (1.37) - matrica konvekcije 
            DO J=1,NDIMEL
             DO K=2,8
              IF(NELTOK(K,JBRPS).EQ.NEL(J,NBREL)) then
               INDEKS(K)=J
              ENDIF
             ENDDO
            ENDDO
            DO K=2,8
             IF(K.NE.5) THEN
             DO J=1,NDIMEL
               DO N=2,8
                IF(NELTOK(N,JBRPS).EQ.NEL(J,NBREL)) THEN
                 AFIFI1(INDEKS(N),J)=AFIFI1(INDEKS(N),J)+HHP*HT(K-1,1)*
     1                   HT(N-1,1)*WDTS
                ENDIF
               ENDDO
             ENDDO
             ENDIF
            ENDDO 
!          
         ENDDO 
!          WRITE(*,*) "posle tetri"
!  prelaznost za heksa elemente         
      ELSEIF (elemtip(NBREL).eq.38.or.elemtip(NBREL).eq.320) THEN
      
      DO 2250 I=1,NUMGAU
      DO 2250 J=1,NUMGAU
C
C KONSTANTNO T (ZETA)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N2.AND.NODE3.EQ.N3.AND.NODE4.EQ.N4).
     1OR.(NODE1.EQ.N2.AND.NODE2.EQ.N3.AND.NODE3.EQ.N4.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N3.AND.NODE2.EQ.N4.AND.NODE3.EQ.N1.AND.NODE4.EQ.N2).
     3OR.(NODE1.EQ.N4.AND.NODE2.EQ.N1.AND.NODE3.EQ.N2.AND.NODE4.EQ.N3).
C
     4OR.(NODE1.EQ.N1.AND.NODE2.EQ.N4.AND.NODE3.EQ.N3.AND.NODE4.EQ.N2).
     5OR.(NODE1.EQ.N4.AND.NODE2.EQ.N3.AND.NODE3.EQ.N2.AND.NODE4.EQ.N1).
     6OR.(NODE1.EQ.N3.AND.NODE2.EQ.N2.AND.NODE3.EQ.N1.AND.NODE4.EQ.N4).
     7OR.(NODE1.EQ.N2.AND.NODE2.EQ.N1.AND.NODE3.EQ.N4.AND.NODE4.EQ.N3))
     8THEN
      CALL INTER3(R(NUMGAU,I),S(NUMGAU,J),1.D0,3,1)
       GOTO 2200
      ENDIF 
C
C KONSTANTNO T (ZETA)
C 
      IF((NODE1.EQ.N5.AND.NODE2.EQ.N6.AND.NODE3.EQ.N7.AND.NODE4.EQ.N8).
     1OR.(NODE1.EQ.N6.AND.NODE2.EQ.N7.AND.NODE3.EQ.N8.AND.NODE4.EQ.N5).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N8.AND.NODE3.EQ.N5.AND.NODE4.EQ.N6).
     3OR.(NODE1.EQ.N8.AND.NODE2.EQ.N5.AND.NODE3.EQ.N6.AND.NODE4.EQ.N7).
C
     5OR.(NODE1.EQ.N5.AND.NODE2.EQ.N8.AND.NODE3.EQ.N7.AND.NODE4.EQ.N6).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N7.AND.NODE3.EQ.N6.AND.NODE4.EQ.N5).
     7OR.(NODE1.EQ.N7.AND.NODE2.EQ.N6.AND.NODE3.EQ.N5.AND.NODE4.EQ.N8).
     8OR.(NODE1.EQ.N6.AND.NODE2.EQ.N5.AND.NODE3.EQ.N8.AND.NODE4.EQ.N7))
     8THEN
      CALL INTER3(R(NUMGAU,I),S(NUMGAU,J),-1.D0,3,1)
       GOTO 2200
      ENDIF 
C 
C KONSTANTNO R (KSI)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N4.AND.NODE3.EQ.N8.AND.NODE4.EQ.N5).
     1OR.(NODE1.EQ.N4.AND.NODE2.EQ.N8.AND.NODE3.EQ.N5.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N8.AND.NODE2.EQ.N5.AND.NODE3.EQ.N1.AND.NODE4.EQ.N4).
     3OR.(NODE1.EQ.N5.AND.NODE2.EQ.N1.AND.NODE3.EQ.N4.AND.NODE4.EQ.N8).
C
     4OR.(NODE1.EQ.N1.AND.NODE2.EQ.N5.AND.NODE3.EQ.N8.AND.NODE4.EQ.N4).
     5OR.(NODE1.EQ.N5.AND.NODE2.EQ.N8.AND.NODE3.EQ.N4.AND.NODE4.EQ.N1).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N4.AND.NODE3.EQ.N1.AND.NODE4.EQ.N5).
     7OR.(NODE1.EQ.N4.AND.NODE2.EQ.N1.AND.NODE3.EQ.N5.AND.NODE4.EQ.N8))
     8THEN
      CALL INTER3(1.D0,S(NUMGAU,I),T(NUMGAU,J),1,1)
       GOTO 2200
      ENDIF 
C 
C KONSTANTNO R (KSI)
C
      IF((NODE1.EQ.N2.AND.NODE2.EQ.N3.AND.NODE3.EQ.N7.AND.NODE4.EQ.N6).
     1OR.(NODE1.EQ.N3.AND.NODE2.EQ.N7.AND.NODE3.EQ.N6.AND.NODE4.EQ.N2).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N6.AND.NODE3.EQ.N2.AND.NODE4.EQ.N3).
     3OR.(NODE1.EQ.N6.AND.NODE2.EQ.N2.AND.NODE3.EQ.N3.AND.NODE4.EQ.N7).
C
     4OR.(NODE1.EQ.N2.AND.NODE2.EQ.N6.AND.NODE3.EQ.N7.AND.NODE4.EQ.N3).
     5OR.(NODE1.EQ.N6.AND.NODE2.EQ.N7.AND.NODE3.EQ.N3.AND.NODE4.EQ.N2).
     6OR.(NODE1.EQ.N7.AND.NODE2.EQ.N3.AND.NODE3.EQ.N2.AND.NODE4.EQ.N6).
     7OR.(NODE1.EQ.N3.AND.NODE2.EQ.N2.AND.NODE3.EQ.N6.AND.NODE4.EQ.N7))
     8THEN
      CALL INTER3(-1.D0,S(NUMGAU,I),T(NUMGAU,J),1,1)
       GOTO 2200
      ENDIF 
C 
C KONSTANTNO S (ETA)
C
      IF((NODE1.EQ.N4.AND.NODE2.EQ.N3.AND.NODE3.EQ.N7.AND.NODE4.EQ.N8).
     1OR.(NODE1.EQ.N3.AND.NODE2.EQ.N7.AND.NODE3.EQ.N8.AND.NODE4.EQ.N4).
     2OR.(NODE1.EQ.N7.AND.NODE2.EQ.N8.AND.NODE3.EQ.N4.AND.NODE4.EQ.N3).
     3OR.(NODE1.EQ.N8.AND.NODE2.EQ.N4.AND.NODE3.EQ.N3.AND.NODE4.EQ.N7).
C
     4OR.(NODE1.EQ.N3.AND.NODE2.EQ.N4.AND.NODE3.EQ.N8.AND.NODE4.EQ.N7).
     5OR.(NODE1.EQ.N4.AND.NODE2.EQ.N8.AND.NODE3.EQ.N7.AND.NODE4.EQ.N3).
     6OR.(NODE1.EQ.N8.AND.NODE2.EQ.N7.AND.NODE3.EQ.N3.AND.NODE4.EQ.N4).
     7OR.(NODE1.EQ.N7.AND.NODE2.EQ.N3.AND.NODE3.EQ.N4.AND.NODE4.EQ.N8))
     8THEN
      CALL INTER3(R(NUMGAU,I),-1.D0,T(NUMGAU,J),2,1)
       GOTO 2200
      ENDIF 
C 
C KONSTANTNO S (ETA)
C
      IF((NODE1.EQ.N1.AND.NODE2.EQ.N2.AND.NODE3.EQ.N6.AND.NODE4.EQ.N5).
     1OR.(NODE1.EQ.N2.AND.NODE2.EQ.N6.AND.NODE3.EQ.N5.AND.NODE4.EQ.N1).
     2OR.(NODE1.EQ.N6.AND.NODE2.EQ.N5.AND.NODE3.EQ.N1.AND.NODE4.EQ.N2).
     3OR.(NODE1.EQ.N5.AND.NODE2.EQ.N1.AND.NODE3.EQ.N2.AND.NODE4.EQ.N6).
C
     4OR.(NODE1.EQ.N2.AND.NODE2.EQ.N1.AND.NODE3.EQ.N5.AND.NODE4.EQ.N6).
     5OR.(NODE1.EQ.N1.AND.NODE2.EQ.N5.AND.NODE3.EQ.N6.AND.NODE4.EQ.N2).
     6OR.(NODE1.EQ.N5.AND.NODE2.EQ.N6.AND.NODE3.EQ.N2.AND.NODE4.EQ.N1).
     7OR.(NODE1.EQ.N6.AND.NODE2.EQ.N2.AND.NODE3.EQ.N1.AND.NODE4.EQ.N5))
     8THEN
      CALL INTER3(R(NUMGAU,I),1.D0,T(NUMGAU,J),2,1)
       GOTO 2200
      ENDIF
 2200  WDTS=W(NUMGAU,I)*W(NUMGAU,J)*DETJS
       TTT=0.0
       TEMP=0.0
       DO K=1,NDIMEL
         IF(LINTE.EQ.0) THEN
            TEMP=TEMP+H(K)*TT210(K)
!  za (1.40)                    
             TTT=TTT+H(K)*FK1TOK
           ELSE
            TEMP=TEMP+H(K)*TT21(K)
!  za  (1.31)                    
             TTT=TTT+H(K)*(FK1TOK-TT21(K))
           ENDIF
       ENDDO
C odredjivanje koeficijenta prelaznosti HHP u zavisnosti od temperature
          CALL TIMFUN (TABF,HHP,TEMP,ITFMAX(IVRFH),IVRFH)
C vektor konvekcije (1.31), (1.40)
       DO K=1,NDIMEL
         RS2(K)=RS2(K)+H(K)*HHP*TTT*WDTS
       ENDDO
! (1.25) i (1.37) matrica konvekcije 
         DO K=1,NDIMEL
          DO N=1,NDIMEL
           AFIFI1(K,N)=AFIFI1(K,N)+HHP*H(K)*H(N)*WDTS
          ENDDO 
         ENDDO 
C          
 2250  CONTINUE
      ENDIF

      ENDIF
 255  CONTINUE

C kraj dela za prelaznost

C PAKOVANJE POVRSINSKIH SILA SA DESNE STRANE
 265   DO I=1,NDIMEL
         F36(I)=F36(I)+RS2(I)
       ENDDO
C=========================================================================
      DO I=1,NDIMEL
      DO J=1,NDIMEL
       AK(I,J)=0.D0
      ENDDO
      ENDDO
C PAKOVANJE MATRICA AMUU,AMUW,AMWW,AKUU,AKUW,AKWW U MATRICE AMM,AC,AK
      DO I=1,NDIMEL
      DO J=1,NDIMEL
        AK(I,J)=AFIFI(I,J)+AFIFI1(I,J)
        IF (NSTAC.EQ.0) AK(I,J)=AK(I,J)+AM1(I,J)/TIME
      ENDDO
      ENDDO
C============================================================================
C PAKOVANJE MATRICE AK U MATRICU SKEF
!       WRITE(*,*) "pre pakovanja AK matrice u skef"
      DO 263 I=1,NDIMEL
      DO 262 J=1,NDIMEL
        SKEF(I,J)=AK(I,J)
  262 CONTINUE
  263 CONTINUE
C============================================================================
C PAKOVANJE Kfifi*fi(i-1) SA DESNE STRANE
      IF (NSTAC.EQ.0) THEN
!  VEKTOR TOPLOTNOG KAPACITETA (1.28) ili (1.39)      
       DO I=1,NDIMEL
        DO J=1,NDIMEL
         IF(LINTE.EQ.0) THEN
! (1.39)      
           F36(I)=F36(I)+AM1(I,J)*TT210(J)/TIME
         ELSE
! (1.28)      
          F36(I)=F36(I)-(AM1(I,J)*(TT21(J)-TT210(J)))/TIME
         ENDIF
        ENDDO
       ENDDO
      ENDIF
! 
!  (1.33) vektor kondukcije      
! 
      IF (LINTE.EQ.1) THEN
       DO I=1,NDIMEL
        DO J=1,NDIMEL
          F36(I)=F36(I)-AFIFI(I,J)*TT21(J)
        ENDDO
       ENDDO
      ENDIF
C============================================================================
C VEKTOR PROTOKA PO CVOROVIMA
!       DO I=1,NDIMEL
!        N=NEL(I,NBREL)
!        JJ=ID(1,N)
! !        IF(JJ.GT.0) THEN
! ! 
! !          POT=0.D0
! ! !         DO J=1,NDIMEL
! ! ! 
! ! ! !          POT=POT+(AFIFI(I,J)-AFIFI1(I,J))*TT21(J)
! ! ! cz uporediti resenja sa ovim uslovom za nestacionarni protok
! ! ! cz proveriti resenja za zasic=0.
! ! ! !       	IF (NSTAC.NE.1) THEN
! ! ! !           POT=POT+((AM1(I,J)-PUS(I,J))*(TT21(J)-TT210(J)))/TIME
! ! ! !         ENDIF
! ! ! 
! ! !         ENDDO
! !          UBRZ(JJ)=UBRZ(JJ)+POT
! !        ENDIF
!       ENDDO
C PAKOVANJE MATRICE ELEMENTA I VEKTRA DESNE STRANE U SISTEM
!        WRITE(*,*) "pre pakovanja desne strane"
       CALL PSKEFN(SKEF,SKEFN,NDIMEL)
!        WRITE(*,*) "posle PSKEFN"
       CALL SPAKDE(SILE,F36,LM2,NDIMEL)
!        WRITE(*,*) "posle SPAKDE"
      IF (ISOLVER.EQ.0) THEN
        IF (MAXCUR.GT.0) THEN
          CALL SPAKUJ(A(LSK),MAXA,SKEFN,LM2,NDIMEL)
        ELSE
cz ovaj uslov ne valja jer ne radi levu stranu za full newton      
C        IF (ITER.EQ.0.OR.NJRAP.EQ.2) 
          IF (ITER.EQ.0) 
     +                  CALL SPAKUJ(A(LSK),MAXA,SKEFN,LM2,NDIMEL)
        ENDIF
      ELSEIF ((ISOLVER.EQ.-11).OR.(ISOLVER.EQ.1)) THEN
C      IMSL Sparse OR MUMPS
        IF(ITER.EQ.0) THEN
C         CALL SPARSE_ELEM_LEFT(SKEF,NEL,ID,NDES,NBREL,NDIM,1)
          CALL sparseassembler_addelemmatrix(NDES,LM2,SKEF)
        ENDIF
      ENDIF
!       WRITE(*,*) "pre 400 continue"
C=======================================================================
C KRAJ PETLJE PO ELEMENTIMA
C=======================================================================
 400  CONTINUE
!       WRITE(*,*) "posle petlje po elementima"
C=======================================================================
C CVOROVI NA LINIJI PROCURIVANJA       
C=======================================================================
  	 IF (MAXCUR.GT.0) THEN
        CALL CURI1(CORD,A(LSK),SILE,MAXA,TT1,ID,ICUR)
C       ELSE
C        IF(ITER.EQ.0) CALL CURI1(CORD,A(LSK),SILE,MAXA,TT1,ID,ICUR)
       ENDIF
C==========================================================================
C MUMPS alociranje matrice krutosti
1235  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(ITER,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(ISOLVER,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(NBLOCK,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(MAXCUR,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_BCAST(KKORAK,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        IF (ITER.EQ.0) THEN

         IF(ISOLVER.EQ.0) THEN
          if(myid.eq.0) then
          IF (NBLOCK.GT.1) THEN
           LLM =LRAD
           LSKE=LLM+100
           CALL SPAKUA(A(LSK),MAXA,A(LSKE),LM2,NDES,0,
     &                        A(LMNQ),A(LLREC),NBLOCK,LR,IBLK,H,NZAD)
           CLOSE (ISCRC,STATUS='KEEP')
          ENDIF
          endif
         ELSEIF((ISOLVER.EQ.-11).OR.(ISOLVER.EQ.1)) THEN
          
          if(.not.allocated(rows)) then
            if(myid.eq.0) call sparseassembler_getnz(nonzeros)
            CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
            CALL MPI_BCAST(nonzeros,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            allocate(rows(nonzeros),STAT=istat)
            if(istat.ne.0) stop 'error allocating rows'
            allocate(columns(nonzeros),STAT=istat)
            if(istat.ne.0) stop 'error allocating columns'
            allocate(stiff(nonzeros),STAT=istat)
            if(istat.ne.0) stop 'error allocating stiff'
          endif
C           IMSL Sparse
c            CALL SPARSE_BUILD(ISPARSE_N,ISPARSE_NZ,
c     1                DSPARSE_A,ISPARSE_ROWS,ISPARSE_COLS)
          if(myid.eq.0) CALL sparseassembler_getsparse(nonzeros,
     1                 rows,columns,stiff)

          if(myid.eq.0) CALL sparseassembler_kill()

          IF(KKORAK.EQ.1) THEN
              stiff_n = JEDN
C Komentirao Milos 20080121
C            CALL MUMPS_INIT(ISPARSE_N,ISPARSE_NZ)
          ENDIF
         ENDIF
        ENDIF
C==========================================================================
! 	IF (MAXCUR.GT.0) THEN
! 	 IF(ISOLVER.EQ.0) THEN
!           if(myid.eq.0) CALL ZADLEV(A(LSK),MAXA,A(LNZADJ),NUMZAD,NZAD)
!          ELSE
!           if(myid.eq.0) CALL SPARS_ZADLEV(A(LNZADJ),NUMZAD,NZAD)
!          ENDIF
!          CALL RESEN(A(LSK),TT10,MAXA,JEDN,1)
!         ELSE
cz proveriti i ovde uslov za full newton        
C        IF (ITER.EQ.0.OR.NJRAP.EQ.2) THEN
        IF (ITER.EQ.0) THEN
cz proveriti rad sa blokovima i zadlev
          if(myid.eq.0) then
          IF (NBLOCK.GT.1) THEN
            LLM =LRAD
            LSKE=LLM+100
            CALL SPAKUA(A(LSK),MAXA,A(LSKE),LM2,NDES,0,
     &          A(LMNQ),A(LLREC),NBLOCK,LR,IBLK,H,NZAD)
            CLOSE (ISCRC,STATUS='KEEP')

          ENDIF
          endif
          
          IF(ISOLVER.EQ.0) THEN
            if(myid.eq.0) CALL ZADLEV(A(LSK),MAXA,A(LNZADJ),NUMZAD,NZAD)
          ELSE
            if(myid.eq.0) CALL SPARS_ZADLEV(A(LNZADJ),NUMZAD,NZAD,
     1                                  CORD,VVREME,TABF,NTABFT,ITFMAX)
          ENDIF
          CALL RESEN(A(LSK),TT10,MAXA,JEDN,1)
        ENDIF
!         ENDIF

       TOL=1.0D-7
!        WRITE(*,*) "pre petlje po zadatim vrednostima"
C=======================================================================
       DO 410 I=1,NUMZAD
c       write(iizlaz,*)'fk1=',fk1
c       do ii=1,2
c        write(iizlaz,*)TABF(1,1,Ii),TABF(2,1,Ii)
c       enddo
       IF(NZAD(2,I).NE.0) THEN
        CALL TIMFUN(TABF,FK1,VVREME,ITFMAX(NZAD(3,I)),NZAD(3,I))
       ELSE
        IF(NZAD(3,I).EQ.1) THEN
          INTERPAXIS=1
          CALL PRED_INTERP(I,INTERPAXIS,NUMAXISPTSX,INTAXISPOINTX,
     1                    ITFMAX,NZAD,XAXISPTCORD,CORD,TABF,VVREME,FK1)
        ENDIF
        IF(NZAD(3,I).EQ.2) THEN
          INTERPAXIS=2
          CALL PRED_INTERP(I,INTERPAXIS,NUMAXISPTSY,INTAXISPOINTY,
     1                    ITFMAX,NZAD,YAXISPTCORD,CORD,TABF,VVREME,FK1)
        ENDIF
        IF(NZAD(3,I).EQ.3) THEN
          INTERPAXIS=3
          CALL PRED_INTERP(I,INTERPAXIS,NUMAXISPTSZ,INTAXISPOINTZ,
     1                    ITFMAX,NZAD,ZAXISPTCORD,CORD,TABF,VVREME,FK1)
        ENDIF

       ENDIF

c       write(iizlaz,*)'fk1=',fk1, vvreme,ITFMAX(NZAD(3,I)),NZAD(3,I)
c       do ii=1,2
c        write(iizlaz,*)TABF(1,1,Ii),TABF(2,1,Ii)
c       enddo
C===========================================================================
C KONSTANTNI POTENCIJALI
!         IF( (NZAD(2,I).EQ.1.OR.(NZAD(2,I).EQ.3.AND.KKORAK.EQ.1))
!      &                 .AND.(ITER.EQ.0).AND.(ID(1,NZAD(1,I)).GT.0)) THEN
cz oduzimanje zadate vrednosti od prethodno izracunate             
         IF(LINTE.EQ.0) THEN
            SILE(ID(1,NZAD(1,I)))=1.D035*ZADVRE(I)*FK1
         ELSE
             SILE(ID(1,NZAD(1,I)))=1.D035*
     1                   (ZADVRE(I)*FK1-TT1(ID(1,NZAD(1,I))))
         ENDIF
!        ENDIF
C===========================================================================
  410  CONTINUE

c         call wrr(SILE,JEDN,'s-p')
      CALL RESEN(A(LSK),SILE,MAXA,JEDN,2)
c         call wrr(SILE,JEDN,'s-k')
c     OVDE UCITATI RESENJA SA DISKA U VEKTOR SILE
c
!   999 IF(NXNASTRAN.EQ.1) THEN
! C
!             NXNAS=99    
!             OPEN (NXNAS,FILE='POTNXNAS',STATUS='OLD',FORM='FORMATTED',
!      1            ACCESS='SEQUENTIAL')
!             DO I=1,JEDN
!                READ(NXNAS,*) IDUM,SILE(I)
!                write(3,4000) IDUM,SILE(I)
!  4000 format(i5,f15.5)               
!                IF(IDUM.NE.I) STOP 'IDUM.NE.I  PAKV3D.FOR'
!             ENDDO
!             CLOSE(NXNAS,STATUS='KEEP')
!       ENDIF
! 
C
           DO 440 I=1,JEDN
             IF(LINTE.EQ.0) THEN
                TT1(I)=SILE(I)
             ELSE
                TT1(I)=TT1(I)+SILE(I)
             ENDIF
 440       CONTINUE
!  Sneza 31.03.2017.
!  Ako radi stacionarnu linearnu analizu ima samo jedan prolaz, ne proverava se konvergenicja
        WRITE(*,*)'period,korak=',NNPER,KORAK
        IF(LINTE.EQ.0) goto 491
! 
!       WRITE(*,*)'ITER= ',ITER
!       WRITE(*,*)'PERIOD= ',NNPER
!       WRITE(*,*)'STEP= ',KKORAK
!       IF(NXNASTRAN.EQ.1) go to 491
C
      CALL KONVTF(TT1,SILE,KONVV2,1,ID,ITER)
C
        IF (KKORAK.GT.1) THEN
C
C        CALL CKONV1(ICUR,TT1,CORD,ID,KONV1,UBRZ)
c         proba da nasilno ne radi iteracije
cz          nasilu=0
cz          if(nstac.eq.1.and.iter.eq.0) nasilu=0
cz          write(*,*) ' nasilu',nasilu
cz          if(nasilu.eq.0) then
          IF (KONVV2*KONV1.EQ.0) THEN
            ITER=ITER+1
            WRITE(IIZLAZ,*)'ITER=',ITER
            GO TO 100
          ENDIF
        endif 

        IF ((NJUTN.NE.0).AND.(INDOPT.EQ.0)) RETURN

C======================================================================= 
C RACUNANJE GRADIJENTA Temperature
C=======================================================================
C  PETLJA PO ELEMENTIMA
C
 491  DO 500 NBREL=1,NET
      IF(elemtip(NBREL).gt.100) THEN
        NTYPE=elemtip(NBREL)/100
        NTIMES=100
      ELSE
        NTYPE=elemtip(NBREL)/10
        NTIMES=10
      ENDIF
      NDIMEL=elemtip(NBREL)-NTIMES*NTYPE
      
      VZAPR=0.
      NMAT=NEL(NDIM+1,NBREL)
      NEMA=NEL(NDIM+2,NBREL)

      IF (NMAT.LE.0) GOTO 500
      IF (NEMA.GT.0) GOTO 500
 
      CALL CLEARD(TT21,NDIM)
      CALL PREB(TT21,TT1,ID,NEL,NBREL)
      CALL CLEARD(TT210,NDIM)
      CALL PREB(TT210,TT10,ID,NEL,NBREL)
C
      DO KLM=1,NDIMEL
       CK(1,KLM)=CORD(1,NEL(KLM,NBREL))
       CK(2,KLM)=CORD(2,NEL(KLM,NBREL))
       CK(3,KLM)=CORD(3,NEL(KLM,NBREL))
      ENDDO
      
      if(NTYPE.eq.2) CALL LCK2D(CK,CKL,TTE,NDIMEL)

       DO KL=1,NDIMEL
        FZAP(1,KL)=0.D0
        FZAP(2,KL)=0.D0
        FZAP(3,KL)=0.D0
       ENDDO
C
C INDIKATOR NJUTN-KOTESOVE INTEGRACIJE (0-NE,1-DA)
      KOTES=0
      NGAUS=0
      IBRGT=2
      IBRKT=2
      IF(KOTES.EQ.1) IBRKT=3
      
        IBRKT1=IBRKT
        IBRKT2=IBRKT
        if(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) THEN
          IBRKT2=1
          IBRKT1=4
        ENDIF
        IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) IBRKT2=1
        IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) IBRKT1=1
        IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
          IBRKT2=1
          IBRKT1=3
        ENDIF
        DO  320 I=1,IBRKT1
        DO  320 J=1,IBRKT2
        DO  320 K=1,IBRKT2
        NGAUS=NGAUS+1
        PORNIEL(NBREL,NGAUS)=0.D0
        
	IF(KOTES.EQ.1) THEN
          IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) then
            CALL GETRST(RT,ST,TT,WTET,NGAUS,IBRKT1)
            CALL INTERPTETRA(RT,ST,TT,0,0,NDIMEL)
            WDT=WTET(1)*WTET(2)*WTET(3)*DETJ
          ELSE IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
            CALL INTERP1D(RK(IBRKT1,I),0,0,NDIMEL,AJS)
            WDT=WK(IBRKT1,I)*thick(NBREL)*AJS
          ELSE IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) THEN
            CALL INTERP2D48(R(IBRGT,J),S(IBRGT,K),0,0,NDIMEL,CKL,TTE)
            WDT=W(IBRGT,J)*W(IBRGT,K)*DETJ*thick(NBREL)
          ELSE IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
            RT=1./6
            ST=1./6
            WT=1./3
            IF(NGAUS.EQ.2) RT=2./3
            IF(NGAUS.EQ.3) ST=2./3
            CALL INTERP2D36(RT,ST,0,0,NDIMEL,CKL,TTE)
            WWDT = WT*thick(NBREL)*DETJ/2
          ELSE
            CALL INTER3(RK(IBRGT,I),SK(IBRGT,J),TK(IBRGT,K),0,0)
            WDT=WK(IBRKT,I)*WK(IBRKT,J)*WK(IBRKT,K)*DETJ
          ENDIF
	ELSE
          IF(elemtip(NBREL).eq.34.or.elemtip(NBREL).eq.310) then
            CALL GETRST(RT,ST,TT,WTET,NGAUS,IBRKT1)
            CALL INTERPTETRA(RT,ST,TT,0,0,NDIMEL)
            WDT=WTET(1)*WTET(2)*WTET(3)*DETJ
          ELSE IF(elemtip(NBREL).eq.12.or.elemtip(NBREL).eq.13) THEN
            CALL INTERP1D(R(IBRKT1,I),0,0,NDIMEL,AJS)
            WDT=W(IBRKT1,I)*thick(NBREL)*AJS
          ELSE IF(elemtip(NBREL).eq.24.or.elemtip(NBREL).eq.28) THEN
!             CALL INTERP2D48(R(IBRGT,I),S(IBRGT,J),0,0,NDIMEL,CKL,TTE)
!             WDT=W(IBRGT,I)*W(IBRGT,J)*DETJ*thick(NBREL)
              CALL INTERP2D48(R(IBRGT,J),S(IBRGT,K),0,0,NDIMEL,CKL,TTE)
              WDT=W(IBRGT,J)*W(IBRGT,K)*DETJ*thick(NBREL)
          ELSE IF(elemtip(NBREL).eq.23.or.elemtip(NBREL).eq.26) THEN
            RT=1./6
            ST=1./6
            WT=1./3
            IF(NGAUS.EQ.2) RT=2./3
            IF(NGAUS.EQ.3) ST=2./3
            CALL INTERP2D36(RT,ST,0,0,NDIMEL,CKL,TTE)
            WDT = WT*thick(NBREL)*DETJ/2
          ELSE
            CALL INTER3(R(IBRGT,I),S(IBRGT,J),T(IBRGT,K),0,0)
            WDT=W(IBRKT,I)*W(IBRKT,J)*W(IBRKT,K)*DETJ
          ENDIF
	ENDIF
        VZAPR=VZAPR+WDT
C
C OKVASENOST U GAUS TACKI - nema pornih pritisaka u termici
!         PR=0.D0
!         DO II=1,NDIMEL
!            PR=PR+H(II)*(TT21(II)-CORD(IOSA,NEL(II,NBREL)))
!        ENDDO
!         OKVAS=0.
!         IF(PR.GT.0.D0) OKVAS=1.
! c porni pritisak u gaus tacki
!         porni=pr*gama
!         IF(PR.GT.0.D0) PORNIEL(NBREL,NGAUS)=porni
C      write(iizlaz,*)NBREL,NGAUS,PORNIEL(NBREL,NGAUS)
C=======================================================================
C RACUNANJE GRADIJENTA TEMPERATURE:
C
      GRADFX=0.D0
      GRADFY=0.D0
      GRADFZ=0.D0
C ISKLJUCENO DA NE RACUNA FILTRACIONE SILE I SILE OD PORNIH PRITISAKA      
!       IF(PR.GT.0.D0) THEN
C      IF(PR.GT.0.D0.OR.ISIL.EQ.0) THEN
C RACUNANJE GRADIJENTA POTENCIJALA
          DO II=1,NDIMEL
            GRADFX=GRADFX+ZVHX(II)*TT21(II)
            GRADFY=GRADFY+ZVHY(II)*TT21(II)
            GRADFZ=GRADFZ+ZVHZ(II)*TT21(II)
          ENDDO
C
C         
!       ENDIF

!       IF ( (PR.LT.0.D0) .AND. (INDFS.EQ.1) )THEN
!          AKX=(1.D0-ZASIC)*AKONST(3,1,NMAT)
!          AKY=(1.D0-ZASIC)*AKONST(3,2,NMAT)
!          AKZ=(1.D0-ZASIC)*AKONST(3,3,NMAT)
!       ELSE
         AKX=AKONST(3,1,NMAT)
         AKY=AKONST(3,2,NMAT)
         AKZ=AKONST(3,3,NMAT)
!       ENDIF

C GRADIJENT U GAUS TACKI
       AJX1(NGAUS)=GRADFX
       AJY1(NGAUS)=GRADFY
       AJZ1(NGAUS)=GRADFZ
       GG(1,NBREL,NGAUS)=GRADFX
       GG(2,NBREL,NGAUS)=GRADFY
       GG(3,NBREL,NGAUS)=GRADFZ
C BRZINA U GAUS TACKI
       AJX(NGAUS)=-AKX*GRADFX
       AJY(NGAUS)=-AKY*GRADFY
       AJZ(NGAUS)=-AKZ*GRADFZ
       VG(1,NBREL,NGAUS)=-AKX*GRADFX
       VG(2,NBREL,NGAUS)=-AKY*GRADFY
       VG(3,NBREL,NGAUS)=-AKZ*GRADFZ
C
  320   CONTINUE
!       CASE DEFAULT
!         WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
!       END SELECT
      
      IMCVOR=0
      IVAL=0
      SELECT CASE(elemtip(NBREL))
      CASE(12)
        IVAL=2
      CASE(13)
        IVAL=2
        IMCVOR=1
      CASE(23)
        IVAL=3
      CASE(26)
        IVAL=3
        IMCVOR=1
      CASE(24)
        IVAL=4
      CASE(28)
        IVAL=4
        IMCVOR=1
      CASE(34)
        IVAL=4
      CASE(38)
        IVAL=8
      CASE(310)
        IVAL=4
        IMCVOR=1
      CASE(320)
        IVAL=8
        IMCVOR=1
      CASE DEFAULT
!         WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
      END SELECT
C       
 	DO KK=1,NDIMEL
	  NCVOR=NEL(KK,NBREL)
	  KK1=NRED(KK)
	  SELECT CASE(elemtip(NBREL))
          CASE(12:13)
            KK1=NRED1(KK)
          CASE(23,26)
            KK1=NRED26(KK)
          CASE(24,28)
            KK1=NRED28(KK)
          CASE(34,310)
            KK1=NREDT10(KK)
          CASE DEFAULT
!          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
          END SELECT
	  IF(KOTES.EQ.1) KK1=NREDK(KK)
	  IF(KK.LE.IVAL.OR.IMCVOR.EQ.0) THEN
! C BRZINE U CVOROVIMA
!           VECTJ(1,NCVOR)=VECTJ(1,NCVOR)+AJX(KK1)/IVECT(NCVOR)
!           VECTJ(2,NCVOR)=VECTJ(2,NCVOR)+AJY(KK1)/IVECT(NCVOR)
!           VECTJ(3,NCVOR)=VECTJ(3,NCVOR)+AJZ(KK1)/IVECT(NCVOR)
C GRADIJENT Temperature U CVOROVIMA
	  GRADJN(1,NCVOR)=GRADJN(1,NCVOR)+AJX1(KK1)/IVECT(NCVOR)
	  GRADJN(2,NCVOR)=GRADJN(2,NCVOR)+AJY1(KK1)/IVECT(NCVOR)
	  GRADJN(3,NCVOR)=GRADJN(3,NCVOR)+AJZ1(KK1)/IVECT(NCVOR)
          ELSE IF(KK.GT.IVAL.AND.IMCVOR.EQ.1) THEN
          KK2=KK1/10
          KK3=KK1-KK2*10
C BRZINE U CVOROVIMA
!           VECTJ(1,NCVOR)=VECTJ(1,NCVOR)+(AJX(KK2)/IVECT(NCVOR))/2
!      1                                 +(AJX(KK3)/IVECT(NCVOR))/2
!           VECTJ(2,NCVOR)=VECTJ(2,NCVOR)+(AJY(KK2)/IVECT(NCVOR))/2
!      1                                 +(AJY(KK3)/IVECT(NCVOR))/2
!           VECTJ(3,NCVOR)=VECTJ(3,NCVOR)+(AJZ(KK2)/IVECT(NCVOR))/2
!      1                                 +(AJZ(KK3)/IVECT(NCVOR))/2
C GRADIJENT Temperature U CVOROVIMA
          GRADJN(1,NCVOR)=GRADJN(1,NCVOR)+(AJX1(KK2)/IVECT(NCVOR))/2
     1                                   +(AJX1(KK3)/IVECT(NCVOR))/2
          GRADJN(2,NCVOR)=GRADJN(2,NCVOR)+(AJY1(KK2)/IVECT(NCVOR))/2
     1                                   +(AJY1(KK3)/IVECT(NCVOR))/2
          GRADJN(3,NCVOR)=GRADJN(3,NCVOR)+(AJZ1(KK2)/IVECT(NCVOR))/2
     1                                   +(AJZ1(KK3)/IVECT(NCVOR))/2
          
          ENDIF
	ENDDO
C======================================================================= 
 500  CONTINUE
C
C
      WRITE(IIZLAZ,*)'PERIOD NUMBER,KKORAK ',NNPER, KKORAK        
      IF (KKORAK.GT.0) THEN
C Sneza 24.06.2011 dodato da stampa rezultate samo za odredjene cvorove
C stampanje potencijala
      IF (INDSC.EQ.0.OR.INDSC.EQ.3.OR.INDSC.EQ.4) THEN
        CALL STAU09c(TT1,CORD,ID,NPT,47,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1,
     1               NEL)
C        CALL PRKONT1c(TT1,ID,UBRZ,CORD,NZAD,IOSA,IVECT,ZADVRE,KOJK)
C stampanje dubine vode
C       CALL STAU09c(TT1,CORD,ID,NPT,49,11,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
C stampa rezultata u neu za sve cvorove STAS09T
        CALL STAS09T(TT1,CORD,ID,NPT,49,10,1,KKORAK,NZAD,NUMZAD,KONT)
       CALL STAU09T(TT1,CORD,ID,NPT,49,9,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
       CALL STAU09T(TT1,CORD,ID,NPT,49,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
!  DUBINA VODE
!        CALL STAU09T(TT1,CORD,ID,NPT,49,11,1,KKORAK,NZAD,NUMZAD,KONT,TT1)
!  BRZINE
!       CALL STAU09T(VECTJ,CORD,ID,NPT,49,41,3,KKORAK,NZAD,NUMZAD,KONT,
!      1     TT1)
       CALL STAU09T(GRADJN,CORD,ID,NPT,49,51,3,KKORAK,NZAD,NUMZAD,KONT,
     1            TT1)
CE WRITING TEMPERATURES FOR TERMOMECHANIC
C      CALL RESTEL(VVREME,TT1,NPT,19)
      CALL RESTEL(VVREME,TT1,NPT,IDJSTAMP1,NP3D1)
C stampanje rezultata u svim cvorovima
      CALL IZLL3DT(ID,TT1,KKORAK,VECTJ,QUK,QUM,VVREME,VG,GG,
     1            KOTES)

      CALL STAG3D(TT1,ID,NASLOV,VVREME,KKORAK,1,NPT,18,0,NET,NEL,
     1            KORAK,VECTJ,CORD,GRADJN,NZAD,FZAPR)
      ENDIF
      IF (INDSC.EQ.1.OR.INDSC.EQ.3) THEN
        CALL STAU09c(TT1,CORD,ID,NPT,47,1,1,KKORAK,NZAD,NUMZAD,KONT,TT1,
     1               NEL)
      ENDIF
      IF(INDSC.EQ.2.OR.INDSC.EQ.4) THEN
        CALL STAU09CT(TT1,CORD,47,VVREME,KKORAK,NEL)
      ENDIF
      ENDIF
C
      IF(NXNASTRAN.EQ.1) RETURN
C
      KKORAK=KKORAK+1
c uslova da sledeci korak u tom periodu ne postoji i da ide na sledeci period
      IF (DABS(VREME(NNPER,KORAK+1)).LT.1.D-10) THEN
        GOTO 600
      ELSE
cz ide na sledeci korak      
        GOTO 35
      ENDIF
  600 CONTINUE
      IF(NSTAC.EQ.1.AND.KKORAK.EQ.2) 
     1  CALL IZBACIT(TT1,CORD,ID,NPT,NEL,NET,NDIM,IIZLAZ)
     
      RETURN      
      END
C=======================================================================
C=======================================================================
      SUBROUTINE IZLL3DT(ID,TT1,KORAK,VECTJ,QUK,QUM,VVREME,VG,GG,
     1                  KOTES)
      USE ELEMENTS
      USE KONTURE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /ULAZNI/ IULAZ,IIZLAZ,IPAKT
      COMMON /NDESUK/ NDES,IDPRIT,IFORM
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /PENALL/ PENALT,PRESS,IBRGT

      COMMON /SRPSKI/ ISRPS

      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /VOPTIM/ NKONT,MAXLIN
      COMMON /POCETN/ IPOCU,IPOCV,IPOCP,IPOCT,POCU,POCV,POCP,POCT,GAMA
      COMMON /DODAT/ NDIMM
      COMMON /ICITANJE/INPT
      COMMON /PRIKAZ/ INDSC,IZPOT

      DIMENSION D4(4),TT1(*),VECTJ(3,*),ID(1,*),N4(4)
      DIMENSION NRED(8),NREDK(8),NRED1(3)
      DIMENSION VG(3,NET,*),GG(3,NET,*),QUK(1000),QUM(1000),NREDT(4)
      DIMENSION NREDT10(10),VGT(3),NRED26(6),NRED28(8)
      DATA NRED/8,4,2,6,7,3,1,5/  
      DATA NREDK/27,9,3,21,25,7,1,19/
      DATA NRED1/2,1,3/
      DATA NRED26/1,2,3,12,23,31/
      DATA NRED28/1,2,3,4,12,23,34,41/
      DATA NREDT/1,2,3,4/
      DATA NREDT10/1,2,3,4,12,23,13,14,24,34/
      
C
      if (indsc.eq.0) then
C      
CS  TEMPERATURA U CVOROVIMA 
CE  NODE TEMPERATURE 
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2001)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6001)
      WRITE (IIZLAZ,*)'STEP NUMBER  ',KORAK

      KG = 0
   20 DO 50 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 100
      N4(I) = KG
      JJEDN=ID(1,KG)
      IF(JJEDN.EQ.0) THEN
        D4(I) = 0.
      ELSE
        D4(I) = TT1(JJEDN)
      ENDIF
   50 CONTINUE
C
  100 I = I - 1
      IF(IFORM.EQ.1) GO TO 88
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 77
C
   88 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
   77 IF(KG.LT.NPT) THEN
      GO TO 20
      ENDIF
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2028)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6028)
C
CS  GUSTINE STRUJE U PRAVCU X1     
CE  CURRENT DESNITIES IN DIRECTION X1
C

      KG = 0
  620 DO 650 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 1600
      N4(I) = KG
         D4(I) = VECTJ(1,KG)
  650 CONTINUE
C
 1600 I = I - 1
      IF(IFORM.EQ.1) GO TO 688
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 677
C
  688 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
  677 IF(KG.LT.NPT) THEN
      GO TO 620
      ENDIF

      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2039)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6039)
C
CS  GUSTINE STRUJE U PRAVCU X2     
CE  CURRENT DESNITIES IN DIRECTION X2
C

      KG = 0
  720 DO 750 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 1700
      N4(I) = KG
         D4(I) = VECTJ(2,KG)
  750 CONTINUE
C
 1700 I = I - 1
      IF(IFORM.EQ.1) GO TO 788
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 777
C
  788 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
  777 IF(KG.LT.NPT) THEN
      GO TO 720
      ENDIF
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2049)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6049)
C
CS  GUSTINE STRUJE U PRAVCU X3     
CE  CURRENT DESNITIES IN DIRECTION X3
C

      KG = 0
  820 DO 850 I=1,4
      KG = KG +1
      IF(KG.GT.NPT) GO TO 1800
      N4(I) = KG
         D4(I) = VECTJ(3,KG)
  850 CONTINUE
C
 1800 I = I - 1
      IF(IFORM.EQ.1) GO TO 888
      IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5012) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5002) (N4(J1),D4(J1),J1=1,I)
      ENDIF
      GO TO 877
C
  888 IF(INPT.EQ.1) THEN
       WRITE(IIZLAZ,5013) (N4(J1),D4(J1),J1=1,I)
      ELSE
       WRITE(IIZLAZ,5003) (N4(J1),D4(J1),J1=1,I)
      ENDIF
  877 IF(KG.LT.NPT) THEN
      GO TO 820
      ENDIF

C
CS  BRZINE
C
      NGAUS=IBRGT*IBRGT*IBRGT
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2059)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6059)
      IF (INDSC.EQ.0)THEN
       DO I=1,NET
         IF(elemtip(I).gt.100) THEN
          NTYPE=elemtip(I)/100
          NTIMES=100
        ELSE
          NTYPE=elemtip(I)/10
          NTIMES=10
        ENDIF
        NDIMEL=elemtip(I)-NTIMES*NTYPE
         WRITE(IIZLAZ,5000) I
       DO J=1,NDIMEL
         NC=NEL(J,I)
         NG=NRED(J)
         SELECT CASE(elemtip(I))
          CASE(12:13)
            NG=NRED1(J)
          CASE(23:26)
            NG=NRED26(J)
          CASE(34,310)
            NG=NREDT10(J)
          CASE DEFAULT
!          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
         END SELECT
!          IF(NDIMEL.eq.2) NG=NRED1(J)
!          IF(NDIMEL.eq.10) NG=NREDT10(J)
	 IF(KOTES.EQ.1) NG=NREDK(J)
	     IF(INPT.EQ.1) THEN
              IF(NG.GT.10) THEN
                NG1=NG/10
                NG2=NG-NG1*10
                DO K=1,3
                  VGT(K)=(VG(K,I,NG1)+(VG(K,I,NG2)))/2
                ENDDO
                WRITE(IIZLAZ,5017) NC,(VGT(K),K=1,3)
              ELSE
                WRITE(IIZLAZ,5017) NC,(VG(K,I,NG),K=1,3)
              ENDIF
	     ELSE
              IF(NG.GT.10) THEN
                NG1=NG/10
                NG2=NG-NG1*10
                DO K=1,3
                  VGT(K)=(VG(K,I,NG1)+(VG(K,I,NG2)))/2
                ENDDO
                WRITE(IIZLAZ,5007) NC,(VGT(K),K=1,3)
              ELSE
                WRITE(IIZLAZ,5007) NC,(VG(K,I,NG),K=1,3)
              ENDIF
	     ENDIF
       ENDDO
       ENDDO
      ENDIF
C
CS  GRADIJENTI
C
      IF(ISRPS.EQ.0)
     1WRITE(IIZLAZ,2069)
      IF(ISRPS.EQ.1)
     1WRITE(IIZLAZ,6069)
      IF (INDSC.EQ.0)THEN
      DO I=1,NET
         IF(elemtip(I).gt.100) THEN
          NTYPE=elemtip(I)/100
          NTIMES=100
        ELSE
          NTYPE=elemtip(I)/10
          NTIMES=10
        ENDIF
        NDIMEL=elemtip(I)-NTIMES*NTYPE
         WRITE(IIZLAZ,5000) I
        DO J=1,NDIMEL
         NC=NEL(J,I)
         NG=NRED(J)
         SELECT CASE(elemtip(I))
          CASE(12:13)
            NG=NRED1(J)
          CASE(23:26)
            NG=NRED26(J)
          CASE(34,310)
            NG=NREDT10(J)
          CASE DEFAULT
!          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NBREL)
         END SELECT
         IF(NDIMEL.eq.2) NG=NRED1(J)
         IF(NDIMEL.eq.10) NG=NREDT10(J)
	   IF(KOTES.EQ.1) NG=NREDK(J)
	   IF(INPT.EQ.1) THEN
                IF(NG.GT.10) THEN
                  NG1=NG/10
                  NG2=NG-NG1*10
                  DO K=1,3
                    VGT(K)=(GG(K,I,NG1)+(GG(K,I,NG2)))/2
                  ENDDO
                  WRITE(IIZLAZ,5017) NC,(VGT(K),K=1,3)
                 ELSE
                  WRITE(IIZLAZ,5017) NC,(GG(K,I,NG),K=1,3)
	         ENDIF
	   ELSE
	     IF(NG.GT.10) THEN
                NG1=NG/10
                NG2=NG-NG1*10
                DO K=1,3
                  VGT(K)=(GG(K,I,NG1)+(GG(K,I,NG2)))/2
                ENDDO
                WRITE(IIZLAZ,5007) NC,(VGT(K),K=1,3)
             ELSE
                WRITE(IIZLAZ,5007) NC,(GG(K,I,NG),K=1,3)
             ENDIF
	   ENDIF
        ENDDO
      ENDDO
      ENDIF
c
      endif      
C
      RETURN
 5000 FORMAT(/' ELEMENT',I10)     
 5002 FORMAT(4(I5,1PD13.5))
 5003 FORMAT(4(I5,3X,F10.3))
 5005 FORMAT(F10.3,10(1PE13.5))
 5007 FORMAT(I5,3(1PE13.5))
 5012 FORMAT(4(I10,1PD13.5))
 5013 FORMAT(4(I10,3X,F10.3))
 5017 FORMAT(I10,3(1PE13.5))
 2001 FORMAT(/'    T E M P E R A T U R E               '
     1,'             '//
     1' CVOR  POMERANJE   CVOR  POMERANJE   CVOR  POMERANJE   CVOR POMER 
     1NJE   '/' BROJ',3(14X,'BROJ'))
 2028 FORMAT(/'     C V O R N E   B R Z I N E   U   X 1  P R A V C U '//
     1' CVOR    BRZINA    CVOR    BRZINA    CVOR    BRZINA    CVOR   BRZ       
     1INA   '/' BROJ',3(14X,'BROJ'))
 2039 FORMAT(/'     C V O R N E   B R Z I N E   U   X 2  P R A V C U '//
     1' CVOR    BRZINA    CVOR    BRZINA    CVOR    BRZINA    CVOR   BRZ       
     1INA   '/' BROJ',3(14X,'BROJ'))
 2049 FORMAT(/'     C V O R N E   B R Z I N E   U   X 3  P R A V C U '//
     1' CVOR    BRZINA    CVOR    BRZINA    CVOR    BRZINA    CVOR   BRZ       
     1INA   '/' BROJ',3(14X,'BROJ'))
 2059 FORMAT(//'     C V O R N E   B R Z I N E'/
     1' CVOR    BRZINA VX    BRZINA VY    BRZINA VZ')
 2069 FORMAT(//'     C V O R N I   G R A D I J E N T I'/
     1' CVOR GRADIJENT GX GRADIJENT GY GRADIJENT GZ')
 6001 FORMAT(/'    N O D A L    T O T A L     P O T E N C I A L        '
     1,'                 '//
     1' NODE  DISPLAC.    NODE  DISPLAC.    NODE  DISPLAC.    NODE DISPL
     1AC.   '/'  No.',3(15X,'No.'))
 6028 FORMAT(/'    N O D A L    V E L O C I T I E S   I N   D I R E C T'
     1,' I O N   X 1 '//
     1' NODE  VELOCITY    NODE  VELOCITY    NODE  VELOCITY    NODE VELOC
     1ITY   '/'  No.',3(15X,'No.'))
 6039 FORMAT(/'    N O D A L    V E L O C I T I E S   I N   D I R E C T'
     1,' I O N   X 2 '//
     1' NODE  VELOCITY    NODE  VELOCITY    NODE  VELOCITY    NODE VELOC
     1ITY   '/'  No.',3(15X,'No.'))
 6049 FORMAT(/'    N O D A L    V E L O C I T I E S   I N   D I R E C T'
     1,' I O N   X 3 '//
     1' NODE  VELOCITY    NODE  VELOCITY    NODE  VELOCITY    NODE VELOC
     1ITY   '/'  No.',3(15X,'No.'))
 6059 FORMAT(//'    N O D A L    V E L O C I T I E S'/
     1' NODE  VELOCITY VX  VELOCITY VY  VELOCITY VZ')
 6069 FORMAT(//'    N O D A L    G R A D I E N T S  '/
     1' NODE  GRADIENT GX  GRADIENT GY  GRADIENT GZ')
 7023 FORMAT(//'    F L U X   W I T H I N  C O N T O U R            '/)
      END
C=======================================================================
      SUBROUTINE DJERDAPREAD(IDJSTAMP1,NP3D1)
       IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       COMMON /DJERDAP/ IDJERDAP
       COMMON /ICITANJE/ INPT
       COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
       DIMENSION NP3D1(5),NDJEL(21),IDJSTAMP1(5,*)
       CHARACTER*5 IME(5)
       
!        CALL ICLEAR(NP3D1,5)
!        KK=1
       IIFILE=90
       IME(1)='DJ3D1'
       IME(2)='DJ3D2'
       IME(3)='DJ3D3'
       IME(4)='DJ3D4'
       IME(5)='DJ3D5'
       DO KK=1,5
          IIFILE=IIFILE+1
c          OPEN (IIFILE,FILE='DJ3D1',STATUS='UNKNOWN',FORM='FORMATTED',
          OPEN (IIFILE,FILE=IME(KK),STATUS='UNKNOWN',FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL')
C
!            OPEN (92,FILE='DJ3D2',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!            OPEN (93,FILE='DJ3D3',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!            OPEN (94,FILE='DJ3D4',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!            OPEN (95,FILE='DJ3D5',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')

           READ(IIFILE,4111,err=9999) NDJ3D1,IT1,NCVE
!           IF(STATTT.EQ.'OLD') THEN
!             OPEN (91,FILE='DJ3D1',STATUS='OLD',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!             READ(91,4111) NDJ3D1,IT1,NCVE
!            write(*,*) 'NDJ3D1',NDJ3D1
           DO I=1,NDJ3D1
                READ(IIFILE,*)
                IF(INPT.EQ.1) THEN
                   READ(IIFILE,4111) (NDJEL(J),J=1,IT1)
                   READ(IIFILE,4111) (NDJEL(J),J=IT1+1,NCVE)
                ELSE
                   READ(IIFILE,1111) (NDJEL(J),J=1,IT1)
                   READ(IIFILE,1111) (NDJEL(J),J=IT1+1,NCVE)
                ENDIF
                DO K=1,NCVE
                   IDJSTAMP1(KK,NDJEL(K))=KK
                ENDDO
             ENDDO
             DO I=1,NPT
                IF(IDJSTAMP1(KK,I).EQ.KK) NP3D1(KK)=NP3D1(KK)+1
             ENDDO
             write(*,*) 'kraj citanja NP3D1',NP3D1(KK)
!           ELSE
!              STOP ' NEMA FAJLA DJ3D1'
!           ENDIF
        ENDDO
        go to 9998  
 9999     write(*,*) ' NE POSTOJI FAJL DJ3D '         
          write(3,*) ' NE POSTOJI FAJL DJ3D '
 1111 FORMAT(14I5)
 4111 FORMAT(13I10)
 9998 RETURN
      END
C=======================================================================
      SUBROUTINE DJERDAPREADT(IDJSTAMP1,NP3D1)
       IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       COMMON /DJERDAP/ IDJERDAP
       COMMON /ICITANJE/ INPT
       COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
       DIMENSION NP3D1(5),NDJEL(21),IDJSTAMP1(5,*)
       CHARACTER*6 IME(5)
       
!        KK=1
       IIFILE=90
       IME(1)='DJ3DT1'
       IME(2)='DJ3DT2'
       IME(3)='DJ3DT3'
       IME(4)='DJ3DT4'
       IME(5)='DJ3DT5'
       DO KK=1,5
          IIFILE=IIFILE+1
c          OPEN (IIFILE,FILE='DJ3D1',STATUS='UNKNOWN',FORM='FORMATTED',
          OPEN (IIFILE,FILE=IME(KK),STATUS='UNKNOWN',FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL')
C
!            OPEN (92,FILE='DJ3D2',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!            OPEN (93,FILE='DJ3D3',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!            OPEN (94,FILE='DJ3D4',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!            OPEN (95,FILE='DJ3D5',STATUS='UNKNOWN',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')

           READ(IIFILE,4111,err=9999) NDJ3D1,IT1,NCVE
!           IF(STATTT.EQ.'OLD') THEN
!             OPEN (91,FILE='DJ3D1',STATUS='OLD',FORM='FORMATTED',
!      1      ACCESS='SEQUENTIAL')
!             READ(91,4111) NDJ3D1,IT1,NCVE
!            write(*,*) 'NDJ3D1',NDJ3D1
           DO I=1,NDJ3D1
                READ(IIFILE,*)
                IF(INPT.EQ.1) THEN
                   READ(IIFILE,4111) (NDJEL(J),J=1,IT1)
                   READ(IIFILE,4111) (NDJEL(J),J=IT1+1,NCVE)
                ELSE
                   READ(IIFILE,1111) (NDJEL(J),J=1,IT1)
                   READ(IIFILE,1111) (NDJEL(J),J=IT1+1,NCVE)
                ENDIF
                DO K=1,NCVE
                   IDJSTAMP1(KK,NDJEL(K))=KK
                ENDDO
             ENDDO
             DO I=1,NPT
                IF(IDJSTAMP1(KK,I).EQ.KK) NP3D1(KK)=NP3D1(KK)+1
             ENDDO
             write(*,*) 'kraj citanja NP3D1',NP3D1(KK)
!           ELSE
!              STOP ' NEMA FAJLA DJ3D1'
!           ENDIF
        ENDDO
        go to 9998  
 9999     write(*,*) ' NE POSTOJI FAJL DJ3DT '         
          write(3,*) ' NE POSTOJI FAJL DJ3D '
 1111 FORMAT(14I5)
 4111 FORMAT(13I10)
 9998 RETURN
      END
