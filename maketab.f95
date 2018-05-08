program maketables
   implicit none
   ! variable declarations
   character ch
   integer, parameter :: RELAYS=3,SROWS=7,TROWS=21,COLS=25
   integer :: eof, i, j, n, idx
   character (len=6) :: vowels, slev
   character (len=20) :: consonants, tlev
   character (len=2048) :: line
   integer, dimension(SROWS,COLS) :: sixes
   integer, dimension(TROWS,COLS) :: twenties
   character (len=6), dimension(COLS) :: srel
   character (len=20), dimension(COLS,RELAYS) :: trel
   ! processing
   vowels='AEIOUY'
   consonants='BCDFGHJKLMNPQRSTVWXZ'
   open (7,file='purple.txt',status='old',action='read')
   ! read and dump the decipher levels
   read (7,1000) line
   read (line,*) sixes
   do j=1,COLS
      write (slev,4000) (vowels(sixes(i,j):sixes(i,j)),i=2,SROWS)
      srel(j)=slev
   end do

   read (7,1000) line
   read (line,*) twenties
   do j=1,COLS
      write (tlev,5000) (consonants(twenties(i,j):twenties(i,j)),i=2,TROWS)
      trel(j,1)=tlev
   end do

   read (7,1000) line
   read (line,*) twenties
   do j=1,COLS
      write (tlev,5000) (consonants(twenties(i,j):twenties(i,j)),i=2,TROWS)
      trel(j,2)=tlev
   end do

   read (7,1000) line
   read (line,*) twenties
   do j=1,COLS
      write (tlev,5000) (consonants(twenties(i,j):twenties(i,j)),i=2,TROWS)
      trel(j,3)=tlev
   end do
   close (7)

   ! dump the levels to data file
   open (8,file='levels.dat',status='unknown',action='write')
   ! find and dump the encipher levels
   do j = 1,COLS
      do i = 1, SROWS - 1
         ch = vowels(i:i)
         idx = index(srel(j), ch)
         slev(i:i) = vowels(idx:idx)
      end do
      write (8,1000) slev
   end do

   do j = 1,COLS
      do i = 1, TROWS - 1
         ch = consonants(i:i)
         idx = index(trel(j,1), ch)
         tlev(i:i) = consonants(idx:idx)
      end do
      write (8,1000) tlev
   end do

   do j = 1,COLS
      do i = 1, TROWS - 1
         ch = consonants(i:i)
         idx = index(trel(j,2), ch)
         tlev(i:i) = consonants(idx:idx)
      end do
      write (8,1000) tlev
   end do

   do j = 1,COLS
      do i = 1, TROWS - 1
         ch = consonants(i:i)
         idx = index(trel(j,3), ch)
         tlev(i:i) = consonants(idx:idx)
      end do
      write (8,1000) tlev
   end do
   
   ! write the deciphering levels
   ! write (8,6000) (srel(j),j=1,COLS)
   ! write (8,7000) (trel(j,1),j=1,COLS)
   ! write (8,7000) (trel(j,2),j=1,COLS)
   ! write (8,7000) (trel(j,3),j=1,COLS)
   close (8)
   write (*,*) 'character maps written to data file levels.dat'
   1000 format (a)
   2000 format (7i3)
   3000 format (21i3)
   4000 format (6a1)
   5000 format (20a1)
   6000 format (a6)
   7000 format (a20)
end program maketables
