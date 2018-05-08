! program emulates Japanese PURPLE machine
! Copyright (c) 2018 Joshua E. Roybal
program purple
   implicit none
   ! interface declaration
   interface
      ! level incrementing subroutine
      subroutine step(levcnt)
         integer, intent(inout), dimension(0:3) :: levcnt
      end subroutine step
      ! function emulates pegboard mapping of sixes and twenties
      function pegboard(ptch, sixes, twenties) result(ctch)
         character, intent(in) :: ptch
         character (len = *), intent(in) :: sixes, twenties
         character :: ctch
      end function pegboard
      ! function emulates sixes relay
      function relay_six(NS, NL, NR, slevs, levcnt, vowels, inchar, flag) & 
            result(outchar)
         integer, intent(in) :: NS, NL, NR
         character, intent(in), dimension(NS,NL) :: slevs
         integer, intent(inout), dimension(0:3) :: levcnt
         character, intent(in) :: inchar, flag
         character (len = *), intent(in) :: vowels
         character :: outchar
      end function relay_six
      ! function simulates twenties relay
      function relay_twenty(NT, NL, NR, tlevs, levcnt, consonants, inchar, &
            flag) result(outchar)
         integer, intent(in) :: NT, NL, NR
         character, intent(in), dimension(NT,NL,NR) :: tlevs
         integer, intent(inout), dimension(0:3) :: levcnt
         character (len = *), intent(in) :: consonants
         character, intent(in) :: inchar, flag
         character :: outchar
      end function relay_twenty      
   end interface
   ! variable and array declarations
   character (len = 6), parameter :: vowels = 'AEIOUY', sixes = 'NOKTYU'
   character (len = 20), parameter :: consonants = 'BCDFGHJKLMNPQRSTVWXZ', &
   twenties = 'XEQLHBRMPDICJASVWGZF'
   integer, parameter :: NR = 3, NL = 25, NS = 6, NT = 20
   integer :: i, j, k, idx, eof
   character :: ch, flag
   character, dimension(25) :: buffer
   character (len = 80) :: record
   character, dimension(NS,NL) :: slevs
   character, dimension(NT,NL,NR) :: tlevs
   integer, dimension(NR) :: levels
   integer, dimension(0:3) :: levcnt
   ! data initializations
   data levels/NR*1/, levcnt/4*1/
   ! processing
   call getarg(1, flag)
   if (flag /= 'e' .and. flag /= 'd') then
      write (*,*) 'Usage: purple e/d'
      stop 'processing terminated'
   end if
   open (7,file='levels.dat',status='old',action='read')
   ! get enciphering relays
   ! get sixes
   do j = 1, NL
      read (7,*) record
      read (record,1000) (slevs(i,j),i=1,NS)
   end do
   ! get twenties
   do k = 1, NR
      do j = 1, NL
         read (7,*) record
         read (record,2000) (tlevs(i,j,k),i=1,NT)
      end do
   end do
   close (7)
   ! encipher input (piped or otherwise)
   read (*,4000,iostat=eof) record
   j = 0
   do while (eof == 0)
      k = len(trim(record))
      do i = 1, k
         ch = record(i:i)
         ! to uppercase
         if (ch >= 'a' .and. ch <= 'z') ch = char(ichar(ch) - 32)
         if (ch < 'A' .or. ch > 'Z') cycle ! not alphabetic - do nothing
         ch = pegboard(ch, sixes, twenties)
         idx = index(vowels, ch)
         if (idx /= 0) then
            j = j + 1
            ch = relay_six(NS, NL, NR, slevs, levcnt, vowels, ch, flag)
            idx = index(vowels, ch)
            buffer(j) = sixes(idx:idx)
            if (j == 25) then
               write (*,5000) (buffer(idx), idx = 1, j)
               j = 0
            end if
            cycle ! we are done here
         end if
         idx = index(consonants, ch)
         if (idx /= 0) then
            j = j + 1
            ch = relay_twenty(NT, NL, NR, tlevs, levcnt, consonants, ch, flag)
            idx = index(consonants, ch)
            buffer(j) = twenties(idx:idx)
            if (j == 25) then
               write (*,5000) (buffer(idx), idx = 1, j)
               j = 0
            end if
         end if
      end do
      read (*,4000,iostat=eof) record
   end do
   ! pad out final block of 5 if necessary
   do while (mod(j, 5) /= 0) 
      j = j + 1
      buffer(j) = char(int(26 * rand()) + 65)
   end do
   ! write final line if there is one
   if (j > 0) write (*,5000) (buffer(idx), idx = 1, j)
   1000 format (6A1)
   2000 format (20A1)
   3000 format (X,A,I1,A)
   4000 format (A)
   5000 format (4(5A1,X),5A1)
end program purple

! function emulates pegboard mapping of sixes and twenties
function pegboard(ptch, sixes, twenties) result(ctch)
   implicit none
   ! dummy arguments
   character, intent(in) :: ptch
   character (len = *), intent(in) :: sixes, twenties
   ! local variables
   character :: ctch
   character (len = 6), parameter :: vowels = 'AEIOUY'
   character (len = 20), parameter :: consonants = 'BCDFGHJKLMNPQRSTVWXZ'
   integer :: idx
   ! processing
   idx = index(sixes, ptch)
   if (idx /= 0) then
      ctch = vowels(idx:idx)
      return
   end if
   idx = index(twenties, ptch)
   if (idx /= 0) then
      ctch = consonants(idx:idx)
      return
   end if
   ctch = ptch ! fail safe
end function pegboard

! subroutine increments levels
subroutine step(levcnt)
   implicit none
   ! dummy arguments
   integer, intent(inout), dimension(0:3) :: levcnt
   ! local variables
   integer i
   ! processing
   
   ! check which of the others advances
   if (levcnt(0) == 24 .and. levcnt(2) == 25) then 
      levcnt(3) = levcnt(3) + 1
   else if (levcnt(0) == 25) then
      levcnt(2) = levcnt(2) + 1
   else
      levcnt(1) = levcnt(1) + 1
   end if
      
   levcnt(0) = levcnt(0) + 1  ! always advances
   
   do i = 0, 3
      if (levcnt(i) > 25) levcnt(i) = 1
   end do 
end subroutine step

! function simualtes the sixes relay
function relay_six(NS, NL, NR, slevs, levcnt, vowels, inchar, flag) & 
      result(outchar)
   ! dummy arguments
   integer, intent(in) :: NS, NL, NR
   character, intent(in), dimension(NS,NL) :: slevs
   integer, intent(inout), dimension(0:3) :: levcnt
   character (len = *), intent(in) :: vowels
   character, intent(in) :: inchar, flag
   ! local variables
   character :: outchar
   integer :: stpidx, levidx
   ! processing
   levidx = levcnt(0)
   if (flag == 'e') then
      stpidx = index(vowels, inchar)
      outchar = slevs(stpidx,levidx)
   else if (flag == 'd') then
      stpidx = 1
      do while (inchar /= slevs(stpidx,levidx))
         stpidx = stpidx + 1
      end do
      outchar = vowels(stpidx:stpidx)
   end if
   call step(levcnt)
end function relay_six

! function simualates twenties relay
function relay_twenty(NT, NL, NR, tlevs, levcnt, consonants, inchar, flag) &
result(outchar)
   ! dummy arguments
   integer, intent(in) :: NT, NL, NR
   character, intent(in), dimension(NT,NL,NR) :: tlevs
   integer, intent(inout), dimension(0:3) :: levcnt
   character (len = *), intent(in) :: consonants
   character, intent(in) :: inchar, flag
   ! local variables
   character :: outchar
   integer :: i, relidx, levidx, stpidx
   ! processing
   outchar = inchar
   do i = 1, NR
      if (flag == 'e') then
         stpidx = index(consonants, outchar)
         outchar = tlevs(stpidx,levcnt(i),i)
      else if (flag == 'd') then
         relidx = NR - i + 1
         levidx = levcnt(relidx)
         stpidx = 1
         do while (outchar /= tlevs(stpidx,levidx,relidx))
            stpidx = stpidx + 1
         end do
         outchar = consonants(stpidx:stpidx)
      end if
   end do
   call step(levcnt)
end function relay_twenty
