! program emulates Japanese PURPLE machine
program purple
   implicit none
   ! interface declaration
   interface
      ! level incrementing subroutine
      subroutine step(levcnt, fst, mdm, slw)
         integer, intent(inout), dimension(0:3) :: levcnt
         integer, intent(in) :: fst, mdm, slw
      end subroutine step
      ! function emulates pegboard mapping of sixes and twenties
      function pegboard(ptch, sixes, twenties, VOWELS, CONSONANTS) result(ctch)
         character, intent(in) :: ptch
         character (len = *), intent(in) :: sixes, twenties, VOWELS, CONSONANTS
         character :: ctch
      end function pegboard
      ! function simulates sixes relay
      function relay_six(NS, NL, NR, relay, lev, stp)
         integer :: relay_six
         integer, intent(in) :: NS, NL, NR, lev, stp
         integer, intent(in), dimension(NS,NL) :: relay
      end function relay_six
      ! function simulate twenties relay
      function relay_twenty(NT, NL, NR, relay, lev, stp)
         integer :: relay_twenty
         integer, intent(in) :: NT, NL, NR
         integer, intent(in), dimension(NT,NL,NR) :: relay
         integer, intent(in), dimension(0:3) :: lev
         integer, intent(inout) :: stp
      end function relay_twenty
   end interface
   ! variable and array declarations
   character (len = 6), parameter :: VOWELS = 'AEIOUY'
   character (len = 20), parameter :: CONSONANTS = 'BCDFGHJKLMNPQRSTVWXZ'
   integer, parameter :: NR = 3, NL = 25, NS = 6, NT = 20
   integer :: i, j, k, fst, mdm, slw, idx, recno, eof
   character :: ch, flag
   character, dimension(25) :: buffer
   character (len = 6) :: sixes = 'NOKTYU'
   character (len = 20) :: twenties = 'XEQLHBRMPDICJASVWGZF'
   character (len = 80) :: line
   integer, dimension(NS,NL) :: r6
   integer, dimension(NT,NL,NR) :: r20
   integer, dimension(0:3) :: levcnt
   ! data initializations
   data levcnt/4*1/
   ! processing
   call getarg(1, flag)
   if (flag /= 'e' .and. flag /= 'd') then
      write (*,*) 'Usage: purple e/d'
      stop 'processing terminated'
   end if
   if (flag == 'e') then
      recno = 1
      fst = 1
      mdm = 2
      slw = 3
   else
      recno = 2
      fst = 3
      mdm = 2
      slw = 1
   end if
   open (7,file='relays.dat',access='direct',form='formatted',recl=3300)
   read (7,1000,rec=recno) r6, r20
   close (7)
   read (*,2000,iostat=eof) line
   j = 0
   do while (eof == 0)
      k = len(trim(line))
      do i = 1, k
         ch = line(i:i)
         ! to uppercase
         if (ch >= 'a' .and. ch <= 'z') ch = char(ichar(ch) - 32)
         if (ch < 'A' .or. ch > 'Z') cycle ! not alphabetic
         ch = pegboard(ch, sixes, twenties, VOWELS, CONSONANTS)
         idx = index(VOWELS, ch)
         if (idx /= 0) then
            j = j + 1
            idx = relay_six(NS, NL, NR, r6, levcnt(0), idx)
            buffer(j) = sixes(idx:idx)
            call step(levcnt, fst, mdm, slw)
            if (j == 25) then
               write (*,3000) (buffer(idx), idx = 1, j)
               j = 0
            end if
            cycle ! we are done here
         end if
         idx = index(CONSONANTS, ch)
         if (idx /= 0) then
            j = j + 1
            idx = relay_twenty(NT, NL, NR, r20, levcnt, idx)
            buffer(j) = twenties(idx:idx)
            call step(levcnt, fst, mdm, slw)
            if (j == 25) then
               write (*,3000) (buffer(idx), idx = 1, j)
               j = 0
            end if
         end if
      end do
      read (*,2000,iostat=eof) line
   end do
   ! pad out final block of 5 if necessary
   do while (mod(j, 5) /= 0) 
      j = j + 1
      buffer(j) = char(int(26 * rand()) + 65)
   end do
   ! write final line if there is one
   if (j > 0) write (*,3000) (buffer(idx), idx = 1, j)
   1000 format (1650i2)
   2000 format (A)
   3000 format (4(5A1,X),5A1)
end program purple

! function emulates pegboard mapping of sixes and twenties
function pegboard(ptch, sixes, twenties, VOWELS, CONSONANTS) result(ctch)
   implicit none
   ! dummy arguments
   character, intent(in) :: ptch
   character (len = *), intent(in) :: sixes, twenties, VOWELS, CONSONANTS
   ! local variables
   character :: ctch
   integer :: idx
   ! processing
   idx = index(sixes, ptch)
   if (idx /= 0) then
      ctch = VOWELS(idx:idx)
      return
   end if
   idx = index(twenties, ptch)
   if (idx /= 0) then
      ctch = CONSONANTS(idx:idx)
      return
   end if
   ctch = ptch ! fail safe
end function pegboard

! subroutine increments levels
subroutine step(levcnt, fst, mdm, slw)
   implicit none
   ! dummy arguments
   integer, intent(inout), dimension(0:3) :: levcnt
   integer, intent(in) :: fst, mdm, slw
   ! local variables
   integer i
   ! processing
   ! check which twenties relay steps
   if (levcnt(0) == 24 .and. levcnt(mdm) == 25) then 
      levcnt(slw) = levcnt(slw) + 1
   else if (levcnt(0) == 25) then
      levcnt(mdm) = levcnt(mdm) + 1
   else
      levcnt(fst) = levcnt(fst) + 1
   end if
      
   levcnt(0) = levcnt(0) + 1  ! always advances

   ! reset when on advance past 25th level
   do i = 0, 3
      if (levcnt(i) > 25) levcnt(i) = 1
   end do 
end subroutine step

! function simulates sixes relay
function relay_six(NS, NL, NR, relay, lev, stp)
   implicit none
   ! dummy arguments
   integer :: relay_six
   integer, intent(in) :: NS, NL, NR, lev, stp
   integer, intent(in), dimension(NS,NL) :: relay
   ! processing
   relay_six = relay(stp,lev)
end function relay_six

! function simulates twenties relay
function relay_twenty(NT, NL, NR, relay, lev, stp)
   implicit none
   ! dummy arguments
   integer :: relay_twenty
   integer, intent(in) :: NT, NL, NR
   integer, intent(in), dimension(NT,NL,NR) :: relay
   integer, intent(in), dimension(0:3) :: lev
   integer, intent(inout) :: stp
   ! local variables
   integer :: i
   ! processing
   do i = 1, NR
      stp = relay(stp,lev(i),i)
   end do
   relay_twenty = stp
end function relay_twenty
