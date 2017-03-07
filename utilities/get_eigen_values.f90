!###############################################################################
!> Module to find the eigenvalues and eigenvectors (if desired)
!> of a real general matrix.
!>
!> @author Yang Liu
!>   @date 01/11/2017
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
  module get_eigen_values  

  private

# include <kind_parameter.h>

  public :: rg

!===============================================================================
  
  contains

!-------------------------------------------------------------------------------
!> To find the eigenvalues and eigenvectors (if desired) 
!> of a real general matrix.
!-------------------------------------------------------------------------------
    
  subroutine rg(n,a,wr,wi,matz,z,iv1,fv1,ierr)
  
    implicit none

    intknd,intent(in) :: n
    realkd,intent(in) :: a(n,n)
    realkd,intent(inout)::wr(n),wi(n)
    
    intknd :: is1,is2,ierr,matz
    realkd :: z(n,n),fv1(n)
    intknd :: iv1(n)
    logknd :: bool_1
    
    call  balanc(n,a,is1,is2,fv1)
    call  elmhes(n,is1,is2,a,iv1)
    call  hqr(n,is1,is2,a,wr,wi,ierr)
  
  end subroutine rg

!-------------------------------------------------------------------------------
!> balances a real matrix and isolates eigenvalues whenever possible.
!-------------------------------------------------------------------------------
  
  subroutine balanc(n,a,low,igh,scale)

    implicit none

    intknd :: i,j,k,l,m,n,jj,igh,low,iexc
    realkd :: a(n,n),scale(n)
    realkd ::c,f,g,r,s,b2,radix
    logknd :: noconv
    logknd :: bool_1
    
    radix = 16.0d0
    b2 = radix * radix
    k = 1
    l = n
    
    do jj = 1, l
      j = l + 1 - jj
      do i = 1, l
        if (i /= j) then
          if (a(j,i) /= 0.0d0) then 
            bool_1 = .true.
            exit
          endif
        endif
      enddo
      if ( .not. bool_1 ) then
        m = l
        iexc = 1
        !> In-line procedure for row and column exchange
        scale(m) = j
        if (j /= m) then
          do i = 1, l
             f = a(i,j)
             a(i,j) = a(i,m)
             a(i,m) = f
          enddo
          do i = k, n
             f = a(j,i)
             a(j,i) = a(m,i)
             a(m,i) = f
          enddo
        endif
        if(iexc==1) then
            if (l /= 1) then
                l = l - 1
            else
              low = k
              igh = l
              exit
            endif
        elseif(iexc==2) then
          k = k + 1
          exit
        endif
      end if
    enddo
    
    do j = k, l
      do i = k, l
        if (i /= j) then
            bool_1 = .false.
            if (a(i,j) /= 0.0d0) then
                bool_1 = .true.
                exit
            endif
        endif
      enddo
      if(.not. bool_1) then
        m = k
        iexc = 2
        scale(m) = j
        if (j /= m) then
          do i = 1, l
             f = a(i,j)
             a(i,j) = a(i,m)
             a(i,m) = f
          enddo
          do i = k, n
             f = a(j,i)
             a(j,i) = a(m,i)
             a(m,i) = f
          enddo
        endif
        if(iexc==1) then
          if (l /= 1) then
              l = l - 1
          else
            low = k
            igh = l
            exit
          endif
        elseif(iexc==2) then
          k = k + 1
          exit
        endif
      end if
    enddo

    !> Balance the submatrix in rows k to l
    do i = k, l
      scale(i) = 1.0d0
    enddo
    noconv = .true.
    !> Iterative loop for norm reduction
    do while(noconv)
      noconv = .false.
      do i = k, l
        c = 0.0d0
        r = 0.0d0
        do j = k, l
          if (j /= i) then
            c = c + abs(a(j,i))
            r = r + abs(a(i,j))
          endif
        enddo
        !> Guard against zero c or r due to underflow
        if (c /= 0.0d0 .and. r /= 0.0d0) then
          g = r / radix
          f = 1.0d0
          s = c + r
          do while(c < g)
            f = f * radix
            c = c * b2
          enddo
          g = r * radix
          do while(c >= g)
            f = f / radix
            c = c / b2
          enddo
          if ((c + r) / f < 0.95d0 * s) then
            g = 1.0d0 / f
            scale(i) = scale(i) * f
            noconv = .true.
            do j = k, n
              a(i,j) = a(i,j) * g
            enddo
            do j = 1, l
              a(j,i) = a(j,i) * f
            enddo
          endif
        endif
      enddo
    enddo
    low = k
    igh = l

  end subroutine balanc
  
!-------------------------------------------------------------------------------
!> reduces a submatrix situated in rows and columns
!> low through igh to upper hessenberg form by
!> stabilized elementary similarity transformations.
!-------------------------------------------------------------------------------
  
  subroutine elmhes(n,low,igh,a,int)

    implicit none
    
      intknd :: i,j,m,n,la,igh,kp1,low,mm1,mp1
      realkd :: a(n,n)
      realkd :: x,y
      intknd ::int(igh)
      
      la = igh - 1
      kp1 = low + 1
  
      if (la >= kp1) then
        do m = kp1, la
          mm1 = m - 1
          x = 0.0d0
          i = m
          do j = m, igh
            if (abs(a(j,mm1)) > abs(x)) then
              x = a(j,mm1)
              i = j
            endif
          enddo
          int(m) = i
          if (i /= m) then
            !> Interchange rows and columns of a
            do j = mm1, n
              y = a(i,j)
              a(i,j) = a(m,j)
              a(m,j) = y
            enddo
            do j = 1, igh
              y = a(j,i)
              a(j,i) = a(j,m)
              a(j,m) = y
            enddo
            !> End interchange
          endif
          if (x /= 0.0d0) then
            mp1 = m + 1
            do i = mp1, igh
              y = a(i,mm1) 
              if (y /= 0.0d0) then
                y = y / x
                a(i,mm1) = y
                do j = m, n
                  a(i,j) = a(i,j) - y * a(m,j)
                enddo
                do j = 1, igh
                  a(j,m) = a(j,m) + y * a(j,i)
                enddo
              endif
            enddo
          endif
        enddo
      endif

  end subroutine elmhes

!-------------------------------------------------------------------------------
!> find the eigenvalues of a real upper hessenberg matrix by the qr method.
!-------------------------------------------------------------------------------
  
  subroutine hqr(n,low,igh,h,wr,wi,ierr)  

     implicit none

     intknd :: i,j,k,l,m,n,en,ll,mm,na,igh,itn,its,low,mp2,enm2,ierr
     realkd :: h(n,n),wr(n),wi(n)
     realkd :: p,q,r,s,t,w,x,y,zz,norm,tst1,tst2
     logknd :: notlas,bool_1,bool_2,bool_3,bool_4
    
     ierr = 0
     norm = 0.0d0
     k = 1
     l = 0

     !> Store roots isolated by balanc and compute matrix norm
     do i = 1, n
       do j = k, n 
         norm = norm + abs(h(i,j))
       enddo
       k = i
       if (i < low .or. i > igh) then
         wr(i) = h(i,i)
         wi(i) = 0.0d0
       endif
     enddo 
     en = igh
     t = 0.0d0
     itn = 30*n
     bool_1 = .true.
     bool_2 = .true.
     do while(bool_1 .or. bool_2 )
       bool_1 = .false.
       bool_2 = .false.
       bool_3 = .false.
       !> Search for next eigenvalues
       if (en >= low) then 
         its = 0
         na = en - 1
         enm2 = na - 1
         !> Look for single small sub-diagonal element 
         !> for l=en step -1 until low do
         do while(l /= en .and. l /= na .and. itn /= 0)
           do ll = low, en
             l = en + low - ll
             if (l == low) exit
             s = abs(h(l-1,l-1)) + abs(h(l,l))
             if (s == 0.0d0) s = norm
             tst1 = s
             tst2 = tst1 + abs(h(l,l-1))
             if (tst2 == tst1) exit
           enddo
           !> Form shift
           x = h(en,en)
           if (l == en) then 
             bool_1 = .true.
             exit
           endif
           y = h(na,na)
           w = h(en,na) * h(na,en)
           if (l == na) then
             bool_2 = .true.
             exit
           endif
           if (itn == 0) then
             bool_3 = .true.
             exit
           endif
           if (its == 10 .or. its == 20) then
             !> Form exceptional shift
             t = t + x
             do i = low, en
               h(i,i) = h(i,i) - x
             enddo
             s = abs(h(en,na)) + abs(h(na,enm2))
             x = 0.75d0 * s
             y = x
             w = -0.4375d0 * s * s
           endif
           its = its + 1
           itn = itn - 1
           !> Look for two consecutive small sub-diagonal elements
           do mm = l, enm2
             m = enm2 + l - mm
             zz = h(m,m)
             r = x - zz
             s = y - zz
             p = (r * s - w) / h(m+1,m) + h(m,m+1)
             q = h(m+1,m+1) - zz - r - s
             r = h(m+2,m+1)
             s = abs(p) + abs(q) + abs(r)
             p = p / s
             q = q / s
             r = r / s
             if (m == l) exit
             tst1 = abs(p)*(abs(h(m-1,m-1)) + abs(zz) + abs(h(m+1,m+1)))
             tst2 = tst1 + abs(h(m,m-1))*(abs(q) + abs(r))
             if (tst2 == tst1) exit
           enddo
           mp2 = m + 2
           do i = mp2, en
             h(i,i-2) = 0.0d0
             if (i /= mp2) then
                 h(i,i-3) = 0.0d0
             endif
           enddo
           !> Double qr step involving rows l to en and columns m to en
           do k = m, na
             notlas = k .ne. na
             bool_4 = .false.
             if (k /= m) then
               p = h(k,k-1)
               q = h(k+1,k-1)
               r = 0.0d0
               if (notlas) r = h(k+2,k-1)
               x = abs(p) + abs(q) + abs(r)
               if (x /= 0.0d0) then
                 p = p / x
                 q = q / x
                 r = r / x
               else
                 bool_4 = .true.
               endif
             endif
             if(.not. bool_4) then
               s = sign(sqrt(p*p+q*q+r*r),p)
               if (k == m) then
                 if (l .ne. m) h(k,k-1) = -h(k,k-1)
               else
                 h(k,k-1) = -s * x
               endif
               p = p + s
               x = p / s
               y = q / s
               zz = r / s
               q = q / p
               r = r / p
               if (notlas) then
                 !> Row modification
                 do j = k, EN
                   p = h(k,j) + q * h(k+1,j) + r * h(k+2,j)
                   h(k,j) = h(k,j) - p * x
                   h(k+1,j) = h(k+1,j) - p * y
                   h(k+2,j) = h(k+2,j) - p * zz
                 enddo
                 j = min0(en,k+3)
                 !> Column modification
                 do i = L, j
                   p = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2)
                   h(i,k) = h(i,k) - p
                   h(i,k+1) = h(i,k+1) - p * q
                   h(i,k+2) = h(i,k+2) - p * r
                 enddo
               else
                 !> Row modification
                 do j = k, EN
                   p = h(k,j) + q * h(k+1,j)
                   h(k,j) = h(k,j) - p * x
                   h(k+1,j) = h(k+1,j) - p * y
                 enddo
                 j = min0(en,k+3)
                 !> Column modification
                 do i = L, j
                   p = x * h(i,k) + y * h(i,k+1)
                   h(i,k) = h(i,k) - p
                   h(i,k+1) = h(i,k+1) - p * q
                 enddo
               endif
             endif
           enddo 
         enddo
         if(bool_1) then 
           !> One root found
           wr(en) = x + t
           wi(en) = 0.0d0
           en = na
         elseif(bool_2) then
           !> Two roots found
           p = (y - x) / 2.0d0
           q = p * p + w
           zz = sqrt(abs(q))
           x = x + t
           if (q >= 0.0d0) then
             !> Real pair
             zz = p + sign(zz,p)
             wr(na) = x + zz
             wr(en) = wr(na)
             if (zz /= 0.0d0) wr(en) = x - w / zz
             wi(na) = 0.0d0
             wi(en) = 0.0d0
           else
             !> Complex pair
             wr(na) = x + p
             wr(en) = x + p
             wi(na) = zz
             wi(en) = -zz
           endif
           en = enm2
         elseif(bool_3) then
           !> Set error -- all eigenvalues have not converged
           !> after 30*n iterations
           ierr = en
         endif
       else
         exit
       endif 
     enddo
  end subroutine hqr
  
end module get_eigen_values