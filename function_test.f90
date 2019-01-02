function foo(i) result(res)
	real, intent(in) :: i
	res=i**2+i**3
end function foo

program function_test
	implicit none
	real :: foo
	real :: res
	res=foo(1.0)
	print *,"The result is",res
end program function_test
