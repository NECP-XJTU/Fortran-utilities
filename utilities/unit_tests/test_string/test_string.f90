program test_string
  use utilities
  implicit none

# include <kind_parameter.h>

  logknd :: bool
  character(20),allocatable :: words(:)
  character :: str
  integer :: m
  assert_start()
  call set_stop_on_error(.false.)

!Test str_to_cap(...)
  assert_component_begin('str_cap(...)')
  assert(str_to_cap('') == '', "Failed to test ''")
  assert(str_to_cap('aaa') == 'AAA', "Failed to test ''")
  bool = (str_to_cap(' .a A1 ') == ' .A A1 ')
  assert(bool, 'Failed to test .aA1 ')
  assert_component_summary('str_to_cap(...)')

!Test str_to_low(...)
  assert_component_begin('str_to_low(...)')
  assert(str_to_low('') == '',"Failed to test''")
  assert(str_to_low('AAA!A') == 'aaa!a',"Failed to test''")
  bool = (str_to_low(' .aA1') == ' .aa1')
  assert(bool, 'Falied to test .aA1')
  assert_component_summary('str_to_low(...)')

!Test str_to_log(...)
  assert_component_begin('str_to_log(...)')
  assert(str_to_log('yes'), 'yes')
  assert(str_to_log('on'), 'on')
  assert(str_to_log('true'), 'true')
  assert(.not. str_to_log('no'), 'no')
  assert(.not. str_to_log('off'), 'off')
  assert(.not. str_to_log('false'), 'false')
  assert_component_summary('str_to_log(...)')

!Test str_slash_replace(...)
  assert_component_begin('str_slash_replace(...)')
  assert(str_slash_replace('') == '', "Failed to test''")
#ifdef SYS_WIN32
  bool = (str_slash_replace('AS1/as1') == 'AS1\as1')
  assert(bool, 'Failed to test AS1/as1')
  bool = (str_slash_replace('AS1\as1') == 'AS1\as1')
  assert(bool, 'Failed to test AS1/as1')
#else
  bool = (str_slash_replace('AS1/as1') == 'AS1/as1')
  assert(bool, 'Failed to test AS1/as1')
  bool = (str_slash_replace('AS1\as1') == 'AS1/as1')
  assert(bool, 'Failed to test AS1/as1')
#endif
  assert_component_summary('str_slash_repalce(...)')

!Test str_replace(...)
  assert_component_begin('str_replace(...)')
  assert(str_replace('','','') == '', "Failed to test ''")
  bool = (str_replace('abcdefabcdef','de','DE') == 'abcDEfabcDEf')
  assert(bool, 'Failed to test abcdefancdef')
  bool = (str_replace('abcdefabcdef','q','p') == 'abcdefabcdef')
  assert(bool, 'Failed to test abcdefancdef')
  assert_component_summary('str_replace(...)')

!Test str_find(...)
  assert_component_begin('str_find(...)')
  assert(str_find('','',1) == 0,"Failed to test ''")
  assert(str_find('','1',1) == 0,"Failed to test ''")
  bool = (str_find('abcdabcd','ab',2) == 5)
  assert(bool, 'Falied to test abcdabcdabcd')
  bool = (str_find('abcdabcd','ab',6) == 0)
  assert(bool, 'Falied to test abcdabcdabcd')
  assert_component_summary('str_find(...)')

!Test procesure_str_cout(...)
  assert_component_begin('str_cout(...)')
  assert(str_cout('','') == 0, "Failed to test ''")
  bool = (str_cout('abcdeabcde', 'de') == 2)
  assert(bool, 'Failed to test abcdeabcde')
  assert(str_cout('  ',' ') == 2, "Failed to test ''")
  assert(bool, 'Failed to test abcdeabcde')
  assert(str_cout(' A ',' ') == 2, "Failed to test ''")
  assert(bool, 'Failed to test abcdeabcde')
  assert_component_summary('str_cout(...)')

!Test int_to_str(...)
  assert_component_begin('int_to_str(...)')
  bool = (to_str(123456) == '123456')
  assert(bool, 'Failed to test 123456')
  bool = (to_str(123000456) == '123000456')
  assert(bool, 'Failed to test 123456')
  assert_component_summary('int_to_str(...)')

!Test real_to_str(...)
  assert_component_begin('real_to_str(...)')
  bool = (to_str(123.45_krp) == '123.450')
  assert(bool, 'Failed to test 123.45')
  bool = (to_str(123.45e-2_krp) == '1.23450')
  assert(bool, 'Failed to test 123.45')
  bool = (to_str(123.45e-3_krp) == '1.23450E-01')
  assert(bool, 'Failed to test 123.45')
  assert_component_summary('real_to_str(...)')

!Test str_to_int(...)
  assert_component_begin('str_to_int(...)')
  bool = (str_to_int(' 123 ') == 123)
  assert(bool, 'Failed to test  123 ')
  bool = (str_to_int(' 123a ') == -huge(0))
  assert(bool, 'Failed to test  123a ')
  assert_component_summary('str_to_int(...)')

!Test str_split(...)
  assert_component_begin('str_split(...)')
  call str_split(' word1 word2, word3; word4:word5/word6\:,;word7',words,      &
    ' ',',',';',':')
  assert(size(words) == 6, 'Failed')
  if(size(words) == 6) then
    bool = (trim(words(1)) == 'word1' .and. trim(words(2)) == 'word2'          &
    .and. trim(words(3)) == 'word3' .and. trim(words(4)) == 'word4' .and.      &
    trim(words(5)) == 'word5/word6\' .and. trim(words(6)) == 'word7')
    assert(bool, 'Failed')
  endif
  call str_split(' word1 word2, !aaa',words)
  assert(size(words) == 3, 'str_split_space is not orrect')

  call str_split(' word1 word2, !aaa',words,'!')
  assert(size(words) == 2, 'str_split_one is not correct')
  if(size(words) == 2) &
    assert(trim(adjustl(words(1))) == 'word1 word2,','str_split is not correct')

  call str_split(' word1 word2, !',words,'!')
  assert(size(words) == 1, 'str_split_one is not correct')
  assert(trim(adjustl(words(1))) == 'word1 word2,','str_split is not correct')

  assert_component_summary('str_split(...)')

!Test str_isInt(...)
  assert_component_begin('str_isInt(...)')
  assert(str_isInt('123'), 'Failed to test 123')
  assert(.not.str_isInt('a 123'),'Failed to test a 123')
  assert_component_summary('str_isInt(...)')

!Test str_isReal(...)
  assert_component_begin('str_isReal(...)')
  assert(str_isReal('123.456'),'Failed to test 123.456')
  assert(.not.str_isReal('a 123.456'),'Failed to test a 123.456')
  assert_component_summary('str_isReal(...)')

!Test str_match(...)
  bool = (str_match('Aa Bb/Cc','Cc'))
  assert_component_begin('str_match(...)')
  assert(bool,'Failed to test Aa Bb/Cc' )
  bool = (.not. str_match('Aa Bb/Cc','Dd'))
  assert(bool,'Failed to test Aa Bb/Cc')
  assert_component_summary('str_match(...)')

!Test str_name_of_str(...)
  assert_component_begin('str_name_of_str(...)')
  bool = (str_name_of_file(' abc\def/pangpang ') == 'pangpang')
  assert(bool, 'Failed to test abc\def/pangpang')
  bool = (str_name_of_file('abc\d.ef/pangpang') == 'pangpang')
  assert(bool, 'Failed to test abc\d.ef/pangpang')
  bool = (str_name_of_file('pangpang') == 'pangpang')
  assert(bool, 'Failed to test pangpang')
  bool = (str_name_of_file('abc\def/pangpang.txt') == 'pangpang')
  assert(bool, 'Failed to test abc/def/pangpang.txt')
  bool = (str_name_of_file('abc/def\pangpang.txt') == 'pangpang')
  assert(bool, 'Failed to test abc/def\pangpang.txt')
  bool = (str_name_of_file('pangpang.txt.rar.7z') ==                     &
    'pangpang.txt.rar')
  write(*,*) '<'//str_name_of_file('pangpang.txt.rar.7z')//'>'
  assert(bool, 'Failed to test pangpang.txt.rar.7z')
  bool = (trim(str_name_of_file('pangpang.txt.rar.7z\')) ==                    &
    '')
  write(*,*) trim(str_name_of_file('pangpang.txt.rar.7z\'))
  assert(bool, 'Failed to test pangpang.txt.rar.7z')
  assert_component_summary('str_name_of_str(...)')

!Test str_ext_of_file(...)
  assert_component_begin('str_ext_of_str(...)')
  bool = (str_ext_of_file('abc/def/pangpang.txt') == '.txt')
  assert(bool, 'Failed to test abc/def/pangpang.txt')
  bool = (str_ext_of_file('abc\def/pangpang.txt') == '.txt')
  assert(bool, 'Failed to test abc/def/pangpang.txt')
  bool = (str_ext_of_file('abc\d.ef/pangpang.txt') == '.txt')
  assert(bool, 'Failed to test abc\d.ef/pangpang.txt')
  bool = (str_ext_of_file('.txt') == '.txt')
  assert(bool, 'Failed to test .txt')
  bool = (str_ext_of_file('txt') == '')
  assert(bool, 'Failed to test txt')
  bool = (str_ext_of_file('') == '')
  assert(bool, "Failed to test ''")
  bool = (str_ext_of_file('input.rar.7z.zip') == '.zip')
  assert(bool, "Failed to test ''")
  assert_component_summary('str_ext_of_str(...)')

!Test str_path_of_file(...)
  assert_component_begin('str_path_of_file(...)')
#ifdef SYS_WIN32
  bool = (str_path_of_file('abc/def\pangpang.txt') == 'abc\def\')
  assert(bool, 'Failed to test abc\def\pangpang.txt')
  bool = (str_path_of_file(' abc/de.f\pangpang.txt') == 'abc\de.f\')
  assert(bool, 'Failed to test abc\def\pangpang.txt')
  bool = (str_path_of_file(' abc/de.f\') == 'abc\de.f\')
  assert(bool, 'Failed to test abc\def\pangpang.txt')
#else
  bool = (str_path_of_file('abc\def/') == 'abc/def/')
  assert(bool, 'Failed to test abc/def/pangpang.txt')
  bool = (str_path_of_file(' abc/de.f\pangpang.txt') == 'abc/de.f/')
  assert(bool, 'Failed to test abc\def\pangpang.txt')
  bool = (str_path_of_file(' abc/de.f\') == 'abc/de.f/')
  assert(bool, 'Failed to test abc\def\pangpang.txt')

  bool = (str_path_of_file('input.txt') == './')
  assert(bool, 'Failed to test input.txt')
#endif
  assert_component_summary('str_path_of_file(...)')

!Test str_join_path(...)
  assert_component_begin('str_join_path(...)')
  bool = (trim(str_join_path('abc\def/','pangpang','.txt')) == 'abc'//    &
    slash//'def'//slash//'pangpang.txt')
  assert(bool, 'Failed to test abc\def/, pangpang, txt')

  bool = (trim(str_join_path('ab\c/def','pangpang','.txt')) == 'ab'//     &
    slash//'c'//slash//'def'//slash//'pangpang.txt')
  assert(bool, 'Failed to test ab\c/def, pangpang, .txt')

  bool = (trim(str_join_path('abc\def/','','.txt')) == 'abc'//slash//     &
    'def'//slash//'.txt')
  assert(bool, "Failed to test 'abc\def/','','.txt'")

  bool = (trim(str_join_path('ab/c\def',' ',' .txt ')) == 'ab'//slash//'c'   &
    //slash//'def'//slash//'.txt')
  assert(bool, "Failed to test ' ab/b\def ',' ',' txt'")

  bool = (str_join_path(' abc\def/ ','pangpang','') == 'abc'//slash        &
    //'def'//slash//'pangpang')
  assert(bool, "Failed to test ' abc\def/ ',' pangpang ',''")

  bool = (trim(str_join_path(' ab/c\def ',' pangpang','')) == 'ab'//slash   &
    //'c'//slash//'def'//slash//'pangpang')
  assert(bool, "Failed to test 'ab/c\def','pangpang',''")

  bool = (trim(str_join_path(' ',' pangpang ',' .txt ')) == 'pangpang.txt')
  assert(bool, "Failed to test '','pangpang','txt'")

  bool = (trim(str_join_path('','pangpang','')) == 'pangpang')
  assert(bool, "Failed to test '','pangpang',''")
  assert_component_summary('str_join_path(...)')

  assert_finish()

endprogram test_string
