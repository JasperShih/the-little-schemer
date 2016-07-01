# -*- coding: utf8 -*-
null = []

def is_list(arg):
	return type(arg) == list
	
def is_null(lst):
	return lst == []

def is_atom(lst):
	lst_not_eq_null = (not is_null(lst))
	lst_not_eq_list = (not is_list(lst))
	
	return lst_not_eq_null and \
		   lst_not_eq_list	   

def car(lst):
	return lst[0]
	
def cdr(lst):
	return lst[1:]

def show(arg):
	print arg

def get_left_atom(lst):
	lst_car = []
	lst_cdr = []
	if (not is_null(lst)):
		lst_car = car(lst)
		lst_cdr = cdr(lst)
		

	if is_null(lst):
		return []
	elif is_atom(lst_car):
		show(lst_car)
	else:
		get_left_atom(lst_car)
		#上面一行就算找到atom, 程式仍然會執行下面一行, 
		#我們希望的是:若上面一行有找到atom就不執行下面一行
		return get_left_atom(lst_cdr)

#def start(lst):
	#get_left_atom(lst)
	
	

def trit(arg):
	is_list(arg)
	return car(arg)
	

get_left_atom([[], ["a"], "b", "c"])















