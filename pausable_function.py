# -*- coding: utf8 -*-

# 將global變數整進function中
# Can we merge play and start functions together?
# abstract similiry logic
# review the code and think about it

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

	
board = []
buf = []
def helper(lst):
	global board, buf
	lst_car = []
	lst_cdr = []
	if (not is_null(lst)):
		lst_car = car(lst)
		lst_cdr = cdr(lst)
		
		
	if is_null(lst):
		board = "got_null"
		return []
	elif is_atom(lst_car):
		board = "got_atom"
		buf.append(lst_cdr)
		return lst_car
	else:
		lst_car_result = helper(lst_car)
		if (board == "got_atom"):
			buf.append(lst_cdr)
			return lst_car_result
		else:
			return helper(lst_cdr)

def step(lst):
	if is_null(lst):
		return "List exhausted"
	else:
		
	
	return atom

def start(lst):
	result = helper(lst)
	if is_null(result):
		print "List exhausted"
	else:
		print result
	
def play():
	global buf
	if is_null(buf):
		print "List exhausted"
	else:
		result = helper(car(buf))
		buf = cdr(buf)
		if is_null(result):
			play()
		else:
			print result
	

start([[], ["a"], "b", "c"])
play()
play()
play()
play()


def receiver(atom):
	print atom
	

def get_left(lst):
	lst_car = None
	
	if is_null(lst):
		lst_car = car(lst)
		lst_cdr = cdr(lst)
		return []
	else: #
		is_atom(lst_car):
		receiver(lst_car)
		get_left(lst_cdr)
	else:
		get_left(lst_car)
		get_left(cdr(lst))
	

		
	
	
		
# atom
# null
# list
	
	
	




"""
#it is same form as
# (let()
#	(funct1 arg)
#	(funct2 arg))

def trit(arg):
	is_list(arg)
	return car(arg)

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
		# try to approach letcc
		lst_car_return = get_left_atom(lst_car)
		condition = is_atom(lst_car_return)
		if (not condition):
			return get_left_atom(lst_cdr)
#	else:
#		if (not is_atom(get_left_atom(lst_car))):
#			return get_left_atom(lst_cdr)			
"""










