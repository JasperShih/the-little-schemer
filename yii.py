# -*- coding: utf8 -*-

# type:
# yii 一

# function:
# chi 取
# yiu 餘
# glue

#任何兩個東西要連在一起, 就是要用glue
#沒有glue的data structure不是空就是yii_U

#圍繞著數據結構寫程式
#基本上不考慮空情形

def glue(item_1, item_2):
	return item_1 + " . " + item_2

	
#print glue(glue("A", glue("B", "()")), glue("C", glue("D", "()")))
print glue(glue(glue("()", glue(glue("()", "A"), "B")), "C"), "D")
# () . [() . A . B] . C . D





def is_yii(data_structure, base_data_structure = "universe"):
	if base == "universe":
		if contain_glue(data_structure):
			# case1: empty 
			# case2: yii_U
			return True
		else:
			return False
		
	#else:
		

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		