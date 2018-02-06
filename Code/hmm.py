import numpy as np

l = input('Enter the length of sequence you want: ')

distribution = input('Enter four numbers for probability of A, C, T, G on Noncoding state, seperated by white space : ')
p1, p2, p3, p4 = distribution.split()
Noncoding_dis = np.array([p1, p2, p3, p4])

p1 = float(p1)
p2 = float(p2)
p3 = float(p3)
p4 = float(p4)

while(p1 + p2 + p3 + p4 != 1.0):
	Noncoding_state_probability = input('Invalid Probability Distribution For Noncoding State. Enter Again')
	p1, p2, p3, p4 = Noncoding_state_probability.split()
	p1 = float(p1)
	p2 = float(p2)
	p3 = float(p3)
	p4 = float(p4)

#------------------------------------------------------------------------------------------------------------

distribution2 = input('Enter four numbers for probability of A, C, T, G on Coding state, seperated by white space : ')
c1, c2, c3, c4 = distribution2.split()
Coding_dis = np.array([c1, c2, c3, c4])

# c1 = float(c1)
# c2 = float(c2)
# c3 = float(c3)
# c4 = float(c4)

# while(c1 + c2 + c3 + c4 != 1.0):
# 	Coding_state_probability = input('Invalid Probability Distribution For Coding State. Enter Again')
# 	c1, c2, c3, c4 = Coding_state_probability.split()
# 	c1 = float(c1)
# 	c2 = float(c2)
# 	c3 = float(c3)
# 	c4 = float(c4)

#------------------------------------------------------------------------------------------------------------

transition_probability = input('Enter the transition probability for Coding State and Noncoding State (Order: Coding to Coding, Coding to Noncoding, Noncoding to Coding, Noncoding to Noncoding): ')
t1, t2, t3, t4 = transition_probability.split()

t1 = float(t1)
t2 = float(t2)
t3 = float(t3)
t4 = float(t4)

while(t1 + t2 != 1.0 or t3 + t4 != 1.0):
	transition_probability = input('Not valid probability. Enter Again.')
	t1, t2, t3, t4 = transition_probability.split()
	t1 = float(t1)
	t2 = float(t2)
	t3 = float(t3)
	t4 = float(t4)

#------------------------------------------------------------------------------------------------------------

cur_state = input('Enter the initial state(Coding or Noncoding): ')

print(Noncoding_dis)
print(Coding_dis)
print(transition_probability)

global sequence
sequence = ''

def generator_in_non_coding_state(g1, g2, g3, g4):
	x = np.random.choice(['A', 'T', 'C', 'G'], 1, p=[g1, g2, g3, g4]) 
	cur_char = str(x[0])
	global sequence
	sequence = sequence + cur_char	

def generator_in_coding_state(g1, g2, g3, g4):
	x = np.random.choice(['A', 'T', 'C', 'G'], 1, p=[g1, g2, g3, g4]) 
	cur_char = str(x[0])
	global sequence
	sequence = sequence + cur_char		

l = int(l)

for i in range(l):
	print(cur_state)
	if(cur_state == 'Noncoding'):
		generator_in_non_coding_state(p1, p2, p3, p4)
		next_state = np.random.choice(['Coding', 'Noncoding'], 1, p=[t3, t4])#t4, t3
		cur_state = next_state
	else:
		generator_in_coding_state(c1, c2, c3, c4)
		next_state = np.random.choice(['Coding', 'Noncoding'], 1, p=[t1, t2]) #t1, t2
		cur_state = next_state
print(sequence)