import torch as t
a = t.FloatTensor([1])
a.requires_grad=True
a.retain_grad()
b = a * 2
b.retain_grad()
c = a * 3
c.retain_grad()
d = a * 4
e = b*(c+d)
#print(a.is_leaf)
#print(e.is_leaf)
print(e.grad_fn)
e.backward(retain_graph = True)
print(c.grad)
print(b.grad)
print(d.grad)
