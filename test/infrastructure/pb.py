from sys import stdout as out

class Progressbar:
	def __init__(self, width, err = "!", succ="="):
		self.width   = width
		self.max_l   = 0
		self.symbols = list()
		self.err     = err
		self.succ    = succ
		for i in range(width):
			self.symbols.append(succ)
		

	def update(self, p, postfix, success=True):
		progress  = int(float(p)*self.width)
		if not success:
			self.symbols[progress] = "!"
		bar = ""
		for i in range(progress):
			bar += self.symbols[i]
		#bar  = progress * "="
		#assert progress <= self.width
		remaining = (self.width - progress-1)*" "
		res = "[%s]" % (bar+ ">"+remaining) + " " + postfix
		w = (self.max_l - len(res))
		self.max_l = max(len(res), self.max_l)
		res += w*" "
		out.write(res)
		out.flush()
		out.write("\b" * len(res))
	
	def done(self):
		out.write("[")
		for e in self.symbols:
			out.write(e)
		out.write("] Done." +(self.max_l - self.width - 8) * " " + "\n")
		out.flush()


