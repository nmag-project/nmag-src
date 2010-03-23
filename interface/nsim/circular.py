# (C) 2009 University of Southampton
#     by Matteo Franchin

class CircularList:
    """Implement a circular list with maximum size. When the list is full and
    an element is appended, the oldest element is removed such that the total
    size of the list is not increased."""

    def __init__(self, size):
        self.idx = 0
        self.size = size
        self.buf = []

    def append(self, item):
        if len(self.buf) < self.size:
            self.buf.append(item)
            self.idx = len(self.buf) - 1

        else:
            self.idx = (self.idx + 1) % self.size
            self.buf[self.idx] = item

    def get_list(self):
        l = len(self.buf)
        i0 = self.idx - l + 1 
        return [self.buf[(i0 + i) % self.size]
                for i in range(len(self.buf))]

if __name__ == "__main__":
    c = CircularList(10)
    for i in range(100):
        c.append(i)
        print c.get_list()
        raw_input()

