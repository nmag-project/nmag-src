
__all__ = ['Computation']

class Computation:
    def __init__(self, inputs=[], outputs=[]):
        self.inputs = inputs
        self.outputs = outputs
        inouts = inputs + outputs
        self.inouts = inouts
        inouts_dict = {}
        for q in inouts:
            inouts_dict[q.name] = q
        self.inouts_dict = inouts_dict

