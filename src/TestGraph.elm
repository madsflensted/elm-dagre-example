module TestGraph exposing (data)


data : String
data =
    """# Nodes are specified as
# <identifier> : <label>

kspacey : Kevin Spacey
swilliams : Saul Williams
bpitt : Brad Pitt
hford : Harrison Ford
lwilson : Luke Wilson
kbacon : Kevin Bacon


# Edges are specified as
# <node id 1> -> <node id 2> : <edge label>
kspacey -> swilliams : worked with
swilliams -> kbacon : worked with
bpitt -> kbacon : worked with
hford -> lwilson : worked with
lwilson -> kbacon : worked with
"""
