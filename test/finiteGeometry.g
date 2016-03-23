LoadPackage("fining");;
LoadPackage("grape");;

q  := 3;;

ps := HermitianPolarSpace(4,q^2);;
linesG := AsList(Lines(ps));;

# ProjectiveDimension == -1 implies the lines do not meet at any point (which is what we need).
adj := function(x,y)
    return ProjectiveDimension(Meet(x,y))=-1;
end;

graph := Graph(CollineationGroup(ps), linesG, \^, adj);;

# lines -> vertex labels map
d := NewDictionary(Random(Lines(ps)), true);;
for v in Vertices(graph) do
    vn := VertexName(graph, v);
    AddDictionary(d, vn, v);
od;

# Given a set of graph vertices calculate possible *unique* choices
# based on the geometry collineation information calculations.
# Return: New set of candidate vertices or [0] on Trivial group (no more symmetries to break).
getCandidates := function (vertices)
    local s, lns, ls, l, orbs, o, res;

    # Convert into fining format
    lns := List(vertices, l -> VertexName(graph, l));

    # Calculate our new group
    s := CollineationGroup(ps);;
    for l in lns do
        s := FiningStabiliser(s, l);
        if IsTrivial(s) then
            return [0];
        fi;
    od;

    # Get reduced geometry
    ls := AsList(Lines(ps));
    for l in lns do
        ls := Filtered(ls, x -> x <> l);
    od;

    # Do I want OnSetsProjSubspaces here? Probably
    orbs := FiningOrbit(s, ls, OnSetsProjSubspaces);

    # Get a choice from each orbit
    res := [];
    for o in orbs do
        Add(res, o[1]);
    od;

    # Convert back to vertex labelling
    return List(res, l -> LookupDictionary(d, l));
end;;
