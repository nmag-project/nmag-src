import nmag
import nmesh
import nmag.fefields as ff
import ocaml

def generate_test_mesh_and_field():
  '''We define a 1D mesh with two regions:
  
    |<---A--->|<--B-->|
    0 1 2 3 4 5 6 7 8 9
  
  We then define a field "vec" with 3 subfields:
  subfield 'vec_a' is defined in region A
  subfield 'vec_b' is defined in region B
  subfield 'vec_ab' is defined in both region A and B'''

  dim = 1
  m = nmesh.generate_1d_mesh([(0.0, 5.0), (5.0, 9.0)], 0.5)

  el_a = ocaml.make_element("vec_a", [3], dim, 1)
  el_b = ocaml.make_element("vec_b", [3], dim, 1)
  el_ab = ocaml.make_element("vec_ab", [3], dim, 1)

  els = [ocaml.empty_element,
         ocaml.fuse_elements(el_ab, el_a), # region A
         ocaml.fuse_elements(el_ab, el_b)] # region B

  def enumerated_list(l): return zip(range(len(l)), l)

  mwe = ocaml.make_mwe("vec",
                       m.raw_mesh, 
                       enumerated_list(els),
                       [],
                       [(1, ['vec_ab', 'vec_a']),
                        (2, ['vec_ab', 'vec_b'])])

  # Create the field and set it to 0 everywhere
  field = ocaml.raw_make_field(mwe, [lambda x, y: 0.0], "", "")
  return m, field

def do_in_region(mesh, regions, do, exclusive=False):
  pointsregions = mesh.pointsregions
  for id in range(len(pointsregions)):    
    pr = pointsregions[id]
    if len(pr) == 1 or not exclusive:
      if True in [(r in regions) for r in pr]:
        do(id)

#-----------------------------------------------------------------------------

def test_set_field_at_site_success():
  reg_a, reg_b = (1, 2)
  mesh, field = generate_test_mesh_and_field()

  def set_field(nm, id):
    ff.set_field_at_site(field, nm, [id], [1, 2, 3])

  set_a  = lambda id: set_field('vec_a', id)
  set_b  = lambda id: set_field('vec_b', id)
  set_ab = lambda id: set_field('vec_ab', id)

  do_in_region(mesh, [reg_a, reg_b], set_ab)
  do_in_region(mesh, [reg_a], set_a)
  do_in_region(mesh, [reg_b], set_b)

def test_set_field_at_site_failure():
  reg_a, reg_b = (1, 2)
  mesh, field = generate_test_mesh_and_field()

  def check_failure(nm, id):
    try:
      ff.set_field_at_site(field, nm, [id], [1, 2, 3])

    except:
      return

    assert False, "set_field_at_site was successful when setting a field " \
                  "(%s) at a site (%d) where it shouldn't be defined!" \
                  % (nm, id)


  check_failure_a  = lambda id: check_failure('vec_a', id)
  check_failure_b  = lambda id: check_failure('vec_b', id)

  do_in_region(mesh, [reg_b], check_failure_a, exclusive=True)
  do_in_region(mesh, [reg_a], check_failure_b, exclusive=True)

