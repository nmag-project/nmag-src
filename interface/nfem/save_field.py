import ocaml
import nfem as nfem

# A very very very stupid way to determine the dimension of the mesh
# corresponding to one field, but is there any alternative?
# (I'm speaking of entirely pythonic solutions)
def field_dim(field):
  global dim
  dim = -1
  def callback(i, dof_name_stem, site, pos, value):
    global dim
    dim = len(pos)
  nfem.field_entry_wise(field, callback)
  return dim

def save_field(field, fieldname, filename):
  ccode = '''
#line 19 "save_field.py"
  static FILE *f = (FILE *) NULL;

  if ( close_file > 0.0 ) {
    if ( f != (FILE *) NULL) {
      (void) fclose(f);
      f = (FILE *) NULL;
    }

  } else {
    if ( f == (FILE *) NULL )
      f = fopen("''' + filename + '''", "w");

    if ( f != (FILE *) NULL ) {
      if ( have_''' + fieldname + ''' ) {
        double c[3] = {0.0, 0.0, 0.0};
        int i;
        for(i=0; i<((int) dim); i++) c[i] = COORDS(i);
        fprintf(f, "%g %f %f %f %g %g %g\\n",
          time, c[0], c[1], c[2], ''' + \
  fieldname + '''(0), ''' + fieldname + '''(1), ''' + fieldname + '''(2));
      }
    }
  }
  '''

  dim = float(field_dim(field))
  mwe = ocaml.get_mwe(field)
  fn = nfem.site_wise_applicator(
   ["close_file", "time", "dim"],
   ccode,
   field_mwes=[mwe])

  def saver(close=False, time=0.0):
    if close:
      fn([ 1.0, float(time), dim], fields=[field])
    else:
      fn([-1.0, float(time), dim], fields=[field])

  return saver
