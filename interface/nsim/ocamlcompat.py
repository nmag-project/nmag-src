'''Module which provides stubs for the OCaml Nmag FEM library (in case the
OCaml Nmag core has not been installed).
'''

try:
    import ocaml
    snippets_register_feature = ocaml.register_feature
    parse_physical_dimensions = ocaml.parse_physical_dimensions

except:
    def not_there(*args):
        raise "OCaml Nmag FEM builtin library is not present!"

    def ignore(*arg):
        pass

    snippets_register_feature = ignore
    nlog_setupLogger = ignore
    nlog_register_handler = ignore

    def petsc_mpi_nr_nodes():
        return 1

    def snippets_all_features():
        return []

    def version():
        return "unknown version"

    def get_nsim_sundials_library_path():
        return ""

    def time_vmem_rss():
        return (0.0, 0.0, 0.0)

