set(example_programs
    example_point_no_type.f90
    example_point_type.f90
    example_distance_function.f90
    example_distance_type_bound_procedure.f90
    # example_defining_possibilities.f90 Does not compile on purpose
    example_adding_vectors.f90
    example_adding_vectors_type_bound_alternative.f90
    example_vector_no_constructor.f90
    example_vector_constructor.f90
    # example_vector_private_components.f90
    example_vector_constructor_getters_setters.f90
    example_vector_factory.f90
    example_logging.f90
    example_logging_interface.f90
    example_logging_multilogger.f90
    example_custom_concat.f90
    example_logging_final_procedure.f90
)

foreach(program_file IN LISTS example_programs)
    get_filename_component(program_name "${program_file}" NAME_WE)
    add_executable("${program_name}" "${program_file}")
    target_link_libraries("${program_name}" PRIVATE
        all_compiler_warnings
        compiler_warnings_as_errors
    )
endforeach()
