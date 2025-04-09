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
    example_logging_logger_factory.f90
    example_logging_logger_factory_with_pointer.f90
    example_2d_vector_generic_type_bound_procedure.f90
    example_logging_file_and_print_logger.f90
    example_logging_file_logger_final_procedure_bad_example.f90
    example_integrate_trapezoid_with_procedure_pointer.f90
    example_integrate_trapezoid_with_function_argument_interface.f90
    example_plot_function_to_be_integrated.f90
    example_integrate_trapezoid_procedure_pointer_performance.f90
    example_integrate_trapezoid_with_derived_type.f90
    example_timer_and_timer_list_copies.f90
    example_timer_and_timer_list_improved.f90
    example_timer_and_timer_list_get_allocated.f90
    example_timer_and_timer_linked_list.f90
    example_logging_logger_factory_with_pointer_and_multi_logger.f90
    example_multi_logger_tryout.f90
    example_wrapping_instances.f90
)

foreach(program_file IN LISTS example_programs)
    get_filename_component(program_name "${program_file}" NAME_WE)
    add_executable("${program_name}" "${program_file}")
    target_link_libraries("${program_name}" PRIVATE
        all_compiler_warnings
        compiler_warnings_as_errors
    )
endforeach()
