# Tests for lazy BCO generation in GHCi
#
# These tests verify that when -fwrite-if-simplified-core is enabled,
# GHCi does not eagerly generate bytecode for all modules in the
# dependency graph. Only modules whose code is actually evaluated
# should have BCOs in memory.

# Correctness: verify that loading and evaluation work correctly
# with -fwrite-if-simplified-core, both on initial load and reload.
test('LazyBCOsCorrectness',
     [ req_interp,
       pre_cmd('./genManyModules 200'),
       extra_files(['genManyModules']),
       extra_hc_opts('-fwrite-if-simplified-core'),
     ],
     ghci_script,
     ['LazyBCOsCorrectness.script'])

# Memory benchmark: measure peak residency when loading 200 modules.
# With lazy BCO generation, this should be significantly lower than
# eager BCO generation because only the evaluated module's bytecode
# is in memory.
test('LazyBCOsResidency',
     [ req_interp,
       collect_compiler_residency(15),
       pre_cmd('./genManyModules 200'),
       extra_files(['genManyModules']),
       extra_hc_opts('-fwrite-if-simplified-core'),
       compile_timeout_multiplier(5),
     ],
     ghci_script,
     ['LazyBCOsResidency.script'])

# Memory benchmark on reload: after :reload, modules should use the
# lazy CoreBindings path from .hi files.
test('LazyBCOsResidencyReload',
     [ req_interp,
       collect_compiler_residency(15),
       pre_cmd('./genManyModules 200'),
       extra_files(['genManyModules']),
       extra_hc_opts('-fwrite-if-simplified-core'),
       compile_timeout_multiplier(5),
     ],
     ghci_script,
     ['LazyBCOsResidencyReload.script'])
