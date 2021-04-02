# FPTOOLS_SET_C_LD_FLAGS
# ----------------------------------
# Set the C, LD and CPP flags for a given platform.
# $1 is the platform
# $2 is the name of the CC flags variable
# $3 is the name of the linker flags variable when linking with gcc
# $4 is the name of the linker flags variable when linking with ld
# $5 is the name of the CPP flags variable
AC_DEFUN([FPTOOLS_SET_C_LD_FLAGS],
[
    AC_REQUIRE([FP_PROG_LD_IS_GNU])
    AC_MSG_CHECKING([Setting up $2, $3, $4 and $5])
    case $$1 in
    i386-*)
        # Workaround for #7799
        $2="$$2 -U__i686"
        ;;
    esac

    case $$1 in
    i386-unknown-mingw32)
        $2="$$2 -march=i686"
        ;;
    i386-portbld-freebsd*)
        $2="$$2 -march=i686"
        ;;
    x86_64-unknown-solaris2)
        # Solaris is a multi-lib platform, providing both 32- and 64-bit
        # user-land. It appears to default to 32-bit builds but we of course want to
        # compile for 64-bits on x86-64.
        #
        # On OpenSolaris uses gnu ld whereas SmartOS appears to use the Solaris
        # implementation, which rather uses the -64 flag.
        $2="$$2 -m64"
        $3="$$3 -m64"
        $5="$$5 -m64"
        if test "$fp_cv_gnu_ld" = "yes"; then
            $4="$$4 -m64"
        else
            $4="$$4 -64"
        fi
        ;;
    alpha-*)
        # For now, to suppress the gcc warning "call-clobbered
        # register used for global register variable", we simply
        # disable all warnings altogether using the -w flag. Oh well.
        $2="$$2 -w -mieee -D_REENTRANT"
        $3="$$3 -w -mieee -D_REENTRANT"
        $5="$$5 -w -mieee -D_REENTRANT"
        ;;
    hppa*)
        # ___HPUX_SOURCE, not _HPUX_SOURCE, is #defined if -ansi!
        # (very nice, but too bad the HP /usr/include files don't agree.)
        $2="$$2 -D_HPUX_SOURCE"
        $3="$$3 -D_HPUX_SOURCE"
        $5="$$5 -D_HPUX_SOURCE"
        ;;

    arm*freebsd*)
        # On arm/freebsd, tell gcc to generate Arm
        # instructions (ie not Thumb).
        $2="$$2 -marm"
        $3="$$3 -Wl,-z,noexecstack"
        $4="$$4 -z noexecstack"
        ;;
    arm*linux*)
        # On arm/linux and arm/android, tell gcc to generate Arm
        # instructions (ie not Thumb).
        $2="$$2 -marm"
        $3="$$3 -Wl,-z,noexecstack"
        $4="$$4 -z noexecstack"
        ;;

    aarch64*freebsd*)
        $3="$$3 -Wl,-z,noexecstack"
        $4="$$4 -z noexecstack"
        ;;
    aarch64*linux*)
        $3="$$3 -Wl,-z,noexecstack"
        $4="$$4 -z noexecstack"
        ;;

    powerpc-ibm-aix*)
        # We need `-D_THREAD_SAFE` to unlock the thread-local `errno`.
        $2="$$2 -D_THREAD_SAFE"
        $3="$$3 -D_THREAD_SAFE -Wl,-bnotextro"
        $4="$$4 -bnotextro"
        $5="$$5 -D_THREAD_SAFE"
        ;;

    x86_64-*-openbsd*)
        # We need -z wxneeded at least to link ghc-stage2 to workaround
        # W^X issue in GHCi on OpenBSD current (as of Aug 2016)
        $3="$$3 -Wl,-z,wxneeded"
        $4="$$4 -z wxneeded"
        ;;

    esac

    AC_MSG_RESULT([done])
])
