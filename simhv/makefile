# CC Command
#
# Note: -O2 is sometimes broken in GCC when setjump/longjump is being
# used. Try -O2 only with released simulators.
#
ifeq ($(WIN32),)
#Unix Environments
ifeq ($(OSTYPE),solaris)
OS_CCDEFS = -lsocket -lnsl -lpthread -D_GNU_SOURCE
else
OS_CCDEFS = -D_GNU_SOURCE
endif
ifeq ($(OSTYPE),macos)
CC = gcc -std=c99 -O2 -U__STRICT_ANSI__ -g -lm -lrt $(OS_CCDEFS) -I .
else
CC = gcc -std=c99 -O2 -U__STRICT_ANSI__ -g -lm $(OS_CCDEFS) -I .
endif
ifeq ($(USE_NETWORK),)
else
NETWORK_OPT = -DUSE_NETWORK -isystem /usr/local/include /usr/local/lib/libpcap.a
endif
else
#Win32 Environments
LDFLAGS = -lm -lwsock32 -lwinmm
CC = gcc -std=c99 -U__STRICT_ANSI__ -O0 -I.
EXE = .exe
ifeq ($(USE_NETWORK),)
else
NETWORK_OPT = -DUSE_NETWORK -lwpcap -lpacket
endif
endif

#
# Common Libraries
#
SIM = scp.c sim_console.c sim_fio.c sim_timer.c sim_sock.c \
	sim_tmxr.c sim_ether.c sim_tape.c


#
# Emulator source files and compile time options
#
PDP11D = PDP11
PDP11 = ${PDP11D}/pdp11_fp.c ${PDP11D}/pdp11_cpu.c ${PDP11D}/pdp11_dz.c \
	${PDP11D}/pdp11_cis.c ${PDP11D}/pdp11_lp.c ${PDP11D}/pdp11_rk.c \
	${PDP11D}/pdp11_rl.c ${PDP11D}/pdp11_rp.c ${PDP11D}/pdp11_rx.c \
	${PDP11D}/pdp11_stddev.c ${PDP11D}/pdp11_sys.c ${PDP11D}/pdp11_tc.c \
	${PDP11D}/pdp11_tm.c ${PDP11D}/pdp11_ts.c ${PDP11D}/pdp11_io.c \
	${PDP11D}/pdp11_rq.c ${PDP11D}/pdp11_tq.c ${PDP11D}/pdp11_pclk.c \
	${PDP11D}/pdp11_ry.c ${PDP11D}/pdp11_pt.c ${PDP11D}/pdp11_hk.c \
	${PDP11D}/pdp11_xq.c ${PDP11D}/pdp11_xu.c ${PDP11D}/pdp11_vh.c \
	${PDP11D}/pdp11_rh.c ${PDP11D}/pdp11_tu.c ${PDP11D}/pdp11_cpumod.c \
	${PDP11D}/pdp11_cr.c ${PDP11D}/pdp11_rf.c ${PDP11D}/pdp11_dl.c \
	${PDP11D}/pdp11_ta.c
PDP11_OPT = -DVM_PDP11 -I ${PDP11D} ${NETWORK_OPT}

all: pdp11

clean :
	rm -rf pdp11sim*

#
# Individual builds
#
pdp11 : pdp11sim${EXE}

pdp11sim${EXE} : ${PDP11} ${SIM}
	${CC} ${PDP11} ${SIM} ${PDP11_OPT} -o $@ ${LDFLAGS}
