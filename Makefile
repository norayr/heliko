# makefile for maemo-leste which is based on devuan chimaera
# define the architecture
ARCH = arm-linux

# determine the architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# set the ARCH variable based on the detected architecture
ifeq ($(UNAME_S),Linux)
    ifeq ($(UNAME_M),x86_64)
        ARCH := x86_64-linux
    else ifeq ($(UNAME_M),aarch64)
        ARCH := aarch64-linux
    else ifeq ($(UNAME_M),armv7l)
        ARCH := arm-linux
    else
        ARCH := unknown
    endif
else
    ARCH := unknown
endif

# print the detected architecture
$(info ARCH is set to $(ARCH))

# define the path to lazarus, this is where is it in case of chimaera
# for normal build we don't need this
# LAZARUS = "/usr/lib/lazarus/2.0.10"

# Define the PATH to include the necessary directories for fpc and fpcres
CUSTOM_PATH = "/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH = $(CUSTOM_PATH)

# Define the project and output file
PROJECT = project1.lpr
OUTFILE = heliko

# Define the FPC command
FPC = fpc

# Default target to build the project
all:
	lazbuild $(PROJECT)

