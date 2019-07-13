include $(MK)/common.mk

all: lib$(LIB)_pic.a

lib$(LIB)_pic.a: $(SHOBJS)
	$(AR) cr $@ $(SHOBJS)
	$(RANLIB) $@

clean:
	rm -f lib$(LIB)_pic.a $(SHOBJS)

install:
	# do-nothing
