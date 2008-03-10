SKRIBILO := skribilo -v2

docs := index.skb
products := $(docs:%.skb=%.html)


all: $(products)

%.html: %.skb
	$(SKRIBILO) --target=html -o $@ $^

clean:
	-rm -f $(products)


.PHONY: all clean

# arch-tag: 38f80131-ee45-4067-8638-2d2db2963e95
