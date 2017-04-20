
src    := src/FCore.fsx src/Dtime.fsx
target := FsxTool.dll
xml    := FsxTool.xml

# Output directory
out := bin

FSC := fsharpc

#----------------------------------------#

all: target

#
target: $(src)
	$(FSC) $(src) --out:$(out)/$(target) --doc:$(out)/$(xml) --target:library



