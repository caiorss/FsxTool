
src    := src/Core.fsx src/Dtime.fsx src/HUnit.fsx src/HttpReq.fsx
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



