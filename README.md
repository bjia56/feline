# feline
FELINE: Esoteric Language Incorporating Novel Elements

## Usage

Build the executable:
```
make
```

Run the executable on input files and test output:
```
./feline.native -file file1.cat -file file2.cat -out out.exe
./out.exe
```

Run compiler test cases:
```
./feline.native -test
```

Generate IR:
```
./feline.native -file file1.cat -ir
cat file1.ir
```

Build with gcc static linking (sometimes required in more recent Linux distros):
```
./feline.native -file file1.cat -out out.exe -static
```
