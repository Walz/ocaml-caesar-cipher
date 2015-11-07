# Caesar Cipher

Use caesar cipher to encrypt and decrypt text file using ascii code, meaning every encoding of a text with a key and the same key modulo 255 will give the same result.

## Installation
#### Dependecy
- Ocaml (4.02+)  

*(See `compatibility` branch for previous version of OCaml)*
#### Building
You can use this command to compile :
```
ocamlc caesar.ml -o caesar && rm caesar.c*
```
#### Commands
```
./caesar [MODE] (KEY) SOURCE_FILE DESTINATION_FILE
```
###### Modes :
- *-e* or *--encrypt* KEY
- *-d* or *--decrypt*  KEY
- *-b* or *--break*

The key is an integer and the source file must exist.

###### Examples:
```
./caesar -e 42 secret.txt gibberish.txt
./caesar --decrypt 154 foo.txt bar.txt
./caesar -b nsa_secret.txt lulz.txt
```

## License
This work is under the terms of the Do What The Fuck You Want To Public License (Version 2).
