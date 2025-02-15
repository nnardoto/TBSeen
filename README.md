TBSeen: A Tight Binding Code for Beyond DFT Exploration
=======================================================

Prerequisites
-------------

* Lapack and Blas Libraries
* Fortran Package Manager

Instalation of Fortran Package Manager
--------------------------------------

* Get the newest version of [Fortran Package Manager](https://fpm.fortran-lang.org/install/index.html)
```
wget https://github.com/fortran-lang/fpm/releases/download/v0.10.1/fpm-0.10.1-linux-x86_64 -O ~/.local/bin/
chmod +x ~/.local/bin/fpm
```

 Instalation (Shortest Way)
 --------------------------

```
git clone --depth 1 https://github.com/nnardoto/TBSeen
cd TBSeen
fpm build
fpm install
```

Usage
-----

```
TBSeen Input_File.in
```
