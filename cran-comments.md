## Resubmission 

Resubmitting `elisr` (0.1.0). First of all, thank you for your comments and
suggestions. I (1) added a reference describing the method in DESCRIPTION:
`Müller-Schneider (2001) <doi:10.1515/zfsoz-2001-0404>)`; (2) changed `elisr` to
`'elisr'`; (3) added `\value` to all .Rd files and explained the functions
results as well as the structure of the output in the documentation.
Additionaly, if a function is only called for its side effects, I mentioned it.

## Test environments

- Linux
  - Manjaro (21.0), R-devel and R-release (local machine)
  - Debian, R-devel and R-release, GCC (on rhub)
  - Ubuntu (20.04 LTS), R-devel and R-release, GCC (on rhub)
  - Fedora, R-devel and R-release, clang, gfortran (on rhub)
- macOS 
  - 10.13.6 High Sierra, R-release (on rhub)
- Windows 
  - Windows Server 2008 R2 SP1, R-devel & R-release, 32/64 bit (on rhub &
  win-builder)

## R CMD check results

New submission

0 errors ✓ | 0 warnings ✓ | 1 note x
