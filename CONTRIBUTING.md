# How to contribute

Welcome! You can report issues [here](https://github.com/OpenRadioss/OpenRadioss/issues) or ask questions [there](https://github.com/OpenRadioss/OpenRadioss/discussions).

## Contributing code to OpenRadioss

Please first discuss the changes you wish to make via the [issue](https://github.com/OpenRadioss/OpenRadioss/issues) or the [discussion](https://github.com/OpenRadioss/OpenRadioss/discussions) tabs.
You must be aware of the [license](./LICENSE.md). We will ask you to sign a **Contributor License Agreement** (CLA).

### Settings

Windows users may want to use [git bash](https://gitforwindows.org/) or [WSL](https://docs.microsoft.com/en-us/windows/wsl/install)
* Create and a github account
    * Add an [SSH key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent). 
    * Review your account setting, in particular: [email](https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-github-user-account/managing-email-preferences/setting-your-commit-email-address), [2FA](https://docs.github.com/en/authentication/securing-your-account-with-two-factor-authentication-2fa/configuring-two-factor-authentication)
* [fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo) the OpenRadioss repository
* Install [git-lfs](https://git-lfs.github.com/). 
  On Linux, you may need to install some package first: 
  
  On Rhel,CentOS
    
        sudo yum install git-lfs
   
  On Ubuntu, debian
  
        sudo apt-get install git-lfs
        
   Then, activate LFS: 
   
        git lfs install
        
* [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) your fork and go into the newly created `OpenRadioss` directory. 
* From your local `OpenRadioss` directory, review your git user name and [email](https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-github-user-account/managing-email-preferences/setting-your-commit-email-address). If you don't want to expose your email address: 
    * Check the boxes `Keep my email addresses private` and  `Block command line pushes that expose my email` [here](https://github.com/settings/emails)
    * Find your `ID+username` [here](https://github.com/settings/emails)
    * Type the following:
```bash
git config --global user.email "<ID+username>@users.noreply.github.com"
```
* Add the official repository as a remote: 
```bash
git remote add upstream git@github.com:OpenRadioss/OpenRadioss.git
```
* Now `origin` points to your fork, and `upstream` points to the official OpenRadioss repository

### Contribution workflow 

The typical workflow is described in this picture:
![image](/doc/workflow.png)
It is not recommended to push commits directly into your `main` branch. This branch should be a copy of the official repository. 

* `git checkout main` to go to the main branch of your local directory
* `git pull upstream main` to get the latest revision of the official main branch
* `git checkout -b <devel_branch_name>` to create and go to a development branch   
* Development, loop over:  
    * Open and edit files  
    * `git status` to see which files has been edited  
    * `git add <filename>` each file
    * `git commit -m “<message>” `  with a [good](https://openpbs.atlassian.net/wiki/spaces/DG/pages/6193155/How+To+Write+a+Good+Git+Commit+Message) message.
* Review your history: squash your commits, write a meaningful commit message  
    * `git rebase -i main` provided that your current branch is derived from the `main` branch. 
    *  To squash all your commits into your first one: replace `pick` by `squash` on for all your commits except your first one. Do not squash your commits into someone else's commit. Do not embed someone else's commit into your squashed commit. 
* Rebase your work on the latest version of OpenRadioss (you can also follow [this](https://openpbs.atlassian.net/wiki/spaces/DG/pages/1183744006/Rebasing+Your+Dev+Branch))
    * `git pull --rebase upstream main`  
    * Solve conflicts, loop over:  
       * Edit conflicting files and resolve conflicts 
       * `git add <filename>` to mark files as resolved (do not commit)
       * `git rebase --continue`  
* Push to the devel branch of your fork: 
    * `git push -f origin <devel_branch_name>` 
* Fill a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request). Note that new commits pushed on that branch will be automatically added to the pull request. 
* Once the merge is accepted, it is recommended to delete the branch from your fork and your local repository  

### Guidelines and coding style

#### Fortran coding style

| DOS                   | DONTS                       |
|-----------------------|-----------------------------|
| Use Fortran 90  |  Runtime polymorphism, type-bound procedures|
| Fixed length (132), uppercase    |  free format, lowercase                           |
| Filenames: `<subroutine_name>.F`, `<module_name>_mod.F` |`*.f`, `*.F90`   |
| Indent using spaces | use tabs | 
| Modules and derived type   |`COMMON`, `EQUIVALENCE`, `SAVE`|
| Pass variables (built-in and derived types) as dummy arguments | use global variables |
| Look for clarity                        |`GOTO`, multiple `RETURN` | 
| `#include "implicit_f.inc"` that contains `IMPLICIT NONE` | use implicit declaration |
| Explicit size of dummy argument arrays  `INTEGER, INTENT(IN) :: A(LEN)`  | `INTEGER, INTENT(IN) :: A(*)` |
| Use bounds for arrays operations | `A = B + C` when A,B,C are arrays     |
| Use the `MY_REAL` type for real numbers  | use `DOUBLE PRECISION` systematically |
| Use `ALLOCATABLE` arrays |use pointers when allocatable is possible |
| use `MY_ALLOCATE` or check the status of the allocation | use large automatic arrays |
| Deallocate arrays as soon as possible | use automatic deallocation |


#### Fortran Performance 

* [vectorization](https://en.wikipedia.org/wiki/Automatic_vectorization)
    * Use `#include <vectorize.inc>` that contains the [IVDEP](https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference/h-to-i/ivdep.html) directive
    * When possible, work on arrays of size `MVSIZ` 
    * when possible, avoid using `IF / THEN / ELSE` inside `DO` loops
    * Avoid using `EXIT` and `CYCLE` inside computationally intensive loops
    * Avoid calling function or subroutine inside computationally intensive loops 
* Rule of thumb for data locality of 2D arrays:
    * largest dimension should be last if it is >= MVSIZ, or 128 (`X(3,NUMNOD)`)
    * largest dimension should be first if it is <= MVSIZ or 128 (`C(MVSIZ,5)`)
* Do not use aliasing of dummy arguments: different arguments must point to different memory locations
* Avoid large arrays of small datatype: prefer `POINT%X(1:NBPOINT)` to `POINT(1:NBOINT)%X`
* Initializing large array can be costly, avoid flushing to zeros entire arrays when it is not needed
* Use integer exponent instead of real exponent ( `A**2` instead of `A**2.0` )

