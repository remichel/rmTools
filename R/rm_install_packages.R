#' rm_install_packages
#'
#' @param git_pkgs A character vector with all packages from public github repositories, specified as "username/reponame@branch"
#' @param git_private_pkgs A character vector with all packages from private github repositories, specified as "username/reponame@branch"
#' @param cran_pkgs A character vector with all package names from cran
#' @param load_after_install defaults to T, loads all pkgs after installation
#' @param force_git for public github packages, force installation
#' @param force_git_private for private github packages, force installation
#'
#' @return
#' @export rm_install_packages
#'
#'
rm_install_packages <- function(git_pkgs = NULL, git_private_pkgs = NULL, cran_pkgs = NULL, load_after_install = T, force_git = F, force_git_private = F){

  # public github repositories
  git_pkgs_names = c()
  if(!is.null(git_pkgs)){
    if(!("devtools" %in% installed.packages())){
      install.packages("devtools")
    }
    for(i in 1:length(git_pkgs)){
      pkgname = strsplit(strsplit(git_pkgs[i], "/")[[1]][[2]], "@")[[1]][[1]]
      if(!(pkgname %in% installed.packages()) | force_git == T){
        devtools::install_github(git_pkgs[i], force = force_git)
        git_pkgs_names[i] = pkgname
      }
    }

  }


# private github repositories
git_private_pkgs_names = c()
if(!is.null(git_pkgs)){
  if(!("devtools" %in% installed.packages())){
    install.packages("devtools")
  }
  if(!("credentials" %in% installed.packages())){
    install.packages("credentials")
  }
  for(i in 1:length(git_pkgs)){
    pkgname = strsplit(strsplit(git_private_pkgs[i], "/")[[1]][[2]], "@")[[1]][[1]]
    if(!(pkgname %in% installed.packages()) | force_git_private == T){
      credentials::set_github_pat()
      devtools::install_github(git_private_pkgs[i], force = force_git_private, auth_token = devtools::github_pat(quiet = T))
      git_private_pkgs_names[i] = pkgname
    }
  }
}

# install cran packages
if(!is.null(cran_pkgs)){
  for(i in 1:length(cran_pkgs)){
    if(!(cran_pkgs[i] %in% installed.packages())){
      install.packages(cran_pkgs[i])
    }
  }
}

if(load_after_install == T){
  rmTools::libraries(c(git_pkgs_names, git_private_pkgs_names, cran_pkgs))
}

}
