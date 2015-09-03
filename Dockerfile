# https://docs.docker.com/reference/builder/

FROM debian:jessie

MAINTAINER Sam Halliday, sam.halliday@gmail.com

################################################
# Package Management
RUN\
  cat /etc/apt/sources.list | sed 's/^deb /deb-src /' >> /etc/apt/sources.list &&\
  echo 'APT::Install-Recommends "0";' >> /etc/apt/apt.conf &&\
  echo 'APT::Install-Suggests "0";' >> /etc/apt/apt.conf &&\
  apt-get update -qq &&\
  apt-get autoremove -qq &&\
  apt-get clean

################################################
# Base System
RUN\
  apt-get install -y git curl locales ca-certificates &&\
  echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen &&\
  locale-gen &&\
  apt-get clean

################################################
# Emacs
#
# This involves installing the build-deps for emacs24. To clean up we
# need to take a debfoster snapshot of before and agressively purge
# once we have done the compiles. Having the additional system emacs
# ensures we have all runtime deps for our builds.
#
# NOTE: 24.1 does not compile due to use of a removed glibc function.
RUN\
  apt-get install -y emacs24 &&\
  apt-get clean
RUN\
  apt-get install -y debfoster &&\
  debfoster -q &&\
  apt-get build-dep -y emacs24 &&\
  mkdir /tmp/emacs-build &&\
  for EMACS_VERSION in 24.2 24.3 24.4 24.5 ; do\
    curl http://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz -o /tmp/emacs-${EMACS_VERSION}.tar.xz &&\
    cd /tmp && tar xf emacs-${EMACS_VERSION}.tar.xz &&\
    cd emacs-${EMACS_VERSION} &&\
    ./configure --prefix=/opt/emacs-${EMACS_VERSION} &&\
    make && make install ;\
  done &&\
  echo Y | debfoster -f &&\
  rm -rf /tmp/emacs* &&\
  apt-get clean
ENV PATH /opt/emacs-24.5/bin:${PATH}

################################################
# Cask
RUN\
  apt-get install -y python &&\
  curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python &&\
  apt-get clean
ENV PATH /root/.cask/bin:${PATH}

################################################
# Aspell (dependency of Emacs flycheck)
RUN\
  apt-get install -y aspell-en &&\
  apt-get clean
