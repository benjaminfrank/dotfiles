# https://docs.docker.com/reference/builder/

FROM debian:jessie

MAINTAINER Sam Halliday, sam.halliday@gmail.com

################################################
# Package Management
RUN\
  echo 'deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main' >> /etc/apt/sources.list &&\
  echo 'deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu precise main' >> /etc/apt/sources.list &&\
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886 &&\
  cat /etc/apt/sources.list | sed 's/^deb /deb-src /' >> /etc/apt/sources.list &&\
  echo 'APT::Install-Recommends "0";' >> /etc/apt/apt.conf &&\
  echo 'APT::Install-Suggests "0";' >> /etc/apt/apt.conf &&\
  apt-get update -qq &&\
  apt-get autoremove -qq &&\
  apt-get clean

################################################
# Base System
RUN\
  apt-get install -y git-core locales &&\
  echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen &&\
  locale-gen &&\
  apt-get clean

################################################
# Emacs
#
# This involves installing the build-deps for emacs23. To clean up we
# need to take a debfoster snapshot of before and agressively purge
# once we have done the compiles. Having the additional system emacs
# ensures we have all runtime deps for our builds.
RUN\
  apt-get install -y emacs23 &&\
  apt-get clean
RUN\
  apt-get install -y debfoster &&\
  debfoster -q &&\
  apt-get build-dep -y emacs23 &&\
  mkdir /tmp/emacs-build &&\
  curl http://ftp.gnu.org/gnu/emacs/emacs-24.1.tar.bz2 -o /tmp/emacs-24.1.tar.bz2 &&\
  cd /tmp && tar xf emacs-24.1.tar.bz2 && cd emacs-24.1 && ./configure --prefix=/opt/emacs-24.1 && make && make install &&\
  curl http://ftp.gnu.org/gnu/emacs/emacs-24.2.tar.xz -o /tmp/emacs-24.2.tar.xz &&\
  cd /tmp && tar xf emacs-24.2.tar.xz && cd emacs-24.2 && ./configure --prefix=/opt/emacs-24.2 && make && make install &&\
  curl http://ftp.gnu.org/gnu/emacs/emacs-24.3.tar.xz -o /tmp/emacs-24.3.tar.xz &&\
  cd /tmp && tar xf emacs-24.3.tar.xz && cd emacs-24.3 && ./configure --prefix=/opt/emacs-24.3 && make && make install &&\
  curl http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz -o /tmp/emacs-24.4.tar.xz &&\
  cd /tmp && tar xf emacs-24.4.tar.xz && cd emacs-24.4 && ./configure --prefix=/opt/emacs-24.4 && make && make install &&\
  curl http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.xz -o /tmp/emacs-24.5.tar.xz &&\
  cd /tmp && tar xf emacs-24.5.tar.xz && cd emacs-24.5 && ./configure --prefix=/opt/emacs-24.5 && make && make install &&\
  echo Y | debfoster -f &&\
  rm -rf /tmp/emacs* &&\
  apt-get clean

################################################
# Cask
RUN\
  apt-get install -y python &&\
  apt-get clean &&\
  curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
ENV PATH /root/.cask/bin:${PATH}

################################################
# Git
RUN\
  apt-get install -y git &&\
  apt-get clean
