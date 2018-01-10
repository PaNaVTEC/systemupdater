FROM opamp/archlinux

ADD ./.stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/systemupdater/systemupdater /opt/systemupdater/

RUN ./opt/systemupdater/systemupdater