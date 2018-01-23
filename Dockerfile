FROM opamp/archlinux

USER 1000

ADD ./.stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/systemupdater/systemupdater /opt/systemupdater/

RUN yaourt -Syyu
RUN ./opt/systemupdater/systemupdater