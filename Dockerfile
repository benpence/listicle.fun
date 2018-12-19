FROM haskell:8

WORKDIR /root

ADD ./ /root/
RUN stack setup
RUN stack build 

ENTRYPOINT ["stack", "run", "listicle-server"]
