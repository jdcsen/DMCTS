ARG EXECUTABLE_NAME=bootstrap
ARG STACK_RESOLVER=lts-16.31
ARG OUTPUT_DIR=/opt

FROM amazon/aws-lambda-provided:al2 as build

ARG STACK_RESOLVER
ARG EXECUTABLE_NAME
ARG OUTPUT_DIR

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

# Saving default system libraries before doing anything else
RUN du -a /lib64 /usr/lib64 | cut -f2 > /root/default-libraries

# Installing basic dependencies

# C/C++ Compiler
RUN yum groupinstall "Development tools" -y

# Basic POSIX Tools
RUN yum install -y \
    tar \
    xz \
    make

# Required Libraries
RUN yum install -y \
    gmp-devel

# Installing Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Setting up GHC
RUN stack setup --resolver=${STACK_RESOLVER} --local-bin-path=${OUTPUT_DIR}

# Installing common packages so that docker builds are faster
# HTTP
RUN stack install --resolver=${STACK_RESOLVER} --local-bin-path=${OUTPUT_DIR} text bytestring http-client http-types
# JSON
RUN stack install --resolver=${STACK_RESOLVER} --local-bin-path=${OUTPUT_DIR} aeson
# Testing
RUN stack install --resolver=${STACK_RESOLVER} --local-bin-path=${OUTPUT_DIR} hspec
# etc (transitive or undetermined).

RUN stack install --resolver=${STACK_RESOLVER} --local-bin-path=${OUTPUT_DIR} path case-insensitive unordered-containers
RUN mkdir /root/lambda-function

COPY . /root/lambda-function/

RUN pwd

# Building the lambda-function and copying it to the output directory
RUN cd /root/lambda-function
WORKDIR /root/lambda-function/
RUN ls
RUN stack clean --full --local-bin-path=${OUTPUT_DIR}
RUN stack build --fast --local-bin-path=${OUTPUT_DIR}

RUN mkdir -p ${OUTPUT_DIR} && \
    mkdir -p ${OUTPUT_DIR}/lib

RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE_NAME} ${OUTPUT_DIR}/${EXECUTABLE_NAME}

FROM  amazon/aws-lambda-provided:al2 as run

ARG EXECUTABLE_NAME
ARG OUTPUT_DIR

COPY --from=build ${OUTPUT_DIR} ${OUTPUT_DIR}

WORKDIR "/opt"
ENTRYPOINT [ "/opt/bootstrap" ]
