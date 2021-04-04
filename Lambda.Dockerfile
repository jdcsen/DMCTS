FROM lambci/lambda:build-provided

ARG EXECUTABLE_NAME=bootstrap
ARG STACK_RESOLVER=lts-16.31
ARG OUTPUT_DIR=/opt
ARG PACKAGER_COMMIT_SHA=6404623b59a4189c17cadeb2c5a2eb96f1a76722

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

# Saving default system libraries before doing anything else
RUN du -a /lib64 /usr/lib64 | cut -f2 > /root/default-libraries

# Installing basic dependencies
RUN yum install -y \
    git-core \
    tar \
    sudo \
    xz \
    make

# Installing Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Setting up GHC
RUN stack setup --resolver=${STACK_RESOLVER}

# Installing common packages so that docker builds are faster
RUN stack install --resolver=${STACK_RESOLVER} text bytestring http-client http-types path template-haskell case-insensitive aeson unordered-containers

RUN mkdir /root/lambda-function

RUN cd /tmp && \
    git clone https://github.com/saurabhnanda/aws-lambda-packager.git && \
    cd /tmp/aws-lambda-packager && \
    git checkout ${PACKAGER_COMMIT_SHA} && \
    stack install --resolver=${STACK_RESOLVER}


COPY . /root/lambda-function/

RUN pwd

# Building the lambda-function and copying it to the output directory
RUN cd /root/lambda-function
WORKDIR /root/lambda-function/
RUN ls
RUN stack clean --full
RUN stack build --fast

RUN mkdir -p ${OUTPUT_DIR} && \
    mkdir -p ${OUTPUT_DIR}/lib

RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE_NAME} ${OUTPUT_DIR}/${EXECUTABLE_NAME}

# Finally, copying over all custom/extra libraries with the help of aws-lambda-packager
RUN /root/.local/bin/aws-lambda-packager copy-custom-libraries \
    -l /root/default-libraries \
    -f ${OUTPUT_DIR}/${EXECUTABLE_NAME} \
    -o ${OUTPUT_DIR}/lib

ENTRYPOINT sh
