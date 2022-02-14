FROM deepnote/python:3.9
RUN apt update -y && apt install -y software-properties-common 
RUN apt install graphviz libgraphviz-dev
RUN apt-get install libgraphviz-dev
RUN apt-get -y install imagemagick
RUN pip install moviepy
RUN apt-add-repository ppa:swi-prolog/stable
RUN apt install -y swi-prolog
RUN mkdir swi-kernel
RUN cd swi-kernel && wget https://raw.githubusercontent.com/targodan/jupyter-swi-prolog/master/kernel.json
RUN pip3 install --upgrade pip && pip3 install jupyter
RUN jupyter kernelspec install swi-kernel --user
RUN rmdir --ignore-fail-on-non-empty swi-kernel
ENV DEFAULT_KERNEL_NAME swi-kernel