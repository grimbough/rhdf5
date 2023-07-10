FROM python:3.10
LABEL Maintainer=mike.smith@embl.de
RUN pip install pandas h5py anndata
RUN mkdir /data/
WORKDIR /usr/app/src
COPY create_nullable_arrays.py ./
CMD python ./create_nullable_arrays.py
